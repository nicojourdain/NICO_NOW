MODULE obs_read_sst
   !!======================================================================
   !!                       ***  MODULE obs_read_sst  ***
   !! Observation diagnostics: Read the SST data from the GHRSST database 
   !!                          or any SST data from feedback files
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   obs_rea_sst : Driver for reading SST data from the GHRSST/feedback
   !!   obs_rea_sst_rey : Driver for reading SST data from Reynolds
   !!----------------------------------------------------------------------

   !! * Modules used   
   USE par_kind                 ! Precision variables
   USE in_out_manager           ! I/O manager
   USE dom_oce                  ! Ocean space and time domain variables
   USE obs_mpp                  ! MPP support routines for observation diagnostics
   USE julian                   ! Julian date routines
   USE obs_utils                ! Observation operator utility functions
   USE obs_grid                 ! Grid search
   USE obs_sort                 ! Sorting observation arrays
   USE obs_surf_def             ! Surface observation definitions
   USE obs_types                ! Observation type definitions
   USE obs_sst_io               ! I/O for sst files
   USE iom                      ! I/O of fields for Reynolds data
   USE netcdf                   ! NetCDF library

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE

   PUBLIC obs_rea_sst      ! Read the SST observations from the point data
   PUBLIC obs_rea_sst_rey  ! Read the gridded Reynolds SST 
   
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_read_sst.F90 2287 2010-10-18 07:53:52Z smasson $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE obs_rea_sst( kformat, &
      &                    sstdata, knumfiles, cfilenames, &
      &                    kvars, kextr, kstp, ddobsini, ddobsend, &
      &                    ldignmis, ldmod )
      !!---------------------------------------------------------------------
      !!
      !!                   *** ROUTINE obs_rea_sst ***
      !!
      !! ** Purpose : Read from file the SST data
      !!
      !! ** Method  : Depending on kformat either AVISO or
      !!              feedback data files are read
      !!
      !! ** Action  : 
      !!
      !!
      !! History :  
      !!      ! :  2009-01 (K. Mogensen) Initial version based on old versions
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Arguments
      INTEGER :: kformat   ! Format of input data
      !                    ! 0: Feedback
      !                    ! 1: GHRSST
      TYPE(obs_surf), INTENT(INOUT) :: sstdata   ! SST data to be read
      INTEGER, INTENT(IN) :: knumfiles    ! Number of corio format files to read in
      CHARACTER(LEN=128), INTENT(IN) :: cfilenames(knumfiles) ! File names to read in
      INTEGER, INTENT(IN) :: kvars      ! Number of variables in sstdata
      INTEGER, INTENT(IN) :: kextr      ! Number of extra fields for each var in sstdata
      INTEGER, INTENT(IN) :: kstp       ! Ocean time-step index
      LOGICAL, INTENT(IN) :: ldignmis   ! Ignore missing files
      LOGICAL, INTENT(IN) :: ldmod      ! Initialize model from input data
      REAL(KIND=dp), INTENT(IN) :: ddobsini   ! Obs. ini time in YYYYMMDD.HHMMSS
      REAL(KIND=dp), INTENT(IN) :: ddobsend   ! Obs. end time in YYYYMMDD.HHMMSS
         
      !! * Local declarations
      CHARACTER(LEN=11), PARAMETER :: cpname='obs_rea_sst'
      INTEGER :: ji
      INTEGER :: jj
      INTEGER :: jk
      INTEGER :: iflag
      INTEGER :: inobf
      INTEGER :: i_file_id
      INTEGER :: inowin
      INTEGER :: iyea
      INTEGER :: imon
      INTEGER :: iday
      INTEGER :: ihou
      INTEGER :: imin
      INTEGER :: isec
      INTEGER, DIMENSION(knumfiles) :: irefdate
      INTEGER :: iobsmpp
      INTEGER, PARAMETER :: isstmaxtype = 1024
      INTEGER, DIMENSION(0:isstmaxtype) :: &
         & ityp, &
         & itypmpp
      INTEGER, DIMENSION(:), ALLOCATABLE :: &
         & iobsi,    &
         & iobsj,    &
         & iproc,    &
         & iindx,    &
         & ifileidx, &
         & isstidx
      INTEGER :: itype
      REAL(wp), DIMENSION(:), ALLOCATABLE :: &
         & zphi, &
         & zlam
      REAL(dp), DIMENSION(:), ALLOCATABLE :: &
         & zdat
      LOGICAL :: llvalprof
      TYPE(obfbdata), POINTER, DIMENSION(:) :: &
         & inpfiles
      REAL(dp), DIMENSION(knumfiles) :: &
         & djulini, &
         & djulend
      INTEGER :: iobs
      INTEGER :: iobstot
      CHARACTER(len=8) :: cl_refdate
   
      ! Local initialization
      iobs = 0
 
      !-----------------------------------------------------------------------
      ! Check data the model part is just with feedback data files
      !-----------------------------------------------------------------------
      IF ( ldmod .AND. ( kformat /= 0 ) ) THEN
         CALL ctl_stop( 'Model can only be read from feedback data' )
         RETURN
      ENDIF

      !-----------------------------------------------------------------------
      ! Count the number of files needed and allocate the obfbdata type
      !-----------------------------------------------------------------------
      
      inobf = knumfiles
      
      ALLOCATE( inpfiles(inobf) )

      sst_files : DO jj = 1, inobf

         CALL init_obfbdata( inpfiles(jj) )
          
         !---------------------------------------------------------------------
         ! Prints
         !---------------------------------------------------------------------
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) ' obs_rea_sst : Reading from file = ', &
               & TRIM( TRIM( cfilenames(jj) ) )
            WRITE(numout,*) ' ~~~~~~~~~~~'
            WRITE(numout,*)
         ENDIF

         !---------------------------------------------------------------------
         !  Initialization: Open file and get dimensions only
         !---------------------------------------------------------------------
         
         iflag = nf90_open( TRIM( TRIM( cfilenames(jj) ) ), nf90_nowrite, &
            &                      i_file_id )
         
         IF ( iflag /= nf90_noerr ) THEN

            IF ( ldignmis ) THEN
               inpfiles(jj)%nobs = 0
               CALL ctl_warn( 'File ' // TRIM( TRIM( cfilenames(jj) ) ) // &
                  &           ' not found' )
            ELSE 
               CALL ctl_stop( 'File ' // TRIM( TRIM( cfilenames(jj) ) ) // &
                  &           ' not found' )
            ENDIF

         ELSE 
            
            !------------------------------------------------------------------
            !  Close the file since it is opened in read_proffile
            !------------------------------------------------------------------
            
            iflag = nf90_close( i_file_id )

            !------------------------------------------------------------------
            !  Read the profile file into inpfiles
            !------------------------------------------------------------------
            IF ( kformat == 0 ) THEN
               IF(lwp) THEN
                  WRITE(numout,*)
                  WRITE(numout,*)'Reading from feedback file :', &
                     &           TRIM( cfilenames(jj) )
               ENDIF
               CALL read_obfbdata( TRIM( cfilenames(jj) ), inpfiles(jj), &
                  &                ldgrid = .TRUE. )
               IF ( ldmod .AND. ( inpfiles(jj)%nadd == 0 ) ) THEN
                  CALL ctl_stop( 'Model not in input data' )
                  RETURN
               ENDIF
            ELSEIF ( kformat == 1) THEN
               CALL read_ghrsst( TRIM( cfilenames(jj) ), inpfiles(jj), &
               &                 numout, lwp, .TRUE. )
            ELSE
               CALL ctl_stop( 'File format unknown' )
            ENDIF

            !------------------------------------------------------------------
            !  Change longitude (-180,180)
            !------------------------------------------------------------------

            DO ji = 1, inpfiles(jj)%nobs 

               IF ( inpfiles(jj)%plam(ji) < -180. ) &
                  &   inpfiles(jj)%plam(ji) = inpfiles(jj)%plam(ji) + 360.

               IF ( inpfiles(jj)%plam(ji) >  180. ) &
                  &   inpfiles(jj)%plam(ji) = inpfiles(jj)%plam(ji) - 360.

            END DO

            !------------------------------------------------------------------
            !  Calculate the date  (change eventually)
            !------------------------------------------------------------------
            cl_refdate=inpfiles(jj)%cdjuldref(1:8)
            READ(cl_refdate,'(I8)') irefdate(jj)
            
            CALL ddatetoymdhms( ddobsini, iyea, imon, iday, ihou, imin, isec )
            CALL greg2jul( isec, imin, ihou, iday, imon, iyea, djulini(jj), &
               &           krefdate = irefdate(jj) )
            CALL ddatetoymdhms( ddobsend, iyea, imon, iday, ihou, imin, isec )
            CALL greg2jul( isec, imin, ihou, iday, imon, iyea, djulend(jj), &
               &           krefdate = irefdate(jj) )
            IF ( inpfiles(jj)%nobs > 0 ) THEN
               inpfiles(jj)%iproc = -1
               inpfiles(jj)%iobsi = -1
               inpfiles(jj)%iobsj = -1
            ENDIF
            inowin = 0
            DO ji = 1, inpfiles(jj)%nobs
               IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
                  & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
                  inowin = inowin + 1
               ENDIF
            END DO
            ALLOCATE( zlam(inowin)  )
            ALLOCATE( zphi(inowin)  )
            ALLOCATE( iobsi(inowin) )
            ALLOCATE( iobsj(inowin) )
            ALLOCATE( iproc(inowin) )
            inowin = 0
            DO ji = 1, inpfiles(jj)%nobs
               IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
                  & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
                  inowin = inowin + 1
                  zlam(inowin) = inpfiles(jj)%plam(ji)
                  zphi(inowin) = inpfiles(jj)%pphi(ji)
               ENDIF
            END DO

            CALL obs_grid_search( inowin, zlam, zphi, iobsi, iobsj, iproc, 'T' )

            inowin = 0
            DO ji = 1, inpfiles(jj)%nobs
               IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
                  & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
                  inowin = inowin + 1
                  inpfiles(jj)%iproc(ji,1) = iproc(inowin)
                  inpfiles(jj)%iobsi(ji,1) = iobsi(inowin)
                  inpfiles(jj)%iobsj(ji,1) = iobsj(inowin)
               ENDIF
            END DO
            DEALLOCATE( zlam, zphi, iobsi, iobsj, iproc )

            DO ji = 1, inpfiles(jj)%nobs
               IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
                  & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
                  IF ( nproc == 0 ) THEN
                     IF ( inpfiles(jj)%iproc(ji,1) >  nproc ) CYCLE
                  ELSE
                     IF ( inpfiles(jj)%iproc(ji,1) /= nproc ) CYCLE
                  ENDIF
                  llvalprof = .FALSE.
                  IF ( ( inpfiles(jj)%ivlqc(1,ji,1) == 1 ) .OR. &
                     & ( inpfiles(jj)%ivlqc(1,ji,1) == 2 ) ) THEN
                     iobs = iobs + 1
                  ENDIF
               ENDIF
            END DO

         ENDIF

      END DO sst_files

      !-----------------------------------------------------------------------
      ! Get the time ordered indices of the input data
      !-----------------------------------------------------------------------

      !---------------------------------------------------------------------
      !  Loop over input data files to count total number of profiles
      !---------------------------------------------------------------------
      iobstot = 0
      DO jj = 1, inobf
         DO ji = 1, inpfiles(jj)%nobs
            IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
               & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
               iobstot = iobstot + 1
            ENDIF
         END DO
      END DO

      ALLOCATE( iindx(iobstot), ifileidx(iobstot), &
         &      isstidx(iobstot), zdat(iobstot) )
      jk = 0
      DO jj = 1, inobf
         DO ji = 1, inpfiles(jj)%nobs
            IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
               & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
               jk = jk + 1
               ifileidx(jk) = jj
               isstidx(jk) = ji
               zdat(jk)     = inpfiles(jj)%ptim(ji)
            ENDIF
         END DO
      END DO
      CALL sort_dp_indx( iobstot, &
         &               zdat,     &
         &               iindx   )
      
      CALL obs_surf_alloc( sstdata, iobs, kvars, kextr, kstp )
      
      ! * Read obs/positions, QC, all variable and assign to sstdata
 
      iobs = 0

      ityp   (:) = 0
      itypmpp(:) = 0
      
      DO jk = 1, iobstot
         
         jj = ifileidx(iindx(jk))
         ji = isstidx(iindx(jk))
         IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND.  &
            & ( inpfiles(jj)%ptim(ji) <= djulend(jj) ) ) THEN
            
            IF ( nproc == 0 ) THEN
               IF ( inpfiles(jj)%iproc(ji,1) >  nproc ) CYCLE
            ELSE
               IF ( inpfiles(jj)%iproc(ji,1) /= nproc ) CYCLE
            ENDIF
            
            ! Set observation information
            
            IF ( ( inpfiles(jj)%ivlqc(1,ji,1) == 1 ) .OR. &
               & ( inpfiles(jj)%ivlqc(1,ji,1) == 2 ) ) THEN

               iobs = iobs + 1

               CALL jul2greg( isec,                   &
                  &           imin,                   &
                  &           ihou,                   &
                  &           iday,                   &
                  &           imon,                   &
                  &           iyea,                   &
                  &           inpfiles(jj)%ptim(ji), &
                  &           irefdate(jj) )


               ! SST time coordinates
               sstdata%nyea(iobs) = iyea
               sstdata%nmon(iobs) = imon
               sstdata%nday(iobs) = iday
               sstdata%nhou(iobs) = ihou
               sstdata%nmin(iobs) = imin
               
               ! SST space coordinates
               sstdata%rlam(iobs) = inpfiles(jj)%plam(ji)
               sstdata%rphi(iobs) = inpfiles(jj)%pphi(ji)

               ! Coordinate search parameters
               sstdata%mi  (iobs) = inpfiles(jj)%iobsi(ji,1)
               sstdata%mj  (iobs) = inpfiles(jj)%iobsj(ji,1)
               
               ! Instrument type
               READ( inpfiles(jj)%cdtyp(ji), '(I4)' ) itype
               sstdata%ntyp(iobs) = itype
               IF ( itype < isstmaxtype + 1 ) THEN
                  ityp(itype+1) = ityp(itype+1) + 1
               ELSE
                  IF(lwp)WRITE(numout,*)'WARNING:Increase isstmaxtype in ',&
                     &                  cpname
               ENDIF

               ! Bookkeeping data to match observations
               sstdata%nsidx(iobs) = iobs
               sstdata%nsfil(iobs) = iindx(jk)

               ! QC flags
               sstdata%nqc(iobs) = inpfiles(jj)%ivqc(ji,1)

               ! Observed value
               sstdata%robs(iobs,1) = inpfiles(jj)%pob(1,ji,1)


               ! Model and MDT is set to fbrmdi unless read from file
               IF ( ldmod ) THEN
                  sstdata%rmod(iobs,1) = inpfiles(jj)%padd(1,ji,1,1)
               ELSE
                  sstdata%rmod(iobs,1) = fbrmdi
               ENDIF
            ENDIF
         ENDIF

      END DO

      !-----------------------------------------------------------------------
      ! Sum up over processors
      !-----------------------------------------------------------------------
      
      CALL obs_mpp_sum_integer( iobs, iobsmpp )
      
      !-----------------------------------------------------------------------
      ! Output number of observations.
      !-----------------------------------------------------------------------
      IF (lwp) THEN

         WRITE(numout,*)
         WRITE(numout,'(1X,A)')'SST data types'
         WRITE(numout,'(1X,A)')'--------------'
         DO jj = 1,8
            IF ( itypmpp(jj) > 0 ) THEN
               WRITE(numout,'(1X,A4,I4,A3,I10)')'Type ', jj,' = ',itypmpp(jj)
            ENDIF
         END DO
         WRITE(numout,'(1X,A50)')'--------------------------------------------------'
         WRITE(numout,'(1X,A40,I10)')'Total                                 = ',iobsmpp
         WRITE(numout,*)

      ENDIF

      !-----------------------------------------------------------------------
      ! Deallocate temporary data
      !-----------------------------------------------------------------------
      DEALLOCATE( ifileidx, isstidx, zdat )

      !-----------------------------------------------------------------------
      ! Deallocate input data
      !-----------------------------------------------------------------------
      DO jj = 1, inobf
         IF ( inpfiles(jj)%lalloc ) THEN
            CALL dealloc_obfbdata( inpfiles(jj) )
         ENDIF
      END DO
      DEALLOCATE( inpfiles )

   END SUBROUTINE obs_rea_sst

   SUBROUTINE obs_rea_sst_rey( sstname, cdsstfmt, sstdata, kvars, kextra, &
      &                        kstp, ddobsini, ddobsend )
      !!---------------------------------------------------------------------
      !!
      !!                   *** ROUTINE obs_rea_sst ***
      !!
      !! ** Purpose : Read from file the pseudo SST data from Reynolds
      !!
      !! ** Method  : 
      !!
      !! ** Action  : 
      !!
      !! References : 
      !!
      !! History :  
      !!      ! :  
      !!----------------------------------------------------------------------
      !! * Modules used
      USE par_oce          ! Ocean parameters
   
      !! * Arguments
      CHARACTER(len=128), INTENT(IN) :: sstname   ! Generic file name
      CHARACTER(len=12), INTENT(IN) :: cdsstfmt   ! Format of SST files (yearly/monthly)
      TYPE(obs_surf), INTENT(INOUT) :: sstdata    ! SST data
      REAL(KIND=dp), INTENT(IN) :: ddobsini    ! Obs. ini time in YYYYMMDD.HHMMSS
      REAL(KIND=dp), INTENT(IN) :: ddobsend    ! Obs. end time in YYYYMMDD.HHMMSS
      INTEGER, INTENT(IN) :: kvars      ! Number of variables in sstdata structures
      INTEGER, INTENT(IN) :: kextra     ! Number of extra variables in sstdata structures
      INTEGER, INTENT(IN) :: kstp       ! Ocean time-step index
      
      INTEGER :: iyear
      INTEGER :: imon
      INTEGER :: iday
      INTEGER :: ihour
      INTEGER :: imin
      INTEGER :: isec
      INTEGER :: ihhmmss
      INTEGER :: iyear1
      INTEGER :: iyear2
      INTEGER :: imon1
      INTEGER :: imon2
      INTEGER :: iyearf
      INTEGER :: imonf
      REAL(KIND=wp) :: pjulini
      REAL(KIND=wp) :: pjulend
      REAL(KIND=wp) :: pjulb
      REAL(KIND=wp) :: pjule
      REAL(KIND=wp) :: pjul
      INTEGER :: inumsst
      INTEGER :: itotrec
      INTEGER :: inumobs
      INTEGER :: irec
      INTEGER :: ifld
      INTEGER :: inum
      INTEGER :: ji, jj
      CHARACTER(len=128) :: clname
      CHARACTER(len=4) :: cdyear
      CHARACTER(len=2) :: cdmon
      REAL(kind=wp), ALLOCATABLE, DIMENSION(:,:,:) :: zsstin

      IF (lwp) WRITE(numout,*)'In obs_rea_sst_rey',sstname

      !-----------------------------------------------------------------------
      ! Convert observation window to julian dates.
      !-----------------------------------------------------------------------
      iyear1 = NINT( ddobsini / 10000 )
      imon1 = NINT( ( ddobsini - iyear1 * 10000 ) / 100 )
      iday = MOD( NINT( ddobsini ), 100 )
      ihhmmss = ( ddobsini - NINT( ddobsini ) ) * 1000000
      ihour = ihhmmss / 10000
      imin = ( ihhmmss - ihour * 100 ) / 100
      isec = MOD( ihhmmss, 100 )
      CALL greg2jul ( isec, imin, ihour, iday, imon1, iyear1, pjulini )
      IF (lwp) WRITE(numout,*)'dateini',ddobsini,iyear1,imon1,iday,ihour, &
         & imin,isec,pjulini

      iyear2 = NINT( ddobsini / 10000 )
      imon2 = NINT( ( ddobsend - iyear2 * 10000 ) / 100 )
      iday = MOD( NINT( ddobsend ), 100 )
      ihhmmss = ( ddobsend - NINT( ddobsend ) ) * 1000000
      ihour = ihhmmss / 10000
      imin = ( ihhmmss - ihour * 100 ) / 100
      isec = MOD( ihhmmss, 100 )
      CALL greg2jul ( isec, imin, ihour, iday, imon2, iyear2, pjulend )
      IF (lwp) WRITE(numout,*)'dateend',ddobsend,iyear2,imon2,iday,ihour, &
         & imin,isec,pjulend

      itotrec = NINT( pjulend - pjulini ) 
      ALLOCATE( &
         & zsstin( jpi, jpj, itotrec) &
         & )
      
      pjul = pjulini + 1
      
      iyearf = -1 
      imonf = -1

      IF ( TRIM(cdsstfmt) == 'yearly' ) THEN

         DO
         
            CALL jul2greg( isec, imin, ihour, iday, imon, iyear, &
               &           pjul, 19500101 )
            !
            IF ( iyear /= iyearf ) THEN
               
               CALL greg2jul ( 0, 0, 0, 1, 1, iyear, pjulb )
               
               IF ( iyearf /= -1 ) THEN
                  
                  CALL iom_close ( inumsst )      
                  
               ENDIF
               
               clname = sstname
               jj = INDEX( clname, 'YYYY' )
               
               IF ( jj == 0 ) THEN
                  
                  CALL ctl_stop( 'obs_rea_sst_rey : ', &
                  &           'Error processing filename ' // TRIM(sstname) )
                  
               ENDIF
               
               WRITE(cdyear,'(I4.4)') iyear
               clname(jj:jj+3) = cdyear
               IF(lwp) WRITE(numout,*)'Reading from Reynolds SST file : ',&
                  & TRIM(clname)
               
               inumsst = 0
               
               CALL iom_open ( clname, inumsst )
               
               IF ( inumsst == 0 ) THEN
                  
                  CALL ctl_stop( 'obs_rea_sst_rey : ', &
                     &           'Error reading ' // TRIM(clname) )
                  
               ENDIF
               
               iyearf = iyear
               
            ENDIF
            
            irec = pjul - pjulb + 1
            ifld = pjul - pjulini
            
            CALL iom_get ( inumsst, jpdom_data, 'sst', zsstin(:,:,ifld), irec )
            
            pjul = pjul + 1
            
            IF ( pjul > pjulend ) EXIT
         
         END DO

      ELSEIF ( TRIM(cdsstfmt) == 'monthly' ) THEN

         DO
         
            CALL jul2greg( isec, imin, ihour, iday, imon, iyear, &
               &           pjul, 19500101 )
            !
            IF ( iyear /= iyearf .OR. imon /= imonf ) THEN
               
               CALL greg2jul ( 0, 0, 0, 1, imon, iyear, pjulb )
               
               IF ( iyearf /= -1 .AND. imonf /= -1 ) THEN
                  
                  CALL iom_close ( inumsst )      
                  
               ENDIF
               
               clname = sstname

               jj = INDEX( clname, 'YYYY' )

               IF ( jj == 0 ) THEN
                  
                  CALL ctl_stop( 'obs_rea_sst_rey : ', &
                  &           'Error processing filename ' // TRIM(sstname) )
                  
               ENDIF
               
               WRITE(cdyear,'(I4.4)') iyear
               clname(jj:jj+3) = cdyear

               jj = INDEX( clname, 'MM' )
               
               IF ( jj == 0 ) THEN
                  
                  CALL ctl_stop( 'obs_rea_sst_rey : ', &
                  &           'Error processing filename ' // TRIM(sstname) )
                  
               ENDIF
               
               WRITE(cdmon,'(I2.2)') imon
               clname(jj:jj+1) = cdmon


               IF(lwp) WRITE(numout,*)'Reading from Reynolds SST file : ',&
                  & TRIM(clname)
               
               inumsst = 0
               
               CALL iom_open ( clname, inumsst )
               
               IF ( inumsst == 0 ) THEN
                  
                  CALL ctl_stop( 'obs_rea_sst_rey : ', &
                     &           'Error reading ' // TRIM(clname) )
                  
               ENDIF
               
               iyearf = iyear
               imonf = iyear
               
            ENDIF
            
            irec = pjul - pjulb + 1
            ifld = pjul - pjulini
            
            CALL iom_get ( inumsst, jpdom_data, 'sst', zsstin(:,:,ifld), irec )
            
            pjul = pjul + 1
            
            IF ( pjul > pjulend ) EXIT
         
         END DO

      ELSE
         
         CALL ctl_stop('Unknown REYNOLDS sst input data file format')

      ENDIF

      CALL iom_close ( inumsst )      

      inumobs = 0
      DO jj = nldj, nlej
         DO ji = nldi, nlei
            IF ( tmask(ji,jj,1) == 1.0_wp ) inumobs = inumobs + 1
         END DO
      END DO
      inumobs = inumobs * itotrec

      ! Allocate obs_surf data structure for time sorted data
         
      CALL obs_surf_alloc( sstdata, inumobs, kvars, kextra, kstp )

      pjul = pjulini + 1

      inumobs = 0

      DO

         CALL jul2greg( isec, imin, ihour, iday, imon, iyear, &
            &           pjul, 19500101 )

         ifld = pjul - pjulini

         DO jj = nldj, nlej
            DO ji = nldi, nlei

               IF ( tmask(ji,jj,1) == 1.0_wp ) THEN

                  inumobs = inumobs + 1
                  
                  ! Integer values
                  IF (ln_grid_global) THEN
                     sstdata%mi(inumobs)     = MAX(mig(ji),2)
                     sstdata%mj(inumobs)     = MAX(mjg(jj),2)
                  ELSE
                     sstdata%mi(inumobs)     = MAX(ji,2)
                     sstdata%mj(inumobs)     = MAX(jj,2)
                  ENDIF
                  sstdata%nsidx(inumobs)  = 0
                  sstdata%nsfil(inumobs)  = 0
                  sstdata%nyea(inumobs)   = iyear
                  sstdata%nmon(inumobs)   = imon
                  sstdata%nday(inumobs)   = iday
                  sstdata%nhou(inumobs)   = ihour
                  sstdata%nmin(inumobs)   = imin
                  sstdata%mstp(inumobs)   = 0
                  sstdata%nqc(inumobs)    = 0
                  sstdata%ntyp(inumobs)   = 0
         
                  ! Real values
                  sstdata%rlam(inumobs)   = glamt(ji,jj)
                  sstdata%rphi(inumobs)   = gphit(ji,jj)
                  sstdata%robs(inumobs,1) = zsstin(ji,jj,ifld)
                  sstdata%rmod(inumobs,1) = fbrmdi
                  sstdata%rext(inumobs,:) = fbrmdi

               ENDIF

            END DO
         END DO

         pjul = pjul + 1

         IF ( pjul > pjulend ) EXIT

      END DO

   END SUBROUTINE obs_rea_sst_rey

END MODULE obs_read_sst
