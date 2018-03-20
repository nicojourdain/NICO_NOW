MODULE obs_read_seaice
   !!======================================================================
   !!                       ***  MODULE obs_read_seaice  ***
   !! Observation diagnostics: Read the along track SEAICE data from
   !!                          GHRSST or any SEAICE data from feedback files
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   obs_rea_seaice : Driver for reading seaice data from the GHRSST/feedback
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
   USE obs_seaice_io            ! I/O for seaice files
   USE iom                      ! I/O of fields for Reynolds data
   USE netcdf                   ! NetCDF library

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE

   PUBLIC obs_rea_seaice      ! Read the seaice observations from the point data
   
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_read_seaice.F90 2287 2010-10-18 07:53:52Z smasson $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE obs_rea_seaice( kformat, &
      &                    seaicedata, knumfiles, cfilenames, &
      &                    kvars, kextr, kstp, ddobsini, ddobsend, &
      &                    ldignmis, ldmod )
      !!---------------------------------------------------------------------
      !!
      !!                   *** ROUTINE obs_rea_seaice ***
      !!
      !! ** Purpose : Read from file the seaice data
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
      TYPE(obs_surf), INTENT(INOUT) :: &
         & seaicedata     ! seaice data to be read
      INTEGER, INTENT(IN) :: knumfiles   ! Number of corio format files to read in
      CHARACTER(LEN=128), INTENT(IN) :: cfilenames(knumfiles) ! File names to read in
      INTEGER, INTENT(IN) :: kvars    ! Number of variables in seaicedata
      INTEGER, INTENT(IN) :: kextr    ! Number of extra fields for each var in seaicedata
      INTEGER, INTENT(IN) :: kstp     ! Ocean time-step index
      LOGICAL, INTENT(IN) :: ldignmis   ! Ignore missing files
      LOGICAL, INTENT(IN) :: ldmod      ! Initialize model from input data
      REAL(KIND=dp), INTENT(IN) :: ddobsini    ! Obs. ini time in YYYYMMDD.HHMMSS
      REAL(KIND=dp), INTENT(IN) :: ddobsend    ! Obs. end time in YYYYMMDD.HHMMSS
         
      !! * Local declarations
      CHARACTER(LEN=14), PARAMETER :: cpname='obs_rea_seaice'
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
      INTEGER, DIMENSION(knumfiles) :: &
         & irefdate
      INTEGER :: iobsmpp
      INTEGER, PARAMETER :: iseaicemaxtype = 1024
      INTEGER, DIMENSION(0:iseaicemaxtype) :: &
         & ityp, &
         & itypmpp
      INTEGER, DIMENSION(:), ALLOCATABLE :: &
         & iobsi,    &
         & iobsj,    &
         & iproc,    &
         & iindx,    &
         & ifileidx, &
         & iseaiceidx
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

      seaice_files : DO jj = 1, inobf
          
         !---------------------------------------------------------------------
         ! Prints
         !---------------------------------------------------------------------
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) ' obs_rea_seaice : Reading from file = ', &
               & TRIM( TRIM( cfilenames(jj) ) )
            WRITE(numout,*) ' ~~~~~~~~~~~~~~'
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
               CALL init_obfbdata( inpfiles(jj) )
               IF(lwp) THEN
                  WRITE(numout,*)
                  WRITE(numout,*)'Reading from feedback file :', &
                     &           TRIM( cfilenames(jj) )
               ENDIF
               CALL read_obfbdata( TRIM( cfilenames(jj) ), inpfiles(jj), &
                  &                ldgrid = .TRUE. )
               IF ( ldmod .AND. ( ( inpfiles(jj)%nadd == 0 ) .OR.&
                  &               ( inpfiles(jj)%next < 2 ) ) ) THEN
                  CALL ctl_stop( 'Model not in input data' )
                  RETURN
               ENDIF
            ELSEIF ( kformat == 1) THEN
               CALL read_seaice( TRIM( cfilenames(jj) ), inpfiles(jj), &
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

      END DO seaice_files

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
         &      iseaiceidx(iobstot), zdat(iobstot) )
      jk = 0
      DO jj = 1, inobf
         DO ji = 1, inpfiles(jj)%nobs
            IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
               & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
               jk = jk + 1
               ifileidx(jk) = jj
               iseaiceidx(jk) = ji
               zdat(jk)     = inpfiles(jj)%ptim(ji)
            ENDIF
         END DO
      END DO
      CALL sort_dp_indx( iobstot, &
         &               zdat,     &
         &               iindx   )
      
      CALL obs_surf_alloc( seaicedata, iobs, kvars, kextr, kstp )
      
      ! * Read obs/positions, QC, all variable and assign to seaicedata
 
      iobs = 0

      ityp   (:) = 0
      itypmpp(:) = 0
      
      DO jk = 1, iobstot
         
         jj = ifileidx(iindx(jk))
         ji = iseaiceidx(iindx(jk))
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


               ! seaice time coordinates
               seaicedata%nyea(iobs) = iyea
               seaicedata%nmon(iobs) = imon
               seaicedata%nday(iobs) = iday
               seaicedata%nhou(iobs) = ihou
               seaicedata%nmin(iobs) = imin
               
               ! seaice space coordinates
               seaicedata%rlam(iobs) = inpfiles(jj)%plam(ji)
               seaicedata%rphi(iobs) = inpfiles(jj)%pphi(ji)

               ! Coordinate search parameters
               seaicedata%mi  (iobs) = inpfiles(jj)%iobsi(ji,1)
               seaicedata%mj  (iobs) = inpfiles(jj)%iobsj(ji,1)
               
               ! Instrument type
               READ( inpfiles(jj)%cdtyp(ji), '(I4)' ) itype
               seaicedata%ntyp(iobs) = itype
               IF ( itype < iseaicemaxtype + 1 ) THEN
                  ityp(itype+1) = ityp(itype+1) + 1
               ELSE
                  IF(lwp)WRITE(numout,*)'WARNING:Increase iseaicemaxtype in ',&
                     &                  cpname
               ENDIF

               ! Bookkeeping data to match observations
               seaicedata%nsidx(iobs) = iobs
               seaicedata%nsfil(iobs) = iindx(jk)

               ! QC flags
               seaicedata%nqc(iobs) = inpfiles(jj)%ivqc(ji,1)

               ! Observed value
               seaicedata%robs(iobs,1) = inpfiles(jj)%pob(1,ji,1)


               ! Model and MDT is set to fbrmdi unless read from file
               IF ( ldmod ) THEN
                  seaicedata%rmod(iobs,1) = inpfiles(jj)%padd(1,ji,1,1)
               ELSE
                  seaicedata%rmod(iobs,1) = fbrmdi
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
         WRITE(numout,'(1X,A)')'Seaice data types'
         WRITE(numout,'(1X,A)')'-----------------'
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
      DEALLOCATE( ifileidx, iseaiceidx, zdat )

      !-----------------------------------------------------------------------
      ! Deallocate input data
      !-----------------------------------------------------------------------
      DO jj = 1, inobf
         CALL dealloc_obfbdata( inpfiles(jj) )
      END DO
      DEALLOCATE( inpfiles )

   END SUBROUTINE obs_rea_seaice

END MODULE obs_read_seaice

