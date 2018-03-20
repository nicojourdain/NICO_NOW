MODULE obs_read_sla
   !!======================================================================
   !!                       ***  MODULE obs_read_sla  ***
   !! Observation diagnostics: Read the along track SLA data from
   !!                          the AVISO/CLS database or any SLA data 
   !!                          from feedback files
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   obs_rea_sla : Driver for reading SLA data from the AVISO/CLS/feedback
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
   USE obs_sla_io               ! I/O for sla files
   USE netcdf                   ! NetCDF library

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE

   PUBLIC obs_rea_sla  ! Read the SLA observations from the AVISO/SLA database

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_read_sla.F90 2287 2010-10-18 07:53:52Z smasson $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE obs_rea_sla( kformat, &
      &                    sladata, knumfiles, cfilenames, &
      &                    kvars, kextr, kstp, ddobsini, ddobsend, &
      &                    ldignmis, ldmod, ldobstd )
      !!---------------------------------------------------------------------
      !!
      !!                   *** ROUTINE obs_rea_sla ***
      !!
      !! ** Purpose : Read from file the SLA data
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
      INTEGER :: kformat    ! Format of input data
      !                     ! 0: Feedback
      !                     ! 1: AVISO
      TYPE(obs_surf), INTENT(INOUT) :: sladata    ! SLA data to be read
      INTEGER, INTENT(IN) :: knumfiles      ! Number of files to read in
      CHARACTER(LEN=128), INTENT(IN) ::  &
         & cfilenames(knumfiles) ! File names to read in
      INTEGER, INTENT(IN) :: kvars     ! Number of variables in sladata
      INTEGER, INTENT(IN) :: kextr     ! Number of extra fields for each var in sladata
      INTEGER, INTENT(IN) :: kstp      ! Ocean time-step index
      LOGICAL, INTENT(IN) :: ldignmis    ! Ignore missing files
      LOGICAL, INTENT(IN) :: ldmod       ! Initialize model from input data
      LOGICAL, INTENT(INOUT), optional :: &
         & ldobstd        ! Read observation standard deviation from fb. file
      REAL(KIND=dp), INTENT(IN) :: ddobsini  ! Obs. ini time in YYYYMMDD.HHMMSS
      REAL(KIND=dp), INTENT(IN) :: ddobsend  ! Obs. end time in YYYYMMDD.HHMMSS
         
      !! * Local declarations
      CHARACTER(LEN=11), PARAMETER :: cpname='obs_rea_sla'
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
      INTEGER, DIMENSION(imaxmissions+1) :: &
         & ityp, &
         & itypmpp
      INTEGER, DIMENSION(:), ALLOCATABLE :: &
         & iobsi,    &
         & iobsj,    &
         & iproc,    &
         & iindx,    &
         & ifileidx, &
         & islaidx
      INTEGER :: itype 
      REAL(wp), DIMENSION(:), ALLOCATABLE :: &
         & zphi, &
         & zlam
      REAL(dp), DIMENSION(:), ALLOCATABLE :: &
         & zdat
      LOGICAL :: llvalprof
      LOGICAL :: llobstd
      TYPE(obfbdata), POINTER, DIMENSION(:) :: &
         & inpfiles
      REAL(dp), DIMENSION(knumfiles) :: &
         & djulini, &
         & djulend
      INTEGER, DIMENSION(knumfiles) :: &
         & iobspos, &
         & iobcpos
      INTEGER :: iobs
      INTEGER :: iobstot
      CHARACTER(len=8) :: cl_refdate
   
      ! Local initialization
      iobs = 0
      IF ( PRESENT(ldobstd) ) THEN
         IF (.NOT.ldmod) THEN
            llobstd = .false.
         ELSE
            llobstd = ldobstd
         ENDIF
      ELSE
         llobstd = .FALSE.
      ENDIF
 
      !-----------------------------------------------------------------------
      ! Check that the model part is just with feedback data files
      !-----------------------------------------------------------------------
      IF ( ldmod .AND. ( kformat /= 0 ) ) THEN
         CALL ctl_stop( 'Model can only be read from feedback data' )
         RETURN
      ENDIF

      !-----------------------------------------------------------------------
      ! Check that the prescribed obs err is just with feedback data files
      !-----------------------------------------------------------------------
      IF ( llobstd .AND. ( kformat /= 0 ) ) THEN
         CALL ctl_stop( 'Observation error can only be read from feedback files' )
         RETURN
      ENDIF

      !-----------------------------------------------------------------------
      ! Count the number of files needed and allocate the obfbdata type
      !-----------------------------------------------------------------------
      
      inobf = knumfiles
      
      ALLOCATE( inpfiles(inobf) )

      sla_files : DO jj = 1, inobf
          
         !---------------------------------------------------------------------
         ! Prints
         !---------------------------------------------------------------------
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) ' obs_rea_sla : Reading from file = ', &
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
               CALL init_obfbdata( inpfiles(jj) )
               IF(lwp) THEN
                  WRITE(numout,*)
                  WRITE(numout,*)'Reading from feedback file :', &
                     &           TRIM( cfilenames(jj) )
               ENDIF
               CALL read_obfbdata( TRIM( cfilenames(jj) ), inpfiles(jj), &
                  &                ldgrid = .TRUE. )
               IF ( inpfiles(jj)%nvar < 1 ) THEN
                  CALL ctl_stop( 'Feedback format error' )
                  RETURN
               ENDIF
               IF ( TRIM(inpfiles(jj)%cname(1)) /= 'SLA' ) THEN
                  CALL ctl_stop( 'Feedback format error' )
                  RETURN
               ENDIF 
               IF ( ldmod .AND. ( ( inpfiles(jj)%nadd == 0 ) .OR.&
                  &               ( inpfiles(jj)%next == 0 ) ) ) THEN
                  CALL ctl_stop( 'Model not in input data' )
                  RETURN
               ENDIF
            ELSEIF ( kformat == 1) THEN
               CALL read_avisofile( TRIM( cfilenames(jj) ), inpfiles(jj), &
               &                    numout, lwp, .TRUE. )
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
               IF ( inpfiles(jj)%ioqc(ji) > 2 ) CYCLE
               IF ( inpfiles(jj)%ivqc(ji,1) > 2 ) CYCLE
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
               IF ( inpfiles(jj)%ioqc(ji) > 2 ) CYCLE
               IF ( inpfiles(jj)%ivqc(ji,1) > 2 ) CYCLE
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
               IF ( inpfiles(jj)%ioqc(ji) > 2 ) CYCLE
               IF ( inpfiles(jj)%ivqc(ji,1) > 2 ) CYCLE
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
               IF ( inpfiles(jj)%ioqc(ji) > 2 ) CYCLE
               IF ( inpfiles(jj)%ivqc(ji,1) > 2 ) CYCLE
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
      
      END DO sla_files

      IF (llobstd) THEN
         
         DO jj = 1, inobf
            iobspos(jj) = -1
            iobcpos(jj) = -1
            DO ji = 1,inpfiles(jj)%nadd
               IF ( TRIM(inpfiles(jj)%caddname(ji)) == 'OSTD' ) THEN
                  iobspos(jj)=ji
               ENDIF
               IF ( TRIM(inpfiles(jj)%caddname(ji)) == 'OCNT' ) THEN
                  iobcpos(jj)=ji
               ENDIF
            END DO
         END DO
         llobstd = ( ( MINVAL(iobspos) > 0 ) .AND. ( MINVAL(iobcpos) > 0 ) )
         IF (llobstd) THEN
            IF (lwp) WRITE(numout,*)'SLA superobs information present.'
         ELSE
            IF (lwp) WRITE(numout,*)'SLA superobs information not present.'
         ENDIF

      ENDIF
      
      !-----------------------------------------------------------------------
      ! Get the time ordered indices of the input data
      !-----------------------------------------------------------------------

      !---------------------------------------------------------------------
      !  Loop over input data files to count total number of profiles
      !---------------------------------------------------------------------
      iobstot = 0
      DO jj = 1, inobf
         DO ji = 1, inpfiles(jj)%nobs
            IF ( inpfiles(jj)%ioqc(ji) > 2 ) CYCLE
            IF ( inpfiles(jj)%ivqc(ji,1) > 2 ) CYCLE
            IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
               & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
               iobstot = iobstot + 1
            ENDIF
         END DO
      END DO

      ALLOCATE( iindx(iobstot), ifileidx(iobstot), &
         &      islaidx(iobstot), zdat(iobstot) )
      jk = 0
      DO jj = 1, inobf
         DO ji = 1, inpfiles(jj)%nobs
            IF ( inpfiles(jj)%ioqc(ji) > 2 ) CYCLE
            IF ( inpfiles(jj)%ivqc(ji,1) > 2 ) CYCLE
            IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
               & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
               jk = jk + 1
               ifileidx(jk) = jj
               islaidx(jk) = ji
               zdat(jk)     = inpfiles(jj)%ptim(ji)
            ENDIF
         END DO
      END DO
      CALL sort_dp_indx( iobstot, &
         &               zdat,     &
         &               iindx   )
      
      CALL obs_surf_alloc( sladata, iobs, kvars, kextr, kstp )
      
      ! * Read obs/positions, QC, all variable and assign to sladata
 
      iobs = 0

      ityp   (:) = 0
      itypmpp(:) = 0
      
      DO jk = 1, iobstot
         
         jj = ifileidx(iindx(jk))
         ji = islaidx(iindx(jk))

         IF ( inpfiles(jj)%ioqc(ji) > 2 ) CYCLE
         IF ( inpfiles(jj)%ivqc(ji,1) > 2 ) CYCLE

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


               ! SLA time coordinates
               sladata%nyea(iobs) = iyea
               sladata%nmon(iobs) = imon
               sladata%nday(iobs) = iday
               sladata%nhou(iobs) = ihou
               sladata%nmin(iobs) = imin
               
               ! SLA space coordinates
               sladata%rlam(iobs) = inpfiles(jj)%plam(ji)
               sladata%rphi(iobs) = inpfiles(jj)%pphi(ji)

               ! Coordinate search parameters
               sladata%mi  (iobs) = inpfiles(jj)%iobsi(ji,1)
               sladata%mj  (iobs) = inpfiles(jj)%iobsj(ji,1)
               
               ! Instrument type
               READ( inpfiles(jj)%cdtyp(ji), '(I4)' ) itype
               sladata%ntyp(iobs) = itype
               ityp(itype+1) = ityp(itype+1) + 1

               ! Identifier
               sladata%cwmo(iobs) = inpfiles(jj)%cdwmo(ji)

               ! Bookkeeping data to match observations
               sladata%nsidx(iobs) = iobs
               sladata%nsfil(iobs) = iindx(jk)

               ! QC flags
               sladata%nqc(iobs) = inpfiles(jj)%ivqc(ji,1)

               ! Observed value
               sladata%robs(iobs,1) = inpfiles(jj)%pob(1,ji,1)


               ! Model and MDT is set to fbrmdi unless read from file
               IF ( ldmod ) THEN
                  sladata%rmod(iobs,1) = inpfiles(jj)%padd(1,ji,1,1)
                  sladata%rext(iobs,1) = inpfiles(jj)%padd(1,ji,2,1)
                  sladata%rext(iobs,2) = inpfiles(jj)%pext(1,ji,1)
                  IF (llobstd) THEN
                     sladata%rext(iobs,3) = &
                        & inpfiles(jj)%padd(1,ji,iobspos(jj),1)
                     sladata%rext(iobs,4) = &
                        & inpfiles(jj)%padd(1,ji,iobcpos(jj),1)
                  ENDIF
               ELSE
                  sladata%rmod(iobs,1) = fbrmdi
                  sladata%rext(iobs,:) = fbrmdi
               ENDIF

            ENDIF
         ENDIF

      END DO

      !-----------------------------------------------------------------------
      ! Sum up over processors
      !-----------------------------------------------------------------------
      
      CALL obs_mpp_sum_integer( iobs, iobsmpp )
      CALL obs_mpp_sum_integers( ityp, itypmpp, imaxmissions + 1 )
      
      !-----------------------------------------------------------------------
      ! Output number of observations.
      !-----------------------------------------------------------------------
      IF (lwp) THEN

         WRITE(numout,*)
         WRITE(numout,'(1X,A)')'Altimeter satellites'
         WRITE(numout,'(1X,A)')'--------------------'
         DO jj = 1,8
            IF ( itypmpp(jj) > 0 ) THEN
               WRITE(numout,'(1X,A38,A2,I10)')calttyp(jj-1),'= ',itypmpp(jj)
            ENDIF
         END DO
         WRITE(numout,'(1X,A50)')'--------------------------------------------------'
         WRITE(numout,'(1X,A40,I10)')'Total                                 = ',iobsmpp
         WRITE(numout,*)

      ENDIF

      !-----------------------------------------------------------------------
      ! Deallocate temporary data
      !-----------------------------------------------------------------------
      DEALLOCATE( ifileidx, islaidx, zdat )

      !-----------------------------------------------------------------------
      ! Deallocate input data
      !-----------------------------------------------------------------------
      DO jj = 1, inobf
         CALL dealloc_obfbdata( inpfiles(jj) )
      END DO
      DEALLOCATE( inpfiles )

      !-----------------------------------------------------------------------
      ! Reset ldobstd if the data is present
      !-----------------------------------------------------------------------
      IF ( PRESENT(ldobstd) ) THEN
         ldobstd = llobstd
      ENDIF

   END SUBROUTINE obs_rea_sla

END MODULE obs_read_sla
