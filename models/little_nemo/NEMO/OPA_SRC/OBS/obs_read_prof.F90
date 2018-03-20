MODULE obs_read_prof
   !!======================================================================
   !!                       ***  MODULE obs_read_prof  ***
   !! Observation diagnostics: Read the T and S profile observations
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   obs_rea_pro_dri : Driver for reading profile obs
   !!----------------------------------------------------------------------

   !! * Modules used   
   USE par_kind                 ! Precision variables
   USE par_oce                  ! Ocean parameters
   USE in_out_manager           ! I/O manager
   USE dom_oce                  ! Ocean space and time domain variables
   USE obs_mpp                  ! MPP support routines for observation diagnostics
   USE julian                   ! Julian date routines
   USE obs_utils                ! Observation operator utility functions
   USE obs_prep                 ! Prepare observation arrays
   USE obs_grid                 ! Grid search
   USE obs_sort                 ! Sorting observation arrays
   USE obs_profiles_def         ! Profile definitions
   USE obs_conv                 ! Various conversion routines
   USE obs_types                ! Observation type definitions
   USE netcdf                   ! NetCDF library
   USE obs_oper                 ! Observation operators
   USE obs_prof_io              ! Profile files I/O (non-FB files)
   USE lib_mpp                  ! For ctl_warn/stop

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE

   PUBLIC obs_rea_pro_dri  ! Read the profile observations 

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_read_prof.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS
 
   SUBROUTINE obs_rea_pro_dri( kformat, &
      &                        profdata, knumfiles, cfilenames, &
      &                        kvars, kextr, kstp, ddobsini, ddobsend, &
      &                        ldt3d, lds3d, ldignmis, ldsatt, ldavtimset, &
      &                        ldmod, kdailyavtypes )
      !!---------------------------------------------------------------------
      !!
      !!                   *** ROUTINE obs_rea_pro_dri ***
      !!
      !! ** Purpose : Read from file the profile observations
      !!
      !! ** Method  : Depending on kformat either ENACT, CORIOLIS or
      !!              feedback data files are read
      !!
      !! ** Action  : 
      !!
      !! References : 
      !!
      !! History :  
      !!      ! :  2009-09 (K. Mogensen) : New merged version of old routines
      !!----------------------------------------------------------------------
      !! * Modules used
   
      !! * Arguments
      INTEGER ::  kformat    ! Format of input data
      !                      ! 1: ENACT
      !                      ! 2: Coriolis
      TYPE(obs_prof), INTENT(OUT) ::  profdata     ! Profile data to be read
      INTEGER, INTENT(IN) :: knumfiles      ! Number of files to read in
      CHARACTER(LEN=128), INTENT(IN) ::  &
         & cfilenames(knumfiles)  ! File names to read in
      INTEGER, INTENT(IN) :: kvars      ! Number of variables in profdata
      INTEGER, INTENT(IN) :: kextr      ! Number of extra fields for each var in profdata
      INTEGER, INTENT(IN) :: kstp        ! Ocean time-step index
      LOGICAL, INTENT(IN) :: ldt3d       ! Observed variables switches
      LOGICAL, INTENT(IN) :: lds3d
      LOGICAL, INTENT(IN) :: ldignmis    ! Ignore missing files
      LOGICAL, INTENT(IN) :: ldsatt      ! Compute salinity at all temperature points
      LOGICAL, INTENT(IN) :: ldavtimset  ! Correct time for daily averaged data
      LOGICAL, INTENT(IN) :: ldmod       ! Initialize model from input data
      REAL(KIND=dp), INTENT(IN) :: ddobsini    ! Obs. ini time in YYYYMMDD.HHMMSS
      REAL(KIND=dp), INTENT(IN) :: ddobsend    ! Obs. end time in YYYYMMDD.HHMMSS
      INTEGER, DIMENSION(imaxavtypes), OPTIONAL :: &
         & kdailyavtypes

      !! * Local declarations
      CHARACTER(LEN=15), PARAMETER :: cpname='obs_rea_pro_dri'
      INTEGER :: jvar
      INTEGER :: ji
      INTEGER :: jj
      INTEGER :: jk
      INTEGER :: ij
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
      INTEGER, DIMENSION(ntyp1770+1) :: &
         & itypt,    &
         & ityptmpp, &
         & ityps,    &
         & itypsmpp 
      INTEGER :: it3dtmpp
      INTEGER :: is3dtmpp
      INTEGER :: ip3dtmpp
      INTEGER, DIMENSION(:), ALLOCATABLE :: &
         & iobsi,    &
         & iobsj,    &
         & iproc,    &
         & iindx,    &
         & ifileidx, &
         & iprofidx
      INTEGER :: itype
      INTEGER, DIMENSION(imaxavtypes) :: &
         & idailyavtypes
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
      INTEGER :: iprof
      INTEGER :: iproftot
      INTEGER :: it3dt0
      INTEGER :: is3dt0
      INTEGER :: it3dt
      INTEGER :: is3dt
      INTEGER :: ip3dt
      INTEGER, DIMENSION(kvars) :: &
         & iv3dt
      CHARACTER(len=8) :: cl_refdate
   
      ! Local initialization
      iprof = 0
      it3dt0 = 0
      is3dt0 = 0
      ip3dt = 0

      ! Daily average types
      IF ( PRESENT(kdailyavtypes) ) THEN
         idailyavtypes(:) = kdailyavtypes(:)
      ELSE
         idailyavtypes(:) = -1
      ENDIF

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

      prof_files : DO jj = 1, inobf
          
         !---------------------------------------------------------------------
         ! Prints
         !---------------------------------------------------------------------
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) ' obs_rea_pro_dri : Reading from file = ', &
               & TRIM( TRIM( cfilenames(jj) ) )
            WRITE(numout,*) ' ~~~~~~~~~~~~~~~'
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
               IF ( inpfiles(jj)%nvar < 2 ) THEN
                  CALL ctl_stop( 'Feedback format error' )
                  RETURN
               ENDIF
               IF ( TRIM(inpfiles(jj)%cname(1)) /= 'POTM' ) THEN
                  CALL ctl_stop( 'Feedback format error' )
                  RETURN
               ENDIF
               IF ( TRIM(inpfiles(jj)%cname(2)) /= 'PSAL' ) THEN
                  CALL ctl_stop( 'Feedback format error' )
                  RETURN
               ENDIF
               IF ( ldmod .AND. ( inpfiles(jj)%nadd == 0 ) ) THEN
                  CALL ctl_stop( 'Model not in input data' )
                  RETURN
               ENDIF
            ELSEIF ( kformat == 1 ) THEN
               CALL read_enactfile( TRIM( cfilenames(jj) ), inpfiles(jj), &
                  &                 numout, lwp, .TRUE. )
            ELSEIF ( kformat == 2 ) THEN
               CALL read_coriofile( TRIM( cfilenames(jj) ), inpfiles(jj), &
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

            IF ( ldavtimset ) THEN
               DO ji = 1, inpfiles(jj)%nobs
                  ! 
                  !  for daily averaged data for example
                  !  MRB data (itype==820) force the time
                  !  to be the  end of the day
                  !
                  READ( inpfiles(jj)%cdtyp(ji), '(I4)' ) itype
                  IF ( ANY (idailyavtypes == itype ) ) THEN
                     inpfiles(jj)%ptim(ji) = &
                     & INT(inpfiles(jj)%ptim(ji)) + 1
                  ENDIF
               END DO
            ENDIF
            
            IF ( inpfiles(jj)%nobs > 0 ) THEN
               inpfiles(jj)%iproc = -1
               inpfiles(jj)%iobsi = -1
               inpfiles(jj)%iobsj = -1
            ENDIF
            inowin = 0
            DO ji = 1, inpfiles(jj)%nobs
               IF ( inpfiles(jj)%ioqc(ji) > 2 ) CYCLE
               IF ( ( inpfiles(jj)%ivqc(ji,1) > 2 ) .AND. &
                  & ( inpfiles(jj)%ivqc(ji,2) > 2 )) CYCLE
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
               IF ( ( inpfiles(jj)%ivqc(ji,1) > 2 ) .AND. &
                  & ( inpfiles(jj)%ivqc(ji,2) > 2 )) CYCLE
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
               IF ( ( inpfiles(jj)%ivqc(ji,1) > 2 ) .AND. &
                  & ( inpfiles(jj)%ivqc(ji,2) > 2 )) CYCLE
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
               IF ( ( inpfiles(jj)%ivqc(ji,1) > 2 ) .AND. &
                  & ( inpfiles(jj)%ivqc(ji,2) > 2 )) CYCLE
               IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
                  & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
                  IF ( nproc == 0 ) THEN
                     IF ( inpfiles(jj)%iproc(ji,1) >  nproc ) CYCLE
                  ELSE
                     IF ( inpfiles(jj)%iproc(ji,1) /= nproc ) CYCLE
                  ENDIF
                  llvalprof = .FALSE.
                  IF ( ldt3d ) THEN
                     loop_t_count : DO ij = 1,inpfiles(jj)%nlev
                        IF ( inpfiles(jj)%pdep(ij,ji) >= 6000. ) &
                           & CYCLE
                        IF ( ( inpfiles(jj)%ivlqc(ij,ji,1) <= 2 ) .AND. &
                           & ( inpfiles(jj)%idqc(ij,ji) <= 2 ) ) THEN
                           it3dt0 = it3dt0 + 1
                        ENDIF
                     END DO loop_t_count
                  ENDIF
                  IF ( lds3d ) THEN
                     loop_s_count : DO ij = 1,inpfiles(jj)%nlev
                        IF ( inpfiles(jj)%pdep(ij,ji) >= 6000. ) &
                           & CYCLE
                        IF ( ( inpfiles(jj)%ivlqc(ij,ji,2) <= 2 ) .AND. &
                           & ( inpfiles(jj)%idqc(ij,ji) <= 2 ) ) THEN
                           is3dt0 = is3dt0 + 1
                        ENDIF
                     END DO loop_s_count
                  ENDIF
                  loop_p_count : DO ij = 1,inpfiles(jj)%nlev
                     IF ( inpfiles(jj)%pdep(ij,ji) >= 6000. ) &
                        & CYCLE
                     IF ( ( ( ( inpfiles(jj)%ivlqc(ij,ji,1) <= 2 ) .AND. &
                        &     ( inpfiles(jj)%idqc(ij,ji) <= 2 ) ) .AND. &
                        &     ldt3d ) .OR. &
                        & ( ( ( inpfiles(jj)%ivlqc(ij,ji,2) <= 2 ) .AND. &
                        &     ( inpfiles(jj)%idqc(ij,ji) <= 2 ) ) .AND. &
                        &     lds3d ) ) THEN
                        ip3dt = ip3dt + 1
                        llvalprof = .TRUE.
                     ENDIF
                  END DO loop_p_count

                  IF ( llvalprof ) iprof = iprof + 1

               ENDIF
            END DO

         ENDIF

      END DO prof_files
      
      !-----------------------------------------------------------------------
      ! Get the time ordered indices of the input data
      !-----------------------------------------------------------------------

      !---------------------------------------------------------------------
      !  Loop over input data files to count total number of profiles
      !---------------------------------------------------------------------
      iproftot = 0
      DO jj = 1, inobf
         DO ji = 1, inpfiles(jj)%nobs
            IF ( inpfiles(jj)%ioqc(ji) > 2 ) CYCLE
            IF ( ( inpfiles(jj)%ivqc(ji,1) > 2 ) .AND. &
               & ( inpfiles(jj)%ivqc(ji,2) > 2 )) CYCLE
            IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
               & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
               iproftot = iproftot + 1
            ENDIF
         END DO
      END DO

      ALLOCATE( iindx(iproftot), ifileidx(iproftot), &
         &      iprofidx(iproftot), zdat(iproftot) )
      jk = 0
      DO jj = 1, inobf
         DO ji = 1, inpfiles(jj)%nobs
            IF ( inpfiles(jj)%ioqc(ji) > 2 ) CYCLE
            IF ( ( inpfiles(jj)%ivqc(ji,1) > 2 ) .AND. &
               & ( inpfiles(jj)%ivqc(ji,2) > 2 )) CYCLE
            IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND. &
               & ( inpfiles(jj)%ptim(ji) <= djulend(jj) )       ) THEN
               jk = jk + 1
               ifileidx(jk) = jj
               iprofidx(jk) = ji
               zdat(jk)     = inpfiles(jj)%ptim(ji)
            ENDIF
         END DO
      END DO
      CALL sort_dp_indx( iproftot, &
         &               zdat,     &
         &               iindx   )
      
      iv3dt(:) = -1
      IF (ldsatt) THEN
         iv3dt(1) = ip3dt
         iv3dt(2) = ip3dt
      ELSE
         iv3dt(1) = it3dt0
         iv3dt(2) = is3dt0
      ENDIF
      CALL obs_prof_alloc( profdata, kvars, kextr, iprof, iv3dt, &
         &                 kstp, jpi, jpj, jpk )
      
      ! * Read obs/positions, QC, all variable and assign to profdata

      profdata%nprof     = 0
      profdata%nvprot(:) = 0

      iprof = 0

      ip3dt = 0
      it3dt = 0
      is3dt = 0
      itypt   (:) = 0
      ityptmpp(:) = 0
      
      ityps   (:) = 0
      itypsmpp(:) = 0
      
      
      DO jk = 1, iproftot
         
         jj = ifileidx(iindx(jk))
         ji = iprofidx(iindx(jk))

         IF ( inpfiles(jj)%ioqc(ji) > 2 ) CYCLE
         IF ( ( inpfiles(jj)%ivqc(ji,1) > 2 ) .AND. &
            & ( inpfiles(jj)%ivqc(ji,2) > 2 )) CYCLE

         IF ( ( inpfiles(jj)%ptim(ji) >  djulini(jj) ) .AND.  &
            & ( inpfiles(jj)%ptim(ji) <= djulend(jj) ) ) THEN
            
            IF ( nproc == 0 ) THEN
               IF ( inpfiles(jj)%iproc(ji,1) >  nproc ) CYCLE
            ELSE
               IF ( inpfiles(jj)%iproc(ji,1) /= nproc ) CYCLE
            ENDIF
            
            llvalprof = .FALSE.

            IF ( inpfiles(jj)%ioqc(ji) > 2 ) CYCLE

            IF ( ( inpfiles(jj)%ivqc(ji,1) > 2 ) .AND. &
               & ( inpfiles(jj)%ivqc(ji,2) > 2 ) ) CYCLE

            loop_prof : DO ij = 1, inpfiles(jj)%nlev
               
               IF ( inpfiles(jj)%pdep(ij,ji) >= 6000. ) &
                  & CYCLE
               
               IF ( ( inpfiles(jj)%ivlqc(ij,ji,1) <= 2 ) .AND. &
                  & ( inpfiles(jj)%idqc(ij,ji) <= 2 ) ) THEN
                  
                  llvalprof = .TRUE. 
                  EXIT loop_prof
                  
               ENDIF
               
               IF ( ( inpfiles(jj)%ivlqc(ij,ji,2) <= 2 ) .AND. &
                  & ( inpfiles(jj)%idqc(ij,ji) <= 2 ) ) THEN
                  
                  llvalprof = .TRUE. 
                  EXIT loop_prof
                  
               ENDIF
               
            END DO loop_prof
            
            ! Set profile information
            
            IF ( llvalprof ) THEN
               
               iprof = iprof + 1

               CALL jul2greg( isec,                   &
                  &           imin,                   &
                  &           ihou,                   &
                  &           iday,                   &
                  &           imon,                   &
                  &           iyea,                   &
                  &           inpfiles(jj)%ptim(ji), &
                  &           irefdate(jj) )


               ! Profile time coordinates
               profdata%nyea(iprof) = iyea
               profdata%nmon(iprof) = imon
               profdata%nday(iprof) = iday
               profdata%nhou(iprof) = ihou
               profdata%nmin(iprof) = imin
               
               ! Profile space coordinates
               profdata%rlam(iprof) = inpfiles(jj)%plam(ji)
               profdata%rphi(iprof) = inpfiles(jj)%pphi(ji)

               ! Coordinate search parameters
               profdata%mi  (iprof,:) = inpfiles(jj)%iobsi(ji,1)
               profdata%mj  (iprof,:) = inpfiles(jj)%iobsj(ji,1)
               
               ! Profile WMO number
               profdata%cwmo(iprof) = inpfiles(jj)%cdwmo(ji)
               
               ! Instrument type
               READ( inpfiles(jj)%cdtyp(ji), '(I4)' ) itype
               profdata%ntyp(iprof) = itype
               
               ! QC stuff

               profdata%nqc(iprof)     = inpfiles(jj)%ioqc(ji)
               profdata%nqcf(:,iprof)  = inpfiles(jj)%ioqcf(:,ji)
               profdata%ipqc(iprof)    = inpfiles(jj)%ipqc(ji)
               profdata%ipqcf(:,iprof) = inpfiles(jj)%ipqcf(:,ji)
               profdata%itqc(iprof)    = inpfiles(jj)%itqc(ji)
               profdata%itqcf(:,iprof) = inpfiles(jj)%itqcf(:,ji)
               profdata%ivqc(iprof,:)  = inpfiles(jj)%ivqc(ji,:)
               profdata%ivqcf(:,iprof,:) = inpfiles(jj)%ivqcf(:,ji,:)

               ! Bookkeeping data to match profiles
               profdata%npidx(iprof) = iprof
               profdata%npfil(iprof) = iindx(jk)

               ! Observation QC flag (whole profile)
               profdata%nqc(iprof)  = 0 !TODO

               loop_p : DO ij = 1, inpfiles(jj)%nlev            
                  
                  IF ( inpfiles(jj)%pdep(ij,ji) >= 6000. ) &
                     & CYCLE

                  IF (ldsatt) THEN

                     IF ( ( ( ( inpfiles(jj)%ivlqc(ij,ji,1) <= 2 ) .AND. &
                        &     ( inpfiles(jj)%idqc(ij,ji) <= 2 ) ) .AND. &
                        &     ldt3d ) .OR. &
                        & ( ( ( inpfiles(jj)%ivlqc(ij,ji,2) <= 2 ) .AND. &
                        &     ( inpfiles(jj)%idqc(ij,ji) <= 2 ) ) .AND. &
                        &     lds3d ) ) THEN
                        ip3dt = ip3dt + 1
                     ELSE
                        CYCLE
                     ENDIF
                     
                  ENDIF

                  IF ( ( ( ( inpfiles(jj)%ivlqc(ij,ji,1) <= 2 ) .AND. &
                     &     ( inpfiles(jj)%idqc(ij,ji) <= 2 ) ) .AND. &
                     &       ldt3d ) .OR. ldsatt ) THEN
                     
                     IF (ldsatt) THEN

                        it3dt = ip3dt

                     ELSE

                        it3dt = it3dt + 1
                        
                     ENDIF

                     ! Depth of T observation
                     profdata%var(1)%vdep(it3dt) = &
                        &                inpfiles(jj)%pdep(ij,ji)
                     
                     ! Depth of T observation QC
                     profdata%var(1)%idqc(it3dt) = &
                        &                inpfiles(jj)%idqc(ij,ji)
                     
                     ! Depth of T observation QC flags
                     profdata%var(1)%idqcf(:,it3dt) = &
                        &                inpfiles(jj)%idqcf(:,ij,ji)
                     
                     ! Profile index
                     profdata%var(1)%nvpidx(it3dt) = iprof
                     
                     ! Vertical index in original profile
                     profdata%var(1)%nvlidx(it3dt) = ij

                     ! Profile potential T value
                     IF ( ( inpfiles(jj)%ivlqc(ij,ji,1) <= 2 ) .AND. &
                        & ( inpfiles(jj)%idqc(ij,ji) <= 2 ) ) THEN
                        profdata%var(1)%vobs(it3dt) = &
                           &                inpfiles(jj)%pob(ij,ji,1)
                        IF ( ldmod ) THEN
                           profdata%var(1)%vmod(it3dt) = &
                              &                inpfiles(jj)%padd(ij,ji,1,1)
                        ENDIF
                        ! Count number of profile T data as function of type
                        itypt( profdata%ntyp(iprof) + 1 ) = &
                           & itypt( profdata%ntyp(iprof) + 1 ) + 1
                     ELSE
                        profdata%var(1)%vobs(it3dt) = fbrmdi
                     ENDIF

                     ! Profile T qc
                     profdata%var(1)%nvqc(it3dt) = &
                        & inpfiles(jj)%ivlqc(ij,ji,1)

                     ! Profile T qc flags
                     profdata%var(1)%nvqcf(:,it3dt) = &
                        & inpfiles(jj)%ivlqcf(:,ij,ji,1)

                     ! Profile insitu T value
                     profdata%var(1)%vext(it3dt,1) = &
                        &                inpfiles(jj)%pext(ij,ji,1)
                     
                  ENDIF
                  
                  IF ( ( ( ( inpfiles(jj)%ivlqc(ij,ji,2) <= 2 ) .AND. &
                     &   ( inpfiles(jj)%idqc(ij,ji) <= 2 ) ) .AND. &
                     &   lds3d ) .OR. ldsatt ) THEN
                     
                     IF (ldsatt) THEN

                        is3dt = ip3dt

                     ELSE

                        is3dt = is3dt + 1
                        
                     ENDIF

                     ! Depth of S observation
                     profdata%var(2)%vdep(is3dt) = &
                        &                inpfiles(jj)%pdep(ij,ji)
                     
                     ! Depth of S observation QC
                     profdata%var(2)%idqc(is3dt) = &
                        &                inpfiles(jj)%idqc(ij,ji)
                     
                     ! Depth of S observation QC flags
                     profdata%var(2)%idqcf(:,is3dt) = &
                        &                inpfiles(jj)%idqcf(:,ij,ji)
                     
                     ! Profile index
                     profdata%var(2)%nvpidx(is3dt) = iprof
                     
                     ! Vertical index in original profile
                     profdata%var(2)%nvlidx(is3dt) = ij

                     ! Profile S value
                     IF ( ( inpfiles(jj)%ivlqc(ij,ji,2) <= 2 ) .AND. &
                        & ( inpfiles(jj)%idqc(ij,ji) <= 2 ) ) THEN
                        profdata%var(2)%vobs(is3dt) = &
                           &                inpfiles(jj)%pob(ij,ji,2)
                        IF ( ldmod ) THEN
                           profdata%var(2)%vmod(is3dt) = &
                              &                inpfiles(jj)%padd(ij,ji,1,2)
                        ENDIF
                        ! Count number of profile S data as function of type
                        ityps( profdata%ntyp(iprof) + 1 ) = &
                           & ityps( profdata%ntyp(iprof) + 1 ) + 1
                     ELSE
                        profdata%var(2)%vobs(is3dt) = fbrmdi
                     ENDIF
                     
                     ! Profile S qc
                     profdata%var(2)%nvqc(is3dt) = &
                        & inpfiles(jj)%ivlqc(ij,ji,2)

                     ! Profile S qc flags
                     profdata%var(2)%nvqcf(:,is3dt) = &
                        & inpfiles(jj)%ivlqcf(:,ij,ji,2)

                  ENDIF
            
               END DO loop_p

            ENDIF

         ENDIF

      END DO

      !-----------------------------------------------------------------------
      ! Sum up over processors
      !-----------------------------------------------------------------------
      
      CALL obs_mpp_sum_integer ( it3dt0, it3dtmpp )
      CALL obs_mpp_sum_integer ( is3dt0, is3dtmpp )
      CALL obs_mpp_sum_integer ( ip3dt, ip3dtmpp )
      
      CALL obs_mpp_sum_integers( itypt, ityptmpp, ntyp1770 + 1 )
      CALL obs_mpp_sum_integers( ityps, itypsmpp, ntyp1770 + 1 )
      
      !-----------------------------------------------------------------------
      ! Output number of observations.
      !-----------------------------------------------------------------------
      IF(lwp) THEN
         WRITE(numout,*) 
         WRITE(numout,'(1X,A)') 'Profile data'
         WRITE(numout,'(1X,A)') '------------'
         WRITE(numout,*) 
         WRITE(numout,'(1X,A)') 'Profile T data'
         WRITE(numout,'(1X,A)') '--------------'
         DO ji = 0, ntyp1770
            IF ( ityptmpp(ji+1) > 0 ) THEN
               WRITE(numout,'(1X,A3,1X,A48,A3,I8)') ctypshort(ji), &
                  & cwmonam1770(ji)(1:52),' = ', &
                  & ityptmpp(ji+1)
            ENDIF
         END DO
         WRITE(numout,'(1X,A)') &
            & '---------------------------------------------------------------'
         WRITE(numout,'(1X,A55,I8)') &
            & 'Total profile T data                                 = ',&
            & it3dtmpp
         WRITE(numout,'(1X,A)') &
            & '---------------------------------------------------------------'
         WRITE(numout,*) 
         WRITE(numout,'(1X,A)') 'Profile S data'
         WRITE(numout,'(1X,A)') '--------------'
         DO ji = 0, ntyp1770
            IF ( itypsmpp(ji+1) > 0 ) THEN
               WRITE(numout,'(1X,A3,1X,A48,A3,I8)') ctypshort(ji), &
                  & cwmonam1770(ji)(1:52),' = ', &
                  & itypsmpp(ji+1)
            ENDIF
         END DO
         WRITE(numout,'(1X,A)') &
            & '---------------------------------------------------------------'
         WRITE(numout,'(1X,A55,I8)') &
            & 'Total profile S data                                 = ',&
            & is3dtmpp
         WRITE(numout,'(1X,A)') &
            & '---------------------------------------------------------------'
         WRITE(numout,*) 
      ENDIF
      
      IF (ldsatt) THEN
         profdata%nvprot(1)    = ip3dt
         profdata%nvprot(2)    = ip3dt
         profdata%nvprotmpp(1) = ip3dtmpp
         profdata%nvprotmpp(2) = ip3dtmpp
      ELSE
         profdata%nvprot(1)    = it3dt
         profdata%nvprot(2)    = is3dt
         profdata%nvprotmpp(1) = it3dtmpp
         profdata%nvprotmpp(2) = is3dtmpp
      ENDIF
      profdata%nprof        = iprof

      !-----------------------------------------------------------------------
      ! Model level search
      !-----------------------------------------------------------------------
      IF ( ldt3d ) THEN
         CALL obs_level_search( jpk, gdept_0, &
            & profdata%nvprot(1), profdata%var(1)%vdep, &
            & profdata%var(1)%mvk )
      ENDIF
      IF ( lds3d ) THEN
         CALL obs_level_search( jpk, gdept_0, &
            & profdata%nvprot(2), profdata%var(2)%vdep, &
            & profdata%var(2)%mvk )
      ENDIF
      
      !-----------------------------------------------------------------------
      ! Set model equivalent to 99999
      !-----------------------------------------------------------------------
      IF ( .NOT. ldmod ) THEN
         DO jvar = 1, kvars
            profdata%var(jvar)%vmod(:) = fbrmdi
         END DO
      ENDIF
      !-----------------------------------------------------------------------
      ! Deallocate temporary data
      !-----------------------------------------------------------------------
      DEALLOCATE( ifileidx, iprofidx, zdat )

      !-----------------------------------------------------------------------
      ! Deallocate input data
      !-----------------------------------------------------------------------
      DO jj = 1, inobf
         CALL dealloc_obfbdata( inpfiles(jj) )
      END DO
      DEALLOCATE( inpfiles )

   END SUBROUTINE obs_rea_pro_dri

END MODULE obs_read_prof
