MODULE obs_prep
   !!=====================================================================
   !!                       ***  MODULE  obs_prep  ***
   !! Observation diagnostics: Prepare observation arrays: screening, 
   !!                          sorting, coordinate search
   !!=====================================================================

   !!---------------------------------------------------------------------
   !!   obs_pre_pro  : First level check and screening of T/S profiles
   !!   obs_pre_sla  : First level check and screening of SLA observations
   !!   obs_pre_sst  : First level check and screening of SLA observations
   !!   obs_pre_seaice : First level check and screening of sea ice observations
   !!   obs_pre_vel  : First level check and screening of velocity obs.
   !!   obs_scr      : Basic screening of the observations
   !!   obs_coo_tim  : Compute number of time steps to the observation time
   !!   obs_sor      : Sort the observation arrays
   !!---------------------------------------------------------------------
   !! * Modules used
   USE par_kind, ONLY : & ! Precision variables
      & wp   
   USE in_out_manager     ! I/O manager
   USE obs_profiles_def   ! Definitions for storage arrays for profiles
   USE obs_surf_def       ! Definitions for storage arrays for surface data
   USE obs_mpp, ONLY : &  ! MPP support routines for observation diagnostics
      & obs_mpp_sum_integer, &
      & obs_mpp_sum_integers
   USE obs_inter_sup      ! Interpolation support
   USE obs_oper           ! Observation operators
   USE lib_mpp, ONLY : &
      & ctl_warn, ctl_stop

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE

   PUBLIC &
      & obs_pre_pro, &    ! First level check and screening of profiles
      & obs_pre_sla, &    ! First level check and screening of SLA data
      & obs_pre_sst, &    ! First level check and screening of SLA data
      & obs_pre_seaice, & ! First level check and screening of sea ice data
      & obs_pre_vel, &     ! First level check and screening of velocity profiles
      & calc_month_len     ! Calculate the number of days in the months of a year  

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_prep.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE obs_pre_pro( profdata, prodatqc, ld_t3d, ld_s3d, ld_nea, &
      &                    kdailyavtypes )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE obs_pre_pro  ***
      !!
      !! ** Purpose : First level check and screening of T and S profiles
      !!
      !! ** Method  : First level check and screening of T and S profiles
      !!
      !! ** Action  : 
      !!
      !! References :
      !!   
      !! History :
      !!        !  2007-01  (K. Mogensen) Merge of obs_pre_t3d and obs_pre_s3d 
      !!        !  2007-03  (K. Mogensen) General handling of profiles
      !!        !  2007-06  (K. Mogensen et al) Reject obs. near land.
      !!----------------------------------------------------------------------
      !! * Modules used
      USE domstp              ! Domain: set the time-step
      USE par_oce             ! Ocean parameters
      USE dom_oce, ONLY : &   ! Geographical information
         & glamt,   &
         & gphit,   &
         & gdept_0, &
         & tmask,   &
         & nproc
      !! * Arguments
      TYPE(obs_prof), INTENT(INOUT) :: profdata     ! Full set of profile data
      TYPE(obs_prof), INTENT(INOUT) :: prodatqc     ! Subset of profile data not failing screening
      LOGICAL, INTENT(IN) :: ld_t3d         ! Switch for temperature
      LOGICAL, INTENT(IN) :: ld_s3d         ! Switch for salinity
      LOGICAL, INTENT(IN) :: ld_nea         ! Switch for rejecting observation near land
      INTEGER, DIMENSION(imaxavtypes), OPTIONAL :: &
         & kdailyavtypes! Types for daily averages
      !! * Local declarations   
      INTEGER :: iyea0         ! Initial date
      INTEGER :: imon0         !  - (year, month, day, hour, minute)
      INTEGER :: iday0   
      INTEGER :: ihou0
      INTEGER :: imin0
      INTEGER :: icycle        ! Current assimilation cycle
                               ! Counters for observations that
      INTEGER :: iotdobs       !  - outside time domain
      INTEGER :: iosdtobs      !  - outside space domain (temperature)
      INTEGER :: iosdsobs      !  - outside space domain (salinity)
      INTEGER :: ilantobs      !  - within a model land cell (temperature)
      INTEGER :: ilansobs      !  - within a model land cell (salinity)
      INTEGER :: inlatobs      !  - close to land (temperature)
      INTEGER :: inlasobs      !  - close to land (salinity)
      INTEGER :: igrdobs       !  - fail the grid search
                               ! Global counters for observations that
      INTEGER :: iotdobsmpp    !  - outside time domain
      INTEGER :: iosdtobsmpp   !  - outside space domain (temperature)
      INTEGER :: iosdsobsmpp   !  - outside space domain (salinity)
      INTEGER :: ilantobsmpp   !  - within a model land cell (temperature)
      INTEGER :: ilansobsmpp   !  - within a model land cell (salinity)
      INTEGER :: inlatobsmpp   !  - close to land (temperature)
      INTEGER :: inlasobsmpp   !  - close to land (salinity)
      INTEGER :: igrdobsmpp    !  - fail the grid search
      TYPE(obs_prof_valid) ::  llvalid     ! Profile selection 
      TYPE(obs_prof_valid), DIMENSION(profdata%nvar) :: &
         & llvvalid            ! T,S selection 
      INTEGER :: jvar          ! Variable loop variable
      INTEGER :: jobs          ! Obs. loop variable
      INTEGER :: jstp          ! Time loop variable
      INTEGER :: inrc          ! Time index variable
      
      IF(lwp) WRITE(numout,*)'obs_pre_pro : Preparing the profile observations...'

      ! Initial date initialization (year, month, day, hour, minute)
      iyea0 =   ndate0 / 10000
      imon0 = ( ndate0 - iyea0 * 10000 ) / 100
      iday0 =   ndate0 - iyea0 * 10000 - imon0 * 100
      ihou0 = 0
      imin0 = 0

      icycle = no     ! Assimilation cycle

      ! Diagnotics counters for various failures.

      iotdobs  = 0
      igrdobs  = 0
      iosdtobs = 0
      iosdsobs = 0
      ilantobs = 0
      ilansobs = 0
      inlatobs = 0
      inlasobs = 0

      ! -----------------------------------------------------------------------
      ! Find time coordinate for profiles
      ! -----------------------------------------------------------------------

      IF ( PRESENT(kdailyavtypes) ) THEN
         CALL obs_coo_tim_prof( icycle, &
            &                iyea0,   imon0,   iday0,   ihou0,   imin0,      &
            &                profdata%nprof,   profdata%nyea, profdata%nmon, &
            &                profdata%nday,    profdata%nhou, profdata%nmin, &
            &                profdata%ntyp,    profdata%nqc,  profdata%mstp, &
            &                iotdobs, kdailyavtypes = kdailyavtypes        )
      ELSE
         CALL obs_coo_tim_prof( icycle, &
            &                iyea0,   imon0,   iday0,   ihou0,   imin0,      &
            &                profdata%nprof,   profdata%nyea, profdata%nmon, &
            &                profdata%nday,    profdata%nhou, profdata%nmin, &
            &                profdata%ntyp,    profdata%nqc,  profdata%mstp, &
            &                iotdobs )
      ENDIF
      CALL obs_mpp_sum_integer( iotdobs, iotdobsmpp )
      
      ! -----------------------------------------------------------------------
      ! Check for profiles failing the grid search
      ! -----------------------------------------------------------------------

      CALL obs_coo_grd( profdata%nprof,   profdata%mi, profdata%mj, &
         &              profdata%nqc,     igrdobs                         )

      CALL obs_mpp_sum_integer( igrdobs, igrdobsmpp )

      ! -----------------------------------------------------------------------
      ! Reject all observations for profiles with nqc > 10
      ! -----------------------------------------------------------------------

      CALL obs_pro_rej( profdata )

      ! -----------------------------------------------------------------------
      ! Check for land points. This includes points below the model
      ! bathymetry so this is done for every point in the profile
      ! -----------------------------------------------------------------------

      ! Temperature

      CALL obs_coo_spc_3d( profdata%nprof,        profdata%nvprot(1),   &
         &                 profdata%npvsta(:,1),  profdata%npvend(:,1), &
         &                 jpi,                   jpj,                  &
         &                 jpk,                                         &
         &                 profdata%mi,           profdata%mj,          & 
         &                 profdata%var(1)%mvk,                         &
         &                 profdata%rlam,         profdata%rphi,        &
         &                 profdata%var(1)%vdep,                        &
         &                 glamt,                 gphit,                &
         &                 gdept_0,               tmask,                &
         &                 profdata%nqc,          profdata%var(1)%nvqc, &
         &                 iosdtobs,              ilantobs,             &
         &                 inlatobs,              ld_nea                )

      CALL obs_mpp_sum_integer( iosdtobs, iosdtobsmpp )
      CALL obs_mpp_sum_integer( ilantobs, ilantobsmpp )
      CALL obs_mpp_sum_integer( inlatobs, inlatobsmpp )

      ! Salinity

      CALL obs_coo_spc_3d( profdata%nprof,        profdata%nvprot(2),   &
         &                 profdata%npvsta(:,2),  profdata%npvend(:,2), &
         &                 jpi,                   jpj,                  &
         &                 jpk,                                         &
         &                 profdata%mi,           profdata%mj,          & 
         &                 profdata%var(2)%mvk,                         &
         &                 profdata%rlam,         profdata%rphi,        &
         &                 profdata%var(2)%vdep,                        &
         &                 glamt,                 gphit,                &
         &                 gdept_0,               tmask,                &
         &                 profdata%nqc,          profdata%var(2)%nvqc, &
         &                 iosdsobs,              ilansobs,             &
         &                 inlasobs,              ld_nea                )

      CALL obs_mpp_sum_integer( iosdsobs, iosdsobsmpp )
      CALL obs_mpp_sum_integer( ilansobs, ilansobsmpp )
      CALL obs_mpp_sum_integer( inlasobs, inlasobsmpp )

      ! -----------------------------------------------------------------------
      ! Copy useful data from the profdata data structure to
      ! the prodatqc data structure 
      ! -----------------------------------------------------------------------

      ! Allocate the selection arrays

      ALLOCATE( llvalid%luse(profdata%nprof) )
      DO jvar = 1,profdata%nvar
         ALLOCATE( llvvalid(jvar)%luse(profdata%nvprot(jvar)) )
      END DO

      ! We want all data which has qc flags <= 10

      llvalid%luse(:) = ( profdata%nqc(:)  <= 10 )
      DO jvar = 1,profdata%nvar
         llvvalid(jvar)%luse(:) = ( profdata%var(jvar)%nvqc(:) <= 10 )
      END DO

      ! The actual copying

      CALL obs_prof_compress( profdata,     prodatqc,       .TRUE.,  numout, &
         &                    lvalid=llvalid, lvvalid=llvvalid )

      ! Dellocate the selection arrays
      DEALLOCATE( llvalid%luse )
      DO jvar = 1,profdata%nvar
         DEALLOCATE( llvvalid(jvar)%luse )
      END DO

      ! -----------------------------------------------------------------------
      ! Print information about what observations are left after qc
      ! -----------------------------------------------------------------------

      ! Update the total observation counter array
      
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'obs_pre_pro :'
         WRITE(numout,*) '~~~~~~~~~~~'
         WRITE(numout,*)
         WRITE(numout,*) ' Profiles outside time domain                = ', &
            &            iotdobsmpp
         WRITE(numout,*) ' Remaining profiles that failed grid search  = ', &
            &            igrdobsmpp
         WRITE(numout,*) ' Remaining T data outside space domain       = ', &
            &            iosdtobsmpp
         WRITE(numout,*) ' Remaining T data at land points             = ', &
            &            ilantobsmpp
         IF (ld_nea) THEN
            WRITE(numout,*) ' Remaining T data near land points (removed) = ',&
               &            inlatobsmpp
         ELSE
            WRITE(numout,*) ' Remaining T data near land points (kept)    = ',&
               &            inlatobsmpp
         ENDIF
         WRITE(numout,*) ' T data accepted                             = ', &
            &            prodatqc%nvprotmpp(1)
         WRITE(numout,*) ' Remaining S data outside space domain       = ', &
            &            iosdsobsmpp
         WRITE(numout,*) ' Remaining S data at land points             = ', &
            &            ilansobsmpp
         IF (ld_nea) THEN
            WRITE(numout,*) ' Remaining S data near land points (removed) = ',&
               &            inlasobsmpp
         ELSE
            WRITE(numout,*) ' Remaining S data near land points (kept)    = ',&
               &            inlasobsmpp
         ENDIF
         WRITE(numout,*) ' S data accepted                             = ', &
            &            prodatqc%nvprotmpp(2)

         WRITE(numout,*)
         WRITE(numout,*) ' Number of observations per time step :'
         WRITE(numout,*)
         WRITE(numout,997)
         WRITE(numout,998)
      ENDIF
      
      DO jobs = 1, prodatqc%nprof
         inrc = prodatqc%mstp(jobs) + 2 - nit000
         prodatqc%npstp(inrc)  = prodatqc%npstp(inrc) + 1
         DO jvar = 1, prodatqc%nvar
            IF ( prodatqc%npvend(jobs,jvar) > 0 ) THEN
               prodatqc%nvstp(inrc,jvar) = prodatqc%nvstp(inrc,jvar) + &
                  &                      ( prodatqc%npvend(jobs,jvar) - &
                  &                        prodatqc%npvsta(jobs,jvar) + 1 )
            ENDIF
         END DO
      END DO
      
      
      CALL obs_mpp_sum_integers( prodatqc%npstp, prodatqc%npstpmpp, &
         &                       nitend - nit000 + 2 )
      DO jvar = 1, prodatqc%nvar
         CALL obs_mpp_sum_integers( prodatqc%nvstp(:,jvar), &
            &                       prodatqc%nvstpmpp(:,jvar), &
            &                       nitend - nit000 + 2 )
      END DO

      IF ( lwp ) THEN
         DO jstp = nit000 - 1, nitend
            inrc = jstp - nit000 + 2
            WRITE(numout,999) jstp, prodatqc%npstpmpp(inrc), &
               &                    prodatqc%nvstpmpp(inrc,1), &
               &                    prodatqc%nvstpmpp(inrc,2)
         END DO
      ENDIF

997   FORMAT(10X,'Time step',5X,'Profiles',5X,'Temperature',5X,'Salinity')
998   FORMAT(10X,'---------',5X,'--------',5X,'-----------',5X,'--------')
999   FORMAT(10X,I9,5X,I8,5X,I11,5X,I8)
      
   END SUBROUTINE obs_pre_pro

   SUBROUTINE obs_pre_sla( sladata, sladatqc, ld_sla, ld_nea )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE obs_pre_sla  ***
      !!
      !! ** Purpose : First level check and screening of SLA observations
      !!
      !! ** Method  : First level check and screening of SLA observations
      !!
      !! ** Action  : 
      !!
      !! References :
      !!   
      !! History :
      !!        !  2007-03  (A. Weaver, K. Mogensen) Original
      !!        !  2007-06  (K. Mogensen et al) Reject obs. near land.
      !!----------------------------------------------------------------------
      !! * Modules used
      USE domstp              ! Domain: set the time-step
      USE par_oce             ! Ocean parameters
      USE dom_oce, ONLY : &   ! Geographical information
         & glamt,   &
         & gphit,   &
         & tmask,   &
         & nproc
      !! * Arguments
      TYPE(obs_surf), INTENT(INOUT) :: sladata    ! Full set of SLA data
      TYPE(obs_surf), INTENT(INOUT) :: sladatqc   ! Subset of SLA data not failing screening
      LOGICAL, INTENT(IN) :: ld_sla         ! Switch for SLA data
      LOGICAL, INTENT(IN) :: ld_nea         ! Switch for rejecting observation near land
      !! * Local declarations
      INTEGER :: iyea0        ! Initial date
      INTEGER :: imon0        !  - (year, month, day, hour, minute)
      INTEGER :: iday0    
      INTEGER :: ihou0    
      INTEGER :: imin0
      INTEGER :: icycle       ! Current assimilation cycle
                              ! Counters for observations that
      INTEGER :: iotdobs      !  - outside time domain
      INTEGER :: iosdsobs     !  - outside space domain
      INTEGER :: ilansobs     !  - within a model land cell
      INTEGER :: inlasobs     !  - close to land
      INTEGER :: igrdobs      !  - fail the grid search
                              ! Global counters for observations that
      INTEGER :: iotdobsmpp     !  - outside time domain
      INTEGER :: iosdsobsmpp    !  - outside space domain
      INTEGER :: ilansobsmpp    !  - within a model land cell
      INTEGER :: inlasobsmpp    !  - close to land
      INTEGER :: igrdobsmpp     !  - fail the grid search
      LOGICAL, DIMENSION(:), ALLOCATABLE :: & 
         & llvalid            ! SLA data selection
      INTEGER :: jobs         ! Obs. loop variable
      INTEGER :: jstp         ! Time loop variable
      INTEGER :: inrc         ! Time index variable

      IF(lwp) WRITE(numout,*)'obs_pre_sla : Preparing the SLA observations...'

      ! Initial date initialization (year, month, day, hour, minute)
      iyea0 =   ndate0 / 10000
      imon0 = ( ndate0 - iyea0 * 10000 ) / 100
      iday0 =   ndate0 - iyea0 * 10000 - imon0 * 100
      ihou0 = 0
      imin0 = 0

      icycle = no     ! Assimilation cycle

      ! Diagnotics counters for various failures.

      iotdobs  = 0
      igrdobs  = 0
      iosdsobs = 0
      ilansobs = 0
      inlasobs = 0

      ! -----------------------------------------------------------------------
      ! Find time coordinate for SLA data
      ! -----------------------------------------------------------------------

      CALL obs_coo_tim( icycle, &
         &              iyea0,   imon0,   iday0,   ihou0,   imin0,      &
         &              sladata%nsurf,   sladata%nyea, sladata%nmon, &
         &              sladata%nday,    sladata%nhou, sladata%nmin, &
         &              sladata%nqc,     sladata%mstp, iotdobs        )

      CALL obs_mpp_sum_integer( iotdobs, iotdobsmpp )
      
      ! -----------------------------------------------------------------------
      ! Check for SLA data failing the grid search
      ! -----------------------------------------------------------------------

      CALL obs_coo_grd( sladata%nsurf,   sladata%mi, sladata%mj, &
         &              sladata%nqc,     igrdobs                         )

      CALL obs_mpp_sum_integer( igrdobs, igrdobsmpp )

      ! -----------------------------------------------------------------------
      ! Check for land points. 
      ! -----------------------------------------------------------------------

      CALL obs_coo_spc_2d( sladata%nsurf,              &
         &                 jpi,          jpj,          &
         &                 sladata%mi,   sladata%mj,   & 
         &                 sladata%rlam, sladata%rphi, &
         &                 glamt,        gphit,        &
         &                 tmask(:,:,1), sladata%nqc,  &
         &                 iosdsobs,     ilansobs,     &
         &                 inlasobs,     ld_nea        )

      CALL obs_mpp_sum_integer( iosdsobs, iosdsobsmpp )
      CALL obs_mpp_sum_integer( ilansobs, ilansobsmpp )
      CALL obs_mpp_sum_integer( inlasobs, inlasobsmpp )

      ! -----------------------------------------------------------------------
      ! Copy useful data from the sladata data structure to
      ! the sladatqc data structure 
      ! -----------------------------------------------------------------------

      ! Allocate the selection arrays

      ALLOCATE( llvalid(sladata%nsurf) )
      
      ! We want all data which has qc flags <= 10

      llvalid(:)  = ( sladata%nqc(:)  <= 10 )

      ! The actual copying

      CALL obs_surf_compress( sladata,     sladatqc,       .TRUE.,  numout, &
         &                    lvalid=llvalid )

      ! Dellocate the selection arrays
      DEALLOCATE( llvalid )

      ! -----------------------------------------------------------------------
      ! Print information about what observations are left after qc
      ! -----------------------------------------------------------------------

      ! Update the total observation counter array
      
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'obs_pre_sla :'
         WRITE(numout,*) '~~~~~~~~~~~'
         WRITE(numout,*)
         WRITE(numout,*) ' SLA data outside time domain                  = ', &
            &            iotdobsmpp
         WRITE(numout,*) ' Remaining SLA data that failed grid search    = ', &
            &            igrdobsmpp
         WRITE(numout,*) ' Remaining SLA data outside space domain       = ', &
            &            iosdsobsmpp
         WRITE(numout,*) ' Remaining SLA data at land points             = ', &
            &            ilansobsmpp
         IF (ld_nea) THEN
            WRITE(numout,*) ' Remaining SLA data near land points (removed) = ', &
               &            inlasobsmpp
         ELSE
            WRITE(numout,*) ' Remaining SLA data near land points (kept)    = ', &
               &            inlasobsmpp
         ENDIF
         WRITE(numout,*) ' SLA data accepted                             = ', &
            &            sladatqc%nsurfmpp

         WRITE(numout,*)
         WRITE(numout,*) ' Number of observations per time step :'
         WRITE(numout,*)
         WRITE(numout,1997)
         WRITE(numout,1998)
      ENDIF
      
      DO jobs = 1, sladatqc%nsurf
         inrc = sladatqc%mstp(jobs) + 2 - nit000
         sladatqc%nsstp(inrc)  = sladatqc%nsstp(inrc) + 1
      END DO
      
      CALL obs_mpp_sum_integers( sladatqc%nsstp, sladatqc%nsstpmpp, &
         &                       nitend - nit000 + 2 )

      IF ( lwp ) THEN
         DO jstp = nit000 - 1, nitend
            inrc = jstp - nit000 + 2
            WRITE(numout,1999) jstp, sladatqc%nsstpmpp(inrc)
         END DO
      ENDIF

1997  FORMAT(10X,'Time step',5X,'Sea level anomaly')
1998  FORMAT(10X,'---------',5X,'-----------------')
1999  FORMAT(10X,I9,5X,I17)

   END SUBROUTINE obs_pre_sla

   SUBROUTINE obs_pre_sst( sstdata, sstdatqc, ld_sst, ld_nea )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE obs_pre_sst  ***
      !!
      !! ** Purpose : First level check and screening of SST observations
      !!
      !! ** Method  : First level check and screening of SST observations
      !!
      !! ** Action  : 
      !!
      !! References :
      !!   
      !! History :
      !!        !  2007-03  (S. Ricci) SST data preparation 
      !!----------------------------------------------------------------------
      !! * Modules used
      USE domstp              ! Domain: set the time-step
      USE par_oce             ! Ocean parameters
      USE dom_oce, ONLY : &   ! Geographical information
         & glamt,   &
         & gphit,   &
         & tmask,   &
         & nproc
      !! * Arguments
      TYPE(obs_surf), INTENT(INOUT) :: sstdata     ! Full set of SST data
      TYPE(obs_surf), INTENT(INOUT) :: sstdatqc    ! Subset of SST data not failing screening
      LOGICAL :: ld_sst             ! Switch for SST data
      LOGICAL :: ld_nea             ! Switch for rejecting observation near land
      !! * Local declarations
      INTEGER :: iyea0        ! Initial date
      INTEGER :: imon0        !  - (year, month, day, hour, minute)
      INTEGER :: iday0   
      INTEGER :: ihou0    
      INTEGER :: imin0
      INTEGER :: icycle       ! Current assimilation cycle
                              ! Counters for observations that
      INTEGER :: iotdobs      !  - outside time domain
      INTEGER :: iosdsobs     !  - outside space domain
      INTEGER :: ilansobs     !  - within a model land cell
      INTEGER :: inlasobs     !  - close to land
      INTEGER :: igrdobs      !  - fail the grid search
                              ! Global counters for observations that
      INTEGER :: iotdobsmpp   !  - outside time domain
      INTEGER :: iosdsobsmpp  !  - outside space domain
      INTEGER :: ilansobsmpp  !  - within a model land cell
      INTEGER :: inlasobsmpp  !  - close to land
      INTEGER :: igrdobsmpp   !  - fail the grid search
      LOGICAL, DIMENSION(:), ALLOCATABLE :: & 
         & llvalid            ! SST data selection
      INTEGER :: jobs         ! Obs. loop variable
      INTEGER :: jstp         ! Time loop variable
      INTEGER :: inrc         ! Time index variable

      IF(lwp) WRITE(numout,*)'obs_pre_sst : Preparing the SST observations...'

      ! Initial date initialization (year, month, day, hour, minute)
      iyea0 =   ndate0 / 10000
      imon0 = ( ndate0 - iyea0 * 10000 ) / 100
      iday0 =   ndate0 - iyea0 * 10000 - imon0 * 100
      ihou0 = 0
      imin0 = 0

      icycle = no     ! Assimilation cycle

      ! Diagnotics counters for various failures.

      iotdobs  = 0
      igrdobs  = 0
      iosdsobs = 0
      ilansobs = 0
      inlasobs = 0

      ! -----------------------------------------------------------------------
      ! Find time coordinate for SST data
      ! -----------------------------------------------------------------------

      CALL obs_coo_tim( icycle, &
         &              iyea0,   imon0,   iday0,   ihou0,   imin0,      &
         &              sstdata%nsurf,   sstdata%nyea, sstdata%nmon, &
         &              sstdata%nday,    sstdata%nhou, sstdata%nmin, &
         &              sstdata%nqc,     sstdata%mstp, iotdobs        )
      CALL obs_mpp_sum_integer( iotdobs, iotdobsmpp )
      ! -----------------------------------------------------------------------
      ! Check for SST data failing the grid search
      ! -----------------------------------------------------------------------

      CALL obs_coo_grd( sstdata%nsurf,   sstdata%mi, sstdata%mj, &
         &              sstdata%nqc,     igrdobs                         )
      CALL obs_mpp_sum_integer( igrdobs, igrdobsmpp )

      ! -----------------------------------------------------------------------
      ! Check for land points. 
      ! -----------------------------------------------------------------------

      CALL obs_coo_spc_2d( sstdata%nsurf,              &
         &                 jpi,          jpj,          &
         &                 sstdata%mi,   sstdata%mj,   & 
         &                 sstdata%rlam, sstdata%rphi, &
         &                 glamt,        gphit,        &
         &                 tmask(:,:,1), sstdata%nqc,  &
         &                 iosdsobs,     ilansobs,     &
         &                 inlasobs,     ld_nea        )

      CALL obs_mpp_sum_integer( iosdsobs, iosdsobsmpp )
      CALL obs_mpp_sum_integer( ilansobs, ilansobsmpp )
      CALL obs_mpp_sum_integer( inlasobs, inlasobsmpp )

      ! -----------------------------------------------------------------------
      ! Copy useful data from the sstdata data structure to
      ! the sstdatqc data structure 
      ! -----------------------------------------------------------------------

      ! Allocate the selection arrays

      ALLOCATE( llvalid(sstdata%nsurf) )
      
      ! We want all data which has qc flags <= 0

      llvalid(:)  = ( sstdata%nqc(:)  <= 10 )

      ! The actual copying

      CALL obs_surf_compress( sstdata,     sstdatqc,       .TRUE.,  numout, &
         &                    lvalid=llvalid )

      ! Dellocate the selection arrays
      DEALLOCATE( llvalid )

      ! -----------------------------------------------------------------------
      ! Print information about what observations are left after qc
      ! -----------------------------------------------------------------------

      ! Update the total observation counter array
      
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'obs_pre_sst :'
         WRITE(numout,*) '~~~~~~~~~~~'
         WRITE(numout,*)
         WRITE(numout,*) ' SST data outside time domain                  = ', &
            &            iotdobsmpp
         WRITE(numout,*) ' Remaining SST data that failed grid search    = ', &
            &            igrdobsmpp
         WRITE(numout,*) ' Remaining SST data outside space domain       = ', &
            &            iosdsobsmpp
         WRITE(numout,*) ' Remaining SST data at land points             = ', &
            &            ilansobsmpp
         IF (ld_nea) THEN
            WRITE(numout,*) ' Remaining SST data near land points (removed) = ', &
               &            inlasobsmpp
         ELSE
            WRITE(numout,*) ' Remaining SST data near land points (kept)    = ', &
               &            inlasobsmpp
         ENDIF
         WRITE(numout,*) ' SST data accepted                             = ', &
            &            sstdatqc%nsurfmpp

         WRITE(numout,*)
         WRITE(numout,*) ' Number of observations per time step :'
         WRITE(numout,*)
         WRITE(numout,1997)
         WRITE(numout,1998)
      ENDIF
      
      DO jobs = 1, sstdatqc%nsurf
         inrc = sstdatqc%mstp(jobs) + 2 - nit000
         sstdatqc%nsstp(inrc)  = sstdatqc%nsstp(inrc) + 1
      END DO
      
      CALL obs_mpp_sum_integers( sstdatqc%nsstp, sstdatqc%nsstpmpp, &
         &                       nitend - nit000 + 2 )

      IF ( lwp ) THEN
         DO jstp = nit000 - 1, nitend
            inrc = jstp - nit000 + 2
            WRITE(numout,1999) jstp, sstdatqc%nsstpmpp(inrc)
         END DO
      ENDIF

1997  FORMAT(10X,'Time step',5X,'Sea surface temperature')
1998  FORMAT(10X,'---------',5X,'-----------------')
1999  FORMAT(10X,I9,5X,I17)
      
   END SUBROUTINE obs_pre_sst

   SUBROUTINE obs_pre_seaice( seaicedata, seaicedatqc, ld_seaice, ld_nea )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE obs_pre_seaice  ***
      !!
      !! ** Purpose : First level check and screening of Sea Ice observations
      !!
      !! ** Method  : First level check and screening of Sea Ice observations
      !!
      !! ** Action  : 
      !!
      !! References :
      !!   
      !! History :
      !!        !  2007-11 (D. Lea) based on obs_pre_sst
      !!----------------------------------------------------------------------
      !! * Modules used
      USE domstp              ! Domain: set the time-step
      USE par_oce             ! Ocean parameters
      USE dom_oce, ONLY : &   ! Geographical information
         & glamt,   &
         & gphit,   &
         & tmask,   &
         & nproc
      !! * Arguments
      TYPE(obs_surf), INTENT(INOUT) :: seaicedata     ! Full set of Sea Ice data
      TYPE(obs_surf), INTENT(INOUT) :: seaicedatqc    ! Subset of sea ice data not failing screening
      LOGICAL :: ld_seaice     ! Switch for sea ice data
      LOGICAL :: ld_nea        ! Switch for rejecting observation near land
      !! * Local declarations
      INTEGER :: iyea0         ! Initial date
      INTEGER :: imon0         !  - (year, month, day, hour, minute)
      INTEGER :: iday0    
      INTEGER :: ihou0    
      INTEGER :: imin0
      INTEGER :: icycle       ! Current assimilation cycle
                              ! Counters for observations that
      INTEGER :: iotdobs      !  - outside time domain
      INTEGER :: iosdsobs     !  - outside space domain
      INTEGER :: ilansobs     !  - within a model land cell
      INTEGER :: inlasobs     !  - close to land
      INTEGER :: igrdobs      !  - fail the grid search
                              ! Global counters for observations that
      INTEGER :: iotdobsmpp   !  - outside time domain
      INTEGER :: iosdsobsmpp  !  - outside space domain
      INTEGER :: ilansobsmpp  !  - within a model land cell
      INTEGER :: inlasobsmpp  !  - close to land
      INTEGER :: igrdobsmpp   !  - fail the grid search
      LOGICAL, DIMENSION(:), ALLOCATABLE :: & 
         & llvalid            ! data selection
      INTEGER :: jobs         ! Obs. loop variable
      INTEGER :: jstp         ! Time loop variable
      INTEGER :: inrc         ! Time index variable

      IF (lwp) WRITE(numout,*)'obs_pre_seaice : Preparing the sea ice observations...'

      ! Initial date initialization (year, month, day, hour, minute)
      iyea0 =   ndate0 / 10000
      imon0 = ( ndate0 - iyea0 * 10000 ) / 100
      iday0 =   ndate0 - iyea0 * 10000 - imon0 * 100
      ihou0 = 0
      imin0 = 0

      icycle = no     ! Assimilation cycle

      ! Diagnotics counters for various failures.

      iotdobs  = 0
      igrdobs  = 0
      iosdsobs = 0
      ilansobs = 0
      inlasobs = 0

      ! -----------------------------------------------------------------------
      ! Find time coordinate for sea ice data
      ! -----------------------------------------------------------------------

      CALL obs_coo_tim( icycle, &
         &              iyea0,   imon0,   iday0,   ihou0,   imin0,      &
         &              seaicedata%nsurf,   seaicedata%nyea, seaicedata%nmon, &
         &              seaicedata%nday,    seaicedata%nhou, seaicedata%nmin, &
         &              seaicedata%nqc,     seaicedata%mstp, iotdobs        )
      CALL obs_mpp_sum_integer( iotdobs, iotdobsmpp )
      ! -----------------------------------------------------------------------
      ! Check for sea ice data failing the grid search
      ! -----------------------------------------------------------------------

      CALL obs_coo_grd( seaicedata%nsurf,   seaicedata%mi, seaicedata%mj, &
         &              seaicedata%nqc,     igrdobs                         )
      CALL obs_mpp_sum_integer( igrdobs, igrdobsmpp )

      ! -----------------------------------------------------------------------
      ! Check for land points. 
      ! -----------------------------------------------------------------------

      CALL obs_coo_spc_2d( seaicedata%nsurf,                 &
         &                 jpi,             jpj,             &
         &                 seaicedata%mi,   seaicedata%mj,   & 
         &                 seaicedata%rlam, seaicedata%rphi, &
         &                 glamt,           gphit,           &
         &                 tmask(:,:,1),    seaicedata%nqc,  &
         &                 iosdsobs,        ilansobs,        &
         &                 inlasobs,        ld_nea           )

      CALL obs_mpp_sum_integer( iosdsobs, iosdsobsmpp )
      CALL obs_mpp_sum_integer( ilansobs, ilansobsmpp )
      CALL obs_mpp_sum_integer( inlasobs, inlasobsmpp )

      ! -----------------------------------------------------------------------
      ! Copy useful data from the seaicedata data structure to
      ! the seaicedatqc data structure 
      ! -----------------------------------------------------------------------

      ! Allocate the selection arrays

      ALLOCATE( llvalid(seaicedata%nsurf) )
      
      ! We want all data which has qc flags <= 0

      llvalid(:)  = ( seaicedata%nqc(:)  <= 10 )

      ! The actual copying

      CALL obs_surf_compress( seaicedata,     seaicedatqc,       .TRUE.,  numout, &
         &                    lvalid=llvalid )

      ! Dellocate the selection arrays
      DEALLOCATE( llvalid )

      ! -----------------------------------------------------------------------
      ! Print information about what observations are left after qc
      ! -----------------------------------------------------------------------

      ! Update the total observation counter array
      
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'obs_pre_seaice :'
         WRITE(numout,*) '~~~~~~~~~~~'
         WRITE(numout,*)
         WRITE(numout,*) ' Sea ice data outside time domain                  = ', &
            &            iotdobsmpp
         WRITE(numout,*) ' Remaining sea ice data that failed grid search    = ', &
            &            igrdobsmpp
         WRITE(numout,*) ' Remaining sea ice data outside space domain       = ', &
            &            iosdsobsmpp
         WRITE(numout,*) ' Remaining sea ice data at land points             = ', &
            &            ilansobsmpp
         IF (ld_nea) THEN
            WRITE(numout,*) ' Remaining sea ice data near land points (removed) = ', &
               &            inlasobsmpp
         ELSE
            WRITE(numout,*) ' Remaining sea ice data near land points (kept)    = ', &
               &            inlasobsmpp
         ENDIF
         WRITE(numout,*) ' Sea ice data accepted                             = ', &
            &            seaicedatqc%nsurfmpp

         WRITE(numout,*)
         WRITE(numout,*) ' Number of observations per time step :'
         WRITE(numout,*)
         WRITE(numout,1997)
         WRITE(numout,1998)
      ENDIF
      
      DO jobs = 1, seaicedatqc%nsurf
         inrc = seaicedatqc%mstp(jobs) + 2 - nit000
         seaicedatqc%nsstp(inrc)  = seaicedatqc%nsstp(inrc) + 1
      END DO
      
      CALL obs_mpp_sum_integers( seaicedatqc%nsstp, seaicedatqc%nsstpmpp, &
         &                       nitend - nit000 + 2 )

      IF ( lwp ) THEN
         DO jstp = nit000 - 1, nitend
            inrc = jstp - nit000 + 2
            WRITE(numout,1999) jstp, seaicedatqc%nsstpmpp(inrc)
         END DO
      ENDIF

1997  FORMAT(10X,'Time step',5X,'Sea ice data           ')
1998  FORMAT(10X,'---------',5X,'-----------------')
1999  FORMAT(10X,I9,5X,I17)
      
   END SUBROUTINE obs_pre_seaice

   SUBROUTINE obs_pre_vel( profdata, prodatqc, ld_vel3d, ld_nea, ld_dailyav )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE obs_pre_taovel  ***
      !!
      !! ** Purpose : First level check and screening of U and V profiles
      !!
      !! ** Method  : First level check and screening of U and V profiles
      !!
      !! History :
      !!        !  2007-06  (K. Mogensen) original : T and S profile data
      !!        !  2008-09  (M. Valdivieso) : TAO velocity data
      !!        !  2009-01  (K. Mogensen) : New feedback strictuer
      !!
      !!----------------------------------------------------------------------
      !! * Modules used
      USE domstp              ! Domain: set the time-step
      USE par_oce             ! Ocean parameters
      USE dom_oce, ONLY : &   ! Geographical information
         & glamt, glamu, glamv,    &
         & gphit, gphiu, gphiv,    &
         & gdept_0, &
         & tmask, umask, vmask,  &
         & nproc
      !! * Arguments
      TYPE(obs_prof), INTENT(INOUT) :: profdata   ! Full set of profile data
      TYPE(obs_prof), INTENT(INOUT) :: prodatqc   ! Subset of profile data not failing screening
      LOGICAL, INTENT(IN) :: ld_vel3d      ! Switch for zonal and meridional velocity components
      LOGICAL, INTENT(IN) :: ld_nea        ! Switch for rejecting observation near land
      LOGICAL, INTENT(IN) :: ld_dailyav    ! Switch for daily average data
      !! * Local declarations
      INTEGER :: iyea0        ! Initial date
      INTEGER :: imon0        !  - (year, month, day, hour, minute)
      INTEGER :: iday0    
      INTEGER :: ihou0    
      INTEGER :: imin0
      INTEGER :: icycle       ! Current assimilation cycle
                              ! Counters for observations that
      INTEGER :: iotdobs      !  - outside time domain
      INTEGER :: iosduobs     !  - outside space domain (zonal velocity component)
      INTEGER :: iosdvobs     !  - outside space domain (meridional velocity component)
      INTEGER :: ilanuobs     !  - within a model land cell (zonal velocity component)
      INTEGER :: ilanvobs     !  - within a model land cell (meridional velocity component)
      INTEGER :: inlauobs     !  - close to land (zonal velocity component)
      INTEGER :: inlavobs     !  - close to land (meridional velocity component)
      INTEGER :: igrdobs      !  - fail the grid search
      INTEGER :: iuvchku      !  - reject u if v rejected and vice versa
      INTEGER :: iuvchkv      !
                              ! Global counters for observations that
      INTEGER :: iotdobsmpp   !  - outside time domain
      INTEGER :: iosduobsmpp  !  - outside space domain (zonal velocity component)
      INTEGER :: iosdvobsmpp  !  - outside space domain (meridional velocity component)
      INTEGER :: ilanuobsmpp  !  - within a model land cell (zonal velocity component)
      INTEGER :: ilanvobsmpp  !  - within a model land cell (meridional velocity component)
      INTEGER :: inlauobsmpp  !  - close to land (zonal velocity component)
      INTEGER :: inlavobsmpp  !  - close to land (meridional velocity component)
      INTEGER :: igrdobsmpp   !  - fail the grid search
      INTEGER :: iuvchkumpp   !  - reject u if v rejected and vice versa
      INTEGER :: iuvchkvmpp   !
      TYPE(obs_prof_valid) ::  llvalid      ! Profile selection 
      TYPE(obs_prof_valid), DIMENSION(profdata%nvar) :: &
         & llvvalid           ! U,V selection 
      INTEGER :: jvar         ! Variable loop variable
      INTEGER :: jobs         ! Obs. loop variable
      INTEGER :: jstp         ! Time loop variable
      INTEGER :: inrc         ! Time index variable

      IF(lwp) WRITE(numout,*)'obs_pre_vel: Preparing the velocity profile data'

      ! Initial date initialization (year, month, day, hour, minute)
      iyea0 =   ndate0 / 10000
      imon0 = ( ndate0 - iyea0 * 10000 ) / 100
      iday0 =   ndate0 - iyea0 * 10000 - imon0 * 100
      ihou0 = 0
      imin0 = 0

      icycle = no     ! Assimilation cycle

      ! Diagnotics counters for various failures.

      iotdobs  = 0
      igrdobs  = 0
      iosduobs = 0
      iosdvobs = 0
      ilanuobs = 0
      ilanvobs = 0
      inlauobs = 0
      inlavobs = 0
      iuvchku  = 0
      iuvchkv = 0

      ! -----------------------------------------------------------------------
      ! Find time coordinate for profiles
      ! -----------------------------------------------------------------------

      CALL obs_coo_tim_prof( icycle, &
         &              iyea0,   imon0,   iday0,   ihou0,   imin0,      &
         &              profdata%nprof,   profdata%nyea, profdata%nmon, &
         &              profdata%nday,    profdata%nhou, profdata%nmin, &
         &              profdata%ntyp,    profdata%nqc,  profdata%mstp, &
         &              iotdobs, ld_dailyav = ld_dailyav        )
	 
      CALL obs_mpp_sum_integer( iotdobs, iotdobsmpp )
      
      ! -----------------------------------------------------------------------
      ! Check for profiles failing the grid search
      ! -----------------------------------------------------------------------

      CALL obs_coo_grd( profdata%nprof,   profdata%mi(:,1), profdata%mj(:,1), &
         &              profdata%nqc,     igrdobs                         )
      CALL obs_coo_grd( profdata%nprof,   profdata%mi(:,2), profdata%mj(:,2), &
         &              profdata%nqc,     igrdobs                         )

      CALL obs_mpp_sum_integer( igrdobs, igrdobsmpp )

      ! -----------------------------------------------------------------------
      ! Reject all observations for profiles with nqc > 10
      ! -----------------------------------------------------------------------

      CALL obs_pro_rej( profdata )

      ! -----------------------------------------------------------------------
      ! Check for land points. This includes points below the model
      ! bathymetry so this is done for every point in the profile
      ! -----------------------------------------------------------------------

      ! Zonal Velocity Component

      CALL obs_coo_spc_3d( profdata%nprof,        profdata%nvprot(1),   &
         &                 profdata%npvsta(:,1),  profdata%npvend(:,1), &
         &                 jpi,                   jpj,                  &
         &                 jpk,                                         &
         &                 profdata%mi,           profdata%mj,          & 
         &                 profdata%var(1)%mvk,                         &
         &                 profdata%rlam,         profdata%rphi,        &
         &                 profdata%var(1)%vdep,                        &
         &                 glamu,                 gphiu,                &
         &                 gdept_0,               umask,                &
         &                 profdata%nqc,          profdata%var(1)%nvqc, &
         &                 iosduobs,              ilanuobs,             &
         &                 inlauobs,              ld_nea                )

      CALL obs_mpp_sum_integer( iosduobs, iosduobsmpp )
      CALL obs_mpp_sum_integer( ilanuobs, ilanuobsmpp )
      CALL obs_mpp_sum_integer( inlauobs, inlauobsmpp )

      ! Meridional Velocity Component

      CALL obs_coo_spc_3d( profdata%nprof,        profdata%nvprot(2),   &
         &                 profdata%npvsta(:,2),  profdata%npvend(:,2), &
         &                 jpi,                   jpj,                  &
         &                 jpk,                                         &
         &                 profdata%mi,           profdata%mj,          & 
         &                 profdata%var(2)%mvk,                         &
         &                 profdata%rlam,         profdata%rphi,        &
         &                 profdata%var(2)%vdep,                        &
         &                 glamv,                 gphiv,                &
         &                 gdept_0,               vmask,                &
         &                 profdata%nqc,          profdata%var(2)%nvqc, &
         &                 iosdvobs,              ilanvobs,             &
         &                 inlavobs,              ld_nea                )

      CALL obs_mpp_sum_integer( iosdvobs, iosdvobsmpp )
      CALL obs_mpp_sum_integer( ilanvobs, ilanvobsmpp )
      CALL obs_mpp_sum_integer( inlavobs, inlavobsmpp )

      ! -----------------------------------------------------------------------
      ! Reject u if v is rejected and vice versa
      ! -----------------------------------------------------------------------

      CALL obs_uv_rej( profdata, iuvchku, iuvchkv )
      CALL obs_mpp_sum_integer( iuvchku, iuvchkumpp )
      CALL obs_mpp_sum_integer( iuvchkv, iuvchkvmpp )

      ! -----------------------------------------------------------------------
      ! Copy useful data from the profdata data structure to
      ! the prodatqc data structure 
      ! -----------------------------------------------------------------------

      ! Allocate the selection arrays

      ALLOCATE( llvalid%luse(profdata%nprof) )
      DO jvar = 1,profdata%nvar
         ALLOCATE( llvvalid(jvar)%luse(profdata%nvprot(jvar)) )
      END DO

      ! We want all data which has qc flags = 0

      llvalid%luse(:) = ( profdata%nqc(:)  <= 10 )
      DO jvar = 1,profdata%nvar
         llvvalid(jvar)%luse(:) = ( profdata%var(jvar)%nvqc(:) <= 10 )
      END DO

      ! The actual copying

      CALL obs_prof_compress( profdata,     prodatqc,       .TRUE.,  numout, &
         &                    lvalid=llvalid, lvvalid=llvvalid )

      ! Dellocate the selection arrays
      DEALLOCATE( llvalid%luse )
      DO jvar = 1,profdata%nvar
         DEALLOCATE( llvvalid(jvar)%luse )
      END DO

      ! -----------------------------------------------------------------------
      ! Print information about what observations are left after qc
      ! -----------------------------------------------------------------------

      ! Update the total observation counter array
      
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'obs_pre_vel :'
         WRITE(numout,*) '~~~~~~~~~~~'
         WRITE(numout,*)
         WRITE(numout,*) ' Profiles outside time domain                = ', &
            &            iotdobsmpp
         WRITE(numout,*) ' Remaining profiles that failed grid search  = ', &
            &            igrdobsmpp
         WRITE(numout,*) ' Remaining U data outside space domain       = ', &
            &            iosduobsmpp
         WRITE(numout,*) ' Remaining U data at land points             = ', &
            &            ilanuobsmpp
         IF (ld_nea) THEN
            WRITE(numout,*) ' Remaining U data near land points (removed) = ',&
               &            inlauobsmpp
         ELSE
            WRITE(numout,*) ' Remaining U data near land points (kept)    = ',&
               &            inlauobsmpp
         ENDIF
         WRITE(numout,*) ' U observation rejected since V rejected     = ', &
            &            iuvchku     
         WRITE(numout,*) ' U data accepted                             = ', &
            &            prodatqc%nvprotmpp(1)
         WRITE(numout,*) ' Remaining V data outside space domain       = ', &
            &            iosdvobsmpp
         WRITE(numout,*) ' Remaining V data at land points             = ', &
            &            ilanvobsmpp
         IF (ld_nea) THEN
            WRITE(numout,*) ' Remaining V data near land points (removed) = ',&
               &            inlavobsmpp
         ELSE
            WRITE(numout,*) ' Remaining V data near land points (kept)    = ',&
               &            inlavobsmpp
         ENDIF
         WRITE(numout,*) ' V observation rejected since U rejected     = ', &
            &            iuvchkv     
         WRITE(numout,*) ' V data accepted                             = ', &
            &            prodatqc%nvprotmpp(2)

         WRITE(numout,*)
         WRITE(numout,*) ' Number of observations per time step :'
         WRITE(numout,*)
         WRITE(numout,997)
         WRITE(numout,998)
      ENDIF
      
      DO jobs = 1, prodatqc%nprof
         inrc = prodatqc%mstp(jobs) + 2 - nit000
         prodatqc%npstp(inrc)  = prodatqc%npstp(inrc) + 1
         DO jvar = 1, prodatqc%nvar
            IF ( prodatqc%npvend(jobs,jvar) > 0 ) THEN
               prodatqc%nvstp(inrc,jvar) = prodatqc%nvstp(inrc,jvar) + &
                  &                      ( prodatqc%npvend(jobs,jvar) - &
                  &                        prodatqc%npvsta(jobs,jvar) + 1 )
            ENDIF
         END DO
      END DO
      
      
      CALL obs_mpp_sum_integers( prodatqc%npstp, prodatqc%npstpmpp, &
         &                       nitend - nit000 + 2 )
      DO jvar = 1, prodatqc%nvar
         CALL obs_mpp_sum_integers( prodatqc%nvstp(:,jvar), &
            &                       prodatqc%nvstpmpp(:,jvar), &
            &                       nitend - nit000 + 2 )
      END DO

      IF ( lwp ) THEN
         DO jstp = nit000 - 1, nitend
            inrc = jstp - nit000 + 2
            WRITE(numout,999) jstp, prodatqc%npstpmpp(inrc), &
               &                    prodatqc%nvstpmpp(inrc,1), &
               &                    prodatqc%nvstpmpp(inrc,2)
         END DO
      ENDIF

997   FORMAT(10X,'Time step',5X,'Profiles',5X,'Zonal Comp.',5X,'Meridional Comp.')
998   FORMAT(10X,'---------',5X,'--------',5X,'-----------',5X,'----------------')
999   FORMAT(10X,I9,5X,I8,5X,I11,5X,I8)

   END SUBROUTINE obs_pre_vel

   SUBROUTINE obs_coo_tim( kcycle, &
      &                    kyea0,   kmon0,   kday0,   khou0,   kmin0,     &
      &                    kobsno,                                        &
      &                    kobsyea, kobsmon, kobsday, kobshou, kobsmin,   &
      &                    kobsqc,  kobsstp, kotdobs                      )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE obs_coo_tim ***
      !!
      !! ** Purpose : Compute the number of time steps to the observation time.
      !!
      !! ** Method  : For time coordinates ( yea_obs, mon_obs, day_obs, 
      !!              hou_obs, min_obs ), this routine locates the time step 
      !!              that is closest to this time.
      !!
      !! ** Action  : 
      !!
      !! References :
      !!   
      !! History :
      !!        !  1997-07  (A. Weaver) Original
      !!        !  2006-08  (A. Weaver) NEMOVAR migration
      !!        !  2006-10  (A. Weaver) Cleanup
      !!        !  2007-01  (K. Mogensen) Rewritten with loop
      !!        !  2010-05  (D. Lea) Fix in leap year calculation for NEMO vn3.2
      !!----------------------------------------------------------------------
      !! * Modules used
      USE dom_oce, ONLY : &  ! Geographical information
         & rdt
      USE phycst, ONLY : &   ! Physical constants
         & rday,  &             
         & rmmss, &             
         & rhhmm                        
      !! * Arguments
      INTEGER, INTENT(IN) :: kcycle     ! Current cycle
      INTEGER, INTENT(IN) :: kyea0      ! Initial date coordinates
      INTEGER, INTENT(IN) :: kmon0
      INTEGER, INTENT(IN) :: kday0 
      INTEGER, INTENT(IN) :: khou0
      INTEGER, INTENT(IN) :: kmin0
      INTEGER, INTENT(IN) :: kobsno     ! Number of observations
      INTEGER, INTENT(INOUT) :: kotdobs   ! Number of observations failing time check
      INTEGER, DIMENSION(kobsno), INTENT(IN ) :: &
         & kobsyea,  &      ! Observation time coordinates
         & kobsmon,  &
         & kobsday,  & 
         & kobshou,  &
         & kobsmin
      INTEGER, DIMENSION(kobsno), INTENT(INOUT) :: &
         & kobsqc           ! Quality control flag
      INTEGER, DIMENSION(kobsno), INTENT(OUT) :: &
         & kobsstp          ! Number of time steps up to the 
                            ! observation time

      !! * Local declarations
      INTEGER :: jyea
      INTEGER :: jmon
      INTEGER :: jday
      INTEGER :: jobs
      INTEGER :: iyeastr
      INTEGER :: iyeaend
      INTEGER :: imonstr
      INTEGER :: imonend
      INTEGER :: idaystr
      INTEGER :: idayend 
      INTEGER :: iskip
      INTEGER :: idaystp
      REAL(KIND=wp) :: zminstp
      REAL(KIND=wp) :: zhoustp
      REAL(KIND=wp) :: zobsstp 
      INTEGER, DIMENSION(12) ::   imonth_len    !: length in days of the months of the current year
 
      !-----------------------------------------------------------------------
      ! Initialization
      !-----------------------------------------------------------------------

      ! Intialize the number of time steps per day
      idaystp = NINT( rday / rdt )

      !---------------------------------------------------------------------
      ! Locate the model time coordinates for interpolation
      !---------------------------------------------------------------------

      DO jobs = 1, kobsno

         ! Initialize the time step counter
         kobsstp(jobs) = nit000 - 1

         ! Flag if observation date is less than the initial date

         IF ( ( kobsyea(jobs) < kyea0 )                   &
            & .OR. ( ( kobsyea(jobs) == kyea0 )           &
            &        .AND. ( kobsmon(jobs) <  kmon0 ) )   &
            & .OR. ( ( kobsyea(jobs) == kyea0 )           &
            &        .AND. ( kobsmon(jobs) == kmon0 )     &
            &        .AND. ( kobsday(jobs) <  kday0 ) )   &
            & .OR. ( ( kobsyea(jobs) == kyea0 )           &
            &        .AND. ( kobsmon(jobs) == kmon0 )     &
            &        .AND. ( kobsday(jobs) == kday0 )     &
            &        .AND. ( kobshou(jobs) <  khou0 ) )   &
            &  .OR. ( ( kobsyea(jobs) == kyea0 )          &
            &        .AND. ( kobsmon(jobs) == kmon0 )     &
            &        .AND. ( kobsday(jobs) == kday0 )          &
            &        .AND. ( kobshou(jobs) == khou0 )          &
            &        .AND. ( kobsmin(jobs) <= kmin0 ) ) ) THEN
            kobsstp(jobs) = -1
            kobsqc(jobs)  = kobsqc(jobs) + 11
            kotdobs       = kotdobs + 1
            CYCLE
         ENDIF

         ! Compute the number of time steps to the observation day
         iyeastr = kyea0
         iyeaend = kobsyea(jobs)
         
         !---------------------------------------------------------------------
         ! Year loop
         !---------------------------------------------------------------------
         DO jyea = iyeastr, iyeaend

            CALL calc_month_len( jyea, imonth_len )
            
            imonstr = 1
            IF ( jyea == kyea0         ) imonstr = kmon0
            imonend = 12
            IF ( jyea == kobsyea(jobs) ) imonend = kobsmon(jobs)
            
            ! Month loop
            DO jmon = imonstr, imonend
               
               idaystr = 1
               IF (       ( jmon == kmon0   ) &
                  & .AND. ( jyea == kyea0   ) ) idaystr = kday0
               idayend = imonth_len(jmon)
               IF (       ( jmon == kobsmon(jobs) ) &
                  & .AND. ( jyea == kobsyea(jobs) ) ) idayend = kobsday(jobs) - 1
               
               ! Day loop
               DO jday = idaystr, idayend
                  kobsstp(jobs) = kobsstp(jobs) + idaystp
               END DO
               
            END DO

         END DO

         ! Add in the number of time steps to the observation minute
         zminstp = rmmss / rdt
         zhoustp = rhhmm * zminstp

         zobsstp =   REAL( kobsmin(jobs) - kmin0, KIND=wp ) * zminstp &
            &      + REAL( kobshou(jobs) - khou0, KIND=wp ) * zhoustp
         kobsstp(jobs) = kobsstp(jobs) + NINT( zobsstp )

         ! Flag if observation step outside the time window
         IF ( ( kobsstp(jobs) < ( nit000 - 1 ) ) &
            & .OR.( kobsstp(jobs) > nitend ) ) THEN
            kobsqc(jobs) = kobsqc(jobs) + 12
            kotdobs = kotdobs + 1
            CYCLE
         ENDIF

      END DO

   END SUBROUTINE obs_coo_tim

   SUBROUTINE calc_month_len( iyear, imonth_len )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE calc_month_len  ***
      !!          
      !! ** Purpose : Compute the number of days in a months given a year.
      !!
      !! ** Method  : 
      !!
      !! ** Action  : 
      !!
      !! History :
      !!        !  10-05  (D. Lea)   New routine based on day_init 
      !!----------------------------------------------------------------------

      INTEGER, DIMENSION(12) ::   imonth_len    !: length in days of the months of the current year
      INTEGER :: iyear         !: year
      
      ! length of the month of the current year (from nleapy, read in namelist)
      IF ( nleapy < 2 ) THEN 
         imonth_len(:) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
         IF ( nleapy == 1 ) THEN   ! we are using calendar with leap years
            IF ( MOD(iyear, 4) == 0 .AND. ( MOD(iyear, 400) == 0 .OR. MOD(iyear, 100) /= 0 ) ) THEN
               imonth_len(2) = 29
            ENDIF
         ENDIF
      ELSE
         imonth_len(:) = nleapy   ! all months with nleapy days per year
      ENDIF

   END SUBROUTINE

   SUBROUTINE obs_coo_tim_prof( kcycle,                                   &
      &                    kyea0,   kmon0,   kday0,   khou0,   kmin0,     &
      &                    kobsno,                                        &
      &                    kobsyea, kobsmon, kobsday, kobshou, kobsmin,   &
      &                    ktyp,    kobsqc,  kobsstp, kotdobs, kdailyavtypes, &
      &                    ld_dailyav )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE obs_coo_tim ***
      !!
      !! ** Purpose : Compute the number of time steps to the observation time.
      !!
      !! ** Method  : For time coordinates ( yea_obs, mon_obs, day_obs, 
      !!              hou_obs, min_obs ), this routine locates the time step 
      !!              that is closest to this time.
      !!
      !! ** Action  : 
      !!
      !! References :
      !!   
      !! History :
      !!        !  1997-07  (A. Weaver) Original
      !!        !  2006-08  (A. Weaver) NEMOVAR migration
      !!        !  2006-10  (A. Weaver) Cleanup
      !!        !  2007-01  (K. Mogensen) Rewritten with loop
      !!----------------------------------------------------------------------
      !! * Modules used
      !! * Arguments
      INTEGER, INTENT(IN) :: kcycle      ! Current cycle
      INTEGER, INTENT(IN) :: kyea0       ! Initial date coordinates
      INTEGER, INTENT(IN) :: kmon0
      INTEGER, INTENT(IN) :: kday0
      INTEGER, INTENT(IN) :: khou0
      INTEGER, INTENT(IN) :: kmin0
      INTEGER, INTENT(IN) :: kobsno      ! Number of observations
      INTEGER, INTENT(INOUT) ::  kotdobs    ! Number of observations failing time check
      INTEGER, DIMENSION(kobsno), INTENT(IN ) :: &
         & kobsyea,  &      ! Observation time coordinates
         & kobsmon,  &
         & kobsday,  & 
         & kobshou,  &
         & kobsmin,  &
         & ktyp             ! Observation type.
      INTEGER, DIMENSION(kobsno), INTENT(INOUT) :: &
         & kobsqc           ! Quality control flag
      INTEGER, DIMENSION(kobsno), INTENT(OUT) :: &
         & kobsstp          ! Number of time steps up to the 
                            ! observation time
      INTEGER, DIMENSION(imaxavtypes), OPTIONAL :: &
         & kdailyavtypes    ! Types for daily averages
      LOGICAL, OPTIONAL :: ld_dailyav    ! All types are daily averages
      !! * Local declarations
      INTEGER :: jobs

      !-----------------------------------------------------------------------
      ! Call standard obs_coo_tim
      !-----------------------------------------------------------------------

      CALL obs_coo_tim( kcycle, &
         &              kyea0,   kmon0,   kday0,   khou0,   kmin0,     &
         &              kobsno,                                        &
         &              kobsyea, kobsmon, kobsday, kobshou, kobsmin,   &
         &              kobsqc,  kobsstp, kotdobs                      )

      !------------------------------------------------------------------------
      ! Always reject daily averaged data (e.g. MRB data (820)) at initial time
      !------------------------------------------------------------------------

      IF ( PRESENT(kdailyavtypes) ) THEN
         DO jobs = 1, kobsno
            
            IF ( kobsqc(jobs) <= 10 ) THEN
               
               IF ( ( kobsstp(jobs) == (nit000 - 1) ).AND.&
                  & ( ANY (kdailyavtypes(:) == ktyp(jobs)) ) ) THEN
                  kobsqc(jobs) = kobsqc(jobs) + 14
                  kotdobs      = kotdobs + 1
                  CYCLE
               ENDIF
               
            ENDIF
         END DO
      ENDIF

      !------------------------------------------------------------------------
      ! If ld_dailyav is set then all data assumed to be daily averaged
      !------------------------------------------------------------------------
      
      IF ( PRESENT( ld_dailyav) ) THEN
         IF (ld_dailyav) THEN
            DO jobs = 1, kobsno
               
               IF ( kobsqc(jobs) <= 10 ) THEN
                  
                  IF ( kobsstp(jobs) == (nit000 - 1) ) THEN
                     kobsqc(jobs) = kobsqc(jobs) + 14
                     kotdobs      = kotdobs + 1
                     CYCLE
                  ENDIF
                  
               ENDIF
            END DO
         ENDIF
      ENDIF

   END SUBROUTINE obs_coo_tim_prof

   SUBROUTINE obs_coo_grd( kobsno, kobsi, kobsj, kobsqc, kgrdobs )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE obs_coo_grd ***
      !!
      !! ** Purpose : Verify that the grid search has not failed
      !!
      !! ** Method  : The previously computed i,j indeces are checked  
      !!
      !! ** Action  : 
      !!
      !! References :
      !!   
      !! History :
      !!        !  2007-01  (K. Mogensen)  Original
      !!----------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT(IN) :: kobsno        ! Number of observations
      INTEGER, DIMENSION(kobsno), INTENT(IN ) :: &
         & kobsi, &         ! i,j indeces previously computed
         & kobsj
      INTEGER, INTENT(INOUT) ::  kgrdobs   ! Number of observations failing the check
      INTEGER, DIMENSION(kobsno), INTENT(INOUT) :: &
         & kobsqc           ! Quality control flag

      !! * Local declarations
      INTEGER :: jobs       ! Loop variable

      ! Flag if the grid search failed

      DO jobs = 1, kobsno
         IF ( ( kobsi(jobs) <= 0 ) .AND. ( kobsj(jobs) <= 0 ) ) THEN
            kobsqc(jobs) = kobsqc(jobs) + 18
            kgrdobs = kgrdobs + 1
         ENDIF
      END DO
      
   END SUBROUTINE obs_coo_grd

   SUBROUTINE obs_coo_spc_2d( kobsno, kpi,     kpj,              &
      &                       kobsi,  kobsj,   pobslam, pobsphi, & 
      &                       plam,   pphi,    pmask,            &
      &                       kobsqc, kosdobs, klanobs,          &
      &                       knlaobs,ld_nea                     )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE obs_coo_spc_2d  ***
      !!
      !! ** Purpose : Check for points outside the domain and land points
      !!
      !! ** Method  : Remove the observations that are outside the model space
      !!              and time domain or located within model land cells.
      !!
      !! ** Action  : 
      !!   
      !! History :
      !!        !  2007-03  (A. Weaver, K. Mogensen)  Original
      !!        !  2007-06  (K. Mogensen et al) Reject obs. near land.
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Arguments
      INTEGER, INTENT(IN) :: kobsno    ! Total number of observations
      INTEGER, INTENT(IN) :: kpi       ! Number of grid points in (i,j)
      INTEGER, INTENT(IN) :: kpj
      INTEGER, DIMENSION(kobsno), INTENT(IN) :: &
         & kobsi, &           ! Observation (i,j) coordinates
         & kobsj
      REAL(KIND=wp), DIMENSION(kobsno), INTENT(IN) :: &
         & pobslam, &         ! Observation (lon,lat) coordinates
         & pobsphi
      REAL(KIND=wp), DIMENSION(kpi,kpj), INTENT(IN) :: &
         & plam, pphi         ! Model (lon,lat) coordinates
      REAL(KIND=wp), DIMENSION(kpi,kpj), INTENT(IN) :: &
         & pmask              ! Land mask array
      INTEGER, DIMENSION(kobsno), INTENT(INOUT) :: &
         & kobsqc             ! Observation quality control
      INTEGER, INTENT(INOUT) :: kosdobs   ! Observations outside space domain
      INTEGER, INTENT(INOUT) :: klanobs   ! Observations within a model land cell
      INTEGER, INTENT(INOUT) :: knlaobs   ! Observations near land
      LOGICAL, INTENT(IN) :: ld_nea       ! Flag observations near land
      !! * Local declarations
      REAL(KIND=wp), DIMENSION(2,2,kobsno) :: &
         & zgmsk              ! Grid mask
      REAL(KIND=wp), DIMENSION(2,2,kobsno) :: &
         & zglam, &           ! Model longitude at grid points
         & zgphi              ! Model latitude at grid points
      INTEGER, DIMENSION(2,2,kobsno) :: &
         & igrdi, &           ! Grid i,j
         & igrdj
      LOGICAL :: lgridobs           ! Is observation on a model grid point.
      INTEGER :: iig, ijg           ! i,j of observation on model grid point.
      INTEGER :: jobs, ji, jj
      
      ! Get grid point indices

      DO jobs = 1, kobsno
         
         ! For invalid points use 2,2

         IF ( kobsqc(jobs) >= 10 ) THEN

            igrdi(1,1,jobs) = 1
            igrdj(1,1,jobs) = 1
            igrdi(1,2,jobs) = 1
            igrdj(1,2,jobs) = 2
            igrdi(2,1,jobs) = 2
            igrdj(2,1,jobs) = 1
            igrdi(2,2,jobs) = 2
            igrdj(2,2,jobs) = 2

         ELSE

            igrdi(1,1,jobs) = kobsi(jobs)-1
            igrdj(1,1,jobs) = kobsj(jobs)-1
            igrdi(1,2,jobs) = kobsi(jobs)-1
            igrdj(1,2,jobs) = kobsj(jobs)
            igrdi(2,1,jobs) = kobsi(jobs)
            igrdj(2,1,jobs) = kobsj(jobs)-1
            igrdi(2,2,jobs) = kobsi(jobs)
            igrdj(2,2,jobs) = kobsj(jobs)

         ENDIF

      END DO
      
      CALL obs_int_comm_2d( 2, 2, kobsno, igrdi, igrdj, pmask, zgmsk )
      CALL obs_int_comm_2d( 2, 2, kobsno, igrdi, igrdj, plam, zglam )
      CALL obs_int_comm_2d( 2, 2, kobsno, igrdi, igrdj, pphi, zgphi )

      DO jobs = 1, kobsno

         ! Skip bad observations
         IF ( kobsqc(jobs) >= 10 ) CYCLE

         ! Flag if the observation falls outside the model spatial domain
         IF (       ( pobslam(jobs) < -180. ) &
            &  .OR. ( pobslam(jobs) >  180. ) &
            &  .OR. ( pobsphi(jobs) <  -90. ) &
            &  .OR. ( pobsphi(jobs) >   90. ) ) THEN
            kobsqc(jobs) = kobsqc(jobs) + 11
            kosdobs = kosdobs + 1
            CYCLE
         ENDIF

         ! Flag if the observation falls with a model land cell
         IF ( SUM( zgmsk(1:2,1:2,jobs) ) == 0.0_wp ) THEN
            kobsqc(jobs) = kobsqc(jobs)  + 12
            klanobs = klanobs + 1
            CYCLE
         ENDIF

         ! Check if this observation is on a grid point

         lgridobs = .FALSE.
         iig = -1
         ijg = -1
         DO jj = 1, 2
            DO ji = 1, 2
               IF ( ( ABS( zgphi(ji,jj,jobs) - pobsphi(jobs) ) < 1.0e-6_wp ) &
                  & .AND. &
                  & ( ABS( zglam(ji,jj,jobs) - pobslam(jobs) ) < 1.0e-6_wp ) &
                  & ) THEN
                  lgridobs = .TRUE.
                  iig = ji
                  ijg = jj
               ENDIF
            END DO
         END DO
  
         ! For observations on the grid reject them if their are at
         ! a masked point
         
         IF (lgridobs) THEN
            IF (zgmsk(iig,ijg,jobs) == 0.0_wp ) THEN
               kobsqc(jobs) = kobsqc(jobs) + 12
               klanobs = klanobs + 1
               CYCLE
            ENDIF
         ENDIF
                      
         ! Flag if the observation falls is close to land
         IF ( MINVAL( zgmsk(1:2,1:2,jobs) ) == 0.0_wp) THEN
            IF (ld_nea) kobsqc(jobs) = kobsqc(jobs) + 14
            knlaobs = knlaobs + 1
            CYCLE
         ENDIF
            
      END DO

   END SUBROUTINE obs_coo_spc_2d

   SUBROUTINE obs_coo_spc_3d( kprofno, kobsno,  kpstart, kpend, &
      &                       kpi,     kpj,     kpk,            &
      &                       kobsi,   kobsj,   kobsk,          &
      &                       pobslam, pobsphi, pobsdep,        &
      &                       plam,    pphi,    pdep,    pmask, &
      &                       kpobsqc, kobsqc,  kosdobs,        &
      &                       klanobs, knlaobs, ld_nea          )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE obs_coo_spc_3d  ***
      !!
      !! ** Purpose : Check for points outside the domain and land points
      !!              Reset depth of observation above highest model level
      !!              to the value of highest model level
      !!
      !! ** Method  : Remove the observations that are outside the model space
      !!              and time domain or located within model land cells.
      !!
      !!              NB. T and S profile observations lying between the ocean
      !!              surface and the depth of the first model T point are 
      !!              assigned a depth equal to that of the first model T pt.
      !!
      !! ** Action  : 
      !!   
      !! History :
      !!        !  2007-01  (K. Mogensen) Rewrite of parts of obs_scr
      !!        !  2007-06  (K. Mogensen et al) Reject obs. near land.
      !!----------------------------------------------------------------------
      !! * Modules used
      USE dom_oce, ONLY : &       ! Geographical information
         & gdepw_0                        

      !! * Arguments
      INTEGER, INTENT(IN) :: kprofno      ! Number of profiles
      INTEGER, INTENT(IN) :: kobsno       ! Total number of observations
      INTEGER, INTENT(IN) :: kpi          ! Number of grid points in (i,j,k)
      INTEGER, INTENT(IN) :: kpj
      INTEGER, INTENT(IN) :: kpk    
      INTEGER, DIMENSION(kprofno), INTENT(IN) :: &
         & kpstart, &         ! Start of individual profiles
         & kpend              ! End of individual profiles
      INTEGER, DIMENSION(kprofno), INTENT(IN) :: &
         & kobsi, &           ! Observation (i,j) coordinates
         & kobsj
      INTEGER, DIMENSION(kobsno), INTENT(IN) :: &
         & kobsk              ! Observation k coordinate
      REAL(KIND=wp), DIMENSION(kprofno), INTENT(IN) :: &
         & pobslam, &         ! Observation (lon,lat) coordinates
         & pobsphi
      REAL(KIND=wp), DIMENSION(kobsno), INTENT(INOUT) :: &
         & pobsdep            ! Observation depths  
      REAL(KIND=wp), DIMENSION(kpi,kpj), INTENT(IN) :: &
         & plam, pphi         ! Model (lon,lat) coordinates
      REAL(KIND=wp), DIMENSION(kpk), INTENT(IN) :: &
         & pdep               ! Model depth coordinates
      REAL(KIND=wp), DIMENSION(kpi,kpj,kpk), INTENT(IN) :: &
         & pmask              ! Land mask array
      INTEGER, DIMENSION(kprofno), INTENT(INOUT) :: &
         & kpobsqc             ! Profile quality control
      INTEGER, DIMENSION(kobsno), INTENT(INOUT) :: &
         & kobsqc             ! Observation quality control
      INTEGER, INTENT(INOUT) :: kosdobs     ! Observations outside space domain
      INTEGER, INTENT(INOUT) :: klanobs     ! Observations within a model land cell
      INTEGER, INTENT(INOUT) :: knlaobs     ! Observations near land
      LOGICAL, INTENT(IN) :: ld_nea         ! Flag observations near land
      !! * Local declarations
      REAL(KIND=wp), DIMENSION(2,2,kpk,kprofno) :: &
         & zgmsk              ! Grid mask
      REAL(KIND=wp), DIMENSION(2,2,kprofno) :: &
         & zglam, &           ! Model longitude at grid points
         & zgphi              ! Model latitude at grid points
      INTEGER, DIMENSION(2,2,kprofno) :: &
         & igrdi, &           ! Grid i,j
         & igrdj
      LOGICAL :: lgridobs           ! Is observation on a model grid point.
      INTEGER :: iig, ijg           ! i,j of observation on model grid point.
      INTEGER :: jobs, jobsp, jk, ji, jj

      ! Get grid point indices
      
      DO jobs = 1, kprofno

         ! For invalid points use 2,2

         IF ( kpobsqc(jobs) >= 10 ) THEN
            
            igrdi(1,1,jobs) = 1
            igrdj(1,1,jobs) = 1
            igrdi(1,2,jobs) = 1
            igrdj(1,2,jobs) = 2
            igrdi(2,1,jobs) = 2
            igrdj(2,1,jobs) = 1
            igrdi(2,2,jobs) = 2
            igrdj(2,2,jobs) = 2
            
         ELSE
            
            igrdi(1,1,jobs) = kobsi(jobs)-1
            igrdj(1,1,jobs) = kobsj(jobs)-1
            igrdi(1,2,jobs) = kobsi(jobs)-1
            igrdj(1,2,jobs) = kobsj(jobs)
            igrdi(2,1,jobs) = kobsi(jobs)
            igrdj(2,1,jobs) = kobsj(jobs)-1
            igrdi(2,2,jobs) = kobsi(jobs)
            igrdj(2,2,jobs) = kobsj(jobs)
            
         ENDIF
         
      END DO
      
      CALL obs_int_comm_3d( 2, 2, kprofno, kpk, igrdi, igrdj, pmask, zgmsk )
      CALL obs_int_comm_2d( 2, 2, kprofno, igrdi, igrdj, plam, zglam )
      CALL obs_int_comm_2d( 2, 2, kprofno, igrdi, igrdj, pphi, zgphi )

      DO jobs = 1, kprofno

         ! Skip bad profiles
         IF ( kpobsqc(jobs) >= 10 ) CYCLE

         ! Check if this observation is on a grid point

         lgridobs = .FALSE.
         iig = -1
         ijg = -1
         DO jj = 1, 2
            DO ji = 1, 2
               IF ( ( ABS( zgphi(ji,jj,jobs) - pobsphi(jobs) ) < 1.0e-6_wp ) &
                  & .AND. &
                  & ( ABS( zglam(ji,jj,jobs) - pobslam(jobs) ) < 1.0e-6_wp ) &
                  & ) THEN
                  lgridobs = .TRUE.
                  iig = ji
                  ijg = jj
               ENDIF
            END DO
         END DO

         ! Reject observations

         DO jobsp = kpstart(jobs), kpend(jobs)

            ! Flag if the observation falls outside the model spatial domain
            IF (       ( pobslam(jobs) < -180.         )       &
               &  .OR. ( pobslam(jobs) >  180.         )       &
               &  .OR. ( pobsphi(jobs) <  -90.         )       &
               &  .OR. ( pobsphi(jobs) >   90.         )       &
               &  .OR. ( pobsdep(jobsp) < 0.0          )       &
               &  .OR. ( pobsdep(jobsp) > gdepw_0(kpk) ) ) THEN
               kobsqc(jobsp) = kobsqc(jobsp) + 11
               kosdobs = kosdobs + 1
               CYCLE
            ENDIF

            ! Flag if the observation falls with a model land cell
            IF ( SUM( zgmsk(1:2,1:2,kobsk(jobsp)-1:kobsk(jobsp),jobs) ) &
               &  == 0.0_wp ) THEN
               kobsqc(jobsp) = kobsqc(jobsp) + 12
               klanobs = klanobs + 1
               CYCLE
            ENDIF

            ! For observations on the grid reject them if their are at
            ! a masked point
            
            IF (lgridobs) THEN
               IF (zgmsk(iig,ijg,kobsk(jobsp)-1,jobs) == 0.0_wp ) THEN
                  kobsqc(jobsp) = kobsqc(jobsp) + 12
                  klanobs = klanobs + 1
                  CYCLE
               ENDIF
            ENDIF
            
            ! Flag if the observation falls is close to land
            IF ( MINVAL( zgmsk(1:2,1:2,kobsk(jobsp)-1:kobsk(jobsp),jobs) ) == &
               &  0.0_wp) THEN
               IF (ld_nea) kobsqc(jobsp) = kobsqc(jobsp) + 14
               knlaobs = knlaobs + 1
            ENDIF

            ! Set observation depth equal to that of the first model depth
            IF ( pobsdep(jobsp) <= pdep(1) ) THEN
               pobsdep(jobsp) = pdep(1)  
            ENDIF
            
         END DO
      END DO

   END SUBROUTINE obs_coo_spc_3d

   SUBROUTINE obs_pro_rej( profdata )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE obs_pro_rej ***
      !!
      !! ** Purpose : Reject all data within a rejected profile
      !!
      !! ** Method  : 
      !!
      !! ** Action  : 
      !!
      !! References :
      !!   
      !! History :
      !!        !  2007-10  (K. Mogensen) Original code
      !!----------------------------------------------------------------------
      !! * Modules used
      !! * Arguments
      TYPE(obs_prof), INTENT(INOUT) :: profdata     ! Profile data
      !! * Local declarations
      INTEGER :: jprof
      INTEGER :: jvar
      INTEGER :: jobs
      
      ! Loop over profiles

      DO jprof = 1, profdata%nprof

         IF ( profdata%nqc(jprof) > 10 ) THEN
            
            DO jvar = 1, profdata%nvar

               DO jobs = profdata%npvsta(jprof,jvar),  &
                  &      profdata%npvend(jprof,jvar)
                  
                  profdata%var(jvar)%nvqc(jobs) = &
                     & profdata%var(jvar)%nvqc(jobs) + 26

               END DO

            END DO

         ENDIF

      END DO

   END SUBROUTINE obs_pro_rej

   SUBROUTINE obs_uv_rej( profdata, knumu, knumv )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE obs_uv_rej ***
      !!
      !! ** Purpose : Reject u if v is rejected and vice versa
      !!
      !! ** Method  : 
      !!
      !! ** Action  : 
      !!
      !! References :
      !!   
      !! History :
      !!        !  2009-2  (K. Mogensen) Original code
      !!----------------------------------------------------------------------
      !! * Modules used
      !! * Arguments
      TYPE(obs_prof), INTENT(INOUT) :: profdata   ! Profile data
      INTEGER, INTENT(INOUT) :: knumu             ! Number of u rejected
      INTEGER, INTENT(INOUT) :: knumv             ! Number of v rejected
      !! * Local declarations
      INTEGER :: jprof
      INTEGER :: jvar
      INTEGER :: jobs
      
      ! Loop over profiles

      DO jprof = 1, profdata%nprof

         IF ( ( profdata%npvsta(jprof,1) /= profdata%npvsta(jprof,2) ) .OR. &
            & ( profdata%npvend(jprof,1) /= profdata%npvend(jprof,2) ) ) THEN

            CALL ctl_stop('U,V profiles inconsistent in obs_uv_rej')
            RETURN

         ENDIF

         DO jobs = profdata%npvsta(jprof,1), profdata%npvend(jprof,1)
            
            IF ( ( profdata%var(1)%nvqc(jobs) > 10 ) .AND. &
               & ( profdata%var(2)%nvqc(jobs) <= 10) ) THEN
               profdata%var(2)%nvqc(jobs) = profdata%var(2)%nvqc(jobs) + 42
               knumv = knumv + 1
            ENDIF
            IF ( ( profdata%var(2)%nvqc(jobs) > 10 ) .AND. &
               & ( profdata%var(1)%nvqc(jobs) <= 10) ) THEN
               profdata%var(1)%nvqc(jobs) = profdata%var(1)%nvqc(jobs) + 42
               knumu = knumu + 1
            ENDIF
            
         END DO
            
      END DO

   END SUBROUTINE obs_uv_rej

END MODULE obs_prep
