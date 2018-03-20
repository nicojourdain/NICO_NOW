MODULE diaobs
   !!======================================================================
   !!                       ***  MODULE diaobs  ***
   !! Observation diagnostics: Computation of the misfit between data and
   !!                          their model equivalent 
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   'key_diaobs' : Switch on the observation diagnostic computation
   !!----------------------------------------------------------------------
   !!   dia_obs_init : Reading and prepare observations
   !!   dia_obs      : Compute model equivalent to observations
   !!   dia_obs_wri  : Write observational diagnostics
   !!   ini_date     : Compute the initial date YYYYMMDD.HHMMSS
   !!   fin_date     : Compute the final date YYYYMMDD.HHMMSS
   !!----------------------------------------------------------------------
   !! * Modules used   
   USE wrk_nemo                 ! Memory Allocation
   USE par_kind                 ! Precision variables
   USE in_out_manager           ! I/O manager
   USE par_oce
   USE dom_oce                  ! Ocean space and time domain variables
   USE obs_read_prof            ! Reading and allocation of observations (Coriolis)
   USE obs_read_sla             ! Reading and allocation of SLA observations  
   USE obs_read_sst             ! Reading and allocation of SST observations  
   USE obs_readmdt              ! Reading and allocation of MDT for SLA.
   USE obs_read_seaice          ! Reading and allocation of Sea Ice observations  
   USE obs_read_vel             ! Reading and allocation of velocity component observations
   USE obs_prep                 ! Preparation of obs. (grid search etc).
   USE obs_oper                 ! Observation operators
   USE obs_write                ! Writing of observation related diagnostics
   USE obs_grid                 ! Grid searching
   USE obs_read_altbias         ! Bias treatment for altimeter
   USE obs_profiles_def         ! Profile data definitions
   USE obs_profiles             ! Profile data storage
   USE obs_surf_def             ! Surface data definitions
   USE obs_sla                  ! SLA data storage
   USE obs_sst                  ! SST data storage
   USE obs_seaice               ! Sea Ice data storage
   USE obs_types                ! Definitions for observation types
   USE mpp_map                  ! MPP mapping
   USE lib_mpp                  ! For ctl_warn/stop

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE
   PUBLIC dia_obs_init, &  ! Initialize and read observations
      &   dia_obs,      &  ! Compute model equivalent to observations
      &   dia_obs_wri      ! Write model equivalent to observations

   !! * Shared Module variables
   LOGICAL, PUBLIC, PARAMETER :: &
#if defined key_diaobs
      & lk_diaobs = .TRUE.   !: Logical switch for observation diangostics
#else
      & lk_diaobs = .FALSE.  !: Logical switch for observation diangostics
#endif

   !! * Module variables
   LOGICAL, PUBLIC :: ln_t3d         !: Logical switch for temperature profiles
   LOGICAL, PUBLIC :: ln_s3d         !: Logical switch for salinity profiles
   LOGICAL, PUBLIC :: ln_ena         !: Logical switch for the ENACT data set
   LOGICAL, PUBLIC :: ln_cor         !: Logical switch for the Coriolis data set
   LOGICAL, PUBLIC :: ln_profb       !: Logical switch for profile feedback datafiles
   LOGICAL, PUBLIC :: ln_sla         !: Logical switch for sea level anomalies 
   LOGICAL, PUBLIC :: ln_sladt       !: Logical switch for SLA from AVISO files
   LOGICAL, PUBLIC :: ln_slafb       !: Logical switch for SLA from feedback files
   LOGICAL, PUBLIC :: ln_sst         !: Logical switch for sea surface temperature
   LOGICAL, PUBLIC :: ln_reysst      !: Logical switch for Reynolds sea surface temperature
   LOGICAL, PUBLIC :: ln_ghrsst      !: Logical switch for GHRSST data
   LOGICAL, PUBLIC :: ln_sstfb       !: Logical switch for SST from feedback files
   LOGICAL, PUBLIC :: ln_seaice      !: Logical switch for sea ice concentration
   LOGICAL, PUBLIC :: ln_vel3d       !: Logical switch for velocity component (u,v) observations
   LOGICAL, PUBLIC :: ln_velavcur    !: Logical switch for raw daily averaged netCDF current meter vel. data 
   LOGICAL, PUBLIC :: ln_velhrcur    !: Logical switch for raw high freq netCDF current meter vel. data 
   LOGICAL, PUBLIC :: ln_velavadcp   !: Logical switch for raw daily averaged netCDF ADCP vel. data 
   LOGICAL, PUBLIC :: ln_velhradcp   !: Logical switch for raw high freq netCDF ADCP vel. data 
   LOGICAL, PUBLIC :: ln_velfb       !: Logical switch for velocities from feedback files
   LOGICAL, PUBLIC :: ln_ssh         !: Logical switch for sea surface height
   LOGICAL, PUBLIC :: ln_sss         !: Logical switch for sea surface salinity
   LOGICAL, PUBLIC :: ln_nea         !: Remove observations near land
   LOGICAL, PUBLIC :: ln_altbias     !: Logical switch for altimeter bias  
   LOGICAL, PUBLIC :: ln_ignmis      !: Logical switch for ignoring missing files
   LOGICAL, PUBLIC :: ln_s_at_t      !: Logical switch to compute model S at T observations

   REAL(KIND=dp), PUBLIC :: dobsini   !: Observation window start date YYYYMMDD.HHMMSS
   REAL(KIND=dp), PUBLIC :: dobsend   !: Observation window end date YYYYMMDD.HHMMSS
  
   INTEGER, PUBLIC :: n1dint       !: Vertical interpolation method
   INTEGER, PUBLIC :: n2dint       !: Horizontal interpolation method 

   INTEGER, DIMENSION(imaxavtypes) :: &
      & endailyavtypes !: ENACT data types which are daily average

   INTEGER, PARAMETER :: MaxNumFiles = 1000
   LOGICAL, DIMENSION(MaxNumFiles) :: &
      & ln_profb_ena, & !: Is the feedback files from ENACT data ?
   !                    !: If so use endailyavtypes
      & ln_profb_enatim !: Change tim for 820 enact data set.
   
   LOGICAL, DIMENSION(MaxNumFiles) :: &
      & ln_velfb_av   !: Is the velocity feedback files daily average?
   LOGICAL, DIMENSION(:), ALLOCATABLE :: &
      & ld_enact     !: Profile data is ENACT so use endailyavtypes
   LOGICAL, DIMENSION(:), ALLOCATABLE :: &
      & ld_velav     !: Velocity data is daily averaged

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: diaobs.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE dia_obs_init
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_obs_init  ***
      !!          
      !! ** Purpose : Initialize and read observations
      !!
      !! ** Method  : Read the namelist and call reading routines
      !!
      !! ** Action  : Read the namelist and call reading routines
      !!
      !! History :
      !!        !  06-03  (K. Mogensen) Original code
      !!        !  06-05  (A. Weaver) Reformatted
      !!        !  06-10  (A. Weaver) Cleaning and add controls
      !!        !  07-03  (K. Mogensen) General handling of profiles
      !!----------------------------------------------------------------------

      IMPLICIT NONE

      !! * Local declarations
      CHARACTER(len=128) :: enactfiles(MaxNumFiles)
      CHARACTER(len=128) :: coriofiles(MaxNumFiles)
      CHARACTER(len=128) :: profbfiles(MaxNumFiles)
      CHARACTER(len=128) :: sstfiles(MaxNumFiles)      
      CHARACTER(len=128) :: sstfbfiles(MaxNumFiles) 
      CHARACTER(len=128) :: slafilesact(MaxNumFiles)      
      CHARACTER(len=128) :: slafilespas(MaxNumFiles)      
      CHARACTER(len=128) :: slafbfiles(MaxNumFiles)
      CHARACTER(len=128) :: seaicefiles(MaxNumFiles)           
      CHARACTER(len=128) :: velcurfiles(MaxNumFiles)  
      CHARACTER(len=128) :: veladcpfiles(MaxNumFiles)    
      CHARACTER(len=128) :: velavcurfiles(MaxNumFiles)
      CHARACTER(len=128) :: velhrcurfiles(MaxNumFiles)
      CHARACTER(len=128) :: velavadcpfiles(MaxNumFiles)
      CHARACTER(len=128) :: velhradcpfiles(MaxNumFiles)
      CHARACTER(len=128) :: velfbfiles(MaxNumFiles)
      CHARACTER(LEN=128) :: reysstname
      CHARACTER(LEN=12)  :: reysstfmt
      CHARACTER(LEN=128) :: bias_file
      CHARACTER(LEN=20)  :: datestr=" ", timestr=" "
      NAMELIST/namobs/ln_ena, ln_cor, ln_profb, ln_t3d, ln_s3d,       &
         &            ln_sla, ln_sladt, ln_slafb,                     &
         &            ln_ssh, ln_sst, ln_sstfb, ln_sss, ln_nea,       &
         &            enactfiles, coriofiles, profbfiles,             &
         &            slafilesact, slafilespas, slafbfiles,           &
         &            sstfiles, sstfbfiles,                           &
         &            ln_seaice, seaicefiles,                         &
         &            dobsini, dobsend, n1dint, n2dint,               &
         &            nmsshc, mdtcorr, mdtcutoff,                     &
         &            ln_reysst, ln_ghrsst, reysstname, reysstfmt,    &
         &            ln_grid_search_lookup,                          &
         &            grid_search_file, grid_search_res,              &
         &            ln_grid_global, bias_file, ln_altbias,          &
         &            endailyavtypes, ln_s_at_t, ln_profb_ena,        &
         &            ln_vel3d, ln_velavcur, velavcurfiles,           &
         &            ln_velhrcur, velhrcurfiles,                     &
         &            ln_velavadcp, velavadcpfiles,                   &
         &            ln_velhradcp, velhradcpfiles,                   &
         &            ln_velfb, velfbfiles, ln_velfb_av,              &
         &            ln_profb_enatim, ln_ignmis

      INTEGER :: jprofset
      INTEGER :: jveloset
      INTEGER :: jvar
      INTEGER :: jnumenact
      INTEGER :: jnumcorio
      INTEGER :: jnumprofb
      INTEGER :: jnumslaact
      INTEGER :: jnumslapas
      INTEGER :: jnumslafb
      INTEGER :: jnumsst
      INTEGER :: jnumsstfb
      INTEGER :: jnumseaice
      INTEGER :: jnumvelavcur
      INTEGER :: jnumvelhrcur  
      INTEGER :: jnumvelavadcp
      INTEGER :: jnumvelhradcp   
      INTEGER :: jnumvelfb
      INTEGER :: ji
      INTEGER :: jset
      LOGICAL :: lmask(MaxNumFiles), ll_u3d, ll_v3d

      !-----------------------------------------------------------------------
      ! Read namelist parameters
      !-----------------------------------------------------------------------
      ! default values already set except:

      ln_t3d = .FALSE.
      ln_s3d = .FALSE.
      ln_vel3d = .FALSE.
      ln_sla = .FALSE.
      ln_altbias = .FALSE.
      ln_ssh = .FALSE.
      ln_sst = .FALSE.
      ln_seaice = .FALSE.
      ln_reysst = .FALSE.
      ln_ghrsst = .FALSE.
      ln_sss = .FALSE.
      ln_profb = .FALSE.
      ln_ena = .TRUE.
      ln_cor = .FALSE.
      ln_sladt = .TRUE.
      ln_slafb = .FALSE.
      ln_sstfb = .FALSE.
      ln_velavcur = .FALSE.
      ln_velhrcur = .FALSE.
      ln_velavadcp = .FALSE.
      ln_velhradcp = .FALSE.
      ln_velfb = .FALSE.
      ln_nea = .FALSE.
      ln_grid_search_lookup = .FALSE.
      ln_grid_global = .FALSE.
      ln_s_at_t = .TRUE.
      grid_search_file = 'xypos'
      bias_file='bias.nc'
      enactfiles(:) = ''
      coriofiles(:) = ''
      profbfiles(:) = ''
      slafilesact(:) = ''
      slafilespas(:) = ''
      slafbfiles(:) = ''
      sstfiles(:)   = ''
      sstfbfiles(:) = ''
      seaicefiles(:) = ''
      velcurfiles(:) = ''
      veladcpfiles(:) = ''
      velavcurfiles(:) = ''
      velhrcurfiles(:) = ''
      velavadcpfiles(:) = ''
      velhradcpfiles(:) = ''
      velfbfiles(:) = ''
      reysstname = 'sst_yYYYYmMM.nc'
      reysstfmt = 'monthly'
      endailyavtypes(:) = -1
      endailyavtypes(1) = 820
      ln_profb_ena(:) = .FALSE.
      ln_profb_enatim(:) = .TRUE.
      ln_velfb_av(:) = .FALSE.
      ln_ignmis = .FALSE.
      CALL ini_date( dobsini )
      CALL fin_date( dobsend )
      n1dint = 1
      n2dint = 3

      ! Read Namelist namobs : control observation diagnostics
      REWIND( numnam )
      READ  ( numnam, namobs )

      ! Count number of files for each type
      IF (ln_ena) THEN
         lmask(:) = .FALSE.
         WHERE (enactfiles(:) /= '') lmask(:) = .TRUE.
         jnumenact = COUNT(lmask)
      ENDIF
      IF (ln_cor) THEN
         lmask(:) = .FALSE.
         WHERE (coriofiles(:) /= '') lmask(:) = .TRUE.
         jnumcorio = COUNT(lmask)
      ENDIF
      IF (ln_profb) THEN
         lmask(:) = .FALSE.
         WHERE (profbfiles(:) /= '') lmask(:) = .TRUE.
         jnumprofb = COUNT(lmask)
      ENDIF
      IF (ln_sladt) THEN
         lmask(:) = .FALSE.
         WHERE (slafilesact(:) /= '') lmask(:) = .TRUE.
         jnumslaact = COUNT(lmask)
         lmask(:) = .FALSE.
         WHERE (slafilespas(:) /= '') lmask(:) = .TRUE.
         jnumslapas = COUNT(lmask)
      ENDIF
      IF (ln_slafb) THEN
         lmask(:) = .FALSE.
         WHERE (slafbfiles(:) /= '') lmask(:) = .TRUE.
         jnumslafb = COUNT(lmask)
         lmask(:) = .FALSE.
      ENDIF
      IF (ln_ghrsst) THEN
         lmask(:) = .FALSE.
         WHERE (sstfiles(:) /= '') lmask(:) = .TRUE.
         jnumsst = COUNT(lmask)
      ENDIF      
      IF (ln_sstfb) THEN
         lmask(:) = .FALSE.
         WHERE (sstfbfiles(:) /= '') lmask(:) = .TRUE.
         jnumsstfb = COUNT(lmask)
         lmask(:) = .FALSE.
      ENDIF
      IF (ln_seaice) THEN
         lmask(:) = .FALSE.
         WHERE (seaicefiles(:) /= '') lmask(:) = .TRUE.
         jnumseaice = COUNT(lmask)
      ENDIF
      IF (ln_velavcur) THEN
         lmask(:) = .FALSE.
         WHERE (velavcurfiles(:) /= '') lmask(:) = .TRUE.
         jnumvelavcur = COUNT(lmask)
      ENDIF
      IF (ln_velhrcur) THEN
         lmask(:) = .FALSE.
         WHERE (velhrcurfiles(:) /= '') lmask(:) = .TRUE.
         jnumvelhrcur = COUNT(lmask)
      ENDIF
      IF (ln_velavadcp) THEN
         lmask(:) = .FALSE.
         WHERE (velavadcpfiles(:) /= '') lmask(:) = .TRUE.
         jnumvelavadcp = COUNT(lmask)
      ENDIF
      IF (ln_velhradcp) THEN
         lmask(:) = .FALSE.
         WHERE (velhradcpfiles(:) /= '') lmask(:) = .TRUE.
         jnumvelhradcp = COUNT(lmask)
      ENDIF
      IF (ln_velfb) THEN
         lmask(:) = .FALSE.
         WHERE (velfbfiles(:) /= '') lmask(:) = .TRUE.
         jnumvelfb = COUNT(lmask)
         lmask(:) = .FALSE.
      ENDIF
      
      ! Control print
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dia_obs_init : Observation diagnostic initialization'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '          Namelist namobs : set observation diagnostic parameters' 
         WRITE(numout,*) '             Logical switch for T profile observations          ln_t3d = ', ln_t3d
         WRITE(numout,*) '             Logical switch for S profile observations          ln_s3d = ', ln_s3d
         WRITE(numout,*) '             Logical switch for ENACT insitu data set           ln_ena = ', ln_ena
         WRITE(numout,*) '             Logical switch for Coriolis insitu data set        ln_cor = ', ln_cor
         WRITE(numout,*) '             Logical switch for feedback insitu data set      ln_profb = ', ln_profb
         WRITE(numout,*) '             Logical switch for SLA observations                ln_sla = ', ln_sla
         WRITE(numout,*) '             Logical switch for AVISO SLA data                ln_sladt = ', ln_sladt
         WRITE(numout,*) '             Logical switch for feedback SLA data             ln_slafb = ', ln_slafb
         WRITE(numout,*) '             Logical switch for SSH observations                ln_ssh = ', ln_ssh
         WRITE(numout,*) '             Logical switch for SST observations                ln_sst = ', ln_sst
         WRITE(numout,*) '             Logical switch for Reynolds observations        ln_reysst = ', ln_reysst    
         WRITE(numout,*) '             Logical switch for GHRSST observations          ln_ghrsst = ', ln_ghrsst
         WRITE(numout,*) '             Logical switch for feedback SST data             ln_sstfb = ', ln_sstfb
         WRITE(numout,*) '             Logical switch for SSS observations                ln_sss = ', ln_sss
         WRITE(numout,*) '             Logical switch for Sea Ice observations         ln_seaice = ', ln_seaice
         WRITE(numout,*) '             Logical switch for velocity observations         ln_vel3d = ', ln_vel3d
         WRITE(numout,*) '             Logical switch for velocity daily av. cur.    ln_velavcur = ', ln_velavcur
         WRITE(numout,*) '             Logical switch for velocity high freq. cur.   ln_velhrcur = ', ln_velhrcur
         WRITE(numout,*) '             Logical switch for velocity daily av. ADCP   ln_velavadcp = ', ln_velavadcp
         WRITE(numout,*) '             Logical switch for velocity high freq. ADCP  ln_velhradcp = ', ln_velhradcp
         WRITE(numout,*) '             Logical switch for feedback velocity data        ln_velfb = ', ln_velfb
         WRITE(numout,*) '             Global distribtion of observations         ln_grid_global = ',ln_grid_global
         WRITE(numout,*) &
   '             Logical switch for obs grid search w/lookup table  ln_grid_search_lookup = ',ln_grid_search_lookup
         IF (ln_grid_search_lookup) &
            WRITE(numout,*) '             Grid search lookup file header       grid_search_file = ', grid_search_file
         IF (ln_ena) THEN
            DO ji = 1, jnumenact
               WRITE(numout,'(1X,2A)') '             ENACT input observation file name          enactfiles = ', &
                  TRIM(enactfiles(ji))
            END DO
         ENDIF
         IF (ln_cor) THEN
            DO ji = 1, jnumcorio
               WRITE(numout,'(1X,2A)') '             Coriolis input observation file name       coriofiles = ', &
                  TRIM(coriofiles(ji))
            END DO
         ENDIF
         IF (ln_profb) THEN
            DO ji = 1, jnumprofb
               IF (ln_profb_ena(ji)) THEN
                  WRITE(numout,'(1X,2A)') '       Enact feedback input observation file name       profbfiles = ', &
                     TRIM(profbfiles(ji))
               ELSE
                  WRITE(numout,'(1X,2A)') '             Feedback input observation file name       profbfiles = ', &
                     TRIM(profbfiles(ji))
               ENDIF
               WRITE(numout,'(1X,2A)') '       Enact feedback input time setting switch    ln_profb_enatim = ', ln_profb_enatim(ji)
            END DO
         ENDIF
         IF (ln_sladt) THEN
            DO ji = 1, jnumslaact
               WRITE(numout,'(1X,2A)') '             Active SLA input observation file name    slafilesact = ', &
                  TRIM(slafilesact(ji))
            END DO
            DO ji = 1, jnumslapas
               WRITE(numout,'(1X,2A)') '             Passive SLA input observation file name   slafilespas = ', &
                  TRIM(slafilespas(ji))
            END DO
         ENDIF
         IF (ln_slafb) THEN
            DO ji = 1, jnumslafb
               WRITE(numout,'(1X,2A)') '             Feedback SLA input observation file name   slafbfiles = ', &
                  TRIM(slafbfiles(ji))
            END DO
         ENDIF
         IF (ln_ghrsst) THEN
            DO ji = 1, jnumsst
               WRITE(numout,'(1X,2A)') '             GHRSST input observation file name           sstfiles = ', &
                  TRIM(sstfiles(ji))
            END DO
         ENDIF
         IF (ln_sstfb) THEN
            DO ji = 1, jnumsstfb
               WRITE(numout,'(1X,2A)') '             Feedback SST input observation file name   sstfbfiles = ', &
                  TRIM(sstfbfiles(ji))
            END DO
         ENDIF
         IF (ln_seaice) THEN
            DO ji = 1, jnumseaice
               WRITE(numout,'(1X,2A)') '             Sea Ice input observation file name       seaicefiles = ', &
                  TRIM(seaicefiles(ji))
            END DO
         ENDIF
         IF (ln_velavcur) THEN
            DO ji = 1, jnumvelavcur
               WRITE(numout,'(1X,2A)') '             Vel. cur. daily av. input file name     velavcurfiles = ', &
                  TRIM(velavcurfiles(ji))
            END DO
         ENDIF
         IF (ln_velhrcur) THEN
            DO ji = 1, jnumvelhrcur
               WRITE(numout,'(1X,2A)') '             Vel. cur. high freq. input file name    velhvcurfiles = ', &
                  TRIM(velhrcurfiles(ji))
            END DO
         ENDIF
         IF (ln_velavadcp) THEN
            DO ji = 1, jnumvelavadcp
               WRITE(numout,'(1X,2A)') '             Vel. ADCP daily av. input file name    velavadcpfiles = ', &
                  TRIM(velavadcpfiles(ji))
            END DO
         ENDIF
         IF (ln_velhradcp) THEN
            DO ji = 1, jnumvelhradcp
               WRITE(numout,'(1X,2A)') '             Vel. ADCP high freq. input file name   velhvadcpfiles = ', &
                  TRIM(velhradcpfiles(ji))
            END DO
         ENDIF
         IF (ln_velfb) THEN
            DO ji = 1, jnumvelfb
               IF (ln_velfb_av(ji)) THEN
                  WRITE(numout,'(1X,2A)') '             Vel. feedback daily av. input file name    velfbfiles = ', &
                     TRIM(velfbfiles(ji))
               ELSE
                  WRITE(numout,'(1X,2A)') '             Vel. feedback input observation file name  velfbfiles = ', &
                     TRIM(velfbfiles(ji))
               ENDIF
            END DO
         ENDIF
         WRITE(numout,*) '             Initial date in window YYYYMMDD.HHMMSS        dobsini = ', dobsini
         WRITE(numout,*) '             Final date in window YYYYMMDD.HHMMSS          dobsend = ', dobsend
         WRITE(numout,*) '             Type of vertical interpolation method          n1dint = ', n1dint
         WRITE(numout,*) '             Type of horizontal interpolation method        n2dint = ', n2dint
         WRITE(numout,*) '             Rejection of observations near land swithch    ln_nea = ', ln_nea
         WRITE(numout,*) '             MSSH correction scheme                         nmsshc = ', nmsshc
         WRITE(numout,*) '             MDT  correction                               mdtcorr = ', mdtcorr
         WRITE(numout,*) '             MDT cutoff for computed correction          mdtcutoff = ', mdtcutoff
         WRITE(numout,*) '             Logical switch for alt bias                ln_altbias = ', ln_altbias
         WRITE(numout,*) '             Logical switch for ignoring missing files   ln_ignmis = ', ln_ignmis
         WRITE(numout,*) '             ENACT daily average types                             = ',endailyavtypes

      ENDIF
      
      IF ( ln_vel3d .AND. ( .NOT. ln_grid_global ) ) THEN
         CALL ctl_stop( 'Velocity data only works with ln_grid_global=.true.' )
         RETURN
      ENDIF

      CALL obs_typ_init
      
      CALL mppmap_init
      
      ! Parameter control
#if defined key_diaobs
      IF ( ( .NOT. ln_t3d ).AND.( .NOT. ln_s3d ).AND.( .NOT. ln_sla ).AND. &
         & ( .NOT. ln_vel3d ).AND.                                         &
         & ( .NOT. ln_ssh ).AND.( .NOT. ln_sst ).AND.( .NOT. ln_sss ).AND. &
         & ( .NOT. ln_seaice ).AND.( .NOT. ln_vel3d ) ) THEN
         IF(lwp) WRITE(numout,cform_war)
         IF(lwp) WRITE(numout,*) ' key_diaobs is activated but logical flags', &
            &                    ' ln_t3d, ln_s3d, ln_sla, ln_ssh, ln_sst, ln_sss, ln_seaice, ln_vel3d are all set to .FALSE.'
         nwarn = nwarn + 1
      ENDIF
#endif

      CALL obs_grid_setup( )
      IF ( ( n1dint < 0 ).OR.( n1dint > 1 ) ) THEN
         IF(lwp) WRITE(numout,cform_err)
         IF(lwp) WRITE(numout,*) ' Choice of vertical (1D) interpolation method', &
            &                    ' is not available'
         nstop = nstop + 1
      ENDIF
      IF ( ( n2dint < 0 ).OR.( n2dint > 4 ) ) THEN
         IF(lwp) WRITE(numout,cform_err)
         IF(lwp) WRITE(numout,*) ' Choice of horizontal (2D) interpolation method', &
            &                    ' is not available'
         nstop = nstop + 1
      ENDIF

      !-----------------------------------------------------------------------
      ! Depending on switches read the various observation types
      !-----------------------------------------------------------------------
      !  - Temperature/salinity profiles

      IF ( ln_t3d .OR. ln_s3d ) THEN

         ! Set the number of variables for profiles to 2 (T and S)
         nprofvars = 2
         ! Set the number of extra variables for profiles to 1 (insitu temp).
         nprofextr = 1

         ! Count how may insitu data sets we have and allocate data.
         jprofset = 0
         IF ( ln_ena ) jprofset = jprofset + 1
         IF ( ln_cor ) jprofset = jprofset + 1
         IF ( ln_profb ) jprofset = jprofset + jnumprofb
         nprofsets = jprofset
         IF ( nprofsets > 0 ) THEN
            ALLOCATE(ld_enact(nprofsets))
            ALLOCATE(profdata(nprofsets))
            ALLOCATE(prodatqc(nprofsets))
         ENDIF

         jprofset = 0
          
         ! ENACT insitu data

         IF ( ln_ena ) THEN

            jprofset = jprofset + 1
            
            ld_enact(jprofset) = .TRUE.

            CALL obs_rea_pro_dri( 1, profdata(jprofset),          &
               &                  jnumenact, enactfiles(1:jnumenact), &
               &                  nprofvars, nprofextr,        &
               &                  nitend-nit000+2,             &
               &                  dobsini, dobsend, ln_t3d, ln_s3d, &
               &                  ln_ignmis, ln_s_at_t, .TRUE., .FALSE., &
               &                  kdailyavtypes = endailyavtypes )

            DO jvar = 1, 2

               CALL obs_prof_staend( profdata(jprofset), jvar )

            END DO

            CALL obs_pre_pro( profdata(jprofset), prodatqc(jprofset),   &
               &              ln_t3d, ln_s3d, ln_nea, &
               &              kdailyavtypes=endailyavtypes )
            
         ENDIF

         ! Coriolis insitu data

         IF ( ln_cor ) THEN
           
            jprofset = jprofset + 1

            ld_enact(jprofset) = .FALSE.

            CALL obs_rea_pro_dri( 2, profdata(jprofset),          &
               &                  jnumcorio, coriofiles(1:jnumcorio), &
               &                  nprofvars, nprofextr,        &
               &                  nitend-nit000+2,             &
               &                  dobsini, dobsend, ln_t3d, ln_s3d, &
               &                  ln_ignmis, ln_s_at_t, .FALSE., .FALSE. )

            DO jvar = 1, 2

               CALL obs_prof_staend( profdata(jprofset), jvar )

            END DO

            CALL obs_pre_pro( profdata(jprofset), prodatqc(jprofset),   &
                 &            ln_t3d, ln_s3d, ln_nea )
            
         ENDIF
 
         ! Feedback insitu data

         IF ( ln_profb ) THEN
           
            DO jset = 1, jnumprofb
               
               jprofset = jprofset + 1
               ld_enact (jprofset) = ln_profb_ena(jset)

               CALL obs_rea_pro_dri( 0, profdata(jprofset),          &
                  &                  1, profbfiles(jset:jset), &
                  &                  nprofvars, nprofextr,        &
                  &                  nitend-nit000+2,             &
                  &                  dobsini, dobsend, ln_t3d, ln_s3d, &
                  &                  ln_ignmis, ln_s_at_t, &
                  &                  ld_enact(jprofset).AND.&
                  &                  ln_profb_enatim(jset), &
                  &                  .FALSE., kdailyavtypes = endailyavtypes )
               
               DO jvar = 1, 2
                  
                  CALL obs_prof_staend( profdata(jprofset), jvar )
                  
               END DO
               
               IF ( ld_enact(jprofset) ) THEN
                  CALL obs_pre_pro( profdata(jprofset), prodatqc(jprofset),   &
                     &              ln_t3d, ln_s3d, ln_nea, &
                     &              kdailyavtypes = endailyavtypes )
               ELSE
                  CALL obs_pre_pro( profdata(jprofset), prodatqc(jprofset),   &
                     &              ln_t3d, ln_s3d, ln_nea )
               ENDIF
               
            END DO

         ENDIF

      ENDIF

      !  - Sea level anomalies
      IF ( ln_sla ) THEN
        ! Set the number of variables for sla to 1
         nslavars = 1

         ! Set the number of extra variables for sla to 2
         nslaextr = 2
         
         ! Set the number of sla data sets to 2
         nslasets = 0
         IF ( ln_sladt ) THEN
            nslasets = nslasets + 2
         ENDIF
         IF ( ln_slafb ) THEN
            nslasets = nslasets + jnumslafb
         ENDIF
         
         ALLOCATE(sladata(nslasets))
         ALLOCATE(sladatqc(nslasets))
         sladata(:)%nsurf=0
         sladatqc(:)%nsurf=0

         nslasets = 0

         ! AVISO SLA data

         IF ( ln_sladt ) THEN

            ! Active SLA observations
            
            nslasets = nslasets + 1
            
            CALL obs_rea_sla( 1, sladata(nslasets), jnumslaact, &
               &              slafilesact(1:jnumslaact), &
               &              nslavars, nslaextr, nitend-nit000+2, &
               &              dobsini, dobsend, ln_ignmis, .FALSE. )
            CALL obs_pre_sla( sladata(nslasets), sladatqc(nslasets), &
               &              ln_sla, ln_nea )
            
            ! Passive SLA observations
            
            nslasets = nslasets + 1
            
            CALL obs_rea_sla( 1, sladata(nslasets), jnumslapas, &
               &              slafilespas(1:jnumslapas), &
               &              nslavars, nslaextr, nitend-nit000+2, &
               &              dobsini, dobsend, ln_ignmis, .FALSE. )
            
            CALL obs_pre_sla( sladata(nslasets), sladatqc(nslasets), &
               &              ln_sla, ln_nea )

         ENDIF
         
         ! Feedback SLA data

         IF ( ln_slafb ) THEN

            DO jset = 1, jnumslafb
            
               nslasets = nslasets + 1
            
               CALL obs_rea_sla( 0, sladata(nslasets), 1, &
                  &              slafbfiles(jset:jset), &
                  &              nslavars, nslaextr, nitend-nit000+2, &
                  &              dobsini, dobsend, ln_ignmis, .FALSE. )
               CALL obs_pre_sla( sladata(nslasets), sladatqc(nslasets), &
                  &              ln_sla, ln_nea )

            END DO               

         ENDIF
         
         CALL obs_rea_mdt( nslasets, sladatqc, n2dint )
            
         ! read in altimeter bias
         
         IF ( ln_altbias ) THEN     
            CALL obs_rea_altbias ( nslasets, sladatqc, n2dint, bias_file )
         ENDIF
     
      ENDIF

      !  - Sea surface height
      IF ( ln_ssh ) THEN
         IF(lwp) WRITE(numout,*) ' SSH currently not available'
      ENDIF

      !  - Sea surface temperature
      IF ( ln_sst ) THEN

         ! Set the number of variables for sst to 1
         nsstvars = 1

         ! Set the number of extra variables for sst to 0
         nsstextr = 0

         nsstsets = 0

         IF (ln_reysst) nsstsets = nsstsets + 1
         IF (ln_ghrsst) nsstsets = nsstsets + 1
         IF ( ln_sstfb ) THEN
            nsstsets = nsstsets + jnumsstfb
         ENDIF

         ALLOCATE(sstdata(nsstsets))
         ALLOCATE(sstdatqc(nsstsets))
         sstdata(:)%nsurf=0
         sstdatqc(:)%nsurf=0         

         nsstsets = 0

         IF (ln_reysst) THEN

            nsstsets = nsstsets + 1

            CALL obs_rea_sst_rey( reysstname, reysstfmt, sstdata(nsstsets), &
               &                  nsstvars, nsstextr, &
               &                  nitend-nit000+2, dobsini, dobsend )
            CALL obs_pre_sst( sstdata(nsstsets), sstdatqc(nsstsets), ln_sst, &
               &              ln_nea )

        ENDIF
        
        IF (ln_ghrsst) THEN
        
            nsstsets = nsstsets + 1
          
            CALL obs_rea_sst( 1, sstdata(nsstsets), jnumsst, &
               &              sstfiles(1:jnumsst), &
               &              nsstvars, nsstextr, nitend-nit000+2, &
               &              dobsini, dobsend, ln_ignmis, .FALSE. )
            CALL obs_pre_sst( sstdata(nsstsets), sstdatqc(nsstsets), ln_sst, &
               &              ln_nea )

        ENDIF
               
         ! Feedback SST data

         IF ( ln_sstfb ) THEN

            DO jset = 1, jnumsstfb
            
               nsstsets = nsstsets + 1
            
               CALL obs_rea_sst( 0, sstdata(nsstsets), 1, &
                  &              sstfbfiles(jset:jset), &
                  &              nsstvars, nsstextr, nitend-nit000+2, &
                  &              dobsini, dobsend, ln_ignmis, .FALSE. )
               CALL obs_pre_sst( sstdata(nsstsets), sstdatqc(nsstsets), &
                  &              ln_sst, ln_nea )

            END DO               

         ENDIF

      ENDIF

      !  - Sea surface salinity
      IF ( ln_sss ) THEN
         IF(lwp) WRITE(numout,*) ' SSS currently not available'
      ENDIF

      !  - Sea Ice Concentration
      
      IF ( ln_seaice ) THEN

         ! Set the number of variables for seaice to 1
         nseaicevars = 1

         ! Set the number of extra variables for seaice to 0
         nseaiceextr = 0
         
         ! Set the number of data sets to 1
         nseaicesets = 1

         ALLOCATE(seaicedata(nseaicesets))
         ALLOCATE(seaicedatqc(nseaicesets))
         seaicedata(:)%nsurf=0
         seaicedatqc(:)%nsurf=0

         CALL obs_rea_seaice( 1, seaicedata(nseaicesets), jnumseaice, &
            &                 seaicefiles(1:jnumseaice), &
            &                 nseaicevars, nseaiceextr, nitend-nit000+2, &
            &                 dobsini, dobsend, ln_ignmis, .FALSE. )

         CALL obs_pre_seaice( seaicedata(nseaicesets), seaicedatqc(nseaicesets), &
            &                 ln_seaice, ln_nea )
 
      ENDIF

      IF (ln_vel3d) THEN

         ! Set the number of variables for profiles to 2 (U and V)
         nvelovars = 2

         ! Set the number of extra variables for profiles to 2 to store 
         ! rotation parameters
         nveloextr = 2

         jveloset = 0
         
         IF ( ln_velavcur ) jveloset = jveloset + 1
         IF ( ln_velhrcur ) jveloset = jveloset + 1
         IF ( ln_velavadcp ) jveloset = jveloset + 1
         IF ( ln_velhradcp ) jveloset = jveloset + 1
         IF (ln_velfb) jveloset = jveloset + jnumvelfb

         nvelosets = jveloset
         IF ( nvelosets > 0 ) THEN
            ALLOCATE( velodata(nvelosets) )
            ALLOCATE( veldatqc(nvelosets) )
            ALLOCATE( ld_velav(nvelosets) )
         ENDIF
         
         jveloset = 0
         
         ! Daily averaged data

         IF ( ln_velavcur ) THEN
            
            jveloset = jveloset + 1
            
            ld_velav(jveloset) = .TRUE.
            
            CALL obs_rea_vel_dri( 1, velodata(jveloset), jnumvelavcur, &
               &                  velavcurfiles(1:jnumvelavcur), &
               &                  nvelovars, nveloextr, &
               &                  nitend-nit000+2,              &
               &                  dobsini, dobsend, ln_ignmis, &
               &                  ld_velav(jveloset), &
               &                  .FALSE. )
            
            DO jvar = 1, 2
               CALL obs_prof_staend( velodata(jveloset), jvar )
            END DO
            
            CALL obs_pre_vel( velodata(jveloset), veldatqc(jveloset), &
               &              ln_vel3d, ln_nea, ld_velav(jveloset) )
            
         ENDIF

         ! High frequency data

         IF ( ln_velhrcur ) THEN
            
            jveloset = jveloset + 1
            
            ld_velav(jveloset) = .FALSE.
               
            CALL obs_rea_vel_dri( 1, velodata(jveloset), jnumvelhrcur, &
               &                  velhrcurfiles(1:jnumvelhrcur), &
               &                  nvelovars, nveloextr, &
               &                  nitend-nit000+2,              &
               &                  dobsini, dobsend, ln_ignmis, &
               &                  ld_velav(jveloset), &
               &                  .FALSE. )
            
            DO jvar = 1, 2
               CALL obs_prof_staend( velodata(jveloset), jvar )
            END DO
            
            CALL obs_pre_vel( velodata(jveloset), veldatqc(jveloset), &
               &              ln_vel3d, ln_nea, ld_velav(jveloset) )
            
         ENDIF

         ! Daily averaged data

         IF ( ln_velavadcp ) THEN
            
            jveloset = jveloset + 1
            
            ld_velav(jveloset) = .TRUE.
            
            CALL obs_rea_vel_dri( 1, velodata(jveloset), jnumvelavadcp, &
               &                  velavadcpfiles(1:jnumvelavadcp), &
               &                  nvelovars, nveloextr, &
               &                  nitend-nit000+2,              &
               &                  dobsini, dobsend, ln_ignmis, &
               &                  ld_velav(jveloset), &
               &                  .FALSE. )
            
            DO jvar = 1, 2
               CALL obs_prof_staend( velodata(jveloset), jvar )
            END DO
            
            CALL obs_pre_vel( velodata(jveloset), veldatqc(jveloset), &
               &              ln_vel3d, ln_nea, ld_velav(jveloset) )
            
         ENDIF

         ! High frequency data

         IF ( ln_velhradcp ) THEN
            
            jveloset = jveloset + 1
            
            ld_velav(jveloset) = .FALSE.
               
            CALL obs_rea_vel_dri( 1, velodata(jveloset), jnumvelhradcp, &
               &                  velhradcpfiles(1:jnumvelhradcp), &
               &                  nvelovars, nveloextr, &
               &                  nitend-nit000+2,              &
               &                  dobsini, dobsend, ln_ignmis, &
               &                  ld_velav(jveloset), &
               &                  .FALSE. )
            
            DO jvar = 1, 2
               CALL obs_prof_staend( velodata(jveloset), jvar )
            END DO
            
            CALL obs_pre_vel( velodata(jveloset), veldatqc(jveloset), &
               &              ln_vel3d, ln_nea, ld_velav(jveloset) )
            
         ENDIF

         IF ( ln_velfb ) THEN

            DO jset = 1, jnumvelfb
            
               jveloset = jveloset + 1

               ld_velav(jveloset) = ln_velfb_av(jset)
               
               CALL obs_rea_vel_dri( 0, velodata(jveloset), 1, &
                  &                  velfbfiles(jset:jset), &
                  &                  nvelovars, nveloextr, &
                  &                  nitend-nit000+2,              &
                  &                  dobsini, dobsend, ln_ignmis, &
                  &                  ld_velav(jveloset), &
                  &                  .FALSE. )
               
               DO jvar = 1, 2
                  CALL obs_prof_staend( velodata(jveloset), jvar )
               END DO
               
               CALL obs_pre_vel( velodata(jveloset), veldatqc(jveloset), &
                  &              ln_vel3d, ln_nea, ld_velav(jveloset) )


            END DO
            
         ENDIF

      ENDIF
     
   END SUBROUTINE dia_obs_init

   SUBROUTINE dia_obs( kstp )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_obs  ***
      !!          
      !! ** Purpose : Call the observation operators on each time step
      !!
      !! ** Method  : Call the observation operators on each time step to
      !!              compute the model equivalent of the following date:
      !!               - T profiles
      !!               - S profiles
      !!               - Sea surface height (referenced to a mean)
      !!               - Sea surface temperature
      !!               - Sea surface salinity
      !!               - Velocity component (U,V) profiles
      !!
      !! ** Action  : 
      !!
      !! History :
      !!        !  06-03  (K. Mogensen) Original code
      !!        !  06-05  (K. Mogensen) Reformatted
      !!        !  06-10  (A. Weaver) Cleaning
      !!        !  07-03  (K. Mogensen) General handling of profiles
      !!        !  07-04  (G. Smith) Generalized surface operators
      !!        !  08-10  (M. Valdivieso) obs operator for velocity profiles
      !!----------------------------------------------------------------------
      !! * Modules used
      USE dom_oce, ONLY : &             ! Ocean space and time domain variables
         & rdt,           &                       
         & gdept_0,       &             
         & tmask, umask, vmask                            
      USE phycst, ONLY : &              ! Physical constants
         & rday                         
      USE oce, ONLY : &                 ! Ocean dynamics and tracers variables
         & tsn,  &             
         & un, vn,  &
         & sshn
#if defined  key_lim3
      USE ice, ONLY : &                     ! LIM Ice model variables
         & frld
#endif
#if defined key_lim2
      USE ice_2, ONLY : &                     ! LIM Ice model variables
         & frld
#endif
      IMPLICIT NONE

      !! * Arguments
      INTEGER, INTENT(IN) :: kstp                         ! Current timestep
      !! * Local declarations
      INTEGER :: idaystp                ! Number of timesteps per day
      INTEGER :: jprofset               ! Profile data set loop variable
      INTEGER :: jslaset                ! SLA data set loop variable
      INTEGER :: jsstset                ! SST data set loop variable
      INTEGER :: jseaiceset             ! sea ice data set loop variable
      INTEGER :: jveloset               ! velocity profile data loop variable
      INTEGER :: jvar                   ! Variable number    
#if ! defined key_lim2 && ! defined key_lim3
      REAL(wp), POINTER, DIMENSION(:,:) :: frld   
#endif
      CHARACTER(LEN=20) :: datestr=" ",timestr=" "
 
#if ! defined key_lim2 && ! defined key_lim3
      CALL wrk_alloc(jpi,jpj,frld) 
#endif

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dia_obs : Call the observation operators', kstp
         WRITE(numout,*) '~~~~~~~'
      ENDIF

      idaystp = NINT( rday / rdt )

      !-----------------------------------------------------------------------
      ! No LIM => frld == 0.0_wp
      !-----------------------------------------------------------------------
#if ! defined key_lim2 && ! defined key_lim3
      frld(:,:) = 0.0_wp
#endif
      !-----------------------------------------------------------------------
      ! Depending on switches call various observation operators
      !-----------------------------------------------------------------------

      !  - Temperature/salinity profiles
      IF ( ln_t3d .OR. ln_s3d ) THEN
         DO jprofset = 1, nprofsets
            IF ( ld_enact(jprofset) ) THEN
               CALL obs_pro_opt( prodatqc(jprofset),                     &
                  &              kstp, jpi, jpj, jpk, nit000, idaystp,   &
                  &              tsn(:,:,:,jp_tem), tsn(:,:,:,jp_sal),   &
                  &              gdept_0, tmask, n1dint, n2dint,         &
                  &              kdailyavtypes = endailyavtypes )
            ELSE
               CALL obs_pro_opt( prodatqc(jprofset),                     &
                  &              kstp, jpi, jpj, jpk, nit000, idaystp,   &
                  &              tsn(:,:,:,jp_tem), tsn(:,:,:,jp_sal),   &
                  &              gdept_0, tmask, n1dint, n2dint               )
            ENDIF
         END DO
      ENDIF

      !  - Sea surface anomaly
      IF ( ln_sla ) THEN
         DO jslaset = 1, nslasets
            CALL obs_sla_opt( sladatqc(jslaset),            &
               &              kstp, jpi, jpj, nit000, sshn, &
               &              tmask(:,:,1), n2dint )
         END DO         
      ENDIF

      !  - Sea surface temperature
      IF ( ln_sst ) THEN
         DO jsstset = 1, nsstsets
            CALL obs_sst_opt( sstdatqc(jsstset),                 &
               &              kstp, jpi, jpj, nit000, tsn(:,:,1,jp_tem), &
               &              tmask(:,:,1), n2dint )
         END DO
      ENDIF

      !  - Sea surface salinity
      IF ( ln_sss ) THEN
         IF(lwp) WRITE(numout,*) ' SSS currently not available'
      ENDIF

#if defined key_lim2 || defined key_lim3
      IF ( ln_seaice ) THEN
         DO jseaiceset = 1, nseaicesets
            CALL obs_seaice_opt( seaicedatqc(jseaiceset),      &
               &              kstp, jpi, jpj, nit000, 1.-frld, &
               &              tmask(:,:,1), n2dint )
         END DO
      ENDIF      
#endif

      !  - Velocity profiles
      IF ( ln_vel3d ) THEN
         DO jveloset = 1, nvelosets
           ! zonal component of velocity
           CALL obs_vel_opt( veldatqc(jveloset), kstp, jpi, jpj, jpk, &
              &              nit000, idaystp, un, vn, gdept_0, umask, vmask, &
                             n1dint, n2dint, ld_velav(jveloset) )
         END DO
      ENDIF

#if ! defined key_lim2 && ! defined key_lim3
      CALL wrk_dealloc(jpi,jpj,frld) 
#endif

   END SUBROUTINE dia_obs
  
   SUBROUTINE dia_obs_wri 
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_obs_wri  ***
      !!          
      !! ** Purpose : Call observation diagnostic output routines
      !!
      !! ** Method  : Call observation diagnostic output routines
      !!
      !! ** Action  : 
      !!
      !! History :
      !!        !  06-03  (K. Mogensen) Original code
      !!        !  06-05  (K. Mogensen) Reformatted
      !!        !  06-10  (A. Weaver) Cleaning
      !!        !  07-03  (K. Mogensen) General handling of profiles
      !!        !  08-09  (M. Valdivieso) Velocity component (U,V) profiles
      !!----------------------------------------------------------------------
      IMPLICIT NONE

      !! * Local declarations

      INTEGER :: jprofset                 ! Profile data set loop variable
      INTEGER :: jveloset                 ! Velocity data set loop variable
      INTEGER :: jslaset                  ! SLA data set loop variable
      INTEGER :: jsstset                  ! SST data set loop variable
      INTEGER :: jseaiceset               ! Sea Ice data set loop variable
      INTEGER :: jset
      INTEGER :: jfbini
      CHARACTER(LEN=20) :: datestr=" ",timestr=" "
      CHARACTER(LEN=10) :: cdtmp
      !-----------------------------------------------------------------------
      ! Depending on switches call various observation output routines
      !-----------------------------------------------------------------------

      !  - Temperature/salinity profiles

      IF( ln_t3d .OR. ln_s3d ) THEN

         ! Copy data from prodatqc to profdata structures
         DO jprofset = 1, nprofsets

            CALL obs_prof_decompress( prodatqc(jprofset), &
                 &                    profdata(jprofset), .TRUE., numout )

         END DO

         ! Write the profiles.

         jprofset = 0

         ! ENACT insitu data

         IF ( ln_ena ) THEN
           
            jprofset = jprofset + 1

            CALL obs_wri_p3d( 'enact', profdata(jprofset) )

         ENDIF

         ! Coriolis insitu data

         IF ( ln_cor ) THEN
            
            jprofset = jprofset + 1

            CALL obs_wri_p3d( 'corio', profdata(jprofset) )
            
         ENDIF
         
         ! Feedback insitu data

         IF ( ln_profb ) THEN

            jfbini = jprofset + 1

            DO jprofset = jfbini, nprofsets
               
               jset = jprofset - jfbini + 1
               WRITE(cdtmp,'(A,I2.2)')'profb_',jset
               CALL obs_wri_p3d( cdtmp, profdata(jprofset) )

            END DO

         ENDIF

      ENDIF

      !  - Sea surface anomaly
      IF ( ln_sla ) THEN

         ! Copy data from sladatqc to sladata structures
         DO jslaset = 1, nslasets

              CALL obs_surf_decompress( sladatqc(jslaset), &
                 &                    sladata(jslaset), .TRUE., numout )

         END DO

         jslaset = 0 

         ! Write the AVISO SLA data

         IF ( ln_sladt ) THEN
            
            jslaset = 1
            CALL obs_wri_sla( 'aviso_act', sladata(jslaset) )
            jslaset = 2
            CALL obs_wri_sla( 'aviso_pas', sladata(jslaset) )

         ENDIF

         IF ( ln_slafb ) THEN
            
            jfbini = jslaset + 1

            DO jslaset = jfbini, nslasets
               
               jset = jslaset - jfbini + 1
               WRITE(cdtmp,'(A,I2.2)')'slafb_',jset
               CALL obs_wri_sla( cdtmp, sladata(jslaset) )

            END DO

         ENDIF

      ENDIF

      !  - Sea surface temperature
      IF ( ln_sst ) THEN

         ! Copy data from sstdatqc to sstdata structures
         DO jsstset = 1, nsstsets
     
              CALL obs_surf_decompress( sstdatqc(jsstset), &
                 &                    sstdata(jsstset), .TRUE., numout )

         END DO

         jsstset = 0 

         ! Write the AVISO SST data

         IF ( ln_reysst ) THEN
            
            jsstset = jsstset + 1
            CALL obs_wri_sst( 'reynolds', sstdata(jsstset) )

         ENDIF

         IF ( ln_ghrsst ) THEN
            
            jsstset = jsstset + 1
            CALL obs_wri_sst( 'ghr', sstdata(jsstset) )

         ENDIF

         IF ( ln_sstfb ) THEN
            
            jfbini = jsstset + 1

            DO jsstset = jfbini, nsstsets
               
               jset = jsstset - jfbini + 1
               WRITE(cdtmp,'(A,I2.2)')'sstfb_',jset
               CALL obs_wri_sst( cdtmp, sstdata(jsstset) )

            END DO

         ENDIF

      ENDIF

      !  - Sea surface salinity
      IF ( ln_sss ) THEN
         IF(lwp) WRITE(numout,*) ' SSS currently not available'
      ENDIF

      !  - Sea Ice Concentration
      IF ( ln_seaice ) THEN

         ! Copy data from seaicedatqc to seaicedata structures
         DO jseaiceset = 1, nseaicesets

              CALL obs_surf_decompress( seaicedatqc(jseaiceset), &
                 &                    seaicedata(jseaiceset), .TRUE., numout )

         END DO

         ! Write the Sea Ice data
         DO jseaiceset = 1, nseaicesets
      
            WRITE(cdtmp,'(A,I2.2)')'seaicefb_',jseaiceset
            CALL obs_wri_seaice( cdtmp, seaicedata(jseaiceset) )

         END DO

      ENDIF
      
      ! Velocity data
      IF( ln_vel3d ) THEN

         ! Copy data from veldatqc to velodata structures
         DO jveloset = 1, nvelosets

            CALL obs_prof_decompress( veldatqc(jveloset), &
                 &                    velodata(jveloset), .TRUE., numout )

         END DO

         ! Write the profiles.

         jveloset = 0

         ! Daily averaged data

         IF ( ln_velavcur ) THEN
            
            jveloset = jveloset + 1

            CALL obs_wri_vel( 'velavcurr', velodata(jveloset), n2dint )

         ENDIF

         ! High frequency data

         IF ( ln_velhrcur ) THEN
            
            jveloset = jveloset + 1

            CALL obs_wri_vel( 'velhrcurr', velodata(jveloset), n2dint )

         ENDIF

         ! Daily averaged data

         IF ( ln_velavadcp ) THEN
            
            jveloset = jveloset + 1

            CALL obs_wri_vel( 'velavadcp', velodata(jveloset), n2dint )

         ENDIF

         ! High frequency data

         IF ( ln_velhradcp ) THEN
            
            jveloset = jveloset + 1
            
            CALL obs_wri_vel( 'velhradcp', velodata(jveloset), n2dint )
               
         ENDIF

         ! Feedback velocity data

         IF ( ln_velfb ) THEN

            jfbini = jveloset + 1

            DO jveloset = jfbini, nvelosets
               
               jset = jveloset - jfbini + 1
               WRITE(cdtmp,'(A,I2.2)')'velfb_',jset
               CALL obs_wri_vel( cdtmp, velodata(jveloset), n2dint )

            END DO

         ENDIF
         
      ENDIF

   END SUBROUTINE dia_obs_wri

   SUBROUTINE ini_date( ddobsini )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ini_date  ***
      !!          
      !! ** Purpose : Get initial data in double precision YYYYMMDD.HHMMSS format
      !!
      !! ** Method  : Get initial data in double precision YYYYMMDD.HHMMSS format
      !!
      !! ** Action  : Get initial data in double precision YYYYMMDD.HHMMSS format
      !!
      !! History :
      !!        !  06-03  (K. Mogensen)  Original code
      !!        !  06-05  (K. Mogensen)  Reformatted
      !!        !  06-10  (A. Weaver) Cleaning
      !!        !  06-10  (G. Smith) Calculates initial date the same as method for final date
      !!        !  10-05  (D. Lea) Update to month length calculation for NEMO vn3.2
      !!----------------------------------------------------------------------
      USE phycst, ONLY : &            ! Physical constants
         & rday
!      USE daymod, ONLY : &            ! Time variables
!         & nmonth_len           
      USE dom_oce, ONLY : &           ! Ocean space and time domain variables
         & rdt

      IMPLICIT NONE

      !! * Arguments
      REAL(KIND=dp), INTENT(OUT) :: ddobsini                         ! Initial date in YYYYMMDD.HHMMSS

      !! * Local declarations
      INTEGER :: iyea        ! date - (year, month, day, hour, minute)
      INTEGER :: imon
      INTEGER :: iday
      INTEGER :: ihou
      INTEGER :: imin
      INTEGER :: imday         ! Number of days in month.
      REAL(KIND=wp) :: zdayfrc ! Fraction of day

      INTEGER, DIMENSION(12) ::   imonth_len    !: length in days of the months of the current year

      !!----------------------------------------------------------------------
      !! Initial date initialization (year, month, day, hour, minute)
      !! (This assumes that the initial date is for 00z))
      !!----------------------------------------------------------------------
      iyea =   ndate0 / 10000
      imon = ( ndate0 - iyea * 10000 ) / 100
      iday =   ndate0 - iyea * 10000 - imon * 100
      ihou = 0
      imin = 0

      !!----------------------------------------------------------------------
      !! Compute number of days + number of hours + min since initial time
      !!----------------------------------------------------------------------
      iday = iday + ( nit000 -1 ) * rdt / rday
      zdayfrc = ( nit000 -1 ) * rdt / rday
      zdayfrc = zdayfrc - aint(zdayfrc)
      ihou = int( zdayfrc * 24 )
      imin = int( (zdayfrc * 24 - ihou) * 60 )

      !!-----------------------------------------------------------------------
      !! Convert number of days (iday) into a real date
      !!----------------------------------------------------------------------

      CALL calc_month_len( iyea, imonth_len )
      
      DO WHILE ( iday > imonth_len(imon) )
         iday = iday - imonth_len(imon)
         imon = imon + 1 
         IF ( imon > 12 ) THEN
            imon = 1
            iyea = iyea + 1
            CALL calc_month_len( iyea, imonth_len )  ! update month lengths
         ENDIF
      END DO

      !!----------------------------------------------------------------------
      !! Convert it into YYYYMMDD.HHMMSS format.
      !!----------------------------------------------------------------------
      ddobsini = iyea * 10000_dp + imon * 100_dp + &
         &       iday + ihou * 0.01_dp + imin * 0.0001_dp


   END SUBROUTINE ini_date

   SUBROUTINE fin_date( ddobsfin )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE fin_date  ***
      !!          
      !! ** Purpose : Get final data in double precision YYYYMMDD.HHMMSS format
      !!
      !! ** Method  : Get final data in double precision YYYYMMDD.HHMMSS format
      !!
      !! ** Action  : Get final data in double precision YYYYMMDD.HHMMSS format
      !!
      !! History :
      !!        !  06-03  (K. Mogensen)  Original code
      !!        !  06-05  (K. Mogensen)  Reformatted
      !!        !  06-10  (A. Weaver) Cleaning
      !!        !  10-05  (D. Lea) Update to month length calculation for NEMO vn3.2
      !!----------------------------------------------------------------------
      USE phycst, ONLY : &            ! Physical constants
         & rday
!      USE daymod, ONLY : &            ! Time variables
!         & nmonth_len                
      USE dom_oce, ONLY : &           ! Ocean space and time domain variables
         & rdt

      IMPLICIT NONE

      !! * Arguments
      REAL(KIND=dp), INTENT(OUT) :: ddobsfin                   ! Final date in YYYYMMDD.HHMMSS

      !! * Local declarations
      INTEGER :: iyea        ! date - (year, month, day, hour, minute)
      INTEGER :: imon
      INTEGER :: iday
      INTEGER :: ihou
      INTEGER :: imin
      INTEGER :: imday         ! Number of days in month.
      REAL(KIND=wp) :: zdayfrc       ! Fraction of day
         
      INTEGER, DIMENSION(12) ::   imonth_len    !: length in days of the months of the current year
            
      !-----------------------------------------------------------------------
      ! Initial date initialization (year, month, day, hour, minute)
      ! (This assumes that the initial date is for 00z)
      !-----------------------------------------------------------------------
      iyea =   ndate0 / 10000
      imon = ( ndate0 - iyea * 10000 ) / 100
      iday =   ndate0 - iyea * 10000 - imon * 100
      ihou = 0
      imin = 0
      
      !-----------------------------------------------------------------------
      ! Compute number of days + number of hours + min since initial time
      !-----------------------------------------------------------------------
      iday    = iday +  nitend  * rdt / rday
      zdayfrc =  nitend  * rdt / rday
      zdayfrc = zdayfrc - AINT( zdayfrc )
      ihou    = INT( zdayfrc * 24 )
      imin    = INT( ( zdayfrc * 24 - ihou ) * 60 )

      !-----------------------------------------------------------------------
      ! Convert number of days (iday) into a real date
      !----------------------------------------------------------------------

      CALL calc_month_len( iyea, imonth_len )
      
      DO WHILE ( iday > imonth_len(imon) )
         iday = iday - imonth_len(imon)
         imon = imon + 1 
         IF ( imon > 12 ) THEN
            imon = 1
            iyea = iyea + 1
            CALL calc_month_len( iyea, imonth_len )  ! update month lengths
         ENDIF
      END DO

      !-----------------------------------------------------------------------
      ! Convert it into YYYYMMDD.HHMMSS format
      !-----------------------------------------------------------------------
      ddobsfin = iyea * 10000_dp + imon * 100_dp    + iday &
         &     + ihou * 0.01_dp  + imin * 0.0001_dp

    END SUBROUTINE fin_date
    
END MODULE diaobs
