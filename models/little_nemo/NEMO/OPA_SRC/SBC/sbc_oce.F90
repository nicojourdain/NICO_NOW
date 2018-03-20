MODULE sbc_oce
   !!======================================================================
   !!                       ***  MODULE  sbc_oce  ***
   !! Surface module :   variables defined in core memory 
   !!======================================================================
   !! History :  3.0  ! 2006-06  (G. Madec)  Original code
   !!             -   ! 2008-08  (G. Madec)  namsbc moved from sbcmod
   !!            3.3  ! 2010-04  (M. Leclair, G. Madec)  Forcing averaged over 2 time steps
   !!             -   ! 2010-11  (G. Madec) ice-ocean stress always computed at each ocean time-step
   !!            3.3  ! 2010-10  (J. Chanut, C. Bricaud)  add the surface pressure forcing
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_oce_alloc : allocation of sbc arrays
   !!   sbc_tau2wnd   : wind speed estimated from wind stress
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_oce_alloc   ! routine called in sbcmod.F90
   PUBLIC   sbc_tau2wnd     ! routine called in several sbc modules
   
   !!----------------------------------------------------------------------
   !!           Namelist for the Ocean Surface Boundary Condition
   !!----------------------------------------------------------------------
   !                                            !!* namsbc namelist *
   LOGICAL , PUBLIC ::   ln_ana      = .FALSE.   !: analytical boundary condition flag
   LOGICAL , PUBLIC ::   ln_flx      = .FALSE.   !: flux      formulation
   LOGICAL , PUBLIC ::   ln_blk_clio = .FALSE.   !: CLIO bulk formulation
   LOGICAL , PUBLIC ::   ln_blk_core = .FALSE.   !: CORE bulk formulation
   LOGICAL , PUBLIC ::   ln_blk_mfs  = .FALSE.   !: MFS  bulk formulation
   LOGICAL , PUBLIC ::   ln_cpl      = .FALSE.   !: coupled   formulation (overwritten by key_sbc_coupled )
   LOGICAL , PUBLIC ::   ln_dm2dc    = .FALSE.   !: Daily mean to Diurnal Cycle short wave (qsr)
   LOGICAL , PUBLIC ::   ln_rnf      = .FALSE.   !: runoffs / runoff mouths
   LOGICAL , PUBLIC ::   ln_ssr      = .FALSE.   !: Sea Surface restoring on SST and/or SSS      
   LOGICAL , PUBLIC ::   ln_tau      = .FALSE.   !: Overwrite utau and vtau     
   LOGICAL , PUBLIC ::   ln_apr_dyn  = .FALSE.   !: Atmospheric pressure forcing used on dynamics (ocean & ice)
   INTEGER , PUBLIC ::   nn_ice      = 0         !: flag on ice in the surface boundary condition (=0/1/2/3)
   INTEGER , PUBLIC ::   nn_fwb      = 0         !: FreshWater Budget: 
   !                                             !:  = 0 unchecked 
   !                                             !:  = 1 global mean of e-p-r set to zero at each nn_fsbc time step
   !                                             !:  = 2 annual global mean of e-p-r set to zero
   LOGICAL , PUBLIC ::   ln_cdgw     = .FALSE.   !: true if neutral drag coefficient read from wave model

   !!----------------------------------------------------------------------
   !!              Ocean Surface Boundary Condition fields
   !!----------------------------------------------------------------------
   LOGICAL , PUBLIC ::   lhftau = .FALSE.        !: HF tau used in TKE: mean(stress module) - module(mean stress)
   !!                                   !!   now    ! before   !!
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   utau   , utau_b   !: sea surface i-stress (ocean referential)     [N/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   vtau   , vtau_b   !: sea surface j-stress (ocean referential)     [N/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   taum              !: module of sea surface stress (at T-point)    [N/m2] 
   !! wndm is used onmpute surface gases exchanges in ice-free ocean or leads
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   wndm              !: wind speed module at T-point (=|U10m-Uoce|)  [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qsr               !: sea heat flux:     solar                     [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qns    , qns_b    !: sea heat flux: non solar                     [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qsr_tot           !: total     solar heat flux (over sea and ice) [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qns_tot           !: total non solar heat flux (over sea and ice) [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   emp    , emp_b    !: freshwater budget: volume flux               [Kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   emps   , emps_b   !: freshwater budget: concentration/dillution   [Kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   emp_tot           !: total E-P over ocean and ice                 [Kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   rnf    , rnf_b    !: river runoff   [Kg/m2/s]  
   !!
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  sbc_tsc, sbc_tsc_b  !: sbc content trend                      [K.m/s] jpi,jpj,jpts
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  qsr_hc , qsr_hc_b   !: heat content trend due to qsr flux     [K.m/s] jpi,jpj,jpk
   !!
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   tprecip           !: total precipitation                          [Kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sprecip           !: solid precipitation                          [Kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fr_i              !: ice fraction = 1 - lead fraction      (between 0 to 1)
#if defined key_cpl_carbon_cycle
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   atm_co2           !: atmospheric pCO2                             [ppm]
#endif

   !!----------------------------------------------------------------------
   !!                     Sea Surface Mean fields
   !!----------------------------------------------------------------------
   INTEGER , PUBLIC                     ::   nn_fsbc   !: frequency of sbc computation (as well as sea-ice model)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ssu_m     !: mean (nn_fsbc time-step) surface sea i-current (U-point) [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ssv_m     !: mean (nn_fsbc time-step) surface sea j-current (V-point) [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sst_m     !: mean (nn_fsbc time-step) surface sea temperature     [Celsius]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sss_m     !: mean (nn_fsbc time-step) surface sea salinity            [psu]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ssh_m     !: mean (nn_fsbc time-step) sea surface height                [m]

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: sbc_oce.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION sbc_oce_alloc()
      !!---------------------------------------------------------------------
      !!                  ***  FUNCTION sbc_oce_alloc  ***
      !!---------------------------------------------------------------------
      INTEGER :: ierr(4)
      !!---------------------------------------------------------------------
      ierr(:) = 0
      !
      ALLOCATE( utau(jpi,jpj) , utau_b(jpi,jpj) , taum(jpi,jpj) ,     &
         &      vtau(jpi,jpj) , vtau_b(jpi,jpj) , wndm(jpi,jpj) , STAT=ierr(1) ) 
         !
      ALLOCATE( qns_tot(jpi,jpj) , qns   (jpi,jpj) , qns_b(jpi,jpj),        &
         &      qsr_tot(jpi,jpj) , qsr   (jpi,jpj) ,                        &
         &      emp    (jpi,jpj) , emp_b (jpi,jpj) ,                        &
         &      emps   (jpi,jpj) , emps_b(jpi,jpj) , emp_tot(jpi,jpj) , STAT=ierr(2) )
         !
      ALLOCATE( rnf  (jpi,jpj) , sbc_tsc  (jpi,jpj,jpts) , qsr_hc  (jpi,jpj,jpk) ,     &
         &      rnf_b(jpi,jpj) , sbc_tsc_b(jpi,jpj,jpts) , qsr_hc_b(jpi,jpj,jpk) , STAT=ierr(3) )
         !
      ALLOCATE( tprecip(jpi,jpj) , sprecip(jpi,jpj) , fr_i(jpi,jpj) ,     &
#if defined key_cpl_carbon_cycle
         &      atm_co2(jpi,jpj) ,                                        &
#endif
         &      ssu_m  (jpi,jpj) , sst_m(jpi,jpj) ,                       &
         &      ssv_m  (jpi,jpj) , sss_m  (jpi,jpj), ssh_m(jpi,jpj) , STAT=ierr(4) )
         !
      sbc_oce_alloc = MAXVAL( ierr )
      IF( lk_mpp            )   CALL mpp_sum ( sbc_oce_alloc )
      IF( sbc_oce_alloc > 0 )   CALL ctl_warn('sbc_oce_alloc: allocation of arrays failed')
      !
   END FUNCTION sbc_oce_alloc


   SUBROUTINE sbc_tau2wnd
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE sbc_tau2wnd  ***
      !!                   
      !! ** Purpose : Estimation of wind speed as a function of wind stress   
      !!
      !! ** Method  : |tau|=rhoa*Cd*|U|^2
      !!---------------------------------------------------------------------
      USE dom_oce         ! ocean space and time domain
      USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
      REAL(wp) ::   zrhoa  = 1.22         ! Air density kg/m3
      REAL(wp) ::   zcdrag = 1.5e-3       ! drag coefficient
      REAL(wp) ::   ztx, zty, ztau, zcoef ! temporary variables
      INTEGER  ::   ji, jj                ! dummy indices
      !!---------------------------------------------------------------------
      zcoef = 0.5 / ( zrhoa * zcdrag ) 
!CDIR NOVERRCHK
      DO jj = 2, jpjm1
!CDIR NOVERRCHK
         DO ji = fs_2, fs_jpim1   ! vect. opt.
            ztx = utau(ji-1,jj  ) + utau(ji,jj) 
            zty = vtau(ji  ,jj-1) + vtau(ji,jj) 
            ztau = SQRT( ztx * ztx + zty * zty )
            wndm(ji,jj) = SQRT ( ztau * zcoef ) * tmask(ji,jj,1)
         END DO
      END DO
      CALL lbc_lnk( wndm(:,:) , 'T', 1. )
      !
   END SUBROUTINE sbc_tau2wnd

   !!======================================================================
END MODULE sbc_oce
