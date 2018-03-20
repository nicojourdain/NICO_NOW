MODULE ice_2
   !!======================================================================
   !!                        ***  MODULE ice  ***
   !! Sea Ice physics:  diagnostics variables of ice defined in memory
   !!=====================================================================
   !! History :  2.0  ! 2003-08  (C. Ethe)  F90: Free form and module
   !!            3.3  ! 2009-05  (G.Garric) addition of the lim2_evp cas
   !!            4.0  ! 2011-01  (A. R. Porter, STFC Daresbury) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim2
   !!----------------------------------------------------------------------
   !!   'key_lim2' :                                  LIM 2.0 sea-ice model
   !!----------------------------------------------------------------------
   USE par_ice_2      ! LIM sea-ice parameters

   IMPLICIT NONE
   PRIVATE
   
   PUBLIC    ice_alloc_2  !  Called in iceini_2.F90

   INTEGER , PUBLIC ::   numit     !: ice iteration index
   REAL(wp), PUBLIC ::   rdt_ice   !: ice time step

   !                                                                     !!* namicerun read in iceini  *
   CHARACTER(len=32)     , PUBLIC ::   cn_icerst_in  = "restart_ice_in"   !: suffix of ice restart name (input)
   CHARACTER(len=32)     , PUBLIC ::   cn_icerst_out = "restart_ice"      !: suffix of ice restart name (output)
   LOGICAL               , PUBLIC ::   ln_limdyn     = .TRUE.             !: flag for ice dynamics (T) or not (F)
   LOGICAL               , PUBLIC ::   ln_limdmp     = .FALSE.            !: Ice damping
   LOGICAL               , PUBLIC ::   ln_nicep      = .TRUE.             !: flag grid points output (T) or not (F)
   REAL(wp)              , PUBLIC ::   hsndif        = 0._wp              !: snow temp. computation (0) or not (9999)
   REAL(wp)              , PUBLIC ::   hicdif        = 0._wp              !: ice  temp. computation (0) or not (9999)
   REAL(wp), DIMENSION(2), PUBLIC ::   acrit = (/ 1.e-6_wp , 1.e-6_wp /)  !: minimum lead fraction in the 2 hemisphere
   
   !                                          !!* ice-dynamic namelist (namicedyn) *
   INTEGER , PUBLIC ::   nbiter =    1         !: number of sub-time steps for relaxation
   INTEGER , PUBLIC ::   nbitdr =  250         !: maximum number of iterations for relaxation
   INTEGER , PUBLIC ::   nevp   =  360         !: number of EVP subcycling iterations
   INTEGER , PUBLIC ::   telast = 3600         !: timescale for EVP elastic waves
   REAL(wp), PUBLIC ::   epsd   = 1.0e-20_wp   !: tolerance parameter for dynamic
   REAL(wp), PUBLIC ::   alpha  = 0.5_wp       !: coefficient for semi-implicit coriolis
   REAL(wp), PUBLIC ::   dm     = 0.6e+03_wp   !: diffusion constant for dynamics
   REAL(wp), PUBLIC ::   om     = 0.5_wp       !: relaxation constant
   REAL(wp), PUBLIC ::   resl   = 5.0e-05_wp   !: maximum value for the residual of relaxation
   REAL(wp), PUBLIC ::   cw     = 5.0e-03_wp   !: drag coefficient for oceanic stress
   REAL(wp), PUBLIC ::   angvg  = 0._wp        !: turning angle for oceanic stress
   REAL(wp), PUBLIC ::   pstar  = 1.0e+04_wp   !: first bulk-rheology parameter
   REAL(wp), PUBLIC ::   c_rhg  = 20._wp       !: second bulk-rhelogy parameter
   REAL(wp), PUBLIC ::   etamn  = 0._wp        !: minimun value for viscosity
   REAL(wp), PUBLIC ::   creepl = 2.e-08_wp    !: creep limit
   REAL(wp), PUBLIC ::   ecc    = 2._wp        !: eccentricity of the elliptical yield curve
   REAL(wp), PUBLIC ::   ahi0   = 350._wp      !: sea-ice hor. eddy diffusivity coeff. (m2/s)
   REAL(wp), PUBLIC ::   alphaevp = 1._wp      !: coefficient for the solution of EVP int. stresses

   REAL(wp), PUBLIC ::   usecc2                !:  = 1.0 / ( ecc * ecc )
   REAL(wp), PUBLIC ::   rhoco                 !: = rau0 * cw
   REAL(wp), PUBLIC ::   sangvg, cangvg        !: sin and cos of the turning angle for ocean stress
   REAL(wp), PUBLIC ::   pstarh                !: pstar / 2.0

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ahiu , ahiv   !: hor. diffusivity coeff. at ocean U- and V-points (m2/s)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   pahu , pahv   !: ice hor. eddy diffusivity coef. at ocean U- and V-points
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ust2s         !: friction velocity

   !!* Ice Rheology
# if defined key_lim2_vp
   !                                                      !!* VP rheology *
   LOGICAL , PUBLIC ::   lk_lim2_vp = .TRUE.               !: Visco-Plactic reology flag 
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hsnm , hicm   !: mean snow and ice thicknesses
   !
# else
   !                                                      !!* EVP rheology *
   LOGICAL , PUBLIC::   lk_lim2_vp = .FALSE.               !: Visco-Plactic reology flag 
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   stress1_i     !: first stress tensor element       
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   stress2_i     !: second stress tensor element
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   stress12_i    !: diagonal stress tensor element
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   delta_i       !: rheology delta factor (see Flato and Hibler 95) [s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   divu_i        !: Divergence of the velocity field [s-1] -> limrhg.F90
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   shear_i       !: Shear of the velocity field [s-1] -> limrhg.F90
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   at_i          !: ice fraction
   !
   REAL(wp), PUBLIC, DIMENSION(:,:)    , POINTER :: vt_s ,vt_i    !: mean snow and ice thicknesses
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:), TARGET  :: hsnm , hicm   !: target vt_s,vt_i pointers 
#endif

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   rdvosif       !: ice volume change at ice surface (only used for outputs)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   rdvobif       !: ice volume change at ice bottom  (only used for outputs)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fdvolif       !: Total   ice volume change (only used for outputs)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   rdvonif       !: Lateral ice volume change (only used for outputs)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sist          !: Sea-Ice Surface Temperature [Kelvin]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   tfu           !: Freezing/Melting point temperature of sea water at SSS
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hicif         !: Ice thickness
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hsnif         !: Snow thickness
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hicifp        !: Ice production/melting
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   frld          !: Leads fraction = 1-a/totalarea
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   phicif        !: ice thickness  at previous time 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   pfrld         !: Leads fraction at previous time  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qstoif        !: Energy stored in the brine pockets
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fbif          !: Heat flux at the ice base
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   rdmsnif       !: Variation of snow mass
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   rdmicif       !: Variation of ice mass
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qldif         !: heat balance of the lead (or of the open ocean)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qcmif         !: Energy needed to freeze the ocean surface layer
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fdtcn         !: net downward heat flux from the ice to the ocean
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qdtcn         !: energy from the ice to the ocean point (at a factor 2)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   thcm          !: part of the solar energy used in the lead heat budget
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fstric        !: Solar flux transmitted trough the ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ffltbif       !: linked with the max heat contained in brine pockets (?)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fscmbq        !: Linked with the solar flux below the ice (?)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fsbbq         !: Also linked with the solar flux below the ice (?)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qfvbq         !: used to store energy in case of toral lateral ablation (?)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   dmgwi         !: Variation of the mass of snow ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   u_ice, v_ice  !: two components of the ice   velocity at I-point (m/s)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   u_oce, v_oce  !: two components of the ocean velocity at I-point (m/s)

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   tbif  !: Temperature inside the ice/snow layer

   !!* moment used in the advection scheme
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sxice, syice, sxxice, syyice, sxyice   !: for ice  volume
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sxsn,  sysn,  sxxsn,  syysn,  sxysn    !: for snow volume                  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sxa,   sya,   sxxa,   syya,   sxya     !: for ice cover area               
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sxc0,  syc0,  sxxc0,  syyc0,  sxyc0    !: for heat content of snow         
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sxc1,  syc1,  sxxc1,  syyc1,  sxyc1    !: for heat content of 1st ice layer
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sxc2,  syc2,  sxxc2,  syyc2,  sxyc2    !: for heat content of 2nd ice layer
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sxst,  syst,  sxxst,  syyst,  sxyst    !: for heat content of brine pockets
   !!----------------------------------------------------------------------
   CONTAINS

   INTEGER FUNCTION ice_alloc_2()
      !!-----------------------------------------------------------------
      !!               *** FUNCTION ice_alloc_2 ***
      !!-----------------------------------------------------------------
      USE lib_mpp, ONLY:   ctl_warn   ! MPP library
      INTEGER :: ierr(9)              ! Local variables
      !!-----------------------------------------------------------------
      ierr(:) = 0
      !
      ALLOCATE( ahiu(jpi,jpj) , pahu(jpi,jpj) ,                      &
         &      ahiv(jpi,jpj) , pahv(jpi,jpj) , ust2s(jpi,jpj) , STAT=ierr(1) )
         !
      !* Ice Rheology
#if defined key_lim2_vp
      ALLOCATE( hsnm(jpi,jpj) , hicm(jpi,jpj) , STAT=ierr(2) )
#else
      ALLOCATE( stress1_i (jpi,jpj) , delta_i(jpi,jpj) , at_i(jpi,jpj) ,     &
                stress2_i (jpi,jpj) , divu_i (jpi,jpj) , hsnm(jpi,jpj) ,     &
                stress12_i(jpi,jpj) , shear_i(jpi,jpj) , hicm(jpi,jpj) , STAT=ierr(2) )
#endif
      ALLOCATE( rdvosif(jpi,jpj) , rdvobif(jpi,jpj) ,                      &
         &      fdvolif(jpi,jpj) , rdvonif(jpi,jpj) ,                      &
         &      sist   (jpi,jpj) , tfu    (jpi,jpj) , hicif(jpi,jpj) ,     &
         &      hsnif  (jpi,jpj) , hicifp (jpi,jpj) , frld (jpi,jpj) , STAT=ierr(3) )

      ALLOCATE(phicif(jpi,jpj) , pfrld  (jpi,jpj) , qstoif (jpi,jpj) ,     &
         &     fbif  (jpi,jpj) , rdmsnif(jpi,jpj) , rdmicif(jpi,jpj) ,     &
         &     qldif (jpi,jpj) , qcmif  (jpi,jpj) , fdtcn  (jpi,jpj) ,     &
         &     qdtcn (jpi,jpj) , thcm   (jpi,jpj)                    , STAT=ierr(4) )

      ALLOCATE(fstric(jpi,jpj) , ffltbif(jpi,jpj) , fscmbq(jpi,jpj) ,     &
         &     fsbbq (jpi,jpj) , qfvbq  (jpi,jpj) , dmgwi (jpi,jpj) ,     &
         &     u_ice (jpi,jpj) , v_ice  (jpi,jpj) ,                       &
         &     u_oce (jpi,jpj) , v_oce  (jpi,jpj) ,                       &
         &     tbif  (jpi,jpj,jplayersp1)                           , STAT=ierr(5))

      !* moment used in the advection scheme
      ALLOCATE(sxice (jpi,jpj) , syice (jpi,jpj) , sxxice(jpi,jpj) ,     &
         &     syyice(jpi,jpj) , sxyice(jpi,jpj) ,                       &
         &     sxsn  (jpi,jpj) , sysn  (jpi,jpj) , sxxsn (jpi,jpj) ,     &
         &     syysn (jpi,jpj) , sxysn (jpi,jpj)                   , STAT=ierr(6) )
      ALLOCATE(sxa   (jpi,jpj) , sya   (jpi,jpj) , sxxa  (jpi,jpj) ,     &
         &     syya  (jpi,jpj) , sxya  (jpi,jpj) ,                       & 
         &     sxc0  (jpi,jpj) , syc0  (jpi,jpj) , sxxc0 (jpi,jpj) ,     &
         &     syyc0 (jpi,jpj) , sxyc0 (jpi,jpj)                   , STAT=ierr(7))
      ALLOCATE(sxc1  (jpi,jpj) , syc1  (jpi,jpj) , sxxc1 (jpi,jpj) ,     &
         &     syyc1 (jpi,jpj) , sxyc1 (jpi,jpj) ,                       &
         &     sxc2  (jpi,jpj) , syc2  (jpi,jpj) , sxxc2 (jpi,jpj) ,     &
         &     syyc2 (jpi,jpj) , sxyc2 (jpi,jpj)                   , STAT=ierr(8))
      ALLOCATE(sxst  (jpi,jpj) , syst  (jpi,jpj) , sxxst (jpi,jpj) ,     &
         &     syyst (jpi,jpj) , sxyst (jpi,jpj)                   , STAT=ierr(9))
         !
      ice_alloc_2 = MAXVAL( ierr )
      !
      IF( ice_alloc_2 /= 0 )   CALL ctl_warn('ice_alloc_2: failed to allocate arrays')
      !
   END FUNCTION ice_alloc_2

#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty module        NO LIM 2.0 sea-ice model
   !!----------------------------------------------------------------------
#endif
   !!-----------------------------------------------------------------
   !! NEMO/LIM2 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: ice_2.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE ice_2
