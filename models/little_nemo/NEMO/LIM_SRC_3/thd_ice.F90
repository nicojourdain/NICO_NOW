MODULE thd_ice
   !!======================================================================
   !!                       ***  MODULE thd_ice  ***
   !! LIM sea-ice :   Ice thermodynamics in 1D
   !!=====================================================================
   !! History :  3.0  !  2002-11  (C. Ethe)  F90: Free form and module
   !!----------------------------------------------------------------------
   USE par_ice        ! LIM-3 parameters
   USE in_out_manager ! I/O manager
   USE lib_mpp         ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC thd_ice_alloc ! Routine called by nemogcm.F90

   !!---------------------------
   !! * Share Module variables
   !!---------------------------
   !                                         !!! ** ice-thermo namelist (namicethd) **
   REAL(wp), PUBLIC ::   hmelt   = -0.15     !: maximum melting at the bottom; active only for one category
   REAL(wp), PUBLIC ::   hicmin  = 0.2       !: (REMOVE)
   REAL(wp), PUBLIC ::   hiclim  = 0.05      !: minimum ice thickness
   REAL(wp), PUBLIC ::   amax    = 0.999     !: maximum lead fraction
   REAL(wp), PUBLIC ::   sbeta   = 1.0       !: numerical scheme for diffusion in ice  (REMOVE)
   REAL(wp), PUBLIC ::   parlat  = 0.0       !: (REMOVE)
   REAL(wp), PUBLIC ::   hakspl  = 0.5       !: (REMOVE)
   REAL(wp), PUBLIC ::   hibspl  = 0.5       !: (REMOVE)
   REAL(wp), PUBLIC ::   exld    = 2.0       !: (REMOVE)
   REAL(wp), PUBLIC ::   hakdif  = 1.0       !: (REMOVE)
   REAL(wp), PUBLIC ::   thth    = 0.2       !: (REMOVE)
   REAL(wp), PUBLIC ::   hnzst   = 0.1       !: thick. of the surf. layer in temp. comp.
   REAL(wp), PUBLIC ::   parsub  = 1.0       !: switch for snow sublimation or not
   REAL(wp), PUBLIC ::   alphs   = 1.0       !: coef. for snow density when snow-ice formation
   REAL(wp), PUBLIC ::   fraz_swi= 1.0       !: use of frazil ice collection in function of wind (1.0) or not (0.0)
   REAL(wp), PUBLIC ::   maxfrazb= 0.7       !: maximum portion of frazil ice collecting at the ice bottom
   REAL(wp), PUBLIC ::   vfrazb  = 0.41667   !: threshold drift speed for collection of bottom frazil ice
   REAL(wp), PUBLIC ::   Cfrazb  = 5.0       !: squeezing coefficient for collection of bottom frazil ice

   REAL(wp), PUBLIC, DIMENSION(2) ::   hiccrit = (/0.3,0.3/)  !: ice th. for lateral accretion in the NH (SH) (m)

   !!-----------------------------
   !! * Share 1D Module variables
   !!-----------------------------
   !: In ice thermodynamics, to spare memory, the vectors are folded
   !: from 1D to 2D vectors. The following variables, with ending _1d (or _b)
   !: are the variables corresponding to 2d vectors

   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   npb    !: number of points where computations has to be done
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   npac   !: correspondance between points (lateral accretion)

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qldif_1d      !: <==> the 2D  qldif
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qcmif_1d      !: <==> the 2D  qcmif
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fstbif_1d     !: <==> the 2D  fstric
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fltbif_1d     !: <==> the 2D  ffltbif
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fscbq_1d      !: <==> the 2D  fscmcbq
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qsr_ice_1d    !: <==> the 2D  qsr_ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fr1_i0_1d     !: <==> the 2D  fr1_i0
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fr2_i0_1d     !: <==> the 2D  fr2_i0
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qnsr_ice_1d   !: <==> the 2D  qns_ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qfvbq_1d      !: <==> the 2D  qfvbq
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   t_bo_b        !: <==> the 2D  t_bo

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sprecip_1d    !: <==> the 2D  sprecip
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   frld_1d       !: <==> the 2D  frld
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   at_i_b        !: <==> the 2D  frld
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fbif_1d       !: <==> the 2D  fbif
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   rdmicif_1d    !: <==> the 2D  rdmicif
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   rdmsnif_1d    !: <==> the 2D  rdmsnif
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qlbbq_1d      !: <==> the 2D  qlbsbq
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dmgwi_1d      !: <==> the 2D  dmgwi
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dvsbq_1d      !: <==> the 2D  rdvosif
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dvbbq_1d      !: <==> the 2D  rdvobif
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dvlbq_1d      !: <==> the 2D  rdvolif
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dvnbq_1d      !: <==> the 2D  rdvolif
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dqns_ice_1d   !: <==> the 2D  dqns_ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qla_ice_1d    !: <==> the 2D  qla_ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dqla_ice_1d   !: <==> the 2D  dqla_ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   tatm_ice_1d   !: <==> the 2D  tatm_ice
   !                                                     ! to reintegrate longwave flux inside the ice thermodynamics
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fsup          !: Energy flux sent from bottom to lateral ablation if |dhb|> 0.15 m
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   focea         !: Remaining energy in case of total ablation
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   i0            !: fraction of radiation transmitted to the ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   old_ht_i_b    !: Ice thickness at the beginnning of the time step [m]
    REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::  old_ht_s_b    !: Snow thickness at the beginning of the time step [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fsbri_1d      !: Salt flux due to brine drainage
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fhbri_1d      !: Heat flux due to brine drainage
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fseqv_1d      !: Equivalent Salt flux due to ice growth/decay
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dsm_i_fl_1d   !: Ice salinity variations due to flushing
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dsm_i_gd_1d   !: Ice salinity variations due to gravity drainage
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dsm_i_se_1d   !: Ice salinity variations due to basal salt entrapment
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dsm_i_si_1d   !: Ice salinity variations due to lateral accretion    
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hicol_b       !: Ice collection thickness accumulated in fleads

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   t_su_b      !: <==> the 2D  t_su
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   a_i_b       !: <==> the 2D  a_i
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ht_i_b      !: <==> the 2D  ht_s
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ht_s_b      !: <==> the 2D  ht_i
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fc_su       !: Surface Conduction flux 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fc_bo_i     !: Bottom  Conduction flux 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_s_tot    !: Snow accretion/ablation        [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_i_surf   !: Ice surface accretion/ablation [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_i_bott   !: Ice bottom accretion/ablation  [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_snowice  !: Snow ice formation             [m of ice]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sm_i_b      !: Ice bulk salinity [ppt]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   s_i_new     !: Salinity of new ice at the bottom
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   s_snowice   !: Salinity of new snow ice on top of the ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   o_i_b       !: Ice age                        [days]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   t_s_b   !: corresponding to the 2D var  t_s
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   t_i_b   !: corresponding to the 2D var  t_i
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   s_i_b   !: profiled ice salinity
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   q_i_b   !:    Ice  enthalpy per unit volume
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   q_s_b   !:    Snow enthalpy per unit volume

   ! Clean the following ...
   ! These variables are coded for conservation checks
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qt_i_in                  !: ice energy summed over categories (initial)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qt_i_fin                 !: ice energy summed over categories (final)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qt_s_in, qt_s_fin        !: snow energy summed over categories
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   dq_i, sum_fluxq          !: increment of energy, sum of fluxes
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fatm, foce               !: atmospheric, oceanic, heat flux
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   cons_error, surf_error   !: conservation, surface error

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   q_i_layer_in        !: goes to trash
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   q_i_layer_fin       !: goes to trash
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   dq_i_layer, radab   !: goes to trash

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ftotal_in    !: initial total heat flux
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ftotal_fin   !: final total heat flux

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fc_s
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fc_i
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   de_s_lay
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   de_i_lay
   
   INTEGER , PUBLIC ::   jiindex_1d   ! 1D index of debugging point

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: thd_ice.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   FUNCTION thd_ice_alloc()
      !!---------------------------------------------------------------------!
      !!                ***  ROUTINE thd_ice_alloc ***
      !!---------------------------------------------------------------------!
      INTEGER ::   thd_ice_alloc   ! return value
      INTEGER ::   ierr(4)
      !!---------------------------------------------------------------------!

      ALLOCATE( npb      (jpij) , npac     (jpij),                          &
         !                                                                  !
         &      qldif_1d (jpij) , qcmif_1d (jpij) , fstbif_1d  (jpij) ,     &
         &      fltbif_1d(jpij) , fscbq_1d (jpij) , qsr_ice_1d (jpij) ,     &
         &      fr1_i0_1d(jpij) , fr2_i0_1d(jpij) , qnsr_ice_1d(jpij) ,     &
         &      qfvbq_1d (jpij) , t_bo_b   (jpij)                     , STAT=ierr(1) )
      !
      ALLOCATE( sprecip_1d (jpij) , frld_1d    (jpij) , at_i_b     (jpij) ,     &
         &      fbif_1d    (jpij) , rdmicif_1d (jpij) , rdmsnif_1d (jpij) ,     &
         &      qlbbq_1d   (jpij) , dmgwi_1d   (jpij) , dvsbq_1d   (jpij) ,     &
         &      dvbbq_1d   (jpij) , dvlbq_1d   (jpij) , dvnbq_1d   (jpij) ,     &
         &      dqns_ice_1d(jpij) , qla_ice_1d (jpij) , dqla_ice_1d(jpij) ,     &
         &      tatm_ice_1d(jpij) , fsup       (jpij) , focea      (jpij) ,     &   
         &      i0         (jpij) , old_ht_i_b (jpij) , old_ht_s_b (jpij) ,     &  
         &      fsbri_1d   (jpij) , fhbri_1d   (jpij) , fseqv_1d   (jpij) ,     &
         &      dsm_i_fl_1d(jpij) , dsm_i_gd_1d(jpij) , dsm_i_se_1d(jpij) ,     &     
         &      dsm_i_si_1d(jpij) , hicol_b    (jpij)                     , STAT=ierr(2) )
      !
      ALLOCATE( t_su_b    (jpij) , a_i_b    (jpij) , ht_i_b   (jpij) ,    &   
         &      ht_s_b    (jpij) , fc_su    (jpij) , fc_bo_i  (jpij) ,    &    
         &      dh_s_tot  (jpij) , dh_i_surf(jpij) , dh_i_bott(jpij) ,    &    
         &      dh_snowice(jpij) , sm_i_b   (jpij) , s_i_new  (jpij) ,    &    
         &      s_snowice (jpij) , o_i_b    (jpij)                   ,    &
         !                                                                !
         &      t_s_b(jpij,nlay_s),                                       &
         !                                                                !
         &      t_i_b(jpij,jkmax), s_i_b(jpij,jkmax)                ,     &            
         &      q_i_b(jpij,jkmax), q_s_b(jpij,jkmax)                , STAT=ierr(3))
      !
      ALLOCATE( qt_i_in   (jpij,jpl) , qt_i_fin(jpij,jpl) , qt_s_in   (jpij,jpl) ,     &
         &      qt_s_fin  (jpij,jpl) , dq_i    (jpij,jpl) , sum_fluxq (jpij,jpl) ,     &
         &      fatm      (jpij,jpl) , foce    (jpij,jpl) , cons_error(jpij,jpl) ,     &
         &      surf_error(jpij,jpl)                                             ,     &
         !                                                                             !
         &      q_i_layer_in(jpij,jkmax) , q_i_layer_fin(jpij,jkmax)             ,     &
         &      dq_i_layer  (jpij,jkmax) , radab        (jpij,jkmax)             ,     &
         !                                                                             !
         &      ftotal_in(jpij), ftotal_fin(jpij)                                ,     &
         !                                                                             !
         &      fc_s(jpij,0:nlay_s) , de_s_lay(jpij,nlay_s)                      ,     &
         &      fc_i(jpij,0:jkmax)  , de_i_lay(jpij,jkmax)                       , STAT=ierr(4) )

      thd_ice_alloc = MAXVAL( ierr )

      IF( thd_ice_alloc /= 0 )   CALL ctl_warn( 'thd_ice_alloc: failed to allocate arrays.' )
      !
   END FUNCTION thd_ice_alloc
   
   !!======================================================================
END MODULE thd_ice
