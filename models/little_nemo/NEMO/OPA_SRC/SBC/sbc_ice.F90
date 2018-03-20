MODULE sbc_ice
   !!======================================================================
   !!                 ***  MODULE  sbc_ice  ***
   !! Surface module - LIM-3: parameters & variables defined in memory
   !!======================================================================
   !! History :  3.0  ! 2006-08  (G. Madec)  Surface module
   !!            3.2  ! 2009-06  (S. Masson) merge with ice_oce
   !!            3.3.1! 2011-01  (A. R. Porter, STFC Daresbury) dynamical allocation
   !!            3.4  ! 2011-11  (C. Harris) CICE added as an option
   !!----------------------------------------------------------------------
#if defined key_lim3 || defined key_lim2 || defined key_cice
   !!----------------------------------------------------------------------
   !!   'key_lim2' or 'key_lim3' :             LIM-2 or LIM-3 sea-ice model
   !!----------------------------------------------------------------------
   USE par_oce          ! ocean parameters
# if defined key_lim3
   USE par_ice          ! LIM-3 parameters
# endif
# if defined key_lim2
   USE par_ice_2        ! LIM-2 parameters
# endif
# if defined key_cice 
   USE ice_domain_size, only: ncat 
#endif
   USE lib_mpp          ! MPP library
   USE in_out_manager   ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC sbc_ice_alloc ! called in iceini(_2).F90

# if defined  key_lim2
   LOGICAL         , PUBLIC, PARAMETER ::   lk_lim2    = .TRUE.   !: LIM-2 ice model
   LOGICAL         , PUBLIC, PARAMETER ::   lk_lim3    = .FALSE.  !: no LIM-3
   LOGICAL         , PUBLIC, PARAMETER ::   lk_cice    = .FALSE.  !: no CICE 
#  if defined key_lim2_vp
   CHARACTER(len=1), PUBLIC, PARAMETER ::   cp_ice_msh = 'I'      !: VP : 'I'-grid ice-velocity (B-grid lower left corner)
#  else
   CHARACTER(len=1), PUBLIC, PARAMETER ::   cp_ice_msh = 'C'      !: EVP: 'C'-grid ice-velocity
#  endif
# endif
# if defined  key_lim3
   LOGICAL         , PUBLIC, PARAMETER ::   lk_lim2    = .FALSE.  !: no LIM-2
   LOGICAL         , PUBLIC, PARAMETER ::   lk_lim3    = .TRUE.   !: LIM-3 ice model
   LOGICAL         , PUBLIC, PARAMETER ::   lk_cice    = .FALSE.  !: no CICE 
   CHARACTER(len=1), PUBLIC, PARAMETER ::   cp_ice_msh = 'C'      !: 'C'-grid ice-velocity
# endif
# if defined  key_cice
   LOGICAL         , PUBLIC, PARAMETER ::   lk_lim2    = .FALSE.  !: no LIM-2
   LOGICAL         , PUBLIC, PARAMETER ::   lk_lim3    = .FALSE.  !: no LIM-3
   LOGICAL         , PUBLIC, PARAMETER ::   lk_cice    = .TRUE.   !: CICE ice model
   CHARACTER(len=1), PUBLIC            ::   cp_ice_msh = 'F'      !: 'F'-grid ice-velocity
# endif

#if defined key_lim3 || defined key_lim2 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qns_ice   !: non solar heat flux over ice                  [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qsr_ice   !: solar heat flux over ice                      [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qla_ice   !: latent flux over ice                          [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   dqla_ice  !: latent sensibility over ice                 [W/m2/K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   dqns_ice  !: non solar heat flux over ice (LW+SEN+LA)    [W/m2/K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   tn_ice    !: ice surface temperature                          [K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   alb_ice   !: albedo of ice

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   utau_ice  !: atmos-ice u-stress. VP: I-pt ; EVP: U,V-pts   [N/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   vtau_ice  !: atmos-ice v-stress. VP: I-pt ; EVP: U,V-pts   [N/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   fr1_i0    !: 1st Qsr fraction penetrating inside ice cover    [-]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   fr2_i0    !: 2nd Qsr fraction penetrating inside ice cover    [-]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   emp_ice   !: sublimation-snow budget over ice             [kg/m2]

# if defined key_lim3
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   tatm_ice  !: air temperature
# endif

#elif defined key_cice
   !
   ! for consistency with LIM, these are declared with three dimensions
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qlw_ice            !: incoming long-wave
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qla_ice            !: latent flux over ice           [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qsr_ice            !: solar heat flux over ice       [W/m2]
   !
   ! other forcing arrays are two dimensional
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   ss_iou             !: x ice-ocean surface stress at NEMO U point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   ss_iov             !: y ice-ocean surface stress at NEMO V point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   emp_ice            !: sublimation-snow budget over ice    [kg/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   tatm_ice           !: air temperature
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qatm_ice           !: specific humidity
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wndi_ice           !: i wind at T point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wndj_ice           !: j wind at T point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   nfrzmlt            !: NEMO frzmlt
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   fr_iu              !: ice fraction at NEMO U point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   fr_iv              !: ice fraction at NEMO V point
   !
   ! finally, arrays corresponding to different ice categories
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   a_i                !: category ice fraction
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   topmelt           !: category topmelt
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   botmelt           !: category botmelt
#endif

   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: sbc_ice.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION sbc_ice_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  FUNCTION sbc_ice_alloc  ***
      !!----------------------------------------------------------------------
#if defined key_lim3 || defined key_lim2
      ALLOCATE( qns_ice (jpi,jpj,jpl) , qsr_ice (jpi,jpj,jpl) ,     &
         &      qla_ice (jpi,jpj,jpl) , dqla_ice(jpi,jpj,jpl) ,     &
         &      dqns_ice(jpi,jpj,jpl) , tn_ice  (jpi,jpj,jpl) ,     &
         &      alb_ice (jpi,jpj,jpl) ,                             &
         &      utau_ice(jpi,jpj)     , vtau_ice(jpi,jpj)     ,     &
         &      fr1_i0  (jpi,jpj)     , fr2_i0  (jpi,jpj)     ,     &
#if defined key_lim3
         &      emp_ice(jpi,jpj)      , tatm_ice(jpi,jpj)     , STAT= sbc_ice_alloc )
#else
         &      emp_ice(jpi,jpj)                              , STAT= sbc_ice_alloc )
#endif
#elif defined key_cice
      ALLOCATE( qla_ice(jpi,jpj,1)    , qlw_ice(jpi,jpj,1)    , qsr_ice(jpi,jpj,1)    , &
                wndi_ice(jpi,jpj)     , tatm_ice(jpi,jpj)     , qatm_ice(jpi,jpj)     , &
                wndj_ice(jpi,jpj)     , nfrzmlt(jpi,jpj)      , ss_iou(jpi,jpj)       , &
                ss_iov(jpi,jpj)       , fr_iu(jpi,jpj)        , fr_iv(jpi,jpj)        , &
                a_i(jpi,jpj,ncat)     , topmelt(jpi,jpj,ncat) , botmelt(jpi,jpj,ncat), STAT= sbc_ice_alloc )
#endif
         !
      IF( lk_mpp            )   CALL mpp_sum ( sbc_ice_alloc )
      IF( sbc_ice_alloc > 0 )   CALL ctl_warn('sbc_ice_alloc: allocation of arrays failed')
   END FUNCTION sbc_ice_alloc

#else
   !!----------------------------------------------------------------------
   !!   Default option                      NO LIM 2.0 or 3.0 or CICE sea-ice model
   !!----------------------------------------------------------------------
   LOGICAL         , PUBLIC, PARAMETER ::   lk_lim2    = .FALSE.  !: no LIM-2 ice model
   LOGICAL         , PUBLIC, PARAMETER ::   lk_lim3    = .FALSE.  !: no LIM-3 ice model
   LOGICAL         , PUBLIC, PARAMETER ::   lk_cice    = .FALSE.  !: no CICE  ice model
   CHARACTER(len=1), PUBLIC, PARAMETER ::   cp_ice_msh = '-'      !: no grid ice-velocity
#endif

   !!======================================================================
END MODULE sbc_ice
