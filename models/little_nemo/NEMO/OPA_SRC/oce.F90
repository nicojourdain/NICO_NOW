MODULE oce
   !!======================================================================
   !!                      ***  MODULE  oce  ***
   !! Ocean        :  dynamics and active tracers defined in memory 
   !!======================================================================
   !! History :  1.0  !  2002-11  (G. Madec)  F90: Free form and module
   !!            3.1  !  2009-02  (G. Madec, M. Leclair)  pure z* coordinate
   !!            3.3  !  2010-09  (C. Ethe) TRA-TRC merge: add ts, gtsu, gtsv 4D arrays
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC oce_alloc ! routine called by nemo_init in nemogcm.F90

   LOGICAL, PUBLIC ::   l_traldf_rot = .FALSE.  !: rotated laplacian operator for lateral diffusion

   !! dynamics and tracer fields                            ! before ! now    ! after  ! the after trends becomes the fields
   !! --------------------------                            ! fields ! fields ! trends ! only after tra_zdf and dyn_spg
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   ub   ,  un    , ua     !: i-horizontal velocity        [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   vb   ,  vn    , va     !: j-horizontal velocity        [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::           wn             !: vertical velocity            [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   rotb ,  rotn           !: relative vorticity           [s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   hdivb,  hdivn          !: horizontal divergence        [s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   tsb  ,  tsn            !: 4D T-S fields        [Celcius,psu] 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   rn2b ,  rn2            !: brunt-vaisala frequency**2   [s-2]
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::  tsa             !: 4D T-S trends fields & work array 
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   rhd    !: in situ density anomalie rhd=(rho-rau0)/rau0  [no units]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   rhop   !: potential volumic mass                           [kg/m3]

   !! free surface                                      !  before  ! now    ! after  !
   !! ------------                                      !  fields  ! fields ! trends !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:), TARGET ::   sshb   , sshn   , ssha   !: sea surface height at t-point [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)         ::   sshu_b , sshu_n , sshu_a !: sea surface height at u-point [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)         ::   sshv_b , sshv_n , sshv_a !: sea surface height at u-point [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)         ::            sshf_n          !: sea surface height at f-point [m]
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   spgu, spgv               !: horizontal surface pressure gradient

   !! interpolated gradient (only used in zps case)
   !! ---------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   gtsu, gtsv   !: horizontal gradient of T, S bottom u-point 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   gru , grv    !: horizontal gradient of rd at bottom u-point

   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: oce.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION oce_alloc()
      !!----------------------------------------------------------------------
      !!                   ***  FUNCTION oce_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr(2)
      !!----------------------------------------------------------------------
      !
      ALLOCATE( ub   (jpi,jpj,jpk)      , un   (jpi,jpj,jpk)      , ua(jpi,jpj,jpk)       ,     &
         &      vb   (jpi,jpj,jpk)      , vn   (jpi,jpj,jpk)      , va(jpi,jpj,jpk)       ,     &      
         &      wn   (jpi,jpj,jpk)      ,                                                       &
         &      rotb (jpi,jpj,jpk)      , rotn (jpi,jpj,jpk)      ,                             &   
         &      hdivb(jpi,jpj,jpk)      , hdivn(jpi,jpj,jpk)      ,                             &
         &      tsb  (jpi,jpj,jpk,jpts) , tsn  (jpi,jpj,jpk,jpts) , tsa(jpi,jpj,jpk,jpts) ,     &
         &      rn2b (jpi,jpj,jpk)      , rn2  (jpi,jpj,jpk)                              , STAT=ierr(1) )
         !
      ALLOCATE(rhd (jpi,jpj,jpk) ,                                         &
         &     rhop(jpi,jpj,jpk) ,                                         &
         &     sshb  (jpi,jpj)   , sshn  (jpi,jpj) , ssha  (jpi,jpj) ,     &
         &     sshu_b(jpi,jpj)   , sshu_n(jpi,jpj) , sshu_a(jpi,jpj) ,     &
         &     sshv_b(jpi,jpj)   , sshv_n(jpi,jpj) , sshv_a(jpi,jpj) ,     &
         &                         sshf_n(jpi,jpj) ,                       &
         &     spgu  (jpi,jpj)   , spgv(jpi,jpj)   ,                       &
         &     gtsu(jpi,jpj,jpts), gtsv(jpi,jpj,jpts),                     &
         &     gru(jpi,jpj)      , grv(jpi,jpj)                      , STAT=ierr(2) )
         !
      oce_alloc = MAXVAL( ierr )
      IF( oce_alloc /= 0 )   CALL ctl_warn('oce_alloc: failed to allocate arrays')
      !
   END FUNCTION oce_alloc

   !!======================================================================
END MODULE oce
