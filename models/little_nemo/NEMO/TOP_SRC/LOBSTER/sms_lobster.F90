MODULE sms_lobster 
   !!----------------------------------------------------------------------
   !!                     ***  sms_lobster.F90  ***  
   !! TOP :   LOBSTER 1 Source Minus Sink variables
   !!----------------------------------------------------------------------
   !! History :  OPA  !  1999-09  (M. Levy)  original code
   !!             -   !  2000-12  (O. Aumont, E. Kestenare) add sediment 
   !!   NEMO     1.0  !  2005-10  (C. Ethe) F90
   !!             -   !  2005-03  (A-S Kremeur) add fphylab, fzoolab, fdetlab, fdbod
   !!             -   !  2005-06  (A-S Kremeur) add sedpocb, sedpocn, sedpoca
   !!            2.0  !  2007-04  (C. Deltel, G. Madec) Free form and modules
   !!----------------------------------------------------------------------
#if defined key_lobster
   !!----------------------------------------------------------------------
   !!   'key_lobster'                                         LOBSTER model
   !!----------------------------------------------------------------------
   USE par_oce    ! ocean parameters
   USE par_trc    ! passive tracer parameters
   USE lib_mpp    ! MPP library

   IMPLICIT NONE
   PUBLIC

   PUBLIC   sms_lobster_alloc   ! called in trcini_lobster.F90

   !!  biological parameters
   !! ----------------------
   REAL(wp) ::   apmin    !: minimum phytoplancton concentration              (NAMELIST)
   REAL(wp) ::   azmin    !: minimum zooplancton concentration                (NAMELIST)
   REAL(wp) ::   anmin    !: minimum nutrients concentration                  (NAMELIST)
   REAL(wp) ::   admin    !: minimum detritus concentration                   (NAMELIST)
   REAL(wp) ::   redf     !: redfield ratio c:n                               (NAMELIST)
   REAL(wp) ::   reddom   !: redfield ratio c:n for DOM                       
   REAL(wp) ::   slopet   !: van t hoff coefficient                           (NAMELIST)
   REAL(wp) ::   toptp    !: optimal photosynthesis temperature               (NAMELIST) 
   REAL(wp) ::   aknut    !: half-saturation nutrient                         (NAMELIST)
   REAL(wp) ::   psinut   !: inhibition of nitrate uptake by ammonium         (NAMELIST)
   REAL(wp) ::   akno3    !: half-saturation for nitrate                      (NAMELIST)
   REAL(wp) ::   aknh4    !: half-saturation for ammonium                     (NAMELIST)
   REAL(wp) ::   rcchl    !: ???                                              
   REAL(wp) ::   rgamma   !: phytoplankton exudation fraction                 (NAMELIST)
   REAL(wp) ::   toptgz   !: optimal temperature for zooplankton growth       (NAMELIST)
   REAL(wp) ::   tmaxgz   !: maximal temperature for zooplankton growth       (NAMELIST) 
   REAL(wp) ::   rgz      !: widtht of zooplankton temperature FUNCTION       (NAMELIST)
   REAL(wp) ::   rppz     !: zooplankton nominal preference for phytoplancton food (NAMELIST)
   REAL(wp) ::   taus     !: maximum specific zooplankton grazing rate        (NAMELIST)
   REAL(wp) ::   aks      !: half saturation constant for total zooplankton grazing (NAMELIST)
   REAL(wp) ::   filmax   !: maximum mass clearance rate for zooplankton      (NAMELIST)
   REAL(wp) ::   rpnaz    !: non-assimilated phytoplankton by zooplancton     (NAMELIST)
   REAL(wp) ::   rdnaz    !: non-assimilated detritus by zooplankton          (NAMELIST) 
   REAL(wp) ::   eggzoo   !: minimum for zooplankton concentration            (NAMELIST)
   REAL(wp) ::   tauzn    !: zooplancton specific excretion rate              (NAMELIST)
   REAL(wp) ::   tmmaxp   !: maximal phytoplancton mortality rate             (NAMELIST)
   REAL(wp) ::   tmminp   !: minimal phytoplancton mortality rate             (NAMELIST)
   REAL(wp) ::   tmmaxz   !: maximal zooplankton mortality rate               (NAMELIST)
   REAL(wp) ::   tmminz   !: minimal zooplankton mortality rate               (NAMELIST)
   REAL(wp) ::   anumin   !: nutrient threshold for phytoplankton mortality   (NAMELIST)
   REAL(wp) ::   afdmin   !: food threshold for zooplankton mortality         (NAMELIST)
   REAL(wp) ::   taudn    !: detrital breakdown rate                          (NAMELIST)
   REAL(wp) ::   vsed     !: sedimentation speed                              (NAMELIST)
   REAL(wp) ::   tmumax   !: maximal phytoplankton growth rate                (NAMELIST)
   REAL(wp) ::   aki      !: light photosynthesis half saturation constant    (NAMELIST)
   REAL(wp) ::   tmaxr    !: maximum coefficient for passive tracer damping   (NAMELIST)
   REAL(wp) ::   tminr    !: minimum coefficient for passive tracer damping   (NAMELIST)
   REAL(wp) ::   fdoml    !: fraction of exsudation that goes to nh4 (should be labile dom)
   REAL(wp) ::   taunn    !: nitrification rate
   REAL(wp) ::   taudomn  !: slow remineralization rate of semi-labile dom to nh4
   REAL(wp) ::   xhr      !: coeff for Martin's remineralistion profile
   REAL(wp) ::   fphylab  !: NH4 fraction of phytoplankton excretion
   REAL(wp) ::   fzoolab  !: NH4 fraction of zooplankton excretion
   REAL(wp) ::   fdetlab  !: NH4 fraction of detritus dissolution
   REAL(wp) ::   fdbod    !: zooplankton mortality fraction that goes to detritus

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   remdmp   !: depth dependant damping coefficient of passive tracers 
   
   !! Optical parameters                                
   !! ------------------                                
   REAL(wp) ::   xkr0     !: water coefficient absorption in red      (NAMELIST)
   REAL(wp) ::   xkg0     !: water coefficient absorption in green    (NAMELIST)
   REAL(wp) ::   xkrp     !: pigment coefficient absorption in red    (NAMELIST)
   REAL(wp) ::   xkgp     !: pigment coefficient absorption in green  (NAMELIST)
   REAL(wp) ::   xlr      !: exposant for pigment absorption in red   (NAMELIST)
   REAL(wp) ::   xlg      !: exposant for pigment absorption in green (NAMELIST)
   REAL(wp) ::   rpig     !: chla/chla+phea ratio                     (NAMELIST)
                                                        
   INTEGER , ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   neln   !: number of levels in the euphotic layer
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   xze    !: euphotic layer depth
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   xpar   !: par (photosynthetic available radiation)

   !! Sediment parameters                               
   !! -------------------                               
   REAL(wp) ::   sedlam       !: time coefficient of POC remineralization in sediments
   REAL(wp) ::   sedlostpoc   !: ???
   REAL(wp) ::   areacot      !: ???
                                                        
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   dminl     !: fraction of sinking POC released in sediments
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   dmin3     !: fraction of sinking POC released at each level
                                                        
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sedpocb   !: mass of POC in sediments
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sedpocn   !: mass of POC in sediments
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sedpoca   !: mass of POC in sediments
                                                        
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   fbod      !: rapid sinking particles
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   cmask     !: ???

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: sms_lobster.F90 2715 2011-03-30 15:58:35Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION sms_lobster_alloc()
      !!----------------------------------------------------------------------
      !!        *** ROUTINE sms_lobster_alloc ***
      !!----------------------------------------------------------------------
      !
      ALLOCATE(                                                                   &
         !*  Biological parameters
         &      remdmp(jpk,jp_lobster)                                      ,     &
         !*  Optical parameters
         &      neln   (jpi,jpj) , xze    (jpi,jpj)     , xpar(jpi,jpj,jpk) ,     &
         !*  Sediment parameters
         &      dminl  (jpi,jpj) , dmin3  (jpi,jpj,jpk)                     ,     &
         &      sedpocb(jpi,jpj) , sedpocn(jpi,jpj)     , sedpoca(jpi,jpj)  ,     &
         &      fbod   (jpi,jpj) , cmask  (jpi,jpj)                         , STAT=sms_lobster_alloc ) 
         !
      IF( lk_mpp                 )   CALL mpp_sum ( sms_lobster_alloc )
      IF( sms_lobster_alloc /= 0 )   CALL ctl_warn('sms_lobster_alloc: failed to allocate arrays')
      !
   END FUNCTION sms_lobster_alloc

#else
   !!----------------------------------------------------------------------
   !!  Empty module :                                     NO LOBSTER model 
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE sms_lobster
