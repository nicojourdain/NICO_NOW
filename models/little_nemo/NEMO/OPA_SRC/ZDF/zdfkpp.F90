MODULE zdfkpp
   !!======================================================================
   !!                       ***  MODULE  zdfkpp  ***
   !! Ocean physics:  vertical mixing coefficient compute from the KPP 
   !!                 turbulent closure parameterization
   !!=====================================================================
   !! History :  OPA  ! 2000-03 (W.G. Large, J. Chanut) Original code
   !!            8.1  ! 2002-06 (J.M. Molines) for real case CLIPPER  
   !!            8.2  ! 2003-10 (Chanut J.) re-writting
   !!   NEMO     1.0  ! 2005-01 (C. Ethe, G. Madec) Free form, F90 + creation of tra_kpp routine
   !!            3.3  ! 2010-10 (C. Ethe, G. Madec) reorganisation of initialisation phase + merge TRC-TRA
   !!----------------------------------------------------------------------
#if defined key_zdfkpp   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_zdfkpp'                                             KPP scheme
   !!----------------------------------------------------------------------
   !!   zdf_kpp      : update momentum and tracer Kz from a kpp scheme
   !!   zdf_kpp_init : initialization, namelist read, and parameters control
   !!   tra_kpp      : compute and add to the T & S trend the non-local flux
   !!   trc_kpp      : compute and add to the passive tracer trend the non-local flux (lk_top=T)
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and active tracers 
   USE dom_oce         ! ocean space and time domain
   USE zdf_oce         ! ocean vertical physics
   USE sbc_oce         ! surface boundary condition: ocean
   USE phycst          ! physical constants
   USE eosbn2          ! equation of state
   USE zdfddm          ! double diffusion mixing
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! work arrays
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE prtctl          ! Print control
   USE trdmod_oce      ! ocean trends definition
   USE trdtra          ! tracers trends
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_kpp       ! routine called by step.F90
   PUBLIC   zdf_kpp_init  ! routine called by opa.F90
   PUBLIC   tra_kpp       ! routine called by step.F90
   PUBLIC   trc_kpp       ! routine called by trcstp.F90

   LOGICAL , PUBLIC, PARAMETER ::   lk_zdfkpp = .TRUE.    !: KPP vertical mixing flag

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ghats    !: non-local scalar mixing term (gamma/<ws>o)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wt0      !: surface temperature flux for non local flux
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   ws0      !: surface salinity flux for non local flux
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hkpp     !: boundary layer depth

   !                                        !!* Namelist namzdf_kpp *
   REAL(wp) ::   rn_difmiw  =  1.2e-04_wp    ! constant internal wave viscosity (m2/s)
   REAL(wp) ::   rn_difsiw  =  1.2e-05_wp    ! constant internal wave diffusivity (m2/s)
   REAL(wp) ::   rn_riinfty =  0.8_wp        ! local Richardson Number limit for shear instability
   REAL(wp) ::   rn_difri   =  5.e-03_wp     ! maximum shear mixing at Rig = 0    (m2/s)
   REAL(wp) ::   rn_bvsqcon = -1.e-09_wp     ! Brunt-Vaisala squared (1/s**2) for maximum convection
   REAL(wp) ::   rn_difcon  =  1._wp         ! maximum mixing in interior convection (m2/s)
   INTEGER  ::   nn_ave     =  1             ! = 0/1 flag for horizontal average on avt, avmu, avmv

#if defined key_zdfddm
   !                                        !!! ** Double diffusion Mixing
   REAL(wp) ::   difssf  = 1.e-03_wp         ! maximum salt fingering mixing 
   REAL(wp) ::   Rrho0   = 1.9_wp            ! limit for salt  fingering mixing 
   REAL(wp) ::   difsdc  = 1.5e-06_wp        ! maximum diffusive convection mixing
#endif
   LOGICAL  ::   ln_kpprimix  = .TRUE.       ! Shear instability mixing 

   !                                        !!! ** General constants  **
   REAL(wp) ::   epsln   = 1.0e-20_wp        ! a small positive number    
   REAL(wp) ::   pthird  = 1._wp/3._wp       ! 1/3
   REAL(wp) ::   pfourth = 1._wp/4._wp       ! 1/4

   !                                        !!! ** Boundary Layer Turbulence Parameters  **
   REAL(wp) ::   vonk     = 0.4_wp           ! von Karman's constant
   REAL(wp) ::   epsilon  = 0.1_wp           ! nondimensional extent of the surface layer
   REAL(wp) ::   rconc1   = 5.0_wp           ! standard flux profile function parmaeters
   REAL(wp) ::   rconc2   = 16.0_wp          !         "        "
   REAL(wp) ::   rconcm   = 8.38_wp          ! momentum flux profile fit
   REAL(wp) ::   rconam   = 1.26_wp          !         "       "
   REAL(wp) ::   rzetam   = -.20_wp          !         "       "       
   REAL(wp) ::   rconcs   = 98.96_wp         !  scalar  flux profile fit
   REAL(wp) ::   rconas   = -28.86_wp        !         "       "
   REAL(wp) ::   rzetas   = -1.0_wp          !         "       "  
   
   !                                        !!! ** Boundary Layer Depth Diagnostic  **
   REAL(wp) ::   Ricr     = 0.3_wp           ! critical bulk Richardson Number
   REAL(wp) ::   rcekman  = 0.7_wp           ! coefficient for ekman depth  
   REAL(wp) ::   rcmonob  = 1.0_wp           ! coefficient for Monin-Obukhov depth 
   REAL(wp) ::   rconcv   = 1.7_wp           ! ratio of interior buoyancy frequency to its value at entrainment depth
   REAL(wp) ::   hbf      = 1.0_wp           ! fraction of bound. layer depth to which absorbed solar 
      !                                      ! rad. and contributes to surf. buo. forcing
   REAL(wp) ::   Vtc                         ! function of rconcv,rconcs,epsilon,vonk,Ricr
   
   !                                        !!! ** Nonlocal Boundary Layer Mixing **
   REAL(wp) ::   rcstar   = 5.0_wp           ! coefficient for convective nonlocal transport
   REAL(wp) ::   rcs      = 1.0e-3_wp        ! conversion: mm/s ==> m/s   
   REAL(wp) ::   rcg                         ! non-dimensional coefficient for nonlocal transport

#if ! defined key_kppcustom
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   del     ! array for reference mean values of vertical integration 
#endif

#if defined key_kpplktb
   !                                         !!! ** Parameters for lookup table for turbulent velocity scales ** 
   INTEGER, PARAMETER ::   nilktb   = 892     ! number of values for zehat in KPP lookup table
   INTEGER, PARAMETER ::   njlktb   = 482     ! number of values for ustar in KPP lookup table
   INTEGER, PARAMETER ::   nilktbm1 = nilktb-1   !
   INTEGER, PARAMETER ::   njlktbm1 = njlktb-1   !

   REAL(wp), DIMENSION(nilktb,njlktb) ::   wmlktb   ! lookup table for the turbulent vertical velocity scale (momentum)
   REAL(wp), DIMENSION(nilktb,njlktb) ::   wslktb   ! lookup table for the turbulent vertical velocity scale (tracers)

   REAL(wp) ::   dehatmin = -4.e-7_wp    ! minimum limit for zhat in lookup table (m3/s3) 
   REAL(wp) ::   dehatmax = 0._wp        ! maximum limit for zhat in lookup table (m3/s3)
   REAL(wp) ::   ustmin   = 0._wp        ! minimum limit for ustar in lookup table (m/s)
   REAL(wp) ::   ustmax   = 0.04_wp      ! maximum limit for ustar in lookup table (m/s)    
   REAL(wp) ::   dezehat                 ! delta zhat in lookup table
   REAL(wp) ::   deustar                 ! delta ustar in lookup table
#endif
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:) ::   ratt   ! attenuation coef  (already defines in module traqsr, 
   !                                    ! but only if the solar radiation penetration is considered)
   
   !                                    !!! * penetrative solar radiation coefficient *
   REAL(wp) ::   rabs = 0.58_wp          ! fraction associated with xsi1
   REAL(wp) ::   xsi1 = 0.35_wp          ! first depth of extinction 
   REAL(wp) ::   xsi2 = 23.0_wp          ! second depth of extinction 
      !                           ! (default values: water type Ib) 

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   etmean, eumean, evmean   ! coeff. used for hor. smoothing at t-, u- & v-points
        
#if defined key_c1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   rig    !: gradient Richardson number
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   rib    !: bulk Richardson number
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   buof   !: buoyancy forcing
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   mols   !: moning-Obukhov length scale 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   ekdp   !: Ekman depth
#endif

   INTEGER  ::   jip = 62 , jjp = 111

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
#  include  "zdfddm_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: zdfkpp.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION zdf_kpp_alloc()
      !!----------------------------------------------------------------------
      !!                 ***  FUNCTION zdf_kpp_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( ghats(jpi,jpj,jpk), wt0(jpi,jpj), ws0(jpi,jpj), hkpp(jpi,jpj), &
#if ! defined key_kpplktb
         &      del(jpk,jpk),                                                  &
#endif
         &      ratt(jpk),                                                     &
         &      etmean(jpi,jpj,jpk), eumean(jpi,jpj,jpk), evmean(jpi,jpj,jpk), &
#if defined key_c1d
         &      rig (jpi,jpj,jpk), rib(jpi,jpj,jpk), buof(jpi,jpj,jpk),        &
         &      mols(jpi,jpj,jpk), ekdp(jpi,jpj),                              &
#endif
         &      STAT= zdf_kpp_alloc )
         !
      IF( lk_mpp             )   CALL mpp_sum ( zdf_kpp_alloc )
      IF( zdf_kpp_alloc /= 0 )   CALL ctl_warn('zdf_kpp_alloc: failed to allocate arrays')
   END FUNCTION zdf_kpp_alloc


   SUBROUTINE zdf_kpp( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE zdf_kpp  ***
      !!
      !! ** Purpose :   Compute the vertical eddy viscosity and diffusivity
      !!      coefficients and non local mixing using K-profile parameterization
      !!
      !! ** Method :   The boundary layer depth hkpp is diagnosed at tracer points
      !!      from profiles of buoyancy, and shear, and the surface forcing.
      !!      Above hbl (sigma=-z/hbl <1) the mixing coefficients are computed from 
      !!
      !!                      Kx =  hkpp  Wx(sigma) G(sigma)  
      !!
      !!             and the non local term ghat = Cs / Ws(sigma) / hkpp
      !!      Below hkpp  the coefficients are the sum of mixing due to internal waves
      !!      shear instability and double diffusion.
      !!
      !!      -1- Compute the now interior vertical mixing coefficients at all depths. 
      !!      -2- Diagnose the boundary layer depth. 
      !!      -3- Compute the now boundary layer vertical mixing coefficients. 
      !!      -4- Compute the now vertical eddy vicosity and diffusivity.
      !!      -5- Smoothing
      !!
      !!        N.B. The computation is done from jk=2 to jpkm1 
      !!             Surface value of avt avmu avmv are set once a time to zero
      !!             in routine zdf_kpp_init.
      !!
      !! ** Action  :   update the non-local terms ghats
      !!                update avt, avmu, avmv (before vertical eddy coef.)
      !!
      !! References : Large W.G., Mc Williams J.C. and Doney S.C.              
      !!         Reviews of Geophysics, 32, 4, November 1994
      !!         Comments in the code refer to this paper, particularly 
      !!         the equation number. (LMD94, here after)
      !!----------------------------------------------------------------------
      USE oce     , zviscos => ua   ! temp. array for viscosities use ua as workspace
      USE oce     , zdiffut => va   ! temp. array for diffusivities use sa as workspace
      !!
      INTEGER, INTENT( in  ) ::   kt   ! ocean time step
      !!
      INTEGER ::   ji, jj, jk              ! dummy loop indices
      INTEGER ::   ikbot, jkmax, jkm1, jkp2   !

      REAL(wp) ::   ztx, zty, zflageos, zstabl, zbuofdep,zucube     !
      REAL(wp) ::   zrhos, zalbet, zbeta, zthermal, zhalin, zatt1   !
      REAL(wp) ::   zref, zt, zs, zh, zu, zv, zrh                   ! Bulk richardson number
      REAL(wp) ::   zrib, zrinum, zdVsq, zVtsq                      !
      REAL(wp) ::   zehat, zeta, zhrib, zsig, zscale, zwst, zws, zwm   ! Velocity scales
#if defined key_kpplktb
      INTEGER ::    il, jl                                          ! Lookup table or Analytical functions 
      REAL(wp) ::   ud, zfrac, ufrac, zwam, zwbm, zwas, zwbs        !
#else
      REAL(wp) ::   zwsun, zwmun, zcons, zconm, zwcons, zwconm      !
#endif
      REAL(wp) ::   zsr, zbw, ze, zb, zd, zc, zaw, za, zb1, za1, zkw, zk0, zcomp , zrhd,zrhdr,zbvzed   ! In situ density
#if ! defined key_kppcustom     
      INTEGER  ::   jm                          ! dummy loop indices
      REAL(wp) ::   zr1, zr2, zr3, zr4, zrhop   ! Compression terms
#endif
      REAL(wp) ::   zflag, ztemp, zrn2, zdep21, zdep32, zdep43
      REAL(wp) ::   zdku2, zdkv2, ze3sqr, zsh2, zri, zfri          ! Interior richardson mixing
      REAL(wp), POINTER, DIMENSION(:,:) ::   zmoek                 ! Moning-Obukov limitation
      REAL(wp), POINTER, DIMENSION(:)   ::   zmoa, zekman                
      REAL(wp)                          ::   zmob, zek
      REAL(wp), POINTER, DIMENSION(:,:) ::   zdepw, zdift, zvisc   ! The pipe 
      REAL(wp), POINTER, DIMENSION(:,:) ::   zdept
      REAL(wp), POINTER, DIMENSION(:,:) ::   zriblk
      REAL(wp), POINTER, DIMENSION(:)   ::   zhmax, zria, zhbl 
      REAL(wp) ::   zflagri, zflagek, zflagmo, zflagh, zflagkb   !
      REAL(wp), POINTER, DIMENSION(:)   ::   za2m, za3m, zkmpm, za2t, za3t, zkmpt   ! Shape function (G)
      REAL(wp) ::   zdelta, zdelta2, zdzup, zdzdn, zdzh, zvath, zgat1, zdat1, zkm1m, zkm1t
#if defined key_zdfddm
      REAL(wp) ::   zrrau, zds, zavdds, zavddt,zinr   ! double diffusion mixing
      REAL(wp), POINTER, DIMENSION(:,:) ::     zdifs
      REAL(wp), POINTER, DIMENSION(:)   ::   za2s, za3s, zkmps
      REAL(wp) ::                            zkm1s
      REAL(wp), POINTER, DIMENSION(:,:) ::   zblcs
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zdiffus
#endif
      REAL(wp), POINTER, DIMENSION(:,:) ::   zBo, zBosol, zustar         ! Surface buoyancy forcing, friction velocity
      REAL(wp), POINTER, DIMENSION(:,:) ::   zmask, zblcm, zblct
      !!--------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zdf_kpp')
      !
      CALL wrk_alloc( jpi, zmoa, zekman, zhmax, zria, zhbl )
      CALL wrk_alloc( jpi, za2m, za3m, zkmpm, za2t, za3t, zkmpt )
      CALL wrk_alloc( jpi,2, zriblk )
      CALL wrk_alloc( jpi,3, zmoek, kjstart = 0 )
      CALL wrk_alloc( jpi,3, zdept )
      CALL wrk_alloc( jpi,4, zdepw, zdift, zvisc )
      CALL wrk_alloc( jpi,jpj, zBo, zBosol, zustar )
      CALL wrk_alloc( jpi,jpk, zmask, zblcm, zblct, zblcs )
#if defined key_zdfddm
      CALL wrk_alloc( jpi,4, zdifs )
      CALL wrk_alloc( jpi, zmoa, za2s, za3s, zkmps )
      CALL wrk_alloc( jpi,jpk, zblcs )
      CALL wrk_alloc( jpi,jpi,jpk, zdiffus )
#endif

      zviscos(:,:,:) = 0.
      zblcm  (:,:  ) = 0. 
      zdiffut(:,:,:) = 0.
      zblct  (:,:  ) = 0. 
#if defined key_zdfddm
      zdiffus(:,:,:) = 0.
      zblcs  (:,:  ) = 0. 
#endif
      ghats(:,:,:) = 0.
     
      zBo   (:,:) = 0.
      zBosol(:,:) = 0.
      zustar(:,:) = 0.


      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! I. Interior diffusivity and viscosity at w points ( T interfaces)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
      DO jk = 2, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1 
               ! Mixing due to internal waves breaking
               ! -------------------------------------
               avmu(ji,jj,jk)  = rn_difmiw 
               avt (ji,jj,jk)  = rn_difsiw             
               ! Mixing due to vertical shear instability
               ! -------------------------------------               
               IF( ln_kpprimix ) THEN          
                  ! Compute the gradient Richardson  number at interfaces (zri):
                  ! LMD94, eq. 27 (is vertical smoothing needed : Rig=N^2 / (dz(u))^2 + (dz(v))^2
                  zdku2 =   ( un(ji - 1,jj,jk - 1) - un(ji - 1,jj,jk) ) &
                     &    * ( un(ji - 1,jj,jk - 1) - un(ji - 1,jj,jk) ) &
                     &    + ( un(ji    ,jj,jk - 1) - un(ji    ,jj,jk) ) &
                     &    * ( un(ji    ,jj,jk - 1) - un(ji    ,jj,jk) )  
                  
                  zdkv2 =   ( vn(ji,jj - 1,jk - 1) - vn(ji,jj - 1,jk) ) &
                     &    * ( vn(ji,jj - 1,jk - 1) - vn(ji,jj - 1,jk) ) &
                     &    + ( vn(ji,    jj,jk - 1) - vn(ji,    jj,jk) ) &
                     &    * ( vn(ji,    jj,jk - 1) - vn(ji,    jj,jk) )  

                  ze3sqr = 1. / ( fse3w(ji,jj,jk) * fse3w(ji,jj,jk) )
                  ! Square of vertical shear  at interfaces
                  zsh2   = 0.5 * ( zdku2 + zdkv2 ) * ze3sqr
                  zri    = MAX( rn2(ji,jj,jk), 0. ) / ( zsh2 + epsln ) 
#if defined key_c1d
                  ! save the gradient richardson number
                  rig(ji,jj,jk) = zri * tmask(ji,jj,jk)
#endif                  
                  ! Evaluate f of Ri (zri) for shear instability store in zfri
                  ! LMD94, eq. 28a,b,c, figure 3 ; Rem: p1 is 3, hard coded
                  zfri  = MAX( zri , 0. )
                  zfri  = MIN( zfri / rn_riinfty , 1.0 )
                  zfri  = ( 1.0 - zfri * zfri )
                  zfri  = zfri * zfri  * zfri
                  ! add shear contribution to mixing coef. 
                  avmu(ji,jj,jk) =  avmu(ji,jj,jk) + rn_difri * zfri   
                  avt (ji,jj,jk) =  avt (ji,jj,jk) + rn_difri * zfri    
               ENDIF
#if defined key_zdfddm 
               avs (ji,jj,jk) =  avt (ji,jj,jk)              
               !  Double diffusion mixing ; NOT IN ROUTINE ZDFDDM.F90
               ! ------------------------------------------------------------------
               ! only retains positive value of rrau
               zrrau = MAX( rrau(ji,jj,jk), epsln )
               zds   = tsn(ji,jj,jk-1,jp_sal) - tsn(ji,jj,jk,jp_sal)
               IF( zrrau > 1. .AND. zds > 0.) THEN
                  !
                  ! Salt fingering case.
                  !---------------------
                  ! Compute interior diffusivity for double diffusive mixing of
                  ! salinity. Upper bound "zrrau" by "Rrho0"; (Rrho0=1.9, difcoefnuf=0.001).
                  ! After that set interior diffusivity for double diffusive mixing
                  ! of temperature
                  zavdds = MIN( zrrau, Rrho0 )
                  zavdds = ( zavdds - 1.0 ) / ( Rrho0 - 1.0 )
                  zavdds = 1.0 - zavdds * zavdds 
                  zavdds = zavdds * zavdds * zavdds 
                  zavdds = difssf * zavdds 
                  zavddt = 0.7 * zavdds
               ELSEIF( zrrau < 1. .AND. zrrau > 0. .AND. zds < 0.) THEN
                  !
                  ! Diffusive convection case.
                  !---------------------------
                  ! Compute interior diffusivity for double diffusive mixing of
                  ! temperature (Marmorino and Caldwell, 1976); 
                  ! Compute interior diffusivity for double diffusive mixing of salinity 
                  zinr   = 1. / zrrau
                  zavddt = 0.909 * EXP( 4.6 * EXP( -0.54* ( zinr - 1. ) ) ) 
                  zavddt = difsdc * zavddt
                  IF( zrrau < 0.5) THEN
                     zavdds = zavddt * 0.15 * zrrau
                  ELSE
                     zavdds = zavddt * (1.85 * zrrau - 0.85 ) 
                  ENDIF
               ELSE
                  zavddt = 0.
                  zavdds = 0.
               ENDIF 
               ! Add double diffusion contribution to temperature and salinity  mixing coefficients.
               avt (ji,jj,jk) =  avt (ji,jj,jk) +  zavddt 
               avs (ji,jj,jk) =  avs (ji,jj,jk) +  zavdds         
#endif                      
            END DO
         END DO
      END DO


      ! Radiative (zBosol) and non radiative (zBo) surface buoyancy
      !JMM at the time zdfkpp is called, q still holds the sum q + qsr
      !---------------------------------------------------------------------
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1     
            IF( nn_eos < 1) THEN   
               zt     = tsn(ji,jj,1,jp_tem)
               zs     = tsn(ji,jj,1,jp_sal) - 35.0
               zh     = fsdept(ji,jj,1)
               !  potential volumic mass
               zrhos  = rhop(ji,jj,1)
               zalbet = ( ( ( - 0.255019e-07 * zt + 0.298357e-05 ) * zt   &   ! ratio alpha/beta
                  &                               - 0.203814e-03 ) * zt   &
                  &                               + 0.170907e-01 ) * zt   &
                  &   + 0.665157e-01                                      &
                  &   +     ( - 0.678662e-05 * zs                         &
                  &           - 0.846960e-04 * zt + 0.378110e-02 ) * zs   &
                  &   +   ( ( - 0.302285e-13 * zh                         &
                  &           - 0.251520e-11 * zs                         &
                  &           + 0.512857e-12 * zt * zt           ) * zh   &
                  &           - 0.164759e-06 * zs                         &
                  &        +(   0.791325e-08 * zt - 0.933746e-06 ) * zt   &
                  &                               + 0.380374e-04 ) * zh

               zbeta  = ( ( -0.415613e-09 * zt + 0.555579e-07 ) * zt      &   ! beta
                  &                            - 0.301985e-05 ) * zt      &
                  &   + 0.785567e-03                                      &
                  &   + (     0.515032e-08 * zs                           &
                  &         + 0.788212e-08 * zt - 0.356603e-06 ) * zs     &
                  &   +(  (   0.121551e-17 * zh                           &
                  &         - 0.602281e-15 * zs                           &
                  &         - 0.175379e-14 * zt + 0.176621e-12 ) * zh     &
                  &                             + 0.408195e-10   * zs     &
                  &     + ( - 0.213127e-11 * zt + 0.192867e-09 ) * zt     &
                  &                             - 0.121555e-07 ) * zh

               zthermal = zbeta * zalbet / ( rcp * zrhos + epsln )
               zhalin   = zbeta * tsn(ji,jj,1,jp_sal) * rcs
            ELSE
               zrhos    = rhop(ji,jj,1) + rau0 * ( 1. - tmask(ji,jj,1) )
               zthermal = rn_alpha / ( rcp * zrhos + epsln )
               zhalin   = rn_beta * tsn(ji,jj,1,jp_sal) * rcs
            ENDIF
            ! Radiative surface buoyancy force
            zBosol(ji,jj) = grav * zthermal * qsr(ji,jj)
            ! Non radiative surface buoyancy force
            zBo   (ji,jj) = grav * zthermal * qns(ji,jj) -  grav * zhalin * ( emps(ji,jj)-rnf(ji,jj) ) 
            ! Surface Temperature flux for non-local term
            wt0(ji,jj) = - ( qsr(ji,jj) + qns(ji,jj) )* ro0cpr * tmask(ji,jj,1)
            ! Surface salinity flux for non-local term
            ws0(ji,jj) = - ( ( emps(ji,jj)-rnf(ji,jj) ) * tsn(ji,jj,1,jp_sal) * rcs ) * tmask(ji,jj,1) 
         ENDDO
      ENDDO

      zflageos = 0.5 + SIGN( 0.5, nn_eos - 1. ) 
      !  Compute surface buoyancy forcing, Monin Obukhov and Ekman depths  
      !------------------------------------------------------------------    
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1
            !  Reference surface density = density at first T point level   
            zrhos         = rhop(ji,jj,1) + zflageos * rau0 * ( 1. - tmask(ji,jj,1) )  
            ! Friction velocity (zustar), at T-point : LMD94 eq. 2
            zustar(ji,jj) = SQRT( taum(ji,jj) / ( zrhos +  epsln ) )
         ENDDO
      ENDDO

!CDIR NOVERRCHK  
      !                                               ! ===============
      DO jj = 2, jpjm1                                 !  Vertical slab
         !                                             ! ===============
         
         !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         ! II Compute Boundary layer mixing coef. and diagnose the new boundary layer depth
         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         
         ! Initialization
         jkmax       = 0
         zdept (:,:) = 0.
         zdepw (:,:) = 0.
         zriblk(:,:) = 0.
         zmoek (:,:) = 0.
         zvisc (:,:) = 0.
         zdift (:,:) = 0.
#if defined key_zdfddm
         zdifs (:,:) = 0.
#endif
         zmask (:,:) = 0.
         DO ji = fs_2, fs_jpim1
            zria(ji ) = 0.
            ! Maximum boundary layer depth
            ikbot     = mbkt(ji,jj)    ! ikbot is the last T point in the water
            zhmax(ji) = fsdept(ji,jj,ikbot) - 0.001      
            ! Compute Monin obukhov length scale at the surface and Ekman depth:
            zbuofdep   = zBo(ji,jj) + zBosol(ji,jj) * ratt(1)
            zekman(ji) = rcekman * zustar(ji,jj) / ( ABS( ff(ji,jj) ) + epsln )
            zucube     = zustar(ji,jj) * zustar(ji,jj) * zustar(ji,jj) 
            zmoa(ji)   = zucube / ( vonk * ( zbuofdep + epsln ) )   
#if defined key_c1d
            ! store the surface buoyancy forcing
            zstabl        = 0.5 + SIGN( 0.5, zbuofdep )
            buof(ji,jj,1) = zbuofdep * tmask(ji,jj,1)
            ! store the moning-oboukov length scale at surface
            zmob          = zstabl * zmoa(ji) + ( 1.0 - zstabl ) * fsdept(ji,jj,1)
            mols(ji,jj,1) = MIN( zmob , zhmax(ji) ) * tmask(ji,jj,1)
            ! store Ekman depth
            zek           = zstabl * zekman(ji) + ( 1.0 - zstabl ) * fsdept(ji,jj,1)  
            ekdp(ji,jj )  = MIN( zek , zhmax(ji) ) * tmask(ji,jj,1)  
#endif 
         END DO     
         ! Compute the pipe
         ! ---------------------
         DO jk = 2, jpkm1
            DO ji = fs_2, fs_jpim1
               ! Compute bfsfc = Bo + radiative contribution down to hbf*depht
               zbuofdep = zBo(ji,jj) + zBosol(ji,jj) * ratt(jk)
               ! Flag (zstabl  = 1) if positive forcing
               zstabl   =  0.5 + SIGN(  0.5, zbuofdep)

               !   Compute bulk richardson number zrib at depht 
               !-------------------------------------------------------
               !                           [Br - B(d)] * d         zrinum
               !             Rib(z) = ----------------------- = -------------
               !                       |Vr - V(d)|^2 + Vt(d)^2   zdVsq + zVtsq
               !
               ! First compute zt,zs,zu,zv = means in the surface layer < epsilon*depht  
               ! Else surface values are taken at the first T level.
               ! For stability, resolved vertical shear is computed with "before velocities".
               zref = epsilon * fsdept(ji,jj,jk)
#if defined key_kppcustom
               ! zref = gdept(1)
               zref = fsdept(ji,jj,1)
               zt   = tsn(ji,jj,1,jp_tem)
               zs   = tsn(ji,jj,1,jp_sal)
               zrh  = rhop(ji,jj,1)
               zu   = ( ub(ji,jj,1) + ub(ji - 1,jj    ,1) ) / MAX( 1. , umask(ji,jj,1) + umask(ji - 1,jj   ,1) )
               zv   = ( vb(ji,jj,1) + vb(ji    ,jj - 1,1) ) / MAX( 1. , vmask(ji,jj,1) + vmask(ji   ,jj - 1,1) )
#else
               zt   = 0.
               zs   = 0.
               zu   = 0.
               zv   = 0.
               zrh  = 0.
               ! vertically integration over the upper epsilon*gdept(jk) ; del () array is computed once in zdf_kpp_init
               DO jm = 1, jpkm1
                  zt   = zt  + del(jk,jm) * tsn(ji,jj,jm,jp_tem)
                  zs   = zs  + del(jk,jm) * tsn(ji,jj,jm,jp_sal)
                  zu   = zu  + 0.5 * del(jk,jm) &
                     &            * ( ub(ji,jj,jm) + ub(ji - 1,jj,jm) ) &
                     &            / MAX( 1. , umask(ji,jj,jm) + umask(ji - 1,jj,jm) )
                  zv   = zv  + 0.5 * del(jk,jm) &
                     &            * ( vb(ji,jj,jm) + vb(ji,jj - 1,jm) ) &
                     &            / MAX( 1. , vmask(ji,jj,jm) + vmask(ji,jj - 1,jm) )
                  zrh  = zrh + del(jk,jm) * rhop(ji,jj,jm)
               END DO
#endif
               zsr = SQRT( ABS( tsn(ji,jj,jk,jp_sal) ) )
               ! depth
               zh = fsdept(ji,jj,jk)
               ! compute compression terms on density
               ze  = ( -3.508914e-8*zt-1.248266e-8 ) *zt-2.595994e-6
               zbw = (  1.296821e-6*zt-5.782165e-9 ) *zt+1.045941e-4
               zb  = zbw + ze * zs
               
               zd  = -2.042967e-2
               zc  =   (-7.267926e-5*zt+2.598241e-3 ) *zt+0.1571896
               zaw = ( ( 5.939910e-6*zt+2.512549e-3 ) *zt-0.1028859 ) *zt - 4.721788
               za  = ( zd*zsr + zc ) *zs + zaw
               
               zb1 =   (-0.1909078*zt+7.390729 ) *zt-55.87545
               za1 = ( ( 2.326469e-3*zt+1.553190)*zt-65.00517 ) *zt+1044.077
               zkw = ( ( (-1.361629e-4*zt-1.852732e-2 ) *zt-30.41638 ) *zt + 2098.925 ) *zt+190925.6
               zk0 = ( zb1*zsr + za1 )*zs + zkw
               zcomp =   1.0 - zh / ( zk0 - zh * ( za - zh * zb ) )
               
#if defined key_kppcustom
               ! potential density of water(zrh = zt,zs at level jk):
               zrhdr = zrh / zcomp
#else
               ! potential density of water(ztref,zsref at level jk):
               ! compute volumic mass pure water at atm pressure
               IF ( nn_eos < 1 ) THEN
                  zr1= ( ( ( ( 6.536332e-9*zt-1.120083e-6 )*zt+1.001685e-4)*zt   &
                     &       -9.095290e-3 )*zt+6.793952e-2 )*zt+999.842594
                  ! seawater volumic mass atm pressure
                  zr2= ( ( ( 5.3875e-9*zt-8.2467e-7 ) *zt+7.6438e-5 ) *zt   &
                     &   -4.0899e-3 ) *zt+0.824493
                  zr3= ( -1.6546e-6*zt+1.0227e-4 ) *zt-5.72466e-3
                  zr4= 4.8314e-4              
                  ! potential volumic mass (reference to the surface)
                  zrhop= ( zr4*zs + zr3*zsr + zr2 ) *zs + zr1                 
                  zrhdr = zrhop / zcomp
               ELSE
                  zrhdr = zrh / zcomp
               ENDIF
#endif
               
               ! potential density of ambiant water at level jk :
               zrhd   = ( rhd(ji,jj,jk) * rau0 + rau0 )  
               
               ! And now the Rib number numerator .
               zrinum = grav * ( zrhd - zrhdr ) / rau0
               zrinum = zrinum * ( fsdept(ji,jj,jk) - zref ) * tmask(ji,jj,jk)
           
               ! Resolved shear contribution to Rib at depth T-point (zdVsq)
               ztx    =   ( ub( ji , jj ,jk)   +  ub(ji - 1, jj ,jk) ) &
                  &     / MAX( 1. , umask( ji , jj ,jk) + umask(ji - 1, jj ,jk) )   
               zty    =   ( vb( ji , jj ,jk)   +  vb(ji  ,jj - 1,jk) ) &
                  &     / MAX( 1., vmask( ji , jj ,jk) + vmask(ji  ,jj - 1,jk) ) 
               
               zdVsq  = ( zu - ztx ) * ( zu - ztx ) + ( zv - zty ) * ( zv - zty )
               
               ! Scalar turbulent velocity scale zws for hbl=gdept
               zscale = zstabl + ( 1.0 - zstabl ) * epsilon
               zehat  = vonk * zscale * fsdept(ji,jj,jk) * zbuofdep
               zucube = zustar(ji,jj) * zustar(ji,jj) * zustar(ji,jj)              
               zeta   = zehat / ( zucube + epsln )
               
               IF( zehat > 0. ) THEN
                  ! Stable case
                  zws  = vonk * zustar(ji,jj) / ( 1.0 + rconc1 * zeta )
               ELSE
                  ! Unstable case
#if defined key_kpplktb
                  ! use lookup table
                  zd     = zehat - dehatmin
                  il     = INT( zd / dezehat )
                  il     = MIN( il, nilktbm1 )
                  il     = MAX( il, 1 )
                  
                  ud     = zustar(ji,jj) - ustmin
                  jl     = INT( ud / deustar )
                  jl     = MIN( jl, njlktbm1 )
                  jl     = MAX( jl, 1 )
                  
                  zfrac  = zd / dezehat - FLOAT( il )  
                  ufrac  = ud / deustar - FLOAT( jl )
                  zwas   = ( 1. - zfrac ) * wslktb(il,jl+1) + zfrac * wslktb(il+1,jl+1)
                  zwbs   = ( 1. - zfrac ) * wslktb(il,jl  ) + zfrac * wslktb(il+1,jl  )
                  !
                  zws    = ( 1. - ufrac ) * zwbs + ufrac * zwas
#else
                  ! use analytical functions:
                  zcons  = 0.5 + SIGN( 0.5 , ( rzetas - zeta ) )
                  zwcons = vonk * zustar(ji,jj) * ( ( ABS( rconas - rconcs * zeta ) )**pthird ) 
                  zwsun  = vonk * zustar(ji,jj) * SQRT( ABS ( 1.0 - rconc2 * zeta ) )
                  !
                  zws    = zcons * zwcons +  ( 1.0 - zcons) * zwsun
#endif
               ENDIF
               
               ! Turbulent shear contribution to Rib (zVtsq) bv frequency at levels  ( ie T-point jk)
               zrn2   = 0.5 * ( rn2(ji,jj,jk) + rn2(ji,jj,jk+1) )   
               zbvzed = SQRT( ABS( zrn2 ) ) 
               zVtsq  = fsdept(ji,jj,jk) * zws * zbvzed  * Vtc
               
               ! Finally, the bulk Richardson number at depth fsdept(i,j,k) 
               zrib  = zrinum   / ( zdVsq + zVtsq + epsln )
 
               ! Find subscripts around the boundary layer depth, build the pipe
               ! ----------------------------------------------------------------

               ! Flag (zflagri = 1) if zrib < Ricr  
               zflagri = 0.5 + SIGN( 0.5, ( Ricr - zrib ) )
               !  Flag (zflagh  = 1) if still within overall boundary layer
               zflagh  = 0.5 + SIGN( 0.5, ( fsdept(ji,jj,1) - zdept(ji,2) ) )
               
               ! Ekman layer depth
               zek     = zstabl * zekman(ji) + ( 1.0 - zstabl ) * zhmax(ji)
               zflag   = 0.5 + SIGN( 0.5, ( zek - fsdept(ji,jj,jk-1) ) )
               zek     = zflag * zek + ( 1.0 - zflag ) * zhmax(ji)
               zflagek = 0.5 + SIGN( 0.5, ( zek - fsdept(ji,jj,jk) ) )
               ! Flag (zflagmo = 1) if still within stable Monin-Obukhov and in water
               zmob    = zucube / ( vonk * ( zbuofdep + epsln ) )  
               ztemp   = zstabl * zmob + ( 1.0 - zstabl) * zhmax(ji)
               ztemp   = MIN( ztemp , zhmax(ji) ) 
               zflagmo = 0.5 + SIGN( 0.5, ( ztemp - fsdept(ji,jj,jk) ) )             

               ! No limitation by Monin Obukhov or Ekman depths:
!               zflagek = 1.0
!               zflagmo = 0.5 + SIGN( 0.5, ( zhmax(ji) - fsdept(ji,jj,jk) ) )

               ! Load  pipe via zflagkb  for later calculations
               ! Flag (zflagkb = 1) if zflagh = 1 and (zflagri = 0 or zflagek = 0 or zflagmo = 0)
               zflagkb = zflagh * ( 1.0 - ( zflagri * zflagek * zflagmo ) )

               zmask(ji,jk) = zflagh
               jkp2         = MIN( jk+2 , ikbot )
               jkm1         = MAX( jk-1 , 2 )
               jkmax        = MAX( jkmax, jk * INT( REAL( zflagh+epsln ) ) ) 

               zdept(ji,1)  = zdept(ji,1) + zflagkb * fsdept(ji,jj,jk-1) 
               zdept(ji,2)  = zdept(ji,2) + zflagkb * fsdept(ji,jj,jk  ) 
               zdept(ji,3)  = zdept(ji,3) + zflagkb * fsdept(ji,jj,jk+1) 

               zdepw(ji,1)  = zdepw(ji,1) + zflagkb * fsdepw(ji,jj,jk-1) 
               zdepw(ji,2)  = zdepw(ji,2) + zflagkb * fsdepw(ji,jj,jk  ) 
               zdepw(ji,3)  = zdepw(ji,3) + zflagkb * fsdepw(ji,jj,jk+1)
               zdepw(ji,4)  = zdepw(ji,4) + zflagkb * fsdepw(ji,jj,jkp2)  

               zriblk(ji,1) = zriblk(ji,1) + zflagkb * zria(ji)
               zriblk(ji,2) = zriblk(ji,2) + zflagkb * zrib

               zmoek (ji,0) = zmoek (ji,0) + zflagkb * zek
               zmoek (ji,1) = zmoek (ji,1) + zflagkb * zmoa(ji)
               zmoek (ji,2) = zmoek (ji,2) + zflagkb * ztemp  
               ! Save Monin Obukhov depth
               zmoa  (ji)   = zmob
           
               zvisc(ji,1) = zvisc(ji,1) + zflagkb * avmu(ji,jj,jkm1)
               zvisc(ji,2) = zvisc(ji,2) + zflagkb * avmu(ji,jj,jk  )
               zvisc(ji,3) = zvisc(ji,3) + zflagkb * avmu(ji,jj,jk+1)
               zvisc(ji,4) = zvisc(ji,4) + zflagkb * avmu(ji,jj,jkp2)
               
               zdift(ji,1) = zdift(ji,1) + zflagkb * avt (ji,jj,jkm1)
               zdift(ji,2) = zdift(ji,2) + zflagkb * avt (ji,jj,jk  )
               zdift(ji,3) = zdift(ji,3) + zflagkb * avt (ji,jj,jk+1)
               zdift(ji,4) = zdift(ji,4) + zflagkb * avt (ji,jj,jkp2)

#if defined key_zdfddm
               zdifs(ji,1) = zdifs(ji,1) + zflagkb * avs (ji,jj,jkm1)
               zdifs(ji,2) = zdifs(ji,2) + zflagkb * avs (ji,jj,jk  )
               zdifs(ji,3) = zdifs(ji,3) + zflagkb * avs (ji,jj,jk+1)
               zdifs(ji,4) = zdifs(ji,4) + zflagkb * avs (ji,jj,jkp2)
#endif               
               ! Save the Richardson number 
               zria  (ji)   = zrib  
#if defined key_c1d
               ! store buoyancy length scale
               buof(ji,jj,jk) = zbuofdep * tmask(ji,jj,jk) 
               ! store Monin Obukhov
               zmob           = zstabl * zmob + ( 1.0 - zstabl) * fsdept(ji,jj,1)
               mols(ji,jj,jk) = MIN( zmob , zhmax(ji) ) * tmask(ji,jj,jk) 
               ! Bulk Richardson number
               rib(ji,jj,jk)  = zrib * tmask(ji,jj,jk)             
#endif               
            END DO
         END DO
         !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         ! III PROCESS THE PIPE
         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         
         DO ji = fs_2, fs_jpim1 
            
            ! Find the boundary layer depth zhbl
            ! ----------------------------------------
            
            ! Interpolate monin Obukhov and critical Ri mumber depths   
            ztemp = zdept(ji,2) - zdept(ji,1)
            zflag = ( Ricr - zriblk(ji,1) ) / ( zriblk(ji,2) - zriblk(ji,1)  + epsln )
            zhrib = zdept(ji,1) + zflag * ztemp      

            IF( zriblk(ji,2) < Ricr ) zhrib = zhmax(ji) 
         
            IF( zmoek(ji,2) < zdept(ji,2) ) THEN
               IF ( zmoek(ji,1) < 0. ) THEN
                  zmob = zdept(ji,2) - epsln
               ELSE
                  zmob = ztemp + zmoek(ji,1) - zmoek(ji,2)
                  zmob = ( zmoek(ji,1) * zdept(ji,2) - zmoek(ji,2) * zdept(ji,1) ) / zmob
                  zmob = MAX( zmob , zdept(ji,1) + epsln )                
               ENDIF
            ELSE           
               zmob = zhmax(ji) 
            ENDIF
            ztemp   = MIN( zmob , zmoek(ji,0) )
                         
            ! Finally, the boundary layer depth, zhbl 
            zhbl(ji) = MAX( fsdept(ji,jj,1) + epsln, MIN( zhrib , ztemp ) )
            
            ! Save hkpp for further diagnostics (optional)
            hkpp(ji,jj) = zhbl(ji) * tmask(ji,jj,1) 
          
            ! Correct mask if zhbl < fsdepw(ji,jj,2) for no viscosity/diffusivity enhancement at fsdepw(ji,jj,2)
            !     zflag = 1 if zhbl(ji) > fsdepw(ji,jj,2)
            IF( zhbl(ji) < fsdepw(ji,jj,2) ) zmask(ji,2) = 0.
          
            
            !  Velocity scales at depth zhbl
            ! -----------------------------------
            
            !  Compute bouyancy forcing down to zhbl
            ztemp    = -hbf * zhbl(ji)
            zatt1    = 1.0 - ( rabs * EXP( ztemp / xsi1 ) + ( 1.0 - rabs ) * EXP( ztemp / xsi2 ) )
            zbuofdep = zBo(ji,jj) + zBosol(ji,jj) * zatt1
            zstabl   = 0.5 + SIGN( 0.5 , zbuofdep ) 

            zbuofdep = zbuofdep + zstabl * epsln

            zscale = zstabl + ( 1.0 - zstabl ) * epsilon          
            zehat  = vonk * zscale * zhbl(ji) * zbuofdep
            zucube = zustar(ji,jj) * zustar(ji,jj) * zustar(ji,jj)              
            zeta   = zehat / ( zucube + epsln )
            
            IF( zehat > 0. ) THEN
               ! Stable case
               zws  = vonk * zustar(ji,jj) / ( 1.0 + rconc1 * zeta )
               zwm  = zws
            ELSE
               ! Unstable case
#if defined key_kpplktb
               ! use lookup table
               zd     = zehat - dehatmin
               il     = INT( zd / dezehat )
               il     = MIN( il, nilktbm1 )
               il     = MAX( il, 1 )
               
               ud     = zustar(ji,jj) - ustmin
               jl     = INT( ud / deustar )
               jl     = MIN( jl, njlktbm1 )
               jl     = MAX( jl, 1 )
               
               zfrac  = zd / dezehat - FLOAT( il )  
               ufrac  = ud / deustar - FLOAT( jl )
               zwas   = ( 1. - zfrac ) * wslktb(il,jl+1) + zfrac * wslktb(il+1,jl+1)
               zwbs   = ( 1. - zfrac ) * wslktb(il,jl  ) + zfrac * wslktb(il+1,jl  )
               zwam   = ( 1. - zfrac ) * wmlktb(il,jl+1) + zfrac * wmlktb(il+1,jl+1)
               zwbm   = ( 1. - zfrac ) * wmlktb(il,jl  ) + zfrac * wmlktb(il+1,jl  )
               !
               zws    = ( 1. - ufrac ) * zwbs + ufrac * zwas
               zwm    = ( 1. - ufrac ) * zwbm + ufrac * zwam
#else
               ! use analytical functions
               zconm  = 0.5 + SIGN( 0.5, ( rzetam - zeta) )
               zcons  = 0.5 + SIGN( 0.5, ( rzetas - zeta) )
               
               ! Momentum : zeta < rzetam (zconm = 1)
               ! Scalars  : zeta < rzetas (zcons = 1) 
               zwconm = zustar(ji,jj) * vonk * ( ( ABS( rconam - rconcm * zeta) )**pthird )
               zwcons = zustar(ji,jj) * vonk * ( ( ABS( rconas - rconcs * zeta) )**pthird )
               
               ! Momentum : rzetam <= zeta < 0 (zconm = 0)
               ! Scalars  : rzetas <= zeta < 0 (zcons = 0)	
               zwmun  = SQRT( ABS( 1.0 - rconc2 * zeta ) )
               zwsun  = vonk * zustar(ji,jj) * zwmun
               zwmun  = vonk * zustar(ji,jj) * SQRT(zwmun)
               !
               zwm    = zconm * zwconm + ( 1.0 - zconm ) * zwmun
               zws    = zcons * zwcons + ( 1.0 - zcons ) * zwsun
               
#endif
            ENDIF
            
            
            ! Viscosity, diffusivity values and derivatives at h
            ! --------------------------------------------------------
            
            ! check between at which interfaces is located zhbl(ji)
            ! ztemp = 1, zdepw(ji,2) < zhbl <  zdepw(ji,3)
            ! ztemp = 0, zdepw(ji,1) < zhbl <  zdepw(ji,2)
            ztemp  =  0.5 + SIGN( 0.5, ( zhbl(ji) - zdepw(ji,2) ) )  
            zdep21 =   zdepw(ji,2) - zdepw(ji,1) + epsln
            zdep32 =   zdepw(ji,3) - zdepw(ji,2) + epsln
            zdep43 =   zdepw(ji,4) - zdepw(ji,3) + epsln  
            
            ! Compute R as in LMD94, eq D5b
            zdelta =  ( zhbl(ji) - zdepw(ji,2) ) *         ztemp   / zdep32   &
               &    + ( zhbl(ji) - zdepw(ji,1) ) * ( 1.0 - ztemp ) / zdep21 
            
            ! Compute the vertical derivative of viscosities (zdzh) at z=zhbl(ji)
            zdzup  =  ( zvisc(ji,2) - zvisc(ji,3) ) *         ztemp   / zdep32 &
               &    + ( zvisc(ji,1) - zvisc(ji,2) ) * ( 1.0 - ztemp ) / zdep21
            
            zdzdn  =  ( zvisc(ji,3) - zvisc(ji,4) ) *         ztemp   / zdep43 &
               &    + ( zvisc(ji,2) - zvisc(ji,3) ) * ( 1.0 - ztemp ) / zdep32
            
            ! LMD94, eq D5b :          
            zdzh   = ( 1.0 - zdelta ) * zdzup + zdelta * zdzdn
            zdzh   = MAX( zdzh , 0. )
            
            ! Compute viscosities (zvath) at z=zhbl(ji), LMD94 eq D5a
            zvath  =          ztemp   * ( zvisc(ji,3) + zdzh * ( zdepw(ji,3) - zhbl(ji) ) ) &
               &    + ( 1.0 - ztemp ) * ( zvisc(ji,2) + zdzh * ( zdepw(ji,2) - zhbl(ji) ) )
            
            ! Compute G (zgat1) and its derivative (zdat1) at z=hbl(ji), LMD94 eq 18
            
            ! Vertical derivative of velocity scale divided by velocity scale squared at z=hbl(ji) 
            ! (non zero only in stable conditions)
            zflag  =  -zstabl * rconc1 * zbuofdep / ( zucube * zustar(ji,jj) + epsln )
            
            ! G at its derivative at z=hbl:
            zgat1  = zvath  / ( zhbl(ji) * ( zwm + epsln )  )
            zdat1  = -zdzh  / ( zwm + epsln ) -  zflag * zvath / zhbl(ji)
            
            ! G coefficients, LMD94 eq 17
            za2m(ji) = -2.0 + 3.0 * zgat1 - zdat1
            za3m(ji) =  1.0 - 2.0 * zgat1 + zdat1

            
            ! Compute the vertical derivative of temperature diffusivities (zdzh) at z=zhbl(ji)
            zdzup  =  ( zdift(ji,2) - zdift(ji,3) ) *         ztemp   / zdep32 &
               &    + ( zdift(ji,1) - zdift(ji,2) ) * ( 1.0 - ztemp ) / zdep21
            
            zdzdn  =  ( zdift(ji,3) - zdift(ji,4) ) *         ztemp   / zdep43 &
               &    + ( zdift(ji,2) - zdift(ji,3) ) * ( 1.0 - ztemp ) / zdep32
            
            ! LMD94, eq D5b :          
            zdzh   = ( 1.0 - zdelta ) * zdzup + zdelta * zdzdn
            zdzh   = MAX( zdzh , 0. )
            
            
            ! Compute diffusivities (zvath) at z=zhbl(ji), LMD94 eq D5a
            zvath  =          ztemp   * ( zdift(ji,3) + zdzh * ( zdepw(ji,3) - zhbl(ji) ) ) &
               &    + ( 1.0 - ztemp ) * ( zdift(ji,2) + zdzh * ( zdepw(ji,2) - zhbl(ji) ) )
                        
            ! G at its derivative at z=hbl:
            zgat1  = zvath  / ( zhbl(ji) * ( zws + epsln )  )
            zdat1  = -zdzh  / ( zws + epsln ) -  zflag * zvath / zhbl(ji)
            
            ! G coefficients, LMD94 eq 17
            za2t(ji) = -2.0 + 3.0 * zgat1 - zdat1
            za3t(ji) =  1.0 - 2.0 * zgat1 + zdat1

#if defined key_zdfddm
            ! Compute the vertical derivative of salinities diffusivities (zdzh) at z=zhbl(ji)
            zdzup  =  ( zdifs(ji,2) - zdifs(ji,3) ) *         ztemp   / zdep32 &
               &    + ( zdifs(ji,1) - zdifs(ji,2) ) * ( 1.0 - ztemp ) / zdep21
            
            zdzdn  =  ( zdifs(ji,3) - zdifs(ji,4) ) *         ztemp   / zdep43 &
               &    + ( zdifs(ji,2) - zdifs(ji,3) ) * ( 1.0 - ztemp ) / zdep32
            
            ! LMD94, eq D5b :          
            zdzh   = ( 1.0 - zdelta ) * zdzup + zdelta * zdzdn
            zdzh   = MAX( zdzh , 0. )           
            
            ! Compute diffusivities (zvath) at z=zhbl(ji), LMD94 eq D5a
            zvath  =          ztemp   * ( zdifs(ji,3) + zdzh * ( zdepw(ji,3) - zhbl(ji) ) ) &
               &    + ( 1.0 - ztemp ) * ( zdifs(ji,2) + zdzh * ( zdepw(ji,2) - zhbl(ji) ) )
                        
            ! G at its derivative at z=hbl:
            zgat1  = zvath  / ( zhbl(ji) * ( zws + epsln )  )
            zdat1  = -zdzh  / ( zws + epsln ) -  zflag * zvath / zhbl(ji)
            
            ! G coefficients, LMD94 eq 17
            za2s(ji) = -2.0 + 3.0 * zgat1 - zdat1
            za3s(ji) =  1.0 - 2.0 * zgat1 + zdat1
#endif

            !-------------------turn off interior matching here------
            !          za2(ji,1) = -2.0
            !          za3(ji,1) =  1.0
            !          za2(ji,2) = -2.0
            !          za3(ji,2) =  1.0
            !--------------------------------------------------------
            
            !  Compute Enhanced Mixing Coefficients (LMD94,eq D6)
            ! ---------------------------------------------------------------
            
            ! Delta 
            zdelta  = ( zhbl(ji)  - zdept(ji,1) ) / ( zdept(ji,2) - zdept(ji,1) + epsln )
            zdelta2 = zdelta * zdelta
            
            !  Mixing coefficients at first level above h (zdept(ji,1)) 
            ! and at first interface in the pipe (zdepw(ji,2))
            
            ! At first T level above h (zdept(ji,1)) (always in the boundary layer)
            zsig    = zdept(ji,1) / zhbl(ji)
            ztemp   = zstabl * zsig  + ( 1.0 - zstabl ) * MIN( zsig , epsilon )
            zehat   = vonk * ztemp * zhbl(ji) * zbuofdep
            zeta    = zehat / ( zucube + epsln)
            zwst    = vonk * zustar(ji,jj) / ( ABS( 1.0 + rconc1 * zeta ) + epsln)
            zwm     = zstabl * zwst + ( 1.0 - zstabl ) * zwm
            zws     = zstabl * zwst + ( 1.0 - zstabl ) * zws

            zkm1m  = zhbl(ji) * zwm * zsig * ( 1.0 + zsig * ( za2m(ji) + zsig * za3m(ji) ) )
            zkm1t  = zhbl(ji) * zws * zsig * ( 1.0 + zsig * ( za2t(ji) + zsig * za3t(ji) ) )
#if defined key_zdfddm
            zkm1s  = zhbl(ji) * zws * zsig * ( 1.0 + zsig * ( za2s(ji) + zsig * za3s(ji) ) )
#endif                        
            ! At first W level in the pipe (zdepw(ji,2)) (not always in the boundary layer ):
            zsig    = MIN( zdepw(ji,2) / zhbl(ji) , 1.0 )
            ztemp   = zstabl * zsig + ( 1.0 - zstabl ) * MIN( zsig , epsilon )
            zehat   = vonk * ztemp * zhbl(ji) * zbuofdep
            zeta    = zehat / ( zucube + epsln )
            zwst    = vonk * zustar(ji,jj) / ( ABS( 1.0 + rconc1 * zeta ) + epsln)
            zws     = zstabl * zws + ( 1.0 - zstabl ) * zws
            zwm     = zstabl * zws + ( 1.0 - zstabl ) * zwm

            zkmpm(ji) = zhbl(ji) * zwm * zsig * ( 1.0 + zsig * ( za2m(ji) + zsig * za3m(ji) ) )
            zkmpt(ji) = zhbl(ji) * zws * zsig * ( 1.0 + zsig * ( za2t(ji) + zsig * za3t(ji) ) )
#if defined key_zdfddm
            zkmps(ji) = zhbl(ji) * zws * zsig * ( 1.0 + zsig * ( za2s(ji) + zsig * za3s(ji) ) )
#endif  
      
            ! check if this point is in the boundary layer,else take interior viscosity/diffusivity:
            zflag       = 0.5 + SIGN( 0.5, ( zhbl(ji) - zdepw(ji,2) ) )
            zkmpm(ji) = zkmpm(ji) * zflag + ( 1.0 - zflag ) * zvisc(ji,2)
            zkmpt(ji) = zkmpt(ji) * zflag + ( 1.0 - zflag ) * zdift(ji,2)
#if defined key_zdfddm
            zkmps(ji) = zkmps(ji) * zflag + ( 1.0 - zflag ) * zdifs(ji,2)
#endif

            ! Enhanced viscosity/diffusivity at zdepw(ji,2)
            ztemp     = ( 1.0 - 2.0 * zdelta + zdelta2 ) * zkm1m + zdelta2 * zkmpm(ji)
            zkmpm(ji) = ( 1.0 - zdelta ) * zvisc(ji,2) + zdelta * ztemp
            ztemp     = ( 1.0 - 2.0 * zdelta + zdelta2 ) * zkm1t + zdelta2 * zkmpt(ji)
            zkmpt(ji) = ( 1.0 - zdelta ) * zdift(ji,2) + zdelta * ztemp
#if defined key_zdfddm
            ztemp     = ( 1.0 - 2.0 * zdelta + zdelta2 ) * zkm1s + zdelta2 * zkmps(ji)
            zkmps(ji) = ( 1.0 - zdelta ) * zdifs(ji,2) + zdelta * ztemp
#endif            

         END DO
         !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         ! IV. Compute vertical eddy viscosity and diffusivity coefficients
         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         
         DO jk  = 2, jkmax
            
            ! Compute turbulent velocity scales on the interfaces
            ! --------------------------------------------------------
            DO  ji = fs_2, fs_jpim1
               zbuofdep = zBo(ji,jj) + zBosol(ji,jj) * zatt1
               zstabl   = 0.5 + SIGN( 0.5 , zbuofdep ) 
               zbuofdep = zbuofdep + zstabl * epsln          
               zsig    = fsdepw(ji,jj,jk) / zhbl(ji)
               ztemp   = zstabl * zsig + ( 1. - zstabl ) * MIN( zsig , epsilon )
               zehat   = vonk * ztemp * zhbl(ji) * zbuofdep
               zucube  = zustar(ji,jj) * zustar(ji,jj) * zustar(ji,jj)
               zeta    = zehat / ( zucube + epsln )

               IF( zehat > 0. ) THEN
                  ! Stable case
                  zws  = vonk * zustar(ji,jj) / ( 1.0 + rconc1 * zeta )
                  zwm  = zws
               ELSE
                  ! Unstable case
#if defined key_kpplktb
                  ! use lookup table
                  zd     = zehat - dehatmin
                  il     = INT( zd / dezehat )
                  il     = MIN( il, nilktbm1 )
                  il     = MAX( il, 1 )
                  
                  ud     = zustar(ji,jj) - ustmin
                  jl     = INT( ud / deustar )
                  jl     = MIN( jl, njlktbm1 )
                  jl     = MAX( jl, 1 )
                  
                  zfrac  = zd / dezehat - FLOAT( il )  
                  ufrac  = ud / deustar - FLOAT( jl )
                  zwas   = ( 1. - zfrac ) * wslktb(il,jl+1) + zfrac * wslktb(il+1,jl+1)
                  zwbs   = ( 1. - zfrac ) * wslktb(il,jl  ) + zfrac * wslktb(il+1,jl  )
                  zwam   = ( 1. - zfrac ) * wmlktb(il,jl+1) + zfrac * wmlktb(il+1,jl+1)
                  zwbm   = ( 1. - zfrac ) * wmlktb(il,jl  ) + zfrac * wmlktb(il+1,jl  )
                  !
                  zws    = ( 1. - ufrac ) * zwbs + ufrac * zwas
                  zwm    = ( 1. - ufrac ) * zwbm + ufrac * zwam
#else
                  ! use analytical functions
                  zconm  = 0.5 + SIGN( 0.5, ( rzetam - zeta) )
                  zcons  = 0.5 + SIGN( 0.5, ( rzetas - zeta) )
                  
                  ! Momentum : zeta < rzetam (zconm = 1)
                  ! Scalars  : zeta < rzetas (zcons = 1) 
                  zwconm = zustar(ji,jj) * vonk * ( ( ABS( rconam - rconcm * zeta) )**pthird )
                  zwcons = zustar(ji,jj) * vonk * ( ( ABS( rconas - rconcs * zeta) )**pthird )
                  
                  ! Momentum : rzetam <= zeta < 0 (zconm = 0)
                  ! Scalars  : rzetas <= zeta < 0 (zcons = 0)	
                  zwmun  = SQRT( ABS( 1.0 - rconc2 * zeta ) )
                  zwsun  = vonk * zustar(ji,jj) * zwmun
                  zwmun  = vonk * zustar(ji,jj) * SQRT(zwmun)
                  !
                  zwm    = zconm * zwconm + ( 1.0 - zconm ) * zwmun
                  zws    = zcons * zwcons + ( 1.0 - zcons ) * zwsun
                  
#endif
               ENDIF
               
               zblcm(ji,jk) = zhbl(ji) * zwm * zsig  * ( 1.0 + zsig * ( za2m(ji) + zsig * za3m(ji) ) )
               zblct(ji,jk) = zhbl(ji) * zws * zsig  * ( 1.0 + zsig * ( za2t(ji) + zsig * za3t(ji) ) )
#if defined key_zdfddm
               zblcs(ji,jk) = zhbl(ji) * zws * zsig  * ( 1.0 + zsig * ( za2s(ji) + zsig * za3s(ji) ) )
#endif              
               !  Compute Nonlocal transport term = ghats * <ws>o
               ! ----------------------------------------------------
               ghats(ji,jj,jk-1) = ( 1. - zstabl ) * rcg / ( zws * zhbl(ji) + epsln ) * tmask(ji,jj,jk)

            END DO
         END DO     
         !  Combine interior and boundary layer coefficients and nonlocal term
         ! -----------------------------------------------------------------------
         DO jk = 2, jpkm1   
            DO ji = fs_2, fs_jpim1
               zflag = zmask(ji,jk) * zmask(ji,jk+1)
               zviscos(ji,jj,jk) = ( 1.0 - zmask(ji,jk) )         * avmu (ji,jj,jk) & ! interior viscosities
                  &              +                        zflag   * zblcm(ji,jk    ) & ! boundary layer viscosities
                  &              + zmask(ji,jk) * ( 1.0 - zflag ) * zkmpm(ji       )   ! viscosity enhancement at W_level near zhbl
               
               zviscos(ji,jj,jk) = zviscos(ji,jj,jk) * tmask(ji,jj,jk)   

            
               zdiffut(ji,jj,jk) = ( 1.0 - zmask(ji,jk) )          * avt (ji,jj,jk) & ! interior diffusivities 
                  &              +                        zflag   * zblct(ji,jk   ) & ! boundary layer diffusivities
                  &              + zmask(ji,jk) * ( 1.0 - zflag ) * zkmpt(ji      )   ! diffusivity enhancement at W_level near zhbl
                       
               zdiffut(ji,jj,jk) = zdiffut(ji,jj,jk) * tmask(ji,jj,jk) 
#if defined key_zdfddm
               zdiffus(ji,jj,jk) = ( 1.0 - zmask(ji,jk) )          * avs (ji,jj,jk) & ! interior diffusivities 
                  &              +                        zflag   * zblcs(ji,jk   ) & ! boundary layer diffusivities
                  &              + zmask(ji,jk) * ( 1.0 - zflag ) * zkmps(ji      )   ! diffusivity enhancement at W_level near zhbl
                       
               zdiffus(ji,jj,jk) = zdiffus(ji,jj,jk) * tmask(ji,jj,jk) 
#endif               
               ! Non local flux in the boundary layer only
               ghats(ji,jj,jk-1) = zmask(ji,jk) * ghats(ji,jj,jk-1)

            ENDDO
         END DO
         !                                             ! ===============
      END DO                                           !   End of slab
      !                                                ! ===============

      ! Lateral boundary conditions on zvicos and zdiffus  (sign unchanged)
      CALL lbc_lnk( zviscos(:,:,:), 'U', 1. )  ; CALL lbc_lnk( zdiffut(:,:,:), 'W', 1. )  
#if defined key_zdfddm  
      CALL lbc_lnk( zdiffus(:,:,:), 'W', 1. ) 
#endif

      SELECT CASE ( nn_ave )
         !
         CASE ( 0 )             ! no viscosity and diffusivity smoothing

            DO jk = 2, jpkm1
               DO jj = 2, jpjm1
                  DO ji = fs_2, fs_jpim1
                     avmu(ji,jj,jk) = ( zviscos(ji,jj,jk) + zviscos(ji+1,jj,jk) ) &
                        &  / MAX( 1., tmask(ji,jj,jk) + tmask (ji + 1,jj,jk) ) * umask(ji,jj,jk)
                     
                     avmv(ji,jj,jk) = ( zviscos(ji,jj,jk) + zviscos(ji,jj+1,jk) ) &
                        &  / MAX( 1., tmask(ji,jj,jk) + tmask (ji,jj+1,jk) ) * vmask(ji,jj,jk)
                     
                     avt (ji,jj,jk) =  zdiffut(ji,jj,jk) * tmask(ji,jj,jk)  
#if defined key_zdfddm     
                     avs (ji,jj,jk) =  zdiffus(ji,jj,jk) * tmask(ji,jj,jk)  
#endif
                  END DO
               END DO
            END DO
            
         CASE ( 1 )                ! viscosity and diffusivity smoothing
            !                      
            !           ( 1/2  1  1/2 )              ( 1/2  1/2 )             ( 1/2  1  1/2 )
            ! avt = 1/8 ( 1    2  1   )   avmu = 1/4 ( 1    1   )   avmv= 1/4 ( 1/2  1  1/2 )
            !           ( 1/2  1  1/2 )              ( 1/2  1/2 )
  
            DO jk = 2, jpkm1
               DO jj = 2, jpjm1
                  DO ji = fs_2, fs_jpim1

                     avmu(ji,jj,jk) = (      zviscos(ji  ,jj  ,jk) + zviscos(ji+1,jj  ,jk)   &
                        &              +.5*( zviscos(ji  ,jj-1,jk) + zviscos(ji+1,jj-1,jk)   &
                        &                   +zviscos(ji  ,jj+1,jk) + zviscos(ji+1,jj+1,jk) ) ) * eumean(ji,jj,jk)
                     
                     avmv(ji,jj,jk) = (      zviscos(ji  ,jj  ,jk) + zviscos(ji  ,jj+1,jk)   &
                        &              +.5*( zviscos(ji-1,jj  ,jk) + zviscos(ji-1,jj+1,jk)   &
                        &                   +zviscos(ji+1,jj  ,jk) + zviscos(ji+1,jj+1,jk) ) ) * evmean(ji,jj,jk)
 
                     avt (ji,jj,jk) = ( .5*( zdiffut(ji-1,jj+1,jk) + zdiffut(ji-1,jj-1,jk)    &
                        &                   +zdiffut(ji+1,jj+1,jk) + zdiffut(ji+1,jj-1,jk) )  &
                        &              +1.*( zdiffut(ji-1,jj  ,jk) + zdiffut(ji  ,jj+1,jk)    &
                        &                   +zdiffut(ji  ,jj-1,jk) + zdiffut(ji+1,jj  ,jk) )  &
                        &              +2.*  zdiffut(ji  ,jj  ,jk)                          ) * etmean(ji,jj,jk)
#if defined key_zdfddm   
                     avs (ji,jj,jk) = ( .5*( zdiffus(ji-1,jj+1,jk) + zdiffus(ji-1,jj-1,jk)    &
                        &                   +zdiffus(ji+1,jj+1,jk) + zdiffus(ji+1,jj-1,jk) )  &
                        &              +1.*( zdiffus(ji-1,jj  ,jk) + zdiffus(ji  ,jj+1,jk)    &
                        &                   +zdiffus(ji  ,jj-1,jk) + zdiffus(ji+1,jj  ,jk) )  &
                        &              +2.*  zdiffus(ji  ,jj  ,jk)                          ) * etmean(ji,jj,jk)  
#endif                
                  END DO
               END DO
            END DO
         
         END SELECT

         DO jk = 2, jpkm1                       ! vertical slab
            !
            !  Minimum value on the eddy diffusivity
            ! ----------------------------------------
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  avt(ji,jj,jk) = MAX( avt(ji,jj,jk), avtb(jk) ) * tmask(ji,jj,jk)
#if defined key_zdfddm  
                  avs(ji,jj,jk) = MAX( avs(ji,jj,jk), avtb(jk) ) * tmask(ji,jj,jk)
#endif
               END DO
            END DO

            !
            ! Minimum value on the eddy viscosity
            ! ----------------------------------------
            DO jj = 1, jpj
               DO ji = 1, jpi
                  avmu(ji,jj,jk) = MAX( avmu(ji,jj,jk), avmb(jk) ) * umask(ji,jj,jk)
                  avmv(ji,jj,jk) = MAX( avmv(ji,jj,jk), avmb(jk) ) * vmask(ji,jj,jk)
               END DO
            END DO
            !
         END DO

         ! Lateral boundary conditions on avt  (sign unchanged)
         CALL lbc_lnk( hkpp(:,:), 'T', 1. )

         ! Lateral boundary conditions on avt  (sign unchanged)
         CALL lbc_lnk( avt(:,:,:), 'W', 1. )
#if defined key_zdfddm  
         CALL lbc_lnk( avs(:,:,:), 'W', 1. )  
#endif
         ! Lateral boundary conditions (avmu,avmv) (U- and V- points, sign unchanged)
         CALL lbc_lnk( avmu(:,:,:), 'U', 1. )   ;    CALL lbc_lnk( avmv(:,:,:), 'V', 1. )  
 
         IF(ln_ctl) THEN
#if defined key_zdfddm
            CALL prt_ctl(tab3d_1=avt , clinfo1=' kpp - t: ', tab3d_2=avs , clinfo2=' s: ', ovlap=1, kdim=jpk)
#else
            CALL prt_ctl(tab3d_1=avt , clinfo1=' kpp - t: ', ovlap=1, kdim=jpk)
#endif
            CALL prt_ctl(tab3d_1=avmu, clinfo1=' kpp - u: ', mask1=umask,  &
               &         tab3d_2=avmv, clinfo2=      ' v: ', mask2=vmask, ovlap=1, kdim=jpk)
         ENDIF

      CALL wrk_dealloc( jpi, zmoa, zekman, zhmax, zria, zhbl )
      CALL wrk_dealloc( jpi, za2m, za3m, zkmpm, za2t, za3t, zkmpt )
      CALL wrk_dealloc( jpi,2, zriblk )
      CALL wrk_dealloc( jpi,3, zmoek, kjstart = 0 )
      CALL wrk_dealloc( jpi,3, zdept )
      CALL wrk_dealloc( jpi,4, zdepw, zdift, zvisc )
      CALL wrk_dealloc( jpi,jpj, zBo, zBosol, zustar )
      CALL wrk_dealloc( jpi,jpk, zmask, zblcm, zblct, zblcs )
#if defined key_zdfddm
      CALL wrk_dealloc( jpi,4, zdifs )
      CALL wrk_dealloc( jpi, zmoa, za2s, za3s, zkmps )
      CALL wrk_dealloc( jpi,jpk, zblcs )
      CALL wrk_dealloc( jpi,jpi,jpk, zdiffus )
#endif
      !
      IF( nn_timing == 1 )  CALL timing_stop('zdf_kpp')
      !
   END SUBROUTINE zdf_kpp


   SUBROUTINE tra_kpp( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_kpp  ***
      !!
      !! ** Purpose :   compute and add to the tracer trend the non-local tracer flux
      !!
      !! ** Method  :   ???
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   ztrdt, ztrds   ! 3D workspace
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt
      INTEGER :: ji, jj, jk
      !
      IF( nn_timing == 1 )  CALL timing_start('tra_kpp')
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*) 
         IF(lwp) WRITE(numout,*) 'tra_kpp : KPP non-local tracer fluxes'
         IF(lwp) WRITE(numout,*) '~~~~~~~   '
      ENDIF

      IF( l_trdtra )   THEN                    !* Save ta and sa trends
         ALLOCATE( ztrdt(jpi,jpj,jpk) )   ;    ztrdt(:,:,:) = tsa(:,:,:,jp_tem)
         ALLOCATE( ztrds(jpi,jpj,jpk) )   ;    ztrds(:,:,:) = tsa(:,:,:,jp_sal)
      ENDIF

      ! add non-local temperature and salinity flux ( in convective case only)
      DO jk = 1, jpkm1
         DO jj = 2, jpjm1 
            DO ji = fs_2, fs_jpim1
               tsa(ji,jj,jk,jp_tem) =  tsa(ji,jj,jk,jp_tem)                      &
                  &                 - (  ghats(ji,jj,jk  ) * avt  (ji,jj,jk  )   & 
                  &                    - ghats(ji,jj,jk+1) * avt  (ji,jj,jk+1) ) * wt0(ji,jj) / fse3t(ji,jj,jk)
               tsa(ji,jj,jk,jp_sal) =  tsa(ji,jj,jk,jp_sal)                      &
                  &                 - (  ghats(ji,jj,jk  ) * fsavs(ji,jj,jk  )   & 
                  &                    - ghats(ji,jj,jk+1) * fsavs(ji,jj,jk+1) ) * ws0(ji,jj) / fse3t(ji,jj,jk)
            END DO
         END DO
      END DO

      ! save the non-local tracer flux trends for diagnostic
      IF( l_trdtra )   THEN
         ztrdt(:,:,:) = tsa(:,:,:,jp_tem) - ztrdt(:,:,:)
         ztrds(:,:,:) = tsa(:,:,:,jp_sal) - ztrds(:,:,:)
!!bug gm jpttdzdf ==> jpttkpp
         CALL trd_tra( kt, 'TRA', jp_tem, jptra_trd_zdf, ztrdt )
         CALL trd_tra( kt, 'TRA', jp_sal, jptra_trd_zdf, ztrds )
         DEALLOCATE( ztrdt )      ;     DEALLOCATE( ztrds )
      ENDIF

      IF(ln_ctl) THEN
         CALL prt_ctl( tab3d_1=tsa(:,:,:,jp_tem), clinfo1=' kpp  - Ta: ', mask1=tmask,   &
         &             tab3d_2=tsa(:,:,:,jp_sal), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('tra_kpp')
      !
   END SUBROUTINE tra_kpp

#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   SUBROUTINE trc_kpp( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_kpp  ***
      !!
      !! ** Purpose :   compute and add to the tracer trend the non-local
      !!                tracer flux
      !!
      !! ** Method  :   ???
      !!
      !! history :
      !!            9.0  ! 2005-11 (G. Madec)  Original code
      !!       NEMO 3.3  ! 2010-06 (C. Ethe )  Adapted to passive tracers
      !!----------------------------------------------------------------------
      USE trc
      USE prtctl_trc          ! Print control
      !
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !
      INTEGER  ::   ji, jj, jk, jn      ! Dummy loop indices
      REAL(wp) ::   ztra, zflx
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   ztrtrd
      !!----------------------------------------------------------------------

      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*) 
         IF(lwp) WRITE(numout,*) 'trc_kpp : KPP non-local tracer fluxes'
         IF(lwp) WRITE(numout,*) '~~~~~~~   '
      ENDIF

      IF( l_trdtrc )  ALLOCATE( ztrtrd(jpi,jpj,jpk) )
      !
      DO jn = 1, jptra
         !
         IF( l_trdtrc )  ztrtrd(:,:,:)  = tra(:,:,:,jn)
         ! add non-local on passive tracer flux ( in convective case only)
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1 
               DO ji = fs_2, fs_jpim1
                  ! Surface tracer flux for non-local term 
                  zflx = - ( emps(ji,jj) * tra(ji,jj,1,jn) * rcs ) * tmask(ji,jj,1)
                  ! compute the trend
                  ztra = - ( ghats(ji,jj,jk  ) * fsavs(ji,jj,jk  )   &
                  &        - ghats(ji,jj,jk+1) * fsavs(ji,jj,jk+1) ) * zflx / fse3t(ji,jj,jk)
                  ! add the trend to the general trend
                  tra(ji,jj,jk,jn) = tra(ji,jj,jk,jn)  + ztra
               END DO
            END DO
         END DO
         ! save the non-local tracer flux trends for diagnostic
         IF( l_trdtrc )  ztrtrd(:,:,:)  = tra(:,:,:,jn) - ztrtrd(:,:,:)
         CALL trd_tra( kt, 'TRC', jn, jptra_trd_zdf, ztrtrd(:,:,:,jn) )
         !
      END DO
      IF( l_trdtrc )  DEALLOCATE( ztrtrd )
      IF( ln_ctl )   THEN
         WRITE(charout, FMT="(' kpp')")  ;  CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( tab4d=tra, mask=tmask, clinfo=clname, clinfo2='trd' )
      ENDIF
      !
   END SUBROUTINE trc_kpp

#else
   !!----------------------------------------------------------------------
   !!   NO 'key_top'           DUMMY routine                  No TOP models
   !!----------------------------------------------------------------------
   SUBROUTINE trc_kpp( kt )         ! Dummy routine
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      WRITE(*,*) 'tra_kpp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_kpp
#endif

   SUBROUTINE zdf_kpp_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_kpp_init  ***
      !!                     
      !! ** Purpose :   Initialization of the vertical eddy diffivity and 
      !!      viscosity when using a kpp turbulent closure scheme
      !!
      !! ** Method  :   Read the namkpp namelist and check the parameters
      !!      called at the first timestep (nit000)
      !!
      !! ** input   :   Namlist namkpp
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk     ! dummy loop indices
#if ! defined key_kppcustom
      INTEGER  ::   jm             ! dummy loop indices     
      REAL(wp) ::   zref, zdist    ! tempory scalars
#endif
#if defined key_kpplktb
      REAL(wp) ::   zustar, zucube, zustvk, zeta, zehat   ! tempory scalars
#endif
      REAL(wp) ::   zhbf           ! tempory scalars
      LOGICAL  ::   ll_kppcustom   ! 1st ocean level taken as surface layer
      LOGICAL  ::   ll_kpplktb     ! Lookup table for turbul. velocity scales 
      !!
      NAMELIST/namzdf_kpp/ ln_kpprimix, rn_difmiw, rn_difsiw, rn_riinfty, rn_difri, rn_bvsqcon, rn_difcon, nn_ave
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zdf_kpp_init')
      !
      REWIND ( numnam )               ! Read Namelist namkpp : K-Profile Parameterisation
      READ   ( numnam, namzdf_kpp )

      IF(lwp) THEN                    ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'zdf_kpp_init : K-Profile Parameterisation'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namzdf_kpp : set tke mixing parameters'
         WRITE(numout,*) '     Shear instability mixing                      ln_kpprimix = ', ln_kpprimix
         WRITE(numout,*) '     max. internal wave viscosity                  rn_difmiw   = ', rn_difmiw
         WRITE(numout,*) '     max. internal wave diffusivity                rn_difsiw   = ', rn_difsiw
         WRITE(numout,*) '     Richardson Number limit for shear instability rn_riinfty  = ', rn_riinfty
         WRITE(numout,*) '     max. shear mixing at Rig = 0                  rn_difri    = ', rn_difri
         WRITE(numout,*) '     Brunt-Vaisala squared for max. convection     rn_bvsqcon  = ', rn_bvsqcon
         WRITE(numout,*) '     max. mix. in interior convec.                 rn_difcon   = ', rn_difcon
         WRITE(numout,*) '     horizontal average flag                       nn_ave      = ', nn_ave
      ENDIF

      !                              ! allocate zdfkpp arrays
      IF( zdf_kpp_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'zdf_kpp_init : unable to allocate arrays' )

      ll_kppcustom = .FALSE.
      ll_kpplktb   = .FALSE.

#if defined key_kppcustom
      ll_kppcustom = .TRUE.
#endif
#if defined key_kpplktb
      ll_kpplktb   = .TRUE.
#endif
      IF(lwp) THEN
         WRITE(numout,*) '     Lookup table for turbul. velocity scales ll_kpplktb   = ', ll_kpplktb
         WRITE(numout,*) '     1st ocean level taken as surface layer   ll_kppcustom = ', ll_kppcustom
      ENDIF
      
      IF( lk_zdfddm) THEN
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) '    Double diffusion mixing on temperature and salinity '
            WRITE(numout,*) '    CAUTION : done in routine zdfkpp, not in routine zdfddm '
         ENDIF
      ENDIF
      

      !set constants not in namelist
      !-----------------------------
      Vtc  = rconcv * SQRT( 0.2 / ( rconcs * epsilon ) ) / ( vonk * vonk * Ricr )
      rcg  = rcstar * vonk * ( rconcs * vonk * epsilon )**pthird

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '     Constant value for unreso. turbul. velocity shear Vtc = ', Vtc
         WRITE(numout,*) '     Non-dimensional coef. for nonlocal transport      rcg = ', rcg
       ENDIF

      ! ratt is the attenuation coefficient for solar flux
      ! Should be different is s_coordinate
      DO jk = 1, jpk
         zhbf     = - fsdept(1,1,jk) * hbf
         ratt(jk) = 1.0 - ( rabs * EXP( zhbf / xsi1 ) + ( 1.0 - rabs ) * EXP( zhbf / xsi2 ) )        
      ENDDO

      ! Horizontal average : initialization of weighting arrays 
      ! -------------------
      
      SELECT CASE ( nn_ave )

      CASE ( 0 )                ! no horizontal average
         IF(lwp) WRITE(numout,*) '          no horizontal average on avt, avmu, avmv'
         IF(lwp) WRITE(numout,*) '          only in very high horizontal resolution !'
         ! weighting mean arrays etmean, eumean and evmean
         !           ( 1  1 )                                          ( 1 )
         ! avt = 1/4 ( 1  1 )     avmu = 1/2 ( 1  1 )       avmv=  1/2 ( 1 )
         !                          
         etmean(:,:,:) = 0.e0
         eumean(:,:,:) = 0.e0
         evmean(:,:,:) = 0.e0
         
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = 2, jpim1   ! vector opt.
                  etmean(ji,jj,jk) = tmask(ji,jj,jk)                     &
                  &  / MAX( 1.,  umask(ji-1,jj  ,jk) + umask(ji,jj,jk)   &
                  &            + vmask(ji  ,jj-1,jk) + vmask(ji,jj,jk)  )
                  
                  eumean(ji,jj,jk) = umask(ji,jj,jk)                     &
                  &  / MAX( 1.,  tmask(ji,jj,jk) + tmask(ji+1,jj  ,jk)  )

                  evmean(ji,jj,jk) = vmask(ji,jj,jk)                     &
                  &  / MAX( 1.,  tmask(ji,jj,jk) + tmask(ji  ,jj+1,jk)  )
               END DO
            END DO
         END DO

      CASE ( 1 )                ! horizontal average 
         IF(lwp) WRITE(numout,*) '          horizontal average on avt, avmu, avmv'
         ! weighting mean arrays etmean, eumean and evmean
         !           ( 1/2  1  1/2 )              ( 1/2  1/2 )             ( 1/2  1  1/2 )
         ! avt = 1/8 ( 1    2  1   )   avmu = 1/4 ( 1    1   )   avmv= 1/4 ( 1/2  1  1/2 )
         !           ( 1/2  1  1/2 )              ( 1/2  1/2 )
         etmean(:,:,:) = 0.e0
         eumean(:,:,:) = 0.e0
         evmean(:,:,:) = 0.e0
         
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  etmean(ji,jj,jk) = tmask(ji, jj,jk)                           &
                     & / MAX( 1., 2.* tmask(ji,jj,jk)                           &
                     &      +.5 * ( tmask(ji-1,jj+1,jk) + tmask(ji-1,jj-1,jk)   &
                     &             +tmask(ji+1,jj+1,jk) + tmask(ji+1,jj-1,jk) ) &
                     &      +1. * ( tmask(ji-1,jj  ,jk) + tmask(ji  ,jj+1,jk)   &
                     &             +tmask(ji  ,jj-1,jk) + tmask(ji+1,jj  ,jk) ) )
                  
                  eumean(ji,jj,jk) = umask(ji,jj,jk)                        &
                     &  / MAX( 1.,   tmask(ji,jj  ,jk) + tmask(ji+1,jj  ,jk)   &
                     &       +.5 * ( tmask(ji,jj-1,jk) + tmask(ji+1,jj-1,jk)   &
                     &              +tmask(ji,jj+1,jk) + tmask(ji+1,jj+1,jk) )  )
                  
                  evmean(ji,jj,jk) = vmask(ji,jj,jk)                        &
                     &  / MAX( 1.,   tmask(ji  ,jj,jk) + tmask(ji  ,jj+1,jk)   &
                     &       +.5 * ( tmask(ji-1,jj,jk) + tmask(ji-1,jj+1,jk)   &
                     &              +tmask(ji+1,jj,jk) + tmask(ji+1,jj+1,jk) )  )
               END DO
            END DO
         END DO

      CASE DEFAULT
         WRITE(ctmp1,*) '          bad flag value for nn_ave = ', nn_ave
         CALL ctl_stop( ctmp1 )

      END SELECT
 
      ! Initialization of vertical eddy coef. to the background value
      ! -------------------------------------------------------------
      DO jk = 1, jpk
         avt (:,:,jk) = avtb(jk) * tmask(:,:,jk)
         avmu(:,:,jk) = avmb(jk) * umask(:,:,jk)
         avmv(:,:,jk) = avmb(jk) * vmask(:,:,jk)
      END DO

      ! zero the surface flux for non local term and kpp mixed layer depth
      ! ------------------------------------------------------------------
      ghats(:,:,:) = 0.
      wt0  (:,:  ) = 0.
      ws0  (:,:  ) = 0.
      hkpp (:,:  ) = 0. ! just a diagnostic (not essential)

#if ! defined key_kppcustom
      ! compute arrays (del, wz) for reference mean values 
      ! (increase speed for vectorization key_kppcustom not defined)
      del(1:jpk, 1:jpk) = 0.
      DO jk = 1, jpk
         zref = epsilon * fsdept(1,1,jk)    
         DO jm = 1 , jpk
            zdist = zref - fsdepw(1,1,jm)   
            IF( zdist > 0.  ) THEN
               del(jk,jm) = MIN( zdist, fse3t(1,1,jm) ) / zref   
            ELSE
               del(jk,jm) = 0.
            ENDIF
         ENDDO
      ENDDO
#endif

#if defined key_kpplktb
      ! build lookup table for turbulent velocity scales
      dezehat = ( dehatmax - dehatmin ) / nilktbm1
      deustar = ( ustmax   - ustmin   ) / njlktbm1
 
      DO jj = 1, njlktb
         zustar = ( jj - 1) * deustar + ustmin
         zustvk = vonk * zustar 
         zucube = zustar * zustar * zustar 
         DO ji = 1 , nilktb
            zehat = ( ji - 1 ) * dezehat + dehatmin
            zeta   = zehat / ( zucube + epsln )
            IF( zehat >= 0 ) THEN             ! Stable case
               wmlktb(ji,jj) = zustvk / ABS( 1.0 + rconc1 * zeta + epsln )                        
               wslktb(ji,jj) = wmlktb(ji,jj)
            ELSE                                ! Unstable case 
               IF( zeta > rzetam ) THEN
                  wmlktb(ji,jj) = zustvk * ABS( 1.0    - rconc2 * zeta )**pfourth
               ELSE
                  wmlktb(ji,jj) = zustvk * ABS( rconam - rconcm * zeta )**pthird
               ENDIF
               
               IF( zeta > rzetas ) THEN
                  wslktb(ji,jj) = zustvk * SQRT( ABS( 1.0 - rconc2 * zeta ) )
               ELSE
                  wslktb(ji,jj) = zustvk * ABS( rconas - rconcs * zeta )**pthird
               ENDIF
            ENDIF
         END DO
      END DO
#endif
      !
      IF( nn_timing == 1 )  CALL timing_stop('zdf_kpp_init')
      !
   END SUBROUTINE zdf_kpp_init

#else
   !!----------------------------------------------------------------------
   !!   Dummy module :                                        NO KPP scheme
   !!----------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_zdfkpp = .FALSE.   !: KPP flag
CONTAINS
   SUBROUTINE zdf_kpp_init           ! Dummy routine
      WRITE(*,*) 'zdf_kpp_init: You should not have seen this print! error?'
   END SUBROUTINE zdf_kpp_init
   SUBROUTINE zdf_kpp( kt )          ! Dummy routine
      WRITE(*,*) 'zdf_kpp: You should not have seen this print! error?', kt
   END SUBROUTINE zdf_kpp
   SUBROUTINE tra_kpp( kt )          ! Dummy routine
      WRITE(*,*) 'tra_kpp: You should not have seen this print! error?', kt
   END SUBROUTINE tra_kpp
   SUBROUTINE trc_kpp( kt )          ! Dummy routine
      WRITE(*,*) 'trc_kpp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_kpp
#endif

   !!======================================================================
END MODULE zdfkpp
