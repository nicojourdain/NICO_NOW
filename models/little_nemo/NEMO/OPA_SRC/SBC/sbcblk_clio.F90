MODULE sbcblk_clio
   !!======================================================================
   !!                   ***  MODULE  sbcblk_clio  ***
   !! Ocean forcing:  bulk thermohaline forcing of the ocean (or ice)
   !!=====================================================================
   !! History :  OPA  !  1997-06 (Louvain-La-Neuve)  Original code
   !!                 !  2001-04 (C. Ethe) add flx_blk_declin
   !!   NEMO     2.0  !  2002-08 (C. Ethe, G. Madec) F90: Free form and module
   !!            3.0  !  2008-03 (C. Talandier, G. Madec) surface module + LIM3
   !!            3.2  !  2009-04 (B. Lemaire) Introduce iom_put
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_blk_clio   : CLIO bulk formulation: read and update required input fields
   !!   blk_clio_oce   : ocean CLIO bulk formulea: compute momentum, heat and freswater fluxes for the ocean
   !!   blk_ice_clio   : ice   CLIO bulk formulea: compute momentum, heat and freswater fluxes for the sea-ice
   !!   blk_clio_qsr_oce : shortwave radiation for ocean computed from the cloud cover
   !!   blk_clio_qsr_ice : shortwave radiation for ice   computed from the cloud cover
   !!   flx_blk_declin : solar declinaison
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE fldread         ! read input fields
   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE iom             ! I/O manager library
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distribued memory computing library
   USE wrk_nemo        ! work arrays
   USE timing          ! Timing
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)

   USE albedo
   USE prtctl          ! Print control
#if defined key_lim3
   USE ice
   USE sbc_ice         ! Surface boundary condition: ice fields
#elif defined key_lim2
   USE ice_2
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC sbc_blk_clio        ! routine called by sbcmod.F90 
   PUBLIC blk_ice_clio        ! routine called by sbcice_lim.F90 

   INTEGER , PARAMETER ::   jpfld   = 7           ! maximum number of files to read 
   INTEGER , PARAMETER ::   jp_utau = 1           ! index of wind stress (i-component)      (N/m2)    at U-point
   INTEGER , PARAMETER ::   jp_vtau = 2           ! index of wind stress (j-component)      (N/m2)    at V-point
   INTEGER , PARAMETER ::   jp_wndm = 3           ! index of 10m wind module                 (m/s)    at T-point
   INTEGER , PARAMETER ::   jp_humi = 4           ! index of specific humidity               ( - )
   INTEGER , PARAMETER ::   jp_ccov = 5           ! index of cloud cover                     ( - )
   INTEGER , PARAMETER ::   jp_tair = 6           ! index of 10m air temperature             (Kelvin)
   INTEGER , PARAMETER ::   jp_prec = 7           ! index of total precipitation (rain+snow) (Kg/m2/s)

   TYPE(FLD),ALLOCATABLE,DIMENSION(:) :: sf  ! structure of input fields (file informations, fields read)

   INTEGER, PARAMETER  ::   jpintsr = 24          ! number of time step between sunrise and sunset
   !                                              ! uses for heat flux computation
   LOGICAL ::   lbulk_init = .TRUE.               ! flag, bulk initialization done or not)

#if ! defined key_lim3                          
   ! in namicerun with LIM3
   REAL(wp) ::   cai = 1.40e-3 ! best estimate of atm drag in order to get correct FS export in ORCA2-LIM
   REAL(wp) ::   cao = 1.00e-3 ! chosen by default  ==> should depends on many things...  !!gmto be updated
#endif

   REAL(wp) ::   rdtbs2      !:   
   
   REAL(wp), DIMENSION(19)  ::  budyko            ! BUDYKO's coefficient (cloudiness effect on LW radiation)
   DATA budyko / 1.00, 0.98, 0.95, 0.92, 0.89, 0.86, 0.83, 0.80, 0.78, 0.75,   &
      &          0.72, 0.69, 0.67, 0.64, 0.61, 0.58, 0.56, 0.53, 0.50 /
   REAL(wp), DIMENSION(20)  :: tauco              ! cloud optical depth coefficient
   DATA tauco / 6.6, 6.6, 7.0, 7.2, 7.1, 6.8, 6.5, 6.6, 7.1, 7.6,   &
      &         6.6, 6.1, 5.6, 5.5, 5.8, 5.8, 5.6, 5.6, 5.6, 5.6 /
   !!
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sbudyko      ! cloudiness effect on LW radiation
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   stauc        ! cloud optical depth 
   
   REAL(wp) ::   eps20  = 1.e-20   ! constant values
   
   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: sbcblk_clio.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_blk_clio( kt )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE sbc_blk_clio  ***
      !!                   
      !! ** Purpose :   provide at each time step the surface ocean fluxes
      !!      (momentum, heat, freshwater and runoff) 
      !!
      !! ** Method  : (1) READ each fluxes in NetCDF files:
      !!      the i-component of the stress                (N/m2)
      !!      the j-component of the stress                (N/m2)
      !!      the 10m wind pseed module                    (m/s)
      !!      the 10m air temperature                      (Kelvin)
      !!      the 10m specific humidity                    (-)
      !!      the cloud cover                              (-)
      !!      the total precipitation (rain+snow)          (Kg/m2/s)
      !!              (2) CALL blk_oce_clio
      !!
      !!      C A U T I O N : never mask the surface stress fields
      !!                      the stress is assumed to be in the mesh referential
      !!                      i.e. the (i,j) referential
      !!
      !! ** Action  :   defined at each time-step at the air-sea interface
      !!              - utau, vtau  i- and j-component of the wind stress
      !!              - taum        wind stress module at T-point
      !!              - wndm        10m wind module at T-point
      !!              - qns, qsr    non-slor and solar heat flux
      !!              - emp, emps   evaporation minus precipitation
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      !!
      INTEGER  ::   ifpr, jfpr   ! dummy indices
      INTEGER  ::   ierr0, ierr1, ierr2, ierr3   ! return error code
      !!
      CHARACTER(len=100) ::  cn_dir                            !   Root directory for location of CLIO files
      TYPE(FLD_N), DIMENSION(jpfld) ::   slf_i                 ! array of namelist informations on the fields to read
      TYPE(FLD_N) ::   sn_utau, sn_vtau, sn_wndm, sn_tair      ! informations about the fields to be read
      TYPE(FLD_N) ::   sn_humi, sn_ccov, sn_prec               !   "                                 "
      !!
      NAMELIST/namsbc_clio/ cn_dir, sn_utau, sn_vtau, sn_wndm, sn_humi,   &
         &                          sn_ccov, sn_tair, sn_prec
      !!---------------------------------------------------------------------

      !                                         ! ====================== !
      IF( kt == nit000 ) THEN                   !  First call kt=nit000  !
         !                                      ! ====================== !
         ! set file information (default values)
         cn_dir = './'       ! directory in which the model is executed

         ! (NB: frequency positive => hours, negative => months)
         !            !    file    ! frequency ! variable ! time intep !  clim   ! 'yearly' or ! weights  ! rotation !
         !            !    name    !  (hours)  !  name    !   (T/F)    !  (T/F)  !  'monthly'  ! filename ! pairs    !
         sn_utau = FLD_N( 'utau'   ,    24     , 'utau'   ,  .true.    , .false. ,   'yearly'  , ''       , ''       ) 
         sn_vtau = FLD_N( 'vtau'   ,    24     , 'vtau'   ,  .true.    , .false. ,   'yearly'  , ''       , ''       ) 
         sn_wndm = FLD_N( 'mwnd10m',    24     , 'm_10'   ,  .true.    , .false. ,   'yearly'  , ''       , ''       ) 
         sn_tair = FLD_N( 'tair10m',    24     , 't_10'   ,  .false.   , .false. ,   'yearly'  , ''       , ''       ) 
         sn_humi = FLD_N( 'humi10m',    24     , 'q_10'   ,  .false.   , .false. ,   'yearly'  , ''       , ''       ) 
         sn_ccov = FLD_N( 'ccover' ,    -1     , 'cloud'  ,  .true.    , .false. ,   'yearly'  , ''       , ''       ) 
         sn_prec = FLD_N( 'precip' ,    -1     , 'precip' ,  .true.    , .false. ,   'yearly'  , ''       , ''       ) 

         REWIND( numnam )                    ! ... read in namlist namsbc_clio
         READ  ( numnam, namsbc_clio )

         ! store namelist information in an array
         slf_i(jp_utau) = sn_utau   ;   slf_i(jp_vtau) = sn_vtau   ;   slf_i(jp_wndm) = sn_wndm
         slf_i(jp_tair) = sn_tair   ;   slf_i(jp_humi) = sn_humi
         slf_i(jp_ccov) = sn_ccov   ;   slf_i(jp_prec) = sn_prec
         
         ! set sf structure
         ALLOCATE( sf(jpfld), STAT=ierr0 )
         IF( ierr0 > 0 )   CALL ctl_stop( 'STOP', 'sbc_blk_clio: unable to allocate sf structure' )
         DO ifpr= 1, jpfld
            ALLOCATE( sf(ifpr)%fnow(jpi,jpj,1) , STAT=ierr1)
            IF( slf_i(ifpr)%ln_tint ) ALLOCATE( sf(ifpr)%fdta(jpi,jpj,1,2) , STAT=ierr2 )
         END DO
         IF( ierr1+ierr2 > 0 )   CALL ctl_stop( 'STOP', 'sbc_blk_clio: unable to allocate sf array structure' )
         ! fill sf with slf_i and control print
         CALL fld_fill( sf, slf_i, cn_dir, 'sbc_blk_clio', 'flux formulation for ocean surface boundary condition', 'namsbc_clio' )
         
         ! allocate sbcblk clio arrays
         ALLOCATE( sbudyko(jpi,jpj) , stauc(jpi,jpj), STAT=ierr3 )
         IF( ierr3 > 0 )   CALL ctl_stop( 'STOP', 'sbc_blk_clio: unable to allocate arrays' )
         !
      ENDIF
      !                                         ! ====================== !
      !                                         !    At each time-step   !
      !                                         ! ====================== !
      !
      CALL fld_read( kt, nn_fsbc, sf )                ! input fields provided at the current time-step
      !
      IF( MOD( kt - 1, nn_fsbc ) == 0 )   CALL blk_oce_clio( sf, sst_m )
      !
   END SUBROUTINE sbc_blk_clio


   SUBROUTINE blk_oce_clio( sf, pst )
      !!---------------------------------------------------------------------------
      !!                     ***  ROUTINE blk_oce_clio  ***
      !!                 
      !!  ** Purpose :   Compute momentum, heat and freshwater fluxes at ocean surface
      !!               using CLIO bulk formulea
      !!         
      !!  ** Method  :   The flux of heat at the ocean surfaces are derived
      !!       from semi-empirical ( or bulk ) formulae which relate the flux to 
      !!       the properties of the surface and of the lower atmosphere. Here, we
      !!       follow the work of Oberhuber, 1988   
      !!               - momentum flux (stresses) directly read in files at U- and V-points
      !!               - compute ocean/ice albedos (call albedo_oce/albedo_ice)  
      !!               - compute shortwave radiation for ocean (call blk_clio_qsr_oce)
      !!               - compute long-wave radiation for the ocean
      !!               - compute the turbulent heat fluxes over the ocean
      !!               - deduce the evaporation over the ocean
      !!  ** Action  :   Fluxes over the ocean:
      !!               - utau, vtau  i- and j-component of the wind stress
      !!               - taum        wind stress module at T-point
      !!               - wndm        10m wind module at T-point
      !!               - qns, qsr    non-slor and solar heat flux
      !!               - emp, emps   evaporation minus precipitation
      !!  ** Nota    :   sf has to be a dummy argument for AGRIF on NEC
      !!----------------------------------------------------------------------
      TYPE(fld), INTENT(in), DIMENSION(:)       ::   sf    ! input data
      REAL(wp) , INTENT(in), DIMENSION(jpi,jpj) ::   pst   ! surface temperature                      [Celcius]
      !!
      INTEGER  ::   ji, jj   ! dummy loop indices
      !!
      REAL(wp) ::   zrhova, zcsho, zcleo, zcldeff               ! temporary scalars
      REAL(wp) ::   zqsato, zdteta, zdeltaq, ztvmoy, zobouks    !    -         -
      REAL(wp) ::   zpsims, zpsihs, zpsils, zobouku, zxins, zpsimu   !    -         -
      REAL(wp) ::   zpsihu, zpsilu, zstab,zpsim, zpsih, zpsil   !    -         -
      REAL(wp) ::   zvatmg, zcmn, zchn, zcln, zcmcmn, zdenum    !    -         -
      REAL(wp) ::   zdtetar, ztvmoyr, zlxins, zchcm, zclcm      !    -         -
      REAL(wp) ::   zmt1, zmt2, zmt3, ztatm3, ztamr, ztaevbk    !    -         -
      REAL(wp) ::   zsst, ztatm, zcco1, zpatm, zcmax, zrmax     !    -         -
      REAL(wp) ::   zrhoa, zev, zes, zeso, zqatm, zevsqr        !    -         -
      REAL(wp) ::   ztx2, zty2                                  !    -         -
      REAL(wp), POINTER, DIMENSION(:,:) ::   zqlw        ! long-wave heat flux over ocean
      REAL(wp), POINTER, DIMENSION(:,:) ::   zqla        ! latent heat flux over ocean
      REAL(wp), POINTER, DIMENSION(:,:) ::   zqsb        ! sensible heat flux over ocean
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('blk_oce_clio')
      !
      CALL wrk_alloc( jpi,jpj, zqlw, zqla, zqsb )

      zpatm = 101000._wp      ! atmospheric pressure  (assumed constant here)

      !------------------------------------!
      !   momentum fluxes  (utau, vtau )   !
      !------------------------------------!
!CDIR COLLAPSE
      utau(:,:) = sf(jp_utau)%fnow(:,:,1)
!CDIR COLLAPSE
      vtau(:,:) = sf(jp_vtau)%fnow(:,:,1)

      !------------------------------------!
      !   wind stress module (taum )       !
      !------------------------------------!
!CDIR NOVERRCHK
      DO jj = 2, jpjm1
!CDIR NOVERRCHK
         DO ji = fs_2, fs_jpim1   ! vector opt.
            ztx2 = utau(ji-1,jj  ) + utau(ji,jj)
            zty2 = vtau(ji  ,jj-1) + vtau(ji,jj)
            taum(ji,jj) = 0.5 * SQRT( ztx2 * ztx2 + zty2 * zty2 )
         END DO
      END DO
      CALL lbc_lnk( taum, 'T', 1. )

      !------------------------------------!
      !   store the wind speed  (wndm )    !
      !------------------------------------!
!CDIR COLLAPSE
      wndm(:,:) = sf(jp_wndm)%fnow(:,:,1)

      !------------------------------------------------!
      !   Shortwave radiation for ocean and snow/ice   !
      !------------------------------------------------!
      
      CALL blk_clio_qsr_oce( qsr )

      !------------------------!
      !   Other ocean fluxes   !
      !------------------------!
!CDIR NOVERRCHK
!CDIR COLLAPSE
      DO jj = 1, jpj
!CDIR NOVERRCHK
         DO ji = 1, jpi
            !
            zsst  = pst(ji,jj)              + rt0           ! converte Celcius to Kelvin the SST
            ztatm = sf(jp_tair)%fnow(ji,jj,1)               ! and set minimum value far above 0 K (=rt0 over land)
            zcco1 = 1.0 - sf(jp_ccov)%fnow(ji,jj,1)         ! fraction of clear sky ( 1 - cloud cover)
            zrhoa = zpatm / ( 287.04 * ztatm )              ! air density (equation of state for dry air) 
            ztamr = ztatm - rtt                             ! Saturation water vapour
            zmt1  = SIGN( 17.269,  ztamr )                  !           ||
            zmt2  = SIGN( 21.875,  ztamr )                  !          \  /
            zmt3  = SIGN( 28.200, -ztamr )                  !           \/
            zes   = 611.0 * EXP(  ABS( ztamr ) * MIN ( zmt1, zmt2 ) / ( ztatm - 35.86  + MAX( 0.e0, zmt3 ) )  )
            zev    = sf(jp_humi)%fnow(ji,jj,1) * zes        ! vapour pressure  
            zevsqr = SQRT( zev * 0.01 )                     ! square-root of vapour pressure
            zqatm = 0.622 * zev / ( zpatm - 0.378 * zev )   ! specific humidity 

            !--------------------------------------!
            !  long-wave radiation over the ocean  !  ( Berliand 1952 ; all latitudes )
            !--------------------------------------!
            ztatm3  = ztatm * ztatm * ztatm
            zcldeff = 1.0 - sbudyko(ji,jj) * sf(jp_ccov)%fnow(ji,jj,1) * sf(jp_ccov)%fnow(ji,jj,1)    
            ztaevbk = ztatm * ztatm3 * zcldeff * ( 0.39 - 0.05 * zevsqr ) 
            !
            zqlw(ji,jj) = - emic * stefan * ( ztaevbk + 4. * ztatm3 * ( zsst - ztatm ) ) 

            !--------------------------------------------------
            !  Latent and sensible heat fluxes over the ocean 
            !--------------------------------------------------
            !                                                          ! vapour pressure at saturation of ocean
            zeso =  611.0 * EXP ( 17.2693884 * ( zsst - rtt ) * tmask(ji,jj,1) / ( zsst - 35.86 ) )

            zqsato = ( 0.622 * zeso ) / ( zpatm - 0.378 * zeso )       ! humidity close to the ocean surface (at saturation)

            ! Drag coefficients from Large and Pond (1981,1982)
            !                                                          ! Stability parameters
            zdteta  = zsst - ztatm
            zdeltaq = zqatm - zqsato
            ztvmoy  = ztatm * ( 1. + 2.2e-3 * ztatm * zqatm )
            zdenum  = MAX( sf(jp_wndm)%fnow(ji,jj,1) * sf(jp_wndm)%fnow(ji,jj,1) * ztvmoy, eps20 )
            zdtetar = zdteta / zdenum
            ztvmoyr = ztvmoy * ztvmoy * zdeltaq / zdenum
            !                                                          ! case of stable atmospheric conditions
            zobouks = -70.0 * 10. * ( zdtetar + 3.2e-3 * ztvmoyr )
            zobouks = MAX( 0.e0, zobouks )
            zpsims = -7.0 * zobouks
            zpsihs =  zpsims
            zpsils =  zpsims
            !                                                          ! case of unstable atmospheric conditions
            zobouku = MIN(  0.e0, -100.0 * 10.0 * ( zdtetar + 2.2e-3 * ztvmoyr )  )
            zxins   = ( 1. - 16. * zobouku )**0.25
            zlxins  = LOG( ( 1. + zxins * zxins ) / 2. )
            zpsimu  = 2. * LOG( ( 1 + zxins ) * 0.5 )  + zlxins - 2. * ATAN( zxins ) + rpi * 0.5
            zpsihu  = 2. * zlxins
            zpsilu  = zpsihu
            !                                                          ! intermediate values
            zstab   = MAX( 0.e0, SIGN( 1.e0, zdteta ) )
            zpsim   = zstab * zpsimu + ( 1.0 - zstab ) * zpsims
            zpsih   = zstab * zpsihu + ( 1.0 - zstab ) * zpsihs
            zpsil   = zpsih
            
            zvatmg         = MAX( 0.032 * 1.5e-3 * sf(jp_wndm)%fnow(ji,jj,1) * sf(jp_wndm)%fnow(ji,jj,1) / grav, eps20 )
            zcmn           = vkarmn / LOG ( 10. / zvatmg )
            zchn           = 0.0327 * zcmn
            zcln           = 0.0346 * zcmn
            zcmcmn         = 1. / ( 1. - zcmn * zpsim / vkarmn )
            ! sometimes the ratio zchn * zpsih / ( vkarmn * zcmn ) is too close to 1 and zchcm becomes very very big
            zcmax = 0.1               ! choice for maximum value of the heat transfer coefficient, guided by my intuition
            zrmax = 1 - 3.e-4 / zcmax ! maximum value of the ratio
            zchcm = zcmcmn / ( 1. - MIN ( zchn * zpsih / ( vkarmn * zcmn ) , zrmax ) )
            zclcm          = zchcm
            !                                                          ! transfert coef. (Large and Pond 1981,1982)
            zcsho          = zchn * zchcm                                
            zcleo          = zcln * zclcm 

            zrhova         = zrhoa * sf(jp_wndm)%fnow(ji,jj,1)

            ! sensible heat flux
            zqsb(ji,jj) = zrhova * zcsho * 1004.0  * ( zsst - ztatm )  
         
            ! latent heat flux (bounded by zero)
            zqla(ji,jj) = MAX(  0.e0, zrhova * zcleo * 2.5e+06 * ( zqsato - zqatm )  )
            !               
         END DO
      END DO
      
      ! ----------------------------------------------------------------------------- !
      !     III    Total FLUXES                                                       !
      ! ----------------------------------------------------------------------------- !

!CDIR COLLAPSE
      emp (:,:) = zqla(:,:) / cevap - sf(jp_prec)%fnow(:,:,1) / rday * tmask(:,:,1)
      qns (:,:) = zqlw(:,:) - zqsb(:,:) - zqla(:,:)         ! Downward Non Solar flux
      emps(:,:) = emp(:,:)
      !
      CALL iom_put( "qlw_oce",   zqlw )   ! output downward longwave  heat over the ocean
      CALL iom_put( "qsb_oce", - zqsb )   ! output downward sensible  heat over the ocean
      CALL iom_put( "qla_oce", - zqla )   ! output downward latent    heat over the ocean
      CALL iom_put( "qns_oce",   qns  )   ! output downward non solar heat over the ocean

      IF(ln_ctl) THEN
         CALL prt_ctl(tab2d_1=zqsb , clinfo1=' blk_oce_clio: zqsb   : ', tab2d_2=zqlw , clinfo2=' zqlw  : ')
         CALL prt_ctl(tab2d_1=zqla , clinfo1=' blk_oce_clio: zqla   : ', tab2d_2=qsr  , clinfo2=' qsr   : ')
         CALL prt_ctl(tab2d_1=pst  , clinfo1=' blk_oce_clio: pst    : ', tab2d_2=emp  , clinfo2=' emp   : ')
         CALL prt_ctl(tab2d_1=utau , clinfo1=' blk_oce_clio: utau   : ', mask1=umask,   &
            &         tab2d_2=vtau , clinfo2=' vtau : ', mask2=vmask )
      ENDIF

      CALL wrk_dealloc( jpi,jpj, zqlw, zqla, zqsb )
      !
      IF( nn_timing == 1 )  CALL timing_stop('blk_oce_clio')
      !
   END SUBROUTINE blk_oce_clio


   SUBROUTINE blk_ice_clio(  pst   , palb_cs, palb_os ,       &
      &                      p_taui, p_tauj, p_qns , p_qsr,   &
      &                      p_qla , p_dqns, p_dqla,          &
      &                      p_tpr , p_spr ,                  &
      &                      p_fr1 , p_fr2 , cd_grid, pdim  )
      !!---------------------------------------------------------------------------
      !!                     ***  ROUTINE blk_ice_clio  ***
      !!                 
      !!  ** Purpose :   Computation of the heat fluxes at ocean and snow/ice
      !!       surface the solar heat at ocean and snow/ice surfaces and the 
      !!       sensitivity of total heat fluxes to the SST variations
      !!         
      !!  ** Method  :   The flux of heat at the ice and ocean surfaces are derived
      !!       from semi-empirical ( or bulk ) formulae which relate the flux to 
      !!       the properties of the surface and of the lower atmosphere. Here, we
      !!       follow the work of Oberhuber, 1988   
      !!
      !!  ** Action  :   call albedo_oce/albedo_ice to compute ocean/ice albedo 
      !!          computation of snow precipitation
      !!          computation of solar flux at the ocean and ice surfaces
      !!          computation of the long-wave radiation for the ocean and sea/ice
      !!          computation of turbulent heat fluxes over water and ice
      !!          computation of evaporation over water
      !!          computation of total heat fluxes sensitivity over ice (dQ/dT)
      !!          computation of latent heat flux sensitivity over ice (dQla/dT)
      !!
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:)   ::   pst      ! ice surface temperature                   [Kelvin]
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:)   ::   palb_cs  ! ice albedo (clear    sky) (alb_ice_cs)         [%]
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:)   ::   palb_os  ! ice albedo (overcast sky) (alb_ice_os)         [%]
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   p_taui   ! surface ice stress at I-point (i-component) [N/m2]
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   p_tauj   ! surface ice stress at I-point (j-component) [N/m2]
      REAL(wp), INTENT(  out), DIMENSION(:,:,:)   ::   p_qns    ! non solar heat flux over ice (T-point)      [W/m2]
      REAL(wp), INTENT(  out), DIMENSION(:,:,:)   ::   p_qsr    !     solar heat flux over ice (T-point)      [W/m2]
      REAL(wp), INTENT(  out), DIMENSION(:,:,:)   ::   p_qla    ! latent    heat flux over ice (T-point)      [W/m2]
      REAL(wp), INTENT(  out), DIMENSION(:,:,:)   ::   p_dqns   ! non solar heat sensistivity  (T-point)      [W/m2]
      REAL(wp), INTENT(  out), DIMENSION(:,:,:)   ::   p_dqla   ! latent    heat sensistivity  (T-point)      [W/m2]
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   p_tpr    ! total precipitation          (T-point)   [Kg/m2/s]
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   p_spr    ! solid precipitation          (T-point)   [Kg/m2/s]
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   p_fr1    ! 1sr fraction of qsr penetration in ice         [%]
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   p_fr2    ! 2nd fraction of qsr penetration in ice         [%]
      CHARACTER(len=1), INTENT(in   )             ::   cd_grid  ! type of sea-ice grid ("C" or "B" grid)
      INTEGER, INTENT(in   )                      ::   pdim     ! number of ice categories
      !!
      INTEGER  ::   ji, jj, jl    ! dummy loop indices
      INTEGER  ::   ijpl          ! number of ice categories (size of 3rd dim of input arrays)
      !!
      REAL(wp) ::   zcoef, zmt1, zmt2, zmt3, ztatm3     ! temporary scalars
      REAL(wp) ::   ztaevbk, zind1, zind2, zind3, ztamr         !    -         -
      REAL(wp) ::   zesi, zqsati, zdesidt                       !    -         -
      REAL(wp) ::   zdqla, zcldeff, zev, zes, zpatm, zrhova     !    -         -
      REAL(wp) ::   zcshi, zclei, zrhovaclei, zrhovacshi        !    -         -
      REAL(wp) ::   ztice3, zticemb, zticemb2, zdqlw, zdqsb     !    -         -
      !!
      REAL(wp), DIMENSION(:,:)  , POINTER ::   ztatm   ! Tair in Kelvin
      REAL(wp), DIMENSION(:,:)  , POINTER ::   zqatm   ! specific humidity
      REAL(wp), DIMENSION(:,:)  , POINTER ::   zevsqr  ! vapour pressure square-root
      REAL(wp), DIMENSION(:,:)  , POINTER ::   zrhoa   ! air density
      REAL(wp), DIMENSION(:,:,:), POINTER ::   z_qlw, z_qsb
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('blk_ice_clio')
      !
      CALL wrk_alloc( jpi,jpj, ztatm, zqatm, zevsqr, zrhoa )
      CALL wrk_alloc( jpi,jpj,pdim, z_qlw, z_qsb )

      ijpl  = pdim                           ! number of ice categories
      zpatm = 101000.                        ! atmospheric pressure  (assumed constant  here)

#if defined key_lim3      
      tatm_ice(:,:) = sf(jp_tair)%fnow(:,:,1)   ! LIM3: make Tair available in sea-ice. WARNING allocated after call to ice_init
#endif
      !                                                        ! surface ocean fluxes computed with CLIO bulk formulea
      !------------------------------------!
      !   momentum fluxes  (utau, vtau )   !
      !------------------------------------!

      SELECT CASE( cd_grid )
      CASE( 'C' )                          ! C-grid ice dynamics
         zcoef  = cai / cao                         ! Change from air-sea stress to air-ice stress
         p_taui(:,:) = zcoef * utau(:,:)
         p_tauj(:,:) = zcoef * vtau(:,:)
      CASE( 'I' )                          ! I-grid ice dynamics:  I-point (i.e. F-point lower-left corner)
         zcoef  = 0.5_wp * cai / cao                ! Change from air-sea stress to air-ice stress
         DO jj = 2, jpj         ! stress from ocean U- and V-points to ice U,V point
            DO ji = 2, jpi   ! I-grid : no vector opt.
               p_taui(ji,jj) = zcoef * ( utau(ji-1,jj  ) + utau(ji-1,jj-1) )
               p_tauj(ji,jj) = zcoef * ( vtau(ji  ,jj-1) + vtau(ji-1,jj-1) )
            END DO
         END DO
         CALL lbc_lnk( p_taui(:,:), 'I', -1. )   ;   CALL lbc_lnk( p_tauj(:,:), 'I', -1. )   ! I-point
      END SELECT


      !  Determine cloud optical depths as a function of latitude (Chou et al., 1981).
      !  and the correction factor for taking into account  the effect of clouds 
      !------------------------------------------------------
!CDIR NOVERRCHK
!CDIR COLLAPSE
      DO jj = 1, jpj
!CDIR NOVERRCHK
         DO ji = 1, jpi
            ztatm (ji,jj) = sf(jp_tair)%fnow(ji,jj,1)                ! air temperature in Kelvins 
      
            zrhoa(ji,jj) = zpatm / ( 287.04 * ztatm(ji,jj) )         ! air density (equation of state for dry air) 
      
            ztamr = ztatm(ji,jj) - rtt                               ! Saturation water vapour
            zmt1  = SIGN( 17.269,  ztamr )
            zmt2  = SIGN( 21.875,  ztamr )
            zmt3  = SIGN( 28.200, -ztamr )
            zes   = 611.0 * EXP(  ABS( ztamr ) * MIN ( zmt1, zmt2 )   &
               &                / ( ztatm(ji,jj) - 35.86  + MAX( 0.e0, zmt3 ) )  )

            zev = sf(jp_humi)%fnow(ji,jj,1) * zes                    ! vapour pressure  
            zevsqr(ji,jj) = SQRT( zev * 0.01 )                       ! square-root of vapour pressure
            zqatm(ji,jj) = 0.622 * zev / ( zpatm - 0.378 * zev )     ! specific humidity 

            !----------------------------------------------------
            !   Computation of snow precipitation (Ledley, 1985) |
            !----------------------------------------------------
            zmt1  =   253.0 - ztatm(ji,jj)            ;   zind1 = MAX( 0.e0, SIGN( 1.e0, zmt1 ) )
            zmt2  = ( 272.0 - ztatm(ji,jj) ) / 38.0   ;   zind2 = MAX( 0.e0, SIGN( 1.e0, zmt2 ) )
            zmt3  = ( 281.0 - ztatm(ji,jj) ) / 18.0   ;   zind3 = MAX( 0.e0, SIGN( 1.e0, zmt3 ) )
            p_spr(ji,jj) = sf(jp_prec)%fnow(ji,jj,1) / rday   &      ! rday = converte mm/day to kg/m2/s
               &         * (          zind1      &                   ! solid  (snow) precipitation [kg/m2/s]
               &            + ( 1.0 - zind1 ) * (          zind2   * ( 0.5 + zmt2 )   &
               &                                 + ( 1.0 - zind2 ) *  zind3 * zmt3  )   ) 

            !----------------------------------------------------!
            !  fraction of net penetrative shortwave radiation   !
            !----------------------------------------------------!
            ! fraction of qsr_ice which is NOT absorbed in the thin surface layer
            ! and thus which penetrates inside the ice cover ( Maykut and Untersteiner, 1971 ; Elbert anbd Curry, 1993 )
            p_fr1(ji,jj) = 0.18  * ( 1.e0 - sf(jp_ccov)%fnow(ji,jj,1) ) + 0.35 * sf(jp_ccov)%fnow(ji,jj,1) 
            p_fr2(ji,jj) = 0.82  * ( 1.e0 - sf(jp_ccov)%fnow(ji,jj,1) ) + 0.65 * sf(jp_ccov)%fnow(ji,jj,1)
         END DO
      END DO
      CALL iom_put( 'snowpre', p_spr )   ! Snow precipitation 
      
      !-----------------------------------------------------------!
      !  snow/ice Shortwave radiation   (abedo already computed)  !
      !-----------------------------------------------------------!
      CALL blk_clio_qsr_ice( palb_cs, palb_os, p_qsr )

      !                                     ! ========================== !
      DO jl = 1, ijpl                       !  Loop over ice categories  !
         !                                  ! ========================== !
!CDIR NOVERRCHK
!CDIR COLLAPSE
         DO jj = 1 , jpj
!CDIR NOVERRCHK
            DO ji = 1, jpi
               !-------------------------------------------!
               !  long-wave radiation over ice categories  !  ( Berliand 1952 ; all latitudes )
               !-------------------------------------------!
               ztatm3  = ztatm(ji,jj) * ztatm(ji,jj) * ztatm(ji,jj)
               zcldeff = 1.0 - sbudyko(ji,jj) * sf(jp_ccov)%fnow(ji,jj,1) * sf(jp_ccov)%fnow(ji,jj,1)    
               ztaevbk = ztatm3 * ztatm(ji,jj) * zcldeff * ( 0.39 - 0.05 * zevsqr(ji,jj) ) 
               !
               z_qlw(ji,jj,jl) = - emic * stefan * ( ztaevbk + 4. * ztatm3 * ( pst(ji,jj,jl) - ztatm(ji,jj) ) ) 

               !----------------------------------------
               !  Turbulent heat fluxes over snow/ice     ( Latent and sensible ) 
               !----------------------------------------        

               ! vapour pressure at saturation of ice (tmask to avoid overflow in the exponential)
               zesi =  611.0 * EXP( 21.8745587 * tmask(ji,jj,1) * ( pst(ji,jj,jl) - rtt )/ ( pst(ji,jj,jl) - 7.66 ) )
               ! humidity close to the ice surface (at saturation)
               zqsati   = ( 0.622 * zesi ) / ( zpatm - 0.378 * zesi )
               
               !  computation of intermediate values
               zticemb  = pst(ji,jj,jl) - 7.66
               zticemb2 = zticemb * zticemb  
               ztice3   = pst(ji,jj,jl) * pst(ji,jj,jl) * pst(ji,jj,jl)
               zdesidt  = zesi * ( 9.5 * LOG( 10.0 ) * ( rtt - 7.66 )  / zticemb2 )
               
               !  Transfer cofficients assumed to be constant (Parkinson 1979 ; Maykut 1982)
               zcshi    = 1.75e-03
               zclei    = zcshi
               
               !  sensible and latent fluxes over ice
               zrhova     = zrhoa(ji,jj) * sf(jp_wndm)%fnow(ji,jj,1)      ! computation of intermediate values
               zrhovaclei = zrhova * zcshi * 2.834e+06
               zrhovacshi = zrhova * zclei * 1004.0
            
               !  sensible heat flux
               z_qsb(ji,jj,jl) = zrhovacshi * ( pst(ji,jj,jl) - ztatm(ji,jj) )
            
               !  latent heat flux 
               p_qla(ji,jj,jl) = MAX(  0.e0, zrhovaclei * ( zqsati - zqatm(ji,jj) )  )
              
               !  sensitivity of non solar fluxes (dQ/dT) (long-wave, sensible and latent fluxes)
               zdqlw = 4.0 * emic * stefan * ztice3
               zdqsb = zrhovacshi
               zdqla = zrhovaclei * ( zdesidt * ( zqsati * zqsati / ( zesi * zesi ) ) * ( zpatm / 0.622 ) )   
               !
               p_dqla(ji,jj,jl) = zdqla                           ! latent flux sensitivity
               p_dqns(ji,jj,jl) = -( zdqlw + zdqsb + zdqla )      !  total non solar sensitivity
            END DO
            !
         END DO
         !
      END DO
      !
      ! ----------------------------------------------------------------------------- !
      !    Total FLUXES                                                       !
      ! ----------------------------------------------------------------------------- !
      !
!CDIR COLLAPSE
      p_qns(:,:,:) = z_qlw (:,:,:) - z_qsb (:,:,:) - p_qla (:,:,:)      ! Downward Non Solar flux
!CDIR COLLAPSE
      p_tpr(:,:)   = sf(jp_prec)%fnow(:,:,1) / rday                     ! total precipitation [kg/m2/s]
      !
!!gm : not necessary as all input data are lbc_lnk...
      CALL lbc_lnk( p_fr1  (:,:) , 'T', 1. )
      CALL lbc_lnk( p_fr2  (:,:) , 'T', 1. )
      DO jl = 1, ijpl
         CALL lbc_lnk( p_qns (:,:,jl) , 'T', 1. )
         CALL lbc_lnk( p_dqns(:,:,jl) , 'T', 1. )
         CALL lbc_lnk( p_qla (:,:,jl) , 'T', 1. )
         CALL lbc_lnk( p_dqla(:,:,jl) , 'T', 1. )
      END DO

!!gm : mask is not required on forcing
      DO jl = 1, ijpl
         p_qns (:,:,jl) = p_qns (:,:,jl) * tmask(:,:,1)
         p_qla (:,:,jl) = p_qla (:,:,jl) * tmask(:,:,1)
         p_dqns(:,:,jl) = p_dqns(:,:,jl) * tmask(:,:,1)
         p_dqla(:,:,jl) = p_dqla(:,:,jl) * tmask(:,:,1)
      END DO

      IF(ln_ctl) THEN
         CALL prt_ctl(tab3d_1=z_qsb  , clinfo1=' blk_ice_clio: z_qsb  : ', tab3d_2=z_qlw  , clinfo2=' z_qlw  : ', kdim=ijpl)
         CALL prt_ctl(tab3d_1=p_qla  , clinfo1=' blk_ice_clio: z_qla  : ', tab3d_2=p_qsr  , clinfo2=' p_qsr  : ', kdim=ijpl)
         CALL prt_ctl(tab3d_1=p_dqns , clinfo1=' blk_ice_clio: p_dqns : ', tab3d_2=p_qns  , clinfo2=' p_qns  : ', kdim=ijpl)
         CALL prt_ctl(tab3d_1=p_dqla , clinfo1=' blk_ice_clio: p_dqla : ', tab3d_2=pst    , clinfo2=' pst    : ', kdim=ijpl)
         CALL prt_ctl(tab2d_1=p_tpr  , clinfo1=' blk_ice_clio: p_tpr  : ', tab2d_2=p_spr  , clinfo2=' p_spr  : ')
         CALL prt_ctl(tab2d_1=p_taui , clinfo1=' blk_ice_clio: p_taui : ', tab2d_2=p_tauj , clinfo2=' p_tauj : ')
      ENDIF

      CALL wrk_dealloc( jpi,jpj, ztatm, zqatm, zevsqr, zrhoa )
      CALL wrk_dealloc( jpi,jpj,pdim, z_qlw, z_qsb )
      !
      IF( nn_timing == 1 )  CALL timing_stop('blk_ice_clio')
      !
   END SUBROUTINE blk_ice_clio


   SUBROUTINE blk_clio_qsr_oce( pqsr_oce )
      !!---------------------------------------------------------------------------
      !!                     ***  ROUTINE blk_clio_qsr_oce  ***
      !!                 
      !!  ** Purpose :   Computation of the shortwave radiation at the ocean and the
      !!               snow/ice surfaces. 
      !!         
      !!  ** Method  : - computed qsr from the cloud cover for both ice and ocean 
      !!               - also initialise sbudyko and stauc once for all 
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj)     ::   pqsr_oce    ! shortwave radiation  over the ocean
      !!
      INTEGER, PARAMETER  ::   jp24 = 24   ! sampling of the daylight period (sunrise to sunset) into 24 equal parts
      !!      
      INTEGER  ::   ji, jj, jt    ! dummy loop indices 
      INTEGER  ::   indaet            !  = -1, 0, 1 for odd, normal and leap years resp.
      INTEGER  ::   iday              ! integer part of day
      INTEGER  ::   indxb, indxc      ! index for cloud depth coefficient

      REAL(wp)  ::   zalat , zclat, zcmue, zcmue2    ! local scalars 
      REAL(wp)  ::   zmt1, zmt2, zmt3                ! 
      REAL(wp)  ::   zdecl, zsdecl , zcdecl          ! 
      REAL(wp)  ::   za_oce, ztamr                   !

      REAL(wp) ::   zdl, zlha                        ! local scalars
      REAL(wp) ::   zlmunoon, zcldcor, zdaycor       !   
      REAL(wp) ::   zxday, zdist, zcoef, zcoef1      !
      REAL(wp) ::   zes
      
      REAL(wp), DIMENSION(:,:), POINTER ::   zev          ! vapour pressure
      REAL(wp), DIMENSION(:,:), POINTER ::   zdlha, zlsrise, zlsset     ! 2D workspace
      REAL(wp), DIMENSION(:,:), POINTER ::   zps, zpc   ! sine (cosine) of latitude per sine (cosine) of solar declination 
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('blk_clio_qsr_oce')
      !
      CALL wrk_alloc( jpi,jpj, zev, zdlha, zlsrise, zlsset, zps, zpc )

      IF( lbulk_init ) THEN             !   Initilization at first time step only
         rdtbs2 = nn_fsbc * rdt * 0.5
         ! cloud optical depths as a function of latitude (Chou et al., 1981).
         ! and the correction factor for taking into account  the effect of clouds 
         DO jj = 1, jpj
            DO ji = 1 , jpi
               zalat          = ( 90.e0 - ABS( gphit(ji,jj) ) ) /  5.e0
               zclat          = ( 95.e0 -      gphit(ji,jj)   ) / 10.e0
               indxb          = 1 + INT( zalat )
               indxc          = 1 + INT( zclat )
               zdl            = zclat - INT( zclat )
               !  correction factor to account for the effect of clouds
               sbudyko(ji,jj) = budyko(indxb)
               stauc  (ji,jj) = ( 1.e0 - zdl ) * tauco( indxc ) + zdl * tauco( indxc + 1 )
            END DO
         END DO
         lbulk_init = .FALSE.
      ENDIF


      ! Saturated water vapour and vapour pressure
      ! ------------------------------------------
!CDIR NOVERRCHK
!CDIR COLLAPSE
      DO jj = 1, jpj
!CDIR NOVERRCHK
         DO ji = 1, jpi
            ztamr = sf(jp_tair)%fnow(ji,jj,1) - rtt
            zmt1  = SIGN( 17.269,  ztamr )
            zmt2  = SIGN( 21.875,  ztamr )
            zmt3  = SIGN( 28.200, -ztamr )
            zes = 611.0 * EXP(  ABS( ztamr ) * MIN ( zmt1, zmt2 )   &              ! Saturation water vapour
               &                     / ( sf(jp_tair)%fnow(ji,jj,1) - 35.86  + MAX( 0.e0, zmt3 ) )  )
            zev(ji,jj) = sf(jp_humi)%fnow(ji,jj,1) * zes * 1.0e-05                 ! vapour pressure  
         END DO
      END DO

      !-----------------------------------!
      !  Computation of solar irradiance  !
      !-----------------------------------!
!!gm : hard coded  leap year ???
      indaet   = 1                                    ! = -1, 0, 1 for odd, normal and leap years resp.
      zxday = nday_year + rdtbs2 / rday               ! day of the year at which the fluxes are calculated
      iday  = INT( zxday )                            ! (centred at the middle of the ice time step)
      CALL flx_blk_declin( indaet, iday, zdecl )      ! solar declination of the current day
      zsdecl = SIN( zdecl * rad )                     ! its sine
      zcdecl = COS( zdecl * rad )                     ! its cosine


      !  correction factor added for computation of shortwave flux to take into account the variation of
      !  the distance between the sun and the earth during the year (Oberhuber 1988)
      zdist    = zxday * 2. * rpi / REAL(nyear_len(1), wp)
      zdaycor  = 1.0 + 0.0013 * SIN( zdist ) + 0.0342 * COS( zdist )

!CDIR NOVERRCHK
      DO jj = 1, jpj
!CDIR NOVERRCHK
         DO ji = 1, jpi
            !  product of sine (cosine) of latitude and sine (cosine) of solar declination
            zps(ji,jj) = SIN( gphit(ji,jj) * rad ) * zsdecl
            zpc(ji,jj) = COS( gphit(ji,jj) * rad ) * zcdecl
            !  computation of the both local time of sunrise and sunset
            zlsrise(ji,jj) = ACOS( - SIGN( 1.e0, zps(ji,jj) )    &
               &                   * MIN(  1.e0, SIGN( 1.e0, zps(ji,jj) ) * ( zps(ji,jj) / zpc(ji,jj) )  )   )
            zlsset (ji,jj) = - zlsrise(ji,jj)
            !  dividing the solar day into jp24 segments of length zdlha
            zdlha  (ji,jj) = ( zlsrise(ji,jj) - zlsset(ji,jj) ) / REAL( jp24, wp )
         END DO
      END DO


      !---------------------------------------------!
      !  shortwave radiation absorbed by the ocean  !
      !---------------------------------------------!
      pqsr_oce(:,:)   = 0.e0      ! set ocean qsr to zero      

      ! compute and sum ocean qsr over the daylight (i.e. between sunrise and sunset)
!CDIR NOVERRCHK   
      DO jt = 1, jp24
         zcoef = FLOAT( jt ) - 0.5
!CDIR NOVERRCHK     
!CDIR COLLAPSE
         DO jj = 1, jpj
!CDIR NOVERRCHK
            DO ji = 1, jpi
               zlha = COS(  zlsrise(ji,jj) - zcoef * zdlha(ji,jj)  )                  ! local hour angle
               zcmue              = MAX( 0.e0 ,   zps(ji,jj) + zpc(ji,jj) * zlha  )   ! cos of local solar altitude
               zcmue2             = 1368.0 * zcmue * zcmue

               ! ocean albedo depending on the cloud cover (Payne, 1972)
               za_oce     = ( 1.0 - sf(jp_ccov)%fnow(ji,jj,1) ) * 0.05 / ( 1.1 * zcmue**1.4 + 0.15 )   &   ! clear sky
                  &       +         sf(jp_ccov)%fnow(ji,jj,1)   * 0.06                                     ! overcast

                  ! solar heat flux absorbed by the ocean (Zillman, 1972)
               pqsr_oce(ji,jj) = pqsr_oce(ji,jj)                                         &
                  &            + ( 1.0 - za_oce ) * zdlha(ji,jj) * zcmue2                &
                  &            / ( ( zcmue + 2.7 ) * zev(ji,jj) + 1.085 * zcmue +  0.10 )
            END DO
         END DO
      END DO
      ! Taking into account the ellipsity of the earth orbit, the clouds AND masked if sea-ice cover > 0%
      zcoef1 = srgamma * zdaycor / ( 2. * rpi )
!CDIR COLLAPSE
      DO jj = 1, jpj
         DO ji = 1, jpi
            zlmunoon = ASIN( zps(ji,jj) + zpc(ji,jj) ) / rad                         ! local noon solar altitude
            zcldcor  = MIN(  1.e0, ( 1.e0 - 0.62 * sf(jp_ccov)%fnow(ji,jj,1)   &     ! cloud correction (Reed 1977)
               &                          + 0.0019 * zlmunoon )                 )
            pqsr_oce(ji,jj) = zcoef1 * zcldcor * pqsr_oce(ji,jj) * tmask(ji,jj,1)    ! and zcoef1: ellipsity
         END DO
      END DO

      CALL wrk_dealloc( jpi,jpj, zev, zdlha, zlsrise, zlsset, zps, zpc )
      !
      IF( nn_timing == 1 )  CALL timing_stop('blk_clio_qsr_oce')
      !
   END SUBROUTINE blk_clio_qsr_oce


   SUBROUTINE blk_clio_qsr_ice( pa_ice_cs, pa_ice_os, pqsr_ice )
      !!---------------------------------------------------------------------------
      !!                     ***  ROUTINE blk_clio_qsr_ice  ***
      !!                 
      !!  ** Purpose :   Computation of the shortwave radiation at the ocean and the
      !!               snow/ice surfaces. 
      !!         
      !!  ** Method  : - computed qsr from the cloud cover for both ice and ocean 
      !!               - also initialise sbudyko and stauc once for all 
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:) ::   pa_ice_cs   ! albedo of ice under clear sky
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:) ::   pa_ice_os   ! albedo of ice under overcast sky
      REAL(wp), INTENT(  out), DIMENSION(:,:,:) ::   pqsr_ice    ! shortwave radiation over the ice/snow
      !!
      INTEGER, PARAMETER  ::   jp24 = 24   ! sampling of the daylight period (sunrise to sunset) into 24 equal parts
      !!
      INTEGER  ::   ji, jj, jl, jt    ! dummy loop indices
      INTEGER  ::   ijpl              ! number of ice categories (3rd dim of pqsr_ice)
      INTEGER  ::   indaet            !  = -1, 0, 1 for odd, normal and leap years resp.
      INTEGER  ::   iday              ! integer part of day
      !!
      REAL(wp) ::   zcmue, zcmue2, ztamr          ! temporary scalars 
      REAL(wp) ::   zmt1, zmt2, zmt3              !    -         -
      REAL(wp) ::   zdecl, zsdecl, zcdecl         !    -         -
      REAL(wp) ::   zlha, zdaycor, zes            !    -         -
      REAL(wp) ::   zxday, zdist, zcoef, zcoef1   !    -         -
      REAL(wp) ::   zqsr_ice_cs, zqsr_ice_os      !    -         -

      REAL(wp), DIMENSION(:,:), POINTER ::   zev                      ! vapour pressure
      REAL(wp), DIMENSION(:,:), POINTER ::   zdlha, zlsrise, zlsset   ! 2D workspace
      REAL(wp), DIMENSION(:,:), POINTER ::   zps, zpc   ! sine (cosine) of latitude per sine (cosine) of solar declination 
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('blk_clio_qsr_ice')
      !
      CALL wrk_alloc( jpi,jpj, zev, zdlha, zlsrise, zlsset, zps, zpc )

      ijpl = SIZE(pqsr_ice, 3 )      ! number of ice categories
      
      ! Saturated water vapour and vapour pressure
      ! ------------------------------------------
!CDIR NOVERRCHK
!CDIR COLLAPSE
      DO jj = 1, jpj
!CDIR NOVERRCHK
         DO ji = 1, jpi           
            ztamr = sf(jp_tair)%fnow(ji,jj,1) - rtt           
            zmt1  = SIGN( 17.269,  ztamr )
            zmt2  = SIGN( 21.875,  ztamr )
            zmt3  = SIGN( 28.200, -ztamr )
            zes = 611.0 * EXP(  ABS( ztamr ) * MIN ( zmt1, zmt2 )   &              ! Saturation water vapour
               &                     / ( sf(jp_tair)%fnow(ji,jj,1) - 35.86  + MAX( 0.e0, zmt3 ) )  )
            zev(ji,jj) = sf(jp_humi)%fnow(ji,jj,1) * zes * 1.0e-05                 ! vapour pressure  
         END DO
      END DO

      !-----------------------------------!
      !  Computation of solar irradiance  !
      !-----------------------------------!
!!gm : hard coded  leap year ???
      indaet   = 1                                    ! = -1, 0, 1 for odd, normal and leap years resp.
      zxday = nday_year + rdtbs2 / rday               ! day of the year at which the fluxes are calculated
      iday  = INT( zxday )                            ! (centred at the middle of the ice time step)
      CALL flx_blk_declin( indaet, iday, zdecl )      ! solar declination of the current day
      zsdecl = SIN( zdecl * rad )                     ! its sine
      zcdecl = COS( zdecl * rad )                     ! its cosine

      
      !  correction factor added for computation of shortwave flux to take into account the variation of
      !  the distance between the sun and the earth during the year (Oberhuber 1988)
      zdist    = zxday * 2. * rpi / REAL(nyear_len(1), wp)
      zdaycor  = 1.0 + 0.0013 * SIN( zdist ) + 0.0342 * COS( zdist )

!CDIR NOVERRCHK
      DO jj = 1, jpj
!CDIR NOVERRCHK
         DO ji = 1, jpi
            !  product of sine (cosine) of latitude and sine (cosine) of solar declination
            zps(ji,jj) = SIN( gphit(ji,jj) * rad ) * zsdecl
            zpc(ji,jj) = COS( gphit(ji,jj) * rad ) * zcdecl
            !  computation of the both local time of sunrise and sunset
            zlsrise(ji,jj) = ACOS( - SIGN( 1.e0, zps(ji,jj) )    &
               &                   * MIN(  1.e0, SIGN( 1.e0, zps(ji,jj) ) * ( zps(ji,jj) / zpc(ji,jj) )  )   ) 
            zlsset (ji,jj) = - zlsrise(ji,jj)
            !  dividing the solar day into jp24 segments of length zdlha
            zdlha  (ji,jj) = ( zlsrise(ji,jj) - zlsset(ji,jj) ) / REAL( jp24, wp )
         END DO
      END DO


      !---------------------------------------------!
      !  shortwave radiation absorbed by the ice    !
      !---------------------------------------------!
      ! compute and sum ice qsr over the daylight for each ice categories
      pqsr_ice(:,:,:) = 0.e0
      zcoef1 = zdaycor / ( 2. * rpi )       ! Correction for the ellipsity of the earth orbit
      
      !                    !----------------------------! 
      DO jl = 1, ijpl      !  loop over ice categories  !
         !                 !----------------------------! 
!CDIR NOVERRCHK   
         DO jt = 1, jp24   
            zcoef = FLOAT( jt ) - 0.5
!CDIR NOVERRCHK     
!CDIR COLLAPSE
            DO jj = 1, jpj
!CDIR NOVERRCHK
               DO ji = 1, jpi
                  zlha = COS(  zlsrise(ji,jj) - zcoef * zdlha(ji,jj)  )                  ! local hour angle
                  zcmue              = MAX( 0.e0 ,   zps(ji,jj) + zpc(ji,jj) * zlha  )   ! cos of local solar altitude
                  zcmue2             = 1368.0 * zcmue * zcmue
                  
                  !  solar heat flux absorbed by the ice/snow system (Shine and Crane 1984 adapted to high albedo) 
                  zqsr_ice_cs =  ( 1.0 - pa_ice_cs(ji,jj,jl) ) * zdlha(ji,jj) * zcmue2        &   ! clear sky
                     &        / ( ( 1.0 + zcmue ) * zev(ji,jj) + 1.2 * zcmue + 0.0455 )
                  zqsr_ice_os = zdlha(ji,jj) * SQRT( zcmue )                                  &   ! overcast sky
                     &        * ( 53.5 + 1274.5 * zcmue )      * ( 1.0 - 0.996  * pa_ice_os(ji,jj,jl) )    &
                     &        / (  1.0 + 0.139  * stauc(ji,jj) * ( 1.0 - 0.9435 * pa_ice_os(ji,jj,jl) ) )       
             
                  pqsr_ice(ji,jj,jl) = pqsr_ice(ji,jj,jl) + (  ( 1.0 - sf(jp_ccov)%fnow(ji,jj,1) ) * zqsr_ice_cs    &
                     &                                       +         sf(jp_ccov)%fnow(ji,jj,1)   * zqsr_ice_os  )
               END DO
            END DO
         END DO
         !
         ! Correction : Taking into account the ellipsity of the earth orbit
         pqsr_ice(:,:,jl) = pqsr_ice(:,:,jl) * zcoef1 * tmask(:,:,1)
         !
         !                 !--------------------------------! 
      END DO               !  end loop over ice categories  !
      !                    !--------------------------------! 


!!gm  : this should be suppress as input data have been passed through lbc_lnk
      DO jl = 1, ijpl
         CALL lbc_lnk( pqsr_ice(:,:,jl) , 'T', 1. )
      END DO
      !
      CALL wrk_dealloc( jpi,jpj, zev, zdlha, zlsrise, zlsset, zps, zpc )
      !
      IF( nn_timing == 1 )  CALL timing_stop('blk_clio_qsr_ice')
      !
   END SUBROUTINE blk_clio_qsr_ice


   SUBROUTINE flx_blk_declin( ky, kday, pdecl )
      !!---------------------------------------------------------------------------
      !!               ***  ROUTINE flx_blk_declin  ***
      !!          
      !! ** Purpose :   Computation of the solar declination for the day
      !!       
      !! ** Method  :   ???
      !!---------------------------------------------------------------------
      INTEGER , INTENT(in   ) ::   ky      ! = -1, 0, 1 for odd, normal and leap years resp.
      INTEGER , INTENT(in   ) ::   kday    ! day of the year ( kday = 1 on january 1)
      REAL(wp), INTENT(  out) ::   pdecl   ! solar declination
      !!
      REAL(wp) ::   a0  =  0.39507671      ! coefficients for solar declinaison computation
      REAL(wp) ::   a1  = 22.85684301      !     "              ""                 "
      REAL(wp) ::   a2  = -0.38637317      !     "              ""                 "
      REAL(wp) ::   a3  =  0.15096535      !     "              ""                 "
      REAL(wp) ::   a4  = -0.00961411      !     "              ""                 "
      REAL(wp) ::   b1  = -4.29692073      !     "              ""                 "
      REAL(wp) ::   b2  =  0.05702074      !     "              ""                 "
      REAL(wp) ::   b3  = -0.09028607      !     "              ""                 "
      REAL(wp) ::   b4  =  0.00592797
      !!
      REAL(wp) ::   zday   ! corresponding day of type year (cf. ky)
      REAL(wp) ::   zp     ! temporary scalars
      !!---------------------------------------------------------------------
            
      IF    ( ky == 1 )  THEN   ;   zday = REAL( kday, wp ) - 0.5
      ELSEIF( ky == 3 )  THEN   ;   zday = REAL( kday, wp ) - 1.
      ELSE                      ;   zday = REAL( kday, wp )
      ENDIF
      
      zp = rpi * ( 2.0 * zday - 367.0 ) / REAL(nyear_len(1), wp)
      
      pdecl  = a0                                                                      &
         &   + a1 * COS( zp ) + a2 * COS( 2. * zp ) + a3 * COS( 3. * zp ) + a4 * COS( 4. * zp )   &
         &   + b1 * SIN( zp ) + b2 * SIN( 2. * zp ) + b3 * SIN( 3. * zp ) + b4 * SIN( 4. * zp )
      !
   END SUBROUTINE flx_blk_declin

   !!======================================================================
END MODULE sbcblk_clio
