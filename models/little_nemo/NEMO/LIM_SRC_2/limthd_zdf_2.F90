MODULE limthd_zdf_2
   !!======================================================================
   !!                       ***  MODULE limthd_zdf_2 ***
   !!                thermodynamic growth and decay of the ice 
   !!======================================================================
   !! History :  1.0  !  01-04 (LIM) Original code
   !!            2.0  !  02-08 (C. Ethe, G. Madec) F90
   !!----------------------------------------------------------------------
#if defined key_lim2
   !!----------------------------------------------------------------------
   !!   'key_lim2'                                    LIM 2.0 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_thd_zdf_2 : vertical accr./abl. and lateral ablation of sea ice
   !!----------------------------------------------------------------------
   USE par_oce          ! ocean parameters
   USE phycst           ! ???
   USE thd_ice_2
   USE ice_2
   USE limistate_2
   USE in_out_manager
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays
   USE cpl_oasis3, ONLY : lk_cpl
      
   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_thd_zdf_2        ! called by lim_thd_2

   REAL(wp) ::   epsi20 = 1.e-20  ,  &  ! constant values
      &          epsi13 = 1.e-13  ,  &
      &          zzero  = 0.e0    ,  &
      &          zone   = 1.e0
   !!----------------------------------------------------------------------
   !! NEMO/LIM2 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: limthd_zdf_2.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_thd_zdf_2( kideb , kiut )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_thd_zdf_2  ***
      !!              
      !! ** Purpose : This routine determines the time evolution of snow 
      !!      and sea-ice thicknesses, concentration and heat content 
      !!      due to the vertical and lateral thermodynamic accretion-
      !!      ablation processes. One only treats the case of lat. abl.
      !!      For lateral accretion, see routine lim_lat_accr 
      !! 
      !! ** Method  : The representation of vertical growth and decay of 
      !!      the sea-ice model is based upon the diffusion of heat 
      !!      through the external and internal boundaries of a 
      !!      three-layer system (two layers of ice and one layer and 
      !!      one layer of snow, if present, on top of the ice).
      !! 
      !! ** Action  : - Calculation of some intermediates variables
      !!              - Calculation of surface temperature
      !!              - Calculation of available heat for surface ablation
      !!              - Calculation of the changes in internal temperature 
      !!                of the three-layer system, due to vertical diffusion 
      !!                processes
      !!              - Performs surface ablation and bottom accretion-ablation
      !!              - Performs snow-ice formation
      !!              - Performs lateral ablation
      !!
      !! References : Fichefet T. and M. Maqueda 1997, J. Geophys. Res., 102(C6), 12609-12646   
      !!              Fichefet T. and M. Maqueda 1999, Clim. Dyn, 15(4), 251-268  
      !!------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kideb    ! Start point on which the  the computation is applied
      INTEGER, INTENT(in) ::   kiut     ! End point on which the  the computation is applied
      !!
      INTEGER ::   ji       ! dummy loop indices
      REAL(wp), POINTER, DIMENSION(:) ::   zqcmlts        ! energy due to surface melting
      REAL(wp), POINTER, DIMENSION(:) ::   zqcmltb        ! energy due to bottom melting
      REAL(wp), POINTER, DIMENSION(:) ::   ztsmlt         ! snow/ice surface melting temperature
      REAL(wp), POINTER, DIMENSION(:) ::   ztbif          ! int. temp. at the mid-point of the 1st layer of the snow/ice sys. 
      REAL(wp), POINTER, DIMENSION(:) ::   zksn           ! effective conductivity of snow
      REAL(wp), POINTER, DIMENSION(:) ::   zkic           ! effective conductivity of ice
      REAL(wp), POINTER, DIMENSION(:) ::   zksndh         ! thermal cond. at the mid-point of the 1st layer of the snow/ice sys. 
      REAL(wp), POINTER, DIMENSION(:) ::   zfcsu          ! conductive heat flux at the surface of the snow/ice system 
      REAL(wp), POINTER, DIMENSION(:) ::   zfcsudt        ! = zfcsu * dt
      REAL(wp), POINTER, DIMENSION(:) ::   zi0            ! frac. of the net SW rad. which is not absorbed at the surface
      REAL(wp), POINTER, DIMENSION(:) ::   z1mi0          ! fraction of the net SW radiation absorbed at the surface
      REAL(wp), POINTER, DIMENSION(:) ::   zqmax          ! maximum energy stored in brine pockets
      REAL(wp), POINTER, DIMENSION(:) ::   zrcpdt         ! h_su*rho_su*cp_su/dt(h_su being the thick. of surf. layer)
      REAL(wp), POINTER, DIMENSION(:) ::   zts_old        ! previous surface temperature
      REAL(wp), POINTER, DIMENSION(:) ::   zidsn , z1midsn , zidsnic ! tempory variables
      REAL(wp), POINTER, DIMENSION(:) ::   zfnet          ! net heat flux at the top surface( incl. conductive heat flux)
      REAL(wp), POINTER, DIMENSION(:) ::   zsprecip       ! snow accumulation
      REAL(wp), POINTER, DIMENSION(:) ::   zhsnw_old      ! previous snow thickness
      REAL(wp), POINTER, DIMENSION(:) ::   zdhictop       ! change in ice thickness due to top surf ablation/accretion
      REAL(wp), POINTER, DIMENSION(:) ::   zdhicbot       ! change in ice thickness due to bottom surf abl/acc
      REAL(wp), POINTER, DIMENSION(:) ::   zqsup          ! energy transmitted to ocean (coming from top surf abl/acc)
      REAL(wp), POINTER, DIMENSION(:) ::   zqocea         ! energy transmitted to ocean (coming from bottom sur abl/acc)
      REAL(wp), POINTER, DIMENSION(:) ::   zfrl_old       ! previous sea/ice fraction
      REAL(wp), POINTER, DIMENSION(:) ::   zfrld_1d       ! new sea/ice fraction
      REAL(wp), POINTER, DIMENSION(:) ::   zep            ! internal temperature of the 2nd layer of the snow/ice system
      REAL(wp), DIMENSION(3) :: & 
          zplediag  &    ! principle diagonal, subdiag. and supdiag. of the 
          , zsubdiag  &    ! tri-diagonal matrix coming from the computation
          , zsupdiag  &    ! of the temperatures inside the snow-ice system
          , zsmbr          ! second member
       REAL(wp) :: & 
          zhsu     &     ! thickness of surface layer
          , zhe      &     ! effective thickness for compu. of equ. thermal conductivity
          , zheshth  &     ! = zhe / thth
          , zghe     &     ! correction factor of the thermal conductivity
          , zumsb    &     ! parameter for numerical method to solve heat-diffusion eq.
          , zkhsn    &     ! conductivity at the snow layer
          , zkhic    &     ! conductivity at the ice layers
          , zkint    &     ! equivalent conductivity at the snow-ice interface
          , zkhsnint &     ! = zkint*dt / (hsn*rhosn*cpsn)  
          , zkhicint &     ! = 2*zkint*dt / (hic*rhoic*cpic)
          , zpiv1 , zpiv2  &       ! tempory scalars used to solve the tri-diagonal system
          , zb2 , zd2 , zb3 , zd3 &
          , ztint          ! equivalent temperature at the snow-ice interface
       REAL(wp) :: & 
          zexp      &     ! exponential function of the ice thickness
          , zfsab     &     ! part of solar radiation stored in brine pockets
          , zfts      &     ! value of energy balance function when the temp. equal surf. temp.
          , zdfts     &     ! value of derivative of ztfs when the temp. equal surf. temp.
          , zdts      &     ! surface temperature increment
          , zqsnw_mlt &     ! energy needed to melt snow
          , zdhsmlt   &     ! change in snow thickness due to melt
          , zhsn      &     ! snow thickness (previous+accumulation-melt)
          , zqsn_mlt_rem &  ! remaining heat coming from snow melting
          , zqice_top_mlt & ! energy used to melt ice at top surface
          , zdhssub      &  ! change in snow thick. due to sublimation or evaporation
          , zdhisub      &  ! change in ice thick. due to sublimation or evaporation    
          , zdhsn        &  ! snow ice thickness increment
          , zdtsn        &  ! snow internal temp. increment
          , zdtic        &  ! ice internal temp. increment
          , zqnes          ! conductive energy due to ice melting in the first ice layer
       REAL(wp) :: & 
          ztbot     &      ! temperature at the bottom surface
          , zfcbot    &      ! conductive heat flux at bottom surface
          , zqice_bot &      ! energy used for bottom melting/growing
          , zqice_bot_mlt &  ! energy used for bottom melting
          , zqstbif_bot  &  ! part of energy stored in brine pockets used for bottom melting
          , zqstbif_old  &  ! tempory var. for zqstbif_bot
          , zdhicmlt      &  ! change in ice thickness due to bottom melting
          , zdhicm        &  ! change in ice thickness var. 
          , zdhsnm        &  ! change in snow thickness var. 
          , zhsnfi        &  ! snow thickness var. 
          , zc1, zpc1, zc2, zpc2, zp1, zp2 & ! tempory variables
          , ztb2, ztb3
       REAL(wp) :: & 
          zdrmh         &   ! change in snow/ice thick. after snow-ice formation
          , zhicnew       &   ! new ice thickness
          , zhsnnew       &   ! new snow thickness
          , zquot , ztneq &   ! tempory temp. variables
          , zqice, zqicetot & ! total heat inside the snow/ice system
          , zdfrl         &   ! change in ice concentration
          , zdvsnvol      &   ! change in snow volume
          , zdrfrl1, zdrfrl2 &  ! tempory scalars
          , zihsn, zidhb, zihic, zihe, zihq, ziexp, ziqf, zihnf, zibmlt, ziqr, zihgnew, zind
       !!----------------------------------------------------------------------
       CALL wrk_alloc( jpij, ztsmlt, ztbif  , zksn    , zkic    , zksndh , zfcsu  , zfcsudt , zi0      , z1mi0   , zqmax    )
       CALL wrk_alloc( jpij, zrcpdt, zts_old, zidsn   , z1midsn , zidsnic, zfnet  , zsprecip, zhsnw_old, zdhictop, zdhicbot )
       CALL wrk_alloc( jpij, zqsup , zqocea , zfrl_old, zfrld_1d, zep    , zqcmlts, zqcmltb                                 )

       !-----------------------------------------------------------------------
       !  1. Boundaries conditions for snow/ice system internal temperature
       !       - If tbif_1d(ji,1) > rt0_snow, tbif_1d(ji,1) = rt0_snow 
       !       - If tbif_1d(ji,2/3) > rt0_ice, tbif_1d(ji,2/3) = rt0_ice 
       !     Computation of energies due to surface and bottom melting 
       !-----------------------------------------------------------------------
       
       DO ji = kideb , kiut
          zihsn = MAX( zzero , SIGN( zone , hsndif - h_snow_1d(ji) ) )
          zihic = MAX( zzero , SIGN( zone , hicdif - h_ice_1d(ji) ) )
          !--computation of energy due to surface melting
          zqcmlts(ji) = ( MAX ( zzero ,  &
             &                   rcpsn * h_snow_1d(ji) * ( tbif_1d(ji,1) - rt0_snow ) ) ) * ( 1.0 - zihsn )
          !--computation of energy due to bottom melting
          zqcmltb(ji) = ( MAX( zzero , &
             &                  rcpic * ( tbif_1d(ji,2) - rt0_ice ) * ( h_ice_1d(ji) / 2. ) ) &
             &           + MAX( zzero , &
             &                  rcpic * ( tbif_1d(ji,3) - rt0_ice ) * ( h_ice_1d(ji) / 2. ) ) &
             &           ) * ( 1.0 - zihic  )
          !--limitation of  snow/ice system internal temperature
          tbif_1d(ji,1)   = MIN( rt0_snow, tbif_1d(ji,1) )
          tbif_1d(ji,2)   = MIN( rt0_ice , tbif_1d(ji,2) )
          tbif_1d(ji,3)   = MIN( rt0_ice , tbif_1d(ji,3) )
       END DO

       !-------------------------------------------
       !  2. Calculate some intermediate variables.  
       !-------------------------------------------
       
       ! initialisation of the thickness of surface layer
       zhsu = hnzst  

       DO ji = kideb , kiut
          zind   = MAX( zzero , SIGN( zone , zhsu - h_snow_1d(ji) ) )
          zihsn  = MAX( zzero , SIGN( zone , hsndif - h_snow_1d(ji) ) )
          zihsn  = MAX( zihsn , zind )
          zihic  = MAX( zzero , sign( zone , hicdif - h_ice_1d(ji) ) )
          !     2.1. Computation of surface melting temperature
          !----------------------------------------------------
          zind  = MAX( zzero , SIGN( zone , -h_snow_1d(ji) ) )
          ztsmlt(ji) = ( 1.0 - zind ) * rt0_snow + zind * rt0_ice
          !
          !     2.2. Effective conductivity of snow and ice
          !-----------------------------------------------

          !---computation of the correction factor on the thermal conductivity
          !-- (Morales Maqueda, 1995 ; Fichefet and Morales Maqueda, 1997)
          zhe      =  ( rcdsn / ( rcdsn + rcdic ) ) * h_ice_1d(ji)   &
             &     + ( rcdic / ( rcdsn + rcdic ) ) * h_snow_1d(ji) 
          zihe     = MAX( zzero , SIGN( zone , 2.0 * zhe - thth ) )
          zheshth  = zhe / thth
          zghe     = ( 1.0 - zihe ) * zheshth * ( 2.0 - zheshth )   &
             &     +         zihe   * 0.5 * ( 1.5 + LOG( 2.0 * zheshth ) )

          !---effective conductivities 
          zksn(ji)  = zghe * rcdsn  
          zkic(ji)  = zghe * rcdic

          !
          !     2.3. Computation of the conductive heat flux from the snow/ice 
          !          system interior toward the top surface
          !------------------------------------------------------------------

          !---Thermal conductivity at the mid-point of the first snow/ice system layer
          zksndh(ji) =   ( ( 1.0 - zihsn ) * 2.0 * zksn(ji) + zihsn * 4.0 * zkic(ji) )   &
             &         / ( ( 1.0 - zihsn ) *  h_snow_1d(ji)                              &
             &           +        zihsn   *  ( ( 1.0 + 3.0 * zihic ) * h_ice_1d(ji)      &
             &           + 4.0 * zkic(ji)/zksn(ji) * h_snow_1d(ji) ) )

          !---internal temperature at the mid-point of the first snow/ice system layer
          ztbif(ji)  = ( 1.0 - zihsn ) * tbif_1d(ji,1)                       &
             &       +         zihsn   * ( ( 1.0 - zihic ) * tbif_1d(ji,2)   &
             &       +         zihic   * tfu_1d(ji)   )
          !---conductive heat flux 
          zfcsu(ji) = zksndh(ji) * ( ztbif(ji) - sist_1d(ji) )

       END DO

       !--------------------------------------------------------------------
       !  3. Calculate : 
       !     - fstbif_1d, part of solar radiation absorbing inside the ice
       !       assuming an exponential absorption (Grenfell and Maykut, 1977)
       !     - zqmax,  maximum energy stored in brine pockets
       !     - qstbif_1d, total energy stored in brine pockets (updating)
       !-------------------------------------------------------------------

       DO ji = kideb , kiut
          zihsn  = MAX( zzero , SIGN (zone , -h_snow_1d(ji) ) )
          zihic  = MAX( zzero , 1.0 - ( h_ice_1d(ji) / zhsu ) )     
          zind   = MAX( zzero , SIGN (zone , hicdif - h_ice_1d(ji) ) )
          !--Computation of the fraction of the net shortwave radiation which
          !--penetrates inside the ice cover ( See Forcat)
          zi0(ji)  = zihsn * ( fr1_i0_1d(ji) + zihic * fr2_i0_1d(ji) )
          zexp     = MIN( zone , EXP( -1.5 * ( h_ice_1d(ji) - zhsu ) ) )
          fstbif_1d(ji) = zi0(ji) * qsr_ice_1d(ji) * zexp
          !--Computation of maximum energy stored in brine pockets zqmax and update
          !--the total energy stored in brine pockets, if less than zqmax
          zqmax(ji) = MAX( zzero , 0.5 * xlic * ( h_ice_1d(ji) - hicmin ) )
          zfsab   = zi0(ji) * qsr_ice_1d(ji) * ( 1.0 - zexp )
          zihq    = ( 1.0 - zind ) * MAX(zzero, SIGN( zone , qstbif_1d(ji) - zqmax(ji) ) ) &
             &    +         zind   * zone
          qstbif_1d(ji) = ( qstbif_1d(ji) + ( 1.0 - zihq ) * zfsab * rdt_ice ) * swiqst
          !--fraction of shortwave radiation absorbed at surface
          ziexp = zihq * zexp + ( 1.0 - zihq ) * ( swiqst + ( 1.0 - swiqst ) * zexp )
          z1mi0(ji) = 1.0 - zi0(ji) * ziexp
       END DO

       !--------------------------------------------------------------------------------
       !  4. Computation of the surface temperature : determined by considering the 
       !     budget of a thin layer of thick. zhsu at the top surface (H. Grenier, 1995) 
       !     and based on a surface energy balance : 
       !     hsu * rcp * dT/dt = Fsr + Fnsr(T) + Fcs(T),
       !     where - Fsr is the net absorbed solar radiation, 
       !           - Fnsr is the total non solar radiation (incoming and outgoing long-wave,
       !             sensible and latent heat fluxes)
       !           - Fcs the conductive heat flux at the top of surface
       !------------------------------------------------------------------------------

       !     4.1. Computation of intermediate values
       !---------------------------------------------
       DO ji = kideb, kiut
          zrcpdt(ji) = ( rcpsn * MIN( h_snow_1d(ji) , zhsu )    &
             &       + rcpic * MAX( zhsu - h_snow_1d(ji) , zzero ) ) / rdt_ice
          zts_old(ji) =  sist_1d(ji)
       END DO

       !     4.2. Computation of surface temperature by expanding the eq. of energy balance
       !          with Ts = Tp + DT. One obtain , F(Tp) + DT * DF(Tp) = 0
       !          where  - F(Tp) = Fsr + Fnsr(Tp) + Fcs(Tp) 
       !                 - DF(Tp)= (dFnsr(Tp)/dT) + (dFcs(Tp)/dT) - hsu*rcp/dt
       !---------------------------------------------------------------------------------

       DO ji = kideb, kiut
          !---computation of the derivative of energy balance function 
          zdfts    =  zksndh(ji)   & ! contribution of the conductive heat flux
             &      + zrcpdt(ji)   & ! contribution of hsu * rcp / dt
             &      - dqns_ice_1d (ji)     ! contribution of the total non solar radiation 
          !---computation of the energy balance function 
          zfts    = - z1mi0 (ji) * qsr_ice_1d(ji)   & ! net absorbed solar radiation
             &      - qns_ice_1d(ji)                & ! total non solar radiation
             &      - zfcsu (ji)                      ! conductive heat flux from the surface
          !---computation of surface temperature increment  
          zdts    = -zfts / zdfts
          !---computation of the new surface temperature 
          sist_1d(ji) = sist_1d(ji) + zdts
       END DO

       !----------------------------------------------------------------------------
       !  5. Boundary condition at the top surface
       !--    IF Tsb < Tmelt, Fnet = Fcs (the net heat flux equal the conductive heat flux)
       !      Otherwise Tsb = Tmelt and Qnet(Tmelt) > 0 
       !      Fnet(Tmelt) is therefore the net surface flux needed for melting
       !----------------------------------------------------------------------------
       
       
       !     5.1.  Limitation of surface temperature and update total non solar fluxes,
       !          latent heat flux and conductive flux at the top surface 
       !----------------------------------------------------------------------  
                     
       IF ( .NOT. lk_cpl ) THEN   ! duplicate the loop for performances issues
          DO ji = kideb, kiut
             sist_1d(ji) = MIN( ztsmlt(ji) , sist_1d(ji) )
             qns_ice_1d(ji) = qns_ice_1d(ji) + dqns_ice_1d(ji) * ( sist_1d(ji) - zts_old(ji) )
             qla_ice_1d(ji) = qla_ice_1d(ji) + dqla_ice_1d(ji) * ( sist_1d(ji) - zts_old(ji) )
             zfcsu(ji)  = zksndh(ji) * ( ztbif(ji) - sist_1d(ji) )
          END DO
       ELSE
          DO ji = kideb, kiut
             sist_1d(ji) = MIN( ztsmlt(ji) , sist_1d(ji) )
             qla_ice_1d(ji) = -9999.   ! default definition, not used as parsub = 0. in this case
             zfcsu(ji)  = zksndh(ji) * ( ztbif(ji) - sist_1d(ji) )
          END DO
       ENDIF

       !     5.2. Calculate available heat for surface ablation. 
       !---------------------------------------------------------------------

       DO ji = kideb, kiut
          zfnet(ji) = qns_ice_1d(ji) + z1mi0(ji) * qsr_ice_1d(ji) + zfcsu(ji)          
          zfnet(ji) = MAX( zzero , zfnet(ji) )
          zfnet(ji) = zfnet(ji) * MAX( zzero , SIGN( zone , sist_1d(ji) - ztsmlt(ji) ) )
       END DO

       !-------------------------------------------------------------------------
       !  6. Calculate changes in internal temperature due to vertical diffusion   
       !     processes. The evolution of this temperature is governed by the one-
       !     dimensionnal heat-diffusion equation. 
       !     Given the temperature tbif(1/2/3), at time m we solve a set
       !     of finite difference equations to obtain new tempe. Each tempe is coupled
       !     to the temp. immediatly above and below by heat conduction terms. Thus 
       !     we have a set of equations of the form A * T = B, where A is a tridiagonal
       !     matrix, T a vector whose components are the unknown new temp.
       !-------------------------------------------------------------------------
       
       !--parameter for the numerical methode use to solve the heat-diffusion equation
       !- implicit, explicit or Crank-Nicholson
       zumsb = 1.0 - sbeta  
       DO ji = kideb, kiut
          zidsn(ji)   = MAX ( zzero, SIGN( zone, hsndif - h_snow_1d(ji) ) ) 
          z1midsn(ji) = 1.0 - zidsn(ji)
          zihic       = MAX ( zzero, SIGN( zone, hicdif - h_ice_1d(ji) ) ) 
          zidsnic(ji) = zidsn(ji) *  zihic 
          zfcsudt(ji) = zfcsu(ji) * rdt_ice 
       END DO 
   
       DO ji = kideb, kiut

          !     6.1 Calculate intermediate variables.
          !----------------------------------------

          !--conductivity at the snow surface
          zkhsn = 2.0 * zksn(ji) * rdt_ice / rcpsn
          !--conductivity at the ice surface
          zkhic = 4.0 * zkic(ji) * rdt_ice / MAX( h_ice_1d(ji) * h_ice_1d(ji) * rcpic , epsi20 )
          !--conductivity at the snow/ice interface 
          zkint = 4.0 * zksn(ji) * zkic(ji)  &
             &        / ( zksn(ji) * h_ice_1d(ji) + 2.0 * zkic(ji) * h_snow_1d(ji) * z1midsn(ji)) 
          zkhsnint = zkint * rdt_ice / rcpsn
          zkhicint = zkint * 2.0 * rdt_ice / MAX( h_ice_1d(ji) * rcpic , epsi20 )
          
          !     6.2. Fulfill the linear system matrix.
          !-----------------------------------------
!$$$          zplediag(1) = 1 + sbeta * z1midsn(ji) * ( zkhsn + zkhsnint )       
          zplediag(1) =   zidsn(ji) + z1midsn(ji) * h_snow_1d(ji)   &
             &          + sbeta * z1midsn(ji) * zkhsnint 
          zplediag(2) = 1 + sbeta * ( z1midsn(ji) * zkhicint + zkhic ) 
          zplediag(3) = 1 + 3.0 * sbeta * zkhic   

          zsubdiag(1) =  0.e0              
          zsubdiag(2) = -1.e0 * z1midsn(ji) * sbeta * zkhicint
          zsubdiag(3) = -1.e0 * sbeta * zkhic 

          zsupdiag(1) = -1.e0 * z1midsn(ji) * sbeta * zkhsnint 
          zsupdiag(2) = zsubdiag(3)
          zsupdiag(3) =  0.e0
          
          !     6.3. Fulfill the idependent term vector.
          !-------------------------------------------
          
!$$$          zsmbr(1) = zidsn(ji) * sist_1d(ji) + z1midsn(ji) *   &
!$$$             &         ( tbif_1d(ji,1) + zkhsn * sist_1d(ji)
!$$$             &         - zumsb * ( zkhsn * tbif_1d(ji,1)
!$$$             &                   + zkhsnint * ( tbif_1d(ji,1) - tbif_1d(ji,2) ) ) )
          zsmbr(1) = zidsn(ji) * sist_1d(ji) + z1midsn(ji) *    &
             &       ( h_snow_1d(ji) * tbif_1d(ji,1) - ( zfcsudt(ji) / rcpsn )  &
             &       - zumsb * zkhsnint * ( tbif_1d(ji,1) - tbif_1d(ji,2) ) )

          zsmbr(2) =  tbif_1d(ji,2)  &
             &      - zidsn(ji) * ( 1.0 - zidsnic(ji) ) &
             &        * ( zfcsudt(ji) / MAX( h_ice_1d(ji) * rcpic , epsi20 ) ) &
             &      + zumsb * ( zkhicint * ( tbif_1d(ji,1) - tbif_1d(ji,2) ) &
             &                   - zkhic * ( tbif_1d(ji,2) - tbif_1d(ji,3) )  )

          zsmbr(3) =  tbif_1d(ji,3)  &
             &      + zkhic * ( 2.0 * tfu_1d(ji) &
             &                + zumsb * ( tbif_1d(ji,2) - 3.0 * tbif_1d(ji,3) ) )
          
          !     6.4. Solve the system (Gauss elimination method).
          !----------------------------------------------------
          
          zpiv1 = zsubdiag(2) / zplediag(1) 
          zb2   = zplediag(2) - zpiv1 * zsupdiag(1)
          zd2   = zsmbr(2) - zpiv1 * zsmbr(1)

          zpiv2 = zsubdiag(3) / zb2
          zb3   = zplediag(3) - zpiv2 * zsupdiag(2)
          zd3   = zsmbr(3) - zpiv2 * zd2

          tbif_1d(ji,3) = zd3 / zb3
          tbif_1d(ji,2) = ( zd2 - zsupdiag(2) * tbif_1d(ji,3) ) / zb2
          tbif_1d(ji,1) = ( zsmbr(1) - zsupdiag(1) * tbif_1d(ji,2) ) / zplediag(1)            

          !--- taking into account the particular case of  zidsnic(ji) = 1
          ztint =  (  zkic(ji) * h_snow_1d(ji) * tfu_1d (ji)    &
             &      + zksn(ji) * h_ice_1d(ji) * sist_1d(ji) )   &
             &   / ( zkic(ji) * h_snow_1d(ji) + zksn(ji) * h_ice_1d(ji) ) 

          tbif_1d(ji,1) = ( 1.0 - zidsnic(ji) ) * tbif_1d(ji,1)   &
             &                + zidsnic(ji)   * ( ztint + sist_1d(ji) ) / 2.0
          tbif_1d(ji,2) = ( 1.0 - zidsnic(ji) ) * tbif_1d(ji,2)   &
             &                + zidsnic(ji)   * ( 3.0 * ztint + tfu_1d(ji) ) / 4.0
          tbif_1d(ji,3) = ( 1.0 - zidsnic(ji) ) * tbif_1d(ji,3)   &
             &                + zidsnic(ji)   * ( ztint + 3.0 * tfu_1d(ji) ) / 4.0     
       END DO
 
       !----------------------------------------------------------------------
       !  9. Take into account surface ablation and bottom accretion-ablation.|
       !----------------------------------------------------------------------
       
       !---Snow accumulation in one thermodynamic time step
       zsprecip(kideb:kiut) = sprecip_1d(kideb:kiut) * rdt_ice / rhosn


       DO ji = kideb, kiut
          
          !      9.1. Surface ablation and update of snow thickness and qstbif_1d
          !--------------------------------------------------------------------
          
          !--------------------------------------------------------------------------
          !--      Melting snow processes :
          !--      Melt at the upper surface is computed from the difference between 
          !--      the net heat flux (including the conductive heat flux) at the upper 
          !--      surface and the pre-existing energy due to surface melting
          !------------------------------------------------------------------------------
          
          !-- store the snow thickness
          zhsnw_old(ji) =  h_snow_1d(ji)
          !--computation of the energy needed to melt snow
          zqsnw_mlt  = zfnet(ji) * rdt_ice - zqcmlts(ji)
          !--change in snow thickness due to melt
          zdhsmlt = - zqsnw_mlt / xlsn
          
          !-- compute new snow thickness, taking into account the part of snow accumulation
          !   (as snow precipitation) and the part of snow lost due to melt
          zhsn =  h_snow_1d(ji) + zsprecip(ji) + zdhsmlt
          h_snow_1d(ji) = MAX( zzero , zhsn )
          !-- compute the volume of snow lost after surface melting and the associated mass
          dvsbq_1d(ji) =  ( 1.0 - frld_1d(ji) ) * ( h_snow_1d(ji) - zhsnw_old(ji) - zsprecip(ji) )
          dvsbq_1d(ji) =  MIN( zzero , dvsbq_1d(ji) )
          rdmsnif_1d(ji) =  rhosn * dvsbq_1d(ji)
          !-- If the snow is completely melted the remaining heat is used to melt ice
          zqsn_mlt_rem  = MAX( zzero , -zhsn ) * xlsn
          zqice_top_mlt = zqsn_mlt_rem 
          zqstbif_old   = qstbif_1d(ji)

          !--------------------------------------------------------------------------
          !--      Melting ice processes at the top surface :
          !--      The energy used to melt ice, zqice_top_mlt, is taken from the energy
          !--      stored in brine pockets qstbif_1d and the remaining energy coming 
          !--      from the melting snow process zqsn_mlt_rem.
          !--      If qstbif_1d > zqsn_mlt_rem then, one uses only a zqsn_mlt_rem part
          !--      of qstbif_1d to melt ice,
          !--         zqice_top_mlt = zqice_top_mlt + zqsn_mlt_rem
          !--         qstbif_1d = qstbif_1d - zqsn_mlt_rem
          !--      Otherwise one uses all qstbif_1d to melt ice
          !--         zqice_top_mlt = zqice_top_mlt + qstbif_1d
          !--         qstbif_1d = 0
          !------------------------------------------------------
          
          ziqf =  MAX ( zzero , SIGN( zone , qstbif_1d(ji) - zqsn_mlt_rem  ) )
          zqice_top_mlt =         ziqf   * ( zqice_top_mlt + zqsn_mlt_rem )   &
             &          + ( 1.0 - ziqf ) * ( zqice_top_mlt + qstbif_1d(ji)  )

          qstbif_1d(ji) =         ziqf   * ( qstbif_1d(ji) - zqsn_mlt_rem )   &
             &          + ( 1.0 - ziqf ) * ( qstbif_1d(ji) - qstbif_1d(ji)  )

          !--    The contribution of the energy stored in brine pockets qstbif_1d to melt
          !--    ice is taking into account only when qstbif_1d is less than zqmax. 
          !--    Otherwise, only the remaining energy coming from the melting snow 
          !--    process is used 
          zihq =  MAX ( zzero , SIGN( zone , qstbif_1d(ji) - zqmax(ji) ) )

          zqice_top_mlt =         zihq   * zqice_top_mlt   &
             &          + ( 1.0 - zihq ) * zqsn_mlt_rem

          qstbif_1d(ji) =         zihq   * qstbif_1d(ji)   &
             &          + ( 1.0 - zihq ) * zqstbif_old

          !--change in ice thickness due to melt at the top surface
          zdhictop(ji) = -zqice_top_mlt / xlic
          !--compute the volume formed after surface melting
          dvsbq_1d(ji) =  zdhictop(ji) * ( 1.0 - frld_1d(ji) )

          !-------------------------------------------------------------------------
          !--      A small variation at the surface also occurs because of sublimation
          !--      associated with the latent flux. If qla_ice_1d is negative, snow condensates at 
          !        the surface. Otherwise, snow evaporates
          !-----------------------------------------------------------------------
          !----change in snow and ice thicknesses due to sublimation or evaporation
          zdhssub  = parsub * ( qla_ice_1d(ji) / ( rhosn * xsn ) ) * rdt_ice 
          zhsn     = h_snow_1d(ji) - zdhssub
          zdhisub  = MAX( zzero , -zhsn ) * rhosn/rhoic
          zdhictop(ji) =  zdhictop(ji) - zdhisub
          h_snow_1d(ji)  =  MAX( zzero , zhsn )
          !-------------------------------------------------
          !--  Update Internal temperature and qstbif_1d.
          !-------------------------------------------
          zihsn  =  MAX( zzero , SIGN( zone, -h_snow_1d(ji) ) )
          tbif_1d(ji,1) = ( 1.0 - zihsn ) * tbif_1d(ji,1) + zihsn   * tfu_1d(ji)
          !--change in snow internal temperature if snow has increased
          zihnf = MAX( zzero , SIGN( zone , h_snow_1d(ji) - zhsnw_old(ji) ) )
          zdhsn = 1.0 - zhsnw_old(ji) / MAX( h_snow_1d(ji) , epsi20 )
          zdtsn = zdhsn * ( sist_1d(ji) - tbif_1d(ji,1) )
          tbif_1d(ji,1) = tbif_1d(ji,1) + z1midsn(ji) * zihnf * zdtsn
          !--energy created due to ice melting in the first ice layer
          zqnes  = ( rt0_ice - tbif_1d(ji,2) ) * rcpic * ( h_ice_1d(ji) / 2. )
          !--change in first ice layer internal temperature
          ziqr  = MAX( zzero , SIGN( zone , qstbif_1d(ji) - zqnes ) )
          zdtic = qstbif_1d(ji) / ( rcpic * ( h_ice_1d(ji) / 2. ) )
          tbif_1d(ji,2) =  ziqr * rt0_ice + ( 1 - ziqr ) * ( tbif_1d(ji,2) + zdtic )
          !--update qstbif_1d
          qstbif_1d(ji) = ziqr * ( qstbif_1d(ji) - zqnes ) * swiqst


          !--      9.2. Calculate bottom accretion-ablation and update qstbif_1d.
          !             Growth and melting at bottom ice surface are governed by  
          !                 -xlic * Dh = (Fcb - Fbot ) * Dt 
          !             where Fbot is the net downward heat flux from ice to the ocean
          !            and Fcb is the conductive heat flux at the bottom surface
          !---------------------------------------------------------------------------
          ztbot = ( 1.0 - zidsnic(ji) ) * tbif_1d(ji,3) + zidsnic(ji) * sist_1d(ji)
          !---computes conductive heat flux at bottom surface 
          zfcbot =  4.0 * zkic(ji) * ( tfu_1d(ji) - ztbot )   &
             &   / ( h_ice_1d(ji) + zidsnic(ji) * ( 3. * h_ice_1d(ji) &
             &   + 4.0 * zkic(ji)/zksn(ji) * h_snow_1d(ji) ) )
          !---computation of net energy needed for bottom melting/growing
          zqice_bot = ( zfcbot - ( fbif_1d(ji) + qlbbq_1d(ji) ) ) * rdt_ice
          zqstbif_bot = qstbif_1d(ji)
          !---switch to know if bottom surface melts ( = 1 ) or grows ( = 0 )occurs
          zibmlt = MAX( zzero , SIGN( zone , -zqice_bot ) )
          !--particular case of melting (in the same way as the top surface)
          zqice_bot_mlt = zqice_bot 
          zqstbif_old = zqstbif_bot

          ziqf =  MAX ( zzero , SIGN( zone , qstbif_1d(ji) + zqice_bot_mlt  ) )
          zqice_bot_mlt =         ziqf   * ( zqice_bot_mlt + zqice_bot_mlt ) &
             &          + ( 1.0 - ziqf ) * ( zqice_bot_mlt + qstbif_1d(ji)  )          
          qstbif_1d(ji)   =         ziqf   * ( qstbif_1d(ji) + zqice_bot_mlt ) &
             &          + ( 1.0 - ziqf ) * ( qstbif_1d(ji) - qstbif_1d(ji)  )
          !--    The contribution of the energy stored in brine pockets qstbif_1d to melt
          !--    ice is taking into account only when qstbif_1d is less than zqmax. 
          zihq =  MAX ( zzero , SIGN( zone , qstbif_1d(ji) - zqmax(ji) ) )
          zqice_bot_mlt =         zihq   * zqice_bot_mlt   &
             &          + ( 1.0 - zihq ) * zqice_bot
          qstbif_1d(ji)   =         zihq   * qstbif_1d(ji)   &
             &             + ( 1.0 - zihq ) * zqstbif_old

          !---treatment of the case of melting/growing
          zqice_bot   =         zibmlt   * ( zqice_bot_mlt - zqcmltb(ji) )   &
             &        + ( 1.0 - zibmlt ) * ( zqice_bot - zqcmltb(ji)  )
          qstbif_1d(ji) =         zibmlt   * qstbif_1d(ji)   &
             &           + ( 1.0 - zibmlt ) * zqstbif_bot

          !--computes change in ice thickness due to melt or growth
          zdhicbot(ji) = zqice_bot / xlic
          !--limitation of bottom melting if so : hmelt maximum melting at bottom
          zdhicmlt  = MAX( hmelt , zdhicbot(ji) ) 
          !-- output part due to bottom melting only
          IF( zdhicmlt < 0.e0 ) rdvomif_1d(ji) = ( 1.0 - frld_1d(ji) ) * zdhicmlt
          !--energy after bottom melting/growing
          zqsup(ji) = ( 1.0 - frld_1d(ji) ) * xlic * ( zdhicmlt - zdhicbot(ji) )
          !-- compute the new thickness and the newly formed volume after bottom melting/growing
          zdhicbot(ji)  = zdhicmlt
          dvbbq_1d(ji) = ( 1.0 - frld_1d(ji) ) * zdhicbot(ji)


          !        9.3.  Updating ice thickness after top surface ablation 
          !              and bottom surface accretion/ablation
          !---------------------------------------------------------------
          zhicnew  = h_ice_1d(ji) + zdhictop(ji) + zdhicbot(ji)

          !
          !        9.4. Case of total ablation (ice is gone but snow may be left)
          !-------------------------------------------------------------------
          zhsn  = h_snow_1d(ji)
          zihgnew = 1.0 - MAX( zzero , SIGN( zone , -zhicnew ) )
          zihsn   = MAX( zzero , SIGN( zone , -zhsn ) )
          !---convert 
          zdhicm  = ( 1.0 - zihgnew ) * ( zhicnew - qstbif_1d(ji) / xlic )
          zdhsnm  = ( 1.0 - zihsn ) * zdhicm * rhoic / rhosn
          !---updating new ice thickness and computing the newly formed ice mass
          zhicnew   =  zihgnew * zhicnew
          rdmicif_1d(ji) =  rdmicif_1d(ji) + ( 1.0 - frld_1d(ji) ) * ( zhicnew - h_ice_1d(ji) ) * rhoic
          !---updating new snow thickness and computing the newly formed snow mass
          zhsnfi   = zhsn + zdhsnm
          h_snow_1d(ji) = MAX( zzero , zhsnfi )
          rdmsnif_1d(ji) =  rdmsnif_1d(ji) + ( 1.0 - frld_1d(ji) ) * ( h_snow_1d(ji) - zhsn ) * rhosn
          !--remaining energy in case of total ablation
          zqocea(ji) = - ( zihsn * xlic * zdhicm + xlsn * ( zhsnfi - h_snow_1d(ji) ) ) * ( 1.0 - frld_1d(ji) )
          qstbif_1d(ji) = zihgnew * qstbif_1d(ji)

          !
          !        9.5. Update internal temperature and ice thickness.
          !-------------------------------------------------------
          !
          sist_1d(ji) = zihgnew * sist_1d(ji) + ( 1.0 - zihgnew ) * tfu_1d(ji)
          zidhb  = MAX( zzero , SIGN( zone , - zdhicbot(ji) ) )
          zc1    = - zhicnew * 0.5
          zpc1   = MIN( 0.5 * zone , - h_ice_1d(ji) * 0.5 - zdhictop(ji) )
          zc2    = - zhicnew
          zpc2   =  zidhb * zc2 + ( 1.0 - zidhb ) * ( - h_ice_1d(ji) - zdhictop(ji) )
          zp1    =  MAX( zpc1 , zc1 )
          zp2    =  MAX( zpc2 , zc1 )
          zep(ji) =  tbif_1d(ji,2)
          ztb2  = 2.0 * (         - zp1   * tbif_1d(ji,2)  &
             &  + ( zp1 - zp2 ) * tbif_1d(ji,3)  &
             &  + ( zp2 - zc1 ) * tfu_1d(ji) ) / MAX( zhicnew , epsi20 ) 
          tbif_1d(ji,2) = zihgnew * ztb2 + ( 1.0 - zihgnew ) * tfu_1d(ji)
          !---
          zp1  =  MIN( zpc1 , zc1 )
          zp2  =  MIN( zpc2 , zc1 )
          zp1  =  MAX( zc2  , zp1 )
          ztb3 =  2.0 * (   ( 1.0 - zidhb ) * (  ( zc1 - zp2 ) * tbif_1d(ji,3)  &
             &                                 + ( zp2 - zc2 ) * tfu_1d(ji) )   &
             &               +      zidhb   * (  ( zc1 - zp1 ) * zep(ji)      &
             &                                 + ( zp1 - zc2 ) * tbif_1d(ji,3))  ) / MAX( zhicnew , epsi20 )
          tbif_1d(ji,3) =  zihgnew * ztb3 + ( 1.0 - zihgnew ) * tfu_1d(ji)
          h_ice_1d(ji)  =  zhicnew
       END DO


       !----------------------------------------------------------------------------
       !  10. Surface accretion. 
       !      The change of ice thickness after snow/ice formation is such that
       !      the interface between snow and ice is located at the same height
       !      as the ocean surface. It is given by (Fichefet and Morales Maqueda 1999)
       !          D(h_ice) = (- D(hsn)/alph) =  [rhosn*hsn - (rau0 - rhoic)*hic]
       !                                     / [alph*rhosn+rau0 - rhoic]
       !----------------------------------------------------------------------------
       !
       DO ji = kideb , kiut

          !--  Computation of the change of ice thickness after snow-ice formation
          zdrmh =  ( rhosn * h_snow_1d(ji) + ( rhoic - rau0 ) * h_ice_1d(ji) )  &
             &  / ( alphs * rhosn + rau0 - rhoic )
          zdrmh = MAX( zzero , zdrmh )

          !--New ice and snow thicknesses Fichefet and Morales Maqueda (1999)
          zhicnew  = MAX( h_ice_1d(ji) , h_ice_1d(ji) + zdrmh )
          zhsnnew  = MIN( h_snow_1d(ji) , h_snow_1d(ji) - alphs * zdrmh )
          !---Compute new ice temperatures. snow temperature remains unchanged
          !   Lepparanta (1983):
          zihic = 1.0 - MAX( zzero , SIGN( zone , -zhicnew ) )
          zquot  = ( 1.0 - zihic ) &
             &   +         zihic * MIN( zone , h_ice_1d(ji) / MAX( zhicnew , epsi20 ) ) 
          ztneq  =         alphs * cnscg * tbif_1d(ji,1)    &
             &   + ( 1.0 - alphs * ( rhosn/rhoic ) ) * tfu_1d(ji)
          zep(ji) = tbif_1d(ji,2)
          tbif_1d(ji,2) = ztneq - zquot * zquot * ( ztneq - tbif_1d(ji,2) )
          tbif_1d(ji,3) = 2.0 * ztneq &
             &        + zquot * ( tbif_1d(ji,3) + zep(ji) - 2.0 * ztneq ) - tbif_1d(ji,2)

          !---  Lepparanta (1983) (latent heat released during white ice formation
          !     goes to the ocean -for lateral ablation-)
          qldif_1d(ji)  = qldif_1d(ji) + zdrmh * ( 1.0 - alphs * ( rhosn/rhoic ) ) * xlic * ( 1.0 - frld_1d(ji) )
          !--   Changes in ice volume and ice mass Lepparanta (1983):
          dvnbq_1d(ji) = ( 1.0 - frld_1d(ji) ) * ( zhicnew - h_ice_1d(ji) )
          dmgwi_1d(ji) = dmgwi_1d(ji) + ( 1.0 -frld_1d(ji) ) * ( h_snow_1d(ji) - zhsnnew ) * rhosn
          !---  volume change of ice and snow (used for ocean-ice freshwater flux computation)
          rdmicif_1d(ji) = rdmicif_1d(ji) + ( 1.0 - frld_1d(ji) )   * ( zhicnew - h_ice_1d (ji) ) * rhoic
          rdmsnif_1d(ji) = rdmsnif_1d(ji) + ( 1.0 - frld_1d(ji) )   * ( zhsnnew - h_snow_1d(ji) ) * rhosn

          !---  Actualize new snow and ice thickness.
          h_snow_1d(ji)  = zhsnnew
          h_ice_1d (ji)  = zhicnew

       END DO

       !----------------------------------------------------
       !  11. Lateral ablation (Changes in sea/ice fraction) 
       !----------------------------------------------------
       DO ji = kideb , kiut
          zfrl_old(ji)  = frld_1d(ji)
          zihic   = 1.0 - MAX( zzero , SIGN( zone , -h_ice_1d(ji) ) )
          zihsn   = 1.0 - MAX( zzero , SIGN( zone , -h_snow_1d(ji) ) )
          !--In the case of total ablation (all the ice ice has melted) frld = 1
          frld_1d(ji)  = ( 1.0 - zihic ) + zihic * zfrl_old(ji)
          !--Part of solar radiation absorbing inside the ice and going
          !--through the ocean
          fscbq_1d(ji) = ( 1.0 - zfrl_old(ji) ) * ( 1.0 - thcm_1d(ji) ) * fstbif_1d(ji)
          !--Total remaining energy after bottom melting/growing 
          qfvbq_1d(ji) = zqsup(ji) + ( 1.0 - zihic ) * zqocea(ji)
          !--Updating of total heat from the ocean
          qldif_1d(ji)  = qldif_1d(ji) + qfvbq_1d(ji) + ( 1.0 - zihic ) * fscbq_1d(ji) * rdt_ice
          !--Computation of total heat inside the snow/ice system
          zqice  = h_snow_1d(ji) * xlsn + h_ice_1d(ji) * xlic
          zqicetot  = ( 1.0 - frld_1d(ji) ) * zqice
          !--The concentration of ice is reduced (frld increases) if the heat 
          !--exchange between ice and ocean is positive
          ziqf = MAX( zzero , SIGN( zone ,  zqicetot - qldif_1d(ji) ) )
          zdfrl = qldif_1d(ji) / MAX( epsi20 , zqice ) 
          frld_1d(ji)  = ( 1.0 - ziqf )    &
             &       +         ziqf * ( frld_1d(ji) + MAX( zzero , zdfrl ) ) 
          fltbif_1d(ji) = ( ( 1.0 - zfrl_old(ji) ) * qstbif_1d(ji) - zqicetot  ) / rdt_ice
          !--  Opening of leads: Hakkinen & Mellor, 1992.
          zdfrl = - ( zdhictop(ji) + zdhicbot(ji) ) * hakspl * ( 1.0 - zfrl_old(ji) ) &
             &  / MAX( epsi13 , h_ice_1d(ji) + h_snow_1d(ji) * rhosn/rhoic ) 
          zfrld_1d(ji) =  frld_1d(ji) + MAX( zzero , zdfrl )
          !--Limitation of sea-ice fraction <= 1
          zfrld_1d(ji) = ziqf * MIN( 0.99 * zone , zfrld_1d(ji) ) + ( 1 - ziqf )
          !---Update surface and internal temperature and snow/ice thicknesses.
          sist_1d(ji)   = sist_1d(ji)   + ( 1.0 - ziqf ) * ( tfu_1d(ji) - sist_1d(ji)   )
          tbif_1d(ji,1) = tbif_1d(ji,1) + ( 1.0 - ziqf ) * ( tfu_1d(ji) - tbif_1d(ji,1) )
          tbif_1d(ji,2) = tbif_1d(ji,2) + ( 1.0 - ziqf ) * ( tfu_1d(ji) - tbif_1d(ji,2) )
          tbif_1d(ji,3) = tbif_1d(ji,3) + ( 1.0 - ziqf ) * ( tfu_1d(ji) - tbif_1d(ji,3) )
          !--variation of ice volume and ice mass 
          dvlbq_1d(ji)   = zihic * ( zfrl_old(ji) - frld_1d(ji) ) * h_ice_1d(ji)
          rdmicif_1d(ji) = rdmicif_1d(ji) + dvlbq_1d(ji) * rhoic
          !--variation of snow volume and snow mass 
          zdvsnvol    = zihsn * ( zfrl_old(ji) - frld_1d(ji) ) * h_snow_1d(ji)
          rdmsnif_1d(ji) = rdmsnif_1d(ji) + zdvsnvol * rhosn
          h_snow_1d(ji)  = ziqf * h_snow_1d(ji)

          zdrfrl1 = ziqf * ( 1.0 -  frld_1d(ji) ) / MAX( epsi20 , 1.0 - zfrld_1d(ji) )
          zdrfrl2 = ziqf * ( 1.0 - zfrl_old(ji) ) / MAX( epsi20 , 1.0 - zfrld_1d(ji) )

          h_snow_1d (ji) = zdrfrl1 * h_snow_1d(ji)
          h_ice_1d  (ji) = zdrfrl1 * h_ice_1d(ji)
          qstbif_1d(ji) = zdrfrl2 * qstbif_1d(ji)
          frld_1d(ji)    = zfrld_1d(ji)
          !
       END DO
       ! 
       CALL wrk_dealloc( jpij, ztsmlt, ztbif  , zksn    , zkic    , zksndh , zfcsu  , zfcsudt , zi0      , z1mi0   , zqmax    )
       CALL wrk_dealloc( jpij, zrcpdt, zts_old, zidsn   , z1midsn , zidsnic, zfnet  , zsprecip, zhsnw_old, zdhictop, zdhicbot )
       CALL wrk_dealloc( jpij, zqsup , zqocea , zfrl_old, zfrld_1d, zep    , zqcmlts, zqcmltb                                 )
       !
    END SUBROUTINE lim_thd_zdf_2

#else
   !!----------------------------------------------------------------------
   !!   Default Option                                     NO sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_thd_zdf_2          ! Empty routine
   END SUBROUTINE lim_thd_zdf_2
#endif

   !!======================================================================
END MODULE limthd_zdf_2
