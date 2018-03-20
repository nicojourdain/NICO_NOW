MODULE sbccpl
   !!======================================================================
   !!                       ***  MODULE  sbccpl  ***
   !! Surface Boundary Condition :  momentum, heat and freshwater fluxes in coupled mode
   !!======================================================================
   !! History :  2.0  ! 2007-06  (R. Redler, N. Keenlyside, W. Park) Original code split into flxmod & taumod
   !!            3.0  ! 2008-02  (G. Madec, C Talandier)  surface module
   !!            3.1  ! 2009_02  (G. Madec, S. Masson, E. Maisonave, A. Caubel) generic coupled interface
   !!            3.4  ! 2011_11  (C. Harris) more flexibility + multi-category fields
   !!----------------------------------------------------------------------
#if defined key_oasis3 || defined key_oasis4
   !!----------------------------------------------------------------------
   !!   'key_oasis3' or 'key_oasis4'   Coupled Ocean/Atmosphere formulation
   !!----------------------------------------------------------------------
   !!   namsbc_cpl      : coupled formulation namlist
   !!   sbc_cpl_init    : initialisation of the coupled exchanges
   !!   sbc_cpl_rcv     : receive fields from the atmosphere over the ocean (ocean only)
   !!                     receive stress from the atmosphere over the ocean (ocean-ice case)
   !!   sbc_cpl_ice_tau : receive stress from the atmosphere over ice
   !!   sbc_cpl_ice_flx : receive fluxes from the atmosphere over ice
   !!   sbc_cpl_snd     : send     fields to the atmosphere
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE sbc_ice         ! Surface boundary condition: ice fields
   USE sbcdcy          ! surface boundary condition: diurnal cycle
   USE phycst          ! physical constants
#if defined key_lim3
   USE par_ice         ! ice parameters
   USE ice             ! ice variables
#endif
#if defined key_lim2
   USE par_ice_2       ! ice parameters
   USE ice_2           ! ice variables
#endif
#if defined key_oasis3
   USE cpl_oasis3      ! OASIS3 coupling
#endif
#if defined key_oasis4
   USE cpl_oasis4      ! OASIS4 coupling
#endif
   USE geo2ocean       ! 
   USE oce   , ONLY : tsn, un, vn
   USE albedo          !
   USE in_out_manager  ! I/O manager
   USE iom             ! NetCDF library
   USE lib_mpp         ! distribued memory computing library
   USE wrk_nemo        ! work arrays
   USE timing          ! Timing
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
#if defined key_cpl_carbon_cycle
   USE p4zflx, ONLY : oce_co2
#endif
   USE diaar5, ONLY :   lk_diaar5
#if defined key_cice
   USE ice_domain_size, only: ncat
#endif
   IMPLICIT NONE
   PRIVATE
!EM XIOS-OASIS-MCT compliance
   PUBLIC   sbc_cpl_init       ! routine called by sbcmod.F90
   PUBLIC   sbc_cpl_rcv        ! routine called by sbc_ice_lim(_2).F90
   PUBLIC   sbc_cpl_snd        ! routine called by step.F90
   PUBLIC   sbc_cpl_ice_tau    ! routine called by sbc_ice_lim(_2).F90
   PUBLIC   sbc_cpl_ice_flx    ! routine called by sbc_ice_lim(_2).F90

   INTEGER, PARAMETER ::   jpr_otx1   =  1            ! 3 atmosphere-ocean stress components on grid 1
   INTEGER, PARAMETER ::   jpr_oty1   =  2            ! 
   INTEGER, PARAMETER ::   jpr_otz1   =  3            ! 
   INTEGER, PARAMETER ::   jpr_otx2   =  4            ! 3 atmosphere-ocean stress components on grid 2
   INTEGER, PARAMETER ::   jpr_oty2   =  5            ! 
   INTEGER, PARAMETER ::   jpr_otz2   =  6            ! 
   INTEGER, PARAMETER ::   jpr_itx1   =  7            ! 3 atmosphere-ice   stress components on grid 1
   INTEGER, PARAMETER ::   jpr_ity1   =  8            ! 
   INTEGER, PARAMETER ::   jpr_itz1   =  9            ! 
   INTEGER, PARAMETER ::   jpr_itx2   = 10            ! 3 atmosphere-ice   stress components on grid 2
   INTEGER, PARAMETER ::   jpr_ity2   = 11            ! 
   INTEGER, PARAMETER ::   jpr_itz2   = 12            ! 
   INTEGER, PARAMETER ::   jpr_qsroce = 13            ! Qsr above the ocean
   INTEGER, PARAMETER ::   jpr_qsrice = 14            ! Qsr above the ice
   INTEGER, PARAMETER ::   jpr_qsrmix = 15 
   INTEGER, PARAMETER ::   jpr_qnsoce = 16            ! Qns above the ocean
   INTEGER, PARAMETER ::   jpr_qnsice = 17            ! Qns above the ice
   INTEGER, PARAMETER ::   jpr_qnsmix = 18
   INTEGER, PARAMETER ::   jpr_rain   = 19            ! total liquid precipitation (rain)
   INTEGER, PARAMETER ::   jpr_snow   = 20            ! solid precipitation over the ocean (snow)
   INTEGER, PARAMETER ::   jpr_tevp   = 21            ! total evaporation
   INTEGER, PARAMETER ::   jpr_ievp   = 22            ! solid evaporation (sublimation)
   INTEGER, PARAMETER ::   jpr_sbpr   = 23            ! sublimation - liquid precipitation - solid precipitation
   INTEGER, PARAMETER ::   jpr_semp   = 24            ! solid freshwater budget (sublimation - snow)
   INTEGER, PARAMETER ::   jpr_oemp   = 25            ! ocean freshwater budget (evap - precip)
   INTEGER, PARAMETER ::   jpr_w10m   = 26            ! 10m wind
   INTEGER, PARAMETER ::   jpr_dqnsdt = 27            ! d(Q non solar)/d(temperature)
   INTEGER, PARAMETER ::   jpr_rnf    = 28            ! runoffs
   INTEGER, PARAMETER ::   jpr_cal    = 29            ! calving
   INTEGER, PARAMETER ::   jpr_taum   = 30            ! wind stress module
   INTEGER, PARAMETER ::   jpr_co2    = 31
   INTEGER, PARAMETER ::   jpr_topm   = 32            ! topmeltn
   INTEGER, PARAMETER ::   jpr_botm   = 33            ! botmeltn
   INTEGER, PARAMETER ::   jprcv      = 33            ! total number of fields received

   INTEGER, PARAMETER ::   jps_fice   =  1            ! ice fraction 
   INTEGER, PARAMETER ::   jps_toce   =  2            ! ocean temperature
   INTEGER, PARAMETER ::   jps_tice   =  3            ! ice   temperature
   INTEGER, PARAMETER ::   jps_tmix   =  4            ! mixed temperature (ocean+ice)
   INTEGER, PARAMETER ::   jps_albice =  5            ! ice   albedo
   INTEGER, PARAMETER ::   jps_albmix =  6            ! mixed albedo
   INTEGER, PARAMETER ::   jps_hice   =  7            ! ice  thickness
   INTEGER, PARAMETER ::   jps_hsnw   =  8            ! snow thickness
   INTEGER, PARAMETER ::   jps_ocx1   =  9            ! ocean current on grid 1
   INTEGER, PARAMETER ::   jps_ocy1   = 10            !
   INTEGER, PARAMETER ::   jps_ocz1   = 11            !
   INTEGER, PARAMETER ::   jps_ivx1   = 12            ! ice   current on grid 1
   INTEGER, PARAMETER ::   jps_ivy1   = 13            !
   INTEGER, PARAMETER ::   jps_ivz1   = 14            !
   INTEGER, PARAMETER ::   jps_co2    = 15
   INTEGER, PARAMETER ::   jpsnd      = 15            ! total number of fields sended

   !                                                         !!** namelist namsbc_cpl **
   TYPE ::   FLD_C
      CHARACTER(len = 32) ::   cldes                  ! desciption of the coupling strategy
      CHARACTER(len = 32) ::   clcat                  ! multiple ice categories strategy
      CHARACTER(len = 32) ::   clvref                 ! reference of vector ('spherical' or 'cartesian')
      CHARACTER(len = 32) ::   clvor                  ! orientation of vector fields ('eastward-northward' or 'local grid')
      CHARACTER(len = 32) ::   clvgrd                 ! grids on which is located the vector fields
   END TYPE FLD_C
   ! Send to the atmosphere                           !
   TYPE(FLD_C) ::   sn_snd_temp, sn_snd_alb, sn_snd_thick, sn_snd_crt, sn_snd_co2                        
   ! Received from the atmosphere                     !
   TYPE(FLD_C) ::   sn_rcv_w10m, sn_rcv_taumod, sn_rcv_tau, sn_rcv_dqnsdt, sn_rcv_qsr, sn_rcv_qns, sn_rcv_emp, sn_rcv_rnf
   TYPE(FLD_C) ::   sn_rcv_cal, sn_rcv_iceflx, sn_rcv_co2                        
   INTEGER     ::   nn_nbgrdatm = 1
   LOGICAL     ::   ln_usecplmask = .FALSE. 
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: xcplmask

   TYPE ::   DYNARR     
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   z3   
   END TYPE DYNARR

   TYPE( DYNARR ), SAVE, DIMENSION(jprcv) ::   frcv                      ! all fields recieved from the atmosphere

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   albedo_oce_mix     ! ocean albedo sent to atmosphere (mix clear/overcast sky)

   INTEGER , ALLOCATABLE, SAVE, DIMENSION(    :) ::   nrcvinfo           ! OASIS info argument

#if ! defined key_lim2   &&   ! defined key_lim3
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   u_ice, v_ice,fr1_i0,fr2_i0          ! jpi, jpj
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   tn_ice, alb_ice, qns_ice, dqns_ice  ! (jpi,jpj,jpl)
#endif

#if defined key_cice
   INTEGER, PARAMETER ::   jpl = ncat
#elif ! defined key_lim2   &&   ! defined key_lim3
   INTEGER, PARAMETER ::   jpl = 1 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   emp_ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qsr_ice
#endif

#if ! defined key_lim3   &&  ! defined key_cice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  a_i
#endif

#if ! defined key_lim3
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  ht_i, ht_s
#endif

#if ! defined key_cice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  topmelt, botmelt
#endif

   !! Substitution
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: sbccpl.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS
  
   INTEGER FUNCTION sbc_cpl_alloc()
      !!----------------------------------------------------------------------
      !!             ***  FUNCTION sbc_cpl_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr(5),jn
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !
      ALLOCATE( albedo_oce_mix(jpi,jpj), nrcvinfo(jprcv),  STAT=ierr(1) )
      !
#if ! defined key_lim2 && ! defined key_lim3
      ! quick patch to be able to run the coupled model without sea-ice...
      ALLOCATE( u_ice(jpi,jpj) , fr1_i0(jpi,jpj) , tn_ice (jpi,jpj,1) ,     &
                v_ice(jpi,jpj) , fr2_i0(jpi,jpj) , alb_ice(jpi,jpj,1),      &
                emp_ice(jpi,jpj) , qns_ice(jpi,jpj,1) , dqns_ice(jpi,jpj,1) , STAT=ierr(2) )
#endif

#if ! defined key_lim3 && ! defined key_cice
      ALLOCATE( a_i(jpi,jpj,jpl) , STAT=ierr(3) )
#endif

#if defined key_cice || defined key_lim2
      ALLOCATE( ht_i(jpi,jpj,jpl) , ht_s(jpi,jpj,jpl) , STAT=ierr(4) )
#endif
      ALLOCATE( xcplmask(jpi,jpj,nn_nbgrdatm) , STAT=ierr(5) )

      sbc_cpl_alloc = MAXVAL( ierr )
      IF( lk_mpp            )   CALL mpp_sum ( sbc_cpl_alloc )
      IF( sbc_cpl_alloc > 0 )   CALL ctl_warn('sbc_cpl_alloc: allocation of arrays failed')
      !
   END FUNCTION sbc_cpl_alloc


   SUBROUTINE sbc_cpl_init( k_ice )     
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE sbc_cpl_init  ***
      !!
      !! ** Purpose :   Initialisation of send and recieved information from
      !!                the atmospheric component
      !!
      !! ** Method  : * Read namsbc_cpl namelist 
      !!              * define the receive interface
      !!              * define the send    interface
      !!              * initialise the OASIS coupler
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   k_ice    ! ice management in the sbc (=0/1/2/3)
      !!
      INTEGER ::   jn   ! dummy loop index
      INTEGER ::   ierror 
      INTEGER ::   inum 
      REAL(wp), POINTER, DIMENSION(:,:) ::   zacs, zaos
      !!
      NAMELIST/namsbc_cpl/  sn_snd_temp, sn_snd_alb   , sn_snd_thick, sn_snd_crt   , sn_snd_co2,   &
         &                  sn_rcv_w10m, sn_rcv_taumod, sn_rcv_tau  , sn_rcv_dqnsdt, sn_rcv_qsr,   &
         &                  sn_rcv_qns , sn_rcv_emp   , sn_rcv_rnf  , sn_rcv_cal   , sn_rcv_iceflx  , sn_rcv_co2, &
         &                  nn_nbgrdatm, ln_usecplmask
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('sbc_cpl_init')
      !
      CALL wrk_alloc( jpi,jpj, zacs, zaos )

      ALLOCATE(srcv(nmaxfld), stat = ierror)
      IF( ierror > 0 ) CALL ctl_stop('Pb of alloction of srcv(nmaxfld) in sbc_cpl_init')
      ALLOCATE(ssnd(nmaxfld), stat = ierror)
      IF( ierror > 0 ) CALL ctl_stop('Pb of alloction of ssnd(nmaxfld) in sbc_cpl_init')

      ! ================================ !
      !      Namelist informations       !
      ! ================================ !

      ! default definitions
      !                    !     description       !  multiple  !    vector   !      vector          ! vector !
      !                    !                       ! categories !  reference  !    orientation       ! grids  !
      ! send
      sn_snd_temp   = FLD_C( 'weighted oce and ice',    'no'    ,     ''      ,         ''           ,   ''   ) 
      sn_snd_alb    = FLD_C( 'weighted ice'        ,    'no'    ,     ''      ,         ''           ,   ''   ) 
      sn_snd_thick  = FLD_C( 'none'                ,    'no'    ,     ''      ,         ''           ,   ''   ) 
      sn_snd_crt    = FLD_C( 'none'                ,    'no'    , 'spherical' , 'eastward-northward' ,  'T'   )     
      sn_snd_co2    = FLD_C( 'none'                ,    'no'    ,     ''      ,         ''           ,   ''   )     
      ! receive
      sn_rcv_w10m   = FLD_C( 'none'                ,    'no'    ,     ''      ,         ''          ,   ''    )
      sn_rcv_taumod = FLD_C( 'coupled'             ,    'no'    ,     ''      ,         ''          ,   ''    )
      sn_rcv_tau    = FLD_C( 'oce only'            ,    'no'    , 'cartesian' , 'eastward-northward',  'U,V'  )  
      sn_rcv_dqnsdt = FLD_C( 'coupled'             ,    'no'    ,     ''      ,         ''          ,   ''    )
      sn_rcv_qsr    = FLD_C( 'oce and ice'         ,    'no'    ,     ''      ,         ''          ,   ''    )
      sn_rcv_qns    = FLD_C( 'oce and ice'         ,    'no'    ,     ''      ,         ''          ,   ''    )
      sn_rcv_emp    = FLD_C( 'conservative'        ,    'no'    ,     ''      ,         ''          ,   ''    )
      sn_rcv_rnf    = FLD_C( 'coupled'             ,    'no'    ,     ''      ,         ''          ,   ''    )
      sn_rcv_cal    = FLD_C( 'coupled'             ,    'no'    ,     ''      ,         ''          ,   ''    )
      sn_rcv_iceflx = FLD_C( 'none'                ,    'no'    ,     ''      ,         ''          ,   ''    )
      sn_rcv_co2    = FLD_C( 'none'                ,    'no'    ,     ''      ,         ''          ,   ''    )

      REWIND( numnam )                    ! ... read namlist namsbc_cpl
      READ  ( numnam, namsbc_cpl )

      IF(lwp) THEN                        ! control print
         WRITE(numout,*)
         WRITE(numout,*)'sbc_cpl_init : namsbc_cpl namelist '
         WRITE(numout,*)'~~~~~~~~~~~~'
         WRITE(numout,*)'  received fields (mutiple ice categogies)'
         WRITE(numout,*)'      10m wind module                 = ', TRIM(sn_rcv_w10m%cldes  ), ' (', TRIM(sn_rcv_w10m%clcat  ), ')'
         WRITE(numout,*)'      stress module                   = ', TRIM(sn_rcv_taumod%cldes), ' (', TRIM(sn_rcv_taumod%clcat), ')'
         WRITE(numout,*)'      surface stress                  = ', TRIM(sn_rcv_tau%cldes   ), ' (', TRIM(sn_rcv_tau%clcat   ), ')'
         WRITE(numout,*)'                     - referential    = ', sn_rcv_tau%clvref
         WRITE(numout,*)'                     - orientation    = ', sn_rcv_tau%clvor
         WRITE(numout,*)'                     - mesh           = ', sn_rcv_tau%clvgrd
         WRITE(numout,*)'      non-solar heat flux sensitivity = ', TRIM(sn_rcv_dqnsdt%cldes), ' (', TRIM(sn_rcv_dqnsdt%clcat), ')'
         WRITE(numout,*)'      solar heat flux                 = ', TRIM(sn_rcv_qsr%cldes   ), ' (', TRIM(sn_rcv_qsr%clcat   ), ')'
         WRITE(numout,*)'      non-solar heat flux             = ', TRIM(sn_rcv_qns%cldes   ), ' (', TRIM(sn_rcv_qns%clcat   ), ')'
         WRITE(numout,*)'      freshwater budget               = ', TRIM(sn_rcv_emp%cldes   ), ' (', TRIM(sn_rcv_emp%clcat   ), ')'
         WRITE(numout,*)'      runoffs                         = ', TRIM(sn_rcv_rnf%cldes   ), ' (', TRIM(sn_rcv_rnf%clcat   ), ')'
         WRITE(numout,*)'      calving                         = ', TRIM(sn_rcv_cal%cldes   ), ' (', TRIM(sn_rcv_cal%clcat   ), ')'
         WRITE(numout,*)'      sea ice heat fluxes             = ', TRIM(sn_rcv_iceflx%cldes), ' (', TRIM(sn_rcv_iceflx%clcat), ')'
         WRITE(numout,*)'      atm co2                         = ', TRIM(sn_rcv_co2%cldes   ), ' (', TRIM(sn_rcv_co2%clcat   ), ')'
         WRITE(numout,*)'  sent fields (multiple ice categories)'
         WRITE(numout,*)'      surface temperature             = ', TRIM(sn_snd_temp%cldes  ), ' (', TRIM(sn_snd_temp%clcat  ), ')'
         WRITE(numout,*)'      albedo                          = ', TRIM(sn_snd_alb%cldes   ), ' (', TRIM(sn_snd_alb%clcat   ), ')'
         WRITE(numout,*)'      ice/snow thickness              = ', TRIM(sn_snd_thick%cldes ), ' (', TRIM(sn_snd_thick%clcat ), ')'
         WRITE(numout,*)'      surface current                 = ', TRIM(sn_snd_crt%cldes   ), ' (', TRIM(sn_snd_crt%clcat   ), ')'
         WRITE(numout,*)'                      - referential   = ', sn_snd_crt%clvref 
         WRITE(numout,*)'                      - orientation   = ', sn_snd_crt%clvor
         WRITE(numout,*)'                      - mesh          = ', sn_snd_crt%clvgrd
         WRITE(numout,*)'      oce co2 flux                    = ', TRIM(sn_snd_co2%cldes   ), ' (', TRIM(sn_snd_co2%clcat   ), ')'
         WRITE(numout,*)'  nn_nbgrdatm                         = ', nn_nbgrdatm
         WRITE(numout,*)'  ln_usecplmask                       = ', ln_usecplmask
      ENDIF

      !                                   ! allocate sbccpl arrays
      IF( sbc_cpl_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'sbc_cpl_alloc : unable to allocate arrays' )
     
      ! ================================ !
      !   Define the receive interface   !
      ! ================================ !
      nrcvinfo(:) = OASIS_idle   ! needed by nrcvinfo(jpr_otx1) if we do not receive ocean stress 

      ! for each field: define the OASIS name                              (srcv(:)%clname)
      !                 define receive or not from the namelist parameters (srcv(:)%laction)
      !                 define the north fold type of lbc                  (srcv(:)%nsgn)

      ! default definitions of srcv
      srcv(:)%laction = .FALSE.   ;   srcv(:)%clgrid = 'T'   ;   srcv(:)%nsgn = 1.   ;   srcv(:)%nct = 1

      !                                                      ! ------------------------- !
      !                                                      ! ice and ocean wind stress !   
      !                                                      ! ------------------------- !
      !                                                           ! Name 
      srcv(jpr_otx1)%clname = 'O_OTaux1'      ! 1st ocean component on grid ONE (T or U)
      srcv(jpr_oty1)%clname = 'O_OTauy1'      ! 2nd   -      -         -     - 
      srcv(jpr_otz1)%clname = 'O_OTauz1'      ! 3rd   -      -         -     - 
      srcv(jpr_otx2)%clname = 'O_OTaux2'      ! 1st ocean component on grid TWO (V)
      srcv(jpr_oty2)%clname = 'O_OTauy2'      ! 2nd   -      -         -     - 
      srcv(jpr_otz2)%clname = 'O_OTauz2'      ! 3rd   -      -         -     - 
      !
      srcv(jpr_itx1)%clname = 'O_ITaux1'      ! 1st  ice  component on grid ONE (T, F, I or U)
      srcv(jpr_ity1)%clname = 'O_ITauy1'      ! 2nd   -      -         -     - 
      srcv(jpr_itz1)%clname = 'O_ITauz1'      ! 3rd   -      -         -     - 
      srcv(jpr_itx2)%clname = 'O_ITaux2'      ! 1st  ice  component on grid TWO (V)
      srcv(jpr_ity2)%clname = 'O_ITauy2'      ! 2nd   -      -         -     - 
      srcv(jpr_itz2)%clname = 'O_ITauz2'      ! 3rd   -      -         -     - 
      ! 
      ! Vectors: change of sign at north fold ONLY if on the local grid
      IF( TRIM( sn_rcv_tau%clvor ) == 'local grid' )   srcv(jpr_otx1:jpr_itz2)%nsgn = -1.
      
      !                                                           ! Set grid and action
      SELECT CASE( TRIM( sn_rcv_tau%clvgrd ) )      !  'T', 'U,V', 'U,V,I', 'U,V,F', 'T,I', 'T,F', or 'T,U,V'
      CASE( 'T' ) 
         srcv(jpr_otx1:jpr_itz2)%clgrid  = 'T'        ! oce and ice components given at T-point
         srcv(jpr_otx1:jpr_otz1)%laction = .TRUE.     ! receive oce components on grid 1 
         srcv(jpr_itx1:jpr_itz1)%laction = .TRUE.     ! receive ice components on grid 1 
      CASE( 'U,V' ) 
         srcv(jpr_otx1:jpr_otz1)%clgrid  = 'U'        ! oce components given at U-point
         srcv(jpr_otx2:jpr_otz2)%clgrid  = 'V'        !           and           V-point
         srcv(jpr_itx1:jpr_itz1)%clgrid  = 'U'        ! ice components given at U-point
         srcv(jpr_itx2:jpr_itz2)%clgrid  = 'V'        !           and           V-point
         srcv(jpr_otx1:jpr_itz2)%laction = .TRUE.     ! receive oce and ice components on both grid 1 & 2
      CASE( 'U,V,T' )
         srcv(jpr_otx1:jpr_otz1)%clgrid  = 'U'        ! oce components given at U-point
         srcv(jpr_otx2:jpr_otz2)%clgrid  = 'V'        !           and           V-point
         srcv(jpr_itx1:jpr_itz1)%clgrid  = 'T'        ! ice components given at T-point
         srcv(jpr_otx1:jpr_otz2)%laction = .TRUE.     ! receive oce components on grid 1 & 2
         srcv(jpr_itx1:jpr_itz1)%laction = .TRUE.     ! receive ice components on grid 1 only
      CASE( 'U,V,I' )
         srcv(jpr_otx1:jpr_otz1)%clgrid  = 'U'        ! oce components given at U-point
         srcv(jpr_otx2:jpr_otz2)%clgrid  = 'V'        !           and           V-point
         srcv(jpr_itx1:jpr_itz1)%clgrid  = 'I'        ! ice components given at I-point
         srcv(jpr_otx1:jpr_otz2)%laction = .TRUE.     ! receive oce components on grid 1 & 2
         srcv(jpr_itx1:jpr_itz1)%laction = .TRUE.     ! receive ice components on grid 1 only
      CASE( 'U,V,F' )
         srcv(jpr_otx1:jpr_otz1)%clgrid  = 'U'        ! oce components given at U-point
         srcv(jpr_otx2:jpr_otz2)%clgrid  = 'V'        !           and           V-point
         srcv(jpr_itx1:jpr_itz1)%clgrid  = 'F'        ! ice components given at F-point
         srcv(jpr_otx1:jpr_otz2)%laction = .TRUE.     ! receive oce components on grid 1 & 2
         srcv(jpr_itx1:jpr_itz1)%laction = .TRUE.     ! receive ice components on grid 1 only
      CASE( 'T,I' ) 
         srcv(jpr_otx1:jpr_itz2)%clgrid  = 'T'        ! oce and ice components given at T-point
         srcv(jpr_itx1:jpr_itz1)%clgrid  = 'I'        ! ice components given at I-point
         srcv(jpr_otx1:jpr_otz1)%laction = .TRUE.     ! receive oce components on grid 1 
         srcv(jpr_itx1:jpr_itz1)%laction = .TRUE.     ! receive ice components on grid 1 
      CASE( 'T,F' ) 
         srcv(jpr_otx1:jpr_itz2)%clgrid  = 'T'        ! oce and ice components given at T-point
         srcv(jpr_itx1:jpr_itz1)%clgrid  = 'F'        ! ice components given at F-point
         srcv(jpr_otx1:jpr_otz1)%laction = .TRUE.     ! receive oce components on grid 1 
         srcv(jpr_itx1:jpr_itz1)%laction = .TRUE.     ! receive ice components on grid 1 
      CASE( 'T,U,V' )
         srcv(jpr_otx1:jpr_otz1)%clgrid  = 'T'        ! oce components given at T-point
         srcv(jpr_itx1:jpr_itz1)%clgrid  = 'U'        ! ice components given at U-point
         srcv(jpr_itx2:jpr_itz2)%clgrid  = 'V'        !           and           V-point
         srcv(jpr_otx1:jpr_otz1)%laction = .TRUE.     ! receive oce components on grid 1 only
         srcv(jpr_itx1:jpr_itz2)%laction = .TRUE.     ! receive ice components on grid 1 & 2
      CASE default   
         CALL ctl_stop( 'sbc_cpl_init: wrong definition of sn_rcv_tau%clvgrd' )
      END SELECT
      !
      IF( TRIM( sn_rcv_tau%clvref ) == 'spherical'  )   &         ! spherical: 3rd component not received
         &     srcv( (/jpr_otz1, jpr_otz2, jpr_itz1, jpr_itz2/) )%laction = .FALSE. 
      !
      IF( TRIM( sn_rcv_tau%clvor  ) == 'local grid' ) THEN        ! already on local grid -> no need of the second grid
            srcv(jpr_otx2:jpr_otz2)%laction = .FALSE. 
            srcv(jpr_itx2:jpr_itz2)%laction = .FALSE. 
            srcv(jpr_oty1)%clgrid = srcv(jpr_oty2)%clgrid   ! not needed but cleaner...
            srcv(jpr_ity1)%clgrid = srcv(jpr_ity2)%clgrid   ! not needed but cleaner...
      ENDIF
      !
      IF( TRIM( sn_rcv_tau%cldes ) /= 'oce and ice' ) THEN        ! 'oce and ice' case ocean stress on ocean mesh used
         srcv(jpr_itx1:jpr_itz2)%laction = .FALSE.    ! ice components not received
         srcv(jpr_itx1)%clgrid = 'U'                  ! ocean stress used after its transformation
         srcv(jpr_ity1)%clgrid = 'V'                  ! i.e. it is always at U- & V-points for i- & j-comp. resp.
      ENDIF
       
      !                                                      ! ------------------------- !
      !                                                      !    freshwater budget      !   E-P
      !                                                      ! ------------------------- !
      ! we suppose that atmosphere modele do not make the difference between precipiration (liquide or solid)
      ! over ice of free ocean within the same atmospheric cell.cd 
      srcv(jpr_rain)%clname = 'OTotRain'      ! Rain = liquid precipitation
      srcv(jpr_snow)%clname = 'OTotSnow'      ! Snow = solid precipitation
      srcv(jpr_tevp)%clname = 'OTotEvap'      ! total evaporation (over oce + ice sublimation)
      srcv(jpr_ievp)%clname = 'OIceEvap'      ! evaporation over ice = sublimation
      srcv(jpr_sbpr)%clname = 'OSubMPre'      ! sublimation - liquid precipitation - solid precipitation 
      srcv(jpr_semp)%clname = 'OISubMSn'      ! ice solid water budget = sublimation - solid precipitation
      srcv(jpr_oemp)%clname = 'OOEvaMPr'      ! ocean water budget = ocean Evap - ocean precip
      SELECT CASE( TRIM( sn_rcv_emp%cldes ) )
      CASE( 'oce only'      )   ;   srcv(                                 jpr_oemp   )%laction = .TRUE. 
      CASE( 'conservative'  )   ;   srcv( (/jpr_rain, jpr_snow, jpr_ievp, jpr_tevp/) )%laction = .TRUE.
      CASE( 'oce and ice'   )   ;   srcv( (/jpr_ievp, jpr_sbpr, jpr_semp, jpr_oemp/) )%laction = .TRUE.
      CASE default              ;   CALL ctl_stop( 'sbc_cpl_init: wrong definition of sn_rcv_emp%cldes' )
      END SELECT

      !                                                      ! ------------------------- !
      !                                                      !     Runoffs & Calving     !   
      !                                                      ! ------------------------- !
      srcv(jpr_rnf   )%clname = 'O_Runoff'   ;   IF( TRIM( sn_rcv_rnf%cldes ) == 'coupled' )   srcv(jpr_rnf)%laction = .TRUE.
! This isn't right - really just want ln_rnf_emp changed
!                                                 IF( TRIM( sn_rcv_rnf%cldes ) == 'climato' )   THEN   ;   ln_rnf = .TRUE.
!                                                 ELSE                                                 ;   ln_rnf = .FALSE.
!                                                 ENDIF
      srcv(jpr_cal   )%clname = 'OCalving'   ;   IF( TRIM( sn_rcv_cal%cldes ) == 'coupled' )   srcv(jpr_cal)%laction = .TRUE.

      !                                                      ! ------------------------- !
      !                                                      !    non solar radiation    !   Qns
      !                                                      ! ------------------------- !
      srcv(jpr_qnsoce)%clname = 'O_QnsOce'
      srcv(jpr_qnsice)%clname = 'O_QnsIce'
      srcv(jpr_qnsmix)%clname = 'O_QnsMix'
      SELECT CASE( TRIM( sn_rcv_qns%cldes ) )
      CASE( 'oce only'      )   ;   srcv(               jpr_qnsoce   )%laction = .TRUE.
      CASE( 'conservative'  )   ;   srcv( (/jpr_qnsice, jpr_qnsmix/) )%laction = .TRUE.
      CASE( 'oce and ice'   )   ;   srcv( (/jpr_qnsice, jpr_qnsoce/) )%laction = .TRUE.
      CASE( 'mixed oce-ice' )   ;   srcv(               jpr_qnsmix   )%laction = .TRUE. 
      CASE default              ;   CALL ctl_stop( 'sbc_cpl_init: wrong definition of sn_rcv_qns%cldes' )
      END SELECT
      IF( TRIM( sn_rcv_qns%cldes ) == 'mixed oce-ice' .AND. jpl > 1 ) &
         CALL ctl_stop( 'sbc_cpl_init: sn_rcv_qns%cldes not currently allowed to be mixed oce-ice for multi-category ice' )
      !                                                      ! ------------------------- !
      !                                                      !    solar radiation        !   Qsr
      !                                                      ! ------------------------- !
      srcv(jpr_qsroce)%clname = 'O_QsrOce'
      srcv(jpr_qsrice)%clname = 'O_QsrIce'
      srcv(jpr_qsrmix)%clname = 'O_QsrMix'
      SELECT CASE( TRIM( sn_rcv_qsr%cldes ) )
      CASE( 'oce only'      )   ;   srcv(               jpr_qsroce   )%laction = .TRUE.
      CASE( 'conservative'  )   ;   srcv( (/jpr_qsrice, jpr_qsrmix/) )%laction = .TRUE.
      CASE( 'oce and ice'   )   ;   srcv( (/jpr_qsrice, jpr_qsroce/) )%laction = .TRUE.
      CASE( 'mixed oce-ice' )   ;   srcv(               jpr_qsrmix   )%laction = .TRUE. 
      CASE default              ;   CALL ctl_stop( 'sbc_cpl_init: wrong definition of sn_rcv_qsr%cldes' )
      END SELECT
      IF( TRIM( sn_rcv_qsr%cldes ) == 'mixed oce-ice' .AND. jpl > 1 ) &
         CALL ctl_stop( 'sbc_cpl_init: sn_rcv_qsr%cldes not currently allowed to be mixed oce-ice for multi-category ice' )
      !                                                      ! ------------------------- !
      !                                                      !   non solar sensitivity   !   d(Qns)/d(T)
      !                                                      ! ------------------------- !
      srcv(jpr_dqnsdt)%clname = 'O_dQnsdT'   
      IF( TRIM( sn_rcv_dqnsdt%cldes ) == 'coupled' )   srcv(jpr_dqnsdt)%laction = .TRUE.
      !
      ! non solar sensitivity mandatory for LIM ice model
      IF( TRIM( sn_rcv_dqnsdt%cldes ) == 'none' .AND. k_ice /= 0 .AND. k_ice /= 4) &
         CALL ctl_stop( 'sbc_cpl_init: sn_rcv_dqnsdt%cldes must be coupled in namsbc_cpl namelist' )
      ! non solar sensitivity mandatory for mixed oce-ice solar radiation coupling technique
      IF( TRIM( sn_rcv_dqnsdt%cldes ) == 'none' .AND. TRIM( sn_rcv_qns%cldes ) == 'mixed oce-ice' ) &
         CALL ctl_stop( 'sbc_cpl_init: namsbc_cpl namelist mismatch between sn_rcv_qns%cldes and sn_rcv_dqnsdt%cldes' )
      !                                                      ! ------------------------- !
      !                                                      !    Ice Qsr penetration    !   
      !                                                      ! ------------------------- !
      ! fraction of net shortwave radiation which is not absorbed in the thin surface layer 
      ! and penetrates inside the ice cover ( Maykut and Untersteiner, 1971 ; Elbert anbd Curry, 1993 )
      ! Coupled case: since cloud cover is not received from atmosphere 
      !               ===> defined as constant value -> definition done in sbc_cpl_init
      fr1_i0(:,:) = 0.18
      fr2_i0(:,:) = 0.82
      !                                                      ! ------------------------- !
      !                                                      !      10m wind module      !   
      !                                                      ! ------------------------- !
      srcv(jpr_w10m)%clname = 'O_Wind10'   ;   IF( TRIM(sn_rcv_w10m%cldes  ) == 'coupled' )   srcv(jpr_w10m)%laction = .TRUE. 
      !
      !                                                      ! ------------------------- !
      !                                                      !   wind stress module      !   
      !                                                      ! ------------------------- !
      srcv(jpr_taum)%clname = 'O_TauMod'   ;   IF( TRIM(sn_rcv_taumod%cldes) == 'coupled' )   srcv(jpr_taum)%laction = .TRUE.
      lhftau = srcv(jpr_taum)%laction

      !                                                      ! ------------------------- !
      !                                                      !      Atmospheric CO2      !
      !                                                      ! ------------------------- !
      srcv(jpr_co2 )%clname = 'O_AtmCO2'   ;   IF( TRIM(sn_rcv_co2%cldes   ) == 'coupled' )    srcv(jpr_co2 )%laction = .TRUE.
      !                                                      ! ------------------------- !
      !                                                      !   topmelt and botmelt     !   
      !                                                      ! ------------------------- !
      srcv(jpr_topm )%clname = 'OTopMlt'
      srcv(jpr_botm )%clname = 'OBotMlt'
      IF( TRIM(sn_rcv_iceflx%cldes) == 'coupled' ) THEN
         IF ( TRIM( sn_rcv_iceflx%clcat ) == 'yes' ) THEN
            srcv(jpr_topm:jpr_botm)%nct = jpl
         ELSE
            CALL ctl_stop( 'sbc_cpl_init: sn_rcv_iceflx%clcat should always be set to yes currently' )
         ENDIF
         srcv(jpr_topm:jpr_botm)%laction = .TRUE.
      ENDIF

      ! Allocate all parts of frcv used for received fields
      DO jn = 1, jprcv
         IF ( srcv(jn)%laction ) ALLOCATE( frcv(jn)%z3(jpi,jpj,srcv(jn)%nct) )
      END DO
      ! Allocate taum part of frcv which is used even when not received as coupling field
      IF ( .NOT. srcv(jpr_taum)%laction ) ALLOCATE( frcv(jpr_taum)%z3(jpi,jpj,srcv(jn)%nct) )

      ! ================================ !
      !     Define the send interface    !
      ! ================================ !
      ! for each field: define the OASIS name                           (ssnd(:)%clname)
      !                 define send or not from the namelist parameters (ssnd(:)%laction)
      !                 define the north fold type of lbc               (ssnd(:)%nsgn)
      
      ! default definitions of nsnd
      ssnd(:)%laction = .FALSE.   ;   ssnd(:)%clgrid = 'T'   ;   ssnd(:)%nsgn = 1.  ; ssnd(:)%nct = 1
         
      !                                                      ! ------------------------- !
      !                                                      !    Surface temperature    !
      !                                                      ! ------------------------- !
      ssnd(jps_toce)%clname = 'O_SSTSST'
      ssnd(jps_tice)%clname = 'O_TepIce'
      ssnd(jps_tmix)%clname = 'O_TepMix'
      SELECT CASE( TRIM( sn_snd_temp%cldes ) )
      CASE( 'none'          )       ! nothing to do
      CASE( 'oce only'             )   ;   ssnd(   jps_toce             )%laction = .TRUE.
      CASE( 'weighted oce and ice' )
         ssnd( (/jps_toce, jps_tice/) )%laction = .TRUE.
         IF ( TRIM( sn_snd_temp%clcat ) == 'yes' )  ssnd(jps_tice)%nct = jpl
      CASE( 'mixed oce-ice'        )   ;   ssnd(   jps_tmix             )%laction = .TRUE.
      CASE default   ;   CALL ctl_stop( 'sbc_cpl_init: wrong definition of sn_snd_temp%cldes' )
      END SELECT
     
      !                                                      ! ------------------------- !
      !                                                      !          Albedo           !
      !                                                      ! ------------------------- !
      ssnd(jps_albice)%clname = 'O_AlbIce' 
      ssnd(jps_albmix)%clname = 'O_AlbMix'
      SELECT CASE( TRIM( sn_snd_alb%cldes ) )
      CASE( 'none'          )       ! nothing to do
      CASE( 'weighted ice'  )   ;   ssnd(jps_albice)%laction = .TRUE.
      CASE( 'mixed oce-ice' )   ;   ssnd(jps_albmix)%laction = .TRUE.
      CASE default   ;   CALL ctl_stop( 'sbc_cpl_init: wrong definition of sn_snd_alb%cldes' )
      END SELECT
      !
      ! Need to calculate oceanic albedo if
      !     1. sending mixed oce-ice albedo or
      !     2. receiving mixed oce-ice solar radiation 
      IF ( TRIM ( sn_snd_alb%cldes ) == 'mixed oce-ice' .OR. TRIM ( sn_rcv_qsr%cldes ) == 'mixed oce-ice' ) THEN
         CALL albedo_oce( zaos, zacs )
         ! Due to lack of information on nebulosity : mean clear/overcast sky
         albedo_oce_mix(:,:) = ( zacs(:,:) + zaos(:,:) ) * 0.5
      ENDIF

      !                                                      ! ------------------------- !
      !                                                      !  Ice fraction & Thickness ! 
      !                                                      ! ------------------------- !
      ssnd(jps_fice)%clname = 'OIceFrc'
      ssnd(jps_hice)%clname = 'OIceTck'
      ssnd(jps_hsnw)%clname = 'OSnwTck'
      IF( k_ice /= 0 ) THEN
         ssnd(jps_fice)%laction = .TRUE.                  ! if ice treated in the ocean (even in climato case)
! Currently no namelist entry to determine sending of multi-category ice fraction so use the thickness entry for now
         IF ( TRIM( sn_snd_thick%clcat ) == 'yes' ) ssnd(jps_fice)%nct = jpl
      ENDIF

      SELECT CASE ( TRIM( sn_snd_thick%cldes ) )
      CASE( 'none'         )       ! nothing to do
      CASE( 'ice and snow' ) 
         ssnd(jps_hice:jps_hsnw)%laction = .TRUE.
         IF ( TRIM( sn_snd_thick%clcat ) == 'yes' ) THEN
            ssnd(jps_hice:jps_hsnw)%nct = jpl
         ELSE
            IF ( jpl > 1 ) THEN
CALL ctl_stop( 'sbc_cpl_init: use weighted ice and snow option for sn_snd_thick%cldes if not exchanging category fields' )
            ENDIF
         ENDIF
      CASE( 'weighted ice and snow' ) 
         ssnd(jps_hice:jps_hsnw)%laction = .TRUE.
         IF ( TRIM( sn_snd_thick%clcat ) == 'yes' ) ssnd(jps_hice:jps_hsnw)%nct = jpl
      CASE default   ;   CALL ctl_stop( 'sbc_cpl_init: wrong definition of sn_snd_thick%cldes' )
      END SELECT

      !                                                      ! ------------------------- !
      !                                                      !      Surface current      !
      !                                                      ! ------------------------- !
      !        ocean currents              !            ice velocities
      ssnd(jps_ocx1)%clname = 'O_OCurx1'   ;   ssnd(jps_ivx1)%clname = 'O_IVelx1'
      ssnd(jps_ocy1)%clname = 'O_OCury1'   ;   ssnd(jps_ivy1)%clname = 'O_IVely1'
      ssnd(jps_ocz1)%clname = 'O_OCurz1'   ;   ssnd(jps_ivz1)%clname = 'O_IVelz1'
      !
      ssnd(jps_ocx1:jps_ivz1)%nsgn = -1.   ! vectors: change of the sign at the north fold

      IF( sn_snd_crt%clvgrd == 'U,V' ) THEN
         ssnd(jps_ocx1)%clgrid = 'U' ; ssnd(jps_ocy1)%clgrid = 'V'
      ELSE IF( sn_snd_crt%clvgrd /= 'T' ) THEN  
         CALL ctl_stop( 'sn_snd_crt%clvgrd must be equal to T' )
         ssnd(jps_ocx1:jps_ivz1)%clgrid  = 'T'      ! all oce and ice components on the same unique grid
      ENDIF
      ssnd(jps_ocx1:jps_ivz1)%laction = .TRUE.   ! default: all are send
      IF( TRIM( sn_snd_crt%clvref ) == 'spherical' )   ssnd( (/jps_ocz1, jps_ivz1/) )%laction = .FALSE. 
      IF( TRIM( sn_snd_crt%clvor ) == 'eastward-northward' ) ssnd(jps_ocx1:jps_ivz1)%nsgn = 1.
      SELECT CASE( TRIM( sn_snd_crt%cldes ) )
      CASE( 'none'                 )   ;   ssnd(jps_ocx1:jps_ivz1)%laction = .FALSE.
      CASE( 'oce only'             )   ;   ssnd(jps_ivx1:jps_ivz1)%laction = .FALSE.
      CASE( 'weighted oce and ice' )   !   nothing to do
      CASE( 'mixed oce-ice'        )   ;   ssnd(jps_ivx1:jps_ivz1)%laction = .FALSE.
      CASE default   ;   CALL ctl_stop( 'sbc_cpl_init: wrong definition of sn_snd_crt%cldes' )
      END SELECT

      !                                                      ! ------------------------- !
      !                                                      !          CO2 flux         !
      !                                                      ! ------------------------- !
      ssnd(jps_co2)%clname = 'O_CO2FLX' ;  IF( TRIM(sn_snd_co2%cldes) == 'coupled' )    ssnd(jps_co2 )%laction = .TRUE.
      !
      ! ================================ !
      !   initialisation of the coupler  !
      ! ================================ !

      CALL cpl_prism_define(jprcv, jpsnd, nn_nbgrdatm)   
      IF (ln_usecplmask) THEN 
         xcplmask(:,:,:) = 0.
         CALL iom_open( 'cplmask', inum )
         CALL iom_get(inum,jpdom_unknown,'cplmask',xcplmask(1:nlci,1:nlcj,1:2),kstart=(/mig(1),mjg(1),1/),kcount=(/nlci,nlcj,2/))
         CALL iom_close( inum )
      ELSE
         xcplmask(:,:,:) = 1.
      ENDIF
      
      !
      IF( ln_dm2dc .AND. ( cpl_prism_freq( jpr_qsroce ) + cpl_prism_freq( jpr_qsrmix ) /= 86400 ) )   &
         &   CALL ctl_stop( 'sbc_cpl_init: diurnal cycle reconstruction (ln_dm2dc) needs daily couping for solar radiation' )

      CALL wrk_dealloc( jpi,jpj, zacs, zaos )
      !
      IF( nn_timing == 1 )  CALL timing_stop('sbc_cpl_init')
      !
   END SUBROUTINE sbc_cpl_init


   SUBROUTINE sbc_cpl_rcv( kt, k_fsbc, k_ice )     
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE sbc_cpl_rcv  ***
      !!
      !! ** Purpose :   provide the stress over the ocean and, if no sea-ice,
      !!                provide the ocean heat and freshwater fluxes.
      !!
      !! ** Method  : - Receive all the atmospheric fields (stored in frcv array). called at each time step.
      !!                OASIS controls if there is something do receive or not. nrcvinfo contains the info
      !!                to know if the field was really received or not
      !!
      !!              --> If ocean stress was really received:
      !!
      !!                  - transform the received ocean stress vector from the received
      !!                 referential and grid into an atmosphere-ocean stress in 
      !!                 the (i,j) ocean referencial and at the ocean velocity point. 
      !!                    The received stress are :
      !!                     - defined by 3 components (if cartesian coordinate)
      !!                            or by 2 components (if spherical)
      !!                     - oriented along geographical   coordinate (if eastward-northward)
      !!                            or  along the local grid coordinate (if local grid)
      !!                     - given at U- and V-point, resp.   if received on 2 grids
      !!                            or at T-point               if received on 1 grid
      !!                    Therefore and if necessary, they are successively 
      !!                  processed in order to obtain them 
      !!                     first  as  2 components on the sphere 
      !!                     second as  2 components oriented along the local grid
      !!                     third  as  2 components on the U,V grid 
      !!
      !!              --> 
      !!
      !!              - In 'ocean only' case, non solar and solar ocean heat fluxes 
      !!             and total ocean freshwater fluxes  
      !!
      !! ** Method  :   receive all fields from the atmosphere and transform 
      !!              them into ocean surface boundary condition fields 
      !!
      !! ** Action  :   update  utau, vtau   ocean stress at U,V grid 
      !!                        taum, wndm   wind stres and wind speed module at T-point
      !!                        qns , qsr    non solar and solar ocean heat fluxes   ('ocean only case)
      !!                        emp = emps   evap. - precip. (- runoffs) (- calving) ('ocean only case)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt       ! ocean model time step index
      INTEGER, INTENT(in) ::   k_fsbc   ! frequency of sbc (-> ice model) computation 
      INTEGER, INTENT(in) ::   k_ice    ! ice management in the sbc (=0/1/2/3)
      !!
      LOGICAL ::    llnewtx, llnewtau      ! update wind stress components and module??
      INTEGER  ::   ji, jj, jn             ! dummy loop indices
      INTEGER  ::   isec                   ! number of seconds since nit000 (assuming rdttra did not change since nit000)
      REAL(wp) ::   zcumulneg, zcumulpos   ! temporary scalars     
      REAL(wp) ::   zcoef                  ! temporary scalar
      REAL(wp) ::   zrhoa  = 1.22          ! Air density kg/m3
      REAL(wp) ::   zcdrag = 1.5e-3        ! drag coefficient
      REAL(wp) ::   zzx, zzy               ! temporary variables
      REAL(wp), POINTER, DIMENSION(:,:) ::   ztx, zty 
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('sbc_cpl_rcv')
      !
      CALL wrk_alloc( jpi,jpj, ztx, zty )

!EM XIOS/OASIS-MCT compliance
!EM      IF( kt == nit000 )   CALL sbc_cpl_init( k_ice )          ! initialisation

      !                                                 ! Receive all the atmos. fields (including ice information)
      isec = ( kt - nit000 ) * NINT( rdttra(1) )             ! date of exchanges
      DO jn = 1, jprcv                                       ! received fields sent by the atmosphere
         IF( srcv(jn)%laction )   CALL cpl_prism_rcv( jn, isec, frcv(jn)%z3, xcplmask, nrcvinfo(jn) )
      END DO

      !                                                      ! ========================= !
      IF( srcv(jpr_otx1)%laction ) THEN                      !  ocean stress components  !
         !                                                   ! ========================= !
         ! define frcv(jpr_otx1)%z3(:,:,1) and frcv(jpr_oty1)%z3(:,:,1): stress at U/V point along model grid
         ! => need to be done only when we receive the field
         IF(  nrcvinfo(jpr_otx1) == OASIS_Rcv ) THEN
            !
            IF( TRIM( sn_rcv_tau%clvref ) == 'cartesian' ) THEN            ! 2 components on the sphere
               !                                                       ! (cartesian to spherical -> 3 to 2 components)
               !
               CALL geo2oce( frcv(jpr_otx1)%z3(:,:,1), frcv(jpr_oty1)%z3(:,:,1), frcv(jpr_otz1)%z3(:,:,1),   &
                  &          srcv(jpr_otx1)%clgrid, ztx, zty )
               frcv(jpr_otx1)%z3(:,:,1) = ztx(:,:)   ! overwrite 1st comp. on the 1st grid
               frcv(jpr_oty1)%z3(:,:,1) = zty(:,:)   ! overwrite 2nd comp. on the 1st grid
               !
               IF( srcv(jpr_otx2)%laction ) THEN
                  CALL geo2oce( frcv(jpr_otx2)%z3(:,:,1), frcv(jpr_oty2)%z3(:,:,1), frcv(jpr_otz2)%z3(:,:,1),   &
                     &          srcv(jpr_otx2)%clgrid, ztx, zty )
                  frcv(jpr_otx2)%z3(:,:,1) = ztx(:,:)   ! overwrite 1st comp. on the 2nd grid
                  frcv(jpr_oty2)%z3(:,:,1) = zty(:,:)   ! overwrite 2nd comp. on the 2nd grid
               ENDIF
               !
            ENDIF
            !
            IF( TRIM( sn_rcv_tau%clvor ) == 'eastward-northward' ) THEN   ! 2 components oriented along the local grid
               !                                                       ! (geographical to local grid -> rotate the components)
               CALL rot_rep( frcv(jpr_otx1)%z3(:,:,1), frcv(jpr_oty1)%z3(:,:,1), srcv(jpr_otx1)%clgrid, 'en->i', ztx )   
               IF( srcv(jpr_otx2)%laction ) THEN
                  CALL rot_rep( frcv(jpr_otx2)%z3(:,:,1), frcv(jpr_oty2)%z3(:,:,1), srcv(jpr_otx2)%clgrid, 'en->j', zty )   
               ELSE	
                  CALL rot_rep( frcv(jpr_otx1)%z3(:,:,1), frcv(jpr_oty1)%z3(:,:,1), srcv(jpr_otx1)%clgrid, 'en->j', zty )  
               ENDIF
               frcv(jpr_otx1)%z3(:,:,1) = ztx(:,:)      ! overwrite 1st component on the 1st grid
               frcv(jpr_oty1)%z3(:,:,1) = zty(:,:)      ! overwrite 2nd component on the 2nd grid
            ENDIF
            !                              
            IF( srcv(jpr_otx1)%clgrid == 'T' ) THEN
               DO jj = 2, jpjm1                                          ! T ==> (U,V)
                  DO ji = fs_2, fs_jpim1   ! vector opt.
                     frcv(jpr_otx1)%z3(ji,jj,1) = 0.5 * ( frcv(jpr_otx1)%z3(ji+1,jj  ,1) + frcv(jpr_otx1)%z3(ji,jj,1) )
                     frcv(jpr_oty1)%z3(ji,jj,1) = 0.5 * ( frcv(jpr_oty1)%z3(ji  ,jj+1,1) + frcv(jpr_oty1)%z3(ji,jj,1) )
                  END DO
               END DO
               CALL lbc_lnk( frcv(jpr_otx1)%z3(:,:,1), 'U',  -1. )   ;   CALL lbc_lnk( frcv(jpr_oty1)%z3(:,:,1), 'V',  -1. )
            ENDIF
            llnewtx = .TRUE.
         ELSE
            llnewtx = .FALSE.
         ENDIF
         !                                                   ! ========================= !
      ELSE                                                   !   No dynamical coupling   !
         !                                                   ! ========================= !
         frcv(jpr_otx1)%z3(:,:,1) = 0.e0                               ! here simply set to zero 
         frcv(jpr_oty1)%z3(:,:,1) = 0.e0                               ! an external read in a file can be added instead
         llnewtx = .TRUE.
         !
      ENDIF
      
      !                                                      ! ========================= !
      !                                                      !    wind stress module     !   (taum)
      !                                                      ! ========================= !
      !
      IF( .NOT. srcv(jpr_taum)%laction ) THEN                    ! compute wind stress module from its components if not received 
         ! => need to be done only when otx1 was changed
         IF( llnewtx ) THEN
!CDIR NOVERRCHK
            DO jj = 2, jpjm1
!CDIR NOVERRCHK
               DO ji = fs_2, fs_jpim1   ! vect. opt.
                  zzx = frcv(jpr_otx1)%z3(ji-1,jj  ,1) + frcv(jpr_otx1)%z3(ji,jj,1)
                  zzy = frcv(jpr_oty1)%z3(ji  ,jj-1,1) + frcv(jpr_oty1)%z3(ji,jj,1)
                  frcv(jpr_taum)%z3(ji,jj,1) = 0.5 * SQRT( zzx * zzx + zzy * zzy )
               END DO
            END DO
            CALL lbc_lnk( frcv(jpr_taum)%z3(:,:,1), 'T', 1. )
            llnewtau = .TRUE.
         ELSE
            llnewtau = .FALSE.
         ENDIF
      ELSE
         llnewtau = nrcvinfo(jpr_taum) == OASIS_Rcv
         ! Stress module can be negative when received (interpolation problem)
         IF( llnewtau ) THEN 
            frcv(jpr_taum)%z3(:,:,1) = MAX( 0.0e0, frcv(jpr_taum)%z3(:,:,1) )
         ENDIF
      ENDIF
      
      !                                                      ! ========================= !
      !                                                      !      10 m wind speed      !   (wndm)
      !                                                      ! ========================= !
      !
      IF( .NOT. srcv(jpr_w10m)%laction ) THEN                    ! compute wind spreed from wind stress module if not received  
         ! => need to be done only when taumod was changed
         IF( llnewtau ) THEN 
            zcoef = 1. / ( zrhoa * zcdrag ) 
!CDIR NOVERRCHK
            DO jj = 1, jpj
!CDIR NOVERRCHK
               DO ji = 1, jpi 
                  wndm(ji,jj) = SQRT( frcv(jpr_taum)%z3(ji,jj,1) * zcoef )
               END DO
            END DO
         ENDIF
      ELSE
         IF ( nrcvinfo(jpr_w10m) == OASIS_Rcv ) wndm(:,:) = frcv(jpr_w10m)%z3(:,:,1)
      ENDIF

      ! u(v)tau and taum will be modified by ice model
      ! -> need to be reset before each call of the ice/fsbc      
      IF( MOD( kt-1, k_fsbc ) == 0 ) THEN
         !
         utau(:,:) = frcv(jpr_otx1)%z3(:,:,1)
         vtau(:,:) = frcv(jpr_oty1)%z3(:,:,1)
         taum(:,:) = frcv(jpr_taum)%z3(:,:,1)
         CALL iom_put( "taum_oce", taum )   ! output wind stress module
         !  
      ENDIF

#if defined key_cpl_carbon_cycle
      !                                                              ! atmosph. CO2 (ppm)
      IF( srcv(jpr_co2)%laction )   atm_co2(:,:) = frcv(jpr_co2)%z3(:,:,1)
#endif

      !                                                      ! ========================= !
      IF( k_ice <= 1 ) THEN                                  !  heat & freshwater fluxes ! (Ocean only case)
         !                                                   ! ========================= !
         !
         !                                                       ! non solar heat flux over the ocean (qns)
         IF( srcv(jpr_qnsoce)%laction )   qns(:,:) = frcv(jpr_qnsoce)%z3(:,:,1)
         IF( srcv(jpr_qnsmix)%laction )   qns(:,:) = frcv(jpr_qnsmix)%z3(:,:,1)
         ! add the latent heat of solid precip. melting
         IF( srcv(jpr_snow  )%laction )   qns(:,:) = qns(:,:) - frcv(jpr_snow)%z3(:,:,1) * lfus              

         !                                                       ! solar flux over the ocean          (qsr)
         IF( srcv(jpr_qsroce)%laction )   qsr(:,:) = frcv(jpr_qsroce)%z3(:,:,1)
         IF( srcv(jpr_qsrmix)%laction )   qsr(:,:) = frcv(jpr_qsrmix)%z3(:,:,1)
         IF( ln_dm2dc )   qsr(:,:) = sbc_dcy( qsr )                           ! modify qsr to include the diurnal cycle
         !
         !                                                       ! total freshwater fluxes over the ocean (emp, emps)
         SELECT CASE( TRIM( sn_rcv_emp%cldes ) )                                    ! evaporation - precipitation
         CASE( 'conservative' )
            emp(:,:) = frcv(jpr_tevp)%z3(:,:,1) - ( frcv(jpr_rain)%z3(:,:,1) + frcv(jpr_snow)%z3(:,:,1) )
         CASE( 'oce only', 'oce and ice' )
            emp(:,:) = frcv(jpr_oemp)%z3(:,:,1)
         CASE default
            CALL ctl_stop( 'sbc_cpl_rcv: wrong definition of sn_rcv_emp%cldes' )
         END SELECT
         !
         !                                                        ! runoffs and calving (added in emp)
         IF( srcv(jpr_rnf)%laction )   emp(:,:) = emp(:,:) - frcv(jpr_rnf)%z3(:,:,1)
         IF( srcv(jpr_cal)%laction )   emp(:,:) = emp(:,:) - frcv(jpr_cal)%z3(:,:,1)
         !
!!gm :  this seems to be internal cooking, not sure to need that in a generic interface 
!!gm                                       at least should be optional...
!!         IF( TRIM( sn_rcv_rnf%cldes ) == 'coupled' ) THEN     ! add to the total freshwater budget
!!            ! remove negative runoff
!!            zcumulpos = SUM( MAX( frcv(jpr_rnf)%z3(:,:,1), 0.e0 ) * e1t(:,:) * e2t(:,:) * tmask_i(:,:) ) 
!!            zcumulneg = SUM( MIN( frcv(jpr_rnf)%z3(:,:,1), 0.e0 ) * e1t(:,:) * e2t(:,:) * tmask_i(:,:) )
!!            IF( lk_mpp )   CALL mpp_sum( zcumulpos )   ! sum over the global domain
!!            IF( lk_mpp )   CALL mpp_sum( zcumulneg ) 
!!            IF( zcumulpos /= 0. ) THEN                 ! distribute negative runoff on positive runoff grid points
!!               zcumulneg = 1.e0 + zcumulneg / zcumulpos
!!               frcv(jpr_rnf)%z3(:,:,1) = MAX( frcv(jpr_rnf)%z3(:,:,1), 0.e0 ) * zcumulneg
!!            ENDIF     
!!            ! add runoff to e-p 
!!            emp(:,:) = emp(:,:) - frcv(jpr_rnf)%z3(:,:,1)
!!         ENDIF
!!gm  end of internal cooking
         !
         emps(:,:) = emp(:,:)                                        ! concentration/dilution = emp
  
      ENDIF
      !
      CALL wrk_dealloc( jpi,jpj, ztx, zty )
      !
      IF( nn_timing == 1 )  CALL timing_stop('sbc_cpl_rcv')
      !
   END SUBROUTINE sbc_cpl_rcv
   

   SUBROUTINE sbc_cpl_ice_tau( p_taui, p_tauj )     
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE sbc_cpl_ice_tau  ***
      !!
      !! ** Purpose :   provide the stress over sea-ice in coupled mode 
      !!
      !! ** Method  :   transform the received stress from the atmosphere into
      !!             an atmosphere-ice stress in the (i,j) ocean referencial
      !!             and at the velocity point of the sea-ice model (cp_ice_msh):
      !!                'C'-grid : i- (j-) components given at U- (V-) point 
      !!                'I'-grid : B-grid lower-left corner: both components given at I-point 
      !!
      !!                The received stress are :
      !!                 - defined by 3 components (if cartesian coordinate)
      !!                        or by 2 components (if spherical)
      !!                 - oriented along geographical   coordinate (if eastward-northward)
      !!                        or  along the local grid coordinate (if local grid)
      !!                 - given at U- and V-point, resp.   if received on 2 grids
      !!                        or at a same point (T or I) if received on 1 grid
      !!                Therefore and if necessary, they are successively 
      !!             processed in order to obtain them 
      !!                 first  as  2 components on the sphere 
      !!                 second as  2 components oriented along the local grid
      !!                 third  as  2 components on the cp_ice_msh point 
      !!
      !!                In 'oce and ice' case, only one vector stress field 
      !!             is received. It has already been processed in sbc_cpl_rcv
      !!             so that it is now defined as (i,j) components given at U-
      !!             and V-points, respectively. Therefore, here only the third
      !!             transformation is done and only if the ice-grid is a 'I'-grid. 
      !!
      !! ** Action  :   return ptau_i, ptau_j, the stress over the ice at cp_ice_msh point
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(out), DIMENSION(:,:) ::   p_taui   ! i- & j-components of atmos-ice stress [N/m2]
      REAL(wp), INTENT(out), DIMENSION(:,:) ::   p_tauj   ! at I-point (B-grid) or U & V-point (C-grid)
      !!
      INTEGER ::   ji, jj                          ! dummy loop indices
      INTEGER ::   itx                             ! index of taux over ice
      REAL(wp), POINTER, DIMENSION(:,:) ::   ztx, zty 
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('sbc_cpl_ice_tau')
      !
      CALL wrk_alloc( jpi,jpj, ztx, zty )

      IF( srcv(jpr_itx1)%laction ) THEN   ;   itx =  jpr_itx1   
      ELSE                                ;   itx =  jpr_otx1
      ENDIF

      ! do something only if we just received the stress from atmosphere
      IF(  nrcvinfo(itx) == OASIS_Rcv ) THEN

         !                                                      ! ======================= !
         IF( srcv(jpr_itx1)%laction ) THEN                      !   ice stress received   !
            !                                                   ! ======================= !
            !  
            IF( TRIM( sn_rcv_tau%clvref ) == 'cartesian' ) THEN            ! 2 components on the sphere
               !                                                       ! (cartesian to spherical -> 3 to 2 components)
               CALL geo2oce(  frcv(jpr_itx1)%z3(:,:,1), frcv(jpr_ity1)%z3(:,:,1), frcv(jpr_itz1)%z3(:,:,1),   &
                  &          srcv(jpr_itx1)%clgrid, ztx, zty )
               frcv(jpr_itx1)%z3(:,:,1) = ztx(:,:)   ! overwrite 1st comp. on the 1st grid
               frcv(jpr_ity1)%z3(:,:,1) = zty(:,:)   ! overwrite 2nd comp. on the 1st grid
               !
               IF( srcv(jpr_itx2)%laction ) THEN
                  CALL geo2oce( frcv(jpr_itx2)%z3(:,:,1), frcv(jpr_ity2)%z3(:,:,1), frcv(jpr_itz2)%z3(:,:,1),   &
                     &          srcv(jpr_itx2)%clgrid, ztx, zty )
                  frcv(jpr_itx2)%z3(:,:,1) = ztx(:,:)   ! overwrite 1st comp. on the 2nd grid
                  frcv(jpr_ity2)%z3(:,:,1) = zty(:,:)   ! overwrite 2nd comp. on the 2nd grid
               ENDIF
               !
            ENDIF
            !
            IF( TRIM( sn_rcv_tau%clvor ) == 'eastward-northward' ) THEN   ! 2 components oriented along the local grid
               !                                                       ! (geographical to local grid -> rotate the components)
               CALL rot_rep( frcv(jpr_itx1)%z3(:,:,1), frcv(jpr_ity1)%z3(:,:,1), srcv(jpr_itx1)%clgrid, 'en->i', ztx )   
               IF( srcv(jpr_itx2)%laction ) THEN
                  CALL rot_rep( frcv(jpr_itx2)%z3(:,:,1), frcv(jpr_ity2)%z3(:,:,1), srcv(jpr_itx2)%clgrid, 'en->j', zty )   
               ELSE
                  CALL rot_rep( frcv(jpr_itx1)%z3(:,:,1), frcv(jpr_ity1)%z3(:,:,1), srcv(jpr_itx1)%clgrid, 'en->j', zty )  
               ENDIF
               frcv(jpr_itx1)%z3(:,:,1) = ztx(:,:)      ! overwrite 1st component on the 1st grid
               frcv(jpr_ity1)%z3(:,:,1) = zty(:,:)      ! overwrite 2nd component on the 1st grid
            ENDIF
            !                                                   ! ======================= !
         ELSE                                                   !     use ocean stress    !
            !                                                   ! ======================= !
            frcv(jpr_itx1)%z3(:,:,1) = frcv(jpr_otx1)%z3(:,:,1)
            frcv(jpr_ity1)%z3(:,:,1) = frcv(jpr_oty1)%z3(:,:,1)
            !
         ENDIF

         !                                                      ! ======================= !
         !                                                      !     put on ice grid     !
         !                                                      ! ======================= !
         !    
         !                                                  j+1   j     -----V---F
         ! ice stress on ice velocity point (cp_ice_msh)                 !       |
         ! (C-grid ==>(U,V) or B-grid ==> I or F)                 j      |   T   U
         !                                                               |       |
         !                                                   j    j-1   -I-------|
         !                                               (for I)         |       |
         !                                                              i-1  i   i
         !                                                               i      i+1 (for I)
         SELECT CASE ( cp_ice_msh )
            !
         CASE( 'I' )                                         ! B-grid ==> I
            SELECT CASE ( srcv(jpr_itx1)%clgrid )
            CASE( 'U' )
               DO jj = 2, jpjm1                                   ! (U,V) ==> I
                  DO ji = 2, jpim1   ! NO vector opt.
                     p_taui(ji,jj) = 0.5 * ( frcv(jpr_itx1)%z3(ji-1,jj  ,1) + frcv(jpr_itx1)%z3(ji-1,jj-1,1) )
                     p_tauj(ji,jj) = 0.5 * ( frcv(jpr_ity1)%z3(ji  ,jj-1,1) + frcv(jpr_ity1)%z3(ji-1,jj-1,1) )
                  END DO
               END DO
            CASE( 'F' )
               DO jj = 2, jpjm1                                   ! F ==> I
                  DO ji = 2, jpim1   ! NO vector opt.
                     p_taui(ji,jj) = frcv(jpr_itx1)%z3(ji-1,jj-1,1)
                     p_tauj(ji,jj) = frcv(jpr_ity1)%z3(ji-1,jj-1,1)
                  END DO
               END DO
            CASE( 'T' )
               DO jj = 2, jpjm1                                   ! T ==> I
                  DO ji = 2, jpim1   ! NO vector opt.
                     p_taui(ji,jj) = 0.25 * ( frcv(jpr_itx1)%z3(ji,jj  ,1) + frcv(jpr_itx1)%z3(ji-1,jj  ,1)   &
                        &                   + frcv(jpr_itx1)%z3(ji,jj-1,1) + frcv(jpr_itx1)%z3(ji-1,jj-1,1) ) 
                     p_tauj(ji,jj) = 0.25 * ( frcv(jpr_ity1)%z3(ji,jj  ,1) + frcv(jpr_ity1)%z3(ji-1,jj  ,1)   &
                        &                   + frcv(jpr_oty1)%z3(ji,jj-1,1) + frcv(jpr_ity1)%z3(ji-1,jj-1,1) )
                  END DO
               END DO
            CASE( 'I' )
               p_taui(:,:) = frcv(jpr_itx1)%z3(:,:,1)                   ! I ==> I
               p_tauj(:,:) = frcv(jpr_ity1)%z3(:,:,1)
            END SELECT
            IF( srcv(jpr_itx1)%clgrid /= 'I' ) THEN 
               CALL lbc_lnk( p_taui, 'I',  -1. )   ;   CALL lbc_lnk( p_tauj, 'I',  -1. )
            ENDIF
            !
         CASE( 'F' )                                         ! B-grid ==> F
            SELECT CASE ( srcv(jpr_itx1)%clgrid )
            CASE( 'U' )
               DO jj = 2, jpjm1                                   ! (U,V) ==> F
                  DO ji = fs_2, fs_jpim1   ! vector opt.
                     p_taui(ji,jj) = 0.5 * ( frcv(jpr_itx1)%z3(ji,jj,1) + frcv(jpr_itx1)%z3(ji  ,jj+1,1) )
                     p_tauj(ji,jj) = 0.5 * ( frcv(jpr_ity1)%z3(ji,jj,1) + frcv(jpr_ity1)%z3(ji+1,jj  ,1) )
                  END DO
               END DO
            CASE( 'I' )
               DO jj = 2, jpjm1                                   ! I ==> F
                  DO ji = 2, jpim1   ! NO vector opt.
                     p_taui(ji,jj) = frcv(jpr_itx1)%z3(ji+1,jj+1,1)
                     p_tauj(ji,jj) = frcv(jpr_ity1)%z3(ji+1,jj+1,1)
                  END DO
               END DO
            CASE( 'T' )
               DO jj = 2, jpjm1                                   ! T ==> F
                  DO ji = 2, jpim1   ! NO vector opt.
                     p_taui(ji,jj) = 0.25 * ( frcv(jpr_itx1)%z3(ji,jj  ,1) + frcv(jpr_itx1)%z3(ji+1,jj  ,1)   &
                        &                   + frcv(jpr_itx1)%z3(ji,jj+1,1) + frcv(jpr_itx1)%z3(ji+1,jj+1,1) ) 
                     p_tauj(ji,jj) = 0.25 * ( frcv(jpr_ity1)%z3(ji,jj  ,1) + frcv(jpr_ity1)%z3(ji+1,jj  ,1)   &
                        &                   + frcv(jpr_ity1)%z3(ji,jj+1,1) + frcv(jpr_ity1)%z3(ji+1,jj+1,1) )
                  END DO
               END DO
            CASE( 'F' )
               p_taui(:,:) = frcv(jpr_itx1)%z3(:,:,1)                   ! F ==> F
               p_tauj(:,:) = frcv(jpr_ity1)%z3(:,:,1)
            END SELECT
            IF( srcv(jpr_itx1)%clgrid /= 'F' ) THEN 
               CALL lbc_lnk( p_taui, 'F',  -1. )   ;   CALL lbc_lnk( p_tauj, 'F',  -1. )
            ENDIF
            !
         CASE( 'C' )                                         ! C-grid ==> U,V
            SELECT CASE ( srcv(jpr_itx1)%clgrid )
            CASE( 'U' )
               p_taui(:,:) = frcv(jpr_itx1)%z3(:,:,1)                   ! (U,V) ==> (U,V)
               p_tauj(:,:) = frcv(jpr_ity1)%z3(:,:,1)
            CASE( 'F' )
               DO jj = 2, jpjm1                                   ! F ==> (U,V)
                  DO ji = fs_2, fs_jpim1   ! vector opt.
                     p_taui(ji,jj) = 0.5 * ( frcv(jpr_itx1)%z3(ji,jj,1) + frcv(jpr_itx1)%z3(ji  ,jj-1,1) )
                     p_tauj(ji,jj) = 0.5 * ( frcv(jpr_ity1)%z3(jj,jj,1) + frcv(jpr_ity1)%z3(ji-1,jj  ,1) )
                  END DO
               END DO
            CASE( 'T' )
               DO jj = 2, jpjm1                                   ! T ==> (U,V)
                  DO ji = fs_2, fs_jpim1   ! vector opt.
                     p_taui(ji,jj) = 0.5 * ( frcv(jpr_itx1)%z3(ji+1,jj  ,1) + frcv(jpr_itx1)%z3(ji,jj,1) )
                     p_tauj(ji,jj) = 0.5 * ( frcv(jpr_ity1)%z3(ji  ,jj+1,1) + frcv(jpr_ity1)%z3(ji,jj,1) )
                  END DO
               END DO
            CASE( 'I' )
               DO jj = 2, jpjm1                                   ! I ==> (U,V)
                  DO ji = 2, jpim1   ! NO vector opt.
                     p_taui(ji,jj) = 0.5 * ( frcv(jpr_itx1)%z3(ji+1,jj+1,1) + frcv(jpr_itx1)%z3(ji+1,jj  ,1) )
                     p_tauj(ji,jj) = 0.5 * ( frcv(jpr_ity1)%z3(ji+1,jj+1,1) + frcv(jpr_ity1)%z3(ji  ,jj+1,1) )
                  END DO
               END DO
            END SELECT
            IF( srcv(jpr_itx1)%clgrid /= 'U' ) THEN 
               CALL lbc_lnk( p_taui, 'U',  -1. )   ;   CALL lbc_lnk( p_tauj, 'V',  -1. )
            ENDIF
         END SELECT

      ENDIF
      !   
      CALL wrk_dealloc( jpi,jpj, ztx, zty )
      !
      IF( nn_timing == 1 )  CALL timing_stop('sbc_cpl_ice_tau')
      !
   END SUBROUTINE sbc_cpl_ice_tau
   

   SUBROUTINE sbc_cpl_ice_flx( p_frld  , palbi   , psst    , pist    )
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE sbc_cpl_ice_flx  ***
      !!
      !! ** Purpose :   provide the heat and freshwater fluxes of the 
      !!              ocean-ice system.
      !!
      !! ** Method  :   transform the fields received from the atmosphere into
      !!             surface heat and fresh water boundary condition for the 
      !!             ice-ocean system. The following fields are provided:
      !!              * total non solar, solar and freshwater fluxes (qns_tot, 
      !!             qsr_tot and emp_tot) (total means weighted ice-ocean flux)
      !!             NB: emp_tot include runoffs and calving.
      !!              * fluxes over ice (qns_ice, qsr_ice, emp_ice) where
      !!             emp_ice = sublimation - solid precipitation as liquid
      !!             precipitation are re-routed directly to the ocean and 
      !!             runoffs and calving directly enter the ocean.
      !!              * solid precipitation (sprecip), used to add to qns_tot 
      !!             the heat lost associated to melting solid precipitation
      !!             over the ocean fraction.
      !!       ===>> CAUTION here this changes the net heat flux received from
      !!             the atmosphere
      !!
      !!                  - the fluxes have been separated from the stress as
      !!                 (a) they are updated at each ice time step compare to
      !!                 an update at each coupled time step for the stress, and
      !!                 (b) the conservative computation of the fluxes over the
      !!                 sea-ice area requires the knowledge of the ice fraction
      !!                 after the ice advection and before the ice thermodynamics,
      !!                 so that the stress is updated before the ice dynamics
      !!                 while the fluxes are updated after it.
      !!
      !! ** Action  :   update at each nf_ice time step:
      !!                   qns_tot, qsr_tot  non-solar and solar total heat fluxes
      !!                   qns_ice, qsr_ice  non-solar and solar heat fluxes over the ice
      !!                   emp_tot            total evaporation - precipitation(liquid and solid) (-runoff)(-calving)
      !!                   emp_ice            ice sublimation - solid precipitation over the ice
      !!                   dqns_ice           d(non-solar heat flux)/d(Temperature) over the ice
      !!                   sprecip             solid precipitation over the ocean  
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   ), DIMENSION(:,:)   ::   p_frld     ! lead fraction                [0 to 1]
      ! optional arguments, used only in 'mixed oce-ice' case
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:), OPTIONAL ::   palbi   ! ice albedo 
      REAL(wp), INTENT(in   ), DIMENSION(:,:  ), OPTIONAL ::   psst    ! sea surface temperature     [Celcius]
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:), OPTIONAL ::   pist    ! ice surface temperature     [Kelvin]
      !
      INTEGER ::   jl   ! dummy loop index
      REAL(wp), POINTER, DIMENSION(:,:) ::   zcptn, ztmp, zicefr
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('sbc_cpl_ice_flx')
      !
      CALL wrk_alloc( jpi,jpj, zcptn, ztmp, zicefr )

      zicefr(:,:) = 1.- p_frld(:,:)
      IF( lk_diaar5 )   zcptn(:,:) = rcp * tsn(:,:,1,jp_tem)
      !
      !                                                      ! ========================= !
      !                                                      !    freshwater budget      !   (emp)
      !                                                      ! ========================= !
      !
      !                                                           ! total Precipitations - total Evaporation (emp_tot)
      !                                                           ! solid precipitation  - sublimation       (emp_ice)
      !                                                           ! solid Precipitation                      (sprecip)
      SELECT CASE( TRIM( sn_rcv_emp%cldes ) )
      CASE( 'conservative'  )   ! received fields: jpr_rain, jpr_snow, jpr_ievp, jpr_tevp
         sprecip(:,:) = frcv(jpr_snow)%z3(:,:,1)                 ! May need to ensure positive here
         tprecip(:,:) = frcv(jpr_rain)%z3(:,:,1) + sprecip (:,:) ! May need to ensure positive here
         emp_tot(:,:) = frcv(jpr_tevp)%z3(:,:,1) - tprecip(:,:)
         emp_ice(:,:) = frcv(jpr_ievp)%z3(:,:,1) - frcv(jpr_snow)%z3(:,:,1)
                           CALL iom_put( 'rain'         , frcv(jpr_rain)%z3(:,:,1)              )   ! liquid precipitation 
         IF( lk_diaar5 )   CALL iom_put( 'hflx_rain_cea', frcv(jpr_rain)%z3(:,:,1) * zcptn(:,:) )   ! heat flux from liq. precip. 
         ztmp(:,:) = frcv(jpr_tevp)%z3(:,:,1) - frcv(jpr_ievp)%z3(:,:,1) * zicefr(:,:)
                           CALL iom_put( 'evap_ao_cea'  , ztmp                            )   ! ice-free oce evap (cell average)
         IF( lk_diaar5 )   CALL iom_put( 'hflx_evap_cea', ztmp(:,:         ) * zcptn(:,:) )   ! heat flux from from evap (cell ave)
      CASE( 'oce and ice'   )   ! received fields: jpr_sbpr, jpr_semp, jpr_oemp, jpr_ievp
         emp_tot(:,:) = p_frld(:,:) * frcv(jpr_oemp)%z3(:,:,1) + zicefr(:,:) * frcv(jpr_sbpr)%z3(:,:,1)
         emp_ice(:,:) = frcv(jpr_semp)%z3(:,:,1)
         sprecip(:,:) = - frcv(jpr_semp)%z3(:,:,1) + frcv(jpr_ievp)%z3(:,:,1)
      END SELECT

      CALL iom_put( 'snowpre'    , sprecip                                )   ! Snow
      CALL iom_put( 'snow_ao_cea', sprecip(:,:         ) * p_frld(:,:)    )   ! Snow        over ice-free ocean  (cell average)
      CALL iom_put( 'snow_ai_cea', sprecip(:,:         ) * zicefr(:,:)    )   ! Snow        over sea-ice         (cell average)
      CALL iom_put( 'subl_ai_cea', frcv(jpr_ievp)%z3(:,:,1) * zicefr(:,:) )   ! Sublimation over sea-ice         (cell average)
      !   
      !                                                           ! runoffs and calving (put in emp_tot)
      IF( srcv(jpr_rnf)%laction ) THEN 
         emp_tot(:,:) = emp_tot(:,:) - frcv(jpr_rnf)%z3(:,:,1)
                           CALL iom_put( 'runoffs'      , frcv(jpr_rnf)%z3(:,:,1)              )   ! rivers
         IF( lk_diaar5 )   CALL iom_put( 'hflx_rnf_cea' , frcv(jpr_rnf)%z3(:,:,1) * zcptn(:,:) )   ! heat flux from rivers
      ENDIF
      IF( srcv(jpr_cal)%laction ) THEN 
         emp_tot(:,:) = emp_tot(:,:) - frcv(jpr_cal)%z3(:,:,1)
         CALL iom_put( 'calving', frcv(jpr_cal)%z3(:,:,1) )
      ENDIF
      !
!!gm :  this seems to be internal cooking, not sure to need that in a generic interface 
!!gm                                       at least should be optional...
!!       ! remove negative runoff                            ! sum over the global domain
!!       zcumulpos = SUM( MAX( frcv(jpr_rnf)%z3(:,:,1), 0.e0 ) * e1t(:,:) * e2t(:,:) * tmask_i(:,:) ) 
!!       zcumulneg = SUM( MIN( frcv(jpr_rnf)%z3(:,:,1), 0.e0 ) * e1t(:,:) * e2t(:,:) * tmask_i(:,:) )
!!       IF( lk_mpp )   CALL mpp_sum( zcumulpos )
!!       IF( lk_mpp )   CALL mpp_sum( zcumulneg ) 
!!       IF( zcumulpos /= 0. ) THEN                          ! distribute negative runoff on positive runoff grid points
!!          zcumulneg = 1.e0 + zcumulneg / zcumulpos
!!          frcv(jpr_rnf)%z3(:,:,1) = MAX( frcv(jpr_rnf)%z3(:,:,1), 0.e0 ) * zcumulneg
!!       ENDIF     
!!       emp_tot(:,:) = emp_tot(:,:) - frcv(jpr_rnf)%z3(:,:,1)   ! add runoff to e-p 
!!
!!gm  end of internal cooking

      !                                                      ! ========================= !
      SELECT CASE( TRIM( sn_rcv_qns%cldes ) )                !   non solar heat fluxes   !   (qns)
      !                                                      ! ========================= !
      CASE( 'oce only' )                                     ! the required field is directly provided
         qns_tot(:,:  ) = frcv(jpr_qnsoce)%z3(:,:,1)
      CASE( 'conservative' )                                      ! the required fields are directly provided
         qns_tot(:,:  ) = frcv(jpr_qnsmix)%z3(:,:,1)
         IF ( TRIM(sn_rcv_qns%clcat) == 'yes' ) THEN
            qns_ice(:,:,1:jpl) = frcv(jpr_qnsice)%z3(:,:,1:jpl)
         ELSE
            ! Set all category values equal for the moment
            DO jl=1,jpl
               qns_ice(:,:,jl) = frcv(jpr_qnsice)%z3(:,:,1)
            ENDDO
         ENDIF
      CASE( 'oce and ice' )       ! the total flux is computed from ocean and ice fluxes
         qns_tot(:,:  ) =  p_frld(:,:) * frcv(jpr_qnsoce)%z3(:,:,1)
         IF ( TRIM(sn_rcv_qns%clcat) == 'yes' ) THEN
            DO jl=1,jpl
               qns_tot(:,:   ) = qns_tot(:,:) + a_i(:,:,jl) * frcv(jpr_qnsice)%z3(:,:,jl)   
               qns_ice(:,:,jl) = frcv(jpr_qnsice)%z3(:,:,jl)
            ENDDO
         ELSE
            DO jl=1,jpl
               qns_tot(:,:   ) = qns_tot(:,:) + zicefr(:,:) * frcv(jpr_qnsice)%z3(:,:,1)
               qns_ice(:,:,jl) = frcv(jpr_qnsice)%z3(:,:,1)
            ENDDO
         ENDIF
      CASE( 'mixed oce-ice' )     ! the ice flux is cumputed from the total flux, the SST and ice informations
! ** NEED TO SORT OUT HOW THIS SHOULD WORK IN THE MULTI-CATEGORY CASE - CURRENTLY NOT ALLOWED WHEN INTERFACE INITIALISED **
         qns_tot(:,:  ) = frcv(jpr_qnsmix)%z3(:,:,1)
         qns_ice(:,:,1) = frcv(jpr_qnsmix)%z3(:,:,1)    &
            &            + frcv(jpr_dqnsdt)%z3(:,:,1) * ( pist(:,:,1) - ( (rt0 + psst(:,:  ) ) * p_frld(:,:)   &
            &                                                   +          pist(:,:,1)   * zicefr(:,:) ) )
      END SELECT
      ztmp(:,:) = p_frld(:,:) * sprecip(:,:) * lfus               ! add the latent heat of solid precip. melting
      qns_tot(:,:) = qns_tot(:,:) - ztmp(:,:)                     ! over free ocean 
      IF( lk_diaar5 )   CALL iom_put( 'hflx_snow_cea', ztmp + sprecip(:,:) * zcptn(:,:) )   ! heat flux from snow (cell average)
!!gm
!!    currently it is taken into account in leads budget but not in the qns_tot, and thus not in 
!!    the flux that enter the ocean....
!!    moreover 1 - it is not diagnose anywhere.... 
!!             2 - it is unclear for me whether this heat lost is taken into account in the atmosphere or not...
!!
!! similar job should be done for snow and precipitation temperature
      !                                     
      IF( srcv(jpr_cal)%laction ) THEN                            ! Iceberg melting 
         ztmp(:,:) = frcv(jpr_cal)%z3(:,:,1) * lfus               ! add the latent heat of iceberg melting 
         qns_tot(:,:) = qns_tot(:,:) - ztmp(:,:)
         IF( lk_diaar5 )   CALL iom_put( 'hflx_cal_cea', ztmp + frcv(jpr_cal)%z3(:,:,1) * zcptn(:,:) )   ! heat flux from calving
      ENDIF

      !                                                      ! ========================= !
      SELECT CASE( TRIM( sn_rcv_qsr%cldes ) )                !      solar heat fluxes    !   (qsr)
      !                                                      ! ========================= !
      CASE( 'oce only' )
         qsr_tot(:,:  ) = MAX(0.0,frcv(jpr_qsroce)%z3(:,:,1))
      CASE( 'conservative' )
         qsr_tot(:,:  ) = frcv(jpr_qsrmix)%z3(:,:,1)
         IF ( TRIM(sn_rcv_qsr%clcat) == 'yes' ) THEN
            qsr_ice(:,:,1:jpl) = frcv(jpr_qsrice)%z3(:,:,1:jpl)
         ELSE
            ! Set all category values equal for the moment
            DO jl=1,jpl
               qsr_ice(:,:,jl) = frcv(jpr_qsrice)%z3(:,:,1)
            ENDDO
         ENDIF
         qsr_tot(:,:  ) = frcv(jpr_qsrmix)%z3(:,:,1)
         qsr_ice(:,:,1) = frcv(jpr_qsrice)%z3(:,:,1)
      CASE( 'oce and ice' )
         qsr_tot(:,:  ) =  p_frld(:,:) * frcv(jpr_qsroce)%z3(:,:,1)
         IF ( TRIM(sn_rcv_qsr%clcat) == 'yes' ) THEN
            DO jl=1,jpl
               qsr_tot(:,:   ) = qsr_tot(:,:) + a_i(:,:,jl) * frcv(jpr_qsrice)%z3(:,:,jl)   
               qsr_ice(:,:,jl) = frcv(jpr_qsrice)%z3(:,:,jl)
            ENDDO
         ELSE
            DO jl=1,jpl
               qsr_tot(:,:   ) = qsr_tot(:,:) + zicefr(:,:) * frcv(jpr_qsrice)%z3(:,:,1)
               qsr_ice(:,:,jl) = frcv(jpr_qsrice)%z3(:,:,1)
            ENDDO
         ENDIF
      CASE( 'mixed oce-ice' )
         qsr_tot(:,:  ) = frcv(jpr_qsrmix)%z3(:,:,1)
! ** NEED TO SORT OUT HOW THIS SHOULD WORK IN THE MULTI-CATEGORY CASE - CURRENTLY NOT ALLOWED WHEN INTERFACE INITIALISED **
!       Create solar heat flux over ice using incoming solar heat flux and albedos
!       ( see OASIS3 user guide, 5th edition, p39 )
         qsr_ice(:,:,1) = frcv(jpr_qsrmix)%z3(:,:,1) * ( 1.- palbi(:,:,1) )   &
            &            / (  1.- ( albedo_oce_mix(:,:  ) * p_frld(:,:)       &
            &                     + palbi         (:,:,1) * zicefr(:,:) ) )
      END SELECT
      IF( ln_dm2dc ) THEN   ! modify qsr to include the diurnal cycle
         qsr_tot(:,:  ) = sbc_dcy( qsr_tot(:,:  ) )
         DO jl=1,jpl
            qsr_ice(:,:,jl) = sbc_dcy( qsr_ice(:,:,jl) )
         ENDDO
      ENDIF

      SELECT CASE( TRIM( sn_rcv_dqnsdt%cldes ) )
      CASE ('coupled')
         IF ( TRIM(sn_rcv_dqnsdt%clcat) == 'yes' ) THEN
            dqns_ice(:,:,1:jpl) = frcv(jpr_dqnsdt)%z3(:,:,1:jpl)
         ELSE
            ! Set all category values equal for the moment
            DO jl=1,jpl
               dqns_ice(:,:,jl) = frcv(jpr_dqnsdt)%z3(:,:,1)
            ENDDO
         ENDIF
      END SELECT

      SELECT CASE( TRIM( sn_rcv_iceflx%cldes ) )
      CASE ('coupled')
         topmelt(:,:,:)=frcv(jpr_topm)%z3(:,:,:)
         botmelt(:,:,:)=frcv(jpr_botm)%z3(:,:,:)
      END SELECT

      CALL wrk_dealloc( jpi,jpj, zcptn, ztmp, zicefr )
      !
      IF( nn_timing == 1 )  CALL timing_stop('sbc_cpl_ice_flx')
      !
   END SUBROUTINE sbc_cpl_ice_flx
   
   
   SUBROUTINE sbc_cpl_snd( kt )
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE sbc_cpl_snd  ***
      !!
      !! ** Purpose :   provide the ocean-ice informations to the atmosphere
      !!
      !! ** Method  :   send to the atmosphere through a call to cpl_prism_snd
      !!              all the needed fields (as defined in sbc_cpl_init)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt
      !
      INTEGER ::   ji, jj, jl   ! dummy loop indices
      INTEGER ::   isec, info   ! local integer
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zfr_l, ztmp1, ztmp2, zotx1, zoty1, zotz1, zitx1, zity1, zitz1
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ztmp3, ztmp4   
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('sbc_cpl_snd')
      !
      CALL wrk_alloc( jpi,jpj, zfr_l, ztmp1, ztmp2, zotx1, zoty1, zotz1, zitx1, zity1, zitz1 )
      CALL wrk_alloc( jpi,jpj,jpl, ztmp3, ztmp4 )

      isec = ( kt - nit000 ) * NINT(rdttra(1))        ! date of exchanges

      zfr_l(:,:) = 1.- fr_i(:,:)

      !                                                      ! ------------------------- !
      !                                                      !    Surface temperature    !   in Kelvin
      !                                                      ! ------------------------- !
      IF( ssnd(jps_toce)%laction .OR. ssnd(jps_tice)%laction .OR. ssnd(jps_tmix)%laction ) THEN
         SELECT CASE( sn_snd_temp%cldes)
         CASE( 'oce only'             )   ;   ztmp1(:,:) =   tsn(:,:,1,jp_tem) + rt0
         CASE( 'weighted oce and ice' )   ;   ztmp1(:,:) = ( tsn(:,:,1,jp_tem) + rt0 ) * zfr_l(:,:)   
            SELECT CASE( sn_snd_temp%clcat )
            CASE( 'yes' )   
               ztmp3(:,:,1:jpl) = tn_ice(:,:,1:jpl) * a_i(:,:,1:jpl)
            CASE( 'no' )
               ztmp3(:,:,:) = 0.0
               DO jl=1,jpl
                  ztmp3(:,:,1) = ztmp3(:,:,1) + tn_ice(:,:,jl) * a_i(:,:,jl)
               ENDDO
            CASE default                  ;   CALL ctl_stop( 'sbc_cpl_snd: wrong definition of sn_snd_temp%clcat' )
            END SELECT
         CASE( 'mixed oce-ice'        )   
            ztmp1(:,:) = ( tsn(:,:,1,1) + rt0 ) * zfr_l(:,:) 
            DO jl=1,jpl
               ztmp1(:,:) = ztmp1(:,:) + tn_ice(:,:,jl) * a_i(:,:,jl)
            ENDDO
         CASE default                     ;   CALL ctl_stop( 'sbc_cpl_snd: wrong definition of sn_snd_temp%cldes' )
         END SELECT
         IF( ssnd(jps_toce)%laction )   CALL cpl_prism_snd( jps_toce, isec, RESHAPE ( ztmp1, (/jpi,jpj,1/) ), info )
         IF( ssnd(jps_tice)%laction )   CALL cpl_prism_snd( jps_tice, isec, ztmp3, info )
         IF( ssnd(jps_tmix)%laction )   CALL cpl_prism_snd( jps_tmix, isec, RESHAPE ( ztmp1, (/jpi,jpj,1/) ), info )
      ENDIF
      !
      !                                                      ! ------------------------- !
      !                                                      !           Albedo          !
      !                                                      ! ------------------------- !
      IF( ssnd(jps_albice)%laction ) THEN                         ! ice 
         ztmp3(:,:,1:jpl) = alb_ice(:,:,1:jpl) * a_i(:,:,1:jpl)
         CALL cpl_prism_snd( jps_albice, isec, ztmp3, info )
      ENDIF
      IF( ssnd(jps_albmix)%laction ) THEN                         ! mixed ice-ocean
         ztmp1(:,:) = albedo_oce_mix(:,:) * zfr_l(:,:)
         DO jl=1,jpl
            ztmp1(:,:) = ztmp1(:,:) + alb_ice(:,:,jl) * a_i(:,:,jl)
         ENDDO
         CALL cpl_prism_snd( jps_albmix, isec, RESHAPE ( ztmp1, (/jpi,jpj,1/) ), info )
      ENDIF
      !                                                      ! ------------------------- !
      !                                                      !  Ice fraction & Thickness ! 
      !                                                      ! ------------------------- !
      ! Send ice fraction field 
      IF( ssnd(jps_fice)%laction ) THEN
         SELECT CASE( sn_snd_thick%clcat )
         CASE( 'yes' )   ;   ztmp3(:,:,1:jpl) =  a_i(:,:,1:jpl)
         CASE( 'no'  )   ;   ztmp3(:,:,1    ) = fr_i(:,:      )
         CASE default    ;   CALL ctl_stop( 'sbc_cpl_snd: wrong definition of sn_snd_thick%clcat' )
         END SELECT
         CALL cpl_prism_snd( jps_fice, isec, ztmp3, info )
      ENDIF

      ! Send ice and snow thickness field 
      IF( ssnd(jps_hice)%laction .OR. ssnd(jps_hsnw)%laction ) THEN 
         SELECT CASE( sn_snd_thick%cldes)
         CASE( 'none'                  )       ! nothing to do
         CASE( 'weighted ice and snow' )   
            SELECT CASE( sn_snd_thick%clcat )
            CASE( 'yes' )   
               ztmp3(:,:,1:jpl) =  ht_i(:,:,1:jpl) * a_i(:,:,1:jpl)
               ztmp4(:,:,1:jpl) =  ht_s(:,:,1:jpl) * a_i(:,:,1:jpl)
            CASE( 'no' )
               ztmp3(:,:,:) = 0.0   ;  ztmp4(:,:,:) = 0.0
               DO jl=1,jpl
                  ztmp3(:,:,1) = ztmp3(:,:,1) + ht_i(:,:,jl) * a_i(:,:,jl)
                  ztmp4(:,:,1) = ztmp4(:,:,1) + ht_s(:,:,jl) * a_i(:,:,jl)
               ENDDO
            CASE default                  ;   CALL ctl_stop( 'sbc_cpl_snd: wrong definition of sn_snd_thick%clcat' )
            END SELECT
         CASE( 'ice and snow'         )   
            ztmp3(:,:,1:jpl) = ht_i(:,:,1:jpl)
            ztmp4(:,:,1:jpl) = ht_s(:,:,1:jpl)
         CASE default                     ;   CALL ctl_stop( 'sbc_cpl_snd: wrong definition of sn_snd_thick%cldes' )
         END SELECT
         IF( ssnd(jps_hice)%laction )   CALL cpl_prism_snd( jps_hice, isec, ztmp3, info )
         IF( ssnd(jps_hsnw)%laction )   CALL cpl_prism_snd( jps_hsnw, isec, ztmp4, info )
      ENDIF
      !
#if defined key_cpl_carbon_cycle
      !                                                      ! ------------------------- !
      !                                                      !  CO2 flux from PISCES     ! 
      !                                                      ! ------------------------- !
      IF( ssnd(jps_co2)%laction )   CALL cpl_prism_snd( jps_co2, isec, RESHAPE ( oce_co2, (/jpi,jpj,1/) ) , info )
      !
#endif
      !                                                      ! ------------------------- !
      IF( ssnd(jps_ocx1)%laction ) THEN                      !      Surface current      !
         !                                                   ! ------------------------- !
         !    
         !                                                  j+1   j     -----V---F
         ! surface velocity always sent from T point                     !       |
         !                                                        j      |   T   U
         !                                                               |       |
         !                                                   j    j-1   -I-------|
         !                                               (for I)         |       |
         !                                                              i-1  i   i
         !                                                               i      i+1 (for I)
         SELECT CASE( TRIM( sn_snd_crt%cldes ) )
         CASE( 'oce only'             )      ! C-grid ==> T
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zotx1(ji,jj) = 0.5 * ( un(ji,jj,1) + un(ji-1,jj  ,1) )
                  zoty1(ji,jj) = 0.5 * ( vn(ji,jj,1) + vn(ji  ,jj-1,1) ) 
               END DO
            END DO
         CASE( 'weighted oce and ice' )   
            SELECT CASE ( cp_ice_msh )
            CASE( 'C' )                      ! Ocean and Ice on C-grid ==> T
               DO jj = 2, jpjm1
                  DO ji = fs_2, fs_jpim1   ! vector opt.
                     zotx1(ji,jj) = 0.5 * ( un   (ji,jj,1) + un   (ji-1,jj  ,1) ) * zfr_l(ji,jj)  
                     zoty1(ji,jj) = 0.5 * ( vn   (ji,jj,1) + vn   (ji  ,jj-1,1) ) * zfr_l(ji,jj)
                     zitx1(ji,jj) = 0.5 * ( u_ice(ji,jj  ) + u_ice(ji-1,jj    ) ) *  fr_i(ji,jj)
                     zity1(ji,jj) = 0.5 * ( v_ice(ji,jj  ) + v_ice(ji  ,jj-1  ) ) *  fr_i(ji,jj)
                  END DO
               END DO
            CASE( 'I' )                      ! Ocean on C grid, Ice on I-point (B-grid) ==> T
               DO jj = 2, jpjm1
                  DO ji = 2, jpim1   ! NO vector opt.
                     zotx1(ji,jj) = 0.5  * ( un(ji,jj,1)      + un(ji-1,jj  ,1) ) * zfr_l(ji,jj)  
                     zoty1(ji,jj) = 0.5  * ( vn(ji,jj,1)      + vn(ji  ,jj-1,1) ) * zfr_l(ji,jj)  
                     zitx1(ji,jj) = 0.25 * ( u_ice(ji+1,jj+1) + u_ice(ji,jj+1)                     &
                        &                  + u_ice(ji+1,jj  ) + u_ice(ji,jj  )  ) *  fr_i(ji,jj)
                     zity1(ji,jj) = 0.25 * ( v_ice(ji+1,jj+1) + v_ice(ji,jj+1)                     &
                        &                  + v_ice(ji+1,jj  ) + v_ice(ji,jj  )  ) *  fr_i(ji,jj)
                  END DO
               END DO
            CASE( 'F' )                      ! Ocean on C grid, Ice on F-point (B-grid) ==> T
               DO jj = 2, jpjm1
                  DO ji = 2, jpim1   ! NO vector opt.
                     zotx1(ji,jj) = 0.5  * ( un(ji,jj,1)      + un(ji-1,jj  ,1) ) * zfr_l(ji,jj)  
                     zoty1(ji,jj) = 0.5  * ( vn(ji,jj,1)      + vn(ji  ,jj-1,1) ) * zfr_l(ji,jj)  
                     zitx1(ji,jj) = 0.25 * ( u_ice(ji-1,jj-1) + u_ice(ji,jj-1)                     &
                        &                  + u_ice(ji-1,jj  ) + u_ice(ji,jj  )  ) *  fr_i(ji,jj)
                     zity1(ji,jj) = 0.25 * ( v_ice(ji-1,jj-1) + v_ice(ji,jj-1)                     &
                        &                  + v_ice(ji-1,jj  ) + v_ice(ji,jj  )  ) *  fr_i(ji,jj)
                  END DO
               END DO
            END SELECT
            CALL lbc_lnk( zitx1, 'T', -1. )   ;   CALL lbc_lnk( zity1, 'T', -1. )
         CASE( 'mixed oce-ice'        )
            SELECT CASE ( cp_ice_msh )
            CASE( 'C' )                      ! Ocean and Ice on C-grid ==> T
               DO jj = 2, jpjm1
                  DO ji = fs_2, fs_jpim1   ! vector opt.
                     zotx1(ji,jj) = 0.5 * ( un   (ji,jj,1) + un   (ji-1,jj  ,1) ) * zfr_l(ji,jj)   &
                        &         + 0.5 * ( u_ice(ji,jj  ) + u_ice(ji-1,jj    ) ) *  fr_i(ji,jj)
                     zoty1(ji,jj) = 0.5 * ( vn   (ji,jj,1) + vn   (ji  ,jj-1,1) ) * zfr_l(ji,jj)   &
                        &         + 0.5 * ( v_ice(ji,jj  ) + v_ice(ji  ,jj-1  ) ) *  fr_i(ji,jj)
                  END DO
               END DO
            CASE( 'I' )                      ! Ocean on C grid, Ice on I-point (B-grid) ==> T
               DO jj = 2, jpjm1
                  DO ji = 2, jpim1   ! NO vector opt.
                     zotx1(ji,jj) = 0.5  * ( un(ji,jj,1)      + un(ji-1,jj  ,1) ) * zfr_l(ji,jj)   &   
                        &         + 0.25 * ( u_ice(ji+1,jj+1) + u_ice(ji,jj+1)                     &
                        &                  + u_ice(ji+1,jj  ) + u_ice(ji,jj  )  ) *  fr_i(ji,jj)
                     zoty1(ji,jj) = 0.5  * ( vn(ji,jj,1)      + vn(ji  ,jj-1,1) ) * zfr_l(ji,jj)   & 
                        &         + 0.25 * ( v_ice(ji+1,jj+1) + v_ice(ji,jj+1)                     &
                        &                  + v_ice(ji+1,jj  ) + v_ice(ji,jj  )  ) *  fr_i(ji,jj)
                  END DO
               END DO
            CASE( 'F' )                      ! Ocean on C grid, Ice on F-point (B-grid) ==> T
               DO jj = 2, jpjm1
                  DO ji = 2, jpim1   ! NO vector opt.
                     zotx1(ji,jj) = 0.5  * ( un(ji,jj,1)      + un(ji-1,jj  ,1) ) * zfr_l(ji,jj)   &   
                        &         + 0.25 * ( u_ice(ji-1,jj-1) + u_ice(ji,jj-1)                     &
                        &                  + u_ice(ji-1,jj  ) + u_ice(ji,jj  )  ) *  fr_i(ji,jj)
                     zoty1(ji,jj) = 0.5  * ( vn(ji,jj,1)      + vn(ji  ,jj-1,1) ) * zfr_l(ji,jj)   & 
                        &         + 0.25 * ( v_ice(ji-1,jj-1) + v_ice(ji,jj-1)                     &
                        &                  + v_ice(ji-1,jj  ) + v_ice(ji,jj  )  ) *  fr_i(ji,jj)
                  END DO
               END DO
            END SELECT
         END SELECT
         CALL lbc_lnk( zotx1, ssnd(jps_ocx1)%clgrid, -1. )   ;   CALL lbc_lnk( zoty1, ssnd(jps_ocy1)%clgrid, -1. )
         !
         !
         IF( TRIM( sn_snd_crt%clvor ) == 'eastward-northward' ) THEN             ! Rotation of the components
            !                                                                     ! Ocean component
            CALL rot_rep( zotx1, zoty1, ssnd(jps_ocx1)%clgrid, 'ij->e', ztmp1 )       ! 1st component 
            CALL rot_rep( zotx1, zoty1, ssnd(jps_ocx1)%clgrid, 'ij->n', ztmp2 )       ! 2nd component 
            zotx1(:,:) = ztmp1(:,:)                                                   ! overwrite the components 
            zoty1(:,:) = ztmp2(:,:)
            IF( ssnd(jps_ivx1)%laction ) THEN                                     ! Ice component
               CALL rot_rep( zitx1, zity1, ssnd(jps_ivx1)%clgrid, 'ij->e', ztmp1 )    ! 1st component 
               CALL rot_rep( zitx1, zity1, ssnd(jps_ivx1)%clgrid, 'ij->n', ztmp2 )    ! 2nd component 
               zitx1(:,:) = ztmp1(:,:)                                                ! overwrite the components 
               zity1(:,:) = ztmp2(:,:)
            ENDIF
         ENDIF
         !
         ! spherical coordinates to cartesian -> 2 components to 3 components
         IF( TRIM( sn_snd_crt%clvref ) == 'cartesian' ) THEN
            ztmp1(:,:) = zotx1(:,:)                     ! ocean currents
            ztmp2(:,:) = zoty1(:,:)
            CALL oce2geo ( ztmp1, ztmp2, 'T', zotx1, zoty1, zotz1 )
            !
            IF( ssnd(jps_ivx1)%laction ) THEN           ! ice velocities
               ztmp1(:,:) = zitx1(:,:)
               ztmp1(:,:) = zity1(:,:)
               CALL oce2geo ( ztmp1, ztmp2, 'T', zitx1, zity1, zitz1 )
            ENDIF
         ENDIF
         !
         IF( ssnd(jps_ocx1)%laction )   CALL cpl_prism_snd( jps_ocx1, isec, RESHAPE ( zotx1, (/jpi,jpj,1/) ), info )   ! ocean x current 1st grid
         IF( ssnd(jps_ocy1)%laction )   CALL cpl_prism_snd( jps_ocy1, isec, RESHAPE ( zoty1, (/jpi,jpj,1/) ), info )   ! ocean y current 1st grid
         IF( ssnd(jps_ocz1)%laction )   CALL cpl_prism_snd( jps_ocz1, isec, RESHAPE ( zotz1, (/jpi,jpj,1/) ), info )   ! ocean z current 1st grid
         !
         IF( ssnd(jps_ivx1)%laction )   CALL cpl_prism_snd( jps_ivx1, isec, RESHAPE ( zitx1, (/jpi,jpj,1/) ), info )   ! ice   x current 1st grid
         IF( ssnd(jps_ivy1)%laction )   CALL cpl_prism_snd( jps_ivy1, isec, RESHAPE ( zity1, (/jpi,jpj,1/) ), info )   ! ice   y current 1st grid
         IF( ssnd(jps_ivz1)%laction )   CALL cpl_prism_snd( jps_ivz1, isec, RESHAPE ( zitz1, (/jpi,jpj,1/) ), info )   ! ice   z current 1st grid
         ! 
      ENDIF
      !
      CALL wrk_dealloc( jpi,jpj, zfr_l, ztmp1, ztmp2, zotx1, zoty1, zotz1, zitx1, zity1, zitz1 )
      CALL wrk_dealloc( jpi,jpj,jpl, ztmp3, ztmp4 )
      !
      IF( nn_timing == 1 )  CALL timing_stop('sbc_cpl_snd')
      !
   END SUBROUTINE sbc_cpl_snd
   
#else
   !!----------------------------------------------------------------------
   !!   Dummy module                                            NO coupling
   !!----------------------------------------------------------------------
   USE par_kind        ! kind definition
CONTAINS
   SUBROUTINE sbc_cpl_snd( kt )
      WRITE(*,*) 'sbc_cpl_snd: You should not have seen this print! error?', kt
   END SUBROUTINE sbc_cpl_snd
   !
   SUBROUTINE sbc_cpl_rcv( kt, k_fsbc, k_ice )     
      WRITE(*,*) 'sbc_cpl_snd: You should not have seen this print! error?', kt, k_fsbc, k_ice
   END SUBROUTINE sbc_cpl_rcv
   !
   SUBROUTINE sbc_cpl_ice_tau( p_taui, p_tauj )     
      REAL(wp), INTENT(out), DIMENSION(:,:) ::   p_taui   ! i- & j-components of atmos-ice stress [N/m2]
      REAL(wp), INTENT(out), DIMENSION(:,:) ::   p_tauj   ! at I-point (B-grid) or U & V-point (C-grid)
      p_taui(:,:) = 0.   ;   p_tauj(:,:) = 0. ! stupid definition to avoid warning message when compiling...
      WRITE(*,*) 'sbc_cpl_snd: You should not have seen this print! error?'
   END SUBROUTINE sbc_cpl_ice_tau
   !
   SUBROUTINE sbc_cpl_ice_flx( p_frld , palbi   , psst    , pist  )
      REAL(wp), INTENT(in   ), DIMENSION(:,:  ) ::   p_frld     ! lead fraction                [0 to 1]
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:), OPTIONAL ::   palbi   ! ice albedo
      REAL(wp), INTENT(in   ), DIMENSION(:,:  ), OPTIONAL ::   psst    ! sea surface temperature      [Celcius]
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:), OPTIONAL ::   pist    ! ice surface temperature      [Kelvin]
      WRITE(*,*) 'sbc_cpl_snd: You should not have seen this print! error?', p_frld(1,1), palbi(1,1,1), psst(1,1), pist(1,1,1) 
   END SUBROUTINE sbc_cpl_ice_flx
   
#endif

   !!======================================================================
END MODULE sbccpl
