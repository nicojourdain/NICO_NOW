MODULE limthd_2
   !!======================================================================
   !!                  ***  MODULE limthd_2   ***
   !!              LIM thermo ice model : ice thermodynamic
   !!======================================================================
   !! History :  1.0  ! 2000-01 (LIM)
   !!            2.0  ! 2002-07 (C. Ethe, G. Madec) F90
   !!            2.0  ! 2003-08 (C. Ethe)  add lim_thd_init
   !!             -   ! 2008-2008  (A. Caubel, G. Madec, E. Maisonnave, S. Masson ) generic coupled interface
   !!---------------------------------------------------------------------
#if defined key_lim2
   !!----------------------------------------------------------------------
   !!   'key_lim2' :                                  LIM 2.0 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_thd_2      : thermodynamic of sea ice
   !!   lim_thd_init_2 : initialisation of sea-ice thermodynamic
   !!----------------------------------------------------------------------
   USE phycst          ! physical constants
   USE dom_oce         ! ocean space and time domain variables
   USE domvvl
   USE lbclnk
   USE in_out_manager  ! I/O manager
   USE lib_mpp
   USE wrk_nemo        ! work arrays
   USE iom             ! IOM library
   USE ice_2           ! LIM sea-ice variables
   USE sbc_oce         ! 
   USE sbc_ice         ! 
   USE thd_ice_2       ! LIM thermodynamic sea-ice variables
   USE dom_ice_2       ! LIM sea-ice domain
   USE limthd_zdf_2
   USE limthd_lac_2
   USE limtab_2
   USE prtctl          ! Print control
   USE cpl_oasis3, ONLY : lk_cpl
   USE diaar5, ONLY :   lk_diaar5
      
   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_thd_2  ! called by lim_step

   REAL(wp) ::   epsi20 = 1.e-20   ! constant values
   REAL(wp) ::   epsi16 = 1.e-16   !
   REAL(wp) ::   epsi04 = 1.e-04   !
   REAL(wp) ::   rzero  = 0.e0     !
   REAL(wp) ::   rone   = 1.e0     !

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!-------- -------------------------------------------------------------
   !! NEMO/LIM2 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: limthd_2.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE lim_thd_2( kt )
      !!-------------------------------------------------------------------
      !!                ***  ROUTINE lim_thd_2  ***       
      !!  
      !! ** Purpose : This routine manages the ice thermodynamic.
      !!         
      !! ** Action : - Initialisation of some variables
      !!             - Some preliminary computation (oceanic heat flux
      !!               at the ice base, snow acc.,heat budget of the leads)
      !!             - selection of the icy points and put them in an array
      !!             - call lim_vert_ther for vert ice thermodynamic
      !!             - back to the geographic grid
      !!             - selection of points for lateral accretion
      !!             - call lim_lat_acc  for the ice accretion
      !!             - back to the geographic grid
      !!
      !! References :   Goosse et al. 1996, Bul. Soc. Roy. Sc. Liege, 65, 87-90
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt     ! number of iteration
      !!
      INTEGER  ::   ji, jj               ! dummy loop indices
      INTEGER  ::   nbpb                 ! nb of icy pts for thermo. cal.
      INTEGER  ::   nbpac                ! nb of pts for lateral accretion 
      CHARACTER (len=22) :: charout
      REAL(wp) ::   zfric_umin = 5e-03   ! lower bound for the friction velocity
      REAL(wp) ::   zfric_umax = 2e-02   ! upper bound for the friction velocity
      REAL(wp) ::   zinda                ! switch for test. the val. of concen.
      REAL(wp) ::   zindb, zindg         ! switches for test. the val of arg
      REAL(wp) ::   zfricp               ! temporary scalar
      REAL(wp) ::   za , zh, zthsnice    !
      REAL(wp) ::   zfric_u              ! friction velocity 
      REAL(wp) ::   zfnsol               ! total non solar heat
      REAL(wp) ::   zfontn               ! heat flux from snow thickness
      REAL(wp) ::   zfntlat, zpareff     ! test. the val. of lead heat budget

      REAL(wp) ::   zuice_m, zvice_m     ! Sea-ice velocities at U & V-points
      REAL(wp) ::   zhice_u, zhice_v     ! Sea-ice volume at U & V-points
      REAL(wp) ::   ztr_fram             ! Sea-ice transport through Fram strait
      REAL(wp) ::   zrhoij, zrhoijm1     ! temporary scalars
      REAL(wp) ::   zztmp                ! temporary scalars within a loop
      REAL(wp), POINTER, DIMENSION(:,:)     ::   ztmp      ! 2D workspace
      REAL(wp), POINTER, DIMENSION(:,:)     ::   zqlbsbq   ! link with lead energy budget qldif
      REAL(wp), POINTER, DIMENSION(:,:)     ::   zlicegr   ! link with lateral ice growth 
!!$      REAL(wp), DIMENSION(:,:) ::   firic         ! IR flux over the ice            (outputs only)
!!$      REAL(wp), DIMENSION(:,:) ::   fcsic         ! Sensible heat flux over the ice (outputs only)
!!$      REAL(wp), DIMENSION(:,:) ::   fleic         ! Latent heat flux over the ice   (outputs only)
!!$      REAL(wp), DIMENSION(:,:) ::   qlatic        ! latent flux                     (outputs only)
      REAL(wp), POINTER, DIMENSION(:,:) ::   zdvosif       ! Variation of volume at surface                (outputs only)
      REAL(wp), POINTER, DIMENSION(:,:) ::   zdvobif       ! Variation of ice volume at the bottom ice     (outputs only)
      REAL(wp), POINTER, DIMENSION(:,:) ::   zdvolif       ! Total variation of ice volume                 (outputs only)
      REAL(wp), POINTER, DIMENSION(:,:) ::   zdvonif       ! Surface accretion Snow to Ice transformation  (outputs only)
      REAL(wp), POINTER, DIMENSION(:,:) ::   zdvomif       ! Bottom variation of ice volume due to melting (outputs only)
      REAL(wp), POINTER, DIMENSION(:,:) ::   zu_imasstr    ! Sea-ice transport along i-axis at U-point     (outputs only) 
      REAL(wp), POINTER, DIMENSION(:,:) ::   zv_imasstr    ! Sea-ice transport along j-axis at V-point     (outputs only) 
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zmsk        ! 3D workspace
      !!-------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, ztmp, zqlbsbq, zlicegr, zdvosif, zdvobif, zdvolif, zdvonif, zdvomif, zu_imasstr, zv_imasstr )
      CALL wrk_alloc( jpi, jpj, jpk, zmsk )

      IF( kt == nit000 )   CALL lim_thd_init_2  ! Initialization (first time-step only)
   
      !-------------------------------------------!
      !   Initilization of diagnostic variables   !
      !-------------------------------------------!
      
!!gm needed?  yes at least for some of these arrays 
      zdvosif(:,:) = 0.e0   ! variation of ice volume at surface
      zdvobif(:,:) = 0.e0   ! variation of ice volume at bottom
      zdvolif(:,:) = 0.e0   ! total variation of ice volume
      zdvonif(:,:) = 0.e0   ! transformation of snow to sea-ice volume
!      zdvonif(:,:) = 0.e0   ! lateral variation of ice volume
      zlicegr(:,:) = 0.e0   ! lateral variation of ice volume
      zdvomif(:,:) = 0.e0   ! variation of ice volume at bottom due to melting only
      ztr_fram     = 0.e0   ! sea-ice transport through Fram strait
      fstric (:,:) = 0.e0   ! part of solar radiation absorbing inside the ice
      fscmbq (:,:) = 0.e0   ! linked with fstric
      ffltbif(:,:) = 0.e0   ! linked with fstric
      qfvbq  (:,:) = 0.e0   ! linked with fstric
      rdmsnif(:,:) = 0.e0   ! variation of snow mass per unit area
      rdmicif(:,:) = 0.e0   ! variation of ice mass per unit area
      zmsk (:,:,:) = 0.e0

      ! set to zero snow thickness smaller than epsi04
      DO jj = 1, jpj
         DO ji = 1, jpi
            hsnif(ji,jj)  = hsnif(ji,jj) *  MAX( rzero, SIGN( rone , hsnif(ji,jj) - epsi04 ) )
         END DO
      END DO
!!gm better coded (do not use SIGN...)
!     WHERE( hsnif(:,:) < epsi04 )   hsnif(:,:) = 0.e0
!!gm

      IF(ln_ctl)   CALL prt_ctl( tab2d_1=hsnif, clinfo1=' lim_thd: hsnif   : ' )
      
      !-----------------------------------!
      !   Treatment of particular cases   !
      !-----------------------------------!
      
      DO jj = 1, jpj
         DO ji = 1, jpi
            !  snow is transformed into ice if the original ice cover disappears.
            zindg         = tms(ji,jj) *  MAX( rzero , SIGN( rone , -hicif(ji,jj) ) )
            hicif(ji,jj)  = hicif(ji,jj) + zindg * rhosn * hsnif(ji,jj) / rau0
            hsnif(ji,jj)  = ( rone - zindg ) * hsnif(ji,jj) + zindg * hicif(ji,jj) * ( rau0 - rhoic ) / rhosn
            dmgwi(ji,jj)  = zindg * (1.0 - frld(ji,jj)) * rhoic * hicif(ji,jj)   ! snow/ice mass
            
            !  the lead fraction, frld, must be little than or equal to amax (ice ridging).
            zthsnice      = hsnif(ji,jj) + hicif(ji,jj)
            zindb         = tms(ji,jj) * ( 1.0 - MAX( rzero , SIGN( rone , - zthsnice ) ) ) 
            za            = zindb * MIN( rone, ( 1.0 - frld(ji,jj) ) * uscomi )
            hsnif (ji,jj) = hsnif(ji,jj)  * za
            hicif (ji,jj) = hicif(ji,jj)  * za
            qstoif(ji,jj) = qstoif(ji,jj) * za
            frld  (ji,jj) = 1.0 - zindb * ( 1.0 - frld(ji,jj) ) / MAX( za, epsi20 )
            
            !  the in situ ice thickness, hicif, must be equal to or greater than hiclim.
            zh            = MAX( rone , zindb * hiclim  / MAX( hicif(ji,jj), epsi20 ) )
            hsnif (ji,jj) = hsnif(ji,jj)  * zh
            hicif (ji,jj) = hicif(ji,jj)  * zh
            qstoif(ji,jj) = qstoif(ji,jj) * zh
            frld  (ji,jj) = ( frld(ji,jj) + ( zh - 1.0 ) ) / zh
         END DO
      END DO

      IF(ln_ctl) THEN
         CALL prt_ctl( tab2d_1=hicif , clinfo1=' lim_thd: hicif   : ' )
         CALL prt_ctl( tab2d_1=hsnif , clinfo1=' lim_thd: hsnif   : ' )
         CALL prt_ctl( tab2d_1=dmgwi , clinfo1=' lim_thd: dmgwi   : ' )
         CALL prt_ctl( tab2d_1=qstoif, clinfo1=' lim_thd: qstoif  : ' )
         CALL prt_ctl( tab2d_1=frld  , clinfo1=' lim_thd: frld    : ' )
      ENDIF

      
      !-------------------------------!
      !   Thermodynamics of sea ice   !
      !-------------------------------!
      
      !      Partial computation of forcing for the thermodynamic sea ice model.
      !--------------------------------------------------------------------------

      sst_m(:,:) = sst_m(:,:) + rt0

!CDIR NOVERRCHK
      DO jj = 1, jpj
!CDIR NOVERRCHK
         DO ji = 1, jpi
            zthsnice       = hsnif(ji,jj) + hicif(ji,jj)
            zindb          = tms(ji,jj) * ( 1.0 - MAX( rzero , SIGN( rone , - zthsnice ) ) ) 
            pfrld(ji,jj)   = frld(ji,jj)
            zfricp         = 1.0 - frld(ji,jj)
            zinda          = 1.0 - MAX( rzero , SIGN( rone , - zfricp ) )
            
            !  solar irradiance transmission at the mixed layer bottom and used in the lead heat budget
            thcm(ji,jj)    = 0.e0 
            
            !  net downward heat flux from the ice to the ocean, expressed as a function of ocean 
            !  temperature and turbulent mixing (McPhee, 1992)
            zfric_u        = MAX ( MIN( SQRT( ust2s(ji,jj) ) , zfric_umax ) , zfric_umin )  ! friction velocity
            fdtcn(ji,jj)  = zindb * rau0 * rcp * 0.006  * zfric_u * ( sst_m(ji,jj) - tfu(ji,jj) ) 
            qdtcn(ji,jj)  = zindb * fdtcn(ji,jj) * frld(ji,jj) * rdt_ice
                        
            !  partial computation of the lead energy budget (qldif)
#if defined key_coupled 
            qldif(ji,jj)   = tms(ji,jj) * rdt_ice                                             &
               &    * (   ( qsr_tot(ji,jj) - qsr_ice(ji,jj,1) * zfricp ) * ( 1.0 - thcm(ji,jj) )   &
               &        + ( qns_tot(ji,jj) - qns_ice(ji,jj,1) * zfricp )                           &
               &        + frld(ji,jj) * ( fdtcn(ji,jj) + ( 1.0 - zindb ) * fsbbq(ji,jj) )   )
#else
            zfontn         = ( sprecip(ji,jj) / rhosn ) * xlsn  !   energy for melting solid precipitation
            zfnsol         = qns(ji,jj)                         !  total non solar flux over the ocean
            qldif(ji,jj)   = tms(ji,jj) * ( qsr(ji,jj) * ( 1.0 - thcm(ji,jj) )   &
               &                               + zfnsol + fdtcn(ji,jj) - zfontn     &
               &                               + ( 1.0 - zindb ) * fsbbq(ji,jj) )   &
               &                        * frld(ji,jj) * rdt_ice    
!!$            qldif(ji,jj)   = tms(ji,jj) * rdt_ice * frld(ji,jj) 
!!$               &           * ( qsr(ji,jj) * ( 1.0 - thcm(ji,jj) )      &
!!$               &             + qns(ji,jj)  + fdtcn(ji,jj) - zfontn     &
!!$               &             + ( 1.0 - zindb ) * fsbbq(ji,jj)      )   &
#endif
            !  parlat : percentage of energy used for lateral ablation (0.0) 
            zfntlat        = 1.0 - MAX( rzero , SIGN( rone ,  - qldif(ji,jj) ) )
            zpareff        = 1.0 + ( parlat - 1.0 ) * zinda * zfntlat
            zqlbsbq(ji,jj) = qldif(ji,jj) * ( 1.0 - zpareff ) / MAX( (1.0 - frld(ji,jj)) * rdt_ice , epsi16 )
            qldif  (ji,jj) = zpareff *  qldif(ji,jj)
            qdtcn  (ji,jj) = zpareff * qdtcn(ji,jj)
            
            !  energy needed to bring ocean surface layer until its freezing
            qcmif  (ji,jj) =  rau0 * rcp * fse3t_m(ji,jj,1)   &
                &          * ( tfu(ji,jj) - sst_m(ji,jj) ) * ( 1 - zinda )
            
            !  calculate oceanic heat flux.
            fbif   (ji,jj) = zindb * (  fsbbq(ji,jj) / MAX( (1.0 - frld(ji,jj)) , epsi20 ) + fdtcn(ji,jj) )
            
            ! computation of the thermodynamic ice production (only needed for output)
            hicifp(ji,jj) = hicif(ji,jj) * ( 1.0 - frld(ji,jj) )
         END DO
      END DO
      
      sst_m(:,:) = sst_m(:,:) - rt0
               
      !         Select icy points and fulfill arrays for the vectorial grid.
      !----------------------------------------------------------------------
      nbpb = 0
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF ( frld(ji,jj) < 1.0 ) THEN     
               nbpb      = nbpb + 1
               npb(nbpb) = (jj - 1) * jpi + ji
            ENDIF
         END DO
      END DO

      IF(ln_ctl) THEN
         CALL prt_ctl(tab2d_1=pfrld, clinfo1=' lim_thd: pfrld   : ', tab2d_2=thcm   , clinfo2='  thcm    : ')
         CALL prt_ctl(tab2d_1=fdtcn, clinfo1=' lim_thd: fdtcn   : ', tab2d_2=qdtcn  , clinfo2='  qdtcn   : ')
         CALL prt_ctl(tab2d_1=qldif, clinfo1=' lim_thd: qldif   : ', tab2d_2=zqlbsbq, clinfo2='  zqlbsbq : ')
         CALL prt_ctl(tab2d_1=qcmif, clinfo1=' lim_thd: qcmif   : ', tab2d_2=fbif   , clinfo2='  fbif    : ')
         zmsk(:,:,1) = tms(:,:)
         CALL prt_ctl(tab2d_1=qcmif , clinfo1=' lim_thd: qcmif  : ', mask1=zmsk)
         CALL prt_ctl(tab2d_1=hicifp, clinfo1=' lim_thd: hicifp : ')
         WRITE(charout, FMT="('lim_thd: nbpb = ',I4)") nbpb
         CALL prt_ctl_info(charout)
      ENDIF
      
      
      ! If there is no ice, do nothing. Otherwise, compute Top and Bottom accretion/ablation 
      !------------------------------------------------------------------------------------ 

      IF( nbpb > 0 ) THEN
         !    
         !  put the variable in a 1-D array for thermodynamics process
         CALL tab_2d_1d_2( nbpb, frld_1d    (1:nbpb)     , frld           , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, h_ice_1d   (1:nbpb)     , hicif          , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, h_snow_1d  (1:nbpb)     , hsnif          , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, sist_1d    (1:nbpb)     , sist           , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, tbif_1d    (1:nbpb , 1 ), tbif(:,:,1)    , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, tbif_1d    (1:nbpb , 2 ), tbif(:,:,2)    , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, tbif_1d    (1:nbpb , 3 ), tbif(:,:,3)    , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, qsr_ice_1d (1:nbpb)     , qsr_ice(:,:,1) , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, fr1_i0_1d  (1:nbpb)     , fr1_i0         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, fr2_i0_1d  (1:nbpb)     , fr2_i0         , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb,  qns_ice_1d(1:nbpb)     ,  qns_ice(:,:,1), jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, dqns_ice_1d(1:nbpb)     , dqns_ice(:,:,1), jpi, jpj, npb(1:nbpb) )
         IF( .NOT. lk_cpl ) THEN 
            CALL tab_2d_1d_2( nbpb, qla_ice_1d (1:nbpb)     ,  qla_ice(:,:,1), jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d_2( nbpb, dqla_ice_1d(1:nbpb)     , dqla_ice(:,:,1), jpi, jpj, npb(1:nbpb) )
         ENDIF
         CALL tab_2d_1d_2( nbpb, tfu_1d     (1:nbpb)     , tfu        , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, sprecip_1d (1:nbpb)     , sprecip    , jpi, jpj, npb(1:nbpb) ) 
         CALL tab_2d_1d_2( nbpb, fbif_1d    (1:nbpb)     , fbif       , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, thcm_1d    (1:nbpb)     , thcm       , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, qldif_1d   (1:nbpb)     , qldif      , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, qstbif_1d  (1:nbpb)     , qstoif     , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, rdmicif_1d (1:nbpb)     , rdmicif    , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, dmgwi_1d   (1:nbpb)     , dmgwi      , jpi, jpj, npb(1:nbpb) )
         CALL tab_2d_1d_2( nbpb, qlbbq_1d   (1:nbpb)     , zqlbsbq    , jpi, jpj, npb(1:nbpb) )
         !
         CALL lim_thd_zdf_2( 1, nbpb )       !  compute ice growth
         !
         !  back to the geographic grid.
         CALL tab_1d_2d_2( nbpb, frld       , npb, frld_1d   (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, hicif      , npb, h_ice_1d  (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, hsnif      , npb, h_snow_1d (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, sist       , npb, sist_1d   (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, tbif(:,:,1), npb, tbif_1d   (1:nbpb , 1 ), jpi, jpj )   
         CALL tab_1d_2d_2( nbpb, tbif(:,:,2), npb, tbif_1d   (1:nbpb , 2 ), jpi, jpj )   
         CALL tab_1d_2d_2( nbpb, tbif(:,:,3), npb, tbif_1d   (1:nbpb , 3 ), jpi, jpj )   
         CALL tab_1d_2d_2( nbpb, fscmbq     , npb, fscbq_1d  (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, ffltbif    , npb, fltbif_1d (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, fstric     , npb, fstbif_1d (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, qldif      , npb, qldif_1d  (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, qfvbq      , npb, qfvbq_1d  (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, qstoif     , npb, qstbif_1d (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, rdmicif    , npb, rdmicif_1d(1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, dmgwi      , npb, dmgwi_1d  (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, rdmsnif    , npb, rdmsnif_1d(1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, zdvosif    , npb, dvsbq_1d  (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, zdvobif    , npb, dvbbq_1d  (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, zdvomif    , npb, rdvomif_1d(1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, zdvolif    , npb, dvlbq_1d  (1:nbpb)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, zdvonif    , npb, dvnbq_1d  (1:nbpb)     , jpi, jpj ) 
         CALL tab_1d_2d_2( nbpb, qsr_ice(:,:,1), npb, qsr_ice_1d(1:nbpb)  , jpi, jpj )
         CALL tab_1d_2d_2( nbpb, qns_ice(:,:,1), npb, qns_ice_1d(1:nbpb)  , jpi, jpj )
         IF( .NOT. lk_cpl )   CALL tab_1d_2d_2( nbpb, qla_ice(:,:,1), npb, qla_ice_1d(1:nbpb)  , jpi, jpj )
         !
      ENDIF

      ! Up-date sea ice thickness
      !--------------------------
      DO jj = 1, jpj
         DO ji = 1, jpi
            phicif(ji,jj) = hicif(ji,jj)  
            hicif(ji,jj)  = hicif(ji,jj) *  ( rone -  MAX( rzero, SIGN( rone, - ( 1.0 - frld(ji,jj) ) ) ) )
         END DO
      END DO

      
      ! Tricky trick : add 2 to frld in the Southern Hemisphere
      !--------------------------------------------------------
      IF( fcor(1,1) < 0.e0 ) THEN
         DO jj = 1, njeqm1
            DO ji = 1, jpi
               frld(ji,jj) = frld(ji,jj) + 2.0
            END DO
         END DO
      ENDIF

      CALL lbc_lnk( frld , 'T', 1. )      
      
      ! Select points for lateral accretion (this occurs when heat exchange
      ! between ice and ocean is negative; ocean losing heat) 
      !-----------------------------------------------------------------
      nbpac = 0
      DO jj = 1, jpj
         DO ji = 1, jpi
!i yes!     IF ( ( qcmif(ji,jj) - qldif(ji,jj) ) > 0.e0 ) THEN
            IF ( tms(ji,jj) * ( qcmif(ji,jj) - qldif(ji,jj) ) > 0.e0 ) THEN
               nbpac = nbpac + 1
               npac( nbpac ) = (jj - 1) * jpi + ji
            ENDIF
         END DO
      END DO
      
      IF(ln_ctl) THEN
         CALL prt_ctl(tab2d_1=phicif, clinfo1=' lim_thd: phicif  : ', tab2d_2=hicif, clinfo2=' hicif : ')
         WRITE(charout, FMT="('lim_thd: nbpac = ',I4)") nbpac
         CALL prt_ctl_info(charout)
      ENDIF


      ! If ocean gains heat do nothing ; otherwise, one performs lateral accretion
      !--------------------------------------------------------------------------------
      IF( nbpac > 0 ) THEN
         !
         zlicegr(:,:) = rdmicif(:,:)		! to output the lateral sea-ice growth 
         !...Put the variable in a 1-D array for lateral accretion
         CALL tab_2d_1d_2( nbpac, frld_1d   (1:nbpac)     , frld       , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d_2( nbpac, h_snow_1d (1:nbpac)     , hsnif      , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d_2( nbpac, h_ice_1d  (1:nbpac)     , hicif      , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d_2( nbpac, tbif_1d   (1:nbpac , 1 ), tbif(:,:,1), jpi, jpj, npac(1:nbpac) )   
         CALL tab_2d_1d_2( nbpac, tbif_1d   (1:nbpac , 2 ), tbif(:,:,2), jpi, jpj, npac(1:nbpac) )   
         CALL tab_2d_1d_2( nbpac, tbif_1d   (1:nbpac , 3 ), tbif(:,:,3), jpi, jpj, npac(1:nbpac) )   
         CALL tab_2d_1d_2( nbpac, qldif_1d  (1:nbpac)     , qldif      , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d_2( nbpac, qcmif_1d  (1:nbpac)     , qcmif      , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d_2( nbpac, qstbif_1d (1:nbpac)     , qstoif     , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d_2( nbpac, rdmicif_1d(1:nbpac)     , rdmicif    , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d_2( nbpac, dvlbq_1d  (1:nbpac)     , zdvolif    , jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d_2( nbpac, tfu_1d    (1:nbpac)     , tfu        , jpi, jpj, npac(1:nbpac) )
         !
         CALL lim_thd_lac_2( 1 , nbpac )         ! lateral accretion routine.
         !
         !   back to the geographic grid
         CALL tab_1d_2d_2( nbpac, frld       , npac(1:nbpac), frld_1d   (1:nbpac)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpac, hsnif      , npac(1:nbpac), h_snow_1d (1:nbpac)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpac, hicif      , npac(1:nbpac), h_ice_1d  (1:nbpac)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpac, tbif(:,:,1), npac(1:nbpac), tbif_1d   (1:nbpac , 1 ), jpi, jpj )
         CALL tab_1d_2d_2( nbpac, tbif(:,:,2), npac(1:nbpac), tbif_1d   (1:nbpac , 2 ), jpi, jpj )
         CALL tab_1d_2d_2( nbpac, tbif(:,:,3), npac(1:nbpac), tbif_1d   (1:nbpac , 3 ), jpi, jpj )
         CALL tab_1d_2d_2( nbpac, qstoif     , npac(1:nbpac), qstbif_1d (1:nbpac)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpac, rdmicif    , npac(1:nbpac), rdmicif_1d(1:nbpac)     , jpi, jpj )
         CALL tab_1d_2d_2( nbpac, zdvolif    , npac(1:nbpac), dvlbq_1d  (1:nbpac)     , jpi, jpj )
         !
      ENDIF
       
       
      ! Recover frld values between 0 and 1 in the Southern Hemisphere (tricky trick)
      ! Update daily thermodynamic ice production.    
      !------------------------------------------------------------------------------
      DO jj = 1, jpj
         DO ji = 1, jpi
            frld  (ji,jj) = MIN( frld(ji,jj), ABS( frld(ji,jj) - 2.0 ) )
            fr_i  (ji,jj) = 1.0 - frld(ji,jj)  
            hicifp(ji,jj) = hicif(ji,jj) * fr_i(ji,jj) - hicifp(ji,jj)
         END DO
      END DO

      ! Outputs
      !--------------------------------------------------------------------------------
      ztmp(:,:) = 1. - pfrld(:,:)                                ! fraction of ice after the dynamic, before the thermodynamic
      CALL iom_put( 'ioceflxb', fbif )                           ! Oceanic flux at the ice base           [W/m2 ???]
      CALL iom_put( 'ist_cea', (sist(:,:) - rt0) * ztmp(:,:) )   ! Ice surface temperature                [Celius]
      CALL iom_put( 'qsr_ai_cea', qsr_ice(:,:,1) * ztmp(:,:) )   ! Solar flux over the ice                [W/m2]
      CALL iom_put( 'qns_ai_cea', qns_ice(:,:,1) * ztmp(:,:) )   ! Non-solar flux over the ice            [W/m2]
      IF( .NOT. lk_cpl )   CALL iom_put( 'qla_ai_cea', qla_ice(:,:,1) * ztmp(:,:) )     ! Latent flux over the ice  [W/m2]
      !
      CALL iom_put( 'snowthic_cea', hsnif  (:,:) * fr_i(:,:) )   ! Snow thickness             [m]
      CALL iom_put( 'icethic_cea' , hicif  (:,:) * fr_i(:,:) )   ! Ice thickness              [m]
      zztmp = 1.0 / rdt_ice
      CALL iom_put( 'iceprod_cea' , hicifp (:,:) * zztmp     )   ! Ice produced               [m/s]
      IF( lk_diaar5 ) THEN
         CALL iom_put( 'snowmel_cea' , rdmsnif(:,:) * zztmp     )   ! Snow melt                  [kg/m2/s]
         zztmp = rhoic / rdt_ice
         CALL iom_put( 'sntoice_cea' , zdvonif(:,:) * zztmp     )   ! Snow to Ice transformation [kg/m2/s]
         CALL iom_put( 'ticemel_cea' , zdvosif(:,:) * zztmp     )   ! Melt at Sea Ice top        [kg/m2/s]
         CALL iom_put( 'bicemel_cea' , zdvomif(:,:) * zztmp     )   ! Melt at Sea Ice bottom     [kg/m2/s]
         zlicegr(:,:) = MAX( 0.e0, rdmicif(:,:)-zlicegr(:,:) )
         CALL iom_put( 'licepro_cea' , zlicegr(:,:) * zztmp     )   ! Latereal sea ice growth    [kg/m2/s]
      ENDIF
      !
      ! Compute the Eastward & Northward sea-ice transport
      zztmp = 0.25 * rhoic
      DO jj = 1, jpjm1 
         DO ji = 1, jpim1   ! NO vector opt.
            ! Ice velocities, volume & transport at U & V-points
            zuice_m = u_ice(ji+1,jj+1) + u_ice(ji+1,jj )
            zvice_m = v_ice(ji+1,jj+1) + v_ice(ji ,jj+1)
            zhice_u = hicif(ji,jj)*e2t(ji,jj)*fr_i(ji,jj) + hicif(ji+1,jj  )*e2t(ji+1,jj  )*fr_i(ji+1,jj  )
            zhice_v = hicif(ji,jj)*e1t(ji,jj)*fr_i(ji,jj) + hicif(ji  ,jj+1)*e1t(ji  ,jj+1)*fr_i(ji  ,jj+1)
            zu_imasstr(ji,jj) = zztmp * zhice_u * zuice_m 
            zv_imasstr(ji,jj) = zztmp * zhice_v * zvice_m 
         END DO
      END DO
      CALL lbc_lnk( zu_imasstr, 'U', -1. )   ;   CALL lbc_lnk( zv_imasstr, 'V', -1. )
      CALL iom_put( 'u_imasstr',  zu_imasstr(:,:) )   ! Ice transport along i-axis at U-point [kg/s] 
      CALL iom_put( 'v_imasstr',  zv_imasstr(:,:) )   ! Ice transport along j-axis at V-point [kg/s] 

      !! Fram Strait sea-ice transport (sea-ice + snow)  (in ORCA2 = 5 points)
      IF( cp_cfg == "orca" .AND. jp_cfg == 2 ) THEN    ! ORCA R2 configuration
         DO jj = mj0(137), mj1(137) ! B grid
            IF( mj0(jj-1) >= nldj ) THEN
               DO ji = MAX(mi0(134),nldi), MIN(mi1(138),nlei)
                  zrhoij    = e1t(ji,jj  ) * fr_i(ji,jj  ) * ( rhoic*hicif(ji,jj  ) + rhosn*hsnif(ji,jj  ) ) 
                  zrhoijm1  = e1t(ji,jj-1) * fr_i(ji,jj-1) * ( rhoic*hicif(ji,jj-1) + rhosn*hsnif(ji,jj-1) ) 
                  ztr_fram  = ztr_fram - 0.25 * ( v_ice(ji,jj)+ v_ice(ji+1,jj) ) * ( zrhoij + zrhoijm1 )
               END DO
            ENDIF
         END DO
         IF( lk_mpp )   CALL mpp_sum( ztr_fram )
         CALL iom_put( 'fram_trans', ztr_fram )   ! Ice transport through Fram strait     [kg/s] 
      ENDIF

!! ce     ztmp(:,:) = 1. - AINT( frld(:,:), wp )                        ! return 1 as soon as there is ice
!! ce     A big warning because the model crashes on IDRIS/IBM SP6 with xlf 13.1.0.3, see ticket #761
!! ce     We Unroll the loop and everything works fine 
      DO jj = 1, jpj
         DO ji = 1, jpi
            ztmp(ji,jj) = 1. - AINT( frld(ji,jj), wp )                ! return 1 as soon as there is ice
         END DO
      END DO
      !
      CALL iom_put( 'ice_pres'  , ztmp                            )   ! Ice presence                          [-]
      CALL iom_put( 'ist_ipa'   , ( sist(:,:) - rt0 ) * ztmp(:,:) )   ! Ice surface temperature               [Celius]
      CALL iom_put( 'uice_ipa'  ,  u_ice(:,:)         * ztmp(:,:) )   ! Ice velocity along i-axis at I-point  [m/s] 
      CALL iom_put( 'vice_ipa'  ,  v_ice(:,:)         * ztmp(:,:) )   ! Ice velocity along j-axis at I-point  [m/s]

      IF(ln_ctl) THEN
         CALL prt_ctl_info(' lim_thd  end  ')
         CALL prt_ctl( tab2d_1=hicif      , clinfo1=' lim_thd: hicif   : ', tab2d_2=hsnif , clinfo2=' hsnif  : ' )
         CALL prt_ctl( tab2d_1=frld       , clinfo1=' lim_thd: frld    : ', tab2d_2=hicifp, clinfo2=' hicifp : ' )
         CALL prt_ctl( tab2d_1=phicif     , clinfo1=' lim_thd: phicif  : ', tab2d_2=pfrld , clinfo2=' pfrld  : ' )
         CALL prt_ctl( tab2d_1=sist       , clinfo1=' lim_thd: sist    : ' )
         CALL prt_ctl( tab2d_1=tbif(:,:,1), clinfo1=' lim_thd: tbif 1  : ' )
         CALL prt_ctl( tab2d_1=tbif(:,:,2), clinfo1=' lim_thd: tbif 2  : ' )
         CALL prt_ctl( tab2d_1=tbif(:,:,3), clinfo1=' lim_thd: tbif 3  : ' )
         CALL prt_ctl( tab2d_1=fdtcn      , clinfo1=' lim_thd: fdtcn   : ', tab2d_2=qdtcn , clinfo2=' qdtcn  : ' )
         CALL prt_ctl( tab2d_1=qstoif     , clinfo1=' lim_thd: qstoif  : ', tab2d_2=fsbbq , clinfo2=' fsbbq  : ' )
      ENDIF
       !
      CALL wrk_dealloc( jpi, jpj, ztmp, zqlbsbq, zlicegr, zdvosif, zdvobif, zdvolif, zdvonif, zdvomif, zu_imasstr, zv_imasstr )
      CALL wrk_dealloc( jpi, jpj, jpk, zmsk )
      !
    END SUBROUTINE lim_thd_2


    SUBROUTINE lim_thd_init_2
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE lim_thd_init_2 *** 
      !!                 
      !! ** Purpose :   Physical constants and parameters linked to the ice 
      !!      thermodynamics
      !!
      !! ** Method  :   Read the namicethd namelist and check the ice-thermo
      !!       parameter values called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namicether
      !!-------------------------------------------------------------------
      NAMELIST/namicethd/ hmelt , hiccrit, hicmin, hiclim, amax  ,        &
         &                swiqst, sbeta  , parlat, hakspl, hibspl, exld,  &
         &                hakdif, hnzst  , thth  , parsub, alphs
      !!-------------------------------------------------------------------
      !
      REWIND( numnam_ice )                  ! read namelist
      READ  ( numnam_ice , namicethd )
      IF( lk_cpl .AND. parsub /= 0.0 )   CALL ctl_stop( 'In coupled mode, use parsub = 0. or send dqla' )
      !
      IF(lwp) THEN                          ! control print
         WRITE(numout,*)
         WRITE(numout,*)'lim_thd_init_2: ice parameters for ice thermodynamic computation '
         WRITE(numout,*)'~~~~~~~~~~~~~~'
         WRITE(numout,*)'       maximum melting at the bottom                           hmelt        = ', hmelt
         WRITE(numout,*)'       ice thick. for lateral accretion in NH (SH)             hiccrit(1/2) = ', hiccrit
         WRITE(numout,*)'       ice thick. corr. to max. energy stored in brine pocket  hicmin       = ', hicmin  
         WRITE(numout,*)'       minimum ice thickness                                   hiclim       = ', hiclim 
         WRITE(numout,*)'       maximum lead fraction                                   amax         = ', amax 
         WRITE(numout,*)'       energy stored in brine pocket (=1) or not (=0)          swiqst       = ', swiqst 
         WRITE(numout,*)'       numerical carac. of the scheme for diffusion in ice '
         WRITE(numout,*)'       Cranck-Nicholson (=0.5), implicit (=1), explicit (=0)   sbeta        = ', sbeta
         WRITE(numout,*)'       percentage of energy used for lateral ablation          parlat       = ', parlat
         WRITE(numout,*)'       slope of distr. for Hakkinen-Mellor lateral melting     hakspl       = ', hakspl  
         WRITE(numout,*)'       slope of distribution for Hibler lateral melting        hibspl       = ', hibspl
         WRITE(numout,*)'       exponent for leads-closure rate                         exld         = ', exld
         WRITE(numout,*)'       coefficient for diffusions of ice and snow              hakdif       = ', hakdif
         WRITE(numout,*)'       threshold thick. for comp. of eq. thermal conductivity  zhth         = ', thth 
         WRITE(numout,*)'       thickness of the surf. layer in temp. computation       hnzst        = ', hnzst
         WRITE(numout,*)'       switch for snow sublimation  (=1) or not (=0)           parsub       = ', parsub  
         WRITE(numout,*)'       coefficient for snow density when snow ice formation    alphs        = ', alphs
      ENDIF
      !          
      uscomi = 1.0 / ( 1.0 - amax )   ! inverse of minimum lead fraction
      rcdsn = hakdif * rcdsn 
      rcdic = hakdif * rcdic
      !
      IF( hsndif > 100.e0 .OR. hicdif > 100.e0 ) THEN
         cnscg = 0.e0
      ELSE
         cnscg = rcpsn / rcpic   ! ratio  rcpsn/rcpic
      ENDIF
      !
   END SUBROUTINE lim_thd_init_2

#else
   !!----------------------------------------------------------------------
   !!   Default option          Dummy module       NO LIM 2.0 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_thd_2         ! Dummy routine
   END SUBROUTINE lim_thd_2
#endif

   !!======================================================================
END MODULE limthd_2
