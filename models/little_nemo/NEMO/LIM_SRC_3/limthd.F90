MODULE limthd
   !!======================================================================
   !!                  ***  MODULE limthd   ***
   !!  LIM-3 :   ice thermodynamic
   !!======================================================================
   !! History :  LIM  ! 2000-01 (M.A. Morales Maqueda, H. Goosse, T. Fichefet) LIM-1
   !!            2.0  ! 2002-07 (C. Ethe, G. Madec)  LIM-2 (F90 rewriting)
   !!            3.0  ! 2005-11 (M. Vancoppenolle)  LIM-3 : Multi-layer thermodynamics + salinity variations
   !!             -   ! 2007-04 (M. Vancoppenolle) add lim_thd_glohec, lim_thd_con_dh and lim_thd_con_dif
   !!            3.2  ! 2009-07 (M. Vancoppenolle, Y. Aksenov, G. Madec) bug correction in rdmsnif
   !!            3.3  ! 2010-11 (G. Madec) corrected snow melting heat (due to factor betas)
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_thd        : thermodynamic of sea ice
   !!   lim_thd_init   : initialisation of sea-ice thermodynamic
   !!----------------------------------------------------------------------
   USE phycst          ! physical constants
   USE dom_oce         ! ocean space and time domain variables
   USE ice             ! LIM: sea-ice variables
   USE par_ice         ! LIM: sea-ice parameters
   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE sbc_ice         ! Surface boundary condition: ice fields
   USE thd_ice         ! LIM thermodynamic sea-ice variables
   USE dom_ice         ! LIM sea-ice domain
   USE domvvl          ! domain: variable volume level
   USE limthd_dif      ! LIM: thermodynamics, vertical diffusion
   USE limthd_dh       ! LIM: thermodynamics, ice and snow thickness variation
   USE limthd_sal      ! LIM: thermodynamics, ice salinity
   USE limthd_ent      ! LIM: thermodynamics, ice enthalpy redistribution
   USE limtab          ! LIM: 1D <==> 2D transformation
   USE limvar          ! LIM: sea-ice variables
   USE lbclnk          ! lateral boundary condition - MPP links
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! work arrays
   USE in_out_manager  ! I/O manager
   USE prtctl          ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_thd        ! called by limstp module
   PUBLIC   lim_thd_init   ! called by iceini module

   REAL(wp) ::   epsi20 = 1e-20_wp   ! constant values
   REAL(wp) ::   epsi16 = 1e-16_wp   !
   REAL(wp) ::   epsi10 = 1e-10_wp   !
   REAL(wp) ::   epsi06 = 1e-06_wp   !
   REAL(wp) ::   epsi04 = 1e-04_wp   !
   REAL(wp) ::   zzero  = 0._wp      !
   REAL(wp) ::   zone   = 1._wp      !

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/LIM3 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: limthd.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_thd( kt )
      !!-------------------------------------------------------------------
      !!                ***  ROUTINE lim_thd  ***       
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
      !! ** References : H. Goosse et al. 1996, Bul. Soc. Roy. Sc. Liege, 65, 87-90
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt    ! number of iteration
      !!
      INTEGER  ::   ji, jj, jk, jl   ! dummy loop indices
      INTEGER  ::   nbpb             ! nb of icy pts for thermo. cal.
      REAL(wp) ::   zfric_umin = 5e-03_wp    ! lower bound for the friction velocity
      REAL(wp) ::   zfric_umax = 2e-02_wp    ! upper bound for the friction velocity
      REAL(wp) ::   zinda, zindb, zthsnice, zfric_u     ! local scalar
      REAL(wp) ::   zfntlat, zpareff, zareamin, zcoef   !    -         -
      REAL(wp), POINTER, DIMENSION(:,:) ::   zqlbsbq   ! link with lead energy budget qldif
      !!-------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, zqlbsbq )
   
      !------------------------------------------------------------------------------!
      ! 1) Initialization of diagnostic variables                                    !
      !------------------------------------------------------------------------------!

      !--------------------
      ! 1.2) Heat content    
      !--------------------
      ! Change the units of heat content; from global units to J.m3
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  !Energy of melting q(S,T) [J.m-3]
                  e_i(ji,jj,jk,jl) = e_i(ji,jj,jk,jl) / ( area(ji,jj) * MAX( v_i(ji,jj,jl) , epsi06 ) ) * nlay_i
                  !0 if no ice and 1 if yes
                  zindb = 1.0 - MAX ( 0.0 , SIGN ( 1.0 , - ht_i(ji,jj,jl) ) ) 
                  !convert units ! very important that this line is here
                  e_i(ji,jj,jk,jl) = e_i(ji,jj,jk,jl) * unit_fac * zindb 
               END DO
            END DO
         END DO
         DO jk = 1, nlay_s
            DO jj = 1, jpj
               DO ji = 1, jpi
                  !Energy of melting q(S,T) [J.m-3]
                  e_s(ji,jj,jk,jl) = e_s(ji,jj,jk,jl) / ( area(ji,jj) * MAX( v_s(ji,jj,jl) , epsi06 ) ) * nlay_s
                  !0 if no ice and 1 if yes
                  zindb = 1.0 - MAX ( 0.0 , SIGN ( 1.0 , - ht_s(ji,jj,jl) ) ) 
                  !convert units ! very important that this line is here
                  e_s(ji,jj,jk,jl) = e_s(ji,jj,jk,jl) * unit_fac * zindb 
               END DO
            END DO
         END DO
      END DO

      !-----------------------------
      ! 1.3) Set some dummies to 0
      !-----------------------------
      rdvosif(:,:) = 0.e0   ! variation of ice volume at surface
      rdvobif(:,:) = 0.e0   ! variation of ice volume at bottom
      fdvolif(:,:) = 0.e0   ! total variation of ice volume
      rdvonif(:,:) = 0.e0   ! lateral variation of ice volume
      fstric (:,:) = 0.e0   ! part of solar radiation transmitted through the ice
      ffltbif(:,:) = 0.e0   ! linked with fstric
      qfvbq  (:,:) = 0.e0   ! linked with fstric
      rdmsnif(:,:) = 0.e0   ! variation of snow mass per unit area
      rdmicif(:,:) = 0.e0   ! variation of ice mass per unit area
      hicifp (:,:) = 0.e0   ! daily thermodynamic ice production. 
      fsbri  (:,:) = 0.e0   ! brine flux contribution to salt flux to the ocean
      fhbri  (:,:) = 0.e0   ! brine flux contribution to heat flux to the ocean
      fseqv  (:,:) = 0.e0   ! equivalent salt flux to the ocean due to ice/growth decay

      !-----------------------------------
      ! 1.4) Compute global heat content
      !-----------------------------------
      qt_i_in  (:,:) = 0.e0
      qt_s_in  (:,:) = 0.e0
      qt_i_fin (:,:) = 0.e0
      qt_s_fin (:,:) = 0.e0
      sum_fluxq(:,:) = 0.e0
      fatm     (:,:) = 0.e0

      ! 2) Partial computation of forcing for the thermodynamic sea ice model.      !
      !-----------------------------------------------------------------------------!

!CDIR NOVERRCHK
      DO jj = 1, jpj
!CDIR NOVERRCHK
         DO ji = 1, jpi
            zthsnice       = SUM( ht_s(ji,jj,1:jpl) ) + SUM( ht_i(ji,jj,1:jpl) )
            zindb          = tms(ji,jj) * ( 1.0 - MAX( zzero , SIGN( zone , - zthsnice ) ) ) 
            phicif(ji,jj)  = vt_i(ji,jj)
            pfrld(ji,jj)   = 1.0 - at_i(ji,jj)
            zinda          = 1.0 - MAX( zzero , SIGN( zone , - ( 1.0 - pfrld(ji,jj) ) ) )
            !
            !           !  solar irradiance transmission at the mixed layer bottom and used in the lead heat budget
            !           !  practically no "direct lateral ablation"
            !           
            !           !  net downward heat flux from the ice to the ocean, expressed as a function of ocean 
            !           !  temperature and turbulent mixing (McPhee, 1992)
            ! friction velocity
            zfric_u        = MAX ( MIN( SQRT( ust2s(ji,jj) ) , zfric_umax ) , zfric_umin ) 

            ! here the drag will depend on ice thickness and type (0.006)
            fdtcn(ji,jj)  = zindb * rau0 * rcp * 0.006  * zfric_u * ( (sst_m(ji,jj) + rt0) - t_bo(ji,jj) ) 
            ! also category dependent
            !           !-- Energy from the turbulent oceanic heat flux heat flux coming in the lead 
            qdtcn(ji,jj)  = zindb * fdtcn(ji,jj) * (1.0 - at_i(ji,jj)) * rdt_ice
            !                       
            !           !-- Lead heat budget, qldif (part 1, next one is in limthd_dh) 
            !           !   caution: exponent betas used as more snow can fallinto leads
            qldif(ji,jj) =  tms(ji,jj) * rdt_ice  * (                             &
               &   pfrld(ji,jj)        * (  qsr(ji,jj)                            &   ! solar heat
               &                            + qns(ji,jj)                          &   ! non solar heat
               &                            + fdtcn(ji,jj)                        &   ! turbulent ice-ocean heat
               &                            + fsbbq(ji,jj) * ( 1.0 - zindb )  )   &   ! residual heat from previous step
               & - pfrld(ji,jj)**betas * sprecip(ji,jj) * lfus                    )   ! latent heat of sprecip melting
            !
            ! Positive heat budget is used for bottom ablation
            zfntlat        = 1.0 - MAX( zzero , SIGN( zone ,  - qldif(ji,jj) ) )
            != 1 if positive heat budget
            zpareff        = 1.0 - zinda * zfntlat
            != 0 if ice and positive heat budget and 1 if one of those two is false
            zqlbsbq(ji,jj) = qldif(ji,jj) * ( 1.0 - zpareff ) / MAX( at_i(ji,jj) * rdt_ice , epsi16 )
            !
            ! Heat budget of the lead, energy transferred from ice to ocean
            qldif  (ji,jj) = zpareff * qldif(ji,jj)
            qdtcn  (ji,jj) = zpareff * qdtcn(ji,jj)
            !
            ! Energy needed to bring ocean surface layer until its freezing (qcmif, limflx)
            qcmif  (ji,jj) =  rau0 * rcp * fse3t(ji,jj,1) * ( t_bo(ji,jj) - (sst_m(ji,jj) + rt0) ) * ( 1. - zinda )
            !
            ! oceanic heat flux (limthd_dh)
            fbif   (ji,jj) = zindb * (  fsbbq(ji,jj) / MAX( at_i(ji,jj) , epsi20 ) + fdtcn(ji,jj) )
            !
         END DO
      END DO

      !------------------------------------------------------------------------------!
      ! 3) Select icy points and fulfill arrays for the vectorial grid.            
      !------------------------------------------------------------------------------!

      DO jl = 1, jpl      !loop over ice categories

         IF( kt == nit000 .AND. lwp ) THEN
            WRITE(numout,*) ' lim_thd : transfer to 1D vectors. Category no : ', jl 
            WRITE(numout,*) ' ~~~~~~~~'
         ENDIF

         zareamin = 1.e-10
         nbpb = 0
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF ( a_i(ji,jj,jl) .gt. zareamin ) THEN     
                  nbpb      = nbpb  + 1
                  npb(nbpb) = (jj - 1) * jpi + ji
               ENDIF
               ! debug point to follow
               IF ( (ji.eq.jiindx).AND.(jj.eq.jjindx) ) THEN
                  jiindex_1d = nbpb
               ENDIF
            END DO
         END DO

         !------------------------------------------------------------------------------!
         ! 4) Thermodynamic computation
         !------------------------------------------------------------------------------!

         IF( lk_mpp )   CALL mpp_ini_ice( nbpb , numout )

         IF( nbpb > 0 ) THEN  ! If there is no ice, do nothing.

            !-------------------------
            ! 4.1 Move to 1D arrays
            !-------------------------

            CALL tab_2d_1d( nbpb, at_i_b     (1:nbpb), at_i            , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, a_i_b      (1:nbpb), a_i(:,:,jl)     , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, ht_i_b     (1:nbpb), ht_i(:,:,jl)    , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, ht_s_b     (1:nbpb), ht_s(:,:,jl)    , jpi, jpj, npb(1:nbpb) )

            CALL tab_2d_1d( nbpb, t_su_b     (1:nbpb), t_su(:,:,jl)    , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, sm_i_b     (1:nbpb), sm_i(:,:,jl)    , jpi, jpj, npb(1:nbpb) )
            DO jk = 1, nlay_s
               CALL tab_2d_1d( nbpb, t_s_b(1:nbpb,jk), t_s(:,:,jk,jl)  , jpi, jpj, npb(1:nbpb) )
               CALL tab_2d_1d( nbpb, q_s_b(1:nbpb,jk), e_s(:,:,jk,jl)  , jpi, jpj, npb(1:nbpb) )
            END DO
            DO jk = 1, nlay_i
               CALL tab_2d_1d( nbpb, t_i_b(1:nbpb,jk), t_i(:,:,jk,jl)  , jpi, jpj, npb(1:nbpb) )
               CALL tab_2d_1d( nbpb, q_i_b(1:nbpb,jk), e_i(:,:,jk,jl)  , jpi, jpj, npb(1:nbpb) )
               CALL tab_2d_1d( nbpb, s_i_b(1:nbpb,jk), s_i(:,:,jk,jl)  , jpi, jpj, npb(1:nbpb) )
            END DO

            CALL tab_2d_1d( nbpb, tatm_ice_1d(1:nbpb), tatm_ice(:,:)   , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, qsr_ice_1d (1:nbpb), qsr_ice(:,:,jl) , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, fr1_i0_1d  (1:nbpb), fr1_i0          , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, fr2_i0_1d  (1:nbpb), fr2_i0          , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, qnsr_ice_1d(1:nbpb), qns_ice(:,:,jl) , jpi, jpj, npb(1:nbpb) )

#if ! defined key_coupled
            CALL tab_2d_1d( nbpb, qla_ice_1d (1:nbpb), qla_ice(:,:,jl)    , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, dqla_ice_1d(1:nbpb), dqla_ice(:,:,jl)   , jpi, jpj, npb(1:nbpb) )
#endif

            CALL tab_2d_1d( nbpb, dqns_ice_1d(1:nbpb), dqns_ice(:,:,jl)   , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, t_bo_b     (1:nbpb), t_bo       , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, sprecip_1d (1:nbpb), sprecip    , jpi, jpj, npb(1:nbpb) ) 
            CALL tab_2d_1d( nbpb, fbif_1d    (1:nbpb), fbif       , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, qldif_1d   (1:nbpb), qldif      , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, rdmicif_1d (1:nbpb), rdmicif    , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, rdmsnif_1d (1:nbpb), rdmsnif    , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, dmgwi_1d   (1:nbpb), dmgwi      , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, qlbbq_1d   (1:nbpb), zqlbsbq    , jpi, jpj, npb(1:nbpb) )

            CALL tab_2d_1d( nbpb, fseqv_1d   (1:nbpb), fseqv      , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, fsbri_1d   (1:nbpb), fsbri      , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, fhbri_1d   (1:nbpb), fhbri      , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, fstbif_1d  (1:nbpb), fstric     , jpi, jpj, npb(1:nbpb) )
            CALL tab_2d_1d( nbpb, qfvbq_1d   (1:nbpb), qfvbq      , jpi, jpj, npb(1:nbpb) )

            !--------------------------------
            ! 4.3) Thermodynamic processes
            !--------------------------------

            IF( con_i )   CALL lim_thd_enmelt( 1, nbpb )   ! computes sea ice energy of melting
            IF( con_i )   CALL lim_thd_glohec( qt_i_in, qt_s_in, q_i_layer_in, 1, nbpb, jl )

            !                                 !---------------------------------!
            CALL lim_thd_dif( 1, nbpb, jl )   ! Ice/Snow Temperature profile    !
            !                                 !---------------------------------!

            CALL lim_thd_enmelt( 1, nbpb )    ! computes sea ice energy of melting compulsory for limthd_dh

            IF( con_i )   CALL lim_thd_glohec ( qt_i_fin, qt_s_fin, q_i_layer_fin, 1, nbpb, jl ) 
            IF( con_i )   CALL lim_thd_con_dif( 1 , nbpb , jl )

            !                                 !---------------------------------!
            CALL lim_thd_dh( 1, nbpb, jl )    ! Ice/Snow thickness              ! 
            !                                 !---------------------------------!

            !                                 !---------------------------------!
            CALL lim_thd_ent( 1, nbpb, jl )   ! Ice/Snow enthalpy remapping     !
            !                                 !---------------------------------!

            !                                 !---------------------------------!
            CALL lim_thd_sal( 1, nbpb )       ! Ice salinity computation        !
            !                                 !---------------------------------!

            !           CALL lim_thd_enmelt(1,nbpb)   ! computes sea ice energy of melting
            IF( con_i )   CALL lim_thd_glohec( qt_i_fin, qt_s_fin, q_i_layer_fin, 1, nbpb, jl ) 
            IF( con_i )   CALL lim_thd_con_dh ( 1 , nbpb , jl )

            !--------------------------------
            ! 4.4) Move 1D to 2D vectors
            !--------------------------------

            CALL tab_1d_2d( nbpb, at_i        , npb, at_i_b(1:nbpb), jpi, jpj )
            CALL tab_1d_2d( nbpb, ht_i(:,:,jl), npb, ht_i_b(1:nbpb), jpi, jpj )
            CALL tab_1d_2d( nbpb, ht_s(:,:,jl), npb, ht_s_b(1:nbpb), jpi, jpj )
            CALL tab_1d_2d( nbpb, a_i (:,:,jl), npb, a_i_b(1:nbpb) , jpi, jpj )
            CALL tab_1d_2d( nbpb, t_su(:,:,jl), npb, t_su_b(1:nbpb), jpi, jpj )
            CALL tab_1d_2d( nbpb, sm_i(:,:,jl), npb, sm_i_b(1:nbpb), jpi, jpj )

            DO jk = 1, nlay_s
               CALL tab_1d_2d( nbpb, t_s(:,:,jk,jl), npb, t_s_b(1:nbpb,jk), jpi, jpj)
               CALL tab_1d_2d( nbpb, e_s(:,:,jk,jl), npb, q_s_b(1:nbpb,jk), jpi, jpj)
            END DO

            DO jk = 1, nlay_i
               CALL tab_1d_2d( nbpb, t_i(:,:,jk,jl), npb, t_i_b(1:nbpb,jk), jpi, jpj)
               CALL tab_1d_2d( nbpb, e_i(:,:,jk,jl), npb, q_i_b(1:nbpb,jk), jpi, jpj)
               CALL tab_1d_2d( nbpb, s_i(:,:,jk,jl), npb, s_i_b(1:nbpb,jk), jpi, jpj)
            END DO

            CALL tab_1d_2d( nbpb, fstric , npb, fstbif_1d (1:nbpb), jpi, jpj )
            CALL tab_1d_2d( nbpb, qldif  , npb, qldif_1d  (1:nbpb), jpi, jpj )
            CALL tab_1d_2d( nbpb, qfvbq  , npb, qfvbq_1d  (1:nbpb), jpi, jpj )
            CALL tab_1d_2d( nbpb, rdmicif, npb, rdmicif_1d(1:nbpb), jpi, jpj )
            CALL tab_1d_2d( nbpb, rdmsnif, npb, rdmsnif_1d(1:nbpb), jpi, jpj )
            CALL tab_1d_2d( nbpb, dmgwi  , npb, dmgwi_1d  (1:nbpb), jpi, jpj )
            CALL tab_1d_2d( nbpb, rdvosif, npb, dvsbq_1d  (1:nbpb), jpi, jpj )
            CALL tab_1d_2d( nbpb, rdvobif, npb, dvbbq_1d  (1:nbpb), jpi, jpj )
            CALL tab_1d_2d( nbpb, fdvolif, npb, dvlbq_1d  (1:nbpb), jpi, jpj )
            CALL tab_1d_2d( nbpb, rdvonif, npb, dvnbq_1d  (1:nbpb), jpi, jpj ) 
            CALL tab_1d_2d( nbpb, fseqv  , npb, fseqv_1d  (1:nbpb), jpi, jpj )
            !
            IF( num_sal == 2 ) THEN
               CALL tab_1d_2d( nbpb, fsbri, npb, fsbri_1d(1:nbpb), jpi, jpj )
               CALL tab_1d_2d( nbpb, fhbri, npb, fhbri_1d(1:nbpb), jpi, jpj )
            ENDIF
            !
            !+++++
            !temporary stuff for a dummy version
            CALL tab_1d_2d( nbpb, dh_i_surf2D, npb, dh_i_surf(1:nbpb)      , jpi, jpj )
            CALL tab_1d_2d( nbpb, dh_i_bott2D, npb, dh_i_bott(1:nbpb)      , jpi, jpj )
            CALL tab_1d_2d( nbpb, fsup2D     , npb, fsup     (1:nbpb)      , jpi, jpj )
            CALL tab_1d_2d( nbpb, focea2D    , npb, focea    (1:nbpb)      , jpi, jpj )
            CALL tab_1d_2d( nbpb, s_i_newice , npb, s_i_new  (1:nbpb)      , jpi, jpj )
            CALL tab_1d_2d( nbpb, izero(:,:,jl) , npb, i0    (1:nbpb)      , jpi, jpj )
            CALL tab_1d_2d( nbpb, qns_ice(:,:,jl), npb, qnsr_ice_1d(1:nbpb), jpi, jpj)
            !+++++
            !
            IF( lk_mpp )   CALL mpp_comm_free( ncomm_ice ) !RB necessary ??
         ENDIF
         !
      END DO

      !------------------------------------------------------------------------------!
      ! 5) Global variables, diagnostics
      !------------------------------------------------------------------------------!

      !------------------------
      ! 5.1) Ice heat content              
      !------------------------
      ! Enthalpies are global variables we have to readjust the units
      zcoef = 1._wp / ( unit_fac * REAL( nlay_i ) )
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            ! Multiply by volume, divide by nlayers so that heat content in 10^9 Joules
            e_i(:,:,jk,jl) = e_i(:,:,jk,jl) * area(:,:) * a_i(:,:,jl) * ht_i(:,:,jl) * zcoef
         END DO
      END DO

      !------------------------
      ! 5.2) Snow heat content              
      !------------------------
      ! Enthalpies are global variables we have to readjust the units
      zcoef = 1._wp / ( unit_fac * REAL( nlay_s ) )
      DO jl = 1, jpl
         DO jk = 1, nlay_s
            ! Multiply by volume, so that heat content in 10^9 Joules
            e_s(:,:,jk,jl) = e_s(:,:,jk,jl) * area(:,:) * a_i(:,:,jl) * ht_s(:,:,jl) * zcoef
         END DO
      END DO

      !----------------------------------
      ! 5.3) Change thickness to volume
      !----------------------------------
      CALL lim_var_eqv2glo

      !--------------------------------------------
      ! 5.4) Diagnostic thermodynamic growth rates
      !--------------------------------------------
      d_v_i_thd(:,:,:) = v_i      (:,:,:) - old_v_i(:,:,:)    ! ice volumes 
      dv_dt_thd(:,:,:) = d_v_i_thd(:,:,:) / rdt_ice * 86400.0

      IF( con_i )   fbif(:,:) = fbif(:,:) + zqlbsbq(:,:)

      IF(ln_ctl) THEN            ! Control print
         CALL prt_ctl_info(' ')
         CALL prt_ctl_info(' - Cell values : ')
         CALL prt_ctl_info('   ~~~~~~~~~~~~~ ')
         CALL prt_ctl(tab2d_1=area , clinfo1=' lim_thd  : cell area :')
         CALL prt_ctl(tab2d_1=at_i , clinfo1=' lim_thd  : at_i      :')
         CALL prt_ctl(tab2d_1=vt_i , clinfo1=' lim_thd  : vt_i      :')
         CALL prt_ctl(tab2d_1=vt_s , clinfo1=' lim_thd  : vt_s      :')
         DO jl = 1, jpl
            CALL prt_ctl_info(' ')
            CALL prt_ctl_info(' - Category : ', ivar1=jl)
            CALL prt_ctl_info('   ~~~~~~~~~~')
            CALL prt_ctl(tab2d_1=a_i   (:,:,jl)   , clinfo1= ' lim_thd  : a_i      : ')
            CALL prt_ctl(tab2d_1=ht_i  (:,:,jl)   , clinfo1= ' lim_thd  : ht_i     : ')
            CALL prt_ctl(tab2d_1=ht_s  (:,:,jl)   , clinfo1= ' lim_thd  : ht_s     : ')
            CALL prt_ctl(tab2d_1=v_i   (:,:,jl)   , clinfo1= ' lim_thd  : v_i      : ')
            CALL prt_ctl(tab2d_1=v_s   (:,:,jl)   , clinfo1= ' lim_thd  : v_s      : ')
            CALL prt_ctl(tab2d_1=e_s   (:,:,1,jl) , clinfo1= ' lim_thd  : e_s      : ')
            CALL prt_ctl(tab2d_1=t_su  (:,:,jl)   , clinfo1= ' lim_thd  : t_su     : ')
            CALL prt_ctl(tab2d_1=t_s   (:,:,1,jl) , clinfo1= ' lim_thd  : t_snow   : ')
            CALL prt_ctl(tab2d_1=sm_i  (:,:,jl)   , clinfo1= ' lim_thd  : sm_i     : ')
            CALL prt_ctl(tab2d_1=smv_i (:,:,jl)   , clinfo1= ' lim_thd  : smv_i    : ')
            DO jk = 1, nlay_i
               CALL prt_ctl_info(' ')
               CALL prt_ctl_info(' - Layer : ', ivar1=jk)
               CALL prt_ctl_info('   ~~~~~~~')
               CALL prt_ctl(tab2d_1=t_i(:,:,jk,jl) , clinfo1= ' lim_thd  : t_i      : ')
               CALL prt_ctl(tab2d_1=e_i(:,:,jk,jl) , clinfo1= ' lim_thd  : e_i      : ')
            END DO
         END DO
      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, zqlbsbq )
      !
   END SUBROUTINE lim_thd


   SUBROUTINE lim_thd_glohec( eti, ets, etilayer, kideb, kiut, jl )
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_thd_glohec *** 
      !!                 
      !! ** Purpose :  Compute total heat content for each category
      !!               Works with 1d vectors only
      !!-----------------------------------------------------------------------
      INTEGER , INTENT(in   )                         ::   kideb, kiut   ! bounds for the spatial loop
      INTEGER , INTENT(in   )                         ::   jl            ! category number
      REAL(wp), INTENT(  out), DIMENSION (jpij,jpl  ) ::   eti, ets      ! vertically-summed heat content for ice & snow
      REAL(wp), INTENT(  out), DIMENSION (jpij,jkmax) ::   etilayer      ! heat content for ice layers
      !!
      INTEGER  ::   ji,jk   ! loop indices
      !!-----------------------------------------------------------------------
      eti(:,:) = 0._wp
      ets(:,:) = 0._wp
      !
      DO jk = 1, nlay_i                ! total q over all layers, ice [J.m-2]
         DO ji = kideb, kiut
            etilayer(ji,jk) = q_i_b(ji,jk) * ht_i_b(ji) / nlay_i
            eti     (ji,jl) = eti(ji,jl) + etilayer(ji,jk) 
         END DO
      END DO
      DO ji = kideb, kiut              ! total q over all layers, snow [J.m-2]
         ets(ji,jl) = ets(ji,jl) + q_s_b(ji,1) * ht_s_b(ji) / nlay_s
      END DO
      !
      IF(lwp) WRITE(numout,*) ' lim_thd_glohec '
      IF(lwp) WRITE(numout,*) ' qt_i_in : ', eti(jiindex_1d,jl) / rdt_ice
      IF(lwp) WRITE(numout,*) ' qt_s_in : ', ets(jiindex_1d,jl) / rdt_ice
      IF(lwp) WRITE(numout,*) ' qt_in   : ', ( eti(jiindex_1d,jl) + ets(jiindex_1d,jl) ) / rdt_ice
      !
   END SUBROUTINE lim_thd_glohec


   SUBROUTINE lim_thd_con_dif( kideb, kiut, jl )
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_thd_con_dif *** 
      !!                 
      !! ** Purpose :   Test energy conservation after heat diffusion
      !!-------------------------------------------------------------------
      INTEGER , INTENT(in   ) ::   kideb, kiut   ! bounds for the spatial loop
      INTEGER , INTENT(in   ) ::   jl            ! category number

      INTEGER  ::   ji, jk         ! loop indices
      INTEGER  ::   zji, zjj
      INTEGER  ::   numce          ! number of points for which conservation is violated
      REAL(wp) ::   meance         ! mean conservation error
      REAL(wp) ::   max_cons_err, max_surf_err
      !!---------------------------------------------------------------------

      max_cons_err =  1.0_wp          ! maximum tolerated conservation error
      max_surf_err =  0.001_wp        ! maximum tolerated surface error

      !--------------------------
      ! Increment of energy
      !--------------------------
      ! global
      DO ji = kideb, kiut
         dq_i(ji,jl) = qt_i_fin(ji,jl) - qt_i_in(ji,jl) + qt_s_fin(ji,jl) - qt_s_in(ji,jl)
      END DO
      ! layer by layer
      dq_i_layer(:,:) = q_i_layer_fin(:,:) - q_i_layer_in(:,:)

      !----------------------------------------
      ! Atmospheric heat flux, ice heat budget
      !----------------------------------------
      DO ji = kideb, kiut
         zji = MOD( npb(ji) - 1 , jpi ) + 1
         zjj =    ( npb(ji) - 1 ) / jpi + 1
         fatm     (ji,jl) = qnsr_ice_1d(ji) + ( 1._wp - i0(ji) ) * qsr_ice_1d(ji)
         sum_fluxq(ji,jl) = fc_su(ji) - fc_bo_i(ji) + qsr_ice_1d(ji) * i0(ji) - fstroc(zji,zjj,jl)
      END DO

      !--------------------
      ! Conservation error
      !--------------------
      DO ji = kideb, kiut
         cons_error(ji,jl) = ABS( dq_i(ji,jl) / rdt_ice + sum_fluxq(ji,jl) )
      END DO

      numce  = 0
      meance = 0._wp
      DO ji = kideb, kiut
         IF ( cons_error(ji,jl) .GT. max_cons_err ) THEN
            numce = numce + 1
            meance = meance + cons_error(ji,jl)
         ENDIF
      END DO
      IF( numce > 0 )   meance = meance / numce

      WRITE(numout,*) ' Maximum tolerated conservation error : ', max_cons_err
      WRITE(numout,*) ' After lim_thd_dif, category : ', jl
      WRITE(numout,*) ' Mean conservation error on big error points ', meance, numit
      WRITE(numout,*) ' Number of points where there is a cons err gt than c.e. : ', numce, numit

      !-------------------------------------------------------
      ! Surface error due to imbalance between Fatm and Fcsu
      !-------------------------------------------------------
      numce  = 0
      meance = 0._wp

      DO ji = kideb, kiut
         surf_error(ji,jl) = ABS ( fatm(ji,jl) - fc_su(ji) )
         IF( ( t_su_b(ji) .LT. rtt ) .AND. ( surf_error(ji,jl) .GT. max_surf_err ) ) THEN
            numce = numce + 1 
            meance = meance + surf_error(ji,jl)
         ENDIF
      ENDDO
      IF( numce > 0 )   meance = meance / numce

      WRITE(numout,*) ' Maximum tolerated surface error : ', max_surf_err
      WRITE(numout,*) ' After lim_thd_dif, category : ', jl
      WRITE(numout,*) ' Mean surface error on big error points ', meance, numit
      WRITE(numout,*) ' Number of points where there is a surf err gt than surf_err : ', numce, numit

      IF (jiindex_1D.GT.0) WRITE(numout,*) ' fc_su      : ', fc_su(jiindex_1d)
      IF (jiindex_1D.GT.0) WRITE(numout,*) ' fatm       : ', fatm(jiindex_1d,jl)
      IF (jiindex_1D.GT.0) WRITE(numout,*) ' t_su       : ', t_su_b(jiindex_1d)

      !---------------------------------------
      ! Write ice state in case of big errors
      !---------------------------------------
      DO ji = kideb, kiut
         IF ( ( ( t_su_b(ji) .LT. rtt ) .AND. ( surf_error(ji,jl) .GT. max_surf_err ) ) .OR. &
            ( cons_error(ji,jl) .GT. max_cons_err  ) ) THEN
            zji                 = MOD( npb(ji) - 1, jpi ) + 1
            zjj                 = ( npb(ji) - 1 ) / jpi + 1
            !
            WRITE(numout,*) ' alerte 1     '
            WRITE(numout,*) ' Untolerated conservation / surface error after '
            WRITE(numout,*) ' heat diffusion in the ice '
            WRITE(numout,*) ' Category   : ', jl
            WRITE(numout,*) ' zji , zjj  : ', zji, zjj
            WRITE(numout,*) ' lat, lon   : ', gphit(zji,zjj), glamt(zji,zjj)
            WRITE(numout,*) ' cons_error : ', cons_error(ji,jl)
            WRITE(numout,*) ' surf_error : ', surf_error(ji,jl)
            WRITE(numout,*) ' dq_i       : ', - dq_i(ji,jl) / rdt_ice
            WRITE(numout,*) ' Fdt        : ', sum_fluxq(ji,jl)
            WRITE(numout,*)
            !        WRITE(numout,*) ' qt_i_in   : ', qt_i_in(ji,jl)
            !        WRITE(numout,*) ' qt_s_in   : ', qt_s_in(ji,jl)
            !        WRITE(numout,*) ' qt_i_fin  : ', qt_i_fin(ji,jl)
            !        WRITE(numout,*) ' qt_s_fin  : ', qt_s_fin(ji,jl)
            !        WRITE(numout,*) ' qt        : ', qt_i_fin(ji,jl) + qt_s_fin(ji,jl)
            WRITE(numout,*) ' ht_i       : ', ht_i_b(ji)
            WRITE(numout,*) ' ht_s       : ', ht_s_b(ji)
            WRITE(numout,*) ' t_su       : ', t_su_b(ji)
            WRITE(numout,*) ' t_s        : ', t_s_b(ji,1)
            WRITE(numout,*) ' t_i        : ', t_i_b(ji,1:nlay_i)
            WRITE(numout,*) ' t_bo       : ', t_bo_b(ji)
            WRITE(numout,*) ' q_i        : ', q_i_b(ji,1:nlay_i)
            WRITE(numout,*) ' s_i        : ', s_i_b(ji,1:nlay_i)
            WRITE(numout,*) ' tmelts     : ', rtt - tmut*s_i_b(ji,1:nlay_i)
            WRITE(numout,*)
            WRITE(numout,*) ' Fluxes '
            WRITE(numout,*) ' ~~~~~~ '
            WRITE(numout,*) ' fatm       : ', fatm(ji,jl)
            WRITE(numout,*) ' fc_su      : ', fc_su    (ji)
            WRITE(numout,*) ' fstr_inice : ', qsr_ice_1d(ji)*i0(ji)
            WRITE(numout,*) ' fc_bo      : ', - fc_bo_i  (ji)
            WRITE(numout,*) ' foc        : ', fbif_1d(ji)
            WRITE(numout,*) ' fstroc     : ', fstroc   (zji,zjj,jl)
            WRITE(numout,*) ' i0         : ', i0(ji)
            WRITE(numout,*) ' qsr_ice    : ', (1.0-i0(ji))*qsr_ice_1d(ji)
            WRITE(numout,*) ' qns_ice    : ', qnsr_ice_1d(ji)
            WRITE(numout,*) ' Conduction fluxes : '
            WRITE(numout,*) ' fc_s      : ', fc_s(ji,0:nlay_s)
            WRITE(numout,*) ' fc_i      : ', fc_i(ji,0:nlay_i)
            WRITE(numout,*)
            WRITE(numout,*) ' Layer by layer ... '
            WRITE(numout,*) ' dq_snow : ', ( qt_s_fin(ji,jl) - qt_s_in(ji,jl) ) / rdt_ice
            WRITE(numout,*) ' dfc_snow  : ', fc_s(ji,1) - fc_s(ji,0)
            DO jk = 1, nlay_i
               WRITE(numout,*) ' layer  : ', jk
               WRITE(numout,*) ' dq_ice : ', dq_i_layer(ji,jk) / rdt_ice  
               WRITE(numout,*) ' radab  : ', radab(ji,jk)
               WRITE(numout,*) ' dfc_i  : ', fc_i(ji,jk) - fc_i(ji,jk-1)
               WRITE(numout,*) ' tot f  : ', fc_i(ji,jk) - fc_i(ji,jk-1) - radab(ji,jk)
            END DO

         ENDIF
         !
      END DO
      !
   END SUBROUTINE lim_thd_con_dif


   SUBROUTINE lim_thd_con_dh( kideb, kiut, jl )
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_thd_con_dh  *** 
      !!                 
      !! ** Purpose :   Test energy conservation after enthalpy redistr.
      !!-----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kideb, kiut   ! bounds for the spatial loop
      INTEGER, INTENT(in) ::   jl            ! category number
      !
      INTEGER  ::   ji                ! loop indices
      INTEGER  ::   zji, zjj, numce         ! local integers
      REAL(wp) ::   meance, max_cons_err    !local scalar
      !!---------------------------------------------------------------------

      max_cons_err = 1._wp

      !--------------------------
      ! Increment of energy
      !--------------------------
      DO ji = kideb, kiut
         dq_i(ji,jl) = qt_i_fin(ji,jl) - qt_i_in(ji,jl) + qt_s_fin(ji,jl) - qt_s_in(ji,jl)   ! global
      END DO
      dq_i_layer(:,:)    = q_i_layer_fin(:,:) - q_i_layer_in(:,:)                            ! layer by layer

      !----------------------------------------
      ! Atmospheric heat flux, ice heat budget
      !----------------------------------------
      DO ji = kideb, kiut
         zji = MOD( npb(ji) - 1 , jpi ) + 1
         zjj =    ( npb(ji) - 1 ) / jpi + 1

         fatm      (ji,jl) = qnsr_ice_1d(ji) + qsr_ice_1d(ji)                       ! total heat flux
         sum_fluxq (ji,jl) = fatm(ji,jl) + fbif_1d(ji) - ftotal_fin(ji) - fstroc(zji,zjj,jl) 
         cons_error(ji,jl) = ABS( dq_i(ji,jl) / rdt_ice + sum_fluxq(ji,jl) )
      END DO

      !--------------------
      ! Conservation error
      !--------------------
      DO ji = kideb, kiut
         cons_error(ji,jl) = ABS( dq_i(ji,jl) / rdt_ice + sum_fluxq(ji,jl) )
      END DO

      numce = 0
      meance = 0._wp
      DO ji = kideb, kiut
         IF( cons_error(ji,jl) .GT. max_cons_err ) THEN
            numce = numce + 1
            meance = meance + cons_error(ji,jl)
         ENDIF
      ENDDO
      IF(numce > 0 ) meance = meance / numce

      WRITE(numout,*) ' Error report - Category : ', jl
      WRITE(numout,*) ' ~~~~~~~~~~~~ '
      WRITE(numout,*) ' Maximum tolerated conservation error : ', max_cons_err
      WRITE(numout,*) ' After lim_thd_ent, category : ', jl
      WRITE(numout,*) ' Mean conservation error on big error points ', meance, numit
      WRITE(numout,*) ' Number of points where there is a cons err gt than 0.1 W/m2 : ', numce, numit

      !---------------------------------------
      ! Write ice state in case of big errors
      !---------------------------------------
      DO ji = kideb, kiut
         IF ( cons_error(ji,jl) .GT. max_cons_err  ) THEN
            zji = MOD( npb(ji) - 1, jpi ) + 1
            zjj =    ( npb(ji) - 1 ) / jpi + 1
            !
            WRITE(numout,*) ' alerte 1 - category : ', jl
            WRITE(numout,*) ' Untolerated conservation error after limthd_ent '
            WRITE(numout,*) ' zji , zjj  : ', zji, zjj
            WRITE(numout,*) ' lat, lon   : ', gphit(zji,zjj), glamt(zji,zjj)
            WRITE(numout,*) ' * '
            WRITE(numout,*) ' Ftotal     : ', sum_fluxq(ji,jl)
            WRITE(numout,*) ' dq_t       : ', - dq_i(ji,jl) / rdt_ice
            WRITE(numout,*) ' dq_i       : ', - ( qt_i_fin(ji,jl) - qt_i_in(ji,jl) ) / rdt_ice
            WRITE(numout,*) ' dq_s       : ', - ( qt_s_fin(ji,jl) - qt_s_in(ji,jl) ) / rdt_ice
            WRITE(numout,*) ' cons_error : ', cons_error(ji,jl)
            WRITE(numout,*) ' * '
            WRITE(numout,*) ' Fluxes        --- : '
            WRITE(numout,*) ' fatm       : ', fatm(ji,jl)
            WRITE(numout,*) ' foce       : ', fbif_1d(ji)
            WRITE(numout,*) ' fres       : ', ftotal_fin(ji)
            WRITE(numout,*) ' fhbri      : ', fhbricat(zji,zjj,jl)
            WRITE(numout,*) ' * '
            WRITE(numout,*) ' Heat contents --- : '
            WRITE(numout,*) ' qt_s_in    : ', qt_s_in(ji,jl) / rdt_ice
            WRITE(numout,*) ' qt_i_in    : ', qt_i_in(ji,jl) / rdt_ice
            WRITE(numout,*) ' qt_in      : ', ( qt_i_in(ji,jl) + qt_s_in(ji,jl) ) / rdt_ice
            WRITE(numout,*) ' qt_s_fin   : ', qt_s_fin(ji,jl) / rdt_ice
            WRITE(numout,*) ' qt_i_fin   : ', qt_i_fin(ji,jl) / rdt_ice
            WRITE(numout,*) ' qt_fin     : ', ( qt_i_fin(ji,jl) + qt_s_fin(ji,jl) ) / rdt_ice
            WRITE(numout,*) ' * '
            WRITE(numout,*) ' Ice variables --- : '
            WRITE(numout,*) ' ht_i       : ', ht_i_b(ji)
            WRITE(numout,*) ' ht_s       : ', ht_s_b(ji)
            WRITE(numout,*) ' dh_s_tot  : ', dh_s_tot(ji)
            WRITE(numout,*) ' dh_snowice: ', dh_snowice(ji)
            WRITE(numout,*) ' dh_i_surf : ', dh_i_surf(ji)
            WRITE(numout,*) ' dh_i_bott : ', dh_i_bott(ji)
         ENDIF
         !
      END DO
      !
   END SUBROUTINE lim_thd_con_dh


   SUBROUTINE lim_thd_enmelt( kideb, kiut )
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_thd_enmelt *** 
      !!                 
      !! ** Purpose :   Computes sea ice energy of melting q_i (J.m-3)
      !!
      !! ** Method  :   Formula (Bitz and Lipscomb, 1999)
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kideb, kiut   ! bounds for the spatial loop
      !!
      INTEGER  ::   ji, jk   ! dummy loop indices
      REAL(wp) ::   ztmelts  ! local scalar 
      !!-------------------------------------------------------------------
      !
      DO jk = 1, nlay_i             ! Sea ice energy of melting
         DO ji = kideb, kiut
            ztmelts      =  - tmut  * s_i_b(ji,jk) + rtt 
            q_i_b(ji,jk) =    rhoic * ( cpic * ( ztmelts - t_i_b(ji,jk) )                                 &
               &                      + lfus * ( 1.0 - (ztmelts-rtt) / MIN( t_i_b(ji,jk)-rtt, -epsi10 ) )   &
               &                      - rcp  * ( ztmelts-rtt  )  ) 
         END DO
      END DO
      DO jk = 1, nlay_s             ! Snow energy of melting
         DO ji = kideb, kiut
            q_s_b(ji,jk) = rhosn * ( cpic * ( rtt - t_s_b(ji,jk) ) + lfus )
         END DO
      END DO
      !
   END SUBROUTINE lim_thd_enmelt


   SUBROUTINE lim_thd_init
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_thd_init *** 
      !!                 
      !! ** Purpose :   Physical constants and parameters linked to the ice 
      !!              thermodynamics
      !!
      !! ** Method  :   Read the namicethd namelist and check the ice-thermo
      !!              parameter values called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namicether
      !!-------------------------------------------------------------------
      NAMELIST/namicethd/ hmelt , hiccrit, fraz_swi, maxfrazb, vfrazb, Cfrazb,   &
         &                hicmin, hiclim, amax  ,                                &
         &                sbeta  , parlat, hakspl, hibspl, exld,                 &
         &                hakdif, hnzst  , thth  , parsub, alphs, betas,         & 
         &                kappa_i, nconv_i_thd, maxer_i_thd, thcon_i_swi
      !!-------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'lim_thd : Ice Thermodynamics'
         WRITE(numout,*) '~~~~~~~'
      ENDIF
      !
      REWIND( numnam_ice )                  ! read Namelist numnam_ice
      READ  ( numnam_ice , namicethd )
      !
      IF(lwp) THEN                          ! control print
         WRITE(numout,*)
         WRITE(numout,*)'   Namelist of ice parameters for ice thermodynamic computation '
         WRITE(numout,*)'      maximum melting at the bottom                           hmelt        = ', hmelt
         WRITE(numout,*)'      ice thick. for lateral accretion in NH (SH)             hiccrit(1/2) = ', hiccrit
         WRITE(numout,*)'      Frazil ice thickness as a function of wind or not       fraz_swi     = ', fraz_swi
         WRITE(numout,*)'      Maximum proportion of frazil ice collecting at bottom   maxfrazb     = ', maxfrazb
         WRITE(numout,*)'      Thresold relative drift speed for collection of frazil  vfrazb       = ', vfrazb
         WRITE(numout,*)'      Squeezing coefficient for collection of frazil          Cfrazb       = ', Cfrazb
         WRITE(numout,*)'      ice thick. corr. to max. energy stored in brine pocket  hicmin       = ', hicmin  
         WRITE(numout,*)'      minimum ice thickness                                   hiclim       = ', hiclim 
         WRITE(numout,*)'      maximum lead fraction                                   amax         = ', amax 
         WRITE(numout,*)'      numerical carac. of the scheme for diffusion in ice '
         WRITE(numout,*)'      Cranck-Nicholson (=0.5), implicit (=1), explicit (=0)   sbeta        = ', sbeta
         WRITE(numout,*)'      percentage of energy used for lateral ablation          parlat       = ', parlat
         WRITE(numout,*)'      slope of distr. for Hakkinen-Mellor lateral melting     hakspl       = ', hakspl  
         WRITE(numout,*)'      slope of distribution for Hibler lateral melting        hibspl       = ', hibspl
         WRITE(numout,*)'      exponent for leads-closure rate                         exld         = ', exld
         WRITE(numout,*)'      coefficient for diffusions of ice and snow              hakdif       = ', hakdif
         WRITE(numout,*)'      threshold thick. for comp. of eq. thermal conductivity  zhth         = ', thth 
         WRITE(numout,*)'      thickness of the surf. layer in temp. computation       hnzst        = ', hnzst
         WRITE(numout,*)'      switch for snow sublimation  (=1) or not (=0)           parsub       = ', parsub  
         WRITE(numout,*)'      coefficient for snow density when snow ice formation    alphs        = ', alphs
         WRITE(numout,*)'      coefficient for ice-lead partition of snowfall          betas        = ', betas
         WRITE(numout,*)'      extinction radiation parameter in sea ice (1.0)         kappa_i      = ', kappa_i
         WRITE(numout,*)'      maximal n. of iter. for heat diffusion computation      nconv_i_thd  = ', nconv_i_thd
         WRITE(numout,*)'      maximal err. on T for heat diffusion computation        maxer_i_thd  = ', maxer_i_thd
         WRITE(numout,*)'      switch for comp. of thermal conductivity in the ice     thcon_i_swi  = ', thcon_i_swi
      ENDIF
      !
      rcdsn = hakdif * rcdsn 
      rcdic = hakdif * rcdic
      !
   END SUBROUTINE lim_thd_init

#else
   !!----------------------------------------------------------------------
   !!   Default option         Dummy module          NO  LIM3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE limthd
