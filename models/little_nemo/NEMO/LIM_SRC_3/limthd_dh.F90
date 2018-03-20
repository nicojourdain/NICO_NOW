MODULE limthd_dh
   !!======================================================================
   !!                       ***  MODULE limthd_dh ***
   !!  LIM-3 :   thermodynamic growth and decay of the ice 
   !!======================================================================
   !! History :  LIM  ! 2003-05 (M. Vancoppenolle) Original code in 1D
   !!                 ! 2005-06 (M. Vancoppenolle) 3D version 
   !!            3.2  ! 2009-07 (M. Vancoppenolle, Y. Aksenov, G. Madec) bug correction in rdmsnif & rdmicif
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_thd_dh  : vertical accr./abl. and lateral ablation of sea ice
   !!----------------------------------------------------------------------
   USE par_oce          ! ocean parameters
   USE phycst           ! physical constants (OCE directory) 
   USE sbc_oce          ! Surface boundary condition: ocean fields
   USE ice              ! LIM variables
   USE par_ice          ! LIM parameters
   USE thd_ice          ! LIM thermodynamics
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_thd_dh   ! called by lim_thd

   REAL(wp) ::   epsi20 = 1e-20   ! constant values
   REAL(wp) ::   epsi13 = 1e-13   !
   REAL(wp) ::   epsi16 = 1e-16   !
   REAL(wp) ::   zzero  = 0.e0    !
   REAL(wp) ::   zone   = 1.e0    !

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2010)
   !! $Id: limthd_dh.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_thd_dh( kideb, kiut, jl )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_thd_dh  ***
      !!
      !! ** Purpose :   determines variations of ice and snow thicknesses.
      !!
      !! ** Method  :   Ice/Snow surface melting arises from imbalance in surface fluxes
      !!              Bottom accretion/ablation arises from flux budget
      !!              Snow thickness can increase by precipitation and decrease by sublimation
      !!              If snow load excesses Archmiede limit, snow-ice is formed by
      !!              the flooding of sea-water in the snow
      !!
      !!                 1) Compute available flux of heat for surface ablation
      !!                 2) Compute snow and sea ice enthalpies
      !!                 3) Surface ablation and sublimation
      !!                 4) Bottom accretion/ablation
      !!                 5) Case of Total ablation
      !!                 6) Snow ice formation
      !!
      !! References : Bitz and Lipscomb, 1999, J. Geophys. Res.
      !!              Fichefet T. and M. Maqueda 1997, J. Geophys. Res., 102(C6), 12609-12646   
      !!              Vancoppenolle, Fichefet and Bitz, 2005, Geophys. Res. Let. 
      !!              Vancoppenolle et al.,2009, Ocean Modelling
      !!------------------------------------------------------------------
      INTEGER , INTENT(in) ::   kideb, kiut   ! Start/End point on which the  the computation is applied
      INTEGER , INTENT(in) ::   jl            ! Thickness cateogry number
      !! 
      INTEGER  ::   ji , jk        ! dummy loop indices
      INTEGER  ::   zji, zjj       ! 2D corresponding indices to ji
      INTEGER  ::   isnow          ! switch for presence (1) or absence (0) of snow
      INTEGER  ::   isnowic        ! snow ice formation not
      INTEGER  ::   i_ice_switch   ! ice thickness above a certain treshold or not
      INTEGER  ::   iter

      REAL(wp) ::   zzfmass_i, zihgnew                     ! local scalar
      REAL(wp) ::   zzfmass_s, zhsnew, ztmelts             ! local scalar
      REAL(wp) ::   zhn, zdhcf, zdhbf, zhni, zhnfi, zihg   !
      REAL(wp) ::   zdhnm, zhnnew, zhisn, zihic, zzc       !
      REAL(wp) ::   zfracs       ! fractionation coefficient for bottom salt entrapment
      REAL(wp) ::   zds          ! increment of bottom ice salinity
      REAL(wp) ::   zcoeff       ! dummy argument for snowfall partitioning over ice and leads
      REAL(wp) ::   zsm_snowice  ! snow-ice salinity
      REAL(wp) ::   zswi1        ! switch for computation of bottom salinity
      REAL(wp) ::   zswi12       ! switch for computation of bottom salinity
      REAL(wp) ::   zswi2        ! switch for computation of bottom salinity
      REAL(wp) ::   zgrr         ! bottom growth rate
      REAL(wp) ::   ztform       ! bottom formation temperature
      !
      REAL(wp), POINTER, DIMENSION(:) ::   zh_i        ! ice layer thickness
      REAL(wp), POINTER, DIMENSION(:) ::   zh_s        ! snow layer thickness
      REAL(wp), POINTER, DIMENSION(:) ::   ztfs        ! melting point
      REAL(wp), POINTER, DIMENSION(:) ::   zhsold      ! old snow thickness
      REAL(wp), POINTER, DIMENSION(:) ::   zqprec      ! energy of fallen snow
      REAL(wp), POINTER, DIMENSION(:) ::   zqfont_su   ! incoming, remaining surface energy
      REAL(wp), POINTER, DIMENSION(:) ::   zqfont_bo   ! incoming, bottom energy
      REAL(wp), POINTER, DIMENSION(:) ::   z_f_surf    ! surface heat for ablation
      REAL(wp), POINTER, DIMENSION(:) ::   zhgnew      ! new ice thickness
      REAL(wp), POINTER, DIMENSION(:) ::   zfmass_i    ! 

      REAL(wp), POINTER, DIMENSION(:) ::   zdh_s_mel     ! snow melt 
      REAL(wp), POINTER, DIMENSION(:) ::   zdh_s_pre     ! snow precipitation 
      REAL(wp), POINTER, DIMENSION(:) ::   zdh_s_sub     ! snow sublimation
      REAL(wp), POINTER, DIMENSION(:) ::   zfsalt_melt   ! salt flux due to ice melt

      REAL(wp), POINTER, DIMENSION(:,:) ::   zdeltah

      ! Pathological cases
      REAL(wp), POINTER, DIMENSION(:) ::   zfdt_init   ! total incoming heat for ice melt
      REAL(wp), POINTER, DIMENSION(:) ::   zfdt_final  ! total remaing heat for ice melt
      REAL(wp), POINTER, DIMENSION(:) ::   zqt_i       ! total ice heat content
      REAL(wp), POINTER, DIMENSION(:) ::   zqt_s       ! total snow heat content
      REAL(wp), POINTER, DIMENSION(:) ::   zqt_dummy   ! dummy heat content

      REAL(wp), POINTER, DIMENSION(:,:) ::   zqt_i_lay   ! total ice heat content

      ! Heat conservation 
      INTEGER  ::   num_iter_max, numce_dh
      REAL(wp) ::   meance_dh
      REAL(wp), POINTER, DIMENSION(:) ::   zinnermelt
      REAL(wp), POINTER, DIMENSION(:) ::   zfbase, zdq_i
      !!------------------------------------------------------------------

      CALL wrk_alloc( jpij, zh_i, zh_s, ztfs, zhsold, zqprec, zqfont_su, zqfont_bo, z_f_surf, zhgnew, zfmass_i )
      CALL wrk_alloc( jpij, zdh_s_mel, zdh_s_pre, zdh_s_sub, zfsalt_melt, zfdt_init, zfdt_final, zqt_i, zqt_s, zqt_dummy )
      CALL wrk_alloc( jpij, zinnermelt, zfbase, zdq_i )
      CALL wrk_alloc( jpij, jkmax, zdeltah, zqt_i_lay )

      zfsalt_melt(:)  = 0._wp
      ftotal_fin(:)   = 0._wp
      zfdt_init(:)    = 0._wp
      zfdt_final(:)   = 0._wp

      DO ji = kideb, kiut
         old_ht_i_b(ji) = ht_i_b(ji)
         old_ht_s_b(ji) = ht_s_b(ji)
      END DO
      !
      !------------------------------------------------------------------------------!
      !  1) Calculate available heat for surface ablation                            !
      !------------------------------------------------------------------------------!
      !
      DO ji = kideb, kiut
         isnow         = INT( 1.0 - MAX ( 0.0 , SIGN ( 1.0 , - ht_s_b(ji) ) ) )
         ztfs(ji)      = isnow * rtt + ( 1.0 - isnow ) * rtt
         z_f_surf(ji)  = qnsr_ice_1d(ji) + ( 1.0 - i0(ji) ) * qsr_ice_1d(ji) - fc_su(ji)
         z_f_surf(ji)  = MAX( zzero , z_f_surf(ji) ) * MAX( zzero , SIGN( zone , t_su_b(ji) - ztfs(ji) ) )
         zfdt_init(ji) = ( z_f_surf(ji) + MAX( fbif_1d(ji) + qlbbq_1d(ji) + fc_bo_i(ji),0.0 ) ) * rdt_ice
      END DO ! ji

      zqfont_su  (:) = 0._wp
      zqfont_bo  (:) = 0._wp
      dsm_i_se_1d(:) = 0._wp     
      dsm_i_si_1d(:) = 0._wp   
      !
      !------------------------------------------------------------------------------!
      !  2) Computing layer thicknesses and  snow and sea-ice enthalpies.            !
      !------------------------------------------------------------------------------!
      !
      DO ji = kideb, kiut     ! Layer thickness
         zh_i(ji) = ht_i_b(ji) / nlay_i
         zh_s(ji) = ht_s_b(ji) / nlay_s
      END DO
      !
      zqt_s(:) = 0._wp        ! Total enthalpy of the snow
      DO jk = 1, nlay_s
         DO ji = kideb, kiut
            zqt_s(ji) =  zqt_s(ji) + q_s_b(ji,jk) * ht_s_b(ji) / nlay_s
         END DO
      END DO
      !
      zqt_i(:) = 0._wp        ! Total enthalpy of the ice
      DO jk = 1, nlay_i
         DO ji = kideb, kiut
            zzc = q_i_b(ji,jk) * ht_i_b(ji) / nlay_i
            zqt_i(ji)        =  zqt_i(ji) + zzc
            zqt_i_lay(ji,jk) =              zzc
         END DO
      END DO
      !
      !------------------------------------------------------------------------------|
      !  3) Surface ablation and sublimation                                         |
      !------------------------------------------------------------------------------|
      !
      !-------------------------
      ! 3.1 Snow precips / melt
      !-------------------------
      ! Snow accumulation in one thermodynamic time step
      ! snowfall is partitionned between leads and ice
      ! if snow fall was uniform, a fraction (1-at_i) would fall into leads
      ! but because of the winds, more snow falls on leads than on sea ice
      ! and a greater fraction (1-at_i)^beta of the total mass of snow 
      ! (beta < 1) falls in leads.
      ! In reality, beta depends on wind speed, 
      ! and should decrease with increasing wind speed but here, it is 
      ! considered as a constant. an average value is 0.66
      ! Martin Vancoppenolle, December 2006

      ! Snow fall
      DO ji = kideb, kiut
         zcoeff = ( 1.0 - ( 1.0 - at_i_b(ji) )**betas ) / at_i_b(ji) 
         zdh_s_pre(ji) = zcoeff * sprecip_1d(ji) * rdt_ice / rhosn
      END DO
      zdh_s_mel(:) =  0._wp

      ! Melt of fallen snow
      DO ji = kideb, kiut
         ! tatm_ice is now in K
         zqprec   (ji)   =  rhosn * ( cpic * ( rtt - tatm_ice_1d(ji) ) + lfus )  
         zqfont_su(ji)   =  z_f_surf(ji) * rdt_ice
         zdeltah  (ji,1) =  MIN( 0.e0 , - zqfont_su(ji) / MAX( zqprec(ji) , epsi13 ) )
         zqfont_su(ji)   =  MAX( 0.e0 , - zdh_s_pre(ji) - zdeltah(ji,1)              ) * zqprec(ji)
         zdeltah  (ji,1) =  MAX( - zdh_s_pre(ji) , zdeltah(ji,1) )
         zdh_s_mel(ji)   =  zdh_s_mel(ji) + zdeltah(ji,1)
         ! heat conservation
         qt_s_in(ji,jl)  =  qt_s_in(ji,jl) + zqprec(ji) * zdh_s_pre(ji)
         zqt_s  (ji)     =  zqt_s  (ji)    + zqprec(ji) * zdh_s_pre(ji)
         zqt_s  (ji)     =  MAX( zqt_s(ji) - zqfont_su(ji) , 0.e0 ) 
      END DO


      ! Snow melt due to surface heat imbalance
      DO jk = 1, nlay_s
         DO ji = kideb, kiut
            zdeltah  (ji,jk) = - zqfont_su(ji) / q_s_b(ji,jk)
            zqfont_su(ji)    =  MAX( 0.0 , - zh_s(ji) - zdeltah(ji,jk) ) * q_s_b(ji,jk) 
            zdeltah  (ji,jk) =  MAX( zdeltah(ji,jk) , - zh_s(ji) )
            zdh_s_mel(ji)    =  zdh_s_mel(ji) + zdeltah(ji,jk)        ! resulting melt of snow    
         END DO
      END DO

      ! Apply snow melt to snow depth
      DO ji = kideb, kiut
         dh_s_tot(ji)   =  zdh_s_mel(ji) + zdh_s_pre(ji)
         ! Old and new snow depths
         zhsold(ji)     =  ht_s_b(ji)
         zhsnew         =  ht_s_b(ji) + dh_s_tot(ji)
         ! If snow is still present zhn = 1, else zhn = 0
         zhn            =  1.0 - MAX( zzero , SIGN( zone , - zhsnew ) )
         ht_s_b(ji)     =  MAX( zzero , zhsnew )
         ! Volume and mass variations of snow
         dvsbq_1d  (ji) =  a_i_b(ji) * ( ht_s_b(ji) - zhsold(ji) - zdh_s_mel(ji) )
         dvsbq_1d  (ji) =  MIN( zzero, dvsbq_1d(ji) )
         rdmsnif_1d(ji) =  rdmsnif_1d(ji) + rhosn * dvsbq_1d(ji)
      END DO ! ji

      !--------------------------
      ! 3.2 Surface ice ablation 
      !--------------------------
      DO ji = kideb, kiut 
         dh_i_surf(ji) =  0._wp
         z_f_surf (ji) =  zqfont_su(ji) / rdt_ice ! heat conservation test
         zdq_i    (ji) =  0._wp
      END DO ! ji

      DO jk = 1, nlay_i
         DO ji = kideb, kiut 
            !                                                    ! melt of layer jk
            zdeltah  (ji,jk) = - zqfont_su(ji) / q_i_b(ji,jk)
            !                                                    ! recompute heat available
            zqfont_su(ji)    = MAX( 0.0 , - zh_i(ji) - zdeltah(ji,jk) ) * q_i_b(ji,jk) 
            !                                                    ! melt of layer jk cannot be higher than its thickness
            zdeltah  (ji,jk) = MAX( zdeltah(ji,jk) , - zh_i(ji) )
            !                                                    ! update surface melt
            dh_i_surf(ji)    = dh_i_surf(ji) + zdeltah(ji,jk) 
            !                                                    ! for energy conservation
            zdq_i    (ji)    = zdq_i(ji) + zdeltah(ji,jk) * q_i_b(ji,jk) / rdt_ice
            !
            ! contribution to ice-ocean salt flux 
            zji = MOD( npb(ji) - 1 , jpi ) + 1
            zjj =    ( npb(ji) - 1 ) / jpi + 1
            zfsalt_melt(ji) = zfsalt_melt(ji) + ( sss_m(zji,zjj) - sm_i_b(ji) ) * a_i_b(ji)    &
               &                              * MIN( zdeltah(ji,jk) , 0.e0 ) * rhoic / rdt_ice 
         END DO
      END DO

      !                     !-------------------
      IF( con_i ) THEN      ! Conservation test
         !                  !-------------------
         numce_dh  = 0
         meance_dh = 0._wp
         DO ji = kideb, kiut
            IF ( ( z_f_surf(ji) + zdq_i(ji) ) .GE. 1.0e-3 ) THEN
               numce_dh  = numce_dh + 1
               meance_dh = meance_dh + z_f_surf(ji) + zdq_i(ji)
            ENDIF
            IF( z_f_surf(ji) + zdq_i(ji) .GE. 1.0e-3  ) THEN!
               WRITE(numout,*) ' ALERTE heat loss for surface melt '
               WRITE(numout,*) ' zji, zjj, jl :', zji, zjj, jl
               WRITE(numout,*) ' ht_i_b       : ', ht_i_b(ji)
               WRITE(numout,*) ' z_f_surf     : ', z_f_surf(ji)
               WRITE(numout,*) ' zdq_i        : ', zdq_i(ji)
               WRITE(numout,*) ' ht_i_b       : ', ht_i_b(ji)
               WRITE(numout,*) ' fc_bo_i      : ', fc_bo_i(ji)
               WRITE(numout,*) ' fbif_1d      : ', fbif_1d(ji)
               WRITE(numout,*) ' qlbbq_1d     : ', qlbbq_1d(ji)
               WRITE(numout,*) ' s_i_new      : ', s_i_new(ji)
               WRITE(numout,*) ' sss_m        : ', sss_m(zji,zjj)
            ENDIF
         END DO
         !
         IF( numce_dh > 0 )   meance_dh = meance_dh / numce_dh
         WRITE(numout,*) ' Error report - Category : ', jl
         WRITE(numout,*) ' ~~~~~~~~~~~~ '
         WRITE(numout,*) ' Number of points where there is sur. me. error : ', numce_dh
         WRITE(numout,*) ' Mean basal growth error on error points : ', meance_dh
         !
      ENDIF

      !----------------------
      ! 3.3 Snow sublimation
      !----------------------

      DO ji = kideb, kiut
         ! if qla is positive (upwards), heat goes to the atmosphere, therefore
         ! snow sublimates, if qla is negative (downwards), snow condensates
         zdh_s_sub(ji)    =  - parsub * qla_ice_1d(ji) / ( rhosn * lsub ) * rdt_ice
         dh_s_tot (ji)    =  dh_s_tot(ji) + zdh_s_sub(ji)
         zdhcf            =  ht_s_b(ji) + zdh_s_sub(ji) 
         ht_s_b   (ji)    =  MAX( zzero , zdhcf )
         ! we recompute dh_s_tot 
         dh_s_tot (ji)    =  ht_s_b(ji) - zhsold(ji)
         qt_s_in  (ji,jl) =  qt_s_in(ji,jl) + zdh_s_sub(ji)*q_s_b(ji,1)
      END DO

      zqt_dummy(:) = 0.e0
      DO jk = 1, nlay_s
         DO ji = kideb,kiut
            q_s_b    (ji,jk) = rhosn * ( cpic * ( rtt - t_s_b(ji,jk) ) + lfus )
            zqt_dummy(ji)    =  zqt_dummy(ji) + q_s_b(ji,jk) * ht_s_b(ji) / nlay_s            ! heat conservation
         END DO
      END DO

      DO jk = 1, nlay_s
         DO ji = kideb, kiut
            ! In case of disparition of the snow, we have to update the snow temperatures
            zhisn  =  MAX( zzero , SIGN( zone, - ht_s_b(ji) ) )
            t_s_b(ji,jk) = ( 1.0 - zhisn ) * t_s_b(ji,jk) + zhisn * rtt
            q_s_b(ji,jk) = ( 1.0 - zhisn ) * q_s_b(ji,jk)
         END DO
      END DO

      !
      !------------------------------------------------------------------------------!
      ! 4) Basal growth / melt                                                       !
      !------------------------------------------------------------------------------!
      !
      ! Ice basal growth / melt is given by the ratio of heat budget over basal
      ! ice heat content.  Basal heat budget is given by the difference between
      ! the inner conductive flux  (fc_bo_i), from the open water heat flux 
      ! (qlbbqb) and the turbulent ocean flux (fbif). 
      ! fc_bo_i is positive downwards. fbif and qlbbq are positive to the ice 

      !-----------------------------------------------------
      ! 4.1 Basal growth - (a) salinity not varying in time 
      !-----------------------------------------------------
      IF(  num_sal /= 2  .AND.  num_sal /= 4  ) THEN
         DO ji = kideb, kiut
            IF(  ( fc_bo_i(ji) + fbif_1d(ji) + qlbbq_1d(ji) ) < 0.0  ) THEN
               s_i_new(ji)         =  sm_i_b(ji)
               ! Melting point in K
               ztmelts             =  - tmut * s_i_new(ji) + rtt 
               ! New ice heat content (Bitz and Lipscomb, 1999)
               ztform              =  t_i_b(ji,nlay_i)  ! t_bo_b crashes in the
               ! Baltic
               q_i_b(ji,nlay_i+1)  = rhoic * (  cpic * ( ztmelts - ztform )                                &
                  &                           + lfus * (  1.0 - ( ztmelts - rtt ) / ( ztform - rtt )  )    &
                  &                           - rcp  * ( ztmelts - rtt )                                 )
               ! Basal growth rate = - F*dt / q
               dh_i_bott(ji)       =  - rdt_ice*( fc_bo_i(ji) + fbif_1d(ji) + qlbbq_1d(ji) ) / q_i_b(ji,nlay_i+1) 
            ENDIF
         END DO
      ENDIF

      !-------------------------------------------------
      ! 4.1 Basal growth - (b) salinity varying in time 
      !-------------------------------------------------
      IF(  num_sal == 2 .OR.  num_sal == 4  ) THEN
         ! the growth rate (dh_i_bott) is function of the new ice
         ! heat content (q_i_b(nlay_i+1)). q_i_b depends on the new ice 
         ! salinity (snewice). snewice depends on dh_i_bott
         ! it converges quickly, so, no problem
         ! See Vancoppenolle et al., OM08 for more info on this

         ! Initial value (tested 1D, can be anything between 1 and 20)
         num_iter_max = 4
         s_i_new(:)   = 4.0

         ! Iterative procedure
         DO iter = 1, num_iter_max
            DO ji = kideb, kiut
               IF(  fc_bo_i(ji) + fbif_1d(ji) + qlbbq_1d(ji) < 0.e0  ) THEN
                  zji = MOD( npb(ji) - 1, jpi ) + 1
                  zjj = ( npb(ji) - 1 ) / jpi + 1
                  ! Melting point in K
                  ztmelts             =   - tmut * s_i_new(ji) + rtt 
                  ! New ice heat content (Bitz and Lipscomb, 1999)
                  q_i_b(ji,nlay_i+1)  =  rhoic * (  cpic * ( ztmelts - t_bo_b(ji) )                             &
                     &                            + lfus * ( 1.0 - ( ztmelts - rtt ) / ( t_bo_b(ji) - rtt ) )   &
                     &                            - rcp * ( ztmelts-rtt )                                     )
                  ! Bottom growth rate = - F*dt / q
                  dh_i_bott(ji) =  - rdt_ice * ( fc_bo_i(ji) + fbif_1d(ji) + qlbbq_1d(ji) ) / q_i_b(ji,nlay_i+1)
                  ! New ice salinity ( Cox and Weeks, JGR, 1988 )
                  ! zswi2  (1) if dh_i_bott/rdt .GT. 3.6e-7
                  ! zswi12 (1) if dh_i_bott/rdt .LT. 3.6e-7 and .GT. 2.0e-8
                  ! zswi1  (1) if dh_i_bott/rdt .LT. 2.0e-8
                  zgrr   = MIN( 1.0e-3, MAX ( dh_i_bott(ji) / rdt_ice , epsi13 ) )
                  zswi2  = MAX( zzero , SIGN( zone , zgrr - 3.6e-7 ) ) 
                  zswi12 = MAX( zzero , SIGN( zone , zgrr - 2.0e-8 ) ) * ( 1.0 - zswi2 )
                  zswi1  = 1. - zswi2 * zswi12 
                  zfracs = zswi1  * 0.12 + zswi12 * ( 0.8925 + 0.0568 * LOG( 100.0 * zgrr ) )   &
                     &                   + zswi2  * 0.26 / ( 0.26 + 0.74 * EXP ( - 724300.0 * zgrr ) ) 
                  zds         = zfracs * sss_m(zji,zjj) - s_i_new(ji)
                  s_i_new(ji) = zfracs * sss_m(zji,zjj)
               ENDIF ! fc_bo_i
            END DO ! ji
         END DO ! iter

         ! Final values
         DO ji = kideb, kiut
            IF( ( fc_bo_i(ji) + fbif_1d(ji) + qlbbq_1d(ji) ) .LT. 0.0  ) THEN
               ! New ice salinity must not exceed 15 psu
               s_i_new(ji) = MIN( s_i_new(ji), s_i_max )
               ! Metling point in K
               ztmelts     =   - tmut * s_i_new(ji) + rtt 
               ! New ice heat content (Bitz and Lipscomb, 1999)
               q_i_b(ji,nlay_i+1)  =  rhoic * (  cpic * ( ztmelts - t_bo_b(ji) )                              &
                  &                            + lfus * ( 1.0 - ( ztmelts - rtt ) / ( t_bo_b(ji) - rtt ) )    &
                  &                            - rcp * ( ztmelts - rtt )                                    )
               ! Basal growth rate = - F*dt / q
               dh_i_bott(ji)       =  - rdt_ice*( fc_bo_i(ji) + fbif_1d(ji) + qlbbq_1d(ji) ) / q_i_b(ji,nlay_i+1) 
               ! Salinity update
               ! entrapment during bottom growth
               dsm_i_se_1d(ji) = ( s_i_new(ji) * dh_i_bott(ji) + sm_i_b(ji) * ht_i_b(ji) )    &
                  &            / MAX( ht_i_b(ji) + dh_i_bott(ji) ,epsi13 ) - sm_i_b(ji)
            ENDIF ! heat budget
         END DO
      ENDIF

      !----------------
      ! 4.2 Basal melt
      !----------------
      meance_dh = 0._wp
      numce_dh  = 0
      zinnermelt(:) = 0._wp

      DO ji = kideb, kiut
         ! heat convergence at the surface > 0
         IF(  ( fc_bo_i(ji) + fbif_1d(ji) + qlbbq_1d(ji) ) >= 0._wp  ) THEN
            s_i_new(ji)   =  s_i_b(ji,nlay_i)
            zqfont_bo(ji) =  rdt_ice * ( fc_bo_i(ji) + fbif_1d(ji) + qlbbq_1d(ji) )
            zfbase(ji)    =  zqfont_bo(ji) / rdt_ice     ! heat conservation test
            zdq_i(ji)     =  0._wp
            dh_i_bott(ji) =  0._wp
         ENDIF
      END DO

      DO jk = nlay_i, 1, -1
         DO ji = kideb, kiut
            IF (  ( fc_bo_i(ji) + fbif_1d(ji) + qlbbq_1d(ji) ) .GE. 0.0  ) THEN
               ztmelts            =   - tmut * s_i_b(ji,jk) + rtt 
               IF( t_i_b(ji,jk) >= ztmelts ) THEN
                  zdeltah(ji,jk)  = - zh_i(ji)
                  dh_i_bott(ji)   = dh_i_bott(ji) + zdeltah(ji,jk)
                  zinnermelt(ji)   = 1._wp
               ELSE  ! normal ablation
                  zdeltah(ji,jk)  = - zqfont_bo(ji) / q_i_b(ji,jk)
                  zqfont_bo(ji)   = MAX( 0.0 , - zh_i(ji) - zdeltah(ji,jk) ) * q_i_b(ji,jk)
                  zdeltah(ji,jk)  = MAX(zdeltah(ji,jk), - zh_i(ji) )
                  dh_i_bott(ji)   = dh_i_bott(ji) + zdeltah(ji,jk)
                  zdq_i(ji)       = zdq_i(ji) + zdeltah(ji,jk) * q_i_b(ji,jk) / rdt_ice
                  ! contribution to salt flux
                  zji             = MOD( npb(ji) - 1, jpi ) + 1
                  zjj             = ( npb(ji) - 1 ) / jpi + 1
                  zfsalt_melt(ji) = zfsalt_melt(ji) + ( sss_m(zji,zjj) - sm_i_b(ji)   ) * a_i_b(ji)   &
                     &                              * MIN( zdeltah(ji,jk) , 0.0 ) * rhoic / rdt_ice 
               ENDIF
            ENDIF
         END DO ! ji
      END DO ! jk

      !                     !-------------------
      IF( con_i ) THEN      ! Conservation test
      !                     !-------------------
         DO ji = kideb, kiut
            IF(  ( fc_bo_i(ji) + fbif_1d(ji) + qlbbq_1d(ji) ) >= 0.e0  ) THEN
               IF( ( zfbase(ji) + zdq_i(ji) ) >= 1.e-3 ) THEN
                  numce_dh  = numce_dh + 1
                  meance_dh = meance_dh + zfbase(ji) + zdq_i(ji)
               ENDIF
               IF ( zfbase(ji) + zdq_i(ji) .GE. 1.0e-3  ) THEN
                  WRITE(numout,*) ' ALERTE heat loss for basal melt : zji, zjj, jl :', zji, zjj, jl
                  WRITE(numout,*) ' ht_i_b    : ', ht_i_b(ji)
                  WRITE(numout,*) ' zfbase    : ', zfbase(ji)
                  WRITE(numout,*) ' zdq_i     : ', zdq_i(ji)
                  WRITE(numout,*) ' ht_i_b    : ', ht_i_b(ji)
                  WRITE(numout,*) ' fc_bo_i   : ', fc_bo_i(ji)
                  WRITE(numout,*) ' fbif_1d   : ', fbif_1d(ji)
                  WRITE(numout,*) ' qlbbq_1d  : ', qlbbq_1d(ji)
                  WRITE(numout,*) ' s_i_new   : ', s_i_new(ji)
                  WRITE(numout,*) ' sss_m     : ', sss_m(zji,zjj)
                  WRITE(numout,*) ' dh_i_bott : ', dh_i_bott(ji)
                  WRITE(numout,*) ' innermelt : ', INT( zinnermelt(ji) )
               ENDIF
            ENDIF
         END DO
         IF( numce_dh > 0 )   meance_dh = meance_dh / numce_dh
         WRITE(numout,*) ' Number of points where there is bas. me. error : ', numce_dh
         WRITE(numout,*) ' Mean basal melt error on error points : ', meance_dh
         WRITE(numout,*) ' Remaining bottom heat : ', zqfont_bo(jiindex_1d)
         !
      ENDIF

      !
      !------------------------------------------------------------------------------!
      !  5) Pathological cases                                                       !
      !------------------------------------------------------------------------------!
      !
      !----------------------------------------------
      ! 5.1 Excessive ablation in a 1-category model
      !----------------------------------------------

      DO ji = kideb, kiut
         !                     ! in a 1-category sea ice model, bottom ablation must not exceed hmelt (-0.15)
         IF( jpl == 1 ) THEN   ;   zdhbf = MAX( hmelt , dh_i_bott(ji) )
         ELSE                  ;   zdhbf =              dh_i_bott(ji) 
         ENDIF
         !                     ! excessive energy is sent to lateral ablation
         fsup     (ji) =  rhoic * lfus * at_i_b(ji) / MAX( 1.0 - at_i_b(ji) , epsi13 )   &
            &                          * ( zdhbf - dh_i_bott(ji) ) / rdt_ice
         dh_i_bott(ji)  = zdhbf
         !                     !since ice volume is only used for outputs, we keep it global for all categories
         dvbbq_1d (ji) = a_i_b(ji) * dh_i_bott(ji)
         !                     !new ice thickness
         zhgnew   (ji) = ht_i_b(ji) + dh_i_surf(ji) + dh_i_bott(ji)
         !                     ! diagnostic ( bottom ice growth )
         zji = MOD( npb(ji) - 1, jpi ) + 1
         zjj = ( npb(ji) - 1 ) / jpi + 1
         diag_bot_gr(zji,zjj) = diag_bot_gr(zji,zjj) + MAX(dh_i_bott(ji),0.0)*a_i_b(ji) / rdt_ice
         diag_sur_me(zji,zjj) = diag_sur_me(zji,zjj) + MIN(dh_i_surf(ji),0.0)*a_i_b(ji) / rdt_ice
         diag_bot_me(zji,zjj) = diag_bot_me(zji,zjj) + MIN(dh_i_bott(ji),0.0)*a_i_b(ji) / rdt_ice
      END DO

      !-----------------------------------
      ! 5.2 More than available ice melts
      !-----------------------------------
      ! then heat applied minus heat content at previous time step
      ! should equal heat remaining 
      !
      DO ji = kideb, kiut
         ! Adapt the remaining energy if too much ice melts
         !--------------------------------------------------
         zihgnew    =  1.0 - MAX( zzero , SIGN( zone , - zhgnew(ji) ) ) !1 if ice
         ! 0 if no more ice
         zhgnew    (ji) =         zihgnew   * zhgnew(ji)      ! ice thickness is put to 0
         ! remaining heat
         zfdt_final(ji) = ( 1.0 - zihgnew ) * ( zqfont_su(ji) +  zqfont_bo(ji) ) 

         ! If snow remains, energy is used to melt snow
         zhni =  ht_s_b(ji)      ! snow depth at previous time step
         zihg =  MAX( zzero , SIGN ( zone , - ht_s_b(ji) ) ) ! 0 if snow 

         ! energy of melting of remaining snow
         zqt_s(ji) =    ( 1. - zihg ) * zqt_s(ji) / MAX( zhni, epsi13 )
         zdhnm     =  - ( 1. - zihg ) * ( 1. - zihgnew ) * zfdt_final(ji) / MAX( zqt_s(ji) , epsi13 )
         zhnfi          =  zhni + zdhnm
         zfdt_final(ji) =  MAX( zfdt_final(ji) + zqt_s(ji) * zdhnm , 0.0 )
         ht_s_b(ji)     =  MAX( zzero , zhnfi )
         zqt_s(ji)      =  zqt_s(ji) * ht_s_b(ji)

         ! Mass variations of ice and snow
         !---------------------------------
         !                                              ! mass variation of the jl category
         zzfmass_s = - a_i_b(ji) * ( zhni       - ht_s_b(ji) ) * rhosn   ! snow
         zzfmass_i =   a_i_b(ji) * ( zhgnew(ji) - ht_i_b(ji) ) * rhoic   ! ice  
         !
         zfmass_i(ji) = zzfmass_i                       ! ice variation saved to compute salt flux (see below)
         !
         !                                              ! mass variation cumulated over category
         rdmsnif_1d(ji) = rdmsnif_1d(ji) + zzfmass_s                     ! snow 
         rdmicif_1d(ji) = rdmicif_1d(ji) + zzfmass_i                     ! ice 

         ! Remaining heat to the ocean 
         !---------------------------------
         focea(ji)  = - zfdt_final(ji) / rdt_ice         ! focea is in W.m-2 * dt

      END DO

      ftotal_fin (:) = zfdt_final(:)  / rdt_ice

      !---------------------------
      ! Salt flux and heat fluxes                    
      !---------------------------
      DO ji = kideb, kiut
         zihgnew    =  1.0 - MAX( zzero , SIGN( zone , - zhgnew(ji) ) )   !1 if ice

         ! Salt flux
         zji = MOD( npb(ji) - 1, jpi ) + 1
         zjj = ( npb(ji) - 1 ) / jpi + 1
         ! new lines
         IF( num_sal == 4 ) THEN
            fseqv_1d(ji) = fseqv_1d(ji) +        zihgnew  * zfsalt_melt(ji)                                &
               &                        + (1.0 - zihgnew) * zfmass_i(ji) * ( sss_m(zji,zjj) - bulk_sal   ) / rdt_ice
         ELSE
            fseqv_1d(ji) = fseqv_1d(ji) +        zihgnew  * zfsalt_melt(ji)                                &
               &                        + (1.0 - zihgnew) * zfmass_i(ji) * ( sss_m(zji,zjj) - sm_i_b(ji) ) / rdt_ice
         ENDIF
         ! Heat flux
         ! excessive bottom ablation energy (fsup) - 0 except if jpl = 1
         ! excessive total ablation energy (focea) sent to the ocean
         qfvbq_1d(ji)  = qfvbq_1d(ji) + fsup(ji) + ( 1.0 - zihgnew ) * focea(ji) * a_i_b(ji) * rdt_ice

         zihic   = 1.0 - MAX( zzero , SIGN( zone , -ht_i_b(ji) ) )
         ! equals 0 if ht_i = 0, 1 if ht_i gt 0
         fscbq_1d(ji) =  a_i_b(ji) * fstbif_1d(ji)
         qldif_1d(ji)  = qldif_1d(ji) + fsup(ji) + ( 1.0 - zihgnew ) * focea(ji)    * a_i_b(ji) * rdt_ice   &
            &                                    + ( 1.0 - zihic   ) * fscbq_1d(ji)             * rdt_ice
      END DO  ! ji

      !-------------------------------------------
      ! Correct temperature, energy and thickness
      !-------------------------------------------
      DO ji = kideb, kiut
         zihgnew    =  1.0 - MAX( zzero , SIGN( zone , - zhgnew(ji) ) ) 
         t_su_b(ji) =  zihgnew * t_su_b(ji) + ( 1.0 - zihgnew ) * rtt
      END DO  ! ji

      DO jk = 1, nlay_i
         DO ji = kideb, kiut
            zihgnew      =  1.0 - MAX( zzero , SIGN( zone , - zhgnew(ji) ) ) 
            t_i_b(ji,jk) =  zihgnew * t_i_b(ji,jk) + ( 1.0 - zihgnew ) * rtt
            q_i_b(ji,jk) =  zihgnew * q_i_b(ji,jk)
         END DO
      END DO  ! ji

      DO ji = kideb, kiut
         ht_i_b(ji) = zhgnew(ji)
      END DO  ! ji
      !
      !------------------------------------------------------------------------------|
      !  6) Snow-Ice formation                                                       |
      !------------------------------------------------------------------------------|
      ! When snow load excesses Archimede's limit, snow-ice interface goes down under sea-level, 
      ! flooding of seawater transforms snow into ice dh_snowice is positive for the ice
      DO ji = kideb, kiut
         !
         dh_snowice(ji) = MAX(  zzero , ( rhosn * ht_s_b(ji) + (rhoic-rau0) * ht_i_b(ji) ) / ( rhosn+rau0-rhoic )  )
         zhgnew(ji)     = MAX(  zhgnew(ji) , zhgnew(ji) + dh_snowice(ji)  )
         zhnnew         = MIN(  ht_s_b(ji) , ht_s_b(ji) - dh_snowice(ji)  )

         !  Changes in ice volume and ice mass.
         dvnbq_1d  (ji) =                a_i_b(ji) * ( zhgnew(ji)-ht_i_b(ji) )
         dmgwi_1d  (ji) = dmgwi_1d(ji) + a_i_b(ji) * ( ht_s_b(ji) - zhnnew ) * rhosn

         rdmicif_1d(ji) = rdmicif_1d(ji) + a_i_b(ji) * ( zhgnew(ji) - ht_i_b(ji) ) * rhoic 
         rdmsnif_1d(ji) = rdmsnif_1d(ji) + a_i_b(ji) * ( zhnnew     - ht_s_b(ji) ) * rhosn

         !        Equivalent salt flux (1) Snow-ice formation component
         !        -----------------------------------------------------
         zji = MOD( npb(ji) - 1, jpi ) + 1
         zjj =    ( npb(ji) - 1 ) / jpi + 1

         IF( num_sal /= 2 ) THEN   ;   zsm_snowice = sm_i_b(ji)
         ELSE                      ;   zsm_snowice = ( rhoic - rhosn ) / rhoic * sss_m(zji,zjj) 
         ENDIF
         IF( num_sal == 4 ) THEN
            fseqv_1d(ji) = fseqv_1d(ji) + ( sss_m(zji,zjj) - bulk_sal    ) * a_i_b(ji)   &
               &                        * ( zhgnew(ji) - ht_i_b(ji) ) * rhoic / rdt_ice
         ELSE
            fseqv_1d(ji) = fseqv_1d(ji) + ( sss_m(zji,zjj) - zsm_snowice ) * a_i_b(ji)   &
               &                        * ( zhgnew(ji) - ht_i_b(ji) ) * rhoic / rdt_ice
         ENDIF
         ! entrapment during snow ice formation
         i_ice_switch = 1.0 - MAX( 0.e0 , SIGN( 1.0 , - ht_i_b(ji) + 1.0e-6 ) )
         isnowic      = 1.0 - MAX( 0.e0 , SIGN( 1.0 , - dh_snowice(ji)      ) ) * i_ice_switch
         IF(  num_sal == 2  .OR.  num_sal == 4  )   &
            dsm_i_si_1d(ji) = ( zsm_snowice*dh_snowice(ji) &
            &               + sm_i_b(ji) * ht_i_b(ji) / MAX( ht_i_b(ji) + dh_snowice(ji), epsi13)   &
            &               - sm_i_b(ji) ) * isnowic     

         !  Actualize new snow and ice thickness.
         ht_s_b(ji)  = zhnnew
         ht_i_b(ji)  = zhgnew(ji)

         ! Total ablation ! new lines added to debug
         IF( ht_i_b(ji) <= 0._wp )   a_i_b(ji) = 0._wp

         ! diagnostic ( snow ice growth )
         zji = MOD( npb(ji) - 1, jpi ) + 1
         zjj =    ( npb(ji) - 1 ) / jpi + 1
         diag_sni_gr(zji,zjj)  = diag_sni_gr(zji,zjj) + dh_snowice(ji)*a_i_b(ji) / rdt_ice
         !
      END DO !ji
      !
      CALL wrk_dealloc( jpij, zh_i, zh_s, ztfs, zhsold, zqprec, zqfont_su, zqfont_bo, z_f_surf, zhgnew, zfmass_i )
      CALL wrk_dealloc( jpij, zdh_s_mel, zdh_s_pre, zdh_s_sub, zfsalt_melt, zfdt_init, zfdt_final, zqt_i, zqt_s, zqt_dummy )
      CALL wrk_dealloc( jpij, zinnermelt, zfbase, zdq_i )
      CALL wrk_dealloc( jpij, jkmax, zdeltah, zqt_i_lay )
      !
   END SUBROUTINE lim_thd_dh
   
#else
   !!----------------------------------------------------------------------
   !!   Default option                               NO  LIM3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_thd_dh          ! Empty routine
   END SUBROUTINE lim_thd_dh
#endif

   !!======================================================================
END MODULE limthd_dh
