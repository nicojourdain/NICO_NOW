MODULE limthd_ent
   !!======================================================================
   !!                       ***  MODULE limthd_ent   ***
   !!                  Redistribution of Enthalpy in the ice
   !!                        on the new vertical grid
   !!                       after vertical growth/decay
   !!======================================================================
   !! History :  LIM  ! 2003-05 (M. Vancoppenolle) Original code in 1D
   !!                 ! 2005-07 (M. Vancoppenolle) 3D version 
   !!                 ! 2006-11 (X. Fettweis) Vectorized 
   !!            3.0  ! 2008-03 (M. Vancoppenolle) Energy conservation and clean code
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_thd_ent : ice redistribution of enthalpy
   !!----------------------------------------------------------------------
   USE par_oce          ! ocean parameters
   USE dom_oce          ! domain variables
   USE domain           !
   USE phycst           ! physical constants
   USE ice              ! LIM variables
   USE par_ice          ! LIM parameters
   USE thd_ice          ! LIM thermodynamics
   USE limvar           ! LIM variables
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_thd_ent         ! called by lim_thd

   REAL(wp) ::   epsi20 = 1e-20_wp   ! constant values
   REAL(wp) ::   epsi13 = 1e-13_wp   !
   REAL(wp) ::   epsi10 = 1e-10_wp   !
   REAL(wp) ::   epsi06 = 1e-06_wp   !
   REAL(wp) ::   zzero  = 0._wp      !
   REAL(wp) ::   zone   = 1._wp      !

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limthd_ent.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS
 
   SUBROUTINE lim_thd_ent( kideb, kiut, jl )
      !!-------------------------------------------------------------------
      !!               ***   ROUTINE lim_thd_ent  ***
      !!
      !! ** Purpose :
      !!           This routine computes new vertical grids 
      !!           in the ice and in the snow, and consistently redistributes 
      !!           temperatures in the snow / ice. 
      !!           Redistribution is made so as to ensure to energy conservation
      !!
      !!
      !! ** Method  : linear conservative remapping
      !!           
      !! ** Steps : 1) Grid
      !!            2) Switches
      !!            3) Snow redistribution
      !!            4) Ice enthalpy redistribution
      !!            5) Ice salinity, recover temperature
      !!
      !! References : Bitz & Lipscomb, JGR 99; Vancoppenolle et al., GRL, 2005
      !!-------------------------------------------------------------------
      INTEGER , INTENT(in) ::   kideb, kiut   ! Start/End point on which the  the computation is applied
      INTEGER , INTENT(in) ::   jl            ! Thickness cateogry number

      INTEGER ::   ji,jk   !  dummy loop indices
      INTEGER ::   zji, zjj       ,   &  !  dummy indices
         ntop0          ,   &  !  old layer top index
         nbot1          ,   &  !  new layer bottom index
         ntop1          ,   &  !  new layer top index
         limsum         ,   &  !  temporary loop index
         nlayi0,nlays0  ,   &  !  old number of layers
         maxnbot0       ,   &  !  old layer bottom index
         layer0, layer1        !  old/new layer indexes


      REAL(wp) :: &
         ztmelts        ,   &  ! ice melting point
         zqsnic         ,   &  ! enthalpy of snow ice layer
         zhsnow         ,   &  ! temporary snow thickness variable
         zswitch        ,   &  ! dummy switch argument
         zfac1          ,   &  ! dummy factor
         zfac2          ,   &  ! dummy factor
         ztform         ,   &  !: bottom formation temperature
         zaaa           ,   &  !: dummy factor
         zbbb           ,   &  !: dummy factor
         zccc           ,   &  !: dummy factor
         zdiscrim              !: dummy factor

      INTEGER, POINTER, DIMENSION(:) ::   snswi     !  snow switch
      INTEGER, POINTER, DIMENSION(:) ::   nbot0     !  old layer bottom index
      INTEGER, POINTER, DIMENSION(:) ::   icsuind   !  ice surface index
      INTEGER, POINTER, DIMENSION(:) ::   icsuswi   !  ice surface switch
      INTEGER, POINTER, DIMENSION(:) ::   icboind   !  ice bottom index
      INTEGER, POINTER, DIMENSION(:) ::   icboswi   !  ice bottom switch
      INTEGER, POINTER, DIMENSION(:) ::   snicind   !  snow ice index
      INTEGER, POINTER, DIMENSION(:) ::   snicswi   !  snow ice switch
      INTEGER, POINTER, DIMENSION(:) ::   snind     !  snow index
      !
      REAL(wp), POINTER, DIMENSION(:) ::   zh_i   ! thickness of an ice layer
      REAL(wp), POINTER, DIMENSION(:) ::   zh_s          ! thickness of a snow layer
      REAL(wp), POINTER, DIMENSION(:) ::   zqsnow        ! enthalpy of the snow put in snow ice 	
      REAL(wp), POINTER, DIMENSION(:) ::   zdeltah       ! temporary variable
      REAL(wp), POINTER, DIMENSION(:) ::   zqti_in, zqts_in
      REAL(wp), POINTER, DIMENSION(:) ::   zqti_fin, zqts_fin

      REAL(wp), POINTER, DIMENSION(:,:) ::   zm0       !  old layer-system vertical cotes 
      REAL(wp), POINTER, DIMENSION(:,:) ::   qm0       !  old layer-system heat content 
      REAL(wp), POINTER, DIMENSION(:,:) ::   z_s       !  new snow system vertical cotes 
      REAL(wp), POINTER, DIMENSION(:,:) ::   z_i       !  new ice system vertical cotes 
      REAL(wp), POINTER, DIMENSION(:,:) ::   zthick0   !  old ice thickness 
      REAL(wp), POINTER, DIMENSION(:,:) ::   zhl0      ! old and new layer thicknesses 
      REAL(wp), POINTER, DIMENSION(:,:) ::   zrl01
      !!-------------------------------------------------------------------

      CALL wrk_alloc( jpij, snswi, nbot0, icsuind, icsuswi, icboind, icboswi, snicind, snicswi, snind )   ! integer
      CALL wrk_alloc( jpij, zh_i, zh_s, zqsnow, zdeltah, zqti_in, zqts_in, zqti_fin, zqts_fin )           ! real
      CALL wrk_alloc( jpij,jkmax+4, zm0, qm0, z_s, z_i, zthick0, zhl0, kjstart = 0 )
      CALL wrk_alloc( jkmax+4,jkmax+4, zrl01, kistart = 0, kjstart = 0 )

      zthick0(:,:) = 0._wp
      zm0    (:,:) = 0._wp
      qm0    (:,:) = 0._wp
      zrl01  (:,:) = 0._wp
      zhl0   (:,:) = 0._wp
      z_i    (:,:) = 0._wp
      z_s    (:,:) = 0._wp

      !
      !------------------------------------------------------------------------------|
      !  1) Grid                                                                     |
      !------------------------------------------------------------------------------|
      nlays0 = nlay_s
      nlayi0 = nlay_i

      DO ji = kideb, kiut
         zh_i(ji) = old_ht_i_b(ji) / nlay_i 
         zh_s(ji) = old_ht_s_b(ji) / nlay_s
      END DO

      !
      !------------------------------------------------------------------------------|
      !  2) Switches                                                                 |
      !------------------------------------------------------------------------------|
      ! 2.1 snind(ji), snswi(ji)
      ! snow surface behaviour : computation of snind(ji)-snswi(ji)
      ! snind(ji) : index which equals 
      ! 	 0 if snow is accumulating
      ! 	 1 if 1st layer is melting
      ! 	 2 if 2nd layer is melting ...
      DO ji = kideb, kiut
         snind  (ji) = 0
         zdeltah(ji) = 0._wp
      ENDDO !ji

      DO jk = 1, nlays0
         DO ji = kideb, kiut
            snind(ji)  = jk        *      INT(MAX(0.0,SIGN(1.0,-dh_s_tot(ji)-zdeltah(ji)-epsi20))) &
               + snind(ji) * (1 - INT(MAX(0.0,SIGN(1.0,-dh_s_tot(ji)-zdeltah(ji)-epsi20))))
            zdeltah(ji)= zdeltah(ji) + zh_s(ji)
         END DO ! ji
      END DO ! jk

      ! snswi(ji) : switch which value equals 1 if snow melts
      !				   0 if not
      DO ji = kideb, kiut
         snswi(ji)     = MAX(0,INT(-dh_s_tot(ji)/MAX(epsi20,ABS(dh_s_tot(ji)))))
      END DO ! ji

      ! 2.2 icsuind(ji), icsuswi(ji)
      ! ice surface behaviour : computation of icsuind(ji)-icsuswi(ji)
      ! icsuind(ji) : index which equals
      !	   0 if nothing happens at the surface
      !	   1 if first layer is melting
      !	   2 if 2nd layer is reached by melt ...
      DO ji = kideb, kiut
         icsuind(ji) = 0
         zdeltah(ji) = 0._wp
      END DO !ji
      DO jk = 1, nlayi0
         DO ji = kideb, kiut
            icsuind(ji) = jk          *      INT(MAX(0.0,SIGN(1.0,-dh_i_surf(ji)-zdeltah(ji)-epsi20))) &
               + icsuind(ji) * (1 - INT(MAX(0.0,SIGN(1.0,-dh_i_surf(ji)-zdeltah(ji)-epsi20))))
            zdeltah(ji) = zdeltah(ji) + zh_i(ji)
         END DO ! ji
      ENDDO !jk

      ! icsuswi(ji) : switch which equals 
      ! 	   1 if ice melts at the surface
      ! 	   0 if not
      DO ji = kideb, kiut
         icsuswi(ji)  = MAX(0,INT(-dh_i_surf(ji)/MAX(epsi20 , ABS(dh_i_surf(ji)) ) ) )
      ENDDO

      ! 2.3 icboind(ji), icboswi(ji)
      ! ice bottom behaviour : computation of icboind(ji)-icboswi(ji)
      ! icboind(ji) : index which equals
      ! 	   0 if accretion is on the way
      ! 	   1 if last layer has started to melt
      ! 	   2 if penultiem layer is melting ... and so on
      !            N+1 if all layers melt and that snow transforms into ice
      DO ji = kideb, kiut 
         icboind(ji) = 0
         zdeltah(ji) = 0._wp
      END DO
      DO jk = nlayi0, 1, -1
         DO ji = kideb, kiut
            icboind(ji) = (nlayi0+1-jk) *      INT(MAX(0.0,SIGN(1.0,-dh_i_bott(ji)-zdeltah(ji)-epsi20))) &
               &          + icboind(ji) * (1 - INT(MAX(0.0,SIGN(1.0,-dh_i_bott(ji)-zdeltah(ji)-epsi20)))) 
            zdeltah(ji) = zdeltah(ji) + zh_i(ji)
         END DO
      END DO

      DO ji = kideb, kiut
         ! case of total ablation with remaining snow
         IF ( ( ht_i_b(ji) .GT. epsi20 ) .AND. &
            ( ht_i_b(ji) - dh_snowice(ji) .LT. epsi20 ) ) icboind(ji) = nlay_i + 1
      END DO

      ! icboswi(ji) : switch which equals 
      ! 	   1 if ice accretion is on the way
      ! 	   0 if ablation is on the way
      DO ji = kideb, kiut 
         icboswi(ji) = MAX(0,INT(dh_i_bott(ji) / MAX(epsi20,ABS(dh_i_bott(ji)))))
      END DO

      ! 2.4 snicind(ji), snicswi(ji)
      ! snow ice formation : calcul de snicind(ji)-snicswi(ji)
      ! snicind(ji) : index which equals 
      ! 	   0 if no snow-ice forms
      ! 	   1 if last layer of snow has started to melt
      ! 	   2 if penultiem layer ...
      DO ji = kideb, kiut
         snicind(ji) = 0
         zdeltah(ji) = 0._wp
      END DO
      DO jk = nlays0, 1, -1
         DO ji = kideb, kiut
            snicind(ji) = (nlays0+1-jk) &
               *      INT(MAX(0.0,SIGN(1.0,dh_snowice(ji)-zdeltah(ji)-epsi20))) + snicind(ji)   &
               * (1 - INT(MAX(0.0,SIGN(1.0,dh_snowice(ji)-zdeltah(ji)-epsi20))))
            zdeltah(ji) = zdeltah(ji) + zh_s(ji)
         END DO
      END DO

      ! snicswi(ji) : switch which equals 
      ! 	   1 if snow-ice forms
      ! 	   0 if not
      DO ji = kideb, kiut
         snicswi(ji)   = MAX(0,INT(dh_snowice(ji)/MAX(epsi20,ABS(dh_snowice(ji)))))
      ENDDO

      !
      !------------------------------------------------------------------------------|
      !  3) Snow redistribution                                                      |
      !------------------------------------------------------------------------------|
      !
      !-------------
      ! Old profile
      !-------------

      ! by 'old', it is meant that layers coming from accretion are included, 
      ! and that interfacial layers which were partly melted are reduced 

      ! indexes of the vectors
      !------------------------
      ntop0    =  1
      maxnbot0 =  0

      DO ji = kideb, kiut
         nbot0(ji) =  nlays0  + 1 - snind(ji) + ( 1. - snicind(ji) ) * snicswi(ji)
         ! cotes of the top of the layers
         zm0(ji,0) =  0._wp
         maxnbot0 =  MAX ( maxnbot0 , nbot0(ji) )
      END DO
      IF( lk_mpp )   CALL mpp_max( maxnbot0, kcom=ncomm_ice )

      DO jk = 1, maxnbot0
         DO ji = kideb, kiut
            !change
            limsum = ( 1 - snswi(ji) ) * ( jk - 1 ) + snswi(ji) * ( jk + snind(ji) - 1 )
            limsum = MIN( limsum , nlay_s )
            zm0(ji,jk) =  dh_s_tot(ji) + zh_s(ji) * limsum
         END DO
      END DO

      DO ji = kideb, kiut
         zm0(ji,nbot0(ji)) =  dh_s_tot(ji) - snicswi(ji) * dh_snowice(ji) + zh_s(ji) * nlays0
         zm0(ji,1)         =  dh_s_tot(ji) * (1 -snswi(ji) ) + snswi(ji) * zm0(ji,1)
      END DO

      DO jk = ntop0, maxnbot0
         DO ji = kideb, kiut
            zthick0(ji,jk)  =  zm0(ji,jk) - zm0(ji,jk-1)            ! layer thickness
         END DO
      END DO

      zqts_in(:) = 0._wp

      DO ji = kideb, kiut         ! layer heat content
         qm0    (ji,1) =  rhosn * (  cpic * ( rtt - ( 1. - snswi(ji) ) * tatm_ice_1d(ji)        &
            &                                            - snswi(ji)   * t_s_b      (ji,1)  )   &
            &                      + lfus  ) * zthick0(ji,1)
         zqts_in(ji)   =  zqts_in(ji) + qm0(ji,1) 
      END DO

      DO jk = 2, maxnbot0
         DO ji = kideb, kiut
            limsum      = ( 1 - snswi(ji) ) * ( jk - 1 ) + snswi(ji) * ( jk + snind(ji) - 1 )
            limsum      = MIN( limsum , nlay_s )
            qm0(ji,jk)  = rhosn * ( cpic * ( rtt - t_s_b(ji,limsum) ) + lfus ) * zthick0(ji,jk)
            zswitch = 1.0 - MAX (0.0, SIGN ( 1.0, epsi20 - ht_s_b(ji) ) )
            zqts_in(ji) = zqts_in(ji) + ( 1. - snswi(ji) ) * qm0(ji,jk) * zswitch
         END DO ! jk
      END DO ! ji

      !------------------------------------------------
      ! Energy given by the snow in snow-ice formation
      !------------------------------------------------
      ! zqsnow, enthalpy of the flooded snow
      DO ji = kideb, kiut
         zqsnow (ji) =  rhosn * lfus
         zdeltah(ji) =  0._wp
      END DO

      DO jk =  nlays0, 1, -1
         DO ji = kideb, kiut
            zhsnow =  MAX( 0._wp , dh_snowice(ji)-zdeltah(ji) )
            zqsnow (ji) =  zqsnow (ji) + rhosn*cpic*(rtt-t_s_b(ji,jk))
            zdeltah(ji) =  zdeltah(ji) + zh_s(ji)
         END DO
      END DO

      DO ji = kideb, kiut
         zqsnow(ji) = zqsnow(ji) * dh_snowice(ji)
      END DO

      !------------------
      ! new snow profile
      !------------------

      !--------------
      ! Vector index   
      !--------------
      ntop1 =  1
      nbot1 =  nlay_s

      !-------------------
      ! Layer coordinates 
      !-------------------
      DO ji = kideb, kiut
         zh_s(ji)  = ht_s_b(ji) / nlay_s
         z_s(ji,0) =  0._wp
      ENDDO

      DO jk = 1, nlay_s
         DO ji = kideb, kiut
            z_s(ji,jk) =  zh_s(ji) * jk
         END DO
      END DO

      !-----------------
      ! Layer thickness
      !-----------------
      DO layer0 = ntop0, maxnbot0
         DO ji = kideb, kiut
            zhl0(ji,layer0) = zm0(ji,layer0) - zm0(ji,layer0-1)
         END DO
      END DO

      DO layer1 = ntop1, nbot1
         DO ji = kideb, kiut
            q_s_b(ji,layer1) = 0._wp
         END DO
      END DO

      !----------------
      ! Weight factors
      !----------------
      DO layer0 = ntop0, maxnbot0
         DO layer1 = ntop1, nbot1
            DO ji = kideb, kiut
               zrl01(layer1,layer0) = MAX(0.0,( MIN(zm0(ji,layer0),z_s(ji,layer1))   &
                  &                 - MAX(zm0(ji,layer0-1), z_s(ji,layer1-1))) / MAX(zhl0(ji,layer0),epsi10)) 
               q_s_b(ji,layer1) = q_s_b(ji,layer1) + zrl01(layer1,layer0)*qm0(ji,layer0)   &
                  &                                * MAX(0.0,SIGN(1.0,nbot0(ji)-layer0+epsi20))
            END DO
         END DO
      END DO

      ! Heat conservation
      zqts_fin(:) = 0._wp
      DO jk = 1, nlay_s
         DO ji = kideb, kiut
            zqts_fin(ji) = zqts_fin(ji) + q_s_b(ji,jk)
         END DO
      END DO

      IF ( con_i ) THEN
         DO ji = kideb, kiut
            IF ( ABS ( zqts_in(ji) - zqts_fin(ji) ) / rdt_ice .GT. 1.0e-6 ) THEN
               zji                 = MOD( npb(ji) - 1, jpi ) + 1
               zjj                 = ( npb(ji) - 1 ) / jpi + 1
               WRITE(numout,*) ' violation of heat conservation : ',             &
                  ABS ( zqts_in(ji) - zqts_fin(ji) ) / rdt_ice
               WRITE(numout,*) ' ji, jj   : ', zji, zjj
               WRITE(numout,*) ' ht_s_b   : ', ht_s_b(ji)
               WRITE(numout,*) ' zqts_in  : ', zqts_in(ji) / rdt_ice
               WRITE(numout,*) ' zqts_fin : ', zqts_fin(ji) / rdt_ice
               WRITE(numout,*) ' dh_snowice : ', dh_snowice(ji)
               WRITE(numout,*) ' dh_s_tot : ', dh_s_tot(ji)
               WRITE(numout,*) ' snswi    : ', snswi(ji)
            ENDIF
         END DO
      ENDIF

      !---------------------
      ! Recover heat content
      !---------------------
      DO jk = 1, nlay_s
         DO ji = kideb, kiut
            q_s_b(ji,jk) = q_s_b(ji,jk) / MAX( zh_s(ji) , epsi20 )
         END DO !ji
      END DO !jk  

      !---------------------
      ! Recover temperature
      !---------------------
      zfac1 = 1. / ( rhosn * cpic )
      zfac2 = lfus / cpic  
      DO jk = 1, nlay_s
         DO ji = kideb, kiut
            zswitch = MAX ( 0.0 , SIGN ( 1.0, epsi20 - ht_s_b(ji) ) )
            t_s_b(ji,jk) = rtt + ( 1.0 - zswitch ) * ( - zfac1 * q_s_b(ji,jk) + zfac2 )
         END DO
      END DO
      !
      !------------------------------------------------------------------------------|
      !  4) Ice redistribution                                                       |
      !------------------------------------------------------------------------------|
      !
      !-------------
      ! OLD PROFILE 
      !-------------

      !----------------
      ! Vector indexes
      !----------------
      ntop0    =  1
      maxnbot0 =  0

      DO ji = kideb, kiut
         ! reference number of the bottommost layer
         nbot0(ji) =  MAX( 1 ,  MIN( nlayi0 + ( 1 - icboind(ji) ) +        &
            &                           ( 1 - icsuind(ji) ) * icsuswi(ji) + snicswi(ji) , nlay_i + 2 ) )
         ! maximum reference number of the bottommost layer over all domain
         maxnbot0 =  MAX( maxnbot0 , nbot0(ji) )
      END DO

      !-------------------------
      ! Cotes of old ice layers
      !-------------------------
      zm0(:,0) =  0._wp

      DO jk = 1, maxnbot0
         DO ji = kideb, kiut
            ! jk goes from 1 to nbot0
            ! the ice layer number goes from 1 to nlay_i
            ! limsum is the real ice layer number corresponding to present jk
            limsum    =  ( (icsuswi(ji)*(icsuind(ji)+jk-1) + & 
               (1-icsuswi(ji))*jk))*(1-snicswi(ji)) + (jk-1)*snicswi(ji)
            zm0(ji,jk)=  icsuswi(ji)*dh_i_surf(ji) + snicswi(ji)*dh_snowice(ji) &
               +  limsum * zh_i(ji)
         END DO
      END DO

      DO ji = kideb, kiut
         zm0(ji,nbot0(ji)) =  icsuswi(ji)*dh_i_surf(ji) + snicswi(ji)*dh_snowice(ji) + dh_i_bott(ji) &
            +  zh_i(ji) * nlayi0
         zm0(ji,1)         =  snicswi(ji)*dh_snowice(ji) + (1-snicswi(ji))*zm0(ji,1)
      END DO

      !-----------------------------
      ! Thickness of old ice layers
      !-----------------------------
      DO jk = ntop0, maxnbot0
         DO ji = kideb, kiut
            zthick0(ji,jk) =  zm0(ji,jk) - zm0(ji,jk-1)
         END DO
      END DO

      !---------------------------
      ! Inner layers heat content
      !---------------------------
      qm0(:,:) =  0.0
      zqti_in(:) = 0.0

      DO jk = ntop0, maxnbot0
         DO ji = kideb, kiut
            limsum =  MAX(1,MIN(snicswi(ji)*(jk-1) + icsuswi(ji)*(jk-1+icsuind(ji)) + &
               (1-icsuswi(ji))*(1-snicswi(ji))*jk,nlay_i))
            ztmelts = -tmut * s_i_b(ji,limsum) + rtt
            qm0(ji,jk) = rhoic * ( cpic * (ztmelts-t_i_b(ji,limsum)) + lfus * ( 1.0-(ztmelts-rtt)/ &
               MIN((t_i_b(ji,limsum)-rtt),-epsi20) ) - rcp*(ztmelts-rtt) ) &
               * zthick0(ji,jk)
         END DO
      END DO

      !----------------------------
      ! Bottom layers heat content
      !----------------------------
      DO ji = kideb, kiut        
         ztmelts    = ( 1.0 - icboswi(ji) ) * (-tmut * s_i_b  (ji,nlayi0) )   &   ! case of melting ice
            &       +         icboswi(ji)   * (-tmut * s_i_new(ji)        )   &   ! case of forming ice
            &       + rtt                                                         ! in Kelvin

         ! bottom formation temperature
         ztform = t_i_b(ji,nlay_i)
         IF ( ( num_sal .EQ. 2 ) .OR. ( num_sal .EQ. 4 ) ) ztform = t_bo_b(ji)
         qm0(ji,nbot0(ji)) = ( 1.0 - icboswi(ji) )*qm0(ji,nbot0(ji))             &   ! case of melting ice
            &              + icboswi(ji) * rhoic * ( cpic*(ztmelts-ztform)       &   ! case of forming ice
            + lfus *( 1.0-(ztmelts-rtt) / MIN ( (ztform-rtt) , - epsi10 ) )      & 
            - rcp*(ztmelts-rtt) ) * zthick0(ji,nbot0(ji)  )
      END DO

      !-----------------------------
      ! Snow ice layer heat content
      !-----------------------------
      DO ji = kideb, kiut
         ! energy of the flooding seawater
         zqsnic = rau0 * rcp * ( rtt - t_bo_b(ji) ) * dh_snowice(ji) * &
            (rhoic - rhosn) / rhoic * snicswi(ji) ! generally positive
         ! Heat conservation diagnostic
         qt_i_in(ji,jl) = qt_i_in(ji,jl) + zqsnic 

         qldif_1d(ji)   = qldif_1d(ji) + zqsnic * a_i_b(ji)

         ! enthalpy of the newly formed snow-ice layer
         ! = enthalpy of snow + enthalpy of frozen water
         zqsnic         =  zqsnow(ji) + zqsnic
         qm0(ji,1)      =  snicswi(ji) * zqsnic + ( 1 - snicswi(ji) ) * qm0(ji,1)

      END DO ! ji

      DO jk = ntop0, maxnbot0
         DO ji = kideb, kiut
            ! Heat conservation
            zqti_in(ji) = zqti_in(ji) + qm0(ji,jk) * MAX( 0.0 , SIGN(1.0,ht_i_b(ji)-epsi06+epsi20) ) &
               &                                   * MAX( 0.0 , SIGN( 1. , nbot0(ji) - jk + epsi20 ) )
         END DO
      END DO

      !-------------
      ! NEW PROFILE
      !-------------

      !---------------
      ! Vectors index
      !---------------
      ntop1 =  1 
      nbot1 =  nlay_i

      !------------------
      ! Layers thickness 
      !------------------
      DO ji = kideb, kiut
         zh_i(ji) = ht_i_b(ji) / nlay_i
      ENDDO

      !-------------
      ! Layer cotes      
      !-------------
      z_i(:,0) =  0._wp
      DO jk = 1, nlay_i
         DO ji = kideb, kiut
            z_i(ji,jk) =  zh_i(ji) * jk
         END DO
      END DO

      !--thicknesses of the layers
      DO layer0 = ntop0, maxnbot0
         DO ji = kideb, kiut
            zhl0(ji,layer0) = zm0(ji,layer0) - zm0(ji,layer0-1)   ! thicknesses of the layers
         END DO
      END DO

      !------------------------
      ! Weights for relayering
      !------------------------
      q_i_b(:,:) = 0._wp
      DO layer0 = ntop0, maxnbot0
         DO layer1 = ntop1, nbot1
            DO ji = kideb, kiut
               zrl01(layer1,layer0) = MAX(0.0,( MIN(zm0(ji,layer0),z_i(ji,layer1)) &
                  - MAX(zm0(ji,layer0-1), z_i(ji,layer1-1)))/MAX(zhl0(ji,layer0),epsi10))
               q_i_b(ji,layer1) = q_i_b(ji,layer1) & 
                  + zrl01(layer1,layer0)*qm0(ji,layer0) &
                  * MAX(0.0,SIGN(1.0,ht_i_b(ji)-epsi06+epsi20)) &
                  * MAX(0.0,SIGN(1.0,nbot0(ji)-layer0+epsi20))
            END DO
         END DO
      END DO

      !-------------------------
      ! Heat conservation check
      !-------------------------
      zqti_fin(:) = 0._wp
      DO jk = 1, nlay_i
         DO ji = kideb, kiut
            zqti_fin(ji) = zqti_fin(ji) + q_i_b(ji,jk)
         END DO
      END DO
      !
      DO ji = kideb, kiut
         IF ( ABS ( zqti_in(ji) - zqti_fin(ji) ) / rdt_ice .GT. 1.0e-6 ) THEN
            zji                 = MOD( npb(ji) - 1, jpi ) + 1
            zjj                 = ( npb(ji) - 1 ) / jpi + 1
            WRITE(numout,*) ' violation of heat conservation : ', ABS ( zqti_in(ji) - zqti_fin(ji) ) / rdt_ice
            WRITE(numout,*) ' ji, jj   : ', zji, zjj
            WRITE(numout,*) ' ht_i_b   : ', ht_i_b(ji)
            WRITE(numout,*) ' zqti_in  : ', zqti_in(ji) / rdt_ice
            WRITE(numout,*) ' zqti_fin : ', zqti_fin(ji) / rdt_ice
            WRITE(numout,*) ' dh_i_bott: ', dh_i_bott(ji)
            WRITE(numout,*) ' dh_i_surf: ', dh_i_surf(ji)
            WRITE(numout,*) ' dh_snowice:', dh_snowice(ji)
            WRITE(numout,*) ' icsuswi  : ', icsuswi(ji)
            WRITE(numout,*) ' icboswi  : ', icboswi(ji)
            WRITE(numout,*) ' snicswi  : ', snicswi(ji)
         ENDIF
      END DO

      !----------------------
      ! Recover heat content 
      !----------------------
      DO jk = 1, nlay_i
         DO ji = kideb, kiut
            q_i_b(ji,jk) = q_i_b(ji,jk) / MAX( zh_i(ji) , epsi20 )
         END DO !ji
      END DO !jk  

      ! Heat conservation
      zqti_fin(:) = 0.0
      DO jk = 1, nlay_i
         DO ji = kideb, kiut
            zqti_fin(ji) = zqti_fin(ji) + q_i_b(ji,jk) * zh_i(ji)
         END DO
      END DO

      !
      !------------------------------------------------------------------------------|
      !  5) Update salinity and recover temperature                                  |
      !------------------------------------------------------------------------------|
      !
      ! Update salinity (basal entrapment, snow ice formation)
      DO ji = kideb, kiut
         sm_i_b(ji) = sm_i_b(ji) + dsm_i_se_1d(ji) + dsm_i_si_1d(ji)
      END DO !ji

      ! Recover temperature
      DO jk = 1, nlay_i
         DO ji = kideb, kiut
            ztmelts    =  -tmut*s_i_b(ji,jk) + rtt
            !Conversion q(S,T) -> T (second order equation)
            zaaa         =  cpic
            zbbb         =  ( rcp - cpic ) * ( ztmelts - rtt ) + q_i_b(ji,jk) / rhoic - lfus
            zccc         =  lfus * ( ztmelts - rtt )
            zdiscrim     =  SQRT( MAX(zbbb*zbbb - 4.0*zaaa*zccc,0.0) )
            t_i_b(ji,jk) =  rtt - ( zbbb + zdiscrim ) / ( 2.0 *zaaa )
         END DO !ji

      END DO !jk
      !
      CALL wrk_dealloc( jpij, snswi, nbot0, icsuind, icsuswi, icboind, icboswi, snicind, snicswi, snind )   ! integer
      CALL wrk_dealloc( jpij, zh_i, zh_s, zqsnow, zdeltah, zqti_in, zqts_in, zqti_fin, zqts_fin )           ! real
      CALL wrk_dealloc( jpij,jkmax+4, zm0, qm0, z_s, z_i, zthick0, zhl0, kjstart = 0 )
      CALL wrk_dealloc( jkmax+4,jkmax+4, zrl01, kistart = 0, kjstart = 0 )
      !
   END SUBROUTINE lim_thd_ent

#else
   !!----------------------------------------------------------------------
   !!   Default option                               NO  LIM3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_thd_ent          ! Empty routine
   END SUBROUTINE lim_thd_ent
#endif

   !!======================================================================
END MODULE limthd_ent
