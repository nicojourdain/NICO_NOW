MODULE limistate
   !!======================================================================
   !!                     ***  MODULE  limistate  ***
   !!              Initialisation of diagnostics ice variables
   !!======================================================================
   !! History :  2.0  ! 2004-01 (C. Ethe, G. Madec)  Original code
   !!            4.0  ! 2011-02  (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3' :                                    LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_istate      :  Initialisation of diagnostics ice variables
   !!   lim_istate_init :  initialization of ice state and namelist read
   !!----------------------------------------------------------------------
   USE phycst           ! physical constant
   USE oce              ! dynamics and tracers variables
   USE dom_oce          ! ocean domain
   USE sbc_oce          ! Surface boundary condition: ocean fields
   USE eosbn2           ! equation of state
   USE ice              ! sea-ice variables
   USE par_ice          ! ice parameters
   USE dom_ice          ! sea-ice domain
   USE in_out_manager   ! I/O manager
   USE lbclnk           ! lateral boundary condition - MPP exchanges
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_istate      ! routine called by lim_init.F90

   !                                  !!** init namelist (namiceini) **
   REAL(wp) ::   ttest    = 2.0_wp     ! threshold water temperature for initial sea ice
   REAL(wp) ::   hninn    = 0.5_wp     ! initial snow thickness in the north
   REAL(wp) ::   hginn_u  = 2.5_wp     ! initial ice thickness in the north
   REAL(wp) ::   aginn_u  = 0.7_wp     ! initial leads area in the north
   REAL(wp) ::   hginn_d  = 5.0_wp     ! initial ice thickness in the north
   REAL(wp) ::   aginn_d  = 0.25_wp    ! initial leads area in the north
   REAL(wp) ::   hnins    = 0.1_wp     ! initial snow thickness in the south
   REAL(wp) ::   hgins_u  = 1.0_wp     ! initial ice thickness in the south
   REAL(wp) ::   agins_u  = 0.7_wp     ! initial leads area in the south
   REAL(wp) ::   hgins_d  = 2.0_wp     ! initial ice thickness in the south
   REAL(wp) ::   agins_d  = 0.2_wp     ! initial leads area in the south
   REAL(wp) ::   sinn     = 6.301_wp   ! initial salinity 
   REAL(wp) ::   sins     = 6.301_wp   !

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limistate.F90 3349 2012-04-11 08:31:17Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_istate
      !!-------------------------------------------------------------------
      !!                    ***  ROUTINE lim_istate  ***
      !!
      !! ** Purpose :   defined the sea-ice initial state
      !!
      !! ** Method  :   restart from a state defined in a binary file
      !!                or from arbitrary sea-ice conditions
      !!-------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk, jl             ! dummy loop indices
      REAL(wp) ::   zeps6, zeps, ztmelts, epsi06   ! local scalars
      REAL(wp) ::   zvol, zare, zh, zh1, zh2, zh3, zan, zbn, zas, zbs 
      REAL(wp), POINTER, DIMENSION(:)   ::   zgfactorn, zhin 
      REAL(wp), POINTER, DIMENSION(:)   ::   zgfactors, zhis
      REAL(wp), POINTER, DIMENSION(:,:) ::   zidto      ! ice indicator
      !--------------------------------------------------------------------

      CALL wrk_alloc( jpm, zgfactorn, zgfactors, zhin, zhis )
      CALL wrk_alloc( jpi, jpj, zidto )

      !--------------------------------------------------------------------
      ! 1) Preliminary things 
      !--------------------------------------------------------------------
      epsi06 = 1.e-6_wp

      CALL lim_istate_init     !  reading the initials parameters of the ice

!!gm  in lim2  the initialisation if only done if required in the namelist :
!!gm      IF( .NOT. ln_limini ) THEN
!!gm  this should be added in lim3 namelist...

      !--------------------------------------------------------------------
      ! 2) Ice initialization (hi,hs,frld,t_su,sm_i,t_i,t_s)              | 
      !--------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'lim_istate : Ice initialization '
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '

      t_bo(:,:) = tfreez( tsn(:,:,1,jp_sal) ) * tmask(:,:,1)       ! freezing/melting point of sea water [Celcius]

      DO jj = 1, jpj                                       ! ice if sst <= t-freez + ttest
         DO ji = 1, jpi
            IF( tsn(ji,jj,1,jp_tem)  - t_bo(ji,jj) >= ttest ) THEN   ;   zidto(ji,jj) = 0.e0      ! no ice
            ELSE                                                     ;   zidto(ji,jj) = 1.e0      !    ice
            ENDIF
         END DO
      END DO

      t_bo(:,:) = t_bo(:,:) + rt0                          ! t_bo converted from Celsius to Kelvin (rt0 over land)

      ! constants for heat contents
      zeps   = 1.e-20_wp
      zeps6  = 1.e-06_wp

      ! zgfactor for initial ice distribution
      zgfactorn(:) = 0._wp
      zgfactors(:) = 0._wp

      ! first ice type
      DO jl = ice_cat_bounds(1,1), ice_cat_bounds(1,2)
         zhin (1)     = ( hi_max(jl-1) + hi_max(jl) ) * 0.5_wp
         zgfactorn(1) = zgfactorn(1) + exp(-(zhin(1)-hginn_u)*(zhin(1)-hginn_u) * 0.5_wp )
         zhis (1)     = ( hi_max(jl-1) + hi_max(jl) ) * 0.5_wp
         zgfactors(1) = zgfactors(1) + exp(-(zhis(1)-hgins_u)*(zhis(1)-hgins_u) * 0.5_wp )
      END DO ! jl
      zgfactorn(1) = aginn_u / zgfactorn(1)
      zgfactors(1) = agins_u / zgfactors(1)

      ! -------------
      ! new distribution, polynom of second order, conserving area and volume
      zh1 = 0._wp
      zh2 = 0._wp
      zh3 = 0._wp
      DO jl = 1, jpl
         zh = ( hi_max(jl-1) + hi_max(jl) ) * 0.5_wp
         zh1 = zh1 + zh
         zh2 = zh2 + zh * zh
         zh3 = zh3 + zh * zh * zh
      END DO
      IF(lwp) WRITE(numout,*) ' zh1 : ', zh1
      IF(lwp) WRITE(numout,*) ' zh2 : ', zh2
      IF(lwp) WRITE(numout,*) ' zh3 : ', zh3

      zvol = aginn_u * hginn_u
      zare = aginn_u
      IF( jpl >= 2 ) THEN
         zbn = ( zvol*zh2 - zare*zh3 ) / ( zh2*zh2 - zh1*zh3)
         zan = ( zare - zbn*zh1 ) / zh2
      ENDIF

      IF(lwp) WRITE(numout,*) ' zvol: ', zvol
      IF(lwp) WRITE(numout,*) ' zare: ', zare
      IF(lwp) WRITE(numout,*) ' zbn : ', zbn 
      IF(lwp) WRITE(numout,*) ' zan : ', zan 

      zvol = agins_u * hgins_u
      zare = agins_u
      IF( jpl >= 2 ) THEN
         zbs = ( zvol*zh2 - zare*zh3 ) / ( zh2*zh2 - zh1*zh3)
         zas = ( zare - zbs*zh1 ) / zh2
      ENDIF

      IF(lwp) WRITE(numout,*) ' zvol: ', zvol
      IF(lwp) WRITE(numout,*) ' zare: ', zare
      IF(lwp) WRITE(numout,*) ' zbn : ', zbn 
      IF(lwp) WRITE(numout,*) ' zan : ', zan 

      !end of new lines
      ! -------------
!!!
      ! retour a LIMA_MEC
      !     ! second ice type
      !     zdummy  = hi_max(ice_cat_bounds(2,1)-1)
      !     hi_max(ice_cat_bounds(2,1)-1) = 0.0

      !     ! here to change !!!!
      !     jm = 2
      !     DO jl = ice_cat_bounds(jm,1), ice_cat_bounds(jm,2)
      !        zhin (2)     = ( hi_max(jl-1) + hi_max(jl) ) / 2.0
      !        zhin (2)     = ( hi_max_typ(jl-ice_cat_bounds(2,1),jm    ) + &
      !                         hi_max_typ(jl-ice_cat_bounds(2,1) + 1,jm)   ) / 2.0
      !        zgfactorn(2) = zgfactorn(2) + exp(-(zhin(2)-hginn_d)*(zhin(2)-hginn_d)/2.0)
      !        zhis (2)     = ( hi_max(jl-1) + hi_max(jl) ) / 2.0
      !        zhis (2)     = ( hi_max_typ(jl-ice_cat_bounds(2,1),jm    ) + &
      !                         hi_max_typ(jl-ice_cat_bounds(2,1) + 1,jm)   ) / 2.0
      !        zgfactors(2) = zgfactors(2) + exp(-(zhis(2)-hgins_d)*(zhis(2)-hgins_d)/2.0)
      !     END DO ! jl
      !     zgfactorn(2) = aginn_d / zgfactorn(2)
      !     zgfactors(2) = agins_d / zgfactors(2)
      !     hi_max(ice_cat_bounds(2,1)-1) = zdummy
      ! END retour a LIMA_MEC
!!!

!!gm  optimisation :  loop over the ice categories inside the ji, jj loop !!!

      DO jj = 1, jpj
         DO ji = 1, jpi

            !--- Northern hemisphere
            !----------------------------------------------------------------
            IF( fcor(ji,jj) >= 0._wp ) THEN    

               !-----------------------
               ! Ice area / thickness
               !-----------------------

               IF ( jpl .EQ. 1) THEN ! one category

                  DO jl = ice_cat_bounds(1,1), ice_cat_bounds(1,2) ! loop over ice thickness categories
                     a_i(ji,jj,jl)    = zidto(ji,jj) * aginn_u
                     ht_i(ji,jj,jl)   = zidto(ji,jj) * hginn_u
                     v_i(ji,jj,jl)    = ht_i(ji,jj,jl)*a_i(ji,jj,jl)
                  END DO

               ELSE ! several categories

                  DO jl = ice_cat_bounds(1,1), ice_cat_bounds(1,2) ! loop over ice thickness categories
                     zhin(1)          = ( hi_max(jl-1) + hi_max(jl) ) / 2.0
                     a_i(ji,jj,jl)    = zidto(ji,jj) * MAX( zgfactorn(1) * exp(-(zhin(1)-hginn_u)* & 
                        (zhin(1)-hginn_u)/2.0) , epsi06)
                     ! new line
                     a_i(ji,jj,jl)    = zidto(ji,jj) * ( zan * zhin(1) * zhin(1) + zbn * zhin(1) )
                     ht_i(ji,jj,jl)   = zidto(ji,jj) * zhin(1) 
                     v_i(ji,jj,jl)    = ht_i(ji,jj,jl)*a_i(ji,jj,jl)
                  END DO

               ENDIF


!!!
               ! retour a LIMA_MEC
               !              !ridged ice
               !              zdummy  = hi_max(ice_cat_bounds(2,1)-1)
               !              hi_max(ice_cat_bounds(2,1)-1) = 0.0
               !              DO jl = ice_cat_bounds(2,1), ice_cat_bounds(2,2) ! loop over ice thickness categories
               !                 zhin(2)          = ( hi_max(jl-1) + hi_max(jl) ) / 2.0
               !                 a_i(ji,jj,jl)    = zidto(ji,jj) * MAX( zgfactorn(2) * exp(-(zhin(2)-hginn_d)* &
               !                                    (zhin(2)-hginn_d)/2.0) , epsi06)
               !                 ht_i(ji,jj,jl)   = zidto(ji,jj) * zhin(2) 
               !                 v_i(ji,jj,jl)    = ht_i(ji,jj,jl)*a_i(ji,jj,jl)
               !              END DO
               !              hi_max(ice_cat_bounds(2,1)-1) = zdummy

               !              !rafted ice
               !              jl = 6
               !              a_i(ji,jj,jl)       = 0.0
               !              ht_i(ji,jj,jl)      = 0.0
               !              v_i(ji,jj,jl)       = 0.0
               ! END retour a LIMA_MEC
!!!

               DO jl = 1, jpl

                  !-------------
                  ! Snow depth
                  !-------------
                  ht_s(ji,jj,jl)   = zidto(ji,jj) * hninn
                  v_s(ji,jj,jl)    = ht_s(ji,jj,jl)*a_i(ji,jj,jl)

                  !---------------
                  ! Ice salinity
                  !---------------
                  sm_i(ji,jj,jl)   = zidto(ji,jj) * sinn  + ( 1.0 - zidto(ji,jj) ) * 0.1
                  smv_i(ji,jj,jl)  = MIN( sm_i(ji,jj,jl) , sss_m(ji,jj) ) * v_i(ji,jj,jl)

                  !----------
                  ! Ice age
                  !----------
                  o_i(ji,jj,jl)    = zidto(ji,jj) * 1.0   + ( 1.0 - zidto(ji,jj) )
                  oa_i(ji,jj,jl)   = o_i(ji,jj,jl) * a_i(ji,jj,jl)

                  !------------------------------
                  ! Sea ice surface temperature
                  !------------------------------

                  t_su(ji,jj,jl)   = zidto(ji,jj) * 270.0 + ( 1.0 - zidto(ji,jj) ) * t_bo(ji,jj)

                  !------------------------------------
                  ! Snow temperature and heat content
                  !------------------------------------

                  DO jk = 1, nlay_s
                     t_s(ji,jj,jk,jl) = zidto(ji,jj) * 270.00 + ( 1.0 - zidto(ji,jj) ) * rtt
                     ! Snow energy of melting
                     e_s(ji,jj,jk,jl) = zidto(ji,jj) * rhosn * ( cpic * ( rtt - t_s(ji,jj,jk,jl) ) + lfus )
                     ! Change dimensions
                     e_s(ji,jj,jk,jl) = e_s(ji,jj,jk,jl) / unit_fac
                     ! Multiply by volume, so that heat content in 10^9 Joules
                     e_s(ji,jj,jk,jl) = e_s(ji,jj,jk,jl) * area(ji,jj) * &
                        v_s(ji,jj,jl)  / nlay_s
                  END DO !jk

                  !-----------------------------------------------
                  ! Ice salinities, temperature and heat content 
                  !-----------------------------------------------

                  DO jk = 1, nlay_i
                     t_i(ji,jj,jk,jl) = zidto(ji,jj)*270.00 + ( 1.0 - zidto(ji,jj) ) * rtt 
                     s_i(ji,jj,jk,jl) = zidto(ji,jj) * sinn + ( 1.0 - zidto(ji,jj) ) * 0.1
                     ztmelts          = - tmut * s_i(ji,jj,jk,jl) + rtt !Melting temperature in K

                     ! heat content per unit volume
                     e_i(ji,jj,jk,jl) = zidto(ji,jj) * rhoic * &
                        (   cpic    * ( ztmelts - t_i(ji,jj,jk,jl) ) &
                        +   lfus    * ( 1.0 - (ztmelts-rtt) / MIN((t_i(ji,jj,jk,jl)-rtt),-zeps) ) &
                        - rcp      * ( ztmelts - rtt ) &
                        )

                     ! Correct dimensions to avoid big values
                     e_i(ji,jj,jk,jl) = e_i(ji,jj,jk,jl) / unit_fac 

                     ! Mutliply by ice volume, and divide by number of layers to get heat content in 10^9 J
                     e_i(ji,jj,jk,jl) = e_i(ji,jj,jk,jl) * & 
                        area(ji,jj) * a_i(ji,jj,jl) * ht_i(ji,jj,jl) / &
                        nlay_i
                  END DO ! jk

               END DO ! jl 

            ELSE ! on fcor 

               !--- Southern hemisphere
               !----------------------------------------------------------------

               !-----------------------
               ! Ice area / thickness
               !-----------------------

               IF ( jpl .EQ. 1) THEN ! one category

                  DO jl = ice_cat_bounds(1,1), ice_cat_bounds(1,2) ! loop over ice thickness categories
                     a_i(ji,jj,jl)    = zidto(ji,jj) * agins_u
                     ht_i(ji,jj,jl)   = zidto(ji,jj) * hgins_u
                     v_i(ji,jj,jl)    = ht_i(ji,jj,jl)*a_i(ji,jj,jl)
                  END DO

               ELSE ! several categories

                  !level ice
                  DO jl = ice_cat_bounds(1,1), ice_cat_bounds(1,2) !over thickness categories

                     zhis(1)       = ( hi_max(jl-1) + hi_max(jl) ) / 2.0
                     a_i(ji,jj,jl) = zidto(ji,jj) * MAX( zgfactors(1) * exp(-(zhis(1)-hgins_u) * & 
                        (zhis(1)-hgins_u)/2.0) , epsi06 )
                     ! new line square distribution volume conserving
                     a_i(ji,jj,jl)    = zidto(ji,jj) * ( zas * zhis(1) * zhis(1) + zbs * zhis(1) )
                     ht_i(ji,jj,jl)   = zidto(ji,jj) * zhis(1) 
                     v_i(ji,jj,jl)    = ht_i(ji,jj,jl)*a_i(ji,jj,jl)

                  END DO ! jl

               ENDIF

!!!
               ! retour a LIMA_MEC
               !              !ridged ice
               !              zdummy  = hi_max(ice_cat_bounds(2,1)-1)
               !              hi_max(ice_cat_bounds(2,1)-1) = 0.0
               !              DO jl = ice_cat_bounds(2,1), ice_cat_bounds(2,2) !over thickness categories
               !                 zhis(2)       = ( hi_max(jl-1) + hi_max(jl) ) / 2.0
               !                 a_i(ji,jj,jl) = zidto(ji,jj)*MAX( zgfactors(2)   &
               !                    &          * exp(-(zhis(2)-hgins_d)*(zhis(2)-hgins_d)/2.0), epsi06 )
               !                 ht_i(ji,jj,jl)   = zidto(ji,jj) * zhis(2) 
               !                 v_i(ji,jj,jl)    = ht_i(ji,jj,jl)*a_i(ji,jj,jl)
               !              END DO
               !              hi_max(ice_cat_bounds(2,1)-1) = zdummy

               !              !rafted ice
               !              jl = 6
               !              a_i(ji,jj,jl)       = 0.0
               !              ht_i(ji,jj,jl)      = 0.0
               !              v_i(ji,jj,jl)       = 0.0
               ! END retour a LIMA_MEC
!!!

               DO jl = 1, jpl !over thickness categories

                  !---------------
                  ! Snow depth
                  !---------------

                  ht_s(ji,jj,jl)   = zidto(ji,jj) * hnins
                  v_s(ji,jj,jl)    = ht_s(ji,jj,jl)*a_i(ji,jj,jl)

                  !---------------
                  ! Ice salinity
                  !---------------

                  sm_i(ji,jj,jl)   = zidto(ji,jj) * sins  + ( 1.0 - zidto(ji,jj) ) * 0.1
                  smv_i(ji,jj,jl)  = MIN( sm_i(ji,jj,jl) , sss_m(ji,jj) ) * v_i(ji,jj,jl)

                  !----------
                  ! Ice age
                  !----------

                  o_i(ji,jj,jl)    = zidto(ji,jj) * 1.0   + ( 1.0 - zidto(ji,jj) )
                  oa_i(ji,jj,jl)   = o_i(ji,jj,jl) * a_i(ji,jj,jl)

                  !------------------------------
                  ! Sea ice surface temperature
                  !------------------------------

                  t_su(ji,jj,jl)   = zidto(ji,jj) * 270.0 + ( 1.0 - zidto(ji,jj) ) * t_bo(ji,jj)

                  !----------------------------------
                  ! Snow temperature / heat content
                  !----------------------------------

                  DO jk = 1, nlay_s
                     t_s(ji,jj,jk,jl) = zidto(ji,jj) * 270.00 + ( 1.0 - zidto(ji,jj) ) * rtt
                     ! Snow energy of melting
                     e_s(ji,jj,jk,jl) = zidto(ji,jj) * rhosn * ( cpic * ( rtt - t_s(ji,jj,jk,jl) ) + lfus )
                     ! Change dimensions
                     e_s(ji,jj,jk,jl) = e_s(ji,jj,jk,jl) / unit_fac
                     ! Multiply by volume, so that heat content in 10^9 Joules
                     e_s(ji,jj,jk,jl) = e_s(ji,jj,jk,jl) * area(ji,jj) * &
                        v_s(ji,jj,jl)  / nlay_s
                  END DO

                  !---------------------------------------------
                  ! Ice temperature, salinity and heat content
                  !---------------------------------------------

                  DO jk = 1, nlay_i
                     t_i(ji,jj,jk,jl) = zidto(ji,jj)*270.00 + ( 1.0 - zidto(ji,jj) ) * rtt 
                     s_i(ji,jj,jk,jl) = zidto(ji,jj) * sins + ( 1.0 - zidto(ji,jj) ) * 0.1
                     ztmelts          = - tmut * s_i(ji,jj,jk,jl) + rtt !Melting temperature in K

                     ! heat content per unit volume
                     e_i(ji,jj,jk,jl) = zidto(ji,jj) * rhoic * &
                        (   cpic    * ( ztmelts - t_i(ji,jj,jk,jl) ) &
                        +   lfus  * ( 1.0 - (ztmelts-rtt) / MIN((t_i(ji,jj,jk,jl)-rtt),-zeps) ) &
                        - rcp      * ( ztmelts - rtt ) &
                        )

                     ! Correct dimensions to avoid big values
                     e_i(ji,jj,jk,jl) = e_i(ji,jj,jk,jl) / unit_fac 

                     ! Mutliply by ice volume, and divide by number of layers to get heat content in 10^9 J
                     e_i(ji,jj,jk,jl) = e_i(ji,jj,jk,jl) * & 
                        area(ji,jj) * a_i(ji,jj,jl) * ht_i(ji,jj,jl) / &
                        nlay_i
                  END DO !jk

               END DO ! jl 

            ENDIF ! on fcor

         END DO
      END DO

      !--------------------------------------------------------------------
      ! 3) Global ice variables for output diagnostics                    | 
      !--------------------------------------------------------------------

      fsbbq (:,:)     = 0.e0
      u_ice (:,:)     = 0.e0
      v_ice (:,:)     = 0.e0
      stress1_i(:,:)  = 0.0
      stress2_i(:,:)  = 0.0
      stress12_i(:,:) = 0.0

      !--------------------------------------------------------------------
      ! 4) Moments for advection
      !--------------------------------------------------------------------

      sxopw (:,:) = 0.e0 
      syopw (:,:) = 0.e0 
      sxxopw(:,:) = 0.e0 
      syyopw(:,:) = 0.e0 
      sxyopw(:,:) = 0.e0

      sxice (:,:,:)  = 0.e0   ;   sxsn (:,:,:)  = 0.e0   ;   sxa  (:,:,:)  = 0.e0
      syice (:,:,:)  = 0.e0   ;   sysn (:,:,:)  = 0.e0   ;   sya  (:,:,:)  = 0.e0
      sxxice(:,:,:)  = 0.e0   ;   sxxsn(:,:,:)  = 0.e0   ;   sxxa (:,:,:)  = 0.e0
      syyice(:,:,:)  = 0.e0   ;   syysn(:,:,:)  = 0.e0   ;   syya (:,:,:)  = 0.e0
      sxyice(:,:,:)  = 0.e0   ;   sxysn(:,:,:)  = 0.e0   ;   sxya (:,:,:)  = 0.e0

      sxc0  (:,:,:)  = 0.e0   ;   sxe  (:,:,:,:)= 0.e0   
      syc0  (:,:,:)  = 0.e0   ;   sye  (:,:,:,:)= 0.e0   
      sxxc0 (:,:,:)  = 0.e0   ;   sxxe (:,:,:,:)= 0.e0   
      syyc0 (:,:,:)  = 0.e0   ;   syye (:,:,:,:)= 0.e0   
      sxyc0 (:,:,:)  = 0.e0   ;   sxye (:,:,:,:)= 0.e0   

      sxsal  (:,:,:)  = 0.e0
      sysal  (:,:,:)  = 0.e0
      sxxsal (:,:,:)  = 0.e0
      syysal (:,:,:)  = 0.e0
      sxysal (:,:,:)  = 0.e0

      !--------------------------------------------------------------------
      ! 5) Lateral boundary conditions                                    | 
      !--------------------------------------------------------------------

      DO jl = 1, jpl
         CALL lbc_lnk( a_i(:,:,jl)  , 'T', 1. )
         CALL lbc_lnk( v_i(:,:,jl)  , 'T', 1. )
         CALL lbc_lnk( v_s(:,:,jl)  , 'T', 1. )
         CALL lbc_lnk( smv_i(:,:,jl), 'T', 1. )
         CALL lbc_lnk( oa_i(:,:,jl) , 'T', 1. )
         !
         CALL lbc_lnk( ht_i(:,:,jl) , 'T', 1. )
         CALL lbc_lnk( ht_s(:,:,jl) , 'T', 1. )
         CALL lbc_lnk( sm_i(:,:,jl) , 'T', 1. )
         CALL lbc_lnk( o_i(:,:,jl)  , 'T', 1. )
         CALL lbc_lnk( t_su(:,:,jl) , 'T', 1. )
         DO jk = 1, nlay_s
            CALL lbc_lnk(t_s(:,:,jk,jl), 'T', 1. )
            CALL lbc_lnk(e_s(:,:,jk,jl), 'T', 1. )
         END DO
         DO jk = 1, nlay_i
            CALL lbc_lnk(t_i(:,:,jk,jl), 'T', 1. )
            CALL lbc_lnk(e_i(:,:,jk,jl), 'T', 1. )
         END DO
         !
         a_i(:,:,jl) = tms(:,:) * a_i(:,:,jl)
      END DO

      CALL lbc_lnk( at_i , 'T', 1. )
      at_i(:,:) = tms(:,:) * at_i(:,:)                       ! put 0 over land
      !
      CALL lbc_lnk( fsbbq  , 'T', 1. )
      !
      CALL wrk_dealloc( jpm, zgfactorn, zgfactors, zhin, zhis )
      CALL wrk_dealloc( jpi, jpj, zidto )
      !
   END SUBROUTINE lim_istate


   SUBROUTINE lim_istate_init
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE lim_istate_init  ***
      !!        
      !! ** Purpose : Definition of initial state of the ice 
      !!
      !! ** Method :   Read the namiceini namelist and check the parameter 
      !!             values called at the first timestep (nit000)
      !!
      !! ** input  :   namelist namiceini
      !!-----------------------------------------------------------------------------
      NAMELIST/namiceini/ ttest, hninn, hginn_u, aginn_u, hginn_d, aginn_d, hnins,   &
         &                hgins_u, agins_u, hgins_d, agins_d, sinn, sins
      !!-----------------------------------------------------------------------------
      !
      REWIND ( numnam_ice )               ! Read Namelist namiceini 
      READ   ( numnam_ice , namiceini )
      !
      IF(lwp) THEN                        ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'lim_istate_init : ice parameters inititialisation '
         WRITE(numout,*) '~~~~~~~~~~~~~~~'
         WRITE(numout,*) '   threshold water temp. for initial sea-ice    ttest      = ', ttest
         WRITE(numout,*) '   initial snow thickness in the north          hninn      = ', hninn
         WRITE(numout,*) '   initial undef ice thickness in the north     hginn_u    = ', hginn_u
         WRITE(numout,*) '   initial undef ice concentr. in the north     aginn_u    = ', aginn_u          
         WRITE(numout,*) '   initial  def  ice thickness in the north     hginn_d    = ', hginn_d
         WRITE(numout,*) '   initial  def  ice concentr. in the north     aginn_d    = ', aginn_d          
         WRITE(numout,*) '   initial snow thickness in the south          hnins      = ', hnins 
         WRITE(numout,*) '   initial undef ice thickness in the north     hgins_u    = ', hgins_u
         WRITE(numout,*) '   initial undef ice concentr. in the north     agins_u    = ', agins_u          
         WRITE(numout,*) '   initial  def  ice thickness in the north     hgins_d    = ', hgins_d
         WRITE(numout,*) '   initial  def  ice concentr. in the north     agins_d    = ', agins_d          
         WRITE(numout,*) '   initial  ice salinity       in the north     sinn       = ', sinn
         WRITE(numout,*) '   initial  ice salinity       in the south     sins       = ', sins
      ENDIF
      !
   END SUBROUTINE lim_istate_init

#else
   !!----------------------------------------------------------------------
   !!   Default option :         Empty module          NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_istate          ! Empty routine
   END SUBROUTINE lim_istate
#endif

   !!======================================================================
END MODULE limistate
