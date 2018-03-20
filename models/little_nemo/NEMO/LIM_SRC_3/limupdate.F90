MODULE limupdate
   !!======================================================================
   !!                     ***  MODULE  limupdate  ***
   !!   LIM-3 : Update of sea-ice global variables at the end of the time step
   !!======================================================================
   !! History :  3.0  !  2006-04  (M. Vancoppenolle) Original code
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!    lim_update   : computes update of sea-ice global variables from trend terms
   !!----------------------------------------------------------------------
   USE limrhg          ! ice rheology

   USE dom_oce
   USE oce             ! dynamics and tracers variables
   USE in_out_manager
   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE sbc_ice         ! Surface boundary condition: ice fields
   USE dom_ice
   USE phycst          ! physical constants
   USE ice
   USE limdyn
   USE limtrp
   USE limthd
   USE limsbc
   USE limdia
   USE limwri
   USE limrst
   USE thd_ice         ! LIM thermodynamic sea-ice variables
   USE par_ice
   USE limitd_th
   USE limvar
   USE prtctl           ! Print control
   USE lbclnk           ! lateral boundary condition - MPP exchanges
   USE wrk_nemo         ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_update   ! routine called by ice_step

      REAL(wp)  ::   epsi06 = 1.e-06_wp   ! module constants
      REAL(wp)  ::   epsi04 = 1.e-04_wp   !    -       -
      REAL(wp)  ::   epsi03 = 1.e-03_wp   !    -       -
      REAL(wp)  ::   epsi10 = 1.e-10_wp   !    -       -
      REAL(wp)  ::   epsi16 = 1.e-16_wp   !    -       -
      REAL(wp)  ::   epsi20 = 1.e-20_wp   !    -       -
      REAL(wp)  ::   rzero  = 0._wp       !    -       -
      REAL(wp)  ::   rone   = 1._wp       !    -       -
         
   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limupdate.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_update
      !!-------------------------------------------------------------------
      !!               ***  ROUTINE lim_update  ***
      !!               
      !! ** Purpose :  Computes update of sea-ice global variables at 
      !!               the end of the time step.
      !!               Address pathological cases
      !!               This place is very important
      !!                
      !! ** Method  :  
      !!    Ice speed from ice dynamics
      !!    Ice thickness, Snow thickness, Temperatures, Lead fraction
      !!      from advection and ice thermodynamics 
      !!
      !! ** Action  : - 
      !!---------------------------------------------------------------------
      INTEGER ::   ji, jj, jk, jl, jm    ! dummy loop indices
      INTEGER ::   jbnd1, jbnd2
      INTEGER ::   i_ice_switch
      INTEGER ::   ind_im, layer      ! indices for internal melt
      REAL(wp) ::   zweight, zesum, zhimax, z_da_i, z_dv_i
      REAL(wp) ::   zindb, zindsn, zindic, zacrith
      REAL(wp) ::   zrtt, zindg, zh, zdvres, zviold
      REAL(wp) ::   zbigvalue, zvsold, z_da_ex, zamax
      REAL(wp) ::   z_prescr_hi, zat_i_old, ztmelts, ze_s

      INTEGER , POINTER, DIMENSION(:,:,:) ::  internal_melt
      REAL(wp), POINTER, DIMENSION(:) ::   zthick0, zqm0      ! thickness of the layers and heat contents for
      !!-------------------------------------------------------------------

      CALL wrk_alloc( jpi,jpj,jpl, internal_melt )   ! integer
      CALL wrk_alloc( jkmax, zthick0, zqm0 )

      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' lim_update '
         WRITE(numout,*) ' ~~~~~~~~~~ '

         WRITE(numout,*) ' O) Initial values '
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' at_i: ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
         DO jk = 1, nlay_i
            WRITE(numout,*) ' e_i : ', e_i(jiindx, jjindx, jk, 1:jpl)
         END DO
      ENDIF

      !------------------------------------------------------------------------------
      ! 1. Update of Global variables                                               |
      !------------------------------------------------------------------------------

      !---------------------
      ! Ice dynamics  
      !---------------------
      u_ice(:,:) = u_ice(:,:) + d_u_ice_dyn(:,:)
      v_ice(:,:) = v_ice(:,:) + d_v_ice_dyn(:,:)

      !-----------------------------
      ! Update ice and snow volumes  
      !-----------------------------
      DO jl = 1, jpl
         v_i(:,:,jl)  = v_i(:,:,jl) + d_v_i_trp(:,:,jl) + d_v_i_thd(:,:,jl) 
         v_s(:,:,jl)  = v_s(:,:,jl) + d_v_s_trp(:,:,jl) + d_v_s_thd(:,:,jl)
      END DO

      !---------------------------------
      ! Classify the pathological cases
      !---------------------------------
      ! (1) v_i (new) > 0; d_v_i_thd + v_i(old) > 0 (easy case)
      ! (2) v_i (new) > 0; d_v_i_thd + v_i(old) = 0 (total thermodynamic ablation)
      ! (3) v_i (new) < 0; d_v_i_thd + v_i(old) > 0 (combined total ablation)
      ! (4) v_i (new) < 0; d_v_i_thd + v_i(old) = 0 (total thermodynamic ablation 
      ! with negative advection, very pathological )
      ! (5) v_i (old) = 0; d_v_i_trp > 0 (advection of ice in a free-cell)
      !
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               patho_case(ji,jj,jl) = 1
               IF( v_i(ji,jj,jl) .GE. 0.0 ) THEN
                  IF ( old_v_i(ji,jj,jl) + d_v_i_thd(ji,jj,jl) .LT. epsi10 ) THEN 
                     patho_case(ji,jj,jl) = 2
                  ENDIF
               ELSE
                  patho_case(ji,jj,jl) = 3
                  IF( old_v_i(ji,jj,jl) + d_v_i_thd(ji,jj,jl) .LT. epsi10 ) THEN 
                     patho_case(ji,jj,jl) = 4
                  ENDIF
               ENDIF
               IF( ( old_v_i(ji,jj,jl) .LE. epsi10 ) .AND. &
                   ( d_v_i_trp(ji,jj,jl) .GT. epsi06 ) ) THEN
                  patho_case(ji,jj,jl) = 5 ! advection of ice in an ice-free
                  ! cell
                  IF( ln_nicep ) THEN  
                     WRITE(numout,*) ' ALERTE patho_case still equal to 5 '
                     WRITE(numout,*) ' ji , jj   : ', ji, jj
                     WRITE(numout,*) ' old_v_i   : ', old_v_i(ji,jj,jl)
                     WRITE(numout,*) ' d_v_i_trp : ', d_v_i_trp(ji,jj,jl)
                  ENDIF

               ENDIF
            END DO
         END DO
      END DO

      !--------------------
      ! Excessive ablation 
      !--------------------

      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF (      ( patho_case(ji,jj,jl) .EQ. 3 ) &
                  .OR. ( patho_case(ji,jj,jl) .EQ. 4 ) ) THEN
                  zviold         = old_v_i(ji,jj,jl)
                  zvsold         = old_v_s(ji,jj,jl)
                  ! in cases 3 ( combined total ablation )
                  !      and 4 ( total ablation with negative advection )
                  ! there is excessive total ablation
                  ! advection is chosen to be prioritary in order to conserve mass. 
                  ! dv_i_thd is computed as a residual
                  ! negative energy has to be kept in memory and to be given to the ocean
                  ! equivalent salt flux is given to the ocean
                  !
                  ! This was the best solution found. Otherwise, mass conservation in advection
                  ! scheme should have been revised, which could have been a big problem
                  ! Martin Vancoppenolle (2006, updated 2007)

                  ! is there any ice left ?
                  zindic        = MAX( rzero, SIGN( rone, v_i(ji,jj,jl) - epsi10 ) ) 
                  !=1 if hi > 1e-3 and 0 if not
                  zdvres        = MAX(0.0,-v_i(ji,jj,jl)) !residual volume if too much ice was molten
                  !this quantity is positive
                  v_i(ji,jj,jl) = zindic*v_i(ji,jj,jl)    !ice volume cannot be negative
                  !correct thermodynamic ablation
                  d_v_i_thd(ji,jj,jl)  = zindic  *  d_v_i_thd(ji,jj,jl) + (1.0-zindic) * (-zviold - d_v_i_trp(ji,jj,jl)) 
                  ! THIS IS NEW
                  d_a_i_thd(ji,jj,jl)  = zindic  *  d_a_i_thd(ji,jj,jl) + & 
                     (1.0-zindic) * (-old_a_i(ji,jj,jl)) 

                  !residual salt flux if ice is over-molten
                  fsalt_res(ji,jj)  = fsalt_res(ji,jj) + ( sss_m(ji,jj) - sm_i(ji,jj,jl) ) * & 
                     ( rhoic * zdvres / rdt_ice )
                  !             fheat_res(ji,jj)  = fheat_res(ji,jj) + rhoic * lfus * zdvres / rdt_ice

                  ! is there any snow left ?
                  zindsn        = MAX( rzero, SIGN( rone, v_s(ji,jj,jl) - epsi10 ) ) 
                  zvsold        = v_s(ji,jj,jl)
                  zdvres        = MAX(0.0,-v_s(ji,jj,jl)) !residual volume if too much ice was molten
                  !this quantity is positive
                  v_s(ji,jj,jl) = zindsn*v_s(ji,jj,jl)    !snow volume cannot be negative
                  !correct thermodynamic ablation
                  d_v_s_thd(ji,jj,jl)  = zindsn  *  d_v_s_thd(ji,jj,jl) + & 
                     (1.0-zindsn) * (-zvsold - d_v_s_trp(ji,jj,jl)) 
                  !unsure correction on salt flux.... maybe future will tell it was not that right

                  !residual salt flux if snow is over-molten
                  fsalt_res(ji,jj)  = fsalt_res(ji,jj) + sss_m(ji,jj) * ( rhosn * zdvres / rdt_ice )
                  !this flux will be positive if snow was over-molten
                  !             fheat_res(ji,jj)  = fheat_res(ji,jj) + rhosn * lfus * zdvres / rdt_ice
               ENDIF
            END DO !ji
         END DO !jj
      END DO !jl

      IF( ln_nicep ) THEN  
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF ( ABS(fsalt_res(ji,jj)) .GT. 1.0 ) THEN 
                  WRITE(numout,*) ' ALERTE 1000 : residual salt flux of -> ', &
                     fsalt_res(ji,jj)
                  WRITE(numout,*) ' ji, jj : ', ji, jj, ' gphit, glamt : ', &
                     gphit(ji,jj), glamt(ji,jj)
               ENDIF
            END DO
         END DO

         WRITE(numout,*) ' 1. Before update of Global variables '
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' at_i: ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
         DO jk = 1, nlay_i
            WRITE(numout,*) ' e_i : ', e_i(jiindx, jjindx, jk, 1:jpl)
         END DO
      ENDIF

      !---------------------------------------------
      ! Ice concentration and ice heat content
      !---------------------------------------------

      a_i (:,:,:) = a_i (:,:,:) + d_a_i_trp(:,:,:) + d_a_i_thd(:,:,:)
      CALL lim_var_glo2eqv    ! useless, just for debug
      IF( ln_nicep ) THEN 
         DO jk = 1, nlay_i
            WRITE(numout,*) ' t_i : ', t_i(jiindx, jjindx, jk, 1:jpl)
         END DO
      ENDIF
      e_i(:,:,:,:) = e_i(:,:,:,:) + d_e_i_trp(:,:,:,:)  
      CALL lim_var_glo2eqv    ! useless, just for debug
      IF( ln_nicep) THEN
         WRITE(numout,*) ' After transport update '
         DO jk = 1, nlay_i
            WRITE(numout,*) ' t_i : ', t_i(jiindx, jjindx, jk, 1:jpl)
         END DO
      ENDIF
      e_i(:,:,:,:) = e_i(:,:,:,:) + d_e_i_thd(:,:,:,:)  
      CALL lim_var_glo2eqv ! useless, just for debug
      IF( ln_nicep ) THEN
         WRITE(numout,*) ' After thermodyn update '
         DO jk = 1, nlay_i
            WRITE(numout,*) ' t_i : ', t_i(jiindx, jjindx, jk, 1:jpl)
         END DO
      ENDIF

      at_i(:,:) = 0._wp
      DO jl = 1, jpl
         at_i(:,:) = a_i(:,:,jl) + at_i(:,:)
      END DO

      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' 1. After update of Global variables (2) '
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' at_i: ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' oa_i : ', oa_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' e_s : ', e_s(jiindx, jjindx, 1, 1:jpl)
         DO jk = 1, nlay_i
            WRITE(numout,*) ' e_i : ', e_i(jiindx, jjindx, jk, 1:jpl)
         END DO
      ENDIF

      !------------------------------
      ! Snow temperature and ice age
      !------------------------------
      e_s (:,:,:,:) = e_s (:,:,:,:) + d_e_s_trp (:,:,:,:) + d_e_s_thd (:,:,:,:)
      oa_i(:,:,:)   = oa_i(:,:,:)   + d_oa_i_trp(:,:,:)   + d_oa_i_thd(:,:,:)

      !--------------
      ! Ice salinity    
      !--------------

      IF(  num_sal == 2  .OR.  num_sal == 4  ) THEN      ! general case
         !
         IF( ln_nicep ) THEN  
            WRITE(numout,*) ' Before everything '
            WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
            WRITE(numout,*) ' oa_i:  ', oa_i(jiindx, jjindx, 1:jpl)
            DO jk = 1, nlay_i
               WRITE(numout,*) ' e_i : ', e_i(jiindx, jjindx, jk, 1:jpl)
            END DO
            WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
         ENDIF

         smv_i(:,:,:) = smv_i(:,:,:) + d_smv_i_thd(:,:,:) + d_smv_i_trp(:,:,:)
         !
         IF( ln_nicep ) THEN  
            WRITE(numout,*) ' After advection   '
            WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
            WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
         ENDIF
         !
      ENDIF

      CALL lim_var_glo2eqv

      !--------------------------------------
      ! 2. Review of all pathological cases
      !--------------------------------------
      zrtt    = 173.15_wp * rone
      zacrith = 1.e-6_wp

      !-------------------------------------------
      ! 2.1) Advection of ice in an ice-free cell
      !-------------------------------------------
      ! should be removed since it is treated after dynamics now

      zhimax = 5._wp
      ! first category
      DO jj = 1, jpj
         DO ji = 1, jpi
            !--- the thickness of such an ice is often out of bounds
            !--- thus we recompute a new area while conserving ice volume
            zat_i_old = SUM(old_a_i(ji,jj,:))
            zindb          =  MAX( rzero, SIGN( rone, ABS(d_a_i_trp(ji,jj,1)) - epsi10 ) ) 
            IF (      ( ABS(d_v_i_trp(ji,jj,1))/MAX(ABS(d_a_i_trp(ji,jj,1)),epsi10)*zindb.GT.zhimax) &
               .AND.( ( v_i(ji,jj,1)/MAX(a_i(ji,jj,1),epsi10)*zindb).GT.zhimax ) &
               .AND.( zat_i_old.LT.zacrith ) )  THEN ! new line
               z_prescr_hi      = hi_max(1) / 2.0
               a_i(ji,jj,1)     = v_i(ji,jj,1) / z_prescr_hi
            ENDIF
         END DO
      END DO

      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' 2.1 '
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' at_i: ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
         DO jk = 1, nlay_i
            WRITE(numout,*) ' e_i : ', e_i(jiindx, jjindx, jk, 1:jpl)
         END DO
      ENDIF

      !change this 14h44
      zhimax = 20.0     ! line added up
      ! change this also 17 aug
      zhimax = 30.0     ! line added up

      DO jl = 2, jpl
         jm = ice_types(jl)
         DO jj = 1, jpj
            DO ji = 1, jpi
               zindb          =  MAX( rzero, SIGN( rone, ABS(d_a_i_trp(ji,jj,jl)) - epsi10 ) ) 
               ! this correction is very tricky... sometimes, advection gets wrong i don't know why
               ! it makes problems when the advected volume and concentration do not seem to be 
               ! related with each other
               ! the new thickness is sometimes very big!
               ! and sometimes d_a_i_trp and d_v_i_trp have different sign
               ! which of course is plausible
               ! but fuck! it fucks everything up :)
               IF ( (ABS(d_v_i_trp(ji,jj,jl))/MAX(ABS(d_a_i_trp(ji,jj,jl)),epsi10)*zindb.GT.zhimax) &
                  .AND.(v_i(ji,jj,jl)/MAX(a_i(ji,jj,jl),epsi10)*zindb).GT.zhimax ) THEN
                  z_prescr_hi  =  ( hi_max_typ(jl-ice_cat_bounds(jm,1)  ,jm) + &
                     hi_max_typ(jl-ice_cat_bounds(jm,1)+1,jm) ) / 2.0
                  a_i (ji,jj,jl) = v_i(ji,jj,jl) / z_prescr_hi
                  ht_i(ji,jj,jl) = v_i(ji,jj,jl) / a_i(ji,jj,jl)
               ENDIF
               zat_i_old = SUM(old_a_i(ji,jj,:))

            END DO ! ji
         END DO !jj
      END DO !jl

      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' 2.1 initial '
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' at_i: ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
         DO jk = 1, nlay_i
            WRITE(numout,*) ' e_i : ', e_i(jiindx, jjindx, jk, 1:jpl)
         END DO
      ENDIF

      at_i(:,:) = 0._wp
      DO jl = 1, jpl
         at_i(:,:) = a_i(:,:,jl) + at_i(:,:)
      END DO

      !----------------------------------------------------
      ! 2.2) Rebin categories with thickness out of bounds
      !----------------------------------------------------
      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' 2.1 before rebinning '
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' at_i: ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
         DO jk = 1, nlay_i
            WRITE(numout,*) ' e_i : ', e_i(jiindx, jjindx, jk, 1:jpl)
         END DO
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
      ENDIF

      DO jm = 1, jpm
         jbnd1 = ice_cat_bounds(jm,1)
         jbnd2 = ice_cat_bounds(jm,2)
         IF (ice_ncat_types(jm) .GT. 1 )   CALL lim_itd_th_reb(jbnd1, jbnd2, jm)
      END DO


      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' 2.1 after rebinning'
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' at_i: ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
         DO jk = 1, nlay_i
            WRITE(numout,*) ' e_i : ', e_i(jiindx, jjindx, jk, 1:jpl)
            WRITE(numout,*) ' t_i : ', t_i(jiindx, jjindx, jk, 1:jpl)
         END DO
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
      ENDIF

      at_i(:,:) = 0._wp
      DO jl = 1, jpl
         at_i(:,:) = a_i(:,:,jl) + at_i(:,:)
      END DO

      !---------------------------------
      ! 2.3) Melt of an internal layer
      !---------------------------------
      internal_melt(:,:,:) = 0

      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj 
               DO ji = 1, jpi
                  ztmelts = - tmut * s_i(ji,jj,jk,jl) + rtt
                  IF ( ( ( e_i(ji,jj,jk,jl) .LE. 0.0 ) .OR. &
                     ( t_i(ji,jj,jk,jl) .GE. ztmelts ) ) .AND. &
                     ( v_i(ji,jj,jl) .GT. 0.0 ) .AND. &
                     ( a_i(ji,jj,jl) .GT. 0.0 ) ) THEN
                     !                    WRITE(numout,*) ' Internal layer melt : '
                     !                    WRITE(numout,*) ' ji, jj, jk, jl : ', ji,jj,jk,jl
                     !                    WRITE(numout,*) ' e_i : ', e_i(ji,jj,jk,jl)
                     !                    WRITE(numout,*) ' v_i : ', v_i(ji,jj,jl)
                     internal_melt(ji,jj,jl) = 1
                  ENDIF
               END DO ! ji
            END DO ! jj
         END DO !jk
      END DO !jl

      DO jl = 1, jpl
         DO jj = 1, jpj 
            DO ji = 1, jpi
               IF( internal_melt(ji,jj,jl) == 1 ) THEN
                  ! initial ice thickness
                  !-----------------------
                  ht_i(ji,jj,jl) = v_i(ji,jj,jl) / a_i(ji,jj,jl)
                  !             WRITE(numout,*) ' ji,jj,jl : ', ji,jj,jl
                  !             WRITE(numout,*) ' old ht_i: ', ht_i(ji,jj,jl)
                  !             WRITE(numout,*) ' Enthalpy at the beg : ', e_i(ji,jj,1:nlay_i,jl)
                  !             WRITE(numout,*) ' smv_i   : ', smv_i(ji,jj,jl)

                  ! reduce ice thickness
                  !-----------------------
                  ind_im = 0
                  zesum = 0.0
                  DO jk = 1, nlay_i
                     ztmelts = - tmut * s_i(ji,jj,jk,jl) + rtt
                     IF ( ( e_i(ji,jj,jk,jl) .LE. 0.0 ) .OR.  & 
                        ( t_i(ji,jj,jk,jl) .GE. ztmelts ) ) &
                        ind_im = ind_im + 1
                     zesum = zesum + e_i(ji,jj,jk,jl)
                  END DO
                  IF (ind_im .LT.nlay_i ) smv_i(ji,jj,jl)= smv_i(ji,jj,jl) / ht_i(ji,jj,jl) * & 
                     ( ht_i(ji,jj,jl) - ind_im*ht_i(ji,jj,jl) / nlay_i )
                  ht_i(ji,jj,jl) = ht_i(ji,jj,jl) - ind_im*ht_i(ji,jj,jl) / nlay_i
                  v_i(ji,jj,jl)  = ht_i(ji,jj,jl) * a_i(ji,jj,jl)

                  !             WRITE(numout,*) ' ind_im  : ', ind_im
                  !             WRITE(numout,*) ' new ht_i: ', ht_i(ji,jj,jl)
                  !             WRITE(numout,*) ' smv_i   : ', smv_i(ji,jj,jl)
                  !             WRITE(numout,*) ' zesum   : ', zesum

                  ! redistribute heat
                  !-----------------------
                  ! old thicknesses and enthalpies
                  ind_im = 0
                  DO jk = 1, nlay_i
                     ztmelts = - tmut * s_i(ji,jj,jk,jl) + rtt
                     IF ( ( e_i(ji,jj,jk,jl) .GT. 0.0 ) .AND.  & 
                        ( t_i(ji,jj,jk,jl) .LT. ztmelts ) ) THEN
                        ind_im = ind_im + 1
                        zthick0(ind_im) = ht_i(ji,jj,jl) * ind_im / nlay_i
                        zqm0   (ind_im) = MAX( e_i(ji,jj,jk,jl) , 0.0 )
                     ENDIF
                  END DO

                  !             WRITE(numout,*) ' Old thickness, enthalpy '
                  !             WRITE(numout,*) ' Number of layer : ind_im ', ind_im
                  !             WRITE(numout,*) ' zthick0 : ', zthick0(1:ind_im)
                  !             WRITE(numout,*) ' zqm0    : ', zqm0(1:ind_im)

                  ! Redistributing energy on the new grid
                  IF ( ind_im .GT. 0 ) THEN

                     DO jk = 1, nlay_i
                        e_i(ji,jj,jk,jl) = 0.0
                        DO layer = 1, ind_im
                           zweight         = MAX (  &
                              MIN( ht_i(ji,jj,jl) * layer / ind_im , ht_i(ji,jj,jl) * jk / nlay_i ) -       &
                              MAX( ht_i(ji,jj,jl) * (layer-1) / ind_im , ht_i(ji,jj,jl) * (jk-1) / nlay_i ) , 0.0 ) &
                              /  ( ht_i(ji,jj,jl) / ind_im )

                           e_i(ji,jj,jk,jl) =  e_i(ji,jj,jk,jl) + zweight*zqm0(layer)
                        END DO !layer
                     END DO ! jk

                     zesum = 0.0
                     DO jk = 1, nlay_i
                        zesum = zesum + e_i(ji,jj,jk,jl)
                     END DO

                     !             WRITE(numout,*) ' Enthalpy at the end : ', e_i(ji,jj,1:nlay_i,jl)
                     !             WRITE(numout,*) ' Volume   at the end : ', v_i(ji,jj,jl)
                     !             WRITE(numout,*) ' zesum : ', zesum

                  ELSE ! ind_im .EQ. 0, total melt
                     e_i(ji,jj,jk,jl) = 0.0
                  ENDIF

               ENDIF ! internal_melt

            END DO ! ji
         END DO !jj
      END DO !jl
      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' 2.3 after melt of an internal ice layer '
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' at_i: ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
         DO jk = 1, nlay_i
            WRITE(numout,*) ' e_i : ', e_i(jiindx, jjindx, jk, 1:jpl)
            WRITE(numout,*) ' t_i : ', t_i(jiindx, jjindx, jk, 1:jpl)
         END DO
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
      ENDIF

      internal_melt(:,:,:) = 0

      ! Melt of snow
      !--------------
      DO jl = 1, jpl
         DO jj = 1, jpj 
            DO ji = 1, jpi
               ! snow energy of melting
               ze_s = e_s(ji,jj,1,jl) * unit_fac / area(ji,jj) /              &
                  MAX( v_s(ji,jj,jl), 1.0e-6 )  ! snow energy of melting

               ! If snow energy of melting smaller then Lf
               ! Then all snow melts and meltwater, heat go to the ocean
               IF ( ze_s .LE. rhosn * lfus ) internal_melt(ji,jj,jl) = 1

               IF( ln_nicep ) THEN  
                  IF ( (ji.eq.jiindx) .AND. (jj.eq.jjindx) ) THEN
                     WRITE(numout,*) ' jl    : ', jl
                     WRITE(numout,*) ' ze_s  : ', ze_s
                     WRITE(numout,*) ' v_s   : ', v_s(ji,jj,jl)
                     WRITE(numout,*) ' rhosn : ', rhosn
                     WRITE(numout,*) ' rhosn : ', lfus 
                     WRITE(numout,*) ' area  : ', area(ji,jj)
                     WRITE(numout,*) ' rhosn * lfus : ', rhosn * lfus 
                     WRITE(numout,*) ' internal_melt : ', internal_melt(ji,jj,jl)
                  ENDIF
               ENDIF

            END DO
         END DO
      END DO

      DO jl = 1, jpl
         DO jj = 1, jpj 
            DO ji = 1, jpi
               IF ( internal_melt(ji,jj,jl) == 1 ) THEN
                  v_s(ji,jj,jl)   = 0.0
                  e_s(ji,jj,1,jl) = 0.0
                  !   ! release heat
                  fheat_res(ji,jj) = fheat_res(ji,jj)  &
                     + ze_s * v_s(ji,jj,jl) / rdt_ice
                  ! release mass
                  rdmsnif(ji,jj) =  rdmsnif(ji,jj) + rhosn * v_s(ji,jj,jl)
               ENDIF
            END DO
         END DO
      END DO

      zbigvalue      = 1.0d+20

      DO jl = 1, jpl
         DO jj = 1, jpj 
            DO ji = 1, jpi

               !switches
               zindb         = MAX( rzero, SIGN( rone, a_i(ji,jj,jl) - epsi06 ) ) 
               !switch = 1 if a_i > 1e-06 and 0 if not
               zindsn        = MAX( rzero, SIGN( rone, v_s(ji,jj,jl) - epsi06 ) ) !=1 if hs > 1e-6 and 0 if not
               zindic        = MAX( rzero, SIGN( rone, v_i(ji,jj,jl) - epsi04 ) ) !=1 if hi > 1e-3 and 0 if not
               ! bug fix 25 avril 2007
               zindb         = zindb*zindic

               !--- 2.3 Correction to ice age 
               !------------------------------
               !                IF ((o_i(ji,jj,jl)-1.0)*86400.0.gt.(rdt_ice*float(numit))) THEN
               !                   o_i(ji,jj,jl) = rdt_ice*FLOAT(numit)/86400.0
               !                ENDIF
               IF ((oa_i(ji,jj,jl)-1.0)*86400.0.gt.(rdt_ice*numit*a_i(ji,jj,jl))) THEN
                  oa_i(ji,jj,jl) = rdt_ice*numit/86400.0*a_i(ji,jj,jl)
               ENDIF
               oa_i(ji,jj,jl) = zindb*zindic*oa_i(ji,jj,jl)

               !--- 2.4 Correction to snow thickness
               !-------------------------------------
               !          ! snow thickness has to be greater than 0, and if ice concentration smaller than 1e-6 then hs = 0
               !             v_s(ji,jj,jl)  = MAX( zindb * v_s(ji,jj,jl), 0.0) 
               ! snow thickness cannot be smaller than 1e-6
               v_s(ji,jj,jl)  = zindsn*v_s(ji,jj,jl)*zindb
               v_s(ji,jj,jl)  = v_s(ji,jj,jl) *  MAX( 0.0 , SIGN( 1.0 , v_s(ji,jj,jl) - epsi06 ) )

               !--- 2.5 Correction to ice thickness
               !-------------------------------------
               ! ice thickness has to be greater than 0, and if ice concentration smaller than 1e-6 then hi = 0
               v_i(ji,jj,jl) = MAX( zindb * v_i(ji,jj,jl), 0.0)
               ! ice thickness cannot be smaller than 1e-3
               v_i(ji,jj,jl)  = zindic*v_i(ji,jj,jl)

               !--- 2.6 Snow is transformed into ice if the original ice cover disappears
               !----------------------------------------------------------------------------
               zindg          = tms(ji,jj) *  MAX( rzero , SIGN( rone , -v_i(ji,jj,jl) ) )
               v_i(ji,jj,jl)  = v_i(ji,jj,jl) + zindg * rhosn * v_s(ji,jj,jl) / rau0
               v_s(ji,jj,jl)  = ( rone - zindg ) * v_s(ji,jj,jl) + & 
                  zindg * v_i(ji,jj,jl) * ( rau0 - rhoic ) / rhosn

               !--- 2.7 Correction to ice concentrations 
               !--------------------------------------------
               ! if greater than 0, ice concentration cannot be smaller than 1e-10
               a_i(ji,jj,jl) = zindb * MAX(zindsn, zindic) * MAX( a_i(ji,jj,jl), epsi06 )
               ! then ice volume has to be corrected too...
               ! instead, zap small areas

               !-------------------------
               ! 2.8) Snow heat content
               !-------------------------

               e_s(ji,jj,1,jl) = zindsn *                                &
                  ( MIN ( MAX ( 0.0, e_s(ji,jj,1,jl) ), zbigvalue ) ) + &
                  ( 1.0 - zindsn ) * 0.0

            END DO ! ji
         END DO ! jj
      END DO ! jl

      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' 2.8 '
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' at_i: ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
         DO jk = 1, nlay_i
            WRITE(numout,*) ' e_i : ', e_i(jiindx, jjindx, jk, 1:jpl)
         END DO
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
      ENDIF

      !------------------------
      ! 2.9) Ice heat content 
      !------------------------

      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj 
               DO ji = 1, jpi
                  zindic        = MAX( rzero, SIGN( rone, v_i(ji,jj,jl) - epsi06 ) ) 
                  ! =1 if v_i > 1e-6 and 0 if not
                  e_i(ji,jj,jk,jl)= zindic * & 
                     ( MIN ( MAX ( 0.0, e_i(ji,jj,jk,jl) ), zbigvalue ) ) + &
                     ( 1.0 - zindic ) * 0.0
               END DO ! ji
            END DO ! jj
         END DO !jk
      END DO !jl

      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' 2.9 '
         DO jk = 1, nlay_i
            WRITE(numout,*) ' e_i : ', e_i(jiindx, jjindx, jk, 1:jpl)
         END DO
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)

         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
      ENDIF

      !---------------------
      ! 2.11) Ice salinity
      !---------------------

      IF ( ( num_sal .EQ. 2 ) .OR. ( num_sal .EQ. 4 ) ) THEN ! general case

         DO jl = 1, jpl
            DO jk = 1, nlay_i
               DO jj = 1, jpj 
                  DO ji = 1, jpi
                     ! salinity stays in bounds
                     smv_i(ji,jj,jl)  =  MAX(MIN((rhoic-rhosn)/rhoic*sss_m(ji,jj),smv_i(ji,jj,jl)), &
                        0.1 * v_i(ji,jj,jl) )
                     i_ice_switch    =  1.0-MAX(0.0,SIGN(1.0,-v_i(ji,jj,jl)))
                     smv_i(ji,jj,jl)  = i_ice_switch*smv_i(ji,jj,jl) + &
                        0.1*(1.0-i_ice_switch)*v_i(ji,jj,jl)
                  END DO ! ji
               END DO ! jj
            END DO !jk
         END DO !jl

      ENDIF

      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' 2.11 '
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' at_i    ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
      ENDIF

      DO jm = 1, jpm
         DO jj = 1, jpj 
            DO ji = 1, jpi
               jl = ice_cat_bounds(jm,1)
               !--- 2.12 Constrain the thickness of the smallest category above 5 cm
               !----------------------------------------------------------------------
               ! the ice thickness of the smallest category should be higher than 5 cm
               ! we changed hiclim to 10
               zindb         = MAX( rzero, SIGN( rone, a_i(ji,jj,jl) - epsi06 ) ) 
               ht_i(ji,jj,jl) = zindb*v_i(ji,jj,jl)/MAX(a_i(ji,jj,jl), epsi06)
               zh            = MAX( rone , zindb * hiclim  / MAX( ht_i(ji,jj,jl) , epsi20 ) )
               ht_s(ji,jj,jl) = ht_s(ji,jj,jl)* zh
               !             v_s(ji,jj,jl)  = v_s(ji,jj,jl) * zh
               ht_i(ji,jj,jl) = ht_i(ji,jj,jl)* zh
               !             v_i(ji,jj,jl)  = v_i(ji,jj,jl) * zh
               a_i (ji,jj,jl) = a_i(ji,jj,jl) /zh
            END DO !ji
         END DO !jj
      END DO !jm
      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' 2.12 '
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' at_i    ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
      ENDIF

      !--- 2.13 Total ice concentration should not exceed 1
      !-----------------------------------------------------
      zamax = amax
      ! 2.13.1) individual concentrations cannot exceed zamax
      !------------------------------------------------------

      at_i(:,:) = 0.0
      DO jl = 1, jpl
         at_i(:,:) = a_i(:,:,jl) + at_i(:,:)
      END DO

      ! 2.13.2) Total ice concentration cannot exceed zamax
      !----------------------------------------------------
      at_i(:,:) = a_i(:,:,1)
      DO jl = 2, jpl
         at_i(:,:) = a_i(:,:,jl) + at_i(:,:)
      END DO

      DO jj = 1, jpj
         DO ji = 1, jpi

            ! 0) Excessive area ?
            z_da_ex =  MAX( at_i(ji,jj) - zamax , 0.0 )        

            ! 1) Count the number of existing categories
            DO jl  = 1, jpl
               zindb   =  MAX( rzero, SIGN( rone, v_i(ji,jj,jl) - epsi03 ) ) 
               zindb   =  MAX( rzero, SIGN( rone, v_i(ji,jj,jl) ) ) 
               z_da_i = a_i(ji,jj,jl) * z_da_ex / MAX( at_i(ji,jj), epsi06 ) * zindb
               z_dv_i = v_i(ji,jj,jl) * z_da_i  / MAX( at_i(ji,jj), epsi06 )
               a_i(ji,jj,jl) = a_i(ji,jj,jl) - z_da_i
               v_i(ji,jj,jl) = v_i(ji,jj,jl) + z_dv_i
            END DO

         END DO !ji
      END DO !jj

      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' 2.13 '
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl), ' at_i    ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl), ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
      ENDIF

      at_i(:,:) = a_i(:,:,1)
      DO jl = 2, jpl
         at_i(:,:) = a_i(:,:,jl) + at_i(:,:)
      END DO

      IF( ln_nicep ) THEN  
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF (at_i(ji,jj).GT.1.0) THEN
                  WRITE(numout,*) ' lim_update ! : at_i > 1 -> PAS BIEN -> ALERTE '
                  WRITE(numout,*) ' ~~~~~~~~~~   at_i     ', at_i(ji,jj)
                  WRITE(numout,*) ' Point ', ji, jj
                  WRITE(numout,*) ' lat - long ', gphit(ji,jj), glamt(ji,jj)
                  DO jl = 1, jpl
                     WRITE(numout,*) ' a_i ***         ', a_i(ji,jj,jl), ' CAT no ', jl
                     WRITE(numout,*) ' a_i_old ***     ', old_a_i(ji,jj,jl), ' CAT no ', jl
                     WRITE(numout,*) ' d_a_i_thd / trp ', d_a_i_thd(ji,jj,jl), d_a_i_trp(ji,jj,jl)
                  END DO
                  !             WRITE(numout,*) ' CORRECTION BARBARE '
                  !             z_da_ex =  MAX( at_i(ji,jj) - zamax , 0.0 )        
               ENDIF
            END DO
         END DO
      ENDIF

      ! Final thickness distribution rebinning
      ! --------------------------------------
      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' rebinning before'
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' at_i    ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
      ENDIF
      !old version
      !    CALL lim_itd_th_reb(1,jpl)

      DO jm = 1, jpm
         jbnd1 = ice_cat_bounds(jm,1)
         jbnd2 = ice_cat_bounds(jm,2)
         IF (ice_ncat_types(jm) .GT. 1 ) CALL lim_itd_th_reb(jbnd1, jbnd2, jm)
         IF (ice_ncat_types(jm) .EQ. 1 ) THEN
         ENDIF
      END DO

      IF( ln_nicep ) THEN  
         WRITE(numout,*) ' rebinning final'
         WRITE(numout,*) ' a_i : ', a_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' at_i    ', at_i(jiindx,jjindx)
         WRITE(numout,*) ' v_i : ', v_i(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' v_s : ', v_s(jiindx, jjindx, 1:jpl)
         WRITE(numout,*) ' smv_i: ', smv_i(jiindx, jjindx, 1:jpl)
      ENDIF

      at_i(:,:) = a_i(:,:,1)
      DO jl = 2, jpl
         at_i(:,:) = a_i(:,:,jl) + at_i(:,:)
      END DO

      !------------------------------------------------------------------------------
      ! 2) Corrections to avoid wrong values                                        |
      !------------------------------------------------------------------------------
      ! Ice drift
      !------------
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1
            IF ( at_i(ji,jj) .EQ. 0.0 ) THEN ! what to do if there is no ice
               IF ( at_i(ji+1,jj) .EQ. 0.0 ) u_ice(ji,jj)   = 0.0 ! right side
               IF ( at_i(ji-1,jj) .EQ. 0.0 ) u_ice(ji-1,jj) = 0.0 ! left side
               IF ( at_i(ji,jj+1) .EQ. 0.0 ) v_ice(ji,jj)   = 0.0 ! upper side
               IF ( at_i(ji,jj-1) .EQ. 0.0 ) v_ice(ji,jj-1) = 0.0 ! bottom side
            ENDIF
         END DO
      END DO
      !mask velocities
      u_ice(:,:) = u_ice(:,:) * tmu(:,:)
      v_ice(:,:) = v_ice(:,:) * tmv(:,:)
      !lateral boundary conditions
      CALL lbc_lnk( u_ice(:,:), 'U', -1. )
      CALL lbc_lnk( v_ice(:,:), 'V', -1. )

      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! ALERTES
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      IF( ln_nicep ) THEN  
         DO jj = 1, jpj
            DO ji = 1, jpi
               DO jl = 1, jpl
                  IF ( (a_i(ji,jj,jl).GT.1.0).OR.(at_i(ji,jj).GT.1.0) ) THEN
                     zindb          =  MAX( rzero, SIGN( rone, a_i(ji,jj,jl) - epsi06 ) ) 
                     WRITE(numout,*) ' lim_update : a_i > 1 '
                     WRITE(numout,*) ' PAS BIEN ----> ALERTE !!! '
                     WRITE(numout,*) ' ~~~~~~~~~~   at_i     ', at_i(ji,jj)
                     WRITE(numout,*) ' Point - category', ji, jj, jl
                     WRITE(numout,*) ' lat - long ', gphit(ji,jj), glamt(ji,jj)
                     WRITE(numout,*) ' a_i *** a_i_old ', a_i(ji,jj,jl), old_a_i(ji,jj,jl)
                     WRITE(numout,*) ' v_i *** v_i_old ', v_i(ji,jj,jl), old_v_i(ji,jj,jl)
                     WRITE(numout,*) ' ht_i ***        ', v_i(ji,jj,jl)/MAX(a_i(ji,jj,jl),epsi06)*zindb 
                     WRITE(numout,*) ' hi_max(jl), hi_max(jl-1) ', hi_max(jl), hi_max(jl-1)
                     WRITE(numout,*) ' d_v_i_thd / trp ', d_v_i_thd(ji,jj,jl), d_v_i_trp(ji,jj,jl)
                     WRITE(numout,*) ' d_a_i_thd / trp ', d_a_i_thd(ji,jj,jl), d_a_i_trp(ji,jj,jl)
                  ENDIF
               END DO

            END DO !jj
         END DO !ji

         WRITE(numout,*) ' TESTOSC1 ', tio_u(jiindx,jjindx), tio_v(jiindx,jjindx)
         WRITE(numout,*) ' TESTOSC2 ', u_ice(jiindx,jjindx), v_ice(jiindx,jjindx)
         WRITE(numout,*) ' TESTOSC3 ', u_oce(jiindx,jjindx), v_oce(jiindx,jjindx)
         WRITE(numout,*) ' TESTOSC4 ', utau (jiindx,jjindx), vtau (jiindx,jjindx)
      ENDIF


      IF(ln_ctl) THEN   ! Control print
         CALL prt_ctl_info(' ')
         CALL prt_ctl_info(' - Cell values : ')
         CALL prt_ctl_info('   ~~~~~~~~~~~~~ ')
         CALL prt_ctl(tab2d_1=area       , clinfo1=' lim_update  : cell area   :')
         CALL prt_ctl(tab2d_1=at_i       , clinfo1=' lim_update  : at_i        :')
         CALL prt_ctl(tab2d_1=vt_i       , clinfo1=' lim_update  : vt_i        :')
         CALL prt_ctl(tab2d_1=vt_s       , clinfo1=' lim_update  : vt_s        :')
         CALL prt_ctl(tab2d_1=strength   , clinfo1=' lim_update  : strength    :')
         CALL prt_ctl(tab2d_1=u_ice      , clinfo1=' lim_update  : u_ice       :', tab2d_2=v_ice      , clinfo2=' v_ice       :')
         CALL prt_ctl(tab2d_1=d_u_ice_dyn, clinfo1=' lim_update  : d_u_ice_dyn :', tab2d_2=d_v_ice_dyn, clinfo2=' d_v_ice_dyn :')
         CALL prt_ctl(tab2d_1=old_u_ice  , clinfo1=' lim_update  : old_u_ice   :', tab2d_2=old_v_ice  , clinfo2=' old_v_ice   :')

         DO jl = 1, jpl
            CALL prt_ctl_info(' ')
            CALL prt_ctl_info(' - Category : ', ivar1=jl)
            CALL prt_ctl_info('   ~~~~~~~~~~')
            CALL prt_ctl(tab2d_1=ht_i       (:,:,jl)        , clinfo1= ' lim_update  : ht_i        : ')
            CALL prt_ctl(tab2d_1=ht_s       (:,:,jl)        , clinfo1= ' lim_update  : ht_s        : ')
            CALL prt_ctl(tab2d_1=t_su       (:,:,jl)        , clinfo1= ' lim_update  : t_su        : ')
            CALL prt_ctl(tab2d_1=t_s        (:,:,1,jl)      , clinfo1= ' lim_update  : t_snow      : ')
            CALL prt_ctl(tab2d_1=sm_i       (:,:,jl)        , clinfo1= ' lim_update  : sm_i        : ')
            CALL prt_ctl(tab2d_1=o_i        (:,:,jl)        , clinfo1= ' lim_update  : o_i         : ')
            CALL prt_ctl(tab2d_1=a_i        (:,:,jl)        , clinfo1= ' lim_update  : a_i         : ')
            CALL prt_ctl(tab2d_1=old_a_i    (:,:,jl)        , clinfo1= ' lim_update  : old_a_i     : ')
            CALL prt_ctl(tab2d_1=d_a_i_trp  (:,:,jl)        , clinfo1= ' lim_update  : d_a_i_trp   : ')
            CALL prt_ctl(tab2d_1=d_a_i_thd  (:,:,jl)        , clinfo1= ' lim_update  : d_a_i_thd   : ')
            CALL prt_ctl(tab2d_1=v_i        (:,:,jl)        , clinfo1= ' lim_update  : v_i         : ')
            CALL prt_ctl(tab2d_1=old_v_i    (:,:,jl)        , clinfo1= ' lim_update  : old_v_i     : ')
            CALL prt_ctl(tab2d_1=d_v_i_trp  (:,:,jl)        , clinfo1= ' lim_update  : d_v_i_trp   : ')
            CALL prt_ctl(tab2d_1=d_v_i_thd  (:,:,jl)        , clinfo1= ' lim_update  : d_v_i_thd   : ')
            CALL prt_ctl(tab2d_1=v_s        (:,:,jl)        , clinfo1= ' lim_update  : v_s         : ')
            CALL prt_ctl(tab2d_1=old_v_s    (:,:,jl)        , clinfo1= ' lim_update  : old_v_s     : ')
            CALL prt_ctl(tab2d_1=d_v_s_trp  (:,:,jl)        , clinfo1= ' lim_update  : d_v_s_trp   : ')
            CALL prt_ctl(tab2d_1=d_v_s_thd  (:,:,jl)        , clinfo1= ' lim_update  : d_v_s_thd   : ')
            CALL prt_ctl(tab2d_1=e_i        (:,:,1,jl)/1.0e9, clinfo1= ' lim_update  : e_i1        : ')
            CALL prt_ctl(tab2d_1=old_e_i    (:,:,1,jl)/1.0e9, clinfo1= ' lim_update  : old_e_i1    : ')
            CALL prt_ctl(tab2d_1=d_e_i_trp  (:,:,1,jl)/1.0e9, clinfo1= ' lim_update  : de_i1_trp   : ')
            CALL prt_ctl(tab2d_1=d_e_i_thd  (:,:,1,jl)/1.0e9, clinfo1= ' lim_update  : de_i1_thd   : ')
            CALL prt_ctl(tab2d_1=e_i        (:,:,2,jl)/1.0e9, clinfo1= ' lim_update  : e_i2        : ')
            CALL prt_ctl(tab2d_1=old_e_i    (:,:,2,jl)/1.0e9, clinfo1= ' lim_update  : old_e_i2    : ')
            CALL prt_ctl(tab2d_1=d_e_i_trp  (:,:,2,jl)/1.0e9, clinfo1= ' lim_update  : de_i2_trp   : ')
            CALL prt_ctl(tab2d_1=d_e_i_thd  (:,:,2,jl)/1.0e9, clinfo1= ' lim_update  : de_i2_thd   : ')
            CALL prt_ctl(tab2d_1=e_s        (:,:,1,jl)      , clinfo1= ' lim_update  : e_snow      : ')
            CALL prt_ctl(tab2d_1=old_e_s    (:,:,1,jl)      , clinfo1= ' lim_update  : old_e_snow  : ')
            CALL prt_ctl(tab2d_1=d_e_s_trp  (:,:,1,jl)/1.0e9, clinfo1= ' lim_update  : d_e_s_trp   : ')
            CALL prt_ctl(tab2d_1=d_e_s_thd  (:,:,1,jl)/1.0e9, clinfo1= ' lim_update  : d_e_s_thd   : ')
            CALL prt_ctl(tab2d_1=smv_i      (:,:,jl)        , clinfo1= ' lim_update  : smv_i       : ')
            CALL prt_ctl(tab2d_1=old_smv_i  (:,:,jl)        , clinfo1= ' lim_update  : old_smv_i   : ')
            CALL prt_ctl(tab2d_1=d_smv_i_trp(:,:,jl)        , clinfo1= ' lim_update  : d_smv_i_trp : ')
            CALL prt_ctl(tab2d_1=d_smv_i_thd(:,:,jl)        , clinfo1= ' lim_update  : d_smv_i_thd : ')
            CALL prt_ctl(tab2d_1=oa_i       (:,:,jl)        , clinfo1= ' lim_update  : oa_i        : ')
            CALL prt_ctl(tab2d_1=old_oa_i   (:,:,jl)        , clinfo1= ' lim_update  : old_oa_i    : ')
            CALL prt_ctl(tab2d_1=d_oa_i_trp (:,:,jl)        , clinfo1= ' lim_update  : d_oa_i_trp  : ')
            CALL prt_ctl(tab2d_1=d_oa_i_thd (:,:,jl)        , clinfo1= ' lim_update  : d_oa_i_thd  : ')
            CALL prt_ctl(tab2d_1=REAL(patho_case(:,:,jl))   , clinfo1= ' lim_update  : Path. case  : ')

            DO jk = 1, nlay_i
               CALL prt_ctl_info(' - Layer : ', ivar1=jk)
               CALL prt_ctl(tab2d_1=t_i(:,:,jk,jl) , clinfo1= ' lim_update  : t_i       : ')
            END DO
         END DO

         CALL prt_ctl_info(' ')
         CALL prt_ctl_info(' - Heat / FW fluxes : ')
         CALL prt_ctl_info('   ~~~~~~~~~~~~~~~~~~ ')
         CALL prt_ctl(tab2d_1=fmmec  , clinfo1= ' lim_update : fmmec : ', tab2d_2=fhmec     , clinfo2= ' fhmec     : ')
         CALL prt_ctl(tab2d_1=sst_m  , clinfo1= ' lim_update : sst   : ', tab2d_2=sss_m     , clinfo2= ' sss       : ')
         CALL prt_ctl(tab2d_1=fhbri  , clinfo1= ' lim_update : fhbri : ', tab2d_2=fheat_rpo , clinfo2= ' fheat_rpo : ')

         CALL prt_ctl_info(' ')
         CALL prt_ctl_info(' - Stresses : ')
         CALL prt_ctl_info('   ~~~~~~~~~~ ')
         CALL prt_ctl(tab2d_1=utau       , clinfo1= ' lim_update : utau      : ', tab2d_2=vtau       , clinfo2= ' vtau      : ')
         CALL prt_ctl(tab2d_1=utau_ice   , clinfo1= ' lim_update : utau_ice  : ', tab2d_2=vtau_ice   , clinfo2= ' vtau_ice  : ')
         CALL prt_ctl(tab2d_1=u_oce      , clinfo1= ' lim_update : u_oce     : ', tab2d_2=v_oce      , clinfo2= ' v_oce     : ')
      ENDIF

      CALL wrk_dealloc( jpi,jpj,jpl, internal_melt )   ! integer
      CALL wrk_dealloc( jkmax, zthick0, zqm0 )

   END SUBROUTINE lim_update
#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty Module               No sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_update     ! Empty routine
   END SUBROUTINE lim_update

#endif

END MODULE limupdate
