MODULE sbcice_lim
   !!======================================================================
   !!                       ***  MODULE  sbcice_lim  ***
   !! Surface module :  update the ocean surface boundary condition over ice
   !!       &           covered area using LIM sea-ice model
   !! Sea-Ice model  :  LIM-3 Sea ice model time-stepping
   !!=====================================================================
   !! History :  2.0  ! 2006-12  (M. Vancoppenolle) Original code
   !!            3.0  ! 2008-02  (C. Talandier)  Surface module from icestp.F90
   !!             -   ! 2008-04  (G. Madec)  sltyle and lim_ctl routine
   !!            3.3  ! 2010-11  (G. Madec) ice-ocean stress always computed at each ocean time-step
   !!            4.0  ! 2011-01  (A Porter)  dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3' :                                  LIM 3.0 sea-ice model
   !!----------------------------------------------------------------------
   !!   sbc_ice_lim  : sea-ice model time-stepping and update ocean sbc over ice-covered area
   !!   lim_ctl       : alerts in case of ice model crash
   !!   lim_prt_state : ice control print at a given grid point
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE par_ice         ! sea-ice parameters
   USE ice             ! LIM-3: ice variables
   USE iceini          ! LIM-3: ice initialisation
   USE dom_ice         ! LIM-3: ice domain

   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE sbc_ice         ! Surface boundary condition: ice   fields
   USE sbcblk_core     ! Surface boundary condition: CORE bulk
   USE sbcblk_clio     ! Surface boundary condition: CLIO bulk
   USE albedo          ! ocean & ice albedo

   USE phycst          ! Define parameters for the routines
   USE eosbn2          ! equation of state
   USE limdyn          ! Ice dynamics
   USE limtrp          ! Ice transport
   USE limthd          ! Ice thermodynamics
   USE limitd_th       ! Thermodynamics on ice thickness distribution 
   USE limitd_me       ! Mechanics on ice thickness distribution
   USE limsbc          ! sea surface boundary condition
   USE limdia          ! Ice diagnostics
   USE limwri          ! Ice outputs
   USE limrst          ! Ice restarts
   USE limupdate       ! update of global variables
   USE limvar          ! Ice variables switch

   USE c1d             ! 1D vertical configuration
   USE lbclnk          ! lateral boundary condition - MPP link
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! work arrays
   USE iom             ! I/O manager library
   USE in_out_manager  ! I/O manager
   USE prtctl          ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC sbc_ice_lim  ! routine called by sbcmod.F90
   
   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , UCL NEMO Consortium (2011)
   !! $Id: sbcice_lim.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_ice_lim( kt, kblk )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sbc_ice_lim  ***
      !!                   
      !! ** Purpose :   update the ocean surface boundary condition via the 
      !!                Louvain la Neuve Sea Ice Model time stepping 
      !!
      !! ** Method  :   ice model time stepping
      !!              - call the ice dynamics routine 
      !!              - call the ice advection/diffusion routine 
      !!              - call the ice thermodynamics routine 
      !!              - call the routine that computes mass and 
      !!                heat fluxes at the ice/ocean interface
      !!              - save the outputs 
      !!              - save the outputs for restart when necessary
      !!
      !! ** Action  : - time evolution of the LIM sea-ice model
      !!              - update all sbc variables below sea-ice:
      !!                utau, vtau, taum, wndm, qns , qsr, emp , emps
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt      ! ocean time step
      INTEGER, INTENT(in) ::   kblk    ! type of bulk (=3 CLIO, =4 CORE)
      !!
      INTEGER  ::   jl      ! dummy loop index
      REAL(wp) ::   zcoef   ! local scalar
      REAL(wp), POINTER, DIMENSION(:,:,:)   ::   zalb_ice_os, zalb_ice_cs  ! albedo of the ice under overcast/clear sky
      !!----------------------------------------------------------------------

      CALL wrk_alloc( jpi,jpj,jpl, zalb_ice_os, zalb_ice_cs )

      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'sbc_ice_lim : update ocean surface boudary condition' 
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~   via Louvain la Neuve Ice Model (LIM-3) time stepping'
         !
         CALL ice_init
         !
         IF( ln_nicep ) THEN      ! control print at a given point
            jiindx = 44   ;   jjindx = 140
            WRITE(numout,*) ' The debugging point is : jiindx : ',jiindx, ' jjindx : ',jjindx
         ENDIF
      ENDIF

      !                                        !----------------------!
      IF( MOD( kt-1, nn_fsbc ) == 0 ) THEN     !  Ice time-step only  !
         !                                     !----------------------!
         !                                           !  Bulk Formulea !
         !                                           !----------------!
         !
         u_oce(:,:) = ssu_m(:,:)                     ! mean surface ocean current at ice velocity point
         v_oce(:,:) = ssv_m(:,:)                     ! (C-grid dynamics :  U- & V-points as the ocean)
         !
         t_bo(:,:) = tfreez( sss_m ) +  rt0          ! masked sea surface freezing temperature [Kelvin]
         !                                           ! (set to rt0 over land)
         CALL albedo_ice( t_su, ht_i, ht_s, zalb_ice_cs, zalb_ice_os )  ! ... ice albedo

         DO jl = 1, jpl
            t_su(:,:,jl) = t_su(:,:,jl) +  rt0 * ( 1. - tmask(:,:,1) )
         END DO
                                                     ! Bulk formulea - provides the following fields:
         ! utau_ice, vtau_ice : surface ice stress                     (U- & V-points)   [N/m2]
         ! qsr_ice , qns_ice  : solar & non solar heat flux over ice   (T-point)         [W/m2]
         ! qla_ice            : latent heat flux over ice              (T-point)         [W/m2]
         ! dqns_ice, dqla_ice : non solar & latent heat sensistivity   (T-point)         [W/m2]
         ! tprecip , sprecip  : total & solid precipitation            (T-point)         [Kg/m2/s]
         ! fr1_i0  , fr2_i0   : 1sr & 2nd fraction of qsr penetration in ice             [%]
         !
         SELECT CASE( kblk )
         CASE( 3 )                                       ! CLIO bulk formulation
            CALL blk_ice_clio( t_su , zalb_ice_cs, zalb_ice_os,                           &
               &                      utau_ice   , vtau_ice   , qns_ice   , qsr_ice   ,   &
               &                      qla_ice    , dqns_ice   , dqla_ice  ,               &
               &                      tprecip    , sprecip    ,                           &
               &                      fr1_i0     , fr2_i0     , cp_ice_msh, jpl  )
            !         
         CASE( 4 )                                       ! CORE bulk formulation
            CALL blk_ice_core( t_su , u_ice     , v_ice     , zalb_ice_cs,               &
               &                      utau_ice  , vtau_ice  , qns_ice    , qsr_ice   ,   &
               &                      qla_ice   , dqns_ice  , dqla_ice   ,               &
               &                      tprecip   , sprecip   ,                            &
               &                      fr1_i0    , fr2_i0    , cp_ice_msh, jpl  )
         END SELECT

         !                                           !----------------------!
         !                                           ! LIM-3  time-stepping !
         !                                           !----------------------!
         ! 
         numit = numit + nn_fsbc                     ! Ice model time step
         !
         !                                           ! Store previous ice values
!!gm : remark   old_...   should becomes ...b  as tn versus tb  
         old_a_i  (:,:,:)   = a_i  (:,:,:)     ! ice area
         old_e_i  (:,:,:,:) = e_i  (:,:,:,:)   ! ice thermal energy
         old_v_i  (:,:,:)   = v_i  (:,:,:)     ! ice volume
         old_v_s  (:,:,:)   = v_s  (:,:,:)     ! snow volume 
         old_e_s  (:,:,:,:) = e_s  (:,:,:,:)   ! snow thermal energy
         old_smv_i(:,:,:)   = smv_i(:,:,:)     ! salt content
         old_oa_i (:,:,:)   = oa_i (:,:,:)     ! areal age content

         !                                           ! intialisation to zero    !!gm is it truly necessary ???
         d_a_i_thd  (:,:,:)   = 0.e0   ;   d_a_i_trp  (:,:,:)   = 0.e0
         d_v_i_thd  (:,:,:)   = 0.e0   ;   d_v_i_trp  (:,:,:)   = 0.e0
         d_e_i_thd  (:,:,:,:) = 0.e0   ;   d_e_i_trp  (:,:,:,:) = 0.e0
         d_v_s_thd  (:,:,:)   = 0.e0   ;   d_v_s_trp  (:,:,:)   = 0.e0
         d_e_s_thd  (:,:,:,:) = 0.e0   ;   d_e_s_trp  (:,:,:,:) = 0.e0
         d_smv_i_thd(:,:,:)   = 0.e0   ;   d_smv_i_trp(:,:,:)   = 0.e0
         d_oa_i_thd (:,:,:)   = 0.e0   ;   d_oa_i_trp (:,:,:)   = 0.e0
         !
         fseqv    (:,:) = 0.e0
         fsbri    (:,:) = 0.e0     ;   fsalt_res(:,:) = 0.e0
         fsalt_rpo(:,:) = 0.e0
         fhmec    (:,:) = 0.e0     ;   fhbri    (:,:) = 0.e0
         fmmec    (:,:) = 0.e0     ;   fheat_res(:,:) = 0.e0
         fheat_rpo(:,:) = 0.e0     ;   focea2D  (:,:) = 0.e0
         fsup2D   (:,:) = 0.e0
         ! 
         diag_sni_gr(:,:) = 0.e0   ;   diag_lat_gr(:,:) = 0.e0
         diag_bot_gr(:,:) = 0.e0   ;   diag_dyn_gr(:,:) = 0.e0
         diag_bot_me(:,:) = 0.e0   ;   diag_sur_me(:,:) = 0.e0
         ! dynamical invariants
         delta_i(:,:) = 0.e0       ;   divu_i(:,:) = 0.e0       ;   shear_i(:,:) = 0.e0

                          CALL lim_rst_opn( kt )     ! Open Ice restart file
         !
         IF( ln_nicep )   CALL lim_prt_state( jiindx, jjindx, 1, ' - Beginning the time step - ' )   ! control print
         !
         IF( .NOT. lk_c1d ) THEN
                                                     ! Ice dynamics & transport (not in 1D case)
                          CALL lim_dyn( kt )              ! Ice dynamics    ( rheology/dynamics )
                          CALL lim_trp( kt )              ! Ice transport   ( Advection/diffusion )
                          CALL lim_var_agg(1)             ! aggregate categories, requested
                          CALL lim_var_glo2eqv            ! equivalent variables, requested for rafting
         IF( ln_nicep )   CALL lim_prt_state( jiindx, jjindx,-1, ' - ice dyn & trp - ' )   ! control print
                          CALL lim_itd_me                 ! Mechanical redistribution ! (ridging/rafting)
         ENDIF
         !                                           ! Ice thermodynamics 
                          CALL lim_var_glo2eqv            ! equivalent variables
                          CALL lim_var_agg(1)             ! aggregate ice categories
                          CALL lim_var_bv                 ! bulk brine volume (diag)
                          CALL lim_thd( kt )              ! Ice thermodynamics 
                          zcoef = rdt_ice / 86400.e0      !  Ice natural aging
                          oa_i(:,:,:) = oa_i(:,:,:) + a_i(:,:,:) * zcoef
                          CALL lim_var_glo2eqv            ! this CALL is maybe not necessary (Martin)
         IF( ln_nicep )   CALL lim_prt_state( jiindx, jjindx, 1, ' - ice thermodyn. - ' )   ! control print
                          CALL lim_itd_th( kt )           !  Remap ice categories, lateral accretion  !
         !
         !                                           ! Global variables update
                          CALL lim_var_agg( 1 )           ! requested by limupdate
                          CALL lim_update                 ! Global variables update
                          CALL lim_var_glo2eqv            ! equivalent variables (outputs)
                          CALL lim_var_agg(2)             ! aggregate ice thickness categories
         IF( ln_nicep )   CALL lim_prt_state( jiindx, jjindx, 2, ' - Final state - ' )   ! control print
         !
                          CALL lim_sbc_flx( kt )     ! Update surface ocean mass, heat and salt fluxes
         !
         IF( ln_nicep )   CALL lim_prt_state( jiindx, jjindx, 3, ' - Final state lim_sbc - ' )   ! control print
         !
         !                                           ! Diagnostics and outputs 
         IF( ( MOD( kt+nn_fsbc-1, ninfo ) == 0 .OR. ntmoy == 1 ) .AND. .NOT. lk_mpp )   &
            &             CALL lim_dia 
                          CALL lim_wri( 1  )              ! Ice outputs 
         IF( lrst_ice )   CALL lim_rst_write( kt )        ! Ice restart file 
                          CALL lim_var_glo2eqv            ! ???
         !
         IF( ln_nicep )   CALL lim_ctl               ! alerts in case of model crash
         !
      ENDIF                                    ! End sea-ice time step only

      !                                        !--------------------------!
      !                                        !  at all ocean time step  !
      !                                        !--------------------------!
      !                                               
      !                                              ! Update surface ocean stresses (only in ice-dynamic case)
      !                                                   ! otherwise the atm.-ocean stresses are used everywhere
      IF( ln_limdyn )     CALL lim_sbc_tau( kt, ub(:,:,1), vb(:,:,1) )  ! using before instantaneous surf. currents
      
!!gm   remark, the ocean-ice stress is not saved in ice diag call above .....  find a solution!!!
      !
      CALL wrk_dealloc( jpi,jpj,jpl, zalb_ice_os, zalb_ice_cs )
      !
   END SUBROUTINE sbc_ice_lim


   SUBROUTINE lim_ctl
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_ctl *** 
      !!                 
      !! ** Purpose :   Alerts in case of model crash
      !!-------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk,  jl   ! dummy loop indices
      INTEGER  ::   inb_altests       ! number of alert tests (max 20)
      INTEGER  ::   ialert_id         ! number of the current alert
      REAL(wp) ::   ztmelts           ! ice layer melting point
      CHARACTER (len=30), DIMENSION(20)      ::   cl_alname   ! name of alert
      INTEGER           , DIMENSION(20)      ::   inb_alp     ! number of alerts positive
      !!-------------------------------------------------------------------

      inb_altests = 10
      inb_alp(:)  = 0

      ! Alert if incompatible volume and concentration
      ialert_id = 2 ! reference number of this alert
      cl_alname(ialert_id) = ' Incompat vol and con         '    ! name of the alert

      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF(  v_i(ji,jj,jl) /= 0.e0   .AND.   a_i(ji,jj,jl) == 0.e0   ) THEN
                  WRITE(numout,*) ' ALERTE 2 :   Incompatible volume and concentration '
                  WRITE(numout,*) ' at_i     ', at_i(ji,jj)
                  WRITE(numout,*) ' Point - category', ji, jj, jl
                  WRITE(numout,*) ' a_i *** a_i_old ', a_i      (ji,jj,jl), old_a_i  (ji,jj,jl)
                  WRITE(numout,*) ' v_i *** v_i_old ', v_i      (ji,jj,jl), old_v_i  (ji,jj,jl)
                  WRITE(numout,*) ' d_a_i_thd/trp   ', d_a_i_thd(ji,jj,jl), d_a_i_trp(ji,jj,jl)
                  WRITE(numout,*) ' d_v_i_thd/trp   ', d_v_i_thd(ji,jj,jl), d_v_i_trp(ji,jj,jl)
                  inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               ENDIF
            END DO
         END DO
      END DO

      ! Alerte if very thick ice
      ialert_id = 3 ! reference number of this alert
      cl_alname(ialert_id) = ' Very thick ice               ' ! name of the alert
      jl = jpl 
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF(   ht_i(ji,jj,jl) .GT. 50.0   ) THEN
               CALL lim_prt_state( ji, jj, 2, ' ALERTE 3 :   Very thick ice ' )
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END DO
      END DO

      ! Alert if very fast ice
      ialert_id = 4 ! reference number of this alert
      cl_alname(ialert_id) = ' Very fast ice               ' ! name of the alert
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF(   MAX( ABS( u_ice(ji,jj) ), ABS( v_ice(ji,jj) ) ) .GT. 0.5  .AND.  &
               &  at_i(ji,jj) .GT. 0.e0   ) THEN
               CALL lim_prt_state( ji, jj, 1, ' ALERTE 4 :   Very fast ice ' )
               WRITE(numout,*) ' ice strength             : ', strength(ji,jj)
               WRITE(numout,*) ' oceanic stress utau      : ', utau(ji,jj) 
               WRITE(numout,*) ' oceanic stress vtau      : ', vtau(ji,jj)
               WRITE(numout,*) ' sea-ice stress utau_ice  : ', utau_ice(ji,jj) 
               WRITE(numout,*) ' sea-ice stress vtau_ice  : ', vtau_ice(ji,jj)
               WRITE(numout,*) ' oceanic speed u          : ', u_oce(ji,jj)
               WRITE(numout,*) ' oceanic speed v          : ', v_oce(ji,jj)
               WRITE(numout,*) ' sst                      : ', sst_m(ji,jj)
               WRITE(numout,*) ' sss                      : ', sss_m(ji,jj)
               WRITE(numout,*) 
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END DO
      END DO

      ! Alert if there is ice on continents
      ialert_id = 6 ! reference number of this alert
      cl_alname(ialert_id) = ' Ice on continents           ' ! name of the alert
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF(   tms(ji,jj) .LE. 0.0   .AND.   at_i(ji,jj) .GT. 0.e0   ) THEN 
               CALL lim_prt_state( ji, jj, 1, ' ALERTE 6 :   Ice on continents ' )
               WRITE(numout,*) ' masks s, u, v        : ', tms(ji,jj), tmu(ji,jj), tmv(ji,jj) 
               WRITE(numout,*) ' sst                  : ', sst_m(ji,jj)
               WRITE(numout,*) ' sss                  : ', sss_m(ji,jj)
               WRITE(numout,*) ' at_i(ji,jj)          : ', at_i(ji,jj)
               WRITE(numout,*) ' v_ice(ji,jj)         : ', v_ice(ji,jj)
               WRITE(numout,*) ' v_ice(ji,jj-1)       : ', v_ice(ji,jj-1)
               WRITE(numout,*) ' u_ice(ji-1,jj)       : ', u_ice(ji-1,jj)
               WRITE(numout,*) ' u_ice(ji,jj)         : ', v_ice(ji,jj)
               !
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END DO
      END DO

!
!     ! Alert if very fresh ice
      ialert_id = 7 ! reference number of this alert
      cl_alname(ialert_id) = ' Very fresh ice               ' ! name of the alert
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
!!gm  test twice sm_i ...  ????  bug?
               IF( ( ( ABS( sm_i(ji,jj,jl) ) .LT. 0.50) .OR. &
                     ( ABS( sm_i(ji,jj,jl) ) .LT. 0.50) ) .AND. &
                             ( a_i(ji,jj,jl) .GT. 0.e0 ) ) THEN
!                 CALL lim_prt_state(ji,jj,1, ' ALERTE 7 :   Very fresh ice ' )
!                 WRITE(numout,*) ' sst                  : ', sst_m(ji,jj)
!                 WRITE(numout,*) ' sss                  : ', sss_m(ji,jj)
!                 WRITE(numout,*) ' s_i_newice           : ', s_i_newice(ji,jj,1:jpl)
!                 WRITE(numout,*) 
                  inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               ENDIF
            END DO
         END DO
      END DO
!

!     ! Alert if too old ice
      ialert_id = 9 ! reference number of this alert
      cl_alname(ialert_id) = ' Very old   ice               ' ! name of the alert
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF ( ( ( ABS( o_i(ji,jj,jl) ) .GT. rdt_ice ) .OR. &
                      ( ABS( o_i(ji,jj,jl) ) .LT. 0.00) ) .AND. &
                             ( a_i(ji,jj,jl) .GT. 0.0 ) ) THEN
                  CALL lim_prt_state( ji, jj, 1, ' ALERTE 9 :   Wrong ice age ')
                  inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               ENDIF
            END DO
         END DO
      END DO
 
      ! Alert on salt flux
      ialert_id = 5 ! reference number of this alert
      cl_alname(ialert_id) = ' High salt flux               ' ! name of the alert
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF( ABS( emps(ji,jj) ) .GT. 1.0e-2 ) THEN
               CALL lim_prt_state( ji, jj, 3, ' ALERTE 5 :   High salt flux ' )
               DO jl = 1, jpl
                  WRITE(numout,*) ' Category no: ', jl
                  WRITE(numout,*) ' a_i        : ', a_i      (ji,jj,jl) , ' old_a_i    : ', old_a_i  (ji,jj,jl)   
                  WRITE(numout,*) ' d_a_i_trp  : ', d_a_i_trp(ji,jj,jl) , ' d_a_i_thd  : ', d_a_i_thd(ji,jj,jl) 
                  WRITE(numout,*) ' v_i        : ', v_i      (ji,jj,jl) , ' old_v_i    : ', old_v_i  (ji,jj,jl)   
                  WRITE(numout,*) ' d_v_i_trp  : ', d_v_i_trp(ji,jj,jl) , ' d_v_i_thd  : ', d_v_i_thd(ji,jj,jl) 
                  WRITE(numout,*) ' '
               END DO
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
            ENDIF
         END DO
      END DO

      ! Alert if qns very big
      ialert_id = 8 ! reference number of this alert
      cl_alname(ialert_id) = ' fnsolar very big             ' ! name of the alert
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF(   ABS( qns(ji,jj) ) .GT. 1500.0   .AND.  ( at_i(ji,jj) .GT. 0.0 ) )  THEN
               !
               WRITE(numout,*) ' ALERTE 8 :   Very high non-solar heat flux'
               WRITE(numout,*) ' ji, jj    : ', ji, jj
               WRITE(numout,*) ' qns       : ', qns(ji,jj)
               WRITE(numout,*) ' sst       : ', sst_m(ji,jj)
               WRITE(numout,*) ' sss       : ', sss_m(ji,jj)
               WRITE(numout,*) ' qcmif     : ', qcmif(ji,jj)
               WRITE(numout,*) ' qldif     : ', qldif(ji,jj)
               WRITE(numout,*) ' qcmif     : ', qcmif(ji,jj) / rdt_ice
               WRITE(numout,*) ' qldif     : ', qldif(ji,jj) / rdt_ice
               WRITE(numout,*) ' qfvbq     : ', qfvbq(ji,jj)
               WRITE(numout,*) ' qdtcn     : ', qdtcn(ji,jj)
               WRITE(numout,*) ' qfvbq / dt: ', qfvbq(ji,jj) / rdt_ice
               WRITE(numout,*) ' qdtcn / dt: ', qdtcn(ji,jj) / rdt_ice
               WRITE(numout,*) ' fdtcn     : ', fdtcn(ji,jj) 
               WRITE(numout,*) ' fhmec     : ', fhmec(ji,jj) 
               WRITE(numout,*) ' fheat_rpo : ', fheat_rpo(ji,jj) 
               WRITE(numout,*) ' fheat_res : ', fheat_res(ji,jj) 
               WRITE(numout,*) ' fhbri     : ', fhbri(ji,jj) 
               !
               CALL lim_prt_state( ji, jj, 2, '   ')
               inb_alp(ialert_id) = inb_alp(ialert_id) + 1
               !
            ENDIF
         END DO
      END DO
      !+++++
 
      ! Alert if very warm ice
      ialert_id = 10 ! reference number of this alert
      cl_alname(ialert_id) = ' Very warm ice                ' ! name of the alert
      inb_alp(ialert_id) = 0
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  ztmelts    =  -tmut * s_i(ji,jj,jk,jl) + rtt
                  IF( t_i(ji,jj,jk,jl) .GE. ztmelts  .AND.  v_i(ji,jj,jl) .GT. 1.e-6   &
                     &                               .AND.  a_i(ji,jj,jl) .GT. 0.e0    ) THEN
                     WRITE(numout,*) ' ALERTE 10 :   Very warm ice'
                     WRITE(numout,*) ' ji, jj, jk, jl : ', ji, jj, jk, jl
                     WRITE(numout,*) ' t_i : ', t_i(ji,jj,jk,jl)
                     WRITE(numout,*) ' e_i : ', e_i(ji,jj,jk,jl)
                     WRITE(numout,*) ' s_i : ', s_i(ji,jj,jk,jl)
                     WRITE(numout,*) ' ztmelts : ', ztmelts
                     inb_alp(ialert_id) = inb_alp(ialert_id) + 1
                  ENDIF
               END DO
            END DO
         END DO
      END DO

      ialert_id = 1                                 ! reference number of this alert
      cl_alname(ialert_id) = ' NO alerte 1      '   ! name of the alert
      WRITE(numout,*)
      WRITE(numout,*) ' All alerts at the end of ice model '
      DO ialert_id = 1, inb_altests
         WRITE(numout,*) ialert_id, cl_alname(ialert_id)//' : ', inb_alp(ialert_id), ' times ! '
      END DO
      !
   END SUBROUTINE lim_ctl
 
   
   SUBROUTINE lim_prt_state( ki, kj, kn, cd1 )
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE lim_prt_state *** 
      !!                 
      !! ** Purpose :   Writes global ice state on the (i,j) point 
      !!                in ocean.ouput 
      !!                3 possibilities exist 
      !!                n = 1/-1 -> simple ice state (plus Mechanical Check if -1)
      !!                n = 2    -> exhaustive state
      !!                n = 3    -> ice/ocean salt fluxes
      !!
      !! ** input   :   point coordinates (i,j) 
      !!                n : number of the option
      !!-------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   ki, kj, kn    ! ocean gridpoint indices
      CHARACTER(len=*), INTENT(in) ::   cd1           !
      !!
      INTEGER :: jl
      !!-------------------------------------------------------------------

      WRITE(numout,*) cd1             ! print title

      !----------------
      !  Simple state
      !----------------

      IF ( kn == 1 .OR. kn == -1 ) THEN
         WRITE(numout,*) ' lim_prt_state - Point : ',ki,kj
         WRITE(numout,*) ' ~~~~~~~~~~~~~~ '
         WRITE(numout,*) ' Simple state '
         WRITE(numout,*) ' masks s,u,v   : ', tms(ki,kj), tmu(ki,kj), tmv(ki,kj)
         WRITE(numout,*) ' lat - long    : ', gphit(ki,kj), glamt(ki,kj)
         WRITE(numout,*) ' Time step     : ', numit
         WRITE(numout,*) ' - Ice drift   '
         WRITE(numout,*) '   ~~~~~~~~~~~ '
         WRITE(numout,*) ' u_ice(i-1,j)  : ', u_ice(ki-1,kj)
         WRITE(numout,*) ' u_ice(i  ,j)  : ', u_ice(ki,kj)
         WRITE(numout,*) ' v_ice(i  ,j-1): ', v_ice(ki,kj-1)
         WRITE(numout,*) ' v_ice(i  ,j)  : ', v_ice(ki,kj)
         WRITE(numout,*) ' strength      : ', strength(ki,kj)
         WRITE(numout,*)
         WRITE(numout,*) ' - Cell values '
         WRITE(numout,*) '   ~~~~~~~~~~~ '
         WRITE(numout,*) ' cell area     : ', area(ki,kj)
         WRITE(numout,*) ' at_i          : ', at_i(ki,kj)       
         WRITE(numout,*) ' vt_i          : ', vt_i(ki,kj)       
         WRITE(numout,*) ' vt_s          : ', vt_s(ki,kj)       
         DO jl = 1, jpl
            WRITE(numout,*) ' - Category (', jl,')'
            WRITE(numout,*) ' a_i           : ', a_i(ki,kj,jl)
            WRITE(numout,*) ' ht_i          : ', ht_i(ki,kj,jl)
            WRITE(numout,*) ' ht_s          : ', ht_s(ki,kj,jl)
            WRITE(numout,*) ' v_i           : ', v_i(ki,kj,jl)
            WRITE(numout,*) ' v_s           : ', v_s(ki,kj,jl)
            WRITE(numout,*) ' e_s           : ', e_s(ki,kj,1,jl)/1.0e9
            WRITE(numout,*) ' e_i           : ', e_i(ki,kj,1:nlay_i,jl)/1.0e9
            WRITE(numout,*) ' t_su          : ', t_su(ki,kj,jl)
            WRITE(numout,*) ' t_snow        : ', t_s(ki,kj,1,jl)
            WRITE(numout,*) ' t_i           : ', t_i(ki,kj,1:nlay_i,jl)
            WRITE(numout,*) ' sm_i          : ', sm_i(ki,kj,jl)
            WRITE(numout,*) ' smv_i         : ', smv_i(ki,kj,jl)
            WRITE(numout,*)
            WRITE(numout,*) ' Pathological case : ', patho_case(ki,kj,jl)
         END DO
      ENDIF
      IF( kn == -1 ) THEN
         WRITE(numout,*) ' Mechanical Check ************** '
         WRITE(numout,*) ' Check what means ice divergence '
         WRITE(numout,*) ' Total ice concentration ', at_i (ki,kj)
         WRITE(numout,*) ' Total lead fraction     ', ato_i(ki,kj)
         WRITE(numout,*) ' Sum of both             ', ato_i(ki,kj) + at_i(ki,kj)
         WRITE(numout,*) ' Sum of both minus 1     ', ato_i(ki,kj) + at_i(ki,kj) - 1.00
      ENDIF


      !--------------------
      !  Exhaustive state
      !--------------------

      IF ( kn .EQ. 2 ) THEN
         WRITE(numout,*) ' lim_prt_state - Point : ',ki,kj
         WRITE(numout,*) ' ~~~~~~~~~~~~~~ '
         WRITE(numout,*) ' Exhaustive state '
         WRITE(numout,*) ' lat - long ', gphit(ki,kj), glamt(ki,kj)
         WRITE(numout,*) ' Time step ', numit
         WRITE(numout,*) 
         WRITE(numout,*) ' - Cell values '
         WRITE(numout,*) '   ~~~~~~~~~~~ '
         WRITE(numout,*) ' cell area     : ', area(ki,kj)
         WRITE(numout,*) ' at_i          : ', at_i(ki,kj)       
         WRITE(numout,*) ' vt_i          : ', vt_i(ki,kj)       
         WRITE(numout,*) ' vt_s          : ', vt_s(ki,kj)       
         WRITE(numout,*) ' u_ice(i-1,j)  : ', u_ice(ki-1,kj)
         WRITE(numout,*) ' u_ice(i  ,j)  : ', u_ice(ki,kj)
         WRITE(numout,*) ' v_ice(i  ,j-1): ', v_ice(ki,kj-1)
         WRITE(numout,*) ' v_ice(i  ,j)  : ', v_ice(ki,kj)
         WRITE(numout,*) ' strength      : ', strength(ki,kj)
         WRITE(numout,*) ' d_u_ice_dyn   : ', d_u_ice_dyn(ki,kj), ' d_v_ice_dyn   : ', d_v_ice_dyn(ki,kj)
         WRITE(numout,*) ' old_u_ice     : ', old_u_ice(ki,kj)  , ' old_v_ice     : ', old_v_ice(ki,kj)  
         WRITE(numout,*)

         DO jl = 1, jpl
              WRITE(numout,*) ' - Category (',jl,')'
              WRITE(numout,*) '   ~~~~~~~~         ' 
              WRITE(numout,*) ' ht_i       : ', ht_i(ki,kj,jl)             , ' ht_s       : ', ht_s(ki,kj,jl)
              WRITE(numout,*) ' t_i        : ', t_i(ki,kj,1:nlay_i,jl)
              WRITE(numout,*) ' t_su       : ', t_su(ki,kj,jl)             , ' t_s        : ', t_s(ki,kj,1,jl)
              WRITE(numout,*) ' sm_i       : ', sm_i(ki,kj,jl)             , ' o_i        : ', o_i(ki,kj,jl)
              WRITE(numout,*) ' a_i        : ', a_i(ki,kj,jl)              , ' old_a_i    : ', old_a_i(ki,kj,jl)   
              WRITE(numout,*) ' d_a_i_trp  : ', d_a_i_trp(ki,kj,jl)        , ' d_a_i_thd  : ', d_a_i_thd(ki,kj,jl) 
              WRITE(numout,*) ' v_i        : ', v_i(ki,kj,jl)              , ' old_v_i    : ', old_v_i(ki,kj,jl)   
              WRITE(numout,*) ' d_v_i_trp  : ', d_v_i_trp(ki,kj,jl)        , ' d_v_i_thd  : ', d_v_i_thd(ki,kj,jl) 
              WRITE(numout,*) ' v_s        : ', v_s(ki,kj,jl)              , ' old_v_s    : ', old_v_s(ki,kj,jl)  
              WRITE(numout,*) ' d_v_s_trp  : ', d_v_s_trp(ki,kj,jl)        , ' d_v_s_thd  : ', d_v_s_thd(ki,kj,jl)
              WRITE(numout,*) ' e_i1       : ', e_i(ki,kj,1,jl)/1.0e9      , ' old_ei1    : ', old_e_i(ki,kj,1,jl)/1.0e9 
              WRITE(numout,*) ' de_i1_trp  : ', d_e_i_trp(ki,kj,1,jl)/1.0e9, ' de_i1_thd  : ', d_e_i_thd(ki,kj,1,jl)/1.0e9
              WRITE(numout,*) ' e_i2       : ', e_i(ki,kj,2,jl)/1.0e9      , ' old_ei2    : ', old_e_i(ki,kj,2,jl)/1.0e9  
              WRITE(numout,*) ' de_i2_trp  : ', d_e_i_trp(ki,kj,2,jl)/1.0e9, ' de_i2_thd  : ', d_e_i_thd(ki,kj,2,jl)/1.0e9
              WRITE(numout,*) ' e_snow     : ', e_s(ki,kj,1,jl)            , ' old_e_snow : ', old_e_s(ki,kj,1,jl) 
              WRITE(numout,*) ' d_e_s_trp  : ', d_e_s_trp(ki,kj,1,jl)      , ' d_e_s_thd  : ', d_e_s_thd(ki,kj,1,jl)
              WRITE(numout,*) ' smv_i      : ', smv_i(ki,kj,jl)            , ' old_smv_i  : ', old_smv_i(ki,kj,jl)   
              WRITE(numout,*) ' d_smv_i_trp: ', d_smv_i_trp(ki,kj,jl)      , ' d_smv_i_thd: ', d_smv_i_thd(ki,kj,jl) 
              WRITE(numout,*) ' oa_i       : ', oa_i(ki,kj,jl)             , ' old_oa_i   : ', old_oa_i(ki,kj,jl)
              WRITE(numout,*) ' d_oa_i_trp : ', d_oa_i_trp(ki,kj,jl)       , ' d_oa_i_thd : ', d_oa_i_thd(ki,kj,jl)
              WRITE(numout,*) ' Path. case : ', patho_case(ki,kj,jl)
        END DO !jl

        WRITE(numout,*)
        WRITE(numout,*) ' - Heat / FW fluxes '
        WRITE(numout,*) '   ~~~~~~~~~~~~~~~~ '
!       WRITE(numout,*) ' fsbri      : ', fsbri(ki,kj)
!       WRITE(numout,*) ' fseqv      : ', fseqv(ki,kj)
!       WRITE(numout,*) ' fsalt_res  : ', fsalt_res(ki,kj)
        WRITE(numout,*) ' fmmec      : ', fmmec(ki,kj)
        WRITE(numout,*) ' fhmec      : ', fhmec(ki,kj)
        WRITE(numout,*) ' fhbri      : ', fhbri(ki,kj)
        WRITE(numout,*) ' fheat_rpo  : ', fheat_rpo(ki,kj)
        WRITE(numout,*) 
        WRITE(numout,*) ' sst        : ', sst_m(ki,kj)  
        WRITE(numout,*) ' sss        : ', sss_m(ki,kj)  
        WRITE(numout,*) 
        WRITE(numout,*) ' - Stresses '
        WRITE(numout,*) '   ~~~~~~~~ '
        WRITE(numout,*) ' utau_ice   : ', utau_ice(ki,kj) 
        WRITE(numout,*) ' vtau_ice   : ', vtau_ice(ki,kj)
        WRITE(numout,*) ' utau       : ', utau(ki,kj) 
        WRITE(numout,*) ' vtau       : ', vtau(ki,kj)
        WRITE(numout,*) ' oc. vel. u : ', u_oce(ki,kj)
        WRITE(numout,*) ' oc. vel. v : ', v_oce(ki,kj)
     ENDIF

     !---------------------
     ! Salt / heat fluxes
     !---------------------

     IF ( kn .EQ. 3 ) THEN
        WRITE(numout,*) ' lim_prt_state - Point : ',ki,kj
        WRITE(numout,*) ' ~~~~~~~~~~~~~~ '
        WRITE(numout,*) ' - Salt / Heat Fluxes '
        WRITE(numout,*) '   ~~~~~~~~~~~~~~~~ '
        WRITE(numout,*) ' lat - long ', gphit(ki,kj), glamt(ki,kj)
        WRITE(numout,*) ' Time step ', numit
        WRITE(numout,*)
        WRITE(numout,*) ' - Heat fluxes at bottom interface ***'
        WRITE(numout,*) ' qsr        : ', qsr(ki,kj)
        WRITE(numout,*) ' qns        : ', qns(ki,kj)
        WRITE(numout,*)
        WRITE(numout,*) ' - Salt fluxes at bottom interface ***'
        WRITE(numout,*) ' emps       : ', emps(ki,kj)
        WRITE(numout,*) ' emp        : ', emp(ki,kj)
        WRITE(numout,*) ' fsbri      : ', fsbri(ki,kj)
        WRITE(numout,*) ' fseqv      : ', fseqv(ki,kj)
        WRITE(numout,*) ' fsalt_res  : ', fsalt_res(ki,kj)
        WRITE(numout,*) ' fsalt_rpo  : ', fsalt_rpo(ki,kj)
        WRITE(numout,*) ' - Heat fluxes at bottom interface ***'
        WRITE(numout,*) ' fheat_res  : ', fheat_res(ki,kj)
        WRITE(numout,*)
        WRITE(numout,*) ' - Momentum fluxes '
        WRITE(numout,*) ' utau      : ', utau(ki,kj) 
        WRITE(numout,*) ' vtau      : ', vtau(ki,kj)
      ENDIF
      WRITE(numout,*) ' '
      !
   END SUBROUTINE lim_prt_state

#else
   !!----------------------------------------------------------------------
   !!   Default option           Dummy module      NO LIM 3.0 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE sbc_ice_lim ( kt, kblk )     ! Dummy routine
      WRITE(*,*) 'sbc_ice_lim: You should not have seen this print! error?', kt, kblk
   END SUBROUTINE sbc_ice_lim
#endif

   !!======================================================================
END MODULE sbcice_lim
