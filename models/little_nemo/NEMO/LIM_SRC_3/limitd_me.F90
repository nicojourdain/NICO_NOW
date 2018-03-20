MODULE limitd_me
   !!======================================================================
   !!                       ***  MODULE limitd_me ***
   !! LIM-3 : Mechanical impact on ice thickness distribution      
   !!======================================================================
   !! History :  LIM  ! 2006-02  (M. Vancoppenolle) Original code 
   !!            3.2  ! 2009-07  (M. Vancoppenolle, Y. Aksenov, G. Madec) bug correction in smsw & fsalt_rpo
   !!            4.0  ! 2011-02  (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3' :                                    LIM3 sea-ice model
   !!----------------------------------------------------------------------
   USE par_oce          ! ocean parameters
   USE dom_oce          ! ocean domain
   USE phycst           ! physical constants (ocean directory) 
   USE sbc_oce          ! surface boundary condition: ocean fields
   USE thd_ice          ! LIM thermodynamics
   USE ice              ! LIM variables
   USE par_ice          ! LIM parameters
   USE dom_ice          ! LIM domain
   USE limthd_lac       ! LIM
   USE limvar           ! LIM
   USE limcons          ! LIM
   USE in_out_manager   ! I/O manager
   USE lbclnk           ! lateral boundary condition - MPP exchanges
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays
   USE prtctl           ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_itd_me               ! called by ice_stp
   PUBLIC   lim_itd_me_icestrength
   PUBLIC   lim_itd_me_init
   PUBLIC   lim_itd_me_zapsmall
   PUBLIC   lim_itd_me_alloc        ! called by iceini.F90

   REAL(wp)  ::   epsi11 = 1.e-11_wp   ! constant values
   REAL(wp)  ::   epsi10 = 1.e-10_wp   ! constant values
   REAL(wp)  ::   epsi06 = 1.e-06_wp   ! constant values

   !-----------------------------------------------------------------------
   ! Variables shared among ridging subroutines
   !-----------------------------------------------------------------------
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   asum     ! sum of total ice and open water area
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   aksum    ! ratio of area removed to area ridged

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   athorn   ! participation function; fraction of ridging/
   !                                                           !  closing associated w/ category n

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   hrmin    ! minimum ridge thickness
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   hrmax    ! maximum ridge thickness
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   hraft    ! thickness of rafted ice
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   krdg     ! mean ridge thickness/thickness of ridging ice 
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   aridge   ! participating ice ridging
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   araft    ! participating ice rafting

   REAL(wp), PARAMETER ::   krdgmin = 1.1_wp    ! min ridge thickness multiplier
   REAL(wp), PARAMETER ::   kraft   = 2.0_wp    ! rafting multipliyer

   REAL(wp) ::   Cp                             ! 
   !
   !-----------------------------------------------------------------------
   ! Ridging diagnostic arrays for history files
   !-----------------------------------------------------------------------
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   dardg1dt   ! rate of fractional area loss by ridging ice (1/s)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   dardg2dt   ! rate of fractional area gain by new ridges (1/s)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   dvirdgdt   ! rate of ice volume ridged (m/s)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   opening    ! rate of opening due to divergence/shear (1/s)
   !!----------------------------------------------------------------------
   !! NEMO/LIM3 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: limitd_me.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION lim_itd_me_alloc()
      !!---------------------------------------------------------------------!
      !!                ***  ROUTINE lim_itd_me_alloc ***
      !!---------------------------------------------------------------------!
      ALLOCATE(                                                                     &
         !* Variables shared among ridging subroutines
         &      asum (jpi,jpj)     , athorn(jpi,jpj,0:jpl)                    ,     &
         &      aksum(jpi,jpj)                                                ,     &
         !
         &      hrmin(jpi,jpj,jpl) , hraft(jpi,jpj,jpl) , aridge(jpi,jpj,jpl) ,     &
         &      hrmax(jpi,jpj,jpl) , krdg (jpi,jpj,jpl) , araft (jpi,jpj,jpl) ,     &
         !
         !* Ridging diagnostic arrays for history files
         &      dardg1dt(jpi,jpj)  , dardg2dt(jpi,jpj)                        ,     & 
         &      dvirdgdt(jpi,jpj)  , opening(jpi,jpj)                         , STAT=lim_itd_me_alloc )
         !
      IF( lim_itd_me_alloc /= 0 )   CALL ctl_warn( 'lim_itd_me_alloc: failed to allocate arrays' )
      !
   END FUNCTION lim_itd_me_alloc


   SUBROUTINE lim_itd_me
      !!---------------------------------------------------------------------!
      !!                ***  ROUTINE lim_itd_me ***
      !!
      !! ** Purpose :   computes the mechanical redistribution of ice thickness
      !!
      !! ** Method  :   Steps :
      !!       1) Thickness categories boundaries, ice / o.w. concentrations
      !!          Ridge preparation
      !!       2) Dynamical inputs (closing rate, divu_adv, opning)
      !!       3) Ridging iteration
      !!       4) Ridging diagnostics
      !!       5) Heat, salt and freshwater fluxes
      !!       6) Compute increments of tate variables and come back to old values
      !!
      !! References :   Flato, G. M., and W. D. Hibler III, 1995, JGR, 100, 18,611-18,626.
      !!                Hibler, W. D. III, 1980, MWR, 108, 1943-1973, 1980.
      !!                Rothrock, D. A., 1975: JGR, 80, 4514-4519.
      !!                Thorndike et al., 1975, JGR, 80, 4501-4513. 
      !!                Bitz et al., JGR, 2001
      !!                Amundrud and Melling, JGR 2005
      !!                Babko et al., JGR 2002 
      !!
      !!     This routine is based on CICE code and authors William H. Lipscomb,
      !!  and Elizabeth C. Hunke, LANL are gratefully acknowledged
      !!--------------------------------------------------------------------!
      INTEGER ::   ji, jj, jk, jl   ! dummy loop index
      INTEGER ::   niter, nitermax = 20   ! local integer 
      LOGICAL  ::   asum_error              ! flag for asum .ne. 1
      INTEGER  ::   iterate_ridging         ! if true, repeat the ridging
      REAL(wp) ::   w1, tmpfac, dti         ! local scalar
      CHARACTER (len = 15) ::   fieldid
      REAL(wp), POINTER, DIMENSION(:,:) ::   closing_net     ! net rate at which area is removed    (1/s)
                                                             ! (ridging ice area - area of new ridges) / dt
      REAL(wp), POINTER, DIMENSION(:,:) ::   divu_adv        ! divu as implied by transport scheme  (1/s)
      REAL(wp), POINTER, DIMENSION(:,:) ::   opning          ! rate of opening due to divergence/shear
      REAL(wp), POINTER, DIMENSION(:,:) ::   closing_gross   ! rate at which area removed, not counting area of new ridges
      REAL(wp), POINTER, DIMENSION(:,:) ::   msnow_mlt       ! mass of snow added to ocean (kg m-2)
      REAL(wp), POINTER, DIMENSION(:,:) ::   esnow_mlt       ! energy needed to melt snow in ocean (J m-2)
      REAL(wp), POINTER, DIMENSION(:,:) ::   vt_i_init, vt_i_final  !  ice volume summed over categories
      !!-----------------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, closing_net, divu_adv, opning, closing_gross, msnow_mlt, esnow_mlt, vt_i_init, vt_i_final )

      IF( numit == nstart  )   CALL lim_itd_me_init   ! Initialization (first time-step only)

      IF(ln_ctl) THEN
         CALL prt_ctl(tab2d_1=ato_i , clinfo1=' lim_itd_me: ato_i  : ', tab2d_2=at_i   , clinfo2=' at_i    : ')
         CALL prt_ctl(tab2d_1=divu_i, clinfo1=' lim_itd_me: divu_i : ', tab2d_2=delta_i, clinfo2=' delta_i : ')
      ENDIF

      !-----------------------------------------------------------------------------!
      ! 1) Thickness categories boundaries, ice / o.w. concentrations, init_ons
      !-----------------------------------------------------------------------------!
      ! Set hi_max(ncat) to a big value to ensure that all ridged ice 
      ! is thinner than hi_max(ncat).

      hi_max(jpl) = 999.99

      Cp = 0.5 * grav * (rau0-rhoic) * rhoic / rau0      ! proport const for PE
      CALL lim_itd_me_ridgeprep ! prepare ridging

      IF( con_i)   CALL lim_column_sum( jpl, v_i, vt_i_init )      ! conservation check

      DO jj = 1, jpj                                     ! Initialize arrays.
         DO ji = 1, jpi
            msnow_mlt(ji,jj) = 0._wp
            esnow_mlt(ji,jj) = 0._wp
            dardg1dt (ji,jj)  = 0._wp
            dardg2dt (ji,jj)  = 0._wp
            dvirdgdt (ji,jj)  = 0._wp
            opening  (ji,jj)  = 0._wp

            !-----------------------------------------------------------------------------!
            ! 2) Dynamical inputs (closing rate, divu_adv, opning)
            !-----------------------------------------------------------------------------!
            !
            ! 2.1 closing_net
            !-----------------
            ! Compute the net rate of closing due to convergence 
            ! and shear, based on Flato and Hibler (1995).
            ! 
            ! The energy dissipation rate is equal to the net closing rate
            ! times the ice strength.
            !
            ! NOTE: The NET closing rate is equal to the rate that open water 
            !  area is removed, plus the rate at which ice area is removed by 
            !  ridging, minus the rate at which area is added in new ridges.
            !  The GROSS closing rate is equal to the first two terms (open
            !  water closing and thin ice ridging) without the third term
            !  (thick, newly ridged ice).

            closing_net(ji,jj) = Cs * 0.5 * ( Delta_i(ji,jj) - ABS( divu_i(ji,jj) ) ) - MIN( divu_i(ji,jj), 0._wp )

            ! 2.2 divu_adv
            !--------------
            ! Compute divu_adv, the divergence rate given by the transport/
            ! advection scheme, which may not be equal to divu as computed 
            ! from the velocity field.
            !
            ! If divu_adv < 0, make sure the closing rate is large enough
            ! to give asum = 1.0 after ridging.

            divu_adv(ji,jj) = ( 1._wp - asum(ji,jj) ) / rdt_ice  ! asum found in ridgeprep

            IF( divu_adv(ji,jj) < 0._wp )   closing_net(ji,jj) = MAX( closing_net(ji,jj), -divu_adv(ji,jj) )

            ! 2.3 opning
            !------------
            ! Compute the (non-negative) opening rate that will give 
            ! asum = 1.0 after ridging.
            opning(ji,jj) = closing_net(ji,jj) + divu_adv(ji,jj)
         END DO
      END DO

      !-----------------------------------------------------------------------------!
      ! 3) Ridging iteration
      !-----------------------------------------------------------------------------!
      niter           = 1                 ! iteration counter
      iterate_ridging = 1

      DO WHILE ( iterate_ridging > 0 .AND. niter < nitermax )

         DO jj = 1, jpj
            DO ji = 1, jpi

               ! 3.2 closing_gross
               !-----------------------------------------------------------------------------!
               ! Based on the ITD of ridging and ridged ice, convert the net
               !  closing rate to a gross closing rate.  
               ! NOTE: 0 < aksum <= 1
               closing_gross(ji,jj) = closing_net(ji,jj) / aksum(ji,jj)

               ! correction to closing rate and opening if closing rate is excessive
               !---------------------------------------------------------------------
               ! Reduce the closing rate if more than 100% of the open water 
               ! would be removed.  Reduce the opening rate proportionately.
               IF ( ato_i(ji,jj) .GT. epsi11 .AND. athorn(ji,jj,0) .GT. 0.0 ) THEN
                  w1 = athorn(ji,jj,0) * closing_gross(ji,jj) * rdt_ice
                  IF ( w1 .GT. ato_i(ji,jj)) THEN
                     tmpfac = ato_i(ji,jj) / w1
                     closing_gross(ji,jj) = closing_gross(ji,jj) * tmpfac
                     opning(ji,jj) = opning(ji,jj) * tmpfac
                  ENDIF !w1
               ENDIF !at0i and athorn

            END DO ! ji
         END DO ! jj

         ! correction to closing rate / opening if excessive ice removal
         !---------------------------------------------------------------
         ! Reduce the closing rate if more than 100% of any ice category 
         ! would be removed.  Reduce the opening rate proportionately.

         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, jpi
                  IF ( a_i(ji,jj,jl) > epsi11 .AND. athorn(ji,jj,jl) > 0._wp )THEN
                     w1 = athorn(ji,jj,jl) * closing_gross(ji,jj) * rdt_ice
                     IF ( w1 > a_i(ji,jj,jl) ) THEN
                        tmpfac = a_i(ji,jj,jl) / w1
                        closing_gross(ji,jj) = closing_gross(ji,jj) * tmpfac
                        opning       (ji,jj) = opning       (ji,jj) * tmpfac
                     ENDIF
                  ENDIF
               END DO !ji
            END DO ! jj
         END DO !jl

         ! 3.3 Redistribute area, volume, and energy.
         !-----------------------------------------------------------------------------!

         CALL lim_itd_me_ridgeshift( opning, closing_gross, msnow_mlt, esnow_mlt )

         ! 3.4 Compute total area of ice plus open water after ridging.
         !-----------------------------------------------------------------------------!

         CALL lim_itd_me_asumr

         ! 3.5 Do we keep on iterating ???
         !-----------------------------------------------------------------------------!
         ! Check whether asum = 1.  If not (because the closing and opening
         ! rates were reduced above), ridge again with new rates.

         iterate_ridging = 0

         DO jj = 1, jpj
            DO ji = 1, jpi
               IF (ABS(asum(ji,jj) - 1.0) .LT. epsi11) THEN
                  closing_net(ji,jj) = 0._wp
                  opning     (ji,jj) = 0._wp
               ELSE
                  iterate_ridging    = 1
                  divu_adv   (ji,jj) = (1._wp - asum(ji,jj)) / rdt_ice
                  closing_net(ji,jj) = MAX( 0._wp, -divu_adv(ji,jj) )
                  opning     (ji,jj) = MAX( 0._wp,  divu_adv(ji,jj) )
               ENDIF
            END DO
         END DO

         IF( lk_mpp )   CALL mpp_max( iterate_ridging )

         ! Repeat if necessary.
         ! NOTE: If strength smoothing is turned on, the ridging must be
         ! iterated globally because of the boundary update in the 
         ! smoothing.

         niter = niter + 1

         IF( iterate_ridging == 1 ) THEN
            IF( niter .GT. nitermax ) THEN
               WRITE(numout,*) ' ALERTE : non-converging ridging scheme '
               WRITE(numout,*) ' niter, iterate_ridging ', niter, iterate_ridging
            ENDIF
            CALL lim_itd_me_ridgeprep
         ENDIF

      END DO !! on the do while over iter

      !-----------------------------------------------------------------------------!
      ! 4) Ridging diagnostics
      !-----------------------------------------------------------------------------!
      ! Convert ridging rate diagnostics to correct units.
      ! Update fresh water and heat fluxes due to snow melt.

      dti = 1._wp / rdt_ice

      asum_error = .false. 

      DO jj = 1, jpj
         DO ji = 1, jpi

            IF (ABS(asum(ji,jj) - 1.0) .GT. epsi11) asum_error = .true.

            dardg1dt(ji,jj) = dardg1dt(ji,jj) * dti
            dardg2dt(ji,jj) = dardg2dt(ji,jj) * dti
            dvirdgdt(ji,jj) = dvirdgdt(ji,jj) * dti
            opening (ji,jj) = opening (ji,jj) * dti

            !-----------------------------------------------------------------------------!
            ! 5) Heat, salt and freshwater fluxes
            !-----------------------------------------------------------------------------!
            fmmec(ji,jj) = fmmec(ji,jj) + msnow_mlt(ji,jj) * dti     ! fresh water source for ocean
            fhmec(ji,jj) = fhmec(ji,jj) + esnow_mlt(ji,jj) * dti     ! heat sink for ocean

         END DO
      END DO

      ! Check if there is a ridging error
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF (ABS(asum(ji,jj) - 1.0) .GT. epsi11) THEN ! there is a bug
               WRITE(numout,*) ' '
               WRITE(numout,*) ' ALERTE : Ridging error: total area = ', asum(ji,jj)
               WRITE(numout,*) ' limitd_me '
               WRITE(numout,*) ' POINT : ', ji, jj
               WRITE(numout,*) ' jpl, a_i, athorn '
               WRITE(numout,*) 0, ato_i(ji,jj), athorn(ji,jj,0)
               DO jl = 1, jpl
                  WRITE(numout,*) jl, a_i(ji,jj,jl), athorn(ji,jj,jl)
               END DO
            ENDIF  ! asum

         END DO !ji
      END DO !jj

      ! Conservation check
      IF ( con_i ) THEN
         CALL lim_column_sum (jpl,   v_i, vt_i_final)
         fieldid = ' v_i : limitd_me '
         CALL lim_cons_check (vt_i_init, vt_i_final, 1.0e-6, fieldid) 
      ENDIF

      !-----------------------------------------------------------------------------!
      ! 6) Updating state variables and trend terms
      !-----------------------------------------------------------------------------!

      CALL lim_var_glo2eqv
      CALL lim_itd_me_zapsmall

      !-----------------
      !  Trend terms
      !-----------------

      d_u_ice_dyn(:,:)     = u_ice(:,:)     - old_u_ice(:,:)
      d_v_ice_dyn(:,:)     = v_ice(:,:)     - old_v_ice(:,:)
      d_a_i_trp  (:,:,:)   = a_i  (:,:,:)   - old_a_i  (:,:,:)
      d_v_s_trp  (:,:,:)   = v_s  (:,:,:)   - old_v_s  (:,:,:)  
      d_v_i_trp  (:,:,:)   = v_i  (:,:,:)   - old_v_i  (:,:,:)   
      d_e_s_trp  (:,:,:,:) = e_s  (:,:,:,:) - old_e_s  (:,:,:,:)  
      d_e_i_trp  (:,:,:,:) = e_i  (:,:,:,:) - old_e_i  (:,:,:,:)
      d_oa_i_trp (:,:,:)   = oa_i (:,:,:)   - old_oa_i (:,:,:)
      d_smv_i_trp(:,:,:)   = 0._wp
      IF(  num_sal == 2  .OR.  num_sal == 4  )   d_smv_i_trp(:,:,:)  = smv_i(:,:,:) - old_smv_i(:,:,:)

      IF(ln_ctl) THEN     ! Control print
         CALL prt_ctl_info(' ')
         CALL prt_ctl_info(' - Cell values : ')
         CALL prt_ctl_info('   ~~~~~~~~~~~~~ ')
         CALL prt_ctl(tab2d_1=area , clinfo1=' lim_itd_me  : cell area :')
         CALL prt_ctl(tab2d_1=at_i , clinfo1=' lim_itd_me  : at_i      :')
         CALL prt_ctl(tab2d_1=vt_i , clinfo1=' lim_itd_me  : vt_i      :')
         CALL prt_ctl(tab2d_1=vt_s , clinfo1=' lim_itd_me  : vt_s      :')
         DO jl = 1, jpl
            CALL prt_ctl_info(' ')
            CALL prt_ctl_info(' - Category : ', ivar1=jl)
            CALL prt_ctl_info('   ~~~~~~~~~~')
            CALL prt_ctl(tab2d_1=a_i   (:,:,jl)   , clinfo1= ' lim_itd_me  : a_i      : ')
            CALL prt_ctl(tab2d_1=ht_i  (:,:,jl)   , clinfo1= ' lim_itd_me  : ht_i     : ')
            CALL prt_ctl(tab2d_1=ht_s  (:,:,jl)   , clinfo1= ' lim_itd_me  : ht_s     : ')
            CALL prt_ctl(tab2d_1=v_i   (:,:,jl)   , clinfo1= ' lim_itd_me  : v_i      : ')
            CALL prt_ctl(tab2d_1=v_s   (:,:,jl)   , clinfo1= ' lim_itd_me  : v_s      : ')
            CALL prt_ctl(tab2d_1=e_s   (:,:,1,jl) , clinfo1= ' lim_itd_me  : e_s      : ')
            CALL prt_ctl(tab2d_1=t_su  (:,:,jl)   , clinfo1= ' lim_itd_me  : t_su     : ')
            CALL prt_ctl(tab2d_1=t_s   (:,:,1,jl) , clinfo1= ' lim_itd_me  : t_snow   : ')
            CALL prt_ctl(tab2d_1=sm_i  (:,:,jl)   , clinfo1= ' lim_itd_me  : sm_i     : ')
            CALL prt_ctl(tab2d_1=smv_i (:,:,jl)   , clinfo1= ' lim_itd_me  : smv_i    : ')
            DO jk = 1, nlay_i
               CALL prt_ctl_info(' ')
               CALL prt_ctl_info(' - Layer : ', ivar1=jk)
               CALL prt_ctl_info('   ~~~~~~~')
               CALL prt_ctl(tab2d_1=t_i(:,:,jk,jl) , clinfo1= ' lim_itd_me  : t_i      : ')
               CALL prt_ctl(tab2d_1=e_i(:,:,jk,jl) , clinfo1= ' lim_itd_me  : e_i      : ')
            END DO
         END DO
      ENDIF

      !-------------------------!
      ! Back to initial values
      !-------------------------!

      ! update of fields will be made later in lim update
      u_ice(:,:)    = old_u_ice(:,:)
      v_ice(:,:)    = old_v_ice(:,:)
      a_i(:,:,:)    = old_a_i(:,:,:)
      v_s(:,:,:)    = old_v_s(:,:,:)
      v_i(:,:,:)    = old_v_i(:,:,:)
      e_s(:,:,:,:)  = old_e_s(:,:,:,:)
      e_i(:,:,:,:)  = old_e_i(:,:,:,:)
      oa_i(:,:,:)   = old_oa_i(:,:,:)
      IF(  num_sal == 2  .OR.  num_sal == 4  )   smv_i(:,:,:)  = old_smv_i(:,:,:)

      !----------------------------------------------------!
      ! Advection of ice in a free cell, newly ridged ice
      !----------------------------------------------------!

      ! to allow for thermodynamics to melt new ice
      ! we immediately advect ice in free cells

      ! heat content has to be corrected before ice volume
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  IF ( ( old_v_i(ji,jj,jl) < epsi06 ) .AND. &
                     ( d_v_i_trp(ji,jj,jl) > epsi06 ) ) THEN
                     old_e_i(ji,jj,jk,jl)   = d_e_i_trp(ji,jj,jk,jl)
                     d_e_i_trp(ji,jj,jk,jl) = 0._wp
                  ENDIF
               END DO
            END DO
         END DO
      END DO

      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF ( ( old_v_i(ji,jj,jl) < epsi06 ) .AND. &
                  ( d_v_i_trp(ji,jj,jl) > epsi06 ) ) THEN
                  old_v_i(ji,jj,jl)     = d_v_i_trp(ji,jj,jl)
                  d_v_i_trp(ji,jj,jl)   = 0._wp
                  old_a_i(ji,jj,jl)     = d_a_i_trp(ji,jj,jl)
                  d_a_i_trp(ji,jj,jl)   = 0._wp
                  old_v_s(ji,jj,jl)     = d_v_s_trp(ji,jj,jl)
                  d_v_s_trp(ji,jj,jl)   = 0._wp
                  old_e_s(ji,jj,1,jl)   = d_e_s_trp(ji,jj,1,jl)
                  d_e_s_trp(ji,jj,1,jl) = 0._wp
                  old_oa_i(ji,jj,jl)    = d_oa_i_trp(ji,jj,jl)
                  d_oa_i_trp(ji,jj,jl)  = 0._wp
                  IF(  num_sal == 2  .OR.  num_sal == 4  )   old_smv_i(ji,jj,jl)   = d_smv_i_trp(ji,jj,jl)
                  d_smv_i_trp(ji,jj,jl) = 0._wp
               ENDIF
            END DO
         END DO
      END DO

      CALL wrk_dealloc( jpi, jpj, closing_net, divu_adv, opning, closing_gross, msnow_mlt, esnow_mlt, vt_i_init, vt_i_final )
      !
   END SUBROUTINE lim_itd_me


   SUBROUTINE lim_itd_me_icestrength( kstrngth )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE lim_itd_me_icestrength ***
      !!
      !! ** Purpose :   computes ice strength used in dynamics routines of ice thickness
      !!
      !! ** Method  :   Compute the strength of the ice pack, defined as the energy (J m-2) 
      !!              dissipated per unit area removed from the ice pack under compression,
      !!              and assumed proportional to the change in potential energy caused
      !!              by ridging. Note that only Hibler's formulation is stable and that
      !!              ice strength has to be smoothed
      !!
      !! ** Inputs / Ouputs : kstrngth (what kind of ice strength we are using)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kstrngth    ! = 1 for Rothrock formulation, 0 for Hibler (1979)

      INTEGER ::   ji,jj, jl   ! dummy loop indices
      INTEGER ::   ksmooth     ! smoothing the resistance to deformation
      INTEGER ::   numts_rm    ! number of time steps for the P smoothing
      REAL(wp) ::   hi, zw1, zp, zdummy, zzc, z1_3   ! local scalars
      REAL(wp), POINTER, DIMENSION(:,:) ::   zworka   ! temporary array used here
      !!----------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, zworka )

      !------------------------------------------------------------------------------!
      ! 1) Initialize
      !------------------------------------------------------------------------------!
      strength(:,:) = 0._wp

      !------------------------------------------------------------------------------!
      ! 2) Compute thickness distribution of ridging and ridged ice
      !------------------------------------------------------------------------------!
      CALL lim_itd_me_ridgeprep

      !------------------------------------------------------------------------------!
      ! 3) Rothrock(1975)'s method
      !------------------------------------------------------------------------------!
      IF( kstrngth == 1 ) THEN
         z1_3 = 1._wp / 3._wp
         DO jl = 1, jpl
            DO jj= 1, jpj
               DO ji = 1, jpi
                  !
                  IF(  a_i(ji,jj,jl)    > epsi11  .AND.     &
                       athorn(ji,jj,jl) > 0._wp   ) THEN
                     hi = v_i(ji,jj,jl) / a_i(ji,jj,jl)
                     !----------------------------
                     ! PE loss from deforming ice
                     !----------------------------
                     strength(ji,jj) = strength(ji,jj) - athorn(ji,jj,jl) * hi * hi

                     !--------------------------
                     ! PE gain from rafting ice
                     !--------------------------
                     strength(ji,jj) = strength(ji,jj) + 2._wp * araft(ji,jj,jl) * hi * hi

                     !----------------------------
                     ! PE gain from ridging ice
                     !----------------------------
                     strength(ji,jj) = strength(ji,jj) + aridge(ji,jj,jl)/krdg(ji,jj,jl)     &
                        * z1_3 * (hrmax(ji,jj,jl)**3 - hrmin(ji,jj,jl)**3) / ( hrmax(ji,jj,jl)-hrmin(ji,jj,jl) )   
!!gm Optimization:  (a**3-b**3)/(a-b) = a*a+ab+b*b   ==> less costly operations even if a**3 is replaced by a*a*a...                    
                  ENDIF            ! aicen > epsi11
                  !
               END DO ! ji
            END DO !jj
         END DO !jl

         zzc = Cf * Cp     ! where Cp = (g/2)*(rhow-rhoi)*(rhoi/rhow) and Cf accounts for frictional dissipation
         strength(:,:) = zzc * strength(:,:) / aksum(:,:)

         ksmooth = 1

         !------------------------------------------------------------------------------!
         ! 4) Hibler (1979)' method
         !------------------------------------------------------------------------------!
      ELSE                      ! kstrngth ne 1:  Hibler (1979) form
         !
         strength(:,:) = Pstar * vt_i(:,:) * EXP( - C_rhg * ( 1._wp - at_i(:,:) )  )
         !
         ksmooth = 1
         !
      ENDIF                     ! kstrngth

      !
      !------------------------------------------------------------------------------!
      ! 5) Impact of brine volume
      !------------------------------------------------------------------------------!
      ! CAN BE REMOVED
      !
      IF ( brinstren_swi == 1 ) THEN

         DO jj = 1, jpj
            DO ji = 1, jpi
               IF ( bv_i(ji,jj) .GT. 0.0 ) THEN
                  zdummy = MIN ( bv_i(ji,jj), 0.10 ) * MIN( bv_i(ji,jj), 0.10 )
               ELSE
                  zdummy = 0.0
               ENDIF
               strength(ji,jj) = strength(ji,jj) * exp(-5.88*SQRT(MAX(bv_i(ji,jj),0.0)))
            END DO              ! j
         END DO                 ! i

      ENDIF

      !
      !------------------------------------------------------------------------------!
      ! 6) Smoothing ice strength
      !------------------------------------------------------------------------------!
      !
      !-------------------
      ! Spatial smoothing
      !-------------------
      IF ( ksmooth == 1 ) THEN

         CALL lbc_lnk( strength, 'T', 1. )

         DO jj = 2, jpj - 1
            DO ji = 2, jpi - 1
               IF ( ( asum(ji,jj) - ato_i(ji,jj) ) .GT. epsi11) THEN ! ice is
                  ! present
                  zworka(ji,jj) = 4.0 * strength(ji,jj)              &
                     + strength(ji-1,jj) * tms(ji-1,jj) &  
                     + strength(ji+1,jj) * tms(ji+1,jj) &  
                     + strength(ji,jj-1) * tms(ji,jj-1) &  
                     + strength(ji,jj+1) * tms(ji,jj+1)    

                  zw1 = 4.0 + tms(ji-1,jj) + tms(ji+1,jj) + tms(ji,jj-1) + tms(ji,jj+1)
                  zworka(ji,jj) = zworka(ji,jj) / zw1
               ELSE
                  zworka(ji,jj) = 0.0
               ENDIF
            END DO
         END DO

         DO jj = 2, jpj - 1
            DO ji = 2, jpi - 1
               strength(ji,jj) = zworka(ji,jj)
            END DO
         END DO
         CALL lbc_lnk( strength, 'T', 1. )

      ENDIF ! ksmooth

      !--------------------
      ! Temporal smoothing
      !--------------------
      IF ( numit == nit000 + nn_fsbc - 1 ) THEN
         strp1(:,:) = 0.0            
         strp2(:,:) = 0.0            
      ENDIF

      IF ( ksmooth == 2 ) THEN


         CALL lbc_lnk( strength, 'T', 1. )

         DO jj = 1, jpj - 1
            DO ji = 1, jpi - 1
               IF ( ( asum(ji,jj) - ato_i(ji,jj) ) .GT. epsi11) THEN       ! ice is present
                  numts_rm = 1 ! number of time steps for the running mean
                  IF ( strp1(ji,jj) .GT. 0.0 ) numts_rm = numts_rm + 1
                  IF ( strp2(ji,jj) .GT. 0.0 ) numts_rm = numts_rm + 1
                  zp = ( strength(ji,jj) + strp1(ji,jj) + strp2(ji,jj) ) / numts_rm
                  strp2(ji,jj) = strp1(ji,jj)
                  strp1(ji,jj) = strength(ji,jj)
                  strength(ji,jj) = zp

               ENDIF
            END DO
         END DO

      ENDIF ! ksmooth

      CALL lbc_lnk( strength, 'T', 1. )      ! Boundary conditions

      CALL wrk_dealloc( jpi, jpj, zworka )
      !
   END SUBROUTINE lim_itd_me_icestrength


   SUBROUTINE lim_itd_me_ridgeprep
      !!---------------------------------------------------------------------!
      !!                ***  ROUTINE lim_itd_me_ridgeprep ***
      !!
      !! ** Purpose :   preparation for ridging and strength calculations
      !!
      !! ** Method  :   Compute the thickness distribution of the ice and open water 
      !!              participating in ridging and of the resulting ridges.
      !!---------------------------------------------------------------------!
      INTEGER ::   ji,jj, jl    ! dummy loop indices
      INTEGER ::   krdg_index   ! 
      REAL(wp) ::   Gstari, astari, hi, hrmean, zdummy   ! local scalar
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zworka    ! temporary array used here
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   Gsum      ! Gsum(n) = sum of areas in categories 0 to n
      !------------------------------------------------------------------------------!

      CALL wrk_alloc( jpi,jpj, zworka )
      CALL wrk_alloc( jpi,jpj,jpl+2, Gsum, kkstart = -1 )

      Gstari     = 1.0/Gstar    
      astari     = 1.0/astar    
      aksum(:,:)    = 0.0
      athorn(:,:,:) = 0.0
      aridge(:,:,:) = 0.0
      araft (:,:,:) = 0.0
      hrmin(:,:,:)  = 0.0 
      hrmax(:,:,:)  = 0.0 
      hraft(:,:,:)  = 0.0 
      krdg (:,:,:)  = 1.0

      !     ! Zero out categories with very small areas
      CALL lim_itd_me_zapsmall

      !------------------------------------------------------------------------------!
      ! 1) Participation function 
      !------------------------------------------------------------------------------!

      ! Compute total area of ice plus open water.
      CALL lim_itd_me_asumr
      ! This is in general not equal to one 
      ! because of divergence during transport

      ! Compute cumulative thickness distribution function
      ! Compute the cumulative thickness distribution function Gsum,
      ! where Gsum(n) is the fractional area in categories 0 to n.
      ! initial value (in h = 0) equals open water area

      Gsum(:,:,-1) = 0._wp

      DO jj = 1, jpj
         DO ji = 1, jpi
            IF( ato_i(ji,jj) > epsi11 ) THEN   ;   Gsum(ji,jj,0) = ato_i(ji,jj)
            ELSE                               ;   Gsum(ji,jj,0) = 0._wp
            ENDIF
         END DO
      END DO

      ! for each value of h, you have to add ice concentration then
      DO jl = 1, jpl
         DO jj = 1, jpj 
            DO ji = 1, jpi
               IF( a_i(ji,jj,jl) .GT. epsi11 ) THEN   ;   Gsum(ji,jj,jl) = Gsum(ji,jj,jl-1) + a_i(ji,jj,jl)
               ELSE                                   ;   Gsum(ji,jj,jl) = Gsum(ji,jj,jl-1)
               ENDIF
            END DO
         END DO
      END DO

      ! Normalize the cumulative distribution to 1
      zworka(:,:) = 1._wp / Gsum(:,:,jpl)
      DO jl = 0, jpl
         Gsum(:,:,jl) = Gsum(:,:,jl) * zworka(:,:)
      END DO

      ! 1.3 Compute participation function a(h) = b(h).g(h) (athorn)
      !--------------------------------------------------------------------------------------------------
      ! Compute the participation function athorn; this is analogous to
      ! a(h) = b(h)g(h) as defined in Thorndike et al. (1975).
      ! area lost from category n due to ridging/closing
      ! athorn(n)   = total area lost due to ridging/closing
      ! assume b(h) = (2/Gstar) * (1 - G(h)/Gstar). 
      !
      ! The expressions for athorn are found by integrating b(h)g(h) between
      ! the category boundaries.
      !-----------------------------------------------------------------

      krdg_index = 1

      IF( krdg_index == 0 ) THEN       !--- Linear formulation (Thorndike et al., 1975)
         DO jl = 0, ice_cat_bounds(1,2)       ! only undeformed ice participates
            DO jj = 1, jpj 
               DO ji = 1, jpi
                  IF( Gsum(ji,jj,jl) < Gstar) THEN
                     athorn(ji,jj,jl) = Gstari * (Gsum(ji,jj,jl)-Gsum(ji,jj,jl-1)) * &
                        (2.0 - (Gsum(ji,jj,jl-1)+Gsum(ji,jj,jl))*Gstari)
                  ELSEIF (Gsum(ji,jj,jl-1) < Gstar) THEN
                     athorn(ji,jj,jl) = Gstari * (Gstar-Gsum(ji,jj,jl-1)) *  &
                        (2.0 - (Gsum(ji,jj,jl-1)+Gstar)*Gstari)
                  ELSE
                     athorn(ji,jj,jl) = 0.0
                  ENDIF
               END DO ! ji
            END DO ! jj
         END DO ! jl 

      ELSE                             !--- Exponential, more stable formulation (Lipscomb et al, 2007)
         !                        
         zdummy = 1._wp / ( 1._wp - EXP(-astari) )        ! precompute exponential terms using Gsum as a work array

         DO jl = -1, jpl
            Gsum(:,:,jl) = EXP( -Gsum(:,:,jl) * astari ) * zdummy
         END DO !jl
         DO jl = 0, ice_cat_bounds(1,2)
             athorn(:,:,jl) = Gsum(:,:,jl-1) - Gsum(:,:,jl)
         END DO
         !
      ENDIF ! krdg_index

      IF( raftswi == 1 ) THEN      ! Ridging and rafting ice participation functions
         !
         DO jl = 1, jpl
            DO jj = 1, jpj 
               DO ji = 1, jpi
                  IF ( athorn(ji,jj,jl) .GT. 0._wp ) THEN
!!gm  TANH( -X ) = - TANH( X )  so can be computed only 1 time....
                     aridge(ji,jj,jl) = ( TANH (  Craft * ( ht_i(ji,jj,jl) - hparmeter ) ) + 1.0 ) * 0.5 * athorn(ji,jj,jl)
                     araft (ji,jj,jl) = ( TANH ( -Craft * ( ht_i(ji,jj,jl) - hparmeter ) ) + 1.0 ) * 0.5 * athorn(ji,jj,jl)
                     IF ( araft(ji,jj,jl) < epsi06 )   araft(ji,jj,jl)  = 0._wp
                     aridge(ji,jj,jl) = MAX( athorn(ji,jj,jl) - araft(ji,jj,jl), 0.0 )
                  ENDIF ! athorn
               END DO ! ji
            END DO ! jj
         END DO ! jl

      ELSE  ! raftswi = 0
         !
         DO jl = 1, jpl
            aridge(:,:,jl) = athorn(:,:,jl)
         END DO
         !
      ENDIF

      IF ( raftswi == 1 ) THEN

         IF( MAXVAL(aridge + araft - athorn(:,:,1:jpl)) .GT. epsi11 ) THEN
            DO jl = 1, jpl
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     IF ( aridge(ji,jj,jl) + araft(ji,jj,jl) - athorn(ji,jj,jl) .GT. &
                        epsi11 ) THEN
                        WRITE(numout,*) ' ALERTE 96 : wrong participation function ... '
                        WRITE(numout,*) ' ji, jj, jl : ', ji, jj, jl
                        WRITE(numout,*) ' lat, lon   : ', gphit(ji,jj), glamt(ji,jj)
                        WRITE(numout,*) ' aridge     : ', aridge(ji,jj,1:jpl)
                        WRITE(numout,*) ' araft      : ', araft(ji,jj,1:jpl)
                        WRITE(numout,*) ' athorn     : ', athorn(ji,jj,1:jpl)
                     ENDIF
                  END DO
               END DO
            END DO
         ENDIF

      ENDIF

      !-----------------------------------------------------------------
      ! 2) Transfer function
      !-----------------------------------------------------------------
      ! Compute max and min ridged ice thickness for each ridging category.
      ! Assume ridged ice is uniformly distributed between hrmin and hrmax.
      ! 
      ! This parameterization is a modified version of Hibler (1980).
      ! The mean ridging thickness, hrmean, is proportional to hi^(0.5)
      !  and for very thick ridging ice must be >= krdgmin*hi
      !
      ! The minimum ridging thickness, hrmin, is equal to 2*hi 
      !  (i.e., rafting) and for very thick ridging ice is
      !  constrained by hrmin <= (hrmean + hi)/2.
      ! 
      ! The maximum ridging thickness, hrmax, is determined by
      !  hrmean and hrmin.
      !
      ! These modifications have the effect of reducing the ice strength
      ! (relative to the Hibler formulation) when very thick ice is
      ! ridging.
      !
      ! aksum = net area removed/ total area removed
      ! where total area removed = area of ice that ridges
      !         net area removed = total area removed - area of new ridges
      !-----------------------------------------------------------------

      ! Transfer function
      DO jl = 1, jpl !all categories have a specific transfer function
         DO jj = 1, jpj
            DO ji = 1, jpi

               IF (a_i(ji,jj,jl) .GT. epsi11 .AND. athorn(ji,jj,jl) .GT. 0.0 ) THEN
                  hi = v_i(ji,jj,jl) / a_i(ji,jj,jl)
                  hrmean          = MAX(SQRT(Hstar*hi), hi*krdgmin)
                  hrmin(ji,jj,jl) = MIN(2.0*hi, 0.5*(hrmean + hi))
                  hrmax(ji,jj,jl) = 2.0*hrmean - hrmin(ji,jj,jl)
                  hraft(ji,jj,jl) = kraft*hi
                  krdg(ji,jj,jl)  = hrmean / hi
               ELSE
                  hraft(ji,jj,jl) = 0.0
                  hrmin(ji,jj,jl) = 0.0 
                  hrmax(ji,jj,jl) = 0.0 
                  krdg (ji,jj,jl) = 1.0
               ENDIF

            END DO ! ji
         END DO ! jj
      END DO ! jl

      ! Normalization factor : aksum, ensures mass conservation
      aksum(:,:) = athorn(:,:,0)
      DO jl = 1, jpl 
         aksum(:,:)    = aksum(:,:) + aridge(:,:,jl) * ( 1._wp - 1._wp / krdg(:,:,jl) )    &
            &                       + araft (:,:,jl) * ( 1._wp - 1._wp / kraft        )
      END DO
      !
      CALL wrk_dealloc( jpi,jpj, zworka )
      CALL wrk_dealloc( jpi,jpj,jpl+2, Gsum, kkstart = -1 )
      !
   END SUBROUTINE lim_itd_me_ridgeprep


   SUBROUTINE lim_itd_me_ridgeshift( opning, closing_gross, msnow_mlt, esnow_mlt )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE lim_itd_me_icestrength ***
      !!
      !! ** Purpose :   shift ridging ice among thickness categories of ice thickness
      !!
      !! ** Method  :   Remove area, volume, and energy from each ridging category
      !!              and add to thicker ice categories.
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) ::   opning         ! rate of opening due to divergence/shear
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) ::   closing_gross  ! rate at which area removed, excluding area of new ridges
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   msnow_mlt      ! mass of snow added to ocean (kg m-2)
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   esnow_mlt      ! energy needed to melt snow in ocean (J m-2)
      !
      CHARACTER (len=80) ::   fieldid   ! field identifier
      LOGICAL, PARAMETER ::   l_conservation_check = .true.  ! if true, check conservation (useful for debugging)
      !
      LOGICAL ::   neg_ato_i      ! flag for ato_i(i,j) < -puny
      LOGICAL ::   large_afrac    ! flag for afrac > 1
      LOGICAL ::   large_afrft    ! flag for afrac > 1
      INTEGER ::   ji, jj, jl, jl1, jl2, jk   ! dummy loop indices
      INTEGER ::   ij                ! horizontal index, combines i and j loops
      INTEGER ::   icells            ! number of cells with aicen > puny
      REAL(wp) ::   zeps, zindb, zsrdg2   ! local scalar
      REAL(wp) ::   hL, hR, farea, zdummy, zdummy0, ztmelts    ! left and right limits of integration

      INTEGER , POINTER, DIMENSION(:) ::   indxi, indxj   ! compressed indices

      REAL(wp), POINTER, DIMENSION(:,:) ::   vice_init, vice_final   ! ice volume summed over categories
      REAL(wp), POINTER, DIMENSION(:,:) ::   eice_init, eice_final   ! ice energy summed over layers

      REAL(wp), POINTER, DIMENSION(:,:,:) ::   aicen_init, vicen_init   ! ice  area    & volume before ridging
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   vsnon_init, esnon_init   ! snow volume  & energy before ridging
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   smv_i_init, oa_i_init    ! ice salinity & age    before ridging

      REAL(wp), POINTER, DIMENSION(:,:,:,:) ::   eicen_init        ! ice energy before ridging

      REAL(wp), POINTER, DIMENSION(:,:) ::   afrac , fvol     ! fraction of category area ridged & new ridge volume going to n2
      REAL(wp), POINTER, DIMENSION(:,:) ::   ardg1 , ardg2    ! area of ice ridged & new ridges
      REAL(wp), POINTER, DIMENSION(:,:) ::   vsrdg , esrdg    ! snow volume & energy of ridging ice
      REAL(wp), POINTER, DIMENSION(:,:) ::   oirdg1, oirdg2   ! areal age content of ridged & rifging ice
      REAL(wp), POINTER, DIMENSION(:,:) ::   dhr   , dhr2     ! hrmax - hrmin  &  hrmax^2 - hrmin^2

      REAL(wp), POINTER, DIMENSION(:,:) ::   vrdg1   ! volume of ice ridged
      REAL(wp), POINTER, DIMENSION(:,:) ::   vrdg2   ! volume of new ridges
      REAL(wp), POINTER, DIMENSION(:,:) ::   vsw     ! volume of seawater trapped into ridges
      REAL(wp), POINTER, DIMENSION(:,:) ::   srdg1   ! sal*volume of ice ridged
      REAL(wp), POINTER, DIMENSION(:,:) ::   srdg2   ! sal*volume of new ridges
      REAL(wp), POINTER, DIMENSION(:,:) ::   smsw    ! sal*volume of water trapped into ridges

      REAL(wp), POINTER, DIMENSION(:,:) ::   afrft            ! fraction of category area rafted
      REAL(wp), POINTER, DIMENSION(:,:) ::   arft1 , arft2    ! area of ice rafted and new rafted zone
      REAL(wp), POINTER, DIMENSION(:,:) ::   virft , vsrft    ! ice & snow volume of rafting ice
      REAL(wp), POINTER, DIMENSION(:,:) ::   esrft , smrft    ! snow energy & salinity of rafting ice
      REAL(wp), POINTER, DIMENSION(:,:) ::   oirft1, oirft2   ! areal age content of rafted ice & rafting ice

      REAL(wp), POINTER, DIMENSION(:,:,:) ::   eirft      ! ice energy of rafting ice
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   erdg1      ! enth*volume of ice ridged
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   erdg2      ! enth*volume of new ridges
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ersw       ! enth of water trapped into ridges
      !!----------------------------------------------------------------------

      CALL wrk_alloc( (jpi+1)*(jpj+1),      indxi, indxj )
      CALL wrk_alloc( jpi, jpj,             vice_init, vice_final, eice_init, eice_final )
      CALL wrk_alloc( jpi, jpj,             afrac, fvol , ardg1, ardg2, vsrdg, esrdg, oirdg1, oirdg2, dhr, dhr2 )
      CALL wrk_alloc( jpi, jpj,             vrdg1, vrdg2, vsw  , srdg1, srdg2, smsw )
      CALL wrk_alloc( jpi, jpj,             afrft, arft1, arft2, virft, vsrft, esrft, smrft, oirft1, oirft2 )
      CALL wrk_alloc( jpi, jpj, jpl,        aicen_init, vicen_init, vsnon_init, esnon_init, smv_i_init, oa_i_init )
      CALL wrk_alloc( jpi, jpj, jkmax,      eirft, erdg1, erdg2, ersw )
      CALL wrk_alloc( jpi, jpj, jkmax, jpl, eicen_init )

      ! Conservation check
      eice_init(:,:) = 0._wp

      IF( con_i ) THEN
         CALL lim_column_sum (jpl,   v_i, vice_init )
         WRITE(numout,*) ' vice_init  : ', vice_init(jiindx,jjindx)
         CALL lim_column_sum_energy (jpl, nlay_i,  e_i, eice_init )
         WRITE(numout,*) ' eice_init  : ', eice_init(jiindx,jjindx)
      ENDIF

      zeps   = 1.e-20_wp

      !-------------------------------------------------------------------------------
      ! 1) Compute change in open water area due to closing and opening.
      !-------------------------------------------------------------------------------

      neg_ato_i = .false.

      DO jj = 1, jpj
         DO ji = 1, jpi
            ato_i(ji,jj) = ato_i(ji,jj) - athorn(ji,jj,0) * closing_gross(ji,jj) * rdt_ice        &
               &                        + opning(ji,jj)                          * rdt_ice
            IF( ato_i(ji,jj) < -epsi11 ) THEN
               neg_ato_i = .TRUE.
            ELSEIF( ato_i(ji,jj) < 0._wp ) THEN    ! roundoff error
               ato_i(ji,jj) = 0._wp
            ENDIF
         END DO !jj
      END DO !ji

      ! if negative open water area alert it
      IF( neg_ato_i ) THEN       ! there is a bug
         DO jj = 1, jpj 
            DO ji = 1, jpi
               IF( ato_i(ji,jj) < -epsi11 ) THEN 
                  WRITE(numout,*) ''  
                  WRITE(numout,*) 'Ridging error: ato_i < 0'
                  WRITE(numout,*) 'ato_i : ', ato_i(ji,jj)
               ENDIF               ! ato_i < -epsi11
            END DO
         END DO
      ENDIF

      !-----------------------------------------------------------------
      ! 2) Save initial state variables
      !-----------------------------------------------------------------

      DO jl = 1, jpl
         aicen_init(:,:,jl) = a_i(:,:,jl)
         vicen_init(:,:,jl) = v_i(:,:,jl)
         vsnon_init(:,:,jl) = v_s(:,:,jl)
         !
         smv_i_init(:,:,jl) = smv_i(:,:,jl)
         oa_i_init (:,:,jl) = oa_i (:,:,jl)
      END DO !jl

      esnon_init(:,:,:) = e_s(:,:,1,:)

      DO jl = 1, jpl  
         DO jk = 1, nlay_i
            eicen_init(:,:,jk,jl) = e_i(:,:,jk,jl)
         END DO
      END DO

      !
      !-----------------------------------------------------------------
      ! 3) Pump everything from ice which is being ridged / rafted
      !-----------------------------------------------------------------
      ! Compute the area, volume, and energy of ice ridging in each
      ! category, along with the area of the resulting ridge.

      DO jl1 = 1, jpl !jl1 describes the ridging category

         !------------------------------------------------
         ! 3.1) Identify grid cells with nonzero ridging
         !------------------------------------------------

         icells = 0
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF (aicen_init(ji,jj,jl1) .GT. epsi11 .AND. athorn(ji,jj,jl1) .GT. 0.0       &
                  .AND. closing_gross(ji,jj) > 0.0) THEN
                  icells = icells + 1
                  indxi(icells) = ji
                  indxj(icells) = jj
               ENDIF ! test on a_icen_init 
            END DO ! ji
         END DO ! jj

         large_afrac = .false.
         large_afrft = .false.

!CDIR NODEP
         DO ij = 1, icells
            ji = indxi(ij)
            jj = indxj(ij)

            !--------------------------------------------------------------------
            ! 3.2) Compute area of ridging ice (ardg1) and of new ridge (ardg2)
            !--------------------------------------------------------------------

            ardg1(ji,jj) = aridge(ji,jj,jl1)*closing_gross(ji,jj)*rdt_ice
            arft1(ji,jj) = araft (ji,jj,jl1)*closing_gross(ji,jj)*rdt_ice
            ardg2(ji,jj) = ardg1(ji,jj) / krdg(ji,jj,jl1)
            arft2(ji,jj) = arft1(ji,jj) / kraft

            oirdg1(ji,jj)= aridge(ji,jj,jl1)*closing_gross(ji,jj)*rdt_ice
            oirft1(ji,jj)= araft (ji,jj,jl1)*closing_gross(ji,jj)*rdt_ice
            oirdg2(ji,jj)= oirdg1(ji,jj) / krdg(ji,jj,jl1)
            oirft2(ji,jj)= oirft1(ji,jj) / kraft

            !---------------------------------------------------------------
            ! 3.3) Compute ridging /rafting fractions, make sure afrac <=1 
            !---------------------------------------------------------------

            afrac(ji,jj) = ardg1(ji,jj) / aicen_init(ji,jj,jl1) !ridging
            afrft(ji,jj) = arft1(ji,jj) / aicen_init(ji,jj,jl1) !rafting

            IF (afrac(ji,jj) > 1.0 + epsi11) THEN  !riging
               large_afrac = .true.
            ELSEIF (afrac(ji,jj) > 1.0) THEN  ! roundoff error
               afrac(ji,jj) = 1.0
            ENDIF
            IF (afrft(ji,jj) > 1.0 + epsi11) THEN !rafting
               large_afrft = .true.
            ELSEIF (afrft(ji,jj) > 1.0) THEN  ! roundoff error
               afrft(ji,jj) = 1.0
            ENDIF

            !--------------------------------------------------------------------------
            ! 3.4) Subtract area, volume, and energy from ridging 
            !     / rafting category n1.
            !--------------------------------------------------------------------------
            vrdg1(ji,jj) = vicen_init(ji,jj,jl1) * afrac(ji,jj) / ( 1._wp + ridge_por )
            vrdg2(ji,jj) = vrdg1(ji,jj) * ( 1. + ridge_por )
            vsw  (ji,jj) = vrdg1(ji,jj) * ridge_por

            vsrdg(ji,jj) = vsnon_init(ji,jj,jl1) * afrac(ji,jj)
            esrdg(ji,jj) = esnon_init(ji,jj,jl1) * afrac(ji,jj)
            srdg1(ji,jj) = smv_i_init(ji,jj,jl1) * afrac(ji,jj) / ( 1._wp + ridge_por )
            srdg2(ji,jj) = smv_i_init(ji,jj,jl1) * afrac(ji,jj)

            ! rafting volumes, heat contents ...
            virft(ji,jj) = vicen_init(ji,jj,jl1) * afrft(ji,jj)
            vsrft(ji,jj) = vsnon_init(ji,jj,jl1) * afrft(ji,jj)
            esrft(ji,jj) = esnon_init(ji,jj,jl1) * afrft(ji,jj)
            smrft(ji,jj) = smv_i_init(ji,jj,jl1) * afrft(ji,jj) 

            ! substract everything
            a_i(ji,jj,jl1)   = a_i(ji,jj,jl1)   - ardg1(ji,jj)  - arft1(ji,jj)
            v_i(ji,jj,jl1)   = v_i(ji,jj,jl1)   - vrdg1(ji,jj)  - virft(ji,jj)
            v_s(ji,jj,jl1)   = v_s(ji,jj,jl1)   - vsrdg(ji,jj)  - vsrft(ji,jj)
            e_s(ji,jj,1,jl1) = e_s(ji,jj,1,jl1) - esrdg(ji,jj)  - esrft(ji,jj)
            oa_i(ji,jj,jl1)  = oa_i(ji,jj,jl1)  - oirdg1(ji,jj) - oirft1(ji,jj)
            smv_i(ji,jj,jl1) = smv_i(ji,jj,jl1) - srdg1(ji,jj)  - smrft(ji,jj)

            !-----------------------------------------------------------------
            ! 3.5) Compute properties of new ridges
            !-----------------------------------------------------------------
            !-------------
            ! Salinity
            !-------------
            smsw(ji,jj)  = sss_m(ji,jj) * vsw(ji,jj) * rhoic / rau0       ! salt content of water frozen in voids

            zsrdg2       = srdg1(ji,jj) + smsw(ji,jj)                     ! salt content of new ridge

            srdg2(ji,jj) = MIN( s_i_max * vrdg2(ji,jj) , zsrdg2 )         ! impose a maximum salinity
            
            !                                                             ! excess of salt is flushed into the ocean
            fsalt_rpo(ji,jj) = fsalt_rpo(ji,jj) + ( zsrdg2 - srdg2(ji,jj) ) * rhoic / rdt_ice

            !------------------------------------            
            ! 3.6 Increment ridging diagnostics
            !------------------------------------            

            !        jl1 looping 1-jpl
            !           ij looping 1-icells

            dardg1dt   (ji,jj) = dardg1dt(ji,jj) + ardg1(ji,jj) + arft1(ji,jj)
            dardg2dt   (ji,jj) = dardg2dt(ji,jj) + ardg2(ji,jj) + arft2(ji,jj)
            diag_dyn_gr(ji,jj) = diag_dyn_gr(ji,jj) + ( vrdg2(ji,jj) + virft(ji,jj) ) / rdt_ice
            opening    (ji,jj) = opening (ji,jj) + opning(ji,jj)*rdt_ice

            IF( con_i )   vice_init(ji,jj) = vice_init(ji,jj) + vrdg2(ji,jj) - vrdg1(ji,jj)

            !------------------------------------------            
            ! 3.7 Put the snow somewhere in the ocean
            !------------------------------------------            

            !  Place part of the snow lost by ridging into the ocean. 
            !  Note that esnow_mlt < 0; the ocean must cool to melt snow.
            !  If the ocean temp = Tf already, new ice must grow.
            !  During the next time step, thermo_rates will determine whether
            !  the ocean cools or new ice grows.
            !        jl1 looping 1-jpl
            !           ij looping 1-icells

            msnow_mlt(ji,jj) = msnow_mlt(ji,jj) + rhosn*vsrdg(ji,jj)*(1.0-fsnowrdg)   &   ! rafting included
               &                                + rhosn*vsrft(ji,jj)*(1.0-fsnowrft)

            esnow_mlt(ji,jj) = esnow_mlt(ji,jj) + esrdg(ji,jj)*(1.0-fsnowrdg)         &   !rafting included
               &                                + esrft(ji,jj)*(1.0-fsnowrft)          

            !-----------------------------------------------------------------
            ! 3.8 Compute quantities used to apportion ice among categories
            ! in the n2 loop below
            !-----------------------------------------------------------------

            !        jl1 looping 1-jpl
            !           ij looping 1-icells

            dhr(ji,jj)  = hrmax(ji,jj,jl1) - hrmin(ji,jj,jl1)
            dhr2(ji,jj) = hrmax(ji,jj,jl1) * hrmax(ji,jj,jl1) - hrmin(ji,jj,jl1) * hrmin(ji,jj,jl1)


         END DO                 ! ij

         !--------------------------------------------------------------------
         ! 3.9 Compute ridging ice enthalpy, remove it from ridging ice and
         !      compute ridged ice enthalpy 
         !--------------------------------------------------------------------
         DO jk = 1, nlay_i
!CDIR NODEP
            DO ij = 1, icells
               ji = indxi(ij)
               jj = indxj(ij)
               ! heat content of ridged ice
               erdg1(ji,jj,jk)      = eicen_init(ji,jj,jk,jl1) * afrac(ji,jj) / ( 1._wp + ridge_por ) 
               eirft(ji,jj,jk)      = eicen_init(ji,jj,jk,jl1) * afrft(ji,jj)
               e_i  (ji,jj,jk,jl1)  = e_i(ji,jj,jk,jl1) - erdg1(ji,jj,jk) - eirft(ji,jj,jk)
               ! sea water heat content
               ztmelts          = - tmut * sss_m(ji,jj) + rtt
               ! heat content per unit volume
               zdummy0          = - rcp * ( sst_m(ji,jj) + rt0 - rtt ) * vsw(ji,jj)

               ! corrected sea water salinity
               zindb  = MAX( 0._wp , SIGN( 1._wp , vsw(ji,jj) - zeps ) )
               zdummy = zindb * ( srdg1(ji,jj) - srdg2(ji,jj) ) / MAX( ridge_por * vsw(ji,jj), zeps )

               ztmelts          = - tmut * zdummy + rtt
               ersw(ji,jj,jk)   = - rcp * ( ztmelts - rtt ) * vsw(ji,jj)

               ! heat flux
               fheat_rpo(ji,jj) = fheat_rpo(ji,jj) + ( ersw(ji,jj,jk) - zdummy0 ) / rdt_ice

               ! Correct dimensions to avoid big values
               ersw(ji,jj,jk)   = ersw(ji,jj,jk) * 1.e-09

               ! Mutliply by ice volume, and divide by number of layers to get heat content in 10^9 J
               ersw (ji,jj,jk)  = ersw(ji,jj,jk) * area(ji,jj) * vsw(ji,jj) / nlay_i

               erdg2(ji,jj,jk)  = erdg1(ji,jj,jk) + ersw(ji,jj,jk)
            END DO ! ij
         END DO !jk


         IF( con_i ) THEN
            DO jk = 1, nlay_i
!CDIR NODEP
               DO ij = 1, icells
                  ji = indxi(ij)
                  jj = indxj(ij)
                  eice_init(ji,jj) = eice_init(ji,jj) + erdg2(ji,jj,jk) - erdg1(ji,jj,jk)
               END DO ! ij
            END DO !jk
         ENDIF

         IF( large_afrac ) THEN   ! there is a bug
!CDIR NODEP
            DO ij = 1, icells
               ji = indxi(ij)
               jj = indxj(ij)
               IF( afrac(ji,jj) > 1.0 + epsi11 ) THEN 
                  WRITE(numout,*) ''
                  WRITE(numout,*) ' ardg > a_i'
                  WRITE(numout,*) ' ardg, aicen_init : ', ardg1(ji,jj), aicen_init(ji,jj,jl1)
               ENDIF
            END DO
         ENDIF
         IF( large_afrft ) THEN  ! there is a bug
!CDIR NODEP
            DO ij = 1, icells
               ji = indxi(ij)
               jj = indxj(ij)
               IF( afrft(ji,jj) > 1.0 + epsi11 ) THEN 
                  WRITE(numout,*) ''
                  WRITE(numout,*) ' arft > a_i'
                  WRITE(numout,*) ' arft, aicen_init : ', arft1(ji,jj), aicen_init(ji,jj,jl1)
               ENDIF
            END DO
         ENDIF

         !-------------------------------------------------------------------------------
         ! 4) Add area, volume, and energy of new ridge to each category jl2
         !-------------------------------------------------------------------------------
         !        jl1 looping 1-jpl
         DO jl2  = ice_cat_bounds(1,1), ice_cat_bounds(1,2) 
            ! over categories to which ridged ice is transferred
!CDIR NODEP
            DO ij = 1, icells
               ji = indxi(ij)
               jj = indxj(ij)

               ! Compute the fraction of ridged ice area and volume going to 
               ! thickness category jl2.
               ! Transfer area, volume, and energy accordingly.

               IF (hrmin(ji,jj,jl1) .GE. hi_max(jl2) .OR.        &
                  hrmax(ji,jj,jl1) .LE. hi_max(jl2-1)) THEN
                  hL = 0.0
                  hR = 0.0
               ELSE
                  hL = MAX (hrmin(ji,jj,jl1), hi_max(jl2-1))
                  hR = MIN (hrmax(ji,jj,jl1), hi_max(jl2))
               ENDIF

               ! fraction of ridged ice area and volume going to n2
               farea = (hR-hL) / dhr(ji,jj) 
               fvol(ji,jj) = (hR*hR - hL*hL) / dhr2(ji,jj)

               a_i  (ji,jj,jl2)   = a_i  (ji,jj,jl2)   + ardg2 (ji,jj) * farea
               v_i  (ji,jj,jl2)   = v_i  (ji,jj,jl2)   + vrdg2 (ji,jj) * fvol(ji,jj)
               v_s  (ji,jj,jl2)   = v_s  (ji,jj,jl2)   + vsrdg (ji,jj) * fvol(ji,jj) * fsnowrdg
               e_s  (ji,jj,1,jl2) = e_s  (ji,jj,1,jl2) + esrdg (ji,jj) * fvol(ji,jj) * fsnowrdg
               smv_i(ji,jj,jl2)   = smv_i(ji,jj,jl2)   + srdg2 (ji,jj) * fvol(ji,jj)
               oa_i (ji,jj,jl2)   = oa_i (ji,jj,jl2)   + oirdg2(ji,jj) * farea

            END DO ! ij

            ! Transfer ice energy to category jl2 by ridging
            DO jk = 1, nlay_i
!CDIR NODEP
               DO ij = 1, icells
                  ji = indxi(ij)
                  jj = indxj(ij)
                  e_i(ji,jj,jk,jl2) = e_i(ji,jj,jk,jl2) + fvol(ji,jj)*erdg2(ji,jj,jk)
               END DO
            END DO
            !
         END DO                 ! jl2 (new ridges)            

         DO jl2 = ice_cat_bounds(1,1), ice_cat_bounds(1,2) 

!CDIR NODEP
            DO ij = 1, icells
               ji = indxi(ij)
               jj = indxj(ij)
               ! Compute the fraction of rafted ice area and volume going to 
               ! thickness category jl2, transfer area, volume, and energy accordingly.

               IF (hraft(ji,jj,jl1) .LE. hi_max(jl2) .AND.        &
                  hraft(ji,jj,jl1) .GT. hi_max(jl2-1)) THEN
                  a_i(ji,jj,jl2) = a_i(ji,jj,jl2) + arft2(ji,jj)
                  v_i(ji,jj,jl2) = v_i(ji,jj,jl2) + virft(ji,jj)
                  v_s(ji,jj,jl2) = v_s(ji,jj,jl2) + vsrft(ji,jj)*fsnowrft
                  e_s(ji,jj,1,jl2) = e_s(ji,jj,1,jl2) + esrft(ji,jj)*fsnowrft
                  smv_i(ji,jj,jl2) = smv_i(ji,jj,jl2) + smrft(ji,jj)    
                  oa_i(ji,jj,jl2)  = oa_i(ji,jj,jl2)  + oirft2(ji,jj)    
               ENDIF ! hraft

            END DO ! ij

            ! Transfer rafted ice energy to category jl2 
            DO jk = 1, nlay_i
!CDIR NODEP
               DO ij = 1, icells
                  ji = indxi(ij)
                  jj = indxj(ij)
                  IF (hraft(ji,jj,jl1) .LE. hi_max(jl2) .AND.        &
                     hraft(ji,jj,jl1) .GT. hi_max(jl2-1)) THEN
                     e_i(ji,jj,jk,jl2) = e_i(ji,jj,jk,jl2) + eirft(ji,jj,jk)
                  ENDIF
               END DO           ! ij
            END DO !jk

         END DO ! jl2

      END DO ! jl1 (deforming categories)

      ! Conservation check
      IF ( con_i ) THEN
         CALL lim_column_sum (jpl,   v_i, vice_final)
         fieldid = ' v_i : limitd_me '
         CALL lim_cons_check (vice_init, vice_final, 1.0e-6, fieldid) 
         WRITE(numout,*) ' vice_init  : ', vice_init(jiindx,jjindx)
         WRITE(numout,*) ' vice_final : ', vice_final(jiindx,jjindx)

         CALL lim_column_sum_energy (jpl, nlay_i,  e_i, eice_final )
         fieldid = ' e_i : limitd_me '
         CALL lim_cons_check (eice_init, eice_final, 1.0e-2, fieldid) 
         WRITE(numout,*) ' eice_init  : ', eice_init(jiindx,jjindx)
         WRITE(numout,*) ' eice_final : ', eice_final(jiindx,jjindx)
      ENDIF
      !
      CALL wrk_dealloc( (jpi+1)*(jpj+1),      indxi, indxj )
      CALL wrk_dealloc( jpi, jpj,             vice_init, vice_final, eice_init, eice_final )
      CALL wrk_dealloc( jpi, jpj,             afrac, fvol , ardg1, ardg2, vsrdg, esrdg, oirdg1, oirdg2, dhr, dhr2 )
      CALL wrk_dealloc( jpi, jpj,             vrdg1, vrdg2, vsw  , srdg1, srdg2, smsw )
      CALL wrk_dealloc( jpi, jpj,             afrft, arft1, arft2, virft, vsrft, esrft, smrft, oirft1, oirft2 )
      CALL wrk_dealloc( jpi, jpj, jpl,        aicen_init, vicen_init, vsnon_init, esnon_init, smv_i_init, oa_i_init )
      CALL wrk_dealloc( jpi, jpj, jkmax,      eirft, erdg1, erdg2, ersw )
      CALL wrk_dealloc( jpi, jpj, jkmax, jpl, eicen_init )
      !
   END SUBROUTINE lim_itd_me_ridgeshift


   SUBROUTINE lim_itd_me_asumr
      !!-----------------------------------------------------------------------------
      !!                ***  ROUTINE lim_itd_me_asumr ***
      !!
      !! ** Purpose :   finds total fractional area
      !!
      !! ** Method  :   Find the total area of ice plus open water in each grid cell.
      !!              This is similar to the aggregate_area subroutine except that the
      !!              total area can be greater than 1, so the open water area is 
      !!              included in the sum instead of being computed as a residual. 
      !!-----------------------------------------------------------------------------
      INTEGER ::   jl   ! dummy loop index
      !!-----------------------------------------------------------------------------
      !
      asum(:,:) = ato_i(:,:)                    ! open water
      DO jl = 1, jpl                            ! ice categories
         asum(:,:) = asum(:,:) + a_i(:,:,jl)
      END DO
      !
   END SUBROUTINE lim_itd_me_asumr


   SUBROUTINE lim_itd_me_init
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE lim_itd_me_init ***
      !!
      !! ** Purpose :   Physical constants and parameters linked 
      !!                to the mechanical ice redistribution
      !!
      !! ** Method  :   Read the namiceitdme namelist 
      !!                and check the parameters values 
      !!                called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namiceitdme
      !!-------------------------------------------------------------------
      NAMELIST/namiceitdme/ ridge_scheme_swi, Cs, Cf, fsnowrdg, fsnowrft,& 
         Gstar, astar,                                &
         Hstar, raftswi, hparmeter, Craft, ridge_por, &
         sal_max_ridge,  partfun_swi, transfun_swi,   &
         brinstren_swi
      !!-------------------------------------------------------------------
      !
      REWIND( numnam_ice )                   ! read namiceitdme namelist
      READ  ( numnam_ice , namiceitdme)
      !
      IF (lwp) THEN                          ! control print
         WRITE(numout,*)
         WRITE(numout,*)' lim_itd_me_init : ice parameters for mechanical ice redistribution '
         WRITE(numout,*)' ~~~~~~~~~~~~~~~'
         WRITE(numout,*)'   Switch choosing the ice redistribution scheme           ridge_scheme_swi', ridge_scheme_swi 
         WRITE(numout,*)'   Fraction of shear energy contributing to ridging        Cs              ', Cs 
         WRITE(numout,*)'   Ratio of ridging work to PotEner change in ridging      Cf              ', Cf 
         WRITE(numout,*)'   Fraction of snow volume conserved during ridging        fsnowrdg        ', fsnowrdg 
         WRITE(numout,*)'   Fraction of snow volume conserved during ridging        fsnowrft        ', fsnowrft 
         WRITE(numout,*)'   Fraction of total ice coverage contributing to ridging  Gstar           ', Gstar
         WRITE(numout,*)'   Equivalent to G* for an exponential part function       astar           ', astar
         WRITE(numout,*)'   Quantity playing a role in max ridged ice thickness     Hstar           ', Hstar
         WRITE(numout,*)'   Rafting of ice sheets or not                            raftswi         ', raftswi
         WRITE(numout,*)'   Parmeter thickness (threshold between ridge-raft)       hparmeter       ', hparmeter
         WRITE(numout,*)'   Rafting hyperbolic tangent coefficient                  Craft           ', Craft  
         WRITE(numout,*)'   Initial porosity of ridges                              ridge_por       ', ridge_por
         WRITE(numout,*)'   Maximum salinity of ridging ice                         sal_max_ridge   ', sal_max_ridge
         WRITE(numout,*)'   Switch for part. function (0) linear (1) exponential    partfun_swi     ', partfun_swi
         WRITE(numout,*)'   Switch for tran. function (0) linear (1) exponential    transfun_swi    ', transfun_swi
         WRITE(numout,*)'   Switch for including brine volume in ice strength comp. brinstren_swi   ', brinstren_swi
      ENDIF
      !
   END SUBROUTINE lim_itd_me_init


   SUBROUTINE lim_itd_me_zapsmall
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE lim_itd_me_zapsmall ***
      !!
      !! ** Purpose :   Remove too small sea ice areas and correct salt fluxes
      !!
      !! history :
      !! author: William H. Lipscomb, LANL
      !! Nov 2003:  Modified by Julie Schramm to conserve volume and energy
      !! Sept 2004: Modified by William Lipscomb; replaced normalize_state with
      !!            additions to local freshwater, salt, and heat fluxes
      !!  9.0, LIM3.0 - 02-2006 (M. Vancoppenolle) original code
      !!-------------------------------------------------------------------
      INTEGER ::   ji, jj, jl, jk   ! dummy loop indices
      INTEGER ::   icells           ! number of cells with ice to zap

      REAL(wp), POINTER, DIMENSION(:,:) ::   zmask   ! 2D workspace
      
!!gm      REAL(wp) ::   xtmp      ! temporary variable
      !!-------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, zmask )

      DO jl = 1, jpl

         !-----------------------------------------------------------------
         ! Count categories to be zapped.
         ! Abort model in case of negative area.
         !-----------------------------------------------------------------
         IF( MINVAL(a_i(:,:,jl)) .LT. -epsi11 .AND. ln_nicep ) THEN
            DO jj = 1, jpj
               DO ji = 1, jpi
                  IF ( a_i(ji,jj,jl) .LT. -epsi11 ) THEN
                     WRITE (numout,*) ' ALERTE 98 ' 
                     WRITE (numout,*) ' Negative ice area: ji, jj, jl: ', ji, jj,jl
                     WRITE (numout,*) ' a_i    ', a_i(ji,jj,jl)
                  ENDIF
               END DO
            END DO
         ENDIF

         icells = 0
         zmask  = 0._wp
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF(  ( a_i(ji,jj,jl) .GE. -epsi11 .AND. a_i(ji,jj,jl) .LT. 0._wp   ) .OR.   &
                  & ( a_i(ji,jj,jl) .GT. 0._wp   .AND. a_i(ji,jj,jl) .LE. 1.0e-11 ) .OR.   &
                  & ( v_i(ji,jj,jl)  ==  0._wp   .AND. a_i(ji,jj,jl) .GT. 0._wp   ) .OR.   &
                  & ( v_i(ji,jj,jl) .GT. 0._wp   .AND. v_i(ji,jj,jl) .LT. 1.e-12  )      )   zmask(ji,jj) = 1._wp
            END DO
         END DO
         IF( ln_nicep )   WRITE(numout,*) SUM(zmask), ' cells of ice zapped in the ocean '

         !-----------------------------------------------------------------
         ! Zap ice energy and use ocean heat to melt ice
         !-----------------------------------------------------------------

         DO jk = 1, nlay_i
            DO jj = 1 , jpj
               DO ji = 1 , jpi
!!gm                  xtmp = e_i(ji,jj,jk,jl) / area(ji,jj) / rdt_ice
!!gm                  xtmp = xtmp * unit_fac
                  ! fheat_res(ji,jj) = fheat_res(ji,jj) - xtmp
                  e_i(ji,jj,jk,jl) = e_i(ji,jj,jk,jl) * ( 1 - zmask(ji,jj) )
               END DO
            END DO
         END DO

         DO jj = 1 , jpj
            DO ji = 1 , jpi

               !-----------------------------------------------------------------
               ! Zap snow energy and use ocean heat to melt snow
               !-----------------------------------------------------------------
               !           xtmp = esnon(i,j,n) / dt ! < 0
               !           fhnet(i,j)      = fhnet(i,j)      + xtmp
               !           fhnet_hist(i,j) = fhnet_hist(i,j) + xtmp
               ! xtmp is greater than 0
               ! fluxes are positive to the ocean
               ! here the flux has to be negative for the ocean
!!gm               xtmp = ( rhosn*cpic*( rtt-t_s(ji,jj,1,jl) ) + rhosn*lfus ) / rdt_ice
               !           fheat_res(ji,jj) = fheat_res(ji,jj) - xtmp

!!gm               xtmp = ( rhosn*cpic*( rtt-t_s(ji,jj,1,jl) ) + rhosn*lfus ) / rdt_ice !RB   ???????

               t_s(ji,jj,1,jl) = rtt * zmask(ji,jj) + t_s(ji,jj,1,jl) * ( 1 - zmask(ji,jj) )

               !-----------------------------------------------------------------
               ! zap ice and snow volume, add water and salt to ocean
               !-----------------------------------------------------------------

               !           xtmp = (rhoi*vicen(i,j,n) + rhos*vsnon(i,j,n)) / dt
               !           fresh(i,j)      = fresh(i,j)      + xtmp
               !           fresh_hist(i,j) = fresh_hist(i,j) + xtmp

               !           fsalt_res(ji,jj)  = fsalt_res(ji,jj) + ( sss_m(ji,jj)                  ) * & 
               !                               rhosn * v_s(ji,jj,jl) / rdt_ice

               !           fsalt_res(ji,jj)  = fsalt_res(ji,jj) + ( sss_m(ji,jj) - sm_i(ji,jj,jl) ) * & 
               !                               rhoic * v_i(ji,jj,jl) / rdt_ice

               !           emps(i,j)      = emps(i,j)      + xtmp
               !           fsalt_hist(i,j) = fsalt_hist(i,j) + xtmp

               ato_i(ji,jj)    = a_i  (ji,jj,jl) *       zmask(ji,jj)   + ato_i(ji,jj)
               a_i  (ji,jj,jl) = a_i  (ji,jj,jl) * ( 1 - zmask(ji,jj) )
               v_i  (ji,jj,jl) = v_i  (ji,jj,jl) * ( 1 - zmask(ji,jj) )
               v_s  (ji,jj,jl) = v_s  (ji,jj,jl) * ( 1 - zmask(ji,jj) )
               t_su (ji,jj,jl) = t_su (ji,jj,jl) * ( 1 - zmask(ji,jj) ) + t_bo(ji,jj) * zmask(ji,jj)
               oa_i (ji,jj,jl) = oa_i (ji,jj,jl) * ( 1 - zmask(ji,jj) )
               smv_i(ji,jj,jl) = smv_i(ji,jj,jl) * ( 1 - zmask(ji,jj) )
               !
            END DO
         END DO
         !
      END DO                 ! jl 
      !
      CALL wrk_dealloc( jpi, jpj, zmask )
      !
   END SUBROUTINE lim_itd_me_zapsmall

#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty module          NO LIM-3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_itd_me           ! Empty routines
   END SUBROUTINE lim_itd_me
   SUBROUTINE lim_itd_me_icestrength
   END SUBROUTINE lim_itd_me_icestrength
   SUBROUTINE lim_itd_me_sort
   END SUBROUTINE lim_itd_me_sort
   SUBROUTINE lim_itd_me_init
   END SUBROUTINE lim_itd_me_init
   SUBROUTINE lim_itd_me_zapsmall
   END SUBROUTINE lim_itd_me_zapsmall
#endif
   !!======================================================================
END MODULE limitd_me
