MODULE limitd_th
   !!======================================================================
   !!                       ***  MODULE limitd_th ***
   !!              Thermodynamics of ice thickness distribution
   !!                   computation of changes in g(h)      
   !!======================================================================
   !! History :   -   !          (W. H. Lipscomb and E.C. Hunke) CICE (c) original code
   !!            3.0  ! 2005-12  (M. Vancoppenolle) adaptation to LIM-3
   !!             -   ! 2006-06  (M. Vancoppenolle) adaptation to include salt, age and types
   !!             -   ! 2007-04  (M. Vancoppenolle) Mass conservation checked
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3' :                                   LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_itd_th       : thermodynamics of ice thickness distribution
   !!   lim_itd_th_rem   :
   !!   lim_itd_th_reb   :
   !!   lim_itd_fitline  :
   !!   lim_itd_shiftice :
   !!----------------------------------------------------------------------
   USE dom_ice          ! LIM-3 domain
   USE par_oce          ! ocean parameters
   USE dom_oce          ! ocean domain
   USE phycst           ! physical constants (ocean directory) 
   USE thd_ice          ! LIM-3 thermodynamic variables
   USE ice              ! LIM-3 variables
   USE par_ice          ! LIM-3 parameters
   USE limthd_lac       ! LIM-3 lateral accretion
   USE limvar           ! LIM-3 variables
   USE limcons          ! LIM-3 conservation
   USE prtctl           ! Print control
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_itd_th         ! called by ice_stp
   PUBLIC   lim_itd_th_rem
   PUBLIC   lim_itd_th_reb
   PUBLIC   lim_itd_fitline
   PUBLIC   lim_itd_shiftice

   REAL(wp) ::   epsi20 = 1e-20_wp   ! constant values
   REAL(wp) ::   epsi13 = 1e-13_wp   !
   REAL(wp) ::   epsi10 = 1e-10_wp   !

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2010)
   !! $Id: limitd_th.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_itd_th( kt )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_itd_th ***
      !!
      !! ** Purpose :   computes the thermodynamics of ice thickness distribution
      !!
      !! ** Method  :
      !!------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! time step index
      !
      INTEGER ::   jl, ja, jm, jbnd1, jbnd2   ! ice types    dummy loop index         

      !!------------------------------------------------------------------

      IF( kt == nit000 .AND. lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'lim_itd_th  : Thermodynamics of the ice thickness distribution'
         WRITE(numout,*) '~~~~~~~~~~~'
      ENDIF

      !------------------------------------------------------------------------------|
      !  1) Transport of ice between thickness categories.                           |
      !------------------------------------------------------------------------------|
      ! Given thermodynamic growth rates, transport ice between
      ! thickness categories.
      DO jm = 1, jpm
         jbnd1 = ice_cat_bounds(jm,1)
         jbnd2 = ice_cat_bounds(jm,2)
         IF( ice_ncat_types(jm) > 1 )   CALL lim_itd_th_rem( jbnd1, jbnd2, jm, kt )
      END DO
      !
      CALL lim_var_glo2eqv    ! only for info
      CALL lim_var_agg(1)

      !------------------------------------------------------------------------------|
      !  3) Add frazil ice growing in leads.
      !------------------------------------------------------------------------------|

      CALL lim_thd_lac
      CALL lim_var_glo2eqv    ! only for info

      !----------------------------------------------------------------------------------------
      !  4) Computation of trend terms and get back to old values      
      !----------------------------------------------------------------------------------------

      !- Trend terms
      d_a_i_thd (:,:,:)  = a_i(:,:,:)   - old_a_i(:,:,:) 
      d_v_s_thd (:,:,:)  = v_s(:,:,:)   - old_v_s(:,:,:)
      d_v_i_thd (:,:,:)  = v_i(:,:,:)   - old_v_i(:,:,:)  
      d_e_s_thd(:,:,:,:) = e_s(:,:,:,:) - old_e_s(:,:,:,:) 
      d_e_i_thd(:,:,:,:) = e_i(:,:,:,:) - old_e_i(:,:,:,:)

      d_smv_i_thd(:,:,:) = 0._wp
      IF( num_sal == 2 .OR. num_sal == 4 )   d_smv_i_thd(:,:,:) = smv_i(:,:,:) - old_smv_i(:,:,:)

      IF(ln_ctl) THEN   ! Control print
         CALL prt_ctl_info(' ')
         CALL prt_ctl_info(' - Cell values : ')
         CALL prt_ctl_info('   ~~~~~~~~~~~~~ ')
         CALL prt_ctl(tab2d_1=area , clinfo1=' lim_itd_th  : cell area :')
         CALL prt_ctl(tab2d_1=at_i , clinfo1=' lim_itd_th  : at_i      :')
         CALL prt_ctl(tab2d_1=vt_i , clinfo1=' lim_itd_th  : vt_i      :')
         CALL prt_ctl(tab2d_1=vt_s , clinfo1=' lim_itd_th  : vt_s      :')
         DO jl = 1, jpl
            CALL prt_ctl_info(' ')
            CALL prt_ctl_info(' - Category : ', ivar1=jl)
            CALL prt_ctl_info('   ~~~~~~~~~~')
            CALL prt_ctl(tab2d_1=a_i   (:,:,jl)   , clinfo1= ' lim_itd_th  : a_i      : ')
            CALL prt_ctl(tab2d_1=ht_i  (:,:,jl)   , clinfo1= ' lim_itd_th  : ht_i     : ')
            CALL prt_ctl(tab2d_1=ht_s  (:,:,jl)   , clinfo1= ' lim_itd_th  : ht_s     : ')
            CALL prt_ctl(tab2d_1=v_i   (:,:,jl)   , clinfo1= ' lim_itd_th  : v_i      : ')
            CALL prt_ctl(tab2d_1=v_s   (:,:,jl)   , clinfo1= ' lim_itd_th  : v_s      : ')
            CALL prt_ctl(tab2d_1=e_s   (:,:,1,jl) , clinfo1= ' lim_itd_th  : e_s      : ')
            CALL prt_ctl(tab2d_1=t_su  (:,:,jl)   , clinfo1= ' lim_itd_th  : t_su     : ')
            CALL prt_ctl(tab2d_1=t_s   (:,:,1,jl) , clinfo1= ' lim_itd_th  : t_snow   : ')
            CALL prt_ctl(tab2d_1=sm_i  (:,:,jl)   , clinfo1= ' lim_itd_th  : sm_i     : ')
            CALL prt_ctl(tab2d_1=smv_i (:,:,jl)   , clinfo1= ' lim_itd_th  : smv_i    : ')
            DO ja = 1, nlay_i
               CALL prt_ctl_info(' ')
               CALL prt_ctl_info(' - Layer : ', ivar1=ja)
               CALL prt_ctl_info('   ~~~~~~~')
               CALL prt_ctl(tab2d_1=t_i(:,:,ja,jl) , clinfo1= ' lim_itd_th  : t_i      : ')
               CALL prt_ctl(tab2d_1=e_i(:,:,ja,jl) , clinfo1= ' lim_itd_th  : e_i      : ')
            END DO
         END DO
      ENDIF

      !- Recover Old values
      a_i(:,:,:)   = old_a_i (:,:,:)
      v_s(:,:,:)   = old_v_s (:,:,:)
      v_i(:,:,:)   = old_v_i (:,:,:)
      e_s(:,:,:,:) = old_e_s (:,:,:,:)
      e_i(:,:,:,:) = old_e_i (:,:,:,:)
      !
      IF( num_sal == 2 .OR. num_sal == 4 )   smv_i(:,:,:)       = old_smv_i (:,:,:)
      !
   END SUBROUTINE lim_itd_th
   !

   SUBROUTINE lim_itd_th_rem( klbnd, kubnd, ntyp, kt )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_itd_th_rem ***
      !!
      !! ** Purpose :   computes the redistribution of ice thickness
      !!              after thermodynamic growth of ice thickness
      !!
      !! ** Method  : Linear remapping 
      !!
      !! References : W.H. Lipscomb, JGR 2001
      !!------------------------------------------------------------------
      INTEGER , INTENT (in) ::   klbnd   ! Start thickness category index point
      INTEGER , INTENT (in) ::   kubnd   ! End point on which the  the computation is applied
      INTEGER , INTENT (in) ::   ntyp    ! Number of the type used
      INTEGER , INTENT (in) ::   kt      ! Ocean time step 
      !
      INTEGER  ::   ji, jj, jl     ! dummy loop index
      INTEGER  ::   zji, zjj, nd   ! local integer
      REAL(wp) ::   zx1, zwk1, zdh0, zetamin, zdamax   ! local scalars
      REAL(wp) ::   zx2, zwk2, zda0, zetamax, zhimin   !   -      -
      REAL(wp) ::   zx3,             zareamin, zindb   !   -      -
      CHARACTER (len = 15) :: fieldid

      INTEGER , POINTER, DIMENSION(:,:,:) ::   zdonor   ! donor category index

      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zdhice      ! ice thickness increment
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   g0          ! coefficients for fitting the line of the ITD
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   g1          ! coefficients for fitting the line of the ITD
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   hL          ! left boundary for the ITD for each thickness
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   hR          ! left boundary for the ITD for each thickness
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zht_i_o     ! old ice thickness
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   dummy_es
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zdaice, zdvice          ! local increment of ice area and volume
      REAL(wp), POINTER, DIMENSION(:)     ::   zvetamin, zvetamax      ! maximum values for etas
      INTEGER , POINTER, DIMENSION(:)     ::   nind_i, nind_j          ! compressed indices for i/j directions
      INTEGER                             ::   nbrem                   ! number of cells with ice to transfer
      REAL(wp)                            ::   zslope                  ! used to compute local thermodynamic "speeds"
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zhb0, zhb1              ! category boundaries for thinnes categories
      REAL(wp), POINTER, DIMENSION(:,:)   ::   vt_i_init, vt_i_final   !  ice volume summed over categories
      REAL(wp), POINTER, DIMENSION(:,:)   ::   vt_s_init, vt_s_final   !  snow volume summed over categories
      REAL(wp), POINTER, DIMENSION(:,:)   ::   et_i_init, et_i_final   !  ice energy summed over categories
      REAL(wp), POINTER, DIMENSION(:,:)   ::   et_s_init, et_s_final   !  snow energy summed over categories
      INTEGER , POINTER, DIMENSION(:,:)   ::   zremap_flag      ! compute remapping or not ????
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zhbnew           ! new boundaries of ice categories
      !!------------------------------------------------------------------

      CALL wrk_alloc( jpi,jpj, zremap_flag )    ! integer
      CALL wrk_alloc( jpi,jpj,jpl-1, zdonor )   ! integer
      CALL wrk_alloc( jpi,jpj,jpl, zdhice, g0, g1, hL, hR, zht_i_o, dummy_es )
      CALL wrk_alloc( jpi,jpj,jpl-1, zdaice, zdvice )   
      CALL wrk_alloc( jpi,jpj,jpl+1, zhbnew, kkstart = 0 )   
      CALL wrk_alloc( (jpi+1)*(jpj+1), zvetamin, zvetamax )   
      CALL wrk_alloc( (jpi+1)*(jpj+1), nind_i, nind_j )   ! integer 
      CALL wrk_alloc( jpi,jpj, zhb0,zhb1,vt_i_init,vt_i_final,vt_s_init,vt_s_final,et_i_init,et_i_final,et_s_init,et_s_final )

      zhimin   = 0.1      !minimum ice thickness tolerated by the model
      zareamin = epsi10   !minimum area in thickness categories tolerated by the conceptors of the model

      !!----------------------------------------------------------------------------------------------
      !! 0) Conservation checkand changes in each ice category
      !!----------------------------------------------------------------------------------------------
      IF( con_i ) THEN
         CALL lim_column_sum (jpl,   v_i, vt_i_init)
         CALL lim_column_sum (jpl,   v_s, vt_s_init)
         CALL lim_column_sum_energy (jpl, nlay_i,   e_i, et_i_init)
         dummy_es(:,:,:) = e_s(:,:,1,:)
         CALL lim_column_sum (jpl, dummy_es(:,:,:) , et_s_init)
      ENDIF

      !!----------------------------------------------------------------------------------------------
      !! 1) Compute thickness and changes in each ice category
      !!----------------------------------------------------------------------------------------------
      IF( kt == nit000 .AND. lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'lim_itd_th_rem  : Remapping the ice thickness distribution'
         WRITE(numout,*) '~~~~~~~~~~~~~~~'
         WRITE(numout,*) ' klbnd :       ', klbnd
         WRITE(numout,*) ' kubnd :       ', kubnd
         WRITE(numout,*) ' ntyp  :       ', ntyp 
      ENDIF

      zdhice(:,:,:) = 0._wp
      DO jl = klbnd, kubnd
         DO jj = 1, jpj
            DO ji = 1, jpi
               zindb             = 1.0-MAX(0.0,SIGN(1.0,-a_i(ji,jj,jl)))     !0 if no ice and 1 if yes
               ht_i(ji,jj,jl)    = v_i(ji,jj,jl) / MAX(a_i(ji,jj,jl),epsi10) * zindb
               zindb             = 1.0-MAX(0.0,SIGN(1.0,-old_a_i(ji,jj,jl))) !0 if no ice and 1 if yes
               zht_i_o(ji,jj,jl) = old_v_i(ji,jj,jl) / MAX(old_a_i(ji,jj,jl),epsi10) * zindb
               IF( a_i(ji,jj,jl) > 1e-6 )   zdhice(ji,jj,jl) = ht_i(ji,jj,jl) - zht_i_o(ji,jj,jl) 
            END DO
         END DO
      END DO

      !-----------------------------------------------------------------------------------------------
      !  2) Compute fractional ice area in each grid cell
      !-----------------------------------------------------------------------------------------------
      at_i(:,:) = 0._wp
      DO jl = klbnd, kubnd
         at_i(:,:) = at_i(:,:) + a_i(:,:,jl)
      END DO

      !-----------------------------------------------------------------------------------------------
      !  3) Identify grid cells with ice
      !-----------------------------------------------------------------------------------------------
      nbrem = 0
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF ( at_i(ji,jj) .gt. zareamin ) THEN
               nbrem         = nbrem + 1
               nind_i(nbrem) = ji
               nind_j(nbrem) = jj
               zremap_flag(ji,jj) = 1
            ELSE
               zremap_flag(ji,jj) = 0
            ENDIF
         END DO !ji
      END DO !jj

      !-----------------------------------------------------------------------------------------------
      !  4) Compute new category boundaries
      !-----------------------------------------------------------------------------------------------
      !- 4.1 Compute category boundaries
      ! Tricky trick see limitd_me.F90
      ! will be soon removed, CT
      ! hi_max(kubnd) = 999.99
      zhbnew(:,:,:) = 0._wp

      DO jl = klbnd, kubnd - 1
         DO ji = 1, nbrem
            zji = nind_i(ji)
            zjj = nind_j(ji)
            !
            IF ( ( zht_i_o(zji,zjj,jl)  .GT.epsi10 ) .AND. & 
               ( zht_i_o(zji,zjj,jl+1).GT.epsi10 ) ) THEN
               !interpolate between adjacent category growth rates
               zslope = ( zdhice(zji,zjj,jl+1)     - zdhice(zji,zjj,jl) ) / &
                  ( zht_i_o   (zji,zjj,jl+1) - zht_i_o   (zji,zjj,jl) )
               zhbnew(zji,zjj,jl) = hi_max(jl) + zdhice(zji,zjj,jl) + &
                  zslope * ( hi_max(jl) - zht_i_o(zji,zjj,jl) )
            ELSEIF (zht_i_o(zji,zjj,jl).gt.epsi10) THEN
               zhbnew(zji,zjj,jl) = hi_max(jl) + zdhice(zji,zjj,jl)
            ELSEIF (zht_i_o(zji,zjj,jl+1).gt.epsi10) THEN
               zhbnew(zji,zjj,jl) = hi_max(jl) + zdhice(zji,zjj,jl+1)
            ELSE
               zhbnew(zji,zjj,jl) = hi_max(jl)
            ENDIF
         END DO

         !- 4.2 Check that each zhbnew lies between adjacent values of ice thickness
         DO ji = 1, nbrem
            ! jl, ji
            zji = nind_i(ji)
            zjj = nind_j(ji)
            ! jl, ji
            IF ( ( a_i(zji,zjj,jl) .GT.epsi10) .AND. & 
               ( ht_i(zji,zjj,jl).GE. zhbnew(zji,zjj,jl) ) &
               ) THEN
               zremap_flag(zji,zjj) = 0
            ELSEIF ( ( a_i(zji,zjj,jl+1) .GT. epsi10 ) .AND. &
               ( ht_i(zji,zjj,jl+1).LE. zhbnew(zji,zjj,jl) ) &
               ) THEN
               zremap_flag(zji,zjj) = 0
            ENDIF

            !- 4.3 Check that each zhbnew does not exceed maximal values hi_max  
            ! jl, ji
            IF (zhbnew(zji,zjj,jl).gt.hi_max(jl+1)) THEN
               zremap_flag(zji,zjj) = 0
            ENDIF
            ! jl, ji
            IF (zhbnew(zji,zjj,jl).lt.hi_max(jl-1)) THEN
               zremap_flag(zji,zjj) = 0
            ENDIF
            ! jl, ji
         END DO !ji
         ! ji
      END DO !jl

      !-----------------------------------------------------------------------------------------------
      !  5) Identify cells where ITD is to be remapped
      !-----------------------------------------------------------------------------------------------
      nbrem = 0
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF ( zremap_flag(ji,jj) == 1 ) THEN
               nbrem         = nbrem + 1
               nind_i(nbrem) = ji
               nind_j(nbrem) = jj
            ENDIF
         END DO !ji
      END DO !jj

      !-----------------------------------------------------------------------------------------------
      !  6) Fill arrays with lowermost / uppermost boundaries of 'new' categories
      !-----------------------------------------------------------------------------------------------
      DO jj = 1, jpj
         DO ji = 1, jpi
            zhb0(ji,jj) = hi_max_typ(0,ntyp) ! 0eme
            zhb1(ji,jj) = hi_max_typ(1,ntyp) ! 1er

            zhbnew(ji,jj,klbnd-1) = 0._wp

            IF( a_i(ji,jj,kubnd) > epsi10 ) THEN
               zhbnew(ji,jj,kubnd) = 3._wp * ht_i(ji,jj,kubnd) - 2._wp * zhbnew(ji,jj,kubnd-1)
            ELSE
               zhbnew(ji,jj,kubnd) = hi_max(kubnd)
            ENDIF

            IF( zhbnew(ji,jj,kubnd) < hi_max(kubnd-1) )   zhbnew(ji,jj,kubnd) = hi_max(kubnd-1)

         END DO !jj
      END DO !jj

      !-----------------------------------------------------------------------------------------------
      !  7) Compute g(h) 
      !-----------------------------------------------------------------------------------------------
      !- 7.1 g(h) for category 1 at start of time step
      CALL lim_itd_fitline( klbnd, zhb0, zhb1, zht_i_o(:,:,klbnd),         &
         &                  g0(:,:,klbnd), g1(:,:,klbnd), hL(:,:,klbnd),   &
         &                  hR(:,:,klbnd), zremap_flag )

      !- 7.2 Area lost due to melting of thin ice (first category,  klbnd)
      DO ji = 1, nbrem
         zji = nind_i(ji) 
         zjj = nind_j(ji) 

         !ji
         IF (a_i(zji,zjj,klbnd) .gt. epsi10) THEN
            zdh0 = zdhice(zji,zjj,klbnd) !decrease of ice thickness in the lower category
            ! ji, a_i > epsi10
            IF (zdh0 .lt. 0.0) THEN !remove area from category 1
               ! ji, a_i > epsi10; zdh0 < 0
               zdh0 = MIN(-zdh0,hi_max(klbnd))

               !Integrate g(1) from 0 to dh0 to estimate area melted
               zetamax = MIN(zdh0,hR(zji,zjj,klbnd)) - hL(zji,zjj,klbnd)
               IF (zetamax.gt.0.0) THEN
                  zx1  = zetamax
                  zx2  = 0.5 * zetamax*zetamax 
                  zda0 = g1(zji,zjj,klbnd) * zx2 + g0(zji,zjj,klbnd) * zx1 !ice area removed
                  ! Constrain new thickness <= ht_i
                  zdamax = a_i(zji,zjj,klbnd) * & 
                     (1.0 - ht_i(zji,zjj,klbnd)/zht_i_o(zji,zjj,klbnd)) ! zdamax > 0
                  !ice area lost due to melting of thin ice
                  zda0   = MIN(zda0, zdamax)

                  ! Remove area, conserving volume
                  ht_i(zji,zjj,klbnd) = ht_i(zji,zjj,klbnd) & 
                     * a_i(zji,zjj,klbnd) / ( a_i(zji,zjj,klbnd) - zda0 )
                  a_i(zji,zjj,klbnd)  = a_i(zji,zjj,klbnd) - zda0
                  v_i(zji,zjj,klbnd)  = a_i(zji,zjj,klbnd)*ht_i(zji,zjj,klbnd)
               ENDIF     ! zetamax > 0
               ! ji, a_i > epsi10

            ELSE ! if ice accretion
               ! ji, a_i > epsi10; zdh0 > 0
               IF ( ntyp .EQ. 1 ) zhbnew(zji,zjj,klbnd-1) = MIN(zdh0,hi_max(klbnd)) 
               ! zhbnew was 0, and is shifted to the right to account for thin ice
               ! growth in openwater (F0 = f1)
               IF ( ntyp .NE. 1 ) zhbnew(zji,zjj,0) = 0 
               ! in other types there is
               ! no open water growth (F0 = 0)
            ENDIF ! zdh0 

            ! a_i > epsi10
         ENDIF ! a_i > epsi10

      END DO ! ji

      !- 7.3 g(h) for each thickness category  
      DO jl = klbnd, kubnd
         CALL lim_itd_fitline(jl, zhbnew(:,:,jl-1), zhbnew(:,:,jl), ht_i(:,:,jl), &
            g0(:,:,jl), g1(:,:,jl), hL(:,:,jl), hR(:,:,jl),     &
            zremap_flag)
      END DO

      !-----------------------------------------------------------------------------------------------
      !  8) Compute area and volume to be shifted across each boundary
      !-----------------------------------------------------------------------------------------------

      DO jl = klbnd, kubnd - 1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zdonor(ji,jj,jl) = 0
               zdaice(ji,jj,jl) = 0.0
               zdvice(ji,jj,jl) = 0.0
            END DO
         END DO

         DO ji = 1, nbrem
            zji = nind_i(ji)
            zjj = nind_j(ji)

            IF (zhbnew(zji,zjj,jl) .gt. hi_max(jl)) THEN ! transfer from jl to jl+1

               ! left and right integration limits in eta space
               zvetamin(ji) = MAX(hi_max(jl), hL(zji,zjj,jl)) - hL(zji,zjj,jl)
               zvetamax(ji) = MIN(zhbnew(zji,zjj,jl), hR(zji,zjj,jl)) - hL(zji,zjj,jl)
               zdonor(zji,zjj,jl) = jl

            ELSE  ! zhbnew(jl) <= hi_max(jl) ; transfer from jl+1 to jl

               ! left and right integration limits in eta space
               zvetamin(ji) = 0.0
               zvetamax(ji) = MIN(hi_max(jl), hR(zji,zjj,jl+1)) - hL(zji,zjj,jl+1)
               zdonor(zji,zjj,jl) = jl + 1

            ENDIF  ! zhbnew(jl) > hi_max(jl)

            zetamax = MAX(zvetamax(ji), zvetamin(ji)) ! no transfer if etamax < etamin
            zetamin = zvetamin(ji)

            zx1  = zetamax - zetamin
            zwk1 = zetamin*zetamin
            zwk2 = zetamax*zetamax
            zx2  = 0.5 * (zwk2 - zwk1)
            zwk1 = zwk1 * zetamin
            zwk2 = zwk2 * zetamax
            zx3  = 1.0/3.0 * (zwk2 - zwk1)
            nd   = zdonor(zji,zjj,jl)
            zdaice(zji,zjj,jl) = g1(zji,zjj,nd)*zx2 + g0(zji,zjj,nd)*zx1
            zdvice(zji,zjj,jl) = g1(zji,zjj,nd)*zx3 + g0(zji,zjj,nd)*zx2 + &
               zdaice(zji,zjj,jl)*hL(zji,zjj,nd)

         END DO ! ji
      END DO ! jl klbnd -> kubnd - 1

      !!----------------------------------------------------------------------------------------------
      !! 9) Shift ice between categories
      !!----------------------------------------------------------------------------------------------
      CALL lim_itd_shiftice ( klbnd, kubnd, zdonor, zdaice, zdvice )

      !!----------------------------------------------------------------------------------------------
      !! 10) Make sure ht_i >= minimum ice thickness hi_min
      !!----------------------------------------------------------------------------------------------

      DO ji = 1, nbrem
         zji = nind_i(ji)
         zjj = nind_j(ji)
         IF ( ( zhimin .GT. 0.0 ) .AND. & 
            ( ( a_i(zji,zjj,1) .GT. epsi10 ) .AND. ( ht_i(zji,zjj,1) .LT. zhimin ) ) &
            ) THEN
            a_i(zji,zjj,1)  = a_i(zji,zjj,1) * ht_i(zji,zjj,1) / zhimin 
            ht_i(zji,zjj,1) = zhimin
            v_i(zji,zjj,1)  = a_i(zji,zjj,1)*ht_i(zji,zjj,1)
         ENDIF
      END DO !ji

      !!----------------------------------------------------------------------------------------------
      !! 11) Conservation check
      !!----------------------------------------------------------------------------------------------
      IF ( con_i ) THEN
         CALL lim_column_sum (jpl,   v_i, vt_i_final)
         fieldid = ' v_i : limitd_th '
         CALL lim_cons_check (vt_i_init, vt_i_final, 1.0e-6, fieldid) 

         CALL lim_column_sum_energy (jpl, nlay_i,  e_i, et_i_final)
         fieldid = ' e_i : limitd_th '
         CALL lim_cons_check (et_i_init, et_i_final, 1.0e-3, fieldid) 

         CALL lim_column_sum (jpl,   v_s, vt_s_final)
         fieldid = ' v_s : limitd_th '
         CALL lim_cons_check (vt_s_init, vt_s_final, 1.0e-6, fieldid) 

         dummy_es(:,:,:) = e_s(:,:,1,:)
         CALL lim_column_sum (jpl, dummy_es(:,:,:) , et_s_final)
         fieldid = ' e_s : limitd_th '
         CALL lim_cons_check (et_s_init, et_s_final, 1.0e-3, fieldid) 
      ENDIF

      CALL wrk_dealloc( jpi,jpj, zremap_flag )    ! integer
      CALL wrk_dealloc( jpi,jpj,jpl-1, zdonor )   ! integer
      CALL wrk_dealloc( jpi,jpj,jpl, zdhice, g0, g1, hL, hR, zht_i_o, dummy_es )
      CALL wrk_dealloc( jpi,jpj,jpl-1, zdaice, zdvice )   
      CALL wrk_dealloc( jpi,jpj,jpl+1, zhbnew, kkstart = 0 )   
      CALL wrk_dealloc( (jpi+1)*(jpj+1), zvetamin, zvetamax )   
      CALL wrk_dealloc( (jpi+1)*(jpj+1), nind_i, nind_j )   ! integer 
      CALL wrk_dealloc( jpi,jpj, zhb0,zhb1,vt_i_init,vt_i_final,vt_s_init,vt_s_final,et_i_init,et_i_final,et_s_init,et_s_final )

   END SUBROUTINE lim_itd_th_rem


   SUBROUTINE lim_itd_fitline( num_cat, HbL, Hbr, hice,   &
      &                        g0, g1, hL, hR, zremap_flag )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_itd_fitline ***
      !!
      !! ** Purpose :   fit g(h) with a line using area, volume constraints
      !!
      !! ** Method  :   Fit g(h) with a line, satisfying area and volume constraints.
      !!              To reduce roundoff errors caused by large values of g0 and g1,
      !!              we actually compute g(eta), where eta = h - hL, and hL is the
      !!              left boundary.
      !!------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   num_cat      ! category index
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) ::   HbL, HbR     ! left and right category boundaries
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) ::   hice         ! ice thickness
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) ::   g0, g1       ! coefficients in linear equation for g(eta)
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) ::   hL           ! min value of range over which g(h) > 0
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) ::   hR           ! max value of range over which g(h) > 0
      INTEGER , DIMENSION(jpi,jpj), INTENT(in   ) ::   zremap_flag  !
      !
      INTEGER ::   ji,jj           ! horizontal indices
      REAL(wp) ::   zh13         ! HbL + 1/3 * (HbR - HbL)
      REAL(wp) ::   zh23         ! HbL + 2/3 * (HbR - HbL)
      REAL(wp) ::   zdhr         ! 1 / (hR - hL)
      REAL(wp) ::   zwk1, zwk2   ! temporary variables
      REAL(wp) ::   zacrith      ! critical minimum concentration in an ice category
      !!------------------------------------------------------------------
      !
      zacrith       = 1.0e-6
      !
      DO jj = 1, jpj
         DO ji = 1, jpi
            !
            IF( zremap_flag(ji,jj) == 1 .AND. a_i(ji,jj,num_cat) > zacrith   &
               &                        .AND. hice(ji,jj)        > 0._wp     ) THEN

               ! Initialize hL and hR

               hL(ji,jj) = HbL(ji,jj)
               hR(ji,jj) = HbR(ji,jj)

               ! Change hL or hR if hice falls outside central third of range

               zh13 = 1.0/3.0 * (2.0*hL(ji,jj) + hR(ji,jj))
               zh23 = 1.0/3.0 * (hL(ji,jj) + 2.0*hR(ji,jj))

               IF    ( hice(ji,jj) < zh13 ) THEN   ;   hR(ji,jj) = 3._wp * hice(ji,jj) - 2._wp * hL(ji,jj)
               ELSEIF( hice(ji,jj) > zh23 ) THEN   ;   hL(ji,jj) = 3._wp * hice(ji,jj) - 2._wp * hR(ji,jj)
               ENDIF

               ! Compute coefficients of g(eta) = g0 + g1*eta

               zdhr = 1._wp / (hR(ji,jj) - hL(ji,jj))
               zwk1 = 6._wp * a_i(ji,jj,num_cat) * zdhr
               zwk2 = ( hice(ji,jj) - hL(ji,jj) ) * zdhr
               g0(ji,jj) = zwk1 * ( 2._wp/3._wp - zwk2 )
               g1(ji,jj) = 2._wp * zdhr * zwk1 * (zwk2 - 0.5)
               !
            ELSE                   ! remap_flag = .false. or a_i < epsi10 
               hL(ji,jj) = 0._wp
               hR(ji,jj) = 0._wp
               g0(ji,jj) = 0._wp
               g1(ji,jj) = 0._wp
            ENDIF                  ! a_i > epsi10
            !
         END DO
      END DO
      !
   END SUBROUTINE lim_itd_fitline


   SUBROUTINE lim_itd_shiftice( klbnd, kubnd, zdonor, zdaice, zdvice )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_itd_shiftice ***
      !!
      !! ** Purpose :   shift ice across category boundaries, conserving everything
      !!              ( area, volume, energy, age*vol, and mass of salt )
      !!
      !! ** Method  :
      !!------------------------------------------------------------------
      INTEGER                           , INTENT(in   ) ::   klbnd    ! Start thickness category index point
      INTEGER                           , INTENT(in   ) ::   kubnd    ! End point on which the  the computation is applied
      INTEGER , DIMENSION(jpi,jpj,jpl-1), INTENT(in   ) ::   zdonor   ! donor category index
      REAL(wp), DIMENSION(jpi,jpj,jpl-1), INTENT(inout) ::   zdaice   ! ice area transferred across boundary
      REAL(wp), DIMENSION(jpi,jpj,jpl-1), INTENT(inout) ::   zdvice   ! ice volume transferred across boundary

      INTEGER ::   ji, jj, jl, jl2, jl1, jk   ! dummy loop indices
      INTEGER ::   zji, zjj          ! indices when changing from 2D-1D is done

      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zaTsfn
      REAL(wp), POINTER, DIMENSION(:,:)   ::   zworka            ! temporary array used here

      REAL(wp) ::   zdvsnow, zdesnow   ! snow volume and energy transferred
      REAL(wp) ::   zdeice             ! ice energy transferred
      REAL(wp) ::   zdsm_vice          ! ice salinity times volume transferred
      REAL(wp) ::   zdo_aice           ! ice age times volume transferred
      REAL(wp) ::   zdaTsf             ! aicen*Tsfcn transferred
      REAL(wp) ::   zindsn             ! snow or not
      REAL(wp) ::   zindb              ! ice or not

      INTEGER, POINTER, DIMENSION(:) ::   nind_i, nind_j   ! compressed indices for i/j directions

      INTEGER ::   nbrem             ! number of cells with ice to transfer

      LOGICAL ::   zdaice_negative         ! true if daice < -puny
      LOGICAL ::   zdvice_negative         ! true if dvice < -puny
      LOGICAL ::   zdaice_greater_aicen    ! true if daice > aicen
      LOGICAL ::   zdvice_greater_vicen    ! true if dvice > vicen
      !!------------------------------------------------------------------

      CALL wrk_alloc( jpi,jpj,jpl, zaTsfn )
      CALL wrk_alloc( jpi,jpj, zworka )
      CALL wrk_alloc( (jpi+1)*(jpj+1), nind_i, nind_j )   ! integer

      !----------------------------------------------------------------------------------------------
      ! 1) Define a variable equal to a_i*T_su
      !----------------------------------------------------------------------------------------------

      DO jl = klbnd, kubnd
         zaTsfn(:,:,jl) = a_i(:,:,jl)*t_su(:,:,jl)
      END DO

      !----------------------------------------------------------------------------------------------
      ! 2) Check for daice or dvice out of range, allowing for roundoff error
      !----------------------------------------------------------------------------------------------
      ! Note: zdaice < 0 or zdvice < 0 usually happens when category jl
      ! has a small area, with h(n) very close to a boundary.  Then
      ! the coefficients of g(h) are large, and the computed daice and
      ! dvice can be in error. If this happens, it is best to transfer
      ! either the entire category or nothing at all, depending on which
      ! side of the boundary hice(n) lies.
      !-----------------------------------------------------------------
      DO jl = klbnd, kubnd-1

         zdaice_negative = .false.
         zdvice_negative = .false.
         zdaice_greater_aicen = .false.
         zdvice_greater_vicen = .false.

         DO jj = 1, jpj
            DO ji = 1, jpi

               IF (zdonor(ji,jj,jl) .GT. 0) THEN
                  jl1 = zdonor(ji,jj,jl)

                  IF (zdaice(ji,jj,jl) .LT. 0.0) THEN
                     IF (zdaice(ji,jj,jl) .GT. -epsi10) THEN
                        IF ( ( jl1.EQ.jl   .AND. ht_i(ji,jj,jl1) .GT. hi_max(jl) )           &
                           .OR.                                      &
                           ( jl1.EQ.jl+1 .AND. ht_i(ji,jj,jl1) .LE. hi_max(jl) )           &  
                           ) THEN                                                             
                           zdaice(ji,jj,jl) = a_i(ji,jj,jl1)  ! shift entire category
                           zdvice(ji,jj,jl) = v_i(ji,jj,jl1)
                        ELSE
                           zdaice(ji,jj,jl) = 0.0 ! shift no ice
                           zdvice(ji,jj,jl) = 0.0
                        ENDIF
                     ELSE
                        zdaice_negative = .true.
                     ENDIF
                  ENDIF

                  IF (zdvice(ji,jj,jl) .LT. 0.0) THEN
                     IF (zdvice(ji,jj,jl) .GT. -epsi10 ) THEN
                        IF ( ( jl1.EQ.jl .AND. ht_i(ji,jj,jl1).GT.hi_max(jl) )     &
                           .OR.                                     &
                           ( jl1.EQ.jl+1 .AND. ht_i(ji,jj,jl1) .LE. hi_max(jl) ) &
                           ) THEN
                           zdaice(ji,jj,jl) = a_i(ji,jj,jl1) ! shift entire category
                           zdvice(ji,jj,jl) = v_i(ji,jj,jl1) 
                        ELSE
                           zdaice(ji,jj,jl) = 0.0    ! shift no ice
                           zdvice(ji,jj,jl) = 0.0
                        ENDIF
                     ELSE
                        zdvice_negative = .true.
                     ENDIF
                  ENDIF

                  ! If daice is close to aicen, set daice = aicen.
                  IF (zdaice(ji,jj,jl) .GT. a_i(ji,jj,jl1) - epsi10 ) THEN
                     IF (zdaice(ji,jj,jl) .LT. a_i(ji,jj,jl1)+epsi10) THEN
                        zdaice(ji,jj,jl) = a_i(ji,jj,jl1)
                        zdvice(ji,jj,jl) = v_i(ji,jj,jl1) 
                     ELSE
                        zdaice_greater_aicen = .true.
                     ENDIF
                  ENDIF

                  IF (zdvice(ji,jj,jl) .GT. v_i(ji,jj,jl1)-epsi10) THEN
                     IF (zdvice(ji,jj,jl) .LT. v_i(ji,jj,jl1)+epsi10) THEN
                        zdaice(ji,jj,jl) = a_i(ji,jj,jl1)
                        zdvice(ji,jj,jl) = v_i(ji,jj,jl1) 
                     ELSE
                        zdvice_greater_vicen = .true.
                     ENDIF
                  ENDIF

               ENDIF               ! donor > 0
            END DO                   ! i
         END DO                 ! j

      END DO !jl

      !-------------------------------------------------------------------------------
      ! 3) Transfer volume and energy between categories
      !-------------------------------------------------------------------------------

      DO jl = klbnd, kubnd - 1
         nbrem = 0
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF (zdaice(ji,jj,jl) .GT. 0.0 ) THEN ! daice(n) can be < puny
                  nbrem = nbrem + 1
                  nind_i(nbrem) = ji
                  nind_j(nbrem) = jj
               ENDIF ! tmask
            END DO
         END DO

         DO ji = 1, nbrem 
            zji = nind_i(ji)
            zjj = nind_j(ji)

            jl1 = zdonor(zji,zjj,jl)
            zindb             = MAX( 0.0 , SIGN( 1.0 , v_i(zji,zjj,jl1) - epsi10 ) )
            zworka(zji,zjj)   = zdvice(zji,zjj,jl) / MAX(v_i(zji,zjj,jl1),epsi10) * zindb
            IF( jl1 == jl) THEN   ;   jl2 = jl1+1
            ELSE                    ;   jl2 = jl 
            ENDIF

            !--------------
            ! Ice areas
            !--------------

            a_i(zji,zjj,jl1) = a_i(zji,zjj,jl1) - zdaice(zji,zjj,jl)
            a_i(zji,zjj,jl2) = a_i(zji,zjj,jl2) + zdaice(zji,zjj,jl)

            !--------------
            ! Ice volumes
            !--------------

            v_i(zji,zjj,jl1) = v_i(zji,zjj,jl1) - zdvice(zji,zjj,jl) 
            v_i(zji,zjj,jl2) = v_i(zji,zjj,jl2) + zdvice(zji,zjj,jl)

            !--------------
            ! Snow volumes
            !--------------

            zdvsnow          = v_s(zji,zjj,jl1) * zworka(zji,zjj)
            v_s(zji,zjj,jl1) = v_s(zji,zjj,jl1) - zdvsnow
            v_s(zji,zjj,jl2) = v_s(zji,zjj,jl2) + zdvsnow 

            !--------------------
            ! Snow heat content  
            !--------------------

            zdesnow              = e_s(zji,zjj,1,jl1) * zworka(zji,zjj)
            e_s(zji,zjj,1,jl1)   = e_s(zji,zjj,1,jl1) - zdesnow
            e_s(zji,zjj,1,jl2)   = e_s(zji,zjj,1,jl2) + zdesnow

            !--------------
            ! Ice age 
            !--------------

            zdo_aice             = oa_i(zji,zjj,jl1) * zdaice(zji,zjj,jl)
            oa_i(zji,zjj,jl1)    = oa_i(zji,zjj,jl1) - zdo_aice
            oa_i(zji,zjj,jl2)    = oa_i(zji,zjj,jl2) + zdo_aice

            !--------------
            ! Ice salinity
            !--------------

            zdsm_vice            = smv_i(zji,zjj,jl1) * zworka(zji,zjj)
            smv_i(zji,zjj,jl1)   = smv_i(zji,zjj,jl1) - zdsm_vice
            smv_i(zji,zjj,jl2)   = smv_i(zji,zjj,jl2) + zdsm_vice

            !---------------------
            ! Surface temperature
            !---------------------

            zdaTsf               = t_su(zji,zjj,jl1) * zdaice(zji,zjj,jl)
            zaTsfn(zji,zjj,jl1)  = zaTsfn(zji,zjj,jl1) - zdaTsf
            zaTsfn(zji,zjj,jl2)  = zaTsfn(zji,zjj,jl2) + zdaTsf 

         END DO                 ! ji

         !------------------
         ! Ice heat content
         !------------------

         DO jk = 1, nlay_i
!CDIR NODEP
            DO ji = 1, nbrem
               zji = nind_i(ji)
               zjj = nind_j(ji)

               jl1 = zdonor(zji,zjj,jl)
               IF (jl1 .EQ. jl) THEN
                  jl2 = jl+1
               ELSE             ! n1 = n+1
                  jl2 = jl 
               ENDIF

               zdeice = e_i(zji,zjj,jk,jl1) * zworka(zji,zjj)
               e_i(zji,zjj,jk,jl1) =  e_i(zji,zjj,jk,jl1) - zdeice
               e_i(zji,zjj,jk,jl2) =  e_i(zji,zjj,jk,jl2) + zdeice 
            END DO              ! ji
         END DO                 ! jk

      END DO                   ! boundaries, 1 to ncat-1

      !-----------------------------------------------------------------
      ! Update ice thickness and temperature
      !-----------------------------------------------------------------

      DO jl = klbnd, kubnd
         DO jj = 1, jpj
            DO ji = 1, jpi 
               IF ( a_i(ji,jj,jl) > epsi10 ) THEN 
                  ht_i(ji,jj,jl)  =  v_i   (ji,jj,jl) / a_i(ji,jj,jl) 
                  t_su(ji,jj,jl)  =  zaTsfn(ji,jj,jl) / a_i(ji,jj,jl) 
                  zindsn          =  1.0 - MAX(0.0,SIGN(1.0,-v_s(ji,jj,jl))) !0 if no ice and 1 if yes
               ELSE
                  ht_i(ji,jj,jl)  = 0._wp
                  t_su(ji,jj,jl)  = rtt
               ENDIF
            END DO                 ! ji
         END DO                 ! jj
      END DO                    ! jl
      !
      CALL wrk_dealloc( jpi,jpj,jpl, zaTsfn )
      CALL wrk_dealloc( jpi,jpj, zworka )
      CALL wrk_dealloc( (jpi+1)*(jpj+1), nind_i, nind_j )   ! integer
      !
   END SUBROUTINE lim_itd_shiftice
   

   SUBROUTINE lim_itd_th_reb( klbnd, kubnd, ntyp )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_itd_th_reb ***
      !!
      !! ** Purpose : rebin - rebins thicknesses into defined categories
      !!
      !! ** Method  :
      !!------------------------------------------------------------------
      INTEGER , INTENT (in) ::   klbnd   ! Start thickness category index point
      INTEGER , INTENT (in) ::   kubnd   ! End point on which the  the computation is applied
      INTEGER , INTENT (in) ::   ntyp    ! number of the ice type involved in the rebinning process
      !
      INTEGER ::   ji,jj, jl   ! dummy loop indices
      INTEGER ::   zshiftflag          ! = .true. if ice must be shifted
      CHARACTER (len = 15) :: fieldid

      INTEGER , POINTER, DIMENSION(:,:,:) ::   zdonor           ! donor category index
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zdaice, zdvice   ! ice area and volume transferred

      REAL(wp), POINTER, DIMENSION(:,:) ::   vt_i_init, vt_i_final   ! ice volume summed over categories
      REAL(wp), POINTER, DIMENSION(:,:) ::   vt_s_init, vt_s_final   ! snow volume summed over categories
      !!------------------------------------------------------------------
      
      CALL wrk_alloc( jpi,jpj,jpl, zdonor )   ! interger
      CALL wrk_alloc( jpi,jpj,jpl, zdaice, zdvice )
      CALL wrk_alloc( jpi,jpj, vt_i_init, vt_i_final, vt_s_init, vt_s_final )
      !     
      IF( con_i ) THEN                 ! conservation check
         CALL lim_column_sum (jpl,   v_i, vt_i_init)
         CALL lim_column_sum (jpl,   v_s, vt_s_init)
      ENDIF

      !
      !------------------------------------------------------------------------------
      ! 1) Compute ice thickness.
      !------------------------------------------------------------------------------
      DO jl = klbnd, kubnd
         DO jj = 1, jpj
            DO ji = 1, jpi 
               IF( a_i(ji,jj,jl) > epsi10 ) THEN 
                  ht_i(ji,jj,jl) = v_i(ji,jj,jl) / a_i(ji,jj,jl)
               ELSE
                  ht_i(ji,jj,jl) = 0._wp
               ENDIF
            END DO
         END DO
      END DO

      !------------------------------------------------------------------------------
      ! 2) Make sure thickness of cat klbnd is at least hi_max_typ(klbnd)
      !------------------------------------------------------------------------------
      DO jj = 1, jpj 
         DO ji = 1, jpi 
            IF( a_i(ji,jj,klbnd) > epsi10 ) THEN
               IF( ht_i(ji,jj,klbnd) <= hi_max_typ(0,ntyp) .AND. hi_max_typ(0,ntyp) > 0._wp ) THEN
                  a_i(ji,jj,klbnd)  = v_i(ji,jj,klbnd) / hi_max_typ(0,ntyp) 
                  ht_i(ji,jj,klbnd) = hi_max_typ(0,ntyp)
               ENDIF
            ENDIF
         END DO
      END DO

      !------------------------------------------------------------------------------
      ! 3) If a category thickness is not in bounds, shift the
      ! entire area, volume, and energy to the neighboring category
      !------------------------------------------------------------------------------
      !-------------------------
      ! Initialize shift arrays
      !-------------------------
      DO jl = klbnd, kubnd
         zdonor(:,:,jl) = 0
         zdaice(:,:,jl) = 0._wp
         zdvice(:,:,jl) = 0._wp
      END DO

      !-------------------------
      ! Move thin categories up
      !-------------------------

      DO jl = klbnd, kubnd - 1  ! loop over category boundaries

         !---------------------------------------
         ! identify thicknesses that are too big
         !---------------------------------------
         zshiftflag = 0

         DO jj = 1, jpj 
            DO ji = 1, jpi 
               IF( a_i(ji,jj,jl) > epsi10 .AND. ht_i(ji,jj,jl) > hi_max(jl) ) THEN 
                  zshiftflag        = 1
                  zdonor(ji,jj,jl)  = jl 
                  zdaice(ji,jj,jl)  = a_i(ji,jj,jl)
                  zdvice(ji,jj,jl)  = v_i(ji,jj,jl)
               ENDIF
            END DO                 ! ji
         END DO                 ! jj
         IF(lk_mpp)   CALL mpp_max( zshiftflag )

         IF( zshiftflag == 1 ) THEN            ! Shift ice between categories
            CALL lim_itd_shiftice( klbnd, kubnd, zdonor, zdaice, zdvice )
            ! Reset shift parameters
            zdonor(:,:,jl) = 0
            zdaice(:,:,jl) = 0._wp
            zdvice(:,:,jl) = 0._wp
         ENDIF
         !
      END DO                    ! jl

      !----------------------------
      ! Move thick categories down
      !----------------------------

      DO jl = kubnd - 1, 1, -1       ! loop over category boundaries

         !-----------------------------------------
         ! Identify thicknesses that are too small
         !-----------------------------------------
         zshiftflag = 0

         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( a_i(ji,jj,jl+1) >  epsi10 .AND.   &
                  ht_i(ji,jj,jl+1) <= hi_max(jl) ) THEN
                  !
                  zshiftflag = 1
                  zdonor(ji,jj,jl) = jl + 1
                  zdaice(ji,jj,jl) = a_i(ji,jj,jl+1) 
                  zdvice(ji,jj,jl) = v_i(ji,jj,jl+1)
               ENDIF
            END DO                 ! ji
         END DO                 ! jj

         IF(lk_mpp)   CALL mpp_max( zshiftflag )
         
         IF( zshiftflag == 1 ) THEN            ! Shift ice between categories
            CALL lim_itd_shiftice( klbnd, kubnd, zdonor, zdaice, zdvice )
            ! Reset shift parameters
            zdonor(:,:,jl) = 0
            zdaice(:,:,jl) = 0._wp
            zdvice(:,:,jl) = 0._wp
         ENDIF

      END DO                    ! jl

      !------------------------------------------------------------------------------
      ! 4) Conservation check
      !------------------------------------------------------------------------------

      IF( con_i ) THEN
         CALL lim_column_sum (jpl,   v_i, vt_i_final)
         fieldid = ' v_i : limitd_reb '
         CALL lim_cons_check (vt_i_init, vt_i_final, 1.0e-6, fieldid) 

         CALL lim_column_sum (jpl,   v_s, vt_s_final)
         fieldid = ' v_s : limitd_reb '
         CALL lim_cons_check (vt_s_init, vt_s_final, 1.0e-6, fieldid) 
      ENDIF
      !
      CALL wrk_dealloc( jpi,jpj,jpl, zdonor )   ! interger
      CALL wrk_dealloc( jpi,jpj,jpl, zdaice, zdvice )
      CALL wrk_dealloc( jpi,jpj, vt_i_init, vt_i_final, vt_s_init, vt_s_final )

   END SUBROUTINE lim_itd_th_reb

#else
   !!----------------------------------------------------------------------
   !!   Default option            Dummy module         NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_itd_th           ! Empty routines
   END SUBROUTINE lim_itd_th
   SUBROUTINE lim_itd_th_ini
   END SUBROUTINE lim_itd_th_ini
   SUBROUTINE lim_itd_th_rem
   END SUBROUTINE lim_itd_th_rem
   SUBROUTINE lim_itd_fitline
   END SUBROUTINE lim_itd_fitline
   SUBROUTINE lim_itd_shiftice
   END SUBROUTINE lim_itd_shiftice
   SUBROUTINE lim_itd_th_reb
   END SUBROUTINE lim_itd_th_reb
#endif
   !!======================================================================
END MODULE limitd_th
