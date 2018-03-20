MODULE dynspg_ts
   !!======================================================================
   !! History :   1.0  ! 2004-12  (L. Bessieres, G. Madec)  Original code
   !!              -   ! 2005-11  (V. Garnier, G. Madec)  optimization
   !!              -   ! 2006-08  (S. Masson)  distributed restart using iom
   !!             2.0  ! 2007-07  (D. Storkey) calls to BDY routines
   !!              -   ! 2008-01  (R. Benshila)  change averaging method
   !!             3.2  ! 2009-07  (R. Benshila, G. Madec) Complete revisit associated to vvl reactivation
   !!             3.3  ! 2010-09  (D. Storkey, E. O'Dea) update for BDY for Shelf configurations
   !!             3.3  ! 2011-03  (R. Benshila, R. Hordoir, P. Oddo) update calculation of ub_b
   !!---------------------------------------------------------------------
#if defined key_dynspg_ts   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_dynspg_ts'         free surface cst volume with time splitting
   !!----------------------------------------------------------------------
   !!   dyn_spg_ts  : compute surface pressure gradient trend using a time-
   !!                 splitting scheme and add to the general trend 
   !!   ts_rst      : read/write the time-splitting restart fields in the ocean restart file
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE sbc_oce         ! surface boundary condition: ocean
   USE dynspg_oce      ! surface pressure gradient variables
   USE phycst          ! physical constants
   USE domvvl          ! variable volume
   USE zdfbfr          ! bottom friction
   USE dynvor          ! vorticity term
   USE obc_oce         ! Lateral open boundary condition
   USE obc_par         ! open boundary condition parameters
   USE obcdta          ! open boundary condition data     
   USE obcfla          ! Flather open boundary condition  
   USE bdy_par         ! for lk_bdy
   USE bdy_oce         ! Lateral open boundary condition
   USE bdydta          ! open boundary condition data     
   USE bdydyn2d        ! open boundary conditions on barotropic variables
   USE sbctide
   USE updtide
   USE lib_mpp         ! distributed memory computing library
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE prtctl          ! Print control
   USE in_out_manager  ! I/O manager
   USE iom             ! IOM library
   USE zdf_oce         ! Vertical diffusion
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing


   IMPLICIT NONE
   PRIVATE

   PUBLIC dyn_spg_ts        ! routine called by step.F90
   PUBLIC ts_rst            ! routine called by istate.F90
   PUBLIC dyn_spg_ts_alloc  ! routine called by dynspg.F90


   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  ftnw, ftne   ! triad of coriolis parameter
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  ftsw, ftse   ! (only used with een vorticity scheme)

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   un_b, vn_b   ! now    averaged velocity
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ub_b, vb_b   ! before averaged velocity

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: dynspg_ts.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION dyn_spg_ts_alloc()
      !!----------------------------------------------------------------------
      !!                  ***  routine dyn_spg_ts_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( ftnw  (jpi,jpj) , ftne(jpi,jpj) , un_b(jpi,jpj) , vn_b(jpi,jpj) ,     &
         &      ftsw  (jpi,jpj) , ftse(jpi,jpj) , ub_b(jpi,jpj) , vb_b(jpi,jpj) , STAT= dyn_spg_ts_alloc )
         !
      IF( lk_mpp                )   CALL mpp_sum( dyn_spg_ts_alloc )
      IF( dyn_spg_ts_alloc /= 0 )   CALL ctl_warn('dynspg_oce_alloc: failed to allocate arrays')
      !
   END FUNCTION dyn_spg_ts_alloc


   SUBROUTINE dyn_spg_ts( kt )
      !!----------------------------------------------------------------------
      !!                  ***  routine dyn_spg_ts  ***
      !!
      !! ** Purpose :   Compute the now trend due to the surface pressure
      !!      gradient in case of free surface formulation with time-splitting.
      !!      Add it to the general trend of momentum equation.
      !!
      !! ** Method  :   Free surface formulation with time-splitting
      !!      -1- Save the vertically integrated trend. This general trend is
      !!          held constant over the barotropic integration.
      !!          The Coriolis force is removed from the general trend as the
      !!          surface gradient and the Coriolis force are updated within
      !!          the barotropic integration.
      !!      -2- Barotropic loop : updates of sea surface height (ssha_e) and 
      !!          barotropic velocity (ua_e and va_e) through barotropic 
      !!          momentum and continuity integration. Barotropic former 
      !!          variables are time averaging over the full barotropic cycle
      !!          (= 2 * baroclinic time step) and saved in uX_b 
      !!          and vX_b (X specifying after, now or before).
      !!      -3- The new general trend becomes :
      !!          ua = ua - sum_k(ua)/H + ( un_b - ub_b )
      !!
      !! ** Action : - Update (ua,va) with the surf. pressure gradient trend
      !!
      !! References : Griffies et al., (2003): A technical guide to MOM4. NOAA/GFDL
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT(in)  ::   kt   ! ocean time-step index
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      INTEGER  ::   icycle           ! local scalar
      INTEGER  ::   ikbu, ikbv       ! local scalar
      REAL(wp) ::   zraur, zcoef, z2dt_e, z1_2dt_b, z2dt_bf   ! local scalars
      REAL(wp) ::   z1_8, zx1, zy1                            !   -      -
      REAL(wp) ::   z1_4, zx2, zy2                            !   -      -
      REAL(wp) ::   zu_spg, zu_cor, zu_sld, zu_asp            !   -      -
      REAL(wp) ::   zv_spg, zv_cor, zv_sld, zv_asp            !   -      -
      REAL(wp) ::   ua_btm, va_btm                            !   -      -
      !
      REAL(wp), POINTER, DIMENSION(:,:) :: zsshun_e, zsshvn_e, zsshb_e, zssh_sum, zhdiv 
      REAL(wp), POINTER, DIMENSION(:,:) :: zua, zva, zun, zvn, zun_e, zvn_e, zub_e, zvb_e 
      REAL(wp), POINTER, DIMENSION(:,:) :: zcu, zcv, zwx, zwy, zbfru, zbfrv, zu_sum, zv_sum
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dyn_spg_ts')
      !
      CALL wrk_alloc( jpi, jpj, zsshun_e, zsshvn_e, zsshb_e, zssh_sum, zhdiv     )
      CALL wrk_alloc( jpi, jpj, zua, zva, zun, zvn, zun_e, zvn_e, zub_e, zvb_e   )
      CALL wrk_alloc( jpi, jpj, zcu, zcv, zwx, zwy, zbfru, zbfrv, zu_sum, zv_sum )
      !
      IF( kt == nit000 ) THEN             !* initialisation
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn_spg_ts : surface pressure gradient trend'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~   free surface with time splitting'
         IF(lwp) WRITE(numout,*) ' Number of sub cycle in 1 time-step (2 rdt) : icycle = ',  2*nn_baro
         !
         CALL ts_rst( nit000, 'READ' )   ! read or initialize the following fields: un_b, vn_b  
         !
         ua_e  (:,:) = un_b (:,:)
         va_e  (:,:) = vn_b (:,:)
         hu_e  (:,:) = hu   (:,:)
         hv_e  (:,:) = hv   (:,:)
         hur_e (:,:) = hur  (:,:)
         hvr_e (:,:) = hvr  (:,:)
         IF( ln_dynvor_een ) THEN
            ftne(1,:) = 0._wp   ;   ftnw(1,:) = 0._wp   ;   ftse(1,:) = 0._wp   ;   ftsw(1,:) = 0._wp
            DO jj = 2, jpj
               DO ji = fs_2, jpi   ! vector opt.
                  ftne(ji,jj) = ( ff(ji-1,jj  ) + ff(ji  ,jj  ) + ff(ji  ,jj-1) ) / 3._wp
                  ftnw(ji,jj) = ( ff(ji-1,jj-1) + ff(ji-1,jj  ) + ff(ji  ,jj  ) ) / 3._wp
                  ftse(ji,jj) = ( ff(ji  ,jj  ) + ff(ji  ,jj-1) + ff(ji-1,jj-1) ) / 3._wp
                  ftsw(ji,jj) = ( ff(ji  ,jj-1) + ff(ji-1,jj-1) + ff(ji-1,jj  ) ) / 3._wp
               END DO
            END DO
         ENDIF
         !
      ENDIF

      !                                                     !* Local constant initialization
      z1_2dt_b = 1._wp / ( 2.0_wp * rdt )                   ! reciprocal of baroclinic time step
      IF( neuler == 0 .AND. kt == nit000 )   z1_2dt_b = 1.0_wp / rdt    ! reciprocal of baroclinic 
                                                                        ! time step (euler timestep)
      z1_8     = 0.125_wp                                   ! coefficient for vorticity estimates
      z1_4     = 0.25_wp        
      zraur    = 1._wp / rau0                               ! 1 / volumic mass
      !
      zhdiv(:,:) = 0._wp                                    ! barotropic divergence
      zu_sld = 0._wp   ;   zu_asp = 0._wp                   ! tides trends (lk_tide=F)
      zv_sld = 0._wp   ;   zv_asp = 0._wp

      IF( kt == nit000 .AND. neuler == 0) THEN              ! for implicit bottom friction
        z2dt_bf = rdt
      ELSE
        z2dt_bf = 2.0_wp * rdt
      ENDIF

      ! -----------------------------------------------------------------------------
      !  Phase 1 : Coupling between general trend and barotropic estimates (1st step)
      ! -----------------------------------------------------------------------------
      !      
      !                                   !* e3*d/dt(Ua), e3*Ub, e3*Vn (Vertically integrated)
      !                                   ! --------------------------
      zua(:,:) = 0._wp   ;   zun(:,:) = 0._wp   ;   ub_b(:,:) = 0._wp
      zva(:,:) = 0._wp   ;   zvn(:,:) = 0._wp   ;   vb_b(:,:) = 0._wp
      !
      DO jk = 1, jpkm1
#if defined key_vectopt_loop
         DO jj = 1, 1         !Vector opt. => forced unrolling
            DO ji = 1, jpij
#else 
         DO jj = 1, jpj
            DO ji = 1, jpi
#endif
               !                                                                              ! now trend
               zua(ji,jj) = zua(ji,jj) + fse3u  (ji,jj,jk) * ua(ji,jj,jk) * umask(ji,jj,jk)
               zva(ji,jj) = zva(ji,jj) + fse3v  (ji,jj,jk) * va(ji,jj,jk) * vmask(ji,jj,jk)
               !                                                                              ! now velocity 
               zun(ji,jj) = zun(ji,jj) + fse3u  (ji,jj,jk) * un(ji,jj,jk)
               zvn(ji,jj) = zvn(ji,jj) + fse3v  (ji,jj,jk) * vn(ji,jj,jk)               
               !
#if defined key_vvl
               ub_b(ji,jj) = ub_b(ji,jj) + fse3u_b(ji,jj,jk)* ub(ji,jj,jk)   *umask(ji,jj,jk) 
               vb_b(ji,jj) = vb_b(ji,jj) + fse3v_b(ji,jj,jk)* vb(ji,jj,jk)   *vmask(ji,jj,jk)
#else
               ub_b(ji,jj) = ub_b(ji,jj) + fse3u_0(ji,jj,jk) * ub(ji,jj,jk)  * umask(ji,jj,jk)
               vb_b(ji,jj) = vb_b(ji,jj) + fse3v_0(ji,jj,jk) * vb(ji,jj,jk)  * vmask(ji,jj,jk)
#endif
            END DO
         END DO
      END DO

      !                                   !* baroclinic momentum trend (remove the vertical mean trend)
      DO jk = 1, jpkm1                    ! --------------------------
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               ua(ji,jj,jk) = ua(ji,jj,jk) - zua(ji,jj) * hur(ji,jj)
               va(ji,jj,jk) = va(ji,jj,jk) - zva(ji,jj) * hvr(ji,jj)
            END DO
         END DO
      END DO

      !                                   !* barotropic Coriolis trends * H (vorticity scheme dependent)
      !                                   ! ---------------------------====
      zwx(:,:) = zun(:,:) * e2u(:,:)                   ! now transport 
      zwy(:,:) = zvn(:,:) * e1v(:,:)
      !
      IF( ln_dynvor_ene .OR. ln_dynvor_mix ) THEN      ! energy conserving or mixed scheme
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zy1 = ( zwy(ji,jj-1) + zwy(ji+1,jj-1) ) / e1u(ji,jj)
               zy2 = ( zwy(ji,jj  ) + zwy(ji+1,jj  ) ) / e1u(ji,jj)
               zx1 = ( zwx(ji-1,jj) + zwx(ji-1,jj+1) ) / e2v(ji,jj)
               zx2 = ( zwx(ji  ,jj) + zwx(ji  ,jj+1) ) / e2v(ji,jj)
               ! energy conserving formulation for planetary vorticity term
               zcu(ji,jj) = z1_4 * ( ff(ji  ,jj-1) * zy1 + ff(ji,jj) * zy2 )
               zcv(ji,jj) =-z1_4 * ( ff(ji-1,jj  ) * zx1 + ff(ji,jj) * zx2 )
            END DO
         END DO
         !
      ELSEIF ( ln_dynvor_ens ) THEN                    ! enstrophy conserving scheme
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zy1 =   z1_8 * ( zwy(ji  ,jj-1) + zwy(ji+1,jj-1) + zwy(ji,jj) + zwy(ji+1,jj  ) ) / e1u(ji,jj)
               zx1 = - z1_8 * ( zwx(ji-1,jj  ) + zwx(ji-1,jj+1) + zwx(ji,jj) + zwx(ji  ,jj+1) ) / e2v(ji,jj)
               zcu(ji,jj)  = zy1 * ( ff(ji  ,jj-1) + ff(ji,jj) )
               zcv(ji,jj)  = zx1 * ( ff(ji-1,jj  ) + ff(ji,jj) )
            END DO
         END DO
         !
      ELSEIF ( ln_dynvor_een ) THEN                    ! enstrophy and energy conserving scheme
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zcu(ji,jj) = + z1_4 / e1u(ji,jj) * (  ftne(ji,jj  ) * zwy(ji  ,jj  ) + ftnw(ji+1,jj) * zwy(ji+1,jj  )   &
                  &                                + ftse(ji,jj  ) * zwy(ji  ,jj-1) + ftsw(ji+1,jj) * zwy(ji+1,jj-1) )
               zcv(ji,jj) = - z1_4 / e2v(ji,jj) * (  ftsw(ji,jj+1) * zwx(ji-1,jj+1) + ftse(ji,jj+1) * zwx(ji  ,jj+1)   &
                  &                                + ftnw(ji,jj  ) * zwx(ji-1,jj  ) + ftne(ji,jj  ) * zwx(ji  ,jj  ) )
            END DO
         END DO
         !
      ENDIF

      !                                   !* Right-Hand-Side of the barotropic momentum equation
      !                                   ! ----------------------------------------------------
      IF( lk_vvl ) THEN                         ! Variable volume : remove both Coriolis and Surface pressure gradient
         DO jj = 2, jpjm1 
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zcu(ji,jj) = zcu(ji,jj) - grav * (  ( rhd(ji+1,jj  ,1) + 1 ) * sshn(ji+1,jj  )    &
                  &                              - ( rhd(ji  ,jj  ,1) + 1 ) * sshn(ji  ,jj  )  ) * hu(ji,jj) / e1u(ji,jj)
               zcv(ji,jj) = zcv(ji,jj) - grav * (  ( rhd(ji  ,jj+1,1) + 1 ) * sshn(ji  ,jj+1)    &
                  &                              - ( rhd(ji  ,jj  ,1) + 1 ) * sshn(ji  ,jj  )  ) * hv(ji,jj) / e2v(ji,jj)
            END DO
         END DO
      ENDIF

      DO jj = 2, jpjm1                             ! Remove coriolis term (and possibly spg) from barotropic trend
         DO ji = fs_2, fs_jpim1
             zua(ji,jj) = zua(ji,jj) - zcu(ji,jj)
             zva(ji,jj) = zva(ji,jj) - zcv(ji,jj)
          END DO
      END DO

                    
      !                                             ! Remove barotropic contribution of bottom friction 
      !                                             ! from the barotropic transport trend
      zcoef = -1._wp * z1_2dt_b

      IF(ln_bfrimp) THEN
      !                                   ! Remove the bottom stress trend from 3-D sea surface level gradient
      !                                   ! and Coriolis forcing in case of 3D semi-implicit bottom friction 
        DO jj = 2, jpjm1         
           DO ji = fs_2, fs_jpim1
              ikbu = mbku(ji,jj)
              ikbv = mbkv(ji,jj)
              ua_btm = zcu(ji,jj) * z2dt_bf * hur(ji,jj) * umask (ji,jj,ikbu)
              va_btm = zcv(ji,jj) * z2dt_bf * hvr(ji,jj) * vmask (ji,jj,ikbv)

              zua(ji,jj) = zua(ji,jj) - bfrua(ji,jj) * ua_btm
              zva(ji,jj) = zva(ji,jj) - bfrva(ji,jj) * va_btm
           END DO
        END DO

      ELSE

# if defined key_vectopt_loop
        DO jj = 1, 1
           DO ji = 1, jpij-jpi   ! vector opt. (forced unrolling)
# else
        DO jj = 2, jpjm1
           DO ji = 2, jpim1
# endif
            ! Apply stability criteria for bottom friction
            !RBbug for vvl and external mode we may need to use varying fse3
            !!gm  Rq: the bottom e3 present the smallest variation, the use of e3u_0 is not a big approx.
              zbfru(ji,jj) = MAX(  bfrua(ji,jj) , fse3u(ji,jj,mbku(ji,jj)) * zcoef  )
              zbfrv(ji,jj) = MAX(  bfrva(ji,jj) , fse3v(ji,jj,mbkv(ji,jj)) * zcoef  )
           END DO
        END DO

        IF( lk_vvl ) THEN
           DO jj = 2, jpjm1
              DO ji = fs_2, fs_jpim1   ! vector opt.
                 zua(ji,jj) = zua(ji,jj) - zbfru(ji,jj) * ub_b(ji,jj)   &
                    &       / ( hu_0(ji,jj) + sshu_b(ji,jj) + 1._wp - umask(ji,jj,1) )
                 zva(ji,jj) = zva(ji,jj) - zbfrv(ji,jj) * vb_b(ji,jj)   &
                    &       / ( hv_0(ji,jj) + sshv_b(ji,jj) + 1._wp - vmask(ji,jj,1) )
              END DO
           END DO
        ELSE
           DO jj = 2, jpjm1
              DO ji = fs_2, fs_jpim1   ! vector opt.
                 zua(ji,jj) = zua(ji,jj) - zbfru(ji,jj) * ub_b(ji,jj) * hur(ji,jj)
                 zva(ji,jj) = zva(ji,jj) - zbfrv(ji,jj) * vb_b(ji,jj) * hvr(ji,jj)
              END DO
           END DO
        ENDIF
      END IF    ! end (ln_bfrimp)

                    
      !                                   !* d/dt(Ua), Ub, Vn (Vertical mean velocity)
      !                                   ! -------------------------- 
      zua(:,:) = zua(:,:) * hur(:,:)
      zva(:,:) = zva(:,:) * hvr(:,:)
      !
      IF( lk_vvl ) THEN
         ub_b(:,:) = ub_b(:,:) * umask(:,:,1) / ( hu_0(:,:) + sshu_b(:,:) + 1._wp - umask(:,:,1) )
         vb_b(:,:) = vb_b(:,:) * vmask(:,:,1) / ( hv_0(:,:) + sshv_b(:,:) + 1._wp - vmask(:,:,1) )
      ELSE
         ub_b(:,:) = ub_b(:,:) * hur(:,:)
         vb_b(:,:) = vb_b(:,:) * hvr(:,:)
      ENDIF

      ! -----------------------------------------------------------------------
      !  Phase 2 : Integration of the barotropic equations with time splitting
      ! -----------------------------------------------------------------------
      !
      !                                             ! ==================== !
      !                                             !    Initialisations   !
      !                                             ! ==================== !
      icycle = 2  * nn_baro            ! Number of barotropic sub time-step
      
      !                                ! Start from NOW field
      hu_e   (:,:) = hu   (:,:)            ! ocean depth at u- and v-points
      hv_e   (:,:) = hv   (:,:) 
      hur_e  (:,:) = hur  (:,:)            ! ocean depth inverted at u- and v-points
      hvr_e  (:,:) = hvr  (:,:)
!RBbug     zsshb_e(:,:) = sshn (:,:)  
      zsshb_e(:,:) = sshn_b(:,:)           ! sea surface height (before and now)
      sshn_e (:,:) = sshn (:,:)
      
      zun_e  (:,:) = un_b (:,:)            ! barotropic velocity (external)
      zvn_e  (:,:) = vn_b (:,:)
      zub_e  (:,:) = un_b (:,:)
      zvb_e  (:,:) = vn_b (:,:)

      zu_sum  (:,:) = un_b (:,:)           ! summation
      zv_sum  (:,:) = vn_b (:,:)
      zssh_sum(:,:) = sshn (:,:)

#if defined key_obc
      ! set ssh corrections to 0
      ! ssh corrections are applied to normal velocities (Flather's algorithm) and averaged over the barotropic loop
      IF( lp_obc_east  )   sshfoe_b(:,:) = 0._wp
      IF( lp_obc_west  )   sshfow_b(:,:) = 0._wp
      IF( lp_obc_south )   sshfos_b(:,:) = 0._wp
      IF( lp_obc_north )   sshfon_b(:,:) = 0._wp
#endif

      !                                             ! ==================== !
      DO jn = 1, icycle                             !  sub-time-step loop  ! (from NOW to AFTER+1)
         !                                          ! ==================== !
         z2dt_e = 2. * ( rdt / nn_baro )
         IF( jn == 1 )   z2dt_e = rdt / nn_baro

         !                                                !* Update the forcing (BDY and tides)
         !                                                !  ------------------
         IF( lk_obc )   CALL obc_dta_bt ( kt, jn   )
         IF( lk_bdy )   CALL bdy_dta ( kt, jit=jn, time_offset=+1 )
         IF ( ln_tide_pot ) CALL upd_tide( kt, jn )

         !                                                !* after ssh_e
         !                                                !  -----------
         DO jj = 2, jpjm1                                 ! Horizontal divergence of barotropic transports
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zhdiv(ji,jj) = (   e2u(ji  ,jj) * zun_e(ji  ,jj) * hu_e(ji  ,jj)     &
                  &             - e2u(ji-1,jj) * zun_e(ji-1,jj) * hu_e(ji-1,jj)     &
                  &             + e1v(ji,jj  ) * zvn_e(ji,jj  ) * hv_e(ji,jj  )     &
                  &             - e1v(ji,jj-1) * zvn_e(ji,jj-1) * hv_e(ji,jj-1)   ) / ( e1t(ji,jj) * e2t(ji,jj) )
            END DO
         END DO
         !
#if defined key_obc
         !                                                     ! OBC : zhdiv must be zero behind the open boundary
!!  mpp remark: The zeroing of hdiv can probably be extended to 1->jpi/jpj for the correct row/column
         IF( lp_obc_east  )   zhdiv(nie0p1:nie1p1,nje0  :nje1  ) = 0._wp      ! east
         IF( lp_obc_west  )   zhdiv(niw0  :niw1  ,njw0  :njw1  ) = 0._wp      ! west
         IF( lp_obc_north )   zhdiv(nin0  :nin1  ,njn0p1:njn1p1) = 0._wp      ! north
         IF( lp_obc_south )   zhdiv(nis0  :nis1  ,njs0  :njs1  ) = 0._wp      ! south
#endif
#if defined key_bdy
         zhdiv(:,:) = zhdiv(:,:) * bdytmask(:,:)               ! BDY mask
#endif
         !
         DO jj = 2, jpjm1                                      ! leap-frog on ssh_e
            DO ji = fs_2, fs_jpim1   ! vector opt.
               ssha_e(ji,jj) = ( zsshb_e(ji,jj) - z2dt_e * ( zraur * ( emp(ji,jj)-rnf(ji,jj) ) + zhdiv(ji,jj) ) ) * tmask(ji,jj,1) 
            END DO
         END DO

         !                                                !* after barotropic velocities (vorticity scheme dependent)
         !                                                !  ---------------------------  
         zwx(:,:) = e2u(:,:) * zun_e(:,:) * hu_e(:,:)     ! now_e transport
         zwy(:,:) = e1v(:,:) * zvn_e(:,:) * hv_e(:,:)
         !
         IF( ln_dynvor_ene .OR. ln_dynvor_mix ) THEN      !==  energy conserving or mixed scheme  ==!
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  ! surface pressure gradient
                  IF( lk_vvl) THEN
                     zu_spg = -grav * (  ( rhd(ji+1,jj ,1) + 1 ) * sshn_e(ji+1,jj  )    &
                        &              - ( rhd(ji  ,jj ,1) + 1 ) * sshn_e(ji  ,jj  )  ) / e1u(ji,jj)
                     zv_spg = -grav * (  ( rhd(ji ,jj+1,1) + 1 ) * sshn_e(ji  ,jj+1)    &
                        &              - ( rhd(ji ,jj  ,1) + 1 ) * sshn_e(ji  ,jj  )  ) / e2v(ji,jj)
                  ELSE
                     zu_spg = -grav * ( sshn_e(ji+1,jj) - sshn_e(ji,jj) ) / e1u(ji,jj)
                     zv_spg = -grav * ( sshn_e(ji,jj+1) - sshn_e(ji,jj) ) / e2v(ji,jj)
                  ENDIF
                  ! add tidal astronomical forcing
                  IF ( ln_tide_pot ) THEN 
                  zu_spg = zu_spg + grav * ( pot_astro(ji+1,jj) - pot_astro(ji,jj) ) / e1u(ji,jj)
                  zv_spg = zv_spg + grav * ( pot_astro(ji,jj+1) - pot_astro(ji,jj) ) / e2v(ji,jj)
                  ENDIF
                  ! energy conserving formulation for planetary vorticity term
                  zy1 = ( zwy(ji  ,jj-1) + zwy(ji+1,jj-1) ) / e1u(ji,jj)
                  zy2 = ( zwy(ji  ,jj  ) + zwy(ji+1,jj  ) ) / e1u(ji,jj)
                  zx1 = ( zwx(ji-1,jj  ) + zwx(ji-1,jj+1) ) / e2v(ji,jj)
                  zx2 = ( zwx(ji  ,jj  ) + zwx(ji  ,jj+1) ) / e2v(ji,jj)
                  zu_cor = z1_4 * ( ff(ji  ,jj-1) * zy1 + ff(ji,jj) * zy2 ) * hur_e(ji,jj)
                  zv_cor =-z1_4 * ( ff(ji-1,jj  ) * zx1 + ff(ji,jj) * zx2 ) * hvr_e(ji,jj)
                  ! after velocities with implicit bottom friction

                  IF( ln_bfrimp ) THEN      ! implicit bottom friction
                     !   A new method to implement the implicit bottom friction. 
                     !   H. Liu
                     !   Sept 2011
                     ua_e(ji,jj) = umask(ji,jj,1) * ( zub_e(ji,jj) +                                            &
                      &                               z2dt_e * ( zu_cor + zu_spg + zu_sld + zu_asp )            &
                      &                               / ( 1._wp      - z2dt_e * bfrua(ji,jj) * hur_e(ji,jj) ) )
                     ua_e(ji,jj) = ( ua_e(ji,jj) + z2dt_e *   zua(ji,jj)  ) * umask(ji,jj,1)   
                     !
                     va_e(ji,jj) = vmask(ji,jj,1) * ( zvb_e(ji,jj) +                                            &
                      &                               z2dt_e * ( zv_cor + zv_spg + zv_sld + zv_asp )            &
                      &                               / ( 1._wp      - z2dt_e * bfrva(ji,jj) * hvr_e(ji,jj) ) )
                     va_e(ji,jj) = ( va_e(ji,jj) + z2dt_e *   zva(ji,jj)  ) * vmask(ji,jj,1)   
                     !
                  ELSE
                     ua_e(ji,jj) = ( zub_e(ji,jj) + z2dt_e * ( zu_cor + zu_spg + zu_sld + zu_asp + zua(ji,jj))) * umask(ji,jj,1)   &
                      &           / ( 1._wp         - z2dt_e * bfrua(ji,jj) * hur_e(ji,jj) )
                     va_e(ji,jj) = ( zvb_e(ji,jj) + z2dt_e * ( zv_cor + zv_spg + zv_sld + zv_asp + zva(ji,jj))) * vmask(ji,jj,1)   &
                      &           / ( 1._wp         - z2dt_e * bfrva(ji,jj) * hvr_e(ji,jj) )
                  ENDIF
               END DO
            END DO
            !
         ELSEIF ( ln_dynvor_ens ) THEN                    !==  enstrophy conserving scheme  ==!
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                   ! surface pressure gradient
                  IF( lk_vvl) THEN
                     zu_spg = -grav * (  ( rhd(ji+1,jj ,1) + 1 ) * sshn_e(ji+1,jj  )    &
                        &              - ( rhd(ji  ,jj ,1) + 1 ) * sshn_e(ji  ,jj  )  ) / e1u(ji,jj)
                     zv_spg = -grav * (  ( rhd(ji ,jj+1,1) + 1 ) * sshn_e(ji  ,jj+1)    &
                        &              - ( rhd(ji ,jj  ,1) + 1 ) * sshn_e(ji  ,jj  )  ) / e2v(ji,jj)
                  ELSE
                     zu_spg = -grav * ( sshn_e(ji+1,jj) - sshn_e(ji,jj) ) / e1u(ji,jj)
                     zv_spg = -grav * ( sshn_e(ji,jj+1) - sshn_e(ji,jj) ) / e2v(ji,jj)
                  ENDIF
                  ! add tidal astronomical forcing
                  IF ( ln_tide_pot ) THEN
                  zu_spg = zu_spg + grav * ( pot_astro(ji+1,jj) - pot_astro(ji,jj) ) / e1u(ji,jj)
                  zv_spg = zv_spg + grav * ( pot_astro(ji,jj+1) - pot_astro(ji,jj) ) / e2v(ji,jj)
                  ENDIF
                  ! enstrophy conserving formulation for planetary vorticity term
                  zy1 =   z1_8 * ( zwy(ji  ,jj-1) + zwy(ji+1,jj-1) + zwy(ji,jj) + zwy(ji+1,jj  ) ) / e1u(ji,jj)
                  zx1 = - z1_8 * ( zwx(ji-1,jj  ) + zwx(ji-1,jj+1) + zwx(ji,jj) + zwx(ji  ,jj+1) ) / e2v(ji,jj)
                  zu_cor  = zy1 * ( ff(ji  ,jj-1) + ff(ji,jj) ) * hur_e(ji,jj)
                  zv_cor  = zx1 * ( ff(ji-1,jj  ) + ff(ji,jj) ) * hvr_e(ji,jj)
                  ! after velocities with implicit bottom friction
                  IF( ln_bfrimp ) THEN
                     !   A new method to implement the implicit bottom friction. 
                     !   H. Liu
                     !   Sept 2011
                     ua_e(ji,jj) = umask(ji,jj,1) * ( zub_e(ji,jj) +                                            &
                      &                               z2dt_e * ( zu_cor + zu_spg + zu_sld + zu_asp )            &
                      &                               / ( 1._wp      - z2dt_e * bfrua(ji,jj) * hur_e(ji,jj) ) )
                     ua_e(ji,jj) = ( ua_e(ji,jj) + z2dt_e *   zua(ji,jj)  ) * umask(ji,jj,1)   
                     !
                     va_e(ji,jj) = vmask(ji,jj,1) * ( zvb_e(ji,jj) +                                            &
                      &                               z2dt_e * ( zv_cor + zv_spg + zv_sld + zv_asp )            &
                      &                               / ( 1._wp      - z2dt_e * bfrva(ji,jj) * hvr_e(ji,jj) ) )
                     va_e(ji,jj) = ( va_e(ji,jj) + z2dt_e *   zva(ji,jj)  ) * vmask(ji,jj,1)   
                     !
                  ELSE
                     ua_e(ji,jj) = ( zub_e(ji,jj) + z2dt_e * ( zu_cor + zu_spg + zu_sld + zu_asp + zua(ji,jj))) * umask(ji,jj,1)   &
                     &            / ( 1._wp        - z2dt_e * bfrua(ji,jj) * hur_e(ji,jj) )
                     va_e(ji,jj) = ( zvb_e(ji,jj) + z2dt_e * ( zv_cor + zv_spg + zv_sld + zv_asp + zva(ji,jj))) * vmask(ji,jj,1)   &
                     &            / ( 1._wp        - z2dt_e * bfrva(ji,jj) * hvr_e(ji,jj) )
                  ENDIF
               END DO
            END DO
            !
         ELSEIF ( ln_dynvor_een ) THEN                    !==  energy and enstrophy conserving scheme  ==!
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  ! surface pressure gradient
                  IF( lk_vvl) THEN
                     zu_spg = -grav * (  ( rhd(ji+1,jj  ,1) + 1 ) * sshn_e(ji+1,jj  )    &
                        &              - ( rhd(ji  ,jj  ,1) + 1 ) * sshn_e(ji  ,jj  )  ) / e1u(ji,jj)
                     zv_spg = -grav * (  ( rhd(ji  ,jj+1,1) + 1 ) * sshn_e(ji  ,jj+1)    &
                        &              - ( rhd(ji  ,jj  ,1) + 1 ) * sshn_e(ji  ,jj  )  ) / e2v(ji,jj)
                  ELSE
                     zu_spg = -grav * ( sshn_e(ji+1,jj) - sshn_e(ji,jj) ) / e1u(ji,jj)
                     zv_spg = -grav * ( sshn_e(ji,jj+1) - sshn_e(ji,jj) ) / e2v(ji,jj)
                  ENDIF
                  ! add tidal astronomical forcing
                  IF ( ln_tide_pot ) THEN
                  zu_spg = zu_spg + grav * ( pot_astro(ji+1,jj) - pot_astro(ji,jj) ) / e1u(ji,jj)
                  zv_spg = zv_spg + grav * ( pot_astro(ji,jj+1) - pot_astro(ji,jj) ) / e2v(ji,jj)
                  ENDIF
                  ! energy/enstrophy conserving formulation for planetary vorticity term
                  zu_cor = + z1_4 / e1u(ji,jj) * (  ftne(ji,jj  ) * zwy(ji  ,jj  ) + ftnw(ji+1,jj) * zwy(ji+1,jj  )   &
                     &                           + ftse(ji,jj  ) * zwy(ji  ,jj-1) + ftsw(ji+1,jj) * zwy(ji+1,jj-1) ) * hur_e(ji,jj)
                  zv_cor = - z1_4 / e2v(ji,jj) * (  ftsw(ji,jj+1) * zwx(ji-1,jj+1) + ftse(ji,jj+1) * zwx(ji  ,jj+1)   &
                     &                           + ftnw(ji,jj  ) * zwx(ji-1,jj  ) + ftne(ji,jj  ) * zwx(ji  ,jj  ) ) * hvr_e(ji,jj)
                  ! after velocities with implicit bottom friction
                  IF( ln_bfrimp ) THEN
                     !   A new method to implement the implicit bottom friction. 
                     !   H. Liu
                     !   Sept 2011
                     ua_e(ji,jj) = umask(ji,jj,1) * ( zub_e(ji,jj) +                                            &
                      &                               z2dt_e * ( zu_cor + zu_spg + zu_sld + zu_asp )            &
                      &                               / ( 1._wp      - z2dt_e * bfrua(ji,jj) * hur_e(ji,jj) ) )
                     ua_e(ji,jj) = ( ua_e(ji,jj) + z2dt_e *   zua(ji,jj)  ) * umask(ji,jj,1)   
                     !
                     va_e(ji,jj) = vmask(ji,jj,1) * ( zvb_e(ji,jj) +                                            &
                      &                               z2dt_e * ( zv_cor + zv_spg + zv_sld + zv_asp )            &
                      &                               / ( 1._wp      - z2dt_e * bfrva(ji,jj) * hvr_e(ji,jj) ) )
                     va_e(ji,jj) = ( va_e(ji,jj) + z2dt_e *   zva(ji,jj)  ) * vmask(ji,jj,1)   
                     !
                  ELSE
                     ua_e(ji,jj) = ( zub_e(ji,jj) + z2dt_e * ( zu_cor + zu_spg + zu_sld + zu_asp + zua(ji,jj))) * umask(ji,jj,1)   &
                     &            / ( 1._wp        - z2dt_e * bfrua(ji,jj) * hur_e(ji,jj) )
                     va_e(ji,jj) = ( zvb_e(ji,jj) + z2dt_e * ( zv_cor + zv_spg + zv_sld + zv_asp + zva(ji,jj))) * vmask(ji,jj,1)   &
                     &            / ( 1._wp        - z2dt_e * bfrva(ji,jj) * hvr_e(ji,jj) )
                  ENDIF
               END DO
            END DO
            ! 
         ENDIF
         !                                                     !* domain lateral boundary
         !                                                     !  -----------------------

                                                               ! OBC open boundaries
         IF( lk_obc               )   CALL obc_fla_ts ( ua_e, va_e, sshn_e, ssha_e )

                                                               ! BDY open boundaries
#if defined key_bdy
         pssh => sshn_e
         phur => hur_e
         phvr => hvr_e
         pu2d => ua_e
         pv2d => va_e

         IF( lk_bdy )   CALL bdy_dyn2d( kt ) 
#endif

         !
         CALL lbc_lnk( ua_e  , 'U', -1. )                      ! local domain boundaries 
         CALL lbc_lnk( va_e  , 'V', -1. )
         CALL lbc_lnk( ssha_e, 'T',  1. )

         zu_sum  (:,:) = zu_sum  (:,:) + ua_e  (:,:)           ! Sum over sub-time-steps
         zv_sum  (:,:) = zv_sum  (:,:) + va_e  (:,:) 
         zssh_sum(:,:) = zssh_sum(:,:) + ssha_e(:,:) 

         !                                                !* Time filter and swap
         !                                                !  --------------------
         IF( jn == 1 ) THEN                                     ! Swap only (1st Euler time step)
            zsshb_e(:,:) = sshn_e(:,:)
            zub_e  (:,:) = zun_e (:,:)
            zvb_e  (:,:) = zvn_e (:,:)
            sshn_e (:,:) = ssha_e(:,:)
            zun_e  (:,:) = ua_e  (:,:)
            zvn_e  (:,:) = va_e  (:,:)
         ELSE                                                   ! Swap + Filter
            zsshb_e(:,:) = atfp * ( zsshb_e(:,:) + ssha_e(:,:) ) + atfp1 * sshn_e(:,:)
            zub_e  (:,:) = atfp * ( zub_e  (:,:) + ua_e  (:,:) ) + atfp1 * zun_e (:,:)
            zvb_e  (:,:) = atfp * ( zvb_e  (:,:) + va_e  (:,:) ) + atfp1 * zvn_e (:,:)
            sshn_e (:,:) = ssha_e(:,:)
            zun_e  (:,:) = ua_e  (:,:)
            zvn_e  (:,:) = va_e  (:,:)
         ENDIF

         IF( lk_vvl ) THEN                                !* Update ocean depth (variable volume case only)
            !                                             !  ------------------
            DO jj = 1, jpjm1                                    ! Sea Surface Height at u- & v-points
               DO ji = 1, fs_jpim1   ! Vector opt.
                  zsshun_e(ji,jj) = 0.5_wp * umask(ji,jj,1) / ( e1u(ji,jj) * e2u(ji,jj) )       &
                     &                     * ( e1t(ji  ,jj) * e2t(ji  ,jj) * sshn_e(ji  ,jj)    &
                     &                     +   e1t(ji+1,jj) * e2t(ji+1,jj) * sshn_e(ji+1,jj) )
                  zsshvn_e(ji,jj) = 0.5_wp * vmask(ji,jj,1) / ( e1v(ji,jj) * e2v(ji,jj) )       &
                     &                     * ( e1t(ji,jj  ) * e2t(ji,jj  ) * sshn_e(ji,jj  )    &
                     &                     +   e1t(ji,jj+1) * e2t(ji,jj+1) * sshn_e(ji,jj+1) )
               END DO
            END DO
            CALL lbc_lnk( zsshun_e, 'U', 1. )                   ! lateral boundaries conditions
            CALL lbc_lnk( zsshvn_e, 'V', 1. ) 
            !
            hu_e (:,:) = hu_0(:,:) + zsshun_e(:,:)              ! Ocean depth at U- and V-points
            hv_e (:,:) = hv_0(:,:) + zsshvn_e(:,:)
            hur_e(:,:) = umask(:,:,1) / ( hu_e(:,:) + 1._wp - umask(:,:,1) )
            hvr_e(:,:) = vmask(:,:,1) / ( hv_e(:,:) + 1._wp - vmask(:,:,1) )
            !
         ENDIF
         !                                                 ! ==================== !
      END DO                                               !        end loop      !
      !                                                    ! ==================== !

#if defined key_obc
      IF( lp_obc_east  )   sshfoe_b(:,:) = zcoef * sshfoe_b(:,:)     !!gm totally useless ?????
      IF( lp_obc_west  )   sshfow_b(:,:) = zcoef * sshfow_b(:,:)
      IF( lp_obc_north )   sshfon_b(:,:) = zcoef * sshfon_b(:,:)
      IF( lp_obc_south )   sshfos_b(:,:) = zcoef * sshfos_b(:,:)
#endif

      ! -----------------------------------------------------------------------------
      ! Phase 3. update the general trend with the barotropic trend
      ! -----------------------------------------------------------------------------
      !
      !                                   !* Time average ==> after barotropic u, v, ssh
      zcoef =  1._wp / ( 2 * nn_baro  + 1 ) 
      zu_sum(:,:) = zcoef * zu_sum  (:,:) 
      zv_sum(:,:) = zcoef * zv_sum  (:,:) 
      ! 
      !                                   !* update the general momentum trend
      DO jk=1,jpkm1
         ua(:,:,jk) = ua(:,:,jk) + ( zu_sum(:,:) - ub_b(:,:) ) * z1_2dt_b
         va(:,:,jk) = va(:,:,jk) + ( zv_sum(:,:) - vb_b(:,:) ) * z1_2dt_b
      END DO
      un_b  (:,:) =  zu_sum(:,:) 
      vn_b  (:,:) =  zv_sum(:,:) 
      sshn_b(:,:) = zcoef * zssh_sum(:,:) 
      !
      !                                   !* write time-spliting arrays in the restart
      IF( lrst_oce )   CALL ts_rst( kt, 'WRITE' )
      !
      CALL wrk_dealloc( jpi, jpj, zsshun_e, zsshvn_e, zsshb_e, zssh_sum, zhdiv     )
      CALL wrk_dealloc( jpi, jpj, zua, zva, zun, zvn, zun_e, zvn_e, zub_e, zvb_e   )
      CALL wrk_dealloc( jpi, jpj, zcu, zcv, zwx, zwy, zbfru, zbfrv, zu_sum, zv_sum )
      !
      IF( nn_timing == 1 )  CALL timing_stop('dyn_spg_ts')
      !
   END SUBROUTINE dyn_spg_ts


   SUBROUTINE ts_rst( kt, cdrw )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE ts_rst  ***
      !!
      !! ** Purpose : Read or write time-splitting arrays in restart file
      !!----------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   kt         ! ocean time-step
      CHARACTER(len=*), INTENT(in) ::   cdrw       ! "READ"/"WRITE" flag
      !
      INTEGER ::  ji, jk        ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF( TRIM(cdrw) == 'READ' ) THEN
         IF( iom_varid( numror, 'un_b', ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numror, jpdom_autoglo, 'un_b'  , un_b  (:,:) )   ! external velocity issued
            CALL iom_get( numror, jpdom_autoglo, 'vn_b'  , vn_b  (:,:) )   ! from barotropic loop
         ELSE
            un_b (:,:) = 0._wp
            vn_b (:,:) = 0._wp
            ! vertical sum
            IF( lk_vopt_loop ) THEN          ! vector opt., forced unroll
               DO jk = 1, jpkm1
                  DO ji = 1, jpij
                     un_b(ji,1) = un_b(ji,1) + fse3u(ji,1,jk) * un(ji,1,jk)
                     vn_b(ji,1) = vn_b(ji,1) + fse3v(ji,1,jk) * vn(ji,1,jk)
                  END DO
               END DO
            ELSE                             ! No  vector opt.
               DO jk = 1, jpkm1
                  un_b(:,:) = un_b(:,:) + fse3u(:,:,jk) * un(:,:,jk)
                  vn_b(:,:) = vn_b(:,:) + fse3v(:,:,jk) * vn(:,:,jk)
               END DO
            ENDIF
            un_b (:,:) = un_b(:,:) * hur(:,:)
            vn_b (:,:) = vn_b(:,:) * hvr(:,:)
         ENDIF

         ! Vertically integrated velocity (before)
         IF (neuler/=0) THEN
            ub_b (:,:) = 0._wp
            vb_b (:,:) = 0._wp

            ! vertical sum
            IF( lk_vopt_loop ) THEN          ! vector opt., forced unroll
               DO jk = 1, jpkm1
                  DO ji = 1, jpij
                     ub_b(ji,1) = ub_b(ji,1) + fse3u_b(ji,1,jk) * ub(ji,1,jk)
                     vb_b(ji,1) = vb_b(ji,1) + fse3v_b(ji,1,jk) * vb(ji,1,jk)
                  END DO
               END DO
            ELSE                             ! No  vector opt.
               DO jk = 1, jpkm1
                  ub_b(:,:) = ub_b(:,:) + fse3u_b(:,:,jk) * ub(:,:,jk)
                  vb_b(:,:) = vb_b(:,:) + fse3v_b(:,:,jk) * vb(:,:,jk)
               END DO
            ENDIF

            IF( lk_vvl ) THEN
               ub_b (:,:) = ub_b(:,:) * umask(:,:,1) / ( hu_0(:,:) + sshu_b(:,:) + 1._wp - umask(:,:,1) )
               vb_b (:,:) = vb_b(:,:) * vmask(:,:,1) / ( hv_0(:,:) + sshv_b(:,:) + 1._wp - vmask(:,:,1) )
            ELSE
               ub_b(:,:) = ub_b(:,:) * hur(:,:)
               vb_b(:,:) = vb_b(:,:) * hvr(:,:)
            ENDIF
         ELSE                                 ! neuler==0
            ub_b (:,:) = un_b (:,:)
            vb_b (:,:) = vn_b (:,:)
         ENDIF

         IF( iom_varid( numror, 'sshn_b', ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numror, jpdom_autoglo, 'sshn_b' , sshn_b (:,:) )   ! filtered ssh
         ELSE
            sshn_b(:,:) = sshb(:,:)   ! if not in restart set previous time mean to current baroclinic before value   
         ENDIF 
      ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN
         CALL iom_rstput( kt, nitrst, numrow, 'un_b'   , un_b  (:,:) )   ! external velocity and ssh
         CALL iom_rstput( kt, nitrst, numrow, 'vn_b'   , vn_b  (:,:) )   ! issued from barotropic loop
         CALL iom_rstput( kt, nitrst, numrow, 'sshn_b' , sshn_b(:,:) )   ! 
      ENDIF
      !
   END SUBROUTINE ts_rst

#else
   !!----------------------------------------------------------------------
   !!   Default case :   Empty module   No standart free surface cst volume
   !!----------------------------------------------------------------------
CONTAINS
   INTEGER FUNCTION dyn_spg_ts_alloc()    ! Dummy function
      dyn_spg_ts_alloc = 0
   END FUNCTION dyn_spg_ts_alloc
   SUBROUTINE dyn_spg_ts( kt )            ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'dyn_spg_ts: You should not have seen this print! error?', kt
   END SUBROUTINE dyn_spg_ts
   SUBROUTINE ts_rst( kt, cdrw )          ! Empty routine
      INTEGER         , INTENT(in) ::   kt         ! ocean time-step
      CHARACTER(len=*), INTENT(in) ::   cdrw       ! "READ"/"WRITE" flag
      WRITE(*,*) 'ts_rst    : You should not have seen this print! error?', kt, cdrw
   END SUBROUTINE ts_rst    
#endif
   
   !!======================================================================
END MODULE dynspg_ts
