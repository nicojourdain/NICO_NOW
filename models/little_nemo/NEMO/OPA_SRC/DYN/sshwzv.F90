MODULE sshwzv   
   !!==============================================================================
   !!                       ***  MODULE  sshwzv  ***
   !! Ocean dynamics : sea surface height and vertical velocity
   !!==============================================================================
   !! History :  3.1  !  2009-02  (G. Madec, M. Leclair)  Original code
   !!            3.3  !  2010-04  (M. Leclair, G. Madec)  modified LF-RA 
   !!             -   !  2010-05  (K. Mogensen, A. Weaver, M. Martin, D. Lea) Assimilation interface
   !!             -   !  2010-09  (D.Storkey and E.O'Dea) bug fixes for BDY module
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   ssh_wzv        : after ssh & now vertical velocity
   !!   ssh_nxt        : filter ans swap the ssh arrays
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables 
   USE sbc_oce         ! surface boundary condition: ocean
   USE domvvl          ! Variable volume
   USE divcur          ! hor. divergence and curl      (div & cur routines)
   USE iom             ! I/O library
   USE in_out_manager  ! I/O manager
   USE prtctl          ! Print control
   USE phycst
   USE lbclnk          ! ocean lateral boundary condition (or mpp link)
   USE lib_mpp         ! MPP library
   USE obc_par         ! open boundary cond. parameter
   USE obc_oce
   USE bdy_oce
   USE diaar5, ONLY:   lk_diaar5
   USE iom
   USE sbcrnf, ONLY: h_rnf, nk_rnf   ! River runoff 
#if defined key_agrif
   USE agrif_opa_update
   USE agrif_opa_interp
#endif
#if defined key_asminc   
   USE asminc          ! Assimilation increment
#endif
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ssh_wzv    ! called by step.F90
   PUBLIC   ssh_nxt    ! called by step.F90

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: sshwzv.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ssh_wzv( kt ) 
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE ssh_wzv  ***
      !!                   
      !! ** Purpose :   compute the after ssh (ssha), the now vertical velocity
      !!              and update the now vertical coordinate (lk_vvl=T).
      !!
      !! ** Method  : - Using the incompressibility hypothesis, the vertical 
      !!      velocity is computed by integrating the horizontal divergence  
      !!      from the bottom to the surface minus the scale factor evolution.
      !!        The boundary conditions are w=0 at the bottom (no flux) and.
      !!
      !! ** action  :   ssha    : after sea surface height
      !!                wn      : now vertical velocity
      !!                sshu_a, sshv_a, sshf_a  : after sea surface height (lk_vvl=T)
      !!                hu, hv, hur, hvr        : ocean depth and its inverse at u-,v-points
      !!
      !! Reference  : Leclair, M., and G. Madec, 2009, Ocean Modelling.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! time step
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zcoefu, zcoefv, zcoeff, z2dt, z1_2dt, z1_rau0   ! local scalars
      REAL(wp), POINTER, DIMENSION(:,:  ) ::  z2d, zhdiv
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  z3d
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('ssh_wzv')
      !
      CALL wrk_alloc( jpi, jpj, z2d, zhdiv ) 
      !
      IF( kt == nit000 ) THEN
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'ssh_wzv : after sea surface height and now vertical velocity '
         IF(lwp) WRITE(numout,*) '~~~~~~~ '
         !
         wn(:,:,jpk) = 0._wp                  ! bottom boundary condition: w=0 (set once for all)
         !
         IF( lk_vvl ) THEN                    ! before and now Sea SSH at u-, v-, f-points (vvl case only)
            DO jj = 1, jpjm1
               DO ji = 1, jpim1                    ! caution: use of Vector Opt. not possible
                  zcoefu = 0.5  * umask(ji,jj,1) / ( e1u(ji,jj) * e2u(ji,jj) )
                  zcoefv = 0.5  * vmask(ji,jj,1) / ( e1v(ji,jj) * e2v(ji,jj) )
                  zcoeff = 0.25 * umask(ji,jj,1) * umask(ji,jj+1,1)
                  sshu_b(ji,jj) = zcoefu * ( e1t(ji  ,jj) * e2t(ji  ,jj) * sshb(ji  ,jj)     &
                     &                     + e1t(ji+1,jj) * e2t(ji+1,jj) * sshb(ji+1,jj) )
                  sshv_b(ji,jj) = zcoefv * ( e1t(ji,jj  ) * e2t(ji,jj  ) * sshb(ji,jj  )     &
                     &                     + e1t(ji,jj+1) * e2t(ji,jj+1) * sshb(ji,jj+1) )
                  sshu_n(ji,jj) = zcoefu * ( e1t(ji  ,jj) * e2t(ji  ,jj) * sshn(ji  ,jj)     &
                     &                     + e1t(ji+1,jj) * e2t(ji+1,jj) * sshn(ji+1,jj) )
                  sshv_n(ji,jj) = zcoefv * ( e1t(ji,jj  ) * e2t(ji,jj  ) * sshn(ji,jj  )     &
                     &                     + e1t(ji,jj+1) * e2t(ji,jj+1) * sshn(ji,jj+1) )
               END DO
            END DO
            CALL lbc_lnk( sshu_b, 'U', 1. )   ;   CALL lbc_lnk( sshu_n, 'U', 1. )
            CALL lbc_lnk( sshv_b, 'V', 1. )   ;   CALL lbc_lnk( sshv_n, 'V', 1. )
            DO jj = 1, jpjm1
               DO ji = 1, jpim1      ! NO Vector Opt.
                  sshf_n(ji,jj) = 0.5  * umask(ji,jj,1) * umask(ji,jj+1,1)                   &
                       &               / ( e1f(ji,jj  ) * e2f(ji,jj  ) )                     &
                       &               * ( e1u(ji,jj  ) * e2u(ji,jj  ) * sshu_n(ji,jj  )     &
                       &                 + e1u(ji,jj+1) * e2u(ji,jj+1) * sshu_n(ji,jj+1) )
               END DO
            END DO
            CALL lbc_lnk( sshf_n, 'F', 1. )
         ENDIF
         !
      ENDIF

      !                                           !------------------------------------------!
      IF( lk_vvl ) THEN                           !  Regridding: Update Now Vertical coord.  !   (only in vvl case)
         !                                        !------------------------------------------!
         DO jk = 1, jpkm1
            fsdept(:,:,jk) = fsdept_n(:,:,jk)         ! now local depths stored in fsdep. arrays
            fsdepw(:,:,jk) = fsdepw_n(:,:,jk)
            fsde3w(:,:,jk) = fsde3w_n(:,:,jk)
            !
            fse3t (:,:,jk) = fse3t_n (:,:,jk)         ! vertical scale factors stored in fse3. arrays
            fse3u (:,:,jk) = fse3u_n (:,:,jk)
            fse3v (:,:,jk) = fse3v_n (:,:,jk)
            fse3f (:,:,jk) = fse3f_n (:,:,jk)
            fse3w (:,:,jk) = fse3w_n (:,:,jk)
            fse3uw(:,:,jk) = fse3uw_n(:,:,jk)
            fse3vw(:,:,jk) = fse3vw_n(:,:,jk)
         END DO
         !
         hu(:,:) = hu_0(:,:) + sshu_n(:,:)            ! now ocean depth (at u- and v-points)
         hv(:,:) = hv_0(:,:) + sshv_n(:,:)
         !                                            ! now masked inverse of the ocean depth (at u- and v-points)
         hur(:,:) = umask(:,:,1) / ( hu(:,:) + 1._wp - umask(:,:,1) )
         hvr(:,:) = vmask(:,:,1) / ( hv(:,:) + 1._wp - vmask(:,:,1) )
         ! 
      ENDIF
      !
      CALL div_cur( kt )                              ! Horizontal divergence & Relative vorticity
      !
      z2dt = 2._wp * rdt                              ! set time step size (Euler/Leapfrog)
      IF( neuler == 0 .AND. kt == nit000 )   z2dt = rdt

      !                                           !------------------------------!
      !                                           !   After Sea Surface Height   !
      !                                           !------------------------------!
      zhdiv(:,:) = 0._wp
      DO jk = 1, jpkm1                                 ! Horizontal divergence of barotropic transports
        zhdiv(:,:) = zhdiv(:,:) + fse3t(:,:,jk) * hdivn(:,:,jk)
      END DO
      !                                                ! Sea surface elevation time stepping
      ! In forward Euler time stepping case, the same formulation as in the leap-frog case can be used
      ! because emp_b field is initialized with the vlaues of emp field. Hence, 0.5 * ( emp + emp_b ) = emp
      z1_rau0 = 0.5 / rau0
      ssha(:,:) = (  sshb(:,:) - z2dt * ( z1_rau0 * ( emp_b(:,:) + emp(:,:) ) + zhdiv(:,:) )  ) * tmask(:,:,1)

#if defined key_agrif
      CALL agrif_ssh( kt )
#endif
#if defined key_obc
      IF( Agrif_Root() ) THEN 
         ssha(:,:) = ssha(:,:) * obctmsk(:,:)
         CALL lbc_lnk( ssha, 'T', 1. )                 ! absolutly compulsory !! (jmm)
      ENDIF
#endif
#if defined key_bdy
      ssha(:,:) = ssha(:,:) * bdytmask(:,:)
      CALL lbc_lnk( ssha, 'T', 1. )                 ! absolutly compulsory !! (jmm)
#endif

      !                                                ! Sea Surface Height at u-,v- and f-points (vvl case only)
      IF( lk_vvl ) THEN                                ! (required only in key_vvl case)
         DO jj = 1, jpjm1
            DO ji = 1, jpim1      ! NO Vector Opt.
               sshu_a(ji,jj) = 0.5  * umask(ji,jj,1) / ( e1u(ji  ,jj) * e2u(ji  ,jj) )                   &
                  &                                  * ( e1t(ji  ,jj) * e2t(ji  ,jj) * ssha(ji  ,jj)     &
                  &                                    + e1t(ji+1,jj) * e2t(ji+1,jj) * ssha(ji+1,jj) )
               sshv_a(ji,jj) = 0.5  * vmask(ji,jj,1) / ( e1v(ji,jj  ) * e2v(ji,jj  ) )                   &
                  &                                  * ( e1t(ji,jj  ) * e2t(ji,jj  ) * ssha(ji,jj  )     &
                  &                                    + e1t(ji,jj+1) * e2t(ji,jj+1) * ssha(ji,jj+1) )
            END DO
         END DO
         CALL lbc_lnk( sshu_a, 'U', 1. )   ;   CALL lbc_lnk( sshv_a, 'V', 1. )      ! Boundaries conditions
      ENDIF
      
#if defined key_asminc
      !                                                ! Include the IAU weighted SSH increment
      IF( lk_asminc .AND. ln_sshinc .AND. ln_asmiau ) THEN
         CALL ssh_asm_inc( kt )
         ssha(:,:) = ssha(:,:) + z2dt * ssh_iau(:,:)
      ENDIF
#endif

      !                                           !------------------------------!
      !                                           !     Now Vertical Velocity    !
      !                                           !------------------------------!
      z1_2dt = 1.e0 / z2dt
      DO jk = jpkm1, 1, -1                             ! integrate from the bottom the hor. divergence
         ! - ML - need 3 lines here because replacement of fse3t by its expression yields too long lines otherwise
         wn(:,:,jk) = wn(:,:,jk+1) -   fse3t_n(:,:,jk) * hdivn(:,:,jk)        &
            &                      - ( fse3t_a(:,:,jk) - fse3t_b(:,:,jk) )    &
            &                         * tmask(:,:,jk) * z1_2dt
#if defined key_bdy
         wn(:,:,jk) = wn(:,:,jk) * bdytmask(:,:)
#endif
      END DO

      !                                           !------------------------------!
      !                                           !           outputs            !
      !                                           !------------------------------!
      CALL iom_put( "woce", wn                    )   ! vertical velocity
      CALL iom_put( "ssh" , sshn                  )   ! sea surface height
      CALL iom_put( "ssh2", sshn(:,:) * sshn(:,:) )   ! square of sea surface height
      IF( lk_diaar5 ) THEN                            ! vertical mass transport & its square value
         ! Caution: in the VVL case, it only correponds to the baroclinic mass transport.
         CALL wrk_alloc( jpi,jpj,jpk, z3d )
         z2d(:,:) = rau0 * e1t(:,:) * e2t(:,:)
         DO jk = 1, jpk
            z3d(:,:,jk) = wn(:,:,jk) * z2d(:,:)
         END DO
         CALL iom_put( "w_masstr" , z3d                     )  
         CALL iom_put( "w_masstr2", z3d(:,:,:) * z3d(:,:,:) )
         CALL wrk_dealloc( jpi,jpj,jpk, z3d )
      ENDIF
      !
      IF(ln_ctl)   CALL prt_ctl( tab2d_1=ssha, clinfo1=' ssha  - : ', mask1=tmask, ovlap=1 )
      !
      CALL wrk_dealloc( jpi, jpj, z2d, zhdiv ) 
      !
      IF( nn_timing == 1 )  CALL timing_stop('ssh_wzv')
      !
   END SUBROUTINE ssh_wzv


   SUBROUTINE ssh_nxt( kt )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ssh_nxt  ***
      !!
      !! ** Purpose :   achieve the sea surface  height time stepping by 
      !!              applying Asselin time filter and swapping the arrays
      !!              ssha  already computed in ssh_wzv  
      !!
      !! ** Method  : - apply Asselin time fiter to now ssh (excluding the forcing
      !!              from the filter, see Leclair and Madec 2010) and swap :
      !!                sshn = ssha + atfp * ( sshb -2 sshn + ssha )
      !!                            - atfp * rdt * ( emp_b - emp ) / rau0
      !!                sshn = ssha
      !!
      !! ** action  : - sshb, sshn   : before & now sea surface height
      !!                               ready for the next time step
      !!
      !! Reference  : Leclair, M., and G. Madec, 2009, Ocean Modelling.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !!
      INTEGER  ::   ji, jj   ! dummy loop indices
      REAL(wp) ::   zec      ! temporary scalar
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('ssh_nxt')
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'ssh_nxt : next sea surface height (Asselin time filter + swap)'
         IF(lwp) WRITE(numout,*) '~~~~~~~ '
      ENDIF

      !                       !--------------------------!
      IF( lk_vvl ) THEN       !  Variable volume levels  !     (ssh at t-, u-, v, f-points)
         !                    !--------------------------!
         !
         IF( neuler == 0 .AND. kt == nit000 ) THEN    !** Euler time-stepping at first time-step : no filter
            sshn  (:,:) = ssha  (:,:)                       ! now <-- after  (before already = now)
            sshu_n(:,:) = sshu_a(:,:)
            sshv_n(:,:) = sshv_a(:,:)
            DO jj = 1, jpjm1                                ! ssh now at f-point
               DO ji = 1, jpim1      ! NO Vector Opt.
                  sshf_n(ji,jj) = 0.5  * umask(ji,jj,1) * umask(ji,jj+1,1)                 &
                     &               / ( e1f(ji,jj  ) * e2f(ji,jj  ) )                     &
                     &               * ( e1u(ji,jj  ) * e2u(ji,jj  ) * sshu_n(ji,jj  )     &
                     &                 + e1u(ji,jj+1) * e2u(ji,jj+1) * sshu_n(ji,jj+1) )
               END DO
            END DO
            CALL lbc_lnk( sshf_n, 'F', 1. )                 ! Boundaries conditions
            !
         ELSE                                         !** Leap-Frog time-stepping: Asselin filter + swap
            zec = atfp * rdt / rau0
            DO jj = 1, jpj
               DO ji = 1, jpi                               ! before <-- now filtered
                  sshb  (ji,jj) = sshn  (ji,jj) + atfp * ( sshb(ji,jj) - 2 * sshn(ji,jj) + ssha(ji,jj) )   &
                     &                          - zec  * ( emp_b(ji,jj) - emp(ji,jj) ) * tmask(ji,jj,1)
                  sshn  (ji,jj) = ssha  (ji,jj)             ! now <-- after
                  sshu_n(ji,jj) = sshu_a(ji,jj)
                  sshv_n(ji,jj) = sshv_a(ji,jj)
               END DO
            END DO
            DO jj = 1, jpjm1                                ! ssh now at f-point
               DO ji = 1, jpim1      ! NO Vector Opt.
                  sshf_n(ji,jj) = 0.5  * umask(ji,jj,1) * umask(ji,jj+1,1)                 &
                     &               / ( e1f(ji,jj  ) * e2f(ji,jj  ) )                     &
                     &               * ( e1u(ji,jj  ) * e2u(ji,jj  ) * sshu_n(ji,jj  )     &
                     &                 + e1u(ji,jj+1) * e2u(ji,jj+1) * sshu_n(ji,jj+1) )
               END DO
            END DO
            CALL lbc_lnk( sshf_n, 'F', 1. )                 ! Boundaries conditions
            !
            DO jj = 1, jpjm1                                ! ssh before at u- & v-points
               DO ji = 1, jpim1      ! NO Vector Opt.
                  sshu_b(ji,jj) = 0.5  * umask(ji,jj,1) / ( e1u(ji  ,jj) * e2u(ji  ,jj) )                   &
                     &                                  * ( e1t(ji  ,jj) * e2t(ji  ,jj) * sshb(ji  ,jj)     &
                     &                                    + e1t(ji+1,jj) * e2t(ji+1,jj) * sshb(ji+1,jj) )
                  sshv_b(ji,jj) = 0.5  * vmask(ji,jj,1) / ( e1v(ji,jj  ) * e2v(ji,jj  ) )                   &
                     &                                  * ( e1t(ji,jj  ) * e2t(ji,jj  ) * sshb(ji,jj  )     &
                     &                                    + e1t(ji,jj+1) * e2t(ji,jj+1) * sshb(ji,jj+1) )
               END DO
            END DO
            CALL lbc_lnk( sshu_b, 'U', 1. )
            CALL lbc_lnk( sshv_b, 'V', 1. )            !  Boundaries conditions
            !
         ENDIF
         !                    !--------------------------!
      ELSE                    !        fixed levels      !     (ssh at t-point only)
         !                    !--------------------------!
         !
         IF( neuler == 0 .AND. kt == nit000 ) THEN    !** Euler time-stepping at first time-step : no filter
            sshn(:,:) = ssha(:,:)                           ! now <-- after  (before already = now)
            !
         ELSE                                               ! Leap-Frog time-stepping: Asselin filter + swap
            DO jj = 1, jpj
               DO ji = 1, jpi                               ! before <-- now filtered
                  sshb(ji,jj) = sshn(ji,jj) + atfp * ( sshb(ji,jj) - 2 * sshn(ji,jj) + ssha(ji,jj) )
                  sshn(ji,jj) = ssha(ji,jj)                 ! now <-- after
               END DO
            END DO
         ENDIF
         !
      ENDIF
      !
      ! Update velocity at AGRIF zoom boundaries
#if defined key_agrif
      IF ( .NOT.Agrif_Root() ) CALL Agrif_Update_Dyn( kt )
#endif
      !
      IF(ln_ctl)   CALL prt_ctl( tab2d_1=sshb, clinfo1=' sshb  - : ', mask1=tmask, ovlap=1 )
      !
      IF( nn_timing == 1 )  CALL timing_stop('ssh_nxt')
      !
   END SUBROUTINE ssh_nxt

   !!======================================================================
END MODULE sshwzv
