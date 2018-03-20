MODULE dynspg_flt
   !!======================================================================
   !!                   ***  MODULE  dynspg_flt  ***
   !! Ocean dynamics:  surface pressure gradient trend
   !!======================================================================
   !! History    OPA  !  1998-05  (G. Roullet)  free surface
   !!                 !  1998-10  (G. Madec, M. Imbard)  release 8.2
   !!   NEMO     O.1  !  2002-08  (G. Madec)  F90: Free form and module
   !!             -   !  2002-11  (C. Talandier, A-M Treguier) Open boundaries
   !!            1.0  !  2004-08  (C. Talandier) New trends organization
   !!             -   !  2005-11  (V. Garnier) Surface pressure gradient organization
   !!            2.0  !  2006-07  (S. Masson)  distributed restart using iom
   !!             -   !  2006-08  (J.Chanut, A.Sellar) Calls to BDY routines. 
   !!            3.2  !  2009-03  (G. Madec, M. Leclair, R. Benshila) introduce sshwzv module
   !!----------------------------------------------------------------------
#if defined key_dynspg_flt   ||   defined key_esopa  
   !!----------------------------------------------------------------------
   !!   'key_dynspg_flt'                              filtered free surface
   !!----------------------------------------------------------------------
   !!   dyn_spg_flt  : update the momentum trend with the surface pressure gradient in the filtered free surface case 
   !!   flt_rst      : read/write the time-splitting restart fields in the ocean restart file
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers 
   USE dom_oce         ! ocean space and time domain 
   USE zdf_oce         ! ocean vertical physics
   USE sbc_oce         ! surface boundary condition: ocean
   USE obc_oce         ! Lateral open boundary condition
   USE bdy_oce         ! Lateral open boundary condition
   USE sol_oce         ! ocean elliptic solver
   USE phycst          ! physical constants
   USE domvvl          ! variable volume
   USE dynadv          ! advection 
   USE solmat          ! matrix construction for elliptic solvers
   USE solpcg          ! preconditionned conjugate gradient solver
   USE solsor          ! Successive Over-relaxation solver
   USE obcdyn          ! ocean open boundary condition on dynamics
   USE obcvol          ! ocean open boundary condition (obc_vol routine)
   USE bdydyn          ! ocean open boundary condition on dynamics
   USE bdyvol          ! ocean open boundary condition (bdy_vol routine)
   USE cla             ! cross land advection
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distributed memory computing library
   USE wrk_nemo        ! Memory Allocation
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE prtctl          ! Print control
   USE iom
   USE lib_fortran
#if defined key_agrif
   USE agrif_opa_interp
#endif
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_spg_flt  ! routine called by step.F90
   PUBLIC   flt_rst      ! routine called by istate.F90

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: dynspg_flt.F90 3692 2012-11-28 07:54:04Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_spg_flt( kt, kindic )
      !!----------------------------------------------------------------------
      !!                  ***  routine dyn_spg_flt  ***
      !!
      !! ** Purpose :   Compute the now trend due to the surface pressure 
      !!      gradient in case of filtered free surface formulation  and add
      !!      it to the general trend of momentum equation.
      !!
      !! ** Method  :   Filtered free surface formulation. The surface
      !!      pressure gradient is given by:
      !!         spgu = 1/rau0 d/dx(ps) =  1/e1u di( sshn + btda )
      !!         spgv = 1/rau0 d/dy(ps) =  1/e2v dj( sshn + btda )
      !!      where sshn is the free surface elevation and btda is the after
      !!      time derivative of the free surface elevation
      !!       -1- evaluate the surface presure trend (including the addi-
      !!      tional force) in three steps:
      !!        a- compute the right hand side of the elliptic equation:
      !!            gcb = 1/(e1t e2t) [ di(e2u spgu) + dj(e1v spgv) ]
      !!         where (spgu,spgv) are given by:
      !!            spgu = vertical sum[ e3u (ub+ 2 rdt ua ) ]
      !!                 - grav 2 rdt hu /e1u di[sshn + (emp-rnf)]
      !!            spgv = vertical sum[ e3v (vb+ 2 rdt va) ]
      !!                 - grav 2 rdt hv /e2v dj[sshn + (emp-rnf)]
      !!         and define the first guess from previous computation :
      !!            zbtd = btda
      !!            btda = 2 zbtd - btdb
      !!            btdb = zbtd
      !!        b- compute the relative accuracy to be reached by the
      !!         iterative solver
      !!        c- apply the solver by a call to sol... routine
      !!       -2- compute and add the free surface pressure gradient inclu-
      !!      ding the additional force used to stabilize the equation.
      !!
      !! ** Action : - Update (ua,va) with the surf. pressure gradient trend
      !!
      !! References : Roullet and Madec 1999, JGR.
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in   ) ::   kt       ! ocean time-step index
      INTEGER, INTENT(  out) ::   kindic   ! solver convergence flag (<0 if not converge)
      !!                                   
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   z2dt, z2dtg, zgcb, zbtd, ztdgu, ztdgv   ! local scalars
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  zub, zvb
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dyn_spg_flt')
      !
      CALL wrk_alloc( jpi,jpj,jpk, zub, zvb )
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn_spg_flt : surface pressure gradient trend'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~   (free surface constant volume case)'
       
         ! set to zero free surface specific arrays
         spgu(:,:) = 0._wp                     ! surface pressure gradient (i-direction)
         spgv(:,:) = 0._wp                     ! surface pressure gradient (j-direction)

         ! read filtered free surface arrays in restart file
         ! when using agrif, sshn, gcx have to be read in istate
         IF(.NOT. lk_agrif)   CALL flt_rst( nit000, 'READ' )      ! read or initialize the following fields:
         !                                                        ! gcx, gcxb
      ENDIF

      ! Local constant initialization
      z2dt = 2. * rdt                                             ! time step: leap-frog
      IF( neuler == 0 .AND. kt == nit000   )   z2dt = rdt         ! time step: Euler if restart from rest
      IF( neuler == 0 .AND. kt == nit000+1 )   CALL sol_mat( kt )
      z2dtg  = grav * z2dt

      ! Evaluate the masked next velocity (effect of the additional force not included)
      ! ---------------------------------  
      IF( lk_vvl ) THEN          ! variable volume  (surface pressure gradient already included in dyn_hpg)
         !
         IF( ln_dynadv_vec ) THEN      ! vector form : applied on velocity
            DO jk = 1, jpkm1
               DO jj = 2, jpjm1
                  DO ji = fs_2, fs_jpim1   ! vector opt.
                     ua(ji,jj,jk) = (  ub(ji,jj,jk) + z2dt * ua(ji,jj,jk)  ) * umask(ji,jj,jk)
                     va(ji,jj,jk) = (  vb(ji,jj,jk) + z2dt * va(ji,jj,jk)  ) * vmask(ji,jj,jk)
                  END DO
               END DO
            END DO
            !
         ELSE                          ! flux form : applied on thickness weighted velocity
            DO jk = 1, jpkm1
               DO jj = 2, jpjm1
                  DO ji = fs_2, fs_jpim1   ! vector opt.
                     ua(ji,jj,jk) = (        ub(ji,jj,jk) * fse3u_b(ji,jj,jk)      &
                        &           + z2dt * ua(ji,jj,jk) * fse3u_n(ji,jj,jk)  )   &
                        &         / fse3u_a(ji,jj,jk) * umask(ji,jj,jk)
                     va(ji,jj,jk) = (        vb(ji,jj,jk) * fse3v_b(ji,jj,jk)      &
                        &           + z2dt * va(ji,jj,jk) * fse3v_n(ji,jj,jk)  )   &
                        &         / fse3v_a(ji,jj,jk) * vmask(ji,jj,jk)
                 END DO
               END DO
            END DO
            !
         ENDIF
         !
      ELSE                       ! fixed volume  (add the surface pressure gradient + unweighted time stepping)
         !
         DO jj = 2, jpjm1              ! Surface pressure gradient (now)
            DO ji = fs_2, fs_jpim1   ! vector opt.
               spgu(ji,jj) = - grav * ( sshn(ji+1,jj) - sshn(ji,jj) ) / e1u(ji,jj)
               spgv(ji,jj) = - grav * ( sshn(ji,jj+1) - sshn(ji,jj) ) / e2v(ji,jj)
            END DO 
         END DO 
         DO jk = 1, jpkm1              ! unweighted time stepping 
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  ua(ji,jj,jk) = (  ub(ji,jj,jk) + z2dt * ( ua(ji,jj,jk) + spgu(ji,jj) )  ) * umask(ji,jj,jk)
                  va(ji,jj,jk) = (  vb(ji,jj,jk) + z2dt * ( va(ji,jj,jk) + spgv(ji,jj) )  ) * vmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      ENDIF

#if defined key_obc
      IF( lk_obc ) CALL obc_dyn( kt )   ! Update velocities on each open boundary with the radiation algorithm
      IF( lk_obc ) CALL obc_vol( kt )   ! Correction of the barotropic componant velocity to control the volume of the system
#endif
#if defined key_bdy
      IF( lk_bdy ) CALL bdy_dyn( kt )   ! Update velocities on each open boundary
      IF( lk_bdy ) CALL bdy_vol( kt )   ! Correction of the barotropic component velocity to control the volume of the system
#endif
#if defined key_agrif
      CALL Agrif_dyn( kt )    ! Update velocities on each coarse/fine interfaces 
#endif
      IF( nn_cla == 1 )   CALL cla_dynspg( kt )      ! Cross Land Advection (update (ua,va))

      ! compute the next vertically averaged velocity (effect of the additional force not included)
      ! ---------------------------------------------
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            spgu(ji,jj) = 0._wp
            spgv(ji,jj) = 0._wp
         END DO
      END DO

      ! vertical sum
!CDIR NOLOOPCHG
      IF( lk_vopt_loop ) THEN          ! vector opt., forced unroll
         DO jk = 1, jpkm1
            DO ji = 1, jpij
               spgu(ji,1) = spgu(ji,1) + fse3u(ji,1,jk) * ua(ji,1,jk)
               spgv(ji,1) = spgv(ji,1) + fse3v(ji,1,jk) * va(ji,1,jk)
            END DO
         END DO
      ELSE                        ! No  vector opt.
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = 2, jpim1
                  spgu(ji,jj) = spgu(ji,jj) + fse3u(ji,jj,jk) * ua(ji,jj,jk)
                  spgv(ji,jj) = spgv(ji,jj) + fse3v(ji,jj,jk) * va(ji,jj,jk)
               END DO
            END DO
         END DO
      ENDIF

      ! transport: multiplied by the horizontal scale factor
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            spgu(ji,jj) = spgu(ji,jj) * e2u(ji,jj)
            spgv(ji,jj) = spgv(ji,jj) * e1v(ji,jj)
         END DO
      END DO
      CALL lbc_lnk( spgu, 'U', -1. )       ! lateral boundary conditions 
      CALL lbc_lnk( spgv, 'V', -1. )

      IF( lk_vvl ) CALL sol_mat( kt )      ! build the matrix at kt (vvl case only)

      ! Right hand side of the elliptic equation and first guess
      ! --------------------------------------------------------
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            ! Divergence of the after vertically averaged velocity
            zgcb =  spgu(ji,jj) - spgu(ji-1,jj)   &
                  + spgv(ji,jj) - spgv(ji,jj-1)
            gcb(ji,jj) = gcdprc(ji,jj) * zgcb
            ! First guess of the after barotropic transport divergence
            zbtd = gcx(ji,jj)
            gcx (ji,jj) = 2. * zbtd   - gcxb(ji,jj)
            gcxb(ji,jj) =      zbtd
         END DO
      END DO
      ! applied the lateral boundary conditions
      IF( nn_solv == 2 .AND. MAX( jpr2di, jpr2dj ) > 0 )   CALL lbc_lnk_e( gcb, c_solver_pt, 1. )   

#if defined key_agrif
      IF( .NOT. AGRIF_ROOT() ) THEN
         ! add contribution of gradient of after barotropic transport divergence 
         IF( nbondi == -1 .OR. nbondi == 2 )   gcb(3     ,:) =   &
            &    gcb(3     ,:) - z2dtg * z2dt * laplacu(2     ,:) * gcdprc(3     ,:) * hu(2     ,:) * e2u(2     ,:)
         IF( nbondi ==  1 .OR. nbondi == 2 )   gcb(nlci-2,:) =   &
            &    gcb(nlci-2,:) + z2dtg * z2dt * laplacu(nlci-2,:) * gcdprc(nlci-2,:) * hu(nlci-2,:) * e2u(nlci-2,:)
         IF( nbondj == -1 .OR. nbondj == 2 )   gcb(:     ,3) =   &
            &    gcb(:,3     ) - z2dtg * z2dt * laplacv(:,2     ) * gcdprc(:,3     ) * hv(:,2     ) * e1v(:,2     )
         IF( nbondj ==  1 .OR. nbondj == 2 )   gcb(:,nlcj-2) =   &
            &    gcb(:,nlcj-2) + z2dtg * z2dt * laplacv(:,nlcj-2) * gcdprc(:,nlcj-2) * hv(:,nlcj-2) * e1v(:,nlcj-2)
      ENDIF
#endif


      ! Relative precision (computation on one processor)
      ! ------------------
      rnorme =0.e0
      rnorme = GLOB_SUM( gcb(1:jpi,1:jpj) * gcdmat(1:jpi,1:jpj) * gcb(1:jpi,1:jpj) * bmask(:,:) )

      epsr = eps * eps * rnorme
      ncut = 0
      ! if rnorme is 0, the solution is 0, the solver is not called
      IF( rnorme == 0._wp ) THEN
         gcx(:,:) = 0._wp
         res   = 0._wp
         niter = 0
         ncut  = 999
      ENDIF

      ! Evaluate the next transport divergence
      ! --------------------------------------
      !    Iterarive solver for the elliptic equation (except IF sol.=0)
      !    (output in gcx with boundary conditions applied)
      kindic = 0
      IF( ncut == 0 ) THEN
         IF    ( nn_solv == 1 ) THEN   ;   CALL sol_pcg( kindic )      ! diagonal preconditioned conjuguate gradient
         ELSEIF( nn_solv == 2 ) THEN   ;   CALL sol_sor( kindic )      ! successive-over-relaxation
         ENDIF
      ENDIF

      ! Transport divergence gradient multiplied by z2dt
      ! --------------------------------------------====
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            ! trend of Transport divergence gradient
            ztdgu = z2dtg * (gcx(ji+1,jj  ) - gcx(ji,jj) ) / e1u(ji,jj)
            ztdgv = z2dtg * (gcx(ji  ,jj+1) - gcx(ji,jj) ) / e2v(ji,jj)
            ! multiplied by z2dt
#if defined key_obc
IF(lk_obc) THEN
            ! caution : grad D = 0 along open boundaries
            ! Remark: The filtering force could be reduced here in the FRS zone
            !         by multiplying spgu/spgv by (1-alpha) ??  
            spgu(ji,jj) = z2dt * ztdgu * obcumask(ji,jj)
            spgv(ji,jj) = z2dt * ztdgv * obcvmask(ji,jj)
            ELSE
               spgu(ji,jj) = z2dt * ztdgu
               spgv(ji,jj) = z2dt * ztdgv
            ENDIF
#elif defined key_bdy
IF(lk_bdy) THEN
            ! caution : grad D = 0 along open boundaries
            spgu(ji,jj) = z2dt * ztdgu * bdyumask(ji,jj)
            spgv(ji,jj) = z2dt * ztdgv * bdyvmask(ji,jj)
            ELSE
               spgu(ji,jj) = z2dt * ztdgu
               spgv(ji,jj) = z2dt * ztdgv
            ENDIF
#else
            spgu(ji,jj) = z2dt * ztdgu
            spgv(ji,jj) = z2dt * ztdgv
#endif
         END DO
      END DO

#if defined key_agrif      
      IF( .NOT. Agrif_Root() ) THEN
         ! caution : grad D (fine) = grad D (coarse) at coarse/fine interface
         IF( nbondi == -1 .OR. nbondi == 2 ) spgu(2     ,:) = z2dtg * z2dt * laplacu(2     ,:) * umask(2     ,:,1)
         IF( nbondi ==  1 .OR. nbondi == 2 ) spgu(nlci-2,:) = z2dtg * z2dt * laplacu(nlci-2,:) * umask(nlci-2,:,1)
         IF( nbondj == -1 .OR. nbondj == 2 ) spgv(:,2     ) = z2dtg * z2dt * laplacv(:,2     ) * vmask(:     ,2,1)
         IF( nbondj ==  1 .OR. nbondj == 2 ) spgv(:,nlcj-2) = z2dtg * z2dt * laplacv(:,nlcj-2) * vmask(:,nlcj-2,1)
      ENDIF
#endif      
      ! Add the trends multiplied by z2dt to the after velocity
      ! -------------------------------------------------------
      !     ( c a u t i o n : (ua,va) here are the after velocity not the
      !                       trend, the leap-frog time stepping will not
      !                       be done in dynnxt.F90 routine)
      DO jk = 1, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               ua(ji,jj,jk) = ( ua(ji,jj,jk) + spgu(ji,jj) ) * umask(ji,jj,jk)
               va(ji,jj,jk) = ( va(ji,jj,jk) + spgv(ji,jj) ) * vmask(ji,jj,jk)
            END DO
         END DO
      END DO

      ! write filtered free surface arrays in restart file
      ! --------------------------------------------------
      IF( lrst_oce ) CALL flt_rst( kt, 'WRITE' )
      !
      CALL wrk_dealloc( jpi,jpj,jpk, zub, zvb )
      !
      IF( nn_timing == 1 )  CALL timing_stop('dyn_spg_flt')
      !
   END SUBROUTINE dyn_spg_flt


   SUBROUTINE flt_rst( kt, cdrw )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE ts_rst  ***
      !!
      !! ** Purpose : Read or write filtered free surface arrays in restart file
      !!----------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   kt         ! ocean time-step
      CHARACTER(len=*), INTENT(in) ::   cdrw       ! "READ"/"WRITE" flag
      !!----------------------------------------------------------------------
      !
      IF( TRIM(cdrw) == 'READ' ) THEN
         IF( iom_varid( numror, 'gcx', ldstop = .FALSE. ) > 0 ) THEN
! Caution : extra-hallow
! gcx and gcxb are defined as: DIMENSION(1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj)
            CALL iom_get( numror, jpdom_autoglo, 'gcx' , gcx (1:jpi,1:jpj) )
            CALL iom_get( numror, jpdom_autoglo, 'gcxb', gcxb(1:jpi,1:jpj) )
            IF( neuler == 0 )   gcxb(:,:) = gcx (:,:)
         ELSE
            gcx (:,:) = 0.e0
            gcxb(:,:) = 0.e0
         ENDIF
      ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN
! Caution : extra-hallow
! gcx and gcxb are defined as: DIMENSION(1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj)
         CALL iom_rstput( kt, nitrst, numrow, 'gcx' , gcx (1:jpi,1:jpj) )
         CALL iom_rstput( kt, nitrst, numrow, 'gcxb', gcxb(1:jpi,1:jpj) )
      ENDIF
      !
   END SUBROUTINE flt_rst

#else
   !!----------------------------------------------------------------------
   !!   Default case :   Empty module   No standart free surface cst volume
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE dyn_spg_flt( kt, kindic )       ! Empty routine
      WRITE(*,*) 'dyn_spg_flt: You should not have seen this print! error?', kt, kindic
   END SUBROUTINE dyn_spg_flt
   SUBROUTINE flt_rst    ( kt, cdrw )         ! Empty routine
      INTEGER         , INTENT(in) ::   kt         ! ocean time-step
      CHARACTER(len=*), INTENT(in) ::   cdrw       ! "READ"/"WRITE" flag
      WRITE(*,*) 'flt_rst: You should not have seen this print! error?', kt, cdrw
   END SUBROUTINE flt_rst
#endif
   
   !!======================================================================
END MODULE dynspg_flt
