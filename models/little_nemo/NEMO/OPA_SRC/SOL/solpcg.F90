MODULE solpcg
   !!======================================================================
   !!                     ***  MODULE  solfet
   !! Ocean solver :  preconditionned conjugate gradient solver
   !!=====================================================================

   !!----------------------------------------------------------------------
   !!   sol_pcg    : preconditionned conjugate gradient solver
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables 
   USE sol_oce         ! ocean solver variables
   USE lib_mpp         ! distributed memory computing
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE in_out_manager  ! I/O manager
   USE lib_fortran     ! Fortran routines library
   USE wrk_nemo        ! Memory allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sol_pcg    ! 

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: solpcg.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sol_pcg( kindic )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE sol_pcg  ***
      !!                    
      !! ** Purpose :   Solve the ellipic equation for the transport
      !!      divergence system  using a diagonal preconditionned
      !!      conjugate gradient method.
      !!
      !! ** Method  :   Diagonal preconditionned conjugate gradient method.
      !!      the algorithm is multitasked. (case of 5 points matrix)
      !!      define              pa  = q^-1 * a
      !!                        pgcb  = q^-1 * gcb
      !!                 < . ; . >_q  = ( . )^t q ( . )
      !!      where q is the preconditioning matrix = diagonal matrix of the
      !!                                              diagonal elements of a
      !!      Initialization  :
      !!         x(o) = gcx
      !!         r(o) = d(o) = pgcb - pa.x(o)
      !!         rr(o)= < r(o) , r(o) >_q
      !!      Iteration 1     :
      !!         standard PCG algorithm
      !!      Iteration n > 1 :
      !!         s(n)   = pa.r(n)
      !!         gam(n) = < r(n) , r(n) >_q
      !!         del(n) = < r(n) , s(n) >_q
      !!         bet(n) = gam(n) / gam(n-1)
      !!         d(n)   = r(n) + bet(n) d(n-1)
      !!         z(n)   = s(n) + bet(n) z(n-1) 
      !!         sig(n) = del(n) - bet(n)*bet(n)*sig(n-1) 
      !!         alp(n) = gam(n) / sig(n) 
      !!         x(n+1) = x(n) + alp(n) d(n)
      !!         r(n+1) = r(n) - alp(n) z(n)
      !!      Convergence test :
      !!         rr(n+1) / < gcb , gcb >_q   =< epsr
      !!
      !! ** Action : - niter  : solver number of iteration done
      !!             - res    : solver residu reached
      !!             - gcx()  : solution of the elliptic system
      !!
      !! References :
      !!      Madec et al. 1988, Ocean Modelling, issue 78, 1-6.
      !!      D Azevedo et al. 1993, Computer Science Technical Report, Tennessee U.
      !!
      !! History :
      !!        !  90-10  (G. Madec)  Original code
      !!        !  91-11  (G. Madec)
      !!        !  93-04  (M. Guyon)  loops and suppress pointers
      !!        !  95-09  (M. Imbard, J. Escobar)  mpp exchange 
      !!        !  96-05  (G. Madec)  merge sor and pcg formulations
      !!        !  96-11  (A. Weaver)  correction to preconditioning
      !!   8.5  !  02-08  (G. Madec)  F90: Free form
      !!        !  08-01  (R. Benshila) mpp optimization
      !!----------------------------------------------------------------------
      !!
      INTEGER, INTENT(inout) ::   kindic   ! solver indicator, < 0 if the conver-
      !                                    ! gence is not reached: the model is stopped in step
      !                                    ! set to zero before the call of solpcg
      !!
      INTEGER  ::   ji, jj, jn   ! dummy loop indices
      REAL(wp) ::   zgcad        ! temporary scalars
      REAL(wp), DIMENSION(2) ::   zsum
      REAL(wp), POINTER, DIMENSION(:,:) ::   zgcr
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('sol_pcg')
      !
      CALL wrk_alloc( jpi, jpj, zgcr )
      !
      ! Initialization of the algorithm with standard PCG
      ! -------------------------------------------------
      zgcr = 0._wp
      gcr  = 0._wp

      CALL lbc_lnk( gcx, c_solver_pt, 1. )   ! lateral boundary condition

      ! gcr   = gcb-a.gcx
      ! gcdes = gcr
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            zgcad = bmask(ji,jj) * ( gcb(ji,jj  ) -                gcx(ji  ,jj  )   &
               &                                  - gcp(ji,jj,1) * gcx(ji  ,jj-1)   &
               &                                  - gcp(ji,jj,2) * gcx(ji-1,jj  )   &
               &                                  - gcp(ji,jj,3) * gcx(ji+1,jj  )   &
               &                                  - gcp(ji,jj,4) * gcx(ji  ,jj+1)   )
            gcr  (ji,jj) = zgcad
            gcdes(ji,jj) = zgcad
         END DO
      END DO

      ! rnorme = (gcr,gcr)
      rnorme = glob_sum(  gcr(:,:) * gcdmat(:,:) * gcr(:,:)  )

      CALL lbc_lnk( gcdes, c_solver_pt, 1. )   ! lateral boundary condition

      ! gccd = matrix . gcdes
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            gccd(ji,jj) = bmask(ji,jj)*( gcdes(ji,jj)   &
               &        +gcp(ji,jj,1)*gcdes(ji,jj-1)+gcp(ji,jj,2)*gcdes(ji-1,jj)   &
               &        +gcp(ji,jj,4)*gcdes(ji,jj+1)+gcp(ji,jj,3)*gcdes(ji+1,jj)   )
         END DO
      END DO 

      ! alph = (gcr,gcr)/(gcdes,gccd)
      radd = glob_sum(  gcdes(:,:) * gcdmat(:,:) * gccd(:,:)  )
      alph = rnorme /radd

      ! gcx = gcx + alph * gcdes
      ! gcr = gcr - alph * gccd
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            gcx(ji,jj) = bmask(ji,jj) * ( gcx(ji,jj) + alph * gcdes(ji,jj) )
            gcr(ji,jj) = bmask(ji,jj) * ( gcr(ji,jj) - alph * gccd (ji,jj) )
         END DO
      END DO

      ! Algorithm wtih Eijkhout rearrangement
      ! -------------------------------------
        
      !                                                !================
      DO jn = 1, nn_nmax                               ! Iterative loop
         !                                             !================

         CALL lbc_lnk( gcr, c_solver_pt, 1. )   ! lateral boundary condition

         ! zgcr = matrix . gcr
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zgcr(ji,jj) = bmask(ji,jj)*( gcr(ji,jj)   &
                  &        +gcp(ji,jj,1)*gcr(ji,jj-1)+gcp(ji,jj,2)*gcr(ji-1,jj)   &
                  &        +gcp(ji,jj,4)*gcr(ji,jj+1)+gcp(ji,jj,3)*gcr(ji+1,jj)   )
            END DO
         END DO
 
         ! rnorme = (gcr,gcr)
         rr = rnorme

         ! zgcad = (zgcr,gcr) 
         zsum(1) = glob_sum(gcr(:,:) * gcdmat(:,:) * gcr(:,:))
         zsum(2) = glob_sum(gcr(:,:) * gcdmat(:,:) * zgcr(:,:) * bmask(:,:))

         !!RB we should gather the 2 glob_sum
         rnorme = zsum(1)  
         zgcad  = zsum(2)
         ! test of convergence
         IF( rnorme < epsr .OR. jn == nn_nmax ) THEN
            res = SQRT( rnorme )
            niter = jn
            ncut = 999
         ENDIF

         ! beta = (rk+1,rk+1)/(rk,rk)
         beta = rnorme / rr
         radd = zgcad - beta*beta*radd
         alph = rnorme / radd

         ! gcx = gcx + alph * gcdes
         ! gcr = gcr - alph * gccd
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               gcdes(ji,jj) = gcr (ji,jj) + beta * gcdes(ji,jj) 
               gccd (ji,jj) = zgcr(ji,jj) + beta * gccd (ji,jj) 
               gcx  (ji,jj) = gcx (ji,jj) + alph * gcdes(ji,jj) 
               gcr  (ji,jj) = gcr (ji,jj) - alph * gccd (ji,jj) 
            END DO
         END DO
        
         ! indicator of non-convergence or explosion
         IF( jn == nn_nmax .OR. SQRT(epsr)/eps > 1.e+20 ) kindic = -2
         IF( ncut == 999 ) GOTO 999

         !                                             !================
      END DO                                           !    End Loop
      !                                                !================
999   CONTINUE
          
      CALL lbc_lnk( gcx, c_solver_pt, 1. )      ! Output in gcx with lateral b.c. applied
      ! 
      CALL wrk_dealloc( jpi, jpj, zgcr )
      !
      IF( nn_timing == 1 )  CALL timing_stop('sol_pcg')
      !
   END SUBROUTINE sol_pcg

   !!=====================================================================
END MODULE solpcg
