MODULE solsor
   !!======================================================================
   !!                     ***  MODULE  solsor  ***
   !! Ocean solver :  Successive Over-Relaxation solver
   !!=====================================================================
   !! History :  OPA  ! 1990-10  (G. Madec)  Original code
   !!            7.1  ! 1993-04  (G. Madec)  time filter
   !!                 ! 1996-05  (G. Madec)  merge sor and pcg formulations
   !!                 ! 1996-11  (A. Weaver)  correction to preconditioning
   !!   NEMO     1.0  ! 2003-04  (C. Deltel, G. Madec)  Red-Black SOR in free form
   !!            2.0  ! 2005-09  (R. Benshila, G. Madec)  MPI optimization
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sol_sor     : Red-Black Successive Over-Relaxation solver
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables 
   USE zdf_oce         ! ocean vertical physics variables
   USE sol_oce         ! solver variables
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distributed memory computing
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE lib_fortran     ! Fortran routines library
   USE wrk_nemo        ! Memory allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sol_sor    ! 

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: solsor.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS
      
   SUBROUTINE sol_sor( kindic )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE sol_sor  ***
      !!                 
      !! ** Purpose :   Solve the ellipic equation for the transport 
      !!      divergence system  using a red-black successive-over-
      !!      relaxation method.
      !!       This routine provides a MPI optimization to the existing solsor
      !!     by reducing the number of call to lbc.
      !! 
      !! ** Method  :   Successive-over-relaxation method using the red-black 
      !!      technique. The former technique used was not compatible with 
      !!      the north-fold boundary condition used in orca configurations.
      !!      Compared to the classical sol_sor, this routine provides a 
      !!      mpp optimization by reducing the number of calls to lnc_lnk
      !!      The solution is computed on a larger area and the boudary
      !!      conditions only when the inside domain is reached.
      !! 
      !! References :   Madec et al. 1988, Ocean Modelling, issue 78, 1-6.
      !!                Beare and Stevens 1997 Ann. Geophysicae 15, 1369-1377
      !!----------------------------------------------------------------------
      !!
      INTEGER, INTENT(inout) ::   kindic   ! solver indicator, < 0 if the convergence is not reached:
      !                                    ! the model is stopped in step (set to zero before the call of solsor)
      !!
      INTEGER  ::   ji, jj, jn       ! dummy loop indices
      INTEGER  ::   ishift, icount, ijmppodd, ijmppeven, ijpr2d   ! local integers
      REAL(wp) ::   ztmp, zres, zres2                             ! local scalars
      REAL(wp), POINTER, DIMENSION(:,:) ::   ztab                 ! 2D workspace
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('sol_sor')
      !
      CALL wrk_alloc( jpi, jpj, ztab )
      !
      ijmppeven = MOD( nimpp+njmpp+jpr2di+jpr2dj   , 2 )
      ijmppodd  = MOD( nimpp+njmpp+jpr2di+jpr2dj+1 , 2 )
      ijpr2d    = MAX( jpr2di , jpr2dj )
      icount = 0
      !                                                       ! ==============
      DO jn = 1, nn_nmax                                      ! Iterative loop 
         !                                                    ! ==============

         IF( MOD(icount,ijpr2d+1) == 0 )   CALL lbc_lnk_e( gcx, c_solver_pt, 1. )   ! lateral boundary conditions
        
         ! Residus
         ! -------

         ! Guess black update
         DO jj = 2-jpr2dj, nlcj-1+jpr2dj
            ishift = MOD( jj-ijmppodd-jpr2dj, 2 )
            DO ji = 2-jpr2di+ishift, nlci-1+jpr2di, 2
               ztmp =                  gcb(ji  ,jj  )   &
                  &   - gcp(ji,jj,1) * gcx(ji  ,jj-1)   &
                  &   - gcp(ji,jj,2) * gcx(ji-1,jj  )   &
                  &   - gcp(ji,jj,3) * gcx(ji+1,jj  )   &
                  &   - gcp(ji,jj,4) * gcx(ji  ,jj+1)
               ! Estimate of the residual
               zres = ztmp - gcx(ji,jj)
               gcr(ji,jj) = zres * gcdmat(ji,jj) * zres
               ! Guess update
               gcx(ji,jj) = rn_sor * ztmp + (1-rn_sor) * gcx(ji,jj)
            END DO
         END DO
         icount = icount + 1 
 
         IF( MOD(icount,ijpr2d+1) == 0 )   CALL lbc_lnk_e( gcx, c_solver_pt, 1. )   ! lateral boundary conditions

         ! Guess red update
         DO jj = 2-jpr2dj, nlcj-1+jpr2dj
            ishift = MOD( jj-ijmppeven-jpr2dj, 2 )
            DO ji = 2-jpr2di+ishift, nlci-1+jpr2di, 2
               ztmp =                  gcb(ji  ,jj  )   &
                  &   - gcp(ji,jj,1) * gcx(ji  ,jj-1)   &
                  &   - gcp(ji,jj,2) * gcx(ji-1,jj  )   &
                  &   - gcp(ji,jj,3) * gcx(ji+1,jj  )   &
                  &   - gcp(ji,jj,4) * gcx(ji  ,jj+1) 
               ! Estimate of the residual
               zres = ztmp - gcx(ji,jj)
               gcr(ji,jj) = zres * gcdmat(ji,jj) * zres
               ! Guess update
               gcx(ji,jj) = rn_sor * ztmp + (1-rn_sor) * gcx(ji,jj)
            END DO
         END DO
         icount = icount + 1

         ! test of convergence
         IF ( jn > nn_nmin .AND. MOD( jn-nn_nmin, nn_nmod ) == 0 ) THEN

            SELECT CASE ( nn_sol_arp )
            CASE ( 0 )                 ! absolute precision (maximum value of the residual)
               zres2 = MAXVAL( gcr(2:nlci-1,2:nlcj-1) )
               IF( lk_mpp )   CALL mpp_max( zres2 )   ! max over the global domain
               ! test of convergence
               IF( zres2 < rn_resmax .OR. jn == nn_nmax ) THEN
                  res = SQRT( zres2 )
                  niter = jn
                  ncut = 999
               ENDIF
            CASE ( 1 )                 ! relative precision
               ztab = 0.
               ztab(:,:) = gcr(2:nlci-1,2:nlcj-1)
               rnorme = glob_sum( ztab)    ! sum over the global domain
               ! test of convergence
               IF( rnorme < epsr .OR. jn == nn_nmax ) THEN
                  res = SQRT( rnorme )
                  niter = jn
                  ncut = 999
               ENDIF
            END SELECT
         
         !****
         !     IF(lwp)WRITE(numsol,9300) jn, res, sqrt( epsr ) / eps
9300     FORMAT('          niter :',i4,' res :',e20.10,' b :',e20.10)
         !****
         
         ENDIF
         ! indicator of non-convergence or explosion
         IF( jn == nn_nmax .OR. SQRT(epsr)/eps > 1.e+20 ) kindic = -2
         IF( ncut == 999 ) GOTO 999
         
         !                                                 ! =====================
      END DO                                               ! END of iterative loop
      !                                                    ! =====================
      
999   CONTINUE
      
      !  Output in gcx
      !  -------------
      CALL lbc_lnk_e( gcx, c_solver_pt, 1. )    ! boundary conditions
      !
      CALL wrk_dealloc( jpi, jpj, ztab )
      !
      IF( nn_timing == 1 )  CALL timing_stop('sol_sor')
      !
   END SUBROUTINE sol_sor

   !!=====================================================================
END MODULE solsor
