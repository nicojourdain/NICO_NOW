MODULE sol_oce
   !!======================================================================
   !!                    ***  MODULE  sol_oce  ***
   !! Ocean solver :  elliptic solver variables defined in memory 
   !!======================================================================
   !! History :  1.0  ! 2002-11  (G. Madec)  F90: Free form and module
   !!            4.0  ! 2011-01  (A. R. Porter, STFC Daresbury) dynamical allocation
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sol_oce_alloc : allocate the solver arrays
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! distributed memory computing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sol_oce_alloc   ! routine called in solver.F90

   !                                             !!* Namelist namsol : elliptic solver *
   INTEGER , PUBLIC ::   nn_solv    =    1        !: = 1/2 type of elliptic solver
   INTEGER , PUBLIC ::   nn_sol_arp =    0        !: = 0/1 absolute/relative precision convergence test
   INTEGER , PUBLIC ::   nn_nmin    =  300        !: minimum of iterations for the SOR solver
   INTEGER , PUBLIC ::   nn_nmax    =  800        !: maximum of iterations for the SOR solver
   INTEGER , PUBLIC ::   nn_nmod    =   10        !: frequency of test for the SOR solver
   REAL(wp), PUBLIC ::   rn_eps     =  1.e-6_wp   !: absolute precision of the solver
   REAL(wp), PUBLIC ::   rn_resmax  = 1.e-14_wp   !: absolute precision for the SOR solver
   REAL(wp), PUBLIC ::   rn_sor     =   1.92_wp   !: optimal coefficient for the SOR solver
   REAL(wp), PUBLIC ::   rn_nu      =    1.0_wp   !: strength of the additional force used in free surface

   CHARACTER(len=1), PUBLIC ::   c_solver_pt = 'T'   !: nature of grid-points T (S) for free surface case

   INTEGER , PUBLIC ::   ncut        !: indicator of solver convergence
   INTEGER , PUBLIC ::   niter       !: number of iteration done by the solver

   REAL(wp), PUBLIC ::   eps, epsr   !: relative precision for SOR & PCG solvers
   REAL(wp), PUBLIC ::   rnorme      !: intermediate modulus
   REAL(wp), PUBLIC ::   res         !: solver residu
   REAL(wp), PUBLIC ::   alph        !: coefficient  =(gcr,gcr)/(gcx,gccd)
   REAL(wp), PUBLIC ::   beta        !: coefficient  =(rn+1,rn+1)/(rn,rn)
   REAL(wp), PUBLIC ::   radd        !: coefficient  =(gccd,gcdes)
   REAL(wp), PUBLIC ::   rr          !: coefficient  =(rn,rn)

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   gcp     !: matrix extra-diagonal elements
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   gcx     !: now    solution of the elliptic eq.
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   gcxb    !: before solution of the elliptic eq.
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   gcdprc  !: inverse diagonal preconditioning matrix
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   gcdmat  !: diagonal preconditioning matrix
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   gcb     !: second member of the elliptic eq.
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   gcr     !: residu =b-a.x
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   gcdes   !: vector descente
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   gccd    !: gccd= gcdprc^-1.a.d 

#if defined key_agrif
      REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) :: laplacu, laplacv
#endif

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: sol_oce.F90 2715 2011-03-30 15:58:35Z rblod $ 
   !! Software governed by the CeCILL licence    (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION sol_oce_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION sol_oce_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER  :: ierr(3)
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !
      ALLOCATE( gcp (1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj,4) ,     &
         &      gcx (1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj)   ,     &
         &      gcxb(1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj)   , STAT=ierr(1) )

      ALLOCATE( gcdprc(1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj) ,     & 
         &      gcdmat(1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj) ,     & 
         &      gcb   (1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj) , STAT=ierr(2) )

      ALLOCATE( gcr  (1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj) ,   & 
         &      gcdes(1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj) ,   & 
         &      gccd (1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj) ,   &
#if defined key_agrif
         &      laplacu(jpi,jpj), laplacv(jpi,jpj),             &
#endif
         &      STAT=ierr(3) )
         !
      sol_oce_alloc = MAXVAL(ierr)
      !
      IF( lk_mpp            )   CALL mpp_sum ( sol_oce_alloc )
      IF( sol_oce_alloc > 0 )   CALL ctl_warn('sol_oce_alloc: allocation of arrays failed')
      !
   END FUNCTION sol_oce_alloc

   !!======================================================================
END MODULE sol_oce
