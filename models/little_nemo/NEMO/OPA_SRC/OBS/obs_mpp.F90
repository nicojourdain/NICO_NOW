MODULE obs_mpp
   !!======================================================================
   !!                       ***  MODULE obs_mpp  ***
   !! Observation diagnostics: Various MPP support routines
   !!======================================================================
   !! History :  2.0  ! 2006-03  (K. Mogensen)  Original code
   !!             -   ! 2006-05  (K. Mogensen)  Reformatted
   !!             -   ! 2008-01  (K. Mogensen)  add mpp_global_max
   !!----------------------------------------------------------------------
#  define mpivar mpi_double_precision
   !!----------------------------------------------------------------------
   !! obs_mpp_bcast_integer : Broadcast an integer array from a processor to all processors
   !! obs_mpp_max_integer   : Find maximum on all processors of each value in an integer on all processors
   !! obs_mpp_find_obs_proc : Find processors which should hold the observations
   !! obs_mpp_sum_integers  : Sum an integer array from all processors
   !! obs_mpp_sum_integer   : Sum an integer from all processors
   !!----------------------------------------------------------------------
   USE dom_oce, ONLY :   nproc, mig, mjg   ! Ocean space and time domain variables
   USE mpp_map, ONLY :   mppmap
   USE in_out_manager
#if defined key_mpp_mpi
   USE lib_mpp, ONLY :   mpi_comm_opa      ! MPP library
#endif
   IMPLICIT NONE
   PRIVATE

   PUBLIC obs_mpp_bcast_integer, & !: Broadcast an integer array from a proc to all procs
      &   obs_mpp_max_integer,   & !: Find maximum across processors in an integer array
      &   obs_mpp_find_obs_proc, & !: Find processors which should hold the observations
      &   obs_mpp_sum_integers,  & !: Sum an integer array from all processors
      &   obs_mpp_sum_integer,   & !: Sum an integer from all processors
      &   mpp_alltoall_int,      &
      &   mpp_alltoallv_int,     &
      &   mpp_alltoallv_real,    &
      &   mpp_global_max

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_mpp.F90 2513 2010-12-23 16:01:47Z smasson $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE obs_mpp_bcast_integer( kvals, kno, kroot )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE obs_mpp_bcast_integer ***
      !!          
      !! ** Purpose : Send array kvals to all processors
      !!
      !! ** Method  : MPI broadcast
      !!
      !! ** Action  : This does only work for MPI. 
      !!              MPI_COMM_OPA needs to be replace for OASIS4.!
      !!
      !! References : http://www.mpi-forum.org
      !!----------------------------------------------------------------------
      INTEGER                , INTENT(in   ) ::   kno     ! Number of elements in array
      INTEGER                , INTENT(in   ) ::   kroot   ! Processor to send data
      INTEGER, DIMENSION(kno), INTENT(inout) ::   kvals   ! Array to send on kroot, receive for non-kroot
      !
#if defined key_mpp_mpi
      !
      INTEGER :: ierr 
      !
INCLUDE 'mpif.h'
      !!----------------------------------------------------------------------

      ! Call the MPI library to broadcast data
      CALL mpi_bcast( kvals, kno, mpi_integer,  &
         &            kroot, mpi_comm_opa, ierr )
#else
      ! no MPI: empty routine
#endif
      !
   END SUBROUTINE obs_mpp_bcast_integer

  
   SUBROUTINE obs_mpp_max_integer( kvals, kno )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE obs_mpp_bcast_integer ***
      !!          
      !! ** Purpose : Find maximum across processors in an integer array.
      !!
      !! ** Method  : MPI all reduce.
      !!
      !! ** Action  : This does only work for MPI. 
      !!              It does not work for SHMEM.
      !!              MPI_COMM_OPA needs to be replace for OASIS4.!
      !!
      !! References : http://www.mpi-forum.org
      !!----------------------------------------------------------------------
      INTEGER                , INTENT(in   ) ::   kno     ! Number of elements in array
      INTEGER, DIMENSION(kno), INTENT(inout) ::   kvals   ! Array to send on kroot, receive for non-kroot  
      !
#if defined key_mpp_mpi
      !
      INTEGER :: ierr 
      INTEGER, DIMENSION(kno) ::   ivals
      !
INCLUDE 'mpif.h'
      !!----------------------------------------------------------------------

      ! Call the MPI library to find the maximum across processors
      CALL mpi_allreduce( kvals, ivals, kno, mpi_integer,   &
         &                mpi_max, mpi_comm_opa, ierr )
      kvals(:) = ivals(:)
#else
      ! no MPI: empty routine
#endif
   END SUBROUTINE obs_mpp_max_integer


   SUBROUTINE obs_mpp_find_obs_proc( kobsp, kobsi, kobsj, kno )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE obs_mpp_find_obs_proc ***
      !!          
      !! ** Purpose : From the array kobsp containing the results of the grid
      !!              grid search on each processor the processor return a
      !!              decision of which processors should hold the observation.
      !!
      !! ** Method  : A temporary 2D array holding all the decisions is
      !!              constructed using mpi_allgather on each processor.
      !!              If more than one processor has found the observation
      !!              with the observation in the inner domain gets it
      !!
      !! ** Action  : This does only work for MPI. 
      !!              It does not work for SHMEM.
      !!
      !! References : http://www.mpi-forum.org
      !!----------------------------------------------------------------------
      INTEGER                , INTENT(in   ) ::   kno
      INTEGER, DIMENSION(kno), INTENT(in   ) ::   kobsi, kobsj
      INTEGER, DIMENSION(kno), INTENT(inout) ::   kobsp
      !
#if defined key_mpp_mpi
      !
      INTEGER :: ji
      INTEGER :: jj
      INTEGER :: size
      INTEGER :: ierr
      INTEGER :: iobsip
      INTEGER :: iobsjp
      INTEGER :: num_sus_obs
      INTEGER, DIMENSION(kno) ::   iobsig, iobsjg
      INTEGER, ALLOCATABLE, DIMENSION(:,:) ::   iobsp, iobsi, iobsj
      !!
INCLUDE 'mpif.h'
      !!----------------------------------------------------------------------

      !-----------------------------------------------------------------------
      ! Call the MPI library to find the maximum accross processors
      !-----------------------------------------------------------------------
      CALL mpi_comm_size( mpi_comm_opa, size, ierr )
      !-----------------------------------------------------------------------
      ! Convert local grids points to global grid points
      !-----------------------------------------------------------------------
      DO ji = 1, kno
         IF ( ( kobsi(ji) >= 1 ) .AND. ( kobsi(ji) <= jpi ) .AND. &
            & ( kobsj(ji) >= 1 ) .AND. ( kobsj(ji) <= jpj ) ) THEN
            iobsig(ji) = mig( kobsi(ji) )
            iobsjg(ji) = mjg( kobsj(ji) )
         ELSE
            iobsig(ji) = -1
            iobsjg(ji) = -1
         ENDIF
      END DO
      !-----------------------------------------------------------------------
      ! Get the decisions from all processors
      !-----------------------------------------------------------------------
      ALLOCATE( iobsp(kno,size) )
      ALLOCATE( iobsi(kno,size) )
      ALLOCATE( iobsj(kno,size) )
      CALL mpi_allgather( kobsp, kno, mpi_integer, &
         &                iobsp, kno, mpi_integer, &
         &                mpi_comm_opa, ierr )
      CALL mpi_allgather( iobsig, kno, mpi_integer, &
         &                iobsi, kno, mpi_integer, &
         &                mpi_comm_opa, ierr )
      CALL mpi_allgather( iobsjg, kno, mpi_integer, &
         &                iobsj, kno, mpi_integer, &
         &                mpi_comm_opa, ierr )

      !-----------------------------------------------------------------------
      ! Find the processor with observations from the lowest processor 
      ! number among processors holding the observation.
      !-----------------------------------------------------------------------
      kobsp(:) = -1
      num_sus_obs = 0
      DO ji = 1, kno
         DO jj = 1, size
            IF ( ( kobsp(ji) == -1 ) .AND. ( iobsp(ji,jj) /= -1 ) ) THEN
               kobsp(ji) = iobsp(ji,jj)
               iobsip = iobsi(ji,jj)
               iobsjp = iobsj(ji,jj)
            ENDIF
            IF ( ( kobsp(ji) /= -1 ) .AND. ( iobsp(ji,jj) /= -1 ) ) THEN
               IF ( ( iobsip /= iobsi(ji,jj) ) .OR. &
                  & ( iobsjp /= iobsj(ji,jj) ) ) THEN
                  IF ( ( kobsp(ji) < 1000000 ) .AND. &
                     & ( iobsp(ji,jj) < 1000000 ) ) THEN
                     num_sus_obs=num_sus_obs+1
                  ENDIF
               ENDIF
               IF ( mppmap(iobsip,iobsjp) /= ( kobsp(ji)+1 ) ) THEN
                  IF ( ( iobsi(ji,jj) /= -1 ) .AND. &
                     & ( iobsj(ji,jj) /= -1 ) ) THEN
                     IF ((mppmap(iobsi(ji,jj),iobsj(ji,jj)) == (iobsp(ji,jj)+1))&
                        & .OR. ( iobsp(ji,jj) < kobsp(ji) ) ) THEN
                        kobsp(ji) = iobsp(ji,jj)
                        iobsip = iobsi(ji,jj)
                        iobsjp = iobsj(ji,jj)
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         END DO
      END DO
      IF (lwp) WRITE(numout,*) 'Number of suspicious observations: ',num_sus_obs

      DEALLOCATE( iobsj )
      DEALLOCATE( iobsi )
      DEALLOCATE( iobsp )
#else
      ! no MPI: empty routine
#endif
      !
   END SUBROUTINE obs_mpp_find_obs_proc


   SUBROUTINE obs_mpp_sum_integers( kvalsin, kvalsout, kno )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE obs_mpp_sum_integers ***
      !!          
      !! ** Purpose : Sum an integer array.
      !!
      !! ** Method  : MPI all reduce.
      !!
      !! ** Action  : This does only work for MPI. 
      !!              It does not work for SHMEM.
      !!
      !! References : http://www.mpi-forum.org
      !!----------------------------------------------------------------------
      INTEGER                , INTENT(in   ) :: kno
      INTEGER, DIMENSION(kno), INTENT(in   ) ::   kvalsin
      INTEGER, DIMENSION(kno), INTENT(  out) ::   kvalsout
      !
#if defined key_mpp_mpi
      !
      INTEGER :: ierr
      !
INCLUDE 'mpif.h'
      !!----------------------------------------------------------------------
      !
      !-----------------------------------------------------------------------
      ! Call the MPI library to find the sum across processors
      !-----------------------------------------------------------------------
      CALL mpi_allreduce( kvalsin, kvalsout, kno, mpi_integer, &
         &                mpi_sum, mpi_comm_opa, ierr )
#else
      !-----------------------------------------------------------------------
      ! For no-MPP just return input values
      !-----------------------------------------------------------------------
      kvalsout(:) = kvalsin(:)
#endif
      !
   END SUBROUTINE obs_mpp_sum_integers


   SUBROUTINE obs_mpp_sum_integer( kvalin, kvalout )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE obs_mpp_sum_integers ***
      !!          
      !! ** Purpose : Sum a single integer
      !!
      !! ** Method  : MPI all reduce.
      !!
      !! ** Action  : This does only work for MPI. 
      !!              It does not work for SHMEM.
      !!
      !! References : http://www.mpi-forum.org
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in   ) ::   kvalin
      INTEGER, INTENT(  out) ::   kvalout
      !
#if defined key_mpp_mpi
      !
      INTEGER :: ierr
      !
INCLUDE 'mpif.h'
      !!----------------------------------------------------------------------
      !
      !-----------------------------------------------------------------------
      ! Call the MPI library to find the sum across processors
      !-----------------------------------------------------------------------
      CALL mpi_allreduce( kvalin, kvalout, 1, mpi_integer,   &
         &                mpi_sum, mpi_comm_opa, ierr )
#else
      !-----------------------------------------------------------------------
      ! For no-MPP just return input values
      !-----------------------------------------------------------------------
      kvalout = kvalin
#endif
      !
   END SUBROUTINE obs_mpp_sum_integer


   SUBROUTINE mpp_global_max( pval )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE mpp_global_or ***
      !!          
      !! ** Purpose : Get the maximum value across processors for a global 
      !!              real array
      !!
      !! ** Method  : MPI allreduce
      !!
      !! ** Action  : This does only work for MPI. 
      !!              It does not work for SHMEM.
      !!
      !! References : http://www.mpi-forum.org
      !!----------------------------------------------------------------------
      REAL(KIND=wp), DIMENSION(jpiglo,jpjglo), INTENT(inout) ::   pval
      !
      INTEGER :: ierr
      !
#if defined key_mpp_mpi
      !
INCLUDE 'mpif.h'
      REAL(KIND=wp), DIMENSION(:,:), ALLOCATABLE ::   zcp
      !!----------------------------------------------------------------------

      ! Copy data for input to MPI

      ALLOCATE( &
         & zcp(jpiglo,jpjglo) &
         & )
      zcp(:,:) = pval(:,:)

      ! Call the MPI library to find the coast lines globally

      CALL mpi_allreduce( zcp, pval, jpiglo*jpjglo, mpivar, &
         &                mpi_max, mpi_comm_opa, ierr )

      DEALLOCATE( &
         & zcp &
         & )

#else
      ! no MPI: empty routine
#endif
      !
   END SUBROUTINE mpp_global_max


   SUBROUTINE mpp_alltoall_int( kno, kvalsin, kvalsout )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE mpp_allgatherv ***
      !!          
      !! ** Purpose : all to all.
      !!
      !! ** Method  : MPI alltoall
      !!
      !! ** Action  : This does only work for MPI. 
      !!              It does not work for SHMEM.
      !!
      !! References : http://www.mpi-forum.org
      !!----------------------------------------------------------------------
      INTEGER                      , INTENT(in   ) ::   kno
      INTEGER, DIMENSION(kno*jpnij), INTENT(in   ) ::   kvalsin
      INTEGER, DIMENSION(kno*jpnij), INTENT(  out) ::   kvalsout
      !!
      INTEGER :: ierr
      !
#if defined key_mpp_mpi
      !
INCLUDE 'mpif.h'
      !-----------------------------------------------------------------------
      ! Call the MPI library to do the all to all operation of the data
      !-----------------------------------------------------------------------
      CALL mpi_alltoall( kvalsin,  kno, mpi_integer, &
         &               kvalsout, kno, mpi_integer, &
         &               mpi_comm_opa, ierr )
#else
      !-----------------------------------------------------------------------
      ! For no-MPP just return input values
      !-----------------------------------------------------------------------
      kvalsout = kvalsin
#endif
      !
   END SUBROUTINE mpp_alltoall_int


   SUBROUTINE mpp_alltoallv_int( kvalsin, knoin , kinv , kvalsout,   &
      &                                   knoout, koutv )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE mpp_alltoallv_int ***
      !!          
      !! ** Purpose : all to all (integer version).
      !!
      !! ** Method  : MPI alltoall
      !!
      !! ** Action  : This does only work for MPI. 
      !!              It does not work for SHMEM.
      !!
      !! References : http://www.mpi-forum.org
      !!----------------------------------------------------------------------
      INTEGER                   , INTENT(in) :: knoin
      INTEGER                   , INTENT(in) :: knoout
      INTEGER, DIMENSION(jpnij)                 ::   kinv, koutv
      INTEGER, DIMENSION(knoin) , INTENT(in   ) ::   kvalsin
      INTEGER, DIMENSION(knoout), INTENT(  out) ::   kvalsout
      !!
      INTEGER :: ierr
      INTEGER :: jproc
      !
#if defined key_mpp_mpi
      !
INCLUDE 'mpif.h'
      INTEGER, DIMENSION(jpnij) ::   irdsp, isdsp
      !-----------------------------------------------------------------------
      ! Compute displacements
      !-----------------------------------------------------------------------
      irdsp(1) = 0
      isdsp(1) = 0
      DO jproc = 2, jpnij
         isdsp(jproc) = isdsp(jproc-1) + kinv(jproc-1)
         irdsp(jproc) = irdsp(jproc-1) + koutv(jproc-1)
      END DO
      !-----------------------------------------------------------------------
      ! Call the MPI library to do the all to all operation of the data
      !-----------------------------------------------------------------------
      CALL mpi_alltoallv( kvalsin,  kinv,  isdsp, mpi_integer, &
         &                kvalsout, koutv, irdsp, mpi_integer, &
         &                mpi_comm_opa, ierr )
#else
      !-----------------------------------------------------------------------
      ! For no-MPP just return input values
      !-----------------------------------------------------------------------
      kvalsout = kvalsin
#endif
      !
   END SUBROUTINE mpp_alltoallv_int


   SUBROUTINE mpp_alltoallv_real( pvalsin, knoin , kinv , pvalsout,   &
      &                                    knoout, koutv )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE mpp_alltoallv_real ***
      !!          
      !! ** Purpose : all to all (integer version).
      !!
      !! ** Method  : MPI alltoall
      !!
      !! ** Action  : This does only work for MPI. 
      !!              It does not work for SHMEM.
      !!
      !! References : http://www.mpi-forum.org
      !!----------------------------------------------------------------------
      INTEGER                    , INTENT(in   ) :: knoin
      INTEGER                    , INTENT(in   ) :: knoout
      INTEGER , DIMENSION(jpnij)                 ::   kinv, koutv
      REAL(wp), DIMENSION(knoin) , INTENT(in   ) ::   pvalsin
      REAL(wp), DIMENSION(knoout), INTENT(  out) ::   pvalsout
      !!
      INTEGER :: ierr
      INTEGER :: jproc
      !
#if defined key_mpp_mpi
      !
INCLUDE 'mpif.h'
      INTEGER, DIMENSION(jpnij) ::   irdsp, isdsp
      !!----------------------------------------------------------------------
      !
      !-----------------------------------------------------------------------
      ! Compute displacements
      !-----------------------------------------------------------------------
      irdsp(1) = 0
      isdsp(1) = 0
      DO jproc = 2, jpnij
         isdsp(jproc) = isdsp(jproc-1) + kinv(jproc-1)
         irdsp(jproc) = irdsp(jproc-1) + koutv(jproc-1)
      END DO
      !-----------------------------------------------------------------------
      ! Call the MPI library to do the all to all operation of the data
      !-----------------------------------------------------------------------
      CALL mpi_alltoallv( pvalsin,  kinv,  isdsp, mpivar, &
         &                pvalsout, koutv, irdsp, mpivar, &
         &                mpi_comm_opa, ierr )
#else
      !-----------------------------------------------------------------------
      ! For no-MPP just return input values
      !-----------------------------------------------------------------------
      pvalsout = pvalsin
#endif
      !
   END SUBROUTINE mpp_alltoallv_real

   !!======================================================================
END MODULE obs_mpp
