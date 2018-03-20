MODULE dommsk
   !!======================================================================
   !!                       ***  MODULE dommsk   ***
   !! Ocean initialization : domain land/sea masks, off-line case 
   !!======================================================================
   !! History :  3.3  ! 2010-10  (C. Ethe)  adapted from OPA_SRC/DOM/dommsk
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_msk        : compute land/ocean mask
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE lib_mpp         ! MPP library
   USE in_out_manager  ! I/O manager
   USE wrk_nemo  

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_msk    ! routine called by inidom.F90

   REAL(wp)        :: rn_shlat   = 2.   ! type of lateral boundary condition on velocity
   LOGICAL, PUBLIC :: ln_vorlat  = .false.   !  consistency of vorticity boundary condition 

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OFF 3.3 , NEMO Consortium (2010)
   !! $Id: dommsk.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_msk
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE dom_msk  ***
      !!
      !! ** Purpose :   Off-line case: defines the interior domain T-mask.
      !!
      !! ** Method  :   The interior ocean/land mask is computed from tmask
      !!              setting to zero the duplicated row and lines due to
      !!              MPP exchange halos, est-west cyclic and north fold
      !!              boundary conditions.
      !!
      !! ** Action :   tmask_i  : interiorland/ocean mask at t-point
      !!               tpol     : ???
      !!----------------------------------------------------------------------
      !
      INTEGER  ::   ji, jk                   ! dummy loop indices
      INTEGER  ::   iif, iil, ijf, ijl       ! local integers
      INTEGER, POINTER, DIMENSION(:,:) ::  imsk 
      !
      !!---------------------------------------------------------------------
      
      CALL wrk_alloc( jpi, jpj, imsk )
      !
      ! Interior domain mask (used for global sum)
      ! --------------------
      tmask_i(:,:) = tmask(:,:,1)
      iif = jpreci                        ! thickness of exchange halos in i-axis
      iil = nlci - jpreci + 1
      ijf = jprecj                        ! thickness of exchange halos in j-axis
      ijl = nlcj - jprecj + 1
      !
      tmask_i( 1 :iif,   :   ) = 0._wp    ! first columns
      tmask_i(iil:jpi,   :   ) = 0._wp    ! last  columns (including mpp extra columns)
      tmask_i(   :   , 1 :ijf) = 0._wp    ! first rows
      tmask_i(   :   ,ijl:jpj) = 0._wp    ! last  rows (including mpp extra rows)
      !
      !                                   ! north fold mask
      tpol(1:jpiglo) = 1._wp
      !                                
      IF( jperio == 3 .OR. jperio == 4 )   tpol(jpiglo/2+1:jpiglo) = 0._wp    ! T-point pivot
      IF( jperio == 5 .OR. jperio == 6 )   tpol(     1    :jpiglo) = 0._wp    ! F-point pivot
      IF( jperio == 3 .OR. jperio == 4 ) THEN      ! T-point pivot: only half of the nlcj-1 row
         IF( mjg(ijl-1) == jpjglo-1 ) THEN
            DO ji = iif+1, iil-1
               tmask_i(ji,ijl-1) = tmask_i(ji,ijl-1) * tpol(mig(ji))
            END DO
         ENDIF
      ENDIF 
      !
      IF( nprint == 1 .AND. lwp ) THEN    ! Control print
         imsk(:,:) = INT( tmask_i(:,:) )
         WRITE(numout,*) ' tmask_i : '
         CALL prihin( imsk(:,:), jpi, jpj, 1, jpi, 1, 1, jpj, 1, 1, numout)
         WRITE (numout,*)
         WRITE (numout,*) ' dommsk: tmask for each level'
         WRITE (numout,*) ' ----------------------------'
         DO jk = 1, jpk
            imsk(:,:) = INT( tmask(:,:,jk) )
            WRITE(numout,*)
            WRITE(numout,*) ' level = ',jk
            CALL prihin( imsk(:,:), jpi, jpj, 1, jpi, 1, 1, jpj, 1, 1, numout)
         END DO
      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, imsk )
      !
   END SUBROUTINE dom_msk
   !!======================================================================
END MODULE dommsk
