MODULE lib_fortran
   !!======================================================================
   !!                       ***  MODULE  lib_fortran  ***
   !! Fortran utilities:  includes some low levels fortran functionality
   !!======================================================================
   !! History :  3.2  !  2010-05  (M. Dunphy, R. Benshila)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   glob_sum    : generic interface for global masked summation over 
   !!                 the interior domain for 1 or 2 2D or 3D arrays
   !!                 it works only for T points   
   !!   SIGN        : generic interface for SIGN to overwrite f95 behaviour
   !!                 of intrinsinc sign function
   !!----------------------------------------------------------------------
   USE par_oce          ! Ocean parameter
   USE lib_mpp          ! distributed memory computing
   USE dom_oce          ! ocean domain
   USE in_out_manager   ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC glob_sum
#if defined key_nosignedzero
   PUBLIC SIGN
#endif

   INTERFACE glob_sum
      MODULE PROCEDURE glob_sum_2d, glob_sum_3d,glob_sum_2d_a, glob_sum_3d_a 
   END INTERFACE

#if defined key_nosignedzero   
   INTERFACE SIGN
      MODULE PROCEDURE SIGN_SCALAR, SIGN_ARRAY_1D, SIGN_ARRAY_2D, SIGN_ARRAY_3D,   &
         &             SIGN_ARRAY_1D_A, SIGN_ARRAY_2D_A, SIGN_ARRAY_3D_A,          & 
         &             SIGN_ARRAY_1D_B, SIGN_ARRAY_2D_B, SIGN_ARRAY_3D_B 
   END INTERFACE
#endif

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: lib_fortran.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS 

#if ! defined key_mpp_rep
   FUNCTION glob_sum_2d( ptab ) 
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION  glob_sum_2D  ***
      !!
      !! ** Purpose : perform a masked sum on the inner global domain of a 2D array
      !!-----------------------------------------------------------------------
      REAL(wp), INTENT(in), DIMENSION(:,:) ::   ptab          ! input 2D array
      REAL(wp)                             ::   glob_sum_2d   ! global masked sum
      !!-----------------------------------------------------------------------
      !
      glob_sum_2d = SUM( ptab(:,:)*tmask_i(:,:) )
      IF( lk_mpp )   CALL mpp_sum( glob_sum_2d )
      !
   END FUNCTION glob_sum_2d
   
   
   FUNCTION glob_sum_3d( ptab ) 
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION  glob_sum_3D  ***
      !!
      !! ** Purpose : perform a masked sum on the inner global domain of a 3D array
      !!-----------------------------------------------------------------------
      REAL(wp), INTENT(in), DIMENSION(:,:,:) ::   ptab          ! input 3D array
      REAL(wp)                               ::   glob_sum_3d   ! global masked sum
      !!
      INTEGER :: jk
      !!-----------------------------------------------------------------------
      !
      glob_sum_3d = 0.e0
      DO jk = 1, jpk
         glob_sum_3d = glob_sum_3d + SUM( ptab(:,:,jk)*tmask_i(:,:) )
      END DO
      IF( lk_mpp )   CALL mpp_sum( glob_sum_3d )
      !
   END FUNCTION glob_sum_3d


   FUNCTION glob_sum_2d_a( ptab1, ptab2 ) 
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION  glob_sum_2D _a ***
      !!
      !! ** Purpose : perform a masked sum on the inner global domain of two 2D array
      !!-----------------------------------------------------------------------
      REAL(wp), INTENT(in), DIMENSION(:,:) ::   ptab1, ptab2    ! input 2D array
      REAL(wp)            , DIMENSION(2)   ::   glob_sum_2d_a   ! global masked sum
      !!-----------------------------------------------------------------------
      !             
      glob_sum_2d_a(1) = SUM( ptab1(:,:)*tmask_i(:,:) )
      glob_sum_2d_a(2) = SUM( ptab2(:,:)*tmask_i(:,:) )
      IF( lk_mpp )   CALL mpp_sum( glob_sum_2d_a, 2 )
      !
   END FUNCTION glob_sum_2d_a
 
 
   FUNCTION glob_sum_3d_a( ptab1, ptab2 ) 
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION  glob_sum_3D_a ***
      !!
      !! ** Purpose : perform a masked sum on the inner global domain of two 3D array
      !!-----------------------------------------------------------------------
      REAL(wp), INTENT(in), DIMENSION(:,:,:) ::   ptab1, ptab2    ! input 3D array
      REAL(wp)            , DIMENSION(2)     ::   glob_sum_3d_a   ! global masked sum
      !!
      INTEGER :: jk
      !!-----------------------------------------------------------------------
      !
      glob_sum_3d_a(:) = 0.e0
      DO jk = 1, jpk
         glob_sum_3d_a(1) = glob_sum_3d_a(1) + SUM( ptab1(:,:,jk)*tmask_i(:,:) )
         glob_sum_3d_a(2) = glob_sum_3d_a(2) + SUM( ptab2(:,:,jk)*tmask_i(:,:) )
      END DO
      IF( lk_mpp )   CALL mpp_sum( glob_sum_3d_a, 2 )
      !
   END FUNCTION glob_sum_3d_a

#else  
   !!----------------------------------------------------------------------
   !!   'key_mpp_rep'                                   MPP reproducibility
   !!----------------------------------------------------------------------
   
   FUNCTION glob_sum_2d( ptab ) 
      !!----------------------------------------------------------------------
      !!                  ***  FUNCTION  glob_sum_2d ***
      !!
      !! ** Purpose : perform a sum in calling DDPDD routine
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in), DIMENSION(jpi,jpj) ::   ptab
      REAL(wp)                                 ::   glob_sum_2d   ! global masked sum
      !!
      COMPLEX(wp)::   ctmp
      REAL(wp)   ::   ztmp
      INTEGER    ::   ji, jj   ! dummy loop indices
      !!-----------------------------------------------------------------------
      !
      ztmp = 0.e0
      ctmp = CMPLX( 0.e0, 0.e0, wp )
      DO jj = 1, jpj
         DO ji =1, jpi
         ztmp =  ptab(ji,jj) * tmask_i(ji,jj)
         CALL DDPDD( CMPLX( ztmp, 0.e0, wp ), ctmp )
         END DO
      END DO
      IF( lk_mpp )   CALL mpp_sum( ctmp )   ! sum over the global domain
      glob_sum_2d = REAL(ctmp,wp)
      !
   END FUNCTION glob_sum_2d   


   FUNCTION glob_sum_3d( ptab ) 
      !!----------------------------------------------------------------------
      !!                  ***  FUNCTION  glob_sum_3d ***
      !!
      !! ** Purpose : perform a sum on a 3D array in calling DDPDD routine
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in), DIMENSION(jpi,jpj,jpk) ::   ptab
      REAL(wp)                                     ::   glob_sum_3d   ! global masked sum
      !!
      COMPLEX(wp)::   ctmp
      REAL(wp)   ::   ztmp
      INTEGER    ::   ji, jj, jk   ! dummy loop indices
      !!-----------------------------------------------------------------------
      !
      ztmp = 0.e0
      ctmp = CMPLX( 0.e0, 0.e0, wp )
      DO jk = 1, jpk
         DO jj = 1, jpj
            DO ji =1, jpi
            ztmp =  ptab(ji,jj,jk) * tmask_i(ji,jj)
            CALL DDPDD( CMPLX( ztmp, 0.e0, wp ), ctmp )
            END DO
         END DO    
      END DO
      IF( lk_mpp )   CALL mpp_sum( ctmp )   ! sum over the global domain
      glob_sum_3d = REAL(ctmp,wp)
      !
   END FUNCTION glob_sum_3d   


   FUNCTION glob_sum_2d_a( ptab1, ptab2 ) 
      !!----------------------------------------------------------------------
      !!                  ***  FUNCTION  glob_sum_2d_a ***
      !!
      !! ** Purpose : perform a sum on two 2D arrays in calling DDPDD routine
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in), DIMENSION(jpi,jpj) ::   ptab1, ptab2
      REAL(wp)                                 ::   glob_sum_2d_a   ! global masked sum
      !!
      COMPLEX(wp)::   ctmp
      REAL(wp)   ::   ztmp
      INTEGER    ::   ji, jj   ! dummy loop indices
      !!-----------------------------------------------------------------------
      !
      ztmp = 0.e0
      ctmp = CMPLX( 0.e0, 0.e0, wp )
      DO jj = 1, jpj
         DO ji =1, jpi
         ztmp =  ptab1(ji,jj) * tmask_i(ji,jj)
         CALL DDPDD( CMPLX( ztmp, 0.e0, wp ), ctmp )
         ztmp =  ptab2(ji,jj) * tmask_i(ji,jj)
         CALL DDPDD( CMPLX( ztmp, 0.e0, wp ), ctmp )
         END DO
      END DO
      IF( lk_mpp )   CALL mpp_sum( ctmp )   ! sum over the global domain
      glob_sum_2d_a = REAL(ctmp,wp)
      !
   END FUNCTION glob_sum_2d_a   


   FUNCTION glob_sum_3d_a( ptab1, ptab2 ) 
      !!----------------------------------------------------------------------
      !!                  ***  FUNCTION  glob_sum_3d_a ***
      !!
      !! ** Purpose : perform a sum on two 3D array in calling DDPDD routine
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in), DIMENSION(jpi,jpj,jpk) ::   ptab1, ptab2
      REAL(wp)                                     ::   glob_sum_3d_a   ! global masked sum
      !!
      COMPLEX(wp)::   ctmp
      REAL(wp)   ::   ztmp
      INTEGER    ::   ji, jj, jk   ! dummy loop indices
      !!-----------------------------------------------------------------------
      !
      ztmp = 0.e0
      ctmp = CMPLX( 0.e0, 0.e0, wp )
      DO jk = 1, jpk
         DO jj = 1, jpj
            DO ji =1, jpi
            ztmp =  ptab1(ji,jj,jk) * tmask_i(ji,jj)
            CALL DDPDD( CMPLX( ztmp, 0.e0, wp ), ctmp )
            ztmp =  ptab2(ji,jj,jk) * tmask_i(ji,jj)
            CALL DDPDD( CMPLX( ztmp, 0.e0, wp ), ctmp )
            END DO
         END DO    
      END DO
      IF( lk_mpp )   CALL mpp_sum( ctmp )   ! sum over the global domain
      glob_sum_3d_a = REAL(ctmp,wp)
      !
   END FUNCTION glob_sum_3d_a   


   SUBROUTINE DDPDD( ydda, yddb )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE DDPDD ***
      !!          
      !! ** Purpose : Add a scalar element to a sum
      !!             
      !!
      !! ** Method  : The code uses the compensated summation with doublet 
      !!              (sum,error) emulated useing complex numbers. ydda is the
      !!               scalar to add to the summ yddb 
      !! 
      !! ** Action  : This does only work for MPI. 
      !!
      !! References : Using Acurate Arithmetics to Improve Numerical
      !!              Reproducibility and Sability in Parallel Applications
      !!              Yun HE and Chris H. Q. DING, Journal of Supercomputing 18, 259-277, 2001 
      !!----------------------------------------------------------------------
      COMPLEX(wp), INTENT(in   ) ::   ydda
      COMPLEX(wp), INTENT(inout) ::   yddb
      !
      REAL(wp) :: zerr, zt1, zt2  ! local work variables
      !!-----------------------------------------------------------------------
      !
      ! Compute ydda + yddb using Knuth's trick.
      zt1  = REAL(ydda) + REAL(yddb)
      zerr = zt1 - REAL(ydda)
      zt2  = ( (REAL(yddb) - zerr) + (REAL(ydda) - (zt1 - zerr)) )   &
         &   + AIMAG(ydda)         + AIMAG(yddb)
      !
      ! The result is t1 + t2, after normalization.
      yddb = CMPLX( zt1 + zt2, zt2 - ((zt1 + zt2) - zt1), wp )
      !
   END SUBROUTINE DDPDD
#endif

#if defined key_nosignedzero
   !!----------------------------------------------------------------------
   !!   'key_nosignedzero'                                         F90 SIGN
   !!----------------------------------------------------------------------
   
   FUNCTION SIGN_SCALAR( pa, pb )
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_SCALAR  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa,pb          ! input
      REAL(wp) :: SIGN_SCALAR    ! result
      !!-----------------------------------------------------------------------
      IF ( pb >= 0.e0) THEN   ;   SIGN_SCALAR = ABS(pa)
      ELSE                    ;   SIGN_SCALAR =-ABS(pa)
      ENDIF
   END FUNCTION SIGN_SCALAR


   FUNCTION SIGN_ARRAY_1D( pa, pb ) 
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_1D  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa,pb(:)                   ! input
      REAL(wp) :: SIGN_ARRAY_1D(SIZE(pb,1))  ! result
      !!-----------------------------------------------------------------------
      WHERE ( pb >= 0.e0 )   ;   SIGN_ARRAY_1D = ABS(pa)
      ELSEWHERE              ;   SIGN_ARRAY_1D =-ABS(pa)
      END WHERE
   END FUNCTION SIGN_ARRAY_1D


   FUNCTION SIGN_ARRAY_2D(pa,pb) 
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_2D  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa,pb(:,:)      ! input
      REAL(wp) :: SIGN_ARRAY_2D(SIZE(pb,1),SIZE(pb,2))  ! result
      !!-----------------------------------------------------------------------
      WHERE ( pb >= 0.e0 )   ;   SIGN_ARRAY_2D = ABS(pa)
      ELSEWHERE              ;   SIGN_ARRAY_2D =-ABS(pa)
      END WHERE
   END FUNCTION SIGN_ARRAY_2D

   FUNCTION SIGN_ARRAY_3D(pa,pb) 
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_3D  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa,pb(:,:,:)      ! input
      REAL(wp) :: SIGN_ARRAY_3D(SIZE(pb,1),SIZE(pb,2),SIZE(pb,3))  ! result
      !!-----------------------------------------------------------------------
      WHERE ( pb >= 0.e0 )   ;   SIGN_ARRAY_3D = ABS(pa)
      ELSEWHERE              ;   SIGN_ARRAY_3D =-ABS(pa)
      END WHERE
   END FUNCTION SIGN_ARRAY_3D


   FUNCTION SIGN_ARRAY_1D_A(pa,pb) 
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_1D_A  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa(:),pb(:)      ! input
      REAL(wp) :: SIGN_ARRAY_1D_A(SIZE(pb,1))  ! result
      !!-----------------------------------------------------------------------
      WHERE ( pb >= 0.e0 )   ;   SIGN_ARRAY_1D_A = ABS(pa)
      ELSEWHERE              ;   SIGN_ARRAY_1D_A =-ABS(pa)
      END WHERE
   END FUNCTION SIGN_ARRAY_1D_A


   FUNCTION SIGN_ARRAY_2D_A(pa,pb) 
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_2D_A  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa(:,:),pb(:,:)      ! input
      REAL(wp) :: SIGN_ARRAY_2D_A(SIZE(pb,1),SIZE(pb,2))  ! result
      !!-----------------------------------------------------------------------
      WHERE ( pb >= 0.e0 )   ;   SIGN_ARRAY_2D_A = ABS(pa)
      ELSEWHERE              ;   SIGN_ARRAY_2D_A =-ABS(pa)
      END WHERE
   END FUNCTION SIGN_ARRAY_2D_A


   FUNCTION SIGN_ARRAY_3D_A(pa,pb) 
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_3D_A  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa(:,:,:),pb(:,:,:)  ! input
      REAL(wp) :: SIGN_ARRAY_3D_A(SIZE(pb,1),SIZE(pb,2),SIZE(pb,3)) ! result
      !!-----------------------------------------------------------------------
      WHERE ( pb >= 0.e0 )   ;   SIGN_ARRAY_3D_A = ABS(pa)
      ELSEWHERE              ;   SIGN_ARRAY_3D_A =-ABS(pa)
      END WHERE
   END FUNCTION SIGN_ARRAY_3D_A


   FUNCTION SIGN_ARRAY_1D_B(pa,pb) 
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_1D_B  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa(:),pb      ! input
      REAL(wp) :: SIGN_ARRAY_1D_B(SIZE(pa,1))  ! result
      !!-----------------------------------------------------------------------
      IF( pb >= 0.e0 ) THEN   ;   SIGN_ARRAY_1D_B = ABS(pa)
      ELSE                    ;   SIGN_ARRAY_1D_B =-ABS(pa)
      ENDIF
   END FUNCTION SIGN_ARRAY_1D_B


   FUNCTION SIGN_ARRAY_2D_B(pa,pb) 
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_2D_B  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa(:,:),pb      ! input
      REAL(wp) :: SIGN_ARRAY_2D_B(SIZE(pa,1),SIZE(pa,2))  ! result
      !!-----------------------------------------------------------------------
      IF( pb >= 0.e0 ) THEN   ;   SIGN_ARRAY_2D_B = ABS(pa)
      ELSE                    ;   SIGN_ARRAY_2D_B =-ABS(pa)
      ENDIF
   END FUNCTION SIGN_ARRAY_2D_B


   FUNCTION SIGN_ARRAY_3D_B(pa,pb) 
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION SIGN_ARRAY_3D_B  ***
      !!
      !! ** Purpose : overwrite f95 behaviour of intrinsinc sign function
      !!-----------------------------------------------------------------------
      REAL(wp) :: pa(:,:,:),pb      ! input
      REAL(wp) :: SIGN_ARRAY_3D_B(SIZE(pa,1),SIZE(pa,2),SIZE(pa,3))  ! result
      !!-----------------------------------------------------------------------
      IF( pb >= 0.e0 ) THEN   ;   SIGN_ARRAY_3D_B = ABS(pa)
      ELSE                    ;   SIGN_ARRAY_3D_B =-ABS(pa)
      ENDIF
   END FUNCTION SIGN_ARRAY_3D_B
#endif

   !!======================================================================
END MODULE lib_fortran
