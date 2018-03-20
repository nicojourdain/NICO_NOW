MODULE sedbtb
#if defined key_sed
   !!======================================================================
   !!              ***  MODULE  sedbtb  ***
   !!    Sediment : bioturbation of the solid components
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sedmat  ! linear system of equations

   PUBLIC sed_btb


CONTAINS
   
   SUBROUTINE sed_btb( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sed_btb  ***
      !!
      !! ** Purpose :  performs bioturbation of the solid sediment components
      !!
      !! ** Method  :  ``diffusion'' of solid sediment components. 
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) F90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------
      !!* Arguments
      INTEGER, INTENT(in) ::  kt              ! time step

      ! * local variables
      INTEGER :: ji, jk, js
      REAL(wp), DIMENSION(:,:,:) , ALLOCATABLE ::  zsol  !   solution
      !------------------------------------------------------------------------

      IF( kt == nitsed000 ) THEN
         WRITE(numsed,*) ' sed_btb : Bioturbation  '
         WRITE(numsed,*) ' '
      ENDIF

      ! Initializations
      !----------------
      ALLOCATE( zsol(jpoce,jpksedm1,jpsol) )

      zsol(:,:,:) = 0.


      ! right hand side of coefficient matrix
      !--------------------------------------
      DO js = 1, jpsol
         DO jk = 1, jpksedm1
            DO ji = 1, jpoce
               zsol(ji,jk,js) = solcp(ji,jk+1,js)
            ENDDO
         ENDDO
      ENDDO

      CALL sed_mat( jpsol, jpoce, jpksedm1, zsol )


      ! store solution of the tridiagonal system
      !------------------------
      DO js = 1, jpsol
         DO jk = 1, jpksedm1
            DO ji = 1, jpoce
               solcp(ji,jk+1,js) = zsol(ji,jk,js)
            ENDDO
         ENDDO
      ENDDO
     
      DEALLOCATE( zsol )

   END SUBROUTINE sed_btb
#else
   !!======================================================================
   !! MODULE sedbtb  :   Dummy module 
   !!======================================================================
CONTAINS
   SUBROUTINE sed_btb( kt )         ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'sed_btb: You should not have seen this print! error?', kt
   END SUBROUTINE sed_btb

   !!======================================================================

#endif
END MODULE sedbtb
