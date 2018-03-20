MODULE sedmat
#if defined key_sed
   !!======================================================================
   !!              ***  MODULE  sedmat  ***
   !!    Sediment : linear system of equations
   !!=====================================================================
   !! * Modules used
   !!----------------------------------------------------------------------

   USE sed     ! sediment global variable

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_mat 

   INTERFACE sed_mat
      MODULE PROCEDURE sed_mat_dsr, sed_mat_btb
   END INTERFACE

   INTEGER, PARAMETER :: nmax = 30 


 CONTAINS

    SUBROUTINE sed_mat_dsr( nvar, ndim, nlev, preac, psol )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_dsr  ***
       !!
       !! ** Purpose :  solves tridiagonal system of linear equations 
       !!
       !! ** Method  : 
       !!        1 - computes left hand side of linear system of equations
       !!            for dissolution reaction
       !!         For mass balance in kbot+sediment :
       !!              dz3d  (:,1) = dz(1) = 0.5 cm
       !!              volw3d(:,1) = dzkbot ( see sedini.F90 ) 
       !!              dz(2)       = 0.3 cm 
       !!              dz3d(:,2)   = 0.3 + dzdep   ( see seddsr.F90 )     
       !!              volw3d(:,2) and vols3d(l,2) are thickened ( see seddsr.F90 ) 
       !!
       !!         2 - forward/backward substitution. 
       !!
       !!   History :
       !!        !  04-10 (N. Emprin, M. Gehlen ) original
       !!        !  06-04 (C. Ethe)  Module Re-organization
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) ::  nvar  ! number of variables
       INTEGER , INTENT(in) ::  ndim  ! number of points
       INTEGER , INTENT(in) ::  nlev  ! number of sediment levels

       REAL(wp), DIMENSION(ndim,nlev,nvar), INTENT(in   ) :: preac  ! reaction rates
       REAL(wp), DIMENSION(ndim,nlev,nvar), INTENT(inout) :: psol   ! solution ( undersaturation values )
 
       !---Local declarations
       INTEGER  ::  ji, jk, jn
       REAL(wp), DIMENSION(ndim,nlev) :: za, zb, zc, zr
       REAL(wp), DIMENSION(ndim)      :: zbet
       REAL(wp), DIMENSION(ndim,nmax) :: zgamm

       REAL(wp) ::  aplus,aminus  
       REAL(wp) ::  rplus,rminus   
       REAL(wp) ::  dxplus,dxminus

       !----------------------------------------------------------------------


       ! Computation left hand side of linear system of 
       ! equations for dissolution reaction
       !---------------------------------------------


       ! first sediment level          
       DO ji = 1, ndim
          aplus  = ( ( volw3d(ji,1) / dz3d(ji,1) ) + &
                     ( volw3d(ji,2) / dz3d(ji,2) ) ) / 2.
          dxplus = ( dz3d(ji,1) + dz3d(ji,2) ) / 2.
          rplus  = ( dtsed / volw3d(ji,1) ) * diff(1) * aplus / dxplus 

          za(ji,1) = 0.
          zb(ji,1) = 1. + rplus
          zc(ji,1) = -rplus
       ENDDO
 
       DO jk = 2, nlev - 1
          DO ji = 1, ndim
             aminus  = ( ( volw3d(ji,jk-1) / dz3d(ji,jk-1) ) + &
                &        ( volw3d(ji,jk  ) / dz3d(ji,jk  ) ) ) / 2.
             dxminus = ( dz3d(ji,jk-1) + dz3d(ji,jk) ) / 2.

             aplus   = ( ( volw3d(ji,jk  ) / dz3d(ji,jk  ) ) + &
                &        ( volw3d(ji,jk+1) / dz3d(ji,jk+1) ) ) / 2.
             dxplus  = ( dz3d(ji,jk) + dz3d(ji,jk+1) ) / 2
             !
             rminus  = ( dtsed / volw3d(ji,jk) ) * diff(jk-1) * aminus / dxminus
             rplus   = ( dtsed / volw3d(ji,jk) ) * diff(jk)   * aplus / dxplus
             !     
             za(ji,jk) = -rminus
             zb(ji,jk) = 1. + rminus + rplus 
             zc(ji,jk) = -rplus
          END DO
       END DO

       DO ji = 1, ndim
          aminus  = ( ( volw3d(ji,nlev-1) / dz3d(ji,nlev-1) ) + &
             &        ( volw3d(ji,nlev)  / dz3d(ji,nlev) ) ) / 2.
          dxminus = ( dz3d(ji,nlev-1) + dz3d(ji,nlev) ) / 2.
          rminus  = ( dtsed / volw3d(ji,nlev) ) * diff(nlev-1) * aminus / dxminus
          !
          za(ji,nlev) = -rminus
          zb(ji,nlev) = 1. + rminus
          zc(ji,nlev) = 0.
       END DO


       ! solves tridiagonal system of linear equations 
       ! -----------------------------------------------

       DO jn = 1, nvar

          zr  (:,:)    = psol(:,:,jn)
          zbet(:  )    = zb(:,1) + preac(:,1,jn)
          psol(:,1,jn) = zr(:,1) / zbet(:)
          ! 
          DO jk = 2, nlev
             DO ji = 1, ndim
                zgamm(ji,jk)   =  zc(ji,jk-1) / zbet(ji)
                zbet(ji)       = ( zb(ji,jk) + preac(ji,jk,jn) ) - za(ji,jk) * zgamm(ji,jk)
                psol(ji,jk,jn) = ( zr(ji,jk) - za(ji,jk) * psol(ji,jk-1,jn) ) / zbet(ji)
             END DO
          ENDDO
          ! 
          DO jk = nlev - 1, 1, -1
             DO ji = 1,ndim
                psol(ji,jk,jn) = psol(ji,jk,jn) - zgamm(ji,jk+1) * psol(ji,jk+1,jn)
             END DO
          ENDDO

       ENDDO


    END SUBROUTINE sed_mat_dsr

    
    SUBROUTINE sed_mat_btb( nvar, ndim, nlev, psol )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_btb  ***
       !!
       !! ** Purpose :  solves tridiagonal system of linear equations 
       !!
       !! ** Method  : 
       !!        1 - computes left hand side of linear system of equations
       !!            for dissolution reaction
       !!
       !!         2 - forward/backward substitution. 
       !!
       !!   History :
       !!        !  04-10 (N. Emprin, M. Gehlen ) original
       !!        !  06-04 (C. Ethe)  Module Re-organization
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) :: &
          nvar , &  ! number of variables
          ndim , &  ! number of points
          nlev      ! number of sediment levels

      REAL(wp), DIMENSION(ndim,nlev,nvar), INTENT(inout) :: &
          psol      ! solution

       !---Local declarations
       INTEGER  ::  &
          ji, jk, jn

       REAL(wp) ::  &
          aplus,aminus   ,  & 
          rplus,rminus   ,  &
          dxplus,dxminus 

       REAL(wp), DIMENSION(nlev)      :: za, zb, zc
       REAL(wp), DIMENSION(ndim,nlev) :: zr
       REAL(wp), DIMENSION(nmax)      :: zgamm 
       REAL(wp) ::  zbet

 
       !----------------------------------------------------------------------

       ! Computation left hand side of linear system of 
       ! equations for dissolution reaction
       !---------------------------------------------


       ! first sediment level          
       aplus  = ( ( vols(2) / dz(2) ) + ( vols(3) / dz(3) ) ) / 2.
       dxplus = ( dz(2) + dz(3) ) / 2.
       rplus  = ( dtsed / vols(2) ) * db * aplus / dxplus 

       za(1) = 0.
       zb(1) = 1. + rplus
       zc(1) = -rplus

             
       DO jk = 2, nlev - 1
          aminus  = ( ( vols(jk) / dz(jk) ) + ( vols(jk+1) / dz(jk+1) ) ) / 2.
          dxminus = ( dz(jk) + dz(jk+1) ) / 2.
          rminus  = ( dtsed / vols(jk+1) ) * db * aminus / dxminus
          !
          aplus   = ( ( vols(jk+1) / dz(jk+1  ) ) + ( vols(jk+2) / dz(jk+2) ) ) / 2.
          dxplus  = ( dz(jk+1) + dz(jk+2) ) / 2.
          rplus   = ( dtsed / vols(jk+1) ) * db * aplus / dxplus
          !     
          za(jk) = -rminus
          zb(jk) = 1. + rminus + rplus 
          zc(jk) = -rplus
       ENDDO
 
       aminus  = ( ( vols(nlev) / dz(nlev) ) + ( vols(nlev+1) / dz(nlev+1) ) ) / 2.
       dxminus = ( dz(nlev) + dz(nlev+1) ) / 2.
       rminus  = ( dtsed / vols(nlev+1) ) * db * aminus / dxminus
       !
 
       za(nlev) = -rminus
       zb(nlev) = 1. + rminus
       zc(nlev) = 0.


       ! solves tridiagonal system of linear equations 
       ! -----------------------------------------------    
       DO jn = 1, nvar
          
          zr  (:,:)    = psol(:,:,jn)
          zbet         = zb(1)
          psol(:,1,jn) = zr(:,1) / zbet
          ! 
          DO jk = 2, nlev
             zgamm(jk) =  zc(jk-1) / zbet
             zbet      =  zb(jk) - za(jk) * zgamm(jk)
             DO ji = 1, ndim
                psol(ji,jk,jn) = ( zr(ji,jk) - za(jk) * psol(ji,jk-1,jn) ) / zbet
             END DO
          ENDDO
          ! 
          DO jk = nlev - 1, 1, -1
             DO ji = 1,ndim
                psol(ji,jk,jn) = psol(ji,jk,jn) - zgamm(jk+1) * psol(ji,jk+1,jn)
             END DO
          ENDDO

       ENDDO

       
    END SUBROUTINE sed_mat_btb


#else
   !!======================================================================
   !! MODULE sedmat  :   Dummy module 
   !!======================================================================
CONTAINS
   SUBROUTINE sed_mat         ! Empty routine
   END SUBROUTINE sed_mat
   !!======================================================================
#endif

 END MODULE sedmat
