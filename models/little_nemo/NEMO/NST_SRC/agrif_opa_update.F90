#define TWO_WAY

MODULE agrif_opa_update
#if defined key_agrif  && ! defined key_offline
   USE par_oce
   USE oce
   USE dom_oce
   USE agrif_oce
   USE in_out_manager  ! I/O manager
   USE lib_mpp
   USE wrk_nemo  
   USE zdf_oce        ! vertical physics: ocean variables

   IMPLICIT NONE
   PRIVATE

   PUBLIC Agrif_Update_Tra, Agrif_Update_Dyn
   PUBLIC Agrif_Update_Tke

   INTEGER, PUBLIC :: nbcline = 0

   !!----------------------------------------------------------------------
   !! NEMO/NST 3.3 , NEMO Consortium (2010)
   !! $Id: agrif_opa_update.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE Agrif_Update_Tra( kt )
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Update_Tra ***
      !!---------------------------------------------
      !!
      INTEGER, INTENT(in) :: kt
      REAL(wp), POINTER, DIMENSION(:,:) :: ztab2d
      REAL(wp), POINTER, DIMENSION(:,:,:,:) :: ztab

      IF( kt == nit000 ) THEN
         CALL wrk_alloc( jpi, jpj, ztab2d )
         CALL Agrif_Update_Variable(ztab2d,glamt_id, procname= updateglamT)   ! check that updating glamt has not impact
         CALL Agrif_Update_Variable(ztab2d,gphit_id, procname= updategphiT)   ! check that updating gphit has not impact
         CALL wrk_dealloc( jpi, jpj, ztab2d )
      ENDIF
       
      IF((Agrif_NbStepint() .NE. (Agrif_irhot()-1)).AND.(kt /= 0)) RETURN
#if defined TWO_WAY
      CALL wrk_alloc( jpi, jpj, jpk, jpts, ztab )


      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid = 0.

      IF (MOD(nbcline,nbclineupdate) == 0) THEN
         CALL Agrif_Update_Variable(ztab,tsn_id, procname=updateTS)
      ELSE
         CALL Agrif_Update_Variable(ztab,tsn_id,locupdate=(/0,2/), procname=updateTS)
      ENDIF

      Agrif_UseSpecialValueInUpdate = .FALSE.

      CALL wrk_dealloc( jpi, jpj, jpk, jpts, ztab )
#endif

   END SUBROUTINE Agrif_Update_Tra

   SUBROUTINE Agrif_Update_Dyn( kt )
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Update_Dyn ***
      !!---------------------------------------------
      !!
      INTEGER, INTENT(in) :: kt
      REAL(wp), POINTER, DIMENSION(:,:) :: ztab2d
      REAL(wp), POINTER, DIMENSION(:,:,:) :: ztab


      IF ((Agrif_NbStepint() .NE. (Agrif_irhot()-1)).AND.(kt /= 0)) Return
#if defined TWO_WAY
      CALL wrk_alloc( jpi, jpj,      ztab2d )
      CALL wrk_alloc( jpi, jpj, jpk, ztab   )

      IF (mod(nbcline,nbclineupdate) == 0) THEN
         CALL Agrif_Update_Variable(ztab,un_id,procname = updateU)
         CALL Agrif_Update_Variable(ztab,vn_id,procname = updateV)
      ELSE
         CALL Agrif_Update_Variable(ztab,un_id,locupdate=(/0,1/),procname = updateU)
         CALL Agrif_Update_Variable(ztab,vn_id,locupdate=(/0,1/),procname = updateV)         
      ENDIF

      CALL Agrif_Update_Variable(ztab2d,e1u_id,procname = updateU2d)
      CALL Agrif_Update_Variable(ztab2d,e2v_id,procname = updateV2d)  

      nbcline = nbcline + 1

      Agrif_UseSpecialValueInUpdate = ln_spc_dyn
      Agrif_SpecialValueFineGrid = 0.
      CALL Agrif_Update_Variable(ztab2d,sshn_id,procname = updateSSH)
      Agrif_UseSpecialValueInUpdate = .FALSE.

      CALL wrk_dealloc( jpi, jpj,      ztab2d )
      CALL wrk_dealloc( jpi, jpj, jpk, ztab   )

!Done in step
!      CALL Agrif_ChildGrid_To_ParentGrid()
!      CALL recompute_diags( kt )
!      CALL Agrif_ParentGrid_To_ChildGrid()

#endif

   END SUBROUTINE Agrif_Update_Dyn


   SUBROUTINE Agrif_Update_Tke( kt )
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Update_Tke ***
      !!---------------------------------------------
      !!
      INTEGER, INTENT(in) :: kt
      REAL(wp), POINTER, DIMENSION(:,:,:) :: ztab

       
      IF((Agrif_NbStepint() .NE. (Agrif_irhot()-1)).AND.(kt /= 0)) RETURN
#if defined TWO_WAY
      CALL wrk_alloc( jpi, jpj, jpk, ztab )

      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid = 0.

      CALL Agrif_Update_Variable(ztab,avt_id ,locupdate=(/0,0/), procname=updateAVT )
      CALL Agrif_Update_Variable(ztab,avm_id ,locupdate=(/0,0/), procname=updateAVM )
      CALL Agrif_Update_Variable(ztab,avmu_id,locupdate=(/0,0/), procname=updateAVMu)
      CALL Agrif_Update_Variable(ztab,avmv_id,locupdate=(/0,0/), procname=updateAVMv)

      Agrif_UseSpecialValueInUpdate = .FALSE.

      CALL wrk_dealloc( jpi, jpj, jpk, ztab )
#endif
      
   END SUBROUTINE Agrif_Update_Tke


   SUBROUTINE recompute_diags( kt )
      !!---------------------------------------------
      !!   *** ROUTINE recompute_diags ***
      !!---------------------------------------------
      INTEGER, INTENT(in) :: kt

   END SUBROUTINE recompute_diags

   SUBROUTINE updateTS( tabres, i1, i2, j1, j2, k1, k2, n1, n2, before )
      !!---------------------------------------------
      !!           *** ROUTINE updateT ***
      !!---------------------------------------------

      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,n1,n2
      REAL(wp),DIMENSION(i1:i2,j1:j2,k1:k2,n1:n2), INTENT(inout) :: tabres
      LOGICAL, iNTENT(in) :: before

      INTEGER :: ji,jj,jk,jn
      REAL(wp):: ztemp

      IF (before) THEN
         DO jn = n1,n2
            DO jk=k1,k2
               DO jj=j1,j2
                  DO ji=i1,i2
                     tabres(ji,jj,jk,jn) = tsn(ji,jj,jk,jn)
                  END DO
               END DO
            END DO
         END DO
      ELSE
         DO jn = n1,n2
            DO jk=k1,k2
               DO jj=j1,j2
                  DO ji=i1,i2
                     IF( tabres(ji,jj,jk,jn) .NE. 0. ) THEN 
                         ztemp = tsn(ji,jj,jk,jn)
                         tsn(ji,jj,jk,jn) = tabres(ji,jj,jk,jn) * tmask(ji,jj,jk)
                         tsb(ji,jj,jk,jn) = tsb(ji,jj,jk,jn) + atfp * ( tsn(ji,jj,jk,jn) - ztemp )
                     END IF
                  END DO
               END DO
            END DO
         END DO
      ENDIF

   END SUBROUTINE updateTS

   SUBROUTINE updateu( tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** ROUTINE updateu ***
      !!---------------------------------------------
#  include "domzgr_substitute.h90"

      INTEGER, INTENT(in) :: i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before

      INTEGER :: ji, jj, jk
      REAL(wp) :: zrhoy, ztemp

      IF (before) THEN
         zrhoy = Agrif_Rhoy()
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk) = e2u(ji,jj) * un(ji,jj,jk) * fse3u(ji,jj,jk)
               END DO
            END DO
         END DO
         tabres = zrhoy * tabres
      ELSE
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  ztemp        = un(ji,jj,jk) 
                  un(ji,jj,jk) = tabres(ji,jj,jk) / (e2u(ji,jj)*fse3u(ji,jj,jk)) * umask(ji,jj,jk)
                  ub(ji,jj,jk) = ub(ji,jj,jk) + atfp * ( un(ji,jj,jk) - ztemp )
               END DO
            END DO
         END DO
      ENDIF

   END SUBROUTINE updateu

   SUBROUTINE updatev( tabres, i1, i2, j1, j2, k1, k2, before )
      !!---------------------------------------------
      !!           *** ROUTINE updatev ***
      !!---------------------------------------------
#  include "domzgr_substitute.h90"

      INTEGER :: i1,i2,j1,j2,k1,k2
      INTEGER :: ji,jj,jk
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2) :: tabres
      LOGICAL :: before

      REAL(wp) :: zrhox, ztemp

      IF (before) THEN
         zrhox = Agrif_Rhox()
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj,jk) = e1v(ji,jj) * vn(ji,jj,jk) * fse3v(ji,jj,jk)
               END DO
            END DO
         END DO
         tabres = zrhox * tabres
      ELSE
         DO jk=k1,k2
            DO jj=j1,j2
               DO ji=i1,i2
                  ztemp        = vn(ji,jj,jk)
                  vn(ji,jj,jk) = tabres(ji,jj,jk) / (e1v(ji,jj)*fse3v(ji,jj,jk)) * vmask(ji,jj,jk)
                  vb(ji,jj,jk) = vb(ji,jj,jk) + atfp * ( vn(ji,jj,jk) - ztemp )
               END DO
            END DO
         END DO
      ENDIF

   END SUBROUTINE updatev

   SUBROUTINE updateu2d( tabres, i1, i2, j1, j2, before )
      !!---------------------------------------------
      !!          *** ROUTINE updateu2d ***
      !!---------------------------------------------
#  include "domzgr_substitute.h90"

      INTEGER, INTENT(in) :: i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before

      INTEGER :: ji, jj, jk
      REAL(wp) :: zrhoy
      REAL(wp) :: zhinv, ztemp

      IF (before) THEN
         zrhoy = Agrif_Rhoy()
         DO jk = 1,jpkm1
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj) = tabres(ji,jj) + fse3u(ji,jj,jk) * un(ji,jj,jk)
               END DO
            END DO
         END DO
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = tabres(ji,jj) * e2u(ji,jj)
            END DO
         END DO
         tabres = zrhoy * tabres
      ELSE
         DO jj=j1,j2
            DO ji=i1,i2
               IF(umask(ji,jj,1) .NE. 0.) THEN             
                  spgu(ji,jj) = 0.e0
                  DO jk=1,jpk
                     spgu(ji,jj) = spgu(ji,jj) + fse3u(ji,jj,jk) * un(ji,jj,jk)
                  END DO
                  spgu(ji,jj) = spgu(ji,jj) * e2u(ji,jj)
                  zhinv = (tabres(ji,jj)-spgu(ji,jj))/(hu(ji,jj)*e2u(ji,jj))
                  Do jk=1,jpk              
                     ztemp        = un(ji,jj,jk)              
                     un(ji,jj,jk) = ( ztemp + zhinv ) * umask(ji,jj,jk)            
                     ub(ji,jj,jk) = ub(ji,jj,jk) + atfp * ( un(ji,jj,jk) - ztemp )
                  END DO
               ENDIF
            END DO
         END DO
      ENDIF

   END SUBROUTINE updateu2d

   SUBROUTINE updatev2d( tabres, i1, i2, j1, j2, before )
      !!---------------------------------------------
      !!          *** ROUTINE updatev2d ***
      !!---------------------------------------------

      INTEGER, INTENT(in) :: i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before

      INTEGER :: ji, jj, jk
      REAL(wp) :: zrhox
      REAL(wp) :: zhinv, ztemp

      IF (before) THEN
         zrhox = Agrif_Rhox()
         tabres = 0.e0
         DO jk = 1,jpkm1
            DO jj=j1,j2
               DO ji=i1,i2
                  tabres(ji,jj) = tabres(ji,jj) + fse3v(ji,jj,jk) * vn(ji,jj,jk)
               END DO
            END DO
         END DO
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = tabres(ji,jj) * e1v(ji,jj)
            END DO
         END DO
         tabres = zrhox * tabres
      ELSE
         DO jj=j1,j2
            DO ji=i1,i2
               IF(vmask(ji,jj,1) .NE. 0.) THEN             
                  spgv(ji,jj) = 0.
                  DO jk=1,jpk
                     spgv(ji,jj) = spgv(ji,jj) + fse3v(ji,jj,jk) * vn(ji,jj,jk)
                  END DO
                  spgv(ji,jj) = spgv(ji,jj) * e1v(ji,jj)
                  zhinv = (tabres(ji,jj)-spgv(ji,jj))/(hv(ji,jj)*e1v(ji,jj))
                  DO jk=1,jpk             
                     ztemp        = vn(ji,jj,jk)             
                     vn(ji,jj,jk) = ( ztemp + zhinv ) * vmask(ji,jj,jk)
                     vb(ji,jj,jk) = vb(ji,jj,jk) + atfp * ( vn(ji,jj,jk) - ztemp )
                  END DO
               ENDIF
            END DO
         END DO
      ENDIF

   END SUBROUTINE updatev2d

   SUBROUTINE updateSSH( tabres, i1, i2, j1, j2, before )
      !!---------------------------------------------
      !!          *** ROUTINE updateSSH ***
      !!---------------------------------------------
#  include "domzgr_substitute.h90"

      INTEGER, INTENT(in) :: i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before

      INTEGER :: ji, jj
      REAL(wp) :: zrhox, zrhoy, ztemp

      IF (before) THEN
         zrhox = Agrif_Rhox()
         zrhoy = Agrif_Rhoy()
         DO jj=j1,j2
            DO ji=i1,i2
               tabres(ji,jj) = e1t(ji,jj) * e2t(ji,jj) * sshn(ji,jj)
            END DO
         END DO
         tabres = zrhox * zrhoy * tabres
      ELSE
         DO jj=j1,j2
            DO ji=i1,i2
               ztemp       = sshn(ji,jj)
               sshn(ji,jj) = tabres(ji,jj) / (e1t(ji,jj) * e2t(ji,jj)) * tmask(ji,jj,1)
               sshb(ji,jj) = sshb(ji,jj) + atfp * ( sshn(ji,jj) - ztemp )
            END DO
         END DO
      ENDIF

   END SUBROUTINE updateSSH


   SUBROUTINE updateglamT( tabres, i1, i2, j1, j2, before )

      INTEGER, INTENT(in) :: i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before

      INTEGER :: ji, jj
      INTEGER :: icnt

      IF (before) THEN
         tabres(i1:i2,j1:j2) = glamt(i1:i2,j1:j2)
      ELSE
         icnt = 0 
         DO jj=j1,j2
            DO ji=i1,i2
               IF( ABS( glamt(ji,jj) - tabres(ji,jj)) > 1.e-2 ) THEN
                  WRITE(numout,*) 'ERROR in glamt update at point ji,jj ', ji,jj
                  WRITE(numout,*) '      glamt(ji,jj), tabres(ji,jj)) ', glamt(ji,jj), tabres(ji,jj)
                  icnt = icnt + 1
               ENDIF
            END DO
         END DO
         IF(icnt /= 0) THEN 
            CALL ctl_stop('ERROR in glamt update...')
         ELSE
            IF(lwp) WRITE(numout,*) 'Update glamt ok...'
         END IF
      ENDIF

   END SUBROUTINE updateglamT


   SUBROUTINE updategphiT( tabres, i1, i2, j1, j2, before )

      INTEGER, INTENT(in) :: i1, i2, j1, j2
      REAL(wp), DIMENSION(i1:i2,j1:j2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before

      INTEGER :: ji, jj
      INTEGER :: icnt

      IF (before) THEN
         tabres(i1:i2,j1:j2) = gphit(i1:i2,j1:j2)
      ELSE
         icnt = 0 
         DO jj=j1,j2
            DO ji=i1,i2
               IF( ABS( gphit(ji,jj) - tabres(ji,jj)) > 1.e-2 ) THEN
                  WRITE(numout,*) 'ERROR in gphit update at point ji,jj ', ji,jj
                  WRITE(numout,*) '      gphit(ji,jj), tabres(ji,jj)) ', gphit(ji,jj), tabres(ji,jj)
                  icnt = icnt + 1
               ENDIF
            END DO
         END DO
         IF(icnt /= 0) THEN 
            CALL ctl_stop('ERROR in gphit update...')
         ELSE
            IF(lwp) WRITE(numout,*) 'Update gphit ok...'
         END IF
      ENDIF

   END SUBROUTINE updategphiT


   SUBROUTINE updateAVT( tabres, i1, i2, j1, j2, k1, k2, before )

      INTEGER, INTENT(in) :: i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before

      IF (before) THEN
         tabres(i1:i2,j1:j2,k1:k2) = avt_k(i1:i2,j1:j2,k1:k2)
      ELSE
         avt_k(i1:i2,j1:j2,k1:k2) = tabres(i1:i2,j1:j2,k1:k2) 
      ENDIF

   END SUBROUTINE updateAVT


   SUBROUTINE updateAVM( tabres, i1, i2, j1, j2, k1, k2, before )

      INTEGER, INTENT(in) :: i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before

      IF (before) THEN
         tabres(i1:i2,j1:j2,k1:k2) = avm_k(i1:i2,j1:j2,k1:k2)
      ELSE
         avm_k(i1:i2,j1:j2,k1:k2) = tabres(i1:i2,j1:j2,k1:k2) 
      ENDIF

   END SUBROUTINE updateAVM


   SUBROUTINE updateAVMu( tabres, i1, i2, j1, j2, k1, k2, before )

      INTEGER, INTENT(in) :: i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before

      IF (before) THEN
         tabres(i1:i2,j1:j2,k1:k2) = avmu_k(i1:i2,j1:j2,k1:k2)
      ELSE
         avmu_k(i1:i2,j1:j2,k1:k2) = tabres(i1:i2,j1:j2,k1:k2) 
      ENDIF

   END SUBROUTINE updateAVMu


   SUBROUTINE updateAVMv( tabres, i1, i2, j1, j2, k1, k2, before )

      INTEGER, INTENT(in) :: i1, i2, j1, j2, k1, k2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before

      IF (before) THEN
         tabres(i1:i2,j1:j2,k1:k2) = avmv_k(i1:i2,j1:j2,k1:k2)
      ELSE
         avmv_k(i1:i2,j1:j2,k1:k2) = tabres(i1:i2,j1:j2,k1:k2) 
      ENDIF

   END SUBROUTINE updateAVMv


#else
CONTAINS
   SUBROUTINE agrif_opa_update_empty
      !!---------------------------------------------
      !!   *** ROUTINE agrif_opa_update_empty ***
      !!---------------------------------------------
      WRITE(*,*)  'agrif_opa_update : You should not have seen this print! error?'
   END SUBROUTINE agrif_opa_update_empty
#endif
END MODULE agrif_opa_update
