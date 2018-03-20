#define TWO_WAY

MODULE agrif_top_update

#if defined key_agrif && defined key_top
   USE par_oce
   USE oce
   USE dom_oce
   USE agrif_oce
   USE trc
   USE wrk_nemo  

   IMPLICIT NONE
   PRIVATE

   PUBLIC Agrif_Update_Trc

   INTEGER, PUBLIC :: nbcline_trc = 0

   !!----------------------------------------------------------------------
   !! NEMO/NST 3.3 , NEMO Consortium (2010)
   !! $Id: agrif_top_update.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

   CONTAINS

   SUBROUTINE Agrif_Update_Trc( kt )
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Update_Trc ***
      !!---------------------------------------------
      !!
      INTEGER, INTENT(in) :: kt
      REAL(wp), POINTER, DIMENSION(:,:,:,:) :: ztra

  
      IF ((Agrif_NbStepint() .NE. (Agrif_irhot()-1)).AND.(kt /= 0)) RETURN

#if defined TWO_WAY
      CALL wrk_alloc( jpi, jpj, jpk, jpts, ztra )

      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid = 0.
 
     IF (MOD(nbcline_trc,nbclineupdate) == 0) THEN
         CALL Agrif_Update_Variable(ztra,trn_id, procname=updateTRC)
      ELSE
         CALL Agrif_Update_Variable(ztra,trn_id,locupdate=(/0,2/), procname=updateTRC)
      ENDIF

      Agrif_UseSpecialValueInUpdate = .FALSE.
      nbcline_trc = nbcline_trc + 1

      CALL wrk_dealloc( jpi, jpj, jpk, jpts, ztra )
#endif

   END SUBROUTINE Agrif_Update_Trc

   SUBROUTINE updateTRC(tabres,i1,i2,j1,j2,k1,k2,l1,l2,before)
      !!---------------------------------------------
      !!   *** ROUTINE UpdateTrc ***
      !!---------------------------------------------
#  include "domzgr_substitute.h90"

      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,l1,l2
      REAL, DIMENSION(i1:i2,j1:j2,k1:k2,l1:l2), INTENT(inout) :: tabres
      LOGICAL, INTENT(in) :: before
   
      INTEGER :: ji,jj,jk,jl

         IF (before) THEN
            DO jl=l1,l2
               DO jk=k1,k2
                  DO jj=j1,j2
                     DO ji=i1,i2
                        tabres(ji,jj,jk,jl) = trn(ji,jj,jk,jl)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ELSE
            DO jl=l1,l2
               DO jk=k1,k2
                  DO jj=j1,j2
                     DO ji=i1,i2
                        IF (tabres(ji,jj,jk,jl).NE.0.) THEN
                           trn(ji,jj,jk,jl) = tabres(ji,jj,jk,jl) * tmask(ji,jj,jk)
                        ENDIF
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDIF

   END SUBROUTINE updateTRC

#else
CONTAINS
   SUBROUTINE agrif_top_update_empty
      !!---------------------------------------------
      !!   *** ROUTINE agrif_Top_update_empty ***
      !!---------------------------------------------
      WRITE(*,*)  'agrif_top_update : You should not have seen this print! error?'
   END SUBROUTINE agrif_top_update_empty
#endif
END Module agrif_top_update
