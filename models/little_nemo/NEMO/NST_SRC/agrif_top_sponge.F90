#define SPONGE_TOP

Module agrif_top_sponge
#if defined key_agrif && defined key_top
   USE par_oce
   USE oce
   USE dom_oce
   USE in_out_manager
   USE agrif_oce
   USE trc
   USE lib_mpp
   USE wrk_nemo  

   IMPLICIT NONE
   PRIVATE

   PUBLIC Agrif_Sponge_Trc, interptrn

   !!----------------------------------------------------------------------
   !! NEMO/NST 3.3 , NEMO Consortium (2010)
   !! $Id: agrif_top_sponge.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

   CONTAINS

   SUBROUTINE Agrif_Sponge_Trc
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Sponge_Trc ***
      !!---------------------------------------------
#include "domzgr_substitute.h90"
      !! 
      INTEGER :: ji,jj,jk,jl
      INTEGER :: spongearea
      REAL(wp) :: timecoeff
      REAL(wp) :: ztra, zabe1, zabe2, zbtr
      REAL(wp), POINTER, DIMENSION(:,:) :: localviscsponge
      REAL(wp), POINTER, DIMENSION(:,:,:,:) :: trbdiff, ztru, ztrv, ztab

#if defined SPONGE_TOP
      CALL wrk_alloc( jpi, jpj, localviscsponge )
      CALL wrk_alloc( jpi, jpj, jpk, jptra, trbdiff, ztru, ztrv, ztab )

      timecoeff = REAL(Agrif_NbStepint(),wp)/Agrif_rhot()

      Agrif_SpecialValue=0.
      Agrif_UseSpecialValue = .TRUE.
      ztab = 0.e0
      CALL Agrif_Bc_Variable(ztab, tra_id,calledweight=timecoeff,procname=interptrn)
      Agrif_UseSpecialValue = .FALSE.

      trbdiff(:,:,:,:) = trb(:,:,:,:) - ztab(:,:,:,:)

      spongearea = 2 + 2 * Agrif_irhox()

      localviscsponge = 0.
      
      IF (.NOT. spongedoneT) THEN
         spe1ur(:,:) = 0.
         spe2vr(:,:) = 0.

      IF ((nbondi == -1).OR.(nbondi == 2)) THEN
         DO ji = 2, spongearea
            localviscsponge(ji,:) = visc_tra * (spongearea-ji)/real(spongearea-2)
         ENDDO
	 
	 spe1ur(2:spongearea-1,:)=0.5 * (localviscsponge(2:spongearea-1,:) + localviscsponge(3:spongearea,:)) &
	       * e2u(2:spongearea-1,:) / e1u(2:spongearea-1,:)

         spe2vr(2:spongearea,1:jpjm1) = 0.5 * (localviscsponge(2:spongearea,1:jpjm1) + &
	          localviscsponge(2:spongearea,2:jpj)) &
	        * e1v(2:spongearea,1:jpjm1) / e2v(2:spongearea,1:jpjm1)
      ENDIF

      IF ((nbondi == 1).OR.(nbondi == 2)) THEN
         DO ji = nlci-spongearea + 1,nlci-1
            localviscsponge(ji,:) = visc_tra * (ji - (nlci-spongearea+1))/real(spongearea-2)
         ENDDO
	 
	 spe1ur(nlci-spongearea + 1:nlci-2,:)=0.5 * (localviscsponge(nlci-spongearea + 1:nlci-2,:) + &
	        localviscsponge(nlci-spongearea + 2:nlci-1,:)) &
	       * e2u(nlci-spongearea + 1:nlci-2,:) / e1u(nlci-spongearea + 1:nlci-2,:)

         spe2vr(nlci-spongearea + 1:nlci-1,1:jpjm1) = 0.5 * (localviscsponge(nlci-spongearea + 1:nlci-1,1:jpjm1) &
	           + localviscsponge(nlci-spongearea + 1:nlci-1,2:jpj)) &
	        * e1v(nlci-spongearea + 1:nlci-1,1:jpjm1) / e2v(nlci-spongearea + 1:nlci-1,1:jpjm1)
      ENDIF


      IF ((nbondj == -1).OR.(nbondj == 2)) THEN
         DO jj = 2, spongearea
            localviscsponge(:,jj) = visc_tra * (spongearea-jj)/real(spongearea-2)
         ENDDO
	 
	 spe1ur(1:jpim1,2:spongearea)=0.5 * (localviscsponge(1:jpim1,2:spongearea) + &
	        localviscsponge(2:jpi,2:spongearea)) &
	       * e2u(1:jpim1,2:spongearea) / e1u(1:jpim1,2:spongearea)

         spe2vr(:,2:spongearea-1) = 0.5 * (localviscsponge(:,2:spongearea-1) + &
	          localviscsponge(:,3:spongearea)) &
	        * e1v(:,2:spongearea-1) / e2v(:,2:spongearea-1)
      ENDIF

      IF ((nbondj == 1).OR.(nbondj == 2)) THEN
         DO jj = nlcj-spongearea + 1,nlcj-1
            localviscsponge(:,jj) = visc_tra * (jj - (nlcj-spongearea+1))/real(spongearea-2)
         ENDDO
	 
	 spe1ur(1:jpim1,nlcj-spongearea + 1:nlcj-1)=0.5 * (localviscsponge(1:jpim1,nlcj-spongearea + 1:nlcj-1) + &
	         localviscsponge(2:jpi,nlcj-spongearea + 1:nlcj-1)) &
	       * e2u(1:jpim1,nlcj-spongearea + 1:nlcj-1) / e1u(1:jpim1,nlcj-spongearea + 1:nlcj-1)

         spe2vr(:,nlcj-spongearea + 1:nlcj-2) = 0.5 * (localviscsponge(:,nlcj-spongearea + 1:nlcj-2) + &
	         localviscsponge(:,nlcj-spongearea + 2:nlcj-1)) &
	        * e1v(:,nlcj-spongearea + 1:nlcj-2) / e2v(:,nlcj-spongearea + 1:nlcj-2)
      ENDIF
      
         spbtr2(:,:) = 1. / ( e1t(:,:) * e2t(:,:))

         spongedoneT = .TRUE.
      ENDIF

      DO jl = 1, jptra
      DO jk = 1, jpkm1
         DO jj = 1, jpjm1
            DO ji = 1, jpim1
               zabe1 = umask(ji,jj,jk) * spe1ur(ji,jj) * fse3u(ji,jj,jk)
               zabe2 = vmask(ji,jj,jk) * spe2vr(ji,jj) * fse3v(ji,jj,jk)
               ztru(ji,jj,jk,jl) = zabe1 * ( trbdiff(ji+1,jj  ,jk,jl) - trbdiff(ji,jj,jk,jl) )
               ztrv(ji,jj,jk,jl) = zabe2 * ( trbdiff(ji  ,jj+1,jk,jl) - trbdiff(ji,jj,jk,jl) )
            ENDDO
         ENDDO

         DO jj = 2,jpjm1
            DO ji = 2,jpim1
               zbtr = spbtr2(ji,jj) / fse3t(ji,jj,jk)
               ! horizontal diffusive trends
               ztra = zbtr * (  ztru(ji,jj,jk,jl) - ztru(ji-1,jj,jk,jl)   &
                  &          + ztrv(ji,jj,jk,jl) - ztrv(ji,jj-1,jk,jl)  )
               ! add it to the general tracer trends
               tra(ji,jj,jk,jl) = (tra(ji,jj,jk,jl) + ztra)
            END DO
         END DO

      ENDDO
      ENDDO
 
      CALL wrk_dealloc( jpi, jpj, localviscsponge )
      CALL wrk_dealloc( jpi, jpj, jpk, jptra, trbdiff, ztru, ztrv, ztab )

#endif

   END SUBROUTINE Agrif_Sponge_Trc

   SUBROUTINE interptrn(tabres,i1,i2,j1,j2,k1,k2,l1,l2)
      !!---------------------------------------------
      !!   *** ROUTINE interptn ***
      !!---------------------------------------------
#  include "domzgr_substitute.h90"       
      
      INTEGER, INTENT(in) :: i1,i2,j1,j2,k1,k2,l1,l2
      REAL(wp), DIMENSION(i1:i2,j1:j2,k1:k2,l1:l2), INTENT(inout) :: tabres

      tabres(i1:i2,j1:j2,k1:k2,l1:l2) = trn(i1:i2,j1:j2,k1:k2,l1:l2)

   END SUBROUTINE interptrn

#else
CONTAINS

   SUBROUTINE agrif_top_sponge_empty
      !!---------------------------------------------
      !!   *** ROUTINE agrif_top_sponge_empty ***
      !!---------------------------------------------
      WRITE(*,*)  'agrif_top_sponge : You should not have seen this print! error?'
   END SUBROUTINE agrif_top_sponge_empty
#endif

END MODULE agrif_top_sponge
