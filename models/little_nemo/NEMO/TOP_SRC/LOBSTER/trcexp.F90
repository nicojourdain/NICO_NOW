MODULE trcexp
   !!======================================================================
   !!                         ***  MODULE p4sed  ***
   !! TOP :   PISCES Compute loss of organic matter in the sediments
   !!======================================================================
   !! History :    -   !  1999    (O. Aumont, C. Le Quere)  original code
   !!              -   !  2001-05 (O. Aumont, E. Kestenare) add sediment computations
   !!             1.0  !  2005-06 (A.-S. Kremeur) new temporal integration for sedpoc
   !!             2.0  !  2007-12  (C. Deltel, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_lobster
   !!----------------------------------------------------------------------
   !!   'key_lobster'                                     LOBSTER bio-model
   !!----------------------------------------------------------------------
   !!   trc_exp        :  Compute loss of organic matter in the sediments
   !!----------------------------------------------------------------------
   USE oce_trc         !
   USE sms_lobster
   USE lbclnk
   USE trc
   USE trcnam_trp
   USE prtctl_trc      ! Print control for debbuging
   USE trdmod_oce
   USE trdmod_trc
   USE iom

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_exp    ! called in p4zprg.F90

   !!* Substitution
#  include "top_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcexp.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_exp( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_exp  ***
      !!
      !! ** Purpose :   MODELS EXPORT OF BIOGENIC MATTER (POC ''SOFT
      !!              TISSUE'') AND ITS DISTRIBUTION IN WATER COLUMN
      !!
      !! ** Method  : - IN THE SURFACE LAYER POC IS PRODUCED ACCORDING TO
      !!              NURTRIENTS AVAILABLE AND GROWTH CONDITIONS. NUTRIENT UPTAKE
      !!              KINETICS FOLLOW MICHAELIS-MENTON FORMULATION. 
      !!              THE TOTAL PARTICLE AMOUNT PRODUCED, IS DISTRIBUTED IN THE WATER
      !!              COLUMN BELOW THE SURFACE LAYER.
      !!---------------------------------------------------------------------
      !!
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index      
      !!
      INTEGER  ::   ji, jj, jk, jl, ikt, ierr
      REAL(wp) ::   zgeolpoc, zfact, zwork, ze3t, zsedpocd
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::  ztrbio
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_exp')
      !
      IF( kt == nittrc000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' trc_exp: LOBSTER export'
         IF(lwp) WRITE(numout,*) ' ~~~~~~~'
      ENDIF

      IF( l_trdtrc )  THEN
         ALLOCATE( ztrbio(jpi,jpj,jpk) , STAT = ierr )   ! temporary save of trends
         IF( ierr > 0 ) THEN
            CALL ctl_stop( 'trc_exp: unable to allocate ztrbio array' )   ;   RETURN
         ENDIF
         ztrbio(:,:,:) = tra(:,:,:,jp_lob_no3)
      ENDIF

      ! VERTICAL DISTRIBUTION OF NEWLY PRODUCED BIOGENIC
      ! POC IN THE WATER COLUMN
      ! (PARTS OF NEWLY FORMED MATTER REMAINING IN THE DIFFERENT
      ! LAYERS IS DETERMINED BY DMIN3 DEFINED IN sms_lobster.F90
      ! ----------------------------------------------------------------------
      DO jk = 1, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1
               ze3t = 1. / fse3t(ji,jj,jk)
               tra(ji,jj,jk,jp_lob_no3) = tra(ji,jj,jk,jp_lob_no3) + ze3t * dmin3(ji,jj,jk) * fbod(ji,jj)
            END DO
         END DO
      END DO

      ! Find the last level of the water column
      ! Compute fluxes due to sinking particles (slow)
   

      zgeolpoc = 0.e0         !     Initialization
      ! Release of nutrients from the "simple" sediment
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1
            ikt = mbkt(ji,jj) 
            tra(ji,jj,ikt,jp_lob_no3) = tra(ji,jj,ikt,jp_lob_no3) + sedlam * sedpocn(ji,jj) / fse3t(ji,jj,ikt) 
            ! Deposition of organic matter in the sediment
            zwork = vsed * trn(ji,jj,ikt,jp_lob_det)
            sedpoca(ji,jj) = ( zwork + dminl(ji,jj) * fbod(ji,jj)   &
               &           - sedlam * sedpocn(ji,jj) - sedlostpoc * sedpocn(ji,jj) ) * rdt
            zgeolpoc = zgeolpoc + sedlostpoc * sedpocn(ji,jj) * e1e2t(ji,jj)
         END DO
      END DO

      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1
            tra(ji,jj,1,jp_lob_no3) = tra(ji,jj,1,jp_lob_no3) + zgeolpoc * cmask(ji,jj) / areacot / fse3t(ji,jj,1)
         END DO
      END DO

      CALL lbc_lnk( sedpocn, 'T', 1. )
 
      ! Oa & Ek: diagnostics depending on jpdia2d !          left as example
      IF( ln_diatrc ) THEN
         IF( lk_iomput ) THEN   ;   CALL iom_put( "SEDPOC" , sedpocn )
         ELSE                   ;   trc2d(:,:,jp_lob0_2d + 18) = sedpocn(:,:)
         ENDIF
      ENDIF

      
      ! Time filter and swap of arrays
      ! ------------------------------
      IF( neuler == 0 .AND. kt == nittrc000 ) THEN        ! Euler time-stepping at first time-step
        !                                             ! (only swap)
        sedpocn(:,:) = sedpoca(:,:)
        !                                              
      ELSE
        !
        DO jj = 1, jpj
           DO ji = 1, jpi
              zsedpocd = sedpoca(ji,jj) - 2. * sedpocn(ji,jj) + sedpocb(ji,jj)      ! time laplacian on tracers
              sedpocb(ji,jj) = sedpocn(ji,jj) + atfp * zsedpocd                     ! sedpocb <-- filtered sedpocn
              sedpocn(ji,jj) = sedpoca(ji,jj)                                       ! sedpocn <-- sedpoca
           END DO
        END DO
        ! 
      ENDIF
      sedpoca(:,:) = 0.e0
      !
      IF( l_trdtrc ) THEN
         ztrbio(:,:,:) = tra(:,:,:,jp_lob_no3) - ztrbio(:,:,:)
         jl = jp_lob0_trd + 16
         CALL trd_mod_trc( ztrbio, jl, kt )   ! handle the trend
         DEALLOCATE( ztrbio ) 
      ENDIF

      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('exp')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('trc_exp')
      !
   END SUBROUTINE trc_exp

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE trc_exp( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_exp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_exp
#endif 

   !!======================================================================
END MODULE  trcexp
