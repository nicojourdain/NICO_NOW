MODULE trcsms_lobster
   !!======================================================================
   !!                         ***  MODULE trcsms_lobster  ***
   !! TOP :   Time loop of LOBSTER model
   !!======================================================================
   !! History :   1.0  !            M. Levy
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
#if defined key_lobster
   !!----------------------------------------------------------------------
   !!   'key_lobster'                                       LOBSTER bio-model
   !!----------------------------------------------------------------------
   !!   trcsms_lobster        :  Time loop of passive tracers sms
   !!----------------------------------------------------------------------
   USE oce_trc          !
   USE trc
   USE trcbio
   USE trcopt
   USE trcsed
   USE trcexp
   USE trdmod_oce
   USE trdmod_trc_oce
   USE trdmod_trc
   USE trdmld_trc

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_sms_lobster    ! called in trcsms.F90

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcsms_lobster.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_sms_lobster( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sms_lobster  ***
      !!
      !! ** Purpose :  Managment of the call to Biological sources and sinks 
      !!               routines of LOBSTER bio-model 
      !!
      !! ** Method  : - ???
      !! --------------------------------------------------------------------
      !!
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index      
      !
      INTEGER :: jn
      !! --------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_sms_lobster')
      !
      CALL trc_opt( kt )      ! optical model
      CALL trc_bio( kt )      ! biological model
      CALL trc_sed( kt )      ! sedimentation model
      CALL trc_exp( kt )      ! export

      IF( l_trdtrc ) THEN
         DO jn = jp_lob0, jp_lob1
           CALL trd_mod_trc( tra(:,:,:,jn), jn, jptra_trd_sms, kt )   ! save trends
         END DO
      END IF

      IF( lk_trdmld_trc )  CALL trd_mld_bio( kt )   ! trends: Mixed-layer
      !
      IF( nn_timing == 1 )  CALL timing_stop('trc_sms_lobster')
      !
   END SUBROUTINE trc_sms_lobster

#else
   !!======================================================================
   !!  Dummy module :                                     No passive tracer
   !!======================================================================
CONTAINS
   SUBROUTINE trc_sms_lobster( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_sms_lobster: You should not have seen this print! error?', kt
   END SUBROUTINE trc_sms_lobster
#endif 

   !!======================================================================
END MODULE  trcsms_lobster
