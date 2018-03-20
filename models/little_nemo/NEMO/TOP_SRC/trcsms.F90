MODULE trcsms
   !!======================================================================
   !!                         ***  MODULE trcsms  ***
   !! TOP :   Time loop of passive tracers sms
   !!======================================================================
   !! History :   1.0  !  2005-03 (O. Aumont, A. El Moussaoui) F90
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_sms        :  Time loop of passive tracers sms
   !!----------------------------------------------------------------------
   USE oce_trc            !
   USE trc                !
   USE trcsms_lobster     ! LOBSTER bio-model
   USE trcsms_pisces      ! PISCES biogeo-model
   USE trcsms_cfc         ! CFC 11 & 12
   USE trcsms_c14b        ! C14b tracer 
   USE trcsms_my_trc      ! MY_TRC  tracers
   USE prtctl_trc         ! Print control for debbuging

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_sms    ! called in trcstp.F90

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcsms.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_sms( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sms  ***
      !!
      !! ** Purpose :   Managment of the time loop of passive tracers sms 
      !!
      !! ** Method  : -  call the main routine of of each defined tracer model
      !! -------------------------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index      
      !!
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )   CALL timing_start('trc_sms')
      !
      IF( lk_lobster )   CALL trc_sms_lobster( kt )    ! main program of LOBSTER
      IF( lk_pisces  )   CALL trc_sms_pisces ( kt )    ! main program of PISCES 
      IF( lk_cfc     )   CALL trc_sms_cfc    ( kt )    ! surface fluxes of CFC
      IF( lk_c14b    )   CALL trc_sms_c14b   ( kt )    ! surface fluxes of C14
      IF( lk_my_trc  )   CALL trc_sms_my_trc ( kt )    ! MY_TRC  tracers

      IF(ln_ctl) THEN      ! print mean trends (used for debugging)
         WRITE(charout, FMT="('sms ')")
         CALL prt_ctl_trc_info( charout )
         CALL prt_ctl_trc( tab4d=trn, mask=tmask, clinfo=ctrcnm )
      ENDIF
      !
      IF( nn_timing == 1 )   CALL timing_stop('trc_sms')
      !
   END SUBROUTINE trc_sms

#else
   !!======================================================================
   !!  Dummy module :                                     No passive tracer
   !!======================================================================
CONTAINS
   SUBROUTINE trc_sms( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_sms: You should not have seen this print! error?', kt
   END SUBROUTINE trc_sms
#endif 

   !!======================================================================
END MODULE  trcsms
