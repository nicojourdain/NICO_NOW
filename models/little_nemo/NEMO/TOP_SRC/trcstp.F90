MODULE trcstp
   !!======================================================================
   !!                       ***  MODULE trcstp  ***
   !! Time-stepping    : time loop of opa for passive tracer
   !!======================================================================
   !! History :  1.0  !  2004-03  (C. Ethe)  Original
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   trc_stp      : passive tracer system time-stepping
   !!----------------------------------------------------------------------
   USE oce_trc          ! ocean dynamics and active tracers variables
   USE trc
   USE trctrp           ! passive tracers transport
   USE trcsms           ! passive tracers sources and sinks
   USE prtctl_trc       ! Print control for debbuging
   USE trcdia
   USE trcwri
   USE trcrst
   USE trdmod_trc_oce
   USE trdmld_trc
   USE iom
   USE in_out_manager
   USE trcsub

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_stp    ! called by step

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcstp.F90 3319 2012-03-05 16:03:27Z cetlod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_stp( kt )
      !!-------------------------------------------------------------------
      !!                     ***  ROUTINE trc_stp  ***
      !!                      
      !! ** Purpose : Time loop of opa for passive tracer
      !! 
      !! ** Method  : 
      !!              Compute the passive tracers trends 
      !!              Update the passive tracers
      !!-------------------------------------------------------------------
      INTEGER, INTENT( in ) ::  kt      ! ocean time-step index
      INTEGER               ::  jk, jn  ! dummy loop indices
      REAL(wp)              ::  ztrai
      CHARACTER (len=25)    ::  charout
      !!-------------------------------------------------------------------
      !
      IF( nn_timing == 1 )   CALL timing_start('trc_stp')
      !
      IF( kt == nittrc000 .AND. lk_trdmld_trc )  CALL trd_mld_trc_init    ! trends: Mixed-layer
      !
      IF( lk_vvl ) THEN                                                   ! update ocean volume due to ssh temporal evolution
         DO jk = 1, jpk
            cvol(:,:,jk) = e1e2t(:,:) * fse3t(:,:,jk) * tmask(:,:,jk)
         END DO
         IF( lk_degrad )  cvol(:,:,:) = cvol(:,:,:) * facvol(:,:,:)       ! degrad option: reduction by facvol
         areatot         = glob_sum( cvol(:,:,:) )
      ENDIF
      !    
     IF( nn_dttrc /= 1 )   CALL trc_sub_stp( kt )  ! averaging physical variables for sub-stepping

     IF( MOD( kt , nn_dttrc ) == 0 ) THEN      ! only every nn_dttrc time step
         !
         IF(ln_ctl) THEN
            WRITE(charout,FMT="('kt =', I4,'  d/m/y =',I2,I2,I4)") kt, nday, nmonth, nyear
            CALL prt_ctl_trc_info(charout)
         ENDIF
         !
         tra(:,:,:,:) = 0.e0
         !
                                   CALL trc_rst_opn  ( kt )       ! Open tracer restart file 
         IF( lk_iomput ) THEN  ;   CALL trc_wri      ( kt )       ! output of passive tracers with iom I/O manager
         ELSE                  ;   CALL trc_dia      ( kt )       ! output of passive tracers with old I/O manager
         ENDIF
                                   CALL trc_sms      ( kt )       ! tracers: sinks and sources
                                   CALL trc_trp      ( kt )       ! transport of passive tracers
         IF( lrst_trc )            CALL trc_rst_wri  ( kt )       ! write tracer restart file
         IF( lk_trdmld_trc  )      CALL trd_mld_trc  ( kt )       ! trends: Mixed-layer
         !
         IF( nn_dttrc /= 1   )     CALL trc_sub_reset( kt )       ! resetting physical variables when sub-stepping
         !
      ENDIF
      !
      ztrai = 0._wp                                                   !  content of all tracers
      DO jn = 1, jptra
         ztrai = ztrai + glob_sum( trn(:,:,:,jn) * cvol(:,:,:)   )
      END DO
      IF( lwp ) WRITE(numstr,9300) kt,  ztrai / areatot
9300  FORMAT(i10,e18.10)
      !
      IF( nn_timing == 1 )   CALL timing_stop('trc_stp')
      !
   END SUBROUTINE trc_stp

#else
   !!----------------------------------------------------------------------
   !!   Default key                                     NO passive tracers
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_stp( kt )        ! Empty routine
      WRITE(*,*) 'trc_stp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_stp
#endif

   !!======================================================================
END MODULE trcstp
