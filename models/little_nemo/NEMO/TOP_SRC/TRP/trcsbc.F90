MODULE trcsbc
   !!==============================================================================
   !!                       ***  MODULE  trcsbc  ***
   !! Ocean passive tracers:  surface boundary condition
   !!======================================================================
   !! History :  8.2  !  1998-10  (G. Madec, G. Roullet, M. Imbard)  Original code
   !!            8.2  !  2001-02  (D. Ludicone)  sea ice and free surface
   !!            8.5  !  2002-06  (G. Madec)  F90: Free form and module
   !!            9.0  !  2004-03  (C. Ethe)  adapted for passive tracers
   !!                 !  2006-08  (C. Deltel) Diagnose ML trends for passive tracers
   !!==============================================================================
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_sbc      : update the tracer trend at ocean surface
   !!----------------------------------------------------------------------
   USE oce_trc         ! ocean dynamics and active tracers variables
   USE trc             ! ocean  passive tracers variables
   USE prtctl_trc      ! Print control for debbuging
   USE trdmod_oce
   USE trdtra

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_sbc   ! routine called by step.F90

   !! * Substitutions
#  include "top_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcsbc.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_sbc ( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_sbc  ***
      !!                   
      !! ** Purpose :   Compute the tracer surface boundary condition trend of
      !!      (concentration/dilution effect) and add it to the general 
      !!       trend of tracer equations.
      !!
      !! ** Method :
      !!      * concentration/dilution effect:
      !!            The surface freshwater flux modify the ocean volume
      !!         and thus the concentration of a tracer as :
      !!            tra = tra + emp * trn / e3t   for k=1
      !!         where emp, the surface freshwater budget (evaporation minus
      !!         precipitation ) given in kg/m2/s is divided
      !!         by 1035 kg/m3 (density of ocean water) to obtain m/s.
      !!
      !! ** Action  : - Update the 1st level of tra with the trend associated
      !!                with the tracer surface boundary condition 
      !!
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT( in ) ::   kt          ! ocean time-step index
      !
      INTEGER  ::   ji, jj, jn           ! dummy loop indices
      REAL(wp) ::   zsrau, zse3t   ! temporary scalars
      CHARACTER (len=22) :: charout
      REAL(wp), POINTER, DIMENSION(:,:  ) :: zemps
      REAL(wp), POINTER, DIMENSION(:,:,:) :: ztrtrd
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_sbc')
      !
      ! Allocate temporary workspace
                      CALL wrk_alloc( jpi, jpj,      zemps  )
      IF( l_trdtrc )  CALL wrk_alloc( jpi, jpj, jpk, ztrtrd )

      IF( kt == nittrc000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'trc_sbc : Passive tracers surface boundary condition'
         IF(lwp) WRITE(numout,*) '~~~~~~~ '
      ENDIF

      ! Coupling online : river runoff is added to the horizontal divergence (hdivn) in the subroutine sbc_rnf_div 
      ! one only consider the concentration/dilution effect due to evaporation minus precipitation + freezing/melting of sea-ice

      ! Coupling in offline, hdivn is computed from ocean horizontal velocities only ; the runoff are not included.
      ! emps in dynamical files contains (emps - rnf)
      IF( .NOT. lk_offline .AND. lk_vvl ) THEN  ! online coupling + volume variable 
         zemps(:,:) = emps(:,:) - emp(:,:)   
      ELSE
         zemps(:,:) = emps(:,:)
      ENDIF 

      ! 0. initialization
      zsrau = 1. / rau0
      DO jn = 1, jptra
         !
         IF( l_trdtrc ) ztrtrd(:,:,:) = tra(:,:,:,jn)  ! save trends
         !                                             ! add the trend to the general tracer trend
         DO jj = 2, jpj
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zse3t = 1. / fse3t(ji,jj,1)
               tra(ji,jj,1,jn) = tra(ji,jj,1,jn) + zemps(ji,jj) *  zsrau * trn(ji,jj,1,jn) * zse3t
            END DO
         END DO
         
         IF( l_trdtrc ) THEN
            ztrtrd(:,:,:) = tra(:,:,:,jn) - ztrtrd(:,:,:)
            CALL trd_tra( kt, 'TRC', jn, jptra_trd_nsr, ztrtrd )
         END IF
         !                                                       ! ===========
      END DO                                                     ! tracer loop
      !                                                          ! ===========
      IF( ln_ctl )   THEN
         WRITE(charout, FMT="('sbc ')") ;  CALL prt_ctl_trc_info(charout)
                                           CALL prt_ctl_trc( tab4d=tra, mask=tmask, clinfo=ctrcnm, clinfo2='trd' )
      ENDIF
                      CALL wrk_dealloc( jpi, jpj,      zemps  )
      IF( l_trdtrc )  CALL wrk_dealloc( jpi, jpj, jpk, ztrtrd )
      !
      IF( nn_timing == 1 )  CALL timing_stop('trc_sbc')
      !
   END SUBROUTINE trc_sbc

#else
   !!----------------------------------------------------------------------
   !!   Dummy module :                      NO passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_sbc (kt)              ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'trc_sbc: You should not have seen this print! error?', kt
   END SUBROUTINE trc_sbc
#endif
   
   !!======================================================================
END MODULE trcsbc
