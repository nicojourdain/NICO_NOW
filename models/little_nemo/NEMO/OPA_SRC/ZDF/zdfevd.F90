MODULE zdfevd
   !!======================================================================
   !!                       ***  MODULE  zdfevd  ***
   !! Ocean physics: parameterization of convection through an enhancement
   !!                of vertical eddy mixing coefficient
   !!======================================================================
   !! History :  OPA  !  1997-06  (G. Madec, A. Lazar)  Original code
   !!   NEMO     1.0  !  2002-06  (G. Madec)  F90: Free form and module
   !!             -   !  2005-06  (C. Ethe) KPP parameterization
   !!            3.2  !  2009-03  (M. Leclair, G. Madec, R. Benshila) test on both before & after
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   zdf_evd      : increase the momentum and tracer Kz at the location of
   !!                  statically unstable portion of the water column (ln_zdfevd=T)
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables
   USE zdf_oce         ! ocean vertical physics variables
   USE zdfkpp          ! KPP vertical mixing
   USE zdfgls          ! GLS vertical mixing
   USE in_out_manager  ! I/O manager
   USE iom             ! for iom_put
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_evd    ! called by step.F90

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: zdfevd.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE zdf_evd( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_evd  ***
      !!                   
      !! ** Purpose :   Local increased the vertical eddy viscosity and diffu-
      !!      sivity coefficients when a static instability is encountered.
      !!
      !! ** Method  :   avt, avm, and the 4 neighbouring avmu, avmv coefficients
      !!      are set to avevd (namelist parameter) if the water column is 
      !!      statically unstable (i.e. if rn2 < -1.e-12 )
      !!
      !! ** Action  :   avt, avm, avmu, avmv updted in static instability cases
      !!
      !! References :   Lazar, A., these de l'universite Paris VI, France, 1997
      !!----------------------------------------------------------------------
      USE oce,   zavt_evd => ua , zavm_evd => va  ! (ua,va) used ua workspace
      !
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step indexocean time step
      !
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zdf_evd')
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'zdf_evd : Enhanced Vertical Diffusion (evd)'
         IF(lwp) WRITE(numout,*) '~~~~~~~ '
         IF(lwp) WRITE(numout,*)
         !
         IF(lwp .AND. lk_zdfgls )   CALL ctl_warn(' No need zdf_evd with GLS closures ')
         !
      ENDIF

      zavt_evd(:,:,:) = avt(:,:,:)           ! set avt prior to evd application

      SELECT CASE ( nn_evdm )
      !
      CASE ( 1 )           ! enhance vertical eddy viscosity and diffusivity (if rn2<-1.e-12)
         !
         zavm_evd(:,:,:) = avm(:,:,:)           ! set avm prior to evd application
         !
         DO jk = 1, jpkm1 
#if defined key_vectopt_loop
            DO jj = 1, 1                     ! big loop forced
               DO ji = jpi+2, jpij   
#else
            DO jj = 2, jpj             ! no vector opt.
               DO ji = 2, jpi
#endif
#if defined key_zdfkpp
                  ! no evd mixing in the boundary layer with KPP
                  IF(  MIN( rn2(ji,jj,jk), rn2b(ji,jj,jk) ) <= -1.e-12  .AND.  fsdepw(ji,jj,jk) > hkpp(ji,jj)  ) THEN
#else
                  IF(  MIN( rn2(ji,jj,jk), rn2b(ji,jj,jk) ) <= -1.e-12 ) THEN
#endif
                     avt (ji  ,jj  ,jk) = rn_avevd * tmask(ji  ,jj  ,jk)
                     avm (ji  ,jj  ,jk) = rn_avevd * tmask(ji  ,jj  ,jk)
                     avmu(ji  ,jj  ,jk) = rn_avevd * umask(ji  ,jj  ,jk)
                     avmu(ji-1,jj  ,jk) = rn_avevd * umask(ji-1,jj  ,jk)
                     avmv(ji  ,jj  ,jk) = rn_avevd * vmask(ji  ,jj  ,jk)
                     avmv(ji  ,jj-1,jk) = rn_avevd * vmask(ji  ,jj-1,jk)
                  ENDIF
               END DO
            END DO
         END DO 
         CALL lbc_lnk( avt , 'W', 1. )   ;   CALL lbc_lnk( avm , 'W', 1. )   ! Lateral boundary conditions
         CALL lbc_lnk( avmu, 'U', 1. )   ;   CALL lbc_lnk( avmv, 'V', 1. )
         !
         zavm_evd(:,:,:) = avm(:,:,:) - zavm_evd(:,:,:)   ! change in avm due to evd
         CALL iom_put( "avm_evd", zavm_evd )              ! output this change
         !
      CASE DEFAULT         ! enhance vertical eddy diffusivity only (if rn2<-1.e-12) 
         DO jk = 1, jpkm1
!!!         WHERE( rn2(:,:,jk) <= -1.e-12 ) avt(:,:,jk) = tmask(:,:,jk) * avevd   ! agissant sur T SEUL! 
#if defined key_vectopt_loop
            DO jj = 1, 1                     ! big loop forced
               DO ji = 1, jpij   
#else
            DO jj = 1, jpj             ! loop over the whole domain (no lbc_lnk call)
               DO ji = 1, jpi
#endif
#if defined key_zdfkpp
                  ! no evd mixing in the boundary layer with KPP
                  IF(  MIN( rn2(ji,jj,jk), rn2b(ji,jj,jk) ) <= -1.e-12  .AND.  fsdepw(ji,jj,jk) > hkpp(ji,jj)  )   &          
#else
                  IF(  MIN( rn2(ji,jj,jk), rn2b(ji,jj,jk) ) <= -1.e-12 )   &
#endif
                     avt(ji,jj,jk) = rn_avevd * tmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      END SELECT 

      zavt_evd(:,:,:) = avt(:,:,:) - zavt_evd(:,:,:)   ! change in avt due to evd
      CALL iom_put( "avt_evd", zavt_evd )              ! output this change
      !
      IF( nn_timing == 1 )  CALL timing_stop('zdf_evd')
      !
   END SUBROUTINE zdf_evd

   !!======================================================================
END MODULE zdfevd
