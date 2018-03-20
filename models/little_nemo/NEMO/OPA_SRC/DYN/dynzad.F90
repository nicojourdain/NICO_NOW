MODULE dynzad
   !!======================================================================
   !!                       ***  MODULE  dynzad  ***
   !! Ocean dynamics : vertical advection trend
   !!======================================================================
   !! History :  OPA  ! 1991-01  (G. Madec) Original code
   !!            7.0  ! 1991-11  (G. Madec)
   !!            7.5  ! 1996-01  (G. Madec) statement function for e3
   !!   NEMO     0.5  ! 2002-07  (G. Madec) Free form, F90
   !!----------------------------------------------------------------------
   
   !!----------------------------------------------------------------------
   !!   dyn_zad       : vertical advection momentum trend
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE sbc_oce        ! surface boundary condition: ocean
   USE trdmod_oce     ! ocean variables trends
   USE trdmod         ! ocean dynamics trends 
   USE in_out_manager ! I/O manager
   USE lib_mpp         ! MPP library
   USE prtctl         ! Print control
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE
   
   PUBLIC   dyn_zad   ! routine called by step.F90

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: dynzad.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_zad ( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dynzad  ***
      !! 
      !! ** Purpose :   Compute the now vertical momentum advection trend and 
      !!      add it to the general trend of momentum equation.
      !!
      !! ** Method  :   The now vertical advection of momentum is given by:
      !!         w dz(u) = ua + 1/(e1u*e2u*e3u) mk+1[ mi(e1t*e2t*wn) dk(un) ]
      !!         w dz(v) = va + 1/(e1v*e2v*e3v) mk+1[ mj(e1t*e2t*wn) dk(vn) ]
      !!      Add this trend to the general trend (ua,va):
      !!         (ua,va) = (ua,va) + w dz(u,v)
      !!
      !! ** Action  : - Update (ua,va) with the vert. momentum adv. trends
      !!              - Save the trends in (ztrdu,ztrdv) ('key_trddyn')
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step inedx
      !
      INTEGER  ::   ji, jj, jk      ! dummy loop indices
      REAL(wp) ::   zua, zva        ! temporary scalars
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  zwuw , zwvw
      REAL(wp), POINTER, DIMENSION(:,:  ) ::  zww
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  ztrdu, ztrdv
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dyn_zad')
      !
      CALL wrk_alloc( jpi,jpj, zww ) 
      CALL wrk_alloc( jpi,jpj,jpk, zwuw , zwvw ) 
      !
      IF( kt == nit000 ) THEN
         IF(lwp)WRITE(numout,*)
         IF(lwp)WRITE(numout,*) 'dyn_zad : arakawa advection scheme'
      ENDIF

      IF( l_trddyn )   THEN         ! Save ua and va trends
         CALL wrk_alloc( jpi, jpj, jpk, ztrdu, ztrdv ) 
         ztrdu(:,:,:) = ua(:,:,:) 
         ztrdv(:,:,:) = va(:,:,:) 
      ENDIF
      
      DO jk = 2, jpkm1              ! Vertical momentum advection at level w and u- and v- vertical
         DO jj = 2, jpj                   ! vertical fluxes 
            DO ji = fs_2, jpi             ! vector opt.
               zww(ji,jj) = 0.25 * e1t(ji,jj) * e2t(ji,jj) * wn(ji,jj,jk)
            END DO
         END DO
         DO jj = 2, jpjm1                 ! vertical momentum advection at w-point
            DO ji = fs_2, fs_jpim1        ! vector opt.
               zwuw(ji,jj,jk) = ( zww(ji+1,jj  ) + zww(ji,jj) ) * ( un(ji,jj,jk-1)-un(ji,jj,jk) )
               zwvw(ji,jj,jk) = ( zww(ji  ,jj+1) + zww(ji,jj) ) * ( vn(ji,jj,jk-1)-vn(ji,jj,jk) )
            END DO  
         END DO   
      END DO
      DO jj = 2, jpjm1              ! Surface and bottom values set to zero
         DO ji = fs_2, fs_jpim1           ! vector opt.
            zwuw(ji,jj, 1 ) = 0.e0
            zwvw(ji,jj, 1 ) = 0.e0
            zwuw(ji,jj,jpk) = 0.e0
            zwvw(ji,jj,jpk) = 0.e0
         END DO  
      END DO

      DO jk = 1, jpkm1              ! Vertical momentum advection at u- and v-points
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1       ! vector opt.
               !                         ! vertical momentum advective trends
               zua = - ( zwuw(ji,jj,jk) + zwuw(ji,jj,jk+1) ) / ( e1u(ji,jj) * e2u(ji,jj) * fse3u(ji,jj,jk) )
               zva = - ( zwvw(ji,jj,jk) + zwvw(ji,jj,jk+1) ) / ( e1v(ji,jj) * e2v(ji,jj) * fse3v(ji,jj,jk) )
               !                         ! add the trends to the general momentum trends
               ua(ji,jj,jk) = ua(ji,jj,jk) + zua
               va(ji,jj,jk) = va(ji,jj,jk) + zva
            END DO  
         END DO  
      END DO

      IF( l_trddyn ) THEN           ! save the vertical advection trends for diagnostic
         ztrdu(:,:,:) = ua(:,:,:) - ztrdu(:,:,:)
         ztrdv(:,:,:) = va(:,:,:) - ztrdv(:,:,:)
         CALL trd_mod(ztrdu, ztrdv, jpdyn_trd_zad, 'DYN', kt)
         CALL wrk_dealloc( jpi, jpj, jpk, ztrdu, ztrdv ) 
      ENDIF
      !                             ! Control print
      IF(ln_ctl)   CALL prt_ctl( tab3d_1=ua, clinfo1=' zad  - Ua: ', mask1=umask,   &
         &                       tab3d_2=va, clinfo2=       ' Va: ', mask2=vmask, clinfo3='dyn' )
      !
      CALL wrk_dealloc( jpi,jpj, zww ) 
      CALL wrk_dealloc( jpi,jpj,jpk, zwuw , zwvw ) 
      !
      IF( nn_timing == 1 )  CALL timing_stop('dyn_zad')
      !
   END SUBROUTINE dyn_zad

   !!======================================================================
END MODULE dynzad
