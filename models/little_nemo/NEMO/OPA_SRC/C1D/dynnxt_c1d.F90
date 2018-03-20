MODULE dynnxt_c1d
   !!======================================================================
   !!                       ***  MODULE  dynnxt_c1d  ***
   !! Ocean dynamics: time stepping in 1D configuration
   !!======================================================================
   !! History :  2.0  !  2004-10  (C. Ethe)  Original code from dynnxt.F90
   !!            3.0  !  2008-04  (G.madec)  Style only
   !!----------------------------------------------------------------------
#if defined key_c1d
   !!----------------------------------------------------------------------
   !!   'key_c1d'                                          1D Configuration
   !!----------------------------------------------------------------------  
   !!   dyn_nxt_c1d : update the horizontal velocity from the momentum trend
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! lateral boundary condition (or mpp link)
   USE prtctl          ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC dyn_nxt_c1d                ! routine called by step.F90
   !!----------------------------------------------------------------------
   !! NEMO/C1D 3.3 , NEMO Consortium (2010)
   !! $Id: dynnxt_c1d.F90 2382 2010-11-13 13:08:12Z gm $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_nxt_c1d ( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_nxt_c1d  ***
      !!                   
      !! ** Purpose :   Compute the after horizontal velocity from the momentum trend.
      !!
      !! ** Method  :   Apply lateral boundary conditions on the trends (ua,va) 
      !!      through calls to routine lbc_lnk.
      !!      After velocity is compute using a leap-frog scheme environment:
      !!         (ua,va) = (ub,vb) + 2 rdt (ua,va)
      !!      Time filter applied on now horizontal velocity to avoid the
      !!      divergence of two consecutive time-steps and swap of dynamics
      !!      arrays to start the next time step:
      !!         (ub,vb) = (un,vn) + atfp [ (ub,vb) + (ua,va) - 2 (un,vn) ]
      !!         (un,vn) = (ua,va) 
      !!
      !! ** Action : - Update ub,vb arrays, the before horizontal velocity
      !!             - Update un,vn arrays, the now horizontal velocity
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index
      !!
      INTEGER  ::   jk           ! dummy loop indices
      REAL(wp) ::   z2dt         ! temporary scalar
      !!----------------------------------------------------------------------

      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn_nxt_c1d : time stepping on 1D configuation'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
      ENDIF

      z2dt = 2._wp * rdt                                                   ! Local constant initialization
      IF( neuler == 0 .AND. kt == nit000 )  z2dt = rdt

      CALL lbc_lnk( ua, 'U', -1. )   ;   CALL lbc_lnk( va, 'V', -1. )      ! Lateral boundary conditions

      DO jk = 1, jpkm1                                                     ! Next Velocity
         ua(:,:,jk) = ( ub(:,:,jk) + z2dt * ua(:,:,jk) ) * umask(:,:,jk)
         va(:,:,jk) = ( vb(:,:,jk) + z2dt * va(:,:,jk) ) * vmask(:,:,jk)
      END DO 
 
      DO jk = 1, jpkm1                                                     ! Time filter and swap of dynamics arrays
         IF( neuler == 0 .AND. kt == nit000 ) THEN                               ! Euler (forward) time stepping
             ub(:,:,jk) = un(:,:,jk)
             vb(:,:,jk) = vn(:,:,jk)
             un(:,:,jk) = ua(:,:,jk)
             vn(:,:,jk) = va(:,:,jk)
         ELSE                                                                    ! Leap-frog time stepping
             ub(:,:,jk) = atfp * ( ub(:,:,jk) + ua(:,:,jk) ) + atfp1 * un(:,:,jk)
             vb(:,:,jk) = atfp * ( vb(:,:,jk) + va(:,:,jk) ) + atfp1 * vn(:,:,jk)
             un(:,:,jk) = ua(:,:,jk)
             vn(:,:,jk) = va(:,:,jk)
         ENDIF
      END DO

      IF(ln_ctl)   CALL prt_ctl( tab3d_1=un, clinfo1=' nxt_c1d  - Un: ', mask1=umask,   &
         &                       tab3d_2=vn, clinfo2=' Vn: '           , mask2=vmask )
      !
   END SUBROUTINE dyn_nxt_c1d

#else
   !!----------------------------------------------------------------------
   !!   Default key                                     NO 1D Config
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE dyn_nxt_c1d ( kt )
      WRITE(*,*) 'dyn_nxt_c1d: You should not have seen this print! error?', kt
   END SUBROUTINE dyn_nxt_c1d
#endif

   !!======================================================================
END MODULE dynnxt_c1d
