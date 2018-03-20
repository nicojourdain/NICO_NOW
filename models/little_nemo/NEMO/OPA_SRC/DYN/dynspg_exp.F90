MODULE dynspg_exp
   !!======================================================================
   !!                   ***  MODULE  dynspg_exp  ***
   !! Ocean dynamics:  surface pressure gradient trend
   !!======================================================================
   !! History :  2.0  !  2005-11  (V. Garnier, G. Madec, L. Bessieres) Original code
   !!            3.2  !  2009-06  (G. Madec, M. Leclair, R. Benshila) introduce sshwzv module
   !!----------------------------------------------------------------------
#if defined key_dynspg_exp   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_dynspg_exp'                              explicit free surface
   !!----------------------------------------------------------------------
   !!   dyn_spg_exp  : update the momentum trend with the surface 
   !!                      pressure gradient in the free surface constant  
   !!                      volume case with vector optimization
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers 
   USE dom_oce         ! ocean space and time domain 
   USE sbc_oce         ! surface boundary condition: ocean
   USE obc_oce         ! Lateral open boundary condition
   USE phycst          ! physical constants
   USE obc_par         ! open boundary condition parameters
   USE obcdta          ! open boundary condition data     (bdy_dta_bt routine)
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distributed memory computing library
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE prtctl          ! Print control
   USE iom             ! I/O library
   USE timing          ! Timing


   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_spg_exp   ! routine called by step.F90

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: dynspg_exp.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_spg_exp( kt )
      !!----------------------------------------------------------------------
      !!                  ***  routine dyn_spg_exp  ***
      !!
      !! ** Purpose :   Compute the now trend due to the surface pressure
      !!              gradient in case of explicit free surface formulation and 
      !!              add it to the general trend of momentum equation.
      !!
      !! ** Method  :   Explicit free surface formulation. Add to the general
      !!              momentum trend the surface pressure gradient :
      !!                      (ua,va) = (ua,va) + (spgu,spgv)
      !!              where spgu = -1/rau0 d/dx(ps) = -g/e1u di( sshn )
      !!                    spgv = -1/rau0 d/dy(ps) = -g/e2v dj( sshn )
      !!
      !! ** Action :   (ua,va)   trend of horizontal velocity increased by 
      !!                         the surf. pressure gradient trend
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in)  ::   kt   ! ocean time-step index
      !!
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dyn_spg_exp')
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn_spg_exp : surface pressure gradient trend'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~   (explicit free surface)'
         !
         spgu(:,:) = 0._wp   ;   spgv(:,:) = 0._wp
         !
         IF( lk_vvl .AND. lwp ) WRITE(numout,*) '              lk_vvl=T : spg is included in dynhpg'
      ENDIF


!!gm bug ??  Rachid we have to discuss of the call below. I don't understand why it is here and not in ssh_wzv
      IF( lk_obc )   CALL obc_dta_bt( kt, 0 )      ! OBC: read or estimate ssh and vertically integrated velocities
!!gm

      IF( .NOT. lk_vvl ) THEN          !* fixed volume : add the surface pressure gradient trend
         !
         DO jj = 2, jpjm1                    ! now surface pressure gradient
            DO ji = fs_2, fs_jpim1   ! vector opt.
               spgu(ji,jj) = - grav * ( sshn(ji+1,jj) - sshn(ji,jj) ) / e1u(ji,jj)
               spgv(ji,jj) = - grav * ( sshn(ji,jj+1) - sshn(ji,jj) ) / e2v(ji,jj)
            END DO 
         END DO 
         DO jk = 1, jpkm1                    ! Add it to the general trend
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  ua(ji,jj,jk) = ua(ji,jj,jk) + spgu(ji,jj)
                  va(ji,jj,jk) = va(ji,jj,jk) + spgv(ji,jj)
               END DO
            END DO
         END DO
         !
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('dyn_spg_exp')
      !
   END SUBROUTINE dyn_spg_exp

#else
   !!----------------------------------------------------------------------
   !!   Default case :   Empty module   No standart explicit free surface 
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE dyn_spg_exp( kt )       ! Empty routine
      WRITE(*,*) 'dyn_spg_exp: You should not have seen this print! error?', kt
   END SUBROUTINE dyn_spg_exp
#endif

   !!======================================================================
END MODULE dynspg_exp
