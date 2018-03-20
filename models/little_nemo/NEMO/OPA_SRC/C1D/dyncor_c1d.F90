MODULE dyncor_c1d
   !!======================================================================
   !!                     ***  MODULE  dyncor_c1d  ***
   !! Ocean Dynamics :   Coriolis term in 1D configuration
   !!=====================================================================
   !! History :  2.0  !  2004-09  (C. Ethe)  Original code
   !!            3.0  !  2008-04  (G. Madec)  style only
   !!----------------------------------------------------------------------
#if defined key_c1d
   !!----------------------------------------------------------------------
   !!   'key_c1d'                                          1D Configuration
   !!----------------------------------------------------------------------
   !!   cor_c1d      : Coriolis factor at T-point (1D configuration)
   !!   dyn_cor_c1d  : vorticity trend due to Coriolis at T-point
   !!----------------------------------------------------------------------
   USE oce               ! ocean dynamics and tracers
   USE dom_oce           ! ocean space and time domain
   USE phycst            ! physical constants
   USE in_out_manager    ! I/O manager
   USE prtctl            ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   cor_c1d      ! routine called by OPA.F90
   PUBLIC   dyn_cor_c1d  ! routine called by step1d.F90

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/C1D 3.3 , NEMO Consortium (2010)
   !! $Id: dyncor_c1d.F90 2382 2010-11-13 13:08:12Z gm $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE cor_c1d
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE cor_c1d  ***
      !! 
      !! ** Purpose : set the Coriolis factor at T-point
      !!----------------------------------------------------------------------
      REAL(wp) ::   zphi0, zbeta, zf0         !  temporary scalars
      !!----------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'cor_c1d : Coriolis factor at T-point'
      IF(lwp) WRITE(numout,*) '~~~~~~~'

      SELECT CASE( jphgr_msh )   ! type of horizontal mesh
      !
      CASE ( 0, 1, 4 )               ! mesh on the sphere
         ff(:,:) = 2. * omega * SIN( rad * gphit(:,:) ) 
         !
      CASE ( 2 )                     ! f-plane at ppgphi0 
         ff(:,:) = 2. * omega * SIN( rad * ppgphi0 )
         IF(lwp) WRITE(numout,*) '          f-plane: Coriolis parameter = constant = ', ff(1,1)
         !
      CASE ( 3 )                     ! beta-plane
         zbeta   = 2. * omega * COS( rad * ppgphi0 ) / ra                     ! beta at latitude ppgphi0
         zphi0   = ppgphi0 - FLOAT( jpjglo/2) * ppe2_m *1.e-3  / ( ra * rad ) ! latitude of the first row F-points
         zf0     = 2. * omega * SIN( rad * zphi0 )                            ! compute f0 1st point south
         ff(:,:) = ( zf0  + zbeta * gphit(:,:) * 1.e+3 )                      ! f = f0 +beta* y ( y=0 at south)
         IF(lwp) WRITE(numout,*) '          Beta-plane: Beta parameter = constant = ', ff(1,1)
         IF(lwp) WRITE(numout,*) '                      Coriolis parameter varies from ', ff(1,1),' to ', ff(1,jpj)
         !
      CASE ( 5 )                     ! beta-plane and rotated domain
         zbeta = 2. * omega * COS( rad * ppgphi0 ) / ra                     ! beta at latitude ppgphi0
         zphi0 = 15.e0                                                      ! latitude of the first row F-points
         zf0   = 2. * omega * SIN( rad * zphi0 )                            ! compute f0 1st point south
         ff(:,:) = ( zf0 + zbeta * ABS( gphit(:,:) - zphi0 ) * rad * ra )   ! f = f0 +beta* y ( y=0 at south)
         IF(lwp) WRITE(numout,*) '          Beta-plane: Beta parameter = constant = ', ff(1,1)
         IF(lwp) WRITE(numout,*) '                      Coriolis parameter varies from ', ff(1,1),' to ', ff(1,jpj)
         !
      END SELECT
      !
   END SUBROUTINE cor_c1d


   SUBROUTINE dyn_cor_c1d( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE dyn_cor_c1d  ***
      !! 
      !! ** Purpose :   Compute the now Coriolis trend and add it to 
      !!               the general trend of the momentum equation in 1D case.
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index
      !!
      INTEGER ::   ji, jj, jk         ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn_cor_c1d : total vorticity trend in 1D'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~'
      ENDIF
      !
      DO jk = 1, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               ua(ji,jj,jk) = ua(ji,jj,jk) + ff(ji,jj) * vn(ji,jj,jk)
               va(ji,jj,jk) = va(ji,jj,jk) - ff(ji,jj) * un(ji,jj,jk)
            END DO
         END DO
      END DO   
      !
      IF(ln_ctl)   CALL prt_ctl( tab3d_1=ua, clinfo1=' cor  - Ua: ', mask1=umask,  &
         &                       tab3d_2=va, clinfo2=' Va: '       , mask2=vmask )
      !
   END SUBROUTINE dyn_cor_c1d

#else
   !!----------------------------------------------------------------------
   !!   Default key                                     NO 1D Configuration
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE cor_c1d              ! Empty routine
   END SUBROUTINE cor_c1d   
   SUBROUTINE dyn_cor_c1d ( kt )      ! Empty routine
      WRITE(*,*) 'dyn_cor_c1d: You should not have seen this print! error?', kt
   END SUBROUTINE dyn_cor_c1d
#endif

   !!=====================================================================
END MODULE dyncor_c1d
