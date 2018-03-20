MODULE obcrad 
   !!=================================================================================
   !!                       ***  MODULE  obcrad  ***
   !! Ocean dynamic :   Phase velocities for each open boundary
   !!=================================================================================
#if defined key_obc
   !!---------------------------------------------------------------------------------
   !!   obc_rad        : call the subroutine for each open boundary
   !!   obc_rad_east   : compute the east phase velocities
   !!   obc_rad_west   : compute the west phase velocities
   !!   obc_rad_north  : compute the north phase velocities
   !!   obc_rad_south  : compute the south phase velocities
   !!---------------------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE phycst          ! physical constants
   USE obc_oce         ! ocean open boundary conditions
   USE lib_mpp         ! for mppobc
   USE in_out_manager  ! I/O units

   IMPLICIT NONE
   PRIVATE

   PUBLIC   obc_rad    ! routine called by step.F90

   INTEGER ::   ji, jj, jk     ! dummy loop indices

   INTEGER ::      & ! ... boundary space indices 
      nib   = 1,   & ! nib   = boundary point
      nibm  = 2,   & ! nibm  = 1st interior point
      nibm2 = 3,   & ! nibm2 = 2nd interior point
                     ! ... boundary time indices 
      nit   = 1,   & ! nit    = now
      nitm  = 2,   & ! nitm   = before
      nitm2 = 3      ! nitm2  = before-before

   !! * Substitutions
#  include "obc_vectopt_loop_substitute.h90"
   !!---------------------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obcrad.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!---------------------------------------------------------------------------------

CONTAINS

   SUBROUTINE obc_rad ( kt )
      !!------------------------------------------------------------------------------
      !!                     SUBROUTINE obc_rad
      !!                    ********************
      !! ** Purpose :
      !!      Perform swap of arrays to calculate radiative phase speeds at the open 
      !!      boundaries and calculate those phase speeds if the open boundaries are 
      !!      not fixed. In case of fixed open boundaries does nothing.
      !!
      !!     The logical variable lp_obc_east, and/or lp_obc_west, and/or lp_obc_north,
      !!     and/or lp_obc_south allow the user to determine which boundary is an
      !!     open one (must be done in the param_obc.h90 file).
      !! 
      !! ** Reference : 
      !!     Marchesiello P., 1995, these de l'universite J. Fourier, Grenoble, France.
      !!
      !!  History :
      !!    8.5  !  02-10  (C. Talandier, A-M. Treguier) Free surface, F90 from the 
      !!                                                 J. Molines and G. Madec version
      !!------------------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt
      !!----------------------------------------------------------------------

      IF( lp_obc_east  .AND. .NOT.lfbceast  )   CALL obc_rad_east ( kt )   ! East open boundary

      IF( lp_obc_west  .AND. .NOT.lfbcwest  )   CALL obc_rad_west ( kt )   ! West open boundary

      IF( lp_obc_north .AND. .NOT.lfbcnorth )   CALL obc_rad_north( kt )   ! North open boundary

      IF( lp_obc_south .AND. .NOT.lfbcsouth )   CALL obc_rad_south( kt )   ! South open boundary

   END SUBROUTINE obc_rad


   SUBROUTINE obc_rad_east ( kt )
      !!------------------------------------------------------------------------------
      !!                     ***  SUBROUTINE obc_rad_east  ***
      !!                   
      !! ** Purpose :
      !!      Perform swap of arrays to calculate radiative phase speeds at the open 
      !!      east boundary and calculate those phase speeds if this OBC is not fixed.
      !!      In case of fixed OBC, this subrountine is not called.
      !!
      !!  History :
      !!         ! 95-03 (J.-M. Molines) Original from SPEM
      !!         ! 97-07 (G. Madec, J.-M. Molines) additions
      !!         ! 97-12 (M. Imbard) Mpp adaptation
      !!         ! 00-06 (J.-M. Molines) 
      !!    8.5  ! 02-10 (C. Talandier, A-M. Treguier) Free surface, F90
      !!------------------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt

      !! * Local declarations
      INTEGER  ::   ij
      REAL(wp) ::   z05cx, zdt, z4nor2, z2dx, z2dy
      REAL(wp) ::   zucb, zucbm, zucbm2
      !!------------------------------------------------------------------------------

      ! 1. Swap arrays before calculating radiative velocities
      ! ------------------------------------------------------

      ! 1.1  zonal velocity 
      ! -------------------

      IF( kt > nit000 .OR. ln_rstart ) THEN 

         ! ... advance in time (time filter, array swap) 
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               uebnd(jj,jk,nib  ,nitm2) = uebnd(jj,jk,nib  ,nitm)*uemsk(jj,jk)
               uebnd(jj,jk,nibm ,nitm2) = uebnd(jj,jk,nibm ,nitm)*uemsk(jj,jk)
               uebnd(jj,jk,nibm2,nitm2) = uebnd(jj,jk,nibm2,nitm)*uemsk(jj,jk)
            END DO
         END DO
         ! ... fields nitm <== nit  plus time filter at the boundary 
         DO ji = fs_nie0, fs_nie1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  uebnd(jj,jk,nib  ,nitm) = uebnd(jj,jk,nib,  nit)*uemsk(jj,jk)
                  uebnd(jj,jk,nibm ,nitm) = uebnd(jj,jk,nibm ,nit)*uemsk(jj,jk)
                  uebnd(jj,jk,nibm2,nitm) = uebnd(jj,jk,nibm2,nit)*uemsk(jj,jk)
         ! ... fields nit <== now (kt+1) 
         ! ... Total or baroclinic velocity at b, bm and bm2
                  zucb   = un(ji,jj,jk)
                  zucbm  = un(ji-1,jj,jk)
                  zucbm2 = un(ji-2,jj,jk)
                  uebnd(jj,jk,nib  ,nit) = zucb   *uemsk(jj,jk)
                  uebnd(jj,jk,nibm ,nit) = zucbm  *uemsk(jj,jk) 
                  uebnd(jj,jk,nibm2,nit) = zucbm2 *uemsk(jj,jk) 
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(uebnd,jpjed,jpjef,jpieob,jpk*3*3,2,jpj, numout )

         ! ... extremeties nie0, nie1
         ij = jpjed +1 - njmpp
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN
            DO jk = 1,jpkm1
               uebnd(ij,jk,nibm,nitm) = uebnd(ij+1 ,jk,nibm,nitm)
            END DO
         END IF
         ij = jpjef +1 - njmpp
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN
            DO jk = 1,jpkm1
               uebnd(ij,jk,nibm,nitm) = uebnd(ij-1 ,jk,nibm,nitm)
            END DO
         END IF

         ! 1.2 tangential velocity
         ! -----------------------

         ! ... advance in time (time filter, array swap)
         DO jk = 1, jpkm1
            DO jj = 1, jpj
         ! ... fields nitm2 <== nitm
               vebnd(jj,jk,nib  ,nitm2) = vebnd(jj,jk,nib  ,nitm)*vemsk(jj,jk)
               vebnd(jj,jk,nibm ,nitm2) = vebnd(jj,jk,nibm ,nitm)*vemsk(jj,jk)
               vebnd(jj,jk,nibm2,nitm2) = vebnd(jj,jk,nibm2,nitm)*vemsk(jj,jk)
            END DO
         END DO

         DO ji = fs_nie0+1, fs_nie1+1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  vebnd(jj,jk,nib  ,nitm) = vebnd(jj,jk,nib,  nit)*vemsk(jj,jk)
                  vebnd(jj,jk,nibm ,nitm) = vebnd(jj,jk,nibm ,nit)*vemsk(jj,jk)
                  vebnd(jj,jk,nibm2,nitm) = vebnd(jj,jk,nibm2,nit)*vemsk(jj,jk)
         ! ... fields nit <== now (kt+1)
                  vebnd(jj,jk,nib  ,nit) = vn(ji  ,jj,jk)*vemsk(jj,jk)
                  vebnd(jj,jk,nibm ,nit) = vn(ji-1,jj,jk)*vemsk(jj,jk)
                  vebnd(jj,jk,nibm2,nit) = vn(ji-2,jj,jk)*vemsk(jj,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(vebnd,jpjed,jpjef,jpieob+1,jpk*3*3,2,jpj, numout )

         !... extremeties nie0, nie1
         ij = jpjed +1 - njmpp
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN
            DO jk = 1,jpkm1
               vebnd(ij,jk,nibm,nitm) = vebnd(ij+1 ,jk,nibm,nitm)
            END DO 
         END IF 
         ij = jpjef +1 - njmpp 
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN 
            DO jk = 1,jpkm1 
               vebnd(ij,jk,nibm,nitm) = vebnd(ij-1 ,jk,nibm,nitm)
            END DO 
         END IF 

         ! 1.3 Temperature and salinity
         ! ----------------------------

         ! ... advance in time (time filter, array swap)
         DO jk = 1, jpkm1
            DO jj = 1, jpj
         ! ... fields nitm <== nit  plus time filter at the boundary
               tebnd(jj,jk,nib,nitm) = tebnd(jj,jk,nib,nit)*temsk(jj,jk)
               sebnd(jj,jk,nib,nitm) = sebnd(jj,jk,nib,nit)*temsk(jj,jk)
            END DO
         END DO

         DO ji = fs_nie0+1, fs_nie1+1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  tebnd(jj,jk,nibm,nitm) = tebnd(jj,jk,nibm,nit)*temsk(jj,jk)
                  sebnd(jj,jk,nibm,nitm) = sebnd(jj,jk,nibm,nit)*temsk(jj,jk)
         ! ... fields nit <== now (kt+1)
                  tebnd(jj,jk,nib  ,nit) = tsn(ji  ,jj,jk,jp_tem)*temsk(jj,jk)
                  tebnd(jj,jk,nibm ,nit) = tsn(ji-1,jj,jk,jp_tem)*temsk(jj,jk)
                  sebnd(jj,jk,nib  ,nit) = tsn(ji  ,jj,jk,jp_sal)*temsk(jj,jk)
                  sebnd(jj,jk,nibm ,nit) = tsn(ji-1,jj,jk,jp_sal)*temsk(jj,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(tebnd,jpjed,jpjef,jpieob+1,jpk*2*2,2,jpj, numout )
         IF( lk_mpp )   CALL mppobc(sebnd,jpjed,jpjef,jpieob+1,jpk*2*2,2,jpj, numout )

         ! ... extremeties nie0, nie1
         ij = jpjed +1 - njmpp
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN
            DO jk = 1,jpkm1
               tebnd(ij,jk,nibm,nitm) = tebnd(ij+1 ,jk,nibm,nitm)
               sebnd(ij,jk,nibm,nitm) = sebnd(ij+1 ,jk,nibm,nitm)
            END DO
         END IF
         ij = jpjef +1 - njmpp
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN
            DO jk = 1,jpkm1
               tebnd(ij,jk,nibm,nitm) = tebnd(ij-1 ,jk,nibm,nitm)
               sebnd(ij,jk,nibm,nitm) = sebnd(ij-1 ,jk,nibm,nitm)
            END DO
         END IF

      END IF     ! End of array swap

      ! 2 - Calculation of radiation velocities
      ! ---------------------------------------

      IF( kt >= nit000 +3 .OR. ln_rstart ) THEN

         ! 2.1  Calculate the normal velocity U based on phase velocity u_cxebnd
         ! ---------------------------------------------------------------------
         !
         !          nibm2      nibm      nib
         !            |  nibm   |   nib   |///
         !            |    |    |    |    |///
         !  jj-line --f----v----f----v----f---
         !            |    |    |    |    |///
         !            |         |         |///
         !  jj-line   u    T    u    T    u///
         !            |         |         |///
         !            |    |    |    |    |///
         !          jpieob-2   jpieob-1   jpieob
         !                 |         |        
         !              jpieob-1    jpieob      
         !
         ! ... (jpjedp1, jpjefm1),jpieob
         DO ji = fs_nie0, fs_nie1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 2, jpjm1
         ! ... 2* gradi(u) (T-point i=nibm, time mean)
                  z2dx = ( uebnd(jj,jk,nibm ,nit) + uebnd(jj,jk,nibm ,nitm2) &
                           - 2.*uebnd(jj,jk,nibm2,nitm) ) / e1t(ji-1,jj)
         ! ... 2* gradj(u) (u-point i=nibm, time nitm)
                  z2dy = ( uebnd(jj+1,jk,nibm,nitm) - uebnd(jj-1,jk,nibm,nitm) ) / e2u(ji-1,jj)
         ! ... square of the norm of grad(u)
                  z4nor2 = z2dx * z2dx + z2dy * z2dy
         ! ... minus time derivative (leap-frog) at nibm, without / 2 dt
                  zdt = uebnd(jj,jk,nibm,nitm2) - uebnd(jj,jk,nibm,nit)
         ! ... i-phase speed ratio (bounded by 1)               
                  IF( z4nor2 == 0. ) THEN
                     z4nor2=.00001
                  END IF
                  z05cx = zdt * z2dx / z4nor2
                  u_cxebnd(jj,jk) = z05cx*uemsk(jj,jk)
               END DO
            END DO
         END DO

         ! 2.2  Calculate the tangential velocity based on phase velocity v_cxebnd
         ! -----------------------------------------------------------------------
         !
         !          nibm2      nibm      nib
         !            |   nibm  |   nib///|///
         !            |    |    |    |////|///
         !  jj-line --v----f----v----f----v---
         !            |    |    |    |////|///
         !            |    |    |    |////|///
         !            | jpieob-1| jpieob /|///
         !            |         |         |   
         !         jpieob-1    jpieob     jpieob+1
         !
         ! ... (jpjedp1, jpjefm1), jpieob+1
         DO ji = fs_nie0+1, fs_nie1+1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 2, jpjm1
         ! ... 2* i-gradient of v (f-point i=nibm, time mean)
                  z2dx = ( vebnd(jj,jk,nibm ,nit) + vebnd(jj,jk,nibm ,nitm2) &
                          - 2.*vebnd(jj,jk,nibm2,nitm) ) / e1f(ji-2,jj)
         ! ... 2* j-gradient of v (v-point i=nibm, time nitm)
                  z2dy = ( vebnd(jj+1,jk,nibm,nitm) -  vebnd(jj-1,jk,nibm,nitm) ) / e2v(ji-1,jj)
         ! ... square of the norm of grad(v)
                  z4nor2 = z2dx * z2dx + z2dy * z2dy
         ! ... minus time derivative (leap-frog) at nibm, without / 2 dt
                  zdt = vebnd(jj,jk,nibm,nitm2) - vebnd(jj,jk,nibm,nit)
         ! ... i-phase speed ratio (bounded by 1) and save the unbounded phase
         !     velocity ratio no divided by e1f for the tracer radiation
                  IF( z4nor2 == 0. ) THEN
                     z4nor2=.000001
                  END IF
                  z05cx = zdt * z2dx / z4nor2
                  v_cxebnd(jj,jk) = z05cx*vemsk(jj,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(v_cxebnd,jpjed,jpjef,jpieob+1,jpk,2,jpj, numout )

         ! ... extremeties nie0, nie1
         ij = jpjed +1 - njmpp
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN
            DO jk = 1,jpkm1
               v_cxebnd(ij,jk) = v_cxebnd(ij+1 ,jk)
            END DO
         END IF
         ij = jpjef +1 - njmpp
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN
            DO jk = 1,jpkm1
               v_cxebnd(ij,jk) = v_cxebnd(ij-1 ,jk)
            END DO
         END IF

      END IF

   END SUBROUTINE obc_rad_east


   SUBROUTINE obc_rad_west ( kt )
      !!------------------------------------------------------------------------------
      !!                  ***  SUBROUTINE obc_rad_west  ***
      !!                    
      !! ** Purpose :
      !!      Perform swap of arrays to calculate radiative phase speeds at the open 
      !!      west boundary and calculate those phase speeds if this OBC is not fixed.
      !!      In case of fixed OBC, this subrountine is not called.
      !!
      !!  History :
      !!         ! 95-03 (J.-M. Molines) Original from SPEM
      !!         ! 97-07 (G. Madec, J.-M. Molines) additions
      !!         ! 97-12 (M. Imbard) Mpp adaptation
      !!         ! 00-06 (J.-M. Molines) 
      !!    8.5  ! 02-10 (C. Talandier, A-M. Treguier) Free surface, F90
      !!------------------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt

      !! * Local declarations
      INTEGER ::   ij
      REAL(wp) ::   z05cx, zdt, z4nor2, z2dx, z2dy
      REAL(wp) ::   zucb, zucbm, zucbm2
      !!------------------------------------------------------------------------------

      ! 1. Swap arrays before calculating radiative velocities
      ! ------------------------------------------------------

      ! 1.1  zonal velocity 
      ! -------------------

      IF( kt > nit000 .OR. ln_rstart ) THEN

         ! ... advance in time (time filter, array swap) 
         DO jk = 1, jpkm1
            DO jj = 1, jpj 
               uwbnd(jj,jk,nib  ,nitm2) = uwbnd(jj,jk,nib  ,nitm)*uwmsk(jj,jk)
               uwbnd(jj,jk,nibm ,nitm2) = uwbnd(jj,jk,nibm ,nitm)*uwmsk(jj,jk)
               uwbnd(jj,jk,nibm2,nitm2) = uwbnd(jj,jk,nibm2,nitm)*uwmsk(jj,jk)
            END DO
         END DO

         ! ... fields nitm <== nit  plus time filter at the boundary 
         DO ji = fs_niw0, fs_niw1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  uwbnd(jj,jk,nib  ,nitm) = uwbnd(jj,jk,nib  ,nit)*uwmsk(jj,jk)
                  uwbnd(jj,jk,nibm ,nitm) = uwbnd(jj,jk,nibm ,nit)*uwmsk(jj,jk)
                  uwbnd(jj,jk,nibm2,nitm) = uwbnd(jj,jk,nibm2,nit)*uwmsk(jj,jk)
         ! ... total or baroclinic velocity at b, bm and bm2
                  zucb   = un (ji,jj,jk)
                  zucbm  = un (ji+1,jj,jk)
                  zucbm2 = un (ji+2,jj,jk)

         ! ... fields nit <== now (kt+1) 
                  uwbnd(jj,jk,nib  ,nit) = zucb  *uwmsk(jj,jk)
                  uwbnd(jj,jk,nibm ,nit) = zucbm *uwmsk(jj,jk)
                  uwbnd(jj,jk,nibm2,nit) = zucbm2*uwmsk(jj,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(uwbnd,jpjwd,jpjwf,jpiwob,jpk*3*3,2,jpj, numout )

         ! ... extremeties niw0, niw1
         ij = jpjwd +1 - njmpp
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN
            DO jk = 1,jpkm1
               uwbnd(ij,jk,nibm,nitm) = uwbnd(ij+1 ,jk,nibm,nitm)
            END DO
         END IF
         ij = jpjwf +1 - njmpp
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN
            DO jk = 1,jpkm1
               uwbnd(ij,jk,nibm,nitm) = uwbnd(ij-1 ,jk,nibm,nitm)
            END DO
         END IF

         ! 1.2 tangential velocity
         ! -----------------------

         ! ... advance in time (time filter, array swap)
         DO jk = 1, jpkm1
            DO jj = 1, jpj 
         ! ... fields nitm2 <== nitm
                  vwbnd(jj,jk,nib  ,nitm2) = vwbnd(jj,jk,nib  ,nitm)*vwmsk(jj,jk)
                  vwbnd(jj,jk,nibm ,nitm2) = vwbnd(jj,jk,nibm ,nitm)*vwmsk(jj,jk)
                  vwbnd(jj,jk,nibm2,nitm2) = vwbnd(jj,jk,nibm2,nitm)*vwmsk(jj,jk)
            END DO
         END DO

         DO ji = fs_niw0, fs_niw1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  vwbnd(jj,jk,nib  ,nitm) = vwbnd(jj,jk,nib,  nit)*vwmsk(jj,jk)
                  vwbnd(jj,jk,nibm ,nitm) = vwbnd(jj,jk,nibm ,nit)*vwmsk(jj,jk)
                  vwbnd(jj,jk,nibm2,nitm) = vwbnd(jj,jk,nibm2,nit)*vwmsk(jj,jk)
         ! ... fields nit <== now (kt+1)
                  vwbnd(jj,jk,nib  ,nit) = vn(ji  ,jj,jk)*vwmsk(jj,jk)
                  vwbnd(jj,jk,nibm ,nit) = vn(ji+1,jj,jk)*vwmsk(jj,jk)
                  vwbnd(jj,jk,nibm2,nit) = vn(ji+2,jj,jk)*vwmsk(jj,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(vwbnd,jpjwd,jpjwf,jpiwob,jpk*3*3,2,jpj, numout )

         ! ... extremeties niw0, niw1 
         ij = jpjwd +1 - njmpp 
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN 
            DO jk = 1,jpkm1 
               vwbnd(ij,jk,nibm,nitm) = vwbnd(ij+1 ,jk,nibm,nitm)
            END DO 
         END IF
         ij = jpjwf +1 - njmpp 
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN 
            DO jk = 1,jpkm1 
               vwbnd(ij,jk,nibm,nitm) = vwbnd(ij-1 ,jk,nibm,nitm)
            END DO 
         END IF 
 
         ! 1.3 Temperature and salinity
         ! ----------------------------
 
         ! ... advance in time (time filter, array swap)
         DO jk = 1, jpkm1
            DO jj = 1, jpj
         ! ... fields nitm <== nit  plus time filter at the boundary
               twbnd(jj,jk,nib,nitm) = twbnd(jj,jk,nib,nit)*twmsk(jj,jk)
               swbnd(jj,jk,nib,nitm) = swbnd(jj,jk,nib,nit)*twmsk(jj,jk)
            END DO
         END DO
 
         DO ji = fs_niw0, fs_niw1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  twbnd(jj,jk,nibm ,nitm) = twbnd(jj,jk,nibm ,nit)*twmsk(jj,jk)
                  swbnd(jj,jk,nibm ,nitm) = swbnd(jj,jk,nibm ,nit)*twmsk(jj,jk)
         ! ... fields nit <== now (kt+1)
                  twbnd(jj,jk,nib  ,nit) = tsn(ji   ,jj,jk,jp_tem)*twmsk(jj,jk)
                  twbnd(jj,jk,nibm ,nit) = tsn(ji+1 ,jj,jk,jp_tem)*twmsk(jj,jk)
                  swbnd(jj,jk,nib  ,nit) = tsn(ji   ,jj,jk,jp_sal)*twmsk(jj,jk)
                  swbnd(jj,jk,nibm ,nit) = tsn(ji+1 ,jj,jk,jp_sal)*twmsk(jj,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(twbnd,jpjwd,jpjwf,jpiwob,jpk*2*2,2,jpj, numout )
         IF( lk_mpp )   CALL mppobc(swbnd,jpjwd,jpjwf,jpiwob,jpk*2*2,2,jpj, numout )

         ! ... extremeties niw0, niw1
         ij = jpjwd +1 - njmpp
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN
            DO jk = 1,jpkm1
               twbnd(ij,jk,nibm,nitm) = twbnd(ij+1 ,jk,nibm,nitm)
               swbnd(ij,jk,nibm,nitm) = swbnd(ij+1 ,jk,nibm,nitm)
            END DO
         END IF
         ij = jpjwf +1 - njmpp
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN
            DO jk = 1,jpkm1
               twbnd(ij,jk,nibm,nitm) = twbnd(ij-1 ,jk,nibm,nitm)
               swbnd(ij,jk,nibm,nitm) = swbnd(ij-1 ,jk,nibm,nitm)
            END DO
         END IF
 
      END IF     ! End of array swap

      ! 2 - Calculation of radiation velocities
      ! ---------------------------------------
   
      IF( kt >= nit000 +3 .OR. ln_rstart ) THEN
  
         ! 2.1  Calculate the normal velocity U based on phase velocity u_cxwbnd
         ! ----------------------------------------------------------------------
         !
         !          nib       nibm      nibm2
         !        ///|   nib   |   nibm  |
         !        ///|    |    |    |    |
         !        ---f----v----f----v----f-- jj-line
         !        ///|    |    |    |    |
         !        ///|         |         |
         !        ///u    T    u    T    u   jj-line
         !        ///|         |         |
         !        ///|    |    |    |    |
         !         jpiwob    jpiwob+1    jpiwob+2
         !                |         |        
         !              jpiwob+1    jpiwob+2     
         !
         ! ... If free surface formulation:
         ! ... radiative conditions on the total part + relaxation toward climatology
         ! ... (jpjwdp1, jpjwfm1), jpiwob
         DO ji = fs_niw0, fs_niw1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 2, jpjm1
         ! ... 2* gradi(u) (T-point i=nibm, time mean)
                  z2dx = ( - uwbnd(jj,jk,nibm ,nit) -  uwbnd(jj,jk,nibm ,nitm2) &
                           + 2.*uwbnd(jj,jk,nibm2,nitm) ) / e1t(ji+2,jj)
         ! ... 2* gradj(u) (u-point i=nibm, time nitm)
                  z2dy = ( uwbnd(jj+1,jk,nibm,nitm) - uwbnd(jj-1,jk,nibm,nitm) ) / e2u(ji+1,jj)
         ! ... square of the norm of grad(u)
                  z4nor2 = z2dx * z2dx + z2dy * z2dy
         ! ... minus time derivative (leap-frog) at nibm, without / 2 dt
                  zdt = uwbnd(jj,jk,nibm,nitm2) - uwbnd(jj,jk,nibm,nit)
         ! ... i-phase speed ratio (bounded by -1)
                  IF( z4nor2 == 0. ) THEN
                     z4nor2=0.00001
                  END IF
                  z05cx = zdt * z2dx / z4nor2
                  u_cxwbnd(jj,jk)=z05cx*uwmsk(jj,jk)
               END DO
            END DO
         END DO

         ! 2.2  Calculate the tangential velocity based on phase velocity v_cxwbnd
         ! -----------------------------------------------------------------------
         !
         !      nib       nibm     nibm2
         !    ///|///nib   |   nibm  |  nibm2
         !    ///|////|    |    |    |    |    |
         !    ---v----f----v----f----v----f----v-- jj-line
         !    ///|////|    |    |    |    |    |
         !    ///|////|    |    |    |    |    |
         !   jpiwob     jpiwob+1    jpiwob+2
         !            |         |         |   
         !          jpiwob   jpiwob+1   jpiwob+2    
         !
         ! ... radiative condition plus Raymond-Kuo
         ! ... (jpjwdp1, jpjwfm1),jpiwob
         DO ji = fs_niw0, fs_niw1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 2, jpjm1
         ! ... 2* i-gradient of v (f-point i=nibm, time mean)
                  z2dx = ( - vwbnd(jj,jk,nibm ,nit) - vwbnd(jj,jk,nibm ,nitm2) &
                           + 2.*vwbnd(jj,jk,nibm2,nitm) ) / e1f(ji+1,jj)
         ! ... 2* j-gradient of v (v-point i=nibm, time nitm)
                  z2dy = ( vwbnd(jj+1,jk,nibm,nitm) - vwbnd(jj-1,jk,nibm,nitm) ) / e2v(ji+1,jj)
         ! ... square of the norm of grad(v)
                  z4nor2 = z2dx * z2dx + z2dy * z2dy
         ! ... minus time derivative (leap-frog) at nibm, without / 2 dt
                  zdt = vwbnd(jj,jk,nibm,nitm2) - vwbnd(jj,jk,nibm,nit)
         ! ... i-phase speed ratio (bounded by -1) and save the unbounded phase
         !     velocity ratio no divided by e1f for the tracer radiation
                  IF( z4nor2 == 0) THEN
                     z4nor2=0.000001
                  endif
                  z05cx = zdt * z2dx / z4nor2
                  v_cxwbnd(jj,jk) = z05cx*vwmsk(jj,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(v_cxwbnd,jpjwd,jpjwf,jpiwob,jpk,2,jpj, numout )

         ! ... extremeties niw0, niw1
         ij = jpjwd +1 - njmpp
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN
            DO jk = 1,jpkm1
               v_cxwbnd(ij,jk) = v_cxwbnd(ij+1 ,jk)
            END DO
         END IF
         ij = jpjwf +1 - njmpp
         IF( ij >= 2 .AND. ij < jpjm1 ) THEN
            DO jk = 1,jpkm1
               v_cxwbnd(ij,jk) = v_cxwbnd(ij-1 ,jk)
            END DO
         END IF

      END IF

   END SUBROUTINE obc_rad_west


   SUBROUTINE obc_rad_north ( kt )
      !!------------------------------------------------------------------------------
      !!                  ***  SUBROUTINE obc_rad_north  ***
      !!           
      !! ** Purpose :
      !!      Perform swap of arrays to calculate radiative phase speeds at the open 
      !!      north boundary and calculate those phase speeds if this OBC is not fixed.
      !!      In case of fixed OBC, this subrountine is not called.
      !!
      !!  History :
      !!         ! 95-03 (J.-M. Molines) Original from SPEM
      !!         ! 97-07 (G. Madec, J.-M. Molines) additions
      !!         ! 97-12 (M. Imbard) Mpp adaptation
      !!         ! 00-06 (J.-M. Molines) 
      !!    8.5  ! 02-10 (C. Talandier, A-M. Treguier) Free surface, F90
      !!------------------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt

      !! * Local declarations
      INTEGER  ::   ii
      REAL(wp) ::   z05cx, zdt, z4nor2, z2dx, z2dy
      REAL(wp) ::   zvcb, zvcbm, zvcbm2
      !!------------------------------------------------------------------------------

      ! 1. Swap arrays before calculating radiative velocities
      ! ------------------------------------------------------

      ! 1.1  zonal velocity 
      ! -------------------

      IF( kt > nit000 .OR. ln_rstart ) THEN 

         ! ... advance in time (time filter, array swap)
         DO jk = 1, jpkm1
            DO ji = 1, jpi
         ! ... fields nitm2 <== nitm
               unbnd(ji,jk,nib  ,nitm2) = unbnd(ji,jk,nib  ,nitm)*unmsk(ji,jk)
               unbnd(ji,jk,nibm ,nitm2) = unbnd(ji,jk,nibm ,nitm)*unmsk(ji,jk)
               unbnd(ji,jk,nibm2,nitm2) = unbnd(ji,jk,nibm2,nitm)*unmsk(ji,jk)
            END DO
         END DO

         DO jj = fs_njn0+1, fs_njn1+1  ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  unbnd(ji,jk,nib  ,nitm) = unbnd(ji,jk,nib,  nit)*unmsk(ji,jk)
                  unbnd(ji,jk,nibm ,nitm) = unbnd(ji,jk,nibm ,nit)*unmsk(ji,jk)
                  unbnd(ji,jk,nibm2,nitm) = unbnd(ji,jk,nibm2,nit)*unmsk(ji,jk)
         ! ... fields nit <== now (kt+1)
                  unbnd(ji,jk,nib  ,nit) = un(ji,jj,  jk)*unmsk(ji,jk)
                  unbnd(ji,jk,nibm ,nit) = un(ji,jj-1,jk)*unmsk(ji,jk)
                  unbnd(ji,jk,nibm2,nit) = un(ji,jj-2,jk)*unmsk(ji,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(unbnd,jpind,jpinf,jpjnob+1,jpk*3*3,1,jpi, numout )

         ! ... extremeties njn0,njn1 
         ii = jpind + 1 - nimpp 
         IF( ii >= 2 .AND. ii < jpim1 ) THEN 
            DO jk = 1, jpkm1
                unbnd(ii,jk,nibm,nitm) = unbnd(ii+1,jk,nibm,nitm)
            END DO
         END IF 
         ii = jpinf + 1 - nimpp 
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               unbnd(ii,jk,nibm,nitm) = unbnd(ii-1,jk,nibm,nitm)
            END DO
         END IF
 
         ! 1.2. normal velocity 
         ! --------------------

         ! ... advance in time (time filter, array swap) 
         DO jk = 1, jpkm1
            DO ji = 1, jpi
         ! ... fields nitm2 <== nitm 
               vnbnd(ji,jk,nib  ,nitm2) = vnbnd(ji,jk,nib  ,nitm)*vnmsk(ji,jk)
               vnbnd(ji,jk,nibm ,nitm2) = vnbnd(ji,jk,nibm ,nitm)*vnmsk(ji,jk)
               vnbnd(ji,jk,nibm2,nitm2) = vnbnd(ji,jk,nibm2,nitm)*vnmsk(ji,jk)
            END DO
         END DO

         DO jj = fs_njn0, fs_njn1  ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  vnbnd(ji,jk,nib  ,nitm) = vnbnd(ji,jk,nib,  nit)*vnmsk(ji,jk)
                  vnbnd(ji,jk,nibm ,nitm) = vnbnd(ji,jk,nibm ,nit)*vnmsk(ji,jk)
                  vnbnd(ji,jk,nibm2,nitm) = vnbnd(ji,jk,nibm2,nit)*vnmsk(ji,jk)
         ! ... fields nit <== now (kt+1)
         ! ... total or baroclinic velocity at b, bm and bm2
                  zvcb   = vn (ji,jj,jk)
                  zvcbm  = vn (ji,jj-1,jk)
                  zvcbm2 = vn (ji,jj-2,jk)
         ! ... fields nit <== now (kt+1) 
                  vnbnd(ji,jk,nib  ,nit) = zvcb  *vnmsk(ji,jk)
                  vnbnd(ji,jk,nibm ,nit) = zvcbm *vnmsk(ji,jk)
                  vnbnd(ji,jk,nibm2,nit) = zvcbm2*vnmsk(ji,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(vnbnd,jpind,jpinf,jpjnob,jpk*3*3,1,jpi, numout )

         ! ... extremeties njn0,njn1
         ii = jpind + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               vnbnd(ii,jk,nibm,nitm) = vnbnd(ii+1,jk,nibm,nitm)
            END DO
         END IF
         ii = jpinf + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               vnbnd(ii,jk,nibm,nitm) = vnbnd(ii-1,jk,nibm,nitm)
            END DO
         END IF

         ! 1.3 Temperature and salinity
         ! ----------------------------

         ! ... advance in time (time filter, array swap)
         DO jk = 1, jpkm1
            DO ji = 1, jpi
         ! ... fields nitm <== nit  plus time filter at the boundary
               tnbnd(ji,jk,nib ,nitm) = tnbnd(ji,jk,nib,nit)*tnmsk(ji,jk)
               snbnd(ji,jk,nib ,nitm) = snbnd(ji,jk,nib,nit)*tnmsk(ji,jk)
            END DO
         END DO

         DO jj = fs_njn0+1, fs_njn1+1  ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  tnbnd(ji,jk,nibm ,nitm) = tnbnd(ji,jk,nibm ,nit)*tnmsk(ji,jk)
                  snbnd(ji,jk,nibm ,nitm) = snbnd(ji,jk,nibm ,nit)*tnmsk(ji,jk)
         ! ... fields nit <== now (kt+1)
                  tnbnd(ji,jk,nib  ,nit) = tsn(ji,jj,  jk,jp_tem)*tnmsk(ji,jk)
                  tnbnd(ji,jk,nibm ,nit) = tsn(ji,jj-1,jk,jp_tem)*tnmsk(ji,jk)
                  snbnd(ji,jk,nib  ,nit) = tsn(ji,jj,  jk,jp_sal)*tnmsk(ji,jk)
                  snbnd(ji,jk,nibm ,nit) = tsn(ji,jj-1,jk,jp_sal)*tnmsk(ji,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(tnbnd,jpind,jpinf,jpjnob+1,jpk*2*2,1,jpi, numout )
         IF( lk_mpp )   CALL mppobc(snbnd,jpind,jpinf,jpjnob+1,jpk*2*2,1,jpi, numout )

         ! ... extremeties  njn0,njn1
         ii = jpind + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               tnbnd(ii,jk,nibm,nitm) = tnbnd(ii+1,jk,nibm,nitm)
               snbnd(ii,jk,nibm,nitm) = snbnd(ii+1,jk,nibm,nitm)
            END DO
         END IF
         ii = jpinf + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               tnbnd(ii,jk,nibm,nitm) = tnbnd(ii-1,jk,nibm,nitm)
               snbnd(ii,jk,nibm,nitm) = snbnd(ii-1,jk,nibm,nitm)
            END DO
         END IF

      END IF     ! End of array swap

      ! 2 - Calculation of radiation velocities
      ! ---------------------------------------

      IF( kt >= nit000 +3 .OR. ln_rstart ) THEN

         ! 2.1  Calculate the normal velocity based on phase velocity u_cynbnd
         ! -------------------------------------------------------------------
         !
         !           ji-row
         !             |
         !     nib -///u//////  jpjnob + 1
         !        /////|//////
         !   nib  -----f-----   jpjnob
         !             |    
         !     nibm--  u   ---- jpjnob
         !             |        
         !  nibm  -----f-----   jpjnob-1
         !             |        
         !    nibm2--  u   ---- jpjnob-1
         !             |        
         !  nibm2 -----f-----   jpjnob-2
         !             |
         ! ... radiative condition
         ! ... jpjnob+1,(jpindp1, jpinfm1)
         DO jj = fs_njn0+1, fs_njn1+1  ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 2, jpim1
         ! ... 2* j-gradient of u (f-point i=nibm, time mean)
                  z2dx = ( unbnd(ji,jk,nibm ,nit) + unbnd(ji,jk,nibm ,nitm2) &
                        - 2.*unbnd(ji,jk,nibm2,nitm)) / e2f(ji,jj-2)
         ! ... 2* i-gradient of u (u-point i=nibm, time nitm)
                  z2dy = ( unbnd(ji+1,jk,nibm,nitm) - unbnd(ji-1,jk,nibm,nitm) ) / e1u(ji,jj-1)
         ! ... square of the norm of grad(v)
                  z4nor2 = z2dx * z2dx + z2dy * z2dy
         ! ... minus time derivative (leap-frog) at nibm, without / 2 dt
                  zdt = unbnd(ji,jk,nibm,nitm2) - unbnd(ji,jk,nibm,nit)
         ! ... i-phase speed ratio (bounded by 1) and save the unbounded phase
         !     velocity ratio no divided by e1f for the tracer radiation
                  IF( z4nor2 == 0.) THEN
                     z4nor2=.000001
                  END IF
                  z05cx = zdt * z2dx / z4nor2
                  u_cynbnd(ji,jk) = z05cx *unmsk(ji,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(u_cynbnd,jpind,jpinf,jpjnob+1,jpk,1,jpi, numout )

         ! ... extremeties  njn0,njn1
         ii = jpind + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               u_cynbnd(ii,jk) = u_cynbnd(ii+1,jk)
            END DO
         END IF
         ii = jpinf + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               u_cynbnd(ii,jk) = u_cynbnd(ii-1,jk)
            END DO
         END IF

         ! 2.2 Calculate the normal velocity based on phase velocity v_cynbnd 
         ! ------------------------------------------------------------------
         !
         !                ji-row  ji-row
         !                     |
         !        /////|/////////////////
         !   nib  -----f----v----f----  jpjnob
         !             |         |
         !     nib  -  u -- T -- u ---- jpjnob
         !             |         |
         !  nibm  -----f----v----f----  jpjnob-1
         !             |         |
         !    nibm --  u -- T -- u ---  jpjnob-1
         !             |         |
         !  nibm2 -----f----v----f----  jpjnob-2
         !             |         |
         ! ... Free surface formulation:
         ! ... radiative conditions on the total part + relaxation toward climatology 
         ! ... jpjnob,(jpindp1, jpinfm1)
         DO jj = fs_njn0, fs_njn1  ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 2, jpim1
         ! ... 2* gradj(v) (T-point i=nibm, time mean)
                  ii = ji -1 + nimpp
                  z2dx = ( vnbnd(ji,jk,nibm ,nit) + vnbnd(ji,jk,nibm ,nitm2) &
                          - 2.*vnbnd(ji,jk,nibm2,nitm)) / e2t(ji,jj-1)
         ! ... 2* gradi(v) (v-point i=nibm, time nitm)
                  z2dy = ( vnbnd(ji+1,jk,nibm,nitm) - vnbnd(ji-1,jk,nibm,nitm) ) / e1v(ji,jj-1)
         ! ... square of the norm of grad(u)
                  z4nor2 = z2dx * z2dx + z2dy * z2dy
         ! ... minus time derivative (leap-frog) at nibm, without / 2 dt
                  zdt = vnbnd(ji,jk,nibm,nitm2) - vnbnd(ji,jk,nibm,nit)
         ! ... j-phase speed ratio (bounded by 1)
                  IF( z4nor2 == 0. ) THEN
                     z4nor2=.00001
                  END IF
                  z05cx = zdt * z2dx / z4nor2
                  v_cynbnd(ji,jk)=z05cx *vnmsk(ji,jk)
               END DO
            END DO
         END DO

      END IF

   END SUBROUTINE obc_rad_north


   SUBROUTINE obc_rad_south ( kt )
      !!------------------------------------------------------------------------------
      !!                  ***  SUBROUTINE obc_rad_south  ***
      !!           
      !! ** Purpose :
      !!      Perform swap of arrays to calculate radiative phase speeds at the open 
      !!      south boundary and calculate those phase speeds if this OBC is not fixed.
      !!      In case of fixed OBC, this subrountine is not called.
      !!
      !!  History :
      !!         ! 95-03 (J.-M. Molines) Original from SPEM
      !!         ! 97-07 (G. Madec, J.-M. Molines) additions
      !!         ! 97-12 (M. Imbard) Mpp adaptation
      !!         ! 00-06 (J.-M. Molines) 
      !!    8.5  ! 02-10 (C. Talandier, A-M. Treguier) Free surface, F90
      !!------------------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt

      !! * Local declarations
      INTEGER ::   ii
      REAL(wp) ::   z05cx, zdt, z4nor2, z2dx, z2dy
      REAL(wp) ::   zvcb, zvcbm, zvcbm2
      !!------------------------------------------------------------------------------

      ! 1. Swap arrays before calculating radiative velocities
      ! ------------------------------------------------------

      ! 1.1  zonal velocity 
      ! --------------------
  
      IF( kt > nit000 .OR. ln_rstart ) THEN 

         ! ... advance in time (time filter, array swap)
         DO jk = 1, jpkm1
            DO ji = 1, jpi
         ! ... fields nitm2 <== nitm
                  usbnd(ji,jk,nib  ,nitm2) = usbnd(ji,jk,nib  ,nitm)*usmsk(ji,jk)
                  usbnd(ji,jk,nibm ,nitm2) = usbnd(ji,jk,nibm ,nitm)*usmsk(ji,jk)
                  usbnd(ji,jk,nibm2,nitm2) = usbnd(ji,jk,nibm2,nitm)*usmsk(ji,jk)
            END DO
         END DO
 
         DO jj = fs_njs0, fs_njs1  ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  usbnd(ji,jk,nib  ,nitm) = usbnd(ji,jk,nib,  nit)*usmsk(ji,jk)
                  usbnd(ji,jk,nibm ,nitm) = usbnd(ji,jk,nibm ,nit)*usmsk(ji,jk)
                  usbnd(ji,jk,nibm2,nitm) = usbnd(ji,jk,nibm2,nit)*usmsk(ji,jk)
         ! ... fields nit <== now (kt+1)
                  usbnd(ji,jk,nib  ,nit) = un(ji,jj  ,jk)*usmsk(ji,jk)
                  usbnd(ji,jk,nibm ,nit) = un(ji,jj+1,jk)*usmsk(ji,jk)
                  usbnd(ji,jk,nibm2,nit) = un(ji,jj+2,jk)*usmsk(ji,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(usbnd,jpisd,jpisf,jpjsob,jpk*3*3,1,jpi, numout )

         ! ... extremeties njs0,njs1
         ii = jpisd + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               usbnd(ii,jk,nibm,nitm) = usbnd(ii+1,jk,nibm,nitm)
            END DO
         END IF
         ii = jpisf + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               usbnd(ii,jk,nibm,nitm) = usbnd(ii-1,jk,nibm,nitm)
            END DO
         END IF
 
         ! 1.2 normal velocity
         ! -------------------
 
         !.. advance in time (time filter, array swap) 
         DO jk = 1, jpkm1
            DO ji = 1, jpi
         ! ... fields nitm2 <== nitm 
               vsbnd(ji,jk,nib  ,nitm2) = vsbnd(ji,jk,nib  ,nitm)*vsmsk(ji,jk)
               vsbnd(ji,jk,nibm ,nitm2) = vsbnd(ji,jk,nibm ,nitm)*vsmsk(ji,jk)
            END DO
         END DO

         DO jj = fs_njs0, fs_njs1  ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  vsbnd(ji,jk,nib  ,nitm) = vsbnd(ji,jk,nib,  nit)*vsmsk(ji,jk)
                  vsbnd(ji,jk,nibm ,nitm) = vsbnd(ji,jk,nibm ,nit)*vsmsk(ji,jk)
                  vsbnd(ji,jk,nibm2,nitm) = vsbnd(ji,jk,nibm2,nit)*vsmsk(ji,jk)
         ! ... total or baroclinic velocity at b, bm and bm2
                  zvcb   = vn (ji,jj,jk)
                  zvcbm  = vn (ji,jj+1,jk)
                  zvcbm2 = vn (ji,jj+2,jk)
         ! ... fields nit <== now (kt+1) 
                  vsbnd(ji,jk,nib  ,nit) = zvcb   *vsmsk(ji,jk)
                  vsbnd(ji,jk,nibm ,nit) = zvcbm  *vsmsk(ji,jk)
                  vsbnd(ji,jk,nibm2,nit) = zvcbm2 *vsmsk(ji,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(vsbnd,jpisd,jpisf,jpjsob,jpk*3*3,1,jpi, numout )

         ! ... extremeties njs0,njs1
         ii = jpisd + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               vsbnd(ii,jk,nibm,nitm) = vsbnd(ii+1,jk,nibm,nitm)
            END DO
         END IF
         ii = jpisf + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               vsbnd(ii,jk,nibm,nitm) = vsbnd(ii-1,jk,nibm,nitm)
            END DO
         END IF

         ! 1.3 Temperature and salinity
         ! ----------------------------

         ! ... advance in time (time filter, array swap)
         DO jk = 1, jpkm1
            DO ji = 1, jpi
         ! ... fields nitm <== nit  plus time filter at the boundary
               tsbnd(ji,jk,nib,nitm) = tsbnd(ji,jk,nib,nit)*tsmsk(ji,jk)
               ssbnd(ji,jk,nib,nitm) = ssbnd(ji,jk,nib,nit)*tsmsk(ji,jk)
            END DO
         END DO

         DO jj = fs_njs0, fs_njs1  ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  tsbnd(ji,jk,nibm ,nitm) = tsbnd(ji,jk,nibm ,nit)*tsmsk(ji,jk)
                  ssbnd(ji,jk,nibm ,nitm) = ssbnd(ji,jk,nibm ,nit)*tsmsk(ji,jk)
         ! ... fields nit <== now (kt+1)
                  tsbnd(ji,jk,nib  ,nit) = tsn(ji,jj   ,jk,jp_tem)*tsmsk(ji,jk)
                  tsbnd(ji,jk,nibm ,nit) = tsn(ji,jj+1 ,jk,jp_tem)*tsmsk(ji,jk)
                  ssbnd(ji,jk,nib  ,nit) = tsn(ji,jj   ,jk,jp_sal)*tsmsk(ji,jk)
                  ssbnd(ji,jk,nibm ,nit) = tsn(ji,jj+1 ,jk,jp_sal)*tsmsk(ji,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(tsbnd,jpisd,jpisf,jpjsob,jpk*2*2,1,jpi, numout )
         IF( lk_mpp )   CALL mppobc(ssbnd,jpisd,jpisf,jpjsob,jpk*2*2,1,jpi, numout )

         ! ... extremeties  njs0,njs1
         ii = jpisd + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               tsbnd(ii,jk,nibm,nitm) = tsbnd(ii+1,jk,nibm,nitm)
               ssbnd(ii,jk,nibm,nitm) = ssbnd(ii+1,jk,nibm,nitm)
            END DO
         END IF
         ii = jpisf + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               tsbnd(ii,jk,nibm,nitm) = tsbnd(ii-1,jk,nibm,nitm)
               ssbnd(ii,jk,nibm,nitm) = ssbnd(ii-1,jk,nibm,nitm)
            END DO
         END IF

      END IF     ! End of array swap

      ! 2 - Calculation of radiation velocities
      ! ---------------------------------------

      IF( kt >= nit000 +3 .OR. ln_rstart ) THEN

         ! 2.1  Calculate the normal velocity based on phase velocity u_cysbnd
         ! -------------------------------------------------------------------
         !
         !          ji-row
         !            |
         ! nibm2 -----f-----   jpjsob +2
         !            |    
         !  nibm2 --  u  ----- jpjsob +2 
         !            |        
         !  nibm -----f-----   jpjsob +1
         !            |        
         !  nibm  --  u  ----- jpjsob +1
         !            |        
         !  nib  -----f-----   jpjsob
         !       /////|//////
         !  nib   ////u/////   jpjsob 
         !
         ! ... radiative condition plus Raymond-Kuo
         ! ... jpjsob,(jpisdp1, jpisfm1)
         DO jj = fs_njs0, fs_njs1  ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 2, jpim1
         ! ... 2* j-gradient of u (f-point i=nibm, time mean)
                  z2dx = (- usbnd(ji,jk,nibm ,nit) - usbnd(ji,jk,nibm ,nitm2) &
                          + 2.*usbnd(ji,jk,nibm2,nitm) ) / e2f(ji,jj+1)
         ! ... 2* i-gradient of u (u-point i=nibm, time nitm)
                  z2dy = ( usbnd(ji+1,jk,nibm,nitm) - usbnd(ji-1,jk,nibm,nitm) ) / e1u(ji, jj+1)
         ! ... square of the norm of grad(v)
                  z4nor2 = z2dx * z2dx + z2dy * z2dy
                  IF( z4nor2 == 0.) THEN
                     z4nor2 = 0.000001
                  END IF
         ! ... minus time derivative (leap-frog) at nibm, without / 2 dt
                  zdt = usbnd(ji,jk,nibm,nitm2) - usbnd(ji,jk,nibm,nit)
         ! ... i-phase speed ratio (bounded by -1) and save the unbounded phase
         !     velocity ratio no divided by e1f for the tracer radiation
                  z05cx = zdt * z2dx / z4nor2
                  u_cysbnd(ji,jk) = z05cx*usmsk(ji,jk)
               END DO
            END DO
         END DO
         IF( lk_mpp )   CALL mppobc(u_cysbnd,jpisd,jpisf,jpjsob,jpk,1,jpi, numout )

         ! ... extremeties  njs0,njs1
         ii = jpisd + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               u_cysbnd(ii,jk) = u_cysbnd(ii+1,jk)
            END DO
         END IF
         ii = jpisf + 1 - nimpp
         IF( ii >= 2 .AND. ii < jpim1 ) THEN
            DO jk = 1, jpkm1
               u_cysbnd(ii,jk) = u_cysbnd(ii-1,jk)
            END DO
         END IF

         ! 2.2 Calculate the normal velocity based on phase velocity v_cysbnd 
         ! -------------------------------------------------------------------
         !
         !               ji-row  ji-row
         !            |         |
         ! nibm2 -----f----v----f----  jpjsob+2
         !            |         |
         !  nibm   -  u -- T -- u ---- jpjsob+2
         !            |         |
         ! nibm  -----f----v----f----  jpjsob+1 
         !            |         |
         ! nib    --  u -- T -- u ---  jpjsob+1
         !            |         |
         ! nib   -----f----v----f----  jpjsob
         !       /////////////////////
         !
         ! ... Free surface formulation:
         ! ... radiative conditions on the total part + relaxation toward climatology
         ! ... jpjsob,(jpisdp1,jpisfm1)
         DO jj = fs_njs0, fs_njs1 ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 2, jpim1
         ! ... 2* gradj(v) (T-point i=nibm, time mean)
                  z2dx = ( - vsbnd(ji,jk,nibm ,nit) - vsbnd(ji,jk,nibm ,nitm2) &
                           + 2.*vsbnd(ji,jk,nibm2,nitm) ) / e2t(ji,jj+1)
         ! ... 2* gradi(v) (v-point i=nibm, time nitm)
                  z2dy = ( vsbnd(ji+1,jk,nibm,nitm) - vsbnd(ji-1,jk,nibm,nitm) ) / e1v(ji,jj+1)
         ! ... square of the norm of grad(u)
                  z4nor2 = z2dx * z2dx + z2dy * z2dy
                  IF( z4nor2 == 0.) THEN
                     z4nor2 = 0.000001
                  END IF
         ! ... minus time derivative (leap-frog) at nibm, without / 2 dt
                  zdt = vsbnd(ji,jk,nibm,nitm2) - vsbnd(ji,jk,nibm,nit)
         ! ... j-phase speed ratio (bounded by -1)
                  z05cx = zdt * z2dx / z4nor2
                  v_cysbnd(ji,jk)=z05cx*vsmsk(ji,jk)
               END DO
            END DO
         END DO

      ENDIF
 
   END SUBROUTINE obc_rad_south

#else
   !!=================================================================================
   !!                       ***  MODULE  obcrad  ***
   !! Ocean dynamic :   Phase velocities for each open boundary
   !!=================================================================================
CONTAINS
   SUBROUTINE obc_rad( kt )            ! No open boundaries ==> empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'obc_rad: You should not have seen this print! error?', kt
   END SUBROUTINE obc_rad
#endif

END MODULE obcrad
