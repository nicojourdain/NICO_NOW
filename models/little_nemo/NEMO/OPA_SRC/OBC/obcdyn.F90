MODULE obcdyn
#if defined key_obc
   !!=================================================================================
   !!                       ***  MODULE  obcdyn  ***
   !! Ocean dynamics:   Radiation of velocities on each open boundary
   !!=================================================================================

   !!---------------------------------------------------------------------------------
   !!   obc_dyn        : call the subroutine for each open boundary
   !!   obc_dyn_east   : radiation of the east open boundary velocities
   !!   obc_dyn_west   : radiation of the west open boundary velocities
   !!   obc_dyn_north  : radiation of the north open boundary velocities
   !!   obc_dyn_south  : radiation of the south open boundary velocities
   !!----------------------------------------------------------------------------------

   !!----------------------------------------------------------------------------------
   !! * Modules used
   USE oce             ! ocean dynamics and tracers 
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE obc_oce         ! ocean open boundary conditions
   USE lbclnk          ! ???
   USE lib_mpp         ! ???
   USE in_out_manager  ! I/O manager
   USE dynspg_oce

   IMPLICIT NONE
   PRIVATE

   !! * Accessibility
   PUBLIC obc_dyn     ! routine called in dynspg_flt (free surface case)

   !! * Module variables
   INTEGER ::   ji, jj, jk     ! dummy loop indices

   INTEGER ::      & ! ... boundary space indices 
      nib   = 1,   & ! nib   = boundary point
      nibm  = 2,   & ! nibm  = 1st interior point
      nibm2 = 3,   & ! nibm2 = 2nd interior point
                     ! ... boundary time indices 
      nit   = 1,   & ! nit    = now
      nitm  = 2,   & ! nitm   = before
      nitm2 = 3      ! nitm2  = before-before

   REAL(wp) ::   rtaue  , rtauw  , rtaun  , rtaus  ,  &
                 rtauein, rtauwin, rtaunin, rtausin

! LITTLE_NEMO
   LOGICAL  ::   ll_fbc
! LITTLE_NEMO

   !!---------------------------------------------------------------------------------

CONTAINS

   SUBROUTINE obc_dyn ( kt )
      !!------------------------------------------------------------------------------
      !!                      SUBROUTINE obc_dyn
      !!                     ********************
      !! ** Purpose :
      !!      Compute  dynamics (u,v) at the open boundaries.
      !!      if defined key_dynspg_flt: 
      !!                 this routine is called by dynspg_flt and updates
      !!                 ua, va which are the actual velocities (not trends)
      !!
      !!      The logical variable lp_obc_east, and/or lp_obc_west, and/or lp_obc_north, 
      !!      and/or lp_obc_south allow the user to determine which boundary is an
      !!      open one (must be done in the param_obc.h90 file).
      !!
      !! ** Reference : 
      !!      Marchesiello P., 1995, these de l'universite J. Fourier, Grenoble, France.
      !!
      !! History :
      !!        !  95-03 (J.-M. Molines) Original, SPEM
      !!        !  97-07 (G. Madec, J.-M. Molines) addition
      !!   8.5  !  02-10 (C. Talandier, A-M. Treguier) Free surface, F90
      !!   9.0  !  05-11  (V. Garnier) Surface pressure gradient organization
      !!----------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt

      !!----------------------------------------------------------------------
      !!  OPA 9.0 , LOCEAN-IPSL (2005) 
      !! $Id: obcdyn.F90 1528 2009-07-23 14:38:47Z rblod $ 
      !! This software is governed by the CeCILL licence see modipsl/doc/NEMO_CeCILL.txt
      !!----------------------------------------------------------------------

      ! 0. Local constant initialization
      ! --------------------------------

      IF( kt == nit000 .OR. ln_rstart) THEN
         ! ... Boundary restoring coefficient
         rtaue = 2. * rdt / rdpeob
         rtauw = 2. * rdt / rdpwob
         rtaun = 2. * rdt / rdpnob
         rtaus = 2. * rdt / rdpsob
         ! ... Boundary restoring coefficient for inflow ( all boundaries)
         rtauein = 2. * rdt / rdpein 
         rtauwin = 2. * rdt / rdpwin
         rtaunin = 2. * rdt / rdpnin
         rtausin = 2. * rdt / rdpsin 
      END IF
! LITTLE_NEMO
!     ll_fbc = ( ( ( kt < nit000+3  ) .AND. .NOT.  ln_rstart     ) .OR. lk_dynspg_exp ) 
!     IF ( cp_cfg == "indian" .OR. cp_cfg == "bengal" ) THEN
         ll_fbc = ( ( ( kt < nit000+30 ) .AND. .NOT.  ln_obc_rstart ) .OR. lk_dynspg_exp ) 
!     ENDIF
! LITTLE_NEMO


      IF( lp_obc_east  )   CALL obc_dyn_east ( kt )
      IF( lp_obc_west  )   CALL obc_dyn_west ( kt )
      IF( lp_obc_north )   CALL obc_dyn_north( kt )
      IF( lp_obc_south )   CALL obc_dyn_south( kt )

      IF( lk_mpp ) THEN
! LITTLE_NEMO BUG OBC_RADIA
       ! IF( kt >= nit000+3 .AND. ln_rstart ) THEN
            CALL lbc_lnk( ub, 'U', -1. )
            CALL lbc_lnk( vb, 'V', -1. )
!        END IF
! LITTLE_NEMO
         CALL lbc_lnk( ua, 'U', -1. )
         CALL lbc_lnk( va, 'V', -1. )
      ENDIF

   END SUBROUTINE obc_dyn


   SUBROUTINE obc_dyn_east ( kt )
      !!------------------------------------------------------------------------------
      !!                  ***  SUBROUTINE obc_dyn_east  ***
      !!              
      !! ** Purpose :
      !!      Apply the radiation algorithm on east OBC velocities ua, va using the 
      !!      phase velocities calculated in obc_rad_east subroutine in obcrad.F90 module
      !!      If the logical lfbceast is .TRUE., there is no radiation but only fixed OBC
      !!
      !!  History :
      !!         ! 95-03 (J.-M. Molines) Original from SPEM
      !!         ! 97-07 (G. Madec, J.-M. Molines) additions
      !!         ! 97-12 (M. Imbard) Mpp adaptation
      !!         ! 00-06 (J.-M. Molines) 
      !!    8.5  ! 02-10 (C. Talandier, A-M. Treguier) Free surface, F90
      !!    9.0  ! 05-11  (V. Garnier) Surface pressure gradient organization
      !!------------------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt

      !! * Local declaration
      REAL(wp) ::   z05cx, ztau, zin
      !!------------------------------------------------------------------------------

      ! 1. First three time steps and more if lfbceast is .TRUE.
      !    In that case open boundary conditions are FIXED.
      ! --------------------------------------------------------

! LITTLE_NEMO
!     IF( ( kt < nit000+3 .AND. .NOT.ln_rstart ) .OR. lfbceast .OR. lk_dynspg_exp ) THEN
      IF ( ll_fbc .OR. lfbceast ) THEN
! LITTLE_NEMO

         ! 1.1 U zonal velocity    
         ! --------------------
         DO ji = nie0, nie1
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  ua(ji,jj,jk) = ua(ji,jj,jk) * (1.-uemsk(jj,jk)) + &
                                 uemsk(jj,jk)*ufoe(jj,jk)
               END DO
            END DO
         END DO

         ! 1.2 V meridional velocity
         ! -------------------------
         DO ji = nie0+1, nie1+1
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  va(ji,jj,jk) = va(ji,jj,jk) * (1.-vemsk(jj,jk)) + &
                                 vfoe(jj,jk)*vemsk(jj,jk)
               END DO
            END DO
         END DO

      ELSE

      ! 2. Beyond the fourth time step if lfbceast is .FALSE.
      ! -----------------------------------------------------
 
         ! 2.1. u-component of the velocity
         ! ---------------------------------
         !
         !          nibm2      nibm      nib
         !            |   nibm  |   nib   |///
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
         ! ... If free surface formulation:
         ! ... radiative conditions on the total part + relaxation toward climatology
         ! ... (jpjedp1, jpjefm1),jpieob
         DO ji = nie0, nie1
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  z05cx = u_cxebnd(jj,jk)
                  z05cx = z05cx / e1t(ji,jj)
                  z05cx = min( z05cx, 1. )
         ! ... z05cx=< 0, inflow  zin=0, ztau=1    
         !           > 0, outflow zin=1, ztau=rtaue
                  zin = sign( 1., z05cx )
                  zin = 0.5*( zin + abs(zin) )
         ! ... for inflow rtauein is used for relaxation coefficient else rtaue
                  ztau = (1.-zin ) * rtauein  + zin * rtaue
                  z05cx = z05cx * zin
         ! ... update ua with radiative or climatological velocity
                  ua(ji,jj,jk) = ua(ji,jj,jk) * ( 1. - uemsk(jj,jk) ) +          &
                                 uemsk(jj,jk) * (  ( 1. - z05cx - ztau )         &
                                 * uebnd(jj,jk,nib ,nitm) + 2.*z05cx               &
                                 * uebnd(jj,jk,nibm,nit ) + ztau * ufoe (jj,jk) )  &
                                 / (1. + z05cx)
               END DO
            END DO
         END DO

         ! 2.2 v-component of the velocity
         ! -------------------------------
         !
         !          nibm2       nibm     nib
         !            |   nibm  |   nib///|///
         !            |    |    |    |////|///
         !  jj-line --v----f----v----f----v---
         !            |    |    |    |////|///
         !            |    |    |    |////|///
         !            | jpieob-1 |  jpieob /|///
         !            |         |         |   
         !         jpieob-1    jpieob     jpieob+1
         !
         ! ... radiative condition
         ! ... (jpjedp1, jpjefm1), jpieob+1
         DO ji = nie0+1, nie1+1
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  z05cx = v_cxebnd(jj,jk) 
                  z05cx = z05cx / e1f(ji-1,jj)
                  z05cx = min( z05cx, 1. )
         ! ... z05cx=< 0, inflow  zin=0, ztau=1    
         !           > 0, outflow zin=1, ztau=rtaue
                  zin = sign( 1., z05cx )
                  zin = 0.5*( zin + abs(zin) )
         ! ... for inflow rtauein is used for relaxation coefficient else rtaue
                  ztau = (1.-zin ) * rtauein  + zin * rtaue
                  z05cx = z05cx * zin
         ! ... update va with radiative or climatological velocity
                  va(ji,jj,jk) = va(ji,jj,jk) * (1. - vemsk(jj,jk) ) +          &
                                 vemsk(jj,jk) * ( ( 1. - z05cx - ztau )         &
                                 * vebnd(jj,jk,nib ,nitm) + 2.*z05cx              &
                                 * vebnd(jj,jk,nibm,nit ) + ztau * vfoe(jj,jk) )  &
                                 / (1. + z05cx)
               END DO
            END DO
         END DO

      END IF

   END SUBROUTINE obc_dyn_east


   SUBROUTINE obc_dyn_west ( kt )
      !!------------------------------------------------------------------------------
      !!                  ***  SUBROUTINE obc_dyn_west  ***
      !!                  
      !! ** Purpose :
      !!      Apply the radiation algorithm on west OBC velocities ua, va using the 
      !!      phase velocities calculated in obc_rad_west subroutine in obcrad.F90 module
      !!      If the logical lfbcwest is .TRUE., there is no radiation but only fixed OBC
      !!
      !!  History :
      !!         ! 95-03 (J.-M. Molines) Original from SPEM
      !!         ! 97-07 (G. Madec, J.-M. Molines) additions
      !!         ! 97-12 (M. Imbard) Mpp adaptation
      !!         ! 00-06 (J.-M. Molines) 
      !!    8.5  ! 02-10 (C. Talandier, A-M. Treguier) Free surface, F90
      !!    9.0  ! 05-11  (V. Garnier) Surface pressure gradient organization
      !!------------------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt

      !! * Local declaration
      REAL(wp) ::   z05cx, ztau, zin
      !!------------------------------------------------------------------------------

      ! 1. First three time steps and more if lfbcwest is .TRUE.
      !    In that case open boundary conditions are FIXED.
      ! --------------------------------------------------------

! LITTLE_NEMO
!     IF( ( kt < nit000+3 .AND. .NOT.ln_rstart ) .OR. lfbcwest .OR. lk_dynspg_exp ) THEN
      IF ( ll_fbc .OR. lfbcwest ) THEN
! LITTLE_NEMO

         ! 1.1 U zonal velocity
         ! ---------------------
         DO ji = niw0, niw1
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  ua(ji,jj,jk) = ua(ji,jj,jk) * (1.-uwmsk(jj,jk)) + &
                                 uwmsk(jj,jk)*ufow(jj,jk)
               END DO
            END DO
         END DO

         ! 1.2 V meridional velocity
         ! -------------------------
         DO ji = niw0, niw1
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  va(ji,jj,jk) = va(ji,jj,jk) * (1.-vwmsk(jj,jk)) + &
                                 vfow(jj,jk)*vwmsk(jj,jk)
               END DO
            END DO
         END DO

      ELSE

      ! 2. Beyond the fourth time step if lfbcwest is .FALSE.
      ! -----------------------------------------------------
 
         ! 2.1. u-component of the velocity
         ! ---------------------------------
         !
         !        nib       nibm     nibm2
         !      ///|   nib   |   nibm  |
         !      ///|    |    |    |    |
         !      ---f----v----f----v----f-- jj-line
         !      ///|    |    |    |    |
         !      ///|         |         |
         !      ///u    T    u    T    u   jj-line
         !      ///|         |         |
         !      ///|    |    |    |    |
         !       jpiwob    jpiwob+1    jpiwob+2
         !              |         |        
         !            jpiwob+1    jpiwob+2     
         !
         ! ... If free surface formulation:
         ! ... radiative conditions on the total part + relaxation toward climatology
         ! ... (jpjwdp1, jpjwfm1), jpiwob
         DO ji = niw0, niw1
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  z05cx = u_cxwbnd(jj,jk)
                  z05cx = z05cx / e1t(ji+1,jj)
                  z05cx = max( z05cx, -1. )
         ! ... z05c  > 0, inflow  zin=0, ztau=1    
         !          =< 0, outflow zin=1, ztau=rtauw
                  zin = sign( 1., -1. * z05cx )
                  zin = 0.5*( zin + abs(zin) )
                  ztau = (1.-zin )* rtauwin + zin * rtauw
                  z05cx = z05cx * zin
         ! ... update un with radiative or climatological velocity
                  ua(ji,jj,jk) = ua(ji,jj,jk) * ( 1. - uwmsk(jj,jk) ) +          &
                                 uwmsk(jj,jk) * ( ( 1. + z05cx - ztau )          &
                                 * uwbnd(jj,jk,nib ,nitm) - 2.*z05cx               &
                                 * uwbnd(jj,jk,nibm,nit ) + ztau  * ufow (jj,jk) ) &
                                 / (1. - z05cx)
               END DO
            END DO
         END DO

         ! 2.2 v-component of the velocity
         ! -------------------------------
         !
         !    nib       nibm     nibm2
         !  ///|///nib   |   nibm  |  nibm2
         !  ///|////|    |    |    |    |    |
         !  ---v----f----v----f----v----f----v-- jj-line
         !  ///|////|    |    |    |    |    |
         !  ///|////|    |    |    |    |    |
         ! jpiwob     jpiwob+1    jpiwob+2
         !          |         |         |   
         !        jpiwob   jpiwob+1   jpiwob+2    
         !
         ! ... radiative condition plus Raymond-Kuo
         ! ... (jpjwdp1, jpjwfm1),jpiwob
         DO ji = niw0, niw1
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  z05cx = v_cxwbnd(jj,jk)  
                  z05cx = z05cx / e1f(ji,jj)
                  z05cx = max( z05cx, -1. )
         ! ... z05cx > 0, inflow  zin=0, ztau=1    
         !          =< 0, outflow zin=1, ztau=rtauw
                  zin = sign( 1., -1. * z05cx )
                  zin = 0.5*( zin + abs(zin) )
                  ztau = (1.-zin )*rtauwin + zin * rtauw
                  z05cx = z05cx * zin 
         ! ... update va with radiative or climatological velocity
                  va(ji,jj,jk) = va(ji,jj,jk) * (1. - vwmsk(jj,jk) ) +          &
                                 vwmsk(jj,jk) * ( ( 1. + z05cx - ztau )         &
                                 * vwbnd(jj,jk,nib ,nitm) - 2.*z05cx              &
                                 * vwbnd(jj,jk,nibm,nit ) + ztau * vfow (jj,jk) ) &
                                 / (1. - z05cx)
                END DO
             END DO
         END DO

      END IF

   END SUBROUTINE obc_dyn_west

   SUBROUTINE obc_dyn_north ( kt )
      !!------------------------------------------------------------------------------
      !!                     SUBROUTINE obc_dyn_north
      !!                    *************************
      !! ** Purpose :
      !!      Apply the radiation algorithm on north OBC velocities ua, va using the 
      !!      phase velocities calculated in obc_rad_north subroutine in obcrad.F90 module
      !!      If the logical lfbcnorth is .TRUE., there is no radiation but only fixed OBC
      !!
      !!  History :
      !!         ! 95-03 (J.-M. Molines) Original from SPEM
      !!         ! 97-07 (G. Madec, J.-M. Molines) additions
      !!         ! 97-12 (M. Imbard) Mpp adaptation
      !!         ! 00-06 (J.-M. Molines) 
      !!    8.5  ! 02-10 (C. Talandier, A-M. Treguier) Free surface, F90
      !!    9.0  ! 05-11  (V. Garnier) Surface pressure gradient organization
      !!------------------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt

      !! * Local declaration
      REAL(wp) ::   z05cx, ztau, zin
      !!------------------------------------------------------------------------------

      ! 1. First three time steps and more if lfbcnorth is .TRUE.
      !    In that case open boundary conditions are FIXED.
      ! ---------------------------------------------------------
 
! LITTLE_NEMO
!     IF( ( kt < nit000+3 .AND. .NOT.ln_rstart ) .OR. lfbcnorth  .OR. lk_dynspg_exp ) THEN
      IF ( ll_fbc .OR. lfbcnorth ) THEN
! LITTLE_NEMO

         ! 1.1 U zonal velocity
         ! --------------------
         DO jj = njn0+1, njn1+1
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  ua(ji,jj,jk)= ua(ji,jj,jk) * (1.-unmsk(ji,jk)) + &
                                ufon(ji,jk)*unmsk(ji,jk)
               END DO
            END DO
         END DO

         ! 1.2 V meridional velocity
         ! -------------------------
         DO jj = njn0, njn1
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  va(ji,jj,jk)= va(ji,jj,jk) * (1.-vnmsk(ji,jk)) + &
                                vfon(ji,jk)*vnmsk(ji,jk)
               END DO
            END DO
         END DO

      ELSE

      ! 2. Beyond the fourth time step if lfbcnorth is .FALSE.
      ! ------------------------------------------------------

         ! 2.1. u-component of the velocity
         ! --------------------------------
         !
         !            ji-row
         !              |
         !       nib ///u//////  jpjnob + 1
         !         /////|//////
         !     nib -----f-----   jpjnob
         !              |    
         !      nibm--  u   ---- jpjnob
         !              |        
         !    nibm -----f-----   jpjnob-1
         !              |        
         !     nibm2--  u   ---- jpjnob-1
         !              |        
         !   nibm2 -----f-----   jpjnob-2
         !              |
         !
         ! ... radiative condition
         ! ... jpjnob+1,(jpindp1, jpinfm1)
         DO jj = njn0+1, njn1+1
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  z05cx= u_cynbnd(ji,jk) 
                  z05cx = z05cx / e2f(ji, jj-1)
                  z05cx = min( z05cx, 1. )
         ! ... z05cx=< 0, inflow  zin=0, ztau=1    
         !           > 0, outflow zin=1, ztau=rtaun
                  zin = sign( 1., z05cx )
                  zin = 0.5*( zin + abs(zin) )
         ! ... for inflow rtaunin is used for relaxation coefficient else rtaun
                  ztau = (1.-zin ) * rtaunin  + zin * rtaun
         ! ... for u, when inflow, ufon is prescribed
                  z05cx = z05cx * zin
         ! ... update un with radiative or climatological velocity
                  ua(ji,jj,jk) = ua(ji,jj,jk) * (1.-unmsk(ji,jk)) +             &
                                 unmsk(ji,jk) * ( ( 1. - z05cx - ztau )         &
                                 * unbnd(ji,jk,nib ,nitm) + 2.*z05cx              &
                                 * unbnd(ji,jk,nibm,nit ) + ztau * ufon (ji,jk) ) &
                                 / (1. + z05cx)
               END DO
            END DO
         END DO

         ! 2.2 v-component of the velocity
         ! -------------------------------
         !
         !                ji-row    ji-row
         !              |         |
         !         /////|/////////////////
         !    nib  -----f----v----f----  jpjnob
         !              |         |
         !      nib  -  u -- T -- u ---- jpjnob
         !              |         |
         !   nibm  -----f----v----f----  jpjnob-1
         !              |         |
         !     nibm --  u -- T -- u ---  jpjnob-1
         !              |         |
         !   nibm2 -----f----v----f----  jpjnob-2
         !              |         |
         !
         ! ... Free surface formulation:
         ! ... radiative conditions on the total part + relaxation toward climatology
         ! ... jpjnob,(jpindp1, jpinfm1)
         DO jj = njn0, njn1
            DO jk = 1, jpkm1
               DO ji = 1, jpi
         ! ... 2* gradj(v) (T-point i=nibm, time mean)
                  z05cx = v_cynbnd(ji,jk)
                  z05cx = z05cx / e2t(ji,jj)
                  z05cx = min( z05cx, 1. )
         ! ... z05cx=< 0, inflow  zin=0, ztau=1    
         !           > 0, outflow zin=1, ztau=rtaun
                  zin = sign( 1., z05cx )
                  zin = 0.5*( zin + abs(zin) )
         ! ... for inflow rtaunin is used for relaxation coefficient else rtaun
                  ztau = (1.-zin ) * rtaunin + zin * rtaun
                  z05cx = z05cx * zin
         ! ... update va with radiative or climatological velocity
                  va(ji,jj,jk) = va(ji,jj,jk) * (1.-vnmsk(ji,jk)) +             &
                                 vnmsk(ji,jk) * ( ( 1. - z05cx - ztau )         &
                                 * vnbnd(ji,jk,nib ,nitm) + 2.*z05cx              &
                                 * vnbnd(ji,jk,nibm,nit ) + ztau * vfon (ji,jk) ) &
                                 / (1. + z05cx)
               END DO
            END DO
         END DO
      END IF

   END SUBROUTINE obc_dyn_north

   SUBROUTINE obc_dyn_south ( kt )
      !!------------------------------------------------------------------------------
      !!                     SUBROUTINE obc_dyn_south
      !!                    *************************
      !! ** Purpose :
      !!      Apply the radiation algorithm on south OBC velocities ua, va using the 
      !!      phase velocities calculated in obc_rad_south subroutine in obcrad.F90 module
      !!      If the logical lfbcsouth is .TRUE., there is no radiation but only fixed OBC
      !!
      !!  History :
      !!         ! 95-03 (J.-M. Molines) Original from SPEM
      !!         ! 97-07 (G. Madec, J.-M. Molines) additions
      !!         ! 97-12 (M. Imbard) Mpp adaptation
      !!         ! 00-06 (J.-M. Molines) 
      !!    8.5  ! 02-10 (C. Talandier, A-M. Treguier) Free surface, F90
      !!    9.0  ! 05-11  (V. Garnier) Surface pressure gradient organization
      !!------------------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt

      !! * Local declaration
      REAL(wp) ::   z05cx, ztau, zin

      !!------------------------------------------------------------------------------
      !!  OPA 8.5, LODYC-IPSL (2002)
      !!------------------------------------------------------------------------------

      ! 1. First three time steps and more if lfbcsouth is .TRUE.
      !    In that case open boundary conditions are FIXED.
      ! ---------------------------------------------------------

! LITTLE_NEMO
!     IF( ( kt < nit000+3 .AND. .NOT.ln_rstart ) .OR. lfbcsouth  .OR. lk_dynspg_exp ) THEN
      IF ( ll_fbc .OR. lfbcsouth ) THEN
! LITTLE_NEMO

         ! 1.1 U zonal velocity
         ! --------------------
         DO jj = njs0, njs1
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  ua(ji,jj,jk)= ua(ji,jj,jk) * (1.-usmsk(ji,jk)) + &
                                usmsk(ji,jk) * ufos(ji,jk)
               END DO
            END DO
         END DO

         ! 1.2 V meridional velocity
         ! -------------------------
         DO jj = njs0, njs1
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  va(ji,jj,jk)= va(ji,jj,jk) * (1.-vsmsk(ji,jk)) + &
                                vsmsk(ji,jk) * vfos(ji,jk)
               END DO
            END DO
         END DO

      ELSE

      ! 2. Beyond the fourth time step if lfbcsouth is .FALSE.
      ! ------------------------------------------------------

         ! 2.1. u-component of the velocity
         ! --------------------------------
         !
         !            ji-row
         !              |
         !   nibm2 -----f-----   jpjsob +2
         !              |    
         !    nibm2 --  u   ---- jpjsob +2 
         !              |        
         !    nibm -----f-----   jpjsob +1
         !              |        
         !    nibm  --  u   ---- jpjsob +1
         !              |        
         !    nib  -----f-----   jpjsob
         !         /////|//////
         !    nib   ////u/////   jpjsob 
         !
         ! ... radiative condition plus Raymond-Kuo
         ! ... jpjsob,(jpisdp1, jpisfm1)
         DO jj = njs0, njs1
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  z05cx= u_cysbnd(ji,jk) 
                  z05cx = z05cx / e2f(ji, jj)
                  z05cx = max( z05cx, -1. )
         ! ... z05cx > 0, inflow  zin=0, ztau=1 
         !          =< 0, outflow zin=1, ztau=rtaus
                  zin = sign( 1., -1. * z05cx )
                  zin = 0.5*( zin + abs(zin) )
                  ztau = (1.-zin ) * rtausin + zin * rtaus
                  z05cx = z05cx * zin 
         ! ... update ua with radiative or climatological velocity
                  ua(ji,jj,jk) = ua(ji,jj,jk) * (1.-usmsk(ji,jk)) +              &
                                 usmsk(ji,jk) * (  ( 1. + z05cx - ztau )         &
                                 * usbnd(ji,jk,nib ,nitm) - 2.*z05cx               &
                                 * usbnd(ji,jk,nibm,nit ) + ztau * ufos (ji,jk) )  &
                                 / (1. - z05cx)
               END DO
            END DO
         END DO

         ! 2.2 v-component of the velocity
         ! -------------------------------
         !
         !                ji-row    ji-row
         !              |         |
         !  nibm2  -----f----v----f----  jpjsob+2
         !              |         |
         !    nibm   -  u -- T -- u ---- jpjsob+2
         !              |         |
         !   nibm  -----f----v----f----  jpjsob+1 
         !              |         |
         !   nib    --  u -- T -- u ---  jpjsob+1
         !              |         |
         !   nib   -----f----v----f----  jpjsob
         !         /////////////////////
         !
         ! ... Free surface formulation:
         ! ... radiative conditions on the total part + relaxation toward climatology
         ! ... jpjsob,(jpisdp1,jpisfm1)
         DO jj = njs0, njs1
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  z05cx = v_cysbnd(ji,jk)
                  z05cx = z05cx / e2t(ji,jj+1)
                  z05cx = max( z05cx, -1. )
         ! ... z05c > 0, inflow  zin=0, ztau=1 
         !         =< 0, outflow zin=1, ztau=rtaus
                  zin = sign( 1., -1. * z05cx )
                  zin = 0.5*( zin + abs(zin) )
                  ztau = (1.-zin )*rtausin + zin * rtaus
                  z05cx = z05cx * zin
         ! ... update va with radiative or climatological velocity
                  va(ji,jj,jk) = va(ji,jj,jk) * (1.-vsmsk(ji,jk)) +             &
                                 vsmsk(ji,jk) * ( ( 1. + z05cx - ztau )         &
                                 * vsbnd(ji,jk,nib ,nitm) - 2.*z05cx              &
                                 * vsbnd(ji,jk,nibm,nit ) + ztau * vfos (ji,jk) ) &
                                 / (1. - z05cx)
               END DO
            END DO
         END DO
      END IF

   END SUBROUTINE obc_dyn_south
#else
   !!=================================================================================
   !!                       ***  MODULE  obcdyn  ***
   !! Ocean dynamics:   Radiation of velocities on each open boundary
   !!=================================================================================
CONTAINS

   SUBROUTINE obc_dyn
                              ! No open boundaries ==> empty routine
   END SUBROUTINE obc_dyn
#endif

END MODULE obcdyn
