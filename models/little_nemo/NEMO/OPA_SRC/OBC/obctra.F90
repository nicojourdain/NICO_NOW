MODULE obctra
   !!=================================================================================
   !!                       ***  MODULE  obctra  ***
   !! Ocean tracers:   Radiation of tracers on each open boundary
   !!=================================================================================
#if defined key_obc
   !!---------------------------------------------------------------------------------
   !!   'key_obc'      :                                      Open Boundary Conditions
   !!---------------------------------------------------------------------------------
   !!   obc_tra        : call the subroutine for each open boundary
   !!   obc_tra_east   : radiation of the east open boundary tracers
   !!   obc_tra_west   : radiation of the west open boundary tracers
   !!   obc_tra_north  : radiation of the north open boundary tracers
   !!   obc_tra_south  : radiation of the south open boundary tracers
   !!----------------------------------------------------------------------------------
   !! * Modules used
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables 
   USE phycst          ! physical constants
   USE obc_oce         ! ocean open boundary conditions
   USE lib_mpp         ! ???
   USE lbclnk          ! ???
   USE in_out_manager  ! I/O manager

   IMPLICIT NONE
   PRIVATE

   !! * Accessibility
   PUBLIC obc_tra     ! routine called in tranxt.F90 

   !! * Module variables
   INTEGER ::      & ! ... boundary space indices 
      nib   = 1,   & ! nib   = boundary point
      nibm  = 2,   & ! nibm  = 1st interior point
      nibm2 = 3,   & ! nibm2 = 2nd interior point
                     ! ... boundary time indices 
      nit   = 1,   & ! nit    = now
      nitm  = 2,   & ! nitm   = before
      nitm2 = 3      ! nitm2  = before-before

   REAL(wp) ::     &
      rtaue  , rtauw  , rtaun  , rtaus  ,  &  ! Boundary restoring coefficient
      rtauein, rtauwin, rtaunin, rtausin      ! Boundary restoring coefficient for inflow 

! LITTLE_NEMO
   LOGICAL  ::   ll_fbc
! LITTLE_NEMO

   !! * Substitutions
#  include "obc_vectopt_loop_substitute.h90"
   !!---------------------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obctra.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!---------------------------------------------------------------------------------

CONTAINS

   SUBROUTINE obc_tra( kt )
      !!-------------------------------------------------------------------------------
      !!                 ***  SUBROUTINE obc_tra  ***
      !!                    
      !! ** Purpose :   Compute tracer fields (t,s) along the open boundaries.
      !!      This routine is called by the tranxt.F routine and updates tsa
      !!      which are the actual temperature and salinity fields.
      !!        The logical variable lp_obc_east, and/or lp_obc_west, and/or lp_obc_north,
      !!      and/or lp_obc_south allow the user to determine which boundary is an
      !!      open one (must be done in the param_obc.h90 file).
      !!
      !! Reference : 
      !!   Marchesiello P., 1995, these de l'universite J. Fourier, Grenoble, France.
      !!
      !!  History :
      !!        !  95-03 (J.-M. Molines) Original, SPEM
      !!        !  97-07 (G. Madec, J.-M. Molines) addition
      !!   8.5  !  02-10 (C. Talandier, A-M. Treguier) F90
      !!----------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt
      !!----------------------------------------------------------------------

      ! 0. Local constant initialization

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
!     ll_fbc = ( ( kt < nit000+3  ) .AND. .NOT.  ln_rstart     )
!     IF ( cp_cfg == "indian" .OR. cp_cfg == "bengal" ) THEN
      ll_fbc = ( ( kt < nit000+30 ) .AND. .NOT.  ln_obc_rstart )
!     ENDIF
! LITTLE_NEMO

      IF( lp_obc_east  )   CALL obc_tra_east ( kt )    ! East open boundary

      IF( lp_obc_west  )   CALL obc_tra_west ( kt )    ! West open boundary

      IF( lp_obc_north )   CALL obc_tra_north( kt )    ! North open boundary

      IF( lp_obc_south )   CALL obc_tra_south( kt )    ! South open boundary

      IF( lk_mpp ) THEN                  !!bug ???
! LITTLE_NEMO BUG OBC_RADIA
!        IF( kt >= nit000+3 .AND. ln_rstart ) THEN
            CALL lbc_lnk( tsb(:,:,:,jp_tem), 'T', 1. )
            CALL lbc_lnk( tsb(:,:,:,jp_sal), 'T', 1. )
!        END IF
         CALL lbc_lnk( tsa(:,:,:,jp_tem), 'T', 1. )
         CALL lbc_lnk( tsa(:,:,:,jp_sal), 'T', 1. )
      ENDIF

   END SUBROUTINE obc_tra


   SUBROUTINE obc_tra_east ( kt )
      !!------------------------------------------------------------------------------
      !!                ***  SUBROUTINE obc_tra_east  ***
      !!                  
      !! ** Purpose :
      !!      Apply the radiation algorithm on east OBC tracers tsa using the 
      !!      phase velocities calculated in obc_rad_east subroutine in obcrad.F90 module
      !!      If the logical lfbceast is .TRUE., there is no radiation but only fixed OBC
      !!
      !!  History :
      !!         ! 95-03 (J.-M. Molines) Original from SPEM
      !!         ! 97-07 (G. Madec, J.-M. Molines) additions
      !!         ! 97-12 (M. Imbard) Mpp adaptation
      !!         ! 00-06 (J.-M. Molines) 
      !!    8.5  ! 02-10 (C. Talandier, A-M. Treguier) F90
      !!------------------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt

      !! * Local declaration
      INTEGER ::   ji, jj, jk      ! dummy loop indices
      REAL(wp) ::   z05cx, ztau, zin
      !!------------------------------------------------------------------------------

      ! 1. First three time steps and more if lfbceast is .TRUE.
      !    In that case open boundary conditions are FIXED.
      ! --------------------------------------------------------

! LITTLE_NEMO
!     IF( ( kt < nit000+3 .AND. .NOT.ln_rstart ) .OR. lfbceast ) THEN
      IF ( ll_fbc .OR. lfbceast ) THEN
! LITTLE_NEMO
         DO ji = fs_nie0+1, fs_nie1+1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) * (1. - temsk(jj,jk)) + &
                                         tfoe(jj,jk)*temsk(jj,jk)
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) * (1. - temsk(jj,jk)) + &
                                         sfoe(jj,jk)*temsk(jj,jk)
               END DO
            END DO
         END DO

      ELSE

      ! 2. Beyond the fourth time step if lfbceast is .FALSE.
      ! -----------------------------------------------------

         ! Temperature and salinity radiation
         ! ----------------------------------
         !
         !            nibm2      nibm      nib
         !              |   nibm  |   nib///|///
         !              |    |    |    |////|///
         !  jj   line --v----f----v----f----v---
         !              |    |    |    |////|///
         !                   |         |///   //
         !  jj   line   T    u    T    u/// T //
         !                   |         |///   //
         !              |    |    |    |////|///
         !  jj-1 line --v----f----v----f----v---
         !              |    |    |    |////|///
         !                jpieob-1    jpieob / ///
         !              |         |         |
         !           jpieob-1    jpieob     jpieob+1
         !
         ! ... radiative conditions + relaxation toward a climatology
         !     the phase velocity is taken as the phase velocity of the tangen-
         !     tial velocity (here vn), which have been saved in (u_cxebnd,v_cxebnd)
         ! ... (jpjedp1, jpjefm1), jpieob+1
         DO ji = fs_nie0+1, fs_nie1+1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 2, jpjm1
         ! ... i-phase speed ratio (from averaged of v_cxebnd)
                  z05cx = ( 0.5 * ( v_cxebnd(jj,jk) + v_cxebnd(jj-1,jk) ) ) / e1t(ji-1,jj)
                  z05cx = min( z05cx, 1. )
         ! ... z05cx=< 0, inflow  zin=0, ztau=1    
         !           > 0, outflow zin=1, ztau=rtaue
                  zin = sign( 1., z05cx )
                  zin = 0.5*( zin + abs(zin) )
         ! ... for inflow rtauein is used for relaxation coefficient else rtaue
                  ztau = (1.-zin ) * rtauein  + zin * rtaue
                  z05cx = z05cx * zin
         ! ... update tsa with radiative or climatological ts
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) * (1. - temsk(jj,jk)) +           &
                                 temsk(jj,jk) * ( ( 1. - z05cx - ztau )         &
                                 * tebnd(jj,jk,nib ,nitm) + 2.*z05cx              &
                                 * tebnd(jj,jk,nibm,nit ) + ztau * tfoe (jj,jk) ) &
                                 / (1. + z05cx)
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) * (1. - temsk(jj,jk)) +           &
                                 temsk(jj,jk) * ( ( 1. - z05cx - ztau )         &
                                 * sebnd(jj,jk,nib ,nitm) + 2.*z05cx              &
                                 * sebnd(jj,jk,nibm,nit ) + ztau * sfoe (jj,jk) ) &
                                 / (1. + z05cx)
               END DO
            END DO
         END DO

      END IF

   END SUBROUTINE obc_tra_east


   SUBROUTINE obc_tra_west ( kt )
      !!------------------------------------------------------------------------------
      !!                 ***  SUBROUTINE obc_tra_west  ***
      !!           
      !! ** Purpose :
      !!      Apply the radiation algorithm on west OBC tracers tsa using the 
      !!      phase velocities calculated in obc_rad_west subroutine in obcrad.F90 module
      !!      If the logical lfbcwest is .TRUE., there is no radiation but only fixed OBC
      !!
      !!  History :
      !!         ! 95-03 (J.-M. Molines) Original from SPEM
      !!         ! 97-07 (G. Madec, J.-M. Molines) additions
      !!         ! 97-12 (M. Imbard) Mpp adaptation
      !!         ! 00-06 (J.-M. Molines) 
      !!    8.5  ! 02-10 (C. Talandier, A-M. Treguier) F90
      !!------------------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt

      !! * Local declaration
      INTEGER ::   ji, jj, jk      ! dummy loop indices
      REAL(wp) ::   z05cx, ztau, zin
      !!------------------------------------------------------------------------------

      ! 1. First three time steps and more if lfbcwest is .TRUE.
      !    In that case open boundary conditions are FIXED.
      ! --------------------------------------------------------

! LITTLE_NEMO
!     IF( ( kt < nit000+3 .AND. .NOT.ln_rstart ) .OR. lfbcwest ) THEN
      IF ( ll_fbc .OR. lfbcwest ) THEN
! LITTLE_NEMO

         DO ji = fs_niw0, fs_niw1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 1, jpj
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) * (1. - twmsk(jj,jk)) + &
                                         tfow(jj,jk)*twmsk(jj,jk)
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) * (1. - twmsk(jj,jk)) + &
                                         sfow(jj,jk)*twmsk(jj,jk)
               END DO
            END DO
         END DO

      ELSE

      ! 2. Beyond the fourth time step if lfbcwest is .FALSE.
      ! -----------------------------------------------------
          
         ! Temperature and salinity radiation
         ! ----------------------------------
         !
         !          nib       nibm     nibm2
         !     nib///|   nibm  |  nibm2  |
         !   ///|////|    |    |    |    |   
         !   ---v----f----v----f----v----f-- jj   line
         !   ///|////|    |    |    |    |   
         !   //   ///|         |         |   
         !   // T ///u    T    u    T    u   jj   line
         !   //   ///|         |         |   
         !   ///|////|    |    |    |    |   
         !   ---v----f----v----f----v----f-- jj-1 line
         !   ///|////|    |    |    |    |   
         !         jpiwob    jpiwob+1    jpiwob+2
         !      |         |         |        
         !    jpiwob    jpiwob+1   jpiwob+2
         !
         ! ... radiative conditions + relaxation toward a climatology
         ! ... the phase velocity is taken as the phase velocity of the tangen-
         ! ... tial velocity (here vn), which have been saved in (v_cxwbnd)
         DO ji = fs_niw0, fs_niw1 ! Vector opt.
            DO jk = 1, jpkm1
               DO jj = 2, jpjm1
         ! ... i-phase speed ratio (from averaged of v_cxwbnd)
                  z05cx = (  0.5 * ( v_cxwbnd(jj,jk) + v_cxwbnd(jj-1,jk) ) ) / e1t(ji+1,jj)
                  z05cx = max( z05cx, -1. )
         ! ... z05cx > 0, inflow  zin=0, ztau=1    
         !           < 0, outflow zin=1, ztau=rtauw
                  zin = sign( 1., -1.* z05cx )
                  zin = 0.5*( zin + abs(zin) )
                  ztau = (1.-zin )*rtauwin + zin * rtauw
                  z05cx = z05cx * zin
         ! ... update tsa with radiative or climatological (ts)
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) * (1. - twmsk(jj,jk)) +           &
                                 twmsk(jj,jk) * ( ( 1. + z05cx - ztau )         &
                                 * twbnd(jj,jk,nib ,nitm) - 2.*z05cx              &
                                 * twbnd(jj,jk,nibm,nit ) + ztau * tfow (jj,jk) ) &
                                 / (1. - z05cx)
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) * (1. - twmsk(jj,jk)) +           &
                                 twmsk(jj,jk) * ( ( 1. + z05cx - ztau )         &
                                 * swbnd(jj,jk,nib ,nitm) - 2.*z05cx              &
                                 * swbnd(jj,jk,nibm,nit ) + ztau * sfow (jj,jk) ) &
                                 / (1. - z05cx)
               END DO
            END DO
         END DO

      END IF

   END SUBROUTINE obc_tra_west


   SUBROUTINE obc_tra_north ( kt )
      !!------------------------------------------------------------------------------
      !!                 ***  SUBROUTINE obc_tra_north  ***
      !!
      !! ** Purpose :
      !!      Apply the radiation algorithm on north OBC tracers ta, sa using the 
      !!      phase velocities calculated in obc_rad_north subroutine in obcrad.F90 module
      !!      If the logical lfbcnorth is .TRUE., there is no radiation but only fixed OBC
      !!
      !!  History :
      !!         ! 95-03 (J.-M. Molines) Original from SPEM
      !!         ! 97-07 (G. Madec, J.-M. Molines) additions
      !!         ! 97-12 (M. Imbard) Mpp adaptation
      !!         ! 00-06 (J.-M. Molines) 
      !!    8.5  ! 02-10 (C. Talandier, A-M. Treguier) F90
      !!------------------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt

      !! * Local declaration
      INTEGER ::   ji, jj, jk      ! dummy loop indices
      REAL(wp) ::   z05cx, ztau, zin
      !!------------------------------------------------------------------------------

      ! 1. First three time steps and more if lfbcnorth is .TRUE.
      !    In that case open boundary conditions are FIXED.
      ! --------------------------------------------------------

! LITTLE_NEMO
!     IF( ( kt < nit000+3 .AND. .NOT.ln_rstart ) .OR. lfbcnorth ) THEN
      IF ( ll_fbc .OR. lfbcnorth ) THEN
! LITTLE_NEMO

         DO jj = fs_njn0+1, fs_njn1+1  ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  tsa(ji,jj,jk,jp_tem)= tsa(ji,jj,jk,jp_tem) * (1.-tnmsk(ji,jk)) + &
                                        tnmsk(ji,jk) * tfon(ji,jk)
                  tsa(ji,jj,jk,jp_sal)= tsa(ji,jj,jk,jp_sal) * (1.-tnmsk(ji,jk)) + &
                                        tnmsk(ji,jk) * sfon(ji,jk)
               END DO
            END DO
         END DO

      ELSE

      ! 2. Beyond the fourth time step if lfbcnorth is .FALSE.
      ! -------------------------------------------------------
          
         ! Temperature and salinity radiation
         ! ----------------------------------
         !
         !           ji-1   ji   ji   ji +1
         !             |
         !    nib //// u // T // u // T //   jpjnob + 1
         !        /////|//////////////////
         !    nib  ----f----v----f----v---   jpjnob
         !             |         |       
         !      nibm-- u -- T -- u -- T --   jpjnob
         !             |         |            
         !   nibm  ----f----v----f----v---  jpjnob-1
         !             |         |      
         !     nibm2-- u -- T -- T -- T --  jpjnob-1
         !             |         |    
         !   nibm2 ----f----v----f----v---  jpjnob-2
         !             |         |
         !
         ! ... radiative conditions + relaxation toward a climatology
         ! ... the phase velocity is taken as the normal phase velocity of the tangen-
         ! ... tial velocity (here un), which has been saved in (u_cynbnd)
         ! ... jpjnob+1,(jpindp1, jpinfm1)
         DO jj = fs_njn0+1, fs_njn1+1 ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 2, jpim1
         ! ... j-phase speed ratio (from averaged of vtnbnd)
         !        (bounded by 1)
                  z05cx = ( 0.5 * ( u_cynbnd(ji,jk) + u_cynbnd(ji-1,jk) ) ) / e2t(ji,jj-1)
                  z05cx = min( z05cx, 1. )
         ! ... z05cx=< 0, inflow  zin=0, ztau=1    
         !           > 0, outflow zin=1, ztau=rtaun
                  zin = sign( 1., z05cx )
                  zin = 0.5*( zin + abs(zin) )
         ! ... for inflow rtaunin is used for relaxation coefficient else rtaun
                  ztau = (1.-zin ) * rtaunin + zin * rtaun
                  z05cx = z05cx * zin
         ! ... update tsa with radiative or climatological (t, s)
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) * (1.-tnmsk(ji,jk)) +             &
                                 tnmsk(ji,jk) * ( ( 1. - z05cx - ztau )         &
                                 * tnbnd(ji,jk,nib ,nitm) + 2.*z05cx              &
                                 * tnbnd(ji,jk,nibm,nit ) + ztau * tfon (ji,jk) ) &
                                 / (1. + z05cx)
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) * (1.-tnmsk(ji,jk)) +             &
                                 tnmsk(ji,jk) * ( ( 1. - z05cx - ztau )         &
                                 * snbnd(ji,jk,nib ,nitm) + 2.*z05cx              &
                                 * snbnd(ji,jk,nibm,nit ) + ztau * sfon (ji,jk) ) &
                                 / (1. + z05cx)
               END DO
            END DO
         END DO

      END IF

   END SUBROUTINE obc_tra_north


   SUBROUTINE obc_tra_south ( kt )
      !!------------------------------------------------------------------------------
      !!                ***  SUBROUTINE obc_tra_south  ***
      !!     
      !! ** Purpose :
      !!      Apply the radiation algorithm on south OBC tracers tsa using the 
      !!      phase velocities calculated in obc_rad_south subroutine in obcrad.F90 module
      !!      If the logical lfbcsouth is .TRUE., there is no radiation but only fixed OBC
      !!
      !!  History :
      !!         ! 95-03 (J.-M. Molines) Original from SPEM
      !!         ! 97-07 (G. Madec, J.-M. Molines) additions
      !!         ! 97-12 (M. Imbard) Mpp adaptation
      !!         ! 00-06 (J.-M. Molines) 
      !!    8.5  ! 02-10 (C. Talandier, A-M Treguier) F90
      !!------------------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   kt

      !! * Local declaration
      INTEGER ::   ji, jj, jk      ! dummy loop indices
      REAL(wp) ::   z05cx, ztau, zin
      !!------------------------------------------------------------------------------

      ! 1. First three time steps and more if lfbcsouth is .TRUE.
      !    In that case open boundary conditions are FIXED.
      ! --------------------------------------------------------

! LITTLE_NEMO
!     IF( ( kt < nit000+3 .AND. .NOT.ln_rstart ) .OR. lfbcsouth ) THEN
      IF ( ll_fbc .OR. lfbcsouth ) THEN
! LITTLE_NEMO

         DO jj = fs_njs0, fs_njs1  ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 1, jpi
                  tsa(ji,jj,jk,jp_tem)= tsa(ji,jj,jk,jp_tem) * (1.-tsmsk(ji,jk)) + &
                                        tsmsk(ji,jk) * tfos(ji,jk)
                  tsa(ji,jj,jk,jp_sal)= tsa(ji,jj,jk,jp_sal) * (1.-tsmsk(ji,jk)) + &
                                        tsmsk(ji,jk) * sfos(ji,jk)
               END DO
            END DO
         END DO

      ELSE

      ! 2. Beyond the fourth time step if lfbcsouth is .FALSE.
      ! -------------------------------------------------------
          
         ! Temperature and salinity radiation
         ! ----------------------------------
         !
         !           ji-1   ji   ji   ji +1
         !             |         |
         !   nibm2 ----f----v----f----v---   jpjsob+2
         !             |         |       
         !   nibm2 --  u -- T -- u -- T --   jpjsob+2
         !             |         |            
         !   nibm  ----f----v----f----v---   jpjsob+1
         !             |         |      
         !    nibm --  u -- T -- T -- T --   jpjsob+1
         !             |         |    
         !   nib  -----f----v----f----v---   jpjsob
         !       //////|/////////|//////// 
         !    nib //// u // T // u // T //   jpjsob 
         !
         !... radiative conditions + relaxation toward a climatology
         !... the phase velocity is taken as the phase velocity of the tangen-
         !... tial velocity (here un), which has been saved in (u_cysbnd)
         !... jpjsob,(jpisdp1, jpisfm1)
         DO jj = fs_njs0, fs_njs1  ! Vector opt.
            DO jk = 1, jpkm1
               DO ji = 2, jpim1
         !... j-phase speed ratio (from averaged of u_cysbnd)
         !       (bounded by 1)
                  z05cx = ( 0.5 * ( u_cysbnd(ji,jk) + u_cysbnd(ji-1,jk) ) ) / e2t(ji,jj+1)
                  z05cx = max( z05cx, -1. )
         !... z05cx > 0, inflow  zin=0, ztau=1
         !          < 0, outflow zin=1, ztau=rtaus
                  zin = sign( 1., -1.* z05cx )
                  zin = 0.5*( zin + abs(zin) )
                  ztau = (1.-zin ) * rtausin + zin * rtaus
                  z05cx = z05cx * zin

         !... update tsa with radiative or climatological (t, s)
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) * (1.-tsmsk(ji,jk)) +             &
                                 tsmsk(ji,jk) * ( ( 1. + z05cx - ztau )         &
                                 * tsbnd(ji,jk,nib ,nitm) - 2.*z05cx              &
                                 * tsbnd(ji,jk,nibm,nit ) + ztau * tfos (ji,jk) ) &
                                 / (1. - z05cx)
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) * (1.-tsmsk(ji,jk)) +             &
                                 tsmsk(ji,jk) * (  ( 1. + z05cx - ztau )        &
                                 * ssbnd(ji,jk,nib ,nitm) - 2.*z05cx              &
                                 * ssbnd(ji,jk,nibm,nit ) + ztau * sfos (ji,jk) ) &
                                 / (1. - z05cx)
               END DO
            END DO
         END DO

      END IF   

   END SUBROUTINE obc_tra_south

#else
   !!---------------------------------------------------------------------------------
   !!   Default option                                                    Empty module
   !!---------------------------------------------------------------------------------
CONTAINS
   SUBROUTINE obc_tra      ! Empty routine
   END SUBROUTINE obc_tra
#endif

   !!=================================================================================
END MODULE obctra
