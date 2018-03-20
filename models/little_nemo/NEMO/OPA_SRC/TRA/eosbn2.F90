MODULE eosbn2
   !!==============================================================================
   !!                       ***  MODULE  eosbn2  ***
   !! Ocean diagnostic variable : equation of state - in situ and potential density
   !!                                               - Brunt-Vaisala frequency
   !!==============================================================================
   !! History :  OPA  ! 1989-03  (O. Marti)  Original code
   !!            6.0  ! 1994-07  (G. Madec, M. Imbard)  add bn2
   !!            6.0  ! 1994-08  (G. Madec)  Add Jackett & McDougall eos
   !!            7.0  ! 1996-01  (G. Madec)  statement function for e3
   !!            8.1  ! 1997-07  (G. Madec)  density instead of volumic mass
   !!             -   ! 1999-02  (G. Madec, N. Grima) semi-implicit pressure gradient
   !!            8.2  ! 2001-09  (M. Ben Jelloul)  bugfix on linear eos
   !!   NEMO     1.0  ! 2002-10  (G. Madec)  add eos_init
   !!             -   ! 2002-11  (G. Madec, A. Bozec)  partial step, eos_insitu_2d
   !!             -   ! 2003-08  (G. Madec)  F90, free form
   !!            3.0  ! 2006-08  (G. Madec)  add tfreez function
   !!            3.3  ! 2010-05  (C. Ethe, G. Madec)  merge TRC-TRA
   !!             -   ! 2010-10  (G. Nurser, G. Madec)  add eos_alpbet used in ldfslp
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   eos            : generic interface of the equation of state
   !!   eos_insitu     : Compute the in situ density
   !!   eos_insitu_pot : Compute the insitu and surface referenced potential
   !!                    volumic mass
   !!   eos_insitu_2d  : Compute the in situ density for 2d fields
   !!   eos_bn2        : Compute the Brunt-Vaisala frequency
   !!   eos_alpbet     : calculates the in situ thermal/haline expansion ratio
   !!   tfreez         : Compute the surface freezing temperature
   !!   eos_init       : set eos parameters (namelist)
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE zdfddm          ! vertical physics: double diffusion
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! MPP library
   USE prtctl          ! Print control
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   !                   !! * Interface
   INTERFACE eos
      MODULE PROCEDURE eos_insitu, eos_insitu_pot, eos_insitu_2d
   END INTERFACE
   INTERFACE bn2
      MODULE PROCEDURE eos_bn2
   END INTERFACE

   PUBLIC   eos        ! called by step, istate, tranpc and zpsgrd modules
   PUBLIC   eos_init   ! called by istate module
   PUBLIC   bn2        ! called by step module
   PUBLIC   eos_alpbet ! called by ldfslp module
   PUBLIC   tfreez     ! called by sbcice_... modules

   !                                          !!* Namelist (nameos) *
   INTEGER , PUBLIC ::   nn_eos   = 0         !: = 0/1/2 type of eq. of state and Brunt-Vaisala frequ.
   REAL(wp), PUBLIC ::   rn_alpha = 2.0e-4_wp !: thermal expension coeff. (linear equation of state)
   REAL(wp), PUBLIC ::   rn_beta  = 7.7e-4_wp !: saline  expension coeff. (linear equation of state)

   REAL(wp), PUBLIC ::   ralpbet              !: alpha / beta ratio

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: eosbn2.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE eos_insitu( pts, prd )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE eos_insitu  ***
      !!
      !! ** Purpose :   Compute the in situ density (ratio rho/rau0) from
      !!       potential temperature and salinity using an equation of state
      !!       defined through the namelist parameter nn_eos.
      !!
      !! ** Method  :   3 cases:
      !!      nn_eos = 0 : Jackett and McDougall (1994) equation of state.
      !!         the in situ density is computed directly as a function of
      !!         potential temperature relative to the surface (the opa t
      !!         variable), salt and pressure (assuming no pressure variation
      !!         along geopotential surfaces, i.e. the pressure p in decibars
      !!         is approximated by the depth in meters.
      !!              prd(t,s,p) = ( rho(t,s,p) - rau0 ) / rau0
      !!         with pressure                      p        decibars
      !!              potential temperature         t        deg celsius
      !!              salinity                      s        psu
      !!              reference volumic mass        rau0     kg/m**3
      !!              in situ volumic mass          rho      kg/m**3
      !!              in situ density anomalie      prd      no units
      !!         Check value: rho = 1060.93298 kg/m**3 for p=10000 dbar,
      !!          t = 40 deg celcius, s=40 psu
      !!      nn_eos = 1 : linear equation of state function of temperature only
      !!              prd(t) = 0.0285 - rn_alpha * t
      !!      nn_eos = 2 : linear equation of state function of temperature and
      !!               salinity
      !!              prd(t,s) = rn_beta * s - rn_alpha * tn - 1.
      !!      Note that no boundary condition problem occurs in this routine
      !!      as pts are defined over the whole domain.
      !!
      !! ** Action  :   compute prd , the in situ density (no units)
      !!
      !! References :   Jackett and McDougall, J. Atmos. Ocean. Tech., 1994
      !!----------------------------------------------------------------------
      !!
      REAL(wp), DIMENSION(:,:,:,:), INTENT(in   ) ::   pts   ! 1 : potential temperature  [Celcius]
      !                                                      ! 2 : salinity               [psu]
      REAL(wp), DIMENSION(:,:,:)  , INTENT(  out) ::   prd   ! in situ density            [-]
      !!
      INTEGER  ::   ji, jj, jk           ! dummy loop indices
      REAL(wp) ::   zt , zs , zh , zsr   ! local scalars
      REAL(wp) ::   zr1, zr2, zr3, zr4   !   -      -
      REAL(wp) ::   zrhop, ze, zbw, zb   !   -      -
      REAL(wp) ::   zd , zc , zaw, za    !   -      -
      REAL(wp) ::   zb1, za1, zkw, zk0   !   -      -
      REAL(wp) ::   zrau0r               !   -      -
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zws
      !!----------------------------------------------------------------------

      !
      IF( nn_timing == 1 ) CALL timing_start('eos')
      !
      CALL wrk_alloc( jpi, jpj, jpk, zws )
      !
      SELECT CASE( nn_eos )
      !
      CASE( 0 )                !==  Jackett and McDougall (1994) formulation  ==!
         zrau0r = 1.e0 / rau0
!CDIR NOVERRCHK
         zws(:,:,:) = SQRT( ABS( pts(:,:,:,jp_sal) ) )
         !
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zt = pts   (ji,jj,jk,jp_tem)
                  zs = pts   (ji,jj,jk,jp_sal)
                  zh = fsdept(ji,jj,jk)        ! depth
                  zsr= zws   (ji,jj,jk)        ! square root salinity
                  !
                  ! compute volumic mass pure water at atm pressure
                  zr1= ( ( ( ( 6.536332e-9_wp  *zt - 1.120083e-6_wp )*zt + 1.001685e-4_wp )*zt   &
                     &        -9.095290e-3_wp )*zt + 6.793952e-2_wp )*zt +  999.842594_wp
                  ! seawater volumic mass atm pressure
                  zr2= ( ( ( 5.3875e-9_wp*zt-8.2467e-7_wp ) *zt+7.6438e-5_wp ) *zt        &
                     &                      -4.0899e-3_wp ) *zt+0.824493_wp
                  zr3= ( -1.6546e-6_wp*zt+1.0227e-4_wp )    *zt-5.72466e-3_wp
                  zr4= 4.8314e-4_wp
                  !
                  ! potential volumic mass (reference to the surface)
                  zrhop= ( zr4*zs + zr3*zsr + zr2 ) *zs + zr1
                  !
                  ! add the compression terms
                  ze = ( -3.508914e-8_wp*zt-1.248266e-8_wp ) *zt-2.595994e-6_wp
                  zbw= (  1.296821e-6_wp*zt-5.782165e-9_wp ) *zt+1.045941e-4_wp
                  zb = zbw + ze * zs
                  !
                  zd = -2.042967e-2_wp
                  zc =   (-7.267926e-5_wp*zt+2.598241e-3_wp ) *zt+0.1571896_wp
                  zaw= ( ( 5.939910e-6_wp*zt+2.512549e-3_wp ) *zt-0.1028859_wp ) *zt - 4.721788_wp
                  za = ( zd*zsr + zc ) *zs + zaw
                  !
                  zb1=   (-0.1909078_wp*zt+7.390729_wp )        *zt-55.87545_wp
                  za1= ( ( 2.326469e-3_wp*zt+1.553190_wp)       *zt-65.00517_wp ) *zt+1044.077_wp
                  zkw= ( ( (-1.361629e-4_wp*zt-1.852732e-2_wp ) *zt-30.41638_wp ) *zt + 2098.925_wp ) *zt+190925.6_wp
                  zk0= ( zb1*zsr + za1 )*zs + zkw
                  !
                  ! masked in situ density anomaly
                  prd(ji,jj,jk) = (  zrhop / (  1.0_wp - zh / ( zk0 - zh * ( za - zh * zb ) )  )    &
                     &             - rau0  ) * zrau0r * tmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      CASE( 1 )                !==  Linear formulation function of temperature only  ==!
         DO jk = 1, jpkm1
            prd(:,:,jk) = ( 0.0285_wp - rn_alpha * pts(:,:,jk,jp_tem) ) * tmask(:,:,jk)
         END DO
         !
      CASE( 2 )                !==  Linear formulation function of temperature and salinity  ==!
         DO jk = 1, jpkm1
            prd(:,:,jk) = ( rn_beta  * pts(:,:,jk,jp_sal) - rn_alpha * pts(:,:,jk,jp_tem) ) * tmask(:,:,jk)
         END DO
         !
      END SELECT
      !
      IF(ln_ctl)   CALL prt_ctl( tab3d_1=prd, clinfo1=' eos  : ', ovlap=1, kdim=jpk )
      !
      CALL wrk_dealloc( jpi, jpj, jpk, zws )
      !
      IF( nn_timing == 1 ) CALL timing_stop('eos')
      !
   END SUBROUTINE eos_insitu


   SUBROUTINE eos_insitu_pot( pts, prd, prhop )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE eos_insitu_pot  ***
      !!
      !! ** Purpose :   Compute the in situ density (ratio rho/rau0) and the
      !!      potential volumic mass (Kg/m3) from potential temperature and
      !!      salinity fields using an equation of state defined through the
      !!     namelist parameter nn_eos.
      !!
      !! ** Method  :
      !!      nn_eos = 0 : Jackett and McDougall (1994) equation of state.
      !!         the in situ density is computed directly as a function of
      !!         potential temperature relative to the surface (the opa t
      !!         variable), salt and pressure (assuming no pressure variation
      !!         along geopotential surfaces, i.e. the pressure p in decibars
      !!         is approximated by the depth in meters.
      !!              prd(t,s,p) = ( rho(t,s,p) - rau0 ) / rau0
      !!              rhop(t,s)  = rho(t,s,0)
      !!         with pressure                      p        decibars
      !!              potential temperature         t        deg celsius
      !!              salinity                      s        psu
      !!              reference volumic mass        rau0     kg/m**3
      !!              in situ volumic mass          rho      kg/m**3
      !!              in situ density anomalie      prd      no units
      !!
      !!         Check value: rho = 1060.93298 kg/m**3 for p=10000 dbar,
      !!          t = 40 deg celcius, s=40 psu
      !!
      !!      nn_eos = 1 : linear equation of state function of temperature only
      !!              prd(t) = ( rho(t) - rau0 ) / rau0 = 0.028 - rn_alpha * t
      !!              rhop(t,s)  = rho(t,s)
      !!
      !!      nn_eos = 2 : linear equation of state function of temperature and
      !!               salinity
      !!              prd(t,s) = ( rho(t,s) - rau0 ) / rau0
      !!                       = rn_beta * s - rn_alpha * tn - 1.
      !!              rhop(t,s)  = rho(t,s)
      !!      Note that no boundary condition problem occurs in this routine
      !!      as (tn,sn) or (ta,sa) are defined over the whole domain.
      !!
      !! ** Action  : - prd  , the in situ density (no units)
      !!              - prhop, the potential volumic mass (Kg/m3)
      !!
      !! References :   Jackett and McDougall, J. Atmos. Ocean. Tech., 1994
      !!                Brown and Campana, Mon. Weather Rev., 1978
      !!----------------------------------------------------------------------
      !!
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts), INTENT(in   ) ::   pts    ! 1 : potential temperature  [Celcius]
      !                                                                ! 2 : salinity               [psu]
      REAL(wp), DIMENSION(jpi,jpj,jpk     ), INTENT(  out) ::   prd    ! in situ density            [-]
      REAL(wp), DIMENSION(jpi,jpj,jpk     ), INTENT(  out) ::   prhop  ! potential density (surface referenced)
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zt, zs, zh, zsr, zr1, zr2, zr3, zr4, zrhop, ze, zbw   ! local scalars
      REAL(wp) ::   zb, zd, zc, zaw, za, zb1, za1, zkw, zk0, zrau0r       !   -      -
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zws
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 ) CALL timing_start('eos-p')
      !
      CALL wrk_alloc( jpi, jpj, jpk, zws )
      !
      SELECT CASE ( nn_eos )
      !
      CASE( 0 )                !==  Jackett and McDougall (1994) formulation  ==!
         zrau0r = 1.e0 / rau0
!CDIR NOVERRCHK
         zws(:,:,:) = SQRT( ABS( pts(:,:,:,jp_sal) ) )
         !
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zt = pts   (ji,jj,jk,jp_tem)
                  zs = pts   (ji,jj,jk,jp_sal)
                  zh = fsdept(ji,jj,jk)        ! depth
                  zsr= zws   (ji,jj,jk)        ! square root salinity
                  !
                  ! compute volumic mass pure water at atm pressure
                  zr1= ( ( ( ( 6.536332e-9_wp*zt-1.120083e-6_wp )*zt+1.001685e-4_wp )*zt   &
                     &                          -9.095290e-3_wp )*zt+6.793952e-2_wp )*zt+999.842594_wp
                  ! seawater volumic mass atm pressure
                  zr2= ( ( ( 5.3875e-9_wp*zt-8.2467e-7_wp ) *zt+7.6438e-5_wp ) *zt   &
                     &                                         -4.0899e-3_wp ) *zt+0.824493_wp
                  zr3= ( -1.6546e-6_wp*zt+1.0227e-4_wp )    *zt-5.72466e-3_wp
                  zr4= 4.8314e-4_wp
                  !
                  ! potential volumic mass (reference to the surface)
                  zrhop= ( zr4*zs + zr3*zsr + zr2 ) *zs + zr1
                  !
                  ! save potential volumic mass
                  prhop(ji,jj,jk) = zrhop * tmask(ji,jj,jk)
                  !
                  ! add the compression terms
                  ze = ( -3.508914e-8_wp*zt-1.248266e-8_wp ) *zt-2.595994e-6_wp
                  zbw= (  1.296821e-6_wp*zt-5.782165e-9_wp ) *zt+1.045941e-4_wp
                  zb = zbw + ze * zs
                  !
                  zd = -2.042967e-2_wp
                  zc =   (-7.267926e-5_wp*zt+2.598241e-3_wp ) *zt+0.1571896_wp
                  zaw= ( ( 5.939910e-6_wp*zt+2.512549e-3_wp ) *zt-0.1028859_wp ) *zt - 4.721788_wp
                  za = ( zd*zsr + zc ) *zs + zaw
                  !
                  zb1=   (  -0.1909078_wp  *zt+7.390729_wp    ) *zt-55.87545_wp
                  za1= ( (   2.326469e-3_wp*zt+1.553190_wp    ) *zt-65.00517_wp ) *zt + 1044.077_wp
                  zkw= ( ( (-1.361629e-4_wp*zt-1.852732e-2_wp ) *zt-30.41638_wp ) *zt + 2098.925_wp ) *zt+190925.6_wp
                  zk0= ( zb1*zsr + za1 )*zs + zkw
                  !
                  ! masked in situ density anomaly
                  prd(ji,jj,jk) = (  zrhop / (  1.0_wp - zh / ( zk0 - zh * ( za - zh * zb ) )  )    &
                     &             - rau0  ) * zrau0r * tmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      CASE( 1 )                !==  Linear formulation = F( temperature )  ==!
         DO jk = 1, jpkm1
            prd  (:,:,jk) = ( 0.0285_wp - rn_alpha * pts(:,:,jk,jp_tem) )        * tmask(:,:,jk)
            prhop(:,:,jk) = ( 1.e0_wp   +            prd (:,:,jk)       ) * rau0 * tmask(:,:,jk)
         END DO
         !
      CASE( 2 )                !==  Linear formulation = F( temperature , salinity )  ==!
         DO jk = 1, jpkm1
            prd  (:,:,jk) = ( rn_beta  * pts(:,:,jk,jp_sal) - rn_alpha * pts(:,:,jk,jp_tem) )        * tmask(:,:,jk)
            prhop(:,:,jk) = ( 1.e0_wp  + prd (:,:,jk) )                                       * rau0 * tmask(:,:,jk)
         END DO
         !
      END SELECT
      !
      IF(ln_ctl)   CALL prt_ctl( tab3d_1=prd, clinfo1=' eos-p: ', tab3d_2=prhop, clinfo2=' pot : ', ovlap=1, kdim=jpk )
      !
      CALL wrk_dealloc( jpi, jpj, jpk, zws )
      !
      IF( nn_timing == 1 ) CALL timing_stop('eos-p')
      !
   END SUBROUTINE eos_insitu_pot


   SUBROUTINE eos_insitu_2d( pts, pdep, prd )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE eos_insitu_2d  ***
      !!
      !! ** Purpose :   Compute the in situ density (ratio rho/rau0) from
      !!      potential temperature and salinity using an equation of state
      !!      defined through the namelist parameter nn_eos. * 2D field case
      !!
      !! ** Method :
      !!      nn_eos = 0 : Jackett and McDougall (1994) equation of state.
      !!         the in situ density is computed directly as a function of
      !!         potential temperature relative to the surface (the opa t
      !!         variable), salt and pressure (assuming no pressure variation
      !!         along geopotential surfaces, i.e. the pressure p in decibars
      !!         is approximated by the depth in meters.
      !!              prd(t,s,p) = ( rho(t,s,p) - rau0 ) / rau0
      !!         with pressure                      p        decibars
      !!              potential temperature         t        deg celsius
      !!              salinity                      s        psu
      !!              reference volumic mass        rau0     kg/m**3
      !!              in situ volumic mass          rho      kg/m**3
      !!              in situ density anomalie      prd      no units
      !!         Check value: rho = 1060.93298 kg/m**3 for p=10000 dbar,
      !!          t = 40 deg celcius, s=40 psu
      !!      nn_eos = 1 : linear equation of state function of temperature only
      !!              prd(t) = 0.0285 - rn_alpha * t
      !!      nn_eos = 2 : linear equation of state function of temperature and
      !!               salinity
      !!              prd(t,s) = rn_beta * s - rn_alpha * tn - 1.
      !!      Note that no boundary condition problem occurs in this routine
      !!      as pts are defined over the whole domain.
      !!
      !! ** Action  : - prd , the in situ density (no units)
      !!
      !! References :   Jackett and McDougall, J. Atmos. Ocean. Tech., 1994
      !!----------------------------------------------------------------------
      !!
      REAL(wp), DIMENSION(jpi,jpj,jpts), INTENT(in   ) ::   pts   ! 1 : potential temperature  [Celcius]
      !                                                           ! 2 : salinity               [psu]
      REAL(wp), DIMENSION(jpi,jpj)     , INTENT(in   ) ::   pdep  ! depth                  [m]
      REAL(wp), DIMENSION(jpi,jpj)     , INTENT(  out) ::   prd   ! in situ density
      !!
      INTEGER  ::   ji, jj                    ! dummy loop indices
      REAL(wp) ::   zt, zs, zh, zsr, zr1, zr2, zr3, zr4, zrhop, ze, zbw   ! temporary scalars
      REAL(wp) ::   zb, zd, zc, zaw, za, zb1, za1, zkw, zk0, zmask        !    -         -
      REAL(wp), POINTER, DIMENSION(:,:) :: zws
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 ) CALL timing_start('eos2d')
      !
      CALL wrk_alloc( jpi, jpj, zws )
      !

      prd(:,:) = 0._wp

      SELECT CASE( nn_eos )
      !
      CASE( 0 )                !==  Jackett and McDougall (1994) formulation  ==!
      !
!CDIR NOVERRCHK
         DO jj = 1, jpjm1
!CDIR NOVERRCHK
            DO ji = 1, fs_jpim1   ! vector opt.
               zws(ji,jj) = SQRT( ABS( pts(ji,jj,jp_sal) ) )
            END DO
         END DO
         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1   ! vector opt.
               zmask = tmask(ji,jj,1)          ! land/sea bottom mask = surf. mask
               zt    = pts  (ji,jj,jp_tem)            ! interpolated T
               zs    = pts  (ji,jj,jp_sal)            ! interpolated S
               zsr   = zws  (ji,jj)            ! square root of interpolated S
               zh    = pdep (ji,jj)            ! depth at the partial step level
               !
               ! compute volumic mass pure water at atm pressure
               zr1 = ( ( ( ( 6.536332e-9_wp*zt-1.120083e-6_wp )*zt+1.001685e-4_wp )*zt   &
                  &                        -9.095290e-3_wp )*zt+6.793952e-2_wp )*zt+999.842594_wp
               ! seawater volumic mass atm pressure
               zr2 = ( ( ( 5.3875e-9_wp*zt-8.2467e-7_wp )*zt+7.6438e-5_wp ) *zt   &
                  &                                   -4.0899e-3_wp ) *zt+0.824493_wp
               zr3 = ( -1.6546e-6_wp*zt+1.0227e-4_wp ) *zt-5.72466e-3_wp
               zr4 = 4.8314e-4_wp
               !
               ! potential volumic mass (reference to the surface)
               zrhop= ( zr4*zs + zr3*zsr + zr2 ) *zs + zr1
               !
               ! add the compression terms
               ze = ( -3.508914e-8_wp*zt-1.248266e-8_wp ) *zt-2.595994e-6_wp
               zbw= (  1.296821e-6_wp*zt-5.782165e-9_wp ) *zt+1.045941e-4_wp
               zb = zbw + ze * zs
               !
               zd =    -2.042967e-2_wp
               zc =   (-7.267926e-5_wp*zt+2.598241e-3_wp ) *zt+0.1571896_wp
               zaw= ( ( 5.939910e-6_wp*zt+2.512549e-3_wp ) *zt-0.1028859_wp ) *zt -4.721788_wp
               za = ( zd*zsr + zc ) *zs + zaw
               !
               zb1=     (-0.1909078_wp  *zt+7.390729_wp      ) *zt-55.87545_wp
               za1=   ( ( 2.326469e-3_wp*zt+1.553190_wp      ) *zt-65.00517_wp ) *zt+1044.077_wp
               zkw= ( ( (-1.361629e-4_wp*zt-1.852732e-2_wp   ) *zt-30.41638_wp ) *zt   &
                  &                             +2098.925_wp ) *zt+190925.6_wp
               zk0= ( zb1*zsr + za1 )*zs + zkw
               !
               ! masked in situ density anomaly
               prd(ji,jj) = ( zrhop / (  1.0_wp - zh / ( zk0 - zh * ( za - zh * zb ) )  ) - rau0 ) / rau0 * zmask
            END DO
         END DO
         !
      CASE( 1 )                !==  Linear formulation = F( temperature )  ==!
         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1   ! vector opt.
               prd(ji,jj) = ( 0.0285_wp - rn_alpha * pts(ji,jj,jp_tem) ) * tmask(ji,jj,1)
            END DO
         END DO
         !
      CASE( 2 )                !==  Linear formulation = F( temperature , salinity )  ==!
         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1   ! vector opt.
               prd(ji,jj) = ( rn_beta * pts(ji,jj,jp_sal) - rn_alpha * pts(ji,jj,jp_tem) ) * tmask(ji,jj,1)
            END DO
         END DO
         !
      END SELECT

      IF(ln_ctl)   CALL prt_ctl( tab2d_1=prd, clinfo1=' eos2d: ' )
      !
      CALL wrk_dealloc( jpi, jpj, zws )
      !
      IF( nn_timing == 1 ) CALL timing_stop('eos2d')
      !
   END SUBROUTINE eos_insitu_2d


   SUBROUTINE eos_bn2( pts, pn2 )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE eos_bn2  ***
      !!
      !! ** Purpose :   Compute the local Brunt-Vaisala frequency at the time-
      !!      step of the input arguments
      !!
      !! ** Method :
      !!       * nn_eos = 0  : UNESCO sea water properties
      !!         The brunt-vaisala frequency is computed using the polynomial
      !!      polynomial expression of McDougall (1987):
      !!            N^2 = grav * beta * ( alpha/beta*dk[ t ] - dk[ s ] )/e3w
      !!      If lk_zdfddm=T, the heat/salt buoyancy flux ratio Rrau is
      !!      computed and used in zdfddm module :
      !!              Rrau = alpha/beta * ( dk[ t ] / dk[ s ] )
      !!       * nn_eos = 1  : linear equation of state (temperature only)
      !!            N^2 = grav * rn_alpha * dk[ t ]/e3w
      !!       * nn_eos = 2  : linear equation of state (temperature & salinity)
      !!            N^2 = grav * (rn_alpha * dk[ t ] - rn_beta * dk[ s ] ) / e3w
      !!      The use of potential density to compute N^2 introduces e r r o r
      !!      in the sign of N^2 at great depths. We recommand the use of
      !!      nn_eos = 0, except for academical studies.
      !!        Macro-tasked on horizontal slab (jk-loop)
      !!      N.B. N^2 is set to zero at the first level (JK=1) in inidtr
      !!      and is never used at this level.
      !!
      !! ** Action  : - pn2 : the brunt-vaisala frequency
      !!
      !! References :   McDougall, J. Phys. Oceanogr., 17, 1950-1964, 1987.
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts), INTENT(in   ) ::   pts   ! 1 : potential temperature  [Celcius]
      !                                                               ! 2 : salinity               [psu]
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   pn2   ! Brunt-Vaisala frequency    [s-1]
      !!
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zgde3w, zt, zs, zh, zalbet, zbeta   ! local scalars
#if defined key_zdfddm
      REAL(wp) ::   zds   ! local scalars
#endif
      !!----------------------------------------------------------------------

      !
      IF( nn_timing == 1 ) CALL timing_start('bn2')
      !
      ! pn2 : interior points only (2=< jk =< jpkm1 )
      ! --------------------------
      !
      SELECT CASE( nn_eos )
      !
      CASE( 0 )                !==  Jackett and McDougall (1994) formulation  ==!
         DO jk = 2, jpkm1
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zgde3w = grav / fse3w(ji,jj,jk)
                  zt = 0.5 * ( pts(ji,jj,jk,jp_tem) + pts(ji,jj,jk-1,jp_tem) )         ! potential temperature at w-pt
                  zs = 0.5 * ( pts(ji,jj,jk,jp_sal) + pts(ji,jj,jk-1,jp_sal) ) - 35.0  ! salinity anomaly (s-35) at w-pt
                  zh = fsdepw(ji,jj,jk)                                                ! depth in meters  at w-point
                  !
                  zalbet = ( ( ( - 0.255019e-07_wp * zt + 0.298357e-05_wp ) * zt   &   ! ratio alpha/beta
                     &                                  - 0.203814e-03_wp ) * zt   &
                     &                                  + 0.170907e-01_wp ) * zt   &
                     &   +         0.665157e-01_wp                                 &
                     &   +     ( - 0.678662e-05_wp * zs                            &
                     &           - 0.846960e-04_wp * zt + 0.378110e-02_wp ) * zs   &
                     &   +   ( ( - 0.302285e-13_wp * zh                            &
                     &           - 0.251520e-11_wp * zs                            &
                     &           + 0.512857e-12_wp * zt * zt              ) * zh   &
                     &           - 0.164759e-06_wp * zs                            &
                     &        +(   0.791325e-08_wp * zt - 0.933746e-06_wp ) * zt   &
                     &                                  + 0.380374e-04_wp ) * zh
                     !
                  zbeta  = ( ( -0.415613e-09_wp * zt + 0.555579e-07_wp ) * zt      &   ! beta
                     &                               - 0.301985e-05_wp ) * zt      &
                     &   +       0.785567e-03_wp                                   &
                     &   + (     0.515032e-08_wp * zs                              &
                     &         + 0.788212e-08_wp * zt - 0.356603e-06_wp ) * zs     &
                     &   + ( (   0.121551e-17_wp * zh                              &
                     &         - 0.602281e-15_wp * zs                              &
                     &         - 0.175379e-14_wp * zt + 0.176621e-12_wp ) * zh     &
                     &                                + 0.408195e-10_wp   * zs     &
                     &     + ( - 0.213127e-11_wp * zt + 0.192867e-09_wp ) * zt     &
                     &                                - 0.121555e-07_wp ) * zh
                     !
                  pn2(ji,jj,jk) = zgde3w * zbeta * tmask(ji,jj,jk)           &   ! N^2
                     &          * ( zalbet * ( pts(ji,jj,jk-1,jp_tem) - pts(ji,jj,jk,jp_tem) )   &
                     &                     - ( pts(ji,jj,jk-1,jp_sal) - pts(ji,jj,jk,jp_sal) ) )
#if defined key_zdfddm
                  !                                                         !!bug **** caution a traiter zds=dk[S]= 0 !!!!
                  zds = ( pts(ji,jj,jk-1,jp_sal) - pts(ji,jj,jk,jp_sal) )                    ! Rrau = (alpha / beta) (dk[t] / dk[s])
                  IF ( ABS( zds) <= 1.e-20_wp ) zds = 1.e-20_wp
                  rrau(ji,jj,jk) = zalbet * ( pts(ji,jj,jk-1,jp_tem) - pts(ji,jj,jk,jp_tem) ) / zds
#endif
               END DO
            END DO
         END DO
         !
      CASE( 1 )                !==  Linear formulation = F( temperature )  ==!
         DO jk = 2, jpkm1
            pn2(:,:,jk) = grav * rn_alpha * ( pts(:,:,jk-1,jp_tem) - pts(:,:,jk,jp_tem) ) / fse3w(:,:,jk) * tmask(:,:,jk)
         END DO
         !
      CASE( 2 )                !==  Linear formulation = F( temperature , salinity )  ==!
         DO jk = 2, jpkm1
            pn2(:,:,jk) = grav * (  rn_alpha * ( pts(:,:,jk-1,jp_tem) - pts(:,:,jk,jp_tem) )      &
               &                  - rn_beta  * ( pts(:,:,jk-1,jp_sal) - pts(:,:,jk,jp_sal) )  )   &
               &               / fse3w(:,:,jk) * tmask(:,:,jk)
         END DO
#if defined key_zdfddm
         DO jk = 2, jpkm1                                 ! Rrau = (alpha / beta) (dk[t] / dk[s])
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zds = ( pts(ji,jj,jk-1,jp_sal) - pts(ji,jj,jk,jp_sal) )
                  IF ( ABS( zds ) <= 1.e-20_wp ) zds = 1.e-20_wp
                  rrau(ji,jj,jk) = ralpbet * ( pts(ji,jj,jk-1,jp_tem) - pts(ji,jj,jk,jp_tem) ) / zds
               END DO
            END DO
         END DO
#endif
      END SELECT

      IF(ln_ctl)   CALL prt_ctl( tab3d_1=pn2, clinfo1=' bn2  : ', ovlap=1, kdim=jpk )
#if defined key_zdfddm
      IF(ln_ctl)   CALL prt_ctl( tab3d_1=rrau, clinfo1=' rrau : ', ovlap=1, kdim=jpk )
#endif
      !
      IF( nn_timing == 1 ) CALL timing_stop('bn2')
      !
   END SUBROUTINE eos_bn2


   SUBROUTINE eos_alpbet( pts, palpbet, beta0 )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE eos_alpbet  ***
      !!
      !! ** Purpose :   Calculates the in situ thermal/haline expansion ratio at T-points
      !!
      !! ** Method  :   calculates alpha / beta ratio at T-points
      !!       * nn_eos = 0  : UNESCO sea water properties
      !!                       The alpha/beta ratio is returned as 3-D array palpbet using the polynomial
      !!                       polynomial expression of McDougall (1987).
      !!                       Scalar beta0 is returned = 1.
      !!       * nn_eos = 1  : linear equation of state (temperature only)
      !!                       The ratio is undefined, so we return alpha as palpbet
      !!                       Scalar beta0 is returned = 0.
      !!       * nn_eos = 2  : linear equation of state (temperature & salinity)
      !!                       The alpha/beta ratio is returned as ralpbet
      !!                       Scalar beta0 is returned = 1.
      !!
      !! ** Action  : - palpbet : thermal/haline expansion ratio at T-points
      !!            :   beta0   : 1. or 0.
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts), INTENT(in   ) ::   pts       ! pot. temperature & salinity
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   palpbet   ! thermal/haline expansion ratio
      REAL(wp),                              INTENT(  out) ::   beta0     ! set = 1 except with case 1 eos, rho=rho(T)
      !!
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zt, zs, zh   ! local scalars
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 ) CALL timing_start('eos_alpbet')
      !
      SELECT CASE ( nn_eos )
      !
      CASE ( 0 )               ! Jackett and McDougall (1994) formulation
         DO jk = 1, jpk
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zt = pts(ji,jj,jk,jp_tem)           ! potential temperature
                  zs = pts(ji,jj,jk,jp_sal) - 35._wp  ! salinity anomaly (s-35)
                  zh = fsdept(ji,jj,jk)               ! depth in meters
                  !
                  palpbet(ji,jj,jk) =                                              &
                     &     ( ( ( - 0.255019e-07_wp * zt + 0.298357e-05_wp ) * zt   &
                     &                                  - 0.203814e-03_wp ) * zt   &
                     &                                  + 0.170907e-01_wp ) * zt   &
                     &   + 0.665157e-01_wp                                         &
                     &   +     ( - 0.678662e-05_wp * zs                            &
                     &           - 0.846960e-04_wp * zt + 0.378110e-02_wp ) * zs   &
                     &   +   ( ( - 0.302285e-13_wp * zh                            &
                     &           - 0.251520e-11_wp * zs                            &
                     &           + 0.512857e-12_wp * zt * zt              ) * zh   &
                     &           - 0.164759e-06_wp * zs                            &
                     &        +(   0.791325e-08_wp * zt - 0.933746e-06_wp ) * zt   &
                     &                                  + 0.380374e-04_wp ) * zh
               END DO
            END DO
         END DO
         beta0 = 1._wp
         !
      CASE ( 1 )              !==  Linear formulation = F( temperature )  ==!
         palpbet(:,:,:) = rn_alpha
         beta0 = 0._wp
         !
      CASE ( 2 )              !==  Linear formulation = F( temperature , salinity )  ==!
         palpbet(:,:,:) = ralpbet
         beta0 = 1._wp
         !
      CASE DEFAULT
         IF(lwp) WRITE(numout,cform_err)
         IF(lwp) WRITE(numout,*) '          bad flag value for nn_eos = ', nn_eos
         nstop = nstop + 1
         !
      END SELECT
      !
      IF( nn_timing == 1 ) CALL timing_stop('eos_alpbet')
      !
   END SUBROUTINE eos_alpbet


   FUNCTION tfreez( psal ) RESULT( ptf )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE eos_init  ***
      !!
      !! ** Purpose :   Compute the sea surface freezing temperature [Celcius]
      !!
      !! ** Method  :   UNESCO freezing point at the surface (pressure = 0???)
      !!       freezing point [Celcius]=(-.0575+1.710523e-3*sqrt(abs(s))-2.154996e-4*s)*s-7.53e-4*p
      !!       checkvalue: tf= -2.588567 Celsius for s=40.0psu, p=500. decibars
      !!
      !! Reference  :   UNESCO tech. papers in the marine science no. 28. 1978
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) ::   psal   ! salinity             [psu]
      ! Leave result array automatic rather than making explicitly allocated
      REAL(wp), DIMENSION(jpi,jpj)                ::   ptf    ! freezing temperature [Celcius]
      !!----------------------------------------------------------------------
      !
      ptf(:,:) = ( - 0.0575_wp + 1.710523e-3_wp * SQRT( psal(:,:) )   &
         &                     - 2.154996e-4_wp *       psal(:,:)   ) * psal(:,:)
      !
   END FUNCTION tfreez


   SUBROUTINE eos_init
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE eos_init  ***
      !!
      !! ** Purpose :   initializations for the equation of state
      !!
      !! ** Method  :   Read the namelist nameos and control the parameters
      !!----------------------------------------------------------------------
      NAMELIST/nameos/ nn_eos, rn_alpha, rn_beta
      !!----------------------------------------------------------------------
      !
      REWIND( numnam )            ! Read Namelist nameos : equation of state
      READ  ( numnam, nameos )
      !
      IF(lwp) THEN                ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'eos_init : equation of state'
         WRITE(numout,*) '~~~~~~~~'
         WRITE(numout,*) '          Namelist nameos : set eos parameters'
         WRITE(numout,*) '             flag for eq. of state and N^2  nn_eos   = ', nn_eos
         WRITE(numout,*) '             thermal exp. coef. (linear)    rn_alpha = ', rn_alpha
         WRITE(numout,*) '             saline  exp. coef. (linear)    rn_beta  = ', rn_beta
      ENDIF
      !
      SELECT CASE( nn_eos )         ! check option
      !
      CASE( 0 )                        !==  Jackett and McDougall (1994) formulation  ==!
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '          use of Jackett & McDougall (1994) equation of state and'
         IF(lwp) WRITE(numout,*) '                 McDougall (1987) Brunt-Vaisala frequency'
         !
      CASE( 1 )                        !==  Linear formulation = F( temperature )  ==!
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '          use of linear eos rho(T) = rau0 * ( 1.0285 - rn_alpha * T )'
         IF( lk_zdfddm ) CALL ctl_stop( '          double diffusive mixing parameterization requires',   &
              &                         ' that T and S are used as state variables' )
         !
      CASE( 2 )                        !==  Linear formulation = F( temperature , salinity )  ==!
         ralpbet = rn_alpha / rn_beta
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '          use of linear eos rho(T,S) = rau0 * ( rn_beta * S - rn_alpha * T )'
         !
      CASE DEFAULT                     !==  ERROR in nn_eos  ==!
         WRITE(ctmp1,*) '          bad flag value for nn_eos = ', nn_eos
         CALL ctl_stop( ctmp1 )
         !
      END SELECT
      !
   END SUBROUTINE eos_init

   !!======================================================================
END MODULE eosbn2
