MODULE ldfdyn
   !!======================================================================
   !!                       ***  MODULE  ldfdyn  ***
   !! Ocean physics:  lateral viscosity coefficient 
   !!=====================================================================
   !! History :  OPA  ! 1997-07  (G. Madec)  multi dimensional coefficients
   !!   NEMO     1.0  ! 2002-09  (G. Madec)  F90: Free form and module
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   ldf_dyn_init : initialization, namelist read, and parameters control
   !!   ldf_dyn_c3d   : 3D eddy viscosity coefficient initialization
   !!   ldf_dyn_c2d   : 2D eddy viscosity coefficient initialization
   !!   ldf_dyn_c1d   : 1D eddy viscosity coefficient initialization
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers   
   USE dom_oce         ! ocean space and time domain 
   USE ldfdyn_oce      ! ocean dynamics lateral physics
   USE phycst          ! physical constants
   USE ldfslp          ! ???
   USE ioipsl
   USE iom
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distribued memory computing library
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE wrk_nemo        ! Memory Allocation

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ldf_dyn_init   ! called by opa.F90

  INTERFACE ldf_zpf
     MODULE PROCEDURE ldf_zpf_1d, ldf_zpf_1d_3d, ldf_zpf_3d
  END INTERFACE

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: ldfdyn.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ldf_dyn_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ldf_dyn_init  ***
      !!                   
      !! ** Purpose :   set the horizontal ocean dynamics physics
      !!
      !! ** Method  :  
      !!      -  default option : ahm = constant coef. = rn_ahm_0 (namelist)
      !!      - 'key_dynldf_c1d': ahm = F(depth)                     see ldf_dyn_c1d.h90
      !!      - 'key_dynldf_c2d': ahm = F(latitude,longitude)        see ldf_dyn_c2d.h90
      !!      - 'key_dynldf_c3d': ahm = F(latitude,longitude,depth)  see ldf_dyn_c3d.h90
      !!
      !!      N.B. User defined include files.  By default, 3d and 2d coef.
      !!      are set to a constant value given in the namelist and the 1d
      !!      coefficients are initialized to a hyperbolic tangent vertical
      !!      profile.
      !!
      !! Reference :   Madec, G. and M. Imbard, 1996: Climate Dynamics, 12, 381-388.
      !!----------------------------------------------------------------------
      INTEGER ::   ioptio         ! ???
      LOGICAL ::   ll_print = .FALSE.    ! Logical flag for printing viscosity coef.
      !! 
      NAMELIST/namdyn_ldf/ ln_dynldf_lap  , ln_dynldf_bilap,                  &
         &                 ln_dynldf_level, ln_dynldf_hor  , ln_dynldf_iso,   &
         &                 rn_ahm_0_lap   , rn_ahmb_0      , rn_ahm_0_blp
      !!----------------------------------------------------------------------

      REWIND( numnam )                  ! Read Namelist namdyn_ldf : Lateral physics
      READ  ( numnam, namdyn_ldf )

      IF(lwp) THEN                      ! Parameter print
         WRITE(numout,*)
         WRITE(numout,*) 'ldf_dyn : lateral momentum physics'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*) '   Namelist nam_dynldf : set lateral mixing parameters'
         WRITE(numout,*) '      laplacian operator                      ln_dynldf_lap   = ', ln_dynldf_lap
         WRITE(numout,*) '      bilaplacian operator                    ln_dynldf_bilap = ', ln_dynldf_bilap
         WRITE(numout,*) '      iso-level                               ln_dynldf_level = ', ln_dynldf_level
         WRITE(numout,*) '      horizontal (geopotential)               ln_dynldf_hor   = ', ln_dynldf_hor
         WRITE(numout,*) '      iso-neutral                             ln_dynldf_iso   = ', ln_dynldf_iso
         WRITE(numout,*) '      horizontal laplacian eddy viscosity     rn_ahm_0_lap    = ', rn_ahm_0_lap
         WRITE(numout,*) '      background viscosity                    rn_ahmb_0       = ', rn_ahmb_0
         WRITE(numout,*) '      horizontal bilaplacian eddy viscosity   rn_ahm_0_blp    = ', rn_ahm_0_blp
      ENDIF

      ahm0     = rn_ahm_0_lap              ! OLD namelist variables defined from DOCTOR namelist variables
      ahmb0    = rn_ahmb_0
      ahm0_blp = rn_ahm_0_blp

      ! ... check of lateral diffusive operator on tracers
      !           ==> will be done in trazdf module

      ! ... Space variation of eddy coefficients
      ioptio = 0
#if defined key_dynldf_c3d
      IF(lwp) WRITE(numout,*) '   momentum mixing coef. = F( latitude, longitude, depth)'
      ioptio = ioptio+1
#endif
#if defined key_dynldf_c2d
      IF(lwp) WRITE(numout,*) '   momentum mixing coef. = F( latitude, longitude)'
      ioptio = ioptio+1
#endif
#if defined key_dynldf_c1d
      IF(lwp) WRITE(numout,*) '   momentum mixing coef. = F( depth )'
      ioptio = ioptio+1
      IF( ln_sco ) CALL ctl_stop( 'key_dynldf_c1d cannot be used in s-coordinate (ln_sco)' )
#endif
      IF( ioptio == 0 ) THEN
          IF(lwp) WRITE(numout,*) '   momentum mixing coef. = constant  (default option)'
        ELSEIF( ioptio > 1 ) THEN
           CALL ctl_stop( 'use only one of the following keys: key_dynldf_c3d, key_dynldf_c2d, key_dynldf_c1d' )
      ENDIF


      IF( ln_dynldf_bilap ) THEN
         IF(lwp) WRITE(numout,*) '   biharmonic momentum diffusion'
         IF( .NOT. ln_dynldf_lap ) ahm0 = ahm0_blp   ! Allow spatially varying coefs, which use ahm0 as input
         IF( ahm0_blp > 0 .AND. .NOT. lk_esopa )   CALL ctl_stop( 'The horizontal viscosity coef. ahm0 must be negative' )
      ELSE
         IF(lwp) WRITE(numout,*) '   harmonic momentum diff. (default)'
         IF( ahm0 < 0 .AND. .NOT. lk_esopa )   CALL ctl_stop( 'The horizontal viscosity coef. ahm0 must be positive' )
      ENDIF


      ! Lateral eddy viscosity
      ! ======================
#if defined key_dynldf_c3d
      CALL ldf_dyn_c3d( ll_print )   ! ahm = 3D coef. = F( longitude, latitude, depth )
#elif defined key_dynldf_c2d
      CALL ldf_dyn_c2d( ll_print )   ! ahm = 1D coef. = F( longitude, latitude )
#elif defined key_dynldf_c1d
      CALL ldf_dyn_c1d( ll_print )   ! ahm = 1D coef. = F( depth )
#else
                                     ! Constant coefficients
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'inildf: constant eddy viscosity coef. '
      IF(lwp) WRITE(numout,*) '~~~~~~'
      IF(lwp) WRITE(numout,*) '        ahm1 = ahm2 = ahm0 =  ',ahm0
#endif
      !
   END SUBROUTINE ldf_dyn_init

#if defined key_dynldf_c3d
#   include "ldfdyn_c3d.h90"
#elif defined key_dynldf_c2d
#   include "ldfdyn_c2d.h90"
#elif defined key_dynldf_c1d
#   include "ldfdyn_c1d.h90"
#endif


   SUBROUTINE ldf_zpf_1d( ld_print, pdam, pwam, pbot, pdep, pah )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ldf_zpf  ***
      !!                   
      !! ** Purpose :   vertical adimensional profile for eddy coefficient
      !!
      !! ** Method  :   1D eddy viscosity coefficients ( depth )
      !!----------------------------------------------------------------------
      LOGICAL , INTENT(in   )                 ::   ld_print   ! If true, output arrays on numout
      REAL(wp), INTENT(in   )                 ::   pdam       ! depth of the inflection point
      REAL(wp), INTENT(in   )                 ::   pwam       ! width of inflection
      REAL(wp), INTENT(in   )                 ::   pbot       ! bottom value (0<pbot<= 1)
      REAL(wp), INTENT(in   ), DIMENSION(jpk) ::   pdep       ! depth of the gridpoint (T, U, V, F)
      REAL(wp), INTENT(inout), DIMENSION(jpk) ::   pah        ! adimensional vertical profile
      !!
      INTEGER  ::   jk           ! dummy loop indices
      REAL(wp) ::   zm00, zm01, zmhb, zmhs       ! temporary scalars
      !!----------------------------------------------------------------------

      zm00 = TANH( ( pdam - gdept_0(1    ) ) / pwam )
      zm01 = TANH( ( pdam - gdept_0(jpkm1) ) / pwam )
      zmhs = zm00 / zm01
      zmhb = ( 1.e0 - pbot ) / ( 1.e0 - zmhs ) / zm01

      DO jk = 1, jpk
         pah(jk) = 1.e0 + zmhb * ( zm00 - TANH( ( pdam - pdep(jk) ) / pwam )  )
      END DO

      IF(lwp .AND. ld_print ) THEN      ! Control print
         WRITE(numout,*)
         WRITE(numout,*) '         ahm profile : '
         WRITE(numout,*)
         WRITE(numout,'("  jk      ahm       ","  depth t-level " )')
         DO jk = 1, jpk
            WRITE(numout,'(i6,2f12.4,3x,2f12.4)') jk, pah(jk), pdep(jk)
         END DO
      ENDIF
      !
   END SUBROUTINE ldf_zpf_1d


   SUBROUTINE ldf_zpf_1d_3d( ld_print, pdam, pwam, pbot, pdep, pah )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ldf_zpf  ***
      !!
      !! ** Purpose :   vertical adimensional profile for eddy coefficient
      !!
      !! ** Method  :   1D eddy viscosity coefficients ( depth )
      !!----------------------------------------------------------------------
      LOGICAL , INTENT(in   )                         ::   ld_print   ! If true, output arrays on numout
      REAL(wp), INTENT(in   )                         ::   pdam       ! depth of the inflection point
      REAL(wp), INTENT(in   )                         ::   pwam       ! width of inflection
      REAL(wp), INTENT(in   )                         ::   pbot       ! bottom value (0<pbot<= 1)
      REAL(wp), INTENT(in   ), DIMENSION          (:) ::   pdep       ! depth of the gridpoint (T, U, V, F)
      REAL(wp), INTENT(inout), DIMENSION      (:,:,:) ::   pah        ! adimensional vertical profile
      !!
      INTEGER  ::   jk           ! dummy loop indices
      REAL(wp) ::   zm00, zm01, zmhb, zmhs, zcf  ! temporary scalars
      !!----------------------------------------------------------------------

      zm00 = TANH( ( pdam - gdept_0(1    ) ) / pwam )
      zm01 = TANH( ( pdam - gdept_0(jpkm1) ) / pwam )
      zmhs = zm00 / zm01
      zmhb = ( 1.e0 - pbot ) / ( 1.e0 - zmhs ) / zm01

      DO jk = 1, jpk
         zcf = 1.e0 + zmhb * ( zm00 - TANH( ( pdam - pdep(jk) ) / pwam )  )
         pah(:,:,jk) = zcf
      END DO

      IF(lwp .AND. ld_print ) THEN      ! Control print
         WRITE(numout,*)
         WRITE(numout,*) '         ahm profile : '
         WRITE(numout,*)
         WRITE(numout,'("  jk      ahm       ","  depth t-level " )')
         DO jk = 1, jpk
            WRITE(numout,'(i6,2f12.4,3x,2f12.4)') jk, pah(1,1,jk), pdep(jk)
         END DO
      ENDIF
      !
   END SUBROUTINE ldf_zpf_1d_3d


   SUBROUTINE ldf_zpf_3d( ld_print, pdam, pwam, pbot, pdep, pah )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ldf_zpf  ***
      !!
      !! ** Purpose :   vertical adimensional profile for eddy coefficient
      !!
      !! ** Method  :   3D for partial step or s-coordinate
      !!----------------------------------------------------------------------
      LOGICAL , INTENT(in   )                         ::   ld_print   ! If true, output arrays on numout
      REAL(wp), INTENT(in   )                         ::   pdam       ! depth of the inflection point
      REAL(wp), INTENT(in   )                         ::   pwam       ! width of inflection
      REAL(wp), INTENT(in   )                         ::   pbot       ! bottom value (0<pbot<= 1)
      REAL(wp), INTENT(in   ), DIMENSION      (:,:,:) ::   pdep       ! dep of the gridpoint (T, U, V, F)
      REAL(wp), INTENT(inout), DIMENSION      (:,:,:) ::   pah        ! adimensional vertical profile
      !!
      INTEGER  ::   jk           ! dummy loop indices
      REAL(wp) ::   zm00, zm01, zmhb, zmhs       ! temporary scalars
      !!----------------------------------------------------------------------

      zm00 = TANH( ( pdam - gdept_0(1    ) ) / pwam )   
      zm01 = TANH( ( pdam - gdept_0(jpkm1) ) / pwam )
      zmhs = zm00 / zm01
      zmhb = ( 1.e0 - pbot ) / ( 1.e0 - zmhs ) / zm01

      DO jk = 1, jpk
         pah(:,:,jk) = 1.e0 + zmhb * ( zm00 - TANH( ( pdam - pdep(:,:,jk) ) / pwam )  )
      END DO

      IF(lwp .AND. ld_print ) THEN      ! Control print
         WRITE(numout,*)
         WRITE(numout,*) '         ahm profile : '
         WRITE(numout,*)
         WRITE(numout,'("  jk      ahm       ","  depth t-level " )')
         DO jk = 1, jpk
            WRITE(numout,'(i6,2f12.4,3x,2f12.4)') jk, pah(1,1,jk), pdep(1,1,jk)
         END DO
      ENDIF
      !
   END SUBROUTINE ldf_zpf_3d

   !!======================================================================
END MODULE ldfdyn
