MODULE trabbc
   !!==============================================================================
   !!                       ***  MODULE  trabbc  ***
   !! Ocean active tracers:  bottom boundary condition (geothermal heat flux)
   !!==============================================================================
   !! History :  OPA  ! 1999-10 (G. Madec)  original code
   !!   NEMO     1.0  ! 2002-08 (G. Madec)  free form + modules
   !!             -   ! 2002-11 (A. Bozec)  tra_bbc_init: original code
   !!            3.3  ! 2010-10 (G. Madec)  dynamical allocation + suppression of key_trabbc
   !!             -   ! 2010-11 (G. Madec)  use mbkt array (deepest ocean t-level)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_bbc      : update the tracer trend at ocean bottom 
   !!   tra_bbc_init : initialization of geothermal heat flux trend
   !!----------------------------------------------------------------------
   USE oce             ! ocean variables
   USE dom_oce         ! domain: ocean
   USE phycst          ! physical constants
   USE trdmod_oce      ! trends: ocean variables 
   USE trdtra          ! trends: active tracers 
   USE in_out_manager  ! I/O manager
   USE prtctl          ! Print control
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC tra_bbc          ! routine called by step.F90
   PUBLIC tra_bbc_init     ! routine called by opa.F90

   !                                                !!* Namelist nambbc: bottom boundary condition *
   LOGICAL, PUBLIC ::   ln_trabbc     = .FALSE.      !: Geothermal heat flux flag
   INTEGER         ::   nn_geoflx     = 1            !  Geothermal flux (=1:constant flux, =2:read in file )
   REAL(wp)        ::   rn_geoflx_cst = 86.4e-3_wp   !  Constant value of geothermal heat flux

   REAL(wp), PUBLIC, DIMENSION(:,:), ALLOCATABLE ::   qgh_trd0   ! geothermal heating trend
 
   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_bbc( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_bbc  ***
      !!
      !! ** Purpose :   Compute the bottom boundary contition on temperature 
      !!              associated with geothermal heating and add it to the 
      !!              general trend of temperature equations.
      !!
      !! ** Method  :   The geothermal heat flux set to its constant value of 
      !!              86.4 mW/m2 (Stein and Stein 1992, Huang 1999).
      !!       The temperature trend associated to this heat flux through the
      !!       ocean bottom can be computed once and is added to the temperature
      !!       trend juste above the bottom at each time step:
      !!            ta = ta + Qsf / (rau0 rcp e3T) for k= mbkt
      !!       Where Qsf is the geothermal heat flux.
      !!
      !! ** Action  : - update the temperature trends (ta) with the trend of
      !!                the ocean bottom boundary condition
      !!
      !! References : Stein, C. A., and S. Stein, 1992, Nature, 359, 123-129.
      !!              Emile-Geay and Madec, 2009, Ocean Science.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !!
      INTEGER  ::   ji, jj, ik    ! dummy loop indices
      REAL(wp) ::   zqgh_trd      ! geothermal heat flux trend
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ztrdt
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('tra_bbc')
      !
      IF( l_trdtra )   THEN         ! Save ta and sa trends
         CALL wrk_alloc( jpi, jpj, jpk, ztrdt )
         ztrdt(:,:,:) = tsa(:,:,:,jp_tem)
      ENDIF
      !
      !                             !  Add the geothermal heat flux trend on temperature
#if defined key_vectopt_loop
      DO jj = 1, 1
         DO ji = jpi+2, jpij-jpi-1   ! vector opt. (forced unrolling)
#else
      DO jj = 2, jpjm1
         DO ji = 2, jpim1
#endif
            ik = mbkt(ji,jj)
            zqgh_trd = qgh_trd0(ji,jj) / fse3t(ji,jj,ik)
            tsa(ji,jj,ik,jp_tem) = tsa(ji,jj,ik,jp_tem) + zqgh_trd
         END DO
      END DO
      !
      IF( l_trdtra ) THEN        ! Save the geothermal heat flux trend for diagnostics
         ztrdt(:,:,:) = tsa(:,:,:,jp_tem) - ztrdt(:,:,:)
         CALL trd_tra( kt, 'TRA', jp_tem, jptra_trd_bbc, ztrdt )
         CALL wrk_dealloc( jpi, jpj, jpk, ztrdt )
      ENDIF
      !
      IF(ln_ctl)   CALL prt_ctl( tab3d_1=tsa(:,:,:,jp_tem), clinfo1=' bbc  - Ta: ', mask1=tmask, clinfo3='tra-ta' )
      !
      IF( nn_timing == 1 )  CALL timing_stop('tra_bbc')
      !
   END SUBROUTINE tra_bbc


   SUBROUTINE tra_bbc_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_bbc_init  ***
      !!
      !! ** Purpose :   Compute once for all the trend associated with geothermal
      !!              heating that will be applied at each time step at the
      !!              last ocean level
      !!
      !! ** Method  :   Read the nambbc namelist and check the parameters.
      !!
      !! ** Input   : - Namlist nambbc
      !!              - NetCDF file  : geothermal_heating.nc ( if necessary )
      !!
      !! ** Action  : - read/fix the geothermal heat qgh_trd0
      !!----------------------------------------------------------------------
      USE iom
      !!
      INTEGER  ::   ji, jj              ! dummy loop indices
      INTEGER  ::   inum                ! temporary logical unit
      !!
      NAMELIST/nambbc/ln_trabbc, nn_geoflx, rn_geoflx_cst 
      !!----------------------------------------------------------------------

      REWIND( numnam )                 ! Read Namelist nambbc : bottom momentum boundary condition
      READ  ( numnam, nambbc )

      IF(lwp) THEN                     ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'tra_bbc : Bottom Boundary Condition (bbc), apply a Geothermal heating'
         WRITE(numout,*) '~~~~~~~   '
         WRITE(numout,*) '   Namelist nambbc : set bbc parameters'
         WRITE(numout,*) '      Apply a geothermal heating at ocean bottom   ln_trabbc     = ', ln_trabbc
         WRITE(numout,*) '      type of geothermal flux                      nn_geoflx     = ', nn_geoflx
         WRITE(numout,*) '      Constant geothermal flux value               rn_geoflx_cst = ', rn_geoflx_cst
         WRITE(numout,*)
      ENDIF

      IF( ln_trabbc ) THEN             !==  geothermal heating  ==!
         !
         ALLOCATE( qgh_trd0(jpi,jpj) )    ! allocation
         !
         SELECT CASE ( nn_geoflx )        ! geothermal heat flux / (rauO * Cp)
         !
         CASE ( 1 )                          !* constant flux
            IF(lwp) WRITE(numout,*) '      *** constant heat flux  =   ', rn_geoflx_cst
            qgh_trd0(:,:) = ro0cpr * rn_geoflx_cst
            !
         CASE ( 2 )                          !* variable geothermal heat flux : read the geothermal fluxes in mW/m2
            IF(lwp) WRITE(numout,*) '      *** variable geothermal heat flux'
            CALL iom_open ( 'geothermal_heating.nc', inum )
            CALL iom_get  ( inum, jpdom_data, 'heatflow', qgh_trd0 )
            CALL iom_close( inum )
            qgh_trd0(:,:) = ro0cpr * qgh_trd0(:,:) * 1.e-3     ! conversion in W/m2
            !
         CASE DEFAULT
            WRITE(ctmp1,*) '     bad flag value for nn_geoflx = ', nn_geoflx
            CALL ctl_stop( ctmp1 )
            !
         END SELECT
         !
      ELSE
         IF(lwp) WRITE(numout,*) '      *** no geothermal heat flux'
      ENDIF
      !
   END SUBROUTINE tra_bbc_init

   !!======================================================================
END MODULE trabbc
