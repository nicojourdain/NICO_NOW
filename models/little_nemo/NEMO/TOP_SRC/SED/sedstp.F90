MODULE sedstp
#if defined key_sed
   !!======================================================================
   !!                       ***  MODULE sedstp   ***
   !!   Sediment model : Sediment model time-stepping
   !!======================================================================
   USE sed      ! sediment global variables
   USE seddta   ! data read
   USE sedchem  ! chemical constant
   USE sedco3   ! carbonate in sediment pore water
   USE seddsr   ! dissolution reaction
   USE sedbtb   ! bioturbation
   USE sedadv   ! vertical advection
   USE sedmbc   ! mass balance calculation
   USE sedsfc   ! sediment surface data
   USE sedrst   ! restart
   USE sedwri   ! outputs

   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC sed_stp  ! called by step.F90

CONTAINS

   SUBROUTINE sed_stp ( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sed_stp  ***
      !!
      !! ** Purpose :   Sediment time stepping
      !!                Simulation of pore water chemistry
      !!
      !! ** Action  :
      !!
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) coupled with PISCES
      !!        !  06-04 (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt       ! number of iteration

      !!----------------------------------------------------------------------

      IF( kt /= nitsed000 )  THEN
        CALL sed_dta( kt )       ! Load  Data for bot. wat. Chem and fluxes
        CALL sed_chem( kt )      ! update of chemical constant to account for salinity, temperature changes
      ENDIF
      CALL sed_dsr( kt )         ! Dissolution reaction
      CALL sed_adv( kt )         ! advection
      CALL sed_btb( kt )         ! Bioturbation

      IF ( ( MOD( kt, nwrised ) == 0 ) .OR. ( MOD( kt, nstock ) == 0 ) .OR. ( kt == nitsedend )  )   &
      CALL sed_co3( kt )         ! pH actualization for saving
      CALL sed_mbc( kt )         ! cumulation for mass balance calculation
#if ! defined key_sed_off
      CALL sed_sfc( kt )         ! Give back new bottom wat chem to tracer model
#endif
      CALL sed_rst_wri( kt )   ! restart file output
      CALL sed_wri( kt )         ! outputs

      IF( kt == nitsedend )  CLOSE( numsed )

   END SUBROUTINE sed_stp

#else
   !!======================================================================
   !! MODULE sedstp  :   Dummy module 
   !!======================================================================
CONTAINS
   SUBROUTINE sed_stp( kt )         ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'sed_stp: You should not have seen this print! error?', kt
   END SUBROUTINE sed_stp
#endif
END MODULE sedstp
