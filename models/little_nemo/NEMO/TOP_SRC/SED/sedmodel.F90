MODULE sedmodel
#if defined key_sed
   !!======================================================================
   !!                       ***  MODULE sedmodel   ***
   !!   Sediment model : Main routine of sediment model 
   !!======================================================================
   USE sed
   USE sedini   ! sediment variables initialization
   USE sedstp   ! time stepping

   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC sed_model  ! called by step.F90

   LOGICAL, PUBLIC, PARAMETER ::   lk_sed = .TRUE.     !: sediment flag

CONTAINS

   SUBROUTINE sed_model ( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sed_model  ***
      !!
      !! ** Purpose :   main routine of sediment model
      !!
      !!
      !! ** Method  : - model general initialization
      !!              - launch the time-stepping (stp routine)
      !!
      !!   History :
      !!        !  07-02 (C. Ethe)  Original
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt       ! number of iteration


      IF( kt == nittrc000 ) CALL sed_init       ! Initialization of sediment model

                         CALL sed_stp( kt )  ! Time stepping of Sediment model


   END SUBROUTINE sed_model

#else
   !!======================================================================
   !! MODULE sedmodel  :   Dummy module 
   !!======================================================================
   LOGICAL, PUBLIC, PARAMETER ::   lk_sed = .FALSE.     !: sediment flag
CONTAINS
   SUBROUTINE sed_model( kt )         ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'sed_stp: You should not have seen this print! error?', kt
   END SUBROUTINE sed_model
#endif

END MODULE sedmodel
