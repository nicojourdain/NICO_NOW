#include "xios_fortran_prefix.hpp"
MODULE IDATE
   USE, INTRINSIC :: ISO_C_BINDING

   ! enum XCalendarType
   INTEGER(kind = C_INT), PARAMETER :: D360 = 0 , ALLLEAP = 1 , NOLEAP = 2 , JULIAN = 3 , GREGORIAN = 4

   TYPE txios(date)
      INTEGER :: year, month, day, hour, minute, second
   END TYPE txios(date)

   TYPE txios(time)
      REAL(kind = 8) :: year=0, month=0, day=0, hour=0, minute=0, second=0
   END TYPE txios(time)   

   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
   
      SUBROUTINE cxios_set_timestep(ts_year, ts_month, ts_day, ts_hour, ts_minute, ts_second) BIND(C)
         IMPORT C_DOUBLE
         REAL (kind = C_DOUBLE), VALUE :: ts_year, ts_month , ts_day   , &
                                          ts_hour, ts_minute, ts_second
      END SUBROUTINE cxios_set_timestep

      SUBROUTINE cxios_update_calendar(step) BIND(C)
         IMPORT C_INT
         INTEGER (kind = C_INT), VALUE :: step
      END SUBROUTINE cxios_update_calendar
      
   END INTERFACE
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.


   SUBROUTINE xios(set_timestep)(timestep)
      IMPLICIT NONE
      TYPE(txios(time)), INTENT(IN):: timestep

      CALL cxios_set_timestep(timestep%year, timestep%month , timestep%day, &
                             timestep%hour, timestep%minute, timestep%second)

   END SUBROUTINE xios(set_timestep)


   
   SUBROUTINE xios(update_calendar)(step)
     IMPLICIT NONE
     INTEGER, INTENT(IN):: step
      
      IF (step < 0) THEN
         PRINT *, "L'argument 'step' ne peut être négatif"
         STOP
      END IF
      CALL cxios_update_calendar(step)
      
   END SUBROUTINE xios(update_calendar)

   
END MODULE IDATE
