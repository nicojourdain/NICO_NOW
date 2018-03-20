#include "xios_fortran_prefix.hpp"

MODULE ICONTEXT
   USE, INTRINSIC :: ISO_C_BINDING
   USE CONTEXT_INTERFACE
!   USE icontext_attr
   USE IDATE

    
   TYPE txios(context)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(context)
      
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.
   
   SUBROUTINE xios(get_context_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *)  , INTENT(IN)  :: idt      
      TYPE(txios(context)), INTENT(OUT):: ret

      CALL cxios_context_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE xios(get_context_handle)
   
   SUBROUTINE xios(set_current_context)(context, withswap)
      IMPLICIT NONE

      TYPE(txios(context))          , INTENT(IN) :: context
      LOGICAL             , OPTIONAL, INTENT(IN) :: withswap
      LOGICAL (kind = 1)                       :: wswap

      IF (PRESENT(withswap)) THEN
         wswap = withswap
      ELSE
         wswap = .FALSE.
      END IF
      CALL cxios_context_set_current(context%daddr, wswap)

   END SUBROUTINE xios(set_current_context)
 
   LOGICAL FUNCTION xios(is_valid_context)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_context_valid_id(val, idt, len(idt));
      xios(is_valid_context) = val

   END FUNCTION  xios(is_valid_context)

   
END MODULE ICONTEXT
