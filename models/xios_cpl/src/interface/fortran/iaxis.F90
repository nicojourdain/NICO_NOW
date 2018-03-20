#include "xios_fortran_prefix.hpp"

MODULE IAXIS
   USE, INTRINSIC :: ISO_C_BINDING
   USE AXIS_INTERFACE
   USE AXISGROUP_INTERFACE
   
   TYPE txios(axis)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(axis)
   
   TYPE txios(axisgroup)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(axisgroup)
   

         
   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios(get_axis_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt      
      TYPE(txios(axis)) , INTENT(OUT):: ret
      CALL cxios_axis_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE xios(get_axis_handle)
   
   SUBROUTINE xios(get_axisgroup_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *)    , INTENT(IN) :: idt      
      TYPE(txios(axisgroup)), INTENT(OUT):: ret

      CALL cxios_axisgroup_handle_create(ret%daddr, idt, len(idt))            

   END SUBROUTINE xios(get_axisgroup_handle)

   LOGICAL FUNCTION xios(is_valid_axis)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val
      
      CALL cxios_axis_valid_id(val, idt, len(idt))
      xios(is_valid_axis) = val

   END FUNCTION  xios(is_valid_axis)

   LOGICAL FUNCTION xios(is_valid_axisgroup)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_axisgroup_valid_id(val, idt, len(idt))
      xios(is_valid_axisgroup) = val

   END FUNCTION  xios(is_valid_axisgroup)

END MODULE IAXIS
