#include "xios_fortran_prefix.hpp"

MODULE IFIELD
   USE, INTRINSIC :: ISO_C_BINDING
   USE FIELD_INTERFACE
   USE FIELDGROUP_INTERFACE
!   USE IFIELD_ATTR
!   USE IFIELDGROUP_ATTR
   
   TYPE txios(field)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(field)
   
   TYPE txios(fieldgroup)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(fieldgroup)
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios(get_field_handle)(idt, ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN)   :: idt      
      TYPE(txios(field)), INTENT(OUT) :: ret
      CALL cxios_field_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE xios(get_field_handle)
   
   SUBROUTINE xios(get_fieldgroup_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *)     , INTENT(IN) :: idt      
      TYPE(txios(fieldgroup)), INTENT(OUT):: ret

      CALL cxios_fieldgroup_handle_create(ret%daddr, idt, len(idt))            

   END SUBROUTINE xios(get_fieldgroup_handle)
   

   LOGICAL FUNCTION xios(is_valid_field)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val
      
      CALL cxios_field_valid_id(val, idt, len(idt));
      xios(is_valid_field) = val

   END FUNCTION  xios(is_valid_field)

   LOGICAL FUNCTION xios(is_valid_fieldgroup)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val
      CALL cxios_fieldgroup_valid_id(val, idt, len(idt));
      xios(is_valid_fieldgroup) = val

   END FUNCTION  xios(is_valid_fieldgroup)
   
  LOGICAL FUNCTION xios(field_is_active_id(field_id))
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: field_id
      LOGICAL  (kind = 1)                 :: val
      TYPE(txios(field))                 :: field_hdl
      
      CALL xios(get_field_handle)(field_id,field_hdl)
      xios(field_is_active_id)=xios(field_is_active_hdl(field_hdl))

   END FUNCTION  xios(field_is_active_id)
   
   
   LOGICAL FUNCTION xios(field_is_active_hdl(field_hdl))
      IMPLICIT NONE
      TYPE(txios(field)),INTENT(IN)       :: field_hdl
      LOGICAL  (kind = 1)                 :: ret
      
      CALL cxios_field_is_active(field_hdl%daddr, ret);
      xios(field_is_active_hdl) = ret
      
   END FUNCTION  xios(field_is_active_hdl) 
 

END MODULE IFIELD
