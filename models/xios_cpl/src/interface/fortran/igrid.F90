#include "xios_fortran_prefix.hpp"

MODULE IGRID
   USE, INTRINSIC :: ISO_C_BINDING
   USE GRID_INTERFACE
   USE GRIDGROUP_INTERFACE
!   USE IGRID_ATTR
!   USE IGRIDGROUP_ATTR
   
   TYPE txios(grid)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(grid)
   
   TYPE txios(gridgroup)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(gridgroup)
   
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios(get_grid_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt      
      TYPE(txios(grid)), INTENT(OUT):: ret

      CALL cxios_grid_handle_create(ret%daddr, idt, len(idt))            

   END SUBROUTINE xios(get_grid_handle)
   
   SUBROUTINE xios(get_gridgroup_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *)     , INTENT(IN) :: idt      
      TYPE(txios(gridgroup))     , INTENT(OUT):: ret

      CALL cxios_gridgroup_handle_create(ret%daddr, idt, len(idt))            

   END SUBROUTINE xios(get_gridgroup_handle)

   LOGICAL FUNCTION xios(is_valid_grid)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_grid_valid_id(val, idt, len(idt));
      xios(is_valid_grid) = val

   END FUNCTION  xios(is_valid_grid)

   LOGICAL FUNCTION xios(is_valid_gridgroup)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_gridgroup_valid_id(val, idt, len(idt));
      xios(is_valid_gridgroup) = val

   END FUNCTION  xios(is_valid_gridgroup)

   
END MODULE IGRID
