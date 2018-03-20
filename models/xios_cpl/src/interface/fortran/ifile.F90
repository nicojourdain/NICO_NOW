#include "xios_fortran_prefix.hpp"

MODULE IFILE
   USE, INTRINSIC :: ISO_C_BINDING
   USE FILE_INTERFACE
   USE FILEGROUP_INTERFACE
!   USE IFILE_ATTR
!   USE IFILEGROUP_ATTR
   
   TYPE txios(file)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(file)
   
   TYPE txios(filegroup)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(filegroup)
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios(get_file_handle)( idt, ret)
      IMPLICIT NONE
      CHARACTER(len = *),   INTENT(IN) :: idt      
      TYPE(txios(file)) , INTENT(OUT):: ret

      CALL cxios_file_handle_create(ret%daddr, idt, len(idt))            

   END SUBROUTINE xios(get_file_handle)
   
   SUBROUTINE xios(get_filegroup_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *)    ,   INTENT(IN) :: idt      
      TYPE(txios(filegroup)), INTENT(OUT):: ret

      CALL cxios_filegroup_handle_create(ret%daddr, idt, len(idt))            

   END SUBROUTINE xios(get_filegroup_handle)

   LOGICAL FUNCTION xios(is_valid_file)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_file_valid_id(val, idt, len(idt));
      xios(is_valid_file) = val

   END FUNCTION  xios(is_valid_file)

   LOGICAL FUNCTION xios(is_valid_filegroup)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_filegroup_valid_id(val, idt, len(idt));
      xios(is_valid_filegroup) = val

   END FUNCTION  xios(is_valid_filegroup)

   
END MODULE IFILE
