#include "xios_fortran_prefix.hpp"

MODULE IXML_TREE
   USE, INTRINSIC :: ISO_C_BINDING
   USE IAXIS
   USE IFILE
   USE IFIELD
   USE IGRID
   USE IDOMAIN
     
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
     
      SUBROUTINE cxios_xml_tree_add_field(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_field
      
      SUBROUTINE cxios_xml_tree_add_grid(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_grid
      
      SUBROUTINE cxios_xml_tree_add_file(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_file
      
      SUBROUTINE cxios_xml_tree_add_axis(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_axis
      
      SUBROUTINE cxios_xml_tree_add_domain(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_domain
      
      SUBROUTINE cxios_xml_tree_add_fieldtofile(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_fieldtofile

      SUBROUTINE cxios_xml_tree_add_fieldgroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_fieldgroup

      SUBROUTINE cxios_xml_tree_add_gridgroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_gridgroup

      SUBROUTINE cxios_xml_tree_add_filegroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_filegroup

      SUBROUTINE cxios_xml_tree_add_axisgroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_axisgroup

      SUBROUTINE cxios_xml_tree_add_domaingroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_domaingroup
      
      SUBROUTINE cxios_xml_tree_add_fieldgrouptofile(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_fieldgrouptofile    

      SUBROUTINE cxios_xml_tree_show(filename, filename_size) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR), DIMENSION(*) :: filename
         INTEGER  (kind = C_INT) , VALUE        :: filename_size
      END SUBROUTINE cxios_xml_tree_show

      SUBROUTINE cxios_xml_parse_file(filename, filename_size) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR), DIMENSION(*) :: filename
         INTEGER  (kind = C_INT) , VALUE        :: filename_size
      END SUBROUTINE cxios_xml_parse_file

      SUBROUTINE cxios_xml_parse_string(xmlcontent, xmlcontent_size) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR), DIMENSION(*) :: xmlcontent
         INTEGER  (kind = C_INT) , VALUE        :: xmlcontent_size
      END SUBROUTINE cxios_xml_parse_string
     
   END INTERFACE

   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.


   SUBROUTINE xios(add_axis)(parent_hdl, child_hdl, child_id)
      TYPE(txios(axisgroup))     , INTENT(IN) :: parent_hdl
      TYPE(txios(axis))          , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_axis(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_axis(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios(add_axis)
   
   SUBROUTINE xios(add_file)(parent_hdl, child_hdl, child_id)
      TYPE(txios(filegroup))      , INTENT(IN) :: parent_hdl
      TYPE(txios(file))           , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN)  :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_file(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_file(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios(add_file)
   
   SUBROUTINE xios(add_grid)(parent_hdl, child_hdl, child_id)
      TYPE(txios(gridgroup))     , INTENT(IN) :: parent_hdl
      TYPE(txios(grid))          , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_grid(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_grid(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios(add_grid)
   
   
   SUBROUTINE xios(add_field)(parent_hdl, child_hdl, child_id)
      TYPE(txios(fieldgroup))     , INTENT(IN) :: parent_hdl
      TYPE(txios(field))          , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_field(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_field(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios(add_field)
   
   
   SUBROUTINE xios(add_domain)(parent_hdl, child_hdl, child_id)
      TYPE(txios(domaingroup))     , INTENT(IN) :: parent_hdl
      TYPE(txios(domain))     , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_domain(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_domain(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios(add_domain)
   
   SUBROUTINE xios(add_fieldtofile)(parent_hdl, child_hdl, child_id)
      TYPE(txios(file))            , INTENT(IN) :: parent_hdl
      TYPE(txios(field))           , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_fieldtofile(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_fieldtofile(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios(add_fieldtofile)


   SUBROUTINE xios(add_axisgroup)(parent_hdl, child_hdl, child_id)
      TYPE(txios(axisgroup))      , INTENT(IN) :: parent_hdl
      TYPE(txios(axisgroup))      , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_axisgroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_axisgroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios(add_axisgroup)


   SUBROUTINE xios(add_filegroup)(parent_hdl, child_hdl, child_id)
      TYPE(txios(filegroup))      , INTENT(IN) :: parent_hdl
      TYPE(txios(filegroup))      , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_filegroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_filegroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios(add_filegroup)

   SUBROUTINE xios(add_gridgroup)(parent_hdl, child_hdl, child_id)
      TYPE(txios(gridgroup))      , INTENT(IN) :: parent_hdl
      TYPE(txios(gridgroup))      , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_gridgroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_gridgroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
      
   END SUBROUTINE xios(add_gridgroup)


   SUBROUTINE xios(add_fieldgroup)(parent_hdl, child_hdl, child_id)
      TYPE(txios(fieldgroup))     , INTENT(IN) :: parent_hdl
      TYPE(txios(fieldgroup))     , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_fieldgroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_fieldgroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xios(add_fieldgroup)

   SUBROUTINE xios(add_domaingroup)(parent_hdl, child_hdl, child_id)
      TYPE(txios(domaingroup))     , INTENT(IN) :: parent_hdl
      TYPE(txios(domaingroup))     , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_domaingroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_domaingroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios(add_domaingroup)

   SUBROUTINE xios(add_fieldgrouptofile)(parent_hdl, child_hdl, child_id)
      TYPE(txios(file))            , INTENT(IN) :: parent_hdl
      TYPE(txios(fieldgroup))     , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL  , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_fieldgrouptofile(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_fieldgrouptofile(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios(add_fieldgrouptofile)
      
END MODULE IXML_TREE
