! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *

MODULE gridgroup_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING
  
  INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99
    
    
    SUBROUTINE cxios_set_gridgroup_axis_ref(gridgroup_hdl, axis_ref, axis_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: axis_ref
      INTEGER  (kind = C_INT)     , VALUE        :: axis_ref_size
    END SUBROUTINE cxios_set_gridgroup_axis_ref
    
    SUBROUTINE cxios_get_gridgroup_axis_ref(gridgroup_hdl, axis_ref, axis_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: axis_ref
      INTEGER  (kind = C_INT)     , VALUE        :: axis_ref_size
    END SUBROUTINE cxios_get_gridgroup_axis_ref
    
    FUNCTION cxios_is_defined_gridgroup_axis_ref(gridgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_gridgroup_axis_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
    END FUNCTION cxios_is_defined_gridgroup_axis_ref
    
    
    SUBROUTINE cxios_set_gridgroup_description(gridgroup_hdl, description, description_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: description
      INTEGER  (kind = C_INT)     , VALUE        :: description_size
    END SUBROUTINE cxios_set_gridgroup_description
    
    SUBROUTINE cxios_get_gridgroup_description(gridgroup_hdl, description, description_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: description
      INTEGER  (kind = C_INT)     , VALUE        :: description_size
    END SUBROUTINE cxios_get_gridgroup_description
    
    FUNCTION cxios_is_defined_gridgroup_description(gridgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_gridgroup_description
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
    END FUNCTION cxios_is_defined_gridgroup_description
    
    
    SUBROUTINE cxios_set_gridgroup_domain_ref(gridgroup_hdl, domain_ref, domain_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_ref
      INTEGER  (kind = C_INT)     , VALUE        :: domain_ref_size
    END SUBROUTINE cxios_set_gridgroup_domain_ref
    
    SUBROUTINE cxios_get_gridgroup_domain_ref(gridgroup_hdl, domain_ref, domain_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_ref
      INTEGER  (kind = C_INT)     , VALUE        :: domain_ref_size
    END SUBROUTINE cxios_get_gridgroup_domain_ref
    
    FUNCTION cxios_is_defined_gridgroup_domain_ref(gridgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_gridgroup_domain_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
    END FUNCTION cxios_is_defined_gridgroup_domain_ref
    
    
    SUBROUTINE cxios_set_gridgroup_group_ref(gridgroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_set_gridgroup_group_ref
    
    SUBROUTINE cxios_get_gridgroup_group_ref(gridgroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_get_gridgroup_group_ref
    
    FUNCTION cxios_is_defined_gridgroup_group_ref(gridgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_gridgroup_group_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
    END FUNCTION cxios_is_defined_gridgroup_group_ref
    
    
    SUBROUTINE cxios_set_gridgroup_mask(gridgroup_hdl, mask, extent1, extent2, extent3) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: gridgroup_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
      INTEGER (kind = C_INT), VALUE  :: extent3
    END SUBROUTINE cxios_set_gridgroup_mask
    
    SUBROUTINE cxios_get_gridgroup_mask(gridgroup_hdl, mask, extent1, extent2, extent3) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: gridgroup_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
      INTEGER (kind = C_INT), VALUE  :: extent3
    END SUBROUTINE cxios_get_gridgroup_mask
    
    FUNCTION cxios_is_defined_gridgroup_mask(gridgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_gridgroup_mask
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
    END FUNCTION cxios_is_defined_gridgroup_mask
    
    
    SUBROUTINE cxios_set_gridgroup_name(gridgroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_set_gridgroup_name
    
    SUBROUTINE cxios_get_gridgroup_name(gridgroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_get_gridgroup_name
    
    FUNCTION cxios_is_defined_gridgroup_name(gridgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_gridgroup_name
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
    END FUNCTION cxios_is_defined_gridgroup_name
    
    
    END INTERFACE
  
END MODULE gridgroup_interface_attr
