! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *

MODULE grid_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING
  
  INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99
    
    
    SUBROUTINE cxios_set_grid_axis_ref(grid_hdl, axis_ref, axis_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: axis_ref
      INTEGER  (kind = C_INT)     , VALUE        :: axis_ref_size
    END SUBROUTINE cxios_set_grid_axis_ref
    
    SUBROUTINE cxios_get_grid_axis_ref(grid_hdl, axis_ref, axis_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: axis_ref
      INTEGER  (kind = C_INT)     , VALUE        :: axis_ref_size
    END SUBROUTINE cxios_get_grid_axis_ref
    
    FUNCTION cxios_is_defined_grid_axis_ref(grid_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_axis_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_axis_ref
    
    
    SUBROUTINE cxios_set_grid_description(grid_hdl, description, description_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: description
      INTEGER  (kind = C_INT)     , VALUE        :: description_size
    END SUBROUTINE cxios_set_grid_description
    
    SUBROUTINE cxios_get_grid_description(grid_hdl, description, description_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: description
      INTEGER  (kind = C_INT)     , VALUE        :: description_size
    END SUBROUTINE cxios_get_grid_description
    
    FUNCTION cxios_is_defined_grid_description(grid_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_description
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_description
    
    
    SUBROUTINE cxios_set_grid_domain_ref(grid_hdl, domain_ref, domain_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_ref
      INTEGER  (kind = C_INT)     , VALUE        :: domain_ref_size
    END SUBROUTINE cxios_set_grid_domain_ref
    
    SUBROUTINE cxios_get_grid_domain_ref(grid_hdl, domain_ref, domain_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_ref
      INTEGER  (kind = C_INT)     , VALUE        :: domain_ref_size
    END SUBROUTINE cxios_get_grid_domain_ref
    
    FUNCTION cxios_is_defined_grid_domain_ref(grid_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_domain_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_domain_ref
    
    
    SUBROUTINE cxios_set_grid_mask(grid_hdl, mask, extent1, extent2, extent3) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
      INTEGER (kind = C_INT), VALUE  :: extent3
    END SUBROUTINE cxios_set_grid_mask
    
    SUBROUTINE cxios_get_grid_mask(grid_hdl, mask, extent1, extent2, extent3) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
      INTEGER (kind = C_INT), VALUE  :: extent3
    END SUBROUTINE cxios_get_grid_mask
    
    FUNCTION cxios_is_defined_grid_mask(grid_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_mask
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_mask
    
    
    SUBROUTINE cxios_set_grid_name(grid_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_set_grid_name
    
    SUBROUTINE cxios_get_grid_name(grid_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_get_grid_name
    
    FUNCTION cxios_is_defined_grid_name(grid_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_name
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_name
    
    
    END INTERFACE
  
END MODULE grid_interface_attr
