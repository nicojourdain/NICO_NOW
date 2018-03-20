! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *

MODULE field_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING
  
  INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99
    
    
    SUBROUTINE cxios_set_field_axis_ref(field_hdl, axis_ref, axis_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: axis_ref
      INTEGER  (kind = C_INT)     , VALUE        :: axis_ref_size
    END SUBROUTINE cxios_set_field_axis_ref
    
    SUBROUTINE cxios_get_field_axis_ref(field_hdl, axis_ref, axis_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: axis_ref
      INTEGER  (kind = C_INT)     , VALUE        :: axis_ref_size
    END SUBROUTINE cxios_get_field_axis_ref
    
    FUNCTION cxios_is_defined_field_axis_ref(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_axis_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_axis_ref
    
    
    SUBROUTINE cxios_set_field_default_value(field_hdl, default_value) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      REAL (KIND=C_DOUBLE)      , VALUE :: default_value
    END SUBROUTINE cxios_set_field_default_value
    
    SUBROUTINE cxios_get_field_default_value(field_hdl, default_value) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      REAL (KIND=C_DOUBLE)             :: default_value
    END SUBROUTINE cxios_get_field_default_value
    
    FUNCTION cxios_is_defined_field_default_value(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_default_value
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_default_value
    
    
    SUBROUTINE cxios_set_field_domain_ref(field_hdl, domain_ref, domain_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_ref
      INTEGER  (kind = C_INT)     , VALUE        :: domain_ref_size
    END SUBROUTINE cxios_set_field_domain_ref
    
    SUBROUTINE cxios_get_field_domain_ref(field_hdl, domain_ref, domain_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_ref
      INTEGER  (kind = C_INT)     , VALUE        :: domain_ref_size
    END SUBROUTINE cxios_get_field_domain_ref
    
    FUNCTION cxios_is_defined_field_domain_ref(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_domain_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_domain_ref
    
    
    SUBROUTINE cxios_set_field_enabled(field_hdl, enabled) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      LOGICAL (KIND=C_BOOL)      , VALUE :: enabled
    END SUBROUTINE cxios_set_field_enabled
    
    SUBROUTINE cxios_get_field_enabled(field_hdl, enabled) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      LOGICAL (KIND=C_BOOL)             :: enabled
    END SUBROUTINE cxios_get_field_enabled
    
    FUNCTION cxios_is_defined_field_enabled(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_enabled
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_enabled
    
    
    SUBROUTINE cxios_set_field_field_ref(field_hdl, field_ref, field_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: field_ref
      INTEGER  (kind = C_INT)     , VALUE        :: field_ref_size
    END SUBROUTINE cxios_set_field_field_ref
    
    SUBROUTINE cxios_get_field_field_ref(field_hdl, field_ref, field_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: field_ref
      INTEGER  (kind = C_INT)     , VALUE        :: field_ref_size
    END SUBROUTINE cxios_get_field_field_ref
    
    FUNCTION cxios_is_defined_field_field_ref(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_field_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_field_ref
    
    
    SUBROUTINE cxios_set_field_freq_offset(field_hdl, freq_offset, freq_offset_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: freq_offset
      INTEGER  (kind = C_INT)     , VALUE        :: freq_offset_size
    END SUBROUTINE cxios_set_field_freq_offset
    
    SUBROUTINE cxios_get_field_freq_offset(field_hdl, freq_offset, freq_offset_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: freq_offset
      INTEGER  (kind = C_INT)     , VALUE        :: freq_offset_size
    END SUBROUTINE cxios_get_field_freq_offset
    
    FUNCTION cxios_is_defined_field_freq_offset(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_freq_offset
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_freq_offset
    
    
    SUBROUTINE cxios_set_field_freq_op(field_hdl, freq_op, freq_op_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: freq_op
      INTEGER  (kind = C_INT)     , VALUE        :: freq_op_size
    END SUBROUTINE cxios_set_field_freq_op
    
    SUBROUTINE cxios_get_field_freq_op(field_hdl, freq_op, freq_op_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: freq_op
      INTEGER  (kind = C_INT)     , VALUE        :: freq_op_size
    END SUBROUTINE cxios_get_field_freq_op
    
    FUNCTION cxios_is_defined_field_freq_op(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_freq_op
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_freq_op
    
    
    SUBROUTINE cxios_set_field_grid_ref(field_hdl, grid_ref, grid_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: grid_ref
      INTEGER  (kind = C_INT)     , VALUE        :: grid_ref_size
    END SUBROUTINE cxios_set_field_grid_ref
    
    SUBROUTINE cxios_get_field_grid_ref(field_hdl, grid_ref, grid_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: grid_ref
      INTEGER  (kind = C_INT)     , VALUE        :: grid_ref_size
    END SUBROUTINE cxios_get_field_grid_ref
    
    FUNCTION cxios_is_defined_field_grid_ref(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_grid_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_grid_ref
    
    
    SUBROUTINE cxios_set_field_level(field_hdl, level) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      INTEGER (KIND=C_INT)      , VALUE :: level
    END SUBROUTINE cxios_set_field_level
    
    SUBROUTINE cxios_get_field_level(field_hdl, level) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      INTEGER (KIND=C_INT)             :: level
    END SUBROUTINE cxios_get_field_level
    
    FUNCTION cxios_is_defined_field_level(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_level
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_level
    
    
    SUBROUTINE cxios_set_field_long_name(field_hdl, long_name, long_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
      INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
    END SUBROUTINE cxios_set_field_long_name
    
    SUBROUTINE cxios_get_field_long_name(field_hdl, long_name, long_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
      INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
    END SUBROUTINE cxios_get_field_long_name
    
    FUNCTION cxios_is_defined_field_long_name(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_long_name
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_long_name
    
    
    SUBROUTINE cxios_set_field_name(field_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_set_field_name
    
    SUBROUTINE cxios_get_field_name(field_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_get_field_name
    
    FUNCTION cxios_is_defined_field_name(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_name
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_name
    
    
    SUBROUTINE cxios_set_field_operation(field_hdl, operation, operation_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: operation
      INTEGER  (kind = C_INT)     , VALUE        :: operation_size
    END SUBROUTINE cxios_set_field_operation
    
    SUBROUTINE cxios_get_field_operation(field_hdl, operation, operation_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: operation
      INTEGER  (kind = C_INT)     , VALUE        :: operation_size
    END SUBROUTINE cxios_get_field_operation
    
    FUNCTION cxios_is_defined_field_operation(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_operation
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_operation
    
    
    SUBROUTINE cxios_set_field_prec(field_hdl, prec) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      INTEGER (KIND=C_INT)      , VALUE :: prec
    END SUBROUTINE cxios_set_field_prec
    
    SUBROUTINE cxios_get_field_prec(field_hdl, prec) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      INTEGER (KIND=C_INT)             :: prec
    END SUBROUTINE cxios_get_field_prec
    
    FUNCTION cxios_is_defined_field_prec(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_prec
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_prec
    
    
    SUBROUTINE cxios_set_field_standard_name(field_hdl, standard_name, standard_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
      INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
    END SUBROUTINE cxios_set_field_standard_name
    
    SUBROUTINE cxios_get_field_standard_name(field_hdl, standard_name, standard_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
      INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
    END SUBROUTINE cxios_get_field_standard_name
    
    FUNCTION cxios_is_defined_field_standard_name(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_standard_name
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_standard_name
    
    
    SUBROUTINE cxios_set_field_unit(field_hdl, unit, unit_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: unit
      INTEGER  (kind = C_INT)     , VALUE        :: unit_size
    END SUBROUTINE cxios_set_field_unit
    
    SUBROUTINE cxios_get_field_unit(field_hdl, unit, unit_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: unit
      INTEGER  (kind = C_INT)     , VALUE        :: unit_size
    END SUBROUTINE cxios_get_field_unit
    
    FUNCTION cxios_is_defined_field_unit(field_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_field_unit
      INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
    END FUNCTION cxios_is_defined_field_unit
    
    
    END INTERFACE
  
END MODULE field_interface_attr
