! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *

MODULE fieldgroup_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING
  
  INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99
    
    
    SUBROUTINE cxios_set_fieldgroup_axis_ref(fieldgroup_hdl, axis_ref, axis_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: axis_ref
      INTEGER  (kind = C_INT)     , VALUE        :: axis_ref_size
    END SUBROUTINE cxios_set_fieldgroup_axis_ref
    
    SUBROUTINE cxios_get_fieldgroup_axis_ref(fieldgroup_hdl, axis_ref, axis_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: axis_ref
      INTEGER  (kind = C_INT)     , VALUE        :: axis_ref_size
    END SUBROUTINE cxios_get_fieldgroup_axis_ref
    
    FUNCTION cxios_is_defined_fieldgroup_axis_ref(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_axis_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_axis_ref
    
    
    SUBROUTINE cxios_set_fieldgroup_default_value(fieldgroup_hdl, default_value) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      REAL (KIND=C_DOUBLE)      , VALUE :: default_value
    END SUBROUTINE cxios_set_fieldgroup_default_value
    
    SUBROUTINE cxios_get_fieldgroup_default_value(fieldgroup_hdl, default_value) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      REAL (KIND=C_DOUBLE)             :: default_value
    END SUBROUTINE cxios_get_fieldgroup_default_value
    
    FUNCTION cxios_is_defined_fieldgroup_default_value(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_default_value
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_default_value
    
    
    SUBROUTINE cxios_set_fieldgroup_domain_ref(fieldgroup_hdl, domain_ref, domain_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_ref
      INTEGER  (kind = C_INT)     , VALUE        :: domain_ref_size
    END SUBROUTINE cxios_set_fieldgroup_domain_ref
    
    SUBROUTINE cxios_get_fieldgroup_domain_ref(fieldgroup_hdl, domain_ref, domain_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_ref
      INTEGER  (kind = C_INT)     , VALUE        :: domain_ref_size
    END SUBROUTINE cxios_get_fieldgroup_domain_ref
    
    FUNCTION cxios_is_defined_fieldgroup_domain_ref(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_domain_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_domain_ref
    
    
    SUBROUTINE cxios_set_fieldgroup_enabled(fieldgroup_hdl, enabled) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      LOGICAL (KIND=C_BOOL)      , VALUE :: enabled
    END SUBROUTINE cxios_set_fieldgroup_enabled
    
    SUBROUTINE cxios_get_fieldgroup_enabled(fieldgroup_hdl, enabled) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      LOGICAL (KIND=C_BOOL)             :: enabled
    END SUBROUTINE cxios_get_fieldgroup_enabled
    
    FUNCTION cxios_is_defined_fieldgroup_enabled(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_enabled
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_enabled
    
    
    SUBROUTINE cxios_set_fieldgroup_field_ref(fieldgroup_hdl, field_ref, field_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: field_ref
      INTEGER  (kind = C_INT)     , VALUE        :: field_ref_size
    END SUBROUTINE cxios_set_fieldgroup_field_ref
    
    SUBROUTINE cxios_get_fieldgroup_field_ref(fieldgroup_hdl, field_ref, field_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: field_ref
      INTEGER  (kind = C_INT)     , VALUE        :: field_ref_size
    END SUBROUTINE cxios_get_fieldgroup_field_ref
    
    FUNCTION cxios_is_defined_fieldgroup_field_ref(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_field_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_field_ref
    
    
    SUBROUTINE cxios_set_fieldgroup_freq_offset(fieldgroup_hdl, freq_offset, freq_offset_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: freq_offset
      INTEGER  (kind = C_INT)     , VALUE        :: freq_offset_size
    END SUBROUTINE cxios_set_fieldgroup_freq_offset
    
    SUBROUTINE cxios_get_fieldgroup_freq_offset(fieldgroup_hdl, freq_offset, freq_offset_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: freq_offset
      INTEGER  (kind = C_INT)     , VALUE        :: freq_offset_size
    END SUBROUTINE cxios_get_fieldgroup_freq_offset
    
    FUNCTION cxios_is_defined_fieldgroup_freq_offset(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_freq_offset
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_freq_offset
    
    
    SUBROUTINE cxios_set_fieldgroup_freq_op(fieldgroup_hdl, freq_op, freq_op_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: freq_op
      INTEGER  (kind = C_INT)     , VALUE        :: freq_op_size
    END SUBROUTINE cxios_set_fieldgroup_freq_op
    
    SUBROUTINE cxios_get_fieldgroup_freq_op(fieldgroup_hdl, freq_op, freq_op_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: freq_op
      INTEGER  (kind = C_INT)     , VALUE        :: freq_op_size
    END SUBROUTINE cxios_get_fieldgroup_freq_op
    
    FUNCTION cxios_is_defined_fieldgroup_freq_op(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_freq_op
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_freq_op
    
    
    SUBROUTINE cxios_set_fieldgroup_grid_ref(fieldgroup_hdl, grid_ref, grid_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: grid_ref
      INTEGER  (kind = C_INT)     , VALUE        :: grid_ref_size
    END SUBROUTINE cxios_set_fieldgroup_grid_ref
    
    SUBROUTINE cxios_get_fieldgroup_grid_ref(fieldgroup_hdl, grid_ref, grid_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: grid_ref
      INTEGER  (kind = C_INT)     , VALUE        :: grid_ref_size
    END SUBROUTINE cxios_get_fieldgroup_grid_ref
    
    FUNCTION cxios_is_defined_fieldgroup_grid_ref(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_grid_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_grid_ref
    
    
    SUBROUTINE cxios_set_fieldgroup_group_ref(fieldgroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_set_fieldgroup_group_ref
    
    SUBROUTINE cxios_get_fieldgroup_group_ref(fieldgroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_get_fieldgroup_group_ref
    
    FUNCTION cxios_is_defined_fieldgroup_group_ref(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_group_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_group_ref
    
    
    SUBROUTINE cxios_set_fieldgroup_level(fieldgroup_hdl, level) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: level
    END SUBROUTINE cxios_set_fieldgroup_level
    
    SUBROUTINE cxios_get_fieldgroup_level(fieldgroup_hdl, level) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      INTEGER (KIND=C_INT)             :: level
    END SUBROUTINE cxios_get_fieldgroup_level
    
    FUNCTION cxios_is_defined_fieldgroup_level(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_level
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_level
    
    
    SUBROUTINE cxios_set_fieldgroup_long_name(fieldgroup_hdl, long_name, long_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
      INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
    END SUBROUTINE cxios_set_fieldgroup_long_name
    
    SUBROUTINE cxios_get_fieldgroup_long_name(fieldgroup_hdl, long_name, long_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
      INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
    END SUBROUTINE cxios_get_fieldgroup_long_name
    
    FUNCTION cxios_is_defined_fieldgroup_long_name(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_long_name
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_long_name
    
    
    SUBROUTINE cxios_set_fieldgroup_name(fieldgroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_set_fieldgroup_name
    
    SUBROUTINE cxios_get_fieldgroup_name(fieldgroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_get_fieldgroup_name
    
    FUNCTION cxios_is_defined_fieldgroup_name(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_name
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_name
    
    
    SUBROUTINE cxios_set_fieldgroup_operation(fieldgroup_hdl, operation, operation_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: operation
      INTEGER  (kind = C_INT)     , VALUE        :: operation_size
    END SUBROUTINE cxios_set_fieldgroup_operation
    
    SUBROUTINE cxios_get_fieldgroup_operation(fieldgroup_hdl, operation, operation_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: operation
      INTEGER  (kind = C_INT)     , VALUE        :: operation_size
    END SUBROUTINE cxios_get_fieldgroup_operation
    
    FUNCTION cxios_is_defined_fieldgroup_operation(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_operation
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_operation
    
    
    SUBROUTINE cxios_set_fieldgroup_prec(fieldgroup_hdl, prec) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: prec
    END SUBROUTINE cxios_set_fieldgroup_prec
    
    SUBROUTINE cxios_get_fieldgroup_prec(fieldgroup_hdl, prec) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      INTEGER (KIND=C_INT)             :: prec
    END SUBROUTINE cxios_get_fieldgroup_prec
    
    FUNCTION cxios_is_defined_fieldgroup_prec(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_prec
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_prec
    
    
    SUBROUTINE cxios_set_fieldgroup_standard_name(fieldgroup_hdl, standard_name, standard_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
      INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
    END SUBROUTINE cxios_set_fieldgroup_standard_name
    
    SUBROUTINE cxios_get_fieldgroup_standard_name(fieldgroup_hdl, standard_name, standard_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
      INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
    END SUBROUTINE cxios_get_fieldgroup_standard_name
    
    FUNCTION cxios_is_defined_fieldgroup_standard_name(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_standard_name
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_standard_name
    
    
    SUBROUTINE cxios_set_fieldgroup_unit(fieldgroup_hdl, unit, unit_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: unit
      INTEGER  (kind = C_INT)     , VALUE        :: unit_size
    END SUBROUTINE cxios_set_fieldgroup_unit
    
    SUBROUTINE cxios_get_fieldgroup_unit(fieldgroup_hdl, unit, unit_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: unit
      INTEGER  (kind = C_INT)     , VALUE        :: unit_size
    END SUBROUTINE cxios_get_fieldgroup_unit
    
    FUNCTION cxios_is_defined_fieldgroup_unit(fieldgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_fieldgroup_unit
      INTEGER (kind = C_INTPTR_T), VALUE :: fieldgroup_hdl
    END FUNCTION cxios_is_defined_fieldgroup_unit
    
    
    END INTERFACE
  
END MODULE fieldgroup_interface_attr
