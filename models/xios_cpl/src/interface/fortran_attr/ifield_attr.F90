! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE ifield_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE ifield
  USE field_interface_attr
  
CONTAINS
  
  SUBROUTINE xios(set_field_attr)  &
    ( field_id, axis_ref, default_value, domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref  &
    , level, long_name, name, operation, prec, standard_name, unit )
    
    IMPLICIT NONE
      TYPE(txios(field))  :: field_hdl
      CHARACTER(LEN=*), INTENT(IN) ::field_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: default_value
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref
      LOGICAL  , OPTIONAL, INTENT(IN) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: field_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: freq_offset
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: freq_op
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: grid_ref
      INTEGER  , OPTIONAL, INTENT(IN) :: level
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: operation
      INTEGER  , OPTIONAL, INTENT(IN) :: prec
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      
      CALL xios(get_field_handle)(field_id,field_hdl)
      CALL xios(set_field_attr_hdl_)   &
      ( field_hdl, axis_ref, default_value, domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref  &
      , level, long_name, name, operation, prec, standard_name, unit )
    
  END SUBROUTINE xios(set_field_attr)
  
  SUBROUTINE xios(set_field_attr_hdl)  &
    ( field_hdl, axis_ref, default_value, domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref  &
    , level, long_name, name, operation, prec, standard_name, unit )
    
    IMPLICIT NONE
      TYPE(txios(field)) , INTENT(IN) :: field_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: default_value
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref
      LOGICAL  , OPTIONAL, INTENT(IN) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: field_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: freq_offset
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: freq_op
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: grid_ref
      INTEGER  , OPTIONAL, INTENT(IN) :: level
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: operation
      INTEGER  , OPTIONAL, INTENT(IN) :: prec
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      
      CALL xios(set_field_attr_hdl_)  &
      ( field_hdl, axis_ref, default_value, domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref  &
      , level, long_name, name, operation, prec, standard_name, unit )
    
  END SUBROUTINE xios(set_field_attr_hdl)
  
  SUBROUTINE xios(set_field_attr_hdl_)   &
    ( field_hdl, axis_ref_, default_value_, domain_ref_, enabled_, field_ref_, freq_offset_, freq_op_  &
    , grid_ref_, level_, long_name_, name_, operation_, prec_, standard_name_, unit_ )
    
    IMPLICIT NONE
      TYPE(txios(field)) , INTENT(IN) :: field_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: default_value_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref_
      LOGICAL  , OPTIONAL, INTENT(IN) :: enabled_
      LOGICAL (KIND=C_BOOL) :: enabled__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: field_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: freq_offset_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: freq_op_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: grid_ref_
      INTEGER  , OPTIONAL, INTENT(IN) :: level_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: operation_
      INTEGER  , OPTIONAL, INTENT(IN) :: prec_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit_
      
      IF (PRESENT(axis_ref_)) THEN
        CALL cxios_set_field_axis_ref(field_hdl%daddr, axis_ref_, len(axis_ref_))
      ENDIF
      
      IF (PRESENT(default_value_)) THEN
        CALL cxios_set_field_default_value(field_hdl%daddr, default_value_)
      ENDIF
      
      IF (PRESENT(domain_ref_)) THEN
        CALL cxios_set_field_domain_ref(field_hdl%daddr, domain_ref_, len(domain_ref_))
      ENDIF
      
      IF (PRESENT(enabled_)) THEN
        enabled__tmp=enabled_
        CALL cxios_set_field_enabled(field_hdl%daddr, enabled__tmp)
      ENDIF
      
      IF (PRESENT(field_ref_)) THEN
        CALL cxios_set_field_field_ref(field_hdl%daddr, field_ref_, len(field_ref_))
      ENDIF
      
      IF (PRESENT(freq_offset_)) THEN
        CALL cxios_set_field_freq_offset(field_hdl%daddr, freq_offset_, len(freq_offset_))
      ENDIF
      
      IF (PRESENT(freq_op_)) THEN
        CALL cxios_set_field_freq_op(field_hdl%daddr, freq_op_, len(freq_op_))
      ENDIF
      
      IF (PRESENT(grid_ref_)) THEN
        CALL cxios_set_field_grid_ref(field_hdl%daddr, grid_ref_, len(grid_ref_))
      ENDIF
      
      IF (PRESENT(level_)) THEN
        CALL cxios_set_field_level(field_hdl%daddr, level_)
      ENDIF
      
      IF (PRESENT(long_name_)) THEN
        CALL cxios_set_field_long_name(field_hdl%daddr, long_name_, len(long_name_))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_set_field_name(field_hdl%daddr, name_, len(name_))
      ENDIF
      
      IF (PRESENT(operation_)) THEN
        CALL cxios_set_field_operation(field_hdl%daddr, operation_, len(operation_))
      ENDIF
      
      IF (PRESENT(prec_)) THEN
        CALL cxios_set_field_prec(field_hdl%daddr, prec_)
      ENDIF
      
      IF (PRESENT(standard_name_)) THEN
        CALL cxios_set_field_standard_name(field_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF
      
      IF (PRESENT(unit_)) THEN
        CALL cxios_set_field_unit(field_hdl%daddr, unit_, len(unit_))
      ENDIF
      
      
    
  END SUBROUTINE xios(set_field_attr_hdl_)
  
  SUBROUTINE xios(get_field_attr)  &
    ( field_id, axis_ref, default_value, domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref  &
    , level, long_name, name, operation, prec, standard_name, unit )
    
    IMPLICIT NONE
      TYPE(txios(field))  :: field_hdl
      CHARACTER(LEN=*), INTENT(IN) ::field_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: default_value
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL  , OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: field_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: freq_offset
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: freq_op
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: grid_ref
      INTEGER  , OPTIONAL, INTENT(OUT) :: level
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: operation
      INTEGER  , OPTIONAL, INTENT(OUT) :: prec
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      
      CALL xios(get_field_handle)(field_id,field_hdl)
      CALL xios(get_field_attr_hdl_)   &
      ( field_hdl, axis_ref, default_value, domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref  &
      , level, long_name, name, operation, prec, standard_name, unit )
    
  END SUBROUTINE xios(get_field_attr)
  
  SUBROUTINE xios(get_field_attr_hdl)  &
    ( field_hdl, axis_ref, default_value, domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref  &
    , level, long_name, name, operation, prec, standard_name, unit )
    
    IMPLICIT NONE
      TYPE(txios(field)) , INTENT(IN) :: field_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: default_value
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL  , OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: field_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: freq_offset
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: freq_op
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: grid_ref
      INTEGER  , OPTIONAL, INTENT(OUT) :: level
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: operation
      INTEGER  , OPTIONAL, INTENT(OUT) :: prec
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      
      CALL xios(get_field_attr_hdl_)  &
      ( field_hdl, axis_ref, default_value, domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref  &
      , level, long_name, name, operation, prec, standard_name, unit )
    
  END SUBROUTINE xios(get_field_attr_hdl)
  
  SUBROUTINE xios(get_field_attr_hdl_)   &
    ( field_hdl, axis_ref_, default_value_, domain_ref_, enabled_, field_ref_, freq_offset_, freq_op_  &
    , grid_ref_, level_, long_name_, name_, operation_, prec_, standard_name_, unit_ )
    
    IMPLICIT NONE
      TYPE(txios(field)) , INTENT(IN) :: field_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: default_value_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: enabled_
      LOGICAL (KIND=C_BOOL) :: enabled__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: field_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: freq_offset_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: freq_op_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: grid_ref_
      INTEGER  , OPTIONAL, INTENT(OUT) :: level_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: operation_
      INTEGER  , OPTIONAL, INTENT(OUT) :: prec_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit_
      
      IF (PRESENT(axis_ref_)) THEN
        CALL cxios_get_field_axis_ref(field_hdl%daddr, axis_ref_, len(axis_ref_))
      ENDIF
      
      IF (PRESENT(default_value_)) THEN
        CALL cxios_get_field_default_value(field_hdl%daddr, default_value_)
      ENDIF
      
      IF (PRESENT(domain_ref_)) THEN
        CALL cxios_get_field_domain_ref(field_hdl%daddr, domain_ref_, len(domain_ref_))
      ENDIF
      
      IF (PRESENT(enabled_)) THEN
        CALL cxios_get_field_enabled(field_hdl%daddr, enabled__tmp)
        enabled_=enabled__tmp
      ENDIF
      
      IF (PRESENT(field_ref_)) THEN
        CALL cxios_get_field_field_ref(field_hdl%daddr, field_ref_, len(field_ref_))
      ENDIF
      
      IF (PRESENT(freq_offset_)) THEN
        CALL cxios_get_field_freq_offset(field_hdl%daddr, freq_offset_, len(freq_offset_))
      ENDIF
      
      IF (PRESENT(freq_op_)) THEN
        CALL cxios_get_field_freq_op(field_hdl%daddr, freq_op_, len(freq_op_))
      ENDIF
      
      IF (PRESENT(grid_ref_)) THEN
        CALL cxios_get_field_grid_ref(field_hdl%daddr, grid_ref_, len(grid_ref_))
      ENDIF
      
      IF (PRESENT(level_)) THEN
        CALL cxios_get_field_level(field_hdl%daddr, level_)
      ENDIF
      
      IF (PRESENT(long_name_)) THEN
        CALL cxios_get_field_long_name(field_hdl%daddr, long_name_, len(long_name_))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_get_field_name(field_hdl%daddr, name_, len(name_))
      ENDIF
      
      IF (PRESENT(operation_)) THEN
        CALL cxios_get_field_operation(field_hdl%daddr, operation_, len(operation_))
      ENDIF
      
      IF (PRESENT(prec_)) THEN
        CALL cxios_get_field_prec(field_hdl%daddr, prec_)
      ENDIF
      
      IF (PRESENT(standard_name_)) THEN
        CALL cxios_get_field_standard_name(field_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF
      
      IF (PRESENT(unit_)) THEN
        CALL cxios_get_field_unit(field_hdl%daddr, unit_, len(unit_))
      ENDIF
      
      
    
  END SUBROUTINE xios(get_field_attr_hdl_)
  
  SUBROUTINE xios(is_defined_field_attr)  &
    ( field_id, axis_ref, default_value, domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref  &
    , level, long_name, name, operation, prec, standard_name, unit )
    
    IMPLICIT NONE
      TYPE(txios(field))  :: field_hdl
      CHARACTER(LEN=*), INTENT(IN) ::field_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_ref
      LOGICAL(KIND=C_BOOL) :: axis_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_value
      LOGICAL(KIND=C_BOOL) :: default_value_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL(KIND=C_BOOL) :: domain_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL(KIND=C_BOOL) :: enabled_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: field_ref
      LOGICAL(KIND=C_BOOL) :: field_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: freq_offset
      LOGICAL(KIND=C_BOOL) :: freq_offset_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: freq_op
      LOGICAL(KIND=C_BOOL) :: freq_op_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: grid_ref
      LOGICAL(KIND=C_BOOL) :: grid_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: level
      LOGICAL(KIND=C_BOOL) :: level_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name
      LOGICAL(KIND=C_BOOL) :: long_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: operation
      LOGICAL(KIND=C_BOOL) :: operation_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: prec
      LOGICAL(KIND=C_BOOL) :: prec_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name
      LOGICAL(KIND=C_BOOL) :: standard_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: unit
      LOGICAL(KIND=C_BOOL) :: unit_tmp
      
      CALL xios(get_field_handle)(field_id,field_hdl)
      CALL xios(is_defined_field_attr_hdl_)   &
      ( field_hdl, axis_ref, default_value, domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref  &
      , level, long_name, name, operation, prec, standard_name, unit )
    
  END SUBROUTINE xios(is_defined_field_attr)
  
  SUBROUTINE xios(is_defined_field_attr_hdl)  &
    ( field_hdl, axis_ref, default_value, domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref  &
    , level, long_name, name, operation, prec, standard_name, unit )
    
    IMPLICIT NONE
      TYPE(txios(field)) , INTENT(IN) :: field_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_ref
      LOGICAL(KIND=C_BOOL) :: axis_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_value
      LOGICAL(KIND=C_BOOL) :: default_value_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL(KIND=C_BOOL) :: domain_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL(KIND=C_BOOL) :: enabled_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: field_ref
      LOGICAL(KIND=C_BOOL) :: field_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: freq_offset
      LOGICAL(KIND=C_BOOL) :: freq_offset_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: freq_op
      LOGICAL(KIND=C_BOOL) :: freq_op_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: grid_ref
      LOGICAL(KIND=C_BOOL) :: grid_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: level
      LOGICAL(KIND=C_BOOL) :: level_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name
      LOGICAL(KIND=C_BOOL) :: long_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: operation
      LOGICAL(KIND=C_BOOL) :: operation_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: prec
      LOGICAL(KIND=C_BOOL) :: prec_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name
      LOGICAL(KIND=C_BOOL) :: standard_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: unit
      LOGICAL(KIND=C_BOOL) :: unit_tmp
      
      CALL xios(is_defined_field_attr_hdl_)  &
      ( field_hdl, axis_ref, default_value, domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref  &
      , level, long_name, name, operation, prec, standard_name, unit )
    
  END SUBROUTINE xios(is_defined_field_attr_hdl)
  
  SUBROUTINE xios(is_defined_field_attr_hdl_)   &
    ( field_hdl, axis_ref_, default_value_, domain_ref_, enabled_, field_ref_, freq_offset_, freq_op_  &
    , grid_ref_, level_, long_name_, name_, operation_, prec_, standard_name_, unit_ )
    
    IMPLICIT NONE
      TYPE(txios(field)) , INTENT(IN) :: field_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_ref_
      LOGICAL(KIND=C_BOOL) :: axis_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_value_
      LOGICAL(KIND=C_BOOL) :: default_value__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_ref_
      LOGICAL(KIND=C_BOOL) :: domain_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: enabled_
      LOGICAL(KIND=C_BOOL) :: enabled__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: field_ref_
      LOGICAL(KIND=C_BOOL) :: field_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: freq_offset_
      LOGICAL(KIND=C_BOOL) :: freq_offset__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: freq_op_
      LOGICAL(KIND=C_BOOL) :: freq_op__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: grid_ref_
      LOGICAL(KIND=C_BOOL) :: grid_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: level_
      LOGICAL(KIND=C_BOOL) :: level__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name_
      LOGICAL(KIND=C_BOOL) :: long_name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_
      LOGICAL(KIND=C_BOOL) :: name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: operation_
      LOGICAL(KIND=C_BOOL) :: operation__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: prec_
      LOGICAL(KIND=C_BOOL) :: prec__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name_
      LOGICAL(KIND=C_BOOL) :: standard_name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: unit_
      LOGICAL(KIND=C_BOOL) :: unit__tmp
      
      IF (PRESENT(axis_ref_)) THEN
        axis_ref__tmp=cxios_is_defined_field_axis_ref(field_hdl%daddr)
        axis_ref_=axis_ref__tmp
      ENDIF
      
      IF (PRESENT(default_value_)) THEN
        default_value__tmp=cxios_is_defined_field_default_value(field_hdl%daddr)
        default_value_=default_value__tmp
      ENDIF
      
      IF (PRESENT(domain_ref_)) THEN
        domain_ref__tmp=cxios_is_defined_field_domain_ref(field_hdl%daddr)
        domain_ref_=domain_ref__tmp
      ENDIF
      
      IF (PRESENT(enabled_)) THEN
        enabled__tmp=cxios_is_defined_field_enabled(field_hdl%daddr)
        enabled_=enabled__tmp
      ENDIF
      
      IF (PRESENT(field_ref_)) THEN
        field_ref__tmp=cxios_is_defined_field_field_ref(field_hdl%daddr)
        field_ref_=field_ref__tmp
      ENDIF
      
      IF (PRESENT(freq_offset_)) THEN
        freq_offset__tmp=cxios_is_defined_field_freq_offset(field_hdl%daddr)
        freq_offset_=freq_offset__tmp
      ENDIF
      
      IF (PRESENT(freq_op_)) THEN
        freq_op__tmp=cxios_is_defined_field_freq_op(field_hdl%daddr)
        freq_op_=freq_op__tmp
      ENDIF
      
      IF (PRESENT(grid_ref_)) THEN
        grid_ref__tmp=cxios_is_defined_field_grid_ref(field_hdl%daddr)
        grid_ref_=grid_ref__tmp
      ENDIF
      
      IF (PRESENT(level_)) THEN
        level__tmp=cxios_is_defined_field_level(field_hdl%daddr)
        level_=level__tmp
      ENDIF
      
      IF (PRESENT(long_name_)) THEN
        long_name__tmp=cxios_is_defined_field_long_name(field_hdl%daddr)
        long_name_=long_name__tmp
      ENDIF
      
      IF (PRESENT(name_)) THEN
        name__tmp=cxios_is_defined_field_name(field_hdl%daddr)
        name_=name__tmp
      ENDIF
      
      IF (PRESENT(operation_)) THEN
        operation__tmp=cxios_is_defined_field_operation(field_hdl%daddr)
        operation_=operation__tmp
      ENDIF
      
      IF (PRESENT(prec_)) THEN
        prec__tmp=cxios_is_defined_field_prec(field_hdl%daddr)
        prec_=prec__tmp
      ENDIF
      
      IF (PRESENT(standard_name_)) THEN
        standard_name__tmp=cxios_is_defined_field_standard_name(field_hdl%daddr)
        standard_name_=standard_name__tmp
      ENDIF
      
      IF (PRESENT(unit_)) THEN
        unit__tmp=cxios_is_defined_field_unit(field_hdl%daddr)
        unit_=unit__tmp
      ENDIF
      
      
    
  END SUBROUTINE xios(is_defined_field_attr_hdl_)
  
END MODULE ifield_attr
