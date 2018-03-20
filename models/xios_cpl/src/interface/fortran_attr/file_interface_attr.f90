! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *

MODULE file_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING
  
  INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99
    
    
    SUBROUTINE cxios_set_file_description(file_hdl, description, description_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: description
      INTEGER  (kind = C_INT)     , VALUE        :: description_size
    END SUBROUTINE cxios_set_file_description
    
    SUBROUTINE cxios_get_file_description(file_hdl, description, description_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: description
      INTEGER  (kind = C_INT)     , VALUE        :: description_size
    END SUBROUTINE cxios_get_file_description
    
    FUNCTION cxios_is_defined_file_description(file_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_file_description
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
    END FUNCTION cxios_is_defined_file_description
    
    
    SUBROUTINE cxios_set_file_enabled(file_hdl, enabled) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      LOGICAL (KIND=C_BOOL)      , VALUE :: enabled
    END SUBROUTINE cxios_set_file_enabled
    
    SUBROUTINE cxios_get_file_enabled(file_hdl, enabled) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      LOGICAL (KIND=C_BOOL)             :: enabled
    END SUBROUTINE cxios_get_file_enabled
    
    FUNCTION cxios_is_defined_file_enabled(file_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_file_enabled
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
    END FUNCTION cxios_is_defined_file_enabled
    
    
    SUBROUTINE cxios_set_file_min_digits(file_hdl, min_digits) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      INTEGER (KIND=C_INT)      , VALUE :: min_digits
    END SUBROUTINE cxios_set_file_min_digits
    
    SUBROUTINE cxios_get_file_min_digits(file_hdl, min_digits) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      INTEGER (KIND=C_INT)             :: min_digits
    END SUBROUTINE cxios_get_file_min_digits
    
    FUNCTION cxios_is_defined_file_min_digits(file_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_file_min_digits
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
    END FUNCTION cxios_is_defined_file_min_digits
    
    
    SUBROUTINE cxios_set_file_name(file_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_set_file_name
    
    SUBROUTINE cxios_get_file_name(file_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_get_file_name
    
    FUNCTION cxios_is_defined_file_name(file_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_file_name
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
    END FUNCTION cxios_is_defined_file_name
    
    
    SUBROUTINE cxios_set_file_name_suffix(file_hdl, name_suffix, name_suffix_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name_suffix
      INTEGER  (kind = C_INT)     , VALUE        :: name_suffix_size
    END SUBROUTINE cxios_set_file_name_suffix
    
    SUBROUTINE cxios_get_file_name_suffix(file_hdl, name_suffix, name_suffix_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name_suffix
      INTEGER  (kind = C_INT)     , VALUE        :: name_suffix_size
    END SUBROUTINE cxios_get_file_name_suffix
    
    FUNCTION cxios_is_defined_file_name_suffix(file_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_file_name_suffix
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
    END FUNCTION cxios_is_defined_file_name_suffix
    
    
    SUBROUTINE cxios_set_file_output_freq(file_hdl, output_freq, output_freq_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: output_freq
      INTEGER  (kind = C_INT)     , VALUE        :: output_freq_size
    END SUBROUTINE cxios_set_file_output_freq
    
    SUBROUTINE cxios_get_file_output_freq(file_hdl, output_freq, output_freq_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: output_freq
      INTEGER  (kind = C_INT)     , VALUE        :: output_freq_size
    END SUBROUTINE cxios_get_file_output_freq
    
    FUNCTION cxios_is_defined_file_output_freq(file_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_file_output_freq
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
    END FUNCTION cxios_is_defined_file_output_freq
    
    
    SUBROUTINE cxios_set_file_output_level(file_hdl, output_level) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      INTEGER (KIND=C_INT)      , VALUE :: output_level
    END SUBROUTINE cxios_set_file_output_level
    
    SUBROUTINE cxios_get_file_output_level(file_hdl, output_level) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      INTEGER (KIND=C_INT)             :: output_level
    END SUBROUTINE cxios_get_file_output_level
    
    FUNCTION cxios_is_defined_file_output_level(file_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_file_output_level
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
    END FUNCTION cxios_is_defined_file_output_level
    
    
    SUBROUTINE cxios_set_file_par_access(file_hdl, par_access, par_access_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: par_access
      INTEGER  (kind = C_INT)     , VALUE        :: par_access_size
    END SUBROUTINE cxios_set_file_par_access
    
    SUBROUTINE cxios_get_file_par_access(file_hdl, par_access, par_access_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: par_access
      INTEGER  (kind = C_INT)     , VALUE        :: par_access_size
    END SUBROUTINE cxios_get_file_par_access
    
    FUNCTION cxios_is_defined_file_par_access(file_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_file_par_access
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
    END FUNCTION cxios_is_defined_file_par_access
    
    
    SUBROUTINE cxios_set_file_split_freq(file_hdl, split_freq, split_freq_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: split_freq
      INTEGER  (kind = C_INT)     , VALUE        :: split_freq_size
    END SUBROUTINE cxios_set_file_split_freq
    
    SUBROUTINE cxios_get_file_split_freq(file_hdl, split_freq, split_freq_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: split_freq
      INTEGER  (kind = C_INT)     , VALUE        :: split_freq_size
    END SUBROUTINE cxios_get_file_split_freq
    
    FUNCTION cxios_is_defined_file_split_freq(file_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_file_split_freq
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
    END FUNCTION cxios_is_defined_file_split_freq
    
    
    SUBROUTINE cxios_set_file_split_freq_format(file_hdl, split_freq_format, split_freq_format_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: split_freq_format
      INTEGER  (kind = C_INT)     , VALUE        :: split_freq_format_size
    END SUBROUTINE cxios_set_file_split_freq_format
    
    SUBROUTINE cxios_get_file_split_freq_format(file_hdl, split_freq_format, split_freq_format_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: split_freq_format
      INTEGER  (kind = C_INT)     , VALUE        :: split_freq_format_size
    END SUBROUTINE cxios_get_file_split_freq_format
    
    FUNCTION cxios_is_defined_file_split_freq_format(file_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_file_split_freq_format
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
    END FUNCTION cxios_is_defined_file_split_freq_format
    
    
    SUBROUTINE cxios_set_file_sync_freq(file_hdl, sync_freq, sync_freq_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: sync_freq
      INTEGER  (kind = C_INT)     , VALUE        :: sync_freq_size
    END SUBROUTINE cxios_set_file_sync_freq
    
    SUBROUTINE cxios_get_file_sync_freq(file_hdl, sync_freq, sync_freq_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: sync_freq
      INTEGER  (kind = C_INT)     , VALUE        :: sync_freq_size
    END SUBROUTINE cxios_get_file_sync_freq
    
    FUNCTION cxios_is_defined_file_sync_freq(file_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_file_sync_freq
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
    END FUNCTION cxios_is_defined_file_sync_freq
    
    
    SUBROUTINE cxios_set_file_type(file_hdl, type, type_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: type
      INTEGER  (kind = C_INT)     , VALUE        :: type_size
    END SUBROUTINE cxios_set_file_type
    
    SUBROUTINE cxios_get_file_type(file_hdl, type, type_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: type
      INTEGER  (kind = C_INT)     , VALUE        :: type_size
    END SUBROUTINE cxios_get_file_type
    
    FUNCTION cxios_is_defined_file_type(file_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_file_type
      INTEGER (kind = C_INTPTR_T), VALUE :: file_hdl
    END FUNCTION cxios_is_defined_file_type
    
    
    END INTERFACE
  
END MODULE file_interface_attr
