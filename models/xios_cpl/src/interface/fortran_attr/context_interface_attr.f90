! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *

MODULE context_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING
  
  INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99
    
    
    SUBROUTINE cxios_set_context_calendar_type(context_hdl, calendar_type, calendar_type_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: calendar_type
      INTEGER  (kind = C_INT)     , VALUE        :: calendar_type_size
    END SUBROUTINE cxios_set_context_calendar_type
    
    SUBROUTINE cxios_get_context_calendar_type(context_hdl, calendar_type, calendar_type_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: calendar_type
      INTEGER  (kind = C_INT)     , VALUE        :: calendar_type_size
    END SUBROUTINE cxios_get_context_calendar_type
    
    FUNCTION cxios_is_defined_context_calendar_type(context_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_calendar_type
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_calendar_type
    
    
    SUBROUTINE cxios_set_context_output_dir(context_hdl, output_dir, output_dir_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: output_dir
      INTEGER  (kind = C_INT)     , VALUE        :: output_dir_size
    END SUBROUTINE cxios_set_context_output_dir
    
    SUBROUTINE cxios_get_context_output_dir(context_hdl, output_dir, output_dir_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: output_dir
      INTEGER  (kind = C_INT)     , VALUE        :: output_dir_size
    END SUBROUTINE cxios_get_context_output_dir
    
    FUNCTION cxios_is_defined_context_output_dir(context_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_output_dir
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_output_dir
    
    
    SUBROUTINE cxios_set_context_start_date(context_hdl, start_date, start_date_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: start_date
      INTEGER  (kind = C_INT)     , VALUE        :: start_date_size
    END SUBROUTINE cxios_set_context_start_date
    
    SUBROUTINE cxios_get_context_start_date(context_hdl, start_date, start_date_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: start_date
      INTEGER  (kind = C_INT)     , VALUE        :: start_date_size
    END SUBROUTINE cxios_get_context_start_date
    
    FUNCTION cxios_is_defined_context_start_date(context_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_start_date
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_start_date
    
    
    SUBROUTINE cxios_set_context_time_origin(context_hdl, time_origin, time_origin_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: time_origin
      INTEGER  (kind = C_INT)     , VALUE        :: time_origin_size
    END SUBROUTINE cxios_set_context_time_origin
    
    SUBROUTINE cxios_get_context_time_origin(context_hdl, time_origin, time_origin_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: time_origin
      INTEGER  (kind = C_INT)     , VALUE        :: time_origin_size
    END SUBROUTINE cxios_get_context_time_origin
    
    FUNCTION cxios_is_defined_context_time_origin(context_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_time_origin
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_time_origin
    
    
    SUBROUTINE cxios_set_context_timestep(context_hdl, timestep, timestep_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: timestep
      INTEGER  (kind = C_INT)     , VALUE        :: timestep_size
    END SUBROUTINE cxios_set_context_timestep
    
    SUBROUTINE cxios_get_context_timestep(context_hdl, timestep, timestep_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: timestep
      INTEGER  (kind = C_INT)     , VALUE        :: timestep_size
    END SUBROUTINE cxios_get_context_timestep
    
    FUNCTION cxios_is_defined_context_timestep(context_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_timestep
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_timestep
    
    
    END INTERFACE
  
END MODULE context_interface_attr
