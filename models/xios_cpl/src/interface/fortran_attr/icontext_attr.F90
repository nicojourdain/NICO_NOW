! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE icontext_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE icontext
  USE context_interface_attr
  
CONTAINS
  
  SUBROUTINE xios(set_context_attr)  &
    ( context_id, calendar_type, output_dir, start_date, time_origin, timestep )
    
    IMPLICIT NONE
      TYPE(txios(context))  :: context_hdl
      CHARACTER(LEN=*), INTENT(IN) ::context_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: calendar_type
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_dir
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: start_date
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: time_origin
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: timestep
      
      CALL xios(get_context_handle)(context_id,context_hdl)
      CALL xios(set_context_attr_hdl_)   &
      ( context_hdl, calendar_type, output_dir, start_date, time_origin, timestep )
    
  END SUBROUTINE xios(set_context_attr)
  
  SUBROUTINE xios(set_context_attr_hdl)  &
    ( context_hdl, calendar_type, output_dir, start_date, time_origin, timestep )
    
    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: calendar_type
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_dir
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: start_date
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: time_origin
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: timestep
      
      CALL xios(set_context_attr_hdl_)  &
      ( context_hdl, calendar_type, output_dir, start_date, time_origin, timestep )
    
  END SUBROUTINE xios(set_context_attr_hdl)
  
  SUBROUTINE xios(set_context_attr_hdl_)   &
    ( context_hdl, calendar_type_, output_dir_, start_date_, time_origin_, timestep_ )
    
    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: calendar_type_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_dir_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: start_date_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: time_origin_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: timestep_
      
      IF (PRESENT(calendar_type_)) THEN
        CALL cxios_set_context_calendar_type(context_hdl%daddr, calendar_type_, len(calendar_type_))
      ENDIF
      
      IF (PRESENT(output_dir_)) THEN
        CALL cxios_set_context_output_dir(context_hdl%daddr, output_dir_, len(output_dir_))
      ENDIF
      
      IF (PRESENT(start_date_)) THEN
        CALL cxios_set_context_start_date(context_hdl%daddr, start_date_, len(start_date_))
      ENDIF
      
      IF (PRESENT(time_origin_)) THEN
        CALL cxios_set_context_time_origin(context_hdl%daddr, time_origin_, len(time_origin_))
      ENDIF
      
      IF (PRESENT(timestep_)) THEN
        CALL cxios_set_context_timestep(context_hdl%daddr, timestep_, len(timestep_))
      ENDIF
      
      
    
  END SUBROUTINE xios(set_context_attr_hdl_)
  
  SUBROUTINE xios(get_context_attr)  &
    ( context_id, calendar_type, output_dir, start_date, time_origin, timestep )
    
    IMPLICIT NONE
      TYPE(txios(context))  :: context_hdl
      CHARACTER(LEN=*), INTENT(IN) ::context_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: calendar_type
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_dir
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: start_date
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: time_origin
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: timestep
      
      CALL xios(get_context_handle)(context_id,context_hdl)
      CALL xios(get_context_attr_hdl_)   &
      ( context_hdl, calendar_type, output_dir, start_date, time_origin, timestep )
    
  END SUBROUTINE xios(get_context_attr)
  
  SUBROUTINE xios(get_context_attr_hdl)  &
    ( context_hdl, calendar_type, output_dir, start_date, time_origin, timestep )
    
    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: calendar_type
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_dir
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: start_date
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: time_origin
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: timestep
      
      CALL xios(get_context_attr_hdl_)  &
      ( context_hdl, calendar_type, output_dir, start_date, time_origin, timestep )
    
  END SUBROUTINE xios(get_context_attr_hdl)
  
  SUBROUTINE xios(get_context_attr_hdl_)   &
    ( context_hdl, calendar_type_, output_dir_, start_date_, time_origin_, timestep_ )
    
    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: calendar_type_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_dir_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: start_date_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: time_origin_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: timestep_
      
      IF (PRESENT(calendar_type_)) THEN
        CALL cxios_get_context_calendar_type(context_hdl%daddr, calendar_type_, len(calendar_type_))
      ENDIF
      
      IF (PRESENT(output_dir_)) THEN
        CALL cxios_get_context_output_dir(context_hdl%daddr, output_dir_, len(output_dir_))
      ENDIF
      
      IF (PRESENT(start_date_)) THEN
        CALL cxios_get_context_start_date(context_hdl%daddr, start_date_, len(start_date_))
      ENDIF
      
      IF (PRESENT(time_origin_)) THEN
        CALL cxios_get_context_time_origin(context_hdl%daddr, time_origin_, len(time_origin_))
      ENDIF
      
      IF (PRESENT(timestep_)) THEN
        CALL cxios_get_context_timestep(context_hdl%daddr, timestep_, len(timestep_))
      ENDIF
      
      
    
  END SUBROUTINE xios(get_context_attr_hdl_)
  
  SUBROUTINE xios(is_defined_context_attr)  &
    ( context_id, calendar_type, output_dir, start_date, time_origin, timestep )
    
    IMPLICIT NONE
      TYPE(txios(context))  :: context_hdl
      CHARACTER(LEN=*), INTENT(IN) ::context_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: calendar_type
      LOGICAL(KIND=C_BOOL) :: calendar_type_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: output_dir
      LOGICAL(KIND=C_BOOL) :: output_dir_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: start_date
      LOGICAL(KIND=C_BOOL) :: start_date_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: time_origin
      LOGICAL(KIND=C_BOOL) :: time_origin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: timestep
      LOGICAL(KIND=C_BOOL) :: timestep_tmp
      
      CALL xios(get_context_handle)(context_id,context_hdl)
      CALL xios(is_defined_context_attr_hdl_)   &
      ( context_hdl, calendar_type, output_dir, start_date, time_origin, timestep )
    
  END SUBROUTINE xios(is_defined_context_attr)
  
  SUBROUTINE xios(is_defined_context_attr_hdl)  &
    ( context_hdl, calendar_type, output_dir, start_date, time_origin, timestep )
    
    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: calendar_type
      LOGICAL(KIND=C_BOOL) :: calendar_type_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: output_dir
      LOGICAL(KIND=C_BOOL) :: output_dir_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: start_date
      LOGICAL(KIND=C_BOOL) :: start_date_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: time_origin
      LOGICAL(KIND=C_BOOL) :: time_origin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: timestep
      LOGICAL(KIND=C_BOOL) :: timestep_tmp
      
      CALL xios(is_defined_context_attr_hdl_)  &
      ( context_hdl, calendar_type, output_dir, start_date, time_origin, timestep )
    
  END SUBROUTINE xios(is_defined_context_attr_hdl)
  
  SUBROUTINE xios(is_defined_context_attr_hdl_)   &
    ( context_hdl, calendar_type_, output_dir_, start_date_, time_origin_, timestep_ )
    
    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: calendar_type_
      LOGICAL(KIND=C_BOOL) :: calendar_type__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: output_dir_
      LOGICAL(KIND=C_BOOL) :: output_dir__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: start_date_
      LOGICAL(KIND=C_BOOL) :: start_date__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: time_origin_
      LOGICAL(KIND=C_BOOL) :: time_origin__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: timestep_
      LOGICAL(KIND=C_BOOL) :: timestep__tmp
      
      IF (PRESENT(calendar_type_)) THEN
        calendar_type__tmp=cxios_is_defined_context_calendar_type(context_hdl%daddr)
        calendar_type_=calendar_type__tmp
      ENDIF
      
      IF (PRESENT(output_dir_)) THEN
        output_dir__tmp=cxios_is_defined_context_output_dir(context_hdl%daddr)
        output_dir_=output_dir__tmp
      ENDIF
      
      IF (PRESENT(start_date_)) THEN
        start_date__tmp=cxios_is_defined_context_start_date(context_hdl%daddr)
        start_date_=start_date__tmp
      ENDIF
      
      IF (PRESENT(time_origin_)) THEN
        time_origin__tmp=cxios_is_defined_context_time_origin(context_hdl%daddr)
        time_origin_=time_origin__tmp
      ENDIF
      
      IF (PRESENT(timestep_)) THEN
        timestep__tmp=cxios_is_defined_context_timestep(context_hdl%daddr)
        timestep_=timestep__tmp
      ENDIF
      
      
    
  END SUBROUTINE xios(is_defined_context_attr_hdl_)
  
END MODULE icontext_attr
