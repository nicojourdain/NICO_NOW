! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE ifilegroup_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE ifile
  USE filegroup_interface_attr
  
CONTAINS
  
  SUBROUTINE xios(set_filegroup_attr)  &
    ( filegroup_id, description, enabled, group_ref, min_digits, name, name_suffix, output_freq  &
    , output_level, par_access, split_freq, split_freq_format, sync_freq, type )
    
    IMPLICIT NONE
      TYPE(txios(filegroup))  :: filegroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::filegroup_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      LOGICAL  , OPTIONAL, INTENT(IN) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      INTEGER  , OPTIONAL, INTENT(IN) :: min_digits
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_suffix
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_freq
      INTEGER  , OPTIONAL, INTENT(IN) :: output_level
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: par_access
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: split_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: split_freq_format
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: sync_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type
      
      CALL xios(get_filegroup_handle)(filegroup_id,filegroup_hdl)
      CALL xios(set_filegroup_attr_hdl_)   &
      ( filegroup_hdl, description, enabled, group_ref, min_digits, name, name_suffix, output_freq  &
      , output_level, par_access, split_freq, split_freq_format, sync_freq, type )
    
  END SUBROUTINE xios(set_filegroup_attr)
  
  SUBROUTINE xios(set_filegroup_attr_hdl)  &
    ( filegroup_hdl, description, enabled, group_ref, min_digits, name, name_suffix, output_freq  &
    , output_level, par_access, split_freq, split_freq_format, sync_freq, type )
    
    IMPLICIT NONE
      TYPE(txios(filegroup)) , INTENT(IN) :: filegroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      LOGICAL  , OPTIONAL, INTENT(IN) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      INTEGER  , OPTIONAL, INTENT(IN) :: min_digits
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_suffix
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_freq
      INTEGER  , OPTIONAL, INTENT(IN) :: output_level
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: par_access
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: split_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: split_freq_format
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: sync_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type
      
      CALL xios(set_filegroup_attr_hdl_)  &
      ( filegroup_hdl, description, enabled, group_ref, min_digits, name, name_suffix, output_freq  &
      , output_level, par_access, split_freq, split_freq_format, sync_freq, type )
    
  END SUBROUTINE xios(set_filegroup_attr_hdl)
  
  SUBROUTINE xios(set_filegroup_attr_hdl_)   &
    ( filegroup_hdl, description_, enabled_, group_ref_, min_digits_, name_, name_suffix_, output_freq_  &
    , output_level_, par_access_, split_freq_, split_freq_format_, sync_freq_, type_ )
    
    IMPLICIT NONE
      TYPE(txios(filegroup)) , INTENT(IN) :: filegroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description_
      LOGICAL  , OPTIONAL, INTENT(IN) :: enabled_
      LOGICAL (KIND=C_BOOL) :: enabled__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref_
      INTEGER  , OPTIONAL, INTENT(IN) :: min_digits_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_suffix_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER  , OPTIONAL, INTENT(IN) :: output_level_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: par_access_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: split_freq_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: split_freq_format_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: sync_freq_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type_
      
      IF (PRESENT(description_)) THEN
        CALL cxios_set_filegroup_description(filegroup_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(enabled_)) THEN
        enabled__tmp=enabled_
        CALL cxios_set_filegroup_enabled(filegroup_hdl%daddr, enabled__tmp)
      ENDIF
      
      IF (PRESENT(group_ref_)) THEN
        CALL cxios_set_filegroup_group_ref(filegroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF
      
      IF (PRESENT(min_digits_)) THEN
        CALL cxios_set_filegroup_min_digits(filegroup_hdl%daddr, min_digits_)
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_set_filegroup_name(filegroup_hdl%daddr, name_, len(name_))
      ENDIF
      
      IF (PRESENT(name_suffix_)) THEN
        CALL cxios_set_filegroup_name_suffix(filegroup_hdl%daddr, name_suffix_, len(name_suffix_))
      ENDIF
      
      IF (PRESENT(output_freq_)) THEN
        CALL cxios_set_filegroup_output_freq(filegroup_hdl%daddr, output_freq_, len(output_freq_))
      ENDIF
      
      IF (PRESENT(output_level_)) THEN
        CALL cxios_set_filegroup_output_level(filegroup_hdl%daddr, output_level_)
      ENDIF
      
      IF (PRESENT(par_access_)) THEN
        CALL cxios_set_filegroup_par_access(filegroup_hdl%daddr, par_access_, len(par_access_))
      ENDIF
      
      IF (PRESENT(split_freq_)) THEN
        CALL cxios_set_filegroup_split_freq(filegroup_hdl%daddr, split_freq_, len(split_freq_))
      ENDIF
      
      IF (PRESENT(split_freq_format_)) THEN
        CALL cxios_set_filegroup_split_freq_format(filegroup_hdl%daddr, split_freq_format_, len(split_freq_format_))
      ENDIF
      
      IF (PRESENT(sync_freq_)) THEN
        CALL cxios_set_filegroup_sync_freq(filegroup_hdl%daddr, sync_freq_, len(sync_freq_))
      ENDIF
      
      IF (PRESENT(type_)) THEN
        CALL cxios_set_filegroup_type(filegroup_hdl%daddr, type_, len(type_))
      ENDIF
      
      
    
  END SUBROUTINE xios(set_filegroup_attr_hdl_)
  
  SUBROUTINE xios(get_filegroup_attr)  &
    ( filegroup_id, description, enabled, group_ref, min_digits, name, name_suffix, output_freq  &
    , output_level, par_access, split_freq, split_freq_format, sync_freq, type )
    
    IMPLICIT NONE
      TYPE(txios(filegroup))  :: filegroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::filegroup_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      LOGICAL  , OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      INTEGER  , OPTIONAL, INTENT(OUT) :: min_digits
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_suffix
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_freq
      INTEGER  , OPTIONAL, INTENT(OUT) :: output_level
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: par_access
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: split_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: split_freq_format
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: sync_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type
      
      CALL xios(get_filegroup_handle)(filegroup_id,filegroup_hdl)
      CALL xios(get_filegroup_attr_hdl_)   &
      ( filegroup_hdl, description, enabled, group_ref, min_digits, name, name_suffix, output_freq  &
      , output_level, par_access, split_freq, split_freq_format, sync_freq, type )
    
  END SUBROUTINE xios(get_filegroup_attr)
  
  SUBROUTINE xios(get_filegroup_attr_hdl)  &
    ( filegroup_hdl, description, enabled, group_ref, min_digits, name, name_suffix, output_freq  &
    , output_level, par_access, split_freq, split_freq_format, sync_freq, type )
    
    IMPLICIT NONE
      TYPE(txios(filegroup)) , INTENT(IN) :: filegroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      LOGICAL  , OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      INTEGER  , OPTIONAL, INTENT(OUT) :: min_digits
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_suffix
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_freq
      INTEGER  , OPTIONAL, INTENT(OUT) :: output_level
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: par_access
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: split_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: split_freq_format
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: sync_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type
      
      CALL xios(get_filegroup_attr_hdl_)  &
      ( filegroup_hdl, description, enabled, group_ref, min_digits, name, name_suffix, output_freq  &
      , output_level, par_access, split_freq, split_freq_format, sync_freq, type )
    
  END SUBROUTINE xios(get_filegroup_attr_hdl)
  
  SUBROUTINE xios(get_filegroup_attr_hdl_)   &
    ( filegroup_hdl, description_, enabled_, group_ref_, min_digits_, name_, name_suffix_, output_freq_  &
    , output_level_, par_access_, split_freq_, split_freq_format_, sync_freq_, type_ )
    
    IMPLICIT NONE
      TYPE(txios(filegroup)) , INTENT(IN) :: filegroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: enabled_
      LOGICAL (KIND=C_BOOL) :: enabled__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref_
      INTEGER  , OPTIONAL, INTENT(OUT) :: min_digits_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_suffix_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_freq_
      INTEGER  , OPTIONAL, INTENT(OUT) :: output_level_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: par_access_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: split_freq_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: split_freq_format_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: sync_freq_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type_
      
      IF (PRESENT(description_)) THEN
        CALL cxios_get_filegroup_description(filegroup_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(enabled_)) THEN
        CALL cxios_get_filegroup_enabled(filegroup_hdl%daddr, enabled__tmp)
        enabled_=enabled__tmp
      ENDIF
      
      IF (PRESENT(group_ref_)) THEN
        CALL cxios_get_filegroup_group_ref(filegroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF
      
      IF (PRESENT(min_digits_)) THEN
        CALL cxios_get_filegroup_min_digits(filegroup_hdl%daddr, min_digits_)
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_get_filegroup_name(filegroup_hdl%daddr, name_, len(name_))
      ENDIF
      
      IF (PRESENT(name_suffix_)) THEN
        CALL cxios_get_filegroup_name_suffix(filegroup_hdl%daddr, name_suffix_, len(name_suffix_))
      ENDIF
      
      IF (PRESENT(output_freq_)) THEN
        CALL cxios_get_filegroup_output_freq(filegroup_hdl%daddr, output_freq_, len(output_freq_))
      ENDIF
      
      IF (PRESENT(output_level_)) THEN
        CALL cxios_get_filegroup_output_level(filegroup_hdl%daddr, output_level_)
      ENDIF
      
      IF (PRESENT(par_access_)) THEN
        CALL cxios_get_filegroup_par_access(filegroup_hdl%daddr, par_access_, len(par_access_))
      ENDIF
      
      IF (PRESENT(split_freq_)) THEN
        CALL cxios_get_filegroup_split_freq(filegroup_hdl%daddr, split_freq_, len(split_freq_))
      ENDIF
      
      IF (PRESENT(split_freq_format_)) THEN
        CALL cxios_get_filegroup_split_freq_format(filegroup_hdl%daddr, split_freq_format_, len(split_freq_format_))
      ENDIF
      
      IF (PRESENT(sync_freq_)) THEN
        CALL cxios_get_filegroup_sync_freq(filegroup_hdl%daddr, sync_freq_, len(sync_freq_))
      ENDIF
      
      IF (PRESENT(type_)) THEN
        CALL cxios_get_filegroup_type(filegroup_hdl%daddr, type_, len(type_))
      ENDIF
      
      
    
  END SUBROUTINE xios(get_filegroup_attr_hdl_)
  
  SUBROUTINE xios(is_defined_filegroup_attr)  &
    ( filegroup_id, description, enabled, group_ref, min_digits, name, name_suffix, output_freq  &
    , output_level, par_access, split_freq, split_freq_format, sync_freq, type )
    
    IMPLICIT NONE
      TYPE(txios(filegroup))  :: filegroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::filegroup_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: description
      LOGICAL(KIND=C_BOOL) :: description_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL(KIND=C_BOOL) :: enabled_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL(KIND=C_BOOL) :: group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: min_digits
      LOGICAL(KIND=C_BOOL) :: min_digits_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_suffix
      LOGICAL(KIND=C_BOOL) :: name_suffix_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: output_freq
      LOGICAL(KIND=C_BOOL) :: output_freq_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: output_level
      LOGICAL(KIND=C_BOOL) :: output_level_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: par_access
      LOGICAL(KIND=C_BOOL) :: par_access_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: split_freq
      LOGICAL(KIND=C_BOOL) :: split_freq_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: split_freq_format
      LOGICAL(KIND=C_BOOL) :: split_freq_format_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: sync_freq
      LOGICAL(KIND=C_BOOL) :: sync_freq_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp
      
      CALL xios(get_filegroup_handle)(filegroup_id,filegroup_hdl)
      CALL xios(is_defined_filegroup_attr_hdl_)   &
      ( filegroup_hdl, description, enabled, group_ref, min_digits, name, name_suffix, output_freq  &
      , output_level, par_access, split_freq, split_freq_format, sync_freq, type )
    
  END SUBROUTINE xios(is_defined_filegroup_attr)
  
  SUBROUTINE xios(is_defined_filegroup_attr_hdl)  &
    ( filegroup_hdl, description, enabled, group_ref, min_digits, name, name_suffix, output_freq  &
    , output_level, par_access, split_freq, split_freq_format, sync_freq, type )
    
    IMPLICIT NONE
      TYPE(txios(filegroup)) , INTENT(IN) :: filegroup_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: description
      LOGICAL(KIND=C_BOOL) :: description_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL(KIND=C_BOOL) :: enabled_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL(KIND=C_BOOL) :: group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: min_digits
      LOGICAL(KIND=C_BOOL) :: min_digits_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_suffix
      LOGICAL(KIND=C_BOOL) :: name_suffix_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: output_freq
      LOGICAL(KIND=C_BOOL) :: output_freq_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: output_level
      LOGICAL(KIND=C_BOOL) :: output_level_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: par_access
      LOGICAL(KIND=C_BOOL) :: par_access_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: split_freq
      LOGICAL(KIND=C_BOOL) :: split_freq_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: split_freq_format
      LOGICAL(KIND=C_BOOL) :: split_freq_format_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: sync_freq
      LOGICAL(KIND=C_BOOL) :: sync_freq_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp
      
      CALL xios(is_defined_filegroup_attr_hdl_)  &
      ( filegroup_hdl, description, enabled, group_ref, min_digits, name, name_suffix, output_freq  &
      , output_level, par_access, split_freq, split_freq_format, sync_freq, type )
    
  END SUBROUTINE xios(is_defined_filegroup_attr_hdl)
  
  SUBROUTINE xios(is_defined_filegroup_attr_hdl_)   &
    ( filegroup_hdl, description_, enabled_, group_ref_, min_digits_, name_, name_suffix_, output_freq_  &
    , output_level_, par_access_, split_freq_, split_freq_format_, sync_freq_, type_ )
    
    IMPLICIT NONE
      TYPE(txios(filegroup)) , INTENT(IN) :: filegroup_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: description_
      LOGICAL(KIND=C_BOOL) :: description__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: enabled_
      LOGICAL(KIND=C_BOOL) :: enabled__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref_
      LOGICAL(KIND=C_BOOL) :: group_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: min_digits_
      LOGICAL(KIND=C_BOOL) :: min_digits__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_
      LOGICAL(KIND=C_BOOL) :: name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_suffix_
      LOGICAL(KIND=C_BOOL) :: name_suffix__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: output_freq_
      LOGICAL(KIND=C_BOOL) :: output_freq__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: output_level_
      LOGICAL(KIND=C_BOOL) :: output_level__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: par_access_
      LOGICAL(KIND=C_BOOL) :: par_access__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: split_freq_
      LOGICAL(KIND=C_BOOL) :: split_freq__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: split_freq_format_
      LOGICAL(KIND=C_BOOL) :: split_freq_format__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: sync_freq_
      LOGICAL(KIND=C_BOOL) :: sync_freq__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type_
      LOGICAL(KIND=C_BOOL) :: type__tmp
      
      IF (PRESENT(description_)) THEN
        description__tmp=cxios_is_defined_filegroup_description(filegroup_hdl%daddr)
        description_=description__tmp
      ENDIF
      
      IF (PRESENT(enabled_)) THEN
        enabled__tmp=cxios_is_defined_filegroup_enabled(filegroup_hdl%daddr)
        enabled_=enabled__tmp
      ENDIF
      
      IF (PRESENT(group_ref_)) THEN
        group_ref__tmp=cxios_is_defined_filegroup_group_ref(filegroup_hdl%daddr)
        group_ref_=group_ref__tmp
      ENDIF
      
      IF (PRESENT(min_digits_)) THEN
        min_digits__tmp=cxios_is_defined_filegroup_min_digits(filegroup_hdl%daddr)
        min_digits_=min_digits__tmp
      ENDIF
      
      IF (PRESENT(name_)) THEN
        name__tmp=cxios_is_defined_filegroup_name(filegroup_hdl%daddr)
        name_=name__tmp
      ENDIF
      
      IF (PRESENT(name_suffix_)) THEN
        name_suffix__tmp=cxios_is_defined_filegroup_name_suffix(filegroup_hdl%daddr)
        name_suffix_=name_suffix__tmp
      ENDIF
      
      IF (PRESENT(output_freq_)) THEN
        output_freq__tmp=cxios_is_defined_filegroup_output_freq(filegroup_hdl%daddr)
        output_freq_=output_freq__tmp
      ENDIF
      
      IF (PRESENT(output_level_)) THEN
        output_level__tmp=cxios_is_defined_filegroup_output_level(filegroup_hdl%daddr)
        output_level_=output_level__tmp
      ENDIF
      
      IF (PRESENT(par_access_)) THEN
        par_access__tmp=cxios_is_defined_filegroup_par_access(filegroup_hdl%daddr)
        par_access_=par_access__tmp
      ENDIF
      
      IF (PRESENT(split_freq_)) THEN
        split_freq__tmp=cxios_is_defined_filegroup_split_freq(filegroup_hdl%daddr)
        split_freq_=split_freq__tmp
      ENDIF
      
      IF (PRESENT(split_freq_format_)) THEN
        split_freq_format__tmp=cxios_is_defined_filegroup_split_freq_format(filegroup_hdl%daddr)
        split_freq_format_=split_freq_format__tmp
      ENDIF
      
      IF (PRESENT(sync_freq_)) THEN
        sync_freq__tmp=cxios_is_defined_filegroup_sync_freq(filegroup_hdl%daddr)
        sync_freq_=sync_freq__tmp
      ENDIF
      
      IF (PRESENT(type_)) THEN
        type__tmp=cxios_is_defined_filegroup_type(filegroup_hdl%daddr)
        type_=type__tmp
      ENDIF
      
      
    
  END SUBROUTINE xios(is_defined_filegroup_attr_hdl_)
  
END MODULE ifilegroup_attr
