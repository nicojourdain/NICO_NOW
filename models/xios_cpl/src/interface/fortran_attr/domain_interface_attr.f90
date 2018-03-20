! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *

MODULE domain_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING
  
  INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99
    
    
    SUBROUTINE cxios_set_domain_data_dim(domain_hdl, data_dim) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: data_dim
    END SUBROUTINE cxios_set_domain_data_dim
    
    SUBROUTINE cxios_get_domain_data_dim(domain_hdl, data_dim) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: data_dim
    END SUBROUTINE cxios_get_domain_data_dim
    
    FUNCTION cxios_is_defined_domain_data_dim(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_data_dim
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_data_dim
    
    
    SUBROUTINE cxios_set_domain_data_i_index(domain_hdl, data_i_index, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domain_hdl
      INTEGER (KIND=C_INT)     , DIMENSION(*) :: data_i_index
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_set_domain_data_i_index
    
    SUBROUTINE cxios_get_domain_data_i_index(domain_hdl, data_i_index, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domain_hdl
      INTEGER (KIND=C_INT)     , DIMENSION(*) :: data_i_index
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_get_domain_data_i_index
    
    FUNCTION cxios_is_defined_domain_data_i_index(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_data_i_index
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_data_i_index
    
    
    SUBROUTINE cxios_set_domain_data_ibegin(domain_hdl, data_ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: data_ibegin
    END SUBROUTINE cxios_set_domain_data_ibegin
    
    SUBROUTINE cxios_get_domain_data_ibegin(domain_hdl, data_ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: data_ibegin
    END SUBROUTINE cxios_get_domain_data_ibegin
    
    FUNCTION cxios_is_defined_domain_data_ibegin(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_data_ibegin
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_data_ibegin
    
    
    SUBROUTINE cxios_set_domain_data_j_index(domain_hdl, data_j_index, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domain_hdl
      INTEGER (KIND=C_INT)     , DIMENSION(*) :: data_j_index
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_set_domain_data_j_index
    
    SUBROUTINE cxios_get_domain_data_j_index(domain_hdl, data_j_index, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domain_hdl
      INTEGER (KIND=C_INT)     , DIMENSION(*) :: data_j_index
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_get_domain_data_j_index
    
    FUNCTION cxios_is_defined_domain_data_j_index(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_data_j_index
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_data_j_index
    
    
    SUBROUTINE cxios_set_domain_data_jbegin(domain_hdl, data_jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: data_jbegin
    END SUBROUTINE cxios_set_domain_data_jbegin
    
    SUBROUTINE cxios_get_domain_data_jbegin(domain_hdl, data_jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: data_jbegin
    END SUBROUTINE cxios_get_domain_data_jbegin
    
    FUNCTION cxios_is_defined_domain_data_jbegin(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_data_jbegin
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_data_jbegin
    
    
    SUBROUTINE cxios_set_domain_data_n_index(domain_hdl, data_n_index) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: data_n_index
    END SUBROUTINE cxios_set_domain_data_n_index
    
    SUBROUTINE cxios_get_domain_data_n_index(domain_hdl, data_n_index) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: data_n_index
    END SUBROUTINE cxios_get_domain_data_n_index
    
    FUNCTION cxios_is_defined_domain_data_n_index(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_data_n_index
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_data_n_index
    
    
    SUBROUTINE cxios_set_domain_data_ni(domain_hdl, data_ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: data_ni
    END SUBROUTINE cxios_set_domain_data_ni
    
    SUBROUTINE cxios_get_domain_data_ni(domain_hdl, data_ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: data_ni
    END SUBROUTINE cxios_get_domain_data_ni
    
    FUNCTION cxios_is_defined_domain_data_ni(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_data_ni
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_data_ni
    
    
    SUBROUTINE cxios_set_domain_data_nj(domain_hdl, data_nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: data_nj
    END SUBROUTINE cxios_set_domain_data_nj
    
    SUBROUTINE cxios_get_domain_data_nj(domain_hdl, data_nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: data_nj
    END SUBROUTINE cxios_get_domain_data_nj
    
    FUNCTION cxios_is_defined_domain_data_nj(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_data_nj
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_data_nj
    
    
    SUBROUTINE cxios_set_domain_domain_group_ref(domain_hdl, domain_group_ref, domain_group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: domain_group_ref_size
    END SUBROUTINE cxios_set_domain_domain_group_ref
    
    SUBROUTINE cxios_get_domain_domain_group_ref(domain_hdl, domain_group_ref, domain_group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: domain_group_ref_size
    END SUBROUTINE cxios_get_domain_domain_group_ref
    
    FUNCTION cxios_is_defined_domain_domain_group_ref(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_domain_group_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_domain_group_ref
    
    
    SUBROUTINE cxios_set_domain_ibegin(domain_hdl, ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: ibegin
    END SUBROUTINE cxios_set_domain_ibegin
    
    SUBROUTINE cxios_get_domain_ibegin(domain_hdl, ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: ibegin
    END SUBROUTINE cxios_get_domain_ibegin
    
    FUNCTION cxios_is_defined_domain_ibegin(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_ibegin
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_ibegin
    
    
    SUBROUTINE cxios_set_domain_iend(domain_hdl, iend) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: iend
    END SUBROUTINE cxios_set_domain_iend
    
    SUBROUTINE cxios_get_domain_iend(domain_hdl, iend) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: iend
    END SUBROUTINE cxios_get_domain_iend
    
    FUNCTION cxios_is_defined_domain_iend(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_iend
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_iend
    
    
    SUBROUTINE cxios_set_domain_jbegin(domain_hdl, jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: jbegin
    END SUBROUTINE cxios_set_domain_jbegin
    
    SUBROUTINE cxios_get_domain_jbegin(domain_hdl, jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: jbegin
    END SUBROUTINE cxios_get_domain_jbegin
    
    FUNCTION cxios_is_defined_domain_jbegin(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_jbegin
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_jbegin
    
    
    SUBROUTINE cxios_set_domain_jend(domain_hdl, jend) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: jend
    END SUBROUTINE cxios_set_domain_jend
    
    SUBROUTINE cxios_get_domain_jend(domain_hdl, jend) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: jend
    END SUBROUTINE cxios_get_domain_jend
    
    FUNCTION cxios_is_defined_domain_jend(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_jend
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_jend
    
    
    SUBROUTINE cxios_set_domain_latvalue(domain_hdl, latvalue, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domain_hdl
      REAL (KIND=C_DOUBLE)     , DIMENSION(*) :: latvalue
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_set_domain_latvalue
    
    SUBROUTINE cxios_get_domain_latvalue(domain_hdl, latvalue, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domain_hdl
      REAL (KIND=C_DOUBLE)     , DIMENSION(*) :: latvalue
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_get_domain_latvalue
    
    FUNCTION cxios_is_defined_domain_latvalue(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_latvalue
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_latvalue
    
    
    SUBROUTINE cxios_set_domain_long_name(domain_hdl, long_name, long_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
      INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
    END SUBROUTINE cxios_set_domain_long_name
    
    SUBROUTINE cxios_get_domain_long_name(domain_hdl, long_name, long_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
      INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
    END SUBROUTINE cxios_get_domain_long_name
    
    FUNCTION cxios_is_defined_domain_long_name(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_long_name
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_long_name
    
    
    SUBROUTINE cxios_set_domain_lonvalue(domain_hdl, lonvalue, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domain_hdl
      REAL (KIND=C_DOUBLE)     , DIMENSION(*) :: lonvalue
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_set_domain_lonvalue
    
    SUBROUTINE cxios_get_domain_lonvalue(domain_hdl, lonvalue, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domain_hdl
      REAL (KIND=C_DOUBLE)     , DIMENSION(*) :: lonvalue
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_get_domain_lonvalue
    
    FUNCTION cxios_is_defined_domain_lonvalue(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_lonvalue
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_lonvalue
    
    
    SUBROUTINE cxios_set_domain_mask(domain_hdl, mask, extent1, extent2) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domain_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
    END SUBROUTINE cxios_set_domain_mask
    
    SUBROUTINE cxios_get_domain_mask(domain_hdl, mask, extent1, extent2) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domain_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
    END SUBROUTINE cxios_get_domain_mask
    
    FUNCTION cxios_is_defined_domain_mask(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_mask
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_mask
    
    
    SUBROUTINE cxios_set_domain_name(domain_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_set_domain_name
    
    SUBROUTINE cxios_get_domain_name(domain_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_get_domain_name
    
    FUNCTION cxios_is_defined_domain_name(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_name
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_name
    
    
    SUBROUTINE cxios_set_domain_ni(domain_hdl, ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: ni
    END SUBROUTINE cxios_set_domain_ni
    
    SUBROUTINE cxios_get_domain_ni(domain_hdl, ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: ni
    END SUBROUTINE cxios_get_domain_ni
    
    FUNCTION cxios_is_defined_domain_ni(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_ni
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_ni
    
    
    SUBROUTINE cxios_set_domain_ni_glo(domain_hdl, ni_glo) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: ni_glo
    END SUBROUTINE cxios_set_domain_ni_glo
    
    SUBROUTINE cxios_get_domain_ni_glo(domain_hdl, ni_glo) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: ni_glo
    END SUBROUTINE cxios_get_domain_ni_glo
    
    FUNCTION cxios_is_defined_domain_ni_glo(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_ni_glo
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_ni_glo
    
    
    SUBROUTINE cxios_set_domain_nj(domain_hdl, nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: nj
    END SUBROUTINE cxios_set_domain_nj
    
    SUBROUTINE cxios_get_domain_nj(domain_hdl, nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: nj
    END SUBROUTINE cxios_get_domain_nj
    
    FUNCTION cxios_is_defined_domain_nj(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_nj
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_nj
    
    
    SUBROUTINE cxios_set_domain_nj_glo(domain_hdl, nj_glo) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: nj_glo
    END SUBROUTINE cxios_set_domain_nj_glo
    
    SUBROUTINE cxios_get_domain_nj_glo(domain_hdl, nj_glo) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: nj_glo
    END SUBROUTINE cxios_get_domain_nj_glo
    
    FUNCTION cxios_is_defined_domain_nj_glo(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_nj_glo
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_nj_glo
    
    
    SUBROUTINE cxios_set_domain_standard_name(domain_hdl, standard_name, standard_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
      INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
    END SUBROUTINE cxios_set_domain_standard_name
    
    SUBROUTINE cxios_get_domain_standard_name(domain_hdl, standard_name, standard_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
      INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
    END SUBROUTINE cxios_get_domain_standard_name
    
    FUNCTION cxios_is_defined_domain_standard_name(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_standard_name
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_standard_name
    
    
    SUBROUTINE cxios_set_domain_zoom_ibegin(domain_hdl, zoom_ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_ibegin
    END SUBROUTINE cxios_set_domain_zoom_ibegin
    
    SUBROUTINE cxios_get_domain_zoom_ibegin(domain_hdl, zoom_ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: zoom_ibegin
    END SUBROUTINE cxios_get_domain_zoom_ibegin
    
    FUNCTION cxios_is_defined_domain_zoom_ibegin(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_zoom_ibegin
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_zoom_ibegin
    
    
    SUBROUTINE cxios_set_domain_zoom_ibegin_loc(domain_hdl, zoom_ibegin_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_ibegin_loc
    END SUBROUTINE cxios_set_domain_zoom_ibegin_loc
    
    SUBROUTINE cxios_get_domain_zoom_ibegin_loc(domain_hdl, zoom_ibegin_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: zoom_ibegin_loc
    END SUBROUTINE cxios_get_domain_zoom_ibegin_loc
    
    FUNCTION cxios_is_defined_domain_zoom_ibegin_loc(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_zoom_ibegin_loc
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_zoom_ibegin_loc
    
    
    SUBROUTINE cxios_set_domain_zoom_jbegin(domain_hdl, zoom_jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_jbegin
    END SUBROUTINE cxios_set_domain_zoom_jbegin
    
    SUBROUTINE cxios_get_domain_zoom_jbegin(domain_hdl, zoom_jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: zoom_jbegin
    END SUBROUTINE cxios_get_domain_zoom_jbegin
    
    FUNCTION cxios_is_defined_domain_zoom_jbegin(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_zoom_jbegin
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_zoom_jbegin
    
    
    SUBROUTINE cxios_set_domain_zoom_jbegin_loc(domain_hdl, zoom_jbegin_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_jbegin_loc
    END SUBROUTINE cxios_set_domain_zoom_jbegin_loc
    
    SUBROUTINE cxios_get_domain_zoom_jbegin_loc(domain_hdl, zoom_jbegin_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: zoom_jbegin_loc
    END SUBROUTINE cxios_get_domain_zoom_jbegin_loc
    
    FUNCTION cxios_is_defined_domain_zoom_jbegin_loc(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_zoom_jbegin_loc
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_zoom_jbegin_loc
    
    
    SUBROUTINE cxios_set_domain_zoom_ni(domain_hdl, zoom_ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_ni
    END SUBROUTINE cxios_set_domain_zoom_ni
    
    SUBROUTINE cxios_get_domain_zoom_ni(domain_hdl, zoom_ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: zoom_ni
    END SUBROUTINE cxios_get_domain_zoom_ni
    
    FUNCTION cxios_is_defined_domain_zoom_ni(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_zoom_ni
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_zoom_ni
    
    
    SUBROUTINE cxios_set_domain_zoom_ni_loc(domain_hdl, zoom_ni_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_ni_loc
    END SUBROUTINE cxios_set_domain_zoom_ni_loc
    
    SUBROUTINE cxios_get_domain_zoom_ni_loc(domain_hdl, zoom_ni_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: zoom_ni_loc
    END SUBROUTINE cxios_get_domain_zoom_ni_loc
    
    FUNCTION cxios_is_defined_domain_zoom_ni_loc(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_zoom_ni_loc
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_zoom_ni_loc
    
    
    SUBROUTINE cxios_set_domain_zoom_nj(domain_hdl, zoom_nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_nj
    END SUBROUTINE cxios_set_domain_zoom_nj
    
    SUBROUTINE cxios_get_domain_zoom_nj(domain_hdl, zoom_nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: zoom_nj
    END SUBROUTINE cxios_get_domain_zoom_nj
    
    FUNCTION cxios_is_defined_domain_zoom_nj(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_zoom_nj
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_zoom_nj
    
    
    SUBROUTINE cxios_set_domain_zoom_nj_loc(domain_hdl, zoom_nj_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_nj_loc
    END SUBROUTINE cxios_set_domain_zoom_nj_loc
    
    SUBROUTINE cxios_get_domain_zoom_nj_loc(domain_hdl, zoom_nj_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
      INTEGER (KIND=C_INT)             :: zoom_nj_loc
    END SUBROUTINE cxios_get_domain_zoom_nj_loc
    
    FUNCTION cxios_is_defined_domain_zoom_nj_loc(domain_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domain_zoom_nj_loc
      INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
    END FUNCTION cxios_is_defined_domain_zoom_nj_loc
    
    
    END INTERFACE
  
END MODULE domain_interface_attr
