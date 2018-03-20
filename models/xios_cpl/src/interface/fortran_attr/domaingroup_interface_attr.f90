! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *

MODULE domaingroup_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING
  
  INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99
    
    
    SUBROUTINE cxios_set_domaingroup_data_dim(domaingroup_hdl, data_dim) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: data_dim
    END SUBROUTINE cxios_set_domaingroup_data_dim
    
    SUBROUTINE cxios_get_domaingroup_data_dim(domaingroup_hdl, data_dim) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: data_dim
    END SUBROUTINE cxios_get_domaingroup_data_dim
    
    FUNCTION cxios_is_defined_domaingroup_data_dim(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_data_dim
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_data_dim
    
    
    SUBROUTINE cxios_set_domaingroup_data_i_index(domaingroup_hdl, data_i_index, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domaingroup_hdl
      INTEGER (KIND=C_INT)     , DIMENSION(*) :: data_i_index
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_set_domaingroup_data_i_index
    
    SUBROUTINE cxios_get_domaingroup_data_i_index(domaingroup_hdl, data_i_index, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domaingroup_hdl
      INTEGER (KIND=C_INT)     , DIMENSION(*) :: data_i_index
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_get_domaingroup_data_i_index
    
    FUNCTION cxios_is_defined_domaingroup_data_i_index(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_data_i_index
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_data_i_index
    
    
    SUBROUTINE cxios_set_domaingroup_data_ibegin(domaingroup_hdl, data_ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: data_ibegin
    END SUBROUTINE cxios_set_domaingroup_data_ibegin
    
    SUBROUTINE cxios_get_domaingroup_data_ibegin(domaingroup_hdl, data_ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: data_ibegin
    END SUBROUTINE cxios_get_domaingroup_data_ibegin
    
    FUNCTION cxios_is_defined_domaingroup_data_ibegin(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_data_ibegin
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_data_ibegin
    
    
    SUBROUTINE cxios_set_domaingroup_data_j_index(domaingroup_hdl, data_j_index, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domaingroup_hdl
      INTEGER (KIND=C_INT)     , DIMENSION(*) :: data_j_index
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_set_domaingroup_data_j_index
    
    SUBROUTINE cxios_get_domaingroup_data_j_index(domaingroup_hdl, data_j_index, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domaingroup_hdl
      INTEGER (KIND=C_INT)     , DIMENSION(*) :: data_j_index
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_get_domaingroup_data_j_index
    
    FUNCTION cxios_is_defined_domaingroup_data_j_index(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_data_j_index
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_data_j_index
    
    
    SUBROUTINE cxios_set_domaingroup_data_jbegin(domaingroup_hdl, data_jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: data_jbegin
    END SUBROUTINE cxios_set_domaingroup_data_jbegin
    
    SUBROUTINE cxios_get_domaingroup_data_jbegin(domaingroup_hdl, data_jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: data_jbegin
    END SUBROUTINE cxios_get_domaingroup_data_jbegin
    
    FUNCTION cxios_is_defined_domaingroup_data_jbegin(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_data_jbegin
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_data_jbegin
    
    
    SUBROUTINE cxios_set_domaingroup_data_n_index(domaingroup_hdl, data_n_index) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: data_n_index
    END SUBROUTINE cxios_set_domaingroup_data_n_index
    
    SUBROUTINE cxios_get_domaingroup_data_n_index(domaingroup_hdl, data_n_index) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: data_n_index
    END SUBROUTINE cxios_get_domaingroup_data_n_index
    
    FUNCTION cxios_is_defined_domaingroup_data_n_index(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_data_n_index
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_data_n_index
    
    
    SUBROUTINE cxios_set_domaingroup_data_ni(domaingroup_hdl, data_ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: data_ni
    END SUBROUTINE cxios_set_domaingroup_data_ni
    
    SUBROUTINE cxios_get_domaingroup_data_ni(domaingroup_hdl, data_ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: data_ni
    END SUBROUTINE cxios_get_domaingroup_data_ni
    
    FUNCTION cxios_is_defined_domaingroup_data_ni(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_data_ni
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_data_ni
    
    
    SUBROUTINE cxios_set_domaingroup_data_nj(domaingroup_hdl, data_nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: data_nj
    END SUBROUTINE cxios_set_domaingroup_data_nj
    
    SUBROUTINE cxios_get_domaingroup_data_nj(domaingroup_hdl, data_nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: data_nj
    END SUBROUTINE cxios_get_domaingroup_data_nj
    
    FUNCTION cxios_is_defined_domaingroup_data_nj(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_data_nj
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_data_nj
    
    
    SUBROUTINE cxios_set_domaingroup_domain_group_ref(domaingroup_hdl, domain_group_ref, domain_group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: domain_group_ref_size
    END SUBROUTINE cxios_set_domaingroup_domain_group_ref
    
    SUBROUTINE cxios_get_domaingroup_domain_group_ref(domaingroup_hdl, domain_group_ref, domain_group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: domain_group_ref_size
    END SUBROUTINE cxios_get_domaingroup_domain_group_ref
    
    FUNCTION cxios_is_defined_domaingroup_domain_group_ref(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_domain_group_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_domain_group_ref
    
    
    SUBROUTINE cxios_set_domaingroup_group_ref(domaingroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_set_domaingroup_group_ref
    
    SUBROUTINE cxios_get_domaingroup_group_ref(domaingroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_get_domaingroup_group_ref
    
    FUNCTION cxios_is_defined_domaingroup_group_ref(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_group_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_group_ref
    
    
    SUBROUTINE cxios_set_domaingroup_ibegin(domaingroup_hdl, ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: ibegin
    END SUBROUTINE cxios_set_domaingroup_ibegin
    
    SUBROUTINE cxios_get_domaingroup_ibegin(domaingroup_hdl, ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: ibegin
    END SUBROUTINE cxios_get_domaingroup_ibegin
    
    FUNCTION cxios_is_defined_domaingroup_ibegin(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_ibegin
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_ibegin
    
    
    SUBROUTINE cxios_set_domaingroup_iend(domaingroup_hdl, iend) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: iend
    END SUBROUTINE cxios_set_domaingroup_iend
    
    SUBROUTINE cxios_get_domaingroup_iend(domaingroup_hdl, iend) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: iend
    END SUBROUTINE cxios_get_domaingroup_iend
    
    FUNCTION cxios_is_defined_domaingroup_iend(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_iend
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_iend
    
    
    SUBROUTINE cxios_set_domaingroup_jbegin(domaingroup_hdl, jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: jbegin
    END SUBROUTINE cxios_set_domaingroup_jbegin
    
    SUBROUTINE cxios_get_domaingroup_jbegin(domaingroup_hdl, jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: jbegin
    END SUBROUTINE cxios_get_domaingroup_jbegin
    
    FUNCTION cxios_is_defined_domaingroup_jbegin(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_jbegin
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_jbegin
    
    
    SUBROUTINE cxios_set_domaingroup_jend(domaingroup_hdl, jend) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: jend
    END SUBROUTINE cxios_set_domaingroup_jend
    
    SUBROUTINE cxios_get_domaingroup_jend(domaingroup_hdl, jend) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: jend
    END SUBROUTINE cxios_get_domaingroup_jend
    
    FUNCTION cxios_is_defined_domaingroup_jend(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_jend
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_jend
    
    
    SUBROUTINE cxios_set_domaingroup_latvalue(domaingroup_hdl, latvalue, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domaingroup_hdl
      REAL (KIND=C_DOUBLE)     , DIMENSION(*) :: latvalue
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_set_domaingroup_latvalue
    
    SUBROUTINE cxios_get_domaingroup_latvalue(domaingroup_hdl, latvalue, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domaingroup_hdl
      REAL (KIND=C_DOUBLE)     , DIMENSION(*) :: latvalue
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_get_domaingroup_latvalue
    
    FUNCTION cxios_is_defined_domaingroup_latvalue(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_latvalue
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_latvalue
    
    
    SUBROUTINE cxios_set_domaingroup_long_name(domaingroup_hdl, long_name, long_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
      INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
    END SUBROUTINE cxios_set_domaingroup_long_name
    
    SUBROUTINE cxios_get_domaingroup_long_name(domaingroup_hdl, long_name, long_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
      INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
    END SUBROUTINE cxios_get_domaingroup_long_name
    
    FUNCTION cxios_is_defined_domaingroup_long_name(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_long_name
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_long_name
    
    
    SUBROUTINE cxios_set_domaingroup_lonvalue(domaingroup_hdl, lonvalue, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domaingroup_hdl
      REAL (KIND=C_DOUBLE)     , DIMENSION(*) :: lonvalue
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_set_domaingroup_lonvalue
    
    SUBROUTINE cxios_get_domaingroup_lonvalue(domaingroup_hdl, lonvalue, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domaingroup_hdl
      REAL (KIND=C_DOUBLE)     , DIMENSION(*) :: lonvalue
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_get_domaingroup_lonvalue
    
    FUNCTION cxios_is_defined_domaingroup_lonvalue(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_lonvalue
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_lonvalue
    
    
    SUBROUTINE cxios_set_domaingroup_mask(domaingroup_hdl, mask, extent1, extent2) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domaingroup_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
    END SUBROUTINE cxios_set_domaingroup_mask
    
    SUBROUTINE cxios_get_domaingroup_mask(domaingroup_hdl, mask, extent1, extent2) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: domaingroup_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
    END SUBROUTINE cxios_get_domaingroup_mask
    
    FUNCTION cxios_is_defined_domaingroup_mask(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_mask
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_mask
    
    
    SUBROUTINE cxios_set_domaingroup_name(domaingroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_set_domaingroup_name
    
    SUBROUTINE cxios_get_domaingroup_name(domaingroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_get_domaingroup_name
    
    FUNCTION cxios_is_defined_domaingroup_name(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_name
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_name
    
    
    SUBROUTINE cxios_set_domaingroup_ni(domaingroup_hdl, ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: ni
    END SUBROUTINE cxios_set_domaingroup_ni
    
    SUBROUTINE cxios_get_domaingroup_ni(domaingroup_hdl, ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: ni
    END SUBROUTINE cxios_get_domaingroup_ni
    
    FUNCTION cxios_is_defined_domaingroup_ni(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_ni
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_ni
    
    
    SUBROUTINE cxios_set_domaingroup_ni_glo(domaingroup_hdl, ni_glo) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: ni_glo
    END SUBROUTINE cxios_set_domaingroup_ni_glo
    
    SUBROUTINE cxios_get_domaingroup_ni_glo(domaingroup_hdl, ni_glo) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: ni_glo
    END SUBROUTINE cxios_get_domaingroup_ni_glo
    
    FUNCTION cxios_is_defined_domaingroup_ni_glo(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_ni_glo
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_ni_glo
    
    
    SUBROUTINE cxios_set_domaingroup_nj(domaingroup_hdl, nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: nj
    END SUBROUTINE cxios_set_domaingroup_nj
    
    SUBROUTINE cxios_get_domaingroup_nj(domaingroup_hdl, nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: nj
    END SUBROUTINE cxios_get_domaingroup_nj
    
    FUNCTION cxios_is_defined_domaingroup_nj(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_nj
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_nj
    
    
    SUBROUTINE cxios_set_domaingroup_nj_glo(domaingroup_hdl, nj_glo) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: nj_glo
    END SUBROUTINE cxios_set_domaingroup_nj_glo
    
    SUBROUTINE cxios_get_domaingroup_nj_glo(domaingroup_hdl, nj_glo) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: nj_glo
    END SUBROUTINE cxios_get_domaingroup_nj_glo
    
    FUNCTION cxios_is_defined_domaingroup_nj_glo(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_nj_glo
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_nj_glo
    
    
    SUBROUTINE cxios_set_domaingroup_standard_name(domaingroup_hdl, standard_name, standard_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
      INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
    END SUBROUTINE cxios_set_domaingroup_standard_name
    
    SUBROUTINE cxios_get_domaingroup_standard_name(domaingroup_hdl, standard_name, standard_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
      INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
    END SUBROUTINE cxios_get_domaingroup_standard_name
    
    FUNCTION cxios_is_defined_domaingroup_standard_name(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_standard_name
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_standard_name
    
    
    SUBROUTINE cxios_set_domaingroup_zoom_ibegin(domaingroup_hdl, zoom_ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_ibegin
    END SUBROUTINE cxios_set_domaingroup_zoom_ibegin
    
    SUBROUTINE cxios_get_domaingroup_zoom_ibegin(domaingroup_hdl, zoom_ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: zoom_ibegin
    END SUBROUTINE cxios_get_domaingroup_zoom_ibegin
    
    FUNCTION cxios_is_defined_domaingroup_zoom_ibegin(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_zoom_ibegin
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_zoom_ibegin
    
    
    SUBROUTINE cxios_set_domaingroup_zoom_ibegin_loc(domaingroup_hdl, zoom_ibegin_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_ibegin_loc
    END SUBROUTINE cxios_set_domaingroup_zoom_ibegin_loc
    
    SUBROUTINE cxios_get_domaingroup_zoom_ibegin_loc(domaingroup_hdl, zoom_ibegin_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: zoom_ibegin_loc
    END SUBROUTINE cxios_get_domaingroup_zoom_ibegin_loc
    
    FUNCTION cxios_is_defined_domaingroup_zoom_ibegin_loc(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_zoom_ibegin_loc
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_zoom_ibegin_loc
    
    
    SUBROUTINE cxios_set_domaingroup_zoom_jbegin(domaingroup_hdl, zoom_jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_jbegin
    END SUBROUTINE cxios_set_domaingroup_zoom_jbegin
    
    SUBROUTINE cxios_get_domaingroup_zoom_jbegin(domaingroup_hdl, zoom_jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: zoom_jbegin
    END SUBROUTINE cxios_get_domaingroup_zoom_jbegin
    
    FUNCTION cxios_is_defined_domaingroup_zoom_jbegin(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_zoom_jbegin
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_zoom_jbegin
    
    
    SUBROUTINE cxios_set_domaingroup_zoom_jbegin_loc(domaingroup_hdl, zoom_jbegin_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_jbegin_loc
    END SUBROUTINE cxios_set_domaingroup_zoom_jbegin_loc
    
    SUBROUTINE cxios_get_domaingroup_zoom_jbegin_loc(domaingroup_hdl, zoom_jbegin_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: zoom_jbegin_loc
    END SUBROUTINE cxios_get_domaingroup_zoom_jbegin_loc
    
    FUNCTION cxios_is_defined_domaingroup_zoom_jbegin_loc(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_zoom_jbegin_loc
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_zoom_jbegin_loc
    
    
    SUBROUTINE cxios_set_domaingroup_zoom_ni(domaingroup_hdl, zoom_ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_ni
    END SUBROUTINE cxios_set_domaingroup_zoom_ni
    
    SUBROUTINE cxios_get_domaingroup_zoom_ni(domaingroup_hdl, zoom_ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: zoom_ni
    END SUBROUTINE cxios_get_domaingroup_zoom_ni
    
    FUNCTION cxios_is_defined_domaingroup_zoom_ni(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_zoom_ni
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_zoom_ni
    
    
    SUBROUTINE cxios_set_domaingroup_zoom_ni_loc(domaingroup_hdl, zoom_ni_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_ni_loc
    END SUBROUTINE cxios_set_domaingroup_zoom_ni_loc
    
    SUBROUTINE cxios_get_domaingroup_zoom_ni_loc(domaingroup_hdl, zoom_ni_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: zoom_ni_loc
    END SUBROUTINE cxios_get_domaingroup_zoom_ni_loc
    
    FUNCTION cxios_is_defined_domaingroup_zoom_ni_loc(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_zoom_ni_loc
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_zoom_ni_loc
    
    
    SUBROUTINE cxios_set_domaingroup_zoom_nj(domaingroup_hdl, zoom_nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_nj
    END SUBROUTINE cxios_set_domaingroup_zoom_nj
    
    SUBROUTINE cxios_get_domaingroup_zoom_nj(domaingroup_hdl, zoom_nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: zoom_nj
    END SUBROUTINE cxios_get_domaingroup_zoom_nj
    
    FUNCTION cxios_is_defined_domaingroup_zoom_nj(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_zoom_nj
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_zoom_nj
    
    
    SUBROUTINE cxios_set_domaingroup_zoom_nj_loc(domaingroup_hdl, zoom_nj_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_nj_loc
    END SUBROUTINE cxios_set_domaingroup_zoom_nj_loc
    
    SUBROUTINE cxios_get_domaingroup_zoom_nj_loc(domaingroup_hdl, zoom_nj_loc) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
      INTEGER (KIND=C_INT)             :: zoom_nj_loc
    END SUBROUTINE cxios_get_domaingroup_zoom_nj_loc
    
    FUNCTION cxios_is_defined_domaingroup_zoom_nj_loc(domaingroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_domaingroup_zoom_nj_loc
      INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
    END FUNCTION cxios_is_defined_domaingroup_zoom_nj_loc
    
    
    END INTERFACE
  
END MODULE domaingroup_interface_attr
