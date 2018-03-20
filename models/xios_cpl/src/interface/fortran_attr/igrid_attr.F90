! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE igrid_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE igrid
  USE grid_interface_attr
  
CONTAINS
  
  SUBROUTINE xios(set_grid_attr)  &
    ( grid_id, axis_ref, description, domain_ref, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(grid))  :: grid_hdl
      CHARACTER(LEN=*), INTENT(IN) ::grid_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      
      CALL xios(get_grid_handle)(grid_id,grid_hdl)
      CALL xios(set_grid_attr_hdl_)   &
      ( grid_hdl, axis_ref, description, domain_ref, mask, name )
    
  END SUBROUTINE xios(set_grid_attr)
  
  SUBROUTINE xios(set_grid_attr_hdl)  &
    ( grid_hdl, axis_ref, description, domain_ref, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      
      CALL xios(set_grid_attr_hdl_)  &
      ( grid_hdl, axis_ref, description, domain_ref, mask, name )
    
  END SUBROUTINE xios(set_grid_attr_hdl)
  
  SUBROUTINE xios(set_grid_attr_hdl_)   &
    ( grid_hdl, axis_ref_, description_, domain_ref_, mask_, name_ )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref_
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask_(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      
      IF (PRESENT(axis_ref_)) THEN
        CALL cxios_set_grid_axis_ref(grid_hdl%daddr, axis_ref_, len(axis_ref_))
      ENDIF
      
      IF (PRESENT(description_)) THEN
        CALL cxios_set_grid_description(grid_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(domain_ref_)) THEN
        CALL cxios_set_grid_domain_ref(grid_hdl%daddr, domain_ref_, len(domain_ref_))
      ENDIF
      
      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(size(mask_,1),size(mask_,2),size(mask_,3)))
        mask__tmp=mask_
        CALL cxios_set_grid_mask(grid_hdl%daddr, mask__tmp,size(mask_,1),size(mask_,2),size(mask_,3))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_set_grid_name(grid_hdl%daddr, name_, len(name_))
      ENDIF
      
      
    
  END SUBROUTINE xios(set_grid_attr_hdl_)
  
  SUBROUTINE xios(get_grid_attr)  &
    ( grid_id, axis_ref, description, domain_ref, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(grid))  :: grid_hdl
      CHARACTER(LEN=*), INTENT(IN) ::grid_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      
      CALL xios(get_grid_handle)(grid_id,grid_hdl)
      CALL xios(get_grid_attr_hdl_)   &
      ( grid_hdl, axis_ref, description, domain_ref, mask, name )
    
  END SUBROUTINE xios(get_grid_attr)
  
  SUBROUTINE xios(get_grid_attr_hdl)  &
    ( grid_hdl, axis_ref, description, domain_ref, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      
      CALL xios(get_grid_attr_hdl_)  &
      ( grid_hdl, axis_ref, description, domain_ref, mask, name )
    
  END SUBROUTINE xios(get_grid_attr_hdl)
  
  SUBROUTINE xios(get_grid_attr_hdl_)   &
    ( grid_hdl, axis_ref_, description_, domain_ref_, mask_, name_ )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask_(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      
      IF (PRESENT(axis_ref_)) THEN
        CALL cxios_get_grid_axis_ref(grid_hdl%daddr, axis_ref_, len(axis_ref_))
      ENDIF
      
      IF (PRESENT(description_)) THEN
        CALL cxios_get_grid_description(grid_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(domain_ref_)) THEN
        CALL cxios_get_grid_domain_ref(grid_hdl%daddr, domain_ref_, len(domain_ref_))
      ENDIF
      
      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(size(mask_,1),size(mask_,2),size(mask_,3)))
        CALL cxios_get_grid_mask(grid_hdl%daddr, mask__tmp,size(mask_,1),size(mask_,2),size(mask_,3))
        mask_=mask__tmp
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_get_grid_name(grid_hdl%daddr, name_, len(name_))
      ENDIF
      
      
    
  END SUBROUTINE xios(get_grid_attr_hdl_)
  
  SUBROUTINE xios(is_defined_grid_attr)  &
    ( grid_id, axis_ref, description, domain_ref, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(grid))  :: grid_hdl
      CHARACTER(LEN=*), INTENT(IN) ::grid_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_ref
      LOGICAL(KIND=C_BOOL) :: axis_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: description
      LOGICAL(KIND=C_BOOL) :: description_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL(KIND=C_BOOL) :: domain_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      
      CALL xios(get_grid_handle)(grid_id,grid_hdl)
      CALL xios(is_defined_grid_attr_hdl_)   &
      ( grid_hdl, axis_ref, description, domain_ref, mask, name )
    
  END SUBROUTINE xios(is_defined_grid_attr)
  
  SUBROUTINE xios(is_defined_grid_attr_hdl)  &
    ( grid_hdl, axis_ref, description, domain_ref, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_ref
      LOGICAL(KIND=C_BOOL) :: axis_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: description
      LOGICAL(KIND=C_BOOL) :: description_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL(KIND=C_BOOL) :: domain_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      
      CALL xios(is_defined_grid_attr_hdl_)  &
      ( grid_hdl, axis_ref, description, domain_ref, mask, name )
    
  END SUBROUTINE xios(is_defined_grid_attr_hdl)
  
  SUBROUTINE xios(is_defined_grid_attr_hdl_)   &
    ( grid_hdl, axis_ref_, description_, domain_ref_, mask_, name_ )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_ref_
      LOGICAL(KIND=C_BOOL) :: axis_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: description_
      LOGICAL(KIND=C_BOOL) :: description__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_ref_
      LOGICAL(KIND=C_BOOL) :: domain_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask_
      LOGICAL(KIND=C_BOOL) :: mask__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_
      LOGICAL(KIND=C_BOOL) :: name__tmp
      
      IF (PRESENT(axis_ref_)) THEN
        axis_ref__tmp=cxios_is_defined_grid_axis_ref(grid_hdl%daddr)
        axis_ref_=axis_ref__tmp
      ENDIF
      
      IF (PRESENT(description_)) THEN
        description__tmp=cxios_is_defined_grid_description(grid_hdl%daddr)
        description_=description__tmp
      ENDIF
      
      IF (PRESENT(domain_ref_)) THEN
        domain_ref__tmp=cxios_is_defined_grid_domain_ref(grid_hdl%daddr)
        domain_ref_=domain_ref__tmp
      ENDIF
      
      IF (PRESENT(mask_)) THEN
        mask__tmp=cxios_is_defined_grid_mask(grid_hdl%daddr)
        mask_=mask__tmp
      ENDIF
      
      IF (PRESENT(name_)) THEN
        name__tmp=cxios_is_defined_grid_name(grid_hdl%daddr)
        name_=name__tmp
      ENDIF
      
      
    
  END SUBROUTINE xios(is_defined_grid_attr_hdl_)
  
END MODULE igrid_attr
