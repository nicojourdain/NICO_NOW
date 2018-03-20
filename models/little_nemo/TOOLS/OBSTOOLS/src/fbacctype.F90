MODULE fbacctype
   IMPLICIT NONE
   TYPE histtype
      INTEGER :: npoints
      INTEGER, POINTER, DIMENSION(:,:,:,:,:) :: nhist
   END TYPE histtype
   TYPE xytype
      INTEGER :: npoints
      INTEGER, POINTER, DIMENSION(:,:,:,:,:,:) :: nxy
   END TYPE xytype
END MODULE fbacctype
