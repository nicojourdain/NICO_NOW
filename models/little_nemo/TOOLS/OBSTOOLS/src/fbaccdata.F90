MODULE fbaccdata
   USE fbacctype
   IMPLICIT NONE
   INTEGER, DIMENSION(:,:,:,:,:), ALLOCATABLE :: inum,inumov,inumbv
   INTEGER, DIMENSION(:,:,:,:), ALLOCATABLE :: inuma
   REAL, DIMENSION(:,:,:,:,:), ALLOCATABLE :: zbias,zrms,zsdev,zomean,zmmean,&
      & zoemea,zovmea,zbemea,zbvmea
   REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: zoamean
   INTEGER, PARAMETER :: maxvars = 10
   REAL, DIMENSION(maxvars) :: zcheck
   REAL, DIMENSION(maxvars) :: zhistmax, zhistmin, zhiststep
   TYPE(histtype), DIMENSION(maxvars) :: hist
   REAL, DIMENSION(maxvars) :: zxymax, zxymin, zxystep
   TYPE(xytype), DIMENSION(maxvars) :: xy
END MODULE fbaccdata

