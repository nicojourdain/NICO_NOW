!************************************************************************
! Fortran 95 OPA Nesting tools						*
!									*
!     Copyright (C) 2005 Florian Lemari�(Florian.Lemarie@imag.fr)	*
!									*
!************************************************************************
!
!********************************************************************************
!										*
! module io_netcdf								*
!										*
! NetCDF Fortran 90 read/write interface					*
! using input/output functions provided						*
! by unidata									*
!										*
!http://my.unidata.ucar.edu/content/software/netcdf/docs/netcdf-f90/index.html	*
!										*
!********************************************************************************
!
!
!
MODULE io_netcdf
  !      
  USE netcdf
  USE agrif_types
  !      
  INTERFACE Read_Ncdf_var
     MODULE PROCEDURE Read_Ncdf_var1d_Real,   &
          Read_Ncdf_var2d_Real,   &
          Read_Ncdf_var2d_Real_bis,   &
          Read_Ncdf_var3d_Real,   &
          Read_Ncdf_var4d_Real,   &
          Read_Ncdf_var3d_Real_t, &
          Read_Ncdf_var4d_Real_t, &
          Read_Ncdf_var4d_Real_nt, &
          Read_Ncdf_var1d_Int,    &
          Read_Ncdf_var2d_Int,    &
          Read_Ncdf_var3d_Int,    &
          Read_Ncdf_var4d_Int,    &
          Read_Ncdf_var0d_Int,    &
          Read_Ncdf_var0d_Real
  END INTERFACE
  !
  INTERFACE Write_Ncdf_var
     MODULE PROCEDURE Write_Ncdf_var1d_Real,   &
          Write_Ncdf_var2d_Real,   &
          Write_Ncdf_var3d_Real,   &
          Write_Ncdf_var4d_Real,   &
          Write_Ncdf_var3d_Real_t, &
          Write_Ncdf_var4d_Real_t, &
          Write_Ncdf_var4d_Real_nt, &                           
          Write_Ncdf_var2d_Real_bis,&			   
          Write_Ncdf_var1d_Int,    &
          Write_Ncdf_var2d_Int,    &
          Write_Ncdf_var3d_Int,    &
          Write_Ncdf_var4d_Int,    &
          Write_Ncdf_var0d_Real
  END INTERFACE
  ! 
  INTERFACE Copy_Ncdf_att
     MODULE PROCEDURE Copy_Ncdf_att_latlon,Copy_Ncdf_att_var
  END INTERFACE
  !
CONTAINS
  !
  !****************************************************************
  !   subroutine Read_Ncdf_dim					*
  !								*
  ! subroutine to retrieve value of a given dimension 		*
  !								*
  !     dimname : name of dimension to retrieve			*
  !     file    : netcdf file name				*
  !     dimval  : value of the required dimension			*
  !								*
  !****************************************************************
  !
  SUBROUTINE Read_Ncdf_dim(dimname,file,dimval)   
    !      
    IMPLICIT NONE
    !           
    CHARACTER(*),INTENT(in) :: dimname,file    
    INTEGER    :: dimval
    !            
    ! local variables
    !
    INTEGER ncid,status,dimid
    !     
    status = nf90_open(file,NF90_NOWRITE,ncid)
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_dimid(ncid,dimname,dimid)
    status = nf90_inquire_dimension(ncid,dimid,len=dimval)						                
    !      
    status = nf90_close(ncid)
    !      
  END SUBROUTINE Read_Ncdf_dim
  !
  !**************************************************************
  ! end subroutine Read_Ncdf_dim
  !**************************************************************      
  !
  !****************************************************************
  !   subroutine Write_Ncdf_dim					*
  !								*
  ! subroutine to write a dimension in a given file 		*
  !								*
  !     dimname : name of dimension to initialize			*
  !     file    : netcdf file name				*
  !     dimval  : value of the dimension to write			*
  !								*
  !****************************************************************
  !
  SUBROUTINE Write_Ncdf_dim(dimname,file,dimval)   
    !      
    IMPLICIT NONE
    !           
    CHARACTER(*),INTENT(in) :: dimname,file    
    INTEGER    :: dimval
    !      
    ! local variables
    !
    INTEGER ncid,status,dimid
    !     
    status = nf90_open(file,NF90_WRITE,ncid)
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_redef(ncid)
    IF(dimval.EQ.0) THEN
       status = nf90_def_dim(ncid,dimname,nf90_unlimited,dimid)       
    ELSE
       status = nf90_def_dim(ncid,dimname,dimval,dimid)  
    END IF

    status = nf90_enddef(ncid)
    !      

    status = nf90_close(ncid)
    !      
  END SUBROUTINE Write_Ncdf_dim
  !
  !**************************************************************
  ! end subroutine Write_Ncdf_dim
  !**************************************************************            
  ! 
  !****************************************************************
  !   subroutine Read_Ncdf_var					*
  !								*
  ! subroutine to retrieve values of a given variable 		*
  !								*
  !     varname : name of variable to retrieve			*
  !     file    : netcdf file name				*
  !     tabvar  : array containing values of the required variable*
  !								*
  !****************************************************************
  !          
  !**************************************************************
  !   subroutine Read_Ncdf_var1d_real
  !**************************************************************          
  !      
  SUBROUTINE Read_Ncdf_var1d_Real(varname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    REAL*8, DIMENSION(:), POINTER :: tabvar
    !
    !local variables
    !
    INTEGER, DIMENSION(1) :: dimID
    INTEGER :: dim1
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_varid(ncid,varname,varid)
    status=nf90_inquire_variable(ncid,varid,dimids=dimID)
    status=nf90_inquire_dimension(ncid,dimID(1),len=dim1)
    !                
    IF(.NOT. ASSOCIATED(tabvar)) THEN
       ALLOCATE(tabvar(dim1))  
    ELSE
       IF( ANY(SHAPE(tabvar)/=(/dim1/)) ) THEN	   
          DEALLOCATE(tabvar)   
          ALLOCATE(tabvar(dim1))      
       ENDIF
    ENDIF
    !        
    status=nf90_get_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Read_Ncdf_var1d_Real
  !           
  !**************************************************************
  ! end subroutine Read_Ncdf_var1d_real
  !**************************************************************          
  !            
  !           
  !**************************************************************
  !   subroutine Read_Ncdf_var2d_real
  !**************************************************************          
  !      
  SUBROUTINE Read_Ncdf_var2d_Real(varname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    REAL*8, DIMENSION(:,:), POINTER :: tabvar
    !local variables
    INTEGER, DIMENSION(10) :: dimIDS
    INTEGER :: dim1,dim2
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_varid(ncid,varname,varid)
    status=nf90_inquire_variable(ncid,varid,dimids=dimIDS)
    status=nf90_inquire_dimension(ncid,dimIDS(1),len=dim1)
    status=nf90_inquire_dimension(ncid,dimIDS(2),len=dim2)
    !                
    IF(.NOT. ASSOCIATED(tabvar)) THEN
       ALLOCATE(tabvar(dim1,dim2))  
    ELSE
       IF( ANY(SHAPE(tabvar)/=(/dim1,dim2/)) ) THEN	   
          DEALLOCATE(tabvar)   
          ALLOCATE(tabvar(dim1,dim2))      
       ENDIF
    ENDIF
    !       
    status=nf90_get_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    !     
  END SUBROUTINE Read_Ncdf_var2d_Real
  !           
  !**************************************************************
  ! end subroutine Read_Ncdf_var2d_real
  !**************************************************************          
  !    

  !           
  !**************************************************************
  !   subroutine Read_Ncdf_var2d_real_bis
  !**************************************************************          
  !      
  SUBROUTINE Read_Ncdf_var2d_Real_bis(varname,file,tabvar,strt,cnt)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    REAL*8, DIMENSION(:,:), POINTER :: tabvar
    !local variables
    INTEGER, DIMENSION(10) :: dimIDS
    INTEGER, DIMENSION(2) :: strt,cnt
    INTEGER :: dim1,dim2
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    dim1 = cnt(1) 
    dim2 = cnt(2)
    !      
    status = nf90_inq_varid(ncid,varname,varid)
    status=nf90_inquire_variable(ncid,varid,dimids=dimIDS)
    !                
    IF(.NOT. ASSOCIATED(tabvar)) THEN
       ALLOCATE(tabvar(dim1,dim2))  
    ELSE
       IF( ANY(SHAPE(tabvar)/=(/dim1,dim2/)) ) THEN	   
          DEALLOCATE(tabvar)   
          ALLOCATE(tabvar(dim1,dim2))      
       ENDIF
    ENDIF
    !       
    status=nf90_get_var(ncid,varid,tabvar,start = strt,count = cnt)     
    !     
    status = nf90_close(ncid)
    !     
  END SUBROUTINE Read_Ncdf_var2d_Real_bis
  !           
  !**************************************************************
  ! end subroutine Read_Ncdf_var2d_real_bis
  !**************************************************************          
  !  


  !           
  !**************************************************************
  !   subroutine Read_Ncdf_var3d_real
  !**************************************************************          
  !      
  SUBROUTINE Read_Ncdf_var3d_Real(varname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    REAL*8, DIMENSION(:,:,:), POINTER :: tabvar
    !
    !local variables
    !
    INTEGER, DIMENSION(10) :: dimIDS
    INTEGER :: dim1,dim2,dim3
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_varid(ncid,varname,varid)
    status=nf90_inquire_variable(ncid,varid,dimids=dimIDS)
    status=nf90_inquire_dimension(ncid,dimIDS(1),len=dim1)
    status=nf90_inquire_dimension(ncid,dimIDS(2),len=dim2)  
    status=nf90_inquire_dimension(ncid,dimIDS(3),len=dim3)
    !                
    IF(.NOT. ASSOCIATED(tabvar)) THEN
       ALLOCATE(tabvar(dim1,dim2,dim3))  
    ELSE
       IF( ANY(SHAPE(tabvar) /= (/dim1,dim2,dim3/)) ) THEN	   
          DEALLOCATE(tabvar)   
          ALLOCATE(tabvar(dim1,dim2,dim3))      
       ENDIF
    ENDIF
    !      
    status=nf90_get_var(ncid,varid,tabvar) 
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to retrieve netcdf variable : ",TRIM(varname)
       STOP
    ENDIF
    !     
    status = nf90_close(ncid)
    !     
  END SUBROUTINE Read_Ncdf_var3d_Real
  !           
  !**************************************************************
  ! end subroutine Read_Ncdf_var3d_real
  !**************************************************************          
  ! 
  !           
  !**************************************************************
  !   subroutine Read_Ncdf_var4d_real
  !**************************************************************          
  !      
  SUBROUTINE Read_Ncdf_var4d_Real(varname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    REAL*8, DIMENSION(:,:,:,:), POINTER :: tabvar
    !
    !local variables
    !
    INTEGER, DIMENSION(10) :: dimIDS
    INTEGER :: dim1,dim2,dim3,dim4
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_varid(ncid,varname,varid)
    status=nf90_inquire_variable(ncid,varid,dimids=dimIDS)
    status=nf90_inquire_dimension(ncid,dimIDS(1),len=dim1)
    status=nf90_inquire_dimension(ncid,dimIDS(2),len=dim2)  
    status=nf90_inquire_dimension(ncid,dimIDS(3),len=dim3)   
    status=nf90_inquire_dimension(ncid,dimIDS(4),len=dim4)       
    !                
    IF(.NOT. ASSOCIATED(tabvar)) THEN
       ALLOCATE(tabvar(dim1,dim2,dim3,dim4))  
    ELSE
       IF( ANY(SHAPE(tabvar) /= (/dim1,dim2,dim3,dim4/)) ) THEN	   
          DEALLOCATE(tabvar)   
          ALLOCATE(tabvar(dim1,dim2,dim3,dim4))      
       ENDIF
    ENDIF
    !  
    status=nf90_get_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    !     
  END SUBROUTINE Read_Ncdf_var4d_Real

  SUBROUTINE Read_Ncdf_var0d_Real(varname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    REAL*8 :: tabvar
    !
    !local variables
    !
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_varid(ncid,varname,varid)
    !
    !  
    status=nf90_get_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    !     
  END SUBROUTINE Read_Ncdf_var0d_Real

  SUBROUTINE Read_Ncdf_var0d_Int(varname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    INTEGER :: tabvar
    !
    !local variables
    !
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_varid(ncid,varname,varid)
    !
    !  
    status=nf90_get_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    !     
  END SUBROUTINE Read_Ncdf_var0d_Int
  !           
  !**************************************************************
  ! end subroutine Read_Ncdf_var4d_real
  !**************************************************************          
  !             
  !           
  !**************************************************************
  !   subroutine Read_Ncdf_var1d_int
  !**************************************************************          
  !      
  SUBROUTINE Read_Ncdf_var1d_Int(varname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    INTEGER, DIMENSION(:), POINTER :: tabvar
    !
    !local variables
    !
    INTEGER,DIMENSION(10) :: dimID
    INTEGER :: dim1
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_varid(ncid,varname,varid)
    status=nf90_inquire_variable(ncid,varid,dimids=dimID)
    status=nf90_inquire_dimension(ncid,dimID(1),len=dim1)          
    !                
    IF(.NOT. ASSOCIATED(tabvar)) THEN
       ALLOCATE(tabvar(dim1))  
    ELSE
       IF( ANY(SHAPE(tabvar) /= (/dim1/)) ) THEN	   
          DEALLOCATE(tabvar)   
          ALLOCATE(tabvar(dim1))      
       ENDIF
    ENDIF
    !  
    status=nf90_get_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    !      
  END SUBROUTINE Read_Ncdf_var1d_Int
  !           
  !**************************************************************
  ! end subroutine Read_Ncdf_var1d_int
  !**************************************************************          
  !            
  !           
  !**************************************************************
  !   subroutine Read_Ncdf_var2d_int
  !**************************************************************          
  !      
  SUBROUTINE Read_Ncdf_var2d_Int(varname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    INTEGER, DIMENSION(:,:), POINTER :: tabvar
    !local variables
    INTEGER, DIMENSION(10) :: dimIDS
    INTEGER :: dim1,dim2
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_varid(ncid,varname,varid)
    status=nf90_inquire_variable(ncid,varid,dimids=dimIDS)
    status=nf90_inquire_dimension(ncid,dimIDS(1),len=dim1)
    status=nf90_inquire_dimension(ncid,dimIDS(2),len=dim2)      
    !                
    IF(.NOT. ASSOCIATED(tabvar)) THEN
       ALLOCATE(tabvar(dim1,dim2))  
    ELSE
       IF( ANY(SHAPE(tabvar) /= (/dim1,dim2/)) ) THEN	   
          DEALLOCATE(tabvar)   
          ALLOCATE(tabvar(dim1,dim2))      
       ENDIF
    ENDIF
    !  
    status=nf90_get_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    !      
  END SUBROUTINE Read_Ncdf_var2d_Int
  !           
  !**************************************************************
  ! end subroutine Read_Ncdf_var2d_Int
  !**************************************************************          
  !    

  !           
  !**************************************************************
  !   subroutine Read_Ncdf_var3d_Int
  !**************************************************************          
  !      
  SUBROUTINE Read_Ncdf_var3d_Int(varname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    INTEGER, DIMENSION(:,:,:), POINTER :: tabvar
    !
    !local variables
    !
    INTEGER, DIMENSION(10) :: dimIDS
    INTEGER :: dim1,dim2,dim3
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_varid(ncid,varname,varid)
    status=nf90_inquire_variable(ncid,varid,dimids=dimIDS)
    status=nf90_inquire_dimension(ncid,dimIDS(1),len=dim1)
    status=nf90_inquire_dimension(ncid,dimIDS(2),len=dim2)  
    status=nf90_inquire_dimension(ncid,dimIDS(3),len=dim3)          
    !                
    IF(.NOT. ASSOCIATED(tabvar)) THEN
       ALLOCATE(tabvar(dim1,dim2,dim3))  
    ELSE
       IF( ANY(SHAPE(tabvar) /= (/dim1,dim2,dim3/)) ) THEN	   
          DEALLOCATE(tabvar)   
          ALLOCATE(tabvar(dim1,dim2,dim3))      
       ENDIF
    ENDIF
    ! 
    status=nf90_get_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    !      
  END SUBROUTINE Read_Ncdf_var3d_Int
  !           
  !**************************************************************
  ! end subroutine Read_Ncdf_var3d_int
  !**************************************************************  
  !
  !
  !
  !**************************************************************
  !   subroutine Read_Ncdf_var4d_int
  !**************************************************************          
  !      
  SUBROUTINE Read_Ncdf_var4d_Int(varname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    INTEGER, DIMENSION(:,:,:,:), POINTER :: tabvar
    !
    !local variables
    !
    INTEGER, DIMENSION(10) :: dimIDS
    INTEGER :: dim1,dim2,dim3,dim4
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_varid(ncid,varname,varid)
    status=nf90_inquire_variable(ncid,varid,dimids=dimIDS)
    status=nf90_inquire_dimension(ncid,dimIDS(1),len=dim1)
    status=nf90_inquire_dimension(ncid,dimIDS(2),len=dim2)  
    status=nf90_inquire_dimension(ncid,dimIDS(3),len=dim3)   
    status=nf90_inquire_dimension(ncid,dimIDS(4),len=dim4)       
    !                
    IF(.NOT. ASSOCIATED(tabvar)) THEN
       ALLOCATE(tabvar(dim1,dim2,dim3,dim4))  
    ELSE
       IF( ANY(SHAPE(tabvar) /= (/dim1,dim2,dim3,dim4/)) ) THEN	   
          DEALLOCATE(tabvar)   
          ALLOCATE(tabvar(dim1,dim2,dim3,dim4))      
       ENDIF
    ENDIF
    !  
    status=nf90_get_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    !     
  END SUBROUTINE Read_Ncdf_var4d_Int
  !           
  !**************************************************************
  ! end subroutine Read_Ncdf_var4d_real
  !**************************************************************         
  !    
  !
  !****************************************************************
  !   subroutine Write_Ncdf_var					*
  !								*
  ! subroutine to write a variable in a given file 		*
  !								*
  !     varname : name of variable to store			*     
  !     dimname : name of dimensions of the given variable	*
  !     file    : netcdf file name				*
  !     tabvar  : values of the variable to write			*
  !								*
  !****************************************************************
  !    
  !      
  !**************************************************************
  !   subroutine Write_Ncdf_var1d_real
  !**************************************************************    
  !      
  SUBROUTINE Write_Ncdf_var1d_Real(varname,dimname,file,tabvar,typevar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file,dimname,typevar
    REAL*8, DIMENSION(:), POINTER :: tabvar
    !
    ! local variables
    !
    INTEGER :: dimid
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_WRITE,ncid)       
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_dimid(ncid,dimname, dimid)
    status = nf90_inq_varid(ncid,varname,varid)
    status = nf90_redef(ncid)
    SELECT CASE(TRIM(typevar))
    CASE('double')
       status = nf90_def_var(ncid,varname,nf90_double,(/dimid/),varid)
    CASE('float')
       status = nf90_def_var(ncid,varname,nf90_float,(/dimid/),varid)      
    END SELECT
    status = nf90_enddef(ncid)
    status = nf90_put_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Write_Ncdf_var1d_Real
  !      
  !**************************************************************
  !   end subroutine Write_Ncdf_var1d_real
  !**************************************************************
  !      
  !**************************************************************
  !   subroutine Write_Ncdf_var2d_real
  !**************************************************************    
  !      
  SUBROUTINE Write_Ncdf_var2d_Real_bis(varname,dimname,file,tabvar,nbdim,typevar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file,typevar
    INTEGER,INTENT(in) :: nbdim
    CHARACTER(*), DIMENSION(4) :: dimname
    REAL*8, DIMENSION(:,:) :: tabvar
    REAL*8, DIMENSION(:,:,:),POINTER :: tabtemp3d
    REAL*8, DIMENSION(:,:,:,:),POINTER :: tabtemp4d
    !
    ! local variables
    !
    INTEGER :: dimid1,dimid2,dimid3,dimid4
    INTEGER :: status,ncid,ncid2
    INTEGER :: varid,varid2             
    ! 
    IF(nbdim==4) THEN      
       ALLOCATE(tabtemp4d(SIZE(tabvar,1),SIZE(tabvar,2),1,1))
       tabtemp4d(:,:,1,1) = tabvar(:,:)
    ELSE IF(nbdim==3) THEN
       ALLOCATE(tabtemp3d(SIZE(tabvar,1),SIZE(tabvar,2),1))
       tabtemp3d(:,:,1) = tabvar(:,:)
    END IF
    !      
    status = nf90_open(file,NF90_WRITE,ncid)       
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_dimid(ncid,dimname(1), dimid1)
    status = nf90_inq_dimid(ncid,dimname(2), dimid2)
    status = nf90_inq_dimid(ncid,dimname(3), dimid3)
    !      
    IF(nbdim==4) status = nf90_inq_dimid(ncid,dimname(4), dimid4)
    !      
    status = nf90_inq_varid(ncid,varname,varid)
    status = nf90_redef(ncid)
    IF(nbdim==4 .AND. typevar == 'double') THEN
       status = nf90_def_var(ncid,varname,nf90_double,     &
            (/dimid1,dimid2,dimid3,dimid4/),varid)
       !                              
    ELSE IF(nbdim==4 .AND. typevar == 'float') THEN
       status = nf90_def_var(ncid,varname,nf90_float,     &
            (/dimid1,dimid2,dimid3,dimid4/),varid)                        
       !          
    ELSE IF(nbdim==3 .AND. typevar == 'float') THEN
       status = nf90_def_var(ncid,varname,nf90_float,     &
            (/dimid1,dimid2,dimid3/),varid)
       !
    ELSE IF(nbdim==3 .AND. typevar == 'double') THEN
       status = nf90_def_var(ncid,varname,nf90_double,     &
            (/dimid1,dimid2,dimid3/),varid)
       !     			      			      
    ENDIF
    !      			      
    status = nf90_enddef(ncid)
    IF(nbdim==4) status = nf90_put_var(ncid,varid,tabtemp4d)
    IF(nbdim==3) status = nf90_put_var(ncid,varid,tabtemp3d)
    !      
    IF(ASSOCIATED( tabtemp3d ) ) DEALLOCATE( tabtemp3d )   
    IF(ASSOCIATED( tabtemp4d ) ) DEALLOCATE( tabtemp4d )     
    !
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Write_Ncdf_var2d_Real_bis
  !      
  !**************************************************************
  !   end subroutine Write_Ncdf_var2d_real
  !**************************************************************  
  !   
  !      
  !**************************************************************
  !   subroutine Write_Ncdf_var2d_real
  !**************************************************************    
  !      
  SUBROUTINE Write_Ncdf_var2d_Real(varname,dimname,file,tabvar,typevar)
    !      
    !      implicit none
    !       
    CHARACTER(*),INTENT(in) :: varname,file,typevar
    CHARACTER(*), DIMENSION(2) :: dimname
    REAL*8, DIMENSION(:,:), POINTER :: tabvar
    !
    ! local variables
    !
    INTEGER :: dimid1,dimid2
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_WRITE,ncid)       
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_dimid(ncid,dimname(1), dimid1)
    status = nf90_inq_dimid(ncid,dimname(2), dimid2)
    status = nf90_inq_varid(ncid,varname,varid)
    status = nf90_redef(ncid)

    SELECT CASE(TRIM(typevar))
    CASE('double')
       status = nf90_def_var(ncid,varname,nf90_double,     &
            (/dimid1,dimid2/),varid)
    CASE('float')
       status = nf90_def_var(ncid,varname,nf90_float,     &
            (/dimid1,dimid2/),varid)     
    END SELECT
    !
    status = nf90_enddef(ncid)
    status = nf90_put_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Write_Ncdf_var2d_Real
  !      
  !**************************************************************
  !   end subroutine Write_Ncdf_var2d_real
  !**************************************************************  
  !
  !      
  !**************************************************************
  !   subroutine Write_Ncdf_var3d_real
  !**************************************************************
  !      
  SUBROUTINE Write_Ncdf_var3d_Real(varname,dimname,file,tabvar,typevar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file,typevar
    CHARACTER(*),DIMENSION(3),INTENT(in) :: dimname
    REAL*8, DIMENSION(:,:,:), POINTER :: tabvar
    !
    ! local variables
    !
    INTEGER :: dimid1,dimid2,dimid3
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_WRITE,ncid)       
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_dimid(ncid,dimname(1), dimid1)
    status = nf90_inq_dimid(ncid,dimname(2), dimid2)
    status = nf90_inq_dimid(ncid,dimname(3), dimid3)
    status = nf90_inq_varid(ncid,varname,varid)
    status = nf90_redef(ncid)

    SELECT CASE(TRIM(typevar))
    CASE('double')
       status = nf90_def_var(ncid,varname,nf90_double,     &
            (/dimid1,dimid2,dimid3/),varid)
    CASE('float')
       status = nf90_def_var(ncid,varname,nf90_float,     &
            (/dimid1,dimid2,dimid3/),varid)    
    END SELECT
    !
    status = nf90_enddef(ncid)
    status = nf90_put_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Write_Ncdf_var3d_Real
  !      
  !**************************************************************
  !   end subroutine Write_Ncdf_var3d_real
  !**************************************************************
  ! 
  !
  !**************************************************************
  !   subroutine Write_Ncdf_var4d_real
  !**************************************************************
  !      
  SUBROUTINE Write_Ncdf_var4d_Real(varname,dimname,file,tabvar,typevar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file,typevar
    CHARACTER(*),DIMENSION(4),INTENT(in) :: dimname
    REAL*8, DIMENSION(:,:,:,:), POINTER :: tabvar
    !
    ! local variables
    !
    INTEGER :: dimid1,dimid2,dimid3,dimid4
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_WRITE,ncid)       
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !
    status = nf90_inq_varid(ncid,varname,varid)
    !      
    IF(status/=nf90_noerr) THEN
       !     
       status = nf90_inq_dimid(ncid,dimname(1), dimid1)
       status = nf90_inq_dimid(ncid,dimname(2), dimid2)
       status = nf90_inq_dimid(ncid,dimname(3), dimid3)
       status = nf90_inq_dimid(ncid,dimname(4), dimid4)
       status = nf90_redef(ncid)
       !      
       SELECT CASE(TRIM(typevar))
       CASE('double')
          status = nf90_def_var(ncid,varname,nf90_double,     &
               (/dimid1,dimid2,dimid3,dimid4/),varid)
       CASE('float')
          status = nf90_def_var(ncid,varname,nf90_float,     &
               (/dimid1,dimid2,dimid3,dimid4/),varid)   
       END SELECT
       !
       status = nf90_enddef(ncid)
    ENDIF
    !         
    status = nf90_put_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Write_Ncdf_var4d_Real
  !      
  !**************************************************************
  !   end subroutine Write_Ncdf_var4d_real
  !**************************************************************
  !
  !
  !  
  !**************************************************************
  !   subroutine Write_Ncdf_var1d_Int
  !**************************************************************    
  !      
  SUBROUTINE Write_Ncdf_var1d_Int(varname,dimname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file,dimname
    INTEGER, DIMENSION(:), POINTER :: tabvar
    !
    ! local variables
    !
    INTEGER :: dimid
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    !           print *,'ici tabvar = ',tabvar,varname,dimname
    status = nf90_open(file,NF90_WRITE,ncid)       
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_dimid(ncid,dimname, dimid)
    status = nf90_inq_varid(ncid,varname,varid)
    status = nf90_redef(ncid)
    status = nf90_def_var(ncid,varname,nf90_int,(/dimid/),varid)
    status = nf90_enddef(ncid)
    status = nf90_put_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Write_Ncdf_var1d_Int
  !      
  !**************************************************************
  !   end subroutine Write_Ncdf_var1d_Int
  !**************************************************************
  !  
  !      
  !**************************************************************
  !   subroutine Write_Ncdf_var2d_Int
  !**************************************************************    
  !      
  SUBROUTINE Write_Ncdf_var2d_Int(varname,dimname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    CHARACTER(*), DIMENSION(2) :: dimname
    INTEGER, DIMENSION(:,:), POINTER :: tabvar
    !
    ! local variables
    !
    INTEGER :: dimid1,dimid2
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_WRITE,ncid)       
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_dimid(ncid,dimname(1), dimid1)
    status = nf90_inq_dimid(ncid,dimname(2), dimid2)
    status = nf90_inq_varid(ncid,varname,varid)
    status = nf90_redef(ncid)
    status = nf90_def_var(ncid,varname,nf90_int,     &
         (/dimid1,dimid2/),varid)
    status = nf90_enddef(ncid)
    status = nf90_put_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Write_Ncdf_var2d_Int
  !      
  !**************************************************************
  !   end subroutine Write_Ncdf_var2d_Int
  !**************************************************************  
  ! 
  !      
  !**************************************************************
  !   subroutine Write_Ncdf_var3d_Int
  !**************************************************************
  !      
  SUBROUTINE Write_Ncdf_var3d_Int(varname,dimname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    CHARACTER(*),DIMENSION(3),INTENT(in) :: dimname
    INTEGER, DIMENSION(:,:,:), POINTER :: tabvar
    !
    ! local variables
    !
    INTEGER :: dimid1,dimid2,dimid3
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_WRITE,ncid)       
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_dimid(ncid,dimname(1), dimid1)
    status = nf90_inq_dimid(ncid,dimname(2), dimid2)
    status = nf90_inq_dimid(ncid,dimname(3), dimid3)
    status = nf90_inq_varid(ncid,varname,varid)
    status = nf90_redef(ncid)
    status = nf90_def_var(ncid,varname,nf90_int,     &
         (/dimid1,dimid2,dimid3/),varid)
    status = nf90_enddef(ncid)
    status = nf90_put_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Write_Ncdf_var3d_Int
  !      
  !**************************************************************
  !   end subroutine Write_Ncdf_var3d_Int
  !**************************************************************
  ! 
  !
  !**************************************************************
  !   subroutine Write_Ncdf_var4d_Int
  !**************************************************************
  !      
  SUBROUTINE Write_Ncdf_var4d_Int(varname,dimname,file,tabvar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    CHARACTER(*),DIMENSION(4),INTENT(in) :: dimname
    INTEGER, DIMENSION(:,:,:,:), POINTER :: tabvar
    !
    ! local variables
    !
    INTEGER :: dimid1,dimid2,dimid3,dimid4
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_WRITE,ncid)       
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_dimid(ncid,dimname(1), dimid1)
    status = nf90_inq_dimid(ncid,dimname(2), dimid2)
    status = nf90_inq_dimid(ncid,dimname(3), dimid3)
    status = nf90_inq_dimid(ncid,dimname(4), dimid4)
    status = nf90_inq_varid(ncid,varname,varid)
    status = nf90_redef(ncid)
    status = nf90_def_var(ncid,varname,nf90_int,     &
         (/dimid1,dimid2,dimid3,dimid4/),varid)
    status = nf90_enddef(ncid)
    status = nf90_put_var(ncid,varid,tabvar)     
    !     
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Write_Ncdf_var4d_Int
  !      
  !**************************************************************
  !   end subroutine Write_Ncdf_var4d_Int
  !************************************************************** 
  !
  !
  !****************************************************************
  !   subroutine Read_Ncdf_var_t					*
  !								*
  ! subroutine to read a variable in a given file for time t	*
  !								*
  !     varname : name of variable to read			*     
  !     file    : netcdf file name				*
  !     tabvar  : values of the read variable			*
  !     time    : time corresponding to the values to read	*	
  !								*
  !****************************************************************
  !
  !
  !**************************************************************
  !   subroutine Read_Ncdf_var3d_real_t
  !**************************************************************          
  !      
  SUBROUTINE Read_Ncdf_var3d_Real_t(varname,file,tabvar,time)
    !      
    USE agrif_types     
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    INTEGER,INTENT(in) :: time
    REAL*8, DIMENSION(:,:,:), POINTER :: tabvar
    !
    !local variables
    !
    INTEGER, DIMENSION(3) :: dimIDS
    INTEGER :: dim1,dim2
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_varid(ncid,varname,varid)
    !        
    status=nf90_inquire_variable(ncid,varid,dimids=dimIDS)
    status=nf90_inquire_dimension(ncid,dimIDS(1),len=dim1)
    status=nf90_inquire_dimension(ncid,dimIDS(2),len=dim2)
    !
    IF(.NOT. ASSOCIATED(tabvar)) THEN
       ALLOCATE(tabvar(dim1,dim2,1))  
    ELSE
       IF( ANY(SHAPE(tabvar) /= (/dim1,dim2,1/)) ) THEN	   
          DEALLOCATE(tabvar)   
          ALLOCATE(tabvar(dim1,dim2,1))      
       ENDIF
    ENDIF

    status=nf90_get_var(ncid,varid,tabvar,start=(/1,1,time/))

    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to retrieve netcdf variable : ",TRIM(varname)
       STOP
    ENDIF
    !     
    status = nf90_close(ncid)
    !     
  END SUBROUTINE Read_Ncdf_var3d_Real_t
  !           
  !**************************************************************
  ! end subroutine Read_Ncdf_var3d_real_t
  !**************************************************************          
  !
  !
  !
  !****************************************************************
  !   subroutine Write_Ncdf_var_t					*
  !								*
  ! subroutine to write a variable in a given file for time t	*
  !								*
  !     varname : name of variable to store			*     
  !     dimname : name of dimensions of the given variable	*
  !     file    : netcdf file name				*
  !     tabvar  : values of the variable to write			*
  !     time    : time corresponding to the values to store	*	
  !								*
  !****************************************************************
  !
  !      
  !**************************************************************
  !   subroutine Write_Ncdf_var3d_real_t
  !**************************************************************
  !      
  SUBROUTINE Write_Ncdf_var3d_Real_t(varname,dimname,file,tabvar,time,typevar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file,typevar
    CHARACTER(*),DIMENSION(3),INTENT(in) :: dimname
    INTEGER :: time
    REAL*8, DIMENSION(:,:,:), POINTER :: tabvar
    !
    ! local variables
    !
    INTEGER :: dimid1,dimid2,dimid3
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_WRITE,ncid)       
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    IF(time==1) THEN
       !      
       status = nf90_inq_dimid(ncid,dimname(1), dimid1)
       status = nf90_inq_dimid(ncid,dimname(2), dimid2)
       status = nf90_inq_dimid(ncid,dimname(3), dimid3)
       status = nf90_redef(ncid)

       !      
       SELECT CASE(TRIM(typevar))
       CASE('double')
          status = nf90_def_var(ncid,varname,nf90_double,     &
               (/dimid1,dimid2,dimid3/),varid) 
       CASE('float')
          status = nf90_def_var(ncid,varname,nf90_float,     &
               (/dimid1,dimid2,dimid3/),varid)    
       END SELECT
       !
       status = nf90_enddef(ncid)

    ELSE
       status = nf90_inq_varid(ncid, varname, varid)
    ENDIF
    !      
    status = nf90_put_var(ncid,varid,tabvar,start=(/1,1,time/))
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to store variable ",varname, &
	    " in file ",file
       STOP
    ENDIF
    !     
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Write_Ncdf_var3d_Real_t
  !      
  !**************************************************************
  !   end subroutine Write_Ncdf_var3d_real_t
  !**************************************************************
  !
  !
  !****************************************************************
  !   subroutine Read_Ncdf_var_t					*
  !								*
  ! subroutine to read a variable in a given file for time t	*
  !						at level n	*
  !     varname : name of variable to read			*     
  !     file    : netcdf file name				*
  !     tabvar  : values of the read variable			*
  !     time    : time corresponding to the values to read	*	
  !     level   : level corresponding to the values to read	*
  !								*
  !****************************************************************
  !
  !
  !**************************************************************
  !   subroutine Read_Ncdf_var4d_real_nt
  !**************************************************************          
  !      
  SUBROUTINE Read_Ncdf_var4d_Real_nt(varname,file,tabvar,time,level)
    !      
    USE agrif_types     
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    INTEGER,INTENT(in) :: time,level
    REAL*8, DIMENSION(:,:,:,:), POINTER :: tabvar
    !
    !local variables
    !
    INTEGER, DIMENSION(4) :: dimIDS
    INTEGER :: dim1,dim2
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_varid(ncid,varname,varid)
    !        
    status=nf90_inquire_variable(ncid,varid,dimids=dimIDS)
    status=nf90_inquire_dimension(ncid,dimIDS(1),len=dim1)
    status=nf90_inquire_dimension(ncid,dimIDS(2),len=dim2)
    !
    IF(.NOT. ASSOCIATED(tabvar)) THEN
       ALLOCATE(tabvar(dim1,dim2,1,1))  
    ELSE
       IF ((SIZE(tabvar,1) /= dim1) .OR. (SIZE(tabvar,2) /= dim2)) THEN
          DEALLOCATE(tabvar)
          ALLOCATE(tabvar(dim1,dim2,1,1))
       ENDIF
    ENDIF
    !
    status=nf90_get_var(ncid,varid,tabvar,start=(/1,1,level,time/),count=(/dim1,dim2,1,1/))
    !                                             
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to retrieve netcdf variable : ",TRIM(varname)
       STOP
    ENDIF
    !     
    status = nf90_close(ncid)
    !     
  END SUBROUTINE Read_Ncdf_var4d_Real_nt
  !           
  !**************************************************************
  ! end subroutine Read_Ncdf_var4d_real_nt
  !**************************************************************          
  !
  !
  !**************************************************************
  !   subroutine Read_Ncdf_var4d_real_t
  !**************************************************************          
  !      
  SUBROUTINE Read_Ncdf_var4d_Real_t(varname,file,tabvar,time)
    !      
    USE agrif_types     
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file
    INTEGER,INTENT(in) :: time
    REAL*8, DIMENSION(:,:,:,:), POINTER :: tabvar
    !
    !local variables
    !
    INTEGER, DIMENSION(4) :: dimIDS
    INTEGER :: dim1,dim2,dim3
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_NOWRITE,ncid)
    !      
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !     
    status = nf90_inq_varid(ncid,varname,varid)
    !        
    status=nf90_inquire_variable(ncid,varid,dimids=dimIDS)
    status=nf90_inquire_dimension(ncid,dimIDS(1),len=dim1)
    status=nf90_inquire_dimension(ncid,dimIDS(2),len=dim2)
    status=nf90_inquire_dimension(ncid,dimIDS(3),len=dim3)
    !
    IF(.NOT. ASSOCIATED(tabvar)) ALLOCATE(tabvar(dim1,dim2,dim3,1))  
    status=nf90_get_var(ncid,varid,tabvar,start=(/1,1,1,time/))

    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to retrieve netcdf variable : ",TRIM(varname)
       STOP
    ENDIF
    !     
    status = nf90_close(ncid)
    !     
  END SUBROUTINE Read_Ncdf_var4d_Real_t
  !           
  !**************************************************************
  ! end subroutine Read_Ncdf_var4d_real_t
  !**************************************************************          
  !
  !
  !****************************************************************
  !   subroutine Write_Ncdf_var_t					*
  !								*
  ! subroutine to write a variable in a given file for time t	*
  !						at level n	*
  !     varname : name of variable to store			*     
  !     dimname : name of dimensions of the given variable	*
  !     file    : netcdf file name				*
  !     tabvar  : values of the variable to write			*
  !     time    : time corresponding to the values to store	*	
  !     level   : level corresponding to the values to store	*
  !								*
  !****************************************************************
  !
  !      
  !**************************************************************
  !   subroutine Write_Ncdf_var4d_real_t
  !**************************************************************
  !      
  SUBROUTINE Write_Ncdf_var4d_Real_t(varname,dimname,file,tabvar,time,typevar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file,typevar
    CHARACTER(*),DIMENSION(4),INTENT(in) :: dimname
    INTEGER :: time,level
    REAL*8, DIMENSION(:,:,:,:), POINTER :: tabvar
    !
    ! local variables
    !
    INTEGER :: dimid1,dimid2,dimid3,dimid4
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_WRITE,ncid)       
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !      
    IF(time==1) THEN
       !      
       status = nf90_inq_dimid(ncid,dimname(1), dimid1)
       status = nf90_inq_dimid(ncid,dimname(2), dimid2)
       status = nf90_inq_dimid(ncid,dimname(3), dimid3)
       status = nf90_inq_dimid(ncid,dimname(4), dimid4)
       status = nf90_redef(ncid)
       !      
       SELECT CASE(TRIM(typevar))
       CASE('double')
          status = nf90_def_var(ncid,TRIM(varname),nf90_double,     &
               (/dimid1,dimid2,dimid3,dimid4/),varid)  
       CASE('float')
          status = nf90_def_var(ncid,TRIM(varname),nf90_float,     &
               (/dimid1,dimid2,dimid3,dimid4/),varid)    
       END SELECT
       !
       status = nf90_enddef(ncid)

    ELSE
       status = nf90_inq_varid(ncid, varname, varid)
    ENDIF
    !    
    status = nf90_put_var(ncid,varid,tabvar,start=(/1,1,1,time/))
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to store variable ",varname, &
	    " in file ",file
       STOP
    ENDIF
    !    
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Write_Ncdf_var4d_Real_t
  !      
  !**************************************************************
  !   end subroutine Write_Ncdf_var4d_real_t
  !**************************************************************
  !
  !      
  !**************************************************************
  !   subroutine Write_Ncdf_var4d_real_nt
  !**************************************************************
  !      
  SUBROUTINE Write_Ncdf_var4d_Real_nt(varname,dimname,file,tabvar,time,level,typevar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file,typevar
    CHARACTER(*),DIMENSION(4),INTENT(in) :: dimname
    INTEGER :: time,level
    REAL*8, DIMENSION(:,:,:,:), POINTER :: tabvar
    !
    ! local variables
    !
    INTEGER :: dimid1,dimid2,dimid3,dimid4
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_WRITE,ncid)       
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !      
    IF(time==1.AND.level==1) THEN
       !      
       status = nf90_inq_dimid(ncid,dimname(1), dimid1)
       status = nf90_inq_dimid(ncid,dimname(2), dimid2)
       status = nf90_inq_dimid(ncid,dimname(3), dimid3)
       status = nf90_inq_dimid(ncid,dimname(4), dimid4)
       status = nf90_redef(ncid)
       !      
       SELECT CASE(TRIM(typevar))
       CASE('double')
          status = nf90_def_var(ncid,TRIM(varname),nf90_double,     &
               (/dimid1,dimid2,dimid3,dimid4/),varid)  
       CASE('float')
          status = nf90_def_var(ncid,TRIM(varname),nf90_float,     &
               (/dimid1,dimid2,dimid3,dimid4/),varid)    
       END SELECT
       !
       status = nf90_enddef(ncid)

    ELSE
       status = nf90_inq_varid(ncid, varname, varid)
    ENDIF
    !    
    status = nf90_put_var(ncid,varid,tabvar,start=(/1,1,level,time/))
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to store variable ",varname, &
	    " in file ",file
       STOP
    ENDIF
    !    
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Write_Ncdf_var4d_Real_nt

  SUBROUTINE Write_Ncdf_var0d_Real(varname,file,tabvar,typevar)
    !      
    IMPLICIT NONE
    !       
    CHARACTER(*),INTENT(in) :: varname,file,typevar
    INTEGER :: time,level
    REAL*8 :: tabvar
    !
    ! local variables
    !
    INTEGER :: status,ncid
    INTEGER :: varid             
    !
    status = nf90_open(file,NF90_WRITE,ncid)       
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",file
       STOP
    ENDIF
    !      

    status = nf90_redef(ncid)
    !      
    SELECT CASE(TRIM(typevar))
    CASE('double')
       status = nf90_def_var(ncid,TRIM(varname),nf90_double,     &
            varid=varid)  
    CASE('float')
       status = nf90_def_var(ncid,TRIM(varname),nf90_float,     &
            varid=varid)    
    END SELECT
    !
    status = nf90_enddef(ncid)

    !    
    status = nf90_put_var(ncid,varid,tabvar)
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to store variable ",varname, &
	    " in file ",file
       STOP
    ENDIF
    !    
    status = nf90_close(ncid)
    ! 
  END SUBROUTINE Write_Ncdf_var0d_Real
  !      
  !**************************************************************
  !   end subroutine Write_Ncdf_var4d_real_nt
  !**************************************************************
  !
  !****************************************************************
  !   subroutine Read_Ncdf_VarName				*
  !								*
  ! subroutine to retrieve of all variables 			*
  ! included in a given file					*
  !								*
  !     filename    : netcdf file name				*
  !     tabvarname  : array containing various variables names	*
  !								*
  !****************************************************************
  !
  !**************************************************************
  !   subroutine Read_Ncdf_VarName
  !**************************************************************          
  !
  SUBROUTINE Read_Ncdf_VarName(filename,tabvarname)
    !      
    CHARACTER(*),INTENT(in) :: filename
    CHARACTER*20,DIMENSION(:),POINTER :: tabvarname
    INTEGER :: nDimensions,nVariables
    INTEGER :: nAttributes,unlimitedDimId,i
    INTEGER :: ncid,status,dimid
    !     
    status = nf90_open(filename,NF90_NOWRITE,ncid)
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",filename
       STOP
    ENDIF
    !      
    status = nf90_inquire(ncid,nDimensions,nVariables,nAttributes, &                                                      
         unlimitedDimId) 
    !
    ALLOCATE(tabvarname(nVariables))
    !
    DO i=1,nVariables
       status = nf90_inquire_variable(ncid,i,tabvarname(i))
    END DO

  END SUBROUTINE Read_Ncdf_Varname
  !
  !**************************************************************
  !   end subroutine Read_Ncdf_VarName
  !**************************************************************          
  !
  !**************************************************************
  !   subroutine Copy_Ncdf_att
  !**************************************************************          
  !
  SUBROUTINE Copy_Ncdf_att_var(varname,filein,fileout)
    !      
    CHARACTER(*),INTENT(in) :: filein,fileout
    CHARACTER(*),INTENT(in) :: varname
    INTEGER :: ncid_in,ncid_out,status,varid_in,varid_out
    !     
    !      print *,'filein = ',filein,fileout
    status = nf90_open(filein,NF90_NOWRITE,ncid_in)
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open input netcdf file : ",filein
       STOP
    ENDIF
    !     							     	   
    status = nf90_open(fileout,NF90_WRITE,ncid_out)
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open output netcdf file : ",fileout
       STOP
    ENDIF
    !  
    !      print *,'ici1'
    status = nf90_inq_varid(ncid_in,varname,varid_in)
    status = nf90_inq_varid(ncid_out,varname,varid_out)
    !
    status = nf90_redef(ncid_out)
    !      
    status = nf90_copy_att(ncid_in,varid_in,'units',ncid_out,varid_out)
    status = nf90_copy_att(ncid_in,varid_in,'valid_min',ncid_out,varid_out)
    status = nf90_copy_att(ncid_in,varid_in,'valid_max',ncid_out,varid_out) 
    status = nf90_copy_att(ncid_in,varid_in,'long_name',ncid_out,varid_out)
    status = nf90_copy_att(ncid_in,varid_in,'calendar',ncid_out,varid_out)
    status = nf90_copy_att(ncid_in,varid_in,'title',ncid_out,varid_out)     
    status = nf90_copy_att(ncid_in,varid_in,'time_origin',ncid_out,varid_out) 
    status = nf90_copy_att(ncid_in,varid_in,'positive',ncid_out,varid_out)                 
    status = nf90_copy_att(ncid_in,varid_in,'tstep_sec',ncid_out,varid_out)          
    status = nf90_copy_att(ncid_in,varid_in,'nav_model',ncid_out,varid_out)   
    status = nf90_copy_att(ncid_in,varid_in,'Minvalue=',ncid_out,varid_out)
    status = nf90_copy_att(ncid_in,varid_in,'Maxvalue=',ncid_out,varid_out)  
    status = nf90_copy_att(ncid_in,varid_in,'short_name',ncid_out,varid_out)
    status = nf90_copy_att(ncid_in,varid_in,'online_operation',ncid_out,varid_out)
    status = nf90_copy_att(ncid_in,varid_in,'axis',ncid_out,varid_out)            
    status = nf90_copy_att(ncid_in,varid_in,'interval_operation',ncid_out,varid_out)
    status = nf90_copy_att(ncid_in,varid_in,'interval_write',ncid_out,varid_out) 
    status = nf90_copy_att(ncid_in,varid_in,'associate',ncid_out,varid_out)
    status = nf90_copy_att(ncid_in,varid_in,'actual_range',ncid_out,varid_out) 
    status = nf90_copy_att(ncid_in,varid_in,'longitude',ncid_out,varid_out)
    status = nf90_copy_att(ncid_in,varid_in,'latitude',ncid_out,varid_out)
    status = nf90_copy_att(ncid_in,varid_in,'scale_factor',ncid_out,varid_out)
    status = nf90_copy_att(ncid_in,varid_in,'add_offset',ncid_out,varid_out)
    status = nf90_copy_att(ncid_in,varid_in,'missing_value',ncid_out,varid_out) 
    !      
    status = nf90_enddef(ncid_out)  
    !
    status = nf90_close(ncid_in)
    status = nf90_close(ncid_out)
    !      print *,'ici2'
    ! 
  END SUBROUTINE Copy_Ncdf_att_var
  !
  !**************************************************************
  !   end subroutine Copy_Ncdf_att
  !**************************************************************         
  !**************************************************************
  !   subroutine Copy_Ncdf_att
  !**************************************************************          
  !
  SUBROUTINE Copy_Ncdf_att_latlon(varname,filein,fileout,min,max)
    !      
    CHARACTER(*),INTENT(in) :: filein,fileout
    CHARACTER(*),INTENT(in) :: varname
    REAL*8 :: min,max
    INTEGER :: ncid_in,ncid_out,status,varid_in,varid_out
    !     
    status = nf90_open(filein,NF90_NOWRITE,ncid_in)
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",filein
       STOP
    ENDIF
    !     							     	   
    status = nf90_open(fileout,NF90_WRITE,ncid_out)
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",fileout
       STOP
    ENDIF
    !  
    status = nf90_inq_varid(ncid_in,varname,varid_in)
    status = nf90_inq_varid(ncid_out,varname,varid_out)
    !
    status = nf90_redef(ncid_out)
    !      
    SELECT CASE (varname)
       !      
    CASE('nav_lon')     
       status = nf90_copy_att(ncid_in,varid_in,'units',ncid_out,varid_out) 
       status = nf90_put_att(ncid_out,varid_out,'valid_min',REAL(min,4))
       status = nf90_put_att(ncid_out,varid_out,'valid_max',REAL(max,4))
       status = nf90_copy_att(ncid_in,varid_in,'long_name',ncid_out,varid_out)
       status = nf90_copy_att(ncid_in,varid_in,'nav_model',ncid_out,varid_out)
       status = nf90_copy_att(ncid_in,varid_in,'title',ncid_out,varid_out)
       !
    CASE('nav_lat')
       status = nf90_copy_att(ncid_in,varid_in,'units',ncid_out,varid_out) 
       status = nf90_put_att(ncid_out,varid_out,'valid_min',REAL(min,4))
       status = nf90_put_att(ncid_out,varid_out,'valid_max',REAL(max,4))
       status = nf90_copy_att(ncid_in,varid_in,'long_name',ncid_out,varid_out)
       status = nf90_copy_att(ncid_in,varid_in,'nav_model',ncid_out,varid_out)
       status = nf90_copy_att(ncid_in,varid_in,'title',ncid_out,varid_out) 
       !
    END SELECT
    !      
    status = nf90_enddef(ncid_out)  
    !
    status = nf90_close(ncid_in)
    status = nf90_close(ncid_out)
  END SUBROUTINE Copy_Ncdf_att_latlon
  !*************************************************************
  !**************************************************************
  !**************************************************************
  !   function Get_NbDims
  !**************************************************************          
  !
  INTEGER FUNCTION Get_NbDims( varname , filename )
    !
    CHARACTER(*),INTENT(in) :: varname,filename
    INTEGER :: status,ncid,varid    
    !      
    status = nf90_open(TRIM(filename),NF90_NOWRITE,ncid)
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",TRIM(filename)
       STOP
    ENDIF
    status = nf90_inq_varid(ncid,TRIM(varname),varid)     
    status = nf90_inquire_variable(ncid, varid , ndims = Get_NbDims)
    !
    RETURN
    !
  END FUNCTION Get_NbDims
  !
  !*************************************************************
  !**************************************************************
  !   function Get_NbDims
  !**************************************************************          
  !
  LOGICAL FUNCTION Dims_Existence( dimname , filename )
    !
    CHARACTER(*),INTENT(in) :: dimname,filename
    INTEGER :: status,ncid,dimid    
    !      
    status = nf90_open(TRIM(filename),NF90_NOWRITE,ncid)
    IF (status/=nf90_noerr) THEN    
       WRITE(*,*)"unable to open netcdf file : ",TRIM(filename)
       STOP
    ENDIF
    status = nf90_inq_dimid(ncid,dimname,dimid)
    !      
    IF (status/=nf90_noerr) THEN
       Dims_Existence = .FALSE.
    ELSE
       Dims_Existence = .TRUE.
    ENDIF
    !
    RETURN
    !
  END FUNCTION Dims_Existence
  !
  !*************************************************************
  !**************************************************************
END MODULE io_netcdf
