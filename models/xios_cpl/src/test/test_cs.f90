PROGRAM test_cs
IMPLICIT NONE
  INCLUDE "mpif.h"
  INTEGER :: rank
  INTEGER :: size
  INTEGER :: ierr
  
  CALL MPI_INIT(ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierr)
  
  IF (rank<11) THEN
   CALL client("client",rank,11)
  ELSE 
    CALL server
  ENDIF
  
  
  CALL MPI_FINALIZE(ierr)
  
END PROGRAM test_cs

  SUBROUTINE client(id,rank,size)
  USE xios
  USE mod_wait
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  CHARACTER(len=*) :: id
  INTEGER :: rank
  INTEGER :: size
  INTEGER :: comm
  TYPE(xios_time)      :: dtime
  TYPE(xios_context) :: ctx_hdl
  INTEGER,PARAMETER :: ni_glo=100 
  INTEGER,PARAMETER :: nj_glo=100 
  INTEGER,PARAMETER :: llm=3 
  DOUBLE PRECISION  :: lval(llm)=(/1.0,2.0,3.0/)
  TYPE(xios_field) :: field_hdl
  TYPE(xios_fieldgroup) :: fieldgroup_hdl
  TYPE(xios_file) :: file_hdl
  
  
  DOUBLE PRECISION,DIMENSION(ni_glo,nj_glo) :: lon_glo,lat_glo
  DOUBLE PRECISION :: field_A_glo(ni_glo,nj_glo,llm)
  DOUBLE PRECISION,ALLOCATABLE :: lon(:),lat(:),field_A(:,:), lonvalue(:) ;
  LOGICAL,ALLOCATABLE :: mask(:,:)
  INTEGER :: ni,ibegin,iend,nj,jbegin,jend,data_ibegin,data_ni
  INTEGER :: i,j,k,l,ts,n,nij_begin
  
  
  CALL init_wait
  
  
  DO j=1,nj_glo
    DO i=1,ni_glo
      lon_glo(i,j)=(i-1)+(j-1)*ni_glo
      lat_glo(i,j)=1000+(i-1)+(j-1)*ni_glo
      DO l=1,llm
        field_A_glo(i,j,l)=(i-1)+(j-1)*ni_glo+10000*l
      ENDDO
    ENDDO
  ENDDO
  ni=ni_glo ; ibegin=1

  
  nij_begin=1
  DO n=0,size-1
    data_ni=(ni_glo*nj_glo)/size
    IF (n < MOD (ni_glo*nj_glo,size)) data_ni=data_ni+1
    IF (n==rank) THEN
      ibegin=1 ; iend=ni_glo ; ni=iend-ibegin+1
      jbegin=(nij_begin-1)/ni_glo +1 
      jend=MOD(nij_begin-1 + data_ni-1,ni_glo) +1
      nj = jend-jbegin+1
      data_ibegin=MOD(nij_begin-1,ni_glo)
      exit
    ELSE
      nij_begin=nij_begin+data_ni
    ENDIF
  ENDDO
  

  ALLOCATE(lon(ni),lat(nj),field_A(data_ni,llm),lonvalue(ni*nj))
  ALLOCATE(mask(ni,nj))
  lon(:)=lon_glo(ibegin:iend,1)
  lat(:)=lat_glo(1,jbegin:jend)

  DO k=1,data_ni
    n=k-1+(jbegin-1)*ni_glo+data_ibegin
    i=MOD(n,ni_glo)+1
    j=n/ni_glo+1
    field_A(k,:)=field_A_glo(i,j,:)
  ENDDO
  
  mask(:,:)=.TRUE.
  mask(1:ni,6)=.TRUE.
  

  CALL xios_initialize(id,return_comm=comm)

  CALL xios_context_initialize("test",comm)
  CALL xios_get_handle("test",ctx_hdl)
  CALL xios_set_current_context(ctx_hdl)
  
  CALL xios_set_context_attr("test",calendar_type="Gregorian") 
  CALL xios_set_context_attr("test",calendar_type="Gregorian") 
  CALL xios_set_context_attr("test",calendar_type="Gregorian") 
!  CALL xios_set_context_attr("test",start_date="01/01/2000 - 00:00:00")
  CALL xios_set_context_attr("test",calendar_type="Gregorian") 
  CALL xios_set_axis_attr("axis_A",size=llm ,value=lval) ;
  CALL xios_set_domain_attr("domain_A",ni_glo=ni_glo, nj_glo=nj_glo, ibegin=ibegin, iend=iend,jbegin=jbegin,jend=jend)
!  CALL xios_set_domain_attr("domain_A",zoom_ni=10,zoom_ibegin=5,zoom_nj=nj_glo,zoom_jbegin=1)
  CALL xios_set_domain_attr("domain_A",data_dim=1, data_ibegin=data_ibegin, data_ni=data_ni)
  CALL xios_set_domain_attr("domain_A",lonvalue=lon,latvalue=lat)
!  CALL xios_set_domain_attr("domain_A",mask=mask)
  CALL xios_set_fieldgroup_attr("field_definition",enabled=.TRUE.)
  
  CALL xios_get_handle("field_definition",fieldgroup_hdl)
  CALL xios_add_child(fieldgroup_hdl,field_hdl,"field_B")
  CALL xios_set_attr(field_hdl,field_ref="field_A",name="field_B")
  
  CALL xios_get_handle("output",file_hdl)
  CALL xios_add_child(file_hdl,field_hdl)
  CALL xios_set_attr(field_hdl,field_ref="field_A",name="field_C")
    
 
    dtime%second=3600
    CALL xios_set_timestep(dtime) 
    
!    ni=0 ; lonvalue(:)=0
!    CALL xios_get_domain_attr("domain_A",ni=ni,lonvalue=lonvalue)
    
!    print *,"ni",ni
!    print *,"lonvalue",lonvalue ;

    CALL xios_close_context_definition()
    
    PRINT*,"field field_A is active ? ",xios_field_is_active("field_A")
    DO ts=1,24*10
      CALL xios_update_calendar(ts)
      CALL xios_send_field("field_A",field_A)
      CALL wait_us(5000) ;
    ENDDO
  
    CALL xios_context_finalize()
    CALL xios_finalize()
     
  END SUBROUTINE client
  

  
  SUBROUTINE server
  USE xios
  IMPLICIT NONE
  
    CALL xios_init_server
 
  END SUBROUTINE server
  

  
