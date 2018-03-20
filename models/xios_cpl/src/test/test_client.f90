PROGRAM test_client

  USE xios
  USE mod_wait
  IMPLICIT NONE
  INCLUDE "mpif.h"
  INTEGER :: rank
  INTEGER :: size
  INTEGER :: ierr
  
  CHARACTER(len=*),PARAMETER :: id="client"
  INTEGER :: comm
  TYPE(xios_time)      :: dtime
  TYPE(xios_context) :: ctx_hdl
  INTEGER,PARAMETER :: ni_glo=100
  INTEGER,PARAMETER :: nj_glo=100 
  INTEGER,PARAMETER :: llm=5 
  DOUBLE PRECISION  :: lval(llm)=1
  TYPE(xios_field) :: field_hdl
  TYPE(xios_fieldgroup) :: fieldgroup_hdl
  TYPE(xios_file) :: file_hdl
  LOGICAL :: ok
  
  DOUBLE PRECISION,DIMENSION(ni_glo,nj_glo) :: lon_glo,lat_glo
  DOUBLE PRECISION :: field_A_glo(ni_glo,nj_glo,llm)
  DOUBLE PRECISION,ALLOCATABLE :: lon(:,:),lat(:,:),field_A(:,:,:), lonvalue(:) ;
  INTEGER :: ni,ibegin,iend,nj,jbegin,jend
  INTEGER :: i,j,l,ts,n

  CALL MPI_INIT(ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierr)
  
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

  jbegin=1
  DO n=0,size-1
    nj=nj_glo/size
    IF (n<MOD(nj_glo,size)) nj=nj+1
    IF (n==rank) exit 
    jbegin=jbegin+nj
  ENDDO
  
  iend=ibegin+ni-1 ; jend=jbegin+nj-1

  ALLOCATE(lon(ni,nj),lat(ni,nj),field_A(0:ni+1,-1:nj+2,llm),lonvalue(ni*nj))
  lon(:,:)=lon_glo(ibegin:iend,jbegin:jend)
  lat(:,:)=lat_glo(ibegin:iend,jbegin:jend)
  field_A(1:ni,1:nj,:)=field_A_glo(ibegin:iend,jbegin:jend,:)
  

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
  CALL xios_set_domain_attr("domain_A",ni_glo=ni_glo, nj_glo=nj_glo, ibegin=ibegin, ni=ni,jbegin=jbegin,nj=nj)
  !CALL xios_set_domain_attr("domain_A",zoom_ni=3,zoom_ibegin=3,zoom_nj=3,zoom_jbegin=6)
  CALL xios_set_domain_attr("domain_A",data_dim=2, data_ibegin=-1, data_ni=ni+2, data_jbegin=-2, data_nj=nj+4)
  CALL xios_set_domain_attr("domain_A",lonvalue=RESHAPE(lon,(/ni*nj/)),latvalue=RESHAPE(lat,(/ni*nj/)))
  CALL xios_set_fieldgroup_attr("field_definition",enabled=.TRUE.)
  
  CALL xios_get_handle("field_definition",fieldgroup_hdl)
  CALL xios_add_child(fieldgroup_hdl,field_hdl,"field_B")
  CALL xios_set_attr(field_hdl,field_ref="field_A",name="field_B")
  
  CALL xios_get_handle("output",file_hdl)
  CALL xios_add_child(file_hdl,field_hdl)
  CALL xios_set_attr(field_hdl,field_ref="field_A",name="field_C")
    
 
    dtime%second=3600
    CALL xios_set_timestep(dtime) 
    
    ni=0 ; lonvalue(:)=0
    CALL xios_get_domain_attr("domain_A",ni=ni,lonvalue=lonvalue)
    
    print *,"ni",ni
    print *,"lonvalue",lonvalue ;

    CALL xios_is_defined_field_attr("field_A",enabled=ok)
    PRINT *,"field_A : attribute enabled is defined ? ",ok
    CALL xios_close_context_definition()
    
    PRINT*,"field field_A is active ? ",xios_field_is_active("field_A")
    DO ts=1,24*10
      CALL xios_update_calendar(ts)
      CALL xios_send_field("field_A",field_A)
      CALL wait_us(5000) ;
    ENDDO
  
    CALL xios_context_finalize()
    CALL xios_finalize()
  
  CALL MPI_FINALIZE(ierr)
  
END PROGRAM test_client


  

  
