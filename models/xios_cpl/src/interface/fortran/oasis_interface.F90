SUBROUTINE fxios_oasis_init(server_id,str_len) BIND(C,NAME="fxios_oasis_init")
  USE, INTRINSIC :: ISO_C_BINDING
#ifdef USE_OASIS
  USE mod_prism_proto
#endif
#ifdef USE_OA3MCT
  USE mod_prism
#endif
    CHARACTER(kind = C_CHAR),DIMENSION(*) :: server_id
    INTEGER(kind = C_INT),VALUE   :: str_len
    
    INTEGER                :: comp_id
    CHARACTER(len=str_len) :: oasis_server_id
    INTEGER                :: ierr
    INTEGER :: i
    
    DO i=1,str_len
      oasis_server_id(i:i)=server_id(i)
    ENDDO

#if defined USE_OASIS || defined USE_OA3MCT
    PRINT *,"---> before prism_init",oasis_server_id
    call flush(6)
    CALL prism_init_comp_proto (comp_id, oasis_server_id, ierr)
    PRINT *,"---> after prism_init",comp_id,ierr
    call flush(6)
#endif

END SUBROUTINE fxios_oasis_init

SUBROUTINE fxios_oasis_enddef() BIND(C,NAME="fxios_oasis_enddef")
  USE, INTRINSIC :: ISO_C_BINDING
#ifdef USE_OA3MCT
  USE mod_prism
#endif
  IMPLICIT NONE
  INTEGER :: ierr

#ifdef USE_OA3MCT
    PRINT *,"---> before prism_enddef"
    call flush(6)
    CALL prism_enddef_proto(ierr)
    PRINT *,"---> after prism_enddef",ierr
    call flush(6)
#endif

END SUBROUTINE fxios_oasis_enddef

SUBROUTINE fxios_oasis_finalize() BIND(C,NAME="fxios_oasis_finalize")
  USE, INTRINSIC :: ISO_C_BINDING
#ifdef USE_OASIS
  USE mod_prism_proto
#endif
#ifdef USE_OA3MCT
  USE mod_prism
#endif
  IMPLICIT NONE
  INTEGER :: ierr
  
#if defined USE_OASIS || defined USE_OA3MCT
    CALL prism_terminate_proto(ierr)
#endif
    
END SUBROUTINE fxios_oasis_finalize


SUBROUTINE fxios_oasis_get_localcomm(f_comm) BIND(C,NAME="fxios_oasis_get_localcomm")
  USE, INTRINSIC :: ISO_C_BINDING
#ifdef USE_OASIS
  USE mod_prism_get_localcomm_proto 
#endif
#ifdef USE_OA3MCT
  USE mod_prism
#endif
  IMPLICIT NONE
  INTEGER(kind=C_INT) :: f_comm
  
  INTEGER :: comm
  INTEGER :: ierr
    
#if defined USE_OASIS || defined USE_OA3MCT
      PRINT *,"---> before get localcomm"
      call flush(6)
    CALL prism_get_localcomm_proto(comm,ierr)
      PRINT *,"---> after get localcomm", comm,ierr
      call flush(6)
#endif
    f_comm=comm

END SUBROUTINE fxios_oasis_get_localcomm


SUBROUTINE fxios_oasis_get_intracomm(f_comm_client_server,client_id,str_len) BIND(C,NAME="fxios_oasis_get_intracomm")
  USE, INTRINSIC :: ISO_C_BINDING
#ifdef USE_OASIS
  USE mod_prism_get_comm 
#endif
#ifdef USE_OA3MCT
  USE mod_prism
#endif
  IMPLICIT NONE
  INTEGER(kind=C_INT) :: f_comm_client_server
  CHARACTER,DIMENSION(*) :: client_id
  INTEGER,VALUE          :: str_len
  
  INTEGER :: comm_client_server
  CHARACTER(len=str_len) :: oasis_client_id
  INTEGER :: ierr
  INTEGER :: i
    
    DO i=1,str_len
      oasis_client_id(i:i)=client_id(i)
    ENDDO
    
#if defined USE_OASIS || defined USE_OA3MCT
    CALL prism_get_intracomm(comm_client_server,oasis_client_id,ierr)
#endif

    f_comm_client_server=comm_client_server

END SUBROUTINE fxios_oasis_get_intracomm

SUBROUTINE fxios_oasis_get_intercomm(f_comm_client_server,client_id,str_len) BIND(C,NAME="fxios_oasis_get_intercomm")
  USE, INTRINSIC :: ISO_C_BINDING
#ifdef USE_OASIS
  USE mod_prism_get_comm 
#endif
#ifdef USE_OA3MCT
  USE mod_prism
#endif
  IMPLICIT NONE
  INTEGER(kind=C_INT) :: f_comm_client_server
  CHARACTER,DIMENSION(*) :: client_id
  INTEGER,VALUE          :: str_len
  
  INTEGER :: comm_client_server
  CHARACTER(len=str_len) :: oasis_client_id
  INTEGER :: ierr
  INTEGER :: i
    
    DO i=1,str_len
      oasis_client_id(i:i)=client_id(i)
    ENDDO
    
#if defined USE_OASIS || defined USE_OA3MCT
    PRINT *,"---> before prism_get_intercomm ", oasis_client_id
    call flush(6)
    CALL prism_get_intercomm(comm_client_server,oasis_client_id,ierr)
    PRINT *,"---> after prism_get_intercomm ", comm_client_server,ierr
    call flush(6)
#endif

    f_comm_client_server=comm_client_server

END SUBROUTINE fxios_oasis_get_intercomm
