MODULE readcoordmesh
  !!=====================================================================
  !!                       ***  MODULE  readcoordmesh  ***
  !! 
  !! History: 2011: Clement Bricaud, Mercator-Ocean
  !! 
  !!=====================================================================
  !! * Modules used
  USE netcdf
  USE declarations

  IMPLICIT NONE
  PRIVATE
  
  PUBLIC  read_coord_mesh

CONTAINS

  SUBROUTINE read_coord_mesh
  !!---------------------------------------------------------------------
  !!              ***  ROUTINE coord_mesh_read  ***
  !!
  !! ** Purpose :   Read a coordinate file and a meshmask file in NetCDF format 
  !!
  !!---------------------------------------------------------------------      
  PRINT*,'              '
  PRINT*,'READ COORDINATES AND MESHMASK'
  PRINT*,'-----------------------------'

  ! Get coordinates dimensions
  CALL getdim(cdfile="coordinates.nc")

  !Allocate coordinates array with domain size
  ALLOCATE(glamt(jpi,jpj)) ; ALLOCATE(gphit(jpi,jpj))
  ALLOCATE(glamf(jpi,jpj)) ; ALLOCATE(gphif(jpi,jpj))
  ALLOCATE(e1t(jpi,jpj)  )   

  !Read glamt
  CALL read_ncdf(cdfile="coordinates.nc",cdvar="glamt",ksize=(/jpi,jpj,1,1/),ptab=glamt)

  !Read gphit
  CALL read_ncdf(cdfile="coordinates.nc",cdvar="gphit",ksize=(/jpi,jpj,1,1/),ptab=gphit)

  !Read glamf
  CALL read_ncdf(cdfile="coordinates.nc",cdvar="glamf",ksize=(/jpi,jpj,1,1/),ptab=glamf)

  !Read gphif
  CALL read_ncdf(cdfile="coordinates.nc",cdvar="gphif",ksize=(/jpi,jpj,1,1/),ptab=gphif)

  !Read e1t
  CALL read_ncdf(cdfile="coordinates.nc",cdvar="e1t",ksize=(/jpi,jpj,1,1/),ptab=e1t)

  END SUBROUTINE read_coord_mesh

  SUBROUTINE getdim(cdfile)
  !!----------------------------------------------------------------------
  !!              ***  ROUTINE getdim  ***
  !!
  !! ** Purpose :   get dimensions of a netcdf file
  !!
  !!----------------------------------------------------------------------
  !! * Arguments
  CHARACTER(*),INTENT(IN):: cdfile

  !! * Local declarations
  INTEGER           :: ncid                 ! file unit
  INTEGER           :: idims                ! number of dimensions
  INTEGER           :: istatus, id_var      ! dummy variable
  CHARACTER(len=30) :: clname               ! dimension name   
  INTEGER, ALLOCATABLE,DIMENSION(:) :: ndim ! dimension value
  !!----------------------------------------------------------------------

  !Open file
  istatus=NF90_OPEN(TRIM(cdfile),nf90_nowrite,ncid)

  IF( istatus/= NF90_NOERR )THEN
     PRINT*,TRIM(cdfile),' not found.stop ' ; STOP
  ELSE
 
     ! read number of dimensions   
     istatus=NF90_INQUIRE(ncid,ndimensions=idims)

     ALLOCATE( ndim(idims) )

     ! read each dimension
     PRINT*,'     File dimensions: '
     DO id_var=1,idims
        istatus=NF90_Inquire_Dimension(ncid,id_var,clname,ndim(id_var))
        PRINT*,'       ',id_var,clname,ndim(id_var)
     ENDDO

     !close
     istatus=NF90_CLOSE( ncid )              
     IF( istatus/=NF90_NOERR )THEN 
        PRINT*,'Problem for closing ',TRIM(cdfile);STOP
     ELSE
        PRINT*,'     close ',TRIM(cdfile),' OK'
     ENDIF

  ENDIF

  !domain dimensions
  jpi = ndim(1)
  jpj = ndim(2)

  DEALLOCATE( ndim )
  END SUBROUTINE getdim

  SUBROUTINE read_ncdf(cdfile,cdvar,ksize,ptab,kstart,kcount)
  !!----------------------------------------------------------------------
  !!              ***  ROUTINE coord_mesh_read  ***
  !!
  !! ** Purpose :   Read a coordinate and a meshmask file in NetCDF format
  !!
  !!----------------------------------------------------------------------
  !! * Arguments
  CHARACTER(*),        INTENT(IN)                                    :: cdfile
  CHARACTER(*),        INTENT(IN)                                    :: cdvar
  INTEGER,DIMENSION(4),INTENT(IN)                                    :: ksize
  INTEGER,DIMENSION(4),INTENT(IN),OPTIONAL                           :: kstart,kcount
  REAL(wp),DIMENSION(ksize(1),ksize(2),ksize(3),ksize(4)),INTENT(OUT):: ptab

  !! * Local declarations
  INTEGER                                 ::istatus,ncid,id_var,len
  CHARACTER(len=30) :: clname , cdvar2

  INTEGER :: idims
  INTEGER,DIMENSION(3)::idimids 
  !!----------------------------------------------------------------------
  ptab=0.
  PRINT*,'read ',TRIM(cdvar),' in ',TRIM(cdfile)
  
  !OPEN
  !----
  istatus=NF90_OPEN(TRIM(cdfile),nf90_nowrite,ncid)
  IF( istatus/= NF90_NOERR )THEN
     PRINT*,TRIM(cdfile),' not found.stop ' ; STOP
  ENDIF

  !READ
  !----
  !search variable
  istatus=NF90_INQ_VARID (ncid,TRIM(cdvar),id_var)
  IF( istatus/=NF90_NOERR )THEN
      PRINT*,TRIM(cdvar),' not found in ',TRIM(cdfile),'.stop';STOP
  ENDIF

  !get variable
  !------------
  istatus=nf90_inquire_variable(ncid,id_var, cdvar2, ndims=idims, dimids=idimids)
  IF ( PRESENT(kstart) .AND. PRESENT(kcount) )THEN  
      istatus=NF90_GET_VAR(ncid,id_var,ptab,start=kstart,count=kcount)
  ELSE
      istatus=NF90_GET_VAR(ncid,id_var,ptab)
  ENDIF

  CALL ERR_HDL(istatus)

  IF( istatus/=NF90_NOERR )THEN
           PRINT*,'Problem for reading ',TRIM(cdvar),' in ',TRIM(cdfile); STOP
  ELSE
      PRINT*,'     read ',TRIM(cdvar),' OK'
  ENDIF

  !CLOSE
  !-----
  istatus=NF90_CLOSE( ncid )
  IF( istatus/=NF90_NOERR )THEN
      PRINT*,'Problem for closing ',TRIM(cdfile);stop
  ELSE
      PRINT*,'     close ',TRIM(cdfile),' OK'
  ENDIF


  END SUBROUTINE read_ncdf

  SUBROUTINE ERR_HDL(kstatus)
  !! ----------------------------------------------------------
  !!   ***  SUBROUTINE err_hdl
  !!
  !!   ** Purpose :  Error handle for NetCDF routine.
  !!          Stop if kstatus indicates error conditions.
  !!
  !! History :
  !!     Original: J.M. Molines (01/99)
  !!
  !! -----------------------------------------------------------
  INTEGER, INTENT(in) ::  kstatus

  !! -----------------------------------------------------------
  IF( kstatus /=  NF90_NOERR ) THEN
     PRINT *, 'ERROR in NETCDF routine, status=',kstatus
     PRINT *,NF90_STRERROR(kstatus)
     STOP
  END IF

  END SUBROUTINE ERR_HDL

END MODULE readcoordmesh
