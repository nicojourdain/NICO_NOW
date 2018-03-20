MODULE readwrite
  !
  USE types
  !
  IMPLICIT NONE
  !
CONTAINS
  !       
  !*****************************************************
  !   function Read_Coordinates(name,Grid)
  !*****************************************************
  INTEGER FUNCTION read_coordinates(name,Grid)
    !
    USE io_netcdf
    !         
    CHARACTER(*) name
    TYPE(coordinates) :: Grid
    !      
    CALL Read_Ncdf_var('glamt',name,Grid%glamt)
    CALL Read_Ncdf_var('glamu',name,Grid%glamu)
    CALL Read_Ncdf_var('glamv',name,Grid%glamv)
    CALL Read_Ncdf_var('glamf',name,Grid%glamf)
    CALL Read_Ncdf_var('gphit',name,Grid%gphit)
    CALL Read_Ncdf_var('gphiu',name,Grid%gphiu)
    CALL Read_Ncdf_var('gphiv',name,Grid%gphiv)
    CALL Read_Ncdf_var('gphif',name,Grid%gphif)
    CALL Read_Ncdf_var('e1t',name,Grid%e1t)
    CALL Read_Ncdf_var('e1u',name,Grid%e1u)
    CALL Read_Ncdf_var('e1v',name,Grid%e1v)
    CALL Read_Ncdf_var('e1f',name,Grid%e1f)
    CALL Read_Ncdf_var('e2t',name,Grid%e2t)
    CALL Read_Ncdf_var('e2u',name,Grid%e2u)
    CALL Read_Ncdf_var('e2v',name,Grid%e2v)
    CALL Read_Ncdf_var('e2f',name,Grid%e2f)
    CALL Read_Ncdf_var('nav_lon',name,Grid%nav_lon)
    CALL Read_Ncdf_var('nav_lat',name,Grid%nav_lat)       
    !            
    WRITE(*,*) ' '
    WRITE(*,*) '*** Reading coordinates file: ',name
    WRITE(*,*) ' '
    !      
    read_coordinates = 1
    !      
  END FUNCTION read_coordinates
  !
  !
  !
  !*****************************************************
  !   function Write_Coordinates(name,Grid)
  !*****************************************************
  INTEGER FUNCTION write_coordinates(name,Grid,kxfine,kyfine)
    !
    USE io_netcdf
	!
    CHARACTER(*) name
    TYPE(coordinates) :: Grid
    INTEGER :: status,ncid,z
    REAL*8,DIMENSION(:),POINTER :: tabtemp
    INTEGER,DIMENSION(:),POINTER :: tabint
    CHARACTER(len=20),DIMENSION(4) :: dimnames
	INTEGER :: kxfine, kyfine
    !
    status = nf90_create(name,NF90_WRITE,ncid)
    status = nf90_close(ncid)
    !           
	CALL Write_Ncdf_dim('x',name,nxfine)
    CALL Write_Ncdf_dim('y',name,nyfine)
    IF(.NOT. ln_iom_activated) CALL Write_Ncdf_dim('z',name,1)
    CALL Write_Ncdf_dim('time',name,0)
    !      
    dimnames(1)='x'
    dimnames(2)='y'
    CALL Write_Ncdf_var('nav_lon',dimnames(1:2),name,Grid%nav_lon,'float')      
    CALL Write_Ncdf_var('nav_lat',dimnames(1:2),name,Grid%nav_lat,'float')
    !
    IF(.NOT. ln_iom_activated) THEN
       ! copy nav_lev variable -> IOIPSL
       CALL Read_Ncdf_dim('z',cn_parent_coordinate_file,z)
       ALLOCATE(tabtemp(z))
       CALL Read_Ncdf_var('nav_lev',TRIM(cn_parent_coordinate_file),tabtemp)
       CALL Write_Ncdf_var('nav_lev','z',name,tabtemp,'float')           
       DEALLOCATE(tabtemp)
    ENDIF
    !
    CALL Read_Ncdf_var('time',TRIM(cn_parent_coordinate_file),tabtemp)
    CALL Write_Ncdf_var('time','time',name,tabtemp,'float')           
    DEALLOCATE(tabtemp)      
    CALL Read_Ncdf_var('time_steps',TRIM(cn_parent_coordinate_file),tabint)
    CALL Write_Ncdf_var('time_steps','time',name,tabint) 
    !      
    dimnames(1)='x'
    dimnames(2)='y'
    IF(ln_iom_activated) THEN
       dimnames(3)='time'
    ELSE
       dimnames(3)='z'
       dimnames(4)='time'
    ENDIF

    CALL Write_Ncdf_var('glamt',dimnames,name,Grid%glamt,3,'double')
    CALL Write_Ncdf_var('glamu',dimnames,name,Grid%glamu,3,'double')
    CALL Write_Ncdf_var('glamv',dimnames,name,Grid%glamv,3,'double')
    CALL Write_Ncdf_var('glamf',dimnames,name,Grid%glamf,3,'double')
    CALL Write_Ncdf_var('gphit',dimnames,name,Grid%gphit,3,'double')
    CALL Write_Ncdf_var('gphiu',dimnames,name,Grid%gphiu,3,'double')
    CALL Write_Ncdf_var('gphiv',dimnames,name,Grid%gphiv,3,'double')
    CALL Write_Ncdf_var('gphif',dimnames,name,Grid%gphif,3,'double')      
    CALL Write_Ncdf_var('e1t',dimnames,name,Grid%e1t,3,'double')      
    CALL Write_Ncdf_var('e1u',dimnames,name,Grid%e1u,3,'double')     
    CALL Write_Ncdf_var('e1v',dimnames,name,Grid%e1v,3,'double')      
    CALL Write_Ncdf_var('e1f',dimnames,name,Grid%e1f,3,'double')
    CALL Write_Ncdf_var('e2t',dimnames,name,Grid%e2t,3,'double')
    CALL Write_Ncdf_var('e2u',dimnames,name,Grid%e2u,3,'double')
    CALL Write_Ncdf_var('e2v',dimnames,name,Grid%e2v,3,'double')
    CALL Write_Ncdf_var('e2f',dimnames,name,Grid%e2f,3,'double')
    !      
    CALL Copy_Ncdf_att('nav_lon',TRIM(cn_parent_coordinate_file),name,&
         MINVAL(Grid%nav_lon),MAXVAL(Grid%nav_lon))
    CALL Copy_Ncdf_att('nav_lat',TRIM(cn_parent_coordinate_file),name,&
         MINVAL(Grid%nav_lat),MAXVAL(Grid%nav_lat))
    CALL Copy_Ncdf_att('nav_lev',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('time',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('time_steps',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('glamt',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('glamu',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('glamv',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('glamf',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('gphit',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('gphiu',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('gphiv',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('gphif',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e1t',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e1u',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e1v',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e1f',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e2t',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e2u',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e2v',TRIM(cn_parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e2f',TRIM(cn_parent_coordinate_file),name)            
    !
    WRITE(*,*) ' '
    WRITE(*,*) 'Writing coordinates file: ',name
    IF(.NOT. ln_iom_activated) WRITE(*,*) 'IOISPL format'
    IF(ln_iom_activated) WRITE(*,*) 'IOM format'      
    WRITE(*,*) ' '
    !
    write_coordinates = 1
    !      
  END FUNCTION write_coordinates
  !
  !
  !
  !*****************************************************
  !   function set_child_name(Parentname,Childname)
  !*****************************************************
  SUBROUTINE set_child_name(Parentname,Childname)
    !
    CHARACTER(*),INTENT(in) :: Parentname
    CHARACTER(*),INTENT(out) :: Childname
    CHARACTER(2) :: prefix
    INTEGER :: pos
    !   
    pos  = INDEX(TRIM(Parentname),'/',back=.TRUE.)
    !
    prefix=Parentname(pos+1:pos+2)
    IF (prefix == '1_') THEN
       Childname = '2_'//Parentname(pos+3:LEN(Parentname)) 
    ELSEIF (prefix == '2_') THEN
       Childname = '3_'//Parentname(pos+3:LEN(Parentname)) 
    ELSEIF (prefix == '3_') THEN
       Childname = '4_'//Parentname(pos+3:LEN(Parentname)) 
    ELSEIF (prefix == '4_') THEN
       Childname = '5_'//Parentname(pos+3:LEN(Parentname)) 
    ELSE
       Childname = '1_'//Parentname(pos+1:LEN(Parentname)) 
    ENDIF
    !   
  END SUBROUTINE set_child_name
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE readwrite
