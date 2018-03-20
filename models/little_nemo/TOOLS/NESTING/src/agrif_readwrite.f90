!************************************************************************
! Fortran 95 OPA Nesting tools						*
!									*
!     Copyright (C) 2005 Florian Lemarié (Florian.Lemarie@imag.fr)	*
!									*
!************************************************************************
!
MODULE agrif_readwrite
  !
  USE agrif_types
  !
  IMPLICIT NONE
  !
CONTAINS
  !
  !************************************************************************
  ! 									*
  ! MODULE  AGRIF_READWRITE						*
  !									*
  ! module containing subroutine used for : 				*
  !   - Coordinates files reading/writing				*
  !   - Bathymetry files reading/writing (meter and levels)		*
  !   - Naming of child grid files					*
  !									*
  !************************************************************************
  !       
  !*****************************************************
  !   function Read_Coordinates(name,Grid)
  !*****************************************************

  INTEGER FUNCTION Read_Coordinates(name,Grid,Pacifique)
    !
    USE io_netcdf
    !    
    !  file name to open
    ! 
    CHARACTER(*) name
    LOGICAL,OPTIONAL :: Pacifique
    ! 
    TYPE(Coordinates) :: Grid
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
    IF( PRESENT(Pacifique) )THEN
       IF ( Grid%glamt(1,1) > Grid%glamt(nxfin,nyfin) ) THEN           
       Pacifique = .TRUE.
       WHERE ( Grid%glamt < 0 )
          Grid%glamt = Grid%glamt + 360.
       END WHERE
       WHERE ( Grid%glamf < 0 )
          Grid%glamf = Grid%glamf + 360.
       END WHERE
       WHERE ( Grid%glamu < 0 )
          Grid%glamu = Grid%glamu + 360.
       END WHERE
       WHERE ( Grid%glamv < 0 )
          Grid%glamv = Grid%glamv + 360.
       END WHERE
       WHERE ( Grid%nav_lon < 0 )
          Grid%nav_lon = Grid%nav_lon + 360.
       END WHERE
       ENDIF
    ENDIF
    !            
    WRITE(*,*) ' '
    WRITE(*,*) 'Reading coordinates file: ',name
    WRITE(*,*) ' '
    !      
    Read_Coordinates = 1
    !      
  END FUNCTION Read_Coordinates

  !*****************************************************
  !   function Read_Coordinates(name,Grid)
  !*****************************************************

  INTEGER FUNCTION Read_Local_Coordinates(name,Grid,strt,cnt)
    !
    USE io_netcdf
    !    
    !  file name to open
    ! 
    CHARACTER(*) name
    INTEGER, DIMENSION(2) :: strt,cnt
    ! 
    TYPE(Coordinates) :: Grid
    !      
    CALL Read_Ncdf_var('glamt',name,Grid%glamt,strt,cnt)
    CALL Read_Ncdf_var('glamu',name,Grid%glamu,strt,cnt)
    CALL Read_Ncdf_var('glamv',name,Grid%glamv,strt,cnt)
    CALL Read_Ncdf_var('glamf',name,Grid%glamf,strt,cnt)
    CALL Read_Ncdf_var('gphit',name,Grid%gphit,strt,cnt)
    CALL Read_Ncdf_var('gphiu',name,Grid%gphiu,strt,cnt)
    CALL Read_Ncdf_var('gphiv',name,Grid%gphiv,strt,cnt)
    CALL Read_Ncdf_var('gphif',name,Grid%gphif,strt,cnt)
    CALL Read_Ncdf_var('e1t',name,Grid%e1t,strt,cnt)
    CALL Read_Ncdf_var('e1u',name,Grid%e1u,strt,cnt)
    CALL Read_Ncdf_var('e1v',name,Grid%e1v,strt,cnt)
    CALL Read_Ncdf_var('e1f',name,Grid%e1f,strt,cnt)
    CALL Read_Ncdf_var('e2t',name,Grid%e2t,strt,cnt)
    CALL Read_Ncdf_var('e2u',name,Grid%e2u,strt,cnt)
    CALL Read_Ncdf_var('e2v',name,Grid%e2v,strt,cnt)
    CALL Read_Ncdf_var('e2f',name,Grid%e2f,strt,cnt)
    CALL Read_Ncdf_var('nav_lon',name,Grid%nav_lon,strt,cnt)
    CALL Read_Ncdf_var('nav_lat',name,Grid%nav_lat,strt,cnt)       
    !
    WRITE(*,*) ' '
    WRITE(*,*) 'Reading coordinates file: ',name
    WRITE(*,*) ' '
    !      
    Read_Local_Coordinates = 1
    !      
  END FUNCTION Read_Local_Coordinates

  !*****************************************************
  !   function Write_Coordinates(name,Grid)
  !*****************************************************

  INTEGER FUNCTION Write_Coordinates(name,Grid)
    !
    USE io_netcdf
    CHARACTER(*) name
    TYPE(Coordinates) :: Grid
    INTEGER :: status,ncid
    CHARACTER(len=1),DIMENSION(2) :: dimnames
    !
    status = nf90_create(name,NF90_WRITE,ncid)
    status = nf90_close(ncid)
    !           
    dimnames = (/ 'x','y' /)
    CALL Write_Ncdf_dim(dimnames(1),name,nxfin)
    CALL Write_Ncdf_dim(dimnames(2),name,nyfin)
    !      
    CALL Write_Ncdf_var('nav_lon',dimnames,name,Grid%nav_lon,'float')      
    CALL Write_Ncdf_var('nav_lat',dimnames,name,Grid%nav_lat,'float')
    !
    CALL Write_Ncdf_var('glamt',dimnames,name,Grid%glamt,'double')
    CALL Write_Ncdf_var('glamu',dimnames,name,Grid%glamu,'double')
    CALL Write_Ncdf_var('glamv',dimnames,name,Grid%glamv,'double')
    CALL Write_Ncdf_var('glamf',dimnames,name,Grid%glamf,'double')
    CALL Write_Ncdf_var('gphit',dimnames,name,Grid%gphit,'double')
    CALL Write_Ncdf_var('gphiu',dimnames,name,Grid%gphiu,'double')
    CALL Write_Ncdf_var('gphiv',dimnames,name,Grid%gphiv,'double')
    CALL Write_Ncdf_var('gphif',dimnames,name,Grid%gphif,'double')      
    CALL Write_Ncdf_var('e1t',dimnames,name,Grid%e1t,'double')      
    CALL Write_Ncdf_var('e1u',dimnames,name,Grid%e1u,'double')     
    CALL Write_Ncdf_var('e1v',dimnames,name,Grid%e1v,'double')      
    CALL Write_Ncdf_var('e1f',dimnames,name,Grid%e1f,'double')
    CALL Write_Ncdf_var('e2t',dimnames,name,Grid%e2t,'double')
    CALL Write_Ncdf_var('e2u',dimnames,name,Grid%e2u,'double')
    CALL Write_Ncdf_var('e2v',dimnames,name,Grid%e2v,'double')
    CALL Write_Ncdf_var('e2f',dimnames,name,Grid%e2f,'double')
    !      
    CALL Copy_Ncdf_att('nav_lon',TRIM(parent_coordinate_file),name,MINVAL(Grid%nav_lon),MAXVAL(Grid%nav_lon))
    CALL Copy_Ncdf_att('nav_lat',TRIM(parent_coordinate_file),name,MINVAL(Grid%nav_lat),MAXVAL(Grid%nav_lat))
    CALL Copy_Ncdf_att('glamt',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('glamu',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('glamv',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('glamf',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('gphit',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('gphiu',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('gphiv',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('gphif',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e1t',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e1u',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e1v',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e1f',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e2t',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e2u',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e2v',TRIM(parent_coordinate_file),name)
    CALL Copy_Ncdf_att('e2f',TRIM(parent_coordinate_file),name)            
    !
    WRITE(*,*) ' '
    WRITE(*,*) 'Writing coordinates file: ',name
    WRITE(*,*) ' '
    !
    Write_Coordinates = 1
    !      
  END FUNCTION Write_Coordinates
  !
  !
  !
  !*****************************************************
  !   function Read_Bathy_level(name,Grid)
  !*****************************************************
  !
  INTEGER FUNCTION Read_Bathy_level(name,Grid)
    !
    USE io_netcdf
    !      
    CHARACTER(*) name
    TYPE(Coordinates) :: Grid
    !
    CALL Read_Ncdf_var('mbathy',name,Grid%Bathy_level)    
    !
    WRITE(*,*) ' '
    WRITE(*,*) 'Reading bathymetry file: ',name
    WRITE(*,*) ' '      
    !
    Read_Bathy_level = 1
    !      
  END FUNCTION Read_Bathy_level
  !
  !*****************************************************
  !   function Write_Bathy_level(name,Grid)
  !*****************************************************
  !
  INTEGER FUNCTION Write_Bathy_level(name,Grid)
    !
    USE io_netcdf
    !      
    CHARACTER(*) name
    TYPE(Coordinates) :: Grid
    INTEGER :: status,ncid
    CHARACTER(len=1),DIMENSION(2) :: dimnames
    !
    status = nf90_create(name,NF90_WRITE,ncid)
    status = nf90_close(ncid)          
    !
    dimnames = (/ 'x','y' /)
    CALL Write_Ncdf_dim(dimnames(1),name,nxfin)
    CALL Write_Ncdf_dim(dimnames(2),name,nyfin)
    !      
    CALL Write_Ncdf_var('nav_lon',dimnames,name,Grid%nav_lon    ,'float')
    CALL Write_Ncdf_var('nav_lat',dimnames,name,Grid%nav_lat    ,'float')
    CALL Write_Ncdf_var('mbathy' ,dimnames,name,Grid%bathy_level,'float')
    !
    CALL Copy_Ncdf_att('nav_lon',TRIM(parent_meshmask_file),name,MINVAL(Grid%nav_lon),MAXVAL(Grid%nav_lon))
    CALL Copy_Ncdf_att('nav_lat',TRIM(parent_meshmask_file),name,MINVAL(Grid%nav_lat),MAXVAL(Grid%nav_lat))
    CALL Copy_Ncdf_att('mbathy' ,TRIM(parent_meshmask_file),name)       
    !
    WRITE(*,*) ' '
    WRITE(*,*) 'Writing bathymetry file: ',name
    WRITE(*,*) ' '
    !
    Write_Bathy_level = 1
    !
  END FUNCTION Write_Bathy_level
  !
  !*****************************************************
  !   function Read_Bathy_meter(name,CoarseGrid,ChildGrid)
  !*****************************************************
  !
  INTEGER FUNCTION Read_Bathy_meter(name,CoarseGrid,ChildGrid,Pacifique)
    !
    USE io_netcdf
    CHARACTER(*) name
    INTEGER :: i,j,tabdim1,tabdim2
    INTEGER, DIMENSION(1) :: i_min,i_max,j_min,j_max
    REAL*8,POINTER,DIMENSION(:) :: topo_lon,topo_lat
    INTEGER :: status,ncid,varid 
    LOGICAL,OPTIONAL :: Pacifique
    TYPE(Coordinates) :: CoarseGrid,ChildGrid
    !
    IF( Dims_Existence('lon',name) .AND. Dims_Existence('lat',name) ) THEN
       WRITE(*,*) '****'
       WRITE(*,*) ' etopo format for external high resolution database  '
       WRITE(*,*) '****'
       CALL Read_Ncdf_var('lon',name,topo_lon)
       CALL Read_Ncdf_var('lat',name,topo_lat)
    ELSE IF( Dims_Existence('x',name) .AND. Dims_Existence('y',name) ) THEN
       WRITE(*,*) '****'
       WRITE(*,*) ' OPA format for external high resolution database  '
       WRITE(*,*) '****'
       CALL Read_Ncdf_var('nav_lon',name,CoarseGrid%nav_lon)
       CALL Read_Ncdf_var('nav_lat',name,CoarseGrid%nav_lat)
       CALL Read_Ncdf_var(parent_batmet_name,name,CoarseGrid%Bathy_meter)
       !            
       IF ( PRESENT(Pacifique) ) THEN
          IF(Pacifique) THEN
             WHERE(CoarseGrid%nav_lon < 0.001) 
                CoarseGrid%nav_lon = CoarseGrid%nav_lon + 360.
             END WHERE
          ENDIF
       ENDIF
       !      
       Read_Bathy_meter = 1
       RETURN      
    ELSE
       WRITE(*,*) '****'
       WRITE(*,*) '*** ERROR Bad format for external high resolution database'
       WRITE(*,*) '****'
       STOP  
    ENDIF
    !
    IF( MAXVAL(ChildGrid%glamt) > 180 ) THEN                 
       !          
       WHERE( topo_lon < 0 )
          topo_lon = topo_lon + 360.
       END WHERE
       !          
       i_min = MAXLOC(topo_lon,mask = topo_lon < MINVAL(ChildGrid%nav_lon))
       i_max = MINLOC(topo_lon,mask = topo_lon > MAXVAL(ChildGrid%nav_lon))                    
       j_min = MAXLOC(topo_lat,mask = topo_lat < MINVAL(ChildGrid%nav_lat))
       j_max = MINLOC(topo_lat,mask = topo_lat > MAXVAL(ChildGrid%nav_lat))
       !          
       tabdim1 = ( SIZE(topo_lon) - i_min(1) + 1 ) + i_max(1)                    
       !          
       IF(j_min(1)-2 >= 1 .AND. j_max(1)+3 <= SIZE(topo_lat,1) ) THEN
          j_min(1) = j_min(1)-2
          j_max(1) = j_max(1)+3
       ENDIF
       tabdim2 = j_max(1) - j_min(1) + 1
       !
       ALLOCATE(CoarseGrid%nav_lon(tabdim1,tabdim2))
       ALLOCATE(CoarseGrid%nav_lat(tabdim1,tabdim2))
       ALLOCATE(CoarseGrid%Bathy_meter(tabdim1,tabdim2))          
       !
       DO i = 1,tabdim1
          CoarseGrid%nav_lat(i,:) = topo_lat(j_min(1):j_max(1))
       END DO
       !
       DO j = 1, tabdim2
          !                     
          CoarseGrid%nav_lon(1:SIZE(topo_lon)-i_min(1)+1      ,j) = topo_lon(i_min(1):SIZE(topo_lon))
          CoarseGrid%nav_lon(2+SIZE(topo_lon)-i_min(1):tabdim1,j) = topo_lon(1:i_max(1))
          !    
       END DO
       status = nf90_open(name,NF90_NOWRITE,ncid)
       status = nf90_inq_varid(ncid,elevation_name,varid)
       !          
       status=nf90_get_var(ncid,varid,CoarseGrid%Bathy_meter(1:SIZE(topo_lon)-i_min(1)+1,:), &
            start=(/i_min(1),j_min(1)/),count=(/SIZE(topo_lon)-i_min(1),tabdim2/))

       status=nf90_get_var(ncid,varid,CoarseGrid%Bathy_meter(2+SIZE(topo_lon)-i_min(1):tabdim1,:), &
            start=(/1,j_min(1)/),count=(/i_max(1),tabdim2/))                
       !
    ELSE
       !
       i_min = MAXLOC(topo_lon,mask = topo_lon < MINVAL(ChildGrid%nav_lon))
       i_max = MINLOC(topo_lon,mask = topo_lon > MAXVAL(ChildGrid%nav_lon))
       j_min = MAXLOC(topo_lat,mask = topo_lat < MINVAL(ChildGrid%nav_lat))
       j_max = MINLOC(topo_lat,mask = topo_lat > MAXVAL(ChildGrid%nav_lat))
       !      
       IF(i_min(1)-2 >= 1 .AND. i_max(1)+3 <= SIZE(topo_lon,1) ) THEN
          i_min(1) = i_min(1)-2
          i_max(1) = i_max(1)+3
       ENDIF
       tabdim1 = i_max(1) - i_min(1) + 1
       !
       IF(j_min(1)-2 >= 1 .AND. j_max(1)+3 <= SIZE(topo_lat,1) ) THEN
          j_min(1) = j_min(1)-2
          j_max(1) = j_max(1)+3
       ENDIF
       tabdim2 = j_max(1) - j_min(1) + 1
       !
       WRITE(*,*) ' '
       WRITE(*,*) 'Reading bathymetry file: ',name
       WRITE(*,*) ' '
       !
       ALLOCATE(CoarseGrid%nav_lon(tabdim1,tabdim2))
       ALLOCATE(CoarseGrid%nav_lat(tabdim1,tabdim2))
       ALLOCATE(CoarseGrid%Bathy_meter(tabdim1,tabdim2))
       !
       DO j = 1,tabdim2
          CoarseGrid%nav_lon(:,j) = topo_lon(i_min(1):i_max(1))
       END DO
       !      
       DO i = 1,tabdim1
          CoarseGrid%nav_lat(i,:) = topo_lat(j_min(1):j_max(1)) 
       END DO
       !
       status = nf90_open(name,NF90_NOWRITE,ncid)
       status = nf90_inq_varid(ncid,elevation_name,varid)
       status = nf90_get_var(ncid,varid,CoarseGrid%Bathy_meter, &
          &                  start=(/i_min(1),j_min(1)/),count=(/tabdim1,tabdim2/))
       !
    ENDIF
    !
    status = nf90_close(ncid)     
    !
    IF(MINVAL(CoarseGrid%Bathy_meter) < 0) CoarseGrid%Bathy_meter(:,:) = -1.0 * CoarseGrid%Bathy_meter(:,:)

    WHERE(CoarseGrid%Bathy_meter.LT.0)
       CoarseGrid%Bathy_meter = 0.0
    END WHERE

    Read_Bathy_meter = 1
    RETURN
    !      
  END FUNCTION Read_Bathy_meter
  ! 
  !
  !*****************************************************
  !   function Read_Bathy_meter(name,CoarseGrid,ChildGrid)
  !*****************************************************
  !
  INTEGER FUNCTION Read_Bathymeter(name,Grid)
    !
    !
    USE io_netcdf
    !      
    CHARACTER(*) name
    TYPE(Coordinates) :: Grid
    !
    CALL Read_Ncdf_var(parent_batmet_name,name,Grid%Bathy_meter)    
    !
    WRITE(*,*) ' '
    WRITE(*,*) 'Reading bathymetry file: ',name
    WRITE(*,*) ' '      
    !
    Read_Bathymeter = 1
    !      
  END FUNCTION Read_Bathymeter
  !     
  !*****************************************************
  !   function Write_Bathy_meter(name,Grid)
  !*****************************************************
  !
  INTEGER FUNCTION Write_Bathy_meter(name,Grid)
    !
    USE io_netcdf
    !
    CHARACTER(*) name
    TYPE(Coordinates) :: Grid
    INTEGER :: status,ncid
    CHARACTER(len=1),DIMENSION(2) :: dimnames
    INTEGER :: nx,ny
    !
    status = nf90_create(name,NF90_WRITE,ncid)
    status = nf90_close(ncid)     
    !
    nx = SIZE(Grid%bathy_meter,1)
    ny = SIZE(Grid%bathy_meter,2)
    dimnames = (/ 'x','y' /)

    CALL Write_Ncdf_dim(dimnames(1),name,nx)
    CALL Write_Ncdf_dim(dimnames(2),name,ny)
    !     
    CALL Write_Ncdf_var('nav_lon'         ,dimnames,name,Grid%nav_lon    ,'float')
    CALL Write_Ncdf_var('nav_lat'         ,dimnames,name,Grid%nav_lat    ,'float')
    CALL Write_Ncdf_var(parent_batmet_name,dimnames,name,Grid%bathy_meter,'float')
    !
    CALL Copy_Ncdf_att('nav_lon'         ,TRIM(parent_bathy_meter),name,MINVAL(Grid%nav_lon),MAXVAL(Grid%nav_lon))
    CALL Copy_Ncdf_att('nav_lat'         ,TRIM(parent_bathy_meter),name,MINVAL(Grid%nav_lat),MAXVAL(Grid%nav_lat))  
    CALL Copy_Ncdf_att(parent_batmet_name,TRIM(parent_bathy_meter),name)
    !
    WRITE(*,*) ' '
    WRITE(*,*) 'Writing bathymetry file: ',name
    WRITE(*,*) ' '
    !
    Write_Bathy_meter = 1
    !      
  END FUNCTION Write_Bathy_meter
  !
  !*****************************************************
  !   function set_child_name(Parentname,Childname)
  !*****************************************************
  !
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
  !*****************************************************
  !   function set_child_name(Parentname,Childname)
  !*****************************************************
  !
  !*****************************************************
  !   subroutine get_interptype(varname,interp_type,conservation)
  !*****************************************************
  !
  SUBROUTINE get_interptype( varname,interp_type,conservation )
    !
    LOGICAL,OPTIONAL :: conservation
    CHARACTER(*) :: interp_type,varname
    INTEGER :: pos,pos1,pos2,pos3,i,k 
    LOGICAL :: find        
    i=1
    DO WHILE ( TRIM(VAR_INTERP(i)) .NE. 'NULL' )     
       pos = INDEX( TRIM(VAR_INTERP(i)) , TRIM(varname) )
       IF ( pos .NE. 0 ) THEN      
          pos1 = INDEX( TRIM(VAR_INTERP(i)) , 'bicubic' )
          pos2 = INDEX( TRIM(VAR_INTERP(i)) , 'bilinear' )
          pos3 = INDEX( TRIM(VAR_INTERP(i)) , 'conservative' )
          ! initialize interp_type
          IF( pos1 .NE. 0 ) interp_type = 'bicubic'
          IF( pos2 .NE. 0 ) interp_type = 'bilinear'
          IF( pos1 .EQ. 0 .AND. pos2 .EQ. 0) interp_type = 'bicubic'
          ! initialize conservation                                           
          IF( pos3 .NE. 0 .AND. PRESENT(conservation) ) THEN
             conservation = .TRUE.
             RETURN
          ELSE 
             conservation = .FALSE.
          ENDIF
          find = .FALSE.
          IF( PRESENT(conservation) ) THEN
             k=0
             conservation = .FALSE.         
             DO WHILE( k < SIZE(flxtab) .AND. .NOT.find ) 
                k = k+1
                IF( TRIM(varname) .EQ. TRIM(flxtab(k)) ) THEN       
                   conservation = .TRUE.
                   find = .TRUE.
                ENDIF
             END DO
          ENDIF
          RETURN
       ENDIF
       i = i+1
    END DO
    !default values interp_type = bicubic // conservation = false      
    interp_type = 'bicubic'      
    IF( PRESENT(conservation) ) conservation = .FALSE.

    RETURN
    !   
  END SUBROUTINE get_interptype
  !      
  !*****************************************************              
  !   end subroutine get_interptype
  !*****************************************************
  !           
  !*****************************************************
  !   subroutine Init_mask(name,Grid)
  !*****************************************************
  !
  SUBROUTINE Init_mask(name,Grid,jpiglo,jpjglo)
    !
    USE io_netcdf
    !      
    CHARACTER(*) name
    INTEGER :: nx,ny,k,i,j,jpiglo,jpjglo
    TYPE(Coordinates) :: Grid
    REAL*8, POINTER, DIMENSION(:,:) ::zwf => NULL()
    !
    IF(jpiglo == 1 .AND. jpjglo == 1) THEN
       CALL Read_Ncdf_var('Bathy_level',name,Grid%Bathy_level)
    ELSE
       CALL Read_Ncdf_var('Bathy_level',name,Grid%Bathy_level,(/jpizoom,jpjzoom/),(/jpiglo,jpjglo/) )
    ENDIF


    !
    WRITE(*,*) 'Init masks in T,U,V,F points'    
    !
    nx = SIZE(Grid%Bathy_level,1)
    ny = SIZE(Grid%Bathy_level,2)
    !
    !      
    ALLOCATE(Grid%tmask(nx,ny,N), &
         Grid%umask(nx,ny,N), &
         Grid%vmask(nx,ny,N), &
         Grid%fmask(nx,ny,N))
    !
    DO k = 1,N
       !      
       WHERE(Grid%Bathy_level(:,:) <= k-1 )    
          Grid%tmask(:,:,k) = 0
       ELSEWHERE
          Grid%tmask(:,:,k) = 1
       END WHERE
       !
    END DO
    !
    Grid%umask(1:nx-1,:,:) = Grid%tmask(1:nx-1,:,:)*Grid%tmask(2:nx,:,:)
    Grid%vmask(:,1:ny-1,:) = Grid%tmask(:,1:ny-1,:)*Grid%tmask(:,2:ny,:)
    !
    Grid%umask(nx,:,:) = Grid%tmask(nx,:,:)
    Grid%vmask(:,ny,:) = Grid%tmask(:,ny,:)
    !      
    Grid%fmask(1:nx-1,1:ny-1,:) = Grid%tmask(1:nx-1,1:ny-1,:)*Grid%tmask(2:nx,1:ny-1,:)* &
         Grid%tmask(1:nx-1,2:ny,:)*Grid%tmask(2:nx,2:ny,:) 
    !
    Grid%fmask(nx,:,:) = Grid%tmask(nx,:,:)
    Grid%fmask(:,ny,:) = Grid%tmask(:,ny,:)
    !
    ALLOCATE(zwf(nx,ny))
    !     
    DO k = 1,N
       !
       zwf(:,:) = Grid%fmask(:,:,k)
       !         
       DO j = 2, ny-1
          DO i = 2,nx-1            
             IF( Grid%fmask(i,j,k) == 0. ) THEN               
                Grid%fmask(i,j,k) = shlat * MIN(1.,MAX( zwf(i+1,j),zwf(i,j+1),zwf(i-1,j),zwf(i,j-1)))
             END IF
          END DO
       END DO
       !
       DO j = 2, ny-1
          IF( Grid%fmask(1,j,k) == 0. ) THEN
             Grid%fmask(1,j,k) = shlat * MIN(1.,MAX(zwf(2,j),zwf(1,j+1),zwf(1,j-1)))
          ENDIF

          IF( Grid%fmask(nx,j,k) == 0. ) THEN
             Grid%fmask(nx,j,k) = shlat * MIN(1.,MAX(zwf(nx,j+1),zwf(nx-1,j),zwf(nx,j-1)))
          ENDIF
       END DO
       !         
       DO i = 2, nx-1         
          IF( Grid%fmask(i,1,k) == 0. ) THEN
             Grid%fmask(i, 1 ,k) = shlat*MIN( 1.,MAX(zwf(i+1,1),zwf(i,2),zwf(i-1,1)))
          ENDIF
          !            
          IF( Grid%fmask(i,ny,k) == 0. ) THEN
             Grid%fmask(i,ny,k) = shlat * MIN(1.,MAX(zwf(i+1,ny),zwf(i-1,ny),zwf(i,ny-1)))
          ENDIF
       END DO
       !!
    END DO
    !!      
  END SUBROUTINE Init_mask
  !
  !*****************************************************
  !   end subroutine Init_mask
  !*****************************************************
  ! 
  !*****************************************************
  !   subroutine Init_Tmask(name,Grid)
  !*****************************************************
  !
  SUBROUTINE Init_Tmask(name,Grid,jpiglo,jpjglo)
    !
    USE io_netcdf
    !      
    CHARACTER(*) name
    INTEGER :: nx,ny,k,i,j,jpiglo,jpjglo
    TYPE(Coordinates) :: Grid
    REAL*8, POINTER, DIMENSION(:,:) ::zwf => NULL()
    !
    IF(jpiglo == 1 .AND. jpjglo == 1) THEN
       CALL Read_Ncdf_var('Bathy_level',name,Grid%Bathy_level)
    ELSE
       CALL Read_Ncdf_var('Bathy_level',name,Grid%Bathy_level,(/jpizoom,jpjzoom/),(/jpiglo,jpjglo/) )
    ENDIF
    !
    nx = SIZE(Grid%Bathy_level,1)
    ny = SIZE(Grid%Bathy_level,2) 
    !
    WRITE(*,*) 'Init masks in T points'    
    !     
    ALLOCATE(Grid%tmask(nx,ny,N))
    !
    DO k = 1,N
       !      
       WHERE(Grid%Bathy_level(:,:) <= k-1 )    
          Grid%tmask(:,:,k) = 0.
       ELSEWHERE
          Grid%tmask(:,:,k) = 1.
       END WHERE
       !
    END DO
    !      
  END SUBROUTINE Init_Tmask
  !
  !*****************************************************
  !   subroutine get_mask(name,Grid)
  !*****************************************************
  !
  SUBROUTINE get_mask(level,posvar,mask,filename)
    !
    USE io_netcdf
    !      
    CHARACTER(*) filename
    CHARACTER(*) posvar
    INTEGER :: level, nx, ny
    LOGICAL,DIMENSION(:,:),POINTER :: mask
    INTEGER,DIMENSION(:,:),POINTER :: maskT,maskU,maskV
    !      
    TYPE(Coordinates) :: Grid
    !
    CALL Read_Ncdf_var('Bathy_level',filename,Grid%Bathy_level)
    ! 
    nx = SIZE(Grid%Bathy_level,1)
    ny = SIZE(Grid%Bathy_level,2)
    ALLOCATE(maskT(nx,ny),mask(nx,ny))
    mask = .TRUE.
    !
    WHERE(Grid%Bathy_level(:,:) <= level-1 )    
       maskT(:,:) = 0
    ELSEWHERE
       maskT(:,:) = 1
    END WHERE
    !
    SELECT CASE(posvar)
       !
    CASE('T')
       !
       WHERE(maskT > 0)
          mask = .TRUE.
       ELSEWHERE
          mask = .FALSE.
       END WHERE
       DEALLOCATE(maskT)
       !
    CASE('U')
       !
       ALLOCATE(maskU(nx,ny))
       maskU(1:nx-1,:) = maskT(1:nx-1,:)*maskT(2:nx,:)
       maskU(nx,:) = maskT(nx,:)
       WHERE(maskU > 0)
          mask = .TRUE.
       ELSEWHERE
          mask = .FALSE.
       END WHERE
       DEALLOCATE(maskU,maskT)
       !
    CASE('V')
       !
       ALLOCATE(maskV(nx,ny))
       maskV(:,1:ny-1) = maskT(:,1:ny-1)*maskT(:,2:ny)
       maskV(:,ny) = maskT(:,ny)
       WHERE(maskT > 0)
          mask = .TRUE.
       ELSEWHERE
          mask = .FALSE.
       END WHERE
       DEALLOCATE(maskV,maskT)
       !
    END SELECT
    !      
  END SUBROUTINE get_mask
  !
  !*****************************************************
  !   end subroutine get_mask
  !*****************************************************
  !       
  !
  !*****************************************************
  !   subroutine read_dimg_var(unit,irec,field)
  !*****************************************************
  !
  SUBROUTINE read_dimg_var(unit,irec,field,jpk)
    !      
    INTEGER :: unit,irec,jpk
    REAL*8,DIMENSION(:,:,:,:),POINTER :: field
    INTEGER :: k
    !      
    DO k = 1,jpk
       READ(unit,REC=irec) field(:,:,k,1)
       irec = irec + 1
    ENDDO
    !
  END SUBROUTINE read_dimg_var
  ! 
  !
  !*****************************************************
  !   subroutine read_dimg_var(unit,irec,field)
  !*****************************************************
  !
  SUBROUTINE write_dimg_var(unit,irec,field,jpk)
    !      
    INTEGER :: unit,irec,jpk
    REAL*8,DIMENSION(:,:,:,:),POINTER :: field
    INTEGER :: k
    !      
    DO k = 1,jpk
       WRITE(unit,REC=irec) field(:,:,k,1)
       irec = irec + 1
    ENDDO
    !
  END SUBROUTINE write_dimg_var
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE agrif_readwrite
