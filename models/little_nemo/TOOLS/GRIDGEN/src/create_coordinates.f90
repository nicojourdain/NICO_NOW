PROGRAM create_coordinates
!!-----------------------------------------------------------
!!
!!	  to create regional coordinates file (e.g. 1_coordinates.nc)
!!    for fine grid from coarse grid (e.g. coordinates.nc)
!!
!!   to make it we use a 4th order polynomial interpolation
!!
!!	        Created by Brice Lemaire on 12/2009.
!!
!!-----------------------------------------------------------
  USE netcdf
  USE cfg_tools
  USE mixed_grid
  USE domain
  !------------
  IMPLICIT NONE
  !
  INTEGER :: narg,iargc
  INTEGER :: nstat, imodulo
  INTEGER :: nik, njk, npk
  INTEGER :: ival
  CHARACTER(len=80) :: cnmlname, cfingrdname
  !
  !
  !
  !*************************************  
  ! Read input file (namelist file)
  !         using types.f90
  !*************************************
  narg = iargc()
  !
  IF (narg == 0) THEN
    cnmlname = 'namelist.input'
  ELSE
	CALL getarg(1,cnmlname)
  ENDIF
  !
  CALL read_namelist(cnmlname)  
  !
  nstat = read_coordinates(TRIM(cn_parent_coordinate_file),scoagrd)    
  IF (nstat/=1) THEN    
   WRITE(*,*) 'unable to open netcdf file : ',cn_parent_coordinate_file
   STOP
  ENDIF
  !
  nsizex = SIZE(scoagrd%glamt,1)
  nsizey = SIZE(scoagrd%glamt,2)
  !
  WRITE(*,*) ''
  WRITE(*,*) 'Size of input matrix: '
  WRITE(*,*) '(',nsizex,';', nsizey,')' 
  WRITE(*,*) ''
  !
  WRITE(*,*) 'Domain: '
  WRITE(*,*) ''
  WRITE(*,*) '           ', '    min(1,1:nsizey)       ', '      max(nsizex,1:nsizey)            '
  WRITE(*,*) 'longitude: ', MINVAL(scoagrd%glamt(1,1:nsizey)), ' --> ', MAXVAL(scoagrd%glamf(nsizex,1:nsizey)) 
  WRITE(*,*) 'latitude:  ', MINVAL(scoagrd%gphit(1:nsizex,1)), ' --> ', MAXVAL(scoagrd%gphif(1:nsizex,nsizey)) 
  WRITE(*,*) ''
  !
  !*************************************  
  ! Check the values read in the namelist
  !*************************************
  IF(nn_imin.LE.0.OR.nn_imin.GT.nsizex)THEN
    WRITE(*,*) 'Wrong values in namelist:'
	WRITE(*,*) 'nn_imin must be greater than 0 and less than', nsizex
	STOP
  ENDIF
  !
  IF(nn_imax.LE.0.OR.nn_imax.GT.nsizex)THEN
    WRITE(*,*) 'Wrong values in namelist:'
	WRITE(*,*) 'nn_imax must be greater than 0 and less than', nsizex
	STOP
  ENDIF
  !
  IF(nn_jmin.LE.0.OR.nn_jmin.GT.nsizey)THEN
    WRITE(*,*) 'Wrong values in namelist:'
	WRITE(*,*) 'nn_jmin must be greater than 0 and less than', nsizey
	STOP
  ENDIF
  !
  IF(nn_jmax.LE.0.OR.nn_jmax.GT.nsizey)THEN
    WRITE(*,*) 'Wrong values in namelist:'
	WRITE(*,*) 'nn_jmax must be greater than 0 and less than', nsizey
	STOP
  ENDIF
  !
  !*************************************  
  ! Define the sub-domain chosen by user
  !*************************************
  WRITE(*,*) 'Domain defined by user: '
  WRITE(*,*) ''
  WRITE(*,*) '           ', '          min           ', '               max            '
  WRITE(*,*) 'longitude: ', MINVAL(scoagrd%glamt(nn_imin,nn_jmin:nn_jmax)), ' --> ', MAXVAL(scoagrd%glamf(nn_imax,nn_jmin:nn_jmax)) 
  WRITE(*,*) 'latitude:  ', MINVAL(scoagrd%gphit(nn_imin:nn_imax,nn_jmin)), ' --> ', MAXVAL(scoagrd%gphif(nn_imin:nn_imax,nn_jmax)) 
  WRITE(*,*) ''
  !
  IF(nn_imin.EQ.nn_imax.AND.nn_jmin.EQ.nn_jmax) THEN
    nglobal = .TRUE.
	WRITE(*,*) 'Size of domain: GLOBAL'
  ELSE
    nglobal = .FALSE.
	WRITE(*,*) 'Size of domain: REGIONAL'
  ENDIF
  !
  IF(cn_position_pivot.EQ.'F-grid') THEN       !case of ORCA05 & ORCA025 grids
   npivot = 0
  ELSEIF(cn_position_pivot.EQ.'T-grid') THEN   !case of ORCA2 grid
   npivot = 1
  ENDIF
  !
  nmid = nsizex/2 + npivot
  !
  !
  !
  !*************************************
  ! Redefine for particular cases
  !*************************************
  IF(((nn_imax - nn_imin).EQ.-1).OR.((nn_imax - nn_imin).EQ.-2)) THEN
    nn_imax = nn_imin
  ELSEIF(ABS(nn_imin - nn_imax).GE.nsizex-1) THEN
    nn_imax = nn_imin
  ELSEIF((nn_imin.EQ.1).AND.(nn_imax.EQ.nsizex)) THEN
    nn_imax = nn_imin
  ELSEIF(nn_imin.EQ.1.AND.nn_imax.GT.nn_imin) THEN
    nn_imin = nsizey-2
  ELSEIF(nn_imax.EQ.nsizex) THEN
    nn_imax = 3
  ELSEIF(nn_imin.EQ.nsizex) THEN
    nn_imin = nsizex-1
  ELSEIF(nn_imax.EQ.1.AND.nn_imax.NE.nn_imin) THEN
    nn_imax = 2
  ENDIF
  !
  !
  !
  !*************************************
  ! We want to fix equator along T and U-points
  !*************************************
  imodulo =  MOD(nn_rhoy,2)    
  !
  IF(.NOT.nglobal.AND.(scoagrd%gphit(nn_imin,nn_jmin)*scoagrd%gphit(nn_imin,nn_jmax)).LT.0.AND.imodulo.EQ.0) THEN 
    nequator = 1
  ELSEIF(nglobal.AND.imodulo.EQ.0) THEN
    nequator = 1
  ELSE
    nequator = 0
  ENDIF	  
  !
  !
  !
  !*************************************
  ! CREATE MIXED GRID
  !*************************************
  CALL define_domain
  !
  CALL define_mixed_grid 
  !
  !
  !
  !*************************************
  !!! CALCULATE FINE GRID DIMENSIONS
  !************************************* 
  IF(.NOT.nglobal) THEN
	IF(nn_rhox.EQ.1) THEN 
	  nxfine = nxcoag
	ELSE
	  nxfine = (nxcoag-2)*nn_rhox + 1
	ENDIF  
	!
	 IF(nn_rhoy.EQ.1) THEN 
	  nyfine = nycoag
	ELSE
	  nyfine = (nycoag-2)*nn_rhoy + 1
	ENDIF  
	!
  ELSEIF(nglobal) THEN
    IF(nn_rhox.GT.1) THEN
	  nxfine = (nxgmix - 4*(nn_rhox-1))/2
	ELSEIF(nn_rhox.EQ.1) THEN
	  nxfine = nsizex
	ENDIF
	!
	IF(nn_rhoy.GT.1) THEN
	  nyfine = (nygmix - (2*nn_rhoy))/2 - nn_rhoy + nequator
	ELSEIF(nn_rhoy.EQ.1) THEN
	  nyfine = nsizey
	ENDIF
	!
  ENDIF
  !
  WRITE(*,*) ''
  WRITE(*,*) '*** SIZE OF FINE GRID ***'
  WRITE(*,*) nxfine, ' x ', nyfine
  WRITE(*,*) ''
  !
  !
  !
  !*************************************
  ! Interpolation inside the mixed grid
  !        from cfg_tools.f90
  !*************************************
  IF(nn_rhox.GT.1.OR.nn_rhoy.GT.1) THEN
	CALL interp_grid    
  ENDIF
  !  
  !
  ! 
  !*************************************  
  ! Define name of child coordinate file  
  !  coordinates.nc -> 1_coordinates.nc
  !*************************************
  CALL set_child_name(cn_parent_coordinate_file,cfingrdname)      
  !
  !
  !
  !*************************************
  ! Allocation of child grid elements
  !          from types.f90
  !*************************************
  CALL grid_allocate(sfingrd,nxfine,nyfine)                         
  !
  !
  !
  !*************************************
  ! Break the mixed grid smixgrd into 4 grids 
  !        from cfg_tools.f90
  !*************************************
  CALL child_grid                                
  !     
  !
  !
  !*************************************
  ! Read parent coordinate file
  !      from readwrite.f90
  !*************************************
  nstat = write_coordinates(cfingrdname,sfingrd,nxfine,nyfine)      
  IF (nstat/=1) THEN    
   WRITE(*,*)"unable to write netcdf file : ",cfingrdname
   STOP
  ENDIF
  !	
  CALL grid_deallocate(scoagrd)
  CALL grid_deallocate(sfingrd)
  CALL mixed_grid_deallocate(smixgrd)
  !
  !
  !
END PROGRAM create_coordinates