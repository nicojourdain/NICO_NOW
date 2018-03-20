MODULE types
  !
  PUBLIC
  !   
  !*****************************
  ! Coordinates type definition 
  !*****************************
  TYPE coordinates
     REAL*8, DIMENSION(:,:), POINTER  :: nav_lon,nav_lat              => NULL()
     REAL*8, DIMENSION(:,:), POINTER  :: glamv, glamu, glamt, glamf   => NULL()
     REAL*8, DIMENSION(:,:), POINTER  :: gphit, gphiu, gphiv, gphif   => NULL()
     REAL*8, DIMENSION(:,:), POINTER  :: e1t, e1u, e1v, e1f           => NULL()
     REAL*8, DIMENSION(:,:), POINTER  :: e2t, e2u, e2v, e2f           => NULL()
     INTEGER, DIMENSION(:) , POINTER  :: time_steps                   => NULL()
  END TYPE coordinates
  !
  !
  !
  TYPE mixed_coordinates
     REAL*8, DIMENSION(:,:), POINTER  :: nav_lon,nav_lat              => NULL()
     REAL*8, DIMENSION(:,:), POINTER  :: glam                         => NULL()
     REAL*8, DIMENSION(:,:), POINTER  :: gphi                         => NULL()
     REAL*8, DIMENSION(:,:), POINTER  :: e1                           => NULL()
     REAL*8, DIMENSION(:,:), POINTER  :: e2                           => NULL()
     INTEGER, DIMENSION(:) , POINTER  :: time_steps                   => NULL()
  END TYPE mixed_coordinates
  !
  !**************************************************************
  ! Declaration of global variables
  !**************************************************************
  !size of input ORCA grid
  INTEGER :: nsizex, nsizey    
  INTEGER :: nmid            
  !
  !kind of input grid
  INTEGER :: npivot 
  !
  INTEGER :: nequator
  !
  LOGICAL :: nglobal
  !
  INTEGER :: nresx, nresy
  !
  !distance between middle of input grid and the border of sub-domain 
  INTEGER :: nval1, nval2
  !
  INTEGER :: nxcoag, nycoag     !size of the sub-domain inside input ORCA grid 
  INTEGER :: nxgmix, nygmix     !size of the mixed grid 
  INTEGER :: nxfine, nyfine     !size of fine grid 
  !
  TYPE(coordinates), SAVE :: scoagrd         !coarse grid
  TYPE(coordinates), SAVE :: sfingrd         !fine grid
  TYPE(mixed_coordinates), SAVE :: smixgrd   !mixed grid to store all components (T,U,V,F) 
  !                                           of the coarse grid in the same one
  !
  !
  !
  !**************************************************************
  ! Declaration of various input file variables (namelist.input) 
  !**************************************************************
  INTEGER nn_imin,nn_jmin,nn_imax,nn_jmax,nn_rhox,nn_rhoy
  LOGICAL ln_iom_activated
  CHARACTER*100 cn_parent_coordinate_file, cn_position_pivot
  !      
  NAMELIST /input_output/ln_iom_activated
  NAMELIST /coarse_grid_files/cn_parent_coordinate_file,cn_position_pivot
  NAMELIST /nesting/nn_imin,nn_jmin,nn_imax,nn_jmax,nn_rhox,nn_rhoy      
  !
  !
  !
CONTAINS
  !********************************************************
  !             subroutine grid_allocate				*
  !					                            		*
  !         allocation of grid type elements 			*
  !             according to nx and ny	     			*
  !							                            *
  !********************************************************
  SUBROUTINE grid_allocate(Grid,nx,ny)
    !
    TYPE(coordinates) :: Grid
    INTEGER :: nx,ny
    !
    ALLOCATE(Grid%nav_lon(nx,ny),Grid%nav_lat(nx,ny))
    ALLOCATE(Grid%glamt(nx,ny),Grid%glamu(nx,ny),Grid%glamv(nx,ny),Grid%glamf(nx,ny))
    ALLOCATE(Grid%gphit(nx,ny),Grid%gphiu(nx,ny),Grid%gphiv(nx,ny),Grid%gphif(nx,ny))
    ALLOCATE(Grid%e1t(nx,ny),Grid%e1u(nx,ny),Grid%e1v(nx,ny),Grid%e1f(nx,ny))
    ALLOCATE(Grid%e2t(nx,ny),Grid%e2u(nx,ny),Grid%e2v(nx,ny),Grid%e2f(nx,ny))
	!
  END SUBROUTINE grid_allocate
  !
  !
  !
  SUBROUTINE grid_deallocate(Grid)
    !
    TYPE(coordinates) :: Grid
    !
    DEALLOCATE(Grid%nav_lon,Grid%nav_lat)
    DEALLOCATE(Grid%glamt,Grid%glamu,Grid%glamv,Grid%glamf)
    DEALLOCATE(Grid%gphit,Grid%gphiu,Grid%gphiv,Grid%gphif)
    DEALLOCATE(Grid%e1t,Grid%e1u,Grid%e1v,Grid%e1f)
    DEALLOCATE(Grid%e2t,Grid%e2u,Grid%e2v,Grid%e2f)
	!
  END SUBROUTINE grid_deallocate
  !
  !
  !
  !********************************************************
  !           subroutine mixed_grid_allocate			*
  !							                            *
  !          allocation of grid type elements 			*
  !               according to nx and ny				*
  !						                            	*
  !********************************************************
  SUBROUTINE mixed_grid_allocate(Grid,nx,ny)
    !
    TYPE(mixed_coordinates) :: Grid
    INTEGER :: nx,ny
    !
    ALLOCATE(Grid%nav_lon(nx,ny),Grid%nav_lat(nx,ny))
    ALLOCATE(Grid%glam(nx,ny))
    ALLOCATE(Grid%gphi(nx,ny))
    ALLOCATE(Grid%e1(nx,ny))
    ALLOCATE(Grid%e2(nx,ny))
	!
  END SUBROUTINE mixed_grid_allocate
  !
  !
  !
  SUBROUTINE mixed_grid_deallocate(Grid)
    !
    TYPE(mixed_coordinates) :: Grid
    !
    DEALLOCATE(Grid%nav_lon,Grid%nav_lat)
    DEALLOCATE(Grid%glam)
    DEALLOCATE(Grid%gphi)
    DEALLOCATE(Grid%e1)
    DEALLOCATE(Grid%e2)
	!
  END SUBROUTINE mixed_grid_deallocate
  !
  !
  !
  !********************************************************
  !             subroutine read_namelist				*
  !				                     					*
  !   read variables contained in namelist.input file   *
  !                filled in by user 					*
  !								                    	*
  !********************************************************
  SUBROUTINE read_namelist(namelistname)
    !
    IMPLICIT NONE
    CHARACTER(len=80) :: namelistname
    CHARACTER*255 :: output
    LOGICAL :: is_it_there
    INTEGER unit_nml
    !
    unit_nml = Get_Unit()
    !      
    INQUIRE ( FILE = namelistname , EXIST = is_it_there )      
    !
    IF ( is_it_there ) THEN 
       !
       OPEN ( FILE   =  namelistname, &
              UNIT   =  unit_nml,	  &
              STATUS = 'OLD',		  &
              FORM   = 'FORMATTED',	  &
              ACTION = 'READ',		  &
              ACCESS = 'SEQUENTIAL'     )	  
       !
       REWIND(unit_nml)
       READ (unit_nml , NML = coarse_grid_files)
       READ (unit_nml , NML = nesting) 
       CLOSE(unit_nml)
	   !
    ELSE
       !
       PRINT *,'namelist file ''',TRIM(namelistname),''' not found'
       STOP	 
       !
    END IF
    !
  END SUBROUTINE read_namelist
  !
  !
  !
  !*************************************************
  !              function Get_Unit                             
  !*************************************************
  INTEGER FUNCTION Get_Unit()
    !
    INTEGER n
    LOGICAL op
    INTEGER :: nunit
    INTEGER :: iii,out,iiimax 
    ! 
    DO n = 7,1000
       ! 
       INQUIRE(Unit=n,Opened=op)
       !
       IF (.NOT.op) EXIT
       !      
    ENDDO
    !
    Get_Unit=n
    !
  END FUNCTION Get_Unit
  !
END MODULE types
