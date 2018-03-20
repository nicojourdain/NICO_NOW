PROGRAM create_data
  !
  USE io_netcdf
  USE bilinear_interp
  USE agrif_readwrite
  USE agrif_interpolation      
  !
  IMPLICIT NONE
  !
  !************************************************************************
  ! 									*
  ! PROGRAM  CREATE_DATA							*
  !									*
  ! program to implement data interpolation to generate	 		*
  ! child grid forcing files						*
  !									*									*
  !Interpolation is carried out using bilinear interpolation		*
  !routine from SCRIP package						*		
  !									*
  !http://climate.lanl.gov/Software/SCRIP/				*									*
  !************************************************************************
  !
  INTEGER :: narg,iargc,i
  CHARACTER(len=80) :: namelistname

  narg = iargc()

  IF (narg == 0) THEN
     namelistname = 'namelist.input'
  ELSE
     CALL getarg(1,namelistname)
  ENDIF

  ! read input file (namelist.input)
  !
  CALL read_namelist(namelistname)

  i = 1
  !
  ! Interpolate U grid  data
  !
  DO WHILE( TRIM(U_Files(i)) .NE. '/NULL' )
     PRINT *,'Grid U forcing files = ',u_files(i)
     !        
     CALL Interp_Extrap_var(U_FILES(i), 'U') 
     i = i+1                
     !             
  END DO

  i = 1
  !
  ! Interpolate V grid  data
  !
  DO WHILE( TRIM(V_Files(i)) .NE. '/NULL' )
     PRINT *,'Grid V forcing files = ',v_files(i)
     !        
     CALL Interp_Extrap_var(V_FILES(i), 'V') 
     i = i+1                
     !             
  END DO

  i = 1
  !
  ! Interpolate flux data
  !
  DO WHILE( TRIM(Flx_Files(i)) .NE. '/NULL' )
     PRINT *,'flxfiles = ',flx_files(i)
     !        
     CALL Interp_Extrap_var(FLX_FILES(i), 'T') 
     i = i+1                
     !             
  END DO
  !
  WRITE(*,*) ' '
  WRITE(*,*) '******* forcing files successfully created *******' 
  WRITE(*,*) ' '  
  !
  STOP
END PROGRAM create_data
