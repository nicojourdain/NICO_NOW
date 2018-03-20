MODULE domain
!!-----------------------------------------------------------
!!
!!         module to define domain to extract 
!!                 from initial grid
!!
!!	       Created by Brice Lemaire on 01/2010.
!!
!!-----------------------------------------------------------
  USE readwrite
  USE mixed_grid
  !
  IMPLICIT NONE
  PUBLIC
  !
  CONTAINS
  !********************************************************
  !              SUBROUTINE  define_domain              *
  !												        *
  !      to define the domain of the coarse grid        *
  !			        which will be used 					*
  !                                                     *
  !          CALLED from create_coordinates             *
  !******************************************************** 
  SUBROUTINE define_domain
  !
  WRITE(*,*) ''
  WRITE(*,*) ' ### SUBROUTINE define_domain ### '
  WRITE(*,*) ''
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! *** without northern boundary ***
  IF((nn_jmax.LT.(nsizey-1)).AND.(nn_jmax.GT.nn_jmin)) THEN             
    !
    WRITE(*,*) ' ****************************** ' 
    WRITE(*,*) ' *** WITHOUT NORTH BOUNDARY *** '
    WRITE(*,*) ' ****************************** ' 
    !     
	! *** with left/right boundary *** 
	IF(nn_imin.GT.nn_imax) THEN                                        
	  nxcoag = (nsizex - (nn_imin-1) + 1) + ((nn_imax+1) - 2) 
	! *** all around the earth ***  
	ELSEIF(nn_imin.EQ.nn_imax) THEN                       
	  nxcoag = nsizex
	ELSE
	  nxcoag = (nn_imax+1) - (nn_imin-1) + 1                 	
	ENDIF
	!  
	!(+/-1) we need ghost cells to make interpolation
	nycoag = (nn_jmax+1) - (nn_jmin-1) + 1                
	!
  ENDIF
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  
  ! *** along northern boundary ***
  IF((nn_jmax.LE.nn_jmin).OR.(nn_jmax.GE.nsizey-1)) THEN             
    !
    WRITE(*,*) ' **************************** ' 
    WRITE(*,*) ' *** ALONG NORTH BOUNDARY *** '
    WRITE(*,*) ' **************************** ' 
	!
	! *** with left/right boundary ***
	IF(nn_imin.GT.nn_imax) THEN                       
	  !
	  WRITE(*,*) '' 
	  WRITE(*,*) ' ** asian bipole ** '
	  WRITE(*,*) '' 
	  !	  
	  nval1 = nsizex - nn_imin + 1
	  nval2 = nn_imax  
	  nn_jmin = nn_jmax
	  !
	  ! *** to respect symmetry around asian bipole ***
	  IF((nval1.LT.nval2).AND.(nval2.LT.nmid)) THEN       
        !
		nn_imin = nsizex - nval2 + 1	
		nxcoag = nval2
		nycoag = (nsizey+1 - (nn_jmin-1))  + (nsizey+1 - (nn_jmax-1)) - 2		  
	    ! 
	    ELSEIF((nval1.GE.nval2).AND.(nval1.LT.nmid)) THEN
	    !
	    nn_imax = nval1
		nxcoag = nval1 
		nycoag = (nsizey+1 - (nn_jmin-1))  + (nsizey+1 - (nn_jmax-1)) - 2		  
		!
		! *** all around the earth ***
	    ELSE                                         
	    !
	    nn_imax = nn_imin	
		!		
      ENDIF
	ENDIF
	!
	IF(nn_imin.LT.nn_imax) THEN                           
	  !
	  ! *** without bipole ***
	  IF(((nn_imin.LT.nmid).AND.(nn_imax.LT.nmid)).OR.((nn_imin.GT.nmid).AND.(nn_imax.GT.nmid))) THEN  
	    !
	    WRITE(*,*) '' 
	    WRITE(*,*) ' ** without bipole ** '
	    WRITE(*,*) '' 
	    !	 	    
	    nxcoag = (nn_imax+1) - (nn_imin-1) + 1 
	    nycoag = (nsizey+1 - (nn_jmin-1))  + (nsizey+1 - (nn_jmax-1)) - 2 -0   
	    !
		! *** including canada bipole ***
	    ELSEIF((nn_imin.LE.nmid).AND.(nn_imax.GE.nmid)) THEN    
		!
		WRITE(*,*) '' 
		WRITE(*,*) ' ** canadian bipole ** '
		WRITE(*,*) '' 
		!	 		
		nn_jmin = nn_jmax
		nval1 = nmid - nn_imin 
		nval2 = nn_imax - (nmid-1) 
		!
		! *** to respect around canada bipole ***
		IF(nval1.LT.nval2) THEN    
		  !
		  nn_imin = nmid - nval2	
		  nxcoag = (nval2+1)  
	      nycoag = (nsizey+1 - (nn_jmin-1))  + (nsizey+1 - (nn_jmax-1)) - 2	- (2*npivot)	  
		  ! 
		  ELSEIF(nval1.GE.nval2) THEN
		  !
		  nn_imax = nmid + nval1
		  nxcoag = (nval1+1)  
	      nycoag = (nsizey+1 - (nn_jmin-1))  + (nsizey+1 - (nn_jmax-1)) - 2	- (2*npivot)	  
		  !
		ENDIF    
	  ENDIF	
    ENDIF
  ENDIF
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
	IF(nglobal) THEN                                  !Global
	  !                                                Don't change the global shape of the matrix
	  WRITE(*,*) '' 
	  WRITE(*,*) ' ** global ** '
	  WRITE(*,*) '' 
	  !	  
	  nn_imin = 1
	  nn_imax = nsizex
	  nn_jmin = 1
	  nn_jmax = nsizey
	  nxcoag = nsizex
	  nycoag = nsizey
	  !
	ELSEIF(nn_imin.EQ.nn_imax) THEN      !Semi-global (e.g northern hemisphere)  
	  !                                   Change the global shape -> suppression of the northern boundary and bipoles 
	  WRITE(*,*) '' 
	  WRITE(*,*) ' ** all around the earth (2 bipoles) ** '
	  WRITE(*,*) '' 
	  !	 	
	  nn_imin = 2 
	  nn_imax = nmid
	  nn_jmin = nn_jmax
	  nxcoag = (nn_imax+1) - (nn_imin-1) + 1 
	  nycoag = (nsizey+1 - (nn_jmin-1))  + (nsizey+1 - (nn_jmax-1)) - 2	
	  !
	ENDIF
  !  
  WRITE(*,*) ''
  WRITE(*,*) ' ### END SUBROUTINE define_domain ### '
  WRITE(*,*) ''
  !
  END SUBROUTINE
  !
END MODULE