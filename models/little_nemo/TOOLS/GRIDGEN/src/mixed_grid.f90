MODULE mixed_grid
!!-----------------------------------------------------------
!!
!!       tools box to create a mixed grid storing 
!!           the known values of grids U,V,T,F
!!
!!	        Created by Brice Lemaire on 01/2010.
!!
!!-----------------------------------------------------------
  USE readwrite
  !
  IMPLICIT NONE
  PUBLIC
  !
  CONTAINS
  !********************************************************
  !            SUBROUTINE define_mixed_grid             *
  !												        *
  !         to define the size of the mixed grid        *
  !														*
  !            CALL from create_coordinates             *
  !********************************************************
  SUBROUTINE define_mixed_grid
    !
    INTEGER :: ixgmix, iygmix
    INTEGER :: ii, ij
    ! 
	WRITE(*,*) ''
	WRITE(*,*) ' ### SUBROUTINE define_mixed_grid ### '
	WRITE(*,*) ''
	!
	WRITE(*,*) ' *** CHECKING SIZE OF COARSE DOMAIN *** '
	WRITE(*,*) nxcoag, 'x', nycoag
	WRITE(*,*) ''
	!
    !*************************************
	!!!Calculate size of mixed grid (ixgmix x iygmix)
    !*************************************
	IF(.NOT.nglobal) THEN
	  ixgmix = (nxcoag) * 2                              !known points (T,U,V,F) along x
	  ixgmix = ixgmix + (nn_rhox-1)*(ixgmix)!-1)           !points to interpolate    ''
	  ! 
	  iygmix = (nycoag) * 2                              !known points (T,U,V,F) along y
	  iygmix = iygmix + (nn_rhoy-1)*(iygmix)!-1)           !points to interpolate    ''
	ELSEIF(nglobal) THEN
	  ixgmix = (nxcoag) * 2                             
	  ixgmix = ixgmix + (nn_rhox-1)*(ixgmix)              
	  ! 
	  iygmix = (nycoag) * 2                            
	  iygmix = iygmix + (nn_rhoy-1)*(iygmix)     
	ENDIF
	!
	nxgmix = ixgmix
	nygmix = iygmix
	!
	WRITE(*,*) ''
	WRITE(*,*) '*** SIZE OF MIXED GRID ***'
	WRITE(*,*) nxgmix, ' x ', nygmix
	WRITE(*,*) ''
    !
    CALL mixed_grid_allocate(smixgrd,ixgmix,iygmix)     !using type.f90
    !
	IF(nglobal)THEN
	  ii = 1
      ij = 1
    ELSE
	  ii = nn_imin-1
	  ij = nn_jmin-1
	ENDIF
	!
    CALL write_mixed_grid(ixgmix,iygmix,ii,ij)   
    !
	WRITE(*,*) ''
	WRITE(*,*) ' ### END SUBROUTINE define_mixed_grid ### '
	WRITE(*,*) ''  
	!
  END SUBROUTINE
  !
  !
  !
  !********************************************************
  !           SUBROUTINE write_mixed_grid   			  *
  !									        			  *
  !  to write the known values into the mixed grid		  *
  !	These known values are spaced every (nn_rho-1) points *
  !	    for allowing to compute the interpolation         *
  !               inside this same grid	     			  *
  !														  *
  !******************************************************** 
  SUBROUTINE write_mixed_grid(ki_end,kj_end,ki_min,kj_min)
    !
	INTEGER, INTENT(IN) :: ki_end, kj_end
	INTEGER, INTENT(INOUT) :: ki_min, kj_min
	INTEGER :: ji_start, jj_start
	INTEGER :: ji,jj
	INTEGER :: isym_x, isym_y
	INTEGER :: itmp1, itmp2, itmp3, itmp4, itmp5, itmp6, itmp7
    INTEGER :: icorrxt, icorrxu, icorrxv, icorrxf      !correction factor for i-indexation
    INTEGER :: icorryt, icorryu, icorryv, icorryf      !correction factor for j-indexation
	LOGICAL :: llp = .TRUE.
	LOGICAL :: llq = .TRUE.
	!
	WRITE(*,*) ''
	WRITE(*,*) ' ### SUBROUTINE write_mixed_grid ### '
	WRITE(*,*) '' 
	!
	ji_start = 1
	jj_start = 1   
    !
	isym_y = 1
	!
	! correction factor for symmetry along north boundary
	icorrxt = 0
	icorrxu = 0
	icorrxv = 0
	icorrxf = 0
    !
	icorryt = 0
	icorryu = 0
	icorryv = 0
	icorryf = 0
	!
	DO jj=nn_rhoy,kj_end,2*nn_rhoy
      !
	  DO ji=nn_rhox,ki_end,2*nn_rhox
        !
		smixgrd%nav_lon(ji,jj)              =  scoagrd%nav_lon(ki_min + icorrxt, kj_min + icorryt)
		smixgrd%nav_lat(ji,jj)              =  scoagrd%nav_lat(ki_min + icorrxt, kj_min + icorryt)	    
		!
		smixgrd%glam(ji,jj)                 =  scoagrd%glamt(ki_min + icorrxt, kj_min + icorryt)
		smixgrd%glam(ji+nn_rhox,jj)         =  scoagrd%glamu(ki_min + icorrxu, kj_min + icorryu)
		smixgrd%glam(ji,jj+nn_rhoy)         =  scoagrd%glamv(ki_min + icorrxv, kj_min + icorryv)
		smixgrd%glam(ji+nn_rhox,jj+nn_rhoy) =  scoagrd%glamf(ki_min + icorrxf, kj_min + icorryf)		    
		!
		smixgrd%gphi(ji,jj)                 =  scoagrd%gphit(ki_min + icorrxt, kj_min + icorryt)
		smixgrd%gphi(ji+nn_rhox,jj)         =  scoagrd%gphiu(ki_min + icorrxu, kj_min + icorryu)		 
		smixgrd%gphi(ji,jj+nn_rhoy)         =  scoagrd%gphiv(ki_min + icorrxv, kj_min + icorryv)
		smixgrd%gphi(ji+nn_rhox,jj+nn_rhoy) =  scoagrd%gphif(ki_min + icorrxf, kj_min + icorryf)
		!
		smixgrd%e1(ji,jj)                   =  scoagrd%e1t(ki_min + icorrxt, kj_min + icorryt)
		smixgrd%e1(ji+nn_rhox,jj)           =  scoagrd%e1u(ki_min + icorrxu, kj_min + icorryu)		 
		smixgrd%e1(ji,jj+nn_rhoy)           =  scoagrd%e1v(ki_min + icorrxv, kj_min + icorryv)
		smixgrd%e1(ji+nn_rhox,jj+nn_rhoy)   =  scoagrd%e1f(ki_min + icorrxf, kj_min + icorryf)	
		!	
		smixgrd%e2(ji,jj)                   =  scoagrd%e2t(ki_min + icorrxt, kj_min + icorryt)
		smixgrd%e2(ji+nn_rhox,jj)           =  scoagrd%e2u(ki_min + icorrxu, kj_min + icorryu)		 
		smixgrd%e2(ji,jj+nn_rhoy)           =  scoagrd%e2v(ki_min + icorrxv, kj_min + icorryv)
		smixgrd%e2(ji+nn_rhox,jj+nn_rhoy)   =  scoagrd%e2f(ki_min + icorrxf, kj_min + icorryf)
        !
		IF(.NOT.nglobal)THEN
		  IF(ki_min.EQ.nsizex.AND.nn_imin.NE.2) THEN          ! across right/left boundary BUT not all around the earth
			ki_min = 3
		  ELSEIF(isym_y.EQ.1) THEN                            ! normal case
			ki_min = ki_min + 1                                
		  ELSEIF(isym_y.EQ.-1) THEN                           ! symetry along north boundary
			ki_min = ki_min - 1              
		  ENDIF
		ELSE
		  ki_min = ki_min + 1
		ENDIF
        !
	  ENDDO
      !	 
	  !
	  ! when we reach north boundary
	  IF(.NOT.nglobal)THEN
		IF(kj_min.EQ.nsizey-npivot-1.AND.llp) THEN           ! npivot => pivot located on T-point or F-point
		  llp = .FALSE.
		  kj_min = nsizey 
		  isym_y = -1
		  IF(nn_imin.LT.nmid.AND.nn_imax.LT.nmid) THEN       ! no bipole (from Asia to Canada)                               
			itmp1 = nsizex - nn_imin + 2 + npivot              
			isym_x = 1
		  ELSEIF(nn_imin.GT.nmid.AND.nn_imax.GT.nmid) THEN   ! no bipole (from Canada to Asia)
			itmp2 = nsizex - nn_imin + 2 + npivot               		  
			isym_x = 2
		  ELSEIF(nn_imin.LT.nmid.AND.nn_imax.GT.nmid) THEN   ! canadian bipole
			IF(nval1.LT.nval2) THEN
			  itmp3 = nmid + nval2 
			  isym_x = 3
			ELSEIF(nval1.GE.nval2) THEN                      ! canadian bipole
			  itmp4 = nmid + nval1 + 2 - npivot
			  isym_x = 4
			ENDIF
		  ELSEIF(ki_min.EQ.nsizex.AND.nval1.GT.nval2) THEN   ! asian bipole
			  itmp5 = nval1 + 1 + npivot
			  isym_x = 5
		  ELSEIF(ki_min.EQ.nsizex.AND.nval1.LT.nval2) THEN   ! asian bipole
			  itmp6 = nval2 + 1
			  isym_x = 6		
		  ELSEIF(ki_min.GE.nmid) THEN                        ! all around the earth (2 bipoles)
			  itmp7 = nsizex 
			  isym_x = 7
		  ENDIF
		ENDIF
	    !
	    !
	    !
	    IF(isym_y.EQ.1) THEN
          kj_min = kj_min + 1                   ! cas normal
          ki_min = nn_imin - 1   
	    ELSEIF(isym_y.EQ.-1) THEN                             
		  kj_min = kj_min - 1  
		  !
		  icorrxt = 0   
		  icorrxu = -1  
		  icorrxv = 0   
		  icorrxf = -1  
		  !
		  icorryt = 0   
		  icorryu = 0    
		  icorryv = -1  
		  icorryf = -1  
		  !      		
		  IF(isym_x.EQ.1) THEN                  ! no bipole
			ki_min = itmp1
			IF(llq)THEN
			  icorrxt = 0   
			  icorrxu = -1 + npivot
			  icorrxv = 0   
			  !
			  icorryt = 0   
			  icorryu = 0    
			  icorryv = -1 + npivot 
			  !  
			  llq = .FALSE.
			ENDIF		
          ELSEIF(isym_x.EQ.2) THEN              ! no bipole
		    ki_min = itmp2 
	      ELSEIF(isym_x.EQ.3) THEN              ! canadian bipole
		    ki_min = itmp3	
	      ELSEIF(isym_x.EQ.4) THEN              ! canadian bipole
		    ki_min = itmp4 
		    IF(llq)THEN
			  icorrxt = 0   
			  icorrxu = -1 + npivot
			  icorrxv = 0   
			  !
			  icorryt = 0   
			  icorryu = 0    
			  icorryv = -1 + npivot 
			  !  
			  llq = .FALSE.
		    ENDIF	
		  ELSEIF(isym_x.EQ.5) THEN              ! asian bipole
		    ki_min = itmp5		    
	      ELSEIF(isym_x.EQ.6) THEN              ! asian bipole
		    ki_min = itmp6      
		  ELSEIF(isym_x.EQ.7) THEN              ! all around the earth (2 bipoles)
		    ki_min = itmp7	
		  ENDIF
		  !
	    ENDIF
		!
	  ELSEIF(nglobal) THEN
        kj_min = kj_min + 1                    
	    ki_min = 1 	  
	  ENDIF
	ENDDO
	!
	WRITE(*,*) ''
	WRITE(*,*) ' ### END SUBROUTINE write_mixed_grid ### '
	WRITE(*,*) ''   
	! 
  END SUBROUTINE
  !
END MODULE