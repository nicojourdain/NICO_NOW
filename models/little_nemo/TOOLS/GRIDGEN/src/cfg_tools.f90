MODULE cfg_tools
!!-----------------------------------------------------------
!!
!!  to make that we use a 4th order polynomial interpolation
!!
!!	         Created by Brice Lemaire on 12/2009.
!!
!!-----------------------------------------------------------
  USE readwrite
  USE projection
  !
  IMPLICIT NONE
  PUBLIC
  !
  !
  !
  CONTAINS
  !********************************************************
  !               SUBROUTINE interp_grid   				*
  !									        			*
  !	 calculate polynomial interpolation at 4th order	*
  !												        *
  !		   CALLED from create_coordinates.f90	        *
  !********************************************************   
  SUBROUTINE interp_grid
    !
	REAL*8, DIMENSION(:,:),ALLOCATABLE :: dlcoef      !Array to store the coefficients of Lagrange 	 
	INTEGER :: ji, jj, jk, jproj
	INTEGER :: istat, ip
	LOGICAL :: llnorth_pole = .FALSE.
	!
	WRITE(*,*) ''
	WRITE(*,*) '### SUBROUTINE interp_grid ###'
	WRITE(*,*) ''
	!
	jproj = 0
	!
	! Calculate coefficients for interpolation along longitude
	!
	ALLOCATE(dlcoef(nn_rhox-1,4))
	istat = pol_coef(dlcoef,nn_rhox)
	IF (istat/=1) THEN    
      WRITE(*,*) "ERROR WITH LAGRANGIAN COEFFICIENTS"
      STOP
    ENDIF
	!
	WRITE(*,*) 'Interpolation along longitude'
    !
	DO jj = nn_rhoy,nygmix,nn_rhoy     
	  DO ji = nn_rhox,nxgmix,nn_rhox
        !
		DO jk = 1,nn_rhox-1
		  !
		  ! First, we check the +-180 discontinuity.
		  ! In this case, we increase the negative values of 360.
		  IF(ABS(smixgrd%glam(ji,jj)-smixgrd%glam(ji+1*nn_rhox,jj)).GT.180.0.AND.smixgrd%glam(ji,jj).LT.0.) THEN
	        smixgrd%glam(ji,jj) = smixgrd%glam(ji,jj) + 360.
		  ENDIF
		  !
		  IF(ABS(smixgrd%glam(ji,jj)-smixgrd%glam(ji+1*nn_rhox,jj)).GT.180.0.AND.smixgrd%glam(ji+1*nn_rhox,jj).LT.0.) THEN
	        smixgrd%glam(ji+1*nn_rhox,jj) = smixgrd%glam(ji+1*nn_rhox,jj) + 360.
		  ENDIF
		  !
		  IF(ABS(smixgrd%glam(ji+1*nn_rhox,jj)-smixgrd%glam(ji+2*nn_rhox,jj)).GT.180.0.AND.smixgrd%glam(ji+1*nn_rhox,jj).LT.0.) THEN
            smixgrd%glam(ji+1*nn_rhox,jj) = smixgrd%glam(ji+1*nn_rhox,jj) + 360.
		  ENDIF
		  !
		  IF(ABS(smixgrd%glam(ji+1*nn_rhox,jj)-smixgrd%glam(ji+2*nn_rhox,jj)).GT.180.0.AND.smixgrd%glam(ji+2*nn_rhox,jj).LT.0.) THEN
            smixgrd%glam(ji+2*nn_rhox,jj) = smixgrd%glam(ji+2*nn_rhox,jj) + 360.
		  ENDIF
		  !
		  IF(ABS(smixgrd%glam(ji+2*nn_rhox,jj)-smixgrd%glam(ji+3*nn_rhox,jj)).GT.180.0.AND.smixgrd%glam(ji+2*nn_rhox,jj).LT.0.) THEN
            smixgrd%glam(ji+2*nn_rhox,jj) = smixgrd%glam(ji+2*nn_rhox,jj) + 360.
		  ENDIF
		  !
		  IF(ABS(smixgrd%glam(ji+2*nn_rhox,jj)-smixgrd%glam(ji+3*nn_rhox,jj)).GT.180.0.AND.smixgrd%glam(ji+3*nn_rhox,jj).LT.0.) THEN
            smixgrd%glam(ji+3*nn_rhox,jj) = smixgrd%glam(ji+3*nn_rhox,jj) + 360.
		  ENDIF		  
		  !
		  ! If we are along north boundary,	
		  ! the variation of longitude looks like a heaviside fonction at the geographical north pole.
		  ! Thus, we can't make an interpolation.	  
		  IF(ABS(smixgrd%glam(ji,jj) - smixgrd%glam(ji+3*nn_rhox,jj)).EQ.180.0)THEN
			 llnorth_pole = .TRUE.
		  ENDIF
		  !  
		  ! Nearby the geographical north pole,
		  ! the variation of the longitudes is too important.
		  ! We need to make a polar stereographic projection before interpolation.
		  !IF(.NOT.llnorth_pole.AND.ABS(smixgrd%glam(ji,jj)-smixgrd%glam(ji+3*nn_rhox,jj)).GE.80.0) THEN
		  IF(.NOT.llnorth_pole.AND.smixgrd%gphi(ji,jj).GE.88.) THEN
			CALL stereo_projection(ji,jj,jk,llnorth_pole,1)
			jproj = 1
		  ENDIF
		  !															
		  smixgrd%glam(ji+nn_rhox+jk,jj) =   dlcoef(jk,1) * smixgrd%glam(ji,jj)            &
										   + dlcoef(jk,2) * smixgrd%glam(ji+1*nn_rhox,jj)   &
										   + dlcoef(jk,3) * smixgrd%glam(ji+2*nn_rhox,jj)   &
										   + dlcoef(jk,4) * smixgrd%glam(ji+3*nn_rhox,jj)
		  !
		  smixgrd%gphi(ji+nn_rhox+jk,jj) =   dlcoef(jk,1) * smixgrd%gphi(ji,jj)            &
										   + dlcoef(jk,2) * smixgrd%gphi(ji+1*nn_rhox,jj)   &
										   + dlcoef(jk,3) * smixgrd%gphi(ji+2*nn_rhox,jj)   &
										   + dlcoef(jk,4) * smixgrd%gphi(ji+3*nn_rhox,jj)
		  !
		  smixgrd%e1(ji+nn_rhox+jk,jj) =     dlcoef(jk,1) * smixgrd%e1(ji,jj)            &
										   + dlcoef(jk,2) * smixgrd%e1(ji+1*nn_rhox,jj)   &
										   + dlcoef(jk,3) * smixgrd%e1(ji+2*nn_rhox,jj)   &
										   + dlcoef(jk,4) * smixgrd%e1(ji+3*nn_rhox,jj)
		  !
		  smixgrd%e2(ji+nn_rhox+jk,jj) =     dlcoef(jk,1) * smixgrd%e2(ji,jj)            &
										   + dlcoef(jk,2) * smixgrd%e2(ji+1*nn_rhox,jj)   &
										   + dlcoef(jk,3) * smixgrd%e2(ji+2*nn_rhox,jj)   &
										   + dlcoef(jk,4) * smixgrd%e2(ji+3*nn_rhox,jj)
		  !
		  smixgrd%nav_lon(ji+nn_rhox+jk,jj) = smixgrd%glam(ji+nn_rhox+jk,jj)
		  smixgrd%nav_lat(ji+nn_rhox+jk,jj) = smixgrd%gphi(ji+nn_rhox+jk,jj)
		  !		
		  ! We make the polar stereographic projection reverse if needs.  
		  IF(jproj.EQ.1)THEN
			CALL stereo_projection_inv(ji,jj,jk,llnorth_pole,1)
		  ENDIF
          !
		  ! We replace the strong values along the north boundary.
  		  IF(llnorth_pole)THEN
			IF(smixgrd%glam(ji+1*nn_rhox,jj).EQ.smixgrd%glam(ji+2*nn_rhox,jj))THEN
			  smixgrd%glam(ji+nn_rhox+jk,jj) = smixgrd%glam(ji+1*nn_rhox,jj)		  
	    	ELSEIF(ABS(smixgrd%glam(ji+1*nn_rhox,jj) - smixgrd%glam(ji+2*nn_rhox,jj)).EQ.180.0)THEN
			  IF(smixgrd%gphi(ji+1*nn_rhox,jj).LT.smixgrd%gphi(ji+2*nn_rhox,jj))THEN
				smixgrd%glam(ji+nn_rhox+jk,jj) = smixgrd%glam(ji+1*nn_rhox,jj)
			  ELSEIF(smixgrd%gphi(ji+1*nn_rhox,jj).GT.smixgrd%gphi(ji+2*nn_rhox,jj))THEN
				smixgrd%glam(ji+nn_rhox+jk,jj) = smixgrd%glam(ji+2*nn_rhox,jj)
			  ENDIF
			ENDIF
		  ENDIF
		  !
		  ip = 0
		  jproj = 0
		  llnorth_pole = .FALSE.
		  !
		END DO
	  END DO
	END DO
	!
  	WHERE(smixgrd%glam.GT.180)
	  smixgrd%glam = smixgrd%glam - 360.0
	ENDWHERE
	!
	DEALLOCATE(dlcoef)
	!
	! Calculate coefficients for interpolation along latitude
	!
	ALLOCATE(dlcoef(nn_rhoy-1,4))
	istat = pol_coef(dlcoef,nn_rhoy)
	IF (istat/=1) THEN    
      WRITE(*,*) "ERROR WITH LAGRANGIAN COEFFICIENTS"
      STOP
    ENDIF
	!
	WRITE(*,*) 'Interpolation along latitude'
	!
print*, nequator
	DO ji = 1,nxgmix,1    
      !
	  DO jj = nn_rhoy,nygmix,nn_rhoy
	  	!
		DO jk = 1,nn_rhoy-1
		  !
		  IF(ABS(smixgrd%glam(ji,jj)-smixgrd%glam(ji,jj+1*nn_rhoy)).GT.180.0.AND.smixgrd%glam(ji,jj).LT.0.) THEN
	        smixgrd%glam(ji,jj) = smixgrd%glam(ji,jj) + 360.
		  ENDIF
		  !
		  IF(ABS(smixgrd%glam(ji,jj)-smixgrd%glam(ji,jj+1*nn_rhoy)).GT.180.0.AND.smixgrd%glam(ji,jj+1*nn_rhoy).LT.0.) THEN
	        smixgrd%glam(ji,jj+1*nn_rhoy) = smixgrd%glam(ji,jj+1*nn_rhoy) + 360.
		  ENDIF
		  !
		  IF(ABS(smixgrd%glam(ji,jj+1*nn_rhoy)-smixgrd%glam(ji,jj+2*nn_rhoy)).GT.180.0.AND.smixgrd%glam(ji,jj+1*nn_rhoy).LT.0.) THEN
            smixgrd%glam(ji,jj+1*nn_rhoy) = smixgrd%glam(ji,jj+1*nn_rhoy) + 360.
		  ENDIF
		  !
		  IF(ABS(smixgrd%glam(ji,jj+1*nn_rhoy)-smixgrd%glam(ji,jj+2*nn_rhoy)).GT.180.0.AND.smixgrd%glam(ji,jj+2*nn_rhoy).LT.0.) THEN
            smixgrd%glam(ji,jj+2*nn_rhoy) = smixgrd%glam(ji,jj+2*nn_rhoy) + 360.
		  ENDIF
		  !
		  IF(ABS(smixgrd%glam(ji,jj+2*nn_rhoy)-smixgrd%glam(ji,jj+3*nn_rhoy)).GT.180.0.AND.smixgrd%glam(ji,jj+2*nn_rhoy).LT.0.) THEN
            smixgrd%glam(ji,jj+2*nn_rhoy) = smixgrd%glam(ji,jj+2*nn_rhoy) + 360.
		  ENDIF
		  !
		  IF(ABS(smixgrd%glam(ji,jj+2*nn_rhoy)-smixgrd%glam(ji,jj+3*nn_rhoy)).GT.180.0.AND.smixgrd%glam(ji,jj+3*nn_rhoy).LT.0.) THEN
            smixgrd%glam(ji,jj+3*nn_rhoy) = smixgrd%glam(ji,jj+3*nn_rhoy) + 360.
		  ENDIF	
		  !
		  !IF(.NOT.llnorth_pole.AND.ABS(smixgrd%glam(ji,jj)-smixgrd%glam(ji,jj+3*nn_rhoy)).GE.60.0) THEN
		  IF(.NOT.llnorth_pole.AND.smixgrd%gphi(ji,jj).GE.88.) THEN
			CALL stereo_projection(ji,jj,jk,llnorth_pole,2)
			jproj = 1
		  ENDIF	
		  !	
		  smixgrd%glam(ji,jj+nn_rhoy+jk) =  dlcoef(jk,1) * smixgrd%glam(ji,jj)           &
								          + dlcoef(jk,2) * smixgrd%glam(ji,jj+nn_rhoy)   &
								          + dlcoef(jk,3) * smixgrd%glam(ji,jj+2*nn_rhoy) &
								          + dlcoef(jk,4) * smixgrd%glam(ji,jj+3*nn_rhoy)
		  !
		  smixgrd%gphi(ji,jj+nn_rhoy+jk) =  dlcoef(jk,1) * smixgrd%gphi(ji,jj)           &
								          + dlcoef(jk,2) * smixgrd%gphi(ji,jj+nn_rhoy)   &
									      + dlcoef(jk,3) * smixgrd%gphi(ji,jj+2*nn_rhoy) &
								          + dlcoef(jk,4) * smixgrd%gphi(ji,jj+3*nn_rhoy)
		  !
		  smixgrd%e1(ji,jj+nn_rhoy+jk) =    dlcoef(jk,1) * smixgrd%e1(ji,jj)           &
								          + dlcoef(jk,2) * smixgrd%e1(ji,jj+nn_rhoy)   &
								          + dlcoef(jk,3) * smixgrd%e1(ji,jj+2*nn_rhoy) &
								          + dlcoef(jk,4) * smixgrd%e1(ji,jj+3*nn_rhoy)
		  !
		  smixgrd%e2(ji,jj+nn_rhoy+jk) =    dlcoef(jk,1) * smixgrd%e2(ji,jj)           &
								          + dlcoef(jk,2) * smixgrd%e2(ji,jj+nn_rhoy)   &
								          + dlcoef(jk,3) * smixgrd%e2(ji,jj+2*nn_rhoy) &
								          + dlcoef(jk,4) * smixgrd%e2(ji,jj+3*nn_rhoy)
		  !
		  smixgrd%nav_lon(ji,jj+nn_rhoy+jk) = smixgrd%glam(ji,jj+nn_rhoy+jk)
		  smixgrd%nav_lat(ji,jj+nn_rhoy+jk) = smixgrd%gphi(ji,jj+nn_rhoy+jk)
		  !
		  IF(jproj.EQ.1)THEN
			CALL stereo_projection_inv(ji,jj,jk,llnorth_pole,2)
		  ENDIF	
		  !	 
		  jproj = 0
		  llnorth_pole = .FALSE.		  
		  !
	    END DO
		!							   
		IF(jj+3*nn_rhoy.EQ.nygmix) EXIT
		!
	  END DO
      !
	END DO	
	!
	WHERE(smixgrd%glam.GT.180.)
	  smixgrd%glam = smixgrd%glam - 360.0
	ENDWHERE
	!	
	WRITE(*,*) ''
	WRITE(*,*) '### END SUBROUTINE interp_grid ###'
	WRITE(*,*) ''
	!
  END SUBROUTINE
  !
  !
  !
  !********************************************************
  !                  FUNCTION pol_coef   			    *
  !									        			*
  !	      calculate the coefficients of Lagrange   		*
  !			 for the polynomial interpolation	        *
  !												        *
  !            CALLED from SUBROUTINE interp            *
  !********************************************************
  REAL FUNCTION pol_coef(kvect,kxy)
    !
	REAL*8, DIMENSION(:,:),ALLOCATABLE :: kvect
	REAL*8, DIMENSION(3) :: dlv		
	INTEGER :: ji, jm, jk
	REAL*8 :: dlx0              !position relative du point ˆ calculer 
	INTEGER :: jx_k, jx_i       !position relative des points utilisŽs pour l'interpolation
	REAL*8 :: dleps 
	INTEGER :: kxy, irho
	!
	!on parle de position relative puisque nous utilisons les positions
	!indiciaires, lesquelles sont rŽpŽtŽes dans toute la grille.
	!Il n'est donc nŽcessaire de calculer qu'une fois les 4 coefficients 
	!qui seront utilisŽs dans toute la grille en fonction de nn_rho
	!
	WRITE(*,*) ''
	WRITE(*,*) '*** FUNCTION pol_coef ***'
	WRITE(*,*) ''
	!
	jm=1
	dleps = 1.-1e-8
	!
	irho = kxy 
	!
	DO jk = 1,irho-1 
	  dlx0 = irho+1+jk
	  ji=1
	  DO jx_i = 1,4+3*(irho-1),irho
		jm=1
		DO jx_k=1,4+3*(irho-1),irho
		  IF(jx_k == jx_i) THEN
			CYCLE
		  ELSE	
			dlv(jm) = (dlx0-jx_k) / (jx_i-jx_k)
			jm = jm + 1
		  END IF
		END DO
		kvect(jk,ji) = product(dlv)
		ji = ji + 1
	  END DO
	END DO
	!
	IF(SUM(kvect).LT.dleps .OR. SUM(kvect).GT.dleps) THEN
	  WRITE(*,*) ''
	  WRITE(*,*) '*** CHECK LAGRANGE COEFFICIENTS: ***'
	  WRITE(*,*) ''
	  !	  
	  DO ji=1,irho-1
		WRITE(*,*)'point #',ji
		WRITE(*,*) 'dlcoef(:)= ', kvect(ji,:)
		WRITE(*,*) 'SUM(dlcoef(:)) =', SUM(kvect(ji,:))
		WRITE(*,*)''
	  END DO
	  pol_coef = 1
	ELSE
	  !
	  pol_coef = 0
    ENDIF
	!
  END FUNCTION pol_coef
  !
  !
  !
  !********************************************************
  !                SUBROUTINE child_grid     		    *
  !									        			*
  !	      create the child grids from mixed grid	    *
  !												        *
  !		   CALLED from create_coordinates.f90	        *
  !********************************************************
  SUBROUTINE child_grid	 
    !
	INTEGER :: ji, jj
	INTEGER :: ip, iq
	REAL*8, DIMENSION(2) :: dlgrdt, dlgrdu, dlgrdv, dlgrdf
	REAL*8 :: dleps
	!
	WRITE(*,*) ''
	WRITE(*,*) '### SUBROUTINE child_grid ###'
	WRITE(*,*) ''
	!	
	IF(.NOT.nglobal.AND.(nn_rhox.GT.1.OR.nn_rhoy.GT.1)) THEN
	  iq=1
	  DO jj = (2*nn_rhoy)+1-nequator,(nygmix-(2*nn_rhoy-1)-nequator),2
	    ip=1
	    DO ji = (2*nn_rhox)+1,(nxgmix-(2*nn_rhox-1)),2
		  !
		  sfingrd%nav_lon(ip,iq) = smixgrd%nav_lon(ji,jj)
		  sfingrd%nav_lat(ip,iq) = smixgrd%nav_lat(ji,jj)
		  !
		  sfingrd%glamt(ip,iq) = smixgrd%glam(ji,jj)
		  sfingrd%glamu(ip,iq) = smixgrd%glam(ji+1,jj)
		  sfingrd%glamv(ip,iq) = smixgrd%glam(ji,jj+1)
		  sfingrd%glamf(ip,iq) = smixgrd%glam(ji+1,jj+1)
		  !
		  sfingrd%gphit(ip,iq) = smixgrd%gphi(ji,jj)
		  sfingrd%gphiu(ip,iq) = smixgrd%gphi(ji+1,jj)
		  sfingrd%gphiv(ip,iq) = smixgrd%gphi(ji,jj+1)
		  sfingrd%gphif(ip,iq) = smixgrd%gphi(ji+1,jj+1)
		  !
		  sfingrd%e1t(ip,iq) = smixgrd%e1(ji,jj)
		  sfingrd%e1u(ip,iq) = smixgrd%e1(ji+1,jj)
		  sfingrd%e1v(ip,iq) = smixgrd%e1(ji,jj+1)
		  sfingrd%e1f(ip,iq) = smixgrd%e1(ji+1,jj+1)
		  !
		  sfingrd%e2t(ip,iq) = smixgrd%e2(ji,jj)
		  sfingrd%e2u(ip,iq) = smixgrd%e2(ji+1,jj)
		  sfingrd%e2v(ip,iq) = smixgrd%e2(ji,jj+1)
		  sfingrd%e2f(ip,iq) = smixgrd%e2(ji+1,jj+1)
		  !		
		  ip=ip+1
		ENDDO
		iq=iq+1
	  ENDDO
      !
	ELSEIF(nglobal.AND.(nn_rhox.GT.1.OR.nn_rhoy.GT.1))THEN
	  iq=1
      DO jj = (2*nn_rhoy)+1-nequator,nygmix,2  
	    ip=1
		DO ji = (2*nn_rhox)-1,nxgmix,2
		  !
		  sfingrd%nav_lon(ip,iq) = smixgrd%nav_lon(ji,jj)
		  sfingrd%nav_lat(ip,iq) = smixgrd%nav_lat(ji,jj)
		  !
		  sfingrd%glamt(ip,iq) = smixgrd%glam(ji,jj)
		  sfingrd%glamu(ip,iq) = smixgrd%glam(ji+1,jj)
		  sfingrd%glamv(ip,iq) = smixgrd%glam(ji,jj+1)
		  sfingrd%glamf(ip,iq) = smixgrd%glam(ji+1,jj+1)
		  !
		  sfingrd%gphit(ip,iq) = smixgrd%gphi(ji,jj)
		  sfingrd%gphiu(ip,iq) = smixgrd%gphi(ji+1,jj)
		  sfingrd%gphiv(ip,iq) = smixgrd%gphi(ji,jj+1)
		  sfingrd%gphif(ip,iq) = smixgrd%gphi(ji+1,jj+1)
		  !
		  sfingrd%e1t(ip,iq) = smixgrd%e1(ji,jj)
		  sfingrd%e1u(ip,iq) = smixgrd%e1(ji+1,jj)
		  sfingrd%e1v(ip,iq) = smixgrd%e1(ji,jj+1)
		  sfingrd%e1f(ip,iq) = smixgrd%e1(ji+1,jj+1)
		  !
		  sfingrd%e2t(ip,iq) = smixgrd%e2(ji,jj)
		  sfingrd%e2u(ip,iq) = smixgrd%e2(ji+1,jj)
		  sfingrd%e2v(ip,iq) = smixgrd%e2(ji,jj+1)
		  sfingrd%e2f(ip,iq) = smixgrd%e2(ji+1,jj+1)
		  !	
		  IF(ip.EQ.nxfine) EXIT
		  !
		  ip=ip+1
		  !
		ENDDO
		!
		IF(iq.EQ.nyfine) EXIT
		!
		iq=iq+1		
		!
	  ENDDO
	  !
	ELSE                                                 !No interpolation
	  iq=1
	  DO jj = 1,nygmix-1,2    
	    ip=1
	    DO ji = 1,nxgmix-1,2
		  !
		  sfingrd%nav_lon(ip,iq) = smixgrd%nav_lon(ji,jj)
		  sfingrd%nav_lat(ip,iq) = smixgrd%nav_lat(ji,jj)
		  !
		  sfingrd%glamt(ip,iq) = smixgrd%glam(ji,jj)
		  sfingrd%glamu(ip,iq) = smixgrd%glam(ji+1,jj)
		  sfingrd%glamv(ip,iq) = smixgrd%glam(ji,jj+1)
		  sfingrd%glamf(ip,iq) = smixgrd%glam(ji+1,jj+1)
		  !
		  sfingrd%gphit(ip,iq) = smixgrd%gphi(ji,jj)
		  sfingrd%gphiu(ip,iq) = smixgrd%gphi(ji+1,jj)
		  sfingrd%gphiv(ip,iq) = smixgrd%gphi(ji,jj+1)
		  sfingrd%gphif(ip,iq) = smixgrd%gphi(ji+1,jj+1)
		  !
		  sfingrd%e1t(ip,iq) = smixgrd%e1(ji,jj)
		  sfingrd%e1u(ip,iq) = smixgrd%e1(ji+1,jj)
		  sfingrd%e1v(ip,iq) = smixgrd%e1(ji,jj+1)
		  sfingrd%e1f(ip,iq) = smixgrd%e1(ji+1,jj+1)
		  !
		  sfingrd%e2t(ip,iq) = smixgrd%e2(ji,jj)
		  sfingrd%e2u(ip,iq) = smixgrd%e2(ji+1,jj)
		  sfingrd%e2v(ip,iq) = smixgrd%e2(ji,jj+1)
		  sfingrd%e2f(ip,iq) = smixgrd%e2(ji+1,jj+1)
		  !		
		  ip=ip+1
		END DO
		iq=iq+1
	  END DO
    !
	ENDIF
	!
	! With a global domain, we check the overlap bands for have
	! * Grid(1,:) = Grid(n-1,:)
	! * Grid(2,:) = Grid(n,:)
	! because after interpolation the first column have no values  
	IF(nglobal.AND.nn_rhox.GT.1)THEN
	  !
	  sfingrd%nav_lon(1,:) = sfingrd%nav_lon(nxfine-1,:)
	  sfingrd%nav_lat(1,:) = sfingrd%nav_lat(nxfine-1,:)
	  !
	  sfingrd%glamt(1,:) = sfingrd%glamt(nxfine-1,:)
	  sfingrd%glamu(1,:) = sfingrd%glamu(nxfine-1,:)
	  sfingrd%glamv(1,:) = sfingrd%glamv(nxfine-1,:)
	  sfingrd%glamf(1,:) = sfingrd%glamf(nxfine-1,:)
	  !
	  sfingrd%gphit(1,:) = sfingrd%gphit(nxfine-1,:)
	  sfingrd%gphiu(1,:) = sfingrd%gphiu(nxfine-1,:)
	  sfingrd%gphiv(1,:) = sfingrd%gphiv(nxfine-1,:)
	  sfingrd%gphif(1,:) = sfingrd%gphif(nxfine-1,:)
	  !
	  sfingrd%e1t(1,:) = sfingrd%e1t(nxfine-1,:)
	  sfingrd%e1u(1,:) = sfingrd%e1u(nxfine-1,:)
	  sfingrd%e1v(1,:) = sfingrd%e1v(nxfine-1,:)
	  sfingrd%e1f(1,:) = sfingrd%e1f(nxfine-1,:)
	  !
	  sfingrd%e2t(1,:) = sfingrd%e2t(nxfine-1,:)
	  sfingrd%e2u(1,:) = sfingrd%e2u(nxfine-1,:)
	  sfingrd%e2v(1,:) = sfingrd%e2v(nxfine-1,:)
	  sfingrd%e2f(1,:) = sfingrd%e2f(nxfine-1,:)
	  !
	  WRITE(*,*) ''
	  WRITE(*,*) 'WE CHECK THE OVERLAP BANDS FOR EACH FINE GRID (T,U,V & F):'
	  WRITE(*,*) ' ==> SUM{ grd(1,:) + grd(2,:) } - SUM{ grd(n-1) + grd(n,:) } = 0'
	  !
	  dleps = 1e-2
	  !
	  WRITE(*,*) '* grid T:'
	  dlgrdt(1) = SUM(sfingrd%glamt(1,:)) + SUM(sfingrd%glamt(2,:))
	  dlgrdt(1) = dlgrdt(1) + SUM(sfingrd%gphit(1,:)) + SUM(sfingrd%gphit(2,:))  
	  dlgrdt(1) = dlgrdt(1) + SUM(sfingrd%e1t(1,:)) + SUM(sfingrd%e1t(2,:))  
	  dlgrdt(1) = dlgrdt(1) + SUM(sfingrd%e2t(1,:)) + SUM(sfingrd%e2t(2,:)) 
	  !
	  dlgrdt(2) = SUM(sfingrd%glamt(nxfine-1,:)) + SUM(sfingrd%glamt(nxfine,:))
	  dlgrdt(2) = dlgrdt(2) + SUM(sfingrd%gphit(nxfine-1,:)) + SUM(sfingrd%gphit(nxfine,:))  
	  dlgrdt(2) = dlgrdt(2) + SUM(sfingrd%e1t(nxfine-1,:)) + SUM(sfingrd%e1t(nxfine,:))  
	  dlgrdt(2) = dlgrdt(2) + SUM(sfingrd%e2t(nxfine-1,:)) + SUM(sfingrd%e2t(nxfine,:))	
	  !  
	  IF((dlgrdt(1)-dlgrdt(2)).GT.dleps.OR.(dlgrdt(1)+dlgrdt(2)).LT.dleps) THEN				
	    WRITE(*,*) '   ERROR'
		print*,(dlgrdt(1)-dlgrdt(2)), (dlgrdt(1)+dlgrdt(2))
	  ELSE
	    WRITE(*,*) 'OVERLAP BANDS OK'
	  ENDIF
	  !
	  WRITE(*,*) '* grid U:'
	  dlgrdu(1) = SUM(sfingrd%glamu(1,:)) + SUM(sfingrd%glamu(2,:))
	  dlgrdu(1) = dlgrdu(1) + SUM(sfingrd%gphiu(1,:)) + SUM(sfingrd%gphiu(2,:))  
	  dlgrdu(1) = dlgrdu(1) + SUM(sfingrd%e1u(1,:)) + SUM(sfingrd%e1u(2,:))  
	  dlgrdu(1) = dlgrdu(1) + SUM(sfingrd%e2u(1,:)) + SUM(sfingrd%e2u(2,:)) 
	  !
	  dlgrdu(2) = SUM(sfingrd%glamu(nxfine-1,:)) + SUM(sfingrd%glamu(nxfine,:))
	  dlgrdu(2) = dlgrdu(2) + SUM(sfingrd%gphiu(nxfine-1,:)) + SUM(sfingrd%gphiu(nxfine,:))  
	  dlgrdu(2) = dlgrdu(2) + SUM(sfingrd%e1u(nxfine-1,:)) + SUM(sfingrd%e1u(nxfine,:))  
	  dlgrdu(2) = dlgrdu(2) + SUM(sfingrd%e2u(nxfine-1,:)) + SUM(sfingrd%e2u(nxfine,:))	
	  !  
	  IF((dlgrdu(1)-dlgrdu(2)).GT.dleps.OR.(dlgrdu(1)+dlgrdu(2)).LT.dleps) THEN				
	    WRITE(*,*) '   ERROR'
		print*,(dlgrdu(1)-dlgrdu(2)),  (dlgrdu(1)+dlgrdu(2))
	  ELSE
	    WRITE(*,*) 'OVERLAP BANDS OK'
	  ENDIF
	  !	  
	  WRITE(*,*) '* grid V:'
	  dlgrdv(1) = SUM(sfingrd%glamv(1,:)) + SUM(sfingrd%glamv(2,:))
	  dlgrdv(1) = dlgrdv(1) + SUM(sfingrd%gphiv(1,:)) + SUM(sfingrd%gphiv(2,:))  
	  dlgrdv(1) = dlgrdv(1) + SUM(sfingrd%e1v(1,:)) + SUM(sfingrd%e1v(2,:))  
	  dlgrdv(1) = dlgrdv(1) + SUM(sfingrd%e2v(1,:)) + SUM(sfingrd%e2v(2,:)) 
	  !
	  dlgrdv(2) = SUM(sfingrd%glamv(nxfine-1,:)) + SUM(sfingrd%glamv(nxfine,:))
	  dlgrdv(2) = dlgrdv(2) + SUM(sfingrd%gphiv(nxfine-1,:)) + SUM(sfingrd%gphiv(nxfine,:))  
	  dlgrdv(2) = dlgrdv(2) + SUM(sfingrd%e1v(nxfine-1,:)) + SUM(sfingrd%e1v(nxfine,:))  
	  dlgrdv(2) = dlgrdv(2) + SUM(sfingrd%e2v(nxfine-1,:)) + SUM(sfingrd%e2v(nxfine,:))	
	  !  
	  IF((dlgrdv(1)-dlgrdv(2)).GT.dleps.OR.(dlgrdv(1)+dlgrdv(2)).LT.dleps) THEN				
	    WRITE(*,*) '   ERROR'
		print*,(dlgrdv(1)-dlgrdv(2)), (dlgrdv(1)+dlgrdv(2))
	  ELSE
	    WRITE(*,*) 'OVERLAP BANDS OK'
	  ENDIF
	  !	
	  WRITE(*,*) '* grid F:'
	  dlgrdf(1) = SUM(sfingrd%glamf(1,:)) + SUM(sfingrd%glamf(2,:))
	  dlgrdf(1) = dlgrdf(1) + SUM(sfingrd%gphif(1,:)) + SUM(sfingrd%gphif(2,:))  
	  dlgrdf(1) = dlgrdf(1) + SUM(sfingrd%e1f(1,:)) + SUM(sfingrd%e1f(2,:))  
	  dlgrdf(1) = dlgrdf(1) + SUM(sfingrd%e2f(1,:)) + SUM(sfingrd%e2f(2,:)) 
	  !
	  dlgrdf(2) = SUM(sfingrd%glamf(nxfine-1,:)) + SUM(sfingrd%glamf(nxfine,:))
	  dlgrdf(2) = dlgrdf(2) + SUM(sfingrd%gphif(nxfine-1,:)) + SUM(sfingrd%gphif(nxfine,:))  
	  dlgrdf(2) = dlgrdf(2) + SUM(sfingrd%e1f(nxfine-1,:)) + SUM(sfingrd%e1f(nxfine,:))  
	  dlgrdf(2) = dlgrdf(2) + SUM(sfingrd%e2f(nxfine-1,:)) + SUM(sfingrd%e2f(nxfine,:))	
	  !  
	  IF((dlgrdf(1)-dlgrdf(2)).GT.dleps.OR.(dlgrdf(1)+dlgrdf(2)).LT.dleps) THEN				
	    WRITE(*,*) '   ERROR'
		print*, (dlgrdf(1)-dlgrdf(2)),  (dlgrdf(1)+dlgrdf(2))
	  ELSE
	    WRITE(*,*) 'OVERLAP BANDS OK'
	  ENDIF
	  !
	  WRITE(*,*) ''
	  WRITE(*,*) ' grid T'
	  WRITE(*,*)  'i=    ','      1            ','         2            ','          3   '
	  WRITE(*,*)  sfingrd%glamt(1:3,1)
	  WRITE(*,*)  sfingrd%gphit(1:3,1)
	  WRITE(*,*)  'i=    ','     n-2           ','        n-1           ','          n   '
	  WRITE(*,*)  sfingrd%glamt(nxfine-2:nxfine,1)
	  WRITE(*,*)  sfingrd%gphit(nxfine-2:nxfine,1)
	  WRITE(*,*) ''
	  WRITE(*,*) ' grid U'
	  WRITE(*,*)  'i=    ','      1            ','         2            ','          3   '
	  WRITE(*,*)  sfingrd%glamu(1:3,1)
	  WRITE(*,*)  sfingrd%gphiu(1:3,1)
	  WRITE(*,*)  'i=    ','     n-2           ','        n-1           ','          n   '
	  WRITE(*,*)  sfingrd%glamu(nxfine-2:nxfine,1)
	  WRITE(*,*)  sfingrd%gphiu(nxfine-2:nxfine,1)
	  WRITE(*,*) ''
	  WRITE(*,*) ' grid V'
	  WRITE(*,*)  'i=    ','      1            ','         2            ','          3   '
	  WRITE(*,*)  sfingrd%glamv(1:3,1)
	  WRITE(*,*)  sfingrd%gphiv(1:3,1)
	  WRITE(*,*)  'i=    ','     n-2           ','        n-1           ','          n   '
	  WRITE(*,*)  sfingrd%glamv(nxfine-2:nxfine,1)
	  WRITE(*,*)  sfingrd%gphiv(nxfine-2:nxfine,1)
	  WRITE(*,*) ''	
	  WRITE(*,*) ' grid F'
	  WRITE(*,*)  'i=    ','      1            ','         2            ','          3   '
	  WRITE(*,*)  sfingrd%glamf(1:3,1)
	  WRITE(*,*)  sfingrd%gphif(1:3,1)
	  WRITE(*,*)  'i=    ','     n-2           ','        n-1           ','          n   '
	  WRITE(*,*)  sfingrd%glamf(nxfine-2:nxfine,1)
	  WRITE(*,*)  sfingrd%gphif(nxfine-2:nxfine,1)
	  WRITE(*,*) ''
	ENDIF
	!
	IF(nglobal.AND.nequator.EQ.1) THEN
      ! *** GRID U
	  jj = 1
	  DO ji = nxfine/2 + nn_rhox,nxfine,2
	    sfingrd%glamu(ji,nyfine-1) = sfingrd%glamu(nxfine/2+1 - jj,nyfine-1)
	    sfingrd%gphiu(ji,nyfine-1) = sfingrd%gphiu(nxfine/2+1 - jj,nyfine-1)
	    jj = jj+2
	  ENDDO
	  !
	  jj = 0
	  DO ji = 2,nxfine
	    sfingrd%glamu(ji,nyfine) = sfingrd%glamu(nxfine - jj,nyfine-2)
	    sfingrd%gphiu(ji,nyfine) = sfingrd%gphiu(nxfine - jj,nyfine-2)
	    jj = jj+1
	  ENDDO
	  !	
	  ! *** GRID T 
	  jj = 0
	  DO ji = nxfine/2 + nn_rhox,nxfine
	    sfingrd%glamt(ji,nyfine-1) = sfingrd%glamt(nxfine/2+1 - jj,nyfine-1)
	    sfingrd%gphit(ji,nyfine-1) = sfingrd%gphit(nxfine/2+1 - jj,nyfine-1)
	    jj = jj+1
	  ENDDO
	  !
	  jj = 0
	  DO ji = 3,nxfine
	    sfingrd%glamt(ji,nyfine) = sfingrd%glamt(nxfine - jj,nyfine-2)
	    sfingrd%gphit(ji,nyfine) = sfingrd%gphit(nxfine - jj,nyfine-2)
	    jj = jj+1
	  ENDDO
    ENDIF			  
	!
	WRITE(*,*) ''
	WRITE(*,*) '### END SUBROUTINE child_grid ###'
	WRITE(*,*) ''
	!
  END SUBROUTINE child_grid
  !
  !
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE cfg_tools
