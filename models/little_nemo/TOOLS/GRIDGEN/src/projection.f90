MODULE projection
!!-----------------------------------------------------------
!!
!!          to make a polar stereographic projection 
!!              in the area of the north pole
!!
!!	         Created by Brice Lemaire on 05/2010.
!!
!!-----------------------------------------------------------
  USE readwrite
  !
  IMPLICIT NONE
  PUBLIC
  !
  REAL*8,DIMENSION(4) :: dmixlam, dmixphi
  REAL*8 :: dray = 6357          !rayon polaire de la Terre (km)    
  REAL*8 :: PI = ACOS(-1.0)
  REAL*8 :: dlong0 = 0.          !longitude du centre de projection (origine)
  REAL*8 :: dlat0 = 90.          !latitude       ''    
  INTEGER :: idisc = 0           !to check the +/- 180 discontinuity
  INTEGER :: m
  !
  CONTAINS  
  !********************************************************
  !            SUBROUTINE stereo_projection      	    *
  !									        			*
  !												        *
  !		          CALLED by cfg_tools.F90	            *
  !********************************************************
  SUBROUTINE stereo_projection(kji,kjj,kjk,knorth_pole,kway)
    !
    INTEGER,INTENT(IN) :: kji,kjj,kjk
	LOGICAL,INTENT(IN) :: knorth_pole
	INTEGER,INTENT(IN) :: kway
	INTEGER :: klon, klat
	REAL*8,DIMENSION(4) :: dlx, dly
	REAL*8,DIMENSION(4) :: dllam, dlphi, dlk
	REAL*8 :: dl_lat0, dl_long0
	!
    dl_lat0 = dlat0 * PI/180.
    dl_long0 = dlong0 * PI/180.
    !
	!Either we interpolate along longitude or along latitude
	IF(kway.EQ.1)THEN
	  klon = 1
	  klat = 0
	ELSEIF(kway.EQ.2) THEN
	  klon = 0
	  klat = 1
	ENDIF
	!
	! Check the discontinuity +/- 180
	IF(kway.EQ.1)THEN
	  IF(smixgrd%glam(kji,kjj).LT.180..AND.smixgrd%glam(kji+3*nn_rhox,kjj).GT.180.) THEN
	    IF(smixgrd%glam(kji+2*nn_rhox,kjj).LT.180.) THEN
	      idisc = 1
	    ELSEIF(smixgrd%glam(kji+1*nn_rhox,kjj).GT.180.) THEN 
	      idisc = 2
        ELSEIF(smixgrd%glam(kji+2*nn_rhox,kjj).GT.180.AND.smixgrd%glam(kji+1*nn_rhox,kjj).LT.180.)THEN
	      idisc = 3
	    ENDIF
	  ELSE
	  idisc = 0
	  ENDIF
	ENDIF
	!
	dllam(1) = smixgrd%glam(kji,kjj) * PI/180.
	dlphi(1) = smixgrd%gphi(kji,kjj) * PI/180.
	dlk(1)   = (2*dray) / (1 + (SIN(dl_lat0)*SIN(dlphi(1))) + (COS(dl_lat0)*COS(dlphi(1))*COS(dllam(1) - dl_long0)))
	!
	dllam(2) = smixgrd%glam(kji+1*nn_rhox*klon,kjj+1*nn_rhoy*klat) * PI/180.
	dlphi(2) = smixgrd%gphi(kji+1*nn_rhox*klon,kjj+1*nn_rhoy*klat) * PI/180.
	dlk(2)   = (2*dray) / (1 + (SIN(dl_lat0)*SIN(dlphi(2))) + (COS(dl_lat0)*COS(dlphi(2))*COS(dllam(2) - dl_long0)))
	!
	dllam(3) = smixgrd%glam(kji+2*nn_rhox*klon,kjj+2*nn_rhoy*klat) * PI/180.
	dlphi(3) = smixgrd%gphi(kji+2*nn_rhox*klon,kjj+2*nn_rhoy*klat) * PI/180.
	dlk(3)   = (2*dray) / (1 + (SIN(dl_lat0)*SIN(dlphi(3))) + (COS(dl_lat0)*COS(dlphi(3))*COS(dllam(3) - dl_long0)))
	!
	dllam(4) = smixgrd%glam(kji+3*nn_rhox*klon,kjj+3*nn_rhoy*klat) * PI/180.
	dlphi(4) = smixgrd%gphi(kji+3*nn_rhox*klon,kjj+3*nn_rhoy*klat) * PI/180.
	dlk(4)   = (2*dray) / (1 + (SIN(dl_lat0)*SIN(dlphi(4))) + (COS(dl_lat0)*COS(dlphi(4))*COS(dllam(4) - dl_long0)))
	!	
	dlx(1) = dlk(1) * COS(dlphi(1)) * SIN(dllam(1) - dl_long0)
	dly(1) = dlk(1) * ((COS(dl_lat0) * SIN(dlphi(1))) - (SIN(dl_lat0) * COS(dlphi(1)) * COS(dllam(1) - dl_long0)))
    !
	dlx(2) = dlk(2) * COS(dlphi(2)) * SIN(dllam(2) - dl_long0)
	dly(2) = dlk(2) * ((COS(dl_lat0) * SIN(dlphi(2))) - (SIN(dl_lat0) * COS(dlphi(2)) * COS(dllam(2) - dl_long0)))
    !
	dlx(3) = dlk(3) * COS(dlphi(3)) * SIN(dllam(3) - dl_long0)
	dly(3) = dlk(3) * ((COS(dl_lat0) * SIN(dlphi(3))) - (SIN(dl_lat0) * COS(dlphi(3)) * COS(dllam(3) - dl_long0)))
    !
	dlx(4) = dlk(4) * COS(dlphi(4)) * SIN(dllam(4) - dl_long0)
	dly(4) = dlk(4) * ((COS(dl_lat0) * SIN(dlphi(4))) - (SIN(dl_lat0) * COS(dlphi(4)) * COS(dllam(4) - dl_long0)))
    ! 
	dmixlam(1) = smixgrd%glam(kji,kjj)         
	dmixlam(2) = smixgrd%glam(kji+1*nn_rhox*klon,kjj+1*nn_rhoy*klat) 
    dmixlam(3) = smixgrd%glam(kji+2*nn_rhox*klon,kjj+2*nn_rhoy*klat) 
    dmixlam(4) = smixgrd%glam(kji+3*nn_rhox*klon,kjj+3*nn_rhoy*klat)
	!
	dmixphi(1) = smixgrd%gphi(kji,kjj)       
	dmixphi(2) = smixgrd%gphi(kji+1*nn_rhox*klon,kjj+1*nn_rhoy*klat) 
    dmixphi(3) = smixgrd%gphi(kji+2*nn_rhox*klon,kjj+2*nn_rhoy*klat) 
    dmixphi(4) = smixgrd%gphi(kji+3*nn_rhox*klon,kjj+3*nn_rhoy*klat)
	!
	smixgrd%glam(kji,kjj)                               = dlx(1)
	smixgrd%glam(kji+1*nn_rhox*klon,kjj+1*nn_rhoy*klat) = dlx(2)
	smixgrd%glam(kji+2*nn_rhox*klon,kjj+2*nn_rhoy*klat) = dlx(3)
	smixgrd%glam(kji+3*nn_rhox*klon,kjj+3*nn_rhoy*klat) = dlx(4)
	!
	smixgrd%gphi(kji,kjj)                               = dly(1)
	smixgrd%gphi(kji+1*nn_rhox*klon,kjj+1*nn_rhoy*klat) = dly(2)
    smixgrd%gphi(kji+2*nn_rhox*klon,kjj+2*nn_rhoy*klat) = dly(3)
    smixgrd%gphi(kji+3*nn_rhox*klon,kjj+3*nn_rhoy*klat) = dly(4)
	!
  END SUBROUTINE stereo_projection
  ! 
  !
  !
  !********************************************************
  !          SUBROUTINE stereo_projection_inv    	    *
  !									        			*
  !												        *
  !		            CALLED from here	                *
  !********************************************************
  SUBROUTINE stereo_projection_inv(kji,kjj,kjk,knorth_pole,kway)
    !
    INTEGER,INTENT(IN) :: kji,kjj,kjk
	LOGICAL,INTENT(IN) :: knorth_pole
	INTEGER,INTENT(IN) :: kway
	INTEGER :: klon, klat
	REAL*8,DIMENSION(5) :: dlx, dly
	REAL*8,DIMENSION(5)  :: dllam, dlphi
    REAL*8,DIMENSION(5)  :: dlro, dlc
	REAL*8 :: dl_long0, dl_lat0
	!	
    dl_lat0 = dlat0 * PI/180.
    dl_long0 = dlong0 * PI/180.
    !
 	IF(kway.EQ.1)THEN
	  klon = 1
	  klat = 0
	ELSEIF(kway.EQ.2) THEN
	  klon = 0
	  klat = 1
	ENDIF
	!
	!
	dlx(1) = smixgrd%glam(kji,kjj)
	dlx(2) = smixgrd%glam(kji+1*nn_rhox*klon,kjj+1*nn_rhoy*klat)
	dlx(3) = smixgrd%glam(kji+2*nn_rhox*klon,kjj+2*nn_rhoy*klat)
	dlx(4) = smixgrd%glam(kji+3*nn_rhox*klon,kjj+3*nn_rhoy*klat)	
	dlx(5) = smixgrd%glam(kji+(nn_rhox+kjk)*klon,kjj+(nn_rhoy+kjk)*klat) 
	!
	dly(1) = smixgrd%gphi(kji,kjj)
	dly(2) = smixgrd%gphi(kji+1*nn_rhox*klon,kjj+1*nn_rhoy*klat)
	dly(3) = smixgrd%gphi(kji+2*nn_rhox*klon,kjj+2*nn_rhoy*klat)
	dly(4) = smixgrd%gphi(kji+3*nn_rhox*klon,kjj+3*nn_rhoy*klat)	
	dly(5) = smixgrd%gphi(kji+(nn_rhox+kjk)*klon,kjj+(nn_rhoy+kjk)*klat) 	
	!
	dlro(1) = SQRT(dlx(1)*dlx(1) + dly(1)*dly(1))
	dlro(2) = SQRT(dlx(2)*dlx(2) + dly(2)*dly(2))
	dlro(3) = SQRT(dlx(3)*dlx(3) + dly(3)*dly(3))
	dlro(4) = SQRT(dlx(4)*dlx(4) + dly(4)*dly(4))
	dlro(5) = SQRT(dlx(5)*dlx(5) + dly(5)*dly(5))
	!
	dlc(1) = 2*ATAN(dlro(1)/(2*dray))
	dlc(2) = 2*ATAN(dlro(2)/(2*dray))
	dlc(3) = 2*ATAN(dlro(3)/(2*dray))
	dlc(4) = 2*ATAN(dlro(4)/(2*dray))
	dlc(5) = 2*ATAN(dlro(5)/(2*dray))
	!
	dlphi(1) =  ASIN(COS(dlc(1))*SIN(dl_lat0) + (dly(1)*SIN(dlc(1))*COS(dl_lat0) / dlro(1)))
	dlphi(2) =  ASIN(COS(dlc(2))*SIN(dl_lat0) + (dly(2)*SIN(dlc(2))*COS(dl_lat0) / dlro(2)))
	dlphi(3) =  ASIN(COS(dlc(3))*SIN(dl_lat0) + (dly(3)*SIN(dlc(3))*COS(dl_lat0) / dlro(3)))
	dlphi(4) =  ASIN(COS(dlc(4))*SIN(dl_lat0) + (dly(4)*SIN(dlc(4))*COS(dl_lat0) / dlro(4)))
	dlphi(5) =  ASIN(COS(dlc(5))*SIN(dl_lat0) + (dly(5)*SIN(dlc(5))*COS(dl_lat0) / dlro(5)))
    !	
	dlphi(:) = dlphi(:) * 180./PI
    !
	dllam(1) = dl_long0 + ATAN(dlx(1)*SIN(dlc(1) / ((dlro(1)*COS(dl_lat0)*COS(dlc(1)))-(dly(1)*SIN(dl_lat0)*SIN(dlc(1))))))
	dllam(2) = dl_long0 + ATAN(dlx(2)*SIN(dlc(2) / ((dlro(2)*COS(dl_lat0)*COS(dlc(2)))-(dly(2)*SIN(dl_lat0)*SIN(dlc(2))))))
	dllam(3) = dl_long0 + ATAN(dlx(3)*SIN(dlc(3) / ((dlro(3)*COS(dl_lat0)*COS(dlc(3)))-(dly(3)*SIN(dl_lat0)*SIN(dlc(3))))))
	dllam(4) = dl_long0 + ATAN(dlx(4)*SIN(dlc(4) / ((dlro(4)*COS(dl_lat0)*COS(dlc(4)))-(dly(4)*SIN(dl_lat0)*SIN(dlc(4))))))
	dllam(5) = dl_long0 + ATAN(dlx(5)*SIN(dlc(5) / ((dlro(5)*COS(dl_lat0)*COS(dlc(5)))-(dly(5)*SIN(dl_lat0)*SIN(dlc(5))))))
    !
	dllam(1) = dllam(1) * 180./PI
	dllam(2) = dllam(2) * 180./PI 
	dllam(3) = dllam(3) * 180./PI 
	dllam(4) = dllam(4) * 180./PI 
    !
	dllam(5) = dllam(5) * 180./PI 
	!
	IF(kway.EQ.1)THEN		
	  IF(idisc.EQ.1) THEN
		dllam(5) = dllam(5) + 180.
	  ELSEIF(idisc.EQ.2) THEN
		dllam(5) = dllam(5) - 180.
	  ELSEIF(idisc.EQ.3) THEN
		 dllam(5) = dllam(5) - 180.
	  ELSE
		dllam(5) = dllam(5)  
	  ENDIF
	  !
	ELSEIF(kway.EQ.2)THEN
	  IF(smixgrd%glam(kji,kjj+1*nn_rhoy).GT.0.AND.smixgrd%glam(kji,kjj+2*nn_rhoy).GT.0)THEN
	    IF(dllam(5).LT.0.)THEN
	      dllam(5) = dllam(5) + 180
		ENDIF
	  ELSEIF(smixgrd%glam(kji,kjj+1*nn_rhoy).LT.0.AND.smixgrd%glam(kji,kjj+2*nn_rhoy).LT.0)THEN
	    IF(dllam(5).GT.0.)THEN
		  dllam(5) = dllam(5) - 180
	    ENDIF
	  ELSEIF(smixgrd%glam(kji,kjj+1*nn_rhoy)*smixgrd%glam(kji,kjj+2*nn_rhoy).LT.0)THEN
	    IF(dllam(5).LT.0.)THEN
	      dllam(5) = dllam(5) + 180
	    ELSEIF(dllam(5).GT.0.)THEN
	      dllam(5) = dllam(5) - 180
	    ENDIF
	  ENDIF
	ENDIF
	!
	smixgrd%glam(kji,kjj)                                       = dmixlam(1)
	smixgrd%glam(kji+1*nn_rhox*klon,kjj+1*nn_rhoy*klat)         = dmixlam(2)
    smixgrd%glam(kji+2*nn_rhox*klon,kjj+2*nn_rhoy*klat)         = dmixlam(3)
    smixgrd%glam(kji+3*nn_rhox*klon,kjj+3*nn_rhoy*klat)         = dmixlam(4)
	!
    smixgrd%glam(kji+(nn_rhox+kjk)*klon,kjj+(nn_rhoy+kjk)*klat) = dllam(5)
	!
	smixgrd%gphi(kji,kjj)                                       = dmixphi(1)
	smixgrd%gphi(kji+1*nn_rhox*klon,kjj+1*nn_rhoy*klat)         = dmixphi(2)
    smixgrd%gphi(kji+2*nn_rhox*klon,kjj+2*nn_rhoy*klat)         = dmixphi(3)
    smixgrd%gphi(kji+3*nn_rhox*klon,kjj+3*nn_rhoy*klat)         = dmixphi(4)
	!
    smixgrd%gphi(kji+(nn_rhox+kjk)*klon,kjj+(nn_rhoy+kjk)*klat) = dlphi(5)
	!
  END SUBROUTINE stereo_projection_inv
  !
  !
  !
END MODULE projection
