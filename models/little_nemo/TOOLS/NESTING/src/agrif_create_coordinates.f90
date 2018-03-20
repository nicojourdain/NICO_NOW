PROGRAM create_coordinate
  !
  USE NETCDF
  USE agrif_readwrite
  USE agrif_interpolation
  !	     
  !****************************************************************      
  !								*
  !************************************************************************
  ! Fortran 95 OPA Nesting tools						*
  !									*
  !     Copyright (C) 2005 Florian Lemarié (Florian.Lemarie@imag.fr)	*
  !									*
  !************************************************************************
  !
  ! PROGRAM CREATE_COORDINATE 					*
  !								*
  ! program to create coordinates file for child grid		*
  ! (child grid defined by imin,imax,jmin,jmax,rho)		*
  !								*
  !****************************************************************
  !
  ! variables declaration
  !
  REAL*8 :: rpi,ra,rad
  CHARACTER*100 :: Child_filename 
  INTEGER :: i
  TYPE(Coordinates) :: G0,G1
  INTEGER :: narg,iargc
  CHARACTER(len=80) :: namelistname

  narg = iargc()

  IF (narg == 0) THEN
     namelistname = 'namelist.input'
  ELSE
     CALL getarg(1,namelistname)
  ENDIF
  !
  ! read input file (namelist.input)
  !
  CALL read_namelist(namelistname)
  !

  !      
  ! read parent coodinates file
  !
  status = Read_Coordinates(TRIM(parent_coordinate_file),G0)
  ! 
  ! define name of child coordinate file
  !     
  CALL set_child_name(parent_coordinate_file,Child_filename) 
  !
  IF( imax > SIZE(G0%glamt,1) .OR. jmax > SIZE(G0%glamt,2) .OR. imax <= imin .OR. jmax <= jmin ) THEN                    
     WRITE(*,*) 'ERROR ***** bad child grid definition ...'
     WRITE(*,*) 'please check imin,jmin,imax,jmax,jpizoom,jpjzoom values'       
     WRITE(*,*) ' '
     STOP
  ENDIF
  !                      
  WRITE(*,*) ' '
  WRITE(*,*)'Size of the High resolution grid: ',nxfin,' x ',nyfin
  WRITE(*,*) ' '
  !
  ! allocation of child grid elements
  !
  CALL agrif_grid_allocate(G1,nxfin,nyfin)
  !
  !
  !  check potential longitude problems 
  !      
  IF( G0%glamt(imin,jmin) > G0%glamt(imax,jmax) ) THEN           
     WRITE(*,*) '    '
     WHERE ( G0%glamt < 0 )
        G0%glamt = G0%glamt + 360.
     END WHERE
     WHERE ( G0%glamf < 0 )
        G0%glamf = G0%glamf + 360.
     END WHERE
  ENDIF

  !      
  ! interpolation from parent grid to child grid for
  ! T points (cell center)
  ! F points (lower left corner)
  ! U points (cell left side)
  ! V points (cell top side)
  !    glam = longitude
  !    gphi = latitude
  !

  !      
  CALL agrif_interp(G0%glamt,G1%glamt,'T')            
  CALL agrif_interp(G0%glamf,G1%glamf,'F')       
  G1%glamu = G1%glamf
  G1%glamv = G1%glamt    
  !
  CALL agrif_interp(G0%gphit,G1%gphit,'T')
  CALL agrif_interp(G0%gphif,G1%gphif,'F')
  G1%gphiu = G1%gphit
  G1%gphiv = G1%gphif
  !
  !
  rpi = 4.*ATAN(1.)
  rad = rpi/180.
  ra  = 6371229.
  !
  ! Compute scale factors e1 e2
  !
  DO j=1,nyfin
     DO i=2,nxfin
        G1%e1t(i,j) = ra * rad * SQRT( (COS(rad*G1%gphit(i,j))*(G1%glamu(i,j)-G1%glamu(i-1,j)))**2 &
             + (G1%gphiu(i,j)-G1%gphiu(i-1,j))**2)
        G1%e1v(i,j) = ra * rad * SQRT( (COS(rad*G1%gphiv(i,j))*(G1%glamf(i,j)-G1%glamf(i-1,j)))**2 &
             + (G1%gphif(i,j)-G1%gphif(i-1,j))**2)                                                             
     END DO
  END DO
  !      
  G1%e1t(1,:)=G1%e1t(2,:)
  G1%e1v(1,:)=G1%e1v(2,:)
  !
  DO j=1,nyfin
     DO i=1,nxfin-1
        G1%e1u(i,j) = ra * rad * SQRT( (COS(rad*G1%gphiu(i,j))*(G1%glamt(i+1,j)-G1%glamt(i,j)))**2 &
             + (G1%gphit(i+1,j)-G1%gphit(i,j))**2)
        G1%e1f(i,j) = ra * rad * SQRT( (COS(rad*G1%gphif(i,j))*(G1%glamv(i+1,j)-G1%glamv(i,j)))**2 &
             + (G1%gphiv(i+1,j)-G1%gphiv(i,j))**2)                                                            
     END DO
  END DO
  !
  G1%e1u(nxfin,:)=G1%e1u(nxfin-1,:)
  G1%e1f(nxfin,:)=G1%e1f(nxfin-1,:)
  !
  DO j=2,nyfin
     DO i=1,nxfin                                     
        G1%e2t(i,j) = ra * rad * SQRT( (COS(rad*G1%gphit(i,j))*(G1%glamv(i,j)-G1%glamv(i,j-1)))**2 &
             + (G1%gphiv(i,j)-G1%gphiv(i,j-1))**2)
        G1%e2u(i,j) = ra * rad * SQRT( (COS(rad*G1%gphiu(i,j))*(G1%glamf(i,j)-G1%glamf(i,j-1)))**2 &
             + (G1%gphif(i,j)-G1%gphif(i,j-1))**2)                                                               
     END DO
  END DO
  !
  G1%e2t(:,1)=G1%e2t(:,2)
  G1%e2u(:,1)=G1%e2u(:,2)
  !
  DO j=1,nyfin-1
     DO i=1,nxfin
        G1%e2v(i,j) = ra * rad * SQRT( (COS(rad*G1%gphiv(i,j))*(G1%glamt(i,j+1)-G1%glamt(i,j)))**2 &
             + (G1%gphit(i,j+1)-G1%gphit(i,j))**2)
        G1%e2f(i,j) = ra * rad * SQRT( (COS(rad*G1%gphif(i,j))*(G1%glamu(i,j+1)-G1%glamu(i,j)))**2 &
             + (G1%gphiu(i,j+1)-G1%gphiu(i,j))**2)
     END DO
  END DO
  !
  G1%e2v(:,nyfin)=G1%e2v(:,nyfin-1)
  G1%e2f(:,nyfin)=G1%e2f(:,nyfin-1)


  CALL agrif_interp(G0%e1t,G1%e1t,'T')
  G1%e1t = G1%e1t / REAL(irafx)
  CALL agrif_interp(G0%e2t,G1%e2t,'T')
  G1%e2t = G1%e2t / REAL(irafy)

  CALL agrif_interp(G0%e1u,G1%e1u,'U')
  G1%e1u = G1%e1u / REAL(irafx)
  CALL agrif_interp(G0%e2u,G1%e2u,'U')
  G1%e2u = G1%e2u / REAL(irafy)  

  CALL agrif_interp(G0%e1v,G1%e1v,'V')
  G1%e1v = G1%e1v / REAL(irafx)
  CALL agrif_interp(G0%e2v,G1%e2v,'V')
  G1%e2v = G1%e2v / REAL(irafy) 

  CALL agrif_interp(G0%e1f,G1%e1f,'F')
  G1%e1f = G1%e1f / REAL(irafx)
  CALL agrif_interp(G0%e2f,G1%e2f,'F')
  G1%e2f = G1%e2f / REAL(irafy)               

  !
  WHERE ( G1%glamt > 180 )
     G1%glamt = G1%glamt - 360.
  END WHERE
  WHERE ( G1%glamf > 180 )
     G1%glamf = G1%glamf - 360.
  END WHERE
  WHERE ( G1%glamu > 180 )
     G1%glamu = G1%glamu - 360.
  END WHERE
  WHERE ( G1%glamv > 180 )
     G1%glamv = G1%glamv - 360.
  END WHERE
  !
  !
  G1%nav_lon=G1%glamt
  G1%nav_lat=G1%gphit 
  !              
  !
  ! Write interpolation result in child coodinates file
  !      
  status = Write_Coordinates(TRIM(Child_filename),G1)

  !
  WRITE(*,*) 'Child domain position : '
  WRITE(*,*) 'latmin =',G1%gphit(3,3)
  WRITE(*,*) 'latmax =',G1%gphit(nxfin-2,nyfin-2)
  WRITE(*,*) 'lonmin =',G1%glamt(3,3)
  WRITE(*,*) 'lonmax =',G1%glamt(nxfin-2,nyfin-2)
  STOP
END PROGRAM create_coordinate



























