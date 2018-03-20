!************************************************************************
! Fortran 95 OPA Nesting tools						*
!									*
!     Copyright (C) 2005 Florian Lemarié (Florian.Lemarie@imag.fr)	*
!									*
!************************************************************************
!
MODULE agrif_interpolation
  !
  USE agrif_types
  USE io_netcdf
  USE bicubic_interp
  USE bilinear_interp
  USE agrif_extrapolation
  USE agrif_readwrite 
  !
  !
  !************************************************************************
  ! 									*
  ! MODULE  AGRIF_INTERPOLATION						*
  !									*
  ! module containing subroutine used for : 				*
  !   - Forcing data interpolation					*
  !   - Parent to Child coordinates interpolation			*
  !									*
  !************************************************************************
  !          
  ! 
CONTAINS
  !  
  !
  !****************************************************************
  !   subroutine agrif_interp					*
  !								*
  ! subroutine to interpolate coordinates		 	*
  !								*
  ! - input :							*
  !     tabin : coarse grid coordinate variable 		*
  !   typevar : position of interpolated variable on cells	*
  !								*
  ! - ouput :							*
  !    tabout : coordinate variable interpolated on fine grid	*
  !								*
  !****************************************************************
  !     
  SUBROUTINE agrif_interp(tabin,tabout,typevar)
    !      
    REAL*8,DIMENSION(:,:) :: tabin,tabout
    CHARACTER(*) :: typevar
    REAL*8 :: cff1
    INTEGER :: ii,jj
    REAL*8,DIMENSION(:,:),ALLOCATABLE :: tabouttemp
    !
    ! 
    !
    CALL agrif_base_interp2(tabin,tabout,imin,jmin,typevar)

    !
  END SUBROUTINE agrif_interp
  !
  !********************************************************
  !   subroutine agrif_base_interp2			*
  !********************************************************
  !      
  SUBROUTINE agrif_base_interp2(tabin,tabout,i_min,j_min,typevar)
    !
    IMPLICIT NONE
    REAL*8,DIMENSION(:,:) :: tabin,tabout
    REAL*8 :: cff1
    INTEGER :: i_min,j_min
    INTEGER :: ii,jj,i,j
    CHARACTER(*) :: typevar   
    REAL*8,DIMENSION(:),ALLOCATABLE :: xc,yc,xf,yf
    REAL*8,DIMENSION(:,:),ALLOCATABLE :: tabtemp
    REAL*8 :: dxc,dyc,dxf,dyf
    REAL*8 :: decalxc,decalyc,decalxf,decalyf

    INTEGER :: ptx,pty
    REAL*8 :: val(4),xval(4)

    INTEGER :: nxc,nyc,nxf,nyf
    REAL :: xmin,ymin,x,y
    INTEGER :: ic,jc,itemp,jtemp

    nxc = SIZE(tabin,1)
    nyc = SIZE(tabin,2)

    nxf = SIZE(tabout,1)
    nyf = SIZE(tabout,2)      
    !

    ALLOCATE(xc(nxc))
    ALLOCATE(yc(nyc))
    ALLOCATE(xf(nxf))
    ALLOCATE(yf(nyf))

    ALLOCATE(tabtemp(nxc,nyf))

    dxc = 1.
    dyc = 1.
    dxf = 1./REAL(irafx)
    dyf = 1./REAL(irafy)

    IF (typevar .EQ. 'F') THEN
       ptx = 2
       pty = 2
       decalxc = 0.
       decalyc = 0.
       decalxf = 0.
       decalyf = 0.
    ELSEIF (typevar .EQ. 'T') THEN
       ptx = 3
       pty = 3
       decalxc = dxc/2.
       decalyc = dyc/2.
       decalxf = dxf/2.
       decalyf = dyf/2.
    ELSEIF (typevar .EQ. 'U') THEN
       ptx = 2
       pty = 3
       decalxc = 0.
       decalyc = dyc/2.
       decalxf = 0.
       decalyf = dyf/2.
    ELSEIF (typevar .EQ. 'V') THEN
       ptx = 3
       pty = 2
       decalxc = dxc/2.
       decalyc = 0.
       decalxf = dxf/2.
       decalyf = 0.
    ENDIF

    DO i=1,nxc
       xc(i) = 0. + (i-ptx) * dxc + decalxc
    ENDDO

    DO j=1,nyc
       yc(j) = 0. + (j-pty) * dyc + decalyc
    ENDDO


    xmin = (i_min - 1) * dxc
    ymin = (j_min - 1) * dyc

    DO i=1,nxf
       xf(i) = xmin + (i-ptx) * dxf + decalxf
    ENDDO

    DO j=1,nyf
       yf(j) = ymin + (j-pty) * dyf + decalyf
    ENDDO


    DO j = 1,nyf
       DO i = 1,nxc
	  x = xc(i)
	  y = yf(j)

	  ic = ptx + NINT((x-0.-decalxc)/dxc)
	  jc = pty + agrif_int((y-0.-decalyc)/dyc)

	  jc = jc - 1

	  CALL polint(yc(jc:jc+3),tabin(ic,jc:jc+3),4,y,tabtemp(i,j))
       ENDDO
    ENDDO

    DO j = 1,nyf
       DO i = 1,nxf
	  x = xf(i)
	  y = yf(j)

	  itemp = ptx + agrif_int((x-0.-decalxc)/dxc)
	  jtemp = pty + NINT((y-ymin-decalyf)/dyf)

	  itemp = itemp - 1

	  val = tabtemp(itemp:itemp+3,jtemp)
	  xval = xc(itemp:itemp+3)
	  CALL polint(xval,val,4,x,tabout(i,j))

       ENDDO
    ENDDO

    DEALLOCATE(xc,yc,xf,yf,tabtemp)


  END SUBROUTINE agrif_base_interp2
  !
  !********************************************************
  !   subroutine polint					*
  !********************************************************
  !       
  SUBROUTINE polint(xin,valin,n,x,val)
    IMPLICIT NONE
    INTEGER n
    REAL*8 xin(n), valin(n)
    REAL*8 x,val

    INTEGER ns,i,m
    REAL *8 dif,dift
    REAL*8 c(n),d(n),ho,hp,w,den,dy

    ns = 1
    dif = ABS(x-xin(1))

    DO i=1,n
       dift = ABS(x-xin(i))
       IF (dift < dif) THEN
	  ns = i
	  dif = dift
       ENDIF
       c(i) = valin(i)
       d(i) = valin(i)
    ENDDO

    val = valin(ns)
    ns = ns - 1

    DO m=1,n-1
       DO i=1,n-m
	  ho = xin(i)-x
	  hp = xin(i+m)-x
	  w=c(i+1)-d(i)
	  den = w/(ho-hp)
          d(i) = hp * den
	  c(i) = ho * den
       ENDDO
       IF (2*ns < (n-m)) THEN
          dy = c(ns+1)
       ELSE
          dy = d(ns)
          ns = ns - 1
       ENDIF
       val = val + dy
    ENDDO
  END SUBROUTINE polint
  !
  !********************************************************
  !   subroutine polcoe					*
  !********************************************************
  !      
  SUBROUTINE polcoe(xin,valin,n,cof)
    IMPLICIT NONE
    INTEGER n
    REAL*8 xin(n),valin(n),cof(n)


    INTEGER i,j,k
    REAL*8 b,ff,phi,s(n)

    s = 0.
    cof = 0.

    s(n)=-xin(1)
    DO i=2,n
       DO j=n+1-i,n-1
          s(j) =s(j) -xin(i)*s(j+1)
       ENDDO
       s(n)=s(n)-xin(i)
    ENDDO

    DO j=1,n
       phi=n
       DO k=n-1,1,-1
          phi = k*s(k+1)+xin(j)*phi
       ENDDO
       ff=valin(j)/phi
       b=1.
       DO k=n,1,-1
	  cof(k)=cof(k)+b*ff
	  b=s(k)+xin(j)*b
       ENDDO
    ENDDO

    RETURN
  END SUBROUTINE polcoe
  !

  !****************************************************************
  !   subroutine Correctforconservation				*
  !								*
  ! Conservation on domain boundaries ( over 2 coarse grid cells)	*
  !								*
  !****************************************************************          
  !
  !
  SUBROUTINE Correctforconservation(tabcoarse,tabfine,e1parent,e2parent,e1,e2,nxfin,nyfin,posvar,i_min,j_min)
    IMPLICIT NONE
    INTEGER :: nxfin,nyfin
    REAL*8,DIMENSION(:,:) :: tabcoarse,tabfine,e1parent,e2parent,e1,e2
    CHARACTER*1  :: posvar
    INTEGER :: i_min,j_min,diff
    INTEGER ji,jj,ipt,jpt,i,j
    INTEGER ind1,ind2,ind3,ind4,ind5,ind6
    REAL*8 cff1,cff2,cff3      
    !
    diff = 0      
    IF ( MOD(irafx,2) .EQ. 0 ) diff = 1
    !       
    ind1 = 2 + CEILING(irafx/2.0) + diff
    ind2 = nxfin-(2-1)-CEILING(irafx/2.0)
    ind3 = 2 + CEILING(irafy/2.0) + diff
    ind4 = nyfin-(2-1)-CEILING(irafy/2.0)
    ind5 = nxfin - 1 - irafx - CEILING(irafx/2.0) 
    ind6 = nyfin - 1 - irafy - CEILING(irafy/2.0) 
    !       
    IF (posvar.EQ.'T') THEN
       !       
       PRINT *,'correction'
       !
       DO ji=ind1,ind1+irafx,irafx
          !       
          ipt=i_min+(3-1)+(ji-ind1)/irafx
          !            
          DO jj=ind3,ind4,irafy
             !            
             jpt=j_min+(3-1)+(jj-ind3)/irafy

             cff1=SUM(e1(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)* &
                  e2(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff))

             cff2=SUM(e1(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)*   &
                  e2(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)*   &
                  tabfine(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff))

             cff3=e1parent(ipt,jpt)*e2parent(ipt,jpt)*tabcoarse(ipt,jpt)                        

             tabfine(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)=  &
                  tabfine(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)+(cff3-cff2)/cff1

          END DO

       ENDDO
       !****      
       DO ji=ind5,ind5+irafx,irafx
          !       
          ipt=i_min+(3-1)+(ji-ind1)/irafx

          DO jj=ind3,ind4,irafy
             jpt=j_min+(3-1)+(jj-ind3)/irafy

             cff1=SUM(e1(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)* &
                  e2(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff))

             cff2=SUM(e1(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)*   &
                  e2(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)*   &
                  tabfine(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff))

             cff3=e1parent(ipt,jpt)*e2parent(ipt,jpt)*tabcoarse(ipt,jpt)                        

             tabfine(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)=  &
                  tabfine(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)+(cff3-cff2)/cff1

          END DO
       ENDDO

       DO jj=ind3,ind3+irafy,irafy
          !       
          jpt=j_min+(3-1)+(jj-ind3)/irafy
          !
          DO ji=ind1,ind2,irafx
             !          
             ipt=i_min+(3-1)+(ji-ind1)/irafx

             cff1=SUM(e1(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)* &
                  e2(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff))

             cff2=SUM(e1(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)*   &
                  e2(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)*   &
                  tabfine(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff))

             cff3=e1parent(ipt,jpt)*e2parent(ipt,jpt)*tabcoarse(ipt,jpt)                        

             tabfine(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)=  &
                  tabfine(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)+(cff3-cff2)/cff1

          END DO
       ENDDO
       !****      
       DO jj=ind6,ind6+irafy,irafy
          !
          jpt=j_min+(3-1)+(jj-ind3)/irafy
          !
          DO ji=ind1,ind2,irafx          
             ipt=i_min+(3-1)+(ji-ind1)/irafx	

             cff1=SUM(e1(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)* &
                  e2(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff))

             cff2=SUM(e1(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)*   &
                  e2(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)*   &
                  tabfine(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff))

             cff3=e1parent(ipt,jpt)*e2parent(ipt,jpt)*tabcoarse(ipt,jpt)                        

             tabfine(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)=  &
                  tabfine(ji-FLOOR(irafx/2.0):ji+FLOOR(irafx/2.0)-diff,jj-FLOOR(irafy/2.0):jj+FLOOR(irafy/2.0)-diff)+(cff3-cff2)/cff1

          END DO
       ENDDO
       !                                     
       !       
    ENDIF
    RETURN
  END SUBROUTINE Correctforconservation

  !
  !****************************************************************
  !   subroutine Interp_Extrap_var				*
  !								*
  ! Interpolation and Extrapolation for temperature and salinity	*
  !								*
  ! -input : 							*
  !       filename : file containing variable to interpolate	*
  !								*
  !****************************************************************          
  !
  SUBROUTINE Interp_Extrap_var(filename, cd_type)      
    !
    IMPLICIT NONE
    !
    ! variables declaration
    !      
    CHARACTER(len=180),INTENT(in) :: filename
    CHARACTER(len=1 ), OPTIONAL, INTENT(in) :: cd_type

    CHARACTER*180 :: interp_type
    CHARACTER*180 :: Child_file,Childcoordinates
    CHARACTER*180 :: varname,childbathy
    CHARACTER*1  :: posvar
    CHARACTER*20,DIMENSION(:),POINTER :: Ncdf_varname
    CHARACTER(len=20),DIMENSION(4) :: dimnames
    !
    REAL*8, POINTER, DIMENSION(:,:) :: lonParent,latParent => NULL()
    REAL*8, POINTER, DIMENSION(:,:) :: lonChild,latChild,latlon_temp => NULL()
    REAL*8, POINTER, DIMENSION(:,:,:,:) :: tabinterp4d,tabvar1,tabvar2,tabvar3 => NULL()
    REAL*8, POINTER, DIMENSION(:,:,:) :: tabinterp3d,tabvar3d => NULL()
    REAL*8, POINTER, DIMENSION(:) :: timedepth_temp,depth => NULL()
    REAL*8,DIMENSION(:,:),POINTER :: matrix => NULL()
    INTEGER,DIMENSION(:),POINTER :: src_add,dst_add  => NULL()
    INTEGER, POINTER, DIMENSION(:) :: tabtemp1DInt => NULL()
    REAL*8, POINTER, DIMENSION(:) :: nav_lev => NULL()
    !      
    LOGICAL, DIMENSION(:,:,:), POINTER :: detected_pts => NULL()  
    REAL*8,DIMENSION(:,:,:),POINTER :: mask => NULL()
    LOGICAL,DIMENSION(:,:),POINTER :: masksrc => NULL()
    LOGICAL :: Interpolation,conservation,Pacifique,Extrapolation,land_level
    !      
    INTEGER :: deptht,time,i,status,ncid,t,ii,j,nb,numlon,numlat

    !        
    TYPE(Coordinates) :: G0,G1      
    !
    !*****************
    !If coarse grid is masked possibility to activate an extrapolation process
    !
    Extrapolation = .FALSE.
    PRINT*,'DEBUT INTERP_EXTRAP_VAR'
    !
    !*****************
    !
    ! check grid position
    !
    IF( PRESENT(cd_type) ) THEN
       posvar = cd_type
    ELSE
       posvar = 'T'
    ENDIF     

    !
    ! read dimensions in netcdf file
    !
    CALL Read_Ncdf_dim('x',filename,numlon)
    CALL Read_Ncdf_dim('y',filename,numlat)
    CALL Read_Ncdf_dim('time_counter',filename,time)
    IF ( Dims_Existence( 'deptht' , filename ) ) THEN
       CALL Read_Ncdf_dim('deptht',filename,deptht)
    ELSE IF ( Dims_Existence( 'depthu' , filename ) ) THEN
       CALL Read_Ncdf_dim('depthu',filename,deptht)
    ELSE IF ( Dims_Existence( 'depthv' , filename ) ) THEN
       CALL Read_Ncdf_dim('depthv',filename,deptht)
    ELSE IF ( Dims_Existence( 'depthw' , filename ) ) THEN
       CALL Read_Ncdf_dim('depthw',filename,deptht)
    ELSE IF ( Dims_Existence( 'z' , filename ) ) THEN
       CALL Read_Ncdf_dim('z',filename,deptht)
    ELSE
       deptht = N
    ENDIF

    !
    ! retrieve netcdf variable name
    !
    CALL Read_Ncdf_VarName(filename,Ncdf_varname)
    !
    ! define name of child grid file
    !
    CALL set_child_name(filename,Child_file) 
    CALL set_child_name(parent_coordinate_file,Childcoordinates)
    CALL set_child_name(parent_meshmask_file,childbathy)
    WRITE(*,*) 'Child grid file name = ',TRIM(Child_file)
    !
    ! create this file
    !
    status = nf90_create(Child_file,NF90_WRITE,ncid)
    status = nf90_close(ncid)
    !
    ! read coordinates of both domains
    !           
    status = Read_Coordinates(parent_coordinate_file,G0)
    status = Read_Coordinates(Childcoordinates,G1,Pacifique)
    !
    ! check consistency of informations read in namelist 
    !      
    IF( imax > SIZE(G0%glamt,1) .OR. jmax > SIZE(G0%glamt,2) .OR. &
         imax <= imin .OR. jmax <= jmin ) THEN                    
       WRITE(*,*) 'ERROR ***** bad child grid definition ...'
       WRITE(*,*) 'please check imin,jmin,imax,jmax,jpizoom,jpjzoom values'       
       WRITE(*,*) ' '
       STOP
    ENDIF
    !
    IF( SIZE(G1%nav_lon,1) .NE. nxfin .OR. SIZE(G1%nav_lon,2) .NE. nyfin ) THEN
       WRITE(*,*) 'ERROR ***** bad child coordinates or bathy file ...'
       WRITE(*,*) ' '
       WRITE(*,*) 'please check that child coordinates file and child bathymetry file'
       WRITE(*,*) 'has been created with the current namelist '
       WRITE(*,*) ' '
       STOP
    ENDIF
    !      

    !
    ! Initialization of T-mask thanks to bathymetry 
    !
    IF( Extrapolation ) THEN

       WRITE(*,*) 'mask initialisation on coarse and fine grids'
       ! 
       ALLOCATE(mask(numlon,numlat,N))
       CALL Init_mask(childbathy,G1,1,1)
       CALL Init_mask(parent_meshmask_file,G0,1,1)
       !
    ENDIF

    !      
    ! select coordinates to use according to variable position
    !
    ALLOCATE(lonParent(numlon,numlat),latParent(numlon,numlat))
    ALLOCATE(lonChild(nxfin,nyfin),latChild(nxfin,nyfin))

    SELECT CASE(posvar)
    CASE('T')
       lonParent = G0%glamt
       latParent = G0%gphit
       lonChild  = G1%glamt
       latChild  = G1%gphit
       mask      = G1%tmask
    CASE('U')
       lonParent = G0%glamu
       latParent = G0%gphiu
       lonChild  = G1%glamu
       latChild  = G1%gphiu
       mask      = G1%umask
    CASE('V')
       lonParent = G0%glamv
       latParent = G0%gphiv
       lonChild  = G1%glamv
       latChild  = G1%gphiv
       mask      = G1%vmask
    END SELECT

    DEALLOCATE(G0%glamu,G0%glamv,G0%gphiu,G0%gphiv)
    DEALLOCATE(G1%glamu,G1%glamv,G1%gphiu,G1%gphiv) 
    DEALLOCATE(G1%glamt,G1%gphit,G0%glamt,G0%gphit)

    !
    !longitude modification if child domain covers Pacific ocean area
    ! 
    IF( lonChild(1,1) > lonChild(nxfin,nyfin) ) THEN
       Pacifique = .TRUE.
       WHERE( lonChild < 0 )
          lonChild = lonChild + 360.
       END WHERE
       WHERE( lonParent < 0 )
          lonParent = lonParent + 360.
       END WHERE
    ENDIF

    !    
    !
    ! dimensions initialization      
    !
    CALL Write_Ncdf_dim('x',Child_file,nxfin)
    CALL Write_Ncdf_dim('y',Child_file,nyfin)
    IF ( Dims_Existence( 'deptht' , filename ) ) CALL Write_Ncdf_dim('deptht',Child_file,deptht)
    IF ( Dims_Existence( 'depthu' , filename ) ) CALL Write_Ncdf_dim('depthu',Child_file,deptht)
    IF ( Dims_Existence( 'depthv' , filename ) ) CALL Write_Ncdf_dim('depthv',Child_file,deptht)
    IF ( Dims_Existence( 'depthw' , filename ) ) CALL Write_Ncdf_dim('depthw',Child_file,deptht)
    IF ( Dims_Existence( 'z' , filename ) ) CALL Write_Ncdf_dim('z',Child_file,deptht)
    CALL Write_Ncdf_dim('time_counter',Child_file,0)

    IF( deptht .NE. 1 .AND. deptht .NE. N ) THEN
       WRITE(*,*) '***'
       WRITE(*,*) 'Number of vertical levels doesn t match between namelist'
       WRITE(*,*) 'and forcing file ',TRIM(filename)
       WRITE(*,*) 'Please check the values in namelist file'
       WRITE(*,*) 'N = ',N
       WRITE(*,*) 'deptht = ',deptht
       STOP     
    ENDIF
    !
    !
    DO i = 1,SIZE(Ncdf_varname)
       !      
       !
       ! ******************************LOOP ON VARIABLE NAMES*******************************************
       !
       !      
       SELECT CASE (TRIM(Ncdf_varname(i)))
          !
          !copy nav_lon from child coordinates to output file      
       CASE('nav_lon')   
          ALLOCATE(latlon_temp(nxfin,nyfin))
          CALL Read_Ncdf_var('nav_lon',TRIM(Childcoordinates),latlon_temp) 
          CALL Write_Ncdf_var('nav_lon',(/'x','y'/),Child_file,latlon_temp,'float')
          CALL Copy_Ncdf_att('nav_lon',TRIM(filename),Child_file, &
               MINVAL(latlon_temp),MAXVAL(latlon_temp))
          DEALLOCATE(latlon_temp)
          varname = TRIM(Ncdf_varname(i))
          Interpolation = .FALSE.
          !	     
          !copy nav_lat from child coordinates to output file
       CASE('nav_lat')             
          ALLOCATE(latlon_temp(nxfin,nyfin))
          CALL Read_Ncdf_var('nav_lat',TRIM(Childcoordinates),latlon_temp) 
          CALL Write_Ncdf_var('nav_lat',(/'x','y'/),Child_file,latlon_temp,'float')
          CALL Copy_Ncdf_att('nav_lat',TRIM(filename),Child_file, &
               MINVAL(latlon_temp),MAXVAL(latlon_temp)) 
          DEALLOCATE(latlon_temp)
          varname = TRIM(Ncdf_varname(i))
          Interpolation = .FALSE.
          !
          !copy nav_lev from restart_file to output file
          !
       CASE('nav_lev')

          WRITE(*,*) 'copy nav_lev'
          CALL Read_Ncdf_var('nav_lev',filename,nav_lev) 
          IF(.NOT. dimg ) THEN
             CALL Write_Ncdf_var('nav_lev','z',Child_file,nav_lev,'float')
             CALL Copy_Ncdf_att('nav_lev',filename,Child_file)      
          ENDIF
          DEALLOCATE(nav_lev)
          Interpolation = .FALSE.
          !	    	
          !copy time_counter from input file to output file
       CASE('time_counter')
          ALLOCATE(timedepth_temp(time))
          CALL Read_Ncdf_var('time_counter',filename,timedepth_temp) 
          CALL Write_Ncdf_var('time_counter','time_counter',  &
               Child_file,timedepth_temp,'float')
          CALL Copy_Ncdf_att('time_counter',TRIM(filename),Child_file)
          DEALLOCATE(timedepth_temp)
          varname = TRIM(Ncdf_varname(i))
          Interpolation = .FALSE.
          !
          !copy deptht from input file to output file
       CASE('deptht')
          ALLOCATE(depth(deptht))
          CALL Read_Ncdf_var('deptht',filename,depth) 
          CALL Write_Ncdf_var('deptht','deptht',Child_file,depth,'float')
          CALL Copy_Ncdf_att('deptht',TRIM(filename),Child_file)
          varname = TRIM(Ncdf_varname(i))
          Interpolation = .FALSE.     
          !
          !copy depthu from input file to output file
       CASE('depthu')
          ALLOCATE(depth(deptht))
          CALL Read_Ncdf_var('depthu',filename,depth) 
          CALL Write_Ncdf_var('depthu','depthu',Child_file,depth,'float')
          CALL Copy_Ncdf_att('depthu',TRIM(filename),Child_file)
          varname = TRIM(Ncdf_varname(i))
          Interpolation = .FALSE.     
          !
          !copy depthv from input file to output file
       CASE('depthv')
          ALLOCATE(depth(deptht))
          CALL Read_Ncdf_var('depthv',filename,depth) 
          CALL Write_Ncdf_var('depthv','depthv',Child_file,depth,'float')
          CALL Copy_Ncdf_att('depthv',TRIM(filename),Child_file)
          varname = TRIM(Ncdf_varname(i))
          Interpolation = .FALSE.     
          !
          !copy depthv from input file to output file
       CASE('depthw')
          ALLOCATE(depth(deptht))
          CALL Read_Ncdf_var('depthw',filename,depth) 
          CALL Write_Ncdf_var('depthw','depthw',Child_file,depth,'float')
          CALL Copy_Ncdf_att('depthw',TRIM(filename),Child_file)
          varname = TRIM(Ncdf_varname(i))
          Interpolation = .FALSE.     
          !
          !copy time_steps from input file in case of restget use in NEMO in/out routines
       CASE('time_steps')
          !         print *,'avant time step'

          CALL Read_Ncdf_var('time_steps',filename,tabtemp1DInt) 
          !		   print *,'timedeph = ',tabtemp1DInt
          CALL Write_Ncdf_var('time_steps','time_counter',Child_file,tabtemp1DInt)
          CALL Copy_Ncdf_att('time_steps',filename,Child_file) 
          DEALLOCATE(tabtemp1DInt)
          Interpolation = .FALSE.  
          !  
          !store tmask in output file
       CASE('tmask')
          dimnames(1)='x'
          dimnames(2)='y'
          dimnames(3)='deptht' 
          IF (.NOT.ASSOCIATED(G1%tmask)) THEN
             ALLOCATE(G1%tmask(nxfin,nyfin,deptht))
             G1%tmask = 1
          ENDIF
          CALL Write_Ncdf_var('tmask',dimnames(1:3),Child_file,G1%tmask,'float')
          varname = TRIM(Ncdf_varname(i))
          Interpolation = .FALSE.     
          !      
       CASE default
          varname = Ncdf_varname(i)
          conservation = .FALSE.
          CALL get_interptype( varname,interp_type,conservation )
          WRITE(*,*) '**********************************************'
          WRITE(*,*) 'varname      = ',TRIM(varname), 'at ', posvar, ' point'
          WRITE(*,*) 'interp_type  = ',TRIM(interp_type)
          WRITE(*,*) 'conservation = ',conservation
          WRITE(*,*) '***********************************************'
          Interpolation = .TRUE.
          !	         
       END SELECT

       ! //////////////// INTERPOLATION FOR 3D VARIABLES /////////////////////////////////////
       ! 
       IF( Interpolation .AND. Get_NbDims(TRIM(varname),TRIM(filename)) == 3 ) THEN
          !      
          ALLOCATE(detected_pts(numlon,numlat,N))
          ALLOCATE(masksrc(numlon,numlat))                                           
          !
          ! ******************************LOOP ON TIME*******************************************
          !loop on time
          DO t = 1,time
             !                   
             IF(extrapolation) THEN
                WRITE(*,*) 'interpolation/extrapolation ',TRIM(varname),' for time t = ',t
             ELSE
                WRITE(*,*) 'interpolation ',TRIM(varname),' for time t = ',t
             ENDIF
             !                            
             ALLOCATE(tabvar3d(numlon,numlat,1))      
             ALLOCATE(tabinterp3d(nxfin,nyfin,1))
             !                    
             CALL Read_Ncdf_var(varname,filename,tabvar3d,t)                                              
             !
             ! search points where extrapolation is required
             ! 
             IF(Extrapolation) THEN
                 WHERE( tabvar3d .GE. 1.e+20 ) tabvar3d = 0.
                IF (t .EQ. 1. ) CALL extrap_detect(G0,G1,detected_pts(:,:,1),1)
                CALL correct_field_2d(detected_pts(:,:,1),tabvar3d,G0,masksrc,'posvar')
             ELSE
                masksrc = .TRUE.
             ENDIF

             IF (t.EQ.1 ) THEN 

                SELECT CASE(TRIM(interp_type))
                CASE('bilinear')
                   CALL get_remap_matrix(latParent,latChild,   &
                        lonParent,lonChild,                    &
                        masksrc,matrix,src_add,dst_add)

                CASE('bicubic')
                   CALL get_remap_bicub(latParent,latChild,   &
                        lonParent,lonChild,   &
                        masksrc,matrix,src_add,dst_add)

                END SELECT
                !                                                        
             ENDIF
             !      
             SELECT CASE(TRIM(interp_type))
             CASE('bilinear')                                                       
                CALL make_remap(tabvar3d(:,:,1),tabinterp3d(:,:,1),nxfin,nyfin, &
                     matrix,src_add,dst_add)     
             CASE('bicubic')                                   
                CALL make_bicubic_remap(tabvar3d(:,:,1),masksrc,tabinterp3d(:,:,1),nxfin,nyfin, &
                     matrix,src_add,dst_add)                        
             END SELECT
             !                     
             IF( conservation ) CALL Correctforconservation(tabvar3d(:,:,1),tabinterp3d(:,:,1), &
                  G0%e1t,G0%e2t,G1%e1t,G1%e2t,nxfin,nyfin,posvar,imin,jmin)  
             !      
             IF(Extrapolation) tabinterp3d(:,:,1) = tabinterp3d(:,:,1) * mask(:,:,1)     
             !                     
             dimnames(1)='x'
             dimnames(2)='y'
             dimnames(3)='time_counter'
             !                                     
             CALL Write_Ncdf_var(TRIM(varname),dimnames(1:3),&
                  Child_file,tabinterp3d,t,'float')
             !      
             DEALLOCATE(tabinterp3d)
             DEALLOCATE(tabvar3d)                     
             !end loop on time
          END DO
          !                   
          DEALLOCATE(detected_pts)
          IF(ASSOCIATED(matrix)) DEALLOCATE(matrix,dst_add,src_add)
          DEALLOCATE( masksrc)

          CALL Copy_Ncdf_att(TRIM(varname),TRIM(filename),Child_file)                        
          !
       ELSE IF( Interpolation .AND. Get_NbDims(TRIM(varname),TRIM(filename)) == 4 ) THEN
          !
          !
          ! //////////////// INTERPOLATION FOR 4D VARIABLES /////////////////////////////////////
          ! 
          dimnames(1)='x'
          dimnames(2)='y'
          IF ( Dims_Existence( 'deptht' , filename ) ) dimnames(3)='deptht'  
          IF ( Dims_Existence( 'depthu' , filename ) ) dimnames(3)='depthu'
          IF ( Dims_Existence( 'depthv' , filename ) ) dimnames(3)='depthv'
          IF ( Dims_Existence( 'depthw' , filename ) ) dimnames(3)='depthw'
          IF ( Dims_Existence( 'z' , filename ) ) dimnames(3)='z'
          dimnames(4)='time_counter'  
          !
          ! loop on vertical levels
          ! 
          DO nb = 1,deptht
             !        
             ALLOCATE(masksrc(numlon,numlat))
             ALLOCATE(detected_pts(numlon,numlat,N))          
             !
             ! point detection et level n                                     
             !
             land_level = .FALSE.
             IF( Extrapolation ) THEN
                IF(MAXVAL(mask(:,:,nb))==0.) land_level = .TRUE.
             ENDIF


             IF ( land_level ) THEN
                !
                WRITE(*,*) 'only land points on level ',nb
                ALLOCATE(tabinterp4d(nxfin,nyfin,1,1)) 
                tabinterp4d = 0.e0
                !                              
                DO ii = 1,time
                   CALL Write_Ncdf_var(TRIM(varname),dimnames, &
                        Child_file,tabinterp4d,ii,nb,'float')
                END DO
                DEALLOCATE(tabinterp4d)
                !
             ELSE
                !                       
                ! loop on time                       
                !
                DO t = 1,time
                   !                   
                   ALLOCATE(tabvar1(numlon,numlat,1,1))    ! level k
                   IF(Extrapolation) ALLOCATE(tabvar2(numlon,numlat,1,1))    ! level k-1
                   IF(Extrapolation) ALLOCATE(tabvar3(numlon,numlat,1,1))    ! level k-2                       
                   ALLOCATE(tabinterp4d(nxfin,nyfin,1,1)) 
                   !
                   IF(Extrapolation) THEN                                  
                      IF(nb==1) THEN
                         CALL Read_Ncdf_var(varname,filename,tabvar1,t,nb)    
                         WHERE( tabvar1 .GE. 1.e+20 ) tabvar1 = 0.
                      ELSE IF (nb==2) THEN
                         CALL Read_Ncdf_var(varname,filename,tabvar2,t,nb-1)
                         CALL Read_Ncdf_var(varname,filename,tabvar1,t,nb)            
                         WHERE( tabvar1 .GE. 1.e+20 ) tabvar1 = 0.
                         WHERE( tabvar2 .GE. 1.e+20 ) tabvar2 = 0.
                      ELSE 
                         CALL Read_Ncdf_var(varname,filename,tabvar3,t,nb-2)
                         CALL Read_Ncdf_var(varname,filename,tabvar2,t,nb-1)
                         CALL Read_Ncdf_var(varname,filename,tabvar1,t,nb)
                         WHERE( tabvar1 .GE. 1.e+20 ) tabvar1 = 0.
                         WHERE( tabvar2 .GE. 1.e+20 ) tabvar2 = 0.
                         WHERE( tabvar3 .GE. 1.e+20 ) tabvar3 = 0.
                      ENDIF
                      !                              
                      IF (t.EQ.1 ) CALL extrap_detect(G0,G1,detected_pts(:,:,nb),nb)

                      CALL correct_field(detected_pts(:,:,nb),tabvar1,tabvar2,&
                           tabvar3,G0,depth,masksrc,nb,'posvar')                       
                      DEALLOCATE(tabvar2,tabvar3)

                   ELSE 
                      CALL Read_Ncdf_var(varname,filename,tabvar1,t,nb)
                      IF(MAXVAL(tabvar1(:,:,1,1))==0.) land_level = .TRUE.
                      masksrc = .TRUE. 
                   ENDIF

                   IF( Extrapolation ) THEN
                      WRITE(*,*) 'interpolation/extrapolation ',TRIM(varname),' for time t = ',t,'vertical level = ',nb   
                   ELSE
                      WRITE(*,*) 'interpolation ',TRIM(varname),' for time t = ',t,'vertical level = ',nb
                   ENDIF

                   !                          

                   IF (t.EQ.1 ) THEN 

                      SELECT CASE(TRIM(interp_type))
                      CASE('bilinear')
                         CALL get_remap_matrix(latParent,latChild,   &
                              lonParent,lonChild,   &
                              masksrc,matrix,src_add,dst_add)

                      CASE('bicubic')
                         CALL get_remap_bicub(latParent,latChild,   &
                              lonParent,lonChild,   &
                              masksrc,matrix,src_add,dst_add)
                         !                             
                      END SELECT
                      !                                                        
                   ENDIF
                   !      
                   SELECT CASE(TRIM(interp_type))
                      !                             
                   CASE('bilinear')                                                       
                      CALL make_remap(tabvar1(:,:,1,1),tabinterp4d(:,:,1,1),nxfin,nyfin, &
                           matrix,src_add,dst_add)     
                   CASE('bicubic')                                   
                      CALL make_bicubic_remap(tabvar1(:,:,1,1),masksrc,tabinterp4d(:,:,1,1),nxfin,nyfin, &
                           matrix,src_add,dst_add)                        
                   END SELECT
                   !                     
                   IF( conservation ) CALL Correctforconservation(tabvar1(:,:,1,1),tabinterp4d(:,:,1,1), &
                        G0%e1t,G0%e2t,G1%e1t,G1%e2t,nxfin,nyfin,posvar,imin,jmin)  

                   !
                   IF(Extrapolation) CALL check_extrap(G1,tabinterp4d,nb)
                   !      
                   IF(Extrapolation) tabinterp4d(:,:,1,1) = tabinterp4d(:,:,1,1) * mask(:,:,nb)     
                   !                     
                   CALL Write_Ncdf_var(TRIM(varname),dimnames, &
                        Child_file,tabinterp4d,t,nb,'float')
                   !      
                   DEALLOCATE(tabinterp4d)
                   DEALLOCATE(tabvar1)                     
                   !
                   ! end loop on time
                   !

                END DO

             ENDIF

             !
             IF(ASSOCIATED(matrix)) DEALLOCATE(matrix,dst_add,src_add)
             DEALLOCATE( masksrc )
             DEALLOCATE(detected_pts)       
             !
             ! end loop on vertical levels
             !      
          END DO
          !   
          CALL Copy_Ncdf_att(TRIM(varname),TRIM(filename),Child_file)
          !
          ! fin du if interpolation ...
          !
       ENDIF
       ! 
    END DO

    PRINT *,'FIN DE INTERPEXTRAPVAR'
    !    
    IF(Extrapolation) DEALLOCATE(G1%tmask,G0%tmask)
    DEALLOCATE(G0%e1t,G0%e2t,G1%e1t,G1%e2t)
    IF(ASSOCIATED(depth)) DEALLOCATE(depth)   
    !
  END SUBROUTINE Interp_Extrap_var
  !
  !**************************************************************
  !   end subroutine Interp_var
  !**************************************************************          
  ! 
END MODULE agrif_interpolation
