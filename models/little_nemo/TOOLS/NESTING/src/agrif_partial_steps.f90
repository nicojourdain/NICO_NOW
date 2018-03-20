!************************************************************************
! Fortran 95 OPA Nesting tools                                    	*
!                                                      			*
!     Copyright (C) 2005 Florian Lemarié (Florian.Lemarie@imag.fr)      *
!                        Laurent Debreu (Laurent.Debreu@imag.fr)      	*
!************************************************************************
!
MODULE agrif_partial_steps
  !
  USE agrif_types
CONTAINS





  !
  !************************************************************************
  !                                                       		*
  ! MODULE  AGRIF_PARTIAL_STEPS                                    	*
  !									*
  !************************************************************************


  !************************************************************************
  !									*
  ! Subroutine get_partial_steps						*
  !									*
  ! subroutine to compute gdepw_ps on the input grid (based on NEMO code) *
  !                                                      			*
  !************************************************************************
  !       
  SUBROUTINE get_partial_steps(Grid)
    !
    IMPLICIT NONE
    !       
    TYPE(Coordinates) :: Grid                     
    REAL*8 :: za2,za1,za0,zsur,zacr,zacr2,zkth,zkth2,zdepth,zdepwp,zmin,zmax,zdiff,ze3tp,ze3wp,zw,zt
    INTEGER :: i,j,jk,jj,ji,jpj,jpi,ik,ii,ipt,jpt
    INTEGER, DIMENSION(1) :: k
    INTEGER :: k1
    REAL*8, POINTER, DIMENSION(:) :: gdepw,gdept,e3w,e3t
    REAL*8, POINTER, DIMENSION(:,:)   :: hdepw,e3tp,e3wp
    REAL*8, POINTER, DIMENSION(:,:,:) :: gdept_ps,gdepw_ps

    !
    WRITE(*,*) 'convert bathymetry from etopo for partial step z-coordinate case'
    WRITE(*,*) 'minimum thickness of partial step   e3zps_min = ', e3zps_min, ' (m)'
    WRITE(*,*) '       step  level                  e3zps_rat = ', e3zps_rat       
    !       
    jpi = SIZE(Grid%bathy_meter,1)
    jpj = SIZE(Grid%bathy_meter,2)       
    !       
    ALLOCATE(gdepw(N),gdept(N),e3w(N),e3t(N))
    ALLOCATE(gdepw_ps(jpi,jpj,N))                  
    IF (.NOT.ASSOCIATED(Grid%bathy_level)) ALLOCATE(Grid%bathy_level(jpi,jpj))
    !       
    IF ( ( pa0 == 0 .OR. pa1 == 0 .OR. psur == 0 ) &
         .AND. ppdzmin.NE.0 .AND. pphmax.NE.0 ) THEN 
       !    
       WRITE(*,*) 'psur,pa0,pa1 computed'
       za1=( ppdzmin - pphmax / (N-1) )          &
            / ( TANH((1-ppkth)/ppacr) - ppacr/(N-1) &
            *  (  LOG( COSH( (N - ppkth) / ppacr) )      &
            - LOG( COSH( ( 1  - ppkth) / ppacr) )  )  )

       za0  = ppdzmin - za1 * TANH( (1-ppkth) / ppacr )
       zsur = - za0 - za1 * ppacr * LOG( COSH( (1-ppkth) / ppacr )  )
       !
    ELSE IF ( (ppdzmin == 0 .OR. pphmax == 0) .AND. psur.NE.0 .AND. &
         pa0.NE.0 .AND. pa1.NE.0 ) THEN
       !       
       WRITE(*,*) 'psur,pa0,pa1 given by namelist'
       zsur = psur
       za0  = pa0
       za1  = pa1
       za2  = pa2
       !
    ELSE
       !       
       WRITE(*,*) 'ERROR ***** bad vertical grid parameters ...' 
       WRITE(*,*) ' '
       WRITE(*,*) 'please check values of variables'
       WRITE(*,*) 'in namelist vertical_grid section'
       WRITE(*,*) ' '  
       STOP    
       !       
    ENDIF

    zacr = ppacr
    zkth = ppkth       
    zacr2 = ppacr2
    zkth2 = ppkth2       
    !
    IF( .NOT. ldbletanh ) THEN
       DO i = 1,N
          ! 
          zw = REAL( i , 8 )
          zt = REAL( i , 8 ) + 0.5
          gdepw(i) = ( zsur + za0 * zw + za1 * zacr * LOG ( COSH( (zw-zkth) / zacr ) )  )
          gdept(i) = ( zsur + za0 * zt + za1 * zacr * LOG ( COSH( (zt-zkth) / zacr ) )  )
          e3w  (i) =          za0      + za1        * TANH(       (zw-zkth) / zacr   )
          e3t  (i) =          za0      + za1        * TANH(       (zt-zkth) / zacr   )
          !
       END DO
    ELSE
       DO i = 1,N
          zw = REAL( i , 8 )
          zt = REAL( i , 8 ) + 0.5
          ! Double tanh function
          gdepw(i) = ( zsur + za0 * zw + za1 * zacr * LOG ( COSH( (zw-zkth ) / zacr  ) )    &
             &                         + za2 * zacr2* LOG ( COSH( (zw-zkth2) / zacr2 ) )  )
          gdept(i) = ( zsur + za0 * zt + za1 * zacr * LOG ( COSH( (zt-zkth ) / zacr  ) )    &
             &                         + za2 * zacr2* LOG ( COSH( (zt-zkth2) / zacr2 ) )  )
          e3w  (i) =          za0      + za1        * TANH(       (zw-zkth ) / zacr  )    &
             &                         + za2        * TANH(       (zw-zkth2) / zacr2 )
          e3t  (i) =          za0      + za1        * TANH(       (zt-zkth ) / zacr  )    &
             &                         + za2        * TANH(       (zt-zkth2) / zacr2 )
       END DO
    ENDIF
    !

    gdepw(1) = 0.0  
    !
    ! Initialization of constant
    !
    zmax = gdepw(N) + e3t(N)
    zmin = gdepw(4)
    !
    ! Initialize bathy_level to the maximum ocean level available
    !
    Grid%bathy_level = N-1
    !
    ! storage of land and island's number (zera and negative values) in mbathy
    !
    DO jj = 1, jpj
       DO ji= 1, jpi
          IF( Grid%bathy_meter(ji,jj) <= 0. )   &
               Grid%bathy_level(ji,jj) = INT( Grid%bathy_meter(ji,jj) )
       END DO
    END DO
    !
    ! the last ocean level thickness cannot exceed e3t(jpkm1)+e3t(jpk)
    !
    DO jj = 1, jpj
       DO ji= 1, jpi
          IF( Grid%bathy_meter(ji,jj) <= 0. ) THEN
             Grid%bathy_meter(ji,jj) = 0.e0
          ELSE
             Grid%bathy_meter(ji,jj) = MAX( Grid%bathy_meter(ji,jj), zmin )
             Grid%bathy_meter(ji,jj) = MIN( Grid%bathy_meter(ji,jj), zmax )
          ENDIF
       END DO
    END DO
    !
    ! Compute bathy_level for ocean points (i.e. the number of ocean levels)
    ! find the number of ocean levels such that the last level thickness
    ! is larger than the minimum of e3zps_min and e3zps_rat * e3t (where
    ! e3t is the reference level thickness   
    !     
    DO jk = N-1, 1, -1
       zdepth = gdepw(jk) + MIN( e3zps_min, e3t(jk)*e3zps_rat )
       DO jj = 1, jpj
          DO ji = 1, jpi
             IF( 0. < Grid%bathy_meter(ji,jj) .AND. Grid%bathy_meter(ji,jj) <= zdepth ) &
                  Grid%bathy_level(ji,jj) = jk-1
          END DO
       END DO
    END DO


    CALL bathymetry_control(grid%bathy_level)
    !
    ! initialization to the reference z-coordinate
    ! 
    WRITE(*,*) ' initialization to the reference z-coordinate '
    !     
    DO jk = 1, N
       !        Write(*,*) 'k = ',jk
       gdepw_ps(1:jpi,1:jpj,jk) = gdepw(jk)
    END DO
    !      
    Grid%gdepw_ps(:,:) = gdepw_ps(:,:,3)           
    !
    DO jj = 1, jpj
       DO ji = 1, jpi
          ik = Grid%bathy_level(ji,jj)
          ! ocean point only
          IF( ik > 0 ) THEN
             ! max ocean level case
             IF( ik == N-1 ) THEN
                zdepwp = Grid%bathy_meter(ji,jj)
                ze3tp  = Grid%bathy_meter(ji,jj) - gdepw(ik)
                ze3wp = 0.5 * e3w(ik) * ( 1. + ( ze3tp/e3t(ik) ) )
                gdepw_ps(ji,jj,ik+1) = zdepwp
                ! standard case
             ELSE
                !
                IF( Grid%bathy_meter(ji,jj) <= gdepw(ik+1) ) THEN
                   gdepw_ps(ji,jj,ik+1) = Grid%bathy_meter(ji,jj)
                ELSE
                   !
                   gdepw_ps(ji,jj,ik+1) = gdepw(ik+1)
                ENDIF
                !
             ENDIF
             !           
          ENDIF
       END DO
    END DO
    !
    DO jj = 1, jpj
       DO ji = 1, jpi
          ik = Grid%bathy_level(ji,jj)
          ! ocean point only
          IF( ik > 0 ) THEN
             ! bathymetry output
             !
             Grid%gdepw_ps(ji,jj) = gdepw_ps(ji,jj,ik+1)
             !
             !AJOUT-----------------------------------------------------------------------
             !
          ELSE
             !           
             Grid%gdepw_ps(ji,jj) = 0
             !
             !AJOUT------------------------------------------------------------------------
             !           
          ENDIF
          !                     
       END DO
    END DO
    !     
    !
    DEALLOCATE(gdepw,gdept,e3w,e3t)
    DEALLOCATE(gdepw_ps)                  
  END SUBROUTINE get_partial_steps
  !
  !
  !*************************************************************************
  !									*
  ! Subroutine check interp						*
  !									*
  ! subroutine to compute gdepw_ps on the input grid (based on NEMO code) *
  !                                                      			*
  !************************************************************************
  !
  !
  SUBROUTINE check_interp( ParentGrid , gdepwChild )
    !
    IMPLICIT NONE
    !                   
    TYPE(Coordinates) :: ParentGrid
    REAL*8,DIMENSION(:,:) :: gdepwChild 
    INTEGER :: i,j,ji,ij,ii,jj,jpt,ipt
    REAL,DIMENSION(N) :: gdepw,e3t
    REAL :: za0,za1,za2,zsur,zacr,zacr2,zkth,zkth2,zmin,zmax,zdepth,zw,zt
    INTEGER :: kbathy,jk,diff
    INTEGER :: bornex,borney,bornex2,borney2
    !       
    IF ( ( pa0 == 0 .OR. pa1 == 0 .OR. psur == 0 ) &
         .AND. ppdzmin.NE.0 .AND. pphmax.NE.0 ) THEN 
       !    
       za1=( ppdzmin - pphmax / (N-1) )          &
            / ( TANH((1-ppkth)/ppacr) - ppacr/(N-1) &
            *  (  LOG( COSH( (N - ppkth) / ppacr) )      &
            - LOG( COSH( ( 1  - ppkth) / ppacr) )  )  )

       za0  = ppdzmin - za1 * TANH( (1-ppkth) / ppacr )
       zsur = - za0 - za1 * ppacr * LOG( COSH( (1-ppkth) / ppacr )  )
       !
    ELSE IF ( (ppdzmin == 0 .OR. pphmax == 0) .AND. psur.NE.0 .AND. &
         pa0.NE.0 .AND. pa1.NE.0 ) THEN
       !       
       zsur = psur
       za0  = pa0
       za1  = pa1
       za2  = pa2
       !
    ELSE
       !       
       WRITE(*,*) 'ERROR ***** bad vertical grid parameters ...' 
       WRITE(*,*) ' '
       WRITE(*,*) 'please check values of variables'
       WRITE(*,*) 'in namelist vertical_grid section'
       WRITE(*,*) ' '      
       !       
    ENDIF
    !       
    zacr = ppacr
    zkth = ppkth       
    zacr2 = ppacr2
    zkth2 = ppkth2
    !
    IF( .NOT. ldbletanh ) THEN
       DO i = 1,N
          ! 
          zw = REAL( i , 8 )
          zt = REAL( i , 8 ) + 0.5
          gdepw(i) = ( zsur + za0 * zw + za1 * zacr * LOG ( COSH( (zw-zkth) / zacr ) )  )
          e3t  (i) =          za0      + za1        * TANH(       (zt-zkth) / zacr   )
          !
       END DO
    ELSE
       DO i = 1,N
          zw = REAL( i , 8 )
          zt = REAL( i , 8 ) + 0.5
          ! Double tanh function
          gdepw(i) = ( zsur + za0 * zw + za1 * zacr * LOG ( COSH( (zw-zkth ) / zacr  ) )    &
             &                         + za2 * zacr2* LOG ( COSH( (zw-zkth2) / zacr2 ) )  )
          e3t  (i) =          za0      + za1        * TANH(       (zt-zkth ) / zacr  )    &
             &                         + za2        * TANH(       (zt-zkth2) / zacr2 )
       END DO
    ENDIF
    !
    gdepw(1) = 0.0
    !
    !
    diff = 0      
    IF ( MOD(irafx,2) .EQ. 0 ) diff = 1
    !       
    bornex = nbghostcellsfine + CEILING(irafx/2.0) + diff - irafx
    borney = nbghostcellsfine + CEILING(irafy/2.0) + diff - irafy
    bornex2 = nxfin - (nbghostcellsfine-1) - irafx - CEILING(irafx/2.0) 
    borney2 = nyfin - (nbghostcellsfine-1) - irafy - CEILING(irafy/2.0)                      
    !
    !
    ! west boundary
    !

    CALL correct_level( gdepwchild,ParentGrid,gdepw,e3t,1,3+connectionsize*irafx-1, &
         1,nyfin)

    !
    ! east boundary
    !

    CALL correct_level( gdepwchild,ParentGrid,gdepw,e3t,nxfin-2-(connectionsize*irafx-1),nxfin, &
         1,nyfin)

    !
    ! north boundary
    !

    CALL correct_level( gdepwchild,ParentGrid,gdepw,e3t,1,nxfin, &
         nyfin-2-(connectionsize*irafy-1),nyfin )

    !
    ! south boundary
    !
    CALL correct_level( gdepwchild,ParentGrid,gdepw,e3t,1,nxfin, &
         1,3+connectionsize*irafy-1 )

    !       
    !
    !
  END SUBROUTINE check_interp
  !
  SUBROUTINE correct_level( gdepwchild,ParentGrid,gdepw,e3t,minboundx,maxboundx,minboundy,maxboundy )
    !
    IMPLICIT NONE
    TYPE(Coordinates) :: ParentGrid
    REAL*8,DIMENSION(:,:) :: gdepwChild
    REAL*8,DIMENSION(N) :: gdepw,e3t
    INTEGER :: minboundx,maxboundx,minboundy,maxboundy
    INTEGER :: kbathy,jk,indx,indy,diff
    REAL :: xdiff
    INTEGER :: i,j,ji,ij,ii,jj,jpt,ipt
    REAL*8 :: slopex, slopey,val,tmp1,tmp2,tmp3,tmp4
    INTEGER :: parentbathy
    REAL :: mindepth, maxdepth
    REAL :: xmin,ymin,dxfin,dyfin,dsparent
    INTEGER ipbegin,ipend,jpbegin,jpend
    INTEGER ibegin,iend,jbegin,jend
    REAL x,y,zmin,zmax
    INTEGER ptx,pty
    REAL,DIMENSION(:,:),ALLOCATABLE :: gdepwtemp
    INTEGER,DIMENSION(:,:),ALLOCATABLE :: parentbathytab
    !
    !
    ! Initialization of constant
    !
    zmax = gdepw(N) + e3t(N)
    zmin = gdepw(4) 
    !
    ! check that interpolated value stays at the same level         
    ! 
    !
    diff = 0      
    IF ( MOD(irafx,2) .EQ. 0 ) diff = 1

    xdiff = REAL(diff)/2.

    dxfin = 1./irafx
    dyfin = 1./irafy

    ptx = 3
    pty = 3

    xmin = (imin-1) * 1
    ymin = (jmin-1) * 1


    ! compute x and y the locations of the indices minbounx and minboundy 

    x = xmin + (minboundx-ptx)*dxfin  + dxfin/2.
    y = ymin + (minboundy-pty)*dyfin  + dyfin/2.

    ! compute the indices of the nearest coarse grid points      
    ipbegin = ptx + agrif_int((x-0.-1./2.) / 1.) - 1
    jpbegin = pty + agrif_int((y-0.-1./2.) / 1.) - 1

    ! compute indices of the fine grid points nearest to the preceeding coarse grid points       
    ! (inferior values)

    x = (ipbegin - ptx) + 1./2.
    y = (jpbegin - pty) + 1./2.

    ibegin = ptx + agrif_int((x-xmin-dxfin/2.)/dxfin) 
    jbegin = pty + agrif_int((y-ymin-dyfin/2.)/dyfin) 

    ! compute x and y the locations of the indices maxbounx and maxboundy        
    x = xmin + (maxboundx-ptx)*dxfin + dxfin/2.
    y = ymin + (maxboundy-pty)*dyfin + dyfin/2.

    ! compute the indices of the nearest coarse grid points         
    ipend = ptx + CEILING((x-0.-1./2) / 1.) + 1
    jpend = pty + CEILING((y-0.-1./2) / 1.) + 1

    ! compute indices of the fine grid points nearest to the preceeding coarse grid points       
    ! (inferior values)

    x = (ipend - ptx) + 1./2.
    y = (jpend - pty) + 1./2.
    iend = ptx + agrif_int((x-xmin-dxfin/2.)/dxfin) 
    jend = pty + agrif_int((y-ymin-dyfin/2.)/dyfin)               


    ALLOCATE(gdepwtemp(ibegin-irafx:iend+irafx,jbegin-irafy:jend+irafy))
    ALLOCATE(parentbathytab(ibegin-irafx:iend+irafx,jbegin-irafy:jend+irafy))


    jpt=jpbegin
    DO j=jbegin,jend,irafy

       ipt=ipbegin


       DO i=ibegin,iend,irafx


          !            
          parentbathy = ParentGrid%bathy_level(ipt,jpt)
          IF (parentbathy == 0) THEN
             mindepth = 0.
             maxdepth = 0.
          ELSE
             mindepth = MAX(gdepw(parentbathy) + MIN( e3zps_min, e3t(parentbathy)*e3zps_rat ),zmin)
             !                  maxdepth = min(gdepw(parentbathy + 1),zmax)
             IF (parentbathy < (N-1)) THEN
                maxdepth = gdepw(parentbathy + 1)
             ELSE
                maxdepth = HUGE(1.)
             ENDIF
          ENDIF

          slopex = vanleer(parentgrid%gdepw_ps(ipt-1:ipt+1,jpt))/REAL(irafx)


          tmp1 = (maxdepth - parentgrid%gdepw_ps(ipt,jpt)) / REAL(irafx)
          tmp2 = (parentgrid%gdepw_ps(ipt,jpt) - mindepth) / REAL(irafx)

          IF (ABS(slopex) > tmp1) THEN
             IF (slopex > 0) THEN
                slopex = tmp1
             ELSE
                slopex = -tmp1
             ENDIF
          ENDIF

          IF (ABS(slopex) > tmp2) THEN
             IF (slopex > 0) THEN
                slopex = tmp2
             ELSE
                slopex = -tmp2
             ENDIF
          ENDIF
          !                  
          ! interpolation on fine grid points (connection zone)
          !
          DO ii = i-FLOOR(irafx/2.0)+diff,i+FLOOR(irafx/2.0)
             x = ii-i - xdiff/2.
             val = parentgrid%gdepw_ps(ipt,jpt)+slopex * x
             gdepwtemp(ii,j) = val
             IF (gdepwtemp(ii,j) < mindepth) THEN
                gdepwtemp(ii,j) = mindepth
             ENDIF
             IF (gdepwtemp(ii,j) > maxdepth) THEN
                gdepwtemp(ii,j) = maxdepth
             ENDIF
             parentbathytab(ii,j) = parentbathy
          ENDDO
          ipt =ipt + 1
       ENDDO

       jpt = jpt + 1                
    ENDDO

    DO j=jbegin+irafy,jend-irafy,irafy

       DO i=ibegin,iend

          parentbathy = parentbathytab(i,j)
          IF (parentbathy == 0) THEN
             mindepth = 0.
             maxdepth = 0.
          ELSE
             mindepth = MAX(gdepw(parentbathy) + MIN( e3zps_min, e3t(parentbathy)*e3zps_rat ),zmin)
             !                  maxdepth = min(gdepw(parentbathy + 1),zmax)
             IF (parentbathy < (N-1)) THEN
                maxdepth = gdepw(parentbathy + 1)
             ELSE
                maxdepth = HUGE(1.)
             ENDIF
          ENDIF

          slopey = vanleer(gdepwtemp(i,j-irafy:j+irafy:irafy))/REAL(irafy)

          tmp1 = (maxdepth - gdepwtemp(i,j)) / REAL(irafy)
          tmp2 = (gdepwtemp(i,j) - mindepth) / REAL(irafy)

          IF (ABS(slopey) > tmp1) THEN
             IF (slopey > 0) THEN
                slopey = tmp1
             ELSE
                slopey = -tmp1
             ENDIF
          ENDIF
          IF (ABS(slopey) > tmp2) THEN
             IF (slopey > 0) THEN
                slopey = tmp2
             ELSE
                slopey = -tmp2
             ENDIF
          ENDIF


          DO jj = j-FLOOR(irafy/2.0)+diff,j+FLOOR(irafy/2.0)
             y = jj-j - xdiff/2.
             val = gdepwtemp(i,j) + slopey*y
             gdepwtemp(i,jj) = val     
          ENDDO
       ENDDO
    ENDDO


    gdepwchild(minboundx:maxboundx,minboundy:maxboundy) = gdepwtemp(minboundx:maxboundx,minboundy:maxboundy)
    DEALLOCATE(gdepwtemp,parentbathytab)

  END SUBROUTINE correct_level
  !
  !
  !***************************************************
  ! function van leer to compute the corresponding 
  ! Van Leer slopes
  !***************************************************
  !      
  REAL FUNCTION vanleer(tab)
    REAL, DIMENSION(3) :: tab
    REAL res,res1
    REAL p1,p2,p3

    p1=(tab(3)-tab(1))/2.
    p2=(tab(2)-tab(1))
    p3=(tab(3)-tab(2))

    IF ((p1>0.).AND.(p2>0.).AND.(p3>0)) THEN
       res1=MINVAL((/p1,p2,p3/))
    ELSEIF ((p1<0.).AND.(p2<0.).AND.(p3<0)) THEN
       res1=MAXVAL((/p1,p2,p3/))
    ELSE
       res1=0.
    ENDIF

    vanleer = res1   


  END FUNCTION vanleer
  !
  !
  !********************************************************************************
  !   subroutine bathymetry_control                        			*
  !                                                				*
  !  - Purpose :   check the bathymetry in levels                  		*
  !										*
  !  - Method  :   The array mbathy is checked to verified its consistency	*
  !      with the model options. in particular:					*
  !            mbathy must have at least 1 land grid-points (mbathy<=0)		*
  !                  along closed boundary.					*
  !            mbathy must be cyclic IF jperio=1.					*
  !            mbathy must be lower or equal to jpk-1.				*
  !            isolated ocean grid points are suppressed from mbathy		*
  !                  since they are only connected to remaining			*
  !                  ocean through vertical diffusion.				*
  !										*
  !										*
  !********************************************************************************

  SUBROUTINE bathymetry_control(mbathy)

    INTEGER ::   i, j, jl           
    INTEGER ::   icompt, ibtest, ikmax          
    REAL*8, DIMENSION(:,:) :: mbathy     

    ! ================
    ! Bathymetry check
    ! ================

    ! Suppress isolated ocean grid points

    WRITE(*,*)'                   suppress isolated ocean grid points'
    WRITE(*,*)'                   -----------------------------------'

    icompt = 0

    DO jl = 1, 2
       !
       DO j = 2, SIZE(mbathy,2)-1
          DO i = 2, SIZE(mbathy,1)-1

             ibtest = MAX( mbathy(i-1,j), mbathy(i+1,j),mbathy(i,j-1),mbathy(i,j+1) )
             !               
             IF( ibtest < mbathy(i,j) ) THEN
                !                  
                WRITE(*,*) 'grid-point(i,j)= ',i,j,'is changed from',mbathy(i,j),' to ', ibtest
                mbathy(i,j) = ibtest
                icompt = icompt + 1
                !
             ENDIF
             !           
          END DO
       END DO
       !
    END DO
    !      
    IF( icompt == 0 ) THEN
       WRITE(*,*)'     no isolated ocean grid points'
    ELSE
       WRITE(*,*)'    ',icompt,' ocean grid points suppressed'
    ENDIF
    !

    ! Number of ocean level inferior or equal to jpkm1

    ikmax = 0
    DO j = 1, SIZE(mbathy,2)
       DO ji = 1, SIZE(mbathy,1)
          ikmax = MAX( ikmax, NINT(mbathy(i,j)) )
       END DO
    END DO
    !
    IF( ikmax > N-1 ) THEN
       WRITE(*,*) ' maximum number of ocean level = ', ikmax,' >  jpk-1'
       WRITE(*,*) ' change jpk to ',ikmax+1,' to use the exact ead bathymetry'
    ELSE IF( ikmax < N-1 ) THEN
       WRITE(*,*) ' maximum number of ocean level = ', ikmax,' < jpk-1' 
       WRITE(*,*) ' you can decrease jpk to ', ikmax+1
    ENDIF

  END SUBROUTINE bathymetry_control
  !
  !
  !**********************************************************************************
  !
  !subroutine get_scale_factors
  !
  !**********************************************************************************
  !
  SUBROUTINE get_scale_factors(Grid,fse3t,fse3u,fse3v)
    !
    IMPLICIT NONE
    !       
    TYPE(Coordinates) :: Grid 
    REAL*8, DIMENSION(:,:,:) :: fse3u,fse3t,fse3v
    !                                  
    REAL*8 :: za2,za1,za0,zsur,zacr2,zacr,zkth2,zkth,zdepth,zdepwp,zmin,zmax,zdiff,ze3tp,ze3wp,zw,zt
    INTEGER :: i,j,jk,jj,ji,jpj,jpi,ik,ii,ipt,jpt,jpk
    INTEGER, DIMENSION(1) :: k
    INTEGER :: k1
    REAL*8, POINTER, DIMENSION(:) :: gdepw,gdept,e3w,e3t
    REAL*8, POINTER, DIMENSION(:,:)   :: hdepw,e3tp,e3wp
    REAL*8, POINTER, DIMENSION(:,:,:) :: gdept_ps,gdepw_ps   
    !       
    jpi = SIZE(fse3t,1)
    jpj = SIZE(fse3t,2) 
    jpk = SIZE(fse3t,3)      
    !       
    ALLOCATE(gdepw(jpk),e3t(jpk))
    ALLOCATE(gdepw_ps(jpi,jpj,jpk))                  
    !       
    IF ( ( pa0 == 0 .OR. pa1 == 0 .OR. psur == 0 ) &
         .AND. ppdzmin.NE.0 .AND. pphmax.NE.0 ) THEN 
       !    
       WRITE(*,*) 'psur,pa0,pa1 computed'
       za1=( ppdzmin - pphmax / (jpk-1) )          &
            / ( TANH((1-ppkth)/ppacr) - ppacr/(jpk-1) &
            *  (  LOG( COSH( (jpk - ppkth) / ppacr) )      &
            - LOG( COSH( ( 1  - ppkth) / ppacr) )  )  ) 
       !
       za0  = ppdzmin - za1 * TANH( (1-ppkth) / ppacr )
       zsur = - za0 - za1 * ppacr * LOG( COSH( (1-ppkth) / ppacr )  )
       !
    ELSE IF ( (ppdzmin == 0 .OR. pphmax == 0) .AND. psur.NE.0 .AND. &
         pa0.NE.0 .AND. pa1.NE.0 ) THEN
       !       
       zsur = psur
       za0  = pa0
       za1  = pa1   
       za2  = pa2
       !       
    ENDIF

    zacr = ppacr
    zkth = ppkth       
    zacr2 = ppacr2
    zkth2 = ppkth2
    !         
    !                
    IF( .NOT. ldbletanh ) THEN
       DO i = 1,jpk
          ! 
          zw = REAL( i , 8 )
          zt = REAL( i , 8 ) + 0.5
          gdepw(i) = ( zsur + za0 * zw + za1 * zacr * LOG ( COSH( (zw-zkth) / zacr ) )  )
          e3t  (i) =          za0      + za1        * TANH(       (zt-zkth) / zacr   )
          fse3t(:,:,i) = e3t(i)
          gdepw_ps(:,:,i) = gdepw(i)
          !
       END DO
    ELSE
       DO i = 1,jpk
          zw = REAL( i , 8 )
          zt = REAL( i , 8 ) + 0.5
          ! Double tanh function
          gdepw(i) = ( zsur + za0 * zw + za1 * zacr * LOG ( COSH( (zw-zkth ) / zacr  ) )    &
             &                         + za2 * zacr2* LOG ( COSH( (zw-zkth2) / zacr2 ) )  )
          e3t  (i) =          za0      + za1        * TANH(       (zt-zkth ) / zacr  )    &
             &                         + za2        * TANH(       (zt-zkth2) / zacr2 )
          fse3t(:,:,i) = e3t(i)
          gdepw_ps(:,:,i) = gdepw(i)
       END DO
    ENDIF
    !                                 
    gdepw(1) = 0.0 
    gdepw_ps(:,:,1) = 0.0 
    !
    zmax = gdepw(jpk) + e3t(jpk)
    zmin = gdepw(4)
    !
    DO jj = 1, jpj
       DO ji= 1, jpi
          IF( Grid%bathy_meter(ji,jj) <= 0. ) THEN
             Grid%bathy_meter(ji,jj) = 0.e0
          ELSE
             Grid%bathy_meter(ji,jj) = MAX( Grid%bathy_meter(ji,jj), zmin )
             Grid%bathy_meter(ji,jj) = MIN( Grid%bathy_meter(ji,jj), zmax )
          ENDIF
       END DO
    END DO
    !
    DO jj = 1, jpj
       DO ji = 1, jpi
          ik = Grid%bathy_level(ji,jj) 
          IF( ik > 0 ) THEN
             ! max ocean level case
             IF( ik == jpk-1 ) THEN
                zdepwp = Grid%bathy_meter(ji,jj)
                ze3tp  = Grid%bathy_meter(ji,jj) - gdepw(ik)
                fse3t(ji,jj,ik  ) = ze3tp
                fse3t(ji,jj,ik+1) = ze3tp
                gdepw_ps(ji,jj,ik+1) = zdepwp
             ELSE
                IF( Grid%bathy_meter(ji,jj) <= gdepw(ik+1) ) THEN
                   gdepw_ps(ji,jj,ik+1) = Grid%bathy_meter(ji,jj)
                ELSE
                   gdepw_ps(ji,jj,ik+1) = gdepw(ik+1)
                ENDIF
                fse3t(ji,jj,ik) = e3t(ik) * ( gdepw_ps(ji,jj,ik+1) - gdepw(ik))   & 
                     /( gdepw(ik+1) - gdepw(ik)) 
                fse3t(ji,jj,ik+1) = fse3t(ji,jj,ik)

             ENDIF
          ENDIF
       END DO
    END DO
    !
    DO i = 1, jpk
       fse3u (:,:,i)  = e3t(i)
       fse3v (:,:,i)  = e3t(i)
    END DO
    !
    DO jk = 1,jpk
       DO jj = 1, jpj-1
          DO ji = 1, jpi-1
             fse3u (ji,jj,jk) = MIN( fse3t(ji,jj,jk), fse3t(ji+1,jj,jk))
             fse3v (ji,jj,jk) = MIN( fse3t(ji,jj,jk), fse3t(ji,jj+1,jk))      
          ENDDO
       ENDDO
    ENDDO
    !           
    DEALLOCATE(gdepw,e3t)
    DEALLOCATE(gdepw_ps)
    DEALLOCATE(Grid%bathy_meter,Grid%bathy_level) 
    !
  END SUBROUTINE get_scale_factors
  !       
END MODULE agrif_partial_steps


