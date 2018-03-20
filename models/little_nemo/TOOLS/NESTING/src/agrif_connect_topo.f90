!************************************************************************
! Fortran 95 OPA Nesting tools						*
!									*
!     Copyright (C) 2005 Florian Lemarié (Florian.Lemarie@imag.fr)	*
!                        Laurent Debreu (Laurent.Debreu@imag.fr)	*
!************************************************************************
!
!Smoothing procedures : Pierrick Penven 2004
!
MODULE agrif_connect_topo
  !
  USE agrif_types
  !
  IMPLICIT NONE           
  !
CONTAINS
  !
  !
  !************************************************************************
  ! 									*
  ! MODULE  CONNECT_TOPO							*
  !									*
  ! module containing subroutine used for : 				*
  !   - Parent-Child bathymetry connection				*
  !   - Bathymetry smoothing						*
  !   - Meters to levels conversion					*
  !   - Parent Bathymetry update 						*
  !									*
  !************************************************************************
  !
  !****************************************************************
  !   subroutine init_constant_bathy				*
  !								*
  !								*
  ! - input :							*
  !     coarse_bathy : coarse grid bathymetry 			*
  ! - ouput :							*
  !    bathy_fin_constant : coarse bathymetry on fine grid	*
  !								*
  !****************************************************************
  !
  SUBROUTINE init_constant_bathy(coarse_bathy,bathy_fin_constant)
    !       
    IMPLICIT NONE 
    !      
    INTEGER :: i,j,ii,jj,ji
    INTEGER :: jpt,ipt,diff,indx,indy,bornex,borney,bornex2,borney2
    INTEGER :: jdeb,ideb,ifin,jfin
    REAL*8, DIMENSION(:,:)  :: coarse_bathy
    REAL*8, DIMENSION(:,:),POINTER :: bathy_fin_constant
    TYPE(Coordinates) :: Grid     
    !
    !
    diff = 0
    IF(MOD(rho,2) .EQ. 0) diff = 1
    !
    indx = 2 + CEILING(irafx/2.0) + diff      
    indy = 2 + CEILING(irafy/2.0) + diff
    bornex = nbghostcellsfine + CEILING(irafx/2.0) + diff - irafx
    borney = nbghostcellsfine + CEILING(irafy/2.0) + diff - irafy
    bornex2 = nxfin - (nbghostcellsfine-1) + irafx - CEILING(irafx/2.0) 
    borney2 = nyfin - (nbghostcellsfine-1) + irafy - CEILING(irafy/2.0) 
    !    
    ALLOCATE(bathy_fin_constant(bornex-FLOOR(irafx/2.0):bornex2+FLOOR(irafx/2.0), &
         borney-FLOOR(irafy/2.0):borney2+FLOOR(irafy/2.0)))
    !
    DO j = borney,borney2,irafy

       jpt = jmin + 3 - 1 + (j-indy)/irafy
       IF(j<=1) jpt = jmin + 1

       DO i = bornex,bornex2,irafx

          ipt = imin + 3 - 1 + (i-indx)/irafx
          IF(i<=1) ipt = imin + 1
          !       
          DO jj = j-FLOOR(irafy/2.0),j+FLOOR(irafy/2.0)-diff
             DO ii = i-FLOOR(irafx/2.0),i+FLOOR(irafx/2.0)-diff

                bathy_fin_constant(ii,jj) = coarse_bathy(ipt,jpt)

             END DO
          END DO

       END DO
    END DO
    !
    !
  END SUBROUTINE init_constant_bathy
  !
  !****************************************************************
  !   subroutine meter_to_levels					*
  !								*
  ! subroutine to convert bathymetry in meters to bathymetry	*
  ! in vertical levels						*
  !								*
  ! - input/output :						*
  !     Grid : grid where conversion is required			*
  !								*
  !various input parameters come from namelist.input files	*
  !****************************************************************
  !
  SUBROUTINE meter_to_levels(Grid)
    !
    IMPLICIT NONE
    !
    REAL*8 :: za2,za1,za0,zsur,zacr,zacr2,zkth,zkth2,zmin,zmax,zw,zt
    TYPE(Coordinates) :: Grid
    INTEGER :: i,j
    INTEGER, DIMENSION(1) :: k
    INTEGER :: k1,ji,jj,jpi,jpj
    REAL*8, POINTER, DIMENSION(:) :: gdepw,gdept,e3w,e3t
    !       
    WRITE(*,*) 'convert bathymetry from etopo to vertical levels'
    !
    jpi = SIZE(Grid%bathy_meter,1)
    jpj = SIZE(Grid%bathy_meter,2)
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
       STOP     
       !       
    ENDIF

    zacr = ppacr
    zkth = ppkth 
    zacr2 = ppacr2
    zkth2 = ppkth2

    !
    ALLOCATE(gdepw(N),gdept(N),e3w(N),e3t(N))
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
    zmax = gdepw(N) + e3t(N)
    zmin = gdepw(4)
    !
    IF ( .NOT. ASSOCIATED(Grid%bathy_level)) &
         ALLOCATE(Grid%bathy_level(jpi,jpj))    
    !
    Grid%bathy_level = N-1
    !
    DO jj = 1, jpj
       DO ji= 1, jpi
          IF( Grid%bathy_meter(ji,jj) <= 0. )   &
               Grid%bathy_level(ji,jj) = INT( Grid%bathy_meter(ji,jj) )
       END DO
    END DO
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
    !
    !
    DO j = 1,jpj
       DO i = 1,jpi
          !
          IF (Grid%bathy_meter(i,j) .EQ. 0.0 ) THEN
             Grid%bathy_level(i,j)=0.
          ELSE
             !	
             k1=4
             DO WHILE (k1 .LT. (N-1))
                IF ((Grid%bathy_meter(i,j).GE.gdepw(k1)) &
                     .AND.(Grid%bathy_meter(i,j).LE.gdepw(k1+1))) EXIT
                k1=k1+1
             END DO
             Grid%bathy_level(i,j)=k1
             !
          ENDIF
          !
       END DO
    END DO
    !
  END SUBROUTINE meter_to_levels
  !
  !!
  !****************************************************************
  !   subroutine levels_to_meter					*
  !								*
  ! subroutine to convert bathymetry in meters to bathymetry	*
  ! in vertical levels						*
  !								*
  ! - input/output :						*
  !     Grid : grid where conversion is required			*
  !								*
  !various input parameters come from namelist.input files	*
  !****************************************************************
  !
  SUBROUTINE levels_to_meter(Grid)
    !
    IMPLICIT NONE
    !
    REAL*8 :: za2,za1,za0,zsur,zacr,zacr2,zkth,zkth2,zmin,zt,zw
    TYPE(Coordinates) :: Grid
    INTEGER :: i,j
    INTEGER, DIMENSION(1) :: k
    INTEGER :: k1,ji,jj,jpi,jpj
    REAL*8, POINTER, DIMENSION(:) :: gdepw,gdept,e3w,e3t
    !       
    WRITE(*,*) 'convert bathymetry in meters for smoothing'
    !
    jpi = SIZE(Grid%bathy_level,1)
    jpj = SIZE(Grid%bathy_level,2)
    !              
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
       STOP      
       !       
    ENDIF
    !
    zacr = ppacr
    zkth = ppkth 
    zacr2 = ppacr2
    zkth2 = ppkth2
    !
    ALLOCATE(gdepw(N),gdept(N),e3w(N),e3t(N))
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
    IF(.NOT. ASSOCIATED(Grid%bathy_meter)) THEN
       ALLOCATE(Grid%bathy_meter(jpi,jpj))  
    ELSE
       IF( ANY(SHAPE(Grid%bathy_meter)/=(/jpi,jpj/)) ) THEN	   
          DEALLOCATE(Grid%bathy_meter)   
          ALLOCATE(Grid%bathy_meter(jpi,jpj))     
       ENDIF
    ENDIF
    !      
    DO jj = 1, jpj
       DO ji= 1, jpi
          !
          Grid%bathy_meter(ji,jj) = gdepw( INT( Grid%bathy_level(ji,jj) ) + 1 )
          !
       END DO
    END DO
    !
  END SUBROUTINE levels_to_meter
  !

  !****************************************************************
  !   subroutine smooth_topo					*
  !								*
  ! subroutine to smooth a given bathymetry (in meters)		*
  !  hanning filter is used (smoothing criterion : rfactor)	*
  !								*
  ! - input/output :						*
  !     h : bathymetry						*
  !								*
  !various input parameters are stored in namelist.input files	*
  !****************************************************************
  !
  SUBROUTINE smooth_topo(h,nbiter)
    !
    IMPLICIT NONE
    ! 
    REAL*8, DIMENSION(:,:) :: h
    REAL*8 :: hmin,cff,nu,r
    REAL*8, DIMENSION(:,:), ALLOCATABLE :: rx,ry,cx,cy,f_x,f_y
    INTEGER :: Mm,Mmm,Lm,Lmm,M,L,nbiter,i,j
    REAL*8,DIMENSION(:,:),ALLOCATABLE :: maskedtopo
    !
    M = SIZE(h,1)
    L = SIZE(h,2)     
    !       
    ALLOCATE(cx(M,L),cy(M,L))
    ALLOCATE(rx(M,L),ry(M,L))
    ALLOCATE(f_x(M,L),f_y(M,L))
    ALLOCATE(maskedtopo(M,L))
    !
    WRITE(*,*) ''
    WRITE(*,*) 'smooth the topography (Hanning filter)'
    WRITE(*,*) 'slope parameter = ',smoothing_factor
    !
    hmin = 1.1
    WHERE(h <= hmin)
       h = hmin
    END WHERE
    !       
    WHERE (h == hmin)
       maskedtopo = 0.
    ELSEWHERE
       maskedtopo = 1.
    END WHERE
    !
    Mm = M-1
    Mmm = Mm - 1
    Lm = L-1
    Lmm = Lm - 1
    cff = 0.8
    nu = 3.0/16.0
    rx=0.
    ry=0.      
    CALL rfact(h,rx,ry,maskedtopo)    
    r = MAX(MAXVAL(rx),MAXVAL(ry))
    h = LOG(h)         
    nbiter = 0
    !       
    DO WHILE (r.GT.smoothing_factor .AND. nbiter < 500  )         
       !       
       nbiter=nbiter+1       
       WHERE(rx > cff*smoothing_factor)
          cx = 1
       ELSEWHERE
          cx = 0
       END WHERE
       CALL hanningx(cx,maskedtopo)      
       WHERE(ry > cff*smoothing_factor)
          cy = 1
       ELSEWHERE
          cy = 0
       END WHERE
       CALL hanningy(cy,maskedtopo)      
       CALL FX(h,f_x,cx,maskedtopo)
       CALL FY(h,f_y,cy,maskedtopo)       
       h(2:Mm,2:Lm) = h(2:Mm,2:Lm) + maskedtopo(2:Mm,2:Lm)*nu *                 &
            ((f_x(2:Mm,3:L)-f_x(2:Mm,2:Lm)) + &     
            (f_y(3:M,2:Lm)-f_y(2:Mm,2:Lm)))       
       CALL rfact(EXP(h),rx,ry,maskedtopo)
       r = MAX(MAXVAL(rx(2:Mm,2:L)),MAXVAL(ry(2:M,2:Lm)))
       !
    END DO
    !       
    WRITE(*,*) 'iterations = ',nbiter
    WRITE(*,*) ''
    h = EXP(h)               
    WHERE( ABS(h-hmin) <= 0.001 )
       h = 0.
    END WHERE
    DEALLOCATE(rx,ry,cx,cy,f_x,f_y,maskedtopo)
    !
  END SUBROUTINE smooth_topo
  !       
  !************************************************************************
  ! subroutine hanning(bathy_meter)
  !************************************************************************
  !
  SUBROUTINE hanning(h,maskedtopo)
    !
    IMPLICIT NONE
    !
    REAL*8, DIMENSION(:,:) :: h,maskedtopo
    !
    INTEGER :: Mm,Mmm,Lm,Lmm,M,L
    !      
    M = SIZE(h,1)
    L = SIZE(h,2)
    Mm = M-1
    Mmm = Mm - 1
    Lm = L-1
    Lmm = Lm - 1
    !      
    h(2:Mm,2:Lm) = maskedtopo(2:Mm,2:Lm)*0.125*(   h(1:Mmm,2:Lm) + &
         h(3:M,2:Lm)   + &
         h(2:Mm,1:Lmm) + &
         h(2:Mm,3:L)   + &
         4*h(2:Mm,2:Lm))+(1.-maskedtopo(2:Mm,2:Lm))*h(2:Mm,2:Lm)
    !
  END SUBROUTINE hanning
  !      
  !************************************************************************
  ! subroutine hanningx(bathy_meter)
  !************************************************************************
  !
  SUBROUTINE hanningx(h,maskedtopo)
    !
    IMPLICIT NONE
    !
    REAL*8, DIMENSION(:,:) :: h,maskedtopo
    REAL*8, DIMENSION(:,:), ALLOCATABLE ::  htemp 
    !
    INTEGER :: Mm,Mmm,Lm,Lmm,M,L
    INTEGER :: i,j
    !      
    M = SIZE(h,1)
    L = SIZE(h,2)
    Mm = M-1
    Mmm = Mm - 1
    Lm = L-1
    Lmm = Lm - 1

    ALLOCATE(htemp(M,L))
    !      
    htemp = h      
    DO j=3,Lm
       DO i=2,Mm
          IF ((maskedtopo(i,j)*maskedtopo(i,j-1)) .NE.0.) THEN
             h(i,j)=0.125*(htemp(i-1,j)+htemp(i+1,j) &
                  +htemp(i,j+1)+htemp(i,j-1)+4.*htemp(i,j))
          ENDIF
       ENDDO
    ENDDO
    j=2
    DO i=2,Mm
       IF ((maskedtopo(i,j)*maskedtopo(i,j-1)) .NE.0.) THEN
          h(i,j)=0.25*(htemp(i+1,j)+htemp(i-1,j)+2.*htemp(i,j))
       ENDIF
    ENDDO
    j=L
    DO i=2,Mm
       IF ((maskedtopo(i,j)*maskedtopo(i,j-1)) .NE.0.) THEN
          h(i,j)=0.25*(htemp(i+1,j)+htemp(i-1,j)+2.*htemp(i,j))
       ENDIF
    ENDDO
    DEALLOCATE(htemp)
    !
  END SUBROUTINE hanningx

  !************************************************************************
  ! subroutine hanning(bathy_meter)
  !************************************************************************
  !
  SUBROUTINE hanningy(h,maskedtopo)
    !
    IMPLICIT NONE
    !
    REAL*8, DIMENSION(:,:) :: h,maskedtopo
    REAL*8, DIMENSION(:,:), ALLOCATABLE ::  htemp 
    !
    INTEGER :: Mm,Mmm,Lm,Lmm,M,L
    INTEGER :: i,j
    !      
    M = SIZE(h,1)
    L = SIZE(h,2)
    Mm = M-1
    Mmm = Mm - 1
    Lm = L-1
    Lmm = Lm - 1      
    ALLOCATE(htemp(M,L))
    !      
    htemp = h

    DO j=2,Lm
       DO i=3,Mm
          IF ((maskedtopo(i,j)*maskedtopo(i-1,j)) .NE.0.) THEN
             h(i,j)=0.125*(htemp(i-1,j)+htemp(i+1,j) &
                  +htemp(i,j+1)+htemp(i,j-1)+4.*htemp(i,j))
          ENDIF
       ENDDO
    ENDDO

    i=2
    DO j=2,Lm
       IF ((maskedtopo(i,j)*maskedtopo(i-1,j)) .NE.0.) THEN
          h(i,j)=0.25*(htemp(i,j+1)+htemp(i,j-1)+2.*htemp(i,j))
       ENDIF
    ENDDO

    i=M
    DO j=2,Lm
       IF ((maskedtopo(i,j)*maskedtopo(i-1,j)) .NE.0.) THEN
          h(i,j)=0.25*(htemp(i,j+1)+htemp(i,j-1)+2.*htemp(i,j))
       ENDIF
    ENDDO

    DEALLOCATE(htemp)
    !
  END SUBROUTINE hanningy

  !      
  !************************************************************************
  ! subroutine FX(bathy_meter,fx)
  !************************************************************************
  !
  SUBROUTINE FX(h,f,c,maskedtopo)
    !
    IMPLICIT NONE
    !
    REAL*8, DIMENSION(:,:)  :: h,c
    REAL*8, DIMENSION(:,:)  :: f,maskedtopo
    REAL*8, DIMENSION(SIZE(h,1),SIZE(h,2))  :: floc
    !
    INTEGER :: Mm,Mmm,Lm,Lmm,M,L,i,j
    !
    f = 0.0     
    M = SIZE(h,1)
    L = SIZE(h,2)
    Mm = M-1
    Mmm = Mm - 1
    Lm = L-1
    Lmm = Lm - 1
    floc = 0.  

    DO j=2,L
       DO i=1,M

          IF ((maskedtopo(i,j)*maskedtopo(i,j-1)).EQ.0.) THEN
             floc(i,j)=0.
          ELSEIF ((i.EQ.1).OR.(i.EQ.M)) THEN     
             floc(i,j)=(7./12.)*(h(i,j)-h(i,j-1))              
          ELSEIF ((maskedtopo(i-1,j)*maskedtopo(i-1,j-1)).EQ.0.) THEN
             floc(i,j)=(7./12.)*(h(i,j)-h(i,j-1))    
          ELSEIF ((maskedtopo(i+1,j)*maskedtopo(i+1,j-1)).EQ.0.) THEN
             floc(i,j)=(7./12.)*(h(i,j)-h(i,j-1))       
          ELSE               
             floc(i,j)=(5./12.)*(h(i,j)-h(i,j-1)) &
                  +(1./12.)*(h(i-1,j)-h(i-1,j-1)+h(i+1,j)-h(i+1,j-1))
          ENDIF
       ENDDO
    ENDDO
    !      
    DO j = 1,L
       DO i = 1,M
          f(i,j) = c(i,j)*floc(i,j) 
       END DO
    END DO
    !           	       
    !
  END SUBROUTINE FX
  !
  !
  !      
  !************************************************************************
  ! subroutine FY(bathy_meter,fy)
  !************************************************************************
  !
  SUBROUTINE FY(h,f,c,maskedtopo)
    !
    IMPLICIT NONE
    !
    REAL*8, DIMENSION(:,:) :: h,c
    REAL*8, DIMENSION(:,:) :: f,maskedtopo
    REAL*8, DIMENSION(SIZE(h,1),SIZE(h,2))  :: floc
    INTEGER :: Mm,Mmm,Lm,Lmm,M,L,i,j
    f=0.0 
    !            
    M = SIZE(h,1)
    L = SIZE(h,2)
    Mm = M-1
    Mmm = Mm - 1
    Lm = L-1
    Lmm = Lm - 1
    !
    floc = 0.

    DO j=1,L
       DO i=2,M
          IF ((maskedtopo(i,j)*maskedtopo(i-1,j)).EQ.0.) THEN
             floc(i,j) = 0.
          ELSEIF ((j.EQ.1).OR.(j.EQ.L)) THEN
             floc(i,j)=(7./12.)*(h(i,j)-h(i-1,j))
          ELSEIF ((maskedtopo(i,j-1)*maskedtopo(i-1,j-1)).EQ.0.) THEN
             floc(i,j)=(7./12.)*(h(i,j)-h(i-1,j))
          ELSEIF ((maskedtopo(i,j+1)*maskedtopo(i-1,j+1)).EQ.0.) THEN
             floc(i,j)=(7./12.)*(h(i,j)-h(i-1,j))
          ELSE      
             floc(i,j)=(5./12.)*(h(i,j)-h(i-1,j)) &
                  +(1./12.)*(h(i,j-1)-h(i-1,j-1)+h(i,j+1)-h(i-1,j+1))
          ENDIF
       ENDDO
    ENDDO
    !
    DO j = 1,L
       DO i = 1,M
          f(i,j) = c(i,j)*floc(i,j) 
       END DO
    END DO
    !      
  END SUBROUTINE FY
  !
  !
  !****************************************************************
  !   subroutine rfact						*
  !								*
  ! subroutine to check if smoothing criterion 			*
  !                     is verified everywhere			*
  !								*
  ! - input :							*
  !     h : bathymetry						*
  ! - ouput :							*
  !    rx,ry : delta(theta)/theta in x and y directions		*
  !****************************************************************
  !
  SUBROUTINE rfact(h,rx,ry,maskedtopo)
    !
    IMPLICIT NONE
    !
    REAL*8, DIMENSION(:,:)  :: h
    REAL*8, DIMENSION(:,:)  :: rx,ry
    REAL*8, DIMENSION(:,:)  :: maskedtopo
    INTEGER M,L,i,j,Mm,Mmm,Lm,Lmm
    !
    M = SIZE(h,1)
    L = SIZE(h,2)
    Mm = M-1
    Mmm = Mm - 1
    Lm = L-1
    Lmm = Lm - 1
    !
    rx=0.
    ry=0.
    !      
    DO j=2,L
       DO i=1,M	     
          rx(i,j) = ABS(h(i,j)-h(i,j-1))/(h(i,j)+h(i,j-1))
          IF ((maskedtopo(i,j)*maskedtopo(i,j-1)) .EQ.0.) THEN
             rx(i,j)=0.
          ENDIF
       ENDDO
    ENDDO
    !
    DO j=1,L
       DO i=2,M
          ry(i,j) = ABS(h(i,j)-h(i-1,j))/(h(i,j)+h(i-1,j))
          IF ((maskedtopo(i,j)*maskedtopo(i-1,j)) .EQ.0.) THEN
             ry(i,j)=0.
          ENDIF
       ENDDO
    ENDDO
    !
  END SUBROUTINE rfact
  !
  !
  !****************************************************************
  !   subroutine Update_Parent_Bathy				*
  !								*
  ! (if desired) subroutine to update parent grid bathymetry	*
  ! for consistency with fine grid bathymetry			*
  !								*
  ! if a given coarse grid point is masked and one of the		*
  ! child grid points contained in this coarse cell is not masked	*
  ! the corresponding coarse grid point is unmasked with gdepw(4) *
  ! value								*
  !								*
  ! - input :							*
  !     G0,G1 : both grids involved				*
  ! - ouput :							*
  !    G0 parent grid containing updated bathymetry		*
  !****************************************************************
  !
  !
  SUBROUTINE Update_Parent_Bathy( G0,G1 )
    !
    IMPLICIT NONE

    TYPE(coordinates) :: G0,G1
    INTEGER :: ji,jj,jk,ipt,jpt,diff,indx,indy,bornex,borney,bornex2,borney2
    !
    INTEGER :: ideb,jdeb,ifin,jfin
    REAL*8 :: za2,za1,za0,zsur,zacr,zacr2,zkth,zkth2,zmin,zt,zw
    INTEGER :: i,j
    INTEGER :: k1
    INTEGER :: compt
    REAL*8, POINTER, DIMENSION(:) :: gdepw,gdept,e3w,e3t
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
       STOP     
       !       
    ENDIF

    zacr = ppacr
    zkth = ppkth 
    zacr2 = ppacr2
    zkth2 = ppkth2

    !
    ALLOCATE(gdepw(N),gdept(N),e3w(N),e3t(N))
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
    zmin = gdepw(4)
    !      
    diff = 0
    IF(MOD(rho,2) .EQ. 0) diff = 1

    ideb = 3 + (irafx - 1)/2
    jdeb = 3 + (irafy - 1)/2

    jfin = nyfin - 2 - (irafy)/2
    ifin = nxfin - 2 - (irafx)/2
    !
    WRITE(*,*) '---------------------------------'
    WRITE(*,*) 'Parent grid bathymetry update ...'
    compt = 0
    !
    DO jj = jdeb,jfin,irafy
       jpt = jmin + 3 - 1 + (jj - jdeb) / irafy
       DO ji = ideb,ifin,irafx
          ipt = imin + 3 - 1 + (ji - ideb) / irafx
          !
          IF( G0%Bathy_meter(ipt,jpt) .LE. 0. .AND.                      &
               MAXVAL(G1%Bathy_meter(ji-irafx/2+diff:ji+irafx/2, &
               jj-irafy/2+diff:jj+irafy/2)).GT. 0.  ) THEN
             G0%Bathy_meter(ipt,jpt) = zmin
             !
             compt = compt + 1
             !
          ENDIF
          !
       ENDDO
    ENDDO
    !
    WRITE(*,*) ' Number of coarse grid points updated = ',compt
    WRITE(*,*) '---------------------------------'
    !      
  END SUBROUTINE Update_Parent_Bathy

END MODULE agrif_connect_topo
