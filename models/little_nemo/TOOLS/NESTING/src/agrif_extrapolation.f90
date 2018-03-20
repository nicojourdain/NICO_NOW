!************************************************************************
! Fortran 95 OPA Nesting tools						*
!									*
!     Copyright (C) 2005 Florian Lemarié (Florian.Lemarie@imag.fr)	*
!									*
!************************************************************************
!
MODULE agrif_extrapolation
  !
  USE agrif_types
  USE agrif_readwrite 
  USE io_netcdf  
  USE agrif_gridsearch  
  
  IMPLICIT NONE 

CONTAINS
  !
  !************************************************************************
  ! 									*
  ! MODULE  AGRIF_EXTRAPOLATION						*
  !									*
  !************************************************************************   
  !      
  !****************************************************************
  !     subroutine extrap_detect					*
  !								*
  !     detection on each level of points 			*
  !     where extrapolation is required 				*
  !								*
  !****************************************************************          
  !      
  !
  SUBROUTINE extrap_detect(G0,G1,detected,n,posvar) 
    !      
    LOGICAL, DIMENSION(:,:) :: detected
    TYPE(Coordinates) :: G0,G1       
    CHARACTER(*), OPTIONAL :: posvar
    INTEGER :: i,j,k,ic,jc,compt,dst_add,n
    INTEGER, DIMENSION(1) :: i_min,j_min     
    !                                
    IF( PRESENT(posvar) .AND. posvar == 'U' ) THEN      
       CALL get_detected_pts(G0%gphiu,G1%gphiu,G0%glamu,G1%glamu,   &
            G0%umask(:,:,n),G1%umask(:,:,n),detected(:,:))       
    ELSE IF( PRESENT(posvar) .AND. posvar == 'V' ) THEN
       !      
       CALL get_detected_pts(G0%gphiv,G1%gphiv,G0%glamv,G1%glamv,   &
            G0%vmask(:,:,n),G1%vmask(:,:,n),detected(:,:))                                  
    ELSE
       CALL get_detected_pts(G0%nav_lat,G1%nav_lat,G0%nav_lon,G1%nav_lon,   &
            G0%tmask(:,:,n),G1%tmask(:,:,n),detected(:,:))        
    ENDIF
    !      
  END SUBROUTINE extrap_detect
  !      
  !   
  !****************************************************************
  !     end subroutine extrap_detect				*
  !**************************************************************** 
  !      
  !      
  !****************************************************************
  !    subroutine correct_field					*
  ! correct field on detected points				*
  !								*
  !****************************************************************          
  ! 
  SUBROUTINE correct_field(detected_pts,tabin,tabinkm1,tabinkm2,G0,nav_lev,newmask,k,posvar)
    !
    LOGICAL, DIMENSION(:,:) :: detected_pts
    LOGICAL, DIMENSION(:,:) :: newmask
    CHARACTER(*),OPTIONAL :: posvar
    INTEGER :: k
    !
    INTEGER :: i,j,ii,jj,nx,ny,n,lbi,ubi,lbj,ubj,kpos,ipos,jpos,r
    !
    REAL*8, DIMENSION(:,:,:,:) ::  tabin
    REAL*8, DIMENSION(:,:,:,:) ::  tabinkm1
    REAL*8, DIMENSION(:,:,:,:) ::  tabinkm2
    REAL*8, DIMENSION(:) :: nav_lev
    REAL*8, DIMENSION(:,:,:), ALLOCATABLE :: mask
    REAL*8, DIMENSION(:,:), ALLOCATABLE :: lon,lat 
    REAL*8 :: deriv,deriv_min
    LOGICAL :: found
    !      
    TYPE(Coordinates) :: G0  
    !
    ! copy coarse grid mask in newmask 
    !            
    IF ( PRESENT(posvar) .AND. posvar == 'U' ) THEN
       WHERE(G0%umask(:,:,k) == 1. )
          newmask(:,:) = .TRUE.
       ELSEWHERE
          newmask(:,:) = .FALSE.
       END WHERE
       ALLOCATE(mask(SIZE(G0%umask,1),SIZE(G0%umask,2),SIZE(G0%umask,3)))
       ALLOCATE(lat(SIZE(G0%umask,1),SIZE(G0%umask,2)))
       ALLOCATE(lon(SIZE(G0%umask,1),SIZE(G0%umask,2)))
       mask = G0%umask
       lat = G0%gphiu 
       lon = G0%glamu     
    ELSE IF ( PRESENT(posvar) .AND. posvar == 'V' ) THEN
       WHERE(G0%vmask(:,:,k) == 1. )
          newmask(:,:) = .TRUE.
       ELSEWHERE
          newmask(:,:) = .FALSE.
       END WHERE
       ALLOCATE(mask(SIZE(G0%vmask,1),SIZE(G0%vmask,2),SIZE(G0%vmask,3)))
       ALLOCATE(lat(SIZE(G0%vmask,1),SIZE(G0%vmask,2)))
       ALLOCATE(lon(SIZE(G0%vmask,1),SIZE(G0%vmask,2)))
       mask = G0%vmask        
       lat = G0%gphiv 
       lon = G0%glamv
    ELSE      
       WHERE(G0%tmask(:,:,k) == 1. )
          newmask(:,:) = .TRUE.
       ELSEWHERE
          newmask(:,:) = .FALSE.
       END WHERE
       ALLOCATE(mask(SIZE(G0%tmask,1),SIZE(G0%tmask,2),SIZE(G0%tmask,3)))
       ALLOCATE(lat(SIZE(G0%tmask,1),SIZE(G0%tmask,2)))
       ALLOCATE(lon(SIZE(G0%tmask,1),SIZE(G0%tmask,2)))
       mask = G0%tmask
       lon = G0%nav_lon
       lat = G0%nav_lat
    ENDIF
    !
    ! dimensions initialisation
    !
    nx = SIZE(tabin,1)
    ny = SIZE(tabin,2)    
    ! 
    !       
    DO j = 1,ny   
       !
       DO i = 1,nx         
          !
          IF( detected_pts(i,j) ) THEN       
             !
             r = 0
             found = .FALSE.
             deriv_min = 2000000.
             ipos=0
             jpos=0
             !
             DO WHILE(.NOT. found)
                !
                r = r + 1      
                !       
                IF(i-r < 1 ) THEN
                   lbi = 1
                   ubi = MIN(i+r,nx)
                ELSE IF(i+r > nx) THEN
                   lbi = MAX(i-r,1)
                   ubi = nx
                ELSE
                   lbi = i-r
                   ubi = i+r
                ENDIF
                !
                IF(j-r < 1) THEN
                   lbj = 1
                   ubj = MIN(j+r,ny)
                ELSE IF(j+r > ny) THEN
                   lbj = MAX(j-r,1)
                   ubj = ny
                ELSE
                   lbj = j-r
                   ubj = j+r
                ENDIF
                !
                DO jj = lbj,ubj,ubj-lbj
                   DO ii = lbi,ubi,ubi-lbi
                      !
                      deriv = search_pts_h(ii,jj,k,i,j,k,tabin(:,:,1,1),mask,lon,lat)  
                      !
                      IF( ABS(deriv) < deriv_min ) THEN                                                    
                         deriv_min = ABS(deriv)
                         ipos = ii
                         jpos = jj
                         kpos = k
                      ENDIF
                      !                           
                      deriv = search_pts_v(ii,jj,k-1,i,j,k,tabinkm1,tabinkm2,mask,nav_lev,lon,lat)
                      !
                      IF( ABS(deriv) < deriv_min ) THEN                                                    
                         deriv_min = ABS(deriv)
                         ipos = ii
                         jpos = jj
                         kpos = k-1
                      ENDIF
                      !                                       
                   END DO
                END DO
                !
                !                                  
                IF( deriv_min < 2000000.  ) THEN 
                   !                
                   IF(kpos == k)   tabin(i,j,1,1) = tabin(ipos,jpos,1,1)
                   IF(kpos == k-1) tabin(i,j,1,1) = tabinkm1(ipos,jpos,1,1)   
                   found = .TRUE.
                   newmask(i,j) = .TRUE.
                ELSE IF ((lbi == 1).AND.(ubi == nx).AND.(lbj == 1).AND.(ubj == ny)) THEN
                   found = .TRUE.
                   newmask(i,j) = .FALSE.
                   !
                ENDIF
                !
             END DO !do while
             !
          ENDIF
          !
       END DO
       !
    END DO
    !
    DEALLOCATE(mask,lon,lat)
    !
  END SUBROUTINE correct_field
  !      
  !**************************************************************
  !    end subroutine correct_field
  !**************************************************************          
  !  
  SUBROUTINE correct_field_2d(detected_pts,tabin,G0,newmask,posvar)
    !
    LOGICAL, DIMENSION(:,:) :: detected_pts
    LOGICAL, DIMENSION(:,:) :: newmask
    CHARACTER(*), OPTIONAL :: posvar
    LOGICAL :: found
    INTEGER :: k
    !
    INTEGER :: i,j,ii,jj,nx,ny,n,lbi,ubi,lbj,ubj,ipos,jpos,r
    !
    REAL*8, DIMENSION(:,:,:), ALLOCATABLE :: mask
    REAL*8, DIMENSION(:,:), ALLOCATABLE :: lon,lat 
    REAL*8, DIMENSION(:,:,:) ::  tabin
    REAL*8 :: deriv,deriv_min
    !      
    TYPE(Coordinates) :: G0  
    !
    ! copy coarse grid mask in newmask 
    !            
    mask = G0%tmask
    lon = G0%nav_lon
    lat = G0%nav_lat      
    IF ( PRESENT(posvar) .AND. posvar == 'U' ) THEN
       WHERE(G0%umask(:,:,1) == 1. )
          newmask(:,:) = .TRUE.
       ELSEWHERE
          newmask(:,:) = .FALSE.
       END WHERE
       ALLOCATE(mask(SIZE(G0%umask,1),SIZE(G0%umask,2),SIZE(G0%umask,3)))
       ALLOCATE(lat(SIZE(G0%umask,1),SIZE(G0%umask,2)))
       ALLOCATE(lon(SIZE(G0%umask,1),SIZE(G0%umask,2)))
       mask = G0%umask
       lat = G0%gphiu 
       lon = G0%glamu     
    ELSE IF ( PRESENT(posvar) .AND. posvar == 'V' ) THEN
       WHERE(G0%vmask(:,:,1) == 1. )
          newmask(:,:) = .TRUE.
       ELSEWHERE
          newmask(:,:) = .FALSE.
       END WHERE
       ALLOCATE(mask(SIZE(G0%vmask,1),SIZE(G0%vmask,2),SIZE(G0%vmask,3)))
       ALLOCATE(lat(SIZE(G0%vmask,1),SIZE(G0%vmask,2)))
       ALLOCATE(lon(SIZE(G0%vmask,1),SIZE(G0%vmask,2)))
       mask = G0%vmask        
       lat = G0%gphiv 
       lon = G0%glamv
    ELSE      
       WHERE(G0%tmask(:,:,1) == 1. )
          newmask(:,:) = .TRUE.
       ELSEWHERE
          newmask(:,:) = .FALSE.
       END WHERE
       ALLOCATE(mask(SIZE(G0%tmask,1),SIZE(G0%tmask,2),SIZE(G0%tmask,3)))
       ALLOCATE(lat(SIZE(G0%tmask,1),SIZE(G0%tmask,2)))
       ALLOCATE(lon(SIZE(G0%tmask,1),SIZE(G0%tmask,2)))
       mask = G0%tmask
       lon = G0%nav_lon
       lat = G0%nav_lat
    ENDIF

    !
    ! dimensions initialisation
    !
    nx = SIZE(tabin,1)
    ny = SIZE(tabin,2)    
    !       
    DO i = 1,nx         
       !
       DO j = 1,ny   
          !                    
          !                      
          IF( detected_pts(i,j) ) THEN       
             ! 
             r = 0 
             found = .FALSE.
             deriv_min = 2000000.
             ipos=0
             jpos=0
             !
             DO WHILE (.NOT. found )

                !
                r = r + 1 
                !
                IF(i-r < 1 ) THEN
                   lbi = 1
                   ubi = MIN(i+r,nx)
                ELSE IF(i+r > nx) THEN
                   lbi = MAX(i-r,1)
                   ubi = nx
                ELSE
                   lbi = i-r
                   ubi = i+r
                ENDIF
                !
                IF(j-r < 1) THEN
                   lbj = 1
                   ubj = MIN(j+r,ny)
                ELSE IF(j+r > ny) THEN
                   lbj = MAX(j-r,1)
                   ubj = ny
                ELSE
                   lbj = j-r
                   ubj = j+r
                ENDIF
                !                                 
                DO ii = lbi,ubi
                   DO jj = lbj,ubj
                      !
                      deriv = search_pts_h(ii,jj,1,i,j,1,tabin(:,:,1),mask,lon,lat)  
                      !
                      IF( ABS(deriv) < deriv_min ) THEN                                                    
                         deriv_min = ABS(deriv)
                         ipos = ii
                         jpos = jj
                      ENDIF
                      !                                       
                   END DO
                END DO
                !
                !                                    
                IF( deriv_min < 2000000.  ) THEN 
                   !
                   found = .TRUE.                                              
                   tabin(i,j,1) = tabin(ipos,jpos,1) 
                   newmask(i,j) = .TRUE.
                   !
                ENDIF
                !
             END DO !do while
             !
          ENDIF
          !
       END DO
       !
    END DO
    !
    DEALLOCATE(mask,lon,lat)
    !
  END SUBROUTINE correct_field_2d
  !      
  !**************************************************************
  !    function get_dist
  !**************************************************************          
  ! 

  !
  REAL*8 FUNCTION  get_dist(plat1,plon1,plat2,plon2)
    !
    REAL*8 :: plat1,plon1,plat2,plon2
    REAL*8 :: dist,ra,rad,rpi,lat,lon
    !      
    rpi = 3.141592653589793
    rad = rpi/180.
    ra  = 6371229.   
    !      
    lat = plat2-plat1
    lon = plon2-plon1
    !
    dist = ra * rad * SQRT( (COS(rad*(plat1+plat2)/2.)*lon)**2 + lat**2 )          
    get_dist = dist
    RETURN
    !
  END FUNCTION get_dist

  !
  !      
  !**************************************************************
  !    end function get_dist
  !**************************************************************
  !
  !      
  !**************************************************************
  !    subroutine check_extrap
  !**************************************************************          
  ! 

  !
  SUBROUTINE check_extrap(Grid,tabin,k)
    !
    REAL*8, DIMENSION(:,:,:,:) ::  tabin
    TYPE(Coordinates) :: Grid 
    INTEGER :: i,j,k  
    !  
    DO i = 2,SIZE(tabin,1)-1
       DO j=2,SIZE(tabin,2)-1
          !                     
          IF( Grid%tmask(i,j,k) == 1. .AND. tabin(i,j,1,1)==0.) THEN
             !      
             WRITE(*,*) 'no masked point with value zero (',i,',',j,',',k,')'
             !                    
          ENDIF

       END DO
    END DO
    !
  END SUBROUTINE check_extrap

  !
  !      
  !**************************************************************
  !    end subroutine check_extrap
  !**************************************************************  
  !
  !**************************************************************
  !    subroutine search_pts_h
  !************************************************************** 
  !
  REAL*8 FUNCTION search_pts_h(i,j,k,ipt,jpt,kpt,tabvar,mask,lon,lat)
    !
    REAL*8 :: hx,hy,fx,fy
    REAL*8 :: h_x,h_y
    REAL*8, DIMENSION(:,:) :: tabvar
    INTEGER :: i,j,k,ipt,jpt,kpt,nx,ny
    LOGICAL :: foundx,foundy
    REAL*8, DIMENSION(:,:,:) :: mask
    REAL*8, DIMENSION(:,:) :: lon,lat
    !
    !
    foundx = .TRUE.
    foundy = .TRUE.
    !      
    nx = SIZE(tabvar,1)
    ny = SIZE(tabvar,2)
    !
    IF( i==ipt .AND. j==jpt ) THEN    
       search_pts_h = 2000000.
       RETURN
    ENDIF
    !      
    IF( mask(i,j,k) == 0. ) THEN    
       search_pts_h = 2000000.
       RETURN
    ENDIF
    !
    ! x direction
    !
    IF(i+1<=nx .AND. i-1>=1) THEN
       IF(mask(i+1,j,k)==1. .AND. mask(i-1,j,k)==1.) THEN     
          hx = get_dist(lat(i+1,j),lon(i+1,j),&
               lat(i-1,j),lon(i-1,j))
          fx = (tabvar(i+1,j) - tabvar(i-1,j))/hx
       ELSE IF(mask(i+1,j,k)==1. .AND. mask(i-1,j,k)==0. .AND. mask(i,j,k)==1.) THEN
          hx = get_dist(lat(i+1,j),lon(i+1,j),&
               lat(i,j),lon(i,j))
          fx = (tabvar(i+1,j) - tabvar(i,j))/hx
       ELSE IF(mask(i+1,j,k)==0. .AND. mask(i-1,j,k)==1. .AND. mask(i,j,k)==1.) THEN
          hx = get_dist(lat(i,j),lon(i,j),&
               lat(i-1,j),lon(i-1,j))
          fx = (tabvar(i,j) - tabvar(i-1,j))/hx
       ELSE
          foundx = .FALSE.                 
       ENDIF
       !           
    ELSE IF(i+1<=nx .AND. i>=1) THEN    
       !
       IF(mask(i+1,j,k)==1. .AND. mask(i,j,k)==1.) THEN     
          hx = get_dist(lat(i+1,j),lon(i+1,j),&
               lat(i,j),lon(i,j))
          fx = (tabvar(i+1,j) - tabvar(i,j))/hx
       ELSE
          foundx = .FALSE.             
       ENDIF
       !   
    ELSE IF(i<=nx .AND. i-1>=1) THEN    
       !
       IF(mask(i,j,k)==1. .AND. mask(i-1,j,k)==1.) THEN     
          hx = get_dist(lat(i,j),lon(i,j),&
               lat(i-1,j),lon(i-1,j))
          fx = (tabvar(i,j) - tabvar(i-1,j))/hx
       ELSE
          foundx = .FALSE.            
       ENDIF
       !
    ELSE
        foundy = .FALSE.             
    ENDIF

    !
    ! y direction
    !
    IF(j+1<=ny .AND. j-1>=1) THEN      
       IF( mask(i,j+1,k)==1. .AND. mask(i,j-1,k)==1. ) THEN      
          hy = get_dist(lat(i,j+1),lon(i,j+1),&
               lat(i,j-1),lon(i,j-1))                      
          fy = (tabvar(i,j+1) - tabvar(i,j-1))/hy
       ELSE IF( mask(i,j+1,k)==1. .AND. mask(i,j-1,k)==0. .AND. mask(i,j,k)==1.) THEN      
          hy = get_dist(lat(i,j+1),lon(i,j+1),&
               lat(i,j),lon(i,j))                      
          fy = (tabvar(i,j+1) - tabvar(i,j))/hy      
       ELSE IF( mask(i,j+1,k)==0. .AND. mask(i,j-1,k)==1. .AND. mask(i,j,k)==1.) THEN     
          hy = get_dist(lat(i,j),lon(i,j),&
               lat(i,j-1),lon(i,j-1))                      
          fy = (tabvar(i,j) - tabvar(i,j-1))/hy      
       ELSE
          foundy = .FALSE.                    
       ENDIF
       !           
    ELSE IF(j+1<=ny .AND. j>=1) THEN    
       !
       IF(mask(i,j+1,k)==1. .AND. mask(i,j,k)==1.) THEN     
          hy = get_dist(lat(i,j+1),lon(i,j+1),&
               lat(i,j),lon(i,j))
          fy = (tabvar(i,j+1) - tabvar(i,j))/hy
       ELSE
          foundy = .FALSE.            
       ENDIF
       !   
    ELSE IF(j<=ny .AND. j-1>=1) THEN    
       !
       IF(mask(i,j,k)==1. .AND. mask(i,j-1,k)==1.) THEN     
          hy = get_dist(lat(i,j),lon(i,j),&
               lat(i,j-1),lon(i,j-1))
          fy = (tabvar(i,j) - tabvar(i,j-1))/hy
       ELSE
          foundy = .FALSE.             
       ENDIF
       !
    ELSE
        foundy = .FALSE.             
    ENDIF
    !                   
    h_x = get_dist(lat(ipt,jpt),lon(ipt,jpt),lat(ipt,jpt),lon(i,j))
    h_y = get_dist(lat(ipt,jpt),lon(ipt,jpt),lat(i,j),lon(ipt,jpt))
    !
    IF(.NOT.foundx .AND. .NOT.foundy)THEN      
       search_pts_h = 2000000.
    ELSE IF( foundx .AND. foundy) THEN        
       search_pts_h = h_x * fx + h_y * fy
    ELSE IF( .NOT.foundx .AND. foundy .AND. h_y.NE.0.) THEN        
       search_pts_h = h_y * fy
    ELSE IF( foundx .AND. .NOT.foundy .AND. h_x.NE.0.) THEN        
       search_pts_h = h_x * fx
    ELSE    
       search_pts_h = 2000000.             
    ENDIF

    !      
    RETURN          
    !
  END FUNCTION search_pts_h
  !
  !**************************************************************
  !    end subroutine search_pts_h
  !************************************************************** 
  !
  !**************************************************************
  !    subroutine search_pts_v
  !************************************************************** 
  !
  REAL*8 FUNCTION search_pts_v(i,j,k,ipt,jpt,kpt,tabvarkm1,tabvarkm2,mask,depth,lon,lat)
    !
    REAL*8 :: hz,fz,dz,fh
    REAL*8, DIMENSION(:) :: depth 
    REAL*8, DIMENSION(:,:,:,:) :: tabvarkm1,tabvarkm2
    INTEGER :: i,j,k,ipt,jpt,kpt,nx,ny
    LOGICAL :: foundz
    REAL*8, DIMENSION(:,:,:) :: mask
    REAL*8, DIMENSION(:,:) :: lon,lat
    !          
    IF( k <= 2 .OR. mask(i,j,k) == 0.   ) THEN
       !
       search_pts_v = 2000000.
       RETURN
       !
    ELSE IF( i==ipt .AND. j==jpt .AND. mask(i,j,k-1) == 1. .AND. mask(i,j,k-2) == 1. ) THEN
       !
       dz = depth(k) - depth(k-1)
       hz = depth(kpt) - depth(k)     
       search_pts_v = ((tabvarkm2(i,j,1,1) - tabvarkm1(i,j,1,1))/dz)*hz
       RETURN
       ! 
    ELSE
       !
       IF( mask(i,j,k) == 1. .AND. mask(i,j,k-1) == 1. ) THEN
          !                         
          dz = depth(k) - depth(k-1)
          fz = (tabvarkm2(i,j,1,1) - tabvarkm1(i,j,1,1))/dz
          hz = depth(kpt) - depth(k)
          !
       ELSE
          foundz = .FALSE.
       ENDIF
       !             
       fh = search_pts_h(i,j,k,ipt,jpt,k,tabvarkm1(:,:,1,1),mask,lon,lat)
       !
       IF(foundz) THEN
          search_pts_v = hz * fz + fh
          RETURN
       ELSE       
          search_pts_v = 2000000.  
          RETURN 
       ENDIF
       !
    ENDIF
    WRITE(*,*) 'cas 2', search_pts_v
    !     
    RETURN          
    !
  END FUNCTION search_pts_v
  !
  !**************************************************************
  !    end subroutine search_pts_v
  !************************************************************** 
  !
  !
END MODULE agrif_extrapolation
