!
MODULE bicubic_interp
  !
  USE agrif_modutil
  !
  !************************************************************************
  ! 									*
  ! MODULE  BICUBIC INTERP						*
  ! 									*
  ! bicubic interpolation routines from SCRIP package			*		
  !									*
  !http://climate.lanl.gov/Software/SCRIP/				*
  !									*
  !Bicubic remapping							*
  !									*
  !************************************************************************
  !                 
  IMPLICIT NONE
  !
  INTEGER :: grid1_size,grid2_size,grid1_rank, grid2_rank      
  INTEGER, DIMENSION(:), POINTER :: grid1_dims, grid2_dims  
  LOGICAL, DIMENSION(:), POINTER :: grid1_mask,grid2_mask        
  REAL*8,DIMENSION(:),POINTER :: &
       grid1_center_lat,  &
       grid1_center_lon,  & 
       grid2_center_lat,  &
       grid2_center_lon,  &
       grid1_frac,        & 
       grid2_frac           
  REAL*8,DIMENSION(:,:), POINTER :: grid1_bound_box,grid2_bound_box   
  INTEGER, PARAMETER :: num_srch_bins = 90  
  INTEGER,DIMENSION(:,:),POINTER :: bin_addr1,bin_addr2 
  REAL*8, DIMENSION(:,:),POINTER :: bin_lats,bin_lons       
  REAL*8, PARAMETER :: zero   = 0.0,  &
       one    = 1.0,  &
       two    = 2.0,  &
       three  = 3.0,  &
       four   = 4.0,  &
       five   = 5.0,  & 
       half   = 0.5,  &
       quart  = 0.25, &
       bignum = 1.e+20, &
       tiny   = 1.e-14, &
       pi     = 3.14159265359, &
       pi2    = two*pi, &
       pih    = half*pi        

  REAL*8, PARAMETER :: deg2rad = pi/180.   
  INTEGER , PARAMETER :: max_iter = 100   
  REAL*8, PARAMETER :: converge = 1.e-10 
  INTEGER, PARAMETER :: norm_opt_none    = 1 &
       ,norm_opt_dstarea = 2 &
       ,norm_opt_frcarea = 3
  !
  INTEGER, PARAMETER :: map_type_conserv  = 1 &
       ,map_type_bilinear = 2 &
       ,map_type_bicubic  = 3 &
       ,map_type_distwgt  = 4
  !
  INTEGER :: max_links_map1  &  ! current size of link arrays
       ,num_links_map1  &  ! actual number of links for remapping
       ,max_links_map2  &  ! current size of link arrays
       ,num_links_map2  &  ! actual number of links for remapping
       ,num_maps        &  ! num of remappings for this grid pair
       ,num_wts         &  ! num of weights used in remapping
       ,map_type        &  ! identifier for remapping method
       ,norm_opt        &  ! option for normalization (conserv only)
       ,resize_increment ! default amount to increase array size

  INTEGER , DIMENSION(:), POINTER :: &
       bicubic_grid1_add_map1, &  ! grid1 address for each link in mapping 1
       bicubic_grid2_add_map1, &  ! grid2 address for each link in mapping 1
       grid1_add_map2, &  ! grid1 address for each link in mapping 2
       grid2_add_map2   ! grid2 address for each link in mapping 2

  REAL*8, DIMENSION(:,:), POINTER ::   &
       bicubic_wts_map1, &   ! map weights for each link (num_wts,max_links)
       wts_map2 => NULL()    ! map weights for each link (num_wts,max_links)






CONTAINS







  !***********************************************************************
  SUBROUTINE get_remap_bicub(grid1_lat,grid2_lat,grid1_lon,grid2_lon,mask, &
       remap_matrix,source_add,destination_add)

    !-----------------------------------------------------------------------
    !
    !     this routine computes the weights for a bicubic interpolation.
    !
    !-----------------------------------------------------------------------
    REAL*8,DIMENSION(:,:),POINTER :: grid1_lat,grid2_lat,grid1_lon,grid2_lon
    LOGICAL,DIMENSION(:,:) :: mask
    !      
    INTEGER,DIMENSION(:),POINTER :: source_add,destination_add
    REAL*8,DIMENSION(:,:),POINTER :: remap_matrix 
    !-----------------------------------------------------------------------
    !
    !     local variables
    !
    !-----------------------------------------------------------------------

    INTEGER :: n,icount,dst_add,iter,nx,ny,i,j,ip1,e_add,jp1,n_add,ne_add,nele
    REAL*8, DIMENSION(4) :: tmp_lats, tmp_lons
    INTEGER, DIMENSION(4) :: src_add

    REAL*8, DIMENSION(4)  :: src_lats,src_lons
    INTEGER lastsrc_add
    REAL*8, DIMENSION(4,4)  :: wgts
    REAL*8 :: dlat,dlon
    REAL*8 :: plat, plon,iguess, jguess,thguess, phguess,deli, delj, &
         dth1, dth2, dth3,dph1, dph2, dph3,dthp, dphp,          &
         mat1, mat2, mat3, mat4,determinant,sum_wgts,           &
         w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16


    !-----------------------------------------------------------------------
    !
    !     compute mappings from grid1 to grid2
    !
    !-----------------------------------------------------------------------
    !
    ALLOCATE(grid1_dims(2),grid2_dims(2))
    grid1_dims(1) = SIZE(grid1_lat,2)
    grid1_dims(2) = SIZE(grid1_lat,1)
    grid2_dims(1) = SIZE(grid2_lat,2)
    grid2_dims(2) = SIZE(grid2_lat,1)
    grid1_size = SIZE(grid1_lat,2) * SIZE(grid1_lat,1)
    grid2_size = SIZE(grid2_lat,2) * SIZE(grid2_lat,1) 
    !      
    ALLOCATE( grid2_mask(grid2_size),         &
         grid1_bound_box (4,grid1_size), &
         grid2_bound_box (4,grid2_size), &
         grid1_frac      (grid1_size),   &
         grid2_frac      (grid2_size))

    grid1_frac = zero
    grid2_frac = zero

    !
    ! 2D array -> 1D array
    !
    ALLOCATE(grid1_center_lat(SIZE(grid1_lat,1)*SIZE(grid1_lat,2)))
    CALL tab2Dto1D(grid1_lat,grid1_center_lat)

    ALLOCATE(grid1_center_lon(SIZE(grid1_lon,1)*SIZE(grid1_lon,2)))
    CALL tab2Dto1D(grid1_lon,grid1_center_lon)

    ALLOCATE(grid2_center_lat(SIZE(grid2_lat,1)*SIZE(grid2_lat,2)))
    CALL tab2Dto1D(grid2_lat,grid2_center_lat)

    ALLOCATE(grid2_center_lon(SIZE(grid2_lon,1)*SIZE(grid2_lon,2)))      
    CALL tab2Dto1D(grid2_lon,grid2_center_lon) 

    ALLOCATE(grid1_mask(SIZE(grid1_lat,1)*SIZE(grid1_lat,2)))
    CALL logtab2Dto1D(mask,grid1_mask)
    !      
    !      Write(*,*) ,'grid1_mask = ',grid1_mask 
    !              
    !
    ! degrees to radian
    !
    grid1_center_lat = grid1_center_lat*deg2rad
    grid1_center_lon = grid1_center_lon*deg2rad
    grid2_center_lat = grid2_center_lat*deg2rad
    grid2_center_lon = grid2_center_lon*deg2rad

    !-----------------------------------------------------------------------
    !     convert longitudes to 0,2pi interval
    !-----------------------------------------------------------------------

    WHERE (grid1_center_lon .GT. pi2)  grid1_center_lon =       &
         grid1_center_lon - pi2
    WHERE (grid1_center_lon .LT. zero) grid1_center_lon =       &
         grid1_center_lon + pi2
    WHERE (grid2_center_lon .GT. pi2)  grid2_center_lon =       &
         grid2_center_lon - pi2
    WHERE (grid2_center_lon .LT. zero) grid2_center_lon =       &
         grid2_center_lon + pi2

    !-----------------------------------------------------------------------
    !
    !     make sure input latitude range is within the machine values
    !     for +/- pi/2 
    !
    !-----------------------------------------------------------------------

    WHERE (grid1_center_lat >  pih) grid1_center_lat =  pih
    WHERE (grid1_center_lat < -pih) grid1_center_lat = -pih
    WHERE (grid2_center_lat >  pih) grid2_center_lat =  pih
    WHERE (grid2_center_lat < -pih) grid2_center_lat = -pih
    !

    !
    !-----------------------------------------------------------------------
    !
    !     compute bounding boxes for restricting future grid searches
    !
    !-----------------------------------------------------------------------
    !
    nx = grid1_dims(1)
    ny = grid1_dims(2)

    DO n=1,grid1_size

       !*** find N,S and NE points to this grid point

       j = (n - 1)/nx +1
       i = n - (j-1)*nx

       IF (i < nx) THEN
          ip1 = i + 1
       ELSE
          !*** assume cyclic
          ip1 = 1
          !*** but if it is not, correct
          e_add = (j - 1)*nx + ip1
          IF (ABS(grid1_center_lat(e_add) -     &
               grid1_center_lat(n   )) > pih) THEN
             ip1 = i
          ENDIF
          ip1=nx
       ENDIF

       IF (j < ny) THEN
          jp1 = j+1
       ELSE
          !*** assume cyclic
          jp1 = 1
          !*** but if it is not, correct
          n_add = (jp1 - 1)*nx + i
          IF (ABS(grid1_center_lat(n_add) -             &
               grid1_center_lat(n   )) > pih) THEN
             jp1 = j
          ENDIF
          jp1=ny
       ENDIF

       n_add = (jp1 - 1)*nx + i
       e_add = (j - 1)*nx + ip1
       ne_add = (jp1 - 1)*nx + ip1

       !*** find N,S and NE lat/lon coords and check bounding box

       tmp_lats(1) = grid1_center_lat(n)
       tmp_lats(2) = grid1_center_lat(e_add)
       tmp_lats(3) = grid1_center_lat(ne_add)
       tmp_lats(4) = grid1_center_lat(n_add)

       tmp_lons(1) = grid1_center_lon(n)
       tmp_lons(2) = grid1_center_lon(e_add)
       tmp_lons(3) = grid1_center_lon(ne_add)
       tmp_lons(4) = grid1_center_lon(n_add)

       grid1_bound_box(1,n) = MINVAL(tmp_lats)
       grid1_bound_box(2,n) = MAXVAL(tmp_lats)

       grid1_bound_box(3,n) = MINVAL(tmp_lons)
       grid1_bound_box(4,n) = MAXVAL(tmp_lons)
    END DO

    nx = grid2_dims(1)
    ny = grid2_dims(2)

    DO n=1,grid2_size

       !*** find N,S and NE points to this grid point

       j = (n - 1)/nx +1
       i = n - (j-1)*nx

       IF (i < nx) THEN
          ip1 = i + 1
       ELSE
          !*** assume cyclic
          ip1 = 1
          !*** but if it is not, correct
          e_add = (j - 1)*nx + ip1
          IF (ABS(grid2_center_lat(e_add) -  &
               grid2_center_lat(n   )) > pih) THEN
             ip1 = i
          ENDIF
       ENDIF

       IF (j < ny) THEN
          jp1 = j+1
       ELSE
          !*** assume cyclic
          jp1 = 1
          !*** but if it is not, correct
          n_add = (jp1 - 1)*nx + i
          IF (ABS(grid2_center_lat(n_add) -  &
               grid2_center_lat(n   )) > pih) THEN
             jp1 = j
          ENDIF
       ENDIF

       n_add = (jp1 - 1)*nx + i
       e_add = (j - 1)*nx + ip1
       ne_add = (jp1 - 1)*nx + ip1

       !*** find N,S and NE lat/lon coords and check bounding box

       tmp_lats(1) = grid2_center_lat(n)
       tmp_lats(2) = grid2_center_lat(e_add)
       tmp_lats(3) = grid2_center_lat(ne_add)
       tmp_lats(4) = grid2_center_lat(n_add)

       tmp_lons(1) = grid2_center_lon(n)
       tmp_lons(2) = grid2_center_lon(e_add)
       tmp_lons(3) = grid2_center_lon(ne_add)
       tmp_lons(4) = grid2_center_lon(n_add)

       grid2_bound_box(1,n) = MINVAL(tmp_lats)
       grid2_bound_box(2,n) = MAXVAL(tmp_lats)
       grid2_bound_box(3,n) = MINVAL(tmp_lons)
       grid2_bound_box(4,n) = MAXVAL(tmp_lons)
    END DO
    !
    !
    !
    WHERE (ABS(grid1_bound_box(4,:) - grid1_bound_box(3,:)) > pi)
       grid1_bound_box(3,:) = zero
       grid1_bound_box(4,:) = pi2
    END WHERE

    WHERE (ABS(grid2_bound_box(4,:) - grid2_bound_box(3,:)) > pi)
       grid2_bound_box(3,:) = zero
       grid2_bound_box(4,:) = pi2
    END WHERE

    !***
    !*** try to check for cells that overlap poles
    !***

    WHERE (grid1_center_lat > grid1_bound_box(2,:)) &
         grid1_bound_box(2,:) = pih

    WHERE (grid1_center_lat < grid1_bound_box(1,:)) &
         grid1_bound_box(1,:) = -pih

    WHERE (grid2_center_lat > grid2_bound_box(2,:)) &
         grid2_bound_box(2,:) = pih

    WHERE (grid2_center_lat < grid2_bound_box(1,:)) &
         grid2_bound_box(1,:) = -pih

    !-----------------------------------------------------------------------
    !     set up and assign address ranges to search bins in order to 
    !     further restrict later searches
    !-----------------------------------------------------------------------

    ALLOCATE(bin_addr1(2,num_srch_bins))
    ALLOCATE(bin_addr2(2,num_srch_bins))
    ALLOCATE(bin_lats (2,num_srch_bins))
    ALLOCATE(bin_lons (2,num_srch_bins))

    dlat = pi/num_srch_bins

    DO n=1,num_srch_bins
       bin_lats(1,n) = (n-1)*dlat - pih
       bin_lats(2,n) =     n*dlat - pih
       bin_lons(1,n) = zero
       bin_lons(2,n) = pi2
       bin_addr1(1,n) = grid1_size + 1
       bin_addr1(2,n) = 0
       bin_addr2(1,n) = grid2_size + 1
       bin_addr2(2,n) = 0
    END DO

    DO nele=1,grid1_size
       DO n=1,num_srch_bins
          IF (grid1_bound_box(1,nele) <= bin_lats(2,n) .AND.   &
               grid1_bound_box(2,nele) >= bin_lats(1,n)) THEN
             bin_addr1(1,n) = MIN(nele,bin_addr1(1,n))
             bin_addr1(2,n) = MAX(nele,bin_addr1(2,n))
          ENDIF
       END DO
    END DO

    DO nele=1,grid2_size
       DO n=1,num_srch_bins
          IF (grid2_bound_box(1,nele) <= bin_lats(2,n) .AND.    &
               grid2_bound_box(2,nele) >= bin_lats(1,n)) THEN
             bin_addr2(1,n) = MIN(nele,bin_addr2(1,n))
             bin_addr2(2,n) = MAX(nele,bin_addr2(2,n))
          ENDIF
       END DO
    END DO

    CALL init_remap_vars    
    !
    !***
    !*** loop over destination grid 
    !***
    !      
    grid2_mask = .TRUE.
    !
    lastsrc_add=1
    !         
    WRITE(*,*) 'Bicubic remapping weights computation'
    !      
    DO dst_add = 1, grid2_size

       plat = grid2_center_lat(dst_add)
       plon = grid2_center_lon(dst_add)         
       !           
       !           
       CALL grid_search_bicub(src_add, src_lats, src_lons,     &
            plat, plon, grid1_dims,              &
            grid1_center_lat, grid1_center_lon,  &
            grid1_bound_box, bin_addr1, bin_addr2,lastsrc_add)
       !        
       !           
       IF (src_add(1) > 0) THEN
          DO n=1,4
             IF (.NOT. grid1_mask(src_add(n))) src_add(1) = 0
          END DO
       ENDIF

       !-----------------------------------------------------------------------
       !
       !       if point found, find local i,j coordinates for weights
       !
       !-----------------------------------------------------------------------

       IF (src_add(1) > 0) THEN

          grid2_frac(dst_add) = one
          !
          dth1 = src_lats(2) - src_lats(1)
          dth2 = src_lats(4) - src_lats(1)
          dth3 = src_lats(3) - src_lats(2) - dth2

          dph1 = src_lons(2) - src_lons(1)
          dph2 = src_lons(4) - src_lons(1)
          dph3 = src_lons(3) - src_lons(2)

          IF (dph1 >  three*pih) dph1 = dph1 - pi2
          IF (dph2 >  three*pih) dph2 = dph2 - pi2
          IF (dph3 >  three*pih) dph3 = dph3 - pi2
          IF (dph1 < -three*pih) dph1 = dph1 + pi2
          IF (dph2 < -three*pih) dph2 = dph2 + pi2
          IF (dph3 < -three*pih) dph3 = dph3 + pi2

          dph3 = dph3 - dph2
          iguess = half
          jguess = half

          DO iter=1,max_iter

             dthp = plat-src_lats(1)-dth1*iguess- &
                  dth2*jguess-dth3*iguess*jguess
             dphp = plon - src_lons(1)

             IF (dphp >  three*pih) dphp = dphp - pi2
             IF (dphp < -three*pih) dphp = dphp + pi2

             dphp = dphp - dph1*iguess - dph2*jguess - & 
                  dph3*iguess*jguess
             !
             mat1 = dth1 + dth3*jguess
             mat2 = dth2 + dth3*iguess
             mat3 = dph1 + dph3*jguess
             mat4 = dph2 + dph3*iguess
             !
             determinant = mat1*mat4 - mat2*mat3
             !
             deli = (dthp*mat4 - mat2*dphp)/determinant
             delj = (mat1*dphp - dthp*mat3)/determinant
             !
             IF (ABS(deli) < converge .AND.            & 
                  ABS(delj) < converge) EXIT
             !
             iguess = iguess + deli
             jguess = jguess + delj

          END DO

          IF (iter <= max_iter) THEN
             !
             wgts(1,1) = (one - jguess**2*(three-two*jguess))*   &
                  (one - iguess**2*(three-two*iguess))

             wgts(1,2) = (one - jguess**2*(three-two*jguess))*   &
                  iguess**2*(three-two*iguess)

             wgts(1,3) = jguess**2*(three-two*jguess)*           &
                  iguess**2*(three-two*iguess)

             wgts(1,4) = jguess**2*(three-two*jguess)*           &
                  (one - iguess**2*(three-two*iguess))

             wgts(2,1) = (one - jguess**2*(three-two*jguess))*   &
                  iguess*(iguess-one)**2

             wgts(2,2) = (one - jguess**2*(three-two*jguess))*   &
                  iguess**2*(iguess-one)

             wgts(2,3) = jguess**2*(three-two*jguess)*           &
                  iguess**2*(iguess-one)

             wgts(2,4) =  jguess**2*(three-two*jguess)*          &
                  iguess*(iguess-one)**2

             wgts(3,1) =  jguess*(jguess-one)**2*                &
                  (one - iguess**2*(three-two*iguess))

             wgts(3,2) =  jguess*(jguess-one)**2*                &
                  iguess**2*(three-two*iguess)                            

             wgts(3,3) =  jguess**2*(jguess-one)*                &
                  iguess**2*(three-two*iguess)

             wgts(3,4) =  jguess**2*(jguess-one)*                &
                  (one - iguess**2*(three-two*iguess))

             wgts(4,1) =  iguess*(iguess-one)**2*                &
                  jguess*(jguess-one)**2

             wgts(4,2) =  iguess**2*(iguess-one)*                &
                  jguess*(jguess-one)**2

             wgts(4,3) =  iguess**2*(iguess-one)*                &
                  jguess**2*(jguess-one)

             wgts(4,4) =  iguess*(iguess-one)**2*                &
                  jguess**2*(jguess-one)

             CALL store_link_bicub(dst_add, src_add, wgts)

          ELSE
             STOP 'Iteration for i,j exceed max iteration count'
          ENDIF
          !
       ELSE IF (src_add(1) < 0) THEN

          src_add = ABS(src_add)
          !
          icount = 0
          !          
          DO n=1,4
             IF (grid1_mask(src_add(n))) THEN
                icount = icount + 1
             ELSE
                src_lats(n) = zero
             ENDIF
          END DO

          IF (icount > 0) THEN

             sum_wgts = SUM(src_lats)
             wgts(1,1) = src_lats(1)/sum_wgts
             wgts(1,2) = src_lats(2)/sum_wgts
             wgts(1,3) = src_lats(3)/sum_wgts
             wgts(1,4) = src_lats(4)/sum_wgts
             wgts(2:4,:) = zero

             CALL store_link_bicub(dst_add, src_add, wgts)

          ENDIF

       ENDIF
       !
    END DO
    !              
    ALLOCATE(remap_matrix(SIZE(bicubic_wts_map1,1),SIZE(bicubic_wts_map1,2)), &
         source_add(num_links_map1),       &
         destination_add(num_links_map1))

    DO j = 1,SIZE(bicubic_wts_map1,2)
       DO i = 1,SIZE(bicubic_wts_map1,1)

          remap_matrix(i,j) = bicubic_wts_map1(i,j)

       END DO
    END DO
    !        
    source_add(:) = bicubic_grid1_add_map1(1:num_links_map1)
    destination_add(:) = bicubic_grid2_add_map1(1:num_links_map1)
    !                
    !
    WHERE(destination_add == 0)
       destination_add = 1
    END WHERE

    WHERE(source_add == 0)
       source_add = 1
    END WHERE
    !               
    DEALLOCATE(grid1_bound_box,grid2_bound_box,grid1_center_lat,grid1_center_lon)
    DEALLOCATE(grid2_center_lat,grid2_center_lon,bicubic_grid1_add_map1,bicubic_wts_map1)
    DEALLOCATE(grid1_frac,grid2_frac,grid1_dims,grid2_dims,grid2_mask)
    DEALLOCATE(bin_addr1,bin_addr2,bin_lats,bin_lons)
    DEALLOCATE(grid1_mask,bicubic_grid2_add_map1)	
    !
    !-----------------------------------------------------------------------

  END SUBROUTINE get_remap_bicub

  !
  !***********************************************************************

  !***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !************************************************************************
  !   SUBROUTINE GRID_SEARCH_BILIN
  !************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  SUBROUTINE grid_search_bicub(src_add, src_lats, src_lons,   &
       plat, plon, src_grid_dims,      &
       src_center_lat, src_center_lon, & 
       src_grid_bound_box,             &
       src_bin_add, dst_bin_add,lastsrc_add)

    !-----------------------------------------------------------------------
    !
    !     this routine finds the location of the search point plat, plon
    !     in the source grid and returns the corners needed for a bilinear
    !     interpolation.
    !
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !     output variables
    !-----------------------------------------------------------------------
    !
    ! address of each corner point enclosing P
    !
    INTEGER,DIMENSION(4) :: src_add  
    REAL*8,DIMENSION(4) :: src_lats,src_lons  
    !      
    !-----------------------------------------------------------------------
    !     input variables
    !-----------------------------------------------------------------------
    ! 
    ! latitude, longitude of the search point
    !
    REAL*8, INTENT(in) :: plat,plon   
    !
    ! size of each src grid dimension
    !
    INTEGER, DIMENSION(2), INTENT(in) :: src_grid_dims  
    !
    ! latitude, longitude of each src grid center
    !
    REAL*8, DIMENSION(:), INTENT(in) :: src_center_lat,src_center_lon  
    !
    ! bound box for source grid
    !
    REAL*8, DIMENSION(:,:), INTENT(in) :: src_grid_bound_box 
    !
    ! latitude bins for restricting searches
    !
    INTEGER, DIMENSION(:,:), INTENT(in) ::src_bin_add,dst_bin_add 

    INTEGER,OPTIONAL :: lastsrc_add
    INTEGER :: loopsrc,l1,l2
    !      
    !-----------------------------------------------------------------------
    !     local variables
    !-----------------------------------------------------------------------
    !
    INTEGER :: n,next_n,srch_add,nx, ny,min_add, max_add,  &
         i, j, jp1, ip1, n_add, e_add, ne_add


    REAL*8 ::  vec1_lat, vec1_lon,vec2_lat, vec2_lon, cross_product,  &
         cross_product_last,coslat_dst, sinlat_dst, coslon_dst, &
         sinlon_dst,dist_min, distance 

    !-----------------------------------------------------------------------
    !     restrict search first using bins
    !-----------------------------------------------------------------------

    src_add = 0
    min_add = SIZE(src_center_lat)
    max_add = 1
    DO n=1,num_srch_bins
       IF (plat >= bin_lats(1,n) .AND. plat <= bin_lats(2,n) .AND. &
            plon >= bin_lons(1,n) .AND. plon <= bin_lons(2,n)) THEN
          min_add = MIN(min_add, src_bin_add(1,n))
          max_add = MAX(max_add, src_bin_add(2,n))
       ENDIF
    END DO

    !-----------------------------------------------------------------------
    !     now perform a more detailed search 
    !-----------------------------------------------------------------------

    nx = src_grid_dims(1)
    ny = src_grid_dims(2)

    loopsrc=0
    DO WHILE (loopsrc <= max_add)


       l1=MAX(min_add,lastsrc_add-loopsrc)
       l2=MIN(max_add,lastsrc_add+loopsrc)      

       loopsrc = loopsrc+1

       srch_loop: DO srch_add = l1,l2,MAX(l2-l1,1)

          !*** first check bounding box

          IF (plat <= src_grid_bound_box(2,srch_add) .AND. & 
               plat >= src_grid_bound_box(1,srch_add) .AND.  &
               plon <= src_grid_bound_box(4,srch_add) .AND.  &
               plon >= src_grid_bound_box(3,srch_add)) THEN


             !***
             !*** we are within bounding box so get really serious
             !***
             !*** determine neighbor addresses
             !
             j = (srch_add - 1)/nx +1
             i = srch_add - (j-1)*nx
             !
             IF (i < nx) THEN
                ip1 = i + 1
             ELSE
                ip1 = 1
             ENDIF
             !
             IF (j < ny) THEN
                jp1 = j+1
             ELSE
                jp1 = 1
             ENDIF
             !
             n_add = (jp1 - 1)*nx + i
             e_add = (j - 1)*nx + ip1
             ne_add = (jp1 - 1)*nx + ip1
             !
             src_lats(1) = src_center_lat(srch_add)
             src_lats(2) = src_center_lat(e_add)
             src_lats(3) = src_center_lat(ne_add)
             src_lats(4) = src_center_lat(n_add)
             !
             src_lons(1) = src_center_lon(srch_add)
             src_lons(2) = src_center_lon(e_add)
             src_lons(3) = src_center_lon(ne_add)
             src_lons(4) = src_center_lon(n_add)
             !
             !***
             !*** for consistency, we must make sure all lons are in
             !*** same 2pi interval
             !***
             !
             vec1_lon = src_lons(1) - plon
             IF (vec1_lon >  pi) THEN
                src_lons(1) = src_lons(1) - pi2
             ELSE IF (vec1_lon < -pi) THEN
                src_lons(1) = src_lons(1) + pi2
             ENDIF
             DO n=2,4
                vec1_lon = src_lons(n) - src_lons(1)
                IF (vec1_lon >  pi) THEN
                   src_lons(n) = src_lons(n) - pi2
                ELSE IF (vec1_lon < -pi) THEN
                   src_lons(n) = src_lons(n) + pi2
                ENDIF
             END DO
             !
             corner_loop: DO n=1,4
                next_n = MOD(n,4) + 1
                !***
                !*** here we take the cross product of the vector making 
                !*** up each box side with the vector formed by the vertex
                !*** and search point.  if all the cross products are 
                !*** positive, the point is contained in the box.
                !***
                vec1_lat = src_lats(next_n) - src_lats(n)
                vec1_lon = src_lons(next_n) - src_lons(n)
                vec2_lat = plat - src_lats(n)
                vec2_lon = plon - src_lons(n)
                !***
                !*** check for 0,2pi crossings
                !***
                IF (vec1_lon >  three*pih) THEN
                   vec1_lon = vec1_lon - pi2
                ELSE IF (vec1_lon < -three*pih) THEN
                   vec1_lon = vec1_lon + pi2
                ENDIF
                IF (vec2_lon >  three*pih) THEN
                   vec2_lon = vec2_lon - pi2
                ELSE IF (vec2_lon < -three*pih) THEN
                   vec2_lon = vec2_lon + pi2
                ENDIF
                !
                cross_product = vec1_lon*vec2_lat - vec2_lon*vec1_lat
                !
                !***
                !*** if cross product is less than zero, this cell
                !*** doesn't work
                !***
                IF (n == 1) cross_product_last = cross_product
                IF (cross_product*cross_product_last < zero) &
                     EXIT corner_loop
                cross_product_last = cross_product
                !
             END DO corner_loop
             !***
             !*** if cross products all same sign, we found the location
             !***
             IF (n > 4) THEN
                src_add(1) = srch_add
                src_add(2) = e_add
                src_add(3) = ne_add
                src_add(4) = n_add
                !
                lastsrc_add = srch_add
                RETURN
             ENDIF
             !***
             !*** otherwise move on to next cell
             !***
          ENDIF !bounding box check
       END DO srch_loop


    ENDDO

    !***
    !*** if no cell found, point is likely either in a box that
    !*** straddles either pole or is outside the grid.  fall back
    !*** to a distance-weighted average of the four closest
    !*** points.  go ahead and compute weights here, but store
    !*** in src_lats and return -add to prevent the parent
    !*** routine from computing bilinear weights
    !***
    !print *,'Could not find location for ',plat,plon
    !print *,'Using nearest-neighbor average for this point'
    !
    coslat_dst = COS(plat)
    sinlat_dst = SIN(plat)
    coslon_dst = COS(plon)
    sinlon_dst = SIN(plon)
    !
    dist_min = bignum
    src_lats = bignum
    DO srch_add = min_add,max_add
       distance = ACOS(coslat_dst*COS(src_center_lat(srch_add))*   &
            (coslon_dst*COS(src_center_lon(srch_add)) +   &
            sinlon_dst*SIN(src_center_lon(srch_add)))+   &
            sinlat_dst*SIN(src_center_lat(srch_add)))

       IF (distance < dist_min) THEN
          sort_loop: DO n=1,4
             IF (distance < src_lats(n)) THEN
                DO i=4,n+1,-1
                   src_add (i) = src_add (i-1)
                   src_lats(i) = src_lats(i-1)
                END DO
                src_add (n) = -srch_add
                src_lats(n) = distance
                dist_min = src_lats(4)
                EXIT sort_loop
             ENDIF
          END DO sort_loop
       ENDIF
    END DO
    !
    src_lons = one/(src_lats + tiny)
    distance = SUM(src_lons)
    src_lats = src_lons/distance

    !-----------------------------------------------------------------------

  END SUBROUTINE grid_search_bicub













  !-----------------------------------------------------------------------

  SUBROUTINE store_link_bicub(dst_add, src_add, weights)

    !-----------------------------------------------------------------------
    !
    !     this routine stores the address and weight for four links 
    !     associated with one destination point in the appropriate address 
    !     and weight arrays and resizes those arrays if necessary.
    !
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !
    !     input variables
    !
    !-----------------------------------------------------------------------

    INTEGER :: dst_add
    INTEGER, DIMENSION(4) :: src_add
    REAL*8, DIMENSION(4,4) :: weights 

    !-----------------------------------------------------------------------
    !
    !     local variables
    !
    !-----------------------------------------------------------------------

    INTEGER :: n,num_links_old

    !-----------------------------------------------------------------------
    !
    !     increment number of links and check to see if remap arrays need
    !     to be increased to accomodate the new link.  then store the
    !     link.
    !
    !-----------------------------------------------------------------------


    num_links_old  = num_links_map1
    num_links_map1 = num_links_old + 4

    IF (num_links_map1 > max_links_map1)    & 
         CALL resize_remap_vars(resize_increment)

    DO n=1,4
       bicubic_grid1_add_map1(num_links_old+n) = src_add(n)
       bicubic_grid2_add_map1(num_links_old+n) = dst_add
       bicubic_wts_map1    (:,num_links_old+n) = weights(:,n)
    END DO





    !-----------------------------------------------------------------------

  END SUBROUTINE store_link_bicub

  !***********************************************************************














  !
  !************************************************************************
  !   SUBROUTINE INIT_REMAP_VAR
  !************************************************************************
  !
  SUBROUTINE init_remap_vars
    !
    num_wts = 4
    !
    num_links_map1 = 0
    max_links_map1 = 4*grid2_size
    IF (num_maps > 1) THEN
       num_links_map2 = 0
       max_links_map1 = MAX(4*grid1_size,4*grid2_size)
       max_links_map2 = max_links_map1
    ENDIF

    resize_increment = 0.1*MAX(grid1_size,grid2_size)
    !
    !-----------------------------------------------------------------------
    !
    !     allocate address and weight arrays for mapping 1
    !   
    !-----------------------------------------------------------------------
    !
    ALLOCATE (bicubic_grid1_add_map1(max_links_map1),  &
         bicubic_grid2_add_map1(max_links_map1),  &
         bicubic_wts_map1(num_wts, max_links_map1))

    !-----------------------------------------------------------------------
    !
    !     allocate address and weight arrays for mapping 2 if necessary 
    !   
    !-----------------------------------------------------------------------

    IF (num_maps > 1) THEN
       ALLOCATE (grid1_add_map2(max_links_map2),    &
            grid2_add_map2(max_links_map2),    &
            wts_map2(num_wts, max_links_map2))
    ENDIF

    !-----------------------------------------------------------------------

  END SUBROUTINE init_remap_vars

  !***********************************************************************
  !************************************************************************
  !   SUBROUTINE RESIZE_REMAP_VAR
  !************************************************************************

  SUBROUTINE resize_remap_vars(increment)

    !-----------------------------------------------------------------------
    !     this routine resizes remapping arrays by increasing(decreasing)
    !     the max_links by increment
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !     input variables
    !-----------------------------------------------------------------------

    INTEGER :: increment

    !-----------------------------------------------------------------------
    !     local variables
    !-----------------------------------------------------------------------

    INTEGER ::    &
         ierr,    &  ! error flag
         mxlinks   ! size of link arrays

    INTEGER, DIMENSION(:), POINTER ::    &
         add1_tmp,   & ! temp array for resizing address arrays
         add2_tmp  ! temp array for resizing address arrays
    !
    ! temp array for resizing weight arrays
    !
    REAL*8, DIMENSION(:,:), POINTER :: wts_tmp   
    !
    !-----------------------------------------------------------------------
    !***
    !*** allocate temporaries to hold original values
    !***
    mxlinks = SIZE(bicubic_grid1_add_map1)
    ALLOCATE (add1_tmp(mxlinks), add2_tmp(mxlinks), &
         wts_tmp(num_wts,mxlinks))

    add1_tmp = bicubic_grid1_add_map1
    add2_tmp = bicubic_grid2_add_map1
    wts_tmp  = bicubic_wts_map1

    !***
    !*** deallocate originals and increment max_links then
    !*** reallocate arrays at new size
    !***

    DEALLOCATE (bicubic_grid1_add_map1, bicubic_grid2_add_map1, bicubic_wts_map1)
    max_links_map1 = mxlinks + increment
    ALLOCATE (bicubic_grid1_add_map1(max_links_map1),    &
         bicubic_grid2_add_map1(max_links_map1),    &
         bicubic_wts_map1(num_wts,max_links_map1))
    !***
    !*** restore original values from temp arrays and
    !*** deallocate temps
    !***
    mxlinks = MIN(mxlinks, max_links_map1)
    bicubic_grid1_add_map1(1:mxlinks) = add1_tmp (1:mxlinks)
    bicubic_grid2_add_map1(1:mxlinks) = add2_tmp (1:mxlinks)
    bicubic_wts_map1    (:,1:mxlinks) = wts_tmp(:,1:mxlinks)
    DEALLOCATE(add1_tmp, add2_tmp, wts_tmp)

    !-----------------------------------------------------------------------
    !
  END SUBROUTINE resize_remap_vars
  !
  !************************************************************************
  !

END MODULE bicubic_interp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
