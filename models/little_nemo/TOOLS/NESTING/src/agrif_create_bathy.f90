!************************************************************************
! Fortran 95 OPA Nesting tools						*
!									*
!     Copyright (C) 2005 Florian Lemarié (Florian.Lemarie@imag.fr)	*
!                        Laurent Debreu (Laurent.Debreu@imag.fr)	*
!************************************************************************
!
PROGRAM create_bathy
  !
  USE NETCDF
  USE bilinear_interp
  USE agrif_readwrite
  USE agrif_partial_steps
  USE agrif_connect_topo
  !
  IMPLICIT NONE
  !
  !************************************************************************
  ! 									*
  ! PROGRAM  CREATE_BATHY						*
  !									*
  ! program to implement bathymetry interpolation to generate 		*
  ! child grid bathymetry file						*
  !									*
  ! various options :							*
  !									*
  ! 1- Interpolation directly from parent bathymetry file (z-coord)	*
  ! 2- Use new topo file in meters (for example etopo)  		*
  !									*
  ! vertical coordinates permitted : z-coord and partial steps		*
  ! sigma coordinates is not yet implemented				*
  !									*
  !Interpolation is carried out using bilinear interpolation		*
  !routine from SCRIP package or median average				*		
  !									*
  !http://climate.lanl.gov/Software/SCRIP/				*
  !************************************************************************
  !
  ! variables declaration
  !      
  CHARACTER(len=80) :: namelistname
  CHARACTER*100 :: Childmeter_file,Childlevel_file,Child_coordinates,child_ps     
  LOGICAL,DIMENSION(:,:),POINTER :: masksrc => NULL()  
  LOGICAL :: identical_grids     
  INTEGER,DIMENSION(:,:),ALLOCATABLE ::mask_oce,trouble_points
  INTEGER :: i,j,num_links,nb,nbadd,status,narg,iargc     
  INTEGER,DIMENSION(:),POINTER :: src_add,dst_add => NULL() 
  INTEGER :: numlatfine,numlonfine,numlat,numlon,pos,pos2
  REAL*8,DIMENSION(:,:),POINTER :: matrix,interpdata => NULL()     
  REAL*8, DIMENSION(:,:),POINTER :: bathy_fin_constant => NULL()  
  REAL*8,DIMENSION(:,:),ALLOCATABLE :: bathy_test,vardep,glamhr,gphihr
  REAL*8,DIMENSION(:),ALLOCATABLE :: vardep1d
  REAL*8, DIMENSION(:,:),POINTER :: gdepw_ps_interp => NULL() 
  REAL*8, DIMENSION(:,:),POINTER :: save_gdepw,rx,ry,maskedtopo
  REAL*8  :: Cell_lonmin,Cell_lonmax,Cell_latmin,Cell_latmax,wghts
  LOGICAL :: Pacifique
  INTEGER :: boundary,xpos,ypos,iimin,iimax,jjmax,jjmin
  INTEGER :: nbloops,nxhr,nyhr,ji,jj,nbiter,nbloopmax
  INTEGER :: ipt,jpt,iloc,jloc
  INTEGER, DIMENSION(2) :: i_min,i_max,j_min,j_max

  TYPE(Coordinates) :: G0,G1 
  !      
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
  ! define names of child grid files
  !
  CALL set_child_name(parent_coordinate_file,Child_coordinates) 
  IF( TRIM(parent_meshmask_file) .NE. '/NULL' ) &
       CALL set_child_name(parent_meshmask_file,Childlevel_file)            
  !
  !
  !
  !
  !
  !------------------------------------------------------------------
  ! ****First option : no new topo file / no partial steps
  ! interpolate levels directly from parent file
  !------------------------------------------------------------------
  !
  !
  !
  !
  !
  !
  IF(.NOT.new_topo .AND. .NOT.partial_steps) THEN      
     !      
     WRITE(*,*) 'First option'
     !read coarse grid bathymetry and coordinates file
     !           
     WRITE(*,*) 'No new topo file ...'
     status = Read_Coordinates(TRIM(parent_coordinate_file),G0)    
     status = Read_bathy_level(TRIM(parent_meshmask_file),G0)
     !           
     IF( imax > SIZE(G0%glamt,1) .OR. jmax > SIZE(G0%glamt,2) .OR. &
          imax <= imin .OR. jmax <= jmin ) THEN                   
        WRITE(*,*) 'ERROR ***** bad child grid definition ...'
        WRITE(*,*) 'please check imin,jmin,imax,jmax,jpizoom,jpjzoom values'       
        WRITE(*,*) ' '
        STOP
     ENDIF
     !
     !read fine grid coordinates file
     !      
     status = Read_Coordinates(TRIM(Child_coordinates),G1,pacifique)
     !  
     IF( SIZE(G1%nav_lon,1) .NE. nxfin .OR. SIZE(G1%nav_lon,2) .NE. nyfin ) THEN
        !
        WRITE(*,*) 'ERROR ***** bad child coordinates file ...'
        WRITE(*,*) ' '
        WRITE(*,*) 'please check that child coordinates file '
        WRITE(*,*) 'has been created with the same namelist '
        WRITE(*,*) ' '
        STOP
        !
     ENDIF
     !
     numlat =  SIZE(G0%nav_lat,2)
     numlon =  SIZE(G0%nav_lat,1)    
     numlatfine =  SIZE(G1%nav_lat,2)
     numlonfine =  SIZE(G1%nav_lat,1)  
     !           
     ALLOCATE(masksrc(numlon,numlat))
     !
     ! create logical array masksrc
     !
     WHERE(G0%bathy_level.LE.0) 
        masksrc = .FALSE.
     ELSEWHERE
        masksrc = .TRUE.
     END WHERE

     IF ( Pacifique ) THEN
        WHERE(G0%nav_lon < 0.001) 
           G0%nav_lon = G0%nav_lon + 360.
        END WHERE
     ENDIF


     !-----------------          
     ! compute remapping matrix thanks to SCRIP package
     !
     ! remapping process
     !              
     ALLOCATE(G1%bathy_meter(nxfin,nyfin))
     CALL levels_to_meter(G0)
     !             
     !             Call levels_to_meter(G1)
     !             
     CALL get_remap_matrix(G0%nav_lat,G1%nav_lat,   &
          G0%nav_lon,G1%nav_lon,   &
          masksrc,matrix,src_add,dst_add)
     CALL make_remap(G0%bathy_meter,G1%bathy_meter,nxfin,nyfin, &
          matrix,src_add,dst_add)  
     !                                  
     !            
     DEALLOCATE(masksrc)
     !-----------------                                      
     !      
     ! 
     ! compute constant bathymetry for Parent-Child bathymetry connection
     !              
     CALL init_constant_bathy(G0%bathy_meter,bathy_fin_constant)
     !
     boundary = connectionsize*irafx + nbghostcellsfine 
     !
     ! connection carried out by copying parent grid values for the fine points
     ! corresponding to 3 coarse grid cells at the boundaries
     !                 
     G1%bathy_meter(1:boundary,:) = bathy_fin_constant(1:boundary,:)
     G1%bathy_meter(:,1:boundary) = bathy_fin_constant(:,1:boundary)
     G1%bathy_meter(nxfin-boundary+1:nxfin,:) = bathy_fin_constant(nxfin-boundary+1:nxfin,:)
     G1%bathy_meter(:,nyfin-boundary+1:nyfin) = bathy_fin_constant(:,nyfin-boundary+1:nyfin)
     !                  
     CALL smooth_topo(G1%bathy_meter(boundary:nxfin-boundary+1,boundary:nyfin-boundary+1),nbiter)
     !             
     CALL meter_to_levels(G1)
     !             
     DEALLOCATE(bathy_fin_constant)
     !
     !
     !------------------------------------------------------------------
     ! ****Second option : new topo file or/and partial steps     
     !------------------------------------------------------------------ 
     !
     !
     !
     !
     !
     !
     !
     !
  ELSE
     !
     WRITE(*,*) 'Second option : partial steps'
     ! read fine and coarse grids coordinates file
     !	    
     status = Read_Coordinates(TRIM(parent_coordinate_file),G0)
     status = Read_Coordinates(TRIM(Child_coordinates),G1,Pacifique)
     !                        
     IF( imax > SIZE(G0%nav_lon,1) .OR. jmax > SIZE(G0%nav_lon,2) .OR. &
          imax <= imin .OR. jmax <= jmin ) THEN                    
        WRITE(*,*) 'ERROR ***** bad child grid definition ...'
        WRITE(*,*) 'please check imin,jmin,imax,jmax,jpizoom,jpjzoom values'       
        WRITE(*,*) ' '
        STOP
     ENDIF
     !

     !  
     IF( SIZE(G1%nav_lon,1) .NE. nxfin .OR. SIZE(G1%nav_lon,2) .NE. nyfin ) THEN
        !
        WRITE(*,*) 'ERROR ***** bad child coordinates file ...'
        WRITE(*,*) ' '
        WRITE(*,*) 'please check that child coordinates file '
        WRITE(*,*) 'has been created with the same namelist '
        WRITE(*,*) ' '
        STOP
        !
     ENDIF
     !      
     ! read coarse grid bathymetry file
     !---
     IF( new_topo ) THEN
        WRITE(*,*) 'use new topo file : ',TRIM(elevation_database)
        DEALLOCATE( G0%nav_lon, G0%nav_lat )
        status = Read_bathy_meter(TRIM(elevation_database),G0,G1,Pacifique)
     ELSE
        WRITE(*,*) 'no new topo file'
        status = Read_Bathymeter(TRIM(parent_bathy_meter),G0)
        IF(Pacifique) THEN
           WHERE(G0%nav_lon < 0.001) 
              G0%nav_lon = G0%nav_lon + 360.
           END WHERE
        ENDIF
     ENDIF
     !---            
     numlatfine =  SIZE(G1%nav_lat,2)
     numlonfine =  SIZE(G1%nav_lat,1) 
     !  
     !               
     ALLOCATE(G1%bathy_meter(nxfin,nyfin))
     G1%bathy_meter(:,:)=0.                       

     WRITE(*,*) 'Interpolation of high resolution bathymetry on child grid'

     IF( type_bathy_interp == 0 ) THEN
        WRITE(*,*) 'Arithmetic average ...'
     ELSE IF( type_bathy_interp == 1 ) THEN
        WRITE(*,*) 'Median average ...'
     ELSE IF( type_bathy_interp == 2 ) THEN     
        WRITE(*,*) 'Bilinear interpolation ...'
     ELSE     
        WRITE(*,*) 'bad value for type_bathy_interp variable ( must be 0,1 or 2 )'
        STOP 
     ENDIF
     !
     !************************************
     !MEDIAN AVERAGE or ARITHMETIC AVERAGE
     !************************************
     !
     IF( type_bathy_interp == 0 .OR. type_bathy_interp == 1 ) THEN 
        !
        ALLOCATE(trouble_points(nxfin,nyfin))
        trouble_points = 0
        !
        !  POINT DETECTION
        !
        !                       
        DO jj = 2,numlatfine
           DO ji = 2,numlonfine
              !	    
              ! FINE GRID CELLS DEFINITION               
              !
              Cell_lonmin = MIN(G1%glamf(ji-1,jj-1),G1%glamf(ji,jj-1),G1%glamf(ji,jj),G1%glamf(ji-1,jj))
              Cell_lonmax = MAX(G1%glamf(ji-1,jj-1),G1%glamf(ji,jj-1),G1%glamf(ji,jj),G1%glamf(ji-1,jj))
              Cell_latmin = MIN(G1%gphif(ji-1,jj-1),G1%gphif(ji,jj-1),G1%gphif(ji,jj),G1%gphif(ji-1,jj))
              Cell_latmax = MAX(G1%gphif(ji-1,jj-1),G1%gphif(ji,jj-1),G1%gphif(ji,jj),G1%gphif(ji-1,jj))	                   
              !               
              ! SEARCH FOR ALL POINTS CONTAINED IN THIS CELL
              !
              iimin = 1
              DO WHILE( G0%nav_lon(iimin,1) < Cell_lonmin ) 
                 iimin = iimin + 1
              ENDDO
              !               
              jjmin = 1
              DO WHILE( G0%nav_lat(iimin,jjmin) < Cell_latmin ) 
                 jjmin = jjmin + 1
              ENDDO
              !                
              iimax = iimin 
              DO WHILE( G0%nav_lon(iimax,1) <= Cell_lonmax ) 
                 iimax = iimax + 1
              ENDDO
              !                               
              jjmax = jjmin 
              DO WHILE( G0%nav_lat(iimax,jjmax) <= Cell_latmax ) 
                 jjmax = jjmax + 1
              ENDDO
              !
              iimax = iimax-1
              jjmax = jjmax-1
              !               
              iimin = MAX( iimin,1 )
              jjmin = MAX( jjmin,1 )
              iimax = MIN( iimax,SIZE(G0%bathy_meter,1))
              jjmax = MIN( jjmax,SIZE(G0%bathy_meter,2))

              nxhr = iimax - iimin + 1
              nyhr = jjmax - jjmin + 1                    

              IF( nxhr == 0 .OR. nyhr == 0 ) THEN
                 trouble_points(ji,jj) = 1
              ELSE

                 ALLOCATE( vardep(nxhr,nyhr) )
                 ALLOCATE( mask_oce(nxhr,nyhr) )
                 mask_oce = 0	       

                 vardep(:,:) = G0%bathy_meter(iimin:iimax,jjmin:jjmax)

                 WHERE( vardep(:,:) .GT. 0. )  mask_oce = 1

                 IF( SUM(mask_oce) == 0 ) THEN
                    G1%bathy_meter(ji,jj) = 0.
                 ELSE
                    IF( type_bathy_interp == 0 ) THEN
                       ! Arithmetic average                   
                       G1%bathy_meter(ji,jj) = SUM (vardep(:,:)*mask_oce(:,:))/SUM(mask_oce)
                    ELSE
                       ! Median average		    
                       !
                       vardep(:,:) = vardep(:,:)*mask_oce(:,:) - 100000*(1-mask_oce(:,:))
                       ALLOCATE(vardep1d(nxhr*nyhr))
                       vardep1d = RESHAPE(vardep,(/ nxhr*nyhr /) )
                       CALL ssort(vardep1d,nxhr*nyhr)
                       !
                       ! Calculate median
                       !
                       IF (MOD(SUM(mask_oce),2) .NE. 0) THEN
                          G1%bathy_meter(ji,jj) = vardep1d( SUM(mask_oce)/2 + 1)
                       ELSE
                          G1%bathy_meter(ji,jj) = ( vardep1d(SUM(mask_oce)/2) + vardep1d(SUM(mask_oce)/2+1) )/2.0
                       END IF
                       DEALLOCATE(vardep1d)		   
                    ENDIF
                 ENDIF
                 DEALLOCATE (mask_oce,vardep)

              ENDIF
           ENDDO
        ENDDO

        IF( SUM( trouble_points ) > 0 ) THEN
           PRINT*,'too much empty cells, proceed to bilinear interpolation !!'
           type_bathy_interp = 2
        ENDIF

     ENDIF

     !
     ! create logical array masksrc
     !
     IF( type_bathy_interp == 2) THEN 
        !

        !            
        identical_grids = .FALSE.

        IF( SIZE(G0%nav_lat,1) == SIZE(G1%nav_lat,1)  .AND.   &
             SIZE(G0%nav_lat,2) == SIZE(G1%nav_lat,2)  .AND.   &
             SIZE(G0%nav_lon,1) == SIZE(G1%nav_lon,1)  .AND.   &
             SIZE(G0%nav_lon,2) == SIZE(G1%nav_lon,2)   ) THEN
           IF( MAXVAL( ABS(G0%nav_lat(:,:)- G1%nav_lat(:,:)) ) < 0.0001 .AND.   &
                MAXVAL( ABS(G0%nav_lon(:,:)- G1%nav_lon(:,:)) ) < 0.0001 ) THEN
              G1%bathy_meter = G0%bathy_meter 
              PRINT*,'same grid between ',elevation_database,' and child domain'    
              identical_grids = .TRUE.                          
           ENDIF
        ENDIF


        IF( .NOT. identical_grids ) THEN 

           ALLOCATE(masksrc(SIZE(G0%bathy_meter,1),SIZE(G0%bathy_meter,2)))
           ALLOCATE(bathy_test(nxfin,nyfin))
           !
           !                    Where(G0%bathy_meter.le.0.00001) 
           !	                masksrc = .false.
           !	            ElseWhere
           !
           masksrc = .TRUE.
           !
           !	            End where                       
           !            
           ! compute remapping matrix thanks to SCRIP package            
           !                                  
           CALL get_remap_matrix(G0%nav_lat,G1%nav_lat,   &
                G0%nav_lon,G1%nav_lon,   &
                masksrc,matrix,src_add,dst_add)
           CALL make_remap(G0%bathy_meter,bathy_test,nxfin,nyfin, &
                matrix,src_add,dst_add)  
           !                                  
           G1%bathy_meter = bathy_test               
           !            
           DEALLOCATE(masksrc)
           DEALLOCATE(bathy_test) 

        ENDIF
        !            
     ENDIF
     !
     !
     !
     !------------------------------------------------------------------------------------------
     ! ! ****Third  option : Partial Steps
     ! The code includes the 
     ! option to include partial cells at the bottom 
     ! in order to better resolve topographic variations
     !------------------------------------------------------------------------------------------
     !
     ! At this step bathymetry in meters has already been interpolated on fine grid
     !
     ! 	                
     IF( partial_steps ) THEN                
        !                  
        status = Read_Bathymeter(TRIM(parent_bathy_meter),G0)
        DEALLOCATE(G0%nav_lat,G0%nav_lon)
        status = Read_coordinates(TRIM(parent_coordinate_file),G0)
        !------------------------                  

        IF (.NOT.ASSOCIATED(G0%gdepw_ps)) &
             ALLOCATE(G0%gdepw_ps(SIZE(G0%bathy_meter,1),SIZE(G0%bathy_meter,2)))
        IF (.NOT.ASSOCIATED(G1%gdepw_ps)) &
             ALLOCATE(G1%gdepw_ps(SIZE(G1%bathy_meter,1),SIZE(G1%bathy_meter,2)))                  
        IF (.NOT.ASSOCIATED(gdepw_ps_interp)) &
             ALLOCATE(gdepw_ps_interp(SIZE(G1%bathy_meter,1),SIZE(G1%bathy_meter,2)))
        !
        !                       
        WRITE(*,*) 'Coarse grid : '
        CALL get_partial_steps(G0) 
        WRITE(*,*) ' '
        WRITE(*,*) 'Fine grid : '
        CALL get_partial_steps(G1)                 ! compute gdepw_ps for G1
        CALL bathymetry_control(G0%Bathy_level)    !    
        CALL Check_interp(G0,gdepw_ps_interp)      ! interpolation in connection zone (3 coarse grid cells)
        !
        boundary = connectionsize*irafx + nbghostcellsfine                     
        G1%gdepw_ps(1:boundary,:) = gdepw_ps_interp(1:boundary,:)
        G1%gdepw_ps(:,1:boundary) = gdepw_ps_interp(:,1:boundary)
        G1%gdepw_ps(nxfin-boundary+1:nxfin,:) = gdepw_ps_interp(nxfin-boundary+1:nxfin,:)
        G1%gdepw_ps(:,nyfin-boundary+1:nyfin) = gdepw_ps_interp(:,nyfin-boundary+1:nyfin)


        !                   

        IF(.NOT. smoothing) WRITE(*,*) 'No smoothing process only connection is carried out'
        WRITE(*,*) ' linear connection on ',nb_connection_pts,'coarse grid points'

        connectionsize = 3 + nb_connection_pts 
        !            
        gdepw_ps_interp = 0.
        CALL Check_interp(G0,gdepw_ps_interp)      ! interpolation in connection zone (3 coarse grid cells)
        !
        !
        !
        !
        !                    LINEAR CONNECTION
        !
        !
        !
        !
        !
        wghts = 1.
        DO ji = boundary + 1 , boundary + nb_connection_pts * irafx
           G1%gdepw_ps(ji,boundary+1:nyfin-boundary) =                                          &
                (1.-wghts) * G1%gdepw_ps(ji,boundary+1:nyfin-boundary) +                          &
                wghts * gdepw_ps_interp(ji,boundary+1:nyfin-boundary)
           wghts = wghts - (1. / (nb_connection_pts*irafx - 1. ) ) 
        ENDDO

        wghts = 1.
        DO ji = nxfin - boundary , nxfin - boundary - nb_connection_pts * irafx + 1 ,-1 
           G1%gdepw_ps(ji,boundary+1:nyfin-boundary) =                                            &
                (1. - wghts) * G1%gdepw_ps(ji,boundary+1:nyfin-boundary) +                          &
                wghts * gdepw_ps_interp(ji,boundary+1:nyfin-boundary)
           wghts = wghts - (1. / ( (nb_connection_pts*irafx) - 1. ) )                      
        ENDDO
        !                     
        wghts = 1.
        DO jj = boundary + 1 , boundary + nb_connection_pts * irafy
           G1%gdepw_ps(boundary + nb_connection_pts * irafx + 1: &
                nxfin - boundary - nb_connection_pts * irafx ,jj) =          &
                (1. - wghts) * G1%gdepw_ps(boundary + nb_connection_pts * irafx + 1:  &
                nxfin - boundary - nb_connection_pts * irafx,jj) +                          &
                wghts * gdepw_ps_interp(boundary + nb_connection_pts * irafx + 1:   &
                nxfin - boundary - nb_connection_pts * irafx,jj)
           wghts = wghts - (1. / (nb_connection_pts*irafx - 1. ) )                      
        ENDDO
        !                    
        wghts = 1.
        DO jj = nyfin - boundary , nyfin - boundary - nb_connection_pts * irafy+ 1 , -1
           G1%gdepw_ps(boundary + nb_connection_pts * irafx + 1: &
                nxfin - boundary - nb_connection_pts * irafx ,jj) =                      &
                (1. - wghts) * G1%gdepw_ps(boundary + nb_connection_pts * irafx + 1: &
                nxfin - boundary - nb_connection_pts * irafx,jj) +                          &
                wghts * gdepw_ps_interp(boundary + nb_connection_pts * irafx + 1: &
                nxfin - boundary - nb_connection_pts * irafx,jj)
           wghts = wghts - (1. / (nb_connection_pts*irafx - 1. ) )   
        ENDDO

        G1%bathy_meter = G1%gdepw_ps
        !                     
        connectionsize = 3
        ! 
        IF(smoothing) THEN 

           !
           ! Smoothing to connect the connection zone (3 + nb_connection_pts coarse grid cells) and the interior domain
           !
           boundary = (connectionsize+nb_connection_pts)*irafx + nbghostcellsfine 
           CALL smooth_topo(G1%gdepw_ps(boundary:nxfin-boundary+1,boundary:nyfin-boundary+1),nbiter)
           G1%bathy_meter = G1%gdepw_ps                         
        ENDIF


        !
        !  
        ! Remove closed seas
        !                            
        IF (removeclosedseas) THEN
           ALLOCATE(bathy_test(nxfin,nyfin))
           bathy_test=0.
           WHERE (G1%bathy_meter(1,:).GT.0.)
              bathy_test(1,:)=1
           END WHERE
           WHERE (G1%bathy_meter(nxfin,:).GT.0.)
              bathy_test(nxfin,:)=1
           END WHERE
           WHERE (G1%bathy_meter(:,1).GT.0.)
              bathy_test(:,1)=1
           END WHERE
           WHERE (G1%bathy_meter(:,nyfin).GT.0.)
              bathy_test(:,nyfin)=1
           END WHERE
           nbadd = 1
           DO WHILE (nbadd.NE.0)
              nbadd = 0
              DO j=2,nyfin-1
                 DO i=2,nxfin-1
                    IF (G1%bathy_meter(i,j).GT.0.) THEN
                       IF (MAX(bathy_test(i,j+1),bathy_test(i,j-1), &
                            bathy_test(i-1,j),bathy_test(i+1,j)).EQ.1) THEN
                          IF (bathy_test(i,j).NE.1.) nbadd = nbadd + 1
                          bathy_test(i,j)=1.
                       ENDIF

                    ENDIF
                 ENDDO
              ENDDO
           ENDDO
           WHERE (bathy_test.EQ.0.)
              G1%bathy_meter = 0.
           END WHERE
           DEALLOCATE(bathy_test)
        ENDIF
        !
        IF(bathy_update) CALL Update_Parent_Bathy( G0,G1 )                  
        !
        CALL set_child_name(parent_bathy_meter,child_ps)
        status = Write_Bathy_meter(TRIM(child_ps),G1)        

        IF(bathy_update) status = Write_Bathy_meter(TRIM(updated_parent_file),G0)

        CALL get_partial_steps(G1)
        !
        G1%bathy_level=NINT(G1%bathy_level)
        !
        IF( TRIM(parent_meshmask_file) .NE. '/NULL' ) &
             status = Write_Bathy_level(TRIM(Childlevel_file),G1)
        !
        WRITE(*,*) '****** Bathymetry successfully created for partial cells ******'
        WRITE(*,*) ' '
        !
        STOP         
     ENDIF
     !            
     !--------------------------------end if partial steps------------------------------------
     !
     !
     status = Read_bathy_level(TRIM(parent_meshmask_file),G0)
     !            
     CALL levels_to_meter(G0)
     ! 
     ! compute constant bathymetry for Parent-Child bathymetry connection
     !              
     WHERE( G0%bathy_meter < 0.000001 ) G0%bathy_meter = 0.

     CALL init_constant_bathy(G0%bathy_meter,bathy_fin_constant)
     !
     boundary = connectionsize*irafx + nbghostcellsfine   
     !             
     G1%bathy_meter(1:boundary,:) = bathy_fin_constant(1:boundary,:)
     G1%bathy_meter(:,1:boundary) = bathy_fin_constant(:,1:boundary)
     G1%bathy_meter(nxfin-boundary+1:nxfin,:) = bathy_fin_constant(nxfin-boundary+1:nxfin,:)
     G1%bathy_meter(:,nyfin-boundary+1:nyfin) = bathy_fin_constant(:,nyfin-boundary+1:nyfin)
     !
     ! bathymetry smoothing
     !                  
     CALL smooth_topo(G1%bathy_meter(boundary:nxfin-boundary+1,boundary:nyfin-boundary+1),nbiter)
     !
     ! convert bathymetry from meters to levels
     !
     CALL meter_to_levels(G1) 
     !           
     DEALLOCATE(G1%bathy_meter)           
     !	     	     
  ENDIF
  !
  !
  ! make connection thanks to constant and interpolated bathymetry
  !
  !      
  G1%bathy_level=NINT(G1%bathy_level)
  !	    
  DO j=1,nyfin
     DO i=1,nxfin
        IF (g1%bathy_level(i,j).LT.0.) THEN
           PRINT *,'error in ',i,j,g1%bathy_level(i,j)
        ENDIF
     ENDDO
  ENDDO
  !	    
  WHERE ((G1%bathy_level.LT.3.).AND.(G1%bathy_level.GT.0.))
     G1%bathy_level=3
  END WHERE
  !
  ! possibility to remove closed seas
  !      
  IF (removeclosedseas) THEN
     ALLOCATE(bathy_test(nxfin,nyfin))

     bathy_test=0.
     WHERE (G1%bathy_level(1,:).GT.0.)
        bathy_test(1,:)=1
     END WHERE

     WHERE (G1%bathy_level(nxfin,:).GT.0.)
        bathy_test(nxfin,:)=1
     END WHERE

     WHERE (G1%bathy_level(:,1).GT.0.)
        bathy_test(:,1)=1
     END WHERE

     WHERE (G1%bathy_level(:,nyfin).GT.0.)
        bathy_test(:,nyfin)=1
     END WHERE

     nbadd = 1

     DO WHILE (nbadd.NE.0)
        nbadd = 0
        DO j=2,nyfin-1
           DO i=2,nxfin-1
              IF (G1%bathy_level(i,j).GT.0.) THEN
                 IF (MAX(bathy_test(i,j+1),bathy_test(i,j-1),bathy_test(i-1,j),bathy_test(i+1,j)).EQ.1) THEN
                    IF (bathy_test(i,j).NE.1.) nbadd = nbadd + 1
                    bathy_test(i,j)=1.
                 ENDIF

              ENDIF
           ENDDO
        ENDDO

     ENDDO

     WHERE (bathy_test.EQ.0.)
        G1%bathy_level = 0.
     END WHERE
     DEALLOCATE(bathy_test)           
  ENDIF


  !
  ! store interpolation result in output file
  !
  status = Write_Bathy_level(TRIM(Childlevel_file),G1)

  WRITE(*,*) '****** Bathymetry successfully created for full cells ******'
  WRITE(*,*) ' '

  STOP
END PROGRAM create_bathy


