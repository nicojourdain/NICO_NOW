program modif                                         
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! N. Jourdain, July 2014, CCRC-UNSW Sydney
!              Sep. 2014 -> fix error in e1,e2 calculation.
!              Oct. 2014 -> take lakemask into account (WRFv3.6)
!                           (lake points are not land but should not be coupled to NEMO)
!
! This scripts interpolates the bathymetry from ETOPO1 
! onto WRF's nest and make a transition with parent bathymetry. 
!
! It keeps the LANDMASK/LAKEMASK of WRF_d02 inside the domain and the 
! LANDMASK/LAKEMASK of WRF_d01 on a (3*pgr+2)-point wide halo.
!
! NB: The ASCII file AGRIF_FixedGrids.in should be :
!       1
!       IPS IPE JPS JPE RRR RRR RRR
!       0
!     with with theses values from WRF (see netcdf metadata):
!       IPS = i_parent_start - 5 ( +1 if parent grid is Eat-West periodic )
!       IPE = i_parent_end   + 2 ( +1 if parent grid is Eat-West periodic )
!       JPS = j_parent_start - 5
!       JPE = j_parent_end   + 2
!       RRR = parent_grid_ratio
!
! NB: as NEMO's child is larger than WRF's child, you need to create an
!     extended version of geo_em.d02 before running this script.
!     (with a domain by 2*(3*pgr+3) larger than the original one)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE netcdf                                            
                                           
IMPLICIT NONE                                         

!-- namelist parameters :
namelist /datactl/ file_etopo, file_runoff, file_chloro, file_tides
namelist /parent/ conf_par, perio, max_dom, feedback, idateline, file_par_coord, file_eff_land
namelist /child/ conf_child, file_child_coord, file_child_extend
INTEGER                               :: max_dom, feedback, perio, idateline
CHARACTER(LEN=150)                    :: file_par_coord, file_eff_land
CHARACTER(LEN=50)                     :: conf_par
CHARACTER(LEN=150)                    :: file_etopo, file_runoff, file_chloro, file_tides
CHARACTER(LEN=150)                    :: conf_child, file_child_coord, file_child_extend
                                        
!-- WRF coordinates :
INTEGER                               :: fidWG, status, dimID_south_north_stag, dimID_west_east_stag, dimID_west_east,  &
&                                        dimID_south_north, dimID_TimeW, msouth_north_stag, mwest_east_stag, mwest_east,& 
&                                        msouth_north, mTimeW, XLONG_V_ID, XLONG_U_ID, XLONG_M_ID, XLAT_V_ID, XLAT_U_ID,&
&                                        XLAT_M_ID, LANDMASK_ID, mwest_east_par, msouth_north_par, mTimeW_par, ilake,   &
&                                        LAKEMASK_ID 
REAL*4,ALLOCATABLE,DIMENSION(:,:,:)   :: XLONG_V, XLONG_U, XLONG_M, XLAT_V, XLAT_U, XLAT_M, LANDMASK, LANDMASK_parent,  &
&                                        LAKEMASK, LAKEMASK_parent
                                           
!-- ETOPO :         
INTEGER                               :: fidtopo, dimID_lat, dimID_lon, mlat, mlon, z_ID, lon_ID, lat_ID 
REAL*4,ALLOCATABLE,DIMENSION(:)       :: lat
REAL*8,ALLOCATABLE,DIMENSION(:)       :: lon
INTEGER*2,ALLOCATABLE,DIMENSION(:,:)  :: z

!-- coordinates (output) :
INTEGER                               :: dimID_x, dimID_y, dimID_z, dimID_time, mx, my, mz, mtime, gphiv_ID, gphiu_ID,&
&                                        gphit_ID, gphif_ID, glamv_ID, glamu_ID, glamt_ID, glamf_ID, e2v_ID, e2u_ID,  &
&                                        e2t_ID, e2f_ID, e1v_ID, e1u_ID, e1t_ID, e1f_ID, time_ID, nav_lon_ID,         &
&                                        nav_lev_ID, nav_lat_ID, fidout 
CHARACTER(LEN=150)                    :: file_coord_out                     
REAL*4,ALLOCATABLE,DIMENSION(:)       :: time, nav_lev           
REAL*4,ALLOCATABLE,DIMENSION(:,:)     :: nav_lon, nav_lat         
REAL*8,ALLOCATABLE,DIMENSION(:,:,:,:) :: gphiv, gphiu, gphit, gphif, glamv, glamu, glamt, glamf,        &
&                                        e2v, e2u, e2t, e2f, e1v, e1u, e1t, e1f
REAL*8,ALLOCATABLE,DIMENSION(:,:)     :: diphit, diphiu, diphiv, diphif, dilamt, dilamu, dilamv, dilamf,&
&                                        djphit, djphiu, djphiv, djphif, djlamt, djlamu, djlamv, djlamf

!-- bathymetry (output) :

INTEGER*4,ALLOCATABLE,DIMENSION(:,:)  :: ntpts, nland
REAL*4,ALLOCATABLE,DIMENSION(:,:)     :: bathy, omsk, tmpba
INTEGER                               :: fidbath, Bathymetry_ID
CHARACTER(LEN=150)                    :: file_bathy_out

!--

INTEGER                               :: ji, jj, jk, iet, jet, jetmin, jetmax, ifill, nsmooth, ns, rs, kf, sumnghb

REAL*8                                :: ra, &  ! earth radius (meter)
&                                        rad    ! deg to rad conversion                                           

!-

INTEGER :: fidbatmo, mymo, mxmo, bathy_parent_ID
                                                      
CHARACTER(LEN=100) :: file_bathy_parent
                                                      
REAL*4,ALLOCATABLE,DIMENSION(:,:) :: bathy_parent

INTEGER :: i_parent_start, j_parent_start, i_parent_end, j_parent_end, pgr
         
!=================================================================================
! 0- Initializations
!=================================================================================

!- read namelist values
OPEN (UNIT=1, FILE='namelist_precpl' )
READ (UNIT=1, NML=datactl)
READ (UNIT=1, NML=parent)
READ (UNIT=1, NML=child)
CLOSE(1)
         
!- Parent Bathymetry (needed for the transition [to have partial step bathymetry])
write(file_bathy_parent,211) TRIM(conf_par)
211 FORMAT('mesh_mask_',a,'.nc')

!- Output files :
write(file_coord_out,212) TRIM(conf_par)
212 FORMAT('1_coordinates_',a,'.nc')
write(file_bathy_out,213) TRIM(conf_par)
213 FORMAT('1_bathy_meter_',a,'.nc')

!- earth radius (meter), must agree with NEMO file phycst.F90
ra = 6371229.0
  
!- deg to rad conversion (same as phycst.F90)
rad = 3.141592653589793 / 180.0
  
!=================================================================================
! 1a- Read WRF child coordinates 
!=================================================================================           
  
  write(*,*) 'Reading ', TRIM(file_child_extend)
                                   
  status = NF90_OPEN(TRIM(file_child_extend),0,fidWG)          
  call erreur(status,.TRUE.,"Open WRF child coordinates netcdf file") 
                                                  
 !- Read domensions IDs :
  status = NF90_INQ_DIMID(fidWG,"south_north_stag",dimID_south_north_stag)
  call erreur(status,.TRUE.,"inq_dimID_south_north_stag")
  status = NF90_INQ_DIMID(fidWG,"west_east_stag",dimID_west_east_stag)
  call erreur(status,.TRUE.,"inq_dimID_west_east_stag")
  status = NF90_INQ_DIMID(fidWG,"west_east",dimID_west_east)
  call erreur(status,.TRUE.,"inq_dimID_west_east")
  status = NF90_INQ_DIMID(fidWG,"south_north",dimID_south_north)
  call erreur(status,.TRUE.,"inq_dimID_south_north")
  status = NF90_INQ_DIMID(fidWG,"Time",dimID_TimeW)
  call erreur(status,.TRUE.,"inq_dimID_TimeW")
                                                      
 !- Read dimensions values :
  status = NF90_INQUIRE_DIMENSION(fidWG,dimID_south_north_stag,len=msouth_north_stag)
  call erreur(status,.TRUE.,"inq_dim_south_north_stag")
  status = NF90_INQUIRE_DIMENSION(fidWG,dimID_west_east_stag,len=mwest_east_stag)
  call erreur(status,.TRUE.,"inq_dim_west_east_stag")
  status = NF90_INQUIRE_DIMENSION(fidWG,dimID_west_east,len=mwest_east)
  call erreur(status,.TRUE.,"inq_dim_west_east")
  status = NF90_INQUIRE_DIMENSION(fidWG,dimID_south_north,len=msouth_north)
  call erreur(status,.TRUE.,"inq_dim_south_north")
  status = NF90_INQUIRE_DIMENSION(fidWG,dimID_TimeW,len=mTimeW)
  call erreur(status,.TRUE.,"inq_dim_Time")
                      
  ALLOCATE(  XLONG_V     (mwest_east     ,msouth_north_stag,mTimeW)   ) 
  ALLOCATE(  XLONG_U     (mwest_east_stag,msouth_north     ,mTimeW)   ) 
  ALLOCATE(  XLONG_M     (mwest_east     ,msouth_north     ,mTimeW)   ) 
  ALLOCATE(  XLAT_V      (mwest_east     ,msouth_north_stag,mTimeW)   ) 
  ALLOCATE(  XLAT_U      (mwest_east_stag,msouth_north     ,mTimeW)   ) 
  ALLOCATE(  XLAT_M      (mwest_east     ,msouth_north     ,mTimeW)   ) 
  ALLOCATE(  LANDMASK    (mwest_east     ,msouth_north     ,mTimeW)   ) 
  ALLOCATE(  LAKEMASK    (mwest_east     ,msouth_north     ,mTimeW)   )           

 !- Read variables IDs :
  status = NF90_INQ_VARID(fidWG,"XLONG_V",XLONG_V_ID)
  call erreur(status,.TRUE.,"inq_XLONG_V_ID")
  status = NF90_INQ_VARID(fidWG,"XLONG_U",XLONG_U_ID)
  call erreur(status,.TRUE.,"inq_XLONG_U_ID")
  status = NF90_INQ_VARID(fidWG,"XLONG",XLONG_M_ID)
  call erreur(status,.TRUE.,"inq_XLONG_M_ID")
  status = NF90_INQ_VARID(fidWG,"XLAT_V",XLAT_V_ID)
  call erreur(status,.TRUE.,"inq_XLAT_V_ID")
  status = NF90_INQ_VARID(fidWG,"XLAT_U",XLAT_U_ID)
  call erreur(status,.TRUE.,"inq_XLAT_U_ID")
  status = NF90_INQ_VARID(fidWG,"XLAT",XLAT_M_ID)
  call erreur(status,.TRUE.,"inq_XLAT_M_ID")
  status = NF90_INQ_VARID(fidWG,"LANDMASK",LANDMASK_ID)
  call erreur(status,.TRUE.,"inq_LANDMASK_ID")
  ilake = NF90_INQ_VARID(fidWG,"LAKEMASK",LAKEMASK_ID)
  if ( ilake .ne. 0 ) then
     write(*,*) '  WARNING: no LAKEMASK variable in wrfinput_d02 ===> assuming no lake'
  else
     write(*,*) '  Using LAKEMASK information'
  endif                 
                                    
 !- Read variables values :
  status = NF90_GET_VAR(fidWG,XLONG_V_ID,XLONG_V)
  call erreur(status,.TRUE.,"getvar_XLONG_V")
  status = NF90_GET_VAR(fidWG,XLONG_U_ID,XLONG_U)
  call erreur(status,.TRUE.,"getvar_XLONG_U")
  status = NF90_GET_VAR(fidWG,XLONG_M_ID,XLONG_M)
  call erreur(status,.TRUE.,"getvar_XLONG_M")
  status = NF90_GET_VAR(fidWG,XLAT_V_ID,XLAT_V)
  call erreur(status,.TRUE.,"getvar_XLAT_V")
  status = NF90_GET_VAR(fidWG,XLAT_U_ID,XLAT_U)
  call erreur(status,.TRUE.,"getvar_XLAT_U")
  status = NF90_GET_VAR(fidWG,XLAT_M_ID,XLAT_M)
  call erreur(status,.TRUE.,"getvar_XLAT_M")
  status = NF90_GET_VAR(fidWG,LANDMASK_ID,LANDMASK)
  call erreur(status,.TRUE.,"getvar_LANDMASK")
  if ( ilake .eq. 0 ) then
    status = NF90_GET_VAR(fidWG,LAKEMASK_ID,LAKEMASK)
    call erreur(status,.TRUE.,"getvar_LAKEMASK")
  else
    LAKEMASK(:,:,:) = 0.0
  endif

 !- Get some global attributes                                            
  status = NF90_GET_ATT(fidWG,NF90_GLOBAL,"I_PARENT_START",i_parent_start)
  call erreur(status,.TRUE.,"get_att_i_parent_start")
  status = NF90_GET_ATT(fidWG,NF90_GLOBAL,"J_PARENT_START",j_parent_start)
  call erreur(status,.TRUE.,"get_att_j_parent_start")
  status = NF90_GET_ATT(fidWG,NF90_GLOBAL,"PARENT_GRID_RATIO",pgr)
  call erreur(status,.TRUE.,"get_att_parent_grid_ratio")
  i_parent_end=i_parent_start-1+mwest_east/pgr
  j_parent_end=j_parent_start-1+msouth_north/pgr
  write(*,*) '  ---- EXTENDED child grid coordinates in parent grid :'
  write(*,*) '      i= ', i_parent_start, 'to ', i_parent_end
  write(*,*) '      j= ', j_parent_start, 'to ', j_parent_end


 !- CORRECTION BECAUSE WE USE EXTENDED GEOGRID FILE FOR AGRIF:
  i_parent_start=i_parent_start+4
  j_parent_start=j_parent_start+4 
  i_parent_end=i_parent_end-4
  j_parent_end=j_parent_end-4
  write(*,*) '  ---- ACTUAL child grid coordinates in parent grid :'
  write(*,*) '      i= ', i_parent_start, 'to ', i_parent_end
  write(*,*) '      j= ', j_parent_start, 'to ', j_parent_end

 !- End reading                    
  status = NF90_CLOSE(fidWG)                      
  call erreur(status,.TRUE.,"End of Reading child coordinates")     

!=================================================================================
! 1b- Read WRF parent coordinates 
!=================================================================================           
  
  write(*,*) 'Reading ', TRIM(file_par_coord)
                                   
  status = NF90_OPEN(TRIM(file_par_coord),0,fidWG)          
  call erreur(status,.TRUE.,"Open WRF parent coordinates netcdf file") 
                                                  
 !- Read dimension IDs : 
  status = NF90_INQ_DIMID(fidWG,"west_east",dimID_west_east)
  call erreur(status,.TRUE.,"inq_dimID_west_east")
  status = NF90_INQ_DIMID(fidWG,"south_north",dimID_south_north)
  call erreur(status,.TRUE.,"inq_dimID_south_north")
  status = NF90_INQ_DIMID(fidWG,"Time",dimID_TimeW)
  call erreur(status,.TRUE.,"inq_dimID_TimeW")
                                                      
 !- Read dimensions values :
  status = NF90_INQUIRE_DIMENSION(fidWG,dimID_west_east,len=mwest_east_par)
  call erreur(status,.TRUE.,"inq_dim_west_east")
  status = NF90_INQUIRE_DIMENSION(fidWG,dimID_south_north,len=msouth_north_par)
  call erreur(status,.TRUE.,"inq_dim_south_north")
  status = NF90_INQUIRE_DIMENSION(fidWG,dimID_TimeW,len=mTimeW_par)
  call erreur(status,.TRUE.,"inq_dim_Time")
                      
  ALLOCATE(  LANDMASK_parent (mwest_east_par,msouth_north_par,mTimeW_par)   ) 
  ALLOCATE(  LAKEMASK_parent (mwest_east_par,msouth_north_par,mTimeW_par)   )           

 !- Read variables IDs
  status = NF90_INQ_VARID(fidWG,"LANDMASK",LANDMASK_ID)
  call erreur(status,.TRUE.,"inq_LANDMASK_ID")
  ilake = NF90_INQ_VARID(fidWG,"LAKEMASK",LAKEMASK_ID)
  if ( ilake .ne. 0 ) then
     write(*,*) '  WARNING: no LAKEMASK variable in wrfinput_d01 ===> assuming no lake'
  else
     write(*,*) '  Using LAKEMASK information'
  endif 
                                                    
 !- Read variables IDs
  status = NF90_GET_VAR(fidWG,LANDMASK_ID,LANDMASK_parent)
  call erreur(status,.TRUE.,"getvar_LANDMASK")
  if ( ilake .eq. 0 ) then
    status = NF90_GET_VAR(fidWG,LAKEMASK_ID,LAKEMASK_parent)
    call erreur(status,.TRUE.,"getvar_LAKEMASK")
  else
    LAKEMASK_parent(:,:,:) = 0.0
  endif

 !- Close file                 
  status = NF90_CLOSE(fidWG)                      
  call erreur(status,.TRUE.,"End reading WRF parent coordinates")     

!=================================================================================
! 2- Read bathymetry from ETOPO1
!=================================================================================

  write(*,*) 'Reading ', TRIM(file_etopo)

  status = NF90_OPEN(TRIM(file_etopo),0,fidtopo)          
  call erreur(status,.TRUE.,"read") 
                                               
 !- Lecture des ID des dimensions qui nous interessent
  status = NF90_INQ_DIMID(fidtopo,"lat",dimID_lat)
  call erreur(status,.TRUE.,"inq_dimID_lat")
  status = NF90_INQ_DIMID(fidtopo,"lon",dimID_lon)
  call erreur(status,.TRUE.,"inq_dimID_lon")
                                                   
 !- Lecture des valeurs des dimensions qui nous interessent
  status = NF90_INQUIRE_DIMENSION(fidtopo,dimID_lat,len=mlat)
  call erreur(status,.TRUE.,"inq_dim_lat")
  status = NF90_INQUIRE_DIMENSION(fidtopo,dimID_lon,len=mlon)
  call erreur(status,.TRUE.,"inq_dim_lon")
                   
  ALLOCATE(  z  (mlon,mlat)  ) 
  ALLOCATE(  lon(mlon     )  ) 
  ALLOCATE(  lat(     mlat)  ) 
                     
 !- Lecture des ID des variables qui nous interessent
  status = NF90_INQ_VARID(fidtopo,"z",z_ID)
  call erreur(status,.TRUE.,"inq_z_ID")
  status = NF90_INQ_VARID(fidtopo,"lon",lon_ID)
  call erreur(status,.TRUE.,"inq_lon_ID")
  status = NF90_INQ_VARID(fidtopo,"lat",lat_ID)
  call erreur(status,.TRUE.,"inq_lat_ID")
                                                  
 !- Lecture des valeurs des variables qui nous interessent
  status = NF90_GET_VAR(fidtopo,z_ID,z)
  call erreur(status,.TRUE.,"getvar_z")
  status = NF90_GET_VAR(fidtopo,lon_ID,lon)
  call erreur(status,.TRUE.,"getvar_lon")
  status = NF90_GET_VAR(fidtopo,lat_ID,lat)
  call erreur(status,.TRUE.,"getvar_lat")
                                          
 !- Fermeture du fichier lu                         
  status = NF90_CLOSE(fidtopo)                      
  call erreur(status,.TRUE.,"fin_lecture")     

!===================================================================================
! 3- READ BATHYMETRY OF MOTHER GRID
!===================================================================================

  status = NF90_OPEN(TRIM(file_bathy_parent),0,fidbatmo)          
  call erreur(status,.TRUE.,"read") 
                                                       
   !Lecture des ID des dimensions qui nous interessent
     status = NF90_INQ_DIMID(fidbatmo,"y",dimID_y)
     call erreur(status,.TRUE.,"inq_dimID_y")
     status = NF90_INQ_DIMID(fidbatmo,"x",dimID_x)
     call erreur(status,.TRUE.,"inq_dimID_x")
                                                           
   !Lecture des valeurs des dimensions qui nous interessent
     status = NF90_INQUIRE_DIMENSION(fidbatmo,dimID_y,len=mymo)
     call erreur(status,.TRUE.,"inq_dim_y")
     status = NF90_INQUIRE_DIMENSION(fidbatmo,dimID_x,len=mxmo)
     call erreur(status,.TRUE.,"inq_dim_x")
                           
   !Allocation of arrays : 
     ALLOCATE(  bathy_parent(mxmo,mymo)  ) 
                             
   !Lecture des ID des variables qui nous interessent
     status = NF90_INQ_VARID(fidbatmo,"hdepw",bathy_parent_ID)
     call erreur(status,.TRUE.,"inq_bathy_parent_ID")
                                                          
   !Lecture des valeurs des variables qui nous interessent
     status = NF90_GET_VAR(fidbatmo,bathy_parent_ID,bathy_parent)
     call erreur(status,.TRUE.,"getvar_bathy_parent")
                                                  
 !Fermeture du fichier lu                         
  status = NF90_CLOSE(fidbatmo)                      
  call erreur(status,.TRUE.,"fin_lecture")     

 !Put bathymetry to the same sign as ETOPO:
 bathy_parent(:,:) = - bathy_parent(:,:)

!===================================================================================
! 4- Calculate NEMO horizontal coordinates (from which NEMO will calculate mesh)
!===================================================================================

  write(*,*) ' '
  write(*,*) 'Calculate NEMO horizontal coordinates'

 !- NEMO's dimensions (we choose that NEMO's grid is (3*pgr+2)-point larger 
 !  than WRF_d02 domain at each boundary...) :
  mx = mwest_east-2    ! NB: mwest_east is the one of the extended file
  my = msouth_north-2  ! NB: msouth_north is the one of the extended file
  mz = 1               ! dummy
  mtime = 1            ! dummy

  ALLOCATE(  gphiv  (mx,my,mz,mtime)  ) 
  ALLOCATE(  gphiu  (mx,my,mz,mtime)  ) 
  ALLOCATE(  gphit  (mx,my,mz,mtime)  ) 
  ALLOCATE(  gphif  (mx,my,mz,mtime)  ) 
  ALLOCATE(  glamv  (mx,my,mz,mtime)  ) 
  ALLOCATE(  glamu  (mx,my,mz,mtime)  ) 
  ALLOCATE(  glamt  (mx,my,mz,mtime)  ) 
  ALLOCATE(  glamf  (mx,my,mz,mtime)  ) 
  ALLOCATE(  e2v    (mx,my,mz,mtime)  ) 
  ALLOCATE(  e2u    (mx,my,mz,mtime)  ) 
  ALLOCATE(  e2t    (mx,my,mz,mtime)  ) 
  ALLOCATE(  e2f    (mx,my,mz,mtime)  ) 
  ALLOCATE(  e1v    (mx,my,mz,mtime)  ) 
  ALLOCATE(  e1u    (mx,my,mz,mtime)  ) 
  ALLOCATE(  e1t    (mx,my,mz,mtime)  ) 
  ALLOCATE(  e1f    (mx,my,mz,mtime)  ) 
  ALLOCATE(  time   (         mtime)  ) 
  ALLOCATE(  nav_lon(mx,my         )  ) 
  ALLOCATE(  nav_lev(      mz      )  ) 
  ALLOCATE(  nav_lat(mx,my         )  ) 
  ALLOCATE(  diphit(mx,my), diphiu(mx,my), diphiv(mx,my), diphif(mx,my) )
  ALLOCATE(  dilamt(mx,my), dilamu(mx,my), dilamv(mx,my), dilamf(mx,my) )
  ALLOCATE(  djphit(mx,my), djphiu(mx,my), djphiv(mx,my), djphif(mx,my) )
  ALLOCATE(  djlamt(mx,my), djlamu(mx,my), djlamv(mx,my), djlamf(mx,my) )

  nav_lev(1) = 1.0 ! dummy
  time(1)    = 1.0 ! dummy

 !- WRF's coordinates on NEMO grid ( phi<->lat, lam<->lon ) :
    gphit(1:mx,1:my,1,1) = XLAT_M(2:mx+1 , 2:my+1 , 1)
    gphiu(1:mx,1:my,1,1) = XLAT_U(3:mx+2 , 2:my+1 , 1)
    gphiv(1:mx,1:my,1,1) = XLAT_V(2:mx+1 , 3:my+2 , 1)
    gphif(1:mx,1:my,1,1) = 0.5 * ( XLAT_V(2:mx+1,2:my+1,1) + XLAT_V(2:mx+1,3:my+2,1) )
    glamt(1:mx,1:my,1,1) = XLONG_M(2:mx+1 , 2:my+1 , 1)
    glamu(1:mx,1:my,1,1) = XLONG_U(3:mx+2 , 2:my+1 , 1)
    glamv(1:mx,1:my,1,1) = XLONG_V(2:mx+1 , 3:my+2 , 1)
    ! map discontinuity to be accounted for in interpolation (date line):
    where( abs(XLONG_V(2:mx+1,2:my+1,1)-XLONG_V(2:mx+1,3:my+2,1)) .gt. 10.0 ) 
        glamf(1:mx,1:my,1,1) = 0.5 * ( XLONG_V(2:mx+1,2:my+1,1) + XLONG_V(2:mx+1,3:my+2,1) + 360.0 )
    elsewhere
        glamf(1:mx,1:my,1,1) = 0.5 * ( XLONG_V(2:mx+1,2:my+1,1) + XLONG_V(2:mx+1,3:my+2,1)         )
    endwhere
    ! NEMO's longitude between -180.0 and 180.0 :
    where( glamf(:,:,1,1) .gt. 180.0 )
        glamf(:,:,1,1) = glamf(:,:,1,1) - 360.0
    endwhere

 ! i-derivatives on T grid :
  do jj=1,my
    do ji=2,mx-1
      diphit(ji,jj) = 0.5 * ( gphit(ji+1,jj,1,1) - gphit(ji-1,jj,1,1) )
      if  ( abs(glamt(ji+1,jj,1,1)-glamt(ji-1,jj,1,1)) .gt. 10.0 ) then
        dilamt(ji,jj) = 0.5 * ( 360.0 - abs ( glamt(ji+1,jj,1,1) - glamt(ji-1,jj,1,1) ) )
      else
        dilamt(ji,jj) = 0.5 * ( glamt(ji+1,jj,1,1) - glamt(ji-1,jj,1,1) )
      endif
    enddo
    !-
    diphit(1 ,jj) = gphit(2 ,jj,1,1) - gphit(1   ,jj,1,1)
    diphit(mx,jj) = gphit(mx,jj,1,1) - gphit(mx-1,jj,1,1)
    if ( abs(glamt(2,jj,1,1)-glamt(1,jj,1,1)) .gt. 10.0 ) then
      dilamt(1,jj) = 360.0 - abs ( glamt(2,jj,1,1) - glamt(1,jj,1,1) )
    else
      dilamt(1,jj) = glamt(2,jj,1,1) - glamt(1,jj,1,1)
    endif
    if ( abs(glamt(mx,jj,1,1)-glamt(mx-1,jj,1,1)) .gt. 10.0 ) then
      dilamt(mx,jj) = 360.0 - abs ( glamt(mx,jj,1,1) - glamt(mx-1,jj,1,1) )
    else
      dilamt(mx,jj) = glamt(mx,jj,1,1) - glamt(mx-1,jj,1,1)
    endif
    !-
    if ( abs(glamt(mx,jj,1,1)-glamt(mx-1,jj,1,1)) .gt. 10.0 ) then
      dilamt(mx,jj) = 360.0 - abs ( glamt(mx,jj,1,1) - glamt(mx-1,jj,1,1) )
    else
      dilamt(mx,jj) = glamt(mx,jj,1,1) - glamt(mx-1,jj,1,1)
    endif
  enddo

 !- i-derivatives on U grid :
  do jj=1,my
    do ji=2,mx-1
      diphiu(ji,jj) = 0.5 * ( gphiu(ji+1,jj,1,1) - gphiu(ji-1,jj,1,1) )
      if  ( abs(glamu(ji+1,jj,1,1)-glamu(ji-1,jj,1,1)) .gt. 10.0 ) then
        dilamu(ji,jj) = 0.5 * ( 360.0 - abs ( glamu(ji+1,jj,1,1) - glamu(ji-1,jj,1,1) ) )
      else
        dilamu(ji,jj) = 0.5 * ( glamu(ji+1,jj,1,1) - glamu(ji-1,jj,1,1) )
      endif
    enddo
    !-
    diphiu(1 ,jj) = gphiu(2 ,jj,1,1) - gphiu(1   ,jj,1,1)
    diphiu(mx,jj) = gphiu(mx,jj,1,1) - gphiu(mx-1,jj,1,1)
    if ( abs(glamu(2,jj,1,1)-glamu(1,jj,1,1)) .gt. 10.0 ) then
      dilamu(1,jj) = 360.0 - abs ( glamu(2,jj,1,1) - glamu(1,jj,1,1) )
    else
      dilamu(1,jj) = glamu(2,jj,1,1) - glamu(1,jj,1,1)
    endif
    if ( abs(glamu(mx,jj,1,1)-glamu(mx-1,jj,1,1)) .gt. 10.0 ) then
      dilamu(mx,jj) = 360.0 - abs ( glamu(mx,jj,1,1) - glamu(mx-1,jj,1,1) )
    else
      dilamu(mx,jj) = glamu(mx,jj,1,1) - glamu(mx-1,jj,1,1)
    endif
    !-
    if ( abs(glamu(mx,jj,1,1)-glamu(mx-1,jj,1,1)) .gt. 10.0 ) then
      dilamu(mx,jj) = 360.0 - abs ( glamu(mx,jj,1,1) - glamu(mx-1,jj,1,1) )
    else
      dilamu(mx,jj) = glamu(mx,jj,1,1) - glamu(mx-1,jj,1,1)
    endif
  enddo

 !- i-derivavives on V grid :
  do jj=1,my
    do ji=2,mx-1
      diphiv(ji,jj) = 0.5 * ( gphiv(ji+1,jj,1,1) - gphiv(ji-1,jj,1,1) )
      if  ( abs(glamv(ji+1,jj,1,1)-glamv(ji-1,jj,1,1)) .gt. 10.0 ) then
        dilamv(ji,jj) = 0.5 * ( 360.0 - abs ( glamv(ji+1,jj,1,1) - glamv(ji-1,jj,1,1) ) )
      else
        dilamv(ji,jj) = 0.5 * ( glamv(ji+1,jj,1,1) - glamv(ji-1,jj,1,1) )
      endif
    enddo
    !-
    diphiv(1 ,jj) = gphiv(2 ,jj,1,1) - gphiv(1   ,jj,1,1)
    diphiv(mx,jj) = gphiv(mx,jj,1,1) - gphiv(mx-1,jj,1,1)
    if ( abs(glamv(2,jj,1,1)-glamv(1,jj,1,1)) .gt. 10.0 ) then
      dilamv(1,jj) = 360.0 - abs ( glamv(2,jj,1,1) - glamv(1,jj,1,1) )
    else
      dilamv(1,jj) = glamv(2,jj,1,1) - glamv(1,jj,1,1)
    endif
    if ( abs(glamv(mx,jj,1,1)-glamv(mx-1,jj,1,1)) .gt. 10.0 ) then
      dilamv(mx,jj) = 360.0 - abs ( glamv(mx,jj,1,1) - glamv(mx-1,jj,1,1) )
    else
      dilamv(mx,jj) = glamv(mx,jj,1,1) - glamv(mx-1,jj,1,1)
    endif
    !-
    if ( abs(glamv(mx,jj,1,1)-glamv(mx-1,jj,1,1)) .gt. 10.0 ) then
      dilamv(mx,jj) = 360.0 - abs ( glamv(mx,jj,1,1) - glamv(mx-1,jj,1,1) )
    else
      dilamv(mx,jj) = glamv(mx,jj,1,1) - glamv(mx-1,jj,1,1)
    endif
  enddo
 
 !- i-derivatives on F grid :
  do jj=1,my
    do ji=2,mx-1
      diphif(ji,jj) = 0.5 * ( gphif(ji+1,jj,1,1) - gphif(ji-1,jj,1,1) )
      if  ( abs(glamf(ji+1,jj,1,1)-glamf(ji-1,jj,1,1)) .gt. 10.0 ) then
        dilamf(ji,jj) = 0.5 * ( 360.0 - abs ( glamf(ji+1,jj,1,1) - glamf(ji-1,jj,1,1) ) )
      else
        dilamf(ji,jj) = 0.5 * ( glamf(ji+1,jj,1,1) - glamf(ji-1,jj,1,1) )
      endif
    enddo
    !-
    diphif(1 ,jj) = gphif(2 ,jj,1,1) - gphif(1   ,jj,1,1)
    diphif(mx,jj) = gphif(mx,jj,1,1) - gphif(mx-1,jj,1,1)
    if ( abs(glamf(2,jj,1,1)-glamf(1,jj,1,1)) .gt. 10.0 ) then
      dilamf(1,jj) = 360.0 - abs ( glamf(2,jj,1,1) - glamf(1,jj,1,1) )
    else
      dilamf(1,jj) = glamf(2,jj,1,1) - glamf(1,jj,1,1)
    endif
    if ( abs(glamf(mx,jj,1,1)-glamf(mx-1,jj,1,1)) .gt. 10.0 ) then
      dilamf(mx,jj) = 360.0 - abs ( glamf(mx,jj,1,1) - glamf(mx-1,jj,1,1) )
    else
      dilamf(mx,jj) = glamf(mx,jj,1,1) - glamf(mx-1,jj,1,1)
    endif
    !-
    if ( abs(glamf(mx,jj,1,1)-glamf(mx-1,jj,1,1)) .gt. 10.0 ) then
      dilamf(mx,jj) = 360.0 - abs ( glamf(mx,jj,1,1) - glamf(mx-1,jj,1,1) )
    else
      dilamf(mx,jj) = glamf(mx,jj,1,1) - glamf(mx-1,jj,1,1)
    endif
  enddo

 ! j-derivatives on T grid :
  do ji=1,mx
    do jj=2,my-1
      djphit(ji,jj) = 0.5 * ( gphit(ji,jj+1,1,1) - gphit(ji,jj-1,1,1) )
      if  ( abs(glamt(ji,jj+1,1,1)-glamt(ji,jj-1,1,1)) .gt. 10.0 ) then
        djlamt(ji,jj) = 0.5 * ( 360.0 - abs ( glamt(ji,jj+1,1,1) - glamt(ji,jj-1,1,1) ) )
      else
        djlamt(ji,jj) = 0.5 * ( glamt(ji,jj+1,1,1) - glamt(ji,jj-1,1,1) )
      endif
    enddo
    djphit(ji,1 ) = gphit(ji, 2,1,1) - gphit(ji,   1,1,1)
    djphit(ji,my) = gphit(ji,my,1,1) - gphit(ji,my-1,1,1)
    if ( abs(glamt(ji,2,1,1)-glamt(ji,1,1,1)) .gt. 10.0 ) then
      djlamt(ji,1) = 360.0 - abs ( glamt(ji,2,1,1) - glamt(ji,1,1,1) )
    else
      djlamt(ji,1) = glamt(ji,2,1,1) - glamt(ji,1,1,1)
    endif
    if ( abs(glamt(ji,my,1,1)-glamt(ji,my-1,1,1)) .gt. 10.0 ) then
      djlamt(ji,my) = 360.0 - abs ( glamt(ji,my,1,1) - glamt(ji,my-1,1,1) )
    else
      djlamt(ji,my) = glamt(ji,my,1,1) - glamt(ji,my-1,1,1)
    endif
  enddo

 !- j-derivatives on U grid :
  do ji=1,mx
    do jj=2,my-1
      djphiu(ji,jj) = 0.5 * ( gphiu(ji,jj+1,1,1) - gphiu(ji,jj-1,1,1) )
      if  ( abs(glamu(ji,jj+1,1,1)-glamu(ji,jj-1,1,1)) .gt. 10.0 ) then
        djlamu(ji,jj) = 0.5 * ( 360.0 - abs ( glamu(ji,jj+1,1,1) - glamu(ji,jj-1,1,1) ) )
      else
        djlamu(ji,jj) = 0.5 * ( glamu(ji,jj+1,1,1) - glamu(ji,jj-1,1,1) )
      endif
    enddo
    djphiu(ji, 1) = gphiu(ji, 2,1,1) - gphiu(ji,   1,1,1)
    djphiu(ji,my) = gphiu(ji,my,1,1) - gphiu(ji,my-1,1,1)
    if ( abs(glamu(ji,2,1,1)-glamu(ji,1,1,1)) .gt. 10.0 ) then
      djlamu(ji,1) = 360.0 - abs ( glamu(ji,2,1,1) - glamu(ji,1,1,1) )
    else
      djlamu(ji,1) = glamu(ji,2,1,1) - glamu(ji,1,1,1)
    endif
    if ( abs(glamu(ji,my,1,1)-glamu(ji,my-1,1,1)) .gt. 10.0 ) then
      djlamu(ji,my) = 360.0 - abs ( glamu(ji,my,1,1) - glamu(ji,my-1,1,1) )
    else
      djlamu(ji,my) = glamu(ji,my,1,1) - glamu(ji,my-1,1,1)
    endif
  enddo

 !- j-derivavives on V grid :
  do ji=1,mx
    do jj=2,my-1
      djphiv(ji,jj) = 0.5 * ( gphiv(ji,jj+1,1,1) - gphiv(ji,jj-1,1,1) )
      if  ( abs(glamv(ji,jj+1,1,1)-glamv(ji,jj-1,1,1)) .gt. 10.0 ) then
        djlamv(ji,jj) = 0.5 * ( 360.0 - abs ( glamv(ji,jj+1,1,1) - glamv(ji,jj-1,1,1) ) )
      else
        djlamv(ji,jj) = 0.5 * ( glamv(ji,jj+1,1,1) - glamv(ji,jj-1,1,1) )
      endif
    enddo
    djphiv(ji, 1) = gphiv(ji, 2,1,1) - gphiv(ji,   1,1,1)
    djphiv(ji,my) = gphiv(ji,my,1,1) - gphiv(ji,my-1,1,1)
    if ( abs(glamv(ji,2,1,1)-glamv(ji,1,1,1)) .gt. 10.0 ) then
      djlamv(ji,1) = 360.0 - abs ( glamv(ji,2,1,1) - glamv(ji,1,1,1) ) 
    else
      djlamv(ji,1) = glamv(ji,2,1,1) - glamv(ji,1,1,1)
    endif
    if ( abs(glamv(ji,my,1,1)-glamv(ji,my-1,1,1)) .gt. 10.0 ) then
      djlamv(ji,my) = 360.0 - abs ( glamv(ji,my,1,1) - glamv(ji,my-1,1,1) ) 
    else
      djlamv(ji,my) = glamv(ji,my,1,1) - glamv(ji,my-1,1,1)
    endif
  enddo

 !- j-derivatives on F grid :
  do ji=1,mx
    do jj=2,my-1
      djphif(ji,jj) = 0.5 * ( gphif(ji,jj+1,1,1) - gphif(ji,jj-1,1,1) )
      if  ( abs(glamf(ji,jj+1,1,1)-glamf(ji,jj-1,1,1)) .gt. 10.0 ) then
        djlamf(ji,jj) = 0.5 * ( 360.0 - abs ( glamf(ji,jj+1,1,1) - glamf(ji,jj-1,1,1) ) )
      else
        djlamf(ji,jj) = 0.5 * ( glamf(ji,jj+1,1,1) - glamf(ji,jj-1,1,1) )
      endif
    enddo
    djphif(ji, 1) = gphif(ji, 2,1,1) - gphif(ji,   1,1,1)
    djphif(ji,my) = gphif(ji,my,1,1) - gphif(ji,my-1,1,1)
    if ( abs(glamf(ji,2,1,1)-glamf(ji,1,1,1)) .gt. 10.0 ) then
      djlamf(ji,1) = 360.0 - abs ( glamf(ji,2,1,1) - glamf(ji,1,1,1) ) 
    else
      djlamf(ji,1) = glamf(ji,2,1,1) - glamf(ji,1,1,1)
    endif
    if ( abs(glamf(ji,my,1,1)-glamf(ji,my-1,1,1)) .gt. 10.0 ) then
      djlamf(ji,my) = 360.0 - abs ( glamf(ji,my,1,1) - glamf(ji,my-1,1,1) ) 
    else
      djlamf(ji,my) = glamf(ji,my,1,1) - glamf(ji,my-1,1,1)
    endif
  enddo

 !- mesh sizes - see NEMO User guide or NEMO's domhgr.F90 :
      !! ** Method  :   The geographical position of the model grid-points is
      !!      defined from analytical functions, fslam and fsphi, the deriva-
      !!      tives of which gives the horizontal scale factors e1,e2.
      !!      Defining two function fslam and fsphi and their derivatives in
      !!      the two horizontal directions (fse1 and fse2), the model grid-
      !!      point position and scale factors are given by:
      !!         t-point:
      !!      glamt(i,j) = fslam(i    ,j    )   e1t(i,j) = fse1(i ,j    )
      !!      gphit(i,j) = fsphi(i    ,j    )   e2t(i,j) = fse2(i ,j    )
      !!         u-point:
      !!      glamu(i,j) = fslam(i+1/2,j    )   e1u(i,j) = fse1(i+1/2,j    )
      !!      gphiu(i,j) = fsphi(i+1/2,j    )   e2u(i,j) = fse2(i+1/2,j    )
      !!         v-point:
      !!      glamv(i,j) = fslam(i    ,j+1/2)   e1v(i,j) = fse1(i ,j+1/2)
      !!      gphiv(i,j) = fsphi(i    ,j+1/2)   e2v(i,j) = fse2(i ,j+1/2)
      !!            f-point:
      !!      glamf(i,j) = fslam(i+1/2,j+1/2)   e1f(i,j) = fse1(i+1/2,j+1/2)
      !!      gphif(i,j) = fsphi(i+1/2,j+1/2)   e2f(i,j) = fse2(i+1/2,j+1/2)
      !!      Where fse1 and fse2 are defined by:
      !!         fse1(i,j) = ra * rad * SQRT( (cos(phi) di(fslam))**2
      !!                                     +          di(fsphi) **2 )(i,j)
      !!         fse2(i,j) = ra * rad * SQRT( (cos(phi) dj(fslam))**2
      !!                                     +          dj(fsphi) **2 )(i,j)
 !- mesh sizes along X
  e1t(:,:,1,1) = ra * rad * sqrt( (cos(rad*gphit(:,:,1,1))*dilamt(:,:))**2  +  diphit(:,:)**2  )
  e1u(:,:,1,1) = ra * rad * sqrt( (cos(rad*gphiu(:,:,1,1))*dilamu(:,:))**2  +  diphiu(:,:)**2  )
  e1v(:,:,1,1) = ra * rad * sqrt( (cos(rad*gphiv(:,:,1,1))*dilamv(:,:))**2  +  diphiv(:,:)**2  )
  e1f(:,:,1,1) = ra * rad * sqrt( (cos(rad*gphif(:,:,1,1))*dilamf(:,:))**2  +  diphif(:,:)**2  )
 !- mesh sizes along Y
  e2t(:,:,1,1) = ra * rad * sqrt( (cos(rad*gphit(:,:,1,1))*djlamt(:,:))**2  +  djphit(:,:)**2  )
  e2u(:,:,1,1) = ra * rad * sqrt( (cos(rad*gphiu(:,:,1,1))*djlamu(:,:))**2  +  djphiu(:,:)**2  )
  e2v(:,:,1,1) = ra * rad * sqrt( (cos(rad*gphiv(:,:,1,1))*djlamv(:,:))**2  +  djphiv(:,:)**2  )
  e2f(:,:,1,1) = ra * rad * sqrt( (cos(rad*gphif(:,:,1,1))*djlamf(:,:))**2  +  djphif(:,:)**2  )

!=================================================================================
! 5- Interpolate ETOPO1 bathymetry on NEMO grid (defined above)
!=================================================================================

  write(*,*) ' '
  write(*,*) 'Interpolate ETOPO1 bathymetry on NEMO grid'

  ALLOCATE( bathy(mx,my), ntpts(mx,my), nland(mx,my), omsk(mx,my), tmpba(mx,my) )
  
  bathy(:,:) = 0.0
  ntpts(:,:) = 0
  nland(:,:) = 0  

  !- first, ETOPO bathy interpolated on NEMO's T-grid:
  do ji=1,mx
  do jj=1,my
    do jet=1,mlat
      if ( lat(jet) .ge. gphit(ji,jj,1,1)-0.5*djphit(ji,jj) .and. lat(jet) .lt. gphit(ji,jj,1,1)+0.5*djphit(ji,jj) ) then
        do iet=1,mlon
          if ( lon(iet) .ge. glamt(ji,jj,1,1)-0.5*dilamt(ji,jj) .and. lon(iet) .lt. glamt(ji,jj,1,1)+0.5*dilamt(ji,jj) ) then
            bathy(ji,jj) = bathy(ji,jj) + z(iet,jet)
            ntpts(ji,jj) = ntpts(ji,jj) + 1
          endif
        enddo
      endif
    enddo
    bathy(ji,jj) = bathy(ji,jj) / ntpts(ji,jj)
  enddo
  enddo 
 
  !-- then, we replace a pgr-point wide halo with bathy from the parent grid (pgr=parent grid ratio):
  !- east and west bdy :
  do jj=1,my
    do ji=1,3*pgr+2
      bathy(ji,jj) = bathy_parent( i_parent_start + perio - 4 + INT(ji/pgr)        , j_parent_start - 4 + INT(jj/pgr) )
    enddo
    do ji=mx-3*pgr-1,mx
      bathy(ji,jj) = bathy_parent( i_parent_end   + perio + 4 - INT((mx-ji+1)/pgr) , j_parent_start - 4 + INT(jj/pgr) )
    enddo
  enddo
  !- south and north bdy :
  do ji=3*pgr+3,mx-3*pgr-2
    do jj=1,3*pgr+2
      bathy(ji,jj) = bathy_parent( i_parent_start - 4 + perio + INT(ji/pgr) , j_parent_start - 4 + INT(jj/pgr)        )
    enddo
    do jj=my-3*pgr-1,my
      bathy(ji,jj) = bathy_parent( i_parent_start - 4 + perio + INT(ji/pgr) , j_parent_end   + 4 - INT((my-jj+1)/pgr) )
    enddo
  enddo

  !- we now modify the bathymetry to get the exact same LANDMASK/LAKEMASK as in WRF d02 (except extra (pgr+2)-point wide halo):
  do ji=3*pgr+3,mx-3*pgr-2
  do jj=3*pgr+3,my-3*pgr-2
    if ( LANDMASK(ji+1,jj+1,1)+LAKEMASK(ji+1,jj+1,1) .gt. 0.5 ) then
    !if ( LANDMASK(ji,jj,1)+LAKEMASK(ji,jj,1) .gt. 0.5 ) then
      bathy(ji,jj) = 0.0
    elseif ( bathy(ji,jj) .gt. -3.5 ) then ! NB must agree with NEMO's namelist
      bathy(ji,jj) = -3.5                  ! (here 3.5m is the minimum to ensure at least 3 levels on L75)
    endif
  enddo
  enddo
  !-- we now modify the bathymetry to get the exact same LANDMASK/LAKEMASK as in WRF d01 in the extra (3*pgr+2)-point wide halo :
  !- east and west bdy
  do jj=1,my
    do ji=1,3*pgr+2
      if (   LANDMASK_parent( i_parent_start - 4 + INT(ji/pgr) + perio , j_parent_start - 4 + INT(jj/pgr) , 1 )                 &
      &    + LAKEMASK_parent( i_parent_start - 4 + INT(ji/pgr) + perio , j_parent_start - 4 + INT(jj/pgr) , 1 ) .gt. 0.5 ) then
        bathy(ji,jj) = 0.0
      !elseif ( bathy(ji,jj) .gt. -3.5 ) then ! NB must agree with NEMO's namelist
      !  bathy(ji,jj) = -3.5                  ! (here 3.5m is the minimum to ensure at least 3 levels on L75)
      endif 
    enddo
    do ji=mx-3*pgr-1,mx
      if (   LANDMASK_parent( i_parent_end   + 4 - INT((mx-ji+1)/pgr) + perio , j_parent_start - 4 + INT(jj/pgr) , 1 )                 &
      &    + LAKEMASK_parent( i_parent_end   + 4 - INT((mx-ji+1)/pgr) + perio , j_parent_start - 4 + INT(jj/pgr) , 1 ) .gt. 0.5 ) then
        bathy(ji,jj) = 0.0
      elseif ( bathy(ji,jj) .gt. -3.5 ) then ! NB must agree with NEMO's namelist
        bathy(ji,jj) = -3.5                  ! (here 3.5m is the minimum to ensure at least 3 levels on L75)
      endif
    enddo
  enddo
  !- south and north bdy
  do ji=3*pgr+3,mx-3*pgr-2
    do jj=1,3*pgr+2
      if (   LANDMASK_parent( i_parent_start - 4 + perio + INT(ji/pgr) , j_parent_start - 4 + INT(jj/pgr) , 1 )                  &
      &    + LAKEMASK_parent( i_parent_start - 4 + perio + INT(ji/pgr) , j_parent_start - 4 + INT(jj/pgr) , 1 ) .gt. 0.5 ) then
        bathy(ji,jj) = 0.0
      !elseif ( bathy(ji,jj) .gt. -3.5 ) then ! NB must agree with NEMO's namelist
      !  bathy(ji,jj) = -3.5                  ! (here 3.5m is the minimum to ensure at least 3 levels on L75)
      endif
    enddo
    do jj=my-3*pgr-1,my
      if (   LANDMASK_parent( i_parent_start - 4 + perio + INT(ji/pgr) , j_parent_end   + 4 - INT((my-jj+1)/pgr) , 1 )                  &
      &    + LAKEMASK_parent( i_parent_start - 4 + perio + INT(ji/pgr) , j_parent_end   + 4 - INT((my-jj+1)/pgr) , 1 ) .gt. 0.5 ) then
        bathy(ji,jj) = 0.0
      !elseif ( bathy(ji,jj) .gt. -3.5 ) then ! NB must agree with NEMO's namelist
      !  bathy(ji,jj) = -3.5                  ! (here 3.5m is the minimum to ensure at least 3 levels on L75)
      endif
    enddo
  enddo

  !- NEMO has positive bathy in the ocean and bathy=0.0 over land
  where ( bathy(:,:) .ge. 0.0 )
    bathy(:,:) = 0.0
  elsewhere
    bathy(:,:) = bathy(:,:) * (-1.0)
  endwhere

  !- we fill the closed seas and the narrow bays (will be considered as lakes in coupled version, and as land in force NEMO)
  !  NB: if lakes are bigger than 3x3 grid points (along x or y) they have to be filled manually.
  do kf=1,50
    do ji=3*pgr+3,mx-3*pgr-2
    do jj=3*pgr+3,my-3*pgr-2
      sumnghb =   NINT( bathy(ji-1,jj  ) / ( bathy(ji-1,jj  ) + 1.e-9 ) ) &
      &         + NINT( bathy(ji+1,jj  ) / ( bathy(ji+1,jj  ) + 1.e-9 ) ) &
      &         + NINT( bathy(ji  ,jj-1) / ( bathy(ji  ,jj-1) + 1.e-9 ) ) &
      &         + NINT( bathy(ji  ,jj+1) / ( bathy(ji  ,jj+1) + 1.e-9 ) )
      if ( bathy(ji,jj) .ne. 0.0 .and. sumnghb .le. 1 ) then  !----- isolated lake point or 1-point bay
        bathy(ji,jj) = 0.0
      else
        sumnghb =   NINT( bathy(ji-1,jj+2) / ( bathy(ji-1,jj+2) + 1.e-9 ) ) &
        &         + NINT( bathy(ji  ,jj+2) / ( bathy(ji  ,jj+2) + 1.e-9 ) ) &
        &         + NINT( bathy(ji+1,jj+2) / ( bathy(ji+1,jj+2) + 1.e-9 ) ) &
        &         + NINT( bathy(ji-2,jj+1) / ( bathy(ji-2,jj+1) + 1.e-9 ) ) &
        &         + NINT( bathy(ji+2,jj+1) / ( bathy(ji+2,jj+1) + 1.e-9 ) ) &
        &         + NINT( bathy(ji-2,jj  ) / ( bathy(ji-2,jj  ) + 1.e-9 ) ) &
        &         + NINT( bathy(ji+2,jj  ) / ( bathy(ji+2,jj  ) + 1.e-9 ) ) &
        &         + NINT( bathy(ji-2,jj-1) / ( bathy(ji-2,jj-1) + 1.e-9 ) ) &
        &         + NINT( bathy(ji+2,jj-1) / ( bathy(ji+2,jj-1) + 1.e-9 ) ) &
        &         + NINT( bathy(ji-1,jj-2) / ( bathy(ji-1,jj-2) + 1.e-9 ) ) &
        &         + NINT( bathy(ji  ,jj-2) / ( bathy(ji  ,jj-2) + 1.e-9 ) ) &
        &         + NINT( bathy(ji+1,jj-2) / ( bathy(ji+1,jj-2) + 1.e-9 ) )
        if ( bathy(ji,jj) .ne. 0.0 .and. sumnghb .le. 1 ) then !----- lake of 3x3 points or smaller, or almost closed bay
          bathy(ji-1:ji+1,jj-1:jj+1) = 0.0
        else
          sumnghb =   NINT( bathy(ji-1,jj+1) / ( bathy(ji-1,jj+1) + 1.e-9 ) ) &
          &         + NINT( bathy(ji  ,jj+1) / ( bathy(ji  ,jj+1) + 1.e-9 ) ) &
          &         + NINT( bathy(ji-2,jj  ) / ( bathy(ji-2,jj  ) + 1.e-9 ) ) &
          &         + NINT( bathy(ji+1,jj  ) / ( bathy(ji+1,jj  ) + 1.e-9 ) ) &
          &         + NINT( bathy(ji-2,jj-1) / ( bathy(ji-2,jj-1) + 1.e-9 ) ) &
          &         + NINT( bathy(ji+1,jj-1) / ( bathy(ji+1,jj-1) + 1.e-9 ) ) &
          &         + NINT( bathy(ji-1,jj-2) / ( bathy(ji-1,jj-2) + 1.e-9 ) ) &
          &         + NINT( bathy(ji  ,jj-2) / ( bathy(ji  ,jj-2) + 1.e-9 ) )
          if ( bathy(ji,jj) .ne. 0.0 .and. sumnghb .le. 1 ) then !----- lake of 2x2 points or smaller, or almost closed bay
            bathy(ji-1:ji,jj-1:jj) = 0.0
          endif
        endif
      endif
    enddo
    enddo
  enddo
  

!=================================================================================
! 5- write file coordinate.nc
!=================================================================================

  write(*,*) ' '
  write(*,*) ' Writing ', TRIM(file_coord_out)

  status = NF90_CREATE(TRIM(file_coord_out),or(NF90_NOCLOBBER,NF90_64BIT_OFFSET),fidout)
  call erreur(status,.TRUE.,'create')                     
                                                         
 !- Definition des dimensions du fichiers                  
  status = NF90_DEF_DIM(fidout,"x",mx,dimID_x)
  call erreur(status,.TRUE.,"def_dimID_x")
  status = NF90_DEF_DIM(fidout,"y",my,dimID_y)
  call erreur(status,.TRUE.,"def_dimID_y")
  status = NF90_DEF_DIM(fidout,"z",mz,dimID_z)
  call erreur(status,.TRUE.,"def_dimID_z")
  status = NF90_DEF_DIM(fidout,"time",NF90_UNLIMITED,dimID_time)
  call erreur(status,.TRUE.,"def_dimID_time")
                                                       
 !- Definition des variables                             
  status = NF90_DEF_VAR(fidout,"gphiv",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),gphiv_ID)
  call erreur(status,.TRUE.,"def_var_gphiv_ID")
  status = NF90_DEF_VAR(fidout,"gphiu",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),gphiu_ID)
  call erreur(status,.TRUE.,"def_var_gphiu_ID")
  status = NF90_DEF_VAR(fidout,"gphit",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),gphit_ID)
  call erreur(status,.TRUE.,"def_var_gphit_ID")
  status = NF90_DEF_VAR(fidout,"gphif",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),gphif_ID)
  call erreur(status,.TRUE.,"def_var_gphif_ID")
  status = NF90_DEF_VAR(fidout,"glamv",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),glamv_ID)
  call erreur(status,.TRUE.,"def_var_glamv_ID")
  status = NF90_DEF_VAR(fidout,"glamu",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),glamu_ID)
  call erreur(status,.TRUE.,"def_var_glamu_ID")
  status = NF90_DEF_VAR(fidout,"glamt",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),glamt_ID)
  call erreur(status,.TRUE.,"def_var_glamt_ID")
  status = NF90_DEF_VAR(fidout,"glamf",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),glamf_ID)
  call erreur(status,.TRUE.,"def_var_glamf_ID")
  status = NF90_DEF_VAR(fidout,"e2v",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),e2v_ID)
  call erreur(status,.TRUE.,"def_var_e2v_ID")
  status = NF90_DEF_VAR(fidout,"e2u",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),e2u_ID)
  call erreur(status,.TRUE.,"def_var_e2u_ID")
  status = NF90_DEF_VAR(fidout,"e2t",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),e2t_ID)
  call erreur(status,.TRUE.,"def_var_e2t_ID")
  status = NF90_DEF_VAR(fidout,"e2f",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),e2f_ID)
  call erreur(status,.TRUE.,"def_var_e2f_ID")
  status = NF90_DEF_VAR(fidout,"e1v",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),e1v_ID)
  call erreur(status,.TRUE.,"def_var_e1v_ID")
  status = NF90_DEF_VAR(fidout,"e1u",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),e1u_ID)
  call erreur(status,.TRUE.,"def_var_e1u_ID")
  status = NF90_DEF_VAR(fidout,"e1t",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),e1t_ID)
  call erreur(status,.TRUE.,"def_var_e1t_ID")
  status = NF90_DEF_VAR(fidout,"e1f",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_time/),e1f_ID)
  call erreur(status,.TRUE.,"def_var_e1f_ID")
  status = NF90_DEF_VAR(fidout,"time",NF90_FLOAT,(/dimID_time/),time_ID)
  call erreur(status,.TRUE.,"def_var_time_ID")
  status = NF90_DEF_VAR(fidout,"nav_lon",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lon_ID)
  call erreur(status,.TRUE.,"def_var_nav_lon_ID")
  status = NF90_DEF_VAR(fidout,"nav_lev",NF90_FLOAT,(/dimID_z/),nav_lev_ID)
  call erreur(status,.TRUE.,"def_var_nav_lev_ID")
  status = NF90_DEF_VAR(fidout,"nav_lat",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lat_ID)
  call erreur(status,.TRUE.,"def_var_nav_lat_ID")
                            
 !- Attributs des variables :
  status = NF90_PUT_ATT(fidout,time_ID,"time_origin"," 0000-JAN-01 00:00:00")
  call erreur(status,.TRUE.,"put_att_time_ID")
  status = NF90_PUT_ATT(fidout,time_ID,"long_name","Time axis")
  call erreur(status,.TRUE.,"put_att_time_ID")
  status = NF90_PUT_ATT(fidout,time_ID,"title","Time")
  call erreur(status,.TRUE.,"put_att_time_ID")
  status = NF90_PUT_ATT(fidout,time_ID,"calendar","gregorian")
  call erreur(status,.TRUE.,"put_att_time_ID")
  status = NF90_PUT_ATT(fidout,time_ID,"units","seconds since 0000-01-01 00:00:00")
  call erreur(status,.TRUE.,"put_att_time_ID")
  status = NF90_PUT_ATT(fidout,nav_lon_ID,"long_name","Longitude")
  call erreur(status,.TRUE.,"put_att_nav_lon_ID")
  status = NF90_PUT_ATT(fidout,nav_lon_ID,"valid_max",180.)
  call erreur(status,.TRUE.,"put_att_nav_lon_ID")
  status = NF90_PUT_ATT(fidout,nav_lon_ID,"valid_min",-180.)
  call erreur(status,.TRUE.,"put_att_nav_lon_ID")
  status = NF90_PUT_ATT(fidout,nav_lon_ID,"units","degrees_east")
  call erreur(status,.TRUE.,"put_att_nav_lon_ID")
  status = NF90_PUT_ATT(fidout,nav_lev_ID,"long_name","Model levels")
  call erreur(status,.TRUE.,"put_att_nav_lev_ID")
  status = NF90_PUT_ATT(fidout,nav_lev_ID,"valid_max",0.)
  call erreur(status,.TRUE.,"put_att_nav_lev_ID")
  status = NF90_PUT_ATT(fidout,nav_lev_ID,"valid_min",0.)
  call erreur(status,.TRUE.,"put_att_nav_lev_ID")
  status = NF90_PUT_ATT(fidout,nav_lev_ID,"units","model_levels")
  call erreur(status,.TRUE.,"put_att_nav_lev_ID")
  status = NF90_PUT_ATT(fidout,nav_lat_ID,"long_name","Latitude")
  call erreur(status,.TRUE.,"put_att_nav_lat_ID")
  status = NF90_PUT_ATT(fidout,nav_lat_ID,"valid_max",90.)
  call erreur(status,.TRUE.,"put_att_nav_lat_ID")
  status = NF90_PUT_ATT(fidout,nav_lat_ID,"valid_min",-90.)
  call erreur(status,.TRUE.,"put_att_nav_lat_ID")
  status = NF90_PUT_ATT(fidout,nav_lat_ID,"units","degrees_north")
  call erreur(status,.TRUE.,"put_att_nav_lat_ID")
  status = NF90_PUT_ATT(fidout,gphiv_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_gphiv_ID")
  status = NF90_PUT_ATT(fidout,gphiu_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_gphiu_ID")
  status = NF90_PUT_ATT(fidout,gphit_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_gphit_ID")
  status = NF90_PUT_ATT(fidout,gphif_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_gphif_ID")
  status = NF90_PUT_ATT(fidout,glamv_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_glamv_ID")
  status = NF90_PUT_ATT(fidout,glamu_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_glamu_ID")
  status = NF90_PUT_ATT(fidout,glamt_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_glamt_ID")
  status = NF90_PUT_ATT(fidout,glamf_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_glamf_ID")
  status = NF90_PUT_ATT(fidout,e2v_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_e2v_ID")
  status = NF90_PUT_ATT(fidout,e2u_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_e2u_ID")
  status = NF90_PUT_ATT(fidout,e2t_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_e2t_ID")
  status = NF90_PUT_ATT(fidout,e2f_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_e2f_ID")
  status = NF90_PUT_ATT(fidout,e1v_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_e1v_ID")
  status = NF90_PUT_ATT(fidout,e1u_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_e1u_ID")
  status = NF90_PUT_ATT(fidout,e1t_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_e1t_ID")
  status = NF90_PUT_ATT(fidout,e1f_ID,"missing_value",1.e+20)
  call erreur(status,.TRUE.,"put_att_e1f_ID")
                            
 !- Attributs globaux       :
  status = NF90_PUT_ATT(fidout,NF90_GLOBAL,"history","Created using build_bathy_nest075_from_ETOPO.f90")
  call erreur(status,.TRUE.,"put_att_global")
  status = NF90_PUT_ATT(fidout,NF90_GLOBAL,"source","from WRF CORDEX domain")
  call erreur(status,.TRUE.,"put_att_global")  
                                             
 !- Fin des definitions                          
  status = NF90_ENDDEF(fidout)                   
  call erreur(status,.TRUE.,"fin_definition") 
                                               
 !- Valeurs prises par les variables :           
  status = NF90_PUT_VAR(fidout,gphiv_ID,gphiv)
  call erreur(status,.TRUE.,"var_gphiv_ID")
  status = NF90_PUT_VAR(fidout,gphiu_ID,gphiu)
  call erreur(status,.TRUE.,"var_gphiu_ID")
  status = NF90_PUT_VAR(fidout,gphit_ID,gphit)
  call erreur(status,.TRUE.,"var_gphit_ID")
  status = NF90_PUT_VAR(fidout,gphif_ID,gphif)
  call erreur(status,.TRUE.,"var_gphif_ID")
  status = NF90_PUT_VAR(fidout,glamv_ID,glamv)
  call erreur(status,.TRUE.,"var_glamv_ID")
  status = NF90_PUT_VAR(fidout,glamu_ID,glamu)
  call erreur(status,.TRUE.,"var_glamu_ID")
  status = NF90_PUT_VAR(fidout,glamt_ID,glamt)
  call erreur(status,.TRUE.,"var_glamt_ID")
  status = NF90_PUT_VAR(fidout,glamf_ID,glamf)
  call erreur(status,.TRUE.,"var_glamf_ID")
  status = NF90_PUT_VAR(fidout,e2v_ID,e2v)
  call erreur(status,.TRUE.,"var_e2v_ID")
  status = NF90_PUT_VAR(fidout,e2u_ID,e2u)
  call erreur(status,.TRUE.,"var_e2u_ID")
  status = NF90_PUT_VAR(fidout,e2t_ID,e2t)
  call erreur(status,.TRUE.,"var_e2t_ID")
  status = NF90_PUT_VAR(fidout,e2f_ID,e2f)
  call erreur(status,.TRUE.,"var_e2f_ID")
  status = NF90_PUT_VAR(fidout,e1v_ID,e1v)
  call erreur(status,.TRUE.,"var_e1v_ID")
  status = NF90_PUT_VAR(fidout,e1u_ID,e1u)
  call erreur(status,.TRUE.,"var_e1u_ID")
  status = NF90_PUT_VAR(fidout,e1t_ID,e1t)
  call erreur(status,.TRUE.,"var_e1t_ID")
  status = NF90_PUT_VAR(fidout,e1f_ID,e1f)
  call erreur(status,.TRUE.,"var_e1f_ID")
  status = NF90_PUT_VAR(fidout,time_ID,time)
  call erreur(status,.TRUE.,"var_time_ID")
  status = NF90_PUT_VAR(fidout,nav_lon_ID,glamt(:,:,1,1))
  call erreur(status,.TRUE.,"var_nav_lon_ID")
  status = NF90_PUT_VAR(fidout,nav_lev_ID,nav_lev)
  call erreur(status,.TRUE.,"var_nav_lev_ID")
  status = NF90_PUT_VAR(fidout,nav_lat_ID,gphit(:,:,1,1))
  call erreur(status,.TRUE.,"var_nav_lat_ID")
                                               
 !- Fin de l'ecriture                            
  status = NF90_CLOSE(fidout)                    
  call erreur(status,.TRUE.,"final")         

!==============================================================================
! 6- Write bathy_meter.nc
!==============================================================================

  write(*,*) ' Writing ', TRIM(file_bathy_out)

  status = NF90_CREATE(TRIM(file_bathy_out),or(NF90_NOCLOBBER,NF90_64BIT_OFFSET),fidbath)
  call erreur(status,.TRUE.,'create')                     
                                                    
 !- Definition des dimensions du fichiers                  
  status = NF90_DEF_DIM(fidbath,"x",mx,dimID_x)
  call erreur(status,.TRUE.,"def_dimID_x")
  status = NF90_DEF_DIM(fidbath,"y",my,dimID_y)
  call erreur(status,.TRUE.,"def_dimID_y")
                                                  
 !- Definition des variables                             
  status = NF90_DEF_VAR(fidbath,"nav_lon",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lon_ID)
  call erreur(status,.TRUE.,"def_var_nav_lon_ID")
  status = NF90_DEF_VAR(fidbath,"nav_lat",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lat_ID)
  call erreur(status,.TRUE.,"def_var_nav_lat_ID")
  status = NF90_DEF_VAR(fidbath,"Bathymetry",NF90_FLOAT,(/dimID_x,dimID_y/),Bathymetry_ID)
  call erreur(status,.TRUE.,"def_var_Bathymetry_ID")
                       
 !- Attributs des variables :
  status = NF90_PUT_ATT(fidbath,Bathymetry_ID,"valid_max",8380.802)
  call erreur(status,.TRUE.,"put_att_Bathymetry_ID")
  status = NF90_PUT_ATT(fidbath,Bathymetry_ID,"valid_min",0.)
  call erreur(status,.TRUE.,"put_att_Bathymetry_ID")
                       
 !- Attributs globaux       :
  status = NF90_PUT_ATT(fidbath,NF90_GLOBAL,"history","Created using build_bathy_nest025_from_ETOPO.f90")
  call erreur(status,.TRUE.,"put_att_global")
  status = NF90_PUT_ATT(fidbath,NF90_GLOBAL,"source","interpolated and modified from ETOPO1, with WRF's LANDMASK")
  call erreur(status,.TRUE.,"put_att_global")
                                          
 !- Fin des definitions                          
  status = NF90_ENDDEF(fidbath)                   
  call erreur(status,.TRUE.,"fin_definition") 
                                          
 !- Valeurs prises par les variables :           
  status = NF90_PUT_VAR(fidbath,nav_lon_ID,glamt(:,:,1,1))
  call erreur(status,.TRUE.,"var_nav_lon_ID")
  status = NF90_PUT_VAR(fidbath,nav_lat_ID,gphit(:,:,1,1))
  call erreur(status,.TRUE.,"var_nav_lat_ID")
  status = NF90_PUT_VAR(fidbath,Bathymetry_ID,bathy)
  call erreur(status,.TRUE.,"var_Bathymetry_ID")
                                          
 !- Fin de l'ecriture                            
  status = NF90_CLOSE(fidbath)                    
  call erreur(status,.TRUE.,"final")         


end program modif



SUBROUTINE erreur(iret, lstop, chaine)
  ! pour les messages d'erreur
  USE netcdf
  INTEGER, INTENT(in)                     :: iret
  LOGICAL, INTENT(in)                     :: lstop
  CHARACTER(LEN=*), INTENT(in)            :: chaine
  !
  CHARACTER(LEN=80)                       :: message
  !
  IF ( iret .NE. 0 ) THEN
 WRITE(*,*) 'ROUTINE: ', TRIM(chaine)
 WRITE(*,*) 'ERROR : ', iret
 message=NF90_STRERROR(iret)
 WRITE(*,*) 'WHICH MEANS :',TRIM(message)
 WRITE(*,*) '  >>>>> check for this message in the fortran program'
 IF ( lstop ) STOP
  ENDIF
  !
END SUBROUTINE erreur
