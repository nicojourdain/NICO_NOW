program modif                                         

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! N. JOURDAIN, CCRC-UNSW, April 2014
!                         
! Updates : 
!           - Jun 2014: LANDMASK is no longer modified
!           - Oct 2014: take LAKEMASK into account (for WRFv3.6, but still works for earlier versions).
!
! This script is used to create the file cplmask.nc that contains :
!  - fill CPLMASK: 
!       CPLMASK=0 if no coupling is required in WRF even though LANDMASK=0 (e.g. over lakes or deep bays)
!       CPLMASK=1 elsewhere.
!
! NB: CPLMASK is only for the variable received by WRF, not those sent by WRF.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                           
USE netcdf                                            
                                           
IMPLICIT NONE                                         
 
!----- namelist parameters -----
namelist /parent/ conf_par, perio, max_dom, feedback, idateline, file_par_coord, file_eff_land
namelist /child/ conf_child, file_child_coord, file_child_extend 
INTEGER                                 :: max_dom, feedback, perio, idateline
CHARACTER(LEN=150)                      :: file_par_coord, file_eff_land, file_child_coord, file_child_extend
CHARACTER(LEN=50)                       :: conf_par, conf_child

!----- WRF -----
INTEGER :: fidWRF, status, dimID_num_ext_model_couple_dom, dimID_soil_cat_stag, dimID_land_cat_stag, dimID_DIM0009,  &
&          dimID_south_north_stag, dimID_west_east_stag, dimID_soil_layers_stag, dimID_bottom_top_stag,              &
&          dimID_bottom_top, dimID_south_north, dimID_west_east, dimID_DateStrLen, dimID_Time,                       &
&          mnum_ext_model_couple_dom, msoil_cat_stag, mland_cat_stag, mDIM0009, msouth_north_stag, mwest_east_stag,  &
&          msoil_layers_stag, mbottom_top_stag, mbottom_top, msouth_north, mwest_east, mDateStrLen, mTime,           &
&          CPLMASK_ID, LANDMASK_ID, LAKEMASK_ID, XLONG_V_ID, XLAT_V_ID, XLONG_U_ID, XLAT_U_ID, XLONG_ID, XLAT_ID,    &
&          fidM, i, j, ilake, fidWG
CHARACTER(LEN=150)                      :: file_out
REAL*4,ALLOCATABLE,DIMENSION(:,:,:)     :: LANDMASK, LAKEMASK, XLONG_V, XLAT_V, XLONG_U, XLAT_U, XLONG, XLAT
REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:)   :: CPLMASK    

!----- NEMO -----
INTEGER                                 :: fidNEMO, dimID_x, dimID_y, dimID_z, dimID_t, mx, my, mz, mt, tmask_ID,     &
&                                          mbathy_ID, nav_lon_ID, nav_lat_ID, nav_lev_ID, fidNEMOpar
CHARACTER(LEN=150)                      :: file_NEMO_par
REAL*4,ALLOCATABLE,DIMENSION(:)         :: nav_lev
REAL*4,ALLOCATABLE,DIMENSION(:,:)       :: nav_lon, nav_lat, mav_lon_child, nav_lat_child
INTEGER*2,ALLOCATABLE,DIMENSION(:,:,:)  :: mbathy    
INTEGER*1,ALLOCATABLE,DIMENSION(:,:,:,:):: tmask, tmask_child
INTEGER*1,ALLOCATABLE,DIMENSION(:,:)    :: tmask_wrfgrid

!=================================================================================
! 0- Initializations 
!=================================================================================

!- read namelist values
OPEN (UNIT=1, FILE='namelist_precpl' )
READ (UNIT=1, NML=parent)
if ( max_dom .gt. 1 ) READ (UNIT=1, NML=child)
CLOSE(1)

write(file_NEMO_par,110) TRIM(conf_par)
110 FORMAT('mesh_mask_',a,'.nc')

if ( max_dom .gt. 1 ) then
  write(file_out,111) TRIM(conf_par), TRIM(conf_child)
  111 FORMAT('cplmask_d01_',a,'_',a,'.nc')
else
  write(file_out,112) TRIM(conf_par)
  112 FORMAT('cplmask_d01_',a,'.nc')
endif

  write(*,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
  write(*,*) ' create_WRF_cplmask_d01 :                            '
  write(*,*) ' ~~~~~~~~~~~~~~~~~~~~~~                              '
  write(*,*) '                                                     '
  write(*,*) '        config : ', TRIM(conf_par)
  write(*,*) '                                                     '
  write(*,*) '               - max_dom  = ', max_dom
  write(*,*) '                                                     '
 
!=================================================================================
! 1- Read NEMO parent mask file
!=================================================================================
 
write(*,*) ' '
write(*,*) ' Reading ', TRIM(file_NEMO_par)
                         
status = NF90_OPEN(TRIM(file_NEMO_par),0,fidNEMO) ; call erreur(status,.TRUE.,"read_NEMO_parent_grid") 
                                            
status = NF90_INQ_DIMID(fidNEMO,"x",dimID_x); call erreur(status,.TRUE.,"inq_dimID_x")
status = NF90_INQ_DIMID(fidNEMO,"y",dimID_y); call erreur(status,.TRUE.,"inq_dimID_y")
status = NF90_INQ_DIMID(fidNEMO,"z",dimID_z); call erreur(status,.TRUE.,"inq_dimID_z")
status = NF90_INQ_DIMID(fidNEMO,"t",dimID_t); call erreur(status,.TRUE.,"inq_dimID_t")
                                                
status = NF90_INQUIRE_DIMENSION(fidNEMO,dimID_x,len=mx); call erreur(status,.TRUE.,"inq_dim_x")
status = NF90_INQUIRE_DIMENSION(fidNEMO,dimID_y,len=my); call erreur(status,.TRUE.,"inq_dim_y")
status = NF90_INQUIRE_DIMENSION(fidNEMO,dimID_z,len=mz); call erreur(status,.TRUE.,"inq_dim_z")
status = NF90_INQUIRE_DIMENSION(fidNEMO,dimID_t,len=mt); call erreur(status,.TRUE.,"inq_dim_t")
                
ALLOCATE(  tmask(mx,my,mz,mt) )
ALLOCATE(  nav_lon(mx,my), nav_lat(mx,my), nav_lev(mz) )
                  
status = NF90_INQ_VARID(fidNEMO,"tmask",tmask_ID);     call erreur(status,.TRUE.,"inq_tmask_ID")
status = NF90_INQ_VARID(fidNEMO,"nav_lon",nav_lon_ID); call erreur(status,.TRUE.,"inq_nav_lon_ID")
status = NF90_INQ_VARID(fidNEMO,"nav_lat",nav_lat_ID); call erreur(status,.TRUE.,"inq_nav_lat_ID")
status = NF90_INQ_VARID(fidNEMO,"nav_lev",nav_lev_ID); call erreur(status,.TRUE.,"inq_nav_lev_ID")
                                               
status = NF90_GET_VAR(fidNEMO,tmask_ID,tmask);     call erreur(status,.TRUE.,"getvar_tmask")
status = NF90_GET_VAR(fidNEMO,nav_lon_ID,nav_lon); call erreur(status,.TRUE.,"getvar_nav_lon")
status = NF90_GET_VAR(fidNEMO,nav_lat_ID,nav_lat); call erreur(status,.TRUE.,"getvar_nav_lat")
status = NF90_GET_VAR(fidNEMO,nav_lev_ID,nav_lev); call erreur(status,.TRUE.,"getvar_nav_lev")
                                       
status = NF90_CLOSE(fidNEMO); call erreur(status,.TRUE.,"fin_lecture_parent")     
                                                
!=================================================================================
! 2- Read WRF input file
!================================================================================= 

write(*,*) ' '
write(*,*) ' Reading ', TRIM(file_par_coord)
                                                
status = NF90_OPEN(TRIM(file_par_coord),0,fidWRF)          
call erreur(status,.TRUE.,"read_WRF_input") 
                                                
!Lecture des ID des dimensions qui nous interessent
status = NF90_INQ_DIMID(fidWRF,"soil_cat_stag",dimID_soil_cat_stag)
call erreur(status,.TRUE.,"inq_dimID_soil_cat_stag")
status = NF90_INQ_DIMID(fidWRF,"land_cat_stag",dimID_land_cat_stag)
call erreur(status,.TRUE.,"inq_dimID_land_cat_stag")
status = NF90_INQ_DIMID(fidWRF,"DIM0009",dimID_DIM0009)
call erreur(status,.TRUE.,"inq_dimID_DIM0009")
status = NF90_INQ_DIMID(fidWRF,"south_north_stag",dimID_south_north_stag)
call erreur(status,.TRUE.,"inq_dimID_south_north_stag")
status = NF90_INQ_DIMID(fidWRF,"west_east_stag",dimID_west_east_stag)
call erreur(status,.TRUE.,"inq_dimID_west_east_stag")
status = NF90_INQ_DIMID(fidWRF,"soil_layers_stag",dimID_soil_layers_stag)
call erreur(status,.TRUE.,"inq_dimID_soil_layers_stag")
status = NF90_INQ_DIMID(fidWRF,"bottom_top_stag",dimID_bottom_top_stag)
call erreur(status,.TRUE.,"inq_dimID_bottom_top_stag")
status = NF90_INQ_DIMID(fidWRF,"bottom_top",dimID_bottom_top)
call erreur(status,.TRUE.,"inq_dimID_bottom_top")
status = NF90_INQ_DIMID(fidWRF,"south_north",dimID_south_north)
call erreur(status,.TRUE.,"inq_dimID_south_north")
status = NF90_INQ_DIMID(fidWRF,"west_east",dimID_west_east)
call erreur(status,.TRUE.,"inq_dimID_west_east")
status = NF90_INQ_DIMID(fidWRF,"DateStrLen",dimID_DateStrLen)
call erreur(status,.TRUE.,"inq_dimID_DateStrLen")
status = NF90_INQ_DIMID(fidWRF,"Time",dimID_Time)
call erreur(status,.TRUE.,"inq_dimID_Time")
                                                    
!Lecture des valeurs des dimensions qui nous interessent
status = NF90_INQUIRE_DIMENSION(fidWRF,dimID_soil_cat_stag,len=msoil_cat_stag)
call erreur(status,.TRUE.,"inq_dim_soil_cat_stag")
status = NF90_INQUIRE_DIMENSION(fidWRF,dimID_land_cat_stag,len=mland_cat_stag)
call erreur(status,.TRUE.,"inq_dim_land_cat_stag")
status = NF90_INQUIRE_DIMENSION(fidWRF,dimID_DIM0009,len=mDIM0009)
call erreur(status,.TRUE.,"inq_dim_DIM0009")
status = NF90_INQUIRE_DIMENSION(fidWRF,dimID_south_north_stag,len=msouth_north_stag)
call erreur(status,.TRUE.,"inq_dim_south_north_stag")
status = NF90_INQUIRE_DIMENSION(fidWRF,dimID_west_east_stag,len=mwest_east_stag)
call erreur(status,.TRUE.,"inq_dim_west_east_stag")
status = NF90_INQUIRE_DIMENSION(fidWRF,dimID_soil_layers_stag,len=msoil_layers_stag)
call erreur(status,.TRUE.,"inq_dim_soil_layers_stag")
status = NF90_INQUIRE_DIMENSION(fidWRF,dimID_bottom_top_stag,len=mbottom_top_stag)
call erreur(status,.TRUE.,"inq_dim_bottom_top_stag")
status = NF90_INQUIRE_DIMENSION(fidWRF,dimID_bottom_top,len=mbottom_top)
call erreur(status,.TRUE.,"inq_dim_bottom_top")
status = NF90_INQUIRE_DIMENSION(fidWRF,dimID_south_north,len=msouth_north)
call erreur(status,.TRUE.,"inq_dim_south_north")
status = NF90_INQUIRE_DIMENSION(fidWRF,dimID_west_east,len=mwest_east)
call erreur(status,.TRUE.,"inq_dim_west_east")
status = NF90_INQUIRE_DIMENSION(fidWRF,dimID_DateStrLen,len=mDateStrLen)
call erreur(status,.TRUE.,"inq_dim_DateStrLen")
status = NF90_INQUIRE_DIMENSION(fidWRF,dimID_Time,len=mTime)
call erreur(status,.TRUE.,"inq_dim_Time")
                    
!Allocation of arrays : 
ALLOCATE(  LANDMASK(mwest_east,msouth_north,mTime)  ) 
ALLOCATE(  LAKEMASK(mwest_east,msouth_north,mTime)  ) 
ALLOCATE(  XLONG_V(mwest_east,msouth_north_stag,mTime)  ) 
ALLOCATE(  XLAT_V(mwest_east,msouth_north_stag,mTime)  ) 
ALLOCATE(  XLONG_U(mwest_east_stag,msouth_north,mTime)  ) 
ALLOCATE(  XLAT_U(mwest_east_stag,msouth_north,mTime)  ) 
ALLOCATE(  XLONG(mwest_east,msouth_north,mTime)  ) 
ALLOCATE(  XLAT(mwest_east,msouth_north,mTime)  ) 
                      
!Lecture des ID des variables qui nous interessent
status = NF90_INQ_VARID(fidWRF,"LANDMASK",LANDMASK_ID)
call erreur(status,.TRUE.,"inq_LANDMASK_ID")
status = NF90_INQ_VARID(fidWRF,"XLONG_V",XLONG_V_ID)
call erreur(status,.TRUE.,"inq_XLONG_V_ID")
status = NF90_INQ_VARID(fidWRF,"XLAT_V",XLAT_V_ID)
call erreur(status,.TRUE.,"inq_XLAT_V_ID")
status = NF90_INQ_VARID(fidWRF,"XLONG_U",XLONG_U_ID)
call erreur(status,.TRUE.,"inq_XLONG_U_ID")
status = NF90_INQ_VARID(fidWRF,"XLAT_U",XLAT_U_ID)
call erreur(status,.TRUE.,"inq_XLAT_U_ID")
status = NF90_INQ_VARID(fidWRF,"XLONG",XLONG_ID)
call erreur(status,.TRUE.,"inq_XLONG_ID")
status = NF90_INQ_VARID(fidWRF,"XLAT",XLAT_ID)
call erreur(status,.TRUE.,"inq_XLAT_ID")
ilake = NF90_INQ_VARID(fidWRF,"LAKEMASK",LAKEMASK_ID)
if ( ilake .ne. 0 ) then
   write(*,*) '  WARNING: no LAKEMASK variable in wrfinput ===> assuming no lake'
else
   write(*,*) '  Using existing LAKEMASK information'
endif
                                                   
!Lecture des valeurs des variables qui nous interessent
status = NF90_GET_VAR(fidWRF,LANDMASK_ID,LANDMASK)
call erreur(status,.TRUE.,"getvar_LANDMASK")
status = NF90_GET_VAR(fidWRF,XLONG_V_ID,XLONG_V)
call erreur(status,.TRUE.,"getvar_XLONG_V")
status = NF90_GET_VAR(fidWRF,XLAT_V_ID,XLAT_V)
call erreur(status,.TRUE.,"getvar_XLAT_V")
status = NF90_GET_VAR(fidWRF,XLONG_U_ID,XLONG_U)
call erreur(status,.TRUE.,"getvar_XLONG_U")
status = NF90_GET_VAR(fidWRF,XLAT_U_ID,XLAT_U)
call erreur(status,.TRUE.,"getvar_XLAT_U")
status = NF90_GET_VAR(fidWRF,XLONG_ID,XLONG)
call erreur(status,.TRUE.,"getvar_XLONG")
status = NF90_GET_VAR(fidWRF,XLAT_ID,XLAT)
call erreur(status,.TRUE.,"getvar_XLAT")
if ( ilake .eq. 0 ) then
  status = NF90_GET_VAR(fidWRF,LAKEMASK_ID,LAKEMASK)
  call erreur(status,.TRUE.,"getvar_LAKEMASK")
else
  LAKEMASK(:,:,:) = 0.0
endif

!Fermeture du fichier lu                         
status = NF90_CLOSE(fidWRF)                      
call erreur(status,.TRUE.,"fin_lecture")     

!--------------------------------------------
!- if updated landmask due to 2-way nesting :
if ( max_dom .gt. 1 .and. feedback .eq. 1 ) then

   write(*,*) '  '
   write(*,*) '   2-way nesting -> must consider effective landmask for coupling'
   write(*,*) '                 -> ', TRIM(file_eff_land)

   status = NF90_OPEN(TRIM(file_eff_land),0,fidWG)
   call erreur(status,.TRUE.,"Read file eff_landmask")

   status = NF90_INQ_VARID(fidWG,"LANDMASK",LANDMASK_ID)
   call erreur(status,.TRUE.,"Inquire LANDMASK ID")

   status = NF90_GET_VAR(fidWG,LANDMASK_ID,LANDMASK)
   call erreur(status,.TRUE.,"Get variable LANDMASK")

   status = NF90_CLOSE(fidWG)
   call erreur(status,.TRUE.,"close netcdf file")

endif

!----------------------------------------------------------   
! Check that coordinates match between the two parent grids

if ( perio .eq. 1 ) then             
  if ( abs(nav_lat(5,3)-XLAT(4,3,1)) .gt. 0.01 .or.  abs(nav_lon(5,3)-XLONG(4,3,1)) .gt. 0.01 ) then
    write(*,*) '~!@#$%^* DIMENSIONS MISSMATCH, CHECK SCRIPT      >>>>>>>>>>> stop !!'
    stop
  endif
else
  if ( abs(nav_lat(4,3)-XLAT(4,3,1)) .gt. 0.01 .or.  abs(nav_lon(4,3)-XLONG(4,3,1)) .gt. 0.01 ) then
    write(*,*) '~!@#$%^* DIMENSIONS MISSMATCH, CHECK SCRIPT      >>>>>>>>>>> stop !!'
    stop
  endif
endif

!=================================================================================
! 3- Creation of CPLMASK 
!=================================================================================

!-nb of external grids to which WRF's domain d01 is coupled:
mnum_ext_model_couple_dom = max_dom

write(*,*) ' '
write(*,*) ' mnum_ext_model_couple_dom = ', mnum_ext_model_couple_dom

ALLOCATE(  CPLMASK(mwest_east,msouth_north,mnum_ext_model_couple_dom,mTime)  )
ALLOCATE(  tmask_wrfgrid(mwest_east,msouth_north) )

!- NEMO's parent mask on WRF_d01's grid :
if ( perio .eq. 1 ) then
  tmask_wrfgrid(1:mwest_east,1:msouth_north) = tmask(2:mx,1:my,1,1)
else
  tmask_wrfgrid(1:mwest_east,1:msouth_north) = tmask(1:mx,1:my,1,1)
endif

CPLMASK(:,:,:,:) = 1.0

!== CPLMASK FOR VARIABLE SENT BY NEMO's d01 AND RECEIVED BY WRF's d01 :
if ( perio .eq. 1 ) then
  do i=1,mwest_east
  do j=4,msouth_north-3  ! There will be a band of 3 points with imposed SST along the boundaries
    if     ( tmask_wrfgrid(i,j) .eq. 0 .and. LANDMASK(i,j,1)+LAKEMASK(i,j,1) .le. 0.5 ) then ! -> land in NEMO, ocean in WRF
      !- lakes, including "coastal lakes" adjacent to the ocean, are not coupled through NEMO -> imposed SST
      CPLMASK(i,j,1,1) = 0.0
    elseif ( tmask_wrfgrid(i,j) .eq. 1 .and. LANDMASK(i,j,1)+LAKEMASK(i,j,1) .gt. 0.5 ) then ! -> ocean in NEMO, land in WRF
      !- land points in WRF that should be oceanic according to NEMO's bathymetry 
      write(*,*) '[WRF] Land -> Ocean at : ', i, j
    endif
  enddo
  enddo
  CPLMASK(:,             1:3           ,1,:) = 0.0  ! put observed SSTs along specified bdy
  CPLMASK(:,msouth_north-2:msouth_north,1,:) = 0.0  !
else
  do i=4,mwest_east-3    ! There will be a band of 3 points with imposed SST along the boundaries
  do j=4,msouth_north-3  !        "                    "                  "
    if     ( tmask_wrfgrid(i,j) .eq. 0 .and. LANDMASK(i,j,1)+LAKEMASK(i,j,1) .le. 0.5 ) then ! -> land in NEMO, ocean in WRF
      !- lakes, including "coastal lakes" adjacent to the ocean, are not coupled through NEMO -> imposed SST
      CPLMASK(i,j,1,1) = 0.0
    elseif ( tmask_wrfgrid(i,j) .eq. 1 .and. LANDMASK(i,j,1)+LAKEMASK(i,j,1) .gt. 0.5 ) then ! -> ocean in NEMO, land in WRF
      !- land points in WRF that should be oceanic according to NEMO's bathymetry
      write(*,*) '[WRF] Land -> Ocean at : ', i, j
    endif
  enddo
  enddo
  CPLMASK(            :           ,              1:3            , 1 , : ) = 0.0  ! put observed SSTs along specified bdy
  CPLMASK(            :           , msouth_north-2:msouth_north , 1 , : ) = 0.0  !  "            "           " 
  CPLMASK(           1:3          ,               :             , 1 , : ) = 0.0  !  "            "           "
  CPLMASK(mwest_east-2:mwest_east ,               :             , 1 , : ) = 0.0  !  "            "           "
endif

!== CPLMASK FOR VARIABLES SENT BY NEMO's d02 AND RECEIVED BY WRF's d01 :
if ( max_dom .gt. 1 ) CPLMASK(:,:,2,:) = 0.0

!=================================================================================
! 4- Writing new netcdf file
!=================================================================================                                   

write(*,*) ' '
write(*,*) ' Writing ', TRIM(file_out)
                                                   
status = NF90_CREATE(TRIM(file_out),or(NF90_NOCLOBBER,NF90_64BIT_OFFSET),fidM)
call erreur(status,.TRUE.,'create')                     
                                                     
!Definition des dimensions du fichiers                  
status = NF90_DEF_DIM(fidM,"num_ext_model_couple_dom",mnum_ext_model_couple_dom,dimID_num_ext_model_couple_dom)
call erreur(status,.TRUE.,"def_dimID_num_ext_model_couple_dom")
status = NF90_DEF_DIM(fidM,"soil_cat_stag",msoil_cat_stag,dimID_soil_cat_stag)
call erreur(status,.TRUE.,"def_dimID_soil_cat_stag")
status = NF90_DEF_DIM(fidM,"land_cat_stag",mland_cat_stag,dimID_land_cat_stag)
call erreur(status,.TRUE.,"def_dimID_land_cat_stag")
status = NF90_DEF_DIM(fidM,"DIM0009",mDIM0009,dimID_DIM0009)
call erreur(status,.TRUE.,"def_dimID_DIM0009")
status = NF90_DEF_DIM(fidM,"south_north_stag",msouth_north_stag,dimID_south_north_stag)
call erreur(status,.TRUE.,"def_dimID_south_north_stag")
status = NF90_DEF_DIM(fidM,"west_east_stag",mwest_east_stag,dimID_west_east_stag)
call erreur(status,.TRUE.,"def_dimID_west_east_stag")
status = NF90_DEF_DIM(fidM,"soil_layers_stag",msoil_layers_stag,dimID_soil_layers_stag)
call erreur(status,.TRUE.,"def_dimID_soil_layers_stag")
status = NF90_DEF_DIM(fidM,"bottom_top_stag",mbottom_top_stag,dimID_bottom_top_stag)
call erreur(status,.TRUE.,"def_dimID_bottom_top_stag")
status = NF90_DEF_DIM(fidM,"bottom_top",mbottom_top,dimID_bottom_top)
call erreur(status,.TRUE.,"def_dimID_bottom_top")
status = NF90_DEF_DIM(fidM,"south_north",msouth_north,dimID_south_north)
call erreur(status,.TRUE.,"def_dimID_south_north")
status = NF90_DEF_DIM(fidM,"west_east",mwest_east,dimID_west_east)
call erreur(status,.TRUE.,"def_dimID_west_east")
status = NF90_DEF_DIM(fidM,"DateStrLen",mDateStrLen,dimID_DateStrLen)
call erreur(status,.TRUE.,"def_dimID_DateStrLen")
status = NF90_DEF_DIM(fidM,"Time",NF90_UNLIMITED,dimID_Time)
call erreur(status,.TRUE.,"def_dimID_Time")
                                                   
!Definition des variables                             
status = NF90_DEF_VAR(fidM,"CPLMASK",NF90_FLOAT,(/dimID_west_east,dimID_south_north,dimID_num_ext_model_couple_dom,dimID_Time/),CPLMASK_ID)
call erreur(status,.TRUE.,"def_var_CPLMASK_ID")

! Attributs des variables :
status = NF90_PUT_ATT(fidM,CPLMASK_ID,"units","")
call erreur(status,.TRUE.,"put_att_CPLMASK_ID")
status = NF90_PUT_ATT(fidM,CPLMASK_ID,"stagger","")
call erreur(status,.TRUE.,"put_att_CPLMASK_ID")
status = NF90_PUT_ATT(fidM,CPLMASK_ID,"description","COUPLING MASK (0 FOR OBSERVATION, 1 FOR COUPLER), number of external domains")
call erreur(status,.TRUE.,"put_att_CPLMASK_ID")
status = NF90_PUT_ATT(fidM,CPLMASK_ID,"coordinates","XLONG XLAT")
call erreur(status,.TRUE.,"put_att_CPLMASK_ID")
status = NF90_PUT_ATT(fidM,CPLMASK_ID,"MemoryOrder","XY ")
call erreur(status,.TRUE.,"put_att_CPLMASK_ID")
status = NF90_PUT_ATT(fidM,CPLMASK_ID,"FieldType",104)
call erreur(status,.TRUE.,"put_att_CPLMASK_ID")
          
! Attributs globaux       :
status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","created using create_WRF_cplmask_d01.f90")
call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
                                           
!Fin des definitions                          
status = NF90_ENDDEF(fidM)                   
call erreur(status,.TRUE.,"fin_definition") 
                                           
!Valeurs prises par les variables :           
status = NF90_PUT_VAR(fidM,CPLMASK_ID,CPLMASK)
call erreur(status,.TRUE.,"var_CPLMASK_ID")
                                           
!Fin de l'ecriture                            
status = NF90_CLOSE(fidM)                    
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
WRITE(*,*) 'ERREUR: ', iret
message=NF90_STRERROR(iret)
WRITE(*,*) 'CA VEUT DIRE:',TRIM(message)
IF ( lstop ) STOP
ENDIF
!
END SUBROUTINE erreur
