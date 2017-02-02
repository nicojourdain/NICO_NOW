program modif                                         
                                                      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! N. Jourdain, CCRC-UNSW, Sydney                                     !
! 28 May 2013                                                        !
!                                                                    !
! This script builds the initial state of a regional configuration   !
! (i.e. interpolates 3D temperature and salinity from global)        !
!                                                                    !
! NB: coeff_3D_xxxx_from_ORCA025.nc has to be created before this    ! 
!                                                                    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

USE netcdf                                            
                                                      
IMPLICIT NONE                                         
                     
!-- namelist parameters :
namelist /global/ orcadir,filemskin,filezgrin,filehgrin
namelist /parent/ conf_par, perio, max_dom, feedback, idateline, file_par_coord, file_eff_land
namelist /timectl/ yeari, yearf
INTEGER                               :: max_dom, feedback, perio, idateline
INTEGER                               :: yeari, yearf
CHARACTER(LEN=150)                    :: file_par_coord, file_eff_land
CHARACTER(LEN=50)                     :: conf_par
CHARACTER(LEN=150)                    :: orcadir, filemskin, filezgrin, filehgrin
 
!-- Global oceanic T,S file
INTEGER                                :: fidGLOB, status, dimID_time, dimID_deptht, dimID_yglo,                &
&                                         mtime, mdeptht, myTglo, mxTglo, vosaline_ID, votemper_ID, dimID_xglo, &
&                                         time_ID, deptht_ID, nav_lat_ID, nav_lon_ID, fidout                                
CHARACTER(LEN=150)                     :: file_GLOB, file_temp_out, file_sal_out 
REAL*4,ALLOCATABLE,DIMENSION(:)        :: time_counter, deptht           
REAL*4,ALLOCATABLE,DIMENSION(:,:)      :: nav_lat, nav_lon         
REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:)  :: vosaline, votemper     
INTEGER*2,ALLOCATABLE,DIMENSION(:,:,:) :: tmaskin

!-- input mask:
INTEGER                                  :: fidmsk, dimID_zm, jpkin
INTEGER*1,ALLOCATABLE,DIMENSION(:,:,:)   :: msktin   

!-- input vertical grid:
INTEGER                                  :: fidzgr, e3t_ID, hdept_ID, e3t_0_ID, gdept_0_ID
REAL*4,ALLOCATABLE,DIMENSION(:)          :: e3t_0in, gdept_0in
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: hdeptin
REAL*4,ALLOCATABLE,DIMENSION(:,:,:)      :: e3tin

!-- interpolation parameters
INTEGER                                :: fidcoeff, dimID_x, dimID_y, dimID_z, mx, my, mz, tmask_ID, wgNt_ID,   &
&                                         wgMt_ID, wgSt_ID, wgEt_ID, wgCt_ID, wgWt_ID, angYt_ID, angXt_ID,      &
&                                         gphit_ID, glamt_ID, zkUt_ID, zjNt_ID, ziNt_ID, zjMt_ID, ziMt_ID,      &
&                                         zjSt_ID, ziSt_ID, zjEt_ID, ziEt_ID, zjCt_ID, ziCt_ID, zjWt_ID, ziWt_ID 
CHARACTER(LEN=150)                     :: file_coeff
INTEGER*4,ALLOCATABLE,DIMENSION(:,:)   :: zjNt, ziNt, zjMt, ziMt, zjSt, ziSt, zjEt, ziEt, zjCt, ziCt, zjWt, ziWt
REAL*4,ALLOCATABLE,DIMENSION(:,:)      :: angYt, angXt, gphit, glamt
INTEGER*2,ALLOCATABLE,DIMENSION(:,:,:) :: tmask
REAL*4,ALLOCATABLE,DIMENSION(:,:)      :: wgNt, wgMt, wgSt, wgEt, wgCt, wgWt       

!-- Regional initial state
INTEGER                                :: ji, jj, jk, irs, jrs, krs, rs, rsmax, rz, rzmax, day, month
REAL*4                                 :: eps, tN, tM, tS, sN, sM, sS, aE, aC, aW, aS, aM, aN 
REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:)  :: to, so

LOGICAL                                :: iout, existfile

!==============================================================
! 0- Initializations
!==============================================================                                               

!- read namelist values
OPEN (UNIT=1, FILE='namelist_precpl' )
READ (UNIT=1, NML=global)
READ (UNIT=1, NML=parent)
READ (UNIT=1, NML=timectl)
CLOSE(1)

!-- look foor 1st day with 3D global ocean T,S in January of yeari :
month=1
do day=1,31
  write(file_GLOB,211) yeari, month, day
  211 FORMAT('/srv/ccrc/data22/z3381502/ORCA025.L75-MJM95-S/ORCA025.L75-MJM95_y',i4.4,'m',i2.2,'d',i2.2,'_gridT.nc')
  inquire(file=file_GLOB, exist=existfile)
  if ( existfile ) exit
enddo

!-- interpolation parameters (must be calculated before running this script) :
write(file_coeff,212) TRIM(conf_par) 
212 FORMAT('coeff_3D_',a,'_from_ORCA025.nc')

!-- output file = 3D oceanic initial temperature on the regional grid :
write(file_temp_out,213) TRIM(conf_par), yeari, month
213 FORMAT('dta_temp_',a,'_y',i4.4,'m',i2.2,'.nc')

!-- output file = 3D oceanic initial salinity on the regional grid :
write(file_sal_out,214) TRIM(conf_par), yeari, month
214 FORMAT('dta_sal_',a,'_y',i4.4,'m',i2.2,'.nc')

write(*,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
write(*,*) ' build_NEMO_parent_istate :                          '
write(*,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~                            '
write(*,*) '                                                     '
write(*,*) '        config : ', TRIM(conf_par)
write(*,*) '                                                     '
write(*,*) '    Building initial state for year: ', yeari
write(*,*) '                                                     '
                            
!==============================================================
! 1- READ GLOBAL TEMPERATURE AND SALINITY
!==============================================================
                               
write(*,*) 'Reading ', TRIM(file_GLOB)

status = NF90_OPEN(TRIM(file_GLOB),0,fidGLOB) ; call erreur(status,.TRUE.,"read_global_TS") 
                                                  
status = NF90_INQ_DIMID(fidGLOB,"time_counter",dimID_time) ; call erreur(status,.TRUE.,"inq_dimID_time")
status = NF90_INQ_DIMID(fidGLOB,"deptht",dimID_deptht)     ; call erreur(status,.TRUE.,"inq_dimID_deptht")
status = NF90_INQ_DIMID(fidGLOB,"y",dimID_yglo)            ; call erreur(status,.TRUE.,"inq_dimID_yglo")
status = NF90_INQ_DIMID(fidGLOB,"x",dimID_xglo)            ; call erreur(status,.TRUE.,"inq_dimID_xglo")
                                                      
status = NF90_INQUIRE_DIMENSION(fidGLOB,dimID_time,len=mtime)     ; call erreur(status,.TRUE.,"inq_dim_time_counter")
status = NF90_INQUIRE_DIMENSION(fidGLOB,dimID_deptht,len=mdeptht) ; call erreur(status,.TRUE.,"inq_dim_deptht")
status = NF90_INQUIRE_DIMENSION(fidGLOB,dimID_yglo,len=myTglo)    ; call erreur(status,.TRUE.,"inq_dim_yglo")
status = NF90_INQUIRE_DIMENSION(fidGLOB,dimID_xglo,len=mxTglo)    ; call erreur(status,.TRUE.,"inq_dim_xglo")
                      
ALLOCATE(  vosaline(mxTglo,myTglo,mdeptht,mtime), votemper(mxTglo,myTglo,mdeptht,mtime)          )
ALLOCATE(  time_counter(mtime), deptht(mdeptht), nav_lat(mxTglo,myTglo), nav_lon(mxTglo,myTglo)  )
ALLOCATE(  tmaskin(mxTglo,myTglo,mdeptht) )
                        
status = NF90_INQ_VARID(fidGLOB,"vosaline",vosaline_ID) ; call erreur(status,.TRUE.,"inq_vosaline_ID")
status = NF90_INQ_VARID(fidGLOB,"votemper",votemper_ID) ; call erreur(status,.TRUE.,"inq_votemper_ID")
status = NF90_INQ_VARID(fidGLOB,"time_counter",time_ID) ; call erreur(status,.TRUE.,"inq_time_ID")
status = NF90_INQ_VARID(fidGLOB,"deptht",deptht_ID)     ; call erreur(status,.TRUE.,"inq_deptht_ID")
status = NF90_INQ_VARID(fidGLOB,"nav_lat",nav_lat_ID)   ; call erreur(status,.TRUE.,"inq_nav_lat_ID")
status = NF90_INQ_VARID(fidGLOB,"nav_lon",nav_lon_ID)   ; call erreur(status,.TRUE.,"inq_nav_lon_ID")
                                                     
status = NF90_GET_VAR(fidGLOB,vosaline_ID,vosaline) ; call erreur(status,.TRUE.,"getvar_vosaline")
status = NF90_GET_VAR(fidGLOB,votemper_ID,votemper) ; call erreur(status,.TRUE.,"getvar_votemper")
status = NF90_GET_VAR(fidGLOB,time_ID,time_counter) ; call erreur(status,.TRUE.,"getvar_time_counter")
status = NF90_GET_VAR(fidGLOB,deptht_ID,deptht)     ; call erreur(status,.TRUE.,"getvar_deptht")
status = NF90_GET_VAR(fidGLOB,nav_lat_ID,nav_lat)   ; call erreur(status,.TRUE.,"getvar_nav_lat")
status = NF90_GET_VAR(fidGLOB,nav_lon_ID,nav_lon)   ; call erreur(status,.TRUE.,"getvar_nav_lon")
                                             
status = NF90_CLOSE(fidGLOB) ; call erreur(status,.TRUE.,"fin_lecture")     

!=====================================================================
! 2- READ VERTICAL GLOBAL GRID AND MASK
!=====================================================================
                       
!-- Read input mask (global)
              
write(*,*) 'Reading ', TRIM(filemskin)
                                             
status = NF90_OPEN(TRIM(filemskin),0,fidmsk)          
call erreur(status,.TRUE.,"read_mask") 
                                             
status = NF90_INQ_DIMID(fidmsk,"z",dimID_zm)
call erreur(status,.TRUE.,"inq_dimID_zm")
                                                 
status = NF90_INQUIRE_DIMENSION(fidmsk,dimID_zm,len=jpkin)
call erreur(status,.TRUE.,"inq_dim_z")
                 
ALLOCATE(  msktin(mxTglo,myTglo,mdeptht)  ) 
                   
status = NF90_INQ_VARID(fidmsk,"tmask",tmask_ID) ; call erreur(status,.TRUE.,"inq_tmask_ID")
                                                
status = NF90_GET_VAR(fidmsk,tmask_ID,msktin) ; call erreur(status,.TRUE.,"getvar_tmask")
                                        
status = NF90_CLOSE(fidmsk) ; call erreur(status,.TRUE.,"fin_lecture_mask")     

!-- Read input vertical grid (global)

write(*,*) 'Reading ', TRIM(filezgrin)

status = NF90_OPEN(TRIM(filezgrin),0,fidzgr) ; call erreur(status,.TRUE.,"read_vertical_grid") 
                                           
ALLOCATE(  e3tin    (mxTglo,myTglo,mdeptht)  ) 
ALLOCATE(  hdeptin  (mxTglo,myTglo        )  ) 
ALLOCATE(  e3t_0in  (              mdeptht)  ) 
ALLOCATE(  gdept_0in(              mdeptht)  ) 
                 
status = NF90_INQ_VARID(fidzgr,"e3t",e3t_ID)         ; call erreur(status,.TRUE.,"inq_e3t_ID")
status = NF90_INQ_VARID(fidzgr,"hdept",hdept_ID)     ; call erreur(status,.TRUE.,"inq_hdept_ID")
status = NF90_INQ_VARID(fidzgr,"e3t_0",e3t_0_ID)     ; call erreur(status,.TRUE.,"inq_e3t_0_ID")
status = NF90_INQ_VARID(fidzgr,"gdept_0",gdept_0_ID) ; call erreur(status,.TRUE.,"inq_gdept_0_ID")
                                              
status = NF90_GET_VAR(fidzgr,e3t_ID,e3tin)         ; call erreur(status,.TRUE.,"getvar_e3t")
status = NF90_GET_VAR(fidzgr,hdept_ID,hdeptin)     ; call erreur(status,.TRUE.,"getvar_hdept")
status = NF90_GET_VAR(fidzgr,e3t_0_ID,e3t_0in)     ; call erreur(status,.TRUE.,"getvar_e3t_0") 
status = NF90_GET_VAR(fidzgr,gdept_0_ID,gdept_0in) ; call erreur(status,.TRUE.,"getvar_gdept_0")
                                      
status = NF90_CLOSE(fidzgr) ; call erreur(status,.TRUE.,"fin_lecture_vertical_grid")     
    
!=====================================================================
! 3- READ INTERPOLATION COEFFICIENTS FOR REGIONAL CONFIGURATION
!=====================================================================

write(*,*) 'Reading ', TRIM(file_coeff)

status = NF90_OPEN(TRIM(file_coeff),0,fidcoeff) ; call erreur(status,.TRUE.,"read") 
                                                  
status = NF90_INQ_DIMID(fidcoeff,"x",dimID_x) ; call erreur(status,.TRUE.,"inq_dimID_x")
status = NF90_INQ_DIMID(fidcoeff,"y",dimID_y) ; call erreur(status,.TRUE.,"inq_dimID_y")
status = NF90_INQ_DIMID(fidcoeff,"z",dimID_z) ; call erreur(status,.TRUE.,"inq_dimID_z")
                                                      
status = NF90_INQUIRE_DIMENSION(fidcoeff,dimID_x,len=mx) ; call erreur(status,.TRUE.,"inq_dim_x")
status = NF90_INQUIRE_DIMENSION(fidcoeff,dimID_y,len=my) ; call erreur(status,.TRUE.,"inq_dim_y")
status = NF90_INQUIRE_DIMENSION(fidcoeff,dimID_z,len=mz) ; call erreur(status,.TRUE.,"inq_dim_z")
           
ALLOCATE(  tmask(mx,my,mz) )
ALLOCATE(  wgNt(mx,my), wgMt(mx,my), wgSt(mx,my)  ) 
ALLOCATE(  wgEt(mx,my), wgCt(mx,my), wgWt(mx,my)  ) 
ALLOCATE(  angYt(mx,my), angXt(mx,my), gphit(mx,my), glamt(mx,my)  ) 
ALLOCATE(  zjNt(mx,my), ziNt(mx,my), zjMt(mx,my), ziMt(mx,my)  ) 
ALLOCATE(  zjSt(mx,my), ziSt(mx,my), zjEt(mx,my), ziEt(mx,my)  ) 
ALLOCATE(  zjCt(mx,my), ziCt(mx,my), zjWt(mx,my), ziWt(mx,my)  ) 
                 
status = NF90_INQ_VARID(fidcoeff,"tmask",tmask_ID) ; call erreur(status,.TRUE.,"inq_tmask_ID")
status = NF90_INQ_VARID(fidcoeff,"wgNt",wgNt_ID)   ; call erreur(status,.TRUE.,"inq_wgNt_ID")
status = NF90_INQ_VARID(fidcoeff,"wgMt",wgMt_ID)   ; call erreur(status,.TRUE.,"inq_wgMt_ID")
status = NF90_INQ_VARID(fidcoeff,"wgSt",wgSt_ID)   ; call erreur(status,.TRUE.,"inq_wgSt_ID")
status = NF90_INQ_VARID(fidcoeff,"wgEt",wgEt_ID)   ; call erreur(status,.TRUE.,"inq_wgEt_ID")
status = NF90_INQ_VARID(fidcoeff,"wgCt",wgCt_ID)   ; call erreur(status,.TRUE.,"inq_wgCt_ID")
status = NF90_INQ_VARID(fidcoeff,"wgWt",wgWt_ID)   ; call erreur(status,.TRUE.,"inq_wgWt_ID")
status = NF90_INQ_VARID(fidcoeff,"angYt",angYt_ID) ; call erreur(status,.TRUE.,"inq_angYt_ID")
status = NF90_INQ_VARID(fidcoeff,"angXt",angXt_ID) ; call erreur(status,.TRUE.,"inq_angXt_ID")
status = NF90_INQ_VARID(fidcoeff,"gphit",gphit_ID) ; call erreur(status,.TRUE.,"inq_gphit_ID")
status = NF90_INQ_VARID(fidcoeff,"glamt",glamt_ID) ; call erreur(status,.TRUE.,"inq_glamt_ID")
status = NF90_INQ_VARID(fidcoeff,"zjNt",zjNt_ID)   ; call erreur(status,.TRUE.,"inq_zjNt_ID")
status = NF90_INQ_VARID(fidcoeff,"ziNt",ziNt_ID)   ; call erreur(status,.TRUE.,"inq_ziNt_ID")
status = NF90_INQ_VARID(fidcoeff,"zjMt",zjMt_ID)   ; call erreur(status,.TRUE.,"inq_zjMt_ID")
status = NF90_INQ_VARID(fidcoeff,"ziMt",ziMt_ID)   ; call erreur(status,.TRUE.,"inq_ziMt_ID")
status = NF90_INQ_VARID(fidcoeff,"zjSt",zjSt_ID)   ; call erreur(status,.TRUE.,"inq_zjSt_ID")
status = NF90_INQ_VARID(fidcoeff,"ziSt",ziSt_ID)   ; call erreur(status,.TRUE.,"inq_ziSt_ID")
status = NF90_INQ_VARID(fidcoeff,"zjEt",zjEt_ID)   ; call erreur(status,.TRUE.,"inq_zjEt_ID")
status = NF90_INQ_VARID(fidcoeff,"ziEt",ziEt_ID)   ; call erreur(status,.TRUE.,"inq_ziEt_ID")
status = NF90_INQ_VARID(fidcoeff,"zjCt",zjCt_ID)   ; call erreur(status,.TRUE.,"inq_zjCt_ID")
status = NF90_INQ_VARID(fidcoeff,"ziCt",ziCt_ID)   ; call erreur(status,.TRUE.,"inq_ziCt_ID")
status = NF90_INQ_VARID(fidcoeff,"zjWt",zjWt_ID)   ; call erreur(status,.TRUE.,"inq_zjWt_ID")
status = NF90_INQ_VARID(fidcoeff,"ziWt",ziWt_ID)   ; call erreur(status,.TRUE.,"inq_ziWt_ID")
                 
status = NF90_GET_VAR(fidcoeff,tmask_ID,tmask) ; call erreur(status,.TRUE.,"getvar_tmask")
status = NF90_GET_VAR(fidcoeff,wgNt_ID,wgNt)   ; call erreur(status,.TRUE.,"getvar_wgNt")
status = NF90_GET_VAR(fidcoeff,wgMt_ID,wgMt)   ; call erreur(status,.TRUE.,"getvar_wgMt")
status = NF90_GET_VAR(fidcoeff,wgSt_ID,wgSt)   ; call erreur(status,.TRUE.,"getvar_wgSt")
status = NF90_GET_VAR(fidcoeff,wgEt_ID,wgEt)   ; call erreur(status,.TRUE.,"getvar_wgEt")
status = NF90_GET_VAR(fidcoeff,wgCt_ID,wgCt)   ; call erreur(status,.TRUE.,"getvar_wgCt")
status = NF90_GET_VAR(fidcoeff,wgWt_ID,wgWt)   ; call erreur(status,.TRUE.,"getvar_wgWt")
status = NF90_GET_VAR(fidcoeff,angYt_ID,angYt) ; call erreur(status,.TRUE.,"getvar_angYt")
status = NF90_GET_VAR(fidcoeff,angXt_ID,angXt) ; call erreur(status,.TRUE.,"getvar_angXt")
status = NF90_GET_VAR(fidcoeff,gphit_ID,gphit) ; call erreur(status,.TRUE.,"getvar_gphit")
status = NF90_GET_VAR(fidcoeff,glamt_ID,glamt) ; call erreur(status,.TRUE.,"getvar_glamt")
status = NF90_GET_VAR(fidcoeff,zjNt_ID,zjNt)   ; call erreur(status,.TRUE.,"getvar_zjNt")
status = NF90_GET_VAR(fidcoeff,ziNt_ID,ziNt)   ; call erreur(status,.TRUE.,"getvar_ziNt")
status = NF90_GET_VAR(fidcoeff,zjMt_ID,zjMt)   ; call erreur(status,.TRUE.,"getvar_zjMt")
status = NF90_GET_VAR(fidcoeff,ziMt_ID,ziMt)   ; call erreur(status,.TRUE.,"getvar_ziMt")
status = NF90_GET_VAR(fidcoeff,zjSt_ID,zjSt)   ; call erreur(status,.TRUE.,"getvar_zjSt")
status = NF90_GET_VAR(fidcoeff,ziSt_ID,ziSt)   ; call erreur(status,.TRUE.,"getvar_ziSt")
status = NF90_GET_VAR(fidcoeff,zjEt_ID,zjEt)   ; call erreur(status,.TRUE.,"getvar_zjEt")
status = NF90_GET_VAR(fidcoeff,ziEt_ID,ziEt)   ; call erreur(status,.TRUE.,"getvar_ziEt")
status = NF90_GET_VAR(fidcoeff,zjCt_ID,zjCt)   ; call erreur(status,.TRUE.,"getvar_zjCt")
status = NF90_GET_VAR(fidcoeff,ziCt_ID,ziCt)   ; call erreur(status,.TRUE.,"getvar_ziCt")
status = NF90_GET_VAR(fidcoeff,zjWt_ID,zjWt)   ; call erreur(status,.TRUE.,"getvar_zjWt")
status = NF90_GET_VAR(fidcoeff,ziWt_ID,ziWt)   ; call erreur(status,.TRUE.,"getvar_ziWt")
                            
status = NF90_CLOSE(fidcoeff) ; call erreur(status,.TRUE.,"fin_lecture")     

!=====================================================================
! 3- INTERPOLATION OF T,S ON REGIONAL GRID
!===================================================================== 

write(*,*) 'Processing to interpolation on regional grid...'

ALLOCATE( to(mx,my,mz,1) , so(mx,my,mz,1) )

eps = 1.e-9  !-- to avoid division by zero

do ji=1,mx
do jj=1,my
do jk=1,mz

    !-- zonal interpolation at Southern point
    aE  = wgEt( ji,jj ) * msktin( ziEt(ji,jj),zjSt(ji,jj),jk ) * e3tin( ziEt(ji,jj),zjSt(ji,jj),jk )
    aC  = wgCt( ji,jj ) * msktin( ziCt(ji,jj),zjSt(ji,jj),jk ) * e3tin( ziCt(ji,jj),zjSt(ji,jj),jk )
    aW  = wgWt( ji,jj ) * msktin( ziWt(ji,jj),zjSt(ji,jj),jk ) * e3tin( ziWt(ji,jj),zjSt(ji,jj),jk ) 

    tS  = (   votemper( ziEt(ji,jj),zjSt(ji,jj),jk,1 ) * aE                             &
    &       + votemper( ziCt(ji,jj),zjSt(ji,jj),jk,1 ) * aC                             &
    &       + votemper( ziWt(ji,jj),zjSt(ji,jj),jk,1 ) * aW )  / ( aE + aC + aW + eps )

    sS  = (   vosaline( ziEt(ji,jj),zjSt(ji,jj),jk,1 ) * aE                             &
    &       + vosaline( ziCt(ji,jj),zjSt(ji,jj),jk,1 ) * aC                             &
    &       + vosaline( ziWt(ji,jj),zjSt(ji,jj),jk,1 ) * aW )  / ( aE + aC + aW + eps )

    aS  = ( aE + aC + aW ) * wgSt( ji,jj )

    !-- zonal interpolation at Central point
    aE  = wgEt( ji,jj ) * msktin( ziEt(ji,jj),zjMt(ji,jj),jk ) * e3tin( ziEt(ji,jj),zjMt(ji,jj),jk )
    aC  = wgCt( ji,jj ) * msktin( ziCt(ji,jj),zjMt(ji,jj),jk ) * e3tin( ziCt(ji,jj),zjMt(ji,jj),jk )
    aW  = wgWt( ji,jj ) * msktin( ziWt(ji,jj),zjMt(ji,jj),jk ) * e3tin( ziWt(ji,jj),zjMt(ji,jj),jk )

    tM  = (   votemper( ziEt(ji,jj),zjMt(ji,jj),jk,1 ) * aE                             &
    &       + votemper( ziCt(ji,jj),zjMt(ji,jj),jk,1 ) * aC                             &
    &       + votemper( ziWt(ji,jj),zjMt(ji,jj),jk,1 ) * aW )  / ( aE + aC + aW + eps )

    sM  = (   vosaline( ziEt(ji,jj),zjMt(ji,jj),jk,1 ) * aE                             &
    &       + vosaline( ziCt(ji,jj),zjMt(ji,jj),jk,1 ) * aC                             &
    &       + vosaline( ziWt(ji,jj),zjMt(ji,jj),jk,1 ) * aW )  / ( aE + aC + aW + eps )

    aM  = ( aE + aC + aW ) * wgMt( ji,jj )

    !-- zonal interpolation at Northern point
    aE  = wgEt( ji,jj ) * msktin( ziEt(ji,jj),zjNt(ji,jj),jk ) * e3tin( ziEt(ji,jj),zjNt(ji,jj),jk )
    aC  = wgCt( ji,jj ) * msktin( ziCt(ji,jj),zjNt(ji,jj),jk ) * e3tin( ziCt(ji,jj),zjNt(ji,jj),jk )
    aW  = wgWt( ji,jj ) * msktin( ziWt(ji,jj),zjNt(ji,jj),jk ) * e3tin( ziWt(ji,jj),zjNt(ji,jj),jk )

    tN  = (   votemper( ziEt(ji,jj),zjNt(ji,jj),jk,1 ) * aE                             &
    &       + votemper( ziCt(ji,jj),zjNt(ji,jj),jk,1 ) * aC                             &
    &       + votemper( ziWt(ji,jj),zjNt(ji,jj),jk,1 ) * aW )  / ( aE + aC + aW + eps )

    sN  = (   vosaline( ziEt(ji,jj),zjNt(ji,jj),jk,1 ) * aE                             &
    &       + vosaline( ziCt(ji,jj),zjNt(ji,jj),jk,1 ) * aC                             &
    &       + vosaline( ziWt(ji,jj),zjNt(ji,jj),jk,1 ) * aW )  / ( aE + aC + aW + eps )

    aN  = ( aE + aC + aW ) * wgNt( ji,jj )

    !-- Meridional interpolation
    if ( abs(aS+aM+aN) .gt. eps ) then

       to (ji,jj,jk,1) = ( tS*aS + tM*aM + tN*aN ) * tmask(ji,jj,jk) / ( aS + aM + aN )
       so (ji,jj,jk,1) = ( sS*aS + sM*aM + sN*aN ) * tmask(ji,jj,jk) / ( aS + aM + aN ) 

    elseif ( tmask(ji,jj,jk) .eq. 1 ) then !- oceanic point on regional grid but all land points on global grid

       !- try various extensions always turning anti-clockwise (then going upward) 
       !  (to favor more or less the same direction first at 2 neighbouring location) 
       rsmax = 8 ! maximum number of point for lateral and vertical extrapolation
                 ! -> need to go far near boundaries because bathymetry has been smoothed
       rzmax = 4 ! maximum upper extension if horizontal extension does not work.
       iout=.FALSE.
       do rz=0,rzmax,1
        do rs=1,rsmax,1
         irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=    zjEt(ji,jj)            ; krs= MAX(jk-rz, 1) !- E 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=    zjNt(ji,jj)            ; krs= MAX(jk-rz, 1) !- NEE 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs= MAX(jk-rz, 1) !- NE
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=    ziEt(ji,jj)            ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs= MAX(jk-rz, 1) !- NNE
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=    ziNt(ji,jj)            ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs= MAX(jk-rz, 1) !- N 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=    ziWt(ji,jj)            ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs= MAX(jk-rz, 1) !- NNW 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs= MAX(jk-rz, 1) !- NW 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=    zjNt(ji,jj)            ; krs= MAX(jk-rz, 1) !- NWW 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=    zjWt(ji,jj)            ; krs= MAX(jk-rz, 1) !- W 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=    zjSt(ji,jj)            ; krs= MAX(jk-rz, 1) !- SWW
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs= MAX(jk-rz, 1) !- SW
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=    ziWt(ji,jj)            ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs= MAX(jk-rz, 1) !- SSW 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=    ziSt(ji,jj)            ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs= MAX(jk-rz, 1) !- S
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=    ziEt(ji,jj)            ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs= MAX(jk-rz, 1) !- SSE 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs= MAX(jk-rz, 1) !- SE 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=    zjSt(ji,jj)            ; krs= MAX(jk-rz, 1) !- SEE 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then
           iout=.TRUE.
           exit
         elseif ( rs .eq. rsmax .and. rz .eq. rzmax ) then
           write(*,*) '!@#$%^* FATAL PROBLEM !! Argh...'
           write(*,953) ji, jj, jk
           953 FORMAT(' >>> you need to develop code to fill T-point (',3I5,')')
           write(*,*) ' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> STOP'
           stop
         endif 
        enddo !- rs
        if (iout) exit 
       enddo !-rz
       !-
       write(*,101) ji, jj, jk, irs,jrs,krs
       101 FORMAT('Further extrapolation at regional point (',3i4,') -> filled with global (',3i5,')') 
       to (ji,jj,jk,1) = votemper( irs,jrs,krs,1 )
       so (ji,jj,jk,1) = vosaline( irs,jrs,krs,1 )

    else

       to (ji,jj,jk,1) = 0.0
       so (ji,jj,jk,1) = 0.0

    endif

enddo
enddo
enddo

!=====================================================================
! 2- WRITE INITIAL STATE ON REGIONAL GRID
!===================================================================== 
                                                   
write(*,*) 'Creating ', TRIM(file_temp_out)
 
status = NF90_CREATE(TRIM(file_temp_out),or(NF90_NOCLOBBER,NF90_64BIT_OFFSET),fidout) ; call erreur(status,.TRUE.,'create')                     
                                                      
!-- dimensions 
status = NF90_DEF_DIM(fidout,"time_counter",NF90_UNLIMITED,dimID_time) ; call erreur(status,.TRUE.,"def_dimID_time")
status = NF90_DEF_DIM(fidout,"z",mdeptht,dimID_z)                      ; call erreur(status,.TRUE.,"def_dimID_z")
status = NF90_DEF_DIM(fidout,"y",my,dimID_y)                           ; call erreur(status,.TRUE.,"def_dimID_y")
status = NF90_DEF_DIM(fidout,"x",mx,dimID_x)                           ; call erreur(status,.TRUE.,"def_dimID_x")
                                                    
!-- variables
status = NF90_DEF_VAR(fidout,"votemper",NF90_FLOAT,(/dimID_x,dimID_y,dimID_z,dimID_time/),votemper_ID)
call erreur(status,.TRUE.,"def_var_votemper_ID")
status = NF90_DEF_VAR(fidout,"time_counter",NF90_FLOAT,(/dimID_time/),time_ID)    ; call erreur(status,.TRUE.,"def_var_time_ID")
status = NF90_DEF_VAR(fidout,"deptht",NF90_FLOAT,(/dimID_z/),deptht_ID)          ; call erreur(status,.TRUE.,"def_var_deptht_ID")
status = NF90_DEF_VAR(fidout,"nav_lat",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lat_ID) ; call erreur(status,.TRUE.,"def_var_nav_lat_ID")
status = NF90_DEF_VAR(fidout,"nav_lon",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lon_ID) ; call erreur(status,.TRUE.,"def_var_nav_lon_ID")
                         
!-- Attributes
status = NF90_PUT_ATT(fidout,votemper_ID,"associate","time_counter deptht nav_lat nav_lon")
status = NF90_PUT_ATT(fidout,votemper_ID,"interval_write",432000.)
status = NF90_PUT_ATT(fidout,votemper_ID,"interval_operation",432000.)
status = NF90_PUT_ATT(fidout,votemper_ID,"axis","TZYX")
status = NF90_PUT_ATT(fidout,votemper_ID,"online_operation","N/A")
status = NF90_PUT_ATT(fidout,votemper_ID,"short_name","votemper")
status = NF90_PUT_ATT(fidout,votemper_ID,"long_name","Temperature")
status = NF90_PUT_ATT(fidout,votemper_ID,"valid_max",45.)
status = NF90_PUT_ATT(fidout,votemper_ID,"valid_min",-2.)
status = NF90_PUT_ATT(fidout,votemper_ID,"missing_value",0.)
status = NF90_PUT_ATT(fidout,votemper_ID,"units","C")
call erreur(status,.TRUE.,"put_att_votemper_ID")
status = NF90_PUT_ATT(fidout,time_ID,"long_name","Time axis")
status = NF90_PUT_ATT(fidout,time_ID,"title","Time")
status = NF90_PUT_ATT(fidout,time_ID,"time_origin","0001-JAN-01 00:00:00")
status = NF90_PUT_ATT(fidout,time_ID,"units","seconds since 0006-01-01 00:00:00")
status = NF90_PUT_ATT(fidout,time_ID,"calendar","gregorian")
call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidout,deptht_ID,"long_name","Vertical T levels")
status = NF90_PUT_ATT(fidout,deptht_ID,"title","deptht")
status = NF90_PUT_ATT(fidout,deptht_ID,"valid_max",75.)
status = NF90_PUT_ATT(fidout,deptht_ID,"valid_min",0.)
status = NF90_PUT_ATT(fidout,deptht_ID,"positive","unknown")
status = NF90_PUT_ATT(fidout,deptht_ID,"units","m")
call erreur(status,.TRUE.,"put_att_deptht_ID")
status = NF90_PUT_ATT(fidout,nav_lat_ID,"nav_model","Default grid")
status = NF90_PUT_ATT(fidout,nav_lat_ID,"long_name","Latitude")
status = NF90_PUT_ATT(fidout,nav_lat_ID,"valid_max",89.94787)
status = NF90_PUT_ATT(fidout,nav_lat_ID,"valid_min",-77.01048)
status = NF90_PUT_ATT(fidout,nav_lat_ID,"units","degrees_north")
call erreur(status,.TRUE.,"put_att_nav_lat_ID")
status = NF90_PUT_ATT(fidout,nav_lon_ID,"nav_model","Default grid")
status = NF90_PUT_ATT(fidout,nav_lon_ID,"long_name","Longitude")
status = NF90_PUT_ATT(fidout,nav_lon_ID,"valid_max",180.)
status = NF90_PUT_ATT(fidout,nav_lon_ID,"valid_min",-180.)
status = NF90_PUT_ATT(fidout,nav_lon_ID,"units","degrees_east")
call erreur(status,.TRUE.,"put_att_nav_lon_ID")
                         
status = NF90_PUT_ATT(fidout,NF90_GLOBAL,"history","created using build_NEMO_parent_istate.f90")
call erreur(status,.TRUE.,"put_att_global")
                                            
status = NF90_ENDDEF(fidout) ; call erreur(status,.TRUE.,"fin_definition") 
                                            
!-- Values
status = NF90_PUT_VAR(fidout,votemper_ID,to)       ; call erreur(status,.TRUE.,"var_votemper_ID")
status = NF90_PUT_VAR(fidout,time_ID,time_counter) ; call erreur(status,.TRUE.,"var_time_ID")
status = NF90_PUT_VAR(fidout,deptht_ID,deptht)     ; call erreur(status,.TRUE.,"var_deptht_ID")
status = NF90_PUT_VAR(fidout,nav_lat_ID,gphit)     ; call erreur(status,.TRUE.,"var_nav_lat_ID")
status = NF90_PUT_VAR(fidout,nav_lon_ID,glamt)     ; call erreur(status,.TRUE.,"var_nav_lon_ID")
                                            
status = NF90_CLOSE(fidout) ;call erreur(status,.TRUE.,"final")         


!=========
! Salinity

write(*,*) 'Creating ', TRIM(file_sal_out)
 
status = NF90_CREATE(TRIM(file_sal_out),or(NF90_NOCLOBBER,NF90_64BIT_OFFSET),fidout) ; call erreur(status,.TRUE.,'create')                     
                                                      
!-- dimensions 
status = NF90_DEF_DIM(fidout,"time_counter",NF90_UNLIMITED,dimID_time) ; call erreur(status,.TRUE.,"def_dimID_time")
status = NF90_DEF_DIM(fidout,"z",mdeptht,dimID_z)                      ; call erreur(status,.TRUE.,"def_dimID_z")
status = NF90_DEF_DIM(fidout,"y",my,dimID_y)                           ; call erreur(status,.TRUE.,"def_dimID_y")
status = NF90_DEF_DIM(fidout,"x",mx,dimID_x)                           ; call erreur(status,.TRUE.,"def_dimID_x")
                                                    
!-- variables
status = NF90_DEF_VAR(fidout,"vosaline",NF90_FLOAT,(/dimID_x,dimID_y,dimID_z,dimID_time/),vosaline_ID)
call erreur(status,.TRUE.,"def_var_vosaline_ID")
status = NF90_DEF_VAR(fidout,"time_counter",NF90_FLOAT,(/dimID_time/),time_ID)    ; call erreur(status,.TRUE.,"def_var_time_ID")
status = NF90_DEF_VAR(fidout,"deptht",NF90_FLOAT,(/dimID_z/),deptht_ID)          ; call erreur(status,.TRUE.,"def_var_deptht_ID")
status = NF90_DEF_VAR(fidout,"nav_lat",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lat_ID) ; call erreur(status,.TRUE.,"def_var_nav_lat_ID")
status = NF90_DEF_VAR(fidout,"nav_lon",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lon_ID) ; call erreur(status,.TRUE.,"def_var_nav_lon_ID")
                         
!-- Attributes
status = NF90_PUT_ATT(fidout,vosaline_ID,"associate","time_counter deptht nav_lat nav_lon")
status = NF90_PUT_ATT(fidout,vosaline_ID,"interval_write",432000.)
status = NF90_PUT_ATT(fidout,vosaline_ID,"interval_operation",432000.)
status = NF90_PUT_ATT(fidout,vosaline_ID,"axis","TZYX")
status = NF90_PUT_ATT(fidout,vosaline_ID,"online_operation","N/A")
status = NF90_PUT_ATT(fidout,vosaline_ID,"short_name","vosaline")
status = NF90_PUT_ATT(fidout,vosaline_ID,"long_name","Salinity")
status = NF90_PUT_ATT(fidout,vosaline_ID,"valid_max",40.)
status = NF90_PUT_ATT(fidout,vosaline_ID,"valid_min",0.)
status = NF90_PUT_ATT(fidout,vosaline_ID,"missing_value",0.)
status = NF90_PUT_ATT(fidout,vosaline_ID,"units","PSU")
call erreur(status,.TRUE.,"put_att_vosaline_ID")
status = NF90_PUT_ATT(fidout,time_ID,"long_name","Time axis")
status = NF90_PUT_ATT(fidout,time_ID,"title","Time")
status = NF90_PUT_ATT(fidout,time_ID,"time_origin","0001-JAN-01 00:00:00")
status = NF90_PUT_ATT(fidout,time_ID,"units","seconds since 0006-01-01 00:00:00")
status = NF90_PUT_ATT(fidout,time_ID,"calendar","gregorian")
call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidout,deptht_ID,"long_name","Vertical T levels")
status = NF90_PUT_ATT(fidout,deptht_ID,"title","deptht")
status = NF90_PUT_ATT(fidout,deptht_ID,"valid_max",75.)
status = NF90_PUT_ATT(fidout,deptht_ID,"valid_min",0.)
status = NF90_PUT_ATT(fidout,deptht_ID,"positive","unknown")
status = NF90_PUT_ATT(fidout,deptht_ID,"units","m")
call erreur(status,.TRUE.,"put_att_deptht_ID")
status = NF90_PUT_ATT(fidout,nav_lat_ID,"nav_model","Default grid")
status = NF90_PUT_ATT(fidout,nav_lat_ID,"long_name","Latitude")
status = NF90_PUT_ATT(fidout,nav_lat_ID,"valid_max",89.94787)
status = NF90_PUT_ATT(fidout,nav_lat_ID,"valid_min",-77.01048)
status = NF90_PUT_ATT(fidout,nav_lat_ID,"units","degrees_north")
call erreur(status,.TRUE.,"put_att_nav_lat_ID")
status = NF90_PUT_ATT(fidout,nav_lon_ID,"nav_model","Default grid")
status = NF90_PUT_ATT(fidout,nav_lon_ID,"long_name","Longitude")
status = NF90_PUT_ATT(fidout,nav_lon_ID,"valid_max",180.)
status = NF90_PUT_ATT(fidout,nav_lon_ID,"valid_min",-180.)
status = NF90_PUT_ATT(fidout,nav_lon_ID,"units","degrees_east")
call erreur(status,.TRUE.,"put_att_nav_lon_ID")
                         
status = NF90_PUT_ATT(fidout,NF90_GLOBAL,"history","Created using build_NEMO_parent_istate.f90")
call erreur(status,.TRUE.,"put_att_global")
                                            
status = NF90_ENDDEF(fidout) ; call erreur(status,.TRUE.,"fin_definition") 
                                            
!-- Values
status = NF90_PUT_VAR(fidout,vosaline_ID,so)       ; call erreur(status,.TRUE.,"var_vosaline_ID")
status = NF90_PUT_VAR(fidout,time_ID,time_counter) ; call erreur(status,.TRUE.,"var_time_ID")
status = NF90_PUT_VAR(fidout,deptht_ID,deptht)     ; call erreur(status,.TRUE.,"var_deptht_ID")
status = NF90_PUT_VAR(fidout,nav_lat_ID,gphit)     ; call erreur(status,.TRUE.,"var_nav_lat_ID")
status = NF90_PUT_VAR(fidout,nav_lon_ID,glamt)     ; call erreur(status,.TRUE.,"var_nav_lon_ID")
                                            
status = NF90_CLOSE(fidout) ;call erreur(status,.TRUE.,"final")         

end program modif

!======================================================

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
