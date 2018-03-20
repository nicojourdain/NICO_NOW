program modif                                         

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! N. Jourdain, CCRC-UNSW, Sydney, May 2013                                                !
!                                                                                         !
! Used to build yearly OBCs for a regional domain                                         !
!                                                                                         !
! 0- USER'S CHOICES                                                                       !
! 1- READ VERTICAL GRID AND MASK FOR GLOBAL OCEAN SIMULATION                              !
! 2- READ INTERPOLATION COEFFICIENTS                                                      !
! 3- CALCULATE INTERPOLATED FIELDS ON OBCs:                                               !
! 4- WRITE OBCs                                                                           !
!                                                                                         !
! rq: partial steps are accounted for by multiplying interpolation weights by e3          !
!                                                                                         !
! Assumes global ocean files with one time step only                                      !
!                                                                                         !
! NB: run build_NEMO_MC25_OBC_intrp_coeff.f90 before this script                       !
!                                                                                         !
! NB: usually takes several hours to run for multi-year OBCs                              !
!                                                                                         !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                      
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

!-- Interpolation coefficients
INTEGER                                   :: fidcoeff, dimID_x, dimID_y, dimID_z, mx, my, mz, mxf, myf,                &
&                                            wgNv_ID, wgMv_ID, wgSv_ID, wgEv_ID, wgCv_ID, wgWv_ID, angYv_ID, angXv_ID, &
&                                            wgNu_ID, wgMu_ID, wgSu_ID, wgEu_ID, wgCu_ID, wgWu_ID, angYu_ID, angXu_ID, &
&                                            wgNt_ID, wgMt_ID, wgSt_ID, wgEt_ID, wgCt_ID, wgWt_ID, angYt_ID, angXt_ID, &
&                                            wgNf_ID, wgMf_ID, wgSf_ID, wgEf_ID, wgCf_ID, wgWf_ID, angYf_ID, angXf_ID, &
&                                            gphibdyv_ID, gphibdyu_ID, gphibdyt_ID, e3ubdy_ID, e3vbdy_ID,              &
&                                            glambdyv_ID, glambdyu_ID, glambdyt_ID, dimID_xf, dimID_yf,                &
&                                            zjNv_ID, ziNv_ID, zjMv_ID, ziMv_ID, zjSv_ID, ziSv_ID, e2ubdy_ID,e1vbdy_ID,&
&                                            zjEv_ID, ziEv_ID, zjCv_ID, ziCv_ID, zjWv_ID, ziWv_ID, zkUv_ID,            &
&                                            zjNu_ID, ziNu_ID, zjMu_ID, ziMu_ID, zjSu_ID, ziSu_ID,                     &
&                                            zjEu_ID, ziEu_ID, zjCu_ID, ziCu_ID, zjWu_ID, ziWu_ID, zkUu_ID,            &
&                                            zjNt_ID, ziNt_ID, zjMt_ID, ziMt_ID, zjSt_ID, ziSt_ID,                     &
&                                            zjEt_ID, ziEt_ID, zjCt_ID, ziCt_ID, zjWt_ID, ziWt_ID, zkUt_ID,            &
&                                            zjNf_ID, ziNf_ID, zjMf_ID, ziMf_ID, zjSf_ID, ziSf_ID,                     &
&                                            zjEf_ID, ziEf_ID, zjCf_ID, ziCf_ID, zjWf_ID, ziWf_ID, zkUf_ID,            &
&                                            tmask_ID, umask_ID, vmask_ID
CHARACTER(LEN=150)                        :: file_coeff                           
INTEGER*4,ALLOCATABLE,DIMENSION(:,:)      :: zjNv, ziNv, zjMv, ziMv, zjSv, ziSv, zjEv, ziEv, zjCv, ziCv, zjWv, ziWv,   &
&                                            zjNu, ziNu, zjMu, ziMu, zjSu, ziSu, zjEu, ziEu, zjCu, ziCu, zjWu, ziWu,   &
&                                            zjNt, ziNt, zjMt, ziMt, zjSt, ziSt, zjEt, ziEt, zjCt, ziCt, zjWt, ziWt,   & 
&                                            zjNf, ziNf, zjMf, ziMf, zjSf, ziSf, zjEf, ziEf, zjCf, ziCf, zjWf, ziWf
REAL*4,ALLOCATABLE,DIMENSION(:,:)         :: angYv, angXv, angYu, angXu, angYt, angXt,                                 &
&                                            gphibdyv, gphibdyu, gphibdyt, glambdyv, glambdyu, glambdyt
INTEGER*4,ALLOCATABLE,DIMENSION(:,:,:)    :: msktbdy, mskubdy, mskvbdy
REAL*4,ALLOCATABLE,DIMENSION(:,:)         :: wgNv, wgMv, wgSv, wgEv, wgCv, wgWv,                                       &
&                                            wgNu, wgMu, wgSu, wgEu, wgCu, wgWu,                                       &
&                                            wgNt, wgMt, wgSt, wgEt, wgCt, wgWt,                                       &
&                                            wgNf, wgMf, wgSf, wgEf, wgCf, wgWf, e2ubdy, e1vbdy
REAL*4,ALLOCATABLE,DIMENSION(:,:,:)       :: e3ubdy, e3vbdy

!-- input global mask:
INTEGER                                  :: fidmsk, dimID_xm, dimID_ym, dimID_zm, jpiin, jpjin, jpkin, jiin, jjin, jkin
INTEGER*1,ALLOCATABLE,DIMENSION(:,:,:)   :: mskvin, mskuin, msktin, mskfin

!-- input global grid:
INTEGER                                  :: fidhgr, fidzgr, e3v_ID, e3u_ID, e3t_ID, hdept_ID, e3t_0_ID, gdept_0_ID,    &
&                                           e2u_ID, e1v_ID
REAL*4,ALLOCATABLE,DIMENSION(:)          :: e3t_0in, gdept_0in
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: hdeptin, e2uin, e1vin
REAL*4,ALLOCATABLE,DIMENSION(:,:,:)      :: e3win, e3vin, e3uin, e3tin

!-- Global Ocean T,S,u,v (fields to interpolate on OBCs)
LOGICAL                                   :: existfileT, existfileU, existfileV
INTEGER                                   :: month, day, yr
CHARACTER(LEN=150)                        :: file_globT, file_globU, file_globV
INTEGER                                   :: fidT, dimID_time_counter, dimID_deptht, dimID_yg, dimID_xg, mtime_counter,&
&                                            mdeptht, myTglo, mxTglo, vosaline_ID, votemper_ID, time_counter_ID,       &
&                                            deptht_ID, nav_lat_ID, nav_lon_ID, fidU, mdepthu, myUglo, mxUglo,         &
&                                            vozocrtx_ID, vomecrty_ID, fidV, mdepthv, myVglo, mxVglo, dimID_depthu,    &
&                                            dimID_depthv
REAL*4,ALLOCATABLE,DIMENSION(:)           :: time_counter, deptht
REAL*4,ALLOCATABLE,DIMENSION(:,:)         :: nav_lat, nav_lon, ztrpu, ztrpv, deptr, psifin
REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:)     :: vosaline, votemper, vozocrtx, vomecrty

!-- OBC
                                                      
INTEGER                                   :: fidobc, status, dimID_t, dimID_XX, mt, vo_ID, uo_ID, so_ID, to_ID, iside, &
&                                            it, ji, jj, jk, nside, irs, jrs, krs, rs, rsmax, jim1, jip1, jjm1, jjp1
CHARACTER(LEN=150)                        :: file_obc
CHARACTER(LEN=5),ALLOCATABLE,DIMENSION(:) :: side
REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:)     :: vobdy, uobdy, sobdy, tobdy
REAL*4,ALLOCATABLE,DIMENSION(:,:,:)       :: uu, vv
REAL*4                                    :: tS, tM, tN, sS, sM, sN, uS, uM, uN, vS, vM, vN, eps,                      &
&                                            aN, aS, aM, aW, aE, aC, pS, pM, pN
                                               
!==================================================================================
! 0- Initializations
!==================================================================================

!- read namelist values
OPEN (UNIT=1, FILE='namelist_precpl' )
READ (UNIT=1, NML=global)
READ (UNIT=1, NML=parent)
READ (UNIT=1, NML=timectl)
CLOSE(1)

!-- Number of OBCs to build :
if ( perio .eq. 1 ) then
  nside=2
  ALLOCATE(side(nside))
  side = (/'north','south'/)
else
  nside=4
  ALLOCATE(side(nside))
  side = (/'north','south','east','west'/)
endif

write(*,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
write(*,*) ' build_NEMO_yearly_OBC :                             '
write(*,*) ' ~~~~~~~~~~~~~~~~~~~~~                               '
write(*,*) '                                                     '
write(*,*) '        config : ', TRIM(conf_par)
write(*,*) '                                                     '
write(*,*) '        first year: ', yeari
write(*,*) '        last  year: ', yearf
write(*,*) '                                                     '
 
eps = 1.e-6  !-- to avoid division by zero

!============================================================
! 1- read vertical grid and mask for global ocean simulation 
!============================================================

!-- Read input mask (global)
              
write(*,*) '  Reading ', TRIM(filemskin)
                                             
status = NF90_OPEN(TRIM(filemskin),0,fidmsk)          
call erreur(status,.TRUE.,"read_mask") 
                                             
status = NF90_INQ_DIMID(fidmsk,"x",dimID_xm) ; call erreur(status,.TRUE.,"inq_dimID_xm")
status = NF90_INQ_DIMID(fidmsk,"y",dimID_ym) ; call erreur(status,.TRUE.,"inq_dimID_ym")
status = NF90_INQ_DIMID(fidmsk,"z",dimID_zm) ; call erreur(status,.TRUE.,"inq_dimID_zm")
                                                 
status = NF90_INQUIRE_DIMENSION(fidmsk,dimID_xm,len=jpiin) ; call erreur(status,.TRUE.,"inq_dim_xm")
status = NF90_INQUIRE_DIMENSION(fidmsk,dimID_ym,len=jpjin) ; call erreur(status,.TRUE.,"inq_dim_ym")
status = NF90_INQUIRE_DIMENSION(fidmsk,dimID_zm,len=jpkin) ; call erreur(status,.TRUE.,"inq_dim_zm")                 

ALLOCATE(  mskvin(jpiin,jpjin,jpkin)  ) 
ALLOCATE(  mskuin(jpiin,jpjin,jpkin)  ) 
ALLOCATE(  msktin(jpiin,jpjin,jpkin)  ) 
                   
status = NF90_INQ_VARID(fidmsk,"vmask",vmask_ID) ; call erreur(status,.TRUE.,"inq_vmask_ID")
status = NF90_INQ_VARID(fidmsk,"umask",umask_ID) ; call erreur(status,.TRUE.,"inq_umask_ID")
status = NF90_INQ_VARID(fidmsk,"tmask",tmask_ID) ; call erreur(status,.TRUE.,"inq_tmask_ID")
                                                
status = NF90_GET_VAR(fidmsk,vmask_ID,mskvin) ; call erreur(status,.TRUE.,"getvar_vmask")
status = NF90_GET_VAR(fidmsk,umask_ID,mskuin) ; call erreur(status,.TRUE.,"getvar_umask")
status = NF90_GET_VAR(fidmsk,tmask_ID,msktin) ; call erreur(status,.TRUE.,"getvar_tmask")
                                        
status = NF90_CLOSE(fidmsk)                      
call erreur(status,.TRUE.,"fin_lecture_mask")     

!-- Read horizontal input grid (global)

write(*,*) '  Reading ', TRIM(filehgrin)

status = NF90_OPEN(TRIM(filehgrin),0,fidhgr) ; call erreur(status,.TRUE.,"read_horizontal_grid") 
                                       
ALLOCATE(  e2uin(jpiin,jpjin) )
ALLOCATE(  e1vin(jpiin,jpjin) )

status = NF90_INQ_VARID(fidhgr,"e2u",e2u_ID) ; call erreur(status,.TRUE.,"inq_e2u_ID")
status = NF90_INQ_VARID(fidhgr,"e1v",e1v_ID) ; call erreur(status,.TRUE.,"inq_e1v_ID")

status = NF90_GET_VAR(fidhgr,e2u_ID,e2uin) ; call erreur(status,.TRUE.,"getvar_e2u")
status = NF90_GET_VAR(fidhgr,e1v_ID,e1vin) ; call erreur(status,.TRUE.,"getvar_e1v")

status = NF90_CLOSE(fidhgr) ; call erreur(status,.TRUE.,"fin_lecture_horizontal_grid") 

!-- Read input vertical grid (global)

write(*,*) 'Reading ', TRIM(filezgrin)

status = NF90_OPEN(TRIM(filezgrin),0,fidzgr)          
call erreur(status,.TRUE.,"read_vertical_grid") 
                                           
ALLOCATE(  e3vin    (jpiin,jpjin,jpkin)  ) 
ALLOCATE(  e3uin    (jpiin,jpjin,jpkin)  ) 
ALLOCATE(  e3tin    (jpiin,jpjin,jpkin)  ) 
ALLOCATE(  hdeptin  (jpiin,jpjin      )  ) 
ALLOCATE(  e3t_0in  (            jpkin)  ) 
ALLOCATE(  gdept_0in(            jpkin)  ) 
                 
status = NF90_INQ_VARID(fidzgr,"e3v",e3v_ID)         ; call erreur(status,.TRUE.,"inq_e3v_ID")
status = NF90_INQ_VARID(fidzgr,"e3u",e3u_ID)         ; call erreur(status,.TRUE.,"inq_e3u_ID")
status = NF90_INQ_VARID(fidzgr,"e3t",e3t_ID)         ; call erreur(status,.TRUE.,"inq_e3t_ID")
status = NF90_INQ_VARID(fidzgr,"hdept",hdept_ID)     ; call erreur(status,.TRUE.,"inq_hdept_ID")
status = NF90_INQ_VARID(fidzgr,"e3t_0",e3t_0_ID)     ; call erreur(status,.TRUE.,"inq_e3t_0_ID")
status = NF90_INQ_VARID(fidzgr,"gdept_0",gdept_0_ID) ; call erreur(status,.TRUE.,"inq_gdept_0_ID")
                                              
status = NF90_GET_VAR(fidzgr,e3v_ID,e3vin)         ; call erreur(status,.TRUE.,"getvar_e3v")
status = NF90_GET_VAR(fidzgr,e3u_ID,e3uin)         ; call erreur(status,.TRUE.,"getvar_e3u")
status = NF90_GET_VAR(fidzgr,e3t_ID,e3tin)         ; call erreur(status,.TRUE.,"getvar_e3t")
status = NF90_GET_VAR(fidzgr,hdept_ID,hdeptin)     ; call erreur(status,.TRUE.,"getvar_hdept")
status = NF90_GET_VAR(fidzgr,e3t_0_ID,e3t_0in)     ; call erreur(status,.TRUE.,"getvar_e3t_0") 
status = NF90_GET_VAR(fidzgr,gdept_0_ID,gdept_0in) ; call erreur(status,.TRUE.,"getvar_gdept_0")
                                      
status = NF90_CLOSE(fidzgr)                      
call erreur(status,.TRUE.,"fin_lecture_vertical_grid")     

!=====
 
DO iside=1,nside
                                                         
  !=========================================================
  ! 2- READ INTERPOLATION COEFFICIENTS (ON REGIONAL GRID)
  !=========================================================

  !-- file containing interpolation coefficients
  write(file_coeff,723) TRIM(side(iside)), TRIM(conf_par)
  723 FORMAT('coeff_OBC_',a,'_',a,'_from_ORCA025_L75.nc')
                                                
  status = NF90_OPEN(TRIM(file_coeff),0,fidcoeff)          
  call erreur(status,.TRUE.,"read_interp_coeff") 
                                                  
  status = NF90_INQ_DIMID(fidcoeff,"x",dimID_x)   ; call erreur(status,.TRUE.,"inq_dimID_x")
  status = NF90_INQ_DIMID(fidcoeff,"y",dimID_y)   ; call erreur(status,.TRUE.,"inq_dimID_y")
  status = NF90_INQ_DIMID(fidcoeff,"z",dimID_z)   ; call erreur(status,.TRUE.,"inq_dimID_z")
  status = NF90_INQ_DIMID(fidcoeff,"xf",dimID_xf) ; call erreur(status,.TRUE.,"inq_dimID_xf")
  status = NF90_INQ_DIMID(fidcoeff,"yf",dimID_yf) ; call erreur(status,.TRUE.,"inq_dimID_yf")
 
  status = NF90_INQUIRE_DIMENSION(fidcoeff,dimID_x,len=mx)   ; call erreur(status,.TRUE.,"inq_dim_x")
  status = NF90_INQUIRE_DIMENSION(fidcoeff,dimID_y,len=my)   ; call erreur(status,.TRUE.,"inq_dim_y")
  status = NF90_INQUIRE_DIMENSION(fidcoeff,dimID_z,len=mz)   ; call erreur(status,.TRUE.,"inq_dim_z")
  status = NF90_INQUIRE_DIMENSION(fidcoeff,dimID_xf,len=mxf) ; call erreur(status,.TRUE.,"inq_dim_xf")
  status = NF90_INQUIRE_DIMENSION(fidcoeff,dimID_yf,len=myf) ; call erreur(status,.TRUE.,"inq_dim_yf")                     
 
  ALLOCATE(  wgNv(mx,my), wgMv(mx,my), wgSv(mx,my)  ) 
  ALLOCATE(  wgEv(mx,my), wgCv(mx,my), wgWv(mx,my)  ) 
  ALLOCATE(  wgNu(mx,my), wgMu(mx,my), wgSu(mx,my)  ) 
  ALLOCATE(  wgEu(mx,my), wgCu(mx,my), wgWu(mx,my)  ) 
  ALLOCATE(  wgNt(mx,my), wgMt(mx,my), wgSt(mx,my)  ) 
  ALLOCATE(  wgEt(mx,my), wgCt(mx,my), wgWt(mx,my)  ) 
  ALLOCATE(  wgNf(mxf,myf), wgMf(mxf,myf), wgSf(mxf,myf)  )
  ALLOCATE(  wgEf(mxf,myf), wgCf(mxf,myf), wgWf(mxf,myf)  )
  ALLOCATE(  msktbdy(mx,my,mz), mskubdy(mx,my,mz), mskvbdy(mx,my,mz) )
  ALLOCATE(  angYv(mx,my), angXv(mx,my), angYu(mx,my), angXu(mx,my), angYt(mx,my), angXt(mx,my)  )
  ALLOCATE(  gphibdyv(mx,my), gphibdyu(mx,my), gphibdyt(mx,my), glambdyv(mx,my), glambdyu(mx,my), glambdyt(mx,my)  ) 
  ALLOCATE(  zjNv(mx,my), ziNv(mx,my), zjMv(mx,my), ziMv(mx,my), zjSv(mx,my), ziSv(mx,my)  ) 
  ALLOCATE(  zjEv(mx,my), ziEv(mx,my), zjCv(mx,my), ziCv(mx,my), zjWv(mx,my), ziWv(mx,my)  ) 
  ALLOCATE(  zjNu(mx,my), ziNu(mx,my), zjMu(mx,my), ziMu(mx,my), zjSu(mx,my), ziSu(mx,my)  )           
  ALLOCATE(  zjEu(mx,my), ziEu(mx,my), zjCu(mx,my), ziCu(mx,my), zjWu(mx,my), ziWu(mx,my)  )
  ALLOCATE(  zjNt(mx,my), ziNt(mx,my), zjMt(mx,my), ziMt(mx,my), zjSt(mx,my), ziSt(mx,my)  )           
  ALLOCATE(  zjEt(mx,my), ziEt(mx,my), zjCt(mx,my), ziCt(mx,my), zjWt(mx,my), ziWt(mx,my)  )
  ALLOCATE(  zjNf(mxf,myf), ziNf(mxf,myf), zjMf(mxf,myf), ziMf(mxf,myf), zjSf(mxf,myf), ziSf(mxf,myf)  )
  ALLOCATE(  zjEf(mxf,myf), ziEf(mxf,myf), zjCf(mxf,myf), ziCf(mxf,myf), zjWf(mxf,myf), ziWf(mxf,myf)  )
  ALLOCATE(  e3ubdy(mx,my,mz), e3vbdy(mx,my,mz), e2ubdy(mx,my), e1vbdy(mx,my) ) 
 
  !-- Variables ID
  status = NF90_INQ_VARID(fidcoeff,"gphiv",gphibdyv_ID) ; call erreur(status,.TRUE.,"inq_gphibdyv_ID")
  status = NF90_INQ_VARID(fidcoeff,"gphiu",gphibdyu_ID) ; call erreur(status,.TRUE.,"inq_gphibdyu_ID")
  status = NF90_INQ_VARID(fidcoeff,"gphit",gphibdyt_ID) ; call erreur(status,.TRUE.,"inq_gphibdyt_ID")
  status = NF90_INQ_VARID(fidcoeff,"glamv",glambdyv_ID) ; call erreur(status,.TRUE.,"inq_glambdyv_ID")
  status = NF90_INQ_VARID(fidcoeff,"glamu",glambdyu_ID) ; call erreur(status,.TRUE.,"inq_glambdyu_ID")
  status = NF90_INQ_VARID(fidcoeff,"glamt",glambdyt_ID) ; call erreur(status,.TRUE.,"inq_glambdyt_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgNv",wgNv_ID)   ; call erreur(status,.TRUE.,"inq_wgNv_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgMv",wgMv_ID)   ; call erreur(status,.TRUE.,"inq_wgMv_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgSv",wgSv_ID)   ; call erreur(status,.TRUE.,"inq_wgSv_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgEv",wgEv_ID)   ; call erreur(status,.TRUE.,"inq_wgEv_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgCv",wgCv_ID)   ; call erreur(status,.TRUE.,"inq_wgCv_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgWv",wgWv_ID)   ; call erreur(status,.TRUE.,"inq_wgWv_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgNf",wgNf_ID)   ; call erreur(status,.TRUE.,"inq_wgNf_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgMf",wgMf_ID)   ; call erreur(status,.TRUE.,"inq_wgMf_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgSf",wgSf_ID)   ; call erreur(status,.TRUE.,"inq_wgSf_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgEf",wgEf_ID)   ; call erreur(status,.TRUE.,"inq_wgEf_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgCf",wgCf_ID)   ; call erreur(status,.TRUE.,"inq_wgCf_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgWf",wgWf_ID)   ; call erreur(status,.TRUE.,"inq_wgWf_ID")
  status = NF90_INQ_VARID(fidcoeff,"angYv",angYv_ID) ; call erreur(status,.TRUE.,"inq_angYv_ID")
  status = NF90_INQ_VARID(fidcoeff,"angXv",angXv_ID) ; call erreur(status,.TRUE.,"inq_angXv_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgNu",wgNu_ID)   ; call erreur(status,.TRUE.,"inq_wgNu_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgMu",wgMu_ID)   ; call erreur(status,.TRUE.,"inq_wgMu_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgSu",wgSu_ID)   ; call erreur(status,.TRUE.,"inq_wgSu_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgEu",wgEu_ID)   ; call erreur(status,.TRUE.,"inq_wgEu_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgCu",wgCu_ID)   ; call erreur(status,.TRUE.,"inq_wgCu_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgWu",wgWu_ID)   ; call erreur(status,.TRUE.,"inq_wgWu_ID")
  status = NF90_INQ_VARID(fidcoeff,"angYu",angYu_ID) ; call erreur(status,.TRUE.,"inq_angYu_ID")
  status = NF90_INQ_VARID(fidcoeff,"angXu",angXu_ID) ; call erreur(status,.TRUE.,"inq_angXu_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgNt",wgNt_ID)   ; call erreur(status,.TRUE.,"inq_wgNt_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgMt",wgMt_ID)   ; call erreur(status,.TRUE.,"inq_wgMt_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgSt",wgSt_ID)   ; call erreur(status,.TRUE.,"inq_wgSt_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgEt",wgEt_ID)   ; call erreur(status,.TRUE.,"inq_wgEt_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgCt",wgCt_ID)   ; call erreur(status,.TRUE.,"inq_wgCt_ID")
  status = NF90_INQ_VARID(fidcoeff,"wgWt",wgWt_ID)   ; call erreur(status,.TRUE.,"inq_wgWt_ID")
  status = NF90_INQ_VARID(fidcoeff,"angYt",angYt_ID) ; call erreur(status,.TRUE.,"inq_angYt_ID")
  status = NF90_INQ_VARID(fidcoeff,"angXt",angXt_ID) ; call erreur(status,.TRUE.,"inq_angXt_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjNv",zjNv_ID)   ; call erreur(status,.TRUE.,"inq_zjNv_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziNv",ziNv_ID)   ; call erreur(status,.TRUE.,"inq_ziNv_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjMv",zjMv_ID)   ; call erreur(status,.TRUE.,"inq_zjMv_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziMv",ziMv_ID)   ; call erreur(status,.TRUE.,"inq_ziMv_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjSv",zjSv_ID)   ; call erreur(status,.TRUE.,"inq_zjSv_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziSv",ziSv_ID)   ; call erreur(status,.TRUE.,"inq_ziSv_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjEv",zjEv_ID)   ; call erreur(status,.TRUE.,"inq_zjEv_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziEv",ziEv_ID)   ; call erreur(status,.TRUE.,"inq_ziEv_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjCv",zjCv_ID)   ; call erreur(status,.TRUE.,"inq_zjCv_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziCv",ziCv_ID)   ; call erreur(status,.TRUE.,"inq_ziCv_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjWv",zjWv_ID)   ; call erreur(status,.TRUE.,"inq_zjWv_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziWv",ziWv_ID)   ; call erreur(status,.TRUE.,"inq_ziWv_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjNf",zjNf_ID)   ; call erreur(status,.TRUE.,"inq_zjNf_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziNf",ziNf_ID)   ; call erreur(status,.TRUE.,"inq_ziNf_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjMf",zjMf_ID)   ; call erreur(status,.TRUE.,"inq_zjMf_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziMf",ziMf_ID)   ; call erreur(status,.TRUE.,"inq_ziMf_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjSf",zjSf_ID)   ; call erreur(status,.TRUE.,"inq_zjSf_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziSf",ziSf_ID)   ; call erreur(status,.TRUE.,"inq_ziSf_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjEf",zjEf_ID)   ; call erreur(status,.TRUE.,"inq_zjEf_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziEf",ziEf_ID)   ; call erreur(status,.TRUE.,"inq_ziEf_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjCf",zjCf_ID)   ; call erreur(status,.TRUE.,"inq_zjCf_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziCf",ziCf_ID)   ; call erreur(status,.TRUE.,"inq_ziCf_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjWf",zjWf_ID)   ; call erreur(status,.TRUE.,"inq_zjWf_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziWf",ziWf_ID)   ; call erreur(status,.TRUE.,"inq_ziWf_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjNu",zjNu_ID)   ; call erreur(status,.TRUE.,"inq_zjNu_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziNu",ziNu_ID)   ; call erreur(status,.TRUE.,"inq_ziNu_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjMu",zjMu_ID)   ; call erreur(status,.TRUE.,"inq_zjMu_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziMu",ziMu_ID)   ; call erreur(status,.TRUE.,"inq_ziMu_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjSu",zjSu_ID)   ; call erreur(status,.TRUE.,"inq_zjSu_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziSu",ziSu_ID)   ; call erreur(status,.TRUE.,"inq_ziSu_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjEu",zjEu_ID)   ; call erreur(status,.TRUE.,"inq_zjEu_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziEu",ziEu_ID)   ; call erreur(status,.TRUE.,"inq_ziEu_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjCu",zjCu_ID)   ; call erreur(status,.TRUE.,"inq_zjCu_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziCu",ziCu_ID)   ; call erreur(status,.TRUE.,"inq_ziCu_ID")
  status = NF90_INQ_VARID(fidcoeff,"zjWu",zjWu_ID)   ; call erreur(status,.TRUE.,"inq_zjWu_ID")
  status = NF90_INQ_VARID(fidcoeff,"ziWu",ziWu_ID)   ; call erreur(status,.TRUE.,"inq_ziWu_ID")
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
  status = NF90_INQ_VARID(fidcoeff,"tmask",tmask_ID) ; call erreur(status,.TRUE.,"inq_tmask_ID")  
  status = NF90_INQ_VARID(fidcoeff,"umask",umask_ID) ; call erreur(status,.TRUE.,"inq_umask_ID")  
  status = NF90_INQ_VARID(fidcoeff,"vmask",vmask_ID) ; call erreur(status,.TRUE.,"inq_vmask_ID")
  status = NF90_INQ_VARID(fidcoeff,"e3u",e3ubdy_ID)  ; call erreur(status,.TRUE.,"inq_e3ubdy_ID")
  status = NF90_INQ_VARID(fidcoeff,"e3v",e3vbdy_ID)  ; call erreur(status,.TRUE.,"inq_e3vbdy_ID") 
  status = NF90_INQ_VARID(fidcoeff,"e2u",e2ubdy_ID)  ; call erreur(status,.TRUE.,"inq_e2ubdy_ID")
  status = NF90_INQ_VARID(fidcoeff,"e1v",e1vbdy_ID)  ; call erreur(status,.TRUE.,"inq_e1vbdy_ID") 
 
  !-- Variables Values
  status = NF90_GET_VAR(fidcoeff,gphibdyv_ID,gphibdyv) ; call erreur(status,.TRUE.,"getvar_gphibdyv")
  status = NF90_GET_VAR(fidcoeff,gphibdyu_ID,gphibdyu) ; call erreur(status,.TRUE.,"getvar_gphibdyu")
  status = NF90_GET_VAR(fidcoeff,gphibdyt_ID,gphibdyt) ; call erreur(status,.TRUE.,"getvar_gphibdyt")
  status = NF90_GET_VAR(fidcoeff,glambdyv_ID,glambdyv) ; call erreur(status,.TRUE.,"getvar_glambdyv")
  status = NF90_GET_VAR(fidcoeff,glambdyu_ID,glambdyu) ; call erreur(status,.TRUE.,"getvar_glambdyu")
  status = NF90_GET_VAR(fidcoeff,glambdyt_ID,glambdyt) ; call erreur(status,.TRUE.,"getvar_glambdyt")
  status = NF90_GET_VAR(fidcoeff,wgNv_ID,wgNv)   ; call erreur(status,.TRUE.,"getvar_wgNv")
  status = NF90_GET_VAR(fidcoeff,wgMv_ID,wgMv)   ; call erreur(status,.TRUE.,"getvar_wgMv")
  status = NF90_GET_VAR(fidcoeff,wgSv_ID,wgSv)   ; call erreur(status,.TRUE.,"getvar_wgSv")
  status = NF90_GET_VAR(fidcoeff,wgEv_ID,wgEv)   ; call erreur(status,.TRUE.,"getvar_wgEv")
  status = NF90_GET_VAR(fidcoeff,wgCv_ID,wgCv)   ; call erreur(status,.TRUE.,"getvar_wgCv")
  status = NF90_GET_VAR(fidcoeff,wgWv_ID,wgWv)   ; call erreur(status,.TRUE.,"getvar_wgWv")
  status = NF90_GET_VAR(fidcoeff,angYv_ID,angYv) ; call erreur(status,.TRUE.,"getvar_angYv")
  status = NF90_GET_VAR(fidcoeff,angXv_ID,angXv) ; call erreur(status,.TRUE.,"getvar_angXv")
  status = NF90_GET_VAR(fidcoeff,wgNu_ID,wgNu)   ; call erreur(status,.TRUE.,"getvar_wgNu")
  status = NF90_GET_VAR(fidcoeff,wgMu_ID,wgMu)   ; call erreur(status,.TRUE.,"getvar_wgMu")
  status = NF90_GET_VAR(fidcoeff,wgSu_ID,wgSu)   ; call erreur(status,.TRUE.,"getvar_wgSu")
  status = NF90_GET_VAR(fidcoeff,wgEu_ID,wgEu)   ; call erreur(status,.TRUE.,"getvar_wgEu")
  status = NF90_GET_VAR(fidcoeff,wgCu_ID,wgCu)   ; call erreur(status,.TRUE.,"getvar_wgCu")
  status = NF90_GET_VAR(fidcoeff,wgWu_ID,wgWu)   ; call erreur(status,.TRUE.,"getvar_wgWu")
  status = NF90_GET_VAR(fidcoeff,angYu_ID,angYu) ; call erreur(status,.TRUE.,"getvar_angYu")
  status = NF90_GET_VAR(fidcoeff,angXu_ID,angXu) ; call erreur(status,.TRUE.,"getvar_angXu")
  status = NF90_GET_VAR(fidcoeff,wgNt_ID,wgNt)   ; call erreur(status,.TRUE.,"getvar_wgNt")
  status = NF90_GET_VAR(fidcoeff,wgMt_ID,wgMt)   ; call erreur(status,.TRUE.,"getvar_wgMt")
  status = NF90_GET_VAR(fidcoeff,wgSt_ID,wgSt)   ; call erreur(status,.TRUE.,"getvar_wgSt")
  status = NF90_GET_VAR(fidcoeff,wgEt_ID,wgEt)   ; call erreur(status,.TRUE.,"getvar_wgEt")
  status = NF90_GET_VAR(fidcoeff,wgCt_ID,wgCt)   ; call erreur(status,.TRUE.,"getvar_wgCt")
  status = NF90_GET_VAR(fidcoeff,wgWt_ID,wgWt)   ; call erreur(status,.TRUE.,"getvar_wgWt")
  status = NF90_GET_VAR(fidcoeff,angYt_ID,angYt) ; call erreur(status,.TRUE.,"getvar_angYt")
  status = NF90_GET_VAR(fidcoeff,angXt_ID,angXt) ; call erreur(status,.TRUE.,"getvar_angXt")
  status = NF90_GET_VAR(fidcoeff,wgNf_ID,wgNf)   ; call erreur(status,.TRUE.,"getvar_wgNf")
  status = NF90_GET_VAR(fidcoeff,wgMf_ID,wgMf)   ; call erreur(status,.TRUE.,"getvar_wgMf")
  status = NF90_GET_VAR(fidcoeff,wgSf_ID,wgSf)   ; call erreur(status,.TRUE.,"getvar_wgSf")
  status = NF90_GET_VAR(fidcoeff,wgEf_ID,wgEf)   ; call erreur(status,.TRUE.,"getvar_wgEf")
  status = NF90_GET_VAR(fidcoeff,wgCf_ID,wgCf)   ; call erreur(status,.TRUE.,"getvar_wgCf")
  status = NF90_GET_VAR(fidcoeff,wgWf_ID,wgWf)   ; call erreur(status,.TRUE.,"getvar_wgWf")
  status = NF90_GET_VAR(fidcoeff,zjNv_ID,zjNv)   ; call erreur(status,.TRUE.,"getvar_zjNv")
  status = NF90_GET_VAR(fidcoeff,ziNv_ID,ziNv)   ; call erreur(status,.TRUE.,"getvar_ziNv")
  status = NF90_GET_VAR(fidcoeff,zjMv_ID,zjMv)   ; call erreur(status,.TRUE.,"getvar_zjMv")
  status = NF90_GET_VAR(fidcoeff,ziMv_ID,ziMv)   ; call erreur(status,.TRUE.,"getvar_ziMv")
  status = NF90_GET_VAR(fidcoeff,zjSv_ID,zjSv)   ; call erreur(status,.TRUE.,"getvar_zjSv")
  status = NF90_GET_VAR(fidcoeff,ziSv_ID,ziSv)   ; call erreur(status,.TRUE.,"getvar_ziSv")
  status = NF90_GET_VAR(fidcoeff,zjEv_ID,zjEv)   ; call erreur(status,.TRUE.,"getvar_zjEv")
  status = NF90_GET_VAR(fidcoeff,ziEv_ID,ziEv)   ; call erreur(status,.TRUE.,"getvar_ziEv")
  status = NF90_GET_VAR(fidcoeff,zjCv_ID,zjCv)   ; call erreur(status,.TRUE.,"getvar_zjCv")
  status = NF90_GET_VAR(fidcoeff,ziCv_ID,ziCv)   ; call erreur(status,.TRUE.,"getvar_ziCv")
  status = NF90_GET_VAR(fidcoeff,zjWv_ID,zjWv)   ; call erreur(status,.TRUE.,"getvar_zjWv")
  status = NF90_GET_VAR(fidcoeff,ziWv_ID,ziWv)   ; call erreur(status,.TRUE.,"getvar_ziWv")
  status = NF90_GET_VAR(fidcoeff,zjNu_ID,zjNu)   ; call erreur(status,.TRUE.,"getvar_zjNu")
  status = NF90_GET_VAR(fidcoeff,ziNu_ID,ziNu)   ; call erreur(status,.TRUE.,"getvar_ziNu")
  status = NF90_GET_VAR(fidcoeff,zjMu_ID,zjMu)   ; call erreur(status,.TRUE.,"getvar_zjMu")
  status = NF90_GET_VAR(fidcoeff,ziMu_ID,ziMu)   ; call erreur(status,.TRUE.,"getvar_ziMu")
  status = NF90_GET_VAR(fidcoeff,zjSu_ID,zjSu)   ; call erreur(status,.TRUE.,"getvar_zjSu")
  status = NF90_GET_VAR(fidcoeff,ziSu_ID,ziSu)   ; call erreur(status,.TRUE.,"getvar_ziSu")
  status = NF90_GET_VAR(fidcoeff,zjEu_ID,zjEu)   ; call erreur(status,.TRUE.,"getvar_zjEu")
  status = NF90_GET_VAR(fidcoeff,ziEu_ID,ziEu)   ; call erreur(status,.TRUE.,"getvar_ziEu")
  status = NF90_GET_VAR(fidcoeff,zjCu_ID,zjCu)   ; call erreur(status,.TRUE.,"getvar_zjCu")
  status = NF90_GET_VAR(fidcoeff,ziCu_ID,ziCu)   ; call erreur(status,.TRUE.,"getvar_ziCu")
  status = NF90_GET_VAR(fidcoeff,zjWu_ID,zjWu)   ; call erreur(status,.TRUE.,"getvar_zjWu")
  status = NF90_GET_VAR(fidcoeff,ziWu_ID,ziWu)   ; call erreur(status,.TRUE.,"getvar_ziWu")
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
  status = NF90_GET_VAR(fidcoeff,zjNf_ID,zjNf)   ; call erreur(status,.TRUE.,"getvar_zjNf")
  status = NF90_GET_VAR(fidcoeff,ziNf_ID,ziNf)   ; call erreur(status,.TRUE.,"getvar_ziNf")
  status = NF90_GET_VAR(fidcoeff,zjMf_ID,zjMf)   ; call erreur(status,.TRUE.,"getvar_zjMf")
  status = NF90_GET_VAR(fidcoeff,ziMf_ID,ziMf)   ; call erreur(status,.TRUE.,"getvar_ziMf")
  status = NF90_GET_VAR(fidcoeff,zjSf_ID,zjSf)   ; call erreur(status,.TRUE.,"getvar_zjSf")
  status = NF90_GET_VAR(fidcoeff,ziSf_ID,ziSf)   ; call erreur(status,.TRUE.,"getvar_ziSf")
  status = NF90_GET_VAR(fidcoeff,zjEf_ID,zjEf)   ; call erreur(status,.TRUE.,"getvar_zjEf")
  status = NF90_GET_VAR(fidcoeff,ziEf_ID,ziEf)   ; call erreur(status,.TRUE.,"getvar_ziEf")
  status = NF90_GET_VAR(fidcoeff,zjCf_ID,zjCf)   ; call erreur(status,.TRUE.,"getvar_zjCf")
  status = NF90_GET_VAR(fidcoeff,ziCf_ID,ziCf)   ; call erreur(status,.TRUE.,"getvar_ziCf")
  status = NF90_GET_VAR(fidcoeff,zjWf_ID,zjWf)   ; call erreur(status,.TRUE.,"getvar_zjWf")
  status = NF90_GET_VAR(fidcoeff,ziWf_ID,ziWf)   ; call erreur(status,.TRUE.,"getvar_ziWf")
  status = NF90_GET_VAR(fidcoeff,tmask_ID,msktbdy) ; call erreur(status,.TRUE.,"getvar_tmask")
  status = NF90_GET_VAR(fidcoeff,umask_ID,mskubdy) ; call erreur(status,.TRUE.,"getvar_umask")
  status = NF90_GET_VAR(fidcoeff,vmask_ID,mskvbdy) ; call erreur(status,.TRUE.,"getvar_vmask")
  status = NF90_GET_VAR(fidcoeff,e3ubdy_ID,e3ubdy) ; call erreur(status,.TRUE.,"getvar_e3ubdy")
  status = NF90_GET_VAR(fidcoeff,e3vbdy_ID,e3vbdy) ; call erreur(status,.TRUE.,"getvar_e3vbdy")                                            
  status = NF90_GET_VAR(fidcoeff,e2ubdy_ID,e2ubdy) ; call erreur(status,.TRUE.,"getvar_e2ubdy")
  status = NF90_GET_VAR(fidcoeff,e1vbdy_ID,e1vbdy) ; call erreur(status,.TRUE.,"getvar_e1vbdy")
 
  status = NF90_CLOSE(fidcoeff)                      
  call erreur(status,.TRUE.,"end_reading_interp_coeff")     

  !===========

  DO yr=yeari,yearf

    !=========================================================
    ! 3a- FIND NUMBER OF FILES (i.e. TIME STEPS) FOR YEAR yr
    !=========================================================

    mt = 0
    do month=1,12 
    do day=1,31
    
       write(file_globT,121) yr, month, day
       121 FORMAT('/srv/ccrc/data22/z3381502/ORCA025.L75-MJM95-S/ORCA025.L75-MJM95_y',i4.4,'m',i2.2,'d',i2.2,'_gridT.nc')

       inquire(file=file_globT, exist=existfileT)

       if ( existfileT ) mt=mt+1

    enddo
    enddo

    !write(*,301) mt, yr
    !301 FORMAT(' ---> ',i3,' input files for year ',i4)

    ALLOCATE( tobdy(mx,my,mz,mt) , sobdy(mx,my,mz,mt) , uobdy(mx,my,mz,mt) , vobdy(mx,my,mz,mt) )
    tobdy(:,:,:,:) = 9999.9
    sobdy(:,:,:,:) = 9999.9
    uobdy(:,:,:,:) = 9999.9
    vobdy(:,:,:,:) = 9999.9

    !=====

    it = 0
    do month=1,12
    do day=1,31

      write(file_globT,221) yr, month, day
      221 FORMAT('/srv/ccrc/data22/z3381502/ORCA025.L75-MJM95-S/ORCA025.L75-MJM95_y',i4.4,'m',i2.2,'d',i2.2,'_gridT.nc')
      inquire(file=file_globT, exist=existfileT)

      write(file_globU,222) yr, month, day
      222 FORMAT('/srv/ccrc/data22/z3381502/ORCA025.L75-MJM95-S/ORCA025.L75-MJM95_y',i4.4,'m',i2.2,'d',i2.2,'_gridU.nc')
      inquire(file=file_globU, exist=existfileU)

      write(file_globV,223) yr, month, day
      223 FORMAT('/srv/ccrc/data22/z3381502/ORCA025.L75-MJM95-S/ORCA025.L75-MJM95_y',i4.4,'m',i2.2,'d',i2.2,'_gridV.nc')
      inquire(file=file_globV, exist=existfileV)   

      if ( existfileT .and. existfileU .and. existfileV ) then

        write(*,221) yr, month, day

        it = it + 1

        !=========================================================
        ! 3b- READ GLOBAL OCEAN SIMULATION FOR GIVEN YEAR
        !=========================================================


        !-----
        !-- Temperature and salinity (on gridT)

        write(*,221) yr, month, day

        status = NF90_OPEN(TRIM(file_globT),0,fidT) ; call erreur(status,.TRUE.,"start_reading_gridT") 
                                                           
        status = NF90_INQ_DIMID(fidT,"time_counter",dimID_time_counter)
        call erreur(status,.TRUE.,"inq_dimID_time_counter")
        status = NF90_INQ_DIMID(fidT,"deptht",dimID_deptht)
        call erreur(status,.TRUE.,"inq_dimID_deptht")
        status = NF90_INQ_DIMID(fidT,"y",dimID_yg)
        call erreur(status,.TRUE.,"inq_dimID_yg")
        status = NF90_INQ_DIMID(fidT,"x",dimID_xg)
        call erreur(status,.TRUE.,"inq_dimID_xg")
                                                               
        status = NF90_INQUIRE_DIMENSION(fidT,dimID_time_counter,len=mtime_counter)
        call erreur(status,.TRUE.,"inq_dim_time_counter")
        status = NF90_INQUIRE_DIMENSION(fidT,dimID_deptht,len=mdeptht)
        call erreur(status,.TRUE.,"inq_dim_deptht")
        status = NF90_INQUIRE_DIMENSION(fidT,dimID_yg,len=myTglo)
        call erreur(status,.TRUE.,"inq_dim_yg")
        status = NF90_INQUIRE_DIMENSION(fidT,dimID_xg,len=mxTglo)
        call erreur(status,.TRUE.,"inq_dim_xg")
        
        if ( it .eq. 1 ) then
          ALLOCATE(  vosaline(mxTglo,myTglo,mdeptht,mtime_counter)  ) 
          ALLOCATE(  votemper(mxTglo,myTglo,mdeptht,mtime_counter)  ) 
          ! check if the file is as expected
          if ( mtime_counter .ne. 1 ) then
            write(*,*) '!@#$%^* ERROR: the program is written for only one date per input file >>>>>> stop'
            stop
          elseif ( mdeptht .ne. mz ) then
            write(*,*) '!@#$%^* ERROR: the program is written for same vertical coordinates in input and output >>>>>> stop'
            stop
          elseif ( mxTglo .ne. jpiin .or. myTglo .ne. jpjin .or. mdeptht .ne. jpkin ) then
            write(*,*) '!@#$%^* ERROR: mask and gridT files do not have the same dimension for global simulation >>>>>>> stop'
            write(*,388) mxTglo,myTglo,mdeptht,jpiin,jpjin,jpkin
            388 FORMAT('(mxTglo,myTglo,mdeptht)=(',i4,',',i4,',',i2,') vs (jpiin,jpjin,jpkin)=(',i4,',',i4,',',i2,')')
            stop
          endif    
        endif
                             
        status = NF90_INQ_VARID(fidT,"vosaline",vosaline_ID) ; call erreur(status,.TRUE.,"inq_vosaline_ID")
        status = NF90_INQ_VARID(fidT,"votemper",votemper_ID) ; call erreur(status,.TRUE.,"inq_votemper_ID")
                                                              
        status = NF90_GET_VAR(fidT,vosaline_ID,vosaline) ; call erreur(status,.TRUE.,"getvar_vosaline")
        status = NF90_GET_VAR(fidT,votemper_ID,votemper) ; call erreur(status,.TRUE.,"getvar_votemper")
                                                      
        status = NF90_CLOSE(fidT) ; call erreur(status,.TRUE.,"complete_reading_gridT")     


        !-----
        !-- X-ocean current (on gridU)

        write(*,222) yr, month, day

        status = NF90_OPEN(TRIM(file_globU),0,fidU) ; call erreur(status,.TRUE.,"starting_reading_gridU") 
                                                           
        status = NF90_INQ_DIMID(fidU,"time_counter",dimID_time_counter)
        call erreur(status,.TRUE.,"inq_dimID_time_counter")
        status = NF90_INQ_DIMID(fidU,"depthu",dimID_depthu)
        call erreur(status,.TRUE.,"inq_dimID_depthu")
        status = NF90_INQ_DIMID(fidU,"y",dimID_y)
        call erreur(status,.TRUE.,"inq_dimID_y")
        status = NF90_INQ_DIMID(fidU,"x",dimID_xg)
        call erreur(status,.TRUE.,"inq_dimID_xg")
                                                               
        status = NF90_INQUIRE_DIMENSION(fidU,dimID_time_counter,len=mtime_counter)
        call erreur(status,.TRUE.,"inq_dim_time_counter")
        status = NF90_INQUIRE_DIMENSION(fidU,dimID_depthu,len=mdepthu)
        call erreur(status,.TRUE.,"inq_dim_depthu")
        status = NF90_INQUIRE_DIMENSION(fidU,dimID_yg,len=myUglo)
        call erreur(status,.TRUE.,"inq_dim_yg")
        status = NF90_INQUIRE_DIMENSION(fidU,dimID_xg,len=mxUglo)
        call erreur(status,.TRUE.,"inq_dim_xg")
        
        if ( it .eq. 1 ) then
          ALLOCATE( vozocrtx(mxUglo,myUglo,mdepthu,mtime_counter) )
          ! check if the file is as expected
          if ( mtime_counter .ne. 1 ) then
            write(*,*) '!@#$%^* ERROR: the program is written for only one date per input file (Ugrd) >>>>>> stop'
            stop
          elseif ( mdepthu .ne. mz ) then
            write(*,*) '!@#$%^* ERROR: the program is written for same vertical coordinates in input and output (Ugrd) >>>>>> stop'
            stop
          elseif ( mxUglo .ne. mxTglo .or. myUglo .ne. myTglo ) then
            write(*,*) '!@#$%^* ERROR: the program is written for same nb of points on T grid and on U grid >>>>>>> stop'
            stop
          endif
        endif
                       
        status = NF90_INQ_VARID(fidU,"vozocrtx",vozocrtx_ID) ;  call erreur(status,.TRUE.,"inq_vozocrtx_ID")
                                                              
        status = NF90_GET_VAR(fidU,vozocrtx_ID,vozocrtx) ; call erreur(status,.TRUE.,"getvar_vozocrtx")
                                                      
        status = NF90_CLOSE(fidU) ; call erreur(status,.TRUE.,"complete_reading_U")     

        !-----
        !-- Y-ocean current (on gridV)
   
        write(*,223) yr, month, day

        status = NF90_OPEN(TRIM(file_globV),0,fidV) ; call erreur(status,.TRUE.,"starting_reading_gridV")

        status = NF90_INQ_DIMID(fidV,"time_counter",dimID_time_counter)
        call erreur(status,.TRUE.,"inq_dimID_time_counter")
        status = NF90_INQ_DIMID(fidV,"depthv",dimID_depthv)
        call erreur(status,.TRUE.,"inq_dimID_depthv")
        status = NF90_INQ_DIMID(fidV,"y",dimID_yg)
        call erreur(status,.TRUE.,"inq_dimID_yg")
        status = NF90_INQ_DIMID(fidV,"x",dimID_xg)
        call erreur(status,.TRUE.,"inq_dimID_xg")

        status = NF90_INQUIRE_DIMENSION(fidV,dimID_time_counter,len=mtime_counter)
        call erreur(status,.TRUE.,"inq_dim_time_counter")
        status = NF90_INQUIRE_DIMENSION(fidV,dimID_depthv,len=mdepthv)
        call erreur(status,.TRUE.,"inq_dim_depthv")
        status = NF90_INQUIRE_DIMENSION(fidV,dimID_yg,len=myVglo)
        call erreur(status,.TRUE.,"inq_dim_yg")
        status = NF90_INQUIRE_DIMENSION(fidV,dimID_xg,len=mxVglo)
        call erreur(status,.TRUE.,"inq_dim_xg")

        if ( it .eq. 1 ) then
          ALLOCATE( vomecrty(mxVglo,myVglo,mdepthv,mtime_counter) )
          ! check if the file is as expected
          if ( mtime_counter .ne. 1 ) then
            write(*,*) '!@#$%^* ERROR: the program is written for only one date per input file (Vgrd) >>>>>> stop'
            stop
          elseif ( mdepthv .ne. mz ) then
            write(*,*) '!@#$%^* ERROR: the program is written for same vertical coordinates in input and output (Vgrd) >>>>>> stop'
            stop
          elseif ( mxVglo .ne. mxTglo .or. myVglo .ne. myTglo ) then
            write(*,*) '!@#$%^* ERROR: the program is written for same nb of points on T grid and on V grid >>>>>>> stop'
            stop
          endif
        endif

        status = NF90_INQ_VARID(fidV,"vomecrty",vomecrty_ID) ;  call erreur(status,.TRUE.,"inq_vomecrty_ID")

        status = NF90_GET_VAR(fidV,vomecrty_ID,vomecrty) ; call erreur(status,.TRUE.,"getvar_vomecrty")

        status = NF90_CLOSE(fidV) ; call erreur(status,.TRUE.,"complete_reading_V") 

        !=========================================================
        ! 3c- INTERPOLATE ON BDY
        !=========================================================
        ! first interpolate 3 points along i, then along j

        write(*,*) '  -----> computing interpolation...'

        if ( it .eq. 1) ALLOCATE( uu(mx,my,mz), vv(mx,my,mz) )

        do ji=1,mx
        do jj=1,my
        do jk=1,mz
         
              !==================== gridT =====================
 
              !-- zonal interpolation at Southern T-point point
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
          
              !-- zonal interpolation at Central T-point point
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
          
              !-- zonal interpolation at Northern T-point
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
          
              !-- Meridional interpolation on T-grid
              if ( abs(aS+aM+aN) .gt. eps ) then !- interpolation works perfectly well

                tobdy (ji,jj,jk,it) = ( tS*aS + tM*aM + tN*aN ) * msktbdy(ji,jj,jk) / ( aS + aM + aN )
                sobdy (ji,jj,jk,it) = ( sS*aS + sM*aM + sN*aN ) * msktbdy(ji,jj,jk) / ( aS + aM + aN ) 

              elseif ( msktbdy(ji,jj,jk) .eq. 1 ) then  !- oceanic point on regional grid but all surrounding=land on global
          
                !write(*,477) ji, jj, jk
                !477 FORMAT('Further extrapolation needed at regional T-point (',3I5,')')
                !- try various extensions always turning anti-clockwise 
                !  (to favor more or less the same direction first at 2 neighbouring location) 
                rsmax = 6 ! maximum number of point for lateral and vertical extrapolation
                          ! -> need to go far near boundaries because bathymetry has been smoothed
                do rs=1,rsmax,1
                  irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=    zjEt(ji,jj)            ; krs=    jk       !- E 
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit 
                  irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=    zjNt(ji,jj)            ; krs=    jk       !- NEE 
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs=    jk       !- NE
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=    ziEt(ji,jj)            ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs=    jk       !- NNE
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=    ziNt(ji,jj)            ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs=    jk       !- N 
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=    ziWt(ji,jj)            ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs=    jk       !- NNW 
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs=    jk       !- NW 
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=    zjNt(ji,jj)            ; krs=    jk       !- NWW 
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=    zjWt(ji,jj)            ; krs=    jk       !- W 
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=    zjSt(ji,jj)            ; krs=    jk       !- SWW
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs=    jk       !- SW
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=    ziWt(ji,jj)            ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs=    jk       !- SSW 
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=    ziSt(ji,jj)            ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs=    jk       !- S
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=    ziEt(ji,jj)            ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs=    jk       !- SSE 
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs=    jk       !- SE 
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) exit
                  irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=    zjSt(ji,jj)            ; krs=    jk       !- SEE 
                  if ( msktin( irs,jrs,krs ) .eq. 1 ) then
                    exit
                  elseif ( rs .eq. rsmax ) then 
                    write(*,*) '!@#$%^* FATAL PROBLEM !! Argh...'
                    write(*,951) ji, jj, jk
                    951 FORMAT(' >>> you need to develop code to fill T-point (',3I5,')')
                    write(*,*) ' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> STOP'
                    stop
                  endif     
                enddo !-rs
                !- 
                tobdy (ji,jj,jk,it) = votemper( irs,jrs,krs,1 )
                sobdy (ji,jj,jk,it) = vosaline( irs,jrs,krs,1 )        
 
              else

                tobdy (ji,jj,jk,it) = 0.0
                sobdy (ji,jj,jk,it) = 0.0 

              endif
         
              !==================== U grid ====================
 
              !-- zonal interpolation at Southern U-point point
              aE  = wgEu( ji,jj ) * mskuin( ziEu(ji,jj),zjSu(ji,jj),jk ) * e3uin( ziEu(ji,jj),zjSu(ji,jj),jk )
              aC  = wgCu( ji,jj ) * mskuin( ziCu(ji,jj),zjSu(ji,jj),jk ) * e3uin( ziCu(ji,jj),zjSu(ji,jj),jk )
              aW  = wgWu( ji,jj ) * mskuin( ziWu(ji,jj),zjSu(ji,jj),jk ) * e3uin( ziWu(ji,jj),zjSu(ji,jj),jk ) 
          
              uS  = (   vozocrtx( ziEu(ji,jj),zjSu(ji,jj),jk,1 ) * aE                             &
              &       + vozocrtx( ziCu(ji,jj),zjSu(ji,jj),jk,1 ) * aC                             &
              &       + vozocrtx( ziWu(ji,jj),zjSu(ji,jj),jk,1 ) * aW )  / ( aE + aC + aW + eps )
          
              aS  = ( aE + aC + aW ) * wgSu( ji,jj )
          
              !-- zonal interpolation at Central U-point point
              aE  = wgEu( ji,jj ) * mskuin( ziEu(ji,jj),zjMu(ji,jj),jk ) * e3uin( ziEu(ji,jj),zjMu(ji,jj),jk )
              aC  = wgCu( ji,jj ) * mskuin( ziCu(ji,jj),zjMu(ji,jj),jk ) * e3uin( ziCu(ji,jj),zjMu(ji,jj),jk )
              aW  = wgWu( ji,jj ) * mskuin( ziWu(ji,jj),zjMu(ji,jj),jk ) * e3uin( ziWu(ji,jj),zjMu(ji,jj),jk )
          
              uM  = (   vozocrtx( ziEu(ji,jj),zjMu(ji,jj),jk,1 ) * aE                             &
              &       + vozocrtx( ziCu(ji,jj),zjMu(ji,jj),jk,1 ) * aC                             &
              &       + vozocrtx( ziWu(ji,jj),zjMu(ji,jj),jk,1 ) * aW )  / ( aE + aC + aW + eps )
          
              aM  = ( aE + aC + aW ) * wgMu( ji,jj )
          
              !-- zonal interpolation at Northern U-point
              aE  = wgEu( ji,jj ) * mskuin( ziEu(ji,jj),zjNu(ji,jj),jk ) * e3uin( ziEu(ji,jj),zjNu(ji,jj),jk )
              aC  = wgCu( ji,jj ) * mskuin( ziCu(ji,jj),zjNu(ji,jj),jk ) * e3uin( ziCu(ji,jj),zjNu(ji,jj),jk )
              aW  = wgWu( ji,jj ) * mskuin( ziWu(ji,jj),zjNu(ji,jj),jk ) * e3uin( ziWu(ji,jj),zjNu(ji,jj),jk )
          
              uN  = (   vozocrtx( ziEu(ji,jj),zjNu(ji,jj),jk,1 ) * aE                             &
              &       + vozocrtx( ziCu(ji,jj),zjNu(ji,jj),jk,1 ) * aC                             &
              &       + vozocrtx( ziWu(ji,jj),zjNu(ji,jj),jk,1 ) * aW )  / ( aE + aC + aW + eps )
          
              aN  = ( aE + aC + aW ) * wgNu( ji,jj )
          
              !-- Meridional interpolation on U-grid
              if ( abs(aS+aM+aN) .gt. eps ) then !- interpolation works perfectly fine

                uu(ji,jj,jk) = ( uS*aS + uM*aM + uN*aN ) * mskubdy(ji,jj,jk) / ( aS + aM + aN )
 
              else

                !- By contrast with T,S velocity is set to zero when there is no related grid point on the ORCA025 grid
                !write(*,177) ji, jj, jk
                !177 FORMAT('U-point (',3I5,') is set to zero because no corresponding point on ORCA025 grid')

                uu (ji,jj,jk) = 0.0           
 
              endif

              !==================== V grid ====================
 
              !-- zonal interpolation at Southern V-point point
              aE  = wgEv( ji,jj ) * mskvin( ziEv(ji,jj),zjSv(ji,jj),jk ) * e3vin( ziEv(ji,jj),zjSv(ji,jj),jk )
              aC  = wgCv( ji,jj ) * mskvin( ziCv(ji,jj),zjSv(ji,jj),jk ) * e3vin( ziCv(ji,jj),zjSv(ji,jj),jk )
              aW  = wgWv( ji,jj ) * mskvin( ziWv(ji,jj),zjSv(ji,jj),jk ) * e3vin( ziWv(ji,jj),zjSv(ji,jj),jk ) 
          
              vS  = (   vomecrty( ziEv(ji,jj),zjSv(ji,jj),jk,1 ) * aE                             &
              &       + vomecrty( ziCv(ji,jj),zjSv(ji,jj),jk,1 ) * aC                             &
              &       + vomecrty( ziWv(ji,jj),zjSv(ji,jj),jk,1 ) * aW )  / ( aE + aC + aW + eps )
          
              aS  = ( aE + aC + aW ) * wgSv( ji,jj )
          
              !-- zonal interpolation at Central V-point point
              aE  = wgEv( ji,jj ) * mskvin( ziEv(ji,jj),zjMv(ji,jj),jk ) * e3vin( ziEv(ji,jj),zjMv(ji,jj),jk )
              aC  = wgCv( ji,jj ) * mskvin( ziCv(ji,jj),zjMv(ji,jj),jk ) * e3vin( ziCv(ji,jj),zjMv(ji,jj),jk )
              aW  = wgWv( ji,jj ) * mskvin( ziWv(ji,jj),zjMv(ji,jj),jk ) * e3vin( ziWv(ji,jj),zjMv(ji,jj),jk )
          
              vM  = (   vomecrty( ziEv(ji,jj),zjMv(ji,jj),jk,1 ) * aE                             &
              &       + vomecrty( ziCv(ji,jj),zjMv(ji,jj),jk,1 ) * aC                             &
              &       + vomecrty( ziWv(ji,jj),zjMv(ji,jj),jk,1 ) * aW )  / ( aE + aC + aW + eps )
          
              aM  = ( aE + aC + aW ) * wgMv( ji,jj )
          
              !-- zonal interpolation at Northern V-point
              aE  = wgEv( ji,jj ) * mskvin( ziEv(ji,jj),zjNv(ji,jj),jk ) * e3vin( ziEv(ji,jj),zjNv(ji,jj),jk )
              aC  = wgCv( ji,jj ) * mskvin( ziCv(ji,jj),zjNv(ji,jj),jk ) * e3vin( ziCv(ji,jj),zjNv(ji,jj),jk )
              aW  = wgWv( ji,jj ) * mskvin( ziWv(ji,jj),zjNv(ji,jj),jk ) * e3vin( ziWv(ji,jj),zjNv(ji,jj),jk )
          
              vN  = (   vomecrty( ziEv(ji,jj),zjNv(ji,jj),jk,1 ) * aE                             &
              &       + vomecrty( ziCv(ji,jj),zjNv(ji,jj),jk,1 ) * aC                             &
              &       + vomecrty( ziWv(ji,jj),zjNv(ji,jj),jk,1 ) * aW )  / ( aE + aC + aW + eps )
          
              aN  = ( aE + aC + aW ) * wgNv( ji,jj )
          
              !-- Meridional interpolation on V-grid
              if ( abs(aS+aM+aN) .gt. eps ) then !- interpolation works perfectly well

                vv(ji,jj,jk) = ( vS*aS + vM*aM + vN*aN ) * mskvbdy(ji,jj,jk) / ( aS + aM + aN )

              else

                !- By contrast with T,S velocity is set to zero when there is no related grid point on the ORCA025 grid
                !write(*,178) ji, jj, jk
                !178 FORMAT('V-point (',3I5,') is set to zero because no corresponding point on ORCA025 grid')

                vv (ji,jj,jk) = 0.0
 
              endif

        enddo
        enddo
        enddo

        !--- rotate vectors ---
        do ji=1,mx
        do jj=1,my
        do jk=1,mz
          uobdy(ji,jj,jk,it) =   uu(ji,jj,jk) * cos(angXu(ji,jj)) + vv(ji,jj,jk) * sin(angYu(ji,jj))
          vobdy(ji,jj,jk,it) = - uu(ji,jj,jk) * sin(angXv(ji,jj)) + vv(ji,jj,jk) * cos(angYv(ji,jj))
        enddo !- jk
        enddo !- jj
        enddo !- ji

        if ( it .eq. mt ) then
          DEALLOCATE(  vosaline, votemper )
          DEALLOCATE(  vomecrty, vozocrtx, uu, vv )
        endif

      endif !-- if ( existfileT .and. existfileU .and. existfileV )

    enddo !-- day
    enddo !-- month

    !=========================================================
    ! 4- WRITE OBC
    !=========================================================
     
    write(file_obc,391) TRIM(side(iside)), TRIM(conf_par), yr
    391 FORMAT('OBC_',a,'_',a,'_y',i4.4,'.nc')
                                                          
    write(*,*) '  Creating ', TRIM(file_obc)

    status = NF90_CREATE(TRIM(file_obc),or(NF90_NOCLOBBER,NF90_64BIT_OFFSET),fidobc)
    call erreur(status,.TRUE.,'create_OBC_file')                     
         
    !-- define dimensions                                                       
    status = NF90_DEF_DIM(fidobc,"t",NF90_UNLIMITED,dimID_t) ; call erreur(status,.TRUE.,"def_dimID_t")
    status = NF90_DEF_DIM(fidobc,"z",mz,dimID_z)             ; call erreur(status,.TRUE.,"def_dimID_z")
    if     ( my .eq. 1 ) then
       status = NF90_DEF_DIM(fidobc,"x",mx,dimID_XX)
       call erreur(status,.TRUE.,"def_dimID_x")
    elseif ( mx .eq. 1 ) then
       status = NF90_DEF_DIM(fidobc,"y",my,dimID_XX)
       call erreur(status,.TRUE.,"def_dimID_y")       
    else
       write(*,*) '!@#$%^* Something wrong with bdy dimension at bdy ', TRIM(side(iside)), ' >>>> stop'
       stop
    endif
                                                       
    !-- Variables definition                 
    status = NF90_DEF_VAR(fidobc,"vo",NF90_FLOAT,(/dimID_XX,dimID_z,dimID_t/),vo_ID)
    call erreur(status,.TRUE.,"def_var_vo_ID")
    status = NF90_DEF_VAR(fidobc,"uo",NF90_FLOAT,(/dimID_XX,dimID_z,dimID_t/),uo_ID)
    call erreur(status,.TRUE.,"def_var_uo_ID")
    status = NF90_DEF_VAR(fidobc,"so",NF90_FLOAT,(/dimID_XX,dimID_z,dimID_t/),so_ID)
    call erreur(status,.TRUE.,"def_var_so_ID")
    status = NF90_DEF_VAR(fidobc,"thetao",NF90_FLOAT,(/dimID_XX,dimID_z,dimID_t/),to_ID)
    call erreur(status,.TRUE.,"def_var_to_ID")
                                   
    !-- global attribute
    status = NF90_PUT_ATT(fidobc,NF90_GLOBAL,"history","Created using build_NEMO_MC25_yearly_OBC.f90")
    call erreur(status,.TRUE.,"put_att_global_ID")
                                                      
    !-- End definitions                          
    status = NF90_ENDDEF(fidobc) ; call erreur(status,.TRUE.,"fin_definition") 
                                                     
    !-- Values to put in the variables
    if     ( my .eq. 1 ) then
      status = NF90_PUT_VAR(fidobc,vo_ID,vobdy(:,1,:,:)) ; call erreur(status,.TRUE.,"var_vo_ID")
      status = NF90_PUT_VAR(fidobc,uo_ID,uobdy(:,1,:,:)) ; call erreur(status,.TRUE.,"var_uo_ID")
      status = NF90_PUT_VAR(fidobc,so_ID,sobdy(:,1,:,:)) ; call erreur(status,.TRUE.,"var_so_ID")
      status = NF90_PUT_VAR(fidobc,to_ID,tobdy(:,1,:,:)) ; call erreur(status,.TRUE.,"var_to_ID")
    elseif ( mx .eq. 1 ) then  
      status = NF90_PUT_VAR(fidobc,vo_ID,vobdy(1,:,:,:)) ; call erreur(status,.TRUE.,"var_vo_ID")
      status = NF90_PUT_VAR(fidobc,uo_ID,uobdy(1,:,:,:)) ; call erreur(status,.TRUE.,"var_uo_ID")
      status = NF90_PUT_VAR(fidobc,so_ID,sobdy(1,:,:,:)) ; call erreur(status,.TRUE.,"var_so_ID")
      status = NF90_PUT_VAR(fidobc,to_ID,tobdy(1,:,:,:)) ; call erreur(status,.TRUE.,"var_to_ID")
    endif                                                  
  
    !-- End writing file     
    status = NF90_CLOSE(fidobc) ; call erreur(status,.TRUE.,"End_write_OBC")         

    DEALLOCATE( tobdy, sobdy, uobdy, vobdy )

  ENDDO !-- yr

  DEALLOCATE(  wgNv, wgMv, wgSv  )
  DEALLOCATE(  wgEv, wgCv, wgWv  )
  DEALLOCATE(  wgNu, wgMu, wgSu  )
  DEALLOCATE(  wgEu, wgCu, wgWu  )
  DEALLOCATE(  wgNt, wgMt, wgSt  )
  DEALLOCATE(  wgEt, wgCt, wgWt  )
  DEALLOCATE(  wgNf, wgMf, wgSf  )
  DEALLOCATE(  wgEf, wgCf, wgWf  )
  DEALLOCATE(  msktbdy, mskubdy, mskvbdy )
  DEALLOCATE(  angYv, angXv, angYu, angXu, angYt, angXt  )
  DEALLOCATE(  gphibdyv, gphibdyu, gphibdyt    )
  DEALLOCATE(  glambdyv, glambdyu, glambdyt  )
  DEALLOCATE(  zjNv, ziNv, zjMv, ziMv, zjSv, ziSv  )
  DEALLOCATE(  zjEv, ziEv, zjCv, ziCv, zjWv, ziWv  )
  DEALLOCATE(  zjNu, ziNu, zjMu, ziMu, zjSu, ziSu  )
  DEALLOCATE(  zjEu, ziEu, zjCu, ziCu, zjWu, ziWu  )
  DEALLOCATE(  zjNt, ziNt, zjMt, ziMt, zjSt, ziSt  )
  DEALLOCATE(  zjEt, ziEt, zjCt, ziCt, zjWt, ziWt  )
  DEALLOCATE(  zjNf, ziNf, zjMf, ziMf, zjSf, ziSf  )
  DEALLOCATE(  zjEf, ziEf, zjCf, ziCf, zjWf, ziWf  )
  DEALLOCATE(  e3ubdy, e3vbdy, e2ubdy, e1vbdy )

ENDDO !-- iside

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
