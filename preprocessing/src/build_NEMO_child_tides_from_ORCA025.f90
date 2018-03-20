program modif                                         
                                                      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! N. Jourdain, CCRC-UNSW, Sydney                                     !
! March 2014                                                         !
!                                                                    !
! This script interpolates tide parameters from coarse to fine grid  !
!                                                                    !
! Method: conservative interpolation                                 !
!                                                                    !
! NB: coeff_3D_nest_cordex24_from_ORCA025.nc has to be created before      ! 
!                                                                    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

USE netcdf                                            
                                                      
IMPLICIT NONE                                         

!-- namelist parameters :
namelist /datactl/ file_etopo, file_runoff, file_chloro, file_tides
namelist /global/ orcadir, filemskin, filezgrin, filehgrin
namelist /parent/ conf_par, perio, max_dom, feedback, idateline, file_par_coord, file_eff_land
namelist /child/ conf_child, file_child_coord, file_child_extend
INTEGER                               :: max_dom, feedback, perio, idateline
CHARACTER(LEN=50)                     :: conf_par
CHARACTER(LEN=50)                     :: conf_child
CHARACTER(LEN=150)                    :: file_child_coord, file_child_extend, file_par_coord, file_eff_land
CHARACTER(LEN=150)                    :: file_etopo, file_runoff, file_chloro, file_tides
CHARACTER(LEN=150)                    :: orcadir, filemskin, filezgrin, filehgrin
                      
!-- Global oceanic T,S file
INTEGER                                :: fidGLOB, status, dimID_time, dimID_deptht, dimID_yglo, &
&                                         mtime, mz, myTglo, mxTglo, sn_ID, tn_ID, dimID_xglo,   &
&                                         time_ID, deptht_ID, nav_lat_ID, nav_lon_ID, fidout,    &
&                                         un_ID, vn_ID, K1_ID, M2_ID, tmaskitf_ID
CHARACTER(LEN=150)                     :: file_out                     
REAL*4,ALLOCATABLE,DIMENSION(:,:,:)    :: K1, M2
INTEGER*1,ALLOCATABLE,DIMENSION(:,:,:) :: tmaskitf

!-- input mask:
INTEGER                                  :: fidmsk, dimID_zm, jpkin
INTEGER*1,ALLOCATABLE,DIMENSION(:,:,:)   :: msktin, mskuin, mskvin 

!-- interpolation parameters
INTEGER                                :: fidcoeff, dimID_x, dimID_y, dimID_z, mx, my, jim1, jip1, jjm1, jjp1,      &
&                                         wgNt_ID, wgMt_ID, wgSt_ID, wgEt_ID, wgCt_ID, wgWt_ID, angYt_ID, angXt_ID, &
&                                         gphit_ID, glamt_ID, zkUt_ID, zjNt_ID, ziNt_ID, zjMt_ID, ziMt_ID,tmask_ID, &
&                                         zjSt_ID, ziSt_ID, zjEt_ID, ziEt_ID, zjCt_ID, ziCt_ID, zjWt_ID, ziWt_ID,   &
&                                         wgNu_ID, wgMu_ID, wgSu_ID, wgEu_ID, wgCu_ID, wgWu_ID, angYu_ID, angXu_ID, &
&                                         gphiu_ID, glamu_ID, zkUu_ID, zjNu_ID, ziNu_ID, zjMu_ID, ziMu_ID,umask_ID, &
&                                         zjSu_ID, ziSu_ID, zjEu_ID, ziEu_ID, zjCu_ID, ziCu_ID, zjWu_ID, ziWu_ID,   &
&                                         wgNv_ID, wgMv_ID, wgSv_ID, wgEv_ID, wgCv_ID, wgWv_ID, angYv_ID, angXv_ID, &
&                                         gphiv_ID, glamv_ID, zkUv_ID, zjNv_ID, ziNv_ID, zjMv_ID, ziMv_ID,vmask_ID, &
&                                         zjSv_ID, ziSv_ID, zjEv_ID, ziEv_ID, zjCv_ID, ziCv_ID, zjWv_ID, ziWv_ID
CHARACTER(LEN=150)                     :: file_coeff
INTEGER*4,ALLOCATABLE,DIMENSION(:,:)   :: zjNt, ziNt, zjMt, ziMt, zjSt, ziSt, zjEt, ziEt, zjCt, ziCt, zjWt, ziWt,   &
&                                         zjNu, ziNu, zjMu, ziMu, zjSu, ziSu, zjEu, ziEu, zjCu, ziCu, zjWu, ziWu,   &
&                                         zjNv, ziNv, zjMv, ziMv, zjSv, ziSv, zjEv, ziEv, zjCv, ziCv, zjWv, ziWv
REAL*4,ALLOCATABLE,DIMENSION(:,:)      :: angYt, angXt, gphit, glamt, &
&                                         angYu, angXu, gphiu, glamu, &
&                                         angYv, angXv, gphiv, glamv
INTEGER*2,ALLOCATABLE,DIMENSION(:,:,:) :: tmask, umask, vmask
REAL*4,ALLOCATABLE,DIMENSION(:,:)      :: wgNt, wgMt, wgSt, wgEt, wgCt, wgWt, &       
&                                         wgNu, wgMu, wgSu, wgEu, wgCu, wgWu, &
&                                         wgNv, wgMv, wgSv, wgEv, wgCv, wgWv

!-- Regional initial state
INTEGER                                :: ji, jj, jk, jl, irs, jrs, krs, rs, rsmax, rz, rzmax
REAL*4                                 :: eps, tNO, tM, tS, sNO, sM, sS, aE, aC, aW, aS, aM, aN 
REAL*4                                 :: zN, zM, zS, yN, yM, yS, xN, xM, xS
REAL*4,ALLOCATABLE,DIMENSION(:,:,:)    :: K1o, M2o, tmaskitfo

LOGICAL                                :: iout

!==============================================================
! 0- INITIALIZATIONS 
!==============================================================                                               

!- read namelist values
OPEN (UNIT=1, FILE='namelist_precpl' )
READ (UNIT=1, NML=datactl)
READ (UNIT=1, NML=global)
READ (UNIT=1, NML=parent)
READ (UNIT=1, NML=child)
CLOSE(1)

!-- interpolation parameters (must be calculated before running this script)
write(file_coeff,212) TRIM(conf_child)
212 FORMAT('coeff_3D_',a,'_from_ORCA025.nc')

!-- output file = 3D oceanic initial state on the regional grid
write(file_out,213) TRIM(conf_par)
213 FORMAT('1_alltides_',a,'.nc')

  write(*,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
  write(*,*) ' build_NEMO_child_tides_from_ORCA025 :               '
  write(*,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                 '
  write(*,*) '                                                     '
  write(*,*) '  parent config : ', TRIM(conf_par)
  write(*,*) '  child  config : ', TRIM(conf_child)
  write(*,*) '                                                     '

!==============================================================
! 1- READ ON COARSE GRID 
!==============================================================
                               
write(*,*) 'Reading ', TRIM(file_tides)

status = NF90_OPEN(TRIM(file_tides),0,fidGLOB) ; call erreur(status,.TRUE.,"read_global_TS") 
                                                  
status = NF90_INQ_DIMID(fidGLOB,"time_counter",dimID_time) ; call erreur(status,.TRUE.,"inq_dimID_time")
status = NF90_INQ_DIMID(fidGLOB,"y",dimID_yglo)            ; call erreur(status,.TRUE.,"inq_dimID_yglo")
status = NF90_INQ_DIMID(fidGLOB,"x",dimID_xglo)            ; call erreur(status,.TRUE.,"inq_dimID_xglo")
                                                      
status = NF90_INQUIRE_DIMENSION(fidGLOB,dimID_time,len=mtime)  ; call erreur(status,.TRUE.,"inq_dim_time")
status = NF90_INQUIRE_DIMENSION(fidGLOB,dimID_yglo,len=myTglo) ; call erreur(status,.TRUE.,"inq_dim_yglo")
status = NF90_INQUIRE_DIMENSION(fidGLOB,dimID_xglo,len=mxTglo) ; call erreur(status,.TRUE.,"inq_dim_xglo")
                      
ALLOCATE(  K1(mxTglo,myTglo,mtime)   )
ALLOCATE(  M2(mxTglo,myTglo,mtime)   )
ALLOCATE(  tmaskitf(mxTglo,myTglo,mtime)   )
                        
status = NF90_INQ_VARID(fidGLOB,"K1",K1_ID) ; call erreur(status,.TRUE.,"inq_K1_ID")
status = NF90_INQ_VARID(fidGLOB,"M2",M2_ID) ; call erreur(status,.TRUE.,"inq_M2_ID")
status = NF90_INQ_VARID(fidGLOB,"tmaskitf",tmaskitf_ID) ; call erreur(status,.TRUE.,"inq_tmaskitf_ID")                                            
         
status = NF90_GET_VAR(fidGLOB,K1_ID,K1) ; call erreur(status,.TRUE.,"getvar_K1")
status = NF90_GET_VAR(fidGLOB,M2_ID,M2) ; call erreur(status,.TRUE.,"getvar_M2")                                             
status = NF90_GET_VAR(fidGLOB,tmaskitf_ID,tmaskitf) ; call erreur(status,.TRUE.,"getvar_tmaskitf")

status = NF90_CLOSE(fidGLOB) ; call erreur(status,.TRUE.,"fin_lecture")     

!=====================================================================
! 2- READ VERTICAL GRID AND MASK ON COARSE DOMAIN
!=====================================================================
                       
write(*,*) 'Reading ', TRIM(filemskin)
                                             
status = NF90_OPEN(TRIM(filemskin),0,fidmsk)          
call erreur(status,.TRUE.,"read_mask") 
                                             
status = NF90_INQ_DIMID(fidmsk,"z",dimID_zm)
call erreur(status,.TRUE.,"inq_dimID_zm")
                                                 
status = NF90_INQUIRE_DIMENSION(fidmsk,dimID_zm,len=mz)
call erreur(status,.TRUE.,"inq_dim_z")
                
ALLOCATE(  msktin(mxTglo,myTglo,mz), mskuin(mxTglo,myTglo,mz), mskvin(mxTglo,myTglo,mz)  )
                   
status = NF90_INQ_VARID(fidmsk,"tmask",tmask_ID) ; call erreur(status,.TRUE.,"inq_tmask_ID")
status = NF90_INQ_VARID(fidmsk,"umask",umask_ID) ; call erreur(status,.TRUE.,"inq_umask_ID")
status = NF90_INQ_VARID(fidmsk,"vmask",vmask_ID) ; call erreur(status,.TRUE.,"inq_vmask_ID")

status = NF90_GET_VAR(fidmsk,tmask_ID,msktin) ; call erreur(status,.TRUE.,"getvar_tmask")
status = NF90_GET_VAR(fidmsk,umask_ID,mskuin) ; call erreur(status,.TRUE.,"getvar_umask")
status = NF90_GET_VAR(fidmsk,vmask_ID,mskvin) ; call erreur(status,.TRUE.,"getvar_vmask")

status = NF90_CLOSE(fidmsk) ; call erreur(status,.TRUE.,"fin_lecture_mask")     

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
           
ALLOCATE(  tmask(mx,my,mz), umask(mx,my,mz), vmask(mx,my,mz) )
ALLOCATE(  angYt(mx,my), angXt(mx,my), gphit(mx,my), glamt(mx,my)  )
ALLOCATE(  angYu(mx,my), angXu(mx,my), gphiu(mx,my), glamu(mx,my)  )
ALLOCATE(  angYv(mx,my), angXv(mx,my), gphiv(mx,my), glamv(mx,my)  )
ALLOCATE(  wgNt(mx,my), wgMt(mx,my), wgSt(mx,my)  ) 
ALLOCATE(  wgEt(mx,my), wgCt(mx,my), wgWt(mx,my)  ) 
ALLOCATE(  wgNu(mx,my), wgMu(mx,my), wgSu(mx,my)  )
ALLOCATE(  wgEu(mx,my), wgCu(mx,my), wgWu(mx,my)  )
ALLOCATE(  wgNv(mx,my), wgMv(mx,my), wgSv(mx,my)  )
ALLOCATE(  wgEv(mx,my), wgCv(mx,my), wgWv(mx,my)  )
ALLOCATE(  zjNt(mx,my), ziNt(mx,my), zjMt(mx,my), ziMt(mx,my)  ) 
ALLOCATE(  zjSt(mx,my), ziSt(mx,my), zjEt(mx,my), ziEt(mx,my)  ) 
ALLOCATE(  zjCt(mx,my), ziCt(mx,my), zjWt(mx,my), ziWt(mx,my)  ) 
ALLOCATE(  zjNu(mx,my), ziNu(mx,my), zjMu(mx,my), ziMu(mx,my)  )
ALLOCATE(  zjSu(mx,my), ziSu(mx,my), zjEu(mx,my), ziEu(mx,my)  )
ALLOCATE(  zjCu(mx,my), ziCu(mx,my), zjWu(mx,my), ziWu(mx,my)  )
ALLOCATE(  zjNv(mx,my), ziNv(mx,my), zjMv(mx,my), ziMv(mx,my)  )
ALLOCATE(  zjSv(mx,my), ziSv(mx,my), zjEv(mx,my), ziEv(mx,my)  )
ALLOCATE(  zjCv(mx,my), ziCv(mx,my), zjWv(mx,my), ziWv(mx,my)  )

!- fine grid masks                 
status = NF90_INQ_VARID(fidcoeff,"tmask",tmask_ID) ; call erreur(status,.TRUE.,"inq_tmask_ID")
status = NF90_INQ_VARID(fidcoeff,"umask",umask_ID) ; call erreur(status,.TRUE.,"inq_umask_ID")
status = NF90_INQ_VARID(fidcoeff,"vmask",vmask_ID) ; call erreur(status,.TRUE.,"inq_vmask_ID")
!- interpolation weights
status = NF90_INQ_VARID(fidcoeff,"wgNt",wgNt_ID)   ; call erreur(status,.TRUE.,"inq_wgNt_ID")
status = NF90_INQ_VARID(fidcoeff,"wgMt",wgMt_ID)   ; call erreur(status,.TRUE.,"inq_wgMt_ID")
status = NF90_INQ_VARID(fidcoeff,"wgSt",wgSt_ID)   ; call erreur(status,.TRUE.,"inq_wgSt_ID")
status = NF90_INQ_VARID(fidcoeff,"wgEt",wgEt_ID)   ; call erreur(status,.TRUE.,"inq_wgEt_ID")
status = NF90_INQ_VARID(fidcoeff,"wgCt",wgCt_ID)   ; call erreur(status,.TRUE.,"inq_wgCt_ID")
status = NF90_INQ_VARID(fidcoeff,"wgWt",wgWt_ID)   ; call erreur(status,.TRUE.,"inq_wgWt_ID")
status = NF90_INQ_VARID(fidcoeff,"wgNu",wgNu_ID)   ; call erreur(status,.TRUE.,"inq_wgNu_ID")
status = NF90_INQ_VARID(fidcoeff,"wgMu",wgMu_ID)   ; call erreur(status,.TRUE.,"inq_wgMu_ID")
status = NF90_INQ_VARID(fidcoeff,"wgSu",wgSu_ID)   ; call erreur(status,.TRUE.,"inq_wgSu_ID")
status = NF90_INQ_VARID(fidcoeff,"wgEu",wgEu_ID)   ; call erreur(status,.TRUE.,"inq_wgEu_ID")
status = NF90_INQ_VARID(fidcoeff,"wgCu",wgCu_ID)   ; call erreur(status,.TRUE.,"inq_wgCu_ID")
status = NF90_INQ_VARID(fidcoeff,"wgWu",wgWu_ID)   ; call erreur(status,.TRUE.,"inq_wgWu_ID")
status = NF90_INQ_VARID(fidcoeff,"wgNv",wgNv_ID)   ; call erreur(status,.TRUE.,"inq_wgNv_ID")
status = NF90_INQ_VARID(fidcoeff,"wgMv",wgMv_ID)   ; call erreur(status,.TRUE.,"inq_wgMv_ID")
status = NF90_INQ_VARID(fidcoeff,"wgSv",wgSv_ID)   ; call erreur(status,.TRUE.,"inq_wgSv_ID")
status = NF90_INQ_VARID(fidcoeff,"wgEv",wgEv_ID)   ; call erreur(status,.TRUE.,"inq_wgEv_ID")
status = NF90_INQ_VARID(fidcoeff,"wgCv",wgCv_ID)   ; call erreur(status,.TRUE.,"inq_wgCv_ID")
status = NF90_INQ_VARID(fidcoeff,"wgWv",wgWv_ID)   ; call erreur(status,.TRUE.,"inq_wgWv_ID")
!- fine grid coordinates and orientation compared to coarse grid (angle)
status = NF90_INQ_VARID(fidcoeff,"angYt",angYt_ID) ; call erreur(status,.TRUE.,"inq_angYt_ID")
status = NF90_INQ_VARID(fidcoeff,"angXt",angXt_ID) ; call erreur(status,.TRUE.,"inq_angXt_ID")
status = NF90_INQ_VARID(fidcoeff,"gphit",gphit_ID) ; call erreur(status,.TRUE.,"inq_gphit_ID")
status = NF90_INQ_VARID(fidcoeff,"glamt",glamt_ID) ; call erreur(status,.TRUE.,"inq_glamt_ID")
status = NF90_INQ_VARID(fidcoeff,"angYu",angYu_ID) ; call erreur(status,.TRUE.,"inq_angYu_ID")
status = NF90_INQ_VARID(fidcoeff,"angXu",angXu_ID) ; call erreur(status,.TRUE.,"inq_angXu_ID")
status = NF90_INQ_VARID(fidcoeff,"gphiu",gphiu_ID) ; call erreur(status,.TRUE.,"inq_gphiu_ID")
status = NF90_INQ_VARID(fidcoeff,"glamu",glamu_ID) ; call erreur(status,.TRUE.,"inq_glamu_ID")
status = NF90_INQ_VARID(fidcoeff,"angYv",angYv_ID) ; call erreur(status,.TRUE.,"inq_angYv_ID")
status = NF90_INQ_VARID(fidcoeff,"angXv",angXv_ID) ; call erreur(status,.TRUE.,"inq_angXv_ID")
status = NF90_INQ_VARID(fidcoeff,"gphiv",gphiv_ID) ; call erreur(status,.TRUE.,"inq_gphiv_ID")
status = NF90_INQ_VARID(fidcoeff,"glamv",glamv_ID) ; call erreur(status,.TRUE.,"inq_glamv_ID")
!- coordinates of coarse grid points used in the interpolation
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

!-
status = NF90_GET_VAR(fidcoeff,tmask_ID,tmask) ; call erreur(status,.TRUE.,"getvar_tmask")
status = NF90_GET_VAR(fidcoeff,umask_ID,umask) ; call erreur(status,.TRUE.,"getvar_umask")
status = NF90_GET_VAR(fidcoeff,vmask_ID,vmask) ; call erreur(status,.TRUE.,"getvar_vmask")
!-
status = NF90_GET_VAR(fidcoeff,wgNt_ID,wgNt)   ; call erreur(status,.TRUE.,"getvar_wgNt")
status = NF90_GET_VAR(fidcoeff,wgMt_ID,wgMt)   ; call erreur(status,.TRUE.,"getvar_wgMt")
status = NF90_GET_VAR(fidcoeff,wgSt_ID,wgSt)   ; call erreur(status,.TRUE.,"getvar_wgSt")
status = NF90_GET_VAR(fidcoeff,wgEt_ID,wgEt)   ; call erreur(status,.TRUE.,"getvar_wgEt")
status = NF90_GET_VAR(fidcoeff,wgCt_ID,wgCt)   ; call erreur(status,.TRUE.,"getvar_wgCt")
status = NF90_GET_VAR(fidcoeff,wgWt_ID,wgWt)   ; call erreur(status,.TRUE.,"getvar_wgWt")
status = NF90_GET_VAR(fidcoeff,wgNu_ID,wgNu)   ; call erreur(status,.TRUE.,"getvar_wgNu")
status = NF90_GET_VAR(fidcoeff,wgMu_ID,wgMu)   ; call erreur(status,.TRUE.,"getvar_wgMu")
status = NF90_GET_VAR(fidcoeff,wgSu_ID,wgSu)   ; call erreur(status,.TRUE.,"getvar_wgSu")
status = NF90_GET_VAR(fidcoeff,wgEu_ID,wgEu)   ; call erreur(status,.TRUE.,"getvar_wgEu")
status = NF90_GET_VAR(fidcoeff,wgCu_ID,wgCu)   ; call erreur(status,.TRUE.,"getvar_wgCu")
status = NF90_GET_VAR(fidcoeff,wgWu_ID,wgWu)   ; call erreur(status,.TRUE.,"getvar_wgWu")
status = NF90_GET_VAR(fidcoeff,wgNv_ID,wgNv)   ; call erreur(status,.TRUE.,"getvar_wgNv")
status = NF90_GET_VAR(fidcoeff,wgMv_ID,wgMv)   ; call erreur(status,.TRUE.,"getvar_wgMv")
status = NF90_GET_VAR(fidcoeff,wgSv_ID,wgSv)   ; call erreur(status,.TRUE.,"getvar_wgSv")
status = NF90_GET_VAR(fidcoeff,wgEv_ID,wgEv)   ; call erreur(status,.TRUE.,"getvar_wgEv")
status = NF90_GET_VAR(fidcoeff,wgCv_ID,wgCv)   ; call erreur(status,.TRUE.,"getvar_wgCv")
status = NF90_GET_VAR(fidcoeff,wgWv_ID,wgWv)   ; call erreur(status,.TRUE.,"getvar_wgWv")
!-
status = NF90_GET_VAR(fidcoeff,angYt_ID,angYt) ; call erreur(status,.TRUE.,"getvar_angYt")
status = NF90_GET_VAR(fidcoeff,angXt_ID,angXt) ; call erreur(status,.TRUE.,"getvar_angXt")
status = NF90_GET_VAR(fidcoeff,gphit_ID,gphit) ; call erreur(status,.TRUE.,"getvar_gphit")
status = NF90_GET_VAR(fidcoeff,glamt_ID,glamt) ; call erreur(status,.TRUE.,"getvar_glamt")
status = NF90_GET_VAR(fidcoeff,angYu_ID,angYu) ; call erreur(status,.TRUE.,"getvar_angYu")
status = NF90_GET_VAR(fidcoeff,angXu_ID,angXu) ; call erreur(status,.TRUE.,"getvar_angXu")
status = NF90_GET_VAR(fidcoeff,gphiu_ID,gphiu) ; call erreur(status,.TRUE.,"getvar_gphiu")
status = NF90_GET_VAR(fidcoeff,glamu_ID,glamu) ; call erreur(status,.TRUE.,"getvar_glamu")
status = NF90_GET_VAR(fidcoeff,angYv_ID,angYv) ; call erreur(status,.TRUE.,"getvar_angYv")
status = NF90_GET_VAR(fidcoeff,angXv_ID,angXv) ; call erreur(status,.TRUE.,"getvar_angXv")
status = NF90_GET_VAR(fidcoeff,gphiv_ID,gphiv) ; call erreur(status,.TRUE.,"getvar_gphiv")
status = NF90_GET_VAR(fidcoeff,glamv_ID,glamv) ; call erreur(status,.TRUE.,"getvar_glamv")
!-
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
                            
status = NF90_CLOSE(fidcoeff) ; call erreur(status,.TRUE.,"end_read_interp_coef")     

!=====================================================================
! 3- INTERPOLATION ON FINE GRID
!===================================================================== 

write(*,*) 'Processing to interpolation of tidal parameters on fine grid...'

eps = 1.e-9  !-- to avoid division by zero

ALLOCATE( K1o(mx,my,mtime)        )
ALLOCATE( M2o(mx,my,mtime)        )
ALLOCATE( tmaskitfo(mx,my,mtime)  )

do ji=1,mx
do jj=1,my
do jl=1,mtime

    !-- zonal interpolation at Southern point
    aE  = wgEt( ji,jj ) * msktin( ziEt(ji,jj),zjSt(ji,jj),1 )
    aC  = wgCt( ji,jj ) * msktin( ziCt(ji,jj),zjSt(ji,jj),1 )
    aW  = wgWt( ji,jj ) * msktin( ziWt(ji,jj),zjSt(ji,jj),1 )

    xS  = (   K1( ziEt(ji,jj),zjSt(ji,jj),jl ) * aE                             &
    &       + K1( ziCt(ji,jj),zjSt(ji,jj),jl ) * aC                             &
    &       + K1( ziWt(ji,jj),zjSt(ji,jj),jl ) * aW )  / ( aE + aC + aW + eps )

    yS  = (   M2( ziEt(ji,jj),zjSt(ji,jj),jl ) * aE                             &
    &       + M2( ziCt(ji,jj),zjSt(ji,jj),jl ) * aC                             &
    &       + M2( ziWt(ji,jj),zjSt(ji,jj),jl ) * aW )  / ( aE + aC + aW + eps )

    zS  = (   tmaskitf( ziEt(ji,jj),zjSt(ji,jj),jl ) * aE                             &
    &       + tmaskitf( ziCt(ji,jj),zjSt(ji,jj),jl ) * aC                             &
    &       + tmaskitf( ziWt(ji,jj),zjSt(ji,jj),jl ) * aW )  / ( aE + aC + aW + eps )

    aS  = ( aE + aC + aW ) * wgSt( ji,jj )

    !-- zonal interpolation at Central point
    aE  = wgEt( ji,jj ) * msktin( ziEt(ji,jj),zjMt(ji,jj),1 )
    aC  = wgCt( ji,jj ) * msktin( ziCt(ji,jj),zjMt(ji,jj),1 )
    aW  = wgWt( ji,jj ) * msktin( ziWt(ji,jj),zjMt(ji,jj),1 )

    xM  = (   K1( ziEt(ji,jj),zjMt(ji,jj),jl ) * aE                             &
    &       + K1( ziCt(ji,jj),zjMt(ji,jj),jl ) * aC                             &
    &       + K1( ziWt(ji,jj),zjMt(ji,jj),jl ) * aW )  / ( aE + aC + aW + eps )

    yM  = (   M2( ziEt(ji,jj),zjMt(ji,jj),jl ) * aE                             &
    &       + M2( ziCt(ji,jj),zjMt(ji,jj),jl ) * aC                             &
    &       + M2( ziWt(ji,jj),zjMt(ji,jj),jl ) * aW )  / ( aE + aC + aW + eps )

    zM  = (   tmaskitf( ziEt(ji,jj),zjMt(ji,jj),jl ) * aE                             &
    &       + tmaskitf( ziCt(ji,jj),zjMt(ji,jj),jl ) * aC                             &
    &       + tmaskitf( ziWt(ji,jj),zjMt(ji,jj),jl ) * aW )  / ( aE + aC + aW + eps )

    aM  = ( aE + aC + aW ) * wgMt( ji,jj )

    !-- zonal interpolation at Northern point
    aE  = wgEt( ji,jj ) * msktin( ziEt(ji,jj),zjNt(ji,jj),1 )
    aC  = wgCt( ji,jj ) * msktin( ziCt(ji,jj),zjNt(ji,jj),1 )
    aW  = wgWt( ji,jj ) * msktin( ziWt(ji,jj),zjNt(ji,jj),1 )

    xN  = (   K1( ziEt(ji,jj),zjNt(ji,jj),jl ) * aE                             &
    &       + K1( ziCt(ji,jj),zjNt(ji,jj),jl ) * aC                             &
    &       + K1( ziWt(ji,jj),zjNt(ji,jj),jl ) * aW )  / ( aE + aC + aW + eps )

    yN  = (   M2( ziEt(ji,jj),zjNt(ji,jj),jl ) * aE                             &
    &       + M2( ziCt(ji,jj),zjNt(ji,jj),jl ) * aC                             &
    &       + M2( ziWt(ji,jj),zjNt(ji,jj),jl ) * aW )  / ( aE + aC + aW + eps )

    zN  = (   tmaskitf( ziEt(ji,jj),zjNt(ji,jj),jl ) * aE                             &
    &       + tmaskitf( ziCt(ji,jj),zjNt(ji,jj),jl ) * aC                             &
    &       + tmaskitf( ziWt(ji,jj),zjNt(ji,jj),jl ) * aW )  / ( aE + aC + aW + eps )

    aN  = ( aE + aC + aW ) * wgNt( ji,jj )

    !-- Meridional interpolation
    if ( abs(aS+aM+aN) .gt. eps ) then

       K1o       (ji,jj,jl) = ( xS*aS + xM*aM + xN*aN ) * tmask(ji,jj,1) / ( aS + aM + aN )
       M2o       (ji,jj,jl) = ( yS*aS + yM*aM + yN*aN ) * tmask(ji,jj,1) / ( aS + aM + aN )
       tmaskitfo (ji,jj,jl) = ( zS*aS + zM*aM + zN*aN ) * tmask(ji,jj,1) / ( aS + aM + aN )

    elseif ( tmask(ji,jj,1) .eq. 1 ) then !- oceanic point on regional grid but all land points on global grid

       !- try various extensions always turning anti-clockwise (then going upward) 
       !  (to favor more or less the same direction first at 2 neighbouring location) 
       rsmax = 8 ! maximum number of point for lateral and vertical extrapolation
                 ! -> need to go far near boundaries because bathymetry has been smoothed
       iout=.FALSE.
       do rs=1,rsmax,1
         irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=    zjEt(ji,jj)            ; krs= 1 !- E 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=    zjNt(ji,jj)            ; krs= 1 !- NEE 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs= 1 !- NE
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=    ziEt(ji,jj)            ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs= 1 !- NNE
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=    ziNt(ji,jj)            ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs= 1 !- N 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=    ziWt(ji,jj)            ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs= 1 !- NNW 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=MIN(zjNt(ji,jj)+rs,myTglo) ; krs= 1 !- NW 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=    zjNt(ji,jj)            ; krs= 1 !- NWW 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=    zjWt(ji,jj)            ; krs= 1 !- W 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=    zjSt(ji,jj)            ; krs= 1 !- SWW
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MAX(ziWt(ji,jj)-rs, 1)     ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs= 1 !- SW
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=    ziWt(ji,jj)            ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs= 1 !- SSW 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=    ziSt(ji,jj)            ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs= 1 !- S
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=    ziEt(ji,jj)            ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs= 1 !- SSE 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=MAX(zjSt(ji,jj)-rs, 1)     ; krs= 1 !- SE 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then ; iout=.TRUE. ; exit ; endif
         irs=MIN(ziEt(ji,jj)+rs,mxTglo) ; jrs=    zjSt(ji,jj)            ; krs= 1 !- SEE 
         if ( msktin( irs,jrs,krs ) .eq. 1 ) then
           iout=.TRUE.
           exit
         elseif ( rs .eq. rsmax ) then
           write(*,*) '!@#$%^* FATAL PROBLEM !! Argh...'
           write(*,*) 'ji, jj =', ji, jj
           write(*,*) ' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> STOP'
           stop
         endif 
       enddo !- rs
       !-
       write(*,107) ji, jj, 1, irs,jrs,krs, rs
       107 FORMAT('[K1,M2] Further extrapolation at point (',3i4,') -> filled with global (',3i5,'), with rs=',i2.2) 
       K1o       (ji,jj,jl) = K1       ( irs,jrs,jl )
       M2o       (ji,jj,jl) = M2       ( irs,jrs,jl )
       tmaskitfo (ji,jj,jl) = tmaskitf ( irs,jrs,jl )

    else

       K1o       (ji,jj,jl) = 0.0
       M2o       (ji,jj,jl) = 0.0
       tmaskitfo (ji,jj,jl) = 0.0

    endif
 
enddo
enddo
enddo

DEALLOCATE( tmaskitf )
ALLOCATE( tmaskitf(mx,my,mtime) )
where( tmaskitfo(:,:,:) .ge. 0.5 )
  tmaskitf(:,:,:) = 1
elsewhere
  tmaskitf(:,:,:) = 0
endwhere


!=====================================================================
! 5- WRITE ON FINE GRID
!===================================================================== 
                                                   
write(*,*) 'Creating ', TRIM(file_out)
 
status = NF90_CREATE(TRIM(file_out),or(NF90_NOCLOBBER,NF90_64BIT_OFFSET),fidout) ; call erreur(status,.TRUE.,'create')                     
                                                      
!-- dimensions 
status = NF90_DEF_DIM(fidout,"y",my,dimID_y)                ; call erreur(status,.TRUE.,"def_dimID_y")
status = NF90_DEF_DIM(fidout,"x",mx,dimID_x)                ; call erreur(status,.TRUE.,"def_dimID_x")
                                                    
!-- variables
status = NF90_DEF_VAR(fidout,"K1",NF90_FLOAT,(/dimID_x,dimID_y/),K1_ID); call erreur(status,.TRUE.,"def_K1_ID")
status = NF90_DEF_VAR(fidout,"M2",NF90_FLOAT,(/dimID_x,dimID_y/),M2_ID); call erreur(status,.TRUE.,"def_M2_ID")
status = NF90_DEF_VAR(fidout,"tmaskitf",NF90_BYTE,(/dimID_x,dimID_y/),tmaskitf_ID); call erreur(status,.TRUE.,"def_tmaskitf_ID")

status = NF90_PUT_ATT(fidout,NF90_GLOBAL,"history","created using build_NEMO_child_tides_from_ORCA025.f90")
call erreur(status,.TRUE.,"put_att_global")
                                            
status = NF90_ENDDEF(fidout) ; call erreur(status,.TRUE.,"fin_definition") 
                                            
!-- Values
status = NF90_PUT_VAR(fidout,K1_ID,K1o(:,:,1))            ; call erreur(status,.TRUE.,"var_K1_ID")
status = NF90_PUT_VAR(fidout,M2_ID,M2o(:,:,1))            ; call erreur(status,.TRUE.,"var_M2_ID") 
status = NF90_PUT_VAR(fidout,tmaskitf_ID,tmaskitf(:,:,1)) ; call erreur(status,.TRUE.,"var_tmaskitf_ID")

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
