program buildobc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! by N. Jourdain, on 29-MAY-2013, at CCRC-UNSW, Sydney                    !
!                                                                         !
! 0- USER'S CHOOICES                                                      !
! 1- READ INPUT GRID (i.e. from global ocean model)                       !
! 2- READ OUTPUT GRID (i.e. grid of the regional oceanic configuration)   !
! 3- CALCULATE WEIGHTS FOR OBC FILES (coord of surrounding pts + weights) !
!    3.1 find the closest T point and coefficients                        !
!    3.2 find the closest U point and coefficients                        !
!    3.3 find the closest V point and coefficients                        !
!    3.4 find the closest F point and coefficients
! 4- WRITE EVERYTHING NEEDED FOR INTERPOLATION IN A NETCDF FILE           !
!                                                                         !
! > creates one netcdf file for each boundary with coeff for interp       !
!   e.g. coeff_OBC_north_XXXX_from_ORCA025_L75.nc                         !
!                                                                         !
! rq: does not take mask or partial steps into account at this stage      !
!     (done in the following script)                                      !
!                                                                         !
! WARNING : for the moment, it will only work well if the target grid     !
!           is finer or no more than 3-4 times coarser than the global    !
!           ocean grid.                                                   !
!                                                                         !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

USE netcdf

IMPLICIT NONE

!-- namelist parameters :
namelist /global/ orcadir,filemskin,filezgrin,filehgrin
namelist /parent/ conf_par, perio, max_dom, feedback, idateline, file_par_coord, file_eff_land
INTEGER                               :: max_dom, feedback, perio, idateline
CHARACTER(LEN=150)                    :: file_par_coord, file_eff_land
CHARACTER(LEN=50)                     :: conf_par
CHARACTER(LEN=150)                    :: orcadir, filemskin, filezgrin, filehgrin

!--
CHARACTER(LEN=5),ALLOCATABLE,DIMENSION(:) :: side
INTEGER                                   :: status, iside, nside, ji, jj, jk
REAL*4                                    :: ra, rad

!-- input horizontal grid:
INTEGER                                  :: fidhgr, dimID_y, dimID_x, jpjin, jpiin, gphif_ID, gphiv_ID,dimID_yf,dimID_xf,&
&                                           gphiu_ID, gphit_ID, glamf_ID, glamv_ID, glamu_ID, glamt_ID, jiin, jjin, jkin,&
&                                           e1t_ID, e2t_ID, e1u_ID, e2u_ID, e1v_ID, e2v_ID, e1f_ID, e2f_ID
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: gphifin, gphivin, gphiuin, gphitin, glamfin, glamvin, glamuin, glamtin,      &
&                                           zglamfin, zglamvin, zglamtin, zglamuin, e1tin, e2tin, e1uin, e2uin, e1vin,   &
&                                           e2vin, e1fin, e2fin
REAL*4                                   :: anginX, anginY

!-- input mask:
INTEGER                                  :: fidmsk, dimID_z, jpkin, vmask_ID, umask_ID, tmask_ID
INTEGER*1,ALLOCATABLE,DIMENSION(:,:,:)   :: mskvin, mskuin, msktin   
INTEGER*1,ALLOCATABLE,DIMENSION(:,:)     :: psimskin

!-- input vertical grid:
INTEGER                                  :: fidzgr, e3v_ID, e3u_ID, e3t_ID, hdept_ID, e3t_0_ID, gdept_0_ID
REAL*4,ALLOCATABLE,DIMENSION(:)          :: e3t_0in, gdept_0in
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: hdeptin
REAL*4,ALLOCATABLE,DIMENSION(:,:,:)      :: e3win, e3vin, e3uin, e3tin

!-- output grid (mesh_mask file is read here and must have been created before) :
INTEGER                                  :: fidgridout, mtout, jpkout, jpjout, jpiout,                                  &
&                                           fmask_ID, e3w_0out_ID, e3t_0out_ID,                                         &
&                                           gdepw_0out_ID, gdept_0out_ID, e3wout_ID, e3vout_ID, e3uout_ID, e3tout_ID,   &
&                                           ffout_ID, e2fout_ID, e2vout_ID, e2uout_ID, e2tout_ID, e1fout_ID, e1vout_ID, &
&                                           e1uout_ID, e1tout_ID, gdepwout_ID, gdepvout_ID, gdepuout_ID, gdeptout_ID,   &
&                                           gphifout_ID, gphivout_ID, gphiuout_ID, gphitout_ID, glamfout_ID,            &
&                                           glamvout_ID, glamtout_ID, glamuout_ID 
CHARACTER(LEN=150)                       :: file_mesh_mask_par                     
REAL*8,ALLOCATABLE,DIMENSION(:)          :: e3w_0out, e3t_0out, gdepw_0out, gdept_0out        
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: gphifout, gphivout, gphiuout, gphitout, glamfout, glamvout, glamtout,       &
&                                           zglamfout, zglamvout, zglamtout, glamuout, zglamuout
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: angoutXt, angoutXu, angoutXv, angoutXf, angoutYt, angoutYu, angoutYv, angoutYf
REAL*8,ALLOCATABLE,DIMENSION(:,:)        :: ffout, e2fout, e2vout, e2uout, e2tout, e1fout, e1vout, e1uout, e1tout      
INTEGER*1,ALLOCATABLE,DIMENSION(:,:,:)   :: fmaskout, vmaskout, umaskout, tmaskout   
REAL*4,ALLOCATABLE,DIMENSION(:,:,:)      :: gdepwout, gdepvout, gdepuout, gdeptout     
REAL*8,ALLOCATABLE,DIMENSION(:,:,:)      :: e3wout, e3vout, e3uout, e3tout    

!-- OBCs :

INTEGER                                  :: iofft, ioffu, ioffv, iofff, jpicnt, jofft, joffu, joffv, jofff, jpjcnt,    &
&                                           jpicntf, jpjcntf, jibdy, jjbdy, jkbdy, lonbdyt_ID, lonbdyu_ID,             &
&                                           lonbdyv_ID, lonbdyf_ID, latbdyt_ID, latbdyu_ID, latbdyv_ID, latbdyf_ID,    &
&                                           ziWt_ID, zjWt_ID, ziCt_ID, zjCt_ID, ziEt_ID, zjEt_ID, ziSt_ID, zjSt_ID,    &
&                                           ziMt_ID, zjMt_ID, ziNt_ID, zjNt_ID, zkUt_ID, wgWt_ID, wgCt_ID, wgEt_ID,    &
&                                           wgSt_ID, wgMt_ID, wgNt_ID, angleXt_ID, angleYt_ID, ziWu_ID, zjWu_ID,       &
&                                           angleXu_ID, angleYu_ID, angleXv_ID, angleYv_ID, e3ubdy_ID, e3vbdy_ID,      &
&                                           ziCu_ID, zjCu_ID, ziEu_ID, zjEu_ID, ziSu_ID, zjSu_ID, ziMu_ID, zjMu_ID,    &
&                                           ziNu_ID, zjNu_ID, zkUu_ID, wgWu_ID, wgCu_ID, wgEu_ID, wgSu_ID, wgMu_ID,    &
&                                           wgNu_ID, angbdyXu_ID, angbdyYu_ID, ziWv_ID, zjWv_ID, ziCv_ID, zjCv_ID,     &
&                                           ziEv_ID, zjEv_ID, ziSv_ID, zjSv_ID, ziMv_ID, zjMv_ID, ziNv_ID, zjNv_ID,    &
&                                           zkUv_ID, wgWv_ID, wgCv_ID, wgEv_ID, wgSv_ID, wgMv_ID, wgNv_ID, angbdyXv_ID,&
&                                           angbdyYv_ID, ziWf_ID, zjWf_ID, ziCf_ID, zjCf_ID, ziEf_ID, zjEf_ID, ziSf_ID,&
&                                           zjSf_ID, ziMf_ID, zjMf_ID, ziNf_ID, zjNf_ID, zkUf_ID, wgWf_ID, wgCf_ID,    &
&                                           wgEf_ID, wgSf_ID, wgMf_ID, wgNf_ID, angbdyXf_ID, angbdyYf_ID, fidOBC,      &
&                                           e2ubdy_ID, e1vbdy_ID
REAL*4                                   :: dist, distmin
REAL*8,ALLOCATABLE,DIMENSION(:)          :: e3w_0bdy, e3t_0bdy, gdepw_0bdy, gdept_0bdy
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: lonbdyt, lonbdyu, lonbdyv, lonbdyf, latbdyt, latbdyu, latbdyv, latbdyf,    &
&                                           angbdyXt, angbdyXu, angbdyXv, angbdyXf, angbdyYt, angbdyYu, angbdyYv,      &
&                                           angbdyYf, zlonbdyt, angleXt, angleYt, angleXu, angleYu, angleXv, angleYv,  &
&                                           e1tbdy, e2tbdy, e1ubdy, e2ubdy, e1vbdy, e2vbdy, e1fbdy, e2fbdy,            &
&                                           zlonbdyu, zlonbdyv, zlonbdyf, angleYf, angleXf
INTEGER,ALLOCATABLE,DIMENSION(:,:)       :: ziWt, zjWt, ziCt, zjCt, ziEt, zjEt, ziSt, zjSt, ziMt, zjMt, ziNt, zjNt,    &
&                                           ziWu, zjWu, ziCu, zjCu, ziEu, zjEu, ziSu, zjSu, ziMu, zjMu, ziNu, zjNu,    &
&                                           ziWv, zjWv, ziCv, zjCv, ziEv, zjEv, ziSv, zjSv, ziMv, zjMv, ziNv, zjNv,    &
&                                           ziWf, zjWf, ziCf, zjCf, ziEf, zjEf, ziSf, zjSf, ziMf, zjMf, ziNf, zjNf,    &
&                                           zzit, zzjt, zzitm1, zzjtm1, zzitp1, zzjtp1,                                &
&                                           zziu, zzju, zzium1, zzjum1, zziup1, zzjup1,                                &
&                                           zziv, zzjv, zzivm1, zzjvm1, zzivp1, zzjvp1,                                &
&                                           zzif, zzjf, zzifm1, zzjfm1, zzifp1, zzjfp1
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: wgWt, wgCt, wgEt, wgSt, wgMt, wgNt, wgWf, wgCf, wgEf, wgSf, wgMf, wgNf,    &
&                                           wgWu, wgCu, wgEu, wgSu, wgMu, wgNu, wgWv, wgCv, wgEv, wgSv, wgMv, wgNv
REAL*4,ALLOCATABLE,DIMENSION(:,:,:)      :: tmskbdy, umskbdy, vmskbdy,                                                 &
&                                           gdepwbdy, gdepvbdy, gdepubdy, gdeptbdy
REAL*8,ALLOCATABLE,DIMENSION(:,:,:)      :: e3wbdy, e3vbdy, e3ubdy, e3tbdy
CHARACTER(LEN=150)                       :: file_bdy

!==================================================================================
! 0- Initializations
!==================================================================================

!- default (modified further if found in namelist_precpl) :
max_dom  = 1
feedback = 0

!- read namelist values
OPEN (UNIT=1, FILE='namelist_precpl' )
READ (UNIT=1, NML=global)
READ (UNIT=1, NML=parent)
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

!- earth radius (meter), must agree with NEMO file phycst.F90
ra = 6371229.0

!- deg to rad conversion (same as phycst.F90)
rad = 3.141592653589793 / 180.0

!-- name of the mesh/mask file corresponding to the grid for which OBCs are calculated (must be created before)
write(file_mesh_mask_par,121) TRIM(conf_par)
121 FORMAT('mesh_mask_',a,'.nc')

  write(*,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
  write(*,*) ' build_NEMO_OBC_intrp_coeff :                        '
  write(*,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~                          '
  write(*,*) '                                                     '
  write(*,*) '        config : ', TRIM(conf_par)
  write(*,*) '                                                     '

!==================================================================================
! 1- READ INPUT GRID (i.e. from global ocean model)
!==================================================================================

!-- Read horizontal input grid (global)

write(*,*) '  Reading ', TRIM(filehgrin)

status = NF90_OPEN(TRIM(filehgrin),0,fidhgr)          
call erreur(status,.TRUE.,"read global horizontal grid") 
                                       
status = NF90_INQ_DIMID(fidhgr,"y",dimID_y)
call erreur(status,.TRUE.,"inq_dimID_y")
status = NF90_INQ_DIMID(fidhgr,"x",dimID_x)
call erreur(status,.TRUE.,"inq_dimID_x")
                                           
status = NF90_INQUIRE_DIMENSION(fidhgr,dimID_y,len=jpjin)
call erreur(status,.TRUE.,"inq_dim_y")
status = NF90_INQUIRE_DIMENSION(fidhgr,dimID_x,len=jpiin)
call erreur(status,.TRUE.,"inq_dim_x")

!--         
ALLOCATE(  glamtin(jpiin,jpjin) , gphitin(jpiin,jpjin) , zglamtin(jpiin,jpjin)  )
ALLOCATE(  glamuin(jpiin,jpjin) , gphiuin(jpiin,jpjin) , zglamuin(jpiin,jpjin)  )
ALLOCATE(  glamvin(jpiin,jpjin) , gphivin(jpiin,jpjin) , zglamvin(jpiin,jpjin)  ) 
ALLOCATE(  glamfin(jpiin,jpjin) , gphifin(jpiin,jpjin) , zglamfin(jpiin,jpjin)  )

ALLOCATE(  e1tin(jpiin,jpjin) , e2tin(jpiin,jpjin) )
ALLOCATE(  e1uin(jpiin,jpjin) , e2uin(jpiin,jpjin) )
ALLOCATE(  e1vin(jpiin,jpjin) , e2vin(jpiin,jpjin) )
ALLOCATE(  e1fin(jpiin,jpjin) , e2fin(jpiin,jpjin) )

!--
status = NF90_INQ_VARID(fidhgr,"e1t",e1t_ID) ; call erreur(status,.TRUE.,"inq_e1t_ID")
status = NF90_INQ_VARID(fidhgr,"e2t",e2t_ID) ; call erreur(status,.TRUE.,"inq_e2t_ID")
status = NF90_INQ_VARID(fidhgr,"e1u",e1u_ID) ; call erreur(status,.TRUE.,"inq_e1u_ID")
status = NF90_INQ_VARID(fidhgr,"e2u",e2u_ID) ; call erreur(status,.TRUE.,"inq_e2u_ID")
status = NF90_INQ_VARID(fidhgr,"e1v",e1v_ID) ; call erreur(status,.TRUE.,"inq_e1v_ID")
status = NF90_INQ_VARID(fidhgr,"e2v",e2v_ID) ; call erreur(status,.TRUE.,"inq_e2v_ID")
status = NF90_INQ_VARID(fidhgr,"e1f",e1f_ID) ; call erreur(status,.TRUE.,"inq_e1f_ID")
status = NF90_INQ_VARID(fidhgr,"e2f",e2f_ID) ; call erreur(status,.TRUE.,"inq_e2f_ID")

status = NF90_INQ_VARID(fidhgr,"glamt",glamt_ID) ; call erreur(status,.TRUE.,"inq_glamt_ID")
status = NF90_INQ_VARID(fidhgr,"glamu",glamu_ID) ; call erreur(status,.TRUE.,"inq_glamu_ID")
status = NF90_INQ_VARID(fidhgr,"glamv",glamv_ID) ; call erreur(status,.TRUE.,"inq_glamv_ID")
status = NF90_INQ_VARID(fidhgr,"glamf",glamf_ID) ; call erreur(status,.TRUE.,"inq_glamf_ID")
status = NF90_INQ_VARID(fidhgr,"gphit",gphit_ID) ; call erreur(status,.TRUE.,"inq_gphit_ID")
status = NF90_INQ_VARID(fidhgr,"gphiu",gphiu_ID) ; call erreur(status,.TRUE.,"inq_gphiu_ID")
status = NF90_INQ_VARID(fidhgr,"gphiv",gphiv_ID) ; call erreur(status,.TRUE.,"inq_gphiv_ID")
status = NF90_INQ_VARID(fidhgr,"gphif",gphif_ID) ; call erreur(status,.TRUE.,"inq_gphif_ID")

!--
status = NF90_GET_VAR(fidhgr,e1t_ID,e1tin) ; call erreur(status,.TRUE.,"getvar_e1t")
status = NF90_GET_VAR(fidhgr,e2t_ID,e2tin) ; call erreur(status,.TRUE.,"getvar_e2t")
status = NF90_GET_VAR(fidhgr,e1u_ID,e1uin) ; call erreur(status,.TRUE.,"getvar_e1u")
status = NF90_GET_VAR(fidhgr,e2u_ID,e2uin) ; call erreur(status,.TRUE.,"getvar_e2u")
status = NF90_GET_VAR(fidhgr,e1v_ID,e1vin) ; call erreur(status,.TRUE.,"getvar_e1v")
status = NF90_GET_VAR(fidhgr,e2v_ID,e2vin) ; call erreur(status,.TRUE.,"getvar_e2v")
status = NF90_GET_VAR(fidhgr,e1f_ID,e1fin) ; call erreur(status,.TRUE.,"getvar_e1f")
status = NF90_GET_VAR(fidhgr,e2f_ID,e2fin) ; call erreur(status,.TRUE.,"getvar_e2f")

status = NF90_GET_VAR(fidhgr,glamt_ID,glamtin) ; call erreur(status,.TRUE.,"getvar_glamt")
status = NF90_GET_VAR(fidhgr,gphit_ID,gphitin) ; call erreur(status,.TRUE.,"getvar_gphit")
status = NF90_GET_VAR(fidhgr,glamu_ID,glamuin) ; call erreur(status,.TRUE.,"getvar_glamu")
status = NF90_GET_VAR(fidhgr,gphiu_ID,gphiuin) ; call erreur(status,.TRUE.,"getvar_gphiu")
status = NF90_GET_VAR(fidhgr,glamv_ID,glamvin) ; call erreur(status,.TRUE.,"getvar_glamv")
status = NF90_GET_VAR(fidhgr,gphiv_ID,gphivin) ; call erreur(status,.TRUE.,"getvar_gphiv")
status = NF90_GET_VAR(fidhgr,glamf_ID,glamfin) ; call erreur(status,.TRUE.,"getvar_glamf")
status = NF90_GET_VAR(fidhgr,gphif_ID,gphifin) ; call erreur(status,.TRUE.,"getvar_gphif")
                                          
status = NF90_CLOSE(fidhgr)                      
call erreur(status,.TRUE.,"fin_lecture_horizontal_grid") 

!----------------------------------------------------------------------------------
!-- Read input mask (global)
              
write(*,*) '  Reading ', TRIM(filemskin)
                                             
status = NF90_OPEN(TRIM(filemskin),0,fidmsk)          
call erreur(status,.TRUE.,"read global mask") 
                                             
status = NF90_INQ_DIMID(fidmsk,"z",dimID_z)
call erreur(status,.TRUE.,"inq_dimID_z")
                                                 
status = NF90_INQUIRE_DIMENSION(fidmsk,dimID_z,len=jpkin)
call erreur(status,.TRUE.,"inq_dim_z")
                 
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

!----------------------------------------------------------------------------------
!-- Read input vertical grid (global)

write(*,*) '  Reading ', TRIM(filezgrin)

status = NF90_OPEN(TRIM(filezgrin),0,fidzgr)          
call erreur(status,.TRUE.,"read global vertical grid") 
                                           
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

!-----------------------------------------------------------------

!-- masking :
e3uin(:,:,:) = e3uin(:,:,:) * mskuin(:,:,:)
e3vin(:,:,:) = e3vin(:,:,:) * mskvin(:,:,:)

!=========================================================================================
! 2- READ OUTPUT GRID (i.e. grid of the regional oceanic conf_pariguration)
!=========================================================================================

write(*,*) '  Reading ', TRIM(file_mesh_mask_par)
                                             
status = NF90_OPEN(TRIM(file_mesh_mask_par),0,fidgridout)          
call erreur(status,.TRUE.,"read regional parent mesh mask") 
                                                     
   status = NF90_INQ_DIMID(fidgridout,"z",dimID_z)
   call erreur(status,.TRUE.,"inq_dimID_z")
   status = NF90_INQ_DIMID(fidgridout,"y",dimID_y)
   call erreur(status,.TRUE.,"inq_dimID_y")
   status = NF90_INQ_DIMID(fidgridout,"x",dimID_x)
   call erreur(status,.TRUE.,"inq_dimID_x")
                                                         
   status = NF90_INQUIRE_DIMENSION(fidgridout,dimID_z,len=jpkout)
   call erreur(status,.TRUE.,"inq_dim_z")
   status = NF90_INQUIRE_DIMENSION(fidgridout,dimID_y,len=jpjout)
   call erreur(status,.TRUE.,"inq_dim_y")
   status = NF90_INQUIRE_DIMENSION(fidgridout,dimID_x,len=jpiout)
   call erreur(status,.TRUE.,"inq_dim_x")
   
   ALLOCATE(  fmaskout(jpiout,jpjout,jpkout), vmaskout(jpiout,jpjout,jpkout) ) 
   ALLOCATE(  umaskout(jpiout,jpjout,jpkout), tmaskout(jpiout,jpjout,jpkout) ) 
   ALLOCATE(  e3tout  (jpiout,jpjout,jpkout), e3uout  (jpiout,jpjout,jpkout) )
   ALLOCATE(  e3vout  (jpiout,jpjout,jpkout), e3wout  (jpiout,jpjout,jpkout) )
   ALLOCATE(  gdepwout(jpiout,jpjout,jpkout), gdepvout(jpiout,jpjout,jpkout) )
   ALLOCATE(  gdepuout(jpiout,jpjout,jpkout), gdeptout(jpiout,jpjout,jpkout) )
   ALLOCATE(  e3w_0out(jpkout), e3t_0out(jpkout), gdepw_0out(jpkout), gdept_0out(jpkout)  ) 
   ALLOCATE(  e1tout(jpiout,jpjout), e1uout(jpiout,jpjout), e1vout(jpiout,jpjout), e1fout(jpiout,jpjout) )   
   ALLOCATE(  e2tout(jpiout,jpjout), e2uout(jpiout,jpjout), e2vout(jpiout,jpjout), e2fout(jpiout,jpjout) )
   ALLOCATE(   gphifout(jpiout,jpjout),  gphivout(jpiout,jpjout),  gphiuout(jpiout,jpjout),  gphitout(jpiout,jpjout) )
   ALLOCATE(   glamfout(jpiout,jpjout),  glamvout(jpiout,jpjout),  glamuout(jpiout,jpjout),  glamtout(jpiout,jpjout) )
   ALLOCATE(  zglamfout(jpiout,jpjout), zglamvout(jpiout,jpjout), zglamuout(jpiout,jpjout), zglamtout(jpiout,jpjout) )
   ALLOCATE(   angoutXt(jpiout,jpjout),  angoutXu(jpiout,jpjout),  angoutXv(jpiout,jpjout), angoutXf(jpiout,jpjout)  )
   ALLOCATE(   angoutYt(jpiout,jpjout),  angoutYu(jpiout,jpjout),  angoutYv(jpiout,jpjout), angoutYf(jpiout,jpjout)  )

   status = NF90_INQ_VARID(fidgridout,"fmask",fmask_ID)        ; call erreur(status,.TRUE.,"inq_fmask_ID")
   status = NF90_INQ_VARID(fidgridout,"vmask",vmask_ID)        ; call erreur(status,.TRUE.,"inq_vmask_ID")
   status = NF90_INQ_VARID(fidgridout,"umask",umask_ID)        ; call erreur(status,.TRUE.,"inq_umask_ID")
   status = NF90_INQ_VARID(fidgridout,"tmask",tmask_ID)        ; call erreur(status,.TRUE.,"inq_tmask_ID")
   status = NF90_INQ_VARID(fidgridout,"e3w_0",e3w_0out_ID)     ; call erreur(status,.TRUE.,"inq_e3w_0out_ID")
   status = NF90_INQ_VARID(fidgridout,"e3t_0",e3t_0out_ID)     ; call erreur(status,.TRUE.,"inq_e3t_0out_ID")
   status = NF90_INQ_VARID(fidgridout,"gdepw_0",gdepw_0out_ID) ; call erreur(status,.TRUE.,"inq_gdepw_0out_ID")
   status = NF90_INQ_VARID(fidgridout,"gdept_0",gdept_0out_ID) ; call erreur(status,.TRUE.,"inq_gdept_0out_ID")
   status = NF90_INQ_VARID(fidgridout,"e3w",e3wout_ID)         ; call erreur(status,.TRUE.,"inq_e3wout_ID")
   status = NF90_INQ_VARID(fidgridout,"e3v",e3vout_ID)         ; call erreur(status,.TRUE.,"inq_e3vout_ID")
   status = NF90_INQ_VARID(fidgridout,"e3u",e3uout_ID)         ; call erreur(status,.TRUE.,"inq_e3uout_ID")
   status = NF90_INQ_VARID(fidgridout,"e3t",e3tout_ID)         ; call erreur(status,.TRUE.,"inq_e3tout_ID")
   status = NF90_INQ_VARID(fidgridout,"ff",ffout_ID)           ; call erreur(status,.TRUE.,"inq_ffout_ID")
   status = NF90_INQ_VARID(fidgridout,"e2f",e2fout_ID)         ; call erreur(status,.TRUE.,"inq_e2fout_ID")
   status = NF90_INQ_VARID(fidgridout,"e2v",e2vout_ID)         ; call erreur(status,.TRUE.,"inq_e2vout_ID")
   status = NF90_INQ_VARID(fidgridout,"e2u",e2uout_ID)         ; call erreur(status,.TRUE.,"inq_e2uout_ID")
   status = NF90_INQ_VARID(fidgridout,"e2t",e2tout_ID)         ; call erreur(status,.TRUE.,"inq_e2tout_ID")
   status = NF90_INQ_VARID(fidgridout,"e1f",e1fout_ID)         ; call erreur(status,.TRUE.,"inq_e1fout_ID")
   status = NF90_INQ_VARID(fidgridout,"e1v",e1vout_ID)         ; call erreur(status,.TRUE.,"inq_e1vout_ID")
   status = NF90_INQ_VARID(fidgridout,"e1u",e1uout_ID)         ; call erreur(status,.TRUE.,"inq_e1uout_ID")
   status = NF90_INQ_VARID(fidgridout,"e1t",e1tout_ID)         ; call erreur(status,.TRUE.,"inq_e1tout_ID")
   status = NF90_INQ_VARID(fidgridout,"gdepw",gdepwout_ID)     ; call erreur(status,.TRUE.,"inq_gdepwout_ID")
   status = NF90_INQ_VARID(fidgridout,"gdepv",gdepvout_ID)     ; call erreur(status,.TRUE.,"inq_gdepvout_ID")
   status = NF90_INQ_VARID(fidgridout,"gdepu",gdepuout_ID)     ; call erreur(status,.TRUE.,"inq_gdepuout_ID")
   status = NF90_INQ_VARID(fidgridout,"gdept",gdeptout_ID)     ; call erreur(status,.TRUE.,"inq_gdeptout_ID")
   status = NF90_INQ_VARID(fidgridout,"gphif",gphifout_ID)     ; call erreur(status,.TRUE.,"inq_gphifout_ID")
   status = NF90_INQ_VARID(fidgridout,"gphiv",gphivout_ID)     ; call erreur(status,.TRUE.,"inq_gphivout_ID")
   status = NF90_INQ_VARID(fidgridout,"gphiu",gphiuout_ID)     ; call erreur(status,.TRUE.,"inq_gphiuout_ID")
   status = NF90_INQ_VARID(fidgridout,"gphit",gphitout_ID)     ; call erreur(status,.TRUE.,"inq_gphitout_ID")
   status = NF90_INQ_VARID(fidgridout,"glamv",glamvout_ID)     ; call erreur(status,.TRUE.,"inq_glamvout_ID")
   status = NF90_INQ_VARID(fidgridout,"glamv",glamuout_ID)     ; call erreur(status,.TRUE.,"inq_glamuout_ID")
   status = NF90_INQ_VARID(fidgridout,"glamf",glamfout_ID)     ; call erreur(status,.TRUE.,"inq_glamfout_ID")
   status = NF90_INQ_VARID(fidgridout,"glamt",glamtout_ID)  ; call erreur(status,.TRUE.,"inq_glamtout_ID")
                                                        
   status = NF90_GET_VAR(fidgridout,fmask_ID,fmaskout)        ; call erreur(status,.TRUE.,"getvar_fmaskout")
   status = NF90_GET_VAR(fidgridout,vmask_ID,vmaskout)        ; call erreur(status,.TRUE.,"getvar_vmaskout")
   status = NF90_GET_VAR(fidgridout,umask_ID,umaskout)        ; call erreur(status,.TRUE.,"getvar_umaskout")
   status = NF90_GET_VAR(fidgridout,tmask_ID,tmaskout)        ; call erreur(status,.TRUE.,"getvar_tmaskout")
   status = NF90_GET_VAR(fidgridout,e3w_0out_ID,e3w_0out)     ; call erreur(status,.TRUE.,"getvar_e3w_0out")
   status = NF90_GET_VAR(fidgridout,e3t_0out_ID,e3t_0out)     ; call erreur(status,.TRUE.,"getvar_e3t_0out")
   status = NF90_GET_VAR(fidgridout,gdepw_0out_ID,gdepw_0out) ; call erreur(status,.TRUE.,"getvar_gdepw_0out")
   status = NF90_GET_VAR(fidgridout,gdept_0out_ID,gdept_0out) ; call erreur(status,.TRUE.,"getvar_gdept_0out")
   status = NF90_GET_VAR(fidgridout,e3wout_ID,e3wout)         ; call erreur(status,.TRUE.,"getvar_e3wout")
   status = NF90_GET_VAR(fidgridout,e3vout_ID,e3vout)         ; call erreur(status,.TRUE.,"getvar_e3vout")
   status = NF90_GET_VAR(fidgridout,e3uout_ID,e3uout)         ; call erreur(status,.TRUE.,"getvar_e3uout")
   status = NF90_GET_VAR(fidgridout,e3tout_ID,e3tout)         ; call erreur(status,.TRUE.,"getvar_e3tout")
   status = NF90_GET_VAR(fidgridout,ffout_ID,ffout)           ; call erreur(status,.TRUE.,"getvar_ffout")
   status = NF90_GET_VAR(fidgridout,e2fout_ID,e2fout)         ; call erreur(status,.TRUE.,"getvar_e2fout")
   status = NF90_GET_VAR(fidgridout,e2vout_ID,e2vout)         ; call erreur(status,.TRUE.,"getvar_e2vout")
   status = NF90_GET_VAR(fidgridout,e2uout_ID,e2uout)         ; call erreur(status,.TRUE.,"getvar_e2uout")
   status = NF90_GET_VAR(fidgridout,e2tout_ID,e2tout)         ; call erreur(status,.TRUE.,"getvar_e2tout")
   status = NF90_GET_VAR(fidgridout,e1fout_ID,e1fout)         ; call erreur(status,.TRUE.,"getvar_e1fout")
   status = NF90_GET_VAR(fidgridout,e1vout_ID,e1vout)         ; call erreur(status,.TRUE.,"getvar_e1vout")
   status = NF90_GET_VAR(fidgridout,e1uout_ID,e1uout)         ; call erreur(status,.TRUE.,"getvar_e1uout")
   status = NF90_GET_VAR(fidgridout,e1tout_ID,e1tout)         ; call erreur(status,.TRUE.,"getvar_e1tout")
   status = NF90_GET_VAR(fidgridout,gdepwout_ID,gdepwout)     ; call erreur(status,.TRUE.,"getvar_gdepwout")
   status = NF90_GET_VAR(fidgridout,gdepvout_ID,gdepvout)     ; call erreur(status,.TRUE.,"getvar_gdepvout")
   status = NF90_GET_VAR(fidgridout,gdepuout_ID,gdepuout)     ; call erreur(status,.TRUE.,"getvar_gdepuout")
   status = NF90_GET_VAR(fidgridout,gdeptout_ID,gdeptout)     ; call erreur(status,.TRUE.,"getvar_gdeptout")
   status = NF90_GET_VAR(fidgridout,gphifout_ID,gphifout)     ; call erreur(status,.TRUE.,"getvar_gphifout")
   status = NF90_GET_VAR(fidgridout,gphivout_ID,gphivout)     ; call erreur(status,.TRUE.,"getvar_gphivout")
   status = NF90_GET_VAR(fidgridout,gphiuout_ID,gphiuout)     ; call erreur(status,.TRUE.,"getvar_gphiuout")
   status = NF90_GET_VAR(fidgridout,gphitout_ID,gphitout)     ; call erreur(status,.TRUE.,"getvar_gphitout")
   status = NF90_GET_VAR(fidgridout,glamvout_ID,glamvout)     ; call erreur(status,.TRUE.,"getvar_glamvout")
   status = NF90_GET_VAR(fidgridout,glamuout_ID,glamuout)     ; call erreur(status,.TRUE.,"getvar_glamuout")
   status = NF90_GET_VAR(fidgridout,glamfout_ID,glamfout)     ; call erreur(status,.TRUE.,"getvar_glamfout")
   status = NF90_GET_VAR(fidgridout,glamtout_ID,glamtout)     ; call erreur(status,.TRUE.,"getvar_glamtout")
                                                
status = NF90_CLOSE(fidgridout)                      
call erreur(status,.TRUE.,"fin_lecture_output_grid")     

e3uout(:,:,:) = e3uout(:,:,:) * umaskout(:,:,:)
e3vout(:,:,:) = e3vout(:,:,:) * vmaskout(:,:,:)

!-- Rotation angles 
if ( idateline .eq. 1 ) then
  where ( glamtout(:,:) .lt. 0.0 )
    zglamtout(:,:) = 360.0 + glamtout(:,:)
  elsewhere
    zglamtout(:,:) = glamtout(:,:)
  endwhere
  where ( glamuout(:,:) .lt. 0.0 )
    zglamuout(:,:) = 360.0 + glamuout(:,:)
  elsewhere
    zglamuout(:,:) = glamuout(:,:)
  endwhere
  where ( glamvout(:,:) .lt. 0.0 )
    zglamvout(:,:) = 360.0 + glamvout(:,:)
  elsewhere
    zglamvout(:,:) = glamvout(:,:)
  endwhere
  where ( glamfout(:,:) .lt. 0.0 )
    zglamfout(:,:) = 360.0 + glamfout(:,:)
  elsewhere
    zglamfout(:,:) = glamfout(:,:)
  endwhere
else
  zglamtout(:,:) = glamtout(:,:)
  zglamuout(:,:) = glamuout(:,:)
  zglamvout(:,:) = glamvout(:,:)
  zglamfout(:,:) = glamfout(:,:)
endif

do ji=1,jpiout
do jj=1,jpjout

  ! local angle between i-direction and the zonal direction and between j-direction and the meridional direction (should be similar)
  if     ( ji .eq. 1      ) then
     angoutXt(ji,jj) = ATAN2( gphitout(ji+1,jj) - gphitout(ji  ,jj) , ( zglamtout(ji+1,jj) - zglamtout(ji  ,jj) ) * cos(rad*gphitout(ji,jj)) )
     angoutXu(ji,jj) = ATAN2( gphiuout(ji+1,jj) - gphiuout(ji  ,jj) , ( zglamuout(ji+1,jj) - zglamuout(ji  ,jj) ) * cos(rad*gphiuout(ji,jj)) )
     angoutXv(ji,jj) = ATAN2( gphivout(ji+1,jj) - gphivout(ji  ,jj) , ( zglamvout(ji+1,jj) - zglamvout(ji  ,jj) ) * cos(rad*gphivout(ji,jj)) )
     angoutXf(ji,jj) = ATAN2( gphifout(ji+1,jj) - gphifout(ji  ,jj) , ( zglamfout(ji+1,jj) - zglamfout(ji  ,jj) ) * cos(rad*gphifout(ji,jj)) )
  elseif ( ji .eq. jpiout ) then 
     angoutXt(ji,jj) = ATAN2( gphitout(ji  ,jj) - gphitout(ji-1,jj) , ( zglamtout(ji  ,jj) - zglamtout(ji-1,jj) ) * cos(rad*gphitout(ji,jj)) )
     angoutXu(ji,jj) = ATAN2( gphiuout(ji  ,jj) - gphiuout(ji-1,jj) , ( zglamuout(ji  ,jj) - zglamuout(ji-1,jj) ) * cos(rad*gphiuout(ji,jj)) )
     angoutXv(ji,jj) = ATAN2( gphivout(ji  ,jj) - gphivout(ji-1,jj) , ( zglamvout(ji  ,jj) - zglamvout(ji-1,jj) ) * cos(rad*gphivout(ji,jj)) )
     angoutXf(ji,jj) = ATAN2( gphifout(ji  ,jj) - gphifout(ji-1,jj) , ( zglamfout(ji  ,jj) - zglamfout(ji-1,jj) ) * cos(rad*gphifout(ji,jj)) )
  else
     angoutXt(ji,jj) = ATAN2( gphitout(ji+1,jj) - gphitout(ji-1,jj) , ( zglamtout(ji+1,jj) - zglamtout(ji-1,jj) ) * cos(rad*gphitout(ji,jj)) )
     angoutXu(ji,jj) = ATAN2( gphiuout(ji+1,jj) - gphiuout(ji-1,jj) , ( zglamuout(ji+1,jj) - zglamuout(ji-1,jj) ) * cos(rad*gphiuout(ji,jj)) )
     angoutXv(ji,jj) = ATAN2( gphivout(ji+1,jj) - gphivout(ji-1,jj) , ( zglamvout(ji+1,jj) - zglamvout(ji-1,jj) ) * cos(rad*gphivout(ji,jj)) )
     angoutXf(ji,jj) = ATAN2( gphifout(ji+1,jj) - gphifout(ji-1,jj) , ( zglamfout(ji+1,jj) - zglamfout(ji-1,jj) ) * cos(rad*gphifout(ji,jj)) )
  endif
  !-
  if     ( jj .eq. 1      ) then
     angoutYt(ji,jj) = ATAN2( gphitout(ji,jj+1) - gphitout(ji,jj  ) , ( zglamtout(ji,jj+1) - zglamtout(ji,jj  ) ) * cos(rad*gphitout(ji,jj)) )
     angoutYu(ji,jj) = ATAN2( gphiuout(ji,jj+1) - gphiuout(ji,jj  ) , ( zglamuout(ji,jj+1) - zglamuout(ji,jj  ) ) * cos(rad*gphiuout(ji,jj)) )
     angoutYv(ji,jj) = ATAN2( gphivout(ji,jj+1) - gphivout(ji,jj  ) , ( zglamvout(ji,jj+1) - zglamvout(ji,jj  ) ) * cos(rad*gphivout(ji,jj)) )
     angoutYf(ji,jj) = ATAN2( gphifout(ji,jj+1) - gphifout(ji,jj  ) , ( zglamfout(ji,jj+1) - zglamfout(ji,jj  ) ) * cos(rad*gphifout(ji,jj)) )
  elseif ( jj .eq. jpjout ) then
     angoutYt(ji,jj) = ATAN2( gphitout(ji,jj  ) - gphitout(ji,jj-1) , ( zglamtout(ji,jj  ) - zglamtout(ji,jj-1) ) * cos(rad*gphitout(ji,jj)) )
     angoutYu(ji,jj) = ATAN2( gphiuout(ji,jj  ) - gphiuout(ji,jj-1) , ( zglamuout(ji,jj  ) - zglamuout(ji,jj-1) ) * cos(rad*gphiuout(ji,jj)) )
     angoutYv(ji,jj) = ATAN2( gphivout(ji,jj  ) - gphivout(ji,jj-1) , ( zglamvout(ji,jj  ) - zglamvout(ji,jj-1) ) * cos(rad*gphivout(ji,jj)) )
     angoutYf(ji,jj) = ATAN2( gphifout(ji,jj  ) - gphifout(ji,jj-1) , ( zglamfout(ji,jj  ) - zglamfout(ji,jj-1) ) * cos(rad*gphifout(ji,jj)) )
  else
     angoutYt(ji,jj) = ATAN2( gphitout(ji,jj+1) - gphitout(ji,jj-1) , ( zglamtout(ji,jj+1) - zglamtout(ji,jj-1) ) * cos(rad*gphitout(ji,jj)) )
     angoutYu(ji,jj) = ATAN2( gphiuout(ji,jj+1) - gphiuout(ji,jj-1) , ( zglamuout(ji,jj+1) - zglamuout(ji,jj-1) ) * cos(rad*gphiuout(ji,jj)) )
     angoutYv(ji,jj) = ATAN2( gphivout(ji,jj+1) - gphivout(ji,jj-1) , ( zglamvout(ji,jj+1) - zglamvout(ji,jj-1) ) * cos(rad*gphivout(ji,jj)) )
     angoutYf(ji,jj) = ATAN2( gphifout(ji,jj+1) - gphifout(ji,jj-1) , ( zglamfout(ji,jj+1) - zglamfout(ji,jj-1) ) * cos(rad*gphifout(ji,jj)) )
  endif

enddo
enddo

!-- As we assume same vertical grid in this prgm (except partial steps),
!   we do a quick check here
if ( jpkout .ne. jpkin ) then
  write(*,*) '!@#$%^* Modify the code to allow 2 different vertical grids >>>>>>> stop'
  stop
endif
do jk=1,jpkout
   if ( abs( e3t_0out(jk) -e3t_0in(jk) ) .gt. 1.e-2 ) then
     write(*,*) '!@#$%^* Modify the code to allow 2 different vertical grids >>>>>>> stop'
     stop
   endif
enddo

!===============

DO iside=1,nside

    !=========================================================================================
    ! 3- CALCULATE WEIGHTS FOR OBC FILES 
    !    (and find what are the 4 global T-points surrounding a T-point of the boundary)
    !=========================================================================================

    write(*,*) '  '
    write(*,*) '  Building interpolation coefficients for boundary ', TRIM(side(iside))

    ! for each side of the domain, define here :
    ! - the positions i,j (offset) of the boundaries (OBC) in the output grid for T, U, V and F points
    ! - the number (cnt) of T, U, V and F points to read in i and j directions
    ! NB: see +1 later because indices were first defined using the IDL convention (starting from 0)
    if     ( TRIM(side(iside)) .eq. 'north') then
      iofft  = 0        ! i-offset for T-grid start (=0 if starting from 1 in fortran conventions)
      ioffu  = 0        ! i-offset for U-grid start (=0 if starting from 1 in fortran conventions)
      ioffv  = 0        ! i-offset for V-grid start (=0 if starting from 1 in fortran conventions)
      iofff  = 0        ! i-offset for F-grid start (=0 if starting from 1 in fortran conventions)
      jpicnt = jpiout   ! number of points along the i-direction on this boundary
      jofft  = jpjout-2 ! j-offset for T-grid start (=0 if starting from 1 in fortran conventions)
      joffu  = jpjout-2 ! j-offset for U-grid start (=0 if starting from 1 in fortran conventions)
      joffv  = jpjout-3 ! j-offset for V-grid start (=0 if starting from 1 in fortran conventions)
      jofff  = jpjout-3 ! j-offset for F-grid start (=0 if starting from 1 in fortran conventions)
      jpjcnt = 1        ! number of points along the j-direction on this boundary
    elseif ( TRIM(side(iside)) .eq. 'south') then
      iofft  = 0
      ioffu  = 0
      ioffv  = 0
      iofff  = 0
      jpicnt = jpiout
      jofft  = 1
      joffu  = 1
      joffv  = 1
      jofff  = 0
      jpjcnt = 1
    elseif ( TRIM(side(iside)) .eq. 'east') then
      iofft  = jpiout - 2
      ioffu  = jpiout - 3
      ioffv  = jpiout - 2
      iofff  = jpiout - 3
      jpicnt = 1
      jofft  = 0
      joffu  = 0
      joffv  = 0
      jofff  = 0
      jpjcnt = jpjout
    elseif ( TRIM(side(iside)) .eq. 'west') then
      iofft  = 1
      ioffu  = 1
      ioffv  = 1
      iofff  = 0
      jpicnt = 1
      jofft  = 0
      joffu  = 0
      joffv  = 0
      jofff  = 0
      jpjcnt = jpjout
    else
      write(*,*) '!@#$%^* Undefined boundary:', TRIM(side(iside))
      stop
    endif

    if ( jpicnt .eq. 1) then
     jpicntf = 2       ! read 2 lines instead of one (needed to derivate stream function on F-grid)
    else 
     jpicntf = jpiout
    endif
    
    if ( jpjcnt .eq. 1) then
     jpjcntf = 2       ! read 2 lines instead of one (needed to derivate stream function on F-grid)
    else
     jpjcntf = jpjout
    endif

    !-- boundary grid coordinates
    ALLOCATE( lonbdyt(jpicnt ,jpjcnt ) , latbdyt(jpicnt ,jpjcnt ) )
    ALLOCATE( lonbdyu(jpicnt ,jpjcnt ) , latbdyu(jpicnt ,jpjcnt ) )
    ALLOCATE( lonbdyv(jpicnt ,jpjcnt ) , latbdyv(jpicnt ,jpjcnt ) )
    ALLOCATE( lonbdyf(jpicntf,jpjcntf) , latbdyf(jpicntf,jpjcntf) )
    lonbdyt (1:jpicnt ,1:jpjcnt ) = glamtout ( iofft+1:iofft+jpicnt  , jofft+1:jofft+jpjcnt  )
    latbdyt (1:jpicnt ,1:jpjcnt ) = gphitout ( iofft+1:iofft+jpicnt  , jofft+1:jofft+jpjcnt  )
    lonbdyu (1:jpicnt ,1:jpjcnt ) = glamuout ( ioffu+1:ioffu+jpicnt  , joffu+1:joffu+jpjcnt  )
    latbdyu (1:jpicnt ,1:jpjcnt ) = gphiuout ( ioffu+1:ioffu+jpicnt  , joffu+1:joffu+jpjcnt  )
    lonbdyv (1:jpicnt ,1:jpjcnt ) = glamvout ( ioffv+1:ioffv+jpicnt  , joffv+1:joffv+jpjcnt  )
    latbdyv (1:jpicnt ,1:jpjcnt ) = gphivout ( ioffv+1:ioffv+jpicnt  , joffv+1:joffv+jpjcnt  )
    lonbdyf (1:jpicntf,1:jpjcntf) = glamfout ( iofff+1:iofff+jpicntf , jofff+1:jofff+jpjcntf )
    latbdyf (1:jpicntf,1:jpjcntf) = gphifout ( iofff+1:iofff+jpicntf , jofff+1:jofff+jpjcntf )
    ALLOCATE( zlonbdyt(jpicnt ,jpjcnt ) )
    ALLOCATE( zlonbdyu(jpicnt ,jpjcnt ) )
    ALLOCATE( zlonbdyv(jpicnt ,jpjcnt ) )
    ALLOCATE( zlonbdyf(jpicntf,jpjcntf) )

    !-- boundary mesh sizes
    ALLOCATE( e1tbdy(jpicnt ,jpjcnt ) , e2tbdy(jpicnt ,jpjcnt ) )
    ALLOCATE( e1ubdy(jpicnt ,jpjcnt ) , e2ubdy(jpicnt ,jpjcnt ) )
    ALLOCATE( e1vbdy(jpicnt ,jpjcnt ) , e2vbdy(jpicnt ,jpjcnt ) )
    ALLOCATE( e1fbdy(jpicntf,jpjcntf) , e2fbdy(jpicntf,jpjcntf) )
    e1tbdy (1:jpicnt ,1:jpjcnt ) = e1tout ( iofft+1:iofft+jpicnt  , jofft+1:jofft+jpjcnt  )
    e2tbdy (1:jpicnt ,1:jpjcnt ) = e2tout ( iofft+1:iofft+jpicnt  , jofft+1:jofft+jpjcnt  )
    e1ubdy (1:jpicnt ,1:jpjcnt ) = e1uout ( ioffu+1:ioffu+jpicnt  , joffu+1:joffu+jpjcnt  )
    e2ubdy (1:jpicnt ,1:jpjcnt ) = e2uout ( ioffu+1:ioffu+jpicnt  , joffu+1:joffu+jpjcnt  )
    e1vbdy (1:jpicnt ,1:jpjcnt ) = e1vout ( ioffv+1:ioffv+jpicnt  , joffv+1:joffv+jpjcnt  )
    e2vbdy (1:jpicnt ,1:jpjcnt ) = e2vout ( ioffv+1:ioffv+jpicnt  , joffv+1:joffv+jpjcnt  )
    e1fbdy (1:jpicntf,1:jpjcntf) = e1fout ( iofff+1:iofff+jpicntf , jofff+1:jofff+jpjcntf )
    e2fbdy (1:jpicntf,1:jpjcntf) = e2fout ( iofff+1:iofff+jpicntf , jofff+1:jofff+jpjcntf )

    !- boundary grid local angle (with respext to zonal and meridional directions)
    ALLOCATE( angbdyXt(jpicnt ,jpjcnt ) , angbdyYt(jpicnt ,jpjcnt ) )
    ALLOCATE( angbdyXu(jpicnt ,jpjcnt ) , angbdyYu(jpicnt ,jpjcnt ) )
    ALLOCATE( angbdyXv(jpicnt ,jpjcnt ) , angbdyYv(jpicnt ,jpjcnt ) )
    ALLOCATE( angbdyXf(jpicntf,jpjcntf) , angbdyYf(jpicntf,jpjcntf) )
    angbdyXt(1:jpicnt ,1:jpjcnt ) = angoutXt ( iofft+1:iofft+jpicnt  , jofft+1:jofft+jpjcnt  )
    angbdyXu(1:jpicnt ,1:jpjcnt ) = angoutXu ( ioffu+1:ioffu+jpicnt  , joffu+1:joffu+jpjcnt  )
    angbdyXv(1:jpicnt ,1:jpjcnt ) = angoutXv ( ioffv+1:ioffv+jpicnt  , joffv+1:joffv+jpjcnt  )
    angbdyXf(1:jpicntf,1:jpjcntf) = angoutXf ( iofff+1:iofff+jpicntf , jofff+1:jofff+jpjcntf )
    angbdyYt(1:jpicnt ,1:jpjcnt ) = angoutYt ( iofft+1:iofft+jpicnt  , jofft+1:jofft+jpjcnt  )
    angbdyYu(1:jpicnt ,1:jpjcnt ) = angoutYu ( ioffu+1:ioffu+jpicnt  , joffu+1:joffu+jpjcnt  )
    angbdyYv(1:jpicnt ,1:jpjcnt ) = angoutYv ( ioffv+1:ioffv+jpicnt  , joffv+1:joffv+jpjcnt  )
    angbdyYf(1:jpicntf,1:jpjcntf) = angoutYf ( iofff+1:iofff+jpicntf , jofff+1:jofff+jpjcntf )
    ALLOCATE( angleXt(jpicnt ,jpjcnt ) , angleYt(jpicnt ,jpjcnt ) )
    ALLOCATE( angleXu(jpicnt ,jpjcnt ) , angleYu(jpicnt ,jpjcnt ) )
    ALLOCATE( angleXv(jpicnt ,jpjcnt ) , angleYv(jpicnt ,jpjcnt ) )
    ALLOCATE( angleXf(jpicntf,jpjcntf) , angleYf(jpicntf,jpjcntf) )

    !-- boundary mask
    ALLOCATE( tmskbdy(jpicnt, jpjcnt, jpkout)  )
    ALLOCATE( umskbdy(jpicnt, jpjcnt, jpkout)  )
    ALLOCATE( vmskbdy(jpicnt, jpjcnt, jpkout)  )
    tmskbdy(1:jpicnt ,1:jpjcnt ,:) = tmaskout ( iofft+1:iofft+jpicnt  , jofft+1:jofft+jpjcnt , : )
    umskbdy(1:jpicnt ,1:jpjcnt ,:) = umaskout ( ioffu+1:ioffu+jpicnt  , joffu+1:joffu+jpjcnt , : )
    vmskbdy(1:jpicnt ,1:jpjcnt ,:) = vmaskout ( ioffv+1:ioffv+jpicnt  , joffv+1:joffv+jpjcnt , : )

    !-- boundary vertical parameters
    ALLOCATE( e3tbdy    (jpicnt, jpjcnt, jpkout)  )
    ALLOCATE( e3ubdy    (jpicnt, jpjcnt, jpkout)  )
    ALLOCATE( e3vbdy    (jpicnt, jpjcnt, jpkout)  )
    ALLOCATE( gdeptbdy  (jpicnt, jpjcnt, jpkout)  )
    ALLOCATE( gdepubdy  (jpicnt, jpjcnt, jpkout)  )
    ALLOCATE( gdepvbdy  (jpicnt, jpjcnt, jpkout)  )
    ALLOCATE( e3t_0bdy  (                jpkout)  )
    ALLOCATE( gdept_0bdy(                jpkout)  )
    e3tbdy   (1:jpicnt ,1:jpjcnt ,:) = e3tout   ( iofft+1:iofft+jpicnt  , jofft+1:jofft+jpjcnt , : )
    e3ubdy   (1:jpicnt ,1:jpjcnt ,:) = e3uout   ( ioffu+1:ioffu+jpicnt  , joffu+1:joffu+jpjcnt , : )
    e3vbdy   (1:jpicnt ,1:jpjcnt ,:) = e3vout   ( ioffv+1:ioffv+jpicnt  , joffv+1:joffv+jpjcnt , : )
    gdeptbdy (1:jpicnt ,1:jpjcnt ,:) = gdeptout ( iofft+1:iofft+jpicnt  , jofft+1:jofft+jpjcnt , : )
    gdepubdy (1:jpicnt ,1:jpjcnt ,:) = gdepuout ( ioffu+1:ioffu+jpicnt  , joffu+1:joffu+jpjcnt , : )
    gdepvbdy (1:jpicnt ,1:jpjcnt ,:) = gdepvout ( ioffv+1:ioffv+jpicnt  , joffv+1:joffv+jpjcnt , : )
    e3t_0bdy   (:) = e3t_0out   (:)
    gdept_0bdy (:) = gdept_0out (:)

    !===============================================
    ! 3.1 find the closest T point and coefficients
    !===============================================

    ALLOCATE( ziWt  (jpicnt,jpjcnt), zjWt  (jpicnt,jpjcnt), wgWt(jpicnt,jpjcnt) )
    ALLOCATE( ziCt  (jpicnt,jpjcnt), zjCt  (jpicnt,jpjcnt), wgCt(jpicnt,jpjcnt) )
    ALLOCATE( ziEt  (jpicnt,jpjcnt), zjEt  (jpicnt,jpjcnt), wgEt(jpicnt,jpjcnt) )
    ALLOCATE( ziSt  (jpicnt,jpjcnt), zjSt  (jpicnt,jpjcnt), wgSt(jpicnt,jpjcnt) )
    ALLOCATE( ziMt  (jpicnt,jpjcnt), zjMt  (jpicnt,jpjcnt), wgMt(jpicnt,jpjcnt) )
    ALLOCATE( ziNt  (jpicnt,jpjcnt), zjNt  (jpicnt,jpjcnt), wgNt(jpicnt,jpjcnt) )
    ALLOCATE( zzit  (jpicnt,jpjcnt), zzjt  (jpicnt,jpjcnt)                      )
    ALLOCATE( zzitm1(jpicnt,jpjcnt), zzjtm1(jpicnt,jpjcnt)                      )
    ALLOCATE( zzitp1(jpicnt,jpjcnt), zzjtp1(jpicnt,jpjcnt)                      )

    IF ( idateline .EQ. 1 ) THEN
      where ( lonbdyt(:,:) .lt. 0.0 ) 
        zlonbdyt(:,:) = 360.0 + lonbdyt(:,:)
      elsewhere
        zlonbdyt(:,:) = lonbdyt(:,:)
      endwhere
      where ( glamtin(:,:) .lt. 0.0 )
        zglamtin(:,:) = 360.0 + glamtin(:,:)
      elsewhere
        zglamtin(:,:) = glamtin(:,:)
      endwhere
    ELSE
      zglamtin(:,:) = glamtin(:,:) 
    ENDIF

    zzit(:,:)=0 !- to notice error if a point is not filled
    zzjt(:,:)=0 !- to notice error if a point is not filled

    DO jibdy=1,jpicnt
    DO jjbdy=1,jpjcnt

      distmin = ra * rad * 10.0 ! let's assume that nobody's using grids coarser than 10 degrees...

      !-- loop to find closest point ( we allow 1 bdy cell to cover a maximum of 3 cells from the input grid )
      do jiin=1,jpiin
      do jjin=1,jpjin
        dist = ra * rad * sqrt(   ( cos(rad*latbdyt(jibdy,jjbdy)) * ( zlonbdyt(jibdy,jjbdy) - zglamtin(jiin,jjin) ) )**2  &
          &                     + (                                    latbdyt(jibdy,jjbdy) -  gphitin(jiin,jjin)   )**2  )
        if ( dist .lt. distmin ) then
          distmin=dist
          zzit(jibdy,jjbdy) = jiin
          zzjt(jibdy,jjbdy) = jjin
        endif
      enddo 
      enddo
      !- East-West bounds :
      if ( perio .eq. 1 ) then !- periodic grid in X-direction
        if     ( zzit(jibdy,jjbdy)+1 .gt. jpiin ) then
          zzitp1(jibdy,jjbdy) = 3
          zzitm1(jibdy,jjbdy) = zzit(jibdy,jjbdy) - 1
        elseif ( zzit(jibdy,jjbdy)-1 .lt. 1     ) then
          zzitp1(jibdy,jjbdy) = zzit(jibdy,jjbdy) + 1
          zzitm1(jibdy,jjbdy) = jpiin-2
        else 
          zzitp1(jibdy,jjbdy) = zzit(jibdy,jjbdy) + 1
          zzitm1(jibdy,jjbdy) = zzit(jibdy,jjbdy) - 1
        endif
      elseif ( perio .eq. 0 ) then !- non-periodic
        if     ( zzit(jibdy,jjbdy)+1 .gt. jpiin ) then
          zzitp1(jibdy,jjbdy) = zzit(jibdy,jjbdy)
          zzitm1(jibdy,jjbdy) = zzit(jibdy,jjbdy) - 1
        elseif ( zzit(jibdy,jjbdy)-1 .lt. 1     ) then
          zzitp1(jibdy,jjbdy) = zzit(jibdy,jjbdy) + 1
          zzitm1(jibdy,jjbdy) = zzit(jibdy,jjbdy)
        else
          zzitp1(jibdy,jjbdy) = zzit(jibdy,jjbdy) + 1
          zzitm1(jibdy,jjbdy) = zzit(jibdy,jjbdy) - 1
        endif
      else
        write(*,*) '~!@#$%^* ERROR: perio must be either 0 or 1 >>>>>>> stop !!'
        stop
      endif
      !- upper lower bounds of global input grid
      if     ( zzjt(jibdy,jjbdy)+1 .gt. jpjin ) then
        zzjtp1(jibdy,jjbdy) = zzjt(jibdy,jjbdy)
        zzjtm1(jibdy,jjbdy) = zzjt(jibdy,jjbdy) - 1
      elseif ( zzit(jibdy,jjbdy)-1 .lt. 1     ) then
        zzjtp1(jibdy,jjbdy) = zzjt(jibdy,jjbdy) + 1
        zzjtm1(jibdy,jjbdy) = zzjt(jibdy,jjbdy)
      else
        zzjtp1(jibdy,jjbdy) = zzjt(jibdy,jjbdy) + 1
        zzjtm1(jibdy,jjbdy) = zzjt(jibdy,jjbdy) - 1
      endif

      !-- local angle on global input grid between i-direction and the zonal direction and between j-direction and the meridional direction
      anginX = ATAN2(    gphitin( zzitp1(jibdy,jjbdy) , zzjt(jibdy,jjbdy) ) -  gphitin( zzitm1(jibdy,jjbdy) , zzjt(jibdy,jjbdy)) , &
      &                ( zglamtin( zzitp1(jibdy,jjbdy) , zzjt(jibdy,jjbdy) ) - zglamtin( zzitm1(jibdy,jjbdy) , zzjt(jibdy,jjbdy)) ) &
      &                * cos( gphitin( zzit(jibdy,jjbdy) , zzjt(jibdy,jjbdy) ) * rad )                                                )
      anginY = ATAN2(    gphitin( zzit(jibdy,jjbdy) , zzjtp1(jibdy,jjbdy) ) -  gphitin( zzit(jibdy,jjbdy) , zzjtm1(jibdy,jjbdy)) , &
      &                ( zglamtin( zzit(jibdy,jjbdy) , zzjtp1(jibdy,jjbdy) ) - zglamtin( zzit(jibdy,jjbdy) , zzjtm1(jibdy,jjbdy)) ) &
      &                * cos( gphitin( zzit(jibdy,jjbdy) , zzjt(jibdy,jjbdy) ) * rad )                                                )

      !-- local angle between the two grids :
      angleXt(jibdy,jjbdy) = angbdyXt(jibdy,jjbdy) - anginX
      angleYt(jibdy,jjbdy) = angbdyYt(jibdy,jjbdy) - anginY
      if ( abs(angleXt(jibdy,jjbdy)/rad) .gt. 45.0 .or. abs(angleYt(jibdy,jjbdy)/rad) .gt. 45.0 ) then
        write(*,*) '@@@@@@ WARNING : angle between 2 T-grids > 45 deg at point of the bdy :', jibdy, jjbdy
      endif     
 
      !------------------------------------------------------------
      !   coordinates of closest points along X and weights
      !   ( take 2 or 3 points according to mesh size )
      !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
      if      ( e1tbdy(jibdy,jjbdy) .gt. 50*e1tin(zzit(jibdy,jjbdy),zzjt(jibdy,jjbdy))*cos(angleXt(jibdy,jjbdy)*rad) ) then ! it should even be 3 here
        write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along X >>>>>>> stop'
        write(*,*) '(jibdy,jjbdy) =', jibdy,jjbdy
        write(*,*) 'angleXt =', angleXt(jibdy,jjbdy)
        write(*,*) 'e1tbdy(jibdy,jjbdy) =', e1tbdy(jibdy,jjbdy)
        write(*,*) 'e1tin(zzit(jibdy,jjbdy),zzjt(jibdy,jjbdy))*cos(angleXt(jibdy,jjbdy)*rad)=', e1tin(zzit(jibdy,jjbdy),zzjt(jibdy,jjbdy))*cos(angleXt(jibdy,jjbdy)*rad)
        stop
      elseif  (   e1tbdy(jibdy,jjbdy) .ge. e1tin(zzit(jibdy,jjbdy),zzjt(jibdy,jjbdy)) * cos(angleXt(jibdy,jjbdy)*rad)  ) then
        !- use 3 points for interpolation
        ziWt(jibdy,jjbdy) = zzitm1(jibdy,jjbdy) ! closest point westward
        ziCt(jibdy,jjbdy) = zzit  (jibdy,jjbdy) ! closest point ( central )
        ziEt(jibdy,jjbdy) = zzitp1(jibdy,jjbdy) ! closest point eastward
        wgCt(jibdy,jjbdy) = e1tin(zzit(jibdy,jjbdy),zzjt(jibdy,jjbdy))* cos(angleXt(jibdy,jjbdy)*rad) !weight for central point
      elseif  (       zlonbdyt(jibdy,jjbdy) .ge.  zglamtin(zzit  (jibdy,jjbdy),zzjt(jibdy,jjbdy))        )  then
        !- use 2 points for interpolation
        ziWt(jibdy,jjbdy) = zzit  (jibdy,jjbdy) ! closest point westward
        ziCt(jibdy,jjbdy) = zzit  (jibdy,jjbdy) ! dummy
        ziEt(jibdy,jjbdy) = zzitp1(jibdy,jjbdy) ! closest point eastward
        wgCt(jibdy,jjbdy) = 0.0                 ! weight for central point
      else
        !- use 2 points for interpolation
        ziWt(jibdy,jjbdy) = zzitm1(jibdy,jjbdy) ! closest point westward
        ziCt(jibdy,jjbdy) = zzit  (jibdy,jjbdy) ! dummy
        ziEt(jibdy,jjbdy) = zzit  (jibdy,jjbdy) ! closest point eastward
        wgCt(jibdy,jjbdy) = 0.0                 ! weight for central point
      endif
      zjWt(jibdy,jjbdy) = zzjt  (jibdy,jjbdy) !  "
      zjCt(jibdy,jjbdy) = zzjt  (jibdy,jjbdy) ! to be adapted if angles > 45 deg (??)
      zjEt(jibdy,jjbdy) = zzjt  (jibdy,jjbdy) !  "
      !- weight for westward point
      wgWt(jibdy,jjbdy) = MAX( 0.0,                                                                                         &
        &                     0.5 * e1tin(ziWt(jibdy,jjbdy),zjWt(jibdy,jjbdy)) * cos(angleXt(jibdy,jjbdy)*rad)                &
        &                   - ra * rad * abs(   ( zlonbdyt(jibdy,jjbdy) - zglamtin(ziWt(jibdy,jjbdy),zjWt(jibdy,jjbdy)) )     &
        &                                     * cos(  latbdyt(jibdy,jjbdy)*rad )                                           )  &
        &                   + 0.5 * e1tbdy(jibdy,jjbdy)                                                                       )
      !- weight for eastward point
      wgEt(jibdy,jjbdy) = MAX( 0.0,                                                                                         &
        &                     0.5 * e1tin(ziEt(jibdy,jjbdy),zjEt(jibdy,jjbdy)) * cos(angleXt(jibdy,jjbdy)*rad)                &
        &                   - ra * rad * abs(  ( zlonbdyt(jibdy,jjbdy) - zglamtin(ziEt(jibdy,jjbdy),zjEt(jibdy,jjbdy)) )      &
        &                                     * cos(  latbdyt(jibdy,jjbdy)*rad )                                           )  &
        &                   + 0.5 * e1tbdy(jibdy,jjbdy)                                                                       )

      !------------------------------------------------------------
      !   coordinates of closest points along Y and weights
      !   ( take 2 or 3 points according to mesh size )
      !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
      if      ( e2tbdy(jibdy,jjbdy) .gt. 50*e2tin(zzit(jibdy,jjbdy),zzjt(jibdy,jjbdy))*cos(angleYt(jibdy,jjbdy)*rad)  ) then ! it should even be 3 here
        write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along Y >>>>>>> stop'
        write(*,*) '(jibdy,jjbdy) =', jibdy,jjbdy
        write(*,*) 'angleYt =', angleYt(jibdy,jjbdy)
        write(*,*) 'e2tbdy(jibdy,jjbdy) =', e2tbdy(jibdy,jjbdy)
        write(*,*) 'e2tin(zzit(jibdy,jjbdy),zzjt(jibdy,jjbdy))*cos(angleYt(jibdy,jjbdy)*rad)=', e2tin(zzit(jibdy,jjbdy),zzjt(jibdy,jjbdy))*cos(angleYt(jibdy,jjbdy)*rad)
        stop
      elseif  (   e2tbdy(jibdy,jjbdy) .ge. e2tin(zzit(jibdy,jjbdy),zzjt(jibdy,jjbdy)) * cos(angleYt(jibdy,jjbdy)*rad)  ) then
        !- use 3 points for interpolation
        zjSt(jibdy,jjbdy) = zzjtm1(jibdy,jjbdy) ! closest point southward
        zjMt(jibdy,jjbdy) = zzjt  (jibdy,jjbdy) ! closest point ( middle )
        zjNt(jibdy,jjbdy) = zzjtp1(jibdy,jjbdy) ! closest point northward
        wgMt(jibdy,jjbdy) = e2tin(zzit(jibdy,jjbdy),zzjt(jibdy,jjbdy))* cos(angleYt(jibdy,jjbdy)*rad) !weight for central point
      elseif  (       latbdyt(jibdy,jjbdy) .ge.  gphitin(zzit  (jibdy,jjbdy),zzjt(jibdy,jjbdy))        )  then
        !- use 2 points for interpolation
        zjSt(jibdy,jjbdy) = zzjt  (jibdy,jjbdy) ! closest point southward
        zjMt(jibdy,jjbdy) = zzjt  (jibdy,jjbdy) ! dummy
        zjNt(jibdy,jjbdy) = zzjtp1(jibdy,jjbdy) ! closest point northward
        wgMt(jibdy,jjbdy) = 0.0                 ! weight for middle point
      else
        !- use 2 points for interpolation
        zjSt(jibdy,jjbdy) = zzjtm1(jibdy,jjbdy) ! closest point southward
        zjMt(jibdy,jjbdy) = zzjt  (jibdy,jjbdy) ! dummy
        zjNt(jibdy,jjbdy) = zzjt  (jibdy,jjbdy) ! closest point northward
        wgMt(jibdy,jjbdy) = 0.0                 ! weight for middle point
      endif
      ziSt(jibdy,jjbdy) = zzit  (jibdy,jjbdy) ! "
      ziMt(jibdy,jjbdy) = zzit  (jibdy,jjbdy) ! to be adapted if angles > 45 deg (??)
      ziNt(jibdy,jjbdy) = zzit  (jibdy,jjbdy) ! "
      !- weight for southward point
      wgSt(jibdy,jjbdy) = MAX( 0.0,                                                                                  &
        &                     0.5 * e2tin(ziSt(jibdy,jjbdy),zjSt(jibdy,jjbdy)) * cos(angleYt(jibdy,jjbdy)*rad)         &
        &                   - ra * rad * abs(  latbdyt(jibdy,jjbdy) - gphitin(ziSt(jibdy,jjbdy),zjSt(jibdy,jjbdy)) )   &
        &                   + 0.5 * e2tbdy(jibdy,jjbdy)                                                                )
      !- weight for northward point
      wgNt(jibdy,jjbdy) = MAX( 0.0,                                                                                  &
        &                     0.5 * e2tin(ziNt(jibdy,jjbdy),zjNt(jibdy,jjbdy)) * cos(angleYt(jibdy,jjbdy)*rad)         &
        &                   - ra * rad * abs(  latbdyt(jibdy,jjbdy) - gphitin(ziNt(jibdy,jjbdy),zjNt(jibdy,jjbdy)) )   &
        &                   + 0.5 * e2tbdy(jibdy,jjbdy)                                                                )

      !----------------------------------------------------------------
      !  NO VERTICAL INTERPOLATION HERE (ASSUMED TO BE THE SAME GRID) !
      !----------------------------------------------------------------  

    ENDDO !-- jpjbdy
    ENDDO !-- jpibdy

    !===============================================
    ! 3.2 find the closest U point and coefficients
    !===============================================

    ALLOCATE( ziWu  (jpicnt,jpjcnt), zjWu  (jpicnt,jpjcnt), wgWu(jpicnt,jpjcnt) )
    ALLOCATE( ziCu  (jpicnt,jpjcnt), zjCu  (jpicnt,jpjcnt), wgCu(jpicnt,jpjcnt) )
    ALLOCATE( ziEu  (jpicnt,jpjcnt), zjEu  (jpicnt,jpjcnt), wgEu(jpicnt,jpjcnt) )
    ALLOCATE( ziSu  (jpicnt,jpjcnt), zjSu  (jpicnt,jpjcnt), wgSu(jpicnt,jpjcnt) )
    ALLOCATE( ziMu  (jpicnt,jpjcnt), zjMu  (jpicnt,jpjcnt), wgMu(jpicnt,jpjcnt) )
    ALLOCATE( ziNu  (jpicnt,jpjcnt), zjNu  (jpicnt,jpjcnt), wgNu(jpicnt,jpjcnt) )
    ALLOCATE( zziu  (jpicnt,jpjcnt), zzju  (jpicnt,jpjcnt)                      )
    ALLOCATE( zzium1(jpicnt,jpjcnt), zzjum1(jpicnt,jpjcnt)                      )
    ALLOCATE( zziup1(jpicnt,jpjcnt), zzjup1(jpicnt,jpjcnt)                      )

    IF ( idateline .EQ. 1 ) THEN
      where ( lonbdyu(:,:) .lt. 0.0 ) 
        zlonbdyu(:,:) = 360.0 + lonbdyu(:,:)
      elsewhere
        zlonbdyu(:,:) = lonbdyu(:,:)
      endwhere
      where ( glamuin(:,:) .lt. 0.0 )
        zglamuin(:,:) = 360.0 + glamuin(:,:)
      elsewhere
        zglamuin(:,:) = glamuin(:,:)
      endwhere
    ELSE
      zglamuin(:,:) = glamuin(:,:) 
    ENDIF

    zziu(:,:)=0 !- to notice error if a point is not filled
    zzju(:,:)=0 !- to notice error if a point is not filled

    DO jibdy=1,jpicnt
    DO jjbdy=1,jpjcnt

      distmin = ra * rad * 10.0 ! let's assume that nobody's using grids coarser than 10 degrees...

      !-- loop to find closest point ( we allow 1 bdy cell to cover a maximum of 3 cells from the input grid )
      do jiin=1,jpiin
      do jjin=1,jpjin
        dist = ra * rad * sqrt(   ( cos(rad*latbdyu(jibdy,jjbdy)) * ( zlonbdyu(jibdy,jjbdy) - zglamuin(jiin,jjin) ) )**2  &
          &                     + (                                    latbdyu(jibdy,jjbdy) -  gphiuin(jiin,jjin)   )**2  )
        if ( dist .lt. distmin ) then
          distmin=dist
          zziu(jibdy,jjbdy) = jiin
          zzju(jibdy,jjbdy) = jjin
        endif
      enddo 
      enddo
      !- East-West bounds : 
      if ( perio .eq. 1 ) then ! periodic
        if     ( zziu(jibdy,jjbdy)+1 .gt. jpiin ) then
          zziup1(jibdy,jjbdy) = 3
          zzium1(jibdy,jjbdy) = zziu(jibdy,jjbdy) - 1
        elseif ( zziu(jibdy,jjbdy)-1 .lt. 1     ) then
          zziup1(jibdy,jjbdy) = zziu(jibdy,jjbdy) + 1
          zzium1(jibdy,jjbdy) = jpiin-2
        else
          zziup1(jibdy,jjbdy) = zziu(jibdy,jjbdy) + 1
          zzium1(jibdy,jjbdy) = zziu(jibdy,jjbdy) - 1
        endif
      else ! non-periodic
        if     ( zziu(jibdy,jjbdy)+1 .gt. jpiin ) then
          zziup1(jibdy,jjbdy) = zziu(jibdy,jjbdy)
          zzium1(jibdy,jjbdy) = zziu(jibdy,jjbdy) - 1
        elseif ( zziu(jibdy,jjbdy)-1 .lt. 1     ) then
          zziup1(jibdy,jjbdy) = zziu(jibdy,jjbdy) + 1
          zzium1(jibdy,jjbdy) = zziu(jibdy,jjbdy)
        else
          zziup1(jibdy,jjbdy) = zziu(jibdy,jjbdy) + 1
          zzium1(jibdy,jjbdy) = zziu(jibdy,jjbdy) - 1
        endif
      endif
      !- upper lower bounds of global input grid
      if     ( zzju(jibdy,jjbdy)+1 .gt. jpjin ) then
        zzjup1(jibdy,jjbdy) = zzju(jibdy,jjbdy)
        zzjum1(jibdy,jjbdy) = zzju(jibdy,jjbdy) - 1
      elseif ( zziu(jibdy,jjbdy)-1 .lt. 1     ) then
        zzjup1(jibdy,jjbdy) = zzju(jibdy,jjbdy) + 1
        zzjum1(jibdy,jjbdy) = zzju(jibdy,jjbdy)
      else
        zzjup1(jibdy,jjbdy) = zzju(jibdy,jjbdy) + 1
        zzjum1(jibdy,jjbdy) = zzju(jibdy,jjbdy) - 1
      endif

      !-- local angle on global input grid between i-direction and the zonal direction and between j-direction and the meridional direction
      anginX  = ATAN2(    gphiuin( zziup1(jibdy,jjbdy) , zzju(jibdy,jjbdy) ) -  gphiuin( zzium1(jibdy,jjbdy) , zzju(jibdy,jjbdy)) , &
      &                ( zglamuin( zziup1(jibdy,jjbdy) , zzju(jibdy,jjbdy) ) - zglamuin( zzium1(jibdy,jjbdy) , zzju(jibdy,jjbdy)) ) &
      &                * cos( gphiuin( zziu(jibdy,jjbdy) , zzju(jibdy,jjbdy) ) * rad )                                                )
      anginY  = ATAN2(    gphiuin( zziu(jibdy,jjbdy) , zzjup1(jibdy,jjbdy) ) -  gphiuin( zziu(jibdy,jjbdy) , zzjum1(jibdy,jjbdy)) , &
      &                ( zglamuin( zziu(jibdy,jjbdy) , zzjup1(jibdy,jjbdy) ) - zglamuin( zziu(jibdy,jjbdy) , zzjum1(jibdy,jjbdy)) ) &
      &                * cos( gphiuin( zziu(jibdy,jjbdy) , zzju(jibdy,jjbdy) ) * rad )                                                )

      !-- local angle between the two grids :
      angleXu(jibdy,jjbdy) = angbdyXu(jibdy,jjbdy) - anginX
      angleYu(jibdy,jjbdy) = angbdyYu(jibdy,jjbdy) - anginY
      if ( abs(angleXu(jibdy,jjbdy)/rad) .gt. 45.0 .or. abs(angleYu(jibdy,jjbdy)/rad) .gt. 45.0 ) then
        write(*,*) '@@@@@@ WARNING : angle between 2 U-grids > 45 deg at point of the bdy :', jibdy, jjbdy
      endif     
 
      !------------------------------------------------------------
      !   coordinates of closest points along X and weights
      !   ( take 2 or 3 points according to mesh size )
      !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
      if      ( e1ubdy(jibdy,jjbdy) .gt. 50*e1uin(zziu(jibdy,jjbdy),zzju(jibdy,jjbdy))*cos(angleXu(jibdy,jjbdy)*rad) ) then ! it should even be 3 here
        write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along X >>>>>>> stop'
        write(*,*) '(jibdy,jjbdy) =', jibdy,jjbdy
        write(*,*) 'angleXu =', angleXu(jibdy,jjbdy)
        write(*,*) 'e1ubdy(jibdy,jjbdy) =', e1ubdy(jibdy,jjbdy)
        write(*,*) 'e1uin(zziu(jibdy,jjbdy),zzju(jibdy,jjbdy))*cos(angleXu(jibdy,jjbdy)*rad)=', e1uin(zziu(jibdy,jjbdy),zzju(jibdy,jjbdy))*cos(angleXu(jibdy,jjbdy)*rad)
        stop
      elseif  (   e1ubdy(jibdy,jjbdy) .ge. e1uin(zziu(jibdy,jjbdy),zzju(jibdy,jjbdy)) * cos(angleXu(jibdy,jjbdy)*rad)  ) then
        !- use 3 points for interpolation
        ziWu(jibdy,jjbdy) = zzium1(jibdy,jjbdy) ! closest point westward
        ziCu(jibdy,jjbdy) = zziu  (jibdy,jjbdy) ! closest point ( central )
        ziEu(jibdy,jjbdy) = zziup1(jibdy,jjbdy) ! closest point eastward
        wgCu(jibdy,jjbdy) = e1uin(zziu(jibdy,jjbdy),zzju(jibdy,jjbdy))* cos(angleXu(jibdy,jjbdy)*rad) !weight for central point
      elseif  (       zlonbdyu(jibdy,jjbdy) .ge.  zglamuin(zziu  (jibdy,jjbdy),zzju(jibdy,jjbdy))        )  then
        !- use 2 points for interpolation
        ziWu(jibdy,jjbdy) = zziu  (jibdy,jjbdy) ! closest point westward
        ziCu(jibdy,jjbdy) = zziu  (jibdy,jjbdy) ! dummy
        ziEu(jibdy,jjbdy) = zziup1(jibdy,jjbdy) ! closest point eastward
        wgCu(jibdy,jjbdy) = 0.0                 ! weight for central point
      else
        !- use 2 points for interpolation
        ziWu(jibdy,jjbdy) = zzium1(jibdy,jjbdy) ! closest point westward
        ziCu(jibdy,jjbdy) = zziu  (jibdy,jjbdy) ! dummy
        ziEu(jibdy,jjbdy) = zziu  (jibdy,jjbdy) ! closest point eastward
        wgCu(jibdy,jjbdy) = 0.0                 ! weight for central point
      endif
      zjWu(jibdy,jjbdy) = zzju  (jibdy,jjbdy) !  "
      zjCu(jibdy,jjbdy) = zzju  (jibdy,jjbdy) ! to be adapted if angles > 45 deg (??)
      zjEu(jibdy,jjbdy) = zzju  (jibdy,jjbdy) !  "
      !- weight for westward point
      wgWu(jibdy,jjbdy) = MAX( 0.0,                                                                                         &
        &                     0.5 * e1uin(ziWu(jibdy,jjbdy),zjWu(jibdy,jjbdy)) * cos(angleXu(jibdy,jjbdy)*rad)                &
        &                   - ra * rad * abs(   ( zlonbdyu(jibdy,jjbdy) - zglamuin(ziWu(jibdy,jjbdy),zjWu(jibdy,jjbdy)) )     &
        &                                     * cos(  latbdyu(jibdy,jjbdy)*rad )                                           )  &
        &                   + 0.5 * e1ubdy(jibdy,jjbdy)                                                                       )
      !- weight for eastward point
      wgEu(jibdy,jjbdy) = MAX( 0.0,                                                                                         &
        &                     0.5 * e1uin(ziEu(jibdy,jjbdy),zjEu(jibdy,jjbdy)) * cos(angleXu(jibdy,jjbdy)*rad)                &
        &                   - ra * rad * abs(  ( zlonbdyu(jibdy,jjbdy) - zglamuin(ziEu(jibdy,jjbdy),zjEu(jibdy,jjbdy)) )      &
        &                                     * cos(  latbdyu(jibdy,jjbdy)*rad )                                           )  &
        &                   + 0.5 * e1ubdy(jibdy,jjbdy)                                                                       )

      !------------------------------------------------------------
      !   coordinates of closest points along Y and weights
      !   ( take 2 or 3 points according to mesh size )
      !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
      if      ( e2ubdy(jibdy,jjbdy) .gt. 50*e2uin(zziu(jibdy,jjbdy),zzju(jibdy,jjbdy))*cos(angleYu(jibdy,jjbdy)*rad)  ) then ! it should even be 3 here
        write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along Y >>>>>>> stop'
        write(*,*) '(jibdy,jjbdy) =', jibdy,jjbdy
        write(*,*) 'angleYu =', angleYu(jibdy,jjbdy)
        write(*,*) 'e2ubdy(jibdy,jjbdy) =', e2ubdy(jibdy,jjbdy)
        write(*,*) 'e2uin(zziu(jibdy,jjbdy),zzju(jibdy,jjbdy))*cos(angleYu(jibdy,jjbdy)*rad)=', e2uin(zziu(jibdy,jjbdy),zzju(jibdy,jjbdy))*cos(angleYu(jibdy,jjbdy)*rad)
        stop
      elseif  (   e2ubdy(jibdy,jjbdy) .ge. e2uin(zziu(jibdy,jjbdy),zzju(jibdy,jjbdy)) * cos(angleYu(jibdy,jjbdy)*rad)  ) then
        !- use 3 points for interpolation
        zjSu(jibdy,jjbdy) = zzjum1(jibdy,jjbdy) ! closest point southward
        zjMu(jibdy,jjbdy) = zzju  (jibdy,jjbdy) ! closest point ( middle )
        zjNu(jibdy,jjbdy) = zzjup1(jibdy,jjbdy) ! closest point northward
        wgMu(jibdy,jjbdy) = e2uin(zziu(jibdy,jjbdy),zzju(jibdy,jjbdy))* cos(angleYu(jibdy,jjbdy)*rad) !weight for central point
      elseif  (       latbdyu(jibdy,jjbdy) .ge.  gphiuin(zziu  (jibdy,jjbdy),zzju(jibdy,jjbdy))        )  then
        !- use 2 points for interpolation
        zjSu(jibdy,jjbdy) = zzju  (jibdy,jjbdy) ! closest point southward
        zjMu(jibdy,jjbdy) = zzju  (jibdy,jjbdy) ! dummy
        zjNu(jibdy,jjbdy) = zzjup1(jibdy,jjbdy) ! closest point northward
        wgMu(jibdy,jjbdy) = 0.0                 ! weight for middle point
      else
        !- use 2 points for interpolation
        zjSu(jibdy,jjbdy) = zzjum1(jibdy,jjbdy) ! closest point southward
        zjMu(jibdy,jjbdy) = zzju  (jibdy,jjbdy) ! dummy
        zjNu(jibdy,jjbdy) = zzju  (jibdy,jjbdy) ! closest point northward
        wgMu(jibdy,jjbdy) = 0.0                 ! weight for middle point
      endif
      ziSu(jibdy,jjbdy) = zziu  (jibdy,jjbdy) ! "
      ziMu(jibdy,jjbdy) = zziu  (jibdy,jjbdy) ! to be adapted if angles > 45 deg (??)
      ziNu(jibdy,jjbdy) = zziu  (jibdy,jjbdy) ! "
      !- weight for southward point
      wgSu(jibdy,jjbdy) = MAX( 0.0,                                                                                  &
        &                     0.5 * e2uin(ziSu(jibdy,jjbdy),zjSu(jibdy,jjbdy)) * cos(angleYu(jibdy,jjbdy)*rad)         &
        &                   - ra * rad * abs(  latbdyu(jibdy,jjbdy) - gphiuin(ziSu(jibdy,jjbdy),zjSu(jibdy,jjbdy)) )   &
        &                   + 0.5 * e2ubdy(jibdy,jjbdy)                                                                )
      !- weight for northward point
      wgNu(jibdy,jjbdy) = MAX( 0.0,                                                                                  &
        &                     0.5 * e2uin(ziNu(jibdy,jjbdy),zjNu(jibdy,jjbdy)) * cos(angleYu(jibdy,jjbdy)*rad)         &
        &                   - ra * rad * abs(  latbdyu(jibdy,jjbdy) - gphiuin(ziNu(jibdy,jjbdy),zjNu(jibdy,jjbdy)) )   &
        &                   + 0.5 * e2ubdy(jibdy,jjbdy)                                                                )

      !----------------------------------------------------------------
      !  NO VERTICAL INTERPOLATION HERE (ASSUMED TO BE THE SAME GRID) !
      !----------------------------------------------------------------  

    ENDDO !-- jpjbdy
    ENDDO !-- jpibdy
   
    !===============================================
    ! 3.3 find the closest V point and coefficients
    !===============================================

    ALLOCATE( ziWv  (jpicnt,jpjcnt), zjWv  (jpicnt,jpjcnt), wgWv(jpicnt,jpjcnt) )
    ALLOCATE( ziCv  (jpicnt,jpjcnt), zjCv  (jpicnt,jpjcnt), wgCv(jpicnt,jpjcnt) )
    ALLOCATE( ziEv  (jpicnt,jpjcnt), zjEv  (jpicnt,jpjcnt), wgEv(jpicnt,jpjcnt) )
    ALLOCATE( ziSv  (jpicnt,jpjcnt), zjSv  (jpicnt,jpjcnt), wgSv(jpicnt,jpjcnt) )
    ALLOCATE( ziMv  (jpicnt,jpjcnt), zjMv  (jpicnt,jpjcnt), wgMv(jpicnt,jpjcnt) )
    ALLOCATE( ziNv  (jpicnt,jpjcnt), zjNv  (jpicnt,jpjcnt), wgNv(jpicnt,jpjcnt) )
    ALLOCATE( zziv  (jpicnt,jpjcnt), zzjv  (jpicnt,jpjcnt)                      )
    ALLOCATE( zzivm1(jpicnt,jpjcnt), zzjvm1(jpicnt,jpjcnt)                      )
    ALLOCATE( zzivp1(jpicnt,jpjcnt), zzjvp1(jpicnt,jpjcnt)                      )

    IF ( idateline .EQ. 1 ) THEN
      where ( lonbdyv(:,:) .lt. 0.0 ) 
        zlonbdyv(:,:) = 360.0 + lonbdyv(:,:)
      elsewhere
        zlonbdyv(:,:) = lonbdyv(:,:)
      endwhere
      where ( glamvin(:,:) .lt. 0.0 )
        zglamvin(:,:) = 360.0 + glamvin(:,:)
      elsewhere
        zglamvin(:,:) = glamvin(:,:)
      endwhere
    ELSE
      zglamvin(:,:) = glamvin(:,:) 
    ENDIF

    zziv(:,:)=0 !- to notice error if a point is not filled
    zzjv(:,:)=0 !- to notice error if a point is not filled

    DO jibdy=1,jpicnt
    DO jjbdy=1,jpjcnt

      distmin = ra * rad * 10.0 ! let's assume that nobody's using grids coarser than 10 degrees...

      !-- loop to find closest point ( we allow 1 bdy cell to cover a maximum of 3 cells from the input grid )
      do jiin=1,jpiin
      do jjin=1,jpjin
        dist = ra * rad * sqrt(   ( cos(rad*latbdyv(jibdy,jjbdy)) * ( zlonbdyv(jibdy,jjbdy) - zglamvin(jiin,jjin) ) )**2  &
          &                     + (                                    latbdyv(jibdy,jjbdy) -  gphivin(jiin,jjin)   )**2  )
        if ( dist .lt. distmin ) then
          distmin=dist
          zziv(jibdy,jjbdy) = jiin
          zzjv(jibdy,jjbdy) = jjin
        endif
      enddo 
      enddo
      !- East-West bounds : 
      if ( perio .eq. 1 ) then ! periodic
        if     ( zziv(jibdy,jjbdy)+1 .gt. jpiin ) then
          zzivp1(jibdy,jjbdy) = 3
          zzivm1(jibdy,jjbdy) = zziv(jibdy,jjbdy) - 1
        elseif ( zziv(jibdy,jjbdy)-1 .lt. 1     ) then
          zzivp1(jibdy,jjbdy) = zziv(jibdy,jjbdy) + 1
          zzivm1(jibdy,jjbdy) = jpiin-2
        else
          zzivp1(jibdy,jjbdy) = zziv(jibdy,jjbdy) + 1
          zzivm1(jibdy,jjbdy) = zziv(jibdy,jjbdy) - 1
        endif
      else ! non-periodic
        if     ( zziv(jibdy,jjbdy)+1 .gt. jpiin ) then
          zzivp1(jibdy,jjbdy) = zziv(jibdy,jjbdy)
          zzivm1(jibdy,jjbdy) = zziv(jibdy,jjbdy) - 1
        elseif ( zziv(jibdy,jjbdy)-1 .lt. 1     ) then
          zzivp1(jibdy,jjbdy) = zziv(jibdy,jjbdy) + 1
          zzivm1(jibdy,jjbdy) = zziv(jibdy,jjbdy)
        else
          zzivp1(jibdy,jjbdy) = zziv(jibdy,jjbdy) + 1
          zzivm1(jibdy,jjbdy) = zziv(jibdy,jjbdy) - 1
        endif
      endif
      !- upper lower bounds of global input grid
      if     ( zzjv(jibdy,jjbdy)+1 .gt. jpjin ) then
        zzjvp1(jibdy,jjbdy) = zzjv(jibdy,jjbdy)
        zzjvm1(jibdy,jjbdy) = zzjv(jibdy,jjbdy) - 1
      elseif ( zziv(jibdy,jjbdy)-1 .lt. 1     ) then
        zzjvp1(jibdy,jjbdy) = zzjv(jibdy,jjbdy) + 1
        zzjvm1(jibdy,jjbdy) = zzjv(jibdy,jjbdy)
      else
        zzjvp1(jibdy,jjbdy) = zzjv(jibdy,jjbdy) + 1
        zzjvm1(jibdy,jjbdy) = zzjv(jibdy,jjbdy) - 1
      endif

      !-- local angle on global input grid between i-direction and the zonal direction and between j-direction and the meridional direction
      anginX  = ATAN2(    gphivin( zzivp1(jibdy,jjbdy) , zzjv(jibdy,jjbdy) ) -  gphivin( zzivm1(jibdy,jjbdy) , zzjv(jibdy,jjbdy)) , &
      &                ( zglamvin( zzivp1(jibdy,jjbdy) , zzjv(jibdy,jjbdy) ) - zglamvin( zzivm1(jibdy,jjbdy) , zzjv(jibdy,jjbdy)) ) &
      &                * cos( gphivin( zziv(jibdy,jjbdy) , zzjv(jibdy,jjbdy) ) * rad )                                                )
      anginY  = ATAN2(    gphivin( zziv(jibdy,jjbdy) , zzjvp1(jibdy,jjbdy) ) -  gphivin( zziv(jibdy,jjbdy) , zzjvm1(jibdy,jjbdy)) , &
      &                ( zglamvin( zziv(jibdy,jjbdy) , zzjvp1(jibdy,jjbdy) ) - zglamvin( zziv(jibdy,jjbdy) , zzjvm1(jibdy,jjbdy)) ) &
      &                * cos( gphivin( zziv(jibdy,jjbdy) , zzjv(jibdy,jjbdy) ) * rad )                                                )

      !-- local angle between the two grids :
      angleXv(jibdy,jjbdy) = angbdyXv(jibdy,jjbdy) - anginX
      angleYv(jibdy,jjbdy) = angbdyYv(jibdy,jjbdy) - anginY
      if ( abs(angleXv(jibdy,jjbdy)/rad) .gt. 45.0 .or. abs(angleYv(jibdy,jjbdy)/rad) .gt. 45.0 ) then
        write(*,*) '@@@@@@ WARNING : angle between 2 V-grids > 45 deg at point of the bdy :', jibdy, jjbdy
      endif     
 
      !------------------------------------------------------------
      !   coordinates of closest points along X and weights
      !   ( take 2 or 3 points according to mesh size )
      !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
      if      ( e1vbdy(jibdy,jjbdy) .gt. 50*e1vin(zziv(jibdy,jjbdy),zzjv(jibdy,jjbdy))*cos(angleXv(jibdy,jjbdy)*rad) ) then ! it should even be 3 here
        write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along X >>>>>>> stop'
        write(*,*) '(jibdy,jjbdy) =', jibdy,jjbdy
        write(*,*) 'angleXv =', angleXv(jibdy,jjbdy)
        write(*,*) 'e1vbdy(jibdy,jjbdy) =', e1vbdy(jibdy,jjbdy)
        write(*,*) 'e1vin(zziv(jibdy,jjbdy),zzjv(jibdy,jjbdy))*cos(angleXv(jibdy,jjbdy)*rad)=', e1vin(zziv(jibdy,jjbdy),zzjv(jibdy,jjbdy))*cos(angleXv(jibdy,jjbdy)*rad)
        stop
      elseif  (   e1vbdy(jibdy,jjbdy) .ge. e1vin(zziv(jibdy,jjbdy),zzjv(jibdy,jjbdy)) * cos(angleXv(jibdy,jjbdy)*rad)  ) then
        !- use 3 points for interpolation
        ziWv(jibdy,jjbdy) = zzivm1(jibdy,jjbdy) ! closest point westward
        ziCv(jibdy,jjbdy) = zziv  (jibdy,jjbdy) ! closest point ( central )
        ziEv(jibdy,jjbdy) = zzivp1(jibdy,jjbdy) ! closest point eastward
        wgCv(jibdy,jjbdy) = e1vin(zziv(jibdy,jjbdy),zzjv(jibdy,jjbdy))* cos(angleXv(jibdy,jjbdy)*rad) !weight for central point
      elseif  (       zlonbdyv(jibdy,jjbdy) .ge.  zglamvin(zziv  (jibdy,jjbdy),zzjv(jibdy,jjbdy))        )  then
        !- use 2 points for interpolation
        ziWv(jibdy,jjbdy) = zziv  (jibdy,jjbdy) ! closest point westward
        ziCv(jibdy,jjbdy) = zziv  (jibdy,jjbdy) ! dummy
        ziEv(jibdy,jjbdy) = zzivp1(jibdy,jjbdy) ! closest point eastward
        wgCv(jibdy,jjbdy) = 0.0                 ! weight for central point
      else
        !- use 2 points for interpolation
        ziWv(jibdy,jjbdy) = zzivm1(jibdy,jjbdy) ! closest point westward
        ziCv(jibdy,jjbdy) = zziv  (jibdy,jjbdy) ! dummy
        ziEv(jibdy,jjbdy) = zziv  (jibdy,jjbdy) ! closest point eastward
        wgCv(jibdy,jjbdy) = 0.0                 ! weight for central point
      endif
      zjWv(jibdy,jjbdy) = zzjv  (jibdy,jjbdy) !  "
      zjCv(jibdy,jjbdy) = zzjv  (jibdy,jjbdy) ! to be adapted if angles > 45 deg (??)
      zjEv(jibdy,jjbdy) = zzjv  (jibdy,jjbdy) !  "
      !- weight for westward point
      wgWv(jibdy,jjbdy) = MAX( 0.0,                                                                                         &
        &                     0.5 * e1vin(ziWv(jibdy,jjbdy),zjWv(jibdy,jjbdy)) * cos(angleXv(jibdy,jjbdy)*rad)                &
        &                   - ra * rad * abs(   ( zlonbdyv(jibdy,jjbdy) - zglamvin(ziWv(jibdy,jjbdy),zjWv(jibdy,jjbdy)) )     &
        &                                     * cos(  latbdyv(jibdy,jjbdy)*rad )                                           )  &
        &                   + 0.5 * e1vbdy(jibdy,jjbdy)                                                                       )
      !- weight for eastward point
      wgEv(jibdy,jjbdy) = MAX( 0.0,                                                                                         &
        &                     0.5 * e1vin(ziEv(jibdy,jjbdy),zjEv(jibdy,jjbdy)) * cos(angleXv(jibdy,jjbdy)*rad)                &
        &                   - ra * rad * abs(  ( zlonbdyv(jibdy,jjbdy) - zglamvin(ziEv(jibdy,jjbdy),zjEv(jibdy,jjbdy)) )      &
        &                                     * cos(  latbdyv(jibdy,jjbdy)*rad )                                           )  &
        &                   + 0.5 * e1vbdy(jibdy,jjbdy)                                                                       )

      !------------------------------------------------------------
      !   coordinates of closest points along Y and weights
      !   ( take 2 or 3 points according to mesh size )
      !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
      if      ( e2vbdy(jibdy,jjbdy) .gt. 50*e2vin(zziv(jibdy,jjbdy),zzjv(jibdy,jjbdy))*cos(angleYv(jibdy,jjbdy)*rad)  ) then ! it should even be 3 here
        write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along Y >>>>>>> stop'
        write(*,*) '(jibdy,jjbdy) =', jibdy,jjbdy
        write(*,*) 'angleYv =', angleYv(jibdy,jjbdy)
        write(*,*) 'e2vbdy(jibdy,jjbdy) =', e2vbdy(jibdy,jjbdy)
        write(*,*) 'e2vin(zziv(jibdy,jjbdy),zzjv(jibdy,jjbdy))*cos(angleYv(jibdy,jjbdy)*rad)=', e2vin(zziv(jibdy,jjbdy),zzjv(jibdy,jjbdy))*cos(angleYv(jibdy,jjbdy)*rad)
        stop
      elseif  (   e2vbdy(jibdy,jjbdy) .ge. e2vin(zziv(jibdy,jjbdy),zzjv(jibdy,jjbdy)) * cos(angleYv(jibdy,jjbdy)*rad)  ) then
        !- use 3 points for interpolation
        zjSv(jibdy,jjbdy) = zzjvm1(jibdy,jjbdy) ! closest point southward
        zjMv(jibdy,jjbdy) = zzjv  (jibdy,jjbdy) ! closest point ( middle )
        zjNv(jibdy,jjbdy) = zzjvp1(jibdy,jjbdy) ! closest point northward
        wgMv(jibdy,jjbdy) = e2vin(zziv(jibdy,jjbdy),zzjv(jibdy,jjbdy))* cos(angleYv(jibdy,jjbdy)*rad) !weight for central point
      elseif  (       latbdyv(jibdy,jjbdy) .ge.  gphivin(zziv  (jibdy,jjbdy),zzjv(jibdy,jjbdy))        )  then
        !- use 2 points for interpolation
        zjSv(jibdy,jjbdy) = zzjv  (jibdy,jjbdy) ! closest point southward
        zjMv(jibdy,jjbdy) = zzjv  (jibdy,jjbdy) ! dummy
        zjNv(jibdy,jjbdy) = zzjvp1(jibdy,jjbdy) ! closest point northward
        wgMv(jibdy,jjbdy) = 0.0                 ! weight for middle point
      else
        !- use 2 points for interpolation
        zjSv(jibdy,jjbdy) = zzjvm1(jibdy,jjbdy) ! closest point southward
        zjMv(jibdy,jjbdy) = zzjv  (jibdy,jjbdy) ! dummy
        zjNv(jibdy,jjbdy) = zzjv  (jibdy,jjbdy) ! closest point northward
        wgMv(jibdy,jjbdy) = 0.0                 ! weight for middle point
      endif
      ziSv(jibdy,jjbdy) = zziv  (jibdy,jjbdy) ! "
      ziMv(jibdy,jjbdy) = zziv  (jibdy,jjbdy) ! to be adapted if angles > 45 deg (??)
      ziNv(jibdy,jjbdy) = zziv  (jibdy,jjbdy) ! "
      !- weight for southward point
      wgSv(jibdy,jjbdy) = MAX( 0.0,                                                                                  &
        &                     0.5 * e2vin(ziSv(jibdy,jjbdy),zjSv(jibdy,jjbdy)) * cos(angleYv(jibdy,jjbdy)*rad)         &
        &                   - ra * rad * abs(  latbdyv(jibdy,jjbdy) - gphivin(ziSv(jibdy,jjbdy),zjSv(jibdy,jjbdy)) )   &
        &                   + 0.5 * e2vbdy(jibdy,jjbdy)                                                                )
      !- weight for northward point
      wgNv(jibdy,jjbdy) = MAX( 0.0,                                                                                  &
        &                     0.5 * e2vin(ziNv(jibdy,jjbdy),zjNv(jibdy,jjbdy)) * cos(angleYv(jibdy,jjbdy)*rad)         &
        &                   - ra * rad * abs(  latbdyv(jibdy,jjbdy) - gphivin(ziNv(jibdy,jjbdy),zjNv(jibdy,jjbdy)) )   &
        &                   + 0.5 * e2vbdy(jibdy,jjbdy)                                                                )

      !----------------------------------------------------------------
      !  NO VERTICAL INTERPOLATION HERE (ASSUMED TO BE THE SAME GRID) !
      !----------------------------------------------------------------  

    ENDDO !-- jpjbdy
    ENDDO !-- jpibdy

    !===============================================
    ! 3.4 find the closest F point and coefficients
    !===============================================

    ALLOCATE( ziWf  (jpicntf,jpjcntf), zjWf  (jpicntf,jpjcntf), wgWf(jpicntf,jpjcntf) )
    ALLOCATE( ziCf  (jpicntf,jpjcntf), zjCf  (jpicntf,jpjcntf), wgCf(jpicntf,jpjcntf) )
    ALLOCATE( ziEf  (jpicntf,jpjcntf), zjEf  (jpicntf,jpjcntf), wgEf(jpicntf,jpjcntf) )
    ALLOCATE( ziSf  (jpicntf,jpjcntf), zjSf  (jpicntf,jpjcntf), wgSf(jpicntf,jpjcntf) )
    ALLOCATE( ziMf  (jpicntf,jpjcntf), zjMf  (jpicntf,jpjcntf), wgMf(jpicntf,jpjcntf) )
    ALLOCATE( ziNf  (jpicntf,jpjcntf), zjNf  (jpicntf,jpjcntf), wgNf(jpicntf,jpjcntf) )
    ALLOCATE( zzif  (jpicntf,jpjcntf), zzjf  (jpicntf,jpjcntf)                        )
    ALLOCATE( zzifm1(jpicntf,jpjcntf), zzjfm1(jpicntf,jpjcntf)                        )
    ALLOCATE( zzifp1(jpicntf,jpjcntf), zzjfp1(jpicntf,jpjcntf)                        )

    IF ( idateline .EQ. 1 ) THEN
      where ( lonbdyf(:,:) .lt. 0.0 ) 
        zlonbdyf(:,:) = 360.0 + lonbdyf(:,:)
      elsewhere
        zlonbdyf(:,:) = lonbdyf(:,:)
      endwhere
      where ( glamfin(:,:) .lt. 0.0 )
        zglamfin(:,:) = 360.0 + glamfin(:,:)
      elsewhere
        zglamfin(:,:) = glamfin(:,:)
      endwhere
    ELSE
      zglamfin(:,:) = glamfin(:,:) 
    ENDIF

    zzif(:,:)=0 !- to notice error if a point is not filled
    zzjf(:,:)=0 !- to notice error if a point is not filled

    DO jibdy=1,jpicntf
    DO jjbdy=1,jpjcntf

      distmin = ra * rad * 10.0 ! let's assume that nobody's using grids coarser than 10 degrees...

      !-- loop to find closest point ( we allow 1 bdy cell to cover a maximum of 3 cells from the input grid )
      do jiin=1,jpiin
      do jjin=1,jpjin
        dist = ra * rad * sqrt(   ( cos(rad*latbdyf(jibdy,jjbdy)) * ( zlonbdyf(jibdy,jjbdy) - zglamfin(jiin,jjin) ) )**2  &
          &                     + (                                    latbdyf(jibdy,jjbdy) -  gphifin(jiin,jjin)   )**2  )
        if ( dist .lt. distmin ) then
          distmin=dist
          zzif(jibdy,jjbdy) = jiin
          zzjf(jibdy,jjbdy) = jjin
        endif
      enddo 
      enddo
      !- East West bounds :
      if (perio .eq. 1 ) then
        if     ( zzif(jibdy,jjbdy)+1 .gt. jpiin ) then
          zzifp1(jibdy,jjbdy) = 3
          zzifm1(jibdy,jjbdy) = zzif(jibdy,jjbdy) - 1
        elseif ( zzif(jibdy,jjbdy)-1 .lt. 1     ) then
          zzifp1(jibdy,jjbdy) = zzif(jibdy,jjbdy) + 1
          zzifm1(jibdy,jjbdy) = jpiin-2
        else
          zzifp1(jibdy,jjbdy) = zzif(jibdy,jjbdy) + 1
          zzifm1(jibdy,jjbdy) = zzif(jibdy,jjbdy) - 1
        endif
      else ! non-periodic
        if     ( zzif(jibdy,jjbdy)+1 .gt. jpiin ) then
          zzifp1(jibdy,jjbdy) = zzif(jibdy,jjbdy)
          zzifm1(jibdy,jjbdy) = zzif(jibdy,jjbdy) - 1
        elseif ( zzif(jibdy,jjbdy)-1 .lt. 1     ) then
          zzifp1(jibdy,jjbdy) = zzif(jibdy,jjbdy) + 1
          zzifm1(jibdy,jjbdy) = zzif(jibdy,jjbdy)
        else
          zzifp1(jibdy,jjbdy) = zzif(jibdy,jjbdy) + 1
          zzifm1(jibdy,jjbdy) = zzif(jibdy,jjbdy) - 1
        endif
      endif
      !- upper lower bounds of global input grid
      if     ( zzjf(jibdy,jjbdy)+1 .gt. jpjin ) then
        zzjfp1(jibdy,jjbdy) = zzjf(jibdy,jjbdy)
        zzjfm1(jibdy,jjbdy) = zzjf(jibdy,jjbdy) - 1
      elseif ( zzif(jibdy,jjbdy)-1 .lt. 1     ) then
        zzjfp1(jibdy,jjbdy) = zzjf(jibdy,jjbdy) + 1
        zzjfm1(jibdy,jjbdy) = zzjf(jibdy,jjbdy)
      else
        zzjfp1(jibdy,jjbdy) = zzjf(jibdy,jjbdy) + 1
        zzjfm1(jibdy,jjbdy) = zzjf(jibdy,jjbdy) - 1
      endif

      !-- local angle on global input grid between i-direction and the zonal direction and between j-direction and the meridional direction
      anginX = ATAN2(    gphifin( zzifp1(jibdy,jjbdy) , zzjf(jibdy,jjbdy) ) -  gphifin( zzifm1(jibdy,jjbdy) , zzjf(jibdy,jjbdy)) , &
      &                ( zglamfin( zzifp1(jibdy,jjbdy) , zzjf(jibdy,jjbdy) ) - zglamfin( zzifm1(jibdy,jjbdy) , zzjf(jibdy,jjbdy)) ) &
      &                * cos( gphifin( zzif(jibdy,jjbdy) , zzjf(jibdy,jjbdy) ) * rad )                                                )
      anginY = ATAN2(    gphifin( zzif(jibdy,jjbdy) , zzjfp1(jibdy,jjbdy) ) -  gphifin( zzif(jibdy,jjbdy) , zzjfm1(jibdy,jjbdy)) , &
      &                ( zglamfin( zzif(jibdy,jjbdy) , zzjfp1(jibdy,jjbdy) ) - zglamfin( zzif(jibdy,jjbdy) , zzjfm1(jibdy,jjbdy)) ) &
      &                * cos( gphifin( zzif(jibdy,jjbdy) , zzjf(jibdy,jjbdy) ) * rad )                                                )

      !-- local angle between the two grids :
      angleXf(jibdy,jjbdy) = angbdyXf(jibdy,jjbdy) - anginX
      angleYf(jibdy,jjbdy) = angbdyYf(jibdy,jjbdy) - anginY
      if ( abs(angleXf(jibdy,jjbdy)/rad) .gt. 45.0 .or. abs(angleYf(jibdy,jjbdy)/rad) .gt. 45.0 ) then
        write(*,*) '@@@@@@ WARNING : angle between 2 F-grids > 45 deg'
        write(*,*) '                 at point of the bdy :', jibdy, jjbdy
        write(*,*) '                 angles = ', angbdyXf(jibdy,jjbdy)/rad, angbdyYf(jibdy,jjbdy)/rad
        write(*,*) '                 angles = ', anginX, anginY
      endif     
 
      !------------------------------------------------------------
      !   coordinates of closest points along X and weights
      !   ( take 2 or 3 points according to mesh size )
      !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
      if      ( e1fbdy(jibdy,jjbdy) .gt. 50*e1fin(zzif(jibdy,jjbdy),zzjf(jibdy,jjbdy))*cos(angleXf(jibdy,jjbdy)*rad) ) then ! it should even be 3 here
        write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along X >>>>>>> stop'
        write(*,*) '(jibdy,jjbdy) =', jibdy,jjbdy
        write(*,*) 'angleXf =', angleXf(jibdy,jjbdy)
        write(*,*) 'e1fbdy(jibdy,jjbdy) =', e1fbdy(jibdy,jjbdy)
        write(*,*) 'e1fin(zzif(jibdy,jjbdy),zzjf(jibdy,jjbdy))*cos(angleXf(jibdy,jjbdy)*rad)=', e1fin(zzif(jibdy,jjbdy),zzjf(jibdy,jjbdy))*cos(angleXf(jibdy,jjbdy)*rad)
        stop
      elseif  (   e1fbdy(jibdy,jjbdy) .ge. e1fin(zzif(jibdy,jjbdy),zzjf(jibdy,jjbdy)) * cos(angleXf(jibdy,jjbdy)*rad)  ) then
        !- use 3 points for interpolation
        ziWf(jibdy,jjbdy) = zzifm1(jibdy,jjbdy) ! closest point westward
        ziCf(jibdy,jjbdy) = zzif  (jibdy,jjbdy) ! closest point ( central )
        ziEf(jibdy,jjbdy) = zzifp1(jibdy,jjbdy) ! closest point eastward
        wgCf(jibdy,jjbdy) = e1fin(zzif(jibdy,jjbdy),zzjf(jibdy,jjbdy))* cos(angleXf(jibdy,jjbdy)*rad) !weight for central point
      elseif  (       zlonbdyf(jibdy,jjbdy) .ge.  zglamfin(zzif  (jibdy,jjbdy),zzjf(jibdy,jjbdy))        )  then
        !- use 2 points for interpolation
        ziWf(jibdy,jjbdy) = zzif  (jibdy,jjbdy) ! closest point westward
        ziCf(jibdy,jjbdy) = zzif  (jibdy,jjbdy) ! dummy
        ziEf(jibdy,jjbdy) = zzifp1(jibdy,jjbdy) ! closest point eastward
        wgCf(jibdy,jjbdy) = 0.0                 ! weight for central point
      else
        !- use 2 points for interpolation
        ziWf(jibdy,jjbdy) = zzifm1(jibdy,jjbdy) ! closest point westward
        ziCf(jibdy,jjbdy) = zzif  (jibdy,jjbdy) ! dummy
        ziEf(jibdy,jjbdy) = zzif  (jibdy,jjbdy) ! closest point eastward
        wgCf(jibdy,jjbdy) = 0.0                 ! weight for central point
      endif
      zjWf(jibdy,jjbdy) = zzjf  (jibdy,jjbdy) !  "
      zjCf(jibdy,jjbdy) = zzjf  (jibdy,jjbdy) ! to be adapted if angles > 45 deg (??)
      zjEf(jibdy,jjbdy) = zzjf  (jibdy,jjbdy) !  "
      !- weight for westward point
      wgWf(jibdy,jjbdy) = MAX( 0.0,                                                                                         &
        &                     0.5 * e1fin(ziWf(jibdy,jjbdy),zjWf(jibdy,jjbdy)) * cos(angleXf(jibdy,jjbdy)*rad)                &
        &                   - ra * rad * abs(   ( zlonbdyf(jibdy,jjbdy) - zglamfin(ziWf(jibdy,jjbdy),zjWf(jibdy,jjbdy)) )     &
        &                                     * cos(  latbdyf(jibdy,jjbdy)*rad )                                           )  &
        &                   + 0.5 * e1fbdy(jibdy,jjbdy)                                                                       )
      !- weight for eastward point
      wgEf(jibdy,jjbdy) = MAX( 0.0,                                                                                         &
        &                     0.5 * e1fin(ziEf(jibdy,jjbdy),zjEf(jibdy,jjbdy)) * cos(angleXf(jibdy,jjbdy)*rad)                &
        &                   - ra * rad * abs(  ( zlonbdyf(jibdy,jjbdy) - zglamfin(ziEf(jibdy,jjbdy),zjEf(jibdy,jjbdy)) )      &
        &                                     * cos(  latbdyf(jibdy,jjbdy)*rad )                                           )  &
        &                   + 0.5 * e1fbdy(jibdy,jjbdy)                                                                       )

      !------------------------------------------------------------
      !   coordinates of closest points along Y and weights
      !   ( take 2 or 3 points according to mesh size )
      !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
      if      ( e2fbdy(jibdy,jjbdy) .gt. 50*e2fin(zzif(jibdy,jjbdy),zzjf(jibdy,jjbdy))*cos(angleYf(jibdy,jjbdy)*rad)  ) then ! it should even be 3 here
        write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along Y >>>>>>> stop'
        write(*,*) '(jibdy,jjbdy) =', jibdy,jjbdy
        write(*,*) 'angleYf =', angleYf(jibdy,jjbdy)
        write(*,*) 'e2fbdy(jibdy,jjbdy) =', e2fbdy(jibdy,jjbdy)
        write(*,*) 'e2fin(zzif(jibdy,jjbdy),zzjf(jibdy,jjbdy))*cos(angleYf(jibdy,jjbdy)*rad)=', e2fin(zzif(jibdy,jjbdy),zzjf(jibdy,jjbdy))*cos(angleYf(jibdy,jjbdy)*rad)
        stop
      elseif  (   e2fbdy(jibdy,jjbdy) .ge. e2fin(zzif(jibdy,jjbdy),zzjf(jibdy,jjbdy)) * cos(angleYf(jibdy,jjbdy)*rad)  ) then
        !- use 3 points for interpolation
        zjSf(jibdy,jjbdy) = zzjfm1(jibdy,jjbdy) ! closest point southward
        zjMf(jibdy,jjbdy) = zzjf  (jibdy,jjbdy) ! closest point ( middle )
        zjNf(jibdy,jjbdy) = zzjfp1(jibdy,jjbdy) ! closest point northward
        wgMf(jibdy,jjbdy) = e2fin(zzif(jibdy,jjbdy),zzjf(jibdy,jjbdy))* cos(angleYf(jibdy,jjbdy)*rad) !weight for central point
      elseif  (       latbdyf(jibdy,jjbdy) .ge.  gphifin(zzif  (jibdy,jjbdy),zzjf(jibdy,jjbdy))        )  then
        !- use 2 points for interpolation
        zjSf(jibdy,jjbdy) = zzjf  (jibdy,jjbdy) ! closest point southward
        zjMf(jibdy,jjbdy) = zzjf  (jibdy,jjbdy) ! dummy
        zjNf(jibdy,jjbdy) = zzjfp1(jibdy,jjbdy) ! closest point northward
        wgMf(jibdy,jjbdy) = 0.0                 ! weight for middle point
      else
        !- use 2 points for interpolation
        zjSf(jibdy,jjbdy) = zzjfm1(jibdy,jjbdy) ! closest point southward
        zjMf(jibdy,jjbdy) = zzjf  (jibdy,jjbdy) ! dummy
        zjNf(jibdy,jjbdy) = zzjf  (jibdy,jjbdy) ! closest point northward
        wgMf(jibdy,jjbdy) = 0.0                 ! weight for middle point
      endif
      ziSf(jibdy,jjbdy) = zzif  (jibdy,jjbdy) ! "
      ziMf(jibdy,jjbdy) = zzif  (jibdy,jjbdy) ! to be adapted if angles > 45 deg (??)
      ziNf(jibdy,jjbdy) = zzif  (jibdy,jjbdy) ! "
      !- weight for southward point
      wgSf(jibdy,jjbdy) = MAX( 0.0,                                                                                  &
        &                     0.5 * e2fin(ziSf(jibdy,jjbdy),zjSf(jibdy,jjbdy)) * cos(angleYf(jibdy,jjbdy)*rad)         &
        &                   - ra * rad * abs(  latbdyf(jibdy,jjbdy) - gphifin(ziSf(jibdy,jjbdy),zjSf(jibdy,jjbdy)) )   &
        &                   + 0.5 * e2fbdy(jibdy,jjbdy)                                                                )
      !- weight for northward point
      wgNf(jibdy,jjbdy) = MAX( 0.0,                                                                                  &
        &                     0.5 * e2fin(ziNf(jibdy,jjbdy),zjNf(jibdy,jjbdy)) * cos(angleYf(jibdy,jjbdy)*rad)         &
        &                   - ra * rad * abs(  latbdyf(jibdy,jjbdy) - gphifin(ziNf(jibdy,jjbdy),zjNf(jibdy,jjbdy)) )   &
        &                   + 0.5 * e2fbdy(jibdy,jjbdy)                                                                )

      !----------------------------------------------------------------
      !  NO VERTICAL INTERPOLATION HERE (ASSUMED TO BE THE SAME GRID) !
      !----------------------------------------------------------------  

    ENDDO !-- jpjbdy
    ENDDO !-- jpibdy


    !==================================================================================
    ! 4- WRITE EVERYTHING NEEDED FOR INTERPOLATION IN A NETCDF FILE
    !==================================================================================

    !-- OBC output file
    write(file_bdy,102) TRIM(side(iside)), TRIM(conf_par)
    102 FORMAT('coeff_OBC_',a,'_',a,'_from_ORCA025_L75.nc')
    write(*,*) '  '
    write(*,*) '  Writing ', TRIM(file_bdy)

    status = NF90_CREATE(TRIM(file_bdy),or(NF90_NOCLOBBER,NF90_64BIT_OFFSET),fidOBC)
    call erreur(status,.TRUE.,'Create OBC file')                     

    !-- File dimensions 
    status = NF90_DEF_DIM(fidOBC,"z",jpkout,dimID_z)    ; call erreur(status,.TRUE.,"def_dimID_z")
    status = NF90_DEF_DIM(fidOBC,"y",jpjcnt,dimID_y)    ; call erreur(status,.TRUE.,"def_dimID_y")
    status = NF90_DEF_DIM(fidOBC,"x",jpicnt,dimID_x)    ; call erreur(status,.TRUE.,"def_dimID_x")
    status = NF90_DEF_DIM(fidOBC,"yf",jpjcntf,dimID_yf) ; call erreur(status,.TRUE.,"def_dimID_yf")
    status = NF90_DEF_DIM(fidOBC,"xf",jpicntf,dimID_xf) ; call erreur(status,.TRUE.,"def_dimID_xf")     
                                                      
    !-- Variables definition                            
    status = NF90_DEF_VAR(fidOBC,"glamt",NF90_FLOAT,(/dimID_x,dimID_y/),lonbdyt_ID) ; call erreur(status,.TRUE.,"def_var_lonbdyt_ID")
    status = NF90_DEF_VAR(fidOBC,"glamu",NF90_FLOAT,(/dimID_x,dimID_y/),lonbdyu_ID) ; call erreur(status,.TRUE.,"def_var_lonbdyu_ID")
    status = NF90_DEF_VAR(fidOBC,"glamv",NF90_FLOAT,(/dimID_x,dimID_y/),lonbdyv_ID) ; call erreur(status,.TRUE.,"def_var_lonbdyv_ID")
    status = NF90_DEF_VAR(fidOBC,"glamf",NF90_FLOAT,(/dimID_x,dimID_y/),lonbdyf_ID) ; call erreur(status,.TRUE.,"def_var_lonbdyf_ID")
    status = NF90_DEF_VAR(fidOBC,"gphit",NF90_FLOAT,(/dimID_x,dimID_y/),latbdyt_ID) ; call erreur(status,.TRUE.,"def_var_latbdyt_ID")
    status = NF90_DEF_VAR(fidOBC,"gphiu",NF90_FLOAT,(/dimID_x,dimID_y/),latbdyu_ID) ; call erreur(status,.TRUE.,"def_var_latbdyu_ID")
    status = NF90_DEF_VAR(fidOBC,"gphiv",NF90_FLOAT,(/dimID_x,dimID_y/),latbdyv_ID) ; call erreur(status,.TRUE.,"def_var_latbdyv_ID")
    status = NF90_DEF_VAR(fidOBC,"gphif",NF90_FLOAT,(/dimID_x,dimID_y/),latbdyf_ID) ; call erreur(status,.TRUE.,"def_var_latbdyf_ID")
    !-
    status = NF90_DEF_VAR(fidOBC,"ziWt", NF90_INT  ,(/dimID_x,dimID_y/),ziWt_ID)    ; call erreur(status,.TRUE.,"def_var_ziWt_ID")
    status = NF90_DEF_VAR(fidOBC,"zjWt", NF90_INT  ,(/dimID_x,dimID_y/),zjWt_ID)    ; call erreur(status,.TRUE.,"def_var_zjWt_ID")
    status = NF90_DEF_VAR(fidOBC,"ziCt", NF90_INT  ,(/dimID_x,dimID_y/),ziCt_ID)    ; call erreur(status,.TRUE.,"def_var_ziCt_ID")
    status = NF90_DEF_VAR(fidOBC,"zjCt", NF90_INT  ,(/dimID_x,dimID_y/),zjCt_ID)    ; call erreur(status,.TRUE.,"def_var_zjCt_ID")
    status = NF90_DEF_VAR(fidOBC,"ziEt", NF90_INT  ,(/dimID_x,dimID_y/),ziEt_ID)    ; call erreur(status,.TRUE.,"def_var_ziEt_ID")
    status = NF90_DEF_VAR(fidOBC,"zjEt", NF90_INT  ,(/dimID_x,dimID_y/),zjEt_ID)    ; call erreur(status,.TRUE.,"def_var_zjEt_ID")
    status = NF90_DEF_VAR(fidOBC,"ziSt", NF90_INT  ,(/dimID_x,dimID_y/),ziSt_ID)    ; call erreur(status,.TRUE.,"def_var_ziSt_ID")
    status = NF90_DEF_VAR(fidOBC,"zjSt", NF90_INT  ,(/dimID_x,dimID_y/),zjSt_ID)    ; call erreur(status,.TRUE.,"def_var_zjSt_ID")
    status = NF90_DEF_VAR(fidOBC,"ziMt", NF90_INT  ,(/dimID_x,dimID_y/),ziMt_ID)    ; call erreur(status,.TRUE.,"def_var_ziMt_ID")
    status = NF90_DEF_VAR(fidOBC,"zjMt", NF90_INT  ,(/dimID_x,dimID_y/),zjMt_ID)    ; call erreur(status,.TRUE.,"def_var_zjMt_ID")
    status = NF90_DEF_VAR(fidOBC,"ziNt", NF90_INT  ,(/dimID_x,dimID_y/),ziNt_ID)    ; call erreur(status,.TRUE.,"def_var_ziNt_ID")
    status = NF90_DEF_VAR(fidOBC,"zjNt", NF90_INT  ,(/dimID_x,dimID_y/),zjNt_ID)    ; call erreur(status,.TRUE.,"def_var_zjNt_ID")
    status = NF90_DEF_VAR(fidOBC,"angXt",NF90_FLOAT,(/dimID_x,dimID_y/),angleXt_ID) ; call erreur(status,.TRUE.,"def_var_angleXt_ID")
    status = NF90_DEF_VAR(fidOBC,"angYt",NF90_FLOAT,(/dimID_x,dimID_y/),angleYt_ID) ; call erreur(status,.TRUE.,"def_var_angleYt_ID")
    status = NF90_DEF_VAR(fidOBC,"wgWt", NF90_FLOAT,(/dimID_x,dimID_y/),wgWt_ID)    ; call erreur(status,.TRUE.,"def_var_wgWt_ID")
    status = NF90_DEF_VAR(fidOBC,"wgCt", NF90_FLOAT,(/dimID_x,dimID_y/),wgCt_ID)    ; call erreur(status,.TRUE.,"def_var_wgCt_ID")
    status = NF90_DEF_VAR(fidOBC,"wgEt", NF90_FLOAT,(/dimID_x,dimID_y/),wgEt_ID)    ; call erreur(status,.TRUE.,"def_var_wgEt_ID")
    status = NF90_DEF_VAR(fidOBC,"wgSt", NF90_FLOAT,(/dimID_x,dimID_y/),wgSt_ID)    ; call erreur(status,.TRUE.,"def_var_wgSt_ID")
    status = NF90_DEF_VAR(fidOBC,"wgMt", NF90_FLOAT,(/dimID_x,dimID_y/),wgMt_ID)    ; call erreur(status,.TRUE.,"def_var_wgMt_ID")
    status = NF90_DEF_VAR(fidOBC,"wgNt", NF90_FLOAT,(/dimID_x,dimID_y/),wgNt_ID)    ; call erreur(status,.TRUE.,"def_var_wgNt_ID")
    !-
    status = NF90_DEF_VAR(fidOBC,"ziWu", NF90_INT  ,(/dimID_x,dimID_y/),ziWu_ID)    ; call erreur(status,.TRUE.,"def_var_ziWu_ID")
    status = NF90_DEF_VAR(fidOBC,"zjWu", NF90_INT  ,(/dimID_x,dimID_y/),zjWu_ID)    ; call erreur(status,.TRUE.,"def_var_zjWu_ID")
    status = NF90_DEF_VAR(fidOBC,"ziCu", NF90_INT  ,(/dimID_x,dimID_y/),ziCu_ID)    ; call erreur(status,.TRUE.,"def_var_ziCu_ID")
    status = NF90_DEF_VAR(fidOBC,"zjCu", NF90_INT  ,(/dimID_x,dimID_y/),zjCu_ID)    ; call erreur(status,.TRUE.,"def_var_zjCu_ID")
    status = NF90_DEF_VAR(fidOBC,"ziEu", NF90_INT  ,(/dimID_x,dimID_y/),ziEu_ID)    ; call erreur(status,.TRUE.,"def_var_ziEu_ID")
    status = NF90_DEF_VAR(fidOBC,"zjEu", NF90_INT  ,(/dimID_x,dimID_y/),zjEu_ID)    ; call erreur(status,.TRUE.,"def_var_zjEu_ID")
    status = NF90_DEF_VAR(fidOBC,"ziSu", NF90_INT  ,(/dimID_x,dimID_y/),ziSu_ID)    ; call erreur(status,.TRUE.,"def_var_ziSu_ID")
    status = NF90_DEF_VAR(fidOBC,"zjSu", NF90_INT  ,(/dimID_x,dimID_y/),zjSu_ID)    ; call erreur(status,.TRUE.,"def_var_zjSu_ID")
    status = NF90_DEF_VAR(fidOBC,"ziMu", NF90_INT  ,(/dimID_x,dimID_y/),ziMu_ID)    ; call erreur(status,.TRUE.,"def_var_ziMu_ID")
    status = NF90_DEF_VAR(fidOBC,"zjMu", NF90_INT  ,(/dimID_x,dimID_y/),zjMu_ID)    ; call erreur(status,.TRUE.,"def_var_zjMu_ID")
    status = NF90_DEF_VAR(fidOBC,"ziNu", NF90_INT  ,(/dimID_x,dimID_y/),ziNu_ID)    ; call erreur(status,.TRUE.,"def_var_ziNu_ID")
    status = NF90_DEF_VAR(fidOBC,"zjNu", NF90_INT  ,(/dimID_x,dimID_y/),zjNu_ID)    ; call erreur(status,.TRUE.,"def_var_zjNu_ID")
    status = NF90_DEF_VAR(fidOBC,"angXu",NF90_FLOAT,(/dimID_x,dimID_y/),angleXu_ID) ; call erreur(status,.TRUE.,"def_var_angleXu_ID")
    status = NF90_DEF_VAR(fidOBC,"angYu",NF90_FLOAT,(/dimID_x,dimID_y/),angleYu_ID) ; call erreur(status,.TRUE.,"def_var_angleYu_ID")
    status = NF90_DEF_VAR(fidOBC,"wgWu", NF90_FLOAT,(/dimID_x,dimID_y/),wgWu_ID)    ; call erreur(status,.TRUE.,"def_var_wgWu_ID")
    status = NF90_DEF_VAR(fidOBC,"wgCu", NF90_FLOAT,(/dimID_x,dimID_y/),wgCu_ID)    ; call erreur(status,.TRUE.,"def_var_wgCu_ID")
    status = NF90_DEF_VAR(fidOBC,"wgEu", NF90_FLOAT,(/dimID_x,dimID_y/),wgEu_ID)    ; call erreur(status,.TRUE.,"def_var_wgEu_ID")
    status = NF90_DEF_VAR(fidOBC,"wgSu", NF90_FLOAT,(/dimID_x,dimID_y/),wgSu_ID)    ; call erreur(status,.TRUE.,"def_var_wgSu_ID")
    status = NF90_DEF_VAR(fidOBC,"wgMu", NF90_FLOAT,(/dimID_x,dimID_y/),wgMu_ID)    ; call erreur(status,.TRUE.,"def_var_wgMu_ID")
    status = NF90_DEF_VAR(fidOBC,"wgNu", NF90_FLOAT,(/dimID_x,dimID_y/),wgNu_ID)    ; call erreur(status,.TRUE.,"def_var_wgNu_ID")
    !-
    status = NF90_DEF_VAR(fidOBC,"ziWv", NF90_INT  ,(/dimID_x,dimID_y/),ziWv_ID)    ; call erreur(status,.TRUE.,"def_var_ziWv_ID")
    status = NF90_DEF_VAR(fidOBC,"zjWv", NF90_INT  ,(/dimID_x,dimID_y/),zjWv_ID)    ; call erreur(status,.TRUE.,"def_var_zjWv_ID")
    status = NF90_DEF_VAR(fidOBC,"ziCv", NF90_INT  ,(/dimID_x,dimID_y/),ziCv_ID)    ; call erreur(status,.TRUE.,"def_var_ziCv_ID")
    status = NF90_DEF_VAR(fidOBC,"zjCv", NF90_INT  ,(/dimID_x,dimID_y/),zjCv_ID)    ; call erreur(status,.TRUE.,"def_var_zjCv_ID")
    status = NF90_DEF_VAR(fidOBC,"ziEv", NF90_INT  ,(/dimID_x,dimID_y/),ziEv_ID)    ; call erreur(status,.TRUE.,"def_var_ziEv_ID")
    status = NF90_DEF_VAR(fidOBC,"zjEv", NF90_INT  ,(/dimID_x,dimID_y/),zjEv_ID)    ; call erreur(status,.TRUE.,"def_var_zjEv_ID")
    status = NF90_DEF_VAR(fidOBC,"ziSv", NF90_INT  ,(/dimID_x,dimID_y/),ziSv_ID)    ; call erreur(status,.TRUE.,"def_var_ziSv_ID")
    status = NF90_DEF_VAR(fidOBC,"zjSv", NF90_INT  ,(/dimID_x,dimID_y/),zjSv_ID)    ; call erreur(status,.TRUE.,"def_var_zjSv_ID")
    status = NF90_DEF_VAR(fidOBC,"ziMv", NF90_INT  ,(/dimID_x,dimID_y/),ziMv_ID)    ; call erreur(status,.TRUE.,"def_var_ziMv_ID")
    status = NF90_DEF_VAR(fidOBC,"zjMv", NF90_INT  ,(/dimID_x,dimID_y/),zjMv_ID)    ; call erreur(status,.TRUE.,"def_var_zjMv_ID")
    status = NF90_DEF_VAR(fidOBC,"ziNv", NF90_INT  ,(/dimID_x,dimID_y/),ziNv_ID)    ; call erreur(status,.TRUE.,"def_var_ziNv_ID")
    status = NF90_DEF_VAR(fidOBC,"zjNv", NF90_INT  ,(/dimID_x,dimID_y/),zjNv_ID)    ; call erreur(status,.TRUE.,"def_var_zjNv_ID")
    status = NF90_DEF_VAR(fidOBC,"angXv",NF90_FLOAT,(/dimID_x,dimID_y/),angleXv_ID) ; call erreur(status,.TRUE.,"def_var_angleXv_ID")
    status = NF90_DEF_VAR(fidOBC,"angYv",NF90_FLOAT,(/dimID_x,dimID_y/),angleYv_ID) ; call erreur(status,.TRUE.,"def_var_angleYv_ID")
    status = NF90_DEF_VAR(fidOBC,"wgWv", NF90_FLOAT,(/dimID_x,dimID_y/),wgWv_ID)    ; call erreur(status,.TRUE.,"def_var_wgWv_ID")
    status = NF90_DEF_VAR(fidOBC,"wgCv", NF90_FLOAT,(/dimID_x,dimID_y/),wgCv_ID)    ; call erreur(status,.TRUE.,"def_var_wgCv_ID")
    status = NF90_DEF_VAR(fidOBC,"wgEv", NF90_FLOAT,(/dimID_x,dimID_y/),wgEv_ID)    ; call erreur(status,.TRUE.,"def_var_wgEv_ID")
    status = NF90_DEF_VAR(fidOBC,"wgSv", NF90_FLOAT,(/dimID_x,dimID_y/),wgSv_ID)    ; call erreur(status,.TRUE.,"def_var_wgSv_ID")
    status = NF90_DEF_VAR(fidOBC,"wgMv", NF90_FLOAT,(/dimID_x,dimID_y/),wgMv_ID)    ; call erreur(status,.TRUE.,"def_var_wgMv_ID")
    status = NF90_DEF_VAR(fidOBC,"wgNv", NF90_FLOAT,(/dimID_x,dimID_y/),wgNv_ID)    ; call erreur(status,.TRUE.,"def_var_wgNv_ID")
    !-
    status = NF90_DEF_VAR(fidOBC,"ziWf", NF90_INT  ,(/dimID_xf,dimID_yf/),ziWf_ID)  ; call erreur(status,.TRUE.,"def_var_ziWf_ID")
    status = NF90_DEF_VAR(fidOBC,"zjWf", NF90_INT  ,(/dimID_xf,dimID_yf/),zjWf_ID)  ; call erreur(status,.TRUE.,"def_var_zjWf_ID")
    status = NF90_DEF_VAR(fidOBC,"ziCf", NF90_INT  ,(/dimID_xf,dimID_yf/),ziCf_ID)  ; call erreur(status,.TRUE.,"def_var_ziCf_ID")
    status = NF90_DEF_VAR(fidOBC,"zjCf", NF90_INT  ,(/dimID_xf,dimID_yf/),zjCf_ID)  ; call erreur(status,.TRUE.,"def_var_zjCf_ID")
    status = NF90_DEF_VAR(fidOBC,"ziEf", NF90_INT  ,(/dimID_xf,dimID_yf/),ziEf_ID)  ; call erreur(status,.TRUE.,"def_var_ziEf_ID")
    status = NF90_DEF_VAR(fidOBC,"zjEf", NF90_INT  ,(/dimID_xf,dimID_yf/),zjEf_ID)  ; call erreur(status,.TRUE.,"def_var_zjEf_ID")
    status = NF90_DEF_VAR(fidOBC,"ziSf", NF90_INT  ,(/dimID_xf,dimID_yf/),ziSf_ID)  ; call erreur(status,.TRUE.,"def_var_ziSf_ID")
    status = NF90_DEF_VAR(fidOBC,"zjSf", NF90_INT  ,(/dimID_xf,dimID_yf/),zjSf_ID)  ; call erreur(status,.TRUE.,"def_var_zjSf_ID")
    status = NF90_DEF_VAR(fidOBC,"ziMf", NF90_INT  ,(/dimID_xf,dimID_yf/),ziMf_ID)  ; call erreur(status,.TRUE.,"def_var_ziMf_ID")
    status = NF90_DEF_VAR(fidOBC,"zjMf", NF90_INT  ,(/dimID_xf,dimID_yf/),zjMf_ID)  ; call erreur(status,.TRUE.,"def_var_zjMf_ID")
    status = NF90_DEF_VAR(fidOBC,"ziNf", NF90_INT  ,(/dimID_xf,dimID_yf/),ziNf_ID)  ; call erreur(status,.TRUE.,"def_var_ziNf_ID")
    status = NF90_DEF_VAR(fidOBC,"zjNf", NF90_INT  ,(/dimID_xf,dimID_yf/),zjNf_ID)  ; call erreur(status,.TRUE.,"def_var_zjNf_ID")
    status = NF90_DEF_VAR(fidOBC,"wgWf", NF90_FLOAT,(/dimID_xf,dimID_yf/),wgWf_ID)  ; call erreur(status,.TRUE.,"def_var_wgWf_ID")
    status = NF90_DEF_VAR(fidOBC,"wgCf", NF90_FLOAT,(/dimID_xf,dimID_yf/),wgCf_ID)  ; call erreur(status,.TRUE.,"def_var_wgCf_ID")
    status = NF90_DEF_VAR(fidOBC,"wgEf", NF90_FLOAT,(/dimID_xf,dimID_yf/),wgEf_ID)  ; call erreur(status,.TRUE.,"def_var_wgEf_ID")
    status = NF90_DEF_VAR(fidOBC,"wgSf", NF90_FLOAT,(/dimID_xf,dimID_yf/),wgSf_ID)  ; call erreur(status,.TRUE.,"def_var_wgSf_ID")
    status = NF90_DEF_VAR(fidOBC,"wgMf", NF90_FLOAT,(/dimID_xf,dimID_yf/),wgMf_ID)  ; call erreur(status,.TRUE.,"def_var_wgMf_ID")
    status = NF90_DEF_VAR(fidOBC,"wgNf", NF90_FLOAT,(/dimID_xf,dimID_yf/),wgNf_ID)  ; call erreur(status,.TRUE.,"def_var_wgNf_ID")
    !-
    status = NF90_DEF_VAR(fidOBC,"tmask",NF90_SHORT,(/dimID_x,dimID_y,dimID_z/),tmask_ID); call erreur(status,.TRUE.,"def_var_tmask_ID")
    status = NF90_DEF_VAR(fidOBC,"umask",NF90_SHORT,(/dimID_x,dimID_y,dimID_z/),umask_ID); call erreur(status,.TRUE.,"def_var_umask_ID")
    status = NF90_DEF_VAR(fidOBC,"vmask",NF90_SHORT,(/dimID_x,dimID_y,dimID_z/),vmask_ID); call erreur(status,.TRUE.,"def_var_vmask_ID")
    !-
    status = NF90_DEF_VAR(fidOBC,"e3u", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),e3ubdy_ID); call erreur(status,.TRUE.,"def_var_e3ubdy_ID")
    status = NF90_DEF_VAR(fidOBC,"e3v", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),e3vbdy_ID); call erreur(status,.TRUE.,"def_var_e3vbdy_ID")
    status = NF90_DEF_VAR(fidOBC,"e2u", NF90_FLOAT,(/dimID_x,dimID_y/),e2ubdy_ID)        ; call erreur(status,.TRUE.,"def_var_e2ubdy_ID")
    status = NF90_DEF_VAR(fidOBC,"e1v", NF90_FLOAT,(/dimID_x,dimID_y/),e1vbdy_ID)        ; call erreur(status,.TRUE.,"def_var_e1vbdy_ID")

    !-- Global attribute
    status = NF90_PUT_ATT(fidOBC,NF90_GLOBAL,"history","Created using build_NEMO_MC25_OBC_intrp_coeff.f90") ; call erreur(status,.TRUE.,"att_global")

    !-- End of definitions
    status = NF90_ENDDEF(fidOBC) ; call erreur(status,.TRUE.,"end_definition") 

    !-- Values to put in each variable 
    status = NF90_PUT_VAR(fidOBC,lonbdyt_ID,lonbdyt)   ; call erreur(status,.TRUE.,"var_lonbdyt_ID")
    status = NF90_PUT_VAR(fidOBC,lonbdyu_ID,lonbdyu)   ; call erreur(status,.TRUE.,"var_lonbdyu_ID")
    status = NF90_PUT_VAR(fidOBC,lonbdyv_ID,lonbdyv)   ; call erreur(status,.TRUE.,"var_lonbdyv_ID")
    status = NF90_PUT_VAR(fidOBC,latbdyt_ID,latbdyt)   ; call erreur(status,.TRUE.,"var_latbdyt_ID")
    status = NF90_PUT_VAR(fidOBC,latbdyu_ID,latbdyu)   ; call erreur(status,.TRUE.,"var_latbdyu_ID")
    status = NF90_PUT_VAR(fidOBC,latbdyv_ID,latbdyv)   ; call erreur(status,.TRUE.,"var_latbdyv_ID")
    !-
    status = NF90_PUT_VAR(fidOBC,ziWt_ID,ziWt)         ; call erreur(status,.TRUE.,"var_ziWt_ID")
    status = NF90_PUT_VAR(fidOBC,zjWt_ID,zjWt)         ; call erreur(status,.TRUE.,"var_zjWt_ID")
    status = NF90_PUT_VAR(fidOBC,ziCt_ID,ziCt)         ; call erreur(status,.TRUE.,"var_ziCt_ID")
    status = NF90_PUT_VAR(fidOBC,zjCt_ID,zjCt)         ; call erreur(status,.TRUE.,"var_zjCt_ID")
    status = NF90_PUT_VAR(fidOBC,ziEt_ID,ziEt)         ; call erreur(status,.TRUE.,"var_ziEt_ID")
    status = NF90_PUT_VAR(fidOBC,zjEt_ID,zjEt)         ; call erreur(status,.TRUE.,"var_zjEt_ID")
    status = NF90_PUT_VAR(fidOBC,ziSt_ID,ziSt)         ; call erreur(status,.TRUE.,"var_ziSt_ID")
    status = NF90_PUT_VAR(fidOBC,zjSt_ID,zjSt)         ; call erreur(status,.TRUE.,"var_zjSt_ID")
    status = NF90_PUT_VAR(fidOBC,ziMt_ID,ziMt)         ; call erreur(status,.TRUE.,"var_ziMt_ID")
    status = NF90_PUT_VAR(fidOBC,zjMt_ID,zjMt)         ; call erreur(status,.TRUE.,"var_zjMt_ID")
    status = NF90_PUT_VAR(fidOBC,ziNt_ID,ziNt)         ; call erreur(status,.TRUE.,"var_ziNt_ID")
    status = NF90_PUT_VAR(fidOBC,zjNt_ID,zjNt)         ; call erreur(status,.TRUE.,"var_zjNt_ID")
    status = NF90_PUT_VAR(fidOBC,wgWt_ID,wgWt)         ; call erreur(status,.TRUE.,"var_wgWt_ID")
    status = NF90_PUT_VAR(fidOBC,wgCt_ID,wgCt)         ; call erreur(status,.TRUE.,"var_wgCt_ID")
    status = NF90_PUT_VAR(fidOBC,wgEt_ID,wgEt)         ; call erreur(status,.TRUE.,"var_wgEt_ID")
    status = NF90_PUT_VAR(fidOBC,wgSt_ID,wgSt)         ; call erreur(status,.TRUE.,"var_wgSt_ID")
    status = NF90_PUT_VAR(fidOBC,wgMt_ID,wgMt)         ; call erreur(status,.TRUE.,"var_wgMt_ID")
    status = NF90_PUT_VAR(fidOBC,wgNt_ID,wgNt)         ; call erreur(status,.TRUE.,"var_wgNt_ID")
    status = NF90_PUT_VAR(fidOBC,angleXt_ID,angleXt)   ; call erreur(status,.TRUE.,"var_angleXt_ID")
    status = NF90_PUT_VAR(fidOBC,angleYt_ID,angleYt)   ; call erreur(status,.TRUE.,"var_angleYt_ID")
    !-
    status = NF90_PUT_VAR(fidOBC,ziWu_ID,ziWu)         ; call erreur(status,.TRUE.,"var_ziWu_ID")
    status = NF90_PUT_VAR(fidOBC,zjWu_ID,zjWu)         ; call erreur(status,.TRUE.,"var_zjWu_ID")
    status = NF90_PUT_VAR(fidOBC,ziCu_ID,ziCu)         ; call erreur(status,.TRUE.,"var_ziCu_ID")
    status = NF90_PUT_VAR(fidOBC,zjCu_ID,zjCu)         ; call erreur(status,.TRUE.,"var_zjCu_ID")
    status = NF90_PUT_VAR(fidOBC,ziEu_ID,ziEu)         ; call erreur(status,.TRUE.,"var_ziEu_ID")
    status = NF90_PUT_VAR(fidOBC,zjEu_ID,zjEu)         ; call erreur(status,.TRUE.,"var_zjEu_ID")
    status = NF90_PUT_VAR(fidOBC,ziSu_ID,ziSu)         ; call erreur(status,.TRUE.,"var_ziSu_ID")
    status = NF90_PUT_VAR(fidOBC,zjSu_ID,zjSu)         ; call erreur(status,.TRUE.,"var_zjSu_ID")
    status = NF90_PUT_VAR(fidOBC,ziMu_ID,ziMu)         ; call erreur(status,.TRUE.,"var_ziMu_ID")
    status = NF90_PUT_VAR(fidOBC,zjMu_ID,zjMu)         ; call erreur(status,.TRUE.,"var_zjMu_ID")
    status = NF90_PUT_VAR(fidOBC,ziNu_ID,ziNu)         ; call erreur(status,.TRUE.,"var_ziNu_ID")
    status = NF90_PUT_VAR(fidOBC,zjNu_ID,zjNu)         ; call erreur(status,.TRUE.,"var_zjNu_ID")
    status = NF90_PUT_VAR(fidOBC,wgWu_ID,wgWu)         ; call erreur(status,.TRUE.,"var_wgWu_ID")
    status = NF90_PUT_VAR(fidOBC,wgCu_ID,wgCu)         ; call erreur(status,.TRUE.,"var_wgCu_ID")
    status = NF90_PUT_VAR(fidOBC,wgEu_ID,wgEu)         ; call erreur(status,.TRUE.,"var_wgEu_ID")
    status = NF90_PUT_VAR(fidOBC,wgSu_ID,wgSu)         ; call erreur(status,.TRUE.,"var_wgSu_ID")
    status = NF90_PUT_VAR(fidOBC,wgMu_ID,wgMu)         ; call erreur(status,.TRUE.,"var_wgMu_ID")
    status = NF90_PUT_VAR(fidOBC,wgNu_ID,wgNu)         ; call erreur(status,.TRUE.,"var_wgNu_ID")
    status = NF90_PUT_VAR(fidOBC,angleXu_ID,angleXu)   ; call erreur(status,.TRUE.,"var_angleXu_ID")
    status = NF90_PUT_VAR(fidOBC,angleYu_ID,angleYu)   ; call erreur(status,.TRUE.,"var_angleYu_ID")
    !-
    status = NF90_PUT_VAR(fidOBC,ziWv_ID,ziWv)         ; call erreur(status,.TRUE.,"var_ziWv_ID")
    status = NF90_PUT_VAR(fidOBC,zjWv_ID,zjWv)         ; call erreur(status,.TRUE.,"var_zjWv_ID")
    status = NF90_PUT_VAR(fidOBC,ziCv_ID,ziCv)         ; call erreur(status,.TRUE.,"var_ziCv_ID")
    status = NF90_PUT_VAR(fidOBC,zjCv_ID,zjCv)         ; call erreur(status,.TRUE.,"var_zjCv_ID")
    status = NF90_PUT_VAR(fidOBC,ziEv_ID,ziEv)         ; call erreur(status,.TRUE.,"var_ziEv_ID")
    status = NF90_PUT_VAR(fidOBC,zjEv_ID,zjEv)         ; call erreur(status,.TRUE.,"var_zjEv_ID")
    status = NF90_PUT_VAR(fidOBC,ziSv_ID,ziSv)         ; call erreur(status,.TRUE.,"var_ziSv_ID")
    status = NF90_PUT_VAR(fidOBC,zjSv_ID,zjSv)         ; call erreur(status,.TRUE.,"var_zjSv_ID")
    status = NF90_PUT_VAR(fidOBC,ziMv_ID,ziMv)         ; call erreur(status,.TRUE.,"var_ziMv_ID")
    status = NF90_PUT_VAR(fidOBC,zjMv_ID,zjMv)         ; call erreur(status,.TRUE.,"var_zjMv_ID")
    status = NF90_PUT_VAR(fidOBC,ziNv_ID,ziNv)         ; call erreur(status,.TRUE.,"var_ziNv_ID")
    status = NF90_PUT_VAR(fidOBC,zjNv_ID,zjNv)         ; call erreur(status,.TRUE.,"var_zjNv_ID")
    status = NF90_PUT_VAR(fidOBC,wgWv_ID,wgWv)         ; call erreur(status,.TRUE.,"var_wgWv_ID")
    status = NF90_PUT_VAR(fidOBC,wgCv_ID,wgCv)         ; call erreur(status,.TRUE.,"var_wgCv_ID")
    status = NF90_PUT_VAR(fidOBC,wgEv_ID,wgEv)         ; call erreur(status,.TRUE.,"var_wgEv_ID")
    status = NF90_PUT_VAR(fidOBC,wgSv_ID,wgSv)         ; call erreur(status,.TRUE.,"var_wgSv_ID")
    status = NF90_PUT_VAR(fidOBC,wgMv_ID,wgMv)         ; call erreur(status,.TRUE.,"var_wgMv_ID")
    status = NF90_PUT_VAR(fidOBC,wgNv_ID,wgNv)         ; call erreur(status,.TRUE.,"var_wgNv_ID")
    status = NF90_PUT_VAR(fidOBC,angleXv_ID,angleXv)   ; call erreur(status,.TRUE.,"var_angleXv_ID")
    status = NF90_PUT_VAR(fidOBC,angleYv_ID,angleYv)   ; call erreur(status,.TRUE.,"var_angleYv_ID")
    !-
    status = NF90_PUT_VAR(fidOBC,ziWf_ID,ziWf)         ; call erreur(status,.TRUE.,"var_ziWf_ID")
    status = NF90_PUT_VAR(fidOBC,zjWf_ID,zjWf)         ; call erreur(status,.TRUE.,"var_zjWf_ID")
    status = NF90_PUT_VAR(fidOBC,ziCf_ID,ziCf)         ; call erreur(status,.TRUE.,"var_ziCf_ID")
    status = NF90_PUT_VAR(fidOBC,zjCf_ID,zjCf)         ; call erreur(status,.TRUE.,"var_zjCf_ID")
    status = NF90_PUT_VAR(fidOBC,ziEf_ID,ziEf)         ; call erreur(status,.TRUE.,"var_ziEf_ID")
    status = NF90_PUT_VAR(fidOBC,zjEf_ID,zjEf)         ; call erreur(status,.TRUE.,"var_zjEf_ID")
    status = NF90_PUT_VAR(fidOBC,ziSf_ID,ziSf)         ; call erreur(status,.TRUE.,"var_ziSf_ID")
    status = NF90_PUT_VAR(fidOBC,zjSf_ID,zjSf)         ; call erreur(status,.TRUE.,"var_zjSf_ID")
    status = NF90_PUT_VAR(fidOBC,ziMf_ID,ziMf)         ; call erreur(status,.TRUE.,"var_ziMf_ID")
    status = NF90_PUT_VAR(fidOBC,zjMf_ID,zjMf)         ; call erreur(status,.TRUE.,"var_zjMf_ID")
    status = NF90_PUT_VAR(fidOBC,ziNf_ID,ziNf)         ; call erreur(status,.TRUE.,"var_ziNf_ID")
    status = NF90_PUT_VAR(fidOBC,zjNf_ID,zjNf)         ; call erreur(status,.TRUE.,"var_zjNf_ID")
    status = NF90_PUT_VAR(fidOBC,wgWf_ID,wgWf)         ; call erreur(status,.TRUE.,"var_wgWf_ID")
    status = NF90_PUT_VAR(fidOBC,wgCf_ID,wgCf)         ; call erreur(status,.TRUE.,"var_wgCf_ID")
    status = NF90_PUT_VAR(fidOBC,wgEf_ID,wgEf)         ; call erreur(status,.TRUE.,"var_wgEf_ID")
    status = NF90_PUT_VAR(fidOBC,wgSf_ID,wgSf)         ; call erreur(status,.TRUE.,"var_wgSf_ID")
    status = NF90_PUT_VAR(fidOBC,wgMf_ID,wgMf)         ; call erreur(status,.TRUE.,"var_wgMf_ID")
    status = NF90_PUT_VAR(fidOBC,wgNf_ID,wgNf)         ; call erreur(status,.TRUE.,"var_wgNf_ID")
    !-
    status = NF90_PUT_VAR(fidOBC,tmask_ID,tmskbdy)     ; call erreur(status,.TRUE.,"var_tmask_ID")
    status = NF90_PUT_VAR(fidOBC,umask_ID,umskbdy)     ; call erreur(status,.TRUE.,"var_umask_ID")
    status = NF90_PUT_VAR(fidOBC,vmask_ID,vmskbdy)     ; call erreur(status,.TRUE.,"var_vmask_ID")
    !-
    status = NF90_PUT_VAR(fidOBC,e3ubdy_ID,e3ubdy)     ; call erreur(status,.TRUE.,"var_e3ubdy_ID")
    status = NF90_PUT_VAR(fidOBC,e3vbdy_ID,e3vbdy)     ; call erreur(status,.TRUE.,"var_e3vbdy_ID")
    status = NF90_PUT_VAR(fidOBC,e2ubdy_ID,e2ubdy)     ; call erreur(status,.TRUE.,"var_e2ubdy_ID")
    status = NF90_PUT_VAR(fidOBC,e1vbdy_ID,e1vbdy)     ; call erreur(status,.TRUE.,"var_e1vbdy_ID")

    !-- End of writing                           
    status = NF90_CLOSE(fidOBC) ; call erreur(status,.TRUE.,"end_writing_OBC")

    DEALLOCATE( lonbdyt, latbdyt, lonbdyu, latbdyu, lonbdyv, latbdyv, lonbdyf, latbdyf         )
    DEALLOCATE( zlonbdyt, zlonbdyu, zlonbdyv                        , zlonbdyf                 )
    DEALLOCATE( e1tbdy, e2tbdy, e1ubdy, e2ubdy, e1vbdy, e2vbdy      , e1fbdy, e2fbdy           )
    DEALLOCATE( angbdyXt, angbdyYt, angbdyXu, angbdyYu, angbdyXv, angbdyYv, angbdyXf, angbdyYf )
    DEALLOCATE( angleXt, angleYt, angleXu, angleYu, angleXv, angleYv, angleXf, angleYf         )
    DEALLOCATE( tmskbdy, umskbdy, vmskbdy, e3tbdy, e3ubdy, e3vbdy                              )
    DEALLOCATE( gdeptbdy, gdepubdy, gdepvbdy, e3t_0bdy, gdept_0bdy                             )
    !== T-points ==
    DEALLOCATE( ziWt, zjWt, wgWt, ziCt, zjCt, wgCt, ziEt, zjEt, wgEt )
    DEALLOCATE( ziSt, zjSt, wgSt, ziMt, zjMt, wgMt, ziNt, zjNt, wgNt )
    DEALLOCATE( zzit, zzjt, zzitm1, zzjtm1, zzitp1, zzjtp1  )
    !== U-points ==
    DEALLOCATE( ziWu, zjWu, wgWu, ziCu, zjCu, wgCu, ziEu, zjEu, wgEu )
    DEALLOCATE( ziSu, zjSu, wgSu, ziMu, zjMu, wgMu, ziNu, zjNu, wgNu )
    DEALLOCATE( zziu, zzju, zzium1, zzjum1, zziup1, zzjup1  )
    !== V-points ==
    DEALLOCATE( ziWv, zjWv, wgWv, ziCv, zjCv, wgCv, ziEv, zjEv, wgEv )
    DEALLOCATE( ziSv, zjSv, wgSv, ziMv, zjMv, wgMv, ziNv, zjNv, wgNv )
    DEALLOCATE( zziv, zzjv, zzivm1, zzjvm1, zzivp1, zzjvp1  )
    !== F-points ==
    DEALLOCATE( ziWf, zjWf, wgWf, ziCf, zjCf, wgCf, ziEf, zjEf, wgEf )
    DEALLOCATE( ziSf, zjSf, wgSf, ziMf, zjMf, wgMf, ziNf, zjNf, wgNf )
    DEALLOCATE( zzif, zzjf, zzifm1, zzjfm1, zzifp1, zzjfp1  )

  ENDDO !!-- iside


end program buildobc


!==================================================================================


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
    IF ( lstop ) STOP
  ENDIF
  !
END SUBROUTINE erreur
