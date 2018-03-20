program buildobc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! by N. Jourdain, on 23-MAY-2013, at CCRC-UNSW, Sydney                    !
!                                                                         !
! build coefficients to inpterpole a global simulation on a regional grid !
! (needed to build the initial state of regional config)                  !
!                                                                         !
! 0- USER'S CHOOICES                                                      !
! 1- READ INPUT GRID (i.e. from global ocean model)                       !
! 2- READ OUTPUT GRID (i.e. grid of the regional oceanic configuration)   !
! 3- CALCULATE WEIGHTS FOR INTERPOLATION (surrounding pts + weights)      !
! 4- WRITE EVERYTHING NEEDED FOR INTERPOLATION IN A NETCDF FILE           !
!                                                                         !
! > creates one netcdf file with coeff for interp                         !
!                                                                         !
! WARNING : for the moment, only work :                                   !
!           - if the vertical grid in global input domain is the same as  !
!             in the regional output domain                               !
!           - if the regional grid is finer or no more than 3-4 times     !
!             coarser than the global ocean grid                          !
!                                                                         !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

USE netcdf

IMPLICIT NONE

!-- namelist parameters :
namelist /global/ orcadir,filemskin,filezgrin,filehgrin
namelist /parent/ conf_par, perio, max_dom, feedback, idateline, file_par_coord, file_eff_land
namelist /child/ conf_child, file_child_coord, file_child_extend
INTEGER                               :: max_dom, feedback, perio, idateline
CHARACTER(LEN=150)                    :: file_par_coord, file_eff_land
CHARACTER(LEN=50)                     :: conf_par
CHARACTER(LEN=150)                    :: orcadir, filemskin, filezgrin, filehgrin
CHARACTER(LEN=50)                     :: conf_child
CHARACTER(LEN=150)                    :: file_child_coord, file_child_extend

!--
INTEGER                                   :: status, yeari, yearf, yr, iside, nside, ji, jj, jk
REAL*4                                    :: ra, rad

!-- input horizontal grid:
INTEGER                                  :: fidhgr, dimID_y, dimID_x, jpjin, jpiin, gphif_ID, gphiv_ID,                  &
&                                           gphiu_ID, gphit_ID, glamf_ID, glamv_ID, glamu_ID, glamt_ID, jiin, jjin, jkin,&
&                                           e1t_ID, e2t_ID, e1u_ID, e2u_ID, e1v_ID, e2v_ID, e1f_ID, e2f_ID
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: gphitin, glamtin, zglamtin, e1tin, e2tin, &
&                                           gphiuin, glamuin, zglamuin, e1uin, e2uin, & 
&                                           gphivin, glamvin, zglamvin, e1vin, e2vin
REAL*4                                   :: anginX, anginY

!-- output grid (mesh_mask file is read here and must have been created before) :
INTEGER                                  :: fidgridout, mtout, jpkout, jpjout, jpiout, jkout, jjout, jiout,             &
&                                           fmask_ID, dimID_z, tmask_ID, umask_ID, vmask_ID,  &
&                                           ffout_ID, e2fout_ID, e2vout_ID, e2uout_ID, e2tout_ID, e1fout_ID, e1vout_ID, &
&                                           e1uout_ID, e1tout_ID,  &
&                                           gphifout_ID, gphivout_ID, gphiuout_ID, gphitout_ID, glamfout_ID,            &
&                                           glamvout_ID, glamtout_ID, glamuout_ID 
CHARACTER(LEN=150)                       :: file_mesh_mask_child                     
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: gphitout, glamtout, zglamtout
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: gphiuout, glamuout, zglamuout
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: gphivout, glamvout, zglamvout
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: angoutXt, angoutYt, angleXt, angleYt
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: angoutXu, angoutYu, angleXu, angleYu
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: angoutXv, angoutYv, angleXv, angleYv
REAL*8,ALLOCATABLE,DIMENSION(:,:)        :: e1tout, e2tout, e1uout, e2uout, e1vout, e2vout     
INTEGER*1,ALLOCATABLE,DIMENSION(:,:,:)   :: tmaskout, umaskout, vmaskout   

!-- INTERPOLATION COEFFICIENTS
INTEGER                                  :: ziWt_ID, zjWt_ID, ziCt_ID, zjCt_ID, ziEt_ID, zjEt_ID, ziSt_ID, zjSt_ID,    &
&                                           ziMt_ID, zjMt_ID, ziNt_ID, zjNt_ID, zkUt_ID, wgWt_ID, wgCt_ID, wgEt_ID,    &
&                                           wgSt_ID, wgMt_ID, wgNt_ID, angleXt_ID, angleYt_ID, fidcoeff
INTEGER                                  :: ziWu_ID, zjWu_ID, ziCu_ID, zjCu_ID, ziEu_ID, zjEu_ID, ziSu_ID, zjSu_ID,    &
&                                           ziMu_ID, zjMu_ID, ziNu_ID, zjNu_ID, zkUu_ID, wgWu_ID, wgCu_ID, wgEu_ID,    &
&                                           wgSu_ID, wgMu_ID, wgNu_ID, angleXu_ID, angleYu_ID
INTEGER                                  :: ziWv_ID, zjWv_ID, ziCv_ID, zjCv_ID, ziEv_ID, zjEv_ID, ziSv_ID, zjSv_ID,    &
&                                           ziMv_ID, zjMv_ID, ziNv_ID, zjNv_ID, zkUv_ID, wgWv_ID, wgCv_ID, wgEv_ID,    &
&                                           wgSv_ID, wgMv_ID, wgNv_ID, angleXv_ID, angleYv_ID
REAL*4                                   :: dist, distmin
INTEGER,ALLOCATABLE,DIMENSION(:,:)       :: ziWt, zjWt, ziCt, zjCt, ziEt, zjEt, ziSt, zjSt, ziMt, zjMt, ziNt, zjNt,    &
&                                           zzit, zzjt, zzitm1, zzjtm1, zzitp1, zzjtp1
INTEGER,ALLOCATABLE,DIMENSION(:,:)       :: ziWu, zjWu, ziCu, zjCu, ziEu, zjEu, ziSu, zjSu, ziMu, zjMu, ziNu, zjNu,    &
&                                           zziu, zzju, zzium1, zzjum1, zziup1, zzjup1
INTEGER,ALLOCATABLE,DIMENSION(:,:)       :: ziWv, zjWv, ziCv, zjCv, ziEv, zjEv, ziSv, zjSv, ziMv, zjMv, ziNv, zjNv,    &
&                                           zziv, zzjv, zzivm1, zzjvm1, zzivp1, zzjvp1
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: wgWt, wgCt, wgEt, wgSt, wgMt, wgNt
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: wgWu, wgCu, wgEu, wgSu, wgMu, wgNu
REAL*4,ALLOCATABLE,DIMENSION(:,:)        :: wgWv, wgCv, wgEv, wgSv, wgMv, wgNv
CHARACTER(LEN=150)                       :: file_coeff

!==================================================================================
! 0- Initializations
!==================================================================================

!- read namelist values
OPEN (UNIT=1, FILE='namelist_precpl' )
READ (UNIT=1, NML=global)
READ (UNIT=1, NML=parent)
READ (UNIT=1, NML=child)
CLOSE(1)

!- earth radius (meter), must agree with NEMO file phycst.F90
ra = 6371229.0

!- deg to rad conversion (same as phycst.F90)
rad = 3.141592653589793 / 180.0

!-- name of file with coordinates of the output grid (i.e. regional grid, must be created before)
write(file_mesh_mask_child,385) TRIM(conf_par)
385 FORMAT('1_mesh_mask_',a,'.nc')

write(file_coeff,386) TRIM(conf_child)
386 FORMAT('coeff_3D_',a,'_from_ORCA025.nc')


  write(*,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
  write(*,*) ' build_child_3D_intrp_coeff_from_ORCA025 :           '
  write(*,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             '
  write(*,*) '                                                     '
  write(*,*) '  parent config : ', TRIM(conf_par)
  write(*,*) '  child  config : ', TRIM(conf_child)
  write(*,*) '                                                     '

!==================================================================================
! 1- READ INPUT GRID (i.e. from global ocean model)
!==================================================================================

!-- Read horizontal input grid (global)

write(*,*) ' '
write(*,*) ' Reading ', TRIM(filehgrin)

status = NF90_OPEN(TRIM(filehgrin),0,fidhgr)          
call erreur(status,.TRUE.,"read_horizontal_grid") 
                                       
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
ALLOCATE(  e1tin(jpiin,jpjin) , e2tin(jpiin,jpjin) )
ALLOCATE(  e1uin(jpiin,jpjin) , e2uin(jpiin,jpjin) )
ALLOCATE(  e1vin(jpiin,jpjin) , e2vin(jpiin,jpjin) )

!--
status = NF90_INQ_VARID(fidhgr,"e1t",e1t_ID) ; call erreur(status,.TRUE.,"inq_e1t_ID")
status = NF90_INQ_VARID(fidhgr,"e2t",e2t_ID) ; call erreur(status,.TRUE.,"inq_e2t_ID")
status = NF90_INQ_VARID(fidhgr,"e1u",e1u_ID) ; call erreur(status,.TRUE.,"inq_e1u_ID")
status = NF90_INQ_VARID(fidhgr,"e2u",e2u_ID) ; call erreur(status,.TRUE.,"inq_e2u_ID")
status = NF90_INQ_VARID(fidhgr,"e1v",e1v_ID) ; call erreur(status,.TRUE.,"inq_e1v_ID")
status = NF90_INQ_VARID(fidhgr,"e2v",e2v_ID) ; call erreur(status,.TRUE.,"inq_e2v_ID")

status = NF90_INQ_VARID(fidhgr,"glamt",glamt_ID) ; call erreur(status,.TRUE.,"inq_glamt_ID")
status = NF90_INQ_VARID(fidhgr,"gphit",gphit_ID) ; call erreur(status,.TRUE.,"inq_gphit_ID")
status = NF90_INQ_VARID(fidhgr,"glamu",glamu_ID) ; call erreur(status,.TRUE.,"inq_glamu_ID")
status = NF90_INQ_VARID(fidhgr,"gphiu",gphiu_ID) ; call erreur(status,.TRUE.,"inq_gphiu_ID")
status = NF90_INQ_VARID(fidhgr,"glamv",glamv_ID) ; call erreur(status,.TRUE.,"inq_glamv_ID")
status = NF90_INQ_VARID(fidhgr,"gphiv",gphiv_ID) ; call erreur(status,.TRUE.,"inq_gphiv_ID")

!--
status = NF90_GET_VAR(fidhgr,e1t_ID,e1tin) ; call erreur(status,.TRUE.,"getvar_e1t")
status = NF90_GET_VAR(fidhgr,e2t_ID,e2tin) ; call erreur(status,.TRUE.,"getvar_e2t")
status = NF90_GET_VAR(fidhgr,e1u_ID,e1uin) ; call erreur(status,.TRUE.,"getvar_e1u")
status = NF90_GET_VAR(fidhgr,e2u_ID,e2uin) ; call erreur(status,.TRUE.,"getvar_e2u")
status = NF90_GET_VAR(fidhgr,e1v_ID,e1vin) ; call erreur(status,.TRUE.,"getvar_e1v")
status = NF90_GET_VAR(fidhgr,e2v_ID,e2vin) ; call erreur(status,.TRUE.,"getvar_e2v")

status = NF90_GET_VAR(fidhgr,glamt_ID,glamtin) ; call erreur(status,.TRUE.,"getvar_glamt")
status = NF90_GET_VAR(fidhgr,gphit_ID,gphitin) ; call erreur(status,.TRUE.,"getvar_gphit")
status = NF90_GET_VAR(fidhgr,glamu_ID,glamuin) ; call erreur(status,.TRUE.,"getvar_glamu")
status = NF90_GET_VAR(fidhgr,gphiu_ID,gphiuin) ; call erreur(status,.TRUE.,"getvar_gphiu")
status = NF90_GET_VAR(fidhgr,glamv_ID,glamvin) ; call erreur(status,.TRUE.,"getvar_glamv")
status = NF90_GET_VAR(fidhgr,gphiv_ID,gphivin) ; call erreur(status,.TRUE.,"getvar_gphiv")                                          

status = NF90_CLOSE(fidhgr)                      
call erreur(status,.TRUE.,"fin_lecture_horizontal_grid") 

!=========================================================================================
! 2- READ OUTPUT GRID (i.e. grid of the regional oceanic configuration)
!=========================================================================================

write(*,*) ' '
write(*,*) ' Reading ', TRIM(file_mesh_mask_child)
                                             
status = NF90_OPEN(TRIM(file_mesh_mask_child),0,fidgridout)          
call erreur(status,.TRUE.,"read_output_grid") 
                                                     
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
 
 ALLOCATE(  tmaskout(jpiout,jpjout,jpkout) ) 
 ALLOCATE(  umaskout(jpiout,jpjout,jpkout) )
 ALLOCATE(  vmaskout(jpiout,jpjout,jpkout) )
 ALLOCATE(  e1tout(jpiout,jpjout) )
 ALLOCATE(  e1uout(jpiout,jpjout) )
 ALLOCATE(  e1vout(jpiout,jpjout) )
 ALLOCATE(  e2tout(jpiout,jpjout) )
 ALLOCATE(  e2uout(jpiout,jpjout) )
 ALLOCATE(  e2vout(jpiout,jpjout) )
 ALLOCATE(  gphitout(jpiout,jpjout)  )
 ALLOCATE(  glamtout(jpiout,jpjout)  )
 ALLOCATE(  zglamtout(jpiout,jpjout) )
 ALLOCATE(  gphiuout(jpiout,jpjout)  )
 ALLOCATE(  glamuout(jpiout,jpjout)  )
 ALLOCATE(  zglamuout(jpiout,jpjout) )
 ALLOCATE(  gphivout(jpiout,jpjout)  )
 ALLOCATE(  glamvout(jpiout,jpjout)  )
 ALLOCATE(  zglamvout(jpiout,jpjout) )
 ALLOCATE(  angoutXt(jpiout,jpjout) , angleXt(jpiout,jpjout) )
 ALLOCATE(  angoutYt(jpiout,jpjout) , angleYt(jpiout,jpjout) )
 ALLOCATE(  angoutXu(jpiout,jpjout) , angleXu(jpiout,jpjout) )
 ALLOCATE(  angoutYu(jpiout,jpjout) , angleYu(jpiout,jpjout) )
 ALLOCATE(  angoutXv(jpiout,jpjout) , angleXv(jpiout,jpjout) )
 ALLOCATE(  angoutYv(jpiout,jpjout) , angleYv(jpiout,jpjout) )

 status = NF90_INQ_VARID(fidgridout,"tmask",tmask_ID)        ; call erreur(status,.TRUE.,"inq_tmask_ID")
 status = NF90_INQ_VARID(fidgridout,"umask",umask_ID)        ; call erreur(status,.TRUE.,"inq_umask_ID")
 status = NF90_INQ_VARID(fidgridout,"vmask",vmask_ID)        ; call erreur(status,.TRUE.,"inq_vmask_ID")
 status = NF90_INQ_VARID(fidgridout,"e2t",e2tout_ID)         ; call erreur(status,.TRUE.,"inq_e2tout_ID")
 status = NF90_INQ_VARID(fidgridout,"e1t",e1tout_ID)         ; call erreur(status,.TRUE.,"inq_e1tout_ID")
 status = NF90_INQ_VARID(fidgridout,"e2u",e2uout_ID)         ; call erreur(status,.TRUE.,"inq_e2uout_ID")
 status = NF90_INQ_VARID(fidgridout,"e1u",e1uout_ID)         ; call erreur(status,.TRUE.,"inq_e1uout_ID")
 status = NF90_INQ_VARID(fidgridout,"e2v",e2vout_ID)         ; call erreur(status,.TRUE.,"inq_e2vout_ID")
 status = NF90_INQ_VARID(fidgridout,"e1v",e1vout_ID)         ; call erreur(status,.TRUE.,"inq_e1vout_ID")
 status = NF90_INQ_VARID(fidgridout,"gphit",gphitout_ID)     ; call erreur(status,.TRUE.,"inq_gphitout_ID")
 status = NF90_INQ_VARID(fidgridout,"glamt",glamtout_ID)     ; call erreur(status,.TRUE.,"inq_glamtout_ID")
 status = NF90_INQ_VARID(fidgridout,"gphiu",gphiuout_ID)     ; call erreur(status,.TRUE.,"inq_gphiuout_ID")
 status = NF90_INQ_VARID(fidgridout,"glamu",glamuout_ID)     ; call erreur(status,.TRUE.,"inq_glamuout_ID")
 status = NF90_INQ_VARID(fidgridout,"gphiv",gphivout_ID)     ; call erreur(status,.TRUE.,"inq_gphivout_ID")
 status = NF90_INQ_VARID(fidgridout,"glamv",glamvout_ID)     ; call erreur(status,.TRUE.,"inq_glamvout_ID")
                                                    
 status = NF90_GET_VAR(fidgridout,tmask_ID,tmaskout)        ; call erreur(status,.TRUE.,"getvar_tmaskout")
 status = NF90_GET_VAR(fidgridout,umask_ID,umaskout)        ; call erreur(status,.TRUE.,"getvar_umaskout")
 status = NF90_GET_VAR(fidgridout,vmask_ID,vmaskout)        ; call erreur(status,.TRUE.,"getvar_vmaskout")
 status = NF90_GET_VAR(fidgridout,e2tout_ID,e2tout)         ; call erreur(status,.TRUE.,"getvar_e2tout")
 status = NF90_GET_VAR(fidgridout,e1tout_ID,e1tout)         ; call erreur(status,.TRUE.,"getvar_e1tout")
 status = NF90_GET_VAR(fidgridout,e2uout_ID,e2uout)         ; call erreur(status,.TRUE.,"getvar_e2uout")
 status = NF90_GET_VAR(fidgridout,e1uout_ID,e1uout)         ; call erreur(status,.TRUE.,"getvar_e1uout")
 status = NF90_GET_VAR(fidgridout,e2vout_ID,e2vout)         ; call erreur(status,.TRUE.,"getvar_e2vout")
 status = NF90_GET_VAR(fidgridout,e1vout_ID,e1vout)         ; call erreur(status,.TRUE.,"getvar_e1vout")
 status = NF90_GET_VAR(fidgridout,gphitout_ID,gphitout)     ; call erreur(status,.TRUE.,"getvar_gphitout")
 status = NF90_GET_VAR(fidgridout,glamtout_ID,glamtout)     ; call erreur(status,.TRUE.,"getvar_glamtout")
 status = NF90_GET_VAR(fidgridout,gphiuout_ID,gphiuout)     ; call erreur(status,.TRUE.,"getvar_gphiuout")
 status = NF90_GET_VAR(fidgridout,glamuout_ID,glamuout)     ; call erreur(status,.TRUE.,"getvar_glamuout")
 status = NF90_GET_VAR(fidgridout,gphivout_ID,gphivout)     ; call erreur(status,.TRUE.,"getvar_gphivout")
 status = NF90_GET_VAR(fidgridout,glamvout_ID,glamvout)     ; call erreur(status,.TRUE.,"getvar_glamvout") 
                                           
status = NF90_CLOSE(fidgridout)                      
call erreur(status,.TRUE.,"fin_lecture_output_grid")     

!-- Rotation angles 
if ( idateline .eq. 1 ) then
  where ( glamtout(:,:) .lt. 0.0 )
    zglamtout(:,:) = 360.0 + glamtout(:,:)
  elsewhere
    zglamtout(:,:) = glamtout(:,:)
  endwhere
  !-
  where ( glamuout(:,:) .lt. 0.0 )
    zglamuout(:,:) = 360.0 + glamuout(:,:)
  elsewhere
    zglamuout(:,:) = glamuout(:,:)
  endwhere
  !-
  where ( glamvout(:,:) .lt. 0.0 )
    zglamvout(:,:) = 360.0 + glamvout(:,:)
  elsewhere
    zglamvout(:,:) = glamvout(:,:)
  endwhere
else
  zglamtout(:,:) = glamtout(:,:)
  zglamuout(:,:) = glamuout(:,:)
  zglamvout(:,:) = glamvout(:,:)
endif

do ji=1,jpiout
do jj=1,jpjout

  ! local angle between i-direction and the zonal direction and between j-direction and the meridional direction (should be similar)

  if     ( ji .eq. 1      ) then
   angoutXt(ji,jj) = ATAN2( gphitout(ji+1,jj) - gphitout(ji  ,jj) , ( zglamtout(ji+1,jj) - zglamtout(ji  ,jj) ) * cos(rad*gphitout(ji,jj)) )
  elseif ( ji .eq. jpiout ) then 
   angoutXt(ji,jj) = ATAN2( gphitout(ji  ,jj) - gphitout(ji-1,jj) , ( zglamtout(ji  ,jj) - zglamtout(ji-1,jj) ) * cos(rad*gphitout(ji,jj)) )
  else
   angoutXt(ji,jj) = ATAN2( gphitout(ji+1,jj) - gphitout(ji-1,jj) , ( zglamtout(ji+1,jj) - zglamtout(ji-1,jj) ) * cos(rad*gphitout(ji,jj)) )
  endif
  if     ( jj .eq. 1      ) then
   angoutYt(ji,jj) = ATAN2( gphitout(ji,jj+1) - gphitout(ji,jj  ) , ( zglamtout(ji,jj+1) - zglamtout(ji,jj  ) ) * cos(rad*gphitout(ji,jj)) )
  elseif ( jj .eq. jpjout ) then
   angoutYt(ji,jj) = ATAN2( gphitout(ji,jj  ) - gphitout(ji,jj-1) , ( zglamtout(ji,jj  ) - zglamtout(ji,jj-1) ) * cos(rad*gphitout(ji,jj)) )
  else
   angoutYt(ji,jj) = ATAN2( gphitout(ji,jj+1) - gphitout(ji,jj-1) , ( zglamtout(ji,jj+1) - zglamtout(ji,jj-1) ) * cos(rad*gphitout(ji,jj)) )
  endif

  if     ( ji .eq. 1      ) then
   angoutXu(ji,jj) = ATAN2( gphiuout(ji+1,jj) - gphiuout(ji  ,jj) , ( zglamuout(ji+1,jj) - zglamuout(ji  ,jj) ) * cos(rad*gphiuout(ji,jj)) )
  elseif ( ji .eq. jpiout ) then 
   angoutXu(ji,jj) = ATAN2( gphiuout(ji  ,jj) - gphiuout(ji-1,jj) , ( zglamuout(ji  ,jj) - zglamuout(ji-1,jj) ) * cos(rad*gphiuout(ji,jj)) )
  else
   angoutXu(ji,jj) = ATAN2( gphiuout(ji+1,jj) - gphiuout(ji-1,jj) , ( zglamuout(ji+1,jj) - zglamuout(ji-1,jj) ) * cos(rad*gphiuout(ji,jj)) )
  endif
  if     ( jj .eq. 1      ) then
   angoutYu(ji,jj) = ATAN2( gphiuout(ji,jj+1) - gphiuout(ji,jj  ) , ( zglamuout(ji,jj+1) - zglamuout(ji,jj  ) ) * cos(rad*gphiuout(ji,jj)) )
  elseif ( jj .eq. jpjout ) then
   angoutYu(ji,jj) = ATAN2( gphiuout(ji,jj  ) - gphiuout(ji,jj-1) , ( zglamuout(ji,jj  ) - zglamuout(ji,jj-1) ) * cos(rad*gphiuout(ji,jj)) )
  else
   angoutYu(ji,jj) = ATAN2( gphiuout(ji,jj+1) - gphiuout(ji,jj-1) , ( zglamuout(ji,jj+1) - zglamuout(ji,jj-1) ) * cos(rad*gphiuout(ji,jj)) )
  endif

  if     ( ji .eq. 1      ) then
   angoutXv(ji,jj) = ATAN2( gphivout(ji+1,jj) - gphivout(ji  ,jj) , ( zglamvout(ji+1,jj) - zglamvout(ji  ,jj) ) * cos(rad*gphivout(ji,jj)) )
  elseif ( ji .eq. jpiout ) then 
   angoutXv(ji,jj) = ATAN2( gphivout(ji  ,jj) - gphivout(ji-1,jj) , ( zglamvout(ji  ,jj) - zglamvout(ji-1,jj) ) * cos(rad*gphivout(ji,jj)) )
  else
   angoutXv(ji,jj) = ATAN2( gphivout(ji+1,jj) - gphivout(ji-1,jj) , ( zglamvout(ji+1,jj) - zglamvout(ji-1,jj) ) * cos(rad*gphivout(ji,jj)) )
  endif
  if     ( jj .eq. 1      ) then
   angoutYv(ji,jj) = ATAN2( gphivout(ji,jj+1) - gphivout(ji,jj  ) , ( zglamvout(ji,jj+1) - zglamvout(ji,jj  ) ) * cos(rad*gphivout(ji,jj)) )
  elseif ( jj .eq. jpjout ) then
   angoutYv(ji,jj) = ATAN2( gphivout(ji,jj  ) - gphivout(ji,jj-1) , ( zglamvout(ji,jj  ) - zglamvout(ji,jj-1) ) * cos(rad*gphivout(ji,jj)) )
  else
   angoutYv(ji,jj) = ATAN2( gphivout(ji,jj+1) - gphivout(ji,jj-1) , ( zglamvout(ji,jj+1) - zglamvout(ji,jj-1) ) * cos(rad*gphivout(ji,jj)) )
  endif

enddo
enddo

!===============

!=========================================================================================
! 3- CALCULATE WEIGHTS FOR OBC FILES 
!    (and find what are the 4 global T-points surrounding a T-point of the boundary)
!=========================================================================================

ALLOCATE( ziWt  (jpiout,jpjout), zjWt  (jpiout,jpjout), wgWt(jpiout,jpjout) )
ALLOCATE( ziCt  (jpiout,jpjout), zjCt  (jpiout,jpjout), wgCt(jpiout,jpjout) )
ALLOCATE( ziEt  (jpiout,jpjout), zjEt  (jpiout,jpjout), wgEt(jpiout,jpjout) )
ALLOCATE( ziSt  (jpiout,jpjout), zjSt  (jpiout,jpjout), wgSt(jpiout,jpjout) )
ALLOCATE( ziMt  (jpiout,jpjout), zjMt  (jpiout,jpjout), wgMt(jpiout,jpjout) )
ALLOCATE( ziNt  (jpiout,jpjout), zjNt  (jpiout,jpjout), wgNt(jpiout,jpjout) )
ALLOCATE( zzit  (jpiout,jpjout), zzjt  (jpiout,jpjout)                             )
ALLOCATE( zzitm1(jpiout,jpjout), zzjtm1(jpiout,jpjout)                             )
ALLOCATE( zzitp1(jpiout,jpjout), zzjtp1(jpiout,jpjout)                             )
!-
ALLOCATE( ziWu  (jpiout,jpjout), zjWu  (jpiout,jpjout), wgWu(jpiout,jpjout) )
ALLOCATE( ziCu  (jpiout,jpjout), zjCu  (jpiout,jpjout), wgCu(jpiout,jpjout) )
ALLOCATE( ziEu  (jpiout,jpjout), zjEu  (jpiout,jpjout), wgEu(jpiout,jpjout) )
ALLOCATE( ziSu  (jpiout,jpjout), zjSu  (jpiout,jpjout), wgSu(jpiout,jpjout) )
ALLOCATE( ziMu  (jpiout,jpjout), zjMu  (jpiout,jpjout), wgMu(jpiout,jpjout) )
ALLOCATE( ziNu  (jpiout,jpjout), zjNu  (jpiout,jpjout), wgNu(jpiout,jpjout) )
ALLOCATE( zziu  (jpiout,jpjout), zzju  (jpiout,jpjout)                             )
ALLOCATE( zzium1(jpiout,jpjout), zzjum1(jpiout,jpjout)                             )
ALLOCATE( zziup1(jpiout,jpjout), zzjup1(jpiout,jpjout)                             )
!-
ALLOCATE( ziWv  (jpiout,jpjout), zjWv  (jpiout,jpjout), wgWv(jpiout,jpjout) )
ALLOCATE( ziCv  (jpiout,jpjout), zjCv  (jpiout,jpjout), wgCv(jpiout,jpjout) )
ALLOCATE( ziEv  (jpiout,jpjout), zjEv  (jpiout,jpjout), wgEv(jpiout,jpjout) )
ALLOCATE( ziSv  (jpiout,jpjout), zjSv  (jpiout,jpjout), wgSv(jpiout,jpjout) )
ALLOCATE( ziMv  (jpiout,jpjout), zjMv  (jpiout,jpjout), wgMv(jpiout,jpjout) )
ALLOCATE( ziNv  (jpiout,jpjout), zjNv  (jpiout,jpjout), wgNv(jpiout,jpjout) )
ALLOCATE( zziv  (jpiout,jpjout), zzjv  (jpiout,jpjout)                             )
ALLOCATE( zzivm1(jpiout,jpjout), zzjvm1(jpiout,jpjout)                             )
ALLOCATE( zzivp1(jpiout,jpjout), zzjvp1(jpiout,jpjout)                             )

IF ( idateline .EQ. 1 ) THEN
  where ( glamtin(:,:) .lt. 0.0 )
    zglamtin(:,:) = 360.0 + glamtin(:,:)
  elsewhere
    zglamtin(:,:) = glamtin(:,:)
  endwhere
  !-
  where ( glamuin(:,:) .lt. 0.0 )
    zglamuin(:,:) = 360.0 + glamuin(:,:)
  elsewhere
    zglamuin(:,:) = glamuin(:,:)
  endwhere
  !-
  where ( glamvin(:,:) .lt. 0.0 )
    zglamvin(:,:) = 360.0 + glamvin(:,:)
  elsewhere
    zglamvin(:,:) = glamvin(:,:)
  endwhere
ELSE
  zglamtin(:,:) = glamtin(:,:) 
  zglamuin(:,:) = glamuin(:,:)
  zglamvin(:,:) = glamvin(:,:)
ENDIF

zzit(:,:)=0 !- to notice error if a point is not filled
zzjt(:,:)=0 !- to notice error if a point is not filled

!##### gridT #####

DO jiout=1,jpiout
DO jjout=1,jpjout

  distmin = ra * rad * 10.0 ! let's assume that nobody's using grids coarser than 10 degrees...

  !-- loop to find closest point ( we allow 1 bdy cell to cover a maximum of 3 cells from the input grid )
  do jiin=1,jpiin
  do jjin=1,jpjin
    dist = ra * rad * sqrt(   ( cos(rad*gphitout(jiout,jjout)) * ( zglamtout(jiout,jjout) - zglamtin(jiin,jjin) ) )**2  &
      &                     + (                                    gphitout(jiout,jjout) -  gphitin(jiin,jjin)   )**2  )
    if ( dist .lt. distmin ) then
      distmin=dist
      zzit(jiout,jjout) = jiin
      zzjt(jiout,jjout) = jjin
    endif
  enddo 
  enddo
  !- East-West bounds
  if ( perio .eq. 1 ) then !- periodic
    if     ( zzit(jiout,jjout)+1 .gt. jpiin ) then
      zzitp1(jiout,jjout) = 3
      zzitm1(jiout,jjout) = zzit(jiout,jjout) - 1
    elseif ( zzit(jiout,jjout)-1 .lt. 1     ) then
      zzitp1(jiout,jjout) = zzit(jiout,jjout) + 1
      zzitm1(jiout,jjout) = jpiin-2
    else
      zzitp1(jiout,jjout) = zzit(jiout,jjout) + 1
      zzitm1(jiout,jjout) = zzit(jiout,jjout) - 1
    endif
  elseif ( perio .eq. 0 ) then
    if     ( zzit(jiout,jjout)+1 .gt. jpiin ) then
      zzitp1(jiout,jjout) = zzit(jiout,jjout)
      zzitm1(jiout,jjout) = zzit(jiout,jjout) - 1
    elseif ( zzit(jiout,jjout)-1 .lt. 1     ) then
      zzitp1(jiout,jjout) = zzit(jiout,jjout) + 1
      zzitm1(jiout,jjout) = zzit(jiout,jjout)
    else
      zzitp1(jiout,jjout) = zzit(jiout,jjout) + 1
      zzitm1(jiout,jjout) = zzit(jiout,jjout) - 1
    endif
  else
    write(*,*) '~!@#$%^* perio must be either 0 or 1 >>>>> stop !!'
    stop
  endif
  !- upper lower bounds of global input grid
  if     ( zzjt(jiout,jjout)+1 .gt. jpjin ) then
    zzjtp1(jiout,jjout) = zzjt(jiout,jjout)
    zzjtm1(jiout,jjout) = zzjt(jiout,jjout) - 1
  elseif ( zzit(jiout,jjout)-1 .lt. 1     ) then
    zzjtp1(jiout,jjout) = zzjt(jiout,jjout) + 1
    zzjtm1(jiout,jjout) = zzjt(jiout,jjout)
  else
    zzjtp1(jiout,jjout) = zzjt(jiout,jjout) + 1
    zzjtm1(jiout,jjout) = zzjt(jiout,jjout) - 1
  endif

  !-- local angle on global input grid between i-direction and the zonal direction and between j-direction and the meridional direction
  anginX = ATAN2(    gphitin( zzitp1(jiout,jjout) , zzjt(jiout,jjout) ) -  gphitin( zzitm1(jiout,jjout) , zzjt(jiout,jjout)) , &
  &                ( zglamtin( zzitp1(jiout,jjout) , zzjt(jiout,jjout) ) - zglamtin( zzitm1(jiout,jjout) , zzjt(jiout,jjout)) ) &
  &                * cos( gphitin( zzit(jiout,jjout) , zzjt(jiout,jjout) ) * rad )                                                )
!      anginY = ATAN2( ( zglamtin( zzit  (jiout,jjout) , zzjtm1(jiout,jjout) ) - zglamtin( zzit  (jiout,jjout) , zzjtp1(jiout,jjout)) ) &
!      &                * cos( gphitin( zzit(jiout,jjout) , zzjt(jiout,jjout) ) * rad )                                                , &
!      &                   gphitin( zzit  (jiout,jjout) , zzjtp1(jiout,jjout) ) -  gphitin( zzit  (jiout,jjout) , zzjtm1(jiout,jjout))   )
  anginY = ATAN2(    gphitin( zzit(jiout,jjout) , zzjtp1(jiout,jjout) ) -  gphitin( zzit(jiout,jjout) , zzjtm1(jiout,jjout)) , &
  &                ( zglamtin( zzit(jiout,jjout) , zzjtp1(jiout,jjout) ) - zglamtin( zzit(jiout,jjout) , zzjtm1(jiout,jjout)) ) &
  &                * cos( gphitin( zzit(jiout,jjout) , zzjt(jiout,jjout) ) * rad )                                                )

  !-- local angle between the two grids :
  angleXt(jiout,jjout) = angoutXt(jiout,jjout) - anginX
  angleYt(jiout,jjout) = angoutYt(jiout,jjout) - anginY
  if ( abs(angleXt(jiout,jjout)/rad) .gt. 45.0 .or. abs(angleYt(jiout,jjout)/rad) .gt. 45.0 ) then
    write(*,*) '@@@@@@ WARNING : angle between 2 grids > 45 deg at point of the bdy :', jiout, jjout
  endif     

  !------------------------------------------------------------
  !   coordinates of closest points along X and weights
  !   ( take 2 or 3 points according to mesh size )
  !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
  if      ( e1tout(jiout,jjout) .gt. 50*e1tin(zzit(jiout,jjout),zzjt(jiout,jjout))*cos(angleXt(jiout,jjout)*rad) ) then ! it should even be 3 here
    write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along X >>>>>>> stop'
    write(*,*) '(jiout,jjout) =', jiout,jjout
    write(*,*) 'angleXt =', angleXt(jiout,jjout)
    write(*,*) 'e1tout(jiout,jjout) =', e1tout(jiout,jjout)
    write(*,*) 'e1tin(zzit(jiout,jjout),zzjt(jiout,jjout))*cos(angleXt(jiout,jjout)*rad)=', e1tin(zzit(jiout,jjout),zzjt(jiout,jjout))*cos(angleXt(jiout,jjout)*rad)
    stop
  elseif  (   e1tout(jiout,jjout) .ge. e1tin(zzit(jiout,jjout),zzjt(jiout,jjout)) * cos(angleXt(jiout,jjout)*rad)  ) then
    !- use 3 points for interpolation
    ziWt(jiout,jjout)   = zzitm1(jiout,jjout) ! closest point westward
    ziCt(jiout,jjout)   = zzit  (jiout,jjout) ! closest point ( central )
    ziEt(jiout,jjout)   = zzitp1(jiout,jjout) ! closest point eastward
    wgCt(jiout,jjout)   = e1tin(zzit(jiout,jjout),zzjt(jiout,jjout))* cos(angleXt(jiout,jjout)*rad) !weight for central point
  elseif  (       zglamtout(jiout,jjout) .ge.  zglamtin(zzit  (jiout,jjout),zzjt(jiout,jjout))        )  then
    !- use 2 points for interpolation
    ziWt(jiout,jjout)   = zzit  (jiout,jjout) ! closest point westward
    ziCt(jiout,jjout)   = 1                   ! dummy
    ziEt(jiout,jjout)   = zzitp1(jiout,jjout) ! closest point eastward
    wgCt(jiout,jjout)   = 0.0                 ! weight for central point
  else
    !- use 2 points for interpolation
    ziWt(jiout,jjout)   = zzitm1(jiout,jjout) ! closest point westward
    ziCt(jiout,jjout)   = 1                   ! dummy
    ziEt(jiout,jjout)   = zzit  (jiout,jjout) ! closest point eastward
    wgCt(jiout,jjout)   = 0.0                 ! weight for central point
  endif
  zjWt(jiout,jjout) = zzjt  (jiout,jjout) !  "
  zjCt(jiout,jjout) = zzjt  (jiout,jjout) ! to be adapted if angles > 45 deg (??)
  zjEt(jiout,jjout) = zzjt  (jiout,jjout) !  "
  !- weight for westward point
  wgWt(jiout,jjout)   = MAX( 0.0,                                                                                         &
    &                     0.5 * e1tin(ziWt(jiout,jjout),zjWt(jiout,jjout)) * cos(angleXt(jiout,jjout)*rad)                &
    &                   - ra * rad * abs(   ( zglamtout(jiout,jjout) - zglamtin(ziWt(jiout,jjout),zjWt(jiout,jjout)) )     &
    &                                     * cos(  gphitout(jiout,jjout)*rad )                                           )  &
    &                   + 0.5 * e1tout(jiout,jjout)                                                                       )
  !- weight for eastward point
  wgEt(jiout,jjout)   = MAX( 0.0,                                                                                         &
    &                     0.5 * e1tin(ziEt(jiout,jjout),zjEt(jiout,jjout)) * cos(angleXt(jiout,jjout)*rad)                &
    &                   - ra * rad * abs(  ( zglamtout(jiout,jjout) - zglamtin(ziEt(jiout,jjout),zjEt(jiout,jjout)) )      &
    &                                     * cos(  gphitout(jiout,jjout)*rad )                                           )  &
    &                   + 0.5 * e1tout(jiout,jjout)                                                                       )

  !------------------------------------------------------------
  !   coordinates of closest points along Y and weights
  !   ( take 2 or 3 points according to mesh size )
  !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
  if      ( e2tout(jiout,jjout) .gt. 50*e2tin(zzit(jiout,jjout),zzjt(jiout,jjout))*cos(angleYt(jiout,jjout)*rad)  ) then ! it should even be 3 here
    write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along Y >>>>>>> stop'
    write(*,*) '(jiout,jjout) =', jiout,jjout
    write(*,*) 'angleYt =', angleYt(jiout,jjout)
    write(*,*) 'e2tout(jiout,jjout) =', e2tout(jiout,jjout)
    write(*,*) 'e2tin(zzit(jiout,jjout),zzjt(jiout,jjout))*cos(angleXt(jiout,jjout)*rad)=', e2tin(zzit(jiout,jjout),zzjt(jiout,jjout))*cos(angleYt(jiout,jjout)*rad)
    stop
  elseif  (   e2tout(jiout,jjout) .ge. e2tin(zzit(jiout,jjout),zzjt(jiout,jjout)) * cos(angleYt(jiout,jjout)*rad)  ) then
    !- use 3 points for interpolation
    zjSt(jiout,jjout)   = zzjtm1(jiout,jjout) ! closest point southward
    zjMt(jiout,jjout)   = zzjt  (jiout,jjout) ! closest point ( middle )
    zjNt(jiout,jjout)   = zzjtp1(jiout,jjout) ! closest point northward
    wgMt(jiout,jjout)   = e2tin(zzit(jiout,jjout),zzjt(jiout,jjout))* cos(angleYt(jiout,jjout)*rad) !weight for central point
  elseif  (       gphitout(jiout,jjout) .ge.  gphitin(zzit  (jiout,jjout),zzjt(jiout,jjout))        )  then
    !- use 2 points for interpolation
    zjSt(jiout,jjout)   = zzjt  (jiout,jjout) ! closest point southward
    zjMt(jiout,jjout)   = zzjt  (jiout,jjout) ! dummy
    zjNt(jiout,jjout)   = zzjtp1(jiout,jjout) ! closest point northward
    wgMt(jiout,jjout)   = 0.0                 ! weight for middle point
  else
    !- use 2 points for interpolation
    zjSt(jiout,jjout)   = zzjtm1(jiout,jjout) ! closest point southward
    zjMt(jiout,jjout)   = zzjt  (jiout,jjout) ! dummy
    zjNt(jiout,jjout)   = zzjt  (jiout,jjout) ! closest point northward
    wgMt(jiout,jjout)   = 0.0                 ! weight for middle point
  endif
  ziSt(jiout,jjout) = zzit  (jiout,jjout) ! "
  ziMt(jiout,jjout) = zzit  (jiout,jjout) ! to be adapted if angles > 45 deg (??)
  ziNt(jiout,jjout) = zzit  (jiout,jjout) ! "
  !- weight for southward point
  wgSt(jiout,jjout)   = MAX( 0.0,                                                                                  &
    &                     0.5 * e2tin(ziSt(jiout,jjout),zjSt(jiout,jjout)) * cos(angleYt(jiout,jjout)*rad)         &
    &                   - ra * rad * abs(  gphitout(jiout,jjout) - gphitin(ziSt(jiout,jjout),zjSt(jiout,jjout)) )   &
    &                   + 0.5 * e2tout(jiout,jjout)                                                                )
  !- weight for northward point
  wgNt(jiout,jjout)   = MAX( 0.0,                                                                                  &
    &                     0.5 * e2tin(ziNt(jiout,jjout),zjNt(jiout,jjout)) * cos(angleYt(jiout,jjout)*rad)         &
    &                   - ra * rad * abs(  gphitout(jiout,jjout) - gphitin(ziNt(jiout,jjout),zjNt(jiout,jjout)) )   &
    &                   + 0.5 * e2tout(jiout,jjout)                                                                )

  !----------------------------------------------------------------
  !  NO VERTICAL INTERPOLATION HERE (ASSUMED TO BE THE SAME GRID) !
  !----------------------------------------------------------------  

ENDDO !-- jjout
ENDDO !-- jiout

!##### gridU #####

zziu(:,:)=0 !- to notice error if a point is not filled
zzju(:,:)=0 !- to notice error if a point is not filled

DO jiout=1,jpiout
DO jjout=1,jpjout

  distmin = ra * rad * 10.0 ! let's assume that nobody's using grids coarser than 10 degrees...

  !-- loop to find closest point ( we allow 1 bdy cell to cover a maximum of 3 cells from the input grid )
  do jiin=1,jpiin
  do jjin=1,jpjin
    dist = ra * rad * sqrt(   ( cos(rad*gphiuout(jiout,jjout)) * ( zglamuout(jiout,jjout) - zglamuin(jiin,jjin) ) )**2  &
      &                     + (                                    gphiuout(jiout,jjout) -  gphiuin(jiin,jjin)   )**2  )
    if ( dist .lt. distmin ) then
      distmin=dist
      zziu(jiout,jjout) = jiin
      zzju(jiout,jjout) = jjin
    endif
  enddo 
  enddo
  !- East-West bounds
  if ( perio .eq. 1 ) then !- periodic
    if     ( zziu(jiout,jjout)+1 .gt. jpiin ) then
      zziup1(jiout,jjout) = 3
      zzium1(jiout,jjout) = zziu(jiout,jjout) - 1
    elseif ( zziu(jiout,jjout)-1 .lt. 1     ) then
      zziup1(jiout,jjout) = zziu(jiout,jjout) + 1
      zzium1(jiout,jjout) = jpiin-2
    else
      zziup1(jiout,jjout) = zziu(jiout,jjout) + 1
      zzium1(jiout,jjout) = zziu(jiout,jjout) - 1
    endif
  else !- non-periodic
    if     ( zziu(jiout,jjout)+1 .gt. jpiin ) then
      zziup1(jiout,jjout) = zziu(jiout,jjout)
      zzium1(jiout,jjout) = zziu(jiout,jjout) - 1
    elseif ( zziu(jiout,jjout)-1 .lt. 1     ) then
      zziup1(jiout,jjout) = zziu(jiout,jjout) + 1
      zzium1(jiout,jjout) = zziu(jiout,jjout)
    else
      zziup1(jiout,jjout) = zziu(jiout,jjout) + 1
      zzium1(jiout,jjout) = zziu(jiout,jjout) - 1
    endif
  endif
  !- upper lower bounds of global input grid
  if     ( zzju(jiout,jjout)+1 .gt. jpjin ) then
    zzjup1(jiout,jjout) = zzju(jiout,jjout)
    zzjum1(jiout,jjout) = zzju(jiout,jjout) - 1
  elseif ( zziu(jiout,jjout)-1 .lt. 1     ) then
    zzjup1(jiout,jjout) = zzju(jiout,jjout) + 1
    zzjum1(jiout,jjout) = zzju(jiout,jjout)
  else
    zzjup1(jiout,jjout) = zzju(jiout,jjout) + 1
    zzjum1(jiout,jjout) = zzju(jiout,jjout) - 1
  endif

  !-- local angle on global input grid between i-direction and the zonal direction and between j-direction and the meridional direction
  anginX = ATAN2(    gphiuin( zziup1(jiout,jjout) , zzju(jiout,jjout) ) -  gphiuin( zzium1(jiout,jjout) , zzju(jiout,jjout)) , &
  &                ( zglamuin( zziup1(jiout,jjout) , zzju(jiout,jjout) ) - zglamuin( zzium1(jiout,jjout) , zzju(jiout,jjout)) ) &
  &                * cos( gphiuin( zziu(jiout,jjout) , zzju(jiout,jjout) ) * rad )                                                )
!      anginY = ATAN2( ( zglamuin( zziu  (jiout,jjout) , zzjum1(jiout,jjout) ) - zglamuin( zziu  (jiout,jjout) , zzjup1(jiout,jjout)) ) &
!      &                * cos( gphiuin( zziu(jiout,jjout) , zzju(jiout,jjout) ) * rad )                                                , &
!      &                   gphiuin( zziu  (jiout,jjout) , zzjup1(jiout,jjout) ) -  gphiuin( zziu  (jiout,jjout) , zzjum1(jiout,jjout))   )
  anginY = ATAN2(    gphiuin( zziu(jiout,jjout) , zzjup1(jiout,jjout) ) -  gphiuin( zziu(jiout,jjout) , zzjum1(jiout,jjout)) , &
  &                ( zglamuin( zziu(jiout,jjout) , zzjup1(jiout,jjout) ) - zglamuin( zziu(jiout,jjout) , zzjum1(jiout,jjout)) ) &
  &                * cos( gphiuin( zziu(jiout,jjout) , zzju(jiout,jjout) ) * rad )                                                )

  !-- local angle between the two grids :
  angleXu(jiout,jjout) = angoutXu(jiout,jjout) - anginX
  angleYu(jiout,jjout) = angoutYt(jiout,jjout) - anginY
  if ( abs(angleXu(jiout,jjout)/rad) .gt. 45.0 .or. abs(angleYu(jiout,jjout)/rad) .gt. 45.0 ) then
    write(*,*) '@@@@@@ WARNING : angle between 2 grids > 45 deg at point of the bdy :', jiout, jjout
  endif     

  !------------------------------------------------------------
  !   coordinates of closest points along X and weights
  !   ( take 2 or 3 points according to mesh size )
  !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
  if      ( e1uout(jiout,jjout) .gt. 50*e1uin(zziu(jiout,jjout),zzju(jiout,jjout))*cos(angleXu(jiout,jjout)*rad) ) then ! it should even be 3 here
    write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along X >>>>>>> stop'
    write(*,*) '(jiout,jjout) =', jiout,jjout
    write(*,*) 'angleXu =', angleXu(jiout,jjout)
    write(*,*) 'e1uout(jiout,jjout) =', e1uout(jiout,jjout)
    write(*,*) 'e1uin(zziu(jiout,jjout),zzju(jiout,jjout))*cos(angleXu(jiout,jjout)*rad)=', e1uin(zziu(jiout,jjout),zzju(jiout,jjout))*cos(angleXu(jiout,jjout)*rad)
    stop
  elseif  (   e1uout(jiout,jjout) .ge. e1uin(zziu(jiout,jjout),zzju(jiout,jjout)) * cos(angleXu(jiout,jjout)*rad)  ) then
    !- use 3 points for interpolation
    ziWu(jiout,jjout)   = zzium1(jiout,jjout) ! closest point westward
    ziCu(jiout,jjout)   = zziu  (jiout,jjout) ! closest point ( central )
    ziEu(jiout,jjout)   = zziup1(jiout,jjout) ! closest point eastward
    wgCu(jiout,jjout)   = e1uin(zziu(jiout,jjout),zzju(jiout,jjout))* cos(angleXu(jiout,jjout)*rad) !weight for central point
  elseif  (       zglamuout(jiout,jjout) .ge.  zglamuin(zziu  (jiout,jjout),zzju(jiout,jjout))        )  then
    !- use 2 points for interpolation
    ziWu(jiout,jjout)   = zziu  (jiout,jjout) ! closest point westward
    ziCu(jiout,jjout)   = 1                   ! dummy
    ziEu(jiout,jjout)   = zziup1(jiout,jjout) ! closest point eastward
    wgCu(jiout,jjout)   = 0.0                 ! weight for central point
  else
    !- use 2 points for interpolation
    ziWu(jiout,jjout)   = zzium1(jiout,jjout) ! closest point westward
    ziCu(jiout,jjout)   = 1                   ! dummy
    ziEu(jiout,jjout)   = zziu  (jiout,jjout) ! closest point eastward
    wgCu(jiout,jjout)   = 0.0                 ! weight for central point
  endif
  zjWu(jiout,jjout) = zzju  (jiout,jjout) !  "
  zjCu(jiout,jjout) = zzju  (jiout,jjout) ! to be adapted if angles > 45 deg (??)
  zjEu(jiout,jjout) = zzju  (jiout,jjout) !  "
  !- weight for westward point
  wgWu(jiout,jjout)   = MAX( 0.0,                                                                                         &
    &                     0.5 * e1uin(ziWu(jiout,jjout),zjWu(jiout,jjout)) * cos(angleXu(jiout,jjout)*rad)                &
    &                   - ra * rad * abs(   ( zglamuout(jiout,jjout) - zglamuin(ziWu(jiout,jjout),zjWu(jiout,jjout)) )     &
    &                                     * cos(  gphiuout(jiout,jjout)*rad )                                           )  &
    &                   + 0.5 * e1uout(jiout,jjout)                                                                       )
  !- weight for eastward point
  wgEu(jiout,jjout)   = MAX( 0.0,                                                                                         &
    &                     0.5 * e1uin(ziEu(jiout,jjout),zjEu(jiout,jjout)) * cos(angleXu(jiout,jjout)*rad)                &
    &                   - ra * rad * abs(  ( zglamuout(jiout,jjout) - zglamuin(ziEu(jiout,jjout),zjEu(jiout,jjout)) )      &
    &                                     * cos(  gphiuout(jiout,jjout)*rad )                                           )  &
    &                   + 0.5 * e1uout(jiout,jjout)                                                                       )

  !------------------------------------------------------------
  !   coordinates of closest points along Y and weights
  !   ( take 2 or 3 points according to mesh size )
  !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
  if      ( e2uout(jiout,jjout) .gt. 50*e2uin(zziu(jiout,jjout),zzju(jiout,jjout))*cos(angleYu(jiout,jjout)*rad)  ) then ! it should even be 3 here
    write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along Y >>>>>>> stop'
    write(*,*) '(jiout,jjout) =', jiout,jjout
    write(*,*) 'angleYu =', angleYu(jiout,jjout)
    write(*,*) 'e2uout(jiout,jjout) =', e2uout(jiout,jjout)
    write(*,*) 'e2uin(zziu(jiout,jjout),zzju(jiout,jjout))*cos(angleXu(jiout,jjout)*rad)=', e2uin(zziu(jiout,jjout),zzju(jiout,jjout))*cos(angleYu(jiout,jjout)*rad)
    stop
  elseif  (   e2uout(jiout,jjout) .ge. e2uin(zziu(jiout,jjout),zzju(jiout,jjout)) * cos(angleYu(jiout,jjout)*rad)  ) then
    !- use 3 points for interpolation
    zjSu(jiout,jjout)   = zzjum1(jiout,jjout) ! closest point southward
    zjMu(jiout,jjout)   = zzju  (jiout,jjout) ! closest point ( middle )
    zjNu(jiout,jjout)   = zzjup1(jiout,jjout) ! closest point northward
    wgMu(jiout,jjout)   = e2uin(zziu(jiout,jjout),zzju(jiout,jjout))* cos(angleYu(jiout,jjout)*rad) !weight for central point
  elseif  (       gphiuout(jiout,jjout) .ge.  gphiuin(zziu  (jiout,jjout),zzju(jiout,jjout))        )  then
    !- use 2 points for interpolation
    zjSu(jiout,jjout)   = zzju  (jiout,jjout) ! closest point southward
    zjMu(jiout,jjout)   = zzju  (jiout,jjout) ! dummy
    zjNu(jiout,jjout)   = zzjup1(jiout,jjout) ! closest point northward
    wgMu(jiout,jjout)   = 0.0                 ! weight for middle point
  else
    !- use 2 points for interpolation
    zjSu(jiout,jjout)   = zzjum1(jiout,jjout) ! closest point southward
    zjMu(jiout,jjout)   = zzju  (jiout,jjout) ! dummy
    zjNu(jiout,jjout)   = zzju  (jiout,jjout) ! closest point northward
    wgMu(jiout,jjout)   = 0.0                 ! weight for middle point
  endif
  ziSu(jiout,jjout) = zziu  (jiout,jjout) ! "
  ziMu(jiout,jjout) = zziu  (jiout,jjout) ! to be adapted if angles > 45 deg (??)
  ziNu(jiout,jjout) = zziu  (jiout,jjout) ! "
  !- weight for southward point
  wgSu(jiout,jjout)   = MAX( 0.0,                                                                                  &
    &                     0.5 * e2uin(ziSu(jiout,jjout),zjSu(jiout,jjout)) * cos(angleYu(jiout,jjout)*rad)         &
    &                   - ra * rad * abs(  gphiuout(jiout,jjout) - gphiuin(ziSu(jiout,jjout),zjSu(jiout,jjout)) )   &
    &                   + 0.5 * e2uout(jiout,jjout)                                                                )
  !- weight for northward point
  wgNu(jiout,jjout)   = MAX( 0.0,                                                                                  &
    &                     0.5 * e2uin(ziNu(jiout,jjout),zjNu(jiout,jjout)) * cos(angleYu(jiout,jjout)*rad)         &
    &                   - ra * rad * abs(  gphiuout(jiout,jjout) - gphiuin(ziNu(jiout,jjout),zjNu(jiout,jjout)) )   &
    &                   + 0.5 * e2uout(jiout,jjout)                                                                )

  !----------------------------------------------------------------
  !  NO VERTICAL INTERPOLATION HERE (ASSUMED TO BE THE SAME GRID) !
  !----------------------------------------------------------------  

ENDDO !-- jjout
ENDDO !-- jiout

!##### gridV #####

zziv(:,:)=0 !- to notice error if a point is not filled
zzjv(:,:)=0 !- to notice error if a point is not filled

DO jiout=1,jpiout
DO jjout=1,jpjout

  distmin = ra * rad * 10.0 ! let's assume that nobody's using grids coarser than 10 degrees...

  !-- loop to find closest point ( we allow 1 bdy cell to cover a maximum of 3 cells from the input grid )
  do jiin=1,jpiin
  do jjin=1,jpjin
    dist = ra * rad * sqrt(   ( cos(rad*gphivout(jiout,jjout)) * ( zglamvout(jiout,jjout) - zglamvin(jiin,jjin) ) )**2  &
      &                     + (                                    gphivout(jiout,jjout) -  gphivin(jiin,jjin)   )**2  )
    if ( dist .lt. distmin ) then
      distmin=dist
      zziv(jiout,jjout) = jiin
      zzjv(jiout,jjout) = jjin
    endif
  enddo 
  enddo
  !- East-West bounds
  if ( perio .eq. 1 ) then !- periodic
    if     ( zziv(jiout,jjout)+1 .gt. jpiin ) then
      zzivp1(jiout,jjout) = 3 
      zzivm1(jiout,jjout) = zziv(jiout,jjout) - 1
    elseif ( zziv(jiout,jjout)-1 .lt. 1     ) then
      zzivp1(jiout,jjout) = zziv(jiout,jjout) + 1
      zzivm1(jiout,jjout) = jpiin-2 
    else
      zzivp1(jiout,jjout) = zziv(jiout,jjout) + 1
      zzivm1(jiout,jjout) = zziv(jiout,jjout) - 1
    endif
  else !- non-periodic
    if     ( zziv(jiout,jjout)+1 .gt. jpiin ) then
      zzivp1(jiout,jjout) = zziv(jiout,jjout)
      zzivm1(jiout,jjout) = zziv(jiout,jjout) - 1
    elseif ( zziv(jiout,jjout)-1 .lt. 1     ) then
      zzivp1(jiout,jjout) = zziv(jiout,jjout) + 1
      zzivm1(jiout,jjout) = zziv(jiout,jjout)
    else
      zzivp1(jiout,jjout) = zziv(jiout,jjout) + 1
      zzivm1(jiout,jjout) = zziv(jiout,jjout) - 1
    endif
  endif
  !- upper lower bounds of global input grid
  if     ( zzjv(jiout,jjout)+1 .gt. jpjin ) then
    zzjvp1(jiout,jjout) = zzjv(jiout,jjout)
    zzjvm1(jiout,jjout) = zzjv(jiout,jjout) - 1
  elseif ( zziv(jiout,jjout)-1 .lt. 1     ) then
    zzjvp1(jiout,jjout) = zzjv(jiout,jjout) + 1
    zzjvm1(jiout,jjout) = zzjv(jiout,jjout)
  else
    zzjvp1(jiout,jjout) = zzjv(jiout,jjout) + 1
    zzjvm1(jiout,jjout) = zzjv(jiout,jjout) - 1
  endif

  !-- local angle on global input grid between i-direction and the zonal direction and between j-direction and the meridional direction
  anginX = ATAN2(    gphivin( zzivp1(jiout,jjout) , zzjv(jiout,jjout) ) -  gphivin( zzivm1(jiout,jjout) , zzjv(jiout,jjout)) , &
  &                ( zglamvin( zzivp1(jiout,jjout) , zzjv(jiout,jjout) ) - zglamvin( zzivm1(jiout,jjout) , zzjv(jiout,jjout)) ) &
  &                * cos( gphivin( zziv(jiout,jjout) , zzjv(jiout,jjout) ) * rad )                                                )
!      anginY = ATAN2( ( zglamvin( zziv  (jiout,jjout) , zzjvm1(jiout,jjout) ) - zglamvin( zziv  (jiout,jjout) , zzjvp1(jiout,jjout)) ) &
!      &                * cos( gphivin( zziv(jiout,jjout) , zzjv(jiout,jjout) ) * rad )                                                , &
!      &                   gphivin( zziv  (jiout,jjout) , zzjvp1(jiout,jjout) ) -  gphivin( zziv  (jiout,jjout) , zzjvm1(jiout,jjout))   )
  anginY = ATAN2(    gphivin( zziv(jiout,jjout) , zzjvp1(jiout,jjout) ) -  gphivin( zziv(jiout,jjout) , zzjvm1(jiout,jjout)) , &
  &                ( zglamvin( zziv(jiout,jjout) , zzjvp1(jiout,jjout) ) - zglamvin( zziv(jiout,jjout) , zzjvm1(jiout,jjout)) ) &
  &                * cos( gphivin( zziv(jiout,jjout) , zzjv(jiout,jjout) ) * rad )                                                )

  !-- local angle between the two grids :
  angleXv(jiout,jjout) = angoutXv(jiout,jjout) - anginX
  angleYv(jiout,jjout) = angoutYt(jiout,jjout) - anginY
  if ( abs(angleXv(jiout,jjout)/rad) .gt. 45.0 .or. abs(angleYv(jiout,jjout)/rad) .gt. 45.0 ) then
    write(*,*) '@@@@@@ WARNING : angle between 2 grids > 45 deg at point of the bdy :', jiout, jjout
  endif     

  !------------------------------------------------------------
  !   coordinates of closest points along X and weights
  !   ( take 2 or 3 points according to mesh size )
  !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
  if      ( e1vout(jiout,jjout) .gt. 50*e1vin(zziv(jiout,jjout),zzjv(jiout,jjout))*cos(angleXv(jiout,jjout)*rad) ) then ! it should even be 3 here
    write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along X >>>>>>> stop'
    write(*,*) '(jiout,jjout) =', jiout,jjout
    write(*,*) 'angleXv =', angleXv(jiout,jjout)
    write(*,*) 'e1vout(jiout,jjout) =', e1vout(jiout,jjout)
    write(*,*) 'e1vin(zziv(jiout,jjout),zzjv(jiout,jjout))*cos(angleXv(jiout,jjout)*rad)=', e1vin(zziv(jiout,jjout),zzjv(jiout,jjout))*cos(angleXv(jiout,jjout)*rad)
    stop
  elseif  (   e1vout(jiout,jjout) .ge. e1vin(zziv(jiout,jjout),zzjv(jiout,jjout)) * cos(angleXv(jiout,jjout)*rad)  ) then
    !- use 3 points for interpolation
    ziWv(jiout,jjout)   = zzivm1(jiout,jjout) ! closest point westward
    ziCv(jiout,jjout)   = zziv  (jiout,jjout) ! closest point ( central )
    ziEv(jiout,jjout)   = zzivp1(jiout,jjout) ! closest point eastward
    wgCv(jiout,jjout)   = e1vin(zziv(jiout,jjout),zzjv(jiout,jjout))* cos(angleXv(jiout,jjout)*rad) !weight for central point
  elseif  (       zglamvout(jiout,jjout) .ge.  zglamvin(zziv  (jiout,jjout),zzjv(jiout,jjout))        )  then
    !- use 2 points for interpolation
    ziWv(jiout,jjout)   = zziv  (jiout,jjout) ! closest point westward
    ziCv(jiout,jjout)   = 1                   ! dummy
    ziEv(jiout,jjout)   = zzivp1(jiout,jjout) ! closest point eastward
    wgCv(jiout,jjout)   = 0.0                 ! weight for central point
  else
    !- use 2 points for interpolation
    ziWv(jiout,jjout)   = zzivm1(jiout,jjout) ! closest point westward
    ziCv(jiout,jjout)   = 1                   ! dummy
    ziEv(jiout,jjout)   = zziv  (jiout,jjout) ! closest point eastward
    wgCv(jiout,jjout)   = 0.0                 ! weight for central point
  endif
  zjWv(jiout,jjout) = zzjv  (jiout,jjout) !  "
  zjCv(jiout,jjout) = zzjv  (jiout,jjout) ! to be adapted if angles > 45 deg (??)
  zjEv(jiout,jjout) = zzjv  (jiout,jjout) !  "
  !- weight for westward point
  wgWv(jiout,jjout)   = MAX( 0.0,                                                                                         &
    &                     0.5 * e1vin(ziWv(jiout,jjout),zjWv(jiout,jjout)) * cos(angleXv(jiout,jjout)*rad)                &
    &                   - ra * rad * abs(   ( zglamvout(jiout,jjout) - zglamvin(ziWv(jiout,jjout),zjWv(jiout,jjout)) )     &
    &                                     * cos(  gphivout(jiout,jjout)*rad )                                           )  &
    &                   + 0.5 * e1vout(jiout,jjout)                                                                       )
  !- weight for eastward point
  wgEv(jiout,jjout)   = MAX( 0.0,                                                                                         &
    &                     0.5 * e1vin(ziEv(jiout,jjout),zjEv(jiout,jjout)) * cos(angleXv(jiout,jjout)*rad)                &
    &                   - ra * rad * abs(  ( zglamvout(jiout,jjout) - zglamvin(ziEv(jiout,jjout),zjEv(jiout,jjout)) )      &
    &                                     * cos(  gphivout(jiout,jjout)*rad )                                           )  &
    &                   + 0.5 * e1vout(jiout,jjout)                                                                       )

  !------------------------------------------------------------
  !   coordinates of closest points along Y and weights
  !   ( take 2 or 3 points according to mesh size )
  !   Made to have a conservative interpolation, i.e. weight=surface in common between 2 grids
  if      ( e2vout(jiout,jjout) .gt. 50*e2vin(zziv(jiout,jjout),zzjv(jiout,jjout))*cos(angleYv(jiout,jjout)*rad)  ) then ! it should even be 3 here
    write(*,*) '!@#$%^* You may need to adapt the script for finer global input grids along Y >>>>>>> stop'
    write(*,*) '(jiout,jjout) =', jiout,jjout
    write(*,*) 'angleYv =', angleYv(jiout,jjout)
    write(*,*) 'e2vout(jiout,jjout) =', e2vout(jiout,jjout)
    write(*,*) 'e2vin(zziv(jiout,jjout),zzjv(jiout,jjout))*cos(angleXv(jiout,jjout)*rad)=', e2vin(zziv(jiout,jjout),zzjv(jiout,jjout))*cos(angleYv(jiout,jjout)*rad)
    stop
  elseif  (   e2vout(jiout,jjout) .ge. e2vin(zziv(jiout,jjout),zzjv(jiout,jjout)) * cos(angleYv(jiout,jjout)*rad)  ) then
    !- use 3 points for interpolation
    zjSv(jiout,jjout)   = zzjvm1(jiout,jjout) ! closest point southward
    zjMv(jiout,jjout)   = zzjv  (jiout,jjout) ! closest point ( middle )
    zjNv(jiout,jjout)   = zzjvp1(jiout,jjout) ! closest point northward
    wgMv(jiout,jjout)   = e2vin(zziv(jiout,jjout),zzjv(jiout,jjout))* cos(angleYv(jiout,jjout)*rad) !weight for central point
  elseif  (       gphivout(jiout,jjout) .ge.  gphivin(zziv  (jiout,jjout),zzjv(jiout,jjout))        )  then
    !- use 2 points for interpolation
    zjSv(jiout,jjout)   = zzjv  (jiout,jjout) ! closest point southward
    zjMv(jiout,jjout)   = zzjv  (jiout,jjout) ! dummy
    zjNv(jiout,jjout)   = zzjvp1(jiout,jjout) ! closest point northward
    wgMv(jiout,jjout)   = 0.0                 ! weight for middle point
  else
    !- use 2 points for interpolation
    zjSv(jiout,jjout)   = zzjvm1(jiout,jjout) ! closest point southward
    zjMv(jiout,jjout)   = zzjv  (jiout,jjout) ! dummy
    zjNv(jiout,jjout)   = zzjv  (jiout,jjout) ! closest point northward
    wgMv(jiout,jjout)   = 0.0                 ! weight for middle point
  endif
  ziSv(jiout,jjout) = zziv  (jiout,jjout) ! "
  ziMv(jiout,jjout) = zziv  (jiout,jjout) ! to be adapted if angles > 45 deg (??)
  ziNv(jiout,jjout) = zziv  (jiout,jjout) ! "
  !- weight for southward point
  wgSv(jiout,jjout)   = MAX( 0.0,                                                                                  &
    &                     0.5 * e2vin(ziSv(jiout,jjout),zjSv(jiout,jjout)) * cos(angleYv(jiout,jjout)*rad)         &
    &                   - ra * rad * abs(  gphivout(jiout,jjout) - gphivin(ziSv(jiout,jjout),zjSv(jiout,jjout)) )   &
    &                   + 0.5 * e2vout(jiout,jjout)                                                                )
  !- weight for northward point
  wgNv(jiout,jjout)   = MAX( 0.0,                                                                                  &
    &                     0.5 * e2vin(ziNv(jiout,jjout),zjNv(jiout,jjout)) * cos(angleYv(jiout,jjout)*rad)         &
    &                   - ra * rad * abs(  gphivout(jiout,jjout) - gphivin(ziNv(jiout,jjout),zjNv(jiout,jjout)) )   &
    &                   + 0.5 * e2vout(jiout,jjout)                                                                )

  !----------------------------------------------------------------
  !  NO VERTICAL INTERPOLATION HERE (ASSUMED TO BE THE SAME GRID) !
  !----------------------------------------------------------------  

ENDDO !-- jjout
ENDDO !-- jiout

!==================================================================================
! 4- WRITE EVERYTHING NEEDED FOR INTERPOLATION IN A NETCDF FILE
!==================================================================================

write(*,*) ' '
write(*,*) ' Writing ', TRIM(file_coeff)

status = NF90_CREATE(TRIM(file_coeff),or(NF90_NOCLOBBER,NF90_64BIT_OFFSET),fidcoeff)
call erreur(status,.TRUE.,'create_OBC_file')                     

!-- File dimensions 
status = NF90_DEF_DIM(fidcoeff,"z",jpkout,dimID_z) ; call erreur(status,.TRUE.,"def_dimID_z")
status = NF90_DEF_DIM(fidcoeff,"y",jpjout,dimID_y) ; call erreur(status,.TRUE.,"def_dimID_y")
status = NF90_DEF_DIM(fidcoeff,"x",jpiout,dimID_x) ; call erreur(status,.TRUE.,"def_dimID_x")
                  
!-- Variables definition                            
status = NF90_DEF_VAR(fidcoeff,"glamt",NF90_FLOAT,(/dimID_x,dimID_y/),glamtout_ID)  ; call erreur(status,.TRUE.,"def_var_glamtout_ID")
status = NF90_DEF_VAR(fidcoeff,"gphit",NF90_FLOAT,(/dimID_x,dimID_y/),gphitout_ID)  ; call erreur(status,.TRUE.,"def_var_gphitout_ID")
status = NF90_DEF_VAR(fidcoeff,"glamu",NF90_FLOAT,(/dimID_x,dimID_y/),glamuout_ID)  ; call erreur(status,.TRUE.,"def_var_glamuout_ID")
status = NF90_DEF_VAR(fidcoeff,"gphiu",NF90_FLOAT,(/dimID_x,dimID_y/),gphiuout_ID)  ; call erreur(status,.TRUE.,"def_var_gphiuout_ID")
status = NF90_DEF_VAR(fidcoeff,"glamv",NF90_FLOAT,(/dimID_x,dimID_y/),glamvout_ID)  ; call erreur(status,.TRUE.,"def_var_glamvout_ID")
status = NF90_DEF_VAR(fidcoeff,"gphiv",NF90_FLOAT,(/dimID_x,dimID_y/),gphivout_ID)  ; call erreur(status,.TRUE.,"def_var_gphivout_ID")
!-
status = NF90_DEF_VAR(fidcoeff,"ziWt", NF90_INT  ,(/dimID_x,dimID_y/),ziWt_ID)    ; call erreur(status,.TRUE.,"def_var_ziWt_ID")
status = NF90_DEF_VAR(fidcoeff,"zjWt", NF90_INT  ,(/dimID_x,dimID_y/),zjWt_ID)    ; call erreur(status,.TRUE.,"def_var_zjWt_ID")
status = NF90_DEF_VAR(fidcoeff,"ziCt", NF90_INT  ,(/dimID_x,dimID_y/),ziCt_ID)    ; call erreur(status,.TRUE.,"def_var_ziCt_ID")
status = NF90_DEF_VAR(fidcoeff,"zjCt", NF90_INT  ,(/dimID_x,dimID_y/),zjCt_ID)    ; call erreur(status,.TRUE.,"def_var_zjCt_ID")
status = NF90_DEF_VAR(fidcoeff,"ziEt", NF90_INT  ,(/dimID_x,dimID_y/),ziEt_ID)    ; call erreur(status,.TRUE.,"def_var_ziEt_ID")
status = NF90_DEF_VAR(fidcoeff,"zjEt", NF90_INT  ,(/dimID_x,dimID_y/),zjEt_ID)    ; call erreur(status,.TRUE.,"def_var_zjEt_ID")
status = NF90_DEF_VAR(fidcoeff,"ziSt", NF90_INT  ,(/dimID_x,dimID_y/),ziSt_ID)    ; call erreur(status,.TRUE.,"def_var_ziSt_ID")
status = NF90_DEF_VAR(fidcoeff,"zjSt", NF90_INT  ,(/dimID_x,dimID_y/),zjSt_ID)    ; call erreur(status,.TRUE.,"def_var_zjSt_ID")
status = NF90_DEF_VAR(fidcoeff,"ziMt", NF90_INT  ,(/dimID_x,dimID_y/),ziMt_ID)    ; call erreur(status,.TRUE.,"def_var_ziMt_ID")
status = NF90_DEF_VAR(fidcoeff,"zjMt", NF90_INT  ,(/dimID_x,dimID_y/),zjMt_ID)    ; call erreur(status,.TRUE.,"def_var_zjMt_ID")
status = NF90_DEF_VAR(fidcoeff,"ziNt", NF90_INT  ,(/dimID_x,dimID_y/),ziNt_ID)    ; call erreur(status,.TRUE.,"def_var_ziNt_ID")
status = NF90_DEF_VAR(fidcoeff,"zjNt", NF90_INT  ,(/dimID_x,dimID_y/),zjNt_ID)    ; call erreur(status,.TRUE.,"def_var_zjNt_ID")
status = NF90_DEF_VAR(fidcoeff,"angXt",NF90_FLOAT,(/dimID_x,dimID_y/),angleXt_ID) ; call erreur(status,.TRUE.,"def_var_angleXt_ID")
status = NF90_DEF_VAR(fidcoeff,"angYt",NF90_FLOAT,(/dimID_x,dimID_y/),angleYt_ID) ; call erreur(status,.TRUE.,"def_var_angleYt_ID")
status = NF90_DEF_VAR(fidcoeff,"wgWt", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgWt_ID) ; call erreur(status,.TRUE.,"def_var_wgWt_ID")
status = NF90_DEF_VAR(fidcoeff,"wgCt", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgCt_ID) ; call erreur(status,.TRUE.,"def_var_wgCt_ID")
status = NF90_DEF_VAR(fidcoeff,"wgEt", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgEt_ID) ; call erreur(status,.TRUE.,"def_var_wgEt_ID")
status = NF90_DEF_VAR(fidcoeff,"wgSt", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgSt_ID) ; call erreur(status,.TRUE.,"def_var_wgSt_ID")
status = NF90_DEF_VAR(fidcoeff,"wgMt", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgMt_ID) ; call erreur(status,.TRUE.,"def_var_wgMt_ID")
status = NF90_DEF_VAR(fidcoeff,"wgNt", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgNt_ID) ; call erreur(status,.TRUE.,"def_var_wgNt_ID")
status = NF90_DEF_VAR(fidcoeff,"tmask",NF90_SHORT,(/dimID_x,dimID_y,dimID_z/),tmask_ID); call erreur(status,.TRUE.,"def_var_tmask_ID")
!-
status = NF90_DEF_VAR(fidcoeff,"ziWu", NF90_INT  ,(/dimID_x,dimID_y/),ziWu_ID)    ; call erreur(status,.TRUE.,"def_var_ziWu_ID")
status = NF90_DEF_VAR(fidcoeff,"zjWu", NF90_INT  ,(/dimID_x,dimID_y/),zjWu_ID)    ; call erreur(status,.TRUE.,"def_var_zjWu_ID")
status = NF90_DEF_VAR(fidcoeff,"ziCu", NF90_INT  ,(/dimID_x,dimID_y/),ziCu_ID)    ; call erreur(status,.TRUE.,"def_var_ziCu_ID")
status = NF90_DEF_VAR(fidcoeff,"zjCu", NF90_INT  ,(/dimID_x,dimID_y/),zjCu_ID)    ; call erreur(status,.TRUE.,"def_var_zjCu_ID")
status = NF90_DEF_VAR(fidcoeff,"ziEu", NF90_INT  ,(/dimID_x,dimID_y/),ziEu_ID)    ; call erreur(status,.TRUE.,"def_var_ziEu_ID")
status = NF90_DEF_VAR(fidcoeff,"zjEu", NF90_INT  ,(/dimID_x,dimID_y/),zjEu_ID)    ; call erreur(status,.TRUE.,"def_var_zjEu_ID")
status = NF90_DEF_VAR(fidcoeff,"ziSu", NF90_INT  ,(/dimID_x,dimID_y/),ziSu_ID)    ; call erreur(status,.TRUE.,"def_var_ziSu_ID")
status = NF90_DEF_VAR(fidcoeff,"zjSu", NF90_INT  ,(/dimID_x,dimID_y/),zjSu_ID)    ; call erreur(status,.TRUE.,"def_var_zjSu_ID")
status = NF90_DEF_VAR(fidcoeff,"ziMu", NF90_INT  ,(/dimID_x,dimID_y/),ziMu_ID)    ; call erreur(status,.TRUE.,"def_var_ziMu_ID")
status = NF90_DEF_VAR(fidcoeff,"zjMu", NF90_INT  ,(/dimID_x,dimID_y/),zjMu_ID)    ; call erreur(status,.TRUE.,"def_var_zjMu_ID")
status = NF90_DEF_VAR(fidcoeff,"ziNu", NF90_INT  ,(/dimID_x,dimID_y/),ziNu_ID)    ; call erreur(status,.TRUE.,"def_var_ziNu_ID")
status = NF90_DEF_VAR(fidcoeff,"zjNu", NF90_INT  ,(/dimID_x,dimID_y/),zjNu_ID)    ; call erreur(status,.TRUE.,"def_var_zjNu_ID")
status = NF90_DEF_VAR(fidcoeff,"angXu",NF90_FLOAT,(/dimID_x,dimID_y/),angleXu_ID) ; call erreur(status,.TRUE.,"def_var_angleXu_ID")
status = NF90_DEF_VAR(fidcoeff,"angYu",NF90_FLOAT,(/dimID_x,dimID_y/),angleYu_ID) ; call erreur(status,.TRUE.,"def_var_angleYu_ID")
status = NF90_DEF_VAR(fidcoeff,"wgWu", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgWu_ID) ; call erreur(status,.TRUE.,"def_var_wgWu_ID")
status = NF90_DEF_VAR(fidcoeff,"wgCu", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgCu_ID) ; call erreur(status,.TRUE.,"def_var_wgCu_ID")
status = NF90_DEF_VAR(fidcoeff,"wgEu", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgEu_ID) ; call erreur(status,.TRUE.,"def_var_wgEu_ID")
status = NF90_DEF_VAR(fidcoeff,"wgSu", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgSu_ID) ; call erreur(status,.TRUE.,"def_var_wgSu_ID")
status = NF90_DEF_VAR(fidcoeff,"wgMu", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgMu_ID) ; call erreur(status,.TRUE.,"def_var_wgMu_ID")
status = NF90_DEF_VAR(fidcoeff,"wgNu", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgNu_ID) ; call erreur(status,.TRUE.,"def_var_wgNu_ID")
status = NF90_DEF_VAR(fidcoeff,"umask",NF90_SHORT,(/dimID_x,dimID_y,dimID_z/),umask_ID); call erreur(status,.TRUE.,"def_var_umask_ID")
!-
status = NF90_DEF_VAR(fidcoeff,"ziWv", NF90_INT  ,(/dimID_x,dimID_y/),ziWv_ID)    ; call erreur(status,.TRUE.,"def_var_ziWv_ID")
status = NF90_DEF_VAR(fidcoeff,"zjWv", NF90_INT  ,(/dimID_x,dimID_y/),zjWv_ID)    ; call erreur(status,.TRUE.,"def_var_zjWv_ID")
status = NF90_DEF_VAR(fidcoeff,"ziCv", NF90_INT  ,(/dimID_x,dimID_y/),ziCv_ID)    ; call erreur(status,.TRUE.,"def_var_ziCv_ID")
status = NF90_DEF_VAR(fidcoeff,"zjCv", NF90_INT  ,(/dimID_x,dimID_y/),zjCv_ID)    ; call erreur(status,.TRUE.,"def_var_zjCv_ID")
status = NF90_DEF_VAR(fidcoeff,"ziEv", NF90_INT  ,(/dimID_x,dimID_y/),ziEv_ID)    ; call erreur(status,.TRUE.,"def_var_ziEv_ID")
status = NF90_DEF_VAR(fidcoeff,"zjEv", NF90_INT  ,(/dimID_x,dimID_y/),zjEv_ID)    ; call erreur(status,.TRUE.,"def_var_zjEv_ID")
status = NF90_DEF_VAR(fidcoeff,"ziSv", NF90_INT  ,(/dimID_x,dimID_y/),ziSv_ID)    ; call erreur(status,.TRUE.,"def_var_ziSv_ID")
status = NF90_DEF_VAR(fidcoeff,"zjSv", NF90_INT  ,(/dimID_x,dimID_y/),zjSv_ID)    ; call erreur(status,.TRUE.,"def_var_zjSv_ID")
status = NF90_DEF_VAR(fidcoeff,"ziMv", NF90_INT  ,(/dimID_x,dimID_y/),ziMv_ID)    ; call erreur(status,.TRUE.,"def_var_ziMv_ID")
status = NF90_DEF_VAR(fidcoeff,"zjMv", NF90_INT  ,(/dimID_x,dimID_y/),zjMv_ID)    ; call erreur(status,.TRUE.,"def_var_zjMv_ID")
status = NF90_DEF_VAR(fidcoeff,"ziNv", NF90_INT  ,(/dimID_x,dimID_y/),ziNv_ID)    ; call erreur(status,.TRUE.,"def_var_ziNv_ID")
status = NF90_DEF_VAR(fidcoeff,"zjNv", NF90_INT  ,(/dimID_x,dimID_y/),zjNv_ID)    ; call erreur(status,.TRUE.,"def_var_zjNv_ID")
status = NF90_DEF_VAR(fidcoeff,"angXv",NF90_FLOAT,(/dimID_x,dimID_y/),angleXv_ID) ; call erreur(status,.TRUE.,"def_var_angleXv_ID")
status = NF90_DEF_VAR(fidcoeff,"angYv",NF90_FLOAT,(/dimID_x,dimID_y/),angleYv_ID) ; call erreur(status,.TRUE.,"def_var_angleYv_ID")
status = NF90_DEF_VAR(fidcoeff,"wgWv", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgWv_ID) ; call erreur(status,.TRUE.,"def_var_wgWv_ID")
status = NF90_DEF_VAR(fidcoeff,"wgCv", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgCv_ID) ; call erreur(status,.TRUE.,"def_var_wgCv_ID")
status = NF90_DEF_VAR(fidcoeff,"wgEv", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgEv_ID) ; call erreur(status,.TRUE.,"def_var_wgEv_ID")
status = NF90_DEF_VAR(fidcoeff,"wgSv", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgSv_ID) ; call erreur(status,.TRUE.,"def_var_wgSv_ID")
status = NF90_DEF_VAR(fidcoeff,"wgMv", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgMv_ID) ; call erreur(status,.TRUE.,"def_var_wgMv_ID")
status = NF90_DEF_VAR(fidcoeff,"wgNv", NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),wgNv_ID) ; call erreur(status,.TRUE.,"def_var_wgNv_ID")
status = NF90_DEF_VAR(fidcoeff,"vmask",NF90_SHORT,(/dimID_x,dimID_y,dimID_z/),vmask_ID); call erreur(status,.TRUE.,"def_var_vmask_ID")

!-- Global attribute
status = NF90_PUT_ATT(fidcoeff,NF90_GLOBAL,"history","created using build_child_3D_intrp_coeff_from_ORCA025.f90") ; call erreur(status,.TRUE.,"att_global")

!-- End of definitions
status = NF90_ENDDEF(fidcoeff) ; call erreur(status,.TRUE.,"end_definition") 


!-- Values to put in each variable 
status = NF90_PUT_VAR(fidcoeff,glamtout_ID,glamtout)   ; call erreur(status,.TRUE.,"var_glamtout_ID")
status = NF90_PUT_VAR(fidcoeff,gphitout_ID,gphitout)   ; call erreur(status,.TRUE.,"var_gphitout_ID")
status = NF90_PUT_VAR(fidcoeff,glamuout_ID,glamuout)   ; call erreur(status,.TRUE.,"var_glamuout_ID")
status = NF90_PUT_VAR(fidcoeff,gphiuout_ID,gphiuout)   ; call erreur(status,.TRUE.,"var_gphiuout_ID")
status = NF90_PUT_VAR(fidcoeff,glamvout_ID,glamvout)   ; call erreur(status,.TRUE.,"var_glamvout_ID")
status = NF90_PUT_VAR(fidcoeff,gphivout_ID,gphivout)   ; call erreur(status,.TRUE.,"var_gphivout_ID")
!-
status = NF90_PUT_VAR(fidcoeff,ziWt_ID,ziWt)         ; call erreur(status,.TRUE.,"var_ziWt_ID")
status = NF90_PUT_VAR(fidcoeff,zjWt_ID,zjWt)         ; call erreur(status,.TRUE.,"var_zjWt_ID")
status = NF90_PUT_VAR(fidcoeff,ziCt_ID,ziCt)         ; call erreur(status,.TRUE.,"var_ziCt_ID")
status = NF90_PUT_VAR(fidcoeff,zjCt_ID,zjCt)         ; call erreur(status,.TRUE.,"var_zjCt_ID")
status = NF90_PUT_VAR(fidcoeff,ziEt_ID,ziEt)         ; call erreur(status,.TRUE.,"var_ziEt_ID")
status = NF90_PUT_VAR(fidcoeff,zjEt_ID,zjEt)         ; call erreur(status,.TRUE.,"var_zjEt_ID")
status = NF90_PUT_VAR(fidcoeff,ziSt_ID,ziSt)         ; call erreur(status,.TRUE.,"var_ziSt_ID")
status = NF90_PUT_VAR(fidcoeff,zjSt_ID,zjSt)         ; call erreur(status,.TRUE.,"var_zjSt_ID")
status = NF90_PUT_VAR(fidcoeff,ziMt_ID,ziMt)         ; call erreur(status,.TRUE.,"var_ziMt_ID")
status = NF90_PUT_VAR(fidcoeff,zjMt_ID,zjMt)         ; call erreur(status,.TRUE.,"var_zjMt_ID")
status = NF90_PUT_VAR(fidcoeff,ziNt_ID,ziNt)         ; call erreur(status,.TRUE.,"var_ziNt_ID")
status = NF90_PUT_VAR(fidcoeff,zjNt_ID,zjNt)         ; call erreur(status,.TRUE.,"var_zjNt_ID")
status = NF90_PUT_VAR(fidcoeff,wgWt_ID,wgWt)         ; call erreur(status,.TRUE.,"var_wgWt_ID")
status = NF90_PUT_VAR(fidcoeff,wgCt_ID,wgCt)         ; call erreur(status,.TRUE.,"var_wgCt_ID")
status = NF90_PUT_VAR(fidcoeff,wgEt_ID,wgEt)         ; call erreur(status,.TRUE.,"var_wgEt_ID")
status = NF90_PUT_VAR(fidcoeff,wgSt_ID,wgSt)         ; call erreur(status,.TRUE.,"var_wgSt_ID")
status = NF90_PUT_VAR(fidcoeff,wgMt_ID,wgMt)         ; call erreur(status,.TRUE.,"var_wgMt_ID")
status = NF90_PUT_VAR(fidcoeff,wgNt_ID,wgNt)         ; call erreur(status,.TRUE.,"var_wgNt_ID")
status = NF90_PUT_VAR(fidcoeff,angleXt_ID,angleXt)   ; call erreur(status,.TRUE.,"var_angleXt_ID")
status = NF90_PUT_VAR(fidcoeff,angleYt_ID,angleYt)   ; call erreur(status,.TRUE.,"var_angleYt_ID")
status = NF90_PUT_VAR(fidcoeff,tmask_ID,tmaskout)    ; call erreur(status,.TRUE.,"var_tmask_ID")
!-
status = NF90_PUT_VAR(fidcoeff,ziWu_ID,ziWu)         ; call erreur(status,.TRUE.,"var_ziWu_ID")
status = NF90_PUT_VAR(fidcoeff,zjWu_ID,zjWu)         ; call erreur(status,.TRUE.,"var_zjWu_ID")
status = NF90_PUT_VAR(fidcoeff,ziCu_ID,ziCu)         ; call erreur(status,.TRUE.,"var_ziCu_ID")
status = NF90_PUT_VAR(fidcoeff,zjCu_ID,zjCu)         ; call erreur(status,.TRUE.,"var_zjCu_ID")
status = NF90_PUT_VAR(fidcoeff,ziEu_ID,ziEu)         ; call erreur(status,.TRUE.,"var_ziEu_ID")
status = NF90_PUT_VAR(fidcoeff,zjEu_ID,zjEu)         ; call erreur(status,.TRUE.,"var_zjEu_ID")
status = NF90_PUT_VAR(fidcoeff,ziSu_ID,ziSu)         ; call erreur(status,.TRUE.,"var_ziSu_ID")
status = NF90_PUT_VAR(fidcoeff,zjSu_ID,zjSu)         ; call erreur(status,.TRUE.,"var_zjSu_ID")
status = NF90_PUT_VAR(fidcoeff,ziMu_ID,ziMu)         ; call erreur(status,.TRUE.,"var_ziMu_ID")
status = NF90_PUT_VAR(fidcoeff,zjMu_ID,zjMu)         ; call erreur(status,.TRUE.,"var_zjMu_ID")
status = NF90_PUT_VAR(fidcoeff,ziNu_ID,ziNu)         ; call erreur(status,.TRUE.,"var_ziNu_ID")
status = NF90_PUT_VAR(fidcoeff,zjNu_ID,zjNu)         ; call erreur(status,.TRUE.,"var_zjNu_ID")
status = NF90_PUT_VAR(fidcoeff,wgWu_ID,wgWu)         ; call erreur(status,.TRUE.,"var_wgWu_ID")
status = NF90_PUT_VAR(fidcoeff,wgCu_ID,wgCu)         ; call erreur(status,.TRUE.,"var_wgCu_ID")
status = NF90_PUT_VAR(fidcoeff,wgEu_ID,wgEu)         ; call erreur(status,.TRUE.,"var_wgEu_ID")
status = NF90_PUT_VAR(fidcoeff,wgSu_ID,wgSu)         ; call erreur(status,.TRUE.,"var_wgSu_ID")
status = NF90_PUT_VAR(fidcoeff,wgMu_ID,wgMu)         ; call erreur(status,.TRUE.,"var_wgMu_ID")
status = NF90_PUT_VAR(fidcoeff,wgNu_ID,wgNu)         ; call erreur(status,.TRUE.,"var_wgNu_ID")
status = NF90_PUT_VAR(fidcoeff,angleXu_ID,angleXu)   ; call erreur(status,.TRUE.,"var_angleXu_ID")
status = NF90_PUT_VAR(fidcoeff,angleYu_ID,angleYu)   ; call erreur(status,.TRUE.,"var_angleYu_ID")
status = NF90_PUT_VAR(fidcoeff,umask_ID,umaskout)    ; call erreur(status,.TRUE.,"var_umask_ID")
!-
status = NF90_PUT_VAR(fidcoeff,ziWv_ID,ziWv)         ; call erreur(status,.TRUE.,"var_ziWv_ID")
status = NF90_PUT_VAR(fidcoeff,zjWv_ID,zjWv)         ; call erreur(status,.TRUE.,"var_zjWv_ID")
status = NF90_PUT_VAR(fidcoeff,ziCv_ID,ziCv)         ; call erreur(status,.TRUE.,"var_ziCv_ID")
status = NF90_PUT_VAR(fidcoeff,zjCv_ID,zjCv)         ; call erreur(status,.TRUE.,"var_zjCv_ID")
status = NF90_PUT_VAR(fidcoeff,ziEv_ID,ziEv)         ; call erreur(status,.TRUE.,"var_ziEv_ID")
status = NF90_PUT_VAR(fidcoeff,zjEv_ID,zjEv)         ; call erreur(status,.TRUE.,"var_zjEv_ID")
status = NF90_PUT_VAR(fidcoeff,ziSv_ID,ziSv)         ; call erreur(status,.TRUE.,"var_ziSv_ID")
status = NF90_PUT_VAR(fidcoeff,zjSv_ID,zjSv)         ; call erreur(status,.TRUE.,"var_zjSv_ID")
status = NF90_PUT_VAR(fidcoeff,ziMv_ID,ziMv)         ; call erreur(status,.TRUE.,"var_ziMv_ID")
status = NF90_PUT_VAR(fidcoeff,zjMv_ID,zjMv)         ; call erreur(status,.TRUE.,"var_zjMv_ID")
status = NF90_PUT_VAR(fidcoeff,ziNv_ID,ziNv)         ; call erreur(status,.TRUE.,"var_ziNv_ID")
status = NF90_PUT_VAR(fidcoeff,zjNv_ID,zjNv)         ; call erreur(status,.TRUE.,"var_zjNv_ID")
status = NF90_PUT_VAR(fidcoeff,wgWv_ID,wgWv)         ; call erreur(status,.TRUE.,"var_wgWv_ID")
status = NF90_PUT_VAR(fidcoeff,wgCv_ID,wgCv)         ; call erreur(status,.TRUE.,"var_wgCv_ID")
status = NF90_PUT_VAR(fidcoeff,wgEv_ID,wgEv)         ; call erreur(status,.TRUE.,"var_wgEv_ID")
status = NF90_PUT_VAR(fidcoeff,wgSv_ID,wgSv)         ; call erreur(status,.TRUE.,"var_wgSv_ID")
status = NF90_PUT_VAR(fidcoeff,wgMv_ID,wgMv)         ; call erreur(status,.TRUE.,"var_wgMv_ID")
status = NF90_PUT_VAR(fidcoeff,wgNv_ID,wgNv)         ; call erreur(status,.TRUE.,"var_wgNv_ID")
status = NF90_PUT_VAR(fidcoeff,angleXv_ID,angleXv)   ; call erreur(status,.TRUE.,"var_angleXv_ID")
status = NF90_PUT_VAR(fidcoeff,angleYv_ID,angleYv)   ; call erreur(status,.TRUE.,"var_angleYv_ID")
status = NF90_PUT_VAR(fidcoeff,vmask_ID,vmaskout)    ; call erreur(status,.TRUE.,"var_vmask_ID")

!-- End of writing                           
status = NF90_CLOSE(fidcoeff) ; call erreur(status,.TRUE.,"end_writing_coef")


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
    WRITE(*,*) 'ERREUR: ', iret
    message=NF90_STRERROR(iret)
    WRITE(*,*) 'CA VEUT DIRE:',TRIM(message)
    IF ( lstop ) STOP
  ENDIF
  !
END SUBROUTINE erreur
