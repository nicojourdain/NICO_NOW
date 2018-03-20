program modif                                         
     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! used to create flxatmos_xxxx.nc with zeros everywhere
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                 
USE netcdf                                            
                                      
IMPLICIT NONE                                         

!----- namelist parameters -----
namelist /parent/ conf_par, perio, max_dom, feedback, idateline, file_par_coord, file_eff_land
namelist /child/ conf_child, file_child_coord, file_child_extend
INTEGER                                  :: max_dom, feedback, perio, idateline
CHARACTER(LEN=150)                       :: file_par_coord, file_eff_land, file_child_coord, file_child_extend
CHARACTER(LEN=50)                        :: conf_par, conf_child

!-------------------------------
                                      
INTEGER                           :: status, dimID_y, dimID_x, my, mx, WRF_d01_EXT_d02_SURF_NET_NON_SOLAR_ID, WRF_d01_EXT_d02_SURF_NET_SOLAR_ID, &
&                                    WRF_d01_EXT_d02_EVAP_PRECIP_ID, WRF_d01_EXT_d02_TAUY_ID, WRF_d01_EXT_d02_TAUX_ID, WRF_d01_EXT_d02_TAUMOD_ID,&
&                                    fidM, msouth_north_stag, mwest_east_stag, dimID_south_north, dimID_west_east, fidAtm
CHARACTER(LEN=150)                :: file_out                     
REAL*8,ALLOCATABLE,DIMENSION(:,:) :: WRF_d01_EXT_d02_SURF_NET_NON_SOLAR, WRF_d01_EXT_d02_SURF_NET_SOLAR, WRF_d01_EXT_d02_EVAP_PRECIP,            &
&                                    WRF_d01_EXT_d02_TAUY, WRF_d01_EXT_d02_TAUX, WRF_d01_EXT_d02_TAUMOD 

!=======================================================================================
! 0- Initializations
!=======================================================================================

!- read namelist values
OPEN (UNIT=1, FILE='namelist_precpl' )
READ (UNIT=1, NML=parent)
if ( max_dom .gt. 1 ) READ (UNIT=1, NML=child)
CLOSE(1)

if ( max_dom .gt. 1 ) then
  write(file_out,121) TRIM(conf_par), TRIM(conf_child)
  121 FORMAT('flxat_Ad01_to_Od02_',a,'_',a,'.nc')
else
  write(*,*) '~!@#$%^* You should not use this script if max_dom = 1  >>>>>  stop !!'
  stop
endif  

  write(*,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
  write(*,*) ' create_OASIS_flxatmos_Ad01_Od02 :                   '
  write(*,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                     '
  write(*,*) '                                                     '
                                   
!=================================================================================
! 1- Read dimensions of atmospheric grid
!=================================================================================     

write(*,*) ' '
write(*,*) ' Reading dimensions in ', TRIM(file_par_coord)

status = NF90_OPEN(TRIM(file_par_coord),0,fidAtm)
call erreur(status,.TRUE.,"read")

!- Lecture des ID des dimensions qui nous interessent
status = NF90_INQ_DIMID(fidAtm,"south_north_stag",dimID_south_north)
call erreur(status,.TRUE.,"inq_dimID_south_north")
status = NF90_INQ_DIMID(fidAtm,"west_east_stag",dimID_west_east)
call erreur(status,.TRUE.,"inq_dimID_west_east")

!- Lecture des valeurs des dimensions qui nous interessent
status = NF90_INQUIRE_DIMENSION(fidAtm,dimID_south_north,len=msouth_north_stag)
call erreur(status,.TRUE.,"inq_dim_south_north")
status = NF90_INQUIRE_DIMENSION(fidAtm,dimID_west_east,len=mwest_east_stag)
call erreur(status,.TRUE.,"inq_dim_west_east")

mx=mwest_east_stag
my=msouth_north_stag

write(*,*) '     mx = ', mx
write(*,*) '     my = ', my

!=======================================================================================
! 2- Create flxatmos file
!=======================================================================================
      
!-- Allocation of arrays : 
ALLOCATE(  WRF_d01_EXT_d02_SURF_NET_NON_SOLAR (mx,my)  ) 
ALLOCATE(  WRF_d01_EXT_d02_SURF_NET_SOLAR     (mx,my)  ) 
ALLOCATE(  WRF_d01_EXT_d02_EVAP_PRECIP        (mx,my)  ) 
ALLOCATE(  WRF_d01_EXT_d02_TAUY               (mx,my)  ) 
ALLOCATE(  WRF_d01_EXT_d02_TAUX               (mx,my)  ) 
ALLOCATE(  WRF_d01_EXT_d02_TAUMOD             (mx,my)  ) 
                                             
WRF_d01_EXT_d02_SURF_NET_NON_SOLAR (:,:) = 0.0
WRF_d01_EXT_d02_SURF_NET_SOLAR     (:,:) = 0.0
WRF_d01_EXT_d02_EVAP_PRECIP        (:,:) = 0.0
WRF_d01_EXT_d02_TAUY               (:,:) = 0.0
WRF_d01_EXT_d02_TAUX               (:,:) = 0.0
WRF_d01_EXT_d02_TAUMOD             (:,:) = 0.0
 
!-- Writing new netcdf file :                                   
                                              
status = NF90_CREATE(TRIM(file_out),NF90_NOCLOBBER,fidM)
call erreur(status,.TRUE.,'create flxatmos')                     
                                                
!Definition des dimensions du fichiers                  
status = NF90_DEF_DIM(fidM,"y",my,dimID_y)
call erreur(status,.TRUE.,"def_dimID_y")
status = NF90_DEF_DIM(fidM,"x",mx,dimID_x)
call erreur(status,.TRUE.,"def_dimID_x")
                                              
!Definition des variables                             
status = NF90_DEF_VAR(fidM,"WRF_d01_EXT_d02_SURF_NET_NON-SOLAR",NF90_DOUBLE,(/dimID_x,dimID_y/),WRF_d01_EXT_d02_SURF_NET_NON_SOLAR_ID)
call erreur(status,.TRUE.,"def_var_WRF_d01_EXT_d02_SURF_NET_NON-SOLAR_ID")
status = NF90_DEF_VAR(fidM,"WRF_d01_EXT_d02_SURF_NET_SOLAR",NF90_DOUBLE,(/dimID_x,dimID_y/),WRF_d01_EXT_d02_SURF_NET_SOLAR_ID)
call erreur(status,.TRUE.,"def_var_WRF_d01_EXT_d02_SURF_NET_SOLAR_ID")
status = NF90_DEF_VAR(fidM,"WRF_d01_EXT_d02_EVAP-PRECIP",NF90_DOUBLE,(/dimID_x,dimID_y/),WRF_d01_EXT_d02_EVAP_PRECIP_ID)
call erreur(status,.TRUE.,"def_var_WRF_d01_EXT_d02_EVAP-PRECIP_ID")
status = NF90_DEF_VAR(fidM,"WRF_d01_EXT_d02_TAUY",NF90_DOUBLE,(/dimID_x,dimID_y/),WRF_d01_EXT_d02_TAUY_ID)
call erreur(status,.TRUE.,"def_var_WRF_d01_EXT_d02_TAUY_ID")
status = NF90_DEF_VAR(fidM,"WRF_d01_EXT_d02_TAUX",NF90_DOUBLE,(/dimID_x,dimID_y/),WRF_d01_EXT_d02_TAUX_ID)
call erreur(status,.TRUE.,"def_var_WRF_d01_EXT_d02_TAUX_ID")
status = NF90_DEF_VAR(fidM,"WRF_d01_EXT_d02_TAUMOD",NF90_DOUBLE,(/dimID_x,dimID_y/),WRF_d01_EXT_d02_TAUMOD_ID)
call erreur(status,.TRUE.,"def_var_WRF_d01_EXT_d02_TAUMOD_ID")
                   
! Attributs globaux       :
status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Created using create_OASIS_flxatmos_Ad01_Od02.f90")
call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
                                      
!Fin des definitions                          
status = NF90_ENDDEF(fidM)                   
call erreur(status,.TRUE.,"fin_definition") 
                                      
!Valeurs prises par les variables :           
status = NF90_PUT_VAR(fidM,WRF_d01_EXT_d02_SURF_NET_NON_SOLAR_ID,WRF_d01_EXT_d02_SURF_NET_NON_SOLAR)
call erreur(status,.TRUE.,"var_WRF_d01_EXT_d02_SURF_NET_NON_SOLAR_ID")
status = NF90_PUT_VAR(fidM,WRF_d01_EXT_d02_SURF_NET_SOLAR_ID,WRF_d01_EXT_d02_SURF_NET_SOLAR)
call erreur(status,.TRUE.,"var_WRF_d01_EXT_d02_SURF_NET_SOLAR_ID")
status = NF90_PUT_VAR(fidM,WRF_d01_EXT_d02_EVAP_PRECIP_ID,WRF_d01_EXT_d02_EVAP_PRECIP)
call erreur(status,.TRUE.,"var_WRF_d01_EXT_d02_EVAP_PRECIP_ID")
status = NF90_PUT_VAR(fidM,WRF_d01_EXT_d02_TAUY_ID,WRF_d01_EXT_d02_TAUY)
call erreur(status,.TRUE.,"var_WRF_d01_EXT_d02_TAUY_ID")
status = NF90_PUT_VAR(fidM,WRF_d01_EXT_d02_TAUX_ID,WRF_d01_EXT_d02_TAUX)
call erreur(status,.TRUE.,"var_WRF_d01_EXT_d02_TAUX_ID")
status = NF90_PUT_VAR(fidM,WRF_d01_EXT_d02_TAUMOD_ID,WRF_d01_EXT_d02_TAUMOD)
call erreur(status,.TRUE.,"var_WRF_d01_EXT_d02_TAUMOD_ID")
                                      
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
