program modif                                         
                                            
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Used to create sstoc*.nc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          
USE netcdf                                            
                                                      
IMPLICIT NONE                                         

!----- namelist parameters -----
namelist /parent/ conf_par, perio, max_dom, feedback, idateline, file_par_coord, file_eff_land
namelist /timectl/ yeari, yearf
namelist /child/ conf_child, file_child_coord, file_child_extend
INTEGER                                  :: max_dom, feedback, perio, idateline, yeari, yearf
CHARACTER(LEN=150)                       :: file_par_coord, file_eff_land, file_child_coord, file_child_extend
CHARACTER(LEN=50)                        :: conf_par, conf_child
                                                      
!---------------------------------
INTEGER                                  :: fidA, status, dimID_x, dimID_y, dimID_z, dimID_time_counter, mx, my, mz, mtime_counter, &
&                                           nav_lon_ID, nav_lat_ID, deptht_ID, time_counter_ID, votemper_ID, fidM, SST_ID, UOCE_ID, &
&                                           VOCE_ID                                
CHARACTER(LEN=150)                       :: file_in, file_out                     
REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:)    :: votemper     

!=======================================================================================
! 0- Initializations
!=======================================================================================

!- read namelist values
OPEN (UNIT=1, FILE='namelist_precpl' )
READ (UNIT=1, NML=parent)
READ (UNIT=1, NML=timectl)
if ( max_dom .gt. 1 ) READ (UNIT=1, NML=child)
CLOSE(1)

write(file_in,120) TRIM(conf_par), yeari
120 FORMAT('1_dta_temp_',a,'_y',i4.4,'m01.nc')

if ( max_dom .gt. 1 ) then
  write(file_out,121) TRIM(conf_par), TRIM(conf_child)
  121 FORMAT('sstoc_Od02_to_Ad02_',a,'_',a,'.nc')
else
  write(*,*) '~!@#$%^* You should not use this script if max_dom = 1  >>>>>  stop !!'
  stop
endif

  write(*,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
  write(*,*) ' create_OASIS_sstocean_Ad02_Od02 :                   '
  write(*,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                     '
  write(*,*) '                                                     '
                             
!===========================================================================================
! 1- Read temperature initial state
!===========================================================================================
                                                           
status = NF90_OPEN(TRIM(file_in),0,fidA)          
call erreur(status,.TRUE.,"read initial state") 
                                                    
status = NF90_INQ_DIMID(fidA,"x",dimID_x)
call erreur(status,.TRUE.,"inq_dimID_x")
status = NF90_INQ_DIMID(fidA,"y",dimID_y)
call erreur(status,.TRUE.,"inq_dimID_y")
status = NF90_INQ_DIMID(fidA,"z",dimID_z)
call erreur(status,.TRUE.,"inq_dimID_z")
status = NF90_INQ_DIMID(fidA,"time_counter",dimID_time_counter)
call erreur(status,.TRUE.,"inq_dimID_time_counter")
                                                        
status = NF90_INQUIRE_DIMENSION(fidA,dimID_x,len=mx)
call erreur(status,.TRUE.,"inq_dim_x")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_y,len=my)
call erreur(status,.TRUE.,"inq_dim_y")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_z,len=mz)
call erreur(status,.TRUE.,"inq_dim_z")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_time_counter,len=mtime_counter)
call erreur(status,.TRUE.,"inq_dim_time_counter")
                      
ALLOCATE(  votemper(mx,my,mz,mtime_counter)  ) 
                          
status = NF90_INQ_VARID(fidA,"votemper",votemper_ID)
call erreur(status,.TRUE.,"inq_votemper_ID")
                                                       
status = NF90_GET_VAR(fidA,votemper_ID,votemper)
call erreur(status,.TRUE.,"getvar_votemper")
                                               
status = NF90_CLOSE(fidA)                      
call erreur(status,.TRUE.,"fin_lecture")     

write(*,*) '     mx = ', mx
write(*,*) '     my = ', my

!===========================================================================================
! 2- Write sstocean file
!===========================================================================================
                                                              
write(*,*) ' '
write(*,*) ' Writing ', TRIM(file_out)

status = NF90_CREATE(TRIM(file_out),or(NF90_NOCLOBBER,NF90_64BIT_OFFSET),fidM)
call erreur(status,.TRUE.,'create sstoc file')                     
                                                        
status = NF90_DEF_DIM(fidM,"x",mx,dimID_x) ;  call erreur(status,.TRUE.,"def_dimID_x")
status = NF90_DEF_DIM(fidM,"y",my,dimID_y) ;  call erreur(status,.TRUE.,"def_dimID_y")

status = NF90_DEF_VAR(fidM,"1_grdatm02_O_SSTSST",NF90_FLOAT,(/dimID_x,dimID_y/),SST_ID)
call erreur(status,.TRUE.,"def_var_SST_ID")
status = NF90_DEF_VAR(fidM,"1_grdatm02_O_OCurx1",NF90_FLOAT,(/dimID_x,dimID_y/),UOCE_ID)
call erreur(status,.TRUE.,"def_var_UOCE_ID")
status = NF90_DEF_VAR(fidM,"1_grdatm02_O_OCury1",NF90_FLOAT,(/dimID_x,dimID_y/),VOCE_ID)
call erreur(status,.TRUE.,"def_var_VOCE_ID")

status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Created using create_OASIS_sstocean_Ad02_Od02.f90")
call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
                                              
status = NF90_ENDDEF(fidM) ; call erreur(status,.TRUE.,"fin_definition") 
      
status = NF90_PUT_VAR(fidM,SST_ID, votemper(:,:,1,1)    ) ; call erreur(status,.TRUE.,"var_SST_ID")
status = NF90_PUT_VAR(fidM,UOCE_ID,votemper(:,:,1,1)*0.0) ; call erreur(status,.TRUE.,"var_UOCE_ID")
status = NF90_PUT_VAR(fidM,VOCE_ID,votemper(:,:,1,1)*0.0) ; call erreur(status,.TRUE.,"var_VOCE_ID")
                                              
status = NF90_CLOSE(fidM) ; call erreur(status,.TRUE.,"final")         

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
