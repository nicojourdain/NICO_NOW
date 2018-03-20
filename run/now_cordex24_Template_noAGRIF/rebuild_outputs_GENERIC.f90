!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! N. Jourdain, CCRC-UNSW, August 2014
!
! This prgm rebuild NEMO's outputs into single files when the option
! "multiple_file" is choosen in iodef.xml (instead of "single_file").
!
! NB: assumes that domains are cut along the Y-direction only
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program modif                                         
                                                      
USE netcdf                                            
                                                      
IMPLICIT NONE                                         
                                                      
INTEGER :: fidA, status, dimID_time_counter, dimID_y, dimID_x, dimID_depth, mtime_counter, mdepth, time_average_ID, time_maximum_ID, time_minimum_ID, nav_lon_ID, nav_lat_ID, depth_ID, fidM, nproc, nproc_xios, mxout, myout, GLOB_DOMAIN_number_total, igridZ, imin, imax, jmin, jmax, kfile, kl, stat_tmin, stat_tmax

INTEGER,ALLOCATABLE,DIMENSION(:) :: mx, my

CHARACTER(LEN=150) :: file_in, file_out, GLOB_file_name, GLOB_description, GLOB_conventions, GLOB_production, GLOB_timeStamp

CHARACTER(LEN=30) :: aa1, aa2, aa3, aa4, aa5, time_average_str, time_maximum_str, time_minimum_str

CHARACTER(LEN=2) :: freq

REAL*8,ALLOCATABLE,DIMENSION(:) :: time_average, time_maximum, time_minimum

REAL*4,ALLOCATABLE,DIMENSION(:,:) :: nav_lon, nav_lat, nav_lon_out, nav_lat_out

REAL*4,ALLOCATABLE,DIMENSION(:) :: depth

INTEGER, DIMENSION(2) :: GLOB_DOMAIN_position_first, GLOB_DOMAIN_position_last, GLOB_DOMAIN_size_global

!----- needed to rebuild each optional 2D variable defined in iodef.xml -----

INTEGER :: mld_dt02_ID, mldr10_3_ID, zos_ID, sos_ID, tossq_ID, tos_min_ID, tos_max_ID, tos_ID, tauuo_ID, tauvo_ID, uos_ID, vos_ID, wfo_ID, rsntds_ID, tohfls_ID, taum_ID, x20d_ID, mldkz5_ID, mldr10_1_ID, topthdep_ID, pycndep_ID, tinv_ID, depti_ID, blt_ID

INTEGER :: stat_mld_dt02, stat_mldr10_3, stat_zos, stat_sos, stat_tossq, stat_tos_min, stat_tos_max, stat_tos, stat_tauuo, stat_tauvo, stat_uos, stat_vos, stat_wfo, stat_rsntds, stat_tohfls, stat_taum, stat_x20d, stat_mldkz5, stat_mldr10_1, stat_topthdep, stat_pycndep, stat_tinv, stat_depti, stat_blt

CHARACTER(LEN=100) :: nam_mld_dt02, nam_mldr10_3, nam_zos, nam_sos, nam_tossq, nam_tos_min, nam_tos_max, nam_tos, nam_tauuo, nam_tauvo, nam_uos, nam_vos, nam_wfo, nam_rsntds, nam_tohfls, nam_taum, nam_x20d, nam_mldkz5, nam_mldr10_1, nam_topthdep, nam_pycndep, nam_tinv, nam_depti, nam_blt

CHARACTER(LEN=100) :: units_mld_dt02, units_mldr10_3, units_zos, units_sos, units_tossq, units_tos_min, units_tos_max, units_tos, units_tauuo, units_tauvo, units_uos, units_vos, units_wfo, units_rsntds, units_tohfls, units_taum, units_x20d, units_mldkz5, units_mldr10_1, units_topthdep, units_pycndep, units_tinv, units_depti, units_blt

CHARACTER(LEN=100) :: coord_mld_dt02, coord_mldr10_3, coord_zos, coord_sos, coord_tossq, coord_tos_min, coord_tos_max, coord_tos, coord_tauuo, coord_tauvo, coord_uos, coord_vos, coord_wfo, coord_rsntds, coord_tohfls, coord_taum, coord_x20d, coord_mldkz5, coord_mldr10_1, coord_topthdep, coord_pycndep, coord_tinv, coord_depti, coord_blt
                                     
REAL*4,ALLOCATABLE,DIMENSION(:,:,:) :: mld_dt02, mldr10_3, zos, sos, tossq, tos_min, tos_max, tos, tauuo, tauvo, uos, vos, wfo, rsntds, tohfls, taum, x20d, mldkz5, mldr10_1, topthdep, pycndep, tinv, depti, blt

REAL*4,ALLOCATABLE,DIMENSION(:,:,:) :: mld_dt02_out, mldr10_3_out, zos_out, sos_out, tossq_out, tos_min_out, tos_max_out, tos_out, tauuo_out, tauvo_out, uos_out, vos_out, wfo_out, rsntds_out, tohfls_out, taum_out, x20d_out, mldkz5_out, mldr10_1_out, topthdep_out, pycndep_out, tinv_out, depti_out, blt_out

!----- needed to rebuild each optional 3D variable defined in iodef.xml -----

INTEGER :: thetao_ID, so_ID, uo_ID, vo_ID, wo_ID

INTEGER :: stat_thetao, stat_so, stat_uo, stat_vo, stat_wo

CHARACTER(LEN=100) :: nam_thetao, nam_so, nam_uo, nam_vo, nam_wo

CHARACTER(LEN=100) :: units_thetao, units_so, units_uo, units_vo, units_wo

CHARACTER(LEN=100) :: coord_thetao, coord_so, coord_uo, coord_vo, coord_wo

REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:) :: thetao, so, uo, vo, wo

REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:) :: thetao_out, so_out, uo_out, vo_out, wo_out


!----------------------------------------------
! Find number of processors used by IO server:

open(unit=31,file='app.copy',status='old')
read(31,*) aa1, nproc, aa2, aa3
if ( TRIM(aa2) == 'xios_server.exe' .or. TRIM(aa3) == 'xios_server.exe' ) nproc_xios = nproc
close(31)
write(*,*) '  # Number of procs used by xios = ', nproc_xios                                                    

ALLOCATE( mx(nproc_xios), my(nproc_xios) )

do kfile=1,nproc_xios

  write(file_in,212) kfile-1
  212 FORMAT('pppppp_', i4.4, '.nc')

  !- Initializations of integers related to 2D variables (can be any value but zero):
  stat_mld_dt02  = 1 
  stat_mldr10_3  = 1 
  stat_zos       = 1 
  stat_sos       = 1 
  stat_tossq     = 1 
  stat_tos_min   = 1 
  stat_tos_max   = 1 
  stat_tos       = 1 
  stat_tauuo     = 1 
  stat_tauvo     = 1 
  stat_uos       = 1 
  stat_vos       = 1
  stat_wfo       = 1 
  stat_rsntds    = 1
  stat_tohfls    = 1
  stat_taum      = 1
  stat_x20d      = 1
  stat_mldkz5    = 1
  stat_mldr10_1  = 1
  stat_topthdep  = 1
  stat_pycndep   = 1
  stat_tinv      = 1
  stat_depti     = 1
  stat_blt       = 1
  !- Initializations of integers related to 3D variables (can be any value but zero):
  stat_thetao    = 1
  stat_so        = 1
  stat_uo        = 1
  stat_vo        = 1
  stat_wo        = 1 

  !---------------------------------------                   
  ! Read NEMO's output file :                                 
  
  write(*,*) '  # Name of XIOS file : ', TRIM(file_in)
                                                         
  status = NF90_OPEN(TRIM(file_in),0,fidA)          
  call erreur(status,.TRUE.,"read") 
                                                           
    !----- Read dimensions ID :
    status = NF90_INQ_DIMID(fidA,"time_counter",dimID_time_counter) ; call erreur(status,.TRUE.,"inq_dimID_time_counter")
    status = NF90_INQ_DIMID(fidA,"y",dimID_y)                       ; call erreur(status,.TRUE.,"inq_dimID_y")
    status = NF90_INQ_DIMID(fidA,"x",dimID_x)                       ; call erreur(status,.TRUE.,"inq_dimID_x")
    !- check for depth dimension :
    status = NF90_INQ_DIMID(fidA,"deptht",dimID_depth)
    if ( status .eq. 0 ) then   
      igridZ = 1 ! 3D gridT
    else
      status = NF90_INQ_DIMID(fidA,"depthu",dimID_depth)
      if ( status .eq. 0 ) then
        igridZ = 2 ! 3D gridU
      else
        status = NF90_INQ_DIMID(fidA,"depthv",dimID_depth)
        if ( status .eq. 0 ) then
          igridZ = 3 ! 3D gridV
        else
          status = NF90_INQ_DIMID(fidA,"depthw",dimID_depth)
          if ( status .eq. 0 ) then
            igridZ = 4 ! 3D gridW
          else
            igridZ = 0 ! 2D grid (no depth dimension)
          endif
        endif
      endif
    endif

    !----- Read dimension values
    status = NF90_INQUIRE_DIMENSION(fidA,dimID_time_counter,len=mtime_counter) ; call erreur(status,.TRUE.,"inq_dim_time_counter")
    status = NF90_INQUIRE_DIMENSION(fidA,dimID_y,len=my(kfile))                       ; call erreur(status,.TRUE.,"inq_dim_y")
    status = NF90_INQUIRE_DIMENSION(fidA,dimID_x,len=mx(kfile))                       ; call erreur(status,.TRUE.,"inq_dim_x")
    if ( igridZ .gt. 0 ) then
      status = NF90_INQUIRE_DIMENSION(fidA,dimID_depth,len=mdepth)             ; call erreur(status,.TRUE.,"inq_dim_depth")
      write(*,*) '  # dimensions :', mx(kfile), my(kfile), mdepth, mtime_counter
    else
      write(*,*) '  # dimensions :', mx(kfile), my(kfile), mtime_counter
    endif 
 
    ! First read compulsory variables :
    status = NF90_INQ_VARID(fidA,"time_average_1d",time_average_ID)
    if ( status .eq. 0 ) then
      freq = "1d" ! as in file name
      stat_tmax = NF90_INQ_VARID(fidA,"time_maximum_1d",time_maximum_ID)
      stat_tmin = NF90_INQ_VARID(fidA,"time_minimum_1d",time_minimum_ID)
    else
      status = NF90_INQ_VARID(fidA,"time_average_3d",time_average_ID)
      if ( status .eq. 0 ) then
        freq = "3d" ! as in file name
        stat_tmax = NF90_INQ_VARID(fidA,"time_maximum_3d",time_maximum_ID)
        stat_tmin = NF90_INQ_VARID(fidA,"time_minimum_3d",time_minimum_ID)
      else
        status = NF90_INQ_VARID(fidA,"time_average_5d",time_average_ID)
        if ( status .eq. 0 ) then
          freq = "5d" ! as in file name
          stat_tmax = NF90_INQ_VARID(fidA,"time_maximum_5d",time_maximum_ID)
          stat_tmin = NF90_INQ_VARID(fidA,"time_minimum_5d",time_minimum_ID)
        else
          status = NF90_INQ_VARID(fidA,"time_average_1h",time_average_ID)
          if ( status .eq. 0 ) then
            freq = "1h" ! as in file name
            stat_tmax = NF90_INQ_VARID(fidA,"time_maximum_1h",time_maximum_ID)
            stat_tmin = NF90_INQ_VARID(fidA,"time_minimum_1h",time_minimum_ID)
          else
            status = NF90_INQ_VARID(fidA,"time_average_6h",time_average_ID)
            if ( status .eq. 0 ) then
              freq = "6h" ! as in file name
              stat_tmin = NF90_INQ_VARID(fidA,"time_maximum_6h",time_maximum_ID)
              stat_tmax = NF90_INQ_VARID(fidA,"time_minimum_6h",time_minimum_ID)
            else
              write(*,*) '~!@#$%^* YOU NEED TO ENHANCE THE FORTRAN SCRIPT TO REBUILD NEMO OUTPUTS'
              write(*,*) '         IN ORDER TO DEAL WITH THIS SPECIFIC OUTPUT FREQUENCY.' 
              write(*,*) '         ONLY 1d, 3d, 5d, 1h, 6h ARE SUPPORTED SO FAR...'
              write(*,*) '                                         >>>>>>>>>>>>>>>>>>>>>>>>> STOP !!!!'
              stop
            endif
          endif
        endif
      endif 
    endif
    write(*,*) '  # frequency : ', TRIM(freq)
    status = NF90_INQ_VARID(fidA,"nav_lon",nav_lon_ID)                 ; call erreur(status,.TRUE.,"inq_nav_lon_ID")
    status = NF90_INQ_VARID(fidA,"nav_lat",nav_lat_ID)                 ; call erreur(status,.TRUE.,"inq_nav_lat_ID")
    !-
    if ( kfile .eq. 1 ) then
      ALLOCATE(  time_average(mtime_counter)  )
      ALLOCATE(  time_maximum(mtime_counter)  )
      ALLOCATE(  time_minimum(mtime_counter)  )
    endif 
    ALLOCATE(  nav_lon(mx(kfile),my(kfile))  )
    ALLOCATE(  nav_lat(mx(kfile),my(kfile))  )
    if ( igridZ .gt. 0 .and. kfile .eq. 1 ) ALLOCATE( depth(mdepth) )
    !-
    status = NF90_GET_VAR(fidA,time_average_ID,time_average) ;  call erreur(status,.TRUE.,"getvar_time_average")
    if ( stat_tmax .eq. 0 ) then
       status = NF90_GET_VAR(fidA,time_maximum_ID,time_maximum) ;  call erreur(status,.TRUE.,"getvar_time_maximum")
    endif
    if ( stat_tmin .eq. 0 ) then
      status = NF90_GET_VAR(fidA,time_minimum_ID,time_minimum) ;  call erreur(status,.TRUE.,"getvar_time_minimum")
    endif
    status = NF90_GET_VAR(fidA,nav_lon_ID,nav_lon)           ;  call erreur(status,.TRUE.,"getvar_nav_lon")
    status = NF90_GET_VAR(fidA,nav_lat_ID,nav_lat)           ;  call erreur(status,.TRUE.,"getvar_nav_lat")
    !-
    SELECT CASE (igridZ)
    CASE(1)
     status = NF90_INQ_VARID(fidA,"deptht",depth_ID) ; call erreur(status,.TRUE.,"inq_deptht_ID") 
     status = NF90_GET_VAR(fidA,depth_ID,depth)      ; call erreur(status,.TRUE.,"getvar_deptht")
    CASE(2)
     status = NF90_INQ_VARID(fidA,"depthu",depth_ID) ; call erreur(status,.TRUE.,"inq_depthu_ID")                    
     status = NF90_GET_VAR(fidA,depth_ID,depth)      ; call erreur(status,.TRUE.,"getvar_depthu")
    CASE(3)
     status = NF90_INQ_VARID(fidA,"depthv",depth_ID) ; call erreur(status,.TRUE.,"inq_depthv_ID")                    
     status = NF90_GET_VAR(fidA,depth_ID,depth)      ; call erreur(status,.TRUE.,"getvar_depthv")
    CASE(4)
     status = NF90_INQ_VARID(fidA,"depthw",depth_ID) ; call erreur(status,.TRUE.,"inq_depthw_ID")                    
     status = NF90_GET_VAR(fidA,depth_ID,depth)      ; call erreur(status,.TRUE.,"getvar_depthw")
    END SELECT

    !--- Get Global Attributes :
    if ( kfile .eq. 1 ) then
      status = NF90_GET_ATT(fidA,NF90_GLOBAL,"name",GLOB_file_name)          ; call erreur(status,.TRUE.,"get_att_GLOBAL_name")
      status = NF90_GET_ATT(fidA,NF90_GLOBAL,"description",GLOB_description) ; call erreur(status,.TRUE.,"get_att_GLOBAL_description")
      status = NF90_GET_ATT(fidA,NF90_GLOBAL,"conventions",GLOB_conventions) ; call erreur(status,.TRUE.,"get_att_GLOBAL_conventions")
      status = NF90_GET_ATT(fidA,NF90_GLOBAL,"production",GLOB_production)   ; call erreur(status,.TRUE.,"get_att_GLOBAL_production")
      status = NF90_GET_ATT(fidA,NF90_GLOBAL,"timeStamp",GLOB_timeStamp)     ; call erreur(status,.TRUE.,"get_att_GLOBAL_timeStamp")
      !- check consistency with nproc_xios :
      status = NF90_GET_ATT(fidA,NF90_GLOBAL,"DOMAIN_number_total",GLOB_DOMAIN_number_total) ; call erreur(status,.TRUE.,"get_att_GLOBAL_DOMAIN_number_total")
      if ( GLOB_DOMAIN_number_total .ne. nproc_xios ) then
        write(*,*) '~!@#$%^* Inconsistency between number of XIOS procs in app.conf and in netcdf metadata >>>>>>>>>>>>>>>> STOP !!!!'
        stop
      endif
      !- read dimensions of total grid size (i.e. of the rebuilt file) :
      status = NF90_GET_ATT(fidA,NF90_GLOBAL,"DOMAIN_size_global",GLOB_DOMAIN_size_global) ; call erreur(status,.TRUE.,"get_att_GLOBAL_DOMAIN_size_global")
      mxout = GLOB_DOMAIN_size_global(1)
      myout = GLOB_DOMAIN_size_global(2)
    endif
    !- First and last points positions on the rebuilt domain :
    status = NF90_GET_ATT(fidA,NF90_GLOBAL,"DOMAIN_position_first",GLOB_DOMAIN_position_first) ; call erreur(status,.TRUE.,"get_att_GLOBAL_DOMAIN_position_first")
    status = NF90_GET_ATT(fidA,NF90_GLOBAL,"DOMAIN_position_last",GLOB_DOMAIN_position_last)   ; call erreur(status,.TRUE.,"get_att_GLOBAL_DOMAIN_position_last")
    imin = GLOB_DOMAIN_position_first(1)
    imax = GLOB_DOMAIN_position_last(1)
    jmin = GLOB_DOMAIN_position_first(2)
    jmax = GLOB_DOMAIN_position_last(2)
    write(*,*) '  # box position within global grid :  from ', imin, jmin
    write(*,*) '                                         to ', imax, jmax

    if ( kfile .eq. 1 ) ALLOCATE( nav_lat_out(mxout,myout), nav_lon_out(mxout,myout) )

    write(*,*) '  # Variables :'
    if ( igridZ .eq. 0 ) then
      !--- Let us test the existence of all possible 2D variables and read them and their attributes :
      stat_mld_dt02 = NF90_INQ_VARID(fidA,"mld_dt02",mld_dt02_ID)
      if ( stat_mld_dt02 .eq. 0 ) then
        write(*,*) '       ----->  mld_dt02'
        ALLOCATE(  mld_dt02(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  mld_dt02_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,mld_dt02_ID,mld_dt02) ; call erreur(status,.TRUE.,"getvar_mld_dt02")
        status = NF90_GET_ATT(fidA,mld_dt02_ID,"long_name",nam_mld_dt02) ; call erreur(status,.TRUE.,"get_att_mld_dt02")
        status = NF90_GET_ATT(fidA,mld_dt02_ID,"units",units_mld_dt02) ; call erreur(status,.TRUE.,"get_att_mld_dt02")
        status = NF90_GET_ATT(fidA,mld_dt02_ID,"coordinates",coord_mld_dt02) ; call erreur(status,.TRUE.,"get_att_mld_dt02")
      endif
      !-
      stat_mldr10_3 = NF90_INQ_VARID(fidA,"mldr10_3",mldr10_3_ID)
      if ( stat_mldr10_3 .eq. 0 ) then
        write(*,*) '       ----->  mldr10_3'
        ALLOCATE(  mldr10_3(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  mldr10_3_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,mldr10_3_ID,mldr10_3) ; call erreur(status,.TRUE.,"getvar_mldr10_3")
        status = NF90_GET_ATT(fidA,mldr10_3_ID,"long_name",nam_mldr10_3) ; call erreur(status,.TRUE.,"get_att_mldr10_3")
        status = NF90_GET_ATT(fidA,mldr10_3_ID,"units",units_mldr10_3) ; call erreur(status,.TRUE.,"get_att_mldr10_3")
        status = NF90_GET_ATT(fidA,mldr10_3_ID,"coordinates",coord_mldr10_3) ; call erreur(status,.TRUE.,"get_att_mldr10_3")
      endif
      !-
      stat_zos = NF90_INQ_VARID(fidA,"zos",zos_ID)
      if ( stat_zos .eq. 0 ) then
        write(*,*) '       ----->  zos'
        ALLOCATE(  zos(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  zos_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,zos_ID,zos) ; call erreur(status,.TRUE.,"getvar_zos")
        status = NF90_GET_ATT(fidA,zos_ID,"long_name",nam_zos) ; call erreur(status,.TRUE.,"get_att_zos")
        status = NF90_GET_ATT(fidA,zos_ID,"units",units_zos) ; call erreur(status,.TRUE.,"get_att_zos")
        status = NF90_GET_ATT(fidA,zos_ID,"coordinates",coord_zos) ; call erreur(status,.TRUE.,"get_att_zos")
      endif
      !-
      stat_sos = NF90_INQ_VARID(fidA,"sos",sos_ID)
      if ( stat_sos .eq. 0 ) then
        write(*,*) '       ----->  sos'
        ALLOCATE(  sos(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  sos_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,sos_ID,sos) ; call erreur(status,.TRUE.,"getvar_sos")
        status = NF90_GET_ATT(fidA,sos_ID,"long_name",nam_sos) ; call erreur(status,.TRUE.,"get_att_sos")
        status = NF90_GET_ATT(fidA,sos_ID,"units",units_sos) ; call erreur(status,.TRUE.,"get_att_sos")
        status = NF90_GET_ATT(fidA,sos_ID,"coordinates",coord_sos) ; call erreur(status,.TRUE.,"get_att_sos")
      endif
      !-
      stat_tossq = NF90_INQ_VARID(fidA,"tossq",tossq_ID)
      if ( stat_tossq .eq. 0 ) then
        write(*,*) '       ----->  tossq'
        ALLOCATE(  tossq(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  tossq_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,tossq_ID,tossq) ; call erreur(status,.TRUE.,"getvar_tossq")
        status = NF90_GET_ATT(fidA,tossq_ID,"long_name",nam_tossq) ; call erreur(status,.TRUE.,"get_att_tossq")
        status = NF90_GET_ATT(fidA,tossq_ID,"units",units_tossq) ; call erreur(status,.TRUE.,"get_att_tossq")
        status = NF90_GET_ATT(fidA,tossq_ID,"coordinates",coord_tossq) ; call erreur(status,.TRUE.,"get_att_tossq")
      endif
      !-
      stat_tos_min = NF90_INQ_VARID(fidA,"tos_min",tos_min_ID)
      if ( stat_tos_min .eq. 0 ) then
        write(*,*) '       ----->  tos_min'
        ALLOCATE(  tos_min(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  tos_min_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,tos_min_ID,tos_min) ; call erreur(status,.TRUE.,"getvar_tos_min")
        status = NF90_GET_ATT(fidA,tos_min_ID,"long_name",nam_tos_min) ; call erreur(status,.TRUE.,"get_att_tos_min")
        status = NF90_GET_ATT(fidA,tos_min_ID,"units",units_tos_min) ; call erreur(status,.TRUE.,"get_att_tos_min")
        status = NF90_GET_ATT(fidA,tos_min_ID,"coordinates",coord_tos_min) ; call erreur(status,.TRUE.,"get_att_tos_min")
      endif
      !-
      stat_tos_max = NF90_INQ_VARID(fidA,"tos_max",tos_max_ID)
      if ( stat_tos_max .eq. 0 ) then
        write(*,*) '       ----->  tos_max'
        ALLOCATE(  tos_max(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  tos_max_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,tos_max_ID,tos_max) ; call erreur(status,.TRUE.,"getvar_tos_max")
        status = NF90_GET_ATT(fidA,tos_max_ID,"long_name",nam_tos_max) ; call erreur(status,.TRUE.,"get_att_tos_max")
        status = NF90_GET_ATT(fidA,tos_max_ID,"units",units_tos_max) ; call erreur(status,.TRUE.,"get_att_tos_max")
        status = NF90_GET_ATT(fidA,tos_max_ID,"coordinates",coord_tos_max) ; call erreur(status,.TRUE.,"get_att_tos_max")
      endif
      !-
      stat_tos = NF90_INQ_VARID(fidA,"tos",tos_ID)
      if ( stat_tos .eq. 0 ) then
        write(*,*) '       ----->  tos'
        ALLOCATE(  tos(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  tos_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,tos_ID,tos) ; call erreur(status,.TRUE.,"getvar_tos")
        status = NF90_GET_ATT(fidA,tos_ID,"long_name",nam_tos) ; call erreur(status,.TRUE.,"get_att_tos")
        status = NF90_GET_ATT(fidA,tos_ID,"units",units_tos) ; call erreur(status,.TRUE.,"get_att_tos")
        status = NF90_GET_ATT(fidA,tos_ID,"coordinates",coord_tos) ; call erreur(status,.TRUE.,"get_att_tos")
      endif
      !-
      stat_tauuo = NF90_INQ_VARID(fidA,"tauuo",tauuo_ID)
      if ( stat_tauuo .eq. 0 ) then
        write(*,*) '       ----->  tauuo'
        ALLOCATE(  tauuo(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  tauuo_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,tauuo_ID,tauuo) ; call erreur(status,.TRUE.,"getvar_tauuo")
        status = NF90_GET_ATT(fidA,tauuo_ID,"long_name",nam_tauuo) ; call erreur(status,.TRUE.,"get_att_tauuo")
        status = NF90_GET_ATT(fidA,tauuo_ID,"units",units_tauuo) ; call erreur(status,.TRUE.,"get_att_tauuo")
        status = NF90_GET_ATT(fidA,tauuo_ID,"coordinates",coord_tauuo) ; call erreur(status,.TRUE.,"get_att_tauuo")
      endif
      !-
      stat_tauvo = NF90_INQ_VARID(fidA,"tauvo",tauvo_ID)
      if ( stat_tauvo .eq. 0 ) then
        write(*,*) '       ----->  tauvo'
        ALLOCATE(  tauvo(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  tauvo_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,tauvo_ID,tauvo) ; call erreur(status,.TRUE.,"getvar_tauvo")
        status = NF90_GET_ATT(fidA,tauvo_ID,"long_name",nam_tauvo) ; call erreur(status,.TRUE.,"get_att_tauvo")
        status = NF90_GET_ATT(fidA,tauvo_ID,"units",units_tauvo) ; call erreur(status,.TRUE.,"get_att_tauvo")
        status = NF90_GET_ATT(fidA,tauvo_ID,"coordinates",coord_tauvo) ; call erreur(status,.TRUE.,"get_att_tauvo")
      endif
      !-
      stat_uos = NF90_INQ_VARID(fidA,"uos",uos_ID)
      if ( stat_uos .eq. 0 ) then
        write(*,*) '       ----->  uos'
        ALLOCATE(  uos(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  uos_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,uos_ID,uos) ; call erreur(status,.TRUE.,"getvar_uos")
        status = NF90_GET_ATT(fidA,uos_ID,"long_name",nam_uos) ; call erreur(status,.TRUE.,"get_att_uos")
        status = NF90_GET_ATT(fidA,uos_ID,"units",units_uos) ; call erreur(status,.TRUE.,"get_att_uos")
        status = NF90_GET_ATT(fidA,uos_ID,"coordinates",coord_uos) ; call erreur(status,.TRUE.,"get_att_uos")
      endif
      !-
      stat_vos = NF90_INQ_VARID(fidA,"vos",vos_ID)
      if ( stat_vos .eq. 0 ) then
        write(*,*) '       ----->  vos'
        ALLOCATE(  vos(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  vos_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,vos_ID,vos) ; call erreur(status,.TRUE.,"getvar_vos")
        status = NF90_GET_ATT(fidA,vos_ID,"long_name",nam_vos) ; call erreur(status,.TRUE.,"get_att_vos")
        status = NF90_GET_ATT(fidA,vos_ID,"units",units_vos) ; call erreur(status,.TRUE.,"get_att_vos")
        status = NF90_GET_ATT(fidA,vos_ID,"coordinates",coord_vos) ; call erreur(status,.TRUE.,"get_att_vos")
      endif
      !-
      stat_wfo = NF90_INQ_VARID(fidA,"wfo",wfo_ID)
      if ( stat_wfo .eq. 0 ) then
        write(*,*) '       ----->  wfo'
        ALLOCATE(  wfo(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  wfo_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,wfo_ID,wfo) ; call erreur(status,.TRUE.,"getvar_wfo")
        status = NF90_GET_ATT(fidA,wfo_ID,"long_name",nam_wfo) ; call erreur(status,.TRUE.,"get_att_wfo")
        status = NF90_GET_ATT(fidA,wfo_ID,"units",units_wfo) ; call erreur(status,.TRUE.,"get_att_wfo")
        status = NF90_GET_ATT(fidA,wfo_ID,"coordinates",coord_wfo) ; call erreur(status,.TRUE.,"get_att_wfo")
      endif
      !-
      stat_rsntds = NF90_INQ_VARID(fidA,"rsntds",rsntds_ID)
      if ( stat_rsntds .eq. 0 ) then
        write(*,*) '       ----->  rsntds'
        ALLOCATE(  rsntds(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  rsntds_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,rsntds_ID,rsntds) ; call erreur(status,.TRUE.,"getvar_rsntds")
        status = NF90_GET_ATT(fidA,rsntds_ID,"long_name",nam_rsntds) ; call erreur(status,.TRUE.,"get_att_rsntds")
        status = NF90_GET_ATT(fidA,rsntds_ID,"units",units_rsntds) ; call erreur(status,.TRUE.,"get_att_rsntds")
        status = NF90_GET_ATT(fidA,rsntds_ID,"coordinates",coord_rsntds) ; call erreur(status,.TRUE.,"get_att_rsntds")
      endif
      !-
      stat_tohfls = NF90_INQ_VARID(fidA,"tohfls",tohfls_ID)
      if ( stat_tohfls .eq. 0 ) then
        write(*,*) '       ----->  tohfls'
        ALLOCATE(  tohfls(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  tohfls_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,tohfls_ID,tohfls) ; call erreur(status,.TRUE.,"getvar_tohfls")
        status = NF90_GET_ATT(fidA,tohfls_ID,"long_name",nam_tohfls) ; call erreur(status,.TRUE.,"get_att_tohfls")
        status = NF90_GET_ATT(fidA,tohfls_ID,"units",units_tohfls) ; call erreur(status,.TRUE.,"get_att_tohfls")
        status = NF90_GET_ATT(fidA,tohfls_ID,"coordinates",coord_tohfls) ; call erreur(status,.TRUE.,"get_att_tohfls")
      endif
      !-
      stat_taum = NF90_INQ_VARID(fidA,"taum",taum_ID)
      if ( stat_taum .eq. 0 ) then
        write(*,*) '       ----->  taum'
        ALLOCATE(  taum(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  taum_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,taum_ID,taum) ; call erreur(status,.TRUE.,"getvar_taum")
        status = NF90_GET_ATT(fidA,taum_ID,"long_name",nam_taum) ; call erreur(status,.TRUE.,"get_att_taum")
        status = NF90_GET_ATT(fidA,taum_ID,"units",units_taum) ; call erreur(status,.TRUE.,"get_att_taum")
        status = NF90_GET_ATT(fidA,taum_ID,"coordinates",coord_taum) ; call erreur(status,.TRUE.,"get_att_taum")
      endif
      !-
      stat_x20d = NF90_INQ_VARID(fidA,"20d",x20d_ID)
      if ( stat_x20d .eq. 0 ) then
        write(*,*) '       ----->  x20d'
        ALLOCATE(  x20d(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  x20d_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,x20d_ID,x20d) ; call erreur(status,.TRUE.,"getvar_x20d")
        status = NF90_GET_ATT(fidA,x20d_ID,"long_name",nam_x20d) ; call erreur(status,.TRUE.,"get_att_x20d")
        status = NF90_GET_ATT(fidA,x20d_ID,"units",units_x20d) ; call erreur(status,.TRUE.,"get_att_x20d")
        status = NF90_GET_ATT(fidA,x20d_ID,"coordinates",coord_x20d) ; call erreur(status,.TRUE.,"get_att_x20d")
      endif
      !-
      stat_mldkz5 = NF90_INQ_VARID(fidA,"mldkz5",mldkz5_ID)
      if ( stat_mldkz5 .eq. 0 ) then
        write(*,*) '       ----->  mldkz5'
        ALLOCATE(  mldkz5(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  mldkz5_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,mldkz5_ID,mldkz5) ; call erreur(status,.TRUE.,"getvar_mldkz5")
        status = NF90_GET_ATT(fidA,mldkz5_ID,"long_name",nam_mldkz5) ; call erreur(status,.TRUE.,"get_att_mldkz5")
        status = NF90_GET_ATT(fidA,mldkz5_ID,"units",units_mldkz5) ; call erreur(status,.TRUE.,"get_att_mldkz5")
        status = NF90_GET_ATT(fidA,mldkz5_ID,"coordinates",coord_mldkz5) ; call erreur(status,.TRUE.,"get_att_mldkz5")
      endif
      !-
      stat_mldr10_1 = NF90_INQ_VARID(fidA,"mldr10_1",mldr10_1_ID)
      if ( stat_mldr10_1 .eq. 0 ) then
        write(*,*) '       ----->  mldr10_1'
        ALLOCATE(  mldr10_1(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  mldr10_1_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,mldr10_1_ID,mldr10_1) ; call erreur(status,.TRUE.,"getvar_mldr10_1")
        status = NF90_GET_ATT(fidA,mldr10_1_ID,"long_name",nam_mldr10_1) ; call erreur(status,.TRUE.,"get_att_mldr10_1")
        status = NF90_GET_ATT(fidA,mldr10_1_ID,"units",units_mldr10_1) ; call erreur(status,.TRUE.,"get_att_mldr10_1")
        status = NF90_GET_ATT(fidA,mldr10_1_ID,"coordinates",coord_mldr10_1) ; call erreur(status,.TRUE.,"get_att_mldr10_1")
      endif
      !-
      stat_topthdep = NF90_INQ_VARID(fidA,"topthdep",topthdep_ID)
      if ( stat_topthdep .eq. 0 ) then
        write(*,*) '       ----->  topthdep'
        ALLOCATE(  topthdep(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  topthdep_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,topthdep_ID,topthdep) ; call erreur(status,.TRUE.,"getvar_topthdep")
        status = NF90_GET_ATT(fidA,topthdep_ID,"long_name",nam_topthdep) ; call erreur(status,.TRUE.,"get_att_topthdep")
        status = NF90_GET_ATT(fidA,topthdep_ID,"units",units_topthdep) ; call erreur(status,.TRUE.,"get_att_topthdep")
        status = NF90_GET_ATT(fidA,topthdep_ID,"coordinates",coord_topthdep) ; call erreur(status,.TRUE.,"get_att_topthdep")
      endif
      !-
      stat_pycndep = NF90_INQ_VARID(fidA,"pycndep",pycndep_ID)
      if ( stat_pycndep .eq. 0 ) then
        write(*,*) '       ----->  pycndep'
        ALLOCATE(  pycndep(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  pycndep_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,pycndep_ID,pycndep) ; call erreur(status,.TRUE.,"getvar_pycndep")
        status = NF90_GET_ATT(fidA,pycndep_ID,"long_name",nam_pycndep) ; call erreur(status,.TRUE.,"get_att_pycndep")
        status = NF90_GET_ATT(fidA,pycndep_ID,"units",units_pycndep) ; call erreur(status,.TRUE.,"get_att_pycndep")
        status = NF90_GET_ATT(fidA,pycndep_ID,"coordinates",coord_pycndep) ; call erreur(status,.TRUE.,"get_att_pycndep")
      endif
      !-
      stat_tinv = NF90_INQ_VARID(fidA,"tinv",tinv_ID)
      if ( stat_tinv .eq. 0 ) then
        write(*,*) '       ----->  tinv'
        ALLOCATE(  tinv(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  tinv_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,tinv_ID,tinv) ; call erreur(status,.TRUE.,"getvar_tinv")
        status = NF90_GET_ATT(fidA,tinv_ID,"long_name",nam_tinv) ; call erreur(status,.TRUE.,"get_att_tinv")
        status = NF90_GET_ATT(fidA,tinv_ID,"units",units_tinv) ; call erreur(status,.TRUE.,"get_att_tinv")
        status = NF90_GET_ATT(fidA,tinv_ID,"coordinates",coord_tinv) ; call erreur(status,.TRUE.,"get_att_tinv")
      endif
      !-
      stat_depti = NF90_INQ_VARID(fidA,"depti",depti_ID)
      if ( stat_depti .eq. 0 ) then
        write(*,*) '       ----->  depti'
        ALLOCATE(  depti(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  depti_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,depti_ID,depti) ; call erreur(status,.TRUE.,"getvar_depti")
        status = NF90_GET_ATT(fidA,depti_ID,"long_name",nam_depti) ; call erreur(status,.TRUE.,"get_att_depti")
        status = NF90_GET_ATT(fidA,depti_ID,"units",units_depti) ; call erreur(status,.TRUE.,"get_att_depti")
        status = NF90_GET_ATT(fidA,depti_ID,"coordinates",coord_depti) ; call erreur(status,.TRUE.,"get_att_depti")
      endif
      !-
      stat_blt = NF90_INQ_VARID(fidA,"blt",blt_ID)
      if ( stat_blt .eq. 0 ) then
        write(*,*) '       ----->  blt'
        ALLOCATE(  blt(mx(kfile),my(kfile),mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  blt_out(mxout,myout,mtime_counter)  )
        status = NF90_GET_VAR(fidA,blt_ID,blt) ; call erreur(status,.TRUE.,"getvar_blt")
        status = NF90_GET_ATT(fidA,blt_ID,"long_name",nam_blt) ; call erreur(status,.TRUE.,"get_att_blt")
        status = NF90_GET_ATT(fidA,blt_ID,"units",units_blt) ; call erreur(status,.TRUE.,"get_att_blt")
        status = NF90_GET_ATT(fidA,blt_ID,"coordinates",coord_blt) ; call erreur(status,.TRUE.,"get_att_blt")
      endif
    else 
      !--- Let us test the existence of all possible 3D variables and read them if they exist :
      stat_thetao = NF90_INQ_VARID(fidA,"thetao",thetao_ID)
      if ( stat_thetao .eq. 0 ) then
        write(*,*) '       ----->  thetao'
        ALLOCATE(  thetao(mx(kfile),my(kfile),mdepth,mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  thetao_out(mxout,myout,mdepth,mtime_counter)  )
        status = NF90_GET_VAR(fidA,thetao_ID,thetao) ; call erreur(status,.TRUE.,"getvar_thetao")
        status = NF90_GET_ATT(fidA,thetao_ID,"long_name",nam_thetao) ; call erreur(status,.TRUE.,"get_att_thetao")
        status = NF90_GET_ATT(fidA,thetao_ID,"units",units_thetao) ; call erreur(status,.TRUE.,"get_att_thetao")
        status = NF90_GET_ATT(fidA,thetao_ID,"coordinates",coord_thetao) ; call erreur(status,.TRUE.,"get_att_thetao")
      endif
      !-
      stat_so = NF90_INQ_VARID(fidA,"so",so_ID)
      if ( stat_so .eq. 0 ) then
        write(*,*) '       ----->  so'
        ALLOCATE(  so(mx(kfile),my(kfile),mdepth,mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  so_out(mxout,myout,mdepth,mtime_counter)  )
        status = NF90_GET_VAR(fidA,so_ID,so) ; call erreur(status,.TRUE.,"getvar_so")
        status = NF90_GET_ATT(fidA,so_ID,"long_name",nam_so) ; call erreur(status,.TRUE.,"get_att_so")
        status = NF90_GET_ATT(fidA,so_ID,"units",units_so) ; call erreur(status,.TRUE.,"get_att_so")
        status = NF90_GET_ATT(fidA,so_ID,"coordinates",coord_so) ; call erreur(status,.TRUE.,"get_att_so")
      endif
      !-
      stat_uo = NF90_INQ_VARID(fidA,"uo",uo_ID)
      if ( stat_uo .eq. 0 ) then
        write(*,*) '       ----->  uo'
        ALLOCATE(  uo(mx(kfile),my(kfile),mdepth,mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  uo_out(mxout,myout,mdepth,mtime_counter)  )
        status = NF90_GET_VAR(fidA,uo_ID,uo) ; call erreur(status,.TRUE.,"getvar_uo")
        status = NF90_GET_ATT(fidA,uo_ID,"long_name",nam_uo) ; call erreur(status,.TRUE.,"get_att_uo")
        status = NF90_GET_ATT(fidA,uo_ID,"units",units_uo) ; call erreur(status,.TRUE.,"get_att_uo")
        status = NF90_GET_ATT(fidA,uo_ID,"coordinates",coord_uo) ; call erreur(status,.TRUE.,"get_att_uo")
      endif
      !-
      stat_vo = NF90_INQ_VARID(fidA,"vo",vo_ID)
      if ( stat_vo .eq. 0 ) then
        write(*,*) '       ----->  vo'
        ALLOCATE(  vo(mx(kfile),my(kfile),mdepth,mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  vo_out(mxout,myout,mdepth,mtime_counter)  )
        status = NF90_GET_VAR(fidA,vo_ID,vo) ; call erreur(status,.TRUE.,"getvar_vo")
        status = NF90_GET_ATT(fidA,vo_ID,"long_name",nam_vo) ; call erreur(status,.TRUE.,"get_att_vo")
        status = NF90_GET_ATT(fidA,vo_ID,"units",units_vo) ; call erreur(status,.TRUE.,"get_att_vo")
        status = NF90_GET_ATT(fidA,vo_ID,"coordinates",coord_vo) ; call erreur(status,.TRUE.,"get_att_vo")
      endif
      !-
      stat_wo = NF90_INQ_VARID(fidA,"wo",wo_ID)
      if ( stat_wo .eq. 0 ) then
        write(*,*) '       ----->  wo'
        ALLOCATE(  wo(mx(kfile),my(kfile),mdepth,mtime_counter)  )
        if ( kfile .eq. 1 ) ALLOCATE(  wo_out(mxout,myout,mdepth,mtime_counter)  )
        status = NF90_GET_VAR(fidA,wo_ID,wo) ; call erreur(status,.TRUE.,"getvar_wo")
        status = NF90_GET_ATT(fidA,wo_ID,"long_name",nam_wo) ; call erreur(status,.TRUE.,"get_att_wo")
        status = NF90_GET_ATT(fidA,wo_ID,"units",units_wo) ; call erreur(status,.TRUE.,"get_att_wo")
        status = NF90_GET_ATT(fidA,wo_ID,"coordinates",coord_wo) ; call erreur(status,.TRUE.,"get_att_wo")
      endif
      !-
    endif ! if ( igridZ .eq. 0 ) 
    
  !----- Close file
  status = NF90_CLOSE(fidA)                      
  call erreur(status,.TRUE.,"close_file")     

  !-----------------------------------------------------
  !- Now, we fill the big output file :
  nav_lat_out (imin:imax,jmin:jmax) = nav_lat (1:mx(kfile),1:my(kfile))
  nav_lon_out (imin:imax,jmin:jmax) = nav_lon (1:mx(kfile),1:my(kfile))
  DEALLOCATE( nav_lat, nav_lon )
  !-
  if ( stat_mld_dt02 .eq. 0 ) then
    mld_dt02_out (imin:imax,jmin:jmax,:) = mld_dt02 (1:mx(kfile),1:my(kfile),:) 
    DEALLOCATE(mld_dt02 )
  endif
  if ( stat_mldr10_3 .eq. 0 ) then
    mldr10_3_out (imin:imax,jmin:jmax,:) = mldr10_3 (1:mx(kfile),1:my(kfile),:) 
    DEALLOCATE(mldr10_3 )
  endif
  if ( stat_zos .eq. 0 ) then
    zos_out      (imin:imax,jmin:jmax,:) = zos      (1:mx(kfile),1:my(kfile),:) 
    DEALLOCATE(zos )
  endif
  if ( stat_sos .eq. 0 ) then
    sos_out      (imin:imax,jmin:jmax,:) = sos      (1:mx(kfile),1:my(kfile),:) 
    DEALLOCATE(sos )
  endif
  if ( stat_tossq .eq. 0 ) then
    tossq_out    (imin:imax,jmin:jmax,:) = tossq    (1:mx(kfile),1:my(kfile),:) 
    DEALLOCATE(tossq )
  endif
  if ( stat_tos_min .eq. 0 ) then
    tos_min_out  (imin:imax,jmin:jmax,:) = tos_min  (1:mx(kfile),1:my(kfile),:) 
    DEALLOCATE(tos_min )
  endif
  if ( stat_tos_max .eq. 0 ) then
    tos_max_out  (imin:imax,jmin:jmax,:) = tos_max  (1:mx(kfile),1:my(kfile),:) 
    DEALLOCATE(tos_max )
  endif
  if ( stat_tos .eq. 0 ) then
    tos_out      (imin:imax,jmin:jmax,:) = tos      (1:mx(kfile),1:my(kfile),:) 
    DEALLOCATE(tos )
  endif
  if ( stat_tauuo .eq. 0 ) then
    tauuo_out    (imin:imax,jmin:jmax,:) = tauuo    (1:mx(kfile),1:my(kfile),:) 
    DEALLOCATE(tauuo )
  endif
  if ( stat_tauvo .eq. 0 ) then
    tauvo_out    (imin:imax,jmin:jmax,:) = tauvo    (1:mx(kfile),1:my(kfile),:) 
    DEALLOCATE(tauvo )
  endif
  if ( stat_uos .eq. 0 ) then
    uos_out      (imin:imax,jmin:jmax,:) = uos      (1:mx(kfile),1:my(kfile),:) 
    DEALLOCATE(uos )
  endif
  if ( stat_vos .eq. 0 ) then
    vos_out      (imin:imax,jmin:jmax,:) = vos      (1:mx(kfile),1:my(kfile),:)
    DEALLOCATE(vos )
  endif
  if ( stat_wfo .eq. 0 ) then
    wfo_out      (imin:imax,jmin:jmax,:) = wfo      (1:mx(kfile),1:my(kfile),:) 
    DEALLOCATE(wfo )
  endif
  if ( stat_rsntds .eq. 0 ) then
    rsntds_out   (imin:imax,jmin:jmax,:) = rsntds   (1:mx(kfile),1:my(kfile),:)
    DEALLOCATE(rsntds )
  endif
  if ( stat_tohfls .eq. 0 ) then
    tohfls_out   (imin:imax,jmin:jmax,:) = tohfls   (1:mx(kfile),1:my(kfile),:)
    DEALLOCATE(tohfls )
  endif
  if ( stat_taum .eq. 0 ) then
    taum_out     (imin:imax,jmin:jmax,:) = taum     (1:mx(kfile),1:my(kfile),:)
    DEALLOCATE(taum )
  endif
  if ( stat_x20d .eq. 0 ) then
    x20d_out      (imin:imax,jmin:jmax,:) = x20d      (1:mx(kfile),1:my(kfile),:)
    DEALLOCATE(x20d )
  endif
  if ( stat_mldkz5 .eq. 0 ) then
    mldkz5_out   (imin:imax,jmin:jmax,:) = mldkz5   (1:mx(kfile),1:my(kfile),:)
    DEALLOCATE(mldkz5 )
  endif
  if ( stat_mldr10_1 .eq. 0 ) then
    mldr10_1_out (imin:imax,jmin:jmax,:) = mldr10_1 (1:mx(kfile),1:my(kfile),:)
    DEALLOCATE(mldr10_1 )
  endif
  if ( stat_topthdep .eq. 0 ) then
    topthdep_out (imin:imax,jmin:jmax,:) = topthdep (1:mx(kfile),1:my(kfile),:)
    DEALLOCATE(topthdep )
  endif
  if ( stat_pycndep .eq. 0 ) then
    pycndep_out  (imin:imax,jmin:jmax,:) = pycndep  (1:mx(kfile),1:my(kfile),:)
    DEALLOCATE(pycndep )
  endif
  if ( stat_tinv .eq. 0 ) then
    tinv_out     (imin:imax,jmin:jmax,:) = tinv     (1:mx(kfile),1:my(kfile),:)
    DEALLOCATE(tinv )
  endif
  if ( stat_depti .eq. 0 ) then
    depti_out    (imin:imax,jmin:jmax,:) = depti    (1:mx(kfile),1:my(kfile),:)
    DEALLOCATE(depti )
  endif
  if ( stat_blt .eq. 0 ) then
    blt_out      (imin:imax,jmin:jmax,:) = blt      (1:mx(kfile),1:my(kfile),:)
    DEALLOCATE(blt )
  endif
  !-
  if ( stat_thetao .eq. 0 ) then
    thetao_out   (imin:imax,jmin:jmax,:,:) = thetao (1:mx(kfile),1:my(kfile),:,:)
    DEALLOCATE(thetao )
  endif
  if ( stat_so .eq. 0 ) then
    so_out       (imin:imax,jmin:jmax,:,:) = so     (1:mx(kfile),1:my(kfile),:,:)
    DEALLOCATE(so )
  endif
  if ( stat_uo .eq. 0 ) then
    uo_out       (imin:imax,jmin:jmax,:,:) = uo     (1:mx(kfile),1:my(kfile),:,:)
    DEALLOCATE(uo )
  endif
  if ( stat_vo .eq. 0 ) then
    vo_out       (imin:imax,jmin:jmax,:,:) = vo     (1:mx(kfile),1:my(kfile),:,:)
    DEALLOCATE(vo )
  endif
  if ( stat_wo .eq. 0 ) then
    wo_out       (imin:imax,jmin:jmax,:,:) = wo     (1:mx(kfile),1:my(kfile),:,:) 
    DEALLOCATE(wo )
  endif                 

 
enddo  !! do kfile=1,nproc_xios
                                            
!---------------------------------------                      
! Writing rebuilt netcdf file :                                   
   
file_out='pppppp.nc' 
write(*,*) '  # Name of rebuilt file :', TRIM(file_out)                         
                     
status = NF90_CREATE(TRIM(file_out),or(NF90_NOCLOBBER,NF90_64BIT_OFFSET),fidM) ; call erreur(status,.TRUE.,'create')                     
                                                                
!--- Definition of file dimensions
status = NF90_DEF_DIM(fidM,"time_counter",NF90_UNLIMITED,dimID_time_counter) ; call erreur(status,.TRUE.,"def_dimID_time_counter")
status = NF90_DEF_DIM(fidM,"y",myout,dimID_y)                                ; call erreur(status,.TRUE.,"def_dimID_y")
status = NF90_DEF_DIM(fidM,"x",mxout,dimID_x)                                ; call erreur(status,.TRUE.,"def_dimID_x")
SELECT CASE (igridZ)
CASE(1)                 
  status = NF90_DEF_DIM(fidM,"deptht",mdepth,dimID_depth)
CASE(2)
  status = NF90_DEF_DIM(fidM,"depthu",mdepth,dimID_depth)
CASE(3)
  status = NF90_DEF_DIM(fidM,"depthv",mdepth,dimID_depth)
CASE(4)
  status = NF90_DEF_DIM(fidM,"depthw",mdepth,dimID_depth)
END SELECT 
call erreur(status,.TRUE.,"def_dimID_depth")

!--- Definition of compulsory variables :                            
write(time_average_str,831) TRIM(freq)
write(time_maximum_str,832) TRIM(freq)
write(time_minimum_str,833) TRIM(freq)
831 FORMAT('time_average_',a2)
832 FORMAT('time_maximum_',a2)
833 FORMAT('time_minimum_',a2)
status = NF90_DEF_VAR(fidM,time_average_str,NF90_DOUBLE,(/dimID_time_counter/),time_average_ID) ; call erreur(status,.TRUE.,"def_var_time_average_ID")
if ( stat_tmax .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,time_maximum_str,NF90_DOUBLE,(/dimID_time_counter/),time_maximum_ID) ; call erreur(status,.TRUE.,"def_var_time_maximum_ID")
endif
if ( stat_tmin .eq. 0 ) then
status = NF90_DEF_VAR(fidM,time_minimum_str,NF90_DOUBLE,(/dimID_time_counter/),time_minimum_ID) ; call erreur(status,.TRUE.,"def_var_time_minimum_ID")
endif
status = NF90_DEF_VAR(fidM,"nav_lat",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lat_ID)                 ; call erreur(status,.TRUE.,"def_var_nav_lat_ID")
status = NF90_DEF_VAR(fidM,"nav_lon",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lon_ID)                 ; call erreur(status,.TRUE.,"def_var_nav_lon_ID")
SELECT CASE (igridZ)
CASE(1)
  status = NF90_DEF_VAR(fidM,"deptht",NF90_FLOAT,(/dimID_depth/),depth_ID) ; call erreur(status,.TRUE.,"def_var_deptht")
  status = NF90_PUT_ATT(fidM,depth_ID,"long_name","Vertical T levels")     ; call erreur(status,.TRUE.,"put_att_long_name_deptht")
CASE(2)
  status = NF90_DEF_VAR(fidM,"depthu",NF90_FLOAT,(/dimID_depth/),depth_ID) ; call erreur(status,.TRUE.,"def_var_depthu")
  status = NF90_PUT_ATT(fidM,depth_ID,"long_name","Vertical U levels")     ; call erreur(status,.TRUE.,"put_att_long_name_depthu")
CASE(3)
  status = NF90_DEF_VAR(fidM,"depthv",NF90_FLOAT,(/dimID_depth/),depth_ID) ; call erreur(status,.TRUE.,"def_var_depthv")
  status = NF90_PUT_ATT(fidM,depth_ID,"long_name","Vertical V levels")     ; call erreur(status,.TRUE.,"put_att_long_name_depthv")
CASE(4)
  status = NF90_DEF_VAR(fidM,"depthw",NF90_FLOAT,(/dimID_depth/),depth_ID) ; call erreur(status,.TRUE.,"def_var_depthw")
  status = NF90_PUT_ATT(fidM,depth_ID,"long_name","Vertical W levels")     ; call erreur(status,.TRUE.,"put_att_long_name_depthw")
END SELECT

!--- Definition of 2D variables and their attributes:
if ( stat_mld_dt02 .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"mld_dt02",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),mld_dt02_ID) ; call erreur(status,.TRUE.,"def_var_mld_dt02")
  status = NF90_PUT_ATT(fidM,mld_dt02_ID,"coordinates",TRIM(coord_mld_dt02)) ; call erreur(status,.TRUE.,"put_att_coordinates_mld_dt02_ID")
  status = NF90_PUT_ATT(fidM,mld_dt02_ID,"units",TRIM(units_mld_dt02)) ; call erreur(status,.TRUE.,"put_att_units_mld_dt02_ID")
  status = NF90_PUT_ATT(fidM,mld_dt02_ID,"long_name",TRIM(nam_mld_dt02)) ; call erreur(status,.TRUE.,"put_att_long_name_mld_dt02_ID")
endif
if ( stat_mldr10_3 .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"mldr10_3",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),mldr10_3_ID) ; call erreur(status,.TRUE.,"def_var_mldr10_3")
  status = NF90_PUT_ATT(fidM,mldr10_3_ID,"coordinates",TRIM(coord_mldr10_3)) ; call erreur(status,.TRUE.,"put_att_coordinates_mldr10_3_ID")
  status = NF90_PUT_ATT(fidM,mldr10_3_ID,"units",TRIM(units_mldr10_3)) ; call erreur(status,.TRUE.,"put_att_units_mldr10_3_ID")
  status = NF90_PUT_ATT(fidM,mldr10_3_ID,"long_name",TRIM(nam_mldr10_3)) ; call erreur(status,.TRUE.,"put_att_long_name_mldr10_3_ID")
endif
if ( stat_zos .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"zos",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),zos_ID) ; call erreur(status,.TRUE.,"def_var_zos")
  status = NF90_PUT_ATT(fidM,zos_ID,"coordinates",TRIM(coord_zos)) ; call erreur(status,.TRUE.,"put_att_coordinates_zos_ID")
  status = NF90_PUT_ATT(fidM,zos_ID,"units",TRIM(units_zos)) ; call erreur(status,.TRUE.,"put_att_units_zos_ID")
  status = NF90_PUT_ATT(fidM,zos_ID,"long_name",TRIM(nam_zos)) ; call erreur(status,.TRUE.,"put_att_long_name_zos_ID")
endif
if ( stat_sos .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"sos",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),sos_ID) ; call erreur(status,.TRUE.,"def_var_sos")
  status = NF90_PUT_ATT(fidM,sos_ID,"coordinates",TRIM(coord_sos)) ; call erreur(status,.TRUE.,"put_att_coordinates_sos_ID")
  status = NF90_PUT_ATT(fidM,sos_ID,"units",TRIM(units_sos)) ; call erreur(status,.TRUE.,"put_att_units_sos_ID")
  status = NF90_PUT_ATT(fidM,sos_ID,"long_name",TRIM(nam_sos)) ; call erreur(status,.TRUE.,"put_att_long_name_sos_ID")
endif
if ( stat_tossq .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"tossq",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),tossq_ID) ; call erreur(status,.TRUE.,"def_var_tossq")
  status = NF90_PUT_ATT(fidM,tossq_ID,"coordinates",TRIM(coord_tossq)) ; call erreur(status,.TRUE.,"put_att_coordinates_tossq_ID")
  status = NF90_PUT_ATT(fidM,tossq_ID,"units",TRIM(units_tossq)) ; call erreur(status,.TRUE.,"put_att_units_tossq_ID")
  status = NF90_PUT_ATT(fidM,tossq_ID,"long_name",TRIM(nam_tossq)) ; call erreur(status,.TRUE.,"put_att_long_name_tossq_ID")
endif
if ( stat_tos_min .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"tos_min",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),tos_min_ID) ; call erreur(status,.TRUE.,"def_var_tos_min")
  status = NF90_PUT_ATT(fidM,tos_min_ID,"coordinates",TRIM(coord_tos_min)) ; call erreur(status,.TRUE.,"put_att_coordinates_tos_min_ID")
  status = NF90_PUT_ATT(fidM,tos_min_ID,"units",TRIM(units_tos_min)) ; call erreur(status,.TRUE.,"put_att_units_tos_min_ID")
  status = NF90_PUT_ATT(fidM,tos_min_ID,"long_name",TRIM(nam_tos_min)) ; call erreur(status,.TRUE.,"put_att_long_name_tos_min_ID")
endif
if ( stat_tos_max .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"tos_max",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),tos_max_ID) ; call erreur(status,.TRUE.,"def_var_tos_max")
  status = NF90_PUT_ATT(fidM,tos_max_ID,"coordinates",TRIM(coord_tos_max)) ; call erreur(status,.TRUE.,"put_att_coordinates_tos_max_ID")
  status = NF90_PUT_ATT(fidM,tos_max_ID,"units",TRIM(units_tos_max)) ; call erreur(status,.TRUE.,"put_att_units_tos_max_ID")
  status = NF90_PUT_ATT(fidM,tos_max_ID,"long_name",TRIM(nam_tos_max)) ; call erreur(status,.TRUE.,"put_att_long_name_tos_max_ID")
endif
if ( stat_tos .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"tos",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),tos_ID) ; call erreur(status,.TRUE.,"def_var_tos")
  status = NF90_PUT_ATT(fidM,tos_ID,"coordinates",TRIM(coord_tos)) ; call erreur(status,.TRUE.,"put_att_coordinates_tos_ID")
  status = NF90_PUT_ATT(fidM,tos_ID,"units",TRIM(units_tos)) ; call erreur(status,.TRUE.,"put_att_units_tos_ID")
  status = NF90_PUT_ATT(fidM,tos_ID,"long_name",TRIM(nam_tos)) ; call erreur(status,.TRUE.,"put_att_long_name_tos_ID")
endif
if ( stat_tauuo .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"tauuo",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),tauuo_ID) ; call erreur(status,.TRUE.,"def_var_tauuo")
  status = NF90_PUT_ATT(fidM,tauuo_ID,"coordinates",TRIM(coord_tauuo)) ; call erreur(status,.TRUE.,"put_att_coordinates_tauuo_ID")
  status = NF90_PUT_ATT(fidM,tauuo_ID,"units",TRIM(units_tauuo)) ; call erreur(status,.TRUE.,"put_att_units_tauuo_ID")
  status = NF90_PUT_ATT(fidM,tauuo_ID,"long_name",TRIM(nam_tauuo)) ; call erreur(status,.TRUE.,"put_att_long_name_tauuo_ID")
endif
if ( stat_tauvo .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"tauvo",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),tauvo_ID) ; call erreur(status,.TRUE.,"def_var_tauvo")
  status = NF90_PUT_ATT(fidM,tauvo_ID,"coordinates",TRIM(coord_tauvo)) ; call erreur(status,.TRUE.,"put_att_coordinates_tauvo_ID")
  status = NF90_PUT_ATT(fidM,tauvo_ID,"units",TRIM(units_tauvo)) ; call erreur(status,.TRUE.,"put_att_units_tauvo_ID")
  status = NF90_PUT_ATT(fidM,tauvo_ID,"long_name",TRIM(nam_tauvo)) ; call erreur(status,.TRUE.,"put_att_long_name_tauvo_ID")
endif
if ( stat_uos .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"uos",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),uos_ID) ; call erreur(status,.TRUE.,"def_var_uos")
  status = NF90_PUT_ATT(fidM,uos_ID,"coordinates",TRIM(coord_uos)) ; call erreur(status,.TRUE.,"put_att_coordinates_uos_ID")
  status = NF90_PUT_ATT(fidM,uos_ID,"units",TRIM(units_uos)) ; call erreur(status,.TRUE.,"put_att_units_uos_ID")
  status = NF90_PUT_ATT(fidM,uos_ID,"long_name",TRIM(nam_uos)) ; call erreur(status,.TRUE.,"put_att_long_name_uos_ID")
endif
if ( stat_vos .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"vos",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),vos_ID) ; call erreur(status,.TRUE.,"def_var_vos")
  status = NF90_PUT_ATT(fidM,vos_ID,"coordinates",TRIM(coord_vos)) ; call erreur(status,.TRUE.,"put_att_coordinates_vos_ID")
  status = NF90_PUT_ATT(fidM,vos_ID,"units",TRIM(units_vos)) ; call erreur(status,.TRUE.,"put_att_units_vos_ID")
  status = NF90_PUT_ATT(fidM,vos_ID,"long_name",TRIM(nam_vos)) ; call erreur(status,.TRUE.,"put_att_long_name_vos_ID")
endif
if ( stat_wfo .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"wfo",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),wfo_ID) ; call erreur(status,.TRUE.,"def_var_wfo")
  status = NF90_PUT_ATT(fidM,wfo_ID,"coordinates",TRIM(coord_wfo)) ; call erreur(status,.TRUE.,"put_att_coordinates_wfo_ID")
  status = NF90_PUT_ATT(fidM,wfo_ID,"units",TRIM(units_wfo)) ; call erreur(status,.TRUE.,"put_att_units_wfo_ID")
  status = NF90_PUT_ATT(fidM,wfo_ID,"long_name",TRIM(nam_wfo)) ; call erreur(status,.TRUE.,"put_att_long_name_wfo_ID")
endif
if ( stat_rsntds .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"rsntds",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),rsntds_ID) ; call erreur(status,.TRUE.,"def_var_rsntds")
  status = NF90_PUT_ATT(fidM,rsntds_ID,"coordinates",TRIM(coord_rsntds)) ; call erreur(status,.TRUE.,"put_att_coordinates_rsntds_ID")
  status = NF90_PUT_ATT(fidM,rsntds_ID,"units",TRIM(units_rsntds)) ; call erreur(status,.TRUE.,"put_att_units_rsntds_ID")
  status = NF90_PUT_ATT(fidM,rsntds_ID,"long_name",TRIM(nam_rsntds)) ; call erreur(status,.TRUE.,"put_att_long_name_rsntds_ID")
endif
if ( stat_tohfls .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"tohfls",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),tohfls_ID) ; call erreur(status,.TRUE.,"def_var_tohfls")
  status = NF90_PUT_ATT(fidM,tohfls_ID,"coordinates",TRIM(coord_tohfls)) ; call erreur(status,.TRUE.,"put_att_coordinates_tohfls_ID")
  status = NF90_PUT_ATT(fidM,tohfls_ID,"units",TRIM(units_tohfls)) ; call erreur(status,.TRUE.,"put_att_units_tohfls_ID")
  status = NF90_PUT_ATT(fidM,tohfls_ID,"long_name",TRIM(nam_tohfls)) ; call erreur(status,.TRUE.,"put_att_long_name_tohfls_ID")
endif
if ( stat_taum .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"taum",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),taum_ID) ; call erreur(status,.TRUE.,"def_var_taum")
  status = NF90_PUT_ATT(fidM,taum_ID,"coordinates",TRIM(coord_taum)) ; call erreur(status,.TRUE.,"put_att_coordinates_taum_ID")
  status = NF90_PUT_ATT(fidM,taum_ID,"units",TRIM(units_taum)) ; call erreur(status,.TRUE.,"put_att_units_taum_ID")
  status = NF90_PUT_ATT(fidM,taum_ID,"long_name",TRIM(nam_taum)) ; call erreur(status,.TRUE.,"put_att_long_name_taum_ID")
endif
if ( stat_x20d .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"20d",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),x20d_ID) ; call erreur(status,.TRUE.,"def_var_x20d")
  status = NF90_PUT_ATT(fidM,x20d_ID,"coordinates",TRIM(coord_x20d)) ; call erreur(status,.TRUE.,"put_att_coordinates_x20d_ID")
  status = NF90_PUT_ATT(fidM,x20d_ID,"units",TRIM(units_x20d)) ; call erreur(status,.TRUE.,"put_att_units_x20d_ID")
  status = NF90_PUT_ATT(fidM,x20d_ID,"long_name",TRIM(nam_x20d)) ; call erreur(status,.TRUE.,"put_att_long_name_x20d_ID")
endif
if ( stat_mldkz5 .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"mldkz5",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),mldkz5_ID) ; call erreur(status,.TRUE.,"def_var_mldkz5")
  status = NF90_PUT_ATT(fidM,mldkz5_ID,"coordinates",TRIM(coord_mldkz5)) ; call erreur(status,.TRUE.,"put_att_coordinates_mldkz5_ID")
  status = NF90_PUT_ATT(fidM,mldkz5_ID,"units",TRIM(units_mldkz5)) ; call erreur(status,.TRUE.,"put_att_units_mldkz5_ID")
  status = NF90_PUT_ATT(fidM,mldkz5_ID,"long_name",TRIM(nam_mldkz5)) ; call erreur(status,.TRUE.,"put_att_long_name_mldkz5_ID")
endif
if ( stat_mldr10_1 .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"mldr10_1",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),mldr10_1_ID) ; call erreur(status,.TRUE.,"def_var_mldr10_1")
  status = NF90_PUT_ATT(fidM,mldr10_1_ID,"coordinates",TRIM(coord_mldr10_1)) ; call erreur(status,.TRUE.,"put_att_coordinates_mldr10_1_ID")
  status = NF90_PUT_ATT(fidM,mldr10_1_ID,"units",TRIM(units_mldr10_1)) ; call erreur(status,.TRUE.,"put_att_units_mldr10_1_ID")
  status = NF90_PUT_ATT(fidM,mldr10_1_ID,"long_name",TRIM(nam_mldr10_1)) ; call erreur(status,.TRUE.,"put_att_long_name_mldr10_1_ID")
endif
if ( stat_topthdep .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"topthdep",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),topthdep_ID) ; call erreur(status,.TRUE.,"def_var_topthdep")
  status = NF90_PUT_ATT(fidM,topthdep_ID,"coordinates",TRIM(coord_topthdep)) ; call erreur(status,.TRUE.,"put_att_coordinates_topthdep_ID")
  status = NF90_PUT_ATT(fidM,topthdep_ID,"units",TRIM(units_topthdep)) ; call erreur(status,.TRUE.,"put_att_units_topthdep_ID")
  status = NF90_PUT_ATT(fidM,topthdep_ID,"long_name",TRIM(nam_topthdep)) ; call erreur(status,.TRUE.,"put_att_long_name_topthdep_ID")
endif
if ( stat_pycndep .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"pycndep",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),pycndep_ID) ; call erreur(status,.TRUE.,"def_var_pycndep")
  status = NF90_PUT_ATT(fidM,pycndep_ID,"coordinates",TRIM(coord_pycndep)) ; call erreur(status,.TRUE.,"put_att_coordinates_pycndep_ID")
  status = NF90_PUT_ATT(fidM,pycndep_ID,"units",TRIM(units_pycndep)) ; call erreur(status,.TRUE.,"put_att_units_pycndep_ID")
  status = NF90_PUT_ATT(fidM,pycndep_ID,"long_name",TRIM(nam_pycndep)) ; call erreur(status,.TRUE.,"put_att_long_name_pycndep_ID")
endif
if ( stat_tinv .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"tinv",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),tinv_ID) ; call erreur(status,.TRUE.,"def_var_tinv")
  status = NF90_PUT_ATT(fidM,tinv_ID,"coordinates",TRIM(coord_tinv)) ; call erreur(status,.TRUE.,"put_att_coordinates_tinv_ID")
  status = NF90_PUT_ATT(fidM,tinv_ID,"units",TRIM(units_tinv)) ; call erreur(status,.TRUE.,"put_att_units_tinv_ID")
  status = NF90_PUT_ATT(fidM,tinv_ID,"long_name",TRIM(nam_tinv)) ; call erreur(status,.TRUE.,"put_att_long_name_tinv_ID")
endif
if ( stat_depti .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"depti",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),depti_ID) ; call erreur(status,.TRUE.,"def_var_depti")
  status = NF90_PUT_ATT(fidM,depti_ID,"coordinates",TRIM(coord_depti)) ; call erreur(status,.TRUE.,"put_att_coordinates_depti_ID")
  status = NF90_PUT_ATT(fidM,depti_ID,"units",TRIM(units_depti)) ; call erreur(status,.TRUE.,"put_att_units_depti_ID")
  status = NF90_PUT_ATT(fidM,depti_ID,"long_name",TRIM(nam_depti)) ; call erreur(status,.TRUE.,"put_att_long_name_depti_ID")
endif
if ( stat_blt .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"blt",NF90_FLOAT,(/dimID_x,dimID_y,dimID_time_counter/),blt_ID) ; call erreur(status,.TRUE.,"def_var_blt")
  status = NF90_PUT_ATT(fidM,blt_ID,"coordinates",TRIM(coord_blt)) ; call erreur(status,.TRUE.,"put_att_coordinates_blt_ID")
  status = NF90_PUT_ATT(fidM,blt_ID,"units",TRIM(units_blt)) ; call erreur(status,.TRUE.,"put_att_units_blt_ID")
  status = NF90_PUT_ATT(fidM,blt_ID,"long_name",TRIM(nam_blt)) ; call erreur(status,.TRUE.,"put_att_long_name_blt_ID")
endif

!--- Definition of 3D variables and their attributes :
if ( stat_thetao .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"thetao",NF90_FLOAT,(/dimID_x,dimID_y,dimID_depth,dimID_time_counter/),thetao_ID) ; call erreur(status,.TRUE.,"def_var_thetao")
  status = NF90_PUT_ATT(fidM,thetao_ID,"coordinates",TRIM(coord_thetao)) ; call erreur(status,.TRUE.,"put_att_coordinates_thetao_ID")
  status = NF90_PUT_ATT(fidM,thetao_ID,"units",TRIM(units_thetao)) ; call erreur(status,.TRUE.,"put_att_units_thetao_ID")
  status = NF90_PUT_ATT(fidM,thetao_ID,"long_name",TRIM(nam_thetao)) ; call erreur(status,.TRUE.,"put_att_long_name_thetao_ID")
endif
if ( stat_so .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"so",NF90_FLOAT,(/dimID_x,dimID_y,dimID_depth,dimID_time_counter/),so_ID) ; call erreur(status,.TRUE.,"def_var_so")
  status = NF90_PUT_ATT(fidM,so_ID,"coordinates",TRIM(coord_so)) ; call erreur(status,.TRUE.,"put_att_coordinates_so_ID")
  status = NF90_PUT_ATT(fidM,so_ID,"units",TRIM(units_so)) ; call erreur(status,.TRUE.,"put_att_units_so_ID")
  status = NF90_PUT_ATT(fidM,so_ID,"long_name",TRIM(nam_so)) ; call erreur(status,.TRUE.,"put_att_long_name_so_ID")
endif
if ( stat_uo .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"uo",NF90_FLOAT,(/dimID_x,dimID_y,dimID_depth,dimID_time_counter/),uo_ID) ; call erreur(status,.TRUE.,"def_var_uo")
  status = NF90_PUT_ATT(fidM,uo_ID,"coordinates",TRIM(coord_uo)) ; call erreur(status,.TRUE.,"put_att_coordinates_uo_ID")
  status = NF90_PUT_ATT(fidM,uo_ID,"units",TRIM(units_uo)) ; call erreur(status,.TRUE.,"put_att_units_uo_ID")
  status = NF90_PUT_ATT(fidM,uo_ID,"long_name",TRIM(nam_uo)) ; call erreur(status,.TRUE.,"put_att_long_name_uo_ID")
endif
if ( stat_vo .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"vo",NF90_FLOAT,(/dimID_x,dimID_y,dimID_depth,dimID_time_counter/),vo_ID) ; call erreur(status,.TRUE.,"def_var_vo")
  status = NF90_PUT_ATT(fidM,vo_ID,"coordinates",TRIM(coord_vo)) ; call erreur(status,.TRUE.,"put_att_coordinates_vo_ID")
  status = NF90_PUT_ATT(fidM,vo_ID,"units",TRIM(units_vo)) ; call erreur(status,.TRUE.,"put_att_units_vo_ID")
  status = NF90_PUT_ATT(fidM,vo_ID,"long_name",TRIM(nam_vo)) ; call erreur(status,.TRUE.,"put_att_long_name_vo_ID")
endif
if ( stat_wo .eq. 0 ) then
  status = NF90_DEF_VAR(fidM,"wo",NF90_FLOAT,(/dimID_x,dimID_y,dimID_depth,dimID_time_counter/),wo_ID) ; call erreur(status,.TRUE.,"def_var_wo")
  status = NF90_PUT_ATT(fidM,wo_ID,"coordinates",TRIM(coord_wo)) ; call erreur(status,.TRUE.,"put_att_coordinates_wo_ID")
  status = NF90_PUT_ATT(fidM,wo_ID,"units",TRIM(units_wo)) ; call erreur(status,.TRUE.,"put_att_units_wo_ID")
  status = NF90_PUT_ATT(fidM,wo_ID,"long_name",TRIM(nam_wo)) ; call erreur(status,.TRUE.,"put_att_long_name_wo_ID")
endif                 

!--- Attributes of compulsory variables :
status = NF90_PUT_ATT(fidM,time_average_ID,"time_origin","1989-01-01 00:00:00")         ; call erreur(status,.TRUE.,"put_att_time_average_ID")
status = NF90_PUT_ATT(fidM,time_average_ID,"units","seconds since 1989-01-01 00:00:00") ; call erreur(status,.TRUE.,"put_att_time_average_ID")
status = NF90_PUT_ATT(fidM,time_average_ID,"calendar","gregorian")                      ; call erreur(status,.TRUE.,"put_att_time_average_ID")
status = NF90_PUT_ATT(fidM,time_average_ID,"title","Time")                              ; call erreur(status,.TRUE.,"put_att_time_average_ID")
status = NF90_PUT_ATT(fidM,time_average_ID,"long_name","Time axis")                     ; call erreur(status,.TRUE.,"put_att_time_average_ID")
status = NF90_PUT_ATT(fidM,time_average_ID,"standard_name","time")                      ; call erreur(status,.TRUE.,"put_att_time_average_ID")
!- 
if ( stat_tmax .eq. 0 ) then
status = NF90_PUT_ATT(fidM,time_maximum_ID,"time_origin","1989-01-01 00:00:00")         ; call erreur(status,.TRUE.,"put_att_time_maximum_ID")
status = NF90_PUT_ATT(fidM,time_maximum_ID,"units","seconds since 1989-01-01 00:00:00") ; call erreur(status,.TRUE.,"put_att_time_maximum_ID")
status = NF90_PUT_ATT(fidM,time_maximum_ID,"calendar","gregorian")                      ; call erreur(status,.TRUE.,"put_att_time_maximum_ID")
status = NF90_PUT_ATT(fidM,time_maximum_ID,"title","Time")                              ; call erreur(status,.TRUE.,"put_att_time_maximum_ID")
status = NF90_PUT_ATT(fidM,time_maximum_ID,"long_name","Time axis")                     ; call erreur(status,.TRUE.,"put_att_time_maximum_ID")
status = NF90_PUT_ATT(fidM,time_maximum_ID,"standard_name","time")                      ; call erreur(status,.TRUE.,"put_att_time_maximum_ID")
endif
!-
if ( stat_tmin .eq. 0 ) then
status = NF90_PUT_ATT(fidM,time_minimum_ID,"time_origin","1989-01-01 00:00:00")         ; call erreur(status,.TRUE.,"put_att_time_minimum_ID")
status = NF90_PUT_ATT(fidM,time_minimum_ID,"units","seconds since 1989-01-01 00:00:00") ; call erreur(status,.TRUE.,"put_att_time_minimum_ID")
status = NF90_PUT_ATT(fidM,time_minimum_ID,"calendar","gregorian")                      ; call erreur(status,.TRUE.,"put_att_time_minimum_ID")
status = NF90_PUT_ATT(fidM,time_minimum_ID,"title","Time")                              ; call erreur(status,.TRUE.,"put_att_time_minimum_ID")
status = NF90_PUT_ATT(fidM,time_minimum_ID,"long_name","Time axis")                     ; call erreur(status,.TRUE.,"put_att_time_minimum_ID")
status = NF90_PUT_ATT(fidM,time_minimum_ID,"standard_name","time")                      ; call erreur(status,.TRUE.,"put_att_time_minimum_ID")
endif
!-
!status = NF90_PUT_ATT(fidM,nav_lon_ID,"nav_model","grid_T")                             ; call erreur(status,.TRUE.,"put_att_nav_lon_ID")
status = NF90_PUT_ATT(fidM,nav_lon_ID,"units","degrees_east")                           ; call erreur(status,.TRUE.,"put_att_nav_lon_ID")
status = NF90_PUT_ATT(fidM,nav_lon_ID,"long_name","Longitude")                          ; call erreur(status,.TRUE.,"put_att_nav_lon_ID")
status = NF90_PUT_ATT(fidM,nav_lon_ID,"standard_name","longitude")                      ; call erreur(status,.TRUE.,"put_att_nav_lon_ID")
status = NF90_PUT_ATT(fidM,nav_lon_ID,"axis","X")                                       ; call erreur(status,.TRUE.,"put_att_nav_lon_ID")
!-
!status = NF90_PUT_ATT(fidM,nav_lat_ID,"nav_model","grid_T")                             ; call erreur(status,.TRUE.,"put_att_nav_lat_ID")
status = NF90_PUT_ATT(fidM,nav_lat_ID,"units","degrees_north")                          ; call erreur(status,.TRUE.,"put_att_nav_lat_ID")
status = NF90_PUT_ATT(fidM,nav_lat_ID,"long_name","Latitude")                           ; call erreur(status,.TRUE.,"put_att_nav_lat_ID")
status = NF90_PUT_ATT(fidM,nav_lat_ID,"standard_name","latitude")                       ; call erreur(status,.TRUE.,"put_att_nav_lat_ID")
status = NF90_PUT_ATT(fidM,nav_lat_ID,"axis","Y")                                       ; call erreur(status,.TRUE.,"put_att_nav_lat_ID")
!-
if ( igridZ .gt. 0 ) then
  status = NF90_PUT_ATT(fidM,depth_ID,"axis","Z")                                       ; call erreur(status,.TRUE.,"put_att_axis_depth")
  status = NF90_PUT_ATT(fidM,depth_ID,"units","m")                                      ; call erreur(status,.TRUE.,"put_att_units_depth")
  status = NF90_PUT_ATT(fidM,depth_ID,"positive","down")                                ; call erreur(status,.TRUE.,"put_att_positive_depth")
endif
                          
!--- Global Attributes :
status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Rebuilt from XIOS multiple files using rebuild_outputs.f90") ; call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidA,NF90_GLOBAL,"name",TRIM(GLOB_file_name))          ; call erreur(status,.TRUE.,"put_att_GLOBAL_name")
status = NF90_PUT_ATT(fidA,NF90_GLOBAL,"description",TRIM(GLOB_description)) ; call erreur(status,.TRUE.,"put_att_GLOBAL_description")
status = NF90_PUT_ATT(fidA,NF90_GLOBAL,"conventions",TRIM(GLOB_conventions)) ; call erreur(status,.TRUE.,"put_att_GLOBAL_conventions")
status = NF90_PUT_ATT(fidA,NF90_GLOBAL,"production",TRIM(GLOB_production))   ; call erreur(status,.TRUE.,"put_att_GLOBAL_production")
status = NF90_PUT_ATT(fidA,NF90_GLOBAL,"timeStamp",TRIM(GLOB_timeStamp))     ; call erreur(status,.TRUE.,"put_att_GLOBAL_timeStamp")
                                                      
!--- End of definitions                          
status = NF90_ENDDEF(fidM) ; call erreur(status,.TRUE.,"end_of_definitions") 
                                                      
!--- Values taken by compulsory variable :           
status = NF90_PUT_VAR(fidM,time_average_ID,time_average) ; call erreur(status,.TRUE.,"var_time_average_ID")
if ( stat_tmax .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,time_maximum_ID,time_maximum) ; call erreur(status,.TRUE.,"var_time_maximum_ID")
endif
if ( stat_tmin .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,time_minimum_ID,time_minimum) ; call erreur(status,.TRUE.,"var_time_minimum_ID")
endif
status = NF90_PUT_VAR(fidM,nav_lon_ID,nav_lon_out)       ; call erreur(status,.TRUE.,"var_nav_lon_ID")
status = NF90_PUT_VAR(fidM,nav_lat_ID,nav_lat_out)       ; call erreur(status,.TRUE.,"var_nav_lat_ID")
if ( igridZ .gt. 0 ) then
  status = NF90_PUT_VAR(fidM,depth_ID,depth) ; call erreur(status,.TRUE.,"var_depth_ID")
endif
                                                     
!--- Values taken by 2D variables :
if ( stat_mld_dt02 .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,mld_dt02_ID,mld_dt02_out) ; call erreur(status,.TRUE.,"put_var_mld_dt02")
endif
if ( stat_mldr10_3 .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,mldr10_3_ID,mldr10_3_out) ; call erreur(status,.TRUE.,"put_var_mldr10_3")
endif
if ( stat_zos .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,zos_ID,zos_out) ; call erreur(status,.TRUE.,"put_var_zos")
endif
if ( stat_sos .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,sos_ID,sos_out) ; call erreur(status,.TRUE.,"put_var_sos")
endif
if ( stat_tossq .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,tossq_ID,tossq_out) ; call erreur(status,.TRUE.,"put_var_tossq")
endif
if ( stat_tos_min .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,tos_min_ID,tos_min_out) ; call erreur(status,.TRUE.,"put_var_tos_min")
endif
if ( stat_tos_max .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,tos_max_ID,tos_max_out) ; call erreur(status,.TRUE.,"put_var_tos_max")
endif
if ( stat_tos .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,tos_ID,tos_out) ; call erreur(status,.TRUE.,"put_var_tos")
endif
if ( stat_tauuo .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,tauuo_ID,tauuo_out) ; call erreur(status,.TRUE.,"put_var_tauuo")
endif
if ( stat_tauvo .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,tauvo_ID,tauvo_out) ; call erreur(status,.TRUE.,"put_var_tauvo")
endif
if ( stat_uos .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,uos_ID,uos_out) ; call erreur(status,.TRUE.,"put_var_uos")
endif
if ( stat_vos .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,vos_ID,vos_out) ; call erreur(status,.TRUE.,"put_var_vos")
endif
if ( stat_wfo .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,wfo_ID,wfo_out) ; call erreur(status,.TRUE.,"put_var_wfo")
endif
if ( stat_rsntds .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,rsntds_ID,rsntds_out) ; call erreur(status,.TRUE.,"put_var_rsntds")
endif
if ( stat_tohfls .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,tohfls_ID,tohfls_out) ; call erreur(status,.TRUE.,"put_var_tohfls")
endif
if ( stat_taum .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,taum_ID,taum_out) ; call erreur(status,.TRUE.,"put_var_taum")
endif
if ( stat_x20d .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,x20d_ID,x20d_out) ; call erreur(status,.TRUE.,"put_var_x20d")
endif
if ( stat_mldkz5 .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,mldkz5_ID,mldkz5_out) ; call erreur(status,.TRUE.,"put_var_mldkz5")
endif
if ( stat_mldr10_1 .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,mldr10_1_ID,mldr10_1_out) ; call erreur(status,.TRUE.,"put_var_mldr10_1")
endif
if ( stat_topthdep .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,topthdep_ID,topthdep_out) ; call erreur(status,.TRUE.,"put_var_topthdep")
endif
if ( stat_pycndep .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,pycndep_ID,pycndep_out) ; call erreur(status,.TRUE.,"put_var_pycndep")
endif
if ( stat_tinv .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,tinv_ID,tinv_out) ; call erreur(status,.TRUE.,"put_var_tinv")
endif
if ( stat_depti .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,depti_ID,depti_out) ; call erreur(status,.TRUE.,"put_var_depti")
endif
if ( stat_blt .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,blt_ID,blt_out) ; call erreur(status,.TRUE.,"put_var_blt")
endif

!--- Values taken by 3D variables :
if ( stat_thetao .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,thetao_ID,thetao_out) ; call erreur(status,.TRUE.,"put_var_thetao")
endif
if ( stat_so .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,so_ID,so_out) ; call erreur(status,.TRUE.,"put_var_so")
endif
if ( stat_uo .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,uo_ID,uo_out) ; call erreur(status,.TRUE.,"put_var_uo")
endif
if ( stat_vo .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,vo_ID,vo_out) ; call erreur(status,.TRUE.,"put_var_vo")
endif
if ( stat_wo .eq. 0 ) then
  status = NF90_PUT_VAR(fidM,wo_ID,wo_out) ; call erreur(status,.TRUE.,"put_var_wo")
endif                 

 
!--- Close netcdf file :            
status = NF90_CLOSE(fidM) ; call erreur(status,.TRUE.,"close_new_file")         

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
    WRITE(*,*) 'ERROR: ', iret
    WRITE(*,*) 'IN FORTRAN SCRIPT, FOR THE FOLLOWING ACTION : ', TRIM(chaine)
    message=NF90_STRERROR(iret)
    WRITE(*,*) 'WHICH MEANS:',TRIM(message)
    IF ( lstop ) STOP
  ENDIF
  !
END SUBROUTINE erreur
