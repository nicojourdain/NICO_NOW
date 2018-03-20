!
!************************************************************************
! Fortran 95 OPA Nesting tools						*
!									*
!     Copyright (C) 2005 Florian Lemari�(Florian.Lemarie@imag.fr)	*
!									*
!************************************************************************
!
PROGRAM create_rstrt_trc
  !
  USE NETCDF
  USE bilinear_interp
  USE bicubic_interp
  USE agrif_readwrite
  USE io_netcdf 
  USE agrif_extrapolation
  USE agrif_interpolation
  USE agrif_partial_steps        
  !
  IMPLICIT NONE
  !
  !************************************************************************
  ! 									*
  ! PROGRAM  CREATE_RSTRT_TRC     					*
  !									*
  ! program to interpolate parent grid restart file to child grid		*
  !									*
  !									*
  !Interpolation is carried out using bilinear interpolation		*
  !routine from SCRIP package						*		
  !									*
  !http://climate.lanl.gov/Software/SCRIP/				*
  !************************************************************************
  !
  ! variables declaration
  !      
  CHARACTER*20,DIMENSION(:),POINTER :: Ncdf_varname => NULL()
  CHARACTER*20 :: vert_coord_name
  CHARACTER*1 :: posvar
  CHARACTER*3 :: prefix
  CHARACTER*20:: suffix 
  CHARACTER*100 :: Child_file,Childcoordinates,varname,varname2,Childbathy,Childbathymeter   
  REAL*8, POINTER, DIMENSION(:,:,:) :: fse3u, fse3v,fse3t => NULL()
  REAL*8, POINTER, DIMENSION(:,:) :: lonChild,latChild => NULL()
  REAL*8, POINTER, DIMENSION(:,:) :: lonParent,latParent => NULL()
  REAL*8, POINTER, DIMENSION(:,:,:) :: cvol => NULL()
  REAL*8, POINTER, DIMENSION(:,:,:) :: tabvar3d,tabinterp3d,mask => NULL()
  REAL*8, POINTER, DIMENSION(:,:,:,:) :: trb,trn => NULL()
  REAL*8, POINTER, DIMENSION(:,:,:,:) :: tabinterp4d,tabvar1,tabvar2,tabvar3 => NULL()
  REAL*8, POINTER, DIMENSION(:) :: timedepth_temp => NULL()
  REAL*8, POINTER, DIMENSION(:) :: tabtemp1D,nav_lev => NULL()
  INTEGER, POINTER, DIMENSION(:) :: tabtemp1DInt => NULL()
  REAL*8, POINTER, DIMENSION(:,:) :: tabtemp2D,zwf => NULL()
  REAL*8, POINTER, DIMENSION(:,:) :: e1f,e2f,e1v,e2v,e1u,e2u,e1t,e2t => NULL()
  REAL*8, POINTER, DIMENSION(:,:,:,:) :: tabtemp4D => NULL()
  INTEGER,DIMENSION(:),POINTER :: src_add,dst_add  => NULL()
  REAL*8,DIMENSION(:,:),POINTER :: matrix,bathy_G0 => NULL()
  LOGICAL,DIMENSION(:,:,:),POINTER :: Tmask => NULL()
  LOGICAL,DIMENSION(:,:),POINTER :: masksrc => NULL()
  LOGICAL, DIMENSION(:,:,:), POINTER :: detected_pts
  LOGICAL :: Interpolation,Extrapolation,Pacifique,op
  REAL*8 :: za1,za0,zsur,zacr,zkth,zdepth,zdepwp,zmin,zmax,zdiff,ze3tp,ze3wp,ztrcor,zvolk
  INTEGER :: narg,iargc,ncid,x,y,t,z,nbvert_lev,m
  REAL*8 :: now_wght,before_wght, ztrmas,zcoef
  INTEGER :: i,j,k,ji,jj,jk,status,varid,numdims
  CHARACTER(len=20),DIMENSION(4) :: dimnames
  CHARACTER(len=80) :: namelistname
  TYPE(Coordinates) :: G0,G1
  INTEGER :: jpi,jpj,jpk,jpni,jpnj,jpnij,jpiglo,jpjglo,nlcit,nlcjt,nldit
  INTEGER :: nldjt,nleit,nlejt,nimppt,njmppt
  REAL*8 :: tabtemp0dreal
  INTEGER :: tabtemp0dint
  CHARACTER(len=20) :: timedimname

  !       
  narg = iargc()
  IF (narg == 0) THEN
     namelistname = 'namelist.input'
  ELSE
     CALL getarg(1,namelistname)
  ENDIF
  !
  ! read input file
  !
  CALL read_namelist(namelistname)
  !      
  IF(TRIM(restart_trc_file) == '/NULL') THEN
     WRITE(*,*) 'no tracers restart file specified in ',TRIM(namelistname)
     STOP
  ENDIF

  timedimname = 't'

  !
  WRITE(*,*) ''
  WRITE(*,*) 'Interpolation of restart file : ',TRIM(restart_trc_file)
  WRITE(*,*) ''
  !
  CALL Read_Ncdf_VarName(restart_trc_file,Ncdf_varname)
  !       
  CALL set_child_name(parent_coordinate_file,Childcoordinates)   
  CALL set_child_name(parent_meshmask_file,Childbathy) 
  CALL set_child_name(parent_bathy_meter,Childbathymeter)   
  !
  ! create this file
  !
  CALL set_child_name(restart_trc_file,Child_file)
  status = nf90_create(Child_file,NF90_WRITE,ncid)
  status = nf90_close(ncid)
  WRITE(*,*) 'Child grid restart file name = ',TRIM(Child_file)      
  WRITE(*,*) ''
  ! 
  ! read dimensions in parent restart file
  !
  CALL Read_Ncdf_dim('x',restart_trc_file,x)
  CALL Read_Ncdf_dim('y',restart_trc_file,y) 
  CALL Read_Ncdf_dim('z',restart_trc_file,z)

  IF( z .NE. N ) THEN
     WRITE(*,*) '***'
     WRITE(*,*) 'Number of vertical levels doesn t match between namelist and restart file'
     WRITE(*,*) 'Please check the values in namelist file'
     STOP
  ENDIF
  !
  ! mask initialization for extrapolation and interpolation
  ! 
  WRITE(*,*) 'mask initialisation on coarse and fine grids'
  !           
  status = Read_Local_Coordinates(parent_coordinate_file,G0,(/jpizoom,jpjzoom/),(/x,y/))
  status = Read_Coordinates(Childcoordinates,G1,Pacifique)
  !
  !longitude modification if child domain covers Pacific ocean area
  !      
  IF( Pacifique ) THEN
     WHERE( G0%nav_lon < 0 )
        G0%nav_lon = G0%nav_lon + 360.
     END WHERE
     WHERE( G1%nav_lon < 0 )
        G1%nav_lon = G1%nav_lon + 360.
     END WHERE
  ENDIF
  !
  CALL Init_tmask(parent_meshmask_file,G0,x,y)
  CALL Init_tmask(childbathy,G1,nxfin,nyfin)
  
  ALLOCATE(fse3u(nxfin,nyfin,z),fse3v(nxfin,nyfin,z),fse3t(nxfin,nyfin,z),cvol(nxfin,nyfin,z))
    !      
    status = Read_Bathymeter(TRIM(Childbathymeter),G1)
    CALL get_scale_factors( G1,fse3t,fse3u,fse3v )
                          
  DO jk =1,z
  cvol(:,:,jk) = G1%e1t(:,:)*G1%e2t(:,:)*fse3t(:,:,jk)*G1%tmask(:,:,jk)
  END DO 
!  CALL Init_mask(childbathy,G1,1,1)

  G0%tmask = 1.    


  status = nf90_open(TRIM(restart_trc_file), NF90_NOWRITE, ncid) ! Open dataset
  DO k=1,z
     ALLOCATE(tabvar1(x,y,1,1))
     !
     status = nf90_inq_varid(ncid, "TRNDIC", VarId) !PISCES
     IF (status == nf90_noerr) THEN
        CALL Read_Ncdf_var('TRNDIC',TRIM(restart_trc_file),tabvar1,1,k)
     ELSE
        status = nf90_inq_varid(ncid, "TRNNO3"  , VarId) ! LOBSTER
        IF (status == nf90_noerr) THEN
           CALL Read_Ncdf_var('TRNNO3',TRIM(restart_trc_file),tabvar1,1,k)
        ELSE
           status = nf90_inq_varid(ncid, "TRNCFC11", VarId) ! CFC
           IF (status == nf90_noerr) THEN
              CALL Read_Ncdf_var('TRNCFC11',TRIM(restart_trc_file),tabvar1,1,k)
           ELSE
              status = nf90_inq_varid(ncid, "TRNCLR  ", VarId) ! My TRC
              IF (status == nf90_noerr) THEN
                 CALL Read_Ncdf_var('TRNCLR',TRIM(restart_trc_file),tabvar1,1,k)
              ELSE
                 WRITE(*,*) 'No suitable tracer found to build the mask '
              ENDIF
           ENDIF
        ENDIF
     ENDIF
     WHERE( tabvar1(:,:,1,1) == 0. ) 
        G0%tmask(:,:,k) = 0.
     END WHERE
     DEALLOCATE(tabvar1)
  END DO

  !
  ! write dimensions in output file
  !          
  CALL Write_Ncdf_dim('x',Child_file,nxfin)
  CALL Write_Ncdf_dim('y',Child_file,nyfin)
  CALL Write_Ncdf_dim('z',Child_file,z)
  CALL Write_Ncdf_dim(TRIM(timedimname),Child_file,0) 
  !
  !
  VARIABLE :  DO i = 1,SIZE(Ncdf_varname)      
     !      
     ! loop on variables names
     !      
     SELECT CASE (TRIM(Ncdf_varname(i)))
        !
        !copy nav_lon from child coordinates to output file      
        !
     CASE('nav_lon')
        WRITE(*,*) 'copy nav_lon'
        CALL Read_Ncdf_var('nav_lon',TRIM(Childcoordinates),tabtemp2D) 
        CALL Write_Ncdf_var('nav_lon',(/'x','y'/),Child_file,tabtemp2D,'float')
        CALL Copy_Ncdf_att('nav_lon',TRIM(restart_trc_file),Child_file, &
             MINVAL(tabtemp2D),MAXVAL(tabtemp2D))
        DEALLOCATE(tabtemp2D)
        Interpolation = .FALSE.
        !	     
        !copy nav_lat from child coordinates to output file
        !
     CASE('nav_lat')             
        WRITE(*,*) 'copy nav_lat'
        CALL Read_Ncdf_var('nav_lat',TRIM(Childcoordinates),tabtemp2D) 
        CALL Write_Ncdf_var('nav_lat',(/'x','y'/),Child_file,tabtemp2D,'float')
        CALL Copy_Ncdf_att('nav_lat',TRIM(restart_trc_file),Child_file, &
             MINVAL(tabtemp2D),MAXVAL(tabtemp2D)) 
        DEALLOCATE(tabtemp2D)
        Interpolation = .FALSE.
        !
        !copy nav_lev from restart_file to output file
        !
     CASE('nav_lev')

        WRITE(*,*) 'copy nav_lev'
        CALL Read_Ncdf_var('nav_lev',TRIM(restart_trc_file),nav_lev) 
        CALL Write_Ncdf_var('nav_lev','z',Child_file,nav_lev,'float')
        CALL Copy_Ncdf_att('nav_lev',TRIM(restart_trc_file),Child_file)      
        Interpolation = .FALSE.
        !
        !copy time from restart_file to output file                       
        !
     CASE('time_counter')
        WRITE(*,*) 'copy time_counter'
        CALL Read_Ncdf_var('time_counter',TRIM(restart_trc_file),tabtemp1D) 
        tabtemp1D = tabtemp1D * rhot
        CALL Write_Ncdf_var('time_counter',TRIM(timedimname),Child_file,tabtemp1D,'double')
        CALL Copy_Ncdf_att('time_counter',TRIM(restart_trc_file),Child_file) 
        DEALLOCATE(tabtemp1D)
        Interpolation = .FALSE.
        !
        !copy info from restart_file to output file
        !
     CASE('arak0') 
        WRITE(*,*) 'copy trp info'        
        CALL Read_Ncdf_var('arak0',TRIM(restart_trc_file),tabtemp0dreal) 
        CALL Write_Ncdf_var('arak0',Child_file,tabtemp0dreal,'double')
        CALL Copy_Ncdf_att('arak0',TRIM(restart_trc_file),Child_file) 
        Interpolation = .FALSE. 
        !
     CASE('kt') 
        WRITE(*,*) 'copy kt'
        CALL Read_Ncdf_var('kt',TRIM(restart_trc_file),tabtemp0dreal)  
        tabtemp0dreal = tabtemp0dreal * rhot
        CALL Write_Ncdf_var('kt',Child_file,tabtemp0dreal,'double')
        CALL Copy_Ncdf_att('kt',TRIM(restart_trc_file),Child_file) 
        Interpolation = .FALSE.
        !
     CASE DEFAULT
        varname = Ncdf_varname(i) 
        WRITE(*,*) TRIM(varname),' interpolation ...'     
        vert_coord_name = 'z'
        posvar='T'
        Interpolation = .TRUE.            
        !      
     END SELECT
     IF( Interpolation ) THEN
        !		  
        nbvert_lev = z 
        !                  
        t = 1

        ALLOCATE(detected_pts(SIZE(G0%tmask,1),SIZE(G0%tmask,2),nbvert_lev))                             
        ALLOCATE(tabvar1(x,y,1,2))
        ALLOCATE(tabvar2(x,y,1,1))
        ALLOCATE(tabvar3(x,y,1,1))
        ALLOCATE(masksrc(x,y))
        ALLOCATE(tabinterp4d(nxfin,nyfin,1,1)) 

        !	    
        DO n = 1,nbvert_lev
           !
           WRITE(*,*) 'interpolate/extrapolate ', &
                TRIM(varname),' for vertical level = ',n   
           !
           CALL Read_Ncdf_var(varname,TRIM(restart_trc_file),tabvar1,t,n)
           IF(n==1) THEN
              !                            
           ELSE IF (n==2) THEN
              tabvar2(:,:,:,1) = tabvar1(:,:,:,2)
           ELSE
              tabvar3(:,:,:,1) = tabvar2(:,:,:,1)
              tabvar2(:,:,:,1) = tabvar1(:,:,:,2)
           ENDIF
           ! 
           !
           IF(MAXVAL(G1%tmask(:,:,n)) == 0.) THEN
              tabinterp4d = 0.0
              WRITE(*,*) 'only land points on level ',n                     
           ELSE
              CALL extrap_detect(G0,G1,detected_pts(:,:,n),n)                                            

              CALL correct_field(detected_pts(:,:,n),tabvar1,tabvar2,&
                   tabvar3,G0,nav_lev,masksrc,n)                                 

              SELECT CASE(TRIM(interp_type))
              CASE('bilinear')                                                       
                 CALL get_remap_matrix(G0%nav_lat,G1%nav_lat, &
                      G0%nav_lon,G1%nav_lon,masksrc,matrix,src_add,dst_add)
                 CALL make_remap(tabvar1(:,:,1,1),tabinterp4d(:,:,1,1),nxfin,nyfin, &
                      matrix,src_add,dst_add)     
              CASE('bicubic')                                   
                 CALL get_remap_bicub(G0%nav_lat,G1%nav_lat, &
                      G0%nav_lon,G1%nav_lon,masksrc,matrix,src_add,dst_add)
                 CALL make_bicubic_remap(tabvar1(:,:,1,1),masksrc,tabinterp4d(:,:,1,1),&
                      nxfin,nyfin,matrix,src_add,dst_add) 
              END SELECT
              !                      
              CALL Correctforconservation(tabvar1(:,:,1,1),tabinterp4d(:,:,1,1), &
                   G0%e1t,G0%e2t,G1%e1t,G1%e2t,nxfin,nyfin,posvar,imin-jpizoom+1,jmin-jpjzoom+1)
           ENDIF

           tabinterp4d(:,:,1,1) =  tabinterp4d(:,:,1,1) * G1%tmask(:,:,n)
           ztrmas = 0.0
           ztrcor = 0.0
           DO jj=1, nyfin
           DO ji=1, nxfin
              zvolk = cvol(ji,jj,n)
              ztrcor = ztrcor + MIN( 0., tabinterp4d(ji,jj,1,1) ) * zvolk
              tabinterp4d(ji,jj,1,1) = MAX( 0., tabinterp4d(ji,jj,1,1)) 
              ztrmas  = ztrmas + tabinterp4d(ji,jj,1,1)  * zvolk
           END DO
           END DO
           IF( ztrcor .LT. 0. ) THEN
               WRITE(*,*) 'Correcting negative concentration'    
               zcoef = 1. + ztrcor / ztrmas
               tabinterp4d(:,:,1,1) =  tabinterp4d(:,:,1,1) *zcoef * G1%tmask(:,:,n)               
           ENDIF        
           IF(MINVAL(tabinterp4d(:,:,1,1)) .LT.0. ) STOP

           !

           status = nf90_inq_varid(ncid,TRIM(varname) , VarId)
           status = nf90_inquire_variable(ncid, VarId, ndims=numdims)
           IF( numdims == 3) THEN
              dimnames(1)='x'
              dimnames(2)='y'
              dimnames(3)=TRIM(timedimname)
              ALLOCATE(tabvar3d(SIZE(tabinterp4d,1),SIZE(tabinterp4d,2),SIZE(tabinterp4d,3)))
              tabvar3d=tabinterp4d(:,:,:,1)
              CALL Write_Ncdf_var(TRIM(varname),dimnames, &
                   Child_file,tabvar3d,t,'double')
              DEALLOCATE(tabvar3d)
           ELSE       
              dimnames(1)='x'
              dimnames(2)='y'
              dimnames(3)=vert_coord_name
              dimnames(4)=TRIM(timedimname)
              CALL Write_Ncdf_var(TRIM(varname),dimnames, &
                   Child_file,tabinterp4d,t,n,'double')
           ENDIF
           !
           CALL Copy_Ncdf_att(TRIM(varname),TRIM(restart_trc_file),Child_file)
           !
           IF(ASSOCIATED(matrix)) DEALLOCATE(matrix,src_add,dst_add)   
           !
           IF( numdims == 3) CYCLE VARIABLE
           !
        END DO
        ! 
        DEALLOCATE(detected_pts)
        DEALLOCATE(tabinterp4d)
        DEALLOCATE(tabvar1,tabvar2,tabvar3)
        DEALLOCATE(masksrc)  
        !                     
     ENDIF

     prefix = varname(1:3)
     suffix = varname(4:LEN_TRIM(varname))   

     IF(rhot == 1 .OR. prefix/= 'TRB') THEN
        WRITE(*,*) ''	 
        WRITE(*,*) 'no time interpolation for', varname
     ELSE   
        WRITE(*,*) ''
        WRITE(*,*) 'time interpolation for', varname
        ALLOCATE(trn(nxfin,nyfin,z,1),trb(nxfin,nyfin,z,1))
        varname2 = 'TRN'//TRIM(suffix)
        now_wght = (rhot-1.)/rhot
        before_wght = 1./rhot
        !    
        CALL Read_Ncdf_var(TRIM(varname),Child_file,trb)
        CALL Read_Ncdf_var(TRIM(varname2),Child_file,trn)            
        trb = now_wght*trn + before_wght*trb

        dimnames(1)='x'
        dimnames(2)='y'
        dimnames(3)='z'
        dimnames(4)=TRIM(timedimname)
        CALL Write_Ncdf_var(TRIM(varname),dimnames,Child_file,trb,'double')
        !    
     ENDIF

  END DO VARIABLE
  !
  WRITE(*,*) ' '
  WRITE(*,*) '******* restart file successfully created *******' 
  WRITE(*,*) ' '
  !
  STOP 
END PROGRAM create_rstrt_trc


