!
!************************************************************************
! Fortran 95 OPA Nesting tools						*
!									*
!     Copyright (C) 2005 Florian Lemari�(Florian.Lemarie@imag.fr)	*
!									*
!************************************************************************
!
PROGRAM create_rstrt
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
  ! PROGRAM  CREATE_RSTRT							*
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
  CHARACTER*100 :: Child_file,Childcoordinates,varname,Childbathy,Childbathymeter   
  REAL*8, POINTER, DIMENSION(:,:) :: lonChild,latChild => NULL()
  REAL*8, POINTER, DIMENSION(:,:) :: lonParent,latParent => NULL()
  REAL*8, POINTER, DIMENSION(:,:,:) :: tabvar3d,tabinterp3d,mask => NULL()
  REAL*8, POINTER, DIMENSION(:,:,:) :: fmask,fse3u,fse3v,fse3t => NULL()
  REAL*8, POINTER, DIMENSION(:,:,:,:) :: un,ub,vn,vb,tn,tb => NULL()
  REAL*8, POINTER, DIMENSION(:,:,:,:) :: sshn,sshb,sb,sn,gcx,gcxb => NULL()      
  REAL*8, POINTER, DIMENSION(:,:,:,:) :: tabinterp4d,tabvar1,tabvar2,tabvar3 => NULL()
  REAL*8, POINTER, DIMENSION(:,:,:,:) :: rotn,rotb,hdivn,hdivb => NULL()
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
  REAL*8 :: za1,za0,zsur,zacr,zkth,zdepth,zdepwp,zmin,zmax,zdiff,ze3tp,ze3wp       
  INTEGER :: narg,iargc,ncid,x,y,t,z,z_a,x_a,y_a,z_b,nbvert_lev,m
  REAL*8 :: now_wght,before_wght
  INTEGER :: i,j,k,status,ji,jj
  CHARACTER(len=20),DIMENSION(4) :: dimnames
  CHARACTER(len=80) :: namelistname
  TYPE(Coordinates) :: G0,G1
  INTEGER :: jpi,jpj,jpk,jpni,jpnj,jpnij,jpiglo,jpjglo,nlcit,nlcjt,nldit
  INTEGER :: nldjt,nleit,nlejt,nimppt,njmppt
  REAL*8 :: tabtemp0dreal
  INTEGER :: tabtemp0dint
  CHARACTER(len=20) :: timedimname

  !      
  ! Variables for dimg
  ! 
  INTEGER :: ino0, it0, ipcg0, isor0, itke0,nfice,nfbulk
  INTEGER :: irecl8, irec,ndastp,narea,nsolv
  INTEGER :: jk,kt            ! dummy loop indices
  INTEGER :: inum             ! temporary logical unit
  INTEGER :: ios1 , ios2      ! flag for ice and bulk in the current run
  INTEGER :: ios3             ! flag for free surface.  0 = none 1 = yes.  0 = none 1 = yes
  INTEGER :: ios4             ! flag for coupled (1) or not (0)    
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
  IF(TRIM(restart_file) == '/NULL') THEN
     WRITE(*,*) 'no restart file specified in ',TRIM(namelistname)
     STOP
  END IF

  IF (iom_activated) THEN
     timedimname = 't'
  ELSE
     timedimname='time'
  ENDIF

  !
  WRITE(*,*) ''
  WRITE(*,*) 'Interpolation of restart file : ',TRIM(restart_file)
  WRITE(*,*) ''
  !
  CALL Read_Ncdf_VarName(restart_file,Ncdf_varname)
  !       
  CALL set_child_name(parent_coordinate_file,Childcoordinates)   
  CALL set_child_name(parent_meshmask_file,Childbathy) 
  CALL set_child_name(parent_bathy_meter,Childbathymeter)   
  !
  ! create this file
  !
  IF( .NOT. dimg ) THEN           
     CALL set_child_name(restart_file,Child_file)
     status = nf90_create(Child_file,NF90_WRITE,ncid)
     status = nf90_close(ncid)
     WRITE(*,*) 'Child grid restart file name = ',TRIM(Child_file)      
     WRITE(*,*) ''
  ENDIF

  ! 
  ! read dimensions in parent restart file
  !
  CALL Read_Ncdf_dim('x',restart_file,x)
  CALL Read_Ncdf_dim('y',restart_file,y) 
  CALL Read_Ncdf_dim('z',restart_file,z)
  CALL Read_Ncdf_dim('x_a',restart_file,x_a)
  CALL Read_Ncdf_dim('y_a',restart_file,y_a)
  CALL Read_Ncdf_dim('z_a',restart_file,z_a)
  CALL Read_Ncdf_dim('z_b',restart_file,z_b)

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
     !
     WHERE( G0%nav_lon < 0 )
        G0%nav_lon = G0%nav_lon + 360.
     END WHERE
     !              
     WHERE( G1%nav_lon < 0 )
        G1%nav_lon = G1%nav_lon + 360.
     END WHERE
     !
  ENDIF
  !
  CALL Init_mask(parent_meshmask_file,G0,x,y)
  CALL Init_mask(childbathy,G1,1,1)

  G0%tmask = 1.    

  DO k=1,z
     ALLOCATE(tabvar1(x,y,1,1))
     CALL Read_Ncdf_var('sn',TRIM(restart_file),tabvar1,1,k)
     WHERE( tabvar1(:,:,1,1) == 0. ) 
        G0%tmask(:,:,k) = 0.
     END WHERE
     DEALLOCATE(tabvar1)
  END DO
  !
  G0%umask(1:x-1,:,:) = G0%tmask(1:x-1,:,:)*G0%tmask(2:x,:,:)
  G0%vmask(:,1:y-1,:) = G0%tmask(:,1:y-1,:)*G0%tmask(:,2:y,:)
  !
  G0%umask(x,:,:) = G0%tmask(x,:,:)
  G0%vmask(:,y,:) = G0%tmask(:,y,:)
  !      
  G0%fmask(1:x-1,1:y-1,:) = G0%tmask(1:x-1,1:y-1,:)*G0%tmask(2:x,1:y-1,:)* &
       G0%tmask(1:x-1,2:y,:)*G0%tmask(2:x,2:y,:) 
  !
  G0%fmask(x,:,:) = G0%tmask(x,:,:)
  G0%fmask(:,y,:) = G0%tmask(:,y,:)
  !
  !   *****   ***  **   ** ******
  !   *    *   *   * * * * *
  !   *    *   *   *  *  * *  ***
  !   *    *   *   *     * *    *
  !   *****   ***  *     * ******
  !
  IF(dimg) THEN
     !        
     WRITE(*,*) 'create dimg restart file'
     DO m = 7,1000 
        INQUIRE(Unit=inum,Opened=op)
        IF( .NOT. op ) THEN
           inum = m
           EXIT
        ENDIF
     ENDDO
     !
     inum = 11
     irecl8 = nxfin * nyfin * 8
     OPEN(inum,FILE=TRIM(dimg_output_file),FORM='UNFORMATTED',  &
          ACCESS='DIRECT',RECL=irecl8)
     !       
     CALL Read_Ncdf_var('info',TRIM(restart_file),tabtemp4D)    
     !        
     ino0 = NINT(tabtemp4D(1,1,1,1))
     it0 = NINT(tabtemp4D(1,1,2,1))*rhot
     WRITE(*,*) 'restart file created for kt = ',it0
     ipcg0 = NINT(tabtemp4D(1,1,3,1))
     isor0 = NINT(tabtemp4D(1,1,4,1))
     itke0 = 0
     IF(tabtemp4D(1,1,5,1)==1.) itke0 = 1 
     ndastp = NINT(tabtemp4D(1,1,6,1))
     ! number of elapsed days since the begining of the run              
     !
     DEALLOCATE(tabtemp4D)
     !
     IF (isor0 + 1 == 3) THEN 
        isor0 = 2
        ipcg0 = 2
     ENDIF
     !        
     CALL Read_Ncdf_var('nfice',TRIM(restart_file),tabtemp4D)     
     nfice = NINT(tabtemp4D(1,1,1,1))
     DEALLOCATE(tabtemp4D)
     !
     CALL Read_Ncdf_var('nfbulk',TRIM(restart_file),tabtemp4D)     
     nfbulk = NINT(tabtemp4D(1,1,1,1))
     DEALLOCATE(tabtemp4D)
     !
     nfice = 5
     nfbulk = 5
     !
     ios1 = 0
     ios2 = 0
     ios3 = 1          ! flag for free surface.  0 = none 1 = yes.  0
     ios4 = 0
     narea = 1
     jpi = nxfin
     jpj = nyfin
     jpk = z
     jpni = 1
     jpnj = 1
     jpnij = 1
     narea = 1
     jpiglo = nxfin
     jpjglo = nyfin
     nlcit = nxfin
     nlcjt = nyfin
     nldit = 1
     nldjt = 1
     nleit = nxfin
     nlejt = nyfin
     nimppt = 1
     njmppt = 1
     !
     PRINT*,'jpi = ',jpi
     PRINT*,'jpj = ',jpj
     PRINT*,'jpk = ',jpk
     PRINT*,'nfice = ',nfice
     PRINT*,'nfbulk = ',nfbulk
     PRINT*,'ndastp = ',ndastp
     !
     WRITE(inum,REC=1) irecl8, ino0, it0, isor0, ipcg0, itke0, &
          nfice, nfbulk , ios1, ios2, ios3, ios4, &
          ndastp, adatrj, jpi, jpj, jpk,  &
          jpni, jpnj, jpnij, narea, jpiglo, jpjglo, &
          nlcit, nlcjt, nldit, nldjt, nleit, nlejt, nimppt, njmppt
     !          
     irec = 2
     !
     !   *****    *****    ******
     !   *        *    *   *   
     !   *        *    *   *** 
     !   *        *    *   *   
     !   *****    *****    *  
     !
  ELSE


     !
     ! write dimensions in output file
     !          
     CALL Write_Ncdf_dim('x',Child_file,nxfin)
     CALL Write_Ncdf_dim('y',Child_file,nyfin)
     CALL Write_Ncdf_dim('z',Child_file,z)
     CALL Write_Ncdf_dim(TRIM(timedimname),Child_file,0) 
     IF (.NOT.iom_activated) THEN
        CALL Write_Ncdf_dim('x_a',Child_file,x_a)
        CALL Write_Ncdf_dim('y_a',Child_file,y_a)
        CALL Write_Ncdf_dim('z_a',Child_file,z_a)
        CALL Write_Ncdf_dim('z_b',Child_file,z_b)
     ENDIF
     !
     !
  ENDIF






  !
  !
  !
  DO i = 1,SIZE(Ncdf_varname)      
     !      
     ! loop on variables names
     !      
     SELECT CASE (TRIM(Ncdf_varname(i)))
        !
        !copy nav_lon from child coordinates to output file      
        !
     CASE('nav_lon')
        IF(.NOT. dimg ) THEN
           WRITE(*,*) 'copy nav_lon'
           CALL Read_Ncdf_var('nav_lon',TRIM(Childcoordinates),tabtemp2D) 
           CALL Write_Ncdf_var('nav_lon',(/'x','y'/),Child_file,tabtemp2D,'float')
           CALL Copy_Ncdf_att('nav_lon',TRIM(restart_file),Child_file, &
                MINVAL(tabtemp2D),MAXVAL(tabtemp2D))
           DEALLOCATE(tabtemp2D)
           Interpolation = .FALSE.
        ENDIF
        !	     
        !copy nav_lat from child coordinates to output file
        !
     CASE('nav_lat')             
        IF(.NOT. dimg ) THEN
           WRITE(*,*) 'copy nav_lat'
           CALL Read_Ncdf_var('nav_lat',TRIM(Childcoordinates),tabtemp2D) 
           CALL Write_Ncdf_var('nav_lat',(/'x','y'/),Child_file,tabtemp2D,'float')
           CALL Copy_Ncdf_att('nav_lat',TRIM(restart_file),Child_file, &
                MINVAL(tabtemp2D),MAXVAL(tabtemp2D)) 
           DEALLOCATE(tabtemp2D)
           Interpolation = .FALSE.
        ENDIF
        !
        !copy nav_lev from restart_file to output file
        !
     CASE('nav_lev')

        WRITE(*,*) 'copy nav_lev'
        CALL Read_Ncdf_var('nav_lev',TRIM(restart_file),nav_lev) 
        IF(.NOT. dimg ) THEN
           CALL Write_Ncdf_var('nav_lev','z',Child_file,nav_lev,'float')
           CALL Copy_Ncdf_att('nav_lev',TRIM(restart_file),Child_file)      
        ENDIF
        Interpolation = .FALSE.
        !
        !copy time from restart_file to output file                       
        !
     CASE('time')
        IF(.NOT. dimg ) THEN
           WRITE(*,*) 'copy time'
           CALL Read_Ncdf_var('time',TRIM(restart_file),tabtemp1D) 
           CALL Write_Ncdf_var('time',TRIM(timedimname),Child_file,tabtemp1D,'float')
           CALL Copy_Ncdf_att('time',TRIM(restart_file),Child_file) 
           DEALLOCATE(tabtemp1D)
           Interpolation = .FALSE.
        ENDIF
        !copy time from restart_file to output file                       
        !
     CASE('time_counter')
        IF(.NOT. dimg ) THEN
           WRITE(*,*) 'copy time_counter'
           CALL Read_Ncdf_var('time_counter',TRIM(restart_file),tabtemp1D) 
           tabtemp1D = tabtemp1D * rhot
           CALL Write_Ncdf_var('time_counter',TRIM(timedimname),Child_file,tabtemp1D,'double')
           CALL Copy_Ncdf_att('time_counter',TRIM(restart_file),Child_file) 
           DEALLOCATE(tabtemp1D)
           Interpolation = .FALSE.
        ENDIF
        !
        !copy time_steps from restart_file to output file             
        !
     CASE('time_steps')
        IF(.NOT. dimg ) THEN
           WRITE(*,*) 'copy time_steps'
           CALL Read_Ncdf_var('time_steps',TRIM(restart_file),tabtemp1DInt) 
           CALL Write_Ncdf_var('time_steps',TRIM(timedimname),Child_file,tabtemp1DInt)
           CALL Copy_Ncdf_att('time_steps',TRIM(restart_file),Child_file) 
           DEALLOCATE(tabtemp1DInt)
           Interpolation = .FALSE.    
        ENDIF
        !
        !copy info from restart_file to output file
        !
     CASE('info') 
        IF(.NOT. dimg ) THEN
           WRITE(*,*) 'copy info'        
           CALL Read_Ncdf_var('info',TRIM(restart_file),tabtemp4D) 
           dimnames(1)='x_a'
           dimnames(2)='y_a'
           dimnames(3)='z_a'
           dimnames(4)=TRIM(timedimname)            
           CALL Write_Ncdf_var('info',dimnames,Child_file,tabtemp4D,'double')
           CALL Copy_Ncdf_att('info',TRIM(restart_file),Child_file) 
           DEALLOCATE(tabtemp4D)
           Interpolation = .FALSE. 
        ENDIF
        !
     CASE('nfice','nfbulk','kt','ndastp','adatrj','rdt','rdttra1') 
        IF(.NOT. dimg ) THEN
           WRITE(*,*) 'copy ',TRIM(Ncdf_varname(i))
           IF (iom_activated) THEN
              CALL Read_Ncdf_var(TRIM(Ncdf_varname(i)),TRIM(restart_file),tabtemp0dreal)  
              SELECT CASE (TRIM(Ncdf_varname(i)))
              CASE('rdt','rdttra1')
                 tabtemp0dreal = tabtemp0dreal/rhot
              CASE('kt')
                 tabtemp0dreal = tabtemp0dreal * rhot
              END SELECT
              CALL Write_Ncdf_var(TRIM(Ncdf_varname(i)),Child_file,tabtemp0dreal,'double')
           ELSE
              CALL Read_Ncdf_var(TRIM(Ncdf_varname(i)),TRIM(restart_file),tabtemp4D) 
              dimnames(1)='x_a'
              dimnames(2)='y_a'
              dimnames(3)='z_b'
              dimnames(4)=TRIM(timedimname)            
              CALL Write_Ncdf_var(TRIM(Ncdf_varname(i)),dimnames,Child_file,tabtemp4D,'double')
	      DEALLOCATE(tabtemp4D)
           ENDIF
           CALL Copy_Ncdf_att(TRIM(Ncdf_varname(i)),TRIM(restart_file),Child_file) 
           Interpolation = .FALSE.
        ENDIF
        !
        ! Variable interpolation according to their position on grid
        !                                   
     CASE('un','ub')  
        varname = Ncdf_varname(i)

        IF(TRIM(varname)=='un') irec = 6 * z + 2
        IF(TRIM(varname)=='ub') irec = 2

        WRITE(*,*) TRIM(varname),'interpolation ...'    
        vert_coord_name = 'z'             
        posvar='U'
        Interpolation = .TRUE.  
        !
     CASE('u_io')  
        varname = Ncdf_varname(i)  
        WRITE(*,*) TRIM(varname),'interpolation ...'    
        vert_coord_name = 'z_b'             
        posvar='U'
        Interpolation = .TRUE. 
        !                          
     CASE('vn','vb')
        varname = Ncdf_varname(i)

        IF(TRIM(varname)=='vn') irec = 7 * z + 2
        IF(TRIM(varname)=='vb') irec = 2 + z

        WRITE(*,*) TRIM(varname),'interpolation ...'      
        vert_coord_name = 'z'
        posvar='V'
        Interpolation = .TRUE.     
        !              
     CASE('v_io')
        varname = Ncdf_varname(i)
        WRITE(*,*) TRIM(varname),'interpolation ...'      
        vert_coord_name = 'z_b'
        posvar='V'
        Interpolation = .TRUE.
        !             
     CASE('gcx','gcxb','sshb','sshn','sst_io','sss_io','gsst')
        varname = Ncdf_varname(i)    

        IF(TRIM(varname)=='gcx') irec = 12 * z + 2
        IF(TRIM(varname)=='gcxb')  irec = 12 * z + 3
        IF(TRIM(varname)=='sshb') irec = 12 * z + 4
        IF(TRIM(varname)=='sshn') irec = 12 * z + 5

        WRITE(*,*) TRIM(varname),'interpolation ...' 
        vert_coord_name = 'z_b'             
        posvar='T'
        Interpolation = .TRUE.     

        !  
     CASE ('tb','sb','sn','tn')
        varname = Ncdf_varname(i) 

        IF(TRIM(varname)=='sn') irec = 9 * z + 2
        IF(TRIM(varname)=='tn') irec = 8 * z + 2
        IF(TRIM(varname)=='sb') irec = 3 * z + 2
        IF(TRIM(varname)=='tb') irec = 2 * z + 2

        WRITE(*,*) TRIM(varname),'interpolation ...'     
        vert_coord_name = 'z'
        posvar='T'
        Interpolation = .TRUE.            

     CASE('en')
        varname = Ncdf_varname(i) 
        irec = 12 * z + 6
        WRITE(*,*) TRIM(varname),'interpolation ...'     
        vert_coord_name = 'z'
        posvar='T'
        Interpolation = .TRUE.             

     CASE ('rotb','rotn','hdivb','hdivn')
        Interpolation = .FALSE.
        !
     END SELECT
     !      
     IF( Interpolation ) THEN
        !		  
        IF( vert_coord_name == 'z') THEN
           nbvert_lev = z 
        ELSE IF( vert_coord_name == 'z_b') THEN
           IF (iom_activated) THEN
              nbvert_lev=1
           ELSE
              nbvert_lev = z_b
           ENDIF
        END IF
        !                  

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
           !                                                       
           !                            If(n==1) then
           !                                      Call Read_Ncdf_var(varname,TRIM(restart_file),tabvar1,t,n)    
           !                            else if (n==2) then
           !                                      Call Read_Ncdf_var(varname,TRIM(restart_file),tabvar2,t,n-1)
           !                                      Call Read_Ncdf_var(varname,TRIM(restart_file),tabvar1,t,n)            
           !                            else 
           !                                      Call Read_Ncdf_var(varname,TRIM(restart_file),tabvar3,t,n-2)
           !                                      Call Read_Ncdf_var(varname,TRIM(restart_file),tabvar2,t,n-1)
           !                                      Call Read_Ncdf_var(varname,TRIM(restart_file),tabvar1,t,n)
           !                            endif                                                        	                  
           !
           CALL Read_Ncdf_var(varname,TRIM(restart_file),tabvar1,t,n)
           IF(n==1) THEN
              !                            
           ELSE IF (n==2) THEN
              tabvar2(:,:,:,1) = tabvar1(:,:,:,2)
           ELSE
              tabvar3(:,:,:,1) = tabvar2(:,:,:,1)
              tabvar2(:,:,:,1) = tabvar1(:,:,:,2)

           ENDIF
           !                            
           SELECT CASE(posvar)
              ! 
           CASE('T')
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
              !
           CASE('U')
              !
              IF(MAXVAL(G1%umask(:,:,n)) == 0) THEN
                 tabinterp4d = 0.0
                 WRITE(*,*) 'only land points on level ',n  
              ELSE		
                 !
                 CALL extrap_detect(G0,G1,detected_pts(:,:,n),n,'U')  
                 CALL correct_field(detected_pts(:,:,n),tabvar1,tabvar2,&
                      tabvar3,G0,nav_lev,masksrc,n,'U')
                 !                                                           
                 SELECT CASE(TRIM(interp_type))
                 CASE('bilinear')                                                       
                    CALL get_remap_matrix(G0%gphiu,G1%gphiu,   &
                         G0%glamu,G1%glamu,masksrc,matrix,src_add,dst_add)
                    CALL make_remap(tabvar1(:,:,1,1),tabinterp4d(:,:,1,1),nxfin,nyfin, &
                         matrix,src_add,dst_add)     
                 CASE('bicubic')                                   
                    CALL get_remap_bicub(G0%gphiu,G1%gphiu,   &
                         G0%glamu,G1%glamu,masksrc,matrix,src_add,dst_add)
                    CALL make_bicubic_remap(tabvar1(:,:,1,1),masksrc,tabinterp4d(:,:,1,1),&
                         nxfin,nyfin,matrix,src_add,dst_add)                        
                 END SELECT
                 !                      
                 CALL Correctforconservation(tabvar1(:,:,1,1),tabinterp4d(:,:,1,1), &
                      G0%e1u,G0%e2u,G1%e1u,G1%e2u,nxfin,nyfin,posvar,imin-jpizoom+1,jmin-jpjzoom+1)

              ENDIF

              tabinterp4d(:,:,1,1) =  tabinterp4d(:,:,1,1) * G1%umask(:,:,n)
              !
           CASE('V')
              !		
              IF(MAXVAL(G1%vmask(:,:,n)) == 0) THEN
                 tabinterp4d = 0.0
                 WRITE(*,*) 'only land points on level ',n 
              ELSE		
                 !

                 CALL extrap_detect(G0,G1,detected_pts(:,:,n),n,'V')

                 CALL correct_field(detected_pts(:,:,n),tabvar1,tabvar2,&
                      tabvar3,G0,nav_lev,masksrc,n,'V')
                 !                                                           
                 SELECT CASE(TRIM(interp_type))
                 CASE('bilinear')                                                       
                    CALL get_remap_matrix(G0%gphiv,G1%gphiv,   &
                         G0%glamv,G1%glamv,masksrc,matrix,src_add,dst_add)
                    CALL make_remap(tabvar1(:,:,1,1),tabinterp4d(:,:,1,1),nxfin,nyfin, &
                         matrix,src_add,dst_add)     
                 CASE('bicubic')                                   
                    CALL get_remap_bicub(G0%gphiv,G1%gphiv,   &
                         G0%glamv,G1%glamv,masksrc,matrix,src_add,dst_add)
                    CALL make_bicubic_remap(tabvar1(:,:,1,1),masksrc,tabinterp4d(:,:,1,1),&
                         nxfin,nyfin,matrix,src_add,dst_add)                        
                 END SELECT
                 !                      
                 CALL Correctforconservation(tabvar1(:,:,1,1),tabinterp4d(:,:,1,1), &
                      G0%e1v,G0%e2v,G1%e1v,G1%e2v,nxfin,nyfin,posvar,imin-jpizoom+1,jmin-jpjzoom+1)

              ENDIF

              tabinterp4d(:,:,1,1) =  tabinterp4d(:,:,1,1) * G1%vmask(:,:,n)
              !
           END SELECT
           !
           IF(dimg) THEN
              !
              WRITE(inum,REC=irec) tabinterp4d(:,:,1,1)
              irec = irec + 1
              !
           ELSE
              !
              dimnames(1)='x'
              dimnames(2)='y'
              IF ((iom_activated).AND.(vert_coord_name == 'z_b')) THEN
                 dimnames(3)=TRIM(timedimname)
              ELSE
                 dimnames(3)=vert_coord_name
                 dimnames(4)=TRIM(timedimname)
              ENDIF
              !             
              IF(TRIM(varname)=='gcx' .OR. TRIM(varname)=='gcxb') THEN 
                 PRINT*,TRIM(varname),MAXVAL(tabinterp4d),MINVAL(tabinterp4d)
              ENDIF
              IF ((iom_activated).AND.(vert_coord_name == 'z_b')) THEN
                 ALLOCATE(tabvar3d(SIZE(tabinterp4d,1),SIZE(tabinterp4d,2),SIZE(tabinterp4d,3)))
                 tabvar3d=tabinterp4d(:,:,:,1)
                 CALL Write_Ncdf_var(TRIM(varname),dimnames, &
                      Child_file,tabvar3d,t,'double')
                 DEALLOCATE(tabvar3d)
              ELSE
                 CALL Write_Ncdf_var(TRIM(varname),dimnames, &
                      Child_file,tabinterp4d,t,n,'double')
              ENDIF
              !
              CALL Copy_Ncdf_att(TRIM(varname),TRIM(restart_file),Child_file)
              !
           ENDIF
           !
           IF(ASSOCIATED(matrix)) DEALLOCATE(matrix,src_add,dst_add)   
           !
        END DO
        ! 
        DEALLOCATE(detected_pts)
        DEALLOCATE(tabinterp4d)
        DEALLOCATE(tabvar1,tabvar2,tabvar3)
        DEALLOCATE(masksrc)  
        !                     
     ENDIF

  END DO
  !
  !
  !
  !
  !
  !
  !
  !
  !
  ALLOCATE(rotn(nxfin,nyfin,z,1),rotb(nxfin,nyfin,z,1))
  ALLOCATE(hdivn(nxfin,nyfin,z,1),hdivb(nxfin,nyfin,z,1))
  !                          
  !
  IF(dimg) THEN         
     ALLOCATE(un(nxfin,nyfin,z,1),ub(nxfin,nyfin,z,1))
     ALLOCATE(vn(nxfin,nyfin,z,1),vb(nxfin,nyfin,z,1))
     irec = 6 * z + 2
     CALL read_dimg_var(inum,irec,un,z)  
     irec = 2
     CALL read_dimg_var(inum,irec,ub,z)
     irec = 7 * z + 2
     CALL read_dimg_var(inum,irec,vn,z)
     irec = 2 + z
     CALL read_dimg_var(inum,irec,vb,z)                      
  ELSE
     CALL Read_Ncdf_var('un',Child_file,un)      
     CALL Read_Ncdf_var('vn',Child_file,vn)
     !	 	 
     CALL Read_Ncdf_var('ub',Child_file,ub)
     CALL Read_Ncdf_var('vb',Child_file,vb)
  ENDIF
  !

  IF(rhot == 1) THEN
     WRITE(*,*) ''	 
     WRITE(*,*) 'no time interpolation (time refinement ratio = 1)'
  ELSE   
     WRITE(*,*) ''
     WRITE(*,*) 'time interpolation'
     now_wght = (rhot-1.)/rhot
     before_wght = 1./rhot
     !    
     ub = now_wght*un + before_wght*ub
     vb = now_wght*vn + before_wght*vb
     !-----------------------	       	       
     IF(dimg) THEN
        ALLOCATE(tn(nxfin,nyfin,z,1),tb(nxfin,nyfin,z,1))
        irec = 8 * z + 2
        CALL read_dimg_var(inum,irec,tn,z)
        irec = 2 * z + 2
        CALL read_dimg_var(inum,irec,tb,z)               
     ELSE
        CALL Read_Ncdf_var('tb',Child_file,tb)
        CALL Read_Ncdf_var('tn',Child_file,tn)
     ENDIF
     !----------------------               
     tb = now_wght*tn + before_wght*tb

     IF(dimg) THEN
        irec = 2 * z + 2
        CALL write_dimg_var(inum,irec,tb,z)                
     ELSE
        dimnames(1)='x'
        dimnames(2)='y'
        dimnames(3)='z'
        dimnames(4)=TRIM(timedimname)
        CALL Write_Ncdf_var('tb',dimnames,Child_file,tb,'double')
     ENDIF
     !
     DEALLOCATE(tn,tb)
     !----------------------	       
     IF(dimg) THEN
        ALLOCATE(sn(nxfin,nyfin,z,1),sb(nxfin,nyfin,z,1))
        irec = 9 * z + 2
        CALL read_dimg_var(inum,irec,sn,z)
        irec = 3 * z + 2
        CALL read_dimg_var(inum,irec,sb,z)              
     ELSE	       
        CALL Read_Ncdf_var('sb',Child_file,sb)
        CALL Read_Ncdf_var('sn',Child_file,sn)            
     ENDIF
     !----------------------               
     sb = now_wght*sn + before_wght*sb

     IF(dimg) THEN
        irec = 3 * z + 2
        CALL write_dimg_var(inum,irec,sb,z)                
     ELSE               
        dimnames(1)='x'
        dimnames(2)='y'
        dimnames(3)='z'
        dimnames(4)=TRIM(timedimname)
        CALL Write_Ncdf_var('sb',dimnames,Child_file,sb,'double')
     ENDIF
     !----------------------                             
     DEALLOCATE(sn,sb)
     !	       
     IF(dimg) THEN
        ALLOCATE(gcx(nxfin,nyfin,1,1),gcxb(nxfin,nyfin,1,1))
        irec = 12 * z + 2
        CALL read_dimg_var(inum,irec,gcx,1)
        irec = 12 * z + 3
        CALL read_dimg_var(inum,irec,gcxb,1) 
     ELSE  
        CALL Read_Ncdf_var('gcx',Child_file,gcx)
        CALL Read_Ncdf_var('gcxb',Child_file,gcxb)
     ENDIF
     !----------------------               
     gcxb = now_wght*gcx + before_wght*gcxb

     IF(dimg) THEN
        irec = 12 * z + 3
        CALL write_dimg_var(inum,irec,gcxb,1)                
     ELSE               
        dimnames(1)='x'
        dimnames(2)='y'
        dimnames(3)='z_b'
        dimnames(4)=TRIM(timedimname)
        CALL Write_Ncdf_var('gcxb',dimnames,Child_file,gcxb,'double')
     ENDIF
     !-----------------------               
     PRINT*,' gcx = ',MAXVAL(gcx),MINVAL(gcx)
     PRINT*,' gcxb = ',MAXVAL(gcxb),MINVAL(gcxb)

     DEALLOCATE(gcx,gcxb)
     !
     IF(dimg) THEN
        ALLOCATE(sshn(nxfin,nyfin,1,1),sshb(nxfin,nyfin,1,1))
        irec = 12 * z + 5
        CALL read_dimg_var(inum,irec,sshn,1)
        irec = 12 * z + 4
        CALL read_dimg_var(inum,irec,sshb,1) 
     ELSE 	       
        CALL Read_Ncdf_var('sshb',Child_file,sshb)
        CALL Read_Ncdf_var('sshn',Child_file,sshn)
     ENDIF
     !----------------------               
     sshb = now_wght*sshn + before_wght*sshb

     IF(dimg) THEN
        irec = 12 * z + 4
        CALL write_dimg_var(inum,irec,sshb,1)                
     ELSE  	       
        dimnames(1)='x'
        dimnames(2)='y'
        dimnames(3)='z_b'
        dimnames(4)=TRIM(timedimname)
        CALL Write_Ncdf_var('sshb',dimnames,Child_file,sshb,'double')
     ENDIF
     !               
     DEALLOCATE(sshb,sshn) 

     !    
  ENDIF
  !
  WRITE(*,*) 'Compute hdivn,rotn with new interpolated fields ...'
  !
  !fmask:land/ocean mask at f-point (=0. or 1.)=shlat along lateral boundaries
  !
  ALLOCATE(fse3u(nxfin,nyfin,z),fse3v(nxfin,nyfin,z),fse3t(nxfin,nyfin,z)) 
  !      
  IF(partial_steps) THEN
     status = Read_Bathymeter(TRIM(Childbathymeter),G1)
     CALL get_scale_factors( G1,fse3t,fse3u,fse3v )
  ELSE       
     fse3t(:,:,:) = 1.0
     fse3u(:,:,:) = 1.0
     fse3v(:,:,:) = 1.0
  ENDIF
  !
  !
  DO k = 1,z  
     !	 
     DO j = 2,nyfin-1
        DO i = 2, nxfin-1
           !
           hdivn(i,j,k,1) = (G1%e2u(i,j)*fse3u(i,j,k)*un(i,j,k,1)-G1%e2u(i-1,j)*fse3u(i-1,j,k)*un(i-1,j,k,1)      &
                +    G1%e1v(i,j)*fse3v(i,j,k)*vn(i,j,k,1)-G1%e1v(i,j-1)*fse3v(i,j-1,k)*vn(i,j-1,k,1))   & 
                / ( G1%e1t(i,j) * G1%e2t(i,j) * fse3t(i,j,k) )
           !
           hdivb(i,j,k,1) = (G1%e2u(i,j)*fse3u(i,j,k)*ub(i,j,k,1)-G1%e2u(i-1,j)*fse3u(i-1,j,k)*ub(i-1,j,k,1)      &
                +    G1%e1v(i,j)*fse3v(i,j,k)*vb(i,j,k,1)-G1%e1v(i,j-1)*fse3v(i,j-1,k)*vb(i,j-1,k,1))   & 
                / ( G1%e1t(i,j) * G1%e2t(i,j) * fse3t(i,j,k) )
           !			 
        END DO
     END DO
     !
     !
     hdivn(1:2,:,k,1) = 0.
     hdivn(:,1:2,k,1) = 0.
     hdivn(nxfin-1:nxfin,:,k,1) = 0.
     hdivn(:,nyfin-1:nyfin,k,1) = 0.
     hdivb(1:2,:,k,1) = 0.
     hdivb(:,1:2,k,1) = 0.
     hdivb(nxfin-1:nxfin,:,k,1) = 0.
     hdivb(:,nyfin-1:nyfin,k,1) = 0.         
     !	 
     DO j = 1, nyfin-1
        DO i = 1, nxfin-1
           !
           rotn(i,j,k,1) = (G1%e2v(i+1,j)*vn(i+1,j,k,1)-G1%e2v(i,j)*vn(i,j,k,1)    &
                - G1%e1u(i,j+1)*un(i,j+1,k,1)+G1%e1u(i,j)*un(i,j,k,1)) &
                * G1%fmask(i,j,k) / ( G1%e1f(i,j) * G1%e2f(i,j))
           !
           rotb(i,j,k,1) = (G1%e2v(i+1,j)*vb(i+1,j,k,1)-G1%e2v(i,j)*vb(i,j,k,1)    &
                - G1%e1u(i,j+1)*ub(i,j+1,k,1)+G1%e1u(i,j)*ub(i,j,k,1)) &
                * G1%fmask(i,j,k) / ( G1%e1f(i,j) * G1%e2f(i,j))
           !			    
        END DO
     END DO
     !
  END DO
  !
  PRINT*,' hdivn = ',MAXVAL(hdivn(2:nxfin-1,2:nyfin-1,:,1)),MINVAL(hdivn(2:nxfin-1,2:nyfin-1,:,1))
  PRINT*,' hdivb = ',MAXVAL(hdivb(2:nxfin-1,2:nyfin-1,:,1)),MINVAL(hdivb(2:nxfin-1,2:nyfin-1,:,1))
  PRINT*,' rotn  = ',MAXVAL(rotn(2:nxfin-1,2:nyfin-1,:,1)),MINVAL(rotn(2:nxfin-1,2:nyfin-1,:,1))
  PRINT*,' rotb  = ',MAXVAL(rotb(2:nxfin-1,2:nyfin-1,:,1)),MINVAL(rotb(2:nxfin-1,2:nyfin-1,:,1))
  !
  IF(dimg) THEN
     !
     irec = 10 * z + 2
     DO k=1,z
        WRITE(inum,REC=irec) rotn(:,:,k,1)
        irec = irec+1
     ENDDO
     !          
     irec = 4 * z + 2
     DO k=1,z
        WRITE(inum,REC=irec) rotb(:,:,k,1)
        irec = irec+1
     ENDDO
     !          
     irec = 11 * z + 2
     DO k=1,z
        WRITE(inum,REC=irec) hdivn(:,:,k,1)
        irec = irec+1
     ENDDO
     !          
     irec = 5 * z + 2
     DO k=1,z
        WRITE(inum,REC=irec) hdivb(:,:,k,1)
        irec = irec+1
     ENDDO
     !          
     CLOSE(inum)
  ELSE
     dimnames(1)='x'
     dimnames(2)='y'
     dimnames(3)='z'
     dimnames(4)=TRIM(timedimname)
     CALL Write_Ncdf_var('rotn',dimnames,Child_file,rotn,'double')
     CALL Copy_Ncdf_att('rotn',TRIM(restart_file),Child_file)
     CALL Write_Ncdf_var('hdivn',dimnames,Child_file,hdivn,'double')
     CALL Copy_Ncdf_att('hdivn',TRIM(restart_file),Child_file)
     CALL Write_Ncdf_var('rotb',dimnames,Child_file,rotb,'double')
     CALL Copy_Ncdf_att('rotb',TRIM(restart_file),Child_file)
     CALL Write_Ncdf_var('hdivb',dimnames,Child_file,hdivb,'double')
     CALL Copy_Ncdf_att('hdivb',TRIM(restart_file),Child_file)
  ENDIF
  !
  WRITE(*,*) ' '
  WRITE(*,*) '******* restart file successfully created *******' 
  WRITE(*,*) ' '
  !
  STOP 
END PROGRAM create_rstrt


