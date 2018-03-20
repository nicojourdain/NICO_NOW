PROGRAM fbstat
   !!---------------------------------------------------------------------
   !!
   !!                     ** PROGRAM fbstat **
   !!
   !!  ** Purpose : Output feedback file summary info/statistics
   !!         into a number of .dat files for different areas
   !!
   !!  ** Method  : Use of utilities from obs_fbm.
   !!
   !!  ** Action  :
   !!
   !!   Usage:
   !!     fbstat.exe [-nmlev] <filenames>
   !!   Optional:
   !!     namelist = namfbstat.in
   !!
   !!   History :
   !!        ! 2010 (K. Mogensen) Initial version
   !!----------------------------------------------------------------------
   USE obs_fbm
   USE fbaccdata
   USE coords
   USE omonainfo
   USE fbstatncio
   USE proftools
   IMPLICIT NONE
   TYPE(obfbdata) :: fbdata
   CHARACTER(len=256) :: filename,outfilename
   INTEGER :: jfile,jbox,jlev,jfirst,jvar,jadd,ji,ja,jt,jboxl
#ifndef NOIARGCPROTO
   INTEGER,EXTERNAL :: iargc
#endif
   REAL,DIMENSION(:),ALLOCATABLE :: zlev
   INTEGER :: nmlev, nfiles
   LOGICAL :: lexists,lomona,ltext,lnetcdf,lzinp
   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: zdat3d
   REAL, ALLOCATABLE, DIMENSION(:,:) :: zdat2d
   INTEGER,DIMENSION(1) :: itime
   INTEGER :: inidate,icurdate,loopno,ityp,iloopno
   INTEGER :: nvar,nadd,noberr,nbgerr
   CHARACTER(len=4) :: expver
   CHARACTER(len=20) :: cltyp
   CHARACTER(len=128) :: cdfmthead,cdfmtbody
   LOGICAL :: lnear,linner,linnerp,linnerini,lpassive,lhistogram,lfound
   LOGICAL :: lxyplot,lrmmean
   INTEGER :: nqc,nqco
   REAL :: rlspc,rlmax
   CHARACTER(len=ilenname), DIMENSION(:), ALLOCATABLE :: cname,caddname,&
      & cobename,cbgename
   INTEGER, PARAMETER :: nmaxareas = 20
   CHARACTER(len=20), DIMENSION(nmaxareas) :: carea
   LOGICAL, DIMENSION(:), ALLOCATABLE :: lskipbox
   INTEGER, parameter :: maxtyp = 10
   CHARACTER(len=ilentyp), DIMENSION(maxtyp) :: ctyp
   INTEGER :: ntyp,nboxuserl,ipdcst
   REAL :: mindcst
   NAMELIST/namfbstat/ltext,lomona,lnetcdf,nmlev,inidate,icurdate,loopno,&
      &               expver,lnear,linner,lpassive,lhistogram,&
      &               zhistmax,zhistmin,zhiststep,zcheck,carea,nmlev,&
      &               nqc,nqco, &
      &               rlspc,rlmax,ntyp,ctyp,&
      &               lxyplot,zxymin,zxymax,zxystep,lzinp,lrmmean,mindcst

   ltext=.TRUE.
   lnetcdf=.TRUE.
   lomona=.FALSE.
   nmlev=31
   inidate=19010101
   icurdate=19010116
   loopno=0
   expver='test'
   lnear=.TRUE.
   linner=.FALSE.
   lpassive=.FALSE.
   lhistogram=.FALSE.
   zhistmin(:)=-10.0
   zhistmax(:)=10.0
   zhiststep(:)=0.1
   zcheck(:)=1000.0
   nqc=2
   nqco=2
   carea(:)='all'
   rlmax=5000.0
   rlspc=-0.1
   ntyp=1
   ctyp(:)='all'
   lxyplot=.FALSE.
   zxymin(:)=-5.0
   zxymax(:)=45.0
   zxystep(:)=0.5
   lzinp=.FALSE.
   lrmmean=.FALSE.
   mindcst=-1.0

   INQUIRE(file='namfbstat.in',exist=lexists)
   IF (lexists) THEN
      OPEN(10,file='namfbstat.in')
      READ(10,namfbstat)
      CLOSE(10)
      WRITE(*,namfbstat)
   ENDIF
   mindcst=mindcst*1000.0 !From km to m.
   IF (iargc()==0) THEN
      WRITE(*,*)'Usage:'
      WRITE(*,*)'fbstat [-nmlev] <filenames>'
      CALL abort
   ENDIF
   jfirst=1
   DO ji=1,2
      CALL getarg(jfirst,filename)
      IF (filename=='-42') THEN
         nmlev=42
         jfirst=jfirst+1
      ELSEIF(filename=='-31') THEN
         nmlev=31
         jfirst=jfirst+1
      ELSEIF(filename=='-1') THEN
         nmlev=1
         lnear=.TRUE.
         jfirst=jfirst+1
      ELSEIF(filename=='-q') THEN
         jfirst=jfirst+1
         CALL getarg(jfirst,filename)
         READ(filename,'(I4)')nqc
         IF ((nqc<0).OR.(nqc>4)) THEN
            WRITE(*,*)'Quality control option (-q) should be 1 to 4'
            CALL abort
         ENDIF
         jfirst=jfirst+1
      ENDIF
   END DO
   nfiles=iargc()

   CALL coord_user_init('o')

   ALLOCATE(lskipbox(nboxuser))
   lskipbox(:)=.FALSE.

   IF (carea(1)/='all') THEN
      IF (lomona) THEN
         WRITE(*,*)'For omona files carea(1) has to be all'
         CALL abort
      ENDIF
      lskipbox(:)=.TRUE.
      DO ji=1,nmaxareas
         IF (carea(ji)/='all') THEN
            lfound=.FALSE.
            DO jbox=1,nboxuser
               IF (TRIM(carea(ji))==TRIM(cl_boxes_user(jbox))) THEN
                  lskipbox(jbox)=.FALSE.
                  lfound=.TRUE.
               ENDIF
            ENDDO
            IF (.NOT.lfound) THEN
               WRITE(*,*)'Area ',TRIM(carea(ji)),' not found'
               CALL abort
            ENDIF
         ENDIF
      ENDDO
      nboxuserl=0
      DO ji=1,nboxuser
         WRITE(*,*)'Area ',TRIM(cl_boxes_user(ji)),' is set to ',lskipbox(ji)
         IF (.NOT.lskipbox(ji)) nboxuserl=nboxuserl+1
      ENDDO
      WRITE(*,*)'Total areas for statistics = ',nboxuserl
      IF (lomona.AND.(nboxuserl/=nboxuser)) THEN
         WRITE(*,*)'Omona files only possible if all areas'
         CALL abort
      ENDIF
   ELSE
      nboxuserl=nboxuser
   ENDIF

   IF (rlspc>0.0) THEN
      lnear=.TRUE.
      nmlev=rlmax/rlspc+1
      ALLOCATE(zlev(nmlev))
      DO ji=1,nmlev
         zlev(ji)=(ji-1)*rlspc
      ENDDO
   ELSE
      IF (.NOT.lnear) nmlev=nmlev-1
      ALLOCATE(&
         & zlev(nmlev) &
         & )
      IF(lnear) THEN
         CALL getlevs(nmlev,zlev)
      ELSE
         CALL getlevsmean(nmlev,zlev)
      ENDIF
   ENDIF

   DO jfile=jfirst, nfiles
      CALL getarg(jfile,filename)
      WRITE(*,*)'Handling file : ',TRIM(filename)
      CALL flush(6)
      IF (lzinp) THEN
#if defined NOSYSTEM
         WRITE(*,*)'Compressed files need the system subroutine call'
         CALL abort
#else
         CALL system('cp '//TRIM(filename)//' fbstat_tmp.nc.gz')
         CALL system('gzip -df fbstat_tmp.nc.gz')
         CALL read_obfbdata('fbstat_tmp.nc',fbdata)
         CALL system('rm -f fbstat_tmp.nc')
#endif
      ELSE
         CALL read_obfbdata(TRIM(filename),fbdata)
      ENDIF
      CALL sealsfromargo( fbdata )
      IF (jfile==jfirst) THEN
         nvar=fbdata%nvar
         nadd=0
         DO ja= 1, fbdata%nadd
            IF (fbdata%caddname(ja)(1:2)=='Hx') nadd=nadd+1
         ENDDO
         noberr=0
         DO ja= 1, fbdata%nadd
            IF (fbdata%caddname(ja)(1:3)=='OBE') noberr=noberr+1
         ENDDO
         nbgerr=0
         DO ja= 1, fbdata%nadd
            IF (fbdata%caddname(ja)(1:3)=='BGE') nbgerr=nbgerr+1
         ENDDO
         IF (lhistogram) THEN
            IF (nvar>maxvars) THEN
               WRITE(*,*)'fbstat.F90: Increase maxvars to ',nvar
               WRITE(*,*)'if you want histograms'
               CALL abort
            ENDIF
            DO jvar = 1, nvar
               hist(jvar)%npoints=(zhistmax(jvar)-zhistmin(jvar))&
                  &              /zhiststep(jvar)+1
               WRITE(*,*)'Number of points in histogram      = ',&
                  &  hist(jvar)%npoints
               WRITE(*,*)'Size of histogram array (elements) = ',&
                  &  hist(jvar)%npoints*nmlev*nboxuserl*nadd
               ALLOCATE(&
                  & hist(jvar)%nhist(hist(jvar)%npoints,nmlev,nboxuserl,nadd,ntyp) &
                  & )
               hist(jvar)%nhist(:,:,:,:,:)=0
            ENDDO
         ENDIF
         ipdcst=0
         IF (mindcst>0) THEN
            DO ja= 1, fbdata%next
               IF (TRIM(fbdata%cextname(ja))=='DCST') THEN
                  ipdcst=ja
                  EXIT 
               ENDIF
            ENDDO
            IF (ipdcst==0) THEN
               WRITE(*,*)'Distance to coast not found in file, but mindcst>0'
               WRITE(*,*)'Extra variables:'
               DO ja= 1, fbdata%next
                  WRITE(*,*)ja,TRIM(fbdata%cextname(ja))
               ENDDO
               CALL abort
            ENDIF
         ENDIF
         IF (lxyplot) THEN
            IF (nvar>maxvars) THEN
               WRITE(*,*)'fbstat.F90: Increase maxvars to ',nvar
               WRITE(*,*)'if you want xyplots'
               CALL abort
            ENDIF
            DO jvar = 1, nvar
               xy(jvar)%npoints=(zxymax(jvar)-zxymin(jvar))&
                  &              /zxystep(jvar)+1
               WRITE(*,*)'Number of points in x and y for xyplots = ',&
                  &  xy(jvar)%npoints
               WRITE(*,*)'Size of xyplot array (elements)         = ',&
                  &  xy(jvar)%npoints*xy(jvar)%npoints*nmlev*nboxuserl*nadd
               ALLOCATE(&
                  & xy(jvar)%nxy(xy(jvar)%npoints,xy(jvar)%npoints,&
                  &              nmlev,nboxuserl,nadd,ntyp) &
                  & )
               xy(jvar)%nxy(:,:,:,:,:,:)=0
            ENDDO
         ENDIF
         ALLOCATE(&
            & inum(nmlev,nboxuserl,nadd,nvar,ntyp),  &
            & inumov(nmlev,nboxuserl,noberr,nvar,ntyp),  &
            & inumbv(nmlev,nboxuserl,nbgerr,nvar,ntyp),  &
            & inuma(nmlev,nboxuserl,nvar,ntyp),      &
            & zbias(nmlev,nboxuserl,nadd,nvar,ntyp), &
            & zrms(nmlev,nboxuserl,nadd,nvar,ntyp),  &
            & zsdev(nmlev,nboxuserl,nadd,nvar,ntyp), &
            & zomean(nmlev,nboxuserl,nadd,nvar,ntyp),&
            & zmmean(nmlev,nboxuserl,nadd,nvar,ntyp),&
            & zoemea(nmlev,nboxuserl,noberr,nvar,ntyp),&
            & zovmea(nmlev,nboxuserl,noberr,nvar,ntyp),&
            & zbemea(nmlev,nboxuserl,nbgerr,nvar,ntyp),&
            & zbvmea(nmlev,nboxuserl,nbgerr,nvar,ntyp),&
            & zoamean(nmlev,nboxuserl,nvar,ntyp),    &
            & cname(nvar),                           &
            & caddname(nadd),                        &
            & cobename(noberr),                      &
            & cbgename(nbgerr)                       &
            & )
         DO jvar = 1, nvar
            cname(jvar) = fbdata%cname(jvar)
         END DO
         jadd = 0
         DO ja= 1, fbdata%nadd
            IF (fbdata%caddname(ja)(1:2)=='Hx') THEN
               jadd=jadd+1
               caddname(jadd) = fbdata%caddname(ja)
            ENDIF
         END DO
         jadd = 0
         DO ja= 1, fbdata%nadd
            IF (fbdata%caddname(ja)(1:3)=='OBE') THEN
               jadd=jadd+1
               cobename(jadd) = fbdata%caddname(ja)
            ENDIF
         END DO
         jadd = 0
         DO ja= 1, fbdata%nadd
            IF (fbdata%caddname(ja)(1:3)=='BGE') THEN
               jadd=jadd+1
               cbgename(jadd) = fbdata%caddname(ja)
            ENDIF
         END DO
         IF (nadd>0) THEN
            inum(:,:,:,:,:)=0
            zbias(:,:,:,:,:)=0.0
            zrms(:,:,:,:,:)=0.0
            zsdev(:,:,:,:,:)=0.0
            zomean(:,:,:,:,:)=0.0 
            zmmean(:,:,:,:,:)=0.0
         ENDIF
         IF (noberr>0) THEN
            inumov(:,:,:,:,:)=0
            zoemea(:,:,:,:,:)=0
            zovmea(:,:,:,:,:)=0
         ENDIF
         IF (nbgerr>0) THEN
            inumbv(:,:,:,:,:)=0
            zbemea(:,:,:,:,:)=0
            zbvmea(:,:,:,:,:)=0
         ENDIF
         inuma(:,:,:,:)=0
         zoamean(:,:,:,:)=0.0
      ELSE
         IF (fbdata%nvar/=nvar) THEN
            WRITE(*,*)'Different number of nvar ',fbdata%nvar,' in ',&
               & TRIM(filename)
            CALL abort
         ENDIF
         jadd = 0
         DO ja= 1, fbdata%nadd
            IF (fbdata%caddname(ja)(1:2)=='Hx') THEN
               jadd=jadd+1
            ENDIF
         END DO
         IF (jadd/=nadd) THEN
            WRITE(*,*)'Different number of nadd ',jadd,' in ',&
               & TRIM(filename)
            CALL abort
         ENDIF
         jadd = 0
         DO ja= 1, fbdata%nadd
            IF (fbdata%caddname(ja)(1:3)=='OBE') THEN
               jadd=jadd+1
            ENDIF
         END DO
         IF (jadd/=noberr) THEN
            WRITE(*,*)'Different number of noberr ',jadd,' in ',&
               & TRIM(filename)
            CALL abort
         ENDIF
         jadd = 0
         DO ja= 1, fbdata%nadd
            IF (fbdata%caddname(ja)(1:3)=='BGE') THEN
               jadd=jadd+1
            ENDIF
         END DO
         IF (jadd/=nbgerr) THEN
            WRITE(*,*)'Different number of nbgerr ',jadd,' in ',&
               & TRIM(filename)
            CALL abort
         ENDIF
         IF (ipdcst>0) THEN
            IF (ipdcst>fbdata%next) THEN
               WRITE(*,*)'Distrance to coast in file not compatible with first file'
               CALL abort
            ENDIF
            IF (TRIM(fbdata%cextname(ipdcst))/='DCST') THEN
               WRITE(*,*)'Distrance to coast in file not compatible with first file'
               CALL abort
            ENDIF
         ENDIF
      ENDIF
      IF (lrmmean) THEN
         CALL fb_rmmean(fbdata)
      ENDIF
      CALL fb_stat(fbdata,lskipbox,nmlev,zlev,lnear,nqc,nqco,&
         &         lhistogram,lxyplot,ntyp,ctyp,ipdcst,mindcst)
      CALL dealloc_obfbdata(fbdata)
   ENDDO

   DO jt=1,ntyp
      DO jvar=1,nvar
         DO jadd=1,nadd
            jboxl=0
            DO jbox=1,nboxuser
               IF (lskipbox(jbox)) CYCLE
               jboxl=jboxl+1
               DO jlev = 1, nmlev
                  IF ( inum(jlev,jboxl,jadd,jvar,jt) > 0 ) THEN
                     zbias(jlev,jboxl,jadd,jvar,jt) = &
                        & zbias(jlev,jboxl,jadd,jvar,jt)/inum(jlev,jboxl,jadd,jvar,jt)
                     zrms(jlev,jboxl,jadd,jvar,jt) = &
                        & SQRT(zrms(jlev,jboxl,jadd,jvar,jt)/inum(jlev,jboxl,jadd,jvar,jt))
                     zsdev(jlev,jboxl,jadd,jvar,jt) = &
                        & SQRT(MAX(zrms(jlev,jboxl,jadd,jvar,jt)**2-zbias(jlev,jboxl,jadd,jvar,jt)**2,0.0))
                     zomean(jlev,jboxl,jadd,jvar,jt) = &
                        & zomean(jlev,jboxl,jadd,jvar,jt)/inum(jlev,jboxl,jadd,jvar,jt)
                     zmmean(jlev,jboxl,jadd,jvar,jt) = &
                        & zmmean(jlev,jboxl,jadd,jvar,jt)/inum(jlev,jboxl,jadd,jvar,jt)
                  ELSE
                     zbias(jlev,jboxl,jadd,jvar,jt) = fbrmdi
                     zrms(jlev,jboxl,jadd,jvar,jt) = fbrmdi
                     zsdev(jlev,jboxl,jadd,jvar,jt) = fbrmdi
                     zomean(jlev,jboxl,jadd,jvar,jt) = fbrmdi
                     zmmean(jlev,jboxl,jadd,jvar,jt) = fbrmdi
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
         DO jadd=1,noberr
            jboxl=0
            DO jbox=1,nboxuser
               IF (lskipbox(jbox)) CYCLE
               jboxl=jboxl+1
               DO jlev = 1, nmlev
                  IF ( inumov(jlev,jboxl,jadd,jvar,jt) > 0 ) THEN
                     zoemea(jlev,jboxl,jadd,jvar,jt) = &
                        & zoemea(jlev,jboxl,jadd,jvar,jt)/inumov(jlev,jboxl,jadd,jvar,jt)
                     zovmea(jlev,jboxl,jadd,jvar,jt) = &
                        & zovmea(jlev,jboxl,jadd,jvar,jt)/inumov(jlev,jboxl,jadd,jvar,jt)
                  ELSE
                     zoemea(jlev,jboxl,jadd,jvar,jt) = fbrmdi
                     zovmea(jlev,jboxl,jadd,jvar,jt) = fbrmdi
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
         DO jadd=1,nbgerr
            jboxl=0
            DO jbox=1,nboxuser
               IF (lskipbox(jbox)) CYCLE
               jboxl=jboxl+1
               DO jlev = 1, nmlev
                  IF ( inumbv(jlev,jboxl,jadd,jvar,jt) > 0 ) THEN
                     zbemea(jlev,jboxl,jadd,jvar,jt) = &
                        & zbemea(jlev,jboxl,jadd,jvar,jt)/inumbv(jlev,jboxl,jadd,jvar,jt)
                     zbvmea(jlev,jboxl,jadd,jvar,jt) = &
                        & zbvmea(jlev,jboxl,jadd,jvar,jt)/inumbv(jlev,jboxl,jadd,jvar,jt)
                  ELSE
                     zbemea(jlev,jboxl,jadd,jvar,jt) = fbrmdi
                     zbvmea(jlev,jboxl,jadd,jvar,jt) = fbrmdi
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
      ENDDO
   ENDDO
   DO jt=1,ntyp
      DO jvar=1,nvar
         jboxl=0
         DO jbox=1,nboxuser
            IF (lskipbox(jbox)) CYCLE
            jboxl=jboxl+1
            DO jlev = 1, nmlev
               IF ( inuma(jlev,jboxl,jvar,jt) > 0 ) THEN
                  zoamean(jlev,jboxl,jvar,jt) = &
                     & zoamean(jlev,jboxl,jvar,jt)/inuma(jlev,jboxl,jvar,jt)
               ELSE
                  zoamean(jlev,jboxl,jvar,jt) = fbrmdi
               ENDIF
            ENDDO
         ENDDO
      ENDDO
   ENDDO

   IF (ltext) THEN

      DO jt=1,ntyp
         DO jvar=1,nvar
            DO jadd=1,nadd
               jboxl=0
               DO jbox=1,nboxuser
                  IF (lskipbox(jbox)) CYCLE
                  jboxl=jboxl+1
                  WRITE(filename,'(7A)')TRIM(cname(jvar)),&
                     &                  TRIM(caddname(jadd)),'_',&
                     &                  TRIM(cl_boxes_user(jbox)),'_',&
                     &                  TRIM(ADJUSTL(ctyp(jt))),'.dat'
                  OPEN(10,file=TRIM(filename))
                  DO jlev = 1, nmlev
                     WRITE(10,'(F16.7,2I12,5F17.10)') zlev(jlev), &
                        & jlev, inum(jlev,jboxl,jadd,jvar,jt), &
                        & zbias(jlev,jboxl,jadd,jvar,jt), &
                        & zrms(jlev,jboxl,jadd,jvar,jt), &
                        & zsdev(jlev,jboxl,jadd,jvar,jt), &
                        & zomean(jlev,jboxl,jadd,jvar,jt), &
                        & zmmean(jlev,jboxl,jadd,jvar,jt)
                  ENDDO
                  CLOSE(10)
               ENDDO
            ENDDO
            DO jadd=1,noberr
               jboxl=0
               DO jbox=1,nboxuser
                  IF (lskipbox(jbox)) CYCLE
                  jboxl=jboxl+1
                  WRITE(filename,'(7A)')TRIM(cname(jvar)),&
                     &                  TRIM(cobename(jadd)),'_',&
                     &                  TRIM(cl_boxes_user(jbox)),'_',&
                     &                  TRIM(ADJUSTL(ctyp(jt))),'.dat'
                  OPEN(10,file=TRIM(filename))
                  DO jlev = 1, nmlev
                     WRITE(10,'(F16.7,2I12,5F17.10)') zlev(jlev), &
                        & jlev, inumov(jlev,jboxl,jadd,jvar,jt), &
                        & zoemea(jlev,jboxl,jadd,jvar,jt), &
                        & zovmea(jlev,jboxl,jadd,jvar,jt)
                  ENDDO
                  CLOSE(10)
               ENDDO
            ENDDO
            DO jadd=1,nbgerr
               jboxl=0
               DO jbox=1,nboxuser
                  IF (lskipbox(jbox)) CYCLE
                  jboxl=jboxl+1
                  WRITE(filename,'(7A)')TRIM(cname(jvar)),&
                     &                  TRIM(cbgename(jadd)),'_',&
                     &                  TRIM(cl_boxes_user(jbox)),'_',&
                     &                  TRIM(ADJUSTL(ctyp(jt))),'.dat'
                  OPEN(10,file=TRIM(filename))
                  DO jlev = 1, nmlev
                     WRITE(10,'(F16.7,2I12,5F17.10)') zlev(jlev), &
                        & jlev, inumbv(jlev,jboxl,jadd,jvar,jt), &
                        & zbemea(jlev,jboxl,jadd,jvar,jt), &
                        & zbvmea(jlev,jboxl,jadd,jvar,jt)
                  ENDDO
                  CLOSE(10)
               ENDDO
            ENDDO
         ENDDO
      ENDDO

      DO jt=1,ntyp
         DO jvar=1,nvar
            jboxl=0
            DO jbox=1,nboxuser
               IF (lskipbox(jbox)) CYCLE
               jboxl=jboxl+1
               WRITE(filename,'(7A)')TRIM(cname(jvar)),'_',&
                  &                  TRIM(cl_boxes_user(jbox)),'_',&
                  &                  TRIM(ADJUSTL(ctyp(jt))),'.dat'
               OPEN(10,file=TRIM(filename))
               DO jlev = 1, nmlev
                  WRITE(10,'(F16.7,2I12,F17.10)') zlev(jlev), &
                     & jlev, inuma(jlev,jboxl,jvar,jt), &
                     & zoamean(jlev,jboxl,jvar,jt)
               ENDDO
               CLOSE(10)
            ENDDO
         ENDDO
      ENDDO

      IF (lhistogram) THEN
         DO jt=1,ntyp
            DO jvar=1,nvar
               DO jadd=1,nadd
                  jboxl=0
                  DO jbox=1,nboxuser
                     IF (lskipbox(jbox)) CYCLE
                     jboxl=jboxl+1
                     WRITE(filename,'(7A)')TRIM(cname(jvar)),&
                        &                  TRIM(caddname(jadd)),'_',&
                        &                  TRIM(cl_boxes_user(jbox)),'_',&
                        &                  TRIM(ADJUSTL(ctyp(jt))),&
                        &                  '_histogram.dat'
                     OPEN(10,file=TRIM(filename))
                     WRITE(10,'(A10,1000F10.2)')'#',(zlev(jlev),jlev=1,nmlev)
                     DO ji=1,hist(jvar)%npoints
                        WRITE(10,'(F10.2,1000I10)') &
                           & zhistmin(jvar)+(ji-1)*zhiststep(jvar), &
                           & (hist(jvar)%nhist(ji,jlev,jboxl,jadd,jt),jlev=1,nmlev)
                     ENDDO
                     CLOSE(10)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDIF
      
   ENDIF

   IF (lnetcdf) THEN

      IF (nadd>0) THEN
         DO jt=1,ntyp
            WRITE(outfilename,'(3A)')'fbstat_',TRIM(ADJUSTL(ctyp(jt))),'.nc'
            CALL fbstat_ncwrite(TRIM(outfilename),&
               & nvar,cname,nadd,caddname,noberr,cobename,nbgerr,cbgename,&
               & nboxuser,nboxuserl,20,cl_boxes_user,lskipbox,nmlev,zlev,&
               & inum(:,:,:,:,jt),zbias(:,:,:,:,jt),zrms(:,:,:,:,jt), &
               & zsdev(:,:,:,:,jt),zomean(:,:,:,:,jt),zmmean(:,:,:,:,jt),&
               & inuma(:,:,:,jt),zoamean(:,:,:,jt), &
               & inumov(:,:,:,:,jt),zoemea(:,:,:,:,jt),zovmea(:,:,:,:,jt), &
               & inumbv(:,:,:,:,jt),zbemea(:,:,:,:,jt),zbvmea(:,:,:,:,jt) )
            IF (lhistogram) THEN
               WRITE(outfilename,'(3A)')'fbstat_hist_',TRIM(ADJUSTL(ctyp(jt))),'.nc'
               CALL fbstat_ncwrite_hist(TRIM(outfilename),&
                  & nvar,cname,nadd,caddname,&
                  & nboxuser,20,cl_boxes_user,lskipbox,nmlev,zlev,&
                  & hist,zhistmin,zhiststep,jt)
            ENDIF
            IF (lxyplot) THEN
               WRITE(outfilename,'(3A)')'fbstat_xyplot_',TRIM(ADJUSTL(ctyp(jt))),'.nc'
               CALL fbstat_ncwrite_xy(TRIM(outfilename),&
                  & nvar,cname,nadd,caddname,&
                  & nboxuser,20,cl_boxes_user,lskipbox,nmlev,zlev,&
                  & xy,zxymin,zxystep,jt)
            ENDIF
         ENDDO
      ENDIF
   ENDIF

   IF (lomona) THEN

      IF (ntyp>1) THEN
         WRITE(*,*)'Omona file only supported for the first type which is : ',TRIM(ctyp(1))
      ENDIF
      IF (nmlev>1) THEN
         ALLOCATE(zdat3d(nmlev,nboxuser,1))
      ELSE
         ALLOCATE(zdat2d(nboxuser,1))
      ENDIF

      cl_expnam=expver
      WRITE(cl_date,'(I8.8)')inidate
      i_dp = nmlev
      itime=icurdate
      linnerp=.TRUE.
      iloopno = loopno
      linnerini = linner
      i_fill=0

      DO jt=1,ntyp
         DO jvar = 1, nvar
            linner = linnerini
            loopno = iloopno
            SELECT CASE (TRIM(cname(jvar)))
            CASE('POTM')
               cl_var = 'votemper'
            CASE('PSAL')
               cl_var='vosaline'
            CASE('SLA')
               cl_var='sossheig'
            CASE('SST')
               cl_var='sosstsst'
            CASE('UVEL')
               cl_var='vozocrtx'
            CASE('VVEL')
               cl_var='vomecrty'
            END SELECT
            DO jadd = 1, nadd
               linner = (caddname(jadd)(1:3)=='Hxa').OR.linner
               IF (lpassive) THEN
                  ityp=145
               ELSE
                  IF (linner) THEN
                     linnerp=.TRUE.
                     ityp=144
                     IF (jadd>1) loopno=loopno+1
                  ELSE
                     ityp=142
                     IF (.NOT.linnerp) THEN
                        IF (jadd>1) loopno=loopno+1
                     ENDIF
                  ENDIF
               ENDIF
               WRITE(cltyp,'(I3.3,A1,I2.2,A1,A)')ityp,'_',loopno,'_',&
                  & TRIM(ADJUSTL(ctyp(jt)))
               CALL obs_variable_att(cltyp)
               IF (nmlev>1) THEN
                  zdat3d(:,:,1) = zbias(:,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat3d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
                  CALL write_dep_netcdf(cl_filename_out,cl_boxes_user,zlev)
               ELSE
                  zdat2d(:,1) = zbias(1,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat2d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
               ENDIF
               IF (lpassive) THEN
                  ityp=245
               ELSE
                  IF (linner) THEN
                     ityp=244
                  ELSE
                     ityp=242
                  ENDIF
               ENDIF
               WRITE(cltyp,'(I3.3,A1,I2.2,A1,A)')ityp,'_',loopno,'_',&
                  & TRIM(ADJUSTL(ctyp(jt)))
               CALL obs_variable_att(cltyp)
               IF (nmlev>1) THEN
                  zdat3d(:,:,1) = zrms(:,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat3d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
                  CALL write_dep_netcdf(cl_filename_out,cl_boxes_user,zlev)
               ELSE
                  zdat2d(:,1) = zrms(1,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat2d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
               ENDIF
               IF (lpassive) THEN
                  ityp=345
               ELSE
                  IF (linner) THEN
                     ityp=344
                  ELSE
                     ityp=342
                  ENDIF
               ENDIF
               WRITE(cltyp,'(I3.3,A1,I2.2,A1,A)')ityp,'_',loopno,'_',&
               & TRIM(ADJUSTL(ctyp(jt)))
               CALL obs_variable_att(cltyp)
               IF (nmlev>1) THEN
                  zdat3d(:,:,1) = zsdev(:,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat3d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
                  CALL write_dep_netcdf(cl_filename_out,cl_boxes_user,zlev)
               ELSE
                  zdat2d(:,1) = zsdev(1,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat2d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
               ENDIF
               IF (lpassive) THEN
                  ityp=445
               ELSE
                  IF (linner) THEN
                     ityp=444
                  ELSE
                     ityp=442
                  ENDIF
               ENDIF
               WRITE(cltyp,'(I3.3,A1,I2.2,A1,A)')ityp,'_',loopno,'_',&
                  & TRIM(ADJUSTL(ctyp(jt)))
               CALL obs_variable_att(cltyp)
               IF (nmlev>1) THEN
                  zdat3d(:,:,1) = inum(:,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat3d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
                  CALL write_dep_netcdf(cl_filename_out,cl_boxes_user,zlev)
               ELSE
                  zdat2d(:,1) = inum(1,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat2d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
               ENDIF
               IF (lpassive) THEN
                  ityp=545
               ELSE
                  IF (linner) THEN
                     ityp=544
                  ELSE
                     ityp=542
                  ENDIF
               ENDIF
               WRITE(cltyp,'(I3.3,A1,I2.2,A1,A)')ityp,'_',loopno,'_',&
                  & TRIM(ADJUSTL(ctyp(jt)))
               CALL obs_variable_att(cltyp)
               IF (nmlev>1) THEN
                  zdat3d(:,:,1) = zomean(:,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat3d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
                  CALL write_dep_netcdf(cl_filename_out,cl_boxes_user,zlev)
               ELSE
                  zdat2d(:,1) = zomean(1,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat2d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
               ENDIF
               IF (lpassive) THEN
                  ityp=645
               ELSE
                  IF (linner) THEN
                     ityp=644
                  ELSE
                     ityp=642
                  ENDIF
               ENDIF
               WRITE(cltyp,'(I3.3,A1,I2.2,A1,A)')ityp,'_',loopno,'_',&
                  & TRIM(ADJUSTL(ctyp(jt)))
               CALL obs_variable_att(cltyp)
               IF (nmlev>1) THEN
                  zdat3d(:,:,1) = zmmean(:,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat3d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
                  CALL write_dep_netcdf(cl_filename_out,cl_boxes_user,zlev)
               ELSE
                  zdat2d(:,1) = zmmean(1,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat2d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
               ENDIF
               linner=.FALSE.
            ENDDO
            loopno = iloopno
            DO jadd = 1, noberr
               linner = .TRUE.
               ityp = 139
               IF (jadd>1) loopno=loopno+1
               WRITE(cltyp,'(I3.3,A1,I2.2,A1,A)')ityp,'_',loopno,'_',&
                  & TRIM(ADJUSTL(ctyp(jt)))
               CALL obs_variable_att(cltyp)
               IF (nmlev>1) THEN
                  zdat3d(:,:,1) = zoemea(:,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat3d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
                  CALL write_dep_netcdf(cl_filename_out,cl_boxes_user,zlev)
               ELSE
                  zdat2d(:,1) = zoemea(1,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat2d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
               ENDIF
               ityp = 239
               WRITE(cltyp,'(I3.3,A1,I2.2,A1,A)')ityp,'_',loopno,'_',&
                  & TRIM(ADJUSTL(ctyp(jt)))
               CALL obs_variable_att(cltyp)
               IF (nmlev>1) THEN
                  zdat3d(:,:,1) = zovmea(:,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat3d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
                  CALL write_dep_netcdf(cl_filename_out,cl_boxes_user,zlev)
               ELSE
                  zdat2d(:,1) = zovmea(1,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat2d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
               ENDIF
            ENDDO
            loopno = iloopno
            DO jadd = 1, nbgerr
               linner = .TRUE.
               ityp = 141
               IF (jadd>1) loopno=loopno+1
               WRITE(cltyp,'(I3.3,A1,I2.2,A1,A)')ityp,'_',loopno,'_',&
                  & TRIM(ADJUSTL(ctyp(jt)))
               CALL obs_variable_att(cltyp)
               IF (nmlev>1) THEN
                  zdat3d(:,:,1) = zbemea(:,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat3d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
                  CALL write_dep_netcdf(cl_filename_out,cl_boxes_user,zlev)
               ELSE
                  zdat2d(:,1) = zbemea(1,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat2d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
               ENDIF
               ityp = 241
               WRITE(cltyp,'(I3.3,A1,I2.2,A1,A)')ityp,'_',loopno,'_',&
                  & TRIM(ADJUSTL(ctyp(jt)))
               CALL obs_variable_att(cltyp)
               IF (nmlev>1) THEN
                  zdat3d(:,:,1) = zbvmea(:,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat3d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
                  CALL write_dep_netcdf(cl_filename_out,cl_boxes_user,zlev)
               ELSE
                  zdat2d(:,1) = zbvmea(1,:,jadd,jvar,jt)
                  i_fill=0
                  CALL write_omona_netcdf(cl_filename_out,zdat2d,itime, &
                     &                    cl_boxes_user,REAL(fbrmdi),i_fill)
               ENDIF
            ENDDO
            IF (lpassive) THEN
               ityp=745
            ELSE
               ityp=742
            ENDIF
            WRITE(cltyp,'(I3.3,A1,A)')ityp,'_',&
               & TRIM(ADJUSTL(ctyp(jt)))
            CALL obs_variable_att(cltyp)
            IF (nmlev>1) THEN
               zdat3d(:,:,1) = inuma(:,:,jvar,jt)
               i_fill=0
               CALL write_omona_netcdf(cl_filename_out,zdat3d,itime, &
                  &                    cl_boxes_user,REAL(fbrmdi),i_fill)
               CALL write_dep_netcdf(cl_filename_out,cl_boxes_user,zlev)
            ELSE
               zdat2d(:,1) = inuma(1,:,jvar,jt)
               i_fill=0
               CALL write_omona_netcdf(cl_filename_out,zdat2d,itime, &
                  &                    cl_boxes_user,REAL(fbrmdi),i_fill)
            ENDIF
            IF (lpassive) THEN
               ityp=845
            ELSE
               ityp=842
            ENDIF
            WRITE(cltyp,'(I3.3,A1,A)')ityp,'_',&
               & TRIM(ADJUSTL(ctyp(jt)))
            CALL obs_variable_att(cltyp)
            IF (nmlev>1) THEN
               zdat3d(:,:,1) = zoamean(:,:,jvar,jt)
               i_fill=0
               CALL write_omona_netcdf(cl_filename_out,zdat3d,itime, &
                  &                    cl_boxes_user,REAL(fbrmdi),i_fill)
               CALL write_dep_netcdf(cl_filename_out,cl_boxes_user,zlev)
            ELSE
               zdat2d(:,1) = zoamean(1,:,jvar,jt)
               i_fill=0
               CALL write_omona_netcdf(cl_filename_out,zdat2d,itime, &
                  &                    cl_boxes_user,REAL(fbrmdi),i_fill)
            ENDIF
         ENDDO
      ENDDO

      IF (nmlev>1) THEN
         DEALLOCATE(zdat3d)
      ELSE
         DEALLOCATE(zdat2d)
      ENDIF
      
   ENDIF

CONTAINS

   SUBROUTINE fb_stat(fbdata,lskipbox,nmlev,zlev,lnear,kqc,kqco,lhist,lxyplot,&
      &               ntyp,ctyp,ipdcst,mindcst)
      USE fbaccdata
      USE coords
      TYPE(obfbdata) :: fbdata
      LOGICAL, DIMENSION(nboxuser) :: lskipbox
      INTEGER :: nmlev
      REAL :: zlev(nmlev)
      LOGICAL :: lnear
      INTEGER :: kqc,kqco
      LOGICAL :: lhist,lxyplot
      INTEGER :: ntyp
      CHARACTER(len=ilentyp), DIMENSION(ntyp) :: ctyp
      INTEGER :: ipdcst
      REAL :: mindcst
      INTEGER, DIMENSION(nboxuser) :: jlboxnum
      INTEGER :: jlev, jobs, jvar, klev,jlev2,ih,ja,jadd,jbox,jt,ix,iy,jboxl
      REAL :: zarea(4),zlat,zlon,zdiff,zdiff2,zvar
      
      jboxl=0
      jlboxnum=-1
      DO jbox = 1, nboxuser
         IF (lskipbox(jbox)) CYCLE
         jboxl=jboxl+1
         jlboxnum(jbox)=jboxl
      ENDDO

      !$omp parallel do default(shared) private(jlev,jobs,jvar,klev,jlev2,ih,ja,jadd,jbox,jt,ix,iy,jboxl,zarea,zlat,zlon,zdiff,zdiff2)
      DO jbox = 1, nboxuser
         IF (lskipbox(jbox)) CYCLE
         jboxl=jlboxnum(jbox)
         CALL coord_area_user(cl_boxes_user(jbox),zarea)
         DO jobs = 1, fbdata%nobs
            ! Reject observations with observation, position or time flag rejections
            IF (fbdata%ioqc(jobs)>kqco) CYCLE
            IF (fbdata%ipqc(jobs)>kqco) CYCLE
            IF (fbdata%itqc(jobs)>kqco) CYCLE
            zlat = fbdata%pphi(jobs)
            zlon = fbdata%plam(jobs)
            IF (zlon<0) zlon=zlon+360
            IF (zlon>360) zlon=zlon-360
            IF ( ( zlat .GE. zarea(3) ) .AND. &
               & ( zlat .LE. zarea(4) ) .AND. &
               & ( ( ( zlon .GE. zarea(1) ) .AND. &
               &     ( zlon .LE. zarea(2) ) ) .OR. &
               &   ( ( zarea(2) .LE. zarea(1) ) .AND. &
               &     ( zlon .GE. zarea(1) ) .AND. &
               &     ( zlon .LE. 360        ) ) .OR. &
               &   ( ( zarea(2)   .LE. zarea(1) ) .AND. &
               &     ( zlon .GE. 0          ) .AND. &
               &     ( zlon .LE. zarea(2) ) ) ) ) THEN
            
               DO jlev = 1, fbdata%nlev
                  IF (ipdcst>0) THEN
                     IF (fbdata%pext(jlev,jobs,ipdcst)==fbrmdi) CYCLE
                     IF (fbdata%pext(jlev,jobs,ipdcst)<mindcst) CYCLE
                  ENDIF
                  DO jvar = 1, fbdata%nvar
                     IF (nmlev==1) THEN
                        klev=1
                     ELSE
                        IF (lnear) THEN
                           zdiff=ABS(fbdata%pdep(jlev,jobs)-zlev(1))
                           klev=1
                           DO jlev2=2,nmlev
                              zdiff2=ABS(fbdata%pdep(jlev,jobs)-zlev(jlev2))
                              IF (zdiff2<zdiff) THEN
                                 klev=jlev2
                                 zdiff=zdiff2
                              ENDIF
                           ENDDO
                        ELSE
                           klev = fbdata%iobsk(jlev,jobs,jvar)-1
                        ENDIF
                        IF ( klev > nmlev ) THEN
                           DO ja = 1, fbdata%nadd
                              IF ( fbdata%caddname(ja)(1:2) /= 'Hx' ) CYCLE
                              IF ( ABS(fbdata%padd(jlev,jobs,ja,jvar))<9000 ) THEN
                                 WRITE(*,*)'Error in fb_stat'
                                 WRITE(*,*)'Increase nmlev to at least ',klev
                                 klev=nmlev
                                 CALL abort
                              ENDIF
                           ENDDO
                        ENDIF
                     ENDIF
                     IF (( klev > 0 ).AND. &
                        &(ABS(fbdata%pob(jlev,jobs,jvar)) < 9000 )) THEN
                        DO jt=1,ntyp
                           IF (TRIM(ADJUSTL(ctyp(jt)))/='all') THEN
                              IF (TRIM(ADJUSTL(ctyp(jt)))/=TRIM(ADJUSTL(fbdata%cdtyp(jobs)))) CYCLE
                           ENDIF
                           inuma(klev,jboxl,jvar,jt) = inuma(klev,jboxl,jvar,jt) + 1
                           zoamean(klev,jboxl,jvar,jt) = zoamean(klev,jboxl,jvar,jt) + &
                              & fbdata%pob(jlev,jobs,jvar)
                        ENDDO
                     ENDIF
                     IF ( fbdata%ivlqc(jlev,jobs,jvar) < 0 ) CYCLE 
                     IF ( fbdata%ivlqc(jlev,jobs,jvar) > kqc ) CYCLE 
                     IF (( klev > 0 ).AND. &
                        &(ABS(fbdata%pob(jlev,jobs,jvar)) < 9000 )) THEN
                        jadd = 0
                        DO ja = 1, fbdata%nadd
                           IF ( fbdata%caddname(ja)(1:2) /= 'Hx' ) CYCLE
                           jadd = jadd + 1
                           IF ( ABS(fbdata%padd(jlev,jobs,ja,jvar)) < 9000 ) THEN
                              zdiff = ( fbdata%padd(jlev,jobs,ja,jvar) - &
                                 &      fbdata%pob(jlev,jobs,jvar) )
                              DO jt=1,ntyp
                                 IF (TRIM(ADJUSTL(ctyp(jt)))/='all') THEN
                                    IF (TRIM(ADJUSTL(ctyp(jt)))/=TRIM(ADJUSTL(fbdata%cdtyp(jobs)))) CYCLE
                                 ENDIF
                                 inum(klev,jboxl,jadd,jvar,jt) = inum(klev,jboxl,jadd,jvar,jt) + 1
                                 zbias(klev,jboxl,jadd,jvar,jt) = zbias(klev,jboxl,jadd,jvar,jt) + &
                                    & zdiff
                                 zrms(klev,jboxl,jadd,jvar,jt) = zrms(klev,jboxl,jadd,jvar,jt) + &
                                    & zdiff * zdiff
                                 zomean(klev,jboxl,jadd,jvar,jt) = zomean(klev,jboxl,jadd,jvar,jt) + &
                                    & fbdata%pob(jlev,jobs,jvar)
                                 zmmean(klev,jboxl,jadd,jvar,jt) = zmmean(klev,jboxl,jadd,jvar,jt) + &
                                    & fbdata%padd(jlev,jobs,ja,jvar)
                              ENDDO
                              IF (ABS(zdiff)>zcheck(jvar)) THEN
                                 WRITE(*,*)'Departure outside check range ',&
                                    & TRIM(fbdata%cname(jvar)),' entry ',&
                                    & fbdata%caddname(jadd)
                                 WRITE(*,*)'Depar = ',zdiff
                                 WRITE(*,*)'Check = ',zcheck(jvar)
                                 WRITE(*,*)'Id    = ',fbdata%cdwmo(jobs)
                                 WRITE(*,*)'Lat   = ',fbdata%pphi(jobs)
                                 WRITE(*,*)'Lon   = ',fbdata%plam(jobs)
                                 WRITE(*,*)'Tim   = ',fbdata%ptim(jobs)
                                 WRITE(*,*)'Depth = ',fbdata%pdep(jlev,jobs)
                                 WRITE(*,*)'Obs   = ',fbdata%pob(jlev,jobs,jvar)
                                 WRITE(*,*)'Var   = ',fbdata%padd(jlev,jobs,ja,jvar)
                                 WRITE(*,*)'QC    = ',fbdata%ivlqc(jlev,jobs,jvar)
                                 WRITE(*,*)'QCflag= ',fbdata%ivlqcf(:,jlev,jobs,jvar)
                              ENDIF
                              IF (lhist) THEN
                                 ih=NINT((zdiff-zhistmin(jvar))/zhiststep(jvar))+1
                                 IF ((ih>=1).AND.(ih<=hist(jvar)%npoints)) THEN
                                    DO jt=1,ntyp
                                       IF (TRIM(ADJUSTL(ctyp(jt)))/='all') THEN
                                          IF (TRIM(ADJUSTL(ctyp(jt)))/=TRIM(ADJUSTL(fbdata%cdtyp(jobs)))) CYCLE
                                       ENDIF
                                       hist(jvar)%nhist(ih,klev,jboxl,jadd,jt) = &
                                          hist(jvar)%nhist(ih,klev,jboxl,jadd,jt) +1
                                    ENDDO
                                 ELSE
                                    WRITE(*,*)'Histogram value outside range for ',&
                                       & TRIM(fbdata%cname(jvar)),' entry ',&
                                       & fbdata%caddname(jadd)
                                    WRITE(*,*)'Value = ',zdiff
                                    WRITE(*,*)'Range = ',zhistmin(jvar),zhistmax(jvar)
                                    WRITE(*,*)'Step  = ',zhiststep(jvar)
                                    WRITE(*,*)'Index = ',ih
                                    WRITE(*,*)'Id    = ',TRIM(fbdata%cdwmo(jobs))
                                    WRITE(*,*)'Typ   = ',TRIM(fbdata%cdtyp(jobs))
                                    WRITE(*,*)'Lat   = ',fbdata%pphi(jobs)
                                    WRITE(*,*)'Lon   = ',fbdata%plam(jobs)
                                    WRITE(*,*)'Tim   = ',fbdata%ptim(jobs)
                                    WRITE(*,*)'Depth = ',fbdata%pdep(jlev,jobs)
                                    WRITE(*,*)'Obs   = ',fbdata%pob(jlev,jobs,jvar)
                                    WRITE(*,*)'Var   = ',fbdata%padd(jlev,jobs,jadd,jvar)
                                    WRITE(*,*)'QC    = ',fbdata%ivlqc(jlev,jobs,jvar)
                                    WRITE(*,*)'QCflag= ',fbdata%ivlqcf(:,jlev,jobs,jvar)
                                 ENDIF
                              ENDIF
                              IF (lxyplot) THEN
                                 ix=NINT((fbdata%pob(jlev,jobs,jvar)-zxymin(jvar))/&
                                    &    zxystep(jvar))+1
                                 iy=NINT((fbdata%padd(jlev,jobs,ja,jvar)-zxymin(jvar))/&
                                    &    zxystep(jvar))+1
                                 IF ((ix>=1).AND.(ix<=xy(jvar)%npoints).AND. &
                                    &(iy>=1).AND.(iy<=xy(jvar)%npoints)) THEN
                                    DO jt=1,ntyp
                                       IF (TRIM(ADJUSTL(ctyp(jt)))/='all') THEN
                                          IF (TRIM(ADJUSTL(ctyp(jt)))/=TRIM(ADJUSTL(fbdata%cdtyp(jobs)))) CYCLE
                                       ENDIF
                                       xy(jvar)%nxy(ix,iy,klev,jboxl,jadd,jt) = &
                                          xy(jvar)%nxy(ix,iy,klev,jboxl,jadd,jt) +1
                                    ENDDO
                                 ELSE
                                    WRITE(*,*)'xy plot values outside range for ',&
                                       & TRIM(fbdata%cname(jvar)),' entry ',&
                                       & fbdata%caddname(jadd)
                                    WRITE(*,*)'Obs   = ',fbdata%pob(jlev,jobs,jvar)
                                    WRITE(*,*)'Model = ',fbdata%padd(jlev,jobs,ja,jvar)
                                    WRITE(*,*)'Range = ',zxymin(jvar),zxymax(jvar)
                                    WRITE(*,*)'Step  = ',zxystep(jvar)
                                    WRITE(*,*)'Index = ',ih
                                    WRITE(*,*)'Id    = ',TRIM(fbdata%cdwmo(jobs))
                                    WRITE(*,*)'Typ   = ',TRIM(fbdata%cdtyp(jobs))
                                    WRITE(*,*)'Lat   = ',fbdata%pphi(jobs)
                                    WRITE(*,*)'Lon   = ',fbdata%plam(jobs)
                                    WRITE(*,*)'Tim   = ',fbdata%ptim(jobs)
                                    WRITE(*,*)'Depth = ',fbdata%pdep(jlev,jobs)
                                    WRITE(*,*)'Obs   = ',fbdata%pob(jlev,jobs,jvar)
                                    WRITE(*,*)'Var   = ',fbdata%padd(jlev,jobs,jadd,jvar)
                                    WRITE(*,*)'QC    = ',fbdata%ivlqc(jlev,jobs,jvar)
                                    WRITE(*,*)'QCflag= ',fbdata%ivlqcf(:,jlev,jobs,jvar)
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDDO
                        jadd = 0
                        DO ja = 1, fbdata%nadd
                           IF ( fbdata%caddname(ja)(1:3) /= 'OBE' ) CYCLE
                           jadd = jadd + 1
                           IF ( ABS(fbdata%padd(jlev,jobs,ja,jvar)) < 9000 ) THEN
                              zvar = fbdata%padd(jlev,jobs,ja,jvar)*fbdata%padd(jlev,jobs,ja,jvar)
                              DO jt=1,ntyp
                                 IF (TRIM(ADJUSTL(ctyp(jt)))/='all') THEN
                                    IF (TRIM(ADJUSTL(ctyp(jt)))/=TRIM(ADJUSTL(fbdata%cdtyp(jobs)))) CYCLE
                                 ENDIF
                                 inumov(klev,jboxl,jadd,jvar,jt) = inumov(klev,jboxl,jadd,jvar,jt) + 1
                                 zoemea(klev,jboxl,jadd,jvar,jt) = zoemea(klev,jboxl,jadd,jvar,jt) + &
                                    & fbdata%padd(jlev,jobs,ja,jvar)                                 
                                 zovmea(klev,jboxl,jadd,jvar,jt) = zovmea(klev,jboxl,jadd,jvar,jt) + zvar
                              ENDDO
                           ENDIF
                        ENDDO
                        jadd = 0
                        DO ja = 1, fbdata%nadd
                           IF ( fbdata%caddname(ja)(1:3) /= 'BGE' ) CYCLE
                           jadd = jadd + 1
                           IF ( ABS(fbdata%padd(jlev,jobs,ja,jvar)) < 9000 ) THEN
                              zvar = fbdata%padd(jlev,jobs,ja,jvar)*fbdata%padd(jlev,jobs,ja,jvar)
                              DO jt=1,ntyp
                                 IF (TRIM(ADJUSTL(ctyp(jt)))/='all') THEN
                                    IF (TRIM(ADJUSTL(ctyp(jt)))/=TRIM(ADJUSTL(fbdata%cdtyp(jobs)))) CYCLE
                                 ENDIF
                                 inumbv(klev,jboxl,jadd,jvar,jt) = inumbv(klev,jboxl,jadd,jvar,jt) + 1
                                 zbemea(klev,jboxl,jadd,jvar,jt) = zbemea(klev,jboxl,jadd,jvar,jt) + &
                                    & fbdata%padd(jlev,jobs,ja,jvar)
                                 zbvmea(klev,jboxl,jadd,jvar,jt) = zbvmea(klev,jboxl,jadd,jvar,jt) + zvar
                              ENDDO
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDDO
      ENDDO
      !$omp end parallel do
      
   END SUBROUTINE fb_stat

   SUBROUTINE fb_rmmean(fbdata)
      TYPE(obfbdata) :: fbdata
      INTEGER :: jadd,jmean

      jmean=0
      DO jadd=1,fbdata%nadd
         IF (fbdata%caddname(jadd)(1:4)=='MEAN') THEN
            jmean=jadd
            EXIT
         ENDIF
      ENDDO
      IF (jmean==0) THEN
         WRITE(*,*)'Warning: MEAN additional variable not found'
         RETURN
      ENDIF
      IF (fbdata%nobs>0) THEN
         DO jadd=1,fbdata%nadd 
            IF (fbdata%caddname(jadd)(1:2)=='Hx') THEN
               fbdata%padd(:,:,jadd,:)=fbdata%padd(:,:,jadd,:)&
                  &                   +fbdata%padd(:,:,jmean,:)
            ENDIF
         ENDDO
      ENDIF

   END SUBROUTINE fb_rmmean

   SUBROUTINE getlevsmean(nmlev,zlev)
      IMPLICIT NONE
      INTEGER :: nmlev
      REAL,DIMENSION(nmlev) :: zlev
      REAL,DIMENSION(nmlev+1) :: ztmp
      INTEGER :: i

      zlev(:)=9999.9
      CALL getlevs(nmlev+1,ztmp)
      DO i=1,nmlev
         zlev(i)=0.5*(ztmp(i)+ztmp(i+1))
      ENDDO

   END SUBROUTINE getlevsmean

   SUBROUTINE getlevs(nmlev,zlev)
      IMPLICIT NONE
      INTEGER :: nmlev
      REAL,DIMENSION(nmlev) :: zlev
      
      zlev(:)=9999.9

      IF (nmlev==42) THEN
         zlev(1)=5.02159
         zlev(2)=15.07854
         zlev(3)=25.16046
         zlev(4)=35.27829
         zlev(5)=45.44776
         zlev(6)=55.69149
         zlev(7)=66.04198
         zlev(8)=76.54591
         zlev(9)=87.27029
         zlev(10)=98.31118
         zlev(11)=109.8062
         zlev(12)=121.9519
         zlev(13)=135.0285
         zlev(14)=149.4337
         zlev(15)=165.7285
         zlev(16)=184.6975
         zlev(17)=207.4254
         zlev(18)=235.3862
         zlev(19)=270.5341
         zlev(20)=315.3741
         zlev(21)=372.9655
         zlev(22)=446.8009
         zlev(23)=540.5022
         zlev(24)=657.3229
         zlev(25)=799.5496
         zlev(26)=967.9958
         zlev(27)=1161.806
         zlev(28)=1378.661
         zlev(29)=1615.291
         zlev(30)=1868.071
         zlev(31)=2133.517
         zlev(32)=2408.583
         zlev(33)=2690.780
         zlev(34)=2978.166
         zlev(35)=3269.278
         zlev(36)=3563.041
         zlev(37)=3858.676
         zlev(38)=4155.628
         zlev(39)=4453.502
         zlev(40)=4752.021
         zlev(41)=5050.990
         zlev(42)=5350.272
      ELSEIF (nmlev==31) THEN
         zlev(1)=4.999938
         zlev(2)=15.00029
         zlev(3)=25.00176
         zlev(4)=35.00541
         zlev(5)=45.01332
         zlev(6)=55.0295
         zlev(7)=65.06181
         zlev(8)=75.12551
         zlev(9)=85.25037
         zlev(10)=95.49429
         zlev(11)=105.9699
         zlev(12)=116.8962
         zlev(13)=128.6979
         zlev(14)=142.1953
         zlev(15)=158.9606
         zlev(16)=181.9628
         zlev(17)=216.6479
         zlev(18)=272.4767
         zlev(19)=364.303
         zlev(20)=511.5348
         zlev(21)=732.2009
         zlev(22)=1033.217
         zlev(23)=1405.698
         zlev(24)=1830.885
         zlev(25)=2289.768
         zlev(26)=2768.242
         zlev(27)=3257.479
         zlev(28)=3752.442
         zlev(29)=4250.401
         zlev(30)=4749.913
         zlev(31)=5250.227
      ELSEIF (nmlev==1) THEN
         zlev(1)=0.0
      ELSE
         WRITE(*,*) 'Unknown number of levels'
         CALL abort
      ENDIF

   END SUBROUTINE getlevs

END PROGRAM fbstat
