#define MYFILE 'fbstatncio.F90'
MODULE fbstatncio

   USE fbacctype
   USE nctools
   IMPLICIT NONE

   REAL, PARAMETER :: fbstncmiss = 99999.

   TYPE fbstatnctype
      INTEGER :: nlev,nbox,nadd
      CHARACTER(len=20), POINTER, DIMENSION(:) :: area
      CHARACTER(len=32), POINTER, DIMENSION(:) :: name
      REAL, POINTER, DIMENSION(:) :: dep
      REAL, POINTER, DIMENSION(:,:,:) :: val
      INTEGER, POINTER, DIMENSION(:,:) :: cnt
   END TYPE fbstatnctype

   TYPE fbstathistnctype
      INTEGER :: nlev,nbox,npoints
      CHARACTER(len=20), POINTER, DIMENSION(:) :: area
      REAL, POINTER, DIMENSION(:) :: dep,val
      INTEGER, POINTER, DIMENSION(:,:,:) :: nhist
   END TYPE fbstathistnctype

   TYPE fbstatxynctype
      INTEGER :: nlev,nbox,npoints
      CHARACTER(len=20), POINTER, DIMENSION(:) :: area
      REAL, POINTER, DIMENSION(:) :: dep,val
      INTEGER, POINTER, DIMENSION(:,:,:,:) :: nxy
   END TYPE fbstatxynctype

CONTAINS

   SUBROUTINE fbstat_ncwrite(cdfilename,nvar,cdvar,nadd,cdadd,&
      & nobe,cdobe,nbge,cdbge,&
      & nbox,nboxl,lenboxname,cdboxnam,lskipbox,nlev,pdep,&
      & knum,pbias,prms,pstd,pomean,pmmean,knuma,poamean, &
      & knumo,poerr,povar,knumb,pberr,pbvar)
      ! Arguments
      CHARACTER(len=*) :: cdfilename                ! Netcdf filename
      INTEGER :: nvar                               ! Number of variables 
      CHARACTER(len=*), DIMENSION(nvar) :: cdvar    ! Name of variables 
      INTEGER :: nadd                               ! Number of additional data
      CHARACTER(len=*), DIMENSION(nadd) :: cdadd    ! Name of entries
      INTEGER :: nobe                               ! Number of obs errors
      CHARACTER(len=*), DIMENSION(nadd) :: cdobe    ! Name of obs erors
      INTEGER :: nbge                               ! Number of bg errors
      CHARACTER(len=*), DIMENSION(nadd) :: cdbge    ! Name of bg erors
      INTEGER :: nbox                               ! Total number of boxes
      INTEGER :: nboxl                              ! Actual number of boxes
      INTEGER :: lenboxname                         ! Length of box names
      CHARACTER(len=lenboxname), DIMENSION(nbox) :: &
         & cdboxnam                                 ! Name of boxes
      LOGICAL, DIMENSION(nbox) :: lskipbox          ! Boxes to skip
      INTEGER :: nlev                               ! Number of levels
      REAL,DIMENSION(nlev) :: pdep                  ! Depth of levels
      INTEGER, DIMENSION(nlev,nboxl,nadd,nvar) :: & ! Output data
         & knum
      REAL, DIMENSION(nlev,nboxl,nadd,nvar) :: &    ! Output data
         & pbias, prms, pstd, pomean, pmmean
      INTEGER, DIMENSION(nlev,nboxl,nvar) :: &      ! Output data
         & knuma
      REAL, DIMENSION(nlev,nboxl,nvar) :: &         ! Output data
         & poamean
      INTEGER, DIMENSION(nlev,nboxl,nobe,nvar) :: & ! Output data
         & knumo
      REAL, DIMENSION(nlev,nboxl,nobe,nvar) :: &    ! Output data
         & poerr,povar
      INTEGER, DIMENSION(nlev,nboxl,nbge,nvar) :: & ! Output data
         & knumb
      REAL, DIMENSION(nlev,nboxl,nbge,nvar) :: &    ! Output data
         & pberr,pbvar
      ! Local variables
      INTEGER :: jadd,jvar,incvar,iv,jbox,ip
      CHARACTER(len=50) :: cncvarbase
      CHARACTER(len=60), ALLOCATABLE, DIMENSION(:) :: cncvar
      ! netcdf stuff
      INTEGER :: ncid,idlev,idbox,idlbox,idimdep(1),idimbox(2),idimids(2)
      INTEGER :: idvbox,idvlev
      INTEGER, ALLOCATABLE, DIMENSION(:) :: idvar
      INTEGER :: inoboxes
      REAL, ALLOCATABLE, DIMENSION(:,:) :: ztmp
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: itmp
      CHARACTER(len=lenboxname), DIMENSION(:), ALLOCATABLE :: &
         & clboxnam                                 ! Name of boxes

      ! Open netCDF files.

      CALL nchdlerr(nf90_create(TRIM(cdfilename),nf90_clobber,ncid),&
         &          __LINE__,MYFILE)

      ! Create dimensions

      inoboxes=nbox-COUNT(lskipbox)
      ALLOCATE(ztmp(nlev,inoboxes),itmp(nlev,inoboxes),clboxnam(inoboxes))

      CALL nchdlerr(nf90_def_dim(ncid,"depth",nlev,idlev),__LINE__,MYFILE)

      CALL nchdlerr(nf90_def_dim(ncid,"box",inoboxes,idbox),&
         &          __LINE__,MYFILE)

      CALL nchdlerr(nf90_def_dim(ncid,"len",lenboxname,idlbox),__LINE__,MYFILE)

      ! Box variable name

      idimbox(1)=idlbox
      idimbox(2)=idbox
      CALL nchdlerr(nf90_def_var(ncid,'box',nf90_char,idimbox,idvbox),&
         &          __LINE__,MYFILE)

      ! Depths

      idimdep(1)=idlev
      CALL nchdlerr(nf90_def_var(ncid,'depth',nf90_float,idimdep,idvlev),&
         &          __LINE__,MYFILE)

      ! Setup variables names

      idimids(1)=idlev
      idimids(2)=idbox
      incvar=(nadd*6+nobe*3+nbge*3+2)*nvar
      ALLOCATE(cncvar(incvar),idvar(incvar))
      iv=0
      DO jvar=1,nvar
         DO jadd=1,nadd
            WRITE(cncvarbase,'(3A)')TRIM(cdvar(jvar)),'_',TRIM(cdadd(jadd))
            iv=iv+1
            cncvar(iv)=TRIM(cncvarbase)//'_bias'
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_float,&
               &                       idimids,idvar(iv)),__LINE__,MYFILE)
            CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
               &          __LINE__,MYFILE)
            iv=iv+1
            cncvar(iv)=TRIM(cncvarbase)//'_rms'
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_float,&
               &                       idimids,idvar(iv)),__LINE__,MYFILE)
            CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
               &          __LINE__,MYFILE)
            iv=iv+1
            cncvar(iv)=TRIM(cncvarbase)//'_std'
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_float,&
               &                       idimids,idvar(iv)),__LINE__,MYFILE)
            CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
               &          __LINE__,MYFILE)
            iv=iv+1
            cncvar(iv)=TRIM(cncvarbase)//'_omean'
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_float,&
               &                       idimids,idvar(iv)),__LINE__,MYFILE)
            CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
               &          __LINE__,MYFILE)
            iv=iv+1
            cncvar(iv)=TRIM(cncvarbase)//'_mmean'
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_float,&
               &                       idimids,idvar(iv)),__LINE__,MYFILE)
            CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
               &          __LINE__,MYFILE)
            iv=iv+1
            cncvar(iv)=TRIM(cncvarbase)//'_count'
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_int,&
               &                       idimids,idvar(iv)),__LINE__,MYFILE)
            CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
               &          __LINE__,MYFILE)
         ENDDO
         DO jadd=1,nobe
            WRITE(cncvarbase,'(3A)')TRIM(cdvar(jvar)),'_',TRIM(cdobe(jadd))
            iv=iv+1
            cncvar(iv)=TRIM(cncvarbase)//'_meanerr'
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_float,&
               &                       idimids,idvar(iv)),__LINE__,MYFILE)
            CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
               &          __LINE__,MYFILE)
            iv=iv+1
            cncvar(iv)=TRIM(cncvarbase)//'_meanvar'
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_float,&
               &                       idimids,idvar(iv)),__LINE__,MYFILE)
            CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
               &          __LINE__,MYFILE)
            iv=iv+1
            cncvar(iv)=TRIM(cncvarbase)//'_count'
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_int,&
               &                       idimids,idvar(iv)),__LINE__,MYFILE)
            CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
               &          __LINE__,MYFILE)
         ENDDO
         DO jadd=1,nbge
            WRITE(cncvarbase,'(3A)')TRIM(cdvar(jvar)),'_',TRIM(cdbge(jadd))
            iv=iv+1
            cncvar(iv)=TRIM(cncvarbase)//'_meanerr'
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_float,&
               &                       idimids,idvar(iv)),__LINE__,MYFILE)
            CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
               &          __LINE__,MYFILE)
            iv=iv+1
            cncvar(iv)=TRIM(cncvarbase)//'_meanvar'
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_float,&
               &                       idimids,idvar(iv)),__LINE__,MYFILE)
            CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
               &          __LINE__,MYFILE)
            iv=iv+1
            cncvar(iv)=TRIM(cncvarbase)//'_count'
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_int,&
               &                       idimids,idvar(iv)),__LINE__,MYFILE)
            CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
               &          __LINE__,MYFILE)
         ENDDO
         WRITE(cncvarbase,'(A)')TRIM(cdvar(jvar))
         iv=iv+1
         cncvar(iv)=TRIM(cncvarbase)//'_omean'
         CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_float,&
            &                       idimids,idvar(iv)),__LINE__,MYFILE)
         CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
            &          __LINE__,MYFILE)
         iv=iv+1
         cncvar(iv)=TRIM(cncvarbase)//'_count'
         CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),nf90_int,&
            &                       idimids,idvar(iv)),__LINE__,MYFILE)
         CALL nchdlerr(nf90_put_att(ncid,idvar(iv),"missing_value",fbstncmiss),&
            &          __LINE__,MYFILE)
      ENDDO
      CALL nchdlerr(nf90_enddef(ncid),__LINE__,MYFILE)
      
      ! Write box names
      
      ip=0
      DO jbox=1,nbox
         IF (.NOT.lskipbox(jbox)) THEN
            ip=ip+1
            clboxnam(ip)=cdboxnam(jbox)
         ENDIF
      ENDDO
      CALL nchdlerr(nf90_put_var(ncid,idvbox,clboxnam),&
         &          __LINE__,MYFILE)

      ! Write levels

      CALL nchdlerr(nf90_put_var(ncid,idvlev,pdep),&
         &          __LINE__,MYFILE)

      ! Write the output data

      iv=0
      DO jvar=1,nvar
         DO jadd=1,nadd
            iv=iv+1
            ip=0
            DO jbox=1,nbox
               IF (.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  ztmp(:,ip)=pbias(:,ip,jadd,jvar)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),ztmp),&
               &          __LINE__,MYFILE)
            iv=iv+1
            ip=0
            DO jbox=1,nbox
               IF (.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  ztmp(:,ip)=prms(:,ip,jadd,jvar)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),ztmp),&
               &          __LINE__,MYFILE)
            iv=iv+1
            ip=0
            DO jbox=1,nbox
               IF (.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  ztmp(:,ip)=pstd(:,ip,jadd,jvar)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),ztmp),&
               &          __LINE__,MYFILE)
            iv=iv+1
            ip=0
            DO jbox=1,nbox
               IF (.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  ztmp(:,ip)=pomean(:,ip,jadd,jvar)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),ztmp),&
               &          __LINE__,MYFILE)
            iv=iv+1
            ip=0
            DO jbox=1,nbox
               IF (.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  ztmp(:,ip)=pmmean(:,ip,jadd,jvar)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),ztmp),&
               &          __LINE__,MYFILE)
            iv=iv+1
            ip=0
            DO jbox=1,nbox
               IF (.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  itmp(:,ip)=knum(:,ip,jadd,jvar)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),itmp),&
               &          __LINE__,MYFILE)
         ENDDO
         DO jadd=1,nobe
            iv=iv+1
            ip=0
            DO jbox=1,nbox
               IF (.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  ztmp(:,ip)=poerr(:,ip,jadd,jvar)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),ztmp),&
               &          __LINE__,MYFILE)
            iv=iv+1
            ip=0
            DO jbox=1,nbox
               IF (.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  ztmp(:,ip)=povar(:,ip,jadd,jvar)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),ztmp),&
               &          __LINE__,MYFILE)
            iv=iv+1
            ip=0
            DO jbox=1,nbox
               IF (.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  itmp(:,ip)=knumo(:,ip,jadd,jvar)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),itmp),&
               &          __LINE__,MYFILE)
         ENDDO
         DO jadd=1,nbge
            iv=iv+1
            ip=0
            DO jbox=1,nbox
               IF (.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  ztmp(:,ip)=pberr(:,ip,jadd,jvar)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),ztmp),&
               &          __LINE__,MYFILE)
            iv=iv+1
            ip=0
            DO jbox=1,nbox
               IF (.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  ztmp(:,ip)=pbvar(:,ip,jadd,jvar)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),ztmp),&
               &          __LINE__,MYFILE)
            iv=iv+1
            ip=0
            DO jbox=1,nbox
               IF (.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  itmp(:,ip)=knumb(:,ip,jadd,jvar)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),itmp),&
               &          __LINE__,MYFILE)
         ENDDO
         iv=iv+1
         ip=0
         DO jbox=1,nbox
            IF (.NOT.lskipbox(jbox)) THEN
               ip=ip+1
               ztmp(:,ip)=poamean(:,ip,jvar)
            ENDIF
         ENDDO
         CALL nchdlerr(nf90_put_var(ncid,idvar(iv),ztmp),&
            &          __LINE__,MYFILE)
         iv=iv+1
         ip=0
         DO jbox=1,nbox
            IF (.NOT.lskipbox(jbox)) THEN
               ip=ip+1
               itmp(:,ip)=knuma(:,ip,jvar)
            ENDIF
         ENDDO
         CALL nchdlerr(nf90_put_var(ncid,idvar(iv),itmp),&
               &          __LINE__,MYFILE)
      ENDDO
      
      ! Close the file

      CALL nchdlerr(nf90_close(ncid),__LINE__,MYFILE)

      DEALLOCATE(cncvar,idvar,ztmp,itmp,clboxnam)

   END SUBROUTINE fbstat_ncwrite

   SUBROUTINE fbstat_ncwrite_hist(cdfilename,nvar,cdvar,nadd,cdadd,&
      & nbox,lenboxname,cdboxnam,lskipbox,nlev,pdep,&
      & zhist,zhistmin,zhiststep,ntyp)
      ! Arguments
      CHARACTER(len=*) :: cdfilename                ! Netcdf filename
      INTEGER :: nvar                               ! Number of variables 
      CHARACTER(len=*), DIMENSION(nvar) :: cdvar    ! Name of variables 
      INTEGER :: nadd                               ! Number of addiables 
      CHARACTER(len=*), DIMENSION(nadd) :: cdadd    ! Name of entries
      INTEGER :: nbox                               ! Number of boxes
      INTEGER :: lenboxname                         ! Length of box names
      CHARACTER(len=lenboxname), dimension(nbox) :: &
         & cdboxnam                                 ! Name of boxes
      LOGICAL, DIMENSION(nbox) :: lskipbox          ! Boxes to skip
      INTEGER :: nlev                               ! Number of levels
      REAL,DIMENSION(nlev) :: pdep                  ! Depth of levels
      TYPE(histtype), DIMENSION(nvar) :: zhist      ! Histogram data
      REAL, DIMENSION(nvar) :: &
         & zhistmin,zhiststep                       ! Histogram info
      integer :: ntyp                               ! Type to write
      ! Local variables
      INTEGER :: jadd,jvar,incvar,ji,iv,ip,jbox
      CHARACTER(len=50) :: cncvarbase
      CHARACTER(len=60), ALLOCATABLE, DIMENSION(:) :: cncvar
      ! netcdf stuff
      INTEGER :: ncid,idlev,idbox,idlbox,idimhist(nvar),&
         & idimdep(1),idimbox(2),idimids(2),idimval(1),idimcnt(3)
      INTEGER :: idvbox,idvlev
      INTEGER, ALLOCATABLE, DIMENSION(:) :: idvar
      CHARACTER(len=40) :: cdhdimname
      REAL, ALLOCATABLE, DIMENSION(:) :: zhval
      INTEGER :: inoboxes
      INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: itmp
      CHARACTER(len=lenboxname), DIMENSION(:), ALLOCATABLE :: &
         & clboxnam                                 ! Name of boxes

      ! Open netCDF files.

      CALL nchdlerr(nf90_create(TRIM(cdfilename),nf90_clobber,ncid),&
         &          __LINE__,MYFILE)

      ! Create dimensions

      inoboxes=nbox-COUNT(lskipbox) 
      ALLOCATE(clboxnam(inoboxes))

      CALL nchdlerr(nf90_def_dim(ncid,"depth",nlev,idlev),__LINE__,MYFILE)

      CALL nchdlerr(nf90_def_dim(ncid,"box",inoboxes,idbox),&
         &          __LINE__,MYFILE)

      CALL nchdlerr(nf90_def_dim(ncid,"len",lenboxname,idlbox),__LINE__,MYFILE)
      
      DO jvar=1,nvar
         WRITE(cdhdimname,'(A,A)')'hist',TRIM(cdvar(jvar))
         CALL nchdlerr(nf90_def_dim(ncid,TRIM(cdhdimname),&
            &                       zhist(jvar)%npoints,idimhist(jvar)),&
            & __LINE__,MYFILE)
      ENDDO

      ! Box variable name

      idimbox(1)=idlbox
      idimbox(2)=idbox
      CALL nchdlerr(nf90_def_var(ncid,'box',nf90_char,idimbox,idvbox),&
         &          __LINE__,MYFILE)

      ! Depths

      idimdep(1)=idlev
      CALL nchdlerr(nf90_def_var(ncid,'depth',nf90_float,idimdep,idvlev),&
         &          __LINE__,MYFILE)

      ! Histogram values and depths

      incvar=nvar+nadd*nvar
      ALLOCATE(cncvar(incvar),idvar(incvar))
      iv=0
      DO jvar=1,nvar
         iv=iv+1
         WRITE(cncvar(iv),'(A,A)')TRIM(cdvar(jvar)),'_val'
         idimval(1)=idimhist(jvar)
         CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),&
            &                       nf90_float,idimval,idvar(iv)),&
            &          __LINE__,MYFILE)
         DO jadd=1,nadd
            iv=iv+1
            WRITE(cncvar(iv),'(A,A,A)')TRIM(cdvar(jvar)),&
               &                       TRIM(cdadd(jadd)),'_count'
            idimcnt(1)=idimhist(jvar)
            idimcnt(2)=idlev
            idimcnt(3)=idbox
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),&
               &                       nf90_int,idimcnt,idvar(iv)),&
               &          __LINE__,MYFILE)
         ENDDO
      ENDDO
      CALL nchdlerr(nf90_enddef(ncid),__LINE__,MYFILE)
      
      ! Write box names

      ip=0
      DO jbox=1,nbox
         IF (.NOT.lskipbox(jbox)) THEN
            ip=ip+1
            clboxnam(ip)=cdboxnam(jbox)
         ENDIF
      ENDDO
      CALL nchdlerr(nf90_put_var(ncid,idvbox,clboxnam),&
         &          __LINE__,MYFILE)

      ! Write levels

      CALL nchdlerr(nf90_put_var(ncid,idvlev,pdep),&
         &          __LINE__,MYFILE)

      iv=0
      DO jvar=1,nvar
         iv=iv+1
         ALLOCATE(zhval(zhist(jvar)%npoints))
         DO ji=1,zhist(jvar)%npoints
            zhval(ji)=(ji-1)*zhiststep(jvar)+zhistmin(jvar)
         ENDDO
         CALL nchdlerr(nf90_put_var(ncid,idvar(iv),zhval),&
            &          __LINE__,MYFILE)
         DEALLOCATE(zhval)
         DO jadd=1,nadd
            iv=iv+1
            ALLOCATE(itmp(zhist(jvar)%npoints,nlev,inoboxes))
            ip=0
            DO jbox=1,nbox
               IF(.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  itmp(:,:,ip)=zhist(jvar)%nhist(:,:,ip,jadd,ntyp)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),itmp),&
               &          __LINE__,MYFILE)
            DEALLOCATE(itmp)
         ENDDO
      ENDDO

      ! Close the file

      CALL nchdlerr(nf90_close(ncid),__LINE__,MYFILE)
      
      DEALLOCATE(cncvar,idvar,clboxnam)

   END SUBROUTINE fbstat_ncwrite_hist

   SUBROUTINE fbstat_ncwrite_xy(cdfilename,nvar,cdvar,nadd,cdadd,&
      & nbox,lenboxname,cdboxnam,lskipbox,nlev,pdep,&
      & zxy,zxymin,zxystep,ntyp)
      ! Arguments
      CHARACTER(len=*) :: cdfilename                ! Netcdf filename
      INTEGER :: nvar                               ! Number of variables 
      CHARACTER(len=*), DIMENSION(nvar) :: cdvar    ! Name of variables 
      INTEGER :: nadd                               ! Number of addiables 
      CHARACTER(len=*), DIMENSION(nadd) :: cdadd    ! Name of entries
      INTEGER :: nbox                               ! Number of boxes
      INTEGER :: lenboxname                         ! Length of box names
      CHARACTER(len=lenboxname), dimension(nbox) :: &
         & cdboxnam                                 ! Name of boxes
      LOGICAL, DIMENSION(nbox) :: lskipbox          ! Boxes to skip
      INTEGER :: nlev                               ! Number of levels
      REAL,DIMENSION(nlev) :: pdep                  ! Depth of levels
      TYPE(xytype), DIMENSION(nvar) :: zxy          ! xyplot data
      REAL, DIMENSION(nvar) :: &
         & zxymin,zxystep                           ! xyplot info
      integer :: ntyp                               ! Type to write
      ! Local variables
      INTEGER :: jadd,jvar,incvar,ji,iv,ip,jbox
      CHARACTER(len=50) :: cncvarbase
      CHARACTER(len=60), ALLOCATABLE, DIMENSION(:) :: cncvar
      ! netcdf stuff
      INTEGER :: ncid,idlev,idbox,idlbox,idimxy(nvar),&
         & idimdep(1),idimbox(2),idimids(2),idimval(1),idimcnt(4)
      INTEGER :: idvbox,idvlev
      INTEGER, ALLOCATABLE, DIMENSION(:) :: idvar
      CHARACTER(len=40) :: cdhdimname
      REAL, ALLOCATABLE, DIMENSION(:) :: zhval
      INTEGER :: inoboxes
      INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:) :: itmp
      CHARACTER(len=lenboxname), DIMENSION(:), ALLOCATABLE :: &
         & clboxnam                                 ! Name of boxes

      ! Open netCDF files.

      CALL nchdlerr(nf90_create(TRIM(cdfilename),nf90_clobber,ncid),&
         &          __LINE__,MYFILE)

      ! Create dimensions

      inoboxes=nbox-COUNT(lskipbox) 
      ALLOCATE(clboxnam(inoboxes))

      CALL nchdlerr(nf90_def_dim(ncid,"depth",nlev,idlev),__LINE__,MYFILE)

      CALL nchdlerr(nf90_def_dim(ncid,"box",inoboxes,idbox),&
         &          __LINE__,MYFILE)

      CALL nchdlerr(nf90_def_dim(ncid,"len",lenboxname,idlbox),__LINE__,MYFILE)
      
      DO jvar=1,nvar
         WRITE(cdhdimname,'(A,A)')'xy',TRIM(cdvar(jvar))
         CALL nchdlerr(nf90_def_dim(ncid,TRIM(cdhdimname),&
            &                       zxy(jvar)%npoints,idimxy(jvar)),&
            & __LINE__,MYFILE)
      ENDDO

      ! Box variable name

      idimbox(1)=idlbox
      idimbox(2)=idbox
      CALL nchdlerr(nf90_def_var(ncid,'box',nf90_char,idimbox,idvbox),&
         &          __LINE__,MYFILE)

      ! Depths

      idimdep(1)=idlev
      CALL nchdlerr(nf90_def_var(ncid,'depth',nf90_float,idimdep,idvlev),&
         &          __LINE__,MYFILE)

      ! Histogram values and depths

      incvar=nvar+nadd*nvar
      ALLOCATE(cncvar(incvar),idvar(incvar))
      iv=0
      DO jvar=1,nvar
         iv=iv+1
         WRITE(cncvar(iv),'(A,A)')TRIM(cdvar(jvar)),'_val'
         idimval(1)=idimxy(jvar)
         CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),&
            &                       nf90_float,idimval,idvar(iv)),&
            &          __LINE__,MYFILE)
         DO jadd=1,nadd
            iv=iv+1
            WRITE(cncvar(iv),'(A,A,A)')TRIM(cdvar(jvar)),&
               &                       TRIM(cdadd(jadd)),'_count'
            idimcnt(1)=idimxy(jvar)
            idimcnt(2)=idimxy(jvar)
            idimcnt(3)=idlev
            idimcnt(4)=idbox
            CALL nchdlerr(nf90_def_var(ncid,TRIM(cncvar(iv)),&
               &                       nf90_int,idimcnt,idvar(iv)),&
               &          __LINE__,MYFILE)
         ENDDO
      ENDDO
      CALL nchdlerr(nf90_enddef(ncid),__LINE__,MYFILE)
      
      ! Write box names

      ip=0
      DO jbox=1,nbox
         IF (.NOT.lskipbox(jbox)) THEN
            ip=ip+1
            clboxnam(ip)=cdboxnam(jbox)
         ENDIF
      ENDDO
      CALL nchdlerr(nf90_put_var(ncid,idvbox,clboxnam),&
         &          __LINE__,MYFILE)

      ! Write levels

      CALL nchdlerr(nf90_put_var(ncid,idvlev,pdep),&
         &          __LINE__,MYFILE)

      iv=0
      DO jvar=1,nvar
         iv=iv+1
         ALLOCATE(zhval(zxy(jvar)%npoints))
         DO ji=1,zxy(jvar)%npoints
            zhval(ji)=(ji-1)*zxystep(jvar)+zxymin(jvar)
         ENDDO
         CALL nchdlerr(nf90_put_var(ncid,idvar(iv),zhval),&
            &          __LINE__,MYFILE)
         DEALLOCATE(zhval)
         DO jadd=1,nadd
            iv=iv+1
            ALLOCATE(itmp(zxy(jvar)%npoints,zxy(jvar)%npoints,nlev,inoboxes))
            ip=0
            DO jbox=1,nbox
               IF(.NOT.lskipbox(jbox)) THEN
                  ip=ip+1
                  itmp(:,:,:,ip)=zxy(jvar)%nxy(:,:,:,ip,jadd,ntyp)
               ENDIF
            ENDDO
            CALL nchdlerr(nf90_put_var(ncid,idvar(iv),itmp),&
               &          __LINE__,MYFILE)
            DEALLOCATE(itmp)
         ENDDO
      ENDDO

      ! Close the file

      CALL nchdlerr(nf90_close(ncid),__LINE__,MYFILE)
      
      DEALLOCATE(cncvar,idvar,clboxnam)

   END SUBROUTINE fbstat_ncwrite_xy

   SUBROUTINE fbstat_ncread(cdfilename,cdvar,sdata)
      ! Arguments
      CHARACTER(len=*) :: cdfilename                ! Netcdf filename
      CHARACTER(len=*) :: cdvar                     ! Name of variables 
      TYPE(fbstatnctype) :: sdata                 ! Data to be filled
      ! Local variables
      INTEGER :: nbox,nlev,nadd,nvar
      INTEGER :: ncid,dimid,varid,i,icntpos
      CHARACTER(len=128) :: cdname,tmpname

      ! Open the file and get the dimensions

      CALL nchdlerr(nf90_open(cdfilename,nf90_nowrite,ncid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inq_dimid(ncid,'box',dimid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inquire_dimension(ncid,dimid,&
         &                      len=nbox),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inq_dimid(ncid,'depth',dimid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inquire_dimension(ncid,dimid,&
         &                      len=nlev),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inquire(ncid,nVariables=nvar),__LINE__,MYFILE)

      ! Count the number of variables and find the "count" position

      nadd=0
      icntpos=0
      DO i=1,nvar
         CALL nchdlerr(nf90_inquire_variable(ncid,i,name=cdname),&
            &          __LINE__,MYFILE)
         IF (TRIM(cdvar)//'_count'==TRIM(cdname)) THEN
            icntpos=i
         ELSE
            IF (TRIM(cdvar)==cdname(1:LEN_TRIM(cdvar))) THEN
               tmpname=cdname(LEN_TRIM(cdvar)+2:)
               IF (INDEX(tmpname,'_')==0) THEN
                  nadd=nadd+1
               ENDIF
            ENDIF
         ENDIF
      ENDDO

      ! Allocate the data structure

      CALL fbstat_ncread_alloc(sdata,nlev,nbox,nadd)

      ! Get the box names in files

      CALL nchdlerr(nf90_inq_varid(ncid,'box',varid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_get_var(ncid,varid,sdata%area),__LINE__,MYFILE)
      
      ! Get the depths
      
      CALL nchdlerr(nf90_inq_varid(ncid,'depth',varid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_get_var(ncid,varid,sdata%dep),__LINE__,MYFILE)

      nadd=0
      DO i=1,nvar
         CALL nchdlerr(nf90_inquire_variable(ncid,i,name=cdname),&
            &          __LINE__,MYFILE)
         IF (i==icntpos) THEN
            CALL nchdlerr(nf90_get_var(ncid,i,sdata%cnt),__LINE__,MYFILE)
         ELSE
            IF (TRIM(cdvar)==cdname(1:LEN_TRIM(cdvar))) THEN
               tmpname=cdname(LEN_TRIM(cdvar)+2:)
               IF (INDEX(tmpname,'_')==0) THEN
                  nadd=nadd+1
                  sdata%name(nadd)=tmpname(1:MAX(LEN_TRIM(tmpname),32))
                  CALL nchdlerr(nf90_get_var(ncid,i,sdata%val(:,:,nadd)),&
                     &                       __LINE__,MYFILE)
               ENDIF
            ENDIF
         ENDIF
      ENDDO

      CALL nchdlerr(nf90_close(ncid),__LINE__,MYFILE)

   END SUBROUTINE fbstat_ncread

   SUBROUTINE fbstat_ncread_alloc(sdata,nlev,nbox,nadd)
      ! Arguments
      TYPE(fbstatnctype) :: sdata    ! Data to be allocated
      INTEGER :: nlev,nbox,nadd
      ! Local variables

      sdata%nlev=nlev
      sdata%nbox=nbox
      sdata%nadd=nadd
      ALLOCATE( &
         & sdata%area(nbox), &
         & sdata%dep(nlev), &
         & sdata%name(nadd), &
         & sdata%val(nlev,nbox,nadd), &
         & sdata%cnt(nlev,nbox) &
         )
      
   END SUBROUTINE fbstat_ncread_alloc

   SUBROUTINE fbstat_ncread_dealloc(sdata)
      ! Arguments
      TYPE(fbstatnctype) :: sdata    ! Data to be deallocated
      ! Local variables

      sdata%nlev=0
      sdata%nbox=0
      sdata%nadd=0
      DEALLOCATE( &
         & sdata%area, &
         & sdata%dep, &
         & sdata%name, &
         & sdata%val, &
         & sdata%cnt &
         )
      
   END SUBROUTINE fbstat_ncread_dealloc

   SUBROUTINE fbstat_ncread_hist(cdfilename,cdvar,cdext,sdata)
      ! Arguments
      CHARACTER(len=*) :: cdfilename                ! Netcdf filename
      CHARACTER(len=*) :: cdvar                     ! Name of variables 
      CHARACTER(len=*) :: cdext                     ! Name of extras
      TYPE(fbstathistnctype) :: sdata               ! Data to be filled
      ! Local variables
      INTEGER :: nbox,nlev,npoints
      INTEGER :: ncid,dimid,varid

      ! Open the file and get the dimensions

      CALL nchdlerr(nf90_open(cdfilename,nf90_nowrite,ncid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inq_dimid(ncid,'box',dimid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inquire_dimension(ncid,dimid,&
         &                      len=nbox),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inq_dimid(ncid,'depth',dimid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inquire_dimension(ncid,dimid,&
         &                      len=nlev),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inq_dimid(ncid,'hist'//TRIM(cdvar),dimid),&
         &                                __LINE__,MYFILE)
      CALL nchdlerr(nf90_inquire_dimension(ncid,dimid,&
         &                      len=npoints),__LINE__,MYFILE)

      ! Allocate the data structure

      CALL fbstat_ncread_hist_alloc(sdata,npoints,nlev,nbox)

      ! Get the box names in files

      CALL nchdlerr(nf90_inq_varid(ncid,'box',varid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_get_var(ncid,varid,sdata%area),__LINE__,MYFILE)
      
      ! Get the depths
      
      CALL nchdlerr(nf90_inq_varid(ncid,'depth',varid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_get_var(ncid,varid,sdata%dep),__LINE__,MYFILE)

      ! Get values

      CALL nchdlerr(nf90_inq_varid(ncid,TRIM(cdvar)//'_val',varid),&
         &          __LINE__,MYFILE)
      CALL nchdlerr(nf90_get_var(ncid,varid,sdata%val),__LINE__,MYFILE)

      ! Get histograms

      CALL nchdlerr(nf90_inq_varid(ncid,&
         &                         TRIM(cdvar)//TRIM(cdext)//'_count',varid),&
         &          __LINE__,MYFILE)
      CALL nchdlerr(nf90_get_var(ncid,varid,sdata%nhist),__LINE__,MYFILE)

      CALL nchdlerr(nf90_close(ncid),__LINE__,MYFILE)

   END SUBROUTINE fbstat_ncread_hist

   SUBROUTINE fbstat_ncread_hist_alloc(sdata,npoints,nlev,nbox)
      ! Arguments
      TYPE(fbstathistnctype) :: sdata    ! Data to be allocated
      INTEGER :: npoints,nlev,nbox
      ! Local variables

      sdata%nlev=nlev
      sdata%nbox=nbox
      sdata%npoints=npoints
      ALLOCATE( &
         & sdata%area(nbox), &
         & sdata%dep(nlev), &
         & sdata%val(npoints), &
         & sdata%nhist(npoints,nlev,nbox) &
         & )
      
   END SUBROUTINE fbstat_ncread_hist_alloc

   SUBROUTINE fbstat_ncread_hist_dealloc(sdata)
      ! Arguments
      TYPE(fbstathistnctype) :: sdata    ! Data to be deallocated
      ! Local variables

      sdata%nlev=0
      sdata%nbox=0
      sdata%npoints=0
      DEALLOCATE( &
         & sdata%area, &
         & sdata%dep, &
         & sdata%val, &
         & sdata%nhist &
         & )
      
   END SUBROUTINE fbstat_ncread_hist_dealloc

   SUBROUTINE fbstat_ncread_xy(cdfilename,cdvar,cdext,sdata)
      ! Arguments
      CHARACTER(len=*) :: cdfilename                ! Netcdf filename
      CHARACTER(len=*) :: cdvar                     ! Name of variables 
      CHARACTER(len=*) :: cdext                     ! Name of extras
      TYPE(fbstatxynctype) :: sdata                 ! Data to be filled
      ! Local variables
      INTEGER :: nbox,nlev,npoints
      INTEGER :: ncid,dimid,varid

      ! Open the file and get the dimensions

      CALL nchdlerr(nf90_open(cdfilename,nf90_nowrite,ncid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inq_dimid(ncid,'box',dimid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inquire_dimension(ncid,dimid,&
         &                      len=nbox),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inq_dimid(ncid,'depth',dimid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inquire_dimension(ncid,dimid,&
         &                      len=nlev),__LINE__,MYFILE)
      CALL nchdlerr(nf90_inq_dimid(ncid,'xy'//TRIM(cdvar),dimid),&
         &                                __LINE__,MYFILE)
      CALL nchdlerr(nf90_inquire_dimension(ncid,dimid,&
         &                      len=npoints),__LINE__,MYFILE)

      ! Allocate the data structure

      CALL fbstat_ncread_xy_alloc(sdata,npoints,nlev,nbox)

      ! Get the box names in files

      CALL nchdlerr(nf90_inq_varid(ncid,'box',varid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_get_var(ncid,varid,sdata%area),__LINE__,MYFILE)
      
      ! Get the depths
      
      CALL nchdlerr(nf90_inq_varid(ncid,'depth',varid),__LINE__,MYFILE)
      CALL nchdlerr(nf90_get_var(ncid,varid,sdata%dep),__LINE__,MYFILE)

      ! Get values

      CALL nchdlerr(nf90_inq_varid(ncid,TRIM(cdvar)//'_val',varid),&
         &          __LINE__,MYFILE)
      CALL nchdlerr(nf90_get_var(ncid,varid,sdata%val),__LINE__,MYFILE)

      ! Get xyograms

      CALL nchdlerr(nf90_inq_varid(ncid,&
         &                         TRIM(cdvar)//TRIM(cdext)//'_count',varid),&
         &          __LINE__,MYFILE)
      CALL nchdlerr(nf90_get_var(ncid,varid,sdata%nxy),__LINE__,MYFILE)

      CALL nchdlerr(nf90_close(ncid),__LINE__,MYFILE)

   END SUBROUTINE fbstat_ncread_xy

   SUBROUTINE fbstat_ncread_xy_alloc(sdata,npoints,nlev,nbox)
      ! Arguments
      TYPE(fbstatxynctype) :: sdata    ! Data to be allocated
      INTEGER :: npoints,nlev,nbox
      ! Local variables

      sdata%nlev=nlev
      sdata%nbox=nbox
      sdata%npoints=npoints
      ALLOCATE( &
         & sdata%area(nbox), &
         & sdata%dep(nlev), &
         & sdata%val(npoints), &
         & sdata%nxy(npoints,npoints,nlev,nbox) &
         & )
      
   END SUBROUTINE fbstat_ncread_xy_alloc

   SUBROUTINE fbstat_ncread_xy_dealloc(sdata)
      ! Arguments
      TYPE(fbstatxynctype) :: sdata    ! Data to be deallocated
      ! Local variables

      sdata%nlev=0
      sdata%nbox=0
      sdata%npoints=0
      DEALLOCATE( &
         & sdata%area, &
         & sdata%dep, &
         & sdata%val, &
         & sdata%nxy &
         & )
      
   END SUBROUTINE fbstat_ncread_xy_dealloc

END MODULE fbstatncio
