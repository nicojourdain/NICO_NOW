PROGRAM fbsel
   !!---------------------------------------------------------------------
   !!
   !!                     ** PROGRAM fbsel **
   !!
   !!  ** Purpose : Select or subsample observations
   !!
   !!  ** Method  : Use of utilities from obs_fbm.
   !!
   !!  ** Action  :
   !!
   !!   Usage:
   !!     fbsel.exe <input filename> <output filename>
   !!
   !!   History :
   !!        ! 2010 (K. Mogensen) Initial version
   !!----------------------------------------------------------------------
   USE obs_fbm
   USE date_utils
   IMPLICIT NONE
   TYPE(obfbdata) :: fbdatain,fbdataout
   CHARACTER(len=256) :: filenamein,filenameout,filenametmp,cnameout
#ifndef NOIARGCPROTO
   INTEGER,EXTERNAL :: iargc
#endif
   INTEGER,PARAMETER :: maxtyp=1023
   INTEGER,PARAMETER :: maxdates=20
   INTEGER :: nqc,ntyp,ndates,ninidate(maxdates),nenddate(maxdates)
   LOGICAL :: lsplitqc,lsplittyp,lsplitstat
   INTEGER :: iqc,ityp,idate,istat
   REAL :: maxlat,minlat,maxlon,minlon
   CHARACTER(len=ilenwmo) :: cdwmo,cdwmobeg,cdwmoend
   CHARACTER(len=ilenwmo), DIMENSION(:), POINTER :: clstatids
   INTEGER :: nstat
   NAMELIST/namsel/nqc,ntyp,ndates,ninidate,nenddate,lsplitqc,lsplittyp, &
      &            lsplitstat,maxlat,minlat,maxlon,minlon,cdwmo,&
      &            cdwmobeg,cdwmoend

   IF (iargc()/=2) THEN
      WRITE(*,*)'Usage:'
      WRITE(*,*)'fbsel <input filename> <output filename>'
      CALL abort
   ENDIF

   CALL getarg(1,filenamein)
   CALL getarg(2,filenameout)
   
   nqc=-1
   ntyp=-1
   ndates=1
   ninidate=19500101
   nenddate=21000101

   lsplitqc=.FALSE.
   lsplittyp=.FALSE.
   lsplitstat=.FALSE.
   cdwmo=REPEAT('X',ilenwmo)
   cdwmobeg=cdwmo
   cdwmoend=cdwmo
   maxlat=1e+38
   minlat=-1e+38
   maxlon=1e+38
   minlon=-1e+38
   OPEN(10,file='namsel.in')
   READ(10,namsel)
   CLOSE(10)
   IF (cdwmobeg==REPEAT('X',ilenwmo)) cdwmobeg=cdwmo
   IF (cdwmoend==REPEAT('X',ilenwmo)) cdwmoend=cdwmo
   WRITE(*,namsel)

   CALL init_obfbdata(fbdatain)
   CALL init_obfbdata(fbdataout)

   WRITE(*,*)'Reading file : ',TRIM(filenamein)
   CALL read_obfbdata(TRIM(filenamein),fbdatain)
   WRITE(*,*)'Number of observations before selection = ',fbdatain%nobs
   DO idate=1,ndates
      IF (ndates==1) THEN
         cnameout=filenameout
      ELSE
         WRITE(cnameout,'(I2.2,2A)')idate,'_',TRIM(filenameout)
      ENDIF
      IF (lsplitqc) THEN
         DO iqc=1,3
            CALL fb_sel(fbdatain,fbdataout,iqc,ntyp, &
               &        ninidate(idate),nenddate(idate), &
               &        maxlat,minlat,maxlon,minlon,cdwmobeg,cdwmoend)
            WRITE(filenametmp,'(A,I2.2,A,A)')'qc_',iqc,'_',TRIM(cnameout)
            IF (fbdataout%nobs>0) THEN
               WRITE(*,*)'QC selected = ',iqc
               WRITE(*,*)'Number of observations selected = ',fbdataout%nobs
               WRITE(*,*)'Writing file : ',TRIM(filenametmp)
               CALL write_obfbdata(TRIM(filenametmp),fbdataout)
            ENDIF
            CALL dealloc_obfbdata(fbdataout)
         ENDDO
      ELSEIF (lsplittyp) THEN
         DO ityp=0,maxtyp
            CALL fb_sel(fbdatain,fbdataout,nqc,ityp, &
               &        ninidate(idate),nenddate(idate), &
               &        maxlat,minlat,maxlon,minlon,cdwmobeg,cdwmoend)
            WRITE(filenametmp,'(A,I4.4,A,A)')'typ_',ityp,'_',TRIM(cnameout)
            IF (fbdataout%nobs>0) THEN
               WRITE(*,*)'Type = ',ityp
               WRITE(*,*)'Number of observations selected = ',fbdataout%nobs
               WRITE(*,*)'Writing file : ',TRIM(filenametmp)
               CALL write_obfbdata(TRIM(filenametmp),fbdataout)
            ENDIF
            CALL dealloc_obfbdata(fbdataout)
         ENDDO
      ELSEIF (lsplitstat) THEN
         CALL fb_sel_uniqueids(fbdatain,clstatids,nstat)
         DO istat=1,nstat
            CALL fb_sel(fbdatain,fbdataout,nqc,ntyp, &
               &        ninidate(idate),nenddate(idate), &
               &        maxlat,minlat,maxlon,minlon,clstatids(istat),clstatids(istat))
            WRITE(filenametmp,'(4A)')'statid_', &
               & TRIM(clstatids(istat)),'_',TRIM(cnameout)
            IF (fbdataout%nobs>0) THEN
               WRITE(*,*)'Station = ',clstatids(istat)
               WRITE(*,*)'Number of observations selected = ',fbdataout%nobs
               WRITE(*,*)'Writing file : ',TRIM(filenametmp)
               CALL write_obfbdata(TRIM(filenametmp),fbdataout)
            ENDIF
            CALL dealloc_obfbdata(fbdataout)
         ENDDO
      ELSE
         CALL fb_sel(fbdatain,fbdataout,nqc,ntyp, & 
            &        ninidate(idate),nenddate(idate), &
            &        maxlat,minlat,maxlon,minlon,cdwmobeg,cdwmoend)
         WRITE(*,*)'Number of observations selected = ',fbdataout%nobs
         WRITE(*,*)'Writing file : ',TRIM(cnameout)
         CALL write_obfbdata(TRIM(cnameout),fbdataout)
         CALL dealloc_obfbdata(fbdataout)
      ENDIF
   ENDDO

CONTAINS

   SUBROUTINE fb_sel(fbdatain,fbdataout,nqc,ntyp,ninidate,nenddate,&
      &              maxlat,minlat,maxlon,minlon,cdwmobeg,cdwmoend)
      TYPE(obfbdata) :: fbdatain,fbdataout
      INTEGER :: nqc,ntyp,ninidate,nenddate
      REAL :: maxlat,minlat,maxlon,minlon
      CHARACTER(len=ilenwmo) :: cdwmobeg,cdwmoend
      INTEGER :: jobs
      INTEGER :: iqc,ityp
      LOGICAL :: llvalid(fbdatain%nobs)
      INTEGER :: iyea,imon,iday
      REAL(KIND=dp) :: zjini,zjend
      LOGICAL :: lcheckwmo

      lcheckwmo=(cdwmobeg/=REPEAT('X',ilenwmo)).OR.&
         &      (cdwmoend/=REPEAT('X',ilenwmo))
      iyea=ninidate/10000
      imon=ninidate/100-iyea*100
      iday=ninidate-iyea*10000-imon*100
      CALL greg2jul(0,0,0,iday,imon,iyea,zjini)
      iyea=nenddate/10000
      imon=nenddate/100-iyea*100
      iday=nenddate-iyea*10000-imon*100
      CALL greg2jul(0,0,0,iday,imon,iyea,zjend)
      DO jobs = 1, fbdatain%nobs
         llvalid(jobs)=.TRUE.
         IF (nqc/=-1) THEN
            CALL check_prof(fbdatain,jobs,iqc)
            llvalid(jobs)=(iqc==nqc).AND.llvalid(jobs)
         ENDIF
         IF (ntyp/=-1) THEN
            READ(fbdatain%cdtyp(jobs),'(I4)')ityp
            llvalid(jobs)=(ityp==ntyp).AND.llvalid(jobs)
         ENDIF
         IF (ninidate/=-1) THEN
            llvalid(jobs)=(fbdatain%ptim(jobs)>zjini).AND.llvalid(jobs)
         ENDIF
         IF (nenddate/=-1) THEN
            llvalid(jobs)=(fbdatain%ptim(jobs)<=zjend).AND.llvalid(jobs)
         ENDIF
         llvalid(jobs)=(fbdatain%pphi(jobs)<=maxlat).AND.       &
            &          (fbdatain%pphi(jobs)>=minlat).AND.       &
            &          (((fbdatain%plam(jobs)<=maxlon).AND.     &
            &            (fbdatain%plam(jobs)>=minlon)).OR.     &
            &           ((fbdatain%plam(jobs)+360<=maxlon).AND. &
            &            (fbdatain%plam(jobs)+360>=minlon)).OR. &
            &           ((fbdatain%plam(jobs)-360<=maxlon).AND. &
            &            (fbdatain%plam(jobs)-360>=minlon))).AND.llvalid(jobs) 
         IF (lcheckwmo) THEN
            llvalid(jobs)=LGE(TRIM(fbdatain%cdwmo(jobs)),TRIM(cdwmobeg)) &
               &    .AND. LLE(TRIM(fbdatain%cdwmo(jobs)),TRIM(cdwmoend)) &
               &    .AND. llvalid(jobs)
         ENDIF
         ! Add more checks here...
      ENDDO

      CALL subsamp_obfbdata(fbdatain,fbdataout,llvalid)

   END SUBROUTINE fb_sel

   SUBROUTINE fb_sel_uniqueids(fbdatain,clstatids,nstat)
      TYPE(obfbdata) :: fbdatain
      CHARACTER(len=ilenwmo), DIMENSION(:), POINTER :: clstatids
      INTEGER :: nstat
      INTEGER :: jobs,kobs
      LOGICAL, DIMENSION(fbdatain%nobs) :: lunique

      lunique(:)=.TRUE.
      DO jobs=1,fbdatain%nobs
         IF (.NOT.lunique(jobs)) CYCLE
         DO kobs=jobs+1,fbdatain%nobs
            IF (.NOT.lunique(kobs)) CYCLE
            IF (fbdatain%cdwmo(jobs)==fbdatain%cdwmo(kobs)) THEN
               lunique(kobs)=.FALSE.
            ENDIF
         ENDDO
      ENDDO
      nstat=COUNT(lunique)
      ALLOCATE(clstatids(nstat))
      kobs=0
      DO jobs=1,fbdatain%nobs
         IF (lunique(jobs)) THEN
            kobs=kobs+1
            clstatids(kobs)=fbdatain%cdwmo(jobs)
         ENDIF
      ENDDO
      WRITE(*,*)'Unique station ids'
      DO jobs=1,nstat
         WRITE(*,'(I5,1X,A)')jobs,clstatids(jobs)
      ENDDO

   END SUBROUTINE fb_sel_uniqueids
   
   SUBROUTINE check_prof(fbdata,iobs,iqc)
      
      TYPE(obfbdata) :: fbdata
      INTEGER :: iobs,iqc
      INTEGER :: i,ivar
      
      LOGICAL :: lpart,lfull

      lpart=.false.
      lfull=.true.
      DO ivar=1,fbdata%nvar
         DO i=1,fbdata%nlev
            IF ((fbdata%ivlqc(i,iobs,ivar)>2).AND.&
               &(fbdata%ivlqc(i,iobs,ivar)<9)) lpart = .TRUE.
            IF (fbdata%ivlqc(i,iobs,ivar)<=2) lfull = .FALSE.
         ENDDO
      ENDDO

      IF(lfull) THEN
         iqc=3
      ELSEIF (lpart) then
         iqc=2
      ELSE
         iqc=1
      ENDIF

   END SUBROUTINE check_prof

END PROGRAM fbsel
