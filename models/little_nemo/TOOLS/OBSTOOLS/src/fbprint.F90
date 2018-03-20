PROGRAM fbprint
   !!---------------------------------------------------------------------
   !!
   !!                     ** PROGRAM fbprint **
   !!
   !!  ** Purpose : Print feedback file contents as text
   !!
   !!  ** Method  : Use of utilities from obs_fbm.
   !!
   !!  ** Action  :
   !!
   !!   Usage :
   !!     fbprint.exe [options] inputfile
   !!   Options :
   !!     -b            shorter output
   !!     -q            QC flags (nqc=1) select observations based on QC flags
   !!     -Q            QC flags (nqc=2) select observations based on QC flags
   !!     -B            QC flags (nqc=3) select observations based on QC flags
   !!     -u            unsorted
   !!     -s ID         select station ID  
   !!     -t TYPE       select observation type
   !!     -v NUM1-NUM2  select variable range to print by number (default all)
   !!     -a NUM1-NUM2  select additional variable range to print by number (default all)
   !!     -e NUM1-NUM2  select extra variable range to print by number (default all)
   !!     -d            output date range
   !!     -D            print depths
   !!     -z            use zipped files
   !! 
   !!   History :
   !!        ! 2010 (K. Mogensen) Initial version
   !!----------------------------------------------------------------------
   USE toolspar_kind 
   USE obs_fbm
   USE index_sort
   USE date_utils
   USE proftools

   IMPLICIT NONE
   !
   ! Command line arguments input file
   !
#ifndef NOIARGCPROTO
   INTEGER,EXTERNAL :: iargc
#endif
   INTEGER :: nargs
   CHARACTER(len=256) :: cdinfile, cdbrief
   LOGICAL :: lbrief, lqcflags, lstat, ltyp, lsort, ldaterange, lzinp, ldepths
   CHARACTER(len=ilenwmo) :: cdstat
   CHARACTER(len=ilentyp) :: cdtyp
   INTEGER :: nqc
   INTEGER :: nvar1, nvar2, nadd1, nadd2, next1, next2
   !
   ! Input data
   !
   TYPE(obfbdata) :: obsdata
   !
   ! Loop variables
   !
   INTEGER :: ii, iarg, ip
   !
   ! Sorting
   !
   INTEGER :: iwmo
   REAL(KIND=dp),ALLOCATABLE :: zsort(:,:)
   INTEGER,ALLOCATABLE  :: iindex(:)
   !
   ! Get number of command line arguments
   !
   nargs  = IARGC()
   lbrief = .FALSE.
   lstat = .FALSE.
   ltyp = .FALSE.
   ldaterange = .FALSE.
   ldepths = .FALSE.
   cdstat = 'XXXXXXX'
   cdtyp = 'XXXX'
   lsort = .TRUE.
   nqc = 0
   nvar1 = -1
   nvar2 = -1
   nadd1 = -1
   nadd2 = -1
   next1 = -1
   next2 = -1
   lzinp = .FALSE.
   IF ( nargs < 1 ) THEN
      CALL usage()
   ENDIF
   iarg = 1
   DO
      IF ( iarg == nargs ) EXIT
      CALL getarg( iarg, cdbrief )
      IF ( cdbrief == '-b' ) THEN
         lbrief = .TRUE.
         iarg = iarg + 1
      ELSEIF( cdbrief == '-q' ) THEN
         lqcflags = .TRUE.
         nqc=1
         iarg = iarg + 1
      ELSEIF( cdbrief == '-Q' ) THEN
         lqcflags = .TRUE.
         nqc=2
         iarg = iarg + 1
      ELSEIF( cdbrief == '-B' ) THEN
         lqcflags = .TRUE.
         nqc=3
         iarg = iarg + 1
      ELSEIF( cdbrief == '-u' ) THEN
         lsort = .FALSE.
         iarg = iarg + 1
      ELSEIF ( cdbrief == '-s' ) THEN
         lstat = .TRUE.
         CALL getarg( iarg + 1, cdstat )
         iarg = iarg + 2 
      ELSEIF ( cdbrief == '-t' ) THEN
         ltyp = .TRUE.
         CALL getarg( iarg + 1, cdtyp )
         iarg = iarg + 2 
      ELSEIF ( cdbrief == '-v' ) THEN
         CALL getarg( iarg + 1, cdbrief )
         ip=INDEX(cdbrief,'-')
         IF (ip==0) THEN
            READ(cdbrief,'(I10)') nvar1
            IF (nvar1==0) THEN
               nvar2=-1
            ELSE
               nvar2 = nvar1
            ENDIF
         ELSEIF(ip==1) THEN
            nvar1=1
            READ(cdbrief(ip+1:),'(I10)') nvar2
         ELSE
            READ(cdbrief(1:ip-1),'(I10)') nvar1
            READ(cdbrief(ip+1:),'(I10)') nvar2
         ENDIF
         iarg = iarg + 2
      ELSEIF ( cdbrief == '-a' ) THEN
         CALL getarg( iarg + 1, cdbrief )
         ip=INDEX(cdbrief,'-')
         IF (ip==0) THEN
            READ(cdbrief,'(I10)') nadd1
            IF (nadd1==0) THEN
               nadd2=-1
            ELSE
               nadd2 = nadd1
            ENDIF
         ELSEIF(ip==1) THEN
            nadd1=1
            READ(cdbrief(ip+1:),'(I10)') nadd2
         ELSE
            READ(cdbrief(1:ip-1),'(I10)') nadd1
            READ(cdbrief(ip+1:),'(I10)') nadd2
         ENDIF
         iarg = iarg + 2
      ELSEIF ( cdbrief == '-e' ) THEN
         CALL getarg( iarg + 1, cdbrief )
         ip=INDEX(cdbrief,'-')
         IF (ip==0) THEN
            READ(cdbrief,'(I10)') next1
            IF (next1==0) THEN
               next2=-1
            ELSE
               next2 = next1
            ENDIF
         ELSEIF(ip==1) THEN
            next1=1
            READ(cdbrief(ip+1:),'(I10)') next2
         ELSE
            READ(cdbrief(1:ip-1),'(I10)') next1
            READ(cdbrief(ip+1:),'(I10)') next2
         ENDIF
         iarg = iarg + 2
      ELSEIF ( cdbrief == '-d' ) THEN
         ldaterange=.TRUE.
         iarg = iarg + 1
      ELSEIF ( cdbrief == '-D' ) THEN
         ldepths=.TRUE.
         iarg = iarg + 1
      ELSEIF ( cdbrief == '-z' ) THEN
         lzinp=.TRUE.
         iarg = iarg + 1
      ELSE
         CALL usage
      ENDIF
   ENDDO
   CALL getarg( nargs, cdinfile )
   !
   ! Get input data
   !
   IF (lzinp) THEN
#if defined NOSYSTEM
      WRITE(*,*)'Compressed files need the system subroutine call'
      CALL abort
#else
      CALL system( 'cp '//TRIM(cdinfile)//' fbprint_tmp.nc.gz' )
      CALL system( 'gzip -df fbprint_tmp.nc.gz' )
      CALL read_obfbdata( 'fbprint_tmp.nc', obsdata )
      CALL system( 'rm -f fbprint_tmp.nc' )
#endif
   ELSE
      CALL read_obfbdata( TRIM(cdinfile), obsdata )
   ENDIF
   CALL sealsfromargo( obsdata )
   WRITE(*,'(2A,I9,A,I9,A)')TRIM(cdinfile), ' has ', obsdata%nobs ,&
      & ' observations and a maximum of ', obsdata%nlev, ' levels'
   IF (nvar1<0) THEN
      nvar1 = 1
      nvar2 = obsdata%nvar
   ENDIF
   IF (nadd1<0) THEN
      nadd1 = 1
      nadd2 = obsdata%nadd
   ENDIF
   IF (next1<0) THEN
      next1 = 1
      next2 = obsdata%next
   ENDIF
   !
   ! Sort the data
   !   
   ALLOCATE(zsort(5,obsdata%nobs),iindex(obsdata%nobs))
   IF (lsort) THEN
      DO ii=1,obsdata%nobs
         zsort(1,ii)=obsdata%ptim(ii)
         zsort(2,ii)=obsdata%pphi(ii)
         zsort(3,ii)=obsdata%plam(ii)
         iwmo = TRANSFER( obsdata%cdwmo(ii)(1:4), iwmo )
         zsort(4,ii) = iwmo
         iwmo = TRANSFER( obsdata%cdwmo(ii)(5:8), iwmo )
         zsort(5,ii) = iwmo
      ENDDO
      CALL index_sort_dp_n(zsort,5,iindex,obsdata%nobs)
   ELSE
      DO ii=1,obsdata%nobs
         iindex(ii)=ii
      ENDDO
   ENDIF
   IF (ldaterange) THEN
      IF (obsdata%nobs>0) THEN
         WRITE(*,'(A)')'First observation'
         CALL print_time(obsdata%ptim(1))
         WRITE(*,'(A)')'Last observation'
         CALL print_time(obsdata%ptim(obsdata%nobs))
      ENDIF
   ELSE
      !
      ! Print the sorted list
      !   
      DO ii=1,obsdata%nobs
         IF (lstat) THEN
            IF (TRIM(ADJUSTL(cdstat)) /= &
               &TRIM(ADJUSTL(obsdata%cdwmo(iindex(ii))))) CYCLE
         ENDIF
         IF (ltyp) THEN
            IF (TRIM(ADJUSTL(cdtyp)) /= &
               &TRIM(ADJUSTL(obsdata%cdtyp(iindex(ii))))) CYCLE
         ENDIF
         IF (ldepths) THEN
            CALL print_depths(obsdata,iindex(ii))
         ELSE
            IF (lqcflags) THEN
               CALL print_obs_qc(obsdata,iindex(ii),nqc,nvar1,nvar2)
            ELSE
               CALL print_obs(obsdata,iindex(ii),lbrief,&
                  &           nvar1,nvar2,nadd1,nadd2,next1,next2)
            ENDIF
         ENDIF
      ENDDO

   ENDIF

CONTAINS

   SUBROUTINE usage
      WRITE(*,'(A)')'Usage:'
      WRITE(*,'(A)')'fbprint [options] inputfile'
      CALL abort()
   END SUBROUTINE usage
   
   SUBROUTINE print_depths(obsdata,iindex)
      IMPLICIT NONE
      TYPE(obfbdata) :: obsdata
      INTEGER :: iindex
      INTEGER :: kj
      REAL :: mindep,maxdep

      mindep=10000
      maxdep=0
      DO kj=1,obsdata%nlev
         IF (obsdata%pdep(kj,iindex)<99999.0) THEN
            IF (obsdata%pdep(kj,iindex)>maxdep) maxdep=obsdata%pdep(kj,iindex)
            IF (obsdata%pdep(kj,iindex)<mindep) mindep=obsdata%pdep(kj,iindex)
         ENDIF
      ENDDO
      
      WRITE(*,*)'Fileindex           = ',obsdata%kindex(iindex)
      WRITE(*,*)'Station identifier  = ',obsdata%cdwmo(iindex)
      WRITE(*,*)'Station type        = ',obsdata%cdtyp(iindex)
      WRITE(*,*)'Latitude            = ',obsdata%pphi(iindex)
      WRITE(*,*)'Longtude            = ',obsdata%plam(iindex)
      CALL print_time( obsdata%ptim(iindex) )
      WRITE(*,*)'Position QC         = ',obsdata%ipqc(iindex)
      WRITE(*,*)'Observation QC      = ',obsdata%ioqc(iindex)
      WRITE(*,*)'Minimum obs. depth  = ',mindep
      WRITE(*,*)'Maximum obs. depth  = ',maxdep
      WRITE(*,*)

   END SUBROUTINE print_depths

   SUBROUTINE print_obs(obsdata,iindex,lshort,&
      &                 kvar1,kvar2,kadd1,kadd2,kext1,kext2)
      IMPLICIT NONE
      TYPE(obfbdata) :: obsdata
      INTEGER :: iindex
      LOGICAL :: lshort
      INTEGER :: kvar1,kvar2,kadd1,kadd2,kext1,kext2
      INTEGER :: jv,ja,je,jk
      INTEGER :: kj
      LOGICAL :: lskip
      CHARACTER(len=1024) :: cdfmt1,cdfmt2
      CHARACTER(len=16) :: cdtmp

      WRITE(*,*)'Fileindex           = ',obsdata%kindex(iindex)
      WRITE(*,*)'Station identifier  = ',obsdata%cdwmo(iindex)
      WRITE(*,*)'Station type        = ',obsdata%cdtyp(iindex)
      WRITE(*,*)'Latitude            = ',obsdata%pphi(iindex)
      WRITE(*,*)'Longtude            = ',obsdata%plam(iindex)
      CALL print_time( obsdata%ptim(iindex) )
      WRITE(*,*)'Position QC         = ',obsdata%ipqc(iindex)
      WRITE(*,*)'Observation QC      = ',obsdata%ioqc(iindex)
      IF (.NOT.lshort) THEN
         DO jv = kvar1, kvar2
            WRITE(*,*)'Variable name       = ',obsdata%cname(jv)
            WRITE(*,*)'Variable QC         = ',obsdata%ivqc(iindex,jv)
            IF (obsdata%lgrid) THEN
               WRITE(*,*)'Grid I              = ',obsdata%iobsi(iindex,jv)
               WRITE(*,*)'Grid J              = ',obsdata%iobsj(iindex,jv)
            ENDIF
         ENDDO
         cdfmt1='(1X,A8,1X,A8'
         cdfmt2='(1X,F8.2,1X,I8'
         DO jv = kvar1, kvar2
            cdfmt1 = TRIM(cdfmt1)//',1X,A15,1X,A8'
            cdfmt2 = TRIM(cdfmt2)//',1X,E15.9,1X,I8'
            IF (kadd2-kadd1+1>0) THEN
               WRITE(cdtmp,'(I10)')kadd2-kadd1+1
               cdfmt1 = TRIM(cdfmt1)//',1X,'//TRIM(ADJUSTL(cdtmp))//'(1X,A15)'
               cdfmt2 = TRIM(cdfmt2)//',1X,'//TRIM(ADJUSTL(cdtmp))//'(1X,E15.9)'
            ENDIF
            IF (obsdata%lgrid) THEN
               cdfmt1 = TRIM(cdfmt1)//',1X,A10'
               cdfmt2 = TRIM(cdfmt2)//',1X,I10'
            ENDIF
         ENDDO
         IF (kext2-kext1+1>0) THEN
            WRITE(cdtmp,'(I10)')kext2-kext1+1
            cdfmt1 = TRIM(cdfmt1)//',1X,'//TRIM(ADJUSTL(cdtmp))//'(1X,A15)'
            cdfmt2 = TRIM(cdfmt2)//',1X,'//TRIM(ADJUSTL(cdtmp))//'(1X,E15.9)'
         ENDIF
         cdfmt1=TRIM(cdfmt1)//')'
         cdfmt2=TRIM(cdfmt2)//')'
         IF (obsdata%lgrid) THEN
            WRITE(*,FMT=cdfmt1)&
               & 'DEPTH', 'DEP_QC', &
               & (TRIM(obsdata%cname(jv))//'_OBS', &
               & TRIM(obsdata%cname(jv))//'_QC' , &
               & (TRIM(obsdata%cname(jv))//'_'//TRIM(obsdata%caddname(ja)),&
               & ja = kadd1, kadd2 ), &
               & TRIM(obsdata%cname(jv))//'_K' , &
               & jv = kvar1, kvar2 ), &
               & ( TRIM(obsdata%cextname(ja)),&
               & ja = kext1, kext2 )
            DO kj=1,obsdata%nlev
               IF (obsdata%pdep(kj,iindex)<99999.0) THEN
                  WRITE (*,FMT=cdfmt2) &
                     & obsdata%pdep(kj,iindex),   &
                     & obsdata%idqc(kj,iindex),   &
                     & ( obsdata%pob(kj,iindex,jv), obsdata%ivlqc(kj,iindex,jv), &
                     & ( obsdata%padd(kj,iindex,ja,jv) , ja = kadd1, kadd2 ), &
                     & obsdata%iobsk(kj,iindex,jv), &
                     & jv = kvar1, kvar2 ), &
                     & ( obsdata%pext(kj,iindex,ja), ja = kext1, kext2 )
               ENDIF
            ENDDO
         ELSE
            cdfmt1=TRIM(cdfmt1)//')'
            cdfmt2=TRIM(cdfmt2)//')'
            WRITE(*,FMT=cdfmt1)&
               & 'DEPTH', 'DEP_QC', &
               & (TRIM(obsdata%cname(jv))//'_OBS', &
               & TRIM(obsdata%cname(jv))//'_QC' , &
               & (TRIM(obsdata%cname(jv))//TRIM(obsdata%caddname(ja)),&
               & ja = kadd1, kadd2 ), &
               & jv = kvar1, kvar2 ), &
               & ( TRIM(obsdata%cextname(ja)),&
               & ja = kext1, kext2 )
            DO kj=1,obsdata%nlev
               IF (obsdata%pdep(kj,iindex)<99999.0) THEN
                  WRITE (*,FMT=cdfmt2) &
                     & obsdata%pdep(kj,iindex),   &
                     & obsdata%idqc(kj,iindex),   &
                     & ( obsdata%pob(kj,iindex,jv), obsdata%ivlqc(kj,iindex,jv), &
                     & ( obsdata%padd(kj,iindex,ja,jv) , ja = kadd1, kadd2 ), &
                     & jv = kvar1, kvar2 ), &
                     & ( obsdata%pext(kj,iindex,ja), ja = kext1, kext2 )
               ENDIF
            ENDDO
         ENDIF
      ENDIF
      WRITE(*,*)
   END SUBROUTINE print_obs
   
   SUBROUTINE print_obs_qc(obsdata,iindex,kqc,kvar1,kvar2)
      IMPLICIT NONE
      TYPE(obfbdata) :: obsdata
      INTEGER :: iindex
      LOGICAL :: lqc
      INTEGER :: kqc
      INTEGER :: kvar1,kvar2
      INTEGER :: jv,ja,je,jk
      INTEGER :: kj
      LOGICAL :: lskip
      CHARACTER(len=1024) :: cdfmt1,cdfmt2
      CHARACTER(len=16) :: cdtmp
      INTEGER :: iqcf
      
      IF (kqc==2) THEN
         lskip=.TRUE.
         IF (obsdata%ipqc(iindex)>1) lskip=.FALSE.
         IF (obsdata%ioqc(iindex)>1) lskip=.FALSE.
         DO jv = kvar1, kvar2
            IF (obsdata%ivqc(iindex,jv)>1) lskip=.FALSE.
         ENDDO
         DO kj=1,obsdata%nlev
            IF (obsdata%pdep(kj,iindex)<99999.0) THEN
               IF (obsdata%idqc(kj,iindex)>1) lskip=.FALSE.
               DO jv = kvar1, kvar2
                  IF (obsdata%ivlqc(kj,iindex,jv)>1) lskip=.FALSE.
               ENDDO
            ENDIF
         ENDDO
         IF (lskip) RETURN
      ELSEIF (kqc==3) THEN
         lskip=.TRUE.
         DO kj=1,obsdata%nlev
            IF (obsdata%pdep(kj,iindex)<99999.0) THEN
               iqcf=0
               DO jv = kvar1, kvar2
                  IF (obsdata%ivlqc(kj,iindex,jv)>1) iqcf=iqcf+1
                  IF (iqcf==obsdata%nvar) lskip=.FALSE.
               ENDDO
            ENDIF
         ENDDO
         IF (lskip) RETURN
      ENDIF
      WRITE(*,*)'Fileindex           = ',obsdata%kindex(iindex)
      WRITE(*,*)'Station identifier  = ',obsdata%cdwmo(iindex)
      WRITE(*,*)'Station type        = ',obsdata%cdtyp(iindex)
      WRITE(*,*)'Latitude            = ',obsdata%pphi(iindex)
      WRITE(*,*)'Longtude            = ',obsdata%plam(iindex)
      CALL print_time( obsdata%ptim(iindex) )
      WRITE(*,*)'Position QC         = ',obsdata%ipqc(iindex)
      WRITE(*,*)'Position QC flags   = ',obsdata%ipqcf(:,iindex)
      WRITE(*,*)'Observation QC      = ',obsdata%ioqc(iindex)
      WRITE(*,*)'Observation QC flags= ',obsdata%ioqcf(:,iindex)
      DO jv = kvar1, kvar2
         WRITE(*,*)'Variable name       = ',obsdata%cname(jv)
         WRITE(*,*)'Variable QC         = ',obsdata%ivqc(iindex,jv)
         WRITE(*,*)'Variable QC flags   = ',obsdata%ivqcf(:,iindex,jv)
      ENDDO
      cdfmt1='(1X,A8,1X,A8'
      cdfmt2='(1X,F8.2,1X,I8'
      WRITE(cdtmp,'(I10)')obsdata%nqcf
      cdfmt1 = TRIM(cdfmt1)//',1X,A18'
      cdfmt2 = TRIM(cdfmt2)//',1X,'//TRIM(ADJUSTL(cdtmp))//'(I9)'
      DO jv = kvar1, kvar2
         cdfmt1 = TRIM(cdfmt1)//',1X,A15,1X,A8'
         cdfmt2 = TRIM(cdfmt2)//',1X,E15.9,1X,I8'
         WRITE(cdtmp,'(I10)')obsdata%nqcf
         cdfmt1 = TRIM(cdfmt1)//',1X,A18'
         cdfmt2 = TRIM(cdfmt2)//',1X,'//TRIM(ADJUSTL(cdtmp))//'(I9)'
      ENDDO
      IF (obsdata%next>0) THEN
         WRITE(cdtmp,'(I10)')obsdata%next
         cdfmt1 = TRIM(cdfmt1)//',1X,'//TRIM(ADJUSTL(cdtmp))//'(1X,A15)'
         cdfmt2 = TRIM(cdfmt2)//',1X,'//TRIM(ADJUSTL(cdtmp))//'(1X,E15.9)'
      ENDIF
      cdfmt1=TRIM(cdfmt1)//')'
      cdfmt2=TRIM(cdfmt2)//')'
      WRITE(*,FMT=cdfmt1)&
         & 'DEPTH', 'DEP_QC', 'DEP_QC_FLAGS', &
         & (TRIM(obsdata%cname(jv))//'_OBS', &
         & TRIM(obsdata%cname(jv))//'_QC' , &
         & TRIM(obsdata%cname(jv))//'_QC_FLAGS',&
         & jv = kvar1, kvar2 ), &
         & ( TRIM(obsdata%cextname(ja)),&
         & ja = 1, obsdata%next )
      DO kj=1,obsdata%nlev
         IF (kqc>=2)  THEN
            lskip=.TRUE.
            IF (obsdata%idqc(kj,iindex)>1) lskip=.FALSE.
            DO jv = kvar1, kvar2
               IF (obsdata%ivlqc(kj,iindex,jv)>1) lskip=.FALSE.
            ENDDO
            IF (lskip) CYCLE
         ENDIF
         IF (obsdata%pdep(kj,iindex)<99999.0) THEN
            WRITE (*,FMT=cdfmt2) &
               & obsdata%pdep(kj,iindex),   &
               & obsdata%idqc(kj,iindex),   &
               & ( obsdata%idqcf(ja,kj,iindex), ja = 1, obsdata%nqcf ), &
               & ( obsdata%pob(kj,iindex,jv), obsdata%ivlqc(kj,iindex,jv), &
               & ( obsdata%ivlqcf(ja,kj,iindex,jv) , ja=1, obsdata%nqcf ), &
               & jv = kvar1, kvar2 ), &
               & ( obsdata%pext(kj,iindex,ja), ja=1, obsdata%next )
         ENDIF
      ENDDO
      WRITE(*,*)
      
   END SUBROUTINE print_obs_qc

   SUBROUTINE print_time(ptim)
      IMPLICIT NONE
      REAL(fbdp) :: ptim
      INTEGER:: iyr,imon,iday,ihou,imin,isec      
      WRITE(*,*)'Julian date         = ',ptim
      CALL jul2greg(isec,imin,ihou,iday,imon,iyr,ptim)
      WRITE(*,'(1X,A,I4,2I2.2)') &
         &      'Gregorian date      = ',iyr,imon,iday
      WRITE(*,'(1X,A,I2.2,A1,I2.2,A1,I2.2)') &
         &      'Time                = ',ihou,':',imin,':',isec
   END  SUBROUTINE print_time

END PROGRAM fbprint

   
