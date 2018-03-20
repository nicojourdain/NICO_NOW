PROGRAM fbmatchup
   !!---------------------------------------------------------------------
   !!
   !!                     ** PROGRAM fbmatchup **
   !!
   !!  ** Purpose : Find matching obs in feedback files
   !!
   !!  ** Method  : Use of utilities from obs_fbm.
   !!
   !!  ** Action  :
   !! 
   !!   Usage:
   !!     fbmatchup.exe outputfile inputfile1 varname1 inputfile2 varname2 ...
   !!
   !!   Optional: 
   !!     namelist = namfbmatchup.in       to set ldaily820
   !!
   !!   History :
   !!        ! 2010 (K. Mogensen) Initial version
   !!----------------------------------------------------------------------
   USE toolspar_kind
   USE obs_fbm
   USE index_sort
   IMPLICIT NONE
   !
   ! Command line arguments for output file and input file
   !
#ifndef NOIARGCPROTO
   INTEGER,EXTERNAL :: iargc
#endif
   INTEGER :: nargs
   CHARACTER(len=256) :: cdoutfile
   CHARACTER(len=256),ALLOCATABLE :: cdinfile(:)
   CHARACTER(len=ilenname),ALLOCATABLE :: cdnames(:)
   CHARACTER(len=2*ilenname) :: cdtmp
   LOGICAL :: ldaily820
   NAMELIST/namfbmatchup/ldaily820
   !
   ! Input data
   !
   TYPE(obfbdata)         :: obsdatatmp(1)
   TYPE(obfbdata),POINTER :: obsdata(:)
   INTEGER :: ninfiles,ntotobs,nlev,nadd,next
   !
   ! Time sorting arrays
   !
   REAL(KIND=dp),ALLOCATABLE :: zsort(:,:)
   INTEGER,ALLOCATABLE  :: iset(:),inum(:),iindex(:)
   !
   ! Comparison arrays and scalars
   !
   REAL(KIND=fbsp), ALLOCATABLE :: zrtim(:),zrphi(:),zrlam(:)
   INTEGER(KIND=SELECTED_INT_KIND(12)), ALLOCATABLE :: irwmo(:)
   REAL(KIND=fbsp) :: ztim,zphi,zlam
   INTEGER(KIND=SELECTED_INT_KIND(12)) :: iwmo
   LOGICAL, ALLOCATABLE :: ltaken(:)
   !
   ! Output data
   !
   TYPE(obfbdata) :: obsoutdata
   !
   ! Storage for extra to search for unique.
   !
   CHARACTER(len=ilenname), ALLOCATABLE :: cexttmp(:)
   TYPE extout
      LOGICAL, POINTER, DIMENSION(:) :: luse
      INTEGER, POINTER, DIMENSION(:) :: ipos
   END TYPE extout
   TYPE(extout), POINTER, DIMENSION(:) :: pextinf
   !
   ! Loop variables
   !
   INTEGER :: ifi,ia,ip,i1,ii,ij,ik1,ik2,iv,ist,iadd,ie,iext
   LOGICAL :: llfound
   LOGICAL :: lexists,lnotobs
   INTEGER :: ityp
   !
   ! Get number of command line arguments
   !
   nargs = IARGC()
   IF ( ( MOD(nargs,2) /= 1 ) .OR. ( nargs == 0 )  ) THEN
      WRITE(*,'(A)')'Usage:'
      WRITE(*,'(A)')'fbmatchup outputfile inputfile1 varname1 inputfile2 varname2 ...'
      CALL abort()
   ENDIF
   CALL getarg( 1, cdoutfile )
   !
   ! Read namelist if present
   !
   ldaily820=.FALSE.
   INQUIRE(file='namfbmatchup.in',exist=lexists)
   IF (lexists) THEN
      OPEN(10,file='namfbmatchup.in')
      READ(10,namfbmatchup)
      CLOSE(10)
      WRITE(*,namfbmatchup)
   ENDIF
   !
   ! Get input data
   !
   ninfiles = ( nargs -1 ) / 2
   ALLOCATE( obsdata( ninfiles ) )
   ALLOCATE( cdinfile( ninfiles ) )
   ALLOCATE( cdnames( ninfiles ) )
   ip = 1
   DO ifi = 1, ninfiles
      !
      ! Read the unsorted file
      !
      ip = ip + 1
      CALL getarg( ip, cdinfile(ifi) )
      ip = ip + 1
      CALL getarg( ip, cdnames(ifi) )
      CALL init_obfbdata( obsdatatmp(1) )
      CALL read_obfbdata( TRIM(cdinfile(ifi)), obsdatatmp(1) )
      !
      ! Check if we have fewer levels than in the first file
      !
      IF ( ifi > 1 )  THEN
         IF ( obsdatatmp(1)%nlev > obsdata(1)%nlev ) THEN
            WRITE(*,*)'Warning. More levels in file than the first file'
            WRITE(*,*)'Number of levels in current file = ', obsdatatmp(1)%nlev
            WRITE(*,*)'Number of levels in first file   = ', obsdata(1)%nlev
            WRITE(*,*)'Only the number of levels in the first'//&
               &' file will be used'
         ENDIF
      ENDIF
      !
      ! Check if we have fewer observations than in the first file
      !
      IF ( ifi > 1 )  THEN
         IF ( obsdatatmp(1)%nobs > obsdata(1)%nobs ) THEN
            WRITE(*,*)'Warning. More obs in file than the first file'
            WRITE(*,*)'Number of obs in current file = ', obsdatatmp(1)%nobs
            WRITE(*,*)'Number of obs in first file   = ', obsdata(1)%nobs
            WRITE(*,*)'Only the observations in the first'//&
               &' file will be stored'
         ENDIF
      ENDIF
      !
      ! Check that we have the same number of variables
      !
      IF ( ifi > 1 )  THEN
         IF ( obsdatatmp(1)%nvar /= obsdata(1)%nvar ) THEN
            WRITE(*,*)'Error. Different number of variables.'
            WRITE(*,*)'Number of var in current file = ', obsdatatmp(1)%nvar
            WRITE(*,*)'Number of var in first file   = ', obsdata(1)%nvar
            CALL abort
         ENDIF
      ENDIF
      !
      ! Check reference datas
      !
      IF ( ifi > 1 )  THEN
         IF ( obsdatatmp(1)%cdjuldref /= obsdata(1)%cdjuldref ) THEN
            WRITE(*,*)'Different reference dates'
            CALL abort
         ENDIF
      ENDIF
      !
      ! Special fix for daily average MRB data (820) for the first file
      !
      IF (ldaily820.AND.(ifi==1)) THEN
         DO ij = 1,obsdatatmp(1)%nobs
            READ(obsdatatmp(1)%cdtyp(ij),'(I5)')ityp
            IF (ityp==820) THEN
               obsdatatmp(1)%ptim(ij)=INT(obsdatatmp(1)%ptim(ij))+1.0
            ENDIF
         ENDDO
      ENDIF
      !
      ! Construct sorting arrays
      !
      ALLOCATE( zsort(3,obsdatatmp(1)%nobs), iset(obsdatatmp(1)%nobs), &
         & inum(obsdatatmp(1)%nobs), iindex(obsdatatmp(1)%nobs))
      ii = 0
      DO ij = 1,obsdatatmp(1)%nobs
         ii = ii+1
         zsort(1,ii) = obsdatatmp(1)%ptim(ij)
         zsort(2,ii) = obsdatatmp(1)%pphi(ij)
         zsort(3,ii) = obsdatatmp(1)%plam(ij)
         iset(ii) = 1
         inum(ii) = ij
      ENDDO
      !
      ! Get indexes for time sorting.
      !
      CALL index_sort_dp_n(zsort,3,iindex,obsdatatmp(1)%nobs)
      CALL init_obfbdata( obsdata(ifi) )
      CALL alloc_obfbdata( obsdata(ifi), &
         &                 obsdatatmp(1)%nvar, obsdatatmp(1)%nobs, &
         &                 obsdatatmp(1)%nlev, obsdatatmp(1)%nadd, &
         &                 obsdatatmp(1)%next, obsdatatmp(1)%lgrid )
      !
      ! Copy input data into output data
      !
      CALL merge_obfbdata( 1, obsdatatmp, obsdata(ifi), iset, inum, iindex )
      CALL dealloc_obfbdata( obsdatatmp(1) )
      
      WRITE(*,'(2A)')'File = ', TRIM(cdinfile(ifi))
      WRITE(*,'(A,I9,A)')'has', obsdata(ifi)%nobs, ' observations'

      DEALLOCATE( zsort, iset, inum, iindex )

   ENDDO
   !
   ! Prepare output data
   ! 
   CALL init_obfbdata( obsoutdata )
   !
   ! Count number of additional fields
   !
   nadd = 0
   DO ifi = 1, ninfiles
      nadd = nadd + obsdata(ifi)%nadd
   ENDDO
   !
   ! Count number of unique extra fields
   !
   ! First the maximum to construct list
   next = 0
   DO ifi = 1, ninfiles
      next = next + obsdata(ifi)%next
   ENDDO
   ALLOCATE( &
      & cexttmp(next) &
      & )
   ! Setup pextinf structure and search for unique extra fields
   ALLOCATE( &
      & pextinf(ninfiles) &
      & )
   next = 0
   DO ifi = 1, ninfiles
      ALLOCATE( &
         & pextinf(ifi)%luse(obsdata(ifi)%next), &
         & pextinf(ifi)%ipos(obsdata(ifi)%next)  &
         & )
      DO ie = 1, obsdata(ifi)%next
         llfound = .FALSE.
         DO ii = 1, next
            IF ( cexttmp(ii) == obsdata(ifi)%cextname(ie) ) THEN
               llfound = .TRUE.
            ENDIF
         ENDDO
         IF (llfound) THEN
            pextinf(ifi)%luse(ie) = .FALSE.
            pextinf(ifi)%ipos(ie) = -1
         ELSE
            next = next + 1
            pextinf(ifi)%luse(ie) = .TRUE.
            pextinf(ifi)%ipos(ie) = next
            cexttmp(next) = obsdata(ifi)%cextname(ie)
         ENDIF
      ENDDO
   ENDDO
   !
   ! Copy the first input data to output data
   !
   CALL copy_obfbdata( obsdata(1), obsoutdata, &
      &                kadd = nadd, kext = next )
   ALLOCATE( ltaken(obsoutdata%nlev) )
   iadd = 0
   DO ifi = 1, ninfiles
      DO ia = 1, obsdata(ifi)%nadd
         cdtmp = TRIM(obsdata(ifi)%caddname(ia))//TRIM(cdnames(ifi))
         obsoutdata%caddname(iadd+ia) = cdtmp(1:ilenname)
         DO iv = 1, obsdata(ifi)%nvar
            obsoutdata%caddlong(iadd+ia,iv) = obsdata(ifi)%caddlong(ia,iv)
            obsoutdata%caddunit(iadd+ia,iv) = obsdata(ifi)%caddunit(ia,iv)
         ENDDO
      ENDDO
      DO ie = 1, obsdata(ifi)%next
         IF ( pextinf(ifi)%luse(ie) ) THEN
            obsoutdata%cextname(pextinf(ifi)%ipos(ie)) = &
               & obsdata(ifi)%cextname(ie)
            obsoutdata%cextlong(pextinf(ifi)%ipos(ie)) = &
               & obsdata(ifi)%cextlong(ie)
            obsoutdata%cextunit(pextinf(ifi)%ipos(ie)) = &
               & obsdata(ifi)%cextunit(ie)
         ENDIF
      ENDDO
      iadd = iadd + obsdata(ifi)%nadd
   ENDDO
   !
   ! Allocate comparison arrays and file them
   ! 
   IF (ilenwmo>8) THEN
      WRITE(*,*)'Fix fbmatchup to allow string length > 8'
      CALL abort
   ENDIF
   ALLOCATE(zrtim(obsoutdata%nobs),zrphi(obsoutdata%nobs), &
      &     zrlam(obsoutdata%nobs),irwmo(obsoutdata%nobs))
   DO i1 = 1, obsoutdata%nobs
      irwmo(i1) = TRANSFER( obsoutdata%cdwmo(i1), irwmo(i1) )
      zrtim(i1) = REAL( obsoutdata%ptim(i1), fbsp )
      zrphi(i1) = REAL( obsoutdata%pphi(i1), fbsp )
      zrlam(i1) = REAL( obsoutdata%plam(i1), fbsp ) 
   ENDDO
   !
   ! Merge extra data into output data
   !
   iadd = obsdata(1)%nadd
   DO ifi = 2, ninfiles
      ist = 1
      DO ii = 1, obsdata(ifi)%nobs
         IF (MOD(ii,10000)==1) THEN
            WRITE(*,*)'Handling observation no ',ii,' for file no ',ifi
         ENDIF
         llfound = .FALSE.
         iwmo = TRANSFER( obsdata(ifi)%cdwmo(ii), iwmo )
         ztim = REAL( obsdata(ifi)%ptim(ii), fbsp )
         zphi = REAL( obsdata(ifi)%pphi(ii), fbsp )
         zlam = REAL( obsdata(ifi)%plam(ii), fbsp ) 
         ! Check if the the same index is the right one.
         IF ( iwmo == irwmo(ii) ) THEN
            IF ( ztim == zrtim(ii) ) THEN
               IF (  zphi == zrphi(ii) ) THEN
                  IF ( zlam == zrlam(ii) ) THEN
                     llfound = .TRUE.
                     i1 = ii
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         ! Search for position in from previous found position
         ! if not the same index
         IF (.NOT.llfound) THEN
            DO i1 = ist, obsoutdata%nobs
               IF ( iwmo == irwmo(i1) ) THEN
                  IF ( ztim == zrtim(i1) ) THEN
                     IF (  zphi == zrphi(i1) ) THEN
                        IF ( zlam == zrlam(i1) ) THEN
                           llfound = .TRUE.
                           EXIT
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
         ! If not fount try agan from the beginnning
         IF ( .NOT.llfound ) THEN
            DO i1 = 1, obsoutdata%nobs
               IF ( iwmo == irwmo(i1) ) THEN
                  IF ( ztim == zrtim(i1) ) THEN
                     IF (  zphi == zrphi(i1) ) THEN
                        IF ( zlam == zrlam(i1) ) THEN
                           llfound = .TRUE.
                           EXIT
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
         ! If found put the data into the common structure
         IF (llfound) THEN
            obsoutdata%ioqc(i1) = & 
               & MAX( obsoutdata%ioqc(i1), obsdata(ifi)%ioqc(ii) )
            obsoutdata%ipqc(i1) = &
               & MAX( obsoutdata%ipqc(i1), obsdata(ifi)%ipqc(ii) )
            obsoutdata%itqc(i1) = &
               & MAX( obsoutdata%itqc(i1), obsdata(ifi)%itqc(ii) )
            obsoutdata%ivqc(i1,:) = &
               & MAX( obsoutdata%ivqc(i1,:), obsdata(ifi)%ivqc(ii,:) )
            obsoutdata%ioqcf(:,i1)   = IOR( obsdata(ifi)%ioqcf(:,ii), &
               &                            obsoutdata%ioqcf(:,i1) )
            obsoutdata%ipqcf(:,i1)   = IOR( obsdata(ifi)%ipqcf(:,ii), &
               &                            obsoutdata%ipqcf(:,i1) )
            obsoutdata%itqcf(:,i1)   = IOR( obsdata(ifi)%itqcf(:,ii), &
               &                            obsoutdata%itqcf(:,i1) )
            obsoutdata%ivqcf(:,i1,:) = IOR( obsdata(ifi)%ivqcf(:,ii,:), &
               &                            obsoutdata%ivqcf(:,i1,:) )
            llfound = .FALSE.
            ! Search for levels 
            ltaken(:) = .FALSE.
            DO ik1 = 1, obsdata(ifi)%nlev
               levloop : DO ik2 = 1, obsoutdata%nlev
                  IF ( REAL( obsdata(ifi)%pdep(ik1,ii), fbsp ) ==  &
                     & REAL( obsoutdata%pdep(ik2,i1),  fbsp ) ) THEN
                     lnotobs=.TRUE.
                     IF (ltaken(ik2)) CYCLE
                     DO iv = 1, obsdata(ifi)%nvar
                        IF ( REAL( obsdata(ifi)%pob(ik1,ii,iv), fbsp ) == &
                           & REAL( obsoutdata%pob(ik2,i1,iv), fbsp ) ) THEN
                           lnotobs=.FALSE.
                        ENDIF
                     ENDDO
                     IF (lnotobs) CYCLE levloop
                     ltaken(ik2)=.TRUE.
                     DO ia = 1, obsdata(ifi)%nadd
                        obsoutdata%padd(ik2,i1,iadd+ia,:) = &
                           & obsdata(ifi)%padd(ik1,ii,ia,:)
                     ENDDO
                     DO ie = 1, obsdata(ifi)%next
                        IF ( pextinf(ifi)%luse(ie) ) THEN
                           obsoutdata%pext(ik2,i1,pextinf(ifi)%ipos(ie)) = &
                              & obsdata(ifi)%pext(ik1,ii,ie)
                        ENDIF
                     ENDDO
                     obsoutdata%idqc(ik2,i1) = &
                        & MAX( obsoutdata%idqc(ik2,i1), obsdata(ifi)%idqc(ik1,ii) )
                     obsoutdata%ivlqc(ik2,i1,:) = &
                        & MAX( obsoutdata%ivlqc(ik2,i1,:), obsdata(ifi)%ivlqc(ik1,ii,:) )
                     obsoutdata%idqcf(:,ik2,i1)    = &
                        &                IOR( obsdata(ifi)%idqcf(:,ik1,ii), &
                        &                     obsoutdata%idqcf(:,ik2,i1) )
                     obsoutdata%ivlqcf(:,ik2,i1,:) = &
                        &                IOR( obsdata(ifi)%ivlqcf(:,ik1,ii,:), &
                        &                     obsoutdata%ivlqcf(:,ik2,i1,:) )
                     llfound = .TRUE.
                     EXIT
                  ENDIF
               ENDDO levloop
               ! Write warning if level not found
               IF (.NOT.llfound.AND.(obsdata(ifi)%pdep(ik1,ii)/=fbrmdi)) THEN
                  WRITE(*,*)'Level not found in first file : ',&
                     &      TRIM( cdinfile(1)  )
                  WRITE(*,*)'Data file                     : ',&
                     &      TRIM( cdinfile(ifi) )
                  WRITE(*,*)'Identifier                    : ',&
                     &      obsdata(ifi)%cdwmo(ii)
                  WRITE(*,*)'Julifin date                   : ',&
                     &      obsdata(ifi)%ptim(ii)
                  WRITE(*,*)'Latitude                      : ',&
                     &      obsdata(ifi)%pphi(ii)
                  WRITE(*,*)'Longitude                     : ',&
                     &      obsdata(ifi)%plam(ii)
                  WRITE(*,*)'Depth                         : ',&
                     &      obsdata(ifi)%pdep(ik1,ii)
               ENDIF
            ENDDO
            ist = i1
         ELSE
            ! Write warning if observation not found
            WRITE(*,*)'Observation not found in first data file : ',&
               &      TRIM( cdinfile(1)  )
            WRITE(*,*)'Data file                                : ',&
               &      TRIM( cdinfile(ifi) )
            WRITE(*,*)'Identifier                               : ',&
               &      obsdata(ifi)%cdwmo(ii)
            WRITE(*,*)'Julifin date                              : ',&
               &      obsdata(ifi)%ptim(ii)
            WRITE(*,*)'Latitude                                 : ',&
               &      obsdata(ifi)%pphi(ii)
            WRITE(*,*)'Longitude                                : ',&
               &      obsdata(ifi)%plam(ii)
            ist = 1
         ENDIF
      ENDDO
      IF (obsdata(ifi)%nobs>0) THEN
         WRITE(*,*)'Handled last obs. no    ',ii,' for file no ',ifi
      ENDIF
      iadd = iadd + obsdata(ifi)%nadd
   ENDDO
   !
   ! Write output file
   !
   CALL write_obfbdata( TRIM(cdoutfile), obsoutdata )
   !
   ! Deallocate temporary data
   ! 
   DEALLOCATE(zrtim,zrphi,zrlam,irwmo )
   DEALLOCATE( &
      & cexttmp &
      & )
   DO ifi = 1, ninfiles
      DEALLOCATE( &
         & pextinf(ifi)%luse, &
         & pextinf(ifi)%ipos  &
         & )
   ENDDO
   DEALLOCATE( &
      & pextinf &
      & )

END PROGRAM fbmatchup
