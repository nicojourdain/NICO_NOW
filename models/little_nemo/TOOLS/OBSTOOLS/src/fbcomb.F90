PROGRAM fbcomb
   !!---------------------------------------------------------------------
   !!
   !!                     ** PROGRAM fbcomb **
   !!
   !!  ** Purpose : Combine MPI decomposed feedback files into one file
   !!
   !!  ** Method  : Use of utilities from obs_fbm.
   !!
   !!  ** Action  : 
   !!
   !!   Usage:
   !!     fbcomb.exe outputfile inputfile1 inputfile2 ...
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
   !
   ! Input data
   !
   TYPE(obfbdata),POINTER :: obsdata(:)
   INTEGER :: ninfiles,ntotobs,nlev
   !
   ! Time sorting arrays
   !
   REAL(KIND=dp),ALLOCATABLE :: zsort(:,:)
   INTEGER,ALLOCATABLE  :: iset(:),inum(:),iindex(:)
   INTEGER :: iwmo
   !
   ! Output data
   !
   TYPE(obfbdata) :: obsoutdata
   !
   ! Loop variables
   !
   INTEGER :: ia,iv,ii,ij
   !
   ! Get number of command line arguments
   !
   nargs = IARGC()
   IF ( nargs < 2 ) THEN
      WRITE(*,'(A)')'Usage:'
      WRITE(*,'(A)')'fbcomb outputfile inputfile1 inputfile2 ...'
      CALL abort()
   ENDIF
   CALL getarg( 1, cdoutfile )
   !
   ! Get input data
   !
   ALLOCATE( obsdata( nargs - 1 ) )
   ALLOCATE( cdinfile( nargs - 1 ) )
   ntotobs = 0
   ninfiles = nargs - 1
   DO ia=1, ninfiles
      CALL getarg( ia+1, cdinfile(ia) )
      CALL init_obfbdata( obsdata(ia) )
      CALL read_obfbdata( TRIM(cdinfile(ia)), obsdata(ia) )
      WRITE(*,'(2A)')'File = ', TRIM(cdinfile(ia))
      WRITE(*,'(A,I9,A)')'has', obsdata(ia)%nobs, ' observations'
      ntotobs = ntotobs + obsdata(ia)%nobs
   ENDDO
   WRITE(*,'(A,I8)') 'Total obsfiles : ',ntotobs
   !
   ! Check that the data is confirming
   !
   DO ia=2, ninfiles
      IF ( obsdata(ia)%cdjuldref /= obsdata(1)%cdjuldref ) THEN
         WRITE(*,*)'Different julian date reference. Aborting'
         CALL abort
      ENDIF
      IF ( obsdata(ia)%nvar /= obsdata(1)%nvar ) THEN
         WRITE(*,*)'Different number of variables. Aborting'
         CALL abort
      ENDIF
      IF  (obsdata(ia)%nadd /= obsdata(1)%nadd ) THEN
         WRITE(*,*)'Different number of additional entries. Aborting'
         CALL abort
      ENDIF
      IF ( obsdata(ia)%next /= obsdata(1)%next ) THEN
         WRITE(*,*)'Different number of additional variables. Aborting'
         CALL abort
      ENDIF
      IF ( obsdata(ia)%lgrid .NEQV. obsdata(1)%lgrid ) THEN
         WRITE(*,*)'Inconsistent grid search info. Aborting'
         CALL abort
      ENDIF
      DO iv=1, obsdata(ia)%nvar
         IF ( obsdata(ia)%cname(iv) /= obsdata(1)%cname(iv) ) THEN
            WRITE(*,*)'Variable name ', TRIM(obsdata(ia)%cname(iv)), &
               &      ' is different from ', TRIM(obsdata(1)%cname(iv)), &
               &      '. Aborting'
            CALL abort
         ENDIF
         IF ( obsdata(1)%lgrid ) THEN
            IF ( obsdata(ia)%cgrid(iv) /= obsdata(1)%cgrid(iv) ) THEN
               IF (obsdata(1)%nobs==0) THEN
                  obsdata(1)%cgrid(iv) = obsdata(ia)%cgrid(iv)
               ELSE
                  IF (obsdata(ia)%nobs>0) THEN
                     WRITE(*,*)'Grid name ', TRIM(obsdata(ia)%cgrid(iv)), &
                        &      ' is different from ', &
                        &      TRIM(obsdata(1)%cgrid(iv)), '. Aborting'
                     CALL abort
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      DO iv=1,obsdata(ia)%nadd
         IF ( obsdata(ia)%caddname(iv) /= obsdata(1)%caddname(iv) ) THEN
            WRITE(*,*)'Additional name ', TRIM(obsdata(ia)%caddname(iv)), &
               &      ' is different from ', TRIM(obsdata(1)%caddname(iv)), &
               &      '. Aborting'
            CALL abort
         ENDIF
      ENDDO
      DO iv=1,obsdata(ia)%next
         IF ( obsdata(ia)%cextname(iv) /= obsdata(1)%cextname(iv) ) THEN
            WRITE(*,*)'Extra name ', TRIM(obsdata(ia)%cextname(iv)), &
               &      ' is different from ', TRIM(obsdata(1)%cextname(iv)), &
               &      '. Aborting'
            CALL abort
         ENDIF
      ENDDO
   ENDDO
   !
   ! Construct sorting arrays
   !
   ALLOCATE( zsort(5,ntotobs), iset(ntotobs), &
      & inum(ntotobs), iindex(ntotobs))
   ii = 0
   DO ia = 1,ninfiles
      DO ij = 1,obsdata(ia)%nobs
         ii = ii+1
         zsort(1,ii) = obsdata(ia)%ptim(ij)
         zsort(2,ii) = obsdata(ia)%pphi(ij)
         zsort(3,ii) = obsdata(ia)%plam(ij)
         iwmo = TRANSFER( obsdata(ia)%cdwmo(ij)(1:4), iwmo )
         zsort(4,ii) = iwmo
         iwmo = TRANSFER( obsdata(ia)%cdwmo(ij)(5:8), iwmo )
         zsort(5,ii) = iwmo
         iset(ii) = ia
         inum(ii) = ij
      ENDDO
   ENDDO
   !
   ! Get indexes for time sorting.
   !
   CALL index_sort_dp_n(zsort,5,iindex,ntotobs)
   !
   ! Allocate output data
   !   
   nlev = -1
   DO ia = 1,ninfiles
      IF ( obsdata(ia)%nlev > nlev ) nlev = obsdata(ia)%nlev
   ENDDO
   CALL init_obfbdata( obsoutdata )
   CALL alloc_obfbdata( obsoutdata, obsdata(1)%nvar, ntotobs, nlev, &
      &                 obsdata(1)%nadd, obsdata(1)%next, obsdata(1)%lgrid )
   !
   ! Copy input data into output data
   !
   CALL merge_obfbdata( ninfiles, obsdata, obsoutdata, iset, inum, iindex )
   !
   ! Save output data 
   !
   CALL write_obfbdata ( TRIM(cdoutfile), obsoutdata )
   
END PROGRAM fbcomb
