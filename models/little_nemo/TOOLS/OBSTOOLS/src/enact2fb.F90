PROGRAM enact2fb
   !!---------------------------------------------------------------------
   !!
   !!                     ** PROGRAM corio2fb **
   !!
   !!  ** Purpose : Convert ENACT format profiles to feedback format
   !!
   !!  ** Method  : Use of utilities from obs_fbm.
   !!
   !!  ** Action  :
   !!
   !!   Usage:
   !!     enact2fb.exe outputfile inputfile1 inputfile2 ...
   !!
   !!   History :
   !!        ! 2010 (K. Mogensen) Initial version
   !!----------------------------------------------------------------------
   USE obs_fbm
   USE obs_prof_io
   USE convmerge
   IMPLICIT NONE
   !
   ! Command line arguments for output file and input files
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
   TYPE(obfbdata), POINTER :: enactf(:)
   INTEGER :: ninfiles,ntotenact,nmaxlev
   INTEGER,ALLOCATABLE  :: iset(:),inum(:),iindex(:)
   !
   ! Output data
   !
   TYPE(obfbdata) :: fbdata
   !
   ! Loop variables
   !
   INTEGER :: ia,ii,ij
   !
   ! Get number of command line arguments
   !
   nargs=IARGC()
   IF (nargs < 1) THEN
      WRITE(*,'(A)')'Usage:'
      WRITE(*,'(A)')'enact2fb outputfile inputfile1 inputfile2 ...'
      CALL abort()
   ENDIF
   CALL getarg(1,cdoutfile)
   !
   ! Get input data
   !
   ALLOCATE( enactf(MAX(nargs-1,1)) )
   ALLOCATE( cdinfile(nargs-1) )
   ntotenact = 0
   ninfiles  = nargs - 1
   DO ia = 1,ninfiles
      CALL getarg( ia + 1, cdinfile(ia) )
      CALL read_enactfile( TRIM(cdinfile(ia)), enactf(ia), 6, .TRUE., .FALSE. )
      WRITE(*,'(2A)')'File = ',TRIM(cdinfile(ia))
      WRITE(*,'(A,I9,A)')'has',enactf(ia)%nobs,' profiles'
      ntotenact = ntotenact + enactf(ia)%nobs
      nmaxlev   = MAX( nmaxlev, enactf(ia)%nlev )
   ENDDO
   IF (ninfiles==0) THEN
      CALL init_obfbdata( enactf(1) )
      CALL alloc_obfbdata( enactf(1), 2, 0, 1, 0, 1, .FALSE. )
      enactf(1)%cname(1) = 'POTM'
      enactf(1)%cname(2) = 'PSAL'
      enactf(1)%coblong(1) = 'Potential temperature'
      enactf(1)%coblong(2) = 'Practical salinity'
      enactf(1)%cobunit(1) = 'Degrees Celsius'
      enactf(1)%cobunit(2) = 'PSU'
      enactf(1)%cextname(1) = 'TEMP'
      enactf(1)%cextlong(1) = 'Insitu temperature'
      enactf(1)%cextunit(1) = 'Degrees Celsius'
      enactf(1)%cdjuldref = '19500101000000'
   ENDIF
   WRITE(*,'(A,I8)') 'Total profiles : ',ntotenact
   !
   ! Merge and output the data.
   !
   CALL conv_fbmerge( TRIM(cdoutfile), ninfiles, enactf )

END PROGRAM enact2fb
