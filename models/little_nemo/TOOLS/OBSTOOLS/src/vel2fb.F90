PROGRAM vel2fb
   !!---------------------------------------------------------------------
   !!
   !!                     ** PROGRAM vel2fb **
   !!
   !!  ** Purpose : Convert TAO/PIRATA/RAMA currents to feedback format
   !!
   !!  ** Method  : Use of utilities from obs_fbm.
   !!
   !!  ** Action  :
   !!
   !!   Usage:
   !!     vel2fb.exe outputfile inputfile1 inputfile2 ...
   !! 
   !!   History :
   !!        ! 2010 (K. Mogensen) Initial version
   !!----------------------------------------------------------------------
   USE obs_fbm
   USE obs_vel_io
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
   TYPE(obfbdata), POINTER :: velf(:)
   INTEGER :: ninfiles,ntotvel,nmaxlev
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
      WRITE(*,'(A)')'vel2fb outputfile inputfile1 inputfile2 ...'
      CALL abort()
   ENDIF
   CALL getarg(1,cdoutfile)
   !
   ! Get input data
   !
   ALLOCATE( velf(MAX(nargs-1,1)) )
   ALLOCATE( cdinfile(nargs-1) )
   ntotvel = 0
   ninfiles  = nargs - 1
   DO ia = 1,ninfiles
      CALL getarg( ia + 1, cdinfile(ia) )
      CALL read_taondbc( TRIM(cdinfile(ia)), velf(ia), 6, .TRUE., .FALSE. )
      WRITE(*,'(2A)')'File = ',TRIM(cdinfile(ia))
      WRITE(*,'(A,I9,A)')'has',velf(ia)%nobs,' profiles'
      ntotvel = ntotvel + velf(ia)%nobs
      nmaxlev   = MAX( nmaxlev, velf(ia)%nlev )
   ENDDO
   IF (ninfiles==0) THEN
      CALL init_obfbdata( velf(1) )
      CALL alloc_obfbdata( velf(1), 2, 0, 1, 0, 1, .FALSE. )
      velf(1)%cname(1) = 'UVEL'
      velf(1)%cname(2) = 'VVEL'
      velf(1)%coblong(1) = 'Zonal current'
      velf(1)%coblong(2) = 'Meridional current'
      velf(1)%cobunit(1) = 'Meters per second'
      velf(1)%cobunit(2) = 'Meters per second'
   ENDIF
   WRITE(*,'(A,I8)') 'Total profiles : ',ntotvel
   !
   ! Merge and output the data.
   !
   CALL conv_fbmerge( TRIM(cdoutfile), ninfiles, velf )

END PROGRAM vel2fb
