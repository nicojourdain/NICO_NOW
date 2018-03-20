PROGRAM sla2fb
   !!---------------------------------------------------------------------
   !!
   !!                     ** PROGRAM sla2fb **
   !!
   !!  ** Purpose : Convert AVISO SLA format to feedback format
   !!
   !!  ** Method  : Use of utilities from obs_fbm.
   !!
   !!  ** Action  :
   !!
   !!   Usage:
   !!     sla2fb.exe [-s type] outputfile inputfile1 inputfile2 ...
   !!   Option:
   !!     -s            Select altimeter data_source
   !!
   !!   History :
   !!        ! 2010 (K. Mogensen) Initial version
   !!----------------------------------------------------------------------
   USE obs_fbm
   USE obs_sla_io
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
   CHARACTER(len=256) :: cdtmp
   CHARACTER(len=5) :: cdsource
   !
   ! Input data
   !
   TYPE(obfbdata), POINTER :: slaf(:)
   INTEGER :: ninfiles,ntotobs
   !
   ! Output data
   !
   TYPE(obfbdata) :: fbdata
   !
   ! Loop variables
   !
   INTEGER :: ip,ia,ji,jk,noff
   !
   ! Get number of command line arguments
   !
   nargs=IARGC()
   IF (nargs < 1) THEN
      WRITE(*,'(A)')'Usage:'
      WRITE(*,'(A)')'sla2fb [-s type] outputfile inputfile1 inputfile2 ...'
      CALL abort()
   ENDIF
   cdsource=''
   !
   ! Get input data
   !
   noff=1
   IF ( nargs > 1 ) THEN
      CALL getarg(1,cdtmp)
      IF (TRIM(cdtmp)=='-s') THEN
         IF ( nargs < 3 ) THEN
            WRITE(*,*)'Missing arguments to -s <datasource>'
            CALL abort
         ENDIF
         CALL getarg(2,cdsource)
         noff=3
      ENDIF
   ENDIF
   CALL getarg(noff,cdoutfile)
   ninfiles = nargs - noff
   ALLOCATE( slaf(MAX(nargs-noff,1)) )
   ALLOCATE( cdinfile(nargs-noff) )
   ntotobs = 0
   DO ia=1,ninfiles
      CALL getarg( ia + noff, cdinfile(ia) )
      WRITE(*,'(2A)')'File = ',TRIM(cdinfile(ia))
      CALL read_avisofile( TRIM(cdinfile(ia)), slaf(ia), 6, .TRUE., .FALSE. )
      WRITE(*,'(A,I9,A)')'has',slaf(ia)%nobs,' observations'
      IF (LEN_TRIM(cdsource)>0) THEN
         DO ji=1,slaf(ia)%nobs
            slaf(ia)%cdwmo(ji)=TRIM(slaf(ia)%cdwmo(ji))//'_'//TRIM(cdsource)
         ENDDO
      ENDIF
      ntotobs = ntotobs + slaf(ia)%nobs
   ENDDO
   IF (ninfiles==0) THEN
      CALL init_obfbdata( slaf(1) )
      CALL alloc_obfbdata( slaf(1), 1, 0, 1, 0, 0, .FALSE. )
      slaf(1)%cname(1) = 'SLA'
      slaf(1)%cdjuldref = '19500101000000'
   ENDIF
   WRITE(*,'(A,I8)') 'Total observations : ',ntotobs
   !
   ! Merge and output the data.
   !
   CALL conv_fbmerge( TRIM(cdoutfile), ninfiles, slaf )

END PROGRAM sla2fb
