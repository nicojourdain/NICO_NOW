PROGRAM generate_sections
     !!==============================================================================
     !!                       ***  PROGRAM generate_sections    ***
     !!
     !! create a binary file containig the IJ positions of sections in global 
     !! coordinates for the diagnostic routine diadct.F90 of NEMO
     !!
     !!
     !! History: 09/2011: Clement Bricaud ( Mercator-Ocean )
     !!
     !!==============================================================================
     !! * Modules used
     USE declarations
     USE sections_tools
     USE readcoordmesh 
     USE readsections
     USE compute_sections
     USE writesections    

     IMPLICIT NONE
 
     !! * Module Variables used
     INTEGER            :: iargc, narg
     CHARACTER(LEN=80)  :: cdum
     INTEGER            :: jsec ,&! loop on sections
                           jseg   ! loop on segments (parts of the section)
     CHARACTER(len=40)  :: clname
     LOGICAL            :: llok

     NAMELIST/namdct/nsecdebug
     !!==============================================================================     
 
     PRINT*,'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
     PRINT*,'CREATION OF SECTIONS FOR NEMO diadct.F90 ROUTINE'
     PRINT*,'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
 
     !-------------------!
     !1. Read namelist   !
     !-------------------!
     PRINT*,'              '
     PRINT*,'READ NAMELIST'
     PRINT*,'--------------'

     !!open, read and close namelist
     nsecdebug=0
     clname='namelist'
     CALL file_open(numnam,clname,llok,cdform="FORMATTED",cdstatus="OLD",cdaction="READ")
     IF ( llok ) THEN
          REWIND( numnam )
          READ  ( numnam, namdct )
          PRINT*,'  '
          PRINT*,'read namelist'
          IF( nsecdebug==-1      )THEN ; PRINT*,' Debug all sections'
          ELSE IF ( nsecdebug==0 )THEN ; PRINT*,' No section to debug'
          ELSE IF ( nsecdebug .GE. 1 .AND. nsecdebug .LE. nb_sec_max )THEN
              PRINT*,' Debug section number ',nsecdebug
          ELSE
              PRINT*,'Wrong number for nsecdebug = ',nsecdebug
          ENDIF
     ENDIF
     CLOSE(numnam)
     PRINT*,'read namelist ok'

     !-------------------------------------!
     !2. Read coordinates and meshmask     !
     !-------------------------------------!
     CALL read_coord_mesh
 
     PRINT*,'domain sizes: '
     PRINT*,'jpi    jpj    = ',jpi   ,jpj 
     PRINT*,'domain boundaries: '
     PRINT*,' 1   1   ',glamt(1,1),gphit(1,1)
     PRINT*,' 1   jpj ',glamt(1,jpj),gphit(1,jpj)
     PRINT*,' jpi 1   ',glamt(jpi,1),gphit(jpi,1)
     PRINT*,'jpi jpj  ',glamt(jpi,jpj),gphit(jpi,jpj)



     !----------------------!
     !3. Read list_sections !
     !----------------------!
     num_sec_debug(:)=0     ! Unit numbers for debug files
     CALL read_list_sections

     !----------------------!
     !4.Compute sections    !
     !----------------------!
     DO jsec=1,nb_sec
          !we use compsec to generate the series of grid points making the section
          IF(jsec == nsecdebug .OR. nsecdebug ==-1)THEN
             CALL compsec(jsec,secs(jsec),.true.)
          ELSE
             CALL compsec(jsec,secs(jsec),.false.)
          ENDIF
          IF (jsec == nb_sec)PRINT*,'compute section ok '
     ENDDO

     !--------------------------------!
     !5.Write section_ijglobal.diadct !
     !--------------------------------!
     CALL write_sections

     !----------------------! 
     !END                   !
     !----------------------!
    
     !close debug files
     DO jsec=1,nb_sec
        IF( num_sec_debug(jsec) .NE. 0 )CLOSE(num_sec_debug(jsec))
     ENDDO

     PRINT*,'END END END END END END END END END END END END'

END PROGRAM generate_sections
