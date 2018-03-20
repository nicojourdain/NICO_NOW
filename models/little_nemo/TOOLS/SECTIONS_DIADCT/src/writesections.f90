MODULE writesections 
   !!=====================================================================
   !!                       ***  MODULE  writesections  ***
   !! 
   !! History: 2011: Clement Bricaud, Mercator-Ocean
   !! 
   !!=====================================================================
   !! * Modules used
   USE declarations   
   USE sections_tools
 
   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC  write_sections 
   PRIVATE file_open 
 
CONTAINS

  SUBROUTINE write_sections
     !!---------------------------------------------------------------------
     !!         ***  ROUTINE read_list_sections ***
     !!
     !! ** Purpose
     !!
     !! ** Method
     !!
     !! ** Input
     !!
     !! ** Action
     !!
     !! History
     !!---------------------------------------------------------------------
     !! * arguments

     !! * Local declarations
     INTEGER             :: jsec ,&!loop on sections 
                            jseg   !loop on segments
     INTEGER             :: i1, i2 !temporary integers
     LOGICAL             :: llok   !
     CHARACTER(len=40)   :: clname !
     TYPE(POINT_SECTION) :: point  !coordinates of a point

     !!---------------------------------------------------------------------

     PRINT*,'              '
     PRINT*,'WRITE SECTIONS'
     PRINT*,'--------------'

     !open output file
     llok=.FALSE.
     clname='section_ijglobal.diadct'
     CALL file_open(numdctout,clname,llok,cdform="UNFORMATTED",cdstatus="REPLACE",cdaction="WRITE")

     !print informations
     IF ( llok ) THEN
          PRINT*,TRIM(clname),' open. '

          DO jsec=1,nb_sec
  
             WRITE(numdctout)jsec
             WRITE(numdctout)secs(jsec)%name
             WRITE(numdctout)secs(jsec)%llstrpond
             WRITE(numdctout)secs(jsec)%ll_ice_section
             WRITE(numdctout)secs(jsec)%ll_date_line
             WRITE(numdctout)secs(jsec)%coordSec
             WRITE(numdctout)secs(jsec)%nb_class
             WRITE(numdctout)secs(jsec)%zsigi
             WRITE(numdctout)secs(jsec)%zsigp
             WRITE(numdctout)secs(jsec)%zsal
             WRITE(numdctout)secs(jsec)%ztem
             WRITE(numdctout)secs(jsec)%zlay
             WRITE(numdctout)secs(jsec)%slopeSection
             WRITE(numdctout)secs(jsec)%nb_point
             IF( secs(jsec)%nb_point .NE. 0 )THEN
                DO jseg=1,secs(jsec)%nb_point
                   i1 = secs(jsec)%listPoint(jseg)%I  ; i2 = secs(jsec)%listPoint(jseg)%J
                   WRITE(numdctout)i1,i2
                ENDDO
                WRITE(numdctout)secs(jsec)%direction(1:secs(jsec)%nb_point)
             ENDIF

          ENDDO !end of loop on sections
         
          CLOSE(numdctout) !Close file
     ENDIF

  END SUBROUTINE  write_sections

END MODULE writesections 
