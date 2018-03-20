MODULE readsections 
   !!=====================================================================
   !!                       ***  MODULE  readsections  ***
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
   PUBLIC read_list_sections 
 
CONTAINS

  SUBROUTINE read_list_sections
     !!---------------------------------------------------------------------
     !!         ***  ROUTINE read_list_sections ***
     !!
     !! ** Purpose: read ascii file 'list_sections.ascii' that contains
     !!             section descriptions
     !!
     !!---------------------------------------------------------------------
     !! * arguments

     !! * Local declarations
     INTEGER             :: jsec !loop on section number
     INTEGER             :: iost,ji
     INTEGER             :: iclass , jclass
     LOGICAL             :: llok,llstrpond,llice,lldate
     REAL(wp)            :: plon1,plat1,plon2,plat2
     REAL(wp)            :: zslope
     CHARACTER(len=5)    :: clclass
     CHARACTER(len=5)    :: cdice
     CHARACTER(len=9)    :: cdstrpond
     CHARACTER(len=110)  :: clname,cdsecname,cltmp
     REAL,DIMENSION(nb_type_class)     :: zclass_value
     TYPE(COORD_SECTION)               :: coord_point1,coord_point2,coordTemp
     TYPE(COORD_SECTION), DIMENSION(2) :: coord_sec
     !!---------------------------------------------------------------------
     PRINT*,'              '
     PRINT*,'READ list_sections'
     PRINT*,'------------------'
     PRINT*,'               '

     nb_sec=0 !initialize number of sections read in list_sections.ascii

     !open and read input file
     clname='list_sections.ascii'
     PRINT*,'               '
     CALL file_open(numdctin,clname,llok,cdform="FORMATTED",cdstatus="OLD",cdaction="READ")

     IF ( llok ) THEN
          PRINT*,'list_sections.ascii open '
           PRINT*,'nb_sec_max = ',nb_sec_max
          PRINT*,'                         '
     
          DO jsec=1,nb_sec_max

                 !read a line corresponding to one section
                 READ(numdctin,'(F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,I2,1X,A9,1X,A5,1X,A40)',iostat=iost) & 
                 &    plon1,plat1,plon2,plat2,iclass,cdstrpond,cdice,cdsecname
                 IF (iost /= 0) EXIT  ! end of file

                 ! cdsecname: change space to underscore for cdsecname
                 cdsecname=ADJUSTL(cdsecname)
                 ji = SCAN(TRIM(cdsecname)," ")
                 DO WHILE(ji .NE. 0)
                    cdsecname(ji:ji) = "_"
                    ji = SCAN(TRIM(cdsecname)," ")
                 ENDDO

                 !computation of heat and salt transport ?
                 llstrpond=.FALSE. ; IF( cdstrpond .EQ. 'okstrpond' ) llstrpond=.TRUE.

                 !computation of ice tranpsort ?
                 llice=.FALSE. ; IF( cdice .EQ. 'okice' ) llice=.TRUE.

                 !store coordinates of the extremities
                 coord_point1=COORD_SECTION(plon1,plat1)
                 coord_point2=COORD_SECTION(plon2,plat2)
                 coord_sec=(/coord_point1,coord_point2/)

                 !Extremities of the section are classed
                 lldate=.FALSE.
                 IF(  coord_sec(2)%lon .LT. coord_sec(1)%lon  .OR.   &
                    ((coord_sec(2)%lon .EQ. coord_sec(1)%lon) .AND.  &
                     (coord_sec(2)%lat .LT. coord_sec(1)%lat)) ) THEN
                      coordTemp   =coord_sec(1)
                      coord_sec(1)=coord_sec(2)
                      coord_sec(2)=coordTemp
                 ENDIF
                 IF((coord_sec(2)%lon - coord_sec(1)%lon) .GT. 180) THEN
                      coordTemp   =coord_sec(1)
                      coord_sec(1)=coord_sec(2)
                      coord_sec(2)=coordTemp
                      lldate=.TRUE.
                 ENDIF

                 !slope of the section (equidistant cylindric projection)
                 zslope=slope_coeff(coord_sec(1),coord_sec(2),lldate)

                 !!initialise global array secs
                 secs(jsec)%llstrpond=.FALSE.  
                 secs(jsec)%ll_date_line=.FALSE. ; secs(jsec)%nb_class=0
                 secs(jsec)%zsigi=99.            ; secs(jsec)%zsigp=99.
                 secs(jsec)%zsal=99.             ; secs(jsec)%ztem=99.
                 secs(jsec)%zlay=99.
                 secs(jsec)%nb_point=0

                 !store all information in global array secs
                 secs(jsec)%name           = cdsecname
                 secs(jsec)%llstrpond      = llstrpond
                 secs(jsec)%ll_ice_section = llice
                 secs(jsec)%coordSec       = (/ coord_sec(1) , coord_sec(2) /)
                 secs(jsec)%slopeSection   = zslope
                 secs(jsec)%ll_date_line   = lldate

                 !debug information
                 CALL write_debug(jsec,'Informations read in ascii file:')
                 CALL write_debug(jsec,'--------------------------------')
                 CALL write_debug(jsec,'section name: '//secs(jsec)%name )
                 IF( secs(jsec)%llstrpond )THEN ; CALL write_debug(jsec,'salt/heat transport computing' )
                 ELSE                           ; CALL write_debug(jsec,'no salt/heat transport computing' )
                 ENDIF
                 IF( secs(jsec)%ll_ice_section )THEN ; CALL write_debug(jsec,'Ice transport computing' )
                 ELSE                                ; CALL write_debug(jsec,'no Ice transport computing' )
                 ENDIF
                 WRITE(cltmp,'(A20,2f8.3)')'Extremity 1 :',secs(jsec)%coordSec(1)
                 CALL write_debug(jsec,cltmp)
                 WRITE(cltmp,'(A20,2f8.3)')'Extremity 2 :',secs(jsec)%coordSec(2)
                 CALL write_debug(jsec,cltmp)
                 WRITE(cltmp,'(A20,f8.3)')'Slope coefficient :',secs(jsec)%slopeSection
                 CALL write_debug(jsec,cltmp)
                 WRITE(cltmp,'(A20,i3.3)')'number of classes : ',iclass
                 CALL write_debug(jsec,cltmp)
                 IF( secs(jsec)%ll_date_line ) THEN ;  CALL write_debug(jsec,'section crosses date line')
                 ELSE                                ; CALL write_debug(jsec,'section don t crosse date line')
                 ENDIF
                 CALL write_debug(jsec,'                        ')

                 !verify number of sections and store it
                 IF ( iclass .GT. nb_class_max) THEN
                     PRINT*,"WARNING:  nb_class_max needs to be greater than ", iclass ; STOP
                 ENDIF
                 secs(jsec)%nb_class=iclass

                 !read classes
                 IF ( iclass .NE. 0 )THEN

                      !classname=zsigi/zsigp/zsal/ztem/zlay
                      READ(numdctin,'(A5)')clclass
                      DO jclass = 1,iclass
                         READ(numdctin,'(F9.3)',iostat=iost) zclass_value(jclass) 
                      ENDDO 
                      IF      ( TRIM(clclass) .EQ. 'zsigi' )THEN
                           secs(jsec)%zsigi(1:iclass)=zclass_value(1:iclass) 
                      ELSE IF ( TRIM(clclass) .EQ. 'zsigp' )THEN
                           secs(jsec)%zsigp(1:iclass)=zclass_value(1:iclass) 
                      ELSE IF ( TRIM(clclass) .EQ. 'zsal'  )THEN
                           secs(jsec)%zsal(1:iclass)=zclass_value(1:iclass) 
                      ELSE IF ( TRIM(clclass) .EQ. 'ztem'  )THEN
                           secs(jsec)%ztem(1:iclass)=zclass_value(1:iclass)
                      ELSE IF ( TRIM(clclass) .EQ. 'zlay'  )THEN
                           secs(jsec)%zlay(1:iclass)=zclass_value(1:iclass)
                      ELSE
                          PRINT*,'Wrong name of class for section/clclass: ', cdsecname,TRIM(clclass)
                      ENDIF

                      IF ( jsec==nsecdebug .OR. nsecdebug==-1)THEN
                           PRINT*,'class type = ',clclass
                           PRINT*,'class values = ',zclass_value(1:iclass)
                      ENDIF

                 ENDIF

          ENDDO !end of loop on sections
         
          CLOSE(numdctin) !Close file
          IF( jsec .EQ. nb_sec_max)THEN
              PRINT*,'   '
              PRINT*,' nb_sec_max is less than the number of sections written in list_sections.ascii'
              STOP
          ELSE 
             nb_sec=jsec-1 !number of read sections
             PRINT*,'   '
             PRINT*,'Number of sections read in list_sections.ascii: ',nb_sec 
             PRINT*,'Reading of list_sections.ascii ok'
             PRINT*,'   '
          ENDIF

     ENDIF

  END SUBROUTINE  read_list_sections

END MODULE readsections 
