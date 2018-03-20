MODULE compute_sections
   !!=====================================================================
   !!                       ***  MODULE  diadct  ***
   !! Ocean diagnostics: Compute the transport through a section
   !!
   !! History: 2011: Clement Bricaud, Mercator-Ocean
   !!
   !!===============================================================
   !! * Modules used
   USE declarations 
   USE sections_tools
 
   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC compsec
   
   !! * Module variables
 
CONTAINS

  SUBROUTINE compsec(jsec,sec,lkdebug)         
     !!---------------------------------------------------------------------
     !!                     ***  ROUTINE compsec  ***
     !!
     !!  ** Purpose : Compute the series of mesh points that represent the section
     !!               defined by its extremities.
     !!
     !!  ** Method  : 
     !!          I.   Find which cells of the mesh the section is crossing
     !!          II.  Classification of the intersections mesh/section
     !!                  -first  classification west to east          
     !!                  -second classification south to north 
     !!          III. Find extremities of section in the mesh
     !!          IV.  Find the series of mesh points that form the section
     !!  ** Input: sec : the section to compute
     !!
     !!  ** Output:
     !!---------------------------------------------------------------------
     !! * Arguments
     INTEGER,INTENT(IN)           :: jsec    ! number of the section
     TYPE(SECTION), INTENT(INOUT) :: sec     ! information about the section
     LOGICAL ,INTENT(IN)          :: lkdebug ! debug or not debug this section

     !! * Local variables
     INTEGER :: &
        ji,jj            ,     & ! dummy loop argument
        jseg             ,     & ! loop on segments that form the section    
        nb_inmesh        ,     & ! number of intersections between the section and the mesh
        nmesh                    ! number of cells in processor domain
     INTEGER :: itest , jtest    ! dummy integer
     REAL(wp)      :: &
        zdistEst   , zdistNorth , zdistWest , zdistSouth ,  &! temporary scalars
        zdistEst2  , zdistNorth2, zdistWest2, zdistSouth2,  &! temporary scalars
        zdistEst3  , zdistNorth3, zdistWest3, zdistSouth3,  &! temporary scalars
        zdistFirst , zdistLast  , zdistref  ,               &! temporary scalars
        zdistante  , zdistante2 , zdistnew  , zdistnew2  ,  &! temporary scalars
        zdeltai    , zdeltaj                                 ! temporary scalars
     LOGICAL :: & 
        ll_overlap_sec_left = .FALSE. , ll_overlap_sec_right = .FALSE. ,&! temporary logical
        ll_date_domain_left = .FALSE. , ll_date_domain_right = .FALSE. ,&! temporary logical
        ll_overlap_sec      = .FALSE. , ll_date_domain       = .FALSE. ,&! temporary logical
        ll_test             = .FALSE.                                    ! temporary logical
     LOGICAL :: lest, lwest, lnorth, lsouth
     LOGICAL :: l_oldmethod
     CHARACTER(len=120) :: cltmp
     TYPE(COORD_SECTION) :: &
        coord_a   , coord_b  , coord_d ,  coord_t ,               &!temporary point (long/lat)
        coordFirst, coordLast, coordTemp                           !        "
     TYPE(COORD_SECTION), DIMENSION(nb_point_max) :: coordSec      !intersections mesh/section    
     TYPE(POINT_SECTION) :: &
        endingPoint, prevPoint , nextPoint,           &!temporary point (I/J)
        SouthPoint , NorthPoint, EstPoint , WestPoint  !        "
     !!---------------------------------------------------------------------
     IF( jsec==1 )THEN
          PRINT*,'                '
          PRINT*,'COMPUTE SECTIONS'
          PRINT*,'----------------'
     ENDIF

     !debug
     CALL write_debug(jsec,'compute section:')
     CALL write_debug(jsec,'================')


     !==================!
     !0. Initializations!
     !==================! 
     
     nmesh       = jpi*jpj          ! number of cells in processor domain
     nb_inmesh   = 0                ! initialize number of intersections between section and the mesh
     zdistEst  =0. ; zdistNorth=0. ; zdistWest=0. ; zdistSouth=0.   ! temporary scalars
     zdistFirst=0. ; zdistLast =0.                                  ! temporary scalars
     zdistante =0. ; zdistante2=0. ; zdistnew=0.  ; zdistnew2=0.    ! temporary scalars
     zdeltai=0.    ; zdeltaj=0. 
     coord_a     = COORD_SECTION( 0., 0. ) ; coord_b    = COORD_SECTION( 0., 0. )
     coord_d     = COORD_SECTION( 0., 0. ) ; coord_t    = COORD_SECTION( 0., 0. ) 
     coordFirst  = COORD_SECTION( 0., 0. ) ; coordLast  = COORD_SECTION( 0., 0. )
     coordTemp   = COORD_SECTION( 0., 0. ) ; coordSec   = COORD_SECTION( 0., 0. )
     endingPoint = POINT_SECTION( -1, -1 ) 
     prevPoint   = POINT_SECTION( -1, -1 ) ; nextPoint   = POINT_SECTION( -1, -1 )
     SouthPoint  = POINT_SECTION( -1, -1 ) ; NorthPoint  = POINT_SECTION( -1, -1 )
     EstPoint    = POINT_SECTION( -1, -1 ) ; WestPoint   = POINT_SECTION( -1, -1 )

     !===========================================================!    
     !I. Find which cells of the mesh the section is  crossing  !
     !===========================================================!

     !loop on the mesh 
     DO jj=2,jpj
        DO ji=2,jpi
           !-----------------------------------------------------------
           !For each cell of the mesh, we find the intersection between 
           !the section and the cell as described below:
           !The cell is defined by 4 points A B C D 
           !and the section by S1 and S2 
           !
           !Section\    ________
           !        \ B|        |
           !         \ |        |
           !          \|one cell|
           !      .....\        |
           !           |\       |
           !           | \      | 
           !          A|__\_____|D
           !       ........\
           !----------------\-------------------------------------------

           !definition of points A,B,D (we don't need C) 
           coord_a = COORD_SECTION( glamf(ji-1,jj-1) ,gphif(ji-1,jj-1) )
           coord_b = COORD_SECTION( glamf(ji-1,jj)   ,gphif(ji-1,jj)   )
           coord_d = COORD_SECTION( glamf(ji,jj-1)   ,gphif(ji,jj-1)   )
 
           !size of the cell 
           zdeltai= glamf(ji-1,jj) - glamf(ji-1,jj-1) !zdeltai=[AB]
           zdeltaj= gphif(ji,jj-1) - gphif(ji-1,jj-1) !zdeltaj=[AD]
           
           !intersection section/[AB]=?
           coordTemp     = intersec(sec,coord_a,coord_b)     !compute intersection 
 
           IF( coordTemp%lon .NE. -9999 )THEN
              IF( nb_inmesh+1 .GT. nb_point_max )THEN
                 PRINT*,"WARNING diadct: nb_point_max needs to be greater than ", nb_inmesh
              ELSE    
                 nb_inmesh=nb_inmesh+1              
                 coordSec(nb_inmesh) = coordTemp    !store the intersection's coordinates

                 !We need to know if the section crosses the overlapping band.

                 !Fist we look if there is an intersection mesh/section 
                 !just on the left of the overlapping band: 
                 IF( coordTemp%lon .GT. glamf(1,1)-5  .AND. coordTemp%lon .LT. glamf(1,1) ) & 
                    & ll_overlap_sec_left  = .TRUE.
                 !And we look if there is an intersection mesh/section 
                 !just on the right of the overlapping band:
                 IF( coordTemp%lon .GT. glamf(jpi,1) .AND. coordTemp%lon .LT. glamf(1,1)+5 ) &
                    & ll_overlap_sec_right = .TRUE.
              ENDIF
           ENDIF
              
           !intersection section/[AD]=?
           coordTemp=intersec(sec,coord_a,coord_d)       !compute intersection
           coordTemp%lon = coordTemp%lon !* zmask(ji,jj)-9999.*(1-zmask(ji,jj))
           coordTemp%lat = coordTemp%lat !* zmask(ji,jj)-9999.*(1-zmask(ji,jj))

           IF( coordTemp%lon .NE. -9999 )THEN
              IF( nb_inmesh+1 .GT. nb_point_max )THEN
                 PRINT*, "WARNING diadct: nb_point_max needs to be greater than ", nb_inmesh
              ELSE  
                 nb_inmesh=nb_inmesh+1
                 coordSec(nb_inmesh)=coordTemp
                   
                 !We need to know if the section crosses the overlapping band:
                 !same test as above 
                 IF( coordTemp%lon .GE. glamf(1,1)-3  .AND. coordTemp%lon .LE. glamf(1,1) ) &
                      & ll_overlap_sec_left  = .TRUE.
                 IF( coordTemp%lon .GE. glamf(jpi,1) .AND. coordTemp%lon .LE. glamf(jpi,1)+3) & 
                      & ll_overlap_sec_right = .TRUE. 
              ENDIF
           ENDIF
         
           !We need to know if the domain crosses the date line:
           !Fist, we search a mesh point that is just one the left of date line:
           IF( glamf(ji-1,jj-1) .GT.  175 .AND. glamf(ji-1,jj-1) .LT.  180 ) &
              & ll_date_domain_left = .TRUE.
           !And we search a mesh point that is just one the right of date line:
           IF( glamf(ji-1,jj-1) .GT. -180 .AND. glamf(ji-1,jj-1) .LT. -175 ) &  
              & ll_date_domain_right = .TRUE.
 
        ENDDO
     ENDDO !End of the loop on the mesh

 
     !Crossing section/overlapping band (we need to know it for later):
     !----------------------------------------------------------------- 
     !If there is one intersection mesh/section just on the left of
     !the overlapping band (ll_overlap_sec_left  = .TRUE.)
     !AND there is one just the right of the overlapping band 
     !(ll_overlap_sec_right  = .TRUE.),
     !so the section crosses the overlapping band.
     ll_overlap_sec = ll_overlap_sec_left .AND. ll_overlap_sec_right 

     !Crossing of the domain and the date line (we need to know it for later): 
     !------------------------------------------------------------------------
     !If there is one point of the domain that is just on the left of the date line
     !(ll_date_domain_left = .TRUE.) AND one point that is just on the right of the
     !date line (ll_date_domain_right = .TRUE. ) 
     !So the domain crosses the date line:
     ll_date_domain = ll_date_domain_left .AND. ll_date_domain_right

     !=====================================================!
     ! II. Classification of the intersections mesh/section!
     !=====================================================!

     !   -first classification  west to east          
     !   -second classification south to north        
     !CAUTION: routine qcksrt doesn't work in the same way if the section
     !and the domain crosse the date line (sec%ll_date_line=T and ll_date_domain=T)

     IF( sec%ll_date_line .AND. ll_date_domain )THEN

        !we add 360° to negative longitudes to have a good classification
        DO jseg=1,nb_inmesh
           IF( coordSec(jseg)%lon .LT. 0 ) coordSec(jseg)%lon=coordSec(jseg)%lon+360.
        ENDDO
        IF( sec%coordSec(1)%lon .NE. sec%coordSec(2)%lon ) THEN
           CALL qcksrt(coordSec(:)%lon,coordSec(:)%lat,nb_inmesh)
        ELSE
           CALL qcksrt(coordSec(:)%lat,coordSec(:)%lon,nb_inmesh)
        ENDIF
        DO jseg=1,nb_inmesh
           IF( coordSec(jseg)%lon .GT. 180 ) coordSec(jseg)%lon=coordSec(jseg)%lon-360.
        ENDDO

     ELSE     

        IF( sec%coordSec(1)%lon .NE. sec%coordSec(2)%lon )THEN
           CALL qcksrt(coordSec(:)%lon,coordSec(:)%lat,nb_inmesh)
        ELSE
           CALL qcksrt(coordSec(:)%lat,coordSec(:)%lon,nb_inmesh)
        ENDIF

     ENDIF

     !debug
     WRITE(cltmp,'(A20,i3.3)')'number intersections = ',nb_inmesh ; CALL write_debug(jsec,cltmp)
     CALL write_debug(jsec,'List of intersections between grid and section: ')
     DO jseg=1,nb_inmesh
        WRITE(cltmp,'(i4.4,1X,2(f8.3,1X) )')jseg,coordSec(jseg) ;  CALL write_debug(jsec,cltmp)
     ENDDO
 
     !=====================================================!
     ! III. Find extremities of section in the mesh        !
     !=====================================================!
     !we can find section's extremities in the mesh only if
     !there is intersection between section and mesh (nb_inmesh .ne. 0)

     IF( nb_inmesh .ne. 0 )THEN
        coordFirst       = coordSec(1)
        coordLast        = coordSec(nb_inmesh) 
        sec%nb_point     = nb_inmesh
        sec%listPoint(1) = POINT_SECTION(-1,-1)
        zdistante        = 1000.
        zdistante2       = 1000.

        !First, we find the point of the mesh that is the closest 
        !to the first intersection section/mesh (=coordFirst=coordSec(1)):
        !this point will be called sec%listPoint(1).
        !Then, we find the point of the mesh that is the closest
        !to the last intersection section/mesh (coordLast=coordSec(nb_inmesh))
        !this point will be called endingPoint.

        DO jj=1,jpj
           DO ji=1,jpi  
              coord_t=COORD_SECTION(glamf(ji,jj),gphif(ji,jj))
              zdistFirst = distance2(coord_t,coordFirst)
              zdistLast = distance2(coord_t,coordLast)
              IF( zdistFirst .LT. zdistante )THEN
                 sec%listPoint(1) = POINT_SECTION(ji,jj)
                 zdistante=zdistFirst
              ENDIF
              IF( zdistLast .LT. zdistante2 )THEN
                 endingPoint = POINT_SECTION(ji,jj)
                 zdistante2=zdistLast
              ENDIF
           ENDDO
        ENDDO

        IF( sec%listPoint(1)%I == endingPoint%I .AND. sec%listPoint(1)%J == endingPoint%J )THEN
           sec%listPoint(1) = POINT_SECTION(-1,-1)
           endingPoint      = POINT_SECTION(-1,-1)
           coordFirst       = coordSec(1)
           coordLast        = coordSec(2)
           sec%nb_point     = 0
        ENDIF

     ELSE
        !If there is no intersection section/mesh
        sec%listPoint(1) = POINT_SECTION(-1,-1)
        endingPoint      = POINT_SECTION(-1,-1)
        coordFirst       = coordSec(1)
        coordLast        = coordSec(2)
        sec%nb_point     = 0
     ENDIF 

     !debug
     CALL write_debug(jsec,"extremities of section in the grid : ")
     ji=sec%listPoint(1)%I ; jj=sec%listPoint(1)%J
     IF( sec%nb_point .ne. 0 )THEN
        ji=sec%listPoint(1)%I ; jj=sec%listPoint(1)%J
        WRITE(cltmp,'(A15,X,i4.4,X,i4.4,X,f8.3,X,f8.3)')'First point: ',sec%listPoint(1),glamf(ji,jj),gphif(ji,jj) 
        CALL write_debug(jsec,cltmp)
        ji=endingPoint%I ; jj=endingPoint%J
        WRITE(cltmp,'(A15,X,i4.4,X,i4.4,X,f8.3,X,f8.3)')'Last  point: ',endingPoint,glamf(ji,jj),gphif(ji,jj)
        CALL write_debug(jsec,cltmp)
        !
        coord_a=pointToCoordF(sec%listPoint(1)) ; coord_b=pointToCoordF(endingPoint)
        ll_test = .FALSE.
        IF ( ll_date_domain .AND. ABS( coord_a%lon - coord_b%lon ).GT. 180) ll_test= .TRUE.
        zdistante=distance2(coord_a,coord_b ,ll_test )
        WRITE(cltmp,'(A20,f10.3)' )'distance between IJ-extremities : ',zdistante
        CALL write_debug(jsec,cltmp)
        !
        CALL write_debug(jsec,"Initial extremities : ") 
        WRITE(cltmp,'( 2(f9.3),A3,2(f9.3) )')coordFirst,'---',coordLast
        CALL write_debug(jsec,cltmp)
        ll_test = .FALSE.
        IF( ll_date_domain .AND. ABS(coordFirst%lon - coordLast%lon).GT. 180)ll_test= .TRUE.
        zdistante=distance2(coordFirst,coordLast,ll_test)
        WRITE(cltmp,'(A30,f10.3)')' distance between initial extremities : ',zdistante
        CALL write_debug(jsec,cltmp)
        CALL write_debug(jsec,"                  ")
     ELSE
        WRITE(cltmp,'(A50)' )"no intersection between section and mesh"
     ENDIF

     !==========================================================!
     ! IV. Find the series of mesh points that form the section !
     !==========================================================!
     CALL write_debug(jsec,"Find the serie of mesh's points that form the section")

     IF( sec%nb_point .ne. 0 )THEN

        !The series of mesh points that form the section will 'link' 
        !sec%listPoint(1) to endingPoint: it will be stored in 
        !sec%listPoint(jseg)
        !
        !We take place on the first point (sec%listPoint(1)) 
        ! a.  We find the 4 adjacent points (North, South, East, West)
        ! b.  Compute distance between current point and endingPoint
        ! c.  Compute distance between the 4 adjacent points and endingPoint
        ! d.  Select the points which are closer to end-point than current point
        ! e.1 If at least one point is selected, select the point which is closest to original section among selected points 
        ! e.2 If no point is selected, select the point which is the closest to end-point 
        ! f. save next point and direction of velocity.
        ! g. Save nextPoint and go to nextPoint
        !
        !We get out of this loop if:
        !    - we are on endingPoint
        !    - the number of points (jseg) that link sec%listPoint(1) to endingPoint is 
        !      twice greater than number of section/mesh intersection (nb_inmesh):
        !      it could be possible if thr algorithm can't link endingPoint (bug).

        !initialize distnew value (with distance between section's extremities) 
        zdistnew  = distance2(coordFirst,coordLast,sec%ll_date_line) 
        prevPoint  = POINT_SECTION(0,0)
        jseg       = 1

        DO WHILE ( (  sec%listPoint(jseg)%I .NE.  endingPoint%I    &
                 .OR. sec%listPoint(jseg)%J .NE. endingPoint%J   ) &
                 .AND. jseg .LT. nb_inmesh + 1 .AND. sec%listPoint(jseg)%I .GT. 0  )         
   
           ! a. find the 4 adjacent points (North, South, East, West)
           !---------------------------------------------------------
           SouthPoint = POINT_SECTION(sec%listPoint(jseg)%I,sec%listPoint(jseg)%J-1)
           NorthPoint = POINT_SECTION(sec%listPoint(jseg)%I,sec%listPoint(jseg)%J+1)
           WestPoint  = POINT_SECTION(sec%listPoint(jseg)%I-1,sec%listPoint(jseg)%J)
           EstPoint   = POINT_SECTION(sec%listPoint(jseg)%I+1,sec%listPoint(jseg)%J)

           !debug
           CALL write_debug(jsec,"---------------")
           WRITE(cltmp,100)'Current points: ',sec%listPoint(jseg), & 
                         glamf(sec%listPoint(jseg)%I,sec%listPoint(jseg)%J), &
                         gphif(sec%listPoint(jseg)%I,sec%listPoint(jseg)%J)
           CALL write_debug(jsec,cltmp)
           CALL write_debug(jsec,"E/W/N/S points")
           WRITE(cltmp,102)EstPoint,WestPoint,NorthPoint,SouthPoint
           CALL write_debug(jsec,cltmp)
           itest=MIN(MAX(EstPoint%I,0),jpi+1) ; jtest=MIN(MAX(EstPoint%J,0),jpj+1)
           IF( itest .NE. 0 .AND. itest .NE. jpi+1 .AND. jtest .NE. 0 .AND. jtest .NE. jpj+1 )THEN 
              WRITE(cltmp,101)'Est',glamf(itest,jtest),gphif(itest,jtest)
              CALL write_debug(jsec,cltmp)
           ELSE
              CALL write_debug(jsec,"Est point out of domain")
           ENDIF
           !
           itest=MIN(MAX(WestPoint%I,0),jpi+1) ; jtest=MIN(MAX(WestPoint%J,0),jpj+1)
           IF( itest .NE. 0 .AND. itest .NE. jpi+1 .AND. jtest .NE. 0 .AND. jtest .NE. jpj+1 )THEN 
              WRITE(cltmp,101)'West',glamf(itest,jtest),gphif(itest,jtest)
              CALL write_debug(jsec,cltmp)
           ELSE
              CALL write_debug(jsec,"West point out of domain")
           ENDIF
           !
           itest=MIN(MAX(NorthPoint%I,0),jpi+1) ; jtest=MIN(MAX(NorthPoint%J,0),jpj+1)
           IF( itest .NE. 0 .AND. itest .NE. jpi+1 .AND. jtest .NE. 0 .AND. jtest .NE. jpj+1 )THEN 
              WRITE(cltmp,101)'North',glamf(itest,jtest),gphif(itest,jtest)
              CALL write_debug(jsec,cltmp)
           ELSE
              CALL write_debug(jsec,"North point out of domain")
           ENDIF
           !
           itest=MIN(MAX(SouthPoint%I,0),jpi+1) ; jtest=MIN(MAX(SouthPoint%J,0),jpj+1)
           IF( itest .NE. 0 .AND. itest .NE. jpi+1 .AND. jtest .NE. 0 .AND. jtest .NE. jpj+1 )THEN 
              WRITE(cltmp,101)'South',glamf(itest,jtest),gphif(itest,jtest)
              CALL write_debug(jsec,cltmp)
           ELSE
              CALL write_debug(jsec,"South point out of domain")
           ENDIF
           !
           !
100 FORMAT ( A15,2(i4.4," "),2(f8.3," ") )
101 FORMAT ( A6,2(f7.3," "))
102 FORMAT ( "E ",i4.4,' ',i4.4,"//W ",i4.4,' ',i4.4,"//N ",i4.4,' ',i4.4,"//S ",i4.4,' ',i4.4 )

               
           !Either we are at an end-point
           !--------------------
           IF(      SouthPoint%I==endingPoint%I .AND. SouthPoint%J==endingPoint%J )THEN 
               jseg = jseg+1 ; sec%listPoint(jseg) = SouthPoint
           ELSE IF( NorthPoint%I==endingPoint%I .AND. NorthPoint%J==endingPoint%J )THEN
               jseg = jseg+1 ; sec%listPoint(jseg) = NorthPoint
           ELSE IF(  WestPoint%I==endingPoint%I .AND.  WestPoint%J==endingPoint%J )THEN
               jseg = jseg+1 ; sec%listPoint(jseg) = WestPoint
           ELSE IF(   EstPoint%I==endingPoint%I .AND.   EstPoint%J==endingPoint%J )THEN
               jseg = jseg+1 ; sec%listPoint(jseg) = EstPoint

           ELSE
           !Else we are NOT on end-point
           !------------------------

              ! b. distance between current point and endingPoint
              !--------------------------------------------------
              zdistante = zdistnew

              ! c. compute distance between the 4 adjacent points and endingPoint
              !------------------------------------------------------------------
              ! BE CAREFUL! When the domain crosses the date line (ll_date_domain):
              !
              ! When we will compute distances between W/E/S/N points and endingPoint,
              ! we have to check if theses 4 lines crosses the date line
              ! (test: ABS(coordTemp%lon - coordLast%lon).GT. 180)
              !
              ! If one of these lines crosses the date line, we have to add 360° 
              ! to the longitudes of the extremities to compute the distance through
              ! the date line and not around the Earth.
       
              ! c.1 compute distWest: distance between west point and endingPoint
              !----------------------
              zdistWest2 = 99999999.9 ;  zdistWest3 = 99999999.9 
              IF( sec%listPoint(jseg)%I .NE. 1 )THEN
                 !When we are on the west side of the mesh (i=1),we can't go to the west.
                 coordTemp = pointToCoordF(WestPoint) 
                 ll_test = .FALSE.
                 IF( ll_date_domain .AND. ABS(coordTemp%lon - coordLast%lon).GT. 180 )ll_test = .TRUE.
                 zdistWest2  = distance2(pointToCoordF(WestPoint) ,coordLast ,ll_test)
              ENDIF

              ! c.2 compute distEst: distance between east point and endingPoint
              !---------------------
              zdistEst2 = 99999999.9 ;  zdistEst3 = 99999999.9
              IF( sec%listPoint(jseg)%I .EQ. jpi )THEN
                 !We test if the current point is on the east side of the mesh
                 ! The method is done such that we go toward east to link
                 ! sec%listPoint(1) to endingPoint.
                 ! So, if the section crosses the overlapping band (ll_overlap_sec=T),
                 ! we won't have to stop if the current point is on the EAST side of the mesh:
                 ! we have to follow the construction of the section on the
                 ! WEST side of the mesh
                 IF( ll_overlap_sec  )THEN
                    !section crosses the overlapping band 
                    !So EastPoint is on the west side of the mesh
                    EstPoint = POINT_SECTION(3,sec%listPoint(jseg)%J)
                    zdistEst2= distance2(pointToCoordF(EstPoint)  ,coordLast ,.FALSE.)
                 ENDIF 
              ELSE
                 coordTemp = pointToCoordF(EstPoint) 
                 ll_test = .FALSE.
                 IF( ll_date_domain .AND. ABS(coordTemp%lon - coordLast%lon).GT. 180 )ll_test= .TRUE.
                 zdistEst2 = distance2(pointToCoordF(EstPoint)  ,coordLast ,ll_test )
              ENDIF

              ! c.3 compute distSouth: distance between south point and endingPoint
              !-----------------------
              zdistSouth2 = 99999999.9 ; zdistSouth3 = 99999999.9
              IF( sec%listPoint(jseg)%J .NE. 1 )THEN
                 !When we are on the south side of the mesh (j=1),we can't go to the south. 
                 coordTemp=pointToCoordF(SouthPoint)
                 ll_test = .FALSE.
                 IF( ll_date_domain .AND. ABS(coordTemp%lon - coordLast%lon).GT. 180 )ll_test= .TRUE.
                    zdistSouth2 = distance2(pointToCoordF(SouthPoint),coordlast ,ll_test )
              ENDIF

              ! c.4 compute distNorth: distance between north and endingPoint
              !-----------------------
              zdistNorth2 = 99999999.9 ; zdistNorth3 = 99999999.9 
              IF( sec%listPoint(jseg)%J .NE. jpj )THEN
                 !When we are on the north side of the mesh (j=jpj),we can't go to the south. 
                 coordTemp=pointToCoordF(NorthPoint)
                 ll_test = .FALSE.
                 IF( ll_date_domain .AND. ABS(coordTemp%lon - coordLast%lon).GT. 180 )ll_test= .TRUE.
                 zdistNorth2 = distance2(pointToCoordF(NorthPoint),coordlast ,ll_test )
              ENDIF

              ! d. select the points which are closer to end-point than current point
              !----------------------------------------------------------------------
              zdistref=distance2(pointToCoordF(sec%listPoint(jseg)),coordlast ,ll_test )
              WRITE(cltmp,'( A56,f10.3 )' )'distance between actual point and last point: zdistref = ',zdistref
              CALL write_debug(jsec,cltmp)
              lest   = .FALSE. ; IF( zdistEst2   .LE. zdistref ) lest  = .TRUE.
              lwest  = .FALSE. ; IF( zdistwest2  .LE. zdistref ) lwest = .TRUE.
              lnorth = .FALSE. ; IF( zdistnorth2 .LE. zdistref ) lnorth= .TRUE.
              lsouth = .FALSE. ; IF( zdistsouth2 .LE. zdistref ) lsouth= .TRUE.

              !debug
              IF( .NOT. lest   )CALL write_debug(jsec,'Est   point eliminated')
              IF( .NOT. lwest  )CALL write_debug(jsec,'West  point eliminated')
              IF( .NOT. lnorth )CALL write_debug(jsec,'North point eliminated')
              IF( .NOT. lsouth )CALL write_debug(jsec,'South point eliminated')

              l_oldmethod = .FALSE.

              IF( ( COUNT((/lest/))+COUNT((/lwest/))+COUNT((/lnorth/))+COUNT((/lsouth/)) ) .NE. 0 )THEN

                 ! e.1 If at least one point is selected, select the point 
                 !     which is the closest to original section among selected points 
                 !-------------------------------------------------------------------

                 zdistWest3  = 9999999.9
                 IF( lwest )zdistWest3  = &
                    distance3(pointToCoordF(sec%listPoint(1)),pointToCoordF(WestPoint) ,pointToCoordF(endingPoint) ,lkdebug )
                 zdistEst3   = 9999999.9
                 IF( lest )zdistEst3   =  &
                    distance3(pointToCoordF(sec%listPoint(1)),pointToCoordF(EstPoint)  ,pointToCoordF(endingPoint) ,lkdebug )
                 zdistSouth3 = 9999999.9
                 IF( lsouth )zdistSouth3 = &
                    distance3(pointToCoordF(sec%listPoint(1)),pointToCoordF(SouthPoint),pointToCoordF(endingPoint) ,lkdebug )
                 zdistNorth3 = 9999999.9
                 IF( lnorth )zdistNorth3 = &
                    distance3(pointToCoordF(sec%listPoint(1)),pointToCoordF(NorthPoint),pointToCoordF(endingPoint) ,lkdebug )

                 zdistEst3   = zdistEst3   + (1-COUNT((/lest/))  )*9999999.9
                 zdistWest3  = zdistWest3  + (1-COUNT((/lwest/)) )*9999999.9
                 zdistNorth3 = zdistNorth3 + (1-COUNT((/lnorth/)))*9999999.9
                 zdistSouth3 = zdistSouth3 + (1-COUNT((/lsouth/)))*9999999.9

                 zdistnew = MIN(zdistEst3,zdistWest3,zdistnorth3,zdistSouth3)

              ELSE 

                 ! e.2 If no point is selected, select the point which is the closest to end-point 
                 !--------------------------------------------------------------------------------
                 l_oldmethod = .TRUE.

                 !debug
                 WRITE(cltmp,'(A30,i3.3)' )'SEARCH NEW POINT WITH OLD METHOD: ',jsec
                 CALL write_debug(jsec,cltmp)
                   
                 !be careful! we can't go backward.
                 zdistNorth = zdistNorth2    ; zdistSouth = zdistSouth2
                 zdistEst   = zdistEst2      ; zdistWest  = zdistWest2 

                 IF(     prevPoint%I .EQ. NorthPoint%I .AND. prevPoint%J .EQ. NorthPoint%J) THEN
                     zdistnew = MIN(zdistEst,zdistWest,zdistSouth)
                 ELSE IF(prevPoint%I .EQ. SouthPoint%I .AND. prevPoint%J .EQ. SouthPoint%J) THEN
                    zdistnew = MIN(zdistEst,zdistWest,zdistNorth)
                 ELSE IF(prevPoint%I .EQ. WestPoint%I  .AND. prevPoint%J .EQ. WestPoint%J ) THEN
                    zdistnew = MIN(zdistEst,zdistNorth,zdistSouth)
                 ELSE IF(prevPoint%I .EQ. EstPoint%I   .AND. prevPoint%J .EQ. EstPoint%J  ) THEN
                    zdistnew = MIN(zdistWest,zdistNorth,zdistSouth)
                 ELSE 
                    zdistnew = MIN(zdistEst,zdistWest,zdistNorth,zdistSouth)
                 ENDIF              

              ENDIF

              !debug
              WRITE(cltmp,'(A11, f8.3)')'zdistref = ',zdistref
              CALL write_debug(jsec,cltmp)
              WRITE(cltmp, 103         )'distance2 :',zdistEst2,zdistWest2,zdistNorth2,zdistSouth2 
              CALL write_debug(jsec,cltmp)
              WRITE(cltmp, 103         )'distance3 :',zdistEst3,zdistWest3,zdistNorth3,zdistSouth3
              CALL write_debug(jsec,cltmp)
              WRITE(cltmp,'(A11, f8.3)')"zdistnew = ",zdistnew
              CALL write_debug(jsec,cltmp)

103 FORMAT (A12,"E",f12.3," W",f12.3," N",f12.3," S",f12.3)

              !f. save next point and direction of velocity.
              !--------------------------------------------- 
              !nextPoint will be the one which is the closest to endingPoint.
              !sec%direction will be direction between current point and nextPoint:
              !It will be used to compute velocity through the segment [CurrentPoint,nextPoint}:
              !                 
              !A:Current Point    NorthPoint(I,J+1)   Nextpoint=NorthPoint(I,J+1) => sec%direction=3
              !                   |                   Nextpoint=SouthPoint(I,J-1) => sec%direction=2   
              !                   |                   Nextpoint=WestPoint(I-1,J)  => sec%direction=0   
              !                   |==>V(I,J+1)        Nextpoint=EastPoint(I+1,J)  => sec%direction=1
              !            U(I,J) |       U(I+1,J)
              !            ^      |       ^
              !   West_____|______A_______|_____EstPoint 
              !   Point           |(I,J)        (I+1,J)  
              !   (I-1,J)         |               
              !                   |==>V(I,J)
              !                   |
              !              SoutPoint(I,J-1)
              IF( l_oldmethod )THEN
                 IF( zdistnew == zdistWest )      THEN
                      sec%direction(jseg)=0 ; nextPoint = WestPoint
                 ELSE IF( zdistnew == zdistEst )  THEN
                      sec%direction(jseg)=1 ; nextPoint = EstPoint
                 ELSE IF( zdistnew == zdistSouth )THEN
                      sec%direction(jseg)=2 ; nextPoint = SouthPoint
                 ELSE IF( zdistnew == zdistNorth )THEN
                      sec%direction(jseg)=3 ; nextPoint= NorthPoint
                 ENDIF
              ELSE 
                 IF( zdistnew == zdistWest3 )      THEN
                      sec%direction(jseg)=0 ; nextPoint = WestPoint
                 ELSE IF( zdistnew == zdistEst3 )  THEN
                      sec%direction(jseg)=1 ; nextPoint = EstPoint
                 ELSE IF( zdistnew == zdistSouth3 )THEN
                      sec%direction(jseg)=2 ; nextPoint = SouthPoint
                 ELSE IF( zdistnew == zdistNorth3 )THEN
                      sec%direction(jseg)=3 ; nextPoint= NorthPoint
                 ENDIF 
              ENDIF

              WRITE(cltmp,'(A11, 2(i4.4,1X) )')'nextPoint = ', nextPoint
              CALL write_debug(jsec,cltmp)
        
              !f. Save nextPoint and go to nextPoint
              !-------------------------------------
              prevPoint = sec%listPoint(jseg)
              jseg = jseg+1                   !increment of number of segments that form the section
              sec%listPoint(jseg) = nextPoint !Save next point
   
           ENDIF ! southP/northP/WestP/EstP == endingpoint ?

        ENDDO                  !End of loop on jseg
        sec%nb_point = jseg    !Save the number of segments that form the section


     ELSE ! isec%nb_point == 0
        DO jseg=1,nb_point_max 
           sec%listPoint(:)=POINT_SECTION(0,0)
        ENDDO
        sec%direction(:)=0.
        sec%nb_point = 0
     ENDIF

     !debug     
     IF( sec%nb_point .ne. 0 )THEN
        CALL write_debug(jsec,"-------------------------------------")
        CALL write_debug(jsec,"list of points in the grid : ")
        DO jseg=1,sec%nb_point 
           ji=sec%listPoint(jseg)%I ; jj=sec%listPoint(jseg)%J
           WRITE(cltmp, '(i4.4,X,i4.4,X,i4.4,X,f8.3,X,f8.3)' )jseg,ji,jj,glamf(ji,jj),gphif(ji,jj)
           CALL write_debug(jsec,cltmp)
        ENDDO
    
        !test if we are an end-point
        IF( sec%listPoint(sec%nb_point)%I .NE. endingPoint%J .AND. sec%listPoint(sec%nb_point)%J .NE. endingPoint%J )THEN   
           PRINT*,TRIM(sec%name)," NOT ARRIVED TO endingPoint FOR jsec =  ",jsec
        ENDIF
     ENDIF

     !now compute new slopeSection with ij-coordinates of first and last point 
     sec%slopeSection = 0  ! default value
     IF( sec%nb_point .ne. 0 )THEN
        IF (  sec%listPoint(sec%nb_point)%I .NE.  sec%listPoint(1)%I ) THEN
           sec%slopeSection = ( sec%listPoint(sec%nb_point)%J - sec%listPoint(1)%J ) /  &
                              ( sec%listPoint(sec%nb_point)%I - sec%listPoint(1)%I )      
        ELSE
           sec%slopeSection = 10000._wp
        ENDIF
     ENDIF
 
  END SUBROUTINE compsec

END MODULE compute_sections 
