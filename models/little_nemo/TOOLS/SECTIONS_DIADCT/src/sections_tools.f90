MODULE sections_tools
   !!=====================================================================
   !!                       ***  MODULE  sections_tools  ***
   !! 
   !! History: 2011: Clement Bricaud, Mercator-Ocean
   !! 
   !!=====================================================================
   !! * Modules used
   USE declarations
 
   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC pointToCoordF         ! define a point with geographical coordinates
   PUBLIC distance2             ! compute distance between 2 points
   PUBLIC distance3             ! compute distance between a point and a line
   PUBLIC intersec              ! intersection between 2 lines
   PUBLIC slope_coeff           ! slope coefficient of a line
   PUBLIC qcksrt                ! organize a list of point
   PUBLIC write_debug           ! write debug messages
   PUBLIC file_open             ! open a file ( ascii, bin)
 
CONTAINS

  TYPE(COORD_SECTION) FUNCTION pointToCoordF(p)
     !!---------------------------------------------------------------------
     !!         ***  FUNCTION pointToCoordF  ***
     !!           
     !! ** Purpose: define a point with geographical coordinates
     !!
     !!---------------------------------------------------------------------
     !! * arguments
     TYPE(POINT_SECTION), INTENT(IN) ::  p

     !!---------------------------------------------------------------------

     pointToCoordF = COORD_SECTION(glamf(p%I,p%J),gphif(p%I,p%J))

     RETURN

  END FUNCTION pointToCoordF

  REAL(wp) FUNCTION distance2(coordA,coordB,ld_date_line)
     !!---------------------------------------------------------------------
     !!         ***  FUNCTION distance  ***
     !!
     !! ** Purpose:compute distance between coordA and coordB
     !!            We add 360 to coordB%long if the line (coordA,coordB)
     !!            crosses the date-line.
     !!
     !!---------------------------------------------------------------------
     !! * arguments
     TYPE(COORD_SECTION), INTENT(IN) :: coordA,coordB
     LOGICAL,INTENT(IN),OPTIONAL     :: ld_date_line

     !! * Local declarations 
     REAL(wp) :: zrad, zrayon
     REAL(wp) :: zpi = 3.141592653589793
     REAL(wp) :: zlam1, zlam2, zphi1, zphi2

     !-----------------------------------------------
     zrayon = 6367000.0
     zrad   = zpi/180.
     zlam1  = coordA%lon*zrad
     zlam2  = coordB%lon*zrad
     zphi1  = coordA%lat*zrad
     zphi2  = coordB%lat*zrad
     !
     !function output
     distance2=0.001*2.*zrayon *ASIN(SQRT(SIN((zphi2-zphi1)/2.0)**2.0+COS(zphi1)*COS(zphi2)*SIN((zlam2-zlam1)/2.0)**2.0) )
     !
     RETURN
  END FUNCTION distance2

  REAL(wp) FUNCTION distance3(coordA,coordB,coordC,ll_debug) 
     !!---------------------------------------------------------------------
     !!         ***  FUNCTION cosinus  ***
     !!
     !! ** Purpose: compute distance between a point B and a line (AC)
     !!
     !! ** Methode: use Al-Kashi theorem
     !!
     !!              B                   A first point of the section
     !!             / \                  B intermediate point on the section
     !!            / | \                 C last point of the section
     !!           /  |  \               
     !!          /   |   \               angle=ACB
     !!         /    |    \              
     !!        /     |     \             distance3=Bd
     !!       /      |      \
     !!    C  -------d------- A  
     !!
     !!---------------------------------------------------------------------
     !! * arguments
     TYPE(COORD_SECTION), INTENT(IN) :: coordA,coordB,coordC
     LOGICAL                         :: ll_debug

     !! * Local declarations
     REAL(wp)                        :: ztmp
     REAL(wp)                        :: za, zb, zc, zangle

     !-----------------------------------------------
     za = SQRT( (coordB%lon-coordC%lon)**2 + (coordB%lat-coordC%lat)**2 )
     zb = SQRT( (coordA%lon-coordC%lon)**2 + (coordA%lat-coordC%lat)**2 )
     zc = SQRT( (coordA%lon-coordB%lon)**2 + (coordA%lat-coordB%lat)**2 )
     !
     IF( za /= 0. .AND. zb /= 0. )THEN
        ztmp = ( za**2 + zb**2  - zc**2 ) / ( 2.0*za*zb )
        ztmp = MIN(ztmp,1.00_wp)
        IF( ztmp==1.00_wp )THEN ; zangle = 0.0 
        ELSE                    ; zangle = ABS(acos(ztmp)) 
        ENDIF
     ELSE
        PRINT*,'You should not have this situation: za zb =',za,zb ; STOP
     ENDIF
     !
     distance3 = za*ABS(sin(zangle)) 
     !
     !function output
     RETURN
  END FUNCTION distance3

  REAL(wp) FUNCTION slope_coeff(coordA,coordB,ld_dateline)
     !!---------------------------------------------------------------------
     !!         ***  Function slope_coeff  ***
     !!
     !! ** Purpose: Compute slope coefficient of the line (coordA,coordB)
     !!
     !! ** Method:
     !!           Usual method :
     !!                   slope_coeff = (latB-latA)/(lonB-lonA)
     !!           Special case: the segment [A,B] crosses the date-line (lddate=T)
     !!                   slope_coeff = (latB-latA)/(360+lonB-lonA)
     !!---------------------------------------------------------------------
     !! * arguments
     TYPE(COORD_SECTION), INTENT(IN) :: coordA, coordB
     LOGICAL,INTENT(IN),OPTIONAL     :: ld_dateline

     !! * Local declarations
     REAL(wp)                        :: zcoeff
     INTEGER                         :: idateline

     !!---------------------------------------------------------
     !initialization
     zcoeff = 1.e20
     idateline=0
     IF( PRESENT( ld_dateline))THEN
       IF( ld_dateline )idateline=1
     ENDIF

     !compute slope coefficient
     IF ( coordB%lon-coordA%lon .NE. 0.) &
         zcoeff=(coordB%lat-coordA%lat)/(360*idateline+coordB%lon-coordA%lon)

     !output
     slope_coeff = zcoeff
     RETURN

  END FUNCTION slope_coeff 

  TYPE(COORD_SECTION) FUNCTION intersec(sec,coord_a,coord_b)
     !!---------------------------------------------------------------------
     !!         ***  Function intersec  ***
     !!
     !! ** Purpose:Return the coordinates of the intersection point, 
     !!            between segment [a,b] and the section.
     !!            If no intersection the point = -9999.
     !!
     !! ** Method:(coord_a,coord_b) => y=za2*x+zb2  (2)
     !!            sec              => y=za1*x+zb1  (1)
     !!    Intersection = (X,Y) solves (1) and (2)
     !!    Vefify that (X,Y) is in [coord_a,coord_b] AND in the section
     !!         
     !! ** Action: 1. compute za1, za1 
     !!            2. compute zb1, zb2
     !!            3. compute X and Y
     !!            4.Verify that (zX,zY) is in [coord_a,coord_b] 
     !!              and in [sec%coordSec(1),sec%coordSec(2)]    
     !!
     !! History: Author: 10/05 Matthieu Laborie
     !!          Additions:
     !!           05-2007: (C Bricaud) add special cases
     !!                                (crossing date line)
     !----------------------------------------------------------------------------- 
     !! * arguments
     TYPE(SECTION), INTENT(IN)        :: sec
     TYPE(COORD_SECTION)  , INTENT(IN):: coord_a, coord_b

     !! * Local declarations
     TYPE(COORD_SECTION) :: coordInter
     REAL(wp)            :: za1,za2, &! slope coefficient
                            zb1,zb2, &! 
                            zX,zY     ! coordinates of intersection
     LOGICAL             :: ll_date_line=.FALSE. !segment [a,b] crosses the date-line ?

     !----------------------------------------------------------------------------

     !=================!
     !0. INITALIZATION !
     !=================!
     coordInter=COORD_SECTION(-9999.,-9999.) 
     ll_date_line=.FALSE.    
     !we need to know if [coord_a,coord_b] crosses the date-line 
     if(coord_a%lon-coord_b%lon .GT. 180)ll_date_line=.TRUE.

     !=======================!
     !1. compute za1 and za2 !
     !=======================!
     za1=sec%slopeSection
     za2=slope_coeff(coord_a,coord_b,ll_date_line)
 
     !=======================!
     !2. compute zb1 and zb2 !
     !=======================!

     ! Compute coefficient b for a straight line y=a*x+b
     !        Usual method:  knowing value of a, we compute b with coordinates of 1 point:
     !                       b=latA-a.lonA or b=latB-a.lonB
     !        Particular case: the straight line crosses the date line; so it is in 2 parts:
     !                         one on the left of the date-line and one the right
     !                         then,b can could have 2 values:
     ! As the date-line can be crossed by the section and the segment of the mesh [a,b],
     ! we have to check if the segment [a,b] crosses the date-line (we know it already for the
     ! section(sec%ll_date_line): ll_date_line
     ! Then, there are 4 cases for computing b1 and b2 ( sec%ll_date_line=T/F AND ll_date_line=T/F )
     !=========================================================================================!
     ! CASE A :                               !CASE C:
     ! sec%ll_date_line=T AND ll_date_line =T !sec%ll_date_line=F AND ll_date_line =T
     !                                        !     |  -180 !+180   |
     ! this case doesn't exist                !     |       !       |
     !========================================! a___|_______!_______|__b
     !CASE B:                                 !     |       !       |
     !sec%ll_date_line=T AND ll_date_line=F   !     |       !       |
     !     \     |                            ! section  date-line  section
     !   a__\__b |                            ! b1: usual method
     !       \   |                            ! b2: depend of longitude of
     !        \  |                            !     sec%coordSec(1) et de  sec%coordSec(2)
     !         \ |                            !=================================================!
     !          \|                            !CASE D:
     !     +180  \  -180                      !sec%ll_date_line=F AND ll_date_line =F
     !           |\                           !      |       !       |
     !           | \                          !      | +180  ! -180  |
     !           |  \                         !  a___|___b   !   a___|___b
     !           |a__\__b                     !      |       !       |
     !           |    \                       !      |       !       |
     !    date-line   section                 !   section date-line section
     !b2: usual method                        !b1: usual method
     !b1: depend of longitude of a and b.     !b2: usual method
     !==========================================================================================!

     IF( sec%ll_date_line )THEN
          IF( ll_date_line )THEN
              !CASE A: this case doesn't exist
              !-------
              zb1=1.e20
              zb2=1.e20
          ELSE 
              !CASE B:
              !-------
              !compute zb2: 
              !zb2 = coord_b%lat - za2 * coord_b%lon = coord_a%lat - za2 * coord_a%lon 
              zb2=coord_b%lat - za2 * coord_b%lon  

              !compute zb1:
              IF( coord_a%lon .GT. 0 .AND. coord_b%lon .GT. 0  )THEN
                  zb1 = sec%coordSec(1)%lat - za1 * sec%coordSec(1)%lon
              ELSE  
                  zb1 = sec%coordSec(2)%lat - za1 * sec%coordSec(2)%lon
              ENDIF 
          ENDIF
     ELSE
          IF( ll_date_line )THEN
              !CASE C:
              !-------
              !Compute zb1: 
              !zb1 = sec%coordSec(1)%lat - za1 * sec%coordSec(1)%lon 
              !    = sec%coordSec(2)%lat - za1 * sec%coordSec(2)%lon 
              zb1 = sec%coordSec(1)%lat - za1 * sec%coordSec(1)%lon

              !Compute zb2:
              IF( sec%coordSec(1)%lon .GT. 0 .AND. sec%coordSec(2)%lon .GT. 0 )THEN
                  zb2 = coord_a%lat - za2 * coord_a%lon
              ELSE  
                  zb2 = coord_b%lat - za2 * coord_b%lon
              ENDIF
          ELSE
              !CASE D:
              !-------
              !compute zb1:
              !zb1 = sec%coordSec(1)%lat - za1 * sec%coordSec(1)%lon  
              !    = sec%coordSec(2)%lat - za1 * sec%coordSec(2)%lon
              zb1 = sec%coordSec(1)%lat - za1 * sec%coordSec(1)%lon

              !compute zb2:
              !zb2 = coord_b%lat - za2 * coord_b%lon = coord_a%lat - za2 * coord_a%lon
              zb2 = coord_b%lat - za2 * coord_b%lon  
          ENDIF
     ENDIF 

     IF( (za1 - za2) .NE. 0) THEN
          !================================!
          !3. Compute intersection (zX,zY) !
          !================================!
          IF( za1 == 1.e20 ) THEN ! Case X=constant
              zX = sec%coordSec(1)%lon
              zY = za2 * zX + zb2
          ELSE IF ( za2 == 1.e20) THEN ! Case X=constant
              zX = coord_b%lon
              zY = za1 * zX + zb1
          ELSE ! Case zY=A*zX+B
              zX = (zb2 - zb1) / (za1 - za2)
              zY = (za1 * zX ) + zb1
          ENDIF
           
          !==============================================!
          !4.Verify that (zX,zY) is in [coord_a,coord_b] !
          !  and in [sec%coordSec(1),sec%coordSec(2)]    !
          !==============================================!
          !Be careful! The test is not the same for all configurations
          IF( sec%ll_date_line )THEN

              IF( ll_date_line )THEN
                  !CASE A: this case doesn't exist
                  !-------
                  coordInter=COORD_SECTION(-9999.,-9999.)
              ELSE
                  !CASE B:
                  !-------   
                  IF( zX .GE. MIN(coord_a%lon,coord_b%lon ) .AND. &
                      zX .LE. MAX(coord_a%lon,coord_b%lon ) .AND. &
                      zY .GE. MIN(coord_a%lat,coord_b%lat ) .AND. &
                      zY .LE. MAX(coord_a%lat,coord_b%lat ) .AND. &
                    ((zX .LE. MIN(sec%coordSec(1)%lon,sec%coordSec(2)%lon ) .AND. &
                      zX .GE. -180) .OR.  &
                     (zX .GE. MAX(sec%coordSec(1)%lon,sec%coordSec(2)%lon ) .AND. &
                      zX .LE. 180)) .AND. &
                      zY .GE. MIN(sec%coordSec(1)%lat,sec%coordSec(2)%lat ) .AND. &
                      zY .LE. MAX(sec%coordSec(1)%lat,sec%coordSec(2)%lat ) ) THEN
                     coordInter = COORD_SECTION(zX, zY)
                  ENDIF
              ENDIF
          ELSE

              IF( ll_date_line )THEN
                  !CASE C:
                  !-------
                  IF( ((zX .LE. MIN(coord_a%lon,coord_b%lon ) .AND. &
                        zX .GE. -180 )  .OR. &
                       (zX .GE. MAX(coord_a%lon,coord_b%lon ) .AND. &
                        zX .LE. 180 )) .AND. & 
                        zY .GE. MIN(coord_a%lat,coord_b%lat ) .AND. &
                        zY .LE. MAX(coord_a%lat,coord_b%lat ) .AND. &
                        zX .GE. MIN(sec%coordSec(1)%lon,sec%coordSec(2)%lon ) .AND. &
                        zX .LE. MAX(sec%coordSec(1)%lon,sec%coordSec(2)%lon ) .AND. &
                        zY .GE. MIN(sec%coordSec(1)%lat,sec%coordSec(2)%lat ) .AND. &
                        zY .LE. MAX(sec%coordSec(1)%lat,sec%coordSec(2)%lat ) ) THEN 
                        coordInter = COORD_SECTION(zX, zY)   
                  ENDIF 
              ELSE
                  !CASE D:
                  !-------
                  IF( zX .GE. MIN(coord_a%lon,coord_b%lon ) .AND. &
                      zX .LE. MAX(coord_a%lon,coord_b%lon ) .AND. &
                      zY .GE. MIN(coord_a%lat,coord_b%lat ) .AND. &
                      zY .LE. MAX(coord_a%lat,coord_b%lat ) .AND. &
                      zX .GE. MIN(sec%coordSec(1)%lon,sec%coordSec(2)%lon ) .AND. &
                      zX .LE. MAX(sec%coordSec(1)%lon,sec%coordSec(2)%lon ) .AND. &
                      zY .GE. MIN(sec%coordSec(1)%lat,sec%coordSec(2)%lat ) .AND. &
                      zY .LE. MAX(sec%coordSec(1)%lat,sec%coordSec(2)%lat ) ) THEN
                      coordInter = COORD_SECTION(zX, zY)
                  ENDIF             
              ENDIF
          ENDIF   
            
     ENDIF
      
     !output
     intersec = coordInter
     RETURN
   
  END FUNCTION intersec

  SUBROUTINE qcksrt(arr1,arr2,n)
     !!---------------------------------------------------------------------
     !!         ***  SUBROUTINE qcksrt  ***
     !!
     !! ** Purpose : organize a list point by latitude , and after by longitude
     !!              using "numerical recipies " routine.
     !!---------------------------------------------------------------------
     !! * arguments
     INTEGER,INTENT(IN)                 :: n           ! number of points
     REAL(wp),DIMENSION(n),INTENT(INOUT):: arr1,arr2   ! longitude and latitude

     !! * Local declarations
     INTEGER , PARAMETER                :: m = 7, nstack = 500
     REAL(wp), PARAMETER                :: fm=7875.,fa=211.,fc=1663.,fmi=1./fm
     INTEGER                            :: il, ir, i, iq, ist    !local integers
     INTEGER                            :: jj                    !loop indices
     REAL(wp)                           :: zfx, za, zb           !local real
     INTEGER,DIMENSION(nstack)          :: istack(nstack)        !temp array
     !---------------------------------------------------
     ist=0
     il=1
     ir=n
     zfx=0.
     !
 10  IF( ir-il .LT. m )THEN

        DO jj = il+1,ir
           za = arr1(jj)
           zb = arr2(jj)
           DO i = jj-1,1,-1
              IF( arr1(i) .LE. za )GOTO 12
              arr1(i+1) = arr1(i)
              arr2(i+1) = arr2(i)
           ENDDO
           i=0
 12        arr1(i+1) = za
           arr2(i+1) = zb
        ENDDO
        IF( ist .EQ. 0 )RETURN
        ir  = istack(ist)
        il  = istack(ist-1)
        ist = ist-2
     ELSE
        i  = il
        jj = ir
        zfx = MOD(zfx*fa+fc,fm)
        iq = il+(ir-il+1)*(zfx*fmi)
        za  = arr1(iq)
        arr1(iq) = arr1(il)
        zb = arr2(iq)
        arr2(iq) = arr2(il)
 20     CONTINUE
 21     IF( jj .GT. 0 )THEN
           IF( za .LT. arr1(jj) )THEN
              jj = jj-1
              GOTO 21
           ENDIF
        ENDIF
        IF( jj .LE. i )THEN
           arr1(i) = za
           arr2(i) = zb
           GOTO 30
        ENDIF
        arr1(i) = arr1(jj)
        arr2(i) = arr2(jj)
        i=i+1
 22     IF( i .LE. n )THEN
           IF( za .GT. arr1(i) )THEN
              i = i+1
              GOTO 22
           ENDIF
        ENDIF 
        IF( jj .LE.i )THEN
           arr1(jj) = za
           arr2(jj) = zb
           i        = jj
           GOTO 30
        ENDIF
        arr1(jj) = arr1(i)
        arr2(jj) = arr2(i)
        jj = jj - 1
        GOTO 20
 30     ist = ist + 2
        IF( ist .GT. nstack )PAUSE 'nstack too small'
        IF( ir-i .GE. i-1 )THEN
           istack(ist  ) = ir
           istack(ist-1) = i + 1
           ir = i-1
        ELSE
           istack(ist  ) = i-1
           istack(ist-1) = il
           il = i+1
        ENDIF
     ENDIF
     GOTO 10
  END SUBROUTINE qcksrt

  SUBROUTINE write_debug(ksec,cd_write)
     !!---------------------------------------------------------------------
     !!         ***  SUBROUTINE write_debug  ***
     !!
     !! ** Purpose: write debug messages
     !!
     !!---------------------------------------------------------------------
     !! * arguments
     INTEGER           :: ksec       !number of section
     CHARACTER(len=*)  :: cd_write   !message to write

     !! * Local declarations
     INTEGER           :: iunit
     LOGICAL           :: llok
     CHARACTER(len=80) :: clfilename
   
     !!---------------------------------------------------------------------
     IF( ksec == nsecdebug .OR. nsecdebug == -1 )THEN


        !open / verify is debug output file is open

        clfilename=TRIM(secs(ksec)%name)//"_debug"
        iunit = num_sec_debug(ksec)

        IF( iunit .EQ. 0 )THEN
           PRINT*,"Open debug file: "//TRIM(clfilename)
           iunit = 100 + ksec
           num_sec_debug(ksec) = iunit 
 
           CALL file_open(iunit,clfilename,llok,cdform="FORMATTED",cdstatus="REPLACE",cdaction="WRITE")
        
           IF( .NOT. llok )THEN 
               PRINT*,"Can not open TRIM(clfilename)." ; STOP
           ENDIF   
        ENDIF
      
       WRITE(iunit,*)TRIM(cd_write)

     ENDIF

  END SUBROUTINE write_debug

  SUBROUTINE file_open(knum,cdfile,ldok,cdform,cdstatus,cdaction)
     !!---------------------------------------------------------------------
     !!         ***  ROUTINE file_open ***
     !!
     !! ** Purpose: open a file
     !!
     !!---------------------------------------------------------------------
     !! * arguments
     INTEGER          ,INTENT(IN)  :: knum       ! file unit number
     CHARACTER(len=*),INTENT(IN)   :: cdfile     ! file name to open
     LOGICAL          ,INTENT(OUT) :: ldok       ! =.TRUE. if file exists and is corectly opened
     CHARACTER(len=*),INTENT(IN)   :: cdform,   &! FORM arguments for OPEN function
                                      cdstatus, &! STATUS arguments for OPEN function
                                      cdaction   ! ACTION arguments for OPEN function
                                      
     !! * Local declarations
     INTEGER :: iost
     LOGICAL :: llbon=.FALSE. !check existence of file
     !!---------------------------------------------------------------------
     ldok = .FALSE.                        ! initialization; file not found and not opened

     !check presence of file
     IF( cdstatus .EQ. "OLD" ) THEN
        INQUIRE( FILE=cdfile, EXIST=llbon ) ! check presence of namelist
        IF( llbon )THEN ;  PRINT*,TRIM(cdfile)//' EXISTS'
        ELSE            ;  PRINT*,TRIM(cdfile)//' NOT EXISTS' ; STOP
        ENDIF
     ENDIF

     !open file
     OPEN(UNIT=knum,FILE=TRIM(cdfile),FORM=cdform,status=cdstatus,action=cdaction,iostat=iost)
     IF ( iost == 0 )THEN
          ldok=.TRUE.
     ELSE
          PRINT*,TRIM(cdfile)//' bad opening. STOP.'; STOP
     ENDIF

  END SUBROUTINE file_open

END MODULE sections_tools 
