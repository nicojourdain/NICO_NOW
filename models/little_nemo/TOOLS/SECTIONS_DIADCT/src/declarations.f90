MODULE declarations 
  !!----------------------------------------------------------------------------
  !!             *** declarations global varibles 
  !!
  !! History: 2011: Clement Bricaud, Mercator-Ocean
  !!
  !!----------------------------------------------------------------------------

  IMPLICIT NONE
  PUBLIC

  !! * Shared module variables 
  INTEGER, PUBLIC, PARAMETER   :: dp=8 , sp=4, wp=dp
  INTEGER, PUBLIC, PARAMETER   :: nb_class_max  = 10            ! Max number of classes
  INTEGER, PUBLIC, PARAMETER   :: nb_sec_max    = 150           ! Max number of sections
  INTEGER, PUBLIC, PARAMETER   :: nb_point_max  = 2000          ! Max number of segments per section
  INTEGER, PUBLIC, PARAMETER   :: nb_type_class = 14            ! Max number of types of classes
  INTEGER, PUBLIC, PARAMETER   :: numnam=3                      ! Unit for namelist
  INTEGER, PUBLIC, PARAMETER   :: numdctin=1                    ! Unit for input file
  INTEGER, PUBLIC, PARAMETER   :: numdctout=2                   ! Unit for output file

  INTEGER, PUBLIC              :: jpi,jpj                       ! domain dimensions
  INTEGER, PUBLIC              :: nb_sec                        ! Number of sections read from input file
  INTEGER, PUBLIC              :: nsecdebug = 0                 ! Number of the section to debug

  REAL(wp), PUBLIC ,DIMENSION(:,:)  , ALLOCATABLE :: glamf,gphif,glamt,gphit,e1t
  INTEGER,  PUBLIC ,DIMENSION(nb_sec_max)         :: num_sec_debug

  TYPE POINT_SECTION
     INTEGER :: I,J
  END TYPE POINT_SECTION

  TYPE COORD_SECTION
     REAL(wp) :: lon,lat
  END TYPE COORD_SECTION

  TYPE SECTION
     CHARACTER(len=60)                              :: name                ! name of the sec
     LOGICAL                                        :: llstrpond           ! true if you want the computation of salt 
                                                                           ! and heat transport
     LOGICAL                                        :: ll_ice_section      ! ice surface and icevolume transport computation
     LOGICAL                                        :: ll_date_line        ! = T if the section crosses the date-line
     TYPE(COORD_SECTION), DIMENSION(2)              :: coordSec            ! longitude and latitude of the extremities of the section
     INTEGER                                        :: nb_class            ! number of boundaries for density classes
     INTEGER, DIMENSION(nb_point_max)               :: direction           ! vector direction of the point in the section
     CHARACTER(len=40),DIMENSION(nb_class_max)      :: classname           ! characteristics of the class
     REAL(wp), DIMENSION(nb_class_max)              :: zsigi             ,&! insitu density classes    (99 if you don't want)
                                                       zsigp             ,&! potential density classes    (99 if you don't want)
                                                       zsal              ,&! salinity classes   (99 if you don't want)
                                                       ztem              ,&! temperature classes(99 if you don't want)
                                                       zlay                ! depth level classes      (99 if you don't want)
     REAL(wp)                                       :: slopeSection        ! slope of the section 
     INTEGER                                        :: nb_point            ! number of points in the section
     TYPE(POINT_SECTION),DIMENSION(nb_point_max)    :: listPoint           ! list of points in the section
  END TYPE SECTION

  TYPE(SECTION),DIMENSION(nb_sec_max) :: secs ! Array of sections



END MODULE declarations
