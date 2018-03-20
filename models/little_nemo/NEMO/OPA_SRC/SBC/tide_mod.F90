MODULE tide_mod
  !!=================================================================================
  !!                       ***  MODULE  tide_mod  ***
  !! Compute nodal modulations corrections and pulsations
  !!=================================================================================
  !!---------------------------------------------------------------------------------
  !!   OPA 9.0 , LODYC-IPSL  (2003)
  !!---------------------------------------------------------------------------------
  USE dom_oce         ! ocean space and time domain
  USE phycst
  USE daymod

  IMPLICIT NONE
  PRIVATE

  REAL(wp) :: sh_T, sh_s, sh_h, sh_p, sh_p1, &
       sh_xi, sh_nu, sh_nuprim, sh_nusec, sh_R, &
       sh_I, sh_x1ra, sh_N

  INTEGER,PUBLIC, PARAMETER ::   &
       jpmax_harmo = 19             ! maximum number of harmonic

  TYPE,PUBLIC:: tide
     CHARACTER(LEN=4)  :: cname_tide
     REAL(wp) :: equitide
     INTEGER  :: nutide
     INTEGER  ::  nt,ns,nh,np,np1,shift
     INTEGER  ::  nksi,nnu0,nnu1,nnu2,R
     INTEGER  :: nformula
  END TYPE tide

  TYPE(tide), PUBLIC, DIMENSION(jpmax_harmo) :: Wave

  !! * Accessibility
  PUBLIC tide_harmo
  PUBLIC nodal_factort
  PUBLIC tide_init_Wave

CONTAINS

  SUBROUTINE tide_init_Wave

#  include "tide.h90"

  END SUBROUTINE tide_init_Wave

  SUBROUTINE tide_harmo( pomega, pvt, put , pcor, ktide ,kc)

    INTEGER, DIMENSION(kc), INTENT( in ) ::   &
         ktide      ! Indice of tidal constituents

    INTEGER, INTENT( in ) :: &
         kc         ! Total number of tidal constituents

    REAL (wp), DIMENSION(kc), INTENT( out ) ::   &
         pomega      ! pulsation in radians/s

    REAL (wp), DIMENSION(kc), INTENT( out ) ::   &
         pvt, &      !
         put, &      !
         pcor         !

    CALL astronomic_angle
    CALL tide_pulse(pomega, ktide ,kc)
    CALL tide_vuf( pvt, put, pcor, ktide ,kc)

  END SUBROUTINE tide_harmo

  SUBROUTINE astronomic_angle

    !!----------------------------------------------------------------------
    !!
    !!  tj is time elapsed since 1st January 1900, 0 hour, counted in julian
    !!  century (e.g. time in days divide by 36525)
    !!----------------------------------------------------------------------

    REAL(wp) ::  cosI,p,q,t2,t4,sin2I,s2,tgI2,P1,sh_tgn2,at1,at2
    REAL(wp) :: zqy,zsy,zday,zdj,zhfrac

    zqy=AINT((nyear-1901.)/4.)
    zsy=nyear-1900.

    zdj=dayjul(nyear,nmonth,nday)
    zday=zdj+zqy-1.

    zhfrac=nsec_day/3600.

    !----------------------------------------------------------------------
    !  Sh_n Longitude of ascending lunar node
    !----------------------------------------------------------------------

    sh_N=(259.1560564-19.328185764*zsy-.0529539336*zday-.0022064139*zhfrac)*rad
    !----------------------------------------------------------------------
    ! T mean solar angle (Greenwhich time)
    !----------------------------------------------------------------------
    sh_T=(180.+zhfrac*(360./24.))*rad
    !----------------------------------------------------------------------
    ! h mean solar Longitude
    !----------------------------------------------------------------------

    sh_h=(280.1895014-.238724988*zsy+.9856473288*zday+.0410686387*zhfrac)*rad
    !----------------------------------------------------------------------
    ! s mean lunar Longitude
    !----------------------------------------------------------------------

    sh_s=(277.0256206+129.38482032*zsy+13.176396768*zday+.549016532*zhfrac)*rad
    !----------------------------------------------------------------------
    ! p1 Longitude of solar perigee
    !----------------------------------------------------------------------

    sh_p1=(281.2208569+.01717836*zsy+.000047064*zday+.000001961*zhfrac)*rad
    !----------------------------------------------------------------------
    ! p Longitude of lunar perigee
    !----------------------------------------------------------------------

    sh_p=(334.3837214+40.66246584*zsy+.111404016*zday+.004641834*zhfrac)*rad

    sh_N =mod(sh_N ,2*rpi)
    sh_s =mod(sh_s ,2*rpi)
    sh_h =mod(sh_h, 2*rpi)
    sh_p =mod(sh_p, 2*rpi)
    sh_p1=mod(sh_p1,2*rpi)

    cosI=0.913694997 -0.035692561 *cos(sh_N)

    sh_I=acos(cosI)

    sin2I=sin(sh_I)
    sh_tgn2=tan(sh_N/2.0)

    at1=atan(1.01883*sh_tgn2)
    at2=atan(0.64412*sh_tgn2)

    sh_xi=-at1-at2+sh_N

    if (sh_N > rpi) sh_xi=sh_xi-2.0*rpi

    sh_nu=at1-at2

    !----------------------------------------------------------------------
    ! For constituents l2 k1 k2
    !----------------------------------------------------------------------

    tgI2=tan(sh_I/2.0)
    P1=sh_p-sh_xi

    t2=tgI2*tgI2
    t4=t2*t2
    sh_x1ra=sqrt(1.0-12.0*t2*cos(2.0*P1)+36.0*t4)

    p=sin(2.0*P1)
    q=1.0/(6.0*t2)-cos(2.0*P1)
    sh_R=atan(p/q)

    p=sin(2.0*sh_I)*sin(sh_nu)
    q=sin(2.0*sh_I)*cos(sh_nu)+0.3347
    sh_nuprim=atan(p/q)

    s2=sin(sh_I)*sin(sh_I)
    p=s2*sin(2.0*sh_nu)
    q=s2*cos(2.0*sh_nu)+0.0727
    sh_nusec=0.5*atan(p/q)

  END SUBROUTINE astronomic_angle

  SUBROUTINE tide_pulse( pomega, ktide ,kc)
    !!----------------------------------------------------------------------
    !!                     ***  ROUTINE tide_pulse  ***
    !!                      
    !! ** Purpose : Compute tidal frequencies
    !!
    !!----------------------------------------------------------------------
    !! * Arguments
    INTEGER, DIMENSION(kc), INTENT( in ) ::   &
         ktide      ! Indice of tidal constituents

    INTEGER, INTENT( in ) :: &
         kc         ! Total number of tidal constituents

    REAL (wp), DIMENSION(kc), INTENT( out ) ::   &
         pomega      ! pulsation in radians/s

    !! * Local declarations
    INTEGER :: jh
    REAL(wp) :: zscale  =  36525*24.0
    REAL(wp) :: zomega_T=  13149000.0
    REAL(wp) :: zomega_s=    481267.892
    REAL(wp) :: zomega_h=     36000.76892
    REAL(wp) :: zomega_p=      4069.0322056
    REAL(wp) :: zomega_n=      1934.1423972
    REAL(wp) :: zomega_p1=        1.719175
    !!----------------------------------------------------------------------

    DO jh=1,kc
       pomega(jh) = zomega_T * Wave(ktide(jh))%nT &
            + zomega_s * Wave(ktide(jh))%ns &
            + zomega_h * Wave(ktide(jh))%nh &
            + zomega_p * Wave(ktide(jh))%np &
            + zomega_p1* Wave(ktide(jh))%np1
       pomega(jh) = (pomega(jh)/zscale)*rad/3600.
    END DO

  END SUBROUTINE tide_pulse

  SUBROUTINE tide_vuf( pvt, put, pcor, ktide ,kc)
    !!----------------------------------------------------------------------
    !!                     ***  ROUTINE tide_vuf  ***
    !!                      
    !! ** Purpose : Compute nodal modulation corrections
    !!
    !! ** Outputs :
    !!          vt: Pase of tidal potential relative to Greenwich (radians)
    !!          ut: Phase correction u due to nodal motion (radians)
    !!          ft: Nodal correction factor
    !!
    !! ** Inputs :
    !!          tname: array of constituents names (dimension<=nc) 
    !!             nc: number of constituents
    !!   
    !!----------------------------------------------------------------------
    !! * Arguments
    INTEGER, DIMENSION(kc), INTENT( in ) ::   &
         ktide      ! Indice of tidal constituents
    INTEGER, INTENT( in ) :: &
         kc         ! Total number of tidal constituents
    REAL (wp), DIMENSION(kc), INTENT( out ) ::   &
         pvt, &      !
         put, &      !
         pcor         !
    !! * Local declarations
    INTEGER :: jh
    !!----------------------------------------------------------------------

    DO jh =1,kc
       !  Phase of the tidal potential relative to the Greenwhich 
       !  meridian (e.g. the position of the fictuous celestial body). Units are
       !  radian:
       pvt(jh) = sh_T *Wave(ktide(jh))%nT        &
            +sh_s *Wave(ktide(jh))%ns        &
            +sh_h *Wave(ktide(jh))%nh        &
            +sh_p *Wave(ktide(jh))%np        &
            +sh_p1*Wave(ktide(jh))%np1       &
            +Wave(ktide(jh))%shift*rad
       !
       !  Phase correction u due to nodal motion. Units are radian:
       put(jh) = sh_xi    *Wave(ktide(jh))%nksi  &
            +sh_nu    *Wave(ktide(jh))%nnu0  &
            +sh_nuprim*Wave(ktide(jh))%nnu1  &
            +sh_nusec *Wave(ktide(jh))%nnu2  &
            +sh_R     *Wave(ktide(jh))%R

       !  Nodal correction factor:
       pcor(jh) = nodal_factort(Wave(ktide(jh))%nformula)
    END DO

  END SUBROUTINE tide_vuf

  recursive function nodal_factort(kformula) result (zf)
    !!----------------------------------------------------------------------
    INTEGER, INTENT(IN) :: kformula
    REAL(wp) :: zf
    REAL(wp) :: zs,zf1,zf2

    SELECT CASE (kformula)

       !!  formule 0, solar waves

    case ( 0 )
       zf=1.0

       !! formule 1, compound waves (78 x 78)

    case ( 1 )
       zf=nodal_factort(78)
       zf=zf*zf

       !! formule 2, compound waves (78 x 0)  ===  (78) 

    case ( 2 )
       zf1=nodal_factort(78)
       zf=nodal_factort(0)
       zf=zf1*zf

       !! formule 4,  compound waves (78 x 235) 

    case ( 4 )
       zf1=nodal_factort(78)
       zf=nodal_factort(235)
       zf=zf1*zf

       !! formule 5,  compound waves (78 *78 x 235)

    case ( 5 )
       zf1=nodal_factort(78)
       zf=nodal_factort(235)
       zf=zf*zf1*zf1

       !! formule 6,  compound waves (78 *78 x 0)

    case ( 6 )
       zf1=nodal_factort(78)
       zf=nodal_factort(0)
       zf=zf*zf1*zf1 

       !! formule 7, compound waves (75 x 75)

    case ( 7 )
       zf=nodal_factort(75)
       zf=zf*zf

       !! formule 8,  compound waves (78 x 0 x 235)

    case ( 8 )
       zf=nodal_factort(78)
       zf1=nodal_factort(0)
       zf2=nodal_factort(235)
       zf=zf*zf1*zf2

       !! formule 9,  compound waves (78 x 0 x 227)

    case ( 9 )
       zf=nodal_factort(78)
       zf1=nodal_factort(0)
       zf2=nodal_factort(227)
       zf=zf*zf1*zf2

       !! formule 10,  compound waves (78 x 227)

    case ( 10 )
       zf=nodal_factort(78)
       zf1=nodal_factort(227)
       zf=zf*zf1

       !! formule 11,  compound waves (75 x 0)

    case ( 11 )
       zf=nodal_factort(75)
       zf=nodal_factort(0)
       zf=zf*zf1

       !! formule 12,  compound waves (78 x 78 x 78 x 0) 

    case ( 12 )
       zf1=nodal_factort(78)
       zf=nodal_factort(0)
       zf=zf*zf1*zf1*zf1

       !! formule 13, compound waves (78 x 75)

    case ( 13 )
       zf1=nodal_factort(78)
       zf=nodal_factort(75)
       zf=zf*zf1

       !! formule 14, compound waves (235 x 0)  ===  (235)

    case ( 14 )
       zf=nodal_factort(235)
       zf1=nodal_factort(0)
       zf=zf*zf1

       !! formule 15, compound waves (235 x 75) 

    case ( 15 )
       zf=nodal_factort(235)
       zf1=nodal_factort(75)
       zf=zf*zf1

       !! formule 16, compound waves (78 x 0 x 0)  ===  (78)

    case ( 16 )
       zf=nodal_factort(78)
       zf1=nodal_factort(0)
       zf=zf*zf1*zf1

       !! formule 17,  compound waves (227 x 0) 

    case ( 17 )
       zf1=nodal_factort(227)
       zf=nodal_factort(0)
       zf=zf*zf1

       !! formule 18,  compound waves (78 x 78 x 78 )

    case ( 18 ) 
       zf1=nodal_factort(78)
       zf=zf1*zf1*zf1

       !! formule 19, compound waves (78 x 0 x 0 x 0)  ===  (78)

    case ( 19 )
       zf=nodal_factort(78)
       zf1=nodal_factort(0)
       zf=zf*zf1*zf1

       !! formule 73

    case ( 73 )
       zs=sin(sh_I)
       zf=(2./3.-zs*zs)/0.5021

       !! formule 74

    case ( 74 )
       zs=sin(sh_I)
       zf=zs*zs/0.1578

       !! formule 75

    case ( 75 )
       zs=cos (sh_I/2)
       zf=sin (sh_I)*zs*zs/0.3800

       !! formule 76

    case ( 76 )
       zf=sin (2*sh_I)/0.7214

       !! formule 77

    case ( 77 )
       zs=sin (sh_I/2)
       zf=sin (sh_I)*zs*zs/0.0164

       !! formule 78

    case ( 78 )
       zs=cos (sh_I/2)
       zf=zs*zs*zs*zs/0.9154

       !! formule 79

    case ( 79 )
       zs=sin(sh_I)
       zf=zs*zs/0.1565

       !! formule 144

    case ( 144 )
       zs=sin (sh_I/2)
       zf=(1-10*zs*zs+15*zs*zs*zs*zs)*cos(sh_I/2)/0.5873

       !! formule 149

    case ( 149 )
       zs=cos (sh_I/2)
       zf=zs*zs*zs*zs*zs*zs/0.8758

       !! formule 215

    case ( 215 )
       zs=cos (sh_I/2)
       zf=zs*zs*zs*zs/0.9154*sh_x1ra

       !! formule 227 

    case ( 227 )
       zs=sin (2*sh_I)
       zf=sqrt (0.8965*zs*zs+0.6001*zs*cos (sh_nu)+0.1006)

       !! formule 235 

    case ( 235 )
       zs=sin (sh_I)
       zf=sqrt (19.0444*zs*zs*zs*zs+2.7702*zs*zs*cos (2*sh_nu)+.0981)

    END SELECT

  end function nodal_factort

  function dayjul(kyr,kmonth,kday)
    !
    !*** THIS ROUTINE COMPUTES THE JULIAN DAY (AS A REAL VARIABLE)
    !
    INTEGER,INTENT(IN) :: kyr,kmonth,kday
    INTEGER,DIMENSION(12) ::  idayt,idays
    INTEGER :: inc,ji
    REAL(wp) :: dayjul,zyq

    DATA idayt/0.,31.,59.,90.,120.,151.,181.,212.,243.,273.,304.,334./
    idays(1)=0.
    idays(2)=31.
    inc=0.
    zyq=MOD((kyr-1900.),4.)
    IF(zyq .eq. 0.) inc=1.
    DO ji=3,12
       idays(ji)=idayt(ji)+inc
    END DO
    dayjul=idays(kmonth)+kday

  END FUNCTION dayjul

END MODULE tide_mod
