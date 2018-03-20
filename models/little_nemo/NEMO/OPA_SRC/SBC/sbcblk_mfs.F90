MODULE sbcblk_mfs
   !!======================================================================
   !!                       ***  MODULE  sbcblk_mfs  ***
   !! Ocean forcing:  momentum, heat and freshwater flux formulation
   !!=====================================================================
   !! History :  3.3  !   2010-05 (P. Oddo) Original Code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_blk_mfs  : bulk formulation as ocean surface boundary condition
   !!                   (forced mode, mfs bulk formulae)
   !!   blk_oce_mfs  : ocean: computes momentum, heat and freshwater fluxes
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE fldread         ! read input fields
   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE iom             ! I/O manager library
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distribued memory computing library
   USE wrk_nemo        ! work arrays
   USE timing          ! Timing
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE prtctl          ! Print control
   USE sbcwave,ONLY : cdn_wave !wave module

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_blk_mfs       ! routine called in sbcmod module
      
   INTEGER , PARAMETER ::   jpfld   = 7         ! maximum number of files to read 
   INTEGER , PARAMETER ::   jp_wndi = 1         ! index of 10m wind velocity (i-component) (m/s) at T-point
   INTEGER , PARAMETER ::   jp_wndj = 2         ! index of 10m wind velocity (j-component) (m/s) at T-point
   INTEGER , PARAMETER ::   jp_clc  = 3         ! index of total cloud cover               ( % )
   INTEGER , PARAMETER ::   jp_msl  = 4         ! index of mean sea level pressure         (Pa)
   INTEGER , PARAMETER ::   jp_tair = 5         ! index of 10m air temperature             (Kelvin)
   INTEGER , PARAMETER ::   jp_rhm  = 6         ! index of dew point temperature           (Kelvin)
   INTEGER , PARAMETER ::   jp_prec = 7         ! index of total precipitation (rain+snow) (Kg/m2/s)
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf ! structure of input fields (file informations, fields read)
         
   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.2 , LOCEAN-IPSL (2009) 
   !! $Id: sbcblk_mfs.F90 1730 2009-11-16 14:34:19Z poddo $
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS


   SUBROUTINE sbc_blk_mfs( kt )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE sbc_blk_mfs  ***
      !!                   
      !! ** Purpose :   provide at each time step the surface ocean fluxes
      !!      (momentum, heat, freshwater, runoff is added later in the code) 
      !!
      !! ** Method  : (1) READ Atmospheric data from NetCDF files:
      !!      the 10m wind velocity (i-component) (m/s) at T-point
      !!      the 10m wind velocity (j-component) (m/s) at T-point
      !!      the 2m Dew point Temperature        (k)
      !!      the Cloud COver                     (%)
      !!      the 2m air temperature              (Kelvin)
      !!      the Mean Sea Level Preesure         (hPa)
      !!      the Climatological Precipitation    (kg/m2/s)
      !!              (2) CALL blk_oce_mfs
      !!
      !!      Computes:
      !!      Solar Radiation using Reed formula (1975, 1977)
      !!      Net Long wave radiation using Bignami et al. (1995)
      !!      Latent and Sensible heat using Kondo (1975)
      !!      Drag coeff using Hllerman and Rosenstein (1983)
      !!      C A U T I O N : never mask the surface stress fields
      !!                      the stress is assumed to be in the mesh referential
      !!                      i.e. the (i,j) referential
      !!
      !! ** Action  :   defined at each time-step at the air-sea interface
      !!              - utau, vtau  i- and j-component of the wind stress
      !!              - taum        wind stress module at T-point
      !!              - wndm        10m wind module at T-point
      !!              - qns, qsr    non-slor and solar heat flux
      !!              - emp, emps   evaporation minus precipitation
      !!----------------------------------------------------------------------
      REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  sh_now   ! specific humidity at T-point 
      REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  catm     ! Cover 
      REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  alonl    ! Lon 
      REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  alatl    ! Lat 
      REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  gsst     ! SST 
      !!---------------------------------------------------------------------
      !! Local fluxes variables
      !!---------------------------------------------------------------------
      REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  qbw     ! Net Long wave 
      REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  ha      ! Sesnible 
      REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  elat    ! Latent 
      REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  evap    ! evaporation rate 

      INTEGER, INTENT( in  ) ::   kt   ! ocean time step
      !!
      INTEGER  :: ierror                          ! return error code
      INTEGER  :: ifpr     ! dummy loop indice
      INTEGER  :: jj,ji    ! dummy loop arguments
      REAL(wp) :: act_hour
      !!--------------------------------------------------------------------
      !! Variables for specific humidity computation
      !!--------------------------------------------------------------------
      REAL(wp) :: onsea,par1,par2
      DATA onsea,par1,par2 / 0.98, 640380., -5107.4 /
      !!                      par1 [Kg/m3], par2 [K]

      CHARACTER(len=100) ::  cn_dir                           ! Root directory for location of Atmospheric forcing files
      TYPE(FLD_N), DIMENSION(jpfld) ::   slf_i                ! array of namelist informations on the fields to read
      TYPE(FLD_N) ::   sn_wndi, sn_wndj, sn_clc, sn_msl       ! informations about the fields to be read
      TYPE(FLD_N) ::   sn_tair , sn_rhm, sn_prec              !   "                                 "
      !!---------------------------------------------------------------------
      NAMELIST/namsbc_mfs/ cn_dir ,                                          &
         &                  sn_wndi , sn_wndj, sn_clc   , sn_msl ,           &
         &                  sn_tair , sn_rhm , sn_prec 
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('sbc_blk_mfs')
      !
      !                                         ! ====================== !
      IF( kt == nit000 ) THEN                   !  First call kt=nit000  !
         !                                      ! ====================== !
      ALLOCATE( sh_now(jpi,jpj), catm(jpi,jpj), alonl(jpi,jpj), alatl(jpi,jpj),     &
         &        gsst(jpi,jpj),  qbw(jpi,jpj),    ha(jpi,jpj),  elat(jpi,jpj),     &
         &        evap(jpi,jpj), STAT=ierror )

         IF( ierror /= 0 )   CALL ctl_warn('sbc_blk_mfs: failed to allocate arrays')
            !
            ! set file information (default values)
            cn_dir = './'       ! directory in which the model is executed
            !
            ! (NB: frequency positive => hours, negative => months)
            !            !    file     ! frequency !  variable  ! time intep !  clim   ! 'yearly' or ! weights  ! rotation   !
            !            !    name     !  (hours)  !   name     !   (T/F)    !  (T/F)  !  'monthly'  ! filename ! pairs      !
            sn_wndi = FLD_N( 'ecmwf'   ,    24     ,  'u10'     ,  .false.   , .false. ,   'daily'   , ''       , ''         )
            sn_wndj = FLD_N( 'ecmwf'   ,    24     ,  'v10'     ,  .false.   , .false. ,   'daily'   , ''       , ''         )
            sn_clc  = FLD_N( 'ecmwf'   ,    24     ,  'clc'     ,  .false.   , .false. ,   'daily'   , ''       , ''         )
            sn_msl  = FLD_N( 'ecmwf'   ,    24     ,  'msl'     ,  .false.   , .false. ,   'daily'   , ''       , ''         )
            sn_tair = FLD_N( 'ecmwf'   ,    24     ,  't2'      ,  .false.   , .false. ,   'daily'   , ''       , ''         )
            sn_rhm  = FLD_N( 'ecmwf'   ,    24     ,  'rh'      ,  .false.   , .false. ,   'daily'   , ''       , ''         )
            sn_prec = FLD_N( 'precip_cmap' ,  -1   ,  'precip'  ,  .true.    ,  .true. ,   'yearly'  , ''       , ''         )
            !
            REWIND( numnam )                    ! ... read in namlist namsbc_mfs
            READ  ( numnam, namsbc_mfs )
            !
            ! store namelist information in an array
            slf_i(jp_wndi) = sn_wndi   ;   slf_i(jp_wndj) = sn_wndj
            slf_i(jp_clc ) = sn_clc    ;   slf_i(jp_msl ) = sn_msl
            slf_i(jp_tair) = sn_tair   ;   slf_i(jp_rhm)  = sn_rhm
            slf_i(jp_prec) = sn_prec   ;  
            !
            ALLOCATE( sf(jpfld), STAT=ierror )         ! set sf structure
            IF( ierror > 0 ) THEN
               CALL ctl_stop( 'sbc_blk_mfs: unable to allocate sf structure' )   ;   RETURN
            ENDIF
            DO ifpr= 1, jpfld
               ALLOCATE( sf(ifpr)%fnow(jpi,jpj,1) )
               IF( slf_i(ifpr)%ln_tint ) ALLOCATE( sf(ifpr)%fdta(jpi,jpj,1,2) )
            END DO
            ! fill sf with slf_i and control print
            CALL fld_fill( sf, slf_i, cn_dir,'sbc_blk_mfs','bulk formulation for ocean SBC', 'namsbc_mfs' )
            !
         ENDIF

         CALL fld_read( kt, nn_fsbc, sf )                   ! input fields provided at the current time-step

         catm(:,:)   = 0.0    ! initializze cloud cover variable
         sh_now(:,:) = 0.0    ! initializze specifif humidity variable

         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.

         ! Calculate Specific Humidity 
         !-------------------------------------------------
            sh_now(ji,jj) = (1/1.22) * onsea * par1 * EXP(par2/sf(jp_rhm)%fnow(ji,jj,1))

         ! Normalize Clouds
         !-------------------------------------------------
            catm(ji,jj)   = max(0.0,min(1.0,sf(jp_clc)%fnow(ji,jj,1)*0.01))

            END DO
         END DO

         ! wind module at 10m 
         !--------------------------------------------------
         wndm(:,:) = SQRT(  sf(jp_wndi)%fnow(:,:,1) * sf(jp_wndi)%fnow(:,:,1)   &
              &             + sf(jp_wndj)%fnow(:,:,1) * sf(jp_wndj)%fnow(:,:,1)  )

         ! Some conv for fluxes computation
         !-------------------------------------------------
         alonl(:,:) = glamt(:,:) * rad
         alatl(:,:) = gphit(:,:) * rad
         gsst(:,:)  = tsn(:,:,1,jp_tem)  * tmask(:,:,1)

         IF( MOD( kt - 1, nn_fsbc ) == 0 ) THEN

         ! Force to zero the output of fluxes 
         !-------------------------------------------------
          qsr(:,:)  = 0.0 ; qbw(:,:)  = 0.0 ; 
          ha(:,:)   = 0.0 ; elat(:,:) = 0.0 ; 
          evap(:,:) = 0.0 ; utau(:,:) = 0.0 ; 
          vtau(:,:) = 0.0

          CALL lbc_lnk( sf(jp_wndi)%fnow(:,:,1), 'T', -1. )
          CALL lbc_lnk( sf(jp_wndj)%fnow(:,:,1), 'T', -1. )

          act_hour = (( nsec_year / rday ) - INT (nsec_year / rday)) * rjjhh

          CALL fluxes_mfs(alatl,alonl,act_hour,                                &     ! input static
                            gsst(:,:),sf(jp_tair)%fnow(:,:,1),sh_now(:,:),     &     ! input dynamic
                            sf(jp_wndi)%fnow(:,:,1), sf(jp_wndj)%fnow(:,:,1),  &     ! input dynamic
                            sf(jp_msl)%fnow(:,:,1) , catm(:,:) ,               &     ! input dynamic
                            qsr,qbw,ha,elat,evap,utau,vtau)                          ! output

         ! Shortwave radiation
         !--------------------------------------------------
          qsr(:,:) = qsr(:,:) * tmask(:,:,1)

         ! total non solar heat flux over water
         !--------------------------------------------------
          qns(:,:) = -1. * ( qbw(:,:) + ha(:,:) + elat(:,:) )
          qns(:,:) = qns(:,:)*tmask(:,:,1)

         ! mask the wind module at 10m 
         !--------------------------------------------------
          wndm(:,:) = wndm(:,:) * tmask(:,:,1)

         !   wind stress module (taum) into T-grid
         !--------------------------------------------------
          taum(:,:) = SQRT( utau(:,:) * utau(:,:) + vtau(:,:) * vtau(:,:) ) * tmask(:,:,1)

          CALL lbc_lnk( taum, 'T', 1. )

         ! Interpolate utau, vtau into the grid_V and grid_V
         !-------------------------------------------------

         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1
               utau(ji,jj) = 0.5 * ( 2. - umask(ji,jj,1) ) * ( utau(ji,jj) * tmask(ji,jj,1) &
               &                                + utau(ji+1,jj) * tmask(ji+1,jj,1) )
               vtau(ji,jj) = 0.5 * ( 2. - vmask(ji,jj,1) ) * ( vtau(ji,jj) * tmask(ji,jj,1) &
               &                                + vtau(ji,jj+1) * tmask(ji,jj+1,1) )
            END DO
         END DO

         CALL lbc_lnk( utau(:,:), 'U', -1. )
         CALL lbc_lnk( vtau(:,:), 'V', -1. )

         ! for basin budget and cooerence
         !--------------------------------------------------
!CDIR COLLAPSE
           emp (:,:) = evap(:,:) - sf(jp_prec)%fnow(:,:,1) * tmask(:,:,1)
!CDIR COLLAPSE
           emps(:,:) = emp(:,:)

         CALL iom_put( "qlw_oce",   qbw  )                 ! output downward longwave heat over the ocean
         CALL iom_put( "qsb_oce", - ha   )                 ! output downward sensible heat over the ocean
         CALL iom_put( "qla_oce", - elat )                 ! output downward latent   heat over the ocean
         CALL iom_put( "qns_oce",   qns  )                 ! output downward non solar heat over the ocean

      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('sbc_blk_mfs')
      !
   END SUBROUTINE sbc_blk_mfs
  
 
   SUBROUTINE fluxes_mfs(alat,alon,hour,                               &
        sst,tnow,shnow,unow,vnow,mslnow,cldnow,qsw,qbw,ha,elat,        &
        evap,taux,tauy)
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE fluxes_mfs  ***
      !!
      !! --- it provides SURFACE HEAT and MOMENTUM FLUXES in MKS :
      !!
      !!  1)   Water flux (WFLUX)                 [ watt/m*m ]
      !!  2)   Short wave flux (QSW)              [ watt/m*m ] Reed 1977
      !!  3)   Long wave flux backward (QBW)      [ watt/m*m ]
      !!  4)   Latent heat of evaporation (ELAT)  [ watt/m*m ]
      !!  5)   Sensible heat flux   (HA)          [ watt/m*m ]
      !!  6)   Wind stress x-component   (TAUX)   [ newton/m*m ]
      !!  7)   Wind stress y-component   (TAUY)   [ newton/m*m ]
      !!
      !!----------------------------------------------------------------------
      USE sbcblk_core, ONLY: turb_core_2z ! For wave coupling and Tair/rh from 2 to 10m

      REAL(wp), INTENT(in   ) :: hour
      REAL(wp), INTENT(in   ), DIMENSION (:,:) :: sst, unow, alat , alon
      REAL(wp), INTENT(in   ), DIMENSION (:,:) :: vnow, cldnow, mslnow
      REAL(wp), INTENT(out  ), DIMENSION (:,:) :: qsw, qbw, ha, elat
      REAL(wp), INTENT(out  ), DIMENSION (:,:) :: evap,taux,tauy
      REAL(wp), INTENT(inout), DIMENSION (:,:) :: tnow , shnow

      INTEGER :: ji,jj 
      REAL(wp)  :: wair, vtnow, ea, deltemp, s, stp , fh , fe
      REAL(wp)  :: esre, cseep

      REAL(wp), DIMENSION (:,:), POINTER ::   rspeed, sh10now, t10now, cdx, ce, shms
      REAL(wp), DIMENSION (:,:), POINTER ::   rhom, sstk, ch, rel_windu, rel_windv
      !!----------------------------------------------------------------------
      !!     coefficients ( in MKS )  :
      !!----------------------------------------------------------------------

      REAL(wp), PARAMETER ::  ps = 1013.    ! --- surface air pressure
      REAL(wp), PARAMETER ::  expsi=0.622   ! --- expsi
      REAL(wp), PARAMETER ::  rd=287.       ! --- dry air gas constant
      REAL(wp), PARAMETER ::  cp=1005.      ! --- specific heat capacity
      REAL(wp), PARAMETER ::  onsea=0.98    ! --- specific humidity factors
      REAL(wp), PARAMETER ::  par1=640380.  ! [Kg/m3]
      REAL(wp), PARAMETER ::  par2=-5107.4  ! [K]

      !---------------------------------------------------------------------
      !--- define Kondo parameters
      !---------------------------------------------------------------------

      REAL(wp), DIMENSION(5) :: a_h = (/0.0,0.927,1.15,1.17,1.652/)
      REAL(wp), DIMENSION(5) :: a_e = (/0.0,0.969,1.18,1.196,1.68/)
      REAL(wp), DIMENSION(5) :: b_h = (/1.185,0.0546,0.01,0.0075,-0.017/)
      REAL(wp), DIMENSION(5) :: b_e = (/1.23,0.0521,0.01,0.008,-0.016/)
      REAL(wp), DIMENSION(5) :: c_h = (/0.0,0.0,0.0,-0.00045,0.0/)
      REAL(wp), DIMENSION(5) :: c_e = (/0.0,0.0,0.0,-0.0004,0.0/)
      REAL(wp), DIMENSION(5) :: p_h = (/-0.157,1.0,1.0,1.0,1.0/)
      REAL(wp), DIMENSION(5) :: p_e = (/-0.16,1.0,1.0,1.0,1.0/)
      INTEGER :: kku                        !index varing with wind speed
      !
      IF( nn_timing == 1 )  CALL timing_start('fluxes_mfs')
      !
      CALL wrk_alloc( jpi,jpj, rspeed, sh10now, t10now, cdx, ce, shms )
      CALL wrk_alloc( jpi,jpj, rhom, sstk, ch, rel_windu, rel_windv )

      !!----------------------------------------------------------------------
      !! ------------------ (i)      short wave
      !!----------------------------------------------------------------------

       CALL qshort(hour,alat,alon,cldnow,qsw)

          rel_windu(:,:) = 0.0_wp
          rel_windv(:,:) = 0.0_wp

       DO jj = 2, jpj
          DO ji = fs_2, jpi
           rel_windu(ji,jj) = unow(ji,jj) - 0.5_wp * ( ssu_m(ji-1,jj) + ssu_m(ji,jj) )
           rel_windv(ji,jj) = vnow(ji,jj) - 0.5_wp * ( ssv_m(ji,jj-1) + ssv_m(ji,jj) )
          END DO
       END DO

       rspeed(:,:)= SQRT(rel_windu(:,:)*rel_windu(:,:)   &
         &                   + rel_windv(:,:)*rel_windv(:,:)) 

       sstk(:,:) = sst(:,:) + rtt                          !- SST data in Kelvin degrees
       shms(:,:) = (1/1.22)*onsea*par1*EXP(par2/sstk(:,:)) !- Saturation Specific Humidity

      ! --- Transport temperature and humidity from 2m to 10m
      !----------------------------------------------------------------------

      t10now(:,:) = 0.0           ;   sh10now(:,:)= 0.0
      ! Note that air temp is converted in potential temp
      CALL turb_core_2z(2.,10.,sstk,tnow+2*0.0098,shms,shnow,rspeed,        &
         &              Cdx,Ch,Ce,t10now,sh10now )
      tnow(:,:)  = t10now(:,:)    ;    shnow(:,:) = sh10now(:,:)

      !!----------------------------------------------------------------------
      !! ------------------ (ii)    net long wave
      !!----------------------------------------------------------------------

      DO jj = 1, jpj
         DO ji = 1, jpi
            wair = shnow(ji,jj) / (1 - shnow(ji,jj))    ! mixing ratio of the air (Wallace and Hobbs)
            vtnow = (tnow(ji,jj)*(expsi+wair))/(expsi*(1.+wair))   ! virtual temperature of air
            rhom(ji,jj) = 100.*(ps/rd)/vtnow                       ! density of the moist air
            ea   = (wair  / (wair  + 0.622 )) * mslnow(ji,jj)

            qbw(ji,jj) = emic*stefan*( sstk(ji,jj)**4. )                    &
                 - ( stefan*( tnow(ji,jj)**4. ) * ( 0.653 + 0.00535*ea ) )  &
                   * ( 1. + 0.1762*( cldnow(ji,jj)**2. ) )

         END DO
      END DO

      DO jj = 1, jpj
         DO ji = 1, jpi
      !!----------------------------------------------------------------------
      !! ------------------ (iii)   sensible heat
      !!----------------------------------------------------------------------

      !! --- calculates the term :      ( Ts - Ta )
      !!----------------------------------------------------------------------
            deltemp = sstk(ji,jj) - tnow (ji,jj)

      !!----------------------------------------------------------------------
      !! --- variable turbulent exchange coefficients ( from Kondo 1975 )
      !! --- calculate the Neutral Transfer Coefficent using an empiric formula
      !! --- by Kondo et al. Then it applies the diabatic approximation.
      !!----------------------------------------------------------------------

            s = deltemp/(wndm(ji,jj)**2.)   !! --- calculate S 
            stp = s*abs(s)/(abs(s)+0.01)    !! --- calculate the Stability Parameter 

      !!----------------------------------------------------------------------
      !! --- for stable condition (sst-t_air < 0):
      !!----------------------------------------------------------------------

            IF (s.lt.0. .and. ((stp.gt.-3.3).and.(stp.lt.0.))) THEN
                fh = 0.1_wp+0.03_wp*stp+0.9_wp*exp(4.8_wp*stp)
                fe = fh
            ELSE IF (s.lt.0. .and. stp.le.-3.3) THEN
                fh = 0._wp
                fe = fh
            ELSE                                       ! --- for unstable condition 
                fh = 1.0_wp+0.63_wp*sqrt(stp)
                fe = fh
            ENDIF

      !!----------------------------------------------------------------------
      !! --- calculate the coefficient CH,CE,CD
      !!----------------------------------------------------------------------

            IF (wndm(ji,jj) >= 0. .AND. wndm(ji,jj) <= 2.2)       THEN
                kku=1
            ELSE IF (wndm(ji,jj) > 2.2 .AND. wndm(ji,jj) <= 5.0)  THEN
                kku=2
            ELSE IF (wndm(ji,jj) > 5.0 .AND. wndm(ji,jj) <= 8.0)  THEN
                kku=3
            ELSE IF (wndm(ji,jj) > 8.0 .AND. wndm(ji,jj) <= 25.0) THEN
                kku=4
            ELSE IF (wndm(ji,jj) > 25.0 )                         THEN
                kku=5
            ENDIF

            ch(ji,jj) = ( a_h(kku) + b_h(kku) * wndm(ji,jj) ** p_h(kku)      &
                        + c_h(kku) * (wndm(ji,jj)-8 ) **2) * fh

            ce(ji,jj) = ( a_e(kku) + b_e(kku) * wndm(ji,jj) ** p_e(kku)      &
                        + c_e(kku) * (wndm(ji,jj)-8 ) **2) * fe

            ch(ji,jj) = ch(ji,jj) / 1000.0
            ce(ji,jj) = ce(ji,jj) / 1000.0

            IF (wndm(ji,jj)<0.3) THEN
               ch(ji,jj) = 1.3e-03 * fh
               ce(ji,jj) = 1.5e-03 * fe
            ELSE IF(wndm(ji,jj)>50.0) THEN
               ch(ji,jj) = 1.25e-03 * fh
               ce(ji,jj) = 1.30e-03 * fe
            ENDIF

      !!----------------------------------------------------------------------
      !! --- calculates the SENSIBLE HEAT FLUX in MKS ( watt/m*m )
      !!----------------------------------------------------------------------

            HA(ji,jj) = rhom(ji,jj)*cp*ch(ji,jj)*wndm(ji,jj)*deltemp

      !!----------------------------------------------------------------------
      !! ------------------ (iv)  latent heat
      !! --- calculates the LATENT HEAT FLUX  ( watt/m*m )
      !! --- ELAT = L*rho*Ce*|V|*[qs(Ts)-qa(t2d)]
      !!----------------------------------------------------------------------

            esre  = shms(ji,jj) - shnow(ji,jj)   ! --- calculates the term : qs(Ta)-qa(t2d)

            cseep = ce(ji,jj) * wndm(ji,jj) * esre     ! --- calculates the term : Ce*|V|*[qs(Ts)-qa(t2d)]

            evap(ji,jj) = (cseep * rhom(ji,jj))  ! in [kg/m2/sec] !! --- calculates the EVAPORATION RATE [m/yr]

            elat(ji,jj) = rhom(ji,jj) * cseep * heatlat(sst(ji,jj))

      !!----------------------------------------------------------------------
      !! --- calculates the Drag Coefficient
      !!----------------------------------------------------------------------

      !!----------------------------------------------------------------------
      !! --- deltemp should be (Ts - Ta) in the formula estimating
      !! --- drag coefficient
      !!----------------------------------------------------------------------

              IF( .NOT. ln_cdgw ) THEN
                 cdx(ji,jj) = cd_HR(wndm(ji,jj),deltemp)
              ENDIF

          END DO
      END DO

      !!----------------------------------------------------------------------
      !! --- calculates the wind stresses in MKS ( newton/m*m )
      !! ---            taux= rho*Cd*|V|u      tauy= rho*Cd*|V|v
      !!----------------------------------------------------------------------

       taux(:,:)= rhom(:,:) * cdx(:,:) * rspeed(:,:) * rel_windu(:,:)
       tauy(:,:)= rhom(:,:) * cdx(:,:) * rspeed(:,:) * rel_windv(:,:)

      CALL wrk_dealloc( jpi,jpj, rspeed, sh10now, t10now, cdx, ce, shms )
      CALL wrk_dealloc( jpi,jpj, rhom, sstk, ch, rel_windu, rel_windv )
      !
      IF( nn_timing == 1 )  CALL timing_stop('fluxes_mfs')
      !
   END SUBROUTINE fluxes_mfs


      REAL(wp) FUNCTION cd_HR(speed,delt)
      !!----------------------------------------------------------------------
      !! --- calculates the Drag Coefficient as a function of the abs. value
      !! --- of the wind velocity ( Hellermann and Rosenstein )
      !!----------------------------------------------------------------------

       REAL(wp), INTENT(in) :: speed,delt
       REAL(wp), PARAMETER  :: a1=0.934e-3 , a2=0.788e-4, a3=0.868e-4     
       REAL(wp), PARAMETER  :: a4=-0.616e-6, a5=-.120e-5, a6=-.214e-5

        cd_HR = a1 + a2*speed + a3*delt + a4*speed*speed        &
           + a5*delt*delt  + a6*speed*delt

      END FUNCTION cd_HR

      REAL(wp) function HEATLAT(t)
      !!----------------------------------------------------------------------
      !! --- calculates the Latent Heat of Vaporization ( J/kg ) as function of
      !! --- the temperature ( Celsius degrees )
      !! --- ( from A. Gill  pag. 607 )
      !!
      !! --- Constant Latent Heat of Vaporization ( Rosati,Miyakoda 1988 )
      !!     L = 2.501e+6  (MKS)
      !!----------------------------------------------------------------------

      REAL(wp) , intent(in) :: t

      heatlat = 2.5008e+6 -2.3e+3*t

      END FUNCTION HEATLAT


   SUBROUTINE qshort(hour,alat,alon,cldnow,qsw)
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE qshort  ***
      !!
      !! ** Purpose :   Compute Solar Radiation
      !!
      !! ** Method  :   Compute Solar Radiation according Astronomical
      !!                formulae
      !!
      !! References :   Reed RK (1975) and Reed RK (1977)
      !!
      !! Note: alat,alon - (lat, lon)  in radians
      !!----------------------------------------------------------------------
        REAL(wp), INTENT (in) :: hour

        REAL(wp), INTENT(in ), DIMENSION(:,:) :: alat,alon
        REAL(wp), INTENT(in ), DIMENSION(:,:) :: cldnow
        REAL(wp), INTENT(out), DIMENSION(:,:) :: qsw
        REAL(wp), DIMENSION(12) :: alpham

        REAL(wp), PARAMETER ::   eclips=23.439* (3.141592653589793_wp / 180._wp)
        REAL(wp), PARAMETER ::   solar = 1350.
        REAL(wp), PARAMETER ::   tau = 0.7
        REAL(wp), PARAMETER ::   aozone = 0.09
        REAL(wp), PARAMETER ::   yrdays = 360.
        REAL(wp) :: days, th0,th02,th03, sundec, thsun, coszen, qatten
        REAL(wp) :: qzer, qdir,qdiff,qtot,tjul,sunbet
        REAL(wp) :: albedo
        INTEGER :: jj, ji

      !!----------------------------------------------------------------------
      !! --- albedo monthly values from Payne (1972) as means of the values
      !! --- at 40N and 30N for the Atlantic Ocean ( hence the same latitudinal
      !! --- band of the Mediterranean Sea ) :
      !!----------------------------------------------------------------------

        data alpham /0.095,0.08,0.065,0.065,0.06,0.06,0.06,0.06,        &
                    0.065,0.075,0.09,0.10/

      !!----------------------------------------------------------------------
      !!   days is the number of days elapsed until the day=nday_year
      !!----------------------------------------------------------------------
        days = nday_year -1.
        th0  = 2.*rpi*days/yrdays
        th02 = 2.*th0
        th03 = 3.*th0

      !! --- sun declination :
      !!----------------------------------------------------------------------
        sundec = 0.006918 - 0.399912*cos(th0) + 0.070257*sin(th0) -   &
                          0.006758*cos(th02) + 0.000907*sin(th02) -   &
                          0.002697*cos(th03) + 0.001480*sin(th03)

      DO jj = 1, jpj
         DO ji = 1, jpi

      !! --- sun hour angle :
      !!----------------------------------------------------------------------
          thsun = (hour -12.)*15.*rad + alon(ji,jj)

      !! --- cosine of the solar zenith angle :
      !!----------------------------------------------------------------------
          coszen =sin(alat(ji,jj))*sin(sundec)                 &
                    +cos(alat(ji,jj))*cos(sundec)*cos(thsun)

          IF(coszen .LE. 5.035D-04) THEN
            coszen = 0.0
            qatten = 0.0
          ELSE
            qatten = tau**(1./coszen)
          END IF

          qzer  = coszen * solar *                                 &
                  (1.0+1.67E-2*cos(rpi*2.*(days-3.0)/365.0))**2
          qdir  = qzer * qatten
          qdiff = ((1.-aozone)*qzer - qdir) * 0.5
          qtot  =  qdir + qdiff
          tjul = (days -81.)*rad

      !! --- sin of the solar noon altitude in radians :
      !!----------------------------------------------------------------------
          sunbet=sin(alat(ji,jj))*sin(eclips*sin(tjul)) +   &
                 cos(alat(ji,jj))*cos(eclips*sin(tjul))

      !! --- solar noon altitude in degrees :
      !!----------------------------------------------------------------------

         sunbet = asin(sunbet)/rad

      !!----------------------------------------------------------------------
      !! --- calculates the albedo according to Payne (1972)
      !!----------------------------------------------------------------------

         albedo = alpham(nmonth)

      !!----------------------------------------------------------------------
      !! --- ( radiation as from Reed(1977), Simpson and Paulson(1979) )
      !! --- calculates SHORT WAVE FLUX ( watt/m*m )
      !! --- ( Rosati,Miyakoda 1988 ; eq. 3.8 )
      !!----------------------------------------------------------------------

          IF(cldnow(ji,jj).LT.0.3) THEN
             qsw(ji,jj) = qtot * (1.-albedo)
          ELSE
             qsw(ji,jj) = qtot*(1.-0.62*cldnow(ji,jj)              &
                                + .0019*sunbet)*(1.-albedo)
          ENDIF

         END DO
      END DO

   END SUBROUTINE qshort


   !!======================================================================

END MODULE sbcblk_mfs
