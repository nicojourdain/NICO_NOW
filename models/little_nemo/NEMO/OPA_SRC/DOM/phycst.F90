MODULE phycst
   !!======================================================================
   !!                    ***  MODULE  phycst  ***
   !!     Definition of of both ocean and ice parameters used in the code
   !!=====================================================================
   !! History :   OPA  !  1990-10  (C. Levy - G. Madec)  Original code
   !!             8.1  !  1991-11  (G. Madec, M. Imbard)  cosmetic changes
   !!   NEMO      1.0  !  2002-08  (G. Madec, C. Ethe)  F90, add ice constants
   !!              -   !  2006-08  (G. Madec)  style 
   !!             3.2  !  2006-08  (S. Masson, G. Madec)  suppress useless variables + style 
   !!             3.4  !  2011-11  (C. Harris)  minor changes for CICE constants 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   phy_cst  : define and print physical constant and domain parameters
   !!----------------------------------------------------------------------
   USE par_oce          ! ocean parameters
   USE in_out_manager   ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   phy_cst     ! routine called by inipar.F90

   REAL(wp), PUBLIC ::   rpi = 3.141592653589793_wp             !: pi
   REAL(wp), PUBLIC ::   rad = 3.141592653589793_wp / 180._wp   !: conversion from degre into radian
   REAL(wp), PUBLIC ::   rsmall = 0.5 * EPSILON( 1.e0 )         !: smallest real computer value
   
   REAL(wp), PUBLIC ::   rday = 24.*60.*60.       !: day (s)
   REAL(wp), PUBLIC ::   rsiyea                   !: sideral year (s)
   REAL(wp), PUBLIC ::   rsiday                   !: sideral day (s)
   REAL(wp), PUBLIC ::   raamo =  12._wp          !: number of months in one year
   REAL(wp), PUBLIC ::   rjjhh =  24._wp          !: number of hours in one day
   REAL(wp), PUBLIC ::   rhhmm =  60._wp          !: number of minutes in one hour
   REAL(wp), PUBLIC ::   rmmss =  60._wp          !: number of seconds in one minute
!! REAL(wp), PUBLIC ::   omega = 7.292115083046061e-5_wp ,  &  !: change the last digit!
   REAL(wp), PUBLIC ::   omega                    !: earth rotation parameter
   REAL(wp), PUBLIC ::   ra    = 6371229._wp      !: earth radius (meter)
   REAL(wp), PUBLIC ::   grav  = 9.80665_wp       !: gravity (m/s2)
   
   REAL(wp), PUBLIC ::   rtt      = 273.16_wp     !: triple point of temperature (Kelvin)
   REAL(wp), PUBLIC ::   rt0      = 273.15_wp     !: freezing point of water (Kelvin)
#if defined key_lim3
   REAL(wp), PUBLIC ::   rt0_snow = 273.16_wp     !: melting point of snow  (Kelvin)
   REAL(wp), PUBLIC ::   rt0_ice  = 273.16_wp     !: melting point of ice   (Kelvin)
#else
   REAL(wp), PUBLIC ::   rt0_snow = 273.15_wp     !: melting point of snow  (Kelvin)
   REAL(wp), PUBLIC ::   rt0_ice  = 273.05_wp     !: melting point of ice   (Kelvin)
#endif

#if defined key_cice
   REAL(wp), PUBLIC ::   rau0     = 1026._wp      !: reference volumic mass (density)  (kg/m3)
#else
   REAL(wp), PUBLIC ::   rau0     = 1035._wp      !: reference volumic mass (density)  (kg/m3)
#endif
   REAL(wp), PUBLIC ::   rau0r                    !: reference specific volume         (m3/kg)
   REAL(wp), PUBLIC ::   rcp      =    4.e+3_wp   !: ocean specific heat
   REAL(wp), PUBLIC ::   ro0cpr                   !: = 1. / ( rau0 * rcp )

#if defined key_lim3 || defined key_cice
   REAL(wp), PUBLIC ::   rcdsn   =   0.31_wp      !: thermal conductivity of snow
   REAL(wp), PUBLIC ::   rcdic   =   2.034396_wp  !: thermal conductivity of fresh ice
   REAL(wp), PUBLIC ::   cpic    = 2067.0         !: specific heat of sea ice
   REAL(wp), PUBLIC ::   lsub    = 2.834e+6       !: pure ice latent heat of sublimation (J.kg-1)
   REAL(wp), PUBLIC ::   lfus    = 0.334e+6       !: latent heat of fusion of fresh ice   (J.kg-1)
   REAL(wp), PUBLIC ::   rhoic   = 917._wp        !: volumic mass of sea ice (kg/m3)
   REAL(wp), PUBLIC ::   tmut    =   0.054        !: decrease of seawater meltpoint with salinity
#else
   REAL(wp), PUBLIC ::   rcdsn   =   0.22_wp      !: conductivity of the snow
   REAL(wp), PUBLIC ::   rcdic   =   2.034396_wp  !: conductivity of the ice
   REAL(wp), PUBLIC ::   rcpsn   =   6.9069e+5_wp !: density times specific heat for snow
   REAL(wp), PUBLIC ::   rcpic   =   1.8837e+6_wp !: volumetric latent heat fusion of sea ice
   REAL(wp), PUBLIC ::   lfus    =   0.3337e+6    !: latent heat of fusion of fresh ice   (J.kg-1) 	
   REAL(wp), PUBLIC ::   xlsn    = 110.121e+6_wp  !: volumetric latent heat fusion of snow
   REAL(wp), PUBLIC ::   xlic    = 300.33e+6_wp   !: volumetric latent heat fusion of ice
   REAL(wp), PUBLIC ::   xsn     =   2.8e+6       !: latent heat of sublimation of snow
   REAL(wp), PUBLIC ::   rhoic   = 900._wp        !: volumic mass of sea ice (kg/m3)
#endif
   REAL(wp), PUBLIC ::   rhosn   = 330._wp        !: volumic mass of snow (kg/m3)
   REAL(wp), PUBLIC ::   emic    =   0.97_wp      !: emissivity of snow or ice
   REAL(wp), PUBLIC ::   sice    =   6.0_wp       !: reference salinity of ice (psu)
   REAL(wp), PUBLIC ::   soce    =  34.7_wp       !: reference salinity of sea (psu)
   REAL(wp), PUBLIC ::   cevap   =   2.5e+6_wp    !: latent heat of evaporation (water)
   REAL(wp), PUBLIC ::   srgamma =   0.9_wp       !: correction factor for solar radiation (Oberhuber, 1974)
   REAL(wp), PUBLIC ::   vkarmn  =   0.4_wp       !: von Karman constant
   REAL(wp), PUBLIC ::   stefan  =   5.67e-8_wp   !: Stefan-Boltzmann constant 
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: phycst.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   
CONTAINS
   
   SUBROUTINE phy_cst
      !!----------------------------------------------------------------------
      !!                       ***  ROUTINE phy_cst  ***
      !!
      !! ** Purpose :   Print model parameters and set and print the constants
      !!----------------------------------------------------------------------
      CHARACTER (len=64) ::   cform = "(A12, 3(A13, I7) )" 
      !!----------------------------------------------------------------------

      !                                   ! Define additional parameters
      rsiyea = 365.25 * rday * 2. * rpi / 6.283076
      rsiday = rday / ( 1. + rday / rsiyea )
#if defined key_cice
      omega =  7.292116e-05
#else
      omega  = 2. * rpi / rsiday 
#endif

      rau0r  = 1. /   rau0  
      ro0cpr = 1. / ( rau0 * rcp )


      IF(lwp) THEN                        ! control print
         WRITE(numout,*)
         WRITE(numout,*) ' phy_cst : initialization of ocean parameters and constants'
         WRITE(numout,*) ' ~~~~~~~'
         WRITE(numout,*) '       Domain info'
         WRITE(numout,*) '          dimension of model'
         WRITE(numout,*) '                 Local domain      Global domain       Data domain '
         WRITE(numout,cform) '            ','   jpi     : ', jpi, '   jpiglo  : ', jpiglo, '   jpidta  : ', jpidta
         WRITE(numout,cform) '            ','   jpj     : ', jpj, '   jpjglo  : ', jpjglo, '   jpjdta  : ', jpjdta
         WRITE(numout,cform) '            ','   jpk     : ', jpk, '   jpk     : ', jpk   , '   jpkdta  : ', jpkdta
         WRITE(numout,*)      '           ','   jpij    : ', jpij
         WRITE(numout,*) '          mpp local domain info (mpp)'
         WRITE(numout,*) '             jpni    : ', jpni, '   jpreci  : ', jpreci
         WRITE(numout,*) '             jpnj    : ', jpnj, '   jprecj  : ', jprecj
         WRITE(numout,*) '             jpnij   : ', jpnij
         WRITE(numout,*) '          lateral domain boundary condition type : jperio  = ', jperio
         WRITE(numout,*)
         WRITE(numout,*) '       Constants'
         WRITE(numout,*)
         WRITE(numout,*) '          mathematical constant                 rpi = ', rpi
         WRITE(numout,*) '          day                                rday   = ', rday,   ' s'
         WRITE(numout,*) '          sideral year                       rsiyea = ', rsiyea, ' s'
         WRITE(numout,*) '          sideral day                        rsiday = ', rsiday, ' s'
         WRITE(numout,*) '          omega                              omega  = ', omega,  ' s-1'
         WRITE(numout,*)
         WRITE(numout,*) '          nb of months per year               raamo = ', raamo, ' months'
         WRITE(numout,*) '          nb of hours per day                 rjjhh = ', rjjhh, ' hours'
         WRITE(numout,*) '          nb of minutes per hour              rhhmm = ', rhhmm, ' mn'
         WRITE(numout,*) '          nb of seconds per minute            rmmss = ', rmmss, ' s'
         WRITE(numout,*)
         WRITE(numout,*) '          earth radius                         ra   = ', ra, ' m'
         WRITE(numout,*) '          gravity                              grav = ', grav , ' m/s^2'
         WRITE(numout,*)
         WRITE(numout,*) '          triple point of temperature      rtt      = ', rtt     , ' K'
         WRITE(numout,*) '          freezing point of water          rt0      = ', rt0     , ' K'
         WRITE(numout,*) '          melting point of snow            rt0_snow = ', rt0_snow, ' K'
         WRITE(numout,*) '          melting point of ice             rt0_ice  = ', rt0_ice , ' K'
         WRITE(numout,*)
         WRITE(numout,*) '          ocean reference volumic mass       rau0   = ', rau0 , ' kg/m^3'
         WRITE(numout,*) '          ocean reference specific volume    rau0r  = ', rau0r, ' m^3/Kg'
         WRITE(numout,*) '          ocean specific heat                rcp    = ', rcp
         WRITE(numout,*) '                       1. / ( rau0 * rcp ) = ro0cpr = ', ro0cpr
         WRITE(numout,*)
         WRITE(numout,*) '          thermal conductivity of the snow          = ', rcdsn   , ' J/s/m/K'
         WRITE(numout,*) '          thermal conductivity of the ice           = ', rcdic   , ' J/s/m/K'
#if defined key_lim3
         WRITE(numout,*) '          fresh ice specific heat                   = ', cpic    , ' J/kg/K'
         WRITE(numout,*) '          latent heat of fusion of fresh ice / snow = ', lfus    , ' J/kg'
         WRITE(numout,*) '          latent heat of subl.  of fresh ice / snow = ', lsub    , ' J/kg'
#elif defined key_cice
         WRITE(numout,*) '          latent heat of fusion of fresh ice / snow = ', lfus    , ' J/kg'
#else
         WRITE(numout,*) '          density times specific heat for snow      = ', rcpsn   , ' J/m^3/K' 
         WRITE(numout,*) '          density times specific heat for ice       = ', rcpic   , ' J/m^3/K'
         WRITE(numout,*) '          volumetric latent heat fusion of sea ice  = ', xlic    , ' J/m' 
         WRITE(numout,*) '          volumetric latent heat fusion of snow     = ', xlsn    , ' J/m' 
         WRITE(numout,*) '          latent heat of sublimation of snow        = ', xsn     , ' J/kg' 
#endif
         WRITE(numout,*) '          density of sea ice                        = ', rhoic   , ' kg/m^3'
         WRITE(numout,*) '          density of snow                           = ', rhosn   , ' kg/m^3'
         WRITE(numout,*) '          emissivity of snow or ice                 = ', emic  
         WRITE(numout,*) '          salinity of ice                           = ', sice    , ' psu'
         WRITE(numout,*) '          salinity of sea                           = ', soce    , ' psu'
         WRITE(numout,*) '          latent heat of evaporation (water)        = ', cevap   , ' J/m^3' 
         WRITE(numout,*) '          correction factor for solar radiation     = ', srgamma 
         WRITE(numout,*) '          von Karman constant                       = ', vkarmn 
         WRITE(numout,*) '          Stefan-Boltzmann constant                 = ', stefan  , ' J/s/m^2/K^4'
         WRITE(numout,*)
         WRITE(numout,*) '          conversion: degre ==> radian          rad = ', rad
         WRITE(numout,*)
         WRITE(numout,*) '          smallest real computer value       rsmall = ', rsmall
      ENDIF

   END SUBROUTINE phy_cst

   !!======================================================================
END MODULE phycst
