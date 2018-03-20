MODULE sed
   !!======================================================================
   !!                        ***  sed  ***
   !! Sediment :   set sediment global variables
   !!======================================================================
#if defined key_sed
   !! History :
   !!        !  06-12  (C. Ethe)  Orignal
   !!----------------------------------------------------------------------
   USE par_sed
   USE in_out_manager

   IMPLICIT NONE
   PUBLIC

   PUBLIC sed_alloc   

   USE dom_oce , ONLY :   nidom     =>   nidom          !:
   USE dom_oce , ONLY :   glamt     =>   glamt          !: longitude of t-point (degre)
   USE dom_oce , ONLY :   gphit     =>   gphit          !: latitude  of t-point (degre)
   USE dom_oce , ONLY :   e3t_0     =>   e3t_0          !: reference depth of t-points (m)
   USE dom_oce , ONLY :   mbkt      =>   mbkt           !: vertical index of the bottom last T- ocean level
   USE dom_oce , ONLY :   tmask     =>   tmask          !: land/ocean mask at t-points
   USE dom_oce , ONLY :   rdt       =>   rdt            !: time step for the dynamics
   USE dom_oce , ONLY :   nyear     =>   nyear          !: Current year
   USE dom_oce , ONLY :   nmonth    =>   nmonth         !: Current month
   USE dom_oce , ONLY :   nday      =>   nday           !: Current day
   USE dom_oce , ONLY :   ndastp    =>   ndastp         !: time step date in year/month/day aammjj
   USE dom_oce , ONLY :   nday_year =>   nday_year      !: curent day counted from jan 1st of the current year
   USE dom_oce , ONLY :   adatrj    =>   adatrj         !: number of elapsed days since the begining of the run
   !                                !: it is the accumulated duration of previous runs
   !                                !: that may have been run with different time steps.

#if ! defined key_sed_off

   USE oce     , ONLY :  tsn        =>   tsn             !: pot. temperature (celsius) and salinity (psu)

   USE trc     , ONLY :  trn        =>   trc             !: tracer 
   USE trc     , ONLY :  nwritetrc  =>   nwritetrc       !: outputs frequency of tracer model

   USE p4zsink , ONLY :  sinking    =>   sinking         !: sinking flux for POC
#if ! defined key_kriest
   USE p4zsink , ONLY :  sinking2   =>   sinking2        !: sinking flux for GOC
#endif
   USE p4zsink , ONLY :  sinkcal    =>   sinkcal         !: sinking flux for calcite
   USE p4zsink , ONLY :  sinksil    =>   sinksil         !: sinking flux for opal ( dsi )

   USE sms_pisces, ONLY : akb3      =>   akb3            !: Chemical constants  
   USE sms_pisces, ONLY : ak13      =>   ak13            !: Chemical constants  
   USE sms_pisces, ONLY : ak23      =>   ak23            !: Chemical constants  
   USE sms_pisces, ONLY : akw3      =>   akw3            !: Chemical constants  
   USE sms_pisces, ONLY : aksp      =>   aksp            !: Chemical constants  
   USE sms_pisces, ONLY : borat     =>   borat           !: Chemical constants ( borat ) 

#endif   


   !! Namelist
   REAL(wp), PUBLIC, DIMENSION(5) ::  reac                !: reactivity rc in  [l.mol-1.s-1]
   REAL(wp), PUBLIC               ::  reac_sil            !: reactivity of silicate in  [l.mol-1.s-1]
   REAL(wp), PUBLIC               ::  reac_clay           !: reactivity of clay in  [l.mol-1.s-1]
   REAL(wp), PUBLIC               ::  reac_poc            !: reactivity of poc in  [l.mol-1.s-1]
   REAL(wp), PUBLIC               ::  reac_no3            !: reactivity of no3 in  [l.mol-1.s-1]
   REAL(wp), PUBLIC               ::  reac_cal            !: reactivity of cal in  [l.mol-1.s-1]
   REAL(wp), PUBLIC               ::  sat_sil             !: saturation concentration for silicate in [mol.l-1]
   REAL(wp), PUBLIC               ::  sat_clay            !: saturation concentration for clay in [mol.l-1]
   REAL(wp), PUBLIC               ::  so2ut 
   REAL(wp), PUBLIC               ::  srno3 
   REAL(wp), PUBLIC               ::  spo4r 
   REAL(wp), PUBLIC               ::  srDnit 
   REAL(wp), PUBLIC               ::  sthro2              !: threshold O2 concen. in [mol.l-1]
   REAL(wp), PUBLIC               ::  pdb = 0.0112372     !: 13C/12C in PD Belemnite
   REAL(wp), PUBLIC               ::  rc13P  = 0.980      !: 13C/12C in POC = rc13P*PDB
   REAL(wp), PUBLIC               ::  rc13Ca = 1.001      !: 13C/12C in CaCO3 = rc13Ca*PDB
   REAL(wp), PUBLIC               ::  dtsed               !: sedimentation time step
   REAL(wp), PUBLIC               ::  db                  !: bioturb coefficient in [cm2.s-1]

   INTEGER , PUBLIC               ::  nitsed000
   INTEGER , PUBLIC               ::  nitsedend
   INTEGER , PUBLIC               ::  nwrised
   INTEGER , PUBLIC               ::  nfreq
   REAL(wp), PUBLIC               ::  dens                !: density of solid material
   !
   REAL(wp), PUBLIC, DIMENSION(:,:,:), ALLOCATABLE ::  pwcp       !: pore water sediment data at given time-step
   REAL(wp), PUBLIC, DIMENSION(:,:,:), ALLOCATABLE ::  pwcp0      !: pore water sediment data at initial time
   REAL(wp), PUBLIC, DIMENSION(:,:,:), ALLOCATABLE ::  solcp      !: solid sediment data at given time-step
   REAL(wp), PUBLIC, DIMENSION(:,:,:), ALLOCATABLE ::  solcp0     !: solid sediment data at initial time

   !! * Shared module variables
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  pwcp_dta   !: pore water data at given time-step
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  rainrm_dta !: rain data at at initial time
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  rainrm     !: rain data at given time-step
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  rainrg     !: rain of each solid component in [g/(cm**2.s)]
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  fromsed    !:
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  tosed      !:
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  rloss      !: 
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  tokbot        
   !
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  temp       !: temperature
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  salt       !: salinity
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  press      !: pressure
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  raintg     !: total massic flux rained in each cell (sum of sol. comp.)
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  dzdep      !: total thickness of solid material rained [cm] in each cell
   !
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  hipor      !: [h+] in mol/kg*densSW 
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  co3por     !: [co3--]solid sediment at initial time
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  dz3d       !:  ???
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  volw3d     !:  ???
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  vols3d     !:  ???


   !! Chemistry
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  densSW 
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  borats 
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  calcon2
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  akbs  
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  ak1s 
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  ak2s   
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  akws  
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  ak12s  
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  ak1ps 
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  ak2ps  
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  ak3ps 
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  ak12ps 
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  ak123ps
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  aksis 
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  aksps 

   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  mol_wgt    !: molecular weight of solid sediment data
 
   REAL(wp), PUBLIC, DIMENSION(:,:,:), ALLOCATABLE ::  trc_data    !: tracer data to share with sediment model
   !! Geometry
   INTEGER , PUBLIC, SAVE                          ::  jpoce, indoce !: Ocean points ( number/indices )
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  iarroce       !: Computation of 1D array of sediments points
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  epkbot        !: ocean bottom layer thickness
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  dzkbot        !: ocean bottom layer thickness in meters
   REAL(wp), PUBLIC, DIMENSION(:,:,:), ALLOCATABLE ::  tmasksed      !: sediment mask
   REAL(wp), PUBLIC, DIMENSION(:,:  ), ALLOCATABLE ::  sbathy        !: bathymetry
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  dz            !: sediment layers thickness
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  por           !: porosity profile     
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  por1          !: 1-por 
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  profsed       !: depth of middle of each layer
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  volw          !: volume of pore water cell fraction
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  vols          !: volume of solid cell fraction
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  diff          !: diffusion ceofficient
   REAL(wp), PUBLIC, DIMENSION(:    ), ALLOCATABLE ::  rdtsed        !:  sediment model time-step
   REAL(wp)  ::   dens               !: density of solid material
   !! Inputs / Outputs
   CHARACTER( len = 80 ), DIMENSION(jptrased  ) ::  sedtrcl
   CHARACTER( len = 20 ), DIMENSION(jptrased  ) ::  sedtrcd , sedtrcu
   CHARACTER( len = 80 ), DIMENSION(jpdia3dsed) ::  seddia3l 
   CHARACTER( len = 20 ), DIMENSION(jpdia3dsed) ::  seddia3d, seddia3u
   CHARACTER( len = 80 ), DIMENSION(jpdia2dsed) ::  seddia2l 
   CHARACTER( len = 20 ), DIMENSION(jpdia2dsed) ::  seddia2d, seddia2u
   !
   REAL(wp), PUBLIC, DIMENSION(:,:,:,:), ALLOCATABLE ::  trcsedi
   REAL(wp), PUBLIC, DIMENSION(:,:,:,:), ALLOCATABLE ::  flxsedi3d
   REAL(wp), PUBLIC, DIMENSION(:,:,:  ), ALLOCATABLE ::  flxsedi2d

   INTEGER, PUBLIC ::  numsed = 27    ! units

CONTAINS

   INTEGER FUNCTION sed_alloc()
      !!-------------------------------------------------------------------
      !!                    *** ROUTINE sed_alloc ***
      !!-------------------------------------------------------------------
      USE lib_mpp, ONLY: ctl_warn
      !!-------------------------------------------------------------------
      !
      ALLOCATE( trc_dta(jpi,jpj,jdta)                                     ,   &
         &      epkbot(jpi,jpj), sbathy(jpi,jpj)                          ,   &
         &      tmasksed(jpi,jpj,jpksed)                                  ,   &
         &      dz(jpksed)  , por(jpksed) , por1(jpksed), profsed(jpksed) ,   &
         &      volw(jpksed), vols(jpksed), diff(jpksed), rdtsed(jpksed)  ,   &
         &      trcsedi  (jpi,jpj,jpksed,jptrased)                        ,   &
         &      flxsedi3d(jpi,jpj,jpksed,jpdia3dsed)                      ,   &
         &      flxsedi2d(jpi,jpj,jpksed,jpdia2dsed)                      ,   &
         &      mol_wgt(jpsol),                                           STAT=sed_alloc )

      IF( sed_alloc /= 0 )   CALL ctl_warn('sed_alloc: failed to allocate arrays')
      !
   END FUNCTION sed_alloc

#else
   !!======================================================================
   !! No Sediment model
   !!======================================================================
#endif

END MODULE sed
