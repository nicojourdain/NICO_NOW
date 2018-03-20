MODULE ice
   !!======================================================================
   !!                        ***  MODULE ice  ***
   !! LIM-3 Sea Ice physics:  diagnostics variables of ice defined in memory
   !!=====================================================================
   !! History :  3.0  ! 2008-03  (M. Vancoppenolle) original code LIM-3
   !!            4.0  ! 2011-02  (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3' :                                   LIM3 sea-ice model
   !!----------------------------------------------------------------------
   USE par_ice          ! LIM sea-ice parameters
   USE in_out_manager   ! I/O manager
   USE lib_mpp         ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC    ice_alloc  !  Called in iceini.F90

   !!======================================================================
   !! LIM3 by the use of sweat, agile fingers and sometimes brain juice, 
   !!  was developed in Louvain-la-Neuve by : 
   !!    * Martin Vancoppenolle (UCL-ASTR, Belgium)
   !!    * Sylvain Bouillon (UCL-ASTR, Belgium)
   !!    * Miguel Angel Morales Maqueda (NOC-L, UK)
   !! 
   !! Based on extremely valuable earlier work by
   !!    * Thierry Fichefet
   !!    * Hugues Goosse
   !!
   !! The following persons also contributed to the code in various ways
   !!    * Gurvan Madec, Claude Talandier, Christian Ethe (LOCEAN, France)
   !!    * Xavier Fettweis (UCL-ASTR), Ralph Timmermann (AWI, Germany)
   !!    * Bill Lipscomb (LANL), Cecilia Bitz (UWa) 
   !!      and Elisabeth Hunke (LANL), USA.
   !! 
   !! For more info, the interested user is kindly invited to consult the following references
   !!    For model description and validation :
   !!    * Vancoppenolle et al., Ocean Modelling, 2008a.
   !!    * Vancoppenolle et al., Ocean Modelling, 2008b.
   !!    For a specific description of EVP :
   !!    * Bouillon et al., Ocean Modelling 2009.
   !!
   !!    Or the reference manual, that should be available by 2011
   !!======================================================================
   !!                                                                     |
   !!              I C E   S T A T E   V A R I A B L E S                  |
   !!                                                                     |
   !! Introduction :                                                      |
   !! --------------                                                      |
   !! Every ice-covered grid cell is characterized by a series of state   |
   !! variables. To account for unresolved spatial variability in ice     |
   !! thickness, the ice cover in divided in ice thickness categories.    |
   !!                                                                     |
   !! Sea ice state variables depend on the ice thickness category        |
   !!                                                                     |
   !! Those variables are divided into two groups                         |
   !! * Extensive (or global) variables.                                  |
   !!   These are the variables that are transported by all means         |
   !! * Intensive (or equivalent) variables.                              |
   !!   These are the variables that are either physically more           |
   !!   meaningful and/or used in ice thermodynamics                      |
   !!                                                                     |
   !! Routines in limvar.F90 perform conversions                          |
   !!  - lim_var_glo2eqv  : from global to equivalent variables           |
   !!  - lim_var_eqv2glo  : from equivalent to global variables           |
   !!                                                                     |
   !! For various purposes, the sea ice state variables have sometimes    |
   !! to be aggregated over all ice thickness categories. This operation  |
   !! is done in :                                                        |
   !!  - lim_var_agg                                                      |
   !!                                                                     |
   !! in icestp.F90, the routines that compute the changes in the ice     |
   !! state variables are called                                          |
   !! - lim_dyn : ice dynamics                                            |
   !! - lim_trp : ice transport                                           |
   !! - lim_itd_me : mechanical redistribution (ridging and rafting)      |
   !! - lim_thd : ice halo-thermodynamics                                 |
   !! - lim_itd_th : thermodynamic changes in ice thickness distribution  |
   !!                and creation of new ice                              |
   !!                                                                     |
   !! See the associated routines for more information                    |
   !!                                                                     |
   !! List of ice state variables :                                       |
   !! -----------------------------                                       |
   !!                                                                     |
   !!-------------|-------------|---------------------------------|-------|
   !!   name in   |   name in   |              meaning            | units |
   !! 2D routines | 1D routines |                                 |       |
   !!-------------|-------------|---------------------------------|-------|
   !!                                                                     |
   !! ******************************************************************* |
   !! ***         Dynamical variables (prognostic)                    *** |
   !! ******************************************************************* |
   !!                                                                     |
   !! u_ice       |      -      |    Comp. U of the ice velocity  | m/s   |
   !! v_ice       |      -      |    Comp. V of the ice velocity  | m/s   |
   !!                                                                     |
   !! ******************************************************************* |
   !! ***         Category dependent state variables (prognostic)     *** |
   !! ******************************************************************* |
   !!                                                                     |
   !! ** Global variables                                                 |
   !!-------------|-------------|---------------------------------|-------|
   !! a_i         | a_i_b       |    Ice concentration            |       |
   !! v_i         |      -      |    Ice volume per unit area     | m     |
   !! v_s         |      -      |    Snow volume per unit area    | m     |
   !! smv_i       |      -      |    Sea ice salt content         | ppt.m |
   !! oa_i        !      -      !    Sea ice areal age content    | day   |
   !! e_i         !      -      !    Ice enthalpy                 | 10^9 J| 
   !!      -      ! q_i_b       !    Ice enthalpy per unit vol.   | J/m3  | 
   !! e_s         !      -      !    Snow enthalpy                | 10^9 J| 
   !!      -      ! q_s_b       !    Snow enthalpy per unit vol.  | J/m3  | 
   !!                                                                     |
   !!-------------|-------------|---------------------------------|-------|
   !!                                                                     |
   !! ** Equivalent variables                                             |
   !!-------------|-------------|---------------------------------|-------|
   !!                                                                     |
   !! ht_i        | ht_i_b      |    Ice thickness                | m     |
   !! ht_s        ! ht_s_b      |    Snow depth                   | m     |
   !! sm_i        ! sm_i_b      |    Sea ice bulk salinity        ! ppt   |
   !! s_i         ! s_i_b       |    Sea ice salinity profile     ! ppt   |
   !! o_i         !      -      |    Sea ice Age                  ! days  |
   !! t_i         ! t_i_b       |    Sea ice temperature          ! K     |
   !! t_s         ! t_s_b       |    Snow temperature             ! K     |
   !! t_su        ! t_su_b      |    Sea ice surface temperature  ! K     |
   !!                                                                     |
   !! notes: the ice model only sees a bulk (i.e., vertically averaged)   |
   !!        salinity, except in thermodynamic computations, for which    |
   !!        the salinity profile is computed as a function of bulk       |
   !!        salinity                                                     |
   !!                                                                     |
   !!        the sea ice surface temperature is not associated to any     |
   !!        heat content. Therefore, it is not a state variable and      |
   !!        does not have to be advected. Nevertheless, it has to be     |
   !!        computed to determine whether the ice is melting or not      |
   !!                                                                     |
   !! ******************************************************************* |
   !! ***         Category-summed state variables (diagnostic)        *** |
   !! ******************************************************************* |
   !! at_i        | at_i_b      |    Total ice concentration      |       |
   !! vt_i        |      -      |    Total ice vol. per unit area | m     |
   !! vt_s        |      -      |    Total snow vol. per unit ar. | m     |
   !! smt_i       |      -      |    Mean sea ice salinity        | ppt   |
   !! tm_i        |      -      |    Mean sea ice temperature     | K     |
   !! ot_i        !      -      !    Sea ice areal age content    | day   |
   !! et_i        !      -      !    Total ice enthalpy           | 10^9 J| 
   !! et_s        !      -      !    Total snow enthalpy          | 10^9 J| 
   !! bv_i        !      -      !    Mean relative brine volume   | ???   | 
   !!=====================================================================

   LOGICAL, PUBLIC ::   con_i = .false.   ! switch for conservation test

   !!--------------------------------------------------------------------------
   !! * Share Module variables
   !!--------------------------------------------------------------------------
   INTEGER , PUBLIC ::   nstart    !: iteration number of the begining of the run 
   INTEGER , PUBLIC ::   nlast     !: iteration number of the end of the run 
   INTEGER , PUBLIC ::   nitrun    !: number of iteration
   INTEGER , PUBLIC ::   numit     !: iteration number
   REAL(wp), PUBLIC ::   rdt_ice   !: ice time step

   !                                          !!** ice-dynamic namelist (namicedyn) **
   INTEGER , PUBLIC ::   nbiter = 1            !: number of sub-time steps for relaxation
   INTEGER , PUBLIC ::   nbitdr = 250          !: maximum number of iterations for relaxation
   INTEGER , PUBLIC ::   nevp   = 400          !: number of iterations for subcycling
   INTEGER , PUBLIC ::   nlay_i = 5            !: number of layers in the ice

   !                                          !!** ice-dynamic namelist (namicedyn) **
   REAL(wp), PUBLIC ::   epsd   = 1.0e-20_wp   !: tolerance parameter for dynamic
   REAL(wp), PUBLIC ::   alpha  = 0.5_wp       !: coefficient for semi-implicit coriolis
   REAL(wp), PUBLIC ::   dm     = 0.6e+03_wp   !: diffusion constant for dynamics
   REAL(wp), PUBLIC ::   om     = 0.5_wp       !: relaxation constant
   REAL(wp), PUBLIC ::   resl   = 5.0e-05_wp   !: maximum value for the residual of relaxation
   REAL(wp), PUBLIC ::   cw     = 5.0e-03_wp   !: drag coefficient for oceanic stress
   REAL(wp), PUBLIC ::   angvg  = 0._wp        !: turning angle for oceanic stress
   REAL(wp), PUBLIC ::   pstar  = 1.0e+04_wp   !: determines ice strength (N/M), Hibler JPO79
   REAL(wp), PUBLIC ::   c_rhg  = 20._wp       !: determines changes in ice strength
   REAL(wp), PUBLIC ::   etamn  = 0.0e+07_wp   !: minimun value for viscosity : has to be 0
   REAL(wp), PUBLIC ::   creepl = 2.0e-08_wp   !: creep limit : has to be under 1.0e-9
   REAL(wp), PUBLIC ::   ecc    = 2._wp        !: eccentricity of the elliptical yield curve
   REAL(wp), PUBLIC ::   ahi0   = 350._wp      !: sea-ice hor. eddy diffusivity coeff. (m2/s)
   REAL(wp), PUBLIC ::   telast = 2880._wp     !: timescale for elastic waves (s) !SB
   REAL(wp), PUBLIC ::   alphaevp = 1._wp      !: coeficient of the internal stresses !SB
   REAL(wp), PUBLIC ::   unit_fac = 1.e+09_wp  !: conversion factor for ice / snow enthalpy

   !                                              !!** ice-salinity namelist (namicesal) **
   REAL(wp), PUBLIC ::   s_i_max  = 20.0_wp        !: maximum ice salinity [PSU]
   REAL(wp), PUBLIC ::   s_i_min  =  0.1_wp        !: minimum ice salinity [PSU]
   REAL(wp), PUBLIC ::   s_i_0    =  3.5_wp        !: 1st sal. value for the computation of sal .prof. [PSU]
   REAL(wp), PUBLIC ::   s_i_1    =  4.5_wp        !: 2nd sal. value for the computation of sal .prof. [PSU]
   REAL(wp), PUBLIC ::   sal_G    =  5.0_wp        !: restoring salinity for gravity drainage [PSU]
   REAL(wp), PUBLIC ::   sal_F    =  2.5_wp        !: restoring salinity for flushing [PSU]
   REAL(wp), PUBLIC ::   time_G   =  1.728e+06_wp  !: restoring time constant for gravity drainage (= 20 days) [s]
   REAL(wp), PUBLIC ::   time_F   =  8.640e+05_wp  !: restoring time constant for gravity drainage (= 10 days) [s]
   REAL(wp), PUBLIC ::   bulk_sal =  4.0_wp        !: bulk salinity (ppt) in case of constant salinity

   !                                              !!** ice-salinity namelist (namicesal) **
   INTEGER , PUBLIC ::   num_sal     = 1           !: salinity configuration used in the model
   !                                               ! 1 - s constant in space and time
   !                                               ! 2 - prognostic salinity (s(z,t))
   !                                               ! 3 - salinity profile, constant in time
   !                                               ! 4 - salinity variations affect only ice thermodynamics
   INTEGER , PUBLIC ::   sal_prof    = 1           !: salinity profile or not 
   INTEGER , PUBLIC ::   thcon_i_swi = 1           !: thermal conductivity: =1 Untersteiner (1964) ; =2 Pringle et al (2007)

   !                                              !!** ice-mechanical redistribution namelist (namiceitdme)
   REAL(wp), PUBLIC ::   Cs        = 0.25_wp       !: fraction of shearing energy contributing to ridging            
   REAL(wp), PUBLIC ::   Cf        = 17.0_wp       !: ratio of ridging work to PE loss
   REAL(wp), PUBLIC ::   fsnowrdg  = 0.5_wp        !: fractional snow loss to the ocean during ridging
   REAL(wp), PUBLIC ::   fsnowrft  = 0.5_wp        !: fractional snow loss to the ocean during ridging
   REAL(wp), PUBLIC ::   Gstar     = 0.15_wp       !: fractional area of young ice contributing to ridging
   REAL(wp), PUBLIC ::   astar     = 0.05_wp       !: equivalent of G* for an exponential participation function
   REAL(wp), PUBLIC ::   Hstar     = 100.0_wp      !: thickness that determines the maximal thickness of ridged ice
   REAL(wp), PUBLIC ::   hparmeter = 0.75_wp       !: threshold thickness (m) for rafting / ridging 
   REAL(wp), PUBLIC ::   Craft     = 5.0_wp        !: coefficient for smoothness of the hyperbolic tangent in rafting
   REAL(wp), PUBLIC ::   ridge_por = 0.0_wp        !: initial porosity of ridges (0.3 regular value)
   REAL(wp), PUBLIC ::   sal_max_ridge = 15.0_wp   !: maximum ridged ice salinity (ppt)
   REAL(wp), PUBLIC ::   betas    = 1.0_wp         !: coef. for partitioning of snowfall between leads and sea ice
   REAL(wp), PUBLIC ::   kappa_i  = 1.0_wp         !: coef. for the extinction of radiation Grenfell et al. (2006) [1/m]
   REAL(wp), PUBLIC ::   nconv_i_thd = 50_wp       !: maximal number of iterations for heat diffusion
   REAL(wp), PUBLIC ::   maxer_i_thd = 1.0e-4_wp   !: maximal tolerated error (C) for heat diffusion

   !                                              !!** ice-mechanical redistribution namelist (namiceitdme)
   INTEGER , PUBLIC ::   ridge_scheme_swi = 0      !: scheme used for ice ridging
   INTEGER , PUBLIC ::   raftswi          = 1      !: rafting of ice or not                        
   INTEGER , PUBLIC ::   partfun_swi      = 1      !: participation function: =0 Thorndike et al. (1975), =1 Lipscomb et al. (2007)
   INTEGER , PUBLIC ::   transfun_swi     = 0      !: transfer function: =0 Hibler 1980, =1 Lipscomb et al. 2007
   INTEGER , PUBLIC ::   brinstren_swi    = 0      !: use brine volume to diminish ice strength

   REAL(wp), PUBLIC ::   usecc2           !:  = 1.0 / ( ecc * ecc )
   REAL(wp), PUBLIC ::   rhoco            !: = rau0 * cw
   REAL(wp), PUBLIC ::   sangvg, cangvg   !: sin and cos of the turning angle for ocean stress
   REAL(wp), PUBLIC ::   pstarh           !: pstar / 2.0

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   u_oce, v_oce   !: surface ocean velocity used in ice dynamics
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ahiu , ahiv    !: hor. diffusivity coeff. at U- and V-points [m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   pahu , pahv    !: ice hor. eddy diffusivity coef. at U- and V-points
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ust2s, hicol   !: friction velocity, ice collection thickness accreted in leads
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   strp1, strp2   !: strength at previous time steps
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   strength       !: ice strength
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   stress1_i, stress2_i, stress12_i   !: 1st, 2nd & diagonal stress tensor element
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   delta_i        !: ice rheology elta factor (Flato & Hibler 95) [s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   divu_i         !: Divergence of the velocity field [s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   shear_i        !: Shear of the velocity field [s-1]
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   firic       !: IR flux over the ice (diag only)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fcsic       !: Sensible heat flux over the ice (diag only)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fleic       !: Latent heat flux over the ice (diag only)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qlatic      !: latent flux
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   rdvosif     !: Variation of volume at surface (diag only)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   rdvobif     !: Variation of ice volume at the bottom ice (diag only)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fdvolif     !: Total variation of ice volume (diag only)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   rdvonif     !: Lateral Variation of ice volume (diag only)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sist        !: Average Sea-Ice Surface Temperature [Kelvin]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   icethi      !: total ice thickness (for all categories) (diag only)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   t_bo        !: Sea-Ice bottom temperature [Kelvin]     
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hicifp      !: Ice production/melting==>!obsolete... can be removed
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   frld        !: Leads fraction = 1 - ice fraction
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   pfrld       !: Leads fraction at previous time  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   phicif      !: Old ice thickness
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fbif        !: Heat flux at the ice base
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   rdmsnif     !: Variation of snow mass
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   rdmicif     !: Variation of ice mass
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qldif       !: heat balance of the lead (or of the open ocean)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qcmif       !: Energy needed to bring the ocean to freezing 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fdtcn       !: net downward heat flux from the ice to the ocean
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qdtcn       !: energy from the ice to the ocean
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fstric      !: transmitted solar radiation under ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fscmbq      !: associated with lead chipotage with solar flux
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ffltbif     !: related to max heat contained in brine pockets (?)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fsbbq       !: Also linked with the solar flux below the ice (?)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qfvbq       !: store energy in case of total lateral ablation (?)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   dmgwi       !: Variation of the mass of snow ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fsalt_res   !: Residual salt flux due to correction of ice thickness
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fsbri       !: Salt flux due to brine rejection
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fsalt_rpo   !: Salt flux associated with porous ridged ice formation
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fheat_rpo   !: Heat flux associated with porous ridged ice formation
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fhbri       !: heat flux due to brine rejection
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fmmec       !: Mass flux due to snow loss during compression
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fseqv       !: Equivalent salt flux due to ice growth/melt
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fhmec       !: Heat flux due to snow loss during compression
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fheat_res   !: Residual heat flux due to correction of ice thickness

   ! temporary arrays for dummy version of the code
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   dh_i_surf2D, dh_i_bott2D, fstbif, fsup2D, focea2D, q_s

   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   patho_case ! number of the pathological case (if any, of course)

   !!--------------------------------------------------------------------------
   !! * Ice global state variables
   !!--------------------------------------------------------------------------
   !! Variables defined for each ice category
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ht_i    !: Ice thickness (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   a_i     !: Ice fractional areas (concentration)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   v_i     !: Ice volume per unit area (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   v_s     !: Snow volume per unit area(m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ht_s    !: Snow thickness (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   t_su    !: Sea-Ice Surface Temperature (K)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   sm_i    !: Sea-Ice Bulk salinity (ppt)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   smv_i   !: Sea-Ice Bulk salinity times volume per area (ppt.m)
   !                                                                  !  this is an extensive variable that has to be transported
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   o_i     !: Sea-Ice Age (days)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ov_i    !: Sea-Ice Age times volume per area (days.m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   oa_i    !: Sea-Ice Age times ice area (days)

   !! Variables summed over all categories, or associated to all the ice in a single grid cell
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   u_ice, v_ice   !: components of the ice velocity (m/s)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   tio_u, tio_v   !: components of the ice-ocean stress (N/m2)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   vt_i , vt_s    !: ice and snow total volume per unit area (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   at_i           !: ice total fractional area (ice concentration)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ato_i          !: =1-at_i ; total open water fractional area
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   et_i , et_s    !: ice and snow total heat content
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ot_i           !: mean age over all categories
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   tm_i           !: mean ice temperature over all categories
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   bv_i           !: brine volume averaged over all categories
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   smt_i          !: mean sea ice salinity averaged over all categories [PSU]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   at_i_typ     !: total area   contained in each ice type [m^2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   vt_i_typ     !: total volume contained in each ice type [m^3]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   t_s        !: Snow temperatures [K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   e_s        !: Snow ...      

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   e_i_cat    !: ! go to trash
      
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   t_i        !: ice temperatures          [K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   e_i        !: ice thermal contents [Giga J]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   s_i        !: ice salinities          [PSU]

   !!--------------------------------------------------------------------------
   !! * Moments for advection
   !!--------------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   sxopw, syopw, sxxopw, syyopw, sxyopw   !: open water in sea ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   sxice, syice, sxxice, syyice, sxyice   !: ice thickness 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   sxsn , sysn , sxxsn , syysn , sxysn    !: snow thickness
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   sxa  , sya  , sxxa  , syya  , sxya     !: lead fraction
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   sxc0 , syc0 , sxxc0 , syyc0 , sxyc0    !: snow thermal content
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   sxsal, sysal, sxxsal, syysal, sxysal   !: ice salinity
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   sxage, syage, sxxage, syyage, sxyage   !: ice age
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   sxe  , sye  , sxxe  , syye  , sxye     !: ice layers heat content

   !!--------------------------------------------------------------------------
   !! * Old values of global variables
   !!--------------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   old_v_s, old_v_i               !: snow and ice volumes
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   old_a_i, old_smv_i, old_oa_i   !: ???
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   old_e_s                        !: snow heat content
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   old_e_i                        !: ice temperatures
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   old_u_ice, old_v_ice           !: ice velocity (gv6 and gv7)
      

   !!--------------------------------------------------------------------------
   !! * Increment of global variables
   !!--------------------------------------------------------------------------
   ! thd refers to changes induced by thermodynamics
   ! trp   ''         ''     ''       advection (transport of ice)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   d_a_i_thd  , d_a_i_trp                 !: icefractions                  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   d_v_s_thd  , d_v_s_trp                 !: snow volume
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   d_v_i_thd  , d_v_i_trp                 !: ice  volume
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   d_smv_i_thd, d_smv_i_trp               !:     
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   d_sm_i_fl  , d_sm_i_gd                 !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   d_sm_i_se  , d_sm_i_si  , d_sm_i_la    !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   d_oa_i_thd , d_oa_i_trp , s_i_newice   !:

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   d_e_s_thd  , d_e_s_trp     !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   d_e_i_thd  , d_e_i_trp     !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   d_u_ice_dyn, d_v_ice_dyn   !: ice velocity 
      
   !!--------------------------------------------------------------------------
   !! * Ice thickness distribution variables
   !!--------------------------------------------------------------------------
   ! REMOVE
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   ice_types      !: Vector connecting types and categories
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ice_cat_bounds !: Matrix containing the integer upper and 
   !                                                                       !  lower boundaries of ice thickness categories
   ! REMOVE
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   ice_ncat_types !: nb of thickness categories in each ice type
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   hi_max         !: Boundary of ice thickness categories in thickness space
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   hi_mean        !: Mean ice thickness in catgories 
   ! REMOVE
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hi_max_typ     !: Boundary of ice thickness categories in thickness space

   !!--------------------------------------------------------------------------
   !! * Ice Run
   !!--------------------------------------------------------------------------
   !                                                                     !!: ** Namelist namicerun read in iceini **
   CHARACTER(len=32)     , PUBLIC ::   cn_icerst_in  = "restart_ice_in"   !: suffix of ice restart name (input)
   CHARACTER(len=32)     , PUBLIC ::   cn_icerst_out = "restart_ice"      !: suffix of ice restart name (output)
   LOGICAL               , PUBLIC ::   ln_limdyn     = .TRUE.             !: flag for ice dynamics (T) or not (F)
   LOGICAL               , PUBLIC ::   ln_nicep      = .TRUE.             !: flag for sea-ice points output (T) or not (F)
   REAL(wp)              , PUBLIC ::   hsndif        = 0.e0               !: computation of temp. in snow (0) or not (9999)
   REAL(wp)              , PUBLIC ::   hicdif        = 0.e0               !: computation of temp. in ice (0) or not (9999)
   REAL(wp)              , PUBLIC ::   cai           = 1.40e-3            !: atmospheric drag over sea ice
   REAL(wp)              , PUBLIC ::   cao           = 1.00e-3            !: atmospheric drag over ocean
   REAL(wp), DIMENSION(2), PUBLIC ::   acrit  = (/ 1.e-06 , 1.e-06 /)     !: minimum fraction for leads in
   !                                                                      !: north and south hemisphere
   !!--------------------------------------------------------------------------
   !! * Ice diagnostics
   !!--------------------------------------------------------------------------
   !! Check if everything down here is necessary
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   v_newice   !: volume of ice formed in the leads
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   dv_dt_thd  !: thermodynamic growth rates 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   izero, fstroc, fhbricat
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   diag_sni_gr   ! snow ice growth 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   diag_lat_gr   ! lateral ice growth 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   diag_bot_gr   ! bottom ice growth 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   diag_dyn_gr   ! dynamical ice growth 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   diag_bot_me   ! vertical bottom melt 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   diag_sur_me   ! vertical surface melt
   INTEGER , PUBLIC ::   jiindx, jjindx        !: indexes of the debugging point

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2010)
   !! $Id: ice.F90 2777 2011-06-07 09:55:02Z smasson $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   FUNCTION ice_alloc()
      !!-----------------------------------------------------------------
      !!               *** Routine ice_alloc_2 ***
      !!-----------------------------------------------------------------
      INTEGER :: ice_alloc
      !
      INTEGER :: ierr(20), ii
      !!-----------------------------------------------------------------

      ierr(:) = 0

      ! What could be one huge allocate statement is broken-up to try to
      ! stay within Fortran's max-line length limit.
      ii = 1
      ALLOCATE( u_oce    (jpi,jpj) , v_oce    (jpi,jpj) ,                           &
         &      ahiu     (jpi,jpj) , ahiv     (jpi,jpj) ,                           &
         &      pahu     (jpi,jpj) , pahv     (jpi,jpj) ,                           &
         &      ust2s    (jpi,jpj) , hicol    (jpi,jpj) ,                           &
         &      strp1    (jpi,jpj) , strp2    (jpi,jpj) , strength  (jpi,jpj) ,     &
         &      stress1_i(jpi,jpj) , stress2_i(jpi,jpj) , stress12_i(jpi,jpj) ,     &
         &      delta_i  (jpi,jpj) , divu_i   (jpi,jpj) , shear_i   (jpi,jpj) , STAT=ierr(ii) )

      ii = ii + 1
      ALLOCATE( firic    (jpi,jpj) , fcsic  (jpi,jpj) , fleic    (jpi,jpj) , qlatic   (jpi,jpj) ,     &
         &      rdvosif  (jpi,jpj) , rdvobif(jpi,jpj) , fdvolif  (jpi,jpj) , rdvonif  (jpi,jpj) ,     &
         &      sist     (jpi,jpj) , icethi (jpi,jpj) , t_bo     (jpi,jpj) , hicifp   (jpi,jpj) ,     &
         &      frld     (jpi,jpj) , pfrld  (jpi,jpj) , phicif   (jpi,jpj) , fbif     (jpi,jpj) ,     &
         &      rdmsnif  (jpi,jpj) , rdmicif(jpi,jpj) , qldif    (jpi,jpj) , qcmif    (jpi,jpj) ,     &
         &      fdtcn    (jpi,jpj) , qdtcn  (jpi,jpj) , fstric   (jpi,jpj) , fscmbq   (jpi,jpj) ,     &
         &      ffltbif  (jpi,jpj) , fsbbq  (jpi,jpj) , qfvbq    (jpi,jpj) , dmgwi    (jpi,jpj) ,     &
         &      fsalt_res(jpi,jpj) , fsbri  (jpi,jpj) , fsalt_rpo(jpi,jpj) , fheat_rpo(jpi,jpj) ,     &
         &      fhbri    (jpi,jpj) , fmmec  (jpi,jpj) , fseqv    (jpi,jpj) , fhmec    (jpi,jpj) ,     &
         &      fheat_res(jpi,jpj)                                                              , STAT=ierr(ii) )

      ii = ii + 1
      ALLOCATE( dh_i_surf2D(jpi,jpj) , dh_i_bott2D(jpi,jpj) , fstbif(jpi,jpj) ,     &
         &      fsup2D     (jpi,jpj) , focea2D    (jpi,jpj) , q_s   (jpi,jpj) , STAT=ierr(ii) )

      ii = ii + 1
      ALLOCATE( patho_case(jpi, jpj, jpl)  , STAT=ierr(ii) )

      ! * Ice global state variables
      ii = ii + 1
      ALLOCATE( ht_i (jpi,jpj,jpl) , a_i  (jpi,jpj,jpl) , v_i  (jpi,jpj,jpl) ,     &
         &      v_s  (jpi,jpj,jpl) , ht_s (jpi,jpj,jpl) , t_su (jpi,jpj,jpl) ,     &
         &      sm_i (jpi,jpj,jpl) , smv_i(jpi,jpj,jpl) , o_i  (jpi,jpj,jpl) ,     &
         &      ov_i (jpi,jpj,jpl) , oa_i (jpi,jpj,jpl)                      , STAT=ierr(ii) )
      ii = ii + 1
      ALLOCATE( u_ice(jpi,jpj) , v_ice(jpi,jpj) , tio_u(jpi,jpj) , tio_v(jpi,jpj) ,     &
         &      vt_i (jpi,jpj) , vt_s (jpi,jpj) , at_i (jpi,jpj) , ato_i(jpi,jpj) ,     &
         &      et_i (jpi,jpj) , et_s (jpi,jpj) , ot_i (jpi,jpj) , tm_i (jpi,jpj) ,     &
         &      bv_i (jpi,jpj) , smt_i(jpi,jpj)                                   , STAT=ierr(ii) )
      ii = ii + 1
      ALLOCATE( t_s(jpi,jpj,nlay_s,jpl) , at_i_typ(jpi,jpj,jpm) ,                            &
         &      e_s(jpi,jpj,nlay_s,jpl) , vt_i_typ(jpi,jpj,jpm) , e_i_cat(jpi,jpj,jpl) , STAT=ierr(ii) )
      ii = ii + 1
      ALLOCATE( t_i(jpi,jpj,jkmax,jpl) , e_i(jpi,jpj,jkmax,jpl) , s_i(jpi,jpj,jkmax,jpl) , STAT=ierr(ii) )

      ! * Moments for advection
      ii = ii + 1
      ALLOCATE( sxopw(jpi,jpj) , syopw(jpi,jpj) , sxxopw(jpi,jpj) , syyopw(jpi,jpj) , sxyopw(jpi,jpj) , STAT=ierr(ii) )
      ii = ii + 1
      ALLOCATE( sxice(jpi,jpj,jpl) , syice(jpi,jpj,jpl) , sxxice(jpi,jpj,jpl) , syyice(jpi,jpj,jpl) , sxyice(jpi,jpj,jpl) ,   &
         &      sxsn (jpi,jpj,jpl) , sysn (jpi,jpj,jpl) , sxxsn (jpi,jpj,jpl) , syysn (jpi,jpj,jpl) , sxysn (jpi,jpj,jpl) ,   &
         &      STAT=ierr(ii) )
      ii = ii + 1
      ALLOCATE( sxa  (jpi,jpj,jpl) , sya  (jpi,jpj,jpl) , sxxa  (jpi,jpj,jpl) , syya  (jpi,jpj,jpl) , sxya  (jpi,jpj,jpl) ,   &
         &      sxc0 (jpi,jpj,jpl) , syc0 (jpi,jpj,jpl) , sxxc0 (jpi,jpj,jpl) , syyc0 (jpi,jpj,jpl) , sxyc0 (jpi,jpj,jpl) ,   &
         &      sxsal(jpi,jpj,jpl) , sysal(jpi,jpj,jpl) , sxxsal(jpi,jpj,jpl) , syysal(jpi,jpj,jpl) , sxysal(jpi,jpj,jpl) ,   &
         &      sxage(jpi,jpj,jpl) , syage(jpi,jpj,jpl) , sxxage(jpi,jpj,jpl) , syyage(jpi,jpj,jpl) , sxyage(jpi,jpj,jpl) ,   &
         &      STAT=ierr(ii) )
      ii = ii + 1
      ALLOCATE( sxe (jpi,jpj,jkmax,jpl) , sye (jpi,jpj,jkmax,jpl) , sxxe(jpi,jpj,jkmax,jpl) ,     &
         &      syye(jpi,jpj,jkmax,jpl) , sxye(jpi,jpj,jkmax,jpl)                           , STAT=ierr(ii) )

      ! * Old values of global variables
      ii = ii + 1
      ALLOCATE( old_v_s  (jpi,jpj,jpl) , old_v_i  (jpi,jpj,jpl) , old_e_s(jpi,jpj,nlay_s,jpl) ,     &
         &      old_a_i  (jpi,jpj,jpl) , old_smv_i(jpi,jpj,jpl) , old_e_i(jpi,jpj,jkmax ,jpl) ,     &
         &      old_oa_i (jpi,jpj,jpl)                                                        ,     &
         &      old_u_ice(jpi,jpj)     , old_v_ice(jpi,jpj)                                   , STAT=ierr(ii) )

      ! * Increment of global variables
      ii = ii + 1
      ALLOCATE( d_a_i_thd(jpi,jpj,jpl) , d_a_i_trp (jpi,jpj,jpl) , d_v_s_thd  (jpi,jpj,jpl) , d_v_s_trp  (jpi,jpj,jpl) ,   &
         &      d_v_i_thd(jpi,jpj,jpl) , d_v_i_trp (jpi,jpj,jpl) , d_smv_i_thd(jpi,jpj,jpl) , d_smv_i_trp(jpi,jpj,jpl) ,   &     
         &      d_sm_i_fl(jpi,jpj,jpl) , d_sm_i_gd (jpi,jpj,jpl) , d_sm_i_se  (jpi,jpj,jpl) , d_sm_i_si  (jpi,jpj,jpl) ,   &
         &      d_sm_i_la(jpi,jpj,jpl) , d_oa_i_thd(jpi,jpj,jpl) , d_oa_i_trp (jpi,jpj,jpl) , s_i_newice (jpi,jpj,jpl) ,   &
         &     STAT=ierr(ii) )
      ii = ii + 1
      ALLOCATE( d_e_s_thd(jpi,jpj,nlay_s,jpl) , d_e_i_thd(jpi,jpj,jkmax,jpl) , d_u_ice_dyn(jpi,jpj) ,     &
         &      d_e_s_trp(jpi,jpj,nlay_s,jpl) , d_e_i_trp(jpi,jpj,jkmax,jpl) , d_v_ice_dyn(jpi,jpj) , STAT=ierr(ii) )
      
      ! * Ice thickness distribution variables
      ii = ii + 1
      ALLOCATE( ice_types(jpl) , ice_cat_bounds(jpm,2) , ice_ncat_types  (jpm) ,     &
         &      hi_max (0:jpl) , hi_mean(jpl)          , hi_max_typ(0:jpl,jpm) , STAT=ierr(ii) )

      ! * Ice diagnostics
      ii = ii + 1
      ALLOCATE( dv_dt_thd(jpi,jpj,jpl) , diag_sni_gr(jpi,jpj) , diag_lat_gr(jpi,jpj) ,     &
         &      izero    (jpi,jpj,jpl) , diag_bot_gr(jpi,jpj) , diag_dyn_gr(jpi,jpj) ,     &
         &      fstroc   (jpi,jpj,jpl) , diag_bot_me(jpi,jpj) , diag_sur_me(jpi,jpj) ,     &
         &      fhbricat (jpi,jpj,jpl) , v_newice   (jpi,jpj)                        , STAT=ierr(ii) )

      ice_alloc = MAXVAL( ierr(:) )
      IF( ice_alloc /= 0 )   CALL ctl_warn('ice_alloc_2: failed to allocate arrays.')
      !
   END FUNCTION ice_alloc

#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty module            NO LIM sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE ice
