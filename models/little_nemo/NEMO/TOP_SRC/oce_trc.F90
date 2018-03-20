MODULE oce_trc
   !!======================================================================
   !!                      ***  MODULE  oce_trc  ***
   !! TOP :   variables shared between ocean and passive tracers
   !!======================================================================
   !! History :   1.0  !  2004-03  (C. Ethe)  original code
   !!             2.0  !  2007-12 (C. Ethe, G. Madec)  rewritting
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------

   !* Domain size *
   USE par_oce , ONLY :   cp_cfg   =>   cp_cfg     !: name of the configuration
   USE par_oce , ONLY :   jp_cfg   =>   jp_cfg     !: resolution of the configuration
   USE par_oce , ONLY :   jpiglo   =>   jpiglo     !: first  dimension of global domain --> i
   USE par_oce , ONLY :   jpjglo   =>   jpjglo     !: second dimension of global domain --> j
   USE par_oce , ONLY :   jpi      =>   jpi        !: first  dimension of grid --> i 
   USE par_oce , ONLY :   jpj      =>   jpj        !: second dimension of grid --> j  
   USE par_oce , ONLY :   jpk      =>   jpk        !: number of levels  
   USE par_oce , ONLY :   jpim1    =>   jpim1      !: jpi - 1
   USE par_oce , ONLY :   jpjm1    =>   jpjm1      !: jpj - 1 
   USE par_oce , ONLY :   jpkm1    =>   jpkm1      !: jpk - 1  
   USE par_oce , ONLY :   jpij     =>   jpij       !: jpi x jpj
   USE par_oce , ONLY :   jpidta   =>   jpidta     !: first horizontal dimension  > or = jpi
   USE par_oce , ONLY :   jpjdta   =>   jpjdta     !: second horizontal dimension > or = jpj
   USE par_oce , ONLY :   jpkdta   =>   jpkdta     !: number of levels            > or = jpk
   USE par_oce , ONLY :   lk_esopa =>   lk_esopa   !: flag to activate the all option
   USE par_oce , ONLY :   jp_tem   =>   jp_tem     !: indice for temperature
   USE par_oce , ONLY :   jp_sal   =>   jp_sal     !: indice for salinity

   !* IO manager *
   USE in_out_manager    
 
   !* Memory Allocation *
   USE wrk_nemo      
 
   !* Timing *
   USE timing    
 
   !* MPP library                         
   USE lib_mpp 

   !* Fortran utilities                         
   USE lib_fortran

   !* Lateral boundary conditions                         
   USE lbclnk

   !* physical constants *
   USE phycst            

   !* 1D configuration
   USE c1d                                         

   !* model domain *
   USE dom_oce , ONLY :   lzoom      => lzoom        !: zoom flag
   USE dom_oce , ONLY :   lzoom_e    => lzoom_e      !: East  zoom type flag
   USE dom_oce , ONLY :   lzoom_w    => lzoom_w      !: West  zoom type flag
   USE dom_oce , ONLY :   lzoom_s    => lzoom_s      !: South zoom type flag
   USE dom_oce , ONLY :   lzoom_n    => lzoom_n      !: North zoom type flag
   USE dom_oce , ONLY :   lzoom_arct => lzoom_arct   !: ORCA    arctic zoom flag
   USE dom_oce , ONLY :   lzoom_anta => lzoom_anta   !: ORCA antarctic zoom flag
   USE dom_oce , ONLY :   nperio     =>   nperio     !: type of lateral boundary condition       
   USE dom_oce , ONLY :   nimpp      =>   nimpp      !: i index for mpp-subdomain left bottom
   USE dom_oce , ONLY :   njmpp      =>   njmpp      !: j index for mpp-subdomain left bottom
   USE dom_oce , ONLY :   nproc      =>   nproc      !: number for local processor
   USE dom_oce , ONLY :   narea      =>   narea      !: number for local area
   USE dom_oce , ONLY :   mig        =>   mig        !: local  ==> global  domain i-indice
   USE dom_oce , ONLY :   mjg        =>   mjg        !: local  ==> global  domain i-indice
   USE dom_oce , ONLY :   mi0        =>   mi0        !: global ==> local domain i-indice 
   USE dom_oce , ONLY :   mi1        =>   mi1        !: (mi0=1 and mi1=0 if the global indice is not in the local one)
   USE dom_oce , ONLY :   mj0        =>   mj0        !: global ==> local domain j-indice 
   USE dom_oce , ONLY :   mj1        =>   mj1        !: (mj0=1 and mj1=0 if the global indice is not in the local one)
   USE dom_oce , ONLY :   nidom      =>   nidom
   USE dom_oce , ONLY :   nimppt     => nimppt     !:i-indexes for each processor
   USE dom_oce , ONLY :   njmppt     => njmppt       !:j-indexes for each processor
   USE dom_oce , ONLY :   ibonit     => ibonit       !:i-processor neighbour existence
   USE dom_oce , ONLY :   ibonjt     => ibonjt       !:j- processor neighbour existence 
   USE dom_oce , ONLY :   nlci       => nlci         !:i- & j-dimensions of the local subdomain
   USE dom_oce , ONLY :   nlcj       => nlcj         !:
   USE dom_oce , ONLY :   nldi       => nldi         !:first and last indoor i- and j-indexes
   USE dom_oce , ONLY :   nlei       => nlei         !:
   USE dom_oce , ONLY :   nldj       => nldj         !:
   USE dom_oce , ONLY :   nlej       => nlej         !:
   USE dom_oce , ONLY :   nlcit      => nlcit        !:dimensions of every i-subdomain
   USE dom_oce , ONLY :   nlcjt      => nlcjt        !:dimensions of every j-subdomain
   USE dom_oce , ONLY :   nldit      => nldit        !:first indoor index for each i-domain 
   USE dom_oce , ONLY :   nleit      => nleit        !:last indoor index for each i-domain 
   USE dom_oce , ONLY :   nldjt      => nldjt        !:first indoor index for each j-domain 
   USE dom_oce , ONLY :   nlejt      => nlejt        !:last indoor index for each j-domain 
 
   !* horizontal mesh *
   USE dom_oce , ONLY :   glamt      =>   glamt      !: longitude of t-point (degre)  
   USE dom_oce , ONLY :   glamu      =>   glamu      !: longitude of t-point (degre)  
   USE dom_oce , ONLY :   glamv      =>   glamv      !: longitude of t-point (degre)  
   USE dom_oce , ONLY :   glamf      =>   glamf      !: longitude of t-point (degre)  
   USE dom_oce , ONLY :   gphit      =>   gphit      !: latitude  of t-point (degre)   
   USE dom_oce , ONLY :   gphiu      =>   gphiu      !: latitude  of t-point (degre)   
   USE dom_oce , ONLY :   gphiv      =>   gphiv      !: latitude  of t-point (degre)   
   USE dom_oce , ONLY :   gphif      =>   gphif      !: latitude  of t-point (degre)   
   USE dom_oce , ONLY :   e1t        =>   e1t        !: horizontal scale factors at t-point (m)  
   USE dom_oce , ONLY :   e2t        =>   e2t        !: horizontal scale factors at t-point (m)   
   USE dom_oce , ONLY :   e1e2t      =>   e1e2t      !: cell surface at t-point (m2)
   USE dom_oce , ONLY :   e1u        =>   e1u        !: horizontal scale factors at u-point (m)
   USE dom_oce , ONLY :   e2u        =>   e2u        !: horizontal scale factors at u-point (m)
   USE dom_oce , ONLY :   e1v        =>   e1v        !: horizontal scale factors at v-point (m)
   USE dom_oce , ONLY :   e2v        =>   e2v        !: horizontal scale factors at v-point (m)  

   !* vertical mesh *
   USE dom_oce , ONLY :   gdept_0    =>   gdept_0    !: reference depth of t-points (m)
   USE dom_oce , ONLY :   e3t_0      =>   e3t_0      !: reference depth of t-points (m)  
   USE dom_oce , ONLY :   e3w_0      =>   e3w_0      !: reference depth of w-points (m)
   USE dom_oce , ONLY :   gdepw_0    =>   gdepw_0    !: reference depth of w-points (m)
# if ! defined key_zco
   USE dom_oce , ONLY :   gdep3w     =>  gdep3w      !: ???
   USE dom_oce , ONLY :   gdept      =>  gdept       !: depth of t-points (m)
   USE dom_oce , ONLY :   gdepw      =>  gdepw       !: depth of t-points (m)
   USE dom_oce , ONLY :   e3t        =>  e3t         !: vertical scale factors at t-
   USE dom_oce , ONLY :   e3u        =>  e3u         !: vertical scale factors at u-
   USE dom_oce , ONLY :   e3v        =>  e3v         !: vertical scale factors v-
   USE dom_oce , ONLY :   e3w        =>  e3w         !: w-points (m)
   USE dom_oce , ONLY :   e3f        =>  e3f         !: f-points (m)
   USE dom_oce , ONLY :   e3uw       =>  e3uw        !: uw-points (m)
   USE dom_oce , ONLY :   e3vw       =>  e3vw        !: vw-points (m)
# endif
   USE dom_oce , ONLY :   ln_zps     =>  ln_zps      !: partial steps flag
   USE dom_oce , ONLY :   ln_sco     =>  ln_sco      !: s-coordinate flag
   USE dom_oce , ONLY :   ln_zco     =>  ln_zco      !: z-coordinate flag
   USE dom_oce , ONLY :   hbatt      =>  hbatt       !: ocean depth at the vertical of  t-point (m)
   USE dom_oce , ONLY :   hbatu      =>  hbatu       !: ocean depth at the vertical of  u-point (m)
   USE dom_oce , ONLY :   hbatv      =>  hbatv       !: ocean depth at the vertical of w-point (m)
   USE dom_oce , ONLY :   gsigt      =>  gsigt       !: model level depth coefficient at T-levels
   USE dom_oce , ONLY :   gsigw      =>  gsigw       !: model level depth coefficient at W-levels
   USE dom_oce , ONLY :   gsi3w      =>  gsi3w       !: model level depth coef at w-levels (defined as the sum of e3w)
   USE dom_oce , ONLY :   esigt      =>  esigt       !: vertical scale factor coef. at t-levels
   USE dom_oce , ONLY :   esigw      =>  esigw       !: vertical scale factor coef. at w-levels
   USE dom_oce , ONLY :   lk_vvl     =>  lk_vvl      !: variable grid flag
# if defined key_vvl
   USE dom_oce , ONLY :   gdep3w_1   =>  gdep3w_1    !: ???
   USE dom_oce , ONLY :   gdept_1    =>  gdept_1     !: depth of t-points (m)
   USE dom_oce , ONLY :   gdepw_1    =>  gdepw_1     !: depth of t-points (m)
   USE dom_oce , ONLY :   e3t_1      =>  e3t_1       !: vertical scale factors at t-
   USE dom_oce , ONLY :   e3u_1      =>  e3u_1       !: vertical scale factors at u-
   USE dom_oce , ONLY :   e3v_1      =>  e3v_1       !: vertical scale factors v-
   USE dom_oce , ONLY :   e3w_1      =>  e3w_1       !: w-points (m)
   USE dom_oce , ONLY :   e3f_1      =>  e3f_1       !: f-points (m)
   USE dom_oce , ONLY :   e3uw_1     =>  e3uw_1      !: uw-points (m)
   USE dom_oce , ONLY :   e3vw_1     =>  e3vw_1      !: vw-points (m)
# endif
   !* masks, bathymetry *
   USE dom_oce , ONLY :   mbkt       =>   mbkt       !: vertical index of the bottom last T- ocean level
   USE dom_oce , ONLY :   mbku       =>   mbku       !: vertical index of the bottom last U- ocean level
   USE dom_oce , ONLY :   mbkv       =>   mbkv       !: vertical index of the bottom last V- ocean level
   USE dom_oce , ONLY :   tmask_i    =>   tmask_i    !: Interior mask at t-points
   USE dom_oce , ONLY :   tmask      =>   tmask      !: land/ocean mask at t-points
   USE dom_oce , ONLY :   umask      =>   umask      !: land/ocean mask at u-points   
   USE dom_oce , ONLY :   vmask      =>   vmask      !: land/ocean mask at v-points 
   USE dom_oce , ONLY :   fmask      =>   fmask      !: land/ocean mask at f-points 

   !* time domain *
   USE dom_oce , ONLY :   neuler     =>   neuler     !: restart euler forward option (0=Euler)
   USE dom_oce , ONLY :   rdt        =>   rdt        !: time step for the dynamics 
   USE dom_oce , ONLY :   atfp       =>   atfp       !: asselin time filter parameter
   USE dom_oce , ONLY :   atfp1      =>   atfp1      !: asselin time filter coeff. (atfp1= 1-2*atfp)
   USE dom_oce , ONLY :   rdttra     =>   rdttra     !: vertical profile of tracer time step
   !                                                 !: it is the accumulated duration of previous runs
   !                                                 !: that may have been run with different time steps.
   !* calendar variables *
   USE dom_oce , ONLY :   nyear      =>   nyear      !: current year
   USE dom_oce , ONLY :   nmonth     =>   nmonth     !: current month
   USE dom_oce , ONLY :   nday       =>   nday       !: current day of the month
   USE dom_oce , ONLY :   ndastp     =>   ndastp     !: time step date in yyyymmdd format
   USE dom_oce , ONLY :   nday_year  =>   nday_year  !: current day counted from jan 1st of the current year
   USE dom_oce , ONLY :   nsec_year  =>   nsec_year  !: current time step counted in second since 00h jan 1st of the current year
   USE dom_oce , ONLY :   nsec_month =>   nsec_month !: current time step counted in second since 00h 1st day of the current month
   USE dom_oce , ONLY :   nsec_day   =>   nsec_day   !: current time step counted in second since 00h of the current day
   USE dom_oce , ONLY :   fjulday    =>   fjulday    !: julian day
   USE dom_oce , ONLY :   adatrj     =>   adatrj     !: number of elapsed days since the begining of the whole simulation
                                                     !: (cumulative duration of previous runs 
                                                     !: that may have used different time-step size)
   USE dom_oce , ONLY :   nyear_len  =>   nyear_len  !: length in days of the previous/current year
   USE dom_oce , ONLY :   nmonth_len =>   nmonth_len !: length in days of the months of the current year


   !* ocean fields: here now and after fields *
   USE oce , ONLY :   ua      =>    ua      !: i-horizontal velocity (m s-1) 
   USE oce , ONLY :   va      =>    va      !: j-horizontal velocity (m s-1)
   USE oce , ONLY :   un      =>    un      !: i-horizontal velocity (m s-1) 
   USE oce , ONLY :   vn      =>    vn      !: j-horizontal velocity (m s-1)
   USE oce , ONLY :   wn      =>    wn      !: vertical velocity (m s-1)  
   USE oce , ONLY :   tsn     =>    tsn     !: 4D array contaning ( tn, sn )
   USE oce , ONLY :   tsb     =>    tsb     !: 4D array contaning ( tb, sb )
   USE oce , ONLY :   tsa     =>    tsa     !: 4D array contaning ( ta, sa )
   USE oce , ONLY :   rhop    =>    rhop    !: potential volumic mass (kg m-3) 
   USE oce , ONLY :   rhd     =>    rhd     !: in situ density anomalie rhd=(rho-rau0)/rau0 (no units)
   USE oce , ONLY :   hdivn   =>    hdivn   !: horizontal divergence (1/s)
   USE oce , ONLY :   rotn    =>    rotn    !: relative vorticity    [s-1]
   USE oce , ONLY :   hdivb   =>    hdivb   !: horizontal divergence (1/s)
   USE oce , ONLY :   rotb    =>    rotb    !: relative vorticity    [s-1]
   USE oce , ONLY :   sshn    =>    sshn    !: sea surface height at t-point [m]   
   USE oce , ONLY :   sshb    =>    sshb    !: sea surface height at t-point [m]   
   USE oce , ONLY :   ssha    =>    ssha    !: sea surface height at t-point [m]   
   USE oce , ONLY :   sshu_n  =>    sshu_n  !: sea surface height at u-point [m]   
   USE oce , ONLY :   sshu_b  =>    sshu_b  !: sea surface height at u-point [m]   
   USE oce , ONLY :   sshu_a  =>    sshu_a  !: sea surface height at u-point [m]   
   USE oce , ONLY :   sshv_n  =>    sshv_n  !: sea surface height at v-point [m]   
   USE oce , ONLY :   sshv_b  =>    sshv_b  !: sea surface height at v-point [m]   
   USE oce , ONLY :   sshv_a  =>    sshv_a  !: sea surface height at v-point [m]   
   USE oce , ONLY :   sshf_n  =>    sshf_n  !: sea surface height at v-point [m]   
   USE oce , ONLY :   l_traldf_rot => l_traldf_rot  !: rotated laplacian operator for lateral diffusion
#if defined key_offline
   USE oce , ONLY :   gtsu    =>    gtsu    !: t-, s- and rd horizontal gradient at u- and
   USE oce , ONLY :   gtsv    =>    gtsv    !:
   USE oce , ONLY :   gru     =>    gru     !:
   USE oce , ONLY :   grv     =>    grv     !: 
#endif

   USE dom_oce , ONLY :   nn_cla    =>  nn_cla        !: flag (0/1) for cross land advection 

   !* surface fluxes *
   USE sbc_oce , ONLY :   utau       =>    utau       !: i-surface stress component
   USE sbc_oce , ONLY :   vtau       =>    vtau       !: j-surface stress component
   USE sbc_oce , ONLY :   wndm       =>    wndm       !: 10m wind speed 
   USE sbc_oce , ONLY :   qsr        =>    qsr        !: penetrative solar radiation (w m-2)  
   USE sbc_oce , ONLY :   emp        =>    emp        !: freshwater budget: volume flux               [Kg/m2/s]
   USE sbc_oce , ONLY :   emp_b      =>    emp_b      !: freshwater budget: volume flux               [Kg/m2/s]
   USE sbc_oce , ONLY :   emps       =>    emps       !: freshwater budget: concentration/dillution   [Kg/m2/s]
   USE sbc_oce , ONLY :   rnf        =>    rnf        !: river runoff   [Kg/m2/s]
   USE sbc_oce , ONLY :   ln_dm2dc   =>    ln_dm2dc   !: Daily mean to Diurnal Cycle short wave (qsr) 
   USE sbc_oce , ONLY :   ln_rnf     =>    ln_rnf     !: runoffs / runoff mouths
   USE sbc_oce , ONLY :   fr_i       =>    fr_i       !: ice fraction (between 0 to 1)
   USE traqsr  , ONLY :   rn_abs     =>    rn_abs     !: fraction absorbed in the very near surface
   USE traqsr  , ONLY :   rn_si0     =>    rn_si0     !: very near surface depth of extinction
   USE traqsr  , ONLY :   ln_qsr_bio =>    ln_qsr_bio !: flag to use or not the biological fluxes for light
   USE sbcrnf  , ONLY :   rnfmsk     =>    rnfmsk     !: mixed adv scheme in runoffs vicinity (hori.) 
   USE sbcrnf  , ONLY :   rnfmsk_z   =>    rnfmsk_z   !: mixed adv scheme in runoffs vicinity (vert.)
   USE sbcrnf  , ONLY :   h_rnf      =>    h_rnf      !: river runoff   [Kg/m2/s]

   USE trc_oce

   !* lateral diffusivity (tracers) *
   USE ldftra_oce , ONLY :  rldf     =>   rldf        !: multiplicative coef. for lateral diffusivity
   USE ldftra_oce , ONLY :  rn_aht_0 =>   rn_aht_0    !: horizontal eddy diffusivity for tracers (m2/s)
   USE ldftra_oce , ONLY :  aht0     =>   aht0        !: horizontal eddy diffusivity for tracers (m2/s)
   USE ldftra_oce , ONLY :  ahtb0    =>   ahtb0       !: background eddy diffusivity for isopycnal diff. (m2/s)
   USE ldftra_oce , ONLY :  ahtu     =>   ahtu        !: lateral diffusivity coef. at u-points 
   USE ldftra_oce , ONLY :  ahtv     =>   ahtv        !: lateral diffusivity coef. at v-points 
   USE ldftra_oce , ONLY :  ahtw     =>   ahtw        !: lateral diffusivity coef. at w-points 
   USE ldftra_oce , ONLY :  ahtt     =>   ahtt        !: lateral diffusivity coef. at t-points
   USE ldftra_oce , ONLY :  aeiv0    =>   aeiv0       !: eddy induced velocity coefficient (m2/s) 
   USE ldftra_oce , ONLY :  aeiu     =>   aeiu        !: eddy induced velocity coef. at u-points (m2/s)   
   USE ldftra_oce , ONLY :  aeiv     =>   aeiv        !: eddy induced velocity coef. at v-points (m2/s) 
   USE ldftra_oce , ONLY :  aeiw     =>   aeiw        !: eddy induced velocity coef. at w-points (m2/s) 
   USE ldftra_oce , ONLY :  lk_traldf_eiv  =>  lk_traldf_eiv     !: eddy induced velocity flag

   !* vertical diffusion *
   USE zdf_oce , ONLY :   avt        =>   avt         !: vert. diffusivity coef. at w-point for temp  
# if defined key_zdfddm
   USE zdfddm  , ONLY :   avs        =>   avs         !: salinity vertical diffusivity coeff. at w-point
# endif

   !* mixing & mixed layer depth *
   USE zdfmxl , ONLY :   nmln        =>   nmln        !: number of level in the mixed layer
   USE zdfmxl , ONLY :   hmld        =>   hmld        !: mixing layer depth (turbocline)
   USE zdfmxl , ONLY :   hmlp        =>   hmlp        !: mixed layer depth  (rho=rho0+zdcrit) (m)
   USE zdfmxl , ONLY :   hmlpt       =>   hmlpt       !: mixed layer depth at t-points (m)

   !* direction of lateral diffusion *
   USE ldfslp , ONLY :   lk_ldfslp  =>  lk_ldfslp     !: slopes flag
# if   defined key_ldfslp
   USE ldfslp , ONLY :   uslp       =>   uslp         !: i-direction slope at u-, w-points
   USE ldfslp , ONLY :   vslp       =>   vslp         !: j-direction slope at v-, w-points
   USE ldfslp , ONLY :   wslpi      =>   wslpi        !: i-direction slope at u-, w-points
   USE ldfslp , ONLY :   wslpj      =>   wslpj        !: j-direction slope at v-, w-points
# endif

#else
   !!----------------------------------------------------------------------
   !!  Empty module :                                     No passive tracer
   !!----------------------------------------------------------------------
#endif

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: oce_trc.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE oce_trc
