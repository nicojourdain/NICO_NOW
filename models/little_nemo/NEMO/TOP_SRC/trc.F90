MODULE trc
   !!======================================================================
   !!                      ***  MODULE  trc  ***
   !! Passive tracers   :  module for tracers defined
   !!======================================================================
   !! History :   OPA  !  1996-01  (M. Levy)  Original code
   !!              -   !  1999-07  (M. Levy)  for LOBSTER1 or NPZD model
   !!              -   !  2000-04  (O. Aumont, M.A. Foujols)  HAMOCC3 and P3ZD
   !!   NEMO      1.0  !  2004-03  (C. Ethe)  Free form and module
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   USE par_oce
   USE par_trc
   
   IMPLICIT NONE
   PUBLIC

   PUBLIC   trc_alloc   ! called by nemogcm.F90

   !! parameters for the control of passive tracers
   !! --------------------------------------------------
   INTEGER, PUBLIC                                                 ::   numnat        !: logicla unit for the passive tracer NAMELIST
   INTEGER, PUBLIC                                                 ::   numstr        !: logical unit for tracer statistics

   !! passive tracers fields (before,now,after)
   !! --------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)               ::  trai           !: initial total tracer
   REAL(wp), PUBLIC                                                ::  areatot        !: total volume 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:  )         ::  cvol           !: volume correction -degrad option- 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:)         ::  trn            !: traceur concentration for now time step
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:)         ::  tra            !: traceur concentration for next time step
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:)         ::  trb            !: traceur concentration for before time step

   !! interpolated gradient
   !!--------------------------------------------------  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)           ::  gtru           !: hor. gradient at u-points at bottom ocean level
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)           ::  gtrv           !: hor. gradient at v-points at bottom ocean level
   
   !! passive tracers  (input and output)
   !! ------------------------------------------  
   LOGICAL             , PUBLIC                                    ::  ln_rsttr       !: boolean term for restart i/o for passive tracers (namelist)
   LOGICAL             , PUBLIC                                    ::  lrst_trc       !: logical to control the trc restart write
   INTEGER             , PUBLIC                                    ::  nn_dttrc       !: frequency of step on passive tracers
   INTEGER             , PUBLIC                                    ::  nn_writetrc    !: time step frequency for concentration outputs (namelist)
   INTEGER             , PUBLIC                                    ::  nutwrs         !: output FILE for passive tracers restart
   INTEGER             , PUBLIC                                    ::  nutrst         !: logical unit for restart FILE for passive tracers
   INTEGER             , PUBLIC                                    ::  nn_rsttr       !: control of the time step ( 0 or 1 ) for pass. tr.
   CHARACTER(len = 80) , PUBLIC                                    ::  cn_trcrst_in   !: suffix of pass. tracer restart name (input)
   CHARACTER(len = 80) , PUBLIC                                    ::  cn_trcrst_out  !: suffix of pass. tracer restart name (output)
   REAL(wp)            , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::  rdttrc         !: vertical profile of passive tracer time step
   LOGICAL             , PUBLIC                                    ::  ln_trcdta      !: Read inputs data from files
   LOGICAL             , PUBLIC                                    ::  ln_trcdmp      !: internal damping flag
   INTEGER             , PUBLIC                                    ::  nittrc000       !: first time step of passive tracers model

   !! information for outputs
   !! --------------------------------------------------
   TYPE, PUBLIC :: PTRACER                                                            !: Passive tracer type
       CHARACTER(len = 20)  :: clsname  !: short name
       CHARACTER(len = 80)  :: cllname  !: long name
       CHARACTER(len = 20)  :: clunit   !: unit
       LOGICAL              :: llinit   !: read in a file or not
       LOGICAL              :: llsave   !: save the tracer or not
   END TYPE PTRACER
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)    ::  ctrcnm         !: tracer name 
   CHARACTER(len = 80), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)    ::  ctrcln         !: trccer field long name
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)    ::  ctrcun         !: tracer unit
   LOGICAL            , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)    ::  ln_trc_ini     !: Initialisation from data input file
   LOGICAL            , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)    ::  ln_trc_wri     !: save the tracer or not

   TYPE, PUBLIC :: DIAG                                                               !: passive trcacer ddditional diagnostic type
      CHARACTER(len = 20)  :: sname    !: short name
      CHARACTER(len = 80)  :: lname    !: long name
      CHARACTER(len = 20)  :: units    !: unit
   END TYPE DIAG

   !! additional 2D/3D outputs namelist
   !! --------------------------------------------------
   REAL(wp)           , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,  :) ::   trc2d         !: additional 2d outputs array 
   REAL(wp)           , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   trc3d         !: additional 3d outputs array 
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   ctrc2d        !: 2d field short name
   CHARACTER(len = 80), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   ctrc2l        !: 2d field long name
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   ctrc2u        !: 2d field unit
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   ctrc3d        !: 3d field short name
   CHARACTER(len = 80), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   ctrc3l        !: 3d field long name
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   ctrc3u        !: 3d field unit
   LOGICAL            , PUBLIC                                        ::  ln_diatrc      !: boolean term for additional diagnostic
   INTEGER            , PUBLIC                                        ::  nn_writedia    !: frequency of additional outputs

   !! Biological trends
   !! -----------------
   LOGICAL            , PUBLIC                                        ::  ln_diabio      !: boolean term for biological diagnostic
   INTEGER            , PUBLIC                                        ::  nn_writebio    !: frequency of biological outputs
   REAL(wp)           , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::  trbio          !: biological trends
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::  ctrbio         !: bio field short name
   CHARACTER(len = 80), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::  ctrbil         !: bio field long name
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::  ctrbiu         !: bio field unit

   !! variables to average over physics over passive tracer sub-steps.
   !! ----------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  un_tm       !: i-horizontal velocity average     [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  vn_tm       !: j-horizontal velocity average     [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::  tsn_tm      !: t/s average     [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  avt_tm      !: vertical diffusivity coeff. at  w-point   [m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  rhop_tm     !: 
# if defined key_zdfddm
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  avs_tm      !: vertical double diffusivity coeff. at w-point   [m/s]
# endif
#if defined key_ldfslp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  wslpi_tm    !: i-direction slope at u-, w-points
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  wslpj_tm    !: j-direction slope at u-, w-points
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  uslp_tm     !: j-direction slope at u-, w-points
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  vslp_tm     !: j-direction slope at u-, w-points
#endif
#if defined key_trabbl
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  ahu_bbl_tm  !: u-, w-points
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  ahv_bbl_tm  !: j-direction slope at u-, w-points
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  utr_bbl_tm  !: j-direction slope at u-, w-points
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  vtr_bbl_tm  !: j-direction slope at u-, w-points
#endif
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  sshn_tm     !: average ssh for the now step [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  sshu_n_tm   !: average ssh for the now step [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  sshv_n_tm   !: average ssh for the now step [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  sshb_hold   !:hold sshb from the beginning of each sub-stepping[m]  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  sshu_b_hold !:hold sshb from the beginning of each sub-stepping[m]  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  sshv_b_hold !:hold sshb from the beginning of each sub-stepping[m] 

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  rnf_tm     !: river runoff
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  h_rnf_tm   !: depth in metres to the bottom of the relevant grid box
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  hmld_tm    !: mixed layer depth average [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  fr_i_tm    !: average ice fraction     [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  emp_tm     !: freshwater budget: volume flux [Kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  emps_tm    !: freshwater budget:concentration/dilution [Kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  emp_b_hold !: hold emp from the beginning of each sub-stepping[m]  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  qsr_tm     !: solar radiation average [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  wndm_tm    !: 10m wind average [m]
   !
#if defined key_traldf_c3d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  ahtt_tm, ahtu_tm, ahtv_tm, ahtw_tm   !: ** 3D coefficients ** at T-,U-,V-,W-points
#elif defined key_traldf_c2d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  ahtt_tm, ahtu_tm, ahtv_tm, ahtw_tm   !: ** 2D coefficients ** at T-,U-,V-,W-points
#elif defined key_traldf_c1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::  ahtt_tm, ahtu_tm, ahtv_tm, ahtw_tm   !: ** 1D coefficients ** at T-,U-,V-,W-points
#else
   REAL(wp), PUBLIC                                        ::  ahtt_tm, ahtu_tm, ahtv_tm, ahtw_tm   !: ** 0D coefficients ** at T-,U-,V-,W-points
#endif
   !
#if defined key_traldf_eiv
#  if defined key_traldf_c3d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  aeiu_tm , aeiv_tm , aeiw_tm   !: ** 3D coefficients **
#  elif defined key_traldf_c2d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  aeiu_tm , aeiv_tm , aeiw_tm   !: ** 2D coefficients **
#  elif defined key_traldf_c1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::  aeiu_tm , aeiv_tm, aeiw_tm   !: ** 1D coefficients **
#  else
   REAL(wp), PUBLIC                                        ::  aeiu_tm , aeiv_tm , aeiw_tm   !: ** 0D coefficients **
#  endif
#endif

   ! Temporary physical arrays for sub_stepping
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::  tsn_temp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  un_temp,vn_temp,wn_temp     !: hold current values of avt, un, vn, wn
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  avt_temp, rhop_temp     !: hold current values of avt, un, vn, wn
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  e3t_temp,e3u_temp,e3v_temp,e3w_temp     !: hold current values
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  sshn_temp, sshb_temp, ssha_temp, rnf_temp,h_rnf_temp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  sshu_n_temp, sshu_b_temp, sshu_a_temp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  sshf_n_temp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  sshv_n_temp, sshv_b_temp, sshv_a_temp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  hu_temp, hv_temp, hur_temp, hvr_temp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  hdivn_temp, rotn_temp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  hdivb_temp, rotb_temp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  hmld_temp, qsr_temp, fr_i_temp,wndm_temp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  emp_temp, emps_temp, emp_b_temp
   !
#if defined key_trabbl
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  ahu_bbl_temp, ahv_bbl_temp, utr_bbl_temp, vtr_bbl_temp !: hold current values 
#endif
   !
#if defined key_ldfslp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  wslpi_temp, wslpj_temp, uslp_temp, vslp_temp    !: hold current values 
#endif
   ! 
# if defined key_zdfddm
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  avs_temp      !: salinity vertical diffusivity coeff. at w-point   [m/s]
# endif
   !
#if defined key_traldf_c3d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  ahtt_temp, ahtu_temp, ahtv_temp, ahtw_temp   
#elif defined key_traldf_c2d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  ahtt_temp, ahtu_temp, ahtv_temp, ahtw_temp  
#elif defined key_traldf_c1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::  ahtt_temp, ahtu_temp, ahtv_temp, ahtw_temp 
#else
   REAL(wp), PUBLIC                                        ::  ahtt_temp, ahtu_temp, ahtv_temp, ahtw_temp
#endif
   !
#if defined key_traldf_eiv
# if defined key_traldf_c3d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  aeiu_temp , aeiv_temp , aeiw_temp   !: ** 3D coefficients **
# elif defined key_traldf_c2d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  aeiu_temp , aeiv_temp , aeiw_temp   !: ** 2D coefficients **
# elif defined key_traldf_c1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::  aeiu_temp , aeiv_temp, aeiw_temp   !: ** 1D coefficients **
# else
   REAL(wp), PUBLIC                                        ::  aeiu_temp , aeiv_temp , aeiw_temp   !: ** 0D coefficients **
# endif
# endif

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3.1 , NEMO Consortium (2010)
   !! $Id: trc.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION trc_alloc()
      !!-------------------------------------------------------------------
      !!                    *** ROUTINE trc_alloc ***
      !!-------------------------------------------------------------------
      USE lib_mpp, ONLY: ctl_warn
      !!-------------------------------------------------------------------
      !
      ALLOCATE( trn(jpi,jpj,jpk,jptra), trb(jpi,jpj,jpk,jptra), tra(jpi,jpj,jpk,jptra),       &  
         &      gtru(jpi,jpj,jpk)     , gtrv(jpi,jpj,jpk)                             ,       &
         &      cvol(jpi,jpj,jpk)     , rdttrc(jpk)           , trai(jptra)           ,       &
         &      ctrcnm(jptra)         , ctrcln(jptra)         , ctrcun(jptra)         ,       & 
         &      ln_trc_ini(jptra)     , ln_trc_wri(jptra)                             ,  STAT = trc_alloc  )  

      IF( trc_alloc /= 0 )   CALL ctl_warn('trc_alloc: failed to allocate arrays')
      !
   END FUNCTION trc_alloc

#else
   !!----------------------------------------------------------------------
   !!  Empty module :                                     No passive tracer
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE trc
