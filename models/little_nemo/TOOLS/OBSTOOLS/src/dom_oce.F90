MODULE dom_oce
   !!======================================================================
   !!                       ***  MODULE dom_oce  ***
   !!       
   !! ** Purpose :   Define in memory all the ocean space domain variables
   !!======================================================================
   !! History :  1.0  ! 2005-10  (A. Beckmann, G. Madec)  reactivate s-coordinate 
   !!            3.3  ! 2010-11  (G. Madec) add mbk. arrays associated to the deepest ocean level
   !!            4.0  ! 2011-01  (A. R. Porter, STFC Daresbury) dynamical allocation
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   Agrif_Root    : dummy function used when lk_agrif=F
   !!   Agrif_CFixed  : dummy function used when lk_agrif=F
   !!   dom_oce_alloc : dynamical allocation of dom_oce arrays
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters

   IMPLICIT NONE
   PUBLIC             ! allows the acces to par_oce when dom_oce is used
   !                  ! exception to coding rules... to be suppressed ???

   PUBLIC dom_oce_alloc  ! Called from nemogcm.F90

   !!----------------------------------------------------------------------
   !! time & space domain namelist
   !! ----------------------------
   !                                              !!* Namelist namdom : time & space domain *
   INTEGER , PUBLIC ::   nn_bathy     =    0       !: = 0/1 ,compute/read the bathymetry file
   REAL(wp), PUBLIC ::   rn_hmin      =   -3.0_wp  !: minimum ocean depth (>0) or minimum number of ocean levels (<0)
   REAL(wp), PUBLIC ::   rn_e3zps_min =    5.0_wp  !: miminum thickness for partial steps (meters)
   REAL(wp), PUBLIC ::   rn_e3zps_rat =    0.1_wp  !: minimum thickness ration for partial steps
   INTEGER , PUBLIC ::   nn_msh       =    0       !: = 1 create a mesh-mask file
   INTEGER , PUBLIC ::   nn_acc       =    0       !: = 0/1 use of the acceleration of convergence technique
   REAL(wp), PUBLIC ::   rn_atfp      =    0.1_wp  !: asselin time filter parameter
   REAL(wp), PUBLIC ::   rn_rdt       = 3600._wp   !: time step for the dynamics (and tracer if nacc=0)
   REAL(wp), PUBLIC ::   rn_rdtmin    = 3600._wp   !: minimum time step on tracers
   REAL(wp), PUBLIC ::   rn_rdtmax    = 3600._wp   !: maximum time step on tracers
   REAL(wp), PUBLIC ::   rn_rdth      =  800._wp   !: depth variation of tracer step
   INTEGER , PUBLIC ::   nn_baro      =   64       !: number of barotropic time steps (key_dynspg_ts)
   INTEGER , PUBLIC ::   nn_closea    =    0       !: =0 suppress closed sea/lake from the ORCA domain or not (=1)

   !                                    !! old non-DOCTOR names still used in the model
   INTEGER , PUBLIC ::   ntopo           !: = 0/1 ,compute/read the bathymetry file
   REAL(wp), PUBLIC ::   e3zps_min       !: miminum thickness for partial steps (meters)
   REAL(wp), PUBLIC ::   e3zps_rat       !: minimum thickness ration for partial steps
   INTEGER , PUBLIC ::   nmsh            !: = 1 create a mesh-mask file
   INTEGER , PUBLIC ::   nacc            !: = 0/1 use of the acceleration of convergence technique
   REAL(wp), PUBLIC ::   atfp            !: asselin time filter parameter
   REAL(wp), PUBLIC ::   rdt             !: time step for the dynamics (and tracer if nacc=0)
   REAL(wp), PUBLIC ::   rdtmin          !: minimum time step on tracers
   REAL(wp), PUBLIC ::   rdtmax          !: maximum time step on tracers
   REAL(wp), PUBLIC ::   rdth            !: depth variation of tracer step
   INTEGER , PUBLIC ::   nclosea         !: =0 suppress closed sea/lake from the ORCA domain or not (=1)

   !                                                  !!! associated variables
   INTEGER , PUBLIC                 ::   neuler  = 0   !: restart euler forward option (0=Euler)
   REAL(wp), PUBLIC                 ::   atfp1         !: asselin time filter coeff. (atfp1= 1-2*atfp)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   rdttra  !: vertical profile of tracer time step
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   r2dtra  !: = 2*rdttra except at nit000 (=rdttra) if neuler=0

   !                                         !!* Namelist namcla : cross land advection
   INTEGER, PUBLIC ::   nn_cla = 0            !: =1 cross land advection for exchanges through some straits (ORCA2)

   !!----------------------------------------------------------------------
   !! space domain parameters
   !!----------------------------------------------------------------------
   LOGICAL, PUBLIC ::   lzoom      =  .FALSE.   !: zoom flag
   LOGICAL, PUBLIC ::   lzoom_e    =  .FALSE.   !: East  zoom type flag
   LOGICAL, PUBLIC ::   lzoom_w    =  .FALSE.   !: West  zoom type flag
   LOGICAL, PUBLIC ::   lzoom_s    =  .FALSE.   !: South zoom type flag
   LOGICAL, PUBLIC ::   lzoom_n    =  .FALSE.   !: North zoom type flag
   LOGICAL, PUBLIC ::   lzoom_arct =  .FALSE.   !: ORCA    arctic zoom flag
   LOGICAL, PUBLIC ::   lzoom_anta =  .FALSE.   !: ORCA antarctic zoom flag

   !                                     !!! domain parameters linked to mpp
   INTEGER, PUBLIC ::   nperio            !: type of lateral boundary condition
   INTEGER, PUBLIC ::   nimpp, njmpp      !: i- & j-indexes for mpp-subdomain left bottom
   INTEGER, PUBLIC ::   nreci, nrecj      !: overlap region in i and j
   INTEGER, PUBLIC ::   nproc             !: number for local processor
   INTEGER, PUBLIC ::   narea             !: number for local area
   INTEGER, PUBLIC ::   nbondi, nbondj    !: mark of i- and j-direction local boundaries
   INTEGER, PUBLIC ::   npolj             !: north fold mark (0, 3 or 4)
   INTEGER, PUBLIC ::   nlci, nldi, nlei  !: i-dimensions of the local subdomain and its first and last indoor indices
   INTEGER, PUBLIC ::   nlcj, nldj, nlej  !: i-dimensions of the local subdomain and its first and last indoor indices
   INTEGER, PUBLIC ::   noea, nowe        !: index of the local neighboring processors in
   INTEGER, PUBLIC ::   noso, nono        !: east, west, south and north directions
   INTEGER, PUBLIC ::   npne, npnw        !: index of north east and north west processor
   INTEGER, PUBLIC ::   npse, npsw        !: index of south east and south west processor
   INTEGER, PUBLIC ::   nbne, nbnw        !: logical of north east & north west processor
   INTEGER, PUBLIC ::   nbse, nbsw        !: logical of south east & south west processor
   INTEGER, PUBLIC ::   nidom             !: ???

   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   mig        !: local  ==> global domain i-index
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   mjg        !: local  ==> global domain j-index
   INTEGER, PUBLIC,               DIMENSION(jpidta) ::   mi0, mi1   !: global ==> local  domain i-index    !!bug ==> other solution?
   !                                                  ! (mi0=1 and mi1=0 if the global index is not in the local domain)
   INTEGER, PUBLIC,               DIMENSION(jpjdta) ::   mj0, mj1   !: global ==> local  domain j-index     !!bug ==> other solution?
   !                                                  ! (mi0=1 and mi1=0 if the global index is not in the local domain)
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   nimppt, njmppt   !: i-, j-indexes for each processor
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ibonit, ibonjt   !: i-, j- processor neighbour existence
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   nlcit , nlcjt    !: dimensions of every subdomain
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   nldit , nldjt    !: first, last indoor index for each i-domain
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   nleit , nlejt    !: first, last indoor index for each j-domain

   !!----------------------------------------------------------------------
   !! horizontal curvilinear coordinate and scale factors
   !! ---------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  glamt, glamu   !: longitude of t-, u-, v- and f-points (degre)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  glamv, glamf   !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  gphit, gphiu   !: latitude  of t-, u-, v- and f-points (degre)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  gphiv, gphif   !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  e1t, e2t       !: horizontal scale factors at t-point (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  e1u, e2u       !: horizontal scale factors at u-point (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  e1v, e2v       !: horizontal scale factors at v-point (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  e1f, e2f       !: horizontal scale factors at f-point (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  e1e2t          !: surface at t-point (m2)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  ff             !: coriolis factor (2.*omega*sin(yphi) ) (s-1)

   !!----------------------------------------------------------------------
   !! vertical coordinate and scale factors
   !! ---------------------------------------------------------------------
   !                                           !!* Namelist namzgr : vertical coordinate *
   LOGICAL, PUBLIC ::   ln_zco     =  .TRUE.    !: z-coordinate - full step
   LOGICAL, PUBLIC ::   ln_zps     =  .FALSE.   !: z-coordinate - partial step
   LOGICAL, PUBLIC ::   ln_sco     =  .FALSE.   !: s-coordinate or hybrid z-s coordinate

   !! All coordinates
   !! ---------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   gdep3w          !: depth of T-points (sum of e3w) (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   gdept , gdepw   !: analytical depth at T-W  points (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e3v   , e3f     !: analytical vertical scale factors at  V--F
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e3t   , e3u     !:                                       T--U  points (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e3vw            !: analytical vertical scale factors at  VW--
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e3w   , e3uw    !:                                        W--UW  points (m)
#if defined key_vvl
   LOGICAL, PUBLIC, PARAMETER ::   lk_vvl = .TRUE.    !: variable grid flag

   !! All coordinates
   !! ---------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   gdep3w_1           !: depth of T-points (sum of e3w) (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   gdept_1, gdepw_1   !: analytical depth at T-W  points (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e3v_1  , e3f_1     !: analytical vertical scale factors at  V--F
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e3t_1  , e3u_1     !:                                       T--U  points (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e3vw_1             !: analytical vertical scale factors at  VW--
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e3w_1  , e3uw_1    !:                                       W--UW  points (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e3t_b              !: before         -      -      -    -   T      points (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e3u_b  , e3v_b     !:   -            -      -      -    -   U--V   points (m)
#else
   LOGICAL, PUBLIC, PARAMETER ::   lk_vvl = .FALSE.   !: fixed grid flag
#endif
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:), TARGET ::   hur  , hvr    !: inverse of u and v-points ocean depth (1/m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)         ::   hu   , hv     !: depth at u- and v-points (meters)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)         ::   hu_0 , hv_0   !: refernce depth at u- and v-points (meters)

   INTEGER, PUBLIC ::   nla10              !: deepest    W level Above  ~10m (nlb10 - 1)
   INTEGER, PUBLIC ::   nlb10              !: shallowest W level Bellow ~10m (nla10 + 1) 

   !! z-coordinate with full steps (also used in the other cases as reference z-coordinate)
   !! =-----------------====------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   :: gdept_0, gdepw_0 !: reference depth of t- and w-points (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   :: e3t_0  , e3w_0   !: reference vertical scale factors at T- and W-pts (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: e3tp   , e3wp    !: ocean bottom level thickness at T and W points

   !! s-coordinate and hybrid z-s-coordinate
   !! =----------------======---------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   gsigt, gsigw   !: model level depth coefficient at t-, w-levels (analytic)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   gsi3w          !: model level depth coefficient at w-level (sum of gsigw)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   esigt, esigw   !: vertical scale factor coef. at t-, w-levels

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hbatv , hbatf    !: ocean depth at the vertical of  V--F
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hbatt , hbatu    !:                                 T--U  points (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   scosrf, scobot   !: ocean surface and bottom topographies 
   !                                        !  (if deviating from coordinate surfaces in HYBRID)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hifv  , hiff     !: interface depth between stretching at  V--F
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hift  , hifu     !: and quasi-uniform spacing              T--U  points (m)

   !!----------------------------------------------------------------------
   !! masks, bathymetry
   !! ---------------------------------------------------------------------
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mbathy       !: number of ocean level (=0, 1, ... , jpk-1)
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mbkt         !: vertical index of the bottom last T- ocean level
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mbku, mbkv   !: vertical index of the bottom last U- and W- ocean level
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   bathy        !: ocean depth (meters)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   tmask_i      !: interior domain T-point mask
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   bmask        !: land/ocean mask of barotropic stream function

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: tmask, umask, vmask, fmask   !: land/ocean mask at T-, U-, V- and F-pts

   REAL(wp), PUBLIC, DIMENSION(jpiglo) ::   tpol, fpol          !: north fold mask (jperio= 3 or 4)

#if defined key_noslip_accurate
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:  ) :: npcoa        !: ???
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: nicoa, njcoa !: ???
#endif

   !!----------------------------------------------------------------------
   !! calendar variables
   !! ---------------------------------------------------------------------
   INTEGER , PUBLIC ::   nyear         !: current year
   INTEGER , PUBLIC ::   nmonth        !: current month
   INTEGER , PUBLIC ::   nday          !: current day of the month
   INTEGER , PUBLIC ::   ndastp        !: time step date in yyyymmdd format
   INTEGER , PUBLIC ::   nday_year     !: current day counted from jan 1st of the current year
   INTEGER , PUBLIC ::   nsec_year     !: current time step counted in second since 00h jan 1st of the current year
   INTEGER , PUBLIC ::   nsec_month    !: current time step counted in second since 00h 1st day of the current month
   INTEGER , PUBLIC ::   nsec_week     !: current time step counted in second since 00h of last monday
   INTEGER , PUBLIC ::   nsec_day      !: current time step counted in second since 00h of the current day
   REAL(wp), PUBLIC ::   fjulday       !: current julian day 
   REAL(wp), PUBLIC ::   fjulstartyear !: first day of the current year in julian days
   REAL(wp), PUBLIC ::   adatrj        !: number of elapsed days since the begining of the whole simulation
   !                                   !: (cumulative duration of previous runs that may have used different time-step size)
   INTEGER , PUBLIC, DIMENSION(0: 1) ::   nyear_len     !: length in days of the previous/current year
   INTEGER , PUBLIC, DIMENSION(0:13) ::   nmonth_len    !: length in days of the months of the current year
   INTEGER , PUBLIC, DIMENSION(0:13) ::   nmonth_half   !: second since Jan 1st 0h of the current year and the half of the months
   INTEGER , PUBLIC, DIMENSION(0:13) ::   nmonth_end    !: second since Jan 1st 0h of the current year and the end of the months
   INTEGER , PUBLIC                  ::   nsec1jan000   !: second since Jan 1st 0h of nit000 year and Jan 1st 0h the current year

   !!----------------------------------------------------------------------
   !! mpp reproducibility
   !!----------------------------------------------------------------------
#if defined key_mpp_rep
   LOGICAL, PUBLIC, PARAMETER ::   lk_mpp_rep = .TRUE.    !: agrif flag
#else
   LOGICAL, PUBLIC, PARAMETER ::   lk_mpp_rep = .FALSE.   !: agrif flag
#endif

   !!----------------------------------------------------------------------
   !! agrif domain
   !!----------------------------------------------------------------------
#if defined key_agrif
   LOGICAL, PUBLIC, PARAMETER ::   lk_agrif = .TRUE.    !: agrif flag
#else
   LOGICAL, PUBLIC, PARAMETER ::   lk_agrif = .FALSE.   !: agrif flag
#endif

   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: dom_oce.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

#if ! defined key_agrif
   !!----------------------------------------------------------------------
   !! NOT 'key_agrif'      dummy function                     No AGRIF zoom
   !!----------------------------------------------------------------------
   LOGICAL FUNCTION Agrif_Root()
      Agrif_Root = .TRUE.
   END FUNCTION Agrif_Root

   CHARACTER(len=3) FUNCTION Agrif_CFixed()
      Agrif_CFixed = '0' 
   END FUNCTION Agrif_CFixed
#endif

   INTEGER FUNCTION dom_oce_alloc()
      !!----------------------------------------------------------------------
      INTEGER, DIMENSION(11) :: ierr
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !
      ALLOCATE( rdttra(jpk), r2dtra(jpk), mig(jpi), mjg(jpj), STAT=ierr(1) )
         !
      ALLOCATE( nimppt(jpnij) , ibonit(jpnij) , nlcit(jpnij) , nlcjt(jpnij) ,     &
         &      njmppt(jpnij) , ibonjt(jpnij) , nldit(jpnij) , nldjt(jpnij) ,     &
         &                                      nleit(jpnij) , nlejt(jpnij) , STAT=ierr(2) )
         !
      ALLOCATE( glamt(jpi,jpj) , gphit(jpi,jpj) , e1t(jpi,jpj) , e2t(jpi,jpj) ,                      & 
         &      glamu(jpi,jpj) , gphiu(jpi,jpj) , e1u(jpi,jpj) , e2u(jpi,jpj) ,                      &  
         &      glamv(jpi,jpj) , gphiv(jpi,jpj) , e1v(jpi,jpj) , e2v(jpi,jpj) , e1e2t(jpi,jpj) ,     &  
         &      glamf(jpi,jpj) , gphif(jpi,jpj) , e1f(jpi,jpj) , e2f(jpi,jpj) , ff   (jpi,jpj) , STAT=ierr(3) )     
         !
      ALLOCATE( gdep3w(jpi,jpj,jpk) , e3v(jpi,jpj,jpk) , e3f (jpi,jpj,jpk) ,                         &
         &      gdept (jpi,jpj,jpk) , e3t(jpi,jpj,jpk) , e3u (jpi,jpj,jpk) ,                         &
         &      gdepw (jpi,jpj,jpk) , e3w(jpi,jpj,jpk) , e3vw(jpi,jpj,jpk) , e3uw(jpi,jpj,jpk) , STAT=ierr(4) )
         !
#if defined key_vvl
      ALLOCATE( gdep3w_1(jpi,jpj,jpk) , e3v_1(jpi,jpj,jpk) , e3f_1 (jpi,jpj,jpk) ,                           &
         &      gdept_1 (jpi,jpj,jpk) , e3t_1(jpi,jpj,jpk) , e3u_1 (jpi,jpj,jpk) ,                           &
         &      gdepw_1 (jpi,jpj,jpk) , e3w_1(jpi,jpj,jpk) , e3vw_1(jpi,jpj,jpk) , e3uw_1(jpi,jpj,jpk) ,     &
         &      e3t_b   (jpi,jpj,jpk) , e3u_b(jpi,jpj,jpk) , e3v_b (jpi,jpj,jpk)                       , STAT=ierr(5) )
#endif
         !
      ALLOCATE( hu(jpi,jpj) , hur(jpi,jpj) , hu_0(jpi,jpj) ,     &
         &      hv(jpi,jpj) , hvr(jpi,jpj) , hv_0(jpi,jpj) , STAT=ierr(6) )
         !
      ALLOCATE( gdept_0(jpk) , gdepw_0(jpk) ,                                     &
         &      e3t_0  (jpk) , e3w_0  (jpk) , e3tp (jpi,jpj), e3wp(jpi,jpj) ,     &
         &      gsigt  (jpk) , gsigw  (jpk) , gsi3w(jpk)    ,                     &
         &      esigt  (jpk) , esigw  (jpk)                                 , STAT=ierr(7) )
         !
      ALLOCATE( hbatv (jpi,jpj) , hbatf (jpi,jpj) ,     &
         &      hbatt (jpi,jpj) , hbatu (jpi,jpj) ,     &
         &      scosrf(jpi,jpj) , scobot(jpi,jpj) ,     &
         &      hifv  (jpi,jpj) , hiff  (jpi,jpj) ,     &
         &      hift  (jpi,jpj) , hifu  (jpi,jpj) , STAT=ierr(8) )

      ALLOCATE( mbathy(jpi,jpj) , bathy(jpi,jpj) ,                     &
         &     tmask_i(jpi,jpj) , bmask(jpi,jpj) ,                     &
         &     mbkt   (jpi,jpj) , mbku (jpi,jpj) , mbkv(jpi,jpj) , STAT=ierr(9) )

      ALLOCATE( tmask(jpi,jpj,jpk) , umask(jpi,jpj,jpk),     & 
         &      vmask(jpi,jpj,jpk) , fmask(jpi,jpj,jpk), STAT=ierr(10) )

#if defined key_noslip_accurate
      ALLOCATE( npcoa(4,jpk), nicoa(2*(jpi+jpj),4,jpk), njcoa(2*(jpi+jpj),4,jpk), STAT=ierr(11) )
#endif
      !
      dom_oce_alloc = MAXVAL(ierr)
      !
   END FUNCTION dom_oce_alloc

   !!======================================================================
END MODULE dom_oce
