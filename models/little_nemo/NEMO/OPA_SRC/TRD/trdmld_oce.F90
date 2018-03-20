MODULE trdmld_oce
   !!======================================================================
   !!                   ***  MODULE trdmld_oce  ***
   !! Ocean trends :   set tracer and momentum trend variables
   !!======================================================================
   !! History :  1.0  ! 2004-08  (C. Talandier)  New trends organization
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trdmld_oce_alloc    ! Called in trdmld.F90

#if defined key_trdmld
   LOGICAL, PUBLIC, PARAMETER ::   lk_trdmld = .TRUE.    !: ML trend flag
#else
   LOGICAL, PUBLIC, PARAMETER ::   lk_trdmld = .FALSE.   !: ML trend flag
#endif
   !!* mixed layer trends indices
   INTEGER, PARAMETER, PUBLIC ::   jpltrd = 11      !: number of mixed-layer trends arrays
   INTEGER, PUBLIC            ::   jpktrd           !: max level for mixed-layer trends diag.
   !
   INTEGER, PUBLIC, PARAMETER ::   jpmld_xad =  1   !:  zonal      
   INTEGER, PUBLIC, PARAMETER ::   jpmld_yad =  2   !:  meridonal   > advection
   INTEGER, PUBLIC, PARAMETER ::   jpmld_zad =  3   !:  vertical   
   INTEGER, PUBLIC, PARAMETER ::   jpmld_ldf =  4   !:  lateral diffusion (geopot. or iso-neutral)
   INTEGER, PUBLIC, PARAMETER ::   jpmld_for =  5   !:  forcing 
   INTEGER, PUBLIC, PARAMETER ::   jpmld_zdf =  6   !:  vertical diffusion (TKE)
   INTEGER, PUBLIC, PARAMETER ::   jpmld_bbc =  7   !:  geothermal flux
   INTEGER, PUBLIC, PARAMETER ::   jpmld_bbl =  8   !:  bottom boundary layer (advective/diffusive)
   INTEGER, PUBLIC, PARAMETER ::   jpmld_dmp =  9   !:  internal restoring trend
   INTEGER, PUBLIC, PARAMETER ::   jpmld_npc = 10   !:  non penetrative convective adjustment
!! INTEGER, PUBLIC, PARAMETER ::   jpmld_xxx = xx   !:  add here any additional trend (add change jpltrd)
   INTEGER, PUBLIC, PARAMETER ::   jpmld_atf = 11   !:  asselin trend (**MUST BE THE LAST ONE**)

#if   defined  key_trdmld   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_trdmld'                         mixed layer trends diagnostics
   !!----------------------------------------------------------------------

   !! Arrays used for diagnosing mixed-layer trends 
   !!---------------------------------------------------------------------
   CHARACTER(LEN=80) , PUBLIC :: clname, ctrd(jpltrd+1,2)

   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   nmld   !: mixed layer depth indexes 
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   nbol   !: mixed-layer depth indexes when read from file

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   wkx    !:

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  &
      rmld   ,                      & !: mld depth (m) corresponding to nmld
      tml    , sml  ,               & !: \ "now" mixed layer temperature/salinity
      tmlb   , smlb ,               & !: /  and associated "before" fields
      tmlbb  , smlbb,               & !: \  idem, but valid at the 1rst time step of the
      tmlbn  , smlbn,               & !: /  current analysis window
      tmltrdm, smltrdm,             & !: total cumulative trends over the analysis window
      tml_sum,                      & !: mixed layer T, summed over the current analysis period
      tml_sumb,                     & !: idem, but from the previous analysis period
      tmltrd_atf_sumb,              & !: Asselin trends, summed over the previous analysis period
      sml_sum,                      & !: 
      sml_sumb,                     & !:    ( idem for salinity )
      smltrd_atf_sumb,              & !: 
      rmld_sum, rmldbn                !: needed to compute the leap-frog time mean of the ML depth

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  &
      tmlatfb, tmlatfn ,            & !: "before" Asselin contribution at begining of the averaging
      smlatfb, smlatfn,             & !: period (i.e. last contrib. from previous such period) and 
                                      !: "now" Asselin contribution to the ML temp. & salinity trends
      tmlatfm, smlatfm                !: accumulator for Asselin trends (needed for storage only)

   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) ::  &
      tmltrd,                       & !: \ physical contributions to the total trend (for T/S),
      smltrd,                       & !: / cumulated over the current analysis window
      tmltrd_sum,                   & !: sum of these trends over the analysis period
      tmltrd_csum_ln,               & !: now cumulated sum of the trends over the "lower triangle"
      tmltrd_csum_ub,               & !: before (prev. analysis period) cumulated sum over the upper triangle
      smltrd_sum,                   & !: 
      smltrd_csum_ln,               & !:    ( idem for salinity )
      smltrd_csum_ub                  !: 
#endif
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: trdmld_oce.F90 2715 2011-03-30 15:58:35Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

  INTEGER FUNCTION trdmld_oce_alloc()
     !!----------------------------------------------------------------------
     !!                 ***  FUNCTION trdmld_oce_alloc   ***
     !!----------------------------------------------------------------------
     USE lib_mpp
     INTEGER :: ierr(5)
     !!----------------------------------------------------------------------

     ! Initialise jpktrd here as can no longer do it in MODULE body since
     ! jpk is now a variable.
     jpktrd = jpk   !: max level for mixed-layer trends diag.

     ierr(:) = 0

#if   defined  key_trdmld   ||   defined key_esopa
     ALLOCATE( nmld(jpi,jpj), nbol(jpi,jpj),       &
        &      wkx(jpi,jpj,jpk), rmld(jpi,jpj),    & 
        &      tml(jpi,jpj)    , sml(jpi,jpj),     & 
        &      tmlb(jpi,jpj)   , smlb(jpi,jpj) ,   &
        &      tmlbb(jpi,jpj)  , smlbb(jpi,jpj), STAT = ierr(1) )

     ALLOCATE( tmlbn(jpi,jpj)  , smlbn(jpi,jpj),   &
        &      tmltrdm(jpi,jpj), smltrdm(jpi,jpj), &
        &      tml_sum(jpi,jpj), tml_sumb(jpi,jpj),&
        &      tmltrd_atf_sumb(jpi,jpj)           , STAT=ierr(2) )

     ALLOCATE( sml_sum(jpi,jpj), sml_sumb(jpi,jpj), &
        &      smltrd_atf_sumb(jpi,jpj),            &
        &      rmld_sum(jpi,jpj), rmldbn(jpi,jpj),  &
        &      tmlatfb(jpi,jpj), tmlatfn(jpi,jpj), STAT = ierr(3) )

     ALLOCATE( smlatfb(jpi,jpj), smlatfn(jpi,jpj), & 
        &      tmlatfm(jpi,jpj), smlatfm(jpi,jpj), &
        &      tmltrd(jpi,jpj,jpltrd),   smltrd(jpi,jpj,jpltrd), STAT=ierr(4))

     ALLOCATE( tmltrd_sum(jpi,jpj,jpltrd),tmltrd_csum_ln(jpi,jpj,jpltrd),      &
        &      tmltrd_csum_ub(jpi,jpj,jpltrd), smltrd_sum(jpi,jpj,jpltrd),     &
        &      smltrd_csum_ln(jpi,jpj,jpltrd), smltrd_csum_ub(jpi,jpj,jpltrd), STAT=ierr(5) )
#endif
      !
      trdmld_oce_alloc = MAXVAL( ierr )
      IF( lk_mpp                )   CALL mpp_sum ( trdmld_oce_alloc )
      IF( trdmld_oce_alloc /= 0 )   CALL ctl_warn('trdmld_oce_alloc: failed to allocate arrays')
      !
   END FUNCTION trdmld_oce_alloc

   !!======================================================================
END MODULE trdmld_oce
