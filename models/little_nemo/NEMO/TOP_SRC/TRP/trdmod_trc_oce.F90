MODULE trdmod_trc_oce
   !!======================================================================
   !!                   ***  MODULE trdmod_trc_oce  ***
   !! Ocean trends :   set tracer and momentum trend variables
   !!======================================================================
#if defined key_top   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   USE par_oce       ! ocean parameters
   USE par_trc       ! passive tracers parameters

   IMPLICIT NONE
   PUBLIC

   !                                         !!* Namelist namtoptrd:  diagnostics on passive tracers trends
   INTEGER  ::    nn_trd_trc                  !: time step frequency dynamics and tracers trends
   INTEGER  ::    nn_ctls_trc                 !: control surface type for trends vertical integration
   REAL(wp) ::    rn_ucf_trc                  !: unit conversion factor (for netCDF trends outputs)
   LOGICAL  ::    ln_trdmld_trc_instant       !: flag to diagnose inst./mean ML trc trends
   LOGICAL  ::    ln_trdmld_trc_restart       !: flag to restart mixed-layer trc diagnostics
   CHARACTER(len=50) ::  cn_trdrst_trc_in     !: suffix of pass. tracer restart name (input)
   CHARACTER(len=50) ::  cn_trdrst_trc_out    !: suffix of pass. tracer restart name (output)
   LOGICAL, DIMENSION(jptra) ::   ln_trdtrc   !: large trends diagnostic to write or not (namelist)

# if defined key_trdtrc && defined key_iomput
   LOGICAL, PARAMETER ::   lk_trdtrc = .TRUE. 
# else
   LOGICAL, PARAMETER ::   lk_trdtrc = .FALSE.   !: ML trend flag
# endif

# if defined key_trdmld_trc   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_trdmld_trc'                     mixed layer trends diagnostics
   !!----------------------------------------------------------------------

   LOGICAL, PARAMETER ::   lk_trdmld_trc = .TRUE.    !: ML trend flag

   INTEGER, PARAMETER ::            & !: mixed layer trends indices
        jpmld_trc_xad     =  1,     & !:     zonal       advection     
        jpmld_trc_yad     =  2,     & !:     meridonal   =========
        jpmld_trc_zad     =  3,     & !:     vertical    =========
        jpmld_trc_ldf     =  4,     & !:     lateral diffusion (geopot. or iso-neutral)
        jpmld_trc_zdf     =  5,     & !:     vertical diffusion (TKE)
        jpmld_trc_bbl     =  6,     & !:     bottom boundary layer (advective/diffusive)
        jpmld_trc_dmp     =  7,     & !:     internal restoring trend
        jpmld_trc_sbc     =  8,     & !:     forcing 
        jpmld_trc_sms     =  9,     & !:     sources minus sinks trend
  !     jpmld_trc_xxx     = xx,     & !:     add here any additional trend    (** AND UPDATE JPLTRD_TRC BELOW **)
        jpmld_trc_radn    = 10,     & !:     corr. trn<0 in trcrad
        jpmld_trc_radb    = 11,     & !:     corr. trb<0 in trcrad (like atf) (** MUST BE BEFORE THE LAST ONE **)
        jpmld_trc_atf     = 12        !:     asselin trend                    (** MUST BE    THE      LAST ONE**)

   !! Trends diagnostics parameters
   !!---------------------------------------------------------------------
   INTEGER, PARAMETER :: jpltrd_trc = 12    !: number of mixed-layer trends arrays
      
   INTEGER            :: jpktrd_trc         !: max level for mixed-layer trends diag.

   !! Arrays used for diagnosing mixed-layer trends 
   !!---------------------------------------------------------------------
   CHARACTER(LEN=80) :: clname_trc, ctrd_trc(jpltrd_trc+1,2)

   INTEGER, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   &
      nmld_trc       , &                            !: mixed layer depth indexes 
      nbol_trc                                   !: mixed-layer depth indexes when read from file

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  wkx_trc  !:

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  rmld_trc     !: ML depth (m) corresponding to nmld_trc
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  rmld_sum_trc !: needed to compute the leap-frog time mean of ML depth
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  rmldbn_trc   !: idem

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  &
      tml_trc    ,                        &      !: \ "now" mixed layer temperature/salinity
      tmlb_trc   ,                        &      !: /  and associated "before" fields
      tmlbb_trc  ,                        &      !: \  idem, but valid at the 1rst time step of the
      tmlbn_trc  ,                        &      !: /  current analysis window
      tml_sum_trc,                        &      !: mixed layer T, summed over the current analysis period
      tml_sumb_trc,                       &      !: idem, but from the previous analysis period
      tmltrd_atf_sumb_trc,                &      !: Asselin trends, summed over the previous analysis period
      tmltrd_rad_sumb_trc                        !: trends due to trb correction in trcrad.F90, summed over the
                                                 !:     previous analysis period
                                                 
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  &      
      tmlatfb_trc, tmlatfn_trc ,          &      !: "before" Asselin contrib. at beginning of the averaging
                                                 !:     period (i.e. last contrib. from previous such period)
                                                 !:     and "now" Asselin contrib. to the ML trc. trends
      tmlatfm_trc,                        &      !: accumulator for Asselin trends (needed for storage only)
      tmlradb_trc, tmlradn_trc ,          &      !: similar to Asselin above, but for the trend due to trb
                                                 !:     correction in trcrad.F90
      tmlradm_trc                                !: accumulator for the previous trcrad trend

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::  &
      tmltrd_trc,                         &      !: \ physical contributions to the total trend (for T/S),
                                                 !: / cumulated over the current analysis window
      tmltrd_sum_trc,                     &      !: sum of these trends over the analysis period
      tmltrd_csum_ln_trc,                 &      !: now cumulated sum of trends over the "lower triangle"
      tmltrd_csum_ub_trc                         !: before (prev. analysis period) cumulated sum over the
                                                 !: upper triangle
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  &
      tmltrdm_trc                                !: total cumulative trends over the analysis window

# else
   LOGICAL, PARAMETER ::   lk_trdmld_trc = .FALSE.   !: ML trend flag
# endif

# if defined key_lobster
   CHARACTER(LEN=80) :: clname_bio, ctrd_bio(jpdiabio,2)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  &
      tmltrd_bio,                         &      !: \ biological contributions to the total trend ,
                                                 !: / cumulated over the current analysis window
      tmltrd_sum_bio,                     &      !: sum of these trends over the analysis period
      tmltrd_csum_ln_bio,                 &      !: now cumulated sum of trends over the "lower triangle"
      tmltrd_csum_ub_bio                         !: before (prev. analysis period) cumulated sum over the
                                                 !: upper triangle
#endif
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Header: /home/opalod/NEMOCVSROOT/NEMO/OPA_SRC/TRD/trdmld_oce.F90,v 1.2 2005/03/27 18:35:23 opalod Exp $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION trd_mod_trc_oce_alloc()
      !!----------------------------------------------------------------------
      !!         *** ROUTINE trd_mod_trc_oce_alloc ***
      !!----------------------------------------------------------------------
      USE lib_mpp, ONLY: ctl_warn
      INTEGER :: ierr(2)
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !
# if defined key_trdmld_trc
      ALLOCATE(nmld_trc(jpi,jpj),          nbol_trc(jpi,jpj),           &
               wkx_trc(jpi,jpj,jpk),       rmld_trc(jpi,jpj),           &
               rmld_sum_trc(jpi,jpj),      rmldbn_trc(jpi,jpj),         &
               tml_trc(jpi,jpj,jptra),     tmlb_trc(jpi,jpj,jptra),     &
               tmlbb_trc(jpi,jpj,jptra),   tmlbn_trc(jpi,jpj,jptra),    &
               tml_sum_trc(jpi,jpj,jptra), tml_sumb_trc(jpi,jpj,jptra), &
               tmltrd_atf_sumb_trc(jpi,jpj,jptra),                      &
               tmltrd_rad_sumb_trc(jpi,jpj,jptra),                      &
               !
               tmlatfb_trc(jpi,jpj,jptra), tmlatfn_trc(jpi,jpj,jptra),  &
               tmlatfm_trc(jpi,jpj,jptra), tmlradb_trc(jpi,jpj,jptra),  &
               tmlradn_trc(jpi,jpj,jptra), tmlradm_trc(jpi,jpj,jptra),  &
               !
               tmltrd_trc(jpi,jpj,jpltrd_trc,jptra)         , &
               tmltrd_sum_trc(jpi,jpj,jpltrd_trc,jptra)     , &
               tmltrd_csum_ln_trc(jpi,jpj,jpltrd_trc,jptra) , &
               tmltrd_csum_ub_trc(jpi,jpj,jpltrd_trc,jptra) , &
               !
               tmltrdm_trc(jpi,jpj,jptra)                   , STAT=ierr(1) )
#endif
      !
# if defined key_lobster
      ALLOCATE( tmltrd_bio        (jpi,jpj,jpdiabio) ,     &
         &      tmltrd_sum_bio    (jpi,jpj,jpdiabio) ,     &
         &      tmltrd_csum_ln_bio(jpi,jpj,jpdiabio) ,     &
         &      tmltrd_csum_ub_bio(jpi,jpj,jpdiabio) , STAT=ierr(2) )
# endif
      !
      trd_mod_trc_oce_alloc = MAXVAL(ierr)
      !
      IF( trd_mod_trc_oce_alloc /= 0 )   CALL ctl_warn('trd_mod_trc_oce_alloc: failed to allocate arrays')
      !
# if defined key_trdmld_trc
      jpktrd_trc = jpk      ! Initialise what used to be a parameter - max level for mixed-layer trends diag.
# endif
      !
   END FUNCTION trd_mod_trc_oce_alloc

#else
   !!----------------------------------------------------------------------
   !!  Empty module :                                     No passive tracer
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE trdmod_trc_oce
