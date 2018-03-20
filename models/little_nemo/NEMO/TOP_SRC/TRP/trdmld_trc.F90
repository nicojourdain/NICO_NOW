MODULE trdmld_trc
   !!======================================================================
   !!                       ***  MODULE  trdmld_trc  ***
   !! Ocean diagnostics:  mixed layer passive tracer trends 
   !!======================================================================
   !! History :  9.0  !  06-08  (C. Deltel)  Original code (from trdmld.F90)
   !!                 !  07-04  (C. Deltel)  Bug fix : add trcrad trends
   !!                 !  07-06  (C. Deltel)  key_gyre : do not call lbc_lnk
   !!----------------------------------------------------------------------
#if   defined key_top && ( defined key_trdmld_trc   ||   defined key_esopa )
   !!----------------------------------------------------------------------
   !!   'key_trdmld_trc'                      mixed layer trend diagnostics
   !!----------------------------------------------------------------------
   !!   trd_mld_trc      : passive tracer cumulated trends averaged over ML
   !!   trd_mld_trc_zint : passive tracer trends vertical integration
   !!   trd_mld_trc_init : initialization step
   !!----------------------------------------------------------------------
   USE trc               ! tracer definitions (trn, trb, tra, etc.)
   USE dom_oce           ! domain definition
   USE zdfmxl  , ONLY : nmln !: number of level in the mixed layer
   USE zdf_oce , ONLY : avt  !: vert. diffusivity coef. at w-point for temp  
# if defined key_zdfddm   
   USE zdfddm  , ONLY : avs  !: salinity vertical diffusivity coeff. at w-point
# endif
   USE trcnam_trp        ! passive tracers transport namelist variables
   USE trdmod_trc_oce    ! definition of main arrays used for trends computations
   USE in_out_manager    ! I/O manager
   USE dianam            ! build the name of file (routine)
   USE ldfslp            ! iso-neutral slopes 
   USE ioipsl            ! NetCDF library
   USE lbclnk            ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp           ! MPP library
   USE trdmld_trc_rst    ! restart for diagnosing the ML trends
   USE prtctl            ! print control
   USE sms_pisces        ! PISCES bio-model
   USE sms_lobster       ! LOBSTER bio-model
   USE wrk_nemo          ! Memory allocation

   IMPLICIT NONE
   PRIVATE

   PUBLIC trd_mld_trc
   PUBLIC trd_mld_trc_alloc
   PUBLIC trd_mld_bio
   PUBLIC trd_mld_trc_init
   PUBLIC trd_mld_trc_zint
   PUBLIC trd_mld_bio_zint

   CHARACTER (LEN=40) ::  clhstnam                                ! name of the trends NetCDF file
   INTEGER ::   nmoymltrd
   INTEGER, ALLOCATABLE, SAVE, DIMENSION(:) ::   ndextrd1
   INTEGER, DIMENSION(jptra) ::   nidtrd, nh_t
   INTEGER ::   ndimtrd1                        
   INTEGER, SAVE ::  ionce, icount
#if defined key_lobster
   INTEGER ::   nidtrdbio, nh_tb
   INTEGER, SAVE ::  ioncebio, icountbio
   INTEGER, SAVE ::   nmoymltrdbio
#endif
   LOGICAL :: llwarn  = .TRUE.                                    ! this should always be .TRUE.
   LOGICAL :: lldebug = .TRUE.

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::  ztmltrd2   !
#if defined key_lobster
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  ztmltrdbio2  ! only needed for mean diagnostics in trd_mld_bio()
#endif

   !! * Substitutions
#  include "top_substitute.h90"
#  include "zdfddm_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Header:  $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION trd_mld_trc_alloc()
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_mld_trc_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( ztmltrd2(jpi,jpj,jpltrd_trc,jptra) ,      &
#if defined key_lobster
         &      ztmltrdbio2(jpi,jpj,jpdiabio)      ,      &
#endif
         &      ndextrd1(jpi*jpj)                  ,  STAT=trd_mld_trc_alloc)
         !
      IF( lk_mpp                )   CALL mpp_sum ( trd_mld_trc_alloc )
      IF( trd_mld_trc_alloc /=0 )   CALL ctl_warn('trd_mld_trc_alloc: failed to allocate arrays')
      !
   END FUNCTION trd_mld_trc_alloc


   SUBROUTINE trd_mld_trc_zint( ptrc_trdmld, ktrd, ctype, kjn )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_mld_trc_zint  ***
      !! 
      !! ** Purpose :   Compute the vertical average of the 3D fields given as arguments 
      !!                to the subroutine. This vertical average is performed from ocean
      !!                surface down to a chosen control surface.
      !!
      !! ** Method/usage :
      !!      The control surface can be either a mixed layer depth (time varying)
      !!      or a fixed surface (jk level or bowl). 
      !!      Choose control surface with nctls_trc in namelist NAMTRD :
      !!        nctls_trc = -2  : use isopycnal surface
      !!        nctls_trc = -1  : use euphotic layer with light criterion 
      !!        nctls_trc =  0  : use mixed layer with density criterion 
      !!        nctls_trc =  1  : read index from file 'ctlsurf_idx'
      !!        nctls_trc >  1  : use fixed level surface jk = nctls_trc
      !!      Note: in the remainder of the routine, the volume between the 
      !!            surface and the control surface is called "mixed-layer"
      !!----------------------------------------------------------------------
      !!
      INTEGER, INTENT( in ) ::   ktrd, kjn                        ! ocean trend index and passive tracer rank
      CHARACTER(len=2), INTENT( in ) ::  ctype                    ! surface/bottom (2D) or interior (3D) physics
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT( in ) ::  ptrc_trdmld ! passive tracer trend
      !
      INTEGER ::   ji, jj, jk, isum
      REAL(wp), POINTER, DIMENSION(:,:) :: zvlmsk
      !!----------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, zvlmsk )

      ! I. Definition of control surface and integration weights
      ! --------------------------------------------------------

      ONCE_PER_TIME_STEP : IF( icount == 1 ) THEN
         !
         tmltrd_trc(:,:,:,:) = 0.e0                               ! <<< reset trend arrays to zero
          
         ! ... Set nmld(ji,jj) = index of first T point below control surf. or outside mixed-layer
         SELECT CASE ( nn_ctls_trc )                                ! choice of the control surface
            CASE ( -2  )   ;   STOP 'trdmld_trc : not ready '     !     -> isopycnal surface (see ???)
#if defined key_pisces || defined key_lobster
            CASE ( -1  )   ;   nmld_trc(:,:) = neln(:,:)          !     -> euphotic layer with light criterion
#endif
            CASE (  0  )   ;   nmld_trc(:,:) = nmln(:,:)          !     -> ML with density criterion (see zdfmxl)
            CASE (  1  )   ;   nmld_trc(:,:) = nbol_trc(:,:)          !     -> read index from file
            CASE (  2: )   ;   nn_ctls_trc = MIN( nn_ctls_trc, jpktrd_trc - 1 )
                               nmld_trc(:,:) = nn_ctls_trc + 1      !     -> model level
         END SELECT

         ! ... Compute ndextrd1 and ndimtrd1  ??? role de jpktrd_trc
         IF( ionce == 1 ) THEN
            !
            isum  = 0   ;   zvlmsk(:,:) = 0.e0

            IF( jpktrd_trc < jpk ) THEN                           ! description ???
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     IF( nmld_trc(ji,jj) <= jpktrd_trc ) THEN
                        zvlmsk(ji,jj) = tmask(ji,jj,1)
                     ELSE
                        isum = isum + 1
                        zvlmsk(ji,jj) = 0.e0
                     ENDIF
                  END DO
               END DO
            ENDIF

            IF( isum > 0 ) THEN                                   ! index of ocean points (2D only)
               WRITE(numout,*)' tmltrd_trc : Number of invalid points nmld_trc > jpktrd', isum 
               CALL wheneq( jpi*jpj, zvlmsk(:,:) , 1, 1., ndextrd1, ndimtrd1 )
            ELSE 
               CALL wheneq( jpi*jpj, tmask(:,:,1), 1, 1., ndextrd1, ndimtrd1 )
            ENDIF                                

            ionce = 0                                             ! no more pass here
            !
         ENDIF   ! ionce == 1
         
         ! ... Weights for vertical averaging
         wkx_trc(:,:,:) = 0.e0
         DO jk = 1, jpktrd_trc                                    ! initialize wkx_trc with vertical scale factor in mixed-layer
            DO jj = 1, jpj
               DO ji = 1, jpi
                  IF( jk - nmld_trc(ji,jj) < 0 )   wkx_trc(ji,jj,jk) = fse3t(ji,jj,jk) * tmask(ji,jj,jk)
               END DO
            END DO
         END DO
         
         rmld_trc(:,:) = 0.e0
         DO jk = 1, jpktrd_trc                                    ! compute mixed-layer depth : rmld_trc
            rmld_trc(:,:) = rmld_trc(:,:) + wkx_trc(:,:,jk)
         END DO
         
         DO jk = 1, jpktrd_trc                                    ! compute integration weights
            wkx_trc(:,:,jk) = wkx_trc(:,:,jk) / MAX( 1., rmld_trc(:,:) )
         END DO

         icount = 0                                               ! <<< flag = off : control surface & integr. weights
         !                                                        !     computed only once per time step
      ENDIF ONCE_PER_TIME_STEP

      ! II. Vertical integration of trends in the mixed-layer
      ! -----------------------------------------------------

      SELECT CASE ( ctype )
         CASE ( '3D' )                                            ! mean passive tracer trends in the mixed-layer
            DO jk = 1, jpktrd_trc
               tmltrd_trc(:,:,ktrd,kjn) = tmltrd_trc(:,:,ktrd,kjn) + ptrc_trdmld(:,:,jk) * wkx_trc(:,:,jk)   
            END DO
         CASE ( '2D' )                                            ! forcing at upper boundary of the mixed-layer 
            tmltrd_trc(:,:,ktrd,kjn) = tmltrd_trc(:,:,ktrd,kjn) + ptrc_trdmld(:,:,1) * wkx_trc(:,:,1)  ! non penetrative
      END SELECT
      !
      CALL wrk_dealloc( jpi, jpj, zvlmsk )
      !
   END SUBROUTINE trd_mld_trc_zint


   SUBROUTINE trd_mld_bio_zint( ptrc_trdmld, ktrd )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_mld_bio_zint  ***
      !!
      !! ** Purpose :   Compute the vertical average of the 3D fields given as arguments
      !!                to the subroutine. This vertical average is performed from ocean
      !!                surface down to a chosen control surface.
      !!
      !! ** Method/usage :
      !!      The control surface can be either a mixed layer depth (time varying)
      !!      or a fixed surface (jk level or bowl).
      !!      Choose control surface with nctls in namelist NAMTRD :
      !!        nctls_trc = 0  : use mixed layer with density criterion
      !!        nctls_trc = 1  : read index from file 'ctlsurf_idx'
      !!        nctls_trc > 1  : use fixed level surface jk = nctls_trc
      !!      Note: in the remainder of the routine, the volume between the
      !!            surface and the control surface is called "mixed-layer"
      !!----------------------------------------------------------------------
      !!
      INTEGER                         , INTENT(in) ::   ktrd          ! bio trend index
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in) ::   ptrc_trdmld   ! passive trc trend
#if defined key_lobster
      !
      INTEGER ::   ji, jj, jk, isum
      REAL(wp), POINTER, DIMENSION(:,:) :: zvlmsk
      !!----------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, zvlmsk )

      ! I. Definition of control surface and integration weights
      ! --------------------------------------------------------
      !            ==> only once per time step <==

      IF( icountbio == 1 ) THEN
         !
         tmltrd_bio(:,:,:) = 0.e0    ! <<< reset trend arrays to zero
         ! ... Set nmld(ji,jj) = index of first T point below control surf. or outside mixed-layer
         SELECT CASE ( nn_ctls_trc )                                    ! choice of the control surface
            CASE ( -2  )   ;   STOP 'trdmld_trc : not ready '     !     -> isopycnal surface (see ???)
            CASE ( -1  )   ;   nmld_trc(:,:) = neln(:,:)          !     -> euphotic layer with light criterion
            CASE (  0  )   ;   nmld_trc(:,:) = nmln(:,:)          !     -> ML with density criterion (see zdfmxl)
            CASE (  1  )   ;   nmld_trc(:,:) = nbol_trc(:,:)          !     -> read index from file
            CASE (  2: )   ;   nn_ctls_trc = MIN( nn_ctls_trc, jpktrd_trc - 1 )
                               nmld_trc(:,:) = nn_ctls_trc + 1          !     -> model level
         END SELECT

         ! ... Compute ndextrd1 and ndimtrd1 only once
         IF( ioncebio == 1 ) THEN
            !
            ! Check of validity : nmld_trc(ji,jj) <= jpktrd_trc
            isum        = 0
            zvlmsk(:,:) = 0.e0

            IF( jpktrd_trc < jpk ) THEN
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     IF( nmld_trc(ji,jj) <= jpktrd_trc ) THEN
                        zvlmsk(ji,jj) = tmask(ji,jj,1)
                     ELSE
                        isum = isum + 1
                        zvlmsk(ji,jj) = 0.
                     END IF
                  END DO
               END DO
            END IF

            ! Index of ocean points (2D only)
            IF( isum > 0 ) THEN
               WRITE(numout,*)' tmltrd_trc : Number of invalid points nmld_trc > jpktrd', isum
               CALL wheneq( jpi*jpj, zvlmsk(:,:) , 1, 1., ndextrd1, ndimtrd1 )
            ELSE
               CALL wheneq( jpi*jpj, tmask(:,:,1), 1, 1., ndextrd1, ndimtrd1 )
            END IF

            ioncebio = 0                  ! no more pass here
            !
         END IF !  ( ioncebio == 1 )

         ! ... Weights for vertical averaging
         wkx_trc(:,:,:) = 0.e0
         DO jk = 1, jpktrd_trc         ! initialize wkx_trc with vertical scale factor in mixed-layer
            DO jj = 1,jpj
              DO ji = 1,jpi
                  IF( jk - nmld_trc(ji,jj) < 0. )   wkx_trc(ji,jj,jk) = fse3t(ji,jj,jk) * tmask(ji,jj,jk)
               END DO
            END DO
         END DO

         rmld_trc(:,:) = 0.
         DO jk = 1, jpktrd_trc         ! compute mixed-layer depth : rmld_trc
            rmld_trc(:,:) = rmld_trc(:,:) + wkx_trc(:,:,jk)
         END DO

         DO jk = 1, jpktrd_trc         ! compute integration weights
            wkx_trc(:,:,jk) = wkx_trc(:,:,jk) / MAX( 1., rmld_trc(:,:) )
         END DO

         icountbio = 0                    ! <<< flag = off : control surface & integr. weights
         !                             !     computed only once per time step
      END IF ! ( icountbio == 1 )

      ! II. Vertical integration of trends in the mixed-layer
      ! -----------------------------------------------------


      DO jk = 1, jpktrd_trc
         tmltrd_bio(:,:,ktrd) = tmltrd_bio(:,:,ktrd) + ptrc_trdmld(:,:,jk) * wkx_trc(:,:,jk)
      END DO

      CALL wrk_alloc( jpi, jpj, zvlmsk )
#endif
      !
   END SUBROUTINE trd_mld_bio_zint


   SUBROUTINE trd_mld_trc( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_mld_trc  ***
      !! 
      !! ** Purpose :  Compute and cumulate the mixed layer trends over an analysis
      !!               period, and write NetCDF (or dimg) outputs.
      !!
      !! ** Method/usage :
      !!          The stored trends can be chosen twofold (according to the ln_trdmld_trc_instant 
      !!          logical namelist variable) :
      !!          1) to explain the difference between initial and final 
      !!             mixed-layer T & S (where initial and final relate to the
      !!             current analysis window, defined by ntrc_trc in the namelist)
      !!          2) to explain the difference between the current and previous 
      !!             TIME-AVERAGED mixed-layer T & S (where time-averaging is
      !!             performed over each analysis window).
      !!
      !! ** Consistency check : 
      !!        If the control surface is fixed ( nctls_trc > 1 ), the residual term (dh/dt
      !!        entrainment) should be zero, at machine accuracy. Note that in the case
      !!        of time-averaged mixed-layer fields, this residual WILL NOT BE ZERO
      !!        over the first two analysis windows (except if restart).
      !!        N.B. For ORCA2_LIM, use e.g. ntrc_trc=5, rn_ucf_trc=1., nctls_trc=8
      !!             for checking residuals.
      !!             On a NEC-SX5 computer, this typically leads to:
      !!                   O(1.e-20) temp. residuals (tml_res) when ln_trdmld_trc_instant=.false.
      !!                   O(1.e-21) temp. residuals (tml_res) when ln_trdmld_trc_instant=.true.
      !!
      !! ** Action :
      !!       At each time step, mixed-layer averaged trends are stored in the 
      !!       tmltrd(:,:,jpmld_xxx) array (see trdmld_oce.F90 for definitions of jpmld_xxx).
      !!       This array is known when trd_mld is called, at the end of the stp subroutine, 
      !!       except for the purely vertical K_z diffusion term, which is embedded in the
      !!       lateral diffusion trend.
      !!
      !!       In I), this K_z term is diagnosed and stored, thus its contribution is removed
      !!       from the lateral diffusion trend.
      !!       In II), the instantaneous mixed-layer T & S are computed, and misc. cumulative
      !!       arrays are updated.
      !!       In III), called only once per analysis window, we compute the total trends,
      !!       along with the residuals and the Asselin correction terms.
      !!       In IV), the appropriate trends are written in the trends NetCDF file.
      !!
      !! References :
      !!       - Vialard & al.
      !!       - See NEMO documentation (in preparation)
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !
      INTEGER ::   ji, jj, jk, jl, ik, it, itmod, jn
      REAL(wp) ::   zavt, zfn, zfn2
      !
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ztmltot             ! d(trc)/dt over the anlysis window (incl. Asselin)
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ztmlres             ! residual = dh/dt entrainment term
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ztmlatf             ! for storage only
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ztmlrad             ! for storage only (for trb<0 corr in trcrad)
      !
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ztmltot2            ! -+
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ztmlres2            !  | working arrays to diagnose the trends
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ztmltrdm2           !  | associated with the time meaned ML
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ztmlatf2            !  | passive tracers
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ztmlrad2            !  | (-> for trb<0 corr in trcrad)
      !
      CHARACTER (LEN=10) ::   clvar
#if defined key_dimgout
      INTEGER ::   iyear,imon,iday
      CHARACTER(LEN=80) ::   cltext, clmode
#endif
      !!----------------------------------------------------------------------

      ! Set-up pointers into sub-arrays of workspaces
      CALL wrk_alloc( jpi, jpj, jptra, ztmltot , ztmlres , ztmlatf , ztmlrad             )
      CALL wrk_alloc( jpi, jpj, jptra, ztmltot2, ztmlres2, ztmlatf2, ztmlrad2, ztmltrdm2 )

      IF( nn_dttrc  /= 1  )   CALL ctl_stop( " Be careful, trends diags never validated " )

      ! ======================================================================
      ! I. Diagnose the purely vertical (K_z) diffusion trend
      ! ======================================================================

      ! ... These terms can be estimated by flux computation at the lower boundary of the ML 
      !     (we compute (-1/h) * K_z * d_z( tracer ))

      IF( ln_trcldf_iso ) THEN
         !
         DO jj = 1,jpj
            DO ji = 1,jpi
               ik = nmld_trc(ji,jj)
               zavt = fsavs(ji,jj,ik)
               DO jn = 1, jptra
                  IF( ln_trdtrc(jn) )    &
                  tmltrd_trc(ji,jj,jpmld_trc_zdf,jn) = - zavt / fse3w(ji,jj,ik) * tmask(ji,jj,ik)  &
                       &                    * ( trn(ji,jj,ik-1,jn) - trn(ji,jj,ik,jn) )            &
                       &                    / MAX( 1., rmld_trc(ji,jj) ) * tmask(ji,jj,1)
               END DO
            END DO
         END DO

         DO jn = 1, jptra
            ! ... Remove this K_z trend from the iso-neutral diffusion term (if any)
            IF( ln_trdtrc(jn) ) &
                 tmltrd_trc(:,:,jpmld_trc_ldf,jn) = tmltrd_trc(:,:,jpmld_trc_ldf,jn) - tmltrd_trc(:,:,jpmld_trc_zdf,jn)
   
         END DO
         !      
      ENDIF

#if ! defined key_gyre
      ! GYRE : for diagnostic fields, are needed if cyclic B.C. are present, but not for purely MPI comm. 
      ! therefore we do not call lbc_lnk in GYRE config. (closed basin, no cyclic B.C.)
      DO jn = 1, jptra
         IF( ln_trdtrc(jn) ) THEN
            DO jl = 1, jpltrd_trc
               CALL lbc_lnk( tmltrd_trc(:,:,jl,jn), 'T', 1. )        ! lateral boundary conditions
            END DO
         ENDIF
      END DO
#endif
      ! ======================================================================
      ! II. Cumulate the trends over the analysis window
      ! ======================================================================

      ztmltrd2(:,:,:,:) = 0.e0   ;   ztmltot2(:,:,:)   = 0.e0     ! <<< reset arrays to zero
      ztmlres2(:,:,:)   = 0.e0   ;   ztmlatf2(:,:,:)   = 0.e0
      ztmlrad2(:,:,:)   = 0.e0

      ! II.1 Set before values of vertically averages passive tracers
      ! -------------------------------------------------------------
      IF( kt > nittrc000 ) THEN
         DO jn = 1, jptra
            IF( ln_trdtrc(jn) ) THEN
               tmlb_trc   (:,:,jn) = tml_trc   (:,:,jn)
               tmlatfn_trc(:,:,jn) = tmltrd_trc(:,:,jpmld_trc_atf,jn)
               tmlradn_trc(:,:,jn) = tmltrd_trc(:,:,jpmld_trc_radb,jn)
            ENDIF
         END DO
      ENDIF

      ! II.2 Vertically averaged passive tracers
      ! ----------------------------------------
      tml_trc(:,:,:) = 0.e0
      DO jk = 1, jpktrd_trc ! - 1 ???
         DO jn = 1, jptra
            IF( ln_trdtrc(jn) ) &
               tml_trc(:,:,jn) = tml_trc(:,:,jn) + wkx_trc(:,:,jk) * trn(:,:,jk,jn)
         END DO
      END DO

      ! II.3 Initialize mixed-layer "before" arrays for the 1rst analysis window    
      ! ------------------------------------------------------------------------
      IF( kt == nittrc000 + nn_dttrc ) THEN  !  i.e. ( .NOT. ln_rstart ).AND.( kt == nit000 + 1)    ???
         !
         DO jn = 1, jptra
            IF( ln_trdtrc(jn) ) THEN
               tmlbb_trc  (:,:,jn) = tmlb_trc   (:,:,jn)   ;   tmlbn_trc  (:,:,jn) = tml_trc    (:,:,jn)
               tmlatfb_trc(:,:,jn) = tmlatfn_trc(:,:,jn)   ;   tmlradb_trc(:,:,jn) = tmlradn_trc(:,:,jn)
               
               tmltrd_csum_ub_trc (:,:,:,jn) = 0.e0   ;   tmltrd_atf_sumb_trc  (:,:,jn) = 0.e0
               tmltrd_rad_sumb_trc  (:,:,jn) = 0.e0
            ENDIF
         END DO
         
         rmldbn_trc(:,:) = rmld_trc(:,:)
         !
      ENDIF

      ! II.4 Cumulated trends over the analysis period
      ! ----------------------------------------------
      !
      !         [  1rst analysis window ] [     2nd analysis window     ]                       
      !
      !     o---[--o-----o-----o-----o--]-[--o-----o-----o-----o-----o--]---o-----o--> time steps
      !                            ntrd                             2*ntrd       etc.
      !     1      2     3     4    =5 e.g.                          =10
      !
      IF( ( kt >= 2 ).OR.( ln_rsttr ) ) THEN                        ! ???
         !
         nmoymltrd = nmoymltrd + 1


         ! ... Cumulate over BOTH physical contributions AND over time steps
         DO jn = 1, jptra
            IF( ln_trdtrc(jn) ) THEN
               DO jl = 1, jpltrd_trc
                  tmltrdm_trc(:,:,jn) = tmltrdm_trc(:,:,jn) + tmltrd_trc(:,:,jl,jn)
               END DO
            ENDIF
         END DO

         DO jn = 1, jptra
            IF( ln_trdtrc(jn) ) THEN
               ! ... Special handling of the Asselin trend 
               tmlatfm_trc(:,:,jn) = tmlatfm_trc(:,:,jn) + tmlatfn_trc(:,:,jn)
               tmlradm_trc(:,:,jn) = tmlradm_trc(:,:,jn) + tmlradn_trc(:,:,jn)

               ! ... Trends associated with the time mean of the ML passive tracers
               tmltrd_sum_trc    (:,:,:,jn) = tmltrd_sum_trc    (:,:,:,jn) + tmltrd_trc    (:,:,:,jn)
               tmltrd_csum_ln_trc(:,:,:,jn) = tmltrd_csum_ln_trc(:,:,:,jn) + tmltrd_sum_trc(:,:,:,jn)
               tml_sum_trc       (:,:,jn)   = tml_sum_trc       (:,:,jn)   + tml_trc       (:,:,jn)
            ENDIF
         ENDDO

         rmld_sum_trc      (:,:)     = rmld_sum_trc      (:,:)     + rmld_trc      (:,:)
         !
      ENDIF

      ! ======================================================================
      ! III. Prepare fields for output (get here ONCE PER ANALYSIS PERIOD)
      ! ======================================================================

      ! Convert to appropriate physical units
      tmltrd_trc(:,:,:,:) = tmltrd_trc(:,:,:,:) * rn_ucf_trc

      itmod = kt - nittrc000 + 1
      it    = kt

      MODULO_NTRD : IF( MOD( itmod, nn_trd_trc ) == 0 ) THEN           ! nitend MUST be multiple of nn_trd_trc
         !
         ztmltot (:,:,:) = 0.e0                                   ! reset arrays to zero
         ztmlres (:,:,:) = 0.e0
         ztmltot2(:,:,:) = 0.e0
         ztmlres2(:,:,:) = 0.e0
      
         zfn  = FLOAT( nmoymltrd )    ;    zfn2 = zfn * zfn
         
         ! III.1 Prepare fields for output ("instantaneous" diagnostics) 
         ! -------------------------------------------------------------

         DO jn = 1, jptra
            IF( ln_trdtrc(jn) ) THEN
               !-- Compute total trends    (use rdttrc instead of rdt ???)
               IF ( ln_trcadv_muscl .OR. ln_trcadv_muscl2 ) THEN  ! EULER-FORWARD schemes
                  ztmltot(:,:,jn) =  ( tml_trc(:,:,jn) - tmlbn_trc(:,:,jn) )/rdt
               ELSE                                                                     ! LEAP-FROG schemes
                  ztmltot(:,:,jn) =  ( tml_trc(:,:,jn) - tmlbn_trc(:,:,jn) + tmlb_trc(:,:,jn) - tmlbb_trc(:,:,jn))/(2.*rdt)
               ENDIF
               
               !-- Compute residuals
               ztmlres(:,:,jn) = ztmltot(:,:,jn) - ( tmltrdm_trc(:,:,jn) - tmlatfn_trc(:,:,jn) + tmlatfb_trc(:,:,jn) &
                  &                                                 - tmlradn_trc(:,:,jn) + tmlradb_trc(:,:,jn)  )
               
               !-- Diagnose Asselin trend over the analysis window 
               ztmlatf(:,:,jn) = tmlatfm_trc(:,:,jn) - tmlatfn_trc(:,:,jn) + tmlatfb_trc(:,:,jn)
               ztmlrad(:,:,jn) = tmlradm_trc(:,:,jn) - tmlradn_trc(:,:,jn) + tmlradb_trc(:,:,jn)
               
         !-- Lateral boundary conditions
#if ! defined key_gyre

               CALL lbc_lnk( ztmltot(:,:,jn) , 'T', 1. )   ;   CALL lbc_lnk( ztmlres(:,:,jn) , 'T', 1. )
               CALL lbc_lnk( ztmlatf(:,:,jn) , 'T', 1. )   ;   CALL lbc_lnk( ztmlrad(:,:,jn) , 'T', 1. )

#endif

#if defined key_diainstant
               STOP 'tmltrd_trc : key_diainstant was never checked within trdmld. Comment this to proceed.'
#endif
            ENDIF
         END DO

         ! III.2 Prepare fields for output ("mean" diagnostics) 
         ! ----------------------------------------------------
         
         !-- Update the ML depth time sum (to build the Leap-Frog time mean)
         rmld_sum_trc(:,:) = rmldbn_trc(:,:) + 2 * ( rmld_sum_trc(:,:) - rmld_trc(:,:) ) + rmld_trc(:,:)

               !-- Compute passive tracer total trends
         DO jn = 1, jptra
            IF( ln_trdtrc(jn) ) THEN
               tml_sum_trc(:,:,jn) = tmlbn_trc(:,:,jn) + 2 * ( tml_sum_trc(:,:,jn) - tml_trc(:,:,jn) ) + tml_trc(:,:,jn)
               ztmltot2   (:,:,jn) = ( tml_sum_trc(:,:,jn) - tml_sumb_trc(:,:,jn) ) /  ( 2.*rdt )    ! now tracer unit is /sec
            ENDIF
         END DO

         !-- Compute passive tracer residuals
         DO jn = 1, jptra
            IF( ln_trdtrc(jn) ) THEN
               !
               DO jl = 1, jpltrd_trc
                  ztmltrd2(:,:,jl,jn) = tmltrd_csum_ub_trc(:,:,jl,jn) + tmltrd_csum_ln_trc(:,:,jl,jn)
               END DO
               
               ztmltrdm2(:,:,jn) = 0.e0
               DO jl = 1, jpltrd_trc
                  ztmltrdm2(:,:,jn) = ztmltrdm2(:,:,jn) + ztmltrd2(:,:,jl,jn)
               END DO
               
               ztmlres2(:,:,jn) =  ztmltot2(:,:,jn)  -       &
                  & ( ztmltrdm2(:,:,jn) - tmltrd_sum_trc(:,:,jpmld_trc_atf ,jn) + tmltrd_atf_sumb_trc(:,:,jn) &
                  &                     - tmltrd_sum_trc(:,:,jpmld_trc_radb,jn) + tmltrd_rad_sumb_trc(:,:,jn) )
               !

               !-- Diagnose Asselin trend over the analysis window
               ztmlatf2(:,:,jn) = ztmltrd2(:,:,jpmld_trc_atf ,jn) - tmltrd_sum_trc(:,:,jpmld_trc_atf ,jn) &
                  &                                               + tmltrd_atf_sumb_trc(:,:,jn)
               ztmlrad2(:,:,jn) = ztmltrd2(:,:,jpmld_trc_radb,jn) - tmltrd_sum_trc(:,:,jpmld_trc_radb,jn) &
                  &                                               + tmltrd_rad_sumb_trc(:,:,jn)

         !-- Lateral boundary conditions
#if ! defined key_gyre         
               CALL lbc_lnk( ztmltot2(:,:,jn), 'T', 1. )
               CALL lbc_lnk( ztmlres2(:,:,jn), 'T', 1. )
               DO jl = 1, jpltrd_trc
                  CALL lbc_lnk( ztmltrd2(:,:,jl,jn), 'T', 1. )       ! will be output in the NetCDF trends file
               END DO
#endif
            ENDIF
         END DO

         ! * Debugging information *
         IF( lldebug ) THEN
            !
            WRITE(numout,*) 'trd_mld_trc : write trends in the Mixed Layer for debugging process:'
            WRITE(numout,*) '~~~~~~~~~~~  '
            WRITE(numout,*)
            WRITE(numout,*) 'TRC kt = ', kt, '    nmoymltrd = ', nmoymltrd

            DO jn = 1, jptra

               IF( ln_trdtrc(jn) ) THEN
                  WRITE(numout, *)
                  WRITE(numout, *) '>>>>>>>>>>>>>>>>>>  TRC TRACER jn =', jn, ' <<<<<<<<<<<<<<<<<<'
                  
                  WRITE(numout, *)
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM ztmlres     : ', SUM2D(ztmlres(:,:,jn))
                  !CD??? PREVOIR: z2d = ztmlres(:,:,jn)   ;   CALL prt_ctl(tab2d_1=z2d, clinfo1=' ztmlres   -   : ')
                  
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM ABS(ztmlres): ', SUM2D(ABS(ztmlres(:,:,jn)))
                  WRITE(numout, '(3x,a)') ' -->>>------------------- ztmlres is computed from ------------- '
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM +ztmltot    : ', SUM2D(+ztmltot    (:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM +tmltrdm_trc: ', SUM2D(+tmltrdm_trc(:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM -tmlatfn_trc: ', SUM2D(-tmlatfn_trc(:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM +tmlatfb_trc: ', SUM2D(+tmlatfb_trc(:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM -tmlradn_trc: ', SUM2D(-tmlradn_trc(:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM +tmlradb_trc: ', SUM2D(+tmlradb_trc(:,:,jn))
                  WRITE(numout, '(3x,a)') ' --<<<----------------------------------------------------------- '
                  
                  WRITE(numout, *)
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM ztmlres2    : ', SUM2D(ztmlres2(:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM ABS(ztmlres2):', SUM2D(ABS(ztmlres2(:,:,jn)))
                  WRITE(numout, '(3x,a)') ' -->>>------------------- ztmlres2 is computed from ------------ '
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM +ztmltot2            : ', SUM2D(+ztmltot2(:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM +ztmltrdm2           : ', SUM2D(+ztmltrdm2(:,:,jn)) 
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM -tmltrd_sum_trc      : ', SUM2D(-tmltrd_sum_trc(:,:,jpmld_trc_atf,jn)) 
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM +tmltrd_atf_sumb_trc : ', SUM2D(+tmltrd_atf_sumb_trc(:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM -tmltrd_sum_trc      : ', SUM2D(-tmltrd_sum_trc(:,:,jpmld_trc_radb,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM +tmltrd_rad_sumb_trc : ', SUM2D(+tmltrd_rad_sumb_trc(:,:,jn) )
                  WRITE(numout, '(3x,a)') ' --<<<----------------------------------------------------------- '
                  
                  WRITE(numout, *)
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM ztmltot     : ', SUM2D(ztmltot    (:,:,jn))
                  WRITE(numout, '(3x,a)') ' -->>>------------------- ztmltot is computed from ------------- '
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM +tml_trc    : ', SUM2D(tml_trc    (:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM -tmlbn_trc  : ', SUM2D(tmlbn_trc  (:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM +tmlb_trc   : ', SUM2D(tmlb_trc   (:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM -tmlbb_trc  : ', SUM2D(tmlbb_trc  (:,:,jn))
                  WRITE(numout, '(3x,a)') ' --<<<----------------------------------------------------------- '
                  
                  WRITE(numout, *)
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM tmltrdm_trc : ', SUM2D(tmltrdm_trc(:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM tmlatfb_trc : ', SUM2D(tmlatfb_trc(:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM tmlatfn_trc : ', SUM2D(tmlatfn_trc(:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM tmlradb_trc : ', SUM2D(tmlradb_trc(:,:,jn))
                  WRITE(numout,98) 'TRC jn =', jn, ' SUM tmlradn_trc : ', SUM2D(tmlradn_trc(:,:,jn))
                  
                  WRITE(numout, *)
                  DO jl = 1, jpltrd_trc
                     WRITE(numout,97) 'TRC jn =', jn, ' TREND INDEX jpmld_trc_xxx = ', jl, &
                        & ' SUM tmltrd_trc : ', SUM2D(tmltrd_trc(:,:,jl,jn))
                  END DO
               
                  WRITE(numout,*) 
                  WRITE(numout,*) ' *********************** ZTMLRES, ZTMLRES2 *********************** '
                  WRITE(numout,*)
                  WRITE(numout,*) 'TRC ztmlres (jpi/2,jpj/2,:) : ', ztmlres (jpi/2,jpj/2,jn)
                  WRITE(numout,*)
                  WRITE(numout,*) 'TRC ztmlres2(jpi/2,jpj/2,:) : ', ztmlres2(jpi/2,jpj/2,jn)
                  
                  WRITE(numout,*) 
                  WRITE(numout,*) ' *********************** ZTMLRES *********************** '
                  WRITE(numout,*)
                  
                  WRITE(numout,*) '...................................................'
                  WRITE(numout,*) 'TRC jn =', jn, ' ztmlres (1:10,1:5,jn) : '
                  DO jj = 5, 1, -1
                     WRITE(numout,99) jj, ( ztmlres (ji,jj,jn), ji=1,10 )
                  END DO
                  
                  WRITE(numout,*) 
                  WRITE(numout,*) ' *********************** ZTMLRES2 *********************** '
                  WRITE(numout,*)
                  
                  WRITE(numout,*) '...................................................'
                  WRITE(numout,*) 'TRC jn =', jn, ' ztmlres2 (1:10,1:5,jn) : '
                  DO jj = 5, 1, -1
                     WRITE(numout,99) jj, ( ztmlres2 (ji,jj,jn), ji=1,10 )
                  END DO
                  !
               ENDIF
               !
            END DO


97            FORMAT(a10, i3, 2x, a30, i3, a20, 2x, g20.10)
98            FORMAT(a10, i3, 2x, a30, 2x, g20.10)
99            FORMAT('TRC jj =', i3,' : ', 10(g10.3,2x))
              WRITE(numout,*)
            !
         ENDIF

         ! III.3 Time evolution array swap
         ! -------------------------------
         ! ML depth
         rmldbn_trc(:,:)   = rmld_trc(:,:)
         rmld_sum_trc(:,:)     = rmld_sum_trc(:,:)     /      (2*zfn)  ! similar to tml_sum and sml_sum
         DO jn = 1, jptra
            IF( ln_trdtrc(jn) ) THEN        
               ! For passive tracer instantaneous diagnostics
               tmlbb_trc  (:,:,jn) = tmlb_trc   (:,:,jn)   ;   tmlbn_trc  (:,:,jn) = tml_trc    (:,:,jn)
               tmlatfb_trc(:,:,jn) = tmlatfn_trc(:,:,jn)   ;   tmlradb_trc(:,:,jn) = tmlradn_trc(:,:,jn)
               
               ! For passive tracer mean diagnostics
               tmltrd_csum_ub_trc (:,:,:,jn) = zfn * tmltrd_sum_trc(:,:,:,jn) - tmltrd_csum_ln_trc(:,:,:,jn)
               tml_sumb_trc       (:,:,jn)   = tml_sum_trc(:,:,jn)
               tmltrd_atf_sumb_trc(:,:,jn)   = tmltrd_sum_trc(:,:,jpmld_trc_atf ,jn)
               tmltrd_rad_sumb_trc(:,:,jn)   = tmltrd_sum_trc(:,:,jpmld_trc_radb,jn)
               
               
               ! III.4 Convert to appropriate physical units
               ! -------------------------------------------
               ztmltot     (:,:,jn)   = ztmltot     (:,:,jn)   * rn_ucf_trc/zfn   ! instant diags
               ztmlres     (:,:,jn)   = ztmlres     (:,:,jn)   * rn_ucf_trc/zfn
               ztmlatf     (:,:,jn)   = ztmlatf     (:,:,jn)   * rn_ucf_trc/zfn
               ztmlrad     (:,:,jn)   = ztmlrad     (:,:,jn)   * rn_ucf_trc/zfn
               tml_sum_trc (:,:,jn)   = tml_sum_trc (:,:,jn)   /      (2*zfn)  ! mean diags
               ztmltot2    (:,:,jn)   = ztmltot2    (:,:,jn)   * rn_ucf_trc/zfn2
               ztmltrd2    (:,:,:,jn) = ztmltrd2    (:,:,:,jn) * rn_ucf_trc/zfn2
               ztmlatf2    (:,:,jn)   = ztmlatf2    (:,:,jn)   * rn_ucf_trc/zfn2
               ztmlrad2    (:,:,jn)   = ztmlrad2    (:,:,jn)   * rn_ucf_trc/zfn2
               ztmlres2    (:,:,jn)   = ztmlres2    (:,:,jn)   * rn_ucf_trc/zfn2
            ENDIF
         END DO
         !
      ENDIF MODULO_NTRD

      ! ======================================================================
      ! IV. Write trends in the NetCDF file
      ! ======================================================================

      ! IV.1 Code for dimg mpp output
      ! -----------------------------

# if defined key_dimgout
      STOP 'Not implemented'
# else
      
      ! IV.2 Code for IOIPSL/NetCDF output
      ! ----------------------------------

      IF( lwp .AND. MOD( itmod , nn_trd_trc ) == 0 ) THEN
         WRITE(numout,*) ' '
         WRITE(numout,*) 'trd_mld_trc : write passive tracer trends in the NetCDF file :'
         WRITE(numout,*) '~~~~~~~~~~~ '
         WRITE(numout,*) '          ', trim(clhstnam), ' at kt = ', kt
         WRITE(numout,*) '          N.B. nmoymltrd = ', nmoymltrd
         WRITE(numout,*) ' '
      ENDIF
         
      NETCDF_OUTPUT : IF( ln_trdmld_trc_instant ) THEN            ! <<< write the trends for passive tracer instant. diags
         !

         DO jn = 1, jptra
            !
            IF( ln_trdtrc(jn) ) THEN
               CALL histwrite( nidtrd(jn), "mxl_depth", it, rmld_trc(:,:), ndimtrd1, ndextrd1 )
               !-- Output the fields
               clvar = trim(ctrcnm(jn))//"ml"                        ! e.g. detml, zooml, nh4ml, etc.
               CALL histwrite( nidtrd(jn), trim(clvar)         , it, tml_trc(:,:,jn), ndimtrd1, ndextrd1 ) 
               CALL histwrite( nidtrd(jn), trim(clvar)//"_tot" , it, ztmltot(:,:,jn), ndimtrd1, ndextrd1 ) 
               CALL histwrite( nidtrd(jn), trim(clvar)//"_res" , it, ztmlres(:,:,jn), ndimtrd1, ndextrd1 ) 
           
               DO jl = 1, jpltrd_trc - 2
                  CALL histwrite( nidtrd(jn), trim(clvar)//trim(ctrd_trc(jl,2)),             &
                    &          it, tmltrd_trc(:,:,jl,jn), ndimtrd1, ndextrd1 )
               END DO

               CALL histwrite( nidtrd(jn), trim(clvar)//trim(ctrd_trc(jpmld_trc_radb,2)),    &  ! now trcrad    : jpltrd_trc - 1
                    &          it, ztmlrad(:,:,jn), ndimtrd1, ndextrd1 )

               CALL histwrite( nidtrd(jn), trim(clvar)//trim(ctrd_trc(jpmld_trc_atf,2)),     &  ! now Asselin   : jpltrd_trc
                    &          it, ztmlatf(:,:,jn), ndimtrd1, ndextrd1 )
                     
            ENDIF
         END DO

         IF( kt == nitend ) THEN 
            DO jn = 1, jptra
               IF( ln_trdtrc(jn) )  CALL histclo( nidtrd(jn) )
            END DO
         ENDIF

      ELSE                                                        ! <<< write the trends for passive tracer mean diagnostics
         
         DO jn = 1, jptra
            !
            IF( ln_trdtrc(jn) ) THEN
               CALL histwrite( nidtrd(jn), "mxl_depth", it, rmld_sum_trc(:,:), ndimtrd1, ndextrd1 ) 
               !-- Output the fields
               clvar = trim(ctrcnm(jn))//"ml"                        ! e.g. detml, zooml, nh4ml, etc.

               CALL histwrite( nidtrd(jn), trim(clvar)         , it, tml_sum_trc(:,:,jn), ndimtrd1, ndextrd1 )
               CALL histwrite( nidtrd(jn), trim(clvar)//"_tot" , it,    ztmltot2(:,:,jn), ndimtrd1, ndextrd1 ) 
               CALL histwrite( nidtrd(jn), trim(clvar)//"_res" , it,    ztmlres2(:,:,jn), ndimtrd1, ndextrd1 ) 

               DO jl = 1, jpltrd_trc - 2
                  CALL histwrite( nidtrd(jn), trim(clvar)//trim(ctrd_trc(jl,2)),           &
                    &          it, ztmltrd2(:,:,jl,jn), ndimtrd1, ndextrd1 )
               END DO
            
               CALL histwrite( nidtrd(jn), trim(clvar)//trim(ctrd_trc(jpmld_trc_radb,2)),   &  ! now trcrad    : jpltrd_trc - 1
                 &          it, ztmlrad2(:,:,jn), ndimtrd1, ndextrd1 )

               CALL histwrite( nidtrd(jn), trim(clvar)//trim(ctrd_trc(jpmld_trc_atf,2)),    &  ! now Asselin   : jpltrd_trc
                 &          it, ztmlatf2(:,:,jn), ndimtrd1, ndextrd1 )

            ENDIF 
            !
         END DO
         IF( kt == nitend ) THEN 
            DO jn = 1, jptra
               IF( ln_trdtrc(jn) )  CALL histclo( nidtrd(jn) )
            END DO
         ENDIF

         !
      ENDIF NETCDF_OUTPUT
         
      ! Compute the control surface (for next time step) : flag = on
      icount = 1

# endif /* key_dimgout */

      IF( MOD( itmod, nn_trd_trc ) == 0 ) THEN
         !
         ! Reset cumulative arrays to zero
         ! -------------------------------         
         nmoymltrd = 0
         tmltrdm_trc        (:,:,:)   = 0.e0   ;   tmlatfm_trc        (:,:,:)   = 0.e0
         tmlradm_trc        (:,:,:)   = 0.e0   ;   tml_sum_trc        (:,:,:)   = 0.e0
         tmltrd_csum_ln_trc (:,:,:,:) = 0.e0   ;   tmltrd_sum_trc     (:,:,:,:) = 0.e0
         rmld_sum_trc       (:,:)     = 0.e0
         !
      ENDIF

      ! ======================================================================
      ! V. Write restart file
      ! ======================================================================

      IF( lrst_trc )   CALL trd_mld_trc_rst_write( kt )  ! this must be after the array swap above (III.3)

      CALL wrk_dealloc( jpi, jpj, jptra, ztmltot , ztmlres , ztmlatf , ztmlrad             )
      CALL wrk_dealloc( jpi, jpj, jptra, ztmltot2, ztmlres2, ztmlatf2, ztmlrad2, ztmltrdm2 )
      !
   END SUBROUTINE trd_mld_trc


   SUBROUTINE trd_mld_bio( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_mld  ***
      !!
      !! ** Purpose :  Compute and cumulate the mixed layer biological trends over an analysis
      !!               period, and write NetCDF (or dimg) outputs.
      !!
      !! ** Method/usage :
      !!          The stored trends can be chosen twofold (according to the ln_trdmld_trc_instant
      !!          logical namelist variable) :
      !!          1) to explain the difference between initial and final
      !!             mixed-layer T & S (where initial and final relate to the
      !!             current analysis window, defined by ntrd in the namelist)
      !!          2) to explain the difference between the current and previous
      !!             TIME-AVERAGED mixed-layer T & S (where time-averaging is
      !!             performed over each analysis window).
      !!
      !! ** Consistency check :
      !!        If the control surface is fixed ( nctls > 1 ), the residual term (dh/dt
      !!        entrainment) should be zero, at machine accuracy. Note that in the case
      !!        of time-averaged mixed-layer fields, this residual WILL NOT BE ZERO
      !!        over the first two analysis windows (except if restart).
      !!        N.B. For ORCA2_LIM, use e.g. ntrd=5, ucf=1., nctls=8
      !!             for checking residuals.
      !!             On a NEC-SX5 computer, this typically leads to:
      !!                   O(1.e-20) temp. residuals (tml_res) when ln_trdmld_trc_instant=.false.
      !!                   O(1.e-21) temp. residuals (tml_res) when ln_trdmld_trc_instant=.true.
      !!
      !! ** Action :
      !!       At each time step, mixed-layer averaged trends are stored in the
      !!       tmltrd(:,:,jpmld_xxx) array (see trdmld_oce.F90 for definitions of jpmld_xxx).
      !!       This array is known when trd_mld is called, at the end of the stp subroutine,
      !!       except for the purely vertical K_z diffusion term, which is embedded in the
      !!       lateral diffusion trend.
      !!
      !!       In I), this K_z term is diagnosed and stored, thus its contribution is removed
      !!       from the lateral diffusion trend.
      !!       In II), the instantaneous mixed-layer T & S are computed, and misc. cumulative
      !!       arrays are updated.
      !!       In III), called only once per analysis window, we compute the total trends,
      !!       along with the residuals and the Asselin correction terms.
      !!       In IV), the appropriate trends are written in the trends NetCDF file.
      !!
      !! References :
      !!       - Vialard & al.
      !!       - See NEMO documentation (in preparation)
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt                       ! ocean time-step index
#if defined key_lobster
      INTEGER  ::  jl, it, itmod
      LOGICAL  :: llwarn  = .TRUE., lldebug = .TRUE.
      REAL(wp) :: zfn, zfn2
#if defined key_dimgout
      INTEGER ::  iyear,imon,iday
      CHARACTER(LEN=80) :: cltext, clmode
#endif
      !!----------------------------------------------------------------------
      ! ... Warnings
      IF( nn_dttrc  /= 1  ) CALL ctl_stop( " Be careful, trends diags never validated " )

      ! ======================================================================
      ! II. Cumulate the trends over the analysis window
      ! ======================================================================

      ztmltrdbio2(:,:,:) = 0.e0  ! <<< reset arrays to zero

      ! II.3 Initialize mixed-layer "before" arrays for the 1rst analysis window
      ! ------------------------------------------------------------------------
      IF( kt == nittrc000 + nn_dttrc ) THEN  !  i.e. ( .NOT. ln_rstart ).AND.( kt == nit000 + 1)
         !
         tmltrd_csum_ub_bio (:,:,:) = 0.e0
         !
      END IF

      ! II.4 Cumulated trends over the analysis period
      ! ----------------------------------------------
      !
      !         [  1rst analysis window ] [     2nd analysis window     ]
      !
      !
      !     o---[--o-----o-----o-----o--]-[--o-----o-----o-----o-----o--]---o-----o--> time steps
      !                            ntrd                             2*ntrd       etc.
      !     1      2     3     4    =5 e.g.                          =10
      !
      IF( ( kt >= 2 ).OR.( ln_rsttr ) ) THEN
         !
         nmoymltrdbio = nmoymltrdbio + 1

         ! ... Trends associated with the time mean of the ML passive tracers
         tmltrd_sum_bio    (:,:,:) = tmltrd_sum_bio    (:,:,:) + tmltrd_bio    (:,:,:)
         tmltrd_csum_ln_bio(:,:,:) = tmltrd_csum_ln_bio(:,:,:) + tmltrd_sum_bio(:,:,:)
         !
      END IF

      ! ======================================================================
      ! III. Prepare fields for output (get here ONCE PER ANALYSIS PERIOD)
      ! ======================================================================

      ! Convert to appropriate physical units
      tmltrd_bio(:,:,:) = tmltrd_bio(:,:,:) * rn_ucf_trc

      MODULO_NTRD : IF( MOD( kt, nn_trd_trc ) == 0 ) THEN      ! nitend MUST be multiple of ntrd
         !
         zfn  = float(nmoymltrdbio)    ;    zfn2 = zfn * zfn

         ! III.1 Prepare fields for output ("instantaneous" diagnostics)
         ! -------------------------------------------------------------

#if defined key_diainstant
         STOP 'tmltrd_bio : key_diainstant was never checked within trdmld. Comment this to proceed.'
#endif
         ! III.2 Prepare fields for output ("mean" diagnostics)
         ! ----------------------------------------------------

         ztmltrdbio2(:,:,:) = tmltrd_csum_ub_bio(:,:,:) + tmltrd_csum_ln_bio(:,:,:)

         !-- Lateral boundary conditions
#if ! defined key_gyre
         ! ES_B27_CD_WARN : lbc inutile GYRE, cf. + haut
         DO jn = 1, jpdiabio
           CALL lbc_lnk( ztmltrdbio2(:,:,jn), 'T', 1. )
         ENDDO
#endif
         IF( lldebug ) THEN
            !
            WRITE(numout,*) 'trd_mld_bio : write trends in the Mixed Layer for debugging process:'
            WRITE(numout,*) '~~~~~~~~~~~  '
            WRITE(numout,*) 'TRC kt = ', kt, 'nmoymltrdbio = ', nmoymltrdbio
            WRITE(numout,*)

            DO jl = 1, jpdiabio
              IF( ln_trdmld_trc_instant ) THEN
                  WRITE(numout,97) 'TRC jl =', jl, ' bio TREND INDEX  = ', jl, &
                     & ' SUM tmltrd_bio : ', SUM2D(tmltrd_bio(:,:,jl))
              ELSE
                  WRITE(numout,97) 'TRC jl =', jl, ' bio TREND INDEX  = ', jl, &
                     & ' SUM ztmltrdbio2 : ', SUM2D(ztmltrdbio2(:,:,jl))
              endif
            END DO

97          FORMAT(a10, i3, 2x, a30, i3, a20, 2x, g20.10)
98          FORMAT(a10, i3, 2x, a30, 2x, g20.10)
99          FORMAT('TRC jj =', i3,' : ', 10(g10.3,2x))
            WRITE(numout,*)
            !
         ENDIF

         ! III.3 Time evolution array swap
         ! -------------------------------

         ! For passive tracer mean diagnostics
         tmltrd_csum_ub_bio (:,:,:) = zfn * tmltrd_sum_bio(:,:,:) - tmltrd_csum_ln_bio(:,:,:)

         ! III.4 Convert to appropriate physical units
         ! -------------------------------------------
         ztmltrdbio2    (:,:,:) = ztmltrdbio2    (:,:,:) * rn_ucf_trc/zfn2

      END IF MODULO_NTRD

      ! ======================================================================
      ! IV. Write trends in the NetCDF file
      ! ======================================================================

      ! IV.1 Code for dimg mpp output
      ! -----------------------------

# if defined key_dimgout
      STOP 'Not implemented'
# else

      ! IV.2 Code for IOIPSL/NetCDF output
      ! ----------------------------------

      ! define time axis
      itmod = kt - nittrc000 + 1
      it    = kt

      IF( lwp .AND. MOD( itmod , nn_trd_trc ) == 0 ) THEN
         WRITE(numout,*) ' '
         WRITE(numout,*) 'trd_mld_bio : write ML bio trends in the NetCDF file :'
         WRITE(numout,*) '~~~~~~~~~~~ '
         WRITE(numout,*) '          ', TRIM(clhstnam), ' at kt = ', kt
         WRITE(numout,*) '          N.B. nmoymltrdbio = ', nmoymltrdbio
         WRITE(numout,*) ' '
      END IF


      ! 2. Start writing data
      ! ---------------------

      NETCDF_OUTPUT : IF( ln_trdmld_trc_instant ) THEN    ! <<< write the trends for passive tracer instant. diags
         !
            DO jl = 1, jpdiabio
               CALL histwrite( nidtrdbio,TRIM("ML_"//ctrd_bio(jl,2)) ,            &
                    &          it, tmltrd_bio(:,:,jl), ndimtrd1, ndextrd1 )
            END DO


         IF( kt == nitend )   CALL histclo( nidtrdbio )

      ELSE    ! <<< write the trends for passive tracer mean diagnostics

            DO jl = 1, jpdiabio
               CALL histwrite( nidtrdbio, TRIM("ML_"//ctrd_bio(jl,2)) ,            &
                    &          it, ztmltrdbio2(:,:,jl), ndimtrd1, ndextrd1 )
            END DO

            IF( kt == nitend )   CALL histclo( nidtrdbio )
            !
      END IF NETCDF_OUTPUT

      ! Compute the control surface (for next time step) : flag = on
      icountbio = 1


# endif /* key_dimgout */

      IF( MOD( itmod, nn_trd_trc ) == 0 ) THEN
         !
         ! III.5 Reset cumulative arrays to zero
         ! -------------------------------------
         nmoymltrdbio = 0
         tmltrd_csum_ln_bio (:,:,:) = 0.e0
         tmltrd_sum_bio     (:,:,:) = 0.e0
      END IF

      ! ======================================================================
      ! Write restart file
      ! ======================================================================

! restart write is done in trd_mld_trc_write which is called by trd_mld_bio (Marina)
!
#endif
   END SUBROUTINE trd_mld_bio


   REAL FUNCTION sum2d( ztab )
      !!----------------------------------------------------------------------
      !! CD ??? prevoir d'utiliser plutot prtctl
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT( in ) ::  ztab      
      !!----------------------------------------------------------------------
      sum2d = SUM( ztab(2:jpi-1,2:jpj-1) )
   END FUNCTION sum2d


   SUBROUTINE trd_mld_trc_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_mld_init  ***
      !! 
      !! ** Purpose :   computation of vertically integrated T and S budgets
      !!      from ocean surface down to control surface (NetCDF output)
      !!
      !! ** Method/usage :
      !!
      !!----------------------------------------------------------------------
      INTEGER :: inum   ! logical unit
      INTEGER :: ilseq, jl, jn, iiter
      REAL(wp) ::   zjulian, zsto, zout
      CHARACTER (LEN=40) ::   clop
      CHARACTER (LEN=15) ::   csuff
      CHARACTER (LEN=12) ::   clmxl
      CHARACTER (LEN=16) ::   cltrcu
      CHARACTER (LEN=10) ::   clvar

      !!----------------------------------------------------------------------

      ! ======================================================================
      ! I. initialization
      ! ======================================================================

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) ' trd_mld_trc_init : Mixed-layer trends for passive tracers                '
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~'
         WRITE(numout,*)
      ENDIF

      
      ! I.1 Check consistency of user defined preferences
      ! -------------------------------------------------

      IF( ( lk_trdmld_trc ) .AND. ( MOD( nitend-nit000+1, nn_trd_trc ) /= 0 ) ) THEN
         WRITE(numout,cform_err)
         WRITE(numout,*) '                Your nitend parameter, nitend = ', nitend
         WRITE(numout,*) '                is no multiple of the trends diagnostics frequency        '
         WRITE(numout,*) '                          you defined, nn_trd_trc   = ', nn_trd_trc
         WRITE(numout,*) '                This will not allow you to restart from this simulation.  '
         WRITE(numout,*) '                You should reconsider this choice.                        ' 
         WRITE(numout,*) 
         WRITE(numout,*) '                N.B. the nitend parameter is also constrained to be a     '
         WRITE(numout,*) '                multiple of the sea-ice frequency parameter (typically 5) '
         nstop = nstop + 1
      ENDIF

      ! * Debugging information *
      IF( lldebug ) THEN
         WRITE(numout,*) '               ln_trcadv_muscl = '      , ln_trcadv_muscl
         WRITE(numout,*) '               ln_trdmld_trc_instant = ', ln_trdmld_trc_instant
      ENDIF

      IF( ( ln_trcadv_muscl .OR. ln_trcadv_muscl2 ) .AND. .NOT. ln_trdmld_trc_instant ) THEN
         WRITE(numout,cform_err)
         WRITE(numout,*) '                Currently, you can NOT use simultaneously tracer MUSCL    '
         WRITE(numout,*) '                advection and window averaged diagnostics of ML trends.   '
         WRITE(numout,*) '                WHY? Everything in trdmld_trc is coded for leap-frog, and '
         WRITE(numout,*) '                MUSCL scheme is Euler forward for passive tracers (note   '
         WRITE(numout,*) '                that MUSCL is leap-frog for active tracers T/S).          '
         WRITE(numout,*) '                In particuliar, entrainment trend would be FALSE. However '
         WRITE(numout,*) '                this residual is correct for instantaneous ML diagnostics.'
         WRITE(numout,*) 
         nstop = nstop + 1
      ENDIF

      ! I.2 Initialize arrays to zero or read a restart file
      ! ----------------------------------------------------
      nmoymltrd   = 0

      rmld_trc           (:,:)     = 0.e0   ;   tml_trc            (:,:,:)   = 0.e0       ! inst.
      tmltrdm_trc        (:,:,:)   = 0.e0   ;   tmlatfm_trc        (:,:,:)   = 0.e0
      tmlradm_trc        (:,:,:)   = 0.e0

      tml_sum_trc        (:,:,:)   = 0.e0   ;   tmltrd_sum_trc     (:,:,:,:) = 0.e0       ! mean
      tmltrd_csum_ln_trc (:,:,:,:) = 0.e0   ;   rmld_sum_trc       (:,:)     = 0.e0

#if defined key_lobster
      nmoymltrdbio   = 0
      tmltrd_sum_bio     (:,:,:) = 0.e0     ;   tmltrd_csum_ln_bio (:,:,:) = 0.e0
      DO jl = 1, jp_lobster_trd
          ctrd_bio(jl,1) = ctrbil(jl)   ! long name
          ctrd_bio(jl,2) = ctrbio(jl)   ! short name
       ENDDO
#endif

      IF( ln_rsttr .AND. ln_trdmld_trc_restart ) THEN
         CALL trd_mld_trc_rst_read
      ELSE
         tmlb_trc           (:,:,:)   = 0.e0   ;   tmlbb_trc          (:,:,:)   = 0.e0     ! inst.
         tmlbn_trc          (:,:,:)   = 0.e0

         tml_sumb_trc       (:,:,:)   = 0.e0   ;   tmltrd_csum_ub_trc (:,:,:,:) = 0.e0     ! mean
         tmltrd_atf_sumb_trc(:,:,:)   = 0.e0   ;   tmltrd_rad_sumb_trc(:,:,:)   = 0.e0 
#if defined key_lobster
         tmltrd_csum_ub_bio (:,:,:) = 0.e0
#endif

       ENDIF

      icount = 1   ;   ionce  = 1  ! open specifier   

#if defined key_lobster
      icountbio = 1   ;   ioncebio  = 1  ! open specifier
#endif

      ! I.3 Read control surface from file ctlsurf_idx
      ! ----------------------------------------------
      IF( nn_ctls_trc == 1 ) THEN
         CALL ctl_opn( inum, 'ctlsurf_idx', 'OLD', 'UNFORMATTED', 'SEQUENTIAL', -1, numout, lwp )
         READ ( inum ) nbol_trc
         CLOSE( inum )
      ENDIF

      ! ======================================================================
      ! II. netCDF output initialization
      ! ======================================================================

#if defined key_dimgout 
      ???
#else
      ! clmxl = legend root for netCDF output
      IF( nn_ctls_trc == 0 ) THEN                                   ! control surface = mixed-layer with density criterion
         clmxl = 'Mixed Layer '
      ELSE IF( nn_ctls_trc == 1 ) THEN                              ! control surface = read index from file 
         clmxl = '      Bowl '
      ELSE IF( nn_ctls_trc >= 2 ) THEN                              ! control surface = model level
         WRITE(clmxl,'(A10,I2,1X)') 'Levels 1 -', nn_ctls_trc
      ENDIF

      ! II.1 Define frequency of output and means
      ! -----------------------------------------
      IF( ln_mskland )   THEN   ;   clop = "only(x)"   ! put 1.e+20 on land (very expensive!!)
      ELSE                      ;   clop = "x"         ! no use of the mask value (require less cpu time)
      ENDIF
#  if defined key_diainstant
      IF( .NOT. ln_trdmld_trc_instant ) THEN
         STOP 'trd_mld_trc : this was never checked. Comment this line to proceed...'
      ENDIF
      zsto = nn_trd_trc * rdt
      clop = "inst("//TRIM(clop)//")"
#  else
      IF( ln_trdmld_trc_instant ) THEN
         zsto = rdt                                               ! inst. diags : we use IOIPSL time averaging
      ELSE
         zsto = nn_trd_trc * rdt                                    ! mean  diags : we DO NOT use any IOIPSL time averaging
      ENDIF
      clop = "ave("//TRIM(clop)//")"
#  endif
      zout = nn_trd_trc * rdt
      iiter = ( nittrc000 - 1 ) / nn_dttrc

      IF(lwp) WRITE (numout,*) '                netCDF initialization'

      ! II.2 Compute julian date from starting date of the run
      ! ------------------------------------------------------
      CALL ymds2ju( nyear, nmonth, nday, rdt, zjulian )
      zjulian = zjulian - adatrj   !   set calendar origin to the beginning of the experiment
      IF(lwp) WRITE(numout,*)' '  
      IF(lwp) WRITE(numout,*)' Date 0 used :', nittrc000               &
           &   ,' YEAR ', nyear, ' MONTH ', nmonth,' DAY ', nday       &
           &   ,'Julian day : ', zjulian

      ! II.3 Define the T grid trend file (nidtrd)
      ! ------------------------------------------

      !-- Define long and short names for the NetCDF output variables
      !       ==> choose them according to trdmld_trc_oce.F90 <==

      ctrd_trc(jpmld_trc_xad    ,1) = " Zonal advection"                 ;   ctrd_trc(jpmld_trc_xad    ,2) = "_xad"
      ctrd_trc(jpmld_trc_yad    ,1) = " Meridional advection"            ;   ctrd_trc(jpmld_trc_yad    ,2) = "_yad"
      ctrd_trc(jpmld_trc_zad    ,1) = " Vertical advection"              ;   ctrd_trc(jpmld_trc_zad    ,2) = "_zad"
      ctrd_trc(jpmld_trc_ldf    ,1) = " Lateral diffusion"               ;   ctrd_trc(jpmld_trc_ldf    ,2) = "_ldf"
      ctrd_trc(jpmld_trc_zdf    ,1) = " Vertical diff. (Kz)"             ;   ctrd_trc(jpmld_trc_zdf    ,2) = "_zdf"
      ctrd_trc(jpmld_trc_bbl    ,1) = " Adv/diff. Bottom boundary layer" ;   ctrd_trc(jpmld_trc_bbl    ,2) = "_bbl"
      ctrd_trc(jpmld_trc_dmp    ,1) = " Tracer damping"                  ;   ctrd_trc(jpmld_trc_dmp    ,2) = "_dmp"
      ctrd_trc(jpmld_trc_sbc    ,1) = " Surface boundary cond."          ;   ctrd_trc(jpmld_trc_sbc    ,2) = "_sbc"
      ctrd_trc(jpmld_trc_sms,    1) = " Sources minus sinks"             ;   ctrd_trc(jpmld_trc_sms    ,2) = "_sms"
      ctrd_trc(jpmld_trc_radb   ,1) = " Correct negative concentrations" ;   ctrd_trc(jpmld_trc_radb   ,2) = "_radb"
      ctrd_trc(jpmld_trc_radn   ,1) = " Correct negative concentrations" ;   ctrd_trc(jpmld_trc_radn   ,2) = "_radn"
      ctrd_trc(jpmld_trc_atf    ,1) = " Asselin time filter"             ;   ctrd_trc(jpmld_trc_atf    ,2) = "_atf"

      DO jn = 1, jptra      
      !-- Create a NetCDF file and enter the define mode 
         IF( ln_trdtrc(jn) ) THEN
            csuff="ML_"//ctrcnm(jn)
            CALL dia_nam( clhstnam, nn_trd_trc, csuff )
            CALL histbeg( clhstnam, jpi, glamt, jpj, gphit,                                            &
               &        1, jpi, 1, jpj, iiter, zjulian, rdt, nh_t(jn), nidtrd(jn), domain_id=nidom, snc4chunks=snc4set )
      
            !-- Define the ML depth variable
            CALL histdef(nidtrd(jn), "mxl_depth", clmxl//" Mixed Layer Depth", "m",                        &
               &        jpi, jpj, nh_t(jn), 1  , 1, 1  , -99 , 32, clop, zsto, zout )

         ENDIF
      END DO

#if defined key_lobster
          !-- Create a NetCDF file and enter the define mode
          CALL dia_nam( clhstnam, nn_trd_trc, 'trdbio' )
          CALL histbeg( clhstnam, jpi, glamt, jpj, gphit,                                            &
             &             1, jpi, 1, jpj, iiter, zjulian, rdt, nh_tb, nidtrdbio, domain_id=nidom, snc4chunks=snc4set )
#endif

      !-- Define physical units
      IF( rn_ucf_trc == 1. ) THEN
         cltrcu = "(mmole-N/m3)/sec"                              ! all passive tracers have the same unit 
      ELSEIF ( rn_ucf_trc == 3600.*24.) THEN                         ! ??? trop long : seulement (mmole-N/m3)
         cltrcu = "(mmole-N/m3)/day"                              ! ??? apparait dans les sorties netcdf 
      ELSE
         cltrcu = "unknown?"
      ENDIF

      !-- Define miscellaneous passive tracer mixed-layer variables 
      IF( jpltrd_trc /= jpmld_trc_atf .OR.  jpltrd_trc - 1 /= jpmld_trc_radb ) THEN
         STOP 'Error : jpltrd_trc /= jpmld_trc_atf .OR.  jpltrd_trc - 1 /= jpmld_trc_radb'  ! see below
      ENDIF

      DO jn = 1, jptra
         !
         IF( ln_trdtrc(jn) ) THEN
            clvar = trim(ctrcnm(jn))//"ml"                           ! e.g. detml, zooml, no3ml, etc.
            CALL histdef(nidtrd(jn), trim(clvar),           clmxl//" "//trim(ctrcnm(jn))//" Mixed Layer ",                         &
              & "mmole-N/m3", jpi, jpj, nh_t(jn), 1  , 1, 1  , -99 , 32, clop, zsto, zout )           
            CALL histdef(nidtrd(jn), trim(clvar)//"_tot"  , clmxl//" "//trim(ctrcnm(jn))//" Total trend ",                         & 
              &       cltrcu, jpi, jpj, nh_t(jn), 1  , 1, 1  , -99 , 32, clop, zout, zout ) 
            CALL histdef(nidtrd(jn), trim(clvar)//"_res"  , clmxl//" "//trim(ctrcnm(jn))//" dh/dt Entrainment (Resid.)",           & 
              &       cltrcu, jpi, jpj, nh_t(jn), 1  , 1, 1  , -99 , 32, clop, zout, zout )                   
         
            DO jl = 1, jpltrd_trc - 2                                ! <== only true if jpltrd_trc == jpmld_trc_atf
               CALL histdef(nidtrd(jn), trim(clvar)//trim(ctrd_trc(jl,2)), clmxl//" "//clvar//ctrd_trc(jl,1),                      & 
                 &    cltrcu, jpi, jpj, nh_t(jn), 1  , 1, 1  , -99 , 32, clop, zsto, zout ) ! IOIPSL: time mean
            END DO                                                                         ! if zsto=rdt above
         
            CALL histdef(nidtrd(jn), trim(clvar)//trim(ctrd_trc(jpmld_trc_radb,2)), clmxl//" "//clvar//ctrd_trc(jpmld_trc_radb,1), & 
              &       cltrcu, jpi, jpj, nh_t(jn), 1  , 1, 1  , -99 , 32, clop, zout, zout ) ! IOIPSL: NO time mean
         
            CALL histdef(nidtrd(jn), trim(clvar)//trim(ctrd_trc(jpmld_trc_atf,2)), clmxl//" "//clvar//ctrd_trc(jpmld_trc_atf,1),   & 
              &       cltrcu, jpi, jpj, nh_t(jn), 1  , 1, 1  , -99 , 32, clop, zout, zout ) ! IOIPSL: NO time mean
         !
         ENDIF
      END DO

#if defined key_lobster
      DO jl = 1, jp_lobster_trd
         CALL histdef(nidtrdbio, TRIM("ML_"//ctrd_bio(jl,2)), TRIM(clmxl//" ML_"//ctrd_bio(jl,1))   ,            &
             &    cltrcu, jpi, jpj, nh_tb, 1  , 1, 1  , -99 , 32, clop, zsto, zout ) ! IOIPSL: time mean
      END DO                                                                         ! if zsto=rdt above
#endif

      !-- Leave IOIPSL/NetCDF define mode
      DO jn = 1, jptra
         IF( ln_trdtrc(jn) )  CALL histend( nidtrd(jn), snc4set )
      END DO

#if defined key_lobster
      !-- Leave IOIPSL/NetCDF define mode
      CALL histend( nidtrdbio, snc4set )

      IF(lwp) WRITE(numout,*)
       IF(lwp) WRITE(numout,*) 'End of NetCDF Initialization for ML bio trends'
#endif

#endif        /* key_dimgout */
   END SUBROUTINE trd_mld_trc_init

#else
   !!----------------------------------------------------------------------
   !!   Default option :                                       Empty module
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trd_mld_trc( kt )                                   ! Empty routine
      INTEGER, INTENT( in) ::   kt
      WRITE(*,*) 'trd_mld_trc: You should not have seen this print! error?', kt
   END SUBROUTINE trd_mld_trc
   SUBROUTINE trd_mld_bio( kt )
      INTEGER, INTENT( in) ::   kt
      WRITE(*,*) 'trd_mld_bio: You should not have seen this print! error?', kt
   END SUBROUTINE trd_mld_bio
   SUBROUTINE trd_mld_trc_zint( ptrc_trdmld, ktrd, ctype, kjn )
      INTEGER               , INTENT( in ) ::  ktrd, kjn              ! ocean trend index and passive tracer rank
      CHARACTER(len=2)      , INTENT( in ) ::  ctype                  ! surface/bottom (2D) or interior (3D) physics
      REAL, DIMENSION(:,:,:), INTENT( in ) ::  ptrc_trdmld            ! passive trc trend
      WRITE(*,*) 'trd_mld_trc_zint: You should not have seen this print! error?', ptrc_trdmld(1,1,1)
      WRITE(*,*) '  "      "      : You should not have seen this print! error?', ctype
      WRITE(*,*) '  "      "      : You should not have seen this print! error?', ktrd
      WRITE(*,*) '  "      "      : You should not have seen this print! error?', kjn
   END SUBROUTINE trd_mld_trc_zint
   SUBROUTINE trd_mld_trc_init                                    ! Empty routine
      WRITE(*,*) 'trd_mld_trc_init: You should not have seen this print! error?'
   END SUBROUTINE trd_mld_trc_init
#endif

   !!======================================================================
END MODULE trdmld_trc
