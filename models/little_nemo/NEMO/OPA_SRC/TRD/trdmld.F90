MODULE trdmld
   !!======================================================================
   !!                       ***  MODULE  trdmld  ***
   !! Ocean diagnostics:  mixed layer T-S trends 
   !!=====================================================================
   !! History :       !  95-04  (J. Vialard)    Original code
   !!                 !  97-02  (E. Guilyardi)  Adaptation global + base cmo
   !!                 !  99-09  (E. Guilyardi)  Re-writing + netCDF output
   !!            8.5  !  02-06  (G. Madec)      F90: Free form and module
   !!            9.0  !  04-08  (C. Talandier)  New trends organization
   !!                 !  05-05  (C. Deltel)     Diagnose trends of time averaged ML T & S
   !!----------------------------------------------------------------------
#if   defined key_trdmld   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_trdmld'                          mixed layer trend diagnostics
   !!----------------------------------------------------------------------
   !!   trd_mld          : T and S cumulated trends averaged over the mixed layer
   !!   trd_mld_zint     : T and S trends vertical integration
   !!   trd_mld_init     : initialization step
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables
   USE trdmod_oce      ! ocean variables trends
   USE trdmld_oce      ! ocean variables trends
   USE ldftra_oce      ! ocean active tracers lateral physics
   USE zdf_oce         ! ocean vertical physics
   USE in_out_manager  ! I/O manager
   USE phycst          ! Define parameters for the routines
   USE dianam          ! build the name of file (routine)
   USE ldfslp          ! iso-neutral slopes 
   USE zdfmxl          ! mixed layer depth
   USE zdfddm          ! ocean vertical physics: double diffusion
   USE ioipsl          ! NetCDF library
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE diadimg         ! dimg direct access file format output
   USE trdmld_rst      ! restart for diagnosing the ML trends
   USE prtctl          ! Print control
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! Memory allocation

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trd_mld        ! routine called by step.F90
   PUBLIC   trd_mld_init   ! routine called by opa.F90
   PUBLIC   trd_mld_zint   ! routine called by tracers routines

   CHARACTER (LEN=40) ::  clhstnam         ! name of the trends NetCDF file
   INTEGER ::   nh_t, nmoymltrd
   INTEGER ::   nidtrd
   INTEGER, ALLOCATABLE, SAVE, DIMENSION(:) ::   ndextrd1
   INTEGER ::   ndimtrd1                        
   INTEGER ::   ionce, icount                   

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "ldftra_substitute.h90"
#  include "zdfddm_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: trdmld.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION trd_mld_alloc()
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_mld_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( ndextrd1(jpi*jpj) , STAT=trd_mld_alloc )
      !
      IF( lk_mpp             )   CALL mpp_sum ( trd_mld_alloc )
      IF( trd_mld_alloc /= 0 )   CALL ctl_warn('trd_mld_alloc: failed to allocate array ndextrd1')
   END FUNCTION trd_mld_alloc


   SUBROUTINE trd_mld_zint( pttrdmld, pstrdmld, ktrd, ctype )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_mld_zint  ***
      !! 
      !! ** Purpose :   Compute the vertical average of the 3D fields given as arguments 
      !!                to the subroutine. This vertical average is performed from ocean
      !!                surface down to a chosen control surface.
      !!
      !! ** Method/usage :
      !!      The control surface can be either a mixed layer depth (time varying)
      !!      or a fixed surface (jk level or bowl). 
      !!      Choose control surface with nn_ctls in namelist NAMTRD :
      !!        nn_ctls = 0  : use mixed layer with density criterion 
      !!        nn_ctls = 1  : read index from file 'ctlsurf_idx'
      !!        nn_ctls > 1  : use fixed level surface jk = nn_ctls
      !!      Note: in the remainder of the routine, the volume between the 
      !!            surface and the control surface is called "mixed-layer"
      !!----------------------------------------------------------------------
      !
      INTEGER                         , INTENT( in ) ::   ktrd       ! ocean trend index
      CHARACTER(len=2)                , INTENT( in ) ::   ctype      ! 2D surface/bottom or 3D interior physics
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT( in ) ::   pttrdmld   ! temperature trend 
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT( in ) ::   pstrdmld   ! salinity trend 
      !
      INTEGER ::   ji, jj, jk, isum
      REAL(wp), POINTER, DIMENSION(:,:)  :: zvlmsk 
      !!----------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, zvlmsk ) 

      ! I. Definition of control surface and associated fields
      ! ------------------------------------------------------
      !            ==> only once per time step <== 

      IF( icount == 1 ) THEN        
         !
         tmltrd(:,:,:) = 0.e0    ;    smltrd(:,:,:) = 0.e0    ! <<< reset trend arrays to zero
         
         ! ... Set nmld(ji,jj) = index of first T point below control surf. or outside mixed-layer
         IF( nn_ctls == 0 ) THEN       ! * control surface = mixed-layer with density criterion 
            nmld(:,:) = nmln(:,:)    ! array nmln computed in zdfmxl.F90
         ELSE IF( nn_ctls == 1 ) THEN  ! * control surface = read index from file 
            nmld(:,:) = nbol(:,:)
         ELSE IF( nn_ctls >= 2 ) THEN  ! * control surface = model level
            nn_ctls = MIN( nn_ctls, jpktrd - 1 )
            nmld(:,:) = nn_ctls + 1
         ENDIF

         ! ... Compute ndextrd1 and ndimtrd1 only once
         IF( ionce == 1 ) THEN
            !
            ! Check of validity : nmld(ji,jj) <= jpktrd
            isum        = 0
            zvlmsk(:,:) = 0.e0

            IF( jpktrd < jpk ) THEN 
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     IF( nmld(ji,jj) <= jpktrd ) THEN
                        zvlmsk(ji,jj) = tmask(ji,jj,1)
                     ELSE
                        isum = isum + 1
                        zvlmsk(ji,jj) = 0.
                     ENDIF
                  END DO
               END DO
            ENDIF

            ! Index of ocean points (2D only)
            IF( isum > 0 ) THEN
               WRITE(numout,*)' Number of invalid points nmld > jpktrd', isum 
               CALL wheneq( jpi*jpj, zvlmsk(:,:) , 1, 1., ndextrd1, ndimtrd1 )    ! surface
            ELSE 
               CALL wheneq( jpi*jpj, tmask(:,:,1), 1, 1., ndextrd1, ndimtrd1 )    ! surface
            ENDIF                                

            ionce = 0                ! no more pass here
            !
         END IF
         
         ! ... Weights for vertical averaging
         wkx(:,:,:) = 0.e0
         DO jk = 1, jpktrd             ! initialize wkx with vertical scale factor in mixed-layer
            DO jj = 1,jpj
               DO ji = 1,jpi
                  IF( jk - nmld(ji,jj) < 0.e0 )   wkx(ji,jj,jk) = fse3t(ji,jj,jk) * tmask(ji,jj,jk)
               END DO
            END DO
         END DO
         
         rmld(:,:) = 0.e0                ! compute mixed-layer depth : rmld
         DO jk = 1, jpktrd
            rmld(:,:) = rmld(:,:) + wkx(:,:,jk)
         END DO
         
         DO jk = 1, jpktrd             ! compute integration weights
            wkx(:,:,jk) = wkx(:,:,jk) / MAX( 1., rmld(:,:) )
         END DO

         icount = 0                    ! <<< flag = off : control surface & integr. weights
         !                             !     computed only once per time step
      END IF

      ! II. Vertical integration of trends in the mixed-layer
      ! -----------------------------------------------------

      SELECT CASE (ctype)
      CASE ( '3D' )   ! mean T/S trends in the mixed-layer
         DO jk = 1, jpktrd
            tmltrd(:,:,ktrd) = tmltrd(:,:,ktrd) + pttrdmld(:,:,jk) * wkx(:,:,jk)   ! temperature
            smltrd(:,:,ktrd) = smltrd(:,:,ktrd) + pstrdmld(:,:,jk) * wkx(:,:,jk)   ! salinity
         END DO
      CASE ( '2D' )   ! forcing at upper boundary of the mixed-layer
         tmltrd(:,:,ktrd) = tmltrd(:,:,ktrd) + pttrdmld(:,:,1) * wkx(:,:,1)        ! non penetrative
         smltrd(:,:,ktrd) = smltrd(:,:,ktrd) + pstrdmld(:,:,1) * wkx(:,:,1)            
      END SELECT
      !
      CALL wrk_dealloc( jpi, jpj, zvlmsk ) 
      !
   END SUBROUTINE trd_mld_zint
    

   SUBROUTINE trd_mld( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_mld  ***
      !! 
      !! ** Purpose :  Compute and cumulate the mixed layer trends over an analysis
      !!               period, and write NetCDF (or dimg) outputs.
      !!
      !! ** Method/usage :
      !!          The stored trends can be chosen twofold (according to the ln_trdmld_instant 
      !!          logical namelist variable) :
      !!          1) to explain the difference between initial and final 
      !!             mixed-layer T & S (where initial and final relate to the
      !!             current analysis window, defined by nn_trd in the namelist)
      !!          2) to explain the difference between the current and previous 
      !!             TIME-AVERAGED mixed-layer T & S (where time-averaging is
      !!             performed over each analysis window).
      !!
      !! ** Consistency check : 
      !!        If the control surface is fixed ( nn_ctls > 1 ), the residual term (dh/dt
      !!        entrainment) should be zero, at machine accuracy. Note that in the case
      !!        of time-averaged mixed-layer fields, this residual WILL NOT BE ZERO
      !!        over the first two analysis windows (except if restart).
      !!        N.B. For ORCA2_LIM, use e.g. nn_trd=5, rn_ucf=1., nn_ctls=8
      !!             for checking residuals.
      !!             On a NEC-SX5 computer, this typically leads to:
      !!                   O(1.e-20) temp. residuals (tml_res) when ln_trdmld_instant=.false.
      !!                   O(1.e-21) temp. residuals (tml_res) when ln_trdmld_instant=.true.
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
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index
      !
      INTEGER :: ji, jj, jk, jl, ik, it, itmod
      LOGICAL :: lldebug = .TRUE.
      REAL(wp) :: zavt, zfn, zfn2
      !                                              ! z(ts)mltot : dT/dt over the anlysis window (including Asselin)
      !                                              ! z(ts)mlres : residual = dh/dt entrainment term
      REAL(wp), POINTER, DIMENSION(:,:  ) ::  ztmltot , zsmltot , ztmlres , zsmlres , ztmlatf , zsmlatf
      REAL(wp), POINTER, DIMENSION(:,:  ) ::  ztmltot2, zsmltot2, ztmlres2, zsmlres2, ztmlatf2, zsmlatf2, ztmltrdm2, zsmltrdm2  
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  ztmltrd2, zsmltrd2   ! only needed for mean diagnostics
#if defined key_dimgout
      INTEGER ::  iyear,imon,iday
      CHARACTER(LEN=80) :: cltext, clmode
#endif
      !!----------------------------------------------------------------------
  
      CALL wrk_alloc( jpi, jpj,         ztmltot , zsmltot , ztmlres , zsmlres , ztmlatf , zsmlatf                        )
      CALL wrk_alloc( jpi, jpj,         ztmltot2, zsmltot2, ztmlres2, zsmlres2, ztmlatf2, zsmlatf2, ztmltrdm2, zsmltrdm2 )  
      CALL wrk_alloc( jpi, jpj, jpltrd, ztmltrd2, zsmltrd2                                                               )

      ! ======================================================================
      ! I. Diagnose the purely vertical (K_z) diffusion trend
      ! ======================================================================

      ! ... These terms can be estimated by flux computation at the lower boundary of the ML 
      !     (we compute (-1/h) * K_z * d_z( T ) and (-1/h) * K_z * d_z( S ))
      IF( ln_traldf_iso ) THEN
         DO jj = 1,jpj
            DO ji = 1,jpi
               ik = nmld(ji,jj)
               zavt = avt(ji,jj,ik)
               tmltrd(ji,jj,jpmld_zdf) = - zavt / fse3w(ji,jj,ik) * tmask(ji,jj,ik)  &
                  &                      * ( tsn(ji,jj,ik-1,jp_tem) - tsn(ji,jj,ik,jp_tem) )         &
                  &                      / MAX( 1., rmld(ji,jj) ) * tmask(ji,jj,1)
               zavt = fsavs(ji,jj,ik)
               smltrd(ji,jj,jpmld_zdf) = - zavt / fse3w(ji,jj,ik) * tmask(ji,jj,ik)  &
                  &                      * ( tsn(ji,jj,ik-1,jp_sal) - tsn(ji,jj,ik,jp_sal) )         &
                  &                      / MAX( 1., rmld(ji,jj) ) * tmask(ji,jj,1)
            END DO
         END DO

         ! ... Remove this K_z trend from the iso-neutral diffusion term (if any)
         tmltrd(:,:,jpmld_ldf) = tmltrd(:,:,jpmld_ldf) - tmltrd(:,:,jpmld_zdf)
         smltrd(:,:,jpmld_ldf) = smltrd(:,:,jpmld_ldf) - smltrd(:,:,jpmld_zdf)
      END IF

      ! ... Lateral boundary conditions
      DO jl = 1, jpltrd
         CALL lbc_lnk( tmltrd(:,:,jl), 'T', 1. )
         CALL lbc_lnk( smltrd(:,:,jl), 'T', 1. )
      END DO

      ! ======================================================================
      ! II. Cumulate the trends over the analysis window
      ! ======================================================================

      ztmltrd2(:,:,:) = 0.e0   ;    zsmltrd2(:,:,:) = 0.e0  ! <<< reset arrays to zero
      ztmltot2(:,:)   = 0.e0   ;    zsmltot2(:,:)   = 0.e0
      ztmlres2(:,:)   = 0.e0   ;    zsmlres2(:,:)   = 0.e0
      ztmlatf2(:,:)   = 0.e0   ;    zsmlatf2(:,:)   = 0.e0

      ! II.1 Set before values of vertically average T and S 
      ! ----------------------------------------------------
      IF( kt > nit000 ) THEN
         !   ... temperature ...                    ... salinity ...
         tmlb   (:,:) = tml   (:,:)           ; smlb   (:,:) = sml   (:,:)
         tmlatfn(:,:) = tmltrd(:,:,jpmld_atf) ; smlatfn(:,:) = smltrd(:,:,jpmld_atf)
      END IF

      ! II.2 Vertically averaged T and S
      ! --------------------------------
      tml(:,:) = 0.e0   ;   sml(:,:) = 0.e0
      DO jk = 1, jpktrd - 1
         tml(:,:) = tml(:,:) + wkx(:,:,jk) * tsn(:,:,jk,jp_tem)
         sml(:,:) = sml(:,:) + wkx(:,:,jk) * tsn(:,:,jk,jp_sal)
      END DO

      ! II.3 Initialize mixed-layer "before" arrays for the 1rst analysis window    
      ! ------------------------------------------------------------------------
      IF( kt == 2 ) THEN  !  i.e. ( .NOT. ln_rstart ).AND.( kt == nit000 + 1)
         !
         !   ... temperature ...                ... salinity ...
         tmlbb  (:,:) = tmlb   (:,:)   ;   smlbb  (:,:) = smlb   (:,:)
         tmlbn  (:,:) = tml    (:,:)   ;   smlbn  (:,:) = sml    (:,:)
         tmlatfb(:,:) = tmlatfn(:,:)   ;   smlatfb(:,:) = smlatfn(:,:)
         
         tmltrd_csum_ub (:,:,:) = 0.e0  ;   smltrd_csum_ub (:,:,:) = 0.e0
         tmltrd_atf_sumb(:,:)   = 0.e0  ;   smltrd_atf_sumb(:,:)   = 0.e0

         rmldbn(:,:) = rmld(:,:)

         IF( ln_ctl ) THEN
            WRITE(numout,*) '             we reach kt == nit000 + 1 = ', nit000+1
            CALL prt_ctl(tab2d_1=tmlbb   , clinfo1=' tmlbb   -   : ', mask1=tmask, ovlap=1)
            CALL prt_ctl(tab2d_1=tmlbn   , clinfo1=' tmlbn   -   : ', mask1=tmask, ovlap=1)
            CALL prt_ctl(tab2d_1=tmlatfb , clinfo1=' tmlatfb -   : ', mask1=tmask, ovlap=1)
         END IF
         !
      END IF

      IF( ( ln_rstart ) .AND. ( kt == nit000 ) .AND. ( ln_ctl ) ) THEN
         IF( ln_trdmld_instant ) THEN
            WRITE(numout,*) '             restart from kt == nit000 = ', nit000
            CALL prt_ctl(tab2d_1=tmlbb   , clinfo1=' tmlbb   -   : ', mask1=tmask, ovlap=1)
            CALL prt_ctl(tab2d_1=tmlbn   , clinfo1=' tmlbn   -   : ', mask1=tmask, ovlap=1)
            CALL prt_ctl(tab2d_1=tmlatfb , clinfo1=' tmlatfb -   : ', mask1=tmask, ovlap=1)
         ELSE
            WRITE(numout,*) '             restart from kt == nit000 = ', nit000
            CALL prt_ctl(tab2d_1=tmlbn          , clinfo1=' tmlbn           -  : ', mask1=tmask, ovlap=1)
            CALL prt_ctl(tab2d_1=rmldbn         , clinfo1=' rmldbn          -  : ', mask1=tmask, ovlap=1)
            CALL prt_ctl(tab2d_1=tml_sumb       , clinfo1=' tml_sumb        -  : ', mask1=tmask, ovlap=1)
            CALL prt_ctl(tab2d_1=tmltrd_atf_sumb, clinfo1=' tmltrd_atf_sumb -  : ', mask1=tmask, ovlap=1)
            CALL prt_ctl(tab3d_1=tmltrd_csum_ub , clinfo1=' tmltrd_csum_ub  -  : ', mask1=tmask, ovlap=1, kdim=1)
         END IF
      END IF

      ! II.4 Cumulated trends over the analysis period
      ! ----------------------------------------------
      !
      !         [  1rst analysis window ] [     2nd analysis window     ]                       
      !
      !     o---[--o-----o-----o-----o--]-[--o-----o-----o-----o-----o--]---o-----o--> time steps
      !                          nn_trd                           2*nn_trd       etc.
      !     1      2     3     4    =5 e.g.                          =10
      !
      IF( ( kt >= 2 ).OR.( ln_rstart ) ) THEN
         !
         nmoymltrd = nmoymltrd + 1
         
         ! ... Cumulate over BOTH physical contributions AND over time steps
         DO jl = 1, jpltrd
            tmltrdm(:,:) = tmltrdm(:,:) + tmltrd(:,:,jl)
            smltrdm(:,:) = smltrdm(:,:) + smltrd(:,:,jl)
         END DO

         ! ... Special handling of the Asselin trend 
         tmlatfm(:,:) = tmlatfm(:,:) + tmlatfn(:,:)
         smlatfm(:,:) = smlatfm(:,:) + smlatfn(:,:)

         ! ... Trends associated with the time mean of the ML T/S
         tmltrd_sum    (:,:,:) = tmltrd_sum    (:,:,:) + tmltrd    (:,:,:) ! tem
         tmltrd_csum_ln(:,:,:) = tmltrd_csum_ln(:,:,:) + tmltrd_sum(:,:,:)
         tml_sum       (:,:)   = tml_sum       (:,:)   + tml       (:,:)
         smltrd_sum    (:,:,:) = smltrd_sum    (:,:,:) + smltrd    (:,:,:) ! sal
         smltrd_csum_ln(:,:,:) = smltrd_csum_ln(:,:,:) + smltrd_sum(:,:,:)
         sml_sum       (:,:)   = sml_sum       (:,:)   + sml       (:,:)
         rmld_sum      (:,:)   = rmld_sum      (:,:)   + rmld      (:,:)   ! rmld
         !
      END IF

      ! ======================================================================
      ! III. Prepare fields for output (get here ONCE PER ANALYSIS PERIOD)
      ! ======================================================================

      ! Convert to appropriate physical units
      ! N.B. It may be useful to check IOIPSL time averaging with :
      !      tmltrd (:,:,:) = 1. ; smltrd (:,:,:) = 1.
      tmltrd(:,:,:) = tmltrd(:,:,:) * rn_ucf   ! (actually needed for 1:jpltrd-1, but trdmld(:,:,jpltrd)
      smltrd(:,:,:) = smltrd(:,:,:) * rn_ucf   !  is no longer used, and is reset to 0. at next time step)
      
      ! define time axis
      it = kt
      itmod = kt - nit000 + 1

      MODULO_NTRD : IF( MOD( itmod, nn_trd ) == 0 ) THEN        ! nitend MUST be multiple of nn_trd
         !
         ztmltot (:,:) = 0.e0   ;   zsmltot (:,:) = 0.e0   ! reset arrays to zero
         ztmlres (:,:) = 0.e0   ;   zsmlres (:,:) = 0.e0
         ztmltot2(:,:) = 0.e0   ;   zsmltot2(:,:) = 0.e0
         ztmlres2(:,:) = 0.e0   ;   zsmlres2(:,:) = 0.e0
      
         zfn  = float(nmoymltrd)    ;    zfn2 = zfn * zfn
         
         ! III.1 Prepare fields for output ("instantaneous" diagnostics) 
         ! -------------------------------------------------------------
         
         !-- Compute total trends
         ztmltot(:,:) = ( tml(:,:) - tmlbn(:,:) + tmlb(:,:) - tmlbb(:,:) ) / ( 2.*rdt )
         zsmltot(:,:) = ( sml(:,:) - smlbn(:,:) + smlb(:,:) - smlbb(:,:) ) / ( 2.*rdt )
         
         !-- Compute residuals
         ztmlres(:,:) = ztmltot(:,:) - ( tmltrdm(:,:) - tmlatfn(:,:) + tmlatfb(:,:) )
         zsmlres(:,:) = zsmltot(:,:) - ( smltrdm(:,:) - smlatfn(:,:) + smlatfb(:,:) )
      
         !-- Diagnose Asselin trend over the analysis window 
         ztmlatf(:,:) = tmlatfm(:,:) - tmlatfn(:,:) + tmlatfb(:,:)
         zsmlatf(:,:) = smlatfm(:,:) - smlatfn(:,:) + smlatfb(:,:)
         
         !-- Lateral boundary conditions
         !         ... temperature ...                    ... salinity ...
         CALL lbc_lnk( ztmltot , 'T', 1. )  ;   CALL lbc_lnk( zsmltot , 'T', 1. )
         CALL lbc_lnk( ztmlres , 'T', 1. )  ;   CALL lbc_lnk( zsmlres , 'T', 1. )
         CALL lbc_lnk( ztmlatf , 'T', 1. )  ;   CALL lbc_lnk( zsmlatf , 'T', 1. )

#if defined key_diainstant
         CALL ctl_stop( 'tml_trd : key_diainstant was never checked within trdmld. Comment this to proceed.')
#endif
         ! III.2 Prepare fields for output ("mean" diagnostics) 
         ! ----------------------------------------------------
         
         !-- Update the ML depth time sum (to build the Leap-Frog time mean)
         rmld_sum(:,:) = rmldbn(:,:) + 2 * ( rmld_sum(:,:) - rmld(:,:) ) + rmld(:,:)

         !-- Compute temperature total trends
         tml_sum (:,:) = tmlbn(:,:) + 2 * ( tml_sum(:,:) - tml(:,:) ) + tml(:,:)
         ztmltot2(:,:) = ( tml_sum(:,:) - tml_sumb(:,:) ) /  ( 2.*rdt )    ! now in degC/s
         
         !-- Compute salinity total trends
         sml_sum (:,:) = smlbn(:,:) + 2 * ( sml_sum(:,:) - sml(:,:) ) + sml(:,:)
         zsmltot2(:,:) = ( sml_sum(:,:) - sml_sumb(:,:) ) /  ( 2.*rdt )    ! now in psu/s
         
         !-- Compute temperature residuals
         DO jl = 1, jpltrd
            ztmltrd2(:,:,jl) = tmltrd_csum_ub(:,:,jl) + tmltrd_csum_ln(:,:,jl)
         END DO

         ztmltrdm2(:,:) = 0.e0
         DO jl = 1, jpltrd
            ztmltrdm2(:,:) = ztmltrdm2(:,:) + ztmltrd2(:,:,jl)
         END DO

         ztmlres2(:,:) =  ztmltot2(:,:)  -       &
              ( ztmltrdm2(:,:) - tmltrd_sum(:,:,jpmld_atf) + tmltrd_atf_sumb(:,:) )
         
         !-- Compute salinity residuals
         DO jl = 1, jpltrd
            zsmltrd2(:,:,jl) = smltrd_csum_ub(:,:,jl) + smltrd_csum_ln(:,:,jl)
         END DO

         zsmltrdm2(:,:) = 0.
         DO jl = 1, jpltrd
            zsmltrdm2(:,:) = zsmltrdm2(:,:) + zsmltrd2(:,:,jl)
         END DO

         zsmlres2(:,:) =  zsmltot2(:,:)  -       &
              ( zsmltrdm2(:,:) - smltrd_sum(:,:,jpmld_atf) + smltrd_atf_sumb(:,:) )
         
         !-- Diagnose Asselin trend over the analysis window
         ztmlatf2(:,:) = ztmltrd2(:,:,jpmld_atf) - tmltrd_sum(:,:,jpmld_atf) + tmltrd_atf_sumb(:,:)
         zsmlatf2(:,:) = zsmltrd2(:,:,jpmld_atf) - smltrd_sum(:,:,jpmld_atf) + smltrd_atf_sumb(:,:)

         !-- Lateral boundary conditions
         !         ... temperature ...                    ... salinity ...
         CALL lbc_lnk( ztmltot2, 'T', 1. )  ;   CALL lbc_lnk( zsmltot2, 'T', 1. )
         CALL lbc_lnk( ztmlres2, 'T', 1. )  ;   CALL lbc_lnk( zsmlres2, 'T', 1. )
         DO jl = 1, jpltrd
            CALL lbc_lnk( ztmltrd2(:,:,jl), 'T', 1. ) ! \  these will be output
            CALL lbc_lnk( zsmltrd2(:,:,jl), 'T', 1. ) ! /  in the NetCDF trends file
         END DO
         
         ! III.3 Time evolution array swap
         ! -------------------------------
         
         ! For T/S instantaneous diagnostics 
         !   ... temperature ...               ... salinity ...
         tmlbb  (:,:) = tmlb   (:,:)  ;   smlbb  (:,:) = smlb   (:,:)
         tmlbn  (:,:) = tml    (:,:)  ;   smlbn  (:,:) = sml    (:,:)
         tmlatfb(:,:) = tmlatfn(:,:)  ;   smlatfb(:,:) = smlatfn(:,:)

         ! For T mean diagnostics 
         tmltrd_csum_ub (:,:,:) = zfn * tmltrd_sum(:,:,:) - tmltrd_csum_ln(:,:,:)
         tml_sumb       (:,:)   = tml_sum(:,:)
         tmltrd_atf_sumb(:,:)   = tmltrd_sum(:,:,jpmld_atf)
         
         ! For S mean diagnostics 
         smltrd_csum_ub (:,:,:) = zfn * smltrd_sum(:,:,:) - smltrd_csum_ln(:,:,:)
         sml_sumb       (:,:)   = sml_sum(:,:)
         smltrd_atf_sumb(:,:)   = smltrd_sum(:,:,jpmld_atf)
         
         ! ML depth
         rmldbn         (:,:)   = rmld    (:,:)
         
         IF( ln_ctl ) THEN
            IF( ln_trdmld_instant ) THEN
               CALL prt_ctl(tab2d_1=tmlbb   , clinfo1=' tmlbb   -   : ', mask1=tmask, ovlap=1)
               CALL prt_ctl(tab2d_1=tmlbn   , clinfo1=' tmlbn   -   : ', mask1=tmask, ovlap=1)
               CALL prt_ctl(tab2d_1=tmlatfb , clinfo1=' tmlatfb -   : ', mask1=tmask, ovlap=1)
            ELSE
               CALL prt_ctl(tab2d_1=tmlbn          , clinfo1=' tmlbn           -  : ', mask1=tmask, ovlap=1)
               CALL prt_ctl(tab2d_1=rmldbn         , clinfo1=' rmldbn          -  : ', mask1=tmask, ovlap=1)
               CALL prt_ctl(tab2d_1=tml_sumb       , clinfo1=' tml_sumb        -  : ', mask1=tmask, ovlap=1)
               CALL prt_ctl(tab2d_1=tmltrd_atf_sumb, clinfo1=' tmltrd_atf_sumb -  : ', mask1=tmask, ovlap=1)
               CALL prt_ctl(tab3d_1=tmltrd_csum_ub , clinfo1=' tmltrd_csum_ub  -  : ', mask1=tmask, ovlap=1, kdim=1)
            END IF
         END IF

         ! III.4 Convert to appropriate physical units
         ! -------------------------------------------

         !    ... temperature ...                         ... salinity ...
         ztmltot (:,:)   = ztmltot(:,:)   * rn_ucf/zfn  ; zsmltot (:,:)   = zsmltot(:,:)   * rn_ucf/zfn
         ztmlres (:,:)   = ztmlres(:,:)   * rn_ucf/zfn  ; zsmlres (:,:)   = zsmlres(:,:)   * rn_ucf/zfn
         ztmlatf (:,:)   = ztmlatf(:,:)   * rn_ucf/zfn  ; zsmlatf (:,:)   = zsmlatf(:,:)   * rn_ucf/zfn

         tml_sum (:,:)   = tml_sum (:,:)  /  (2*zfn) ; sml_sum (:,:)   = sml_sum (:,:)  /  (2*zfn)
         ztmltot2(:,:)   = ztmltot2(:,:)  * rn_ucf/zfn2 ; zsmltot2(:,:)   = zsmltot2(:,:)  * rn_ucf/zfn2
         ztmltrd2(:,:,:) = ztmltrd2(:,:,:)* rn_ucf/zfn2 ; zsmltrd2(:,:,:) = zsmltrd2(:,:,:)* rn_ucf/zfn2
         ztmlatf2(:,:)   = ztmlatf2(:,:)  * rn_ucf/zfn2 ; zsmlatf2(:,:)   = zsmlatf2(:,:)  * rn_ucf/zfn2
         ztmlres2(:,:)   = ztmlres2(:,:)  * rn_ucf/zfn2 ; zsmlres2(:,:)   = zsmlres2(:,:)  * rn_ucf/zfn2

         rmld_sum(:,:)   = rmld_sum(:,:)  /  (2*zfn)  ! similar to tml_sum and sml_sum

         ! * Debugging information *
         IF( lldebug ) THEN
            !
            WRITE(numout,*)
            WRITE(numout,*) 'trd_mld : write trends in the Mixed Layer for debugging process:'
            WRITE(numout,*) '~~~~~~~  '
            WRITE(numout,*) '          TRA kt = ', kt, 'nmoymltrd = ', nmoymltrd
            WRITE(numout,*)
            WRITE(numout,*) '          >>>>>>>>>>>>>>>>>>  TRA TEMPERATURE <<<<<<<<<<<<<<<<<<'
            WRITE(numout,*) '          TRA ztmlres    : ', SUM(ztmlres(:,:))
            WRITE(numout,*) '          TRA ztmltot    : ', SUM(ztmltot(:,:))
            WRITE(numout,*) '          TRA tmltrdm    : ', SUM(tmltrdm(:,:))
            WRITE(numout,*) '          TRA tmlatfb    : ', SUM(tmlatfb(:,:))
            WRITE(numout,*) '          TRA tmlatfn    : ', SUM(tmlatfn(:,:))
            DO jl = 1, jpltrd
               WRITE(numout,*) '          * TRA TREND INDEX jpmld_xxx = jl = ', jl, &
                    & ' tmltrd : ', SUM(tmltrd(:,:,jl))
            END DO
            WRITE(numout,*) '          TRA ztmlres (jpi/2,jpj/2) : ', ztmlres (jpi/2,jpj/2)
            WRITE(numout,*) '          TRA ztmlres2(jpi/2,jpj/2) : ', ztmlres2(jpi/2,jpj/2)
            WRITE(numout,*)
            WRITE(numout,*) '          >>>>>>>>>>>>>>>>>>  TRA SALINITY <<<<<<<<<<<<<<<<<<'
            WRITE(numout,*) '          TRA zsmlres    : ', SUM(zsmlres(:,:))
            WRITE(numout,*) '          TRA zsmltot    : ', SUM(zsmltot(:,:))
            WRITE(numout,*) '          TRA smltrdm    : ', SUM(smltrdm(:,:))
            WRITE(numout,*) '          TRA smlatfb    : ', SUM(smlatfb(:,:))
            WRITE(numout,*) '          TRA smlatfn    : ', SUM(smlatfn(:,:))
            DO jl = 1, jpltrd
               WRITE(numout,*) '          * TRA TREND INDEX jpmld_xxx = jl = ', jl, &
                    & ' smltrd : ', SUM(smltrd(:,:,jl))
            END DO
            WRITE(numout,*) '          TRA zsmlres (jpi/2,jpj/2) : ', zsmlres (jpi/2,jpj/2)
            WRITE(numout,*) '          TRA zsmlres2(jpi/2,jpj/2) : ', zsmlres2(jpi/2,jpj/2)
            !
         END IF
         !
      END IF MODULO_NTRD

      ! ======================================================================
      ! IV. Write trends in the NetCDF file
      ! ======================================================================

      ! IV.1 Code for dimg mpp output
      ! -----------------------------

#if defined key_dimgout

      IF( MOD( itmod, nn_trd ) == 0 ) THEN
         iyear =  ndastp/10000
         imon  = (ndastp-iyear*10000)/100
         iday  =  ndastp - imon*100 - iyear*10000
         WRITE(clname,9000) TRIM(cexper),'MLDiags',iyear,imon,iday
         WRITE(clmode,'(f5.1,a)') nn_trd*rdt/86400.,' days average'
         cltext = TRIM(cexper)//' mld diags'//TRIM(clmode)
         CALL dia_wri_dimg (clname, cltext, smltrd, jpltrd, '2')
      END IF

9000  FORMAT(a,"_",a,"_y",i4.4,"m",i2.2,"d",i2.2,".dimgproc")

#else
      
      ! IV.2 Code for IOIPSL/NetCDF output
      ! ----------------------------------

      IF( lwp .AND. MOD( itmod , nn_trd ) == 0 ) THEN
         WRITE(numout,*) ' '
         WRITE(numout,*) 'trd_mld : write trends in the NetCDF file :'
         WRITE(numout,*) '~~~~~~~  '
         WRITE(numout,*) '          ', TRIM(clhstnam), ' at kt = ', kt
         WRITE(numout,*) '          N.B. nmoymltrd = ', nmoymltrd
         WRITE(numout,*) ' '
      END IF
         
      !-- Write the trends for T/S instantaneous diagnostics 
      IF( ln_trdmld_instant ) THEN           

         CALL histwrite( nidtrd, "mxl_depth", it, rmld(:,:), ndimtrd1, ndextrd1 )
         
         !................................. ( ML temperature ) ...................................
         
         !-- Output the fields
         CALL histwrite( nidtrd, "tml"     , it, tml    (:,:), ndimtrd1, ndextrd1 ) 
         CALL histwrite( nidtrd, "tml_tot" , it, ztmltot(:,:), ndimtrd1, ndextrd1 ) 
         CALL histwrite( nidtrd, "tml_res" , it, ztmlres(:,:), ndimtrd1, ndextrd1 ) 
         
         DO jl = 1, jpltrd - 1
            CALL histwrite( nidtrd, trim("tml"//ctrd(jl,2)),            &
                 &          it, tmltrd (:,:,jl), ndimtrd1, ndextrd1 )
         END DO
         
         CALL histwrite( nidtrd, trim("tml"//ctrd(jpmld_atf,2)),        &
              &          it, ztmlatf(:,:), ndimtrd1, ndextrd1 )
         
         !.................................. ( ML salinity ) .....................................
         
         !-- Output the fields
         CALL histwrite( nidtrd, "sml"     , it, sml    (:,:), ndimtrd1, ndextrd1 ) 
         CALL histwrite( nidtrd, "sml_tot" , it, zsmltot(:,:), ndimtrd1, ndextrd1 ) 
         CALL histwrite( nidtrd, "sml_res" , it, zsmlres(:,:), ndimtrd1, ndextrd1 ) 
         
         DO jl = 1, jpltrd - 1
            CALL histwrite( nidtrd, trim("sml"//ctrd(jl,2)),            &
                 &          it, smltrd(:,:,jl), ndimtrd1, ndextrd1 )
         END DO
         
         CALL histwrite( nidtrd, trim("sml"//ctrd(jpmld_atf,2)),        &
              &          it, zsmlatf(:,:), ndimtrd1, ndextrd1 )
         
         IF( kt == nitend )   CALL histclo( nidtrd )

      !-- Write the trends for T/S mean diagnostics 
      ELSE
         
         CALL histwrite( nidtrd, "mxl_depth", it, rmld_sum(:,:), ndimtrd1, ndextrd1 ) 
         
         !................................. ( ML temperature ) ...................................
         
         !-- Output the fields
         CALL histwrite( nidtrd, "tml"     , it, tml_sum (:,:), ndimtrd1, ndextrd1 ) 
         CALL histwrite( nidtrd, "tml_tot" , it, ztmltot2(:,:), ndimtrd1, ndextrd1 ) 
         CALL histwrite( nidtrd, "tml_res" , it, ztmlres2(:,:), ndimtrd1, ndextrd1 ) 
         
         DO jl = 1, jpltrd - 1
            CALL histwrite( nidtrd, trim("tml"//ctrd(jl,2)),            &
                 &          it, ztmltrd2(:,:,jl), ndimtrd1, ndextrd1 )
         END DO
         
         CALL histwrite( nidtrd, trim("tml"//ctrd(jpmld_atf,2)),        &
              &          it, ztmlatf2(:,:), ndimtrd1, ndextrd1 )
         
         !.................................. ( ML salinity ) .....................................
                     
         !-- Output the fields
         CALL histwrite( nidtrd, "sml"     , it, sml_sum (:,:), ndimtrd1, ndextrd1 ) 
         CALL histwrite( nidtrd, "sml_tot" , it, zsmltot2(:,:), ndimtrd1, ndextrd1 ) 
         CALL histwrite( nidtrd, "sml_res" , it, zsmlres2(:,:), ndimtrd1, ndextrd1 ) 
         
         DO jl = 1, jpltrd - 1
            CALL histwrite( nidtrd, trim("sml"//ctrd(jl,2)),            &
                 &          it, zsmltrd2(:,:,jl), ndimtrd1, ndextrd1 )
         END DO
         
         CALL histwrite( nidtrd, trim("sml"//ctrd(jpmld_atf,2)),        &
              &          it, zsmlatf2(:,:), ndimtrd1, ndextrd1 )
         
         IF( kt == nitend )   CALL histclo( nidtrd )

      END IF
      
      ! Compute the control surface (for next time step) : flag = on
      icount = 1
      !
#endif

      IF( MOD( itmod, nn_trd ) == 0 ) THEN
         !
         ! III.5 Reset cumulative arrays to zero
         ! -------------------------------------
         nmoymltrd = 0
         
         !   ... temperature ...               ... salinity ...
         tmltrdm        (:,:)   = 0.e0   ;   smltrdm        (:,:)   = 0.e0
         tmlatfm        (:,:)   = 0.e0   ;   smlatfm        (:,:)   = 0.e0
         tml_sum        (:,:)   = 0.e0   ;   sml_sum        (:,:)   = 0.e0
         tmltrd_csum_ln (:,:,:) = 0.e0   ;   smltrd_csum_ln (:,:,:) = 0.e0
         tmltrd_sum     (:,:,:) = 0.e0   ;   smltrd_sum     (:,:,:) = 0.e0

         rmld_sum       (:,:)   = 0.e0
         !
      END IF

      ! ======================================================================
      ! V. Write restart file
      ! ======================================================================

      IF( lrst_oce )   CALL trd_mld_rst_write( kt ) 

      CALL wrk_dealloc( jpi, jpj,         ztmltot , zsmltot , ztmlres , zsmlres , ztmlatf , zsmlatf                        )
      CALL wrk_dealloc( jpi, jpj,         ztmltot2, zsmltot2, ztmlres2, zsmlres2, ztmlatf2, zsmlatf2, ztmltrdm2, zsmltrdm2 )  
      CALL wrk_dealloc( jpi, jpj, jpltrd, ztmltrd2, zsmltrd2                                                               )
      !
   END SUBROUTINE trd_mld


   SUBROUTINE trd_mld_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_mld_init  ***
      !! 
      !! ** Purpose :   computation of vertically integrated T and S budgets
      !!      from ocean surface down to control surface (NetCDF output)
      !!----------------------------------------------------------------------
      INTEGER :: jl
      INTEGER :: inum   ! logical unit
      REAL(wp) ::   zjulian, zsto, zout
      CHARACTER (LEN=40) ::   clop
      CHARACTER (LEN=12) ::   clmxl, cltu, clsu
      !!----------------------------------------------------------------------

      ! ======================================================================
      ! I. initialization
      ! ======================================================================

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) ' trd_mld_init : Mixed-layer trends'
         WRITE(numout,*) ' ~~~~~~~~~~~~~'
         WRITE(numout,*) '                namelist namtrd read in trd_mod_init                        '
         WRITE(numout,*)
      END IF

      ! I.1 Check consistency of user defined preferences
      ! -------------------------------------------------

      IF( ( lk_trdmld ) .AND. ( MOD( nitend-nit000+1, nn_trd ) /= 0 ) ) THEN
         WRITE(numout,cform_err)
         WRITE(numout,*) '                Your nitend parameter, nitend = ', nitend
         WRITE(numout,*) '                is no multiple of the trends diagnostics frequency        '
         WRITE(numout,*) '                          you defined, nn_trd   = ', nn_trd
         WRITE(numout,*) '                This will not allow you to restart from this simulation.  '
         WRITE(numout,*) '                You should reconsider this choice.                        ' 
         WRITE(numout,*) 
         WRITE(numout,*) '                N.B. the nitend parameter is also constrained to be a     '
         WRITE(numout,*) '                multiple of the sea-ice frequency parameter (typically 5) '
         nstop = nstop + 1
      END IF

      IF( ( lk_trdmld ) .AND. ( nn_cla == 1 ) ) THEN
         WRITE(numout,cform_war)
         WRITE(numout,*) '                You set n_cla = 1. Note that the Mixed-Layer diagnostics  '
         WRITE(numout,*) '                are not exact along the corresponding straits.            '
         nwarn = nwarn + 1
      END IF

      !                                   ! allocate trdmld arrays
      IF( trd_mld_alloc()    /= 0 )   CALL ctl_stop( 'STOP', 'trd_mld_init : unable to allocate trdmld     arrays' )
      IF( trdmld_oce_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'trd_mld_init : unable to allocate trdmld_oce arrays' )

      ! I.2 Initialize arrays to zero or read a restart file
      ! ----------------------------------------------------

      nmoymltrd = 0

      !     ... temperature ...                  ... salinity ...
      tml            (:,:)   = 0.e0    ;    sml            (:,:)   = 0.e0     ! inst.
      tmltrdm        (:,:)   = 0.e0    ;    smltrdm        (:,:)   = 0.e0
      tmlatfm        (:,:)   = 0.e0    ;    smlatfm        (:,:)   = 0.e0
      tml_sum        (:,:)   = 0.e0    ;    sml_sum        (:,:)   = 0.e0     ! mean
      tmltrd_sum     (:,:,:) = 0.e0    ;    smltrd_sum     (:,:,:) = 0.e0
      tmltrd_csum_ln (:,:,:) = 0.e0    ;    smltrd_csum_ln (:,:,:) = 0.e0

      rmld           (:,:)   = 0.e0            
      rmld_sum       (:,:)   = 0.e0

      IF( ln_rstart .AND. ln_trdmld_restart ) THEN
         CALL trd_mld_rst_read
      ELSE
         !     ... temperature ...                  ... salinity ...
         tmlb           (:,:)   = 0.e0    ;    smlb           (:,:)   = 0.e0  ! inst.
         tmlbb          (:,:)   = 0.e0    ;    smlbb          (:,:)   = 0.e0  
         tmlbn          (:,:)   = 0.e0    ;    smlbn          (:,:)   = 0.e0  
         tml_sumb       (:,:)   = 0.e0    ;    sml_sumb       (:,:)   = 0.e0  ! mean
         tmltrd_csum_ub (:,:,:) = 0.e0    ;    smltrd_csum_ub (:,:,:) = 0.e0
         tmltrd_atf_sumb(:,:)   = 0.e0    ;    smltrd_atf_sumb(:,:)   = 0.e0  
      END IF

      icount = 1   ;   ionce  = 1                            ! open specifier

      ! I.3 Read control surface from file ctlsurf_idx
      ! ----------------------------------------------
 
      IF( nn_ctls == 1 ) THEN
         CALL ctl_opn( inum, 'ctlsurf_idx', 'OLD', 'UNFORMATTED', 'SEQUENTIAL', -1, numout, lwp )
         READ ( inum ) nbol
         CLOSE( inum )
      END IF

      ! ======================================================================
      ! II. netCDF output initialization
      ! ======================================================================

#if defined key_dimgout 
      ???
#else
      ! clmxl = legend root for netCDF output
      IF( nn_ctls == 0 ) THEN      ! control surface = mixed-layer with density criterion
         clmxl = 'Mixed Layer '  !                   (array nmln computed in zdfmxl.F90)
      ELSE IF( nn_ctls == 1 ) THEN ! control surface = read index from file 
         clmxl = '      Bowl '
      ELSE IF( nn_ctls >= 2 ) THEN ! control surface = model level
         WRITE(clmxl,'(A10,I2,1X)') 'Levels 1 -', nn_ctls
      END IF

      ! II.1 Define frequency of output and means
      ! -----------------------------------------
      IF( ln_mskland )   THEN   ;   clop = "only(x)"   ! put 1.e+20 on land (very expensive!!)
      ELSE                      ;   clop = "x"         ! no use of the mask value (require less cpu time)
      ENDIF
#  if defined key_diainstant
      IF( .NOT. ln_trdmld_instant ) THEN
         CALL ctl_stop( 'trd_mld : this was never checked. Comment this line to proceed...' )
      END IF
      zsto = nn_trd * rdt
      clop = "inst("//TRIM(clop)//")"
#  else
      IF( ln_trdmld_instant ) THEN
         zsto = rdt                 ! inst. diags : we use IOIPSL time averaging
      ELSE
         zsto = nn_trd * rdt          ! mean  diags : we DO NOT use any IOIPSL time averaging
      END IF
      clop = "ave("//TRIM(clop)//")"
#  endif
      zout = nn_trd * rdt

      IF(lwp) WRITE (numout,*) '                netCDF initialization'

      ! II.2 Compute julian date from starting date of the run
      ! ------------------------------------------------------
      CALL ymds2ju( nyear, nmonth, nday, rdt, zjulian )
      zjulian = zjulian - adatrj   !   set calendar origin to the beginning of the experiment
      IF(lwp) WRITE(numout,*)' '  
      IF(lwp) WRITE(numout,*)'                Date 0 used :',nit000,    &
         &                   ' YEAR ', nyear,' MONTH '      , nmonth,   &
         &                   ' DAY ' , nday, 'Julian day : ', zjulian


      ! II.3 Define the T grid trend file (nidtrd)
      ! ------------------------------------------
      !-- Define long and short names for the NetCDF output variables
      !       ==> choose them according to trdmld_oce.F90 <==

      ctrd(jpmld_xad,1) = " Zonal advection"                  ;   ctrd(jpmld_xad,2) = "_xad"
      ctrd(jpmld_yad,1) = " Meridional advection"             ;   ctrd(jpmld_yad,2) = "_yad"
      ctrd(jpmld_zad,1) = " Vertical advection"               ;   ctrd(jpmld_zad,2) = "_zad"
      ctrd(jpmld_ldf,1) = " Lateral diffusion"                ;   ctrd(jpmld_ldf,2) = "_ldf"
      ctrd(jpmld_for,1) = " Forcing"                          ;   ctrd(jpmld_for,2) = "_for"
      ctrd(jpmld_zdf,1) = " Vertical diff. (Kz)"              ;   ctrd(jpmld_zdf,2) = "_zdf"
      ctrd(jpmld_bbc,1) = " Geothermal flux"                  ;   ctrd(jpmld_bbc,2) = "_bbc"
      ctrd(jpmld_bbl,1) = " Adv/diff. Bottom boundary layer"  ;   ctrd(jpmld_bbl,2) = "_bbl"
      ctrd(jpmld_dmp,1) = " Tracer damping"                   ;   ctrd(jpmld_dmp,2) = "_dmp"
      ctrd(jpmld_npc,1) = " Non penetrative convec. adjust."  ;   ctrd(jpmld_npc,2) = "_npc"
      ctrd(jpmld_atf,1) = " Asselin time filter"              ;   ctrd(jpmld_atf,2) = "_atf"
                                                                  
      !-- Create a NetCDF file and enter the define mode 
      CALL dia_nam( clhstnam, nn_trd, 'trends' )
      IF(lwp) WRITE(numout,*) ' Name of NETCDF file ', clhstnam
      CALL histbeg( clhstnam, jpi, glamt, jpj, gphit,                                            &
      &             1, jpi, 1, jpj, nit000-1, zjulian, rdt, nh_t, nidtrd, domain_id=nidom, snc4chunks=snc4set )

      !-- Define the ML depth variable
      CALL histdef(nidtrd, "mxl_depth", clmxl//"  Mixed Layer Depth"              , "m",         &
                   jpi, jpj, nh_t, 1  , 1, 1  , -99 , 32, clop, zsto, zout )

      !-- Define physical units
      IF     ( rn_ucf == 1.       ) THEN   ;   cltu = "degC/s"     ;   clsu = "p.s.u./s"
      ELSEIF ( rn_ucf == 3600.*24.) THEN   ;   cltu = "degC/day"   ;   clsu = "p.s.u./day"
      ELSE                                 ;   cltu = "unknown?"   ;   clsu = "unknown?"
      END IF


      !-- Define miscellaneous T and S mixed-layer variables 

      IF( jpltrd /= jpmld_atf ) CALL ctl_stop( 'Error : jpltrd /= jpmld_atf' ) ! see below

      !................................. ( ML temperature ) ...................................

      CALL histdef(nidtrd, "tml"      , clmxl//" T Mixed Layer Temperature"       ,  "C",        &
                   jpi, jpj, nh_t, 1  , 1, 1  , -99 , 32, clop, zsto, zout )           
      CALL histdef(nidtrd, "tml_tot",   clmxl//" T Total trend"                   , cltu,        & 
                   jpi, jpj, nh_t, 1  , 1, 1  , -99 , 32, clop, zout, zout )              
      CALL histdef(nidtrd, "tml_res",   clmxl//" T dh/dt Entrainment (Resid.)"    , cltu,        & 
                   jpi, jpj, nh_t, 1  , 1, 1  , -99 , 32, clop, zout, zout )                   
      
      DO jl = 1, jpltrd - 1      ! <== only true if jpltrd == jpmld_atf
         CALL histdef(nidtrd, trim("tml"//ctrd(jl,2)), clmxl//" T"//ctrd(jl,1), cltu,            & 
                   jpi, jpj, nh_t, 1  , 1, 1  , -99 , 32, clop, zsto, zout ) ! IOIPSL: time mean
      END DO                                                                 ! if zsto=rdt above
      
      CALL histdef(nidtrd, trim("tml"//ctrd(jpmld_atf,2)), clmxl//" T"//ctrd(jpmld_atf,1), cltu, & 
                   jpi, jpj, nh_t, 1  , 1, 1  , -99 , 32, clop, zout, zout ) ! IOIPSL: NO time mean
      
      !.................................. ( ML salinity ) .....................................
     
      CALL histdef(nidtrd, "sml"      , clmxl//" S Mixed Layer Salinity"          , "p.s.u.",       &
                   jpi, jpj, nh_t, 1  , 1, 1  , -99 , 32, clop, zsto, zout )           
      CALL histdef(nidtrd, "sml_tot",   clmxl//" S Total trend"                   , clsu,        & 
                   jpi, jpj, nh_t, 1  , 1, 1  , -99 , 32, clop, zout, zout )              
      CALL histdef(nidtrd, "sml_res",   clmxl//" S dh/dt Entrainment (Resid.)"    , clsu,        & 
                   jpi, jpj, nh_t, 1  , 1, 1  , -99 , 32, clop, zout, zout )                   
      
      DO jl = 1, jpltrd - 1      ! <== only true if jpltrd == jpmld_atf
         CALL histdef(nidtrd, trim("sml"//ctrd(jl,2)), clmxl//" S"//ctrd(jl,1), clsu,            & 
                   jpi, jpj, nh_t, 1  , 1, 1  , -99 , 32, clop, zsto, zout ) ! IOIPSL: time mean
      END DO                                                                 ! if zsto=rdt above
      
      CALL histdef(nidtrd, trim("sml"//ctrd(jpmld_atf,2)), clmxl//" S"//ctrd(jpmld_atf,1), clsu, & 
                   jpi, jpj, nh_t, 1  , 1, 1  , -99 , 32, clop, zout, zout ) ! IOIPSL: NO time mean

      !-- Leave IOIPSL/NetCDF define mode
      CALL histend( nidtrd, snc4set )

#endif        /* key_dimgout */
   END SUBROUTINE trd_mld_init

#else
   !!----------------------------------------------------------------------
   !!   Default option :                                       Empty module
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trd_mld( kt )             ! Empty routine
      INTEGER, INTENT( in) ::   kt
      WRITE(*,*) 'trd_mld: You should not have seen this print! error?', kt
   END SUBROUTINE trd_mld
   SUBROUTINE trd_mld_zint( pttrdmld, pstrdmld, ktrd, ctype )
      REAL, DIMENSION(:,:,:), INTENT( in ) ::   &
         pttrdmld, pstrdmld                   ! Temperature and Salinity trends
      INTEGER, INTENT( in ) ::   ktrd         ! ocean trend index
      CHARACTER(len=2), INTENT( in ) ::   &  
         ctype                                ! surface/bottom (2D arrays) or
         !                                    ! interior (3D arrays) physics
      WRITE(*,*) 'trd_mld_zint: You should not have seen this print! error?', pttrdmld(1,1,1)
      WRITE(*,*) '  "      "  : You should not have seen this print! error?', pstrdmld(1,1,1)
      WRITE(*,*) '  "      "  : You should not have seen this print! error?', ctype
      WRITE(*,*) '  "      "  : You should not have seen this print! error?', ktrd
   END SUBROUTINE trd_mld_zint
   SUBROUTINE trd_mld_init              ! Empty routine
      WRITE(*,*) 'trd_mld_init: You should not have seen this print! error?'
   END SUBROUTINE trd_mld_init
#endif

   !!======================================================================
END MODULE trdmld
