MODULE step
   !!======================================================================
   !!                       ***  MODULE step  ***
   !! Time-stepping    : manager of the ocean, tracer and ice time stepping
   !!======================================================================
   !! History :  OPA  !  1991-03  (G. Madec)  Original code
   !!             -   !  1991-11  (G. Madec)
   !!             -   !  1992-06  (M. Imbard)  add a first output record
   !!             -   !  1996-04  (G. Madec)  introduction of dynspg
   !!             -   !  1996-04  (M.A. Foujols)  introduction of passive tracer
   !!            8.0  !  1997-06  (G. Madec)  new architecture of call
   !!            8.2  !  1997-06  (G. Madec, M. Imbard, G. Roullet)  free surface
   !!             -   !  1999-02  (G. Madec, N. Grima)  hpg implicit
   !!             -   !  2000-07  (J-M Molines, M. Imbard)  Open Bondary Conditions
   !!   NEMO     1.0  !  2002-06  (G. Madec)  free form, suppress macro-tasking
   !!             -   !  2004-08  (C. Talandier) New trends organization
   !!             -   !  2005-01  (C. Ethe) Add the KPP closure scheme
   !!             -   !  2005-11  (G. Madec)  Reorganisation of tra and dyn calls
   !!             -   !  2006-01  (L. Debreu, C. Mazauric)  Agrif implementation
   !!             -   !  2006-07  (S. Masson)  restart using iom
   !!            3.2  !  2009-02  (G. Madec, R. Benshila)  reintroduicing z*-coordinate
   !!             -   !  2009-06  (S. Masson, G. Madec)  TKE restart compatible with key_cpl
   !!            3.3  !  2010-05  (K. Mogensen, A. Weaver, M. Martin, D. Lea) Assimilation interface
   !!             -   !  2010-10  (C. Ethe, G. Madec) reorganisation of initialisation phase + merge TRC-TRA
   !!            3.4  !  2011-04  (G. Madec, C. Ethe) Merge of dtatem and dtasal
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   stp             : OPA system time-stepping
   !!----------------------------------------------------------------------
   USE step_oce         ! time stepping definition modules 
#if defined key_top
   USE trcstp           ! passive tracer time-stepping      (trc_stp routine)
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC   stp   ! called by opa.F90

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "zdfddm_substitute.h90"
#if defined key_mpp_mpi
   INCLUDE 'mpif.h'
#endif
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: step.F90 3415 2012-06-15 13:29:37Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

#if defined key_agrif
   SUBROUTINE stp( )
      INTEGER             ::   kstp   ! ocean time-step index
#else
   SUBROUTINE stp( kstp )
      INTEGER, INTENT(in) ::   kstp   ! ocean time-step index
#endif
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE stp  ***
      !!                      
      !! ** Purpose : - Time stepping of OPA (momentum and active tracer eqs.)
      !!              - Time stepping of LIM (dynamic and thermodynamic eqs.)
      !!              - Tme stepping  of TRC (passive tracer eqs.)
      !! 
      !! ** Method  : -1- Update forcings and data  
      !!              -2- Update ocean physics 
      !!              -3- Compute the t and s trends 
      !!              -4- Update t and s 
      !!              -5- Compute the momentum trends
      !!              -6- Update the horizontal velocity
      !!              -7- Compute the diagnostics variables (rd,N2, div,cur,w)
      !!              -8- Outputs and diagnostics
      !!----------------------------------------------------------------------
      INTEGER ::   jk       ! dummy loop indice
      INTEGER ::   indic    ! error indicator if < 0
      
      REAL(kind=wp)  ::   t_start
      !! ---------------------------------------------------------------------

#if defined key_mpp_mpi
      t_start = MPI_Wtime()
#endif
#if defined key_agrif
      kstp = nit000 + Agrif_Nb_Step()
!      IF ( Agrif_Root() .and. lwp) Write(*,*) '---'
!      IF (lwp) Write(*,*) 'Grid Number',Agrif_Fixed(),' time step ',kstp
# if defined key_iomput
      IF( Agrif_Nbstepint() == 0 )   CALL iom_swap
# endif   
#endif   
                             indic = 0                ! reset to no error condition
      IF( kstp == nit000 )   CALL iom_init   ! iom_put initialization
      IF( kstp /= nit000 )   CALL day( kstp )         ! Calendar (day was already called at nit000 in day_init)
                             CALL iom_setkt( kstp - nit000 + 1 )   ! say to iom that we are at time step kstp

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Update data, open boundaries, surface boundary condition (including sea-ice)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                         CALL sbc    ( kstp )         ! Sea Boundary Condition (including sea-ice)
      IF( lk_tide    )   CALL sbc_tide( kstp )
      IF( lk_obc     )   CALL obc_dta( kstp )         ! update dynamic and tracer data at open boundaries
      IF( lk_obc     )   CALL obc_rad( kstp )         ! compute phase velocities at open boundaries
      IF( lk_bdy     )   CALL bdy_dta( kstp, time_offset=+1 ) ! update dynamic and tracer data at open boundaries

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !  Ocean dynamics : ssh, wn, hdiv, rot                                 !
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                         CALL ssh_wzv( kstp )         ! after ssh & vertical velocity

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Ocean physics update                (ua, va, tsa used as workspace)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                         CALL bn2( tsb, rn2b )        ! before Brunt-Vaisala frequency
                         CALL bn2( tsn, rn2  )        ! now    Brunt-Vaisala frequency
      !
      !  VERTICAL PHYSICS   
                         CALL zdf_bfr( kstp )         ! bottom friction
                         
      !                                               ! Vertical eddy viscosity and diffusivity coefficients
      IF( lk_zdfric  )   CALL zdf_ric( kstp )            ! Richardson number dependent Kz
      IF( lk_zdftke  )   CALL zdf_tke( kstp )            ! TKE closure scheme for Kz
      IF( lk_zdfgls  )   CALL zdf_gls( kstp )            ! GLS closure scheme for Kz
      IF( lk_zdfkpp  )   CALL zdf_kpp( kstp )            ! KPP closure scheme for Kz
      IF( lk_zdfcst  )   THEN                            ! Constant Kz (reset avt, avm[uv] to the background value)
         avt (:,:,:) = rn_avt0 * tmask(:,:,:)
         avmu(:,:,:) = rn_avm0 * umask(:,:,:)
         avmv(:,:,:) = rn_avm0 * vmask(:,:,:)
      ENDIF
      IF( ln_rnf_mouth ) THEN                         ! increase diffusivity at rivers mouths
         DO jk = 2, nkrnf   ;   avt(:,:,jk) = avt(:,:,jk) + 2.e0 * rn_avt_rnf * rnfmsk(:,:) * tmask(:,:,jk)   ;   END DO
      ENDIF
      IF( ln_zdfevd  )   CALL zdf_evd( kstp )         ! enhanced vertical eddy diffusivity

      IF( lk_zdftmx  )   CALL zdf_tmx( kstp )         ! tidal vertical mixing

      IF( lk_zdfddm .AND. .NOT. lk_zdfkpp )   &
         &               CALL zdf_ddm( kstp )         ! double diffusive mixing
         
                         CALL zdf_mxl( kstp )         ! mixed layer depth

                                                      ! write TKE or GLS information in the restart file
      IF( lrst_oce .AND. lk_zdftke )   CALL tke_rst( kstp, 'WRITE' )
      IF( lrst_oce .AND. lk_zdfgls )   CALL gls_rst( kstp, 'WRITE' )
      !
      !  LATERAL  PHYSICS 
      !
      IF( lk_ldfslp ) THEN                            ! slope of lateral mixing
                         CALL eos( tsb, rhd )                ! before in situ density
         IF( ln_zps )    CALL zps_hde( kstp, jpts, tsb, gtsu, gtsv,  &    ! Partial steps: before horizontal gradient
            &                                      rhd, gru , grv  )      ! of t, s, rd at the last ocean level
         IF( ln_traldf_grif ) THEN                           ! before slope for Griffies operator
                         CALL ldf_slp_grif( kstp )
         ELSE
                         CALL ldf_slp( kstp, rhd, rn2b )     ! before slope for Madec operator
         ENDIF
      ENDIF
#if defined key_traldf_c2d
      IF( lk_traldf_eiv )   CALL ldf_eiv( kstp )      ! eddy induced velocity coefficient
#endif

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! diagnostics and outputs             (ua, va, tsa used as workspace)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      IF( lk_floats  )   CALL flo_stp( kstp )         ! drifting Floats
      IF( lk_diahth  )   CALL dia_hth( kstp )         ! Thermocline depth (20 degres isotherm depth)
      IF( lk_diafwb  )   CALL dia_fwb( kstp )         ! Fresh water budget diagnostics
      IF( ln_diaptr  )   CALL dia_ptr( kstp )         ! Poleward TRansports diagnostics
      IF( lk_diadct  )   CALL dia_dct( kstp )         ! Transports
      IF( lk_diaar5  )   CALL dia_ar5( kstp )         ! ar5 diag
      IF( lk_diaharm )   CALL dia_harm( kstp )        ! Tidal harmonic analysis
                         CALL dia_wri( kstp )         ! ocean model: outputs

#if defined key_top
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Passive Tracer Model
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                         CALL trc_stp( kstp )         ! time-stepping
#endif

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Active tracers                              (ua, va used as workspace)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                             tsa(:,:,:,:) = 0.e0            ! set tracer trends to zero

      IF(  ln_asmiau .AND. &
         & ln_trainc     )   CALL tra_asm_inc( kstp )       ! apply tracer assimilation increment
                             CALL tra_sbc    ( kstp )       ! surface boundary condition
      IF( ln_traqsr      )   CALL tra_qsr    ( kstp )       ! penetrative solar radiation qsr
      IF( ln_trabbc      )   CALL tra_bbc    ( kstp )       ! bottom heat flux
      IF( lk_trabbl      )   CALL tra_bbl    ( kstp )       ! advective (and/or diffusive) bottom boundary layer scheme
      IF( ln_tradmp      )   CALL tra_dmp    ( kstp )       ! internal damping trends
                             CALL tra_adv    ( kstp )       ! horizontal & vertical advection
      IF( lk_zdfkpp      )   CALL tra_kpp    ( kstp )       ! KPP non-local tracer fluxes
                             CALL tra_ldf    ( kstp )       ! lateral mixing
#if defined key_agrif
      IF(.NOT. Agrif_Root()) CALL Agrif_Sponge_tra          ! tracers sponge
#endif
                             CALL tra_zdf    ( kstp )       ! vertical mixing and after tracer fields

      IF( ln_dynhpg_imp  ) THEN                             ! semi-implicit hpg (time stepping then eos)
         IF( ln_zdfnpc   )   CALL tra_npc( kstp )                ! update after fields by non-penetrative convection
                             CALL tra_nxt( kstp )                ! tracer fields at next time step
                             CALL eos    ( tsa, rhd, rhop )      ! Time-filtered in situ density for hpg computation
         IF( ln_zps      )   CALL zps_hde( kstp, jpts, tsa, gtsu, gtsv,  &    ! zps: time filtered hor. derivative
            &                                          rhd, gru , grv  )      ! of t, s, rd at the last ocean level
         
      ELSE                                                  ! centered hpg  (eos then time stepping)
                             CALL eos    ( tsn, rhd, rhop )      ! now in situ density for hpg computation
         IF( ln_zps      )   CALL zps_hde( kstp, jpts, tsn, gtsu, gtsv,  &    ! zps: now hor. derivative
            &                                          rhd, gru , grv  )      ! of t, s, rd at the last ocean level
         IF( ln_zdfnpc   )   CALL tra_npc( kstp )                ! update after fields by non-penetrative convection
                             CALL tra_nxt( kstp )                ! tracer fields at next time step
      ENDIF 
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Dynamics                                    (tsa used as workspace)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                               ua(:,:,:) = 0.e0             ! set dynamics trends to zero
                               va(:,:,:) = 0.e0

      IF(  ln_asmiau .AND. &
         & ln_dyninc       )   CALL dyn_asm_inc( kstp )     ! apply dynamics assimilation increment
      IF( ln_neptsimp )        CALL dyn_nept_cor( kstp )    ! subtract Neptune velocities (simplified)
                               CALL dyn_adv( kstp )         ! advection (vector or flux form)
                               CALL dyn_vor( kstp )         ! vorticity term including Coriolis
                               CALL dyn_ldf( kstp )         ! lateral mixing
      IF( ln_neptsimp )        CALL dyn_nept_cor( kstp )    ! add Neptune velocities (simplified)
#if defined key_agrif
      IF(.NOT. Agrif_Root())   CALL Agrif_Sponge_dyn        ! momemtum sponge
#endif
                               CALL dyn_hpg( kstp )         ! horizontal gradient of Hydrostatic pressure
                               CALL dyn_bfr( kstp )         ! bottom friction   
                               CALL dyn_zdf( kstp )         ! vertical diffusion
                               CALL dyn_spg( kstp, indic )  ! surface pressure gradient
                               CALL dyn_nxt( kstp )         ! lateral velocity at next time step

                               CALL ssh_nxt( kstp )         ! sea surface height at next time step

      IF( ln_diahsb        )   CALL dia_hsb( kstp )         ! - ML - global conservation diagnostics
      IF( lk_diaobs  )         CALL dia_obs( kstp )         ! obs-minus-model (assimilation) diagnostics (call after dynamics update)

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Control and restarts
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                               CALL stp_ctl( kstp, indic )
      IF( indic < 0        )   THEN
                               CALL ctl_stop( 'step: indic < 0' )
                               CALL dia_wri_state( 'output.abort', kstp )
      ENDIF
      IF( kstp == nit000   )   CALL iom_close( numror )     ! close input  ocean restart file
      IF( lrst_oce         )   CALL rst_write    ( kstp )   ! write output ocean restart file
      IF( lk_obc           )   CALL obc_rst_write( kstp )   ! write open boundary restart file

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Trends                              (ua, va, tsa used as workspace)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      IF( nstop == 0 ) THEN                         
         IF( lk_trddyn     )   CALL trd_dwr( kstp )         ! trends: dynamics 
         IF( lk_trdtra     )   CALL trd_twr( kstp )         ! trends: active tracers
         IF( lk_trdmld     )   CALL trd_mld( kstp )         ! trends: Mixed-layer 
         IF( lk_trdvor     )   CALL trd_vor( kstp )         ! trends: vorticity budget
      ENDIF

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Coupled mode
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      IF( lk_cpl           )   CALL sbc_cpl_snd( kstp )     ! coupled mode : field exchanges
      !
      IF( nn_timing == 1 .AND.  kstp == nit000  )   CALL timing_reset
      !
#if defined key_iomput
      IF( kstp == nitend .OR. indic < 0 )   CALL xios_context_finalize()
#endif
      !
#if defined key_mpp_mpi
      IF( lwp ) WRITE(numout,'(a, i6, f10.5)') 'step timing ', kstp, MPI_WTIME() - t_start
#endif
      !
   END SUBROUTINE stp

   !!======================================================================
END MODULE step
