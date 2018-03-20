MODULE trdmod
   !!======================================================================
   !!                       ***  MODULE  trdmod  ***
   !! Ocean diagnostics:  ocean tracers and dynamic trends
   !!=====================================================================
   !! History :  1.0  !  2004-08  (C. Talandier) Original code
   !!             -   !  2005-04  (C. Deltel)    Add Asselin trend in the ML budget
   !!            3.3  ! 2010-10  (C. Ethe, G. Madec) reorganisation of initialisation phase
   !!----------------------------------------------------------------------
#if  defined key_trdtra || defined key_trddyn || defined key_trdmld || defined key_trdvor || defined key_esopa
   !!----------------------------------------------------------------------
   !!   trd_mod          : Call the trend to be computed
   !!   trd_mod_init     : Initialization step
   !!----------------------------------------------------------------------
   USE oce                     ! ocean dynamics and tracers variables
   USE dom_oce                 ! ocean space and time domain variables
   USE zdf_oce                 ! ocean vertical physics variables
   USE trdmod_oce              ! ocean variables trends
   USE ldftra_oce              ! ocean active tracers lateral physics
   USE sbc_oce                 ! surface boundary condition: ocean
   USE phycst                  ! physical constants
   USE trdvor                  ! ocean vorticity trends 
   USE trdicp                  ! ocean bassin integral constraints properties
   USE trdmld                  ! ocean active mixed layer tracers trends 
   USE in_out_manager          ! I/O manager
   USE lib_mpp                 ! MPP library
   USE wrk_nemo                ! Memory allocation


   IMPLICIT NONE
   PRIVATE

   REAL(wp) ::   r2dt          ! time-step, = 2 rdttra except at nit000 (=rdttra) if neuler=0

   PUBLIC trd_mod              ! called by all dynXX or traXX modules
   PUBLIC trd_mod_init         ! called by opa.F90 module

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: trdmod.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trd_mod( ptrdx, ptrdy, ktrd, ctype, kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_mod  ***
      !! 
      !! ** Purpose : Dispatch all trends computation, e.g. vorticity, mld or 
      !!              integral constraints
      !!----------------------------------------------------------------------
      !
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   ptrdx   ! Temperature or U trend 
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   ptrdy   ! Salinity    or V trend
      CHARACTER(len=3)          , INTENT(in   ) ::   ctype   ! momentum or tracers trends type 'DYN'/'TRA'
      INTEGER                   , INTENT(in   ) ::   kt      ! time step
      INTEGER                   , INTENT(in   ) ::   ktrd    ! tracer trend index
      !!
      INTEGER ::   ji, jj   ! dummy loop indices
      REAL(wp), POINTER, DIMENSION(:,:)  :: ztswu, ztswv, ztbfu, ztbfv, z2dx, z2dy 
      !!----------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, ztswu, ztswv, ztbfu, ztbfv, z2dx, z2dy )

      z2dx(:,:) = 0._wp   ;   z2dy(:,:) = 0._wp                            ! initialization of workspace arrays

      IF( neuler == 0 .AND. kt == nit000    ) THEN   ;   r2dt =      rdt   ! = rdtra (restart with Euler time stepping)
      ELSEIF(               kt <= nit000 + 1) THEN   ;   r2dt = 2. * rdt   ! = 2 rdttra (leapfrog)
      ENDIF

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! I. Integral Constraints Properties for momentum and/or tracers trends
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      IF( ( mod(kt,nn_trd) == 0 .OR. kt == nit000 .OR. kt == nitend) )   THEN
         !
         IF( lk_trdtra .AND. ctype == 'TRA' )   THEN       ! active tracer trends
            SELECT CASE ( ktrd )
            CASE ( jptra_trd_ldf )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpt_ldf, ctype )   ! lateral diff
            CASE ( jptra_trd_zdf )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpt_zdf, ctype )   ! vertical diff (Kz)
            CASE ( jptra_trd_bbc )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpt_bbc, ctype )   ! bottom boundary cond
            CASE ( jptra_trd_bbl )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpt_bbl, ctype )   ! bottom boundary layer
            CASE ( jptra_trd_npc )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpt_npc, ctype )   ! static instability mixing
            CASE ( jptra_trd_dmp )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpt_dmp, ctype )   ! damping
            CASE ( jptra_trd_qsr )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpt_qsr, ctype )   ! penetrative solar radiat.
            CASE ( jptra_trd_nsr )   ;   z2dx(:,:) = ptrdx(:,:,1)   
                                         z2dy(:,:) = ptrdy(:,:,1)
                                         CALL trd_icp( z2dx , z2dy , jpicpt_nsr, ctype )   ! non solar radiation
            CASE ( jptra_trd_xad )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpt_xad, ctype )   ! x- horiz adv
            CASE ( jptra_trd_yad )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpt_yad, ctype )   ! y- horiz adv
            CASE ( jptra_trd_zad )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpt_zad, ctype )   ! z- vertical adv 
                                         CALL trd_icp( ptrdx, ptrdy, jpicpt_zad, ctype )   
                                         ! compute the surface flux condition wn(:,:,1)*tsn(:,:,1,jp_tem)
                                         z2dx(:,:) = wn(:,:,1)*tsn(:,:,1,jp_tem)/fse3t(:,:,1)
                                         z2dy(:,:) = wn(:,:,1)*tsn(:,:,1,jp_sal)/fse3t(:,:,1)
                                         CALL trd_icp( z2dx , z2dy , jpicpt_zl1, ctype )   ! 1st z- vertical adv 
            END SELECT
         END IF

         IF( lk_trddyn .AND. ctype == 'DYN' )   THEN       ! momentum trends 
            !
            SELECT CASE ( ktrd )
            CASE ( jpdyn_trd_hpg )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpd_hpg, ctype )   ! hydrost. pressure grad
            CASE ( jpdyn_trd_keg )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpd_keg, ctype )   ! KE gradient 
            CASE ( jpdyn_trd_rvo )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpd_rvo, ctype )   ! relative vorticity 
            CASE ( jpdyn_trd_pvo )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpd_pvo, ctype )   ! planetary vorticity
            CASE ( jpdyn_trd_ldf )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpd_ldf, ctype )   ! lateral diffusion 
            CASE ( jpdyn_trd_had )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpd_had, ctype )   ! horizontal advection 
            CASE ( jpdyn_trd_zad )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpd_zad, ctype )   ! vertical advection 
            CASE ( jpdyn_trd_spg )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpd_spg, ctype )   ! surface pressure grad.
            CASE ( jpdyn_trd_dat )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpd_dat, ctype )   ! damping term
            CASE ( jpdyn_trd_zdf )                                                         ! vertical diffusion 
               ! subtract surface forcing/bottom friction trends 
               ! from vertical diffusive momentum trends
               ztswu(:,:) = 0._wp   ;   ztswv(:,:) = 0._wp
               ztbfu(:,:) = 0._wp   ;   ztbfv(:,:) = 0._wp 
               DO jj = 2, jpjm1   
                  DO ji = fs_2, fs_jpim1   ! vector opt.
                     ! save the surface forcing momentum fluxes
                     ztswu(ji,jj) = utau(ji,jj) / ( fse3u(ji,jj,1)*rau0 )
                     ztswv(ji,jj) = vtau(ji,jj) / ( fse3v(ji,jj,1)*rau0 )
                     ! bottom friction contribution now handled explicitly
                     ptrdx(ji,jj,1) = ptrdx(ji,jj,1) - ztswu(ji,jj)
                     ptrdy(ji,jj,1) = ptrdy(ji,jj,1) - ztswv(ji,jj)
                  END DO
               END DO
               !
               CALL trd_icp( ptrdx, ptrdy, jpicpd_zdf, ctype )   
               CALL trd_icp( ztswu, ztswv, jpicpd_swf, ctype )                               ! wind stress forcing term
               ! bottom friction contribution now handled explicitly
            CASE ( jpdyn_trd_bfr )   ;   CALL trd_icp( ptrdx, ptrdy, jpicpd_bfr, ctype )     ! bottom friction term
            END SELECT
            !
         END IF
         !
      END IF

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! II. Vorticity trends
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      IF( lk_trdvor .AND. ctype == 'DYN' )   THEN
         !
         SELECT CASE ( ktrd ) 
         CASE ( jpdyn_trd_hpg )   ;   CALL trd_vor_zint( ptrdx, ptrdy, jpvor_prg )   ! Hydrostatique Pressure Gradient 
         CASE ( jpdyn_trd_keg )   ;   CALL trd_vor_zint( ptrdx, ptrdy, jpvor_keg )   ! KE Gradient 
         CASE ( jpdyn_trd_rvo )   ;   CALL trd_vor_zint( ptrdx, ptrdy, jpvor_rvo )   ! Relative Vorticity 
         CASE ( jpdyn_trd_pvo )   ;   CALL trd_vor_zint( ptrdx, ptrdy, jpvor_pvo )   ! Planetary Vorticity Term 
         CASE ( jpdyn_trd_ldf )   ;   CALL trd_vor_zint( ptrdx, ptrdy, jpvor_ldf )   ! Horizontal Diffusion 
         CASE ( jpdyn_trd_had )   ;   CALL ctl_warn('Vorticity for horizontal advection trend never checked')   
         CASE ( jpdyn_trd_zad )   ;   CALL trd_vor_zint( ptrdx, ptrdy, jpvor_zad )   ! Vertical Advection 
         CASE ( jpdyn_trd_spg )   ;   CALL trd_vor_zint( ptrdx, ptrdy, jpvor_spg )   ! Surface Pressure Grad. 
         CASE ( jpdyn_trd_dat )   ;   CALL trd_vor_zint( ptrdx, ptrdy, jpvor_bev )   ! Beta V  
         CASE ( jpdyn_trd_zdf )                                                      ! Vertical Diffusion 
            ! subtract surface forcing/bottom friction trends 
            ! from vertical diffusive momentum trends
            ztswu(:,:) = 0.e0   ;   ztswv(:,:) = 0.e0
            ztbfu(:,:) = 0.e0   ;   ztbfv(:,:) = 0.e0  
            DO jj = 2, jpjm1   
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  ! save the surface forcing momentum fluxes
                  ztswu(ji,jj) = utau(ji,jj) / ( fse3u(ji,jj,1)*rau0 )
                  ztswv(ji,jj) = vtau(ji,jj) / ( fse3v(ji,jj,1)*rau0 )
                  !
                  ptrdx(ji,jj,1     ) = ptrdx(ji,jj,1     ) - ztswu(ji,jj)
                  ptrdy(ji,jj,1     ) = ptrdy(ji,jj,1     ) - ztswv(ji,jj)
               END DO
            END DO
            !
            CALL trd_vor_zint( ptrdx, ptrdy, jpvor_zdf )   
            CALL trd_vor_zint( ztswu, ztswv, jpvor_swf )                               ! Wind stress forcing term
         CASE ( jpdyn_trd_bfr )
            CALL trd_vor_zint( ptrdx, ptrdy, jpvor_bfr )                               ! Bottom friction term
         END SELECT
         !
      ENDIF

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! III. Mixed layer trends for active tracers
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      IF( lk_trdmld .AND. ctype == 'TRA' )   THEN
         
         !-----------------------------------------------------------------------------------------------
         ! W.A.R.N.I.N.G :
         ! jptra_trd_ldf : called by traldf.F90
         !                 at this stage we store:
         !                  - the lateral geopotential diffusion (here, lateral = horizontal)
         !                  - and the iso-neutral diffusion if activated 
         ! jptra_trd_zdf : called by trazdf.F90
         !                 * in case of iso-neutral diffusion we store the vertical diffusion component in the 
         !                   lateral trend including the K_z contrib, which will be removed later (see trd_mld)
         !-----------------------------------------------------------------------------------------------

         SELECT CASE ( ktrd )
         CASE ( jptra_trd_xad )   ;   CALL trd_mld_zint( ptrdx, ptrdy, jpmld_xad, '3D' )   ! merid. advection
         CASE ( jptra_trd_yad )   ;   CALL trd_mld_zint( ptrdx, ptrdy, jpmld_yad, '3D' )   ! zonal  advection
         CASE ( jptra_trd_zad )   ;   CALL trd_mld_zint( ptrdx, ptrdy, jpmld_zad, '3D' )   ! vertical advection
         CASE ( jptra_trd_ldf )   ;   CALL trd_mld_zint( ptrdx, ptrdy, jpmld_ldf, '3D' )   ! lateral diffusive
         CASE ( jptra_trd_bbl )   ;   CALL trd_mld_zint( ptrdx, ptrdy, jpmld_bbl, '3D' )   ! bottom boundary layer
         CASE ( jptra_trd_zdf )
            IF( ln_traldf_iso )   THEN
               CALL trd_mld_zint( ptrdx, ptrdy, jpmld_ldf, '3D' )   ! vertical diffusion (K_z)
            ELSE
               CALL trd_mld_zint( ptrdx, ptrdy, jpmld_zdf, '3D' )   ! vertical diffusion (K_z)
            ENDIF
         CASE ( jptra_trd_dmp )   ;   CALL trd_mld_zint( ptrdx, ptrdy, jpmld_dmp, '3D' )   ! internal 3D restoring (tradmp)
         CASE ( jptra_trd_qsr )   ;   CALL trd_mld_zint( ptrdx, ptrdy, jpmld_for, '3D' )   ! air-sea : penetrative sol radiat
         CASE ( jptra_trd_nsr )
            ptrdx(:,:,2:jpk) = 0.e0   ;   ptrdy(:,:,2:jpk) = 0.e0
            CALL trd_mld_zint( ptrdx, ptrdy, jpmld_for, '2D' )                             ! air-sea : non penetr sol radiat
         CASE ( jptra_trd_bbc )   ;   CALL trd_mld_zint( ptrdx, ptrdy, jpmld_bbc, '3D' )   ! bottom bound cond (geoth flux)
         CASE ( jptra_trd_atf )   ;   CALL trd_mld_zint( ptrdx, ptrdy, jpmld_atf, '3D' )   ! asselin numerical
         CASE ( jptra_trd_npc )   ;   CALL trd_mld_zint( ptrdx, ptrdy, jpmld_npc, '3D' )   ! non penetr convect adjustment
         END SELECT

      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, ztswu, ztswv, ztbfu, ztbfv, z2dx, z2dy )
      !
   END SUBROUTINE trd_mod

#else
   !!----------------------------------------------------------------------
   !!   Default case :                                         Empty module
   !!----------------------------------------------------------------------
   USE trdmod_oce      ! ocean variables trends
   USE trdvor          ! ocean vorticity trends 
   USE trdicp          ! ocean bassin integral constraints properties
   USE trdmld          ! ocean active mixed layer tracers trends 
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trd_mod(ptrd3dx, ptrd3dy, ktrd , ctype, kt )   ! Empty routine
      REAL(wp) ::   ptrd3dx(:,:,:), ptrd3dy(:,:,:)
      INTEGER  ::   ktrd, kt                            
      CHARACTER(len=3) ::  ctype                  
      WRITE(*,*) 'trd_3d: You should not have seen this print! error ?', ptrd3dx(1,1,1), ptrd3dy(1,1,1)
      WRITE(*,*) ' "   ": You should not have seen this print! error ?', ktrd, ctype, kt
   END SUBROUTINE trd_mod
#endif

   SUBROUTINE trd_mod_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_mod_init  ***
      !! 
      !! ** Purpose :   Initialization of activated trends
      !!----------------------------------------------------------------------
      USE in_out_manager          ! I/O manager
      !!    
      NAMELIST/namtrd/ nn_trd, nn_ctls, cn_trdrst_in, cn_trdrst_out, ln_trdmld_restart, rn_ucf, ln_trdmld_instant
      !!----------------------------------------------------------------------

      IF( l_trdtra .OR. l_trddyn )   THEN
         REWIND( numnam )
         READ  ( numnam, namtrd )      ! namelist namtrd : trends diagnostic

         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) ' trd_mod_init : Momentum/Tracers trends'
            WRITE(numout,*) ' ~~~~~~~~~~~~~'
            WRITE(numout,*) '   Namelist namtrd : set trends parameters'
            WRITE(numout,*) '      frequency of trends diagnostics   nn_trd             = ', nn_trd
            WRITE(numout,*) '      control surface type              nn_ctls            = ', nn_ctls
            WRITE(numout,*) '      restart for ML diagnostics        ln_trdmld_restart  = ', ln_trdmld_restart
            WRITE(numout,*) '      instantaneous or mean ML T/S      ln_trdmld_instant  = ', ln_trdmld_instant
            WRITE(numout,*) '      unit conversion factor            rn_ucf             = ', rn_ucf
        ENDIF
      ENDIF
      !
      IF( lk_trddyn .OR. lk_trdtra )    CALL trd_icp_init       ! integral constraints trends
      IF( lk_trdmld                )    CALL trd_mld_init       ! mixed-layer trends (active  tracers)  
      IF( lk_trdvor                )    CALL trd_vor_init       ! vorticity trends        
      !
   END SUBROUTINE trd_mod_init

   !!======================================================================
END MODULE trdmod
