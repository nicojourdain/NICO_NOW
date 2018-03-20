MODULE sbcmod
   !!======================================================================
   !!                       ***  MODULE  sbcmod  ***
   !! Surface module :  provide to the ocean its surface boundary condition
   !!======================================================================
   !! History :  3.0  ! 2006-07  (G. Madec)  Original code
   !!            3.1  ! 2008-08  (S. Masson, A. Caubel, E. Maisonnave, G. Madec) coupled interface
   !!            3.3  ! 2010-04  (M. Leclair, G. Madec)  Forcing averaged over 2 time steps
   !!            3.3  ! 2010-10  (S. Masson)  add diurnal cycle
   !!            3.3  ! 2010-09  (D. Storkey) add ice boundary conditions (BDY)
   !!             -   ! 2010-11  (G. Madec) ice-ocean stress always computed at each ocean time-step
   !!             -   ! 2010-10  (J. Chanut, C. Bricaud, G. Madec)  add the surface pressure forcing
   !!            3.4  ! 2011-11  (C. Harris) CICE added as an option
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_init       : read namsbc namelist
   !!   sbc            : surface ocean momentum, heat and freshwater boundary conditions
   !!----------------------------------------------------------------------
   USE oce              ! ocean dynamics and tracers
   USE dom_oce          ! ocean space and time domain
   USE phycst           ! physical constants
   USE sbc_oce          ! Surface boundary condition: ocean fields
   USE sbc_ice          ! Surface boundary condition: ice fields
   USE sbcdcy           ! surface boundary condition: diurnal cycle
   USE sbcssm           ! surface boundary condition: sea-surface mean variables
   USE sbcapr           ! surface boundary condition: atmospheric pressure
   USE sbcana           ! surface boundary condition: analytical formulation
   USE sbcflx           ! surface boundary condition: flux formulation
   USE sbcblk_clio      ! surface boundary condition: bulk formulation : CLIO
   USE sbcblk_core      ! surface boundary condition: bulk formulation : CORE
   USE sbcblk_mfs       ! surface boundary condition: bulk formulation : MFS
   USE sbcice_if        ! surface boundary condition: ice-if sea-ice model
   USE sbcice_lim       ! surface boundary condition: LIM 3.0 sea-ice model
   USE sbcice_lim_2     ! surface boundary condition: LIM 2.0 sea-ice model
   USE sbcice_cice      ! surface boundary condition: CICE    sea-ice model
   USE sbccpl           ! surface boundary condition: coupled florulation
   USE cpl_oasis3, ONLY:lk_cpl      ! are we in coupled mode?
   USE sbcssr           ! surface boundary condition: sea surface restoring
   USE sbcrnf           ! surface boundary condition: runoffs
   USE sbcfwb           ! surface boundary condition: freshwater budget
   USE sbctau           ! surface boundary condition: read and overwite utau and vtau
   USE closea           ! closed sea
   USE bdy_par          ! for lk_bdy
   USE bdyice_lim2      ! unstructured open boundary data  (bdy_ice_lim_2 routine)

   USE prtctl           ! Print control                    (prt_ctl routine)
   USE iom              ! IOM library
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library
   USE timing           ! Timing
   USE sbcwave          ! Wave module

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc        ! routine called by step.F90
   PUBLIC   sbc_init   ! routine called by opa.F90
   
   INTEGER ::   nsbc   ! type of surface boundary condition (deduced from namsbc informations)
      
   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO-consortium (2011) 
   !! $Id: sbcmod.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_init
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE sbc_init ***
      !!
      !! ** Purpose :   Initialisation of the ocean surface boundary computation
      !!
      !! ** Method  :   Read the namsbc namelist and set derived parameters
      !!
      !! ** Action  : - read namsbc parameters
      !!              - nsbc: type of sbc
      !!----------------------------------------------------------------------
      INTEGER ::   icpt   ! local integer
      !!
      NAMELIST/namsbc/ nn_fsbc   , ln_ana , ln_flx  , ln_blk_clio, ln_blk_core, ln_cpl, ln_tau,   &
         &             ln_blk_mfs, ln_apr_dyn, nn_ice , ln_dm2dc, ln_rnf, ln_ssr     , nn_fwb, ln_cdgw
      !!----------------------------------------------------------------------

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'sbc_init : surface boundary condition setting'
         WRITE(numout,*) '~~~~~~~~ '
      ENDIF

      REWIND( numnam )           ! Read Namelist namsbc
      READ  ( numnam, namsbc )

      !                          ! overwrite namelist parameter using CPP key information
      IF( Agrif_Root() ) THEN                ! AGRIF zoom
        IF( lk_lim2 )   nn_ice      = 2
        IF( lk_lim3 )   nn_ice      = 3
        IF( lk_cice )   nn_ice      = 4
      ENDIF
      IF( cp_cfg == 'gyre' ) THEN            ! GYRE configuration
          ln_ana      = .TRUE.   
          nn_ice      =   0
      ENDIF
      
      IF(lwp) THEN               ! Control print
         WRITE(numout,*) '        Namelist namsbc (partly overwritten with CPP key setting)'
         WRITE(numout,*) '           frequency update of sbc (and ice)             nn_fsbc     = ', nn_fsbc
         WRITE(numout,*) '           Type of sbc : '
         WRITE(numout,*) '              analytical formulation                     ln_ana      = ', ln_ana
         WRITE(numout,*) '              flux       formulation                     ln_flx      = ', ln_flx
         WRITE(numout,*) '              CLIO bulk  formulation                     ln_blk_clio = ', ln_blk_clio
         WRITE(numout,*) '              CORE bulk  formulation                     ln_blk_core = ', ln_blk_core
         WRITE(numout,*) '              MFS  bulk  formulation                     ln_blk_mfs  = ', ln_blk_mfs
         WRITE(numout,*) '              coupled    formulation (T if key_sbc_cpl)  ln_cpl      = ', ln_cpl
         WRITE(numout,*) '           Misc. options of sbc : '
         WRITE(numout,*) '              Patm gradient added in ocean & ice Eqs.    ln_apr_dyn  = ', ln_apr_dyn
         WRITE(numout,*) '              ice management in the sbc (=0/1/2/3)       nn_ice      = ', nn_ice 
         WRITE(numout,*) '              daily mean to diurnal cycle qsr            ln_dm2dc    = ', ln_dm2dc 
         WRITE(numout,*) '              runoff / runoff mouths                     ln_rnf      = ', ln_rnf
         WRITE(numout,*) '              Sea Surface Restoring on SST and/or SSS    ln_ssr      = ', ln_ssr
         WRITE(numout,*) '              FreshWater Budget control  (=0/1/2)        nn_fwb      = ', nn_fwb
         WRITE(numout,*) '              closed sea (=0/1) (set in namdom)          nn_closea   = ', nn_closea
         WRITE(numout,*) '              overwrite utau and vtau                    ln_tau      = ', ln_tau
      ENDIF

      !                              ! allocate sbc arrays
      IF( sbc_oce_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'sbc_init : unable to allocate sbc_oce arrays' )

      !                          ! Checks:
      IF( .NOT. ln_rnf ) THEN                      ! no specific treatment in vicinity of river mouths 
         ln_rnf_mouth  = .false.                      
         IF( sbc_rnf_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'sbc_init : unable to allocate sbc_rnf arrays' )
         nkrnf         = 0
         rnf     (:,:) = 0.e0
         rnfmsk  (:,:) = 0.e0
         rnfmsk_z(:)   = 0.e0
      ENDIF
      IF( nn_ice == 0  )   fr_i(:,:) = 0.e0        ! no ice in the domain, ice fraction is always zero

      !                                            ! restartability   
      IF( MOD( nitend - nit000 + 1, nn_fsbc) /= 0 .OR.   &
          MOD( nstock             , nn_fsbc) /= 0 ) THEN 
         WRITE(ctmp1,*) 'experiment length (', nitend - nit000 + 1, ') or nstock (', nstock,   &
            &           ' is NOT a multiple of nn_fsbc (', nn_fsbc, ')'
         CALL ctl_stop( ctmp1, 'Impossible to properly do model restart' )
      ENDIF
      !
      IF( MOD( rday, REAL(nn_fsbc, wp) * rdt ) /= 0 )   &
         &  CALL ctl_warn( 'nn_fsbc is NOT a multiple of the number of time steps in a day' )
      !
      IF( ( nn_ice == 2 .OR. nn_ice ==3 ) .AND. .NOT.( ln_blk_clio .OR. ln_blk_core .OR. lk_cpl ) )   &
         &   CALL ctl_stop( 'LIM sea-ice model requires a bulk formulation or coupled configuration' )
      IF( nn_ice == 4 .AND. .NOT.( ln_blk_core .OR. lk_cpl ) )   &
         &   CALL ctl_stop( 'CICE sea-ice model requires ln_blk_core or lk_cpl' )
      IF( nn_ice == 4 .AND. ( .NOT. ( cp_cfg == 'orca' ) .OR. lk_agrif ) )   &
         &   CALL ctl_stop( 'CICE sea-ice model currently only available in a global ORCA configuration without AGRIF' )
      
      IF( ln_dm2dc )   nday_qsr = -1   ! initialisation flag

      IF( ln_dm2dc .AND. .NOT.( ln_flx .OR. ln_blk_core ) )   &
         &   CALL ctl_stop( 'diurnal cycle into qsr field from daily values requires a flux or core-bulk formulation' )
      
      IF( ln_dm2dc .AND. ( ( NINT(rday) / ( nn_fsbc * NINT(rdt) ) )  < 8 ) )   &
         &   CALL ctl_warn( 'diurnal cycle for qsr: the sampling of the diurnal cycle is too small...' )

       !drag coefficient read from wave model definable only with mfs bulk formulae and core 
       IF(ln_cdgw .AND. .NOT.(ln_blk_mfs .OR. ln_blk_core) )              &
          &   CALL ctl_stop( 'drag coefficient read from wave model definable only with mfs bulk formulae and core')
      
      !                          ! Choice of the Surface Boudary Condition (set nsbc)
      icpt = 0
      IF( ln_ana          ) THEN   ;   nsbc =  1   ; icpt = icpt + 1   ;   ENDIF       ! analytical      formulation
      IF( ln_flx          ) THEN   ;   nsbc =  2   ; icpt = icpt + 1   ;   ENDIF       ! flux            formulation
      IF( ln_blk_clio     ) THEN   ;   nsbc =  3   ; icpt = icpt + 1   ;   ENDIF       ! CLIO bulk       formulation
      IF( ln_blk_core     ) THEN   ;   nsbc =  4   ; icpt = icpt + 1   ;   ENDIF       ! CORE bulk       formulation
      IF( ln_blk_mfs      ) THEN   ;   nsbc =  6   ; icpt = icpt + 1   ;   ENDIF       ! MFS  bulk       formulation
      IF( ln_cpl          ) THEN   ;   nsbc =  5   ; icpt = icpt + 1   ;   ENDIF       ! Coupled         formulation
      IF( cp_cfg == 'gyre') THEN   ;   nsbc =  0                       ;   ENDIF       ! GYRE analytical formulation
      IF( lk_esopa        )            nsbc = -1                                       ! esopa test, ALL formulations
      !
      IF( icpt /= 1 .AND. .NOT.lk_esopa ) THEN
         WRITE(numout,*)
         WRITE(numout,*) '           E R R O R in setting the sbc, one and only one namelist/CPP key option '
         WRITE(numout,*) '                     must be choosen. You choose ', icpt, ' option(s)'
         WRITE(numout,*) '                     We stop'
         nstop = nstop + 1
      ENDIF
      IF(lwp) THEN
         WRITE(numout,*)
         IF( nsbc == -1 )   WRITE(numout,*) '              ESOPA test All surface boundary conditions'
         IF( nsbc ==  0 )   WRITE(numout,*) '              GYRE analytical formulation'
         IF( nsbc ==  1 )   WRITE(numout,*) '              analytical formulation'
         IF( nsbc ==  2 )   WRITE(numout,*) '              flux formulation'
         IF( nsbc ==  3 )   WRITE(numout,*) '              CLIO bulk formulation'
         IF( nsbc ==  4 )   WRITE(numout,*) '              CORE bulk formulation'
         IF( nsbc ==  5 )   WRITE(numout,*) '              coupled formulation'
         IF( nsbc ==  6 )   WRITE(numout,*) '              MFS Bulk formulation'
      ENDIF

      IF( nn_ice == 4 )   CALL cice_sbc_init (nsbc)

#if defined key_oasis_mct
      IF( nsbc == 5 )   CALL sbc_cpl_init (nn_ice)
#endif
      !
   END SUBROUTINE sbc_init


   SUBROUTINE sbc( kt )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE sbc  ***
      !!              
      !! ** Purpose :   provide at each time-step the ocean surface boundary
      !!                condition (momentum, heat and freshwater fluxes)
      !!
      !! ** Method  :   blah blah  to be written ????????? 
      !!                CAUTION : never mask the surface stress field (tke sbc)
      !!
      !! ** Action  : - set the ocean surface boundary condition at before and now 
      !!                time step, i.e.  
      !!                utau_b, vtau_b, qns_b, qsr_b, emp_n, emps_b, qrp_b, erp_b
      !!                utau  , vtau  , qns  , qsr  , emp  , emps  , qrp  , erp
      !!              - updte the ice fraction : fr_i
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt       ! ocean time step
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('sbc')
      !
      !                                            ! ---------------------------------------- !
      IF( kt /= nit000 ) THEN                      !          Swap of forcing fields          !
         !                                         ! ---------------------------------------- !
         utau_b(:,:) = utau(:,:)                         ! Swap the ocean forcing fields
         vtau_b(:,:) = vtau(:,:)                         ! (except at nit000 where before fields
         qns_b (:,:) = qns (:,:)                         !  are set at the end of the routine)
         ! The 3D heat content due to qsr forcing is treated in traqsr
         ! qsr_b (:,:) = qsr (:,:)
         emp_b (:,:) = emp (:,:)
         emps_b(:,:) = emps(:,:)
      ENDIF
      !                                            ! ---------------------------------------- !
      !                                            !        forcing field computation         !
      !                                            ! ---------------------------------------- !

!     CALL iom_setkt( kt + nn_fsbc - 1 )                 ! in sbc, iom_put is called every nn_fsbc time step
      !
      IF( ln_apr_dyn ) CALL sbc_apr( kt )                ! atmospheric pressure provided at kt+0.5*nn_fsbc
                                                         ! (caution called before sbc_ssm)
      !
      CALL sbc_ssm( kt )                                 ! ocean sea surface variables (sst_m, sss_m, ssu_m, ssv_m)
      !                                                  ! averaged over nf_sbc time-step

      IF (ln_cdgw) CALL sbc_wave( kt )
                                                   !==  sbc formulation  ==!
                                                            
      SELECT CASE( nsbc )                                ! Compute ocean surface boundary condition
      !                                                  ! (i.e. utau,vtau, qns, qsr, emp, emps)
      CASE(  0 )   ;   CALL sbc_gyre    ( kt )                    ! analytical formulation : GYRE configuration
      CASE(  1 )   ;   CALL sbc_ana     ( kt )                    ! analytical formulation : uniform sbc
      CASE(  2 )   ;   CALL sbc_flx     ( kt )                    ! flux formulation
      CASE(  3 )   ;   CALL sbc_blk_clio( kt )                    ! bulk formulation : CLIO for the ocean
      CASE(  4 )   ;   CALL sbc_blk_core( kt )                    ! bulk formulation : CORE for the ocean
      CASE(  5 )   ;   CALL sbc_cpl_rcv ( kt, nn_fsbc, nn_ice )   ! coupled formulation
      CASE(  6 )   ;   CALL sbc_blk_mfs ( kt )                    ! bulk formulation : MFS for the ocean
      CASE( -1 )                                
                       CALL sbc_ana     ( kt )                    ! ESOPA, test ALL the formulations
                       CALL sbc_gyre    ( kt )                    !
                       CALL sbc_flx     ( kt )                    !
                       CALL sbc_blk_clio( kt )                    !
                       CALL sbc_blk_core( kt )                    !
                       CALL sbc_cpl_rcv ( kt, nn_fsbc, nn_ice )   !
      END SELECT

      !                                            !==  Misc. Options  ==!
      
      SELECT CASE( nn_ice )                                     ! Update heat and freshwater fluxes over sea-ice areas
      CASE(  1 )   ;       CALL sbc_ice_if   ( kt )                  ! Ice-cover climatology ("Ice-if" model)
         !                                                      
      CASE(  2 )   ;       CALL sbc_ice_lim_2( kt, nsbc )            ! LIM-2 ice model
         IF( lk_bdy )      CALL bdy_ice_lim_2( kt )                  ! BDY boundary condition
         !                                                     
      CASE(  3 )   ;       CALL sbc_ice_lim  ( kt, nsbc )            ! LIM-3 ice model
         !
      CASE(  4 )   ;       CALL sbc_ice_cice ( kt, nsbc )            ! CICE ice model
      END SELECT                                              

      IF( ln_rnf       )   CALL sbc_rnf( kt )                   ! add runoffs to fresh water fluxes
 
      IF( ln_ssr       )   CALL sbc_ssr( kt )                   ! add SST/SSS damping term

      IF( nn_fwb  /= 0 )   CALL sbc_fwb( kt, nn_fwb, nn_fsbc )  ! control the freshwater budget

      IF( nclosea == 1 )   CALL sbc_clo( kt )                   ! treatment of closed sea in the model domain 
      !                                                         ! (update freshwater fluxes)
      IF( ln_tau       )   CALL sbc_tau( kt )                   ! overwrite utau and vtau

!RBbug do not understand why see ticket 667
      CALL lbc_lnk( emp, 'T', 1. )
      !
      IF( kt == nit000 ) THEN                          !   set the forcing field at nit000 - 1    !
         !                                             ! ---------------------------------------- !
         IF( ln_rstart .AND.    &                               !* Restart: read in restart file
            & iom_varid( numror, 'utau_b', ldstop = .FALSE. ) > 0 ) THEN 
            IF(lwp) WRITE(numout,*) '          nit000-1 surface forcing fields red in the restart file'
            CALL iom_get( numror, jpdom_autoglo, 'utau_b', utau_b )   ! before i-stress  (U-point)
            CALL iom_get( numror, jpdom_autoglo, 'vtau_b', vtau_b )   ! before j-stress  (V-point)
            CALL iom_get( numror, jpdom_autoglo, 'qns_b' , qns_b  )   ! before non solar heat flux (T-point)
            ! The 3D heat content due to qsr forcing is treated in traqsr
            ! CALL iom_get( numror, jpdom_autoglo, 'qsr_b' , qsr_b  )   ! before     solar heat flux (T-point)
            CALL iom_get( numror, jpdom_autoglo, 'emp_b' , emp_b  )   ! before     freshwater flux (T-point)
            CALL iom_get( numror, jpdom_autoglo, 'emps_b', emps_b )   ! before C/D freshwater flux (T-point)
         ELSE                                                   !* no restart: set from nit000 values
            IF(lwp) WRITE(numout,*) '          nit000-1 surface forcing fields set to nit000'
            utau_b(:,:) = utau(:,:) 
            vtau_b(:,:) = vtau(:,:)
            qns_b (:,:) = qns (:,:)
            ! qsr_b (:,:) = qsr (:,:)
            emp_b (:,:) = emp (:,:)
            emps_b(:,:) = emps(:,:)
         ENDIF
      ENDIF
      !                                                ! ---------------------------------------- !
      IF( lrst_oce ) THEN                              !      Write in the ocean restart file     !
         !                                             ! ---------------------------------------- !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'sbc : ocean surface forcing fields written in ocean restart file ',   &
            &                    'at it= ', kt,' date= ', ndastp
         IF(lwp) WRITE(numout,*) '~~~~'
         CALL iom_rstput( kt, nitrst, numrow, 'utau_b' , utau )
         CALL iom_rstput( kt, nitrst, numrow, 'vtau_b' , vtau )
         CALL iom_rstput( kt, nitrst, numrow, 'qns_b'  , qns  )
         ! The 3D heat content due to qsr forcing is treated in traqsr
         ! CALL iom_rstput( kt, nitrst, numrow, 'qsr_b'  , qsr  )
         CALL iom_rstput( kt, nitrst, numrow, 'emp_b'  , emp  )
         CALL iom_rstput( kt, nitrst, numrow, 'emps_b' , emps )
      ENDIF

      !                                                ! ---------------------------------------- !
      !                                                !        Outputs and control print         !
      !                                                ! ---------------------------------------- !
      IF( MOD( kt-1, nn_fsbc ) == 0 ) THEN
         CALL iom_put( "empmr" , emp  - rnf )                   ! upward water flux
         CALL iom_put( "empsmr", emps - rnf )                   ! c/d water flux
         CALL iom_put( "qt"    , qns  + qsr )                   ! total heat flux 
         CALL iom_put( "qns"   , qns        )                   ! solar heat flux
         CALL iom_put( "qsr"   ,       qsr  )                   ! solar heat flux
         IF( nn_ice > 0 )   CALL iom_put( "ice_cover", fr_i )   ! ice fraction 
      ENDIF
      !
!     CALL iom_setkt( kt )           ! iom_put outside of sbc is called at every time step
      !
      CALL iom_put( "utau", utau )   ! i-wind stress   (stress can be updated at 
      CALL iom_put( "vtau", vtau )   ! j-wind stress    each time step in sea-ice)
      CALL iom_put( "taum", taum )   ! wind stress module 
      CALL iom_put( "wspd", wndm )   ! wind speed  module 
      !
      IF(ln_ctl) THEN         ! print mean trends (used for debugging)
         CALL prt_ctl(tab2d_1=fr_i             , clinfo1=' fr_i     - : ', mask1=tmask, ovlap=1 )
         CALL prt_ctl(tab2d_1=(emp-rnf)        , clinfo1=' emp-rnf  - : ', mask1=tmask, ovlap=1 )
         CALL prt_ctl(tab2d_1=(emps-rnf)       , clinfo1=' emps-rnf - : ', mask1=tmask, ovlap=1 )
         CALL prt_ctl(tab2d_1=qns              , clinfo1=' qns      - : ', mask1=tmask, ovlap=1 )
         CALL prt_ctl(tab2d_1=qsr              , clinfo1=' qsr      - : ', mask1=tmask, ovlap=1 )
         CALL prt_ctl(tab3d_1=tmask            , clinfo1=' tmask    - : ', mask1=tmask, ovlap=1, kdim=jpk )
         CALL prt_ctl(tab3d_1=tsn(:,:,:,jp_tem), clinfo1=' sst      - : ', mask1=tmask, ovlap=1, kdim=1   )
         CALL prt_ctl(tab3d_1=tsn(:,:,:,jp_sal), clinfo1=' sss      - : ', mask1=tmask, ovlap=1, kdim=1   )
         CALL prt_ctl(tab2d_1=utau             , clinfo1=' utau     - : ', mask1=umask,                      &
            &         tab2d_2=vtau             , clinfo2=' vtau     - : ', mask2=vmask, ovlap=1 )
      ENDIF

      IF( kt == nitend )   CALL sbc_final         ! Close down surface module if necessary
      !
      IF( nn_timing == 1 )  CALL timing_stop('sbc')
      !
   END SUBROUTINE sbc

   SUBROUTINE sbc_final
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE sbc_final  ***
      !!---------------------------------------------------------------------

      !-----------------------------------------------------------------
      ! Finalize CICE (if used)
      !-----------------------------------------------------------------

      IF( nn_ice == 4 )   CALL cice_sbc_final
      !
   END SUBROUTINE sbc_final

   !!======================================================================
END MODULE sbcmod
