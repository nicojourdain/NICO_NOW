MODULE iceini
   !!======================================================================
   !!                       ***  MODULE iceini   ***
   !!   Sea-ice model : LIM Sea ice model Initialization
   !!======================================================================
   !! History :  3.0  ! 2008-03  (M. Vancoppenolle) LIM-3 original code
   !!            3.3  ! 2010-12  (G. Madec) add call to lim_thd_init and lim_thd_sal_init
   !!            4.0  ! 2011-02  (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3' :                                   LIM sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_init       : sea-ice model initialization
   !!----------------------------------------------------------------------
   USE phycst           ! physical constants
   USE dom_oce          ! ocean domain
   USE sbc_oce          ! Surface boundary condition: ocean fields
   USE sbc_ice          ! Surface boundary condition: ice   fields
   USE ice              ! LIM variables
   USE par_ice          ! LIM parameters
   USE dom_ice          ! LIM domain
   USE thd_ice          ! LIM thermodynamical variables
   USE limitd_me        ! LIM ice thickness distribution
   USE limmsh           ! LIM mesh
   USE limistate        ! LIM initial state
   USE limrst           ! LIM restart
   USE limthd           ! LIM ice thermodynamics
   USE limthd_sal       ! LIM ice thermodynamics: salinity
   USE limvar           ! LIM variables
   USE limsbc           ! LIM surface boundary condition
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_init   ! called by sbcice_lim.F90

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: iceini.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ice_init  ***
      !!
      !! ** purpose :   Allocate all the dynamic arrays of the LIM-3 modules
      !!----------------------------------------------------------------------
      INTEGER :: ierr
      !!----------------------------------------------------------------------

      !                                ! Allocate the ice arrays
      ierr =        ice_alloc        ()      ! ice variables
      ierr = ierr + dom_ice_alloc    ()      ! domain
      ierr = ierr + sbc_ice_alloc    ()      ! surface forcing
      ierr = ierr + thd_ice_alloc    ()      ! thermodynamics
      ierr = ierr + lim_itd_me_alloc ()      ! ice thickness distribution - mechanics
      !
      IF( lk_mpp    )   CALL mpp_sum( ierr )
      IF( ierr /= 0 )   CALL ctl_stop('STOP', 'ice_init : unable to allocate ice arrays')
      !
      !                                ! adequation jpk versus ice/snow layers/categories
      IF( jpl   > jpk  .OR.  jpm    > jpk .OR.                                    &
          jkmax > jpk  .OR.  nlay_s > jpk      )   CALL ctl_stop( 'STOP',         &
         &     'ice_init: the 3rd dimension of workspace arrays is too small.',   &
         &     'use more ocean levels or less ice/snow layers/categories.' )

      !                                ! Open the namelist file 
      CALL ctl_opn( numnam_ice, 'namelist_ice', 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp )
      !
      CALL ice_run                     ! set some ice run parameters
      !
      CALL lim_thd_init                ! set ice thermodynics parameters
      !
      CALL lim_thd_sal_init            ! set ice salinity parameters
      !
      rdt_ice = nn_fsbc * rdttra(1)    ! sea-ice timestep
      !
      CALL lim_msh                     ! ice mesh initialization
      !
      CALL lim_itd_ini                 ! ice thickness distribution initialization
      !
      CALL lim_sbc_init                ! ice surface boundary condition   


      !                                ! Initial sea-ice state
      IF( .NOT.ln_rstart ) THEN              ! start from rest
         numit = 0
         numit = nit000 - 1
         CALL lim_istate                        ! start from rest: sea-ice deduced from sst
         CALL lim_var_agg(1)                    ! aggregate category variables in bulk variables
         CALL lim_var_glo2eqv                   ! convert global variables in equivalent variables
      ELSE                                   ! start from a restart file
         CALL lim_rst_read                      ! read the restart file
         numit = nit000 - 1
         CALL lim_var_agg(1)                    ! aggregate ice variables
         CALL lim_var_glo2eqv                   ! convert global var in equivalent variables
      ENDIF
      !
      fr_i(:,:) = at_i(:,:)           ! initialisation of sea-ice fraction
      !
      nstart = numit  + nn_fsbc      
      nitrun = nitend - nit000 + 1 
      nlast  = numit  + nitrun 
      !
      IF( nstock == 0  )   nstock = nlast + 1
      !
   END SUBROUTINE ice_init


   SUBROUTINE ice_run
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE ice_run ***
      !!                 
      !! ** Purpose :   Definition some run parameter for ice model
      !!
      !! ** Method  :   Read the namicerun namelist and check the parameter 
      !!              values called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namicerun
      !!-------------------------------------------------------------------
      NAMELIST/namicerun/ cn_icerst_in, cn_icerst_out, ln_limdyn, acrit, hsndif, hicdif, cai, cao, ln_nicep
      !!-------------------------------------------------------------------
      !                    
      REWIND( numnam_ice )                ! Read Namelist namicerun 
      READ  ( numnam_ice , namicerun )
      !
      IF( lk_mpp .AND. ln_nicep ) THEN
         ln_nicep = .FALSE.
         CALL ctl_warn( 'ice_run : specific control print for LIM3 desactivated with MPI' )
      ENDIF       
      !
      IF(lwp) THEN                        ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_run : ice share parameters for dynamics/advection/thermo of sea-ice'
         WRITE(numout,*) ' ~~~~~~'
         WRITE(numout,*) '   switch for ice dynamics (1) or not (0)      ln_limdyn   = ', ln_limdyn
         WRITE(numout,*) '   minimum fraction for leads in the NH (SH)  acrit(1/2)   = ', acrit(:)
         WRITE(numout,*) '   computation of temp. in snow (=0) or not (=9999) hsndif = ', hsndif
         WRITE(numout,*) '   computation of temp. in ice  (=0) or not (=9999) hicdif = ', hicdif
         WRITE(numout,*) '   atmospheric drag over sea ice                           = ', cai
         WRITE(numout,*) '   atmospheric drag over ocean                             = ', cao
         WRITE(numout,*) '   Several ice points in the ice or not in ocean.output    = ', ln_nicep
      ENDIF
      !
   END SUBROUTINE ice_run


   SUBROUTINE lim_itd_ini
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_itd_ini ***
      !!
      !! ** Purpose :   Initializes the ice thickness distribution
      !! ** Method  :   ...
      !!------------------------------------------------------------------
      INTEGER  ::   jl, jm               ! dummy loop index
      REAL(wp) ::   zc1, zc2, zc3, zx1   ! local scalars
      !!------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'lim_itd_ini : Initialization of ice thickness distribution '
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'

      !------------------------------------------------------------------------------!
      ! 1) Ice thickness distribution parameters initialization    
      !------------------------------------------------------------------------------!

      !- Types boundaries (integer)
      !----------------------------
      ice_cat_bounds(1,1) = 1
      ice_cat_bounds(1,2) = jpl

      !- Number of ice thickness categories in each ice type
      DO jm = 1, jpm
         ice_ncat_types(jm) = ice_cat_bounds(jm,2) - ice_cat_bounds(jm,1) + 1 
      END DO

      !- Make the correspondence between thickness categories and ice types
      !---------------------------------------------------------------------
      DO jm = 1, jpm       !over types
         DO jl = ice_cat_bounds(jm,1), ice_cat_bounds(jm,2) !over thickness categories
            ice_types(jl) = jm
         END DO
      END DO

      IF(lwp) THEN  
         WRITE(numout,*) ' Number of ice types jpm =      ', jpm
         WRITE(numout,*) ' Number of ice categories jpl = ', jpl
         DO jm = 1, jpm
            WRITE(numout,*) ' Ice type ', jm
            WRITE(numout,*) ' Number of thickness categories ', ice_ncat_types(jm)
            WRITE(numout,*) ' Thickness category boundaries  ', ice_cat_bounds(jm,1:2)
         END DO
         WRITE(numout,*) 'Ice type vector', ice_types(1:jpl)
         WRITE(numout,*)
      ENDIF

      !- Thickness categories boundaries 
      !----------------------------------
      hi_max(:) = 0._wp
      hi_max_typ(:,:) = 0._wp

      !- Type 1 - undeformed ice
      zc1 =  3._wp / REAL( ice_cat_bounds(1,2) - ice_cat_bounds(1,1) + 1 , wp )
      zc2 = 10._wp * zc1
      zc3 =  3._wp

      DO jl = ice_cat_bounds(1,1), ice_cat_bounds(1,2)
         zx1 = REAL( jl-1 , wp ) / REAL( ice_cat_bounds(1,2) - ice_cat_bounds(1,1) + 1 , wp )
         hi_max(jl) = hi_max(jl-1) + zc1 + zc2 * (1._wp + TANH( zc3 * (zx1 - 1._wp ) ) )
      END DO

      !- Fill in the hi_max_typ vector, useful in other circumstances
      ! Tricky trick: hi_max_typ is actually not used in the code and will be removed in a
      ! next flyspray at this time, the tricky trick will also be removed (Martin, march 08)
      DO jl = ice_cat_bounds(1,1), ice_cat_bounds(1,2)
         hi_max_typ(jl,1) = hi_max(jl)
      END DO

      IF(lwp) WRITE(numout,*) ' Thickness category boundaries independently of ice type '
      IF(lwp) WRITE(numout,*) ' hi_max ', hi_max(0:jpl)

      IF(lwp) WRITE(numout,*) ' Thickness category boundaries inside ice types '
      IF(lwp) THEN 
         DO jm = 1, jpm
            WRITE(numout,*) ' Type number ', jm
            WRITE(numout,*) ' hi_max_typ : ', hi_max_typ(0:ice_ncat_types(jm),jm)
         END DO
      ENDIF
      !
      DO jl = 1, jpl
         hi_mean(jl) = ( hi_max(jl) + hi_max(jl-1) ) * 0.5_wp
      END DO
      !
      tn_ice(:,:,:) = t_su(:,:,:)
      !
   END SUBROUTINE lim_itd_ini

#else
   !!----------------------------------------------------------------------
   !!   Default option :        Empty module           NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE ice_init        ! Empty routine
   END SUBROUTINE ice_init
#endif

   !!======================================================================
END MODULE iceini
