MODULE iceini_2
   !!======================================================================
   !!                       ***  MODULE iceini   ***
   !!   Sea-ice model : LIM 2.0 Sea ice model Initialization
   !!======================================================================
   !! History :  1.0  ! 2002-08  (G. Madec)  F90: Free form and modules
   !!            2.0  ! 2003-08  (C. Ethe)  add ice_run
   !!            3.3  ! 2009-05  (G. Garric, C. Bricaud) addition of the lim2_evp case
   !!            4.0  ! 2011-02  (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim2
   !!----------------------------------------------------------------------
   !!   'key_lim2' :                                  LIM 2.0 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_init_2       : sea-ice model initialization
   !!   ice_run_2        : Definition some run parameter for ice model
   !!----------------------------------------------------------------------
   USE phycst           ! physical constants
   USE dom_oce          ! ocean domain
   USE sbc_oce          ! surface boundary condition: ocean
   USE sbc_ice          ! LIM2 surface boundary condition
   USE dom_ice_2        ! LIM2 ice domain
   USE par_ice_2        ! LIM2 parameters
   USE thd_ice_2        ! LIM2 thermodynamical variables
   USE ice_2            ! LIM2 ice variable
   USE limmsh_2         ! LIM2 mesh
   USE limistate_2      ! LIM2 initial state
   USE limrst_2         ! LIM2 restart
   USE limsbc_2         ! LIM2 surface boundary condition
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_init_2               ! called by sbcice_lim_2.F90

   !!----------------------------------------------------------------------
   !! NEMO/LIM2 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: iceini_2.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_init_2
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ice_init_2  ***
      !!
      !! ** purpose :   initialisation of LIM-2 domain and variables  
      !!----------------------------------------------------------------------
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'ice_init_2 : LIM-2 sea-ice - initialization'
         WRITE(numout,*) '~~~~~~~~~~~   '
      ENDIF
      !                                ! Allocate the ice arrays
      ierr =        ice_alloc_2    ()       ! ice variables
      ierr = ierr + dom_ice_alloc_2()       ! domain
      ierr = ierr + sbc_ice_alloc  ()       ! surface forcing
      ierr = ierr + thd_ice_alloc_2()       ! thermodynamics

      IF( lk_mpp    )   CALL mpp_sum( ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'ice_init_2 : unable to allocate ice arrays' )

      !                                ! adequation jpk versus ice/snow layers
      IF( jpl > jpk  .OR.  jplayersp1 > jpk  )   CALL ctl_stop( 'STOP',           &
         &     'ice_init: the 3rd dimension of workspace arrays is too small.',   &
         &     'use more ocean levels or less ice layers/categories.' )

      !                                ! Open the namelist file 
      CALL ctl_opn( numnam_ice, 'namelist_ice', 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp ) 
      !    
      CALL ice_run_2                   ! read in namelist some run parameters
      !          
      rdt_ice = nn_fsbc * rdttra(1)    ! sea-ice time step
      numit   = nit000 - 1
      !
      CALL lim_msh_2                   ! ice mesh initialization
      !
      !                                ! Initial sea-ice state
      IF( .NOT.ln_rstart ) THEN   ;   CALL lim_istate_2     ! start from rest: sea-ice deduced from sst
      ELSE                        ;   CALL lim_rst_read_2   ! start from a restart file
      ENDIF
      !
      tn_ice(:,:,1) = sist(:,:)        ! ice temperature  known by the ocean
      fr_i  (:,:)   = 1.0 - frld(:,:)  ! sea-ice fraction known by the ocean
      !
      CALL lim_sbc_init_2              ! ice surface boundary condition   
      !
      IF( lk_lim2_vp )   THEN   ;   IF(lwp) WRITE(numout,*) '                VP  rheology - B-grid case'
      ELSE                      ;   IF(lwp) WRITE(numout,*) '                EVP rheology - C-grid case'
      ENDIF
      !
   END SUBROUTINE ice_init_2


   SUBROUTINE ice_run_2
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE ice_run_2 ***
      !!                 
      !! ** Purpose :   Definition some run parameter for ice model
      !!
      !! ** Method  :   Read the namicerun namelist and check the parameter 
      !!       values called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namicerun
      !!-------------------------------------------------------------------
      NAMELIST/namicerun/ cn_icerst_in, cn_icerst_out, ln_limdyn, ln_limdmp, acrit, hsndif, hicdif
      !!-------------------------------------------------------------------
      !                    
      REWIND( numnam_ice )                      ! Read Namelist namicerun 
      READ  ( numnam_ice , namicerun )
      !
      IF(lwp) THEN                              ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_run : ice share parameters for dynamics/advection/thermo of sea-ice'
         WRITE(numout,*) ' ~~~~~~'
         WRITE(numout,*) '   switch for ice dynamics (1) or not (0)      ln_limdyn   = ', ln_limdyn
         WRITE(numout,*) '   Ice damping                                 ln_limdmp   = ', ln_limdmp
         WRITE(numout,*) '   minimum fraction for leads in the NH (SH)  acrit(1/2)   = ', acrit(:)
         WRITE(numout,*) '   computation of temp. in snow (=0) or not (=9999) hsndif = ', hsndif
         WRITE(numout,*) '   computation of temp. in ice  (=0) or not (=9999) hicdif = ', hicdif
      ENDIF
      !
   END SUBROUTINE ice_run_2

#else
   !!----------------------------------------------------------------------
   !!   Default option :        Empty module       NO LIM 2.0 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE ice_init_2      ! Empty routine
   END SUBROUTINE ice_init_2
#endif

   !!======================================================================
END MODULE iceini_2
