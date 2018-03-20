MODULE floats
   !!======================================================================
   !!                       ***  MODULE  floats  ***
   !! Ocean floats : floats
   !!======================================================================
   !! History :  OPA  !          (CLIPPER)   original Code
   !!   NEMO     1.0  ! 2002-06  (A. Bozec)  F90, Free form and module
   !!----------------------------------------------------------------------
#if   defined key_floats   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_floats'                                     float trajectories
   !!----------------------------------------------------------------------
   !!   flo_stp   : float trajectories computation
   !!   flo_init  : initialization of float trajectories computation
   !!----------------------------------------------------------------------
   USE oce             ! ocean variables
   USE flo_oce         ! floats variables
   USE lib_mpp         ! distributed memory computing
   USE flodom          ! initialisation Module 
   USE flowri          ! float output                     (flo_wri routine)
   USE florst          ! float restart                    (flo_rst routine)
   USE flo4rk          ! Trajectories, Runge Kutta scheme (flo_4rk routine)
   USE floblk          ! Trajectories, Blanke scheme      (flo_blk routine)
   USE in_out_manager  ! I/O manager
   USE timing          ! preformance summary

   IMPLICIT NONE
   PRIVATE  

   PUBLIC   flo_stp    ! routine called by step.F90
   PUBLIC   flo_init   ! routine called by opa.F90

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: floats.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE flo_stp( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE flo_stp  ***
      !!                    
      !! ** Purpose :   Compute the geographical position (lat., long., depth)
      !!      of each float at each time step with one of the algorithm.
      !! 
      !! ** Method  :   The position of a float is computed with Bruno Blanke 
      !!        algorithm by default and with a 4th order Runge-Kutta scheme
      !!        if ln_flork4 =T
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in  ) ::   kt   ! ocean time step
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )   CALL timing_start('flo_stp')
      !
      IF( ln_flork4 ) THEN   ;   CALL flo_4rk( kt )        ! Trajectories using a 4th order Runge Kutta scheme
      ELSE                   ;   CALL flo_blk( kt )        ! Trajectories using Blanke' algorithme
      ENDIF
      !
      IF( lk_mpp )   CALL mppsync   ! synchronization of all the processor
      !
      CALL flo_wri( kt )      ! trajectories ouput 
      !
      CALL flo_rst( kt )      ! trajectories restart
      !
      wb(:,:,:) = wn(:,:,:)         ! Save the old vertical velocity field
      !
      IF( nn_timing == 1 )   CALL timing_stop('flo_stp')
      !
   END SUBROUTINE flo_stp


   SUBROUTINE flo_init
      !!----------------------------------------------------------------
      !!                 ***  ROUTINE flo_init  ***
      !!                   
      !! ** Purpose :   Read the namelist of floats
      !!----------------------------------------------------------------------
      INTEGER :: jfl
      !
      NAMELIST/namflo/ jpnfl, jpnnewflo, ln_rstflo, nn_writefl, nn_stockfl, ln_argo, ln_flork4, ln_ariane, ln_flo_ascii
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )   CALL timing_start('flo_init')
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'flo_stp : call floats routine '
      IF(lwp) WRITE(numout,*) '~~~~~~~'

      REWIND( numnam )              ! Namelist namflo : floats
      READ  ( numnam, namflo )
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) '         Namelist floats :'
         WRITE(numout,*) '            number of floats                      jpnfl        = ', jpnfl
         WRITE(numout,*) '            number of new floats                  jpnflnewflo  = ', jpnnewflo
         WRITE(numout,*) '            restart                               ln_rstflo    = ', ln_rstflo
         WRITE(numout,*) '            frequency of float output file        nn_writefl   = ', nn_writefl
         WRITE(numout,*) '            frequency of float restart file       nn_stockfl   = ', nn_stockfl
         WRITE(numout,*) '            Argo type floats                      ln_argo      = ', ln_argo
         WRITE(numout,*) '            Computation of T trajectories         ln_flork4    = ', ln_flork4
         WRITE(numout,*) '            Use of ariane convention              ln_ariane    = ', ln_ariane
         WRITE(numout,*) '            ascii output (T) or netcdf output (F) ln_flo_ascii = ', ln_flo_ascii

      ENDIF
      !
      !                             ! allocate floats arrays
      IF( flo_oce_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'flo_init : unable to allocate arrays' )
      !
      !                             ! allocate flodom arrays
      IF( flo_dom_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'flo_dom : unable to allocate arrays' )
      !
      !                             ! allocate flowri arrays
      IF( flo_wri_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'flo_wri : unable to allocate arrays' )
      !
      !                             ! allocate florst arrays
      IF( flo_rst_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'flo_rst : unable to allocate arrays' )
      !
      !memory allocation 
      jpnrstflo = jpnfl-jpnnewflo

      !vertical axe for netcdf IOM ouput
      DO jfl=1,jpnfl ; nfloat(jfl)=jfl ; ENDDO

      !
      CALL flo_dom                  ! compute/read initial position of floats

      wb(:,:,:) = wn(:,:,:)         ! set wb for computation of floats trajectories at the first time step
      !
      IF( nn_timing == 1 )   CALL timing_stop('flo_init')
      !
   END SUBROUTINE flo_init

#  else
   !!----------------------------------------------------------------------
   !!   Default option :                                       Empty module
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE flo_stp( kt )          ! Empty routine
      WRITE(*,*) 'flo_stp: You should not have seen this print! error?', kt
   END SUBROUTINE flo_stp
   SUBROUTINE flo_init          ! Empty routine
   END SUBROUTINE flo_init
#endif

   !!======================================================================
 END MODULE floats
