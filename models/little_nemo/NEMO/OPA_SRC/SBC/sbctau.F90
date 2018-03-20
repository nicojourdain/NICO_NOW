MODULE sbctau
   !!======================================================================
   !!                       ***  MODULE  sbctau  ***
   !! Surface module :  read utau/vtau and overwrite it!
   !!======================================================================
   !! History :  
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_tau        : overwrite utau and vtau
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE sbc_oce         ! surface boundary condition
   USE fldread         ! read input fields
   USE in_out_manager  ! I/O manager
   USE timing          ! Timing
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_tau    ! routine called in sbcmod

   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_utau   ! structure of input UTAU (file informations, fields read)
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_vtau   ! structure of input VTAU (file informations, fields read)

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: sbctau.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_tau( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE sbc_tau  ***
      !!
      !! ** Purpose :   read and overwite utau and vtau
      !!
      !! ** Method  : - Read namelist namsbc_tau
      !!              - Read observed utau and vtau
      !!              - at each nscb time step overwrite these variables
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in   ) ::   kt   ! ocean time step
      !!
      INTEGER  ::   ierror   ! return error code
      !!
      CHARACTER(len=100) ::  cn_dir           ! Root directory for location of tau files
      TYPE(FLD_N) ::   sn_utau, sn_vtau        ! informations about the fields to be read
      NAMELIST/namsbc_tau/ cn_dir, sn_utau, sn_vtau
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('sbc_tau')
      !
      !                                               ! -------------------- !
      IF( kt == nit000 ) THEN                         ! First call kt=nit000 !
         !                                            ! -------------------- !
         !                            !* set file information
         cn_dir  = './'            ! directory in which the model is executed
         ! ... default values (NB: frequency positive => hours, negative => months)
         !            !   file    ! frequency !  variable  ! time intep !  clim   ! 'yearly' or ! weights  ! rotation   !
         !            !   name    !  (hours)  !   name     !   (T/F)    !  (T/F)  !  'monthly'  ! filename ! pairs      !
         sn_utau = FLD_N( 'utau'    ,    6     ,  'utau'    ,  .false.   , .false. ,   'yearly'  , ''       , ''         )
         sn_vtau = FLD_N( 'vtau'    ,    6     ,  'vtau'    ,  .true.    , .false. ,   'yearly'  , ''       , ''         )

         REWIND ( numnam )            !* read in namlist namflx
         READ( numnam, namsbc_tau ) 

         IF(lwp) THEN                 !* control print
            WRITE(numout,*)
            WRITE(numout,*) 'sbc_tau : read and overwite utau and vtau '
            WRITE(numout,*) '~~~~~~~ '
         ENDIF

         ALLOCATE( sf_utau(1), STAT=ierror )
         IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_tau: unable to allocate sf_utau structure' )
         ALLOCATE( sf_utau(1)%fnow(jpi,jpj,1), STAT=ierror )
         IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_tau: unable to allocate sf_utau now array' )
         !
         ! fill sf_utau with sn_utau and control print
         CALL fld_fill( sf_utau, (/ sn_utau /), cn_dir, 'sbc_tau', 'UTAU data', 'namsbc_tau' )
         IF( sf_utau(1)%ln_tint )   ALLOCATE( sf_utau(1)%fdta(jpi,jpj,1,2), STAT=ierror )
         IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_tau: unable to allocate sf_utau data array' )
         !
         ALLOCATE( sf_vtau(1), STAT=ierror )
         IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_tau: unable to allocate sf_vtau structure' )
         ALLOCATE( sf_vtau(1)%fnow(jpi,jpj,1), STAT=ierror )
         IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_tau: unable to allocate sf_vtau now array' )
         !
         ! fill sf_vtau with sn_vtau and control print
         CALL fld_fill( sf_vtau, (/ sn_vtau /), cn_dir, 'sbc_tau', 'VTAU data', 'namsbc_tau' )
         IF( sf_vtau(1)%ln_tint )   ALLOCATE( sf_vtau(1)%fdta(jpi,jpj,1,2), STAT=ierror )
         IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_tau: unable to allocate sf_vtau data array' )
         !
      ENDIF

      !
      CALL fld_read( kt, nn_fsbc, sf_utau )   ! Read UTAU data and provides it at kt
      CALL fld_read( kt, nn_fsbc, sf_vtau )   ! Read VTAU data and provides it at kt
      !
      !                                         ! ========================= !
      IF( MOD( kt-1, nn_fsbc ) == 0 ) THEN      !    overwrite utau/vtau    !
         !                                      ! ========================= !
         !
         utau(:,:) = sf_utau(1)%fnow(:,:,1)
         vtau(:,:) = sf_vtau(1)%fnow(:,:,1)
         !
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('sbc_tau')
      !
   END SUBROUTINE sbc_tau
      
   !!======================================================================
END MODULE sbctau
