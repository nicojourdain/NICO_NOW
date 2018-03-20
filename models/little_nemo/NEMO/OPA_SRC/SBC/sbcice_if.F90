MODULE sbcice_if
   !!======================================================================
   !!                       ***  MODULE  sbcice  ***
   !! Surface module :  update surface ocean boundary condition over ice
   !!                   covered area using ice-if model
   !!======================================================================
   !! History :  3.0   !  2006-06  (G. Madec)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_ice_if     : update sbc in ice-covered area
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE eosbn2          ! equation of state
   USE sbc_oce         ! surface boundary condition: ocean fields
   USE sbccpl
   USE fldread         ! read input field
   USE iom             ! I/O manager library
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_ice_if      ! routine called in sbcmod

   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_ice   ! structure of input ice-cover (file informations, fields read)
   
   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: sbcice_if.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_ice_if( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE sbc_ice_if  ***
      !!
      !! ** Purpose :   handle surface boundary condition over ice cover area
      !!      when sea-ice model are not used
      !!
      !! ** Method  : - read sea-ice cover climatology
      !!              - blah blah blah, ...
      !!
      !! ** Action  :   utau, vtau : remain unchanged
      !!                taum, wndm : remain unchanged
      !!                qns, qsr   : update heat flux below sea-ice
      !!                emp, emps  : update freshwater flux below sea-ice
      !!                fr_i       : update the ice fraction
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      !
      INTEGER  ::   ji, jj     ! dummy loop indices
      INTEGER  ::   ierror     ! return error code
      REAL(wp) ::   ztrp, zsice, zt_fzp, zfr_obs
      REAL(wp) ::   zqri, zqrj, zqrp, zqi
      !!
      CHARACTER(len=100) ::   cn_dir              ! Root directory for location of ice-if files
      TYPE(FLD_N)        ::   sn_ice              ! informations about the fields to be read
      NAMELIST/namsbc_iif/ cn_dir, sn_ice
      !!---------------------------------------------------------------------
      !                                         ! ====================== !
      IF( kt == nit000 ) THEN                   !  First call kt=nit000  !
         !                                      ! ====================== !
         ! set file information
         cn_dir = './'        ! directory in which the model is executed
         ! ... default values (NB: frequency positive => hours, negative => months)
         !             !   file    ! frequency !  variable  ! time intep !  clim  ! 'yearly' or ! weights  ! rotation   !
         !             !   name    !  (hours)  !   name     !   (T/F)    !  (T/F) !  'monthly'  ! filename ! pairs      ! 
         sn_ice = FLD_N('ice_cover',    -1    ,  'ice_cov' ,  .true.    , .true. ,   'yearly'  , ''       , ''         )

         REWIND ( numnam )               ! ... read in namlist namiif
         READ   ( numnam, namsbc_iif )

         ALLOCATE( sf_ice(1), STAT=ierror )
         IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_ice_if: unable to allocate sf_ice structure' )
         ALLOCATE( sf_ice(1)%fnow(jpi,jpj,1) )
         IF( sn_ice%ln_tint )   ALLOCATE( sf_ice(1)%fdta(jpi,jpj,1,2) )

         ! fill sf_ice with sn_ice and control print
         CALL fld_fill( sf_ice, (/ sn_ice /), cn_dir, 'sbc_ice_if', 'ice-if sea-ice model', 'namsbc_iif' )
         !
      ENDIF

      CALL fld_read( kt, nn_fsbc, sf_ice )           ! Read input fields and provides the
      !                                              ! input fields at the current time-step
      
      IF( MOD( kt-1, nn_fsbc) == 0 ) THEN
         !
         ztrp = -40.             ! restoring terme for temperature (w/m2/k)
         zsice = - 0.04 / 0.8    ! ratio of isohaline compressibility over isotherme compressibility
                                 ! ( d rho / dt ) / ( d rho / ds )      ( s = 34, t = -1.8 )
         
         fr_i(:,:) = tfreez( sss_m ) * tmask(:,:,1)      ! sea surface freezing temperature [Celcius]
#if defined key_coupled 
         a_i(:,:,1) = fr_i(:,:)         
#endif

         ! Flux and ice fraction computation
!CDIR COLLAPSE
         DO jj = 1, jpj
            DO ji = 1, jpi
               !
               zt_fzp  = fr_i(ji,jj)                        ! freezing point temperature
               zfr_obs = sf_ice(1)%fnow(ji,jj,1)            ! observed ice cover
               !                                            ! ocean ice fraction (0/1) from the freezing point temperature
               IF( sst_m(ji,jj) <= zt_fzp ) THEN   ;   fr_i(ji,jj) = 1.e0
               ELSE                                ;   fr_i(ji,jj) = 0.e0
               ENDIF

               tsn(ji,jj,1,jp_tem) = MAX( tsn(ji,jj,1,jp_tem), zt_fzp )     ! avoid over-freezing point temperature

               qsr(ji,jj) = ( 1. - zfr_obs ) * qsr(ji,jj)   ! solar heat flux : zero below observed ice cover

               !                                            ! non solar heat flux : add a damping term 
               !      # ztrp*(t-(tgel-1.))  if observed ice and no opa ice   (zfr_obs=1 fr_i=0)
               !      # ztrp*min(0,t-tgel)  if observed ice and opa ice      (zfr_obs=1 fr_i=1)
               zqri = ztrp * ( tsb(ji,jj,1,jp_tem) - ( zt_fzp - 1.) )
               zqrj = ztrp * MIN( 0., tsb(ji,jj,1,jp_tem) - zt_fzp )
               zqrp = ( zfr_obs * ( (1. - fr_i(ji,jj) ) * zqri    &
                 &                 +      fr_i(ji,jj)   * zqrj ) ) * tmask(ji,jj,1)

               !                                            ! non-solar heat flux 
               !      # qns unchanged              if no climatological ice              (zfr_obs=0)
               !      # qns = zqrp                 if climatological ice and no opa ice  (zfr_obs=1, fr_i=0)
               !      # qns = zqrp -2(-4) watt/m2  if climatological ice and opa ice     (zfr_obs=1, fr_i=1)
               !                                   (-2=arctic, -4=antarctic)   
               zqi = -3. + SIGN( 1.e0, ff(ji,jj) )
               qns(ji,jj) = ( ( 1.- zfr_obs ) * qns(ji,jj)                             &
                  &          +      zfr_obs   * fr_i(ji,jj) * zqi ) * tmask(ji,jj,1)   &
                  &       + zqrp
            END DO
         END DO
         !
      ENDIF
      !
   END SUBROUTINE sbc_ice_if

   !!======================================================================
END MODULE sbcice_if
