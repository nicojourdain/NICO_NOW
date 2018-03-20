MODULE sbcssr
   !!======================================================================
   !!                       ***  MODULE  sbcssr  ***
   !! Surface module :  heat and fresh water fluxes a restoring term toward observed SST/SSS
   !!======================================================================
   !! History :  3.0  !  2006-06  (G. Madec)  Original code
   !!            3.2  !  2009-04  (B. Lemaire)  Introduce iom_put
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_ssr        : add to sbc a restoring term toward SST/SSS climatology
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE sbc_oce         ! surface boundary condition
   USE phycst          ! physical constants
   USE sbcrnf          ! surface boundary condition : runoffs
   USE fldread         ! read input fields
   USE iom             ! I/O manager
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distribued memory computing library
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_ssr    ! routine called in sbcmod

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   erp   !: evaporation damping   [kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   qrp   !: heat flux damping        [w/m2]

   !                                           !!* Namelist namsbc_ssr *
   INTEGER, PUBLIC ::   nn_sstr     =   0       ! SST/SSS restoring indicator
   INTEGER, PUBLIC ::   nn_sssr     =   0       ! SST/SSS restoring indicator
   REAL(wp)        ::   rn_dqdt     = -40.e0    ! restoring factor on SST and SSS
   REAL(wp)        ::   rn_deds     = -27.70    ! restoring factor on SST and SSS
   LOGICAL         ::   ln_sssr_bnd = .false.   ! flag to bound erp term 
   REAL(wp)        ::   rn_sssr_bnd =   0.e0    ! ABS(Max./Min.) value of erp term [mm/day]

   REAL(wp) , ALLOCATABLE, DIMENSION(:) ::   buffer   ! Temporary buffer for exchange
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_sst   ! structure of input SST (file informations, fields read)
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_sss   ! structure of input SSS (file informations, fields read)

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: sbcssr.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_ssr( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE sbc_ssr  ***
      !!
      !! ** Purpose :   Add to heat and/or freshwater fluxes a damping term
      !!                toward observed SST and/or SSS.
      !!
      !! ** Method  : - Read namelist namsbc_ssr
      !!              - Read observed SST and/or SSS
      !!              - at each nscb time step
      !!                   add a retroaction term on qns    (nn_sstr = 1)
      !!                   add a damping term on emps       (nn_sssr = 1)
      !!                   add a damping term on emp & emps (nn_sssr = 2)
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in   ) ::   kt   ! ocean time step
      !!
      INTEGER  ::   ji, jj   ! dummy loop indices
      REAL(wp) ::   zerp     ! local scalar for evaporation damping
      REAL(wp) ::   zqrp     ! local scalar for heat flux damping
      REAL(wp) ::   zsrp     ! local scalar for unit conversion of rn_deds factor
      REAL(wp) ::   zerp_bnd ! local scalar for unit conversion of rn_epr_max factor
      INTEGER  ::   ierror   ! return error code
      !!
      CHARACTER(len=100) ::  cn_dir          ! Root directory for location of ssr files
      TYPE(FLD_N) ::   sn_sst, sn_sss        ! informations about the fields to be read
      NAMELIST/namsbc_ssr/ cn_dir, nn_sstr, nn_sssr, rn_dqdt, rn_deds, sn_sst, sn_sss, ln_sssr_bnd, rn_sssr_bnd
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('sbc_ssr')
      !
      !                                               ! -------------------- !
      IF( kt == nit000 ) THEN                         ! First call kt=nit000 !
         !                                            ! -------------------- !
         !                            !* set file information
         cn_dir  = './'            ! directory in which the model is executed
         ! ... default values (NB: frequency positive => hours, negative => months)
         !            !   file    ! frequency !  variable  ! time intep !  clim   ! 'yearly' or ! weights  ! rotation   !
         !            !   name    !  (hours)  !   name     !   (T/F)    !  (T/F)  !  'monthly'  ! filename ! pairs      !
         sn_sst = FLD_N( 'sst'    ,    24     ,  'sst'     ,  .false.   , .false. ,   'yearly'  , ''       , ''         )
         sn_sss = FLD_N( 'sss'    ,    -1     ,  'sss'     ,  .true.    , .false. ,   'yearly'  , ''       , ''         )

         REWIND ( numnam )            !* read in namlist namflx
         READ( numnam, namsbc_ssr ) 

         IF(lwp) THEN                 !* control print
            WRITE(numout,*)
            WRITE(numout,*) 'sbc_ssr : SST and/or SSS damping term '
            WRITE(numout,*) '~~~~~~~ '
            WRITE(numout,*) '   Namelist namsbc_ssr :'
            WRITE(numout,*) '      SST restoring term (Yes=1)             nn_sstr     = ', nn_sstr
            WRITE(numout,*) '      SSS damping term (Yes=1, salt flux)    nn_sssr     = ', nn_sssr
            WRITE(numout,*) '                       (Yes=2, volume flux) '
            WRITE(numout,*) '      dQ/dT (restoring magnitude on SST)     rn_dqdt     = ', rn_dqdt, ' W/m2/K'
            WRITE(numout,*) '      dE/dS (restoring magnitude on SST)     rn_deds     = ', rn_deds, ' mm/day'
            WRITE(numout,*) '      flag to bound erp term                 ln_sssr_bnd = ', ln_sssr_bnd
            WRITE(numout,*) '      ABS(Max./Min.) erp threshold           rn_sssr_bnd = ', rn_sssr_bnd, ' mm/day'
         ENDIF

         ! Allocate erp and qrp array
         ALLOCATE( qrp(jpi,jpj), erp(jpi,jpj), STAT=ierror )
         IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_ssr: unable to allocate erp and qrp array' )

         IF( nn_sstr == 1 ) THEN      !* set sf_sst structure & allocate arrays
            !
            ALLOCATE( sf_sst(1), STAT=ierror )
            IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_ssr: unable to allocate sf_sst structure' )
            ALLOCATE( sf_sst(1)%fnow(jpi,jpj,1), STAT=ierror )
            IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_ssr: unable to allocate sf_sst now array' )
            !
            ! fill sf_sst with sn_sst and control print
            CALL fld_fill( sf_sst, (/ sn_sst /), cn_dir, 'sbc_ssr', 'SST restoring term toward SST data', 'namsbc_ssr' )
            IF( sf_sst(1)%ln_tint )   ALLOCATE( sf_sst(1)%fdta(jpi,jpj,1,2), STAT=ierror )
            IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_ssr: unable to allocate sf_sst data array' )
            !
         ENDIF
         !
         IF( nn_sssr >= 1 ) THEN      ! set sf_sss structure & allocate arrays
            !
            ALLOCATE( sf_sss(1), STAT=ierror )
            IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_ssr: unable to allocate sf_sss structure' )
            ALLOCATE( sf_sss(1)%fnow(jpi,jpj,1), STAT=ierror )
            IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_ssr: unable to allocate sf_sss now array' )
            !
            ! fill sf_sss with sn_sss and control print
            CALL fld_fill( sf_sss, (/ sn_sss /), cn_dir, 'sbc_ssr', 'SSS restoring term toward SSS data', 'namsbc_ssr' )
            IF( sf_sss(1)%ln_tint )   ALLOCATE( sf_sss(1)%fdta(jpi,jpj,1,2), STAT=ierror )
            IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_ssr: unable to allocate sf_sss data array' )
            !
         ENDIF
         !
         ! Initialize qrp and erp if no restoring 
         IF( nn_sstr /= 1                   )   qrp(:,:) = 0.e0 
         IF( nn_sssr /= 1 .OR. nn_sssr /= 2 )   erp(:,:) = 0.e0 
      ENDIF

      IF( nn_sstr + nn_sssr /= 0 ) THEN
         !
         IF( nn_sstr == 1)   CALL fld_read( kt, nn_fsbc, sf_sst )   ! Read SST data and provides it at kt
         IF( nn_sssr >= 1)   CALL fld_read( kt, nn_fsbc, sf_sss )   ! Read SSS data and provides it at kt
         !
         !                                         ! ========================= !
         IF( MOD( kt-1, nn_fsbc ) == 0 ) THEN      !    Add restoring term     !
            !                                      ! ========================= !
            !
            IF( nn_sstr == 1 ) THEN                   !* Temperature restoring term
!CDIR COLLAPSE
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     zqrp = rn_dqdt * ( sst_m(ji,jj) - sf_sst(1)%fnow(ji,jj,1) )
                     qns(ji,jj) = qns(ji,jj) + zqrp
                     qrp(ji,jj) = zqrp
                  END DO
               END DO
               CALL iom_put( "qrp", qrp )                             ! heat flux damping
            ENDIF
            !
            IF( nn_sssr == 1 ) THEN                   !* Salinity damping term (salt flux, emps only)
               zsrp = rn_deds / rday                                  ! from [mm/day] to [kg/m2/s]
!CDIR COLLAPSE
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     zerp = zsrp * ( 1. - 2.*rnfmsk(ji,jj) )   &      ! No damping in vicinity of river mouths
                        &        * ( sss_m(ji,jj) - sf_sss(1)%fnow(ji,jj,1) )   &
                        &        / ( sss_m(ji,jj) + 1.e-20   )
                     emps(ji,jj) = emps(ji,jj) + zerp
                     erp( ji,jj) = zerp
                  END DO
               END DO
               CALL iom_put( "erp", erp )                             ! freshwater flux damping
               !
            ELSEIF( nn_sssr == 2 ) THEN               !* Salinity damping term (volume flux, emp and emps)
               zsrp = rn_deds / rday                                  ! from [mm/day] to [kg/m2/s]
               zerp_bnd = rn_sssr_bnd / rday                          !       -              -    
!CDIR COLLAPSE
               DO jj = 1, jpj
                  DO ji = 1, jpi                            
                     zerp = zsrp * ( 1. - 2.*rnfmsk(ji,jj) )   &      ! No damping in vicinity of river mouths
                        &        * ( sss_m(ji,jj) - sf_sss(1)%fnow(ji,jj,1) )   &
                        &        / ( sss_m(ji,jj) + 1.e-20   )
                     IF( ln_sssr_bnd )   zerp = SIGN( 1., zerp ) * MIN( zerp_bnd, ABS(zerp) )
                     emp (ji,jj) = emp (ji,jj) + zerp
                     emps(ji,jj) = emps(ji,jj) + zerp
                     erp (ji,jj) = zerp
                  END DO
               END DO
               CALL iom_put( "erp", erp )                             ! freshwater flux damping
            ENDIF
            !
         ENDIF
         !
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('sbc_ssr')
      !
   END SUBROUTINE sbc_ssr
      
   !!======================================================================
END MODULE sbcssr
