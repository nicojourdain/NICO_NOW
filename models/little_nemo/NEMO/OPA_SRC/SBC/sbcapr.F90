MODULE sbcapr
   !!======================================================================
   !!                       ***  MODULE  sbcapr  ***
   !! Surface module :   atmospheric pressure forcing
   !!======================================================================
   !! History :  3.3  !   2010-09  (J. Chanut, C. Bricaud, G. Madec)  Original code
   !!----------------------------------------------------------------------
   
   !!----------------------------------------------------------------------
   !!   sbc_apr        : read atmospheric pressure in netcdf files 
   !!----------------------------------------------------------------------
   USE obc_par         ! open boundary condition parameters
   USE dom_oce         ! ocean space and time domain
   USE sbc_oce         ! surface boundary condition
   USE dynspg_oce      ! surface pressure gradient variables
   USE phycst          ! physical constants
   USE fldread         ! read input fields
   USE in_out_manager  ! I/O manager
   USE lib_fortran     ! distribued memory computing library
   USE iom             ! IOM library
   USE lib_mpp         ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_apr    ! routine called in sbcmod
   
   !                                         !!* namsbc_apr namelist (Atmospheric PRessure) *
   LOGICAL, PUBLIC ::   ln_apr_obc = .FALSE.  !: inverse barometer added to OBC ssh data 
   LOGICAL, PUBLIC ::   ln_ref_apr = .FALSE.  !: ref. pressure: global mean Patm (F) or a constant (F)

   REAL(wp), ALLOCATABLE, SAVE, PUBLIC, DIMENSION(:,:) ::   ssh_ib    ! Inverse barometer now    sea surface height   [m]
   REAL(wp), ALLOCATABLE, SAVE, PUBLIC, DIMENSION(:,:) ::   ssh_ibb   ! Inverse barometer before sea surface height   [m]
   REAL(wp), ALLOCATABLE, SAVE, PUBLIC, DIMENSION(:,:) ::   apr       ! atmospheric pressure at kt                 [N/m2]
   
   REAL(wp) ::   rpref = 101000._wp   ! reference atmospheric pressure          [N/m2]
   REAL(wp) ::   tarea                ! whole domain mean masked ocean surface
   REAL(wp) ::   r1_grau              ! = 1.e0 / (grav * rau0)
   
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_apr   ! structure of input fields (file informations, fields read)

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011) 
   !! $Id: $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_apr( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE sbc_apr  ***
      !!
      !! ** Purpose :   read atmospheric pressure fields in netcdf files.
      !!
      !! ** Method  : - Read namelist namsbc_apr
      !!              - Read Patm fields in netcdf files 
      !!              - Compute reference atmospheric pressure
      !!              - Compute inverse barometer ssh
      !! ** action  :   apr      : atmospheric pressure at kt
      !!                ssh_ib   : inverse barometer ssh at kt
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in)::   kt   ! ocean time step
      !!
      INTEGER            ::   ierror  ! local integer 
      REAL(wp)           ::   zpref   ! local scalar
      !!
      CHARACTER(len=100) ::  cn_dir   ! Root directory for location of ssr files
      TYPE(FLD_N)        ::  sn_apr   ! informations about the fields to be read
      !!
      NAMELIST/namsbc_apr/ cn_dir, sn_apr, ln_ref_apr
      !!----------------------------------------------------------------------
      !
      !
      !                                         ! -------------------- !
      IF( kt == nit000 ) THEN                   ! First call kt=nit000 !
         !                                      ! -------------------- !
         !                                            !* set file information (default values)
         ! ... default values (NB: frequency positive => hours, negative => months)
         !            !   file   ! frequency !  variable  ! time intep !  clim   ! 'yearly' or ! weights  ! rotation !
         !            !   name   !  (hours)  !   name     !   (T/F)    !  (T/F)  !  'monthly'  ! filename ! pairs    !
         sn_apr = FLD_N( 'patm'  ,    24     ,  'patm'    ,  .false.   , .true.  ,   'yearly'  , ''       , ''       )
         cn_dir  = './'          ! directory in which the Patm data are 

         REWIND( numnam )                             !* read in namlist namsbc_apr
         READ  ( numnam, namsbc_apr ) 
         !
         ALLOCATE( sf_apr(1), STAT=ierror )           !* allocate and fill sf_sst (forcing structure) with sn_sst
         IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_apr: unable to allocate sf_apr structure' )
         !
         CALL fld_fill( sf_apr, (/ sn_apr /), cn_dir, 'sbc_apr', 'Atmospheric pressure ', 'namsbc_apr' )
                                ALLOCATE( sf_apr(1)%fnow(jpi,jpj,1)   )
         IF( sn_apr%ln_tint )   ALLOCATE( sf_apr(1)%fdta(jpi,jpj,1,2) )
                                ALLOCATE( ssh_ib(jpi,jpj) , ssh_ibb(jpi,jpj) )
                                ALLOCATE( apr (jpi,jpj) )
         !
         IF(lwp) THEN                                 !* control print
            WRITE(numout,*)
            WRITE(numout,*) '   Namelist namsbc_apr : Atmospheric PRessure as extrenal forcing'
            WRITE(numout,*) '      ref. pressure: global mean Patm (T) or a constant (F)  ln_ref_apr = ', ln_ref_apr
         ENDIF
         !
         IF( ln_ref_apr ) THEN                        !* Compute whole inner domain mean masked ocean surface
            tarea = glob_sum( e1t(:,:) * e2t(:,:) )
            IF(lwp) WRITE(numout,*) '         Variable ref. Patm computed over a ocean surface of ', tarea*1e-6, 'km2'
         ELSE
            IF(lwp) WRITE(numout,*) '         Reference Patm used : ', rpref, ' N/m2'
         ENDIF
         !
         r1_grau = 1.e0 / (grav * rau0)               !* constant for optimization
         !
         !                                            !* control check
         IF( ln_apr_obc  )   &
            CALL ctl_stop( 'sbc_apr: inverse barometer added to OBC ssh data not yet implemented ' )
         IF( ln_apr_obc .AND. .NOT. lk_obc )   &
            CALL ctl_stop( 'sbc_apr: add inverse barometer to OBC requires to use key_obc' )
         IF( ( ln_apr_obc ) .AND. .NOT. lk_dynspg_ts )   &
            CALL ctl_stop( 'sbc_apr: use inverse barometer ssh at open boundary ONLY possible with time-splitting' )
         IF( ( ln_apr_obc ) .AND. .NOT. ln_apr_dyn   )   &
            CALL ctl_stop( 'sbc_apr: use inverse barometer ssh at open boundary ONLY requires ln_apr_dyn=T' )
      ENDIF

      !                                         ! ========================== !
      IF( MOD( kt-1, nn_fsbc ) == 0 ) THEN      !    At each sbc time-step   !
         !                                      ! ===========+++============ !
         !
         IF( kt /= nit000 )   ssh_ibb(:,:) = ssh_ib(:,:)    !* Swap of ssh_ib fields
         !
         CALL fld_read( kt, nn_fsbc, sf_apr )               !* input Patm provided at kt + nn_fsbc/2
         !
         !                                                  !* update the reference atmospheric pressure (if necessary)
         IF( ln_ref_apr )   rpref = glob_sum( sf_apr(1)%fnow(:,:,1) * e1t(:,:) * e2t(:,:) ) / tarea
         !
         !                                                  !* Patm related forcing at kt
         ssh_ib(:,:) = - ( sf_apr(1)%fnow(:,:,1) - rpref ) * r1_grau    ! equivalent ssh (inverse barometer)
         apr   (:,:) =     sf_apr(1)%fnow(:,:,1)                        ! atmospheric pressure
         !
         CALL iom_put( "ssh_ib", ssh_ib )                   !* output the inverse barometer ssh
      ENDIF

      !                                         ! ---------------------------------------- !
      IF( kt == nit000 ) THEN                   !   set the forcing field at nit000 - 1    !
         !                                      ! ---------------------------------------- !
         !                                            !* Restart: read in restart file
         IF( ln_rstart .AND. iom_varid( numror, 'ssh_ibb', ldstop = .FALSE. ) > 0 ) THEN 
            IF(lwp) WRITE(numout,*) 'sbc_apr:   ssh_ibb read in the restart file'
            CALL iom_get( numror, jpdom_autoglo, 'ssh_ibb', ssh_ibb )   ! before inv. barometer ssh
            !
         ELSE                                         !* no restart: set from nit000 values
            IF(lwp) WRITE(numout,*) 'sbc_apr:   ssh_ibb set to nit000 values'
            ssh_ibb(:,:) = ssh_ib(:,:)
         ENDIF
      ENDIF
      !                                         ! ---------------------------------------- !
      IF( lrst_oce ) THEN                       !      Write in the ocean restart file     !
         !                                      ! ---------------------------------------- !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'sbc_apr : ssh_ib written in ocean restart file at it= ', kt,' date= ', ndastp
         IF(lwp) WRITE(numout,*) '~~~~'
         CALL iom_rstput( kt, nitrst, numrow, 'ssh_ibb' , ssh_ib )
      ENDIF
      !
   END SUBROUTINE sbc_apr
      
   !!======================================================================
END MODULE sbcapr
