MODULE sbcwave
   !!======================================================================
   !!                       ***  MODULE  sbcwave  ***
   !! Wave module 
   !!======================================================================
   !! History :  3.3.1  !   2011-09  (Adani M)  Original code
   !!----------------------------------------------------------------------
   USE iom             ! I/O manager library
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distribued memory computing library
   USE fldread	       ! read input fields
   USE sbc_oce	       ! Surface boundary condition: ocean fields

   
   !!----------------------------------------------------------------------
   !!   sbc_wave       : read drag coefficient from wave model in netcdf files 
   !!----------------------------------------------------------------------

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_wave    ! routine called in sbc_blk_core or sbc_blk_mfs
   
   TYPE(FLD), ALLOCATABLE, DIMENSION(:)  :: sf_wave	  ! structure of input fields (file informations, fields read)
   REAL(wp),PUBLIC,ALLOCATABLE,DIMENSION (:,:)       :: cdn_wave 

   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011) 
   !! $Id: $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_wave( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE sbc_apr  ***
      !!
      !! ** Purpose :   read drag coefficient from wave model  in netcdf files.
      !!
      !! ** Method  : - Read namelist namsbc_wave
      !!              - Read Cd_n10 fields in netcdf files 
      !! ** action  :   
      !!               
      !!---------------------------------------------------------------------
      INTEGER, INTENT( in  ) ::  kt   ! ocean time step
      INTEGER                ::  ierror   ! return error code
      CHARACTER(len=100)     ::  cn_dir_cdg                       ! Root directory for location of drag coefficient files
      TYPE(FLD_N)            ::  sn_cdg                          ! informations about the fields to be read
      !!---------------------------------------------------------------------
      NAMELIST/namsbc_wave/  sn_cdg, cn_dir_cdg
      !!---------------------------------------------------------------------

      !!----------------------------------------------------------------------
      !
      !
      !                                         ! -------------------- !
      IF( kt == nit000 ) THEN                   ! First call kt=nit000 !
         !                                      ! -------------------- !
         !                                            !* set file information (default values)
         ! ... default values (NB: frequency positive => hours, negative => months)
         !              !   file   ! frequency !  variable  ! time intep !  clim   ! 'yearly' or ! weights  ! rotation !
         !              !   name   !  (hours)  !   name     !   (T/F)    !  (T/F)  !  'monthly'  ! filename ! pairs    !
         sn_cdg = FLD_N('cdg_wave'  ,    1     ,'drag_coeff',  .true.    , .false. ,   'daily'   , ''       , ''       )
         cn_dir_cdg = './'          ! directory in which the Patm data are 
         

         REWIND( numnam )                             !* read in namlist namsbc_wave
         READ  ( numnam, namsbc_wave ) 
         !

         ALLOCATE( sf_wave(1), STAT=ierror )           !* allocate and fill sf_wave with sn_cdg
         IF( ierror > 0 )   CALL ctl_stop( 'STOP', 'sbc_wave: unable to allocate sf_wave structure' )
         !
         CALL fld_fill( sf_wave, (/ sn_cdg /), cn_dir_cdg, 'sbc_wave', 'Wave module ', 'namsbc_wave' )
                                ALLOCATE( sf_wave(1)%fnow(jpi,jpj,1)   )
         IF( sn_cdg%ln_tint )   ALLOCATE( sf_wave(1)%fdta(jpi,jpj,1,2) )
         ALLOCATE( cdn_wave(jpi,jpj) )
         cdn_wave(:,:) = 0.0
      ENDIF
         !
         !
      CALL fld_read( kt, nn_fsbc, sf_wave )      !* read drag coefficient from external forcing
      cdn_wave(:,:) = sf_wave(1)%fnow(:,:,1)

   END SUBROUTINE sbc_wave
      
   !!======================================================================
END MODULE sbcwave
