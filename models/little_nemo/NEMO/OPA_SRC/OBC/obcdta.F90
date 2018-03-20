MODULE obcdta
   !!======================================================================
   !!                       ***  MODULE obcdta  ***
   !! Open boundary data : read the data for the open boundaries.
   !!======================================================================
   !! History : 3.3  !  2010-12  ()        -              -
   !!----------------------------------------------------------------------
#if defined key_obc
   !!------------------------------------------------------------------------------
   !!   'key_obc'         :                                Open Boundary Conditions
   !!------------------------------------------------------------------------------
   !!   obc_dta           : read u, v, t, s data along each open boundary
   !!------------------------------------------------------------------------------
   USE obc_oce         ! ocean open boundary conditions
   USE obc_par         ! ocean open boundary conditions
   USE fldread         ! read input fields
   USE in_out_manager  ! I/O logical units
   USE lib_mpp         ! for ctl_stop

   IMPLICIT NONE
   PRIVATE

   PUBLIC obc_dta      ! routines called by step.F90
   PUBLIC obc_dta_bt   ! routines called by dynspg_ts.F90

   TYPE(FLD), DIMENSION(:), ALLOCATABLE ::   sf_obc       !: structure: 
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obcdta.F90 188 2010-12-28 21:15:19Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE obc_dta( kt )
      !!---------------------------------------------------------------------------
      !!                      ***  SUBROUTINE obc_dta  ***
      !!                    
      !! ** Purpose :   read the data for the open boundaries.
      !!---------------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt     ! ocean time-step
      !!---------------------------------------------------------------------
      !      
      IF( lk_obc ) THEN

      IF( kt == nit000 )  CALL obc_dta_init()
      !
      IF ( lp_obc_east ) THEN
         CALL fld_read( kt, 1, sf_obc(1:4) )
         ufoe(:,:) = sf_obc(1)%fnow(:,:,1)
         vfoe(:,:) = sf_obc(2)%fnow(:,:,1)
         tfoe(:,:) = sf_obc(3)%fnow(:,:,1)
         sfoe(:,:) = sf_obc(4)%fnow(:,:,1)
      ENDIF
 
      IF ( lp_obc_west ) THEN
         CALL fld_read( kt, 1, sf_obc(5:8) )
         ufow(:,:) = sf_obc(5)%fnow(:,:,1)
         vfow(:,:) = sf_obc(6)%fnow(:,:,1)
         tfow(:,:) = sf_obc(7)%fnow(:,:,1)
         sfow(:,:) = sf_obc(8)%fnow(:,:,1)
      ENDIF
 
      IF ( lp_obc_north ) THEN
         CALL fld_read( kt, 1, sf_obc(9:12) )
         ufon(:,:) = sf_obc( 9)%fnow(:,:,1)
         vfon(:,:) = sf_obc(10)%fnow(:,:,1)
         tfon(:,:) = sf_obc(11)%fnow(:,:,1)
         sfon(:,:) = sf_obc(12)%fnow(:,:,1)
      ENDIF
 
      IF ( lp_obc_south ) THEN
         CALL fld_read( kt, 1, sf_obc(13:16) )
         ufos(:,:) = sf_obc(13)%fnow(:,:,1)
         vfos(:,:) = sf_obc(14)%fnow(:,:,1)
         tfos(:,:) = sf_obc(15)%fnow(:,:,1)
         sfos(:,:) = sf_obc(16)%fnow(:,:,1)
      ENDIF

      ENDIF
 
   END SUBROUTINE obc_dta


# if defined key_dynspg_ts || defined key_dynspg_exp
   SUBROUTINE obc_dta_bt( kt, kbt )
      INTEGER,INTENT(in) :: kt
      INTEGER, INTENT( in ) ::   kbt         ! barotropic ocean time-step index
      !!---------------------------------------------------------------------------
      !!                      ***  SUBROUTINE obc_dta  ***
      !!
      !! ** Purpose :   time interpolation of barotropic data for time-splitting scheme
      !!                Data at the boundary must be in m2/s 
   END SUBROUTINE obc_dta_bt

# else
   !!-----------------------------------------------------------------------------
   !!   Default option
   !!-----------------------------------------------------------------------------
   SUBROUTINE obc_dta_bt ( kt, kbt )       ! Empty routine
      INTEGER,INTENT(in) :: kt
      INTEGER, INTENT( in ) ::   kbt         ! barotropic ocean time-step index
      WRITE(*,*) 'obc_dta_bt: You should not have seen this print! error?', kt
      WRITE(*,*) 'obc_dta_bt: You should not have seen this print! error?', kbt
   END SUBROUTINE obc_dta_bt
# endif


   SUBROUTINE obc_dta_init
      !!---------------------------------------------------------------------------
      !!                      ***  SUBROUTINE obc_dta  ***
      !!                    
      !! ** Purpose :   initialization of ....
      !!
      !! ** Method  : - read the obc namobc_dta namelist
      !!
      !! ** Action  : - read parameters
      !!---------------------------------------------------------------------------
      INTEGER :: ifpr,ierror
      CHARACTER(len=100) ::  cn_dir   !   Root directory for location of core files
      TYPE(FLD_N), DIMENSION(4) ::   sn_obce, sn_obcw, sn_obcn, sn_obcs  ! array of namelist informations on the obc to read
      NAMELIST/namobc_dta/ sn_obce, sn_obcw, sn_obcn, sn_obcs
      !!---------------------------------------------------------------------
      ALLOCATE(sf_obc(16), stat = ierror)
      IF( ierror > 0 ) CALL ctl_stop('Pb of alloction of sf_obc(16) obc_dta_init')
      !
      ! set file information (default values)
      cn_dir = './'       ! directory in which the model is executed
      !
      ! (NB: frequency positive => hours, negative => months)
      !            !    file    ! frequency ! variable ! time intep !  clim   ! 'yearly' or ! weights  ! rotation !
      !            !    name    !  (hours)  !  name    !   (T/F)    !  (T/F)  !  'monthly'  ! filename ! pairs    !
      sn_obce(1) = FLD_N( 'obc_east', 120   ,   'vozocrtx'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
      sn_obce(2) = FLD_N( 'obc_east', 120   ,   'vomecrty'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
      sn_obce(3) = FLD_N( 'obc_east', 120   ,   'votemper'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
      sn_obce(4) = FLD_N( 'obc_east', 120   ,   'vosaline'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
         
      sn_obcw(1) = FLD_N( 'obc_west', 120   ,   'vozocrtx'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
      sn_obcw(2) = FLD_N( 'obc_west', 120   ,   'vomecrty'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
      sn_obcw(3) = FLD_N( 'obc_west', 120   ,   'votemper'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
      sn_obcw(4) = FLD_N( 'obc_west', 120   ,   'vosaline'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
         
      sn_obcn(1) = FLD_N( 'obc_north', 120  ,   'vozocrtx'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
      sn_obcn(2) = FLD_N( 'obc_north', 120  ,   'vomecrty'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
      sn_obcn(3) = FLD_N( 'obc_north', 120  ,   'votemper'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
      sn_obcn(4) = FLD_N( 'obc_north', 120  ,   'vosaline'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
         
      sn_obcs(1) = FLD_N( 'obc_south', 120  ,   'vozocrtx'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
      sn_obcs(2) = FLD_N( 'obc_south', 120  ,   'vomecrty'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
      sn_obcs(3) = FLD_N( 'obc_south', 120  ,   'votemper'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
      sn_obcs(4) = FLD_N( 'obc_south', 120  ,   'vosaline'   ,   .TRUE.   , .FALSE. ,   'yearly'  , ''       , ''       )
      !
      REWIND( numnam )                          ! read in namlist namobc_dta
      READ  ( numnam, namobc_dta )
         
      IF ( lp_obc_east ) THEN
         DO ifpr= 1, 4
            ALLOCATE( sf_obc(ifpr)%fnow(jpj,jpk,1) )
            IF( sn_obce(ifpr)%ln_tint ) ALLOCATE( sf_obc(ifpr)%fdta(jpj,jpk,1,2) )
         END DO
         CALL fld_fill( sf_obc(1:4), sn_obce, cn_dir, 'obc_dta_init', 'fill east OBC', 'namobc_dta' )
      ENDIF
 
      IF ( lp_obc_west ) THEN
         DO ifpr= 5, 8
            ALLOCATE( sf_obc(ifpr)%fnow(jpj,jpk,1) )
            IF( sn_obcw(ifpr-4)%ln_tint ) ALLOCATE( sf_obc(ifpr)%fdta(jpj,jpk,1,2) )
         END DO
         CALL fld_fill( sf_obc(5:8), sn_obcw, cn_dir, 'obc_dta_init', 'fill west OBC', 'namobc_dta' )
      ENDIF
 
      IF ( lp_obc_north ) THEN
         DO ifpr= 9, 12
            ALLOCATE( sf_obc(ifpr)%fnow(jpi,jpk,1) )
            IF( sn_obcn(ifpr-8)%ln_tint ) ALLOCATE( sf_obc(ifpr)%fdta(jpi,jpk,1,2) )
         END DO
         CALL fld_fill( sf_obc(9:12), sn_obcn, cn_dir, 'obc_dta_init', 'fill north OBC', 'namobc_dta' )
      ENDIF
 
      IF ( lp_obc_south ) THEN
         DO ifpr= 13, 16
            ALLOCATE( sf_obc(ifpr)%fnow(jpi,jpk,1) )
            IF( sn_obcs(ifpr-12)%ln_tint ) ALLOCATE( sf_obc(ifpr)%fdta(jpi,jpk,1,2) )
         END DO
         CALL fld_fill( sf_obc(13:16), sn_obcs, cn_dir, 'obc_dta_init', 'fill south OBC', 'namobc_dta' )
      ENDIF

   END SUBROUTINE obc_dta_init

#else
      !!------------------------------------------------------------------------------
      !!   default option:           Dummy module          NO Open Boundary Conditions
      !!------------------------------------------------------------------------------
   CONTAINS
      SUBROUTINE obc_dta( kt )             ! Dummy routine
         INTEGER, INTENT (in) :: kt
         WRITE(*,*) 'obc_dta: You should not have seen this print! error?', kt
      END SUBROUTINE obc_dta
#endif

   !!======================================================================
   END MODULE obcdta
