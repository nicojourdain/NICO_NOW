MODULE trcdta
   !!======================================================================
   !!                     ***  MODULE  trcdta  ***
   !! TOP :  reads passive tracer data 
   !!=====================================================================
   !! History :   1.0  !  2002-04  (O. Aumont)  original code
   !!              -   !  2004-03  (C. Ethe)  module
   !!              -   !  2005-03  (O. Aumont, A. El Moussaoui) F90
   !!            3.4   !  2010-11  (C. Ethe, G. Madec)  use of fldread + dynamical allocation 
   !!----------------------------------------------------------------------
#if  defined key_top 
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP model 
   !!----------------------------------------------------------------------
   !!   trc_dta    : read and time interpolated passive tracer data
   !!----------------------------------------------------------------------
   USE par_trc       !  passive tracers parameters
   USE oce_trc       !  shared variables between ocean and passive tracers
   USE trc           !  passive tracers common variables
   USE iom           !  I/O manager
   USE lib_mpp       !  MPP library
   USE fldread       !  read input fields

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_dta         ! called in trcini.F90 and trcdmp.F90
   PUBLIC   trc_dta_init    ! called in trcini.F90 

   INTEGER  , SAVE, PUBLIC                             :: nb_trcdta   ! number of tracers to be initialised with data
   INTEGER  , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)  :: n_trc_index ! indice of tracer which is initialised with data
   INTEGER  , SAVE                                     :: ntra        ! MAX( 1, nb_trcdta ) to avoid compilation error with bounds checking
   REAL(wp) , SAVE,         ALLOCATABLE, DIMENSION(:)  :: rf_trfac    ! multiplicative factor for tracer values
   TYPE(FLD), SAVE,         ALLOCATABLE, DIMENSION(:)  :: sf_trcdta   ! structure of input SST (file informations, fields read)

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: trcdta.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_dta_init
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_dta_init  ***
      !!                    
      !! ** Purpose :   initialisation of passive tracer input data 
      !! 
      !! ** Method  : - Read namtsd namelist
      !!              - allocates passive tracer data structure 
      !!----------------------------------------------------------------------
      !
      INTEGER            :: jl, jn                   ! dummy loop indicies
      INTEGER            :: ierr0, ierr1, ierr2, ierr3       ! temporary integers
      CHARACTER(len=100) :: clndta, clntrc
      REAL(wp)           :: zfact
      !
      CHARACTER(len=100) :: cn_dir
      TYPE(FLD_N), DIMENSION(jptra) :: slf_i     ! array of namelist informations on the fields to read
      TYPE(FLD_N), DIMENSION(jptra) :: sn_trcdta
      REAL(wp)   , DIMENSION(jptra) :: rn_trfac    ! multiplicative factor for tracer values
      !!
      NAMELIST/namtrc_dta/ sn_trcdta, cn_dir, rn_trfac 
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_dta_init')
      !
      !  Initialisation
      ierr0 = 0  ;  ierr1 = 0  ;  ierr2 = 0  ;  ierr3 = 0  
      ! Compute the number of tracers to be initialised with data
      ALLOCATE( n_trc_index(jptra), STAT=ierr0 )
      IF( ierr0 > 0 ) THEN
         CALL ctl_stop( 'trc_nam: unable to allocate n_trc_index' )   ;   RETURN
      ENDIF
      nb_trcdta      = 0
      n_trc_index(:) = 0
      DO jn = 1, jptra
         IF( ln_trc_ini(jn) ) THEN
             nb_trcdta       = nb_trcdta + 1 
             n_trc_index(jn) = nb_trcdta 
         ENDIF
      ENDDO
      !
      ntra = MAX( 1, nb_trcdta )   ! To avoid compilation error with bounds checking
      WRITE(numout,*) ' '
      WRITE(numout,*) ' number of passive tracers to be initialize by data :', ntra
      WRITE(numout,*) ' '
      !                         ! allocate the arrays (if necessary)
      !
      cn_dir  = './'            ! directory in which the model is executed
      DO jn = 1, jptra
         WRITE( clndta,'("TR_",I1)' ) jn
         clndta = TRIM( clndta )
         !                 !  file      ! frequency ! variable  ! time intep !  clim   ! 'yearly' or ! weights  ! rotation !
         !                 !  name      !  (hours)  !  name     !   (T/F)    !  (T/F)  !  'monthly'  ! filename ! pairs    !
         sn_trcdta(jn)  = FLD_N( clndta ,   -1      , clndta    ,  .false.   , .true.  ,  'monthly'  , ''       , ''       )
         !
         rn_trfac(jn) = 1._wp
      END DO
      !
      REWIND( numnat )               ! read nattrc
      READ  ( numnat, namtrc_dta )

      IF( lwp ) THEN
         DO jn = 1, jptra
            IF( ln_trc_ini(jn) )  THEN    ! open input file only if ln_trc_ini(jn) is true
               clndta = TRIM( sn_trcdta(jn)%clvar ) 
               clntrc = TRIM( ctrcnm   (jn)       ) 
               zfact  = rn_trfac(jn)
               IF( clndta /=  clntrc ) THEN 
                  CALL ctl_warn( 'trc_dta_init: passive tracer data initialisation :  ',   &
                  &              'the variable name in the data file : '//clndta//   & 
                  &              '  must be the same than the name of the passive tracer : '//clntrc//' ')
               ENDIF
               WRITE(numout,*) ' read an initial file for passive tracer number :', jn, ' name : ', clndta, & 
               &               ' multiplicative factor : ', zfact
            ENDIF
         END DO
      ENDIF
      !
      IF( nb_trcdta > 0 ) THEN       !  allocate only if the number of tracer to initialise is greater than zero
         ALLOCATE( sf_trcdta(nb_trcdta), rf_trfac(nb_trcdta), STAT=ierr1 )
         IF( ierr1 > 0 ) THEN
            CALL ctl_stop( 'trc_dta_ini: unable to allocate  sf_trcdta structure' )   ;   RETURN
         ENDIF
         !
         DO jn = 1, jptra
            IF( ln_trc_ini(jn) ) THEN      ! update passive tracers arrays with input data read from file
               jl = n_trc_index(jn)
               slf_i(jl)    = sn_trcdta(jn)
               rf_trfac(jl) = rn_trfac(jn)
                                            ALLOCATE( sf_trcdta(jl)%fnow(jpi,jpj,jpk)   , STAT=ierr2 )
               IF( sn_trcdta(jn)%ln_tint )  ALLOCATE( sf_trcdta(jl)%fdta(jpi,jpj,jpk,2) , STAT=ierr3 )
               IF( ierr2 + ierr3 > 0 ) THEN
                 CALL ctl_stop( 'trc_dta : unable to allocate passive tracer data arrays' )   ;   RETURN
               ENDIF
            ENDIF
            !   
         ENDDO
         !                         ! fill sf_trcdta with slf_i and control print
         CALL fld_fill( sf_trcdta, slf_i, cn_dir, 'trc_dta', 'Passive tracer data', 'namtrc' )
         !
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('trc_dta_init')
      !
   END SUBROUTINE trc_dta_init


   SUBROUTINE trc_dta( kt, ptrc )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_dta  ***
      !!                    
      !! ** Purpose :   provides passive tracer data at kt
      !! 
      !! ** Method  : - call fldread routine
      !!              - s- or mixed z-s coordinate: vertical interpolation on model mesh
      !!              - ln_trcdmp=F: deallocates the data structure as they are not used
      !!
      !! ** Action  :   ptrc   passive tracer data on medl mesh and interpolated at time-step kt
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   kt     ! ocean time-step
      REAL(wp), DIMENSION(:,:,:,:), INTENT(  out) ::   ptrc   ! passive tracer data
      !
      INTEGER ::   ji, jj, jk, jl, jn, jkk, ik    ! dummy loop indicies
      REAL(wp)::   zl, zi
      REAL(wp), DIMENSION(jpk) ::  ztp                ! 1D workspace
      CHARACTER(len=100) :: clndta
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_dta')
      !
      IF( nb_trcdta > 0 ) THEN
         !
         CALL fld_read( kt, 1, sf_trcdta )      !==   read data at kt time step   ==!
         !
         DO jn = 1, ntra
            ptrc(:,:,:,jn) = sf_trcdta(jn)%fnow(:,:,:)    ! NO mask
         ENDDO
         !
         IF( ln_sco ) THEN                   !==   s- or mixed s-zps-coordinate   ==!
            !
            IF( kt == nit000 .AND. lwp )THEN
               WRITE(numout,*)
               WRITE(numout,*) 'trc_dta: interpolates passive tracer data onto the s- or mixed s-z-coordinate mesh'
            ENDIF
            !
            DO jn = 1, ntra
               DO jj = 1, jpj                         ! vertical interpolation of T & S
                  DO ji = 1, jpi
                     DO jk = 1, jpk                        ! determines the intepolated T-S profiles at each (i,j) points
                        zl = fsdept_0(ji,jj,jk)
                        IF(     zl < gdept_0(1  ) ) THEN          ! above the first level of data
                           ztp(jk) =  ptrc(ji,jj,1    ,jn)
                        ELSEIF( zl > gdept_0(jpk) ) THEN          ! below the last level of data
                           ztp(jk) =  ptrc(ji,jj,jpkm1,jn)
                        ELSE                                      ! inbetween : vertical interpolation between jkk & jkk+1
                           DO jkk = 1, jpkm1                                  ! when  gdept(jkk) < zl < gdept(jkk+1)
                              IF( (zl-gdept_0(jkk)) * (zl-gdept_0(jkk+1)) <= 0._wp ) THEN
                                 zi = ( zl - gdept_0(jkk) ) / (gdept_0(jkk+1)-gdept_0(jkk))
                                 ztp(jk) = ptrc(ji,jj,jkk,jn) + ( ptrc(ji,jj,jkk+1,jn) - ptrc(ji,jj,jkk,jn) ) * zi 
                              ENDIF
                           END DO
                        ENDIF
                     END DO
                     DO jk = 1, jpkm1
                        ptrc(ji,jj,jk,jn) = ztp(jk) * tmask(ji,jj,jk)     ! mask required for mixed zps-s-coord
                     END DO
                     ptrc(ji,jj,jpk,jn) = 0._wp
                  END DO
               END DO
            ENDDO 
            ! 
         ELSE                                !==   z- or zps- coordinate   ==!
            !                             
            DO jn = 1, ntra
               ptrc(:,:,:,jn) = ptrc(:,:,:,jn) * tmask(:,:,:)    ! Mask
               !
               IF( ln_zps ) THEN                      ! zps-coordinate (partial steps) interpolation at the last ocean level
                  DO jj = 1, jpj
                     DO ji = 1, jpi
                        ik = mbkt(ji,jj) 
                        IF( ik > 1 ) THEN
                           zl = ( gdept_0(ik) - fsdept_0(ji,jj,ik) ) / ( gdept_0(ik) - gdept_0(ik-1) )
                           ptrc(ji,jj,ik,jn) = (1.-zl) * ptrc(ji,jj,ik,jn) + zl * ptrc(ji,jj,ik-1,jn)
                        ENDIF
                     END DO
                  END DO
               ENDIF
            ENDDO 
            !
         ENDIF
         !
         DO jn = 1, ntra
            ptrc(:,:,:,jn) = ptrc(:,:,:,jn) * rf_trfac(jn)   !  multiplicative factor
         ENDDO 
         !
         IF( lwp .AND. kt == nit000 ) THEN
            DO jn = 1, ntra
               clndta = TRIM( sf_trcdta(jn)%clvar ) 
               WRITE(numout,*) ''//clndta//' data '
               WRITE(numout,*)
               WRITE(numout,*)'  level = 1'
               CALL prihre( ptrc(:,:,1    ,jn), jpi, jpj, 1, jpi, 20, 1, jpj, 20, 1., numout )
               WRITE(numout,*)'  level = ', jpk/2
               CALL prihre( ptrc(:,:,jpk/2,jn), jpi, jpj, 1, jpi, 20, 1, jpj, 20, 1., numout )
               WRITE(numout,*)'  level = ', jpkm1
               CALL prihre( ptrc(:,:,jpkm1,jn), jpi, jpj, 1, jpi, 20, 1, jpj, 20, 1., numout )
               WRITE(numout,*)
            ENDDO
         ENDIF
         !
         IF( .NOT.ln_trcdmp ) THEN                   !==   deallocate data structure   ==! 
            !                                              (data used only for initialisation)
            IF(lwp) WRITE(numout,*) 'trc_dta: deallocate data arrays as they are only use to initialize the run'
            DO jn = 1, ntra
                                             DEALLOCATE( sf_trcdta(jn)%fnow )     !  arrays in the structure
               IF( sf_trcdta(jn)%ln_tint )   DEALLOCATE( sf_trcdta(jn)%fdta )
            ENDDO
                                             DEALLOCATE( sf_trcdta          )     ! the structure itself
            !
         ENDIF
         !
      ENDIF
      ! 
      IF( nn_timing == 1 )  CALL timing_stop('trc_dta')
      !
   END SUBROUTINE trc_dta
#else
   !!----------------------------------------------------------------------
   !!   Dummy module                              NO 3D passive tracer data
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_dta( kt )        ! Empty routine
      WRITE(*,*) 'trc_dta: You should not have seen this print! error?', kt
   END SUBROUTINE trc_dta
#endif
   !!======================================================================
END MODULE trcdta
