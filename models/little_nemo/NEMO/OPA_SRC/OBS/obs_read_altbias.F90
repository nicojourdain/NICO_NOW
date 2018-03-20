MODULE obs_read_altbias
   !!======================================================================
   !!                       ***  MODULE obs_readaltbias  ***
   !! Observation diagnostics: Read the bias for SLA data
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   obs_rea_altbias : Driver for reading altimeter bias
   !!----------------------------------------------------------------------

   !! * Modules used   
   USE par_kind, ONLY : &       ! Precision variables
      & wp, &
      & dp, &
      & sp
   USE par_oce, ONLY : &        ! Domain parameters
      & jpi, &
      & jpj, &
      & jpim1
   USE in_out_manager, ONLY : & ! I/O manager
      & lwp,    &
      & numout 
   USE obs_surf_def             ! Surface observation definitions
   USE dom_oce, ONLY : &        ! Domain variables
      & tmask, &
      & tmask_i, &
      & e1t,   &
      & e2t,   &
      & gphit
   USE oce, ONLY : &           ! Model variables
      & sshn
   USE obs_inter_h2d
   USE obs_utils               ! Various observation tools
   USE obs_inter_sup
   USE wrk_nemo                ! Memory Allocation

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE

   PUBLIC obs_rea_altbias     ! Read the altimeter bias

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_read_altbias.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE obs_rea_altbias( kslano, sladata, k2dint, bias_file )
      !!---------------------------------------------------------------------
      !!
      !!                   *** ROUTINE obs_rea_altbias ***
      !!
      !! ** Purpose : Read from file the bias data 
      !!
      !! ** Method  : 
      !!
      !! ** Action  : 
      !!
      !! References :
      !!
      !! History :  
      !!      ! :  2008-02 (D. Lea) Initial version
      !!----------------------------------------------------------------------
      !! * Modules used
      USE iom
      !
      !! * Arguments
      INTEGER, INTENT(IN) :: kslano      ! Number of SLA Products
      TYPE(obs_surf), DIMENSION(kslano), INTENT(INOUT) :: &
         & sladata       ! SLA data
      INTEGER, INTENT(IN) :: k2dint
      CHARACTER(LEN=128) :: bias_file

      !! * Local declarations

      CHARACTER(LEN=12), PARAMETER :: cpname = 'obs_rea_altbias'

      INTEGER :: jslano       ! Data set loop variable
      INTEGER :: jobs         ! Obs loop variable
      INTEGER :: jpialtbias   ! Number of grid point in latitude for the bias
      INTEGER :: jpjaltbias   ! Number of grid point in longitude for the bias
      INTEGER :: iico         ! Grid point indicies
      INTEGER :: ijco
      INTEGER :: i_nx_id      ! Index to read the NetCDF file
      INTEGER :: i_ny_id      ! 
      INTEGER :: i_file_id    ! 
      INTEGER :: i_var_id

      REAL(wp), DIMENSION(1) :: &
         & zext, &
         & zobsmask
      REAL(wp), DIMENSION(2,2,1) :: &
         & zweig
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: &
         & zmask, &
         & zbias, &
         & zglam, &
         & zgphi
      REAL(wp), POINTER, DIMENSION(:,:) ::   z_altbias
      REAL(wp) :: zlam
      REAL(wp) :: zphi
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: &
         & igrdi, &
         & igrdj
      INTEGER :: numaltbias

      CALL wrk_alloc(jpi,jpj,z_altbias) 

      IF(lwp)WRITE(numout,*) 
      IF(lwp)WRITE(numout,*) ' obs_rea_altbias : '
      IF(lwp)WRITE(numout,*) ' ------------- '
      IF(lwp)WRITE(numout,*) '   Read altimeter bias'

      ! Open the file

      z_altbias(:,:)=0.0_wp
      numaltbias=0

      IF(lwp)WRITE(numout,*) 'Opening ',bias_file

      CALL iom_open( bias_file, numaltbias, ldstop=.FALSE. )
      

      IF (numaltbias .GT. 0) THEN      

         ! Get the Alt bias data
         
         CALL iom_get( numaltbias, jpdom_data, 'altbias', z_altbias(:,:), 1 )
         
         ! Close the file
         
         CALL iom_close(numaltbias)     
         
      ELSE

         IF(lwp)WRITE(numout,*) 'no file found'
      
      ENDIF

      ! Intepolate the bias already on the model grid at the observation point
  
      DO jslano = 1, kslano

         ALLOCATE( &
            & igrdi(2,2,sladata(jslano)%nsurf), &
            & igrdj(2,2,sladata(jslano)%nsurf), &
            & zglam(2,2,sladata(jslano)%nsurf), &
            & zgphi(2,2,sladata(jslano)%nsurf), &
            & zmask(2,2,sladata(jslano)%nsurf), &
            & zbias(2,2,sladata(jslano)%nsurf)  &
            & )
         
         DO jobs = 1, sladata(jslano)%nsurf

            igrdi(1,1,jobs) = sladata(jslano)%mi(jobs)-1
            igrdj(1,1,jobs) = sladata(jslano)%mj(jobs)-1
            igrdi(1,2,jobs) = sladata(jslano)%mi(jobs)-1
            igrdj(1,2,jobs) = sladata(jslano)%mj(jobs)
            igrdi(2,1,jobs) = sladata(jslano)%mi(jobs)
            igrdj(2,1,jobs) = sladata(jslano)%mj(jobs)-1
            igrdi(2,2,jobs) = sladata(jslano)%mi(jobs)
            igrdj(2,2,jobs) = sladata(jslano)%mj(jobs)

         END DO

         CALL obs_int_comm_2d( 2, 2, sladata(jslano)%nsurf, &
            &                  igrdi, igrdj, glamt, zglam )
         CALL obs_int_comm_2d( 2, 2, sladata(jslano)%nsurf, &
            &                  igrdi, igrdj, gphit, zgphi )
         CALL obs_int_comm_2d( 2, 2, sladata(jslano)%nsurf, &
            &                  igrdi, igrdj, tmask(:,:,1), zmask )
         CALL obs_int_comm_2d( 2, 2, sladata(jslano)%nsurf, &
            &                  igrdi, igrdj, z_altbias, zbias )

         DO jobs = 1, sladata(jslano)%nsurf

            zlam = sladata(jslano)%rlam(jobs)
            zphi = sladata(jslano)%rphi(jobs)
            iico = sladata(jslano)%mi(jobs)
            ijco = sladata(jslano)%mj(jobs)
            
            CALL obs_int_h2d_init( 1, 1, k2dint, zlam, zphi,         &
               &                   zglam(:,:,jobs), zgphi(:,:,jobs), &
               &                   zmask(:,:,jobs), zweig, zobsmask )
            
            CALL obs_int_h2d( 1, 1,      &
               &              zweig, zbias(:,:,jobs),  zext )

            ! adjust mdt with bias field
            sladata(jslano)%rext(jobs,2) = &
               sladata(jslano)%rext(jobs,2) - zext(1)
            
         END DO

         DEALLOCATE( &
            & igrdi, &
            & igrdj, &
            & zglam, &
            & zgphi, &
            & zmask, &
            & zbias  &
            & )
         
      END DO

      CALL wrk_dealloc(jpi,jpj,z_altbias) 

   END SUBROUTINE obs_rea_altbias


 
END MODULE obs_read_altbias
