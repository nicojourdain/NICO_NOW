MODULE bdydta
   !!======================================================================
   !!                       ***  MODULE bdydta  ***
   !! Open boundary data : read the data for the unstructured open boundaries.
   !!======================================================================
   !! History :  1.0  !  2005-01  (J. Chanut, A. Sellar)  Original code
   !!             -   !  2007-01  (D. Storkey) Update to use IOM module
   !!             -   !  2007-07  (D. Storkey) add bdy_dta_fla
   !!            3.0  !  2008-04  (NEMO team)  add in the reference version
   !!            3.3  !  2010-09  (E.O'Dea) modifications for Shelf configurations 
   !!            3.3  !  2010-09  (D.Storkey) add ice boundary conditions
   !!            3.4  !  2011     (D. Storkey) rewrite in preparation for OBC-BDY merge
   !!----------------------------------------------------------------------
#if defined key_bdy
   !!----------------------------------------------------------------------
   !!   'key_bdy'                     Open Boundary Conditions
   !!----------------------------------------------------------------------
   !!    bdy_dta        : read external data along open boundaries from file
   !!    bdy_dta_init   : initialise arrays etc for reading of external data
   !!----------------------------------------------------------------------
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE bdy_oce         ! ocean open boundary conditions  
   USE bdytides        ! tidal forcing at boundaries
   USE fldread         ! read input fields
   USE iom             ! IOM library
   USE in_out_manager  ! I/O logical units
#if defined key_lim2
   USE ice_2
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC   bdy_dta          ! routine called by step.F90 and dynspg_ts.F90
   PUBLIC   bdy_dta_init     ! routine called by nemogcm.F90

   INTEGER, ALLOCATABLE, DIMENSION(:)   ::   nb_bdy_fld        ! Number of fields to update for each boundary set.
   INTEGER                              ::   nb_bdy_fld_sum    ! Total number of fields to update for all boundary sets.

   LOGICAL,           DIMENSION(jp_bdy) ::   ln_full_vel_array ! =T => full velocities in 3D boundary conditions
                                                               ! =F => baroclinic velocities in 3D boundary conditions

   TYPE(FLD), PUBLIC, ALLOCATABLE, DIMENSION(:), TARGET ::   bf        ! structure of input fields (file informations, fields read)

   TYPE(MAP_POINTER), ALLOCATABLE, DIMENSION(:) :: nbmap_ptr   ! array of pointers to nbmap

#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: bdydta.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

      SUBROUTINE bdy_dta( kt, jit, time_offset )
      !!----------------------------------------------------------------------
      !!                   ***  SUBROUTINE bdy_dta  ***
      !!                    
      !! ** Purpose :   Update external data for open boundary conditions
      !!
      !! ** Method  :   Use fldread.F90
      !!                
      !!----------------------------------------------------------------------
      !!
      INTEGER, INTENT( in )           ::   kt    ! ocean time-step index 
      INTEGER, INTENT( in ), OPTIONAL ::   jit   ! subcycle time-step index (for timesplitting option)
      INTEGER, INTENT( in ), OPTIONAL ::   time_offset  ! time offset in units of timesteps. NB. if jit
                                                        ! is present then units = subcycle timesteps.
                                                        ! time_offset = 0 => get data at "now" time level
                                                        ! time_offset = -1 => get data at "before" time level
                                                        ! time_offset = +1 => get data at "after" time level
                                                        ! etc.
      !!
      INTEGER     ::  ib_bdy, jfld, jstart, jend, ib, ii, ij, ik, igrd  ! local indices
      INTEGER,          DIMENSION(jpbgrd) ::   ilen1 
      INTEGER, POINTER, DIMENSION(:)      ::   nblen, nblenrim  ! short cuts
      !!
      !!---------------------------------------------------------------------------
      !!
      IF( nn_timing == 1 ) CALL timing_start('bdy_dta')

      ! Initialise data arrays once for all from initial conditions where required
      !---------------------------------------------------------------------------
      IF( kt .eq. nit000 .and. .not. PRESENT(jit) ) THEN

         ! Calculate depth-mean currents
         !-----------------------------
         CALL wrk_alloc(jpi,jpj,pu2d,pv2d) 

         pu2d(:,:) = 0.e0
         pv2d(:,:) = 0.e0

         DO ik = 1, jpkm1   !! Vertically integrated momentum trends
             pu2d(:,:) = pu2d(:,:) + fse3u(:,:,ik) * umask(:,:,ik) * un(:,:,ik)
             pv2d(:,:) = pv2d(:,:) + fse3v(:,:,ik) * vmask(:,:,ik) * vn(:,:,ik)
         END DO
         pu2d(:,:) = pu2d(:,:) * hur(:,:)
         pv2d(:,:) = pv2d(:,:) * hvr(:,:)
         
         DO ib_bdy = 1, nb_bdy

            nblen => idx_bdy(ib_bdy)%nblen
            nblenrim => idx_bdy(ib_bdy)%nblenrim

            IF( nn_dyn2d(ib_bdy) .gt. 0 .and. nn_dyn2d_dta(ib_bdy) .eq. 0 ) THEN 
               IF( nn_dyn2d(ib_bdy) .eq. jp_frs ) THEN
                  ilen1(:) = nblen(:)
               ELSE
                  ilen1(:) = nblenrim(:)
               ENDIF
               igrd = 1
               DO ib = 1, ilen1(igrd)
                  ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                  ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                  dta_bdy(ib_bdy)%ssh(ib) = sshn(ii,ij) * tmask(ii,ij,1)         
               END DO 
               igrd = 2
               DO ib = 1, ilen1(igrd)
                  ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                  ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                  dta_bdy(ib_bdy)%u2d(ib) = pu2d(ii,ij) * umask(ii,ij,1)         
               END DO 
               igrd = 3
               DO ib = 1, ilen1(igrd)
                  ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                  ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                  dta_bdy(ib_bdy)%v2d(ib) = pv2d(ii,ij) * vmask(ii,ij,1)         
               END DO 
            ENDIF

            IF( nn_dyn3d(ib_bdy) .gt. 0 .and. nn_dyn3d_dta(ib_bdy) .eq. 0 ) THEN 
               IF( nn_dyn3d(ib_bdy) .eq. jp_frs ) THEN
                  ilen1(:) = nblen(:)
               ELSE
                  ilen1(:) = nblenrim(:)
               ENDIF
               igrd = 2 
               DO ib = 1, ilen1(igrd)
                  DO ik = 1, jpkm1
                     ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                     ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                     dta_bdy(ib_bdy)%u3d(ib,ik) =  ( un(ii,ij,ik) - pu2d(ii,ij) ) * umask(ii,ij,ik)         
                  END DO
               END DO 
               igrd = 3 
               DO ib = 1, ilen1(igrd)
                  DO ik = 1, jpkm1
                     ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                     ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                     dta_bdy(ib_bdy)%v3d(ib,ik) =  ( vn(ii,ij,ik) - pv2d(ii,ij) ) * vmask(ii,ij,ik)         
                     END DO
               END DO 
            ENDIF

            IF( nn_tra(ib_bdy) .gt. 0 .and. nn_tra_dta(ib_bdy) .eq. 0 ) THEN 
               IF( nn_tra(ib_bdy) .eq. jp_frs ) THEN
                  ilen1(:) = nblen(:)
               ELSE
                  ilen1(:) = nblenrim(:)
               ENDIF
               igrd = 1                       ! Everything is at T-points here
               DO ib = 1, ilen1(igrd)
                  DO ik = 1, jpkm1
                     ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                     ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                     dta_bdy(ib_bdy)%tem(ib,ik) = tsn(ii,ij,ik,jp_tem) * tmask(ii,ij,ik)         
                     dta_bdy(ib_bdy)%sal(ib,ik) = tsn(ii,ij,ik,jp_sal) * tmask(ii,ij,ik)         
                  END DO
               END DO 
            ENDIF

#if defined key_lim2
            IF( nn_ice_lim2(ib_bdy) .gt. 0 .and. nn_ice_lim2_dta(ib_bdy) .eq. 0 ) THEN 
               IF( nn_ice_lim2(ib_bdy) .eq. jp_frs ) THEN
                  ilen1(:) = nblen(:)
               ELSE
                  ilen1(:) = nblenrim(:)
               ENDIF
               igrd = 1                       ! Everything is at T-points here
               DO ib = 1, ilen1(igrd)
                  ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                  ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                  dta_bdy(ib_bdy)%frld(ib) = frld(ii,ij) * tmask(ii,ij,1)         
                  dta_bdy(ib_bdy)%hicif(ib) = hicif(ii,ij) * tmask(ii,ij,1)         
                  dta_bdy(ib_bdy)%hsnif(ib) = hsnif(ii,ij) * tmask(ii,ij,1)         
               END DO 
            ENDIF
#endif

         ENDDO ! ib_bdy

         CALL wrk_dealloc(jpi,jpj,pu2d,pv2d) 

      ENDIF ! kt .eq. nit000

      ! update external data from files
      !--------------------------------
     
      jstart = 1
      DO ib_bdy = 1, nb_bdy   
         IF( nn_dta(ib_bdy) .eq. 1 ) THEN ! skip this bit if no external data required
      
            IF( PRESENT(jit) ) THEN
               ! Update barotropic boundary conditions only
               ! jit is optional argument for fld_read and tide_update
               IF( nn_dyn2d(ib_bdy) .gt. 0 ) THEN
                  IF( nn_dyn2d_dta(ib_bdy) .eq. 2 ) THEN ! tidal harmonic forcing ONLY: initialise arrays
                     dta_bdy(ib_bdy)%ssh(:) = 0.0
                     dta_bdy(ib_bdy)%u2d(:) = 0.0
                     dta_bdy(ib_bdy)%v2d(:) = 0.0
                  ENDIF
                  IF( nn_dyn2d_dta(ib_bdy) .eq. 1 .or. nn_dyn2d_dta(ib_bdy) .eq. 3 ) THEN ! update external data
                     jend = jstart + 2
                     CALL fld_read( kt=kt, kn_fsbc=1, sd=bf(jstart:jend), map=nbmap_ptr(jstart:jend),   &
                     &              jit=jit, time_offset=time_offset )
                  ENDIF
                  IF( nn_dyn2d_dta(ib_bdy) .ge. 2 ) THEN ! update tidal harmonic forcing
                     CALL tide_update( kt=kt, idx=idx_bdy(ib_bdy), dta=dta_bdy(ib_bdy), td=tides(ib_bdy),   & 
                     &                 jit=jit, time_offset=time_offset )
                  ENDIF
               ENDIF
            ELSE
               IF( nn_dyn2d(ib_bdy) .gt. 0 .and. nn_dyn2d_dta(ib_bdy) .eq. 2 ) THEN ! tidal harmonic forcing ONLY: initialise arrays
                  dta_bdy(ib_bdy)%ssh(:) = 0.0
                  dta_bdy(ib_bdy)%u2d(:) = 0.0
                  dta_bdy(ib_bdy)%v2d(:) = 0.0
               ENDIF
               IF( nb_bdy_fld(ib_bdy) .gt. 0 ) THEN ! update external data
                  jend = jstart + nb_bdy_fld(ib_bdy) - 1
                  CALL fld_read( kt=kt, kn_fsbc=1, sd=bf(jstart:jend), map=nbmap_ptr(jstart:jend), time_offset=time_offset )
               ENDIF
               IF( nn_dyn2d(ib_bdy) .gt. 0 .and. nn_dyn2d_dta(ib_bdy) .ge. 2 ) THEN ! update tidal harmonic forcing
                  CALL tide_update( kt=kt, idx=idx_bdy(ib_bdy), dta=dta_bdy(ib_bdy), td=tides(ib_bdy), time_offset=time_offset )
               ENDIF
            ENDIF
            jstart = jend+1

            ! If full velocities in boundary data then split into barotropic and baroclinic data
            ! (Note that we have already made sure that you can't use ln_full_vel = .true. at the same
            ! time as the dynspg_ts option). 

            IF( ln_full_vel_array(ib_bdy) .and.                                             & 
           &    ( nn_dyn2d_dta(ib_bdy) .eq. 1 .or. nn_dyn2d_dta(ib_bdy) .eq. 3 .or. nn_dyn3d_dta(ib_bdy) .eq. 1 ) ) THEN 

               igrd = 2                      ! zonal velocity
               dta_bdy(ib_bdy)%u2d(:) = 0.0
               DO ib = 1, idx_bdy(ib_bdy)%nblen(igrd)
                  ii   = idx_bdy(ib_bdy)%nbi(ib,igrd)
                  ij   = idx_bdy(ib_bdy)%nbj(ib,igrd)
                  DO ik = 1, jpkm1
                     dta_bdy(ib_bdy)%u2d(ib) = dta_bdy(ib_bdy)%u2d(ib) &
              &                                + fse3u(ii,ij,ik) * umask(ii,ij,ik) * dta_bdy(ib_bdy)%u3d(ib,ik)
                  END DO
                  dta_bdy(ib_bdy)%u2d(ib) =  dta_bdy(ib_bdy)%u2d(ib) * hur(ii,ij)
                  DO ik = 1, jpkm1
                     dta_bdy(ib_bdy)%u3d(ib,ik) = dta_bdy(ib_bdy)%u3d(ib,ik) - dta_bdy(ib_bdy)%u2d(ib) 
                  END DO
               END DO

               igrd = 3                      ! meridional velocity
               dta_bdy(ib_bdy)%v2d(:) = 0.0
               DO ib = 1, idx_bdy(ib_bdy)%nblen(igrd)
                  ii   = idx_bdy(ib_bdy)%nbi(ib,igrd)
                  ij   = idx_bdy(ib_bdy)%nbj(ib,igrd)
                  DO ik = 1, jpkm1
                     dta_bdy(ib_bdy)%v2d(ib) = dta_bdy(ib_bdy)%v2d(ib) &
              &                                + fse3v(ii,ij,ik) * vmask(ii,ij,ik) * dta_bdy(ib_bdy)%v3d(ib,ik)
                  END DO
                  dta_bdy(ib_bdy)%v2d(ib) =  dta_bdy(ib_bdy)%v2d(ib) * hvr(ii,ij)
                  DO ik = 1, jpkm1
                     dta_bdy(ib_bdy)%v3d(ib,ik) = dta_bdy(ib_bdy)%v3d(ib,ik) - dta_bdy(ib_bdy)%v2d(ib) 
                  END DO
               END DO
    
            ENDIF

         END IF ! nn_dta(ib_bdy) = 1
      END DO  ! ib_bdy

      IF( nn_timing == 1 ) CALL timing_stop('bdy_dta')

      END SUBROUTINE bdy_dta


      SUBROUTINE bdy_dta_init
      !!----------------------------------------------------------------------
      !!                   ***  SUBROUTINE bdy_dta_init  ***
      !!                    
      !! ** Purpose :   Initialise arrays for reading of external data 
      !!                for open boundary conditions
      !!
      !! ** Method  :   Use fldread.F90
      !!                
      !!----------------------------------------------------------------------
      USE dynspg_oce, ONLY: lk_dynspg_ts
      !!
      INTEGER     ::  ib_bdy, jfld, jstart, jend, ierror  ! local indices
      !!
      CHARACTER(len=100)                     ::   cn_dir        ! Root directory for location of data files
      CHARACTER(len=100), DIMENSION(nb_bdy)  ::   cn_dir_array  ! Root directory for location of data files
      LOGICAL                                ::   ln_full_vel   ! =T => full velocities in 3D boundary data
                                                                ! =F => baroclinic velocities in 3D boundary data
      INTEGER                                ::   ilen_global   ! Max length required for global bdy dta arrays
      INTEGER,              DIMENSION(jpbgrd) ::  ilen0         ! size of local arrays
      INTEGER, ALLOCATABLE, DIMENSION(:)     ::   ilen1, ilen3  ! size of 1st and 3rd dimensions of local arrays
      INTEGER, ALLOCATABLE, DIMENSION(:)     ::   ibdy           ! bdy set for a particular jfld
      INTEGER, ALLOCATABLE, DIMENSION(:)     ::   igrid         ! index for grid type (1,2,3 = T,U,V)
      INTEGER, POINTER, DIMENSION(:)         ::   nblen, nblenrim  ! short cuts
      TYPE(FLD_N), ALLOCATABLE, DIMENSION(:) ::   blf_i         !  array of namelist information structures
      TYPE(FLD_N) ::   bn_tem, bn_sal, bn_u3d, bn_v3d   ! 
      TYPE(FLD_N) ::   bn_ssh, bn_u2d, bn_v2d           ! informations about the fields to be read
#if defined key_lim2
      TYPE(FLD_N) ::   bn_frld, bn_hicif, bn_hsnif      !
#endif
      NAMELIST/nambdy_dta/ cn_dir, bn_tem, bn_sal, bn_u3d, bn_v3d, bn_ssh, bn_u2d, bn_v2d 
#if defined key_lim2
      NAMELIST/nambdy_dta/ bn_frld, bn_hicif, bn_hsnif
#endif
      NAMELIST/nambdy_dta/ ln_full_vel
      !!---------------------------------------------------------------------------

      IF( nn_timing == 1 ) CALL timing_start('bdy_dta_init')

      ! Set nn_dta
      DO ib_bdy = 1, nb_bdy
         nn_dta(ib_bdy) = MAX(  nn_dyn2d_dta(ib_bdy)       &
                               ,nn_dyn3d_dta(ib_bdy)       &
                               ,nn_tra_dta(ib_bdy)         &
#if defined key_lim2
                               ,nn_ice_lim2_dta(ib_bdy)    &
#endif
                              )
         IF(nn_dta(ib_bdy) .gt. 1) nn_dta(ib_bdy) = 1
      END DO

      ! Work out upper bound of how many fields there are to read in and allocate arrays
      ! ---------------------------------------------------------------------------
      ALLOCATE( nb_bdy_fld(nb_bdy) )
      nb_bdy_fld(:) = 0
      DO ib_bdy = 1, nb_bdy         
         IF( nn_dyn2d(ib_bdy) .gt. 0 .and. ( nn_dyn2d_dta(ib_bdy) .eq. 1 .or. nn_dyn2d_dta(ib_bdy) .eq. 3 ) ) THEN
            nb_bdy_fld(ib_bdy) = nb_bdy_fld(ib_bdy) + 3
         ENDIF
         IF( nn_dyn3d(ib_bdy) .gt. 0 .and. nn_dyn3d_dta(ib_bdy) .eq. 1 ) THEN
            nb_bdy_fld(ib_bdy) = nb_bdy_fld(ib_bdy) + 2
         ENDIF
         IF( nn_tra(ib_bdy) .gt. 0 .and. nn_tra_dta(ib_bdy) .eq. 1  ) THEN
            nb_bdy_fld(ib_bdy) = nb_bdy_fld(ib_bdy) + 2
         ENDIF
#if defined key_lim2
         IF( nn_ice_lim2(ib_bdy) .gt. 0 .and. nn_ice_lim2_dta(ib_bdy) .eq. 1  ) THEN
            nb_bdy_fld(ib_bdy) = nb_bdy_fld(ib_bdy) + 3
         ENDIF
#endif               
      ENDDO            

      nb_bdy_fld_sum = SUM( nb_bdy_fld )

      ALLOCATE( bf(nb_bdy_fld_sum), STAT=ierror )
      IF( ierror > 0 ) THEN   
         CALL ctl_stop( 'bdy_dta: unable to allocate bf structure' )   ;   RETURN  
      ENDIF
      ALLOCATE( blf_i(nb_bdy_fld_sum), STAT=ierror )
      IF( ierror > 0 ) THEN   
         CALL ctl_stop( 'bdy_dta: unable to allocate blf_i structure' )   ;   RETURN  
      ENDIF
      ALLOCATE( nbmap_ptr(nb_bdy_fld_sum), STAT=ierror )
      IF( ierror > 0 ) THEN   
         CALL ctl_stop( 'bdy_dta: unable to allocate nbmap_ptr structure' )   ;   RETURN  
      ENDIF
      ALLOCATE( ilen1(nb_bdy_fld_sum), ilen3(nb_bdy_fld_sum) ) 
      ALLOCATE( ibdy(nb_bdy_fld_sum) ) 
      ALLOCATE( igrid(nb_bdy_fld_sum) ) 

      ! Read namelists
      ! --------------
      REWIND(numnam)
      jfld = 0 
      DO ib_bdy = 1, nb_bdy         
         IF( nn_dta(ib_bdy) .eq. 1 ) THEN
            ! set file information
            cn_dir = './'        ! directory in which the model is executed
            ln_full_vel = .false.
            ! ... default values (NB: frequency positive => hours, negative => months)
            !                    !  file       ! frequency !  variable   ! time intep !  clim   ! 'yearly' or ! weights  ! rotation  !
            !                    !  name       ! hours !   name     !  (T/F)  !  (T/F)  !  'monthly'  ! filename ! pairs     !
            bn_ssh     = FLD_N(  'bdy_ssh'     ,  24   , 'sossheig' , .false. , .false. ,   'yearly'  , ''       , ''        )
            bn_u2d     = FLD_N(  'bdy_vel2d_u' ,  24   , 'vobtcrtx' , .false. , .false. ,   'yearly'  , ''       , ''        )
            bn_v2d     = FLD_N(  'bdy_vel2d_v' ,  24   , 'vobtcrty' , .false. , .false. ,   'yearly'  , ''       , ''        )
            bn_u3d     = FLD_N(  'bdy_vel3d_u' ,  24   , 'vozocrtx' , .false. , .false. ,   'yearly'  , ''       , ''        )
            bn_v3d     = FLD_N(  'bdy_vel3d_v' ,  24   , 'vomecrty' , .false. , .false. ,   'yearly'  , ''       , ''        )
            bn_tem     = FLD_N(  'bdy_tem'     ,  24   , 'votemper' , .false. , .false. ,   'yearly'  , ''       , ''        )
            bn_sal     = FLD_N(  'bdy_sal'     ,  24   , 'vosaline' , .false. , .false. ,   'yearly'  , ''       , ''        )
#if defined key_lim2
            bn_frld    = FLD_N(  'bdy_frld'    ,  24   , 'ildsconc' , .false. , .false. ,   'yearly'  , ''       , ''        )
            bn_hicif   = FLD_N(  'bdy_hicif'   ,  24   , 'iicethic' , .false. , .false. ,   'yearly'  , ''       , ''        )
            bn_hsnif   = FLD_N(  'bdy_hsnif'   ,  24   , 'isnothic' , .false. , .false. ,   'yearly'  , ''       , ''        )
#endif

            ! Important NOT to rewind here.
            READ( numnam, nambdy_dta )

            cn_dir_array(ib_bdy) = cn_dir
            ln_full_vel_array(ib_bdy) = ln_full_vel

            IF( ln_full_vel_array(ib_bdy) .and. lk_dynspg_ts )  THEN
               CALL ctl_stop( 'bdy_dta_init: ERROR, cannot specify full velocities in boundary data',&
            &                  'with dynspg_ts option' )   ;   RETURN  
            ENDIF             

            nblen => idx_bdy(ib_bdy)%nblen
            nblenrim => idx_bdy(ib_bdy)%nblenrim

            ! Only read in necessary fields for this set.
            ! Important that barotropic variables come first.
            IF( nn_dyn2d(ib_bdy) .gt. 0 .and. ( nn_dyn2d_dta(ib_bdy) .eq. 1 .or. nn_dyn2d_dta(ib_bdy) .eq. 3 ) ) THEN 

               IF( nn_dyn2d(ib_bdy) .ne. jp_frs ) THEN
                  jfld = jfld + 1
                  blf_i(jfld) = bn_ssh
                  ibdy(jfld) = ib_bdy
                  igrid(jfld) = 1
                  ilen1(jfld) = nblenrim(igrid(jfld))
                  ilen3(jfld) = 1
               ENDIF

               IF( .not. ln_full_vel_array(ib_bdy) ) THEN

                  jfld = jfld + 1
                  blf_i(jfld) = bn_u2d
                  ibdy(jfld) = ib_bdy
                  igrid(jfld) = 2
                  IF( nn_dyn2d(ib_bdy) .eq. jp_frs ) THEN
                     ilen1(jfld) = nblen(igrid(jfld))
                  ELSE
                     ilen1(jfld) = nblenrim(igrid(jfld))
                  ENDIF
                  ilen3(jfld) = 1

                  jfld = jfld + 1
                  blf_i(jfld) = bn_v2d
                  ibdy(jfld) = ib_bdy
                  igrid(jfld) = 3
                  IF( nn_dyn2d(ib_bdy) .eq. jp_frs ) THEN
                     ilen1(jfld) = nblen(igrid(jfld))
                  ELSE
                     ilen1(jfld) = nblenrim(igrid(jfld))
                  ENDIF
                  ilen3(jfld) = 1

               ENDIF

            ENDIF

            ! baroclinic velocities
            IF( ( nn_dyn3d(ib_bdy) .gt. 0 .and. nn_dyn3d_dta(ib_bdy) .eq. 1 ) .or. &
           &      ( ln_full_vel_array(ib_bdy) .and. nn_dyn2d(ib_bdy) .gt. 0 .and.  &
           &        ( nn_dyn2d_dta(ib_bdy) .eq. 1 .or. nn_dyn2d_dta(ib_bdy) .eq. 3 ) ) ) THEN

               jfld = jfld + 1
               blf_i(jfld) = bn_u3d
               ibdy(jfld) = ib_bdy
               igrid(jfld) = 2
               IF( nn_dyn3d(ib_bdy) .eq. jp_frs ) THEN
                  ilen1(jfld) = nblen(igrid(jfld))
               ELSE
                  ilen1(jfld) = nblenrim(igrid(jfld))
               ENDIF
               ilen3(jfld) = jpk

               jfld = jfld + 1
               blf_i(jfld) = bn_v3d
               ibdy(jfld) = ib_bdy
               igrid(jfld) = 3
               IF( nn_dyn3d(ib_bdy) .eq. jp_frs ) THEN
                  ilen1(jfld) = nblen(igrid(jfld))
               ELSE
                  ilen1(jfld) = nblenrim(igrid(jfld))
               ENDIF
               ilen3(jfld) = jpk

            ENDIF

            ! temperature and salinity
            IF( nn_tra(ib_bdy) .gt. 0 .and. nn_tra_dta(ib_bdy) .eq. 1 ) THEN

               jfld = jfld + 1
               blf_i(jfld) = bn_tem
               ibdy(jfld) = ib_bdy
               igrid(jfld) = 1
               IF( nn_tra(ib_bdy) .eq. jp_frs ) THEN
                  ilen1(jfld) = nblen(igrid(jfld))
               ELSE
                  ilen1(jfld) = nblenrim(igrid(jfld))
               ENDIF
               ilen3(jfld) = jpk

               jfld = jfld + 1
               blf_i(jfld) = bn_sal
               ibdy(jfld) = ib_bdy
               igrid(jfld) = 1
               IF( nn_tra(ib_bdy) .eq. jp_frs ) THEN
                  ilen1(jfld) = nblen(igrid(jfld))
               ELSE
                  ilen1(jfld) = nblenrim(igrid(jfld))
               ENDIF
               ilen3(jfld) = jpk

            ENDIF

#if defined key_lim2
            ! sea ice
            IF( nn_ice_lim2(ib_bdy) .gt. 0 .and. nn_ice_lim2_dta(ib_bdy) .eq. 1 ) THEN

               jfld = jfld + 1
               blf_i(jfld) = bn_frld
               ibdy(jfld) = ib_bdy
               igrid(jfld) = 1
               IF( nn_ice_lim2(ib_bdy) .eq. jp_frs ) THEN
                  ilen1(jfld) = nblen(igrid(jfld))
               ELSE
                  ilen1(jfld) = nblenrim(igrid(jfld))
               ENDIF
               ilen3(jfld) = 1

               jfld = jfld + 1
               blf_i(jfld) = bn_hicif
               ibdy(jfld) = ib_bdy
               igrid(jfld) = 1
               IF( nn_ice_lim2(ib_bdy) .eq. jp_frs ) THEN
                  ilen1(jfld) = nblen(igrid(jfld))
               ELSE
                  ilen1(jfld) = nblenrim(igrid(jfld))
               ENDIF
               ilen3(jfld) = 1

               jfld = jfld + 1
               blf_i(jfld) = bn_hsnif
               ibdy(jfld) = ib_bdy
               igrid(jfld) = 1
               IF( nn_ice_lim2(ib_bdy) .eq. jp_frs ) THEN
                  ilen1(jfld) = nblen(igrid(jfld))
               ELSE
                  ilen1(jfld) = nblenrim(igrid(jfld))
               ENDIF
               ilen3(jfld) = 1

            ENDIF
#endif
            ! Recalculate field counts
            !-------------------------
            nb_bdy_fld_sum = 0
            IF( ib_bdy .eq. 1 ) THEN 
               nb_bdy_fld(ib_bdy) = jfld
               nb_bdy_fld_sum     = jfld              
            ELSE
               nb_bdy_fld(ib_bdy) = jfld - nb_bdy_fld_sum
               nb_bdy_fld_sum = nb_bdy_fld_sum + nb_bdy_fld(ib_bdy)
            ENDIF

         ENDIF ! nn_dta .eq. 1
      ENDDO ! ib_bdy


      DO jfld = 1, nb_bdy_fld_sum
         ALLOCATE( bf(jfld)%fnow(ilen1(jfld),1,ilen3(jfld)) )
         IF( blf_i(jfld)%ln_tint ) ALLOCATE( bf(jfld)%fdta(ilen1(jfld),1,ilen3(jfld),2) )
         nbmap_ptr(jfld)%ptr => idx_bdy(ibdy(jfld))%nbmap(:,igrid(jfld))
      ENDDO

      ! fill bf with blf_i and control print
      !-------------------------------------
      jstart = 1
      DO ib_bdy = 1, nb_bdy
         jend = jstart + nb_bdy_fld(ib_bdy) - 1
         CALL fld_fill( bf(jstart:jend), blf_i(jstart:jend), cn_dir_array(ib_bdy), 'bdy_dta',   &
         &              'open boundary conditions', 'nambdy_dta' )
         jstart = jend + 1
      ENDDO

      ! Initialise local boundary data arrays
      ! nn_xxx_dta=0 : allocate space - will be filled from initial conditions later
      ! nn_xxx_dta=1 : point to "fnow" arrays
      !-------------------------------------

      jfld = 0
      DO ib_bdy=1, nb_bdy

         nblen => idx_bdy(ib_bdy)%nblen
         nblenrim => idx_bdy(ib_bdy)%nblenrim

         IF (nn_dyn2d(ib_bdy) .gt. 0) THEN
            IF( nn_dyn2d_dta(ib_bdy) .eq. 0 .or. nn_dyn2d_dta(ib_bdy) .eq. 2 .or. ln_full_vel_array(ib_bdy) ) THEN
               IF( nn_dyn2d(ib_bdy) .eq. jp_frs ) THEN
                  ilen0(1:3) = nblen(1:3)
               ELSE
                  ilen0(1:3) = nblenrim(1:3)
               ENDIF
               ALLOCATE( dta_bdy(ib_bdy)%ssh(ilen0(1)) )
               ALLOCATE( dta_bdy(ib_bdy)%u2d(ilen0(2)) )
               ALLOCATE( dta_bdy(ib_bdy)%v2d(ilen0(3)) )
            ELSE
               IF( nn_dyn2d(ib_bdy) .ne. jp_frs ) THEN
                  jfld = jfld + 1
                  dta_bdy(ib_bdy)%ssh => bf(jfld)%fnow(:,1,1)
               ENDIF
               jfld = jfld + 1
               dta_bdy(ib_bdy)%u2d => bf(jfld)%fnow(:,1,1)
               jfld = jfld + 1
               dta_bdy(ib_bdy)%v2d => bf(jfld)%fnow(:,1,1)
            ENDIF
         ENDIF

         IF ( nn_dyn3d(ib_bdy) .gt. 0 .and. nn_dyn3d_dta(ib_bdy) .eq. 0 ) THEN
            IF( nn_dyn3d(ib_bdy) .eq. jp_frs ) THEN
               ilen0(1:3) = nblen(1:3)
            ELSE
               ilen0(1:3) = nblenrim(1:3)
            ENDIF
            ALLOCATE( dta_bdy(ib_bdy)%u3d(ilen0(2),jpk) )
            ALLOCATE( dta_bdy(ib_bdy)%v3d(ilen0(3),jpk) )
         ENDIF
         IF ( ( nn_dyn3d(ib_bdy) .gt. 0 .and. nn_dyn3d_dta(ib_bdy) .eq. 1 ).or. &
           &  ( ln_full_vel_array(ib_bdy) .and. nn_dyn2d(ib_bdy) .gt. 0 .and.   &
           &    ( nn_dyn2d_dta(ib_bdy) .eq. 1 .or. nn_dyn2d_dta(ib_bdy) .eq. 3 ) ) ) THEN
            jfld = jfld + 1
            dta_bdy(ib_bdy)%u3d => bf(jfld)%fnow(:,1,:)
            jfld = jfld + 1
            dta_bdy(ib_bdy)%v3d => bf(jfld)%fnow(:,1,:)
         ENDIF

         IF (nn_tra(ib_bdy) .gt. 0) THEN
            IF( nn_tra_dta(ib_bdy) .eq. 0 ) THEN
               IF( nn_tra(ib_bdy) .eq. jp_frs ) THEN
                  ilen0(1:3) = nblen(1:3)
               ELSE
                  ilen0(1:3) = nblenrim(1:3)
               ENDIF
               ALLOCATE( dta_bdy(ib_bdy)%tem(ilen0(1),jpk) )
               ALLOCATE( dta_bdy(ib_bdy)%sal(ilen0(1),jpk) )
            ELSE
               jfld = jfld + 1
               dta_bdy(ib_bdy)%tem => bf(jfld)%fnow(:,1,:)
               jfld = jfld + 1
               dta_bdy(ib_bdy)%sal => bf(jfld)%fnow(:,1,:)
            ENDIF
         ENDIF

#if defined key_lim2
         IF (nn_ice_lim2(ib_bdy) .gt. 0) THEN
            IF( nn_ice_lim2_dta(ib_bdy) .eq. 0 ) THEN
               IF( nn_ice_lim2(ib_bdy) .eq. jp_frs ) THEN
                  ilen0(1:3) = nblen(1:3)
               ELSE
                  ilen0(1:3) = nblenrim(1:3)
               ENDIF
               ALLOCATE( dta_bdy(ib_bdy)%frld(ilen0(1)) )
               ALLOCATE( dta_bdy(ib_bdy)%hicif(ilen0(1)) )
               ALLOCATE( dta_bdy(ib_bdy)%hsnif(ilen0(1)) )
            ELSE
               jfld = jfld + 1
               dta_bdy(ib_bdy)%frld  => bf(jfld)%fnow(:,1,1)
               jfld = jfld + 1
               dta_bdy(ib_bdy)%hicif => bf(jfld)%fnow(:,1,1)
               jfld = jfld + 1
               dta_bdy(ib_bdy)%hsnif => bf(jfld)%fnow(:,1,1)
            ENDIF
         ENDIF
#endif

      ENDDO ! ib_bdy 

      IF( nn_timing == 1 ) CALL timing_stop('bdy_dta_init')

      END SUBROUTINE bdy_dta_init

#else
   !!----------------------------------------------------------------------
   !!   Dummy module                   NO Open Boundary Conditions
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE bdy_dta( kt, jit, time_offset ) ! Empty routine
      INTEGER, INTENT( in )           ::   kt    
      INTEGER, INTENT( in ), OPTIONAL ::   jit   
      INTEGER, INTENT( in ), OPTIONAL ::   time_offset
      WRITE(*,*) 'bdy_dta: You should not have seen this print! error?', kt
   END SUBROUTINE bdy_dta
   SUBROUTINE bdy_dta_init()                  ! Empty routine
      WRITE(*,*) 'bdy_dta_init: You should not have seen this print! error?'
   END SUBROUTINE bdy_dta_init
#endif

   !!==============================================================================
END MODULE bdydta
