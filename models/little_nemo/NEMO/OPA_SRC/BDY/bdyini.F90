MODULE bdyini
   !!======================================================================
   !!                       ***  MODULE  bdyini  ***
   !! Unstructured open boundaries : initialisation
   !!======================================================================
   !! History :  1.0  !  2005-01  (J. Chanut, A. Sellar)  Original code
   !!             -   !  2007-01  (D. Storkey) Update to use IOM module
   !!             -   !  2007-01  (D. Storkey) Tidal forcing
   !!            3.0  !  2008-04  (NEMO team)  add in the reference version
   !!            3.3  !  2010-09  (E.O'Dea) updates for Shelf configurations
   !!            3.3  !  2010-09  (D.Storkey) add ice boundary conditions
   !!            3.4  !  2011     (D. Storkey) rewrite in preparation for OBC-BDY merge
   !!----------------------------------------------------------------------
#if defined key_bdy
   !!----------------------------------------------------------------------
   !!   'key_bdy'                     Unstructured Open Boundary Conditions
   !!----------------------------------------------------------------------
   !!   bdy_init       : Initialization of unstructured open boundaries
   !!----------------------------------------------------------------------
   USE timing          ! Timing
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain
   USE bdy_oce         ! unstructured open boundary conditions
   USE in_out_manager  ! I/O units
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp         ! for mpp_sum  
   USE iom             ! I/O

   IMPLICIT NONE
   PRIVATE

   PUBLIC   bdy_init   ! routine called in nemo_init

   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: bdyini.F90 3298 2012-02-07 17:12:09Z cbricaud $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS
   
   SUBROUTINE bdy_init
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE bdy_init  ***
      !!         
      !! ** Purpose :   Initialization of the dynamics and tracer fields with 
      !!              unstructured open boundaries.
      !!
      !! ** Method  :   Read initialization arrays (mask, indices) to identify 
      !!              an unstructured open boundary
      !!
      !! ** Input   :  bdy_init.nc, input file for unstructured open boundaries
      !!----------------------------------------------------------------------      
      ! namelist variables
      !-------------------
      INTEGER, PARAMETER          :: jp_nseg = 100
      INTEGER                     :: nbdysege, nbdysegw, nbdysegn, nbdysegs 
      INTEGER, DIMENSION(jp_nseg) :: jpieob, jpjedt, jpjeft
      INTEGER, DIMENSION(jp_nseg) :: jpiwob, jpjwdt, jpjwft
      INTEGER, DIMENSION(jp_nseg) :: jpjnob, jpindt, jpinft
      INTEGER, DIMENSION(jp_nseg) :: jpjsob, jpisdt, jpisft

      ! local variables
      !-------------------
      INTEGER  ::   ib_bdy, ii, ij, ik, igrd, ib, ir, iseg ! dummy loop indices
      INTEGER  ::   icount, icountr, ibr_max, ilen1, ibm1  ! local integers
      INTEGER  ::   iw, ie, is, in, inum, id_dummy         !   -       -
      INTEGER  ::   igrd_start, igrd_end, jpbdta           !   -       -
      INTEGER, POINTER  ::  nbi, nbj, nbr                  ! short cuts
      REAL   , POINTER  ::  flagu, flagv                   !    -   -
      REAL(wp) ::   zefl, zwfl, znfl, zsfl                 ! local scalars
      INTEGER, DIMENSION (2)                ::   kdimsz
      INTEGER, DIMENSION(jpbgrd,jp_bdy)       ::   nblendta         ! Length of index arrays 
      INTEGER, ALLOCATABLE, DIMENSION(:,:,:)  ::   nbidta, nbjdta   ! Index arrays: i and j indices of bdy dta
      INTEGER, ALLOCATABLE, DIMENSION(:,:,:)  ::   nbrdta           ! Discrete distance from rim points
      REAL(wp), DIMENSION(jpidta,jpjdta)    ::   zmask            ! global domain mask
      CHARACTER(LEN=80),DIMENSION(jpbgrd)  ::   clfile
      CHARACTER(LEN=1),DIMENSION(jpbgrd)   ::   cgrid
      !!
      NAMELIST/nambdy/ nb_bdy, ln_coords_file, cn_coords_file,             &
         &             ln_mask_file, cn_mask_file, nn_dyn2d, nn_dyn2d_dta, &
         &             nn_dyn3d, nn_dyn3d_dta, nn_tra, nn_tra_dta,         &  
#if defined key_lim2
         &             nn_ice_lim2, nn_ice_lim2_dta,                       &
#endif
         &             ln_vol, nn_volctl, nn_rimwidth
      !!
      NAMELIST/nambdy_index/ nbdysege, jpieob, jpjedt, jpjeft,             &
                             nbdysegw, jpiwob, jpjwdt, jpjwft,             &
                             nbdysegn, jpjnob, jpindt, jpinft,             &
                             nbdysegs, jpjsob, jpisdt, jpisft

      !!----------------------------------------------------------------------

      IF( nn_timing == 1 ) CALL timing_start('bdy_init')

      IF( bdy_oce_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'bdy_init : unable to allocate oce arrays' )

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'bdy_init : initialization of open boundaries'
      IF(lwp) WRITE(numout,*) '~~~~~~~~'
      !

      IF( jperio /= 0 )   CALL ctl_stop( 'Cyclic or symmetric,',   &
         &                               ' and general open boundary condition are not compatible' )

      cgrid= (/'t','u','v'/)

      ! -----------------------------------------
      ! Initialise and read namelist parameters
      ! -----------------------------------------

      nb_bdy            = 0
      ln_coords_file(:) = .false.
      cn_coords_file(:) = ''
      ln_mask_file      = .false.
      cn_mask_file(:)   = ''
      nn_dyn2d(:)       = 0
      nn_dyn2d_dta(:)   = -1  ! uninitialised flag
      nn_dyn3d(:)       = 0
      nn_dyn3d_dta(:)   = -1  ! uninitialised flag
      nn_tra(:)         = 0
      nn_tra_dta(:)     = -1  ! uninitialised flag
#if defined key_lim2
      nn_ice_lim2(:)    = 0
      nn_ice_lim2_dta(:)= -1  ! uninitialised flag
#endif
      ln_vol            = .false.
      nn_volctl         = -1  ! uninitialised flag
      nn_rimwidth(:)    = -1  ! uninitialised flag

      REWIND( numnam )                    
      READ  ( numnam, nambdy )

      ! -----------------------------------------
      ! Check and write out namelist parameters
      ! -----------------------------------------

      !                                   ! control prints
      IF(lwp) WRITE(numout,*) '         nambdy'

      IF( nb_bdy .eq. 0 ) THEN 
        IF(lwp) WRITE(numout,*) 'nb_bdy = 0, NO OPEN BOUNDARIES APPLIED.'
      ELSE
        IF(lwp) WRITE(numout,*) 'Number of open boundary sets : ',nb_bdy
      ENDIF

      DO ib_bdy = 1,nb_bdy
        IF(lwp) WRITE(numout,*) ' ' 
        IF(lwp) WRITE(numout,*) '------ Open boundary data set ',ib_bdy,'------' 

        IF( ln_coords_file(ib_bdy) ) THEN
           IF(lwp) WRITE(numout,*) 'Boundary definition read from file '//TRIM(cn_coords_file(ib_bdy))
        ELSE
           IF(lwp) WRITE(numout,*) 'Boundary defined in namelist.'
        ENDIF
        IF(lwp) WRITE(numout,*)

        IF(lwp) WRITE(numout,*) 'Boundary conditions for barotropic solution:  '
        SELECT CASE( nn_dyn2d(ib_bdy) )                  
          CASE( 0 )      ;   IF(lwp) WRITE(numout,*) '      no open boundary condition'        
          CASE( 1 )      ;   IF(lwp) WRITE(numout,*) '      Flow Relaxation Scheme'
          CASE( 2 )      ;   IF(lwp) WRITE(numout,*) '      Flather radiation condition'
          CASE DEFAULT   ;   CALL ctl_stop( 'unrecognised value for nn_dyn2d' )
        END SELECT
        IF( nn_dyn2d(ib_bdy) .gt. 0 ) THEN
           SELECT CASE( nn_dyn2d_dta(ib_bdy) )                   ! 
              CASE( 0 )      ;   IF(lwp) WRITE(numout,*) '      initial state used for bdy data'        
              CASE( 1 )      ;   IF(lwp) WRITE(numout,*) '      boundary data taken from file'
              CASE( 2 )      ;   IF(lwp) WRITE(numout,*) '      tidal harmonic forcing taken from file'
              CASE( 3 )      ;   IF(lwp) WRITE(numout,*) '      boundary data AND tidal harmonic forcing taken from files'
              CASE DEFAULT   ;   CALL ctl_stop( 'nn_dyn2d_dta must be between 0 and 3' )
           END SELECT
        ENDIF
        IF(lwp) WRITE(numout,*)

        IF(lwp) WRITE(numout,*) 'Boundary conditions for baroclinic velocities:  '
        SELECT CASE( nn_dyn3d(ib_bdy) )                  
          CASE( 0 )      ;   IF(lwp) WRITE(numout,*) '      no open boundary condition'        
          CASE( 1 )      ;   IF(lwp) WRITE(numout,*) '      Flow Relaxation Scheme'
          CASE DEFAULT   ;   CALL ctl_stop( 'unrecognised value for nn_dyn3d' )
        END SELECT
        IF( nn_dyn3d(ib_bdy) .gt. 0 ) THEN
           SELECT CASE( nn_dyn3d_dta(ib_bdy) )                   ! 
              CASE( 0 )      ;   IF(lwp) WRITE(numout,*) '      initial state used for bdy data'        
              CASE( 1 )      ;   IF(lwp) WRITE(numout,*) '      boundary data taken from file'
              CASE DEFAULT   ;   CALL ctl_stop( 'nn_dyn3d_dta must be 0 or 1' )
           END SELECT
        ENDIF
        IF(lwp) WRITE(numout,*)

        IF(lwp) WRITE(numout,*) 'Boundary conditions for temperature and salinity:  '
        SELECT CASE( nn_tra(ib_bdy) )                  
          CASE( 0 )      ;   IF(lwp) WRITE(numout,*) '      no open boundary condition'        
          CASE( 1 )      ;   IF(lwp) WRITE(numout,*) '      Flow Relaxation Scheme'
          CASE DEFAULT   ;   CALL ctl_stop( 'unrecognised value for nn_tra' )
        END SELECT
        IF( nn_tra(ib_bdy) .gt. 0 ) THEN
           SELECT CASE( nn_tra_dta(ib_bdy) )                   ! 
              CASE( 0 )      ;   IF(lwp) WRITE(numout,*) '      initial state used for bdy data'        
              CASE( 1 )      ;   IF(lwp) WRITE(numout,*) '      boundary data taken from file'
              CASE DEFAULT   ;   CALL ctl_stop( 'nn_tra_dta must be 0 or 1' )
           END SELECT
        ENDIF
        IF(lwp) WRITE(numout,*)

#if defined key_lim2
        IF(lwp) WRITE(numout,*) 'Boundary conditions for sea ice:  '
        SELECT CASE( nn_ice_lim2(ib_bdy) )                  
          CASE( 0 )      ;   IF(lwp) WRITE(numout,*) '      no open boundary condition'        
          CASE( 1 )      ;   IF(lwp) WRITE(numout,*) '      Flow Relaxation Scheme'
          CASE DEFAULT   ;   CALL ctl_stop( 'unrecognised value for nn_tra' )
        END SELECT
        IF( nn_ice_lim2(ib_bdy) .gt. 0 ) THEN 
           SELECT CASE( nn_ice_lim2_dta(ib_bdy) )                   ! 
              CASE( 0 )      ;   IF(lwp) WRITE(numout,*) '      initial state used for bdy data'        
              CASE( 1 )      ;   IF(lwp) WRITE(numout,*) '      boundary data taken from file'
              CASE DEFAULT   ;   CALL ctl_stop( 'nn_ice_lim2_dta must be 0 or 1' )
           END SELECT
        ENDIF
        IF(lwp) WRITE(numout,*)
#endif

        IF(lwp) WRITE(numout,*) 'Boundary rim width for the FRS scheme = ', nn_rimwidth(ib_bdy)
        IF(lwp) WRITE(numout,*)

      ENDDO

     IF( ln_vol ) THEN                     ! check volume conservation (nn_volctl value)
       IF(lwp) WRITE(numout,*) 'Volume correction applied at open boundaries'
       IF(lwp) WRITE(numout,*)
       SELECT CASE ( nn_volctl )
         CASE( 1 )      ;   IF(lwp) WRITE(numout,*) '      The total volume will be constant'
         CASE( 0 )      ;   IF(lwp) WRITE(numout,*) '      The total volume will vary according to the surface E-P flux'
         CASE DEFAULT   ;   CALL ctl_stop( 'nn_volctl must be 0 or 1' )
       END SELECT
       IF(lwp) WRITE(numout,*)
     ELSE
       IF(lwp) WRITE(numout,*) 'No volume correction applied at open boundaries'
       IF(lwp) WRITE(numout,*)
     ENDIF

      ! -------------------------------------------------
      ! Initialise indices arrays for open boundaries
      ! -------------------------------------------------

      ! Work out global dimensions of boundary data
      ! ---------------------------------------------
      REWIND( numnam )                    
      DO ib_bdy = 1, nb_bdy

         jpbdta = 1
         IF( .NOT. ln_coords_file(ib_bdy) ) THEN ! Work out size of global arrays from namelist parameters
 
            ! No REWIND here because may need to read more than one nambdy_index namelist.
            READ  ( numnam, nambdy_index )

            ! Automatic boundary definition: if nbdysegX = -1
            ! set boundary to whole side of model domain.
            IF( nbdysege == -1 ) THEN
               nbdysege = 1
               jpieob(1) = jpiglo - 1
               jpjedt(1) = 2
               jpjeft(1) = jpjglo - 1
            ENDIF
            IF( nbdysegw == -1 ) THEN
               nbdysegw = 1
               jpiwob(1) = 2
               jpjwdt(1) = 2
               jpjwft(1) = jpjglo - 1
            ENDIF
            IF( nbdysegn == -1 ) THEN
               nbdysegn = 1
               jpjnob(1) = jpjglo - 1
               jpindt(1) = 2
               jpinft(1) = jpiglo - 1
            ENDIF
            IF( nbdysegs == -1 ) THEN
               nbdysegs = 1
               jpjsob(1) = 2
               jpisdt(1) = 2
               jpisft(1) = jpiglo - 1
            ENDIF

            nblendta(:,ib_bdy) = 0
            DO iseg = 1, nbdysege
               igrd = 1
               nblendta(igrd,ib_bdy) = nblendta(igrd,ib_bdy) + jpjeft(iseg) - jpjedt(iseg) + 1               
               igrd = 2
               nblendta(igrd,ib_bdy) = nblendta(igrd,ib_bdy) + jpjeft(iseg) - jpjedt(iseg) + 1               
               igrd = 3
               nblendta(igrd,ib_bdy) = nblendta(igrd,ib_bdy) + jpjeft(iseg) - jpjedt(iseg)               
            ENDDO
            DO iseg = 1, nbdysegw
               igrd = 1
               nblendta(igrd,ib_bdy) = nblendta(igrd,ib_bdy) + jpjwft(iseg) - jpjwdt(iseg) + 1               
               igrd = 2
               nblendta(igrd,ib_bdy) = nblendta(igrd,ib_bdy) + jpjwft(iseg) - jpjwdt(iseg) + 1               
               igrd = 3
               nblendta(igrd,ib_bdy) = nblendta(igrd,ib_bdy) + jpjwft(iseg) - jpjwdt(iseg)               
            ENDDO
            DO iseg = 1, nbdysegn
               igrd = 1
               nblendta(igrd,ib_bdy) = nblendta(igrd,ib_bdy) + jpinft(iseg) - jpindt(iseg) + 1               
               igrd = 2
               nblendta(igrd,ib_bdy) = nblendta(igrd,ib_bdy) + jpinft(iseg) - jpindt(iseg)               
               igrd = 3
               nblendta(igrd,ib_bdy) = nblendta(igrd,ib_bdy) + jpinft(iseg) - jpindt(iseg) + 1
            ENDDO
            DO iseg = 1, nbdysegs
               igrd = 1
               nblendta(igrd,ib_bdy) = nblendta(igrd,ib_bdy) + jpisft(iseg) - jpisdt(iseg) + 1               
               igrd = 2
               nblendta(igrd,ib_bdy) = nblendta(igrd,ib_bdy) + jpisft(iseg) - jpisdt(iseg)
               igrd = 3
               nblendta(igrd,ib_bdy) = nblendta(igrd,ib_bdy) + jpisft(iseg) - jpisdt(iseg) + 1               
            ENDDO

            nblendta(:,ib_bdy) = nblendta(:,ib_bdy) * nn_rimwidth(ib_bdy)
            jpbdta = MAXVAL(nblendta(:,ib_bdy))               


         ELSE            ! Read size of arrays in boundary coordinates file.


            CALL iom_open( cn_coords_file(ib_bdy), inum )
            jpbdta = 1
            DO igrd = 1, jpbgrd
               id_dummy = iom_varid( inum, 'nbi'//cgrid(igrd), kdimsz=kdimsz )  
               nblendta(igrd,ib_bdy) = kdimsz(1)
               jpbdta = MAX(jpbdta, kdimsz(1))
            ENDDO

         ENDIF 

      ENDDO ! ib_bdy

      ! Allocate arrays
      !---------------
      ALLOCATE( nbidta(jpbdta, jpbgrd, nb_bdy), nbjdta(jpbdta, jpbgrd, nb_bdy),    &
         &      nbrdta(jpbdta, jpbgrd, nb_bdy) )

      ALLOCATE( dta_global(jpbdta, 1, jpk) )

      ! Calculate global boundary index arrays or read in from file
      !------------------------------------------------------------
      REWIND( numnam )                    
      DO ib_bdy = 1, nb_bdy

         IF( .NOT. ln_coords_file(ib_bdy) ) THEN ! Calculate global index arrays from namelist parameters

            ! No REWIND here because may need to read more than one nambdy_index namelist.
            READ  ( numnam, nambdy_index )

            ! Automatic boundary definition: if nbdysegX = -1
            ! set boundary to whole side of model domain.
            IF( nbdysege == -1 ) THEN
               nbdysege = 1
               jpieob(1) = jpiglo - 1
               jpjedt(1) = 2
               jpjeft(1) = jpjglo - 1
            ENDIF
            IF( nbdysegw == -1 ) THEN
               nbdysegw = 1
               jpiwob(1) = 2
               jpjwdt(1) = 2
               jpjwft(1) = jpjglo - 1
            ENDIF
            IF( nbdysegn == -1 ) THEN
               nbdysegn = 1
               jpjnob(1) = jpjglo - 1
               jpindt(1) = 2
               jpinft(1) = jpiglo - 1
            ENDIF
            IF( nbdysegs == -1 ) THEN
               nbdysegs = 1
               jpjsob(1) = 2
               jpisdt(1) = 2
               jpisft(1) = jpiglo - 1
            ENDIF

            ! ------------ T points -------------
            igrd = 1  
            icount = 0
            DO ir = 1, nn_rimwidth(ib_bdy)
               ! east
               DO iseg = 1, nbdysege
                  DO ij = jpjedt(iseg), jpjeft(iseg)
                     icount = icount + 1
                     nbidta(icount, igrd, ib_bdy) = jpieob(iseg) - ir + 1
                     nbjdta(icount, igrd, ib_bdy) = ij
                     nbrdta(icount, igrd, ib_bdy) = ir
                  ENDDO
               ENDDO
               ! west
               DO iseg = 1, nbdysegw
                  DO ij = jpjwdt(iseg), jpjwft(iseg)
                     icount = icount + 1
                     nbidta(icount, igrd, ib_bdy) = jpiwob(iseg) + ir - 1
                     nbjdta(icount, igrd, ib_bdy) = ij
                     nbrdta(icount, igrd, ib_bdy) = ir
                  ENDDO
               ENDDO
               ! north
               DO iseg = 1, nbdysegn
                  DO ii = jpindt(iseg), jpinft(iseg)
                     icount = icount + 1
                     nbidta(icount, igrd, ib_bdy) = ii
                     nbjdta(icount, igrd, ib_bdy) = jpjnob(iseg) - ir + 1
                     nbrdta(icount, igrd, ib_bdy) = ir
                  ENDDO
               ENDDO
               ! south
               DO iseg = 1, nbdysegs
                  DO ii = jpisdt(iseg), jpisft(iseg)
                     icount = icount + 1
                     nbidta(icount, igrd, ib_bdy) = ii
                     nbjdta(icount, igrd, ib_bdy) = jpjsob(iseg) + ir - 1
                     nbrdta(icount, igrd, ib_bdy) = ir
                  ENDDO
               ENDDO
            ENDDO

            ! ------------ U points -------------
            igrd = 2  
            icount = 0
            DO ir = 1, nn_rimwidth(ib_bdy)
               ! east
               DO iseg = 1, nbdysege
                  DO ij = jpjedt(iseg), jpjeft(iseg)
                     icount = icount + 1
                     nbidta(icount, igrd, ib_bdy) = jpieob(iseg) - ir
                     nbjdta(icount, igrd, ib_bdy) = ij
                     nbrdta(icount, igrd, ib_bdy) = ir
                  ENDDO
               ENDDO
               ! west
               DO iseg = 1, nbdysegw
                  DO ij = jpjwdt(iseg), jpjwft(iseg)
                     icount = icount + 1
                     nbidta(icount, igrd, ib_bdy) = jpiwob(iseg) + ir - 1
                     nbjdta(icount, igrd, ib_bdy) = ij
                     nbrdta(icount, igrd, ib_bdy) = ir
                  ENDDO
               ENDDO
               ! north
               DO iseg = 1, nbdysegn
                  DO ii = jpindt(iseg), jpinft(iseg) - 1
                     icount = icount + 1
                     nbidta(icount, igrd, ib_bdy) = ii
                     nbjdta(icount, igrd, ib_bdy) = jpjnob(iseg) - ir + 1
                     nbrdta(icount, igrd, ib_bdy) = ir
                  ENDDO
               ENDDO
               ! south
               DO iseg = 1, nbdysegs
                  DO ii = jpisdt(iseg), jpisft(iseg) - 1
                     icount = icount + 1
                     nbidta(icount, igrd, ib_bdy) = ii
                     nbjdta(icount, igrd, ib_bdy) = jpjsob(iseg) + ir - 1
                     nbrdta(icount, igrd, ib_bdy) = ir
                  ENDDO
               ENDDO
            ENDDO

            ! ------------ V points -------------
            igrd = 3  
            icount = 0
            DO ir = 1, nn_rimwidth(ib_bdy)
               ! east
               DO iseg = 1, nbdysege
                  DO ij = jpjedt(iseg), jpjeft(iseg) - 1
                     icount = icount + 1
                     nbidta(icount, igrd, ib_bdy) = jpieob(iseg) - ir + 1
                     nbjdta(icount, igrd, ib_bdy) = ij
                     nbrdta(icount, igrd, ib_bdy) = ir
                  ENDDO
               ENDDO
               ! west
               DO iseg = 1, nbdysegw
                  DO ij = jpjwdt(iseg), jpjwft(iseg) - 1
                     icount = icount + 1
                     nbidta(icount, igrd, ib_bdy) = jpiwob(iseg) + ir - 1
                     nbjdta(icount, igrd, ib_bdy) = ij
                     nbrdta(icount, igrd, ib_bdy) = ir
                  ENDDO
               ENDDO
               ! north
               DO iseg = 1, nbdysegn
                  DO ii = jpindt(iseg), jpinft(iseg)
                     icount = icount + 1
                     nbidta(icount, igrd, ib_bdy) = ii
                     nbjdta(icount, igrd, ib_bdy) = jpjnob(iseg) - ir
                     nbrdta(icount, igrd, ib_bdy) = ir
                  ENDDO
               ENDDO
               ! south
               DO iseg = 1, nbdysegs
                  DO ii = jpisdt(iseg), jpisft(iseg)
                     icount = icount + 1
                     nbidta(icount, igrd, ib_bdy) = ii
                     nbjdta(icount, igrd, ib_bdy) = jpjsob(iseg) + ir - 1
                     nbrdta(icount, igrd, ib_bdy) = ir
                  ENDDO
               ENDDO
            ENDDO

         ELSE            ! Read global index arrays from boundary coordinates file.

            DO igrd = 1, jpbgrd
               CALL iom_get( inum, jpdom_unknown, 'nbi'//cgrid(igrd), dta_global(1:nblendta(igrd,ib_bdy),:,1) )
               DO ii = 1,nblendta(igrd,ib_bdy)
                  nbidta(ii,igrd,ib_bdy) = INT( dta_global(ii,1,1) )
               END DO
               CALL iom_get( inum, jpdom_unknown, 'nbj'//cgrid(igrd), dta_global(1:nblendta(igrd,ib_bdy),:,1) )
               DO ii = 1,nblendta(igrd,ib_bdy)
                  nbjdta(ii,igrd,ib_bdy) = INT( dta_global(ii,1,1) )
               END DO
               CALL iom_get( inum, jpdom_unknown, 'nbr'//cgrid(igrd), dta_global(1:nblendta(igrd,ib_bdy),:,1) )
               DO ii = 1,nblendta(igrd,ib_bdy)
                  nbrdta(ii,igrd,ib_bdy) = INT( dta_global(ii,1,1) )
               END DO

               ibr_max = MAXVAL( nbrdta(:,igrd,ib_bdy) )
               IF(lwp) WRITE(numout,*)
               IF(lwp) WRITE(numout,*) ' Maximum rimwidth in file is ', ibr_max
               IF(lwp) WRITE(numout,*) ' nn_rimwidth from namelist is ', nn_rimwidth(ib_bdy)
               IF (ibr_max < nn_rimwidth(ib_bdy))   &
                     CALL ctl_stop( 'nn_rimwidth is larger than maximum rimwidth in file',cn_coords_file(ib_bdy) )

            END DO
            CALL iom_close( inum )

         ENDIF 

      ENDDO 

      ! Work out dimensions of boundary data on each processor
      ! ------------------------------------------------------
     
      iw = mig(1) + 1            ! if monotasking and no zoom, iw=2
      ie = mig(1) + nlci-1 - 1   ! if monotasking and no zoom, ie=jpim1
      is = mjg(1) + 1            ! if monotasking and no zoom, is=2
      in = mjg(1) + nlcj-1 - 1   ! if monotasking and no zoom, in=jpjm1

      DO ib_bdy = 1, nb_bdy
         DO igrd = 1, jpbgrd
            icount  = 0
            icountr = 0
            idx_bdy(ib_bdy)%nblen(igrd)    = 0
            idx_bdy(ib_bdy)%nblenrim(igrd) = 0
            DO ib = 1, nblendta(igrd,ib_bdy)
               ! check that data is in correct order in file
               ibm1 = MAX(1,ib-1)
               IF(lwp) THEN         ! Since all procs read global data only need to do this check on one proc...
                  IF( nbrdta(ib,igrd,ib_bdy) < nbrdta(ibm1,igrd,ib_bdy) ) THEN
                     CALL ctl_stop('bdy_init : ERROR : boundary data in file must be defined in order of distance from edge nbr.', &
                    'A utility for re-ordering boundary coordinates and data files exists in the TOOLS/OBC directory')
                  ENDIF    
               ENDIF
               ! check if point is in local domain
               IF(  nbidta(ib,igrd,ib_bdy) >= iw .AND. nbidta(ib,igrd,ib_bdy) <= ie .AND.   &
                  & nbjdta(ib,igrd,ib_bdy) >= is .AND. nbjdta(ib,igrd,ib_bdy) <= in       ) THEN
                  !
                  icount = icount  + 1
                  !
                  IF( nbrdta(ib,igrd,ib_bdy) == 1 )   icountr = icountr+1
               ENDIF
            ENDDO
            idx_bdy(ib_bdy)%nblenrim(igrd) = icountr !: length of rim boundary data on each proc
            idx_bdy(ib_bdy)%nblen   (igrd) = icount  !: length of boundary data on each proc        
         ENDDO  ! igrd

         ! Allocate index arrays for this boundary set
         !--------------------------------------------
         ilen1 = MAXVAL(idx_bdy(ib_bdy)%nblen(:))
         ALLOCATE( idx_bdy(ib_bdy)%nbi(ilen1,jpbgrd) )
         ALLOCATE( idx_bdy(ib_bdy)%nbj(ilen1,jpbgrd) )
         ALLOCATE( idx_bdy(ib_bdy)%nbr(ilen1,jpbgrd) )
         ALLOCATE( idx_bdy(ib_bdy)%nbmap(ilen1,jpbgrd) )
         ALLOCATE( idx_bdy(ib_bdy)%nbw(ilen1,jpbgrd) )
         ALLOCATE( idx_bdy(ib_bdy)%flagu(ilen1) )
         ALLOCATE( idx_bdy(ib_bdy)%flagv(ilen1) )

         ! Dispatch mapping indices and discrete distances on each processor
         ! -----------------------------------------------------------------

         DO igrd = 1, jpbgrd
            icount  = 0
            ! Loop on rimwidth to ensure outermost points come first in the local arrays.
            DO ir=1, nn_rimwidth(ib_bdy)
               DO ib = 1, nblendta(igrd,ib_bdy)
                  ! check if point is in local domain and equals ir
                  IF(  nbidta(ib,igrd,ib_bdy) >= iw .AND. nbidta(ib,igrd,ib_bdy) <= ie .AND.   &
                     & nbjdta(ib,igrd,ib_bdy) >= is .AND. nbjdta(ib,igrd,ib_bdy) <= in .AND.   &
                     & nbrdta(ib,igrd,ib_bdy) == ir  ) THEN
                     !
                     icount = icount  + 1
                     idx_bdy(ib_bdy)%nbi(icount,igrd)   = nbidta(ib,igrd,ib_bdy)- mig(1)+1
                     idx_bdy(ib_bdy)%nbj(icount,igrd)   = nbjdta(ib,igrd,ib_bdy)- mjg(1)+1
                     idx_bdy(ib_bdy)%nbr(icount,igrd)   = nbrdta(ib,igrd,ib_bdy)
                     idx_bdy(ib_bdy)%nbmap(icount,igrd) = ib
                  ENDIF
               ENDDO
            ENDDO
         ENDDO 

         ! Compute rim weights for FRS scheme
         ! ----------------------------------
         DO igrd = 1, jpbgrd
            DO ib = 1, idx_bdy(ib_bdy)%nblen(igrd)
               nbr => idx_bdy(ib_bdy)%nbr(ib,igrd)
               idx_bdy(ib_bdy)%nbw(ib,igrd) = 1.- TANH( FLOAT( nbr - 1 ) *0.5 )      ! tanh formulation
!              idx_bdy(ib_bdy)%nbw(ib,igrd) = (FLOAT(nn_rimwidth+1-nbr)/FLOAT(nn_rimwidth))**2      ! quadratic
!              idx_bdy(ib_bdy)%nbw(ib,igrd) =  FLOAT(nn_rimwidth+1-nbr)/FLOAT(nn_rimwidth)          ! linear
            END DO
         END DO 

      ENDDO

      ! ------------------------------------------------------
      ! Initialise masks and find normal/tangential directions
      ! ------------------------------------------------------

      ! Read global 2D mask at T-points: bdytmask
      ! -----------------------------------------
      ! bdytmask = 1  on the computational domain AND on open boundaries
      !          = 0  elsewhere   
 
      IF( cp_cfg == "eel" .AND. jp_cfg == 5 ) THEN          ! EEL configuration at 5km resolution
         zmask(         :                ,:) = 0.e0
         zmask(jpizoom+1:jpizoom+jpiglo-2,:) = 1.e0          
      ELSE IF( ln_mask_file ) THEN
         CALL iom_open( cn_mask_file, inum )
         CALL iom_get ( inum, jpdom_data, 'bdy_msk', zmask(:,:) )
         CALL iom_close( inum )
      ELSE
         zmask(:,:) = 1.e0
      ENDIF

      DO ij = 1, nlcj      ! Save mask over local domain      
         DO ii = 1, nlci
            bdytmask(ii,ij) = zmask( mig(ii), mjg(ij) )
         END DO
      END DO

      ! Derive mask on U and V grid from mask on T grid
      bdyumask(:,:) = 0.e0
      bdyvmask(:,:) = 0.e0
      DO ij=1, jpjm1
         DO ii=1, jpim1
            bdyumask(ii,ij)=bdytmask(ii,ij)*bdytmask(ii+1, ij )
            bdyvmask(ii,ij)=bdytmask(ii,ij)*bdytmask(ii  ,ij+1)  
         END DO
      END DO
      CALL lbc_lnk( bdyumask(:,:), 'U', 1. )   ;   CALL lbc_lnk( bdyvmask(:,:), 'V', 1. )      ! Lateral boundary cond.


      ! Mask corrections
      ! ----------------
      DO ik = 1, jpkm1
         DO ij = 1, jpj
            DO ii = 1, jpi
               tmask(ii,ij,ik) = tmask(ii,ij,ik) * bdytmask(ii,ij)
               umask(ii,ij,ik) = umask(ii,ij,ik) * bdyumask(ii,ij)
               vmask(ii,ij,ik) = vmask(ii,ij,ik) * bdyvmask(ii,ij)
               bmask(ii,ij)    = bmask(ii,ij)    * bdytmask(ii,ij)
            END DO      
         END DO
      END DO

      DO ik = 1, jpkm1
         DO ij = 2, jpjm1
            DO ii = 2, jpim1
               fmask(ii,ij,ik) = fmask(ii,ij,ik) * bdytmask(ii,ij  ) * bdytmask(ii+1,ij  )   &
                  &                              * bdytmask(ii,ij+1) * bdytmask(ii+1,ij+1)
            END DO      
         END DO
      END DO

      tmask_i (:,:) = tmask(:,:,1) * tmask_i(:,:)             
      bdytmask(:,:) = tmask(:,:,1)

      ! bdy masks and bmask are now set to zero on boundary points:
      igrd = 1       ! In the free surface case, bmask is at T-points
      DO ib_bdy = 1, nb_bdy
        DO ib = 1, idx_bdy(ib_bdy)%nblenrim(igrd)     
          bmask(idx_bdy(ib_bdy)%nbi(ib,igrd), idx_bdy(ib_bdy)%nbj(ib,igrd)) = 0.e0
        ENDDO
      ENDDO
      !
      igrd = 1
      DO ib_bdy = 1, nb_bdy
        DO ib = 1, idx_bdy(ib_bdy)%nblenrim(igrd)      
          bdytmask(idx_bdy(ib_bdy)%nbi(ib,igrd), idx_bdy(ib_bdy)%nbj(ib,igrd)) = 0.e0
        ENDDO
      ENDDO
      !
      igrd = 2
      DO ib_bdy = 1, nb_bdy
        DO ib = 1, idx_bdy(ib_bdy)%nblenrim(igrd)
          bdyumask(idx_bdy(ib_bdy)%nbi(ib,igrd), idx_bdy(ib_bdy)%nbj(ib,igrd)) = 0.e0
        ENDDO
      ENDDO
      !
      igrd = 3
      DO ib_bdy = 1, nb_bdy
        DO ib = 1, idx_bdy(ib_bdy)%nblenrim(igrd)
          bdyvmask(idx_bdy(ib_bdy)%nbi(ib,igrd), idx_bdy(ib_bdy)%nbj(ib,igrd)) = 0.e0
        ENDDO
      ENDDO

      ! Lateral boundary conditions
      CALL lbc_lnk( fmask        , 'F', 1. )   ;   CALL lbc_lnk( bdytmask(:,:), 'T', 1. )
      CALL lbc_lnk( bdyumask(:,:), 'U', 1. )   ;   CALL lbc_lnk( bdyvmask(:,:), 'V', 1. )

      DO ib_bdy = 1, nb_bdy       ! Indices and directions of rim velocity components

         idx_bdy(ib_bdy)%flagu(:) = 0.e0
         idx_bdy(ib_bdy)%flagv(:) = 0.e0
         icount = 0 

         !flagu = -1 : u component is normal to the dynamical boundary but its direction is outward
         !flagu =  0 : u is tangential
         !flagu =  1 : u is normal to the boundary and is direction is inward
  
         igrd = 2      ! u-component 
         DO ib = 1, idx_bdy(ib_bdy)%nblenrim(igrd)  
            nbi => idx_bdy(ib_bdy)%nbi(ib,igrd)
            nbj => idx_bdy(ib_bdy)%nbj(ib,igrd)
            zefl = bdytmask(nbi  ,nbj)
            zwfl = bdytmask(nbi+1,nbj)
            IF( zefl + zwfl == 2 ) THEN
               icount = icount + 1
            ELSE
               idx_bdy(ib_bdy)%flagu(ib)=-zefl+zwfl
            ENDIF
         END DO

         !flagv = -1 : u component is normal to the dynamical boundary but its direction is outward
         !flagv =  0 : u is tangential
         !flagv =  1 : u is normal to the boundary and is direction is inward

         igrd = 3      ! v-component
         DO ib = 1, idx_bdy(ib_bdy)%nblenrim(igrd)  
            nbi => idx_bdy(ib_bdy)%nbi(ib,igrd)
            nbj => idx_bdy(ib_bdy)%nbj(ib,igrd)
            znfl = bdytmask(nbi,nbj  )
            zsfl = bdytmask(nbi,nbj+1)
            IF( znfl + zsfl == 2 ) THEN
               icount = icount + 1
            ELSE
               idx_bdy(ib_bdy)%flagv(ib) = -znfl + zsfl
            END IF
         END DO
 
         IF( icount /= 0 ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) ' E R R O R : Some data velocity points,',   &
               ' are not boundary points. Check nbi, nbj, indices for boundary set ',ib_bdy
            IF(lwp) WRITE(numout,*) ' ========== '
            IF(lwp) WRITE(numout,*)
            nstop = nstop + 1
         ENDIF 
    
      ENDDO

      ! Compute total lateral surface for volume correction:
      ! ----------------------------------------------------
      bdysurftot = 0.e0 
      IF( ln_vol ) THEN  
         igrd = 2      ! Lateral surface at U-points
         DO ib_bdy = 1, nb_bdy
            DO ib = 1, idx_bdy(ib_bdy)%nblenrim(igrd)
               nbi => idx_bdy(ib_bdy)%nbi(ib,igrd)
               nbj => idx_bdy(ib_bdy)%nbi(ib,igrd)
               flagu => idx_bdy(ib_bdy)%flagu(ib)
               bdysurftot = bdysurftot + hu     (nbi  , nbj)                           &
                  &                    * e2u    (nbi  , nbj) * ABS( flagu ) &
                  &                    * tmask_i(nbi  , nbj)                           &
                  &                    * tmask_i(nbi+1, nbj)                   
            ENDDO
         ENDDO

         igrd=3 ! Add lateral surface at V-points
         DO ib_bdy = 1, nb_bdy
            DO ib = 1, idx_bdy(ib_bdy)%nblenrim(igrd)
               nbi => idx_bdy(ib_bdy)%nbi(ib,igrd)
               nbj => idx_bdy(ib_bdy)%nbi(ib,igrd)
               flagv => idx_bdy(ib_bdy)%flagv(ib)
               bdysurftot = bdysurftot + hv     (nbi, nbj  )                           &
                  &                    * e1v    (nbi, nbj  ) * ABS( flagv ) &
                  &                    * tmask_i(nbi, nbj  )                           &
                  &                    * tmask_i(nbi, nbj+1)
            ENDDO
         ENDDO
         !
         IF( lk_mpp )   CALL mpp_sum( bdysurftot )      ! sum over the global domain
      END IF   
      !
      ! Tidy up
      !--------
      DEALLOCATE(nbidta, nbjdta, nbrdta)

      IF( nn_timing == 1 ) CALL timing_stop('bdy_init')

   END SUBROUTINE bdy_init

#else
   !!---------------------------------------------------------------------------------
   !!   Dummy module                                   NO open boundaries
   !!---------------------------------------------------------------------------------
CONTAINS
   SUBROUTINE bdy_init      ! Dummy routine
   END SUBROUTINE bdy_init
#endif

   !!=================================================================================
END MODULE bdyini
