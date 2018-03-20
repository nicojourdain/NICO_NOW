MODULE fldread
   !!======================================================================
   !!                       ***  MODULE  fldread  ***
   !! Ocean forcing:  read input field for surface boundary condition
   !!=====================================================================
   !! History :  2.0  !  06-2006  (S. Masson, G. Madec) Original code
   !!                 !  05-2008  (S. Alderson) Modified for Interpolation in memory
   !!                 !                         from input grid to model grid
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   fld_read      : read input fields used for the computation of the
   !!                   surface boundary condition
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! ???
   USE in_out_manager  ! I/O manager
   USE iom             ! I/O manager library
   USE geo2ocean       ! for vector rotation on to model grid
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! work arrays
   USE ioipsl, ONLY :   ymds2ju, ju2ymds   ! for calendar

   IMPLICIT NONE
   PRIVATE   
 
   PUBLIC   fld_map    ! routine called by tides_init

   TYPE, PUBLIC ::   FLD_N      !: Namelist field informations
      CHARACTER(len = 256) ::   clname      ! generic name of the NetCDF flux file
      INTEGER              ::   nfreqh      ! frequency of each flux file
      CHARACTER(len = 34)  ::   clvar       ! generic name of the variable in the NetCDF flux file
      LOGICAL              ::   ln_tint     ! time interpolation or not (T/F)
      LOGICAL              ::   ln_clim     ! climatology or not (T/F)
      CHARACTER(len = 8)   ::   cltype      ! type of data file 'daily', 'monthly' or yearly'
      CHARACTER(len = 34)  ::   wname       ! generic name of a NetCDF weights file to be used, blank if not
      CHARACTER(len = 34)  ::   vcomp       ! symbolic component name if a vector that needs rotation
      !                                     ! a string starting with "U" or "V" for each component   
      !                                     ! chars 2 onwards identify which components go together  
   END TYPE FLD_N

   TYPE, PUBLIC ::   FLD        !: Input field related variables
      CHARACTER(len = 256)            ::   clrootname   ! generic name of the NetCDF file
      CHARACTER(len = 256)            ::   clname       ! current name of the NetCDF file
      INTEGER                         ::   nfreqh       ! frequency of each flux file
      CHARACTER(len = 34)             ::   clvar        ! generic name of the variable in the NetCDF flux file
      LOGICAL                         ::   ln_tint      ! time interpolation or not (T/F)
      LOGICAL                         ::   ln_clim      ! climatology or not (T/F)
      CHARACTER(len = 8)              ::   cltype       ! type of data file 'daily', 'monthly' or yearly'
      INTEGER                         ::   num          ! iom id of the jpfld files to be read
      INTEGER , DIMENSION(2)          ::   nrec_b       ! before record (1: index, 2: second since Jan. 1st 00h of nit000 year)
      INTEGER , DIMENSION(2)          ::   nrec_a       ! after  record (1: index, 2: second since Jan. 1st 00h of nit000 year)
      REAL(wp) , ALLOCATABLE, DIMENSION(:,:,:  ) ::   fnow       ! input fields interpolated to now time step
      REAL(wp) , ALLOCATABLE, DIMENSION(:,:,:,:) ::   fdta       ! 2 consecutive record of input fields
      CHARACTER(len = 256)            ::   wgtname      ! current name of the NetCDF weight file acting as a key
      !                                                 ! into the WGTLIST structure
      CHARACTER(len = 34)             ::   vcomp        ! symbolic name for a vector component that needs rotation
      LOGICAL                         ::   rotn         ! flag to indicate whether field has been rotated
   END TYPE FLD

   TYPE, PUBLIC ::   MAP_POINTER      !: Array of integer pointers to 1D arrays
      INTEGER, POINTER   ::  ptr(:)
   END TYPE MAP_POINTER

!$AGRIF_DO_NOT_TREAT

   !! keep list of all weights variables so they're only read in once
   !! need to add AGRIF directives not to process this structure
   !! also need to force wgtname to include AGRIF nest number
   TYPE         ::   WGT        !: Input weights related variables
      CHARACTER(len = 256)                    ::   wgtname      ! current name of the NetCDF weight file
      INTEGER , DIMENSION(2)                  ::   ddims        ! shape of input grid
      INTEGER , DIMENSION(2)                  ::   botleft      ! top left corner of box in input grid containing 
      !                                                         ! current processor grid
      INTEGER , DIMENSION(2)                  ::   topright     ! top right corner of box 
      INTEGER                                 ::   jpiwgt       ! width of box on input grid
      INTEGER                                 ::   jpjwgt       ! height of box on input grid
      INTEGER                                 ::   numwgt       ! number of weights (4=bilinear, 16=bicubic)
      INTEGER                                 ::   nestid       ! for agrif, keep track of nest we're in
      INTEGER                                 ::   overlap      ! =0 when cyclic grid has no overlapping EW columns
      !                                                         ! =>1 when they have one or more overlapping columns      
      !                                                         ! =-1 not cyclic
      LOGICAL                                 ::   cyclic       ! east-west cyclic or not
      INTEGER,  DIMENSION(:,:,:), POINTER     ::   data_jpi     ! array of source integers
      INTEGER,  DIMENSION(:,:,:), POINTER     ::   data_jpj     ! array of source integers
      REAL(wp), DIMENSION(:,:,:), POINTER     ::   data_wgt     ! array of weights on model grid
      REAL(wp), DIMENSION(:,:,:), POINTER     ::   fly_dta      ! array of values on input grid
      REAL(wp), DIMENSION(:,:,:), POINTER     ::   col          ! temporary array for reading in columns
   END TYPE WGT

   INTEGER,     PARAMETER             ::   tot_wgts = 10
   TYPE( WGT ), DIMENSION(tot_wgts)   ::   ref_wgts     ! array of wgts
   INTEGER                            ::   nxt_wgt = 1  ! point to next available space in ref_wgts array

!$AGRIF_END_DO_NOT_TREAT

   PUBLIC   fld_read, fld_fill   ! called by sbc... modules

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: fldread.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE fld_read( kt, kn_fsbc, sd, map, jit, time_offset )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE fld_read  ***
      !!                   
      !! ** Purpose :   provide at each time step the surface ocean fluxes
      !!                (momentum, heat, freshwater and runoff) 
      !!
      !! ** Method  :   READ each input fields in NetCDF files using IOM
      !!      and intepolate it to the model time-step.
      !!         Several assumptions are made on the input file:
      !!      blahblahblah....
      !!----------------------------------------------------------------------
      INTEGER  , INTENT(in   )               ::   kt        ! ocean time step
      INTEGER  , INTENT(in   )               ::   kn_fsbc   ! sbc computation period (in time step) 
      TYPE(FLD), INTENT(inout), DIMENSION(:) ::   sd        ! input field related variables
      TYPE(MAP_POINTER),INTENT(in), OPTIONAL, DIMENSION(:) ::   map   ! global-to-local mapping index
      INTEGER  , INTENT(in   ), OPTIONAL     ::   jit       ! subcycle timestep for timesplitting option
      INTEGER  , INTENT(in   ), OPTIONAL     ::   time_offset ! provide fields at time other than "now"
                                                              ! time_offset = -1 => fields at "before" time level
                                                              ! time_offset = +1 => fields at "after" time levels
                                                              ! etc.
      !!
      INTEGER  ::   imf        ! size of the structure sd
      INTEGER  ::   jf         ! dummy indices
      INTEGER  ::   ireclast   ! last record to be read in the current year file
      INTEGER  ::   isecend    ! number of second since Jan. 1st 00h of nit000 year at nitend
      INTEGER  ::   isecsbc    ! number of seconds between Jan. 1st 00h of nit000 year and the middle of sbc time step
      INTEGER  ::   itime_add  ! local time offset variable
      LOGICAL  ::   llnxtyr    ! open next year  file?
      LOGICAL  ::   llnxtmth   ! open next month file?
      LOGICAL  ::   llstop     ! stop is the file does not exist
      LOGICAL  ::   ll_firstcall ! true if this is the first call to fld_read for this set of fields
      REAL(wp) ::   ztinta     ! ratio applied to after  records when doing time interpolation
      REAL(wp) ::   ztintb     ! ratio applied to before records when doing time interpolation
      CHARACTER(LEN=1000) ::   clfmt   ! write format
      !!---------------------------------------------------------------------
      ll_firstcall = .false.
      IF( PRESENT(jit) ) THEN
         IF(kt == nit000 .and. jit == 1) ll_firstcall = .true.
      ELSE
         IF(kt == nit000) ll_firstcall = .true.
      ENDIF

      itime_add = 0
      IF( PRESENT(time_offset) ) itime_add = time_offset
         
      ! Note that shifting time to be centrered in the middle of sbc time step impacts only nsec_* variables of the calendar 
      IF( present(jit) ) THEN 
         ! ignore kn_fsbc in this case
         isecsbc = nsec_year + nsec1jan000 + (jit+itime_add)*rdt/REAL(nn_baro,wp) 
      ELSE
         isecsbc = nsec_year + nsec1jan000 + NINT(0.5 * REAL(kn_fsbc - 1,wp) * rdttra(1)) + itime_add * rdttra(1)  ! middle of sbc time step
      ENDIF
      imf = SIZE( sd )
      !
      IF( ll_firstcall ) THEN                      ! initialization
         IF( PRESENT(map) ) THEN
            DO jf = 1, imf 
               CALL fld_init( kn_fsbc, sd(jf), map(jf)%ptr )  ! read each before field (put them in after as they will be swapped)
            END DO
         ELSE
            DO jf = 1, imf 
               CALL fld_init( kn_fsbc, sd(jf) )       ! read each before field (put them in after as they will be swapped)
            END DO
         ENDIF
         IF( lwp ) CALL wgt_print()                ! control print
         CALL fld_rot( kt, sd )                    ! rotate vector fiels if needed
      ENDIF
      !                                            ! ====================================== !
      IF( MOD( kt-1, kn_fsbc ) == 0 ) THEN         ! update field at each kn_fsbc time-step !
         !                                         ! ====================================== !
         !
         DO jf = 1, imf                            ! ---   loop over field   --- !
            
            IF( isecsbc > sd(jf)%nrec_a(2) .OR. ll_firstcall ) THEN  ! read/update the after data?

               IF( sd(jf)%ln_tint ) THEN                             ! swap before record field and informations
                  sd(jf)%nrec_b(:) = sd(jf)%nrec_a(:)
!CDIR COLLAPSE
                  sd(jf)%fdta(:,:,:,1) = sd(jf)%fdta(:,:,:,2)
               ENDIF

               IF( PRESENT(jit) ) THEN
                  CALL fld_rec( kn_fsbc, sd(jf), time_offset=itime_add, jit=jit )              ! update record informations
               ELSE
                  CALL fld_rec( kn_fsbc, sd(jf), time_offset=itime_add )                       ! update record informations
               ENDIF

               ! do we have to change the year/month/week/day of the forcing field?? 
               IF( sd(jf)%ln_tint ) THEN
                  ! if we do time interpolation we will need to open next year/month/week/day file before the end of the current
                  ! one. If so, we are still before the end of the year/month/week/day when calling fld_rec so sd(jf)%nrec_a(1)
                  ! will be larger than the record number that should be read for current year/month/week/day

                  ! last record to be read in the current file
                  IF    ( sd(jf)%nfreqh == -12 ) THEN                 ;   ireclast = 1    !  yearly mean
                  ELSEIF( sd(jf)%nfreqh ==  -1 ) THEN                                     ! monthly mean
                     IF(     sd(jf)%cltype      == 'monthly' ) THEN   ;   ireclast = 1
                     ELSE                                             ;   ireclast = 12
                     ENDIF
                  ELSE                                                                    ! higher frequency mean (in hours)
                     IF(     sd(jf)%cltype      == 'monthly' ) THEN   ;   ireclast = 24 * nmonth_len(nmonth) / sd(jf)%nfreqh 
                     ELSEIF( sd(jf)%cltype(1:4) == 'week'    ) THEN   ;   ireclast = 24 * 7                  / sd(jf)%nfreqh
                     ELSEIF( sd(jf)%cltype      == 'daily'   ) THEN   ;   ireclast = 24                      / sd(jf)%nfreqh
                     ELSE                                             ;   ireclast = 24 * nyear_len(     1 ) / sd(jf)%nfreqh 
                     ENDIF
                  ENDIF

                  ! do we need next file data?
                  IF( sd(jf)%nrec_a(1) > ireclast ) THEN

                     sd(jf)%nrec_a(1) = 1              ! force to read the first record of the next file

                     IF( .NOT. sd(jf)%ln_clim ) THEN   ! close the current file and open a new one.

                        llnxtmth = sd(jf)%cltype == 'monthly' .OR. nday == nmonth_len(nmonth)      ! open next month file?
                        llnxtyr  = sd(jf)%cltype == 'yearly'  .OR. (nmonth == 12 .AND. llnxtmth)   ! open next year  file?

                        ! if the run finishes at the end of the current year/month/week/day, we will allow next
                        ! year/month/week/day file to be not present. If the run continue further than the current
                        ! year/month/week/day, next year/month/week/day file must exist
                        isecend = nsec_year + nsec1jan000 + (nitend - kt) * NINT(rdttra(1))   ! second at the end of the run 
                        llstop = isecend > sd(jf)%nrec_a(2)                                   ! read more than 1 record of next year

                        CALL fld_clopn( sd(jf), nyear  + COUNT((/llnxtyr /))                                           ,         &
                           &                    nmonth + COUNT((/llnxtmth/)) - 12                 * COUNT((/llnxtyr /)),         &
                           &                    nday   + 1                   - nmonth_len(nmonth) * COUNT((/llnxtmth/)), llstop )

                        IF( sd(jf)%num <= 0 .AND. .NOT. llstop ) THEN    ! next year file does not exist
                           CALL ctl_warn('next year/month/week/day file: '//TRIM(sd(jf)%clname)//     &
                              &     ' not present -> back to current year/month/day')
                           CALL fld_clopn( sd(jf), nyear, nmonth, nday )       ! back to the current year/month/day
                           sd(jf)%nrec_a(1) = ireclast     ! force to read the last record to be read in the current year file
                        ENDIF

                     ENDIF
                  ENDIF

               ELSE
                  ! if we are not doing time interpolation, we must change the year/month/week/day of the file just after
                  ! switching to the NEW year/month/week/day. If it is the case, we are at the beginning of the
                  ! year/month/week/day when calling fld_rec so sd(jf)%nrec_a(1) = 1
                  IF( sd(jf)%nrec_a(1) == 1 .AND. .NOT. ( sd(jf)%ln_clim .AND. sd(jf)%cltype == 'yearly' ) )   &
                     &   CALL fld_clopn( sd(jf), nyear, nmonth, nday )
               ENDIF

               ! read after data
               IF( PRESENT(map) ) THEN
                  CALL fld_get( sd(jf), map(jf)%ptr )
               ELSE
                  CALL fld_get( sd(jf) )
               ENDIF

            ENDIF
         END DO                                    ! --- end loop over field --- !

         CALL fld_rot( kt, sd )                    ! rotate vector fiels if needed

         DO jf = 1, imf                            ! ---   loop over field   --- !
            !
            IF( sd(jf)%ln_tint ) THEN              ! temporal interpolation
               IF(lwp .AND. kt - nit000 <= 100 ) THEN 
                  clfmt = "('fld_read: var ', a, ' kt = ', i8, ' (', f7.2,' days), Y/M/D = ', i4.4,'/', i2.2,'/', i2.2," //   &
                     &    "', records b/a: ', i4.4, '/', i4.4, ' (days ', f7.2,'/', f7.2, ')')"
                  WRITE(numout, clfmt)  TRIM( sd(jf)%clvar ), kt, REAL(isecsbc,wp)/rday, nyear, nmonth, nday,   &            
                     & sd(jf)%nrec_b(1), sd(jf)%nrec_a(1), REAL(sd(jf)%nrec_b(2),wp)/rday, REAL(sd(jf)%nrec_a(2),wp)/rday
                  WRITE(numout, *) 'itime_add is : ',itime_add
               ENDIF
               ! temporal interpolation weights
               ztinta =  REAL( isecsbc - sd(jf)%nrec_b(2), wp ) / REAL( sd(jf)%nrec_a(2) - sd(jf)%nrec_b(2), wp )
               ztintb =  1. - ztinta
!CDIR COLLAPSE
               sd(jf)%fnow(:,:,:) = ztintb * sd(jf)%fdta(:,:,:,1) + ztinta * sd(jf)%fdta(:,:,:,2)
            ELSE   ! nothing to do...
               IF(lwp .AND. kt - nit000 <= 100 ) THEN
                  clfmt = "('fld_read: var ', a, ' kt = ', i8,' (', f7.2,' days), Y/M/D = ', i4.4,'/', i2.2,'/', i2.2," //   &
                     &    "', record: ', i4.4, ' (days ', f7.2, ' <-> ', f7.2, ')')"
                  WRITE(numout, clfmt) TRIM(sd(jf)%clvar), kt, REAL(isecsbc,wp)/rday, nyear, nmonth, nday,    &
                     &                 sd(jf)%nrec_a(1), REAL(sd(jf)%nrec_b(2),wp)/rday, REAL(sd(jf)%nrec_a(2),wp)/rday
               ENDIF
            ENDIF
            !
            IF( kt == nitend - kn_fsbc + 1 )   CALL iom_close( sd(jf)%num )   ! Close the input files

         END DO                                    ! --- end loop over field --- !
         !
         !                                         ! ====================================== !
      ENDIF                                        ! update field at each kn_fsbc time-step !
      !                                            ! ====================================== !
      !
   END SUBROUTINE fld_read


   SUBROUTINE fld_init( kn_fsbc, sdjf, map )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE fld_init  ***
      !!
      !! ** Purpose :  - if time interpolation, read before data 
      !!               - open current year file
      !!----------------------------------------------------------------------
      INTEGER  , INTENT(in   ) ::   kn_fsbc   ! sbc computation period (in time step) 
      TYPE(FLD), INTENT(inout) ::   sdjf      ! input field related variables
      INTEGER  , INTENT(in), OPTIONAL, DIMENSION(:) :: map ! global-to-local mapping indices
      !!
      LOGICAL :: llprevyr              ! are we reading previous year  file?
      LOGICAL :: llprevmth             ! are we reading previous month file?
      LOGICAL :: llprevweek            ! are we reading previous week  file?
      LOGICAL :: llprevday             ! are we reading previous day   file?
      LOGICAL :: llprev                ! llprevyr .OR. llprevmth .OR. llprevweek .OR. llprevday
      INTEGER :: idvar                 ! variable id 
      INTEGER :: inrec                 ! number of record existing for this variable
      INTEGER :: iyear, imonth, iday   ! first day of the current file in yyyy mm dd
      INTEGER :: isec_week             ! number of seconds since start of the weekly file
      CHARACTER(LEN=1000) ::   clfmt   ! write format
      !!---------------------------------------------------------------------
      
      ! some default definitions...
      sdjf%num = 0   ! default definition for non-opened file
      IF( sdjf%ln_clim )   sdjf%clname = TRIM( sdjf%clrootname )   ! file name defaut definition, never change in this case
      llprevyr   = .FALSE.
      llprevmth  = .FALSE.
      llprevweek = .FALSE.
      llprevday  = .FALSE.
      isec_week  = 0
            
      IF( sdjf%cltype(1:4) == 'week' .AND. nn_leapy == 0 )   &
         &   CALL ctl_stop('fld_clopn: weekly file ('//TRIM(sdjf%clrootname)//') needs nn_leapy = 1')
      IF( sdjf%cltype(1:4) == 'week' .AND. sdjf%ln_clim  )   &
         &   CALL ctl_stop('fld_clopn: weekly file ('//TRIM(sdjf%clrootname)//') needs ln_clim = .FALSE.')

      ! define record informations
      CALL fld_rec( kn_fsbc, sdjf, ldbefore = .TRUE. )  ! return before values in sdjf%nrec_a (as we will swap it later)

      ! Note that shifting time to be centrered in the middle of sbc time step impacts only nsec_* variables of the calendar 

      IF( sdjf%ln_tint ) THEN ! we need to read the previous record and we will put it in the current record structure

         IF( sdjf%nrec_a(1) == 0  ) THEN   ! we redefine record sdjf%nrec_a(1) with the last record of previous year file
            IF    ( sdjf%nfreqh == -12 ) THEN   ! yearly mean
               IF( sdjf%cltype == 'yearly' ) THEN             ! yearly file
                  sdjf%nrec_a(1) = 1                                                       ! force to read the unique record
                  llprevyr  = .NOT. sdjf%ln_clim                                           ! use previous year  file?
               ELSE
                  CALL ctl_stop( "fld_init: yearly mean file must be in a yearly type of file: "//TRIM(sdjf%clname) )
               ENDIF
            ELSEIF( sdjf%nfreqh ==  -1 ) THEN   ! monthly mean
               IF( sdjf%cltype == 'monthly' ) THEN            ! monthly file
                  sdjf%nrec_a(1) = 1                                                       ! force to read the unique record
                  llprevmth = .TRUE.                                                       ! use previous month file?
                  llprevyr  = llprevmth .AND. nmonth == 1                                  ! use previous year  file?
               ELSE                                           ! yearly file
                  sdjf%nrec_a(1) = 12                                                      ! force to read december mean
                  llprevyr = .NOT. sdjf%ln_clim                                            ! use previous year  file?
               ENDIF
            ELSE                                ! higher frequency mean (in hours) 
               IF    ( sdjf%cltype      == 'monthly' ) THEN   ! monthly file
                  sdjf%nrec_a(1) = 24 * nmonth_len(nmonth-1) / sdjf%nfreqh                 ! last record of previous month
                  llprevmth = .TRUE.                                                       ! use previous month file?
                  llprevyr  = llprevmth .AND. nmonth == 1                                  ! use previous year  file?
               ELSEIF( sdjf%cltype(1:4) == 'week'    ) THEN   ! weekly file
                  llprevweek = .TRUE.                                                      ! use previous week  file?
                  sdjf%nrec_a(1) = 24 * 7 / sdjf%nfreqh                                    ! last record of previous week
                  isec_week = NINT(rday) * 7                                               ! add a shift toward previous week
               ELSEIF( sdjf%cltype      == 'daily'   ) THEN   ! daily file
                  sdjf%nrec_a(1) = 24 / sdjf%nfreqh                                        ! last record of previous day
                  llprevday = .TRUE.                                                       ! use previous day   file?
                  llprevmth = llprevday .AND. nday   == 1                                  ! use previous month file?
                  llprevyr  = llprevmth .AND. nmonth == 1                                  ! use previous year  file?
               ELSE                                           ! yearly file
                  sdjf%nrec_a(1) = 24 * nyear_len(0) / sdjf%nfreqh                         ! last record of previous year 
                  llprevyr = .NOT. sdjf%ln_clim                                            ! use previous year  file?
               ENDIF
            ENDIF
         ENDIF
         IF ( sdjf%cltype(1:4) == 'week' ) THEN
            isec_week = isec_week + ksec_week( sdjf%cltype(6:8) )   ! second since the beginning of the week
            llprevmth = isec_week > nsec_month                      ! longer time since the beginning of the week than the month
            llprevyr  = llprevmth .AND. nmonth == 1
         ENDIF
         llprev = llprevyr .OR. llprevmth .OR. llprevweek .OR. llprevday
         !
         iyear  = nyear  - COUNT((/llprevyr /))
         imonth = nmonth - COUNT((/llprevmth/)) + 12 * COUNT((/llprevyr /))
         iday   = nday   - COUNT((/llprevday/)) + nmonth_len(nmonth-1) * COUNT((/llprevmth/)) - isec_week / NINT(rday)
         !
         CALL fld_clopn( sdjf, iyear, imonth, iday, .NOT. llprev )

         ! if previous year/month/day file does not exist, we switch to the current year/month/day
         IF( llprev .AND. sdjf%num <= 0 ) THEN
            CALL ctl_warn( 'previous year/month/week/day file: '//TRIM(sdjf%clname)//   &
               &           ' not present -> back to current year/month/week/day' )
            ! we force to read the first record of the current year/month/day instead of last record of previous year/month/day
            llprev = .FALSE.
            sdjf%nrec_a(1) = 1
            CALL fld_clopn( sdjf, nyear, nmonth, nday )
         ENDIF
         
         IF( llprev ) THEN   ! check if the last record sdjf%nrec_n(1) exists in the file
            idvar = iom_varid( sdjf%num, sdjf%clvar )                                        ! id of the variable sdjf%clvar
            IF( idvar <= 0 )   RETURN
            inrec = iom_file( sdjf%num )%dimsz( iom_file( sdjf%num )%ndims(idvar), idvar )   ! size of the last dim of idvar
            sdjf%nrec_a(1) = MIN( sdjf%nrec_a(1), inrec )   ! make sure we select an existing record
         ENDIF

         ! read before data 
         IF( PRESENT(map) ) THEN
            CALL fld_get( sdjf, map )  ! read before values in after arrays(as we will swap it later)
         ELSE
            CALL fld_get( sdjf )  ! read before values in after arrays(as we will swap it later)
         ENDIF

         clfmt = "('fld_init : time-interpolation for ', a, ' read previous record = ', i4, ' at time = ', f7.2, ' days')"
         IF(lwp) WRITE(numout, clfmt) TRIM(sdjf%clvar), sdjf%nrec_a(1), REAL(sdjf%nrec_a(2),wp)/rday

         IF( llprev )   CALL iom_close( sdjf%num )          ! force to close previous year file (-> redefine sdjf%num to 0)

      ENDIF

      ! make sure current year/month/day file is opened
      IF( sdjf%num <= 0 ) THEN
         !
         IF ( sdjf%cltype(1:4) == 'week' ) THEN
            isec_week  = ksec_week( sdjf%cltype(6:8) )      ! second since the beginning of the week
            llprevmth  = isec_week > nsec_month             ! longer time since beginning of the week than the month
            llprevyr   = llprevmth .AND. nmonth == 1
         ELSE
            isec_week  = 0
            llprevmth  = .FALSE.
            llprevyr   = .FALSE.
         ENDIF
         !
         iyear  = nyear  - COUNT((/llprevyr /))
         imonth = nmonth - COUNT((/llprevmth/)) + 12 * COUNT((/llprevyr /))
         iday   = nday   + nmonth_len(nmonth-1) * COUNT((/llprevmth/)) - isec_week / NINT(rday)
         !
         CALL fld_clopn( sdjf, iyear, imonth, iday )
      ENDIF 
      !
   END SUBROUTINE fld_init


   SUBROUTINE fld_rec( kn_fsbc, sdjf, ldbefore, jit, time_offset )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE fld_rec  ***
      !!
      !! ** Purpose : Compute
      !!              if sdjf%ln_tint = .TRUE.
      !!                  nrec_a: record number and its time (nrec_b is obtained from nrec_a when swapping)
      !!              if sdjf%ln_tint = .FALSE.
      !!                  nrec_a(1): record number
      !!                  nrec_b(2) and nrec_a(2): time of the beginning and end of the record (for print only)
      !!----------------------------------------------------------------------
      INTEGER  , INTENT(in   )           ::   kn_fsbc   ! sbc computation period (in time step) 
      TYPE(FLD), INTENT(inout)           ::   sdjf      ! input field related variables
      LOGICAL  , INTENT(in   ), OPTIONAL ::   ldbefore  ! sent back before record values (default = .FALSE.)
      INTEGER  , INTENT(in   ), OPTIONAL ::   jit       ! index of barotropic subcycle
                                                        ! used only if sdjf%ln_tint = .TRUE.
      INTEGER  , INTENT(in   ), OPTIONAL ::   time_offset  ! Offset of required time level compared to "now"
                                                           ! time level in units of time steps.
      !!
      LOGICAL  ::   llbefore    ! local definition of ldbefore
      INTEGER  ::   iendrec     ! end of this record (in seconds)
      INTEGER  ::   imth        ! month number
      INTEGER  ::   ifreq_sec   ! frequency mean (in seconds)
      INTEGER  ::   isec_week   ! number of seconds since the start of the weekly file
      INTEGER  ::   itime_add   ! local time offset variable
      REAL(wp) ::   ztmp        ! temporary variable
      !!----------------------------------------------------------------------
      !
      ! Note that shifting time to be centrered in the middle of sbc time step impacts only nsec_* variables of the calendar 
      !
      IF( PRESENT(ldbefore) ) THEN   ;   llbefore = ldbefore .AND. sdjf%ln_tint   ! needed only if sdjf%ln_tint = .TRUE.
      ELSE                           ;   llbefore = .FALSE.
      ENDIF
      !
      itime_add = 0
      IF( PRESENT(time_offset) ) itime_add = time_offset
      !
      !                                      ! =========== !
      IF    ( sdjf%nfreqh == -12 ) THEN      ! yearly mean
         !                                   ! =========== !
         !
         IF( sdjf%ln_tint ) THEN                 ! time interpolation, shift by 1/2 record
            !
            !                  INT( ztmp )
            !                     /|\
            !                    1 |    *----
            !                    0 |----(              
            !                      |----+----|--> time
            !                      0   /|\   1   (nday/nyear_len(1))
            !                           |   
            !                           |   
            !       forcing record :    1 
            !                            
            ztmp = REAL( nday, wp ) / REAL( nyear_len(1), wp ) + 0.5
            IF( PRESENT(jit) ) THEN 
               ztmp = ztmp + (jit+itime_add)*rdt/REAL(nn_baro,wp)
            ELSE
               ztmp = ztmp + itime_add*rdttra(1)
            ENDIF
            sdjf%nrec_a(1) = 1 + INT( ztmp ) - COUNT((/llbefore/))
            ! swap at the middle of the year
            IF( llbefore ) THEN   ;   sdjf%nrec_a(2) = nsec1jan000 - NINT(0.5 * rday) * nyear_len(0)
            ELSE                  ;   sdjf%nrec_a(2) = nsec1jan000 + NINT(0.5 * rday) * nyear_len(1)   
            ENDIF
         ELSE                                    ! no time interpolation
            sdjf%nrec_a(1) = 1
            sdjf%nrec_a(2) = NINT(rday) * nyear_len(1) + nsec1jan000   ! swap at the end    of the year
            sdjf%nrec_b(2) = nsec1jan000                               ! beginning of the year (only for print)
         ENDIF
         !
         !                                   ! ============ !
      ELSEIF( sdjf%nfreqh ==  -1 ) THEN      ! monthly mean !
         !                                   ! ============ !
         !
         IF( sdjf%ln_tint ) THEN                 ! time interpolation, shift by 1/2 record
            !
            !                  INT( ztmp )
            !                     /|\
            !                    1 |    *----
            !                    0 |----(              
            !                      |----+----|--> time
            !                      0   /|\   1   (nday/nmonth_len(nmonth))
            !                           |   
            !                           |   
            !       forcing record :  nmonth 
            !                            
            ztmp = REAL( nday, wp ) / REAL( nmonth_len(nmonth), wp ) + 0.5
            IF( PRESENT(jit) ) THEN 
               ztmp = ztmp + (jit+itime_add)*rdt/REAL(nn_baro,wp)
            ELSE
               ztmp = ztmp + itime_add*rdttra(1)
            ENDIF
            imth = nmonth + INT( ztmp ) - COUNT((/llbefore/))
            IF( sdjf%cltype == 'monthly' ) THEN   ;   sdjf%nrec_a(1) = 1 + INT( ztmp ) - COUNT((/llbefore/))
            ELSE                                  ;   sdjf%nrec_a(1) = imth
            ENDIF
            sdjf%nrec_a(2) = nmonth_half(   imth ) + nsec1jan000   ! swap at the middle of the month
         ELSE                                    ! no time interpolation
            IF( sdjf%cltype == 'monthly' ) THEN   ;   sdjf%nrec_a(1) = 1
            ELSE                                  ;   sdjf%nrec_a(1) = nmonth
            ENDIF
            sdjf%nrec_a(2) =  nmonth_end(nmonth  ) + nsec1jan000   ! swap at the end    of the month
            sdjf%nrec_b(2) =  nmonth_end(nmonth-1) + nsec1jan000   ! beginning of the month (only for print)
         ENDIF
         !
         !                                   ! ================================ !
      ELSE                                   ! higher frequency mean (in hours)
         !                                   ! ================================ !
         !
         ifreq_sec = sdjf%nfreqh * 3600                                                 ! frequency mean (in seconds)
         IF( sdjf%cltype(1:4) == 'week' )   isec_week = ksec_week( sdjf%cltype(6:8) )   ! since the first day of the current week
         ! number of second since the beginning of the file
         IF(     sdjf%cltype      == 'monthly' ) THEN   ;   ztmp = REAL(nsec_month,wp)  ! since the first day of the current month
         ELSEIF( sdjf%cltype(1:4) == 'week'    ) THEN   ;   ztmp = REAL(isec_week ,wp)  ! since the first day of the current week
         ELSEIF( sdjf%cltype      == 'daily'   ) THEN   ;   ztmp = REAL(nsec_day  ,wp)  ! since 00h of the current day
         ELSE                                           ;   ztmp = REAL(nsec_year ,wp)  ! since 00h on Jan 1 of the current year
         ENDIF
         ztmp = ztmp + 0.5 * REAL(kn_fsbc - 1, wp) * rdttra(1)   ! shift time to be centrered in the middle of sbc time step
         ztmp = ztmp + 0.01 * rdttra(1)                          ! add 0.01 time step to avoid truncation error 
         IF( PRESENT(jit) ) THEN 
            ztmp = ztmp + (jit+itime_add)*rdt/REAL(nn_baro,wp)
         ELSE
            ztmp = ztmp + itime_add*rdttra(1)
         ENDIF
         IF( sdjf%ln_tint ) THEN                ! time interpolation, shift by 1/2 record
            !
            !                  INT( ztmp )
            !                     /|\
            !                    2 |        *-----(
            !                    1 |  *-----(
            !                    0 |--(              
            !                      |--+--|--+--|--+--|--> time
            !                      0 /|\ 1 /|\ 2 /|\ 3 (nsec_year/ifreq_sec) or (nsec_month/ifreq_sec)
            !                         |     |     |
            !                         |     |     |
            !       forcing record :  1     2     3
            !                   
            ztmp= ztmp / REAL(ifreq_sec, wp) + 0.5
         ELSE                                   ! no time interpolation
            !
            !                  INT( ztmp )
            !                     /|\
            !                    2 |           *-----(
            !                    1 |     *-----(
            !                    0 |-----(              
            !                      |--+--|--+--|--+--|--> time
            !                      0 /|\ 1 /|\ 2 /|\ 3 (nsec_year/ifreq_sec) or (nsec_month/ifreq_sec)
            !                         |     |     |
            !                         |     |     |
            !       forcing record :  1     2     3
            !                            
            ztmp= ztmp / REAL(ifreq_sec, wp)
         ENDIF
         sdjf%nrec_a(1) = 1 + INT( ztmp ) - COUNT((/llbefore/))   ! record nomber to be read

         iendrec = ifreq_sec * sdjf%nrec_a(1) + nsec1jan000       ! end of this record (in second)
         ! add the number of seconds between 00h Jan 1 and the end of previous month/week/day (ok if nmonth=1)
         IF( sdjf%cltype      == 'monthly' )   iendrec = iendrec + NINT(rday) * SUM(nmonth_len(1:nmonth -1))
         IF( sdjf%cltype(1:4) == 'week'    )   iendrec = iendrec + ( nsec_year - isec_week )
         IF( sdjf%cltype      == 'daily'   )   iendrec = iendrec + NINT(rday) * ( nday_year - 1 )
         IF( sdjf%ln_tint ) THEN
             sdjf%nrec_a(2) = iendrec - ifreq_sec / 2        ! swap at the middle of the record
         ELSE
             sdjf%nrec_a(2) = iendrec                        ! swap at the end    of the record
             sdjf%nrec_b(2) = iendrec - ifreq_sec            ! beginning of the record (only for print)
         ENDIF
         !
      ENDIF
      !
   END SUBROUTINE fld_rec


   SUBROUTINE fld_get( sdjf, map )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE fld_get  ***
      !!
      !! ** Purpose :   read the data
      !!----------------------------------------------------------------------
      TYPE(FLD), INTENT(inout) ::   sdjf   ! input field related variables
      INTEGER  , INTENT(in), OPTIONAL, DIMENSION(:) :: map ! global-to-local mapping indices
      !!
      INTEGER   ::   ipi,ipj    ! size of the structure along i and j direction
      INTEGER   ::   ipk        ! number of vertical levels of sdjf%fdta ( 2D: ipk=1 ; 3D: ipk=jpk )
      INTEGER   ::   iw         ! index into wgts array
      INTEGER   ::   jk         ! 
      INTEGER, DIMENSION(2)   ::  istart, icount
      !!---------------------------------------------------------------------
            
      ipk = SIZE( sdjf%fnow, 3 )

      IF( PRESENT(map) ) THEN
         IF( sdjf%ln_tint ) THEN   ;   CALL fld_map( sdjf%num, sdjf%clvar, sdjf%fdta(:,:,:,2), sdjf%nrec_a(1), map )
         ELSE                      ;   CALL fld_map( sdjf%num, sdjf%clvar, sdjf%fnow(:,:,:  ), sdjf%nrec_a(1), map )
         ENDIF
      ELSE IF( LEN(TRIM(sdjf%wgtname)) > 0 ) THEN
         CALL wgt_list( sdjf, iw )
         IF( sdjf%ln_tint ) THEN   ;   CALL fld_interp( sdjf%num, sdjf%clvar, iw , ipk  , sdjf%fdta(:,:,:,2), sdjf%nrec_a(1) )
         ELSE                      ;   CALL fld_interp( sdjf%num, sdjf%clvar, iw , ipk  , sdjf%fnow(:,:,:  ), sdjf%nrec_a(1) )
         ENDIF
      ELSE
         SELECT CASE( ipk )
         CASE(1)
            ipi = SIZE( sdjf%fnow, 1 )
            ipj = SIZE( sdjf%fnow, 2 )
            IF ( ipi == jpi .AND. ipj == jpj ) THEN
               IF( sdjf%ln_tint ) THEN   ;   CALL iom_get( sdjf%num, jpdom_data, sdjf%clvar, sdjf%fdta(:,:,1,2), sdjf%nrec_a(1) )
               ELSE                      ;   CALL iom_get( sdjf%num, jpdom_data, sdjf%clvar, sdjf%fnow(:,:,1  ), sdjf%nrec_a(1) )
               ENDIF
            ELSE
               IF     ( ipi == jpi .AND. ipj == jpk ) THEN
                  istart = (/ mig(1),   1 /)
                  icount = (/   nlci, jpk /)
                  IF( sdjf%ln_tint ) THEN
                     CALL iom_get( sdjf%num, jpdom_unknown, sdjf%clvar, sdjf%fdta(1:nlci,:,1,2), sdjf%nrec_a(1), istart, icount )
                     DO jk = 1, jpk   ;   sdjf%fdta(nlci+1:jpi,jk,1,2) = sdjf%fdta(nlci,jk,1,2)   ;   END DO
                  ELSE
                     CALL iom_get( sdjf%num, jpdom_unknown, sdjf%clvar, sdjf%fnow(1:nlci,:,1  ), sdjf%nrec_a(1), istart, icount )
                     DO jk = 1, jpk   ;   sdjf%fnow(nlci+1:jpi,jk,1  ) = sdjf%fnow(nlci,jk,1  )   ;   END DO
                  ENDIF
               ELSEIF ( ipi == jpj   .AND. ipj == jpk ) THEN
                  istart = (/ mjg(1),   1 /)
                  icount = (/   nlcj, jpk /)
                  IF( sdjf%ln_tint ) THEN
                     CALL iom_get( sdjf%num, jpdom_unknown, sdjf%clvar, sdjf%fdta(1:nlcj,:,1,2), sdjf%nrec_a(1), istart, icount )
                     DO jk = 1, jpk   ;   sdjf%fdta(nlcj+1:jpj,jk,1,2) = sdjf%fdta(nlcj,jk,1,2)   ;   END DO
                  ELSE
                     CALL iom_get( sdjf%num, jpdom_unknown, sdjf%clvar, sdjf%fnow(1:nlcj,:,1  ), sdjf%nrec_a(1), istart, icount )
                     DO jk = 1, jpk   ;   sdjf%fnow(nlcj+1:jpj,jk,1  ) = sdjf%fnow(nlcj,jk,1  )   ;   END DO
                  ENDIF
               ELSE
                  CALL ctl_stop('fld_get : case not coded...')
               ENDIF
            ENDIF
         CASE DEFAULT
            IF( sdjf%ln_tint ) THEN   ;   CALL iom_get( sdjf%num, jpdom_data, sdjf%clvar, sdjf%fdta(:,:,:,2), sdjf%nrec_a(1) )
            ELSE                      ;   CALL iom_get( sdjf%num, jpdom_data, sdjf%clvar, sdjf%fnow(:,:,:  ), sdjf%nrec_a(1) )
            ENDIF
         END SELECT
      ENDIF
      !
      sdjf%rotn = .false.   ! vector not yet rotated

   END SUBROUTINE fld_get

   SUBROUTINE fld_map( num, clvar, dta, nrec, map )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE fld_get  ***
      !!
      !! ** Purpose :   read global data from file and map onto local data
      !!                using a general mapping (for open boundaries)
      !!----------------------------------------------------------------------
#if defined key_bdy
      USE bdy_oce, ONLY:  dta_global         ! workspace to read in global data arrays
#endif 

      INTEGER                   , INTENT(in ) ::   num     ! stream number
      CHARACTER(LEN=*)          , INTENT(in ) ::   clvar   ! variable name
      REAL(wp), DIMENSION(:,:,:), INTENT(out) ::   dta   ! output field on model grid (2 dimensional)
      INTEGER                   , INTENT(in ) ::   nrec    ! record number to read (ie time slice)
      INTEGER,  DIMENSION(:)    , INTENT(in ) ::   map     ! global-to-local mapping indices
      !!
      INTEGER                                 ::   ipi      ! length of boundary data on local process
      INTEGER                                 ::   ipj      ! length of dummy dimension ( = 1 )
      INTEGER                                 ::   ipk      ! number of vertical levels of dta ( 2D: ipk=1 ; 3D: ipk=jpk )
      INTEGER                                 ::   ilendta  ! length of data in file
      INTEGER                                 ::   idvar    ! variable ID
      INTEGER                                 ::   ib, ik   ! loop counters
      INTEGER                                 ::   ierr
      REAL(wp), POINTER, DIMENSION(:,:,:)     ::   dta_read ! work space for global data
      !!---------------------------------------------------------------------
            
#if defined key_bdy
      dta_read => dta_global
#endif

      ipi = SIZE( dta, 1 )
      ipj = 1
      ipk = SIZE( dta, 3 )

      idvar   = iom_varid( num, clvar )
      ilendta = iom_file(num)%dimsz(1,idvar)
      IF(lwp) WRITE(numout,*) 'Dim size for ',TRIM(clvar),' is ', ilendta
      IF(lwp) WRITE(numout,*) 'Number of levels for ',TRIM(clvar),' is ', ipk

      SELECT CASE( ipk )
      CASE(1)   
         CALL iom_get ( num, jpdom_unknown, clvar, dta_read(1:ilendta,1:ipj,1    ), nrec )
      CASE DEFAULT
         CALL iom_get ( num, jpdom_unknown, clvar, dta_read(1:ilendta,1:ipj,1:ipk), nrec )
      END SELECT
      !
      DO ib = 1, ipi
         DO ik = 1, ipk
            dta(ib,1,ik) =  dta_read(map(ib),1,ik)
         END DO
      END DO

   END SUBROUTINE fld_map


   SUBROUTINE fld_rot( kt, sd )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE fld_rot  ***
      !!
      !! ** Purpose :   Vector fields may need to be rotated onto the local grid direction
      !!----------------------------------------------------------------------
      INTEGER  , INTENT(in   )               ::   kt        ! ocean time step
      TYPE(FLD), INTENT(inout), DIMENSION(:) ::   sd        ! input field related variables
      !!
      INTEGER                           ::   ju, jv, jk   ! loop indices
      INTEGER                           ::   imf          ! size of the structure sd
      INTEGER                           ::   ill          ! character length
      INTEGER                           ::   iv           ! indice of V component
      REAL(wp), POINTER, DIMENSION(:,:) ::   utmp, vtmp   ! temporary arrays for vector rotation
      CHARACTER (LEN=100)               ::   clcomp       ! dummy weight name
      !!---------------------------------------------------------------------

      CALL wrk_alloc( jpi,jpj, utmp, vtmp )

      !! (sga: following code should be modified so that pairs arent searched for each time
      !
      imf = SIZE( sd )
      DO ju = 1, imf
         ill = LEN_TRIM( sd(ju)%vcomp )
         IF( ill > 0 .AND. .NOT. sd(ju)%rotn ) THEN   ! find vector rotations required             
             IF( sd(ju)%vcomp(1:1) == 'U' ) THEN      ! east-west component has symbolic name starting with 'U'
                ! look for the north-south component which has same symbolic name but with 'U' replaced with 'V'
                clcomp = 'V' // sd(ju)%vcomp(2:ill)   ! works even if ill == 1
                iv = -1
                DO jv = 1, imf
                  IF( TRIM(sd(jv)%vcomp) == TRIM(clcomp) )   iv = jv
                END DO
                IF( iv > 0 ) THEN   ! fields ju and iv are two components which need to be rotated together
                   DO jk = 1, SIZE( sd(ju)%fnow, 3 )
                      IF( sd(ju)%ln_tint )THEN
                         CALL rot_rep( sd(ju)%fdta(:,:,jk,2), sd(iv)%fdta(:,:,jk,2), 'T', 'en->i', utmp(:,:) )
                         CALL rot_rep( sd(ju)%fdta(:,:,jk,2), sd(iv)%fdta(:,:,jk,2), 'T', 'en->j', vtmp(:,:) )
                         sd(ju)%fdta(:,:,jk,2) = utmp(:,:)   ;   sd(iv)%fdta(:,:,jk,2) = vtmp(:,:)
                      ELSE 
                         CALL rot_rep( sd(ju)%fnow(:,:,jk  ), sd(iv)%fnow(:,:,jk  ), 'T', 'en->i', utmp(:,:) )
                         CALL rot_rep( sd(ju)%fnow(:,:,jk  ), sd(iv)%fnow(:,:,jk  ), 'T', 'en->j', vtmp(:,:) )
                         sd(ju)%fnow(:,:,jk  ) = utmp(:,:)   ;   sd(iv)%fnow(:,:,jk  ) = vtmp(:,:)
                      ENDIF
                   END DO
                   sd(ju)%rotn = .TRUE.               ! vector was rotated 
                   IF( lwp .AND. kt == nit000 )   WRITE(numout,*)   &
                      &   'fld_read: vector pair ('//TRIM(sd(ju)%clvar)//', '//TRIM(sd(iv)%clvar)//') rotated on to model grid'
                ENDIF
             ENDIF
          ENDIF
       END DO
      !
      CALL wrk_dealloc( jpi,jpj, utmp, vtmp )
      !
   END SUBROUTINE fld_rot


   SUBROUTINE fld_clopn( sdjf, kyear, kmonth, kday, ldstop )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE fld_clopn  ***
      !!
      !! ** Purpose :   update the file name and open the file
      !!----------------------------------------------------------------------
      TYPE(FLD)        , INTENT(inout) ::   sdjf     ! input field related variables
      INTEGER          , INTENT(in   ) ::   kyear    ! year value
      INTEGER          , INTENT(in   ) ::   kmonth   ! month value
      INTEGER          , INTENT(in   ) ::   kday     ! day value
      LOGICAL, OPTIONAL, INTENT(in   ) ::   ldstop   ! stop if open to read a non-existing file (default = .TRUE.)
      !!----------------------------------------------------------------------

      IF( sdjf%num /= 0 )   CALL iom_close( sdjf%num )   ! close file if already open
      ! build the new filename if not climatological data
      sdjf%clname=TRIM(sdjf%clrootname)
      !
      ! note that sdjf%ln_clim is is only acting on presence of the year in the file
      IF( .NOT. sdjf%ln_clim ) THEN   
                                         WRITE(sdjf%clname, '(a,"_y",i4.4)' ) TRIM( sdjf%clrootname ), kyear    ! add year
         IF( sdjf%cltype /= 'yearly' )   WRITE(sdjf%clname, '(a,"m" ,i2.2)' ) TRIM( sdjf%clname     ), kmonth   ! add month
      ELSE
         ! build the new filename if climatological data
         IF( sdjf%cltype /= 'yearly' )   WRITE(sdjf%clname, '(a,"_m",i2.2)' ) TRIM( sdjf%clrootname ), kmonth   ! add month
      ENDIF
      IF( sdjf%cltype == 'daily' .OR. sdjf%cltype(1:4) == 'week' ) &
            &                            WRITE(sdjf%clname, '(a,"d" ,i2.2)' ) TRIM( sdjf%clname     ), kday     ! add day
      !
      CALL iom_open( sdjf%clname, sdjf%num, ldstop = ldstop, ldiof =  LEN(TRIM(sdjf%wgtname)) > 0 )
     !
   END SUBROUTINE fld_clopn


   SUBROUTINE fld_fill( sdf, sdf_n, cdir, cdcaller, cdtitle, cdnam )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE fld_fill  ***
      !!
      !! ** Purpose :   fill sdf with sdf_n and control print
      !!----------------------------------------------------------------------
      TYPE(FLD)  , DIMENSION(:), INTENT(inout) ::   sdf        ! structure of input fields (file informations, fields read)
      TYPE(FLD_N), DIMENSION(:), INTENT(in   ) ::   sdf_n      ! array of namelist information structures
      CHARACTER(len=*)         , INTENT(in   ) ::   cdir       ! Root directory for location of flx files
      CHARACTER(len=*)         , INTENT(in   ) ::   cdcaller   ! 
      CHARACTER(len=*)         , INTENT(in   ) ::   cdtitle    ! 
      CHARACTER(len=*)         , INTENT(in   ) ::   cdnam      ! 
      !
      INTEGER  ::   jf       ! dummy indices
      !!---------------------------------------------------------------------

      DO jf = 1, SIZE(sdf)
         sdf(jf)%clrootname = TRIM( cdir )//TRIM( sdf_n(jf)%clname )
         sdf(jf)%nfreqh     = sdf_n(jf)%nfreqh
         sdf(jf)%clvar      = sdf_n(jf)%clvar
         sdf(jf)%ln_tint    = sdf_n(jf)%ln_tint
         sdf(jf)%ln_clim    = sdf_n(jf)%ln_clim
         sdf(jf)%cltype     = sdf_n(jf)%cltype
         sdf(jf)%wgtname = " "
         IF( LEN( TRIM(sdf_n(jf)%wname) ) > 0 )   sdf(jf)%wgtname = TRIM( cdir )//TRIM( sdf_n(jf)%wname )
         sdf(jf)%vcomp   = sdf_n(jf)%vcomp
         sdf(jf)%rotn    = .TRUE.
      END DO

      IF(lwp) THEN      ! control print
         WRITE(numout,*)
         WRITE(numout,*) TRIM( cdcaller )//' : '//TRIM( cdtitle )
         WRITE(numout,*) (/ ('~', jf = 1, LEN_TRIM( cdcaller ) ) /)
         WRITE(numout,*) '          '//TRIM( cdnam )//' Namelist'
         WRITE(numout,*) '          list of files and frequency (>0: in hours ; <0 in months)'
         DO jf = 1, SIZE(sdf)
            WRITE(numout,*) '               root filename: '  , TRIM( sdf(jf)%clrootname ),   &
               &                          ' variable name: '  , TRIM( sdf(jf)%clvar      )
            WRITE(numout,*) '               frequency: '      ,       sdf(jf)%nfreqh      ,   &
               &                          ' time interp: '    ,       sdf(jf)%ln_tint     ,   &
               &                          ' climatology: '    ,       sdf(jf)%ln_clim     ,   &
               &                          ' weights    : '    , TRIM( sdf(jf)%wgtname    ),   &
               &                          ' pairing    : '    , TRIM( sdf(jf)%vcomp      ),   &
               &                          ' data type: '      ,       sdf(jf)%cltype
            call flush(numout)
         END DO
      ENDIF
      
   END SUBROUTINE fld_fill


   SUBROUTINE wgt_list( sd, kwgt )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE wgt_list  ***
      !!
      !! ** Purpose :   search array of WGTs and find a weights file
      !!                entry, or return a new one adding it to the end
      !!                if it is a new entry, the weights data is read in and
      !!                restructured (fld_weight)
      !!----------------------------------------------------------------------
      TYPE( FLD ), INTENT(in   ) ::   sd        ! field with name of weights file
      INTEGER    , INTENT(inout) ::   kwgt      ! index of weights
      !!
      INTEGER ::   kw, nestid   ! local integer
      LOGICAL ::   found        ! local logical
      !!----------------------------------------------------------------------
      !
      !! search down linked list 
      !! weights filename is either present or we hit the end of the list
      found = .FALSE.

      !! because agrif nest part of filenames are now added in iom_open
      !! to distinguish between weights files on the different grids, need to track
      !! nest number explicitly
      nestid = 0
#if defined key_agrif
      nestid = Agrif_Fixed()
#endif
      DO kw = 1, nxt_wgt-1
         IF( TRIM(ref_wgts(kw)%wgtname) == TRIM(sd%wgtname) .AND. &
             ref_wgts(kw)%nestid == nestid) THEN
            kwgt = kw
            found = .TRUE.
            EXIT
         ENDIF
      END DO
      IF( .NOT.found ) THEN
         kwgt = nxt_wgt
         CALL fld_weight( sd )
      ENDIF
      !
   END SUBROUTINE wgt_list


   SUBROUTINE wgt_print( )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE wgt_print  ***
      !!
      !! ** Purpose :   print the list of known weights
      !!----------------------------------------------------------------------
      INTEGER ::   kw   !
      !!----------------------------------------------------------------------
      !
      DO kw = 1, nxt_wgt-1
         WRITE(numout,*) 'weight file:  ',TRIM(ref_wgts(kw)%wgtname)
         WRITE(numout,*) '      ddims:  ',ref_wgts(kw)%ddims(1),ref_wgts(kw)%ddims(2)
         WRITE(numout,*) '     numwgt:  ',ref_wgts(kw)%numwgt
         WRITE(numout,*) '     jpiwgt:  ',ref_wgts(kw)%jpiwgt
         WRITE(numout,*) '     jpjwgt:  ',ref_wgts(kw)%jpjwgt
         WRITE(numout,*) '    botleft:  ',ref_wgts(kw)%botleft
         WRITE(numout,*) '   topright:  ',ref_wgts(kw)%topright
         IF( ref_wgts(kw)%cyclic ) THEN
            WRITE(numout,*) '       cyclical'
            IF( ref_wgts(kw)%overlap > 0 ) WRITE(numout,*) '              with overlap of ', ref_wgts(kw)%overlap
         ELSE
            WRITE(numout,*) '       not cyclical'
         ENDIF
         IF( ASSOCIATED(ref_wgts(kw)%data_wgt) )  WRITE(numout,*) '       allocated'
      END DO
      !
   END SUBROUTINE wgt_print


   SUBROUTINE fld_weight( sd )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE fld_weight  ***
      !!
      !! ** Purpose :   create a new WGT structure and fill in data from  
      !!                file, restructuring as required
      !!----------------------------------------------------------------------
      TYPE( FLD ), INTENT(in) ::   sd   ! field with name of weights file
      !!
      INTEGER                           ::   jn            ! dummy loop indices
      INTEGER                           ::   inum          ! temporary logical unit
      INTEGER                           ::   id            ! temporary variable id
      INTEGER                           ::   ipk           ! temporary vertical dimension
      CHARACTER (len=5)                 ::   aname
      INTEGER , DIMENSION(3)            ::   ddims
      INTEGER , POINTER, DIMENSION(:,:) ::   data_src
      REAL(wp), POINTER, DIMENSION(:,:) ::   data_tmp
      LOGICAL                           ::   cyclical
      INTEGER                           ::   zwrap      ! local integer
      !!----------------------------------------------------------------------
      !
      CALL wrk_alloc( jpi,jpj, data_src )   ! integer
      CALL wrk_alloc( jpi,jpj, data_tmp )
      !
      IF( nxt_wgt > tot_wgts ) THEN
        CALL ctl_stop("fld_weight: weights array size exceeded, increase tot_wgts")
      ENDIF
      !
      !! new weights file entry, add in extra information
      !! a weights file represents a 2D grid of a certain shape, so we assume that the current
      !! input data file is representative of all other files to be opened and processed with the
      !! current weights file

      !! open input data file (non-model grid)
      CALL iom_open( sd%clname, inum, ldiof =  LEN(TRIM(sd%wgtname)) > 0 )

      !! get dimensions
      id = iom_varid( inum, sd%clvar, ddims )

      !! close it
      CALL iom_close( inum )

      !! now open the weights file

      CALL iom_open ( sd%wgtname, inum )   ! interpolation weights
      IF ( inum > 0 ) THEN

         !! determine whether we have an east-west cyclic grid
         !! from global attribute called "ew_wrap" in the weights file
         !! note that if not found, iom_getatt returns -999 and cyclic with no overlap is assumed
         !! since this is the most common forcing configuration

         CALL iom_getatt(inum, 'ew_wrap', zwrap)
         IF( zwrap >= 0 ) THEN
            cyclical = .TRUE.
         ELSE IF( zwrap == -999 ) THEN
            cyclical = .TRUE.
            zwrap = 0
         ELSE
            cyclical = .FALSE.
         ENDIF

         ref_wgts(nxt_wgt)%ddims(1) = ddims(1)
         ref_wgts(nxt_wgt)%ddims(2) = ddims(2)
         ref_wgts(nxt_wgt)%wgtname = sd%wgtname
         ref_wgts(nxt_wgt)%overlap = zwrap
         ref_wgts(nxt_wgt)%cyclic = cyclical
         ref_wgts(nxt_wgt)%nestid = 0
#if defined key_agrif
         ref_wgts(nxt_wgt)%nestid = Agrif_Fixed()
#endif
         !! weights file is stored as a set of weights (wgt01->wgt04 or wgt01->wgt16)
         !! for each weight wgtNN there is an integer array srcNN which gives the point in
         !! the input data grid which is to be multiplied by the weight
         !! they are both arrays on the model grid so the result of the multiplication is
         !! added into an output array on the model grid as a running sum

         !! two possible cases: bilinear (4 weights) or bicubic (16 weights)
         id = iom_varid(inum, 'src05', ldstop=.FALSE.)
         IF( id <= 0) THEN
            ref_wgts(nxt_wgt)%numwgt = 4
         ELSE
            ref_wgts(nxt_wgt)%numwgt = 16
         ENDIF

         ALLOCATE( ref_wgts(nxt_wgt)%data_jpi(jpi,jpj,4) )
         ALLOCATE( ref_wgts(nxt_wgt)%data_jpj(jpi,jpj,4) )
         ALLOCATE( ref_wgts(nxt_wgt)%data_wgt(jpi,jpj,ref_wgts(nxt_wgt)%numwgt) )

         DO jn = 1,4
            aname = ' '
            WRITE(aname,'(a3,i2.2)') 'src',jn
            data_tmp(:,:) = 0
            CALL iom_get ( inum, jpdom_data, aname, data_tmp(:,:) )
            data_src(:,:) = INT(data_tmp(:,:))
            ref_wgts(nxt_wgt)%data_jpj(:,:,jn) = 1 + (data_src(:,:)-1) / ref_wgts(nxt_wgt)%ddims(1)
            ref_wgts(nxt_wgt)%data_jpi(:,:,jn) = data_src(:,:) - ref_wgts(nxt_wgt)%ddims(1)*(ref_wgts(nxt_wgt)%data_jpj(:,:,jn)-1)
         END DO

         DO jn = 1, ref_wgts(nxt_wgt)%numwgt
            aname = ' '
            WRITE(aname,'(a3,i2.2)') 'wgt',jn
            ref_wgts(nxt_wgt)%data_wgt(:,:,jn) = 0.0
            CALL iom_get ( inum, jpdom_data, aname, ref_wgts(nxt_wgt)%data_wgt(:,:,jn) )
         END DO
         CALL iom_close (inum)
 
         ! find min and max indices in grid
         ref_wgts(nxt_wgt)%botleft(1) = MINVAL(ref_wgts(nxt_wgt)%data_jpi(:,:,:))
         ref_wgts(nxt_wgt)%botleft(2) = MINVAL(ref_wgts(nxt_wgt)%data_jpj(:,:,:))
         ref_wgts(nxt_wgt)%topright(1) = MAXVAL(ref_wgts(nxt_wgt)%data_jpi(:,:,:))
         ref_wgts(nxt_wgt)%topright(2) = MAXVAL(ref_wgts(nxt_wgt)%data_jpj(:,:,:))

         ! and therefore dimensions of the input box
         ref_wgts(nxt_wgt)%jpiwgt = ref_wgts(nxt_wgt)%topright(1) - ref_wgts(nxt_wgt)%botleft(1) + 1
         ref_wgts(nxt_wgt)%jpjwgt = ref_wgts(nxt_wgt)%topright(2) - ref_wgts(nxt_wgt)%botleft(2) + 1

         ! shift indexing of source grid
         ref_wgts(nxt_wgt)%data_jpi(:,:,:) = ref_wgts(nxt_wgt)%data_jpi(:,:,:) - ref_wgts(nxt_wgt)%botleft(1) + 1
         ref_wgts(nxt_wgt)%data_jpj(:,:,:) = ref_wgts(nxt_wgt)%data_jpj(:,:,:) - ref_wgts(nxt_wgt)%botleft(2) + 1

         ! create input grid, give it a halo to allow gradient calculations
         ! SA: +3 stencil is a patch to avoid out-of-bound computation in some configuration. 
         ! a more robust solution will be given in next release
         ipk =  SIZE(sd%fnow, 3)
         ALLOCATE( ref_wgts(nxt_wgt)%fly_dta(ref_wgts(nxt_wgt)%jpiwgt+3, ref_wgts(nxt_wgt)%jpjwgt+3 ,ipk) )
         IF( ref_wgts(nxt_wgt)%cyclic ) ALLOCATE( ref_wgts(nxt_wgt)%col(1,ref_wgts(nxt_wgt)%jpjwgt+3,ipk) )

         nxt_wgt = nxt_wgt + 1

      ELSE 
         CALL ctl_stop( '    fld_weight : unable to read the file ' )
      ENDIF

      CALL wrk_dealloc( jpi,jpj, data_src )   ! integer
      CALL wrk_dealloc( jpi,jpj, data_tmp )
      !
   END SUBROUTINE fld_weight


   SUBROUTINE fld_interp( num, clvar, kw, kk, dta, nrec )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE fld_interp  ***
      !!
      !! ** Purpose :   apply weights to input gridded data to create data
      !!                on model grid
      !!----------------------------------------------------------------------
      INTEGER                   , INTENT(in   ) ::   num     ! stream number
      CHARACTER(LEN=*)          , INTENT(in   ) ::   clvar   ! variable name
      INTEGER                   , INTENT(in   ) ::   kw      ! weights number
      INTEGER                   , INTENT(in   ) ::   kk      ! vertical dimension of kk
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   dta     ! output field on model grid
      INTEGER                   , INTENT(in   ) ::   nrec    ! record number to read (ie time slice)
      !! 
      INTEGER, DIMENSION(3) ::   rec1,recn   ! temporary arrays for start and length
      INTEGER ::  jk, jn, jm           ! loop counters
      INTEGER ::  ni, nj               ! lengths
      INTEGER ::  jpimin,jpiwid        ! temporary indices
      INTEGER ::  jpjmin,jpjwid        ! temporary indices
      INTEGER ::  jpi1,jpi2,jpj1,jpj2  ! temporary indices
      !!----------------------------------------------------------------------
      !
      !! for weighted interpolation we have weights at four corners of a box surrounding 
      !! a model grid point, each weight is multiplied by a grid value (bilinear case)
      !! or by a grid value and gradients at the corner point (bicubic case) 
      !! so we need to have a 4 by 4 subgrid surrounding each model point to cover both cases

      !! sub grid from non-model input grid which encloses all grid points in this nemo process
      jpimin = ref_wgts(kw)%botleft(1)
      jpjmin = ref_wgts(kw)%botleft(2)
      jpiwid = ref_wgts(kw)%jpiwgt
      jpjwid = ref_wgts(kw)%jpjwgt

      !! when reading in, expand this sub-grid by one halo point all the way round for calculating gradients
      rec1(1) = MAX( jpimin-1, 1 )
      rec1(2) = MAX( jpjmin-1, 1 )
      rec1(3) = 1
      recn(1) = MIN( jpiwid+2, ref_wgts(kw)%ddims(1)-rec1(1)+1 )
      recn(2) = MIN( jpjwid+2, ref_wgts(kw)%ddims(2)-rec1(2)+1 )
      recn(3) = kk

      !! where we need to put it in the non-nemo grid fly_dta
      !! note that jpi1 and jpj1 only differ from 1 when jpimin and jpjmin are 1
      !! (ie at the extreme west or south of the whole input grid) and similarly for jpi2 and jpj2
      jpi1 = 2 + rec1(1) - jpimin
      jpj1 = 2 + rec1(2) - jpjmin
      jpi2 = jpi1 + recn(1) - 1
      jpj2 = jpj1 + recn(2) - 1

      ref_wgts(kw)%fly_dta(:,:,:) = 0.0
      SELECT CASE( SIZE(ref_wgts(kw)%fly_dta(jpi1:jpi2,jpj1:jpj2,:),3) )
      CASE(1)
           CALL iom_get( num, jpdom_unknown, clvar, ref_wgts(kw)%fly_dta(jpi1:jpi2,jpj1:jpj2,1), nrec, rec1, recn)
      CASE DEFAULT
           CALL iom_get( num, jpdom_unknown, clvar, ref_wgts(kw)%fly_dta(jpi1:jpi2,jpj1:jpj2,:), nrec, rec1, recn)
      END SELECT 

      !! first four weights common to both bilinear and bicubic
      !! data_jpi, data_jpj have already been shifted to (1,1) corresponding to botleft
      !! note that we have to offset by 1 into fly_dta array because of halo
      dta(:,:,:) = 0.0
      DO jk = 1,4
        DO jn = 1, jpj
          DO jm = 1,jpi
            ni = ref_wgts(kw)%data_jpi(jm,jn,jk)
            nj = ref_wgts(kw)%data_jpj(jm,jn,jk)
            dta(jm,jn,:) = dta(jm,jn,:) + ref_wgts(kw)%data_wgt(jm,jn,jk) * ref_wgts(kw)%fly_dta(ni+1,nj+1,:)
          END DO
        END DO
      END DO

      IF (ref_wgts(kw)%numwgt .EQ. 16) THEN

        !! fix up halo points that we couldnt read from file
        IF( jpi1 == 2 ) THEN
           ref_wgts(kw)%fly_dta(jpi1-1,:,:) = ref_wgts(kw)%fly_dta(jpi1,:,:)
        ENDIF
        IF( jpi2 + jpimin - 1 == ref_wgts(kw)%ddims(1)+1 ) THEN
           ref_wgts(kw)%fly_dta(jpi2+1,:,:) = ref_wgts(kw)%fly_dta(jpi2,:,:)
        ENDIF
        IF( jpj1 == 2 ) THEN
           ref_wgts(kw)%fly_dta(:,jpj1-1,:) = ref_wgts(kw)%fly_dta(:,jpj1,:)
        ENDIF
        IF( jpj2 + jpjmin - 1 == ref_wgts(kw)%ddims(2)+1 .AND. jpj2 .lt. jpjwid+2 ) THEN
           ref_wgts(kw)%fly_dta(:,jpj2+1,:) = 2.0*ref_wgts(kw)%fly_dta(:,jpj2,:) - ref_wgts(kw)%fly_dta(:,jpj2-1,:)
        ENDIF

        !! if data grid is cyclic we can do better on east-west edges
        !! but have to allow for whether first and last columns are coincident
        IF( ref_wgts(kw)%cyclic ) THEN
           rec1(2) = MAX( jpjmin-1, 1 )
           recn(1) = 1
           recn(2) = MIN( jpjwid+2, ref_wgts(kw)%ddims(2)-rec1(2)+1 )
           jpj1 = 2 + rec1(2) - jpjmin
           jpj2 = jpj1 + recn(2) - 1
           IF( jpi1 == 2 ) THEN
              rec1(1) = ref_wgts(kw)%ddims(1) - ref_wgts(kw)%overlap
              SELECT CASE( SIZE( ref_wgts(kw)%col(:,jpj1:jpj2,:),3) )
              CASE(1)
                   CALL iom_get( num, jpdom_unknown, clvar, ref_wgts(kw)%col(:,jpj1:jpj2,1), nrec, rec1, recn)
              CASE DEFAULT
                   CALL iom_get( num, jpdom_unknown, clvar, ref_wgts(kw)%col(:,jpj1:jpj2,:), nrec, rec1, recn)
              END SELECT      
              ref_wgts(kw)%fly_dta(jpi1-1,jpj1:jpj2,:) = ref_wgts(kw)%col(1,jpj1:jpj2,:)
           ENDIF
           IF( jpi2 + jpimin - 1 == ref_wgts(kw)%ddims(1)+1 ) THEN
              rec1(1) = 1 + ref_wgts(kw)%overlap
              SELECT CASE( SIZE( ref_wgts(kw)%col(:,jpj1:jpj2,:),3) )
              CASE(1)
                   CALL iom_get( num, jpdom_unknown, clvar, ref_wgts(kw)%col(:,jpj1:jpj2,1), nrec, rec1, recn)
              CASE DEFAULT
                   CALL iom_get( num, jpdom_unknown, clvar, ref_wgts(kw)%col(:,jpj1:jpj2,:), nrec, rec1, recn)
              END SELECT
              ref_wgts(kw)%fly_dta(jpi2+1,jpj1:jpj2,:) = ref_wgts(kw)%col(1,jpj1:jpj2,:)
           ENDIF
        ENDIF

        ! gradient in the i direction
        DO jk = 1,4
          DO jn = 1, jpj
            DO jm = 1,jpi
              ni = ref_wgts(kw)%data_jpi(jm,jn,jk)
              nj = ref_wgts(kw)%data_jpj(jm,jn,jk)
              dta(jm,jn,:) = dta(jm,jn,:) + ref_wgts(kw)%data_wgt(jm,jn,jk+4) * 0.5 *         &
                               (ref_wgts(kw)%fly_dta(ni+2,nj+1,:) - ref_wgts(kw)%fly_dta(ni,nj+1,:))
            END DO
          END DO
        END DO

        ! gradient in the j direction
        DO jk = 1,4
          DO jn = 1, jpj
            DO jm = 1,jpi
              ni = ref_wgts(kw)%data_jpi(jm,jn,jk)
              nj = ref_wgts(kw)%data_jpj(jm,jn,jk)
              dta(jm,jn,:) = dta(jm,jn,:) + ref_wgts(kw)%data_wgt(jm,jn,jk+8) * 0.5 *         &
                               (ref_wgts(kw)%fly_dta(ni+1,nj+2,:) - ref_wgts(kw)%fly_dta(ni+1,nj,:))
            END DO
          END DO
        END DO

         ! gradient in the ij direction
         DO jk = 1,4
            DO jn = 1, jpj
               DO jm = 1,jpi
                  ni = ref_wgts(kw)%data_jpi(jm,jn,jk)
                  nj = ref_wgts(kw)%data_jpj(jm,jn,jk)
                  dta(jm,jn,:) = dta(jm,jn,:) + ref_wgts(kw)%data_wgt(jm,jn,jk+12) * 0.25 * ( &
                               (ref_wgts(kw)%fly_dta(ni+2,nj+2,:) - ref_wgts(kw)%fly_dta(ni  ,nj+2,:)) -   &
                               (ref_wgts(kw)%fly_dta(ni+2,nj  ,:) - ref_wgts(kw)%fly_dta(ni  ,nj  ,:)))
               END DO
            END DO
         END DO
         !
      END IF
      !
   END SUBROUTINE fld_interp


   FUNCTION ksec_week( cdday )
      !!---------------------------------------------------------------------
      !!                    ***  FUNCTION kshift_week *** 
      !!
      !! ** Purpose :  
      !!---------------------------------------------------------------------
      CHARACTER(len=*), INTENT(in)   ::   cdday   !3 first letters of the first day of the weekly file
      !!
      INTEGER                        ::   ksec_week  ! output variable
      INTEGER                        ::   ijul       !temp variable
      INTEGER                        ::   ishift     !temp variable
      CHARACTER(len=3),DIMENSION(7)  ::   cl_week 
      !!----------------------------------------------------------------------
      cl_week = (/"sun","sat","fri","thu","wed","tue","mon"/)
      DO ijul = 1, 7
         IF( cl_week(ijul) == TRIM(cdday) ) EXIT
      END DO
      IF( ijul .GT. 7 )   CALL ctl_stop( 'ksec_week: wrong day for sdjf%cltype(6:8): '//TRIM(cdday) )
      !
      ishift = ijul * NINT(rday)
      ! 
      ksec_week = nsec_week + ishift
      ksec_week = MOD( ksec_week, 7*NINT(rday) )
      ! 
   END FUNCTION ksec_week

   !!======================================================================
END MODULE fldread
