MODULE iom_ioipsl
   !!=====================================================================
   !!                    ***  MODULE  iom_ioipsl ***
   !! Input/Output manager :  Library to read input files with IOIPSL (only fliocom module)
   !!====================================================================
   !! History :  9.0  ! 05 12  (J. Belier) Original code
   !!            9.0  ! 06 02  (S. Masson) Adaptation to NEMO
   !!             "   ! 07 07  (D. Storkey) Changes to iom_ioipsl_gettime
   !!--------------------------------------------------------------------
   !!gm  caution add !DIR nec: improved performance to be checked as well as no result changes

   !!--------------------------------------------------------------------
   !!   iom_open       : open a file read only
   !!   iom_close      : close a file or all files opened by iom
   !!   iom_get        : read a field (interfaced to several routines)
   !!   iom_gettime    : read the time axis kvid in the file
   !!   iom_varid      : get the id of a variable in a file
   !!   iom_rstput     : write a field in a restart file (interfaced to several routines)
   !!--------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE iom_def         ! iom variables definitions
   USE ioipsl          ! IOIPSL library
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC iom_ioipsl_open, iom_ioipsl_close, iom_ioipsl_varid, iom_ioipsl_get, iom_ioipsl_gettime, iom_ioipsl_rstput

   INTERFACE iom_ioipsl_get
      MODULE PROCEDURE iom_ioipsl_g0d, iom_ioipsl_g123d
   END INTERFACE
   INTERFACE iom_ioipsl_rstput
      MODULE PROCEDURE iom_ioipsl_rp0123d
   END INTERFACE
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: iom_ioipsl.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE iom_ioipsl_open( cdname, kiomid, ldwrt, ldok, kdompar )
      !!---------------------------------------------------------------------
      !!                   ***  SUBROUTINE  iom_open  ***
      !!
      !! ** Purpose :  open an input file with IOIPSL (only fliocom module)
      !!---------------------------------------------------------------------
      CHARACTER(len=*)       , INTENT(inout)           ::   cdname      ! File name
      INTEGER                , INTENT(  out)           ::   kiomid      ! ioipsl identifier of the opened file
      LOGICAL                , INTENT(in   )           ::   ldwrt       ! read or write the file?
      LOGICAL                , INTENT(in   )           ::   ldok        ! check the existence 
      INTEGER, DIMENSION(2,5), INTENT(in   ), OPTIONAL ::   kdompar     ! domain parameters: 

      CHARACTER(LEN=100) ::   clinfo      ! info character
      CHARACTER(LEN=10 ) ::   clstatus    ! status of opened file (REPLACE or NEW)
      INTEGER            ::   iln         ! lengths of character
      INTEGER            ::   istop       ! temporary storage of nstop
      INTEGER            ::   ifliodom    ! model domain identifier (see flio_dom_set)
      INTEGER            ::   ioipslid    ! ioipsl identifier of the opened file
      INTEGER            ::   jl          ! loop variable
      LOGICAL            ::   llclobber   ! local definition of ln_clobber
      !---------------------------------------------------------------------

      clinfo = '                    iom_ioipsl_open ~~~  '
      istop = nstop
      !
      llclobber = ldwrt .AND. ln_clobber
      IF( ldok .AND. .NOT. llclobber ) THEN      ! Open existing file...
         !                 ! =============
         IF( ldwrt ) THEN  ! ... in write mode
            IF(lwp) WRITE(numout,*) TRIM(clinfo)//' open existing file: '//TRIM(cdname)//' in WRITE mode'
            CALL flioopfd( TRIM(cdname), ioipslid, "WRITE" )
         ELSE              ! ... in read mode
            IF(lwp) WRITE(numout,*) TRIM(clinfo)//' open existing file: '//TRIM(cdname)//' in READ mode'
            CALL flioopfd( TRIM(cdname), ioipslid )
         ENDIF
      ELSE                 ! the file does not exist
         !                 ! =============
         iln = INDEX( cdname, '.nc' )
         IF( ldwrt ) THEN  ! the file should be open in write mode so we create it...
            IF( llclobber ) THEN   ;   clstatus = 'REPLACE 64' 
            ELSE                   ;   clstatus = 'NEW 64'
            ENDIF
            IF( jpnij > 1 ) THEN
               ! define the domain position regarding to the global domain (mainly useful in mpp)
               CALL flio_dom_set( jpnij, narea-1, (/1, 2/), (/jpiglo, jpjglo/)   &
                  &             , kdompar(:,1), kdompar(:,2), kdompar(:,3), kdompar(:,4), kdompar(:,5)   &
                  &             , 'BOX', ifliodom )        
               ! Note that fliocrfd may change the value of cdname (add the cpu number...)
               IF(lwp) WRITE(numout,*) TRIM(clinfo)//' create new file: '//cdname(1:iln-1)//'... in WRITE mode'
               CALL fliocrfd( cdname, (/'x'         , 'y'         , 'z', 't'/)   &
                  &         , (/kdompar(1,1), kdompar(2,1), jpk, -1 /), ioipslid, ifliodom, mode = clstatus )
            ELSE              ! the file should be open for read mode so it must exist...
               IF(lwp) WRITE(numout,*) TRIM(clinfo)//' create new file: '//cdname//' in WRITE mode'
               CALL fliocrfd( cdname, (/'x'         , 'y'         , 'z', 't'/)   &
                  &         , (/kdompar(1,1), kdompar(2,1), jpk, -1 /), ioipslid,           mode = clstatus )
            ENDIF
         ELSE              ! the file should be open for read mode so it must exist...
            CALL ctl_stop( TRIM(clinfo), ' should be impossible case...' )
         ENDIF
      ENDIF
      ! start to fill file informations
      ! =============
      IF( istop == nstop ) THEN   ! no error within this routine
!does not work with some compilers         kiomid = MINLOC(iom_file(:)%nfid, dim = 1)
         kiomid = 0
         DO jl = jpmax_files, 1, -1
            IF( iom_file(jl)%nfid == 0 )   kiomid = jl
         ENDDO
         iom_file(kiomid)%name   = TRIM(cdname)
         iom_file(kiomid)%nfid   = ioipslid
         iom_file(kiomid)%iolib  = jpioipsl
         iom_file(kiomid)%nvars  = 0
         iom_file(kiomid)%irec   = -1   ! useless for NetCDF files
         CALL flioinqf( ioipslid, id_uld = iom_file(kiomid)%iduld )
         IF(lwp) WRITE(numout,*) '                   ---> '//TRIM(cdname)//' OK'
      ELSE
         kiomid = 0               ! return error flag
      ENDIF
      !
   END SUBROUTINE iom_ioipsl_open


   SUBROUTINE iom_ioipsl_close( kiomid )
      !!--------------------------------------------------------------------
      !!                   ***  SUBROUTINE  iom_ioipsl_close  ***
      !!
      !! ** Purpose : close an input file with IOIPSL (only fliocom module) 
      !!--------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kiomid   ! iom identifier of the file to be closed
      !---------------------------------------------------------------------
      !
      CALL flioclo( iom_file(kiomid)%nfid )
      !    
   END SUBROUTINE iom_ioipsl_close


   FUNCTION iom_ioipsl_varid ( kiomid, cdvar, kiv, kdimsz )  
      !!-----------------------------------------------------------------------
      !!                  ***  FUNCTION  iom_varid  ***
      !!
      !! ** Purpose : get the id of a variable in a file with IOIPSL (only fliocom module)
      !!-----------------------------------------------------------------------
      INTEGER              , INTENT(in   )           ::   kiomid   ! file Identifier
      CHARACTER(len=*)     , INTENT(in   )           ::   cdvar    ! name of the variable
      INTEGER              , INTENT(in   )           ::   kiv   ! 
      INTEGER, DIMENSION(:), INTENT(  out), OPTIONAL ::   kdimsz   ! size of the dimensions
      !
      INTEGER                        ::   iom_ioipsl_varid  ! iom variable Id
      INTEGER                        ::   ioipslid          ! ioipsl file identifier
      INTEGER                        ::   ji                ! dummy loop index
      INTEGER                        ::   i_nvd             ! number of dimension of the variable
      INTEGER, DIMENSION(jpmax_dims) ::   idimid            ! dimension ids of the variable
      LOGICAL                        ::   ll_fnd            ! found test
      CHARACTER(LEN=100)             ::   clinfo            ! info character
      !!-----------------------------------------------------------------------
      clinfo = 'iom_ioipsl_varid, file: '//trim(iom_file(kiomid)%name)//', var: '//trim(cdvar)
      iom_ioipsl_varid = 0                  ! default definition
      IF( PRESENT(kdimsz) ) kdimsz(:) = 0   ! default definition
      ioipslid = iom_file(kiomid)%nfid      ! get back ioipsl file identifier 
      CALL flioinqv( ioipslid, cdvar, ll_fnd, nb_dims = i_nvd )   ! does the variable exist in the file
      IF( ll_fnd ) THEN
         IF( i_nvd <= jpmax_dims ) THEN
            iom_ioipsl_varid = kiv
            iom_file(kiomid)%nvars       = kiv
            iom_file(kiomid)%nvid(kiv)   = -1   ! variable id is not available in ioipsl
            iom_file(kiomid)%cn_var(kiv) = TRIM(cdvar)
            iom_file(kiomid)%ndims(kiv)  = i_nvd
            CALL flioinqv( ioipslid, cdvar, ll_fnd,   &
                  &           len_dims = iom_file(kiomid)%dimsz(1:i_nvd,kiv), &   ! dimensions size
                  &           id_dims  = idimid(1:i_nvd) )                        ! dimensions ids
            iom_file(kiomid)%luld(kiv) = .FALSE.   ! default value
            DO ji = 1, i_nvd                       ! find the unlimited dimension
               IF( idimid(ji) == iom_file(kiomid)%iduld ) iom_file(kiomid)%luld(kiv) = .TRUE.
            END DO
            !---------- Deal with scale_factor and add_offset
            CALL flioinqa( ioipslid, cdvar, 'scale_factor', ll_fnd )
            IF( ll_fnd) THEN
               CALL fliogeta( ioipslid, cdvar, 'scale_factor', iom_file(kiomid)%scf(kiv) )
            ELSE
               iom_file(kiomid)%scf(kiv) = 1.
            END IF
            CALL flioinqa( ioipslid, cdvar, 'add_offset', ll_fnd )
            IF( ll_fnd ) THEN
               CALL fliogeta( ioipslid, cdvar, 'add_offset', iom_file(kiomid)%ofs(kiv) )
            ELSE
               iom_file(kiomid)%ofs(kiv) = 0.
            END IF
            ! return the simension size
            IF( PRESENT(kdimsz) ) THEN 
               IF( i_nvd == SIZE(kdimsz) ) THEN
                  kdimsz(:) = iom_file(kiomid)%dimsz(1:i_nvd,kiv)
               ELSE
                  WRITE(ctmp1,*) i_nvd, SIZE(kdimsz)
                  CALL ctl_stop( TRIM(clinfo), 'error in kdimsz size'//TRIM(ctmp1) )
               ENDIF
            ENDIF
         ELSE
            CALL ctl_stop( TRIM(clinfo), 'Too many dimensions in the file '//iom_file(kiomid)%name,   &
                  &                       'increase the parameter jpmax_vars')
         ENDIF
      ELSE  
         iom_ioipsl_varid = -1   !   variable not found, return error code: -1
      ENDIF
      !
   END FUNCTION iom_ioipsl_varid


   SUBROUTINE iom_ioipsl_g0d( kiomid, kvid, pvar )
      !!-----------------------------------------------------------------------
      !!                  ***  ROUTINE  iom_ioipsl_g0d  ***
      !!
      !! ** Purpose : read a scalar with IOIPSL (only fliocom module)
      !!-----------------------------------------------------------------------
      INTEGER , INTENT(in   ) ::   kiomid    ! Identifier of the file
      INTEGER , INTENT(in   ) ::   kvid      ! variable id
      REAL(wp), INTENT(  out) ::   pvar      ! read field
      !
      CALL fliogetv( iom_file(kiomid)%nfid, TRIM(iom_file(kiomid)%cn_var(kvid)), pvar )
      ! 
   END SUBROUTINE iom_ioipsl_g0d


   SUBROUTINE iom_ioipsl_g123d( kiomid, kvid, knbdim, kstart, kcount, kx1, kx2, ky1, ky2,   &
         &                      pv_r1d, pv_r2d, pv_r3d)
      !!-----------------------------------------------------------------------
      !!                  ***  ROUTINE  iom_ioipsl_g123d  ***
      !!
      !! ** Purpose : read a 1D/2D/3D variable with IOIPSL (only fliocom module)
      !!
      !! ** Method : read ONE record at each CALL
      !!-----------------------------------------------------------------------
      INTEGER                    , INTENT(in   )           ::   kiomid     ! iom identifier of the file
      INTEGER                    , INTENT(in   )           ::   kvid       ! Name of the variable
      INTEGER                    , INTENT(in   )           ::   knbdim     ! number of dimensions of the variable
      INTEGER , DIMENSION(:)     , INTENT(in   )           ::   kstart     ! start position of the reading in each axis 
      INTEGER , DIMENSION(:)     , INTENT(in   )           ::   kcount     ! number of points to be read in each axis
      INTEGER ,                    INTENT(in   )           ::   kx1, kx2, ky1, ky2   ! subdomain indexes
      REAL(wp), DIMENSION(:)     , INTENT(  out), OPTIONAL ::   pv_r1d     ! read field (1D case)
      REAL(wp), DIMENSION(:,:)   , INTENT(  out), OPTIONAL ::   pv_r2d     ! read field (2D case)
      REAL(wp), DIMENSION(:,:,:) , INTENT(  out), OPTIONAL ::   pv_r3d     ! read field (3D case)
      !
      INTEGER               ::   ioipslid   ! ioipsl file identifier
      CHARACTER(LEN=100)    ::   clvn       ! variable name
      !---------------------------------------------------------------------
      clvn = TRIM(iom_file(kiomid)%cn_var(kvid))   ! get back variable name 
      ioipslid = iom_file(kiomid)%nfid             ! get back IPIPSL file id
      !
      IF( PRESENT(pv_r1d) ) THEN
         CALL fliogetv( ioipslid, clvn, pv_r1d(:                ), start = kstart(1:knbdim), count = kcount(1:knbdim) )
      ELSEIF( PRESENT(pv_r2d) ) THEN
         CALL fliogetv( ioipslid, clvn, pv_r2d(kx1:kx2,ky1:ky2  ), start = kstart(1:knbdim), count = kcount(1:knbdim) )
      ELSEIF( PRESENT(pv_r3d) ) THEN
         CALL fliogetv( ioipslid, clvn, pv_r3d(kx1:kx2,ky1:ky2,:), start = kstart(1:knbdim), count = kcount(1:knbdim) )
      ENDIF
      !
      !
   END SUBROUTINE iom_ioipsl_g123d


   SUBROUTINE iom_ioipsl_gettime( kiomid, kvid, ptime, cdunits, cdcalendar )
      !!--------------------------------------------------------------------
      !!                   ***  SUBROUTINE iom_gettime  ***
      !!
      !! ** Purpose : read the time axis kvid in the file with IOIPSL (only fliocom module)
      !!--------------------------------------------------------------------
      INTEGER                   , INTENT(in   ) ::   kiomid     ! file Identifier
      INTEGER                   , INTENT(in   ) ::   kvid       ! variable id
      REAL(wp), DIMENSION(:)    , INTENT(  out) ::   ptime      ! the time axis
      CHARACTER(len=*), OPTIONAL, INTENT(  out) ::   cdunits    ! units attribute
      CHARACTER(len=*), OPTIONAL, INTENT(  out) ::   cdcalendar ! calendar attribute
      !---------------------------------------------------------------------
      !
      CALL fliogetv( iom_file(kiomid)%nfid, TRIM(iom_file(kiomid)%cn_var(kvid)), ptime(:),   &
            &         start=(/ 1 /), count=(/ iom_file(kiomid)%dimsz(1, kvid) /) )
      IF ( PRESENT(cdunits) ) THEN 
         CALL fliogeta( iom_file(kiomid)%nfid, TRIM(iom_file(kiomid)%cn_var(kvid)), "units", cdunits )
      ENDIF
      IF ( PRESENT(cdcalendar) ) THEN 
         CALL fliogeta( iom_file(kiomid)%nfid, TRIM(iom_file(kiomid)%cn_var(kvid)), "calendar", cdcalendar )
      ENDIF
      !
   END SUBROUTINE iom_ioipsl_gettime


   SUBROUTINE iom_ioipsl_rp0123d( kt, kwrite, kiomid, cdvar , kvid  , ktype,   &
         &                                 pv_r0d, pv_r1d, pv_r2d, pv_r3d )
      !!--------------------------------------------------------------------
      !!                   ***  SUBROUTINE  iom_ioipsl_rstput  ***
      !!
      !! ** Purpose : read the time axis cdvar in the file 
      !!--------------------------------------------------------------------
      INTEGER                     , INTENT(in)           ::   kt       ! ocean time-step
      INTEGER                     , INTENT(in)           ::   kwrite   ! writing time-step
      INTEGER                     , INTENT(in)           ::   kiomid   ! Identifier of the file 
      CHARACTER(len=*)            , INTENT(in)           ::   cdvar    ! variable name
      INTEGER                     , INTENT(in)           ::   kvid     ! variable id
      INTEGER                     , INTENT(in), OPTIONAL ::   ktype    ! variable type (default R8)
      REAL(wp)                    , INTENT(in), OPTIONAL ::   pv_r0d   ! written Od field
      REAL(wp), DIMENSION(      :), INTENT(in), OPTIONAL ::   pv_r1d   ! written 1d field
      REAL(wp), DIMENSION(:, :   ), INTENT(in), OPTIONAL ::   pv_r2d   ! written 2d field
      REAL(wp), DIMENSION(:, :, :), INTENT(in), OPTIONAL ::   pv_r3d   ! written 3d field
      !
      INTEGER               :: idims                ! number of dimension
      INTEGER               :: idvar                ! variable id
      INTEGER               :: itype                ! variable type
      INTEGER               :: ix1, ix2, iy1, iy2   ! subdomain indexes
      INTEGER, DIMENSION(4) :: idimsz               ! dimensions size  
      INTEGER, DIMENSION(4) :: idimid               ! dimensions id
      CHARACTER(LEN=100)    :: clinfo               ! info character
      INTEGER               :: ioipslid             ! ioipsl file identifier
      !---------------------------------------------------------------------
      !
      clinfo = '          iom_ioipsl_rp0123d, file: '//TRIM(iom_file(kiomid)%name)//', var: '//TRIM(cdvar)
      ioipslid = iom_file(kiomid)%nfid
      !
      ! define dimension variables if it is not already done
      ! ==========================
      IF( iom_file(kiomid)%nvars == 0 ) THEN
         ! define the dimension variables if it is not already done
         CALL fliodefv( ioipslid,'nav_lon', (/1,2/), v_t=flio_r4   , axis='X',   &
               &                  long_name="Longitude", units="degrees_east" )
         CALL fliodefv( ioipslid,'nav_lat', (/1,2/), v_t=flio_r4   , axis='Y',   &
               &                  long_name="Latitude", units="degrees_north" )
         CALL fliodefv( ioipslid,'nav_lev', (/3/)  , v_t=flio_i4   , axis='Z',   &
               &                  long_name="Model levels",units="model_levels")
         CALL fliodefv( ioipslid,'time_counter', (/4/), v_t=flio_r4, axis='T',   &
               &                  long_name="Time axis", units='seconds since 0001-01-01 00:00:00' )
         ! update informations structure related the dimension variable we just added...
         iom_file(kiomid)%nvars       = 4
         iom_file(kiomid)%luld(1:4)   = (/ .FALSE., .FALSE., .FALSE., .TRUE. /)
         iom_file(kiomid)%cn_var(1:3) = (/ 'nav_lon', 'nav_lat', 'nav_lev' /)
         iom_file(kiomid)%cn_var(4)   = 'time_counter'
         iom_file(kiomid)%ndims(1:4)  = (/ 2, 2, 1, 1 /)
         ! trick: defined to 0 to say that dimension variables are defined but not yet written
         iom_file(kiomid)%dimsz(1, 1) = 0   
         IF(lwp) WRITE(numout,*) TRIM(clinfo)//' define dimension variables done'
      ENDIF

      ! define the data if it is not already done
      ! ===============
      IF( kvid <= 0 ) THEN
         ! variable definition
         IF(     PRESENT(pv_r0d) ) THEN   ;   idims = 0
         ELSEIF( PRESENT(pv_r1d) ) THEN   ;   idims = 2   ;   idimid(1:idims) = (/    3,4/)
         ELSEIF( PRESENT(pv_r2d) ) THEN   ;   idims = 3   ;   idimid(1:idims) = (/1,2  ,4/)
         ELSEIF( PRESENT(pv_r3d) ) THEN   ;   idims = 4   ;   idimid(1:idims) = (/1,2,3,4/)
         ENDIF
         IF( PRESENT(ktype) ) THEN   ! variable external type
            SELECT CASE (ktype)
            CASE (jp_r8)  ;   itype = flio_r8
            CASE (jp_r4)  ;   itype = flio_r4
            CASE (jp_i4)  ;   itype = flio_i4
            CASE (jp_i2)  ;   itype = flio_i2
            CASE (jp_i1)  ;   itype = flio_i1   !   fliocom does not handle i1 type of variable
            CASE DEFAULT   ;   CALL ctl_stop( TRIM(clinfo)//' unknown variable type' )
            END SELECT
         ELSE
            itype = flio_r8
         ENDIF
         IF( PRESENT(pv_r0d) ) THEN   ;   CALL fliodefv (ioipslid, TRIM(cdvar)                 , v_t = itype)
         ELSE                         ;   CALL fliodefv (ioipslid, TRIM(cdvar), idimid(1:idims), v_t = itype)
         ENDIF
         ! update informations structure related the new variable we want to add...
         idvar                          = iom_file(kiomid)%nvars + 1
         iom_file(kiomid)%nvars         = idvar
         iom_file(kiomid)%cn_var(idvar) = TRIM(cdvar)
         iom_file(kiomid)%nvid(idvar)   = -1   ! netcdf variable id is not available in ioipsl
         iom_file(kiomid)%scf(idvar)    = 1.
         iom_file(kiomid)%ofs(idvar)    = 0.
         iom_file(kiomid)%ndims(idvar)  = idims
         IF( .NOT. PRESENT(pv_r0d) ) THEN
            iom_file(kiomid)%luld(idvar) = .TRUE.
            CALL flioinqf( ioipslid, ln_dim = idimsz )
            iom_file(kiomid)%dimsz(1:idims-1,idvar) = idimsz(idimid(1:idims-1))
         ELSE                               
            iom_file(kiomid)%luld(idvar) = .FALSE.
         ENDIF
         IF(lwp) WRITE(numout,*) TRIM(clinfo)//' defined ok'
      ELSE
         idvar = kvid
      ENDIF

      ! time step kwrite : write the variable
      IF( kt == kwrite ) THEN
         ! on what kind of domain must the data be written?
         IF( PRESENT(pv_r2d) .OR. PRESENT(pv_r3d) ) THEN
            idimsz(1:2) = iom_file(kiomid)%dimsz(1:2,idvar)
            IF(     idimsz(1) == (nlei - nldi + 1) .AND. idimsz(2) == (nlej - nldj + 1) ) THEN
               ix1 = nldi   ;   ix2 = nlei   ;   iy1 = nldj   ;   iy2 = nlej
            ELSEIF( idimsz(1) == nlci              .AND. idimsz(2) == nlcj              ) THEN
               ix1 = 1      ;   ix2 = nlci   ;   iy1 = 1      ;   iy2 = nlcj
            ELSEIF( idimsz(1) == jpi               .AND. idimsz(2) == jpj               ) THEN
               ix1 = 1      ;   ix2 = jpi    ;   iy1 = 1      ;   iy2 = jpj
            ELSE 
               CALL ctl_stop( 'iom_ioipsl_rp0123d: should have been an impossible case...' )
            ENDIF

            ! write dimension variables if it is not already done
            ! =============
            ! trick: is defined to 0 => dimension variable are defined but not yet written
            IF( iom_file(kiomid)%dimsz(1, 1) == 0 ) THEN
               CALL flioputv( ioipslid, 'nav_lon'     , glamt(ix1:ix2, iy1:iy2) )
               CALL flioputv( ioipslid, 'nav_lat'     , gphit(ix1:ix2, iy1:iy2) )
               CALL flioputv( ioipslid, 'nav_lev'     , gdept_0 )
               ! +++ WRONG VALUE: to be improved but not really useful...
               CALL flioputv( ioipslid, 'time_counter', kt )
               ! update the values of the variables dimensions size
               CALL flioinqf( ioipslid, ln_dim = idimsz )
               iom_file(kiomid)%dimsz(1:2, 1) = idimsz(1:2)
               iom_file(kiomid)%dimsz(1:2, 2) = idimsz(1:2)
               iom_file(kiomid)%dimsz(1, 3:4) = (/idimsz(3), 1/)
               IF(lwp) WRITE(numout,*) TRIM(clinfo)//' write dimension variables done'
            ENDIF
         ENDIF

         ! write the data
         ! =============
         IF(     PRESENT(pv_r0d) ) THEN   ;   CALL flioputv( ioipslid, cdvar, pv_r0d                      )
         ELSEIF( PRESENT(pv_r1d) ) THEN   ;   CALL flioputv( ioipslid, cdvar, pv_r1d(                  :) )
         ELSEIF( PRESENT(pv_r2d) ) THEN   ;   CALL flioputv( ioipslid, cdvar, pv_r2d(ix1:ix2, iy1:iy2   ) )
         ELSEIF( PRESENT(pv_r3d) ) THEN   ;   CALL flioputv( ioipslid, cdvar, pv_r3d(ix1:ix2, iy1:iy2, :) )
         ENDIF
         ! add 1 to the size of the temporal dimension (not really useful...)
         IF( iom_file(kiomid)%luld(idvar) )   iom_file(kiomid)%dimsz(iom_file(kiomid)%ndims(idvar), idvar)    &
               &                            = iom_file(kiomid)%dimsz(iom_file(kiomid)%ndims(idvar), idvar) + 1
         IF(lwp) WRITE(numout,*) TRIM(clinfo)//' written ok'
      ENDIF
      !     
   END SUBROUTINE iom_ioipsl_rp0123d


   !!======================================================================
END MODULE iom_ioipsl
