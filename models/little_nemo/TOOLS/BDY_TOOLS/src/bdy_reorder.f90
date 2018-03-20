PROGRAM bdy_reorder

!===============================================================================
! A routine to reorder old BDY data files to make them compatible with NEMO 3.4.
!
! This routine has 2 modes:
!   1. If no template file is given then it will re-order the data in the input
!      file so that it is in order of increasing nbr. 
!   2. If a template file is given it will re-order the data in the input file 
!      be consistent with the order of the data in the template file. This is 
!      useful for making old barotropic and baroclinic data files consistent. 
!      For older versions of NEMO the barotropic boundary data did not have to
!      be in the same order as the baroclinic boundary data. The rimwidth 
!      value can be different in the input file and template file; in this case 
!      the routine will just re-order the data that the two files have in common. 
!
! The routine is mainly for re-ordering BDY data files, but can also be used to
! re-order BDY coordinate files if ln_coordinates is set to .true.
!
! Author: Dave Storkey  Aug 2011
! Bug notifications etc to:  dave.storkey@metoffice.gov.uk
!
!===============================================================================
  
      USE netcdf

      IMPLICIT NONE

      INTEGER,PARAMETER         :: numnam=11
      INTEGER,PARAMETER         :: sp=SELECTED_REAL_KIND(6,37)
      INTEGER,PARAMETER         :: dp=SELECTED_REAL_KIND(12,307)
      INTEGER                   :: chunksize = 32000000

      INTEGER                   :: jpbgrd, iostat, ncid_in, ncid_out, ncid_template, nbrid(4), nbrid_template(4)
      INTEGER                   :: dimids(10), unlimitedDimId
      INTEGER                   :: xtype, dimid, varid, ndims, ndims_var, nvars, natts, nblen(4), nblen_template(4), dimlen(10)
      INTEGER                   :: lenvar(10), igrid, ib, ib1, ir, idim, jgrid, jv, icount, attid, len1, idepth, itime
      INTEGER                   :: idim_time, idim_xb, idim_yb, idim_depth
      INTEGER                   :: nbr_min, nbr_max, nbr_min_template, nbr_max_template, strlen, nbr_match

      INTEGER, ALLOCATABLE, DIMENSION(:,:)   :: nbr, imap
      INTEGER, ALLOCATABLE, DIMENSION(:,:)   :: nbi, nbj
      INTEGER, ALLOCATABLE, DIMENSION(:,:)   :: nbi_template
      INTEGER, ALLOCATABLE, DIMENSION(:,:)   :: nbj_template
      INTEGER, ALLOCATABLE, DIMENSION(:,:)   :: nbr_template

      INTEGER, ALLOCATABLE, DIMENSION(:)     :: nbi_extract, nbj_extract

      INTEGER,  ALLOCATABLE, DIMENSION(:,:,:,:)  :: varin_int, varout_int
      REAL(sp), ALLOCATABLE, DIMENSION(:,:,:,:)  :: varin_float, varout_float
      REAL(dp), ALLOCATABLE, DIMENSION(:,:,:,:)  :: varin_dble, varout_dble

      LOGICAL                   :: ln_coordinates
      LOGICAL                   :: ln_template

      CHARACTER(LEN=nf90_max_name)        :: attname, dimname, varname, time, date, zone, timestamp
      CHARACTER(LEN=nf90_max_name)        :: file_in, file_out, file_template, nbi_name, nbj_name, nbr_name
      CHARACTER(LEN=1),DIMENSION(4)       :: cgrid
      CHARACTER(LEN=1)                    :: end_letter

      NAMELIST/nam_bdy_reorder/ file_in, file_out, file_template, ln_coordinates, nbr_match

!=============================================================================

!-----------------------------------------------------------------------------
! 0. Read namelist and initialise parameters
!----------------------------------------------------------------------------- 

      file_in = ''
      file_out = ''
      file_template = ''
      ln_coordinates = .false.
      nbr_match = 0

      OPEN( UNIT=numnam, FILE='nam_bdy_reorder', FORM='FORMATTED', STATUS='OLD' )
      READ( numnam, nam_bdy_reorder )
      CLOSE( UNIT=numnam )

      IF( ln_coordinates ) THEN
         WRITE(*,*) 'Input file is a coordinates.bdy.nc file.'
         jpbgrd = 4
      ELSE
          WRITE(*,*) 'Input file is a boundary data file.'
         jpbgrd = 1
      ENDIF
 
      IF( LEN_TRIM(file_template) > 0 ) THEN 
         WRITE(*,*) 'Reordering data to match template file.'
         ln_template = .true.
      ELSE 
         WRITE(*,*) 'Reordering data in order of increasing nbr values.'
         ln_template = .false.
      ENDIF

      cgrid= (/'t','u','v','f'/)

!-----------------------------------------------------------------------------
! 1. Read in nbr variables from file and create mapping. 
!----------------------------------------------------------------------------- 

! 1.1 Open input files

      iostat = nf90_open( TRIM(file_in), nf90_nowrite, ncid_in )
      IF (iostat /= nf90_noerr) THEN
        WRITE(6,*) TRIM(nf90_strerror(iostat))
        STOP
      ENDIF
      iostat = nf90_inquire( ncid_in, ndims, nvars, natts )

      IF( ln_template ) THEN
         iostat = nf90_open( TRIM(file_template), nf90_nowrite, ncid_template )
         IF (iostat /= nf90_noerr) THEN
           WRITE(6,*) TRIM(nf90_strerror(iostat))
           STOP
         ENDIF
      ENDIF

      DO igrid=1,jpbgrd

! 1.2 Find dimensions of data in input files and allocate arrays

         IF( ln_coordinates ) THEN
            nbr_name = 'nbr'//cgrid(igrid)
         ELSE
            nbr_name = 'nbrdta'
         ENDIF

         iostat = nf90_inq_varid( ncid_in, nbr_name , nbrid(igrid) )
         IF (iostat /= nf90_noerr) THEN
            WRITE(6,*) TRIM(nf90_strerror(iostat))
            STOP
         ENDIF
         iostat = nf90_inquire_variable( ncid_in, nbrid(igrid), ndims=ndims, dimids=dimids )
         IF( ndims .ne. 2 ) THEN
            WRITE(*,*) 'ERROR : ',TRIM(nbr_name),' does not have exactly 2 dimensions.'
            WRITE(*,*) '        in file ',TRIM(file_in)
            STOP
         ENDIF
         iostat = nf90_inquire_dimension( ncid_in, dimids(1), len=nblen(igrid))
         iostat = nf90_inquire_dimension( ncid_in, dimids(2), len=len1)
         WRITE(*,*) 'nblen(igrid), len1 : ',nblen(igrid), len1
         IF( len1 .ne. 1 ) THEN 
            WRITE(*,*) 'ERROR : second dimension of ',TRIM(nbr_name),' does not have length 1.'
            WRITE(*,*) '        in file ',TRIM(file_in)
            WRITE(*,*) '        len1 = ',len1
            STOP
         ENDIF

         IF( ln_template ) THEN 

            iostat = nf90_inq_varid( ncid_template, nbr_name , nbrid_template(igrid) )
            IF (iostat /= nf90_noerr) THEN
               WRITE(6,*) TRIM(nf90_strerror(iostat))
               STOP
            ENDIF
            iostat = nf90_inquire_variable( ncid_template, nbrid_template(igrid), ndims=ndims, dimids=dimids )
            IF( ndims .ne. 2 ) THEN
               WRITE(*,*) 'ERROR : ',TRIM(nbr_name),' does not have exactly 2 dimensions.'
               WRITE(*,*) '        in file ',TRIM(file_template)
               STOP
            ENDIF
            iostat = nf90_inquire_dimension( ncid_template, dimids(1), len=nblen_template(igrid))
            iostat = nf90_inquire_dimension( ncid_template, dimids(2), len=len1)
            WRITE(*,*) 'nblen_template(igrid), len1 : ',nblen_template(igrid), len1
            IF( len1 .ne. 1 ) THEN 
               WRITE(*,*) 'ERROR : second dimension of ',TRIM(nbr_name),' does not have length 1.'
               WRITE(*,*) '        in file ',TRIM(file_template)
               WRITE(*,*) '        len1 = ',len1
               STOP
            ENDIF

         ENDIF ! ln_template

      ENDDO  ! jpbgrd

      ALLOCATE( nbr(MAXVAL(nblen),1), imap(MAXVAL(nblen),jpbgrd) )
      IF( ln_template ) THEN 
         ALLOCATE( nbi(MAXVAL(nblen),1), nbj(MAXVAL(nblen),1) )
         ALLOCATE( nbi_template(MAXVAL(nblen_template),1) )
         ALLOCATE( nbj_template(MAXVAL(nblen_template),1) )
         ALLOCATE( nbr_template(MAXVAL(nblen_template),1) )
      ENDIF

! 1.3 Read in nbr variables and generate mapping.

      nbr(:,:) = -1
      imap(:,:) = -1    
      IF( ln_template ) THEN 
         nbi(:,:) = -1
         nbj(:,:) = -1
         nbi_template(:,:) = -1
         nbj_template(:,:) = -1
         nbr_template(:,:) = -1
      ENDIF

      WRITE(*,*) '>>> Generating map.'
      DO igrid=1,jpbgrd

         iostat = nf90_get_var( ncid_in, nbrid(igrid), nbr(1:nblen(igrid),1:1) ) 
         IF (iostat /= nf90_noerr) THEN
           WRITE(6,*) TRIM(nf90_strerror(iostat))
           STOP
         ENDIF

         nbr_min = MINVAL( nbr(1:nblen(igrid),1) )
         nbr_max = MAXVAL( nbr(1:nblen(igrid),1) )
         IF( nbr_min .ne. 1 ) THEN
            WRITE(*,*) 'Something wrong with input file ',file_in
            WRITE(*,*) 'MIN(nbr) /= 1'
            STOP
         ENDIF

         IF( ln_template ) THEN 

            IF( ln_coordinates ) THEN
               nbi_name = 'nbi'//cgrid(igrid)
               nbj_name = 'nbj'//cgrid(igrid)
            ELSE
               nbi_name = 'nbidta'            
               nbj_name = 'nbjdta'            
            ENDIF

            ! read in nbi, nbj values from input file
            iostat = nf90_inq_varid( ncid_in, nbi_name, varid ) 
            IF (iostat == nf90_noerr) iostat = nf90_get_var( ncid_in, varid, nbi(1:nblen(igrid),1:1) ) 
            IF (iostat /= nf90_noerr) THEN
              WRITE(6,*) TRIM(nf90_strerror(iostat))
              STOP
            ENDIF
            iostat = nf90_inq_varid( ncid_in, nbj_name, varid ) 
            IF (iostat == nf90_noerr) iostat = nf90_get_var( ncid_in, varid, nbj(1:nblen(igrid),1:1) ) 
            IF (iostat /= nf90_noerr) THEN
              WRITE(6,*) TRIM(nf90_strerror(iostat))
              STOP
            ENDIF

            ! read in nbi, nbj, nbr values from template file
            iostat = nf90_inq_varid( ncid_template, nbi_name, varid ) 
            IF (iostat == nf90_noerr) iostat = nf90_get_var( ncid_template, varid, nbi_template(1:nblen_template(igrid),1:1) ) 
            IF (iostat /= nf90_noerr) THEN
              WRITE(6,*) TRIM(nf90_strerror(iostat))
              STOP
            ENDIF
            iostat = nf90_inq_varid( ncid_template, nbj_name, varid ) 
            IF (iostat == nf90_noerr) iostat = nf90_get_var( ncid_template, varid, nbj_template(1:nblen_template(igrid),1:1) ) 
            IF (iostat /= nf90_noerr) THEN
              WRITE(6,*) TRIM(nf90_strerror(iostat))
              STOP
            ENDIF
            iostat = nf90_get_var( ncid_template, nbrid_template(igrid), nbr_template(1:nblen_template(igrid),1:1) ) 
            IF (iostat /= nf90_noerr) THEN
              WRITE(6,*) TRIM(nf90_strerror(iostat))
              STOP
            ENDIF

            nbr_min_template = MINVAL( nbr_template(1:nblen_template(igrid),1) )
            nbr_max_template = MAXVAL( nbr_template(1:nblen_template(igrid),1) )
            IF( nbr_min_template .ne. 1 ) THEN
               WRITE(*,*) 'Something wrong with template file ',file_template
               WRITE(*,*) 'MIN(nbr) /= 1'
               STOP
            ENDIF

            IF( nbr_match .lt. 1 ) nbr_match = MIN(nbr_max, nbr_max_template)

            ! initialise imap to the identity mapping
            DO ib = 1, nblen(igrid)
               imap(ib, igrid) = ib
            ENDDO

            ! allocate "extract" arrays
            icount = 0
            DO ib = 1, nblen_template(igrid)
               IF( nbr_template(ib,1) .eq. 1 ) THEN
                  icount = icount+1
               ENDIF
            ENDDO
            ALLOCATE( nbi_extract(icount), nbj_extract(icount) )

            DO ir = 1, nbr_match

               ! extract values from template array for this ir value
               icount = 0
               DO ib = 1, nblen_template(igrid)
                  IF( nbr_template(ib,1) .eq. ir ) THEN
                     icount = icount+1
                     nbi_extract(icount) = nbi_template(ib,1)               
                     nbj_extract(icount) = nbj_template(ib,1)               
                  ENDIF
               ENDDO

               ! work out the mapping for this ir value
               icount = 1
               DO ib = 1, nblen(igrid)
                  IF( nbr(ib,1) .eq. ir ) THEN                  
                     DO ib1 = 1, nblen(igrid)
                        IF( nbi(ib1,1) .eq. nbi_extract(icount) .and. &
                       &    nbj(ib1,1) .eq. nbj_extract(icount)       ) THEN
                           imap(ib,igrid) = ib1
                           icount = icount + 1
                           EXIT
                        ENDIF                        
                     ENDDO ! ib1
                  ENDIF           
               ENDDO ! ib

            ENDDO ! ir
            
            DEALLOCATE( nbi_extract, nbj_extract )

         ELSE 

            icount = 0
            DO ir = nbr_min, nbr_max
               DO ib = 1, nblen(igrid)
                  IF( nbr(ib,1) .eq. ir ) THEN
                     icount = icount + 1
                     imap(icount,igrid) = ib
                  ENDIF           
               ENDDO
            ENDDO

         ENDIF
     
      ENDDO  ! jpbgrd

!-----------------------------------------------------------------------------
! 2. Open output file and copy dimensions and attributes across
!----------------------------------------------------------------------------- 

      iostat = nf90_inquire( ncid_in, ndims, nvars, natts )

! 2.1 Create the output file  

      WRITE(*,*) '>>> Initialising output file.'
      iostat = nf90_create( TRIM(file_out), nf90_64bit_offset, ncid_out, chunksize=chunksize)

! 2.2 Copy the dimensions into the output file.

      iostat = nf90_inquire( ncid_in, unlimitedDimId = unlimitedDimId )
      dimlen(:) = 1
      DO idim = 1, ndims
         iostat = nf90_inquire_dimension(ncid_in, idim, dimname, dimlen(idim))
         IF (idim == unlimitedDimId) THEN
            iostat = nf90_def_dim( ncid_out, dimname, nf90_unlimited, dimid)    
            idim_time = idim
         ELSE
            iostat = nf90_def_dim( ncid_out, dimname, dimlen(idim), dimid)
            IF( INDEX(dimname,'x') .gt. 0 ) THEN
               idim_xb = idim
            ELSE IF( INDEX(dimname,'y') .gt. 0 ) THEN
               idim_yb = idim
            ELSE IF( INDEX(dimname,'depth') .gt. 0 .or. INDEX(dimname,'z') .gt. 0 ) THEN
               idim_depth = idim
            ELSE
               WRITE(*,*) 'ERROR: Unrecognised dimension : ',dimname
               STOP
            ENDIF            
         ENDIF
      END DO
      

! 2.2 Copy the global attributes into the output file.
!     Also need to change the file_name attribute and the TimeStamp attribute.

      DO attid = 1, natts
         iostat = nf90_inq_attname( ncid_in, nf90_global, attid, attname )
         WRITE(6,*)'>>> Copying attribute '//TRIM(attname)//' into destination file...'
         iostat = nf90_copy_att( ncid_in, nf90_global, attname, ncid_out, nf90_global )  
      END DO
      iostat = nf90_put_att( ncid_out, nf90_global, "file_name", TRIM(file_out))
      CALL DATE_AND_TIME ( date=date, time=time, zone=zone )
      timestamp = date(7:8) // "/" // date(5:6) // "/" // date(1:4) // " " // &
                  time(1:2) // ":" // time(3:4) // ":" // time(5:6) // " " // &
                  zone  
      iostat = nf90_put_att( ncid_out, nf90_global, "TimeStamp", timestamp)
  
! 2.3 Copy the variable definitions and attributes into the output file.

      DO jv = 1, nvars
         iostat = nf90_inquire_variable( ncid_in, jv, varname, xtype, ndims_var, dimids, natts )
         iostat = nf90_def_var( ncid_out, varname, xtype, dimids(1:ndims_var), varid )
            IF (natts > 0) THEN
            DO attid = 1, natts
               iostat = nf90_inq_attname(ncid_in, varid, attid, attname)
               iostat = nf90_copy_att( ncid_in, varid, attname, ncid_out, varid )   
            END DO
         ENDIF
      END DO
 
! 2.4 End definitions in output file

      iostat = nf90_enddef( ncid_out )    

!-----------------------------------------------------------------------------
! 3. Read in variables from input file, re-order and write to output file
!----------------------------------------------------------------------------- 

     IF( ln_coordinates ) THEN
        ALLOCATE( varin_int(MAXVAL(nblen),1,1,1), varout_int(MAXVAL(nblen),1,1,1) )
        ALLOCATE( varin_float(MAXVAL(nblen),1,1,1), varout_float(MAXVAL(nblen),1,1,1) )
        ALLOCATE( varin_dble(MAXVAL(nblen),1,1,1), varout_dble(MAXVAL(nblen),1,1,1) )
     ELSE
        SELECT CASE( ndims ) 
           CASE( 2 )
              ALLOCATE(  varin_int(dimlen(idim_xb),dimlen(idim_yb),1,1) )
              ALLOCATE( varout_int(dimlen(idim_xb),dimlen(idim_yb),1,1) )
              ALLOCATE(  varin_float(dimlen(idim_xb),dimlen(idim_yb),1,1) ) 
              ALLOCATE( varout_float(dimlen(idim_xb),dimlen(idim_yb),1,1) )
              ALLOCATE(  varin_dble(dimlen(idim_xb),dimlen(idim_yb),1,1) ) 
              ALLOCATE( varout_dble(dimlen(idim_xb),dimlen(idim_yb),1,1) )
           CASE ( 3 )
              ALLOCATE(  varin_int(dimlen(idim_xb),dimlen(idim_yb),1,dimlen(idim_time)) )
              ALLOCATE( varout_int(dimlen(idim_xb),dimlen(idim_yb),1,dimlen(idim_time)) )
              ALLOCATE(  varin_float(dimlen(idim_xb),dimlen(idim_yb),1,dimlen(idim_time)) ) 
              ALLOCATE( varout_float(dimlen(idim_xb),dimlen(idim_yb),1,dimlen(idim_time)) )
              ALLOCATE(  varin_dble(dimlen(idim_xb),dimlen(idim_yb),1,dimlen(idim_time)) ) 
              ALLOCATE( varout_dble(dimlen(idim_xb),dimlen(idim_yb),1,dimlen(idim_time)) )
           CASE ( 4 )
              ALLOCATE(  varin_int(dimlen(idim_xb),dimlen(idim_yb),dimlen(idim_depth),dimlen(idim_time)) ) 
              ALLOCATE( varout_int(dimlen(idim_xb),dimlen(idim_yb),dimlen(idim_depth),dimlen(idim_time)) )
              ALLOCATE(  varin_float(dimlen(idim_xb),dimlen(idim_yb),dimlen(idim_depth),dimlen(idim_time)) ) 
              ALLOCATE( varout_float(dimlen(idim_xb),dimlen(idim_yb),dimlen(idim_depth),dimlen(idim_time)) )
              ALLOCATE(  varin_dble(dimlen(idim_xb),dimlen(idim_yb),dimlen(idim_depth),dimlen(idim_time)) ) 
              ALLOCATE( varout_dble(dimlen(idim_xb),dimlen(idim_yb),dimlen(idim_depth),dimlen(idim_time)) )
           CASE DEFAULT
              WRITE(*,*) 'ERROR : Can only cope with 2, 3 or 4 dimensions in the boundary data files.'
              WRITE(*,*) '        This file appears to have ',ndims,' dimensions.'
              STOP
        END SELECT
     ENDIF

     DO jv = 1, nvars
       iostat = nf90_inquire_variable( ncid_in, jv, varname, xtype, ndims_var, dimids )
       DO idim = 1, ndims_var
          lenvar(idim) = dimlen(dimids(idim))
       ENDDO

       IF( ndims_var .eq. 1 ) THEN
          WRITE(*,*) '>>> Copying coordinate variable ',TRIM(varname)
       ELSE
          WRITE(*,*) '>>> Reordering variable ',TRIM(varname)
       ENDIF
       ! Error check here

       IF( ln_coordinates ) THEN
          strlen = len_trim(varname)
          end_letter = varname(strlen:strlen)
          jgrid = -1
          DO igrid = 1,4
             IF( end_letter .eq. cgrid(igrid) ) jgrid = igrid
          END DO
          IF( jgrid .lt. 0 ) THEN
             WRITE(*,*) 'ERROR : Could not identify grid for variable ',TRIM(varname)
             WRITE(*,*) ' varname : ',TRIM(varname),'!'
             WRITE(*,*) ' strlen : ',strlen
             WRITE(*,*) ' end_letter : ',end_letter
             WRITE(*,*) ' cgrid(1) : ',cgrid(1)
             WRITE(*,*) ' cgrid(2) : ',cgrid(2)
             WRITE(*,*) ' cgrid(3) : ',cgrid(3)
             WRITE(*,*) ' cgrid(4) : ',cgrid(4)
             STOP
          ENDIF
       ELSE
          jgrid=1
       ENDIF
           
       SELECT CASE(xtype)

          CASE( NF90_INT ) 
             SELECT CASE(ndims_var)
                CASE( 1 )
                   WRITE(*,*) 'This is a 1D integer.'
                   ! Assume this is a depth or time coordinate variable and copy across unchanged.
                   iostat = nf90_get_var( ncid_in, jv, varin_int(1:lenvar(1),1,1,1) )
                   iostat = nf90_put_var( ncid_out, jv, varout_int(1:lenvar(1),1,1,1) ) 
                CASE( 2 )
                   WRITE(*,*) 'This is a 2D integer.'
                   iostat = nf90_get_var( ncid_in, jv, varin_int(1:lenvar(1),1:1,1,1) )
                   DO ib = 1,lenvar(1)
                      varout_int(ib,1,1,1) = varin_int(imap(ib,jgrid),1,1,1)
                   END DO
                   iostat = nf90_put_var( ncid_out, jv, varout_int(1:lenvar(1),1:1,1,1) ) 
                CASE( 3 )
                   WRITE(*,*) 'This is a 3D integer.'
                   ! Assume third dimension is time.
                   iostat = nf90_get_var( ncid_in, jv, varin_int(1:lenvar(1),1:1,1,1:lenvar(3)) )
                   DO itime = 1,lenvar(3)
                      DO ib = 1,lenvar(1)
                         varout_int(ib,1,1,itime) = varin_int(imap(ib,jgrid),1,1,itime)
                      END DO
                   END DO
                   iostat = nf90_put_var( ncid_out, jv, varout_int(1:lenvar(1),1:1,1,1:lenvar(3)) ) 
                CASE( 4 )
                   WRITE(*,*) 'This is a 4D integer.'
                   ! Assume third and fourth dimensions are depth and time respectively.
                   iostat = nf90_get_var( ncid_in, jv, varin_int(1:lenvar(1),1:1,1:lenvar(3),1:lenvar(4)) )
                   DO itime = 1,lenvar(4)
                      DO idepth = 1,lenvar(3)
                         DO ib = 1,lenvar(1)
                            varout_int(ib,1,idepth,itime) = varin_int(imap(ib,jgrid),1,idepth,itime)
                         END DO
                      END DO
                   END DO
                   iostat = nf90_put_var( ncid_out, jv, varout_int(1:lenvar(1),1:1,1:lenvar(3),1:lenvar(4)) ) 
                CASE DEFAULT
                   WRITE(*,*) 'ERROR : Variable with ',ndims_var,' dimensions. Case not coded.'
                   STOP
             END SELECT

          CASE( NF90_FLOAT ) 
             SELECT CASE(ndims_var)
                CASE( 1 )
                   WRITE(*,*) 'This is a 1D float.'
                   ! Assume this is a depth or time coordinate variable and copy across unchanged.
                   iostat = nf90_get_var( ncid_in, jv, varin_float(1:lenvar(1),1,1,1) )
                   iostat = nf90_put_var( ncid_out, jv, varout_float(1:lenvar(1),1,1,1) ) 
                CASE( 2 )
                   WRITE(*,*) 'This is a 2D float.'
                   iostat = nf90_get_var( ncid_in, jv, varin_float(1:lenvar(1),1:1,1,1) )
                   DO ib = 1,lenvar(1)
                      varout_float(ib,1,1,1) = varin_float(imap(ib,jgrid),1,1,1)
                   END DO
                   iostat = nf90_put_var( ncid_out, jv, varout_float(1:lenvar(1),1:1,1,1) ) 
                CASE( 3 )
                   WRITE(*,*) 'This is a 3D float.'
                   ! Assume third dimension is time.
                   iostat = nf90_get_var( ncid_in, jv, varin_float(1:lenvar(1),1:1,1,1:lenvar(3)) )
                   DO itime = 1,lenvar(3)
                      DO ib = 1,lenvar(1)
                         varout_float(ib,1,1,itime) = varin_float(imap(ib,jgrid),1,1,itime)
                      END DO
                   END DO
                   iostat = nf90_put_var( ncid_out, jv, varout_float(1:lenvar(1),1:1,1,1:lenvar(3)) ) 
                CASE( 4 )
                   WRITE(*,*) 'This is a 4D float.'
                   ! Assume third and fourth dimensions are depth and time respectively.
                   iostat = nf90_get_var( ncid_in, jv, varin_float(1:lenvar(1),1:1,1:lenvar(3),1:lenvar(4)) )
                   DO itime = 1,lenvar(4)
                      DO idepth = 1,lenvar(3)
                         DO ib = 1,lenvar(1)
                            varout_float(ib,1,idepth,itime) = varin_float(imap(ib,jgrid),1,idepth,itime)
                         END DO
                      END DO
                   END DO
                   iostat = nf90_put_var( ncid_out, jv, varout_float(1:lenvar(1),1:1,1:lenvar(3),1:lenvar(4)) ) 
                CASE DEFAULT
                   WRITE(*,*) 'ERROR : Variable with ',ndims_var,' dimensions. Case not coded.'
                   STOP
             END SELECT

          CASE( NF90_DOUBLE ) 
             SELECT CASE(ndims_var)
                CASE( 1 )
                   WRITE(*,*) 'This is a 1D double.'
                   ! Assume this is a depth or time coordinate variable and copy across unchanged.
                   iostat = nf90_get_var( ncid_in, jv, varin_dble(1:lenvar(1),1,1,1) )
                   iostat = nf90_put_var( ncid_out, jv, varout_dble(1:lenvar(1),1,1,1) ) 
                CASE( 2 )
                   WRITE(*,*) 'This is a 2D double.'
                   iostat = nf90_get_var( ncid_in, jv, varin_dble(1:lenvar(1),1:1,1,1) )
                   DO ib = 1,lenvar(1)
                      varout_dble(ib,1,1,1) = varin_dble(imap(ib,jgrid),1,1,1)
                   END DO
                   iostat = nf90_put_var( ncid_out, jv, varout_dble(1:lenvar(1),1:1,1,1) ) 
                CASE( 3 )
                   WRITE(*,*) 'This is a 3D double.'
                   ! Assume third dimension is time.
                   iostat = nf90_get_var( ncid_in, jv, varin_dble(1:lenvar(1),1:1,1,1:lenvar(3)) )
                   DO itime = 1,lenvar(3)
                      DO ib = 1,lenvar(1)
                         varout_dble(ib,1,1,itime) = varin_dble(imap(ib,jgrid),1,1,itime)
                      END DO
                   END DO
                   iostat = nf90_put_var( ncid_out, jv, varout_dble(1:lenvar(1),1:1,1,1:lenvar(3)) ) 
                CASE( 4 )
                   WRITE(*,*) 'This is a 4D double.'
                   ! Assume third and fourth dimensions are depth and time respectively.
                   iostat = nf90_get_var( ncid_in, jv, varin_dble(1:lenvar(1),1:1,1:lenvar(3),1:lenvar(4)) )
                   DO itime = 1,lenvar(4)
                      DO idepth = 1,lenvar(3)
                         DO ib = 1,lenvar(1)
                            varout_dble(ib,1,idepth,itime) = varin_dble(imap(ib,jgrid),1,idepth,itime)
                         END DO
                      END DO
                   END DO
                   iostat = nf90_put_var( ncid_out, jv, varout_dble(1:lenvar(1),1:1,1:lenvar(3),1:lenvar(4)) ) 
                CASE DEFAULT
                   WRITE(*,*) 'ERROR : Variable with ',ndims_var,' dimensions. Case not coded.'
                   STOP
             END SELECT

             CASE DEFAULT
                WRITE(*,*) 'ERROR : Unrecognised data type.'           
                STOP

       END SELECT

       END DO ! jv

!-----------------------------------------------------------------------------
! 4. Close input and output files
!----------------------------------------------------------------------------- 

      iostat = nf90_close( ncid_out )
      iostat = nf90_close( ncid_in )


END PROGRAM bdy_reorder
