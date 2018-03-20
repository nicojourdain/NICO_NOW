#define __MYFILE__ 'ioncdf.F90'
MODULE ioncdf
   !!======================================================================
   !!                       ***  MODULE  ioncdf  ***
   !! Input/Output: manage netcdf inputs/outputs
   !!=====================================================================

   !!----------------------------------------------------------------------
   !!   ioncdf     : manage netcdf inputs/outputs
   !!       put_map      : write a 2d/3d map 
   !!       put_scalar   : write a scalar
   !!       test_var     : test if variable exists
   !!       get_nb_dim   : get dimension number of a variable
   !!       get_dim_val  : get dimensions values of a variable
   !!       get_var      : read a 1d/2d/3d/4d array 
   !!       put_coord    : write coordinates
   !!       create_map   : create map netcdf file
   !!       put_att      : write CF attributes
   !!       get_var_info : get CF attributes
   !!  Original    : Nicolas Daget
   !!  Modified      : MAB (nf90)
   !!---------------------------------------------------------------------
   !! * Modules used
   USE toolspar_kind
   USE netcdf
   USE nctools

   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC :: put_coord, put_map, get_var, test_var, put_scalar, get_nb_dim, &
             get_dim_val, get_scalar

   !! * Substitutions

   !! * Interface
   INTERFACE put_coord
     MODULE PROCEDURE put_coord_r, put_coord_d
   END INTERFACE

   INTERFACE put_map
     MODULE PROCEDURE put_map_2d_r, put_map_3d_r, put_map_2d_d, put_map_3d_d
   END INTERFACE

   INTERFACE put_scalar
     MODULE PROCEDURE put_scalar_i,  put_scalar_r,  put_scalar_d
   END INTERFACE

   INTERFACE get_var
     MODULE PROCEDURE get_var_1d_r, get_var_2d_r, get_var_3d_r, get_var_4d_r, &
                      get_var_1d_d, get_var_2d_d, get_var_3d_d, get_var_4d_d, &
                      get_var_1d_i, get_var_2d_i, get_var_3d_i, get_var_4d_i
   END INTERFACE

   INTERFACE get_scalar
     MODULE PROCEDURE get_scalar_i,  get_scalar_r,  get_scalar_d
   END INTERFACE

CONTAINS

   SUBROUTINE test_var ( cd_filename, cd_var, k_test )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE test_var  ***
      !!
      !! ** Purpose :  test if cd_var exists
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used


      !! * arguments
      CHARACTER(len=80), INTENT(in)               :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)     :: &
         cd_var                                        ! variable name
      INTEGER, INTENT(out)                        :: &
         k_test                                        ! test if variable exists

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id                           ! file and variable identifier
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ),&
         & __LINE__,__MYFILE__)
      k_test = nf90_inq_varid( i_file_id, cd_var, i_var_id )
      CALL nchdlerr( nf90_close( i_file_id ),&
         & __LINE__,__MYFILE__)

      IF ( k_test .EQ. nf90_noerr  ) THEN
         k_test = 1
      ELSE
         k_test = 0
      ENDIF

   END SUBROUTINE test_var
   SUBROUTINE get_nb_dim ( cd_filename, cd_var, k_nb_dim )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_nb_dim  ***
      !!
      !! ** Purpose :  Get dimension number of variable
      !!
      !! ** Method  : use netcdf fortran library
      !!
      !! ** Action : - get dimension number of variables
      !!
      !! Reference :
      !!
      !! History :
      !!    06-06  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used


      !! * arguments
      CHARACTER(len=80), INTENT(in)               :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)     :: &
         cd_var                                        ! variable name
      INTEGER, INTENT(out)                        :: &
         k_nb_dim                                      ! c$dimension number of variable

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id                           ! file and variable identifier
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr(nf90_inquire_variable( i_file_id, i_var_id, ndims=k_nb_dim ),&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_close( i_file_id ),&
         & __LINE__,__MYFILE__) 
   END SUBROUTINE get_nb_dim

   SUBROUTINE get_dim_val ( cd_filename, cd_var, k_dim )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_dim_val  ***
      !!
      !! ** Purpose :  Get dimensions values of a variable
      !!
      !! ** Method  : use netcdf fortran library
      !!
      !! ** Action : - get dimensions values of a variable
      !!
      !! Reference :
      !!
      !! History :
      !!    06-06  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used


      !! * arguments
      CHARACTER(len=80), INTENT(in)               :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)     :: &
         cd_var                                        ! variable name
      INTEGER, DIMENSION(:), INTENT(out)          :: &
         k_dim                                         ! dimensions values of a variable

      !! * local declarations
      INTEGER, DIMENSION(1) :: i_nbdim                 ! dimensions of the variable
      INTEGER               ::                       &
         i_file_id, i_var_id,                        & ! file and variable identifier
         ii                                     ! counter
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr(nf90_inquire_variable( i_file_id, i_var_id, dimids=k_dim ) ,&
         & __LINE__,__MYFILE__)
      i_nbdim = SHAPE( k_dim )
      DO ii =  1, i_nbdim(1)
         CALL nchdlerr( nf90_inquire_dimension( i_file_id, k_dim(ii), len=k_dim(ii) ),__LINE__,__MYFILE__ )
      ENDDO
      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

   END SUBROUTINE get_dim_val

   !!======================================================================

   SUBROUTINE get_scalar_i ( cd_filename, cd_var, k_var )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_scalar_i  ***
      !!
      !! ** Purpose :  read integer scalar in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library
      !!
      !! ** Action : - get variable informations
      !!             - read variable
      !!
      !! Reference :
      !!
      !! History :
      !!    06-09  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), INTENT(in)            :: &
         cd_var                                        ! variable name
      INTEGER, INTENT(out)                     :: &
         k_var                                         ! variable to read in netcdf file

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type                                        ! external data type for this variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr(nf90_open( cd_filename, nf90_nowrite, i_file_id ),&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr(nf90_inquire_variable( i_file_id, i_var_id,xtype=i_type,ndims=i_ndims),&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. 4 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF_INT : 4'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      IF ( i_ndims .NE. 1 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 1'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, k_var ),&
         & __LINE__,__MYFILE__ )

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

   END SUBROUTINE get_scalar_i

   SUBROUTINE get_scalar_r ( cd_filename, cd_var, p_var )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_scalar_r  ***
      !!
      !! ** Purpose :  read real scalar in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library
      !!
      !! ** Action : - get variable informations
      !!             - read variable
      !!
      !! Reference :
      !!
      !! History :
      !!    06-09  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), INTENT(in)            :: &
         cd_var                                        ! variable name
      REAL(KIND=sp), INTENT(out)                        :: &
         p_var                                         ! variable to read in netcdf file

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type                                        ! external data type for this variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type,ndims=i_ndims ),&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. 5 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF_FLOAT : 5'
!         WRITE(*,*)'CALL abort'
!         CALL abort
      ENDIF
      IF ( i_ndims .NE. 1 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 1'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, p_var ) ,__LINE__,__MYFILE__ )

      CALL nchdlerr( nf90_close( i_file_id ),__LINE__,__MYFILE__  )

   END SUBROUTINE get_scalar_r

   SUBROUTINE get_scalar_d ( cd_filename, cd_var, dd_var )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_scalar_d  ***
      !!
      !! ** Purpose :  read double scalar in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library
      !!
      !! ** Action : - get variable informations
      !!             - read variable
      !!
      !! Reference :
      !!
      !! History :
      !!    06-09  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), INTENT(in)            :: &
         cd_var                                        ! variable name
      REAL(KIND=dp), INTENT(out)            :: &
         dd_var                                        ! variable to read in netcdf file

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type                                        ! external data type for this variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ),__LINE__,__MYFILE__ )
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type,ndims=i_ndims),__LINE__,__MYFILE__ )
      IF ( i_type .NE. 6 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF_DOUBLE : 6'
!         WRITE(*,*)'CALL abort'
!         CALL abort
      ENDIF
      IF ( i_ndims .NE. 1 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 1'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, dd_var ),__LINE__,__MYFILE__ )

      CALL nchdlerr( nf90_close( i_file_id ),__LINE__,__MYFILE__ )

   END SUBROUTINE get_scalar_d

   !!======================================================================

   SUBROUTINE put_scalar_i ( cd_filename, k_var, cd_var, cd_longname )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE put_scalar_i  ***
      !!
      !! ** Purpose :  write an integer scalar in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - (re)define variable
      !!             - put variable in netcdf file
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used


      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename,                             &    ! filename
         cd_var                                        ! variable name
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_longname                                   ! variable longname
      INTEGER, INTENT(in)                      :: &
        k_var                                          ! variable to write in netcdf file

      !! * local declarations
      LOGICAL               :: llexist                 ! test
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_dim_id,                                 &   ! dimension identifier
         i_var_exist, i_dim_exist                      ! tests 
      INTEGER, DIMENSION(1) ::                     &
         i_dim                                          ! netcdf information 
      !!----------------------------------------------------------------------

      !
      ! Open or create file
      ! -------------------
      INQUIRE( FILE=cd_filename, EXIST=llexist )
      IF ( .NOT. llexist ) THEN
         CALL nchdlerr( nf90_create( cd_filename, nf90_clobber, i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
      ELSE
         CALL nchdlerr( nf90_open( cd_filename, nf90_write, i_file_id ) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      i_dim_exist =  nf90_inq_dimid( i_file_id, 'scalar', i_dim_id ) 
      IF ( i_dim_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_def_dim( i_file_id, 'scalar', 1, i_dim_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      i_dim(1) = i_dim_id

      !
      ! Define variable or find it
      ! --------------------------
      i_var_exist = nf90_inq_varid( i_file_id, cd_var, i_var_id )
      IF ( i_var_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_def_var( i_file_id, cd_var, nf90_int, i_dim, i_var_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'standard_name', TRIM(cd_var) ) ,&
            & __LINE__,__MYFILE__)
         IF ( PRESENT(cd_longname) ) THEN
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'long_name', TRIM(cd_longname) ) ,&
               & __LINE__,__MYFILE__)
         ENDIF
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      ! Put variable(s)
      ! --------------
      CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, k_var ) ,&
         & __LINE__,__MYFILE__)

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

   END SUBROUTINE put_scalar_i

   SUBROUTINE put_scalar_r ( cd_filename, p_var, cd_var, cd_longname )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE put_scalar_r  ***
      !!
      !! ** Purpose :  write an real scalar in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - (re)define variable
      !!             - put variable in netcdf file
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename,                             &    ! filename
         cd_var                                        ! variable name
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_longname                                   ! variable longname
      REAL(KIND=sp), INTENT(in)                         :: &
        p_var                                          ! variable to write in netcdf file

      !! * local declarations
      LOGICAL               :: llexist                 ! test
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_dim_id,                                 &   ! dimension identifier
         i_var_exist, i_dim_exist                      ! tests 
      INTEGER, DIMENSION(1) ::                     &
         i_dim                                          ! netcdf information 
      !!----------------------------------------------------------------------

      !
      ! Open or create file
      ! -------------------
      INQUIRE( FILE=cd_filename, EXIST=llexist )
      IF ( .NOT. llexist ) THEN
         CALL nchdlerr( nf90_create( cd_filename, nf90_clobber, i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
      ELSE
         CALL nchdlerr( nf90_open( cd_filename, nf90_write, i_file_id ) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      i_dim_exist =  nf90_inq_dimid( i_file_id, 'scalar', i_dim_id ) 
      IF ( i_dim_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_def_dim( i_file_id, 'scalar', 1, i_dim_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      i_dim(1) = i_dim_id

      !
      ! Define variable or find it
      ! --------------------------
      i_var_exist = nf90_inq_varid( i_file_id, cd_var, i_var_id )
      IF ( i_var_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_def_var( i_file_id, cd_var, nf90_real, i_dim, i_var_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'standard_name', TRIM(cd_var) ) ,&
            & __LINE__,__MYFILE__)
         IF ( PRESENT(cd_longname) ) THEN
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'long_name', TRIM(cd_longname) ) ,&
               & __LINE__,__MYFILE__)
         ENDIF
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      ! Put variable(s)
      ! --------------
      CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, p_var ) ,&
         & __LINE__,__MYFILE__)

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

   END SUBROUTINE put_scalar_r

   SUBROUTINE put_scalar_d ( cd_filename, dd_var, cd_var, cd_longname )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE put_scalar_d  ***
      !!
      !! ** Purpose :  write an integer scalar in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - (re)define variable
      !!             - put variable in netcdf file
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used


      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename,                             &    ! filename
         cd_var                                        ! variable name
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_longname                                   ! variable longname
      REAL(KIND=dp), INTENT(in)             :: &
        dd_var                                         ! variable to write in netcdf file

      !! * local declarations
      LOGICAL               :: llexist                 ! test
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_dim_id,                                 &   ! dimension identifier
         i_var_exist, i_dim_exist                      ! tests 
      INTEGER, DIMENSION(1) ::                     &
         i_dim                                          ! netcdf information 
      !!----------------------------------------------------------------------

      !
      ! Open or create file
      ! -------------------
      INQUIRE( FILE=cd_filename, EXIST=llexist )
      IF ( .NOT. llexist ) THEN
         CALL nchdlerr( nf90_create( cd_filename, nf90_clobber, i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
      ELSE
         CALL nchdlerr( nf90_open( cd_filename, nf90_write, i_file_id ) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      i_dim_exist =  nf90_inq_dimid( i_file_id, 'scalar', i_dim_id ) 
      IF ( i_dim_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_def_dim( i_file_id, 'double', 1, i_dim_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      i_dim(1) = i_dim_id

      !
      ! Define variable or find it
      ! --------------------------
      i_var_exist = nf90_inq_varid( i_file_id, cd_var, i_var_id )
      IF ( i_var_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_def_var( i_file_id, cd_var, nf90_double, i_dim, i_var_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'standard_name', TRIM(cd_var) ) ,&
            & __LINE__,__MYFILE__)
         IF ( PRESENT(cd_longname) ) THEN
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'long_name', TRIM(cd_longname) ) ,&
               & __LINE__,__MYFILE__)
         ENDIF
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      ! Put variable(s)
      ! --------------
      CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, dd_var ) ,&
         & __LINE__,__MYFILE__)

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

   END SUBROUTINE put_scalar_d

   !!======================================================================

   SUBROUTINE put_map_2d_r ( cd_filename, p_var, p_missing, cd_var, k_code, k_time, reftime, leadtime, time_bnd, cd_descr )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE put_map_2d_r  ***
      !!
      !! ** Purpose :  write 2d map of reals in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - (re)define variable
      !!             - put variable in netcdf file
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_var                                        ! variable name
      INTEGER, OPTIONAL, INTENT(in)            :: &
        k_code,                                   &    ! CF code
        k_time,                                   &    ! time of the variable
        leadtime                                       ! leadtime of the variable
      INTEGER, DIMENSION(:), OPTIONAL, INTENT(in) :: &
        time_bnd                                       ! time_bnd
      REAL(KIND=sp), OPTIONAL, INTENT(in)               :: &
        p_missing,                                &    ! missing value of the variable
        reftime                                        ! reftime of the variable
      REAL(KIND=sp), DIMENSION(:,:), INTENT(in)         :: &
        p_var                                          ! variable to write in netcdf file
      CHARACTER(len=80), DIMENSION(6), OPTIONAL, INTENT(in)   :: &
         cd_descr                                               ! description of file contents

      !! * local declarations
      LOGICAL               :: llexist                 ! test
      CHARACTER(len=80)     :: cl_var                  ! new variable name
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_dim_x_id, i_dim_y_id, i_time_id,        &   ! dimension identifier
         i_reftime_id, i_leadtime_id,              &   ! dimension identifier
         i_time_bnd_id, i_dim_id,                  &   ! dimension identifier
         i_var_exist, i_pos, ii                        ! test and temporary variable
      INTEGER, DIMENSION(1) :: i_time_bnd              ! dimensions of the variable
      INTEGER, DIMENSION(2) :: i_dimxy, i_len          ! dimensions of the variable
      INTEGER, DIMENSION(3) ::                     &
         i_tab_start, i_tab_count, i_dim               ! netcdf information 
      INTEGER, DIMENSION(:), ALLOCATABLE        :: &
         i_time                                        ! time
      REAL                  ::                     &
         z_min, z_max, z_min_old, z_max_old            ! Minima and maxima of variable
      !!----------------------------------------------------------------------

      ! Read dimensions and compute min and max
      ! ---------------------------------------
      i_dimxy = SHAPE( p_var)
      i_tab_start(1) = 1
      i_tab_count(1) = i_dimxy(1)
      i_tab_start(2) = 1
      i_tab_count(2) = i_dimxy(2)
      i_tab_count(3) = 1

      IF ( PRESENT(p_missing) ) THEN
         z_min = MINVAL(p_var, mask = p_var .NE. p_missing )
         z_max = MAXVAL(p_var, mask = p_var .NE. p_missing )
      ELSE
         z_min = MINVAL(p_var )
         z_max = MAXVAL(p_var )
      ENDIF

      ! Define the name of the variable
      ! -------------------------------
      cl_var = "var"
      IF ( PRESENT(cd_var) ) cl_var = cd_var
      IF ( PRESENT(k_code) ) CALL get_var_info( k_code, cl_var )

      !
      ! Open or create file
      ! -------------------
      INQUIRE( FILE=cd_filename, EXIST=llexist )
      IF ( .NOT. llexist ) THEN
         IF ( PRESENT( cd_descr ) ) THEN
            CALL create_map ( cd_filename, i_dimxy, cd_descr=cd_descr )
         ELSE
            CALL create_map ( cd_filename, i_dimxy )
         ENDIF
      ENDIF

      CALL nchdlerr( nf90_open( cd_filename, nf90_write, i_file_id ) ,&
         & __LINE__,__MYFILE__)

      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'time', i_time_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'x', i_dim_x_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'y', i_dim_y_id ) ,&
         & __LINE__,__MYFILE__)
      i_dim(1) = i_dim_x_id
      i_dim(2) = i_dim_y_id
      i_dim(3) = i_time_id

      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dim(1), len=i_len(1) ),__LINE__,__MYFILE__ )
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dim(2), len=i_len(2) ) ,&
         & __LINE__,__MYFILE__)

      IF ( (i_len(1) .NE. i_dimxy(1)) .OR. (i_len(2) .NE. i_dimxy(2)) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cl_var
         WRITE(*,*)'shape of array = ',i_dimxy
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Define variable or find it
      ! --------------------------
      i_var_exist = nf90_inq_varid( i_file_id, cl_var, i_var_id )
      IF ( i_var_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_def_var( i_file_id, cl_var, nf90_real, i_dim, i_var_id ) ,&
            & __LINE__,__MYFILE__)
         IF ( PRESENT(k_code) ) CALL put_att( i_file_id, i_var_id, k_code )
         IF ( PRESENT(p_missing) )   &
           & CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, '_FillValue', p_missing ) ,&
           & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', z_min ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', z_max ) ,&
            & __LINE__,__MYFILE__)
         i_var_exist = nf90_inq_varid( i_file_id, 'time', i_time_id )
         IF ( i_var_exist .NE. 0 ) THEN
            CALL nchdlerr( nf90_def_var( i_file_id, 'time', nf90_int, i_dim(3), i_time_id ) ,&
               & __LINE__,__MYFILE__)
         ENDIF
         IF ( PRESENT(reftime) ) THEN
            i_var_exist = nf90_inq_varid( i_file_id, 'reftime', i_reftime_id )
            IF ( i_var_exist .NE. 0 ) THEN
               CALL nchdlerr( nf90_def_var( i_file_id, 'reftime', nf90_real, i_dim(3), i_reftime_id ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_reftime_id, 'units', 'days since 1950-01-01 00:00:00' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_reftime_id, 'standard_name', 'forecast_reference_time' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_reftime_id, 'long_name', 'forecast reference time' ) ,&
                  & __LINE__,__MYFILE__)
            ENDIF
         ENDIF
         IF ( PRESENT(leadtime) ) THEN
            i_var_exist = nf90_inq_varid( i_file_id, 'leadtime', i_leadtime_id )
            IF ( i_var_exist .NE. 0 ) THEN
               CALL nchdlerr( nf90_def_var( i_file_id, 'leadtime', nf90_int, i_dim(3), i_leadtime_id ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'units', 'days' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'standard_name', 'forecast_period' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'long_name', &
                  &         'Time elapsed since the start of the forecast' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'bounds', 'time_bnd' ) ,&
                  & __LINE__,__MYFILE__)
            ENDIF
         ENDIF
         IF ( PRESENT(time_bnd) ) THEN
            i_var_exist = nf90_inq_varid( i_file_id, 'time_bnd', i_time_bnd_id )
            IF ( i_var_exist .NE. 0 ) THEN 
               i_time_bnd=SHAPE(time_bnd)
               CALL nchdlerr( nf90_def_dim( i_file_id, 'time_bnd', i_time_bnd(1) , i_dim_id ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_def_var( i_file_id, 'time_bnd', nf90_int, (/i_dim_id, i_dim(3)/), i_time_bnd_id ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_time_bnd_id, 'units', 'days' ) ,&
                  & __LINE__,__MYFILE__)
            ENDIF
         ENDIF
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         i_tab_start(3) = 1
         i_pos = 1
      ELSE
         CALL nchdlerr( nf90_get_att( i_file_id, i_var_id, 'valid_min', z_min_old ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_get_att( i_file_id, i_var_id, 'valid_max', z_max_old ) ,&
            & __LINE__,__MYFILE__)
         z_min = MIN( z_min, z_min_old )
         z_max = MAX( z_max, z_max_old )
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', z_min ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', z_max ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_time_id, len=i_pos ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_inq_varid( i_file_id, 'time', i_time_id ) ,&
            & __LINE__,__MYFILE__)
         IF ( PRESENT(reftime) ) THEN
            CALL nchdlerr( nf90_inq_varid( i_file_id, 'reftime', i_reftime_id ) ,&
               & __LINE__,__MYFILE__) 
         ENDIF
         IF ( PRESENT(leadtime) ) THEN
            CALL nchdlerr( nf90_inq_varid( i_file_id, 'leadtime', i_leadtime_id ) ,&
               & __LINE__,__MYFILE__) 
         ENDIF
         IF ( PRESENT(time_bnd) ) THEN
            i_time_bnd=SHAPE(time_bnd)
            CALL nchdlerr( nf90_inq_varid( i_file_id, 'time_bnd', i_time_bnd_id ) ,&
               & __LINE__,__MYFILE__) 
         ENDIF
         ALLOCATE( i_time(i_pos) )
         CALL nchdlerr( nf90_get_var( i_file_id, i_time_id, i_time ) ,&
            & __LINE__,__MYFILE__)
         IF ( PRESENT(k_time) ) THEN
            DO ii =  1, i_pos 
               IF ( k_time .EQ. i_time(ii) ) THEN
                  i_pos = ii - 1
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         DEALLOCATE( i_time )
         i_pos = i_pos + 1
         i_tab_start(3) = i_pos 
      ENDIF

      ! Put variable(s)
      ! --------------
      CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, p_var, &
         & start=i_tab_start, count=i_tab_count ) ,&
         & __LINE__,__MYFILE__)
      IF ( PRESENT(k_time) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_time_id, (/k_time/), &
            & start=(/i_tab_start(3)/), count=(/i_tab_count(3)/) ) ,&
            & __LINE__,__MYFILE__)
      ELSE
         CALL nchdlerr( nf90_put_var( i_file_id, i_time_id, (/i_pos/), &
            & start=(/i_tab_start(3)/), count=(/i_tab_count(3)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      IF ( PRESENT(reftime) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_reftime_id, (/reftime/), &
            & start=(/i_tab_start(3)/), count=(/i_tab_count(3)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      IF ( PRESENT(leadtime) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_leadtime_id, (/leadtime/), &
            & start=(/i_tab_start(3)/), count=(/i_tab_count(3)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      IF ( PRESENT(time_bnd) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_time_bnd_id, time_bnd, &
                                    start=(/1,i_tab_start(3)/), count=(/i_time_bnd(1),i_tab_count(3)/) ) ,&
                                    & __LINE__,__MYFILE__)
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

   END SUBROUTINE put_map_2d_r

   SUBROUTINE put_map_2d_d ( cd_filename, dd_var, dd_missing, cd_var, k_code, k_time, reftime, leadtime, time_bnd, cd_descr )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE put_map_2d_d  ***
      !!
      !! ** Purpose :  write 2d map of reals in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - (re)define variable
      !!             - put variable in netcdf file
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used


      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_var                                        ! variable name
      INTEGER, OPTIONAL, INTENT(in)            :: &
        k_code,                                   &    ! CF code
        k_time,                                   &    ! time of the variable
        leadtime                                       ! leadtime of the variable
      INTEGER, DIMENSION(:), OPTIONAL, INTENT(in) :: &
        time_bnd                                       ! time_bnd
      REAL(KIND=dp), OPTIONAL, INTENT(in)      :: &
        dd_missing,                               &    ! missing value of the variable
        reftime                                        ! reftime of the variable
      REAL(KIND=dp), DIMENSION(:,:), INTENT(in)  :: &
        dd_var                                         ! variable to write in netcdf file
      CHARACTER(len=80), DIMENSION(6), OPTIONAL, INTENT(in)   :: &
         cd_descr                                               ! description of file contents

      !! * local declarations
      LOGICAL               :: llexist                 ! test
      CHARACTER(len=80)     :: cl_var                  ! new variable name
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_dim_x_id, i_dim_y_id, i_time_id,        &   ! dimension identifier
         i_reftime_id, i_leadtime_id,              &   ! dimension identifier
         i_time_bnd_id, i_dim_id,                  &   ! dimension identifier
         i_var_exist, i_pos, ii                        ! test and temporary variable
      INTEGER, DIMENSION(1) :: i_time_bnd              ! dimensions of the variable
      INTEGER, DIMENSION(2) :: i_dimxy, i_len          ! dimensions of the variable
      INTEGER, DIMENSION(3) ::                     &
         i_tab_start, i_tab_count, i_dim               ! netcdf information 
      INTEGER, DIMENSION(:), ALLOCATABLE        :: &
         i_time                                        ! time
      REAL(KIND=dp)      ::                     &
         dl_min, dl_max, dl_min_old, dl_max_old        ! Minima and maxima of variable
      !!----------------------------------------------------------------------

      ! Read dimensions and compute min and max
      ! ---------------------------------------
      i_dimxy = SHAPE( dd_var)
      i_tab_start(1) = 1
      i_tab_count(1) = i_dimxy(1)
      i_tab_start(2) = 1
      i_tab_count(2) = i_dimxy(2)
      i_tab_count(3) = 1

      IF ( PRESENT(dd_missing) ) THEN
         dl_min = MINVAL( dd_var, mask = dd_var .NE. dd_missing )
         dl_max = MAXVAL( dd_var, mask = dd_var .NE. dd_missing )
      ELSE
         dl_min = MINVAL( dd_var )
         dl_max = MAXVAL( dd_var )
      ENDIF

      ! Define the name of the variable
      ! -------------------------------
      cl_var = "var"
      IF ( PRESENT(cd_var) ) cl_var = cd_var
      IF ( PRESENT(k_code) ) CALL get_var_info( k_code, cl_var )

      !
      ! Open or create file
      ! -------------------
      INQUIRE( FILE=cd_filename, EXIST=llexist )
      IF ( .NOT. llexist ) THEN
         IF ( PRESENT( cd_descr ) ) THEN
            CALL create_map ( cd_filename, i_dimxy, cd_descr=cd_descr )
         ELSE
            CALL create_map ( cd_filename, i_dimxy )
         ENDIF
      ENDIF

      CALL nchdlerr( nf90_open( cd_filename, nf90_write, i_file_id ) ,&
         & __LINE__,__MYFILE__)

      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'time', i_time_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'x', i_dim_x_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'y', i_dim_y_id ) ,&
         & __LINE__,__MYFILE__)
      i_dim(1) = i_dim_x_id
      i_dim(2) = i_dim_y_id
      i_dim(3) = i_time_id

      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dim(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dim(2), len=i_len(2) ) ,&
         & __LINE__,__MYFILE__)

      IF ( (i_len(1) .NE. i_dimxy(1)) .OR. (i_len(2) .NE. i_dimxy(2)) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cl_var
         WRITE(*,*)'shape of array = ',i_dimxy
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Define variable or find it
      ! --------------------------
      i_var_exist = nf90_inq_varid( i_file_id, cl_var, i_var_id )
      IF ( i_var_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_def_var( i_file_id, cl_var, nf90_double, i_dim, i_var_id ) ,&
            & __LINE__,__MYFILE__)
         IF ( PRESENT(k_code) ) CALL put_att( i_file_id, i_var_id, k_code )
         IF ( PRESENT(dd_missing) )   &
            & CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, '_FillValue', dd_missing ),__LINE__,__MYFILE__ )
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', dl_min ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', dl_max ) ,&
            & __LINE__,__MYFILE__)
         i_var_exist = nf90_inq_varid( i_file_id, 'time', i_time_id )
         IF ( i_var_exist .NE. 0 ) THEN
            CALL nchdlerr( nf90_def_var( i_file_id, 'time', nf90_int, i_dim(3), i_time_id ) ,&
               & __LINE__,__MYFILE__)
         ENDIF
         IF ( PRESENT(reftime) ) THEN
            i_var_exist = nf90_inq_varid( i_file_id, 'reftime', i_reftime_id )
            IF ( i_var_exist .NE. 0 ) THEN
               CALL nchdlerr( nf90_def_var( i_file_id, 'reftime', nf90_double, i_dim(3), i_reftime_id ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_reftime_id, 'units', 'days since 1950-01-01 00:00:00' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_reftime_id, 'standard_name', 'forecast_reference_time' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_reftime_id, 'long_name', 'forecast reference time' ) ,&
                  & __LINE__,__MYFILE__)
            ENDIF
         ENDIF
         IF ( PRESENT(leadtime) ) THEN
            i_var_exist = nf90_inq_varid( i_file_id, 'leadtime', i_leadtime_id )
            IF ( i_var_exist .NE. 0 ) THEN
               CALL nchdlerr( nf90_def_var( i_file_id, 'leadtime', nf90_int, i_dim(3), i_leadtime_id ),__LINE__,__MYFILE__ )
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'units', 'days' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'standard_name', 'forecast_period' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'long_name', &
                  & 'Time elapsed since the start of the forecast' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'bounds', 'time_bnd' ) ,&
                  & __LINE__,__MYFILE__)
            ENDIF
         ENDIF
         IF ( PRESENT(time_bnd) ) THEN
            i_var_exist = nf90_inq_varid( i_file_id, 'time_bnd', i_time_bnd_id )
            IF ( i_var_exist .NE. 0 ) THEN 
               i_time_bnd=SHAPE(time_bnd)
               CALL nchdlerr( nf90_def_dim( i_file_id, 'time_bnd', i_time_bnd(1) , i_dim_id ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_def_var( i_file_id, 'time_bnd', nf90_int, (/i_dim_id, i_dim(3)/), i_time_bnd_id ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_time_bnd_id, 'units', 'days' ) ,&
                  & __LINE__,__MYFILE__)
            ENDIF
         ENDIF
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         i_tab_start(3) = 1
         i_pos = 1
      ELSE
         CALL nchdlerr( nf90_get_att( i_file_id, i_var_id, 'valid_min', dl_min_old ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_get_att( i_file_id, i_var_id, 'valid_max', dl_max_old ) ,&
            & __LINE__,__MYFILE__)
         dl_min = MIN( dl_min, dl_min_old )
         dl_max = MAX( dl_max, dl_max_old )
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', dl_min ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', dl_max ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_time_id, len=i_pos ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_inq_varid( i_file_id, 'time', i_time_id ) ,&
            & __LINE__,__MYFILE__)
         IF ( PRESENT(reftime) ) THEN
            CALL nchdlerr( nf90_inq_varid( i_file_id, 'reftime', i_reftime_id ) ,&
               & __LINE__,__MYFILE__) 
         ENDIF
         IF ( PRESENT(leadtime) ) THEN
            CALL nchdlerr( nf90_inq_varid( i_file_id, 'leadtime', i_leadtime_id ) ,&
               & __LINE__,__MYFILE__) 
         ENDIF
         IF ( PRESENT(time_bnd) ) THEN
            i_time_bnd=SHAPE(time_bnd)
            CALL nchdlerr( nf90_inq_varid( i_file_id, 'time_bnd', i_time_bnd_id ) ,&
               & __LINE__,__MYFILE__) 
         ENDIF
         ALLOCATE( i_time(i_pos) )
         CALL nchdlerr( nf90_get_var( i_file_id, i_time_id, i_time ) ,&
            & __LINE__,__MYFILE__)
         DO ii =  1, i_pos 
            IF ( k_time .EQ. i_time(ii) ) THEN
               i_pos = ii - 1
               EXIT
            ENDIF
         ENDDO
         DEALLOCATE( i_time )
         i_pos = i_pos + 1
         i_tab_start(3) = i_pos 
      ENDIF

      ! Put variable(s)
      ! --------------
      CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, dd_var, &
         & start=i_tab_start, count=i_tab_count ) ,&
         & __LINE__,__MYFILE__)
      IF ( PRESENT(k_time) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_time_id, (/k_time/), &
            & start=(/i_tab_start(3)/), count=(/i_tab_count(3)/) ) ,&
            & __LINE__,__MYFILE__)
      ELSE
         CALL nchdlerr( nf90_put_var( i_file_id, i_time_id, (/i_pos/), &
            & start=(/i_tab_start(3)/), count=(/i_tab_count(3)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      IF ( PRESENT(reftime) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_reftime_id, (/reftime/), &
            & start=(/i_tab_start(3)/), count=(/i_tab_count(3)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      IF ( PRESENT(leadtime) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_leadtime_id, (/leadtime/), &
            & start=(/i_tab_start(3)/), count=(/i_tab_count(3)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      IF ( PRESENT(time_bnd) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_time_bnd_id, time_bnd, &
            & start=(/1,i_tab_start(3)/), count=(/i_time_bnd(1),i_tab_count(3)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

   END SUBROUTINE put_map_2d_d

   SUBROUTINE put_map_3d_r ( cd_filename, p_var, p_missing, cd_var, k_code, k_time, reftime, leadtime, time_bnd, cd_descr )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE put_map_3d_r  ***
      !!
      !! ** Purpose :  write 3d map of reals in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - (re)define variable
      !!             - put variable in netcdf file
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_var                                        ! variable name
      INTEGER, OPTIONAL, INTENT(in)            :: &
        k_code,                                   &    ! CF code
        k_time,                                   &    ! time of the variable
        leadtime                                       ! leadtime of the variable
      INTEGER, DIMENSION(:), OPTIONAL, INTENT(in) :: &
        time_bnd                                       ! time_bnd
      REAL(KIND=sp), OPTIONAL, INTENT(in)               :: &
        p_missing,                                &    ! missing value of the variable
        reftime                                        ! reftime of the variable
      REAL(KIND=sp), DIMENSION(:,:,:), INTENT(in)       :: &
        p_var                                          ! variable to write in netcdf file
      CHARACTER(len=80), DIMENSION(6), OPTIONAL, INTENT(in)   :: &
         cd_descr                                               ! description of file contents

      !! * local declarations
      LOGICAL               :: llexist                 ! test
      CHARACTER(len=80)     :: cl_var                  ! new variable name
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_dim_x_id, i_dim_y_id,                   &   ! dimension identifiers
         i_dim_z_id, i_time_id,                    &   ! dimension identifiers
         i_reftime_id, i_leadtime_id,              &   ! dimension identifier
         i_time_bnd_id, i_dim_id,                  &   ! dimension identifier
         i_var_exist, i_pos, ii                        ! test and temporary variable
      INTEGER, DIMENSION(1) :: i_time_bnd              ! dimensions of the variable
      INTEGER, DIMENSION(3) :: i_dimxy, i_len          ! dimensions of the variable
      INTEGER, DIMENSION(4) ::                     &
         i_tab_start, i_tab_count, i_dim               ! netcdf information 
      INTEGER, DIMENSION(:), ALLOCATABLE        :: &
         i_time                                        ! time
      REAL                  ::                     &
         z_min, z_max, z_min_old, z_max_old            ! Minima and maxima of variable
      !!----------------------------------------------------------------------

      ! Read dimensions and compute min and max
      ! ---------------------------------------
      i_dimxy = SHAPE( p_var)
      i_tab_start(1) = 1
      i_tab_count(1) = i_dimxy(1)
      i_tab_start(2) = 1
      i_tab_count(2) = i_dimxy(2)
      i_tab_start(3) = 1
      i_tab_count(3) = i_dimxy(3)
      i_tab_count(4) = 1

      IF ( PRESENT(p_missing) ) THEN
         z_min = MINVAL(p_var, mask = p_var .NE. p_missing )
         z_max = MAXVAL(p_var, mask = p_var .NE. p_missing )
      ELSE
         z_min = MINVAL(p_var )
         z_max = MAXVAL(p_var )
      ENDIF

      ! Define the name of the variable
      ! -------------------------------
      cl_var = "var"
      IF ( PRESENT(cd_var) ) cl_var = cd_var
      IF ( PRESENT(k_code) ) CALL get_var_info( k_code, cl_var )

      !
      ! Open or create file
      ! -------------------
      INQUIRE( FILE=cd_filename, EXIST=llexist )
      IF ( .NOT. llexist ) THEN
         IF ( PRESENT( cd_descr ) ) THEN
            CALL create_map ( cd_filename, i_dimxy, cd_descr=cd_descr )
         ELSE
            CALL create_map ( cd_filename, i_dimxy )
         ENDIF
      ENDIF

      CALL nchdlerr( nf90_open( cd_filename, nf90_write, i_file_id ) ,&
         & __LINE__,__MYFILE__)

      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'time', i_time_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'x', i_dim_x_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'y', i_dim_y_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'z', i_dim_z_id ) ,&
         & __LINE__,__MYFILE__)
      i_dim(1) = i_dim_x_id
      i_dim(2) = i_dim_y_id
      i_dim(3) = i_dim_z_id
      i_dim(4) = i_time_id

      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dim(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dim(2), len=i_len(2) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dim(3), len=i_len(3) ) ,&
         & __LINE__,__MYFILE__)

      IF ( (i_len(1) .NE. i_dimxy(1)) .OR. (i_len(2) .NE. i_dimxy(2)) .OR.  &
           (i_len(3) .NE. i_dimxy(3)) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cl_var
         WRITE(*,*)'shape of array = ',i_dimxy
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Define variable or find it
      ! --------------------------
      i_var_exist = nf90_inq_varid( i_file_id, cl_var, i_var_id )
      IF ( i_var_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_def_var( i_file_id, cl_var, nf90_real, i_dim, i_var_id ) ,&
            & __LINE__,__MYFILE__)
         IF ( PRESENT(k_code) ) CALL put_att( i_file_id, i_var_id, k_code )
         IF ( PRESENT(p_missing) )   &
           & CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, '_FillValue', p_missing ) ,&
           & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', z_min ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', z_max ) ,&
            & __LINE__,__MYFILE__)
         i_var_exist = nf90_inq_varid( i_file_id, 'time', i_time_id )
         IF ( i_var_exist .NE. 0 ) THEN
            CALL nchdlerr( nf90_def_var( i_file_id, 'time', nf90_int, i_dim(4), i_time_id ) ,&
               & __LINE__,__MYFILE__)
         ENDIF
         IF ( PRESENT(reftime) ) THEN
            i_var_exist = nf90_inq_varid( i_file_id, 'reftime', i_reftime_id )
            IF ( i_var_exist .NE. 0 ) THEN
               CALL nchdlerr( nf90_def_var( i_file_id, 'reftime', nf90_real, i_dim(4), i_reftime_id ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_reftime_id, 'units', 'days since 1950-01-01 00:00:00' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_reftime_id, 'standard_name', 'forecast_reference_time' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_reftime_id, 'long_name', 'forecast reference time' ) ,&
                  & __LINE__,__MYFILE__)
            ENDIF
         ENDIF
         IF ( PRESENT(leadtime) ) THEN
            i_var_exist = nf90_inq_varid( i_file_id, 'leadtime', i_leadtime_id )
            IF ( i_var_exist .NE. 0 ) THEN
               CALL nchdlerr( nf90_def_var( i_file_id, 'leadtime', nf90_int, i_dim(4), i_leadtime_id ),__LINE__,__MYFILE__ )
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'units', 'days' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'standard_name', 'forecast_period' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'long_name', &
                  & 'Time elapsed since the start of the forecast' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'bounds', 'time_bnd' ) ,&
                  & __LINE__,__MYFILE__)
            ENDIF
         ENDIF
         IF ( PRESENT(time_bnd) ) THEN
            i_var_exist = nf90_inq_varid( i_file_id, 'time_bnd', i_time_bnd_id )
            IF ( i_var_exist .NE. 0 ) THEN 
               i_time_bnd=SHAPE(time_bnd)
               CALL nchdlerr( nf90_def_dim( i_file_id, 'time_bnd', i_time_bnd(1) , i_dim_id ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_def_var( i_file_id, 'time_bnd', nf90_int, (/i_dim_id, i_dim(4)/), i_time_bnd_id ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_time_bnd_id, 'units', 'days' ) ,&
                  & __LINE__,__MYFILE__)
            ENDIF
         ENDIF
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         i_tab_start(4) = 1
         i_pos = 1
      ELSE
         CALL nchdlerr( nf90_get_att( i_file_id, i_var_id, 'valid_min', z_min_old ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_get_att( i_file_id, i_var_id, 'valid_max', z_max_old ) ,&
            & __LINE__,__MYFILE__)
         z_min = MIN( z_min, z_min_old )
         z_max = MAX( z_max, z_max_old )
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', z_min ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', z_max ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_time_id, len=i_pos ),__LINE__,__MYFILE__ )
         CALL nchdlerr( nf90_inq_varid( i_file_id, 'time', i_time_id ) ,&
            & __LINE__,__MYFILE__)
         IF ( PRESENT(reftime) ) THEN
            CALL nchdlerr( nf90_inq_varid( i_file_id, 'reftime', i_reftime_id ) ,&
               & __LINE__,__MYFILE__) 
         ENDIF
         IF ( PRESENT(leadtime) ) THEN
            CALL nchdlerr( nf90_inq_varid( i_file_id, 'leadtime', i_leadtime_id ) ,&
               & __LINE__,__MYFILE__) 
         ENDIF
         IF ( PRESENT(time_bnd) ) THEN
            i_time_bnd=SHAPE(time_bnd)
            CALL nchdlerr( nf90_inq_varid( i_file_id, 'time_bnd', i_time_bnd_id ) ,&
               & __LINE__,__MYFILE__) 
         ENDIF
         ALLOCATE( i_time(i_pos) )
         CALL nchdlerr( nf90_get_var( i_file_id, i_time_id, i_time ) ,&
            & __LINE__,__MYFILE__)
         DO ii =  1, i_pos 
            IF ( k_time .EQ. i_time(ii) ) THEN
               i_pos = ii - 1
               EXIT
            ENDIF
         ENDDO
         DEALLOCATE( i_time )
         i_pos = i_pos + 1
         i_tab_start(4) = i_pos 
      ENDIF

      ! Put variable(s)
      ! --------------
      CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, p_var, &
         & start=i_tab_start, count=i_tab_count ) ,&
         & __LINE__,__MYFILE__)
      IF ( PRESENT(k_time) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_time_id, (/k_time/), &
            & start=(/i_tab_start(4)/), count=(/i_tab_count(4)/) ) ,&
            & __LINE__,__MYFILE__)
      ELSE
         CALL nchdlerr( nf90_put_var( i_file_id, i_time_id, (/i_pos/), &
            & start=(/i_tab_start(4)/), count=(/i_tab_count(4)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      IF ( PRESENT(reftime) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_reftime_id, (/reftime/), &
            & start=(/i_tab_start(4)/), count=(/i_tab_count(4)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      IF ( PRESENT(leadtime) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_leadtime_id, (/leadtime/), &
            & start=(/i_tab_start(4)/), count=(/i_tab_count(4)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      IF ( PRESENT(time_bnd) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_time_bnd_id, time_bnd, &
            start=(/1,i_tab_start(4)/), count=(/i_time_bnd(1),i_tab_count(4)/) ),__LINE__,__MYFILE__ )
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

   END SUBROUTINE put_map_3d_r

   SUBROUTINE put_map_3d_d ( cd_filename, dd_var, dd_missing, cd_var, k_code, k_time, reftime, leadtime, time_bnd, cd_descr )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE put_map_3d_d  ***
      !!
      !! ** Purpose :  write 3d map of reals in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - (re)define variable
      !!             - put variable in netcdf file
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_var                                        ! variable name
      INTEGER, OPTIONAL, INTENT(in)            :: &
        k_code,                                   &    ! CF code
        k_time,                                   &    ! time of the variable
        leadtime                                       ! leadtime of the variable
      INTEGER, DIMENSION(:), OPTIONAL, INTENT(in) :: &
        time_bnd                                       ! time_bnd
      REAL(KIND=dp), OPTIONAL, INTENT(in)      :: &
        dd_missing,                               &    ! missing value of the variable
        reftime                                        ! reftime of the variable
      REAL(KIND=dp), DIMENSION(:,:,:), INTENT(in)  :: &
        dd_var                                         ! variable to write in netcdf file
      CHARACTER(len=80), DIMENSION(6), OPTIONAL, INTENT(in)   :: &
         cd_descr                                               ! description of file contents

      !! * local declarations
      LOGICAL               :: llexist                 ! test
      CHARACTER(len=80)     :: cl_var                  ! new variable name
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_dim_x_id, i_dim_y_id,                   &   ! dimension identifiers
         i_dim_z_id, i_time_id,                    &   ! dimension identifiers
         i_reftime_id, i_leadtime_id,              &   ! dimension identifier
         i_time_bnd_id, i_dim_id,                  &   ! dimension identifier
         i_var_exist, i_pos, ii                        ! test and temporary variable
      INTEGER, DIMENSION(1) :: i_time_bnd              ! dimensions of the variable
      INTEGER, DIMENSION(3) :: i_dimxy, i_len          ! dimensions of the variable
      INTEGER, DIMENSION(4) ::                     &
         i_tab_start, i_tab_count, i_dim               ! netcdf information 
      INTEGER, DIMENSION(:), ALLOCATABLE        :: &
         i_time                                        ! time
      REAL(KIND=dp)      ::                     &
         dl_min, dl_max, dl_min_old, dl_max_old        ! Minima and maxima of variable
      !!----------------------------------------------------------------------

      ! Read dimensions and compute min and max
      ! ---------------------------------------
      i_dimxy = SHAPE( dd_var)
      i_tab_start(1) = 1
      i_tab_count(1) = i_dimxy(1)
      i_tab_start(2) = 1
      i_tab_count(2) = i_dimxy(2)
      i_tab_start(3) = 1
      i_tab_count(3) = i_dimxy(3)
      i_tab_count(4) = 1

      IF ( PRESENT(dd_missing) ) THEN
         dl_min = MINVAL(dd_var, mask = dd_var .NE. dd_missing )
         dl_max = MAXVAL(dd_var, mask = dd_var .NE. dd_missing )
      ELSE
         dl_min = MINVAL(dd_var )
         dl_max = MAXVAL(dd_var )
      ENDIF

      ! Define the name of the variable
      ! -------------------------------
      cl_var = "var"
      IF ( PRESENT(cd_var) ) cl_var = cd_var
      IF ( PRESENT(k_code) ) CALL get_var_info( k_code, cl_var )

      !
      ! Open or create file
      ! -------------------
      INQUIRE( FILE=cd_filename, EXIST=llexist )
      IF ( .NOT. llexist ) THEN
         IF ( PRESENT( cd_descr ) ) THEN
            CALL create_map ( cd_filename, i_dimxy, cd_descr=cd_descr )
         ELSE
            CALL create_map ( cd_filename, i_dimxy )
         ENDIF
      ENDIF

      CALL nchdlerr( nf90_open( cd_filename, nf90_write, i_file_id ) ,&
         & __LINE__,__MYFILE__)

      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'time', i_time_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'x', i_dim_x_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'y', i_dim_y_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'z', i_dim_z_id ) ,&
         & __LINE__,__MYFILE__)
      i_dim(1) = i_dim_x_id
      i_dim(2) = i_dim_y_id
      i_dim(3) = i_dim_z_id
      i_dim(4) = i_time_id

      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dim(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dim(2), len=i_len(2) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dim(3), len=i_len(3) ) ,&
         & __LINE__,__MYFILE__)

      IF ( (i_len(1) .NE. i_dimxy(1)) .OR. (i_len(2) .NE. i_dimxy(2)) .OR.  &
           (i_len(3) .NE. i_dimxy(3)) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cl_var
         WRITE(*,*)'shape of array = ',i_dimxy
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      ! Define variable or find it
      ! --------------------------
      i_var_exist = nf90_inq_varid( i_file_id, cl_var, i_var_id )
      IF ( i_var_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_def_var( i_file_id, cl_var, nf90_double, i_dim, i_var_id ) ,&
            & __LINE__,__MYFILE__)
         IF ( PRESENT(k_code) ) CALL put_att( i_file_id, i_var_id, k_code )
         IF ( PRESENT(dd_missing) )   &
           & CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, '_FillValue', dd_missing ) ,&
           & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', dl_min ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', dl_max ) ,&
            & __LINE__,__MYFILE__)
         i_var_exist = nf90_inq_varid( i_file_id, 'time', i_time_id )
         IF ( i_var_exist .NE. 0 ) THEN
            CALL nchdlerr( nf90_def_var( i_file_id, 'time', nf90_int, i_dim(4), i_time_id ) ,&
               & __LINE__,__MYFILE__)
         ENDIF
         IF ( PRESENT(reftime) ) THEN
            i_var_exist = nf90_inq_varid( i_file_id, 'reftime', i_reftime_id )
            IF ( i_var_exist .NE. 0 ) THEN
               CALL nchdlerr( nf90_def_var( i_file_id, 'reftime', nf90_double, i_dim(4), i_reftime_id ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_reftime_id, 'units', 'days since 1950-01-01 00:00:00' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_reftime_id, 'standard_name', 'forecast_reference_time' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_reftime_id, 'long_name', 'forecast reference time' ) ,&
                  & __LINE__,__MYFILE__)
            ENDIF
         ENDIF
         IF ( PRESENT(leadtime) ) THEN
            i_var_exist = nf90_inq_varid( i_file_id, 'leadtime', i_leadtime_id )
            IF ( i_var_exist .NE. 0 ) THEN
               CALL nchdlerr( nf90_def_var( i_file_id, 'leadtime', nf90_int, i_dim(4), i_leadtime_id ),__LINE__,__MYFILE__ )
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'units', 'days' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'standard_name', 'forecast_period' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'long_name', &
                  & 'Time elapsed since the start of the forecast' ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_leadtime_id, 'bounds', 'time_bnd' ) ,&
                  & __LINE__,__MYFILE__)
            ENDIF
         ENDIF
         IF ( PRESENT(time_bnd) ) THEN
            i_var_exist = nf90_inq_varid( i_file_id, 'time_bnd', i_time_bnd_id )
            IF ( i_var_exist .NE. 0 ) THEN 
               i_time_bnd=SHAPE(time_bnd)
               CALL nchdlerr( nf90_def_dim( i_file_id, 'time_bnd', i_time_bnd(1) , i_dim_id ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_def_var( i_file_id, 'time_bnd', nf90_int, (/i_dim_id, i_dim(4)/), i_time_bnd_id ) ,&
                  & __LINE__,__MYFILE__)
               CALL nchdlerr( nf90_put_att( i_file_id, i_time_bnd_id, 'units', 'days' ) ,&
                  & __LINE__,__MYFILE__)
            ENDIF
         ENDIF
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         i_tab_start(4) = 1
         i_pos = 1
      ELSE
         CALL nchdlerr( nf90_get_att( i_file_id, i_var_id, 'valid_min', dl_min_old ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_get_att( i_file_id, i_var_id, 'valid_max', dl_max_old ) ,&
            & __LINE__,__MYFILE__)
         dl_min = MIN( dl_min, dl_min_old )
         dl_max = MAX( dl_max, dl_max_old )
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', dl_min ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', dl_max ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_time_id, len=i_pos ),__LINE__,__MYFILE__ )
         CALL nchdlerr( nf90_inq_varid( i_file_id, 'time', i_time_id ) ,&
            & __LINE__,__MYFILE__)
         IF ( PRESENT(reftime) ) THEN
            CALL nchdlerr( nf90_inq_varid( i_file_id, 'reftime', i_reftime_id ) ,&
               & __LINE__,__MYFILE__) 
         ENDIF
         IF ( PRESENT(leadtime) ) THEN
            CALL nchdlerr( nf90_inq_varid( i_file_id, 'leadtime', i_leadtime_id ) ,&
               & __LINE__,__MYFILE__) 
         ENDIF
         IF ( PRESENT(time_bnd) ) THEN
            i_time_bnd=SHAPE(time_bnd)
            CALL nchdlerr( nf90_inq_varid( i_file_id, 'time_bnd', i_time_bnd_id ) ,&
               & __LINE__,__MYFILE__) 
         ENDIF
         ALLOCATE( i_time(i_pos) )
         CALL nchdlerr( nf90_get_var( i_file_id, i_time_id, i_time ) ,&
            & __LINE__,__MYFILE__)
         DO ii =  1, i_pos 
            IF ( k_time .EQ. i_time(ii) ) THEN
               i_pos = ii - 1
               EXIT
            ENDIF
         ENDDO
         DEALLOCATE( i_time )
         i_pos = i_pos + 1
         i_tab_start(4) = i_pos 
      ENDIF

      ! Put variable(s)
      ! --------------
      CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, dd_var, &
         & start=i_tab_start, count=i_tab_count ) ,&
         & __LINE__,__MYFILE__)
      IF ( PRESENT(k_time) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_time_id, (/k_time/), &
            & start=(/i_tab_start(4)/), count=(/i_tab_count(4)/) ) ,&
            & __LINE__,__MYFILE__)
      ELSE
         CALL nchdlerr( nf90_put_var( i_file_id, i_time_id, (/i_pos/), &
            & start=(/i_tab_start(4)/), count=(/i_tab_count(4)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      IF ( PRESENT(reftime) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_reftime_id, (/reftime/), &
            & start=(/i_tab_start(4)/), count=(/i_tab_count(4)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      IF ( PRESENT(leadtime) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_leadtime_id, (/leadtime/), &
            & start=(/i_tab_start(4)/), count=(/i_tab_count(4)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      IF ( PRESENT(time_bnd) ) THEN
         CALL nchdlerr( nf90_put_var( i_file_id, i_time_bnd_id, time_bnd, &
            & start=(/1,i_tab_start(4)/), count=(/i_time_bnd(1),i_tab_count(4)/) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

   END SUBROUTINE put_map_3d_d

   !!======================================================================

   SUBROUTINE get_var_1d_i ( cd_filename, cd_var, k_var, k_missing )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_var_1d_i  ***
      !!
      !! ** Purpose :  read 1d map of integers in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - read variable 
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_var                                        ! variable name
      INTEGER, DIMENSION(:), INTENT(out)       :: &
        k_var                                          ! variable to read in netcdf file
      INTEGER, OPTIONAL, INTENT(out)           :: &
        k_missing                                      ! missing value of the variable

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type,                                   &   ! external data type for this variable
         i_exist                                       ! test
      INTEGER, DIMENSION(1) :: i_dimid, i_len, i_shape ! dimensions of the variable
      INTEGER               :: i_missing2              ! missing value of the variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. 4 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF_INT : 4'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, ndims=i_ndims ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_ndims .NE. 1 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 1'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, dimids=i_dimid ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      i_shape=SHAPE(k_var)
      IF ( i_len(1) .NE. i_shape(1) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'shape of array = ',SHAPE(k_var)
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, k_var ) ,&
         & __LINE__,__MYFILE__)

      i_exist = nf90_get_att( i_file_id, i_var_id, '_FillValue', i_missing2 )
      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_exist = nf90_get_att( i_file_id, i_var_id, 'missing_value', i_missing2 )
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_missing2 = MAXVAL(k_var)
      ENDIF

      IF ( PRESENT(k_missing) ) THEN
         k_missing = i_missing2
      ELSE
         WHERE(k_var .EQ. i_missing2) k_var = 0.0
         WHERE((k_var .GT. 0.) .EQV. (k_var .LE. 0.)) k_var = 0.0
      ENDIF

   END SUBROUTINE get_var_1d_i

   SUBROUTINE get_var_2d_i ( cd_filename, cd_var, k_var, k_missing )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_var_2d_i  ***
      !!
      !! ** Purpose :  read 2d map of integers in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - read variable 
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_var                                        ! variable name
      INTEGER, DIMENSION(:,:), INTENT(out)        :: &
        k_var                                          ! variable to read in netcdf file
      INTEGER, OPTIONAL, INTENT(out)              :: &
        k_missing                                      ! missing value of the variable

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type,                                   &   ! external data type for this variable
         i_exist                                       ! test
      INTEGER, DIMENSION(2) :: i_dimid, i_len, i_shape ! dimensions of the variable
      INTEGER                  :: i_missing2              ! missing value of the variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. 4 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF_INT : 4'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, ndims=i_ndims ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_ndims .NE. 2 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 2'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, dimids=i_dimid ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(2), len=i_len(2) ) ,&
         & __LINE__,__MYFILE__)
      i_shape=SHAPE(k_var)
      IF ( (i_len(1) .NE. i_shape(1)) .OR. (i_len(2) .NE. i_shape(2)) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'shape of array = ',SHAPE(k_var)
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, k_var ) ,&
         & __LINE__,__MYFILE__)

      i_exist = nf90_get_att( i_file_id, i_var_id, '_FillValue', i_missing2 )
      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_exist = nf90_get_att( i_file_id, i_var_id, 'missing_value', i_missing2 )
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_missing2 = MAXVAL(k_var)
      ENDIF

      IF ( PRESENT(k_missing) ) THEN
         k_missing = i_missing2
      ELSE
         WHERE(k_var .EQ. i_missing2) k_var = 0.0
         WHERE((k_var .GT. 0.) .EQV. (k_var .LE. 0.)) k_var = 0.0
      ENDIF

   END SUBROUTINE get_var_2d_i

   SUBROUTINE get_var_3d_i ( cd_filename, cd_var, k_var, k_missing )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_var_3d_i  ***
      !!
      !! ** Purpose :  read 3d map of integers in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - read variable 
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_var                                        ! variable name
      INTEGER, DIMENSION(:,:,:), INTENT(out)      :: &
        k_var                                          ! variable to read in netcdf file
      INTEGER, OPTIONAL, INTENT(out)              :: &
        k_missing                                      ! missing value of the variable

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type,                                   &   ! external data type for this variable
         i_exist                                       ! test
      INTEGER, DIMENSION(3) :: i_dimid, i_len, i_shape ! dimensions of the variable
      INTEGER                  :: i_missing2              ! missing value of the variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. 4 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF_INT : 4'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, ndims=i_ndims ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_ndims .NE. 3 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 3'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, dimids=i_dimid ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(2), len=i_len(2) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(3), len=i_len(3) ) ,&
         & __LINE__,__MYFILE__)
      i_shape=SHAPE(k_var)
      IF ( (i_len(1) .NE. i_shape(1)) .OR. (i_len(2) .NE. i_shape(2)) &
          .OR. (i_len(3) .NE. i_shape(3)) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'shape of array = ',SHAPE(k_var)
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, k_var ) ,&
         & __LINE__,__MYFILE__)

      i_exist = nf90_get_att( i_file_id, i_var_id, '_FillValue', i_missing2 )
      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_exist = nf90_get_att( i_file_id, i_var_id, 'missing_value', i_missing2 )
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

      IF ( i_exist .NE. nf90_noerr  ) THEN
            i_missing2 = MAXVAL(k_var)
      ENDIF

      IF ( PRESENT(k_missing) ) THEN
         k_missing = i_missing2
      ELSE
         WHERE(k_var .EQ. i_missing2) k_var = 0.0
         WHERE((k_var .GT. 0.) .EQV. (k_var .LE. 0.)) k_var = 0.0
      ENDIF

   END SUBROUTINE get_var_3d_i

   SUBROUTINE get_var_4d_i ( cd_filename, cd_var, k_var, k_missing )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_var_4d_i  ***
      !!
      !! ** Purpose :  read 4d map of integers in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - read variable 
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used


      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_var                                        ! variable name
      INTEGER, DIMENSION(:,:,:,:), INTENT(out)    :: &
        k_var                                          ! variable to read in netcdf file
      INTEGER, OPTIONAL, INTENT(out)              :: &
        k_missing                                      ! missing value of the variable

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type,                                   &   ! external data type for this variable
         i_exist                                       ! test
      INTEGER, DIMENSION(4) :: i_dimid, i_len, i_shape ! dimensions of the variable
      INTEGER                  :: i_missing2              ! missing value of the variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. 4 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF_INT : 4'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, ndims=i_ndims ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_ndims .NE. 4 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 4'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, dimids=i_dimid ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(2), len=i_len(2) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(3), len=i_len(3) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(4), len=i_len(4) ) ,&
         & __LINE__,__MYFILE__)
      i_shape=SHAPE(k_var)
      IF ( (i_len(1) .NE. i_shape(1)) .OR. (i_len(2) .NE. i_shape(2)) &
          .OR. (i_len(3) .NE. i_shape(3)) .OR. (i_len(4) .NE. i_shape(4)) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'shape of array = ',SHAPE(k_var)
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, k_var ) ,&
         & __LINE__,__MYFILE__)

      i_exist = nf90_get_att( i_file_id, i_var_id, '_FillValue', i_missing2 )
      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_exist = nf90_get_att( i_file_id, i_var_id, 'missing_value', i_missing2 )
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_missing2 = MAXVAL(k_var)
      ENDIF

      IF ( PRESENT(k_missing) ) THEN
         k_missing = i_missing2
      ELSE
         WHERE(k_var .EQ. i_missing2) k_var = 0.0
         WHERE((k_var .GT. 0.) .EQV. (k_var .LE. 0.)) k_var = 0.0
      ENDIF

   END SUBROUTINE get_var_4d_i

   SUBROUTINE get_var_1d_r ( cd_filename, cd_var, p_var, p_missing )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_var_1d_r  ***
      !!
      !! ** Purpose :  read 1d map of reals in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - read variable 
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_var                                        ! variable name
      REAL(KIND=sp), DIMENSION(:), INTENT(out)          :: &
        p_var                                          ! variable to read in netcdf file
      REAL(KIND=sp), OPTIONAL, INTENT(out)              :: &
        p_missing                                      ! missing value of the variable

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type,                                   &   ! external data type for this variable
         i_exist                                       ! test
      INTEGER, DIMENSION(1) :: i_dimid, i_len, i_shape ! dimensions of the variable
      REAL                  :: z_missing2              ! missing value of the variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ),__LINE__,__MYFILE__ )
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. 5 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF_FLOAT : 5'
!         WRITE(*,*)'CALL abort'
!         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, ndims=i_ndims ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_ndims .NE. 1 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 1'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, dimids=i_dimid ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      i_shape=SHAPE(p_var)
      IF ( i_len(1) .NE. i_shape(1) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'shape of array = ',SHAPE(p_var)
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, p_var ) ,&
         & __LINE__,__MYFILE__)

      i_exist = nf90_get_att( i_file_id, i_var_id, '_FillValue', z_missing2 )
      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_exist = nf90_get_att( i_file_id, i_var_id, 'missing_value', z_missing2 )
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

      IF ( i_exist .NE. nf90_noerr  ) THEN
         z_missing2 = MAXVAL(p_var)
      ENDIF

      IF ( PRESENT(p_missing) ) THEN
         p_missing = z_missing2
      ELSE
         WHERE(p_var .EQ. z_missing2) p_var = 0.0
         WHERE((p_var .GT. 0.) .EQV. (p_var .LE. 0.)) p_var = 0.0
      ENDIF

   END SUBROUTINE get_var_1d_r

   SUBROUTINE get_var_2d_r ( cd_filename, cd_var, p_var, p_missing )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_var_2d_r  ***
      !!
      !! ** Purpose :  read 2d map of reals in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - read variable 
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_var                                        ! variable name
      REAL(KIND=sp), DIMENSION(:,:), INTENT(out)        :: &
        p_var                                          ! variable to read in netcdf file
      REAL(KIND=sp), OPTIONAL, INTENT(out)              :: &
        p_missing                                      ! missing value of the variable

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type,                                   &   ! external data type for this variable
         i_exist                                       ! test
      INTEGER, DIMENSION(2) :: i_dimid, i_len, i_shape ! dimensions of the variable
      REAL                  :: z_missing2              ! missing value of the variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. 5 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF_FLOAT : 5'
!         WRITE(*,*)'CALL abort'
!         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, ndims=i_ndims ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_ndims .NE. 2 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 2'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, dimids=i_dimid ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(2), len=i_len(2) ) ,&
         & __LINE__,__MYFILE__)
      i_shape=SHAPE(p_var)
      IF ( (i_len(1) .NE. i_shape(1)) .OR. (i_len(2) .NE. i_shape(2)) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'shape of array = ',SHAPE(p_var)
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, p_var ) ,&
         & __LINE__,__MYFILE__)

      i_exist = nf90_get_att( i_file_id, i_var_id, '_FillValue', z_missing2 )
      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_exist = nf90_get_att( i_file_id, i_var_id, 'missing_value', z_missing2 )
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

      IF ( i_exist .NE. nf90_noerr  ) THEN
         z_missing2 = MAXVAL(p_var)
      ENDIF

      IF ( PRESENT(p_missing) ) THEN
         p_missing = z_missing2
      ELSE
         WHERE(p_var .EQ. z_missing2) p_var = 0.0
         WHERE((p_var .GT. 0.) .EQV. (p_var .LE. 0.)) p_var = 0.0
      ENDIF

   END SUBROUTINE get_var_2d_r

   SUBROUTINE get_var_3d_r ( cd_filename, cd_var, p_var, p_missing )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_var_3d_r  ***
      !!
      !! ** Purpose :  read 3d map of reals in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - read variable 
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_var                                        ! variable name
      REAL(KIND=sp), DIMENSION(:,:,:), INTENT(out)      :: &
        p_var                                          ! variable to read in netcdf file
      REAL(KIND=sp), OPTIONAL, INTENT(out)              :: &
        p_missing                                      ! missing value of the variable

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type,                                   &   ! external data type for this variable
         i_exist                                       ! test
      INTEGER, DIMENSION(3) :: i_dimid, i_len, i_shape ! dimensions of the variable
      REAL                  :: z_missing2              ! missing value of the variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. 5 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF_FLOAT : 5'
!         WRITE(*,*)'CALL abort'
!         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, ndims=i_ndims ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_ndims .NE. 3 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 3'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, dimids=i_dimid ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(2), len=i_len(2) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(3), len=i_len(3) ) ,&
         & __LINE__,__MYFILE__)
      i_shape=SHAPE(p_var)
      IF ( (i_len(1) .NE. i_shape(1)) .OR. (i_len(2) .NE. i_shape(2)) &
          .OR. (i_len(3) .NE. i_shape(3)) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'shape of array = ',SHAPE(p_var)
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, p_var ) ,&
         & __LINE__,__MYFILE__)

      i_exist = nf90_get_att( i_file_id, i_var_id, '_FillValue', z_missing2 )
      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_exist = nf90_get_att( i_file_id, i_var_id, 'missing_value', z_missing2 )
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

      IF ( i_exist .NE. nf90_noerr  ) THEN
            z_missing2 = MAXVAL(p_var)
      ENDIF

      IF ( PRESENT(p_missing) ) THEN
         p_missing = z_missing2
      ELSE
         WHERE(p_var .EQ. z_missing2) p_var = 0.0
         WHERE((p_var .GT. 0.) .EQV. (p_var .LE. 0.)) p_var = 0.0
      ENDIF

   END SUBROUTINE get_var_3d_r

   SUBROUTINE get_var_4d_r ( cd_filename, cd_var, p_var, p_missing )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_var_4d_r  ***
      !!
      !! ** Purpose :  read 4d map of reals in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - read variable 
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)  :: &
         cd_var                                        ! variable name
      REAL(KIND=sp), DIMENSION(:,:,:,:), INTENT(out)    :: &
        p_var                                          ! variable to read in netcdf file
      REAL(KIND=sp), OPTIONAL, INTENT(out)              :: &
        p_missing                                      ! missing value of the variable

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type,                                   &   ! external data type for this variable
         i_exist                                       ! test
      INTEGER, DIMENSION(4) :: i_dimid, i_len, i_shape ! dimensions of the variable
      REAL                  :: z_missing2              ! missing value of the variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. 5 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF_FLOAT : 5'
!         WRITE(*,*)'CALL abort'
!         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, ndims=i_ndims ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_ndims .NE. 4 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 4'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, dimids=i_dimid ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(2), len=i_len(2) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(3), len=i_len(3) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(4), len=i_len(4) ) ,&
         & __LINE__,__MYFILE__)
      i_shape=SHAPE(p_var)
      IF ( (i_len(1) .NE. i_shape(1)) .OR. (i_len(2) .NE. i_shape(2)) &
          .OR. (i_len(3) .NE. i_shape(3)) .OR. (i_len(4) .NE. i_shape(4)) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'shape of array = ',SHAPE(p_var)
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, p_var ) ,&
         & __LINE__,__MYFILE__)

      i_exist = nf90_get_att( i_file_id, i_var_id, '_FillValue', z_missing2 )
      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_exist = nf90_get_att( i_file_id, i_var_id, 'missing_value', z_missing2 )
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

      IF ( i_exist .NE. nf90_noerr  ) THEN
         z_missing2 = MAXVAL(p_var)
      ENDIF

      IF ( PRESENT(p_missing) ) THEN
         p_missing = z_missing2
      ELSE
         WHERE(p_var .EQ. z_missing2) p_var = 0.0
         WHERE((p_var .GT. 0.) .EQV. (p_var .LE. 0.)) p_var = 0.0
      ENDIF

   END SUBROUTINE get_var_4d_r

   SUBROUTINE get_var_1d_d ( cd_filename, cd_var, dd_var, dd_missing )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_var_1d_d  ***
      !!
      !! ** Purpose :  read 1d map of real(kind=dp) reals in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - read variable 
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)               :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)     :: &
         cd_var                                        ! variable name
      REAL(KIND=dp), DIMENSION(:), INTENT(out) :: &
         dd_var                                        ! variable to read in netcdf file
      REAL(KIND=dp), OPTIONAL, INTENT(out)     :: &
         dd_missing                                    ! missing value of the variable

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type,                                   &   ! external data type for this variable
         i_exist                                       ! test
      INTEGER, DIMENSION(1) :: i_dimid, i_len, i_shape ! dimensions of the variable
      REAL(KIND=dp)      :: dl_missing2             ! missing value of the variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. NF90_DOUBLE ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF90_DOUBLE : 6'
!         WRITE(*,*)'CALL abort'
!         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, ndims=i_ndims ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_ndims .NE. 1 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 1'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, dimids=i_dimid ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      i_shape=SHAPE(dd_var)
      IF ( i_len(1) .NE. i_shape(1) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'shape of array = ',SHAPE(dd_var)
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, dd_var ) ,&
         & __LINE__,__MYFILE__)

      i_exist = nf90_get_att( i_file_id, i_var_id, '_FillValue', dl_missing2 )
      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_exist = nf90_get_att( i_file_id, i_var_id, 'missing_value', dl_missing2 )
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

      IF ( i_exist .NE. nf90_noerr  ) THEN
         dl_missing2 = MAXVAL(dd_var)
      ENDIF

      IF ( PRESENT(dd_missing) ) THEN
         dd_missing = dl_missing2
      ELSE
         WHERE(dd_var .EQ. dl_missing2) dd_var = 0.0
         WHERE((dd_var .GT. 0.) .EQV. (dd_var .LE. 0.)) dd_var = 0.0
      ENDIF

   END SUBROUTINE get_var_1d_d

   SUBROUTINE get_var_2d_d ( cd_filename, cd_var, dd_var, dd_missing )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_var_2d_d  ***
      !!
      !! ** Purpose :  read 2d map of real(kind=dp) reals in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - read variable 
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)               :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)     :: &
         cd_var                                        ! variable name
      REAL(KIND=dp), DIMENSION(:,:), INTENT(out) :: &
         dd_var                                        ! variable to read in netcdf file
      REAL(KIND=dp), OPTIONAL, INTENT(out)     :: &
         dd_missing                                    ! missing value of the variable

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type,                                   &   ! external data type for this variable
         i_exist                                       ! test
      INTEGER, DIMENSION(2) :: i_dimid, i_len, i_shape ! dimensions of the variable
      REAL(KIND=dp)      :: dl_missing2             ! missing value of the variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. NF90_DOUBLE ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF90_DOUBLE : 6'
!         WRITE(*,*)'CALL abort'
!         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, ndims=i_ndims ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_ndims .NE. 2 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 2'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, dimids=i_dimid ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(2), len=i_len(2) ) ,&
         & __LINE__,__MYFILE__)
      i_shape=SHAPE(dd_var)
      IF ( (i_len(1) .NE. i_shape(1)) .OR. (i_len(2) .NE. i_shape(2)) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'shape of array = ',SHAPE(dd_var)
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, dd_var ) ,&
         & __LINE__,__MYFILE__)

      i_exist = nf90_get_att( i_file_id, i_var_id, '_FillValue', dl_missing2 )
      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_exist = nf90_get_att( i_file_id, i_var_id, 'missing_value', dl_missing2 )
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

      IF ( i_exist .NE. nf90_noerr  ) THEN
         dl_missing2 = MAXVAL(dd_var)
      ENDIF

      IF ( PRESENT(dd_missing) ) THEN
         dd_missing = dl_missing2
      ELSE
         WHERE(dd_var .EQ. dl_missing2) dd_var = 0.0
         WHERE((dd_var .GT. 0.) .EQV. (dd_var .LE. 0.)) dd_var = 0.0
      ENDIF

   END SUBROUTINE get_var_2d_d

   SUBROUTINE get_var_3d_d ( cd_filename, cd_var, dd_var, dd_missing )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_var_3d_d  ***
      !!
      !! ** Purpose :  read 3d map of real(kind=dp) reals in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - read variable 
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)               :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)     :: &
         cd_var                                        ! variable name
      REAL(KIND=dp), DIMENSION(:,:,:), INTENT(out) :: &
         dd_var                                        ! variable to read in netcdf file
      REAL(KIND=dp), OPTIONAL, INTENT(out)     :: &
         dd_missing                                    ! missing value of the variable

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type,                                   &   ! external data type for this variable
         i_exist                                       ! test
      INTEGER, DIMENSION(3) :: i_dimid, i_len, i_shape ! dimensions of the variable
      REAL(KIND=dp)      :: dl_missing2             ! missing value of the variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. NF90_DOUBLE ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF90_DOUBLE : 6'
!         WRITE(*,*)'CALL abort'
!         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, ndims=i_ndims ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_ndims .NE. 3 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 3'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, dimids=i_dimid ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(2), len=i_len(2) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(3), len=i_len(3) ) ,&
         & __LINE__,__MYFILE__)
      i_shape=SHAPE(dd_var)
      IF ( (i_len(1) .NE. i_shape(1)) .OR. (i_len(2) .NE. i_shape(2)) &
          .OR. (i_len(3) .NE. i_shape(3)) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'shape of array = ',SHAPE(dd_var)
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, dd_var ) ,&
         & __LINE__,__MYFILE__)

      i_exist = nf90_get_att( i_file_id, i_var_id, '_FillValue', dl_missing2 )
      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_exist = nf90_get_att( i_file_id, i_var_id, 'missing_value', dl_missing2 )
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

      IF ( i_exist .NE. nf90_noerr  ) THEN
            dl_missing2 = MAXVAL(dd_var)
      ENDIF

      IF ( PRESENT(dd_missing) ) THEN
         dd_missing = dl_missing2
      ELSE
         WHERE(dd_var .EQ. dl_missing2) dd_var = 0.0
         WHERE((dd_var .GT. 0.) .EQV. (dd_var .LE. 0.)) dd_var = 0.0
      ENDIF

   END SUBROUTINE get_var_3d_d

   SUBROUTINE get_var_4d_d ( cd_filename, cd_var, dd_var, dd_missing )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE get_var_4d_d  ***
      !!
      !! ** Purpose :  read 4d map of real(kind=dp) reals in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - read variable 
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)               :: &
         cd_filename                                   ! filename
      CHARACTER(len=80), OPTIONAL, INTENT(in)     :: &
         cd_var                                        ! variable name
      REAL(KIND=dp), DIMENSION(:,:,:,:), INTENT(out) :: &
         dd_var                                        ! variable to read in netcdf file
      REAL(KIND=dp), OPTIONAL, INTENT(out)     :: &
         dd_missing                                    ! missing value of the variable

      !! * local declarations
      INTEGER               ::                     &
         i_file_id, i_var_id,                      &   ! file and variable identifier
         i_ndims,                                  &   ! Number of dimensions for this variable
         i_type,                                   &   ! external data type for this variable
         i_exist                                       ! test
      INTEGER, DIMENSION(4) :: i_dimid, i_len, i_shape ! dimensions of the variable
      REAL(KIND=dp)      :: dl_missing2             ! missing value of the variable
      !!----------------------------------------------------------------------

      ! Open and test netcdf file
      ! -------------------------
      CALL nchdlerr( nf90_open( cd_filename, nf90_nowrite, i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_varid( i_file_id, cd_var, i_var_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, xtype=i_type ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_type .NE. NF90_DOUBLE ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'External type for this variable = ',i_type
         WRITE(*,*)'The valid external data type is NF90_DOUBLE : 6'
!         WRITE(*,*)'CALL abort'
!         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, ndims=i_ndims ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_ndims .NE. 4 ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'Number of dimensions for this variable = ',i_ndims
         WRITE(*,*)'The valid number of dimension is 4'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF
      CALL nchdlerr( nf90_inquire_variable( i_file_id, i_var_id, dimids=i_dimid ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(1), len=i_len(1) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(2), len=i_len(2) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(3), len=i_len(3) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dimid(4), len=i_len(4) ) ,&
         & __LINE__,__MYFILE__)
      i_shape=SHAPE(dd_var)
      IF ( (i_len(1) .NE. i_shape(1)) .OR. (i_len(2) .NE. i_shape(2)) &
          .OR. (i_len(3) .NE. i_shape(3)) .OR. (i_len(4) .NE. i_shape(4)) ) THEN
         WRITE(*,*)'filename = ',cd_filename
         WRITE(*,*)'variable = ',cd_var
         WRITE(*,*)'shape of array = ',SHAPE(dd_var)
         WRITE(*,*)'Dimension length of variable = ',i_len
         WRITE(*,*)'Dimensions are different'
         WRITE(*,*)'CALL abort'
         CALL abort
      ENDIF

      ! Read variable
      ! -------------
      CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, dd_var ) ,&
         & __LINE__,__MYFILE__)

      i_exist = nf90_get_att( i_file_id, i_var_id, '_FillValue', dl_missing2 )
      IF ( i_exist .NE. nf90_noerr  ) THEN
         i_exist = nf90_get_att( i_file_id, i_var_id, 'missing_value', dl_missing2 )
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

      IF ( i_exist .NE. nf90_noerr  ) THEN
         dl_missing2 = MAXVAL(dd_var)
      ENDIF

      IF ( PRESENT(dd_missing) ) THEN
         dd_missing = dl_missing2
      ELSE
         WHERE(dd_var .EQ. dl_missing2) dd_var = 0.0
         WHERE((dd_var .GT. 0.) .EQV. (dd_var .LE. 0.)) dd_var = 0.0
      ENDIF

   END SUBROUTINE get_var_4d_d

   !!======================================================================

   SUBROUTINE put_coord_r ( cd_filename, p_lon, p_lat, p_dep, cd_descr )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE put_coord_r  ***
      !!
      !! ** Purpose :  write coordinates as reals in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - (re)define variable
      !!             - put variable in netcdf file
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)            :: &
         cd_filename                                    ! filename
      REAL(KIND=sp), DIMENSION(:,:), INTENT(in)         :: &
         p_lon, p_lat                                   ! longitude and latitude
      REAL(KIND=sp), DIMENSION(:), OPTIONAL, INTENT(in) :: &
         p_dep                                          ! depth
      CHARACTER(len=80), DIMENSION(6), OPTIONAL, INTENT(in)   :: &
         cd_descr                                               ! description of file contents

      !! * local declarations
      LOGICAL               :: llexist                  ! test
      INTEGER               ::                    &
         i_file_id, i_var_id,                     &     ! file and variable identifiers
         i_dim_x_id, i_dim_y_id, i_dim_z_id,      &     ! dimension identifiers
         i_var_exist                                    ! test
      INTEGER, DIMENSION(2) :: i_dimxy                  ! dimensions of longitude and latitude
      INTEGER, DIMENSION(1) :: i_dimz                   ! dimension of depth
      INTEGER, DIMENSION(3) :: i_dim                    ! dimensions for netcdf file
      REAL                  ::                     &
         z_minx, z_maxx, z_miny, z_maxy, z_minz, z_maxz ! minima and maxima
      !!----------------------------------------------------------------------

      ! Read dimensions and compute min and max
      ! ---------------------------------------
      i_dimxy = SHAPE( p_lon ) 
      z_minx = MINVAL( p_lon )
      z_maxx = MAXVAL( p_lon )
      z_miny = MINVAL( p_lat )
      z_maxy = MAXVAL( p_lat )

      IF ( PRESENT( p_dep ) ) THEN
         i_dimz = SHAPE(  p_dep )
         z_minz = MINVAL( p_dep )
         z_maxz = MAXVAL( p_dep )
      ENDIF
   
      ! Open or create netcdf file
      ! --------------------------
      INQUIRE( FILE=TRIM(cd_filename), EXIST=llexist )
      IF ( .NOT. llexist ) THEN
         IF ( PRESENT( p_dep ) ) THEN
            IF ( PRESENT( cd_descr ) ) THEN
               CALL create_map ( cd_filename, i_dimxy, i_dimz, cd_descr=cd_descr )
            ELSE
               CALL create_map ( cd_filename, i_dimxy, i_dimz )
            ENDIF
         ELSE
            IF ( PRESENT( cd_descr ) ) THEN
               CALL create_map ( cd_filename, i_dimxy, cd_descr=cd_descr )
            ELSE
               CALL create_map ( cd_filename, i_dimxy )
            ENDIF
         ENDIF
      ENDIF

      CALL nchdlerr( nf90_open( cd_filename, nf90_write, i_file_id ) ,&
         & __LINE__,__MYFILE__)

      ! Add longitude, latitude and depth
      ! --------------------------------- 
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'x', i_dim_x_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'y', i_dim_y_id ) ,&
         & __LINE__,__MYFILE__)
      i_dim(1) = i_dim_x_id
      i_dim(2) = i_dim_y_id
      IF ( PRESENT( p_dep ) ) THEN
         i_var_exist = nf90_inq_dimid( i_file_id, 'z', i_dim_z_id ) 
         IF ( i_var_exist .NE. 0 ) THEN
            CALL nchdlerr( nf90_redef( i_file_id ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_def_dim( i_file_id, 'z', i_dimz(1), i_dim_z_id ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_enddef( i_file_id ) ,&
               & __LINE__,__MYFILE__)
         ENDIF
         CALL nchdlerr( nf90_inq_dimid( i_file_id, 'z', i_dim_z_id ) ,&
            & __LINE__,__MYFILE__)
         i_dim(3) = i_dim_z_id
      ENDIF

      i_var_exist = nf90_inq_varid( i_file_id, "longitude", i_var_id )
      CALL nchdlerr( nf90_redef( i_file_id ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_var_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_def_var( i_file_id, "longitude", nf90_real, (/i_dim(1),i_dim(2)/), i_var_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'data_type', 'float' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'standard_name', 'longitude' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'long_name', 'longitude' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'units', 'degrees_east' ),__LINE__,__MYFILE__ )
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'axis', 'Y' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'positive', 'east' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, '_FillValue', 9.e+19 ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', z_minx ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', z_maxx ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_enddef( i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, p_lon) ,&
         & __LINE__,__MYFILE__)

      i_var_exist = nf90_inq_varid( i_file_id, "latitude", i_var_id )
      CALL nchdlerr( nf90_redef( i_file_id ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_var_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_def_var( i_file_id, "latitude", nf90_real, (/i_dim(1),i_dim(2)/), i_var_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'data_type', 'float' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'standard_name', 'latitude' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'long_name', 'latitude' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'units', 'degrees_north' ),__LINE__,__MYFILE__ )
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'axis', 'X' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'positive', 'north' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, '_FillValue', 9.e+19 ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', z_miny ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', z_maxy ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_enddef( i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, p_lat),__LINE__,__MYFILE__ )

      IF ( PRESENT( p_dep ) ) THEN
         i_var_exist = nf90_inq_varid( i_file_id, "depth", i_var_id )
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         IF ( i_var_exist .NE. 0 ) THEN
            CALL nchdlerr( nf90_def_var( i_file_id, "depth", nf90_real, i_dim(3), i_var_id ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'data_type', 'float' ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'standard_name', 'depth' ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'long_name', 'depth below the surface') ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'units', 'm' ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'unit_long', 'meter' ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'axis', 'Z' ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'positive', 'down' ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, '_FillValue', 9.e+19 ),__LINE__,__MYFILE__ )
         ENDIF
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', z_minz ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', z_maxz ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, p_dep) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

   END SUBROUTINE put_coord_r
 
   SUBROUTINE put_coord_d ( cd_filename, dd_lon, dd_lat, dd_dep, cd_descr )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE put_coord_d  ***
      !!
      !! ** Purpose :  write coordinates as real(kind=dp) in a netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - get variable informations
      !!             - (re)define variable
      !!             - put variable in netcdf file
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)                        :: &
         cd_filename                                            ! filename
      REAL(KIND=dp), DIMENSION(:,:), INTENT(in)         :: &
         dd_lon, dd_lat                                         ! longitude and latitude
      REAL(KIND=dp), DIMENSION(:), OPTIONAL, INTENT(in) :: &
         dd_dep                                                 ! depth
      CHARACTER(len=80), DIMENSION(6), OPTIONAL, INTENT(in)   :: &
         cd_descr                                               ! description of file contents

      !! * local declarations
      LOGICAL               :: llexist                  ! test
      INTEGER               ::                    &
         i_file_id, i_var_id,                     &     ! file and variable identifiers
         i_dim_x_id, i_dim_y_id, i_dim_z_id,      &     ! dimension identifiers
         i_var_exist                                    ! test
      INTEGER, DIMENSION(2) :: i_dimxy                  ! dimensions of longitude and latitude
      INTEGER, DIMENSION(1) :: i_dimz                   ! dimension of depth
      INTEGER, DIMENSION(3) :: i_dim                    ! dimensions for netcdf file
      REAL(KIND=dp)      ::                     &
         dl_minx, dl_maxx, dl_miny, dl_maxy,       &
         dl_minz, dl_maxz,                         &    ! minima and maxima
         dl_missing                                     ! missing value
      !!----------------------------------------------------------------------

      ! Read dimensions and compute min and max
      ! ---------------------------------------
      i_dimxy = SHAPE( dd_lon ) 
      dl_minx = MINVAL( dd_lon )
      dl_maxx = MAXVAL( dd_lon )
      dl_miny = MINVAL( dd_lat )
      dl_maxy = MAXVAL( dd_lat )

      IF ( PRESENT( dd_dep ) ) THEN
         i_dimz = SHAPE( dd_dep )
         dl_minz = MINVAL(dd_dep)
         dl_maxz = MAXVAL(dd_dep)
      ENDIF

      ! Open or create netcdf file
      ! --------------------------
      INQUIRE( FILE=TRIM(cd_filename), EXIST=llexist )
      IF ( .NOT. llexist ) THEN
         IF ( PRESENT( dd_dep ) ) THEN
            IF ( PRESENT( cd_descr ) ) THEN
               CALL create_map ( cd_filename, i_dimxy, i_dimz, cd_descr=cd_descr )
            ELSE
               CALL create_map ( cd_filename, i_dimxy, i_dimz )
            ENDIF
         ELSE
            IF ( PRESENT( cd_descr ) ) THEN
               CALL create_map ( cd_filename, i_dimxy, cd_descr=cd_descr )
            ELSE
               CALL create_map ( cd_filename, i_dimxy )
            ENDIF
         ENDIF
      ENDIF

      CALL nchdlerr( nf90_open( cd_filename, nf90_write, i_file_id ) ,&
         & __LINE__,__MYFILE__)

      ! Add longitude, latitude and depth
      ! --------------------------------- 
      dl_missing = 9.e+19_8
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'x', i_dim_x_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'y', i_dim_y_id ) ,&
         & __LINE__,__MYFILE__)
      i_dim(1) = i_dim_x_id
      i_dim(2) = i_dim_y_id
      IF ( PRESENT( dd_dep ) ) THEN
         i_var_exist = nf90_inq_dimid( i_file_id, 'z', i_dim_z_id )
         IF ( i_var_exist .NE. 0 ) THEN
            CALL nchdlerr( nf90_redef( i_file_id ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_def_dim( i_file_id, 'z', i_dimz(1), i_dim_z_id ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_enddef( i_file_id ) ,&
               & __LINE__,__MYFILE__)
         ENDIF
         i_dim(3) = i_dim_z_id
      ENDIF

      i_var_exist = nf90_inq_varid( i_file_id, "longitude", i_var_id )
      CALL nchdlerr( nf90_redef( i_file_id ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_var_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_def_var( i_file_id, "longitude", nf90_double, (/i_dim(1),i_dim(2)/), i_var_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'data_type', 'float' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'standard_name', 'longitude' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'long_name', 'longitude' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'units', 'degrees_east' ),__LINE__,__MYFILE__ )
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'axis', 'Y' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'positive', 'east' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, '_FillValue', dl_missing ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', dl_minx ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', dl_maxx ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_enddef( i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, dd_lon) ,&
         & __LINE__,__MYFILE__)

      i_var_exist = nf90_inq_varid( i_file_id, "latitude", i_var_id )
      CALL nchdlerr( nf90_redef( i_file_id ) ,&
         & __LINE__,__MYFILE__)
      IF ( i_var_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_def_var( i_file_id, "latitude", nf90_double, (/i_dim(1),i_dim(2)/), i_var_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'data_type', 'float' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'standard_name', 'latitude' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'long_name', 'latitude' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'units', 'degrees_north' ),__LINE__,__MYFILE__ )
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'axis', 'X' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'positive', 'north' ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, '_FillValue', dl_missing ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', dl_miny ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', dl_maxy ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_enddef( i_file_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, dd_lat),__LINE__,__MYFILE__ )

      IF ( PRESENT( dd_dep ) ) THEN
         i_var_exist = nf90_inq_varid( i_file_id, "depth", i_var_id )
         CALL nchdlerr( nf90_redef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         IF ( i_var_exist .NE. 0 ) THEN
            CALL nchdlerr( nf90_def_var( i_file_id, "depth", nf90_double, i_dim(3), i_var_id ),__LINE__,__MYFILE__ )
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'data_type', 'float' ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'standard_name', 'depth' ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'long_name', 'depth below the surface'),__LINE__,__MYFILE__ )
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'units', 'm' ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'unit_long', 'meter' ),__LINE__,__MYFILE__ )
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'axis', 'Z' ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'positive', 'down' ) ,&
               & __LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, '_FillValue', dl_missing ) ,&
               & __LINE__,__MYFILE__)
         ENDIF
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', dl_minz ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', dl_maxz ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, dd_dep) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)
!
   END SUBROUTINE put_coord_d

   !!======================================================================

   SUBROUTINE create_map ( cd_filename, k_dimxy, k_dimz, cd_descr )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE create_map  ***
      !!
      !! ** Purpose :  create netcdf file
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - define dimension of the netcdf file
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(in)                 :: &
         cd_filename                                            ! filename
      INTEGER, DIMENSION(:), INTENT(in)             :: k_dimxy  ! dimensions of longitude and latitude
      INTEGER, DIMENSION(1), OPTIONAL, INTENT(in)   :: k_dimz   ! dimension of depth
      CHARACTER(len=80), DIMENSION(6), OPTIONAL, INTENT(in)   :: &
         cd_descr                                               ! description of file contents

      !! * local declarations
      INTEGER               ::                    &
         i_file_id, i_dim_id                            ! file and dimension identifiers
      INTEGER, DIMENSION(1) :: i_dim                    ! dimension of k_dimxy
      !!----------------------------------------------------------------------

      i_dim = SHAPE(k_dimxy)

      ! Create file
      ! -----------
      CALL nchdlerr( nf90_create( cd_filename, nf90_clobber, i_file_id ) ,&
         & __LINE__,__MYFILE__)

      ! Define dimensions
      ! -----------------
      CALL nchdlerr( nf90_def_dim( i_file_id, 'x', k_dimxy(1), i_dim_id ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_def_dim( i_file_id, 'y', k_dimxy(2), i_dim_id ) ,&
         & __LINE__,__MYFILE__)
      IF ( PRESENT(k_dimz) ) THEN
         CALL nchdlerr( nf90_def_dim( i_file_id, 'z', k_dimz(1), i_dim_id ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      IF ( i_dim(1) .EQ. 3 ) THEN
         CALL nchdlerr( nf90_def_dim( i_file_id, 'z', k_dimxy(3), i_dim_id ) ,&
            & __LINE__,__MYFILE__)
      ENDIF
      CALL nchdlerr( nf90_def_dim( i_file_id, 'time', nf90_unlimited, i_dim_id ) ,&
         & __LINE__,__MYFILE__)

      IF ( PRESENT( cd_descr ) ) THEN
         CALL nchdlerr( nf90_put_att( i_file_id, nf90_global, "title", cd_descr(1) ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, nf90_global, "institution", cd_descr(2) ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, nf90_global, "source", cd_descr(3) ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, nf90_global, "history", cd_descr(4) ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, nf90_global, "references", cd_descr(5) ) ,&
            & __LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, nf90_global, "comment", cd_descr(6) ) ,&
            & __LINE__,__MYFILE__)
      ENDIF

      CALL nchdlerr( nf90_close( i_file_id ) ,&
         & __LINE__,__MYFILE__)

   END SUBROUTINE create_map

   !!======================================================================

   SUBROUTINE put_att( k_file_id, k_var_id, k_code )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE put_att  ***
      !!
      !! ** Purpose :  write CF attributes
      !!
      !! ** Method  : use netcdf fortran library 
      !!       
      !! ** Action : - write CF attributes
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      INTEGER, INTENT(in)      :: k_file_id, k_var_id,  &       ! file and variable identifiers
                                  k_code                        ! CF identifier

      !! * local declarations
      CHARACTER(len=80)        ::                       &
         cl_name, cl_longname,                          &       ! names of variable
         cl_unit, cl_longunit                                   ! units of variable
      !!----------------------------------------------------------------------

      ! Get variable attributes and write them
      ! --------------------------------------
      CALL get_var_info( k_code, cl_name, cl_longname, cl_unit, cl_longunit )

      CALL nchdlerr( nf90_put_att( k_file_id, k_var_id, 'standard_name', TRIM(cl_name) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_att( k_file_id, k_var_id, 'long_name', TRIM(cl_longname) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_att( k_file_id, k_var_id, 'units', TRIM(cl_unit) ) ,&
         & __LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_att( k_file_id, k_var_id, 'unit_long', TRIM(cl_longunit) ),__LINE__,__MYFILE__ )

   END SUBROUTINE put_att

   !!======================================================================

   SUBROUTINE get_var_info( k_code, cd_name, cd_longname, cd_unit, cd_longunit )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE put_att  ***
      !!
      !! ** Purpose :  get CF attributes
      !!
      !! ** Method  : outofcore database
      !!       
      !! ** Action : - find iCF attributes
      !!
      !! Reference :
      !!      
      !! History :
      !!    06-05  (N. Daget)  original code
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Library used

      !! * arguments
      CHARACTER(len=80), INTENT(out)           :: cd_name       ! name of variable
      CHARACTER(len=80), OPTIONAL, INTENT(out) ::       &
         cd_longname, cd_unit, cd_longunit                      ! name and units of variable
      INTEGER, INTENT(in)                      :: k_code        ! CF identifier

      !! * local declarations
      !!----------------------------------------------------------------------

      ! Get CF attributes
      ! -----------------
      SELECT CASE ( k_code )
         CASE (129)
            cd_name="sea_water_potential_temperature"
            IF ( PRESENT(cd_longname) ) cd_longname="Potential temperature ref to surface K"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_Celcius"
         CASE (130)
            cd_name="sea_water_salinity"
            IF ( PRESENT(cd_longname) ) cd_longname="sea water salinity"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (131)
            cd_name="sea_water_x_velocity"
            IF ( PRESENT(cd_longname) ) cd_longname="U zonal current m/s"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (132)
            cd_name="sea_water_y_velocity"
            IF ( PRESENT(cd_longname) ) cd_longname="V meridional current m/s"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (133)
            cd_name="upward_sea_water_velocity"
            IF ( PRESENT(cd_longname) ) cd_longname="W vertical current m/s"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (134)
            cd_name="mst"
            IF ( PRESENT(cd_longname) ) cd_longname="Modulus of strain rate tensor"
            IF ( PRESENT(cd_unit) ) cd_unit="s-1"
            IF ( PRESENT(cd_longunit) ) cd_longunit="per_second"
         CASE (135)
            cd_name="vvs"
            IF ( PRESENT(cd_longname) ) cd_longname="Vertical viscosity"
            IF ( PRESENT(cd_unit) ) cd_unit="m^2/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="square_meter_per_second"
         CASE (136)
            cd_name="vdf"
            IF ( PRESENT(cd_longname) ) cd_longname="Vertical diffusivity"
            IF ( PRESENT(cd_unit) ) cd_unit="m^2/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="square_meter_per_second"
         CASE (137)
            cd_name="depth"
            IF ( PRESENT(cd_longname) ) cd_longname="depth"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (138)
            cd_name="sea_water_sigma_theta"
            IF ( PRESENT(cd_longname) ) cd_longname="Sigma theta"
            IF ( PRESENT(cd_unit) ) cd_unit="kgm3"
            IF ( PRESENT(cd_longunit) ) cd_longunit="kilogramme cubicmeter"
         CASE (139)
            cd_name="richardson_number"
            IF ( PRESENT(cd_longname) ) cd_longname="Richardson number"
            IF ( PRESENT(cd_unit) ) cd_unit=""
            IF ( PRESENT(cd_longunit) ) cd_longunit=""
         CASE (140)
            cd_name="UV"
            IF ( PRESENT(cd_longname) ) cd_longname="UV product"
            IF ( PRESENT(cd_unit) ) cd_unit="m**2 s**-2"
            IF ( PRESENT(cd_longunit) ) cd_longunit="m**2 s**-2"
         CASE (141)
            cd_name="UT"
            IF ( PRESENT(cd_longname) ) cd_longname="UT product"
            IF ( PRESENT(cd_unit) ) cd_unit="m s**-1 degC"
            IF ( PRESENT(cd_longunit) ) cd_longunit="m s**-1 degC"
         CASE (142)
            cd_name="VT"
            IF ( PRESENT(cd_longname) ) cd_longname="VT product"
            IF ( PRESENT(cd_unit) ) cd_unit="m s**-1 degC"
            IF ( PRESENT(cd_longunit) ) cd_longunit="m s**-1 degC"
         CASE (143)
            cd_name="UU"
            IF ( PRESENT(cd_longname) ) cd_longname="UU product"
            IF ( PRESENT(cd_unit) ) cd_unit="m**2 s**-2"
            IF ( PRESENT(cd_longunit) ) cd_longunit="m**2 s**-2"
         CASE (144)
            cd_name="VV"
            IF ( PRESENT(cd_longname) ) cd_longname="VV product"
            IF ( PRESENT(cd_unit) ) cd_unit="m**2 s**-2"
            IF ( PRESENT(cd_longunit) ) cd_longunit="m**2 s**-2"
         CASE (145)
            cd_name="sea_surface_height_above_geoid"
            IF ( PRESENT(cd_longname) ) cd_longname="sea_surface_height_above_geoid"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (146)
            cd_name="sea_floor_topography"
            IF ( PRESENT(cd_longname) ) cd_longname="Sea floor topography"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (147)
            cd_name="ocean_barotropic_streamfunction"
            IF ( PRESENT(cd_longname) ) cd_longname="Barotropic Stream Function"
            IF ( PRESENT(cd_unit) ) cd_unit="m3/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="cubic_meter_per_second"
         CASE (148)
            cd_name="ocean_mixed_layer_thickness"
            IF ( PRESENT(cd_longname) ) cd_longname="depth of the ocean mixed layer from the surface"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (149)
            cd_name="sea_water_bottom_pressure"
            IF ( PRESENT(cd_longname) ) cd_longname="sea_water_bottom_pressure"
            IF ( PRESENT(cd_unit) ) cd_unit="Pa"
            IF ( PRESENT(cd_longunit) ) cd_longunit="Pascal"
         CASE (153)
            cd_name="surface_downward_eastward_stress"
            IF ( PRESENT(cd_longname) ) cd_longname="U-Stress"
            IF ( PRESENT(cd_unit) ) cd_unit="Pa"
            IF ( PRESENT(cd_longunit) ) cd_longunit="Pascal"
         CASE (154)
            cd_name="surface_downward_northward_stress"
            IF ( PRESENT(cd_longname) ) cd_longname="V-Stress"
            IF ( PRESENT(cd_unit) ) cd_unit="Pa"
            IF ( PRESENT(cd_longunit) ) cd_longunit="Pascal"
         CASE (156)
            cd_name="turbulent_kinetic_energy_input"
            IF ( PRESENT(cd_longname) ) cd_longname="turbulent kinetic energy input"
            IF ( PRESENT(cd_unit) ) cd_unit="W.m^2"
            IF ( PRESENT(cd_longunit) ) cd_longunit="watt_per_square_meter"
         CASE (157)
            cd_name="absorbed_solar_radiation"
            IF ( PRESENT(cd_longname) ) cd_longname="Absorbed solar radiation"
            IF ( PRESENT(cd_unit) ) cd_unit="W.m^2"
            IF ( PRESENT(cd_longunit) ) cd_longunit="watt_per_square_meter"
         CASE (158)
            cd_name="surface_downward_water_flux"
            IF ( PRESENT(cd_longname) ) cd_longname="Precipitation minus Evaporation"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (159)
            cd_name="sea_surface_temperature"
            IF ( PRESENT(cd_longname) ) cd_longname="specified sea surface temperature"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_Celcius"
         CASE (160)
            cd_name="surface_downward_heat_flux_in_sea_water"
            IF ( PRESENT(cd_longname) ) cd_longname="Specified Surface Heat flux"
            IF ( PRESENT(cd_unit) ) cd_unit="W.m^2"
            IF ( PRESENT(cd_longunit) ) cd_longunit="watt_per_square_meter"
         CASE (161)
            cd_name="diagnosed_sst_error"
            IF ( PRESENT(cd_longname) ) cd_longname="diagnosed sea surface temperature error"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius"
         CASE (162)
            cd_name="surface_downward_heat_flux_in_sea_water"
            IF ( PRESENT(cd_longname) ) cd_longname="Heat flux Correction"
            IF ( PRESENT(cd_unit) ) cd_unit="W.m^2"
            IF ( PRESENT(cd_longunit) ) cd_longunit="watt_per_square_meter"
         CASE (163)
            cd_name="d20_isotherm_depth"
            IF ( PRESENT(cd_longname) ) cd_longname="depth of the D20 isotherm"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (164)
            cd_name="ocean_integral_of_sea_water_temperature_wrt_depth"
            IF ( PRESENT(cd_longname) ) cd_longname="averaged temperature over the fisrt 300m"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius"
         CASE (167)
            cd_name="barotropic_eastward_sea_water_velocity"
            IF ( PRESENT(cd_longname) ) cd_longname="integral of U dz"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (168)
            cd_name="barotropic_northward_sea_water_velocity"
            IF ( PRESENT(cd_longname) ) cd_longname="integral of V dz"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (169)
            cd_name="product_of_eastward_sea_water_velocity_and_temperature"
            IF ( PRESENT(cd_longname) ) cd_longname="integral of Tudz"
            IF ( PRESENT(cd_unit) ) cd_unit="Km/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="Kelvin_meter_per_second"
         CASE (170)
            cd_name="product_of_northward_sea_water_velocity_and_temperature"
            IF ( PRESENT(cd_longname) ) cd_longname="integral of Tvdz"
            IF ( PRESENT(cd_unit) ) cd_unit="Km/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="Kelvin_meter_per_second"
         CASE (171)
            cd_name="velocity_maximum"
            IF ( PRESENT(cd_longname) ) cd_longname="velocity maximum"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (172)
            cd_name="depth_of_maximum_velocity"
            IF ( PRESENT(cd_longname) ) cd_longname="depth of maximum velocity"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (173)
            cd_name="salinity_maximum"
            IF ( PRESENT(cd_longname) ) cd_longname="salinity maximum"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (174)
            cd_name="depth_of_maximum_salinity"
            IF ( PRESENT(cd_longname) ) cd_longname="depth of maximum salinity"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (175)
            cd_name="ocean_integral_of_sea_water_salinity_wrt_depth"
            IF ( PRESENT(cd_longname) ) cd_longname="averaged salinity over the fisrt 300m"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (179)
            cd_name="potential_temperature_analysis_error"
            IF ( PRESENT(cd_longname) ) cd_longname="Potential temperature analysis error"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius"
         CASE (180)
            cd_name="background_potential_temperature"
            IF ( PRESENT(cd_longname) ) cd_longname="background potential temperature"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius"
         CASE (181)
            cd_name="analysed_potential_temperature"
            IF ( PRESENT(cd_longname) ) cd_longname="Analysed potential temperature"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius"
         CASE (182)
            cd_name="potential_temperature_background_error"
            IF ( PRESENT(cd_longname) ) cd_longname="Potential temperature background error"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius"
         CASE (183)
            cd_name="analysed_salinity_from_soft"
            IF ( PRESENT(cd_longname) ) cd_longname="analysed salinity from S(T) balance operator"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (184)
            cd_name="sea_water_salinity_increment"
            IF ( PRESENT(cd_longname) ) cd_longname="salinity increment"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (185)
            cd_name="temperature_bias"
            IF ( PRESENT(cd_longname) ) cd_longname="bias in temperature"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius"
         CASE (186)
            cd_name="salinity_bias"
            IF ( PRESENT(cd_longname) ) cd_longname="bias in salinity"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (187)
            cd_name="eastward_sea_water_velocity_increment_from_balance"
            IF ( PRESENT(cd_longname) ) cd_longname="eastward sea water velocity increment from balance operator"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (188)
            cd_name="northward_sea_water_velocity_increment_from_balance"
            IF ( PRESENT(cd_longname) ) cd_longname="northward sea water velocity increment from balance operator"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (190)
            cd_name="sea_water_salinity_increment_from_soft"
            IF ( PRESENT(cd_longname) ) cd_longname="sea water salinity increment from soft"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (191)
            cd_name="sea_water_salinity_analysis_error"
            IF ( PRESENT(cd_longname) ) cd_longname="sea water salinity analysis error"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (192)
            cd_name="sea_water_background_salinity"
            IF ( PRESENT(cd_longname) ) cd_longname="sea water background salinity"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (193)
            cd_name="sea_water_analysed_salinity"
            IF ( PRESENT(cd_longname) ) cd_longname="sea water analysed salinity"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (194)
            cd_name="sea_water_salinity_background_error"
            IF ( PRESENT(cd_longname) ) cd_longname="sea water salinity background error"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (500)
            cd_name="bcksta_bal_S"
            IF ( PRESENT(cd_longname) ) cd_longname="balanced part of S"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (501)
            cd_name="bcksta_unbal_S"
            IF ( PRESENT(cd_longname) ) cd_longname="unbalanced part of S"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (502)
            cd_name="bcksta_bal_u"
            IF ( PRESENT(cd_longname) ) cd_longname="balanced part of u"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (503)
            cd_name="bcksta_unbal_u"
            IF ( PRESENT(cd_longname) ) cd_longname="unbalanced part of u"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (504)
            cd_name="bcksta_bal_v"
            IF ( PRESENT(cd_longname) ) cd_longname="balanced part of v"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (505)
            cd_name="bcksta_unbal_v"
            IF ( PRESENT(cd_longname) ) cd_longname="unbalanced part of v"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (506)
            cd_name="bcksta_bal_e"
            IF ( PRESENT(cd_longname) ) cd_longname="balanced part of SSH"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (507)
            cd_name="bcksta_unbal_e"
            IF ( PRESENT(cd_longname) ) cd_longname="unbalanced part of SSH"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (510)
            cd_name="increm_tot_T"
            IF ( PRESENT(cd_longname) ) cd_longname="increment for total part of T"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius"
         CASE (511)
            cd_name="increm_tot_S"
            IF ( PRESENT(cd_longname) ) cd_longname="increment for total part of S"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (512)
            cd_name="increm_bal_S"
            IF ( PRESENT(cd_longname) ) cd_longname="increment for balanced part of S"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (513)
            cd_name="increm_unbal_S"
            IF ( PRESENT(cd_longname) ) cd_longname="increment for unbalanced part of S"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (514)
            cd_name="increm_tot_u"
            IF ( PRESENT(cd_longname) ) cd_longname="increment for total part of U"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (515)
            cd_name="increm_bal_u"
            IF ( PRESENT(cd_longname) ) cd_longname="increment for balanced part of U"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (516)
            cd_name="increm_unbal_u"
            IF ( PRESENT(cd_longname) ) cd_longname="increment for unbalanced part of U"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (517)
            cd_name="increm_tot_v"
            IF ( PRESENT(cd_longname) ) cd_longname="increment for total part of V"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (518)
            cd_name="increm_bal_v"
            IF ( PRESENT(cd_longname) ) cd_longname="increment for balanced part of V"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (519)
            cd_name="increm_unbal_v"
            IF ( PRESENT(cd_longname) ) cd_longname="increment for unbalanced part of V"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (520)
            cd_name="increm_tot_e"
            IF ( PRESENT(cd_longname) ) cd_longname="increment for total part of SSH"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (521)
            cd_name="increm_bal_e"
            IF ( PRESENT(cd_longname) ) cd_longname="increment for balanced part of SSH"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (522)
            cd_name="increm_unbal_e"
            IF ( PRESENT(cd_longname) ) cd_longname="increment for unbalanced part of SSH"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (530)
            cd_name="pbudct"
            IF ( PRESENT(cd_longname) ) cd_longname="distance between coastline and model u-points"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (531)
            cd_name="pbvdct"
            IF ( PRESENT(cd_longname) ) cd_longname="distance between coastline and model v-points"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (532)
            cd_name="pbtdct"
            IF ( PRESENT(cd_longname) ) cd_longname="distance between coastline and model t-points"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (533)
            cd_name="pbfdct"
            IF ( PRESENT(cd_longname) ) cd_longname="distance between coastline and model f-points"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (540)
            cd_name="bckenw_wetjc"
            IF ( PRESENT(cd_longname) ) cd_longname="energy T weighting coefficient for Jc term"
            IF ( PRESENT(cd_unit) ) cd_unit="m^2/s^2/C^2"
            IF ( PRESENT(cd_longunit) ) cd_longunit="square_meter_per_square_second_per_square_degre_celcius"
         CASE (541)
            cd_name="bckenw_wesjc"
            IF ( PRESENT(cd_longname) ) cd_longname="energy S weighting coefficient for Jc term"
            IF ( PRESENT(cd_unit) ) cd_unit="m^2/s^2/PSU^2"
            IF ( PRESENT(cd_longunit) ) cd_longunit="square_meter_per_square_second_per_square_practical_salinity_scale"
         CASE (550)
            cd_name="bckesd_ana_t"
            IF ( PRESENT(cd_longname) ) cd_longname="standard deviations for total part of T"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius"
         CASE (551)
            cd_name="bckesd_ana_s"
            IF ( PRESENT(cd_longname) ) cd_longname="standard deviations for unbalanced part of S"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (552)
            cd_name="bckesd_ana_u"
            IF ( PRESENT(cd_longname) ) cd_longname="standard deviations for unbalanced part of u"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (553)
            cd_name="bckesd_ana_v"
            IF ( PRESENT(cd_longname) ) cd_longname="standard deviations for unbalanced part of V"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (554)
            cd_name="bckesd_ana_e"
            IF ( PRESENT(cd_longname) ) cd_longname="standard deviations for unbalanced part of SSH"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (560)
            cd_name="bckesd_mod_t"
            IF ( PRESENT(cd_longname) ) cd_longname="background error standard deviations for temperature"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius"
         CASE (561)
            cd_name="bckesd_mod_s"
            IF ( PRESENT(cd_longname) ) cd_longname="background error standard deviations for salinity"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (562)
            cd_name="bckesd_mod_u"
            IF ( PRESENT(cd_longname) ) cd_longname="background error standard deviations for u-velocity"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (563)
            cd_name="bckesd_mod_v"
            IF ( PRESENT(cd_longname) ) cd_longname="background error standard deviations for v-velocity"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (564)
            cd_name="bckesd_mod_e"
            IF ( PRESENT(cd_longname) ) cd_longname="background error standard deviations for SSH"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (565)
            cd_name="bckesd_modf_t"
            IF ( PRESENT(cd_longname) ) cd_longname="background error standard deviations for temperature at final time"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius"
         CASE (566)
            cd_name="bckesd_modf_s"
            IF ( PRESENT(cd_longname) ) cd_longname="background error standard deviations for salinity at final time"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (567)
            cd_name="bckesd_modf_u"
            IF ( PRESENT(cd_longname) ) cd_longname="background error standard deviations for u-velocity at final time"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (568)
            cd_name="bckesd_modf_v"
            IF ( PRESENT(cd_longname) ) cd_longname="background error standard deviations for v-velocity at final time"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (569)
            cd_name="bckesd_modf_e"
            IF ( PRESENT(cd_longname) ) cd_longname="background error standard deviations for SSH at final time"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (570)
            cd_name="bcksal_dSdT"
            IF ( PRESENT(cd_longname) ) cd_longname="dS/dT coefficient for the salinity balance"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU/C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale_per_degree_celcius"
         CASE (571)
            cd_name="bcksal_dTdz"
            IF ( PRESENT(cd_longname) ) cd_longname="dT/dz for the background"
            IF ( PRESENT(cd_unit) ) cd_unit="C/m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius_per_meter"
         CASE (572)
            cd_name="bcksal_dSdz"
            IF ( PRESENT(cd_longname) ) cd_longname="dS/dz for the background"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU/m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale_per_meter"
         CASE (575)
            cd_name="bckssh_alpha"
            IF ( PRESENT(cd_longname) ) cd_longname="Thermal expansion coefficient for the SSH balance"
            IF ( PRESENT(cd_unit) ) cd_unit=""
            IF ( PRESENT(cd_longunit) ) cd_longunit=""
         CASE (576)
            cd_name="bckssh_beta"
            IF ( PRESENT(cd_longname) ) cd_longname="Saline expansion coefficient for the SSH balance"
            IF ( PRESENT(cd_unit) ) cd_unit=""
            IF ( PRESENT(cd_longunit) ) cd_longunit=""
         CASE (580)
            cd_name="filnot"
            IF ( PRESENT(cd_longname) ) cd_longname="filter normalization factor at T-points"
            IF ( PRESENT(cd_unit) ) cd_unit="m^(3/2)"
            IF ( PRESENT(cd_longunit) ) cd_longunit="square_root_of_cube_meter"
         CASE (581)
            cd_name="filnou"
            IF ( PRESENT(cd_longname) ) cd_longname="filter normalization factor at u-points"
            IF ( PRESENT(cd_unit) ) cd_unit="m^(3/2)"
            IF ( PRESENT(cd_longunit) ) cd_longunit="square_root_of_cube_meter"
         CASE (582)
            cd_name="filnov"
            IF ( PRESENT(cd_longname) ) cd_longname="filter normalization factor at v-points"
            IF ( PRESENT(cd_unit) ) cd_unit="m^(3/2)"
            IF ( PRESENT(cd_longunit) ) cd_longunit="square_root_of_cube_meter"
         CASE (583)
            cd_name="filnoe"
            IF ( PRESENT(cd_longname) ) cd_longname="filter normalization factor at eta-points"
            IF ( PRESENT(cd_unit) ) cd_unit="m^(3/2)"
            IF ( PRESENT(cd_longunit) ) cd_longunit="square_root_of_cube_meter"
         CASE (590)
            cd_name="bckint"
            IF ( PRESENT(cd_longname) ) cd_longname="increment to background temperature"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius"
         CASE (591)
            cd_name="bckins"
            IF ( PRESENT(cd_longname) ) cd_longname="increment to background salinity"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity"
         CASE (592)
            cd_name="bckinu"
            IF ( PRESENT(cd_longname) ) cd_longname="increment to background u-velocity"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (593)
            cd_name="bckinv"
            IF ( PRESENT(cd_longname) ) cd_longname="increment to background v-velocity"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (594)
            cd_name="bckineta"
            IF ( PRESENT(cd_longname) ) cd_longname="increment to background SSH"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (610)
            cd_name="tangent_T"
            IF ( PRESENT(cd_longname) ) cd_longname="tangent potential temperature field"
            IF ( PRESENT(cd_unit) ) cd_unit="C"
            IF ( PRESENT(cd_longunit) ) cd_longunit="degree_celcius"
         CASE (611)
            cd_name="tangent_S"
            IF ( PRESENT(cd_longname) ) cd_longname="tangent salinity field"
            IF ( PRESENT(cd_unit) ) cd_unit="PSU"
            IF ( PRESENT(cd_longunit) ) cd_longunit="practical_salinity_scale"
         CASE (612)
            cd_name="tangent_u"
            IF ( PRESENT(cd_longname) ) cd_longname="tangent U zonal current"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (613)
            cd_name="tangent_v"
            IF ( PRESENT(cd_longname) ) cd_longname="tangent V meridional current"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (614)
            cd_name="tangent_w"
            IF ( PRESENT(cd_longname) ) cd_longname="tangent W vertical current"
            IF ( PRESENT(cd_unit) ) cd_unit="m/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter_per_second"
         CASE (615)
            cd_name="tangent_SSH"
            IF ( PRESENT(cd_longname) ) cd_longname="tangent sea surface height above geoid"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (616)
            cd_name="tangent BSF"
            IF ( PRESENT(cd_longname) ) cd_longname="tangent barotropic stream function"
            IF ( PRESENT(cd_unit) ) cd_unit="m3/s"
            IF ( PRESENT(cd_longunit) ) cd_longunit="cubic_meter_per_second"
         CASE (620)
            cd_name="d28_isotherm_depth"
            IF ( PRESENT(cd_longname) ) cd_longname="depth of the D28 isotherm"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (621)
            cd_name="ocean_mixed_layer_thickness_bn2_criterion"
            IF ( PRESENT(cd_longname) ) cd_longname="depth of the ocean mixed layer from the surface with bn2 citerion"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (622)
            cd_name="thermocline_depth"
            IF ( PRESENT(cd_longname) ) cd_longname="depth of the strongest vertical temperature gradient"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (623)
            cd_name="heat_content_of_first_300m"
            IF ( PRESENT(cd_longname) ) cd_longname="heat content of first 300 m"
            IF ( PRESENT(cd_unit) ) cd_unit="J"
            IF ( PRESENT(cd_longunit) ) cd_longunit="joules"
         CASE (624)
            cd_name="turbocline_depth"
            IF ( PRESENT(cd_longname) ) cd_longname="turbocline depth"
            IF ( PRESENT(cd_unit) ) cd_unit="m"
            IF ( PRESENT(cd_longunit) ) cd_longunit="meter"
         CASE (625)
            cd_name="U-energy_transport"
            IF ( PRESENT(cd_longname) ) cd_longname="U-energy transport"
            IF ( PRESENT(cd_unit) ) cd_unit="J/s/m^2"
            IF ( PRESENT(cd_longunit) ) cd_longunit="joules_per_second_per_square_meter"
         CASE (626)
            cd_name="V-energy_transport"
            IF ( PRESENT(cd_longname) ) cd_longname="V-energy transport"
            IF ( PRESENT(cd_unit) ) cd_unit="J/s/m^2"
            IF ( PRESENT(cd_longunit) ) cd_longunit="joules_per_second_per_square_meter"
         CASE default
            cd_name="varname"
            IF ( PRESENT(cd_longname) ) cd_longname="long_varname"
            IF ( PRESENT(cd_unit) ) cd_unit="varunit"
            IF ( PRESENT(cd_longunit) ) cd_longunit="long_varunit"
      END SELECT

   END SUBROUTINE get_var_info

END MODULE ioncdf
