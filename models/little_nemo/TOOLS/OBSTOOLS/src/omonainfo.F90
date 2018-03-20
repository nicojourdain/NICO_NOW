#define __MYFILE__ 'omonainfo.F90'
MODULE omonainfo
   
   ! Specific utilities for omona files
   ! 
   ! History
   ! Original: Magdalena A. Balmaseda
   !           2010: Split as to remove "scale factor information", 
   !           which is now in auxgrid
   !
   
   
   USE netcdf
   USE nctools
   USE ioncdf
   USE coords
!   USE orcagrid

   CHARACTER(len=4)    :: cl_expnam  
   CHARACTER(len=8)    :: cl_date
   CHARACTER(len=80)   :: cl_filename_out
   CHARACTER(len=80)   :: cl_filename2_out 
   CHARACTER(len=3)    :: cl_code
   CHARACTER(len=1)    :: cl_grid
   CHARACTER(len=80)   :: cl_var,cl_var_out
   CHARACTER(len=80)   :: cl_varname1, cl_varname2, cl_varunit1, cl_varunit2
   CHARACTER(len=1)    :: sec !o =box average omona
                              !x =zonal section. Integral along x
                              !y =meridional section. Integral along y

   CHARACTER(len=1)    :: grid0='?'
   
   INTEGER ::  i_nb_dims,i_dp
   INTEGER ::  i_fill
   REAL, DIMENSION(:,:),     ALLOCATABLE :: z_var_2d
   REAL, DIMENSION(:,:,:),   ALLOCATABLE :: z_var_3d
   REAL, DIMENSION(:),       ALLOCATABLE :: z_dep2

   PUBLIC  :: variable_att, write_omona_netcdf, &
      &       write_dep_netcdf, create_time_series_netcdf_file

   
   INTERFACE write_omona_netcdf
      MODULE PROCEDURE write_omona_netcdf_2d_r, write_omona_netcdf_3d_r
   END INTERFACE
   
CONTAINS

   !
   !----------------------------------------------------------------------
   !
   ! W R I T E _ O M O N A _ N E T C D F
   !
   !----------------------------------------------------------------------
   !
   SUBROUTINE write_omona_netcdf_2d_r (cl_filename, z_field,i_time, &
      cl_boxes, z_missing,i_fill)
      !
      ! write a 2d array (time_serie) of reals
      !
      IMPLICIT NONE
      !
      CHARACTER(len=80), INTENT(in)                :: cl_filename
      INTEGER, DIMENSION(:), INTENT(in)            :: i_time
      INTEGER, INTENT(inout)                       :: i_fill
      REAL, DIMENSION(:,:), INTENT(in)             :: z_field
      REAL, INTENT(in)                             :: z_missing
      CHARACTER(len=20), DIMENSION(:), INTENT(in)  :: cl_boxes
      
      !
      LOGICAL               :: clbon
      INTEGER               :: i_var_exist, i_pos
      INTEGER               :: i_file_id, i_var_id
      INTEGER               :: i_nb_files,i_z, i_b
      INTEGER               :: i_tim_id,i_box_id
      INTEGER               :: i_time_var
      INTEGER               :: i_file
      INTEGER, DIMENSION(2) :: i_dim, i_tab_start, i_tab_count
      INTEGER               :: i_rest_start, i_rest_end
      INTEGER               :: i_time0, i_time1
      REAL                  :: z_min, z_max
      REAL                  :: z_min_old, z_max_old
      INTEGER, DIMENSION(:), ALLOCATABLE :: i_time_old
      INTEGER, DIMENSION(:), ALLOCATABLE :: i_time_rest
      INTEGER               :: i_t
      REAL, DIMENSION(:,:), ALLOCATABLE :: z_old_2d
      !
      i_tab_start(1) = 1
      i_z=1
      i_b=SIZE(z_field,1)
      i_nb_files = SIZE(z_field,2)
      i_tab_count(1) = i_b
      i_tab_count(2) = i_nb_files
      i_rest_start = 0
      i_rest_end   = 0
      i_time0      = 0
      i_time1      = 0
      
      WRITE(6,*)' Input time dimensions ',i_nb_files
      WRITE(6,*)' file for output       ',cl_filename
      !
      z_min = MINVAL(z_field, mask = z_field .NE. z_missing )
      z_max = MAXVAL(z_field, mask = z_field .NE. z_missing )
      !
      
      INQUIRE( FILE=cl_filename_out, EXIST=clbon )
      WRITE(*,*)' Inquiring clbon ',clbon
      IF ( .NOT. clbon ) THEN
         CALL create_time_series_netcdf_file ( cl_filename, cl_boxes)
      ELSE
         WRITE(*,*)' Time series file already exists'
      ENDIF
      !     
      WRITE(*,*)' opening file ',cl_filename
      CALL nchdlerr( nf90_open( cl_filename, nf90_write, i_file_id ) ,__LINE__,__MYFILE__)
      !
      
      WRITE(*,*)' Inquiring var ',cl_var_out
      i_var_exist = nf90_inq_varid( i_file_id, cl_var_out, i_var_id ) 
      WRITE(*,*)' i_var_exist ',i_var_exist, i_var_id
      IF ( i_var_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_inq_dimid( i_file_id, 'time', i_tim_id ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_inq_dimid( i_file_id, 'box', i_box_id ) ,__LINE__,__MYFILE__)
         i_dim(1) = i_box_id
         i_dim(2) = i_tim_id
         !         write(*,*)' i_dim ',i_dim
         !
         CALL nchdlerr( nf90_redef(i_file_id) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_def_var(i_file_id, cl_var_out, nf90_real, i_dim, i_var_id ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'standard_name', cl_varname1 ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'long_name', cl_varname2 ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'units',cl_varunit1 ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'unit_long',cl_varunit2 ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'missing_value', z_missing ) ,__LINE__,__MYFILE__)
         WRITE(*,*)'nf_put valid_min ',z_min
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', z_min ) ,__LINE__,__MYFILE__)
         WRITE(*,*)'nf_put valid_max ',z_max
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', z_max ) ,__LINE__,__MYFILE__)
         
         CALL nchdlerr( nf90_enddef( i_file_id ) ,__LINE__,__MYFILE__)
         
         CALL nchdlerr( nf90_inq_varid( i_file_id, 'time', i_time_var ) ,__LINE__,__MYFILE__)
         i_pos = 1 
         i_tab_start(2)=1
      ELSE
         CALL nchdlerr( nf90_get_att( i_file_id, i_var_id, 'valid_min', z_min_old ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_get_att( i_file_id, i_var_id, 'valid_max', z_max_old ) ,__LINE__,__MYFILE__)
         z_min = MIN( z_min, z_min_old )
         z_max = MAX( z_max, z_max_old )
         CALL nchdlerr( nf90_redef( i_file_id ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', z_min ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', z_max ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,__LINE__,__MYFILE__)
         
         CALL nchdlerr( nf90_inq_dimid( i_file_id, 'time', i_tim_id ) ,__LINE__,__MYFILE__)
 
         CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_tim_id,len=i_pos ) ,__LINE__,__MYFILE__)
         ALLOCATE(i_time_old(i_pos))
         
         CALL nchdlerr( nf90_inq_varid( i_file_id, 'time', i_time_var ) ,__LINE__,__MYFILE__)
         
         CALL nchdlerr( nf90_get_var( i_file_id, i_time_var, i_time_old ) ,__LINE__,__MYFILE__)
         i_tab_start(2)=i_pos+1
         DO i_t=i_pos,1,-1
            IF(i_time_old(i_t).GE.i_time(1)) THEN
               i_tab_start(2)=i_t
            ELSE
               EXIT
            ENDIF
         ENDDO
         WRITE(*,*)' i_tab_start(2)',i_tab_start(2)

         IF (I_FILL == 1 ) THEN
           DO i_t=1,i_tab_start(2),1
             IF(i_time_old(i_t).LT.i_time(1)) THEN
                i_time0=i_t+1
             ELSE
                EXIT
             ENDIF
          ENDDO
          WRITE(*,*)' i_tab_start(2), i_time0',i_tab_start(2),i_time0
          i_time0=min(i_time0,i_tab_start(2))
          i_tab_start(2)=i_time0
          i_time1=i_tab_start(2)+i_tab_count(2)-1
          WRITE(*,*)' Starting at time ',i_time_old(i_tab_start(2)-1),i_time(1)
          i_rest_start=i_time1+1
          i_rest_end = i_pos
          IF ( i_rest_end .gt. i_rest_start ) THEN
            ALLOCATE( z_old_2d(i_b, i_pos))
            CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, z_old_2d ) ,__LINE__,__MYFILE__)
            z_old_2d(:,i_time0:i_time1)=z_field
            i_time_old(i_time0:i_time1)=i_time
            i_tab_start(2)=1
            i_tab_count(2)=i_pos
            WRITE(*,*)' writing output I_FILL i_time0 ',i_time0
            WRITE(*,*)' writing output I_FILL i_time1 ',i_time1
            CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, &
            & z_old_2d, start=i_tab_start, count=i_tab_count),__LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_var( i_file_id, i_time_var, &
         & i_time_old,start=(/i_tab_start(2)/), count=(/i_tab_count(2)/)),__LINE__,__MYFILE__)
          ELSE
            I_FILL = 0
            DEALLOCATE(i_time_old)
            DEALLOCATE(i_time_rest)
            DEALLOCATE(z_old_2d)
          ENDIF
         ELSE
         WRITE(*,*)' Starting at time ',i_time_old(i_tab_start(2)-1),i_time(1)
         DEALLOCATE(i_time_old)
         ENDIF
      ENDIF
      !
      IF ( I_FILL == 0 ) THEN
         WRITE(*,*)' writing output start ',i_tab_start
	 WRITE(*,*)' writing output count ',i_tab_count
         CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, &
         & z_field, start=i_tab_start, count=i_tab_count),__LINE__,__MYFILE__)

         CALL nchdlerr( nf90_put_var( i_file_id, i_time_var, &
         & i_time,start=(/i_tab_start(2)/), count=(/i_tab_count(2)/)),__LINE__,__MYFILE__)
      ENDIF

      !
      
      CALL nchdlerr( nf90_close( i_file_id ) ,__LINE__,__MYFILE__)
      !
      RETURN
      !
   END SUBROUTINE write_omona_netcdf_2d_r
   !
   !----------------------------------------------------------------------
   !
   SUBROUTINE write_omona_netcdf_3d_r (cl_filename, z_field,i_time, &
                                       cl_boxes, z_missing, i_fill)
!
! write a 3d array (time_serie) of reals
!
      IMPLICIT NONE
!
      CHARACTER(len=80), INTENT(in)                :: cl_filename 
      INTEGER, DIMENSION(:), INTENT(in)            :: i_time
      INTEGER, INTENT(inout)                       :: i_fill
      REAL, DIMENSION(:,:,:), INTENT(in)           :: z_field
      REAL, INTENT(in)                             :: z_missing
      CHARACTER(len=20), DIMENSION(:), INTENT(in)  :: cl_boxes

!
      LOGICAL               :: clbon
      INTEGER               :: i_var_exist, i_pos
      INTEGER               :: i_file_id, i_var_id
      INTEGER               :: i_nb_files,i_z,i_b
      INTEGER               :: i_tim_id,i_box_id,i_z_id
      INTEGER               :: i_time_var
      INTEGER               :: i_file
      INTEGER, DIMENSION(3) :: i_dim, i_tab_start, i_tab_count
      INTEGER               :: i_rest_start, i_rest_end
      INTEGER               :: i_time0, i_time1
      REAL                  :: z_min, z_max
      REAL                  :: z_min_old, z_max_old
      INTEGER, DIMENSION(:), ALLOCATABLE :: i_time_old
      INTEGER, DIMENSION(:), ALLOCATABLE :: i_time_rest
      INTEGER               :: i_t
      REAL, DIMENSION(:,:,:), ALLOCATABLE :: z_old_3d
!
      i_tab_start(1) = 1
      i_tab_start(2) = 1
      i_tab_start(3) = 1
      i_z=SIZE(z_field,1)   !depth
      i_b=SIZE(z_field,2)   !nbox
      i_nb_files = SIZE(z_field,3)
      i_tab_count(1) = i_z
      i_tab_count(2) = i_b
      i_tab_count(3) = i_nb_files
      i_time0        = 0
      i_time1        = 0
      i_rest_start   = 0
      i_rest_end     = 0

!      write(6,*)' in write_3d ,shape(z_field) ',shape(z_field)
!      write(6,*)' i_z =',i_z
!      write(6,*)' i_b =',i_b
!      write(6,*)' i_nb_files =',i_nb_files


!
      z_min = MINVAL(z_field, mask = z_field .NE. z_missing )
      z_max = MAXVAL(z_field, mask = z_field .NE. z_missing )
!
      INQUIRE( FILE=cl_filename, EXIST=clbon )
      IF ( .NOT. clbon ) THEN
         CALL create_time_series_netcdf_file ( cl_filename, cl_boxes)
      ELSE
         WRITE(*,*)' Time series file already exists'
      ENDIF
!     
      WRITE(*,*)' opening file ',cl_filename
      CALL nchdlerr( nf90_open( cl_filename, nf90_write, i_file_id ) ,__LINE__,__MYFILE__)
!
      WRITE(*,*)'Inquiring var ',cl_var_out, i_var_id
      i_var_exist = nf90_inq_varid( i_file_id, cl_var_out, i_var_id)
      WRITE(*,*)'i_var_exist ',i_var_exist

      IF ( i_var_exist .NE. 0 ) THEN
!       write(*,*)' Inquiring dimensions'
       CALL nchdlerr( nf90_inq_dimid( i_file_id, 'depth', i_z_id ) ,__LINE__,__MYFILE__)
       CALL nchdlerr( nf90_inq_dimid( i_file_id, 'box', i_box_id ) ,__LINE__,__MYFILE__)
       CALL nchdlerr( nf90_inq_dimid( i_file_id, 'time', i_tim_id ) ,__LINE__,__MYFILE__)

       i_dim(1) = i_z_id
       i_dim(2) = i_box_id
       i_dim(3) = i_tim_id
!       write(*,*)' In write_omona_ncdf_3d: Dimensions: i_dim ',i_dim


       CALL nchdlerr( nf90_redef( i_file_id ) ,__LINE__,__MYFILE__)
       CALL nchdlerr( nf90_def_var( i_file_id, cl_var_out,  nf90_real,i_dim, i_var_id ) ,__LINE__,__MYFILE__)
        CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'standard_name', cl_varname1 ),__LINE__,__MYFILE__ )
       CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'long_name', cl_varname2 ) ,__LINE__,__MYFILE__)
        CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'units', cl_varunit1 ) ,__LINE__,__MYFILE__)
        CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'unit_long', cl_varunit2 ) ,__LINE__,__MYFILE__)
        CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'missing_value', z_missing ) ,__LINE__,__MYFILE__)
        CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', z_min ) ,__LINE__,__MYFILE__)
        CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', z_max ) ,__LINE__,__MYFILE__)
!
        CALL nchdlerr( nf90_enddef( i_file_id ) ,__LINE__,__MYFILE__)

!       CALL nchdlerr( nf90_inq_dimid( i_file_id, 'time', i_tim_id ) ,__LINE__,__MYFILE__)
!        write(6,*)' inq_dimid ',i_tim_id
        CALL nchdlerr( nf90_inq_varid( i_file_id, 'time', i_time_var ) ,__LINE__,__MYFILE__)
        WRITE(*,*) 'inq_varid i_tim_id for var time ',i_time_var
        i_pos = 1 
        i_tab_start(3)=1
      ELSE
        !new addition
!        CALL nchdlerr (nf90_inquire_variable( i_file_id, i_var_id, dimids=i_dim ) ,__LINE__,__MYFILE__)
!        write(6,*)' after inquire variable i_dim = ',i_dim
!      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dim(1), len=i_pos )  ,__LINE__,__MYFILE__)
!        write(6,*)' i_dim(1), len(1) ',i_dim(1),i_pos
!      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dim(2), len=i_pos )  ,__LINE__,__MYFILE__)
!        write(6,*)' i_dim(2), len(2) ',i_dim(2),i_pos
!      CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_dim(3), len=i_pos )  ,__LINE__,__MYFILE__)
!        write(6,*)' i_dim(3), len(3) ',i_dim(3),i_pos
        !!!!

        CALL nchdlerr( nf90_get_att( i_file_id, i_var_id, 'valid_min', z_min_old ) ,__LINE__,__MYFILE__)
        CALL nchdlerr( nf90_get_att( i_file_id, i_var_id, 'valid_max', z_max_old ) ,__LINE__,__MYFILE__)
        z_min = MIN( z_min, z_min_old )
        z_max = MAX( z_max, z_max_old )
        CALL nchdlerr( nf90_redef( i_file_id ) ,__LINE__,__MYFILE__)
        CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', z_min ) ,__LINE__,__MYFILE__)
        CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', z_max ) ,__LINE__,__MYFILE__)
        CALL nchdlerr( nf90_enddef( i_file_id ) ,__LINE__,__MYFILE__)


 
!        CALL nchdlerr( nf90_inq_varid( i_file_id, 'depth', i_z_id ) ,__LINE__,__MYFILE__)
!        CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_z_id, len=i_pos ) ,__LINE__,__MYFILE__)
!        write(*,*)' i_z_id, dim ',i_z_id,i_pos

!        CALL nchdlerr( nf90_inq_varid( i_file_id, 'box', i_box_id ) ,__LINE__,__MYFILE__)
!        CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_box_id, len=i_pos ) ,__LINE__,__MYFILE__)
!        write(*,*)' i_box_id, dim ',i_box_id,i_pos


 
       CALL nchdlerr( nf90_inq_dimid( i_file_id, 'time', i_tim_id ) ,__LINE__,__MYFILE__)
!        write(6,*)' inq_dimid ',i_tim_id
        CALL nchdlerr( nf90_inquire_dimension( i_file_id, i_tim_id, len=i_pos ) ,__LINE__,__MYFILE__)
!       write(*,*) 'inq_var_id i_tim_id,dim ',i_tim_id,i_pos
        ALLOCATE(i_time_old(i_pos))
        CALL nchdlerr( nf90_inq_varid( i_file_id, 'time', i_time_var ) ,__LINE__,__MYFILE__)
!        write(*,*) 'inq_var_id time',i_time_var
        CALL nchdlerr( nf90_get_var( i_file_id, i_time_var, i_time_old ) ,__LINE__,__MYFILE__)
        i_tab_start(3)=i_pos+1
        DO i_t=i_pos,1,-1
          IF(i_time_old(i_t).GE.i_time(1)) THEN
            i_tab_start(3)=i_t
          ELSE
            EXIT
         ENDIF
        ENDDO

         WRITE(*,*)'i_tab_start(3)',i_tab_start(3)
         WRITE(*,*)'i_tab_count(3)',i_tab_count(3)

         IF (I_FILL == 1 ) THEN
           DO i_t=1,i_tab_start(3),1
             IF(i_time_old(i_t).LT.i_time(1)) THEN
                i_time0=i_t+1
             ELSE
                EXIT
             ENDIF
           ENDDO

           i_time0=min(i_time0,i_tab_start(3))
           i_tab_start(3)=i_time0
           i_time1=i_time0+i_tab_count(3)-1
           WRITE(*,*)'i_time0,i_tab_count(3),i_time1',i_Time0,i_tab_count(3),i_time1
           WRITE(*,*)' Starting at time ',i_time_old(i_tab_start(3)-1),i_time(1)
           i_rest_start=i_time1+1
           i_rest_end = i_pos
          IF ( i_rest_end .gt. i_rest_start ) THEN
            ALLOCATE( z_old_3d(i_z,i_b, i_pos))
            CALL nchdlerr( nf90_get_var( i_file_id, i_var_id, z_old_3d ) ,__LINE__,__MYFILE__)
            z_old_3d(:,:,i_time0:i_time1)=z_field
            i_time_old(i_time0:i_time1)=i_time
            i_tab_start(3)=1
            i_tab_count(3)=i_pos
            WRITE(*,*)' writing output I_FILL i_time0 ',i_time0
            WRITE(*,*)' writing output I_FILL i_time1 ',i_time1
            CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, &
            & z_old_3d, start=i_tab_start, count=i_tab_count),__LINE__,__MYFILE__)
            CALL nchdlerr( nf90_put_var( i_file_id, i_time_var, &
         & i_time_old,start=(/i_tab_start(3)/), count=(/i_tab_count(3)/)),__LINE__,__MYFILE__)
          ELSE
            I_FILL = 0
            DEALLOCATE(i_time_old)
            DEALLOCATE(i_time_rest)
            DEALLOCATE(z_old_3d)
          ENDIF
         ELSE
          WRITE(*,*)' Starting at time ',i_time_old(i_tab_start(3)-1),i_time(1)
          DEALLOCATE(i_time_old)
         ENDIF
      ENDIF
      !
      IF ( I_FILL == 0 ) THEN
         WRITE(*,*)' writing output count ',i_tab_count
         WRITE(*,*)' writing output start ',i_tab_start
         CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, &
           z_field, start=i_tab_start, count=i_tab_count),__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_var( i_file_id, i_time_var, &
           i_time,start=(/i_tab_start(3)/), count=(/i_tab_count(3)/) ),__LINE__,__MYFILE__)
      ENDIF
!
      CALL nchdlerr( nf90_close( i_file_id ),__LINE__,__MYFILE__ )
!
      RETURN
      !
   END SUBROUTINE write_omona_netcdf_3d_r
   !!----------------------------------------------------------------------
   !
   SUBROUTINE write_dep_netcdf ( cl_filename, cl_boxes, z_dep )
      !
      ! write a 2d array (map) of reals
      !
      IMPLICIT NONE
      !
      CHARACTER(len=80), INTENT(in)    :: cl_filename
      CHARACTER(len=20), DIMENSION(:), INTENT(in)  :: cl_boxes
      REAL, DIMENSION(:),   INTENT(in) :: z_dep
      !
      LOGICAL               :: clbon
      INTEGER               :: i_file_id, i_var_id
      INTEGER               :: i_dim_z_id
      INTEGER               :: i_dimz, i_var_exist
      INTEGER, DIMENSION(1) :: i_dim
      REAL                  :: z_min, z_max
      !
      i_dimz = i_dp
      !
      z_min = MINVAL(z_dep)
      z_max = MAXVAL(z_dep)
      !
      INQUIRE( FILE=cl_filename, EXIST=clbon )
      IF ( .NOT. clbon ) THEN
         CALL create_time_series_netcdf_file ( cl_filename, cl_boxes )
      ELSE
         WRITE(*,*)' Time series file already exists'
      ENDIF
      !     
      CALL nchdlerr( nf90_open( cl_filename, nf90_write, i_file_id ) ,__LINE__,__MYFILE__)
      !
      CALL nchdlerr( nf90_inq_dimid( i_file_id, 'depth', i_dim_z_id ) ,__LINE__,__MYFILE__)
      i_dim(1) = i_dim_z_id
      !      write(6,*)' defining depth axis : i_dim(1)= ',i_dim(1)
      !
      i_var_exist = nf90_inq_varid( i_file_id, "depth", i_var_id ) 
      IF ( i_var_exist .NE. 0 ) THEN
         CALL nchdlerr( nf90_redef( i_file_id ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_def_var( i_file_id, "depth", nf90_real,i_dim, i_var_id ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'standard_name','depth') ,__LINE__,__MYFILE__) 
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'long_name', 'depth below the surface') ,__LINE__,__MYFILE__) 
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'units', 'm' ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'unit_long','meter' ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_min', z_min ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_put_att( i_file_id, i_var_id, 'valid_max', z_max ) ,__LINE__,__MYFILE__)
         CALL nchdlerr( nf90_enddef( i_file_id ) ,__LINE__,__MYFILE__)
      ENDIF
      !
      CALL nchdlerr( nf90_put_var( i_file_id, i_var_id, z_dep) ,__LINE__,__MYFILE__)
      !
      CALL nchdlerr( nf90_close( i_file_id ) ,__LINE__,__MYFILE__)
      !
      RETURN
      !
   END SUBROUTINE write_dep_netcdf
   !
   !
   !----------------------------------------------------------------------
   !
   ! C R E A T E _ T I M E _ S E R I E S _ N E T C D F _ F I L E
   !
   !----------------------------------------------------------------------
   !
   SUBROUTINE create_time_series_netcdf_file ( cl_filename,cl_boxes)
      !
      ! create time_series netcdf file
      !
      IMPLICIT NONE
      !

      CHARACTER(len=80), INTENT(in)         :: cl_filename
      CHARACTER(len=20), DIMENSION(:)       :: cl_boxes
      !
      LOGICAL               :: clbon
      INTEGER               :: i_file_id, i_dim_z_id, i_dim_box_id, &
         i_dim_len_id, i_dim_t_id
      INTEGER               :: i_box_id, i_tim_id
      INTEGER               :: i_nbox
      !
      i_nbox = SIZE(cl_boxes,1)
      !
      
      WRITE(*,*)' create_time_series',cl_filename
      CALL nchdlerr( nf90_create( cl_filename, nf90_clobber, i_file_id ) ,__LINE__,__MYFILE__)
      !
      CALL nchdlerr( nf90_def_dim( i_file_id, 'len', 20, i_dim_len_id ) ,__LINE__,__MYFILE__)
      CALL nchdlerr( nf90_def_dim( i_file_id, 'depth',i_dp, i_dim_z_id ) ,__LINE__,__MYFILE__)
      CALL nchdlerr( nf90_def_dim( i_file_id, 'box', i_nbox, i_dim_box_id ) ,__LINE__,__MYFILE__)
      CALL nchdlerr( nf90_def_dim( i_file_id, 'time', nf90_unlimited, i_dim_t_id ) ,__LINE__,__MYFILE__)


      WRITE(*,*)' In create_time_series_ntcdf'
      WRITE(*,*)' z,box,time:       ',i_dim_z_id,i_dim_box_id, i_dim_t_id

      WRITE(*,*)' Definining box var '
      CALL nchdlerr( nf90_def_var( i_file_id, 'box', nf90_char, &
         (/i_dim_len_id,i_dim_box_id/), i_box_id ),__LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_att( i_file_id, i_box_id, &
         'long_name', 'Name of the box' ) ,__LINE__,__MYFILE__)

      WRITE(*,*)' Definining time var'
      CALL nchdlerr( nf90_def_var( i_file_id, 'time', nf90_int, &
         (/i_dim_t_id/),i_tim_id ) ,__LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_att( i_file_id, i_tim_id, 'standard_name','time') ,__LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_att( i_file_id, i_tim_id, 'long_name','time') ,__LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_att( i_file_id, i_tim_id, 'unit','days as %Y%m%d') ,__LINE__,__MYFILE__)
      CALL nchdlerr( nf90_put_att( i_file_id, i_tim_id, 'unit_long','days as %Y%m%d') ,__LINE__,__MYFILE__)
      !
      CALL nchdlerr( nf90_enddef( i_file_id ) ,__LINE__,__MYFILE__)

      CALL nchdlerr( nf90_put_var( i_file_id, i_box_id, cl_boxes &
         ),__LINE__,__MYFILE__ )
      !
      CALL nchdlerr( nf90_close( i_file_id ),__LINE__,__MYFILE__ )
      !
      RETURN
      !
   END SUBROUTINE create_time_series_netcdf_file


   !-----------------------------------------------------------------------
   SUBROUTINE variable_att
      !-----------------------------------------------------------------------
      !
      !                       ROUTINE variable_att
      !                     **********************
      !
      !  Purpose :
      !  -------
      !    set attributes for a given variable name 
      !
      !  Original :  M. Balmaseda
      IMPLICIT NONE
      !-----------------------------------------------------------------------
      !   Different variables
      !-----------------------------------------------------------------------
      !
      INTEGER:: ji,jj,jk
      REAL   :: dx0    !nominal longitudinal resolution in deg
      REAL   :: alat, alon,aux
      INTEGER,DIMENSION(:,:), ALLOCATABLE:: iaux

      SELECT CASE ( TRIM(cl_var) )
      CASE ('sosstsst')
         i_nb_dims=2
         cl_var_out='specified_sea_surface_temperature'
         cl_varname1='Sea_Surface_Temperature'
         cl_varname2='Sea Surface Temperature'
         cl_varunit1='C'
         cl_varunit2='degrees C'
         cl_code='159'
         cl_grid='t'
      CASE ('somxl010')
         i_nb_dims=2
         cl_var_out='ocean_mixed_layer_thickness'
         cl_varname1='ocean_mixed_layer_thickness'
         cl_varname2='depth of the ocean mixed layer from the surface'
         cl_varunit1='m'
         cl_varunit2='meter'
         cl_code='148'
         cl_grid='t'
      CASE ('somixhgt')
         i_nb_dims=2
         cl_var_out='turbocline_depth'
         cl_varname1='turbocline_depth'
         cl_varname2='turbocline depth'
         cl_varunit1='m'
         cl_varunit2='meter'
         cl_code='248'
         cl_grid='t'
      CASE ('somxlt05')
         i_nb_dims=2
         cl_var_out='ocean_mixed_layer_depth'
         cl_varname1='ocean_mixed_layer_depth'
         cl_varname2='depth of the ocean mixed layer T 0.5 criteria'
         cl_varunit1='m'
         cl_varunit2='meter'
         cl_code='348'
         cl_grid='t'
      CASE ('sohtc300')
         i_nb_dims=2
         cl_var_out='hc300'
         cl_varname1='300m_ocean_heat_content'
         cl_varname2='ocean heat content over the first 300m'
         cl_varunit1='J/m2'
         cl_varunit2='J/m2'
         cl_code='164'
         cl_grid='t'
      CASE ('sohtc700')
         i_nb_dims=2
         cl_var_out='hc700'
         cl_varname1='700m_ocean_heat_content'
         cl_varname2='ocean heat content over the first 700m'
         cl_varunit1='J/m2'
         cl_varunit2='J/m2'
         cl_code='264'
         cl_grid='t'
      CASE ('sohtcbtm')
         i_nb_dims=2
         cl_var_out='hcbtm'
         cl_varname1='Column_ocean_heat_content'
         cl_varname2='ocean heat content over the whole column'
         cl_varunit1='J/m2'
         cl_varunit2='J/m2'
         cl_code='364'
         cl_grid='t'
      CASE ('sosal300')
         i_nb_dims=2
         cl_var_out='sal300'
         cl_varname1='300m_integrated salinity'
         cl_varname2='salinity integrated over the first 300m'
         cl_varunit1='psu*m'
         cl_varunit2='psu*m'
         cl_code='175'
         cl_grid='t'
      CASE ('sosal700')
         i_nb_dims=2
         cl_var_out='sal700'
         cl_varname1='700m_integrated salinity'
         cl_varname2='salinity integrated over the first 700m'
         cl_varunit1='psu*m'
         cl_varunit2='psu*m'
         cl_code='275'
         cl_grid='t'
      CASE ('sosalbtm')
         i_nb_dims=2
         cl_var_out='salbtm'
         cl_varname1='Column_integrate_salinity'
         cl_varname2='Integrated Salinity over the whole column'
         cl_varunit1='psu*m'
         cl_varunit2='psu*m'
         cl_code='375'
         cl_grid='t'
      CASE ('so20chgt')
         i_nb_dims=2
         cl_var_out='d20_isotherm_depth'
         cl_varname1='depth'
         cl_varname2='depth of the D20 isotherm'
         cl_varunit1='m'
         cl_varunit2='meter'
         cl_code='163'
         cl_grid='t'
      CASE ('so28chgt')
         i_nb_dims=2
         cl_var_out='d28_isotherm_depth'
         cl_varname1='28I_depth'
         cl_varname2='depth of the D28 isotherm'
         cl_varunit1='m'
         cl_varunit2='meter'
         cl_code='263'
         cl_grid='t'
      CASE ('so26chgt')
         i_nb_dims=2
         cl_var_out='d26_isotherm_depth'
         cl_varname1='26I_depth'
         cl_varname2='depth of the D26 isotherm'
         cl_varunit1='m'
         cl_varunit2='meter'
         cl_code='463'
         cl_grid='t'
      CASE ('sothedep')
         i_nb_dims=2
         cl_var_out='thermocline_depth'
         cl_varname1='thermocline_depth'
         cl_varname2='thermocline depth'
         cl_varunit1='m'
         cl_varunit2='meter'
         cl_code='363'
         cl_grid='t'
      CASE ('sossheig')
         i_nb_dims=2
         cl_var_out='sea_surface_height_above_geoid'
         cl_varname1='sea_surface_height_above_geoid'
         cl_varname2='sea_surface_height_above_geoid'
         cl_varunit1='m'
         cl_varunit2='meter'
         cl_code='145'      
         cl_grid='t'
      CASE ('sostheig')
         i_nb_dims=2
         cl_var_out='steric_height'
         cl_varname1='steric_height'
         cl_varname2='steric_height'
         cl_varunit1='m'
         cl_varunit2='meter'
         cl_code='150'      
         cl_grid='t'
      CASE ('sobpheig')
         i_nb_dims=2
         cl_var_out='bottom_pressure_equivalent_height'
         cl_varname1='Bottom Pressure'
         cl_varname2='Bottom Pressure'
         cl_varunit1='m'
         cl_varunit2='meter'
         cl_code='149'      
         cl_grid='t'
      CASE ('sohefldo')
         i_nb_dims=2
         cl_var_out='specified_surface_heat_flux'
         cl_varname1='Net Downward Heat Flux'
         cl_varname2='Net Downward Heat Flux'
         cl_varunit1='W/m2'
         cl_varunit2='Watt_per_square_meter'
         cl_code='160'      
         cl_grid='t'
      CASE ('sohefldp')
         i_nb_dims=2
         cl_var_out='heat_flux_correction'
         cl_varname1='Surface_Heat_Flux_Damping'
         cl_varname2='Surface Heat Flux: Damping'
         cl_varunit1='W/m2'
         cl_varunit2='Watt_per_square_meter'
         cl_code='162'      
         cl_grid='t'
      CASE ('soshfldo')
         i_nb_dims=2
         cl_var_out='absorbed_solar_radiation'
         cl_varname1='Shortwave_Radiation'
         cl_varname2='Shortwave Radiation'
         cl_varunit1='W/m2'
         cl_varunit2='Watt_per_square_meter'
         cl_code='157'      
         cl_grid='t'
      CASE ('sosalflx')
         i_nb_dims=2
         cl_var_out='Surface_Salt_Flux'
         cl_varname1='Surface_Salt_Flux'
         cl_varname2='Surface Salt Flux'
         cl_varunit1='Kg/m2/s'
         cl_varunit2='Kg/m2/s'
         cl_code='260'      
         cl_grid='t'
      CASE ('sosafldp')
         i_nb_dims=2
         cl_var_out='Surface_salt_flux_damping'
         cl_varname1=cl_var_out
         cl_varname2='Surface salt flux: damping'
         cl_varunit1='Kg/m2/s'
         cl_varunit2='Kg/m2/s'
         cl_code='261'      
         cl_grid='t'
      CASE ('sowaflup')
         i_nb_dims=2
         cl_var_out='Net_Upward_Water_Flux'
         cl_varname1=cl_var_out
         cl_varname2=cl_var_out
         cl_varunit1='Kg/m2/s'
         cl_varunit2='Kg/m2/s'
         cl_code='258'      
         cl_grid='t'
      CASE ('sowafldp')
         i_nb_dims=2
         cl_var_out='Surface_Water_Flux_Damping'
         cl_varname1=cl_var_out
         cl_varname2=cl_var_out
         cl_varunit1='Kg/m2/s'
         cl_varunit2='Kg/m2/s'
         cl_code='262'      
         cl_grid='t'
      CASE ('soicetem')
         i_nb_dims=2
         cl_var_out='Ice_Surface_Temperature'
         cl_varname1=cl_var_out
         cl_varname2=cl_var_out
         cl_varunit1='K'
         cl_varunit2='Kelvin'
         cl_code='429'      
         cl_grid='t'
      CASE ('soicealb')
         i_nb_dims=2
         cl_var_out='Ice_Albedo'
         cl_varname1=cl_var_out
         cl_varname2=cl_var_out
         cl_varunit1='dl'
         cl_varunit2='dimensionless'
         cl_code='430'      
         cl_grid='t'
      CASE ('soicecov')
         i_nb_dims=2
         cl_var_out='Ice_Fraction'
         cl_varname1=cl_var_out
         cl_varname2=cl_var_out
         cl_varunit1='dl'
         cl_varunit2='dimensionless'
         cl_code='431'      
         cl_grid='t'
      CASE ('votemper')
         i_nb_dims=3
         cl_var_out='sea_water_potential_temperature'
         cl_varname1='sea_water_potential_temperature'
         cl_varname2='Potential temperature ref to surface C'
         cl_varunit1='C'
         cl_varunit2='degree_Celcius'
         cl_code='129'
         cl_grid='t'
      CASE ('vosaline')
         i_nb_dims=3
         cl_var_out='sea_water_salinity'
         cl_varname1='sea_water_salinity'
         cl_varname2='sea water salinity'
         cl_varunit1='PSU'
         cl_varunit2='practical_salinity_scale'
         cl_code='130'
         cl_grid='t'
      CASE ('vosigmat')
         i_nb_dims=3
         cl_var_out='sigmat'
         cl_varname1='sigmat'
         cl_varname2='sigmat'
         cl_varunit1='C'
         cl_varunit2='NA'
         cl_code='138'
         cl_grid='t'
      CASE ('vottrdmp')
         i_nb_dims=3
         cl_var_out='Damping_T_3D'
         cl_varname1='Damping_T_3D'
         cl_varname2='Damping T 3D'
         cl_varunit1='C/s'
         cl_varunit2='degree_Celsius/second'
         cl_code='201'
         cl_grid='t'
      CASE ('vostrdmp')
         i_nb_dims=3
         cl_var_out='Damping_S_3D'
         cl_varname1='Damping_S_3D'
         cl_varname2='Damping S_3D'
         cl_varunit1='psu/s'
         cl_varunit2='psu/second'
         cl_code='202'
         cl_grid='t'
      CASE ('votbiasd')
         i_nb_dims=3
         cl_var_out='Bias_T_direct'
         cl_varname1='Bias_T_direct'
         cl_varname2='Bias_T_direct'
         cl_varunit1='C'
         cl_varunit2='degree_Celsius'
         cl_code='203'
         cl_grid='t'
      CASE ('vosbiasd')
         i_nb_dims=3
         cl_var_out='Bias_S_direct'
         cl_varname1='Bias_S_direct'
         cl_varname2='Bias_S_direct'
         cl_varunit1='psu'
         cl_varunit2='psu'
         cl_code='204'
         cl_grid='t'
      CASE ('votbiasp')
         i_nb_dims=3
         cl_var_out='Bias_T_pressure'
         cl_varname1='Bias_T_pressure'
         cl_varname2='Bias_T_pressure'
         cl_varunit1='C'
         cl_varunit2='degree_Celsius'
         cl_code='205'
         cl_grid='t'
      CASE ('vosbiasp')
         i_nb_dims=3
         cl_var_out='Bias_S_pressure'
         cl_varname1='Bias_S_pressure'
         cl_varname2='Bias_S_pressure'
         cl_varunit1='psu'
         cl_varunit2='psu'
         cl_code='206'
         cl_grid='t'
      CASE ('vorbiasp')
         i_nb_dims=3
         cl_var_out='Bias_rho_pressure'
         cl_varname1='Bias_rho_pressure'
         cl_varname2='Bias_rho_pressure'
         cl_varunit1='kg/m3/s'
         cl_varunit2='kg/m3/s'
         cl_code='207'
         cl_grid='t'
      CASE ('sozotaux')
         i_nb_dims=2
         cl_var_out='surface_downward_eastward_stress'
         cl_varname1='Wind_Stress_along_i-axis'
         cl_varname2=cl_var_out
         cl_varunit1='N/m2'
         cl_varunit2='N/m2'
         cl_code='153'      
         cl_grid='u'
         if (sec .eq. 'v') then
            cl_varname2='taux*x*z'
            cl_varunit1='Sv'
            cl_varunit2='Sv'
         endif
         if (sec .eq. 'u') then
            cl_varname2='taux*y*z'
            cl_varunit1='Sv'
            cl_varunit2='Sv'
         endif
      CASE ('vozocrtx')
         i_nb_dims=3
         cl_var_out='sea_water_x_velocity'
         cl_varname1='sea_water_x_velocity'
         cl_varname2='U zonal current m/s'
         cl_varunit1='m/s'
         cl_varunit2='meter_per_second'
         cl_code='131'
         cl_grid='u'
         if (sec .eq. 'u') then
            cl_var_out='zonal_transport'
            cl_varname1='zonal_transport'
            cl_varname2='zonal_transport'
            cl_varunit1='Sv'
            cl_varunit2='Sv'
         endif
      CASE ('vozout')
         i_nb_dims=3
         cl_var_out='UT'
         cl_varname1='UT'
         cl_varname2='UT'
         cl_varunit1='K*m/s'
         cl_varunit2='K*m/s'
         cl_code='169'
         cl_grid='t'
         if (sec .eq. 'u') then
          cl_var_out='zonal_H_transport'
          cl_varname1='zonal_H_transport'
          cl_varname2='zonal_H_transport'
          cl_varunit1='PW'
          cl_varunit2='PW'
         endif
      CASE ('vozous')
         i_nb_dims=3
         cl_var_out='US'
         cl_varname1='US'
         cl_varname2='US'
         cl_varunit1='psu*m/s'
         cl_varunit2='psu*m/s'
         cl_code='171'
         cl_grid='t'
         if (sec .eq. 'u') then
          cl_var_out='zonal_S_transport'
          cl_varname1='zonal_S_transport'
          cl_varname2='zonal_S_transport'
          cl_varunit1='kT/s'
          cl_varunit2='kT/s'
         endif
      CASE ('vomecrty')
         i_nb_dims=3
         cl_var_out='sea_water_y_velocity'
         cl_varname1='sea_water_y_velocity'
         cl_varname2='V meridional current m/s'
         cl_varunit1='m/s'
         cl_varunit2='meter_per_second'
         cl_code='132'
         cl_grid='v'
         if (sec .eq. 'v') then
            cl_var_out='meridional_transport'
            cl_varname1='meridional_transport'
            cl_varname2='meridional_transport'
            cl_varunit1='Sv'
            cl_varunit2='Sv'
         endif
      CASE ('sometauy')
         i_nb_dims=2
         cl_var_out='surface_downward_northward_stress'
         cl_varname1='Wind_Stress_along_j-axis'
         cl_varname2=cl_var_out
         cl_varunit1='N/m2'
         cl_varunit2='N/m2'
         cl_code='154'      
         cl_grid='v'
         if (sec .eq. 'v') then
            cl_varname2='tauy*x*z'
            cl_varunit1='Sv'
            cl_varunit2='Sv'
         endif
         if (sec .eq. 'u') then
            cl_varname2='tauy*y*z'
            cl_varunit1='Sv'
            cl_varunit2='Sv'
         endif
      CASE ('vomevt')
         i_nb_dims=3
         cl_var_out='VT'
         cl_varname1='VT'
         cl_varname2='VT'
         cl_varunit1='K*m/s'
         cl_varunit2='K*m/s'
         cl_code='170'
         cl_grid='t'
         if (sec .eq. 'v') then
          cl_var_out='meridional_H_transport'
          cl_varname1='meridional_H_transport'
          cl_varname2='meridional_H_transport'
          cl_varunit1='PW'
          cl_varunit2='PW'
         endif
      CASE ('vomevs')
         i_nb_dims=3
         cl_var_out='VS'
         cl_varname1='VS'
         cl_varname2='VS'
         cl_varunit1='psu*m/s'
         cl_varunit2='psu*m/s'
         cl_code='172'
         cl_grid='t'
         if (sec .eq. 'v') then
          cl_var_out='meridional_S_transport'
          cl_varname1='meridional_S_transport'
          cl_varname2='meridional_S_transport'
          cl_varunit1='kT/s'
          cl_varunit2='kT/s'
         endif
      CASE ('vovecrtz')
         i_nb_dims=3
         cl_var_out='upward_sea_water_velocity'
         cl_varname1='upward_sea_water_velocity'
         cl_varname2='W vertical current m/s'
         cl_varunit1='m/s'
         cl_varunit2='meter_per_second'
         cl_code='133'
         cl_grid='f'
      CASE ('votkeavm')
         i_nb_dims=3
         cl_var_out='vertical_eddy_viscosity'
         cl_varname1='vertical_eddy_viscosity'
         cl_varname2='vertical eddy viscosity'
         cl_varunit1='m2/s'
         cl_varunit2='square_meter_per_second'
         cl_code='135'
         cl_grid='f'
      CASE ('votkeavt')
         i_nb_dims=3
         cl_var_out='vertical_eddy_diffusivity'
         cl_varname1='vertical_eddy_diffusivity'
         cl_varname2='vertical eddy diffusivity'
         cl_varunit1='m2/s'
         cl_varunit2='square_meter_per_second'
         cl_code='136'
         cl_grid='f'
      CASE ('votkeevm')
         i_nb_dims=3
         cl_var_out='enhanced_vertical_eddy_viscosity'
         cl_varname1='enhanced_vertical_eddy_viscosity'
         cl_varname2='enhanced vertical eddy viscosity'
         cl_varunit1='m2/s'
         cl_varunit2='square_meter_per_second'
         cl_code='235'
         cl_grid='f'
      CASE ('votkeevd')
         i_nb_dims=3
         cl_var_out='enhanced_vertical_eddy_diffusivity'
         cl_varname1='enhanced_vertical_eddy_diffusivity'
         cl_varname2='enhanced vertical eddy diffusivity'
         cl_varunit1='m2/s'
         cl_varunit2='square_meter_per_second'
         cl_code='236'
         cl_grid='f'
      CASE ('voddmavs')
         i_nb_dims=3
         cl_var_out='salt_vertical_eddy_diffusivity'
         cl_varname1='salt_vertical_eddy_diffusivity'
         cl_varname2='salt vertical eddy diffusivity'
         cl_varunit1='m2/s'
         cl_varunit2='square_meter_per_second'
         cl_code='237'
         cl_grid='f'
      CASE ('bckint')
         i_nb_dims=3
         cl_var_out='assim_incr_temperature'
         cl_varname1='assim_incr_temperature'
         cl_varname2='assim_incr_temperature'
         cl_varunit1='K/s'
         cl_varunit2='Kelvin_per_second'
         cl_code='178'
         cl_grid='t'
      CASE ('bckins')
         i_nb_dims=3
         cl_var_out='assim_incr_salinity'
         cl_varname1='assim_incr_salinity'
         cl_varname2='assim_incr_salinity'
         cl_varunit1='psu/s'
         cl_varunit2='psu_per_second'
         cl_code='184'
         cl_grid='t'
      CASE ('bckinu')
         i_nb_dims=3
         cl_var_out='assim_incr_uvel'
         cl_varname1='assim_incr_uvel'
         cl_varname2='assim_incr_uvel'
         cl_varunit1='m/s2'
         cl_varunit2='meter_per_square_second'
         cl_code='179'
         cl_grid='u'
      CASE ('bckinv')
         i_nb_dims=3
         cl_var_out='assim_incr_vvel'
         cl_varname1='assim_incr_vvel'
         cl_varname2='assim_incr_vvel'
         cl_varunit1='m/s2'
         cl_varunit2='meter_per_square_second'
         cl_code='180'
         cl_grid='v'
      CASE ('bckineta')
         i_nb_dims=2
         cl_var_out='assim_incr_eta'
         cl_varname1='assim_incr_eta'
         cl_varname2='assim_incr_eta'
         cl_varunit1='m/s'
         cl_varunit2='meter_per_second'
         cl_code='181'
         cl_grid='t'
      CASE ('vozoeivu')
         i_nb_dims=3
         cl_var_out='vozoeivu'
         cl_varname1='vozoeivu'
         cl_varname2='eddy U velocity m/s'
         cl_varunit1='m/s'
         cl_varunit2='meter_per_second'
         cl_code='231'
      CASE ('vomeeivv')
         i_nb_dims=3
         cl_var_out='vomeeivv'
         cl_varname1='vomeeivv'
         cl_varname2='eddy V velocity m/s'
         cl_varunit1='m/s'
         cl_varunit2='meter_per_second'
         cl_code='232'
      CASE default
         WRITE(*,*)'Variables not defined' 
         CALL ABORT
      END SELECT
      cl_var_out  = TRIM(cl_var_out) 
      cl_varname1 = TRIM(cl_varname1) 
      cl_varname2 = TRIM(cl_varname2) 
      cl_varunit1 = TRIM(cl_varunit1) 
      cl_varunit2 = TRIM(cl_varunit2)
      cl_code     = TRIM(cl_code)
      cl_grid     = TRIM(cl_grid) 

      cl_filename_out = TRIM(cl_expnam) // TRIM(cl_date) // '_' // TRIM(cl_code) //sec//'mona.nc'
      cl_filename2_out = TRIM(cl_expnam) // TRIM(cl_date) // '_' // TRIM(cl_code) // 'xy' //sec//'mona.nc'
!      IF ( i_nb_dims .EQ. 2 ) THEN
!         i_dp=1
!      ELSE
!         i_dp=jpk
!      ENDIF
      WRITE(*,*)'Variable Attributes: '
      WRITE(*,*)'   cl_var_out = ',cl_var_out
!      WRITE(*,*)'   i_dp       = ',i_dp
      WRITE(*,*)'   cl_grid    = ',cl_grid
      WRITE(*,*)'   cl_code    = ',cl_code
      WRITE(*,*)'   i_nb_dims  = ',i_nb_dims
      WRITE(*,*)'------------------------------------ '

      RETURN
   END SUBROUTINE variable_att
   !-----------------------------------------------------------------------
   SUBROUTINE obs_variable_att(cl_type)
      !-----------------------------------------------------------------------
      !
      !                       ROUTINE obs_variable_att
      !                     ****************************
      !
      !  Purpose :
      !  -------
      !    set attributes for a given variable name (feedback version)
      !
      !  Original :  K. Mogensen
      IMPLICIT NONE
      !-----------------------------------------------------------------------
      !   Different variables
      !-----------------------------------------------------------------------
      CHARACTER(len=*) :: cl_type
      !
      SELECT CASE ( TRIM(cl_var) )
      CASE ('sossheig')
         i_nb_dims=2
         cl_var_out='sea_surface_height_above_geoid'
         cl_varname1='sea_surface_height_above_geoid'
         cl_varname2='sea_surface_height_above_geoid'
         cl_varunit1='m'
         cl_varunit2='meter'
         cl_code='145' 
      CASE ('sosstsst')
         i_nb_dims=2
         cl_var_out='sea_surface_temperature'
         cl_varname1='sea_surface_temperature'
         cl_varname2='sea_surface_temperature'
         cl_varunit1='K'
         cl_varunit2='Kelvin'
         cl_code='159'
      CASE ('votemper')
         i_nb_dims=3
         cl_var_out='sea_water_potential_temperature'
         cl_varname1='sea_water_potential_temperature'
         cl_varname2='Potential temperature ref to surface C'
         cl_varunit1='C'
         cl_varunit2='degree_Celcius'
         cl_code='129'
      CASE ('vosaline')
         i_nb_dims=3
         cl_var_out='sea_water_salinity'
         cl_varname1='sea_water_salinity'
         cl_varname2='sea water salinity'
         cl_varunit1='PSU'
         cl_varunit2='practical_salinity_scale'
         cl_code='130'
      CASE ('vozocrtx')
         i_nb_dims=3
         cl_var_out='sea_water_x_velocity'
         cl_varname1='sea_water_x_velocity'
         cl_varname2='U zonal current m/s'
         cl_varunit1='m/s'
         cl_varunit2='meter_per_second'
         cl_code='131'
      CASE ('vomecrty')
         i_nb_dims=3
         cl_var_out='sea_water_y_velocity'
         cl_varname1='sea_water_y_velocity'
         cl_varname2='V meridional current m/s'
         cl_varunit1='m/s'
         cl_varunit2='meter_per_second'
         cl_code='132'
      CASE default
         WRITE(*,*)'Variables not defined' 
         CALL ABORT
      END SELECT
      cl_var_out  = TRIM(cl_var_out) 
      cl_varname1 = TRIM(cl_varname1) 
      cl_varname2 = TRIM(cl_varname2) 
      cl_varunit1 = TRIM(cl_varunit1) 
      cl_varunit2 = TRIM(cl_varunit2)
      cl_code     = TRIM(cl_code)
      cl_filename_out = TRIM(cl_expnam) // TRIM(cl_date) // '_' // &
         & TRIM(cl_code) // '_'//TRIM(cl_type)//'_'//'fbmona.nc'
      cl_filename2_out = TRIM(cl_expnam) // TRIM(cl_date) // '_' // &
         & TRIM(cl_code) // '_'//TRIM(cl_type)//'_'// 'xy' //'_'//'fbmona.nc'
     
      RETURN
   END SUBROUTINE obs_variable_att


END MODULE omonainfo
