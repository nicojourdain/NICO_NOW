MODULE trcrst_c14b
   !!======================================================================
   !!                       ***  MODULE trcrst_c14b  ***
   !! TOP :   create, write, read the restart files of c14b tracer
   !!======================================================================
   !! History :   1.0  !  2010-01 (C. Ethe) Original
   !!----------------------------------------------------------------------
#if defined key_c14b
   !!----------------------------------------------------------------------
   !!   'key_c14b'                                               c14b tracers
   !!----------------------------------------------------------------------
   !!   trc_rst_read_c14b   : read  restart file
   !!   trc_rst_wri_c14b    : write restart file
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE par_trc         ! TOP parameters
   USE trc             ! TOP variables
   USE trcsms_c14b          ! c14b sms trends
   USE iom

   IMPLICIT NONE
   PRIVATE

   PUBLIC  trc_rst_read_c14b   ! called by trcini.F90 module
   PUBLIC  trc_rst_wri_c14b   ! called by trcini.F90 module

CONTAINS
   
   SUBROUTINE trc_rst_read_c14b( knum ) 
      !!----------------------------------------------------------------------
      !!                     ***  trc_rst_read_c14b  ***  
      !!
      !! ** Purpose : Read in restart file specific variables from c14b model
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in)  :: knum  ! unit of the restart file
      INTEGER              :: jn    ! dummy loop indices
      !!----------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_rst_read_c14b : Read specific variables from c14b model '
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'
      
      CALL iom_get( knum, jpdom_autoglo, 'qint_c14', qint_c14 ) 

   END SUBROUTINE trc_rst_read_c14b

   SUBROUTINE trc_rst_wri_c14b( kt, kitrst, knum )
      !!----------------------------------------------------------------------
      !!                     ***  trc_rst_read_c14b  ***
      !!
      !! ** Purpose : Read in restart file specific variables from c14b model
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in)  :: kt      ! time step
      INTEGER, INTENT(in)  :: kitrst  ! time step of restart write
      INTEGER, INTENT(in)  :: knum    ! unit of the restart file
      !!----------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_rst_wri_c14b : Write specific variables from c14b model '
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'

      CALL iom_rstput( kt, kitrst, knum, 'qint_c14', qint_c14 )

   END SUBROUTINE trc_rst_wri_c14b

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_rst_read_c14b( knum )
     INTEGER, INTENT(in)  :: knum
     WRITE(*,*) 'trc_rst_wri_c14b: You should not have seen this print! error?', knum
   END SUBROUTINE trc_rst_read_c14b

   SUBROUTINE trc_rst_wri_c14b( kt, kitrst, knum )
     INTEGER, INTENT(in)  :: kt, kitrst, knum
     WRITE(*,*) 'trc_rst_wri_c14b: You should not have seen this print! error?', kt, kitrst, knum
   END SUBROUTINE trc_rst_wri_c14b
#endif

   !!======================================================================
END MODULE trcrst_c14b
