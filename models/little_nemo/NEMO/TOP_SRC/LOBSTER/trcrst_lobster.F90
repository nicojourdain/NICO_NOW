MODULE trcrst_lobster
   !!======================================================================
   !!                       ***  MODULE trcrst_lobster  ***
   !! TOP :   create, write, read the restart files of LOBSTER tracer
   !!======================================================================
   !! History :   1.0  !  2010-01 (C. Ethe) Original
   !!----------------------------------------------------------------------
#if defined key_lobster
   !!----------------------------------------------------------------------
   !!   'key_lobster'                                               lobster tracers
   !!----------------------------------------------------------------------
   !!   trc_rst_read_lobster   : read  restart file
   !!   trc_rst_wri_lobster    : write restart file
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE par_trc         ! TOP parameters
   USE trc             ! TOP variables
   USE trcsms_lobster          ! lobster sms trends
   USE sms_lobster          ! lobster sms trends
   USE iom

   IMPLICIT NONE
   PRIVATE

   PUBLIC  trc_rst_read_lobster   ! called by trcini.F90 module
   PUBLIC  trc_rst_wri_lobster   ! called by trcini.F90 module

CONTAINS
   
   SUBROUTINE trc_rst_read_lobster( knum ) 
      !!----------------------------------------------------------------------
      !!                     ***  trc_rst_read_lobster  ***  
      !!
      !! ** Purpose : Read in restart file specific variables from lobster model
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in)  :: knum  ! unit of the restart file
      !!----------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_rst_read_lobster : Read specific variables from lobster model '
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'

      CALL iom_get( knum, jpdom_autoglo, 'SEDB'//ctrcnm(jp_lob_det), sedpocb(:,:) ) 
      CALL iom_get( knum, jpdom_autoglo, 'SEDN'//ctrcnm(jp_lob_det), sedpocn(:,:) ) 

   END SUBROUTINE trc_rst_read_lobster

   SUBROUTINE trc_rst_wri_lobster( kt, kitrst, knum )
      !!----------------------------------------------------------------------
      !!                     ***  trc_rst_read_lobster  ***
      !!
      !! ** Purpose : Read in restart file specific variables from lobster model
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in)  :: kt      ! time step
      INTEGER, INTENT(in)  :: kitrst  ! time step of restart write
      INTEGER, INTENT(in)  :: knum    ! unit of the restart file
      !!----------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_rst_wri_lobster : Write specific variables from lobster model '
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'

      CALL iom_rstput( kt, kitrst, knum, 'SEDB'//ctrcnm(jp_lob_det), sedpocb(:,:) )
      CALL iom_rstput( kt, kitrst, knum, 'SEDN'//ctrcnm(jp_lob_det), sedpocn(:,:) )

   END SUBROUTINE trc_rst_wri_lobster

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_rst_read_lobster( knum )
     INTEGER, INTENT(in)  :: knum
     WRITE(*,*) 'trc_rst_wri_lobster: You should not have seen this print! error?',knum
   END SUBROUTINE trc_rst_read_lobster

   SUBROUTINE trc_rst_wri_lobster( kt, kitrst, knum )
     INTEGER, INTENT(in)  :: kt, kitrst, knum
     WRITE(*,*) 'trc_rst_wri_lobster: You should not have seen this print! error?', kt, kitrst, knum
   END SUBROUTINE trc_rst_wri_lobster
#endif

   !!======================================================================
END MODULE trcrst_lobster
