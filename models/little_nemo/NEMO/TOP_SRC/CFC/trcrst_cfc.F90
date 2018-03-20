MODULE trcrst_cfc
   !!======================================================================
   !!                       ***  MODULE trcrst_cfc  ***
   !! TOP :   create, write, read the restart files of CFC tracer
   !!======================================================================
   !! History :   1.0  !  2010-01 (C. Ethe) Original
   !!----------------------------------------------------------------------
#if defined key_cfc
   !!----------------------------------------------------------------------
   !!   'key_cfc'                                               CFC tracers
   !!----------------------------------------------------------------------
   !!   trc_rst_read_cfc   : read  restart file
   !!   trc_rst_wri_cfc    : write restart file
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE par_trc         ! TOP parameters
   USE trc             ! TOP variables
   USE trcsms_cfc          ! CFC sms trends
   USE iom

   IMPLICIT NONE
   PRIVATE

   PUBLIC  trc_rst_read_cfc   ! called by trcini.F90 module
   PUBLIC  trc_rst_wri_cfc   ! called by trcini.F90 module

CONTAINS
   
   SUBROUTINE trc_rst_read_cfc( knum ) 
      !!----------------------------------------------------------------------
      !!                     ***  trc_rst_read_cfc  ***  
      !!
      !! ** Purpose : Read in restart file specific variables from CFC model
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in)  :: knum  ! unit of the restart file
      INTEGER              :: jn    ! dummy loop indices
      !!----------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_rst_read_cfc : Read specific variables from CFC model '
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'
      
      DO jn = jp_cfc0, jp_cfc1
         CALL iom_get( knum, jpdom_autoglo, 'qint_'//ctrcnm(jn), qint_cfc(:,:,jn) ) 
      END DO

   END SUBROUTINE trc_rst_read_cfc

   SUBROUTINE trc_rst_wri_cfc( kt, kitrst, knum )
      !!----------------------------------------------------------------------
      !!                     ***  trc_rst_read_cfc  ***
      !!
      !! ** Purpose : Read in restart file specific variables from CFC model
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in)  :: kt      ! time step
      INTEGER, INTENT(in)  :: kitrst  ! time step of restart write
      INTEGER, INTENT(in)  :: knum    ! unit of the restart file
      INTEGER              :: jn      ! dummy loop indices
      !!----------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_rst_wri_cfc : Write specific variables from CFC model '
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'

      DO jn = jp_cfc0, jp_cfc1
         CALL iom_rstput( kt, kitrst, knum, 'qint_'//ctrcnm(jn), qint_cfc(:,:,jn) )
      END DO

   END SUBROUTINE trc_rst_wri_cfc

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_rst_read_cfc( knum )
     INTEGER, INTENT(in)  :: knum
     WRITE(*,*) 'trc_rst_wri_cfc: You should not have seen this print! error?', knum
   END SUBROUTINE trc_rst_read_cfc

   SUBROUTINE trc_rst_wri_cfc( kt, kitrst, knum )
     INTEGER, INTENT(in)  :: kt, kitrst, knum
     WRITE(*,*) 'trc_rst_wri_cfc: You should not have seen this print! error?', kt, kitrst, knum
   END SUBROUTINE trc_rst_wri_cfc
#endif

   !!======================================================================
END MODULE trcrst_cfc
