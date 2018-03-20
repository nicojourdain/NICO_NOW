MODULE trcrst_my_trc
   !!======================================================================
   !!                       ***  MODULE trcrst_my_trc  ***
   !! TOP :   create, write, read the restart files of MY_TRC tracer
   !!======================================================================
   !! History :   1.0  !  2010-01 (C. Ethe)  Original
   !!----------------------------------------------------------------------
#if defined key_my_trc
   !!----------------------------------------------------------------------
   !!   'key_my_trc'                                               CFC tracers
   !!----------------------------------------------------------------------
   !!   trc_rst_read_my_trc   : read  restart file
   !!   trc_rst_wri_my_trc    : write restart file
   !!----------------------------------------------------------------------

   IMPLICIT NONE
   PRIVATE

   PUBLIC  trc_rst_read_my_trc   ! called by trcini.F90 module
   PUBLIC  trc_rst_wri_my_trc   ! called by trcini.F90 module

CONTAINS
   
   SUBROUTINE trc_rst_read_my_trc( knum ) 
     INTEGER, INTENT(in)  :: knum
     WRITE(*,*) 'trc_rst_read_my_trc: No specific variables to read on unit', knum
   END SUBROUTINE trc_rst_read_my_trc

   SUBROUTINE trc_rst_wri_my_trc( kt, kirst, knum )
     INTEGER, INTENT(in)  :: kt, kirst, knum
     WRITE(*,*) 'trc_rst_wri_my_trc: No specific variables to write on unit' ,knum, ' at time ', kt, kirst
   END SUBROUTINE trc_rst_wri_my_trc

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_rst_read_my_trc( knum )
     INTEGER, INTENT(in)  :: knum
     WRITE(*,*) 'trc_rst_read_my_trc: You should not have seen this print! error?', knum
   END SUBROUTINE trc_rst_read_my_trc

   SUBROUTINE trc_rst_wri_my_trc( kt, kirst, knum )
     INTEGER, INTENT(in)  :: kt, kirst, knum
     WRITE(*,*) 'trc_rst_wri_my_trc: You should not have seen this print! error?', kt, kirst, knum
   END SUBROUTINE trc_rst_wri_my_trc
#endif

   !!======================================================================
END MODULE trcrst_my_trc
