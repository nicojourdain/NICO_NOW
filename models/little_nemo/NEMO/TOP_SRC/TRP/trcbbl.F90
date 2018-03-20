MODULE trcbbl
  !!======================================================================
   !!                       ***  MODULE  trcbbl  ***
   !! Ocean passive tracers physics :  advective and/or diffusive bottom boundary 
   !!                                  layer scheme
   !!======================================================================
   !!==============================================================================
   !! History :  OPA  !  1996-06  (L. Mortier)  Original code
   !!            8.0  !  1997-11  (G. Madec)    Optimization
   !!   NEMO     1.0  !  2002-08  (G. Madec)  free form + modules
   !!             -   !  2004-01  (A. de Miranda, G. Madec, J.M. Molines ) add advective bbl
   !!            3.3  !  2009-11  (G. Madec)  merge trabbl and trabbl_adv + style + optimization 
   !!             -   !  2010-04  (G. Madec)  Campin & Goosse advective bbl 
   !!             -   !  2010-06  (C. Ethe, G. Madec)  merge TRA-TRC
   !!----------------------------------------------------------------------
#if  defined key_top &&  defined key_trabbl 
   !!----------------------------------------------------------------------
   !!   'key_trabbl                      diffusive or/and adevective bottom boundary layer
   !!----------------------------------------------------------------------
   !!    trc_bbl       : update the tracer trends due to the bottom boundary layer (advective and/or diffusive)
   !!----------------------------------------------------------------------
   USE oce_trc             ! ocean dynamics and active tracers variables
   USE trc                 ! ocean passive tracers variables
   USE trcnam_trp      ! passive tracers transport namelist variables
   USE trabbl              ! 
   USE prtctl_trc          ! Print control for debbuging
   USE trdmod_oce
   USE trdtra

   PUBLIC   trc_bbl       !  routine called by step.F90


   !! * Substitutions
#  include "top_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcbbl.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS


   SUBROUTINE trc_bbl( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE bbl  ***
      !!                   
      !! ** Purpose :   Compute the before tracer (t & s) trend associated 
      !!     with the bottom boundary layer and add it to the general trend
      !!     of tracer equations.
      !!
      !!----------------------------------------------------------------------  
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step 
      CHARACTER (len=22) :: charout
      REAL(wp), POINTER, DIMENSION(:,:,:,:) ::   ztrtrd
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_bbl')
      !
      IF( .NOT. lk_offline .AND. nn_dttrc == 1 ) THEN
         CALL bbl( kt, nittrc000, 'TRC' )      ! Online coupling with dynamics  : Computation of bbl coef and bbl transport
         l_bbl = .FALSE.                       ! Offline coupling with dynamics : Read bbl coef and bbl transport from input files
      ENDIF

      IF( l_trdtrc )  THEN
         CALL wrk_alloc( jpi, jpj, jpk, jptra, ztrtrd ) ! temporary save of trends
         ztrtrd(:,:,:,:)  = tra(:,:,:,:)
      ENDIF

      !* Diffusive bbl :
      IF( nn_bbl_ldf == 1 ) THEN
         !
         CALL tra_bbl_dif( trb, tra, jptra )  
         IF( ln_ctl )   THEN
            WRITE(charout, FMT="(' bbl_dif')")  ;  CALL prt_ctl_trc_info(charout)
            CALL prt_ctl_trc( tab4d=tra, mask=tmask, clinfo=ctrcnm, clinfo2='trd' )
         ENDIF
         !
      END IF

      !* Advective bbl : bbl upstream advective trends added to the tracer trends
      IF( nn_bbl_adv /= 0 ) THEN
         !
         CALL tra_bbl_adv( trb, tra, jptra )  
         IF( ln_ctl )   THEN
            WRITE(charout, FMT="(' bbl_adv')")  ;  CALL prt_ctl_trc_info(charout)
            CALL prt_ctl_trc( tab4d=tra, mask=tmask, clinfo=ctrcnm, clinfo2='trd' )
         ENDIF
         !
      END IF

      IF( l_trdtrc )   THEN                      ! save the horizontal diffusive trends for further diagnostics
        DO jn = 1, jptra
           ztrtrd(:,:,:,jn) = tra(:,:,:,jn) - ztrtrd(:,:,:,jn)
           CALL trd_tra( kt, 'TRC', jn, jptra_trd_ldf, ztrtrd(:,:,:,jn) )
        END DO
        CALL wrk_dealloc( jpi, jpj, jpk, jptra, ztrtrd ) ! temporary save of trends
      ENDIF
      !
      IF( nn_timing == 1 ) CALL timing_stop('trc_bbl')
      !
   END SUBROUTINE trc_bbl

#else
   !!----------------------------------------------------------------------
   !!   Dummy module :                      No bottom boundary layer scheme
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_bbl( kt )              ! Empty routine
      WRITE(*,*) 'tra_bbl: You should not have seen this print! error?', kt
   END SUBROUTINE trc_bbl
#endif

   !!======================================================================
END MODULE trcbbl
