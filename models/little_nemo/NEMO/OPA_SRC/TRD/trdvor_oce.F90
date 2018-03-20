MODULE trdvor_oce
   !!======================================================================
   !!                   ***  MODULE trdvor_oce  ***
   !! Ocean trends :   set vorticity trend variables
   !!======================================================================
   !! History :  9.0  !  04-2006  (L. Brunier, A-M. Treguier) Original code 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   USE par_oce      ! ocean parameters

   IMPLICIT NONE
   PRIVATE

#if defined key_trdvor
   LOGICAL, PUBLIC, PARAMETER ::   lk_trdvor = .TRUE.    !: momentum trend flag
#else
   LOGICAL, PUBLIC, PARAMETER ::   lk_trdvor = .FALSE.   !: momentum trend flag
#endif
   !                                               !!* vorticity trends index
   INTEGER, PUBLIC, PARAMETER ::   jpltot_vor = 11  !: Number of vorticity trend terms
   !
   INTEGER, PUBLIC, PARAMETER ::   jpvor_prg =  1   !: Pressure Gradient Trend
   INTEGER, PUBLIC, PARAMETER ::   jpvor_keg =  2   !: KE Gradient Trend
   INTEGER, PUBLIC, PARAMETER ::   jpvor_rvo =  3   !: Relative Vorticity Trend
   INTEGER, PUBLIC, PARAMETER ::   jpvor_pvo =  4   !: Planetary Vorticity Term Trend
   INTEGER, PUBLIC, PARAMETER ::   jpvor_ldf =  5   !: Horizontal Diffusion Trend
   INTEGER, PUBLIC, PARAMETER ::   jpvor_zad =  6   !: Vertical Advection Trend
   INTEGER, PUBLIC, PARAMETER ::   jpvor_zdf =  7   !: Vertical Diffusion Trend
   INTEGER, PUBLIC, PARAMETER ::   jpvor_spg =  8   !: Surface Pressure Grad. Trend
   INTEGER, PUBLIC, PARAMETER ::   jpvor_bev =  9   !: Beta V
   INTEGER, PUBLIC, PARAMETER ::   jpvor_swf = 10   !: wind stress forcing term
   INTEGER, PUBLIC, PARAMETER ::   jpvor_bfr = 11   !: bottom friction term

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: trdvor_oce.F90 2715 2011-03-30 15:58:35Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE trdvor_oce
