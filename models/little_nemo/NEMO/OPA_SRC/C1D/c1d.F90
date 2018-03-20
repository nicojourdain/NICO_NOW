MODULE c1d
   !!======================================================================
   !!                     ***  MODULE  c1d  ***
   !! Ocean domain  :  1D configuration
   !!=====================================================================
   !! History :   2.0  !  2004-09  (C. Ethe)  Original code
   !!             3.0  !  2008-04 (G. Madec)  adaptation to SBC
   !!----------------------------------------------------------------------

   IMPLICIT NONE
   PRIVATE

#if defined key_c1d
   LOGICAL, PUBLIC, PARAMETER ::   lk_c1d = .TRUE.    !: 1D config. flag activated
#else
   LOGICAL, PUBLIC, PARAMETER ::   lk_c1d = .FALSE.   !: 1D config. flag de-activated
#endif

   !!----------------------------------------------------------------------
   !! NEMO/C1D 3.3 , NEMO Consortium (2010)
   !! $Id: c1d.F90 2382 2010-11-13 13:08:12Z gm $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE c1d
