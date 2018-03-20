MODULE par_ice
   !!======================================================================
   !!                       ***  MODULE par_ice   ***
   !! LIM-3 Sea Ice :   definition of parameters
   !!======================================================================
   !! History :  3.0  ! 2003-08  (M. Vancoppenolle)  LIM-3
   !!----------------------------------------------------------------------
   USE par_oce

   IMPLICIT NONE
   PUBLIC          

   !                                             !!! ice thermodynamics
   INTEGER, PUBLIC, PARAMETER ::   jkmax    = 6   !: maximum number of ice layers
   INTEGER, PUBLIC, PARAMETER ::   nlay_s   = 1   !: number of snow layers

   !                                             !!! ice mechanical redistribution
   INTEGER, PUBLIC, PARAMETER ::   jpl      = 5   !: number of ice categories
   INTEGER, PUBLIC, PARAMETER ::   jpm      = 1   !: number of ice types

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: par_ice.F90 2528 2010-12-27 17:33:53Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE par_ice
