MODULE par_ice_2
   !!======================================================================
   !!                       ***  MODULE par_ice_2   ***
   !! Sea-Ice model : definition of the parameters
   !!======================================================================
#if defined key_lim2
   !!----------------------------------------------------------------------
   !!  'key_lim2'                                       LIM-2 sea-ice model
   !!----------------------------------------------------------------------
   USE par_oce

   IMPLICIT NONE
   PUBLIC               ! allows par_oce and par_kind to be known in ice modules

   INTEGER, PUBLIC, PARAMETER ::   jpl        = 1              !: number of ice categories (only 1 in LIM-2)

   INTEGER, PUBLIC, PARAMETER ::   jplayers   = 2              !: number of vertical ice layers
   INTEGER, PUBLIC, PARAMETER ::   jplayersp1 = jplayers + 1   !: ???

#else
   !!----------------------------------------------------------------------
   !!  Default option                                No LIM-2 sea-ice model
   !!----------------------------------------------------------------------
#endif
   !!----------------------------------------------------------------------
   !! NEMO/LIM2 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: par_ice_2.F90 2528 2010-12-27 17:33:53Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE par_ice_2
