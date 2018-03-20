MODULE obs_inter_z1d
   !!======================================================================
   !!                       ***  MODULE obs_inter_z1d  ***
   !! Observation diagnostics: Perform the vertical interpolation
   !!                          from model grid to observation location
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   obs_int_z1d     : Vertical interpolation to the observation point
   !!   obs_int_z1d_spl : Compute the vertical 2nd derivative of the
   !!                     interpolating function for a cubic spline (n1dint=1)
   !!----------------------------------------------------------------------
   !! * Modules used
   USE par_kind, ONLY : &  ! Precision variables
      & wp

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE

   PUBLIC obs_int_z1d,    &  ! Vertical interpolation to the observation pt.
      &   obs_int_z1d_spl    ! Compute the vertical 2nd derivative of the
                             ! interpolating function used with a cubic spline

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_inter_z1d.F90 2287 2010-10-18 07:53:52Z smasson $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

#include "obsinter_z1d.h90"

END MODULE obs_inter_z1d

