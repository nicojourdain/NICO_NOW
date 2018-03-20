MODULE obs_prof_io
   !!======================================================================
   !!                       ***  MODULE obs_prof_io  ***
   !! Observation operators : I/O for ENACT and Coriolis files
   !!======================================================================
   !! History : 
   !!             !  09-01  (K. Mogensen) Initial version
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   read_enactfile    :  Read a obfbdata structure from an ENACT file.
   !!   read_coriofile    :  Read a obfbdata structure from an Coriolis file.
   !!----------------------------------------------------------------------
   USE par_kind
   USE obs_utils
   USE obs_fbm
   USE obs_conv
   USE netcdf
   IMPLICIT NONE

   INTEGER, PARAMETER :: imaxlev = 10000

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_prof_io.F90 2287 2010-10-18 07:53:52Z smasson $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

#include "obsprof_io.h90"

END MODULE obs_prof_io
