MODULE obs_vel_io
   !!======================================================================
   !!                       ***  MODULE obs_vel_io  ***
   !! Observation operators : I/O for TAO velocity data
   !!======================================================================
   !! History : 
   !!             !  09-01  (K. Mogensen) Initial version based on obs_read_taovel
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   read_taofile    :  Read a obfbdata structure from an TAO velocity file.
   !!----------------------------------------------------------------------
   USE par_kind
   USE obs_utils
   USE obs_fbm
   USE obs_conv
   USE in_out_manager
   USE netcdf
   IMPLICIT NONE

   INTEGER, PARAMETER :: imaxlev = 10000

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_vel_io.F90 2287 2010-10-18 07:53:52Z smasson $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

#include "obsvel_io.h90"

END MODULE obs_vel_io
