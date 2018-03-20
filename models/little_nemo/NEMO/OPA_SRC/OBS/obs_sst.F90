MODULE obs_sst
   !!=====================================================================
   !!                       ***  MODULE  obs_sst  ***
   !! Observation diagnostics: Storage space for SST observations
   !!                          arrays and additional flags etc.
   !!=====================================================================
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_sst.F90 2733 2011-04-08 15:55:31Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

   
   !! * Modules used 
   USE obs_surf_def ! Definition of SST data types and tools

   IMPLICIT NONE
   
   SAVE

   !! * Routine accessibility
   PRIVATE

   PUBLIC nsstvars, nsstextr, nsstsets, sstdata, sstdatqc

   !! * Shared Module variables
   INTEGER :: nsstvars                               ! Number of sstdata variables
   INTEGER :: nsstextr                               ! Number of sstdata extra 
                                                     ! variables
   INTEGER :: nsstsets                               ! Number of sstdata sets
   TYPE(obs_surf), POINTER, DIMENSION(:) :: sstdata  ! Initial SST data
   TYPE(obs_surf), POINTER, DIMENSION(:) :: sstdatqc ! SST data after quality control

END MODULE obs_sst

