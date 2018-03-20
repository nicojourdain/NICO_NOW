MODULE obs_seaice
   !!=====================================================================
   !!                       ***  MODULE  obs_seaice  ***
   !! Observation diagnostics: Storage space for sea ice observations
   !!                          arrays and additional flags etc.
   !!=====================================================================
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_seaice.F90 2733 2011-04-08 15:55:31Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

   
   !! * Modules used 
   USE obs_surf_def ! Definition of sea ice data types and tools

   IMPLICIT NONE
   
   SAVE

   !! * Routine accessibility
   PRIVATE

   PUBLIC nseaicevars, nseaiceextr, nseaicesets, seaicedata, seaicedatqc

   !! * Shared Module variables
   INTEGER :: nseaicevars                               ! Number of seaicedata variables
   INTEGER :: nseaiceextr                               ! Number of seaicedata extra 
                                                     ! variables
   INTEGER :: nseaicesets                               ! Number of seaicedata sets
   TYPE(obs_surf), POINTER, DIMENSION(:) :: seaicedata  ! Initial sea ice data
   TYPE(obs_surf), POINTER, DIMENSION(:) :: seaicedatqc ! Sea ice data after quality control

END MODULE obs_seaice

