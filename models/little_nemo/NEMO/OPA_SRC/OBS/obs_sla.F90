MODULE obs_sla
   !!=====================================================================
   !!                       ***  MODULE  obs_sla  ***
   !! Observation diagnostics: Storage space for SLA observations
   !!                          arrays and additional flags etc.
   !!=====================================================================
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obs_sla.F90 2733 2011-04-08 15:55:31Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   
   !! * Modules used 
   USE obs_surf_def ! Definition of SLA data types and tools

   IMPLICIT NONE
   
   SAVE

   !! * Routine accessibility
   PRIVATE

   PUBLIC nslavars, nslaextr, nslasets, sladata, sladatqc

   !! * Shared Module variables
   INTEGER :: nslavars                               ! Number of sladata variables
   INTEGER :: nslaextr                               ! Number of sladata extra 
                                                     ! variables
   INTEGER :: nslasets                               ! Number of sladata sets                                               
   TYPE(obs_surf), POINTER, DIMENSION(:) :: sladata  ! Initial SLA data
   TYPE(obs_surf), POINTER, DIMENSION(:) :: sladatqc ! SLA data after quality control

END MODULE obs_sla
