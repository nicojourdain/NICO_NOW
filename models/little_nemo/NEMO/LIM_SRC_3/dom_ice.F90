MODULE dom_ice
   !!======================================================================
   !!                   ***  MODULE  dom_ice  ***
   !! LIM-3 Sea Ice :   Domain  variables
   !!======================================================================
   !! History :  3.0  ! 2003-08  (M. Vancoppenolle)  LIM-3 original code
   !!            4.0  ! 2011-02  (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
   USE par_ice        ! LIM-3 parameter
   USE in_out_manager ! I/O manager
   USE lib_mpp         ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC dom_ice_alloc   ! Routine called by nemogcm.F90

   LOGICAL, PUBLIC ::   l_jeq = .TRUE.       !: Equator inside the domain flag

   INTEGER, PUBLIC ::   njeq , njeqm1        !: j-index of the equator if it is inside the domain

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fcor       !: coriolis coefficient
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   covrai     !: sine of geographic latitude
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   area       !: surface of grid cell 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   tms, tmi   !: temperature mask, mask for stress
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   tmu, tmv   !: mask at u and v velocity points
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   tmf        !: mask at f-point

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   wght   !: weight of the 4 neighbours to compute averages

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: dom_ice.F90 2777 2011-06-07 09:55:02Z smasson $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   FUNCTION dom_ice_alloc()
      !!-------------------------------------------------------------------
      !!            *** Routine dom_ice_alloc ***
      !!-------------------------------------------------------------------
      INTEGER :: dom_ice_alloc
      !!-------------------------------------------------------------------
      !
      ALLOCATE( fcor(jpi,jpj)                   ,      &
         &      covrai(jpi,jpj) , area(jpi,jpj) ,      &
         &      tms   (jpi,jpj) , tmi (jpi,jpj) ,      &
         &      tmu   (jpi,jpj) , tmv (jpi,jpj) ,      &
         &      tmf   (jpi,jpj) ,                      &
         &      wght(jpi,jpj,2,2)               , STAT = dom_ice_alloc )
      !
      IF( dom_ice_alloc /= 0 )   CALL ctl_warn( 'dom_ice_alloc: failed to allocate arrays.' )
      !
   END FUNCTION dom_ice_alloc

   !!======================================================================
END MODULE dom_ice
