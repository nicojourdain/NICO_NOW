MODULE dom_ice_2
   !!======================================================================
   !!                   ***  MODULE  dom_ice  ***
   !! LIM 2.0 Sea Ice :   Domain  variables
   !!======================================================================
   !! History :   2.0  !  03-08  (C. Ethe)  Free form and module
   !!             3.3  !  2009-05 (G.Garric, C. Bricaud) addition of lim2_evp case
   !!----------------------------------------------------------------------
#if defined key_lim2
   !!----------------------------------------------------------------------
   !!   'key_lim2'                                       LIM2 sea-ice model
   !!----------------------------------------------------------------------
   !! NEMO/LIM2 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: dom_ice_2.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   USE par_ice_2   ! LIM parameters

   IMPLICIT NONE
   PRIVATE

   PUBLIC    dom_ice_alloc_2    ! Called from nemogcm.F90

   LOGICAL, PUBLIC ::   l_jeq     = .TRUE.     !: Equator inside the domain flag

   INTEGER, PUBLIC ::   njeq , njeqm1          !: j-index of the equator if it is inside the domain
      !                                        !  (otherwise = jpj+10 (SH) or -10 (SH) )

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)         ::   fs2cor , fcor     !: coriolis factor and coeficient
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)         ::   covrai            !: sine of geographic latitude
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)         ::   area              !: surface of grid cell 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)         ::   tms    , tmu      !: temperature and velocity points masks
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:)     ::   wght              !: weight of the 4 neighbours to compute averages


# if defined key_lim2_vp
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:)     ::   akappa , bkappa   !: first and third group of metric coefficients
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:,:,:) ::   alambd            !: second group of metric coefficients
# else
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)         ::   tmv    , tmf      !: y-velocity and F-points masks
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)         ::   tmi               !: ice mask: =1 if ice thick > 0
# endif
   !!----------------------------------------------------------------------
   CONTAINS

   INTEGER FUNCTION dom_ice_alloc_2()
      !!----------------------------------------------------------------------
      USE lib_mpp, ONLY:   ctl_warn   ! MPP library
      INTEGER :: ierr(2)
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !
      ALLOCATE( fs2cor(jpi,jpj)     , fcor(jpi,jpj) ,                                   &
         &      covrai(jpi,jpj)     , area(jpi,jpj) , tms(jpi,jpj) , tmu(jpi,jpj) ,     &
         &      wght  (jpi,jpj,2,2)                                               , STAT=ierr(1) )
         !
      ALLOCATE(                                                    &
#if defined key_lim2_vp 
         &        akappa(jpi,jpj,2,2)     , bkappa(jpi,jpj,2,2),   &
         &        alambd(jpi,jpj,2,2,2,2) ,                        &
#else
         &        tmv(jpi,jpj) , tmf(jpi,jpj) , tmi(jpi,jpj) ,     &
#endif
         &        STAT=ierr(2) )
         !
      dom_ice_alloc_2 = MAXVAL(ierr)
      IF( dom_ice_alloc_2 /= 0 )   CALL ctl_warn('dom_ice_alloc_2: failed to allocate arrays')
      !
   END FUNCTION dom_ice_alloc_2

#else
   !!----------------------------------------------------------------------
   !!   Default option          Empty module         NO LIM-2 sea-ice model
   !!----------------------------------------------------------------------
#endif
   !!======================================================================
END MODULE dom_ice_2
