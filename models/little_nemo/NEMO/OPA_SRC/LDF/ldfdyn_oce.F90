MODULE ldfdyn_oce
   !!======================================================================
   !!                  ***  MODULE  ldfdyn_oce  ***
   !! Ocean physics:  lateral momentum mixing coefficient defined in memory 
   !!======================================================================
   !! History :  1.0  ! 2002-11  (G. Madec)  F90: Free form and module
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters
   USE in_out_manager ! I/O manager
   USE lib_mpp         ! MPP library

   IMPLICIT NONE
   PUBLIC

   !                                                  !!* Namelist namdyn_ldf : lateral mixing *
   LOGICAL , PUBLIC ::   ln_dynldf_lap   = .TRUE.      !: laplacian operator
   LOGICAL , PUBLIC ::   ln_dynldf_bilap = .FALSE.     !: bilaplacian operator
   LOGICAL , PUBLIC ::   ln_dynldf_level = .FALSE.     !: iso-level direction
   LOGICAL , PUBLIC ::   ln_dynldf_hor   = .TRUE.      !: horizontal (geopotential) direction
   LOGICAL , PUBLIC ::   ln_dynldf_iso   = .FALSE.     !: iso-neutral direction
   REAL(wp), PUBLIC ::   rn_ahm_0_lap    = 40000._wp   !: lateral laplacian eddy viscosity (m2/s)
   REAL(wp), PUBLIC ::   rn_ahmb_0       =     0._wp   !: lateral laplacian background eddy viscosity (m2/s)
   REAL(wp), PUBLIC ::   rn_ahm_0_blp    =     0._wp   !: lateral bilaplacian eddy viscosity (m4/s)
   REAL(wp), PUBLIC ::   ahm0, ahmb0, ahm0_blp         !: OLD namelist names

   !                                                                                  !!! eddy coeff. at U-,V-,W-pts [m2/s]
#if defined key_dynldf_c3d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ahm1, ahm2, ahm3, ahm4   !: ** 3D coefficients **
#elif defined key_dynldf_c2d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   ahm1, ahm2, ahm3, ahm4   !: ** 2D coefficients **
#elif defined key_dynldf_c1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)     ::   ahm1, ahm2, ahm3, ahm4   !: ** 2D coefficients **
#else
   REAL(wp), PUBLIC                                      ::   ahm1, ahm2, ahm3, ahm4   !: ** 0D coefficients **
#endif

   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: ldfdyn_oce.F90 2715 2011-03-30 15:58:35Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION ldfdyn_oce_alloc()
      !!----------------------------------------------------------------------
      !!                 ***  FUNCTION ldfdyn_oce_alloc  ***
      !!----------------------------------------------------------------------
      ldfdyn_oce_alloc = 0
#if defined key_dynldf_c3d
      ALLOCATE( ahm1(jpi,jpj,jpk) , ahm2(jpi,jpj,jpk) , ahm3(jpi,jpj,jpk) , ahm4(jpi,jpj,jpk) , STAT=ldfdyn_oce_alloc )
#elif defined key_dynldf_c2d
      ALLOCATE( ahm1(jpi,jpj    ) , ahm2(jpi,jpj    ) , ahm3(jpi,jpj    ) , ahm4(jpi,jpj    ) , STAT=ldfdyn_oce_alloc )
#elif defined key_dynldf_c1d
      ALLOCATE( ahm1(        jpk) , ahm2(        jpk) , ahm3(        jpk) , ahm4(        jpk) , STAT=ldfdyn_oce_alloc )
#endif
      IF( ldfdyn_oce_alloc /= 0 )   CALL ctl_warn('ldfdyn_oce_alloc: failed to allocate arrays')
      !
   END FUNCTION ldfdyn_oce_alloc

   !!======================================================================
END MODULE ldfdyn_oce
