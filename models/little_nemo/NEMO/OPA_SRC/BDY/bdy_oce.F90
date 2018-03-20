MODULE bdy_oce
   !!======================================================================
   !!                       ***  MODULE bdy_oce   ***
   !! Unstructured Open Boundary Cond. :   define related variables
   !!======================================================================
   !! History :  1.0  !  2001-05  (J. Chanut, A. Sellar)  Original code
   !!            3.0  !  2008-04  (NEMO team)  add in the reference version     
   !!            3.3  !  2010-09  (D. Storkey) add ice boundary conditions
   !!            3.4  !  2011     (D. Storkey) rewrite in preparation for OBC-BDY merge
   !!----------------------------------------------------------------------
#if defined key_bdy 
   !!----------------------------------------------------------------------
   !!   'key_bdy'                      Unstructured Open Boundary Condition
   !!----------------------------------------------------------------------
   USE par_oce         ! ocean parameters
   USE bdy_par         ! Unstructured boundary parameters
   USE lib_mpp         ! distributed memory computing

   IMPLICIT NONE
   PUBLIC

   TYPE, PUBLIC ::   OBC_INDEX    !: Indices and weights which define the open boundary
      INTEGER,          DIMENSION(jpbgrd) ::  nblen
      INTEGER,          DIMENSION(jpbgrd) ::  nblenrim
      INTEGER, POINTER, DIMENSION(:,:)   ::  nbi
      INTEGER, POINTER, DIMENSION(:,:)   ::  nbj
      INTEGER, POINTER, DIMENSION(:,:)   ::  nbr
      INTEGER, POINTER, DIMENSION(:,:)   ::  nbmap
      REAL   , POINTER, DIMENSION(:,:)   ::  nbw
      REAL   , POINTER, DIMENSION(:)     ::  flagu
      REAL   , POINTER, DIMENSION(:)     ::  flagv
   END TYPE OBC_INDEX

   TYPE, PUBLIC ::   OBC_DATA     !: Storage for external data
      REAL, POINTER, DIMENSION(:)     ::  ssh
      REAL, POINTER, DIMENSION(:)     ::  u2d
      REAL, POINTER, DIMENSION(:)     ::  v2d
      REAL, POINTER, DIMENSION(:,:)   ::  u3d
      REAL, POINTER, DIMENSION(:,:)   ::  v3d
      REAL, POINTER, DIMENSION(:,:)   ::  tem
      REAL, POINTER, DIMENSION(:,:)   ::  sal
#if defined key_lim2
      REAL, POINTER, DIMENSION(:)     ::  frld
      REAL, POINTER, DIMENSION(:)     ::  hicif
      REAL, POINTER, DIMENSION(:)     ::  hsnif
#endif
   END TYPE OBC_DATA

   !!----------------------------------------------------------------------
   !! Namelist variables
   !!----------------------------------------------------------------------
   CHARACTER(len=80), DIMENSION(jp_bdy) ::   cn_coords_file !: Name of bdy coordinates file
   CHARACTER(len=80)                    ::   cn_mask_file   !: Name of bdy mask file
   !
   LOGICAL, DIMENSION(jp_bdy) ::   ln_coords_file           !: =T read bdy coordinates from file; 
   !                                                        !: =F read bdy coordinates from namelist
   LOGICAL                    ::   ln_mask_file             !: =T read bdymask from file
   LOGICAL                    ::   ln_vol                   !: =T volume correction             
   !
   INTEGER                    ::   nb_bdy                   !: number of open boundary sets
   INTEGER, DIMENSION(jp_bdy) ::   nn_rimwidth              !: boundary rim width for Flow Relaxation Scheme
   INTEGER                    ::   nn_volctl                !: = 0 the total volume will have the variability of the surface Flux E-P 
   !                                                        !  = 1 the volume will be constant during all the integration.
   INTEGER, DIMENSION(jp_bdy) ::   nn_dyn2d                 ! Choice of boundary condition for barotropic variables (U,V,SSH)
   INTEGER, DIMENSION(jp_bdy) ::   nn_dyn2d_dta      	    !: = 0 use the initial state as bdy dta ; 
                                                            !: = 1 read it in a NetCDF file
                                                            !: = 2 read tidal harmonic forcing from a NetCDF file
                                                            !: = 3 read external data AND tidal harmonic forcing from NetCDF files
   INTEGER, DIMENSION(jp_bdy) ::   nn_dyn3d                 ! Choice of boundary condition for baroclinic velocities 
   INTEGER, DIMENSION(jp_bdy) ::   nn_dyn3d_dta      	    !: = 0 use the initial state as bdy dta ; 
                                                            !: = 1 read it in a NetCDF file
   INTEGER, DIMENSION(jp_bdy) ::   nn_tra                   ! Choice of boundary condition for active tracers (T and S)
   INTEGER, DIMENSION(jp_bdy) ::   nn_tra_dta      	    !: = 0 use the initial state as bdy dta ; 
                                                            !: = 1 read it in a NetCDF file
#if defined key_lim2
   INTEGER, DIMENSION(jp_bdy) ::   nn_ice_lim2              ! Choice of boundary condition for sea ice variables 
   INTEGER, DIMENSION(jp_bdy) ::   nn_ice_lim2_dta          !: = 0 use the initial state as bdy dta ; 
                                                            !: = 1 read it in a NetCDF file
#endif
   !
   
   !!----------------------------------------------------------------------
   !! Global variables
   !!----------------------------------------------------------------------
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   bdytmask   !: Mask defining computational domain at T-points
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   bdyumask   !: Mask defining computational domain at U-points
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   bdyvmask   !: Mask defining computational domain at V-points

   REAL(wp)                                    ::   bdysurftot !: Lateral surface of unstructured open boundary

   REAL(wp), POINTER, DIMENSION(:,:)           ::   pssh       !: 
   REAL(wp), POINTER, DIMENSION(:,:)           ::   phur       !: 
   REAL(wp), POINTER, DIMENSION(:,:)           ::   phvr       !: Pointers for barotropic fields 
   REAL(wp), POINTER, DIMENSION(:,:)           ::   pu2d       !: 
   REAL(wp), POINTER, DIMENSION(:,:)           ::   pv2d       !: 

   !!----------------------------------------------------------------------
   !! open boundary data variables
   !!----------------------------------------------------------------------

   INTEGER,  DIMENSION(jp_bdy)                     ::   nn_dta            !: =0 => *all* data is set to initial conditions
                                                                          !: =1 => some data to be read in from data files
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:), TARGET ::   dta_global        !: workspace for reading in global data arrays
   TYPE(OBC_INDEX), DIMENSION(jp_bdy), TARGET      ::   idx_bdy           !: bdy indices (local process)
   TYPE(OBC_DATA) , DIMENSION(jp_bdy)              ::   dta_bdy           !: bdy external data (local process)

   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: bdy_oce.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   FUNCTION bdy_oce_alloc()
      !!----------------------------------------------------------------------
      USE lib_mpp, ONLY: ctl_warn, mpp_sum
      !
      INTEGER :: bdy_oce_alloc
      !!----------------------------------------------------------------------
      !
      ALLOCATE( bdytmask(jpi,jpj) , bdyumask(jpi,jpj), bdyvmask(jpi,jpj),                    &  
         &      STAT=bdy_oce_alloc )
         !
      IF( lk_mpp             )   CALL mpp_sum ( bdy_oce_alloc )
      IF( bdy_oce_alloc /= 0 )   CALL ctl_warn('bdy_oce_alloc: failed to allocate arrays.')
      !
   END FUNCTION bdy_oce_alloc

#else
   !!----------------------------------------------------------------------
   !!   Dummy module                NO Unstructured Open Boundary Condition
   !!----------------------------------------------------------------------
   LOGICAL ::   ln_tides = .false.  !: =T apply tidal harmonic forcing along open boundaries
#endif

   !!======================================================================
END MODULE bdy_oce

