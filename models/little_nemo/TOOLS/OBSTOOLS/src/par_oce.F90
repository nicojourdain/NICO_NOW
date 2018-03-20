MODULE par_oce
   !!======================================================================
   !!                        ***  par_oce  ***
   !! Ocean :   set the ocean parameters
   !!======================================================================
   !! History :  OPA  !  1991     (Imbard, Levy, Madec)  Original code
   !!   NEMO     1.0  !  2004-01  (G. Madec, J.-M. Molines)  Free form and module
   !!            3.3  !  2010-09  (C. Ethe) TRA-TRC merge: add jpts, jp_tem & jp_sal
   !!----------------------------------------------------------------------
   USE par_kind          ! kind parameters

   IMPLICIT NONE
   PUBLIC

   !!----------------------------------------------------------------------
   !!   Domain decomposition
   !!----------------------------------------------------------------------
   !! if we dont use massively parallel computer (parameters jpni=jpnj=1) so jpiglo=jpi and jpjglo=jpj
   INTEGER, PUBLIC            ::   jpni         !: number of processors following i 
   INTEGER, PUBLIC            ::   jpnj         !: number of processors following j
   INTEGER, PUBLIC            ::   jpnij        !: nb of local domain = nb of processors ( <= jpni x jpnj )
   INTEGER, PUBLIC, PARAMETER ::   jpr2di = 0   !: number of columns for extra outer halo 
   INTEGER, PUBLIC, PARAMETER ::   jpr2dj = 0   !: number of rows    for extra outer halo 
   INTEGER, PUBLIC, PARAMETER ::   jpreci = 1   !: number of columns for overlap 
   INTEGER, PUBLIC, PARAMETER ::   jprecj = 1   !: number of rows    for overlap 

   !! Ocean Domain sizes
   !! ------------------
   !!   data           domain   (jpidta,jpjdta)
   !!   global or zoom domain   (jpiglo,jpjglo)
   !!   local          domain   ( jpi  , jpj  )
   
!#if   defined key_orca_r4
!   !!---------------------------------------------------------------------
!   !!   'key_orca_r4'   :                           global ocean : ORCA R4
!   !!---------------------------------------------------------------------
!#             include "par_ORCA_R4.h90"
!#elif defined key_orca_r2
!   !!---------------------------------------------------------------------
!   !!   'key_orca_r2'   :                           global ocean : ORCA R4
!   !!---------------------------------------------------------------------
!#             include "par_ORCA_R2.h90"
!#elif defined key_orca_r1
!   !!---------------------------------------------------------------------
!   !!   'key_orca_r1'   :                           global ocean : ORCA R1
!   !!---------------------------------------------------------------------
!#             include "par_ORCA_R1.h90"
!#elif defined key_orca_r05
!   !!---------------------------------------------------------------------
!   !!   'key_orca_r05'  :                          global ocean : ORCA R05
!   !!---------------------------------------------------------------------
!#             include "par_ORCA_R05.h90"
!#elif defined key_orca_r025
!   !!---------------------------------------------------------------------
!   !!   'key_orca_r025' :                         global ocean : ORCA R025
!   !!---------------------------------------------------------------------
!#             include "par_ORCA_R025.h90"
!#elif defined key_eel_r2
!   !!---------------------------------------------------------------------
!   !!   'key_eel_r2'    :                                 channel : EEL R2
!   !!---------------------------------------------------------------------
!#             include "par_EEL_R2.h90"
!#elif defined key_eel_r5
!   !!---------------------------------------------------------------------
!   !!   'key_eel_r5'    :                                 channel : EEL R5
!   !!---------------------------------------------------------------------
!#             include "par_EEL_R5.h90"
!#elif defined key_eel_r6
!   !!---------------------------------------------------------------------
!   !!   'key_eel_r6'    :                                 channel : EEL R6
!   !!---------------------------------------------------------------------
!#             include "par_EEL_R6.h90"
!#elif defined key_gyre
!   !!---------------------------------------------------------------------
!   !!   'key_gyre'      :                        mid-latitude basin : GYRE
!   !!---------------------------------------------------------------------
!#             include "par_GYRE.h90"
!#elif defined key_pomme_r025
!   !!---------------------------------------------------------------------
!   !!   'key_pomme_r025':                        regional basin : POMME025
!   !!---------------------------------------------------------------------
!#             include "par_POMME_R025.h90"
!#else
   !!---------------------------------------------------------------------
   !!   default option  :                               small closed basin
   !!---------------------------------------------------------------------
   CHARACTER(len=16), PUBLIC, PARAMETER ::   cp_cfg = "default"   !: name of the configuration
   INTEGER          , PUBLIC, PARAMETER ::   jp_cfg = 0           !: resolution of the configuration

   ! data size                                       !!! * size of all input files *
   INTEGER, PUBLIC, PARAMETER ::   jpidta  = 10       !: 1st lateral dimension ( >= jpi )
   INTEGER, PUBLIC, PARAMETER ::   jpjdta  = 12       !: 2nd    "         "    ( >= jpj )
   INTEGER, PUBLIC, PARAMETER ::   jpkdta  = 31       !: number of levels      ( >= jpk )

   ! global or zoom domain size                      !!! * computational domain *
   INTEGER, PUBLIC, PARAMETER ::   jpiglo  = jpidta   !: 1st dimension of global domain --> i
   INTEGER, PUBLIC, PARAMETER ::   jpjglo  = jpjdta   !: 2nd    -                  -    --> j
   INTEGER, PUBLIC            ::   jpk     = jpkdta   !: number of vertical levels
   ! zoom starting position 
   INTEGER, PUBLIC, PARAMETER ::   jpizoom =   1      !: left bottom (i,j) indices of the zoom
   INTEGER, PUBLIC, PARAMETER ::   jpjzoom =   1      !: in data domain indices

   ! Domain characteristics
   INTEGER, PUBLIC, PARAMETER ::   jperio  =  0       !: lateral cond. type (between 0 and 6)
   !                                                  !  = 0 closed                 ;   = 1 cyclic East-West
   !                                                  !  = 2 equatorial symmetric   ;   = 3 North fold T-point pivot
   !                                                  !  = 4 cyclic East-West AND North fold T-point pivot
   !                                                  !  = 5 North fold F-point pivot
   !                                                  !  = 6 cyclic East-West AND North fold F-point pivot

   !!  Values set to pp_not_used indicates that this parameter is not used in THIS config.
   !!  Values set to pp_to_be_computed  indicates that variables will be computed in domzgr
   REAL(wp), PUBLIC, PARAMETER ::   pp_not_used       = 999999._wp   !: vertical grid parameter
   REAL(wp), PUBLIC, PARAMETER ::   pp_to_be_computed = 999999._wp   !:    -      -       -


   !! Horizontal grid parameters for domhgr
   !! =====================================
   INTEGER, PUBLIC, PARAMETER  ::   jphgr_msh = 0   !: type of horizontal mesh
   !                                                !  = 0 curvilinear coordinate on the sphere read in coordinate.nc
   !                                                !  = 1 geographical mesh on the sphere with regular grid-spacing
   !                                                !  = 2 f-plane with regular grid-spacing
   !                                                !  = 3 beta-plane with regular grid-spacing
   !                                                !  = 4 Mercator grid with T/U point at the equator

   REAL(wp) , PUBLIC, PARAMETER ::   ppglam0  =    0.0_wp   !: longitude of first raw and column T-point (jphgr_msh = 1)
   REAL(wp) , PUBLIC, PARAMETER ::   ppgphi0  =  -35.0_wp   !: latitude  of first raw and column T-point (jphgr_msh = 1)
   !                                                        !  used for Coriolis & Beta parameters (jphgr_msh = 2 or 3)
   REAL(wp) , PUBLIC, PARAMETER ::   ppe1_deg =    1.0_wp   !: zonal      grid-spacing (degrees)
   REAL(wp) , PUBLIC, PARAMETER ::   ppe2_deg =    0.5_wp   !: meridional grid-spacing (degrees)
   REAL(wp) , PUBLIC, PARAMETER ::   ppe1_m   = 5000.0_wp   !: zonal      grid-spacing (degrees)
   REAL(wp) , PUBLIC, PARAMETER ::   ppe2_m   = 5000.0_wp   !: meridional grid-spacing (degrees)

   !! Vertical grid parameter for domzgr
   !! ==================================
   REAL(wp), PUBLIC, PARAMETER ::   ppsur = -4762.96143546300_wp   !: ORCA r4, r2 and r05 coefficients
   REAL(wp), PUBLIC, PARAMETER ::   ppa0  =   255.58049070440_wp   !: (default coefficients)
   REAL(wp), PUBLIC, PARAMETER ::   ppa1  =   245.58132232490_wp   !:
   REAL(wp), PUBLIC, PARAMETER ::   ppkth =    21.43336197938_wp   !:
   REAL(wp), PUBLIC, PARAMETER ::   ppacr =     3.00000000000_wp   !:
   !
   !  If both ppa0 ppa1 and ppsur are specified to 0, then
   !  they are computed from ppdzmin, pphmax , ppkth, ppacr in dom_zgr
   REAL(wp), PUBLIC, PARAMETER ::   ppdzmin = 10._wp     !: Minimum vertical spacing
   REAL(wp), PUBLIC, PARAMETER ::   pphmax  = 5000._wp   !: Maximum depth
   !
   LOGICAL , PUBLIC, PARAMETER ::   ldbletanh = .TRUE.   !: Use/do not use double tanf function for vertical coordinates
   REAL(wp), PUBLIC, PARAMETER ::   ppa2  =   100.760928500000_wp   !: Double tanh function parameters
   REAL(wp), PUBLIC, PARAMETER ::   ppkth2=    48.029893720000_wp   !:
   REAL(wp), PUBLIC, PARAMETER ::   ppacr2=    13.000000000000_wp   !:
   !
!#endif


   !!---------------------------------------------------------------------
   !! Active tracer parameters
   !!---------------------------------------------------------------------
   INTEGER, PUBLIC, PARAMETER ::   jpts   = 2    !: Number of active tracers (=2, i.e. T & S )
   INTEGER, PUBLIC, PARAMETER ::   jp_tem = 1    !: indice for temperature
   INTEGER, PUBLIC, PARAMETER ::   jp_sal = 2    !: indice for salinity

   !!---------------------------------------------------------------------
   !! Domain Matrix size  (if AGRIF, they are not all parameters)
   !!---------------------------------------------------------------------
#if defined key_agrif
   INTEGER, PUBLIC, PARAMETER ::   nbghostcells = 1                             !: number of ghost cells
   INTEGER, PUBLIC            ::   nbcellsx     = jpiglo - 2 - 2*nbghostcells   !: number of cells in i-direction
   INTEGER, PUBLIC            ::   nbcellsy     = jpjglo - 2 - 2*nbghostcells   !: number of cells in j-direction
   !
#endif
   INTEGER, PUBLIC  ::   jpi   ! = ( jpiglo-2*jpreci + (jpni-1) ) / jpni + 2*jpreci   !: first  dimension
   INTEGER, PUBLIC  ::   jpj   ! = ( jpjglo-2*jprecj + (jpnj-1) ) / jpnj + 2*jprecj   !: second dimension
   INTEGER, PUBLIC  ::   jpk   ! = jpkdta                                             !: third dimension
   INTEGER, PUBLIC  ::   jpim1 ! = jpi-1                                            !: inner domain indices
   INTEGER, PUBLIC  ::   jpjm1 ! = jpj-1                                            !:   -     -      -
   INTEGER, PUBLIC  ::   jpkm1 ! = jpk-1                                            !:   -     -      -
   INTEGER, PUBLIC  ::   jpij  ! = jpi*jpj                                          !:  jpi x jpj

   !!---------------------------------------------------------------------
   !! Optimization/control flags
   !!---------------------------------------------------------------------
#if defined key_esopa
   LOGICAL, PUBLIC, PARAMETER ::   lk_esopa     = .TRUE.   !: flag to activate the all options
#else
   LOGICAL, PUBLIC, PARAMETER ::   lk_esopa     = .FALSE.  !: flag to activate the all options
#endif

#if defined key_vectopt_loop
   LOGICAL, PUBLIC, PARAMETER ::   lk_vopt_loop = .TRUE.   !: vector optimization flag
#else
   LOGICAL, PUBLIC, PARAMETER ::   lk_vopt_loop = .FALSE.  !: vector optimization flag
#endif

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: par_oce.F90 2715 2011-03-30 15:58:35Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE par_oce
