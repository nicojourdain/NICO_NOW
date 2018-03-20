MODULE par_cfc
   !!======================================================================
   !!                        ***  par_cfc  ***
   !! TOP :   set the CFC parameters
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: par_cfc.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   USE par_lobster, ONLY : jp_lobster      !: number of tracers in LOBSTER
   USE par_lobster, ONLY : jp_lobster_2d   !: number of 2D diag in LOBSTER
   USE par_lobster, ONLY : jp_lobster_3d   !: number of 3D diag in LOBSTER
   USE par_lobster, ONLY : jp_lobster_trd  !: number of biological diag in LOBSTER

   USE par_pisces , ONLY : jp_pisces       !: number of tracers in PISCES
   USE par_pisces , ONLY : jp_pisces_2d    !: number of 2D diag in PISCES
   USE par_pisces , ONLY : jp_pisces_3d    !: number of 3D diag in PISCES
   USE par_pisces , ONLY : jp_pisces_trd   !: number of biological diag in PISCES

   IMPLICIT NONE

   INTEGER, PARAMETER ::   jp_lc      = jp_lobster     + jp_pisces     !: cumulative number of passive tracers
   INTEGER, PARAMETER ::   jp_lc_2d   = jp_lobster_2d  + jp_pisces_2d  !:
   INTEGER, PARAMETER ::   jp_lc_3d   = jp_lobster_3d  + jp_pisces_3d  !:
   INTEGER, PARAMETER ::   jp_lc_trd  = jp_lobster_trd + jp_pisces_trd !:
   
#if defined key_cfc
   !!---------------------------------------------------------------------
   !!   'key_cfc'   :                                          CFC tracers
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_cfc     = .TRUE.      !: CFC flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc     =  1          !: number of passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc_2d  =  2          !: additional 2d output arrays ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc_3d  =  0          !: additional 3d output arrays ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc_trd =  0          !: number of sms trends for CFC
   
   ! assign an index in trc arrays for each CFC prognostic variables
   INTEGER, PUBLIC, PARAMETER ::   jpc11       = jp_lc + 1   !: CFC-11 
   INTEGER, PUBLIC, PARAMETER ::   jpc12       = jp_lc + 2   !: CFC-12   
#else
   !!---------------------------------------------------------------------
   !!   Default     :                                       No CFC tracers
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_cfc     = .FALSE.     !: CFC flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc     =  0          !: No CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc_2d  =  0          !: No CFC additional 2d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc_3d  =  0          !: No CFC additional 3d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc_trd =  0          !: number of sms trends for CFC
#endif

   ! Starting/ending CFC do-loop indices (N.B. no CFC : jp_cfc0 > jp_cfc1 the do-loop are never done)
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc0     = jp_lc + 1       !: First index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc1     = jp_lc + jp_cfc  !: Last  index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc0_2d  = jp_lc_2d  + 1       !: First index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc1_2d  = jp_lc_2d  + jp_cfc_2d  !: Last  index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc0_3d  = jp_lc_3d  + 1       !: First index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc1_3d  = jp_lc_3d  + jp_cfc_3d  !: Last  index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc0_trd = jp_lc_trd + 1       !: First index of CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_cfc1_trd = jp_lc_trd + jp_cfc_trd  !: Last  index of CFC tracers

   !!======================================================================
END MODULE par_cfc
