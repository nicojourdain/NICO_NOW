MODULE par_my_trc
   !!======================================================================
   !!                        ***  par_my_trc  ***
   !! TOP :   set the MY_TRC parameters
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: par_my_trc.F90 2528 2010-12-27 17:33:53Z rblod $ 
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

   USE par_cfc    , ONLY : jp_cfc          !: number of tracers in CFC
   USE par_cfc    , ONLY : jp_cfc_2d       !: number of tracers in CFC
   USE par_cfc    , ONLY : jp_cfc_3d       !: number of tracers in CFC
   USE par_cfc    , ONLY : jp_cfc_trd      !: number of tracers in CFC

   USE par_c14b   , ONLY : jp_c14b         !: number of tracers in C14
   USE par_c14b   , ONLY : jp_c14b_2d      !: number of tracers in C14
   USE par_c14b   , ONLY : jp_c14b_3d      !: number of tracers in C14
   USE par_c14b   , ONLY : jp_c14b_trd     !: number of tracers in C14

   IMPLICIT NONE

   INTEGER, PARAMETER ::   jp_lm      = jp_lobster     + jp_pisces     + jp_cfc     + jp_c14b     !: 
   INTEGER, PARAMETER ::   jp_lm_2d   = jp_lobster_2d  + jp_pisces_2d  + jp_cfc_2d  + jp_c14b_2d  !:
   INTEGER, PARAMETER ::   jp_lm_3d   = jp_lobster_3d  + jp_pisces_3d  + jp_cfc_3d  + jp_c14b_3d  !:
   INTEGER, PARAMETER ::   jp_lm_trd  = jp_lobster_trd + jp_pisces_trd + jp_cfc_trd + jp_c14b_trd !:

#if defined key_my_trc
   !!---------------------------------------------------------------------
   !!   'key_my_trc'                     user defined tracers (MY_TRC)
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_my_trc     = .TRUE.   !: PTS flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc     =  2       !: number of PTS tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc_2d  =  0       !: additional 2d output arrays ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc_3d  =  0       !: additional 3d output arrays ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc_trd =  0       !: number of sms trends for MY_TRC

   ! assign an index in trc arrays for each PTS prognostic variables
   INTEGER, PUBLIC, PARAMETER ::   jpmyt1 = jp_lm + 1     !: 1st MY_TRC tracer
   INTEGER, PUBLIC, PARAMETER ::   jpmyt2 = jp_lm + 2     !: 2nd MY_TRC tracer

#else
   !!---------------------------------------------------------------------
   !!   Default                           No user defined tracers (MY_TRC)
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_my_trc     = .FALSE.  !: MY_TRC flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc     =  0       !: No MY_TRC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc_2d  =  0       !: No MY_TRC additional 2d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc_3d  =  0       !: No MY_TRC additional 3d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_my_trc_trd =  0       !: number of sms trends for MY_TRC
#endif

   ! Starting/ending PISCES do-loop indices (N.B. no PISCES : jpl_pcs < jpf_pcs the do-loop are never done)
   INTEGER, PUBLIC, PARAMETER ::   jp_myt0     = jp_lm     + 1              !: First index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt1     = jp_lm     + jp_my_trc      !: Last  index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt0_2d  = jp_lm_2d  + 1              !: First index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt1_2d  = jp_lm_2d  + jp_my_trc_2d   !: Last  index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt0_3d  = jp_lm_3d  + 1              !: First index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt1_3d  = jp_lm_3d  + jp_my_trc_3d   !: Last  index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt0_trd = jp_lm_trd + 1              !: First index of MY_TRC passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_myt1_trd = jp_lm_trd + jp_my_trc_trd  !: Last  index of MY_TRC passive tracers

   !!======================================================================
END MODULE par_my_trc
