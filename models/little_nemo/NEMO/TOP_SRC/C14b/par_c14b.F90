MODULE par_c14b
   !!======================================================================
   !!                        ***  par_c14b ***
   !! TOP :   set the C14 bomb parameters
   !!======================================================================
   !! History :   2.0  !  2008-12  (C. Ethe, G. Madec)  revised architecture
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
   USE par_cfc    , ONLY : jp_cfc_2d       !: number of 2D diag in CFC
   USE par_cfc    , ONLY : jp_cfc_3d       !: number of 3D diag in CFC
   USE par_cfc    , ONLY : jp_cfc_trd      !: number of biological diag in CFC


   IMPLICIT NONE

   INTEGER, PARAMETER ::   jp_lb      = jp_lobster     + jp_pisces     + jp_cfc     !: cum. number of pass. tracers
   INTEGER, PARAMETER ::   jp_lb_2d   = jp_lobster_2d  + jp_pisces_2d  + jp_cfc_2d  !:
   INTEGER, PARAMETER ::   jp_lb_3d   = jp_lobster_3d  + jp_pisces_3d  + jp_cfc_3d  !:
   INTEGER, PARAMETER ::   jp_lb_trd  = jp_lobster_trd + jp_pisces_trd + jp_cfc_trd !:
   
#if defined key_c14b
   !!---------------------------------------------------------------------
   !!   'key_c14b'   :                                   C14 bomb tracer
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_c14b     = .TRUE.      !: C14 bomb flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b     =  1          !: number of passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b_2d  =  2          !: additional 2d output arrays ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b_3d  =  1          !: additional 3d output arrays ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b_trd =  0          !: number of sms trends for C14
   INTEGER, PUBLIC, PARAMETER ::   jpc14       = jp_lb + 1   !: assign an index in trc arrays for C14 bomb 
#else
   !!---------------------------------------------------------------------
   !!   Default     :                                       No C14 tracer
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_c14b     = .FALSE.     !: C14 bomb flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b     =  0          !: No C14 tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b_2d  =  0          !: No C14 additional 2d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b_3d  =  0          !: No C14 additional 3d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b_trd =  0          !: number of sms trends for C14
#endif

   ! Starting/ending C14 do-loop indices (N.B. no C14 : jp_c14b0 > jp_c14b1 the do-loop are never done)
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b0     = jp_lb     + 1            !: First index of C14 tracer
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b1     = jp_lb     + jp_c14b      !: Last  index of C14 tracer
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b0_2d  = jp_lb_2d  + 1            !: First index of C14 tracer
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b1_2d  = jp_lb_2d  + jp_c14b_2d   !: Last  index of C14 tracer
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b0_3d  = jp_lb_3d  + 1            !: First index of C14 tracer
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b1_3d  = jp_lb_3d  + jp_c14b_3d   !: Last  index of C14 tracer
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b0_trd = jp_lb_trd + 1            !: First index of C14 tracer
   INTEGER, PUBLIC, PARAMETER ::   jp_c14b1_trd = jp_lb_trd + jp_c14b_trd  !: Last  index of C14 tracer

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: par_c14b.F90 2715 2011-03-30 15:58:35Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE par_c14b
