MODULE par_pisces
   !!======================================================================
   !!                        ***  par_pisces  ***
   !! TOP :   set the PISCES parameters
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: par_pisces.F90 3295 2012-01-30 15:49:07Z cetlod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   USE par_lobster, ONLY : jp_lobster      !: number of tracers in LOBSTER
   USE par_lobster, ONLY : jp_lobster_2d   !: number of 2D diag in LOBSTER
   USE par_lobster, ONLY : jp_lobster_3d   !: number of 3D diag in LOBSTER
   USE par_lobster, ONLY : jp_lobster_trd  !: number of biological diag in LOBSTER

   IMPLICIT NONE

   INTEGER, PUBLIC, PARAMETER ::   jp_lp      = jp_lobster      !: cumulative number of already defined TRC
   INTEGER, PUBLIC, PARAMETER ::   jp_lp_2d   = jp_lobster_2d   !:
   INTEGER, PUBLIC, PARAMETER ::   jp_lp_3d   = jp_lobster_3d   !:
   INTEGER, PUBLIC, PARAMETER ::   jp_lp_trd  = jp_lobster_trd  !:

#if defined key_pisces  &&  defined key_kriest
   !!---------------------------------------------------------------------
   !!   'key_pisces' & 'key_kriest'                 PISCES bio-model + ???
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_pisces     = .TRUE.  !: PISCES flag 
   LOGICAL, PUBLIC, PARAMETER ::   lk_kriest     = .TRUE.  !: Kriest flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces     =  23     !: number of passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_2d  =  13     !: additional 2d output 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_3d  =  18     !: additional 3d output 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_trd =   1     !: number of sms trends for PISCES

   ! assign an index in trc arrays for each LOBSTER prognostic variables
   !    WARNING: be carefull about the order when reading the restart
        !   !!gm  this warning should be obsolet with IOM
   INTEGER, PUBLIC, PARAMETER ::   jpdic = jp_lp +  1    !: dissolved inoganic carbon concentration 
   INTEGER, PUBLIC, PARAMETER ::   jptal = jp_lp +  2    !: total alkalinity 
   INTEGER, PUBLIC, PARAMETER ::   jpoxy = jp_lp +  3    !: oxygen carbon concentration 
   INTEGER, PUBLIC, PARAMETER ::   jpcal = jp_lp +  4    !: calcite  concentration 
   INTEGER, PUBLIC, PARAMETER ::   jppo4 = jp_lp +  5    !: phosphate concentration 
   INTEGER, PUBLIC, PARAMETER ::   jppoc = jp_lp +  6    !: small particulate organic phosphate concentration
   INTEGER, PUBLIC, PARAMETER ::   jpsil = jp_lp +  7    !: silicate concentration
   INTEGER, PUBLIC, PARAMETER ::   jpphy = jp_lp +  8    !: phytoplancton concentration 
   INTEGER, PUBLIC, PARAMETER ::   jpzoo = jp_lp +  9    !: zooplancton concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdoc = jp_lp + 10    !: dissolved organic carbon concentration 
   INTEGER, PUBLIC, PARAMETER ::   jpdia = jp_lp + 11    !: Diatoms Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpmes = jp_lp + 12    !: Mesozooplankton Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdsi = jp_lp + 13    !: (big) Silicate Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpfer = jp_lp + 14    !: Iron Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnum = jp_lp + 15    !: Big iron particles Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpsfe = jp_lp + 16    !: number of particulate organic phosphate concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdfe = jp_lp + 17    !: Diatoms iron Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpgsi = jp_lp + 18    !: Diatoms Silicate Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnfe = jp_lp + 19    !: Nano iron Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnch = jp_lp + 20    !: Nano Chlorophyll Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdch = jp_lp + 21    !: Diatoms Chlorophyll Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpno3 = jp_lp + 22    !: Nitrates Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnh4 = jp_lp + 23    !: Ammonium Concentration

#elif defined key_pisces
   !!---------------------------------------------------------------------
   !!   'key_pisces'   :                         standard PISCES bio-model
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_pisces     = .TRUE.  !: PISCES flag 
   LOGICAL, PUBLIC, PARAMETER ::   lk_kriest     = .FALSE. !: Kriest flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces     = 24      !: number of PISCES passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_2d  = 13      !: additional 2d output 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_3d  = 11      !: additional 3d output 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_trd =  1      !: number of sms trends for PISCES

   ! assign an index in trc arrays for each LOBSTER prognostic variables
   !    WARNING: be carefull about the order when reading the restart
        !   !!gm  this warning should be obsolet with IOM
   INTEGER, PUBLIC, PARAMETER ::   jpdic = jp_lp +  1    !: dissolved inoganic carbon concentration 
   INTEGER, PUBLIC, PARAMETER ::   jptal = jp_lp +  2    !: total alkalinity 
   INTEGER, PUBLIC, PARAMETER ::   jpoxy = jp_lp +  3    !: oxygen carbon concentration 
   INTEGER, PUBLIC, PARAMETER ::   jpcal = jp_lp +  4    !: calcite  concentration 
   INTEGER, PUBLIC, PARAMETER ::   jppo4 = jp_lp +  5    !: phosphate concentration 
   INTEGER, PUBLIC, PARAMETER ::   jppoc = jp_lp +  6    !: small particulate organic phosphate concentration
   INTEGER, PUBLIC, PARAMETER ::   jpsil = jp_lp +  7    !: silicate concentration
   INTEGER, PUBLIC, PARAMETER ::   jpphy = jp_lp +  8    !: phytoplancton concentration 
   INTEGER, PUBLIC, PARAMETER ::   jpzoo = jp_lp +  9    !: zooplancton concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdoc = jp_lp + 10    !: dissolved organic carbon concentration 
   INTEGER, PUBLIC, PARAMETER ::   jpdia = jp_lp + 11    !: Diatoms Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpmes = jp_lp + 12    !: Mesozooplankton Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdsi = jp_lp + 13    !: (big) Silicate Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpfer = jp_lp + 14    !: Iron Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpbfe = jp_lp + 15    !: Big iron particles Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpgoc = jp_lp + 16    !: big particulate organic phosphate concentration
   INTEGER, PUBLIC, PARAMETER ::   jpsfe = jp_lp + 17    !: Small iron particles Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdfe = jp_lp + 18    !: Diatoms iron Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpgsi = jp_lp + 19    !: Diatoms Silicate Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnfe = jp_lp + 20    !: Nano iron Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnch = jp_lp + 21    !: Nano Chlorophyll Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdch = jp_lp + 22    !: Diatoms Chlorophyll Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpno3 = jp_lp + 23    !: Nitrates Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnh4 = jp_lp + 24    !: Ammonium Concentration

#else
   !!---------------------------------------------------------------------
   !!   Default                                   No CFC geochemical model
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_pisces     = .FALSE.  !: CFC flag 
   LOGICAL, PUBLIC, PARAMETER ::   lk_kriest     = .FALSE.  !: Kriest flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces     =  0       !: No CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_2d  =  0       !: No CFC additional 2d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_3d  =  0       !: No CFC additional 3d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_trd =  0       !: number of sms trends for PISCES
#endif

   ! Starting/ending PISCES do-loop indices (N.B. no PISCES : jpl_pcs < jpf_pcs the do-loop are never done)
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs0     = jp_lp + 1                  !: First index of PISCES tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs1     = jp_lp + jp_pisces          !: Last  index of PISCES tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs0_2d  = jp_lp_2d + 1               !: First index of 2D diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs1_2d  = jp_lp_2d + jp_pisces_2d    !: Last  index of 2D diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs0_3d  = jp_lp_3d + 1               !: First index of 3D diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs1_3d  = jp_lp_3d + jp_pisces_3d    !: Last  index of 3d diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs0_trd = jp_lp_trd + 1              !: First index of bio diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs1_trd = jp_lp_trd + jp_pisces_trd  !: Last  index of bio diag


   !!======================================================================
END MODULE par_pisces
