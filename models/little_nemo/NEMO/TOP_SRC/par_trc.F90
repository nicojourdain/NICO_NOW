MODULE par_trc
   !!======================================================================
   !!                        ***  par_trc  ***
   !! TOP :   set the passive tracers parameters
   !!======================================================================
   !! History :    -   !  1996-01  (M. Levy)  original code
   !!              -   !  1999-07  (M. Levy)  for LOBSTER1 or NPZD model
   !!              -   !  2000-04  (O. Aumont, M.A. Foujols)  HAMOCC3 and P3ZD
   !!             1.0  !  2004-03  (C. Ethe) Free form and module
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
   USE par_kind          ! kind parameters
   !
   USE par_lobster   ! LOBSTER model
   USE par_pisces    ! PISCES  model
   USE par_c14b      ! C14 bomb tracer
   USE par_cfc       ! CFC 11 and 12 tracers
   USE par_my_trc    ! user defined passive tracers

   IMPLICIT NONE

   ! Passive tracers : Total size
   ! ---------------               ! total number of passive tracers, of 2d and 3d output and trend arrays
   INTEGER, PUBLIC,  PARAMETER ::   jptra    =  jp_lobster    + jp_pisces     + jp_cfc     + jp_c14b    + jp_my_trc
   INTEGER, PUBLIC,  PARAMETER ::   jpdia2d  =  jp_lobster_2d + jp_pisces_2d  + jp_cfc_2d  + jp_c14b_2d + jp_my_trc_2d
   INTEGER, PUBLIC,  PARAMETER ::   jpdia3d  =  jp_lobster_3d + jp_pisces_3d  + jp_cfc_3d  + jp_c14b_3d + jp_my_trc_3d
   !                     ! total number of sms diagnostic arrays
   INTEGER, PUBLIC,  PARAMETER ::   jpdiabio = jp_lobster_trd + jp_pisces_trd + jp_cfc_trd + jp_c14b_trd + jp_my_trc_trd
   
   !  1D configuration ("key_c1d")
   ! -----------------
# if defined key_c1d
   LOGICAL, PUBLIC, PARAMETER ::   lk_trc_c1d   = .TRUE.   !: 1D pass. tracer configuration flag
# else   
   LOGICAL, PUBLIC, PARAMETER ::   lk_trc_c1d   = .FALSE.  !: 1D pass. tracer configuration flag
# endif

   REAL(wp), PUBLIC  :: rtrn  = 1.e-15      !: truncation value     

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: par_trc.F90 2528 2010-12-27 17:33:53Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE par_trc
