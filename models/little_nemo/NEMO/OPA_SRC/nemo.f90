PROGRAM nemo
   !!======================================================================
   !!                     ***  PROGRAM nemo  ***
   !!
   !! ** Purpose :   encapsulate nemo_gcm so that it can also be called
   !!              together with the linear tangent and adjoint models
   !!======================================================================
   !! History :   OPA  ! 2001-02  (M. Imbard, A. Weaver)  Original code
   !!   NEMO      1.0  ! 2003-10  (G. Madec) F90
   !!----------------------------------------------------------------------
   USE nemogcm   ! NEMO system   (nemo_gcm routine)
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: nemo.f90 2528 2010-12-27 17:33:53Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   !
   CALL nemo_gcm           ! NEMO direct code
   ! 
   !!======================================================================
END PROGRAM nemo
