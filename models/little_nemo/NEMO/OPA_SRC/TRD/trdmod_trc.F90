MODULE trdmod_trc
   !!======================================================================
   !!                       ***  MODULE trdmod_trc  ***
   !!  Dummy module
   !!======================================================================
   !!----------------------------------------------------------------------
   !!   Dummy module                                             NO TOP use
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trd_mod_trc( ptrtrd, kjn, ktrd, kt )
      INTEGER ::   kt, kjn, ktrd   
      REAL    ::   ptrtrd(:,:,:)  
      WRITE(*,*) 'trd_mod_trc_trp : You should not have seen this print! error?', ptrtrd(1,1,1)
      WRITE(*,*) '  "      "      : You should not have seen this print! error?', kjn, ktrd, kt
   END SUBROUTINE trd_mod_trc

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: trdmod_trc.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE trdmod_trc
