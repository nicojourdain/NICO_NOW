MODULE oasis4_date
   !!======================================================================
   !!                     ***  MODULE oasis_date  ***  
   !!   Date and related information required to couple NEMO via OASIS4
   !!   Made separate from cpl_oasis4 module to allow wider use.
   !!======================================================================
   !! History :  2.0  ! 2005-12  (R. Hill, Met. Office) Original code
   !!----------------------------------------------------------------------
#if defined key_oasis4
   !!----------------------------------------------------------------------
   !!   'key_oasis4'                                   coupled with OASIS-4
   !!----------------------------------------------------------------------
   !##################### WARNING coupled mode ###############################
   !##################### WARNING coupled mode ###############################
   !   Following line must be enabled if coupling with OASIS
   !   USE PRISM
   !##################### WARNING coupled mode ###############################
   !##################### WARNING coupled mode ###############################

   PRIVATE
   IMPLICIT NONE

   INTEGER, PUBLIC :: date_err, date_info   !: 

   TYPE(PRISM_Time_struct), PUBLIC ::   dates            !: date info for send operation
   TYPE(PRISM_Time_struct), PUBLIC ::   dates_bound(2)   !: date info for send operation
   TYPE(PRISM_Time_struct), PUBLIC ::   dater            !: date info for receive operation
   TYPE(PRISM_Time_struct), PUBLIC ::   dater_bound(2)   !: date info for receive operation
   TYPE(PRISM_Time_struct), PUBLIC ::   tmpdate          !:
   
#else
   !!----------------------------------------------------------------------
   !!   Default option            Dummy module                   NO OASIS-4
   !!----------------------------------------------------------------------
#endif
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: oasis4_date.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE oasis4_date
