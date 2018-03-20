#if defined key_agrif
   !!----------------------------------------------------------------------
   !! NEMO/NST 3.3 , NEMO Consortium (2010)
   !! $Id: agrif2model.F90 2528 2010-12-27 17:33:53Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

   SUBROUTINE Agrif_Set_numberofcells(Agrif_Gr)
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Set_numberofcells ***
      !!--------------------------------------------- 
      USE Agrif_Types
      IMPLICIT NONE

      Type(Agrif_Grid), Pointer :: Agrif_Gr

      IF ( associated(Agrif_Curgrid) )THEN
#include "SetNumberofcells.h"
      ENDIF

   END SUBROUTINE Agrif_Set_numberofcells

   SUBROUTINE Agrif_Get_numberofcells(Agrif_Gr)
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Get_numberofcells ***
      !!--------------------------------------------- 
      USE Agrif_Types
      IMPLICIT NONE

      Type(Agrif_Grid), Pointer :: Agrif_Gr

#include "GetNumberofcells.h"

   END SUBROUTINE Agrif_Get_numberofcells

   SUBROUTINE Agrif_Allocationcalls(Agrif_Gr)
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Allocationscalls ***
      !!--------------------------------------------- 
      USE Agrif_Types 
#include "include_use_Alloc_agrif.h"
      IMPLICIT NONE

      Type(Agrif_Grid), Pointer :: Agrif_Gr

#include "allocations_calls_agrif.h"

   END SUBROUTINE Agrif_Allocationcalls

   SUBROUTINE Agrif_probdim_modtype_def()
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_probdim_modtype_def ***
      !!--------------------------------------------- 
      USE Agrif_Types
      IMPLICIT NONE

#include "modtype_agrif.h"
#include "probdim_agrif.h"
#include "keys_agrif.h"

      Return

   END SUBROUTINE Agrif_probdim_modtype_def

   SUBROUTINE Agrif_clustering_def()
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_clustering_def ***
      !!--------------------------------------------- 
      Use Agrif_Types
      IMPLICIT NONE

      Return

   END SUBROUTINE Agrif_clustering_def

   SUBROUTINE Agrif_comm_def(modelcomm)

      !!---------------------------------------------
      !!   *** ROUTINE Agrif_clustering_def ***
      !!--------------------------------------------- 
      Use Agrif_Types
      Use lib_mpp

      IMPLICIT NONE

      INTEGER :: modelcomm

#if defined key_mpp_mpi
      modelcomm = mpi_comm_opa
#endif
      Return

   END SUBROUTINE Agrif_comm_def
#else
   SUBROUTINE Agrif2Model
      !!---------------------------------------------
      !!   *** ROUTINE Agrif2Model ***
      !!--------------------------------------------- 
      WRITE(*,*) 'Impossible to bet here'
   END SUBROUTINE Agrif2model
#endif
