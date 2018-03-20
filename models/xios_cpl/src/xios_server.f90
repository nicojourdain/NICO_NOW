PROGRAM server_main
  USE xios
  IMPLICIT NONE
  INCLUDE "mpif.h"
  INTEGER :: ierr
  
    CALL xios_init_server

  END PROGRAM server_main
