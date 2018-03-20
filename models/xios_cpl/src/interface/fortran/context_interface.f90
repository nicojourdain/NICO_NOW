MODULE CONTEXT_INTERFACE
   USE, INTRINSIC :: ISO_C_BINDING
     
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
     
      SUBROUTINE cxios_context_handle_create(ret, idt, idt_size) BIND(C)
         import C_CHAR, C_INTPTR_T, C_INT
         INTEGER  (kind = C_INTPTR_T)               :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE cxios_context_handle_create
      
      SUBROUTINE cxios_context_set_current(context, withswap) BIND(C)
         import C_BOOL, C_INT, C_INTPTR_T
         INTEGER (kind = C_INTPTR_T), VALUE :: context
         LOGICAL (kind = C_BOOL)    , VALUE :: withswap
      END SUBROUTINE cxios_context_set_current

      SUBROUTINE cxios_context_valid_id(ret, idt, idt_size) BIND(C)
         USE ISO_C_BINDING
         LOGICAL  (kind = C_BOOL)                   :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE cxios_context_valid_id

   END INTERFACE
     
END MODULE CONTEXT_INTERFACE
