PROGRAM Hello

USE Hello_Constants, ONLY: hello_string

IMPLICIT NONE

#if defined(CALL_HELLO_SUB)
INCLUDE 'hello_sub.interface'
#endif
INCLUDE 'hello_sub2.interface'

CHARACTER (LEN=*), PARAMETER :: this = 'Hello'

WRITE (*, '(A)') this // ': ' // TRIM (hello_string)
#if defined(CALL_HELLO_SUB)
CALL Hello_Sub (HUGE(0))
#endif
CALL Hello_Sub2 ()

END PROGRAM Hello
