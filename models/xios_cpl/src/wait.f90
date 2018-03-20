MODULE mod_wait
  INTEGER, SAVE :: wait_param=10
  REAL, SAVE    :: opt_param


  INTEGER, SAVE :: last_count

CONTAINS

  
  FUNCTION Top()
  IMPLICIT NONE
    DOUBLE PRECISION :: Top
    INTEGER :: count,count_rate,count_max
    LOGICAL, SAVE :: first=.TRUE.
    
      
    CALL system_clock(count,count_rate,count_max)
    IF (first) THEN
      Top=0.
    ELSE
      IF (Count>=Last_Count) THEN
        Top=(1.*(Count-last_Count))/count_rate
      ELSE
        Top=(1.*(Count-last_Count+Count_max))/count_rate
      ENDIF
    ENDIF
    Last_Count=Count
    first=.FALSE. 
  END FUNCTION Top
  
  SUBROUTINE Init_wait
  IMPLICIT NONE
    INTEGER :: i,j
    LOGICAL :: out_ok
    DOUBLE PRECISION :: time 
    INTEGER :: last_param
  
    out_ok=.FALSE.
    
    DO WHILE (.NOT. out_ok)
      opt_param=0.
    
      time=top()
!CDIR NOVECTOR
      DO i=1,1000000*wait_param
         opt_param=opt_param+(i/(i+opt_param))
      ENDDO
      time=top()
      last_param=wait_param
      wait_param=wait_param*(1./time)
      IF (ABS(wait_param-last_param)/(0.5*(wait_param+last_param)) <0.01) out_ok=.TRUE.
    END DO
  END SUBROUTINE Init_wait

  SUBROUTINE Wait_us(n)
  IMPLICIT NONE
  INTEGER :: n
  INTEGER :: i

!CDIR NOVECTOR  
    DO i=1,n*wait_param
         opt_param=opt_param+(i/(i+opt_param))
    ENDDO
      
  END SUBROUTINE Wait_us   

END MODULE mod_wait  
