MODULE index_sort

CONTAINS

   LOGICAL FUNCTION lessn(a,b,n)
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE lessn  ***
      !!          
      !! ** Purpose : Compare two array and return true if the first
      !!              element of array "a" different from the corresponding 
      !!              array "b" element is less than the this element
      !!
      !! ** Method  : 
      !!
      !! ** Action  : 
      !!
      !! References : 
      !!
      !! History :
      !!        !  08-02  (K. Mogensen)  Original code
      !!----------------------------------------------------------------------
      !! * Arguments
      USE toolspar_kind
      IMPLICIT NONE
      INTEGER :: n
      REAL(KIND=dp), DIMENSION(n) :: a,b
      INTEGER :: i,j

      DO i=1,n
         IF (a(i)/=b(i)) THEN
            IF (a(i)<b(i)) THEN
               lessn=.TRUE.
            ELSE
               lessn=.FALSE.
            ENDIF
            EXIT
         ENDIF
      ENDDO

   END FUNCTION lessn

   SUBROUTINE  index_sort_dp_n(pval, n, kindx, kvals)
      USE toolspar_kind
      IMPLICIT NONE
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE index_sort  ***
      !!          
      !! ** Purpose : Get indicies for ascending order for a
      !!              double precision array 2D
      !!
      !! ** Method  : Heapsort with call to lessn for comparision
      !!
      !! ** Action  : 
      !!
      !! References : http://en.wikipedia.org/wiki/Heapsort
      !!
      !! History :
      !!        !  08-02  (K. Mogensen)  Original code based on index_sort_dp
      !!----------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT(IN) :: &
         & n                               ! Number of keys
      INTEGER, INTENT(IN) :: &
         & kvals                           ! Number of values
      REAL(KIND=dp),DIMENSION(n,kvals),INTENT(IN) :: &
         & pval                            ! Array to be sorted
      INTEGER,DIMENSION(kvals),INTENT(INOUT) :: &
         & kindx                           ! Indicies for ordering
      !! * Local variables
      INTEGER :: ji, jj, jt, jn, jparent, jchild

      DO ji = 1, kvals
         kindx(ji) = ji
      END DO

      IF (kvals > 1) THEN

         ji = kvals/2 + 1
         jn = kvals

         main_loop : DO

            IF ( ji > 1 ) THEN
               ji = ji-1
               jt = kindx(ji)
            ELSE
               jt = kindx(jn)
               kindx(jn) = kindx(1)
               jn = jn-1
               IF ( jn == 1 ) THEN
                  kindx(1) = jt
                  EXIT main_loop
               ENDIF
            ENDIF

            jparent = ji
            jchild =  2*ji

            inner_loop : DO
               IF ( jchild > jn ) EXIT inner_loop
               IF ( jchild < jn ) THEN
                  IF ( lessn(pval(:,kindx(jchild)),pval(:,kindx(jchild+1)),n) ) THEN
                     jchild = jchild+1
                  ENDIF
               ENDIF
               IF  ( lessn(pval(:,jt),pval(:,kindx(jchild)),n) ) THEN
                  kindx(jparent) = kindx(jchild)
                  jparent = jchild
                  jchild = jchild*2
               ELSE 
                  jchild = jn + 1 
               ENDIF
            ENDDO inner_loop

            kindx(jparent) = jt

         END DO  main_loop
      ENDIF

   END SUBROUTINE index_sort_dp_n

   SUBROUTINE  index_sort_dp(pval, kindx, kvals)
      USE toolspar_kind
      IMPLICIT NONE
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE index_sort  ***
      !!          
      !! ** Purpose : Get indicies for ascending order for a
      !!              double precision array
      !!
      !! ** Method  : Heapsort
      !!
      !! ** Action  : 
      !!
      !! References : http://en.wikipedia.org/wiki/Heapsort
      !!
      !! History :
      !!        !  06-05  (K. Mogensen)  Original code
      !!----------------------------------------------------------------------
      !! * Arguments
      REAL(KIND=dp),DIMENSION(*),INTENT(IN) :: &
         & pval                            ! Array to be sorted
      INTEGER,DIMENSION(*),INTENT(INOUT) :: &
         & kindx                           ! Indicies for ordering
      INTEGER, INTENT(IN) :: &
         & kvals                           ! Number of values

      !! * Local variables
      INTEGER :: ji, jj, jt, jn, jparent, jchild

      DO ji = 1, kvals
         kindx(ji) = ji
      END DO

      IF (kvals > 1) THEN

         ji = kvals/2 + 1
         jn = kvals

         main_loop : DO

            IF ( ji > 1 ) THEN
               ji = ji-1
               jt = kindx(ji)
            ELSE
               jt = kindx(jn)
               kindx(jn) = kindx(1)
               jn = jn-1
               IF ( jn == 1 ) THEN
                  kindx(1) = jt
                  EXIT main_loop
               ENDIF
            ENDIF

            jparent = ji
            jchild =  2*ji

            inner_loop : DO
               IF ( jchild > jn ) EXIT inner_loop
               IF ( jchild < jn ) THEN
                  IF ( pval(kindx(jchild)) <  pval(kindx(jchild+1)) ) THEN
                     jchild = jchild+1
                  ENDIF
               ENDIF
               IF  ( pval(jt) < pval(kindx(jchild))) THEN
                  kindx(jparent) = kindx(jchild)
                  jparent = jchild
                  jchild = jchild*2
               ELSE 
                  jchild = jn + 1 
               ENDIF
            ENDDO inner_loop

            kindx(jparent) = jt

         END DO  main_loop

      ENDIF
   END SUBROUTINE index_sort_dp

   SUBROUTINE  index_sort_int(kval, kindx, kvals)
      USE toolspar_kind
      IMPLICIT NONE
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE index_sort  ***
      !!          
      !! ** Purpose : Get indicies for ascending order for an
      !!              integer array
      !!
      !! ** Method  : Heapsort
      !!
      !! ** Action  : 
      !!
      !! References : http://en.wikipedia.org/wiki/Heapsort
      !!
      !! History :
      !!        !  06-05  (K. Mogensen)  Original code
      !!----------------------------------------------------------------------
      !! * Arguments
      INTEGER,DIMENSION(*),INTENT(IN) :: &
         & kval                            ! Array to be sorted
      INTEGER,DIMENSION(*),INTENT(INOUT) :: &
         & kindx                           ! Indicies for ordering
      INTEGER, INTENT(IN) :: &
         & kvals                           ! Number of values

      !! * Local variables
      INTEGER :: ji, jj, jt, jn, jparent, jchild

      DO ji = 1, kvals
         kindx(ji) = ji
      END DO

      IF (kvals > 1 ) THEN

         ji = kvals/2 + 1
         jn = kvals

         main_loop : DO

            IF ( ji > 1 ) THEN
               ji = ji-1
               jt = kindx(ji)
            ELSE
               jt = kindx(jn)
               kindx(jn) = kindx(1)
               jn = jn-1
               IF ( jn == 1 ) THEN
                  kindx(1) = jt
                  EXIT main_loop
               ENDIF
            ENDIF

            jparent = ji
            jchild =  2*ji

            inner_loop : DO
               IF ( jchild > jn ) EXIT inner_loop
               IF ( jchild < jn ) THEN
                  IF ( kval(kindx(jchild)) <  kval(kindx(jchild+1)) ) THEN
                     jchild = jchild+1
                  ENDIF
               ENDIF
               IF  ( kval(jt) < kval(kindx(jchild))) THEN
                  kindx(jparent) = kindx(jchild)
                  jparent = jchild
                  jchild = jchild*2
               ELSE 
                  jchild = jn + 1 
               ENDIF
            ENDDO inner_loop

            kindx(jparent) = jt

         END DO  main_loop

      ENDIF

   END SUBROUTINE index_sort_int

   SUBROUTINE  index_sort_string(cdval, kindx, kvals)
      USE toolspar_kind
      IMPLICIT NONE
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE index_sort  ***
      !!          
      !! ** Purpose : Get indicies for ascending order for an
      !!              integer array
      !!
      !! ** Method  : Heapsort
      !!
      !! ** Action  : 
      !!
      !! References : http://en.wikipedia.org/wiki/Heapsort
      !!
      !! History :
      !!        !  06-05  (K. Mogensen)  Original code
      !!----------------------------------------------------------------------
      !! * Arguments
      CHARACTER(len=*),DIMENSION(*),INTENT(IN) :: &
         & cdval                            ! Array to be sorted
      INTEGER,DIMENSION(*),INTENT(INOUT) :: &
         & kindx                           ! Indicies for ordering
      INTEGER, INTENT(IN) :: &
         & kvals                           ! Number of values

      !! * Local variables
      INTEGER :: ji, jj, jt, jn, jparent, jchild

      DO ji = 1, kvals
         kindx(ji) = ji
      END DO

      IF (kvals > 1 ) THEN

         ji = kvals/2 + 1
         jn = kvals

         main_loop : DO

            IF ( ji > 1 ) THEN
               ji = ji-1
               jt = kindx(ji)
            ELSE
               jt = kindx(jn)
               kindx(jn) = kindx(1)
               jn = jn-1
               IF ( jn == 1 ) THEN
                  kindx(1) = jt
                  EXIT main_loop
               ENDIF
            ENDIF

            jparent = ji
            jchild =  2*ji

            inner_loop : DO
               IF ( jchild > jn ) EXIT inner_loop
               IF ( jchild < jn ) THEN
                  IF ( cdval(kindx(jchild)) <  cdval(kindx(jchild+1)) ) THEN
                     jchild = jchild+1
                  ENDIF
               ENDIF
               IF  ( cdval(jt) < cdval(kindx(jchild))) THEN
                  kindx(jparent) = kindx(jchild)
                  jparent = jchild
                  jchild = jchild*2
               ELSE 
                  jchild = jn + 1 
               ENDIF
            ENDDO inner_loop

            kindx(jparent) = jt

         END DO  main_loop

      ENDIF

   END SUBROUTINE index_sort_string

END MODULE index_sort
