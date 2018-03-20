MODULE proftools

   ! Various tools to manipulate fb profiles data.

   USE obs_fbm
   IMPLICIT NONE

CONTAINS

   SUBROUTINE sealsfromargo( fbdata )

      ! Separate seals  from argo

      ! Notes:
      ! Argo is type 831
      ! Seals have station ids from Q9900000 to Q9900328
      ! according to
      ! http://www.nodc.noaa.gov/GTSPP/document/codetbls/shipcode/name_1-C.html
      ! on 2010-04-28 but we assume that all Q990???? are seals
      
      TYPE(obfbdata) :: fbdata
      CHARACTER(len=ilentyp) :: cdtyp
      INTEGER :: i

      DO i=1,fbdata%nobs
         
         cdtyp=ADJUSTL(fbdata%cdtyp(i))
         IF ((cdtyp(1:3)=='831') .AND. &
            &(fbdata%cdwmo(i)(1:4)=='Q990')) fbdata%cdtyp(i)='Seal'
      ENDDO

   END SUBROUTINE sealsfromargo

END MODULE proftools
