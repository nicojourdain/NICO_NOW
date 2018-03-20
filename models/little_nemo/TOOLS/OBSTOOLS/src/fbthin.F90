PROGRAM fbthin
   !!---------------------------------------------------------------------
   !!
   !!                     ** PROGRAM fbthin **
   !!
   !!  ** Purpose : Thin the data to 1 degree resolution
   !!
   !!  ** Method  : Use of utilities from obs_fbm.
   !!
   !!  ** Action  :
   !!
   !!   Usage:
   !!     fbthin.exe inputfile outputfile
   !!
   !!   Required:
   !!     namelist = namthin.in
   !!
   !!   History :
   !!        ! 2010 (K. Mogensen) Initial version
   !!----------------------------------------------------------------------
   USE obs_fbm
   IMPLICIT NONE
   !
   ! Command line arguments for output file and input file
   !
#ifndef NOIARGCPROTO
   INTEGER,EXTERNAL :: iargc
#endif
   INTEGER :: nargs
   CHARACTER(len=256) :: cdoutfile
   CHARACTER(len=256) :: cdinfile
   CHARACTER(len=256) :: cdtmp
   INTEGER :: nout,ninn,nadd,next,i,j,k
   LOGICAL :: lgrid
   !
   ! Feedback data
   !
   TYPE(obfbdata) :: fbdatain
   !
   ! Get number of command line arguments
   !
   nargs=IARGC()
   IF ((nargs /= 2)) THEN
      WRITE(*,'(A)')'Usage:'
      WRITE(*,'(A)')'fbthin inputfile outputfile'
      CALL abort()
   ENDIF
   CALL getarg(1,cdinfile)
   CALL getarg(2,cdoutfile)
   !
   ! Initialize feedback data
   !
   CALL init_obfbdata( fbdatain )
   !
   ! Read the file
   !
   CALL read_obfbdata( TRIM(cdinfile), fbdatain )
   !
   ! Do the thining
   !
   CALL fb_thin( fbdatain )
   !
   ! Write the file
   !
   CALL write_obfbdata( TRIM(cdoutfile), fbdatain )

CONTAINS
   
   SUBROUTINE fb_thin( fbdata )
      !
      ! Observation thinning 
      !
      IMPLICIT NONE
      TYPE(obfbdata) :: fbdata
      ! Namelist parameters
      INTEGER, PARAMETER :: nmaxtypes = 10
      CHARACTER(len=ilentyp), DIMENSION(nmaxtypes) :: thintypes
      REAL, DIMENSION(nmaxtypes) :: thindists, thindtime
      ! Local variables
      NAMELIST/namthin/thintypes, thindists, thindtime
      INTEGER :: it,ii,ij,iv,iobs,irej
      REAL :: zdist

      ! Get namelist
      thintypes(:) = 'XXXX'
      ! Distance in km
      thindists(:) = 100.0
      ! Time difference in days
      thindtime(:) = 0.99999999
      OPEN(10,file='namthin.in')
      READ(10,namthin)
      CLOSE(10)
      WRITE(*,namthin)
      
      ! Convert to meters
      thindists(:) = thindists(:) * 1000.0

      DO it = 1, nmaxtypes

         IF ( TRIM(thintypes(it)) == 'XXXX' ) CYCLE

         iobs = 0 
         irej = 0

         master_loop: DO ii= 1, fbdata%nobs 
            
            IF ( TRIM(ADJUSTL(thintypes(it))) /= 'all' ) THEN
               IF ( TRIM(ADJUSTL(fbdata%cdtyp(ii))) /= &
                  & TRIM(ADJUSTL(thintypes(it))) ) CYCLE
            ENDIF
            
            iobs = iobs + 1

            ! Skip data with missing lon and lat and observation flag rejected.

            IF (fbdata%plam(ii)==fbrmdi) CYCLE
            IF (fbdata%pphi(ii)==fbrmdi) CYCLE
            IF (fbdata%ioqc(ii)>2) CYCLE
            
            DO ij=ii+1, fbdata%nobs

               ! Skip data with missing lon and lat and observation flag rejected.
               
               IF (fbdata%plam(ij)==fbrmdi) CYCLE
               IF (fbdata%pphi(ij)==fbrmdi) CYCLE
               IF (fbdata%ioqc(ij)>2) CYCLE
               
               ! Skip different type unless thintypes is 'all'

               IF ( TRIM(ADJUSTL(thintypes(it))) /= 'all' ) THEN
                  IF ( TRIM(ADJUSTL(fbdata%cdtyp(ij))) /= &
                     & TRIM(ADJUSTL(thintypes(it))) ) CYCLE
               ENDIF

               IF ( ABS( fbdata%ptim(ij) - fbdata%ptim(ii) ) &
                  & >= thindtime(it) ) CYCLE

               zdist = distance( fbdata%plam(ii), fbdata%pphi(ii), &
                  &              fbdata%plam(ij), fbdata%pphi(ij) )

               IF ( zdist < thindists(it) ) THEN

                  irej = irej + 1
                  fbdata%ioqc(ij)    = 4
                  fbdata%ioqcf(2,ij) = fbdata%ioqcf(2,ij) + 32

               ENDIF
            ENDDO
            
         ENDDO master_loop

         WRITE(*,*)'For type                = ',TRIM(thintypes(it))
         WRITE(*,*)'Observations considered = ',iobs
         WRITE(*,*)'Observations rejected   = ',irej

      ENDDO


     
   END SUBROUTINE fb_thin

#include "distance.h90"

END PROGRAM fbthin
