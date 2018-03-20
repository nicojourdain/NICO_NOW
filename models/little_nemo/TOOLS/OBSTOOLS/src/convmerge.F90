MODULE convmerge

   USE toolspar_kind
   USE obs_fbm
   USE obs_utils
   IMPLICIT NONE

CONTAINS

   SUBROUTINE conv_fbmerge( cdoutfile, nfiles, fbdata )
      !!---------------------------------------------------------------------
      !!
      !!                     ** ROUTINE conv_fbmerg **
      !!
      !!  ** Purpose : Merge all fbfiles into a single fbfile
      !!
      !!  ** Method  : Use of utilities from obs_fbm.
      !!
      !!  ** Action  :
      !!
      !!   Optional :
      !!     namelist = namobs.in    to select the observation range 
      !!
      !!   History :
      !!        ! 2010 (K. Mogensen) Initial version
      !!----------------------------------------------------------------------
      !! * Arguments
      CHARACTER(LEN=*) :: cdoutfile                ! Input file.
      INTEGER :: nfiles                            ! Number of files
      TYPE(obfbdata), dimension(nfiles) :: fbdata  ! Structure to merge
      !! * Local variables
      type(obfbdata) :: fbmerge
      INTEGER,ALLOCATABLE  :: iset(:),inum(:),iindex(:)
      INTEGER :: nmaxlev
      INTEGER :: ia,ij,ii
      REAL(fbdp), DIMENSION(nfiles) :: djulini, djulend
      CHARACTER(len=8) :: cl_refdate
      INTEGER :: irefdate,iyea,imon,iday,ihou,imin,isec
      ! Namelist variables
      CHARACTER(len=9) :: cdnamefile='namobs.in'
      LOGICAL :: lexists 
      REAL(fbdp) :: dobsini,dobsend
      NAMELIST/namobs/dobsini,dobsend
      
      dobsini = 0.0
      dobsend = 99991231.235959

      INQUIRE(file=cdnamefile, exist=lexists)
      IF (lexists) THEN
         OPEN(10,file=cdnamefile)
         READ(10,namobs)
         WRITE(*,namobs)
         CLOSE(10)
      ENDIF
      !
      ! Count number of data points
      !
      nmaxlev = 1
      ii = 0
      DO ia = 1,nfiles
         IF (lexists) THEN
            cl_refdate=fbdata(ia)%cdjuldref(1:8)
            READ(cl_refdate,'(I8)') irefdate
            CALL ddatetoymdhms( dobsini, iyea, imon, iday, ihou, imin, isec )
            CALL greg2jul( isec, imin, ihou, iday, imon, iyea, djulini(ia), &
               &           krefdate = irefdate )
            CALL ddatetoymdhms( dobsend, iyea, imon, iday, ihou, imin, isec )
            CALL greg2jul( isec, imin, ihou, iday, imon, iyea, djulend(ia), &
            &           krefdate = irefdate )
            DO ij = 1, fbdata(ia)%nobs
               IF ( ( fbdata(ia)%ptim(ij) >  djulini(ia) ) .AND. &
                  & ( fbdata(ia)%ptim(ij) <= djulend(ia) ) ) THEN
                  ii = ii + 1
                  nmaxlev = MAX(nmaxlev,fbdata(ia)%nlev)
               ENDIF
            ENDDO
         ELSE
            nmaxlev = MAX(nmaxlev,fbdata(ia)%nlev)
            ii = ii + fbdata(ia)%nobs
         ENDIF
      ENDDO
      !
      ! Merge the input structures into the output structure
      !
      ALLOCATE( iset(ii), inum(ii), iindex(ii))
      ii = 0
      DO ia = 1,nfiles
         DO ij = 1, fbdata(ia)%nobs
            IF (lexists) THEN
               IF ( ( fbdata(ia)%ptim(ij) >  djulini(ia) ) .AND. &
                  & ( fbdata(ia)%ptim(ij) <= djulend(ia) ) ) THEN
                  ii = ii + 1
                  iset(ii)   = ia
                  inum(ii)   = ij
                  iindex(ii) = ii
               ENDIF
            ELSE
               ii = ii + 1
               iset(ii)   = ia
               inum(ii)   = ij
               iindex(ii) = ii
            ENDIF
         ENDDO
      ENDDO
      WRITE(*,*)'Output number of observations = ',ii
      WRITE(*,*)'Output number of levels       = ',nmaxlev
      !
      ! Prepare fbmerge structure
      !
      CALL init_obfbdata( fbmerge )
      CALL alloc_obfbdata( fbmerge, fbdata(1)%nvar, ii, nmaxlev, &
         &                 fbdata(1)%nadd, fbdata(1)%next, fbdata(1)%lgrid )
      CALL merge_obfbdata( nfiles, fbdata, fbmerge, iset, inum, iindex )
      !
      ! Write the file
      !
      CALL write_obfbdata( TRIM(cdoutfile), fbmerge )
      !
      ! Dellocate the data
      !
      CALL dealloc_obfbdata( fbmerge )

   END SUBROUTINE conv_fbmerge

#include "ctl_stop.h90"

#include "greg2jul.h90"

!#include "ddatetoymdhms.h90"

END MODULE convmerge
