MODULE trcnam_c14b
   !!======================================================================
   !!                         ***  MODULE trcnam_c14b  ***
   !! TOP :   initialisation of some run parameters for C14 chemical model
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec) from trcnam.cfc.h90
   !!----------------------------------------------------------------------
#if defined key_c14b
   !!----------------------------------------------------------------------
   !!   'key_c14b'                                         C14 bomb tracer
   !!----------------------------------------------------------------------
   !! trc_nam_c14b      : C14 model initialisation
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE par_trc         ! TOP parameters
   USE trc             ! TOP variables
   USE trcsms_c14b     ! C14b specific variable
   USE iom             ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_nam_c14b   ! called by trcnam.F90 module

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcnam_c14b.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_nam_c14b
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE trc_nam_c14b  ***
      !!                 
      !! ** Purpose :   Definition some run parameter for C14 model
      !!
      !! ** Method  :   Read the namc14 namelist and check the parameter 
      !!       values called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namelist_c14b
      !!----------------------------------------------------------------------
      INTEGER ::   numnatb

      ! definition of additional diagnostic as a structure
      INTEGER :: jl, jn
      TYPE(DIAG), DIMENSION(jp_c14b_2d) :: c14dia2d
      TYPE(DIAG), DIMENSION(jp_c14b_3d) :: c14dia3d
      !!
      NAMELIST/namc14date/ ndate_beg_b, nyear_res_b
      NAMELIST/namc14dia/  c14dia2d, c14dia3d     ! additional diagnostics
      !!-------------------------------------------------------------------

      ndate_beg_b = 650101            ! default namelist value
      nyear_res_b = 1955

      !                             ! Open namelist file
      CALL ctl_opn( numnatb, 'namelist_c14b', 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
         
      READ( numnatb , namc14date )     ! read namelist

      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) ' trc_nam: Read namdates, namelist for C14 chemical model'
         WRITE(numout,*) ' ~~~~~~~'
         WRITE(numout,*) '    initial calendar date (aammjj) for C14  ndate_beg_b = ', ndate_beg_b
         WRITE(numout,*) '    restoring time constant (year)          nyear_res_b = ', nyear_res_b
      ENDIF
      nyear_beg_b = ndate_beg_b / 10000
      IF(lwp) WRITE(numout,*) '    initial year (aa)                  nyear_beg_b = ', nyear_beg_b
      !
      IF( .NOT.lk_iomput .AND. ln_diatrc ) THEN
         !
         ! Namelist namc14dia
         ! -------------------
         DO jl = 1, jp_c14b_2d
            WRITE(c14dia2d(jl)%sname,'("2D_",I1)') jl                      ! short name
            WRITE(c14dia2d(jl)%lname,'("2D DIAGNOSTIC NUMBER ",I2)') jl    ! long name
            c14dia2d(jl)%units = ' '                                       ! units
         END DO
         !                                 ! 3D output arrays
         DO jl = 1, jp_c14b_3d
            WRITE(c14dia3d(jl)%sname,'("3D_",I1)') jl                      ! short name
            WRITE(c14dia3d(jl)%lname,'("3D DIAGNOSTIC NUMBER ",I2)') jl    ! long name
            c14dia3d(jl)%units = ' '                                       ! units
         END DO

         REWIND( numnatb )               ! 
         READ  ( numnatb, namc14dia )

         DO jl = 1, jp_c14b_2d
            jn = jp_c14b0_2d + jl - 1
            ctrc2d(jn) = c14dia2d(jl)%sname
            ctrc2l(jn) = c14dia2d(jl)%lname
            ctrc2u(jn) = c14dia2d(jl)%units
         END DO

         DO jl = 1, jp_c14b_3d
            jn = jp_c14b0_3d + jl - 1
            ctrc3d(jn) = c14dia3d(jl)%sname
            ctrc3l(jn) = c14dia3d(jl)%lname
            ctrc3u(jn) = c14dia3d(jl)%units
         END DO

         IF(lwp) THEN                   ! control print
            WRITE(numout,*)
            WRITE(numout,*) ' Namelist : natadd'
            DO jl = 1, jp_c14b_3d
               jn = jp_c14b0_3d + jl - 1
               WRITE(numout,*) '  3d diag nb : ', jn, '    short name : ', ctrc3d(jn), &
                 &             '  long name  : ', ctrc3l(jn), '   unit : ', ctrc3u(jn)
            END DO
            WRITE(numout,*) ' '

            DO jl = 1, jp_c14b_2d
               jn = jp_c14b0_2d + jl - 1
               WRITE(numout,*) '  2d diag nb : ', jn, '    short name : ', ctrc2d(jn), &
                 &             '  long name  : ', ctrc2l(jn), '   unit : ', ctrc2u(jn)
            END DO
            WRITE(numout,*) ' '
         ENDIF
         !
      ENDIF

   END SUBROUTINE trc_nam_c14b
   
#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                                No 14C
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_nam_c14b                      ! Empty routine
   END  SUBROUTINE  trc_nam_c14b
#endif  

   !!======================================================================
END MODULE trcnam_c14b
