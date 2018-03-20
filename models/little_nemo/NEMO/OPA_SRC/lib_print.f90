MODULE lib_print
   !!======================================================================
   !!                    ***  MODULE  lib_print  ***
   !! print librairy :  formated real and integer array print
   !!=====================================================================
      
   !!----------------------------------------------------------------------
   !!   prihin       : print an integer 2D horizontal field
   !!   prihre       : print an real 2D horizontal field
   !!   prizre       : print an real 2D vertical field
   !!----------------------------------------------------------------------
   USE par_kind      ! kind parameters

   IMPLICIT NONE
   PRIVATE

   PUBLIC   prihin, prihre, prizre

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: lib_print.f90 2715 2011-03-30 15:58:35Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE prihin( ktab, ki   , kj   , kideb, kifin ,   &
                      kind, kjdeb, kjfin, kjnd , kscale, kumout )
      !!----------------------------------------------------------------------
      !!                   ***  SUBROUTINE  prihre  ***
      !!  
      !! ** purpose :   Print an integer field
      !!
      !! ** method :   format of print is selected with the dummy argument kscale
      !!
      !! History :
      !!        !  90-04 (0. Marti)  Original code
      !!        !  92-02 (M. Imbard)
      !!        !  03-07 (G. Madec)  F90, free form
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   &
         ki, kj,                 &  ! array dimensions
         kideb, kifin, kind,     &  ! first and last index, increment for i 
         kjdeb, kjfin, kjnd,     &  ! first and last index, increment for j
         kscale,                 &  ! kscale=0 or > 5  print ktab with format i8
      !                             !         kscale=1 print ktab with format i1
      !                             !         kscale=2 print ktab with format i2
      !                             !         kscale=3 print ktab with format i3
      !                             !         kscale=4 print ktab with format i4
      !                             !         kscale=5 print ktab with format i5
         kumout                     ! unit in which print
      INTEGER, DIMENSION(ki,kj), INTENT( in ) ::   &
         ktab                       ! integer 2D array to be print

      !! * local declarations
      INTEGER ::   ji, jj, jn       ! dummy loop indices
      INTEGER ::   isca, il1, il2   ! temporary integers
      INTEGER ::   iind, ijnd       ! temporary integers

      isca = 10
      IF( kscale == 0 )   isca = 10
      IF( kscale == 1 )   isca = 100
      IF( kscale == 2 )   isca = 60
      IF( kscale == 3 )   isca = 40
      IF( kscale == 4 )   isca = 30
      IF( kscale == 5 )   isca = 20

      iind = MAX( 1, kind )
      ijnd = MAX( 1, kjnd )

      il1 = kideb

      DO jn = 1, (kifin-kideb+1)/(iind*isca) + 1

        IF( il1 > kifin ) RETURN
        WRITE(kumout,'(/)')
        il2 = il1+iind*(isca-1)
        IF( il2 > kifin )   il2 = kifin

        IF( kscale == 1 ) THEN
            WRITE(kumout,'(4x,i14," to ",1i4," each ",1i4,/)') il1, il2, iind
            DO jj = kjfin, kjdeb, -ijnd
              WRITE (kumout,'(1x,i3,100i1)') jj, ( ktab(ji,jj), ji = il1, il2, iind )
            END DO  
        ELSEIF( kscale == 2 ) THEN
            WRITE(kumout,'(4x,i14," to ",1i4," each ",1i4,/)')il1, il2, iind
            DO jj = kjfin, kjdeb, -ijnd
              WRITE (kumout,'(1x,i3,60i2)') jj, ( ktab(ji,jj), ji = il1, il2, iind )
            END DO  
        ELSEIF( kscale == 3 ) THEN
            WRITE(kumout,'(4x,i14," to ",1i4," each ",1i4,/)')il1, il2, iind
            DO jj = kjfin, kjdeb, -ijnd
              WRITE (kumout,'(1x,i3,40i3)') jj, ( ktab(ji,jj), ji = il1, il2, iind )
            END DO  
        ELSEIF( kscale == 4 ) THEN
            WRITE(kumout,'(4x,30i4,/)') ( ji, ji = il1, il2, iind )
            DO jj = kjfin, kjdeb, -ijnd
              WRITE (kumout,'(1x,i3,30i4)') jj, ( ktab(ji,jj), ji = il1, il2, iind )
            END DO  
        ELSEIF( kscale == 5 ) THEN
            WRITE(kumout,'(4x,20i5,/)') ( ji, ji = il1, il2, iind )
            DO jj = kjfin, kjdeb, -ijnd
              WRITE (kumout,'(1x,i3,20i5)') jj, ( ktab(ji,jj), ji = il1, il2, iind )
            END DO  
        ELSE
            WRITE(kumout,'(4x,10i8,/)') ( ji, ji = il1, il2, iind )
            DO jj = kjfin, kjdeb, -ijnd
              WRITE (kumout,'(1x,i3,10i8)') jj, ( ktab(ji,jj), ji = il1, il2, iind )
            END DO  
        ENDIF

        il1 = il1 + iind * isca
      END DO  

   END SUBROUTINE prihin


   SUBROUTINE prihre( ptab, ki   , kj   , kideb, kifin ,   &
                      kind, kjdeb, kjfin, kjnd , pscale, kumout )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE prihre  ***
      !!      
      !! ** purpose :   Print a real field with the format 10e12.4 or 20f6.2
      !!
      !! ** method  :   the print format is selected with the pscale argument
      !!
      !! History :
      !!   1.0  !  86-01  (P. Andrich)  Original code
      !!        !  89-11  (C. Levy)
      !!        !  92-02  (M. Imbard)
      !!        !  92-06  (M. Imbard)
      !!----------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   &
         ki, kj,                 &  ! array dimensions
         kideb, kifin, kind,     &  ! first and last index, increment for i
         kjdeb, kjfin, kjnd,     &  ! first and last index, increment for j
         kumout                     ! unit in which print
      REAL(wp), INTENT( in ) ::   &
         pscale                     ! = 0  print        ptab with e13.5 format
         !                          ! else print pscale*ptab with f6.2 format
      REAL(wp), DIMENSION(ki,kj), INTENT( in ) ::   &
         ptab                       ! integer 2D array to be print

      !! * Local variables
      INTEGER ::   ji, jj, jn       ! dummy loop indices
      INTEGER ::   isca, il1, il2   ! temporary integers

      isca = 10
      IF( pscale /= 0. )   isca=20

      il1 = kideb

      DO jn = 1, (kifin-kideb+1)/(kind*isca) + 1

        IF( il1 > kifin )   RETURN

        WRITE(kumout,9100)

        il2 = il1+kind*(isca-1)
        IF(il2 > kifin) il2 = kifin
        IF( pscale == 0.) THEN
            WRITE(kumout,9101) ( ji, ji = il1, il2, kind )
            DO jj = kjfin, kjdeb, -kjnd
              WRITE(kumout,9102) jj, ( ptab(ji,jj), ji = il1, il2, kind )
            END DO  
        ELSE
            WRITE(kumout,9103) ( ji, ji = il1, il2, kind )
            DO jj = kjfin, kjdeb, -kjnd
              WRITE(kumout,9104) jj, ( pscale*ptab(ji,jj), ji = il1, il2, kind )
            END DO  
        ENDIF
        il1 = il1+kind*isca

      END DO  

      ! formats
 9100 FORMAT(/)
 9101 FORMAT(10i12, /)
 9102 FORMAT(1x, i3, 10(1pe12.4))
 9103 FORMAT(3x, 20i6, /)
 9104 FORMAT(1x, i3, 1x, 20f6.2)

   END SUBROUTINE prihre


   SUBROUTINE prizre( ptab , ki   , kj   , kk   , kjcut ,   &
                      kideb, kifin, kid  , kkdeb, kkfin ,   &
                      kkd  , pscale, kumout )
      !!----------------------------------------------------------------------
      !!                      ***  ROUTINE prizre  ***
      !!
      !! ** purpose :   Print a vertical slab from a tridimentional real field
      !!
      !!   METHOD :
      !! ** method  :   the print format is selected with the argument pscale
      !!
      !! History :
      !!      original : 86-01 (o. Marti)
      !!      addition : 92-02 (M. Imbard)
      !!      addition : 92-06 (M. Imbard)
      !!----------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT( in ) ::   &
         ki, kj, kk,             &  ! array dimensions
         kjcut,                  &  ! index j for the vertical slab
         kideb, kifin, kid,      &  ! first and last index, increment for i
         kkdeb, kkfin, kkd,      &  ! first and last index, increment for k
         kumout                     ! unit in which print
      REAL(wp), INTENT( in ) ::   &
         pscale                     ! = 0  print        ptab with e12.4 format
         !                          ! else print pscale*ptab with f6.2 format
      REAL(wp), DIMENSION(ki,kj,kk), INTENT( in ) ::   &
         ptab                       ! integer 3D array to be print

      !! * Local variables
      INTEGER ::   ji, jn, jk       ! dummy loop indices
      INTEGER ::   isca, il1, il2   ! temporary integers
      INTEGER ::   iind, iknd       !    "          "


      iind = kid
      iknd = kkd
      isca = 10
      IF( pscale /= 0.) isca = 20

      IF (iind == 0) iind = 1
      IF (iknd == 0) iknd = 1

      il1 = kideb

      DO jn = 1, (kifin-kideb+1)/(iind*isca) + 1

        IF(il1 > kifin) RETURN
        WRITE(kumout,9100)
        il2 = il1+iind*(isca-1)
        IF(il2 > kifin) il2 = kifin

        IF( pscale == 0.) THEN
            WRITE(kumout,9101) ( ji, ji = il1, il2, iind )
            DO jk = kkdeb, kkfin, iknd
              WRITE (kumout,9102) jk, ( ptab(ji,kjcut,jk), ji = il1, il2, iind )
            END DO  
        ELSE
            WRITE (kumout,9103) ( ji, ji = il1, il2, iind )
            DO jk = kkdeb, kkfin, iknd
              WRITE(kumout,9104)jk, ( pscale*ptab(ji,kjcut,jk), ji = il1, il2, iind )
            END DO  
        ENDIF

        il1 = il1+iind*isca
      END DO      

 9100 FORMAT(/)
 9101 FORMAT(10i12, /)
 9102 FORMAT(1x, i3, 10(1pe12.4))
 9103 FORMAT(3x, 20i6, /)
 9104 FORMAT(1x, i3, 1x, 20f6.1)

      END SUBROUTINE prizre

   !!======================================================================
END MODULE lib_print
