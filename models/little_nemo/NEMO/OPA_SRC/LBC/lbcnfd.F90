MODULE lbcnfd
   !!======================================================================
   !!                       ***  MODULE  lbcnfd  ***
   !! Ocean        : north fold  boundary conditions
   !!======================================================================
   !! History :  3.2  ! 2009-03  (R. Benshila)  Original code 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   lbc_nfd       : generic interface for lbc_nfd_3d and lbc_nfd_2d routines
   !!   lbc_nfd_3d    : lateral boundary condition: North fold treatment for a 3D arrays   (lbc_nfd)
   !!   lbc_nfd_2d    : lateral boundary condition: North fold treatment for a 2D arrays   (lbc_nfd)
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain 
   USE in_out_manager ! I/O manager

   IMPLICIT NONE
   PRIVATE

   INTERFACE lbc_nfd
      MODULE PROCEDURE   lbc_nfd_3d, lbc_nfd_2d
   END INTERFACE

   PUBLIC   lbc_nfd   ! north fold conditions

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: lbcnfd.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lbc_nfd_3d( pt3d, cd_type, psgn )
      !!----------------------------------------------------------------------
      !!                  ***  routine lbc_nfd_3d  ***
      !!
      !! ** Purpose :   3D lateral boundary condition : North fold treatment
      !!              without processor exchanges. 
      !!
      !! ** Method  :   
      !!
      !! ** Action  :   pt3d with updated values along the north fold
      !!----------------------------------------------------------------------
      CHARACTER(len=1)          , INTENT(in   ) ::   cd_type   ! define the nature of ptab array grid-points
      !                                                        !   = T , U , V , F , W points
      REAL(wp)                  , INTENT(in   ) ::   psgn      ! control of the sign change
      !                                                        !   = -1. , the sign is changed if north fold boundary
      !                                                        !   =  1. , the sign is kept  if north fold boundary
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   pt3d      ! 3D array on which the boundary condition is applied
      !
      INTEGER  ::   ji, jk
      INTEGER  ::   ijt, iju, ijpj, ijpjm1
      !!----------------------------------------------------------------------

      SELECT CASE ( jpni )
      CASE ( 1 )     ;   ijpj = nlcj      ! 1 proc only  along the i-direction
      CASE DEFAULT   ;   ijpj = 4         ! several proc along the i-direction
      END SELECT
      ijpjm1 = ijpj-1

      DO jk = 1, jpk
         !
         SELECT CASE ( npolj )
         !
         CASE ( 3 , 4 )                        ! *  North fold  T-point pivot
            !
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  pt3d(ji,ijpj,jk) = psgn * pt3d(ijt,ijpj-2,jk)
               END DO
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+2
                  pt3d(ji,ijpjm1,jk) = psgn * pt3d(ijt,ijpjm1,jk)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  pt3d(ji,ijpj,jk) = psgn * pt3d(iju,ijpj-2,jk)
               END DO
               DO ji = jpiglo/2, jpiglo-1
                  iju = jpiglo-ji+1
                  pt3d(ji,ijpjm1,jk) = psgn * pt3d(iju,ijpjm1,jk)
               END DO
            CASE ( 'V' )                               ! V-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  pt3d(ji,ijpj-1,jk) = psgn * pt3d(ijt,ijpj-2,jk)
                  pt3d(ji,ijpj  ,jk) = psgn * pt3d(ijt,ijpj-3,jk)
               END DO
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  pt3d(ji,ijpj-1,jk) = psgn * pt3d(iju,ijpj-2,jk)
                  pt3d(ji,ijpj  ,jk) = psgn * pt3d(iju,ijpj-3,jk)
               END DO
            END SELECT
            !
         CASE ( 5 , 6 )                        ! *  North fold  F-point pivot
            !
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  pt3d(ji,ijpj,jk) = psgn * pt3d(ijt,ijpj-1,jk)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  pt3d(ji,ijpj,jk) = psgn * pt3d(iju,ijpj-1,jk)
               END DO
            CASE ( 'V' )                               ! V-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  pt3d(ji,ijpj,jk) = psgn * pt3d(ijt,ijpj-2,jk)
               END DO
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+1
                  pt3d(ji,ijpjm1,jk) = psgn * pt3d(ijt,ijpjm1,jk)
               END DO
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  pt3d(ji,ijpj  ,jk) = psgn * pt3d(iju,ijpj-2,jk)
               END DO
               DO ji = jpiglo/2+1, jpiglo-1
                  iju = jpiglo-ji
                  pt3d(ji,ijpjm1,jk) = psgn * pt3d(iju,ijpjm1,jk)
               END DO
            END SELECT
            !
         CASE DEFAULT                           ! *  closed : the code probably never go through
            !
            SELECT CASE ( cd_type)
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               pt3d(:, 1  ,jk) = 0.e0
               pt3d(:,ijpj,jk) = 0.e0
            CASE ( 'F' )                               ! F-point
               pt3d(:,ijpj,jk) = 0.e0
            END SELECT
            !
         END SELECT     !  npolj
         !
      END DO
      !
   END SUBROUTINE lbc_nfd_3d


   SUBROUTINE lbc_nfd_2d( pt2d, cd_type, psgn, pr2dj )
      !!----------------------------------------------------------------------
      !!                  ***  routine lbc_nfd_2d  ***
      !!
      !! ** Purpose :   2D lateral boundary condition : North fold treatment
      !!       without processor exchanges. 
      !!
      !! ** Method  :   
      !!
      !! ** Action  :   pt2d with updated values along the north fold
      !!----------------------------------------------------------------------
      CHARACTER(len=1)        , INTENT(in   ) ::   cd_type   ! define the nature of ptab array grid-points
      !                                                      ! = T , U , V , F , W points
      REAL(wp)                , INTENT(in   ) ::   psgn      ! control of the sign change
      !                                                      !   = -1. , the sign is changed if north fold boundary
      !                                                      !   =  1. , the sign is kept  if north fold boundary
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::   pt2d      ! 2D array on which the boundary condition is applied
      INTEGER , OPTIONAL      , INTENT(in   ) ::   pr2dj     ! number of additional halos
      !
      INTEGER  ::   ji, jl, ipr2dj
      INTEGER  ::   ijt, iju, ijpj, ijpjm1
      !!----------------------------------------------------------------------

      SELECT CASE ( jpni )
      CASE ( 1 )     ;   ijpj = nlcj      ! 1 proc only  along the i-direction
      CASE DEFAULT   ;   ijpj = 4         ! several proc along the i-direction
      END SELECT
      !
      IF( PRESENT(pr2dj) ) THEN           ! use of additional halos
         ipr2dj = pr2dj
         IF( jpni > 1 )   ijpj = ijpj + ipr2dj
      ELSE
         ipr2dj = 0 
      ENDIF
      !
      ijpjm1 = ijpj-1


      SELECT CASE ( npolj )
      !
      CASE ( 3, 4 )                       ! *  North fold  T-point pivot
         !
         SELECT CASE ( cd_type )
         !
         CASE ( 'T' , 'W' )                               ! T- , W-points
            DO jl = 0, ipr2dj
               DO ji = 2, jpiglo
                  ijt=jpiglo-ji+2
                  pt2d(ji,ijpj+jl) = psgn * pt2d(ijt,ijpj-2-jl)
               END DO
            END DO
            DO ji = jpiglo/2+1, jpiglo
               ijt=jpiglo-ji+2
               pt2d(ji,ijpj-1) = psgn * pt2d(ijt,ijpj-1)
            END DO
         CASE ( 'U' )                                     ! U-point
            DO jl = 0, ipr2dj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  pt2d(ji,ijpj+jl) = psgn * pt2d(iju,ijpj-2-jl)
               END DO
            END DO
            DO ji = jpiglo/2, jpiglo-1
               iju = jpiglo-ji+1
               pt2d(ji,ijpjm1) = psgn * pt2d(iju,ijpjm1)
            END DO
         CASE ( 'V' )                                     ! V-point
            DO jl = -1, ipr2dj
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  pt2d(ji,ijpj+jl) = psgn * pt2d(ijt,ijpj-3-jl)
               END DO
            END DO
         CASE ( 'F' )                                     ! F-point
            DO jl = -1, ipr2dj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  pt2d(ji,ijpj+jl) = psgn * pt2d(iju,ijpj-3-jl)
               END DO
            END DO
         CASE ( 'I' )                                     ! ice U-V point (I-point)
            DO jl = 0, ipr2dj
               pt2d(2,ijpj+jl) = psgn * pt2d(3,ijpj-1+jl)
               DO ji = 3, jpiglo
                  iju = jpiglo - ji + 3
                  pt2d(ji,ijpj+jl) = psgn * pt2d(iju,ijpj-1-jl)
               END DO
            END DO
         CASE ( 'J' )                                     ! first ice U-V point
            DO jl =0, ipr2dj
               pt2d(2,ijpj+jl) = psgn * pt2d(3,ijpj-1+jl)
               DO ji = 3, jpiglo
                  iju = jpiglo - ji + 3
                  pt2d(ji,ijpj+jl) = psgn * pt2d(iju,ijpj-1-jl)
               END DO
            END DO
         CASE ( 'K' )                                     ! second ice U-V point
            DO jl =0, ipr2dj
               pt2d(2,ijpj+jl) = psgn * pt2d(3,ijpj-1+jl)
               DO ji = 3, jpiglo
                  iju = jpiglo - ji + 3
                  pt2d(ji,ijpj+jl) = psgn * pt2d(iju,ijpj-1-jl)
               END DO
            END DO
         END SELECT
         !
      CASE ( 5, 6 )                        ! *  North fold  F-point pivot
         !
         SELECT CASE ( cd_type )
         CASE ( 'T' , 'W' )                               ! T-, W-point
            DO jl = 0, ipr2dj
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  pt2d(ji,ijpj+jl) = psgn * pt2d(ijt,ijpj-1-jl)
               END DO
            END DO
         CASE ( 'U' )                                     ! U-point
            DO jl = 0, ipr2dj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  pt2d(ji,ijpj+jl) = psgn * pt2d(iju,ijpj-1-jl)
               END DO
            END DO
         CASE ( 'V' )                                     ! V-point
            DO jl = 0, ipr2dj
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  pt2d(ji,ijpj+jl) = psgn * pt2d(ijt,ijpj-2-jl)
               END DO
            END DO
            DO ji = jpiglo/2+1, jpiglo
               ijt = jpiglo-ji+1
               pt2d(ji,ijpjm1) = psgn * pt2d(ijt,ijpjm1)
            END DO
         CASE ( 'F' )                               ! F-point
            DO jl = 0, ipr2dj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  pt2d(ji,ijpj+jl) = psgn * pt2d(iju,ijpj-2-jl)
               END DO
            END DO
            DO ji = jpiglo/2+1, jpiglo-1
               iju = jpiglo-ji
               pt2d(ji,ijpjm1) = psgn * pt2d(iju,ijpjm1)
            END DO
         CASE ( 'I' )                                  ! ice U-V point (I-point)
            pt2d( 2 ,ijpj:ijpj+ipr2dj) = 0.e0
            DO jl = 0, ipr2dj
               DO ji = 2 , jpiglo-1
                  ijt = jpiglo - ji + 2
                  pt2d(ji,ijpj+jl)= 0.5 * ( pt2d(ji,ijpj-1-jl) + psgn * pt2d(ijt,ijpj-1-jl) )
               END DO
            END DO
         CASE ( 'J' )                                  ! first ice U-V point
            pt2d( 2 ,ijpj:ijpj+ipr2dj) = 0.e0
            DO jl = 0, ipr2dj
               DO ji = 2 , jpiglo-1
                  ijt = jpiglo - ji + 2
                  pt2d(ji,ijpj+jl)= pt2d(ji,ijpj-1-jl)
               END DO
            END DO
         CASE ( 'K' )                                  ! second ice U-V point
            pt2d( 2 ,ijpj:ijpj+ipr2dj) = 0.e0
            DO jl = 0, ipr2dj
               DO ji = 2 , jpiglo-1
                  ijt = jpiglo - ji + 2
                  pt2d(ji,ijpj+jl)= pt2d(ijt,ijpj-1-jl)
               END DO
            END DO
         END SELECT
         !
      CASE DEFAULT                           ! *  closed : the code probably never go through
         !
         SELECT CASE ( cd_type)
         CASE ( 'T' , 'U' , 'V' , 'W' )                 ! T-, U-, V-, W-points
            pt2d(:, 1:1-ipr2dj     ) = 0.e0
            pt2d(:,ijpj:ijpj+ipr2dj) = 0.e0
         CASE ( 'F' )                                   ! F-point
            pt2d(:,ijpj:ijpj+ipr2dj) = 0.e0
         CASE ( 'I' )                                   ! ice U-V point
            pt2d(:, 1:1-ipr2dj     ) = 0.e0
            pt2d(:,ijpj:ijpj+ipr2dj) = 0.e0
         CASE ( 'J' )                                   ! first ice U-V point
            pt2d(:, 1:1-ipr2dj     ) = 0.e0
            pt2d(:,ijpj:ijpj+ipr2dj) = 0.e0
         CASE ( 'K' )                                   ! second ice U-V point
            pt2d(:, 1:1-ipr2dj     ) = 0.e0
            pt2d(:,ijpj:ijpj+ipr2dj) = 0.e0
         END SELECT
         !
      END SELECT
      !
   END SUBROUTINE lbc_nfd_2d

   !!======================================================================
END MODULE lbcnfd
