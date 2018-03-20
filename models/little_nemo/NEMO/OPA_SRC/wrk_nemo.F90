MODULE wrk_nemo
   !!======================================================================
   !!                       ***  MODULE  wrk_nemo  ***
   !! NEMO work space:  define and allocate work-space arrays used in 
   !! all components of NEMO
   !!======================================================================
   !! History :  4.0  !  2011-01  (A Porter)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   wrk_alloc         : get work space arrays
   !!   wrk_dealloc       : release work space arrays
   !!
   !! 1d arrays:
   !!   REAL(wp), POINTER, DIMENSION(:) :: arr1, arr2, ... arr10
   !!    or
   !!   INTEGER, POINTER, DIMENSION(:) :: arr1, arr2, ... arr10
   !!   ...
   !!   CALL wrk_alloc( nx, arr1, arr2, ... arr10, kistart = kistart )
   !!   ...
   !!   CALL wrk_dealloc( nx, arr1, arr2, ... arr10, kistart = kistart)
   !!   with:
   !!     - arr*: 1d arrays. real or (not and) integer
   !!     - nx: size of the 1d arr* arrays
   !!     - arr2, ..., arr10: optional parameters
   !!     - kistart: optional parameter to lower bound of the 1st dimension (default = 1)
   !!
   !! 2d arrays:
   !!   REAL(wp), POINTER, DIMENSION(:,:) :: arr1, arr2, ... arr10
   !!    or
   !!   INTEGER, POINTER, DIMENSION(:,:) :: arr1, arr2, ... arr10
   !!   ...
   !!   CALL wrk_alloc( nx, ny, arr1, arr2, ... arr10, kistart = kistart, kjstart = kjstart )
   !!   ...
   !!   CALL wrk_dealloc( nx, ny, arr1, arr2, ... arr10, kistart = kistart, kjstart = kjstart )
   !!   with:
   !!     - arr* 2d arrays. real or (not and) integer
   !!     - nx, ny: size of the 2d arr* arrays
   !!     - arr2, ..., arr10: optional parameters
   !!     - kistart, kjstart: optional parameters to lower bound of the 1st/2nd dimension (default = 1)
   !!
   !! 3d arrays:
   !!   REAL(wp), POINTER, DIMENSION(:,:,:) :: arr1, arr2, ... arr10
   !!    or
   !!   INTEGER, POINTER, DIMENSION(:,:,:) :: arr1, arr2, ... arr10
   !!   ...
   !!   CALL wrk_alloc( nx, ny, nz, arr1, arr2, ... arr10, kistart = kistart, kjstart = kjstart, kkstart = kkstart )
   !!   ...
   !!   CALL wrk_dealloc( nx, ny, nz, arr1, arr2, ... arr10, kistart = kistart, kjstart = kjstart, kkstart = kkstart )
   !!   with:
   !!     - arr* 3d arrays. real or (not and) integer
   !!     - nx, ny, nz: size of the 3d arr* arrays
   !!     - arr2, ..., arr10: optional parameters
   !!     - kistart, kjstart, kkstart: optional parameters to lower bound of the 1st/2nd/3rd dimension (default = 1)
   !!
   !! 4d arrays:
   !!   REAL(wp), POINTER, DIMENSION(:,:,:,:) :: arr1, arr2, ... arr10
   !!    or
   !!   INTEGER, POINTER, DIMENSION(:,:,:,:) :: arr1, arr2, ... arr10
   !!   ...
   !!   CALL wrk_alloc( nx, ny, nz, nl, arr1, arr2, ... arr10, &
   !!      &            kistart = kistart, kjstart = kjstart, kkstart = kkstart, klstart = klstart )
   !!   ...
   !!   CALL wrk_dealloc( nx, ny, nz, nl, arr1, arr2, ... arr10, &
   !!      &            kistart = kistart, kjstart = kjstart, kkstart = kkstart, klstart = klstart )
   !!   with:
   !!     - arr* 3d arrays. real or (not and) integer
   !!     - nx, ny, nz, nl: size of the 4d arr* arrays
   !!     - arr2, ..., arr10: optional parameters
   !!     - kistart, kjstart, kkstart, klstart: optional parameters to lower bound of the 1st/2nd/3rd/4th dimension (default = 1)
   !!   
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters

   IMPLICIT NONE
   PRIVATE
   
   PUBLIC wrk_alloc, wrk_dealloc, wrk_list

   INTERFACE wrk_alloc
      MODULE PROCEDURE wrk_alloc_1dr, wrk_alloc_2dr, wrk_alloc_3dr, wrk_alloc_4dr,   &
         &             wrk_alloc_1di, wrk_alloc_2di, wrk_alloc_3di, wrk_alloc_4di
   END INTERFACE

   INTERFACE wrk_dealloc
      MODULE PROCEDURE wrk_dealloc_1dr, wrk_dealloc_2dr, wrk_dealloc_3dr, wrk_dealloc_4dr,   &
         &             wrk_dealloc_1di, wrk_dealloc_2di, wrk_dealloc_3di, wrk_dealloc_4di
   END INTERFACE


   INTEGER, PARAMETER :: jparray = 1000
   INTEGER, PARAMETER :: jpmaxdim = 4

   INTEGER, PARAMETER :: jpnotdefined = 0
   INTEGER, PARAMETER :: jpinteger = 1
   INTEGER, PARAMETER :: jpreal = 2
  
   TYPE leaf
      LOGICAL :: in_use
      INTEGER :: indic
      INTEGER , DIMENSION(:)      , POINTER :: iwrk1d => NULL()    
      INTEGER , DIMENSION(:,:)    , POINTER :: iwrk2d => NULL()    
      INTEGER , DIMENSION(:,:,:)  , POINTER :: iwrk3d => NULL()    
      INTEGER , DIMENSION(:,:,:,:), POINTER :: iwrk4d => NULL()    
      REAL(wp), DIMENSION(:)      , POINTER :: zwrk1d => NULL()    
      REAL(wp), DIMENSION(:,:)    , POINTER :: zwrk2d => NULL()    
      REAL(wp), DIMENSION(:,:,:)  , POINTER :: zwrk3d => NULL()    
      REAL(wp), DIMENSION(:,:,:,:), POINTER :: zwrk4d => NULL()    
      TYPE (leaf), POINTER :: next => NULL() 
      TYPE (leaf), POINTER :: prev => NULL() 
   END TYPE leaf
   
   TYPE branch
      INTEGER :: itype
      INTEGER, DIMENSION(jpmaxdim) :: ishape, istart
      TYPE(leaf), POINTER :: start => NULL()     
      TYPE(leaf), POINTER :: current => NULL()      
   END TYPE branch

   TYPE(branch), SAVE, DIMENSION(jparray) :: tree

   LOGICAL ::   linit = .FALSE.
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id:$
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE wrk_list
      ! to list 3d arrays in use, to be duplicated for all cases 
      WRITE(*,*) 'Arrays in use :'
      !      CALL listage(tree_3d(1)%s_wrk_3d_start)
      WRITE(*,*) ''
      
   END SUBROUTINE wrk_list
   
   
   RECURSIVE SUBROUTINE listage(ptr)
      
      TYPE(leaf), POINTER, INTENT(in) :: ptr
      !
      IF( ASSOCIATED(ptr%next) ) CALL listage(ptr%next)
      WRITE(*,*) ptr%in_use, ptr%indic   
      
   END SUBROUTINE listage


   SUBROUTINE wrk_alloc_1dr( kidim, p1d01, p1d02, p1d03, p1d04, p1d05, p1d06, p1d07, p1d08, p1d09, p1d10, kistart )
      INTEGER                        , INTENT(in   )           ::   kidim   ! dimensions size
      REAL(wp), POINTER, DIMENSION(:), INTENT(inout)           ::   p1d01
      REAL(wp), POINTER, DIMENSION(:), INTENT(inout), OPTIONAL ::   p1d02,p1d03,p1d04,p1d05,p1d06,p1d07,p1d08,p1d09,p1d10
      INTEGER                        , INTENT(in   ), OPTIONAL ::   kistart
      !
      CALL wrk_alloc_xd( kidim, 0, 0, 0, kistart, 1, 1, 1,                                            &
         &               p1d01 = p1d01, p1d02 = p1d02, p1d03 = p1d03, p1d04 = p1d04, p1d05 = p1d05,   &
         &               p1d06 = p1d06, p1d07 = p1d07, p1d08 = p1d08, p1d09 = p1d09, p1d10 = p1d10    )
      !
   END SUBROUTINE wrk_alloc_1dr


   SUBROUTINE wrk_alloc_1di( kidim, k1d01, k1d02, k1d03, k1d04, k1d05, k1d06, k1d07, k1d08, k1d09, k1d10, kistart )
      INTEGER                        , INTENT(in   )           ::   kidim   ! dimensions size
      INTEGER , POINTER, DIMENSION(:), INTENT(inout)           ::   k1d01
      INTEGER , POINTER, DIMENSION(:), INTENT(inout), OPTIONAL ::   k1d02,k1d03,k1d04,k1d05,k1d06,k1d07,k1d08,k1d09,k1d10
      INTEGER                        , INTENT(in   ), OPTIONAL ::   kistart
      !
      CALL wrk_alloc_xd( kidim, 0, 0, 0, kistart, 1, 1, 1,                                            &
         &               k1d01 = k1d01, k1d02 = k1d02, k1d03 = k1d03, k1d04 = k1d04, k1d05 = k1d05,   &
         &               k1d06 = k1d06, k1d07 = k1d07, k1d08 = k1d08, k1d09 = k1d09, k1d10 = k1d10    )
      !
   END SUBROUTINE wrk_alloc_1di


   SUBROUTINE wrk_alloc_2dr( kidim, kjdim, p2d01, p2d02, p2d03, p2d04, p2d05, p2d06, p2d07, p2d08, p2d09, p2d10, kistart, kjstart )
      INTEGER                          , INTENT(in   )           ::   kidim, kjdim   ! dimensions size
      REAL(wp), POINTER, DIMENSION(:,:), INTENT(inout)           ::   p2d01
      REAL(wp), POINTER, DIMENSION(:,:), INTENT(inout), OPTIONAL ::   p2d02,p2d03,p2d04,p2d05,p2d06,p2d07,p2d08,p2d09,p2d10
      INTEGER                          , INTENT(in   ), OPTIONAL ::   kistart, kjstart
      !
      CALL wrk_alloc_xd( kidim, kjdim, 0, 0, kistart, kjstart, 1, 1,                                  &
         &               p2d01 = p2d01, p2d02 = p2d02, p2d03 = p2d03, p2d04 = p2d04, p2d05 = p2d05,   &
         &               p2d06 = p2d06, p2d07 = p2d07, p2d08 = p2d08, p2d09 = p2d09, p2d10 = p2d10    )
      !
   END SUBROUTINE wrk_alloc_2dr


   SUBROUTINE wrk_alloc_2di( kidim, kjdim, k2d01, k2d02, k2d03, k2d04, k2d05, k2d06, k2d07, k2d08, k2d09, k2d10, kistart, kjstart )
      INTEGER                          , INTENT(in   )           ::   kidim, kjdim   ! dimensions size
      INTEGER , POINTER, DIMENSION(:,:), INTENT(inout)           ::   k2d01
      INTEGER , POINTER, DIMENSION(:,:), INTENT(inout), OPTIONAL ::   k2d02,k2d03,k2d04,k2d05,k2d06,k2d07,k2d08,k2d09,k2d10
      INTEGER                          , INTENT(in   ), OPTIONAL ::   kistart, kjstart
      !
      CALL wrk_alloc_xd( kidim, kjdim, 0, 0, kistart, kjstart, 1, 1,                                  &
         &               k2d01 = k2d01, k2d02 = k2d02, k2d03 = k2d03, k2d04 = k2d04, k2d05 = k2d05,   &
         &               k2d06 = k2d06, k2d07 = k2d07, k2d08 = k2d08, k2d09 = k2d09, k2d10 = k2d10    )
      !
   END SUBROUTINE wrk_alloc_2di


   SUBROUTINE wrk_alloc_3dr( kidim, kjdim, kkdim, p3d01, p3d02, p3d03, p3d04, p3d05, p3d06, p3d07, p3d08, p3d09, p3d10,   &
      &                      kistart, kjstart, kkstart )
      INTEGER                            , INTENT(in   )           ::   kidim, kjdim, kkdim   ! dimensions size
      REAL(wp), POINTER, DIMENSION(:,:,:), INTENT(inout)           ::   p3d01
      REAL(wp), POINTER, DIMENSION(:,:,:), INTENT(inout), OPTIONAL ::   p3d02,p3d03,p3d04,p3d05,p3d06,p3d07,p3d08,p3d09,p3d10
      INTEGER                            , INTENT(in   ), OPTIONAL ::   kistart, kjstart, kkstart
      !
      CALL wrk_alloc_xd( kidim, kjdim, kkdim, 0, kistart, kjstart, kkstart, 1,                        &
         &               p3d01 = p3d01, p3d02 = p3d02, p3d03 = p3d03, p3d04 = p3d04, p3d05 = p3d05,   &
         &               p3d06 = p3d06, p3d07 = p3d07, p3d08 = p3d08, p3d09 = p3d09, p3d10 = p3d10    )
      !
   END SUBROUTINE wrk_alloc_3dr


   SUBROUTINE wrk_alloc_3di( kidim, kjdim, kkdim, k3d01, k3d02, k3d03, k3d04, k3d05, k3d06, k3d07, k3d08, k3d09, k3d10,   &
      &                      kistart, kjstart, kkstart )
      INTEGER                            , INTENT(in   )           ::   kidim, kjdim, kkdim   ! dimensions size
      INTEGER , POINTER, DIMENSION(:,:,:), INTENT(inout)           ::   k3d01
      INTEGER , POINTER, DIMENSION(:,:,:), INTENT(inout), OPTIONAL ::   k3d02,k3d03,k3d04,k3d05,k3d06,k3d07,k3d08,k3d09,k3d10
      INTEGER                            , INTENT(in   ), OPTIONAL ::   kistart, kjstart, kkstart
      !
      CALL wrk_alloc_xd( kidim, kjdim, kkdim, 0, kistart, kjstart, kkstart, 1,                        &
         &               k3d01 = k3d01, k3d02 = k3d02, k3d03 = k3d03, k3d04 = k3d04, k3d05 = k3d05,   &
         &               k3d06 = k3d06, k3d07 = k3d07, k3d08 = k3d08, k3d09 = k3d09, k3d10 = k3d10    )
      !
   END SUBROUTINE wrk_alloc_3di


   SUBROUTINE wrk_alloc_4dr( kidim, kjdim, kkdim, kldim, p4d01, p4d02, p4d03, p4d04, p4d05, p4d06, p4d07, p4d08, p4d09, p4d10,   &
      &                      kistart, kjstart, kkstart, klstart )
      INTEGER                              , INTENT(in   )           ::   kidim, kjdim, kkdim, kldim   ! dimensions size
      REAL(wp), POINTER, DIMENSION(:,:,:,:), INTENT(inout)           ::   p4d01
      REAL(wp), POINTER, DIMENSION(:,:,:,:), INTENT(inout), OPTIONAL ::   p4d02,p4d03,p4d04,p4d05,p4d06,p4d07,p4d08,p4d09,p4d10
      INTEGER                              , INTENT(in   ), OPTIONAL ::   kistart, kjstart, kkstart, klstart
      !
      CALL wrk_alloc_xd( kidim, kjdim, kkdim, kldim, kistart, kjstart, kkstart, klstart,              &
         &               p4d01 = p4d01, p4d02 = p4d02, p4d03 = p4d03, p4d04 = p4d04, p4d05 = p4d05,   &
         &               p4d06 = p4d06, p4d07 = p4d07, p4d08 = p4d08, p4d09 = p4d09, p4d10 = p4d10    )
      !
   END SUBROUTINE wrk_alloc_4dr


   SUBROUTINE wrk_alloc_4di( kidim, kjdim, kkdim, kldim, k4d01, k4d02, k4d03, k4d04, k4d05, k4d06, k4d07, k4d08, k4d09, k4d10,   &
      &                      kistart, kjstart, kkstart, klstart )
      INTEGER                              , INTENT(in   )           ::   kidim, kjdim, kkdim, kldim   ! dimensions size
      INTEGER , POINTER, DIMENSION(:,:,:,:), INTENT(inout)           ::   k4d01
      INTEGER , POINTER, DIMENSION(:,:,:,:), INTENT(inout), OPTIONAL ::   k4d02,k4d03,k4d04,k4d05,k4d06,k4d07,k4d08,k4d09,k4d10
      INTEGER                              , INTENT(in   ), OPTIONAL ::   kistart, kjstart, kkstart, klstart
      !
      CALL wrk_alloc_xd( kidim, kjdim, kkdim, kldim, kistart, kjstart, kkstart, klstart,              &
         &               k4d01 = k4d01, k4d02 = k4d02, k4d03 = k4d03, k4d04 = k4d04, k4d05 = k4d05,   &
         &               k4d06 = k4d06, k4d07 = k4d07, k4d08 = k4d08, k4d09 = k4d09, k4d10 = k4d10    )
      !
   END SUBROUTINE wrk_alloc_4di


   SUBROUTINE wrk_dealloc_1dr( kidim, p1d01, p1d02, p1d03, p1d04, p1d05, p1d06, p1d07, p1d08, p1d09, p1d10, kistart )
      INTEGER                        , INTENT(in   )           ::   kidim   ! dimensions size
      REAL(wp), POINTER, DIMENSION(:), INTENT(inout)           ::   p1d01
      REAL(wp), POINTER, DIMENSION(:), INTENT(inout), OPTIONAL ::   p1d02,p1d03,p1d04,p1d05,p1d06,p1d07,p1d08,p1d09,p1d10
      INTEGER                        , INTENT(in   ), OPTIONAL ::   kistart
      !
      INTEGER :: icnt, jn
      icnt = 1 + COUNT( (/                PRESENT(p1d02),PRESENT(p1d03),PRESENT(p1d04),PRESENT(p1d05),   &
         &                 PRESENT(p1d06),PRESENT(p1d07),PRESENT(p1d08),PRESENT(p1d09),PRESENT(p1d10) /) )
      DO jn = 1, icnt   ;   CALL wrk_deallocbase( jpreal, kidim, 0, 0, 0, kistart, 1, 1, 1)   ;   END DO
      !
   END SUBROUTINE wrk_dealloc_1dr


   SUBROUTINE wrk_dealloc_1di( kidim, k1d01, k1d02, k1d03, k1d04, k1d05, k1d06, k1d07, k1d08, k1d09, k1d10, kistart )
      INTEGER                        , INTENT(in   )           ::   kidim   ! dimensions size
      INTEGER , POINTER, DIMENSION(:), INTENT(inout)           ::   k1d01
      INTEGER , POINTER, DIMENSION(:), INTENT(inout), OPTIONAL ::   k1d02,k1d03,k1d04,k1d05,k1d06,k1d07,k1d08,k1d09,k1d10
      INTEGER                        , INTENT(in   ), OPTIONAL ::   kistart
      !
      INTEGER :: icnt, jn
      icnt = 1 + COUNT( (/                PRESENT(k1d02),PRESENT(k1d03),PRESENT(k1d04),PRESENT(k1d05),   &
         &                 PRESENT(k1d06),PRESENT(k1d07),PRESENT(k1d08),PRESENT(k1d09),PRESENT(k1d10) /) )
      DO jn = 1, icnt   ;   CALL wrk_deallocbase( jpinteger, kidim, 0, 0, 0, kistart, 1, 1, 1 )   ;   END DO
      !
   END SUBROUTINE wrk_dealloc_1di


   SUBROUTINE wrk_dealloc_2dr( kidim, kjdim, p2d01, p2d02, p2d03, p2d04, p2d05, p2d06, p2d07, p2d08, p2d09, p2d10, kistart,kjstart )
      INTEGER                          , INTENT(in   )           ::   kidim, kjdim   ! dimensions size
      REAL(wp), POINTER, DIMENSION(:,:), INTENT(inout)           ::   p2d01
      REAL(wp), POINTER, DIMENSION(:,:), INTENT(inout), OPTIONAL ::   p2d02,p2d03,p2d04,p2d05,p2d06,p2d07,p2d08,p2d09,p2d10
      INTEGER                          , INTENT(in   ), OPTIONAL ::   kistart, kjstart
      !
      INTEGER :: icnt, jn
      icnt = 1 + COUNT( (/                PRESENT(p2d02),PRESENT(p2d03),PRESENT(p2d04),PRESENT(p2d05),   &
         &                 PRESENT(p2d06),PRESENT(p2d07),PRESENT(p2d08),PRESENT(p2d09),PRESENT(p2d10) /) )
      DO jn = 1, icnt   ;   CALL wrk_deallocbase( jpreal, kidim, kjdim, 0, 0, kistart, kjstart, 1, 1 )   ;   END DO
      !
   END SUBROUTINE wrk_dealloc_2dr


   SUBROUTINE wrk_dealloc_2di( kidim, kjdim, k2d01, k2d02, k2d03, k2d04, k2d05, k2d06, k2d07, k2d08, k2d09, k2d10, kistart,kjstart )
      INTEGER                          , INTENT(in   )           ::   kidim, kjdim   ! dimensions size
      INTEGER , POINTER, DIMENSION(:,:), INTENT(inout)           ::   k2d01
      INTEGER , POINTER, DIMENSION(:,:), INTENT(inout), OPTIONAL ::   k2d02,k2d03,k2d04,k2d05,k2d06,k2d07,k2d08,k2d09,k2d10
      INTEGER                          , INTENT(in   ), OPTIONAL ::   kistart, kjstart
      !
      INTEGER :: icnt, jn
      icnt = 1 + COUNT( (/                PRESENT(k2d02),PRESENT(k2d03),PRESENT(k2d04),PRESENT(k2d05),   &
         &                 PRESENT(k2d06),PRESENT(k2d07),PRESENT(k2d08),PRESENT(k2d09),PRESENT(k2d10) /) )
      DO jn = 1, icnt   ;   CALL wrk_deallocbase( jpinteger, kidim, kjdim, 0, 0, kistart, kjstart, 1, 1 )   ;   END DO
      !
   END SUBROUTINE wrk_dealloc_2di


   SUBROUTINE wrk_dealloc_3dr( kidim, kjdim, kkdim, p3d01, p3d02, p3d03, p3d04, p3d05, p3d06, p3d07, p3d08, p3d09, p3d10,   &
      &                        kistart, kjstart, kkstart )
      INTEGER                            , INTENT(in   )           ::   kidim, kjdim, kkdim   ! dimensions size
      REAL(wp), POINTER, DIMENSION(:,:,:), INTENT(inout)           ::   p3d01
      REAL(wp), POINTER, DIMENSION(:,:,:), INTENT(inout), OPTIONAL ::   p3d02,p3d03,p3d04,p3d05,p3d06,p3d07,p3d08,p3d09,p3d10
      INTEGER                            , INTENT(in   ), OPTIONAL ::   kistart, kjstart, kkstart
      !
      INTEGER :: icnt, jn
      icnt = 1 + COUNT( (/                PRESENT(p3d02),PRESENT(p3d03),PRESENT(p3d04),PRESENT(p3d05),   &
         &                 PRESENT(p3d06),PRESENT(p3d07),PRESENT(p3d08),PRESENT(p3d09),PRESENT(p3d10) /) )
      DO jn = 1, icnt   ;   CALL wrk_deallocbase( jpreal, kidim, kjdim, kkdim, 0, kistart, kjstart, kkstart, 1 )   ;   END DO
      !
   END SUBROUTINE wrk_dealloc_3dr


   SUBROUTINE wrk_dealloc_3di( kidim, kjdim, kkdim, k3d01, k3d02, k3d03, k3d04, k3d05, k3d06, k3d07, k3d08, k3d09, k3d10,   &
      &                        kistart, kjstart, kkstart )
      INTEGER                            , INTENT(in   )           ::   kidim, kjdim, kkdim   ! dimensions size
      INTEGER , POINTER, DIMENSION(:,:,:), INTENT(inout)           ::   k3d01
      INTEGER , POINTER, DIMENSION(:,:,:), INTENT(inout), OPTIONAL ::   k3d02,k3d03,k3d04,k3d05,k3d06,k3d07,k3d08,k3d09,k3d10
      INTEGER                            , INTENT(in   ), OPTIONAL ::   kistart, kjstart, kkstart
      !
      INTEGER :: icnt, jn
      icnt = 1 + COUNT( (/                PRESENT(k3d02),PRESENT(k3d03),PRESENT(k3d04),PRESENT(k3d05),   &
         &                 PRESENT(k3d06),PRESENT(k3d07),PRESENT(k3d08),PRESENT(k3d09),PRESENT(k3d10) /) )
      DO jn = 1, icnt   ;   CALL wrk_deallocbase( jpinteger, kidim, kjdim, kkdim, 0, kistart, kjstart, kkstart, 1 )   ;   END DO
      !
   END SUBROUTINE wrk_dealloc_3di


   SUBROUTINE wrk_dealloc_4dr( kidim, kjdim, kkdim, kldim, p4d01, p4d02, p4d03, p4d04, p4d05, p4d06, p4d07, p4d08, p4d09, p4d10,   &
      &                        kistart, kjstart, kkstart, klstart )
      INTEGER                              , INTENT(in   )           ::   kidim, kjdim, kkdim, kldim   ! dimensions size
      REAL(wp), POINTER, DIMENSION(:,:,:,:), INTENT(inout)           ::   p4d01
      REAL(wp), POINTER, DIMENSION(:,:,:,:), INTENT(inout), OPTIONAL ::   p4d02,p4d03,p4d04,p4d05,p4d06,p4d07,p4d08,p4d09,p4d10
      INTEGER                              , INTENT(in   ), OPTIONAL ::   kistart, kjstart, kkstart, klstart
      !
      INTEGER :: icnt, jn
      icnt = 1 + COUNT( (/                PRESENT(p4d02),PRESENT(p4d03),PRESENT(p4d04),PRESENT(p4d05),   &
         &                 PRESENT(p4d06),PRESENT(p4d07),PRESENT(p4d08),PRESENT(p4d09),PRESENT(p4d10) /) )
      DO jn = 1, icnt ; CALL wrk_deallocbase( jpreal, kidim, kjdim, kkdim, kldim, kistart, kjstart, kkstart, klstart ) ; END DO
      !
   END SUBROUTINE wrk_dealloc_4dr


   SUBROUTINE wrk_dealloc_4di( kidim, kjdim, kkdim, kldim, k4d01, k4d02, k4d03, k4d04, k4d05, k4d06, k4d07, k4d08, k4d09, k4d10,   &
      &                        kistart, kjstart, kkstart, klstart )
      INTEGER                              , INTENT(in   )           ::   kidim, kjdim, kkdim, kldim   ! dimensions size
      INTEGER , POINTER, DIMENSION(:,:,:,:), INTENT(inout)           ::   k4d01
      INTEGER , POINTER, DIMENSION(:,:,:,:), INTENT(inout), OPTIONAL ::   k4d02,k4d03,k4d04,k4d05,k4d06,k4d07,k4d08,k4d09,k4d10
      INTEGER                              , INTENT(in   ), OPTIONAL ::   kistart, kjstart, kkstart, klstart
      !
      INTEGER :: icnt, jn
      icnt = 1 + COUNT( (/                PRESENT(k4d02),PRESENT(k4d03),PRESENT(k4d04),PRESENT(k4d05),   &
         &                 PRESENT(k4d06),PRESENT(k4d07),PRESENT(k4d08),PRESENT(k4d09),PRESENT(k4d10) /) )
      DO jn = 1, icnt ; CALL wrk_deallocbase( jpinteger, kidim, kjdim, kkdim, kldim, kistart, kjstart, kkstart, klstart ) ; END DO
      !
   END SUBROUTINE wrk_dealloc_4di


   SUBROUTINE wrk_alloc_xd( kidim, kjdim, kkdim, kldim,                                             &
      &                     kisrt, kjsrt, kksrt, klsrt,                                             &
      &                     k1d01, k1d02, k1d03, k1d04, k1d05, k1d06, k1d07, k1d08, k1d09, k1d10,   &
      &                     k2d01, k2d02, k2d03, k2d04, k2d05, k2d06, k2d07, k2d08, k2d09, k2d10,   &
      &                     k3d01, k3d02, k3d03, k3d04, k3d05, k3d06, k3d07, k3d08, k3d09, k3d10,   &
      &                     k4d01, k4d02, k4d03, k4d04, k4d05, k4d06, k4d07, k4d08, k4d09, k4d10,   &
      &                     p1d01, p1d02, p1d03, p1d04, p1d05, p1d06, p1d07, p1d08, p1d09, p1d10,   &
      &                     p2d01, p2d02, p2d03, p2d04, p2d05, p2d06, p2d07, p2d08, p2d09, p2d10,   &
      &                     p3d01, p3d02, p3d03, p3d04, p3d05, p3d06, p3d07, p3d08, p3d09, p3d10,   &
      &                     p4d01, p4d02, p4d03, p4d04, p4d05, p4d06, p4d07, p4d08, p4d09, p4d10    )
      INTEGER                              ,INTENT(in   )         ::   kidim, kjdim, kkdim, kldim   ! dimensions size
      INTEGER                              ,INTENT(in   ),OPTIONAL::   kisrt, kjsrt, kksrt, klsrt
      INTEGER , POINTER, DIMENSION(:      ),INTENT(inout),OPTIONAL::   k1d01,k1d02,k1d03,k1d04,k1d05,k1d06,k1d07,k1d08,k1d09,k1d10
      INTEGER , POINTER, DIMENSION(:,:    ),INTENT(inout),OPTIONAL::   k2d01,k2d02,k2d03,k2d04,k2d05,k2d06,k2d07,k2d08,k2d09,k2d10
      INTEGER , POINTER, DIMENSION(:,:,:  ),INTENT(inout),OPTIONAL::   k3d01,k3d02,k3d03,k3d04,k3d05,k3d06,k3d07,k3d08,k3d09,k3d10
      INTEGER , POINTER, DIMENSION(:,:,:,:),INTENT(inout),OPTIONAL::   k4d01,k4d02,k4d03,k4d04,k4d05,k4d06,k4d07,k4d08,k4d09,k4d10
      REAL(wp), POINTER, DIMENSION(:      ),INTENT(inout),OPTIONAL::   p1d01,p1d02,p1d03,p1d04,p1d05,p1d06,p1d07,p1d08,p1d09,p1d10
      REAL(wp), POINTER, DIMENSION(:,:    ),INTENT(inout),OPTIONAL::   p2d01,p2d02,p2d03,p2d04,p2d05,p2d06,p2d07,p2d08,p2d09,p2d10
      REAL(wp), POINTER, DIMENSION(:,:,:  ),INTENT(inout),OPTIONAL::   p3d01,p3d02,p3d03,p3d04,p3d05,p3d06,p3d07,p3d08,p3d09,p3d10
      REAL(wp), POINTER, DIMENSION(:,:,:,:),INTENT(inout),OPTIONAL::   p4d01,p4d02,p4d03,p4d04,p4d05,p4d06,p4d07,p4d08,p4d09,p4d10
      !
      LOGICAL ::   llpres
      INTEGER ::   jn, iisrt, ijsrt, iksrt, ilsrt
      !
      IF( .NOT. linit ) THEN
         tree(:)%itype = jpnotdefined
         DO jn = 1, jparray   ;   tree(jn)%ishape(:) = 0   ;   tree(jn)%istart(:) = 0   ;   END DO
         linit = .TRUE.
      ENDIF

      IF( PRESENT(kisrt) ) THEN   ;   iisrt =  kisrt   ;   ELSE   ;   iisrt = 1   ;   ENDIF 
      IF( PRESENT(kjsrt) ) THEN   ;   ijsrt =  kjsrt   ;   ELSE   ;   ijsrt = 1   ;   ENDIF 
      IF( PRESENT(kksrt) ) THEN   ;   iksrt =  kksrt   ;   ELSE   ;   iksrt = 1   ;   ENDIF 
      IF( PRESENT(klsrt) ) THEN   ;   ilsrt =  klsrt   ;   ELSE   ;   ilsrt = 1   ;   ENDIF 

      llpres =  PRESENT(k1d01) .OR. PRESENT(k2d01) .OR. PRESENT(k3d01) .OR. PRESENT(k4d01)   &
         & .OR. PRESENT(p1d01) .OR. PRESENT(p2d01) .OR. PRESENT(p3d01) .OR. PRESENT(p4d01)
      IF( llpres ) CALL wrk_allocbase( kidim, kjdim, kkdim, kldim, iisrt, ijsrt, iksrt, ilsrt,   &
         &                             k1d01, k2d01, k3d01, k4d01, p1d01, p2d01, p3d01, p4d01    )
      llpres =  PRESENT(k1d02) .OR. PRESENT(k2d02) .OR. PRESENT(k3d02) .OR. PRESENT(k4d02)   &
         & .OR. PRESENT(p1d02) .OR. PRESENT(p2d02) .OR. PRESENT(p3d02) .OR. PRESENT(p4d02)
      IF( llpres ) CALL wrk_allocbase( kidim, kjdim, kkdim, kldim, iisrt, ijsrt, iksrt, ilsrt,   &
         &                             k1d02, k2d02, k3d02, k4d02, p1d02, p2d02, p3d02, p4d02    )
      llpres =  PRESENT(k1d03) .OR. PRESENT(k2d03) .OR. PRESENT(k3d03) .OR. PRESENT(k4d03)   &
         & .OR. PRESENT(p1d03) .OR. PRESENT(p2d03) .OR. PRESENT(p3d03) .OR. PRESENT(p4d03)
      IF( llpres ) CALL wrk_allocbase( kidim, kjdim, kkdim, kldim, iisrt, ijsrt, iksrt, ilsrt,   &
         &                             k1d03, k2d03, k3d03, k4d03, p1d03, p2d03, p3d03, p4d03    )
      llpres =  PRESENT(k1d04) .OR. PRESENT(k2d04) .OR. PRESENT(k3d04) .OR. PRESENT(k4d04)   &
         & .OR. PRESENT(p1d04) .OR. PRESENT(p2d04) .OR. PRESENT(p3d04) .OR. PRESENT(p4d04)
      IF( llpres ) CALL wrk_allocbase( kidim, kjdim, kkdim, kldim, iisrt, ijsrt, iksrt, ilsrt,   &
         &                             k1d04, k2d04, k3d04, k4d04, p1d04, p2d04, p3d04, p4d04    )
      llpres =  PRESENT(k1d05) .OR. PRESENT(k2d05) .OR. PRESENT(k3d05) .OR. PRESENT(k4d05)   &
         & .OR. PRESENT(p1d05) .OR. PRESENT(p2d05) .OR. PRESENT(p3d05) .OR. PRESENT(p4d05)
      IF( llpres ) CALL wrk_allocbase( kidim, kjdim, kkdim, kldim, iisrt, ijsrt, iksrt, ilsrt,   &
         &                             k1d05, k2d05, k3d05, k4d05, p1d05, p2d05, p3d05, p4d05    )
      llpres =  PRESENT(k1d06) .OR. PRESENT(k2d06) .OR. PRESENT(k3d06) .OR. PRESENT(k4d06)   &
         & .OR. PRESENT(p1d06) .OR. PRESENT(p2d06) .OR. PRESENT(p3d06) .OR. PRESENT(p4d06)
      IF( llpres ) CALL wrk_allocbase( kidim, kjdim, kkdim, kldim, iisrt, ijsrt, iksrt, ilsrt,   &
         &                             k1d06, k2d06, k3d06, k4d06, p1d06, p2d06, p3d06, p4d06    )
      llpres =  PRESENT(k1d07) .OR. PRESENT(k2d07) .OR. PRESENT(k3d07) .OR. PRESENT(k4d07)   &
         & .OR. PRESENT(p1d07) .OR. PRESENT(p2d07) .OR. PRESENT(p3d07) .OR. PRESENT(p4d07)
      IF( llpres ) CALL wrk_allocbase( kidim, kjdim, kkdim, kldim, iisrt, ijsrt, iksrt, ilsrt,   &
         &                             k1d07, k2d07, k3d07, k4d07, p1d07, p2d07, p3d07, p4d07    )
      llpres =  PRESENT(k1d08) .OR. PRESENT(k2d08) .OR. PRESENT(k3d08) .OR. PRESENT(k4d08)   &
         & .OR. PRESENT(p1d08) .OR. PRESENT(p2d08) .OR. PRESENT(p3d08) .OR. PRESENT(p4d08)
      IF( llpres ) CALL wrk_allocbase( kidim, kjdim, kkdim, kldim, iisrt, ijsrt, iksrt, ilsrt,   &
         &                             k1d08, k2d08, k3d08, k4d08, p1d08, p2d08, p3d08, p4d08    )
      llpres =  PRESENT(k1d09) .OR. PRESENT(k2d09) .OR. PRESENT(k3d09) .OR. PRESENT(k4d09)   &
         & .OR. PRESENT(p1d09) .OR. PRESENT(p2d09) .OR. PRESENT(p3d09) .OR. PRESENT(p4d09)
      IF( llpres ) CALL wrk_allocbase( kidim, kjdim, kkdim, kldim, iisrt, ijsrt, iksrt, ilsrt,   &
         &                             k1d09, k2d09, k3d09, k4d09, p1d09, p2d09, p3d09, p4d09    )
      llpres =  PRESENT(k1d10) .OR. PRESENT(k2d10) .OR. PRESENT(k3d10) .OR. PRESENT(k4d10)   &
         & .OR. PRESENT(p1d10) .OR. PRESENT(p2d10) .OR. PRESENT(p3d10) .OR. PRESENT(p4d10)
      IF( llpres ) CALL wrk_allocbase( kidim, kjdim, kkdim, kldim, iisrt, ijsrt, iksrt, ilsrt,   &
         &                             k1d10, k2d10, k3d10, k4d10, p1d10, p2d10, p3d10, p4d10    )

   END SUBROUTINE wrk_alloc_xd


   SUBROUTINE wrk_allocbase( kidim , kjdim , kkdim , kldim , kisrt , kjsrt , kksrt , klsrt ,   &
      &                      kwrk1d, kwrk2d, kwrk3d, kwrk4d, pwrk1d, pwrk2d, pwrk3d, pwrk4d    )
      INTEGER                              , INTENT(in   )           :: kidim, kjdim, kkdim, kldim
      INTEGER                              , INTENT(in   )           :: kisrt, kjsrt, kksrt, klsrt
      INTEGER , POINTER, DIMENSION(:)      , INTENT(inout), OPTIONAL :: kwrk1d  
      INTEGER , POINTER, DIMENSION(:,:)    , INTENT(inout), OPTIONAL :: kwrk2d  
      INTEGER , POINTER, DIMENSION(:,:,:)  , INTENT(inout), OPTIONAL :: kwrk3d  
      INTEGER , POINTER, DIMENSION(:,:,:,:), INTENT(inout), OPTIONAL :: kwrk4d  
      REAL(wp), POINTER, DIMENSION(:)      , INTENT(inout), OPTIONAL :: pwrk1d  
      REAL(wp), POINTER, DIMENSION(:,:)    , INTENT(inout), OPTIONAL :: pwrk2d  
      REAL(wp), POINTER, DIMENSION(:,:,:)  , INTENT(inout), OPTIONAL :: pwrk3d  
      REAL(wp), POINTER, DIMENSION(:,:,:,:), INTENT(inout), OPTIONAL :: pwrk4d  
      !
      INTEGER, DIMENSION(jpmaxdim) :: ishape, isrt, iend
      INTEGER :: itype
      INTEGER :: ii

      ! define the shape to be given to the work array
      ishape(:) = (/ kidim, kjdim, kkdim, kldim /)
      ! define the starting index of the dimension shape to be given to the work array
      isrt  (:) = (/ kisrt, kjsrt, kksrt, klsrt /)
      iend  (:) = ishape(:) + isrt(:) - 1

      ! is it integer or real array?
      IF( PRESENT(kwrk1d) .OR. PRESENT(kwrk2d) .OR. PRESENT(kwrk3d) .OR. PRESENT(kwrk4d) )   itype = jpinteger   
      IF( PRESENT(pwrk1d) .OR. PRESENT(pwrk2d) .OR. PRESENT(pwrk3d) .OR. PRESENT(pwrk4d) )   itype = jpreal         

      ! find the branch with the matching shape, staring index and type or get the first "free" branch 
      ii = 1                          
      DO WHILE(       ( ANY( tree(ii)%ishape /= ishape ) .OR. ANY( tree(ii)%istart /= isrt ) .OR. tree(ii)%itype /= itype )   &
         &      .AND. SUM( tree(ii)%ishape ) /= 0 )
         ii = ii + 1
         IF (ii > jparray) STOP   ! increase the value of jparray (should not be needed as already very big!)
      END DO
      
      IF( SUM( tree(ii)%ishape ) == 0 ) THEN                    ! create a new branch 
         tree(ii)%itype = itype                                        ! define the type of this branch 
         tree(ii)%ishape(:) = ishape(:)                                ! define the shape of this branch 
         tree(ii)%istart(:) = isrt(:)                                  ! define the lower bounds of this branch 
         ALLOCATE( tree(ii)%start )                                    ! allocate its start
         ALLOCATE( tree(ii)%current)                                   ! allocate the current leaf (the first leaf)

         tree(ii)%start%in_use = .FALSE.                               ! Never use the start as work array
         tree(ii)%start%indic = 0
         tree(ii)%start%prev => NULL()                                 ! nothing before the start
         tree(ii)%start%next => tree(ii)%current                       ! first leaf link to the start
         
         tree(ii)%current%in_use = .FALSE.                             ! first leaf is not yet used
         tree(ii)%current%indic = 1                                    ! first leaf
         tree(ii)%current%prev => tree(ii)%start                       ! previous leaf is the start
         tree(ii)%current%next => NULL()                               ! next leaf is not yet defined
         ! allocate the array of the first leaf
         IF( PRESENT(kwrk1d) ) ALLOCATE( tree(ii)%current%iwrk1d(isrt(1):iend(1)                                                ) )
         IF( PRESENT(kwrk2d) ) ALLOCATE( tree(ii)%current%iwrk2d(isrt(1):iend(1),isrt(2):iend(2)                                ) )
         IF( PRESENT(kwrk3d) ) ALLOCATE( tree(ii)%current%iwrk3d(isrt(1):iend(1),isrt(2):iend(2),isrt(3):iend(3)                ) )
         IF( PRESENT(kwrk4d) ) ALLOCATE( tree(ii)%current%iwrk4d(isrt(1):iend(1),isrt(2):iend(2),isrt(3):iend(3),isrt(4):iend(4)) )
         IF( PRESENT(pwrk1d) ) ALLOCATE( tree(ii)%current%zwrk1d(isrt(1):iend(1)                                                ) )
         IF( PRESENT(pwrk2d) ) ALLOCATE( tree(ii)%current%zwrk2d(isrt(1):iend(1),isrt(2):iend(2)                                ) )
         IF( PRESENT(pwrk3d) ) ALLOCATE( tree(ii)%current%zwrk3d(isrt(1):iend(1),isrt(2):iend(2),isrt(3):iend(3)                ) )
         IF( PRESENT(pwrk4d) ) ALLOCATE( tree(ii)%current%zwrk4d(isrt(1):iend(1),isrt(2):iend(2),isrt(3):iend(3),isrt(4):iend(4)) )
                  
      ELSE IF( .NOT. ASSOCIATED(tree(ii)%current%next) ) THEN   ! all leafs used -> define a new one
         ALLOCATE( tree(ii)%current%next )                             ! allocate the new leaf
         tree(ii)%current%next%in_use = .FALSE.                        ! this leaf is not yet used
         tree(ii)%current%next%indic = tree(ii)%current%indic + 1      ! number of this leaf
         tree(ii)%current%next%prev => tree(ii)%current                ! previous leaf of the new leaf is the current leaf
         tree(ii)%current%next%next => NULL()                          ! next leaf is not yet defined

         tree(ii)%current => tree(ii)%current%next                     ! the current leaf becomes the new one 
 
         ! allocate the array of the new leaf
         IF( PRESENT(kwrk1d) ) ALLOCATE( tree(ii)%current%iwrk1d(isrt(1):iend(1)                                                ) )
         IF( PRESENT(kwrk2d) ) ALLOCATE( tree(ii)%current%iwrk2d(isrt(1):iend(1),isrt(2):iend(2)                                ) )
         IF( PRESENT(kwrk3d) ) ALLOCATE( tree(ii)%current%iwrk3d(isrt(1):iend(1),isrt(2):iend(2),isrt(3):iend(3)                ) )
         IF( PRESENT(kwrk4d) ) ALLOCATE( tree(ii)%current%iwrk4d(isrt(1):iend(1),isrt(2):iend(2),isrt(3):iend(3),isrt(4):iend(4)) )
         IF( PRESENT(pwrk1d) ) ALLOCATE( tree(ii)%current%zwrk1d(isrt(1):iend(1)                                                ) )
         IF( PRESENT(pwrk2d) ) ALLOCATE( tree(ii)%current%zwrk2d(isrt(1):iend(1),isrt(2):iend(2)                                ) )
         IF( PRESENT(pwrk3d) ) ALLOCATE( tree(ii)%current%zwrk3d(isrt(1):iend(1),isrt(2):iend(2),isrt(3):iend(3)                ) )
         IF( PRESENT(pwrk4d) ) ALLOCATE( tree(ii)%current%zwrk4d(isrt(1):iend(1),isrt(2):iend(2),isrt(3):iend(3),isrt(4):iend(4)) )
         
      ELSE 
         tree(ii)%current => tree(ii)%current%next                     ! the current leaf becomes the next one 
      ENDIF   
      !       
      ! use the array of the current leaf as a work array
      IF( PRESENT(kwrk1d) ) kwrk1d => tree(ii)%current%iwrk1d   
      IF( PRESENT(kwrk2d) ) kwrk2d => tree(ii)%current%iwrk2d   
      IF( PRESENT(kwrk3d) ) kwrk3d => tree(ii)%current%iwrk3d   
      IF( PRESENT(kwrk4d) ) kwrk4d => tree(ii)%current%iwrk4d   
      IF( PRESENT(pwrk1d) ) pwrk1d => tree(ii)%current%zwrk1d   
      IF( PRESENT(pwrk2d) ) pwrk2d => tree(ii)%current%zwrk2d   
      IF( PRESENT(pwrk3d) ) pwrk3d => tree(ii)%current%zwrk3d   
      IF( PRESENT(pwrk4d) ) pwrk4d => tree(ii)%current%zwrk4d   
      tree(ii)%current%in_use = .TRUE.   ! this leaf is now used
      !      
   END SUBROUTINE wrk_allocbase


   SUBROUTINE wrk_deallocbase( ktype, kidim, kjdim, kkdim, kldim, kisrt, kjsrt, kksrt, klsrt )
      INTEGER, INTENT(in   )           :: ktype
      INTEGER, INTENT(in   )           :: kidim, kjdim, kkdim, kldim
      INTEGER, INTENT(in   ), OPTIONAL :: kisrt, kjsrt, kksrt, klsrt
      !
      INTEGER, DIMENSION(jpmaxdim) :: ishape, istart
      INTEGER :: ii

      ishape(:) = (/ kidim, kjdim, kkdim, kldim /)
      IF( PRESENT(kisrt) ) THEN   ;   istart(1) =  kisrt   ;   ELSE   ;   istart(1) = 1   ;   ENDIF 
      IF( PRESENT(kjsrt) ) THEN   ;   istart(2) =  kjsrt   ;   ELSE   ;   istart(2) = 1   ;   ENDIF 
      IF( PRESENT(kksrt) ) THEN   ;   istart(3) =  kksrt   ;   ELSE   ;   istart(3) = 1   ;   ENDIF 
      IF( PRESENT(klsrt) ) THEN   ;   istart(4) =  klsrt   ;   ELSE   ;   istart(4) = 1   ;   ENDIF 

      ! find the branch with the matcing shape and type or get the first "free" branch 
      ii = 1                          
      DO WHILE( ANY( tree(ii)%ishape /= ishape ) .OR. ANY( tree(ii)%istart /= istart ) .OR. tree(ii)%itype /= ktype )
         ii = ii + 1
      END DO
      !
      tree(ii)%current%in_use = .FALSE.           ! current leaf is no more used
      tree(ii)%current => tree(ii)%current%prev   ! move back toward previous leaf   
      ! 
   END SUBROUTINE wrk_deallocbase


   SUBROUTINE wrk_stop(cmsg)
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE wrk_stop  ***
      !! ** Purpose :   to act as local alternative to ctl_stop. 
      !!                Avoids dependency on in_out_manager module.
      !!----------------------------------------------------------------------
      CHARACTER(LEN=*), INTENT(in) :: cmsg
      !!----------------------------------------------------------------------
      !
!      WRITE(kumout, cform_err2)
      WRITE(*,*) TRIM(cmsg)
      ! ARPDBG - would like to CALL mppstop here to force a stop but that
      ! introduces a dependency on lib_mpp. Could CALL mpi_abort() directly
      ! but that's fairly brutal. Better to rely on CALLing routine to
      ! deal with the error passed back from the wrk_X routine?
      !CALL mppstop
      !
   END SUBROUTINE wrk_stop

   !!=====================================================================
END MODULE wrk_nemo
