MODULE bdydyn3d
   !!======================================================================
   !!                       ***  MODULE  bdydyn3d  ***
   !! Unstructured Open Boundary Cond. :   Flow relaxation scheme on baroclinic velocities
   !!======================================================================
   !! History :  3.4  !  2011     (D. Storkey) new module as part of BDY rewrite 
   !!----------------------------------------------------------------------
#if defined key_bdy 
   !!----------------------------------------------------------------------
   !!   'key_bdy' :                    Unstructured Open Boundary Condition
   !!----------------------------------------------------------------------
   !!   bdy_dyn3d        : apply open boundary conditions to baroclinic velocities
   !!   bdy_dyn3d_frs    : apply Flow Relaxation Scheme
   !!----------------------------------------------------------------------
   USE timing          ! Timing
   USE oce             ! ocean dynamics and tracers 
   USE dom_oce         ! ocean space and time domain
   USE bdy_oce         ! ocean open boundary conditions
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE in_out_manager  !

   IMPLICIT NONE
   PRIVATE

   PUBLIC   bdy_dyn3d     ! routine called by bdy_dyn

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: bdydyn.F90 2528 2010-12-27 17:33:53Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE bdy_dyn3d( kt )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE bdy_dyn3d  ***
      !!
      !! ** Purpose : - Apply open boundary conditions for baroclinic velocities
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt     ! Main time step counter
      !!
      INTEGER               :: ib_bdy ! loop index
      !!

      DO ib_bdy=1, nb_bdy

!!$         IF ( using Orlanski radiation conditions ) THEN 
!!$            CALL bdy_rad( kt,  bdyidx(ib_bdy) )
!!$         ENDIF

         SELECT CASE( nn_dyn3d(ib_bdy) )
         CASE(jp_none)
            CYCLE
         CASE(jp_frs)
            CALL bdy_dyn3d_frs( idx_bdy(ib_bdy), dta_bdy(ib_bdy), kt )
         CASE DEFAULT
            CALL ctl_stop( 'bdy_dyn3d : unrecognised option for open boundaries for baroclinic velocities' )
         END SELECT
      ENDDO

   END SUBROUTINE bdy_dyn3d

   SUBROUTINE bdy_dyn3d_frs( idx, dta, kt )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE bdy_dyn3d_frs  ***
      !!
      !! ** Purpose : - Apply the Flow Relaxation Scheme for baroclinic velocities
      !!                at open boundaries.
      !!
      !! References :- Engedahl H., 1995: Use of the flow relaxation scheme in 
      !!               a three-dimensional baroclinic ocean model with realistic
      !!               topography. Tellus, 365-382.
      !!----------------------------------------------------------------------
      INTEGER                     ::   kt
      TYPE(OBC_INDEX), INTENT(in) ::   idx  ! OBC indices
      TYPE(OBC_DATA),  INTENT(in) ::   dta  ! OBC external data
      !!
      INTEGER  ::   jb, jk         ! dummy loop indices
      INTEGER  ::   ii, ij, igrd   ! local integers
      REAL(wp) ::   zwgt           ! boundary weight
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 ) CALL timing_start('bdy_dyn3d_frs')
      !
      igrd = 2                      ! Relaxation of zonal velocity
      DO jb = 1, idx%nblen(igrd)
         DO jk = 1, jpkm1
            ii   = idx%nbi(jb,igrd)
            ij   = idx%nbj(jb,igrd)
            zwgt = idx%nbw(jb,igrd)
            ua(ii,ij,jk) = ( ua(ii,ij,jk) + zwgt * ( dta%u3d(jb,jk) - ua(ii,ij,jk) ) ) * umask(ii,ij,jk)
         END DO
      END DO
      !
      igrd = 3                      ! Relaxation of meridional velocity
      DO jb = 1, idx%nblen(igrd)
         DO jk = 1, jpkm1
            ii   = idx%nbi(jb,igrd)
            ij   = idx%nbj(jb,igrd)
            zwgt = idx%nbw(jb,igrd)
            va(ii,ij,jk) = ( va(ii,ij,jk) + zwgt * ( dta%v3d(jb,jk) - va(ii,ij,jk) ) ) * vmask(ii,ij,jk)
         END DO
      END DO 
      CALL lbc_lnk( ua, 'U', -1. )   ;   CALL lbc_lnk( va, 'V', -1. )   ! Boundary points should be updated
      !
      IF( kt .eq. nit000 ) CLOSE( unit = 102 )

      IF( nn_timing == 1 ) CALL timing_stop('bdy_dyn3d_frs')

   END SUBROUTINE bdy_dyn3d_frs


#else
   !!----------------------------------------------------------------------
   !!   Dummy module                   NO Unstruct Open Boundary Conditions
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE bdy_dyn3d( kt )      ! Empty routine
      WRITE(*,*) 'bdy_dyn_frs: You should not have seen this print! error?', kt
   END SUBROUTINE bdy_dyn3d
#endif

   !!======================================================================
END MODULE bdydyn3d
