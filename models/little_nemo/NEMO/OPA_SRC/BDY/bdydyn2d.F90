MODULE bdydyn2d
   !!======================================================================
   !!                       ***  MODULE  bdydyn  ***
   !! Unstructured Open Boundary Cond. :   Apply boundary conditions to barotropic solution
   !!======================================================================
   !! History :  3.4  !  2011     (D. Storkey) new module as part of BDY rewrite
   !!----------------------------------------------------------------------
#if defined key_bdy 
   !!----------------------------------------------------------------------
   !!   'key_bdy' :                    Unstructured Open Boundary Condition
   !!----------------------------------------------------------------------
   !!   bdy_dyn2d      : Apply open boundary conditions to barotropic variables.
   !!   bdy_dyn2d_fla    : Apply Flather condition
   !!----------------------------------------------------------------------
   USE timing          ! Timing
   USE oce             ! ocean dynamics and tracers 
   USE dom_oce         ! ocean space and time domain
   USE bdy_oce         ! ocean open boundary conditions
   USE dynspg_oce      ! for barotropic variables
   USE phycst          ! physical constants
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE in_out_manager  !

   IMPLICIT NONE
   PRIVATE

   PUBLIC   bdy_dyn2d     ! routine called in dynspg_ts and bdy_dyn

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: bdydyn.F90 2528 2010-12-27 17:33:53Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE bdy_dyn2d( kt )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE bdy_dyn2d  ***
      !!
      !! ** Purpose : - Apply open boundary conditions for barotropic variables
      !!
      !!----------------------------------------------------------------------
      INTEGER,                      INTENT(in) ::   kt   ! Main time step counter
      !!
      INTEGER                                  ::   ib_bdy ! Loop counter

      DO ib_bdy=1, nb_bdy

         SELECT CASE( nn_dyn2d(ib_bdy) )
         CASE(jp_none)
            CYCLE
         CASE(jp_frs)
            CALL bdy_dyn2d_frs( idx_bdy(ib_bdy), dta_bdy(ib_bdy) )
         CASE(jp_flather)
            CALL bdy_dyn2d_fla( idx_bdy(ib_bdy), dta_bdy(ib_bdy) )
         CASE DEFAULT
            CALL ctl_stop( 'bdy_dyn2d : unrecognised option for open boundaries for barotropic variables' )
         END SELECT
      ENDDO

   END SUBROUTINE bdy_dyn2d

   SUBROUTINE bdy_dyn2d_frs( idx, dta )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE bdy_dyn2d_frs  ***
      !!
      !! ** Purpose : - Apply the Flow Relaxation Scheme for barotropic velocities
      !!                at open boundaries.
      !!
      !! References :- Engedahl H., 1995: Use of the flow relaxation scheme in 
      !!               a three-dimensional baroclinic ocean model with realistic
      !!               topography. Tellus, 365-382.
      !!----------------------------------------------------------------------
      TYPE(OBC_INDEX), INTENT(in) ::   idx  ! OBC indices
      TYPE(OBC_DATA),  INTENT(in) ::   dta  ! OBC external data
      !!
      INTEGER  ::   jb, jk         ! dummy loop indices
      INTEGER  ::   ii, ij, igrd   ! local integers
      REAL(wp) ::   zwgt           ! boundary weight
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 ) CALL timing_start('bdy_dyn2d_frs')
      !
      igrd = 2                      ! Relaxation of zonal velocity
      DO jb = 1, idx%nblen(igrd)
         ii   = idx%nbi(jb,igrd)
         ij   = idx%nbj(jb,igrd)
         zwgt = idx%nbw(jb,igrd)
         pu2d(ii,ij) = ( pu2d(ii,ij) + zwgt * ( dta%u2d(jb) - pu2d(ii,ij) ) ) * umask(ii,ij,1)
      END DO
      !
      igrd = 3                      ! Relaxation of meridional velocity
      DO jb = 1, idx%nblen(igrd)
         ii   = idx%nbi(jb,igrd)
         ij   = idx%nbj(jb,igrd)
         zwgt = idx%nbw(jb,igrd)
         pv2d(ii,ij) = ( pv2d(ii,ij) + zwgt * ( dta%v2d(jb) - pv2d(ii,ij) ) ) * vmask(ii,ij,1)
      END DO 
      CALL lbc_lnk( pu2d, 'U', -1. ) 
      CALL lbc_lnk( pv2d, 'V', -1. )   ! Boundary points should be updated
      !
      IF( nn_timing == 1 ) CALL timing_stop('bdy_dyn2d_frs')
      !

   END SUBROUTINE bdy_dyn2d_frs


   SUBROUTINE bdy_dyn2d_fla( idx, dta )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE bdy_dyn2d_fla  ***
      !!             
      !!              - Apply Flather boundary conditions on normal barotropic velocities 
      !!
      !! ** WARNINGS about FLATHER implementation:
      !!1. According to Palma and Matano, 1998 "after ssh" is used. 
      !!   In ROMS and POM implementations, it is "now ssh". In the current 
      !!   implementation (tested only in the EEL-R5 conf.), both cases were unstable. 
      !!   So I use "before ssh" in the following.
      !!
      !!2. We assume that the normal ssh gradient at the bdy is zero. As a matter of 
      !!   fact, the model ssh just inside the dynamical boundary is used (the outside  
      !!   ssh in the code is not updated).
      !!
      !! References:  Flather, R. A., 1976: A tidal model of the northwest European
      !!              continental shelf. Mem. Soc. R. Sci. Liege, Ser. 6,10, 141-164.     
      !!----------------------------------------------------------------------
      TYPE(OBC_INDEX),              INTENT(in) ::   idx  ! OBC indices
      TYPE(OBC_DATA),               INTENT(in) ::   dta  ! OBC external data

      INTEGER  ::   jb, igrd                         ! dummy loop indices
      INTEGER  ::   ii, ij, iim1, iip1, ijm1, ijp1   ! 2D addresses
      REAL(wp) ::   zcorr                            ! Flather correction
      REAL(wp) ::   zforc                            ! temporary scalar
      !!----------------------------------------------------------------------

      IF( nn_timing == 1 ) CALL timing_start('bdy_dyn2d_fla')

      ! ---------------------------------!
      ! Flather boundary conditions     :!
      ! ---------------------------------! 
     
!!! REPLACE spgu with nemo_wrk work space

      ! Fill temporary array with ssh data (here spgu):
      igrd = 1
      spgu(:,:) = 0.0
      DO jb = 1, idx%nblenrim(igrd)
         ii = idx%nbi(jb,igrd)
         ij = idx%nbj(jb,igrd)
         spgu(ii, ij) = dta%ssh(jb)
      END DO
      !
      igrd = 2      ! Flather bc on u-velocity; 
      !             ! remember that flagu=-1 if normal velocity direction is outward
      !             ! I think we should rather use after ssh ?
      DO jb = 1, idx%nblenrim(igrd)
         ii  = idx%nbi(jb,igrd)
         ij  = idx%nbj(jb,igrd) 
         iim1 = ii + MAX( 0, INT( idx%flagu(jb) ) )   ! T pts i-indice inside the boundary
         iip1 = ii - MIN( 0, INT( idx%flagu(jb) ) )   ! T pts i-indice outside the boundary 
         !
         zcorr = - idx%flagu(jb) * SQRT( grav * phur(ii, ij) ) * ( pssh(iim1, ij) - spgu(iip1,ij) )
         zforc = dta%u2d(jb)
         pu2d(ii,ij) = zforc + zcorr * umask(ii,ij,1) 
      END DO
      !
      igrd = 3      ! Flather bc on v-velocity
      !             ! remember that flagv=-1 if normal velocity direction is outward
      DO jb = 1, idx%nblenrim(igrd)
         ii  = idx%nbi(jb,igrd)
         ij  = idx%nbj(jb,igrd) 
         ijm1 = ij + MAX( 0, INT( idx%flagv(jb) ) )   ! T pts j-indice inside the boundary
         ijp1 = ij - MIN( 0, INT( idx%flagv(jb) ) )   ! T pts j-indice outside the boundary 
         !
         zcorr = - idx%flagv(jb) * SQRT( grav * phvr(ii, ij) ) * ( pssh(ii, ijm1) - spgu(ii,ijp1) )
         zforc = dta%v2d(jb)
         pv2d(ii,ij) = zforc + zcorr * vmask(ii,ij,1)
      END DO
      CALL lbc_lnk( pu2d, 'U', -1. )   ! Boundary points should be updated
      CALL lbc_lnk( pv2d, 'V', -1. )   !
      !
      IF( nn_timing == 1 ) CALL timing_stop('bdy_dyn2d_fla')
      !
   END SUBROUTINE bdy_dyn2d_fla
#else
   !!----------------------------------------------------------------------
   !!   Dummy module                   NO Unstruct Open Boundary Conditions
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE bdy_dyn2d( kt )      ! Empty routine
      WRITE(*,*) 'bdy_dyn_frs: You should not have seen this print! error?', kt
   END SUBROUTINE bdy_dyn2d
#endif

   !!======================================================================
END MODULE bdydyn2d
