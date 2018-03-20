MODULE bdytra
   !!======================================================================
   !!                       ***  MODULE  bdytra  ***
   !! Ocean tracers:   Apply boundary conditions for tracers
   !!======================================================================
   !! History :  1.0  !  2005-01  (J. Chanut, A. Sellar)  Original code
   !!            3.0  !  2008-04  (NEMO team)  add in the reference version
   !!            3.4  !  2011     (D. Storkey) rewrite in preparation for OBC-BDY merge
   !!----------------------------------------------------------------------
#if defined key_bdy
   !!----------------------------------------------------------------------
   !!   'key_bdy'                     Unstructured Open Boundary Conditions
   !!----------------------------------------------------------------------
   !!   bdy_tra            : Apply open boundary conditions to T and S
   !!   bdy_tra_frs        : Apply Flow Relaxation Scheme
   !!----------------------------------------------------------------------
   USE timing          ! Timing
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables 
   USE bdy_oce         ! ocean open boundary conditions
   USE bdydta, ONLY:   bf
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE in_out_manager  ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC bdy_tra      ! routine called in tranxt.F90 

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: bdytra.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE bdy_tra( kt )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE bdy_tra  ***
      !!
      !! ** Purpose : - Apply open boundary conditions for temperature and salinity
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt     ! Main time step counter
      !!
      INTEGER               :: ib_bdy ! Loop index

      DO ib_bdy=1, nb_bdy

         SELECT CASE( nn_tra(ib_bdy) )
         CASE(jp_none)
            CYCLE
         CASE(jp_frs)
            CALL bdy_tra_frs( idx_bdy(ib_bdy), dta_bdy(ib_bdy), kt )
         CASE DEFAULT
            CALL ctl_stop( 'bdy_tra : unrecognised option for open boundaries for T and S' )
         END SELECT
      ENDDO

   END SUBROUTINE bdy_tra

   SUBROUTINE bdy_tra_frs( idx, dta, kt )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE bdy_tra_frs  ***
      !!                    
      !! ** Purpose : Apply the Flow Relaxation Scheme for tracers at open boundaries.
      !! 
      !! Reference : Engedahl H., 1995, Tellus, 365-382.
      !!----------------------------------------------------------------------
      INTEGER,         INTENT(in) ::   kt
      TYPE(OBC_INDEX), INTENT(in) ::   idx  ! OBC indices
      TYPE(OBC_DATA),  INTENT(in) ::   dta  ! OBC external data
      !! 
      REAL(wp) ::   zwgt           ! boundary weight
      INTEGER  ::   ib, ik, igrd   ! dummy loop indices
      INTEGER  ::   ii, ij         ! 2D addresses
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 ) CALL timing_start('bdy_tra_frs')
      !
      igrd = 1                       ! Everything is at T-points here
      DO ib = 1, idx%nblen(igrd)
         DO ik = 1, jpkm1
            ii = idx%nbi(ib,igrd)
            ij = idx%nbj(ib,igrd)
            zwgt = idx%nbw(ib,igrd)
            tsa(ii,ij,ik,jp_tem) = ( tsa(ii,ij,ik,jp_tem) + zwgt * ( dta%tem(ib,ik) - tsa(ii,ij,ik,jp_tem) ) ) * tmask(ii,ij,ik)         
            tsa(ii,ij,ik,jp_sal) = ( tsa(ii,ij,ik,jp_sal) + zwgt * ( dta%sal(ib,ik) - tsa(ii,ij,ik,jp_sal) ) ) * tmask(ii,ij,ik)
         END DO
      END DO 
      !
      CALL lbc_lnk( tsa(:,:,:,jp_tem), 'T', 1. )   ; CALL lbc_lnk( tsa(:,:,:,jp_sal), 'T', 1. )    ! Boundary points should be updated
      !
      IF( kt .eq. nit000 ) CLOSE( unit = 102 )
      !
      IF( nn_timing == 1 ) CALL timing_stop('bdy_tra_frs')
      !
   END SUBROUTINE bdy_tra_frs
   
#else
   !!----------------------------------------------------------------------
   !!   Dummy module                   NO Unstruct Open Boundary Conditions
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE bdy_tra(kt)      ! Empty routine
      WRITE(*,*) 'bdy_tra: You should not have seen this print! error?', kt
   END SUBROUTINE bdy_tra
#endif

   !!======================================================================
END MODULE bdytra
