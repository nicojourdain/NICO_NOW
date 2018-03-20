MODULE bdyice_lim2
   !!======================================================================
   !!                       ***  MODULE  bdyice_lim2  ***
   !! Unstructured Open Boundary Cond. :  Open boundary conditions for sea-ice (LIM2)
   !!======================================================================
   !!  History :  3.3  !  2010-09 (D. Storkey)  Original code
   !!             3.4  !  2011    (D. Storkey) rewrite in preparation for OBC-BDY merge
   !!----------------------------------------------------------------------
#if defined   key_bdy   &&   defined key_lim2
   !!----------------------------------------------------------------------
   !!   'key_bdy'            and                 Unstructured Open Boundary Conditions
   !!   'key_lim2'                                                 LIM-2 sea ice model
   !!----------------------------------------------------------------------
   !!   bdy_ice_lim_2      : Application of open boundaries to ice
   !!   bdy_ice_frs        : Application of Flow Relaxation Scheme
   !!----------------------------------------------------------------------
   USE timing          ! Timing
   USE oce             ! ocean dynamics and tracers variables
   USE ice_2           ! LIM_2 ice variables
   USE dom_oce         ! ocean space and time domain variables 
   USE bdy_oce         ! ocean open boundary conditions
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE in_out_manager  ! write to numout file
   USE lib_mpp         ! distributed memory computing
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   bdy_ice_lim_2    ! routine called in sbcmod

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: bdyice.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE bdy_ice_lim_2( kt )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE bdy_ice_lim_2  ***
      !!
      !! ** Purpose : - Apply open boundary conditions for ice (LIM2)
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt     ! Main time step counter
      !!
      INTEGER               :: ib_bdy ! Loop index

      DO ib_bdy=1, nb_bdy

         SELECT CASE( nn_ice_lim2(ib_bdy) )
         CASE(jp_none)
            CYCLE
         CASE(jp_frs)
            CALL bdy_ice_frs( idx_bdy(ib_bdy), dta_bdy(ib_bdy) )
         CASE DEFAULT
            CALL ctl_stop( 'bdy_ice_lim_2 : unrecognised option for open boundaries for ice fields' )
         END SELECT
      ENDDO

   END SUBROUTINE bdy_ice_lim_2

   SUBROUTINE bdy_ice_frs( idx, dta )
      !!------------------------------------------------------------------------------
      !!                 ***  SUBROUTINE bdy_ice_frs  ***
      !!                    
      !! ** Purpose : Apply the Flow Relaxation Scheme for sea-ice fields in the case 
      !!              of unstructured open boundaries. Currently only tested for LIM2.
      !! 
      !! Reference : Engedahl H., 1995: Use of the flow relaxation scheme in a three-
      !!             dimensional baroclinic ocean model with realistic topography. Tellus, 365-382.
      !!------------------------------------------------------------------------------
      TYPE(OBC_INDEX), INTENT(in) ::   idx  ! OBC indices
      TYPE(OBC_DATA),  INTENT(in) ::   dta  ! OBC external data
      !!
      INTEGER  ::   jb, jk, jgrd   ! dummy loop indices
      INTEGER  ::   ii, ij         ! local scalar
      REAL(wp) ::   zwgt, zwgt1    ! local scalar
      !!------------------------------------------------------------------------------
      !
      IF( nn_timing == 1 ) CALL timing_start('bdy_ice_frs')
      !
      jgrd = 1      ! Everything is at T-points here
      !
      DO jb = 1, idx%nblen(jgrd)
         DO jk = 1, jpkm1
            ii    = idx%nbi(jb,jgrd)
            ij    = idx%nbj(jb,jgrd)
            zwgt  = idx%nbw(jb,jgrd)
            zwgt1 = 1.e0 - idx%nbw(jb,jgrd)
            frld (ii,ij) = ( frld (ii,ij) * zwgt1 + dta%frld (jb) * zwgt ) * tmask(ii,ij,1)     ! Leads fraction 
            hicif(ii,ij) = ( hicif(ii,ij) * zwgt1 + dta%hicif(jb) * zwgt ) * tmask(ii,ij,1)     ! Ice depth 
            hsnif(ii,ij) = ( hsnif(ii,ij) * zwgt1 + dta%hsnif(jb) * zwgt ) * tmask(ii,ij,1)     ! Snow depth
         END DO
      END DO 
      CALL lbc_lnk( frld, 'T', 1. )                                         ! lateral boundary conditions
      CALL lbc_lnk( hicif, 'T', 1. )   ;   CALL lbc_lnk( hsnif, 'T', 1. )
      !      
      IF( nn_timing == 1 ) CALL timing_stop('bdy_ice_frs')
      !
   END SUBROUTINE bdy_ice_frs
#else
   !!---------------------------------------------------------------------------------
   !!   Default option                                                    Empty module
   !!---------------------------------------------------------------------------------
CONTAINS
   SUBROUTINE bdy_ice_lim_2( kt )      ! Empty routine
      WRITE(*,*) 'bdy_ice_frs_lim_2: You should not have seen this print! error?', kt
   END SUBROUTINE bdy_ice_lim_2
#endif

   !!=================================================================================
END MODULE bdyice_lim2
