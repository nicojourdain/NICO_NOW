MODULE bdydyn
   !!======================================================================
   !!                       ***  MODULE  bdydyn  ***
   !! Unstructured Open Boundary Cond. :   Apply boundary conditions to velocities
   !!======================================================================
   !! History :  1.0  !  2005-02  (J. Chanut, A. Sellar)  Original code
   !!             -   !  2007-07  (D. Storkey) Move Flather implementation to separate routine.
   !!            3.0  !  2008-04  (NEMO team)  add in the reference version
   !!            3.2  !  2008-04  (R. Benshila) consider velocity instead of transport 
   !!            3.3  !  2010-09  (E.O'Dea) modifications for Shelf configurations 
   !!            3.3  !  2010-09  (D.Storkey) add ice boundary conditions
   !!            3.4  !  2011     (D. Storkey) rewrite in preparation for OBC-BDY merge
   !!----------------------------------------------------------------------
#if defined key_bdy 
   !!----------------------------------------------------------------------
   !!   'key_bdy' :                    Unstructured Open Boundary Condition
   !!----------------------------------------------------------------------
   !!   bdy_dyn        : split velocities into barotropic and baroclinic parts
   !!                    and call bdy_dyn2d and bdy_dyn3d to apply boundary
   !!                    conditions
   !!----------------------------------------------------------------------
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing
   USE oce             ! ocean dynamics and tracers 
   USE dom_oce         ! ocean space and time domain
   USE dynspg_oce      
   USE bdy_oce         ! ocean open boundary conditions
   USE bdydyn2d        ! open boundary conditions for barotropic solution
   USE bdydyn3d        ! open boundary conditions for baroclinic velocities
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE in_out_manager  !

   IMPLICIT NONE
   PRIVATE

   PUBLIC   bdy_dyn     ! routine called in dynspg_flt (if lk_dynspg_flt) or 
                        ! dyn_nxt (if lk_dynspg_ts or lk_dynspg_exp)

#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: bdydyn.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE bdy_dyn( kt, dyn3d_only )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE bdy_dyn  ***
      !!
      !! ** Purpose : - Wrapper routine for bdy_dyn2d and bdy_dyn3d.
      !!
      !!----------------------------------------------------------------------
      !!
      INTEGER, INTENT( in )           :: kt               ! Main time step counter
      LOGICAL, INTENT( in ), OPTIONAL :: dyn3d_only       ! T => only update baroclinic velocities
      !!
      INTEGER               :: jk,ii,ij,ib,igrd     ! Loop counter
      LOGICAL               :: ll_dyn2d, ll_dyn3d  
      !!

      IF( nn_timing == 1 ) CALL timing_start('bdy_dyn')

      ll_dyn2d = .true.
      ll_dyn3d = .true.

      IF( PRESENT(dyn3d_only) ) THEN
         IF( dyn3d_only ) ll_dyn2d = .false.
      ENDIF

      !-------------------------------------------------------
      ! Set pointers
      !-------------------------------------------------------

      pssh => sshn
      phur => hur
      phvr => hvr
      CALL wrk_alloc(jpi,jpj,pu2d,pv2d) 

      !-------------------------------------------------------
      ! Split velocities into barotropic and baroclinic parts
      !-------------------------------------------------------

      pu2d(:,:) = 0.e0
      pv2d(:,:) = 0.e0
      DO jk = 1, jpkm1   !! Vertically integrated momentum trends
          pu2d(:,:) = pu2d(:,:) + fse3u(:,:,jk) * umask(:,:,jk) * ua(:,:,jk)
          pv2d(:,:) = pv2d(:,:) + fse3v(:,:,jk) * vmask(:,:,jk) * va(:,:,jk)
      END DO
      pu2d(:,:) = pu2d(:,:) * phur(:,:)
      pv2d(:,:) = pv2d(:,:) * phvr(:,:)
      DO jk = 1 , jpkm1
         ua(:,:,jk) = ua(:,:,jk) - pu2d(:,:)
         va(:,:,jk) = va(:,:,jk) - pv2d(:,:)
      END DO

      !-------------------------------------------------------
      ! Apply boundary conditions to barotropic and baroclinic
      ! parts separately
      !-------------------------------------------------------

      IF( ll_dyn2d ) CALL bdy_dyn2d( kt )

      IF( ll_dyn3d ) CALL bdy_dyn3d( kt )

      !-------------------------------------------------------
      ! Recombine velocities
      !-------------------------------------------------------

      DO jk = 1 , jpkm1
         ua(:,:,jk) = ( ua(:,:,jk) + pu2d(:,:) ) * umask(:,:,jk)
         va(:,:,jk) = ( va(:,:,jk) + pv2d(:,:) ) * vmask(:,:,jk)
      END DO

      CALL wrk_dealloc(jpi,jpj,pu2d,pv2d) 

      IF( nn_timing == 1 ) CALL timing_stop('bdy_dyn')

   END SUBROUTINE bdy_dyn

#else
   !!----------------------------------------------------------------------
   !!   Dummy module                   NO Unstruct Open Boundary Conditions
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE bdy_dyn( kt )      ! Empty routine
      WRITE(*,*) 'bdy_dyn: You should not have seen this print! error?', kt
   END SUBROUTINE bdy_dyn
#endif

   !!======================================================================
END MODULE bdydyn
