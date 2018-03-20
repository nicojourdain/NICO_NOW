MODULE domngb
   !!======================================================================
   !!                       ***  MODULE  domngb  ***
   !! Grid search:  find the closest grid point from a given on/lat position
   !!======================================================================
   !! History : 3.2  !  2009-11  (S. Masson)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_ngb       : find the closest grid point from a given lon/lat position
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! for mppsum
   USE wrk_nemo       ! Memory allocation
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_ngb   ! routine called in iom.F90 module

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: domngb.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_ngb( plon, plat, kii, kjj, cdgrid )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dom_ngb  ***
      !!
      !! ** Purpose :   find the closest grid point from a given lon/lat position
      !!
      !! ** Method  :   look for minimum distance in cylindrical projection 
      !!                -> not good if located at too high latitude...
      !!----------------------------------------------------------------------
      !
      REAL(wp)        , INTENT(in   ) ::   plon, plat   ! longitude,latitude of the point
      INTEGER         , INTENT(  out) ::   kii, kjj     ! i-,j-index of the closes grid point
      CHARACTER(len=1), INTENT(in   ) ::   cdgrid       ! grid name 'T', 'U', 'V', 'W'
      !
      INTEGER , DIMENSION(2) ::   iloc
      REAL(wp)               ::   zlon, zmini
      REAL(wp), POINTER, DIMENSION(:,:) ::  zglam, zgphi, zmask, zdist
      !!--------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dom_ngb')
      !
      CALL wrk_alloc( jpi, jpj, zglam, zgphi, zmask, zdist )
      !
      zmask(:,:) = 0._wp
      SELECT CASE( cdgrid )
      CASE( 'U' )  ; zglam(:,:) = glamu(:,:) ; zgphi(:,:) = gphiu(:,:) ; zmask(nldi:nlei,nldj:nlej) = umask(nldi:nlei,nldj:nlej,1)
      CASE( 'V' )  ; zglam(:,:) = glamv(:,:) ; zgphi(:,:) = gphiv(:,:) ; zmask(nldi:nlei,nldj:nlej) = vmask(nldi:nlei,nldj:nlej,1)
      CASE( 'F' )  ; zglam(:,:) = glamf(:,:) ; zgphi(:,:) = gphif(:,:) ; zmask(nldi:nlei,nldj:nlej) = fmask(nldi:nlei,nldj:nlej,1)
      CASE DEFAULT ; zglam(:,:) = glamt(:,:) ; zgphi(:,:) = gphit(:,:) ; zmask(nldi:nlei,nldj:nlej) = tmask(nldi:nlei,nldj:nlej,1)
      END SELECT

      zlon       = MOD( plon       + 720., 360. )                                     ! plon between    0 and 360
      zglam(:,:) = MOD( zglam(:,:) + 720., 360. )                                     ! glam between    0 and 360
      IF( zlon > 270. )   zlon = zlon - 360.                                          ! zlon between  -90 and 270
      IF( zlon <  90. )   WHERE( zglam(:,:) > 180. ) zglam(:,:) = zglam(:,:) - 360.   ! glam between -180 and 180

      zglam(:,:) = zglam(:,:) - zlon
      zgphi(:,:) = zgphi(:,:) - plat
      zdist(:,:) = zglam(:,:) * zglam(:,:) + zgphi(:,:) * zgphi(:,:)
      
      IF( lk_mpp ) THEN  
         CALL mpp_minloc( zdist(:,:), zmask, zmini, kii, kjj)
      ELSE
         iloc(:) = MINLOC( zdist(:,:), mask = zmask(:,:) == 1.e0 )
         kii = iloc(1) + nimpp - 1
         kjj = iloc(2) + njmpp - 1
      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, zglam, zgphi, zmask, zdist )
      !
      IF( nn_timing == 1 )  CALL timing_stop('dom_ngb')
      !
   END SUBROUTINE dom_ngb

   !!======================================================================
END MODULE domngb
