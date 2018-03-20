MODULE limmsh
   !!======================================================================
   !!                     ***  MODULE  limmsh  ***
   !! LIM ice model :   definition of the ice mesh parameters
   !!======================================================================
   !! History :  3.2  !  2008-01 (NEMO team)  LIM-3: adaptation from LIM-2
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_msh   : definition of the ice mesh
   !!----------------------------------------------------------------------
   USE phycst         ! physical constants
   USE dom_oce        ! ocean domain
   USE dom_ice        ! sea-ice domain
   USE in_out_manager ! I/O manager
   USE lbclnk         ! lateral boundary condition - MPP exchanges
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_msh   ! routine called by ice_ini.F90

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limmsh.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_msh
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE lim_msh  ***
      !!              
      !! ** Purpose : Definition of the charact. of the numerical grid
      !!       
      !! ** Action  : - Initialisation of some variables
      !!              - Definition of some constants linked with the grid
      !!              - Definition of the metric coef. for the sea/ice
      !!              - Initialization of the ice masks (tmsk, umsk)
      !! 
      !! Reference  : Deleersnijder et al. Ocean Modelling 100, 7-10 
      !!--------------------------------------------------------------------- 
      INTEGER  ::   ji, jj   ! dummy loop indices
      REAL(wp) ::   zusden   ! local scalar
      !!---------------------------------------------------------------------

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'lim_msh : LIM-3 sea-ice model, mesh initialization'
         WRITE(numout,*) '~~~~~~~'
      ENDIF

      IF( jphgr_msh == 2 .OR. jphgr_msh == 3 .OR. jphgr_msh == 5 )   &
          &      CALL ctl_stop(' Coriolis parameter in LIM not set for f- or beta-plane')

      !                           !==  coriolis factor & Equator position ==!
      njeq   = INT( jpj / 2 ) 
      njeqm1 = njeq - 1 
      !
      fcor(:,:) = 2. * omega * SIN( gphit(:,:) * rad )   ! coriolis factor
      !
      IF( fcor(1,1) * fcor(1,nlcj) < 0.e0 ) THEN   ! local domain include both hemisphere
         l_jeq = .TRUE.
         njeq  = 1
         DO WHILE ( njeq <= jpj .AND. fcor(1,njeq) < 0.e0 )
            njeq = njeq + 1
         END DO
         IF(lwp ) WRITE(numout,*) '          the equator is inside the domain at about njeq = ', njeq
      ELSEIF( fcor(1,1) < 0.e0 ) THEN
         l_jeq = .FALSE.
         njeq = jpj
         IF(lwp ) WRITE(numout,*) '          the model domain is entirely in the southern hemisphere: njeq = ', njeq
      ELSE
         l_jeq = .FALSE.
         njeq = 2
         IF(lwp ) WRITE(numout,*) '          the model domain is entirely in the northern hemisphere: njeq = ', njeq
      ENDIF
      !
      njeqm1 = njeq - 1


      !                           !==  metric coefficients for sea ice dynamic  ==!
      wght(:,:,:,:) = 0.e0
!!gm  Optimisation :  wght to be defined at F-point, not I-point  and change in limrhg
      DO jj = 2, jpj
         DO ji = 2, jpi
            zusden = 1.e0 / (  ( e1t(ji,jj) + e1t(ji-1,jj  ) )   &
               &             * ( e2t(ji,jj) + e2t(ji  ,jj-1) ) )
            wght(ji,jj,1,1) = zusden * e1t(ji  ,jj) * e2t(ji,jj  )
            wght(ji,jj,1,2) = zusden * e1t(ji  ,jj) * e2t(ji,jj-1)
            wght(ji,jj,2,1) = zusden * e1t(ji-1,jj) * e2t(ji,jj  )
            wght(ji,jj,2,2) = zusden * e1t(ji-1,jj) * e2t(ji,jj-1)
         END DO
      END DO
      CALL lbc_lnk( wght(:,:,1,1), 'I', 1. )      ! CAUTION: even with the lbc_lnk at ice U-V-point
      CALL lbc_lnk( wght(:,:,1,2), 'I', 1. )      ! the value of wght at jpj is wrong
      CALL lbc_lnk( wght(:,:,2,1), 'I', 1. )      ! but it is never used
      CALL lbc_lnk( wght(:,:,2,2), 'I', 1. )
!!gm end

      !                           !==  ice masks  ==!
      tms(:,:) = tmask(:,:,1)             ! ice T-point  : use surface tmask
      tmu(:,:) = umask(:,:,1)             ! ice U-point  : use surface umask  (C-grid EVP)
      tmv(:,:) = vmask(:,:,1)             ! ice V-point  : use surface vmask  (C-grid EVP)
      DO jj = 1, jpjm1                    ! ice F-point  : recompute fmask (due to nn_shlat)
         DO ji = 1 , jpim1   ! NO vector opt.
            tmf(ji,jj) =  tms(ji,jj) * tms(ji+1,jj) * tms(ji,jj+1) * tms(ji+1,jj+1)
         END DO
      END DO
      CALL lbc_lnk( tmf(:,:), 'F', 1. )           ! lateral boundary conditions

      !                           !==  unmasked and masked area of T-grid cell
      area(:,:) = e1t(:,:) * e2t(:,:)
      !
   END SUBROUTINE lim_msh

#else
   !!----------------------------------------------------------------------
   !!   Default option            Dummy Module         NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_msh           ! Dummy routine
   END SUBROUTINE lim_msh
#endif

   !!======================================================================
END MODULE limmsh
