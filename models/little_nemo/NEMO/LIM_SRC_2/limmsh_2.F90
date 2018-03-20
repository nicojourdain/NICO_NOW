MODULE limmsh_2
   !!======================================================================
   !!                     ***  MODULE  limmsh_2  ***
   !! LIM 2.0 ice model :   definition of the ice mesh parameters
   !!======================================================================
   !! History :   -   ! 2001-04 (LIM) original code
   !!            1.0  ! 2002-08 (C. Ethe, G. Madec) F90, module
   !!            3.3  ! 2009-05 (G. Garric, C. Bricaud) addition of the lim2_evp case
   !!----------------------------------------------------------------------
#if defined key_lim2
   !!----------------------------------------------------------------------
   !!   'key_lim2'                                     LIM 2.0sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_msh_2   : definition of the ice mesh
   !!----------------------------------------------------------------------
   USE phycst
   USE dom_oce
   USE dom_ice_2
   USE lbclnk
   USE in_out_manager
   USE lib_mpp          ! MPP library
#if defined key_lim2_vp
   USE wrk_nemo         ! work arrays
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC lim_msh_2      ! routine called by ice_ini_2.F90

   !!----------------------------------------------------------------------
   !! NEMO/LIM2 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: limmsh_2.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_msh_2
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE lim_msh_2  ***
      !!              
      !! ** Purpose : Definition of the charact. of the numerical grid
      !!       
      !! ** Action  : - Initialisation of some variables
      !!              - Definition of some constants linked with the grid
      !!              - Definition of the metric coef. for the sea/ice
      !!              - Initialization of the ice masks (tmsk, umsk)
      !! 
      !! ** Refer.  : Deleersnijder et al. Ocean Modelling 100, 7-10 
      !!--------------------------------------------------------------------- 
      INTEGER :: ji, jj      ! dummy loop indices
      REAL(wp) ::   zusden   ! local scalars
#if defined key_lim2_vp
      REAL(wp) ::   zusden2           ! local scalars
      REAL(wp) ::   zh1p  , zh2p      !   -      -
      REAL(wp) ::   zd2d1p, zd1d2p    !   -      -
      REAL(wp), POINTER, DIMENSION(:,:) ::   zd2d1, zd1d2   ! 2D workspace
#endif
      !!---------------------------------------------------------------------

#if defined key_lim2_vp
      CALL wrk_alloc( jpi, jpj, zd2d1, zd1d2 )
#endif

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'lim_msh_2 : LIM 2.0 sea-ice model, mesh initialization'
         WRITE(numout,*) '~~~~~~~~~'
      ENDIF
      
      IF( jphgr_msh == 2 .OR. jphgr_msh == 3 .OR. jphgr_msh == 5 )   &
          &      CALL ctl_stop(' Coriolis parameter in LIM not set for f- or beta-plane' )

      !----------------------------------------------------------                          
      !    Initialization of local and some global (common) variables 
      !------------------------------------------------------------------ 
      
      njeq   = INT( jpj / 2 )   !i bug mpp potentiel
      njeqm1 = njeq - 1 

      fcor(:,:) = 2. * omega * SIN( gphit(:,:) * rad )   !  coriolis factor at T-point
 
!i    DO jj = 1, jpj
!i       zmsk(jj) = SUM( tmask(:,jj,:) )   ! = 0          if land  everywhere on a j-line
!!ii     write(numout,*) jj, zind(jj)
!i    END DO

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

      njeqm1 = njeq - 1


      !   For each grid, definition of geometric tables 
      !------------------------------------------------------------------
      
      !-------------------
      ! Conventions :    !
      !-------------------
      !  indices 1 \ 2 <-> localisation in the 2 direction x \ y
      !  3rd indice <-> localisation on the mesh :
      !  0 = Centre ;  1 = corner W x(i-1/2) ; 2 = corner S y(j-1/2) ;
      !  3 = corner SW x(i-1/2),y(j-1/2)
      !-------------------
!!ibug ???
      wght(:,:,:,:) = 0.e0
      tmu(:,:)      = 0.e0
#if defined key_lim2_vp 
      akappa(:,:,:,:)     = 0.e0
      alambd(:,:,:,:,:,:) = 0.e0
#else
      tmv(:,:) = 0.e0
      tmf(:,:) = 0.e0
#endif
!!i
      

#if defined key_lim2_vp      
      ! metric coefficients for sea ice dynamic
      !----------------------------------------
      !                                                       ! akappa
      DO jj = 2, jpj
         zd1d2(:,jj) = e1v(:,jj) - e1v(:,jj-1)
      END DO
      CALL lbc_lnk( zd1d2, 'T', -1. )

      DO ji = 2, jpi
         zd2d1(ji,:) = e2u(ji,:) - e2u(ji-1,:)
      END DO
      CALL lbc_lnk( zd2d1, 'T', -1. )

      akappa(:,:,1,1) =        1.0 / ( 2.0 * e1t(:,:) )
      akappa(:,:,1,2) = zd1d2(:,:) / ( 4.0 * e1t(:,:) * e2t(:,:) )
      akappa(:,:,2,1) = zd2d1(:,:) / ( 4.0 * e1t(:,:) * e2t(:,:) )
      akappa(:,:,2,2) =        1.0 / ( 2.0 * e2t(:,:) )
      
      !                                                      ! weights (wght)
      DO jj = 2, jpj
         DO ji = 2, jpi
            zusden = 1. / (  ( e1t(ji,jj) + e1t(ji-1,jj  ) )   &
               &           * ( e2t(ji,jj) + e2t(ji  ,jj-1) ) )
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
#else
      ! metric coefficients for sea ice dynamic (EVP rheology)
      !----------------------------------------
      DO jj = 1, jpjm1                                       ! weights (wght) at F-points
         DO ji = 1, jpim1
            zusden = 1. / (  ( e1t(ji+1,jj  ) + e1t(ji,jj) )   &
               &           * ( e2t(ji  ,jj+1) + e2t(ji,jj) ) ) 
            wght(ji,jj,1,1) = zusden * e1t(ji+1,jj) * e2t(ji,jj+1)
            wght(ji,jj,1,2) = zusden * e1t(ji+1,jj) * e2t(ji,jj  )
            wght(ji,jj,2,1) = zusden * e1t(ji  ,jj) * e2t(ji,jj+1)
            wght(ji,jj,2,2) = zusden * e1t(ji  ,jj) * e2t(ji,jj  )
         END DO
      END DO
      CALL lbc_lnk( wght(:,:,1,1), 'F', 1. )   ;   CALL lbc_lnk( wght(:,:,1,2),'F', 1. )       ! lateral boundary cond.   
      CALL lbc_lnk( wght(:,:,2,1), 'F', 1. )   ;   CALL lbc_lnk( wght(:,:,2,2),'F', 1. )
#endif
    
      ! Coefficients for divergence of the stress tensor
      !-------------------------------------------------

#if defined key_lim2_vp
      DO jj = 2, jpj
         DO ji = 2, jpi   ! NO vector opt.
            zh1p  =  e1t(ji  ,jj  ) * wght(ji,jj,2,2)   &
               &   + e1t(ji-1,jj  ) * wght(ji,jj,1,2)   &
               &   + e1t(ji  ,jj-1) * wght(ji,jj,2,1)   &
               &   + e1t(ji-1,jj-1) * wght(ji,jj,1,1)

            zh2p  =  e2t(ji  ,jj  ) * wght(ji,jj,2,2)   &
               &   + e2t(ji-1,jj  ) * wght(ji,jj,1,2)   &
               &   + e2t(ji  ,jj-1) * wght(ji,jj,2,1)   &
               &   + e2t(ji-1,jj-1) * wght(ji,jj,1,1)

! better written but change the last digit and thus solver in less than 100 timestep
!           zh1p  = e1t(ji-1,jj  ) * wght(ji,jj,1,2) + e1t(ji,jj  ) * wght(ji,jj,2,2)   &
!              &  + e1t(ji-1,jj-1) * wght(ji,jj,1,1) + e1t(ji,jj-1) * wght(ji,jj,2,1) 

!           zh2p  = e2t(ji-1,jj  ) * wght(ji,jj,1,2) + e2t(ji,jj  ) * wght(ji,jj,2,2)   &
!              &  + e2t(ji-1,jj-1) * wght(ji,jj,1,1) + e2t(ji,jj-1) * wght(ji,jj,2,1)

!!ibug =0   zusden = 1.0 / ( zh1p * zh2p * 4.e0 )
            zusden = 1.0 / MAX( zh1p * zh2p * 4.e0 , 1.e-20 )
            zusden2 = zusden * 2.0 

            zd1d2p = zusden * 0.5 * ( -e1t(ji-1,jj-1) + e1t(ji-1,jj  ) - e1t(ji,jj-1) + e1t(ji  ,jj)   )
            zd2d1p = zusden * 0.5 * (  e2t(ji  ,jj-1) - e2t(ji-1,jj-1) + e2t(ji,jj  ) - e2t(ji-1,jj)   )

            alambd(ji,jj,2,2,2,1) = zusden2 * e2t(ji  ,jj-1)
            alambd(ji,jj,2,2,2,2) = zusden2 * e2t(ji  ,jj  )
            alambd(ji,jj,2,2,1,1) = zusden2 * e2t(ji-1,jj-1)
            alambd(ji,jj,2,2,1,2) = zusden2 * e2t(ji-1,jj  )

            alambd(ji,jj,1,1,2,1) = zusden2 * e1t(ji  ,jj-1)
            alambd(ji,jj,1,1,2,2) = zusden2 * e1t(ji  ,jj  )
            alambd(ji,jj,1,1,1,1) = zusden2 * e1t(ji-1,jj-1)
            alambd(ji,jj,1,1,1,2) = zusden2 * e1t(ji-1,jj  )

            alambd(ji,jj,1,2,2,1) = zd1d2p
            alambd(ji,jj,1,2,2,2) = zd1d2p
            alambd(ji,jj,1,2,1,1) = zd1d2p
            alambd(ji,jj,1,2,1,2) = zd1d2p

            alambd(ji,jj,2,1,2,1) = zd2d1p
            alambd(ji,jj,2,1,2,2) = zd2d1p
            alambd(ji,jj,2,1,1,1) = zd2d1p
            alambd(ji,jj,2,1,1,2) = zd2d1p
         END DO
      END DO

      CALL lbc_lnk( alambd(:,:,2,2,2,1), 'I', 1. )      ! CAUTION: even with the lbc_lnk at ice U-V point
      CALL lbc_lnk( alambd(:,:,2,2,2,2), 'I', 1. )      ! the value of wght at jpj is wrong
      CALL lbc_lnk( alambd(:,:,2,2,1,1), 'I', 1. )      ! but it is never used
      CALL lbc_lnk( alambd(:,:,2,2,1,2), 'I', 1. )      ! 

      CALL lbc_lnk( alambd(:,:,1,1,2,1), 'I', 1. )      ! CAUTION: idem
      CALL lbc_lnk( alambd(:,:,1,1,2,2), 'I', 1. )      ! 
      CALL lbc_lnk( alambd(:,:,1,1,1,1), 'I', 1. )      !
      CALL lbc_lnk( alambd(:,:,1,1,1,2), 'I', 1. )      !

      CALL lbc_lnk( alambd(:,:,1,2,2,1), 'I', 1. )      ! CAUTION: idem
      CALL lbc_lnk( alambd(:,:,1,2,2,2), 'I', 1. )      !
      CALL lbc_lnk( alambd(:,:,1,2,1,1), 'I', 1. )      !
      CALL lbc_lnk( alambd(:,:,1,2,1,2), 'I', 1. )      !

      CALL lbc_lnk( alambd(:,:,2,1,2,1), 'I', 1. )      ! CAUTION: idem
      CALL lbc_lnk( alambd(:,:,2,1,2,2), 'I', 1. )      !
      CALL lbc_lnk( alambd(:,:,2,1,1,1), 'I', 1. )      !
      CALL lbc_lnk( alambd(:,:,2,1,1,2), 'I', 1. )      !
#endif
            

      ! Initialization of ice masks
      !----------------------------
      
      tms(:,:) = tmask(:,:,1)      ! ice T-point  : use surface tmask

#if defined key_lim2_vp
      ! VP rheology : ice velocity point is I-point
!i here we can use umask with a i and j shift of -1,-1
      tmu(:,1) = 0.e0
      tmu(1,:) = 0.e0
      DO jj = 2, jpj               ! ice U.V-point: computed from ice T-point mask
         DO ji = 2, jpim1   ! NO vector opt.
            tmu(ji,jj) =  tms(ji,jj) * tms(ji-1,jj) * tms(ji,jj-1) * tms(ji-1,jj-1)            
         END DO
      END DO
      CALL lbc_lnk( tmu(:,:), 'I', 1. )      !--lateral boundary conditions    
#else
      ! EVP rheology : ice velocity point are U- & V-points ; ice vorticity
      ! point is F-point
      tmu(:,:) = umask(:,:,1)
      tmv(:,:) = vmask(:,:,1)
      tmf(:,:) = 0.e0                        ! used of fmask except its special value along the coast (rn_shlat)
      WHERE( fmask(:,:,1) == 1.e0 )   tmf(:,:) = 1.e0
#endif
      !
      ! unmasked and masked area of T-grid cell
      area(:,:) = e1t(:,:) * e2t(:,:)
      !
#if defined key_lim2_vp
      CALL wrk_dealloc( jpi, jpj, zd2d1, zd1d2 )
#endif
      !
   END SUBROUTINE lim_msh_2

#else
   !!----------------------------------------------------------------------
   !!   Default option            Dummy Module         NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_msh_2           ! Dummy routine
   END SUBROUTINE lim_msh_2
#endif

   !!======================================================================
END MODULE limmsh_2
