MODULE limtrp
   !!======================================================================
   !!                       ***  MODULE limtrp   ***
   !! LIM transport ice model : sea-ice advection/diffusion
   !!======================================================================
   !! History : LIM-2 ! 2000-01 (M.A. Morales Maqueda, H. Goosse, and T. Fichefet)  Original code
   !!            3.0  ! 2005-11 (M. Vancoppenolle)   Multi-layer sea ice, salinity variations
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_trp      : advection/diffusion process of sea ice
   !!----------------------------------------------------------------------
   USE phycst          ! physical constant
   USE dom_oce         ! ocean domain
   USE sbc_oce         ! ocean surface boundary condition
   USE par_ice         ! LIM-3 parameter
   USE dom_ice         ! LIM-3 domain
   USE ice             ! LIM-3 variables
   USE limadv          ! LIM-3 advection
   USE limhdf          ! LIM-3 horizontal diffusion
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! lateral boundary conditions -- MPP exchanges
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! work arrays
   USE prtctl          ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_trp    ! called by ice_step

   REAL(wp)  ::   epsi06 = 1.e-06_wp   ! constant values
   REAL(wp)  ::   epsi03 = 1.e-03_wp  
   REAL(wp)  ::   zeps10 = 1.e-10_wp  
   REAL(wp)  ::   epsi16 = 1.e-16_wp
   REAL(wp)  ::   rzero  = 0._wp   
   REAL(wp)  ::   rone   = 1._wp

   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:,:,:,:) ::   zs0e

   !! * Substitution
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limtrp.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_trp( kt ) 
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE lim_trp ***
      !!                    
      !! ** purpose : advection/diffusion process of sea ice
      !!
      !! ** method  : variables included in the process are scalar,   
      !!     other values are considered as second order. 
      !!     For advection, a second order Prather scheme is used.  
      !!
      !! ** action :
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! number of iteration
      !
      INTEGER  ::   ji, jj, jk, jl, layer   ! dummy loop indices
      INTEGER  ::   initad                  ! number of sub-timestep for the advection
      INTEGER  ::   ierr                    ! error status
      REAL(wp) ::   zindb  , zindsn , zindic      ! local scalar
      REAL(wp) ::   zusvosn, zusvoic, zbigval     !   -      -
      REAL(wp) ::   zcfl , zusnit , zrtt          !   -      -
      REAL(wp) ::   ze   , zsal   , zage          !   -      -
      !
      REAL(wp), POINTER, DIMENSION(:,:)      ::   zui_u, zvi_v, zsm, zs0at, zs0ow
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   zs0ice, zs0sn, zs0a, zs0c0 , zs0sm , zs0oi
      REAL(wp), POINTER, DIMENSION(:,:,:,:)  ::   zs0e
      !!---------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, zui_u, zvi_v, zsm, zs0at, zs0ow )
      CALL wrk_alloc( jpi, jpj, jpl, zs0ice, zs0sn, zs0a, zs0c0 , zs0sm , zs0oi )
      CALL wrk_alloc( jpi, jpj, jkmax, jpl, zs0e )

      IF( numit == nstart .AND. lwp ) THEN
         WRITE(numout,*)
         IF( ln_limdyn ) THEN   ;   WRITE(numout,*) 'lim_trp : Ice transport '
         ELSE                   ;   WRITE(numout,*) 'lim_trp : No ice advection as ln_limdyn = ', ln_limdyn
         ENDIF
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      
      zsm(:,:) = area(:,:)

      !                             !-------------------------------------!
      IF( ln_limdyn ) THEN          !   Advection of sea ice properties   !
         !                          !-------------------------------------!
         !

         !-------------------------
         ! transported fields                                        
         !-------------------------
         ! Snow vol, ice vol, salt and age contents, area
         zs0ow(:,:) = ato_i(:,:) * area(:,:)               ! Open water area 
         DO jl = 1, jpl
            zs0sn (:,:,jl)   = v_s  (:,:,jl) * area(:,:)    ! Snow volume
            zs0ice(:,:,jl)   = v_i  (:,:,jl) * area(:,:)    ! Ice  volume
            zs0a  (:,:,jl)   = a_i  (:,:,jl) * area(:,:)    ! Ice area
            zs0sm (:,:,jl)   = smv_i(:,:,jl) * area(:,:)    ! Salt content
            zs0oi (:,:,jl)   = oa_i (:,:,jl) * area(:,:)    ! Age content
            zs0c0 (:,:,jl)   = e_s  (:,:,1,jl)              ! Snow heat content
            zs0e  (:,:,:,jl) = e_i  (:,:,:,jl)              ! Ice  heat content
         END DO

         !--------------------------
         ! Advection of Ice fields  (Prather scheme)                                            
         !--------------------------
         ! If ice drift field is too fast, use an appropriate time step for advection.         
         ! CFL test for stability
         zcfl  =            MAXVAL( ABS( u_ice(:,:) ) * rdt_ice / e1u(:,:) )
         zcfl  = MAX( zcfl, MAXVAL( ABS( v_ice(:,:) ) * rdt_ice / e2v(:,:) ) )
         IF(lk_mpp )   CALL mpp_max( zcfl )
!!gm more readability:
!         IF( zcfl > 0.5 ) THEN   ;   initad = 2   ;   zusnit = 0.5_wp
!         ELSE                    ;   initad = 1   ;   zusnit = 1.0_wp
!         ENDIF
!!gm end
         initad = 1 + INT( MAX( rzero, SIGN( rone, zcfl-0.5 ) ) )
         zusnit = 1.0 / REAL( initad ) 
         IF( zcfl > 0.5 .AND. lwp )   &
            WRITE(numout,*) 'lim_trp_2 : CFL violation at day ', nday, ', cfl = ', zcfl,   &
               &                        ': the ice time stepping is split in two'

         IF( MOD( ( kt - 1) / nn_fsbc , 2 ) == 0 ) THEN       !==  odd ice time step:  adv_x then adv_y  ==!
            DO jk = 1,initad
               CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0ow (:,:), sxopw(:,:),   &             !--- ice open water area
                  &                                       sxxopw(:,:), syopw(:,:), syyopw(:,:), sxyopw(:,:)  )
               CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0ow (:,:), sxopw(:,:),   &
                  &                                       sxxopw(:,:), syopw(:,:), syyopw(:,:), sxyopw(:,:)  )
               DO jl = 1, jpl
                  CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0ice(:,:,jl), sxice(:,:,jl),   &    !--- ice volume  ---
                     &                                       sxxice(:,:,jl), syice(:,:,jl), syyice(:,:,jl), sxyice(:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0ice(:,:,jl), sxice(:,:,jl),   &
                     &                                       sxxice(:,:,jl), syice(:,:,jl), syyice(:,:,jl), sxyice(:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0sn (:,:,jl), sxsn (:,:,jl),   &    !--- snow volume  ---
                     &                                       sxxsn (:,:,jl), sysn (:,:,jl), syysn (:,:,jl), sxysn (:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0sn (:,:,jl), sxsn (:,:,jl),   &
                     &                                       sxxsn (:,:,jl), sysn (:,:,jl), syysn (:,:,jl), sxysn (:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0sm (:,:,jl), sxsal(:,:,jl),   &    !--- ice salinity ---
                     &                                       sxxsal(:,:,jl), sysal(:,:,jl), syysal(:,:,jl), sxysal(:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0sm (:,:,jl), sxsal(:,:,jl),   &
                     &                                       sxxsal(:,:,jl), sysal(:,:,jl), syysal(:,:,jl), sxysal(:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0oi (:,:,jl), sxage(:,:,jl),   &   !--- ice age      ---     
                     &                                       sxxage(:,:,jl), syage(:,:,jl), syyage(:,:,jl), sxyage(:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0oi (:,:,jl), sxage(:,:,jl),   &
                     &                                       sxxage(:,:,jl), syage(:,:,jl), syyage(:,:,jl), sxyage(:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0a  (:,:,jl), sxa  (:,:,jl),   &   !--- ice concentrations ---
                     &                                       sxxa  (:,:,jl), sya  (:,:,jl), syya  (:,:,jl), sxya  (:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0a  (:,:,jl), sxa  (:,:,jl),   & 
                     &                                       sxxa  (:,:,jl), sya  (:,:,jl), syya  (:,:,jl), sxya  (:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0c0 (:,:,jl), sxc0 (:,:,jl),   &  !--- snow heat contents ---
                     &                                       sxxc0 (:,:,jl), syc0 (:,:,jl), syyc0 (:,:,jl), sxyc0 (:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0c0 (:,:,jl), sxc0 (:,:,jl),   &
                     &                                       sxxc0 (:,:,jl), syc0 (:,:,jl), syyc0 (:,:,jl), sxyc0 (:,:,jl)  )
                  DO layer = 1, nlay_i                                                           !--- ice heat contents ---
                     CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0e(:,:,layer,jl), sxe (:,:,layer,jl),   & 
                        &                                       sxxe(:,:,layer,jl), sye (:,:,layer,jl),   &
                        &                                       syye(:,:,layer,jl), sxye(:,:,layer,jl) )
                     CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0e(:,:,layer,jl), sxe (:,:,layer,jl),   & 
                        &                                       sxxe(:,:,layer,jl), sye (:,:,layer,jl),   &
                        &                                       syye(:,:,layer,jl), sxye(:,:,layer,jl) )
                  END DO
               END DO
            END DO
         ELSE
            DO jk = 1, initad
               CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0ow (:,:), sxopw(:,:),   &             !--- ice open water area
                  &                                       sxxopw(:,:), syopw(:,:), syyopw(:,:), sxyopw(:,:)  )
               CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0ow (:,:), sxopw(:,:),   &
                  &                                       sxxopw(:,:), syopw(:,:), syyopw(:,:), sxyopw(:,:)  )
               DO jl = 1, jpl
                  CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0ice(:,:,jl), sxice(:,:,jl),   &    !--- ice volume  ---
                     &                                       sxxice(:,:,jl), syice(:,:,jl), syyice(:,:,jl), sxyice(:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0ice(:,:,jl), sxice(:,:,jl),   &
                     &                                       sxxice(:,:,jl), syice(:,:,jl), syyice(:,:,jl), sxyice(:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0sn (:,:,jl), sxsn (:,:,jl),   &    !--- snow volume  ---
                     &                                       sxxsn (:,:,jl), sysn (:,:,jl), syysn (:,:,jl), sxysn (:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0sn (:,:,jl), sxsn (:,:,jl),   &
                     &                                       sxxsn (:,:,jl), sysn (:,:,jl), syysn (:,:,jl), sxysn (:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0sm (:,:,jl), sxsal(:,:,jl),   &    !--- ice salinity ---
                     &                                       sxxsal(:,:,jl), sysal(:,:,jl), syysal(:,:,jl), sxysal(:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0sm (:,:,jl), sxsal(:,:,jl),   &
                     &                                       sxxsal(:,:,jl), sysal(:,:,jl), syysal(:,:,jl), sxysal(:,:,jl)  )

                  CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0oi (:,:,jl), sxage(:,:,jl),   &   !--- ice age      ---
                     &                                       sxxage(:,:,jl), syage(:,:,jl), syyage(:,:,jl), sxyage(:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0oi (:,:,jl), sxage(:,:,jl),   &
                     &                                       sxxage(:,:,jl), syage(:,:,jl), syyage(:,:,jl), sxyage(:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0a  (:,:,jl), sxa  (:,:,jl),   &   !--- ice concentrations ---
                     &                                       sxxa  (:,:,jl), sya  (:,:,jl), syya  (:,:,jl), sxya  (:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0a  (:,:,jl), sxa  (:,:,jl),   &
                     &                                       sxxa  (:,:,jl), sya  (:,:,jl), syya  (:,:,jl), sxya  (:,:,jl)  )
                  CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0c0 (:,:,jl), sxc0 (:,:,jl),   &  !--- snow heat contents ---
                     &                                       sxxc0 (:,:,jl), syc0 (:,:,jl), syyc0 (:,:,jl), sxyc0 (:,:,jl)  )
                  CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0c0 (:,:,jl), sxc0 (:,:,jl),   &
                     &                                       sxxc0 (:,:,jl), syc0 (:,:,jl), syyc0 (:,:,jl), sxyc0 (:,:,jl)  )
                  DO layer = 1, nlay_i                                                           !--- ice heat contents ---
                     CALL lim_adv_y( zusnit, v_ice, rzero, zsm, zs0e(:,:,layer,jl), sxe (:,:,layer,jl),   & 
                        &                                       sxxe(:,:,layer,jl), sye (:,:,layer,jl),   &
                        &                                       syye(:,:,layer,jl), sxye(:,:,layer,jl) )
                     CALL lim_adv_x( zusnit, u_ice, rone , zsm, zs0e(:,:,layer,jl), sxe (:,:,layer,jl),   & 
                        &                                       sxxe(:,:,layer,jl), sye (:,:,layer,jl),   &
                        &                                       syye(:,:,layer,jl), sxye(:,:,layer,jl) )
                  END DO
               END DO
            END DO
         ENDIF

         !-------------------------------------------
         ! Recover the properties from their contents
         !-------------------------------------------
         zs0ow(:,:) = zs0ow(:,:) / area(:,:)
         DO jl = 1, jpl
            zs0ice(:,:,jl) = zs0ice(:,:,jl) / area(:,:)
            zs0sn (:,:,jl) = zs0sn (:,:,jl) / area(:,:)
            zs0sm (:,:,jl) = zs0sm (:,:,jl) / area(:,:)
            zs0oi (:,:,jl) = zs0oi (:,:,jl) / area(:,:)
            zs0a  (:,:,jl) = zs0a  (:,:,jl) / area(:,:)
            zs0c0 (:,:,jl) = zs0c0 (:,:,jl) / area(:,:)
            DO jk = 1, nlay_i
               zs0e(:,:,jk,jl) = zs0e(:,:,jk,jl) / area(:,:)
            END DO
         END DO

         !------------------------------------------------------------------------------!
         ! 4) Diffusion of Ice fields                  
         !------------------------------------------------------------------------------!

         !--------------------------------
         !  diffusion of open water area
         !--------------------------------
         zs0at(:,:) = zs0a(:,:,1)      ! total ice fraction
         DO jl = 2, jpl
            zs0at(:,:) = zs0at(:,:) + zs0a(:,:,jl)
         END DO
         !
         !                             ! Masked eddy diffusivity coefficient at ocean U- and V-points
         DO jj = 1, jpjm1                    ! NB: has not to be defined on jpj line and jpi row
            DO ji = 1 , fs_jpim1   ! vector opt.
               pahu(ji,jj) = ( 1._wp - MAX( rzero, SIGN( rone, -zs0at(ji  ,jj) ) ) )   &
                  &        * ( 1._wp - MAX( rzero, SIGN( rone, -zs0at(ji+1,jj) ) ) ) * ahiu(ji,jj)
               pahv(ji,jj) = ( 1._wp - MAX( rzero, SIGN( rone, -zs0at(ji,jj  ) ) ) )   &
                  &        * ( 1._wp - MAX( rzero, SIGN( rone,- zs0at(ji,jj+1) ) ) ) * ahiv(ji,jj)
            END DO
         END DO
         !
         CALL lim_hdf( zs0ow (:,:) )   ! Diffusion

         !------------------------------------
         !  Diffusion of other ice variables
         !------------------------------------
         DO jl = 1, jpl
         !                             ! Masked eddy diffusivity coefficient at ocean U- and V-points
            DO jj = 1, jpjm1                 ! NB: has not to be defined on jpj line and jpi row
               DO ji = 1 , fs_jpim1   ! vector opt.
                  pahu(ji,jj) = ( 1._wp - MAX( rzero, SIGN( rone, -zs0a(ji  ,jj,jl) ) ) )   &
                     &        * ( 1._wp - MAX( rzero, SIGN( rone, -zs0a(ji+1,jj,jl) ) ) ) * ahiu(ji,jj)
                  pahv(ji,jj) = ( 1._wp - MAX( rzero, SIGN( rone, -zs0a(ji,jj  ,jl) ) ) )   &
                     &        * ( 1._wp - MAX( rzero, SIGN( rone,- zs0a(ji,jj+1,jl) ) ) ) * ahiv(ji,jj)
               END DO
            END DO

            CALL lim_hdf( zs0ice (:,:,jl) )
            CALL lim_hdf( zs0sn  (:,:,jl) )
            CALL lim_hdf( zs0sm  (:,:,jl) )
            CALL lim_hdf( zs0oi  (:,:,jl) )
            CALL lim_hdf( zs0a   (:,:,jl) )
            CALL lim_hdf( zs0c0  (:,:,jl) )
            DO jk = 1, nlay_i
               CALL lim_hdf( zs0e (:,:,jk,jl) )
            END DO
         END DO

         !-----------------------------------------
         !  Remultiply everything by ice area
         !-----------------------------------------
         zs0ow(:,:) = MAX( rzero, zs0ow(:,:) * area(:,:) )
         DO jl = 1, jpl
            zs0ice(:,:,jl) = MAX( rzero, zs0ice(:,:,jl) * area(:,:) )    !!bug:  est-ce utile
            zs0sn (:,:,jl) = MAX( rzero, zs0sn (:,:,jl) * area(:,:) )    !!bug:  cf /area  juste apres
            zs0sm (:,:,jl) = MAX( rzero, zs0sm (:,:,jl) * area(:,:) )    !!bug:  cf /area  juste apres
            zs0oi (:,:,jl) = MAX( rzero, zs0oi (:,:,jl) * area(:,:) )
            zs0a  (:,:,jl) = MAX( rzero, zs0a  (:,:,jl) * area(:,:) )    !! suppress both change le resultat
            zs0c0 (:,:,jl) = MAX( rzero, zs0c0 (:,:,jl) * area(:,:) )
            DO jk = 1, nlay_i
               zs0e(:,:,jk,jl) = MAX( rzero, zs0e (:,:,jk,jl) * area(:,:) )
            END DO ! jk
         END DO ! jl

         !------------------------------------------------------------------------------!
         ! 5) Update and limit ice properties after transport                           
         !------------------------------------------------------------------------------!

         !--------------------------------------------------
         ! 5.1) Recover mean values over the grid squares.
         !--------------------------------------------------

         DO jl = 1, jpl
            DO jk = 1, nlay_i
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     zs0e(ji,jj,jk,jl) = MAX( rzero, zs0e(ji,jj,jk,jl) / area(ji,jj) )
                  END DO
               END DO
            END DO
         END DO

         DO jj = 1, jpj
            DO ji = 1, jpi
               zs0ow(ji,jj) = MAX( rzero, zs0ow (ji,jj) / area(ji,jj) )
            END DO
         END DO

         zs0at(:,:) = 0._wp
         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zs0sn (ji,jj,jl) = MAX( rzero, zs0sn (ji,jj,jl)/area(ji,jj) )
                  zs0ice(ji,jj,jl) = MAX( rzero, zs0ice(ji,jj,jl)/area(ji,jj) )
                  zs0sm (ji,jj,jl) = MAX( rzero, zs0sm (ji,jj,jl)/area(ji,jj) )
                  zs0oi (ji,jj,jl) = MAX( rzero, zs0oi (ji,jj,jl)/area(ji,jj) )
                  zs0a  (ji,jj,jl) = MAX( rzero, zs0a  (ji,jj,jl)/area(ji,jj) )
                  zs0c0 (ji,jj,jl) = MAX( rzero, zs0c0 (ji,jj,jl)/area(ji,jj) )
                  zs0at (ji,jj)    = zs0at(ji,jj) + zs0a(ji,jj,jl)
               END DO
            END DO
         END DO

         !---------------------------------------------------------
         ! 5.2) Snow thickness, Ice thickness, Ice concentrations
         !---------------------------------------------------------
         DO jj = 1, jpj
            DO ji = 1, jpi
               zindb        = MAX( 0._wp , SIGN( 1.0, zs0at(ji,jj) - zeps10) )
               zs0ow(ji,jj) = ( 1._wp - zindb ) + zindb * MAX( zs0ow(ji,jj), 0._wp )
               ato_i(ji,jj) = zs0ow(ji,jj)
            END DO
         END DO

         DO jl = 1, jpl         ! Remove very small areas 
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zindb         = MAX( 0.0 , SIGN( 1.0, zs0a(ji,jj,jl) - zeps10) )
                  !
                  zs0a(ji,jj,jl) = zindb * MIN( zs0a(ji,jj,jl), 0.99 )
                  v_s(ji,jj,jl)  = zindb * zs0sn (ji,jj,jl) 
                  v_i(ji,jj,jl)  = zindb * zs0ice(ji,jj,jl)
                  !
                  zindsn         = MAX( rzero, SIGN( rone, v_s(ji,jj,jl) - zeps10 ) )
                  zindic         = MAX( rzero, SIGN( rone, v_i(ji,jj,jl) - zeps10 ) )
                  zindb          = MAX( zindsn, zindic )
                  zs0a(ji,jj,jl) = zindb  * zs0a(ji,jj,jl) !ice concentration
                  a_i (ji,jj,jl) = zs0a(ji,jj,jl)
                  v_s (ji,jj,jl) = zindsn * v_s(ji,jj,jl)
                  v_i (ji,jj,jl) = zindic * v_i(ji,jj,jl)
               END DO
            END DO
         END DO

         DO jj = 1, jpj
            DO ji = 1, jpi
               zs0at(ji,jj) = SUM( zs0a(ji,jj,1:jpl) )
            END DO
         END DO

         !----------------------
         ! 5.3) Ice properties
         !----------------------

         zbigval = 1.d+13

         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, jpi

                  ! Switches and dummy variables
                  zusvosn         = 1.0/MAX( v_s(ji,jj,jl) , epsi16 )
                  zusvoic         = 1.0/MAX( v_i(ji,jj,jl) , epsi16 )
                  zrtt            = 173.15 * rone 
                  zindsn          = MAX( rzero, SIGN( rone, v_s(ji,jj,jl) - zeps10 ) )
                  zindic          = MAX( rzero, SIGN( rone, v_i(ji,jj,jl) - zeps10 ) )
                  zindb           = MAX( zindsn, zindic )

                  ! Ice salinity and age
                  zsal = MAX( MIN( (rhoic-rhosn)/rhoic*sss_m(ji,jj)  , &
                     zusvoic * zs0sm(ji,jj,jl) ), s_i_min ) * v_i(ji,jj,jl)
                  IF ( ( num_sal .EQ. 2 ) .OR. ( num_sal .EQ. 4 ) ) & 
                     smv_i(ji,jj,jl) = zindic*zsal + (1.0-zindic)*0.0

                  zage = MAX( MIN( zbigval, zs0oi(ji,jj,jl) / & 
                     MAX( a_i(ji,jj,jl), epsi16 )  ), 0.0 ) * a_i(ji,jj,jl)
                  oa_i (ji,jj,jl)  = zindic*zage 

                  ! Snow heat content
                  ze              =  MIN( MAX( 0.0, zs0c0(ji,jj,jl)*area(ji,jj) ), zbigval )
                  e_s(ji,jj,1,jl) = zindsn * ze + (1.0 - zindsn) * 0.0      

               END DO !ji
            END DO !jj
         END DO ! jl

         DO jl = 1, jpl
            DO jk = 1, nlay_i
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     ! Ice heat content
                     zindic          =  MAX( rzero, SIGN( rone, v_i(ji,jj,jl) - zeps10 ) )
                     ze              =  MIN( MAX( 0.0, zs0e(ji,jj,jk,jl)*area(ji,jj) ), zbigval )
                     e_i(ji,jj,jk,jl) = zindic * ze    + ( 1.0 - zindic ) * 0.0
                  END DO !ji
               END DO ! jj
            END DO ! jk
         END DO ! jl

      ENDIF

      IF(ln_ctl) THEN   ! Control print
         CALL prt_ctl_info(' ')
         CALL prt_ctl_info(' - Cell values : ')
         CALL prt_ctl_info('   ~~~~~~~~~~~~~ ')
         CALL prt_ctl(tab2d_1=area , clinfo1=' lim_trp  : cell area :')
         CALL prt_ctl(tab2d_1=at_i , clinfo1=' lim_trp  : at_i      :')
         CALL prt_ctl(tab2d_1=vt_i , clinfo1=' lim_trp  : vt_i      :')
         CALL prt_ctl(tab2d_1=vt_s , clinfo1=' lim_trp  : vt_s      :')
         DO jl = 1, jpl
            CALL prt_ctl_info(' ')
            CALL prt_ctl_info(' - Category : ', ivar1=jl)
            CALL prt_ctl_info('   ~~~~~~~~~~')
            CALL prt_ctl(tab2d_1=a_i   (:,:,jl)   , clinfo1= ' lim_trp  : a_i      : ')
            CALL prt_ctl(tab2d_1=ht_i  (:,:,jl)   , clinfo1= ' lim_trp  : ht_i     : ')
            CALL prt_ctl(tab2d_1=ht_s  (:,:,jl)   , clinfo1= ' lim_trp  : ht_s     : ')
            CALL prt_ctl(tab2d_1=v_i   (:,:,jl)   , clinfo1= ' lim_trp  : v_i      : ')
            CALL prt_ctl(tab2d_1=v_s   (:,:,jl)   , clinfo1= ' lim_trp  : v_s      : ')
            CALL prt_ctl(tab2d_1=e_s   (:,:,1,jl) , clinfo1= ' lim_trp  : e_s      : ')
            CALL prt_ctl(tab2d_1=t_su  (:,:,jl)   , clinfo1= ' lim_trp  : t_su     : ')
            CALL prt_ctl(tab2d_1=t_s   (:,:,1,jl) , clinfo1= ' lim_trp  : t_snow   : ')
            CALL prt_ctl(tab2d_1=sm_i  (:,:,jl)   , clinfo1= ' lim_trp  : sm_i     : ')
            CALL prt_ctl(tab2d_1=smv_i (:,:,jl)   , clinfo1= ' lim_trp  : smv_i    : ')
            DO jk = 1, nlay_i
               CALL prt_ctl_info(' ')
               CALL prt_ctl_info(' - Layer : ', ivar1=jk)
               CALL prt_ctl_info('   ~~~~~~~')
               CALL prt_ctl(tab2d_1=t_i(:,:,jk,jl) , clinfo1= ' lim_trp  : t_i      : ')
               CALL prt_ctl(tab2d_1=e_i(:,:,jk,jl) , clinfo1= ' lim_trp  : e_i      : ')
            END DO
         END DO
      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, zui_u, zvi_v, zsm, zs0at, zs0ow )
      CALL wrk_dealloc( jpi, jpj, jpl, zs0ice, zs0sn, zs0a, zs0c0 , zs0sm , zs0oi )
      CALL wrk_dealloc( jpi, jpj, jkmax, jpl, zs0e )
      !
   END SUBROUTINE lim_trp

#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty Module                No sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_trp        ! Empty routine
   END SUBROUTINE lim_trp
#endif

   !!======================================================================
END MODULE limtrp
