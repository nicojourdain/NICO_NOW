MODULE limtrp_2
   !!======================================================================
   !!                       ***  MODULE limtrp_2   ***
   !! LIM 2.0 transport ice model : sea-ice advection/diffusion
   !!======================================================================
   !! History :  LIM  !  2000-01 (UCL)  Original code
   !!            2.0  !  2001-05 (G. Madec, R. Hordoir) opa norm
   !!             -   !  2004-01 (G. Madec, C. Ethe)  F90, mpp
   !!            3.3  !  2009-05  (G. Garric, C. Bricaud) addition of the lim2_evp case
   !!----------------------------------------------------------------------
#if defined key_lim2
   !!----------------------------------------------------------------------
   !!   'key_lim2' :                                  LIM 2.0 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_trp_2      : advection/diffusion process of sea ice
   !!   lim_trp_init_2 : initialization and namelist read
   !!----------------------------------------------------------------------
   USE phycst          ! physical constant
   USE sbc_oce         ! ocean surface boundary condition
   USE dom_oce         ! ocean domain
   USE in_out_manager  ! I/O manager
   USE dom_ice_2       ! LIM-2 domain
   USE ice_2           ! LIM-2 variables
   USE limistate_2     ! LIM-2 initial state
   USE limadv_2        ! LIM-2 advection
   USE limhdf_2        ! LIM-2 horizontal diffusion
   USE lbclnk          ! lateral boundary conditions -- MPP exchanges
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_trp_2   ! called by sbc_ice_lim_2

   REAL(wp), PUBLIC ::   bound  = 0.e0          !: boundary condit. (0.0 no-slip, 1.0 free-slip)

   REAL(wp)  ::   epsi06 = 1.e-06   ! constant values
   REAL(wp)  ::   epsi03 = 1.e-03  
   REAL(wp)  ::   epsi16 = 1.e-16  
   REAL(wp)  ::   rzero  = 0.e0   
   REAL(wp)  ::   rone   = 1.e0

   !! * Substitution
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/LIM2 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: limtrp_2.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE lim_trp_2( kt )
      !!-------------------------------------------------------------------
      !!                   ***  ROUTINE lim_trp_2 ***
      !!                    
      !! ** purpose : advection/diffusion process of sea ice
      !!
      !! ** method  : variables included in the process are scalar,   
      !!     other values are considered as second order. 
      !!     For advection, a second order Prather scheme is used.  
      !!
      !! ** action :
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt     ! number of iteration
      !!
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      INTEGER  ::   initad       ! number of sub-timestep for the advection
      REAL(wp) ::   zindb  , zindsn , zindic, zacrith   ! local scalars
      REAL(wp) ::   zusvosn, zusvoic, zignm , zindhe    !   -      -
      REAL(wp) ::   zvbord , zcfl   , zusnit            !   -      -
      REAL(wp) ::   zrtt   , ztsn   , ztic1 , ztic2     !   -      -
      REAL(wp), POINTER, DIMENSION(:,:)  ::   zui_u , zvi_v , zsm             ! 2D workspace
      REAL(wp), POINTER, DIMENSION(:,:)  ::   zs0ice, zs0sn , zs0a            !  -      -
      REAL(wp), POINTER, DIMENSION(:,:)  ::   zs0c0 , zs0c1 , zs0c2 , zs0st   !  -      -
      !---------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, zui_u , zvi_v , zsm, zs0ice, zs0sn , zs0a, zs0c0 , zs0c1 , zs0c2 , zs0st )

      IF( kt == nit000  )   CALL lim_trp_init_2      ! Initialization (first time-step only)

      zsm(:,:) = area(:,:)
      
      IF( ln_limdyn ) THEN
         !-------------------------------------!
         !   Advection of sea ice properties   !
         !-------------------------------------!

         ! ice velocities at ocean U- and V-points (zui_u,zvi_v)
         ! ---------------------------------------
         IF( lk_lim2_vp ) THEN      ! VP rheology : B-grid sea-ice dynamics (I-point ice velocity)
            zvbord = 1._wp + ( 1._wp - bound )      ! zvbord=2 no-slip, =0 free slip boundary conditions        
            DO jj = 1, jpjm1
               DO ji = 1, jpim1   ! NO vector opt.
                  zui_u(ji,jj) = ( u_ice(ji+1,jj) + u_ice(ji+1,jj+1) ) / ( MAX( tmu(ji+1,jj)+tmu(ji+1,jj+1), zvbord ) )
                  zvi_v(ji,jj) = ( v_ice(ji,jj+1) + v_ice(ji+1,jj+1) ) / ( MAX( tmu(ji,jj+1)+tmu(ji+1,jj+1), zvbord ) )
               END DO
            END DO
            CALL lbc_lnk( zui_u, 'U', -1. )   ;   CALL lbc_lnk( zvi_v, 'V', -1. )      ! Lateral boundary conditions
            !
         ELSE                       ! EVP rheology : C-grid sea-ice dynamics (u- & v-points ice velocity)
            zui_u(:,:) = u_ice(:,:)      ! EVP rheology: ice (u,v) at u- and v-points
            zvi_v(:,:) = v_ice(:,:)
         ENDIF

         ! CFL test for stability
         ! ----------------------
         zcfl  = 0._wp
         zcfl  = MAX( zcfl, MAXVAL( ABS( zui_u(1:jpim1, :     ) ) * rdt_ice / e1u(1:jpim1, :     ) ) )
         zcfl  = MAX( zcfl, MAXVAL( ABS( zvi_v( :     ,1:jpjm1) ) * rdt_ice / e2v( :     ,1:jpjm1) ) )
         !
         IF(lk_mpp)   CALL mpp_max( zcfl )
         !
         IF( zcfl > 0.5 .AND. lwp )   WRITE(numout,*) 'lim_trp_2 : violation of cfl criterion the ',nday,'th day, cfl = ', zcfl

         ! content of properties
         ! ---------------------
         zs0sn (:,:) =  hsnm(:,:)              * area  (:,:)  ! Snow volume.
         zs0ice(:,:) =  hicm(:,:)              * area  (:,:)  ! Ice volume.
         zs0a  (:,:) =  ( 1.0 - frld(:,:) )    * area  (:,:)  ! Surface covered by ice.
         zs0c0 (:,:) =  tbif(:,:,1) / rt0_snow * zs0sn (:,:)  ! Heat content of the snow layer.
         zs0c1 (:,:) =  tbif(:,:,2) / rt0_ice  * zs0ice(:,:)  ! Heat content of the first ice layer.
         zs0c2 (:,:) =  tbif(:,:,3) / rt0_ice  * zs0ice(:,:)  ! Heat content of the second ice layer.
         zs0st (:,:) =  qstoif(:,:) / xlic     * zs0a  (:,:)  ! Heat reservoir for brine pockets.
         
 
         ! Advection (Prather scheme)
         ! ---------
         initad = 1 + INT( MAX( rzero, SIGN( rone, zcfl-0.5 ) ) )   ! If ice drift field is too fast,          
         zusnit = 1.0 / REAL( initad )                              ! split the ice time step in two
         !
         IF( MOD( ( kt - 1) / nn_fsbc , 2 ) == 0) THEN        !==  odd ice time step:  adv_x then adv_y  ==!
            DO jk = 1, initad
               CALL lim_adv_x_2( zusnit, zui_u, rone , zsm, zs0ice, sxice, sxxice, syice, syyice, sxyice )
               CALL lim_adv_y_2( zusnit, zvi_v, rzero, zsm, zs0ice, sxice, sxxice, syice, syyice, sxyice )
               CALL lim_adv_x_2( zusnit, zui_u, rone , zsm, zs0sn , sxsn , sxxsn , sysn , syysn , sxysn  )
               CALL lim_adv_y_2( zusnit, zvi_v, rzero, zsm, zs0sn , sxsn , sxxsn , sysn , syysn , sxysn  )
               CALL lim_adv_x_2( zusnit, zui_u, rone , zsm, zs0a  , sxa  , sxxa  , sya  , syya  , sxya   )
               CALL lim_adv_y_2( zusnit, zvi_v, rzero, zsm, zs0a  , sxa  , sxxa  , sya  , syya  , sxya   )
               CALL lim_adv_x_2( zusnit, zui_u, rone , zsm, zs0c0 , sxc0 , sxxc0 , syc0 , syyc0 , sxyc0  )
               CALL lim_adv_y_2( zusnit, zvi_v, rzero, zsm, zs0c0 , sxc0 , sxxc0 , syc0 , syyc0 , sxyc0  )
               CALL lim_adv_x_2( zusnit, zui_u, rone , zsm, zs0c1 , sxc1 , sxxc1 , syc1 , syyc1 , sxyc1  )
               CALL lim_adv_y_2( zusnit, zvi_v, rzero, zsm, zs0c1 , sxc1 , sxxc1 , syc1 , syyc1 , sxyc1  )
               CALL lim_adv_x_2( zusnit, zui_u, rone , zsm, zs0c2 , sxc2 , sxxc2 , syc2 , syyc2 , sxyc2  )
               CALL lim_adv_y_2( zusnit, zvi_v, rzero, zsm, zs0c2 , sxc2 , sxxc2 , syc2 , syyc2 , sxyc2  )
               CALL lim_adv_x_2( zusnit, zui_u, rone , zsm, zs0st , sxst , sxxst , syst , syyst , sxyst  )
               CALL lim_adv_y_2( zusnit, zvi_v, rzero, zsm, zs0st , sxst , sxxst , syst , syyst , sxyst  )
            END DO
         ELSE                                                 !==  even ice time step:  adv_x then adv_y  ==!
            DO jk = 1, initad
               CALL lim_adv_y_2( zusnit, zvi_v, rone , zsm, zs0ice, sxice, sxxice, syice, syyice, sxyice )
               CALL lim_adv_x_2( zusnit, zui_u, rzero, zsm, zs0ice, sxice, sxxice, syice, syyice, sxyice )
               CALL lim_adv_y_2( zusnit, zvi_v, rone , zsm, zs0sn , sxsn , sxxsn , sysn , syysn , sxysn  )
               CALL lim_adv_x_2( zusnit, zui_u, rzero, zsm, zs0sn , sxsn , sxxsn , sysn , syysn , sxysn  )
               CALL lim_adv_y_2( zusnit, zvi_v, rone , zsm, zs0a  , sxa  , sxxa  , sya  , syya  , sxya   )
               CALL lim_adv_x_2( zusnit, zui_u, rzero, zsm, zs0a  , sxa  , sxxa  , sya  , syya  , sxya   )
               CALL lim_adv_y_2( zusnit, zvi_v, rone , zsm, zs0c0 , sxc0 , sxxc0 , syc0 , syyc0 , sxyc0  )
               CALL lim_adv_x_2( zusnit, zui_u, rzero, zsm, zs0c0 , sxc0 , sxxc0 , syc0 , syyc0 , sxyc0  )
               CALL lim_adv_y_2( zusnit, zvi_v, rone , zsm, zs0c1 , sxc1 , sxxc1 , syc1 , syyc1 , sxyc1  )
               CALL lim_adv_x_2( zusnit, zui_u, rzero, zsm, zs0c1 , sxc1 , sxxc1 , syc1 , syyc1 , sxyc1  )
               CALL lim_adv_y_2( zusnit, zvi_v, rone , zsm, zs0c2 , sxc2 , sxxc2 , syc2 , syyc2 , sxyc2  )
               CALL lim_adv_x_2( zusnit, zui_u, rzero, zsm, zs0c2 , sxc2 , sxxc2 , syc2 , syyc2 , sxyc2  )
               CALL lim_adv_y_2( zusnit, zvi_v, rone , zsm, zs0st , sxst , sxxst , syst , syyst , sxyst  )
               CALL lim_adv_x_2( zusnit, zui_u, rzero, zsm, zs0st , sxst , sxxst , syst , syyst , sxyst  )
            END DO
         ENDIF
                        
         ! recover the properties from their contents
         ! ------------------------------------------
!!gm Define in limmsh one for all area = 1 /area  (CPU time saved !)
         zs0ice(:,:) = zs0ice(:,:) / area(:,:)
         zs0sn (:,:) = zs0sn (:,:) / area(:,:)
         zs0a  (:,:) = zs0a  (:,:) / area(:,:)
         zs0c0 (:,:) = zs0c0 (:,:) / area(:,:)
         zs0c1 (:,:) = zs0c1 (:,:) / area(:,:)
         zs0c2 (:,:) = zs0c2 (:,:) / area(:,:)
         zs0st (:,:) = zs0st (:,:) / area(:,:)


         !-------------------------------------!
         !   Diffusion of sea ice properties   !
         !-------------------------------------!

         ! Masked eddy diffusivity coefficient at ocean U- and V-points
         ! ------------------------------------------------------------
         DO jj = 1, jpjm1          ! NB: has not to be defined on jpj line and jpi row
            DO ji = 1 , fs_jpim1   ! vector opt.
               pahu(ji,jj) = ( 1.0 - MAX( rzero, SIGN( rone, -zs0a(ji  ,jj) ) ) )   &
                  &        * ( 1.0 - MAX( rzero, SIGN( rone, -zs0a(ji+1,jj) ) ) ) * ahiu(ji,jj)
               pahv(ji,jj) = ( 1.0 - MAX( rzero, SIGN( rone, -zs0a(ji,jj  ) ) ) )   &
                  &        * ( 1.0 - MAX( rzero, SIGN( rone,- zs0a(ji,jj+1) ) ) ) * ahiv(ji,jj)
            END DO
         END DO
!!gm more readable coding: (and avoid an error in F90 with sign of zero)
!        DO jj = 1, jpjm1          ! NB: has not to be defined on jpj line and jpi row
!           DO ji = 1 , fs_jpim1   ! vector opt.
!              IF( MIN( zs0a(ji,jj) , zs0a(ji+1,jj) ) == 0.e0 )   pahu(ji,jj) = 0._wp
!              IF( MIN( zs0a(ji,jj) , zs0a(ji,jj+1) ) == 0.e0 )   pahv(ji,jj) = 0._wp
!           END DO
!        END DO
!!gm end

         ! diffusion
         ! ---------
         CALL lim_hdf_2( zs0ice )
         CALL lim_hdf_2( zs0sn  )
         CALL lim_hdf_2( zs0a   )
         CALL lim_hdf_2( zs0c0  )
         CALL lim_hdf_2( zs0c1  )
         CALL lim_hdf_2( zs0c2  )
         CALL lim_hdf_2( zs0st  )

!!gm see comment this can be skipped
         zs0ice(:,:) = MAX( rzero, zs0ice(:,:) * area(:,:) )    !!bug:  useless
         zs0sn (:,:) = MAX( rzero, zs0sn (:,:) * area(:,:) )    !!bug:  cf /area  just below
         zs0a  (:,:) = MAX( rzero, zs0a  (:,:) * area(:,:) )    !! caution: the suppression of the 2 changes 
         zs0c0 (:,:) = MAX( rzero, zs0c0 (:,:) * area(:,:) )    !! the last digit of the results
         zs0c1 (:,:) = MAX( rzero, zs0c1 (:,:) * area(:,:) )
         zs0c2 (:,:) = MAX( rzero, zs0c2 (:,:) * area(:,:) )
         zs0st (:,:) = MAX( rzero, zs0st (:,:) * area(:,:) )


         !-------------------------------------------------------------------!
         !   Updating and limitation of sea ice properties after transport   !
         !-------------------------------------------------------------------!
         DO jj = 1, jpj
            zindhe = MAX( 0.e0, SIGN( 1.e0, fcor(1,jj) ) )              ! = 0 for SH, =1 for NH
            DO ji = 1, jpi
               !
               ! Recover mean values over the grid squares.
               zs0sn (ji,jj) = MAX( rzero, zs0sn (ji,jj)/area(ji,jj) )
               zs0ice(ji,jj) = MAX( rzero, zs0ice(ji,jj)/area(ji,jj) )
               zs0a  (ji,jj) = MAX( rzero, zs0a  (ji,jj)/area(ji,jj) )
               zs0c0 (ji,jj) = MAX( rzero, zs0c0 (ji,jj)/area(ji,jj) )
               zs0c1 (ji,jj) = MAX( rzero, zs0c1 (ji,jj)/area(ji,jj) )
               zs0c2 (ji,jj) = MAX( rzero, zs0c2 (ji,jj)/area(ji,jj) )
               zs0st (ji,jj) = MAX( rzero, zs0st (ji,jj)/area(ji,jj) )

               ! Recover in situ values.
               zindb         = MAX( rzero, SIGN( rone, zs0a(ji,jj) - epsi06 ) )
               zacrith       = 1.0 - ( zindhe * acrit(1) + ( 1.0 - zindhe ) * acrit(2) )
               zs0a (ji,jj)  = zindb * MIN( zs0a(ji,jj), zacrith )
               hsnif(ji,jj)  = zindb * ( zs0sn(ji,jj) /MAX( zs0a(ji,jj), epsi16 ) )
               hicif(ji,jj)  = zindb * ( zs0ice(ji,jj)/MAX( zs0a(ji,jj), epsi16 ) )
               zindsn        = MAX( rzero, SIGN( rone, hsnif(ji,jj) - epsi06 ) )
               zindic        = MAX( rzero, SIGN( rone, hicif(ji,jj) - epsi03 ) )
               zindb         = MAX( zindsn, zindic )
               zs0a (ji,jj)  = zindb * zs0a(ji,jj)
               frld (ji,jj)  = 1.0 - zs0a(ji,jj)
               hsnif(ji,jj)  = zindsn * hsnif(ji,jj)
               hicif(ji,jj)  = zindic * hicif(ji,jj)
               zusvosn       = 1.0/MAX( hsnif(ji,jj) * zs0a(ji,jj), epsi16 )
               zusvoic       = 1.0/MAX( hicif(ji,jj) * zs0a(ji,jj), epsi16 )
               zignm         = MAX( rzero,  SIGN( rone, hsndif - hsnif(ji,jj) ) )
               zrtt          = 173.15 * rone 
               ztsn          =          zignm   * tbif(ji,jj,1)  &
                              + ( 1.0 - zignm ) * MIN( MAX( zrtt, rt0_snow * zusvosn * zs0c0(ji,jj)) , tfu(ji,jj) ) 
               ztic1          = MIN( MAX( zrtt, rt0_ice * zusvoic * zs0c1(ji,jj) ) , tfu(ji,jj) )
               ztic2          = MIN( MAX( zrtt, rt0_ice * zusvoic * zs0c2(ji,jj) ) , tfu(ji,jj) )
 
               tbif(ji,jj,1) = zindsn * ztsn  + ( 1.0 - zindsn ) * tfu(ji,jj)               
               tbif(ji,jj,2) = zindic * ztic1 + ( 1.0 - zindic ) * tfu(ji,jj)
               tbif(ji,jj,3) = zindic * ztic2 + ( 1.0 - zindic ) * tfu(ji,jj)
               qstoif(ji,jj) = zindb  * xlic * zs0st(ji,jj) /  MAX( zs0a(ji,jj), epsi16 )
            END DO
         END DO
         !
      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, zui_u , zvi_v , zsm, zs0ice, zs0sn , zs0a, zs0c0 , zs0c1 , zs0c2 , zs0st )
      !
   END SUBROUTINE lim_trp_2


   SUBROUTINE lim_trp_init_2
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE lim_trp_init_2  ***
      !!
      !! ** Purpose :   initialization of ice advection parameters
      !!
      !! ** Method  :   Read the namicetrp namelist and check the parameter 
      !!              values called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namicetrp
      !!-------------------------------------------------------------------
      NAMELIST/namicetrp/ bound
      !!-------------------------------------------------------------------
      !
      REWIND ( numnam_ice )      ! Read Namelist namicetrp
      READ   ( numnam_ice  , namicetrp )
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'lim_trp_init_2 : Ice parameters for advection '
         WRITE(numout,*) '~~~~~~~~~~~~~~'
         WRITE(numout,*) '   boundary conditions (0. no-slip, 1. free-slip) bound  = ', bound
      ENDIF
      !
   END SUBROUTINE lim_trp_init_2

#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty Module                No sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_trp_2        ! Empty routine
   END SUBROUTINE lim_trp_2
#endif

   !!======================================================================
END MODULE limtrp_2
