MODULE trdicp
   !!======================================================================
   !!                       ***  MODULE  trdicp  ***
   !! Ocean diagnostics:  ocean tracers and dynamic trends
   !!=====================================================================
   !! History :  1.0  !  2004-08 (C. Talandier) New trends organization
   !!----------------------------------------------------------------------
#if  defined key_trdtra   ||   defined key_trddyn   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_trdtra'  or                  active tracers trends diagnostics
   !!   'key_trddyn'                            momentum trends diagnostics
   !!----------------------------------------------------------------------
   !!   trd_icp          : compute the basin averaged properties for tra/dyn 
   !!   trd_dwr          : print dynmaic trends in ocean.output file
   !!   trd_twr          : print tracers trends in ocean.output file
   !!   trd_icp_init     : initialization step
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables
   USE trdmod_oce      ! ocean variables trends
   USE ldftra_oce      ! ocean active tracers: lateral physics
   USE ldfdyn_oce      ! ocean dynamics: lateral physics
   USE zdf_oce         ! ocean vertical physics
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distibuted memory computing library
   USE eosbn2          ! equation of state
   USE phycst          ! physical constants
   USE wrk_nemo        ! Memory allocation


   IMPLICIT NONE
   PRIVATE

   INTERFACE trd_icp
      MODULE PROCEDURE trd_2d, trd_3d
   END INTERFACE

   PUBLIC   trd_icp       ! called by trdmod.F90
   PUBLIC   trd_dwr       ! called by step.F90
   PUBLIC   trd_twr       ! called by step.F90
   PUBLIC   trd_icp_init  ! called by opa.F90

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: trdicp.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trd_2d( ptrd2dx, ptrd2dy, ktrd , ctype )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_2d  ***
      !! 
      !! ** Purpose : verify the basin averaged properties of tracers and/or
      !!              momentum equations at every time step frequency nn_trd.
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   ptrd2dx   ! Temperature or U trend 
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   ptrd2dy   ! Salinity    or V trend
      INTEGER                     , INTENT(in   ) ::   ktrd      ! tracer trend index
      CHARACTER(len=3)            , INTENT(in   ) ::   ctype     ! momentum ('DYN') or tracers ('TRA') trends
      !!
      INTEGER ::   ji, jj   ! loop indices
      !!----------------------------------------------------------------------

      SELECT CASE( ctype )    !==  Mask trends  ==!
      !
      CASE( 'DYN' )                    ! Momentum
         DO jj = 1, jpjm1
            DO ji = 1, jpim1
               ptrd2dx(ji,jj) = ptrd2dx(ji,jj) * tmask_i(ji+1,jj  ) * tmask_i(ji,jj) * umask(ji,jj,1)
               ptrd2dy(ji,jj) = ptrd2dy(ji,jj) * tmask_i(ji  ,jj+1) * tmask_i(ji,jj) * vmask(ji,jj,1)
            END DO
         END DO
         ptrd2dx(jpi, : ) = 0._wp      ;      ptrd2dy(jpi, : ) = 0._wp
         ptrd2dx( : ,jpj) = 0._wp      ;      ptrd2dy( : ,jpj) = 0._wp
         !
      CASE( 'TRA' )                    ! Tracers
         ptrd2dx(:,:) = ptrd2dx(:,:) * tmask_i(:,:)
         ptrd2dy(:,:) = ptrd2dy(:,:) * tmask_i(:,:)
         !
      END SELECT
      
      SELECT CASE( ctype )    !==  Basin averaged tracer/momentum trends  ==!
      !
      CASE( 'DYN' )                    ! Momentum
         umo(ktrd) = 0._wp
         vmo(ktrd) = 0._wp
         !
         SELECT CASE( ktrd )
         CASE( jpdyn_trd_swf )         ! surface forcing
            umo(ktrd) = SUM( ptrd2dx(:,:) * e1u(:,:) * e2u(:,:) * fse3u(:,:,1) )
            vmo(ktrd) = SUM( ptrd2dy(:,:) * e1v(:,:) * e2v(:,:) * fse3v(:,:,1) )
         END SELECT
         !
      CASE( 'TRA' )              ! Tracers
         tmo(ktrd) = SUM( ptrd2dx(:,:) * e1e2t(:,:) * fse3t(:,:,1) )
         smo(ktrd) = SUM( ptrd2dy(:,:) * e1e2t(:,:) * fse3t(:,:,1) )
      END SELECT
      
      SELECT CASE( ctype )    !==  Basin averaged tracer/momentum square trends  ==!   (now field)
      !
      CASE( 'DYN' )              ! Momentum
         hke(ktrd) = SUM(   un(:,:,1) * ptrd2dx(:,:) * e1u(:,:) * e2u(:,:) * fse3u(:,:,1)   &
            &             + vn(:,:,1) * ptrd2dy(:,:) * e1v(:,:) * e2v(:,:) * fse3v(:,:,1)   )
         !
      CASE( 'TRA' )              ! Tracers
         t2(ktrd) = SUM( ptrd2dx(:,:) * e1e2t(:,:) * fse3t(:,:,1) * tsn(:,:,1,jp_tem) )
         s2(ktrd) = SUM( ptrd2dy(:,:) * e1e2t(:,:) * fse3t(:,:,1) * tsn(:,:,1,jp_sal) )
         !      
      END SELECT
      !
   END SUBROUTINE trd_2d


   SUBROUTINE trd_3d( ptrd3dx, ptrd3dy, ktrd, ctype )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_3d  ***
      !! 
      !! ** Purpose : verify the basin averaged properties of tracers and/or
      !!              momentum equations at every time step frequency nn_trd.
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   ptrd3dx   ! Temperature or U trend 
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   ptrd3dy   ! Salinity    or V trend
      INTEGER,                          INTENT(in   ) ::   ktrd      ! momentum or tracer trend index
      CHARACTER(len=3),                 INTENT(in   ) ::   ctype     ! momentum ('DYN') or tracers ('TRA') trends
      !!
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------

      SELECT CASE( ctype )    !==  Mask the trends  ==!
      !
      CASE( 'DYN' )              ! Momentum        
         DO jk = 1, jpkm1
            DO jj = 1, jpjm1
               DO ji = 1, jpim1
                  ptrd3dx(ji,jj,jk) = ptrd3dx(ji,jj,jk) * tmask_i(ji+1,jj  ) * tmask_i(ji,jj) * umask(ji,jj,jk)
                  ptrd3dy(ji,jj,jk) = ptrd3dy(ji,jj,jk) * tmask_i(ji  ,jj+1) * tmask_i(ji,jj) * vmask(ji,jj,jk)
               END DO
            END DO
         END DO
         ptrd3dx(jpi, : ,:) = 0._wp      ;      ptrd3dy(jpi, : ,:) = 0._wp
         ptrd3dx( : ,jpj,:) = 0._wp      ;      ptrd3dy( : ,jpj,:) = 0._wp
         !
      CASE( 'TRA' )              ! Tracers
         DO jk = 1, jpkm1
            ptrd3dx(:,:,jk) = ptrd3dx(:,:,jk) * tmask(:,:,jk) * tmask_i(:,:)
            ptrd3dy(:,:,jk) = ptrd3dy(:,:,jk) * tmask(:,:,jk) * tmask_i(:,:)
         END DO
         !
      END SELECT   

      SELECT CASE( ctype )    !==  Basin averaged tracer/momentum trends  ==!
      !
      CASE( 'DYN' )              ! Momentum
         umo(ktrd) = 0._wp
         vmo(ktrd) = 0._wp
         DO jk = 1, jpkm1
            umo(ktrd) = umo(ktrd) + SUM( ptrd3dx(:,:,jk) * e1u(:,:) * e2u(:,:) * fse3u(:,:,jk) )
            vmo(ktrd) = vmo(ktrd) + SUM( ptrd3dy(:,:,jk) * e1v(:,:) * e2v(:,:) * fse3v(:,:,jk) )
         END DO
         !
      CASE( 'TRA' )              ! Tracers
         tmo(ktrd) = 0._wp
         smo(ktrd) = 0._wp
         DO jk = 1, jpkm1
            tmo(ktrd) = tmo(ktrd) + SUM( ptrd3dx(:,:,jk) * e1e2t(:,:) * fse3t(:,:,jk) )
            smo(ktrd) = smo(ktrd) + SUM( ptrd3dy(:,:,jk) * e1e2t(:,:) * fse3t(:,:,jk) )
         END DO
         !
      END SELECT

      SELECT CASE( ctype )    !==  Basin averaged tracer/momentum square trends  ==!   (now field)
      !
      CASE( 'DYN' )              ! Momentum
         hke(ktrd) = 0._wp
         DO jk = 1, jpkm1
            hke(ktrd) = hke(ktrd) + SUM(   un(:,:,jk) * ptrd3dx(:,:,jk) * e1u(:,:) * e2u(:,:) * fse3u(:,:,jk)   &
               &                         + vn(:,:,jk) * ptrd3dy(:,:,jk) * e1v(:,:) * e2v(:,:) * fse3v(:,:,jk)   )
         END DO
         !
      CASE( 'TRA' )              ! Tracers
         t2(ktrd) = 0._wp
         s2(ktrd) = 0._wp
         DO jk = 1, jpkm1
            t2(ktrd) = t2(ktrd) + SUM( ptrd3dx(:,:,jk) * tsn(:,:,jk,jp_tem) * e1e2t(:,:) * fse3t(:,:,jk) )
            s2(ktrd) = s2(ktrd) + SUM( ptrd3dy(:,:,jk) * tsn(:,:,jk,jp_sal) * e1e2t(:,:) * fse3t(:,:,jk) )
         END DO
         !
      END SELECT
      !
   END SUBROUTINE trd_3d


   SUBROUTINE trd_icp_init
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_icp_init  ***
      !! 
      !! ** Purpose :   Read the namtrd namelist
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'trd_icp_init : integral constraints properties trends'
         WRITE(numout,*) '~~~~~~~~~~~~~'
      ENDIF

      ! Total volume at t-points:
      tvolt = 0._wp
      DO jk = 1, jpkm1
         tvolt = SUM( e1e2t(:,:) * fse3t(:,:,jk) * tmask(:,:,jk) * tmask_i(:,:) )
      END DO
      IF( lk_mpp )   CALL mpp_sum( tvolt )   ! sum over the global domain

      IF(lwp) WRITE(numout,*) '                total ocean volume at T-point   tvolt = ',tvolt

#if  defined key_trddyn
      ! Initialization of potential to kinetic energy conversion
      rpktrd = 0._wp

      ! Total volume at u-, v- points:
      tvolu = 0._wp
      tvolv = 0._wp

      DO jk = 1, jpk
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               tvolu = tvolu + e1u(ji,jj) * e2u(ji,jj) * fse3u(ji,jj,jk) * tmask_i(ji+1,jj  ) * tmask_i(ji,jj) * umask(ji,jj,jk)
               tvolv = tvolv + e1v(ji,jj) * e2v(ji,jj) * fse3v(ji,jj,jk) * tmask_i(ji  ,jj+1) * tmask_i(ji,jj) * vmask(ji,jj,jk)
            END DO
         END DO
      END DO
      IF( lk_mpp )   CALL mpp_sum( tvolu )   ! sums over the global domain
      IF( lk_mpp )   CALL mpp_sum( tvolv )

      IF(lwp) THEN
         WRITE(numout,*) '                total ocean volume at U-point   tvolu = ',tvolu
         WRITE(numout,*) '                total ocean volume at V-point   tvolv = ',tvolv
      ENDIF
#endif
      !
   END SUBROUTINE trd_icp_init


   SUBROUTINE trd_dwr( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_dwr  ***
      !! 
      !! ** Purpose :  write dynamic trends in ocean.output 
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zcof         ! local scalar
      REAL(wp), POINTER, DIMENSION(:,:,:)  ::  zkx, zky, zkz, zkepe  
      !!----------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, jpk, zkx, zky, zkz, zkepe )

      ! I. Momentum trends
      ! -------------------

      IF( MOD(kt,nn_trd) == 0 .OR. kt == nit000 .OR. kt == nitend ) THEN

         ! I.1 Conversion potential energy - kinetic energy
         ! --------------------------------------------------
         ! c a u t i o n here, trends are computed at kt+1 (now , but after the swap)
         zkx  (:,:,:) = 0._wp
         zky  (:,:,:) = 0._wp
         zkz  (:,:,:) = 0._wp
         zkepe(:,:,:) = 0._wp
   
         CALL eos( tsn, rhd, rhop )       ! now potential and in situ densities

         zcof = 0.5_wp / rau0             ! Density flux at w-point
         zkz(:,:,1) = 0._wp
         DO jk = 2, jpk
            zkz(:,:,jk) = e1e2t(:,:) * wn(:,:,jk) * ( rhop(:,:,jk) + rhop(:,:,jk-1) ) * tmask_i(:,:)
         END DO
         
         zcof   = 0.5_wp / rau0           ! Density flux at u and v-points
         DO jk = 1, jpkm1
            DO jj = 1, jpjm1
               DO ji = 1, jpim1
                  zkx(ji,jj,jk) = zcof * e2u(ji,jj) * fse3u(ji,jj,jk) * un(ji,jj,jk) * ( rhop(ji,jj,jk) + rhop(ji+1,jj,jk) )
                  zky(ji,jj,jk) = zcof * e1v(ji,jj) * fse3v(ji,jj,jk) * vn(ji,jj,jk) * ( rhop(ji,jj,jk) + rhop(ji,jj+1,jk) )
               END DO
            END DO
         END DO
         
         DO jk = 1, jpkm1                 ! Density flux divergence at t-point
            DO jj = 2, jpjm1
               DO ji = 2, jpim1
                  zkepe(ji,jj,jk) = - (  zkz(ji,jj,jk) - zkz(ji  ,jj  ,jk+1)               &
                     &                 + zkx(ji,jj,jk) - zkx(ji-1,jj  ,jk  )               &
                     &                 + zky(ji,jj,jk) - zky(ji  ,jj-1,jk  )   )           &
                     &              / ( e1e2t(ji,jj) * fse3t(ji,jj,jk) ) * tmask(ji,jj,jk) * tmask_i(ji,jj)
               END DO
            END DO
         END DO

         ! I.2 Basin averaged kinetic energy trend
         ! ----------------------------------------
         peke = 0._wp
         DO jk = 1, jpkm1
            peke = peke + SUM( zkepe(:,:,jk) * fsdept(:,:,jk) * e1e2t(:,:) * fse3t(:,:,jk) )
         END DO
         peke = grav * peke

         ! I.3 Sums over the global domain
         ! ---------------------------------
         IF( lk_mpp ) THEN
            CALL mpp_sum( peke )
            CALL mpp_sum( umo , jptot_dyn )
            CALL mpp_sum( vmo , jptot_dyn )
            CALL mpp_sum( hke , jptot_dyn )
         ENDIF

         ! I.2 Print dynamic trends in the ocean.output file
         ! --------------------------------------------------

         IF(lwp) THEN
            WRITE (numout,*)
            WRITE (numout,*)
            WRITE (numout,9500) kt
            WRITE (numout,9501) umo(jpicpd_hpg) / tvolu, vmo(jpicpd_hpg) / tvolv
            WRITE (numout,9502) umo(jpicpd_keg) / tvolu, vmo(jpicpd_keg) / tvolv
            WRITE (numout,9503) umo(jpicpd_rvo) / tvolu, vmo(jpicpd_rvo) / tvolv
            WRITE (numout,9504) umo(jpicpd_pvo) / tvolu, vmo(jpicpd_pvo) / tvolv
            WRITE (numout,9505) umo(jpicpd_ldf) / tvolu, vmo(jpicpd_ldf) / tvolv
            WRITE (numout,9506) umo(jpicpd_had) / tvolu, vmo(jpicpd_had) / tvolv
            WRITE (numout,9507) umo(jpicpd_zad) / tvolu, vmo(jpicpd_zad) / tvolv
            WRITE (numout,9508) umo(jpicpd_zdf) / tvolu, vmo(jpicpd_zdf) / tvolv
            WRITE (numout,9509) umo(jpicpd_spg) / tvolu, vmo(jpicpd_spg) / tvolv
            WRITE (numout,9510) umo(jpicpd_swf) / tvolu, vmo(jpicpd_swf) / tvolv
            WRITE (numout,9511) umo(jpicpd_dat) / tvolu, vmo(jpicpd_dat) / tvolv
            WRITE (numout,9512) umo(jpicpd_bfr) / tvolu, vmo(jpicpd_bfr) / tvolv
            WRITE (numout,9513)
            WRITE (numout,9514)                                                 &
            &     (  umo(jpicpd_hpg) + umo(jpicpd_keg) + umo(jpicpd_rvo) + umo(jpicpd_pvo) + umo(jpicpd_ldf)   &
            &      + umo(jpicpd_had) + umo(jpicpd_zad) + umo(jpicpd_zdf) + umo(jpicpd_spg) + umo(jpicpd_dat)   &
            &      + umo(jpicpd_swf) + umo(jpicpd_bfr) ) / tvolu,   &
            &     (  vmo(jpicpd_hpg) + vmo(jpicpd_keg) + vmo(jpicpd_rvo) + vmo(jpicpd_pvo) + vmo(jpicpd_ldf)   &
            &      + vmo(jpicpd_had) + vmo(jpicpd_zad) + vmo(jpicpd_zdf) + vmo(jpicpd_spg) + vmo(jpicpd_dat)   &
            &      + vmo(jpicpd_swf) + vmo(jpicpd_bfr) ) / tvolv
         ENDIF

 9500    FORMAT(' momentum trend at it= ', i6, ' :', /' ==============================')
 9501    FORMAT(' pressure gradient          u= ', e20.13, '    v= ', e20.13)
 9502    FORMAT(' ke gradient                u= ', e20.13, '    v= ', e20.13)
 9503    FORMAT(' relative vorticity term    u= ', e20.13, '    v= ', e20.13)
 9504    FORMAT(' coriolis term              u= ', e20.13, '    v= ', e20.13)
 9505    FORMAT(' horizontal diffusion       u= ', e20.13, '    v= ', e20.13)
 9506    FORMAT(' horizontal advection       u= ', e20.13, '    v= ', e20.13)
 9507    FORMAT(' vertical advection         u= ', e20.13, '    v= ', e20.13)
 9508    FORMAT(' vertical diffusion         u= ', e20.13, '    v= ', e20.13)
 9509    FORMAT(' surface pressure gradient  u= ', e20.13, '    v= ', e20.13)
 9510    FORMAT(' surface wind forcing       u= ', e20.13, '    v= ', e20.13)
 9511    FORMAT(' dampimg term               u= ', e20.13, '    v= ', e20.13)
 9512    FORMAT(' bottom flux                u= ', e20.13, '    v= ', e20.13)
 9513    FORMAT(' -----------------------------------------------------------------------------')
 9514    FORMAT(' total trend                u= ', e20.13, '    v= ', e20.13)

         IF(lwp) THEN
            WRITE (numout,*)
            WRITE (numout,*)
            WRITE (numout,9520) kt
            WRITE (numout,9521) hke(jpicpd_hpg) / tvolt
            WRITE (numout,9522) hke(jpicpd_keg) / tvolt
            WRITE (numout,9523) hke(jpicpd_rvo) / tvolt
            WRITE (numout,9524) hke(jpicpd_pvo) / tvolt
            WRITE (numout,9525) hke(jpicpd_ldf) / tvolt
            WRITE (numout,9526) hke(jpicpd_had) / tvolt
            WRITE (numout,9527) hke(jpicpd_zad) / tvolt
            WRITE (numout,9528) hke(jpicpd_zdf) / tvolt
            WRITE (numout,9529) hke(jpicpd_spg) / tvolt
            WRITE (numout,9530) hke(jpicpd_swf) / tvolt
            WRITE (numout,9531) hke(jpicpd_dat) / tvolt
            WRITE (numout,9532) hke(jpicpd_bfr) / tvolt
            WRITE (numout,9533)
            WRITE (numout,9534)   &
            &     (  hke(jpicpd_hpg) + hke(jpicpd_keg) + hke(jpicpd_rvo) + hke(jpicpd_pvo) + hke(jpicpd_ldf)   &
            &      + hke(jpicpd_had) + hke(jpicpd_zad) + hke(jpicpd_zdf) + hke(jpicpd_spg) + hke(jpicpd_dat)   &
            &      + hke(jpicpd_swf) + hke(jpicpd_bfr) ) / tvolt
         ENDIF

 9520    FORMAT(' kinetic energy trend at it= ', i6, ' :', /' ====================================')
 9521    FORMAT(' pressure gradient         u2= ', e20.13)
 9522    FORMAT(' ke gradient               u2= ', e20.13)
 9523    FORMAT(' relative vorticity term   u2= ', e20.13)
 9524    FORMAT(' coriolis term             u2= ', e20.13)
 9525    FORMAT(' horizontal diffusion      u2= ', e20.13)
 9526    FORMAT(' horizontal advection      u2= ', e20.13)
 9527    FORMAT(' vertical advection        u2= ', e20.13)
 9528    FORMAT(' vertical diffusion        u2= ', e20.13)
 9529    FORMAT(' surface pressure gradient u2= ', e20.13)
 9530    FORMAT(' surface wind forcing      u2= ', e20.13)
 9531    FORMAT(' dampimg term              u2= ', e20.13)
 9532    FORMAT(' bottom flux               u2= ', e20.13)
 9533    FORMAT(' --------------------------------------------------')
 9534    FORMAT(' total trend               u2= ', e20.13)

         IF(lwp) THEN
            WRITE (numout,*)
            WRITE (numout,*)
            WRITE (numout,9540) kt
            WRITE (numout,9541) ( hke(jpicpd_keg) + hke(jpicpd_rvo) + hke(jpicpd_had) + hke(jpicpd_zad) ) / tvolt
            WRITE (numout,9542) ( hke(jpicpd_keg) + hke(jpicpd_had) + hke(jpicpd_zad) ) / tvolt
            WRITE (numout,9543) ( hke(jpicpd_pvo) ) / tvolt
            WRITE (numout,9544) ( hke(jpicpd_rvo) ) / tvolt
            WRITE (numout,9545) ( hke(jpicpd_spg) ) / tvolt
            WRITE (numout,9546) ( hke(jpicpd_ldf) ) / tvolt
            WRITE (numout,9547) ( hke(jpicpd_zdf) ) / tvolt
            WRITE (numout,9548) ( hke(jpicpd_hpg) ) / tvolt, rpktrd / tvolt
            WRITE (numout,*)
            WRITE (numout,*)
         ENDIF

 9540    FORMAT(' energetic consistency at it= ', i6, ' :', /' =========================================')
 9541    FORMAT(' 0 = non linear term(true if key_vorenergy or key_combined): ', e20.13)
 9542    FORMAT(' 0 = ke gradient + horizontal + vertical advection         : ', e20.13)
 9543    FORMAT(' 0 = coriolis term  (true if key_vorenergy or key_combined): ', e20.13)
 9544    FORMAT(' 0 = uh.( rot(u) x uh ) (true if enstrophy conser.)        : ', e20.13)
 9545    FORMAT(' 0 = surface pressure gradient                             : ', e20.13)
 9546    FORMAT(' 0 > horizontal diffusion                                  : ', e20.13)
 9547    FORMAT(' 0 > vertical diffusion                                    : ', e20.13)
 9548    FORMAT(' pressure gradient u2 = - 1/rau0 u.dz(rhop)                : ', e20.13, '  u.dz(rhop) =', e20.13)
         !
         ! Save potential to kinetic energy conversion for next time step
         rpktrd = peke
         !
      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, jpk, zkx, zky, zkz, zkepe )
      !
   END SUBROUTINE trd_dwr


   SUBROUTINE trd_twr( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_twr  ***
      !! 
      !! ** Purpose :  write active tracers trends in ocean.output 
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !!----------------------------------------------------------------------

      ! I. Tracers trends
      ! -----------------

      IF( MOD(kt,nn_trd) == 0 .OR. kt == nit000 .OR. kt == nitend ) THEN

         ! I.1 Sums over the global domain
         ! -------------------------------
         IF( lk_mpp ) THEN
            CALL mpp_sum( tmo, jptot_tra )   
            CALL mpp_sum( smo, jptot_tra )
            CALL mpp_sum( t2 , jptot_tra )
            CALL mpp_sum( s2 , jptot_tra )
         ENDIF

         ! I.2 Print tracers trends in the ocean.output file
         ! --------------------------------------------------
         
         IF(lwp) THEN
            WRITE (numout,*)
            WRITE (numout,*)
            WRITE (numout,9400) kt
            WRITE (numout,9401)  tmo(jpicpt_xad) / tvolt, smo(jpicpt_xad) / tvolt
            WRITE (numout,9411)  tmo(jpicpt_yad) / tvolt, smo(jpicpt_yad) / tvolt
            WRITE (numout,9402)  tmo(jpicpt_zad) / tvolt, smo(jpicpt_zad) / tvolt
            WRITE (numout,9403)  tmo(jpicpt_ldf) / tvolt, smo(jpicpt_ldf) / tvolt
            WRITE (numout,9404)  tmo(jpicpt_zdf) / tvolt, smo(jpicpt_zdf) / tvolt
            WRITE (numout,9405)  tmo(jpicpt_npc) / tvolt, smo(jpicpt_npc) / tvolt
            WRITE (numout,9406)  tmo(jpicpt_dmp) / tvolt, smo(jpicpt_dmp) / tvolt
            WRITE (numout,9407)  tmo(jpicpt_qsr) / tvolt
            WRITE (numout,9408)  tmo(jpicpt_nsr) / tvolt, smo(jpicpt_nsr) / tvolt
            WRITE (numout,9409) 
            WRITE (numout,9410) (  tmo(jpicpt_xad) + tmo(jpicpt_yad) + tmo(jpicpt_zad) + tmo(jpicpt_ldf) + tmo(jpicpt_zdf)   &
            &                    + tmo(jpicpt_npc) + tmo(jpicpt_dmp) + tmo(jpicpt_qsr) + tmo(jpicpt_nsr) ) / tvolt,   &
            &                   (  smo(jpicpt_xad) + smo(jpicpt_yad) + smo(jpicpt_zad) + smo(jpicpt_ldf) + smo(jpicpt_zdf)   &
            &                    + smo(jpicpt_npc) + smo(jpicpt_dmp)                   + smo(jpicpt_nsr) ) / tvolt
         ENDIF

9400     FORMAT(' tracer trend at it= ',i6,' :     temperature',   &
              '              salinity',/' ============================')
9401     FORMAT(' zonal      advection        ',e20.13,'     ',e20.13)
9411     FORMAT(' meridional advection        ',e20.13,'     ',e20.13)
9402     FORMAT(' vertical advection          ',e20.13,'     ',e20.13)
9403     FORMAT(' horizontal diffusion        ',e20.13,'     ',e20.13)
9404     FORMAT(' vertical diffusion          ',e20.13,'     ',e20.13)
9405     FORMAT(' static instability mixing   ',e20.13,'     ',e20.13)
9406     FORMAT(' damping term                ',e20.13,'     ',e20.13)
9407     FORMAT(' penetrative qsr             ',e20.13)
9408     FORMAT(' non solar radiation         ',e20.13,'     ',e20.13)
9409     FORMAT(' -------------------------------------------------------------------------')
9410     FORMAT(' total trend                 ',e20.13,'     ',e20.13)


         IF(lwp) THEN
            WRITE (numout,*)
            WRITE (numout,*)
            WRITE (numout,9420) kt
            WRITE (numout,9421)   t2(jpicpt_xad) / tvolt, s2(jpicpt_xad) / tvolt
            WRITE (numout,9431)   t2(jpicpt_yad) / tvolt, s2(jpicpt_yad) / tvolt
            WRITE (numout,9422)   t2(jpicpt_zad) / tvolt, s2(jpicpt_zad) / tvolt
            WRITE (numout,9423)   t2(jpicpt_ldf) / tvolt, s2(jpicpt_ldf) / tvolt
            WRITE (numout,9424)   t2(jpicpt_zdf) / tvolt, s2(jpicpt_zdf) / tvolt
            WRITE (numout,9425)   t2(jpicpt_npc) / tvolt, s2(jpicpt_npc) / tvolt
            WRITE (numout,9426)   t2(jpicpt_dmp) / tvolt, s2(jpicpt_dmp) / tvolt
            WRITE (numout,9427)   t2(jpicpt_qsr) / tvolt
            WRITE (numout,9428)   t2(jpicpt_nsr) / tvolt, s2(jpicpt_nsr) / tvolt
            WRITE (numout,9429)
            WRITE (numout,9430) (  t2(jpicpt_xad) + t2(jpicpt_yad) + t2(jpicpt_zad) + t2(jpicpt_ldf) + t2(jpicpt_zdf)   &
            &                    + t2(jpicpt_npc) + t2(jpicpt_dmp) + t2(jpicpt_qsr) + t2(jpicpt_nsr) ) / tvolt,   &
            &                   (  s2(jpicpt_xad) + s2(jpicpt_yad) + s2(jpicpt_zad) + s2(jpicpt_ldf) + s2(jpicpt_zdf)   &
            &                    + s2(jpicpt_npc) + s2(jpicpt_dmp)                  + s2(jpicpt_nsr) ) / tvolt
         ENDIF

9420     FORMAT(' tracer**2 trend at it= ', i6, ' :      temperature',   &
            '               salinity', /, ' ===============================')
9421     FORMAT(' zonal      advection      * t   ', e20.13, '     ', e20.13)
9431     FORMAT(' meridional advection      * t   ', e20.13, '     ', e20.13)
9422     FORMAT(' vertical advection        * t   ', e20.13, '     ', e20.13)
9423     FORMAT(' horizontal diffusion      * t   ', e20.13, '     ', e20.13)
9424     FORMAT(' vertical diffusion        * t   ', e20.13, '     ', e20.13)
9425     FORMAT(' static instability mixing * t   ', e20.13, '     ', e20.13)
9426     FORMAT(' damping term              * t   ', e20.13, '     ', e20.13)
9427     FORMAT(' penetrative qsr           * t   ', e20.13)
9428     FORMAT(' non solar radiation       * t   ', e20.13, '     ', e20.13)
9429     FORMAT(' -----------------------------------------------------------------------------')
9430     FORMAT(' total trend                *t = ', e20.13, '  *s = ', e20.13)


         IF(lwp) THEN
            WRITE (numout,*)
            WRITE (numout,*)
            WRITE (numout,9440) kt
            WRITE (numout,9441) ( tmo(jpicpt_xad)+tmo(jpicpt_yad)+tmo(jpicpt_zad) )/tvolt,   &
            &                   ( smo(jpicpt_xad)+smo(jpicpt_yad)+smo(jpicpt_zad) )/tvolt
            WRITE (numout,9442)   tmo(jpicpt_zl1)/tvolt,  smo(jpicpt_zl1)/tvolt
            WRITE (numout,9443)   tmo(jpicpt_ldf)/tvolt,  smo(jpicpt_ldf)/tvolt
            WRITE (numout,9444)   tmo(jpicpt_zdf)/tvolt,  smo(jpicpt_zdf)/tvolt
            WRITE (numout,9445)   tmo(jpicpt_npc)/tvolt,  smo(jpicpt_npc)/tvolt
            WRITE (numout,9446) ( t2(jpicpt_xad)+t2(jpicpt_yad)+t2(jpicpt_zad) )/tvolt,    &
            &                   ( s2(jpicpt_xad)+s2(jpicpt_yad)+s2(jpicpt_zad) )/tvolt
            WRITE (numout,9447)   t2(jpicpt_ldf)/tvolt,   s2(jpicpt_ldf)/tvolt
            WRITE (numout,9448)   t2(jpicpt_zdf)/tvolt,   s2(jpicpt_zdf)/tvolt
            WRITE (numout,9449)   t2(jpicpt_npc)/tvolt,   s2(jpicpt_npc)/tvolt
         ENDIF

9440     FORMAT(' tracer consistency at it= ',i6,   &
            ' :         temperature','                salinity',/,   &
            ' ==================================')
9441     FORMAT(' 0 = horizontal+vertical advection +    ',e20.13,'       ',e20.13)
9442     FORMAT('     1st lev vertical advection         ',e20.13,'       ',e20.13)
9443     FORMAT(' 0 = horizontal diffusion               ',e20.13,'       ',e20.13)
9444     FORMAT(' 0 = vertical diffusion                 ',e20.13,'       ',e20.13)
9445     FORMAT(' 0 = static instability mixing          ',e20.13,'       ',e20.13)
9446     FORMAT(' 0 = horizontal+vertical advection * t  ',e20.13,'       ',e20.13)
9447     FORMAT(' 0 > horizontal diffusion          * t  ',e20.13,'       ',e20.13)
9448     FORMAT(' 0 > vertical diffusion            * t  ',e20.13,'       ',e20.13)
9449     FORMAT(' 0 > static instability mixing     * t  ',e20.13,'       ',e20.13)
         !
      ENDIF
      !
   END SUBROUTINE trd_twr

#   else
   !!----------------------------------------------------------------------
   !!   Default case :                                         Empty module
   !!----------------------------------------------------------------------
   INTERFACE trd_icp
      MODULE PROCEDURE trd_2d, trd_3d
   END INTERFACE

CONTAINS
   SUBROUTINE trd_2d( ptrd2dx, ptrd2dy, ktrd , ctype )       ! Empty routine
      REAL, DIMENSION(:,:) ::   ptrd2dx, ptrd2dy
      INTEGER                     , INTENT(in   ) ::   ktrd         ! tracer trend index
      CHARACTER(len=3)            , INTENT(in   ) ::   ctype        ! momentum ('DYN') or tracers ('TRA') trends
      WRITE(*,*) 'trd_2d: You should not have seen this print! error ?', &
          &       ptrd2dx(1,1), ptrd2dy(1,1), ktrd, ctype
   END SUBROUTINE trd_2d
   SUBROUTINE trd_3d( ptrd3dx, ptrd3dy, ktrd , ctype )       ! Empty routine
      REAL, DIMENSION(:,:,:) ::   ptrd3dx, ptrd3dy
      INTEGER                     , INTENT(in   ) ::   ktrd         ! tracer trend index
      CHARACTER(len=3)            , INTENT(in   ) ::   ctype        ! momentum ('DYN') or tracers ('TRA') trends
      WRITE(*,*) 'trd_3d: You should not have seen this print! error ?', &
          &       ptrd3dx(1,1,1), ptrd3dy(1,1,1), ktrd, ctype
   END SUBROUTINE trd_3d
   SUBROUTINE trd_icp_init               ! Empty routine
   END SUBROUTINE trd_icp_init
   SUBROUTINE trd_dwr( kt )          ! Empty routine
      WRITE(*,*) 'trd_dwr: You should not have seen this print! error ?', kt
   END SUBROUTINE trd_dwr
   SUBROUTINE trd_twr( kt )          ! Empty routine
      WRITE(*,*) 'trd_twr: You should not have seen this print! error ?', kt
   END SUBROUTINE trd_twr
#endif

   !!======================================================================
END MODULE trdicp
