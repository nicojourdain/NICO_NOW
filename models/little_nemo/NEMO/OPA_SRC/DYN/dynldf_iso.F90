MODULE dynldf_iso
   !!======================================================================
   !!                     ***  MODULE  dynldf_iso  ***
   !! Ocean dynamics:  lateral viscosity trend
   !!======================================================================
   !! History :  OPA  !  97-07  (G. Madec)  Original code
   !!  NEMO      1.0  !  2002-08  (G. Madec)  F90: Free form and module
   !!             -   !  2004-08  (C. Talandier) New trends organization
   !!            2.0  !  2005-11  (G. Madec)  s-coordinate: horizontal diffusion
   !!----------------------------------------------------------------------
#if defined key_ldfslp   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_ldfslp'                      slopes of the direction of mixing
   !!----------------------------------------------------------------------
   !!   dyn_ldf_iso  : update the momentum trend with the horizontal part
   !!                  of the lateral diffusion using isopycnal or horizon-
   !!                  tal s-coordinate laplacian operator.
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE ldfdyn_oce      ! ocean dynamics lateral physics
   USE ldftra_oce      ! ocean tracer   lateral physics
   USE zdf_oce         ! ocean vertical physics
   USE trdmod          ! ocean dynamics trends 
   USE trdmod_oce      ! ocean variables trends
   USE ldfslp          ! iso-neutral slopes 
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! MPP library
   USE prtctl          ! Print control
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_ldf_iso           ! called by step.F90
   PUBLIC   dyn_ldf_iso_alloc     ! called by nemogcm.F90

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) :: zfuw, zdiu, zdju, zdj1u   ! 2D workspace (dyn_ldf_iso) 
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) :: zfvw, zdiv, zdjv, zdj1v   !  -      -

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "ldfdyn_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2011)
   !! $Id: dynldf_iso.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION dyn_ldf_iso_alloc()
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_ldf_iso_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( zfuw(jpi,jpk) , zdiu(jpi,jpk) , zdju(jpi,jpk) , zdj1u(jpi,jpk) ,     & 
         &      zfvw(jpi,jpk) , zdiv(jpi,jpk) , zdjv(jpi,jpk) , zdj1v(jpi,jpk) , STAT=dyn_ldf_iso_alloc )
         !
      IF( dyn_ldf_iso_alloc /= 0 )   CALL ctl_warn('dyn_ldf_iso_alloc: array allocate failed.')
   END FUNCTION dyn_ldf_iso_alloc


   SUBROUTINE dyn_ldf_iso( kt )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dyn_ldf_iso  ***
      !!                       
      !! ** Purpose :   Compute the before trend of the rotated laplacian
      !!      operator of lateral momentum diffusion except the diagonal
      !!      vertical term that will be computed in dynzdf module. Add it
      !!      to the general trend of momentum equation.
      !!
      !! ** Method :
      !!        The momentum lateral diffusive trend is provided by a 2nd
      !!      order operator rotated along neutral or geopotential surfaces
      !!      (in s-coordinates).
      !!      It is computed using before fields (forward in time) and isopyc-
      !!      nal or geopotential slopes computed in routine ldfslp.
      !!      Here, u and v components are considered as 2 independent scalar
      !!      fields. Therefore, the property of splitting divergent and rota-
      !!      tional part of the flow of the standard, z-coordinate laplacian
      !!      momentum diffusion is lost.
      !!      horizontal fluxes associated with the rotated lateral mixing:
      !!      u-component:
      !!         ziut = ( ahmt + ahmb0 ) e2t * e3t / e1t  di[ ub ]
      !!               -      ahmt       e2t * mi-1(uslp) dk[ mi(mk(ub)) ]
      !!         zjuf = ( ahmf + ahmb0 ) e1f * e3f / e2f  dj[ ub ]
      !!               -      ahmf       e1f * mi(vslp)   dk[ mj(mk(ub)) ]
      !!      v-component:
      !!         zivf = ( ahmf + ahmb0 ) e2t * e3t / e1t  di[ vb ]
      !!               -      ahmf       e2t * mj(uslp)   dk[ mi(mk(vb)) ]
      !!         zjvt = ( ahmt + ahmb0 ) e1f * e3f / e2f  dj[ ub ]
      !!               -      ahmt       e1f * mj-1(vslp) dk[ mj(mk(vb)) ]
      !!      take the horizontal divergence of the fluxes:
      !!         diffu = 1/(e1u*e2u*e3u) {  di  [ ziut ] + dj-1[ zjuf ]  }
      !!         diffv = 1/(e1v*e2v*e3v) {  di-1[ zivf ] + dj  [ zjvt ]  }
      !!      Add this trend to the general trend (ua,va):
      !!         ua = ua + diffu
      !!      CAUTION: here the isopycnal part is with a coeff. of aht. This
      !!      should be modified for applications others than orca_r2 (!!bug)
      !!
      !! ** Action :
      !!        Update (ua,va) arrays with the before geopotential biharmonic
      !!      mixing trend.
      !!        Update (avmu,avmv) to accompt for the diagonal vertical component
      !!      of the rotated operator in dynzdf module
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zabe1, zabe2, zcof1, zcof2                       ! local scalars
      REAL(wp) ::   zmskt, zmskf, zbu, zbv, zuah, zvah               !   -      -
      REAL(wp) ::   zcoef0, zcoef3, zcoef4, zmkt, zmkf               !   -      -
      REAL(wp) ::   zuav, zvav, zuwslpi, zuwslpj, zvwslpi, zvwslpj   !   -      -
      !
      REAL(wp), POINTER, DIMENSION(:,:) :: ziut, zjuf, zjvt, zivf, zdku, zdk1u, zdkv, zdk1v
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dyn_ldf_iso')
      !
      CALL wrk_alloc( jpi, jpj, ziut, zjuf, zjvt, zivf, zdku, zdk1u, zdkv, zdk1v ) 
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn_ldf_iso : iso-neutral laplacian diffusive operator or '
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~   s-coordinate horizontal diffusive operator'
         !                                      ! allocate dyn_ldf_bilap arrays
         IF( dyn_ldf_iso_alloc() /= 0 )   CALL ctl_stop('STOP', 'dyn_ldf_iso: failed to allocate arrays')
      ENDIF

      ! s-coordinate: Iso-level diffusion on momentum but not on tracer
      IF( ln_dynldf_hor .AND. ln_traldf_iso ) THEN
         !
         DO jk = 1, jpk         ! set the slopes of iso-level
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  uslp (ji,jj,jk) = -1./e1u(ji,jj) * ( fsdept(ji+1,jj,jk) - fsdept(ji ,jj ,jk) ) * umask(ji,jj,jk)
                  vslp (ji,jj,jk) = -1./e2v(ji,jj) * ( fsdept(ji,jj+1,jk) - fsdept(ji ,jj ,jk) ) * vmask(ji,jj,jk)
                  wslpi(ji,jj,jk) = -1./e1t(ji,jj) * ( fsdepw(ji+1,jj,jk) - fsdepw(ji-1,jj,jk) ) * tmask(ji,jj,jk) * 0.5
                  wslpj(ji,jj,jk) = -1./e2t(ji,jj) * ( fsdepw(ji,jj+1,jk) - fsdepw(ji,jj-1,jk) ) * tmask(ji,jj,jk) * 0.5
               END DO
            END DO
         END DO
         ! Lateral boundary conditions on the slopes
         CALL lbc_lnk( uslp , 'U', -1. )      ;      CALL lbc_lnk( vslp , 'V', -1. )
         CALL lbc_lnk( wslpi, 'W', -1. )      ;      CALL lbc_lnk( wslpj, 'W', -1. )
 
!!bug
         IF( kt == nit000 ) then
            IF(lwp) WRITE(numout,*) ' max slop: u', SQRT( MAXVAL(uslp*uslp)), ' v ', SQRT(MAXVAL(vslp)),  &
               &                             ' wi', sqrt(MAXVAL(wslpi))     , ' wj', sqrt(MAXVAL(wslpj))
         endif
!!end
      ENDIF

      !                                                ! ===============
      DO jk = 1, jpkm1                                 ! Horizontal slab
         !                                             ! ===============

         ! Vertical u- and v-shears at level jk and jk+1
         ! ---------------------------------------------
         ! surface boundary condition: zdku(jk=1)=zdku(jk=2)
         !                             zdkv(jk=1)=zdkv(jk=2)

         zdk1u(:,:) = ( ub(:,:,jk) -ub(:,:,jk+1) ) * umask(:,:,jk+1)
         zdk1v(:,:) = ( vb(:,:,jk) -vb(:,:,jk+1) ) * vmask(:,:,jk+1)

         IF( jk == 1 ) THEN
            zdku(:,:) = zdk1u(:,:)
            zdkv(:,:) = zdk1v(:,:)
         ELSE
            zdku(:,:) = ( ub(:,:,jk-1) - ub(:,:,jk) ) * umask(:,:,jk)
            zdkv(:,:) = ( vb(:,:,jk-1) - vb(:,:,jk) ) * vmask(:,:,jk)
         ENDIF

         !                               -----f-----
         ! Horizontal fluxes on U             |  
         ! --------------------===        t   u   t
         !                                    |  
         ! i-flux at t-point             -----f-----

         IF( ln_zps ) THEN      ! z-coordinate - partial steps : min(e3u)
            DO jj = 2, jpjm1
               DO ji = fs_2, jpi   ! vector opt.
                  zabe1 = (fsahmt(ji,jj,jk)+ahmb0) * e2t(ji,jj) * MIN( fse3u(ji,jj,jk), fse3u(ji-1,jj,jk) ) / e1t(ji,jj)

                  zmskt = 1./MAX(  umask(ji-1,jj,jk  )+umask(ji,jj,jk+1)   &
                     &           + umask(ji-1,jj,jk+1)+umask(ji,jj,jk  ), 1. )

                  zcof1 = - aht0 * e2t(ji,jj) * zmskt * 0.5  * ( uslp(ji-1,jj,jk) + uslp(ji,jj,jk) )
   
                  ziut(ji,jj) = (  zabe1 * ( ub(ji,jj,jk) - ub(ji-1,jj,jk) )   &
                     &           + zcof1 * ( zdku (ji,jj) + zdk1u(ji-1,jj)     &
                     &                      +zdk1u(ji,jj) + zdku (ji-1,jj) )  ) * tmask(ji,jj,jk)
               END DO
            END DO
         ELSE                   ! other coordinate system (zco or sco) : e3t
            DO jj = 2, jpjm1
               DO ji = fs_2, jpi   ! vector opt.
                  zabe1 = (fsahmt(ji,jj,jk)+ahmb0) * e2t(ji,jj) * fse3t(ji,jj,jk) / e1t(ji,jj)

                  zmskt = 1./MAX(  umask(ji-1,jj,jk  )+umask(ji,jj,jk+1)   &
                     &           + umask(ji-1,jj,jk+1)+umask(ji,jj,jk  ), 1. )

                  zcof1 = - aht0 * e2t(ji,jj) * zmskt * 0.5  * ( uslp(ji-1,jj,jk) + uslp(ji,jj,jk) )

                  ziut(ji,jj) = (  zabe1 * ( ub(ji,jj,jk) - ub(ji-1,jj,jk) )   &
                     &           + zcof1 * ( zdku (ji,jj) + zdk1u(ji-1,jj)     &
                     &                      +zdk1u(ji,jj) + zdku (ji-1,jj) )  ) * tmask(ji,jj,jk)
               END DO
            END DO
         ENDIF

         ! j-flux at f-point
         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1   ! vector opt.
               zabe2 = ( fsahmf(ji,jj,jk) + ahmb0 ) * e1f(ji,jj) * fse3f(ji,jj,jk) / e2f(ji,jj)

               zmskf = 1./MAX(  umask(ji,jj+1,jk  )+umask(ji,jj,jk+1)   &
                  &           + umask(ji,jj+1,jk+1)+umask(ji,jj,jk  ), 1. )

               zcof2 = - aht0 * e1f(ji,jj) * zmskf * 0.5  * ( vslp(ji+1,jj,jk) + vslp(ji,jj,jk) )

               zjuf(ji,jj) = (  zabe2 * ( ub(ji,jj+1,jk) - ub(ji,jj,jk) )   &
                  &           + zcof2 * ( zdku (ji,jj+1) + zdk1u(ji,jj)     &
                  &                      +zdk1u(ji,jj+1) + zdku (ji,jj) )  ) * fmask(ji,jj,jk)
            END DO
         END DO

         !                                |   t   |
         ! Horizontal fluxes on V         |       |
         ! --------------------===        f---v---f
         !                                |       |
         ! i-flux at f-point              |   t   |

         DO jj = 2, jpjm1
            DO ji = 1, fs_jpim1   ! vector opt.
               zabe1 = ( fsahmf(ji,jj,jk) + ahmb0 ) * e2f(ji,jj) * fse3f(ji,jj,jk) / e1f(ji,jj)

               zmskf = 1./MAX(  vmask(ji+1,jj,jk  )+vmask(ji,jj,jk+1)   &
                  &           + vmask(ji+1,jj,jk+1)+vmask(ji,jj,jk  ), 1. )

               zcof1 = - aht0 * e2f(ji,jj) * zmskf * 0.5 * ( uslp(ji,jj+1,jk) + uslp(ji,jj,jk) )

               zivf(ji,jj) = (  zabe1 * ( vb(ji+1,jj,jk) - vb(ji,jj,jk) )   &
                  &           + zcof1 * ( zdkv (ji,jj) + zdk1v(ji+1,jj)     &
                  &                      +zdk1v(ji,jj) + zdkv (ji+1,jj) )  ) * fmask(ji,jj,jk)
            END DO
         END DO

         ! j-flux at t-point
         IF( ln_zps ) THEN      ! z-coordinate - partial steps : min(e3u)
            DO jj = 2, jpj
               DO ji = 1, fs_jpim1   ! vector opt.
                  zabe2 = (fsahmt(ji,jj,jk)+ahmb0) * e1t(ji,jj) * MIN( fse3v(ji,jj,jk), fse3v(ji,jj-1,jk) ) / e2t(ji,jj)

                  zmskt = 1./MAX(  vmask(ji,jj-1,jk  )+vmask(ji,jj,jk+1)   &
                     &           + vmask(ji,jj-1,jk+1)+vmask(ji,jj,jk  ), 1. )

                  zcof2 = - aht0 * e1t(ji,jj) * zmskt * 0.5 * ( vslp(ji,jj-1,jk) + vslp(ji,jj,jk) )

                  zjvt(ji,jj) = (  zabe2 * ( vb(ji,jj,jk) - vb(ji,jj-1,jk) )   &
                     &           + zcof2 * ( zdkv (ji,jj-1) + zdk1v(ji,jj)     &
                     &                      +zdk1v(ji,jj-1) + zdkv (ji,jj) )  ) * tmask(ji,jj,jk)
               END DO
            END DO
         ELSE                   ! other coordinate system (zco or sco) : e3t
            DO jj = 2, jpj
               DO ji = 1, fs_jpim1   ! vector opt.
                  zabe2 = (fsahmt(ji,jj,jk)+ahmb0) * e1t(ji,jj) * fse3t(ji,jj,jk) / e2t(ji,jj)

                  zmskt = 1./MAX(  vmask(ji,jj-1,jk  )+vmask(ji,jj,jk+1)   &
                     &           + vmask(ji,jj-1,jk+1)+vmask(ji,jj,jk  ), 1. )

                  zcof2 = - aht0 * e1t(ji,jj) * zmskt * 0.5 * ( vslp(ji,jj-1,jk) + vslp(ji,jj,jk) )

                  zjvt(ji,jj) = (  zabe2 * ( vb(ji,jj,jk) - vb(ji,jj-1,jk) )   &
                     &           + zcof2 * ( zdkv (ji,jj-1) + zdk1v(ji,jj)     &
                     &                      +zdk1v(ji,jj-1) + zdkv (ji,jj) )  ) * tmask(ji,jj,jk)
               END DO
            END DO
         ENDIF


         ! Second derivative (divergence) and add to the general trend
         ! -----------------------------------------------------------

         DO jj = 2, jpjm1
            DO ji = 2, jpim1          !! Question vectop possible??? !!bug
               ! volume elements
               zbu = e1u(ji,jj) * e2u(ji,jj) * fse3u(ji,jj,jk)
               zbv = e1v(ji,jj) * e2v(ji,jj) * fse3v(ji,jj,jk)
               ! horizontal component of isopycnal momentum diffusive trends
               zuah =( ziut (ji+1,jj) - ziut (ji,jj  ) +   &
                  &    zjuf (ji  ,jj) - zjuf (ji,jj-1)  ) / zbu
               zvah =( zivf (ji,jj  ) - zivf (ji-1,jj) +   &
                  &    zjvt (ji,jj+1) - zjvt (ji,jj  )  ) / zbv
               ! add the trends to the general trends
               ua (ji,jj,jk) = ua (ji,jj,jk) + zuah
               va (ji,jj,jk) = va (ji,jj,jk) + zvah
            END DO
         END DO
         !                                             ! ===============
      END DO                                           !   End of slab
      !                                                ! ===============

      ! print sum trends (used for debugging)
      IF(ln_ctl)   CALL prt_ctl( tab3d_1=ua, clinfo1=' ldfh - Ua: ', mask1=umask, &
         &                       tab3d_2=va, clinfo2=       ' Va: ', mask2=vmask, clinfo3='dyn' )


      !                                                ! ===============
      DO jj = 2, jpjm1                                 !  Vertical slab
         !                                             ! ===============

 
         ! I. vertical trends associated with the lateral mixing
         ! =====================================================
         !  (excluding the vertical flux proportional to dk[t]


         ! I.1 horizontal momentum gradient
         ! --------------------------------

         DO jk = 1, jpk
            DO ji = 2, jpi
               ! i-gradient of u at jj
               zdiu (ji,jk) = tmask(ji,jj  ,jk) * ( ub(ji,jj  ,jk) - ub(ji-1,jj  ,jk) )
               ! j-gradient of u and v at jj
               zdju (ji,jk) = fmask(ji,jj  ,jk) * ( ub(ji,jj+1,jk) - ub(ji  ,jj  ,jk) )
               zdjv (ji,jk) = tmask(ji,jj  ,jk) * ( vb(ji,jj  ,jk) - vb(ji  ,jj-1,jk) )
               ! j-gradient of u and v at jj+1
               zdj1u(ji,jk) = fmask(ji,jj-1,jk) * ( ub(ji,jj  ,jk) - ub(ji  ,jj-1,jk) )
               zdj1v(ji,jk) = tmask(ji,jj+1,jk) * ( vb(ji,jj+1,jk) - vb(ji  ,jj  ,jk) )
            END DO
         END DO
         DO jk = 1, jpk
            DO ji = 1, jpim1
               ! i-gradient of v at jj
               zdiv (ji,jk) = fmask(ji,jj  ,jk) * ( vb(ji+1,jj,jk) - vb(ji  ,jj  ,jk) )
            END DO
         END DO


         ! I.2 Vertical fluxes
         ! -------------------

         ! Surface and bottom vertical fluxes set to zero
         DO ji = 1, jpi
            zfuw(ji, 1 ) = 0.e0
            zfvw(ji, 1 ) = 0.e0
            zfuw(ji,jpk) = 0.e0
            zfvw(ji,jpk) = 0.e0
         END DO

         ! interior (2=<jk=<jpk-1) on U field
         DO jk = 2, jpkm1
            DO ji = 2, jpim1
               zcoef0= 0.5 * aht0 * umask(ji,jj,jk)

               zuwslpi = zcoef0 * ( wslpi(ji+1,jj,jk) + wslpi(ji,jj,jk) )
               zuwslpj = zcoef0 * ( wslpj(ji+1,jj,jk) + wslpj(ji,jj,jk) )

               zmkt = 1./MAX(  tmask(ji,jj,jk-1)+tmask(ji+1,jj,jk-1)   &
                             + tmask(ji,jj,jk  )+tmask(ji+1,jj,jk  ), 1. )
               zmkf = 1./MAX(  fmask(ji,jj-1,jk-1)+fmask(ji,jj,jk-1)   &
                             + fmask(ji,jj-1,jk  )+fmask(ji,jj,jk  ), 1. )

               zcoef3 = - e2u(ji,jj) * zmkt * zuwslpi
               zcoef4 = - e1u(ji,jj) * zmkf * zuwslpj
               ! vertical flux on u field
               zfuw(ji,jk) = zcoef3 * ( zdiu (ji,jk-1) + zdiu (ji+1,jk-1)     &
                                       +zdiu (ji,jk  ) + zdiu (ji+1,jk  ) )   &
                           + zcoef4 * ( zdj1u(ji,jk-1) + zdju (ji  ,jk-1)     &
                                       +zdj1u(ji,jk  ) + zdju (ji  ,jk  ) )
               ! update avmu (add isopycnal vertical coefficient to avmu)
               ! Caution: zcoef0 include aht0, so divided by aht0 to obtain slp^2 * aht0
               avmu(ji,jj,jk) = avmu(ji,jj,jk) + ( zuwslpi * zuwslpi + zuwslpj * zuwslpj ) / aht0
            END DO
         END DO

         ! interior (2=<jk=<jpk-1) on V field
         DO jk = 2, jpkm1
            DO ji = 2, jpim1
               zcoef0= 0.5 * aht0 * vmask(ji,jj,jk)

               zvwslpi = zcoef0 * ( wslpi(ji,jj+1,jk) + wslpi(ji,jj,jk) )
               zvwslpj = zcoef0 * ( wslpj(ji,jj+1,jk) + wslpj(ji,jj,jk) )

               zmkf = 1./MAX(  fmask(ji-1,jj,jk-1)+fmask(ji,jj,jk-1)   &
                             + fmask(ji-1,jj,jk  )+fmask(ji,jj,jk  ), 1. )
               zmkt = 1./MAX(  tmask(ji,jj,jk-1)+tmask(ji,jj+1,jk-1)   &
                             + tmask(ji,jj,jk  )+tmask(ji,jj+1,jk  ), 1. )

               zcoef3 = - e2v(ji,jj) * zmkf * zvwslpi
               zcoef4 = - e1v(ji,jj) * zmkt * zvwslpj
               ! vertical flux on v field
               zfvw(ji,jk) = zcoef3 * ( zdiv (ji,jk-1) + zdiv (ji-1,jk-1)     &
                                       +zdiv (ji,jk  ) + zdiv (ji-1,jk  ) )   &
                           + zcoef4 * ( zdjv (ji,jk-1) + zdj1v(ji  ,jk-1)     &
                                       +zdjv (ji,jk  ) + zdj1v(ji  ,jk  ) )
               ! update avmv (add isopycnal vertical coefficient to avmv)
               ! Caution: zcoef0 include aht0, so divided by aht0 to obtain slp^2 * aht0
               avmv(ji,jj,jk) = avmv(ji,jj,jk) + ( zvwslpi * zvwslpi + zvwslpj * zvwslpj ) / aht0
            END DO
         END DO


         ! I.3 Divergence of vertical fluxes added to the general tracer trend
         ! -------------------------------------------------------------------
         DO jk = 1, jpkm1
            DO ji = 2, jpim1
               ! volume elements
               zbu = e1u(ji,jj) * e2u(ji,jj) * fse3u(ji,jj,jk)
               zbv = e1v(ji,jj) * e2v(ji,jj) * fse3v(ji,jj,jk)
               ! part of the k-component of isopycnal momentum diffusive trends
               zuav = ( zfuw(ji,jk) - zfuw(ji,jk+1) ) / zbu
               zvav = ( zfvw(ji,jk) - zfvw(ji,jk+1) ) / zbv
               ! add the trends to the general trends
               ua(ji,jj,jk) = ua(ji,jj,jk) + zuav
               va(ji,jj,jk) = va(ji,jj,jk) + zvav
            END DO
         END DO
         !                                             ! ===============
      END DO                                           !   End of slab
      !                                                ! ===============
      CALL wrk_dealloc( jpi, jpj, ziut, zjuf, zjvt, zivf, zdku, zdk1u, zdkv, zdk1v ) 
      !
      IF( nn_timing == 1 )  CALL timing_stop('dyn_ldf_iso')
      !
   END SUBROUTINE dyn_ldf_iso

# else
   !!----------------------------------------------------------------------
   !!   Dummy module                           NO rotation of mixing tensor
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE dyn_ldf_iso( kt )               ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'dyn_ldf_iso: You should not have seen this print! error?', kt
   END SUBROUTINE dyn_ldf_iso
#endif

   !!======================================================================
END MODULE dynldf_iso
