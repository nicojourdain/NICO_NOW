MODULE domvvl
   !!======================================================================
   !!                       ***  MODULE domvvl   ***
   !! Ocean : 
   !!======================================================================
   !! History :  2.0  !  2006-06  (B. Levier, L. Marie)  original code
   !!            3.1  !  2009-02  (G. Madec, M. Leclair, R. Benshila)  pure z* coordinate
   !!----------------------------------------------------------------------
#if defined key_vvl
   !!----------------------------------------------------------------------
   !!   'key_vvl'                              variable volume
   !!----------------------------------------------------------------------
   !!   dom_vvl     : defined coefficients to distribute ssh on each layers
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE sbc_oce         ! surface boundary condition: ocean
   USE phycst          ! physical constants
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distributed memory computing library
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE wrk_nemo        ! Memory allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_vvl         ! called by domain.F90
   PUBLIC   dom_vvl_2       ! called by domain.F90
   PUBLIC   dom_vvl_alloc   ! called by nemogcm.F90

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   mut , muu , muv , muf    !: 1/H_0 at t-,u-,v-,f-points 

   REAL(wp),         ALLOCATABLE, SAVE, DIMENSION(:)     ::   r2dt   ! vertical profile time-step, = 2 rdttra 
      !                                                              ! except at nit000 (=rdttra) if neuler=0

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: domvvl.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS       

   INTEGER FUNCTION dom_vvl_alloc()
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE dom_vvl_alloc  ***
      !!----------------------------------------------------------------------
      !
      ALLOCATE( mut (jpi,jpj,jpk) , muu (jpi,jpj,jpk) , muv (jpi,jpj,jpk) , muf (jpi,jpj,jpk) ,     &
         &      r2dt        (jpk)                                                             , STAT=dom_vvl_alloc )
         !
      IF( lk_mpp             )   CALL mpp_sum ( dom_vvl_alloc )
      IF( dom_vvl_alloc /= 0 )   CALL ctl_warn('dom_vvl_alloc: failed to allocate arrays')
      !
   END FUNCTION dom_vvl_alloc


   SUBROUTINE dom_vvl
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE dom_vvl  ***
      !!                   
      !! ** Purpose :   compute mu coefficients at t-, u-, v- and f-points to 
      !!              spread ssh over the whole water column (scale factors)
      !!                set the before and now ssh at u- and v-points 
      !!              (also f-point in now case)
      !!----------------------------------------------------------------------
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zcoefu, zcoefv , zcoeff                ! local scalars
      REAL(wp) ::   zvt   , zvt_ip1, zvt_jp1, zvt_ip1jp1   !   -      -
      REAL(wp), POINTER, DIMENSION(:,:) ::  zee_t, zee_u, zee_v, zee_f   ! 2D workspace
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dom_vvl')
      !
      CALL wrk_alloc( jpi, jpj, zee_t, zee_u, zee_v, zee_f )
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dom_vvl : Variable volume initialization'
         WRITE(numout,*) '~~~~~~~~  compute coef. used to spread ssh over each layers'
      ENDIF
      
      IF( dom_vvl_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'dom_vvl : unable to allocate arrays' )

      fsdept(:,:,:) = gdept (:,:,:)
      fsdepw(:,:,:) = gdepw (:,:,:)
      fsde3w(:,:,:) = gdep3w(:,:,:)
      fse3t (:,:,:) = e3t   (:,:,:)
      fse3u (:,:,:) = e3u   (:,:,:)
      fse3v (:,:,:) = e3v   (:,:,:)
      fse3f (:,:,:) = e3f   (:,:,:)
      fse3w (:,:,:) = e3w   (:,:,:)
      fse3uw(:,:,:) = e3uw  (:,:,:)
      fse3vw(:,:,:) = e3vw  (:,:,:)

      !                                 !==  mu computation  ==!
      zee_t(:,:) = fse3t_0(:,:,1)                ! Lower bound : thickness of the first model level
      zee_u(:,:) = fse3u_0(:,:,1)
      zee_v(:,:) = fse3v_0(:,:,1)
      zee_f(:,:) = fse3f_0(:,:,1)
      DO jk = 2, jpkm1                          ! Sum of the masked vertical scale factors
         zee_t(:,:) = zee_t(:,:) + fse3t_0(:,:,jk) * tmask(:,:,jk)
         zee_u(:,:) = zee_u(:,:) + fse3u_0(:,:,jk) * umask(:,:,jk)
         zee_v(:,:) = zee_v(:,:) + fse3v_0(:,:,jk) * vmask(:,:,jk)
         DO jj = 1, jpjm1                      ! f-point : fmask=shlat at coasts, use the product of umask
            zee_f(:,jj) = zee_f(:,jj) + fse3f_0(:,jj,jk) *  umask(:,jj,jk) * umask(:,jj+1,jk)
         END DO
      END DO  
      !                                         ! Compute and mask the inverse of the local depth at T, U, V and F points
      zee_t(:,:) = 1._wp / zee_t(:,:) * tmask(:,:,1)
      zee_u(:,:) = 1._wp / zee_u(:,:) * umask(:,:,1)
      zee_v(:,:) = 1._wp / zee_v(:,:) * vmask(:,:,1)
      DO jj = 1, jpjm1                               ! f-point case fmask cannot be used 
         zee_f(:,jj) = 1._wp / zee_f(:,jj) * umask(:,jj,1) * umask(:,jj+1,1)
      END DO
      CALL lbc_lnk( zee_f, 'F', 1. )                 ! lateral boundary condition on ee_f
      !
      DO jk = 1, jpk                            ! mu coefficients
         mut(:,:,jk) = zee_t(:,:) * tmask(:,:,jk)     ! T-point at T levels
         muu(:,:,jk) = zee_u(:,:) * umask(:,:,jk)     ! U-point at T levels
         muv(:,:,jk) = zee_v(:,:) * vmask(:,:,jk)     ! V-point at T levels
      END DO
      DO jk = 1, jpk                                 ! F-point : fmask=shlat at coasts, use the product of umask
         DO jj = 1, jpjm1
               muf(:,jj,jk) = zee_f(:,jj) * umask(:,jj,jk) * umask(:,jj+1,jk)   ! at T levels
         END DO
         muf(:,jpj,jk) = 0._wp
      END DO
      CALL lbc_lnk( muf, 'F', 1. )                   ! lateral boundary condition


      hu_0(:,:) = 0.e0                          ! Reference ocean depth at U- and V-points
      hv_0(:,:) = 0.e0
      DO jk = 1, jpk
         hu_0(:,:) = hu_0(:,:) + fse3u_0(:,:,jk) * umask(:,:,jk)
         hv_0(:,:) = hv_0(:,:) + fse3v_0(:,:,jk) * vmask(:,:,jk)
      END DO
      
      DO jj = 1, jpjm1                          ! initialise before and now Sea Surface Height at u-, v-, f-points
         DO ji = 1, jpim1   ! NO vector opt.
            zcoefu = 0.50_wp / ( e1u(ji,jj) * e2u(ji,jj) ) * umask(ji,jj,1)
            zcoefv = 0.50_wp / ( e1v(ji,jj) * e2v(ji,jj) ) * vmask(ji,jj,1)
            zcoeff = 0.25_wp / ( e1f(ji,jj) * e2f(ji,jj) ) * umask(ji,jj,1) * umask(ji,jj+1,1)
            !
            zvt           = e1e2t(ji  ,jj  ) * sshb(ji  ,jj  )    ! before fields
            zvt_ip1       = e1e2t(ji+1,jj  ) * sshb(ji+1,jj  )
            zvt_jp1       = e1e2t(ji  ,jj+1) * sshb(ji  ,jj+1)
            sshu_b(ji,jj) = zcoefu * ( zvt + zvt_ip1 )
            sshv_b(ji,jj) = zcoefv * ( zvt + zvt_jp1 )
            !
            zvt           = e1e2t(ji  ,jj  ) * sshn(ji  ,jj  )    ! now fields
            zvt_ip1       = e1e2t(ji+1,jj  ) * sshn(ji+1,jj  )
            zvt_jp1       = e1e2t(ji  ,jj+1) * sshn(ji  ,jj+1)
            zvt_ip1jp1    = e1e2t(ji+1,jj+1) * sshn(ji+1,jj+1)
            sshu_n(ji,jj) = zcoefu * ( zvt + zvt_ip1 )
            sshv_n(ji,jj) = zcoefv * ( zvt + zvt_jp1 )
            sshf_n(ji,jj) = zcoeff * ( zvt + zvt_ip1 + zvt_jp1 + zvt_ip1jp1 )
         END DO
      END DO
      CALL lbc_lnk( sshu_n, 'U', 1. )   ;   CALL lbc_lnk( sshu_b, 'U', 1. )      ! lateral boundary conditions
      CALL lbc_lnk( sshv_n, 'V', 1. )   ;   CALL lbc_lnk( sshv_b, 'V', 1. )
      CALL lbc_lnk( sshf_n, 'F', 1. )
      !
      CALL wrk_dealloc( jpi, jpj, zee_t, zee_u, zee_v, zee_f )
      !
      IF( nn_timing == 1 )  CALL timing_stop('dom_vvl')
      !
   END SUBROUTINE dom_vvl


   SUBROUTINE dom_vvl_2( kt, pe3u_b, pe3v_b )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE dom_vvl_2  ***
      !!                   
      !! ** Purpose :   compute the vertical scale factors at u- and v-points
      !!              in variable volume case.
      !!
      !! ** Method  :   In variable volume case (non linear sea surface) the 
      !!              the vertical scale factor at velocity points is computed
      !!              as the average of the cell surface weighted e3t.
      !!                It uses the sea surface heigth so it have to be initialized
      !!              after ssh is read/set
      !!----------------------------------------------------------------------
      INTEGER                   , INTENT(in   ) ::   kt               ! ocean time-step index
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   pe3u_b, pe3v_b   ! before vertical scale factor at u- & v-pts
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      INTEGER  ::   iku, ikv     ! local integers    
      INTEGER  ::   ii0, ii1, ij0, ij1   ! temporary integers
      REAL(wp) ::   zvt          ! local scalars
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dom_vvl_2')
      !
      IF( lwp .AND. kt == nit000 ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dom_vvl_2 : Variable volume, fse3t_b initialization'
         WRITE(numout,*) '~~~~~~~~~ '
         pe3u_b(:,:,jpk) = fse3u_0(:,:,jpk)
         pe3v_b(:,:,jpk) = fse3u_0(:,:,jpk)
      ENDIF
      
      DO jk = 1, jpkm1           ! set the before scale factors at u- & v-points
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1
               zvt = fse3t_b(ji,jj,jk) * e1e2t(ji,jj)
               pe3u_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji+1,jj,jk) * e1e2t(ji+1,jj) ) / ( e1u(ji,jj) * e2u(ji,jj) )
               pe3v_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji,jj+1,jk) * e1e2t(ji,jj+1) ) / ( e1v(ji,jj) * e2v(ji,jj) )
            END DO
         END DO
      END DO

      ! Correct scale factors at locations that have been individually modified in domhgr
      ! Such modifications break the relationship between e1e2t and e1u*e2u etc. Recompute
      ! scale factors ignoring the modified metric.
      !                                                ! =====================
      IF( cp_cfg == "orca" .AND. jp_cfg == 2 ) THEN    ! ORCA R2 configuration
         !                                             ! =====================
         IF( nn_cla == 0 ) THEN
            !
            ii0 = 139   ;   ii1 = 140        ! Gibraltar Strait (e2u was modified)
            ij0 = 102   ;   ij1 = 102   
            DO jk = 1, jpkm1                 ! set the before scale factors at u-points
               DO jj = mj0(ij0), mj1(ij1)
                  DO ji = mi0(ii0), mi1(ii1)
                     zvt = fse3t_b(ji,jj,jk) * e1t(ji,jj)
                     pe3u_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji+1,jj,jk) * e1t(ji+1,jj) ) / ( e1u(ji,jj) )
                  END DO
               END DO
            END DO
            !
            ii0 = 160   ;   ii1 = 160        ! Bab el Mandeb (e2u and e1v were modified)
            ij0 =  88   ;   ij1 =  88   
            DO jk = 1, jpkm1                 ! set the before scale factors at u-points
               DO jj = mj0(ij0), mj1(ij1)
                  DO ji = mi0(ii0), mi1(ii1)
                     zvt = fse3t_b(ji,jj,jk) * e1t(ji,jj)
                     pe3u_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji+1,jj,jk) * e1t(ji+1,jj) ) / ( e1u(ji,jj) )
                  END DO
               END DO
            END DO
            DO jk = 1, jpkm1                 ! set the before scale factors at v-points
               DO jj = mj0(ij0), mj1(ij1)
                  DO ji = mi0(ii0), mi1(ii1)
                     zvt = fse3t_b(ji,jj,jk) * e2t(ji,jj)
                     pe3v_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji,jj+1,jk) * e2t(ji,jj+1) ) / ( e2v(ji,jj) )
                  END DO
               END DO
            END DO
         ENDIF

         ii0 = 145   ;   ii1 = 146        ! Danish Straits (e2u was modified)
         ij0 = 116   ;   ij1 = 116   
         DO jk = 1, jpkm1                 ! set the before scale factors at u-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e1t(ji,jj)
                  pe3u_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji+1,jj,jk) * e1t(ji+1,jj) ) / ( e1u(ji,jj) )
               END DO
            END DO
         END DO
         !
      ENDIF
         !                                             ! =====================
      IF( cp_cfg == "orca" .AND. jp_cfg == 1 ) THEN    ! ORCA R1 configuration
         !                                             ! =====================

         ii0 = 281   ;   ii1 = 282        ! Gibraltar Strait (e2u was modified)
         ij0 = 200   ;   ij1 = 200   
         DO jk = 1, jpkm1                 ! set the before scale factors at u-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e1t(ji,jj)
                  pe3u_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji+1,jj,jk) * e1t(ji+1,jj) ) / ( e1u(ji,jj) )
               END DO
            END DO
         END DO

         ii0 = 314   ;   ii1 = 315        ! Bhosporus Strait (e2u was modified)
         ij0 = 208   ;   ij1 = 208   
         DO jk = 1, jpkm1                 ! set the before scale factors at u-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e1t(ji,jj)
                  pe3u_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji+1,jj,jk) * e1t(ji+1,jj) ) / ( e1u(ji,jj) )
               END DO
            END DO
         END DO

         ii0 =  44   ;   ii1 =  44        ! Lombok Strait (e1v was modified)
         ij0 = 124   ;   ij1 = 125   
         DO jk = 1, jpkm1                 ! set the before scale factors at v-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e2t(ji,jj)
                  pe3v_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji,jj+1,jk) * e2t(ji,jj+1) ) / ( e2v(ji,jj) )
               END DO
            END DO
         END DO

         ii0 =  48   ;   ii1 =  48        ! Sumba Strait (e1v was modified) [closed from bathy_11 on]
         ij0 = 124   ;   ij1 = 125   
         DO jk = 1, jpkm1                 ! set the before scale factors at v-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e2t(ji,jj)
                  pe3v_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji,jj+1,jk) * e2t(ji,jj+1) ) / ( e2v(ji,jj) )
               END DO
            END DO
         END DO

         ii0 =  53   ;   ii1 =  53        ! Ombai Strait (e1v was modified)
         ij0 = 124   ;   ij1 = 125   
         DO jk = 1, jpkm1                 ! set the before scale factors at v-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e2t(ji,jj)
                  pe3v_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji,jj+1,jk) * e2t(ji,jj+1) ) / ( e2v(ji,jj) )
               END DO
            END DO
         END DO

         ii0 =  56   ;   ii1 =  56        ! Timor Passage (e1v was modified)
         ij0 = 124   ;   ij1 = 125   
         DO jk = 1, jpkm1                 ! set the before scale factors at v-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e2t(ji,jj)
                  pe3v_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji,jj+1,jk) * e2t(ji,jj+1) ) / ( e2v(ji,jj) )
               END DO
            END DO
         END DO

         ii0 =  55   ;   ii1 =  55        ! West Halmahera Strait (e1v was modified)
         ij0 = 141   ;   ij1 = 142   
         DO jk = 1, jpkm1                 ! set the before scale factors at v-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e2t(ji,jj)
                  pe3v_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji,jj+1,jk) * e2t(ji,jj+1) ) / ( e2v(ji,jj) )
               END DO
            END DO
         END DO

         ii0 =  58   ;   ii1 =  58        ! East Halmahera Strait (e1v was modified)
         ij0 = 141   ;   ij1 = 142   
         DO jk = 1, jpkm1                 ! set the before scale factors at v-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e2t(ji,jj)
                  pe3v_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji,jj+1,jk) * e2t(ji,jj+1) ) / ( e2v(ji,jj) )
               END DO
            END DO
         END DO

         !
      ENDIF
      !                                                ! ======================
      IF( cp_cfg == "orca" .AND. jp_cfg == 05 ) THEN   ! ORCA R05 configuration
         !                                             ! ======================
         ii0 = 563   ;   ii1 = 564        ! Gibraltar Strait (e2u was modified)
         ij0 = 327   ;   ij1 = 327   
         DO jk = 1, jpkm1                 ! set the before scale factors at u-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e1t(ji,jj)
                  pe3u_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji+1,jj,jk) * e1t(ji+1,jj) ) / ( e1u(ji,jj) )
               END DO
            END DO
         END DO
         !
         ii0 = 627   ;   ii1 = 628        ! Bosphore Strait (e2u was modified)
         ij0 = 343   ;   ij1 = 343   
         DO jk = 1, jpkm1                 ! set the before scale factors at u-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e1t(ji,jj)
                  pe3u_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji+1,jj,jk) * e1t(ji+1,jj) ) / ( e1u(ji,jj) )
               END DO
            END DO
         END DO
         !
         ii0 =  93   ;   ii1 =  94        ! Sumba Strait (e2u was modified)
         ij0 = 232   ;   ij1 = 232   
         DO jk = 1, jpkm1                 ! set the before scale factors at u-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e1t(ji,jj)
                  pe3u_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji+1,jj,jk) * e1t(ji+1,jj) ) / ( e1u(ji,jj) )
               END DO
            END DO
         END DO
         !
         ii0 = 103   ;   ii1 = 103        ! Ombai Strait (e2u was modified)
         ij0 = 232   ;   ij1 = 232   
         DO jk = 1, jpkm1                 ! set the before scale factors at u-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e1t(ji,jj)
                  pe3u_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji+1,jj,jk) * e1t(ji+1,jj) ) / ( e1u(ji,jj) )
               END DO
            END DO
         END DO
         !
         ii0 =  15   ;   ii1 =  15        ! Palk Strait (e2u was modified)
         ij0 = 270   ;   ij1 = 270   
         DO jk = 1, jpkm1                 ! set the before scale factors at u-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e1t(ji,jj)
                  pe3u_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji+1,jj,jk) * e1t(ji+1,jj) ) / ( e1u(ji,jj) )
               END DO
            END DO
         END DO
         !
         ii0 =  87   ;   ii1 =  87        ! Lombok Strait (e1v was modified)
         ij0 = 232   ;   ij1 = 233   
         DO jk = 1, jpkm1                 ! set the before scale factors at v-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e2t(ji,jj)
                  pe3v_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji,jj+1,jk) * e2t(ji,jj+1) ) / ( e2v(ji,jj) )
               END DO
            END DO
         END DO
         !
         ii0 = 662   ;   ii1 = 662        ! Bab el Mandeb (e1v was modified)
         ij0 = 276   ;   ij1 = 276   
         DO jk = 1, jpkm1                 ! set the before scale factors at v-points
            DO jj = mj0(ij0), mj1(ij1)
               DO ji = mi0(ii0), mi1(ii1)
                  zvt = fse3t_b(ji,jj,jk) * e2t(ji,jj)
                  pe3v_b(ji,jj,jk) = 0.5_wp * ( zvt + fse3t_b(ji,jj+1,jk) * e2t(ji,jj+1) ) / ( e2v(ji,jj) )
               END DO
            END DO
         END DO
         !
      ENDIF
      ! End of individual corrections to scale factors

      IF( ln_zps ) THEN          ! minimum of the e3t at partial cell level
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1
               iku = mbku(ji,jj)
               ikv = mbkv(ji,jj)
               pe3u_b(ji,jj,iku) = MIN( fse3t_b(ji,jj,iku), fse3t_b(ji+1,jj  ,iku) ) 
               pe3v_b(ji,jj,ikv) = MIN( fse3t_b(ji,jj,ikv), fse3t_b(ji  ,jj+1,ikv) ) 
            END DO
         END DO
      ENDIF

      pe3u_b(:,:,:) = pe3u_b(:,:,:) - fse3u_0(:,:,:)      ! anomaly to avoid zero along closed boundary/extra halos
      pe3v_b(:,:,:) = pe3v_b(:,:,:) - fse3v_0(:,:,:)
      CALL lbc_lnk( pe3u_b(:,:,:), 'U', 1. )               ! lateral boundary conditions
      CALL lbc_lnk( pe3v_b(:,:,:), 'V', 1. )
      pe3u_b(:,:,:) = pe3u_b(:,:,:) + fse3u_0(:,:,:)      ! recover the full scale factor
      pe3v_b(:,:,:) = pe3v_b(:,:,:) + fse3v_0(:,:,:)
      !
      IF( nn_timing == 1 )  CALL timing_stop('dom_vvl_2')
      !
   END SUBROUTINE dom_vvl_2
   
#else
   !!----------------------------------------------------------------------
   !!   Default option :                                      Empty routine
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE dom_vvl
   END SUBROUTINE dom_vvl
   SUBROUTINE dom_vvl_2(kdum, pudum, pvdum )
      USE par_kind
      INTEGER                   , INTENT(in   ) ::   kdum       
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   pudum, pvdum
   END SUBROUTINE dom_vvl_2
#endif

   !!======================================================================
END MODULE domvvl
