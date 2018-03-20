MODULE traadv_qck
   !!==============================================================================
   !!                       ***  MODULE  traadv_qck  ***
   !! Ocean tracers:  horizontal & vertical advective trend
   !!==============================================================================
   !! History :  3.0  !  2008-07  (G. Reffray)  Original code
   !!            3.3  !  2010-05  (C.Ethe, G. Madec)  merge TRC-TRA + switch from velocity to transport
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_adv_qck    : update the tracer trend with the horizontal advection
   !!                    trends using a 3rd order finite difference scheme
   !!   tra_adv_qck_i  : apply QUICK scheme in i-direction
   !!   tra_adv_qck_j  : apply QUICK scheme in j-direction
   !!   tra_adv_cen2_k : 2nd centered scheme for the vertical advection
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and active tracers
   USE dom_oce         ! ocean space and time domain
   USE trdmod_oce      ! ocean space and time domain
   USE trdtra          ! ocean tracers trends 
   USE trabbl          ! advective term in the BBL
   USE lib_mpp         ! distribued memory computing
   USE lbclnk          ! ocean lateral boundary condition (or mpp link)
   USE dynspg_oce      ! surface pressure gradient variables
   USE in_out_manager  ! I/O manager
   USE diaptr          ! poleward transport diagnostics
   USE trc_oce         ! share passive tracers/Ocean variables
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_adv_qck   ! routine called by step.F90

   LOGICAL  :: l_trd           ! flag to compute trends
   REAL(wp) :: r1_6 = 1./ 6.   ! 1/6 ratio

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: traadv_qck.F90 3301 2012-02-08 16:56:27Z cbricaud $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_adv_qck ( kt, kit000, cdtype, p2dt, pun, pvn, pwn,      &
      &                                       ptb, ptn, pta, kjpt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_adv_qck  ***
      !!
      !! ** Purpose :   Compute the now trend due to the advection of tracers
      !!      and add it to the general trend of passive tracer equations.
      !!
      !! ** Method :   The advection is evaluated by a third order scheme
      !!             For a positive velocity u :              u(i)>0
      !!                                          |--FU--|--FC--|--FD--|------|
      !!                                             i-1    i      i+1   i+2
      !!
      !!             For a negative velocity u :              u(i)<0
      !!                                          |------|--FD--|--FC--|--FU--|
      !!                                             i-1    i      i+1   i+2
      !!             where  FU is the second upwind point
      !!                    FD is the first douwning point
      !!                    FC is the central point (or the first upwind point)
      !!
      !!      Flux(i) = u(i) * { 0.5(FC+FD)  -0.5C(i)(FD-FC)  -((1-C(i))/6)(FU+FD-2FC) }
      !!                with C(i)=|u(i)|dx(i)/dt (=Courant number)
      !!
      !!         dt = 2*rdtra and the scalar values are tb and sb
      !!
      !!       On the vertical, the simple centered scheme used ptn
      !!
      !!               The fluxes are bounded by the ULTIMATE limiter to
      !!             guarantee the monotonicity of the solution and to
      !!            prevent the appearance of spurious numerical oscillations
      !!
      !! ** Action : - update (pta) with the now advective tracer trends
      !!             - save the trends 
      !!
      !! ** Reference : Leonard (1979, 1991)
      !!----------------------------------------------------------------------
      INTEGER                              , INTENT(in   ) ::   kt              ! ocean time-step index
      INTEGER                              , INTENT(in   ) ::   kit000          ! first time step index
      CHARACTER(len=3)                     , INTENT(in   ) ::   cdtype          ! =TRA or TRC (tracer indicator)
      INTEGER                              , INTENT(in   ) ::   kjpt            ! number of tracers
      REAL(wp), DIMENSION(        jpk     ), INTENT(in   ) ::   p2dt            ! vertical profile of tracer time-step
      REAL(wp), DIMENSION(jpi,jpj,jpk     ), INTENT(in   ) ::   pun, pvn, pwn   ! 3 ocean velocity components
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(in   ) ::   ptb, ptn        ! before and now tracer fields
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(inout) ::   pta             ! tracer trend 
      !!----------------------------------------------------------------------

      !
      IF( nn_timing == 1 )  CALL timing_start('tra_adv_qck')
      !
      IF( kt == kit000 )  THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'tra_adv_qck : 3rd order quickest advection scheme on ', cdtype
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'
         IF(lwp) WRITE(numout,*)
         !
         l_trd = .FALSE.
         IF( ( cdtype == 'TRA' .AND. l_trdtra ) .OR. ( cdtype == 'TRC' .AND. l_trdtrc ) ) l_trd = .TRUE.
      ENDIF

      ! I. The horizontal fluxes are computed with the QUICKEST + ULTIMATE scheme
      CALL tra_adv_qck_i( kt, cdtype, p2dt, pun, ptb, ptn, pta, kjpt ) 
      CALL tra_adv_qck_j( kt, cdtype, p2dt, pvn, ptb, ptn, pta, kjpt ) 

      ! II. The vertical fluxes are computed with the 2nd order centered scheme
      CALL tra_adv_cen2_k( kt, cdtype, pwn,         ptn, pta, kjpt )
      !
      IF( nn_timing == 1 )  CALL timing_stop('tra_adv_qck')
      !
   END SUBROUTINE tra_adv_qck


   SUBROUTINE tra_adv_qck_i( kt, cdtype, p2dt, pun,                  &
      &                                        ptb, ptn, pta, kjpt   )
      !!----------------------------------------------------------------------
      !!
      !!----------------------------------------------------------------------
      USE oce     , ONLY:   zwx => ua       ! ua used as workspace
      !
      INTEGER                              , INTENT(in   ) ::   kt         ! ocean time-step index
      CHARACTER(len=3)                     , INTENT(in   ) ::   cdtype     ! =TRA or TRC (tracer indicator)
      INTEGER                              , INTENT(in   ) ::   kjpt       ! number of tracers
      REAL(wp), DIMENSION(        jpk     ), INTENT(in   ) ::   p2dt       ! vertical profile of tracer time-step
      REAL(wp), DIMENSION(jpi,jpj,jpk     ), INTENT(in   ) ::   pun        ! i-velocity components
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(in   ) ::   ptb, ptn   ! before and now tracer fields
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(inout) ::   pta        ! tracer trend 
      !!
      INTEGER  :: ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) :: ztra, zbtr, zdir, zdx, zdt, zmsk   ! local scalars
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zfu, zfc, zfd
      !----------------------------------------------------------------------
      !
      CALL wrk_alloc( jpi, jpj, jpk, zfu, zfc, zfd )
      !                                                          ! ===========
      DO jn = 1, kjpt                                            ! tracer loop
         !                                                       ! ===========
         zfu(:,:,:) = 0.0     ;   zfc(:,:,:) = 0.0  
         zfd(:,:,:) = 0.0     ;   zwx(:,:,:) = 0.0     
         !                                                  
         DO jk = 1, jpkm1                                
            !                                             
            !--- Computation of the ustream and downstream value of the tracer and the mask
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  ! Upstream in the x-direction for the tracer
                  zfc(ji,jj,jk) = ptb(ji-1,jj,jk,jn)
                  ! Downstream in the x-direction for the tracer
                  zfd(ji,jj,jk) = ptb(ji+1,jj,jk,jn)
               END DO
            END DO
         END DO
         CALL lbc_lnk( zfc(:,:,:), 'T', 1. )   ;   CALL lbc_lnk( zfd(:,:,:), 'T', 1. )   ! Lateral boundary conditions 
         
         !
         ! Horizontal advective fluxes
         ! ---------------------------
         !
         DO jk = 1, jpkm1                             
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.         
                  zdir = 0.5 + SIGN( 0.5, pun(ji,jj,jk) )   ! if pun > 0 : zdir = 1 otherwise zdir = 0 
                  zfu(ji,jj,jk) = zdir * zfc(ji,jj,jk ) + ( 1. - zdir ) * zfd(ji+1,jj,jk)  ! FU in the x-direction for T 
               END DO
            END DO
         END DO
         !
         DO jk = 1, jpkm1  
            zdt =  p2dt(jk)
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.   
                  zdir = 0.5 + SIGN( 0.5, pun(ji,jj,jk) )   ! if pun > 0 : zdir = 1 otherwise zdir = 0 
                  zdx = ( zdir * e1t(ji,jj) + ( 1. - zdir ) * e1t(ji+1,jj) ) * e2u(ji,jj) * fse3u(ji,jj,jk)
                  zwx(ji,jj,jk)  = ABS( pun(ji,jj,jk) ) * zdt / zdx    ! (0<zc_cfl<1 : Courant number on x-direction)
                  zfc(ji,jj,jk)  = zdir * ptb(ji  ,jj,jk,jn) + ( 1. - zdir ) * ptb(ji+1,jj,jk,jn)  ! FC in the x-direction for T
                  zfd(ji,jj,jk)  = zdir * ptb(ji+1,jj,jk,jn) + ( 1. - zdir ) * ptb(ji  ,jj,jk,jn)  ! FD in the x-direction for T
               END DO
            END DO
         END DO 
         !--- Lateral boundary conditions 
         CALL lbc_lnk( zfu(:,:,:), 'T', 1. )   ;   CALL lbc_lnk( zfd(:,:,:), 'T', 1. )
         CALL lbc_lnk( zfc(:,:,:), 'T', 1. )   ;   CALL lbc_lnk( zwx(:,:,:), 'T', 1. )

         !--- QUICKEST scheme
         CALL quickest( zfu, zfd, zfc, zwx )
         !
         ! Mask at the T-points in the x-direction (mask=0 or mask=1)
         DO jk = 1, jpkm1  
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.               
                  zfu(ji,jj,jk) = tmask(ji-1,jj,jk) + tmask(ji,jj,jk) + tmask(ji+1,jj,jk) - 2.
               END DO
            END DO
         END DO
         CALL lbc_lnk( zfu(:,:,:), 'T', 1. )      ! Lateral boundary conditions 

         !
         ! Tracer flux on the x-direction
         DO jk = 1, jpkm1  
            !
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.               
                  zdir = 0.5 + SIGN( 0.5, pun(ji,jj,jk) )   ! if pun > 0 : zdir = 1 otherwise zdir = 0 
                  !--- If the second ustream point is a land point
                  !--- the flux is computed by the 1st order UPWIND scheme
                  zmsk = zdir * zfu(ji,jj,jk) + ( 1. - zdir ) * zfu(ji+1,jj,jk)
                  zwx(ji,jj,jk) = zmsk * zwx(ji,jj,jk) + ( 1. - zmsk ) * zfc(ji,jj,jk)
                  zwx(ji,jj,jk) = zwx(ji,jj,jk) * pun(ji,jj,jk)
               END DO
            END DO
         END DO
         !
         CALL lbc_lnk( zwx(:,:,:), 'T', 1. ) ! Lateral boundary conditions
         !
         ! Computation of the trend
         DO jk = 1, jpkm1  
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.  
                  zbtr = 1. / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
                  ! horizontal advective trends
                  ztra = - zbtr * ( zwx(ji,jj,jk) - zwx(ji-1,jj,jk) )
                  !--- add it to the general tracer trends
                  pta(ji,jj,jk,jn) = pta(ji,jj,jk,jn) + ztra
               END DO
            END DO
         END DO
         !                                 ! trend diagnostics (contribution of upstream fluxes)
         IF( l_trd )  CALL trd_tra( kt, cdtype, jn, jptra_trd_xad, zwx, pun, ptn(:,:,:,jn) )
         !
      END DO
      !
      CALL wrk_dealloc( jpi, jpj, jpk, zfu, zfc, zfd )
      !
   END SUBROUTINE tra_adv_qck_i


   SUBROUTINE tra_adv_qck_j( kt, cdtype, p2dt, pvn,                &
      &                                        ptb, ptn, pta, kjpt )
      !!----------------------------------------------------------------------
      !!
      !!----------------------------------------------------------------------
      USE oce     , ONLY:   zwy => ua       ! ua used as workspace
      !
      INTEGER                              , INTENT(in   ) ::   kt         ! ocean time-step index
      CHARACTER(len=3)                     , INTENT(in   ) ::   cdtype     ! =TRA or TRC (tracer indicator)
      INTEGER                              , INTENT(in   ) ::   kjpt       ! number of tracers
      REAL(wp), DIMENSION(        jpk     ), INTENT(in   ) ::   p2dt       ! vertical profile of tracer time-step
      REAL(wp), DIMENSION(jpi,jpj,jpk     ), INTENT(in   ) ::   pvn        ! j-velocity components
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(in   ) ::   ptb, ptn   ! before and now tracer fields
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(inout) ::   pta        ! tracer trend 
      !!
      INTEGER  :: ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) :: ztra, zbtr, zdir, zdx, zdt, zmsk   ! local scalars
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zfu, zfc, zfd
      !----------------------------------------------------------------------
      !
      CALL wrk_alloc( jpi, jpj, jpk, zfu, zfc, zfd )
      !
      !                                                          ! ===========
      DO jn = 1, kjpt                                            ! tracer loop
         !                                                       ! ===========
         zfu(:,:,:) = 0.0     ;   zfc(:,:,:) = 0.0  
         zfd(:,:,:) = 0.0     ;   zwy(:,:,:) = 0.0     
         !                                                  
         DO jk = 1, jpkm1                                
            !                                             
            !--- Computation of the ustream and downstream value of the tracer and the mask
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  ! Upstream in the x-direction for the tracer
                  zfc(ji,jj,jk) = ptb(ji,jj-1,jk,jn)
                  ! Downstream in the x-direction for the tracer
                  zfd(ji,jj,jk) = ptb(ji,jj+1,jk,jn)
               END DO
            END DO
         END DO
         CALL lbc_lnk( zfc(:,:,:), 'T', 1. )   ;   CALL lbc_lnk( zfd(:,:,:), 'T', 1. )   ! Lateral boundary conditions 

         
         !
         ! Horizontal advective fluxes
         ! ---------------------------
         !
         DO jk = 1, jpkm1                             
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.         
                  zdir = 0.5 + SIGN( 0.5, pvn(ji,jj,jk) )   ! if pun > 0 : zdir = 1 otherwise zdir = 0 
                  zfu(ji,jj,jk) = zdir * zfc(ji,jj,jk ) + ( 1. - zdir ) * zfd(ji,jj+1,jk)  ! FU in the x-direction for T 
               END DO
            END DO
         END DO
         !
         DO jk = 1, jpkm1  
            zdt =  p2dt(jk)
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.   
                  zdir = 0.5 + SIGN( 0.5, pvn(ji,jj,jk) )   ! if pun > 0 : zdir = 1 otherwise zdir = 0 
                  zdx = ( zdir * e2t(ji,jj) + ( 1. - zdir ) * e2t(ji,jj+1) ) * e1v(ji,jj) * fse3v(ji,jj,jk)
                  zwy(ji,jj,jk)  = ABS( pvn(ji,jj,jk) ) * zdt / zdx    ! (0<zc_cfl<1 : Courant number on x-direction)
                  zfc(ji,jj,jk)  = zdir * ptb(ji,jj  ,jk,jn) + ( 1. - zdir ) * ptb(ji,jj+1,jk,jn)  ! FC in the x-direction for T
                  zfd(ji,jj,jk)  = zdir * ptb(ji,jj+1,jk,jn) + ( 1. - zdir ) * ptb(ji,jj  ,jk,jn)  ! FD in the x-direction for T
               END DO
            END DO
         END DO

         !--- Lateral boundary conditions 
         CALL lbc_lnk( zfu(:,:,:), 'T', 1. )      ;     CALL lbc_lnk( zfd(:,:,:), 'T', 1. )
         CALL lbc_lnk( zfc(:,:,:), 'T', 1. )      ;     CALL lbc_lnk( zwy(:,:,:), 'T', 1. )

         !--- QUICKEST scheme
         CALL quickest( zfu, zfd, zfc, zwy )
         !
         ! Mask at the T-points in the x-direction (mask=0 or mask=1)
         DO jk = 1, jpkm1  
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.               
                  zfu(ji,jj,jk) = tmask(ji,jj-1,jk) + tmask(ji,jj,jk) + tmask(ji,jj+1,jk) - 2.
               END DO
            END DO
         END DO
         !--- Lateral boundary conditions 
         CALL lbc_lnk( zfu(:,:,:), 'T', 1. ) 
         !
         ! Tracer flux on the x-direction
         DO jk = 1, jpkm1  
            !
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.               
                  zdir = 0.5 + SIGN( 0.5, pvn(ji,jj,jk) )   ! if pun > 0 : zdir = 1 otherwise zdir = 0 
                  !--- If the second ustream point is a land point
                  !--- the flux is computed by the 1st order UPWIND scheme
                  zmsk = zdir * zfu(ji,jj,jk) + ( 1. - zdir ) * zfu(ji,jj+1,jk)
                  zwy(ji,jj,jk) = zmsk * zwy(ji,jj,jk) + ( 1. - zmsk ) * zfc(ji,jj,jk)
                  zwy(ji,jj,jk) = zwy(ji,jj,jk) * pvn(ji,jj,jk)
               END DO
            END DO
         END DO
         !
         CALL lbc_lnk( zwy(:,:,:), 'T', 1. ) ! Lateral boundary conditions
         !
         ! Computation of the trend
         DO jk = 1, jpkm1  
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.  
                  zbtr = 1. / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
                  ! horizontal advective trends
                  ztra = - zbtr * ( zwy(ji,jj,jk) - zwy(ji,jj-1,jk) )
                  !--- add it to the general tracer trends
                  pta(ji,jj,jk,jn) = pta(ji,jj,jk,jn) + ztra
               END DO
            END DO
         END DO
         !                                 ! trend diagnostics (contribution of upstream fluxes)
         IF( l_trd )  CALL trd_tra( kt, cdtype, jn, jptra_trd_yad, zwy, pvn, ptn(:,:,:,jn) )
         !                                 ! "Poleward" heat and salt transports (contribution of upstream fluxes)
         IF( cdtype == 'TRA' .AND. ln_diaptr .AND. ( MOD( kt, nn_fptr ) == 0 ) ) THEN  
           IF( jn == jp_tem )  htr_adv(:) = ptr_vj( zwy(:,:,:) )
           IF( jn == jp_sal )  str_adv(:) = ptr_vj( zwy(:,:,:) )
         ENDIF
         !
      END DO
      !
      CALL wrk_dealloc( jpi, jpj, jpk, zfu, zfc, zfd )
      !
   END SUBROUTINE tra_adv_qck_j


   SUBROUTINE tra_adv_cen2_k( kt, cdtype, pwn,           &
     &                                    ptn, pta, kjpt )
      !!----------------------------------------------------------------------
      !!
      !!----------------------------------------------------------------------
      USE oce, ONLY:   zwz => ua   ! ua used as workspace
      !
      INTEGER                              , INTENT(in   ) ::   kt       ! ocean time-step index
      CHARACTER(len=3)                     , INTENT(in   ) ::   cdtype   ! =TRA or TRC (tracer indicator)
      INTEGER                              , INTENT(in   ) ::   kjpt     ! number of tracers
      REAL(wp), DIMENSION(jpi,jpj,jpk     ), INTENT(in   ) ::   pwn      ! vertical velocity 
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(in   ) ::   ptn      ! before and now tracer fields
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(inout) ::   pta      ! tracer trend 
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) ::   zbtr , ztra      ! local scalars
      !!----------------------------------------------------------------------

      !                                                          ! ===========
      DO jn = 1, kjpt                                            ! tracer loop
         !                                                       ! ===========
         ! 1. Bottom value : flux set to zero
         zwz(:,:,jpk) = 0.e0             ! Bottom value : flux set to zero
         !
         !                                 ! Surface value
         IF( lk_vvl ) THEN   ;   zwz(:,:, 1 ) = 0.e0                      ! Variable volume : flux set to zero
         ELSE                ;   zwz(:,:, 1 ) = pwn(:,:,1) * ptn(:,:,1,jn)   ! Constant volume : advective flux through the surface
         ENDIF
         !
         DO jk = 2, jpkm1                  ! Interior point: second order centered tracer flux at w-point
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zwz(ji,jj,jk) = 0.5 * pwn(ji,jj,jk) * ( ptn(ji,jj,jk-1,jn) + ptn(ji,jj,jk,jn) )
               END DO
            END DO
         END DO
         !
         DO jk = 1, jpkm1          !==  Tracer flux divergence added to the general trend  ==!
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zbtr = 1. / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
                  ! k- vertical advective trends 
                  ztra = - zbtr * ( zwz(ji,jj,jk) - zwz(ji,jj,jk+1) ) 
                  ! added to the general tracer trends
                  pta(ji,jj,jk,jn) = pta(ji,jj,jk,jn) + ztra
               END DO
            END DO
         END DO
         !                                 ! Save the vertical advective trends for diagnostic
         IF( l_trd )  CALL trd_tra( kt, cdtype, jn, jptra_trd_zad, zwz, pwn, ptn(:,:,:,jn) )
         !
      END DO
      !
   END SUBROUTINE tra_adv_cen2_k


   SUBROUTINE quickest( pfu, pfd, pfc, puc )
      !!----------------------------------------------------------------------
      !!
      !! ** Purpose :  Computation of advective flux with Quickest scheme
      !!
      !! ** Method :   
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in   ) ::   pfu   ! second upwind point
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in   ) ::   pfd   ! first douwning point
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in   ) ::   pfc   ! the central point (or the first upwind point)
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   puc   ! input as Courant number ; output as flux
      !!
      INTEGER  ::  ji, jj, jk               ! dummy loop indices 
      REAL(wp) ::  zcoef1, zcoef2, zcoef3   ! local scalars          
      REAL(wp) ::  zc, zcurv, zfho          !   -      -
      !----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('quickest')
      !
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zc     = puc(ji,jj,jk)                         ! Courant number
               zcurv  = pfd(ji,jj,jk) + pfu(ji,jj,jk) - 2. * pfc(ji,jj,jk)
               zcoef1 = 0.5 *      ( pfc(ji,jj,jk) + pfd(ji,jj,jk) )
               zcoef2 = 0.5 * zc * ( pfd(ji,jj,jk) - pfc(ji,jj,jk) )
               zcoef3 = ( 1. - ( zc * zc ) ) * r1_6 * zcurv
               zfho   = zcoef1 - zcoef2 - zcoef3              !  phi_f QUICKEST 
               !
               zcoef1 = pfd(ji,jj,jk) - pfu(ji,jj,jk)
               zcoef2 = ABS( zcoef1 )
               zcoef3 = ABS( zcurv )
               IF( zcoef3 >= zcoef2 ) THEN
                  zfho = pfc(ji,jj,jk) 
               ELSE
                  zcoef3 = pfu(ji,jj,jk) + ( ( pfc(ji,jj,jk) - pfu(ji,jj,jk) ) / MAX( zc, 1.e-9 ) )    ! phi_REF
                  IF( zcoef1 >= 0. ) THEN
                     zfho = MAX( pfc(ji,jj,jk), zfho ) 
                     zfho = MIN( zfho, MIN( zcoef3, pfd(ji,jj,jk) ) ) 
                  ELSE
                     zfho = MIN( pfc(ji,jj,jk), zfho ) 
                     zfho = MAX( zfho, MAX( zcoef3, pfd(ji,jj,jk) ) ) 
                  ENDIF
               ENDIF
               puc(ji,jj,jk) = zfho
            END DO
         END DO
      END DO
      !
      IF( nn_timing == 1 )  CALL timing_stop('quickest')
      !
   END SUBROUTINE quickest

   !!======================================================================
END MODULE traadv_qck
