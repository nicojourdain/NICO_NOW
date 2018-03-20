MODULE zdfbfr
   !!======================================================================
   !!                       ***  MODULE  zdfbfr  ***
   !! Ocean physics: Bottom friction
   !!======================================================================
   !! History :  OPA  ! 1997-06  (G. Madec, A.-M. Treguier)  Original code
   !!   NEMO     1.0  ! 2002-06  (G. Madec)  F90: Free form and module
   !!            3.2  ! 2009-09  (A.C.Coward)  Correction to include barotropic contribution
   !!            3.3  ! 2010-10  (C. Ethe, G. Madec) reorganisation of initialisation phase
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   zdf_bfr      : update momentum Kz at the ocean bottom due to the type of bottom friction chosen
   !!   zdf_bfr_init : read in namelist and control the bottom friction parameters.
   !!   zdf_bfr_2d   : read in namelist and control the bottom friction parameters.
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables 
   USE zdf_oce         ! ocean vertical physics variables
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp         ! distributed memory computing
   USE prtctl          ! Print control
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_bfr         ! called by step.F90
   PUBLIC   zdf_bfr_init    ! called by opa.F90

   !                                    !!* Namelist nambfr: bottom friction namelist *
   INTEGER  ::   nn_bfr    = 0           ! = 0/1/2/3 type of bottom friction 
   REAL(wp) ::   rn_bfri1  = 4.0e-4_wp   ! bottom drag coefficient (linear case) 
   REAL(wp) ::   rn_bfri2  = 1.0e-3_wp   ! bottom drag coefficient (non linear case)
   REAL(wp) ::   rn_bfeb2  = 2.5e-3_wp   ! background bottom turbulent kinetic energy  [m2/s2]
   REAL(wp) ::   rn_bfrien = 30._wp      ! local factor to enhance coefficient bfri
   LOGICAL  ::   ln_bfr2d  = .false.     ! logical switch for 2D enhancement
   LOGICAL , PUBLIC                            ::  ln_bfrimp = .false.  ! logical switch for implicit bottom friction
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::  bfrcoef2d            ! 2D bottom drag coefficient

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: zdfbfr.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION zdf_bfr_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION zdf_bfr_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( bfrcoef2d(jpi,jpj), STAT=zdf_bfr_alloc )
      !
      IF( lk_mpp             )   CALL mpp_sum ( zdf_bfr_alloc )
      IF( zdf_bfr_alloc /= 0 )   CALL ctl_warn('zdf_bfr_alloc: failed to allocate arrays.')
   END FUNCTION zdf_bfr_alloc


   SUBROUTINE zdf_bfr( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE zdf_bfr  ***
      !!                 
      !! ** Purpose :   compute the bottom friction coefficient.
      !!
      !! ** Method  :   Calculate and store part of the momentum trend due    
      !!              to bottom friction following the chosen friction type 
      !!              (free-slip, linear, or quadratic). The component
      !!              calculated here is multiplied by the bottom velocity in
      !!              dyn_bfr to provide the trend term.
      !!                The coefficients are updated at each time step only
      !!              in the quadratic case.
      !!
      !! ** Action  :   bfrua , bfrva   bottom friction coefficients
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index
      !!
      INTEGER  ::   ji, jj       ! dummy loop indices
      INTEGER  ::   ikbu, ikbv   ! local integers
      REAL(wp) ::   zvu, zuv, zecu, zecv   ! temporary scalars
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zdf_bfr')
      !
      IF( nn_bfr == 2 ) THEN                 ! quadratic botton friction
         ! Calculate and store the quadratic bottom friction coefficient bfrua and bfrva
         ! where bfrUa = C_d*SQRT(u_bot^2 + v_bot^2 + e_b) {U=[u,v]}
         ! from these the trend due to bottom friction:  -F_h/e3U  can be calculated
         ! where -F_h/e3U_bot = bfrUa*Ub/e3U_bot {U=[u,v]}
         !
# if defined key_vectopt_loop
         DO jj = 1, 1
!CDIR NOVERRCHK
            DO ji = jpi+2, jpij-jpi-1   ! vector opt. (forced unrolling)
# else
!CDIR NOVERRCHK
         DO jj = 2, jpjm1
!CDIR NOVERRCHK
            DO ji = 2, jpim1
# endif
               ikbu = mbku(ji,jj)         ! ocean bottom level at u- and v-points 
               ikbv = mbkv(ji,jj)         ! (deepest ocean u- and v-points)
               !
               zvu  = 0.25 * (  vn(ji,jj  ,ikbu) + vn(ji+1,jj  ,ikbu)     &
                  &           + vn(ji,jj-1,ikbu) + vn(ji+1,jj-1,ikbu)  )
               zuv  = 0.25 * (  un(ji,jj  ,ikbv) + un(ji-1,jj  ,ikbv)     &
                  &           + un(ji,jj+1,ikbv) + un(ji-1,jj+1,ikbv)  )
               !
               zecu = SQRT(  un(ji,jj,ikbu) * un(ji,jj,ikbu) + zvu*zvu + rn_bfeb2  )
               zecv = SQRT(  vn(ji,jj,ikbv) * vn(ji,jj,ikbv) + zuv*zuv + rn_bfeb2  )
               !
               bfrua(ji,jj) = - 0.5_wp * ( bfrcoef2d(ji,jj) + bfrcoef2d(ji+1,jj  ) ) * zecu 
               bfrva(ji,jj) = - 0.5_wp * ( bfrcoef2d(ji,jj) + bfrcoef2d(ji  ,jj+1) ) * zecv
            END DO
         END DO
         !
         CALL lbc_lnk( bfrua, 'U', 1. )   ;   CALL lbc_lnk( bfrva, 'V', 1. )      ! Lateral boundary condition
         !
         IF(ln_ctl)   CALL prt_ctl( tab2d_1=bfrua, clinfo1=' bfr  - u: ', mask1=umask,        &
            &                       tab2d_2=bfrva, clinfo2=       ' v: ', mask2=vmask,ovlap=1 )
      ENDIF

      !
      IF( nn_timing == 1 )  CALL timing_stop('zdf_bfr')
      !
   END SUBROUTINE zdf_bfr


   SUBROUTINE zdf_bfr_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_bfr_init  ***
      !!                    
      !! ** Purpose :   Initialization of the bottom friction
      !!
      !! ** Method  :   Read the nammbf namelist and check their consistency
      !!              called at the first timestep (nit000)
      !!----------------------------------------------------------------------
      USE iom   ! I/O module for ehanced bottom friction file
      !!
      INTEGER ::   inum         ! logical unit for enhanced bottom friction file
      INTEGER ::   ji, jj       ! dummy loop indexes
      INTEGER ::   ikbu, ikbv   ! temporary integers
      INTEGER ::   ictu, ictv   !    -          -
      REAL(wp) ::  zminbfr, zmaxbfr   ! temporary scalars
      REAL(wp) ::  zfru, zfrv         !    -         -
      !!
      NAMELIST/nambfr/ nn_bfr, rn_bfri1, rn_bfri2, rn_bfeb2, ln_bfr2d, rn_bfrien, ln_bfrimp
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zdf_bfr_init')
      !
      REWIND ( numnam )               !* Read Namelist nam_bfr : bottom momentum boundary condition
      READ   ( numnam, nambfr )

      !                               !* Parameter control and print
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'zdf_bfr : momentum bottom friction'
      IF(lwp) WRITE(numout,*) '~~~~~~~'
      IF(lwp) WRITE(numout,*) '   Namelist nam_bfr : set bottom friction parameters'

      !                              ! allocate zdfbfr arrays
      IF( zdf_bfr_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'zdf_bfr_init : unable to allocate arrays' )

      !                              ! Make sure ln_zdfexp=.false. when use implicit bfr
      IF( ln_bfrimp .AND. ln_zdfexp ) THEN
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'Implicit bottom friction can only be used when ln_zdfexp=.false.'
            WRITE(numout,*) '         but you set: ln_bfrimp=.true. and ln_zdfexp=.true.!!!!'
            WRITE(ctmp1,*)  '         set either ln_zdfexp = .false or ln_bfrimp = .false.'
            CALL ctl_stop( ctmp1 )
         END IF
      END IF

      SELECT CASE (nn_bfr)
      !
      CASE( 0 )
         IF(lwp) WRITE(numout,*) '      free-slip '
         bfrua(:,:) = 0.e0
         bfrva(:,:) = 0.e0
         !
      CASE( 1 )
         IF(lwp) WRITE(numout,*) '      linear botton friction'
         IF(lwp) WRITE(numout,*) '      friction coef.   rn_bfri1  = ', rn_bfri1
         IF( ln_bfr2d ) THEN
            IF(lwp) WRITE(numout,*) '      read coef. enhancement distribution from file   ln_bfr2d  = ', ln_bfr2d
            IF(lwp) WRITE(numout,*) '      coef rn_bfri2 enhancement factor                rn_bfrien  = ',rn_bfrien
         ENDIF
         !
         bfrcoef2d(:,:) = rn_bfri1  ! initialize bfrcoef2d to the namelist variable
         !
         IF(ln_bfr2d) THEN 
            ! bfr_coef is a coefficient in [0,1] giving the mask where to apply the bfr enhancement
            CALL iom_open('bfr_coef.nc',inum)
            CALL iom_get (inum, jpdom_data, 'bfr_coef',bfrcoef2d,1) ! bfrcoef2d is used as tmp array
            CALL iom_close(inum)
            bfrcoef2d(:,:) = rn_bfri1 * ( 1 + rn_bfrien * bfrcoef2d(:,:) )
         ENDIF
         bfrua(:,:) = - bfrcoef2d(:,:)
         bfrva(:,:) = - bfrcoef2d(:,:)
         !
      CASE( 2 )
         IF(lwp) WRITE(numout,*) '      quadratic botton friction'
         IF(lwp) WRITE(numout,*) '      friction coef.   rn_bfri2  = ', rn_bfri2
         IF(lwp) WRITE(numout,*) '      background tke   rn_bfeb2  = ', rn_bfeb2
         IF( ln_bfr2d ) THEN
            IF(lwp) WRITE(numout,*) '      read coef. enhancement distribution from file   ln_bfr2d  = ', ln_bfr2d
            IF(lwp) WRITE(numout,*) '      coef rn_bfri2 enhancement factor                rn_bfrien  = ',rn_bfrien
         ENDIF
         bfrcoef2d(:,:) = rn_bfri2  ! initialize bfrcoef2d to the namelist variable
         !
         IF(ln_bfr2d) THEN 
            ! bfr_coef is a coefficient in [0,1] giving the mask where to apply the bfr enhancement
            CALL iom_open('bfr_coef.nc',inum)
            CALL iom_get (inum, jpdom_data, 'bfr_coef',bfrcoef2d,1) ! bfrcoef2d is used as tmp array
            CALL iom_close(inum)
            bfrcoef2d(:,:)= rn_bfri2 * ( 1 + rn_bfrien * bfrcoef2d(:,:) )
         ENDIF
         !
      CASE DEFAULT
         IF(lwp) WRITE(ctmp1,*) '         bad flag value for nn_bfr = ', nn_bfr
         CALL ctl_stop( ctmp1 )
         !
      END SELECT
      IF(lwp) WRITE(numout,*) '      implicit bottom friction switch                ln_bfrimp  = ', ln_bfrimp
      !
      ! Basic stability check on bottom friction coefficient
      !
      ictu = 0               ! counter for stability criterion breaches at U-pts
      ictv = 0               ! counter for stability criterion breaches at V-pts
      zminbfr =  1.e10_wp    ! initialise tracker for minimum of bottom friction coefficient
      zmaxbfr = -1.e10_wp    ! initialise tracker for maximum of bottom friction coefficient
      !
#  if defined key_vectopt_loop
      DO jj = 1, 1
!CDIR NOVERRCHK
         DO ji = jpi+2, jpij-jpi-1   ! vector opt. (forced unrolling)
#  else
!CDIR NOVERRCHK
      DO jj = 2, jpjm1
!CDIR NOVERRCHK
         DO ji = 2, jpim1
#  endif
             ikbu = mbku(ji,jj)       ! deepest ocean level at u- and v-points
             ikbv = mbkv(ji,jj)
             zfru = 0.5 * fse3u(ji,jj,ikbu) / rdt
             zfrv = 0.5 * fse3v(ji,jj,ikbv) / rdt
             IF( ABS( bfrcoef2d(ji,jj) ) > zfru ) THEN
                IF( ln_ctl ) THEN
                   WRITE(numout,*) 'BFR ', narea, nimpp+ji, njmpp+jj, ikbu
                   WRITE(numout,*) 'BFR ', ABS( bfrcoef2d(ji,jj) ), zfru
                ENDIF
                ictu = ictu + 1
             ENDIF
             IF( ABS( bfrcoef2d(ji,jj) ) > zfrv ) THEN
                 IF( ln_ctl ) THEN
                     WRITE(numout,*) 'BFR ', narea, nimpp+ji, njmpp+jj, ikbv
                     WRITE(numout,*) 'BFR ', bfrcoef2d(ji,jj), zfrv
                 ENDIF
                 ictv = ictv + 1
             ENDIF
             zminbfr = MIN(  zminbfr, MIN( zfru, ABS( bfrcoef2d(ji,jj) ) )  )
             zmaxbfr = MAX(  zmaxbfr, MIN( zfrv, ABS( bfrcoef2d(ji,jj) ) )  )
         END DO
      END DO
      IF( lk_mpp ) THEN
         CALL mpp_sum( ictu )
         CALL mpp_sum( ictv )
         CALL mpp_min( zminbfr )
         CALL mpp_max( zmaxbfr )
      ENDIF
      IF( .NOT.ln_bfrimp) THEN
      IF( lwp .AND. ictu + ictv > 0 ) THEN
         WRITE(numout,*) ' Bottom friction stability check failed at ', ictu, ' U-points '
         WRITE(numout,*) ' Bottom friction stability check failed at ', ictv, ' V-points '
         WRITE(numout,*) ' Bottom friction coefficient now ranges from: ', zminbfr, ' to ', zmaxbfr
         WRITE(numout,*) ' Bottom friction coefficient will be reduced where necessary'
      ENDIF
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('zdf_bfr_init')
      !
   END SUBROUTINE zdf_bfr_init

   !!======================================================================
END MODULE zdfbfr
