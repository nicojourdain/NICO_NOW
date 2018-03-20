MODULE zdfric
   !!======================================================================
   !!                       ***  MODULE  zdfric  ***
   !! Ocean physics:  vertical mixing coefficient compute from the local
   !!                 Richardson number dependent formulation
   !!======================================================================
   !! History :  OPA  ! 1987-09  (P. Andrich)  Original code
   !!            4.0  ! 1991-11  (G. Madec)
   !!            7.0  ! 1996-01  (G. Madec)  complete rewriting of multitasking suppression of common work arrays
   !!            8.0  ! 1997-06  (G. Madec)  complete rewriting of zdfmix
   !!   NEMO     1.0  ! 2002-06  (G. Madec)  F90: Free form and module
   !!            3.3  ! 2010-10  (C. Ethe, G. Madec) reorganisation of initialisation phase
   !!            3.3.1! 2011-09  (P. Oddo) Mixed layer depth parameterization
   !!----------------------------------------------------------------------
#if defined key_zdfric   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_zdfric'                                             Kz = f(Ri)
   !!----------------------------------------------------------------------
   !!   zdf_ric      : update momentum and tracer Kz from the Richardson
   !!                  number computation
   !!   zdf_ric_init : initialization, namelist read, & parameters control
   !!----------------------------------------------------------------------
   USE oce                   ! ocean dynamics and tracers variables
   USE dom_oce               ! ocean space and time domain variables
   USE zdf_oce               ! ocean vertical physics
   USE in_out_manager        ! I/O manager
   USE lbclnk                ! ocean lateral boundary condition (or mpp link)
   USE lib_mpp               ! MPP library
   USE wrk_nemo              ! work arrays
   USE timing                ! Timing

   USE eosbn2, ONLY : nn_eos

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_ric         ! called by step.F90
   PUBLIC   zdf_ric_init    ! called by opa.F90

   LOGICAL, PUBLIC, PARAMETER ::   lk_zdfric = .TRUE.   !: Richardson vertical mixing flag

   !                                    !!* Namelist namzdf_ric : Richardson number dependent Kz *
   INTEGER  ::   nn_ric   = 2            ! coefficient of the parameterization
   REAL(wp) ::   rn_avmri = 100.e-4_wp   ! maximum value of the vertical eddy viscosity
   REAL(wp) ::   rn_alp   =   5._wp      ! coefficient of the parameterization
   REAL(wp) ::   rn_ekmfc =   0.7_wp     ! Ekman Factor Coeff
   REAL(wp) ::   rn_mldmin=   1.0_wp     ! minimum mixed layer (ML) depth    
   REAL(wp) ::   rn_mldmax=1000.0_wp     ! maximum mixed layer depth
   REAL(wp) ::   rn_wtmix =  10.0_wp     ! Vertical eddy Diff. in the ML
   REAL(wp) ::   rn_wvmix =  10.0_wp     ! Vertical eddy Visc. in the ML
   LOGICAL  ::   ln_mldw  = .TRUE.       ! Use or not the MLD parameters

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   tmric   !: coef. for the horizontal mean at t-point

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: zdfric.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION zdf_ric_alloc()
      !!----------------------------------------------------------------------
      !!                 ***  FUNCTION zdf_ric_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( tmric(jpi,jpj,jpk)   , STAT= zdf_ric_alloc )
      !
      IF( lk_mpp             )   CALL mpp_sum ( zdf_ric_alloc )
      IF( zdf_ric_alloc /= 0 )   CALL ctl_warn('zdf_ric_alloc: failed to allocate arrays')
   END FUNCTION zdf_ric_alloc


   SUBROUTINE zdf_ric( kt )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE zdfric  ***
      !!                    
      !! ** Purpose :   Compute the before eddy viscosity and diffusivity as
      !!                a function of the local richardson number.
      !!
      !! ** Method  :   Local richardson number dependent formulation of the 
      !!                vertical eddy viscosity and diffusivity coefficients. 
      !!                The eddy coefficients are given by:
      !!                    avm = avm0 + avmb
      !!                    avt = avm0 / (1 + rn_alp*ri)
      !!                with ri  = N^2 / dz(u)**2
      !!                         = e3w**2 * rn2/[ mi( dk(ub) )+mj( dk(vb) ) ]
      !!                    avm0= rn_avmri / (1 + rn_alp*ri)**nn_ric
      !!      Where ri is the before local Richardson number,
      !!            rn_avmri is the maximum value reaches by avm and avt 
      !!            avmb and avtb are the background (or minimum) values
      !!            and rn_alp, nn_ric are adjustable parameters.
      !!      Typical values used are : avm0=1.e-2 m2/s, avmb=1.e-6 m2/s
      !!      avtb=1.e-7 m2/s, rn_alp=5. and nn_ric=2.
      !!      a numerical threshold is impose on the vertical shear (1.e-20)
      !!      As second step compute Ekman depth from wind stress forcing
      !!      and apply namelist provided vertical coeff within this depth.
      !!      The Ekman depth is:
      !!              Ustar = SQRT(Taum/rho0)
      !!              ekd= rn_ekmfc * Ustar / f0
      !!      Large et al. (1994, eq.29) suggest rn_ekmfc=0.7; however, the derivation
      !!      of the above equation indicates the value is somewhat arbitrary; therefore
      !!      we allow the freedom to increase or decrease this value, if the
      !!      Ekman depth estimate appears too shallow or too deep, respectively.
      !!      Ekd is then limited by rn_mldmin and rn_mldmax provided in the
      !!      namelist
      !!        N.B. the mask are required for implicit scheme, and surface
      !!      and bottom value already set in zdfini.F90
      !!
      !! References : Pacanowski & Philander 1981, JPO, 1441-1451.
      !!              PFJ Lermusiaux 2001.
      !!----------------------------------------------------------------------
      USE phycst,   ONLY:   rsmall,rau0
      USE sbc_oce,  ONLY:   taum
      !!
      INTEGER, INTENT( in ) ::   kt                           ! ocean time-step
      !!
      INTEGER  ::   ji, jj, jk                                ! dummy loop indices
      REAL(wp) ::   zcoef, zdku, zdkv, zri, z05alp, zflageos  ! temporary scalars
      REAL(wp) ::   zrhos, zustar
      REAL(wp), POINTER, DIMENSION(:,:) ::   zwx, ekm_dep  
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zdf_ric')
      !
      CALL wrk_alloc( jpi,jpj, zwx, ekm_dep )
      !                                                ! ===============
      DO jk = 2, jpkm1                                 ! Horizontal slab
         !                                             ! ===============
         ! Richardson number (put in zwx(ji,jj))
         ! -----------------
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               zcoef = 0.5 / fse3w(ji,jj,jk)
               !                                            ! shear of horizontal velocity
               zdku = zcoef * (  ub(ji-1,jj,jk-1) + ub(ji,jj,jk-1)   &
                  &             -ub(ji-1,jj,jk  ) - ub(ji,jj,jk  )  )
               zdkv = zcoef * (  vb(ji,jj-1,jk-1) + vb(ji,jj,jk-1)   &
                  &             -vb(ji,jj-1,jk  ) - vb(ji,jj,jk  )  )
               !                                            ! richardson number (minimum value set to zero)
               zri = rn2(ji,jj,jk) / ( zdku*zdku + zdkv*zdkv + 1.e-20 )
               zwx(ji,jj) = MAX( zri, 0.e0 )
            END DO
         END DO
         CALL lbc_lnk( zwx, 'W', 1. )                       ! Boundary condition   (sign unchanged)

         ! Vertical eddy viscosity and diffusivity coefficients
         ! -------------------------------------------------------
         z05alp = 0.5_wp * rn_alp
         DO jj = 1, jpjm1                                   ! Eddy viscosity coefficients (avm)
            DO ji = 1, jpim1
               avmu(ji,jj,jk) = umask(ji,jj,jk) * rn_avmri / ( 1. + z05alp*( zwx(ji+1,jj)+zwx(ji,jj) ) )**nn_ric
               avmv(ji,jj,jk) = vmask(ji,jj,jk) * rn_avmri / ( 1. + z05alp*( zwx(ji,jj+1)+zwx(ji,jj) ) )**nn_ric
            END DO
         END DO
         DO jj = 2, jpjm1                                   ! Eddy diffusivity coefficients (avt)
            DO ji = 2, jpim1
               avt(ji,jj,jk) = tmric(ji,jj,jk) / ( 1._wp + rn_alp * zwx(ji,jj) )           &
                  &                            * (  avmu(ji,jj,jk) + avmu(ji-1,jj,jk)      &
                  &                               + avmv(ji,jj,jk) + avmv(ji,jj-1,jk)  )   &
                  &          + avtb(jk) * tmask(ji,jj,jk)
               !                                            ! Add the background coefficient on eddy viscosity
               avmu(ji,jj,jk) = avmu(ji,jj,jk) + avmb(jk) * umask(ji,jj,jk)
               avmv(ji,jj,jk) = avmv(ji,jj,jk) + avmb(jk) * vmask(ji,jj,jk)
            END DO
         END DO
         !                                             ! ===============
      END DO                                           !   End of slab
      !                                                ! ===============
      !
      IF( ln_mldw ) THEN

      !  Compute Ekman depth from wind stress forcing.
      ! -------------------------------------------------------
      zflageos = ( 0.5 + SIGN( 0.5, nn_eos - 1. ) ) * rau0
      DO jj = 1, jpj
         DO ji = 1, jpi
            zrhos          = rhop(ji,jj,1) + zflageos * ( 1. - tmask(ji,jj,1) )
            zustar         = SQRT( taum(ji,jj) / ( zrhos +  rsmall ) )
            ekm_dep(ji,jj) = rn_ekmfc * zustar / ( ABS( ff(ji,jj) ) + rsmall )
            ekm_dep(ji,jj) = MAX(ekm_dep(ji,jj),rn_mldmin) ! Minimun allowed
            ekm_dep(ji,jj) = MIN(ekm_dep(ji,jj),rn_mldmax) ! Maximum allowed
         END DO
      END DO

      ! In the first model level vertical diff/visc coeff.s 
      ! are always equal to the namelist values rn_wtmix/rn_wvmix
      ! -------------------------------------------------------
      DO jj = 1, jpj
         DO ji = 1, jpi
            avmv(ji,jj,1) = MAX( avmv(ji,jj,1), rn_wvmix )
            avmu(ji,jj,1) = MAX( avmu(ji,jj,1), rn_wvmix )
            avt( ji,jj,1) = MAX(  avt(ji,jj,1), rn_wtmix )
         END DO
      END DO

      !  Force the vertical mixing coef within the Ekman depth
      ! -------------------------------------------------------
      DO jk = 2, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( fsdept(ji,jj,jk) < ekm_dep(ji,jj) ) THEN
                  avmv(ji,jj,jk) = MAX( avmv(ji,jj,jk), rn_wvmix )
                  avmu(ji,jj,jk) = MAX( avmu(ji,jj,jk), rn_wvmix )
                  avt( ji,jj,jk) = MAX(  avt(ji,jj,jk), rn_wtmix )
               ENDIF
            END DO
         END DO
      END DO

      DO jk = 1, jpkm1                
         DO jj = 1, jpj
            DO ji = 1, jpi
               avmv(ji,jj,jk) = avmv(ji,jj,jk) * vmask(ji,jj,jk)
               avmu(ji,jj,jk) = avmu(ji,jj,jk) * umask(ji,jj,jk)
               avt( ji,jj,jk) = avt( ji,jj,jk) * tmask(ji,jj,jk)
            END DO
         END DO
      END DO

     ENDIF

      CALL lbc_lnk( avt , 'W', 1. )                         ! Boundary conditions   (unchanged sign)
      CALL lbc_lnk( avmu, 'U', 1. )   ;   CALL lbc_lnk( avmv, 'V', 1. )
      !
      CALL wrk_dealloc( jpi,jpj, zwx, ekm_dep )
      !
      IF( nn_timing == 1 )  CALL timing_stop('zdf_ric')
      !
   END SUBROUTINE zdf_ric


   SUBROUTINE zdf_ric_init
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE zdfbfr_init  ***
      !!                    
      !! ** Purpose :   Initialization of the vertical eddy diffusivity and
      !!      viscosity coef. for the Richardson number dependent formulation.
      !!
      !! ** Method  :   Read the namzdf_ric namelist and check the parameter values
      !!
      !! ** input   :   Namelist namzdf_ric
      !!
      !! ** Action  :   increase by 1 the nstop flag is setting problem encounter
      !!----------------------------------------------------------------------
      INTEGER :: ji, jj, jk   ! dummy loop indices
      !!
      NAMELIST/namzdf_ric/ rn_avmri, rn_alp   , nn_ric  , rn_ekmfc,  &
         &                rn_mldmin, rn_mldmax, rn_wtmix, rn_wvmix, ln_mldw
      !!----------------------------------------------------------------------
      !
      REWIND( numnam )               ! Read Namelist namzdf_ric : richardson number dependent Kz
      READ  ( numnam, namzdf_ric )
      !
      IF(lwp) THEN                   ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'zdf_ric : Ri depend vertical mixing scheme'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*) '   Namelist namzdf_ric : set Kz(Ri) parameters'
         WRITE(numout,*) '      maximum vertical viscosity     rn_avmri  = ', rn_avmri
         WRITE(numout,*) '      coefficient                    rn_alp    = ', rn_alp
         WRITE(numout,*) '      coefficient                    nn_ric    = ', nn_ric
         WRITE(numout,*) '      Ekman Factor Coeff             rn_ekmfc  = ', rn_ekmfc
         WRITE(numout,*) '      minimum mixed layer depth      rn_mldmin = ', rn_mldmin
         WRITE(numout,*) '      maximum mixed layer depth      rn_mldmax = ', rn_mldmax
         WRITE(numout,*) '      Vertical eddy Diff. in the ML  rn_wtmix  = ', rn_wtmix
         WRITE(numout,*) '      Vertical eddy Visc. in the ML  rn_wvmix  = ', rn_wvmix
         WRITE(numout,*) '      Use the MLD parameterization   ln_mldw   = ', ln_mldw
      ENDIF
      !
      !                              ! allocate zdfric arrays
      IF( zdf_ric_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'zdf_ric_init : unable to allocate arrays' )
      !
      DO jk = 1, jpk                 ! weighting mean array tmric for 4 T-points
         DO jj = 2, jpj              ! which accounts for coastal boundary conditions            
            DO ji = 2, jpi
               tmric(ji,jj,jk) =  tmask(ji,jj,jk)                                  &
                  &            / MAX( 1.,  umask(ji-1,jj  ,jk) + umask(ji,jj,jk)   &
                  &                      + vmask(ji  ,jj-1,jk) + vmask(ji,jj,jk)  )
            END DO
         END DO
      END DO
      tmric(:,1,:) = 0._wp
      !
      DO jk = 1, jpk                 ! Initialization of vertical eddy coef. to the background value
         avt (:,:,jk) = avtb(jk) * tmask(:,:,jk)
         avmu(:,:,jk) = avmb(jk) * umask(:,:,jk)
         avmv(:,:,jk) = avmb(jk) * vmask(:,:,jk)
      END DO
      !
   END SUBROUTINE zdf_ric_init

#else
   !!----------------------------------------------------------------------
   !!   Dummy module :              NO Richardson dependent vertical mixing
   !!----------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_zdfric = .FALSE.   !: Richardson mixing flag
CONTAINS
   SUBROUTINE zdf_ric_init         ! Dummy routine
   END SUBROUTINE zdf_ric_init
   SUBROUTINE zdf_ric( kt )        ! Dummy routine
      WRITE(*,*) 'zdf_ric: You should not have seen this print! error?', kt
   END SUBROUTINE zdf_ric
#endif

   !!======================================================================
END MODULE zdfric
