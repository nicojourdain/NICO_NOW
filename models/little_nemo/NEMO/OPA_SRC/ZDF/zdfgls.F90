MODULE zdfgls
   !!======================================================================
   !!                       ***  MODULE  zdfgls  ***
   !! Ocean physics:  vertical mixing coefficient computed from the gls 
   !!                 turbulent closure parameterization
   !!======================================================================
   !! History :   3.0  !  2009-09  (G. Reffray)  Original code
   !!             3.3  !  2010-10  (C. Bricaud)  Add in the reference
   !!----------------------------------------------------------------------
#if defined key_zdfgls   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_zdfgls'                 Generic Length Scale vertical physics
   !!----------------------------------------------------------------------
   !!   zdf_gls      : update momentum and tracer Kz from a gls scheme
   !!   zdf_gls_init : initialization, namelist read, and parameters control
   !!   gls_rst      : read/write gls restart in ocean restart file
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers 
   USE dom_oce        ! ocean space and time domain
   USE domvvl         ! ocean space and time domain : variable volume layer
   USE zdf_oce        ! ocean vertical physics
   USE sbc_oce        ! surface boundary condition: ocean
   USE phycst         ! physical constants
   USE zdfmxl         ! mixed layer
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! MPP manager
   USE wrk_nemo       ! work arrays
   USE prtctl         ! Print control
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_gls        ! routine called in step module
   PUBLIC   zdf_gls_init   ! routine called in opa module
   PUBLIC   gls_rst        ! routine called in step module

   LOGICAL , PUBLIC, PARAMETER ::   lk_zdfgls = .TRUE.   !: TKE vertical mixing flag
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   en      !: now turbulent kinetic energy
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   mxln    !: now mixing length
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   zwall   !: wall function
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   avt_k   ! not enhanced Kz
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   avm_k   ! not enhanced Kz
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   avmu_k  ! not enhanced Kz
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   avmv_k  ! not enhanced Kz
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   ustars2 !: Squared surface velocity scale at T-points
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   ustarb2 !: Squared bottom  velocity scale at T-points

   !                                         !!! ** Namelist  namzdf_gls  **
   LOGICAL  ::   ln_crban      = .FALSE.      ! =T use Craig and Banner scheme
   LOGICAL  ::   ln_length_lim = .FALSE.      ! use limit on the dissipation rate under stable stratification (Galperin et al. 1988)
   LOGICAL  ::   ln_sigpsi     = .FALSE.      ! Activate Burchard (2003) modification for k-eps closure & wave breaking mixing
   INTEGER  ::   nn_tkebc_surf = 0            ! TKE surface boundary condition (=0/1)
   INTEGER  ::   nn_tkebc_bot  = 0            ! TKE bottom boundary condition (=0/1)
   INTEGER  ::   nn_psibc_surf = 0            ! PSI surface boundary condition (=0/1)
   INTEGER  ::   nn_psibc_bot  = 0            ! PSI bottom boundary condition (=0/1)
   INTEGER  ::   nn_stab_func  = 0            ! stability functions G88, KC or Canuto (=0/1/2)
   INTEGER  ::   nn_clos       = 0            ! closure 0/1/2/3 MY82/k-eps/k-w/gen
   REAL(wp) ::   rn_clim_galp  = 0.53_wp      ! Holt 2008 value for k-eps: 0.267
   REAL(wp) ::   rn_epsmin     = 1.e-12_wp    ! minimum value of dissipation (m2/s3)
   REAL(wp) ::   rn_emin       = 1.e-6_wp     ! minimum value of TKE (m2/s2)
   REAL(wp) ::   rn_charn      = 2.e+5_wp     ! Charnock constant for surface breaking waves mixing : 1400. (standard) or 2.e5 (Stacey value)
   REAL(wp) ::   rn_crban      = 100._wp      ! Craig and Banner constant for surface breaking waves mixing

   REAL(wp) ::   hsro          =  0.003_wp    ! Minimum surface roughness
   REAL(wp) ::   hbro          =  0.003_wp    ! Bottom roughness (m)
   REAL(wp) ::   rcm_sf        =  0.73_wp     ! Shear free turbulence parameters
   REAL(wp) ::   ra_sf         = -2.0_wp      ! Must be negative -2 < ra_sf < -1 
   REAL(wp) ::   rl_sf         =  0.2_wp      ! 0 <rl_sf<vkarmn    
   REAL(wp) ::   rghmin        = -0.28_wp
   REAL(wp) ::   rgh0          =  0.0329_wp
   REAL(wp) ::   rghcri        =  0.03_wp
   REAL(wp) ::   ra1           =  0.92_wp
   REAL(wp) ::   ra2           =  0.74_wp
   REAL(wp) ::   rb1           = 16.60_wp
   REAL(wp) ::   rb2           = 10.10_wp         
   REAL(wp) ::   re2           =  1.33_wp         
   REAL(wp) ::   rl1           =  0.107_wp
   REAL(wp) ::   rl2           =  0.0032_wp
   REAL(wp) ::   rl3           =  0.0864_wp
   REAL(wp) ::   rl4           =  0.12_wp
   REAL(wp) ::   rl5           = 11.9_wp
   REAL(wp) ::   rl6           =  0.4_wp
   REAL(wp) ::   rl7           =  0.0_wp
   REAL(wp) ::   rl8           =  0.48_wp
   REAL(wp) ::   rm1           =  0.127_wp
   REAL(wp) ::   rm2           =  0.00336_wp
   REAL(wp) ::   rm3           =  0.0906_wp
   REAL(wp) ::   rm4           =  0.101_wp
   REAL(wp) ::   rm5           = 11.2_wp
   REAL(wp) ::   rm6           =  0.4_wp
   REAL(wp) ::   rm7           =  0.0_wp
   REAL(wp) ::   rm8           =  0.318_wp
   
   REAL(wp) ::   rc02, rc02r, rc03, rc04                          ! coefficients deduced from above parameters
   REAL(wp) ::   rc03_sqrt2_galp                                  !     -           -           -        -
   REAL(wp) ::   rsbc_tke1, rsbc_tke2, rsbc_tke3, rfact_tke       !     -           -           -        -
   REAL(wp) ::   rsbc_psi1, rsbc_psi2, rsbc_psi3, rfact_psi       !     -           -           -        -
   REAL(wp) ::   rsbc_mb  , rsbc_std , rsbc_zs                    !     -           -           -        -
   REAL(wp) ::   rc0, rc2, rc3, rf6, rcff, rc_diff                !     -           -           -        -
   REAL(wp) ::   rs0, rs1, rs2, rs4, rs5, rs6                     !     -           -           -        -
   REAL(wp) ::   rd0, rd1, rd2, rd3, rd4, rd5                     !     -           -           -        -
   REAL(wp) ::   rsc_tke, rsc_psi, rpsi1, rpsi2, rpsi3, rsc_psi0  !     -           -           -        -
   REAL(wp) ::   rpsi3m, rpsi3p, rpp, rmm, rnn                    !     -           -           -        -

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: zdfgls.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION zdf_gls_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION zdf_gls_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( en(jpi,jpj,jpk),  mxln(jpi,jpj,jpk), zwall(jpi,jpj,jpk) ,     &
         &      avt_k (jpi,jpj,jpk) , avm_k (jpi,jpj,jpk),                    &
         &      avmu_k(jpi,jpj,jpk) , avmv_k(jpi,jpj,jpk),                    &
         &      ustars2(jpi,jpj), ustarb2(jpi,jpj)                      , STAT= zdf_gls_alloc )
         !
      IF( lk_mpp             )   CALL mpp_sum ( zdf_gls_alloc )
      IF( zdf_gls_alloc /= 0 )   CALL ctl_warn('zdf_gls_alloc: failed to allocate arrays')
   END FUNCTION zdf_gls_alloc


   SUBROUTINE zdf_gls( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE zdf_gls  ***
      !!
      !! ** Purpose :   Compute the vertical eddy viscosity and diffusivity
      !!              coefficients using the GLS turbulent closure scheme.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt ! ocean time step
      INTEGER  ::   ji, jj, jk, ibot, ibotm1, dir  ! dummy loop arguments
      REAL(wp) ::   zesh2, zsigpsi, zcoef, zex1, zex2   ! local scalars
      REAL(wp) ::   ztx2, zty2, zup, zdown, zcof        !   -      - 
      REAL(wp) ::   zratio, zrn2, zflxb, sh             !   -      -
      REAL(wp) ::   prod, buoy, diss, zdiss, sm         !   -      -
      REAL(wp) ::   gh, gm, shr, dif, zsqen, zav        !   -      -
      REAL(wp), POINTER, DIMENSION(:,:  ) ::   zdep
      REAL(wp), POINTER, DIMENSION(:,:  ) ::   zflxs       ! Turbulence fluxed induced by internal waves 
      REAL(wp), POINTER, DIMENSION(:,:  ) ::   zhsro       ! Surface roughness (surface waves)
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   eb          ! tke at time before
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   mxlb        ! mixing length at time before
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   shear       ! vertical shear
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   eps         ! dissipation rate
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zwall_psi   ! Wall function use in the wb case (ln_sigpsi.AND.ln_crban=T)
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   z_elem_a, z_elem_b, z_elem_c, psi
      !!--------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zdf_gls')
      !
      CALL wrk_alloc( jpi,jpj, zdep, zflxs, zhsro )
      CALL wrk_alloc( jpi,jpj,jpk, eb, mxlb, shear, eps, zwall_psi, z_elem_a, z_elem_b, z_elem_c, psi )

      ! Preliminary computing

      ustars2 = 0._wp   ;   ustarb2 = 0._wp   ;   psi  = 0._wp   ;   zwall_psi = 0._wp

      IF( kt /= nit000 ) THEN   ! restore before value to compute tke
         avt (:,:,:) = avt_k (:,:,:)
         avm (:,:,:) = avm_k (:,:,:)
         avmu(:,:,:) = avmu_k(:,:,:)
         avmv(:,:,:) = avmv_k(:,:,:) 
      ENDIF

      ! Compute surface and bottom friction at T-points
!CDIR NOVERRCHK
      DO jj = 2, jpjm1
!CDIR NOVERRCHK
         DO ji = fs_2, fs_jpim1   ! vector opt.
            ! 
            ! surface friction 
            ustars2(ji,jj) = rau0r * taum(ji,jj) * tmask(ji,jj,1)
            !
            ! bottom friction (explicit before friction)
            ! Note that we chose here not to bound the friction as in dynbfr)
            ztx2 = (  bfrua(ji,jj)  * ub(ji,jj,mbku(ji,jj)) + bfrua(ji-1,jj) * ub(ji-1,jj,mbku(ji-1,jj))  )   &
               & * ( 1._wp - 0.5_wp * umask(ji,jj,1) * umask(ji-1,jj,1)  )
            zty2 = (  bfrva(ji,jj)  * vb(ji,jj,mbkv(ji,jj)) + bfrva(ji,jj-1) * vb(ji,jj-1,mbkv(ji,jj-1))  )   &
               & * ( 1._wp - 0.5_wp * vmask(ji,jj,1) * vmask(ji,jj-1,1)  )
            ustarb2(ji,jj) = SQRT( ztx2 * ztx2 + zty2 * zty2 ) * tmask(ji,jj,1)
         END DO
      END DO  

      ! In case of breaking surface waves mixing,
      ! Compute surface roughness length according to Charnock formula:
      IF( ln_crban ) THEN   ;   zhsro(:,:) = MAX(rsbc_zs * ustars2(:,:), hsro)
      ELSE                  ;   zhsro(:,:) = hsro
      ENDIF

      ! Compute shear and dissipation rate
      DO jk = 2, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               avmu(ji,jj,jk) = avmu(ji,jj,jk) * ( un(ji,jj,jk-1) - un(ji,jj,jk) )   &
                  &                            * ( ub(ji,jj,jk-1) - ub(ji,jj,jk) )   &
                  &                            / (  fse3uw_n(ji,jj,jk)               &
                  &                            *    fse3uw_b(ji,jj,jk) )
               avmv(ji,jj,jk) = avmv(ji,jj,jk) * ( vn(ji,jj,jk-1) - vn(ji,jj,jk) )   &
                  &                            * ( vb(ji,jj,jk-1) - vb(ji,jj,jk) )   &
                  &                            / (  fse3vw_n(ji,jj,jk)               &
                  &                            *    fse3vw_b(ji,jj,jk) )
               eps(ji,jj,jk)  = rc03 * en(ji,jj,jk) * SQRT(en(ji,jj,jk)) / mxln(ji,jj,jk)
            END DO
         END DO
      END DO
      !
      ! Lateral boundary conditions (avmu,avmv) (sign unchanged)
      CALL lbc_lnk( avmu, 'U', 1. )   ;   CALL lbc_lnk( avmv, 'V', 1. )

      ! Save tke at before time step
      eb  (:,:,:) = en  (:,:,:)
      mxlb(:,:,:) = mxln(:,:,:)

      IF( nn_clos == 0 ) THEN    ! Mellor-Yamada
         DO jk = 2, jpkm1
            DO jj = 2, jpjm1 
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zup   = mxln(ji,jj,jk) * fsdepw(ji,jj,mbkt(ji,jj)+1)
                  zdown = vkarmn * fsdepw(ji,jj,jk) * ( -fsdepw(ji,jj,jk) + fsdepw(ji,jj,mbkt(ji,jj)+1) )
                  zcoef = ( zup / MAX( zdown, rsmall ) )
                  zwall (ji,jj,jk) = ( 1._wp + re2 * zcoef*zcoef ) * tmask(ji,jj,jk)
               END DO
            END DO
         END DO
      ENDIF

      !!---------------------------------!!
      !!   Equation to prognostic k      !!
      !!---------------------------------!!
      !
      ! Now Turbulent kinetic energy (output in en)
      ! -------------------------------
      ! Resolution of a tridiagonal linear system by a "methode de chasse"
      ! computation from level 2 to jpkm1  (e(1) computed after and e(jpk)=0 ).
      ! The surface boundary condition are set after
      ! The bottom boundary condition are also set after. In standard e(bottom)=0.
      ! z_elem_b : diagonal z_elem_c : upper diagonal z_elem_a : lower diagonal
      ! Warning : after this step, en : right hand side of the matrix

      DO jk = 2, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               !
               ! shear prod. at w-point weightened by mask
               shear(ji,jj,jk) =  ( avmu(ji-1,jj,jk) + avmu(ji,jj,jk) ) / MAX( 1.e0 , umask(ji-1,jj,jk) + umask(ji,jj,jk) )   &
                  &             + ( avmv(ji,jj-1,jk) + avmv(ji,jj,jk) ) / MAX( 1.e0 , vmask(ji,jj-1,jk) + vmask(ji,jj,jk) )
               !
               ! stratif. destruction
               buoy = - avt(ji,jj,jk) * rn2(ji,jj,jk)
               !
               ! shear prod. - stratif. destruction
               diss = eps(ji,jj,jk)
               !
               dir = 0.5_wp + SIGN( 0.5_wp, shear(ji,jj,jk) + buoy )   ! dir =1(=0) if shear(ji,jj,jk)+buoy >0(<0)
               !
               zesh2 = dir*(shear(ji,jj,jk)+buoy)+(1._wp-dir)*shear(ji,jj,jk)          ! production term
               zdiss = dir*(diss/en(ji,jj,jk))   +(1._wp-dir)*(diss-buoy)/en(ji,jj,jk) ! dissipation term
               !
               ! Compute a wall function from 1. to rsc_psi*zwall/rsc_psi0
               ! Note that as long that Dirichlet boundary conditions are NOT set at the first and last levels (GOTM style)
               ! there is no need to set a boundary condition for zwall_psi at the top and bottom boundaries.
               ! Otherwise, this should be rsc_psi/rsc_psi0
               IF( ln_sigpsi ) THEN
                  zsigpsi = MIN( 1._wp, zesh2 / eps(ji,jj,jk) )     ! 0. <= zsigpsi <= 1.
                  zwall_psi(ji,jj,jk) = rsc_psi /   & 
                     &     (  zsigpsi * rsc_psi + (1._wp-zsigpsi) * rsc_psi0 / MAX( zwall(ji,jj,jk), 1._wp )  )
               ELSE
                  zwall_psi(ji,jj,jk) = 1._wp
               ENDIF
               !
               ! building the matrix
               zcof = rfact_tke * tmask(ji,jj,jk)
               !
               ! lower diagonal
               z_elem_a(ji,jj,jk) = zcof * ( avm  (ji,jj,jk  ) + avm  (ji,jj,jk-1) )   &
                  &                      / ( fse3t(ji,jj,jk-1) * fse3w(ji,jj,jk  ) )
               !
               ! upper diagonal
               z_elem_c(ji,jj,jk) = zcof * ( avm  (ji,jj,jk+1) + avm  (ji,jj,jk  ) )   &
                  &                      / ( fse3t(ji,jj,jk  ) * fse3w(ji,jj,jk) )
               !
               ! diagonal
               z_elem_b(ji,jj,jk) = 1._wp - z_elem_a(ji,jj,jk) - z_elem_c(ji,jj,jk)  &
                  &                       + rdt * zdiss * tmask(ji,jj,jk) 
               !
               ! right hand side in en
               en(ji,jj,jk) = en(ji,jj,jk) + rdt * zesh2 * tmask(ji,jj,jk)
            END DO
         END DO
      END DO
      !
      z_elem_b(:,:,jpk) = 1._wp
      !
      ! Set surface condition on zwall_psi (1 at the bottom)
      IF( ln_sigpsi ) THEN
         zcoef = rsc_psi / rsc_psi0
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zwall_psi(ji,jj,1) = zcoef
            END DO
         END DO
      ENDIF

      ! Surface boundary condition on tke
      ! ---------------------------------
      !
      SELECT CASE ( nn_tkebc_surf )
      !
      CASE ( 0 )             ! Dirichlet case
         !
         IF (ln_crban) THEN     ! Wave induced mixing case
            !                      ! en(1) = q2(1) = 0.5 * (15.8 * Ccb)^(2/3) * u*^2
            !                      ! balance between the production and the dissipation terms including the wave effect
            en(:,:,1) = MAX( rsbc_tke1 * ustars2(:,:), rn_emin )
            z_elem_a(:,:,1) = en(:,:,1)
            z_elem_c(:,:,1) = 0._wp
            z_elem_b(:,:,1) = 1._wp
            ! 
            ! one level below
            en(:,:,2) = MAX( rsbc_tke1 * ustars2(:,:) * ( (zhsro(:,:)+fsdepw(:,:,2))/zhsro(:,:) )**ra_sf, rn_emin )
            z_elem_a(:,:,2) = 0._wp
            z_elem_c(:,:,2) = 0._wp
            z_elem_b(:,:,2) = 1._wp
            !
         ELSE                   ! No wave induced mixing case
            !                      ! en(1) = u*^2/C0^2  &  l(1)  = K*zs
            !                      ! balance between the production and the dissipation terms
            en(:,:,1) = MAX( rc02r * ustars2(:,:), rn_emin )
            z_elem_a(:,:,1) = en(:,:,1) 
            z_elem_c(:,:,1) = 0._wp
            z_elem_b(:,:,1) = 1._wp
            !
            ! one level below
            en(:,:,2) = MAX( rc02r * ustars2(:,:), rn_emin )
            z_elem_a(:,:,2) = 0._wp
            z_elem_c(:,:,2) = 0._wp
            z_elem_b(:,:,2) = 1._wp
            !
         ENDIF
         !
      CASE ( 1 )             ! Neumann boundary condition on d(e)/dz
         !
         IF (ln_crban) THEN ! Shear free case: d(e)/dz= Fw
            !
            ! Dirichlet conditions at k=1 (Cosmetic)
            en(:,:,1) = MAX( rsbc_tke1 * ustars2(:,:), rn_emin )
            z_elem_a(:,:,1) = en(:,:,1)
            z_elem_c(:,:,1) = 0._wp
            z_elem_b(:,:,1) = 1._wp
            ! at k=2, set de/dz=Fw
            z_elem_b(:,:,2) = z_elem_b(:,:,2) +  z_elem_a(:,:,2) ! Remove z_elem_a from z_elem_b
            z_elem_a(:,:,2) = 0._wp        
            zflxs(:,:) = rsbc_tke3 * ustars2(:,:)**1.5_wp * ( (zhsro(:,:)+fsdept(:,:,1) ) / zhsro(:,:) )**(1.5*ra_sf)
            en(:,:,2) = en(:,:,2) + zflxs(:,:) / fse3w(:,:,2)
            !
         ELSE                   ! No wave induced mixing case: d(e)/dz=0.
            !
            ! Dirichlet conditions at k=1 (Cosmetic)
            en(:,:,1) = MAX( rc02r * ustars2(:,:), rn_emin )
            z_elem_a(:,:,1) = en(:,:,1)
            z_elem_c(:,:,1) = 0._wp
            z_elem_b(:,:,1) = 1._wp
            ! at k=2 set de/dz=0.:
            z_elem_b(:,:,2) = z_elem_b(:,:,2) +  z_elem_a(:,:,2)  ! Remove z_elem_a from z_elem_b
            z_elem_a(:,:,2) = 0._wp
            !
         ENDIF
         !
      END SELECT

      ! Bottom boundary condition on tke
      ! --------------------------------
      !
      SELECT CASE ( nn_tkebc_bot )
      !
      CASE ( 0 )             ! Dirichlet 
         !                      ! en(ibot) = u*^2 / Co2 and mxln(ibot) = rn_lmin
         !                      ! Balance between the production and the dissipation terms
!CDIR NOVERRCHK
         DO jj = 2, jpjm1
!CDIR NOVERRCHK
            DO ji = fs_2, fs_jpim1   ! vector opt.
               ibot   = mbkt(ji,jj) + 1      ! k   bottom level of w-point
               ibotm1 = mbkt(ji,jj)          ! k-1 bottom level of w-point but >=1
               !
               ! Bottom level Dirichlet condition:
               z_elem_a(ji,jj,ibot  ) = 0._wp
               z_elem_c(ji,jj,ibot  ) = 0._wp
               z_elem_b(ji,jj,ibot  ) = 1._wp
               en(ji,jj,ibot  ) = MAX( rc02r * ustarb2(ji,jj), rn_emin )
               !
               ! Just above last level, Dirichlet condition again
               z_elem_a(ji,jj,ibotm1) = 0._wp
               z_elem_c(ji,jj,ibotm1) = 0._wp
               z_elem_b(ji,jj,ibotm1) = 1._wp
               en(ji,jj,ibotm1) = MAX( rc02r * ustarb2(ji,jj), rn_emin ) 
            END DO
         END DO
         !
      CASE ( 1 )             ! Neumman boundary condition
         !                      
!CDIR NOVERRCHK
         DO jj = 2, jpjm1
!CDIR NOVERRCHK
            DO ji = fs_2, fs_jpim1   ! vector opt.
               ibot   = mbkt(ji,jj) + 1      ! k   bottom level of w-point
               ibotm1 = mbkt(ji,jj)          ! k-1 bottom level of w-point but >=1
               !
               ! Bottom level Dirichlet condition:
               z_elem_a(ji,jj,ibot) = 0._wp
               z_elem_c(ji,jj,ibot) = 0._wp
               z_elem_b(ji,jj,ibot) = 1._wp
               en(ji,jj,ibot) = MAX( rc02r * ustarb2(ji,jj), rn_emin )
               !
               ! Just above last level: Neumann condition
               z_elem_b(ji,jj,ibotm1) = z_elem_b(ji,jj,ibotm1) + z_elem_c(ji,jj,ibotm1)   ! Remove z_elem_c from z_elem_b
               z_elem_c(ji,jj,ibotm1) = 0._wp
            END DO
         END DO
         !
      END SELECT

      ! Matrix inversion (en prescribed at surface and the bottom)
      ! ----------------------------------------------------------
      !
      DO jk = 2, jpkm1                             ! First recurrence : Dk = Dk - Lk * Uk-1 / Dk-1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1    ! vector opt.
               z_elem_b(ji,jj,jk) = z_elem_b(ji,jj,jk) - z_elem_a(ji,jj,jk) * z_elem_c(ji,jj,jk-1) / z_elem_b(ji,jj,jk-1)
            END DO
         END DO
      END DO
      DO jk = 2, jpk                               ! Second recurrence : Lk = RHSk - Lk / Dk-1 * Lk-1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1    ! vector opt.
               z_elem_a(ji,jj,jk) = en(ji,jj,jk) - z_elem_a(ji,jj,jk) / z_elem_b(ji,jj,jk-1) * z_elem_a(ji,jj,jk-1)
            END DO
         END DO
      END DO
      DO jk = jpk-1, 2, -1                         ! thrid recurrence : Ek = ( Lk - Uk * Ek+1 ) / Dk
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1    ! vector opt.
               en(ji,jj,jk) = ( z_elem_a(ji,jj,jk) - z_elem_c(ji,jj,jk) * en(ji,jj,jk+1) ) / z_elem_b(ji,jj,jk)
            END DO
         END DO
      END DO
      !                                            ! set the minimum value of tke 
      en(:,:,:) = MAX( en(:,:,:), rn_emin )
      
      !!----------------------------------------!!
      !!   Solve prognostic equation for psi    !!
      !!----------------------------------------!!

      ! Set psi to previous time step value
      !
      SELECT CASE ( nn_clos )
      !
      CASE( 0 )               ! k-kl  (Mellor-Yamada)
         DO jk = 2, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  psi(ji,jj,jk)  = eb(ji,jj,jk) * mxlb(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      CASE( 1 )               ! k-eps
         DO jk = 2, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  psi(ji,jj,jk)  = eps(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      CASE( 2 )               ! k-w
         DO jk = 2, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  psi(ji,jj,jk)  = SQRT( eb(ji,jj,jk) ) / ( rc0 * mxlb(ji,jj,jk) )
               END DO
            END DO
         END DO
         !
      CASE( 3 )               ! generic
         DO jk = 2, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  psi(ji,jj,jk)  = rc02 * eb(ji,jj,jk) * mxlb(ji,jj,jk)**rnn 
               END DO
            END DO
         END DO
         !
      END SELECT
      !
      ! Now gls (output in psi)
      ! -------------------------------
      ! Resolution of a tridiagonal linear system by a "methode de chasse"
      ! computation from level 2 to jpkm1  (e(1) already computed and e(jpk)=0 ).
      ! z_elem_b : diagonal z_elem_c : upper diagonal z_elem_a : lower diagonal
      ! Warning : after this step, en : right hand side of the matrix

      DO jk = 2, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               !
               ! psi / k
               zratio = psi(ji,jj,jk) / eb(ji,jj,jk) 
               !
               ! psi3+ : stable : B=-KhN²<0 => N²>0 if rn2>0 dir = 1 (stable) otherwise dir = 0 (unstable)
               dir = 0.5_wp + SIGN( 0.5_wp, rn2(ji,jj,jk) )
               !
               rpsi3 = dir * rpsi3m + ( 1._wp - dir ) * rpsi3p
               !
               ! shear prod. - stratif. destruction
               prod = rpsi1 * zratio * shear(ji,jj,jk)
               !
               ! stratif. destruction
               buoy = rpsi3 * zratio * (- avt(ji,jj,jk) * rn2(ji,jj,jk) )
               !
               ! shear prod. - stratif. destruction
               diss = rpsi2 * zratio * zwall(ji,jj,jk) * eps(ji,jj,jk)
               !
               dir = 0.5_wp + SIGN( 0.5_wp, prod + buoy )   ! dir =1(=0) if shear(ji,jj,jk)+buoy >0(<0)
               !
               zesh2 = dir * ( prod + buoy )          + (1._wp - dir ) * prod                        ! production term
               zdiss = dir * ( diss / psi(ji,jj,jk) ) + (1._wp - dir ) * (diss-buoy) / psi(ji,jj,jk) ! dissipation term
               !                                                        
               ! building the matrix
               zcof = rfact_psi * zwall_psi(ji,jj,jk) * tmask(ji,jj,jk)
               ! lower diagonal
               z_elem_a(ji,jj,jk) = zcof * ( avm  (ji,jj,jk  ) + avm  (ji,jj,jk-1) )   &
                  &                      / ( fse3t(ji,jj,jk-1) * fse3w(ji,jj,jk  ) )
               ! upper diagonal
               z_elem_c(ji,jj,jk) = zcof * ( avm  (ji,jj,jk+1) + avm  (ji,jj,jk  ) )   &
                  &                      / ( fse3t(ji,jj,jk  ) * fse3w(ji,jj,jk) )
               ! diagonal
               z_elem_b(ji,jj,jk) = 1._wp - z_elem_a(ji,jj,jk) - z_elem_c(ji,jj,jk)  &
                  &                       + rdt * zdiss * tmask(ji,jj,jk)
               !
               ! right hand side in psi
               psi(ji,jj,jk) = psi(ji,jj,jk) + rdt * zesh2 * tmask(ji,jj,jk)
            END DO
         END DO
      END DO
      !
      z_elem_b(:,:,jpk) = 1._wp

      ! Surface boundary condition on psi
      ! ---------------------------------
      !
      SELECT CASE ( nn_psibc_surf )
      !
      CASE ( 0 )             ! Dirichlet boundary conditions
         !
         IF( ln_crban ) THEN       ! Wave induced mixing case
            !                      ! en(1) = q2(1) = 0.5 * (15.8 * Ccb)^(2/3) * u*^2
            !                      ! balance between the production and the dissipation terms including the wave effect
            zdep(:,:) = rl_sf * zhsro(:,:)
            psi (:,:,1) = rc0**rpp * en(:,:,1)**rmm * zdep(:,:)**rnn * tmask(:,:,1)
            z_elem_a(:,:,1) = psi(:,:,1)
            z_elem_c(:,:,1) = 0._wp
            z_elem_b(:,:,1) = 1._wp
            !
            ! one level below
            zex1 = (rmm*ra_sf+rnn)
            zex2 = (rmm*ra_sf)
            zdep(:,:) = ( (zhsro(:,:) + fsdepw(:,:,2))**zex1 ) / zhsro(:,:)**zex2
            psi (:,:,2) = rsbc_psi1 * ustars2(:,:)**rmm * zdep(:,:) * tmask(:,:,1)
            z_elem_a(:,:,2) = 0._wp
            z_elem_c(:,:,2) = 0._wp
            z_elem_b(:,:,2) = 1._wp
            ! 
         ELSE                   ! No wave induced mixing case
            !                      ! en(1) = u*^2/C0^2  &  l(1)  = K*zs
            !                      ! balance between the production and the dissipation terms
            !
            zdep(:,:) = vkarmn * zhsro(:,:)
            psi (:,:,1) = rc0**rpp * en(:,:,1)**rmm * zdep(:,:)**rnn * tmask(:,:,1)
            z_elem_a(:,:,1) = psi(:,:,1)
            z_elem_c(:,:,1) = 0._wp
            z_elem_b(:,:,1) = 1._wp
            !
            ! one level below
            zdep(:,:) = vkarmn * ( zhsro(:,:) + fsdepw(:,:,2) )
            psi (:,:,2) = rc0**rpp * en(:,:,1)**rmm * zdep(:,:)**rnn * tmask(:,:,1)
            z_elem_a(:,:,2) = 0._wp
            z_elem_c(:,:,2) = 0._wp
            z_elem_b(:,:,2) = 1.
            !
         ENDIF
         !
      CASE ( 1 )             ! Neumann boundary condition on d(psi)/dz
         !
         IF( ln_crban ) THEN     ! Wave induced mixing case
            !
            zdep(:,:) = rl_sf * zhsro(:,:)
            psi (:,:,1) = rc0**rpp * en(:,:,1)**rmm * zdep(:,:)**rnn * tmask(:,:,1)
            z_elem_a(:,:,1) = psi(:,:,1)
            z_elem_c(:,:,1) = 0._wp
            z_elem_b(:,:,1) = 1._wp
            !
            ! Neumann condition at k=2
            z_elem_b(:,:,2) = z_elem_b(:,:,2) +  z_elem_a(:,:,2) ! Remove z_elem_a from z_elem_b
            z_elem_a(:,:,2) = 0._wp
            !
            ! Set psi vertical flux at the surface:
            zdep(:,:) = (zhsro(:,:) + fsdept(:,:,1))**(rmm*ra_sf+rnn-1._wp) / zhsro(:,:)**(rmm*ra_sf)
            zflxs(:,:) = rsbc_psi3 * ( zwall_psi(:,:,1)*avm(:,:,1) + zwall_psi(:,:,2)*avm(:,:,2) ) & 
               &                   * en(:,:,1)**rmm * zdep         
            psi(:,:,2) = psi(:,:,2) + zflxs(:,:) / fse3w(:,:,2)
            !
      ELSE                   ! No wave induced mixing
            !
            zdep(:,:) = vkarmn * zhsro(:,:)
            psi (:,:,1) = rc0**rpp * en(:,:,1)**rmm * zdep(:,:)**rnn * tmask(:,:,1)
            z_elem_a(:,:,1) = psi(:,:,1)
            z_elem_c(:,:,1) = 0._wp
            z_elem_b(:,:,1) = 1._wp
            !
            ! Neumann condition at k=2
            z_elem_b(:,:,2) = z_elem_b(:,:,2) +  z_elem_a(:,:,2) ! Remove z_elem_a from z_elem_b
            z_elem_a(ji,jj,2) = 0._wp
            !
            ! Set psi vertical flux at the surface:
            zdep(:,:)  = zhsro(:,:) + fsdept(:,:,1)
            zflxs(:,:) = rsbc_psi2 * ( avm(:,:,1) + avm(:,:,2) ) * en(:,:,1)**rmm * zdep**(rnn-1._wp)
            psi(:,:,2) = psi(:,:,2) + zflxs(:,:) / fse3w(:,:,2)
            !     
         ENDIF
         !
      END SELECT

      ! Bottom boundary condition on psi
      ! --------------------------------
      !
      SELECT CASE ( nn_psibc_bot )
      !
      CASE ( 0 )             ! Dirichlet 
         !                      ! en(ibot) = u*^2 / Co2 and mxln(ibot) = vkarmn * hbro
         !                      ! Balance between the production and the dissipation terms
!CDIR NOVERRCHK
         DO jj = 2, jpjm1
!CDIR NOVERRCHK
            DO ji = fs_2, fs_jpim1   ! vector opt.
               ibot   = mbkt(ji,jj) + 1      ! k   bottom level of w-point
               ibotm1 = mbkt(ji,jj)          ! k-1 bottom level of w-point but >=1
               zdep(ji,jj) = vkarmn * hbro
               psi (ji,jj,ibot) = rc0**rpp * en(ji,jj,ibot)**rmm * zdep(ji,jj)**rnn
               z_elem_a(ji,jj,ibot) = 0._wp
               z_elem_c(ji,jj,ibot) = 0._wp
               z_elem_b(ji,jj,ibot) = 1._wp
               !
               ! Just above last level, Dirichlet condition again (GOTM like)
               zdep(ji,jj) = vkarmn * ( hbro + fse3t(ji,jj,ibotm1) )
               psi (ji,jj,ibotm1) = rc0**rpp * en(ji,jj,ibot  )**rmm * zdep(ji,jj)**rnn
               z_elem_a(ji,jj,ibotm1) = 0._wp
               z_elem_c(ji,jj,ibotm1) = 0._wp
               z_elem_b(ji,jj,ibotm1) = 1._wp
            END DO
         END DO
         !
      CASE ( 1 )             ! Neumman boundary condition
         !                      
!CDIR NOVERRCHK
         DO jj = 2, jpjm1
!CDIR NOVERRCHK
            DO ji = fs_2, fs_jpim1   ! vector opt.
               ibot   = mbkt(ji,jj) + 1      ! k   bottom level of w-point
               ibotm1 = mbkt(ji,jj)          ! k-1 bottom level of w-point but >=1
               !
               ! Bottom level Dirichlet condition:
               zdep(ji,jj) = vkarmn * hbro
               psi (ji,jj,ibot) = rc0**rpp * en(ji,jj,ibot)**rmm * zdep(ji,jj)**rnn
               !
               z_elem_a(ji,jj,ibot) = 0._wp
               z_elem_c(ji,jj,ibot) = 0._wp
               z_elem_b(ji,jj,ibot) = 1._wp
               !
               ! Just above last level: Neumann condition with flux injection
               z_elem_b(ji,jj,ibotm1) = z_elem_b(ji,jj,ibotm1) + z_elem_c(ji,jj,ibotm1) ! Remove z_elem_c from z_elem_b
               z_elem_c(ji,jj,ibotm1) = 0.
               !
               ! Set psi vertical flux at the bottom:
               zdep(ji,jj) = hbro + 0.5_wp*fse3t(ji,jj,ibotm1)
               zflxb = rsbc_psi2 * ( avm(ji,jj,ibot) + avm(ji,jj,ibotm1) )   &
                  &  * (0.5_wp*(en(ji,jj,ibot)+en(ji,jj,ibotm1)))**rmm * zdep(ji,jj)**(rnn-1._wp)
               psi(ji,jj,ibotm1) = psi(ji,jj,ibotm1) + zflxb / fse3w(ji,jj,ibotm1)
            END DO
         END DO
         !
      END SELECT

      ! Matrix inversion
      ! ----------------
      !
      DO jk = 2, jpkm1                             ! First recurrence : Dk = Dk - Lk * Uk-1 / Dk-1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1    ! vector opt.
               z_elem_b(ji,jj,jk) = z_elem_b(ji,jj,jk) - z_elem_a(ji,jj,jk) * z_elem_c(ji,jj,jk-1) / z_elem_b(ji,jj,jk-1)
            END DO
         END DO
      END DO
      DO jk = 2, jpk                               ! Second recurrence : Lk = RHSk - Lk / Dk-1 * Lk-1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1    ! vector opt.
               z_elem_a(ji,jj,jk) = psi(ji,jj,jk) - z_elem_a(ji,jj,jk) / z_elem_b(ji,jj,jk-1) * z_elem_a(ji,jj,jk-1)
            END DO
         END DO
      END DO
      DO jk = jpk-1, 2, -1                         ! Third recurrence : Ek = ( Lk - Uk * Ek+1 ) / Dk
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1    ! vector opt.
               psi(ji,jj,jk) = ( z_elem_a(ji,jj,jk) - z_elem_c(ji,jj,jk) * psi(ji,jj,jk+1) ) / z_elem_b(ji,jj,jk)
            END DO
         END DO
      END DO

      ! Set dissipation
      !----------------

      SELECT CASE ( nn_clos )
      !
      CASE( 0 )               ! k-kl  (Mellor-Yamada)
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  eps(ji,jj,jk) = rc03 * en(ji,jj,jk) * en(ji,jj,jk) * SQRT( en(ji,jj,jk) ) / psi(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      CASE( 1 )               ! k-eps
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  eps(ji,jj,jk) = psi(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      CASE( 2 )               ! k-w
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  eps(ji,jj,jk) = rc04 * en(ji,jj,jk) * psi(ji,jj,jk) 
               END DO
            END DO
         END DO
         !
      CASE( 3 )               ! generic
         zcoef = rc0**( 3._wp  + rpp/rnn )
         zex1  =      ( 1.5_wp + rmm/rnn )
         zex2  = -1._wp / rnn
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  eps(ji,jj,jk) = zcoef * en(ji,jj,jk)**zex1 * psi(ji,jj,jk)**zex2
               END DO
            END DO
         END DO
         !
      END SELECT

      ! Limit dissipation rate under stable stratification
      ! --------------------------------------------------
      DO jk = 1, jpkm1 ! Note that this set boundary conditions on mxln at the same time
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1    ! vector opt.
               ! limitation
               eps(ji,jj,jk)  = MAX( eps(ji,jj,jk), rn_epsmin )
               mxln(ji,jj,jk)  = rc03 * en(ji,jj,jk) * SQRT( en(ji,jj,jk) ) / eps(ji,jj,jk)
               ! Galperin criterium (NOTE : Not required if the proper value of C3 in stable cases is calculated) 
               zrn2 = MAX( rn2(ji,jj,jk), rsmall )
               mxln(ji,jj,jk) = MIN(  rn_clim_galp * SQRT( 2._wp * en(ji,jj,jk) / zrn2 ), mxln(ji,jj,jk)  )
            END DO
         END DO
      END DO 

      !
      ! Stability function and vertical viscosity and diffusivity
      ! ---------------------------------------------------------
      !
      SELECT CASE ( nn_stab_func )
      !
      CASE ( 0 , 1 )             ! Galperin or Kantha-Clayson stability functions
         DO jk = 2, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  ! zcof =  l²/q²
                  zcof = mxlb(ji,jj,jk) * mxlb(ji,jj,jk) / ( 2._wp*eb(ji,jj,jk) )
                  ! Gh = -N²l²/q²
                  gh = - rn2(ji,jj,jk) * zcof
                  gh = MIN( gh, rgh0   )
                  gh = MAX( gh, rghmin )
                  ! Stability functions from Kantha and Clayson (if C2=C3=0 => Galperin)
                  sh = ra2*( 1._wp-6._wp*ra1/rb1 ) / ( 1.-3.*ra2*gh*(6.*ra1+rb2*( 1._wp-rc3 ) ) )
                  sm = ( rb1**(-1._wp/3._wp) + ( 18._wp*ra1*ra1 + 9._wp*ra1*ra2*(1._wp-rc2) )*sh*gh ) / (1._wp-9._wp*ra1*ra2*gh)
                  !
                  ! Store stability function in avmu and avmv
                  avmu(ji,jj,jk) = rc_diff * sh * tmask(ji,jj,jk)
                  avmv(ji,jj,jk) = rc_diff * sm * tmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      CASE ( 2, 3 )               ! Canuto stability functions
         DO jk = 2, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  ! zcof =  l²/q²
                  zcof = mxlb(ji,jj,jk)*mxlb(ji,jj,jk) / ( 2._wp * eb(ji,jj,jk) )
                  ! Gh = -N²l²/q²
                  gh = - rn2(ji,jj,jk) * zcof
                  gh = MIN( gh, rgh0   )
                  gh = MAX( gh, rghmin )
                  gh = gh * rf6
                  ! Gm =  M²l²/q² Shear number
                  shr = shear(ji,jj,jk) / MAX( avm(ji,jj,jk), rsmall )
                  gm = MAX( shr * zcof , 1.e-10 )
                  gm = gm * rf6
                  gm = MIN ( (rd0 - rd1*gh + rd3*gh*gh) / (rd2-rd4*gh) , gm )
                  ! Stability functions from Canuto
                  rcff = rd0 - rd1*gh +rd2*gm + rd3*gh*gh - rd4*gh*gm + rd5*gm*gm
                  sm = (rs0 - rs1*gh + rs2*gm) / rcff
                  sh = (rs4 - rs5*gh + rs6*gm) / rcff
                  !
                  ! Store stability function in avmu and avmv
                  avmu(ji,jj,jk) = rc_diff * sh * tmask(ji,jj,jk)
                  avmv(ji,jj,jk) = rc_diff * sm * tmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      END SELECT

      ! Boundary conditions on stability functions for momentum (Neumann):
      ! Lines below are useless if GOTM style Dirichlet conditions are used
      zcoef = rcm_sf / SQRT( 2._wp )
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            avmv(ji,jj,1) = zcoef
         END DO
      END DO
      zcoef = rc0 / SQRT( 2._wp )
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            avmv(ji,jj,mbkt(ji,jj)+1) = zcoef
         END DO
      END DO

      ! Compute diffusivities/viscosities
      ! The computation below could be restrained to jk=2 to jpkm1 if GOTM style Dirichlet conditions are used
      DO jk = 1, jpk
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zsqen         = SQRT( 2._wp * en(ji,jj,jk) ) * mxln(ji,jj,jk)
               zav           = zsqen * avmu(ji,jj,jk)
               avt(ji,jj,jk) = MAX( zav, avtb(jk) )*tmask(ji,jj,jk) ! apply mask for zdfmxl routine
               zav           = zsqen * avmv(ji,jj,jk)
               avm(ji,jj,jk) = MAX( zav, avmb(jk) ) ! Note that avm is not masked at the surface and the bottom
            END DO
         END DO
      END DO
      !
      ! Lateral boundary conditions (sign unchanged)
      avt(:,:,1)  = 0._wp
      CALL lbc_lnk( avm, 'W', 1. )   ;   CALL lbc_lnk( avt, 'W', 1. )

      DO jk = 2, jpkm1            !* vertical eddy viscosity at u- and v-points
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               avmu(ji,jj,jk) = 0.5 * ( avm(ji,jj,jk) + avm(ji+1,jj  ,jk) ) * umask(ji,jj,jk)
               avmv(ji,jj,jk) = 0.5 * ( avm(ji,jj,jk) + avm(ji  ,jj+1,jk) ) * vmask(ji,jj,jk)
            END DO
         END DO
      END DO
      avmu(:,:,1) = 0._wp             ;   avmv(:,:,1) = 0._wp                 ! set surface to zero
      CALL lbc_lnk( avmu, 'U', 1. )   ;   CALL lbc_lnk( avmv, 'V', 1. )       ! Lateral boundary conditions

      IF(ln_ctl) THEN
         CALL prt_ctl( tab3d_1=en  , clinfo1=' gls  - e: ', tab3d_2=avt, clinfo2=' t: ', ovlap=1, kdim=jpk)
         CALL prt_ctl( tab3d_1=avmu, clinfo1=' gls  - u: ', mask1=umask,                   &
            &          tab3d_2=avmv, clinfo2=       ' v: ', mask2=vmask, ovlap=1, kdim=jpk )
      ENDIF
      !
      avt_k (:,:,:) = avt (:,:,:)
      avm_k (:,:,:) = avm (:,:,:)
      avmu_k(:,:,:) = avmu(:,:,:)
      avmv_k(:,:,:) = avmv(:,:,:)
      !
      CALL wrk_dealloc( jpi,jpj, zdep, zflxs, zhsro )
      CALL wrk_dealloc( jpi,jpj,jpk, eb, mxlb, shear, eps, zwall_psi, z_elem_a, z_elem_b, z_elem_c, psi )
      !
      IF( nn_timing == 1 )  CALL timing_stop('zdf_gls')
      !
      !
   END SUBROUTINE zdf_gls


   SUBROUTINE zdf_gls_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_gls_init  ***
      !!                     
      !! ** Purpose :   Initialization of the vertical eddy diffivity and 
      !!      viscosity when using a gls turbulent closure scheme
      !!
      !! ** Method  :   Read the namzdf_gls namelist and check the parameters
      !!      called at the first timestep (nit000)
      !!
      !! ** input   :   Namlist namzdf_gls
      !!
      !! ** Action  :   Increase by 1 the nstop flag is setting problem encounter
      !!
      !!----------------------------------------------------------------------
      USE dynzdf_exp
      USE trazdf_exp
      !
      INTEGER ::   jk    ! dummy loop indices
      REAL(wp)::   zcr   ! local scalar
      !!
      NAMELIST/namzdf_gls/rn_emin, rn_epsmin, ln_length_lim, &
         &            rn_clim_galp, ln_crban, ln_sigpsi,     &
         &            rn_crban, rn_charn,                    &
         &            nn_tkebc_surf, nn_tkebc_bot,           &
         &            nn_psibc_surf, nn_psibc_bot,           &
         &            nn_stab_func, nn_clos
      !!----------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zdf_gls_init')
      !
      REWIND( numnam )                 !* Read Namelist namzdf_gls
      READ  ( numnam, namzdf_gls )

      IF(lwp) THEN                     !* Control print
         WRITE(numout,*)
         WRITE(numout,*) 'zdf_gls_init : gls turbulent closure scheme'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namzdf_gls : set gls mixing parameters'
         WRITE(numout,*) '      minimum value of en                           rn_emin       = ', rn_emin
         WRITE(numout,*) '      minimum value of eps                          rn_epsmin     = ', rn_epsmin
         WRITE(numout,*) '      Limit dissipation rate under stable stratif.  ln_length_lim = ', ln_length_lim
         WRITE(numout,*) '      Galperin limit (Standard: 0.53, Holt: 0.26)   rn_clim_galp  = ', rn_clim_galp
         WRITE(numout,*) '      TKE Surface boundary condition                nn_tkebc_surf = ', nn_tkebc_surf
         WRITE(numout,*) '      TKE Bottom boundary condition                 nn_tkebc_bot  = ', nn_tkebc_bot
         WRITE(numout,*) '      PSI Surface boundary condition                nn_psibc_surf = ', nn_psibc_surf
         WRITE(numout,*) '      PSI Bottom boundary condition                 nn_psibc_bot  = ', nn_psibc_bot
         WRITE(numout,*) '      Craig and Banner scheme                       ln_crban      = ', ln_crban
         WRITE(numout,*) '      Modify psi Schmidt number (wb case)           ln_sigpsi     = ', ln_sigpsi
         WRITE(numout,*) '      Craig and Banner coefficient                  rn_crban       = ', rn_crban
         WRITE(numout,*) '      Charnock coefficient                          rn_charn       = ', rn_charn
         WRITE(numout,*) '      Stability functions                           nn_stab_func   = ', nn_stab_func
         WRITE(numout,*) '      Type of closure                               nn_clos        = ', nn_clos
         WRITE(numout,*) '   Hard coded parameters'
         WRITE(numout,*) '      Surface roughness (m)                         hsro          = ', hsro
         WRITE(numout,*) '      Bottom roughness (m)                          hbro          = ', hbro
      ENDIF

      !                                !* allocate gls arrays
      IF( zdf_gls_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'zdf_gls_init : unable to allocate arrays' )

      !                                !* Check of some namelist values
      IF( nn_tkebc_surf < 0 .OR. nn_tkebc_surf > 1 ) CALL ctl_stop( 'bad flag: nn_tkebc_surf is 0 or 1' )
      IF( nn_psibc_surf < 0 .OR. nn_psibc_surf > 1 ) CALL ctl_stop( 'bad flag: nn_psibc_surf is 0 or 1' )
      IF( nn_tkebc_bot  < 0 .OR. nn_tkebc_bot  > 1 ) CALL ctl_stop( 'bad flag: nn_tkebc_bot is 0 or 1' )
      IF( nn_psibc_bot  < 0 .OR. nn_psibc_bot  > 1 ) CALL ctl_stop( 'bad flag: nn_psibc_bot is 0 or 1' )
      IF( nn_stab_func  < 0 .OR. nn_stab_func  > 3 ) CALL ctl_stop( 'bad flag: nn_stab_func is 0, 1, 2 and 3' )
      IF( nn_clos       < 0 .OR. nn_clos       > 3 ) CALL ctl_stop( 'bad flag: nn_clos is 0, 1, 2 or 3' )

      SELECT CASE ( nn_clos )          !* set the parameters for the chosen closure
      !
      CASE( 0 )                              ! k-kl  (Mellor-Yamada)
         !
         IF(lwp) WRITE(numout,*) 'The choosen closure is k-kl closed to the classical Mellor-Yamada'
         rpp     = 0._wp
         rmm     = 1._wp
         rnn     = 1._wp
         rsc_tke = 1.96_wp
         rsc_psi = 1.96_wp
         rpsi1   = 0.9_wp
         rpsi3p  = 1._wp
         rpsi2   = 0.5_wp
         !
         SELECT CASE ( nn_stab_func )
         CASE( 0, 1 )   ;   rpsi3m = 2.53_wp       ! G88 or KC stability functions
         CASE( 2 )      ;   rpsi3m = 2.38_wp       ! Canuto A stability functions
         CASE( 3 )      ;   rpsi3m = 2.38          ! Canuto B stability functions (caution : constant not identified)
         END SELECT
         !
      CASE( 1 )                              ! k-eps
         !
         IF(lwp) WRITE(numout,*) 'The choosen closure is k-eps'
         rpp     =  3._wp
         rmm     =  1.5_wp
         rnn     = -1._wp
         rsc_tke =  1._wp
         rsc_psi =  1.3_wp  ! Schmidt number for psi
         rpsi1   =  1.44_wp
         rpsi3p  =  1._wp
         rpsi2   =  1.92_wp
         !
         SELECT CASE ( nn_stab_func )
         CASE( 0, 1 )   ;   rpsi3m = -0.52_wp      ! G88 or KC stability functions
         CASE( 2 )      ;   rpsi3m = -0.629_wp     ! Canuto A stability functions
         CASE( 3 )      ;   rpsi3m = -0.566        ! Canuto B stability functions
         END SELECT
         !
      CASE( 2 )                              ! k-omega
         !
         IF(lwp) WRITE(numout,*) 'The choosen closure is k-omega'
         rpp     = -1._wp
         rmm     =  0.5_wp
         rnn     = -1._wp
         rsc_tke =  2._wp
         rsc_psi =  2._wp
         rpsi1   =  0.555_wp
         rpsi3p  =  1._wp
         rpsi2   =  0.833_wp
         !
         SELECT CASE ( nn_stab_func )
         CASE( 0, 1 )   ;   rpsi3m = -0.58_wp       ! G88 or KC stability functions
         CASE( 2 )      ;   rpsi3m = -0.64_wp       ! Canuto A stability functions
         CASE( 3 )      ;   rpsi3m = -0.64_wp       ! Canuto B stability functions caution : constant not identified)
         END SELECT
         !
      CASE( 3 )                              ! generic
         !
         IF(lwp) WRITE(numout,*) 'The choosen closure is generic'
         rpp     = 2._wp
         rmm     = 1._wp
         rnn     = -0.67_wp
         rsc_tke = 0.8_wp
         rsc_psi = 1.07_wp
         rpsi1   = 1._wp
         rpsi3p  = 1._wp
         rpsi2   = 1.22_wp
         !
         SELECT CASE ( nn_stab_func )
         CASE( 0, 1 )   ;   rpsi3m = 0.1_wp         ! G88 or KC stability functions
         CASE( 2 )      ;   rpsi3m = 0.05_wp        ! Canuto A stability functions
         CASE( 3 )      ;   rpsi3m = 0.05_wp        ! Canuto B stability functions caution : constant not identified)
         END SELECT
         !
      END SELECT

      !
      SELECT CASE ( nn_stab_func )     !* set the parameters of the stability functions
      !
      CASE ( 0 )                             ! Galperin stability functions
         !
         IF(lwp) WRITE(numout,*) 'Stability functions from Galperin'
         rc2     =  0._wp
         rc3     =  0._wp
         rc_diff =  1._wp
         rc0     =  0.5544_wp
         rcm_sf  =  0.9884_wp
         rghmin  = -0.28_wp
         rgh0    =  0.0233_wp
         rghcri  =  0.02_wp
         !
      CASE ( 1 )                             ! Kantha-Clayson stability functions
         !
         IF(lwp) WRITE(numout,*) 'Stability functions from Kantha-Clayson'
         rc2     =  0.7_wp
         rc3     =  0.2_wp
         rc_diff =  1._wp
         rc0     =  0.5544_wp
         rcm_sf  =  0.9884_wp
         rghmin  = -0.28_wp
         rgh0    =  0.0233_wp
         rghcri  =  0.02_wp
         !
      CASE ( 2 )                             ! Canuto A stability functions
         !
         IF(lwp) WRITE(numout,*) 'Stability functions from Canuto A'
         rs0 = 1.5_wp * rl1 * rl5*rl5
         rs1 = -rl4*(rl6+rl7) + 2._wp*rl4*rl5*(rl1-(1._wp/3._wp)*rl2-rl3) + 1.5_wp*rl1*rl5*rl8
         rs2 = -(3._wp/8._wp) * rl1*(rl6*rl6-rl7*rl7)
         rs4 = 2._wp * rl5
         rs5 = 2._wp * rl4
         rs6 = (2._wp/3._wp) * rl5 * ( 3._wp*rl3*rl3 - rl2*rl2 ) - 0.5_wp * rl5*rl1 * (3._wp*rl3-rl2)   &
            &                                                    + 0.75_wp * rl1 * ( rl6 - rl7 )
         rd0 = 3._wp * rl5*rl5
         rd1 = rl5 * ( 7._wp*rl4 + 3._wp*rl8 )
         rd2 = rl5*rl5 * ( 3._wp*rl3*rl3 - rl2*rl2 ) - 0.75_wp*(rl6*rl6 - rl7*rl7 )
         rd3 = rl4 * ( 4._wp*rl4 + 3._wp*rl8)
         rd4 = rl4 * ( rl2 * rl6 - 3._wp*rl3*rl7 - rl5*(rl2*rl2 - rl3*rl3 ) ) + rl5*rl8 * ( 3._wp*rl3*rl3 - rl2*rl2 )
         rd5 = 0.25_wp * ( rl2*rl2 - 3._wp *rl3*rl3 ) * ( rl6*rl6 - rl7*rl7 )
         rc0 = 0.5268_wp
         rf6 = 8._wp / (rc0**6._wp)
         rc_diff = SQRT(2._wp) / (rc0**3._wp)
         rcm_sf  =  0.7310_wp
         rghmin  = -0.28_wp
         rgh0    =  0.0329_wp
         rghcri  =  0.03_wp
         !
      CASE ( 3 )                             ! Canuto B stability functions
         !
         IF(lwp) WRITE(numout,*) 'Stability functions from Canuto B'
         rs0 = 1.5_wp * rm1 * rm5*rm5
         rs1 = -rm4 * (rm6+rm7) + 2._wp * rm4*rm5*(rm1-(1._wp/3._wp)*rm2-rm3) + 1.5_wp * rm1*rm5*rm8
         rs2 = -(3._wp/8._wp) * rm1 * (rm6*rm6-rm7*rm7 )
         rs4 = 2._wp * rm5
         rs5 = 2._wp * rm4
         rs6 = (2._wp/3._wp) * rm5 * (3._wp*rm3*rm3-rm2*rm2) - 0.5_wp * rm5*rm1*(3._wp*rm3-rm2) + 0.75_wp * rm1*(rm6-rm7)
         rd0 = 3._wp * rm5*rm5
         rd1 = rm5 * (7._wp*rm4 + 3._wp*rm8)
         rd2 = rm5*rm5 * (3._wp*rm3*rm3 - rm2*rm2) - 0.75_wp * (rm6*rm6 - rm7*rm7)
         rd3 = rm4 * ( 4._wp*rm4 + 3._wp*rm8 )
         rd4 = rm4 * ( rm2*rm6 -3._wp*rm3*rm7 - rm5*(rm2*rm2 - rm3*rm3) ) + rm5 * rm8 * ( 3._wp*rm3*rm3 - rm2*rm2 )
         rd5 = 0.25_wp * ( rm2*rm2 - 3._wp*rm3*rm3 ) * ( rm6*rm6 - rm7*rm7 )
         rc0 = 0.5268_wp            !!       rc0 = 0.5540_wp (Warner ...) to verify !
         rf6 = 8._wp / ( rc0**6._wp )
         rc_diff = SQRT(2._wp)/(rc0**3.)
         rcm_sf  =  0.7470_wp
         rghmin  = -0.28_wp
         rgh0    =  0.0444_wp
         rghcri  =  0.0414_wp
         !
      END SELECT
    
      !                                !* Set Schmidt number for psi diffusion in the wave breaking case
      !                                     ! See Eq. (13) of Carniel et al, OM, 30, 225-239, 2009
      !                                     !  or Eq. (17) of Burchard, JPO, 31, 3133-3145, 2001
      IF( ln_sigpsi .AND. ln_crban ) THEN
         zcr = SQRT( 1.5_wp*rsc_tke ) * rcm_sf / vkarmn
         rsc_psi0 = vkarmn*vkarmn / ( rpsi2 * rcm_sf*rcm_sf )                       & 
        &         * ( rnn*rnn - 4._wp/3._wp * zcr*rnn*rmm - 1._wp/3._wp * zcr*rnn   &
        &           + 2._wp/9._wp * rmm * zcr*zcr + 4._wp/9._wp * zcr*zcr * rmm*rmm )                                 
      ELSE
         rsc_psi0 = rsc_psi
      ENDIF
 
      !                                !* Shear free turbulence parameters
      !
      ra_sf  = -4._wp * rnn * SQRT( rsc_tke ) / ( (1._wp+4._wp*rmm) * SQRT( rsc_tke )   &
         &                                      - SQRT(rsc_tke + 24._wp*rsc_psi0*rpsi2 ) )
      rl_sf  = rc0 * SQRT( rc0 / rcm_sf )                                                                   &
         &         * SQRT(  (  (1._wp + 4._wp*rmm + 8._wp*rmm*rmm) * rsc_tke                                &
         &                   + 12._wp * rsc_psi0 * rpsi2                                                    &
         &                   - (1._wp + 4._wp*rmm) * SQRT( rsc_tke*(rsc_tke+ 24._wp*rsc_psi0*rpsi2) )  )    &
         &                / ( 12._wp*rnn*rnn )                                                              )

      !
      IF(lwp) THEN                     !* Control print
         WRITE(numout,*)
         WRITE(numout,*) 'Limit values'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) 'Parameter  m = ',rmm
         WRITE(numout,*) 'Parameter  n = ',rnn
         WRITE(numout,*) 'Parameter  p = ',rpp
         WRITE(numout,*) 'rpsi1   = ',rpsi1
         WRITE(numout,*) 'rpsi2   = ',rpsi2
         WRITE(numout,*) 'rpsi3m  = ',rpsi3m
         WRITE(numout,*) 'rpsi3p  = ',rpsi3p
         WRITE(numout,*) 'rsc_tke = ',rsc_tke
         WRITE(numout,*) 'rsc_psi = ',rsc_psi
         WRITE(numout,*) 'rsc_psi0 = ',rsc_psi0
         WRITE(numout,*) 'rc0     = ',rc0
         WRITE(numout,*)
         WRITE(numout,*) 'Shear free turbulence parameters:'
         WRITE(numout,*) 'rcm_sf  = ',rcm_sf
         WRITE(numout,*) 'ra_sf   = ',ra_sf
         WRITE(numout,*) 'rl_sf   = ',rl_sf
         WRITE(numout,*)
      ENDIF

      !                                !* Constants initialization
      rc02  = rc0  * rc0   ;   rc02r = 1. / rc02
      rc03  = rc02 * rc0
      rc04  = rc03 * rc0
      rc03_sqrt2_galp = rc03 / SQRT(2._wp) / rn_clim_galp
      rsbc_mb   = 0.5_wp * (15.8_wp*rn_crban)**(2._wp/3._wp)               ! Surf. bound. cond. from Mellor and Blumberg
      rsbc_std  = 3.75_wp                                                  ! Surf. bound. cond. standard (prod=diss)
      rsbc_tke1 = (-rsc_tke*rn_crban/(rcm_sf*ra_sf*rl_sf))**(2._wp/3._wp)  ! k_eps = 53.  Dirichlet + Wave breaking 
      rsbc_tke2 = 0.5_wp / rau0
      rsbc_tke3 = rdt * rn_crban                                                         ! Neumann + Wave breaking
      rsbc_zs   = rn_charn / grav                                                        ! Charnock formula
      rsbc_psi1 = rc0**rpp * rsbc_tke1**rmm * rl_sf**rnn                           ! Dirichlet + Wave breaking
      rsbc_psi2 = -0.5_wp * rdt * rc0**rpp * rnn * vkarmn**rnn / rsc_psi                   ! Neumann + NO Wave breaking 
      rsbc_psi3 = -0.5_wp * rdt * rc0**rpp * rl_sf**rnn / rsc_psi  * (rnn + rmm*ra_sf) ! Neumann + Wave breaking
      rfact_tke = -0.5_wp / rsc_tke * rdt               ! Cst used for the Diffusion term of tke
      rfact_psi = -0.5_wp / rsc_psi * rdt               ! Cst used for the Diffusion term of tke

      !                                !* Wall proximity function
      zwall (:,:,:) = 1._wp * tmask(:,:,:)

      !                                !* set vertical eddy coef. to the background value
      DO jk = 1, jpk
         avt (:,:,jk) = avtb(jk) * tmask(:,:,jk)
         avm (:,:,jk) = avmb(jk) * tmask(:,:,jk)
         avmu(:,:,jk) = avmb(jk) * umask(:,:,jk)
         avmv(:,:,jk) = avmb(jk) * vmask(:,:,jk)
      END DO
      !                              
      CALL gls_rst( nit000, 'READ' )   !* read or initialize all required files
      !
      IF( nn_timing == 1 )  CALL timing_stop('zdf_gls_init')
      !
   END SUBROUTINE zdf_gls_init


   SUBROUTINE gls_rst( kt, cdrw )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE ts_rst  ***
      !!                     
      !! ** Purpose :   Read or write TKE file (en) in restart file
      !!
      !! ** Method  :   use of IOM library
      !!                if the restart does not contain TKE, en is either 
      !!                set to rn_emin or recomputed (nn_igls/=0)
      !!----------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   kt         ! ocean time-step
      CHARACTER(len=*), INTENT(in) ::   cdrw       ! "READ"/"WRITE" flag
      !
      INTEGER ::   jit, jk   ! dummy loop indices
      INTEGER ::   id1, id2, id3, id4, id5, id6
      INTEGER ::   ji, jj, ikbu, ikbv
      REAL(wp)::   cbx, cby
      !!----------------------------------------------------------------------
      !
      IF( TRIM(cdrw) == 'READ' ) THEN        ! Read/initialise 
         !                                   ! ---------------
         IF( ln_rstart ) THEN                   !* Read the restart file
            id1 = iom_varid( numror, 'en'   , ldstop = .FALSE. )
            id2 = iom_varid( numror, 'avt'  , ldstop = .FALSE. )
            id3 = iom_varid( numror, 'avm'  , ldstop = .FALSE. )
            id4 = iom_varid( numror, 'avmu' , ldstop = .FALSE. )
            id5 = iom_varid( numror, 'avmv' , ldstop = .FALSE. )
            id6 = iom_varid( numror, 'mxln' , ldstop = .FALSE. )
            !
            IF( MIN( id1, id2, id3, id4, id5, id6 ) > 0 ) THEN        ! all required arrays exist
               CALL iom_get( numror, jpdom_autoglo, 'en'    , en     )
               CALL iom_get( numror, jpdom_autoglo, 'avt'   , avt    )
               CALL iom_get( numror, jpdom_autoglo, 'avm'   , avm    )
               CALL iom_get( numror, jpdom_autoglo, 'avmu'  , avmu   )
               CALL iom_get( numror, jpdom_autoglo, 'avmv'  , avmv   )
               CALL iom_get( numror, jpdom_autoglo, 'mxln'  , mxln   )
            ELSE                        
               IF(lwp) WRITE(numout,*) ' ===>>>> : previous run without gls scheme, en and mxln computed by iterative loop'
               en  (:,:,:) = rn_emin
               mxln(:,:,:) = 0.001        
               DO jit = nit000 + 1, nit000 + 10   ;   CALL zdf_gls( jit )   ;   END DO
            ENDIF
         ELSE                                   !* Start from rest
            IF(lwp) WRITE(numout,*) ' ===>>>> : Initialisation of en and mxln by background values'
            en  (:,:,:) = rn_emin
            mxln(:,:,:) = 0.001       
         ENDIF
         !
      ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN   ! Create restart file
         !                                   ! -------------------
         IF(lwp) WRITE(numout,*) '---- gls-rst ----'
         CALL iom_rstput( kt, nitrst, numrow, 'en'   , en    )
         CALL iom_rstput( kt, nitrst, numrow, 'avt'  , avt_k  )
         CALL iom_rstput( kt, nitrst, numrow, 'avm'  , avm_k  )
         CALL iom_rstput( kt, nitrst, numrow, 'avmu' , avmu_k )
         CALL iom_rstput( kt, nitrst, numrow, 'avmv' , avmv_k )
         CALL iom_rstput( kt, nitrst, numrow, 'mxln' , mxln  )
         !
      ENDIF
      !
   END SUBROUTINE gls_rst

#else
   !!----------------------------------------------------------------------
   !!   Dummy module :                                        NO TKE scheme
   !!----------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_zdfgls = .FALSE.   !: TKE flag
CONTAINS
   SUBROUTINE zdf_gls_init           ! Empty routine
      WRITE(*,*) 'zdf_gls_init: You should not have seen this print! error?'
   END SUBROUTINE zdf_gls_init
   SUBROUTINE zdf_gls( kt )          ! Empty routine
      WRITE(*,*) 'zdf_gls: You should not have seen this print! error?', kt
   END SUBROUTINE zdf_gls
   SUBROUTINE gls_rst( kt, cdrw )          ! Empty routine
      INTEGER         , INTENT(in) ::   kt         ! ocean time-step
      CHARACTER(len=*), INTENT(in) ::   cdrw       ! "READ"/"WRITE" flag
      WRITE(*,*) 'gls_rst: You should not have seen this print! error?', kt, cdrw
   END SUBROUTINE gls_rst
#endif

   !!======================================================================
END MODULE zdfgls

