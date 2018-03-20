MODULE zdftke
   !!======================================================================
   !!                       ***  MODULE  zdftke  ***
   !! Ocean physics:  vertical mixing coefficient computed from the tke 
   !!                 turbulent closure parameterization
   !!=====================================================================
   !! History :  OPA  !  1991-03  (b. blanke)  Original code
   !!            7.0  !  1991-11  (G. Madec)   bug fix
   !!            7.1  !  1992-10  (G. Madec)   new mixing length and eav
   !!            7.2  !  1993-03  (M. Guyon)   symetrical conditions
   !!            7.3  !  1994-08  (G. Madec, M. Imbard)  nn_pdl flag
   !!            7.5  !  1996-01  (G. Madec)   s-coordinates
   !!            8.0  !  1997-07  (G. Madec)   lbc
   !!            8.1  !  1999-01  (E. Stretta) new option for the mixing length
   !!  NEMO      1.0  !  2002-06  (G. Madec) add tke_init routine
   !!             -   !  2004-10  (C. Ethe )  1D configuration
   !!            2.0  !  2006-07  (S. Masson)  distributed restart using iom
   !!            3.0  !  2008-05  (C. Ethe,  G.Madec) : update TKE physics:
   !!                 !           - tke penetration (wind steering)
   !!                 !           - suface condition for tke & mixing length
   !!                 !           - Langmuir cells
   !!             -   !  2008-05  (J.-M. Molines, G. Madec)  2D form of avtb
   !!             -   !  2008-06  (G. Madec)  style + DOCTOR name for namelist parameters
   !!             -   !  2008-12  (G. Reffray) stable discretization of the production term 
   !!            3.2  !  2009-06  (G. Madec, S. Masson) TKE restart compatible with key_cpl 
   !!                 !                                + cleaning of the parameters + bugs correction
   !!            3.3  !  2010-10  (C. Ethe, G. Madec) reorganisation of initialisation phase
   !!----------------------------------------------------------------------
#if defined key_zdftke   ||   defined key_esopa
   !!----------------------------------------------------------------------
   !!   'key_zdftke'                                   TKE vertical physics
   !!----------------------------------------------------------------------
   !!   zdf_tke      : update momentum and tracer Kz from a tke scheme
   !!   tke_tke      : tke time stepping: update tke at now time step (en)
   !!   tke_avn      : compute mixing length scale and deduce avm and avt
   !!   zdf_tke_init : initialization, namelist read, and parameters control
   !!   tke_rst      : read/write tke restart in ocean restart file
   !!----------------------------------------------------------------------
   USE oce            ! ocean: dynamics and active tracers variables
   USE phycst         ! physical constants
   USE dom_oce        ! domain: ocean
   USE domvvl         ! domain: variable volume layer
   USE sbc_oce        ! surface boundary condition: ocean
   USE zdf_oce        ! vertical physics: ocean variables
   USE zdfmxl         ! vertical physics: mixed layer
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE prtctl         ! Print control
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE wrk_nemo       ! work arrays
   USE timing         ! Timing
#if defined key_agrif
   USE agrif_opa_interp
   USE agrif_opa_update
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_tke        ! routine called in step module
   PUBLIC   zdf_tke_init   ! routine called in opa module
   PUBLIC   tke_rst        ! routine called in step module

   LOGICAL , PUBLIC, PARAMETER ::   lk_zdftke = .TRUE.  !: TKE vertical mixing flag

   !                                      !!** Namelist  namzdf_tke  **
   LOGICAL  ::   ln_mxl0   = .FALSE.       ! mixing length scale surface value as function of wind stress or not
   INTEGER  ::   nn_mxl    =  2            ! type of mixing length (=0/1/2/3)
   REAL(wp) ::   rn_mxl0   = 0.04_wp       ! surface  min value of mixing length (kappa*z_o=0.4*0.1 m)  [m]
   INTEGER  ::   nn_pdl    =  1            ! Prandtl number or not (ratio avt/avm) (=0/1)
   REAL(wp) ::   rn_ediff  = 0.1_wp        ! coefficient for avt: avt=rn_ediff*mxl*sqrt(e)
   REAL(wp) ::   rn_ediss  = 0.7_wp        ! coefficient of the Kolmogoroff dissipation 
   REAL(wp) ::   rn_ebb    = 3.75_wp       ! coefficient of the surface input of tke
   REAL(wp) ::   rn_emin   = 0.7071e-6_wp  ! minimum value of tke           [m2/s2]
   REAL(wp) ::   rn_emin0  = 1.e-4_wp      ! surface minimum value of tke   [m2/s2]
   REAL(wp) ::   rn_bshear = 1.e-20_wp     ! background shear (>0) currently a numerical threshold (do not change it)
   INTEGER  ::   nn_etau   = 0             ! type of depth penetration of surface tke (=0/1/2/3)
   INTEGER  ::   nn_htau   = 0             ! type of tke profile of penetration (=0/1)
   REAL(wp) ::   rn_efr    = 1.0_wp        ! fraction of TKE surface value which penetrates in the ocean
   LOGICAL  ::   ln_lc     = .FALSE.       ! Langmuir cells (LC) as a source term of TKE or not
   REAL(wp) ::   rn_lc     = 0.15_wp       ! coef to compute vertical velocity of Langmuir cells

   REAL(wp) ::   ri_cri                    ! critic Richardson number (deduced from rn_ediff and rn_ediss values)
   REAL(wp) ::   rmxl_min                  ! minimum mixing length value (deduced from rn_ediff and rn_emin values)  [m]
   REAL(wp) ::   rhftau_add = 1.e-3_wp     ! add offset   applied to HF part of taum  (nn_etau=3)
   REAL(wp) ::   rhftau_scl = 1.0_wp       ! scale factor applied to HF part of taum  (nn_etau=3)

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   en             !: now turbulent kinetic energy   [m2/s2]
   REAL(wp)        , ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   htau           ! depth of tke penetration (nn_htau)
   REAL(wp)        , ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   dissl          ! now mixing lenght of dissipation
#if defined key_c1d
   !                                                                        !!** 1D cfg only  **   ('key_c1d')
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e_dis, e_mix   !: dissipation and mixing turbulent lengh scales
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e_pdl, e_ric   !: prandl and local Richardson numbers
#endif
   REAL(wp)        , ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   wei3d          ! 
   REAL(wp)        , ALLOCATABLE, SAVE, DIMENSION(:,:  ) ::   wmix           ! 

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: zdftke.F90 3406 2012-06-05 16:32:34Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION zdf_tke_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION zdf_tke_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE(                                                                    &
#if defined key_c1d
         &      e_dis(jpi,jpj,jpk) , e_mix(jpi,jpj,jpk) ,                          &
         &      e_pdl(jpi,jpj,jpk) , e_ric(jpi,jpj,jpk) ,                          &
#endif
         &      en    (jpi,jpj,jpk) , htau  (jpi,jpj)    , dissl(jpi,jpj,jpk) ,    & 
         &      STAT= zdf_tke_alloc      )
         !
      IF( lk_mpp             )   CALL mpp_sum ( zdf_tke_alloc )
      IF( zdf_tke_alloc /= 0 )   CALL ctl_warn('zdf_tke_alloc: failed to allocate arrays')
      !
      IF(.NOT. Agrif_Root()) THEN
         ALLOCATE( wei3d(jpi,jpj,jpk), wmix(jpi,jpj), STAT= zdf_tke_alloc )
         IF( lk_mpp             )   CALL mpp_sum ( zdf_tke_alloc )
         IF( zdf_tke_alloc /= 0 )   CALL ctl_warn('zdf_tke_alloc2: failed to allocate arrays')
      ENDIF
      !
   END FUNCTION zdf_tke_alloc


   SUBROUTINE zdf_tke( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE zdf_tke  ***
      !!
      !! ** Purpose :   Compute the vertical eddy viscosity and diffusivity
      !!              coefficients using a turbulent closure scheme (TKE).
      !!
      !! ** Method  :   The time evolution of the turbulent kinetic energy (tke)
      !!              is computed from a prognostic equation :
      !!         d(en)/dt = avm (d(u)/dz)**2             ! shear production
      !!                  + d( avm d(en)/dz )/dz         ! diffusion of tke
      !!                  + avt N^2                      ! stratif. destruc.
      !!                  - rn_ediss / emxl en**(2/3)    ! Kolmogoroff dissipation
      !!      with the boundary conditions:
      !!         surface: en = max( rn_emin0, rn_ebb * taum )
      !!         bottom : en = rn_emin
      !!      The associated critical Richardson number is: ri_cri = 2/(2+rn_ediss/rn_ediff) 
      !!
      !!        The now Turbulent kinetic energy is computed using the following 
      !!      time stepping: implicit for vertical diffusion term, linearized semi
      !!      implicit for kolmogoroff dissipation term, and explicit forward for 
      !!      both buoyancy and shear production terms. Therefore a tridiagonal 
      !!      linear system is solved. Note that buoyancy and shear terms are
      !!      discretized in a energy conserving form (Bruchard 2002).
      !!
      !!        The dissipative and mixing length scale are computed from en and
      !!      the stratification (see tke_avn)
      !!
      !!        The now vertical eddy vicosity and diffusivity coefficients are
      !!      given by: 
      !!              avm = max( avtb, rn_ediff * zmxlm * en^1/2 )
      !!              avt = max( avmb, pdl * avm                 )  
      !!              eav = max( avmb, avm )
      !!      where pdl, the inverse of the Prandtl number is 1 if nn_pdl=0 and
      !!      given by an empirical funtion of the localRichardson number if nn_pdl=1 
      !!
      !! ** Action  :   compute en (now turbulent kinetic energy)
      !!                update avt, avmu, avmv (before vertical eddy coef.)
      !!
      !! References : Gaspar et al., JGR, 1990,
      !!              Blanke and Delecluse, JPO, 1991
      !!              Mellor and Blumberg, JPO 2004
      !!              Axell, JGR, 2002
      !!              Bruchard OM 2002
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      !!----------------------------------------------------------------------
      !
      IF( kt /= nit000 ) THEN   ! restore before value to compute tke
#if defined key_agrif 
         ! interpolation parent grid => child grid for avt_k, avm_k, avmu_k, avmv_k (at west border: update column 1 and 2)
         CALL Agrif_Tke   
#endif
         avt (:,:,:) = avt_k (:,:,:) 
         avm (:,:,:) = avm_k (:,:,:) 
         avmu(:,:,:) = avmu_k(:,:,:) 
         avmv(:,:,:) = avmv_k(:,:,:) 
      ENDIF 
      !
      CALL tke_tke      ! now tke (en)
      !
      CALL tke_avn      ! now avt, avm, avmu, avmv
      !
      avt_k (:,:,:) = avt (:,:,:) 
      avm_k (:,:,:) = avm (:,:,:) 
      avmu_k(:,:,:) = avmu(:,:,:) 
      avmv_k(:,:,:) = avmv(:,:,:) 
      !
#if defined key_agrif
      ! Update child grid f => parent grid 
      IF( .NOT.Agrif_Root() )    CALL Agrif_Update_Tke( kt )      ! children only
#endif      


   END SUBROUTINE zdf_tke


   SUBROUTINE tke_tke
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE tke_tke  ***
      !!
      !! ** Purpose :   Compute the now Turbulente Kinetic Energy (TKE)
      !!
      !! ** Method  : - TKE surface boundary condition
      !!              - source term due to Langmuir cells (Axell JGR 2002) (ln_lc=T)
      !!              - source term due to shear (saved in avmu, avmv arrays)
      !!              - Now TKE : resolution of the TKE equation by inverting
      !!                a tridiagonal linear system by a "methode de chasse"
      !!              - increase TKE due to surface and internal wave breaking
      !!
      !! ** Action  : - en : now turbulent kinetic energy)
      !!              - avmu, avmv : production of TKE by shear at u and v-points
      !!                (= Kz dz[Ub] * dz[Un] )
      !! ---------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk                      ! dummy loop arguments
!!bfr      INTEGER  ::   ikbu, ikbv, ikbum1, ikbvm1      ! temporary scalar
!!bfr      INTEGER  ::   ikbt, ikbumm1, ikbvmm1          ! temporary scalar
      REAL(wp) ::   zrhoa  = 1.22                   ! Air density kg/m3
      REAL(wp) ::   zcdrag = 1.5e-3                 ! drag coefficient
      REAL(wp) ::   zbbrau, zesh2                   ! temporary scalars
      REAL(wp) ::   zfact1, zfact2, zfact3          !    -         -
      REAL(wp) ::   ztx2  , zty2  , zcof            !    -         -
      REAL(wp) ::   ztau  , zdif                    !    -         -
      REAL(wp) ::   zus   , zwlc  , zind            !    -         -
      REAL(wp) ::   zzd_up, zzd_lw                  !    -         -
!!bfr      REAL(wp) ::   zebot                           !    -         -
      INTEGER , POINTER, DIMENSION(:,:  ) :: imlc
      REAL(wp), POINTER, DIMENSION(:,:  ) :: zhlc
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zpelc, zdiag, zd_up, zd_lw
      !!--------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('tke_tke')
      !
      CALL wrk_alloc( jpi,jpj, imlc )    ! integer
      CALL wrk_alloc( jpi,jpj, zhlc ) 
      CALL wrk_alloc( jpi,jpj,jpk, zpelc, zdiag, zd_up, zd_lw ) 
      !
      zbbrau = rn_ebb / rau0       ! Local constant initialisation
      zfact1 = -.5_wp * rdt 
      zfact2 = 1.5_wp * rdt * rn_ediss
      zfact3 = 0.5_wp       * rn_ediss
      !
      !                     !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !                     !  Surface boundary condition on tke
      !                     !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO jj = 2, jpjm1            ! en(1)   = rn_ebb taum / rau0  (min value rn_emin0)
         DO ji = fs_2, fs_jpim1   ! vector opt.
            en(ji,jj,1) = MAX( rn_emin0, zbbrau * taum(ji,jj) ) * tmask(ji,jj,1)
         END DO
      END DO
      
!!bfr   - start commented area
      !                     !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !                     !  Bottom boundary condition on tke
      !                     !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ! Tests to date have found the bottom boundary condition on tke to have very little effect.
      ! The condition is coded here for completion but commented out until there is proof that the
      ! computational cost is justified
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !                     en(bot)   = (rn_ebb0/rau0)*0.5*sqrt(u_botfr^2+v_botfr^2) (min value rn_emin)
!CDIR NOVERRCHK
!!    DO jj = 2, jpjm1
!CDIR NOVERRCHK
!!       DO ji = fs_2, fs_jpim1   ! vector opt.
!!          ztx2 = bfrua(ji-1,jj) * ub(ji-1,jj,mbku(ji-1,jj)) + &
!!                 bfrua(ji  ,jj) * ub(ji  ,jj,mbku(ji  ,jj) )
!!          zty2 = bfrva(ji,jj  ) * vb(ji,jj  ,mbkv(ji,jj  )) + &
!!                 bfrva(ji,jj-1) * vb(ji,jj-1,mbkv(ji,jj-1) )
!!          zebot = 0.001875_wp * SQRT( ztx2 * ztx2 + zty2 * zty2 )   !  where 0.001875 = (rn_ebb0/rau0) * 0.5 = 3.75*0.5/1000.
!!          en (ji,jj,mbkt(ji,jj)+1) = MAX( zebot, rn_emin ) * tmask(ji,jj,1)
!!       END DO
!!    END DO
!!bfr   - end commented area
      !
      !                     !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      IF( ln_lc ) THEN      !  Langmuir circulation source term added to tke       (Axell JGR 2002)
         !                  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         !
         !                        !* total energy produce by LC : cumulative sum over jk
         zpelc(:,:,1) =  MAX( rn2b(:,:,1), 0._wp ) * fsdepw(:,:,1) * fse3w(:,:,1)
         DO jk = 2, jpk
            zpelc(:,:,jk)  = zpelc(:,:,jk-1) + MAX( rn2b(:,:,jk), 0._wp ) * fsdepw(:,:,jk) * fse3w(:,:,jk)
         END DO
         !                        !* finite Langmuir Circulation depth
         zcof = 0.5 * 0.016 * 0.016 / ( zrhoa * zcdrag )
         imlc(:,:) = mbkt(:,:) + 1       ! Initialization to the number of w ocean point (=2 over land)
         DO jk = jpkm1, 2, -1
            DO jj = 1, jpj               ! Last w-level at which zpelc>=0.5*us*us 
               DO ji = 1, jpi            !      with us=0.016*wind(starting from jpk-1)
                  zus  = zcof * taum(ji,jj)
                  IF( zpelc(ji,jj,jk) > zus )   imlc(ji,jj) = jk
               END DO
            END DO
         END DO
         !                               ! finite LC depth
# if defined key_vectopt_loop
         DO jj = 1, 1
            DO ji = 1, jpij   ! vector opt. (forced unrolling)
# else
         DO jj = 1, jpj 
            DO ji = 1, jpi
# endif
               zhlc(ji,jj) = fsdepw(ji,jj,imlc(ji,jj))
            END DO
         END DO
         zcof = 0.016 / SQRT( zrhoa * zcdrag )
!CDIR NOVERRCHK
         DO jk = 2, jpkm1         !* TKE Langmuir circulation source term added to en
!CDIR NOVERRCHK
            DO jj = 2, jpjm1
!CDIR NOVERRCHK
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zus  = zcof * SQRT( taum(ji,jj) )           ! Stokes drift
                  !                                           ! vertical velocity due to LC
                  zind = 0.5 - SIGN( 0.5, fsdepw(ji,jj,jk) - zhlc(ji,jj) )
                  zwlc = zind * rn_lc * zus * SIN( rpi * fsdepw(ji,jj,jk) / zhlc(ji,jj) )
                  !                                           ! TKE Langmuir circulation source term
                  en(ji,jj,jk) = en(ji,jj,jk) + rdt * ( zwlc * zwlc * zwlc ) / zhlc(ji,jj) * tmask(ji,jj,jk)
               END DO
            END DO
         END DO
         !
      ENDIF
      !
      !                     !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !                     !  Now Turbulent kinetic energy (output in en)
      !                     !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !                     ! Resolution of a tridiagonal linear system by a "methode de chasse"
      !                     ! computation from level 2 to jpkm1  (e(1) already computed and e(jpk)=0 ).
      !                     ! zdiag : diagonal zd_up : upper diagonal zd_lw : lower diagonal
      !
      DO jk = 2, jpkm1           !* Shear production at uw- and vw-points (energy conserving form)
         DO jj = 1, jpj                 ! here avmu, avmv used as workspace
            DO ji = 1, jpi
               avmu(ji,jj,jk) = avmu(ji,jj,jk) * ( un(ji,jj,jk-1) - un(ji,jj,jk) )   &
                  &                            * ( ub(ji,jj,jk-1) - ub(ji,jj,jk) )   & 
                  &           / (  fse3uw_n(ji,jj,jk)         &
                  &              * fse3uw_b(ji,jj,jk) )
               avmv(ji,jj,jk) = avmv(ji,jj,jk) * ( vn(ji,jj,jk-1) - vn(ji,jj,jk) )   &
                  &                            * ( vb(ji,jj,jk-1) - vb(ji,jj,jk) )   &
                  &                            / (  fse3vw_n(ji,jj,jk)               &
                  &                              *  fse3vw_b(ji,jj,jk)  )
            END DO
         END DO
      END DO
      !
      DO jk = 2, jpkm1           !* Matrix and right hand side in en
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zcof   = zfact1 * tmask(ji,jj,jk)
               zzd_up = zcof * ( avm  (ji,jj,jk+1) + avm  (ji,jj,jk  ) )   &  ! upper diagonal
                  &          / ( fse3t(ji,jj,jk  ) * fse3w(ji,jj,jk  ) )
               zzd_lw = zcof * ( avm  (ji,jj,jk  ) + avm  (ji,jj,jk-1) )   &  ! lower diagonal
                  &          / ( fse3t(ji,jj,jk-1) * fse3w(ji,jj,jk  ) )
                  !                                                           ! shear prod. at w-point weightened by mask
               zesh2  =  ( avmu(ji-1,jj,jk) + avmu(ji,jj,jk) ) / MAX( 1._wp , umask(ji-1,jj,jk) + umask(ji,jj,jk) )   &
                  &    + ( avmv(ji,jj-1,jk) + avmv(ji,jj,jk) ) / MAX( 1._wp , vmask(ji,jj-1,jk) + vmask(ji,jj,jk) )    
                  !
               zd_up(ji,jj,jk) = zzd_up            ! Matrix (zdiag, zd_up, zd_lw)
               zd_lw(ji,jj,jk) = zzd_lw
               zdiag(ji,jj,jk) = 1._wp - zzd_lw - zzd_up + zfact2 * dissl(ji,jj,jk) * tmask(ji,jj,jk)
               !
               !                                   ! right hand side in en
               en(ji,jj,jk) = en(ji,jj,jk) + rdt * (  zesh2  -   avt(ji,jj,jk) * rn2(ji,jj,jk)    &
                  &                                 + zfact3 * dissl(ji,jj,jk) * en (ji,jj,jk)  ) * tmask(ji,jj,jk)
            END DO
         END DO
      END DO
      !                          !* Matrix inversion from level 2 (tke prescribed at level 1)
      DO jk = 3, jpkm1                             ! First recurrence : Dk = Dk - Lk * Uk-1 / Dk-1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1    ! vector opt.
               zdiag(ji,jj,jk) = zdiag(ji,jj,jk) - zd_lw(ji,jj,jk) * zd_up(ji,jj,jk-1) / zdiag(ji,jj,jk-1)
            END DO
         END DO
      END DO
      DO jj = 2, jpjm1                             ! Second recurrence : Lk = RHSk - Lk / Dk-1 * Lk-1
         DO ji = fs_2, fs_jpim1    ! vector opt.
            zd_lw(ji,jj,2) = en(ji,jj,2) - zd_lw(ji,jj,2) * en(ji,jj,1)    ! Surface boudary conditions on tke
         END DO
      END DO
      DO jk = 3, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1    ! vector opt.
               zd_lw(ji,jj,jk) = en(ji,jj,jk) - zd_lw(ji,jj,jk) / zdiag(ji,jj,jk-1) *zd_lw(ji,jj,jk-1)
            END DO
         END DO
      END DO
      DO jj = 2, jpjm1                             ! thrid recurrence : Ek = ( Lk - Uk * Ek+1 ) / Dk
         DO ji = fs_2, fs_jpim1    ! vector opt.
            en(ji,jj,jpkm1) = zd_lw(ji,jj,jpkm1) / zdiag(ji,jj,jpkm1)
         END DO
      END DO
      DO jk = jpk-2, 2, -1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1    ! vector opt.
               en(ji,jj,jk) = ( zd_lw(ji,jj,jk) - zd_up(ji,jj,jk) * en(ji,jj,jk+1) ) / zdiag(ji,jj,jk)
            END DO
         END DO
      END DO
      DO jk = 2, jpkm1                             ! set the minimum value of tke
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               en(ji,jj,jk) = MAX( en(ji,jj,jk), rn_emin ) * tmask(ji,jj,jk)
            END DO
         END DO
      END DO

      !                            !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !                            !  TKE due to surface and internal wave breaking
      !                            !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      IF( nn_etau == 1 ) THEN           !* penetration below the mixed layer (rn_efr fraction)
         DO jk = 2, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  en(ji,jj,jk) = en(ji,jj,jk) + rn_efr * en(ji,jj,1) * EXP( -fsdepw(ji,jj,jk) / htau(ji,jj) )   &
                     &                                               * ( 1._wp - fr_i(ji,jj) )  * tmask(ji,jj,jk)
               END DO
            END DO
         END DO
      ELSEIF( nn_etau == 2 ) THEN       !* act only at the base of the mixed layer (jk=nmln)  (rn_efr fraction)
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               jk = nmln(ji,jj)
               en(ji,jj,jk) = en(ji,jj,jk) + rn_efr * en(ji,jj,1) * EXP( -fsdepw(ji,jj,jk) / htau(ji,jj) )   &
                  &                                               * ( 1._wp - fr_i(ji,jj) )  * tmask(ji,jj,jk)
            END DO
         END DO
      ELSEIF( nn_etau == 3 ) THEN       !* penetration belox the mixed layer (HF variability)
!CDIR NOVERRCHK
         DO jk = 2, jpkm1
!CDIR NOVERRCHK
            DO jj = 2, jpjm1
!CDIR NOVERRCHK
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  ztx2 = utau(ji-1,jj  ) + utau(ji,jj)
                  zty2 = vtau(ji  ,jj-1) + vtau(ji,jj)
                  ztau = 0.5_wp * SQRT( ztx2 * ztx2 + zty2 * zty2 )    ! module of the mean stress 
                  zdif = taum(ji,jj) - ztau                            ! mean of modulus - modulus of the mean 
                  zdif = rhftau_scl * MAX( 0._wp, zdif + rhftau_add )  ! apply some modifications...
                  en(ji,jj,jk) = en(ji,jj,jk) + zbbrau * zdif * EXP( -fsdepw(ji,jj,jk) / htau(ji,jj) )   &
                     &                                        * ( 1._wp - fr_i(ji,jj) ) * tmask(ji,jj,jk)
               END DO
            END DO
         END DO
      ENDIF
      CALL lbc_lnk( en, 'W', 1. )      ! Lateral boundary conditions (sign unchanged)
      !
      CALL wrk_dealloc( jpi,jpj, imlc )    ! integer
      CALL wrk_dealloc( jpi,jpj, zhlc ) 
      CALL wrk_dealloc( jpi,jpj,jpk, zpelc, zdiag, zd_up, zd_lw ) 
      !
      IF( nn_timing == 1 )  CALL timing_stop('tke_tke')
      !
   END SUBROUTINE tke_tke


   SUBROUTINE tke_avn
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE tke_avn  ***
      !!
      !! ** Purpose :   Compute the vertical eddy viscosity and diffusivity
      !!
      !! ** Method  :   At this stage, en, the now TKE, is known (computed in 
      !!              the tke_tke routine). First, the now mixing lenth is 
      !!      computed from en and the strafification (N^2), then the mixings
      !!      coefficients are computed.
      !!              - Mixing length : a first evaluation of the mixing lengh
      !!      scales is:
      !!                      mxl = sqrt(2*en) / N  
      !!      where N is the brunt-vaisala frequency, with a minimum value set
      !!      to rmxl_min (rn_mxl0) in the interior (surface) ocean.
      !!        The mixing and dissipative length scale are bound as follow : 
      !!         nn_mxl=0 : mxl bounded by the distance to surface and bottom.
      !!                        zmxld = zmxlm = mxl
      !!         nn_mxl=1 : mxl bounded by the e3w and zmxld = zmxlm = mxl
      !!         nn_mxl=2 : mxl bounded such that the vertical derivative of mxl is 
      !!                    less than 1 (|d/dz(mxl)|<1) and zmxld = zmxlm = mxl
      !!         nn_mxl=3 : mxl is bounded from the surface to the bottom usings
      !!                    |d/dz(xml)|<1 to obtain lup, and from the bottom to 
      !!                    the surface to obtain ldown. the resulting length 
      !!                    scales are:
      !!                        zmxld = sqrt( lup * ldown ) 
      !!                        zmxlm = min ( lup , ldown )
      !!              - Vertical eddy viscosity and diffusivity:
      !!                      avm = max( avtb, rn_ediff * zmxlm * en^1/2 )
      !!                      avt = max( avmb, pdlr * avm )  
      !!      with pdlr=1 if nn_pdl=0, pdlr=1/pdl=F(Ri) otherwise.
      !!
      !! ** Action  : - avt : now vertical eddy diffusivity (w-point)
      !!              - avmu, avmv : now vertical eddy viscosity at uw- and vw-points
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zrn2, zraug, zcoef, zav     ! local scalars
      REAL(wp) ::   zdku, zpdlr, zri, zsqen     !   -      -
      REAL(wp) ::   zdkv, zemxl, zemlm, zemlp   !   -      -
      REAL(wp), POINTER, DIMENSION(:,:  ) :: ztmp2d
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zmpdl, zmxlm, zmxld
      !!--------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('tke_avn')

      CALL wrk_alloc( jpi,jpj, ztmp2d ) 
      CALL wrk_alloc( jpi,jpj,jpk, zmpdl, zmxlm, zmxld ) 

      !                     !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !                     !  Mixing length
      !                     !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      !                     !* Buoyancy length scale: l=sqrt(2*e/n**2)
      !
      IF( ln_mxl0 ) THEN            ! surface mixing length = F(stress) : l=vkarmn*2.e5*taum/(rau0*g)
         zraug = vkarmn * 2.e5_wp / ( rau0 * grav )
         zmxlm(:,:,1) = MAX(  rn_mxl0,  zraug * taum(:,:)  )
      ELSE                          ! surface set to the minimum value
         zmxlm(:,:,1) = rn_mxl0
      ENDIF
      zmxlm(:,:,jpk)  = rmxl_min     ! last level set to the interior minium value
      !
!CDIR NOVERRCHK
      DO jk = 2, jpkm1              ! interior value : l=sqrt(2*e/n^2)
!CDIR NOVERRCHK
         DO jj = 2, jpjm1
!CDIR NOVERRCHK
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zrn2 = MAX( rn2(ji,jj,jk), rsmall )
               zmxlm(ji,jj,jk) = MAX(  rmxl_min,  SQRT( 2._wp * en(ji,jj,jk) / zrn2 )  )
            END DO
         END DO
      END DO
      !
      !                     !* Physical limits for the mixing length
      !
      zmxld(:,:, 1 ) = zmxlm(:,:,1)   ! surface set to the zmxlm   value
      zmxld(:,:,jpk) = rmxl_min       ! last level  set to the minimum value
      !
      SELECT CASE ( nn_mxl )
      !
      CASE ( 0 )           ! bounded by the distance to surface and bottom
         DO jk = 2, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zemxl = MIN( fsdepw(ji,jj,jk), zmxlm(ji,jj,jk),   &
                  &            fsdepw(ji,jj,mbkt(ji,jj)+1) - fsdepw(ji,jj,jk) )
                  zmxlm(ji,jj,jk) = zemxl
                  zmxld(ji,jj,jk) = zemxl
               END DO
            END DO
         END DO
         !
      CASE ( 1 )           ! bounded by the vertical scale factor
         DO jk = 2, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zemxl = MIN( fse3w(ji,jj,jk), zmxlm(ji,jj,jk) )
                  zmxlm(ji,jj,jk) = zemxl
                  zmxld(ji,jj,jk) = zemxl
               END DO
            END DO
         END DO
         !
      CASE ( 2 )           ! |dk[xml]| bounded by e3t :
         DO jk = 2, jpkm1         ! from the surface to the bottom :
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zmxlm(ji,jj,jk) = MIN( zmxlm(ji,jj,jk-1) + fse3t(ji,jj,jk-1), zmxlm(ji,jj,jk) )
               END DO
            END DO
         END DO
         DO jk = jpkm1, 2, -1     ! from the bottom to the surface :
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zemxl = MIN( zmxlm(ji,jj,jk+1) + fse3t(ji,jj,jk+1), zmxlm(ji,jj,jk) )
                  zmxlm(ji,jj,jk) = zemxl
                  zmxld(ji,jj,jk) = zemxl
               END DO
            END DO
         END DO
         !
      CASE ( 3 )           ! lup and ldown, |dk[xml]| bounded by e3t :
         DO jk = 2, jpkm1         ! from the surface to the bottom : lup
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zmxld(ji,jj,jk) = MIN( zmxld(ji,jj,jk-1) + fse3t(ji,jj,jk-1), zmxlm(ji,jj,jk) )
               END DO
            END DO
         END DO
         DO jk = jpkm1, 2, -1     ! from the bottom to the surface : ldown
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zmxlm(ji,jj,jk) = MIN( zmxlm(ji,jj,jk+1) + fse3t(ji,jj,jk+1), zmxlm(ji,jj,jk) )
               END DO
            END DO
         END DO
!CDIR NOVERRCHK
         DO jk = 2, jpkm1
!CDIR NOVERRCHK
            DO jj = 2, jpjm1
!CDIR NOVERRCHK
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zemlm = MIN ( zmxld(ji,jj,jk),  zmxlm(ji,jj,jk) )
                  zemlp = SQRT( zmxld(ji,jj,jk) * zmxlm(ji,jj,jk) )
                  zmxlm(ji,jj,jk) = zemlm
                  zmxld(ji,jj,jk) = zemlp
               END DO
            END DO
         END DO
         !
      END SELECT
      !
# if defined key_c1d
      e_dis(:,:,:) = zmxld(:,:,:)      ! c1d configuration : save mixing and dissipation turbulent length scales
      e_mix(:,:,:) = zmxlm(:,:,:)
# endif

      !                     !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !                     !  Vertical eddy viscosity and diffusivity  (avmu, avmv, avt)
      !                     !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!CDIR NOVERRCHK
      DO jk = 1, jpkm1            !* vertical eddy viscosity & diffivity at w-points
!CDIR NOVERRCHK
         DO jj = 2, jpjm1
!CDIR NOVERRCHK
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zsqen = SQRT( en(ji,jj,jk) )
               zav   = rn_ediff * zmxlm(ji,jj,jk) * zsqen
               avm  (ji,jj,jk) = MAX( zav,                  avmb(jk) ) * tmask(ji,jj,jk)
               avt  (ji,jj,jk) = MAX( zav, avtb_2d(ji,jj) * avtb(jk) ) * tmask(ji,jj,jk)
               dissl(ji,jj,jk) = zsqen / zmxld(ji,jj,jk)
            END DO
         END DO
      END DO
      CALL lbc_lnk( avm, 'W', 1. )      ! Lateral boundary conditions (sign unchanged)
      !
      IF(.NOT. Agrif_Root()) THEN
         
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = 2, jpim1
                  ztmp2d(ji,jj) = 1. * avm(ji-1,jj-1,jk) * tmask(ji-1,jj-1,jk)   &
                     &          + 2. * avm(ji  ,jj-1,jk) * tmask(ji  ,jj-1,jk)   &
                     &          + 1. * avm(ji+1,jj-1,jk) * tmask(ji+1,jj-1,jk)   &
                     &          + 2. * avm(ji-1,jj  ,jk) * tmask(ji-1,jj  ,jk)   &
                     &          + 4. * avm(ji  ,jj  ,jk) * tmask(ji  ,jj  ,jk)   &
                     &          + 2. * avm(ji+1,jj  ,jk) * tmask(ji+1,jj  ,jk)   &
                     &          + 1. * avm(ji-1,jj+1,jk) * tmask(ji-1,jj+1,jk)   &
                     &          + 2. * avm(ji  ,jj+1,jk) * tmask(ji  ,jj+1,jk)   &
                     &          + 1. * avm(ji+1,jj+1,jk) * tmask(ji+1,jj+1,jk)
               END DO
            END DO
            DO jj = 2, jpjm1
               DO ji = 2, jpim1
                  avm(ji,jj,jk) = ztmp2d(ji,jj) * wei3d(ji,jj,jk) * wmix(ji,jj) + avm(ji,jj,jk) * ( 1. - wmix(ji,jj) )
               END DO
            END DO
         END DO
         CALL lbc_lnk( avm, 'W', 1. )      ! Lateral boundary conditions (sign unchanged)
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = 2, jpim1
                  ztmp2d(ji,jj) = 1. * avt(ji-1,jj-1,jk) * tmask(ji-1,jj-1,jk)   &
                     &          + 2. * avt(ji  ,jj-1,jk) * tmask(ji  ,jj-1,jk)   &
                     &          + 1. * avt(ji+1,jj-1,jk) * tmask(ji+1,jj-1,jk)   &
                     &          + 2. * avt(ji-1,jj  ,jk) * tmask(ji-1,jj  ,jk)   &
                     &          + 4. * avt(ji  ,jj  ,jk) * tmask(ji  ,jj  ,jk)   &
                     &          + 2. * avt(ji+1,jj  ,jk) * tmask(ji+1,jj  ,jk)   &
                     &          + 1. * avt(ji-1,jj+1,jk) * tmask(ji-1,jj+1,jk)   &
                     &          + 2. * avt(ji  ,jj+1,jk) * tmask(ji  ,jj+1,jk)   &
                     &          + 1. * avt(ji+1,jj+1,jk) * tmask(ji+1,jj+1,jk)
               END DO
            END DO
            DO jj = 2, jpjm1
               DO ji = 2, jpim1
                  avt(ji,jj,jk) = ztmp2d(ji,jj) * wei3d(ji,jj,jk) * wmix(ji,jj) + avt(ji,jj,jk) * ( 1. - wmix(ji,jj) )
               END DO
            END DO
         END DO
         CALL lbc_lnk( avt, 'W', 1. )      ! Lateral boundary conditions (sign unchanged)

      END IF
      !
      DO jk = 2, jpkm1            !* vertical eddy viscosity at u- and v-points
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               avmu(ji,jj,jk) = 0.5 * ( avm(ji,jj,jk) + avm(ji+1,jj  ,jk) ) * umask(ji,jj,jk)
               avmv(ji,jj,jk) = 0.5 * ( avm(ji,jj,jk) + avm(ji  ,jj+1,jk) ) * vmask(ji,jj,jk)
            END DO
         END DO
      END DO
      CALL lbc_lnk( avmu, 'U', 1. )   ;   CALL lbc_lnk( avmv, 'V', 1. )      ! Lateral boundary conditions
      !
      IF( nn_pdl == 1 ) THEN      !* Prandtl number case: update avt
         DO jk = 2, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zcoef = avm(ji,jj,jk) * 2._wp * fse3w(ji,jj,jk) * fse3w(ji,jj,jk)
                  !                                          ! shear
                  zdku = avmu(ji-1,jj,jk) * ( un(ji-1,jj,jk-1) - un(ji-1,jj,jk) ) * ( ub(ji-1,jj,jk-1) - ub(ji-1,jj,jk) )   &
                    &  + avmu(ji  ,jj,jk) * ( un(ji  ,jj,jk-1) - un(ji  ,jj,jk) ) * ( ub(ji  ,jj,jk-1) - ub(ji  ,jj,jk) )
                  zdkv = avmv(ji,jj-1,jk) * ( vn(ji,jj-1,jk-1) - vn(ji,jj-1,jk) ) * ( vb(ji,jj-1,jk-1) - vb(ji,jj-1,jk) )   &
                    &  + avmv(ji,jj  ,jk) * ( vn(ji,jj  ,jk-1) - vn(ji,jj  ,jk) ) * ( vb(ji,jj  ,jk-1) - vb(ji,jj  ,jk) )
                  !                                          ! local Richardson number
                  zri   = MAX( rn2b(ji,jj,jk), 0._wp ) * zcoef / (zdku + zdkv + rn_bshear )
                  zpdlr = MAX(  0.1_wp,  0.2 / MAX( 0.2 , zri )  )
!!gm and even better with the use of the "true" ri_crit=0.22222...  (this change the results!)
!!gm              zpdlr = MAX(  0.1_wp,  ri_crit / MAX( ri_crit , zri )  )
                  avt(ji,jj,jk)   = MAX( zpdlr * avt(ji,jj,jk), avtb_2d(ji,jj) * avtb(jk) ) * tmask(ji,jj,jk)
# if defined key_c1d
                  e_pdl(ji,jj,jk) = zpdlr * tmask(ji,jj,jk)    ! c1d configuration : save masked Prandlt number
                  e_ric(ji,jj,jk) = zri * tmask(ji,jj,jk)                            ! c1d config. : save Ri
# endif
              END DO
            END DO
         END DO
      ENDIF
      CALL lbc_lnk( avt, 'W', 1. )                      ! Lateral boundary conditions on avt  (sign unchanged)
      !
      IF(ln_ctl) THEN
         CALL prt_ctl( tab3d_1=en  , clinfo1=' tke  - e: ', tab3d_2=avt, clinfo2=' t: ', ovlap=1, kdim=jpk)
         CALL prt_ctl( tab3d_1=avmu, clinfo1=' tke  - u: ', mask1=umask,                   &
            &          tab3d_2=avmv, clinfo2=       ' v: ', mask2=vmask, ovlap=1, kdim=jpk )
      ENDIF
      !
      CALL wrk_dealloc( jpi,jpj, ztmp2d ) 
      CALL wrk_dealloc( jpi,jpj,jpk, zmpdl, zmxlm, zmxld ) 
      !
      IF( nn_timing == 1 )  CALL timing_stop('tke_avn')
      !
   END SUBROUTINE tke_avn


   SUBROUTINE zdf_tke_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_tke_init  ***
      !!                     
      !! ** Purpose :   Initialization of the vertical eddy diffivity and 
      !!              viscosity when using a tke turbulent closure scheme
      !!
      !! ** Method  :   Read the namzdf_tke namelist and check the parameters
      !!              called at the first timestep (nit000)
      !!
      !! ** input   :   Namlist namzdf_tke
      !!
      !! ** Action  :   Increase by 1 the nstop flag is setting problem encounter
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      !!
      NAMELIST/namzdf_tke/ rn_ediff, rn_ediss , rn_ebb , rn_emin  ,   &
         &                 rn_emin0, rn_bshear, nn_mxl , ln_mxl0  ,   &
         &                 rn_mxl0 , nn_pdl   , ln_lc  , rn_lc    ,   &
         &                 nn_etau , nn_htau  , rn_efr   
      !!----------------------------------------------------------------------
      !
      REWIND ( numnam )               !* Read Namelist namzdf_tke : Turbulente Kinetic Energy
      READ   ( numnam, namzdf_tke )
      !
      ri_cri   = 2._wp    / ( 2._wp + rn_ediss / rn_ediff )   ! resulting critical Richardson number
      rmxl_min = 1.e-6_wp / ( rn_ediff * SQRT( rn_emin ) )    ! resulting minimum length to recover molecular viscosity
      !
      IF(lwp) THEN                    !* Control print
         WRITE(numout,*)
         WRITE(numout,*) 'zdf_tke_init : tke turbulent closure scheme - initialisation'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namzdf_tke : set tke mixing parameters'
         WRITE(numout,*) '      coef. to compute avt                        rn_ediff  = ', rn_ediff
         WRITE(numout,*) '      Kolmogoroff dissipation coef.               rn_ediss  = ', rn_ediss
         WRITE(numout,*) '      tke surface input coef.                     rn_ebb    = ', rn_ebb
         WRITE(numout,*) '      minimum value of tke                        rn_emin   = ', rn_emin
         WRITE(numout,*) '      surface minimum value of tke                rn_emin0  = ', rn_emin0
         WRITE(numout,*) '      background shear (>0)                       rn_bshear = ', rn_bshear
         WRITE(numout,*) '      mixing length type                          nn_mxl    = ', nn_mxl
         WRITE(numout,*) '      prandl number flag                          nn_pdl    = ', nn_pdl
         WRITE(numout,*) '      surface mixing length = F(stress) or not    ln_mxl0   = ', ln_mxl0
         WRITE(numout,*) '      surface  mixing length minimum value        rn_mxl0   = ', rn_mxl0
         WRITE(numout,*) '      flag to take into acc.  Langmuir circ.      ln_lc     = ', ln_lc
         WRITE(numout,*) '      coef to compute verticla velocity of LC     rn_lc     = ', rn_lc
         WRITE(numout,*) '      test param. to add tke induced by wind      nn_etau   = ', nn_etau
         WRITE(numout,*) '      flag for computation of exp. tke profile    nn_htau   = ', nn_htau
         WRITE(numout,*) '      fraction of en which pene. the thermocline  rn_efr    = ', rn_efr
         WRITE(numout,*)
         WRITE(numout,*) '      critical Richardson nb with your parameters  ri_cri = ', ri_cri
      ENDIF
      !
      !                              ! allocate tke arrays
      IF( zdf_tke_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'zdf_tke_init : unable to allocate arrays' )
      !
      !                               !* Check of some namelist values
      IF( nn_mxl  < 0  .OR.  nn_mxl  > 3 )   CALL ctl_stop( 'bad flag: nn_mxl is  0, 1 or 2 ' )
      IF( nn_pdl  < 0  .OR.  nn_pdl  > 1 )   CALL ctl_stop( 'bad flag: nn_pdl is  0 or 1    ' )
      IF( nn_htau < 0  .OR.  nn_htau > 1 )   CALL ctl_stop( 'bad flag: nn_htau is 0, 1 or 2 ' )
#if ! key_coupled
      IF( nn_etau == 3 )   CALL ctl_stop( 'nn_etau == 3 : HF taum only known in coupled mode' )
#endif

      IF( ln_mxl0 ) THEN
         IF(lwp) WRITE(numout,*) '   use a surface mixing length = F(stress) :   set rn_mxl0 = rmxl_min'
         rn_mxl0 = rmxl_min
      ENDIF
      
      IF( nn_etau == 2  )   CALL zdf_mxl( nit000 )      ! Initialization of nmln 

      !                               !* depth of penetration of surface tke
      IF( nn_etau /= 0 ) THEN      
         SELECT CASE( nn_htau )             ! Choice of the depth of penetration
         CASE( 0 )                                 ! constant depth penetration (here 10 meters)
            htau(:,:) = 10._wp
         CASE( 1 )                                 ! F(latitude) : 0.5m to 30m poleward of 40 degrees
            htau(:,:) = MAX(  0.5_wp, MIN( 30._wp, 45._wp* ABS( SIN( rpi/180._wp * gphit(:,:) ) ) )   )            
         END SELECT
      ENDIF
      !                               !* set vertical eddy coef. to the background value
      DO jk = 1, jpk
         avt (:,:,jk) = avtb(jk) * tmask(:,:,jk)
         avm (:,:,jk) = avmb(jk) * tmask(:,:,jk)
         avmu(:,:,jk) = avmb(jk) * umask(:,:,jk)
         avmv(:,:,jk) = avmb(jk) * vmask(:,:,jk)
      END DO
      dissl(:,:,:) = 1.e-12_wp
      !                              
      CALL tke_rst( nit000, 'READ' )  !* read or initialize all required files
      !
      IF(.NOT. Agrif_Root()) THEN

         wei3d(:,:,:) = 1.
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = 2, jpim1  
                  wei3d(ji,jj,jk) =   &
                     &   1.*tmask(ji-1,jj-1,jk) + 2.*tmask(ji,jj-1,jk) + 1.*tmask(ji+1,jj-1,jk)&
                     & + 2.*tmask(ji-1,jj  ,jk) + 4.*tmask(ji,jj  ,jk) + 2.*tmask(ji+1,jj  ,jk)&
                     & + 1.*tmask(ji-1,jj+1,jk) + 2.*tmask(ji,jj+1,jk) + 1.*tmask(ji+1,jj+1,jk)
                  wei3d(ji,jj,jk) = tmask(ji,jj,jk) / MAX( 1., wei3d(ji,jj,jk) )
               END DO
            END DO
         END DO
         CALL lbc_lnk( wei3d, 'T', 1. )

         wmix(:,:) = 0.
         wmix(mi0(2):mi1(jpiglo-1),mj0(2):mj1(jpjglo-1)) = 1.
         wmix(mi0(6):mi1(jpiglo-5),mj0(6):mj1(jpjglo-5)) = 0.75
         wmix(mi0(7):mi1(jpiglo-6),mj0(7):mj1(jpjglo-6)) = 0.5
         wmix(mi0(8):mi1(jpiglo-7),mj0(8):mj1(jpjglo-7)) = 0.25
         wmix(mi0(9):mi1(jpiglo-8),mj0(9):mj1(jpjglo-8)) = 0.

      END IF

   END SUBROUTINE zdf_tke_init


   SUBROUTINE tke_rst( kt, cdrw )
     !!---------------------------------------------------------------------
     !!                   ***  ROUTINE tke_rst  ***
     !!                     
     !! ** Purpose :   Read or write TKE file (en) in restart file
     !!
     !! ** Method  :   use of IOM library
     !!                if the restart does not contain TKE, en is either 
     !!                set to rn_emin or recomputed 
     !!----------------------------------------------------------------------
     INTEGER         , INTENT(in) ::   kt     ! ocean time-step
     CHARACTER(len=*), INTENT(in) ::   cdrw   ! "READ"/"WRITE" flag
     !
     INTEGER ::   jit, jk   ! dummy loop indices
     INTEGER ::   id1, id2, id3, id4, id5, id6   ! local integers
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
           id6 = iom_varid( numror, 'dissl', ldstop = .FALSE. )
           !
           IF( id1 > 0 ) THEN                       ! 'en' exists
              CALL iom_get( numror, jpdom_autoglo, 'en', en )
              IF( MIN( id2, id3, id4, id5, id6 ) > 0 ) THEN        ! all required arrays exist
                 CALL iom_get( numror, jpdom_autoglo, 'avt'  , avt   )
                 CALL iom_get( numror, jpdom_autoglo, 'avm'  , avm   )
                 CALL iom_get( numror, jpdom_autoglo, 'avmu' , avmu  )
                 CALL iom_get( numror, jpdom_autoglo, 'avmv' , avmv  )
                 CALL iom_get( numror, jpdom_autoglo, 'dissl', dissl )
              ELSE                                                 ! one at least array is missing
                 CALL tke_avn                                          ! compute avt, avm, avmu, avmv and dissl (approximation)
              ENDIF
           ELSE                                     ! No TKE array found: initialisation
              IF(lwp) WRITE(numout,*) ' ===>>>> : previous run without tke scheme, en computed by iterative loop'
              en (:,:,:) = rn_emin * tmask(:,:,:)
              CALL tke_avn                               ! recompute avt, avm, avmu, avmv and dissl (approximation)
              DO jit = nit000 + 1, nit000 + 10   ;   CALL zdf_tke( jit )   ;   END DO
           ENDIF
        ELSE                                   !* Start from rest
           en(:,:,:) = rn_emin * tmask(:,:,:)
           DO jk = 1, jpk                           ! set the Kz to the background value
              avt (:,:,jk) = avtb(jk) * tmask(:,:,jk)
              avm (:,:,jk) = avmb(jk) * tmask(:,:,jk)
              avmu(:,:,jk) = avmb(jk) * umask(:,:,jk)
              avmv(:,:,jk) = avmb(jk) * vmask(:,:,jk)
           END DO
        ENDIF
        !
     ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN   ! Create restart file
        !                                   ! -------------------
        IF(lwp) WRITE(numout,*) '---- tke-rst ----'
        CALL iom_rstput( kt, nitrst, numrow, 'en'   , en     )
        CALL iom_rstput( kt, nitrst, numrow, 'avt'  , avt_k  )
        CALL iom_rstput( kt, nitrst, numrow, 'avm'  , avm_k  )
        CALL iom_rstput( kt, nitrst, numrow, 'avmu' , avmu_k )
        CALL iom_rstput( kt, nitrst, numrow, 'avmv' , avmv_k )
        CALL iom_rstput( kt, nitrst, numrow, 'dissl', dissl  )
        !
     ENDIF
     !
   END SUBROUTINE tke_rst

#else
   !!----------------------------------------------------------------------
   !!   Dummy module :                                        NO TKE scheme
   !!----------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_zdftke = .FALSE.   !: TKE flag
CONTAINS
   SUBROUTINE zdf_tke_init           ! Dummy routine
   END SUBROUTINE zdf_tke_init
   SUBROUTINE zdf_tke( kt )          ! Dummy routine
      WRITE(*,*) 'zdf_tke: You should not have seen this print! error?', kt
   END SUBROUTINE zdf_tke
   SUBROUTINE tke_rst( kt, cdrw )
     CHARACTER(len=*) ::   cdrw
     WRITE(*,*) 'tke_rst: You should not have seen this print! error?', kt, cdwr
   END SUBROUTINE tke_rst
#endif

   !!======================================================================
END MODULE zdftke
