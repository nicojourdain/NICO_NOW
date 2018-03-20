MODULE limthd_dif
   !!======================================================================
   !!                       ***  MODULE limthd_dif ***
   !!                       heat diffusion in sea ice 
   !!                   computation of surface and inner T  
   !!======================================================================
   !! History :  LIM  ! 02-2003 (M. Vancoppenolle) original 1D code
   !!                 ! 06-2005 (M. Vancoppenolle) 3d version
   !!                 ! 11-2006 (X Fettweis) Vectorization by Xavier
   !!                 ! 04-2007 (M. Vancoppenolle) Energy conservation
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   USE par_oce          ! ocean parameters
   USE phycst           ! physical constants (ocean directory) 
   USE ice              ! LIM-3 variables
   USE par_ice          ! LIM-3 parameters
   USE thd_ice          ! LIM-3: thermodynamics
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_thd_dif   ! called by lim_thd

   REAL(wp) ::   epsi20 = 1e-20     ! constant values
   REAL(wp) ::   epsi13 = 1e-13     ! constant values

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limthd_dif.F90 3351 2012-04-11 12:16:00Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_thd_dif( kideb , kiut , jl )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_thd_dif  ***
      !! ** Purpose :
      !!           This routine determines the time evolution of snow and sea-ice 
      !!           temperature profiles.
      !! ** Method  :
      !!           This is done by solving the heat equation diffusion with
      !!           a Neumann boundary condition at the surface and a Dirichlet one
      !!           at the bottom. Solar radiation is partially absorbed into the ice.
      !!           The specific heat and thermal conductivities depend on ice salinity
      !!           and temperature to take into account brine pocket melting. The 
      !!           numerical
      !!           scheme is an iterative Crank-Nicolson on a non-uniform multilayer grid 
      !!           in the ice and snow system.
      !!
      !!           The successive steps of this routine are
      !!           1.  Thermal conductivity at the interfaces of the ice layers
      !!           2.  Internal absorbed radiation
      !!           3.  Scale factors due to non-uniform grid
      !!           4.  Kappa factors
      !!           Then iterative procedure begins
      !!           5.  specific heat in the ice
      !!           6.  eta factors
      !!           7.  surface flux computation
      !!           8.  tridiagonal system terms
      !!           9.  solving the tridiagonal system with Gauss elimination
      !!           Iterative procedure ends according to a criterion on evolution
      !!           of temperature
      !!
      !! ** Arguments :
      !!           kideb , kiut : Starting and ending points on which the 
      !!                         the computation is applied
      !!
      !! ** Inputs / Ouputs : (global commons)
      !!           surface temperature : t_su_b
      !!           ice/snow temperatures   : t_i_b, t_s_b
      !!           ice salinities          : s_i_b
      !!           number of layers in the ice/snow: nlay_i, nlay_s
      !!           profile of the ice/snow layers : z_i, z_s
      !!           total ice/snow thickness : ht_i_b, ht_s_b
      !!
      !! ** External : 
      !!
      !! ** References :
      !!
      !! ** History :
      !!           (02-2003) Martin Vancoppenolle, Louvain-la-Neuve, Belgium
      !!           (06-2005) Martin Vancoppenolle, 3d version
      !!           (11-2006) Vectorized by Xavier Fettweis (UCL-ASTR)
      !!           (04-2007) Energy conservation tested by M. Vancoppenolle
      !!------------------------------------------------------------------
      INTEGER , INTENT (in) ::   kideb   ! Start point on which the  the computation is applied
      INTEGER , INTENT (in) ::   kiut    ! End point on which the  the computation is applied
      INTEGER , INTENT (in) ::   jl      ! Category number

      !! * Local variables
      INTEGER ::   ji          ! spatial loop index
      INTEGER ::   ii, ij      ! temporary dummy loop index
      INTEGER ::   numeq       ! current reference number of equation
      INTEGER ::   layer       ! vertical dummy loop index 
      INTEGER ::   nconv       ! number of iterations in iterative procedure
      INTEGER ::   minnumeqmin, maxnumeqmax
      INTEGER, DIMENSION(kiut) ::   numeqmin   ! reference number of top equation
      INTEGER, DIMENSION(kiut) ::   numeqmax   ! reference number of bottom equation
      INTEGER, DIMENSION(kiut) ::   isnow      ! switch for presence (1) or absence (0) of snow
      REAL(wp) ::   zeps      =  1.e-10_wp    !
      REAL(wp) ::   zg1s      =  2._wp        ! for the tridiagonal system
      REAL(wp) ::   zg1       =  2._wp        !
      REAL(wp) ::   zgamma    =  18009._wp    ! for specific heat
      REAL(wp) ::   zbeta     =  0.117_wp     ! for thermal conductivity (could be 0.13)
      REAL(wp) ::   zraext_s  =  1.e+8_wp     ! extinction coefficient of radiation in the snow
      REAL(wp) ::   zkimin    =  0.10_wp      ! minimum ice thermal conductivity
      REAL(wp) ::   zht_smin  =  1.e-4_wp     ! minimum snow depth
      REAL(wp) ::   ztmelt_i    ! ice melting temperature
      REAL(wp) ::   zerritmax   ! current maximal error on temperature 
      REAL(wp), DIMENSION(kiut) ::   ztfs        ! ice melting point
      REAL(wp), DIMENSION(kiut) ::   ztsuold     ! old surface temperature (before the iterative procedure )
      REAL(wp), DIMENSION(kiut) ::   ztsuoldit   ! surface temperature at previous iteration
      REAL(wp), DIMENSION(kiut) ::   zh_i        ! ice layer thickness
      REAL(wp), DIMENSION(kiut) ::   zh_s        ! snow layer thickness
      REAL(wp), DIMENSION(kiut) ::   zfsw        ! solar radiation absorbed at the surface
      REAL(wp), DIMENSION(kiut) ::   zf          ! surface flux function
      REAL(wp), DIMENSION(kiut) ::   dzf         ! derivative of the surface flux function
      REAL(wp), DIMENSION(kiut) ::   zerrit      ! current error on temperature
      REAL(wp), DIMENSION(kiut) ::   zdifcase    ! case of the equation resolution (1->4)
      REAL(wp), DIMENSION(kiut) ::   zftrice     ! solar radiation transmitted through the ice
      REAL(wp), DIMENSION(kiut) ::   zihic, zhsu
      REAL(wp), DIMENSION(kiut,0:nlay_i) ::   ztcond_i    ! Ice thermal conductivity
      REAL(wp), DIMENSION(kiut,0:nlay_i) ::   zradtr_i    ! Radiation transmitted through the ice
      REAL(wp), DIMENSION(kiut,0:nlay_i) ::   zradab_i    ! Radiation absorbed in the ice
      REAL(wp), DIMENSION(kiut,0:nlay_i) ::   zkappa_i    ! Kappa factor in the ice
      REAL(wp), DIMENSION(kiut,0:nlay_i) ::   ztiold      ! Old temperature in the ice
      REAL(wp), DIMENSION(kiut,0:nlay_i) ::   zeta_i      ! Eta factor in the ice
      REAL(wp), DIMENSION(kiut,0:nlay_i) ::   ztitemp     ! Temporary temperature in the ice to check the convergence
      REAL(wp), DIMENSION(kiut,0:nlay_i) ::   zspeche_i   ! Ice specific heat
      REAL(wp), DIMENSION(kiut,0:nlay_i) ::   z_i         ! Vertical cotes of the layers in the ice
      REAL(wp), DIMENSION(kiut,0:nlay_s) ::   zradtr_s    ! Radiation transmited through the snow
      REAL(wp), DIMENSION(kiut,0:nlay_s) ::   zradab_s    ! Radiation absorbed in the snow
      REAL(wp), DIMENSION(kiut,0:nlay_s) ::   zkappa_s    ! Kappa factor in the snow
      REAL(wp), DIMENSION(kiut,0:nlay_s) ::   zeta_s       ! Eta factor in the snow
      REAL(wp), DIMENSION(kiut,0:nlay_s) ::   ztstemp      ! Temporary temperature in the snow to check the convergence
      REAL(wp), DIMENSION(kiut,0:nlay_s) ::   ztsold       ! Temporary temperature in the snow
      REAL(wp), DIMENSION(kiut,0:nlay_s) ::   z_s          ! Vertical cotes of the layers in the snow
      REAL(wp), DIMENSION(kiut,jkmax+2) ::   zindterm   ! Independent term
      REAL(wp), DIMENSION(kiut,jkmax+2) ::   zindtbis   ! temporary independent term
      REAL(wp), DIMENSION(kiut,jkmax+2) ::   zdiagbis
      REAL(wp), DIMENSION(kiut,jkmax+2,3) ::   ztrid   ! tridiagonal system terms
      !!------------------------------------------------------------------
      
      ! 
      !------------------------------------------------------------------------------!
      ! 1) Initialization                                                            !
      !------------------------------------------------------------------------------!
      !
      DO ji = kideb , kiut
         ! is there snow or not
         isnow(ji)= INT(  1._wp - MAX( 0._wp , SIGN(1._wp, - ht_s_b(ji) ) )  )
         ! surface temperature of fusion
!!gm ???  ztfs(ji) = rtt !!!????
         ztfs(ji) = isnow(ji) * rtt + (1.0-isnow(ji)) * rtt
         ! layer thickness
         zh_i(ji) = ht_i_b(ji) / nlay_i
         zh_s(ji) = ht_s_b(ji) / nlay_s
      END DO

      !--------------------
      ! Ice / snow layers
      !--------------------

      z_s(:,0) = 0._wp   ! vert. coord. of the up. lim. of the 1st snow layer
      z_i(:,0) = 0._wp   ! vert. coord. of the up. lim. of the 1st ice layer

      DO layer = 1, nlay_s            ! vert. coord of the up. lim. of the layer-th snow layer
         DO ji = kideb , kiut
            z_s(ji,layer) = z_s(ji,layer-1) + ht_s_b(ji) / nlay_s
         END DO
      END DO

      DO layer = 1, nlay_i            ! vert. coord of the up. lim. of the layer-th ice layer
         DO ji = kideb , kiut
            z_i(ji,layer) = z_i(ji,layer-1) + ht_i_b(ji) / nlay_i
         END DO
      END DO
      !
      !------------------------------------------------------------------------------|
      ! 2) Radiations                                                                |
      !------------------------------------------------------------------------------|
      !
      !-------------------
      ! Computation of i0
      !-------------------
      ! i0 describes the fraction of solar radiation which does not contribute
      ! to the surface energy budget but rather penetrates inside the ice.
      ! We assume that no radiation is transmitted through the snow
      ! If there is no no snow
      ! zfsw    = (1-i0).qsr_ice   is absorbed at the surface 
      ! zftrice = io.qsr_ice       is below the surface 
      ! fstbif  = io.qsr_ice.exp(-k(h_i)) transmitted below the ice 

      DO ji = kideb , kiut
         ! switches
         isnow(ji) = INT(  1._wp - MAX( 0._wp , SIGN( 1._wp , - ht_s_b(ji) ) )  ) 
         ! hs > 0, isnow = 1
         zhsu (ji) = hnzst  ! threshold for the computation of i0
         zihic(ji) = MAX( 0._wp , 1._wp - ( ht_i_b(ji) / zhsu(ji) ) )     

         i0(ji)    = ( 1._wp - isnow(ji) ) * ( fr1_i0_1d(ji) + zihic(ji) * fr2_i0_1d(ji) )
         !fr1_i0_1d = i0 for a thin ice surface
         !fr1_i0_2d = i0 for a thick ice surface
         !            a function of the cloud cover
         !
         !i0(ji)     =  (1.0-FLOAT(isnow(ji)))*3.0/(100*ht_s_b(ji)+10.0)
         !formula used in Cice
      END DO

      !-------------------------------------------------------
      ! Solar radiation absorbed / transmitted at the surface
      ! Derivative of the non solar flux
      !-------------------------------------------------------
      DO ji = kideb , kiut
         zfsw   (ji) =  qsr_ice_1d(ji) * ( 1 - i0(ji) )   ! Shortwave radiation absorbed at surface
         zftrice(ji) =  qsr_ice_1d(ji) *       i0(ji)     ! Solar radiation transmitted below the surface layer
         dzf    (ji) = dqns_ice_1d(ji)                    ! derivative of incoming nonsolar flux 
      END DO

      !---------------------------------------------------------
      ! Transmission - absorption of solar radiation in the ice
      !---------------------------------------------------------

      DO ji = kideb, kiut           ! snow initialization
         zradtr_s(ji,0) = zftrice(ji)     ! radiation penetrating through snow
      END DO

      DO layer = 1, nlay_s          ! Radiation through snow
         DO ji = kideb, kiut
            !                             ! radiation transmitted below the layer-th snow layer
            zradtr_s(ji,layer) = zradtr_s(ji,0) * EXP( - zraext_s * ( MAX ( 0._wp , z_s(ji,layer) ) ) )
            !                             ! radiation absorbed by the layer-th snow layer
            zradab_s(ji,layer) = zradtr_s(ji,layer-1) - zradtr_s(ji,layer)
         END DO
      END DO

      DO ji = kideb, kiut           ! ice initialization
         zradtr_i(ji,0) = zradtr_s(ji,nlay_s) * isnow(ji) + zftrice(ji) * ( 1._wp - isnow(ji) )
      END DO

      DO layer = 1, nlay_i          ! Radiation through ice
         DO ji = kideb, kiut
            !                             ! radiation transmitted below the layer-th ice layer
            zradtr_i(ji,layer) = zradtr_i(ji,0) * EXP( - kappa_i * ( MAX ( 0._wp , z_i(ji,layer) ) ) )
            !                             ! radiation absorbed by the layer-th ice layer
            zradab_i(ji,layer) = zradtr_i(ji,layer-1) - zradtr_i(ji,layer)
         END DO
      END DO

      DO ji = kideb, kiut           ! Radiation transmitted below the ice
         fstbif_1d(ji) = fstbif_1d(ji) + zradtr_i(ji,nlay_i) * a_i_b(ji) / at_i_b(ji)
      END DO

      ! +++++
      ! just to check energy conservation
      DO ji = kideb, kiut
         ii                = MOD( npb(ji) - 1, jpi ) + 1
         ij                = ( npb(ji) - 1 ) / jpi + 1
         fstroc(ii,ij,jl) = zradtr_i(ji,nlay_i)
      END DO
      ! +++++

      DO layer = 1, nlay_i
         DO ji = kideb, kiut
            radab(ji,layer) = zradab_i(ji,layer)
         END DO
      END DO


      !
      !------------------------------------------------------------------------------|
      !  3) Iterative procedure begins                                               |
      !------------------------------------------------------------------------------|
      !
      DO ji = kideb, kiut        ! Old surface temperature
         ztsuold  (ji) =  t_su_b(ji)                              ! temperature at the beg of iter pr.
         ztsuoldit(ji) =  t_su_b(ji)                              ! temperature at the previous iter
         t_su_b   (ji) =  MIN( t_su_b(ji), ztfs(ji)-0.00001 )     ! necessary
         zerrit   (ji) =  1000._wp                                ! initial value of error
      END DO

      DO layer = 1, nlay_s       ! Old snow temperature
         DO ji = kideb , kiut
            ztsold(ji,layer) =  t_s_b(ji,layer)
         END DO
      END DO

      DO layer = 1, nlay_i       ! Old ice temperature
         DO ji = kideb , kiut
            ztiold(ji,layer) =  t_i_b(ji,layer)
         END DO
      END DO

      nconv     =  0           ! number of iterations
      zerritmax =  1000._wp    ! maximal value of error on all points

      DO WHILE ( zerritmax > maxer_i_thd .AND. nconv < nconv_i_thd )
         !
         nconv = nconv + 1
         !
         !------------------------------------------------------------------------------|
         ! 4) Sea ice thermal conductivity                                              |
         !------------------------------------------------------------------------------|
         !
         IF( thcon_i_swi == 0 ) THEN      ! Untersteiner (1964) formula
            DO ji = kideb , kiut
               ztcond_i(ji,0)        = rcdic + zbeta*s_i_b(ji,1) / MIN(-zeps,t_i_b(ji,1)-rtt)
               ztcond_i(ji,0)        = MAX(ztcond_i(ji,0),zkimin)
            END DO
            DO layer = 1, nlay_i-1
               DO ji = kideb , kiut
                  ztcond_i(ji,layer) = rcdic + zbeta*( s_i_b(ji,layer) + s_i_b(ji,layer+1) ) /  &
                     MIN(-2.0_wp * zeps, t_i_b(ji,layer)+t_i_b(ji,layer+1) - 2.0_wp * rtt)
                  ztcond_i(ji,layer) = MAX(ztcond_i(ji,layer),zkimin)
               END DO
            END DO
         ENDIF

         IF( thcon_i_swi == 1 ) THEN      ! Pringle et al formula included: 2.11 + 0.09 S/T - 0.011.T
            DO ji = kideb , kiut
               ztcond_i(ji,0) = rcdic + 0.090_wp * s_i_b(ji,1) / MIN( -zeps, t_i_b(ji,1)-rtt )   &
                  &                   - 0.011_wp * ( t_i_b(ji,1) - rtt )  
               ztcond_i(ji,0) = MAX( ztcond_i(ji,0), zkimin )
            END DO
            DO layer = 1, nlay_i-1
               DO ji = kideb , kiut
                  ztcond_i(ji,layer) = rcdic + 0.090_wp * ( s_i_b(ji,layer) + s_i_b(ji,layer+1) )   &
                     &                                  / MIN(-2.0_wp * zeps, t_i_b(ji,layer)+t_i_b(ji,layer+1) - 2.0_wp * rtt)   &
                     &                       - 0.0055_wp* ( t_i_b(ji,layer) + t_i_b(ji,layer+1) - 2.0*rtt )  
                  ztcond_i(ji,layer) = MAX( ztcond_i(ji,layer), zkimin )
               END DO
            END DO
            DO ji = kideb , kiut
               ztcond_i(ji,nlay_i) = rcdic + 0.090_wp * s_i_b(ji,nlay_i) / MIN(-zeps,t_bo_b(ji)-rtt)   &
                  &                        - 0.011_wp * ( t_bo_b(ji) - rtt )  
               ztcond_i(ji,nlay_i) = MAX( ztcond_i(ji,nlay_i), zkimin )
            END DO
         ENDIF
         !
         !------------------------------------------------------------------------------|
         !  5) kappa factors                                                            |
         !------------------------------------------------------------------------------|
         !
         DO ji = kideb, kiut

            !-- Snow kappa factors
            zkappa_s(ji,0)         = rcdsn / MAX(zeps,zh_s(ji))
            zkappa_s(ji,nlay_s)    = rcdsn / MAX(zeps,zh_s(ji))
         END DO

         DO layer = 1, nlay_s-1
            DO ji = kideb , kiut
               zkappa_s(ji,layer)  = 2.0 * rcdsn / &
                  MAX(zeps,2.0*zh_s(ji))
            END DO
         END DO

         DO layer = 1, nlay_i-1
            DO ji = kideb , kiut
               !-- Ice kappa factors
               zkappa_i(ji,layer)  = 2.0*ztcond_i(ji,layer)/ &
                  MAX(zeps,2.0*zh_i(ji)) 
            END DO
         END DO

         DO ji = kideb , kiut
            zkappa_i(ji,0)        = ztcond_i(ji,0)/MAX(zeps,zh_i(ji))
            zkappa_i(ji,nlay_i)   = ztcond_i(ji,nlay_i) / MAX(zeps,zh_i(ji))
            !-- Interface
            zkappa_s(ji,nlay_s)   = 2.0*rcdsn*ztcond_i(ji,0)/MAX(zeps, &
               (ztcond_i(ji,0)*zh_s(ji) + rcdsn*zh_i(ji)))
            zkappa_i(ji,0)        = zkappa_s(ji,nlay_s)*isnow(ji) &
               + zkappa_i(ji,0)*(1.0-isnow(ji))
         END DO
         !
         !------------------------------------------------------------------------------|
         ! 6) Sea ice specific heat, eta factors                                        |
         !------------------------------------------------------------------------------|
         !
         DO layer = 1, nlay_i
            DO ji = kideb , kiut
               ztitemp(ji,layer)   = t_i_b(ji,layer)
               zspeche_i(ji,layer) = cpic + zgamma*s_i_b(ji,layer)/ &
                  MAX((t_i_b(ji,layer)-rtt)*(ztiold(ji,layer)-rtt),zeps)
               zeta_i(ji,layer)    = rdt_ice / MAX(rhoic*zspeche_i(ji,layer)*zh_i(ji), &
                  zeps)
            END DO
         END DO

         DO layer = 1, nlay_s
            DO ji = kideb , kiut
               ztstemp(ji,layer) = t_s_b(ji,layer)
               zeta_s(ji,layer)  = rdt_ice / MAX(rhosn*cpic*zh_s(ji),zeps)
            END DO
         END DO
         !
         !------------------------------------------------------------------------------|
         ! 7) surface flux computation                                                  |
         !------------------------------------------------------------------------------|
         !
         DO ji = kideb , kiut

            ! update of the non solar flux according to the update in T_su
            qnsr_ice_1d(ji) = qnsr_ice_1d(ji) + dqns_ice_1d(ji) * & 
               ( t_su_b(ji) - ztsuoldit(ji) )

            ! update incoming flux
            zf(ji)    =   zfsw(ji)              & ! net absorbed solar radiation
               + qnsr_ice_1d(ji)           ! non solar total flux 
            ! (LWup, LWdw, SH, LH)

         END DO

         !
         !------------------------------------------------------------------------------|
         ! 8) tridiagonal system terms                                                  |
         !------------------------------------------------------------------------------|
         !
         !!layer denotes the number of the layer in the snow or in the ice
         !!numeq denotes the reference number of the equation in the tridiagonal
         !!system, terms of tridiagonal system are indexed as following :
         !!1 is subdiagonal term, 2 is diagonal and 3 is superdiagonal one

         !!ice interior terms (top equation has the same form as the others)

         DO numeq=1,jkmax+2
            DO ji = kideb , kiut
               ztrid(ji,numeq,1) = 0.
               ztrid(ji,numeq,2) = 0.
               ztrid(ji,numeq,3) = 0.
               zindterm(ji,numeq)= 0.
               zindtbis(ji,numeq)= 0.
               zdiagbis(ji,numeq)= 0.
            ENDDO
         ENDDO

         DO numeq = nlay_s + 2, nlay_s + nlay_i 
            DO ji = kideb , kiut
               layer              = numeq - nlay_s - 1
               ztrid(ji,numeq,1)  =  - zeta_i(ji,layer)*zkappa_i(ji,layer-1)
               ztrid(ji,numeq,2)  =  1.0 + zeta_i(ji,layer)*(zkappa_i(ji,layer-1) + &
                  zkappa_i(ji,layer))
               ztrid(ji,numeq,3)  =  - zeta_i(ji,layer)*zkappa_i(ji,layer)
               zindterm(ji,numeq) =  ztiold(ji,layer) + zeta_i(ji,layer)* &
                  zradab_i(ji,layer)
            END DO
         ENDDO

         numeq =  nlay_s + nlay_i + 1
         DO ji = kideb , kiut
            !!ice bottom term
            ztrid(ji,numeq,1)  =  - zeta_i(ji,nlay_i)*zkappa_i(ji,nlay_i-1)   
            ztrid(ji,numeq,2)  =  1.0 + zeta_i(ji,nlay_i)*( zkappa_i(ji,nlay_i)*zg1 &
               +  zkappa_i(ji,nlay_i-1) )
            ztrid(ji,numeq,3)  =  0.0
            zindterm(ji,numeq) =  ztiold(ji,nlay_i) + zeta_i(ji,nlay_i)* &
               ( zradab_i(ji,nlay_i) + zkappa_i(ji,nlay_i)*zg1 &
               *  t_bo_b(ji) ) 
         ENDDO


         DO ji = kideb , kiut
            IF ( ht_s_b(ji).gt.0.0 ) THEN
               !
               !------------------------------------------------------------------------------|
               !  snow-covered cells                                                          |
               !------------------------------------------------------------------------------|
               !
               !!snow interior terms (bottom equation has the same form as the others)
               DO numeq = 3, nlay_s + 1
                  layer =  numeq - 1
                  ztrid(ji,numeq,1)   =  - zeta_s(ji,layer)*zkappa_s(ji,layer-1)
                  ztrid(ji,numeq,2)   =  1.0 + zeta_s(ji,layer)*( zkappa_s(ji,layer-1) + &
                     zkappa_s(ji,layer) )
                  ztrid(ji,numeq,3)   =  - zeta_s(ji,layer)*zkappa_s(ji,layer)
                  zindterm(ji,numeq)  =  ztsold(ji,layer) + zeta_s(ji,layer)* &
                     zradab_s(ji,layer)
               END DO

               !!case of only one layer in the ice (ice equation is altered)
               IF ( nlay_i.eq.1 ) THEN
                  ztrid(ji,nlay_s+2,3)    =  0.0
                  zindterm(ji,nlay_s+2)   =  zindterm(ji,nlay_s+2) + zkappa_i(ji,1)* &
                     t_bo_b(ji) 
               ENDIF

               IF ( t_su_b(ji) .LT. rtt ) THEN

                  !------------------------------------------------------------------------------|
                  !  case 1 : no surface melting - snow present                                  |
                  !------------------------------------------------------------------------------|
                  zdifcase(ji)    =  1.0
                  numeqmin(ji)    =  1
                  numeqmax(ji)    =  nlay_i + nlay_s + 1

                  !!surface equation
                  ztrid(ji,1,1) = 0.0
                  ztrid(ji,1,2) = dzf(ji) - zg1s*zkappa_s(ji,0)
                  ztrid(ji,1,3) = zg1s*zkappa_s(ji,0)
                  zindterm(ji,1) = dzf(ji)*t_su_b(ji)   - zf(ji)

                  !!first layer of snow equation
                  ztrid(ji,2,1)  =  - zkappa_s(ji,0)*zg1s*zeta_s(ji,1)
                  ztrid(ji,2,2)  =  1.0 + zeta_s(ji,1)*(zkappa_s(ji,1) + zkappa_s(ji,0)*zg1s)
                  ztrid(ji,2,3)  =  - zeta_s(ji,1)* zkappa_s(ji,1)
                  zindterm(ji,2) =  ztsold(ji,1) + zeta_s(ji,1)*zradab_s(ji,1)

               ELSE 
                  !
                  !------------------------------------------------------------------------------|
                  !  case 2 : surface is melting - snow present                                  |
                  !------------------------------------------------------------------------------|
                  !
                  zdifcase(ji)    =  2.0
                  numeqmin(ji)    =  2
                  numeqmax(ji)    =  nlay_i + nlay_s + 1

                  !!first layer of snow equation
                  ztrid(ji,2,1)  =  0.0
                  ztrid(ji,2,2)  =  1.0 + zeta_s(ji,1) * ( zkappa_s(ji,1) + &
                     zkappa_s(ji,0) * zg1s )
                  ztrid(ji,2,3)  =  - zeta_s(ji,1)*zkappa_s(ji,1) 
                  zindterm(ji,2) = ztsold(ji,1) + zeta_s(ji,1) *            &
                     ( zradab_s(ji,1) +                         &
                     zkappa_s(ji,0) * zg1s * t_su_b(ji) ) 
               ENDIF
            ELSE
               !
               !------------------------------------------------------------------------------|
               !  cells without snow                                                          |
               !------------------------------------------------------------------------------|
               !
               IF (t_su_b(ji) .LT. rtt) THEN
                  !
                  !------------------------------------------------------------------------------|
                  !  case 3 : no surface melting - no snow                                       |
                  !------------------------------------------------------------------------------|
                  !
                  zdifcase(ji)      =  3.0
                  numeqmin(ji)      =  nlay_s + 1
                  numeqmax(ji)      =  nlay_i + nlay_s + 1

                  !!surface equation	
                  ztrid(ji,numeqmin(ji),1)   =  0.0
                  ztrid(ji,numeqmin(ji),2)   =  dzf(ji) - zkappa_i(ji,0)*zg1    
                  ztrid(ji,numeqmin(ji),3)   =  zkappa_i(ji,0)*zg1
                  zindterm(ji,numeqmin(ji))  =  dzf(ji)*t_su_b(ji) - zf(ji)

                  !!first layer of ice equation
                  ztrid(ji,numeqmin(ji)+1,1) =  - zkappa_i(ji,0) * zg1 * zeta_i(ji,1)
                  ztrid(ji,numeqmin(ji)+1,2) =  1.0 + zeta_i(ji,1) * ( zkappa_i(ji,1) & 
                     + zkappa_i(ji,0) * zg1 )
                  ztrid(ji,numeqmin(ji)+1,3) =  - zeta_i(ji,1)*zkappa_i(ji,1)  
                  zindterm(ji,numeqmin(ji)+1)=  ztiold(ji,1) + zeta_i(ji,1)*zradab_i(ji,1)  

                  !!case of only one layer in the ice (surface & ice equations are altered)

                  IF (nlay_i.eq.1) THEN
                     ztrid(ji,numeqmin(ji),1)    =  0.0
                     ztrid(ji,numeqmin(ji),2)    =  dzf(ji) - zkappa_i(ji,0)*2.0
                     ztrid(ji,numeqmin(ji),3)    =  zkappa_i(ji,0)*2.0
                     ztrid(ji,numeqmin(ji)+1,1)  =  -zkappa_i(ji,0)*2.0*zeta_i(ji,1)
                     ztrid(ji,numeqmin(ji)+1,2)  =  1.0 + zeta_i(ji,1)*(zkappa_i(ji,0)*2.0 + &
                        zkappa_i(ji,1))
                     ztrid(ji,numeqmin(ji)+1,3)  =  0.0

                     zindterm(ji,numeqmin(ji)+1) =  ztiold(ji,1) + zeta_i(ji,1)* &
                        ( zradab_i(ji,1) + zkappa_i(ji,1)*t_bo_b(ji) )
                  ENDIF

               ELSE

                  !
                  !------------------------------------------------------------------------------|
                  ! case 4 : surface is melting - no snow                                        |
                  !------------------------------------------------------------------------------|
                  !
                  zdifcase(ji)    =  4.0
                  numeqmin(ji)    =  nlay_s + 2
                  numeqmax(ji)    =  nlay_i + nlay_s + 1

                  !!first layer of ice equation
                  ztrid(ji,numeqmin(ji),1)      =  0.0
                  ztrid(ji,numeqmin(ji),2)      =  1.0 + zeta_i(ji,1)*(zkappa_i(ji,1) + zkappa_i(ji,0)* &
                     zg1)  
                  ztrid(ji,numeqmin(ji),3)      =  - zeta_i(ji,1) * zkappa_i(ji,1)
                  zindterm(ji,numeqmin(ji))     =  ztiold(ji,1) + zeta_i(ji,1)*( zradab_i(ji,1) + &
                     zkappa_i(ji,0) * zg1 * t_su_b(ji) ) 

                  !!case of only one layer in the ice (surface & ice equations are altered)
                  IF (nlay_i.eq.1) THEN
                     ztrid(ji,numeqmin(ji),1)  =  0.0
                     ztrid(ji,numeqmin(ji),2)  =  1.0 + zeta_i(ji,1)*(zkappa_i(ji,0)*2.0 + &
                        zkappa_i(ji,1))
                     ztrid(ji,numeqmin(ji),3)  =  0.0
                     zindterm(ji,numeqmin(ji)) =  ztiold(ji,1) + zeta_i(ji,1)* &
                        (zradab_i(ji,1) + zkappa_i(ji,1)*t_bo_b(ji)) &
                        + t_su_b(ji)*zeta_i(ji,1)*zkappa_i(ji,0)*2.0
                  ENDIF

               ENDIF
            ENDIF

         END DO

         !
         !------------------------------------------------------------------------------|
         ! 9) tridiagonal system solving                                                |
         !------------------------------------------------------------------------------|
         !

         ! Solve the tridiagonal system with Gauss elimination method.
         ! Thomas algorithm, from Computational fluid Dynamics, J.D. ANDERSON, 
         ! McGraw-Hill 1984.	

         maxnumeqmax = 0
         minnumeqmin = jkmax+4

         DO ji = kideb , kiut
            zindtbis(ji,numeqmin(ji)) =  zindterm(ji,numeqmin(ji))
            zdiagbis(ji,numeqmin(ji)) =  ztrid(ji,numeqmin(ji),2)
            minnumeqmin               =  MIN(numeqmin(ji),minnumeqmin)
            maxnumeqmax               =  MAX(numeqmax(ji),maxnumeqmax)
         END DO

         DO layer = minnumeqmin+1, maxnumeqmax
            DO ji = kideb , kiut
               numeq               =  min(max(numeqmin(ji)+1,layer),numeqmax(ji))
               zdiagbis(ji,numeq)  =  ztrid(ji,numeq,2) - ztrid(ji,numeq,1)* &
                  ztrid(ji,numeq-1,3)/zdiagbis(ji,numeq-1)
               zindtbis(ji,numeq)  =  zindterm(ji,numeq) - ztrid(ji,numeq,1)* &
                  zindtbis(ji,numeq-1)/zdiagbis(ji,numeq-1)
            END DO
         END DO

         DO ji = kideb , kiut
            ! ice temperatures
            t_i_b(ji,nlay_i)    =  zindtbis(ji,numeqmax(ji))/zdiagbis(ji,numeqmax(ji))
         END DO

         DO numeq = nlay_i + nlay_s + 1, nlay_s + 2, -1
            DO ji = kideb , kiut
               layer    =  numeq - nlay_s - 1
               t_i_b(ji,layer)  =  (zindtbis(ji,numeq) - ztrid(ji,numeq,3)* &
                  t_i_b(ji,layer+1))/zdiagbis(ji,numeq)
            END DO
         END DO

         DO ji = kideb , kiut
            ! snow temperatures      
            IF (ht_s_b(ji).GT.0) &
               t_s_b(ji,nlay_s)     =  (zindtbis(ji,nlay_s+1) - ztrid(ji,nlay_s+1,3) &
               *  t_i_b(ji,1))/zdiagbis(ji,nlay_s+1) &
               *        MAX(0.0,SIGN(1.0,ht_s_b(ji)-zeps)) 

            ! surface temperature
            isnow(ji)     = INT(1.0-max(0.0,sign(1.0,-ht_s_b(ji))))
            ztsuoldit(ji) = t_su_b(ji)
            IF (t_su_b(ji) .LT. ztfs(ji)) &
               t_su_b(ji) = ( zindtbis(ji,numeqmin(ji)) - ztrid(ji,numeqmin(ji),3)* ( isnow(ji)*t_s_b(ji,1)   &
               &          + (1.0-isnow(ji))*t_i_b(ji,1) ) ) / zdiagbis(ji,numeqmin(ji))  
         END DO
         !
         !--------------------------------------------------------------------------
         !  10) Has the scheme converged ?, end of the iterative procedure         |
         !--------------------------------------------------------------------------
         !
         ! check that nowhere it has started to melt
         ! zerrit(ji) is a measure of error, it has to be under maxer_i_thd
         DO ji = kideb , kiut
            t_su_b(ji) =  MAX(  MIN( t_su_b(ji) , ztfs(ji) ) , 190._wp  )
            zerrit(ji) =  ABS( t_su_b(ji) - ztsuoldit(ji) )     
         END DO

         DO layer  =  1, nlay_s
            DO ji = kideb , kiut
               ii = MOD( npb(ji) - 1, jpi ) + 1
               ij = ( npb(ji) - 1 ) / jpi + 1
               t_s_b(ji,layer) = MAX(  MIN( t_s_b(ji,layer), rtt ), 190._wp  )
               zerrit(ji)      = MAX(zerrit(ji),ABS(t_s_b(ji,layer) - ztstemp(ji,layer)))
            END DO
         END DO

         DO layer  =  1, nlay_i
            DO ji = kideb , kiut
               ztmelt_i        = -tmut * s_i_b(ji,layer) + rtt 
               t_i_b(ji,layer) =  MAX(MIN(t_i_b(ji,layer),ztmelt_i),190.0)
               zerrit(ji)      =  MAX(zerrit(ji),ABS(t_i_b(ji,layer) - ztitemp(ji,layer)))
            END DO
         END DO

         ! Compute spatial maximum over all errors
         ! note that this could be optimized substantially by iterating only the non-converging points
         zerritmax = 0._wp
         DO ji = kideb, kiut
            zerritmax = MAX( zerritmax, zerrit(ji) )   
         END DO
         IF( lk_mpp ) CALL mpp_max( zerritmax, kcom=ncomm_ice )

      END DO  ! End of the do while iterative procedure

      IF( ln_nicep ) THEN
         WRITE(numout,*) ' zerritmax : ', zerritmax
         WRITE(numout,*) ' nconv     : ', nconv
      ENDIF

      !
      !-------------------------------------------------------------------------!
      !   11) Fluxes at the interfaces                                          !
      !-------------------------------------------------------------------------!
      DO ji = kideb, kiut
         !                                ! update of latent heat fluxes
         qla_ice_1d (ji) = qla_ice_1d (ji) + dqla_ice_1d(ji) * ( t_su_b(ji) - ztsuold(ji) )
         !                                ! surface ice conduction flux
         isnow(ji)       = INT(  1._wp - MAX( 0._wp, SIGN( 1._wp, -ht_s_b(ji) ) )  )
         fc_su(ji)       =  -           isnow(ji)   * zkappa_s(ji,0) * zg1s * (t_s_b(ji,1) - t_su_b(ji))   &
            &               - ( 1._wp - isnow(ji) ) * zkappa_i(ji,0) * zg1  * (t_i_b(ji,1) - t_su_b(ji))
         !                                ! bottom ice conduction flux
         fc_bo_i(ji)     =  - zkappa_i(ji,nlay_i) * ( zg1*(t_bo_b(ji) - t_i_b(ji,nlay_i)) )
      END DO

      !-------------------------!
      ! Heat conservation       !
      !-------------------------!
      IF( con_i ) THEN
         DO ji = kideb, kiut
            ! Upper snow value
            fc_s(ji,0) = - isnow(ji) * zkappa_s(ji,0) * zg1s * ( t_s_b(ji,1) - t_su_b(ji) ) 
            ! Bott. snow value
            fc_s(ji,1) = - isnow(ji)* zkappa_s(ji,1) * ( t_i_b(ji,1) - t_s_b(ji,1) ) 
         END DO
         DO ji = kideb, kiut         ! Upper ice layer
            fc_i(ji,0) = - isnow(ji) * &  ! interface flux if there is snow
               ( zkappa_i(ji,0)  * ( t_i_b(ji,1) - t_s_b(ji,nlay_s ) ) ) &
               - ( 1.0 - isnow(ji) ) * ( zkappa_i(ji,0) * & 
               zg1 * ( t_i_b(ji,1) - t_su_b(ji) ) ) ! upper flux if not
         END DO
         DO layer = 1, nlay_i - 1         ! Internal ice layers
            DO ji = kideb, kiut
               fc_i(ji,layer) = - zkappa_i(ji,layer) * ( t_i_b(ji,layer+1) - t_i_b(ji,layer) )
               ii = MOD( npb(ji) - 1, jpi ) + 1
               ij = ( npb(ji) - 1 ) / jpi + 1
            END DO
         END DO
         DO ji = kideb, kiut         ! Bottom ice layers
            fc_i(ji,nlay_i) = - zkappa_i(ji,nlay_i) * ( zg1*(t_bo_b(ji) - t_i_b(ji,nlay_i)) )
         END DO
      ENDIF
      !
   END SUBROUTINE lim_thd_dif

#else
   !!----------------------------------------------------------------------
   !!                   Dummy Module                 No LIM-3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_thd_dif          ! Empty routine
   END SUBROUTINE lim_thd_dif
#endif
   !!======================================================================
END MODULE limthd_dif
