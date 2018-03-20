MODULE limthd_lac
   !!======================================================================
   !!                       ***  MODULE limthd_lac   ***
   !!                lateral thermodynamic growth of the ice 
   !!======================================================================
   !! History :  LIM  ! 2005-12 (M. Vancoppenolle)  Original code
   !!             -   ! 2006-01 (M. Vancoppenolle)  add ITD
   !!            3.0  ! 2007-07 (M. Vancoppenolle)  Mass and energy conservation tested
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_lat_acr    : lateral accretion of ice
   !!----------------------------------------------------------------------
   USE par_oce          ! ocean parameters
   USE dom_oce          ! domain variables
   USE phycst           ! physical constants
   USE sbc_oce          ! Surface boundary condition: ocean fields
   USE sbc_ice          ! Surface boundary condition: ice fields
   USE thd_ice          ! LIM thermodynamics
   USE dom_ice          ! LIM domain
   USE par_ice          ! LIM parameters
   USE ice              ! LIM variables
   USE limtab           ! LIM 2D <==> 1D
   USE limcons          ! LIM conservation
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC lim_thd_lac     ! called by lim_thd

   REAL(wp) ::   epsi20 = 1e-20_wp   ! constant values
   REAL(wp) ::   epsi13 = 1e-13_wp   !
   REAL(wp) ::   epsi11 = 1e-11_wp   !
   REAL(wp) ::   epsi10 = 1e-10_wp   !
   REAL(wp) ::   epsi06 = 1e-06_wp   !
   REAL(wp) ::   epsi03 = 1e-03_wp   !
   REAL(wp) ::   zzero  = 0._wp      !
   REAL(wp) ::   zone   = 1._wp      !

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limthd_lac.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_thd_lac
      !!-------------------------------------------------------------------
      !!               ***   ROUTINE lim_thd_lac  ***
      !!  
      !! ** Purpose : Computation of the evolution of the ice thickness and 
      !!      concentration as a function of the heat balance in the leads.
      !!      It is only used for lateral accretion
      !!       
      !! ** Method  : Ice is formed in the open water when ocean lose heat
      !!      (heat budget of open water Bl is negative) .
      !!      Computation of the increase of 1-A (ice concentration) fol-
      !!      lowing the law :
      !!      (dA/dt)acc = F[ (1-A)/(1-a) ] * [ Bl / (Li*h0) ]
      !!       where - h0 is the thickness of ice created in the lead
      !!             - a is a minimum fraction for leads
      !!             - F is a monotonic non-increasing function defined as:
      !!                  F(X)=( 1 - X**exld )**(1.0/exld)
      !!             - exld is the exponent closure rate (=2 default val.)
      !! 
      !! ** Action : - Adjustment of snow and ice thicknesses and heat
      !!                content in brine pockets
      !!             - Updating ice internal temperature
      !!             - Computation of variation of ice volume and mass
      !!             - Computation of frldb after lateral accretion and 
      !!               update ht_s_b, ht_i_b and tbif_1d(:,:)      
      !!------------------------------------------------------------------------
      INTEGER ::   ji,jj,jk,jl,jm   ! dummy loop indices
      INTEGER ::   layer, nbpac     ! local integers 
      INTEGER ::   zji, zjj, iter   !   -       -
      REAL(wp)  ::   ztmelts, zdv, zqold, zfrazb, zweight, zalphai, zindb, zde  ! local scalars
      REAL(wp) ::   zgamafr, zvfrx, zvgx, ztaux, ztwogp, zf , zhicol_new        !   -      -
      REAL(wp) ::   ztenagm, zvfry, zvgy, ztauy, zvrel2, zfp, zsqcd , zhicrit   !   -      -
      LOGICAL  ::   iterate_frazil   ! iterate frazil ice collection thickness
      CHARACTER (len = 15) :: fieldid
      !
      INTEGER , POINTER, DIMENSION(:) ::   zcatac      ! indexes of categories where new ice grows
      REAL(wp), POINTER, DIMENSION(:) ::   zswinew     ! switch for new ice or not

      REAL(wp), POINTER, DIMENSION(:) ::   zv_newice   ! volume of accreted ice
      REAL(wp), POINTER, DIMENSION(:) ::   za_newice   ! fractional area of accreted ice
      REAL(wp), POINTER, DIMENSION(:) ::   zh_newice   ! thickness of accreted ice
      REAL(wp), POINTER, DIMENSION(:) ::   ze_newice   ! heat content of accreted ice
      REAL(wp), POINTER, DIMENSION(:) ::   zs_newice   ! salinity of accreted ice
      REAL(wp), POINTER, DIMENSION(:) ::   zo_newice   ! age of accreted ice
      REAL(wp), POINTER, DIMENSION(:) ::   zdv_res     ! residual volume in case of excessive heat budget
      REAL(wp), POINTER, DIMENSION(:) ::   zda_res     ! residual area in case of excessive heat budget
      REAL(wp), POINTER, DIMENSION(:) ::   zat_i_ac    ! total ice fraction    
      REAL(wp), POINTER, DIMENSION(:) ::   zat_i_lev   ! total ice fraction for level ice only (type 1)   
      REAL(wp), POINTER, DIMENSION(:) ::   zdh_frazb   ! accretion of frazil ice at the ice bottom
      REAL(wp), POINTER, DIMENSION(:) ::   zvrel_ac    ! relative ice / frazil velocity (1D vector)

      REAL(wp), POINTER, DIMENSION(:,:) ::   zhice_old   ! previous ice thickness
      REAL(wp), POINTER, DIMENSION(:,:) ::   zdummy      ! dummy thickness of new ice 
      REAL(wp), POINTER, DIMENSION(:,:) ::   zdhicbot    ! thickness of new ice which is accreted vertically
      REAL(wp), POINTER, DIMENSION(:,:) ::   zv_old      ! old volume of ice in category jl
      REAL(wp), POINTER, DIMENSION(:,:) ::   za_old      ! old area of ice in category jl
      REAL(wp), POINTER, DIMENSION(:,:) ::   za_i_ac     ! 1-D version of a_i
      REAL(wp), POINTER, DIMENSION(:,:) ::   zv_i_ac     ! 1-D version of v_i
      REAL(wp), POINTER, DIMENSION(:,:) ::   zoa_i_ac    ! 1-D version of oa_i
      REAL(wp), POINTER, DIMENSION(:,:) ::   zsmv_i_ac   ! 1-D version of smv_i

      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ze_i_ac   !: 1-D version of e_i

      REAL(wp), POINTER, DIMENSION(:) ::   zqbgow    ! heat budget of the open water (negative)
      REAL(wp), POINTER, DIMENSION(:) ::   zdhex     ! excessively thick accreted sea ice (hlead-hice)

      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zqm0      ! old layer-system heat content
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zthick0   ! old ice thickness

      REAL(wp), POINTER, DIMENSION(:,:) ::   vt_i_init, vt_i_final   ! ice volume summed over categories
      REAL(wp), POINTER, DIMENSION(:,:) ::   vt_s_init, vt_s_final   !  snow volume summed over categories
      REAL(wp), POINTER, DIMENSION(:,:) ::   et_i_init, et_i_final   !  ice energy summed over categories
      REAL(wp), POINTER, DIMENSION(:,:) ::   et_s_init               !  snow energy summed over categories
      REAL(wp), POINTER, DIMENSION(:,:) ::   zvrel                   ! relative ice / frazil velocity
      !!-----------------------------------------------------------------------!

      CALL wrk_alloc( jpij, zcatac )   ! integer
      CALL wrk_alloc( jpij, zswinew, zv_newice, za_newice, zh_newice, ze_newice, zs_newice, zo_newice )
      CALL wrk_alloc( jpij, zdv_res, zda_res, zat_i_ac, zat_i_lev, zdh_frazb, zvrel_ac, zqbgow, zdhex )
      CALL wrk_alloc( jpij,jpl, zhice_old, zdummy, zdhicbot, zv_old, za_old, za_i_ac, zv_i_ac, zoa_i_ac, zsmv_i_ac )
      CALL wrk_alloc( jpij,jkmax,jpl, ze_i_ac )
      CALL wrk_alloc( jpij,jkmax+1,jpl, zqm0, zthick0 )
      CALL wrk_alloc( jpi,jpj, vt_i_init, vt_i_final, vt_s_init, vt_s_final, et_i_init, et_i_final, et_s_init, zvrel )

      et_i_init(:,:) = 0._wp
      et_s_init(:,:) = 0._wp
      vt_i_init(:,:) = 0._wp
      vt_s_init(:,:) = 0._wp

      !------------------------------------------------------------------------------!
      ! 1) Conservation check and changes in each ice category
      !------------------------------------------------------------------------------!
      IF ( con_i ) THEN
         CALL lim_column_sum (jpl, v_i, vt_i_init)
         CALL lim_column_sum (jpl, v_s, vt_s_init)
         CALL lim_column_sum_energy (jpl, nlay_i, e_i, et_i_init)
         CALL lim_column_sum (jpl,   e_s(:,:,1,:) , et_s_init)
      ENDIF

      !------------------------------------------------------------------------------|
      ! 2) Convert units for ice internal energy
      !------------------------------------------------------------------------------|
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  !Energy of melting q(S,T) [J.m-3]
                  e_i(ji,jj,jk,jl) = e_i(ji,jj,jk,jl) / &
                     MAX( area(ji,jj) * v_i(ji,jj,jl) ,  epsi10 ) * &
                     nlay_i
                  zindb      = 1.0-MAX(0.0,SIGN(1.0,-v_i(ji,jj,jl))) !0 if no ice and 1 if yes
                  e_i(ji,jj,jk,jl) = e_i(ji,jj,jk,jl)*unit_fac*zindb
               END DO
            END DO
         END DO
      END DO

      !------------------------------------------------------------------------------!
      ! 3) Collection thickness of ice formed in leads and polynyas
      !------------------------------------------------------------------------------!    
      ! hicol is the thickness of new ice formed in open water
      ! hicol can be either prescribed (frazswi = 0)
      ! or computed (frazswi = 1)
      ! Frazil ice forms in open water, is transported by wind
      ! accumulates at the edge of the consolidated ice edge
      ! where it forms aggregates of a specific thickness called
      ! collection thickness.

      ! Note : the following algorithm currently breaks vectorization
      ! 

      zvrel(:,:) = 0.0

      ! Default new ice thickness 
      DO jj = 1, jpj
         DO ji = 1, jpi
            hicol(ji,jj) = hiccrit(1)
         END DO
      END DO

      IF (fraz_swi.eq.1.0) THEN

         !--------------------
         ! Physical constants
         !--------------------
         hicol(:,:) = 0.0

         zhicrit = 0.04 ! frazil ice thickness
         ztwogp  = 2. * rau0 / ( grav * 0.3 * ( rau0 - rhoic ) ) ! reduced grav
         zsqcd   = 1.0 / SQRT( 1.3 * cai ) ! 1/SQRT(airdensity*drag)
         zgamafr = 0.03

         DO jj = 1, jpj
            DO ji = 1, jpi

               IF ( tms(ji,jj) * ( qcmif(ji,jj) - qldif(ji,jj) ) > 0.e0 ) THEN
                  !-------------
                  ! Wind stress
                  !-------------
                  ! C-grid wind stress components
                  ztaux         = ( utau_ice(ji-1,jj  ) * tmu(ji-1,jj  ) &
                     &          +   utau_ice(ji  ,jj  ) * tmu(ji  ,jj  ) ) / 2.0
                  ztauy         = ( vtau_ice(ji  ,jj-1) * tmv(ji  ,jj-1) &
                     &          +   vtau_ice(ji  ,jj  ) * tmv(ji  ,jj  ) ) / 2.0
                  ! Square root of wind stress
                  ztenagm       =  SQRT( SQRT( ztaux * ztaux + ztauy * ztauy ) )

                  !---------------------
                  ! Frazil ice velocity
                  !---------------------
                  zvfrx         = zgamafr * zsqcd * ztaux / MAX(ztenagm,epsi10)
                  zvfry         = zgamafr * zsqcd * ztauy / MAX(ztenagm,epsi10)

                  !-------------------
                  ! Pack ice velocity
                  !-------------------
                  ! C-grid ice velocity
                  zindb = MAX(0.0, SIGN(1.0, at_i(ji,jj) ))
                  zvgx  = zindb * ( u_ice(ji-1,jj  ) * tmu(ji-1,jj  ) &
                     + u_ice(ji,jj    ) * tmu(ji  ,jj  ) ) / 2.0
                  zvgy  = zindb * ( v_ice(ji  ,jj-1) * tmv(ji  ,jj-1) &
                     + v_ice(ji,jj    ) * tmv(ji  ,jj  ) ) / 2.0

                  !-----------------------------------
                  ! Relative frazil/pack ice velocity
                  !-----------------------------------
                  ! absolute relative velocity
                  zvrel2        = MAX( ( zvfrx - zvgx ) * ( zvfrx - zvgx ) + &
                     ( zvfry - zvgy ) * ( zvfry - zvgy )   &
                     , 0.15 * 0.15 )
                  zvrel(ji,jj)  = SQRT(zvrel2)

                  !---------------------
                  ! Iterative procedure
                  !---------------------
                  hicol(ji,jj) = zhicrit + 0.1 
                  hicol(ji,jj) = zhicrit + hicol(ji,jj) /      & 
                     ( hicol(ji,jj) * hicol(ji,jj) - &
                     zhicrit * zhicrit ) * ztwogp * zvrel2

                  iter = 1
                  iterate_frazil = .true.

                  DO WHILE ( iter .LT. 100 .AND. iterate_frazil ) 
                     zf = ( hicol(ji,jj) - zhicrit ) * ( hicol(ji,jj)**2 - zhicrit**2 ) &
                        - hicol(ji,jj) * zhicrit * ztwogp * zvrel2
                     zfp = ( hicol(ji,jj) - zhicrit ) * ( 3.0*hicol(ji,jj) + zhicrit ) &
                        - zhicrit * ztwogp * zvrel2
                     zhicol_new = hicol(ji,jj) - zf/zfp
                     hicol(ji,jj)   = zhicol_new

                     iter = iter + 1

                  END DO ! do while

               ENDIF ! end of selection of pixels where ice forms

            END DO ! loop on ji ends
         END DO ! loop on jj ends

      ENDIF ! End of computation of frazil ice collection thickness

      !------------------------------------------------------------------------------!
      ! 4) Identify grid points where new ice forms
      !------------------------------------------------------------------------------!

      !-------------------------------------
      ! Select points for new ice formation
      !-------------------------------------
      ! This occurs if open water energy budget is negative
      nbpac = 0
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF ( tms(ji,jj) * ( qcmif(ji,jj) - qldif(ji,jj) ) > 0.e0 ) THEN
               nbpac = nbpac + 1
               npac( nbpac ) = (jj - 1) * jpi + ji
               IF ( (ji.eq.jiindx).AND.(jj.eq.jjindx) ) THEN
                  jiindex_1d = nbpac
               ENDIF
            ENDIF
         END DO
      END DO

      IF( ln_nicep ) THEN
         WRITE(numout,*) 'lim_thd_lac : nbpac = ', nbpac
      ENDIF

      !------------------------------
      ! Move from 2-D to 1-D vectors
      !------------------------------
      ! If ocean gains heat do nothing 
      ! 0therwise compute new ice formation

      IF ( nbpac > 0 ) THEN

         CALL tab_2d_1d( nbpac, zat_i_ac  (1:nbpac)     , at_i         ,       &
            jpi, jpj, npac(1:nbpac) )
         DO jl = 1, jpl
            CALL tab_2d_1d( nbpac, za_i_ac(1:nbpac,jl)  , a_i(:,:,jl)  ,       &
               jpi, jpj, npac(1:nbpac) )
            CALL tab_2d_1d( nbpac, zv_i_ac(1:nbpac,jl)  , v_i(:,:,jl)  ,       &
               jpi, jpj, npac(1:nbpac) )
            CALL tab_2d_1d( nbpac, zoa_i_ac(1:nbpac,jl) , oa_i(:,:,jl) ,       &
               jpi, jpj, npac(1:nbpac) )
            CALL tab_2d_1d( nbpac, zsmv_i_ac(1:nbpac,jl), smv_i(:,:,jl),       &
               jpi, jpj, npac(1:nbpac) )
            DO jk = 1, nlay_i
               CALL tab_2d_1d( nbpac, ze_i_ac(1:nbpac,jk,jl), e_i(:,:,jk,jl) , &
                  jpi, jpj, npac(1:nbpac) )
            END DO ! jk
         END DO ! jl

         CALL tab_2d_1d( nbpac, qldif_1d  (1:nbpac)     , qldif ,              &
            jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d( nbpac, qcmif_1d  (1:nbpac)     , qcmif ,              &
            jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d( nbpac, t_bo_b    (1:nbpac)     , t_bo  ,              &
            jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d( nbpac, fseqv_1d  (1:nbpac)     , fseqv ,              &
            jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d( nbpac, hicol_b   (1:nbpac)     , hicol ,              &
            jpi, jpj, npac(1:nbpac) )
         CALL tab_2d_1d( nbpac, zvrel_ac  (1:nbpac)     , zvrel ,              &
            jpi, jpj, npac(1:nbpac) )

         !------------------------------------------------------------------------------!
         ! 5) Compute thickness, salinity, enthalpy, age, area and volume of new ice
         !------------------------------------------------------------------------------!

         !----------------------
         ! Thickness of new ice
         !----------------------
         DO ji = 1, nbpac
            zh_newice(ji)     = hiccrit(1)
         END DO
         IF ( fraz_swi .EQ. 1.0 ) zh_newice(:) = hicol_b(:)

         !----------------------
         ! Salinity of new ice 
         !----------------------

         IF ( num_sal .EQ. 1 ) THEN
            zs_newice(:)      =   bulk_sal
         ENDIF ! num_sal

         IF ( ( num_sal .EQ. 2 ) .OR. ( num_sal .EQ. 4 ) ) THEN

            DO ji = 1, nbpac
               zs_newice(ji)  =   MIN( 4.606 + 0.91 / zh_newice(ji) , s_i_max )
               zji            =   MOD( npac(ji) - 1, jpi ) + 1
               zjj            =   ( npac(ji) - 1 ) / jpi + 1
               zs_newice(ji)  =   MIN( 0.5*sss_m(zji,zjj) , zs_newice(ji) )
            END DO ! jl

         ENDIF ! num_sal

         IF ( num_sal .EQ. 3 ) THEN
            zs_newice(:)      =   2.3
         ENDIF ! num_sal

         !-------------------------
         ! Heat content of new ice
         !-------------------------
         ! We assume that new ice is formed at the seawater freezing point
         DO ji = 1, nbpac
            ztmelts           = - tmut * zs_newice(ji) + rtt ! Melting point (K)
            ze_newice(ji)     =   rhoic * ( cpic * ( ztmelts - t_bo_b(ji) )    &
               + lfus * ( 1.0 - ( ztmelts - rtt )   &
               / ( t_bo_b(ji) - rtt ) )           &
               - rcp * ( ztmelts-rtt ) )
            ze_newice(ji)     =   MAX( ze_newice(ji) , 0.0 ) +                 &
               MAX( 0.0 , SIGN( 1.0 , - ze_newice(ji) ) )   & 
               * rhoic * lfus
         END DO ! ji
         !----------------
         ! Age of new ice
         !----------------
         DO ji = 1, nbpac
            zo_newice(ji)     = 0.0
         END DO ! ji

         !--------------------------
         ! Open water energy budget 
         !--------------------------
         DO ji = 1, nbpac
            zqbgow(ji)        = qldif_1d(ji) - qcmif_1d(ji) !<0
         END DO ! ji

         !-------------------
         ! Volume of new ice
         !-------------------
         DO ji = 1, nbpac
            zv_newice(ji)     = - zqbgow(ji) / ze_newice(ji)

            ! A fraction zfrazb of frazil ice is accreted at the ice bottom
            zfrazb        = ( TANH ( Cfrazb * ( zvrel_ac(ji) - vfrazb ) )     & 
               + 1.0 ) / 2.0 * maxfrazb
            zdh_frazb(ji) = zfrazb*zv_newice(ji)
            zv_newice(ji) = ( 1.0 - zfrazb ) * zv_newice(ji)
         END DO

         !---------------------------------
         ! Salt flux due to new ice growth
         !---------------------------------
         IF ( ( num_sal .EQ. 4 ) ) THEN 
            DO ji = 1, nbpac
               zji            = MOD( npac(ji) - 1, jpi ) + 1
               zjj            = ( npac(ji) - 1 ) / jpi + 1
               fseqv_1d(ji)   = fseqv_1d(ji) +                                     &
                  ( sss_m(zji,zjj) - bulk_sal      ) * rhoic *       &
                  zv_newice(ji) / rdt_ice
            END DO
         ELSE
            DO ji = 1, nbpac
               zji            = MOD( npac(ji) - 1, jpi ) + 1
               zjj            = ( npac(ji) - 1 ) / jpi + 1
               fseqv_1d(ji)   = fseqv_1d(ji) +                                     &
                  ( sss_m(zji,zjj) - zs_newice(ji) ) * rhoic *       &
                  zv_newice(ji) / rdt_ice
            END DO ! ji
         ENDIF

         !------------------------------------
         ! Diags for energy conservation test
         !------------------------------------
         DO ji = 1, nbpac
            ! Volume
            zji                  = MOD( npac(ji) - 1, jpi ) + 1
            zjj                  = ( npac(ji) - 1 ) / jpi + 1
            vt_i_init(zji,zjj)   = vt_i_init(zji,zjj) + zv_newice(ji)
            ! Energy
            zde                  = ze_newice(ji) / unit_fac
            zde                  = zde * area(zji,zjj) * zv_newice(ji)
            et_i_init(zji,zjj)   = et_i_init(zji,zjj) + zde
         END DO

         ! keep new ice volume in memory
         CALL tab_1d_2d( nbpac, v_newice , npac(1:nbpac), zv_newice(1:nbpac) , &
            jpi, jpj )

         !-----------------
         ! Area of new ice
         !-----------------
         DO ji = 1, nbpac
            za_newice(ji)     = zv_newice(ji) / zh_newice(ji)
            ! diagnostic
            zji                  = MOD( npac(ji) - 1, jpi ) + 1
            zjj                  = ( npac(ji) - 1 ) / jpi + 1
            diag_lat_gr(zji,zjj) = zv_newice(ji) / rdt_ice
         END DO !ji

         !------------------------------------------------------------------------------!
         ! 6) Redistribute new ice area and volume into ice categories                  !
         !------------------------------------------------------------------------------!

         !-----------------------------------------
         ! Keep old ice areas and volume in memory
         !-----------------------------------------
         zv_old(:,:) = zv_i_ac(:,:) 
         za_old(:,:) = za_i_ac(:,:)

         !-------------------------------------------
         ! Compute excessive new ice area and volume
         !-------------------------------------------
         ! If lateral ice growth gives an ice concentration gt 1, then
         ! we keep the excessive volume in memory and attribute it later
         ! to bottom accretion
         DO ji = 1, nbpac
            ! vectorize
            IF ( za_newice(ji) .GT. ( 1.0 - zat_i_ac(ji) ) ) THEN
               zda_res(ji)    = za_newice(ji) - (1.0 - zat_i_ac(ji) )
               zdv_res(ji)    = zda_res(ji) * zh_newice(ji) 
               za_newice(ji)  = za_newice(ji) - zda_res(ji)
               zv_newice(ji)  = zv_newice(ji) - zdv_res(ji)
            ELSE
               zda_res(ji) = 0.0
               zdv_res(ji) = 0.0
            ENDIF
         END DO ! ji

         !------------------------------------------------
         ! Laterally redistribute new ice volume and area
         !------------------------------------------------
         zat_i_ac(:) = 0._wp
         DO jl = 1, jpl
            DO ji = 1, nbpac
               IF(  hi_max   (jl-1)  <  zh_newice(ji)   .AND.   &
                  & zh_newice(ji)    <= hi_max   (jl)         ) THEN
                  za_i_ac (ji,jl) = za_i_ac (ji,jl) + za_newice(ji)
                  zv_i_ac (ji,jl) = zv_i_ac (ji,jl) + zv_newice(ji)
                  zat_i_ac(ji)    = zat_i_ac(ji)    + za_i_ac  (ji,jl)
                  zcatac  (ji)    = jl
               ENDIF
            END DO ! ji
         END DO ! jl

         !----------------------------------
         ! Heat content - lateral accretion
         !----------------------------------
         DO ji = 1, nbpac
            jl = zcatac(ji)                                                           ! categroy in which new ice is put
            zindb = 1._wp - MAX( 0._wp , SIGN( 1._wp , -za_old(ji,jl) ) )             ! zindb=1 if ice =0 otherwise
            zhice_old(ji,jl) = zv_old(ji,jl) / MAX( za_old(ji,jl) , epsi10 ) * zindb  ! old ice thickness
            zdhex    (ji) = MAX( 0._wp , zh_newice(ji) - zhice_old(ji,jl) )           ! difference in thickness
            zswinew  (ji) = MAX( 0._wp , SIGN( 1._wp , - za_old(ji,jl) + epsi11 ) )   ! ice totally new in jl category
         END DO

         DO jk = 1, nlay_i
            DO ji = 1, nbpac
               jl = zcatac(ji)
               zqold   = ze_i_ac(ji,jk,jl) ! [ J.m-3 ]
               zalphai = MIN( zhice_old(ji,jl) *   jk       / nlay_i , zh_newice(ji) )   &
                  &    - MIN( zhice_old(ji,jl) * ( jk - 1 ) / nlay_i , zh_newice(ji) )
               ze_i_ac(ji,jk,jl) = zswinew(ji) * ze_newice(ji)                                     &
                  + ( 1.0 - zswinew(ji) ) * ( za_old(ji,jl)  * zqold * zhice_old(ji,jl) / nlay_i   &
                  + za_newice(ji)  * ze_newice(ji) * zalphai                                       &
                  + za_newice(ji)  * ze_newice(ji) * zdhex(ji) / nlay_i ) / ( ( zv_i_ac(ji,jl) ) / nlay_i )
            END DO
         END DO

         !-----------------------------------------------
         ! Add excessive volume of new ice at the bottom
         !-----------------------------------------------
         ! If the ice concentration exceeds 1, the remaining volume of new ice
         ! is equally redistributed among all ice categories in which there is
         ! ice

         ! Fraction of level ice
         jm = 1
         zat_i_lev(:) = 0._wp

         DO jl = ice_cat_bounds(jm,1), ice_cat_bounds(jm,2)
            DO ji = 1, nbpac
               zat_i_lev(ji) = zat_i_lev(ji) + za_i_ac(ji,jl) 
            END DO
         END DO

         IF( ln_nicep ) WRITE(numout,*) ' zv_i_ac : ', zv_i_ac(jiindx, 1:jpl)
         DO jl = ice_cat_bounds(jm,1), ice_cat_bounds(jm,2)
            DO ji = 1, nbpac
               zindb = MAX( 0._wp, SIGN( 1._wp , zdv_res(ji) ) )
               zv_i_ac(ji,jl) = zv_i_ac(ji,jl) + zindb * zdv_res(ji) * za_i_ac(ji,jl) / MAX( zat_i_lev(ji) , epsi06 )
            END DO
         END DO
         IF( ln_nicep )   WRITE(numout,*) ' zv_i_ac : ', zv_i_ac(jiindx, 1:jpl)

         !---------------------------------
         ! Heat content - bottom accretion
         !---------------------------------
         jm = 1
         DO jl = ice_cat_bounds(jm,1), ice_cat_bounds(jm,2)
            DO ji = 1, nbpac
               zindb =  1._wp - MAX( 0._wp , SIGN( 1._wp , - za_i_ac(ji,jl ) ) )       ! zindb=1 if ice =0 otherwise
               zhice_old(ji,jl) = zv_i_ac(ji,jl) / MAX( za_i_ac(ji,jl) , epsi10 ) * zindb
               zdhicbot (ji,jl) = zdv_res(ji)    / MAX( za_i_ac(ji,jl) , epsi10 ) * zindb    &
                  &             +  zindb * zdh_frazb(ji)                               ! frazil ice may coalesce
               zdummy(ji,jl)    = zv_i_ac(ji,jl)/MAX(za_i_ac(ji,jl),epsi10)*zindb      ! thickness of residual ice
            END DO
         END DO

         ! old layers thicknesses and enthalpies
         DO jl = ice_cat_bounds(jm,1), ice_cat_bounds(jm,2)
            DO jk = 1, nlay_i
               DO ji = 1, nbpac
                  zthick0(ji,jk,jl) =  zhice_old(ji,jl) / nlay_i
                  zqm0   (ji,jk,jl) =  ze_i_ac(ji,jk,jl) * zthick0(ji,jk,jl)
               END DO
            END DO
         END DO
!!gm ???  why the previous do loop  if ocerwriten by the following one ?
         DO jl = ice_cat_bounds(jm,1), ice_cat_bounds(jm,2)
            DO ji = 1, nbpac
               zthick0(ji,nlay_i+1,jl) =  zdhicbot(ji,jl)
               zqm0   (ji,nlay_i+1,jl) =  ze_newice(ji) * zdhicbot(ji,jl)
            END DO ! ji
         END DO ! jl

         ! Redistributing energy on the new grid
         ze_i_ac(:,:,:) = 0._wp
         DO jl = ice_cat_bounds(jm,1), ice_cat_bounds(jm,2)
            DO jk = 1, nlay_i
               DO layer = 1, nlay_i + 1
                  DO ji = 1, nbpac
                     zindb =  1._wp -  MAX( 0._wp , SIGN( 1._wp , - za_i_ac(ji,jl) ) ) 
                     ! Redistributing energy on the new grid
                     zweight = MAX (  MIN( zhice_old(ji,jl) * layer , zdummy(ji,jl) * jk )   &
                        &    - MAX( zhice_old(ji,jl) * ( layer - 1 ) , zdummy(ji,jl) * ( jk - 1 ) ) , 0._wp )   &
                        &    /( MAX(nlay_i * zthick0(ji,layer,jl),epsi10) ) * zindb
                     ze_i_ac(ji,jk,jl) =  ze_i_ac(ji,jk,jl) + zweight * zqm0(ji,layer,jl)  
                  END DO ! ji
               END DO ! layer
            END DO ! jk
         END DO ! jl

         DO jl = ice_cat_bounds(jm,1), ice_cat_bounds(jm,2)
            DO jk = 1, nlay_i
               DO ji = 1, nbpac
                  zindb =  1._wp -  MAX( 0._wp , SIGN( 1._wp , - zv_i_ac(ji,jl) ) ) 
                  ze_i_ac(ji,jk,jl) = ze_i_ac(ji,jk,jl)   &
                     &              / MAX( zv_i_ac(ji,jl) , epsi10) * za_i_ac(ji,jl) * nlay_i * zindb
               END DO
            END DO
         END DO

         !------------
         ! Update age 
         !------------
         DO jl = 1, jpl
            DO ji = 1, nbpac
               zindb = 1._wp - MAX( 0._wp , SIGN( 1._wp , - za_i_ac(ji,jl) ) )  ! 0 if no ice and 1 if yes
               zoa_i_ac(ji,jl)  = za_old(ji,jl) * zoa_i_ac(ji,jl) / MAX( za_i_ac(ji,jl) , epsi10 ) * zindb   
            END DO 
         END DO   

         !-----------------
         ! Update salinity
         !-----------------
         IF(  num_sal == 2  .OR.  num_sal == 4  ) THEN
            DO jl = 1, jpl
               DO ji = 1, nbpac
                  zindb = 1._wp - MAX( 0._wp , SIGN( 1._wp , - zv_i_ac(ji,jl) ) )  ! 0 if no ice and 1 if yes
                  zdv   = zv_i_ac(ji,jl) - zv_old(ji,jl)
                  zsmv_i_ac(ji,jl) = ( zsmv_i_ac(ji,jl) + zdv * zs_newice(ji) ) * zindb
               END DO
            END DO   
         ENDIF

         !------------------------------------------------------------------------------!
         ! 8) Change 2D vectors to 1D vectors 
         !------------------------------------------------------------------------------!
         DO jl = 1, jpl
            CALL tab_1d_2d( nbpac, a_i (:,:,jl), npac(1:nbpac), za_i_ac (1:nbpac,jl), jpi, jpj )
            CALL tab_1d_2d( nbpac, v_i (:,:,jl), npac(1:nbpac), zv_i_ac (1:nbpac,jl), jpi, jpj )
            CALL tab_1d_2d( nbpac, oa_i(:,:,jl), npac(1:nbpac), zoa_i_ac(1:nbpac,jl), jpi, jpj )
            IF (  num_sal == 2  .OR.  num_sal == 4  )   &
               CALL tab_1d_2d( nbpac, smv_i (:,:,jl), npac(1:nbpac), zsmv_i_ac(1:nbpac,jl) , jpi, jpj )
            DO jk = 1, nlay_i
               CALL tab_1d_2d( nbpac, e_i(:,:,jk,jl), npac(1:nbpac), ze_i_ac(1:nbpac,jk,jl), jpi, jpj )
            END DO
         END DO
         CALL tab_1d_2d( nbpac, fseqv , npac(1:nbpac), fseqv_1d  (1:nbpac) , jpi, jpj )
         !
      ENDIF ! nbpac > 0

      !------------------------------------------------------------------------------!
      ! 9) Change units for e_i
      !------------------------------------------------------------------------------!    
      DO jl = 1, jpl
         DO jk = 1, nlay_i          ! heat content in 10^9 Joules
            e_i(:,:,jk,jl) = e_i(:,:,jk,jl) * area(:,:) * v_i(:,:,jl) / nlay_i  / unit_fac 
         END DO
      END DO

      !------------------------------------------------------------------------------|
      ! 10) Conservation check and changes in each ice category
      !------------------------------------------------------------------------------|
      IF( con_i ) THEN 
         CALL lim_column_sum (jpl,   v_i, vt_i_final)
         fieldid = 'v_i, limthd_lac'
         CALL lim_cons_check (vt_i_init, vt_i_final, 1.0e-6, fieldid) 
         !
         CALL lim_column_sum_energy(jpl, nlay_i, e_i, et_i_final)
         fieldid = 'e_i, limthd_lac'
         CALL lim_cons_check (et_i_final, et_i_final, 1.0e-3, fieldid) 
         !
         CALL lim_column_sum (jpl,   v_s, vt_s_final)
         fieldid = 'v_s, limthd_lac'
         CALL lim_cons_check (vt_s_init, vt_s_final, 1.0e-6, fieldid) 
         !
         !     CALL lim_column_sum (jpl,   e_s(:,:,1,:) , et_s_init)
         !     fieldid = 'e_s, limthd_lac'
         !     CALL lim_cons_check (et_s_init, et_s_final, 1.0e-3, fieldid) 
         IF( ln_nicep ) THEN
            WRITE(numout,*) ' vt_i_init : ', vt_i_init(jiindx,jjindx)
            WRITE(numout,*) ' vt_i_final: ', vt_i_final(jiindx,jjindx)
            WRITE(numout,*) ' et_i_init : ', et_i_init(jiindx,jjindx)
            WRITE(numout,*) ' et_i_final: ', et_i_final(jiindx,jjindx)
         ENDIF
         !
      ENDIF
      !
      CALL wrk_dealloc( jpij, zcatac )   ! integer
      CALL wrk_dealloc( jpij, zswinew, zv_newice, za_newice, zh_newice, ze_newice, zs_newice, zo_newice )
      CALL wrk_dealloc( jpij, zdv_res, zda_res, zat_i_ac, zat_i_lev, zdh_frazb, zvrel_ac, zqbgow, zdhex )
      CALL wrk_dealloc( jpij,jpl, zhice_old, zdummy, zdhicbot, zv_old, za_old, za_i_ac, zv_i_ac, zoa_i_ac, zsmv_i_ac )
      CALL wrk_dealloc( jpij,jkmax,jpl, ze_i_ac )
      CALL wrk_dealloc( jpij,jkmax+1,jpl, zqm0, zthick0 )
      CALL wrk_dealloc( jpi,jpj, vt_i_init, vt_i_final, vt_s_init, vt_s_final, et_i_init, et_i_final, et_s_init, zvrel )
      !
   END SUBROUTINE lim_thd_lac

#else
   !!----------------------------------------------------------------------
   !!   Default option                               NO  LIM3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_thd_lac           ! Empty routine
   END SUBROUTINE lim_thd_lac
#endif

   !!======================================================================
END MODULE limthd_lac
