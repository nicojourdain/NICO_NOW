MODULE limrhg_2
   !!======================================================================
   !!                     ***  MODULE  limrhg_2  ***
   !!   Ice rheology :  performs sea ice rheology
   !!======================================================================
   !! History :  0.0  !  1993-12  (M.A. Morales Maqueda.)  Original code
   !!            1.0  !  1994-12  (H. Goosse) 
   !!            2.0  !  2003-12  (C. Ethe, G. Madec)  F90, mpp
   !!             -   !  2006-08  (G. Madec)  surface module, ice-stress at I-point
   !!             -   !  2009-09  (G. Madec)  Huge verctor optimisation
   !!            3.3  !  2009-05  (G.Garric, C. Bricaud) addition of the lim2_evp case
   !!----------------------------------------------------------------------
#if defined   key_lim2   &&   defined key_lim2_vp
   !!----------------------------------------------------------------------
   !!   'key_lim2'                AND                   LIM-2 sea-ice model
   !!   'key_lim2_vp'                                       VP ice rheology
   !!----------------------------------------------------------------------
   !!   lim_rhg_2   : computes ice velocities
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameter
   USE dom_oce        ! ocean space and time domain
   USE sbc_oce        ! surface boundary condition: ocean variables
   USE sbc_ice        ! surface boundary condition: ice variables
   USE dom_ice_2      ! LIM2: ice domain
   USE phycst         ! physical constant
   USE ice_2          ! LIM2: ice variables
   USE lbclnk         ! lateral boundary condition - MPP exchanges
   USE lib_mpp        ! MPP library
   USE wrk_nemo       ! work arrays
   USE in_out_manager ! I/O manager
   USE prtctl         ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_rhg_2         ! routine called by lim_dyn

   REAL(wp) ::   rzero   = 0._wp   ! constant value: zero
   REAL(wp) ::   rone    = 1._wp   !            and  one

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/LIM2 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: limrhg_2.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_rhg_2( k_j1, k_jpj )
      !!-------------------------------------------------------------------
      !!                 ***  SUBROUTINR lim_rhg_2  ***
      !!
      !! ** purpose :   determines the velocity field of sea ice by using
      !!  atmospheric (wind stress) and oceanic (water stress and surface
      !!  tilt) forcings. Ice-ice interaction is described by a non-linear
      !!  viscous-plastic law including shear strength and a bulk rheology.
      !!
      !! ** Action  : - compute u_ice, v_ice the sea-ice velocity defined
      !!              at I-point
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   k_j1    ! southern j-index for ice computation
      INTEGER, INTENT(in) ::   k_jpj   ! northern j-index for ice computation
      !!
      INTEGER ::   ji, jj              ! dummy loop indices
      INTEGER ::   iter, jter          ! temporary integers
      CHARACTER (len=50) ::   charout
      REAL(wp) ::   ze11  , ze12  , ze22  , ze21               ! local scalars
      REAL(wp) ::   zt11  , zt12  , zt21  , zt22               !   -      -
      REAL(wp) ::   zvis11, zvis21, zvis12, zvis22             !   -      -
      REAL(wp) ::   zgphsx, ztagnx, zgsshx, zunw, zur, zusw    !   -      -
      REAL(wp) ::   zgphsy, ztagny, zgsshy, zvnw, zvr          !   -      -
      REAL(wp) ::   zresm,  za, zac, zmod
      REAL(wp) ::   zmpzas, zstms, zindu, zusdtp, zmassdt, zcorlal
      REAL(wp) ::   ztrace2, zdeter, zdelta, zmask, zdgp, zdgi, zdiag
      REAL(wp) ::   za1, zb1, zc1, zd1
      REAL(wp) ::   za2, zb2, zc2, zd2, zden
      REAL(wp) ::   zs11_11, zs11_12, zs11_21, zs11_22
      REAL(wp) ::   zs12_11, zs12_12, zs12_21, zs12_22
      REAL(wp) ::   zs21_11, zs21_12, zs21_21, zs21_22
      REAL(wp) ::   zs22_11, zs22_12, zs22_21, zs22_22
      REAL(wp), POINTER, DIMENSION(:,:) ::   zfrld, zmass, zcorl
      REAL(wp), POINTER, DIMENSION(:,:) ::   za1ct, za2ct, zresr
      REAL(wp), POINTER, DIMENSION(:,:) ::   zc1u, zc1v, zc2u, zc2v
      REAL(wp), POINTER, DIMENSION(:,:) ::   zsang
      REAL(wp), POINTER, DIMENSION(:,:) ::   zu0, zv0
      REAL(wp), POINTER, DIMENSION(:,:) ::   zu_n, zv_n
      REAL(wp), POINTER, DIMENSION(:,:) ::   zu_a, zv_a
      REAL(wp), POINTER, DIMENSION(:,:) ::   zviszeta, zviseta
      REAL(wp), POINTER, DIMENSION(:,:) ::   zzfrld, zztms
      REAL(wp), POINTER, DIMENSION(:,:) ::   zi1, zi2, zmasst, zpresh
      !!-------------------------------------------------------------------
      
      CALL wrk_alloc( jpi,jpj, zfrld, zmass, zcorl, za1ct, za2ct, zresr )
      CALL wrk_alloc( jpi,jpj, zc1u , zc1v , zc2u , zc2v , zsang )
      CALL wrk_alloc( jpi,jpj+2, zu0, zv0, zu_n, zv_n, zu_a, zv_a, zviszeta, zviseta, kjstart = 0 )
      CALL wrk_alloc( jpi,jpj+2, zzfrld, zztms, zi1, zi2, zmasst, zpresh, kjstart = 0 )

      !  Store initial velocities
      !  ----------------
      zztms(:,0    ) = 0._wp        ;   zzfrld(:,0    ) = 0._wp
      zztms(:,jpj+1) = 0._wp        ;   zzfrld(:,jpj+1) = 0._wp
      zu0  (:,0    ) = 0._wp        ;   zv0   (:,0    ) = 0._wp
      zu0  (:,jpj+1) = 0._wp        ;   zv0   (:,jpj+1) = 0._wp
      zztms(:,1:jpj) = tms  (:,:)   ;   zzfrld(:,1:jpj) = frld (:,:)
      zu0  (:,1:jpj) = u_ice(:,:)   ;   zv0   (:,1:jpj) = v_ice(:,:)
      zu_a (:, :   ) = zu0  (:,:)   ;   zv_a  (:, :   ) = zv0  (:,:)
      zu_n (:, :   ) = zu0  (:,:)   ;   zv_n  (:, :   ) = zv0  (:,:)

!i
      zi1   (:,:) = 0._wp
      zi2   (:,:) = 0._wp
      zpresh(:,:) = 0._wp
      zmasst(:,:) = 0._wp
!i
!!gm violant
      zfrld(:,:) =0._wp
      zcorl(:,:) =0._wp
      zmass(:,:) =0._wp
      za1ct(:,:) =0._wp
      za2ct(:,:) =0._wp
!!gm end

      zviszeta(:,:) = 0._wp
      zviseta (:,:) = 0._wp

!i    zviszeta(:,0    ) = 0._wp    ;    zviseta(:,0    ) = 0._wp
!i    zviszeta(:,jpj  ) = 0._wp    ;    zviseta(:,jpj  ) = 0._wp
!i    zviszeta(:,jpj+1) = 0._wp    ;    zviseta(:,jpj+1) = 0._wp


      ! Ice mass, ice strength, and wind stress at the center            |
      ! of the grid squares.                                             |
      !-------------------------------------------------------------------

!CDIR NOVERRCHK
      DO jj = k_j1 , k_jpj-1
!CDIR NOVERRCHK
         DO ji = 1 , jpi
            ! only the sinus changes its sign with the hemisphere
            zsang(ji,jj)  = SIGN( 1._wp, fcor(ji,jj) ) * sangvg   ! only the sinus changes its sign with the hemisphere
            !
            zmasst(ji,jj) = tms(ji,jj) * ( rhosn * hsnm(ji,jj) + rhoic * hicm(ji,jj) )
            zpresh(ji,jj) = tms(ji,jj) *  pstarh * hicm(ji,jj) * EXP( -c_rhg * frld(ji,jj) )
!!gm  :: stress given at I-point (F-point for the ocean) only compute the ponderation with the ice fraction (1-frld)
            zi1(ji,jj)    = tms(ji,jj) * ( 1._wp - frld(ji,jj) )
            zi2(ji,jj)    = tms(ji,jj) * ( 1._wp - frld(ji,jj) )
         END DO
      END DO


      !---------------------------------------------------------------------------
      !  Wind stress, coriolis and mass terms at the corners of the grid squares |
      !  Gradient of ice strenght.                                               |
      !---------------------------------------------------------------------------
         
      DO jj = k_j1+1, k_jpj-1
         DO ji = 2, jpi    ! NO vector opt.
            zstms = zztms(ji,jj  ) * wght(ji,jj,2,2) + zztms(ji-1,jj  ) * wght(ji,jj,1,2)   &
               &  + zztms(ji,jj-1) * wght(ji,jj,2,1) + zztms(ji-1,jj-1) * wght(ji,jj,1,1)
            zusw  = 1._wp / MAX( zstms, epsd )

            zt11 = zztms(ji  ,jj  ) * zzfrld(ji  ,jj  ) 
            zt12 = zztms(ji-1,jj  ) * zzfrld(ji-1,jj  ) 
            zt21 = zztms(ji  ,jj-1) * zzfrld(ji  ,jj-1) 
            zt22 = zztms(ji-1,jj-1) * zzfrld(ji-1,jj-1)

            ! Leads area.
            zfrld(ji,jj) =  (  zt11 * wght(ji,jj,2,2) + zt12 * wght(ji,jj,1,2)   &
               &             + zt21 * wght(ji,jj,2,1) + zt22 * wght(ji,jj,1,1) ) * zusw

            ! Mass and coriolis coeff. at I-point
            zmass(ji,jj) = ( zmasst(ji,jj  ) * wght(ji,jj,2,2) + zmasst(ji-1,jj  ) * wght(ji,jj,1,2)   &
               &           + zmasst(ji,jj-1) * wght(ji,jj,2,1) + zmasst(ji-1,jj-1) * wght(ji,jj,1,1) ) * zusw
            zcorl(ji,jj) = zmass(ji,jj) &
               &           *( fcor(ji,jj  ) * wght(ji,jj,2,2) + fcor(ji-1,jj  )*wght(ji,jj,1,2)   &
               &            + fcor(ji,jj-1) * wght(ji,jj,2,1) + fcor(ji-1,jj-1)*wght(ji,jj,1,1) ) * zusw

            ! Wind stress.
            ! always provide stress at I-point
            ztagnx = ( zi1(ji,jj  ) * wght(ji,jj,2,2) + zi1(ji-1,jj  ) * wght(ji,jj,1,2)   &
               &     + zi1(ji,jj-1) * wght(ji,jj,2,1) + zi1(ji-1,jj-1) * wght(ji,jj,1,1) ) * zusw * utau_ice(ji,jj)
            ztagny = ( zi2(ji,jj  ) * wght(ji,jj,2,2) + zi2(ji-1,jj  ) * wght(ji,jj,1,2)   &
               &     + zi2(ji,jj-1) * wght(ji,jj,2,1) + zi2(ji-1,jj-1) * wght(ji,jj,1,1) ) * zusw * vtau_ice(ji,jj)

            ! Gradient of ice strength
            zgphsx =   ( alambd(ji,jj,2,2,2,1) - alambd(ji,jj,2,1,2,1) ) * zpresh(ji  ,jj-1)   &
               &     + ( alambd(ji,jj,2,2,2,2) - alambd(ji,jj,2,1,2,2) ) * zpresh(ji  ,jj  )   &
               &     - ( alambd(ji,jj,2,2,1,1) + alambd(ji,jj,2,1,1,1) ) * zpresh(ji-1,jj-1)   &
               &     - ( alambd(ji,jj,2,2,1,2) + alambd(ji,jj,2,1,1,2) ) * zpresh(ji-1,jj  )

            zgphsy = - ( alambd(ji,jj,1,1,2,1) + alambd(ji,jj,1,2,2,1) ) * zpresh(ji  ,jj-1)   &
               &     - ( alambd(ji,jj,1,1,1,1) + alambd(ji,jj,1,2,1,1) ) * zpresh(ji-1,jj-1)   &
               &     + ( alambd(ji,jj,1,1,2,2) - alambd(ji,jj,1,2,2,2) ) * zpresh(ji  ,jj  )   &
               &     + ( alambd(ji,jj,1,1,1,2) - alambd(ji,jj,1,2,1,2) ) * zpresh(ji-1,jj  )

            ! Gradient of the sea surface height
            zgsshx =  (   (ssh_m(ji  ,jj  ) - ssh_m(ji-1,jj  ))/e1u(ji-1,jj  )   &
               &       +  (ssh_m(ji  ,jj-1) - ssh_m(ji-1,jj-1))/e1u(ji-1,jj-1)   ) * 0.5_wp
            zgsshy =  (   (ssh_m(ji  ,jj  ) - ssh_m(ji  ,jj-1))/e2v(ji  ,jj-1)   &
               &       +  (ssh_m(ji-1,jj  ) - ssh_m(ji-1,jj-1))/e2v(ji-1,jj-1)   ) * 0.5_wp

            ! Computation of the velocity field taking into account the ice-ice interaction.                                 
            ! Terms that are independent of the ice velocity field.
            za1ct(ji,jj) = ztagnx - zmass(ji,jj) * grav * zgsshx - zgphsx
            za2ct(ji,jj) = ztagny - zmass(ji,jj) * grav * zgsshy - zgphsy
         END DO
      END DO


      ! SOLUTION OF THE MOMENTUM EQUATION.
      !------------------------------------------
      !                                                   ! ==================== !
      DO iter = 1 , 2 * nbiter                            !    loop over iter    !
         !                                                ! ==================== !        
         zindu = MOD( iter , 2 )
         zusdtp = ( zindu * 2._wp + ( 1._wp - zindu ) * 1._wp )  * REAL( nbiter ) / rdt_ice

         ! Computation of free drift field for free slip boundary conditions.

!CDIR NOVERRCHK
         DO jj = k_j1, k_jpj-1
!CDIR NOVERRCHK
            DO ji = 1, fs_jpim1
               !- Rate of strain tensor.
               zt11 =   akappa(ji,jj,1,1) * ( zu_a(ji+1,jj) + zu_a(ji+1,jj+1) - zu_a(ji,jj  ) - zu_a(ji  ,jj+1) )  &
                  &   + akappa(ji,jj,1,2) * ( zv_a(ji+1,jj) + zv_a(ji+1,jj+1) + zv_a(ji,jj  ) + zv_a(ji  ,jj+1) )
               zt12 = - akappa(ji,jj,2,2) * ( zu_a(ji  ,jj) + zu_a(ji+1,jj  ) - zu_a(ji,jj+1) - zu_a(ji+1,jj+1) )  &
                  &   - akappa(ji,jj,2,1) * ( zv_a(ji  ,jj) + zv_a(ji+1,jj  ) + zv_a(ji,jj+1) + zv_a(ji+1,jj+1) )
               zt22 = - akappa(ji,jj,2,2) * ( zv_a(ji  ,jj) + zv_a(ji+1,jj  ) - zv_a(ji,jj+1) - zv_a(ji+1,jj+1) )  &
                  &   + akappa(ji,jj,2,1) * ( zu_a(ji  ,jj) + zu_a(ji+1,jj  ) + zu_a(ji,jj+1) + zu_a(ji+1,jj+1) )
               zt21 =   akappa(ji,jj,1,1) * ( zv_a(ji+1,jj) + zv_a(ji+1,jj+1) - zv_a(ji,jj  ) - zv_a(ji  ,jj+1) )  &
                  &   - akappa(ji,jj,1,2) * ( zu_a(ji+1,jj) + zu_a(ji+1,jj+1) + zu_a(ji,jj  ) + zu_a(ji  ,jj+1) )

               !- Rate of strain tensor. 
               zdgp = zt11 + zt22
               zdgi = zt12 + zt21
               ztrace2 = zdgp * zdgp 
               zdeter  = zt11 * zt22 - 0.25_wp * zdgi * zdgi

               !  Creep limit depends on the size of the grid.
               zdelta = MAX( SQRT( ztrace2 + ( ztrace2 - 4._wp * zdeter ) * usecc2 ),  creepl)

               !-  Computation of viscosities.
               zviszeta(ji,jj) = MAX( zpresh(ji,jj) / zdelta, etamn )
               zviseta (ji,jj) = zviszeta(ji,jj) * usecc2
            END DO
         END DO

         !-  Determination of zc1u, zc2u, zc1v and zc2v.
         DO jj = k_j1+1, k_jpj-1
            DO ji = 2, fs_jpim1   ! NO vector opt.
               !* zc1u , zc2v
               zvis11 = 2._wp * zviseta (ji-1,jj-1) + dm
               zvis12 =         zviseta (ji-1,jj-1) + dm
               zvis21 =         zviseta (ji-1,jj-1)
               zvis22 =         zviszeta(ji-1,jj-1) - zviseta(ji-1,jj-1)
               zdiag  = zvis22 * ( akappa(ji-1,jj-1,1,1) + akappa(ji-1,jj-1,2,1) )
               zs11_11 =  zvis11 * akappa(ji-1,jj-1,1,1) + zdiag
               zs12_11 =  zvis12 * akappa(ji-1,jj-1,2,2) - zvis21 * akappa(ji-1,jj-1,1,2)
               zs21_11 = -zvis12 * akappa(ji-1,jj-1,1,2) + zvis21 * akappa(ji-1,jj-1,2,2)
               zs22_11 =  zvis11 * akappa(ji-1,jj-1,2,1) + zdiag

               zvis11 = 2._wp * zviseta (ji,jj-1) + dm
               zvis22 =         zviszeta(ji,jj-1) - zviseta(ji,jj-1)
               zvis12 =         zviseta (ji,jj-1) + dm
               zvis21 =         zviseta (ji,jj-1)
               zdiag = zvis22 * ( -akappa(ji,jj-1,1,1) + akappa(ji,jj-1,2,1) )
               zs11_21 = -zvis11 * akappa(ji,jj-1,1,1) + zdiag
               zs12_21 =  zvis12 * akappa(ji,jj-1,2,2) - zvis21 * akappa(ji,jj-1,1,2)
               zs22_21 =  zvis11 * akappa(ji,jj-1,2,1) + zdiag
               zs21_21 = -zvis12 * akappa(ji,jj-1,1,2) + zvis21 * akappa(ji,jj-1,2,2)

               zvis11 = 2._wp * zviseta (ji-1,jj) + dm
               zvis22 =         zviszeta(ji-1,jj) - zviseta(ji-1,jj)
               zvis12 =         zviseta (ji-1,jj) + dm
               zvis21 =         zviseta (ji-1,jj)
               zdiag  = zvis22 * ( akappa(ji-1,jj,1,1) + akappa(ji-1,jj,2,1) )
               zs11_12 =  zvis11 * akappa(ji-1,jj,1,1) + zdiag
               zs12_12 = -zvis12 * akappa(ji-1,jj,2,2) - zvis21 * akappa(ji-1,jj,1,2)
               zs22_12 =  zvis11 * akappa(ji-1,jj,2,1) + zdiag
               zs21_12 = -zvis12 * akappa(ji-1,jj,1,2) - zvis21 * akappa(ji-1,jj,2,2)

               zvis11 = 2._wp * zviseta (ji,jj) + dm
               zvis22 =         zviszeta(ji,jj) - zviseta(ji,jj)
               zvis12 =         zviseta (ji,jj) + dm
               zvis21 =         zviseta (ji,jj)
               zdiag = zvis22 * ( -akappa(ji,jj,1,1) + akappa(ji,jj,2,1) )
               zs11_22 = -zvis11 * akappa(ji,jj,1,1) + zdiag
               zs12_22 = -zvis12 * akappa(ji,jj,2,2) - zvis21 * akappa(ji,jj,1,2)
               zs22_22 =  zvis11 * akappa(ji,jj,2,1) + zdiag
               zs21_22 = -zvis12 * akappa(ji,jj,1,2) - zvis21 * akappa(ji,jj,2,2)

               zc1u(ji,jj) = + alambd(ji,jj,2,2,2,1) * zs11_21 + alambd(ji,jj,2,2,2,2) * zs11_22   &
                  &          - alambd(ji,jj,2,2,1,1) * zs11_11 - alambd(ji,jj,2,2,1,2) * zs11_12   &
                  &          - alambd(ji,jj,1,1,2,1) * zs12_21 - alambd(ji,jj,1,1,1,1) * zs12_11   &
                  &          + alambd(ji,jj,1,1,2,2) * zs12_22 + alambd(ji,jj,1,1,1,2) * zs12_12   &
                  &          + alambd(ji,jj,1,2,1,1) * zs21_11 + alambd(ji,jj,1,2,2,1) * zs21_21   &
                  &          + alambd(ji,jj,1,2,1,2) * zs21_12 + alambd(ji,jj,1,2,2,2) * zs21_22   &
                  &          - alambd(ji,jj,2,1,1,1) * zs22_11 - alambd(ji,jj,2,1,2,1) * zs22_21   &
                  &          - alambd(ji,jj,2,1,1,2) * zs22_12 - alambd(ji,jj,2,1,2,2) * zs22_22

               zc2u(ji,jj) = + alambd(ji,jj,2,2,2,1) * zs21_21 + alambd(ji,jj,2,2,2,2) * zs21_22   &
                  &          - alambd(ji,jj,2,2,1,1) * zs21_11 - alambd(ji,jj,2,2,1,2) * zs21_12   &
                  &          - alambd(ji,jj,1,1,2,1) * zs22_21 - alambd(ji,jj,1,1,1,1) * zs22_11   &
                  &          + alambd(ji,jj,1,1,2,2) * zs22_22 + alambd(ji,jj,1,1,1,2) * zs22_12   &
                  &          - alambd(ji,jj,1,2,1,1) * zs11_11 - alambd(ji,jj,1,2,2,1) * zs11_21   &
                  &          - alambd(ji,jj,1,2,1,2) * zs11_12 - alambd(ji,jj,1,2,2,2) * zs11_22   &
                  &          + alambd(ji,jj,2,1,1,1) * zs12_11 + alambd(ji,jj,2,1,2,1) * zs12_21   &
                  &          + alambd(ji,jj,2,1,1,2) * zs12_12 + alambd(ji,jj,2,1,2,2) * zs12_22

               !* zc1v , zc2v.
               zvis11 = 2._wp * zviseta (ji-1,jj-1) + dm
               zvis22 =         zviszeta(ji-1,jj-1) - zviseta(ji-1,jj-1)
               zvis12 =         zviseta (ji-1,jj-1) + dm
               zvis21 =         zviseta (ji-1,jj-1)
               zdiag = zvis22 * ( akappa(ji-1,jj-1,1,2) + akappa(ji-1,jj-1,2,2) )
               zs11_11 =  zvis11 * akappa(ji-1,jj-1,1,2) + zdiag
               zs12_11 = -zvis12 * akappa(ji-1,jj-1,2,1) + zvis21 * akappa(ji-1,jj-1,1,1)
               zs22_11 =  zvis11 * akappa(ji-1,jj-1,2,2) + zdiag
               zs21_11 =  zvis12 * akappa(ji-1,jj-1,1,1) - zvis21 * akappa(ji-1,jj-1,2,1)
 
               zvis11 = 2._wp * zviseta (ji,jj-1) + dm
               zvis22 =         zviszeta(ji,jj-1) - zviseta(ji,jj-1)
               zvis12 =         zviseta (ji,jj-1) + dm
               zvis21 =         zviseta (ji,jj-1)
               zdiag = zvis22 * ( akappa(ji,jj-1,1,2) + akappa(ji,jj-1,2,2) )
               zs11_21 =  zvis11 * akappa(ji,jj-1,1,2) + zdiag
               zs12_21 = -zvis12 * akappa(ji,jj-1,2,1) - zvis21 * akappa(ji,jj-1,1,1)
               zs22_21 =  zvis11 * akappa(ji,jj-1,2,2) + zdiag
               zs21_21 = -zvis12 * akappa(ji,jj-1,1,1) - zvis21 * akappa(ji,jj-1,2,1)

               zvis11 = 2._wp * zviseta (ji-1,jj) + dm
               zvis22 =         zviszeta(ji-1,jj) - zviseta(ji-1,jj)
               zvis12 =         zviseta (ji-1,jj) + dm
               zvis21 =         zviseta (ji-1,jj)
               zdiag = zvis22 * ( akappa(ji-1,jj,1,2) - akappa(ji-1,jj,2,2) )
               zs11_12 =  zvis11 * akappa(ji-1,jj,1,2) + zdiag
               zs12_12 = -zvis12 * akappa(ji-1,jj,2,1) + zvis21 * akappa(ji-1,jj,1,1)
               zs22_12 = -zvis11 * akappa(ji-1,jj,2,2) + zdiag
               zs21_12 =  zvis12 * akappa(ji-1,jj,1,1) - zvis21 * akappa(ji-1,jj,2,1)

               zvis11 = 2._wp * zviseta (ji,jj) + dm
               zvis22 =         zviszeta(ji,jj) - zviseta(ji,jj)
               zvis12 =         zviseta (ji,jj) + dm
               zvis21 =         zviseta (ji,jj)
               zdiag = zvis22 * ( akappa(ji,jj,1,2) - akappa(ji,jj,2,2) )
               zs11_22 =  zvis11 * akappa(ji,jj,1,2) + zdiag
               zs12_22 = -zvis12 * akappa(ji,jj,2,1) - zvis21 * akappa(ji,jj,1,1)
               zs22_22 = -zvis11 * akappa(ji,jj,2,2) + zdiag
               zs21_22 = -zvis12 * akappa(ji,jj,1,1) - zvis21 * akappa(ji,jj,2,1)

               zc1v(ji,jj) = + alambd(ji,jj,2,2,2,1) * zs11_21 + alambd(ji,jj,2,2,2,2) * zs11_22   &
                  &          - alambd(ji,jj,2,2,1,1) * zs11_11 - alambd(ji,jj,2,2,1,2) * zs11_12   &
                  &          - alambd(ji,jj,1,1,2,1) * zs12_21 - alambd(ji,jj,1,1,1,1) * zs12_11   &
                  &          + alambd(ji,jj,1,1,2,2) * zs12_22 + alambd(ji,jj,1,1,1,2) * zs12_12   &
                  &          + alambd(ji,jj,1,2,1,1) * zs21_11 + alambd(ji,jj,1,2,2,1) * zs21_21   &
                  &          + alambd(ji,jj,1,2,1,2) * zs21_12 + alambd(ji,jj,1,2,2,2) * zs21_22   &
                  &          - alambd(ji,jj,2,1,1,1) * zs22_11 - alambd(ji,jj,2,1,2,1) * zs22_21   &
                  &          - alambd(ji,jj,2,1,1,2) * zs22_12 - alambd(ji,jj,2,1,2,2) * zs22_22

               zc2v(ji,jj) = + alambd(ji,jj,2,2,2,1) * zs21_21 + alambd(ji,jj,2,2,2,2) * zs21_22   &
                  &          - alambd(ji,jj,2,2,1,1) * zs21_11 - alambd(ji,jj,2,2,1,2) * zs21_12   &
                  &          - alambd(ji,jj,1,1,2,1) * zs22_21 - alambd(ji,jj,1,1,1,1) * zs22_11   &
                  &          + alambd(ji,jj,1,1,2,2) * zs22_22 + alambd(ji,jj,1,1,1,2) * zs22_12   &
                  &          - alambd(ji,jj,1,2,1,1) * zs11_11 - alambd(ji,jj,1,2,2,1) * zs11_21   &
                  &          - alambd(ji,jj,1,2,1,2) * zs11_12 - alambd(ji,jj,1,2,2,2) * zs11_22   &
                  &          + alambd(ji,jj,2,1,1,1) * zs12_11 + alambd(ji,jj,2,1,2,1) * zs12_21   &
                  &          + alambd(ji,jj,2,1,1,2) * zs12_12 + alambd(ji,jj,2,1,2,2) * zs12_22
            END DO
         END DO

         ! GAUSS-SEIDEL method
         !                                                      ! ================ !
iflag:   DO jter = 1 , nbitdr                                   !    Relaxation    !
            !                                                   ! ================ !
!CDIR NOVERRCHK
            DO jj = k_j1+1, k_jpj-1
!CDIR NOVERRCHK
               DO ji = 2, fs_jpim1   ! NO vector opt.
                  !
                  ze11 =   akappa(ji,jj-1,1,1) * zu_a(ji+1,jj) + akappa(ji,jj-1,1,2) * zv_a(ji+1,jj)
                  ze12 = + akappa(ji,jj-1,2,2) * zu_a(ji+1,jj) - akappa(ji,jj-1,2,1) * zv_a(ji+1,jj)
                  ze22 = + akappa(ji,jj-1,2,2) * zv_a(ji+1,jj) + akappa(ji,jj-1,2,1) * zu_a(ji+1,jj)
                  ze21 =   akappa(ji,jj-1,1,1) * zv_a(ji+1,jj) - akappa(ji,jj-1,1,2) * zu_a(ji+1,jj)
                  zvis11 = 2._wp * zviseta (ji,jj-1) + dm
                  zvis22 =         zviszeta(ji,jj-1) - zviseta(ji,jj-1)
                  zvis12 =         zviseta (ji,jj-1) + dm
                  zvis21 =         zviseta (ji,jj-1)
                  zdiag = zvis22 * ( ze11 + ze22 )
                  zs11_21 =  zvis11 * ze11 + zdiag
                  zs12_21 =  zvis12 * ze12 + zvis21 * ze21
                  zs22_21 =  zvis11 * ze22 + zdiag
                  zs21_21 =  zvis12 * ze21 + zvis21 * ze12

                  ze11 =   akappa(ji-1,jj,1,1) * ( zu_a(ji  ,jj+1) - zu_a(ji-1,jj+1) )   &
                     &   + akappa(ji-1,jj,1,2) * ( zv_a(ji  ,jj+1) + zv_a(ji-1,jj+1) )
                  ze12 = + akappa(ji-1,jj,2,2) * ( zu_a(ji-1,jj+1) + zu_a(ji  ,jj+1) )   &
                     &   - akappa(ji-1,jj,2,1) * ( zv_a(ji-1,jj+1) + zv_a(ji  ,jj+1) )
                  ze22 = + akappa(ji-1,jj,2,2) * ( zv_a(ji-1,jj+1) + zv_a(ji  ,jj+1) )   &
                     &   + akappa(ji-1,jj,2,1) * ( zu_a(ji-1,jj+1) + zu_a(ji  ,jj+1) )
                  ze21 =   akappa(ji-1,jj,1,1) * ( zv_a(ji  ,jj+1) - zv_a(ji-1,jj+1) )   &
                     &   - akappa(ji-1,jj,1,2) * ( zu_a(ji  ,jj+1) + zu_a(ji-1,jj+1) )
                  zvis11 = 2._wp * zviseta (ji-1,jj) + dm
                  zvis22 =         zviszeta(ji-1,jj) - zviseta(ji-1,jj)
                  zvis12 =         zviseta (ji-1,jj) + dm
                  zvis21 =         zviseta (ji-1,jj)
                  zdiag = zvis22 * ( ze11 + ze22 )
                  zs11_12 =  zvis11 * ze11 + zdiag
                  zs12_12 =  zvis12 * ze12 + zvis21 * ze21
                  zs22_12 =  zvis11 * ze22 + zdiag
                  zs21_12 =  zvis12 * ze21 + zvis21 * ze12

                  ze11 =   akappa(ji,jj,1,1) * ( zu_a(ji+1,jj) + zu_a(ji+1,jj+1) - zu_a(ji  ,jj+1) )   &
                     &   + akappa(ji,jj,1,2) * ( zv_a(ji+1,jj) + zv_a(ji+1,jj+1) + zv_a(ji  ,jj+1) )
                  ze12 = - akappa(ji,jj,2,2) * ( zu_a(ji+1,jj) - zu_a(ji  ,jj+1) - zu_a(ji+1,jj+1) )   &
                     &   - akappa(ji,jj,2,1) * ( zv_a(ji+1,jj) + zv_a(ji  ,jj+1) + zv_a(ji+1,jj+1) )
                  ze22 = - akappa(ji,jj,2,2) * ( zv_a(ji+1,jj) - zv_a(ji  ,jj+1) - zv_a(ji+1,jj+1) )   &
                     &   + akappa(ji,jj,2,1) * ( zu_a(ji+1,jj) + zu_a(ji  ,jj+1) + zu_a(ji+1,jj+1) )
                  ze21 =   akappa(ji,jj,1,1) * ( zv_a(ji+1,jj) + zv_a(ji+1,jj+1) - zv_a(ji  ,jj+1) )   &
                     &   - akappa(ji,jj,1,2) * ( zu_a(ji+1,jj) + zu_a(ji+1,jj+1) + zu_a(ji  ,jj+1) )
                  zvis11 = 2._wp * zviseta (ji,jj) + dm
                  zvis22 =         zviszeta(ji,jj) - zviseta(ji,jj)
                  zvis12 =         zviseta (ji,jj) + dm
                  zvis21 =         zviseta (ji,jj)
                  zdiag = zvis22 * ( ze11 + ze22 )
                  zs11_22 =  zvis11 * ze11 + zdiag
                  zs12_22 =  zvis12 * ze12 + zvis21 * ze21
                  zs22_22 =  zvis11 * ze22 + zdiag
                  zs21_22 =  zvis12 * ze21 + zvis21 * ze12

            ! 2nd part
                  ze11 =   akappa(ji-1,jj-1,1,1) * ( zu_a(ji  ,jj-1) - zu_a(ji-1,jj-1) - zu_a(ji-1,jj) )   &
                     &   + akappa(ji-1,jj-1,1,2) * ( zv_a(ji  ,jj-1) + zv_a(ji-1,jj-1) + zv_a(ji-1,jj) )
                  ze12 = - akappa(ji-1,jj-1,2,2) * ( zu_a(ji-1,jj-1) + zu_a(ji  ,jj-1) - zu_a(ji-1,jj) )   &
                     &   - akappa(ji-1,jj-1,2,1) * ( zv_a(ji-1,jj-1) + zv_a(ji  ,jj-1) + zv_a(ji-1,jj) )
                  ze22 = - akappa(ji-1,jj-1,2,2) * ( zv_a(ji-1,jj-1) + zv_a(ji  ,jj-1) - zv_a(ji-1,jj) )   &
                     &   + akappa(ji-1,jj-1,2,1) * ( zu_a(ji-1,jj-1) + zu_a(ji  ,jj-1) + zu_a(ji-1,jj) )
                  ze21 =   akappa(ji-1,jj-1,1,1) * ( zv_a(ji  ,jj-1) - zv_a(ji-1,jj-1) - zv_a(ji-1,jj) )   &
                     &  -  akappa(ji-1,jj-1,1,2) * ( zu_a(ji  ,jj-1) + zu_a(ji-1,jj-1) + zu_a(ji-1,jj) )
                  zvis11 = 2._wp * zviseta (ji-1,jj-1) + dm
                  zvis22 =         zviszeta(ji-1,jj-1) - zviseta(ji-1,jj-1)
                  zvis12 =         zviseta (ji-1,jj-1) + dm
                  zvis21 =         zviseta (ji-1,jj-1)
                  zdiag = zvis22 * ( ze11 + ze22 )
                  zs11_11 =  zvis11 * ze11 + zdiag
                  zs12_11 =  zvis12 * ze12 + zvis21 * ze21
                  zs22_11 =  zvis11 * ze22 + zdiag
                  zs21_11 =  zvis12 * ze21 + zvis21 * ze12

                  ze11 =   akappa(ji,jj-1,1,1) * ( zu_a(ji+1,jj-1) - zu_a(ji  ,jj-1) )   &
                     &   + akappa(ji,jj-1,1,2) * ( zv_a(ji+1,jj-1) + zv_a(ji  ,jj-1) )
                  ze12 = - akappa(ji,jj-1,2,2) * ( zu_a(ji  ,jj-1) + zu_a(ji+1,jj-1) )   &
                     &   - akappa(ji,jj-1,2,1) * ( zv_a(ji  ,jj-1) + zv_a(ji+1,jj-1) )
                  ze22 = - akappa(ji,jj-1,2,2) * ( zv_a(ji  ,jj-1) + zv_a(ji+1,jj-1) )   &
                     &   + akappa(ji,jj-1,2,1) * ( zu_a(ji  ,jj-1) + zu_a(ji+1,jj-1) )
                  ze21 =   akappa(ji,jj-1,1,1) * ( zv_a(ji+1,jj-1) - zv_a(ji  ,jj-1) )   &
                     &   - akappa(ji,jj-1,1,2) * ( zu_a(ji+1,jj-1) + zu_a(ji  ,jj-1) )
                  zvis11 = 2._wp * zviseta (ji,jj-1) + dm
                  zvis22 =         zviszeta(ji,jj-1) - zviseta(ji,jj-1)
                  zvis12 =         zviseta (ji,jj-1) + dm
                  zvis21 =         zviseta (ji,jj-1)
                  zdiag = zvis22 * ( ze11 + ze22 )
                  zs11_21 =  zs11_21 + zvis11 * ze11 + zdiag
                  zs12_21 =  zs12_21 + zvis12 * ze12 + zvis21 * ze21
                  zs22_21 =  zs22_21 + zvis11 * ze22 + zdiag
                  zs21_21 =  zs21_21 + zvis12 * ze21 + zvis21 * ze12

                  ze11 = - akappa(ji-1,jj,1,1) * zu_a(ji-1,jj) + akappa(ji-1,jj,1,2) * zv_a(ji-1,jj)
                  ze12 = - akappa(ji-1,jj,2,2) * zu_a(ji-1,jj) - akappa(ji-1,jj,2,1) * zv_a(ji-1,jj)
                  ze22 = - akappa(ji-1,jj,2,2) * zv_a(ji-1,jj) + akappa(ji-1,jj,2,1) * zu_a(ji-1,jj)
                  ze21 = - akappa(ji-1,jj,1,1) * zv_a(ji-1,jj) - akappa(ji-1,jj,1,2) * zu_a(ji-1,jj)
                  zvis11 = 2._wp * zviseta (ji-1,jj) + dm
                  zvis22 =         zviszeta(ji-1,jj) - zviseta(ji-1,jj)
                  zvis12 =         zviseta (ji-1,jj) + dm
                  zvis21 =         zviseta (ji-1,jj)
                  zdiag = zvis22 * ( ze11 + ze22 )
                  zs11_12 =  zs11_12 + zvis11 * ze11 + zdiag
                  zs12_12 =  zs12_12 + zvis12 * ze12 + zvis21 * ze21
                  zs22_12 =  zs22_12 + zvis11 * ze22 + zdiag
                  zs21_12 =  zs21_12 + zvis12 * ze21 + zvis21 * ze12

                  zd1 = + alambd(ji,jj,2,2,2,1) * zs11_21 + alambd(ji,jj,2,2,2,2) * zs11_22  &
                     &  - alambd(ji,jj,2,2,1,1) * zs11_11 - alambd(ji,jj,2,2,1,2) * zs11_12  &
                     &  - alambd(ji,jj,1,1,2,1) * zs12_21 - alambd(ji,jj,1,1,1,1) * zs12_11  &
                     &  + alambd(ji,jj,1,1,2,2) * zs12_22 + alambd(ji,jj,1,1,1,2) * zs12_12  &
                     &  + alambd(ji,jj,1,2,1,1) * zs21_11 + alambd(ji,jj,1,2,2,1) * zs21_21  &
                     &  + alambd(ji,jj,1,2,1,2) * zs21_12 + alambd(ji,jj,1,2,2,2) * zs21_22  &
                     &  - alambd(ji,jj,2,1,1,1) * zs22_11 - alambd(ji,jj,2,1,2,1) * zs22_21  &
                     &  - alambd(ji,jj,2,1,1,2) * zs22_12 - alambd(ji,jj,2,1,2,2) * zs22_22

                  zd2 = + alambd(ji,jj,2,2,2,1) * zs21_21 + alambd(ji,jj,2,2,2,2) * zs21_22  &
                     &  - alambd(ji,jj,2,2,1,1) * zs21_11 - alambd(ji,jj,2,2,1,2) * zs21_12  &
                     &  - alambd(ji,jj,1,1,2,1) * zs22_21 - alambd(ji,jj,1,1,1,1) * zs22_11  &
                     &  + alambd(ji,jj,1,1,2,2) * zs22_22 + alambd(ji,jj,1,1,1,2) * zs22_12  &
                     &  - alambd(ji,jj,1,2,1,1) * zs11_11 - alambd(ji,jj,1,2,2,1) * zs11_21  &
                     &  - alambd(ji,jj,1,2,1,2) * zs11_12 - alambd(ji,jj,1,2,2,2) * zs11_22  &
                     &  + alambd(ji,jj,2,1,1,1) * zs12_11 + alambd(ji,jj,2,1,2,1) * zs12_21  &
                     &  + alambd(ji,jj,2,1,1,2) * zs12_12 + alambd(ji,jj,2,1,2,2) * zs12_22

                  zur     = zu_a(ji,jj) - u_oce(ji,jj)
                  zvr     = zv_a(ji,jj) - v_oce(ji,jj)
!!!!
                  zmod    = SQRT( zur*zur + zvr*zvr ) * ( 1._wp - zfrld(ji,jj) )
                  za      = rhoco * zmod
!!!!
!!gm chg resul    za      = rhoco * SQRT( zur*zur + zvr*zvr ) * ( 1._wp - zfrld(ji,jj) )
                  zac     = za * cangvg
                  zmpzas  = alpha * zcorl(ji,jj) + za * zsang(ji,jj)
                  zmassdt = zusdtp * zmass(ji,jj)
                  zcorlal = ( 1._wp - alpha ) * zcorl(ji,jj)

                  za1 =  zmassdt * zu0(ji,jj) + zcorlal * zv0(ji,jj) + za1ct(ji,jj)   &
                     &        + za * ( cangvg * u_oce(ji,jj) - zsang(ji,jj) * v_oce(ji,jj) )
                  za2 =  zmassdt * zv0(ji,jj) - zcorlal * zu0(ji,jj) + za2ct(ji,jj)   &
                     &        + za * ( cangvg * v_oce(ji,jj) + zsang(ji,jj) * u_oce(ji,jj) )
                  zb1    = zmassdt + zac - zc1u(ji,jj)
                  zb2    = zmpzas        - zc2u(ji,jj)
                  zc1    = zmpzas        + zc1v(ji,jj)
                  zc2    = zmassdt + zac - zc2v(ji,jj)
                  zdeter = zc1 * zb2 + zc2 * zb1
                  zden   = SIGN( rone, zdeter) / MAX( epsd , ABS( zdeter ) )
                  zunw   = (  ( za1 + zd1 ) * zc2 + ( za2 + zd2 ) * zc1 ) * zden
                  zvnw   = (  ( za2 + zd2 ) * zb1 - ( za1 + zd1 ) * zb2 ) * zden
                  zmask  = ( 1._wp - MAX( rzero, SIGN( rone , 1._wp - zmass(ji,jj) ) ) ) * tmu(ji,jj)

                  zu_n(ji,jj) = ( zu_a(ji,jj) + om * ( zunw - zu_a(ji,jj) ) * tmu(ji,jj) ) * zmask
                  zv_n(ji,jj) = ( zv_a(ji,jj) + om * ( zvnw - zv_a(ji,jj) ) * tmu(ji,jj) ) * zmask
               END DO
            END DO

            CALL lbc_lnk( zu_n(:,1:jpj), 'I', -1. )
            CALL lbc_lnk( zv_n(:,1:jpj), 'I', -1. )

            ! Test of Convergence
            DO jj = k_j1+1 , k_jpj-1
               zresr(:,jj) = MAX( ABS( zu_a(:,jj) - zu_n(:,jj) ) , ABS( zv_a(:,jj) - zv_n(:,jj) ) )
            END DO
            zresm = MAXVAL( zresr(1:jpi,k_j1+1:k_jpj-1) )
!!!! this should be faster on scalar processor
!           zresm = MAXVAL(  MAX( ABS( zu_a(1:jpi,k_j1+1:k_jpj-1) - zu_n(1:jpi,k_j1+1:k_jpj-1) ),   &
!              &                  ABS( zv_a(1:jpi,k_j1+1:k_jpj-1) - zv_n(1:jpi,k_j1+1:k_jpj-1) ) )  )
!!!!
            IF( lk_mpp )   CALL mpp_max( zresm )   ! max over the global domain

            DO jj = k_j1, k_jpj
               zu_a(:,jj) = zu_n(:,jj)
               zv_a(:,jj) = zv_n(:,jj)
            END DO

            IF( zresm <= resl )   EXIT   iflag

            !                                                   ! ================ !
         END DO    iflag                                        !  end Relaxation  !
         !                                                      ! ================ !

         IF( zindu == 0 ) THEN      ! even iteration
            DO jj = k_j1 , k_jpj-1
               zu0(:,jj) = zu_a(:,jj)
               zv0(:,jj) = zv_a(:,jj)
            END DO
         ENDIF
         !                                                ! ==================== !
      END DO                                              !  end loop over iter  !
      !                                                   ! ==================== !

      u_ice(:,:) = zu_a(:,1:jpj)
      v_ice(:,:) = zv_a(:,1:jpj)

      IF(ln_ctl) THEN
         WRITE(charout,FMT="('lim_rhg  : res =',D23.16, ' iter =',I4)") zresm, jter
         CALL prt_ctl_info(charout)
         CALL prt_ctl(tab2d_1=u_ice, clinfo1=' lim_rhg  : u_ice :', tab2d_2=v_ice, clinfo2=' v_ice :')
      ENDIF

      CALL wrk_dealloc( jpi,jpj, zfrld, zmass, zcorl, za1ct, za2ct, zresr )
      CALL wrk_dealloc( jpi,jpj, zc1u , zc1v , zc2u , zc2v , zsang )
      CALL wrk_dealloc( jpi,jpj+2, zu0, zv0, zu_n, zv_n, zu_a, zv_a, zviszeta, zviseta, kjstart = 0 )
      CALL wrk_dealloc( jpi,jpj+2, zzfrld, zztms, zi1, zi2, zmasst, zpresh, kjstart = 0 )

   END SUBROUTINE lim_rhg_2

#else
   !!----------------------------------------------------------------------
   !!   Default option        Dummy module      NO VP & LIM-2 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_rhg_2( k1 , k2 )       ! Dummy routine
      WRITE(*,*) 'lim_rhg_2: You should not have seen this print! error?', k1, k2
   END SUBROUTINE lim_rhg_2
#endif

   !!==============================================================================
END MODULE limrhg_2
