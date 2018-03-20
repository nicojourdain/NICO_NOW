MODULE zpshde
   !!======================================================================
   !!                       ***  MODULE zpshde   ***
   !! z-coordinate + partial step : Horizontal Derivative at ocean bottom level
   !!======================================================================
   !! History :  OPA  !  2002-04  (A. Bozec)  Original code
   !!   NEMO     1.0  !  2002-08  (G. Madec E. Durand)  Optimization and Free form
   !!             -   !  2004-03  (C. Ethe)  adapted for passive tracers
   !!            3.3  !  2010-05  (C. Ethe, G. Madec)  merge TRC-TRA 
   !!======================================================================
   
   !!----------------------------------------------------------------------
   !!   zps_hde      :  Horizontal DErivative of T, S and rd at the last
   !!                   ocean level (Z-coord. with Partial Steps)
   !!----------------------------------------------------------------------
   USE oce             ! ocean: dynamics and tracers variables
   USE dom_oce         ! domain: ocean variables
   USE phycst          ! physical constants
   USE eosbn2          ! ocean equation of state
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! lateral boundary conditions (or mpp link)
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! Memory allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zps_hde    ! routine called by step.F90

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: zpshde.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE zps_hde( kt, kjpt, pta, pgtu, pgtv,   &
                                 prd, pgru, pgrv    )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE zps_hde  ***
      !!                    
      !! ** Purpose :   Compute the horizontal derivative of T, S and rho
      !!      at u- and v-points with a linear interpolation for z-coordinate
      !!      with partial steps.
      !!
      !! ** Method  :   In z-coord with partial steps, scale factors on last 
      !!      levels are different for each grid point, so that T, S and rd 
      !!      points are not at the same depth as in z-coord. To have horizontal
      !!      gradients again, we interpolate T and S at the good depth : 
      !!      Linear interpolation of T, S   
      !!         Computation of di(tb) and dj(tb) by vertical interpolation:
      !!          di(t) = t~ - t(i,j,k) or t(i+1,j,k) - t~
      !!          dj(t) = t~ - t(i,j,k) or t(i,j+1,k) - t~
      !!         This formulation computes the two cases:
      !!                 CASE 1                   CASE 2  
      !!         k-1  ___ ___________   k-1   ___ ___________
      !!                    Ti  T~                  T~  Ti+1
      !!                  _____                        _____
      !!         k        |   |Ti+1     k           Ti |   |
      !!                  |   |____                ____|   |
      !!              ___ |   |   |           ___  |   |   |
      !!                  
      !!      case 1->   e3w(i+1) >= e3w(i) ( and e3w(j+1) >= e3w(j) ) then
      !!          t~ = t(i+1,j  ,k) + (e3w(i+1) - e3w(i)) * dk(Ti+1)/e3w(i+1)
      !!        ( t~ = t(i  ,j+1,k) + (e3w(j+1) - e3w(j)) * dk(Tj+1)/e3w(j+1)  )
      !!          or
      !!      case 2->   e3w(i+1) <= e3w(i) ( and e3w(j+1) <= e3w(j) ) then
      !!          t~ = t(i,j,k) + (e3w(i) - e3w(i+1)) * dk(Ti)/e3w(i )
      !!        ( t~ = t(i,j,k) + (e3w(j) - e3w(j+1)) * dk(Tj)/e3w(j ) )
      !!          Idem for di(s) and dj(s)          
      !!
      !!      For rho, we call eos_insitu_2d which will compute rd~(t~,s~) at 
      !!      the good depth zh from interpolated T and S for the different
      !!      formulation of the equation of state (eos).
      !!      Gradient formulation for rho :
      !!          di(rho) = rd~ - rd(i,j,k) or rd(i+1,j,k) - rd~
      !!
      !! ** Action  : - pgtu, pgtv: horizontal gradient of tracer at u- & v-points
      !!              - pgru, pgrv: horizontal gradient of rho (if present) at u- & v-points 
      !!----------------------------------------------------------------------
      !
      INTEGER                              , INTENT(in   )           ::  kt          ! ocean time-step index
      INTEGER                              , INTENT(in   )           ::  kjpt        ! number of tracers
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt), INTENT(in   )           ::  pta         ! 4D tracers fields
      REAL(wp), DIMENSION(jpi,jpj,    kjpt), INTENT(  out)           ::  pgtu, pgtv  ! hor. grad. of ptra at u- & v-pts 
      REAL(wp), DIMENSION(jpi,jpj,jpk     ), INTENT(in   ), OPTIONAL ::  prd         ! 3D density anomaly fields
      REAL(wp), DIMENSION(jpi,jpj         ), INTENT(  out), OPTIONAL ::  pgru, pgrv  ! hor. grad. of prd at u- & v-pts 
      !
      INTEGER  ::   ji, jj, jn      ! Dummy loop indices
      INTEGER  ::   iku, ikv, ikum1, ikvm1   ! partial step level (ocean bottom level) at u- and v-points
      REAL(wp) ::  ze3wu, ze3wv, zmaxu, zmaxv  ! temporary scalars
      REAL(wp), POINTER, DIMENSION(:,:  ) ::  zri, zrj, zhi, zhj
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  zti, ztj    ! interpolated value of tracer
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start( 'zps_hde')
      !
      CALL wrk_alloc( jpi, jpj,       zri, zrj, zhi, zhj ) 
      CALL wrk_alloc( jpi, jpj, kjpt, zti, ztj           ) 
      !
      DO jn = 1, kjpt      !==   Interpolation of tracers at the last ocean level   ==!
         !
# if defined key_vectopt_loop
         jj = 1
         DO ji = 1, jpij-jpi   ! vector opt. (forced unrolled)
# else
         DO jj = 1, jpjm1
            DO ji = 1, jpim1
# endif
               iku = mbku(ji,jj)   ;   ikum1 = MAX( iku - 1 , 1 )    ! last and before last ocean level at u- & v-points
               ikv = mbkv(ji,jj)   ;   ikvm1 = MAX( ikv - 1 , 1 )    ! if level first is a p-step, ik.m1=1
               ze3wu = fse3w(ji+1,jj  ,iku) - fse3w(ji,jj,iku)
               ze3wv = fse3w(ji  ,jj+1,ikv) - fse3w(ji,jj,ikv)
               !
               ! i- direction
               IF( ze3wu >= 0._wp ) THEN      ! case 1
                  zmaxu =  ze3wu / fse3w(ji+1,jj,iku)
                  ! interpolated values of tracers
                  zti(ji,jj,jn) = pta(ji+1,jj,iku,jn) + zmaxu * ( pta(ji+1,jj,ikum1,jn) - pta(ji+1,jj,iku,jn) )
                  ! gradient of  tracers
                  pgtu(ji,jj,jn) = umask(ji,jj,1) * ( zti(ji,jj,jn) - pta(ji,jj,iku,jn) )
               ELSE                           ! case 2
                  zmaxu = -ze3wu / fse3w(ji,jj,iku)
                  ! interpolated values of tracers
                  zti(ji,jj,jn) = pta(ji,jj,iku,jn) + zmaxu * ( pta(ji,jj,ikum1,jn) - pta(ji,jj,iku,jn) )
                  ! gradient of tracers
                  pgtu(ji,jj,jn) = umask(ji,jj,1) * ( pta(ji+1,jj,iku,jn) - zti(ji,jj,jn) )
               ENDIF
               !
               ! j- direction
               IF( ze3wv >= 0._wp ) THEN      ! case 1
                  zmaxv =  ze3wv / fse3w(ji,jj+1,ikv)
                  ! interpolated values of tracers
                  ztj(ji,jj,jn) = pta(ji,jj+1,ikv,jn) + zmaxv * ( pta(ji,jj+1,ikvm1,jn) - pta(ji,jj+1,ikv,jn) )
                  ! gradient of tracers
                  pgtv(ji,jj,jn) = vmask(ji,jj,1) * ( ztj(ji,jj,jn) - pta(ji,jj,ikv,jn) )
               ELSE                           ! case 2
                  zmaxv =  -ze3wv / fse3w(ji,jj,ikv)
                  ! interpolated values of tracers
                  ztj(ji,jj,jn) = pta(ji,jj,ikv,jn) + zmaxv * ( pta(ji,jj,ikvm1,jn) - pta(ji,jj,ikv,jn) )
                  ! gradient of tracers
                  pgtv(ji,jj,jn) = vmask(ji,jj,1) * ( pta(ji,jj+1,ikv,jn) - ztj(ji,jj,jn) )
               ENDIF
# if ! defined key_vectopt_loop
            END DO
# endif
         END DO
         CALL lbc_lnk( pgtu(:,:,jn), 'U', -1. )   ;   CALL lbc_lnk( pgtv(:,:,jn), 'V', -1. )   ! Lateral boundary cond.
         !
      END DO

      ! horizontal derivative of density anomalies (rd)
      IF( PRESENT( prd ) ) THEN         ! depth of the partial step level
# if defined key_vectopt_loop
         jj = 1
         DO ji = 1, jpij-jpi   ! vector opt. (forced unrolled)
# else
         DO jj = 1, jpjm1
            DO ji = 1, jpim1
# endif
               iku = mbku(ji,jj)
               ikv = mbkv(ji,jj)
               ze3wu  = fse3w(ji+1,jj  ,iku) - fse3w(ji,jj,iku)
               ze3wv  = fse3w(ji  ,jj+1,ikv) - fse3w(ji,jj,ikv)
               IF( ze3wu >= 0._wp ) THEN   ;   zhi(ji,jj) = fsdept(ji  ,jj,iku)     ! i-direction: case 1
               ELSE                        ;   zhi(ji,jj) = fsdept(ji+1,jj,iku)     ! -     -      case 2
               ENDIF
               IF( ze3wv >= 0._wp ) THEN   ;   zhj(ji,jj) = fsdept(ji,jj  ,ikv)     ! j-direction: case 1
               ELSE                        ;   zhj(ji,jj) = fsdept(ji,jj+1,ikv)     ! -     -      case 2
               ENDIF
# if ! defined key_vectopt_loop
            END DO
# endif
         END DO

         ! Compute interpolated rd from zti, ztj for the 2 cases at the depth of the partial
         ! step and store it in  zri, zrj for each  case
         CALL eos( zti, zhi, zri )  
         CALL eos( ztj, zhj, zrj )

         ! Gradient of density at the last level 
# if defined key_vectopt_loop
         jj = 1
         DO ji = 1, jpij-jpi   ! vector opt. (forced unrolled)
# else
         DO jj = 1, jpjm1
            DO ji = 1, jpim1
# endif
               iku = mbku(ji,jj)
               ikv = mbkv(ji,jj)
               ze3wu  = fse3w(ji+1,jj  ,iku) - fse3w(ji,jj,iku)
               ze3wv  = fse3w(ji  ,jj+1,ikv) - fse3w(ji,jj,ikv)
               IF( ze3wu >= 0._wp ) THEN   ;   pgru(ji,jj) = umask(ji,jj,1) * ( zri(ji  ,jj) - prd(ji,jj,iku) )   ! i: 1
               ELSE                        ;   pgru(ji,jj) = umask(ji,jj,1) * ( prd(ji+1,jj,iku) - zri(ji,jj) )   ! i: 2
               ENDIF
               IF( ze3wv >= 0._wp ) THEN   ;   pgrv(ji,jj) = vmask(ji,jj,1) * ( zrj(ji,jj  ) - prd(ji,jj,ikv) )   ! j: 1
               ELSE                        ;   pgrv(ji,jj) = vmask(ji,jj,1) * ( prd(ji,jj+1,ikv) - zrj(ji,jj) )   ! j: 2
               ENDIF
# if ! defined key_vectopt_loop
            END DO
# endif
         END DO
         CALL lbc_lnk( pgru , 'U', -1. )   ;   CALL lbc_lnk( pgrv , 'V', -1. )   ! Lateral boundary conditions
         !
      END IF
      !
      CALL wrk_dealloc( jpi, jpj,       zri, zrj, zhi, zhj ) 
      CALL wrk_dealloc( jpi, jpj, kjpt, zti, ztj           ) 
      !
      IF( nn_timing == 1 )  CALL timing_stop( 'zps_hde')
      !
   END SUBROUTINE zps_hde

   !!======================================================================
END MODULE zpshde
