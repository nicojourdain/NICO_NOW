MODULE limvar
   !!======================================================================
   !!                       ***  MODULE limvar ***
   !!                 Different sets of ice model variables 
   !!                   how to switch from one to another
   !!
   !!                 There are three sets of variables
   !!                 VGLO : global variables of the model
   !!                        - v_i (jpi,jpj,jpl)
   !!                        - v_s (jpi,jpj,jpl)
   !!                        - a_i (jpi,jpj,jpl)
   !!                        - t_s (jpi,jpj,jpl)
   !!                        - e_i (jpi,jpj,nlay_i,jpl)
   !!                        - smv_i(jpi,jpj,jpl)
   !!                        - oa_i (jpi,jpj,jpl)
   !!                 VEQV : equivalent variables sometimes used in the model
   !!                        - ht_i(jpi,jpj,jpl)
   !!                        - ht_s(jpi,jpj,jpl)
   !!                        - t_i (jpi,jpj,nlay_i,jpl)
   !!                        ...
   !!                 VAGG : aggregate variables, averaged/summed over all
   !!                        thickness categories
   !!                        - vt_i(jpi,jpj)
   !!                        - vt_s(jpi,jpj)
   !!                        - at_i(jpi,jpj)
   !!                        - et_s(jpi,jpj)  !total snow heat content
   !!                        - et_i(jpi,jpj)  !total ice thermal content 
   !!                        - smt_i(jpi,jpj) !mean ice salinity
   !!                        - ot_i(jpi,jpj)  !average ice age
   !!======================================================================
   !! History :   -   ! 2006-01 (M. Vancoppenolle) Original code
   !!            4.0  ! 2011-02 (G. Madec) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_var_agg       : 
   !!   lim_var_glo2eqv   :
   !!   lim_var_eqv2glo   :
   !!   lim_var_salprof   : 
   !!   lim_var_salprof1d :
   !!   lim_var_bv        :
   !!----------------------------------------------------------------------
   USE par_oce          ! ocean parameters
   USE phycst           ! physical constants (ocean directory) 
   USE sbc_oce          ! Surface boundary condition: ocean fields
   USE ice              ! LIM variables
   USE par_ice          ! LIM parameters
   USE dom_ice          ! LIM domain
   USE thd_ice          ! LIM thermodynamics
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_var_agg          !
   PUBLIC   lim_var_glo2eqv      !
   PUBLIC   lim_var_eqv2glo      !
   PUBLIC   lim_var_salprof      !
   PUBLIC   lim_var_bv           !
   PUBLIC   lim_var_salprof1d    !

   REAL(wp) ::   eps20 = 1.e-20_wp   ! module constants
   REAL(wp) ::   eps16 = 1.e-16_wp   !    -       -
   REAL(wp) ::   eps13 = 1.e-13_wp   !    -       -
   REAL(wp) ::   eps10 = 1.e-10_wp   !    -       -
   REAL(wp) ::   eps06 = 1.e-06_wp   !    -       -
   REAL(wp) ::   zzero = 0.e0        !    -       -
   REAL(wp) ::   zone  = 1.e0        !    -       -

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limvar.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_var_agg( kn )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_var_agg  ***
      !!
      !! ** Purpose :   aggregates ice-thickness-category variables to all-ice variables
      !!              i.e. it turns VGLO into VAGG
      !! ** Method  :
      !!
      !! ** Arguments : n = 1, at_i vt_i only
      !!                n = 2 everything
      !!
      !! note : you could add an argument when you need only at_i, vt_i
      !!        and when you need everything
      !!------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kn     ! =1 at_i & vt only ; = what is needed
      !
      INTEGER  ::   ji, jj, jk, jl   ! dummy loop indices
      REAL(wp) ::   zinda
      !!------------------------------------------------------------------

      !--------------------
      ! Compute variables
      !--------------------
      vt_i (:,:) = 0._wp
      vt_s (:,:) = 0._wp
      at_i (:,:) = 0._wp
      ato_i(:,:) = 1._wp
      !
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               !
               vt_i(ji,jj) = vt_i(ji,jj) + v_i(ji,jj,jl) ! ice volume
               vt_s(ji,jj) = vt_s(ji,jj) + v_s(ji,jj,jl) ! snow volume
               at_i(ji,jj) = at_i(ji,jj) + a_i(ji,jj,jl) ! ice concentration
               !
               zinda = MAX( zzero , SIGN( zone , at_i(ji,jj) - 0.10 ) ) 
               icethi(ji,jj) = vt_i(ji,jj) / MAX( at_i(ji,jj) , eps16 ) * zinda  ! ice thickness
            END DO
         END DO
      END DO

      DO jj = 1, jpj
         DO ji = 1, jpi
            ato_i(ji,jj) = MAX( 1._wp - at_i(ji,jj), 0._wp )   ! open water fraction
         END DO
      END DO

      IF( kn > 1 ) THEN
         et_s (:,:) = 0._wp
         ot_i (:,:) = 0._wp
         smt_i(:,:) = 0._wp
         et_i (:,:) = 0._wp
         !
         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, jpi
                  et_s(ji,jj)  = et_s(ji,jj)  + e_s(ji,jj,1,jl)                                       ! snow heat content
                  zinda = MAX( zzero , SIGN( zone , vt_i(ji,jj) - 0.10 ) ) 
                  smt_i(ji,jj) = smt_i(ji,jj) + smv_i(ji,jj,jl) / MAX( vt_i(ji,jj) , eps13 ) * zinda   ! ice salinity
                  zinda = MAX( zzero , SIGN( zone , at_i(ji,jj) - 0.10 ) ) 
                  ot_i(ji,jj)  = ot_i(ji,jj)  + oa_i(ji,jj,jl)  / MAX( at_i(ji,jj) , eps13 ) * zinda   ! ice age
               END DO
            END DO
         END DO
         !
         DO jl = 1, jpl
            DO jk = 1, nlay_i
               et_i(:,:) = et_i(:,:) + e_i(:,:,jk,jl)       ! ice heat content
            END DO
         END DO
         !
      ENDIF
      !
   END SUBROUTINE lim_var_agg


   SUBROUTINE lim_var_glo2eqv
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_var_glo2eqv ***
      !!
      !! ** Purpose :   computes equivalent variables as function of global variables 
      !!              i.e. it turns VGLO into VEQV
      !!------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk, jl   ! dummy loop indices
      REAL(wp) ::   zq_i, zaaa, zbbb, zccc, zdiscrim     ! local scalars
      REAL(wp) ::   ztmelts, zindb, zq_s, zfac1, zfac2   !   -      -
      !!------------------------------------------------------------------

      !-------------------------------------------------------
      ! Ice thickness, snow thickness, ice salinity, ice age
      !-------------------------------------------------------
      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               zindb = 1._wp - MAX( 0._wp , SIGN( 1._wp,- a_i(ji,jj,jl) ) )   !0 if no ice and 1 if yes
               ht_i(ji,jj,jl) = v_i (ji,jj,jl) / MAX( a_i(ji,jj,jl) , eps10 ) * zindb
               ht_s(ji,jj,jl) = v_s (ji,jj,jl) / MAX( a_i(ji,jj,jl) , eps10 ) * zindb
               o_i(ji,jj,jl)  = oa_i(ji,jj,jl) / MAX( a_i(ji,jj,jl) , eps10 ) * zindb
            END DO
         END DO
      END DO

      IF(  num_sal == 2  .OR.  num_sal == 4  )THEN
         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zindb = 1._wp - MAX( 0._wp , SIGN( 1._wp,- a_i(ji,jj,jl) ) )   !0 if no ice and 1 if yes
                  sm_i(ji,jj,jl) = smv_i(ji,jj,jl) / MAX( v_i(ji,jj,jl) , eps10 ) * zindb
               END DO
            END DO
         END DO
      ENDIF

      CALL lim_var_salprof      ! salinity profile

      !-------------------
      ! Ice temperatures
      !-------------------
!CDIR NOVERRCHK
      DO jl = 1, jpl
!CDIR NOVERRCHK
         DO jk = 1, nlay_i
!CDIR NOVERRCHK
            DO jj = 1, jpj
!CDIR NOVERRCHK
               DO ji = 1, jpi
                  !                                                              ! Energy of melting q(S,T) [J.m-3]
                  zq_i    = e_i(ji,jj,jk,jl) / area(ji,jj) / MAX( v_i(ji,jj,jl) , eps06 ) * REAL(nlay_i,wp) 
                  zindb   = 1.0 - MAX( 0.0 , SIGN( 1.0 , - v_i(ji,jj,jl) ) )     ! zindb = 0 if no ice and 1 if yes
                  zq_i    = zq_i * unit_fac * zindb                              !convert units
                  ztmelts = -tmut * s_i(ji,jj,jk,jl) + rtt                       ! Ice layer melt temperature
                  !
                  zaaa       =  cpic                  ! Conversion q(S,T) -> T (second order equation)
                  zbbb       =  ( rcp - cpic ) * ( ztmelts - rtt ) + zq_i / rhoic - lfus
                  zccc       =  lfus * (ztmelts-rtt)
                  zdiscrim   =  SQRT( MAX(zbbb*zbbb - 4._wp*zaaa*zccc , 0._wp) )
                  t_i(ji,jj,jk,jl) = rtt + zindb *( - zbbb - zdiscrim ) / ( 2.0 *zaaa )
                  t_i(ji,jj,jk,jl) = MIN( rtt, MAX( 173.15_wp, t_i(ji,jj,jk,jl) ) )       ! 100-rtt < t_i < rtt
               END DO
            END DO
         END DO
      END DO

      !--------------------
      ! Snow temperatures
      !--------------------
      zfac1 = 1._wp / ( rhosn * cpic )
      zfac2 = lfus / cpic  
      DO jl = 1, jpl
         DO jk = 1, nlay_s
            DO jj = 1, jpj
               DO ji = 1, jpi
                  !Energy of melting q(S,T) [J.m-3]
                  zq_s  = e_s(ji,jj,jk,jl) / ( area(ji,jj) * MAX( v_s(ji,jj,jl) , eps06 ) ) * REAL(nlay_s,wp)
                  zindb = 1._wp - MAX( 0._wp , SIGN( 1._wp , - v_s(ji,jj,jl) ) )     ! zindb = 0 if no ice and 1 if yes
                  zq_s  = zq_s * unit_fac * zindb                                    ! convert units
                  !
                  t_s(ji,jj,jk,jl) = rtt + zindb * ( - zfac1 * zq_s + zfac2 )
                  t_s(ji,jj,jk,jl) = MIN( rtt, MAX( 173.15, t_s(ji,jj,jk,jl) ) )     ! 100-rtt < t_i < rtt
               END DO
            END DO
         END DO
      END DO

      !-------------------
      ! Mean temperature
      !-------------------
      tm_i(:,:) = 0._wp
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zindb = (  1._wp - MAX( 0._wp , SIGN( 1._wp , -a_i(ji,jj,jl) ) )  )   &
                     &  * (  1._wp - MAX( 0._wp , SIGN( 1._wp , -v_i(ji,jj,jl) ) )  )
                  tm_i(ji,jj) = tm_i(ji,jj) + t_i(ji,jj,jk,jl) * v_i(ji,jj,jl)   &
                     &                      / (  REAL(nlay_i,wp) * MAX( vt_i(ji,jj) , eps10 )  )
               END DO
            END DO
         END DO
      END DO
      !
   END SUBROUTINE lim_var_glo2eqv


   SUBROUTINE lim_var_eqv2glo
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_var_eqv2glo ***
      !!
      !! ** Purpose :   computes global variables as function of equivalent variables
      !!                i.e. it turns VEQV into VGLO
      !! ** Method  :
      !!
      !! ** History :  (01-2006) Martin Vancoppenolle, UCL-ASTR
      !!------------------------------------------------------------------
      !
      v_i(:,:,:)   = ht_i(:,:,:) * a_i(:,:,:)
      v_s(:,:,:)   = ht_s(:,:,:) * a_i(:,:,:)
      smv_i(:,:,:) = sm_i(:,:,:) * v_i(:,:,:)
      oa_i (:,:,:) = o_i (:,:,:) * a_i(:,:,:)
      !
   END SUBROUTINE lim_var_eqv2glo


   SUBROUTINE lim_var_salprof
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_var_salprof ***
      !!
      !! ** Purpose :   computes salinity profile in function of bulk salinity     
      !!
      !! ** Method  : If bulk salinity greater than s_i_1, 
      !!              the profile is assumed to be constant (S_inf)
      !!              If bulk salinity lower than s_i_0,
      !!              the profile is linear with 0 at the surface (S_zero)
      !!              If it is between s_i_0 and s_i_1, it is a
      !!              alpha-weighted linear combination of s_inf and s_zero
      !!
      !! ** References : Vancoppenolle et al., 2007 (in preparation)
      !!------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk, jl   ! dummy loop index
      REAL(wp) ::   dummy_fac0, dummy_fac1, dummy_fac, zsal      ! local scalar
      REAL(wp) ::   zind0, zind01, zindbal, zargtemp , zs_zero   !   -      -
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   z_slope_s, zalpha   ! 3D pointer
      !!------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, jpl, z_slope_s, zalpha )

      !---------------------------------------
      ! Vertically constant, constant in time
      !---------------------------------------
      IF( num_sal == 1 )   s_i(:,:,:,:) = bulk_sal

      !-----------------------------------
      ! Salinity profile, varying in time
      !-----------------------------------

      IF(   num_sal == 2  .OR.   num_sal == 4   ) THEN
         !
         DO jk = 1, nlay_i
            s_i(:,:,jk,:)  = sm_i(:,:,:)
         END DO
         !
         DO jl = 1, jpl                               ! Slope of the linear profile 
            DO jj = 1, jpj
               DO ji = 1, jpi
                  z_slope_s(ji,jj,jl) = 2._wp * sm_i(ji,jj,jl) / MAX( 0.01 , ht_i(ji,jj,jl) )
               END DO
            END DO
         END DO
         !
         dummy_fac0 = 1._wp / ( s_i_0 - s_i_1 )       ! Weighting factor between zs_zero and zs_inf
         dummy_fac1 = s_i_1 / ( s_i_1 - s_i_0 )

         zalpha(:,:,:) = 0._wp
         DO jl = 1, jpl
            DO jj = 1, jpj
               DO ji = 1, jpi
                  ! zind0 = 1 if sm_i le s_i_0 and 0 otherwise
                  zind0  = MAX( 0.0   , SIGN( 1.0  , s_i_0 - sm_i(ji,jj,jl) ) ) 
                  ! zind01 = 1 if sm_i is between s_i_0 and s_i_1 and 0 othws 
                  zind01 = ( 1.0 - zind0 ) * MAX( 0.0   , SIGN( 1.0  , s_i_1 - sm_i(ji,jj,jl) ) ) 
                  ! If 2.sm_i GE sss_m then zindbal = 1
                  zindbal = MAX( 0.0 , SIGN( 1.0 , 2. * sm_i(ji,jj,jl) - sss_m(ji,jj) ) )
                  zalpha(ji,jj,jl) = zind0  * 1.0 + zind01 * ( sm_i(ji,jj,jl) * dummy_fac0 + dummy_fac1 )
                  zalpha(ji,jj,jl) = zalpha(ji,jj,jl) * ( 1.0 - zindbal )
               END DO
            END DO
         END DO

         dummy_fac = 1._wp / nlay_i                   ! Computation of the profile
         DO jl = 1, jpl
            DO jk = 1, nlay_i
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     !                                      ! linear profile with 0 at the surface
                     zs_zero = z_slope_s(ji,jj,jl) * ( REAL(jk,wp) - 0.5_wp ) * ht_i(ji,jj,jl) * dummy_fac
                     !                                      ! weighting the profile
                     s_i(ji,jj,jk,jl) = zalpha(ji,jj,jl) * zs_zero + ( 1._wp - zalpha(ji,jj,jl) ) * sm_i(ji,jj,jl)
                  END DO ! ji
               END DO ! jj
            END DO ! jk
         END DO ! jl

      ENDIF ! num_sal

      !-------------------------------------------------------
      ! Vertically varying salinity profile, constant in time
      !-------------------------------------------------------

      IF( num_sal == 3 ) THEN      ! Schwarzacher (1959) multiyear salinity profile (mean = 2.30)
         !
         sm_i(:,:,:) = 2.30_wp
         !
         DO jl = 1, jpl
!CDIR NOVERRCHK
            DO jk = 1, nlay_i
               zargtemp  = ( REAL(jk,wp) - 0.5_wp ) / REAL(nlay_i,wp)
               zsal =  1.6_wp * (  1._wp - COS( rpi * zargtemp**(0.407_wp/(0.573_wp+zargtemp)) )  )
               s_i(:,:,jk,jl) =  zsal
            END DO
         END DO

      ENDIF ! num_sal
      !
      CALL wrk_dealloc( jpi, jpj, jpl, z_slope_s, zalpha )
      !
   END SUBROUTINE lim_var_salprof


   SUBROUTINE lim_var_bv
      !!------------------------------------------------------------------
      !!                ***  ROUTINE lim_var_bv ***
      !!
      !! ** Purpose :   computes mean brine volume (%) in sea ice
      !!
      !! ** Method  : e = - 0.054 * S (ppt) / T (C)
      !!
      !! References : Vancoppenolle et al., JGR, 2007
      !!------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk, jl   ! dummy loop indices
      REAL(wp) ::   zbvi, zindb      ! local scalars
      !!------------------------------------------------------------------
      !
      bv_i(:,:) = 0._wp
      DO jl = 1, jpl
         DO jk = 1, nlay_i
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zindb = 1.0-MAX(0.0,SIGN(1.0,-a_i(ji,jj,jl))) !0 if no ice and 1 if yes
                  zbvi  = - zindb * tmut * s_i(ji,jj,jk,jl) / MIN( t_i(ji,jj,jk,jl) - 273.15 , eps13 )   &
                     &                   * v_i(ji,jj,jl)    / REAL(nlay_i,wp)
                  bv_i(ji,jj) = bv_i(ji,jj) + zbvi  / MAX( vt_i(ji,jj) , eps13 )
               END DO
            END DO
         END DO
      END DO
      !
   END SUBROUTINE lim_var_bv


   SUBROUTINE lim_var_salprof1d( kideb, kiut )
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE lim_thd_salprof1d  ***
      !!
      !! ** Purpose :   1d computation of the sea ice salinity profile
      !!                Works with 1d vectors and is used by thermodynamic modules
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kideb, kiut   ! thickness category index
      !
      INTEGER  ::   ji, jk    ! dummy loop indices
      INTEGER  ::   zji, zjj  ! local integers
      REAL(wp) ::   dummy_fac0, dummy_fac1, dummy_fac2, zargtemp, zsal   ! local scalars
      REAL(wp) ::   zalpha, zind0, zind01, zindbal, zs_zero              !   -      -
      !
      REAL(wp), POINTER, DIMENSION(:) ::   z_slope_s
      !!---------------------------------------------------------------------

      CALL wrk_alloc( jpij, z_slope_s )

      !---------------------------------------
      ! Vertically constant, constant in time
      !---------------------------------------
      IF( num_sal == 1 )   s_i_b(:,:) = bulk_sal

      !------------------------------------------------------
      ! Vertically varying salinity profile, varying in time
      !------------------------------------------------------

      IF(  num_sal == 2  .OR.  num_sal == 4  ) THEN
         !
         DO ji = kideb, kiut          ! Slope of the linear profile zs_zero
            z_slope_s(ji) = 2._wp * sm_i_b(ji) / MAX( 0.01 , ht_i_b(ji) )
         END DO

         ! Weighting factor between zs_zero and zs_inf
         !---------------------------------------------
         dummy_fac0 = 1._wp / ( s_i_0 - s_i_1 )
         dummy_fac1 = s_i_1 / ( s_i_1 - s_i_0 )
         dummy_fac2 = 1._wp / REAL(nlay_i,wp)

!CDIR NOVERRCHK
         DO jk = 1, nlay_i
!CDIR NOVERRCHK
            DO ji = kideb, kiut
               zji =  MOD( npb(ji) - 1 , jpi ) + 1
               zjj =     ( npb(ji) - 1 ) / jpi + 1
               ! zind0 = 1 if sm_i le s_i_0 and 0 otherwise
               zind0  = MAX( 0._wp , SIGN( 1._wp  , s_i_0 - sm_i_b(ji) ) ) 
               ! zind01 = 1 if sm_i is between s_i_0 and s_i_1 and 0 othws 
               zind01 = ( 1._wp - zind0 ) * MAX( 0._wp , SIGN( 1._wp , s_i_1 - sm_i_b(ji) ) ) 
               ! if 2.sm_i GE sss_m then zindbal = 1
               zindbal = MAX( 0._wp , SIGN( 1._wp , 2._wp * sm_i_b(ji) - sss_m(zji,zjj) ) )
               !
               zalpha = (  zind0 + zind01 * ( sm_i_b(ji) * dummy_fac0 + dummy_fac1 )  ) * ( 1.0 - zindbal )
               !
               zs_zero = z_slope_s(ji) * ( REAL(jk,wp) - 0.5_wp ) * ht_i_b(ji) * dummy_fac2
               ! weighting the profile
               s_i_b(ji,jk) = zalpha * zs_zero + ( 1._wp - zalpha ) * sm_i_b(ji)
            END DO ! ji
         END DO ! jk

      ENDIF ! num_sal

      !-------------------------------------------------------
      ! Vertically varying salinity profile, constant in time
      !-------------------------------------------------------

      IF( num_sal == 3 ) THEN      ! Schwarzacher (1959) multiyear salinity profile (mean = 2.30)
         !
         sm_i_b(:) = 2.30_wp
         !
!CDIR NOVERRCHK
         DO jk = 1, nlay_i
            zargtemp  = ( REAL(jk,wp) - 0.5_wp ) / REAL(nlay_i,wp)
            zsal =  1.6_wp * (  1._wp - COS( rpi * zargtemp**(0.407_wp/(0.573_wp+zargtemp)) )  )
            DO ji = kideb, kiut
               s_i_b(ji,jk) = zsal
            END DO
         END DO
         !
      ENDIF
      !
      CALL wrk_dealloc( jpij, z_slope_s )
      !
   END SUBROUTINE lim_var_salprof1d

#else
   !!----------------------------------------------------------------------
   !!   Default option         Dummy module          NO  LIM3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_var_agg          ! Empty routines
   END SUBROUTINE lim_var_agg
   SUBROUTINE lim_var_glo2eqv      ! Empty routines
   END SUBROUTINE lim_var_glo2eqv
   SUBROUTINE lim_var_eqv2glo      ! Empty routines
   END SUBROUTINE lim_var_eqv2glo
   SUBROUTINE lim_var_salprof      ! Empty routines
   END SUBROUTINE lim_var_salprof
   SUBROUTINE lim_var_bv           ! Emtpy routines
   END SUBROUTINE lim_var_bv
   SUBROUTINE lim_var_salprof1d    ! Emtpy routines
   END SUBROUTINE lim_var_salprof1d
#endif

   !!======================================================================
END MODULE limvar
