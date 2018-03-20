MODULE limdia
   !!======================================================================
   !!                       ***  MODULE limdia   ***
   !!  LIM-3 sea ice model :   diagnostics of ice model 
   !!======================================================================
   !! History :  3.2  ! 2007-01  (M. Vancoppenolle)  Code adapted from LIM-2
   !!             -   ! 2008-03  (M. Vancoppenolle)  add lim_dia_init
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                       LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_dia        : computation and output of the time evolution of keys variables
   !!   lim_dia_init   : initialization and namelist read
   !!----------------------------------------------------------------------
   USE ice             ! LIM-3: sea-ice variable
   USE par_ice         ! LIM-3: ice parameters
   USE dom_ice         ! LIM-3: sea-ice domain
   USE dom_oce         ! ocean domain
   USE sbc_oce         ! surface boundary condition: ocean fields
   USE daymod          ! model calendar
   USE phycst          ! physical constant
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! MPP library
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC lim_dia       ! called by ice_step

   INTEGER, PUBLIC  ::   ntmoy   = 1   !: instantaneous values of ice evolution or averaging ntmoy
   INTEGER, PUBLIC  ::   ninfo   = 1   !: frequency of ouputs on file ice_evolu in case of averaging

   !                                              !!! Parameters for outputs to files "evolu"
   INTEGER, PARAMETER ::   jpinfmx = 100           ! maximum number of key variables
   INTEGER, PARAMETER ::   jpchinf = 5             ! ???
   INTEGER, PARAMETER ::   jpchsep = jpchinf + 2   ! ???

   INTEGER  ::   nfrinf  = 4         ! number of variables written in one line 
   INTEGER  ::   nferme              ! last time step at which the var. are written on file
   INTEGER  ::   nvinfo              ! number of total variables 
   INTEGER  ::   nbvt                ! number of time variables
   INTEGER  ::   naveg               ! number of step for accumulation before averaging
   REAL(wp) ::   epsi06 = 1.e-6_wp   ! small number

   CHARACTER(len= 8) ::   fmtinf = '1PE13.5 '   ! format of the output values  
   CHARACTER(len=30) ::   fmtw                  ! formats
   CHARACTER(len=30) ::   fmtr                  ! ???
   CHARACTER(len=30) ::   fmtitr                ! ???

   CHARACTER(len=jpchsep), DIMENSION(jpinfmx) ::   titvar   ! title of key variables

   REAL(wp), DIMENSION(jpinfmx) ::   vinfom     ! temporary working space
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   aire       ! masked grid cell area

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/LIM3 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: limdia.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_dia
      !!--------------------------------------------------------------------
      !!                  ***  ROUTINE lim_dia  ***
      !!   
      !! ** Purpose :   Computation and outputs on file ice.evolu 
      !!              the temporal evolution of some key variables
      !!-------------------------------------------------------------------
      INTEGER  ::   jv, ji, jj, jl   ! dummy loop indices
      REAL(wp) ::   zshift_date      ! date from the minimum ice extent
      REAL(wp) ::   zday, zday_min   ! current day, day of minimum extent
      REAL(wp) ::   zafy, zamy       ! temporary area of fy and my ice
      REAL(wp) ::   zindb
      REAL(wp), DIMENSION(jpinfmx) ::   vinfor           ! temporary working space 
      !!-------------------------------------------------------------------

      ! 0) date from the minimum of ice extent
      !---------------------------------------
      zday_min = 273._wp        ! zday_min = date of minimum extent, here September 30th
      zday = REAL(numit-nit000,wp) * rdt_ice / ( 86400._wp * REAL(nn_fsbc,wp) )
      !
      IF( zday > zday_min ) THEN   ;   zshift_date  =  zday - zday_min
      ELSE                         ;   zshift_date  =  zday - (365.0 - zday_min)
      ENDIF

      IF( numit == nstart )   CALL lim_dia_init   ! initialisation of ice_evolu file      

      vinfor(1) = REAL(numit)       ! time diagnostics 
      vinfor(2) = nyear

      DO jv = nbvt + 1, nvinfo      ! put everything to zero
         vinfor(jv) = 0._wp
      END DO

      !!-------------------------------------------------------------------
      !! 1) Northern hemisphere
      !!-------------------------------------------------------------------
      !! 1.1) Diagnostics independent on age
      !!------------------------------------
      DO jj = njeq, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            IF( tms(ji,jj) == 1 ) THEN
               vinfor(3)  = vinfor(3)  + at_i(ji,jj)*aire(ji,jj) * 1.e-12_wp !ice area
               IF (at_i(ji,jj).GT.0.15) vinfor(5) = vinfor(5) + aire(ji,jj) * 1.e-12_wp !ice extent
               vinfor(7)  = vinfor(7)  + vt_i(ji,jj)*aire(ji,jj) * 1.e-12_wp !ice volume
               vinfor(9)  = vinfor(9)  + vt_s(ji,jj)*aire(ji,jj) * 1.e-12_wp !snow volume
               vinfor(15) = vinfor(15) + ot_i(ji,jj) *vt_i(ji,jj)*aire(ji,jj) * 1.e-12_wp !mean age
               vinfor(29) = vinfor(29) + smt_i(ji,jj)*vt_i(ji,jj)*aire(ji,jj) * 1.e-12_wp !mean salinity
               ! the computation of this diagnostic is not reliable
               vinfor(31) = vinfor(31) + vt_i(ji,jj)*( u_ice(ji,jj)*u_ice(ji,jj) + & 
                  v_ice(ji,jj)*v_ice(ji,jj) )*aire(ji,jj)/1.0e12 
               vinfor(53) = vinfor(53) + emps(ji,jj)*aire(ji,jj) * 1.e-12_wp !salt flux
               vinfor(55) = vinfor(55) + fsbri(ji,jj)*aire(ji,jj) * 1.e-12_wp !brine drainage flux
               vinfor(57) = vinfor(57) + fseqv(ji,jj)*aire(ji,jj) * 1.e-12_wp !equivalent salt flux
               vinfor(59) = vinfor(59) +(sst_m(ji,jj)+rt0)*at_i(ji,jj)*aire(ji,jj) * 1.e-12_wp  !SST
               vinfor(61) = vinfor(61) + sss_m(ji,jj)*at_i(ji,jj)*aire(ji,jj) * 1.e-12_wp  !SSS
               vinfor(65) = vinfor(65) + et_s(ji,jj)/1.0e9*aire(ji,jj) * 1.e-12_wp  ! snow temperature
               vinfor(67) = vinfor(67) + et_i(ji,jj)/1.0e9*aire(ji,jj) * 1.e-12_wp       ! ice heat content
               vinfor(69) = vinfor(69) + v_i(ji,jj,1)*aire(ji,jj) * 1.e-12_wp !ice volume
               vinfor(71) = vinfor(71) + v_i(ji,jj,2)*aire(ji,jj) * 1.e-12_wp !ice volume
               vinfor(73) = vinfor(73) + v_i(ji,jj,3)*aire(ji,jj) * 1.e-12_wp !ice volume
               vinfor(75) = vinfor(75) + v_i(ji,jj,4)*aire(ji,jj) * 1.e-12_wp !ice volume
               vinfor(77) = vinfor(77) + v_i(ji,jj,5)*aire(ji,jj) * 1.e-12_wp !ice volume
               vinfor(79) = 0.0
               vinfor(81) = vinfor(81) + emp(ji,jj)*aire(ji,jj) * 1.e-12_wp ! mass flux
            ENDIF
         END DO
      END DO

      DO jl = ice_cat_bounds(1,1), ice_cat_bounds(1,2)
         DO jj = njeq, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               IF( tms(ji,jj) == 1 ) THEN
                  vinfor(11) = vinfor(11) + v_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp !undef def ice volume
               ENDIF
            END DO
         END DO
      END DO

      vinfor(13) = 0._wp

      vinfor(15) = vinfor(15) / MAX(vinfor(7),epsi06) ! these have to be divided by total ice volume to have the
      vinfor(29) = vinfor(29) / MAX(vinfor(7),epsi06) ! right value
      vinfor(31) = SQRT( vinfor(31) / MAX( vinfor(7) , epsi06 ) )
      vinfor(67) = vinfor(67) / MAX(vinfor(7),epsi06)

      vinfor(53) = vinfor(53) / MAX(vinfor(5),epsi06) ! these have to be divided by total ice extent to have the
      vinfor(55) = vinfor(55) / MAX(vinfor(5),epsi06) ! right value 
      vinfor(57) = vinfor(57) / MAX(vinfor(5),epsi06) ! 
      vinfor(79) = vinfor(79) / MAX(vinfor(5),epsi06) !

      zindb      = 1.0 - MAX(0.0,SIGN(1.0,-vinfor(3))) !
      vinfor(59) = zindb*vinfor(59) / MAX(vinfor(3),epsi06) ! divide by ice area
      vinfor(61) = zindb*vinfor(61) / MAX(vinfor(3),epsi06) !

      zindb      = 1.0 - MAX(0.0,SIGN(1.0,-vinfor(9))) !
      vinfor(65) = zindb*vinfor(65) / MAX(vinfor(9),epsi06) ! divide it by snow volume


      DO jl = 1, jpl
         DO jj = njeq, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               IF( tms(ji,jj) == 1 ) THEN
                  vinfor(33) = vinfor(33) + d_v_i_trp(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp !ice volume
                  vinfor(35) = vinfor(35) + d_v_i_thd(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp !ice volume
               ENDIF
            END DO
         END DO
      END DO

      DO jj = njeq, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            IF( tms(ji,jj) == 1 ) THEN
               vinfor(37) = vinfor(37) + diag_sni_gr(ji,jj)*aire(ji,jj) * 1.e-12_wp !th growth rates
               vinfor(39) = vinfor(39) + diag_lat_gr(ji,jj)*aire(ji,jj) * 1.e-12_wp 
               vinfor(41) = vinfor(41) + diag_bot_gr(ji,jj)*aire(ji,jj) * 1.e-12_wp
               vinfor(43) = vinfor(43) + diag_dyn_gr(ji,jj)*aire(ji,jj) * 1.e-12_wp 
               vinfor(45) = vinfor(45) + dv_dt_thd(ji,jj,5)*aire(ji,jj) * 1.e-12_wp
               vinfor(47) = vinfor(47) + v_newice(ji,jj) *aire(ji,jj) * 1.e-12_wp / rdt_ice ! volume acc in OW
            ENDIF
         END DO
      END DO

      DO jl = 1, jpl
         DO jj = njeq, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               IF( tms(ji,jj) == 1 ) THEN
                  vinfor(63) = vinfor(63) + t_su(ji,jj,jl)*a_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp
               ENDIF
            END DO
         END DO
      END DO
      vinfor(63) = vinfor(63) / MAX(vinfor(3),epsi06) ! these have to be divided by total ice area

      !! 1.2) Diagnostics dependent on age
      !!------------------------------------
      DO jj = njeq, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            IF( tms(ji,jj) == 1 ) THEN
               zafy = 0.0
               zamy = 0.0
               DO jl = 1, jpl
                  IF ((o_i(ji,jj,jl) - zshift_date).LT.0.0) THEN
                     vinfor(17) = vinfor(17) + a_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp ! FY ice area
                     vinfor(25) = vinfor(25) + v_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp ! FY ice volume
                     vinfor(49) = vinfor(49) + sm_i(ji,jj,jl)*v_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp !FY ice salinity
                     zafy = zafy + a_i(ji,jj,jl)
                  ENDIF
                  IF ((o_i(ji,jj,jl) - zshift_date).GT.0.0) THEN
                     vinfor(19) = vinfor(19) + a_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp    ! MY ice area
                     vinfor(27) = vinfor(27) + v_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp ! MY ice volume
                     vinfor(51) = vinfor(51) + sm_i(ji,jj,jl)*v_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp !MY ice salinity
                     zamy = zamy + a_i(ji,jj,jl)
                  ENDIF
               END DO
               IF ((at_i(ji,jj).GT.0.15).AND.(zafy.GT.zamy)) THEN
                  vinfor(21) = vinfor(21) + aire(ji,jj) * 1.e-12_wp ! Seasonal ice extent
               ENDIF
               IF ((at_i(ji,jj).GT.0.15).AND.(zafy.LE.zamy)) THEN
                  vinfor(23) = vinfor(23) + aire(ji,jj) * 1.e-12_wp ! Perennial ice extent
               ENDIF
            ENDIF
         END DO
      END DO
      zindb      = 1.0 - MAX(0.0,SIGN(1.0,-vinfor(25))) !=0 if no multiyear ice 1 if yes
      vinfor(49) = zindb*vinfor(49) / MAX(vinfor(25),epsi06)
      zindb      = 1.0 - MAX(0.0,SIGN(1.0,-vinfor(27))) !=0 if no multiyear ice 1 if yes
      vinfor(51) = zindb*vinfor(51) / MAX(vinfor(27),epsi06)

      !! Fram Strait Export
      !! 83 = area export
      !! 84 = volume export
      !! Fram strait in ORCA2 = 5 points
      !! export = -v_ice*e1t*ddtb*at_i or -v_ice*e1t*ddtb*at_i*h_i
      jj = 136 ! C grid
      vinfor(83) = 0.0
      vinfor(84) = 0.0
      DO ji = 134, 138
         vinfor(83) = vinfor(83) - v_ice(ji,jj) * & 
            e1t(ji,jj)*at_i(ji,jj)*rdt_ice * 1.e-12_wp
         vinfor(84) = vinfor(84) - v_ice(ji,jj) * & 
            e1t(ji,jj)*vt_i(ji,jj)*rdt_ice * 1.e-12_wp
      END DO

      !!-------------------------------------------------------------------
      !! 2) Southern hemisphere
      !!-------------------------------------------------------------------
      !! 2.1) Diagnostics independent on age
      !!------------------------------------
      DO jj = 2, njeqm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            IF( tms(ji,jj) == 1 ) THEN
               vinfor(4)  = vinfor(4)  + at_i(ji,jj)*aire(ji,jj) * 1.e-12_wp !ice area
               IF (at_i(ji,jj).GT.0.15) vinfor(6) = vinfor(6) + aire(ji,jj) * 1.e-12_wp !ice extent
               vinfor(8)  = vinfor(8)  + vt_i(ji,jj)*aire(ji,jj) * 1.e-12_wp !ice volume
               vinfor(10) = vinfor(10) + vt_s(ji,jj)*aire(ji,jj) * 1.e-12_wp !snow volume
               vinfor(16) = vinfor(16) + ot_i(ji,jj)*vt_i(ji,jj)*aire(ji,jj) * 1.e-12_wp !mean age
               vinfor(30) = vinfor(30) + smt_i(ji,jj)*vt_i(ji,jj)*aire(ji,jj) * 1.e-12_wp !mean salinity
               ! this diagnostic is not well computed (weighted by vol instead
               ! of area)
               vinfor(32) = vinfor(32) + vt_i(ji,jj)*( u_ice(ji,jj)*u_ice(ji,jj) + & 
                  v_ice(ji,jj)*v_ice(ji,jj) )*aire(ji,jj)/1.0e12 !ice vel
               vinfor(54) = vinfor(54) + at_i(ji,jj)*emps(ji,jj)*aire(ji,jj) * 1.e-12_wp ! Total salt flux
               vinfor(56) = vinfor(56) + at_i(ji,jj)*fsbri(ji,jj)*aire(ji,jj) * 1.e-12_wp ! Brine drainage salt flux
               vinfor(58) = vinfor(58) + at_i(ji,jj)*fseqv(ji,jj)*aire(ji,jj) * 1.e-12_wp ! Equivalent salt flux
               vinfor(60) = vinfor(60) +(sst_m(ji,jj)+rt0)*at_i(ji,jj)*aire(ji,jj) * 1.e-12_wp  !SST
               vinfor(62) = vinfor(62) + sss_m(ji,jj)*at_i(ji,jj)*aire(ji,jj) * 1.e-12_wp  !SSS
               vinfor(66) = vinfor(66) + et_s(ji,jj)/1.0e9*aire(ji,jj) * 1.e-12_wp ! snow temperature
               vinfor(68) = vinfor(68) + et_i(ji,jj)/1.0e9*aire(ji,jj) * 1.e-12_wp ! ice enthalpy
               vinfor(70) = vinfor(70) + v_i(ji,jj,1)*aire(ji,jj) * 1.e-12_wp !ice volume
               vinfor(72) = vinfor(72) + v_i(ji,jj,2)*aire(ji,jj) * 1.e-12_wp !ice volume
               vinfor(74) = vinfor(74) + v_i(ji,jj,3)*aire(ji,jj) * 1.e-12_wp !ice volume
               vinfor(76) = vinfor(76) + v_i(ji,jj,4)*aire(ji,jj) * 1.e-12_wp !ice volume
               vinfor(78) = vinfor(78) + v_i(ji,jj,5)*aire(ji,jj) * 1.e-12_wp !ice volume
               vinfor(80) = 0.0
               vinfor(82) = vinfor(82) + emp(ji,jj)*aire(ji,jj) * 1.e-12_wp ! mass flux
            ENDIF
         END DO
      END DO

      DO jl = ice_cat_bounds(1,1), ice_cat_bounds(1,2)
         DO jj = 2, njeqm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               vinfor(12) = vinfor(12) + v_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp !undef def ice volume
            END DO
         END DO
      END DO

      vinfor(14) = 0.0

      zindb      = 1.0 - MAX(0.0,SIGN(1.0,-vinfor(8))) 
      vinfor(16) = zindb * vinfor(16) / MAX(vinfor(8),epsi06) ! these have to be divided by ice vol
      vinfor(30) = zindb * vinfor(30) / MAX(vinfor(8),epsi06) ! 
      vinfor(32) = zindb * SQRT( vinfor(32) / MAX( vinfor(8) , epsi06 ) )
      vinfor(68) = zindb * vinfor(68) / MAX(vinfor(8),epsi06) ! 

      zindb      = 1.0 - MAX(0.0,SIGN(1.0,-vinfor(6))) 
      vinfor(54) = zindb * vinfor(54) / MAX(vinfor(6),epsi06) ! these have to be divided by ice extt
      vinfor(56) = zindb * vinfor(56) / MAX(vinfor(6),epsi06) ! 
      vinfor(58) = zindb * vinfor(58) / MAX(vinfor(6),epsi06) ! 
      vinfor(80) = zindb * vinfor(80) / MAX(vinfor(6),epsi06) !
      !      vinfor(84) = vinfor(84) / vinfor(6) !

      zindb      = 1.0 - MAX(0.0,SIGN(1.0,-vinfor(4))) !
      vinfor(60) = zindb*vinfor(60) / ( MAX(vinfor(4), epsi06) ) ! divide by ice area
      vinfor(62) = zindb*vinfor(62) / ( MAX(vinfor(4), epsi06) ) !

      zindb      = 1.0 - MAX(0.0,SIGN(1.0,-vinfor(10))) !
      vinfor(66) = zindb*vinfor(66) / MAX(vinfor(10),epsi06) ! divide it by snow volume

      DO jl = 1, jpl
         DO jj = 2, njeqm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               IF( tms(ji,jj) == 1 ) THEN
                  vinfor(34) = vinfor(34) + d_v_i_trp(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp !ice volume
                  vinfor(36) = vinfor(36) + d_v_i_thd(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp !ice volume
               ENDIF
            END DO
         END DO
      END DO

      DO jj = 2, njeqm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            IF( tms(ji,jj) == 1 ) THEN
               vinfor(38) = vinfor(38) + diag_sni_gr(ji,jj)*aire(ji,jj) * 1.e-12_wp !th growth rates
               vinfor(40) = vinfor(40) + diag_lat_gr(ji,jj)*aire(ji,jj) * 1.e-12_wp 
               vinfor(42) = vinfor(42) + diag_bot_gr(ji,jj)*aire(ji,jj) * 1.e-12_wp
               vinfor(44) = vinfor(44) + diag_dyn_gr(ji,jj)*aire(ji,jj) * 1.e-12_wp 
               vinfor(46) = vinfor(46) + dv_dt_thd(ji,jj,5)*aire(ji,jj) * 1.e-12_wp
               vinfor(48) = vinfor(48) + v_newice(ji,jj) *aire(ji,jj) * 1.e-12_wp / rdt_ice ! volume acc in OW
            ENDIF
         END DO
      END DO

      DO jl = 1, jpl
         DO jj = 2, njeqm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               IF( tms(ji,jj) == 1 ) THEN
                  vinfor(64) = vinfor(64) + t_su(ji,jj,jl)*a_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp
               ENDIF
            END DO
         END DO
      END DO
      zindb      = 1.0 - MAX(0.0,SIGN(1.0,-vinfor(4))) !
      vinfor(64) = zindb * vinfor(64) / MAX(vinfor(4),epsi06) ! divide by ice extt
      !! 2.2) Diagnostics dependent on age
      !!------------------------------------
      DO jj = 2, njeqm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            IF( tms(ji,jj) == 1 ) THEN
               zafy = 0._wp
               zamy = 0._wp
               DO jl = 1, jpl
                  IF( (o_i(ji,jj,jl) - zshift_date) < 0._wp ) THEN
                     vinfor(18) = vinfor(18) + a_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp ! FY ice area
                     vinfor(26) = vinfor(26) + v_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp ! FY ice volume
                     zafy = zafy + a_i(ji,jj,jl)
                     vinfor(50) = vinfor(50) + sm_i(ji,jj,jl)*v_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp !FY ice salinity
                  ENDIF
                  IF( (o_i(ji,jj,jl) - zshift_date) > 0._wp ) THEN
                     vinfor(20) = vinfor(20) + a_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp    ! MY ice area
                     vinfor(28) = vinfor(28) + v_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp
                     vinfor(52) = vinfor(52) + sm_i(ji,jj,jl)*v_i(ji,jj,jl)*aire(ji,jj) * 1.e-12_wp !FY ice salinity
                     zamy = zamy + a_i(ji,jj,jl)
                  ENDIF
               END DO ! jl
               IF ((at_i(ji,jj).GT.0.15).AND.(zafy.GT.zamy)) THEN
                  vinfor(22) = vinfor(22) + aire(ji,jj) * 1.e-12_wp ! Seasonal ice extent
               ENDIF
               IF ((at_i(ji,jj).GT.0.15).AND.(zafy.LE.zamy)) THEN
                  vinfor(24) = vinfor(24) + aire(ji,jj) * 1.e-12_wp ! Perennial ice extent
               ENDIF
            ENDIF ! tms
         END DO ! jj
      END DO ! ji
      zindb      = 1.0 - MAX(0.0,SIGN(1.0,-vinfor(26))) !=0 if no multiyear ice 1 if yes
      vinfor(50) = zindb*vinfor(50) / MAX(vinfor(26),epsi06)
      zindb      = 1.0 - MAX(0.0,SIGN(1.0,-vinfor(28))) !=0 if no multiyear ice 1 if yes
      vinfor(52) = zindb*vinfor(52) / MAX(vinfor(28),epsi06)

      !  Accumulation before averaging 
      DO jv = 1, nvinfo
         vinfom(jv) = vinfom(jv) + vinfor(jv)
      END DO
      naveg = naveg + 1  

      ! oututs on file ice_evolu    
      !MV      IF( MOD( numit , ninfo ) == 0 ) THEN
      WRITE(numevo_ice,fmtw) ( titvar(jv), vinfom(jv)/naveg, jv = 1, nvinfo )
      naveg = 0
      DO jv = 1, nvinfo
         vinfom(jv) = 0._wp
      END DO
      !MV      ENDIF
      !
   END SUBROUTINE lim_dia


   SUBROUTINE lim_dia_init
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE lim_dia_init  ***
      !!             
      !! ** Purpose : Preparation of the file ice_evolu for the output of
      !!      the temporal evolution of key variables
      !!
      !! ** input   : Namelist namicedia
      !!-------------------------------------------------------------------
      INTEGER  ::   jv    ! dummy loop indice
      INTEGER  ::   ierr, ntot , ndeb , irecl   ! local integers
      REAL(wp) ::   zxx0, zxx1    ! local scalars
      CHARACTER(len=jpchinf) ::   titinf
      CHARACTER(len=50)      ::   clname
      !!
      NAMELIST/namicedia/fmtinf, nfrinf, ninfo, ntmoy
      !!-------------------------------------------------------------------
      !
      REWIND( numnam_ice )             ! read namicedia namelist
      READ  ( numnam_ice, namicedia )
      !
      IF(lwp) THEN                     ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'lim_dia_init : ice parameters for ice diagnostics '
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   format of the output values                                 fmtinf = ', fmtinf
         WRITE(numout,*) '   number of variables written in one line                     nfrinf = ', nfrinf 
         WRITE(numout,*) '   Instantaneous values of ice evolution or averaging          ntmoy  = ', ntmoy
         WRITE(numout,*) '   frequency of ouputs on file ice_evolu in case of averaging  ninfo  = ', ninfo
      ENDIF

      ALLOCATE( aire(jpi,jpj) , STAT=ierr )      ! masked grid cell area (interior domain only)
      IF( lk_mpp    )   CALL mpp_sum( ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'lim_dia_init_2 : unable to allocate arrays' )
      aire(:,:) = area(:,:) * tms(:,:) * tmask_i(:,:)

      ! Titles of ice key variables :
      titvar(1) = 'NoIt'  ! iteration number
      titvar(2) = 'T yr'  ! time step in years
      nbvt      = 2       ! number of time variables

      titvar(3) = 'AI_N'  ! sea ice area in the northern Hemisp.(10^12 km2)
      titvar(4) = 'AI_S'  ! sea ice area in the southern Hemisp.(10^12 km2)
      titvar(5) = 'EI_N'  ! sea ice extent (15%) in the northern Hemisp.(10^12 km2)
      titvar(6) = 'EI_S'  ! sea ice extent (15%) in the southern Hemisp.(10^12 km2)
      titvar(7) = 'VI_N'  ! sea ice volume in the northern Hemisp.(10^3 km3)
      titvar(8) = 'VI_S'  ! sea ice volume in the southern Hemisp.(10^3 km3)
      titvar(9) = 'VS_N'  ! snow volume over sea ice in the northern Hemisp.(10^3 km3)
      titvar(10)= 'VS_S'  ! snow volume over sea ice in the northern Hemisp.(10^3 km3)
      titvar(11)= 'VuIN'  ! undeformed sea ice volume in the northern Hemisp.(10^3 km3)
      titvar(12)= 'VuIS'  ! undeformed sea ice volume in the southern Hemisp.(10^3 km3)
      titvar(13)= 'VdIN'  ! deformed sea ice volume in the northern Hemisp.(10^3 km3)
      titvar(14)= 'VdIS'  ! deformed sea ice volume in the southern Hemisp.(10^3 km3)
      titvar(15)= 'OI_N'  ! sea ice mean age in the northern Hemisp.(years)
      titvar(16)= 'OI_S'  ! sea ice mean age in the southern Hemisp.(years)
      titvar(17)= 'AFYN'  ! total FY ice area northern Hemisp.(10^12 km2)
      titvar(18)= 'AFYS'  ! total FY ice area southern Hemisp.(10^12 km2)
      titvar(19)= 'AMYN'  ! total MY ice area northern Hemisp.(10^12 km2)
      titvar(20)= 'AMYS'  ! total MY ice area southern Hemisp.(10^12 km2)
      titvar(21)= 'EFYN'  ! total FY ice extent northern Hemisp.(10^12 km2) (with more 50% FY ice)
      titvar(22)= 'EFYS'  ! total FY ice extent southern Hemisp.(10^12 km2) (with more 50% FY ice)
      titvar(23)= 'EMYN'  ! total MY ice extent northern Hemisp.(10^12 km2) (with more 50% MY ice)
      titvar(24)= 'EMYS'  ! total MY ice extent southern Hemisp.(10^12 km2) (with more 50% MY ice)
      titvar(25)= 'VFYN'  ! total undeformed FY ice volume northern Hemisp.(10^3 km3) 
      titvar(26)= 'VFYS'  ! total undeformed FY ice volume southern Hemisp.(10^3 km3)
      titvar(27)= 'VMYN'  ! total undeformed MY ice volume northern Hemisp.(10^3 km3) 
      titvar(28)= 'VMYS'  ! total undeformed MY ice volume southern Hemisp.(10^3 km3) 
      titvar(29)= 'IS_N'  ! sea ice mean salinity in the northern hemisphere (ppt)  
      titvar(30)= 'IS_S'  ! sea ice mean salinity in the southern hemisphere (ppt)  
      titvar(31)= 'IVeN'  ! sea ice mean velocity in the northern hemisphere (m/s) 
      titvar(32)= 'IVeS'  ! sea ice mean velocity in the southern hemisphere (m/s) 
      titvar(33)= 'DVDN'  ! variation of sea ice volume due to dynamics in the northern hemisphere
      titvar(34)= 'DVDS'  ! variation of sea ice volume due to dynamics in the southern hemisphere
      titvar(35)= 'DVTN'  ! variation of sea ice volume due to thermo in the   northern hemisphere
      titvar(36)= 'DVTS'  ! variation of sea ice volume due to thermo in the   southern hemisphere
      titvar(37)= 'TG1N'  ! thermodynamic vertical growth rate in the northern hemisphere, cat 1  
      titvar(38)= 'TG1S'  ! thermodynamic vertical growth rate in the souhtern hemisphere, cat 1  
      titvar(39)= 'TG2N'  ! thermodynamic vertical growth rate in the northern hemisphere, cat 2  
      titvar(40)= 'TG2S'  ! thermodynamic vertical growth rate in the souhtern hemisphere, cat 2  
      titvar(41)= 'TG3N'  ! thermodynamic vertical growth rate in the northern hemisphere, cat 3  
      titvar(42)= 'TG3S'  ! thermodynamic vertical growth rate in the souhtern hemisphere, cat 3  
      titvar(43)= 'TG4N'  ! thermodynamic vertical growth rate in the northern hemisphere, cat 4  
      titvar(44)= 'TG4S'  ! thermodynamic vertical growth rate in the souhtern hemisphere, cat 4  
      titvar(45)= 'TG5N'  ! thermodynamic vertical growth rate in the northern hemisphere, cat 5  
      titvar(46)= 'TG5S'  ! thermodynamic vertical growth rate in the souhtern hemisphere, cat 5  
      titvar(47)= 'LA_N'  ! lateral accretion growth rate, northern hemisphere
      titvar(48)= 'LA_S'  ! lateral accretion growth rate, southern hemisphere 
      titvar(49)= 'SF_N'  ! Salinity FY, NH 
      titvar(50)= 'SF_S'  ! Salinity FY, SH 
      titvar(51)= 'SF_N'  ! Salinity MY, NH 
      titvar(52)= 'SF_S'  ! Salinity MY, SH 
      titvar(53)= 'Fs_N'  ! Total salt flux NH
      titvar(54)= 'Fs_S'  ! Total salt flux SH
      titvar(55)= 'FsbN'  ! Salt - brine drainage flux NH
      titvar(56)= 'FsbS'  ! Salt - brine drainage flux SH
      titvar(57)= 'FseN'  ! Salt - Equivalent salt flux NH
      titvar(58)= 'FseS'  ! Salt - Equivalent salt flux SH
      titvar(59)= 'SSTN'  ! SST, NH
      titvar(60)= 'SSTS'  ! SST, SH
      titvar(61)= 'SSSN'  ! SSS, NH
      titvar(62)= 'SSSS'  ! SSS, SH
      titvar(63)= 'TsuN'  ! Tsu, NH
      titvar(64)= 'TsuS'  ! Tsu, SH
      titvar(65)= 'TsnN'  ! Tsn, NH
      titvar(66)= 'TsnS'  ! Tsn, SH
      titvar(67)= 'ei_N'  ! ei, NH
      titvar(68)= 'ei_S'  ! ei, SH
      titvar(69)= 'vi1N'  ! vi1, NH
      titvar(70)= 'vi1S'  ! vi1, SH
      titvar(71)= 'vi2N'  ! vi2, NH
      titvar(72)= 'vi2S'  ! vi2, SH
      titvar(73)= 'vi3N'  ! vi3, NH
      titvar(74)= 'vi3S'  ! vi3, SH
      titvar(75)= 'vi4N'  ! vi4, NH
      titvar(76)= 'vi4S'  ! vi4, SH
      titvar(77)= 'vi5N'  ! vi5, NH
      titvar(78)= 'vi5S'  ! vi5, SH
      titvar(79)= 'vi6N'  ! vi6, NH
      titvar(80)= 'vi6S'  ! vi6, SH
      titvar(81)= 'fmaN'  ! mass flux in the ocean, NH
      titvar(82)= 'fmaS'  ! mass flux in the ocean, SH
      titvar(83)= 'AFSE'  ! Fram Strait Area export
      titvar(84)= 'VFSE'  ! Fram Strait Volume export
      nvinfo = 84

      ! Definition et Ecriture de l'entete : nombre d'enregistrements 
      ndeb   = ( nstart - 1 ) / ninfo
      IF( nstart == 1 ) ndeb = -1

      nferme = ( nstart - 1 + nitrun) / ninfo
      ntot   = nferme - ndeb
      ndeb   = ninfo * ( 1 + ndeb )
      nferme = ninfo * nferme

      ! definition of formats 
      WRITE( fmtw  , '(A,I3,A2,I1,A)' )  '(', nfrinf, '(A', jpchsep, ','//fmtinf//'))'
      WRITE( fmtr  , '(A,I3,A,I1,A)'  )  '(', nfrinf, '(', jpchsep, 'X,'//fmtinf//'))'
      WRITE( fmtitr, '(A,I3,A,I1,A)'  )  '(', nvinfo, 'A', jpchinf, ')'

      ! opening  "ice_evolu" file
      IF( lk_mpp ) THEN   ;   WRITE(clname,FMT="('ice.evolu_',I4.4)") narea-1
      ELSE                ;   clname = 'ice.evolu'
      END IF
      irecl = ( jpchinf + 1 ) * nvinfo 
      CALL ctl_opn( numevo_ice, clname, 'UNKNOWN', 'FORMATTED', 'SEQUENTIAL',    &
         &         irecl, numout, lwp, narea )

      !- ecriture de 2 lignes d''entete :
      WRITE(numevo_ice,1000) fmtr, fmtw, fmtitr, nvinfo, ntot, 0, nfrinf
      zxx0 = 0.001 * REAL(ninfo)
      zxx1 = 0.001 * REAL(ndeb)
      WRITE(numevo_ice,1111) REAL(jpchinf), 0., zxx1, zxx0, 0., 0., 0

      !- ecriture de 2 lignes de titre :
      WRITE(numevo_ice,'(A,I8,A,I8,A,I5)')                 &
         'Evolution chronologique - Experience '//cexper   &
         //'   de', ndeb, ' a', nferme, ' pas', ninfo
      WRITE(numevo_ice,fmtitr) ( titvar(jv), jv = 1, nvinfo )

      !--preparation de "titvar" pour l''ecriture parmi les valeurs numeriques :
      DO jv = 2 , nvinfo
         titinf     = titvar(jv)(:jpchinf)
         titvar(jv) = '  '//titinf
      END DO

      !--Initialisation of the arrays for the accumulation
      DO jv = 1, nvinfo
         vinfom(jv) = 0._wp
      END DO
      naveg = 0

1000  FORMAT( 3(A20),4(1x,I6) )
1111  FORMAT( 3(F7.1,1X,F7.3,1X),I3,A )  
      !
   END SUBROUTINE lim_dia_init

#else
   !!----------------------------------------------------------------------
   !!   Default option :                             NO LIM-3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_dia         ! Empty routine
   END SUBROUTINE lim_dia
#endif

   !!======================================================================
END MODULE limdia
