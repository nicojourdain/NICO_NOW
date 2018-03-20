MODULE cla
   !!======================================================================
   !!                    ***  MODULE  cla  ***
   !! Cross Land Advection : specific update of the horizontal divergence,
   !!                        tracer trends and after velocity
   !!
   !!                 ---   Specific to ORCA_R2   ---
   !!
   !!======================================================================
   !! History :  1.0  ! 2002-11 (A. Bozec)  Original code
   !!            3.2  ! 2009-07 (G. Madec)  merge cla, cla_div, tra_cla, cla_dynspg
   !!                 !                     and correct a mpp bug reported by A.R. Porter
   !!----------------------------------------------------------------------
#if defined key_orca_r2
   !!----------------------------------------------------------------------
   !!   'key_orca_r2'                                 global ocean model R2
   !!----------------------------------------------------------------------
   !!   cla_div           : update of horizontal divergence at cla straits
   !!   tra_cla           : update of tracers at cla straits
   !!   cla_dynspg        : update of after horizontal velocities at cla straits
   !!   cla_init          : initialisation - control check
   !!   cla_bab_el_mandeb : cross land advection for Bab-el-mandeb strait
   !!   cla_gibraltar     : cross land advection for Gibraltar strait
   !!   cla_hormuz        : cross land advection for Hormuz strait
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE sbc_oce        ! surface boundary condition: ocean
   USE dynspg_oce     ! ocean dynamics: surface pressure gradient variables
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! distributed memory computing library
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE
   
   PUBLIC   cla_init     ! routine called by opa.F90
   PUBLIC   cla_div      ! routine called by divcur.F90
   PUBLIC   cla_traadv   ! routine called by traadv.F90
   PUBLIC   cla_dynspg   ! routine called by dynspg_flt.F90

   INTEGER  ::   nbab, ngib, nhor   ! presence or not of required grid-point on local domain
   !                                ! for Bab-el-Mandeb, Gibraltar, and Hormuz straits
   
   !                                           !   fixed part  !  time evolving    !!! profile of hdiv for some straits
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION (:) ::   hdiv_139_101, hdiv_139_101_kt    ! Gibraltar     (i,j)=(172,101)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION (:) ::   hdiv_139_102                     ! Gibraltar     (i,j)=(139,102)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION (:) ::   hdiv_141_102, hdiv_141_102_kt    ! Gibraltar     (i,j)=(141,102)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION (:) ::   hdiv_161_88 , hdiv_161_88_kt     ! Bab-el-Mandeb (i,j)=(161,88)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION (:) ::   hdiv_161_87                      ! Bab-el-Mandeb (i,j)=(161,87)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION (:) ::   hdiv_160_89 , hdiv_160_89_kt     ! Bab-el-Mandeb (i,j)=(160,89)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION (:) ::   hdiv_172_94                      ! Hormuz        (i,j)=(172, 94)

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION (:) ::   t_171_94_hor, s_171_94_hor       ! Temperature, salinity in Hormuz strait
   
   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: cla.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE cla_div( kt )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE div_cla  ***
      !!
      !! ** Purpose :   update the horizontal divergence of the velocity field
      !!              at some straits ( Gibraltar, Bab el Mandeb and Hormuz ).
      !!
      !! ** Method  : - first time-step: initialisation of cla
      !!              - all   time-step: using imposed transport at each strait, 
      !!              the now horizontal divergence is updated
      !!
      !! ** Action  :   phdivn   updted now horizontal divergence at cla straits
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index
      !!----------------------------------------------------------------------
      !     
      IF( kt == nit000 ) THEN
         !
         CALL cla_init                                        ! control check 
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'div_cla : cross land advection on hdiv '
         IF(lwp) WRITE(numout,*) '~~~~~~~~'
         !
         IF( nbab == 1 )   CALL cla_bab_el_mandeb('ini')    ! Bab el Mandeb ( Red Sea - Indian ocean )
         IF( ngib == 1 )   CALL cla_gibraltar    ('ini')    ! Gibraltar strait (Med Sea - Atlantic ocean)
         IF( nhor == 1 )   CALL cla_hormuz       ('ini')    ! Hormuz Strait ( Persian Gulf - Indian ocean )
         !
      ENDIF                           
      !
      IF( nbab == 1    )   CALL cla_bab_el_mandeb('div')    ! Bab el Mandeb ( Red Sea - Indian ocean )
      IF( ngib == 1    )   CALL cla_gibraltar    ('div')    ! Gibraltar strait (Med Sea - Atlantic ocean)
      IF( nhor == 1    )   CALL cla_hormuz       ('div')    ! Hormuz Strait ( Persian Gulf - Indian ocean )
      !
!!gm  lbc useless here, no?
!!gm      CALL lbc_lnk( hdivn, 'T', 1. )                    ! Lateral boundary conditions on hdivn
      !
   END SUBROUTINE cla_div
   
   
   SUBROUTINE cla_traadv( kt )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE tra_cla  ***
      !!                   
      !! ** Purpose :   Update the now trend due to the advection of tracers
      !!      and add it to the general trend of passive tracer equations
      !!      at some straits ( Bab el Mandeb, Gibraltar, Hormuz ).
      !!
      !! ** Method  :   using both imposed transport at each strait and T & S
      !!              budget, the now tracer trends is updated
      !!
      !! ** Action  :   (ta,sa)   updated now tracer trends at cla straits
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt         ! ocean time-step index
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 ) THEN 
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'tra_cla : cross land advection on tracers '
         IF(lwp) WRITE(numout,*) '~~~~~~~~'
      ENDIF
      !
      IF( nbab == 1    )   CALL cla_bab_el_mandeb('tra')      ! Bab el Mandeb strait
      IF( ngib == 1    )   CALL cla_gibraltar    ('tra')      ! Gibraltar strait
      IF( nhor == 1    )   CALL cla_hormuz       ('tra')      ! Hormuz Strait ( Persian Gulf)
      !
   END SUBROUTINE cla_traadv

   
   SUBROUTINE cla_dynspg( kt )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE cla_dynspg  ***
      !!                   
      !! ** Purpose :   Update the after velocity at some straits 
      !!              (Bab el Mandeb, Gibraltar, Hormuz).
      !!
      !! ** Method  :   required to compute the filtered surface pressure gradient 
      !!
      !! ** Action  :   (ua,va)   after velocity at the cla straits
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt         ! ocean time-step index
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 ) THEN 
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'cla_dynspg : cross land advection on (ua,va) '
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~'
      ENDIF
      !
      IF( nbab == 1    )   CALL cla_bab_el_mandeb('spg')      ! Bab el Mandeb strait
      IF( ngib == 1    )   CALL cla_gibraltar    ('spg')      ! Gibraltar strait
      IF( nhor == 1    )   CALL cla_hormuz       ('spg')      ! Hormuz Strait ( Persian Gulf)
      !
!!gm lbc is needed here, not?
!!gm      CALL lbc_lnk( hdivn, 'U', -1. )   ;   CALL lbc_lnk( hdivn, 'V', -1. )      ! Lateral boundary conditions 
      !
   END SUBROUTINE cla_dynspg


   SUBROUTINE cla_init
      !! -------------------------------------------------------------------
      !!                   ***  ROUTINE cla_init  ***
      !!           
      !! ** Purpose :   control check for mpp computation  
      !!
      !! ** Method  : - All the strait grid-points must be inside one of the 
      !!              local domain interior for the cla advection to work
      !!              properly in mpp (i.e. inside (2:jpim1,2:jpjm1) ).
      !!              Define the corresponding indicators (nbab, ngib, nhor)
      !!              - The profiles of cross-land fluxes are currently hard
      !!              coded for L31 levels. Stop if jpk/=31
      !!
      !! ** Action  :   nbab, ngib, nhor   strait inside the local domain or not
      !!---------------------------------------------------------------------
      REAL(wp) ::   ztemp   ! local scalar
      INTEGER  ::   ierr    ! local integer
      !!---------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'cla_init : cross land advection initialisation '
      IF(lwp) WRITE(numout,*) '~~~~~~~~~'
      !
      !                           ! Allocate arrays for this module
      ALLOCATE( hdiv_139_101(jpk) , hdiv_139_101_kt(jpk) ,     &    ! Gibraltar
         &      hdiv_139_102(jpk) ,                            &
         &      hdiv_141_102(jpk) , hdiv_141_102_kt(jpk) ,     &
         &      hdiv_161_88 (jpk) , hdiv_161_88_kt (jpk) ,     &    ! Bab-el-Mandeb
         &      hdiv_161_87 (jpk) ,                            &                     
         &      hdiv_160_89 (jpk) , hdiv_160_89_kt (jpk) ,     &     ! Hormuz
         &      hdiv_172_94 (jpk) ,                            &
         &      t_171_94_hor(jpk) , s_171_94_hor   (jpk) , STAT=ierr )
      IF( lk_mpp    )   CALL mpp_sum( ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'cla_init: unable to allocate arrays' )
      !
      IF( .NOT.lk_dynspg_flt )   CALL ctl_stop( 'cla_init: Cross Land Advection works only with lk_dynspg_flt=T ' )
      !
      IF( lk_vvl             )   CALL ctl_stop( 'cla_init: Cross Land Advection does not work with lk_vvl=T option' )
      !
      IF( jpk /= 31          )   CALL ctl_stop( 'cla_init: Cross Land Advection hard coded for ORCA_R2_L31' )
      !
      !                                        _|_______|_______|_
      !                                     89  |       |///////|  
      !                                        _|_______|_______|_
      ! ------------------------ !          88  |///////|       | 
      !   Bab el Mandeb strait   !             _|_______|_______|_
      ! ------------------------ !          87  |///////|       | 
      !                                        _|_______|_______|_
      !                                         |  160  |  161  |  
      !
      ! The 6 Bab el Mandeb grid-points must be inside one of the interior of the
      ! local domain for the cla advection to work properly (i.e. (2:jpim1,2:jpjm1)
      nbab = 0
      IF(  ( 1 <= mj0( 88) .AND. mj1( 89) <= jpj ) .AND.    &  !* (161,89), (161,88) and (161,88) on the local pocessor
         & ( 1 <= mi0(160) .AND. mi1(161) <= jpi )       )    nbab = 1 
      !
      ! test if there is no local domain that includes all required grid-points
      ztemp = REAL( nbab )
      IF( lk_mpp )   CALL mpp_sum( ztemp )      ! sum with other processors value
      IF( ztemp == 0 ) THEN                     ! Only 2 points in each direction, this should never be a problem
         CALL ctl_stop( ' cross land advection at Bab-el_Mandeb does not work with your processor cutting: change it' )
      ENDIF
      !                                        ___________________________
      ! ------------------------ !         102  |       |///////|       |
      !     Gibraltar strait     !             _|_______|_______|_______|_
      ! ------------------------ !         101  |       |///////|       |
      !                                        _|_______|_______|_______|_ 
      !                                         |  139  |  140  |  141  |
      !
      ! The 6 Gibraltar grid-points must be inside one of the interior of the
      ! local domain for the cla advection to work properly (i.e. (2:jpim1,2:jpjm1)
      ngib = 0
      IF(  ( 2 <= mj0(101) .AND. mj1(102) <= jpjm1 ) .AND.    &  !* (139:141,101:102) on the local pocessor
         & ( 2 <= mi0(139) .AND. mi1(141) <= jpim1 )       )    ngib = 1 
      !
      ! test if there is no local domain that includes all required grid-points
      ztemp = REAL( ngib )
      IF( lk_mpp )   CALL mpp_sum( ztemp )      ! sum with other processors value
      IF( ztemp == 0 ) THEN                     ! 3 points in i-direction, this may be a problem with some cutting
           CALL ctl_stop( ' cross land advection at Gibraltar does not work with your processor cutting: change it' )
      ENDIF
      !                                        _______________
      ! ------------------------ !          94  |/////|     | 
      !       Hormuz strait      !             _|_____|_____|_
      ! ------------------------ !                171   172     
      !           
      ! The 2 Hormuz grid-points must be inside one of the interior of the
      ! local domain for the cla advection to work properly (i.e. (2:jpim1,2:jpjm1)
      nhor = 0
      IF(    2 <= mj0( 94) .AND. mj1( 94) <= jpjm1  .AND.  & 
         &   2 <= mi0(171) .AND. mi1(172) <= jpim1         )   nhor = 1 
      !
      ! test if there is no local domain that includes all required grid-points
      ztemp = REAL( nhor )
      IF( lk_mpp )   CALL mpp_sum( ztemp )      ! sum with other processors value
      IF( ztemp == 0 ) THEN                     ! 3 points in i-direction, this may be a problem with some cutting
           CALL ctl_stop( ' cross land advection at Hormuz does not work with your processor cutting: change it' )
      ENDIF
      !
   END SUBROUTINE cla_init


   SUBROUTINE cla_bab_el_mandeb( cd_td )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE cla_bab_el_mandeb  ***
      !!       
      !! ** Purpose :   update the now horizontal divergence, the tracer tendancy
      !!              and the after velocity in vicinity of Bab el Mandeb ( Red Sea - Indian ocean).
      !!
      !! ** Method  :   compute the exchanges at each side of the strait :
      !!
      !!       surf. zio_flow
      !! (+ balance of emp) /\  |\\\\\\\\\\\|
      !!                    ||  |\\\\\\\\\\\|  
      !!    deep zio_flow   ||  |\\\\\\\\\\\|  
      !!            |  ||   ||  |\\\\\\\\\\\|  
      !!        89  |  ||   ||  |\\\\\\\\\\\|  
      !!            |__\/_v_||__|____________ 
      !!            !\\\\\\\\\\\|          surf. zio_flow
      !!            |\\\\\\\\\\\|<===    (+ balance of emp)
      !!            |\\\\\\\\\\\u
      !!        88  |\\\\\\\\\\\|<---      deep  zrecirc (upper+deep at 2 different levels)
      !!            |___________|__________   
      !!            !\\\\\\\\\\\|         
      !!            |\\\\\\\\\\\| ---\     deep  zrecirc (upper+deep) 
      !!        87  !\\\\\\\\\\\u ===/   + deep  zio_flow   (all at the same level)
      !!            !\\\\\\\\\\\|  
      !!            !___________|__________ 
      !!                160         161
      !!
      !!----------------------------------------------------------------------
      CHARACTER(len=1), INTENT(in) ::   cd_td   ! ='div' update the divergence
      !                                         ! ='tra' update the tracers
      !                                         ! ='spg' update after velocity
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zemp_red     ! temporary scalar
      REAL(wp) ::   zio_flow, zrecirc_upp, zrecirc_mid, zrecirc_bot
      !!---------------------------------------------------------------------
      !
      SELECT CASE( cd_td ) 
      !                     ! ---------------- !
      CASE( 'ini' )         !  initialisation  ! 
         !                  ! ---------------- ! 
         !                                   
         zio_flow    = 0.4e6                       ! imposed in/out flow
         zrecirc_upp = 0.2e6                       ! imposed upper recirculation water
         zrecirc_bot = 0.5e6                       ! imposed bottom  recirculation water

         hdiv_161_88(:) = 0.e0                     ! (161,88) Gulf of Aden side, north point
         hdiv_161_87(:) = 0.e0                     ! (161,87) Gulf of Aden side, south point
         hdiv_160_89(:) = 0.e0                     ! (160,89) Red sea side

         DO jj = mj0(88), mj1(88)              !** profile of hdiv at (161,88)   (Gulf of Aden side, north point)
            DO ji = mi0(161), mi1(161)         !------------------------------
               DO jk = 1, 8                        ! surface in/out flow   (Ind -> Red)   (div >0)
                  hdiv_161_88(jk) = + zio_flow / ( 8. * e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
               END DO
               !                                   ! recirculation water   (Ind -> Red)   (div >0)
               hdiv_161_88(20) =                 + zrecirc_upp   / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,20) )
               hdiv_161_88(21) = + ( zrecirc_bot - zrecirc_upp ) / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,21) )
            END DO
         END DO
         !
         DO jj = mj0(87), mj1(87)              !** profile of hdiv at (161,88)   (Gulf of Aden side, south point)
            DO ji = mi0(161), mi1(161)         !------------------------------
               !                                   ! deep out flow + recirculation   (Red -> Ind)   (div <0)
               hdiv_161_87(21) = - ( zio_flow + zrecirc_bot ) / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,21) )
            END DO
         END DO
         !
         DO jj = mj0(89), mj1(89)              !** profile of hdiv at (161,88)   (Red sea side)
            DO ji = mi0(160), mi1(160)         !------------------------------
               DO jk = 1, 8                        ! surface inflow    (Ind -> Red)   (div <0)
                  hdiv_160_89(jk) = - zio_flow / ( 8. * e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
               END DO
               !                                   ! deep    outflow   (Red -> Ind)   (div >0)
               hdiv_160_89(16)    = + zio_flow / (      e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,16) )
            END DO
         END DO
         !                  ! ---------------- !
      CASE( 'div' )         !   update hdivn   ! (call by divcur module)
         !                  ! ---------=====-- ! 
         !                                     !** emp on the Red Sea   (div >0) 
         zemp_red = 0.e0                       !---------------------
         DO jj = mj0(87), mj1(96)                  ! sum over the Red sea
            DO ji = mi0(148), mi1(160) 
               zemp_red = zemp_red + emp(ji,jj) * e1t(ji,jj) * e2t(ji,jj) * tmask_i(ji,jj)
            END DO
         END DO
         IF( lk_mpp )   CALL mpp_sum( zemp_red )   ! sum with other processors value
         zemp_red = zemp_red * 1.e-3               ! convert in m3
         !
         !                                     !** Correct hdivn (including emp adjustment)
         !                                     !-------------------------------------------
         DO jj = mj0(88), mj1(88)                  !* profile of hdiv at (161,88)   (Gulf of Aden side, north point)
            DO ji = mi0(161), mi1(161) 
               hdiv_161_88_kt(:) = hdiv_161_88(:)
               DO jk = 1, 8                              ! increase the inflow from the Indian   (div >0) 
                  hdiv_161_88_kt(jk) = hdiv_161_88(jk) + zemp_red / (8. * e2u(ji,jj) * fse3u(ji,jj,jk) )
               END DO
               hdivn(ji,jj,:) = hdivn(ji,jj,:) + hdiv_161_88_kt(:)
            END DO
         END DO
         DO jj = mj0(87), mj1(87)                  !* profile of divergence at (161,87)   (Gulf of Aden side, south point)
            DO ji = mi0(161), mi1(161) 
               hdivn(ji,jj,:) = hdivn(ji,jj,:) + hdiv_161_87(:)
            END DO
         END DO
         DO jj = mj0(89), mj1(89)                  !* profile of divergence at (160,89)   (Red sea side)
            DO ji = mi0(160), mi1(160) 
               hdiv_160_89_kt(:) = hdiv_160_89(:)
               DO jk = 1, 18                              ! increase the inflow from the Indian   (div <0) 
                  hdiv_160_89_kt(jk) = hdiv_160_89(jk) - zemp_red / (10. * e1v(ji,jj) * fse3v(ji,jj,jk) )
               END DO
               hdivn(ji, jj,:) = hdivn(ji, jj,:) + hdiv_160_89_kt(:)
            END DO
         END DO
         !                  ! ---------------- !
      CASE( 'tra' )         !  update (ta,sa)  ! (call by traadv module)
         !                  ! --------=======- !
         !
         DO jj = mj0(88), mj1(88)              !** (161,88)   (Gulf of Aden side, north point)
            DO ji = mi0(161), mi1(161) 
               DO jk = 1, jpkm1                         ! surf inflow + reciculation (from Gulf of Aden)
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) - hdiv_161_88_kt(jk) * tsn(ji,jj,jk,jp_tem)
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) - hdiv_161_88_kt(jk) * tsn(ji,jj,jk,jp_sal)
               END DO
            END DO
         END DO
         DO jj = mj0(87), mj1(87)              !** (161,87)   (Gulf of Aden side, south point)
            DO ji = mi0(161), mi1(161) 
               jk =  21                                 ! deep outflow + recirulation (combined flux)
               tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) + hdiv_161_88(20) * tsn(ji  ,jj+1,20,jp_tem)   &  ! upper recirculation from Gulf of Aden
                  &                        + hdiv_161_88(21) * tsn(ji  ,jj+1,21,jp_tem)   &  ! deep  recirculation from Gulf of Aden
                  &                        + hdiv_160_89(16) * tsn(ji-1,jj+2,16,jp_tem)      ! deep inflow from Red sea
               tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) + hdiv_161_88(20) * tsn(ji  ,jj+1,20,jp_sal)   &
                  &                        + hdiv_161_88(21) * tsn(ji  ,jj+1,21,jp_sal)   &
                  &                        + hdiv_160_89(16) * tsn(ji-1,jj+2,16,jp_sal)   
            END DO
         END DO
         DO jj = mj0(89), mj1(89)              !** (161,88)   (Red sea side)
            DO ji = mi0(160), mi1(160)
               DO jk = 1, 14                            ! surface inflow (from Gulf of Aden)
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) - hdiv_160_89_kt(jk) * tsn(ji+1,jj-1,jk,jp_tem)
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) - hdiv_160_89_kt(jk) * tsn(ji+1,jj-1,jk,jp_sal)
               END DO
               !                                  ! deep    outflow (from Red sea)
               tsa(ji,jj,16,jp_tem) = tsa(ji,jj,16,jp_tem) - hdiv_160_89(16) * tsn(ji,jj,16,jp_tem)
               tsa(ji,jj,16,jp_sal) = tsa(ji,jj,16,jp_sal) - hdiv_160_89(16) * tsn(ji,jj,16,jp_sal)
            END DO
         END DO
         !
         !                  ! ---------------- !
      CASE( 'spg' )         !  update (ua,va)  ! (call by dynspg module)
         !                  ! --------=======- !
         ! at this stage, (ua,va) are the after velocity, not the tendancy
         ! compute the velocity from the divergence at T-point
         !
         DO jj = mj0(88), mj1(88)              !** (160,88)   (Gulf of Aden side, north point)
            DO ji = mi0(160), mi1(160)                   ! 160, not 161 as it is a U-point) 
               ua(ji,jj,:) = - hdiv_161_88_kt(:) / ( e1t(ji+1,jj) * e2t(ji+1,jj) * fse3t(ji+1,jj,:) )   &
                  &                              * e2u(ji,jj) * fse3u(ji,jj,:)
            END DO
         END DO
         DO jj = mj0(87), mj1(87)              !** (160,87)   (Gulf of Aden side, south point)
            DO ji = mi0(160), mi1(160)                   ! 160, not 161 as it is a U-point) 
               ua(ji,jj,:) = - hdiv_161_87(:) / ( e1t(ji+1,jj) * e2t(ji+1,jj) * fse3t(ji+1,jj,:) )   &
                  &                           * e2u(ji,jj) * fse3u(ji,jj,:)
            END DO
         END DO
         DO jj = mj0(88), mj1(88)              !** profile of divergence at (160,89)   (Red sea side)
            DO ji = mi0(160), mi1(160)                   ! 88, not 89 as it is a V-point)
               va(ji,jj,:) = - hdiv_160_89_kt(:) / ( e1t(ji,jj+1) * e2t(ji,jj+1) * fse3t(ji,jj+1,:) )   &
                  &                              * e1v(ji,jj) * fse3v(ji,jj,:)
            END DO
         END DO
      END SELECT
      !
   END SUBROUTINE cla_bab_el_mandeb
   

   SUBROUTINE cla_gibraltar( cd_td )
      !! -------------------------------------------------------------------
      !!                 ***  ROUTINE cla_gibraltar  ***
      !!        
      !! ** Purpose :   update the now horizontal divergence, the tracer 
      !!              tendancyand the after velocity in vicinity of Gibraltar 
      !!              strait ( Persian Gulf - Indian ocean ).
      !!
      !! ** Method :
      !!                     _______________________            
      !!      deep  zio_flow /====|///////|====> surf. zio_flow
      !!    + deep  zrecirc  \----|///////|     (+balance of emp)
      !! 102                      u///////u
      !!      mid.  recicul    <--|///////|<==== deep  zio_flow
      !!                     _____|_______|_____  
      !!      surf. zio_flow ====>|///////|       
      !!    (+balance of emp)     |///////|
      !! 101                      u///////|             
      !!      mid.  recicul    -->|///////|               Caution: zrecirc split into 
      !!      deep  zrecirc  ---->|///////|                  upper & bottom recirculation
      !!                   _______|_______|_______ 
      !!                     139     140     141  
      !!
      !!---------------------------------------------------------------------
      CHARACTER(len=1), INTENT(in) ::   cd_td   ! ='div' update the divergence
      !                                         ! ='tra' update the tracers
      !                                         ! ='spg' update after velocity
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zemp_med     ! temporary scalar
      REAL(wp) ::   zio_flow, zrecirc_upp, zrecirc_mid, zrecirc_bot
      !!---------------------------------------------------------------------
      !
      SELECT CASE( cd_td ) 
      !                     ! ---------------- !
      CASE( 'ini' )         !  initialisation  ! 
         !                  ! ---------------- ! 
         !                                     !** initialization of the velocity
         hdiv_139_101(:) = 0.e0                     !  139,101 (Atlantic side, south point)
         hdiv_139_102(:) = 0.e0                     !  139,102 (Atlantic side, north point)
         hdiv_141_102(:) = 0.e0                     !  141,102 (Med sea  side)
            
         !                                     !** imposed transport
         zio_flow    = 0.8e6                        ! inflow surface  water
         zrecirc_mid = 0.7e6                        ! middle recirculation water
         zrecirc_upp = 2.5e6                        ! upper  recirculation water
         zrecirc_bot = 3.5e6                        ! bottom recirculation water
         !
         DO jj = mj0(101), mj1(101)            !** profile of hdiv at 139,101 (Atlantic side, south point)
            DO ji = mi0(139), mi1(139)         !-----------------------------
               DO jk = 1, 14                        ! surface in/out flow (Atl -> Med)   (div >0)
                  hdiv_139_101(jk) = + zio_flow / ( 14. * e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
               END DO
               DO jk = 15, 20                       ! middle  reciculation (Atl 101 -> Atl 102)   (div >0)   
                  hdiv_139_101(jk) = + zrecirc_mid / ( 6. * e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
               END DO
               !                                    ! upper reciculation (Atl 101 -> Atl 101)   (div >0)
               hdiv_139_101(21) =               + zrecirc_upp / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
               !
               !                                    ! upper & bottom reciculation (Atl 101 -> Atl 101 & 102)   (div >0)
               hdiv_139_101(22) = ( zrecirc_bot - zrecirc_upp ) / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
            END DO
         END DO
         DO jj = mj0(102), mj1(102)            !** profile of hdiv at 139,102 (Atlantic side, north point)
            DO ji = mi0(139), mi1(139)         !-----------------------------
               DO jk = 15, 20                       ! middle reciculation (Atl 101 -> Atl 102)   (div <0)                
                  hdiv_139_102(jk) = - zrecirc_mid / ( 6. * e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
               END DO
               !                                    ! outflow of Mediterranean sea + deep recirculation   (div <0) 
               hdiv_139_102(22) = - ( zio_flow + zrecirc_bot ) / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
            END DO
         END DO
         DO jj = mj0(102), mj1(102)            !** velocity profile at 141,102  (Med sea side)
            DO ji = mi0(141), mi1(141)         !------------------------------
               DO  jk = 1, 14                       ! surface inflow in the Med     (div <0)
                  hdiv_141_102(jk) = - zio_flow / ( 14. * e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
               END DO
               !                                    ! deep    outflow toward the Atlantic    (div >0) 
               hdiv_141_102(21)    = + zio_flow / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
            END DO
         END DO
         !                  ! ---------------- !
      CASE( 'div' )         !   update hdivn   ! (call by divcur module)
         !                  ! ---------=====-- ! 
         !                                     !** emp on the Mediterranean Sea  (div >0) 
         zemp_med = 0.e0                       !-------------------------------
         DO jj = mj0(96), mj1(110)                  ! sum over the Med sea
            DO ji = mi0(141),mi1(181)
               zemp_med = zemp_med + emp(ji,jj) * e1t(ji,jj) * e2t(ji,jj) * tmask_i(ji,jj) 
            END DO
         END DO
         DO jj = mj0(96), mj1(96)                   ! minus 2 points in Red Sea 
            DO ji = mi0(148),mi1(148)
               zemp_med = zemp_med - emp(ji,jj) * e1t(ji,jj) * e2t(ji,jj) * tmask_i(ji,jj)
            END DO
            DO ji = mi0(149),mi1(149)
               zemp_med = zemp_med - emp(ji,jj) * e1t(ji,jj) * e2t(ji,jj) * tmask_i(ji,jj)
            END DO
         END DO
         IF( lk_mpp )   CALL mpp_sum( zemp_med )    ! sum with other processors value
         zemp_med = zemp_med * 1.e-3                ! convert in m3
         !
         !                                     !** Correct hdivn (including emp adjustment)
         !                                     !-------------------------------------------
         DO jj = mj0(101), mj1(101)                 !* 139,101 (Atlantic side, south point)
            DO ji = mi0(139), mi1(139) 
               hdiv_139_101_kt(:) = hdiv_139_101(:)      
               DO jk = 1, 14                              ! increase the inflow from the Atlantic   (div >0) 
                  hdiv_139_101_kt(jk) = hdiv_139_101(jk) + zemp_med / ( 14. * e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
               END DO
               hdivn(ji, jj,:) = hdivn(ji, jj,:) + hdiv_139_101_kt(:)
            END DO
         END DO
         DO jj = mj0(102), mj1(102)                 !* 139,102 (Atlantic side, north point)
            DO ji = mi0(139), mi1(139) 
               hdivn(ji,jj,:) = hdivn(ji,jj,:) + hdiv_139_102(:)
            END DO
         END DO
         DO jj = mj0(102), mj1(102)                 !* 141,102 (Med side)
            DO ji = mi0(141), mi1(141) 
               hdiv_141_102(:) = hdiv_141_102(:)
               DO jk = 1, 14                              ! increase the inflow from the Atlantic   (div <0)
                  hdiv_141_102_kt(jk) = hdiv_141_102(jk) - zemp_med / ( 14. * e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
               END DO
               hdivn(ji, jj,:) = hdivn(ji, jj,:) + hdiv_141_102_kt(:)
            END DO
         END DO
         !                  ! ---------------- !
      CASE( 'tra' )         !  update (ta,sa)  ! (call by traadv module)
         !                  ! --------=======- !
         !
         DO jj = mj0(101), mj1(101)            !** 139,101 (Atlantic side, south point)   (div >0)
            DO ji = mi0(139), mi1(139) 
               DO jk = 1, jpkm1                         ! surf inflow + mid. & bottom reciculation (from Atlantic)	
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) - hdiv_139_101_kt(jk) * tsn(ji,jj,jk,jp_tem)
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) - hdiv_139_101_kt(jk) * tsn(ji,jj,jk,jp_sal)
               END DO
            END DO
         END DO
         !
         DO jj = mj0(102), mj1(102)            !** 139,102 (Atlantic side, north point)   (div <0)
            DO ji = mi0(139), mi1(139) 
               DO jk = 15, 20                            ! middle  reciculation (Atl 101 -> Atl 102)   (div <0)
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) - hdiv_139_102(jk) * tsn(ji,jj-1,jk,jp_tem)  ! middle Atlantic recirculation
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) - hdiv_139_102(jk) * tsn(ji,jj-1,jk,jp_sal)
               END DO
               !                                         ! upper & bottom Atl. reciculation (Atl 101 -> Atl 102) - (div <0)
               !                                         ! deep Med flow                    (Med 102 -> Atl 102) - (div <0)
               tsa(ji,jj,22,jp_tem) = tsa(ji,jj,22,jp_tem) + hdiv_141_102(21) * tsn(ji+2,jj,21,jp_tem)   &  ! deep Med flow  
                  &                        + hdiv_139_101(21) * tsn(ji,jj-1,21,jp_tem)   &  ! upper  Atlantic recirculation  
                  &                        + hdiv_139_101(22) * tsn(ji,jj-1,22,jp_tem)      ! bottom Atlantic recirculation  
               tsa(ji,jj,22,jp_sal) = tsa(ji,jj,22,jp_sal) + hdiv_141_102(21) * tsn(ji+2,jj,21,jp_sal)   &
                  &                        + hdiv_139_101(21) * tsn(ji,jj-1,21,jp_sal)   &
                  &                        + hdiv_139_101(22) * tsn(ji,jj-1,22,jp_sal) 
            END DO
         END DO
         DO jj = mj0(102), mj1(102)                 !* 141,102 (Med side)   (div <0)
            DO ji = mi0(141), mi1(141) 
               DO jk = 1, 14                             ! surface flow from Atlantic to Med sea
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) - hdiv_141_102_kt(jk) * tsn(ji-2,jj-1,jk,jp_tem)
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) - hdiv_141_102_kt(jk) * tsn(ji-2,jj-1,jk,jp_sal)
               END DO
               !                                         ! deeper flow from Med sea to Atlantic
               tsa(ji,jj,21,jp_tem) = tsa(ji,jj,21,jp_tem) - hdiv_141_102(21) * tsn(ji,jj,21,jp_tem)
               tsa(ji,jj,21,jp_sal) = tsa(ji,jj,21,jp_sal) - hdiv_141_102(21) * tsn(ji,jj,21,jp_sal)
            END DO
         END DO
         !                  ! ---------------- !
      CASE( 'spg' )         !  update (ua,va)  ! (call by dynspg module)
         !                  ! --------=======- !
         ! at this stage, (ua,va) are the after velocity, not the tendancy
         ! compute the velocity from the divergence at T-point
         !
         DO jj = mj0(101), mj1(101)            !** 139,101 (Atlantic side, south point)
            DO ji = mi0(139), mi1(139)                    ! div >0 => ua >0, same sign
               ua(ji,jj,:) = hdiv_139_101_kt(:) / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,:) )   &
                  &                             * e2u(ji,jj) * fse3u(ji,jj,:)
            END DO
         END DO
         DO jj = mj0(102), mj1(102)            !** 139,102 (Atlantic side, north point)
            DO ji = mi0(139), mi1(139)                    ! div <0 => ua <0, same sign
               ua(ji,jj,:) = hdiv_139_102(:) / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,:) )   &
                  &                          * e2u(ji,jj) * fse3u(ji,jj,:)   
            END DO
         END DO
         DO jj = mj0(102), mj1(102)            !** 140,102 (Med side) (140 not 141 as it is a U-point)
            DO ji = mi0(140), mi1(140)                    ! div >0 => ua <0, opposite sign
               ua(ji,jj,:) = - hdiv_141_102(:) / ( e1t(ji+1,jj) * e2t(ji+1,jj) * fse3t(ji+1,jj,:) )   &
                  &                            * e2u(ji,jj) * fse3u(ji,jj,:)
            END DO
         END DO
         !
      END SELECT
      !
   END SUBROUTINE cla_gibraltar


   SUBROUTINE cla_hormuz( cd_td )
      !! -------------------------------------------------------------------
      !!                   ***  ROUTINE div_hormuz  ***
      !!              
      !! ** Purpose :   update the now horizontal divergence, the tracer 
      !!              tendancyand the after velocity in vicinity of Hormuz 
      !!              strait ( Persian Gulf - Indian ocean ).
      !!
      !! ** Method  :   Hormuz strait
      !!            ______________   
      !!            |/////|<==      surface inflow
      !!        94  |/////|     
      !!            |/////|==>      deep    outflow
      !!            |_____|_______
      !!              171    172     
      !!---------------------------------------------------------------------
      CHARACTER(len=1), INTENT(in) ::   cd_td   ! ='ini' initialisation
      !!                                        ! ='div' update the divergence
      !!                                        ! ='tra' update the tracers
      !!                                        ! ='spg' update after velocity
      !!
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zio_flow     ! temporary scalar
      !!---------------------------------------------------------------------
      !
      SELECT CASE( cd_td ) 
      !                     ! ---------------- !
      CASE( 'ini' )         !  initialisation  ! 
         !                  ! ---------------- ! 
         !                                     !** profile of horizontal divergence due to cross-land advection
         zio_flow  = 1.e6                          ! imposed in/out flow
         !
         hdiv_172_94(:) = 0.e0         
         !
         DO jj = mj0(94), mj1(94)                  ! in/out flow at (i,j) = (172,94)
            DO ji = mi0(172), mi1(172) 
               DO jk = 1, 8                            ! surface inflow  (Indian ocean to Persian Gulf) (div<0)
                  hdiv_172_94(jk) = - ( zio_flow / 8.e0 * e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
               END DO
               DO jk = 16, 18                          ! deep    outflow (Persian Gulf to Indian ocean) (div>0)
                  hdiv_172_94(jk) = + ( zio_flow / 3.e0 * e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
               END DO
            END DO
         END DO
         !                                     !** T & S profile in the Hormuz strait (use in deep outflow)
         !      Temperature       and         Salinity
         t_171_94_hor(:)  = 0.e0   ;   s_171_94_hor(:)  = 0.e0
         t_171_94_hor(16) = 18.4   ;   s_171_94_hor(16) = 36.27
         t_171_94_hor(17) = 17.8   ;   s_171_94_hor(17) = 36.4
         t_171_94_hor(18) = 16.    ;   s_171_94_hor(18) = 36.27
         !
         !                  ! ---------------- !
      CASE( 'div' )         !   update hdivn   ! (call by divcur module)
         !                  ! ---------=====-- ! 
         !                                   
         DO jj = mj0(94), mj1(94)              !** 172,94 (Indian ocean side)
            DO ji = mi0(172), mi1(172) 
               hdivn(ji,jj,:) = hdivn(ji,jj,:) + hdiv_172_94(:)
            END DO
         END DO
         !                  ! ---------------- !
      CASE( 'tra' )         !  update (ta,sa)  ! (call by traadv module)
         !                  ! --------=======- !
         !                          
         DO jj = mj0(94), mj1(94)              !** 172,94 (Indian ocean side)
            DO ji = mi0(172), mi1(172) 
               DO jk = 1, 8                          ! surface inflow   (Indian ocean to Persian Gulf) (div<0)
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) - hdiv_172_94(jk) * tsn(ji,jj,jk,jp_tem) 
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) - hdiv_172_94(jk) * tsn(ji,jj,jk,jp_sal) 
               END DO
               DO jk = 16, 18                        ! deep outflow     (Persian Gulf to Indian ocean) (div>0)
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) - hdiv_172_94(jk) * t_171_94_hor(jk)
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) - hdiv_172_94(jk) * s_171_94_hor(jk)
               END DO
            END DO
         END DO
         !                  ! ---------------- !
      CASE( 'spg' )         !  update (ua,va)  ! (call by dynspg module)
         !                  ! --------=======- !
         ! No barotropic flow through Hormuz strait
         ! at this stage, (ua,va) are the after velocity, not the tendancy
         ! compute the velocity from the divergence at T-point
         DO jj = mj0(94), mj1(94)              !** 171,94 (Indian ocean side) (171 not 172 as it is the western U-point)
            DO ji = mi0(171), mi1(171)                ! div >0 => ua >0, opposite sign
               ua(ji,jj,:) = - hdiv_172_94(:) / ( e1t(ji+1,jj) * e2t(ji+1,jj) * fse3t(ji+1,jj,:) )   &
                  &                           * e2u(ji,jj) * fse3u(ji,jj,:)
            END DO
         END DO
         !
      END SELECT
      !
   END SUBROUTINE cla_hormuz
   
#else
   !!----------------------------------------------------------------------
   !!   Default key                                            Dummy module
   !!----------------------------------------------------------------------
   USE lib_mpp, ONLY:   ctl_stop
CONTAINS
   SUBROUTINE cla_init
      CALL ctl_stop( 'cla_init: Cross Land Advection hard coded for ORCA_R2 with 31 levels' )
   END SUBROUTINE cla_init
   SUBROUTINE cla_div( kt )
      WRITE(*,*) 'cla_div: You should have not see this print! error?', kt
   END SUBROUTINE cla_div
   SUBROUTINE cla_traadv( kt ) 
      WRITE(*,*) 'cla_traadv: You should have not see this print! error?', kt
   END SUBROUTINE cla_traadv
   SUBROUTINE cla_dynspg( kt ) 
      WRITE(*,*) 'dyn_spg_cla: You should have not see this print! error?', kt
   END SUBROUTINE cla_dynspg
#endif
   
   !!======================================================================
END MODULE cla
