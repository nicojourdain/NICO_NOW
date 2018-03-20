MODULE traadv
   !!==============================================================================
   !!                       ***  MODULE  traadv  ***
   !! Ocean active tracers:  advection trend 
   !!==============================================================================
   !! History :  2.0  !  2005-11  (G. Madec)  Original code
   !!            3.3  !  2010-09  (C. Ethe, G. Madec)  merge TRC-TRA + switch from velocity to transport
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_adv      : compute ocean tracer advection trend
   !!   tra_adv_ctl  : control the different options of advection scheme
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and active tracers
   USE dom_oce         ! ocean space and time domain
   USE traadv_cen2     ! 2nd order centered scheme (tra_adv_cen2   routine)
   USE traadv_tvd      ! TVD      scheme           (tra_adv_tvd    routine)
   USE traadv_muscl    ! MUSCL    scheme           (tra_adv_muscl  routine)
   USE traadv_muscl2   ! MUSCL2   scheme           (tra_adv_muscl2 routine)
   USE traadv_ubs      ! UBS      scheme           (tra_adv_ubs    routine)
   USE traadv_qck      ! QUICKEST scheme           (tra_adv_qck    routine)
   USE traadv_eiv      ! eddy induced velocity     (tra_adv_eiv    routine)
   USE cla             ! cross land advection      (cla_traadv     routine)
   USE ldftra_oce      ! lateral diffusion coefficient on tracers
   USE in_out_manager  ! I/O manager
   USE iom             ! I/O module
   USE prtctl          ! Print control
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing


   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_adv        ! routine called by step module
   PUBLIC   tra_adv_init   ! routine called by opa module

   !                                        !!* Namelist namtra_adv *
   LOGICAL ::   ln_traadv_cen2   = .TRUE.    ! 2nd order centered scheme flag
   LOGICAL ::   ln_traadv_tvd    = .FALSE.   ! TVD scheme flag
   LOGICAL ::   ln_traadv_muscl  = .FALSE.   ! MUSCL scheme flag
   LOGICAL ::   ln_traadv_muscl2 = .FALSE.   ! MUSCL2 scheme flag
   LOGICAL ::   ln_traadv_ubs    = .FALSE.   ! UBS scheme flag
   LOGICAL ::   ln_traadv_qck    = .FALSE.   ! QUICKEST scheme flag

   INTEGER ::   nadv   ! choice of the type of advection scheme

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: traadv.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_adv( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_adv  ***
      !!
      !! ** Purpose :   compute the ocean tracer advection trend.
      !!
      !! ** Method  : - Update (ua,va) with the advection term following nadv
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index
      !
      INTEGER ::   jk   ! dummy loop index
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zun, zvn, zwn
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('tra_adv')
      !
      CALL wrk_alloc( jpi, jpj, jpk, zun, zvn, zwn )
      !                                          ! set time step
      IF( neuler == 0 .AND. kt == nit000 ) THEN     ! at nit000
         r2dtra(:) =  rdttra(:)                          ! = rdtra (restarting with Euler time stepping)
      ELSEIF( kt <= nit000 + 1) THEN                ! at nit000 or nit000+1
         r2dtra(:) = 2._wp * rdttra(:)                   ! = 2 rdttra (leapfrog)
      ENDIF
      !
      IF( nn_cla == 1 )   CALL cla_traadv( kt )       !==  Cross Land Advection  ==! (hor. advection)
      !
      !                                               !==  effective transport  ==!
      DO jk = 1, jpkm1
         zun(:,:,jk) = e2u(:,:) * fse3u(:,:,jk) * un(:,:,jk)                  ! eulerian transport only
         zvn(:,:,jk) = e1v(:,:) * fse3v(:,:,jk) * vn(:,:,jk)
         zwn(:,:,jk) = e1t(:,:) * e2t(:,:)      * wn(:,:,jk)
      END DO
      zun(:,:,jpk) = 0._wp                                                     ! no transport trough the bottom
      zvn(:,:,jpk) = 0._wp                                                     ! no transport trough the bottom
      zwn(:,:,jpk) = 0._wp                                                     ! no transport trough the bottom
      !
      IF( lk_traldf_eiv .AND. .NOT. ln_traldf_grif )   &
         &              CALL tra_adv_eiv( kt, nit000, zun, zvn, zwn, 'TRA' )    ! add the eiv transport (if necessary)
      !
      CALL iom_put( "uocetr_eff", zun )                                         ! output effective transport      
      CALL iom_put( "vocetr_eff", zvn )
      CALL iom_put( "wocetr_eff", zwn )

      SELECT CASE ( nadv )                            !==  compute advection trend and add it to general trend  ==!
      CASE ( 1 )   ;    CALL tra_adv_cen2  ( kt, nit000, 'TRA',         zun, zvn, zwn, tsb, tsn, tsa, jpts )   !  2nd order centered
      CASE ( 2 )   ;    CALL tra_adv_tvd   ( kt, nit000, 'TRA', r2dtra, zun, zvn, zwn, tsb, tsn, tsa, jpts )   !  TVD 
      CASE ( 3 )   ;    CALL tra_adv_muscl ( kt, nit000, 'TRA', r2dtra, zun, zvn, zwn, tsb,      tsa, jpts )   !  MUSCL 
      CASE ( 4 )   ;    CALL tra_adv_muscl2( kt, nit000, 'TRA', r2dtra, zun, zvn, zwn, tsb, tsn, tsa, jpts )   !  MUSCL2 
      CASE ( 5 )   ;    CALL tra_adv_ubs   ( kt, nit000, 'TRA', r2dtra, zun, zvn, zwn, tsb, tsn, tsa, jpts )   !  UBS 
      CASE ( 6 )   ;    CALL tra_adv_qck   ( kt, nit000, 'TRA', r2dtra, zun, zvn, zwn, tsb, tsn, tsa, jpts )   !  QUICKEST 
      !
      CASE (-1 )                                      !==  esopa: test all possibility with control print  ==!
         CALL tra_adv_cen2  ( kt, nit000, 'TRA',         zun, zvn, zwn, tsb, tsn, tsa, jpts )          
         CALL prt_ctl( tab3d_1=tsa(:,:,:,jp_tem), clinfo1=' adv0 - Ta: ', mask1=tmask,               &
            &          tab3d_2=tsa(:,:,:,jp_sal), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
         CALL tra_adv_tvd   ( kt, nit000, 'TRA', r2dtra, zun, zvn, zwn, tsb, tsn, tsa, jpts )          
         CALL prt_ctl( tab3d_1=tsa(:,:,:,jp_tem), clinfo1=' adv1 - Ta: ', mask1=tmask,               &
            &          tab3d_2=tsa(:,:,:,jp_sal), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
         CALL tra_adv_muscl ( kt, nit000, 'TRA', r2dtra, zun, zvn, zwn, tsb,      tsa, jpts )          
         CALL prt_ctl( tab3d_1=tsa(:,:,:,jp_tem), clinfo1=' adv3 - Ta: ', mask1=tmask,               &
            &          tab3d_2=tsa(:,:,:,jp_sal), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
         CALL tra_adv_muscl2( kt, nit000, 'TRA', r2dtra, zun, zvn, zwn, tsb, tsn, tsa, jpts )          
         CALL prt_ctl( tab3d_1=tsa(:,:,:,jp_tem), clinfo1=' adv4 - Ta: ', mask1=tmask,               &
            &          tab3d_2=tsa(:,:,:,jp_sal), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
         CALL tra_adv_ubs   ( kt, nit000, 'TRA', r2dtra, zun, zvn, zwn, tsb, tsn, tsa, jpts )          
         CALL prt_ctl( tab3d_1=tsa(:,:,:,jp_tem), clinfo1=' adv5 - Ta: ', mask1=tmask,               &
            &          tab3d_2=tsa(:,:,:,jp_sal), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
         CALL tra_adv_qck   ( kt, nit000, 'TRA', r2dtra, zun, zvn, zwn, tsb, tsn, tsa, jpts )          
         CALL prt_ctl( tab3d_1=tsa(:,:,:,jp_tem), clinfo1=' adv6 - Ta: ', mask1=tmask,               &
            &          tab3d_2=tsa(:,:,:,jp_sal), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
      END SELECT
      !
      !                                              ! print mean trends (used for debugging)
      IF(ln_ctl)   CALL prt_ctl( tab3d_1=tsa(:,:,:,jp_tem), clinfo1=' adv  - Ta: ', mask1=tmask,               &
         &                       tab3d_2=tsa(:,:,:,jp_sal), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
      !
      IF( nn_timing == 1 )  CALL timing_stop('tra_adv')
      !
      CALL wrk_dealloc( jpi, jpj, jpk, zun, zvn, zwn )
      !                                          
   END SUBROUTINE tra_adv


   SUBROUTINE tra_adv_init
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE tra_adv_init  ***
      !!                
      !! ** Purpose :   Control the consistency between namelist options for 
      !!              tracer advection schemes and set nadv
      !!----------------------------------------------------------------------
      INTEGER ::   ioptio
      !!
      NAMELIST/namtra_adv/ ln_traadv_cen2 , ln_traadv_tvd,     &
         &                 ln_traadv_muscl, ln_traadv_muscl2,  &
         &                 ln_traadv_ubs  , ln_traadv_qck
      !!----------------------------------------------------------------------

      REWIND( numnam )                ! Read Namelist namtra_adv : tracer advection scheme
      READ  ( numnam, namtra_adv )

      IF(lwp) THEN                    ! Namelist print
         WRITE(numout,*)
         WRITE(numout,*) 'tra_adv_init : choice/control of the tracer advection scheme'
         WRITE(numout,*) '~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namtra_adv : chose a advection scheme for tracers'
         WRITE(numout,*) '      2nd order advection scheme     ln_traadv_cen2   = ', ln_traadv_cen2
         WRITE(numout,*) '      TVD advection scheme           ln_traadv_tvd    = ', ln_traadv_tvd
         WRITE(numout,*) '      MUSCL  advection scheme        ln_traadv_muscl  = ', ln_traadv_muscl
         WRITE(numout,*) '      MUSCL2 advection scheme        ln_traadv_muscl2 = ', ln_traadv_muscl2
         WRITE(numout,*) '      UBS    advection scheme        ln_traadv_ubs    = ', ln_traadv_ubs
         WRITE(numout,*) '      QUICKEST advection scheme      ln_traadv_qck    = ', ln_traadv_qck
      ENDIF

      ioptio = 0                      ! Parameter control
      IF( ln_traadv_cen2   )   ioptio = ioptio + 1
      IF( ln_traadv_tvd    )   ioptio = ioptio + 1
      IF( ln_traadv_muscl  )   ioptio = ioptio + 1
      IF( ln_traadv_muscl2 )   ioptio = ioptio + 1
      IF( ln_traadv_ubs    )   ioptio = ioptio + 1
      IF( ln_traadv_qck    )   ioptio = ioptio + 1
      IF( lk_esopa         )   ioptio =          1

      IF( ioptio /= 1 )   CALL ctl_stop( 'Choose ONE advection scheme in namelist namtra_adv' )

      !                              ! Set nadv
      IF( ln_traadv_cen2   )   nadv =  1
      IF( ln_traadv_tvd    )   nadv =  2
      IF( ln_traadv_muscl  )   nadv =  3
      IF( ln_traadv_muscl2 )   nadv =  4
      IF( ln_traadv_ubs    )   nadv =  5
      IF( ln_traadv_qck    )   nadv =  6
      IF( lk_esopa         )   nadv = -1

      IF(lwp) THEN                   ! Print the choice
         WRITE(numout,*)
         IF( nadv ==  1 )   WRITE(numout,*) '         2nd order scheme is used'
         IF( nadv ==  2 )   WRITE(numout,*) '         TVD       scheme is used'
         IF( nadv ==  3 )   WRITE(numout,*) '         MUSCL     scheme is used'
         IF( nadv ==  4 )   WRITE(numout,*) '         MUSCL2    scheme is used'
         IF( nadv ==  5 )   WRITE(numout,*) '         UBS       scheme is used'
         IF( nadv ==  6 )   WRITE(numout,*) '         QUICKEST  scheme is used'
         IF( nadv == -1 )   WRITE(numout,*) '         esopa test: use all advection scheme'
      ENDIF
      !
   END SUBROUTINE tra_adv_init

  !!======================================================================
END MODULE traadv
