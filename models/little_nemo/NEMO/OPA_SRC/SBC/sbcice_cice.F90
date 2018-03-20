MODULE sbcice_cice
   !!======================================================================
   !!                       ***  MODULE  sbcice_cice  ***
   !! To couple with sea ice model CICE (LANL)
   !!=====================================================================
#if defined key_cice
   !!----------------------------------------------------------------------
   !!   'key_cice' :                                     CICE sea-ice model
   !!----------------------------------------------------------------------
   !!   sbc_ice_cice  : sea-ice model time-stepping and update ocean sbc over ice-covered area
   !!   
   !!   
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE domvvl
   USE phycst, only : rcp, rau0
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distributed memory computing library
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE wrk_nemo        ! work arrays
   USE timing          ! Timing
   USE daymod          ! calendar
   USE fldread         ! read input fields

   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE sbc_ice         ! Surface boundary condition: ice   fields
   USE sbcblk_core     ! Surface boundary condition: CORE bulk
   USE sbccpl

   USE ice_kinds_mod
   USE ice_blocks
   USE ice_domain
   USE ice_domain_size
   USE ice_boundary
   USE ice_constants
   USE ice_gather_scatter
   USE ice_calendar, only: dt
   USE ice_state, only: aice,aicen,uvel,vvel,vsnon,vicen
   USE ice_flux, only: strax,stray,strocnx,strocny,frain,fsnow,  &
                sst,sss,uocn,vocn,ss_tltx,ss_tlty,fsalt_gbm,     &
                fresh_gbm,fhocn_gbm,fswthru_gbm,frzmlt,          &
                flatn_f,fsurfn_f,fcondtopn_f,                    &
                uatm,vatm,wind,fsw,flw,Tair,potT,Qa,rhoa,zlvl,   &
                swvdr,swvdf,swidr,swidf
   USE ice_forcing, only: frcvdr,frcvdf,frcidr,frcidf
   USE ice_atmo, only: calc_strair
   USE ice_therm_vertical, only: calc_Tsfc

   USE CICE_InitMod
   USE CICE_RunMod
   USE CICE_FinalMod

   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC cice_sbc_init   ! routine called by sbc_init
   PUBLIC cice_sbc_final  ! routine called by sbc_final
   PUBLIC sbc_ice_cice    ! routine called by sbc

   INTEGER , PARAMETER ::   jpfld   = 13   ! maximum number of files to read 
   INTEGER , PARAMETER ::   jp_snow = 1    ! index of snow file
   INTEGER , PARAMETER ::   jp_rain = 2    ! index of rain file
   INTEGER , PARAMETER ::   jp_sblm = 3    ! index of sublimation file
   INTEGER , PARAMETER ::   jp_top1 = 4    ! index of category 1 topmelt file
   INTEGER , PARAMETER ::   jp_top2 = 5    ! index of category 2 topmelt file
   INTEGER , PARAMETER ::   jp_top3 = 6    ! index of category 3 topmelt file
   INTEGER , PARAMETER ::   jp_top4 = 7    ! index of category 4 topmelt file
   INTEGER , PARAMETER ::   jp_top5 = 8    ! index of category 5 topmelt file
   INTEGER , PARAMETER ::   jp_bot1 = 9    ! index of category 1 botmelt file
   INTEGER , PARAMETER ::   jp_bot2 = 10   ! index of category 2 botmelt file
   INTEGER , PARAMETER ::   jp_bot3 = 11   ! index of category 3 botmelt file
   INTEGER , PARAMETER ::   jp_bot4 = 12   ! index of category 4 botmelt file
   INTEGER , PARAMETER ::   jp_bot5 = 13   ! index of category 5 botmelt file
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf    ! structure of input fields (file informations, fields read)

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:), PRIVATE ::   png     ! local array used in sbc_cice_ice

   !! * Substitutions
#  include "domzgr_substitute.h90"

CONTAINS

   INTEGER FUNCTION sbc_ice_cice_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION sbc_ice_cice_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( png(jpi,jpj,jpnij), STAT=sbc_ice_cice_alloc )
      IF( lk_mpp                 )   CALL mpp_sum ( sbc_ice_cice_alloc )
      IF( sbc_ice_cice_alloc > 0 )   CALL ctl_warn('sbc_ice_cice_alloc: allocation of arrays failed.')
   END FUNCTION sbc_ice_cice_alloc

   SUBROUTINE sbc_ice_cice( kt, nsbc )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sbc_ice_cice  ***
      !!                   
      !! ** Purpose :   update the ocean surface boundary condition via the 
      !!                CICE Sea Ice Model time stepping 
      !!
      !! ** Method  : - Get any extra forcing fields for CICE  
      !!              - Prepare forcing fields
      !!              - CICE model time stepping
      !!              - call the routine that computes mass and 
      !!                heat fluxes at the ice/ocean interface
      !!
      !! ** Action  : - time evolution of the CICE sea-ice model
      !!              - update all sbc variables below sea-ice:
      !!                utau, vtau, qns , qsr, emp , emps
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt      ! ocean time step
      INTEGER, INTENT(in) ::   nsbc    ! surface forcing type
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('sbc_ice_cice')
      !
      !                                        !----------------------!
      IF( MOD( kt-1, nn_fsbc ) == 0 ) THEN     !  Ice time-step only  !
         !                                     !----------------------!

         ! Make sure any fluxes required for CICE are set
         IF ( nsbc == 2 )  THEN
            CALL cice_sbc_force(kt)
         ELSE IF ( nsbc == 5 ) THEN
            CALL sbc_cpl_ice_flx( 1.0-fr_i  )
         ENDIF

         CALL cice_sbc_in ( kt, nsbc )
         CALL CICE_Run
         CALL cice_sbc_out ( kt, nsbc )

         IF ( nsbc == 5 )  CALL cice_sbc_hadgam(kt+1)

      ENDIF                                          ! End sea-ice time step only
      !
      IF( nn_timing == 1 )  CALL timing_stop('sbc_ice_cice')

   END SUBROUTINE sbc_ice_cice

   SUBROUTINE cice_sbc_init (nsbc)
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE cice_sbc_init  ***
      !! ** Purpose: Initialise ice related fields for NEMO and coupling
      !!
      INTEGER, INTENT( in  ) ::   nsbc ! surface forcing type
      !!---------------------------------------------------------------------

      INTEGER  ::   ji, jj, jpl                        ! dummy loop indices

      IF( nn_timing == 1 )  CALL timing_start('cice_sbc_init')
      !
      IF(lwp) WRITE(numout,*)'cice_sbc_init'

! Initialize CICE
      CALL CICE_Initialize

! Do some CICE consistency checks
      IF ( (nsbc == 2) .OR. (nsbc == 5) ) THEN
         IF ( calc_strair .OR. calc_Tsfc ) THEN
            CALL ctl_stop( 'STOP', 'cice_sbc_init : Forcing option requires calc_strair=F and calc_Tsfc=F in ice_in' )
         ENDIF
      ELSEIF (nsbc == 4) THEN
         IF ( .NOT. (calc_strair .AND. calc_Tsfc) ) THEN
            CALL ctl_stop( 'STOP', 'cice_sbc_init : Forcing option requires calc_strair=T and calc_Tsfc=T in ice_in' )
         ENDIF
      ENDIF


! allocate sbc_ice and sbc_cice arrays
      IF( sbc_ice_alloc()      /= 0 )   CALL ctl_stop( 'STOP', 'sbc_ice_cice_alloc : unable to allocate arrays' )
      IF( sbc_ice_cice_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'sbc_ice_cice_alloc : unable to allocate cice arrays' )

! Ensure ocean temperatures are nowhere below freezing if not a NEMO restart
      IF( .NOT. ln_rstart ) THEN
         tsn(:,:,:,jp_tem) = MAX (tsn(:,:,:,jp_tem),Tocnfrz)
         tsb(:,:,:,jp_tem) = tsn(:,:,:,jp_tem)
      ENDIF

      fr_iu(:,:)=0.0
      fr_iv(:,:)=0.0

      CALL cice2nemo(aice,fr_i, 'T', 1. )
      IF ( (nsbc == 2).OR.(nsbc == 5) ) THEN
         DO jpl=1,ncat
            CALL cice2nemo(aicen(:,:,jpl,:),a_i(:,:,jpl), 'T', 1. )
         ENDDO
      ENDIF

! T point to U point
! T point to V point
      DO jj=1,jpjm1
         DO ji=1,jpim1
            fr_iu(ji,jj)=0.5*(fr_i(ji,jj)+fr_i(ji+1,jj))*umask(ji,jj,1)
            fr_iv(ji,jj)=0.5*(fr_i(ji,jj)+fr_i(ji,jj+1))*vmask(ji,jj,1)
         ENDDO
      ENDDO

      CALL lbc_lnk ( fr_iu , 'U', 1. )
      CALL lbc_lnk ( fr_iv , 'V', 1. )
      !
      IF( nn_timing == 1 )  CALL timing_stop('cice_sbc_init')
      !
   END SUBROUTINE cice_sbc_init

   
   SUBROUTINE cice_sbc_in (kt, nsbc)
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE cice_sbc_in  ***
      !! ** Purpose: Set coupling fields and pass to CICE
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in   ) ::   kt   ! ocean time step
      INTEGER, INTENT(in   ) ::   nsbc ! surface forcing type

      INTEGER  ::   ji, jj, jpl                   ! dummy loop indices      
      REAL(wp), DIMENSION(:,:), POINTER :: ztmp
      REAL(wp), DIMENSION(:,:,:), POINTER :: ztmpn
      !!---------------------------------------------------------------------

      IF( nn_timing == 1 )  CALL timing_start('cice_sbc_in')
      !
      CALL wrk_alloc( jpi,jpj, ztmp )
      CALL wrk_alloc( jpi,jpj,ncat, ztmpn )

      IF( kt == nit000 )  THEN
         IF(lwp) WRITE(numout,*)'cice_sbc_in'
      ENDIF

      ztmp(:,:)=0.0

! Aggregate ice concentration already set in cice_sbc_out (or cice_sbc_init on 
! the first time-step)

! forced and coupled case 

      IF ( (nsbc == 2).OR.(nsbc == 5) ) THEN

         ztmpn(:,:,:)=0.0

! x comp of wind stress (CI_1)
! U point to F point
         DO jj=1,jpjm1
            DO ji=1,jpi
               ztmp(ji,jj) = 0.5 * (  fr_iu(ji,jj) * utau(ji,jj)      &
                                    + fr_iu(ji,jj+1) * utau(ji,jj+1) ) * fmask(ji,jj,1)
            ENDDO
         ENDDO
         CALL nemo2cice(ztmp,strax,'F', -1. )

! y comp of wind stress (CI_2)
! V point to F point
         DO jj=1,jpj
            DO ji=1,jpim1
               ztmp(ji,jj) = 0.5 * (  fr_iv(ji,jj) * vtau(ji,jj)      &
                                    + fr_iv(ji+1,jj) * vtau(ji+1,jj) ) * fmask(ji,jj,1)
            ENDDO
         ENDDO
         CALL nemo2cice(ztmp,stray,'F', -1. )

! Surface downward latent heat flux (CI_5)
         IF (nsbc == 2) THEN
            DO jpl=1,ncat
               ztmpn(:,:,jpl)=qla_ice(:,:,1)*a_i(:,:,jpl)
            ENDDO
         ELSE
! emp_ice is set in sbc_cpl_ice_flx as sublimation-snow
            qla_ice(:,:,1)= - ( emp_ice(:,:)+sprecip(:,:) ) * Lsub
! End of temporary code
            DO jj=1,jpj
               DO ji=1,jpi
                  IF (fr_i(ji,jj).eq.0.0) THEN
                     DO jpl=1,ncat
                        ztmpn(ji,jj,jpl)=0.0
                     ENDDO
                     ! This will then be conserved in CICE
                     ztmpn(ji,jj,1)=qla_ice(ji,jj,1)
                  ELSE
                     DO jpl=1,ncat
                        ztmpn(ji,jj,jpl)=qla_ice(ji,jj,1)*a_i(ji,jj,jpl)/fr_i(ji,jj)
                     ENDDO
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
         DO jpl=1,ncat
            CALL nemo2cice(ztmpn(:,:,jpl),flatn_f(:,:,jpl,:),'T', 1. )

! GBM conductive flux through ice (CI_6)
!  Convert to GBM
            IF (nsbc == 2) THEN
               ztmp(:,:) = botmelt(:,:,jpl)*a_i(:,:,jpl)
            ELSE
               ztmp(:,:) = botmelt(:,:,jpl)
            ENDIF
            CALL nemo2cice(ztmp,fcondtopn_f(:,:,jpl,:),'T', 1. )

! GBM surface heat flux (CI_7)
!  Convert to GBM
            IF (nsbc == 2) THEN
               ztmp(:,:) = (topmelt(:,:,jpl)+botmelt(:,:,jpl))*a_i(:,:,jpl) 
            ELSE
               ztmp(:,:) = (topmelt(:,:,jpl)+botmelt(:,:,jpl))
            ENDIF
            CALL nemo2cice(ztmp,fsurfn_f(:,:,jpl,:),'T', 1. )
         ENDDO

      ELSE IF (nsbc == 4) THEN

! Pass CORE forcing fields to CICE (which will calculate heat fluxes etc itself)
! x comp and y comp of atmosphere surface wind (CICE expects on T points)
         ztmp(:,:) = wndi_ice(:,:)
         CALL nemo2cice(ztmp,uatm,'T', -1. )
         ztmp(:,:) = wndj_ice(:,:)
         CALL nemo2cice(ztmp,vatm,'T', -1. )
         ztmp(:,:) = SQRT ( wndi_ice(:,:)**2 + wndj_ice(:,:)**2 )
         CALL nemo2cice(ztmp,wind,'T', 1. )    ! Wind speed (m/s)
         ztmp(:,:) = qsr_ice(:,:,1)
         CALL nemo2cice(ztmp,fsw,'T', 1. )     ! Incoming short-wave (W/m^2)
         ztmp(:,:) = qlw_ice(:,:,1)
         CALL nemo2cice(ztmp,flw,'T', 1. )     ! Incoming long-wave (W/m^2)
         ztmp(:,:) = tatm_ice(:,:)
         CALL nemo2cice(ztmp,Tair,'T', 1. )    ! Air temperature (K)
         CALL nemo2cice(ztmp,potT,'T', 1. )    ! Potential temp (K)
! Following line uses MAX(....) to avoid problems if tatm_ice has unset halo rows  
         ztmp(:,:) = 101000. / ( 287.04 * MAX(1.0,tatm_ice(:,:)) )    
                                               ! Constant (101000.) atm pressure assumed
         CALL nemo2cice(ztmp,rhoa,'T', 1. )    ! Air density (kg/m^3)
         ztmp(:,:) = qatm_ice(:,:)
         CALL nemo2cice(ztmp,Qa,'T', 1. )      ! Specific humidity (kg/kg)
         ztmp(:,:)=10.0
         CALL nemo2cice(ztmp,zlvl,'T', 1. )    ! Atmos level height (m)

! May want to check all values are physically realistic (as in CICE routine 
! prepare_forcing)?

! Divide shortwave into spectral bands (as in prepare_forcing)
         ztmp(:,:)=qsr_ice(:,:,1)*frcvdr       ! visible direct
         CALL nemo2cice(ztmp,swvdr,'T', 1. )             
         ztmp(:,:)=qsr_ice(:,:,1)*frcvdf       ! visible diffuse
         CALL nemo2cice(ztmp,swvdf,'T', 1. )              
         ztmp(:,:)=qsr_ice(:,:,1)*frcidr       ! near IR direct
         CALL nemo2cice(ztmp,swidr,'T', 1. )
         ztmp(:,:)=qsr_ice(:,:,1)*frcidf       ! near IR diffuse
         CALL nemo2cice(ztmp,swidf,'T', 1. )

      ENDIF

! Snowfall
! Ensure fsnow is positive (as in CICE routine prepare_forcing)  
      ztmp(:,:)=MAX(fr_i(:,:)*sprecip(:,:),0.0)  
      CALL nemo2cice(ztmp,fsnow,'T', 1. ) 

! Rainfall
      ztmp(:,:)=fr_i(:,:)*(tprecip(:,:)-sprecip(:,:))
      CALL nemo2cice(ztmp,frain,'T', 1. ) 

! Freezing/melting potential
! Calculated over NEMO leapfrog timestep (hence 2*dt)
      nfrzmlt(:,:)=rau0*rcp*fse3t_m(:,:,1)*(Tocnfrz-sst_m(:,:))/(2.0*dt)

      ztmp(:,:) = nfrzmlt(:,:)
      CALL nemo2cice(ztmp,frzmlt,'T', 1. )

! SST  and SSS

      CALL nemo2cice(sst_m,sst,'T', 1. )
      CALL nemo2cice(sss_m,sss,'T', 1. )

! x comp and y comp of surface ocean current
! U point to F point
      DO jj=1,jpjm1
         DO ji=1,jpi
            ztmp(ji,jj)=0.5*(ssu_m(ji,jj)+ssu_m(ji,jj+1))*fmask(ji,jj,1)
         ENDDO
      ENDDO
      CALL nemo2cice(ztmp,uocn,'F', -1. )

! V point to F point
      DO jj=1,jpj
         DO ji=1,jpim1
            ztmp(ji,jj)=0.5*(ssv_m(ji,jj)+ssv_m(ji+1,jj))*fmask(ji,jj,1)
         ENDDO
      ENDDO
      CALL nemo2cice(ztmp,vocn,'F', -1. )

! x comp and y comp of sea surface slope (on F points)
! T point to F point
      DO jj=1,jpjm1
         DO ji=1,jpim1
            ztmp(ji,jj)=0.5 * (  (ssh_m(ji+1,jj  )-ssh_m(ji,jj  ))/e1u(ji,jj  )   &
                               + (ssh_m(ji+1,jj+1)-ssh_m(ji,jj+1))/e1u(ji,jj+1) ) & 
                            *  fmask(ji,jj,1)
         ENDDO
      ENDDO
      CALL nemo2cice(ztmp,ss_tltx,'F', -1. )

! T point to F point
      DO jj=1,jpjm1
         DO ji=1,jpim1
            ztmp(ji,jj)=0.5 * (  (ssh_m(ji  ,jj+1)-ssh_m(ji  ,jj))/e2v(ji  ,jj)   &
                               + (ssh_m(ji+1,jj+1)-ssh_m(ji+1,jj))/e2v(ji+1,jj) ) &
                            *  fmask(ji,jj,1)
         ENDDO
      ENDDO
      CALL nemo2cice(ztmp,ss_tlty,'F', -1. )

      CALL wrk_dealloc( jpi,jpj, ztmp )
      CALL wrk_dealloc( jpi,jpj,ncat, ztmpn )
      !
      IF( nn_timing == 1 )  CALL timing_stop('cice_sbc_in')
      !
   END SUBROUTINE cice_sbc_in


   SUBROUTINE cice_sbc_out (kt,nsbc)
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE cice_sbc_out  ***
      !! ** Purpose: Get fields from CICE and set surface fields for NEMO
      !!---------------------------------------------------------------------
      INTEGER, INTENT( in  ) ::   kt   ! ocean time step
      INTEGER, INTENT( in  ) ::   nsbc ! surface forcing type
      
      INTEGER  ::   ji, jj, jpl                 ! dummy loop indices
      REAL(wp), DIMENSION(:,:), POINTER :: ztmp
      !!---------------------------------------------------------------------

      IF( nn_timing == 1 )  CALL timing_start('cice_sbc_out')
      !
      CALL wrk_alloc( jpi,jpj, ztmp )
      
      IF( kt == nit000 )  THEN
         IF(lwp) WRITE(numout,*)'cice_sbc_out'
      ENDIF
      
! x comp of ocean-ice stress 
      CALL cice2nemo(strocnx,ztmp,'F', -1. )
      ss_iou(:,:)=0.0
! F point to U point
      DO jj=2,jpjm1
         DO ji=2,jpim1
            ss_iou(ji,jj) = 0.5 * ( ztmp(ji,jj-1) + ztmp(ji,jj) ) * umask(ji,jj,1)
         ENDDO
      ENDDO
      CALL lbc_lnk( ss_iou , 'U', -1. )

! y comp of ocean-ice stress 
      CALL cice2nemo(strocny,ztmp,'F', -1. )
      ss_iov(:,:)=0.0
! F point to V point

      DO jj=1,jpjm1
         DO ji=2,jpim1
            ss_iov(ji,jj) = 0.5 * ( ztmp(ji-1,jj) + ztmp(ji,jj) ) * vmask(ji,jj,1)
         ENDDO
      ENDDO
      CALL lbc_lnk( ss_iov , 'V', -1. )

! x and y comps of surface stress
! Combine wind stress and ocean-ice stress
! [Note that fr_iu hasn't yet been updated, so still from start of CICE timestep]

      utau(:,:)=(1.0-fr_iu(:,:))*utau(:,:)-ss_iou(:,:)
      vtau(:,:)=(1.0-fr_iv(:,:))*vtau(:,:)-ss_iov(:,:)     

! Freshwater fluxes 

      IF (nsbc == 2) THEN
! Note that emp from the forcing files is evap*(1-aice)-(tprecip-aice*sprecip)
! What we want here is evap*(1-aice)-tprecip*(1-aice) hence manipulation below
! Not ideal since aice won't be the same as in the atmosphere.  
! Better to use evap and tprecip? (but for now don't read in evap in this case)
         emp(:,:)  = emp(:,:)+fr_i(:,:)*(tprecip(:,:)-sprecip(:,:))
      ELSE IF (nsbc == 4) THEN
         emp(:,:)  = (1.0-fr_i(:,:))*emp(:,:)        
      ELSE IF (nsbc ==5) THEN
! emp_tot is set in sbc_cpl_ice_flx (call from cice_sbc_in above) 
         emp(:,:) = emp_tot(:,:)+tprecip(:,:)*fr_i(:,:) 
      ENDIF

! Subtract fluxes from CICE to get freshwater equivalent flux used in 
! salinity calculation
      CALL cice2nemo(fresh_gbm,ztmp,'T', 1. )
      emps(:,:)=emp(:,:)-ztmp(:,:)
! Note the 1000.0 is to convert from kg salt to g salt (needed for PSU)
      CALL cice2nemo(fsalt_gbm,ztmp,'T', 1. )
      DO jj=1,jpj
         DO ji=1,jpi
            IF (sss_m(ji,jj).gt.0.0) THEN
               emps(ji,jj)=emps(ji,jj)+ztmp(ji,jj)*1000.0/sss_m(ji,jj)
            ENDIF
         ENDDO
      ENDDO

! No longer remove precip over ice from free surface calculation on basis that the
! weight of the precip will affect the free surface even if it falls on the ice
! (same to the argument that freezing / melting of ice doesn't change the free surface) 
! Sublimation from the ice is treated in a similar way (included in emp but not emps)  
!
! This should not be done in the variable volume case

      IF (.NOT. lk_vvl) THEN

         emp(:,:)  = emp(:,:) - tprecip(:,:)*fr_i(:,:)

! Take sublimation into account
         IF (nsbc == 5 ) THEN 
            emp(:,:) = emp(:,:) + ( emp_ice(:,:) + sprecip(:,:) )
         ELSE IF (nsbc == 2 ) THEN
            emp(:,:) = emp(:,:) - qla_ice(:,:,1) / Lsub
         ENDIF

      ENDIF

      CALL lbc_lnk( emp , 'T', 1. )
      CALL lbc_lnk( emps , 'T', 1. )

! Solar penetrative radiation and non solar surface heat flux

! Scale qsr and qns according to ice fraction (bulk formulae only)

      IF (nsbc == 4) THEN
         qsr(:,:)=qsr(:,:)*(1.0-fr_i(:,:))
         qns(:,:)=qns(:,:)*(1.0-fr_i(:,:))
      ENDIF
! Take into account snow melting except for fully coupled when already in qns_tot
      IF (nsbc == 5) THEN
         qsr(:,:)= qsr_tot(:,:)
         qns(:,:)= qns_tot(:,:)
      ELSE
         qns(:,:)= qns(:,:)-sprecip(:,:)*Lfresh*(1.0-fr_i(:,:))
      ENDIF

! Now add in ice / snow related terms
! [fswthru will be zero unless running with calc_Tsfc=T in CICE]
      CALL cice2nemo(fswthru_gbm,ztmp,'T', 1. )
      qsr(:,:)=qsr(:,:)+ztmp(:,:)
      CALL lbc_lnk( qsr , 'T', 1. )

      DO jj=1,jpj
         DO ji=1,jpi
            nfrzmlt(ji,jj)=MAX(nfrzmlt(ji,jj),0.0)
         ENDDO
      ENDDO

      CALL cice2nemo(fhocn_gbm,ztmp,'T', 1. )
      qns(:,:)=qns(:,:)+nfrzmlt(:,:)+ztmp(:,:)

      CALL lbc_lnk( qns , 'T', 1. )

! Prepare for the following CICE time-step

      CALL cice2nemo(aice,fr_i,'T', 1. )
      IF ( (nsbc == 2).OR.(nsbc == 5) ) THEN
         DO jpl=1,ncat
            CALL cice2nemo(aicen(:,:,jpl,:),a_i(:,:,jpl), 'T', 1. )
         ENDDO
      ENDIF

! T point to U point
! T point to V point
      DO jj=1,jpjm1
         DO ji=1,jpim1
            fr_iu(ji,jj)=0.5*(fr_i(ji,jj)+fr_i(ji+1,jj))*umask(ji,jj,1)
            fr_iv(ji,jj)=0.5*(fr_i(ji,jj)+fr_i(ji,jj+1))*vmask(ji,jj,1)
         ENDDO
      ENDDO

      CALL lbc_lnk ( fr_iu , 'U', 1. )
      CALL lbc_lnk ( fr_iv , 'V', 1. )

! Release work space

      CALL wrk_dealloc( jpi,jpj, ztmp )
      !
      IF( nn_timing == 1 )  CALL timing_stop('cice_sbc_out')
      !
   END SUBROUTINE cice_sbc_out


#if defined key_oasis3 || defined key_oasis4
   SUBROUTINE cice_sbc_hadgam( kt )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE cice_sbc_hadgam  ***
      !! ** Purpose: Prepare fields needed to pass to HadGAM3 atmosphere
      !! 
      !!
      INTEGER, INTENT( in  ) ::   kt   ! ocean time step
      !!---------------------------------------------------------------------

      INTEGER  ::   jpl                        ! dummy loop index
      INTEGER  ::   ierror

      IF( nn_timing == 1 )  CALL timing_start('cice_sbc_hadgam')
      !
      IF( kt == nit000 )  THEN
         IF(lwp) WRITE(numout,*)'cice_sbc_hadgam'
         IF( sbc_cpl_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'sbc_cpl_alloc : unable to allocate arrays' )
      ENDIF

      !                                         ! =========================== !
      !                                         !   Prepare Coupling fields   !
      !                                         ! =========================== !

! x and y comp of ice velocity

      CALL cice2nemo(uvel,u_ice,'F', -1. )
      CALL cice2nemo(vvel,v_ice,'F', -1. )

! Ice concentration (CO_1) = a_i calculated at end of cice_sbc_out  

! Snow and ice thicknesses (CO_2 and CO_3)

      DO jpl = 1,ncat
         CALL cice2nemo(vsnon(:,:,jpl,:),ht_s(:,:,jpl),'T', 1. )
         CALL cice2nemo(vicen(:,:,jpl,:),ht_i(:,:,jpl),'T', 1. )
      ENDDO
      !
      IF( nn_timing == 1 )  CALL timing_stop('cice_sbc_hadgam')
      !
   END SUBROUTINE cice_sbc_hadgam

#else
   SUBROUTINE cice_sbc_hadgam( kt )    ! Dummy routine
      INTEGER, INTENT( in  ) ::   kt   ! ocean time step
      WRITE(*,*) 'cice_sbc_hadgam: You should not have seen this print! error?'
   END SUBROUTINE cice_sbc_hadgam
#endif

   SUBROUTINE cice_sbc_final
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE cice_sbc_final  ***
      !! ** Purpose: Finalize CICE
      !!---------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)'cice_sbc_final'

      CALL CICE_Finalize

   END SUBROUTINE cice_sbc_final

   SUBROUTINE cice_sbc_force (kt)
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE cice_sbc_force  ***
      !! ** Purpose : Provide CICE forcing from files
      !!
      !!---------------------------------------------------------------------
      !! ** Method  :   READ monthly flux file in NetCDF files
      !!      
      !!  snowfall    
      !!  rainfall    
      !!  sublimation rate    
      !!  topmelt (category)
      !!  botmelt (category)
      !!
      !! History :
      !!----------------------------------------------------------------------
      !! * Modules used
      USE iom

      !! * arguments
      INTEGER, INTENT( in  ) ::   kt ! ocean time step

      INTEGER  ::   ierror             ! return error code
      INTEGER  ::   ifpr               ! dummy loop index
      !!
      CHARACTER(len=100) ::  cn_dir                            !   Root directory for location of CICE forcing files
      TYPE(FLD_N), DIMENSION(jpfld) ::   slf_i                 ! array of namelist informations on the fields to read
      TYPE(FLD_N) ::   sn_snow, sn_rain, sn_sblm               ! informations about the fields to be read
      TYPE(FLD_N) ::   sn_top1, sn_top2, sn_top3, sn_top4, sn_top5
      TYPE(FLD_N) ::   sn_bot1, sn_bot2, sn_bot3, sn_bot4, sn_bot5 

      !!
      NAMELIST/namsbc_cice/ cn_dir, sn_snow, sn_rain, sn_sblm,   &
         &                          sn_top1, sn_top2, sn_top3, sn_top4, sn_top5,  &
         &                          sn_bot1, sn_bot2, sn_bot3, sn_bot4, sn_bot5
      !!---------------------------------------------------------------------

      !                                         ! ====================== !
      IF( kt == nit000 ) THEN                   !  First call kt=nit000  !
         !                                      ! ====================== !
         ! set file information (default values)
         cn_dir = './'       ! directory in which the model is executed

         ! (NB: frequency positive => hours, negative => months)
         !            !    file          ! frequency !  variable    ! time intep !  clim   ! 'yearly' or ! weights  ! rotation   !
         !            !    name          !  (hours)  !   name       !   (T/F)    !  (T/F)  !  'monthly'  ! filename ! pairs      !
         sn_snow = FLD_N( 'snowfall_1m'  ,    -1.    ,  'snowfall'  ,  .true.    , .true.  ,  ' yearly'  , ''       , ''         ) 
         sn_rain = FLD_N( 'rainfall_1m'  ,    -1.    ,  'rainfall'  ,  .true.    , .true.  ,  ' yearly'  , ''       , ''         ) 
         sn_sblm = FLD_N( 'sublim_1m'    ,    -1.    ,  'sublim'    ,  .true.    , .true.  ,  ' yearly'  , ''       , ''         )
         sn_top1 = FLD_N( 'topmeltn1_1m' ,    -1.    ,  'topmeltn1' ,  .true.    , .true.  ,  ' yearly'  , ''       , ''         )
         sn_top2 = FLD_N( 'topmeltn2_1m' ,    -1.    ,  'topmeltn2' ,  .true.    , .true.  ,  ' yearly'  , ''       , ''         )
         sn_top3 = FLD_N( 'topmeltn3_1m' ,    -1.    ,  'topmeltn3' ,  .true.    , .true.  ,  ' yearly'  , ''       , ''         )
         sn_top4 = FLD_N( 'topmeltn4_1m' ,    -1.    ,  'topmeltn4' ,  .true.    , .true.  ,  ' yearly'  , ''       , ''         )
         sn_top5 = FLD_N( 'topmeltn5_1m' ,    -1.    ,  'topmeltn5' ,  .true.    , .true.  ,  ' yearly'  , ''       , ''         )
         sn_bot1 = FLD_N( 'botmeltn1_1m' ,    -1.    ,  'botmeltn1' ,  .true.    , .true.  ,  ' yearly'  , ''       , ''         )
         sn_bot2 = FLD_N( 'botmeltn2_1m' ,    -1.    ,  'botmeltn2' ,  .true.    , .true.  ,  ' yearly'  , ''       , ''         )
         sn_bot3 = FLD_N( 'botmeltn3_1m' ,    -1.    ,  'botmeltn3' ,  .true.    , .true.  ,  ' yearly'  , ''       , ''         )
         sn_bot4 = FLD_N( 'botmeltn4_1m' ,    -1.    ,  'botmeltn4' ,  .true.    , .true.  ,  ' yearly'  , ''       , ''         )
         sn_bot5 = FLD_N( 'botmeltn5_1m' ,    -1.    ,  'botmeltn5' ,  .true.    , .true.  ,  ' yearly'  , ''       , ''         )

!         REWIND ( numnam )               ! ... at some point might read in from NEMO namelist?
!         READ   ( numnam, namsbc_cice ) 

         ! store namelist information in an array
         slf_i(jp_snow) = sn_snow   ;   slf_i(jp_rain) = sn_rain   ;   slf_i(jp_sblm) = sn_sblm
         slf_i(jp_top1) = sn_top1   ;   slf_i(jp_top2) = sn_top2   ;   slf_i(jp_top3) = sn_top3
         slf_i(jp_top4) = sn_top4   ;   slf_i(jp_top5) = sn_top5   ;   slf_i(jp_bot1) = sn_bot1
         slf_i(jp_bot2) = sn_bot2   ;   slf_i(jp_bot3) = sn_bot3   ;   slf_i(jp_bot4) = sn_bot4
         slf_i(jp_bot5) = sn_bot5
         
         ! set sf structure
         ALLOCATE( sf(jpfld), STAT=ierror )
         IF( ierror > 0 ) THEN
            CALL ctl_stop( 'cice_sbc_force: unable to allocate sf structure' )   ;   RETURN
         ENDIF

         DO ifpr= 1, jpfld
            ALLOCATE( sf(ifpr)%fnow(jpi,jpj,1) )
            ALLOCATE( sf(ifpr)%fdta(jpi,jpj,1,2) )
         END DO

         ! fill sf with slf_i and control print
         CALL fld_fill( sf, slf_i, cn_dir, 'cice_sbc_force', 'flux formulation for CICE', 'namsbc_cice' )
         !
      ENDIF

      CALL fld_read( kt, nn_fsbc, sf )           ! Read input fields and provides the
      !                                          ! input fields at the current time-step

      ! set the fluxes from read fields
      sprecip(:,:) = sf(jp_snow)%fnow(:,:,1)
      tprecip(:,:) = sf(jp_snow)%fnow(:,:,1)+sf(jp_rain)%fnow(:,:,1)
! May be better to do this conversion somewhere else
      qla_ice(:,:,1) = -Lsub*sf(jp_sblm)%fnow(:,:,1)
      topmelt(:,:,1) = sf(jp_top1)%fnow(:,:,1)
      topmelt(:,:,2) = sf(jp_top2)%fnow(:,:,1)
      topmelt(:,:,3) = sf(jp_top3)%fnow(:,:,1)
      topmelt(:,:,4) = sf(jp_top4)%fnow(:,:,1)
      topmelt(:,:,5) = sf(jp_top5)%fnow(:,:,1)
      botmelt(:,:,1) = sf(jp_bot1)%fnow(:,:,1)
      botmelt(:,:,2) = sf(jp_bot2)%fnow(:,:,1)
      botmelt(:,:,3) = sf(jp_bot3)%fnow(:,:,1)
      botmelt(:,:,4) = sf(jp_bot4)%fnow(:,:,1)
      botmelt(:,:,5) = sf(jp_bot5)%fnow(:,:,1)

      ! control print (if less than 100 time-step asked)
      IF( nitend-nit000 <= 100 .AND. lwp ) THEN
         WRITE(numout,*) 
         WRITE(numout,*) '        read forcing fluxes for CICE OK'
         CALL FLUSH(numout)
      ENDIF

   END SUBROUTINE cice_sbc_force

   SUBROUTINE nemo2cice( pn, pc, cd_type, psgn)
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE nemo2cice  ***
      !! ** Purpose :   Transfer field in NEMO array to field in CICE array.  
#if defined key_nemocice_decomp
      !!             
      !!                NEMO and CICE PE sub domains are identical, hence
      !!                there is no need to gather or scatter data from 
      !!                one PE configuration to another.
#else
      !!                Automatically gather/scatter between 
      !!                different processors and blocks
      !! ** Method :    A. Ensure all haloes are filled in NEMO field (pn)
      !!                B. Gather pn into global array (png)
      !!                C. Map png into CICE global array (pcg)
      !!                D. Scatter pcg to CICE blocks (pc) + update haloes  
#endif
      !!---------------------------------------------------------------------

      CHARACTER(len=1), INTENT( in ) ::   &
          cd_type       ! nature of pn grid-point
          !             !   = T or F gridpoints
      REAL(wp), INTENT( in ) ::   &
          psgn          ! control of the sign change
          !             !   =-1 , the sign is modified following the type of b.c. used
          !             !   = 1 , no sign change
      REAL(wp), DIMENSION(jpi,jpj) :: pn
#if !defined key_nemocice_decomp
      REAL (kind=dbl_kind), dimension(nx_global,ny_global) :: pcg
#endif
      REAL (kind=dbl_kind), dimension(nx_block,ny_block,max_blocks) :: pc
      INTEGER (int_kind) :: &
         field_type,        &! id for type of field (scalar, vector, angle)
         grid_loc            ! id for location on horizontal grid
                            !  (center, NEcorner, Nface, Eface)

      INTEGER  ::   ji, jj, jn                      ! dummy loop indices

!     A. Ensure all haloes are filled in NEMO field (pn)

      CALL lbc_lnk( pn , cd_type, psgn )

#if defined key_nemocice_decomp

      ! Copy local domain data from NEMO to CICE field
      pc(:,:,1)=0.0
      DO jj=2,ny_block
         DO ji=2,nx_block
            pc(ji,jj,1)=pn(ji,jj-1)
         ENDDO
      ENDDO

#else

!     B. Gather pn into global array (png)

      IF ( jpnij > 1) THEN
         CALL mppsync
         CALL mppgather (pn,0,png) 
         CALL mppsync
      ELSE
         png(:,:,1)=pn(:,:)
      ENDIF

!     C. Map png into CICE global array (pcg)

! Need to make sure this is robust to changes in NEMO halo rows....
! (may be OK but not 100% sure)

      IF (nproc==0) THEN     
!        pcg(:,:)=0.0
         DO jn=1,jpnij
            DO jj=1,nlcjt(jn)-1
               DO ji=2,nlcit(jn)-1
                  pcg(ji+nimppt(jn)-2,jj+njmppt(jn)-1)=png(ji,jj,jn)       
               ENDDO
            ENDDO
         ENDDO
      ENDIF

#endif

      SELECT CASE ( cd_type )
         CASE ( 'T' )
            grid_loc=field_loc_center
         CASE ( 'F' )                              
            grid_loc=field_loc_NEcorner
      END SELECT

      SELECT CASE ( NINT(psgn) )
         CASE ( -1 )
            field_type=field_type_vector
         CASE ( 1 )                              
            field_type=field_type_scalar
      END SELECT

#if defined key_nemocice_decomp
      ! Ensure CICE halos are up to date
      CALL ice_HaloUpdate (pc, halo_info, grid_loc, field_type)
#else
!     D. Scatter pcg to CICE blocks (pc) + update halos
      CALL scatter_global(pc, pcg, 0, distrb_info, grid_loc, field_type)
#endif

   END SUBROUTINE nemo2cice

   SUBROUTINE cice2nemo ( pc, pn, cd_type, psgn )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE cice2nemo  ***
      !! ** Purpose :   Transfer field in CICE array to field in NEMO array.
#if defined key_nemocice_decomp
      !!             
      !!                NEMO and CICE PE sub domains are identical, hence
      !!                there is no need to gather or scatter data from 
      !!                one PE configuration to another.
#else 
      !!                Automatically deal with scatter/gather between
      !!                different processors and blocks
      !! ** Method :    A. Gather CICE blocks (pc) into global array (pcg)
      !!                B. Map pcg into NEMO global array (png)
      !!                C. Scatter png into NEMO field (pn) for each processor
      !!                D. Ensure all haloes are filled in pn 
#endif
      !!---------------------------------------------------------------------

      CHARACTER(len=1), INTENT( in ) ::   &
          cd_type       ! nature of pn grid-point
          !             !   = T or F gridpoints
      REAL(wp), INTENT( in ) ::   &
          psgn          ! control of the sign change
          !             !   =-1 , the sign is modified following the type of b.c. used
          !             !   = 1 , no sign change
      REAL(wp), DIMENSION(jpi,jpj) :: pn

#if defined key_nemocice_decomp
      INTEGER (int_kind) :: &
         field_type,        & ! id for type of field (scalar, vector, angle)
         grid_loc             ! id for location on horizontal grid
                              ! (center, NEcorner, Nface, Eface)
#else
      REAL (kind=dbl_kind), dimension(nx_global,ny_global) :: pcg
#endif

      REAL (kind=dbl_kind), dimension(nx_block,ny_block,max_blocks) :: pc

      INTEGER  ::   ji, jj, jn                      ! dummy loop indices


#if defined key_nemocice_decomp

      SELECT CASE ( cd_type )
         CASE ( 'T' )
            grid_loc=field_loc_center
         CASE ( 'F' )                              
            grid_loc=field_loc_NEcorner
      END SELECT

      SELECT CASE ( NINT(psgn) )
         CASE ( -1 )
            field_type=field_type_vector
         CASE ( 1 )                              
            field_type=field_type_scalar
      END SELECT

      CALL ice_HaloUpdate (pc, halo_info, grid_loc, field_type)


      pn(:,:)=0.0
      DO jj=1,jpjm1
         DO ji=1,jpim1
            pn(ji,jj)=pc(ji,jj+1,1)
         ENDDO
      ENDDO

#else

!      A. Gather CICE blocks (pc) into global array (pcg) 

      CALL gather_global(pcg, pc, 0, distrb_info)

!     B. Map pcg into NEMO global array (png)

! Need to make sure this is robust to changes in NEMO halo rows....
! (may be OK but not spent much time thinking about it)

      IF (nproc==0) THEN
         png(:,:,:)=0.0
         DO jn=1,jpnij
            DO jj=1,nlcjt(jn)-1
               DO ji=2,nlcit(jn)-1
                  png(ji,jj,jn)=pcg(ji+nimppt(jn)-2,jj+njmppt(jn)-1)      
               ENDDO
            ENDDO
         ENDDO
      ENDIF

!     C. Scatter png into NEMO field (pn) for each processor

      IF ( jpnij > 1) THEN
         CALL mppsync
         CALL mppscatter (png,0,pn) 
         CALL mppsync
      ELSE
         pn(:,:)=png(:,:,1)
      ENDIF

#endif

!     D. Ensure all haloes are filled in pn

      CALL lbc_lnk( pn , cd_type, psgn )

   END SUBROUTINE cice2nemo

#else
   !!----------------------------------------------------------------------
   !!   Default option           Dummy module         NO CICE sea-ice model
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_ice_cice ( kt, nsbc )     ! Dummy routine
      WRITE(*,*) 'sbc_ice_cice: You should not have seen this print! error?', kt
   END SUBROUTINE sbc_ice_cice

   SUBROUTINE cice_sbc_init (nsbc)    ! Dummy routine
      WRITE(*,*) 'cice_sbc_init: You should not have seen this print! error?'
   END SUBROUTINE cice_sbc_init

   SUBROUTINE cice_sbc_final     ! Dummy routine
      WRITE(*,*) 'cice_sbc_final: You should not have seen this print! error?'
   END SUBROUTINE cice_sbc_final

#endif

   !!======================================================================
END MODULE sbcice_cice
