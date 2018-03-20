MODULE trasbc
   !!==============================================================================
   !!                       ***  MODULE  trasbc  ***
   !! Ocean active tracers:  surface boundary condition
   !!==============================================================================
   !! History :  OPA  !  1998-10  (G. Madec, G. Roullet, M. Imbard)  Original code
   !!            8.2  !  2001-02  (D. Ludicone)  sea ice and free surface
   !!  NEMO      1.0  !  2002-06  (G. Madec)  F90: Free form and module
   !!            3.3  !  2010-04  (M. Leclair, G. Madec)  Forcing averaged over 2 time steps
   !!             -   !  2010-09  (C. Ethe, G. Madec) Merge TRA-TRC
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_sbc      : update the tracer trend at ocean surface
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and active tracers
   USE sbc_oce         ! surface boundary condition: ocean
   USE dom_oce         ! ocean space domain variables
   USE phycst          ! physical constant
   USE traqsr          ! solar radiation penetration
   USE trdmod_oce      ! ocean trends 
   USE trdtra          ! ocean trends
   USE in_out_manager  ! I/O manager
   USE prtctl          ! Print control
   USE sbcrnf          ! River runoff  
   USE sbcmod          ! ln_rnf  
   USE iom
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_sbc    ! routine called by step.F90

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: trasbc.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_sbc ( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_sbc  ***
      !!                   
      !! ** Purpose :   Compute the tracer surface boundary condition trend of
      !!      (flux through the interface, concentration/dilution effect)
      !!      and add it to the general trend of tracer equations.
      !!
      !! ** Method :
      !!      Following Roullet and Madec (2000), the air-sea flux can be divided 
      !!      into three effects: (1) Fext, external forcing; 
      !!      (2) Fwi, concentration/dilution effect due to water exchanged 
      !!         at the surface by evaporation, precipitations and runoff (E-P-R); 
      !!      (3) Fwe, tracer carried with the water that is exchanged. 
      !!
      !!      Fext, flux through the air-sea interface for temperature and salt: 
      !!            - temperature : heat flux q (w/m2). If penetrative solar
      !!         radiation q is only the non solar part of the heat flux, the
      !!         solar part is added in traqsr.F routine.
      !!            ta = ta + q /(rau0 rcp e3t)  for k=1
      !!            - salinity    : no salt flux
      !!
      !!      The formulation for Fwb and Fwi vary according to the free 
      !!      surface formulation (linear or variable volume). 
      !!      * Linear free surface
      !!            The surface freshwater flux modifies the ocean volume
      !!         and thus the concentration of a tracer and the temperature.
      !!         First order of the effect of surface freshwater exchange 
      !!         for salinity, it can be neglected on temperature (especially
      !!         as the temperature of precipitations and runoffs is usually
      !!         unknown).
      !!            - temperature : we assume that the temperature of both
      !!         precipitations and runoffs is equal to the SST, thus there
      !!         is no additional flux since in this case, the concentration
      !!         dilution effect is balanced by the net heat flux associated
      !!         to the freshwater exchange (Fwe+Fwi=0):
      !!            (Tp P - Te E) + SST (P-E) = 0 when Tp=Te=SST
      !!            - salinity    : evaporation, precipitation and runoff
      !!         water has a zero salinity (Fwe=0), thus only Fwi remains:
      !!            sa = sa + emp * sn / e3t   for k=1
      !!         where emp, the surface freshwater budget (evaporation minus
      !!         precipitation minus runoff) given in kg/m2/s is divided
      !!         by 1035 kg/m3 (density of ocena water) to obtain m/s.    
      !!         Note: even though Fwe does not appear explicitly for 
      !!         temperature in this routine, the heat carried by the water
      !!         exchanged through the surface is part of the total heat flux
      !!         forcing and must be taken into account in the global heat
      !!         balance).
      !!      * nonlinear free surface (variable volume, lk_vvl)
      !!         contrary to the linear free surface case, Fwi is properly 
      !!         taken into account by using the true layer thicknesses to       
      !!         calculate tracer content and advection. There is no need to 
      !!         deal with it in this routine.
      !!           - temperature: Fwe=SST (P-E+R) is added to Fext.
      !!           - salinity:  Fwe = 0, there is no surface flux of salt.
      !!
      !! ** Action  : - Update the 1st level of (ta,sa) with the trend associated
      !!                with the tracer surface boundary condition 
      !!              - save the trend it in ttrd ('key_trdtra')
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !!
      INTEGER  ::   ji, jj, jk, jn           ! dummy loop indices  
      REAL(wp) ::   zfact, z1_e3t, zsrau, zdep
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  ztrdt, ztrds
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('tra_sbc')
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'tra_sbc : TRAcer Surface Boundary Condition'
         IF(lwp) WRITE(numout,*) '~~~~~~~ '
      ENDIF

      zsrau = 1. / rau0             ! initialization

      IF( l_trdtra )   THEN                    !* Save ta and sa trends
         CALL wrk_alloc( jpi, jpj, jpk, ztrdt, ztrds ) 
         ztrdt(:,:,:) = tsa(:,:,:,jp_tem)
         ztrds(:,:,:) = tsa(:,:,:,jp_sal)
      ENDIF

!!gm      IF( .NOT.ln_traqsr )   qsr(:,:) = 0.e0   ! no solar radiation penetration
      IF( .NOT.ln_traqsr ) THEN     ! no solar radiation penetration
         qns(:,:) = qns(:,:) + qsr(:,:)      ! total heat flux in qns
         qsr(:,:) = 0.e0                     ! qsr set to zero
      ENDIF

      !----------------------------------------
      !        EMP, EMPS and QNS effects
      !----------------------------------------
      !                                          Set before sbc tracer content fields
      !                                          ************************************
      IF( kt == nit000 ) THEN                      ! Set the forcing field at nit000 - 1
         !                                         ! -----------------------------------
         IF( ln_rstart .AND.    &                     ! Restart: read in restart file
              & iom_varid( numror, 'sbc_hc_b', ldstop = .FALSE. ) > 0 ) THEN
            IF(lwp) WRITE(numout,*) '          nit000-1 surface tracer content forcing fields red in the restart file'
            zfact = 0.5e0
            CALL iom_get( numror, jpdom_autoglo, 'sbc_hc_b', sbc_tsc_b(:,:,jp_tem) )   ! before heat content sbc trend
            CALL iom_get( numror, jpdom_autoglo, 'sbc_sc_b', sbc_tsc_b(:,:,jp_sal) )   ! before salt content sbc trend
         ELSE                                         ! No restart or restart not found: Euler forward time stepping
            zfact = 1.e0
            sbc_tsc_b(:,:,:) = 0.e0
         ENDIF
      ELSE                                         ! Swap of forcing fields
         !                                         ! ----------------------
         zfact = 0.5e0
         sbc_tsc_b(:,:,:) = sbc_tsc(:,:,:)
      ENDIF
      !                                          Compute now sbc tracer content fields
      !                                          *************************************

                                                   ! Concentration dilution effect on (t,s) due to  
                                                   ! evaporation, precipitation and qns, but not river runoff 
                                               
      IF( lk_vvl ) THEN                            ! Variable Volume case
         DO jj = 1, jpj
            DO ji = 1, jpi 
               ! temperature : heat flux + cooling/heating effet of EMP flux
               sbc_tsc(ji,jj,jp_tem) = ro0cpr * qns(ji,jj) - zsrau * emp(ji,jj) * tsn(ji,jj,1,jp_tem)
               ! concent./dilut. effect due to sea-ice melt/formation and (possibly) SSS restoration
               sbc_tsc(ji,jj,jp_sal) = ( emps(ji,jj) - emp(ji,jj) ) * zsrau * tsn(ji,jj,1,jp_sal)
            END DO
         END DO
      ELSE                                         ! Constant Volume case
         DO jj = 2, jpj
            DO ji = fs_2, fs_jpim1   ! vector opt.
               ! temperature : heat flux
               sbc_tsc(ji,jj,jp_tem) = ro0cpr * qns(ji,jj)
               ! salinity    : salt flux + concent./dilut. effect (both in emps)
               sbc_tsc(ji,jj,jp_sal) = zsrau * emps(ji,jj) * tsn(ji,jj,1,jp_sal)
            END DO
         END DO
      ENDIF
      ! Concentration dilution effect on (t,s) due to evapouration, precipitation and qns, but not river runoff  
      DO jn = 1, jpts
         DO jj = 2, jpj
            DO ji = fs_2, fs_jpim1   ! vector opt.
               z1_e3t = zfact / fse3t(ji,jj,1)
               tsa(ji,jj,1,jn) = tsa(ji,jj,1,jn) + ( sbc_tsc_b(ji,jj,jn) + sbc_tsc(ji,jj,jn) ) * z1_e3t
            END DO
         END DO
      END DO
      !                                          Write in the ocean restart file
      !                                          *******************************
      IF( lrst_oce ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'sbc : ocean surface tracer content forcing fields written in ocean restart file ',   &
            &                    'at it= ', kt,' date= ', ndastp
         IF(lwp) WRITE(numout,*) '~~~~'
         CALL iom_rstput( kt, nitrst, numrow, 'sbc_hc_b', sbc_tsc(:,:,jp_tem) )
         CALL iom_rstput( kt, nitrst, numrow, 'sbc_sc_b', sbc_tsc(:,:,jp_sal) )
      ENDIF
      !
      !----------------------------------------
      !        River Runoff effects
      !----------------------------------------
      !
      IF( ln_rnf ) THEN         ! input of heat and salt due to river runoff 
         zfact = 0.5_wp
         DO jj = 2, jpj 
            DO ji = fs_2, fs_jpim1
               IF( rnf(ji,jj) /= 0._wp ) THEN
                  zdep = zfact / h_rnf(ji,jj)
                  DO jk = 1, nk_rnf(ji,jj)
                                        tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem)   &
                                          &               +  ( rnf_tsc_b(ji,jj,jp_tem) + rnf_tsc(ji,jj,jp_tem) ) * zdep
                     IF( ln_rnf_sal )   tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal)   &
                                          &               +  ( rnf_tsc_b(ji,jj,jp_sal) + rnf_tsc(ji,jj,jp_sal) ) * zdep 
                  END DO
               ENDIF
            END DO  
         END DO  
      ENDIF
 
      IF( l_trdtra )   THEN                      ! save the horizontal diffusive trends for further diagnostics
         ztrdt(:,:,:) = tsa(:,:,:,jp_tem) - ztrdt(:,:,:)
         ztrds(:,:,:) = tsa(:,:,:,jp_sal) - ztrds(:,:,:)
         CALL trd_tra( kt, 'TRA', jp_tem, jptra_trd_nsr, ztrdt )
         CALL trd_tra( kt, 'TRA', jp_sal, jptra_trd_nsr, ztrds )
         CALL wrk_dealloc( jpi, jpj, jpk, ztrdt, ztrds ) 
      ENDIF
      !
      IF(ln_ctl)   CALL prt_ctl( tab3d_1=tsa(:,:,:,jp_tem), clinfo1=' sbc  - Ta: ', mask1=tmask,   &
         &                       tab3d_2=tsa(:,:,:,jp_sal), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
      !
      IF( nn_timing == 1 )  CALL timing_stop('tra_sbc')
      !
   END SUBROUTINE tra_sbc

   !!======================================================================
END MODULE trasbc
