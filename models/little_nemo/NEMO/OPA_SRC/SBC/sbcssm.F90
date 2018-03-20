MODULE sbcssm
   !!======================================================================
   !!                       ***  MODULE  sbcssm  ***
   !! Surface module :  provide time-mean ocean surface variables
   !!======================================================================
   !! History :  9.0  ! 2006-07  (G. Madec)  Original code
   !!            3.3  ! 2010-10  (C. Bricaud, G. Madec)  add the Patm forcing for sea-ice
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_ssm        : calculate sea surface mean currents, temperature,  
   !!                    and salinity over nn_fsbc time-step
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE sbc_oce         ! surface boundary condition: ocean fields
   USE sbcapr          ! surface boundary condition: atmospheric pressure
   USE prtctl          ! Print control                    (prt_ctl routine)
   USE iom
   USE in_out_manager  ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_ssm    ! routine called by step.F90
   
   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: sbcssm.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_ssm( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE sbc_oce  ***
      !!                     
      !! ** Purpose :   provide ocean surface variable to sea-surface boundary
      !!                condition computation 
      !!                
      !! ** Method  :   compute mean surface velocity (2 components at U and 
      !!      V-points) [m/s], temperature [Celcius] and salinity [psu] over
      !!      the periode (kt - nn_fsbc) to kt
      !!         Note that the inverse barometer ssh (i.e. ssh associated with Patm)
      !!      is add to ssh_m when ln_apr_dyn = T. Required for sea-ice dynamics.
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      !
      REAL(wp) ::   zcoef, zf_sbc       ! local scalar
      !!---------------------------------------------------------------------
      !                                                   ! ---------------------------------------- !
      IF( nn_fsbc == 1 ) THEN                             !      Instantaneous surface fields        !
         !                                                ! ---------------------------------------- !
         IF( kt == nit000 ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'sbc_ssm : sea surface mean fields, nn_fsbc=1 : instantaneous values'
            IF(lwp) WRITE(numout,*) '~~~~~~~ '
         ENDIF
         !
         ssu_m(:,:) = ub(:,:,1)
         ssv_m(:,:) = vb(:,:,1)
         sst_m(:,:) = tsn(:,:,1,jp_tem)
         sss_m(:,:) = tsn(:,:,1,jp_sal)
         !                          ! removed inverse barometer ssh when Patm forcing is used (for sea-ice dynamics)
         IF( ln_apr_dyn ) THEN   ;   ssh_m(:,:) = sshn(:,:) - 0.5 * ( ssh_ib(:,:) + ssh_ibb(:,:) )
         ELSE                    ;   ssh_m(:,:) = sshn(:,:)
         ENDIF

         !
      ELSE
         !                                                ! ---------------------------------------- !
         IF( kt == nit000) THEN                           !       Initialisation: 1st time-step      !
            !                                             ! ---------------------------------------- !
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'sbc_ssm : sea surface mean fields'
            !
            IF( ln_rstart .AND. iom_varid( numror, 'nn_fsbc', ldstop = .FALSE. ) > 0 ) THEN 
               CALL iom_get( numror               , 'nn_fsbc', zf_sbc )   ! sbc frequency of previous run
               CALL iom_get( numror, jpdom_autoglo, 'ssu_m'  , ssu_m  )   ! sea surface mean velocity    (T-point)
               CALL iom_get( numror, jpdom_autoglo, 'ssv_m'  , ssv_m  )   !   "         "    velocity    (V-point)
               CALL iom_get( numror, jpdom_autoglo, 'sst_m'  , sst_m  )   !   "         "    temperature (T-point)
               CALL iom_get( numror, jpdom_autoglo, 'sss_m'  , sss_m  )   !   "         "    salinity    (T-point)
               CALL iom_get( numror, jpdom_autoglo, 'ssh_m'  , ssh_m  )   !   "         "    height      (T-point)
               !
               IF( zf_sbc /= REAL( nn_fsbc, wp ) ) THEN      ! nn_fsbc has changed between 2 runs
                  IF(lwp) WRITE(numout,*) '~~~~~~~   restart with a change in the frequency of mean ',   &
                     &                    'from ', zf_sbc, ' to ', nn_fsbc
                  zcoef = REAL( nn_fsbc - 1, wp ) / zf_sbc
                  ssu_m(:,:) = zcoef * ssu_m(:,:)
                  ssv_m(:,:) = zcoef * ssv_m(:,:)
                  sst_m(:,:) = zcoef * sst_m(:,:)
                  sss_m(:,:) = zcoef * sss_m(:,:)
                  ssh_m(:,:) = zcoef * ssh_m(:,:)
               ELSE
                  IF(lwp) WRITE(numout,*) '~~~~~~~   mean fields read in the ocean restart file'
               ENDIF
            ELSE
               IF(lwp) WRITE(numout,*) '~~~~~~~   mean fields initialised to instantaneous values'
               zcoef = REAL( nn_fsbc - 1, wp )
               ssu_m(:,:) = zcoef * ub(:,:,1)
               ssv_m(:,:) = zcoef * vb(:,:,1)
               sst_m(:,:) = zcoef * tsn(:,:,1,jp_tem)
               sss_m(:,:) = zcoef * tsn(:,:,1,jp_sal)
               !                          ! removed inverse barometer ssh when Patm forcing is used 
               IF( ln_apr_dyn ) THEN   ;   ssh_m(:,:) = zcoef * ( sshn(:,:) - 0.5 * ( ssh_ib(:,:) + ssh_ibb(:,:) ) )
               ELSE                    ;   ssh_m(:,:) = zcoef *   sshn(:,:)
               ENDIF

            ENDIF
            !                                             ! ---------------------------------------- !
         ELSEIF( MOD( kt - 2 , nn_fsbc ) == 0 ) THEN      !   Initialisation: New mean computation   !
            !                                             ! ---------------------------------------- !
            ssu_m(:,:) = 0.e0      ! reset to zero ocean mean sbc fields
            ssv_m(:,:) = 0.e0
            sst_m(:,:) = 0.e0
            sss_m(:,:) = 0.e0
            ssh_m(:,:) = 0.e0
         ENDIF
         !                                                ! ---------------------------------------- !
         !                                                !        Cumulate at each time step        !
         !                                                ! ---------------------------------------- !
         ssu_m(:,:) = ssu_m(:,:) + ub(:,:,1)
         ssv_m(:,:) = ssv_m(:,:) + vb(:,:,1)
         sst_m(:,:) = sst_m(:,:) + tsn(:,:,1,jp_tem)
         sss_m(:,:) = sss_m(:,:) + tsn(:,:,1,jp_sal)
         !                          ! removed inverse barometer ssh when Patm forcing is used (for sea-ice dynamics)
         IF( ln_apr_dyn ) THEN   ;   ssh_m(:,:) = ssh_m(:,:) + sshn(:,:) - 0.5 *  ( ssh_ib(:,:) + ssh_ibb(:,:) )
         ELSE                    ;   ssh_m(:,:) = ssh_m(:,:) + sshn(:,:)
         ENDIF

         !                                                ! ---------------------------------------- !
         IF( MOD( kt - 1 , nn_fsbc ) == 0 ) THEN          !   Mean value at each nn_fsbc time-step   !
            !                                             ! ---------------------------------------- !
            zcoef = 1. / REAL( nn_fsbc, wp )
            sst_m(:,:) = sst_m(:,:) * zcoef           ! mean SST             [Celcius]
            sss_m(:,:) = sss_m(:,:) * zcoef           ! mean SSS             [psu]
            ssu_m(:,:) = ssu_m(:,:) * zcoef           ! mean suface current  [m/s]
            ssv_m(:,:) = ssv_m(:,:) * zcoef           !
            ssh_m(:,:) = ssh_m(:,:) * zcoef           ! mean SSH             [m]
            !
         ENDIF
         !                                                ! ---------------------------------------- !
         IF( lrst_oce ) THEN                              !      Write in the ocean restart file     !
            !                                             ! ---------------------------------------- !
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'sbc_ssm : sea surface mean fields written in ocean restart file ',   &
               &                    'at it= ', kt,' date= ', ndastp
            IF(lwp) WRITE(numout,*) '~~~~~~~'
            zf_sbc = REAL( nn_fsbc, wp )
            CALL iom_rstput( kt, nitrst, numrow, 'nn_fsbc', zf_sbc )    ! sbc frequency
            CALL iom_rstput( kt, nitrst, numrow, 'ssu_m'  , ssu_m  )    ! sea surface mean fields
            CALL iom_rstput( kt, nitrst, numrow, 'ssv_m'  , ssv_m  )
            CALL iom_rstput( kt, nitrst, numrow, 'sst_m'  , sst_m  )
            CALL iom_rstput( kt, nitrst, numrow, 'sss_m'  , sss_m  )
            CALL iom_rstput( kt, nitrst, numrow, 'ssh_m'  , ssh_m  )
            !
         ENDIF
         !
      ENDIF
      !
   END SUBROUTINE sbc_ssm

   !!======================================================================
END MODULE sbcssm
