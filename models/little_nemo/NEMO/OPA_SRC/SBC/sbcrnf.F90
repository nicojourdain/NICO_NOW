MODULE sbcrnf
   !!======================================================================
   !!                       ***  MODULE  sbcrnf  ***
   !! Ocean forcing:  river runoff
   !!=====================================================================
   !! History :  OPA  ! 2000-11  (R. Hordoir, E. Durand)  NetCDF FORMAT
   !!   NEMO     1.0  ! 2002-09  (G. Madec)  F90: Free form and module
   !!            3.0  ! 2006-07  (G. Madec)  Surface module 
   !!            3.2  ! 2009-04  (B. Lemaire)  Introduce iom_put
   !!            3.3  ! 2010-10  (R. Furner, G. Madec) runoff distributed over ocean levels
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_rnf      : monthly runoffs read in a NetCDF file
   !!   sbc_rnf_init : runoffs initialisation
   !!   rnf_mouth    : set river mouth mask
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE sbc_oce         ! surface boundary condition variables
   USE closea          ! closed seas
   USE fldread         ! read input field at current time step
   USE in_out_manager  ! I/O manager
   USE iom             ! I/O module
   USE lib_mpp         ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_rnf       ! routine call in sbcmod module
   PUBLIC   sbc_rnf_div   ! routine called in sshwzv module
   PUBLIC   sbc_rnf_alloc ! routine call in sbcmod module

   !                                                     !!* namsbc_rnf namelist *
   CHARACTER(len=100), PUBLIC ::   cn_dir       = './'    !: Root directory for location of ssr files
   LOGICAL           , PUBLIC ::   ln_rnf_depth = .false. !: depth       river runoffs attribute specified in a file
   LOGICAL           , PUBLIC ::   ln_rnf_tem   = .false. !: temperature river runoffs attribute specified in a file 
   LOGICAL           , PUBLIC ::   ln_rnf_sal   = .false. !: salinity    river runoffs attribute specified in a file 
   LOGICAL           , PUBLIC ::   ln_rnf_emp   = .false. !: runoffs into a file to be read or already into precipitation
   TYPE(FLD_N)       , PUBLIC ::   sn_rnf                 !: information about the runoff file to be read
   TYPE(FLD_N)       , PUBLIC ::   sn_cnf                 !: information about the runoff mouth file to be read
   TYPE(FLD_N)                ::   sn_s_rnf               !: information about the salinities of runoff file to be read  
   TYPE(FLD_N)                ::   sn_t_rnf               !: information about the temperatures of runoff file to be read  
   TYPE(FLD_N)                ::   sn_dep_rnf             !: information about the depth which river inflow affects
   LOGICAL           , PUBLIC ::   ln_rnf_mouth = .false. !: specific treatment in mouths vicinity
   REAL(wp)          , PUBLIC ::   rn_hrnf      = 0._wp   !: runoffs, depth over which enhanced vertical mixing is used
   REAL(wp)          , PUBLIC ::   rn_avt_rnf   = 0._wp   !: runoffs, value of the additional vertical mixing coef. [m2/s]
   REAL(wp)          , PUBLIC ::   rn_rfact     = 1._wp   !: multiplicative factor for runoff

   INTEGER , PUBLIC  ::   nkrnf = 0         !: nb of levels over which Kz is increased at river mouths
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   rnfmsk              !: river mouth mask (hori.)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)     ::   rnfmsk_z            !: river mouth mask (vert.)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   h_rnf               !: depth of runoff in m
   INTEGER,  PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   nk_rnf              !: depth of runoff in model levels
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   rnf_tsc_b, rnf_tsc  !: before and now T & S runoff contents   [K.m/s & PSU.m/s]
   
   REAL(wp) ::   r1_rau0   ! = 1 / rau0 

   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_rnf       ! structure: river runoff (file information, fields read)
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_s_rnf     ! structure: river runoff salinity (file information, fields read)  
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_t_rnf     ! structure: river runoff temperature (file information, fields read)  
 
   !! * Substitutions  
#  include "domzgr_substitute.h90"  
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: sbcrnf.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION sbc_rnf_alloc()
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE sbc_rnf_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( rnfmsk(jpi,jpj)         , rnfmsk_z(jpk)          ,     &
         &      h_rnf (jpi,jpj)         , nk_rnf  (jpi,jpj)      ,     &
         &      rnf_tsc_b(jpi,jpj,jpts) , rnf_tsc (jpi,jpj,jpts) , STAT=sbc_rnf_alloc )
         !
      IF( lk_mpp            )   CALL mpp_sum ( sbc_rnf_alloc )
      IF( sbc_rnf_alloc > 0 )   CALL ctl_warn('sbc_rnf_alloc: allocation of arrays failed')
   END FUNCTION sbc_rnf_alloc

   SUBROUTINE sbc_rnf( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE sbc_rnf  ***
      !!       
      !! ** Purpose :   Introduce a climatological run off forcing
      !!
      !! ** Method  :   Set each river mouth with a monthly climatology 
      !!                provided from different data.
      !!                CAUTION : upward water flux, runoff forced to be < 0
      !!
      !! ** Action  :   runoff updated runoff field at time-step kt
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt          ! ocean time step
      !!
      INTEGER  ::   ji, jj   ! dummy loop indices
      !!----------------------------------------------------------------------
      !                                   
      IF( kt == nit000 )   CALL sbc_rnf_init                           ! Read namelist and allocate structures

      !                                            ! ---------------------------------------- !
      IF( kt /= nit000 ) THEN                      !          Swap of forcing fields          !
         !                                         ! ---------------------------------------- !
         rnf_b    (:,:  ) = rnf    (:,:  )               ! Swap the ocean forcing fields except at nit000
         rnf_tsc_b(:,:,:) = rnf_tsc(:,:,:)               ! where before fields are set at the end of the routine
         !
      ENDIF

      !                                                   !-------------------!
      IF( .NOT. ln_rnf_emp ) THEN                         !   Update runoff   !
         !                                                !-------------------!
         !
                             CALL fld_read ( kt, nn_fsbc, sf_rnf   )    ! Read Runoffs data and provide it at kt 
         IF( ln_rnf_tem  )   CALL fld_read ( kt, nn_fsbc, sf_t_rnf )    ! idem for runoffs temperature if required
         IF( ln_rnf_sal  )   CALL fld_read ( kt, nn_fsbc, sf_s_rnf )    ! idem for runoffs salinity    if required
         !
         ! Runoff reduction only associated to the ORCA2_LIM configuration
         ! when reading the NetCDF file runoff_1m_nomask.nc
         IF( cp_cfg == 'orca' .AND. jp_cfg == 2 )   THEN
            WHERE( 40._wp < gphit(:,:) .AND. gphit(:,:) < 65._wp )
               sf_rnf(1)%fnow(:,:,1) = 0.85 * sf_rnf(1)%fnow(:,:,1)
            END WHERE
         ENDIF
         !
         IF( MOD( kt - 1, nn_fsbc ) == 0 ) THEN
            rnf(:,:) = rn_rfact * ( sf_rnf(1)%fnow(:,:,1) )  
            !
            r1_rau0 = 1._wp / rau0
            !                                                     ! set temperature & salinity content of runoffs
            IF( ln_rnf_tem ) THEN                                       ! use runoffs temperature data
               rnf_tsc(:,:,jp_tem) = ( sf_t_rnf(1)%fnow(:,:,1) ) * rnf(:,:) * r1_rau0
               WHERE( sf_t_rnf(1)%fnow(:,:,1) == -999 )                 ! if missing data value use SST as runoffs temperature  
                   rnf_tsc(:,:,jp_tem) = sst_m(:,:) * rnf(:,:) * r1_rau0
               END WHERE
            ELSE                                                        ! use SST as runoffs temperature
               rnf_tsc(:,:,jp_tem) = sst_m(:,:) * rnf(:,:) * r1_rau0
            ENDIF  
            !                                                           ! use runoffs salinity data 
            IF( ln_rnf_sal )   rnf_tsc(:,:,jp_sal) = ( sf_s_rnf(1)%fnow(:,:,1) ) * rnf(:,:) * r1_rau0
            !                                                           ! else use S=0 for runoffs (done one for all in the init)
            !
            IF( ln_rnf_tem .OR. ln_rnf_sal ) THEN                 ! runoffs as outflow: use ocean SST and SSS
               WHERE( rnf(:,:) < 0._wp )                                 ! example baltic model when flow is out of domain 
                  rnf_tsc(:,:,jp_tem) = sst_m(:,:) * rnf(:,:) * r1_rau0
                  rnf_tsc(:,:,jp_sal) = sss_m(:,:) * rnf(:,:) * r1_rau0
               END WHERE
            ENDIF
            !
            CALL iom_put( "runoffs", rnf )         ! output runoffs arrays
         ENDIF
         !
      ENDIF
      !
      IF( kt == nit000 ) THEN                          !   set the forcing field at nit000 - 1    !
         !                                             ! ---------------------------------------- !
         IF( ln_rstart .AND.    &                               !* Restart: read in restart file
            & iom_varid( numror, 'rnf_b', ldstop = .FALSE. ) > 0 ) THEN 
            IF(lwp) WRITE(numout,*) '          nit000-1 runoff forcing fields red in the restart file'
            CALL iom_get( numror, jpdom_autoglo, 'rnf_b', rnf_b )     ! before runoff
            CALL iom_get( numror, jpdom_autoglo, 'rnf_hc_b', rnf_tsc_b(:,:,jp_tem) )   ! before heat content of runoff
            CALL iom_get( numror, jpdom_autoglo, 'rnf_sc_b', rnf_tsc_b(:,:,jp_sal) )   ! before salinity content of runoff
         ELSE                                                   !* no restart: set from nit000 values
            IF(lwp) WRITE(numout,*) '          nit000-1 runoff forcing fields set to nit000'
             rnf_b    (:,:  ) = rnf    (:,:  )  
             rnf_tsc_b(:,:,:) = rnf_tsc(:,:,:)   
         ENDIF
      ENDIF
      !                                                ! ---------------------------------------- !
      IF( lrst_oce ) THEN                              !      Write in the ocean restart file     !
         !                                             ! ---------------------------------------- !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'sbcrnf : runoff forcing fields written in ocean restart file ',   &
            &                    'at it= ', kt,' date= ', ndastp
         IF(lwp) WRITE(numout,*) '~~~~'
         CALL iom_rstput( kt, nitrst, numrow, 'rnf_b' , rnf )
         CALL iom_rstput( kt, nitrst, numrow, 'rnf_hc_b', rnf_tsc(:,:,jp_tem) )
         CALL iom_rstput( kt, nitrst, numrow, 'rnf_sc_b', rnf_tsc(:,:,jp_sal) )
      ENDIF
      !
   END SUBROUTINE sbc_rnf


   SUBROUTINE sbc_rnf_div( phdivn )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE sbc_rnf  ***
      !!       
      !! ** Purpose :   update the horizontal divergence with the runoff inflow
      !!
      !! ** Method  :   
      !!                CAUTION : rnf is positive (inflow) decreasing the 
      !!                          divergence and expressed in m/s
      !!
      !! ** Action  :   phdivn   decreased by the runoff inflow
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   phdivn   ! horizontal divergence
      !!
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   r1_rau0   ! local scalar
      REAL(wp) ::   zfact     ! local scalar
      !!----------------------------------------------------------------------
      !
      zfact = 0.5_wp
      !
      r1_rau0 = 1._wp / rau0
      IF( ln_rnf_depth ) THEN      !==   runoff distributed over several levels   ==!
         IF( lk_vvl ) THEN             ! variable volume case 
            DO jj = 1, jpj                   ! update the depth over which runoffs are distributed
               DO ji = 1, jpi
                  h_rnf(ji,jj) = 0._wp 
                  DO jk = 1, nk_rnf(ji,jj)                           ! recalculates h_rnf to be the depth in metres
                     h_rnf(ji,jj) = h_rnf(ji,jj) + fse3t(ji,jj,jk)   ! to the bottom of the relevant grid box 
                  END DO 
                  !                          ! apply the runoff input flow
                  DO jk = 1, nk_rnf(ji,jj)
                     phdivn(ji,jj,jk) = phdivn(ji,jj,jk) - ( rnf(ji,jj) + rnf_b(ji,jj) ) * zfact * r1_rau0 / h_rnf(ji,jj)
                  END DO
               END DO
            END DO
         ELSE                          ! constant volume case : just apply the runoff input flow
            DO jj = 1, jpj
               DO ji = 1, jpi
                  DO jk = 1, nk_rnf(ji,jj)
                     phdivn(ji,jj,jk) = phdivn(ji,jj,jk) - ( rnf(ji,jj) + rnf_b(ji,jj) ) * zfact * r1_rau0 / h_rnf(ji,jj)
                  END DO
               END DO
            END DO
         ENDIF
      ELSE                       !==   runoff put only at the surface   ==!
         IF( lk_vvl ) THEN              ! variable volume case
            h_rnf(:,:) = fse3t(:,:,1)   ! recalculate h_rnf to be depth of top box
         ENDIF
         phdivn(:,:,1) = phdivn(:,:,1) - ( rnf(:,:) + rnf_b(:,:) ) * zfact * r1_rau0 / fse3t(:,:,1)
      ENDIF
      !
   END SUBROUTINE sbc_rnf_div


   SUBROUTINE sbc_rnf_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE sbc_rnf_init  ***
      !!
      !! ** Purpose :   Initialisation of the runoffs if (ln_rnf=T)
      !!
      !! ** Method  : - read the runoff namsbc_rnf namelist
      !!
      !! ** Action  : - read parameters
      !!----------------------------------------------------------------------
      CHARACTER(len=32) ::   rn_dep_file   ! runoff file name  
      INTEGER           ::   ji, jj, jk    ! dummy loop indices
      INTEGER           ::   ierror, inum  ! temporary integer
      !! 
      NAMELIST/namsbc_rnf/ cn_dir, ln_rnf_emp, ln_rnf_depth, ln_rnf_tem, ln_rnf_sal,   &
         &                 sn_rnf, sn_cnf    , sn_s_rnf    , sn_t_rnf  , sn_dep_rnf,   &  
         &                 ln_rnf_mouth      , rn_hrnf     , rn_avt_rnf, rn_rfact  
      !!----------------------------------------------------------------------

      !                                   ! ============
      !                                   !   Namelist
      !                                   ! ============
      ! (NB: frequency positive => hours, negative => months)
      !            !   file    ! frequency !  variable  ! time intep !  clim  ! 'yearly' or ! weights  ! rotation   !
      !            !   name    !  (hours)  !   name     !   (T/F)    !  (T/F) !  'monthly'  ! filename ! pairs      !
      sn_rnf = FLD_N( 'runoffs',    -1     , 'sorunoff' ,  .TRUE.    , .true. ,   'yearly'  , ''       , ''         )
      sn_cnf = FLD_N( 'runoffs',     0     , 'sorunoff' ,  .FALSE.   , .true. ,   'yearly'  , ''       , ''         )

      sn_s_rnf = FLD_N( 'runoffs',  24.  , 'rosaline' ,  .TRUE.    , .true. ,   'yearly'  , ''    , ''  )  
      sn_t_rnf = FLD_N( 'runoffs',  24.  , 'rotemper' ,  .TRUE.    , .true. ,   'yearly'  , ''    , ''  )  
      sn_dep_rnf = FLD_N( 'runoffs',   0.  , 'rodepth'  ,  .FALSE.   , .true. ,   'yearly'  , ''    , ''  )  
      !
      REWIND ( numnam )                         ! Read Namelist namsbc_rnf
      READ   ( numnam, namsbc_rnf )

      !                                         ! Control print
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'sbc_rnf : runoff '
         WRITE(numout,*) '~~~~~~~ '
         WRITE(numout,*) '   Namelist namsbc_rnf'
         WRITE(numout,*) '      runoff in a file to be read                ln_rnf_emp   = ', ln_rnf_emp
         WRITE(numout,*) '      specific river mouths treatment            ln_rnf_mouth = ', ln_rnf_mouth
         WRITE(numout,*) '      river mouth additional Kz                  rn_avt_rnf   = ', rn_avt_rnf
         WRITE(numout,*) '      depth of river mouth additional mixing     rn_hrnf      = ', rn_hrnf
         WRITE(numout,*) '      multiplicative factor for runoff           rn_rfact     = ', rn_rfact    
      ENDIF

      !                                   ! ==================
      !                                   !   Type of runoff
      !                                   ! ==================
      !                                         !==  allocate runoff arrays
      IF( sbc_rnf_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'sbc_rnf_alloc : unable to allocate arrays' )
      !
      IF( ln_rnf_emp ) THEN                     !==  runoffs directly provided in the precipitations  ==!
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '          runoffs directly provided in the precipitations'
         IF( ln_rnf_depth .OR. ln_rnf_tem .OR. ln_rnf_sal ) THEN
           CALL ctl_warn( 'runoffs already included in precipitations, so runoff (T,S, depth) attributes will not be used' ) 
           ln_rnf_depth = .FALSE.   ;   ln_rnf_tem = .FALSE.   ;   ln_rnf_sal = .FALSE.
         ENDIF
         !
      ELSE                                      !==  runoffs read in a file : set sf_rnf structure  ==!
         !
         ALLOCATE( sf_rnf(1), STAT=ierror )         ! Create sf_rnf structure (runoff inflow)
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '          runoffs inflow read in a file'
         IF( ierror > 0 ) THEN
            CALL ctl_stop( 'sbc_rnf: unable to allocate sf_rnf structure' )   ;   RETURN
         ENDIF
         ALLOCATE( sf_rnf(1)%fnow(jpi,jpj,1)   )
         IF( sn_rnf%ln_tint ) ALLOCATE( sf_rnf(1)%fdta(jpi,jpj,1,2) )
         !                                          ! fill sf_rnf with the namelist (sn_rnf) and control print
         CALL fld_fill( sf_rnf, (/ sn_rnf /), cn_dir, 'sbc_rnf_init', 'read runoffs data', 'namsbc_rnf' )
         !
         IF( ln_rnf_tem ) THEN                      ! Create (if required) sf_t_rnf structure
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) '          runoffs temperatures read in a file'
            ALLOCATE( sf_t_rnf(1), STAT=ierror  )
            IF( ierror > 0 ) THEN
               CALL ctl_stop( 'sbc_rnf_init: unable to allocate sf_t_rnf structure' )   ;   RETURN
            ENDIF
            ALLOCATE( sf_t_rnf(1)%fnow(jpi,jpj,1)   )
            IF( sn_t_rnf%ln_tint ) ALLOCATE( sf_t_rnf(1)%fdta(jpi,jpj,1,2) )
            CALL fld_fill (sf_t_rnf, (/ sn_t_rnf /), cn_dir, 'sbc_rnf_init', 'read runoff temperature data', 'namsbc_rnf' )  
         ENDIF
         !
         IF( ln_rnf_sal  ) THEN                     ! Create (if required) sf_s_rnf and sf_t_rnf structures
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) '          runoffs salinities read in a file'
            ALLOCATE( sf_s_rnf(1), STAT=ierror  )
            IF( ierror > 0 ) THEN
               CALL ctl_stop( 'sbc_rnf_init: unable to allocate sf_s_rnf structure' )   ;   RETURN
            ENDIF
            ALLOCATE( sf_s_rnf(1)%fnow(jpi,jpj,1)   )
            IF( sn_s_rnf%ln_tint ) ALLOCATE( sf_s_rnf(1)%fdta(jpi,jpj,1,2) )
            CALL fld_fill (sf_s_rnf, (/ sn_s_rnf /), cn_dir, 'sbc_rnf_init', 'read runoff salinity data', 'namsbc_rnf' )  
         ENDIF
         !
         IF( ln_rnf_depth ) THEN                    ! depth of runoffs set from a file 
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) '          runoffs depth read in a file'
            rn_dep_file = TRIM( cn_dir )//TRIM( sn_dep_rnf%clname )  
            CALL iom_open ( rn_dep_file, inum )                           ! open file  
            CALL iom_get  ( inum, jpdom_data, sn_dep_rnf%clvar, h_rnf )   ! read the river mouth array  
            CALL iom_close( inum )                                        ! close file  
            !
            nk_rnf(:,:) = 0                               ! set the number of level over which river runoffs are applied
            DO jj = 1, jpj
               DO ji = 1, jpi
                  IF( h_rnf(ji,jj) > 0._wp ) THEN
                     jk = 2
                     DO WHILE ( jk /= mbkt(ji,jj) .AND. fsdept(ji,jj,jk) < h_rnf(ji,jj) ) ;  jk = jk + 1 ;  END DO
                     nk_rnf(ji,jj) = jk
                  ELSEIF( h_rnf(ji,jj) == -1._wp   ) THEN   ;  nk_rnf(ji,jj) = 1
                  ELSEIF( h_rnf(ji,jj) == -999._wp ) THEN   ;  nk_rnf(ji,jj) = mbkt(ji,jj)
                  ELSE
                     CALL ctl_stop( 'runoff depth not positive, and not -999 or -1, rnf value in file fort.999'  )
                     WRITE(999,*) 'ji, jj, rnf(ji,jj) :', ji, jj, rnf(ji,jj)
                  ENDIF
               END DO
            END DO
            DO jj = 1, jpj                                ! set the associated depth
               DO ji = 1, jpi
                  h_rnf(ji,jj) = 0._wp
                  DO jk = 1, nk_rnf(ji,jj)                        
                     h_rnf(ji,jj) = h_rnf(ji,jj) + fse3t(ji,jj,jk)  
                  END DO
               END DO
            END DO
         ELSE                                       ! runoffs applied at the surface 
            nk_rnf(:,:) = 1  
            h_rnf (:,:) = fse3t(:,:,1)
         ENDIF  
         ! 
      ENDIF
      !
      rnf(:,:) =  0._wp                         ! runoff initialisation
      rnf_tsc(:,:,:) = 0._wp                    ! runoffs temperature & salinty contents initilisation
      !
      !                                   ! ========================
      !                                   !   River mouth vicinity
      !                                   ! ========================
      !
      IF( ln_rnf_mouth ) THEN                   ! Specific treatment in vicinity of river mouths :
         !                                      !    - Increase Kz in surface layers ( rn_hrnf > 0 )
         !                                      !    - set to zero SSS damping (ln_ssr=T)
         !                                      !    - mixed upstream-centered (ln_traadv_cen2=T)
         !
         IF ( ln_rnf_depth )   CALL ctl_warn( 'sbc_rnf_init: increased mixing turned on but effects may already',   &
            &                                              'be spread through depth by ln_rnf_depth'               ) 
         !
         nkrnf = 0                                  ! Number of level over which Kz increase
         IF( rn_hrnf > 0._wp ) THEN
            nkrnf = 2
            DO WHILE( nkrnf /= jpkm1 .AND. gdepw_0(nkrnf+1) < rn_hrnf )   ;   nkrnf = nkrnf + 1   ;   END DO
            IF( ln_sco )   &
               CALL ctl_warn( 'sbc_rnf: number of levels over which Kz is increased is computed for zco...' )
         ENDIF
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '          Specific treatment used in vicinity of river mouths :'
         IF(lwp) WRITE(numout,*) '             - Increase Kz in surface layers (if rn_hrnf > 0 )'
         IF(lwp) WRITE(numout,*) '               by ', rn_avt_rnf,' m2/s  over ', nkrnf, ' w-levels'
         IF(lwp) WRITE(numout,*) '             - set to zero SSS damping       (if ln_ssr=T)'
         IF(lwp) WRITE(numout,*) '             - mixed upstream-centered       (if ln_traadv_cen2=T)'
         !
         CALL rnf_mouth                             ! set river mouth mask
         !
      ELSE                                      ! No treatment at river mouths
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '          No specific treatment at river mouths'
         rnfmsk  (:,:) = 0._wp 
         rnfmsk_z(:)   = 0._wp
         nkrnf = 0
      ENDIF

   END SUBROUTINE sbc_rnf_init


   SUBROUTINE rnf_mouth
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE rnf_mouth  ***
      !!       
      !! ** Purpose :   define the river mouths mask
      !!
      !! ** Method  :   read the river mouth mask (=0/1) in the river runoff
      !!                climatological file. Defined a given vertical structure. 
      !!                CAUTION, the vertical structure is hard coded on the 
      !!                first 5 levels.
      !!                This fields can be used to:
      !!                 - set an upstream advection scheme  
      !!                   (ln_rnf_mouth=T and ln_traadv_cen2=T)
      !!                 - increase vertical on the top nn_krnf vertical levels 
      !!                   at river runoff input grid point (nn_krnf>=2, see step.F90)
      !!                 - set to zero SSS restoring flux at river mouth grid points
      !!
      !! ** Action  :   rnfmsk   set to 1 at river runoff input, 0 elsewhere
      !!                rnfmsk_z vertical structure
      !!----------------------------------------------------------------------
      !
      INTEGER            ::   inum        ! temporary integers
      CHARACTER(len=140) ::   cl_rnfile   ! runoff file name
      !!----------------------------------------------------------------------
      ! 
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'rnf_mouth : river mouth mask'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~ '

      cl_rnfile = TRIM( cn_dir )//TRIM( sn_cnf%clname )
      IF( .NOT. sn_cnf%ln_clim ) THEN   ;   WRITE(cl_rnfile, '(a,"_y",i4)' ) TRIM( cl_rnfile ), nyear    ! add year
         IF( sn_cnf%cltype == 'monthly' )   WRITE(cl_rnfile, '(a,"m",i2)'  ) TRIM( cl_rnfile ), nmonth   ! add month
      ENDIF
  
      ! horizontal mask (read in NetCDF file)
      CALL iom_open ( cl_rnfile, inum )                           ! open file
      CALL iom_get  ( inum, jpdom_data, sn_cnf%clvar, rnfmsk )    ! read the river mouth array
      CALL iom_close( inum )                                      ! close file
      
      IF( nclosea == 1 )    CALL clo_rnf( rnfmsk )                ! closed sea inflow set as ruver mouth

      rnfmsk_z(:)   = 0._wp                                        ! vertical structure 
      rnfmsk_z(1)   = 1.0
      rnfmsk_z(2)   = 1.0                                         ! **********
      rnfmsk_z(3)   = 0.5                                         ! HARD CODED on the 5 first levels
      rnfmsk_z(4)   = 0.25                                        ! **********
      rnfmsk_z(5)   = 0.125
      !         
   END SUBROUTINE rnf_mouth
   
   !!======================================================================
END MODULE sbcrnf
