MODULE limwri
   !!======================================================================
   !!                     ***  MODULE  limwri  ***
   !!         Ice diagnostics :  write ice output files
   !!======================================================================
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3'                                      LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!   lim_wri      : write of the diagnostics variables in ouput file 
   !!   lim_wri_init : initialization and namelist read
   !!----------------------------------------------------------------------
   USE ioipsl
   USE dianam          ! build name of file (routine)
   USE phycst
   USE dom_oce
   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE sbc_ice         ! Surface boundary condition: ice fields
   USE dom_ice
   USE ice
   USE limvar
   USE in_out_manager
   USE lbclnk
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! work arrays
   USE par_ice

   IMPLICIT NONE
   PRIVATE

   PUBLIC lim_wri        ! routine called by lim_step.F90

   INTEGER, PARAMETER ::   jpnoumax = 40   !: maximum number of variable for ice output
   
   INTEGER  ::   noumef             ! number of fields
   INTEGER  ::   noumefa            ! number of additional fields
   INTEGER  ::   add_diag_swi       ! additional diagnostics
   INTEGER  ::   nz                                         ! dimension for the itd field

   REAL(wp) , DIMENSION(jpnoumax) ::   cmulti         ! multiplicative constant
   REAL(wp) , DIMENSION(jpnoumax) ::   cadd           ! additive constant
   REAL(wp) , DIMENSION(jpnoumax) ::   cmultia        ! multiplicative constant
   REAL(wp) , DIMENSION(jpnoumax) ::   cadda          ! additive constant
   CHARACTER(len = 35), DIMENSION(jpnoumax) ::   titn, titna   ! title of the field
   CHARACTER(len = 8 ), DIMENSION(jpnoumax) ::   nam , nama    ! name of the field
   CHARACTER(len = 8 ), DIMENSION(jpnoumax) ::   uni , unia    ! unit of the field
   INTEGER            , DIMENSION(jpnoumax) ::   nc  , nca     ! switch for saving field ( = 1 ) or not ( = 0 )

   REAL(wp)  ::   epsi16 = 1e-16_wp
   REAL(wp)  ::   zzero  = 0._wp
   REAL(wp)  ::   zone   = 1._wp      
   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limwri.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

#if defined key_dimgout
# include "limwri_dimg.h90"
#else

   SUBROUTINE lim_wri( kindic )
      !!-------------------------------------------------------------------
      !!  This routine computes the average of some variables and write it
      !!  on the ouput files.
      !!  ATTENTION cette routine n'est valable que si le pas de temps est
      !!  egale a une fraction entiere de 1 jours.
      !!  Diff 1-D 3-D : suppress common also included in etat
      !!                 suppress cmoymo 11-18
      !!  modif : 03/06/98
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kindic   ! if kindic < 0 there has been an error somewhere
      !
      INTEGER ::  ji, jj, jk, jl, jf, ipl ! dummy loop indices
      INTEGER ::  ierr
      REAL(wp),DIMENSION(1) ::   zdept
      REAL(wp) ::  zsto, zjulian, zout, zindh, zinda, zindb
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zcmo, zcmoa
      REAL(wp), POINTER, DIMENSION(:,:  ) ::   zfield
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zmaskitd, zoi, zei

      CHARACTER(len = 40) ::   clhstnam, clop, clhstnama

      INTEGER , SAVE ::   nice, nhorid, ndim, niter, ndepid
      INTEGER , SAVE ::   nicea, nhorida, ndimitd
      INTEGER , ALLOCATABLE, DIMENSION(:), SAVE ::   ndex51
      INTEGER , ALLOCATABLE, DIMENSION(:), SAVE ::   ndexitd
      !!-------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, zfield )
      CALL wrk_alloc( jpi, jpj, jpnoumax, zcmo, zcmoa )
      CALL wrk_alloc( jpi, jpj, jpl, zmaskitd, zoi, zei )

      ipl = jpl

      IF( numit == nstart ) THEN 

         ALLOCATE( ndex51(jpij), ndexitd(jpij*jpl), STAT=ierr )
         IF( lk_mpp    )   CALL mpp_sum ( ierr )
         IF( ierr /= 0 ) THEN
            CALL ctl_stop( 'lim_wri : unable to allocate standard arrays' )   ;   RETURN
         ENDIF

         CALL lim_wri_init 

         IF(lwp) WRITE(numout,*) ' lim_wri, first time step '
         IF(lwp) WRITE(numout,*) ' add_diag_swi ', add_diag_swi

         !--------------------
         !  1) Initialization
         !--------------------

         !-------------
         ! Normal file
         !-------------

         zsto     = rdt_ice
         IF( ln_mskland )   THEN   ;   clop = "ave(only(x))"   ! put 1.e+20 on land (very expensive!!)
         ELSE                      ;   clop = "ave(x)"         ! no use of the mask value (require less cpu time)
         ENDIF
         zout     = nwrite * rdt_ice / nn_fsbc
         niter    = ( nit000 - 1 ) / nn_fsbc
         zdept(1) = 0.

         CALL ymds2ju ( nyear, nmonth, nday, rdt, zjulian )
         zjulian = zjulian - adatrj   !   set calendar origin to the beginning of the experiment
         CALL dia_nam ( clhstnam, nwrite, 'icemod' )
         CALL histbeg ( clhstnam, jpi, glamt, jpj, gphit, 1, jpi, 1, jpj, niter, zjulian, rdt_ice,   &
            &           nhorid, nice, domain_id=nidom, snc4chunks=snc4set )
         CALL histvert( nice, "deptht", "Vertical T levels", "m", 1, zdept, ndepid, "down")
         CALL wheneq  ( jpij , tmask(:,:,1), 1, 1., ndex51, ndim)

         DO jf = 1 , noumef
            IF(lwp) WRITE(numout,*) 'jf', jf
            IF ( nc(jf) == 1 ) THEN
               CALL histdef( nice, nam(jf), titn(jf), uni(jf), jpi, jpj &
                  , nhorid, 1, 1, 1, -99, 32, clop, zsto, zout )
               IF(lwp) WRITE(numout,*) 'nice, nam(jf), titn(jf), uni(jf), nhorid, clop, zsto, zout'
               IF(lwp) WRITE(numout,*)  nice, nam(jf), titn(jf), uni(jf), nhorid, clop, zsto, zout 
            ENDIF
         END DO

         CALL histend(nice, snc4set)

         !-----------------
         ! ITD file output
         !-----------------
         zsto     = rdt_ice
         clop     = "ave(x)"
         zout     = nwrite * rdt_ice / nn_fsbc
         zdept(1) = 0.

         CALL dia_nam ( clhstnama, nwrite, 'icemoa' )
         CALL histbeg ( clhstnama, jpi, glamt, jpj, gphit,         &
            1, jpi, 1, jpj,            & ! zoom
            niter, zjulian, rdt_ice,   & ! time
            nhorida,                   & ! ? linked with horizontal ...
            nicea , domain_id=nidom, snc4chunks=snc4set)                  ! file 
         CALL histvert( nicea, "icethi", "L levels",               &
            "m", ipl , hi_mean , nz )
         DO jl = 1, jpl
            zmaskitd(:,:,jl) = tmask(:,:,1)
         END DO
         CALL wheneq  ( jpij , tmask(:,:,1), 1, 1., ndex51, ndim)
         CALL wheneq( jpi*jpj*jpl, zmaskitd, 1, 1., ndexitd, ndimitd  )  
         CALL histdef( nicea, "iice_itd", "Ice area in categories"         , "-"    ,   &  
            jpi, jpj, nhorida, jpl, 1, jpl, nz, 15, clop, zsto, zout )
         CALL histdef( nicea, "iice_hid", "Ice thickness in categories"    , "m"    ,   &  
            jpi, jpj, nhorida, jpl, 1, jpl, nz, 15, clop, zsto, zout )
         CALL histdef( nicea, "iice_hsd", "Snow depth in in categories"    , "m"    ,   &  
            jpi, jpj, nhorida, jpl, 1, jpl, nz, 15, clop, zsto, zout )
         CALL histdef( nicea, "iice_std", "Ice salinity distribution"      , "ppt"  ,   &  
            jpi, jpj, nhorida, jpl, 1, jpl, nz, 15, clop, zsto, zout )
         CALL histdef( nicea, "iice_otd", "Ice age distribution"               , "days",   &  
            jpi, jpj, nhorida, jpl, 1, jpl, nz, 15, clop, zsto, zout )
         CALL histdef( nicea, "iice_etd", "Brine volume distr. "               , "%"    ,   &  
            jpi, jpj, nhorida, jpl, 1, jpl, nz, 15, clop, zsto, zout )
         CALL histend(nicea, snc4set)
      ENDIF

      !     !-----------------------------------------------------------------------!
      !     !--2. Computation of instantaneous values                               ! 
      !     !-----------------------------------------------------------------------!

      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF( ln_nicep ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'lim_wri : write ice outputs in NetCDF files at time : ', nyear, nmonth, nday, numit
         WRITE(numout,*) '~~~~~~~ '
         WRITE(numout,*) ' kindic = ', kindic
      ENDIF
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      !-- calculs des valeurs instantanees
      zcmo ( 1:jpi, 1:jpj, 1:jpnoumax ) = 0._wp
      zcmoa( 1:jpi, 1:jpj, 1:jpnoumax ) = 0._wp

      DO jl = 1, jpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               zindh  = MAX( zzero , SIGN( zone , vt_i(ji,jj) * at_i(ji,jj) - 0.10 ) )
               zinda  = MAX( zzero , SIGN( zone , at_i(ji,jj) - 0.10 ) )
               zcmo(ji,jj,17) = zcmo(ji,jj,17) + a_i(ji,jj,jl)*qsr_ice (ji,jj,jl) 
               zcmo(ji,jj,18) = zcmo(ji,jj,18) + a_i(ji,jj,jl)*qns_ice(ji,jj,jl) 
               zcmo(ji,jj,27) = zcmo(ji,jj,27) + t_su(ji,jj,jl)*a_i(ji,jj,jl)/MAX(at_i(ji,jj),epsi16)*zinda
            END DO
         END DO
      END DO

      CALL lim_var_bv

      DO jj = 2 , jpjm1
         DO ji = 2 , jpim1
            zindh  = MAX( zzero , SIGN( zone , vt_i(ji,jj) * at_i(ji,jj) - 0.10 ) )
            zinda  = MAX( zzero , SIGN( zone , at_i(ji,jj) - 0.10 ) )
            zindb  = zindh * zinda

            zcmo(ji,jj,1)  = at_i(ji,jj)
            zcmo(ji,jj,2)  = vt_i(ji,jj) / MAX( at_i(ji,jj), epsi16 ) * zinda
            zcmo(ji,jj,3)  = vt_s(ji,jj) / MAX( at_i(ji,jj), epsi16 ) * zinda
            zcmo(ji,jj,4)  = diag_bot_gr(ji,jj) * 86400.0 * zinda    ! Bottom thermodynamic ice production
            zcmo(ji,jj,5)  = diag_dyn_gr(ji,jj) * 86400.0 * zinda    ! Dynamic ice production (rid/raft)
            zcmo(ji,jj,22) = diag_lat_gr(ji,jj) * 86400.0 * zinda    ! Lateral thermodynamic ice production
            zcmo(ji,jj,23) = diag_sni_gr(ji,jj) * 86400.0 * zinda    ! Snow ice production ice production
            zcmo(ji,jj,24) = tm_i(ji,jj) - rtt

            zcmo(ji,jj,6)  = fbif  (ji,jj)
            zcmo(ji,jj,7)  = zindb * (  u_ice(ji,jj) * tmu(ji,jj) + u_ice(ji-1,jj) * tmu(ji-1,jj) ) * 0.5_wp
            zcmo(ji,jj,8)  = zindb * (  v_ice(ji,jj) * tmv(ji,jj) + v_ice(ji,jj-1) * tmv(ji,jj-1) ) * 0.5_wp
            zcmo(ji,jj,9)  = sst_m(ji,jj)
            zcmo(ji,jj,10) = sss_m(ji,jj)

            zcmo(ji,jj,11) = qns(ji,jj) + qsr(ji,jj)
            zcmo(ji,jj,12) = qsr(ji,jj)
            zcmo(ji,jj,13) = qns(ji,jj)
            zcmo(ji,jj,14) = fhbri(ji,jj)
            zcmo(ji,jj,15) = utau_ice(ji,jj)
            zcmo(ji,jj,16) = vtau_ice(ji,jj)
            zcmo(ji,jj,17) = zcmo(ji,jj,17) + ( 1._wp - at_i(ji,jj) ) * qsr(ji,jj)
            zcmo(ji,jj,18) = zcmo(ji,jj,18) + ( 1._wp - at_i(ji,jj) ) * qns(ji,jj)
            zcmo(ji,jj,19) = sprecip(ji,jj)
            zcmo(ji,jj,20) = smt_i(ji,jj)
            zcmo(ji,jj,21) = ot_i(ji,jj)
            zcmo(ji,jj,25) = et_i(ji,jj)
            zcmo(ji,jj,26) = et_s(ji,jj)
            zcmo(ji,jj,28) = fsbri(ji,jj)
            zcmo(ji,jj,29) = fseqv(ji,jj)

            zcmo(ji,jj,30) = bv_i(ji,jj)
            zcmo(ji,jj,31) = hicol(ji,jj)
            zcmo(ji,jj,32) = strength(ji,jj)
            zcmo(ji,jj,33) = SQRT(  zcmo(ji,jj,7)*zcmo(ji,jj,7) + zcmo(ji,jj,8)*zcmo(ji,jj,8)  )
            zcmo(ji,jj,34) = diag_sur_me(ji,jj) * 86400.0 * zinda    ! Surface melt
            zcmo(ji,jj,35) = diag_bot_me(ji,jj) * 86400.0 * zinda    ! Bottom melt
            zcmo(ji,jj,36) = divu_i(ji,jj)
            zcmo(ji,jj,37) = shear_i(ji,jj)
         END DO
      END DO

      !
      ! ecriture d'un fichier netcdf
      !
      niter = niter + 1
      DO jf = 1 , noumef
         !
         zfield(:,:) = zcmo(:,:,jf) * cmulti(jf) + cadd(jf)
         !
         IF( jf == 7  .OR. jf == 8  .OR. jf == 15 .OR. jf == 16 ) THEN   ;   CALL lbc_lnk( zfield, 'T', -1. )
         ELSE                                                            ;   CALL lbc_lnk( zfield, 'T',  1. )
         ENDIF
         !
         IF( ln_nicep ) THEN 
            WRITE(numout,*)
            WRITE(numout,*) 'nc(jf), nice, nam(jf), niter, ndim'
            WRITE(numout,*) nc(jf), nice, nam(jf), niter, ndim
         ENDIF
         IF( nc(jf) == 1 ) CALL histwrite( nice, nam(jf), niter, zfield, ndim, ndex51 )
         !
      END DO

      IF( ( nn_fsbc * niter ) >= nitend .OR. kindic < 0 ) THEN
         IF( lwp) WRITE(numout,*) ' Closing the icemod file '
         CALL histclo( nice )
      ENDIF

      !-----------------------------
      ! Thickness distribution file
      !-----------------------------
      IF( add_diag_swi == 1 ) THEN

         DO jl = 1, jpl 
            CALL lbc_lnk( a_i(:,:,jl)  , 'T' ,  1. )
            CALL lbc_lnk( sm_i(:,:,jl) , 'T' ,  1. )
            CALL lbc_lnk( oa_i(:,:,jl) , 'T' ,  1. )
            CALL lbc_lnk( ht_i(:,:,jl) , 'T' ,  1. )
            CALL lbc_lnk( ht_s(:,:,jl) , 'T' ,  1. )
         END DO

         ! Compute ice age
         DO jl = 1, jpl 
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zinda = MAX( zzero , SIGN( zone , a_i(ji,jj,jl) - 1.0e-6 ) )
                  zoi(ji,jj,jl) = oa_i(ji,jj,jl)  / MAX( a_i(ji,jj,jl) , 1.0e-6 ) * zinda
               END DO
            END DO
         END DO

         ! Compute brine volume
         zei(:,:,:) = 0._wp
         DO jl = 1, jpl 
            DO jk = 1, nlay_i
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     zinda = MAX( zzero , SIGN( zone , a_i(ji,jj,jl) - 1.0e-6 ) )
                     zei(ji,jj,jl) = zei(ji,jj,jl) + 100.0* &
                        ( - tmut * s_i(ji,jj,jk,jl) / MIN( ( t_i(ji,jj,jk,jl) - rtt ), -1.0e-6 ) ) * &
                        zinda / nlay_i
                  END DO
               END DO
            END DO
         END DO

         DO jl = 1, jpl 
            CALL lbc_lnk( zei(:,:,jl) , 'T' ,  1. )
         END DO

         CALL histwrite( nicea, "iice_itd", niter, a_i  , ndimitd , ndexitd  )   ! area
         CALL histwrite( nicea, "iice_hid", niter, ht_i , ndimitd , ndexitd  )   ! thickness
         CALL histwrite( nicea, "iice_hsd", niter, ht_s , ndimitd , ndexitd  )   ! snow depth
         CALL histwrite( nicea, "iice_std", niter, sm_i , ndimitd , ndexitd  )   ! salinity
         CALL histwrite( nicea, "iice_otd", niter, zoi  , ndimitd , ndexitd  )   ! age
         CALL histwrite( nicea, "iice_etd", niter, zei  , ndimitd , ndexitd  )   ! brine volume

         !     !  Create an output files (output.lim.abort.nc) if S < 0 or u > 20 m/s
         !     IF( kindic < 0 )   CALL lim_wri_state( 'output.abort' )
         !     not yet implemented

         IF( ( nn_fsbc * niter ) >= nitend .OR. kindic < 0 ) THEN
            IF(lwp) WRITE(numout,*) ' Closing the icemod file '
            CALL histclo( nicea ) 
         ENDIF
         !
      ENDIF

      CALL wrk_dealloc( jpi, jpj, zfield )
      CALL wrk_dealloc( jpi, jpj, jpnoumax, zcmo, zcmoa )
      CALL wrk_dealloc( jpi, jpj, jpl, zmaskitd, zoi, zei )
      
   END SUBROUTINE lim_wri
#endif

   SUBROUTINE lim_wri_init
      !!-------------------------------------------------------------------
      !!                    ***   ROUTINE lim_wri_init  ***
      !!                
      !! ** Purpose :   ???
      !!
      !! ** Method  : Read the namicewri namelist and check the parameter 
      !!       values called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namicewri
      !!-------------------------------------------------------------------
      INTEGER ::   nf      ! ???

      TYPE FIELD 
         CHARACTER(len = 35) :: ztitle 
         CHARACTER(len = 8 ) :: zname          
         CHARACTER(len = 8 ) :: zunit
         INTEGER             :: znc   
         REAL                :: zcmulti 
         REAL                :: zcadd        
      END TYPE FIELD

      TYPE(FIELD) ::  &
         field_1 , field_2 , field_3 , field_4 , field_5 , field_6 ,   &
         field_7 , field_8 , field_9 , field_10, field_11, field_12,   &
         field_13, field_14, field_15, field_16, field_17, field_18,   &
         field_19, field_20, field_21, field_22, field_23, field_24,   &
         field_25, field_26, field_27, field_28, field_29, field_30,   &
         field_31, field_32, field_33, field_34, field_35, field_36,   &
         field_37

      TYPE(FIELD) , DIMENSION(jpnoumax) :: zfield
      !
      NAMELIST/namiceout/ noumef, &
         field_1 , field_2 , field_3 , field_4 , field_5 , field_6 ,   &
         field_7 , field_8 , field_9 , field_10, field_11, field_12,   &
         field_13, field_14, field_15, field_16, field_17, field_18,   &
         field_19, field_20, field_21, field_22, field_23, field_24,   &
         field_25, field_26, field_27, field_28, field_29, field_30,   &
         field_31, field_32, field_33, field_34, field_35, field_36,   &
         field_37, add_diag_swi
      !!-------------------------------------------------------------------

      REWIND( numnam_ice )                ! Read Namelist namicewri
      READ  ( numnam_ice  , namiceout )

      zfield(1)  = field_1
      zfield(2)  = field_2
      zfield(3)  = field_3
      zfield(4)  = field_4
      zfield(5)  = field_5
      zfield(6)  = field_6
      zfield(7)  = field_7
      zfield(8)  = field_8
      zfield(9)  = field_9
      zfield(10) = field_10
      zfield(11) = field_11
      zfield(12) = field_12
      zfield(13) = field_13
      zfield(14) = field_14
      zfield(15) = field_15
      zfield(16) = field_16
      zfield(17) = field_17
      zfield(18) = field_18
      zfield(19) = field_19
      zfield(20) = field_20
      zfield(21) = field_21
      zfield(22) = field_22
      zfield(23) = field_23
      zfield(24) = field_24
      zfield(25) = field_25
      zfield(26) = field_26
      zfield(27) = field_27
      zfield(28) = field_28
      zfield(29) = field_29
      zfield(30) = field_30
      zfield(31) = field_31
      zfield(32) = field_32
      zfield(33) = field_33
      zfield(34) = field_34
      zfield(35) = field_35
      zfield(36) = field_36
      zfield(37) = field_37

      DO nf = 1, noumef
         titn  (nf) = zfield(nf)%ztitle
         nam   (nf) = zfield(nf)%zname
         uni   (nf) = zfield(nf)%zunit
         nc    (nf) = zfield(nf)%znc
         cmulti(nf) = zfield(nf)%zcmulti
         cadd  (nf) = zfield(nf)%zcadd
      END DO

      IF(lwp) THEN                        ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'lim_wri_init : Ice parameters for outputs'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '    number of fields to be stored         noumef = ', noumef
         WRITE(numout,*) '           title                            name     unit      Saving (1/0) ',   &
            &            '    multiplicative constant       additive constant '
         DO nf = 1 , noumef         
            WRITE(numout,*) '   ', titn(nf), '   '    , nam   (nf), '      '  , uni (nf),   &
               &            '  ' , nc  (nf),'        ', cmulti(nf), '        ', cadd(nf)
         END DO
         WRITE(numout,*) ' add_diag_swi ', add_diag_swi
      ENDIF
      !
   END SUBROUTINE lim_wri_init

#else
   !!----------------------------------------------------------------------
   !!   Default option :         Empty module          NO LIM sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_wri          ! Empty routine
   END SUBROUTINE lim_wri
#endif

   !!======================================================================
END MODULE limwri
