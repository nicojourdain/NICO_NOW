MODULE limwri_2
   !!======================================================================
   !!                     ***  MODULE  limwri_2  ***
   !!         Ice diagnostics :  write ice output files
   !!======================================================================
   !! history :  2.0  ! 2003-08  (C. Ethe)      original code
   !!            2.0  ! 2004-10  (C. Ethe )     1D configuration
   !!             -   ! 2009-06  (B. Lemaire )  iom_put + lim_wri_state_2
   !!-------------------------------------------------------------------
#if defined key_lim2
   !!----------------------------------------------------------------------
   !!   'key_lim2'                                    LIM 2.0 sea-ice model
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   lim_wri_2      : write of the diagnostics variables in ouput file 
   !!   lim_wri_init_2 : initialization and namelist read
   !!   lim_wri_state_2 : write for initial state or/and abandon:
   !!                     > output.init.nc (if ninist = 1 in namelist)
   !!                     > output.abort.nc
   !!----------------------------------------------------------------------
   USE phycst
   USE dom_oce
   USE sbc_oce
   USE sbc_ice
   USE dom_ice_2
   USE ice_2

   USE dianam          ! build name of file (routine)
   USE lbclnk
   USE in_out_manager
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! work arrays
   USE iom
   USE ioipsl

   IMPLICIT NONE
   PRIVATE

#if ! defined key_iomput
   PUBLIC   lim_wri_2         ! called by sbc_ice_lim_2
#endif
   PUBLIC   lim_wri_state_2   ! called by dia_wri_state 
   PUBLIC   lim_wri_alloc_2   ! called by nemogcm.F90

   INTEGER, PARAMETER                       ::   jpnoumax = 40   ! maximum number of variable for ice output
   INTEGER                                  ::   noumef          ! number of fields
   REAL(wp)           , DIMENSION(jpnoumax) ::   cmulti ,     &  ! multiplicative constant
      &                                          cadd            ! additive constant
   CHARACTER(len = 35), DIMENSION(jpnoumax) ::   titn            ! title of the field
   CHARACTER(len = 8 ), DIMENSION(jpnoumax) ::   nam             ! name of the field
   CHARACTER(len = 8 ), DIMENSION(jpnoumax) ::   uni             ! unit of the field
   INTEGER            , DIMENSION(jpnoumax) ::   nc              ! switch for saving field ( = 1 ) or not ( = 0 )

   INTEGER ::   nice, nhorid, ndim, niter, ndepid       ! ????
   INTEGER, ALLOCATABLE, SAVE, DIMENSION(:) :: ndex51   ! ????

   REAL(wp) ::   epsi16 = 1.e-16_wp   ! constant values
   REAL(wp) ::   zzero  = 0._wp       !     -      -
   REAL(wp) ::   zone   = 1._wp       !     -      -

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   zcmo      ! Workspace array for netcdf writer. 


   !! * Substitutions
#   include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/LIM2 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: limwri_2.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION lim_wri_alloc_2()
      !!-------------------------------------------------------------------
      !!                  ***   ROUTINE lim_wri_alloc_2  ***
      !!-------------------------------------------------------------------
      ALLOCATE( ndex51(jpij), zcmo(jpi,jpj,jpnoumax), STAT=lim_wri_alloc_2)
      !
      IF( lk_mpp               )   CALL mpp_sum ( lim_wri_alloc_2 )
      IF( lim_wri_alloc_2 /= 0 )   CALL ctl_warn('lim_wri_alloc_2: failed to allocate array ndex51')
      !
   END FUNCTION lim_wri_alloc_2


#if ! defined key_iomput
# if defined key_dimgout
   !!----------------------------------------------------------------------
   !!   'key_dimgout'                                    Direct Access file
   !!----------------------------------------------------------------------
# include "limwri_dimg_2.h90"
# else
   SUBROUTINE lim_wri_2( kt )
      !!-------------------------------------------------------------------
      !!                    ***   ROUTINE lim_wri_2  ***
      !!                
      !! ** Purpose :   write the sea-ice output file in NetCDF
      !!
      !! ** Method  :   computes the average of some variables and write
      !!      it in the NetCDF ouput files
      !!      CAUTION: the sea-ice time-step must be an integer fraction
      !!      of a day
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt     ! number of iteration
      !!
      INTEGER  ::   ji, jj, jf                      ! dummy loop indices
      CHARACTER(len = 40)  ::   clhstnam, clop
      REAL(wp) ::   zsto, zjulian, zout,   &  ! temporary scalars
         &          zindh, zinda, zindb, ztmu
      REAL(wp), DIMENSION(1)                ::   zdept
      REAL(wp), POINTER, DIMENSION(:,:)     ::   zfield
      !!-------------------------------------------------------------------

      CALL wrk_alloc( jpi, jpj, zfield )
                                                 !--------------------!
      IF( kt == nit000 ) THEN                    !   Initialisation   !
         !                                       !--------------------!

         CALL lim_wri_init_2 
                           
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
         CALL histbeg ( clhstnam, jpi, glamt, jpj, gphit,    &
            &           1, jpi, 1, jpj, niter, zjulian, rdt_ice, nhorid, nice , domain_id=nidom, snc4chunks=snc4set)
         CALL histvert( nice, "deptht", "Vertical T levels", "m", 1, zdept, ndepid, "down")
         CALL wheneq  ( jpij , tmask(:,:,1), 1, 1., ndex51, ndim)
         
         DO jf = 1, noumef
            IF( nc(jf) == 1 )   CALL histdef( nice, nam(jf), titn(jf), uni(jf), jpi, jpj   &
               &                                  , nhorid, 1, 1, 1, -99, 32, clop, zsto, zout )
         END DO
         CALL histend( nice, snc4set )
         !
      ENDIF
      !                                          !--------------------!
      !                                          !   Cumulate at kt   !
      !                                          !--------------------!

      !-- Store instantaneous values in zcmo
      
      zcmo(:,:, 1:jpnoumax ) = 0.e0 
      DO jj = 2 , jpjm1
         DO ji = 1 , jpim1   ! NO vector opt.
            zindh  = MAX( zzero , SIGN( zone , hicif(ji,jj) * (1.0 - frld(ji,jj) ) - 0.10 ) )
            zinda  = MAX( zzero , SIGN( zone , ( 1.0 - frld(ji,jj) ) - 0.10 ) )
            zindb  = zindh * zinda
            ztmu   = MAX( 0.5 * zone , ( tmu(ji,jj) + tmu(ji+1,jj) + tmu(ji,jj+1) + tmu(ji+1,jj+1) ) ) 
            zcmo(ji,jj,1)  = hsnif (ji,jj)
            zcmo(ji,jj,2)  = hicif (ji,jj)
            zcmo(ji,jj,3)  = hicifp(ji,jj)
            zcmo(ji,jj,4)  = frld  (ji,jj)
            zcmo(ji,jj,5)  = sist  (ji,jj)
            zcmo(ji,jj,6)  = fbif  (ji,jj)
            zcmo(ji,jj,7)  = zindb * (  u_ice(ji,jj  ) * tmu(ji,jj  ) + u_ice(ji+1,jj  ) * tmu(ji+1,jj  )   &
                                      + u_ice(ji,jj+1) * tmu(ji,jj+1) + u_ice(ji+1,jj+1) * tmu(ji+1,jj+1) ) &
                                  / ztmu 

            zcmo(ji,jj,8)  = zindb * (  v_ice(ji,jj  ) * tmu(ji,jj  ) + v_ice(ji+1,jj  ) * tmu(ji+1,jj  )   &
                                      + v_ice(ji,jj+1) * tmu(ji,jj+1) + v_ice(ji+1,jj+1) * tmu(ji+1,jj+1) ) &
                                  / ztmu
            zcmo(ji,jj,9)  = sst_m(ji,jj)
            zcmo(ji,jj,10) = sss_m(ji,jj)
            zcmo(ji,jj,11) = qns(ji,jj) + qsr(ji,jj)
            zcmo(ji,jj,12) = qsr(ji,jj)
            zcmo(ji,jj,13) = qns(ji,jj)
            ! See thersf for the coefficient
            zcmo(ji,jj,14) = - emps(ji,jj) * rday * ( sss_m(ji,jj) + epsi16 ) / soce    !!gm ???
            zcmo(ji,jj,15) = utau_ice(ji,jj)
            zcmo(ji,jj,16) = vtau_ice(ji,jj)
            zcmo(ji,jj,17) = qsr_ice(ji,jj,1)
            zcmo(ji,jj,18) = qns_ice(ji,jj,1)
            zcmo(ji,jj,19) = sprecip(ji,jj)
         END DO
      END DO
      !
      ! Write the netcdf file
      !
      niter = niter + 1
      DO jf = 1 , noumef
         DO jj = 1 , jpj
            DO ji = 1 , jpi
               zfield(ji,jj) = zcmo(ji,jj,jf) * cmulti(jf) + cadd(jf)
            END DO
         END DO
         
         IF( jf == 7  .OR. jf == 8  .OR. jf == 15 .OR. jf == 16 ) THEN
            CALL lbc_lnk( zfield, 'T', -1. )
         ELSE 
            CALL lbc_lnk( zfield, 'T',  1. )
         ENDIF
         
         IF( nc(jf) == 1 )   CALL histwrite( nice, nam(jf), niter, zfield, ndim, ndex51 )
         
      END DO
      
      IF( ( nn_fsbc * niter ) >= nitend )   CALL histclo( nice ) 

      CALL wrk_dealloc( jpi, jpj, zfield )
      !
   END SUBROUTINE lim_wri_2
     
# endif

   SUBROUTINE lim_wri_init_2
      !!-------------------------------------------------------------------
      !!                    ***   ROUTINE lim_wri_init_2  ***
      !!                
      !! ** Purpose :   intialisation of LIM sea-ice output
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
         field_19
      TYPE(FIELD) , DIMENSION(jpnoumax) :: zfield

      NAMELIST/namiceout/ noumef, &
         field_1 , field_2 , field_3 , field_4 , field_5 , field_6 ,   &
         field_7 , field_8 , field_9 , field_10, field_11, field_12,   &
         field_13, field_14, field_15, field_16, field_17, field_18,   &
         field_19
      !!-------------------------------------------------------------------
      !
      IF( lim_wri_alloc_2() /= 0 ) THEN      ! allocate lim_wri arrrays
         CALL ctl_stop( 'STOP', 'lim_wri_init_2 : unable to allocate standard arrays' )   ;   RETURN
      ENDIF

      REWIND ( numnam_ice )                ! Read Namelist namicewri
      READ   ( numnam_ice  , namiceout )
      
      zfield( 1) = field_1
      zfield( 2) = field_2
      zfield( 3) = field_3
      zfield( 4) = field_4
      zfield( 5) = field_5
      zfield( 6) = field_6
      zfield( 7) = field_7
      zfield( 8) = field_8
      zfield( 9) = field_9
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
      
      DO nf = 1, noumef
         titn  (nf) = zfield(nf)%ztitle
         nam   (nf) = zfield(nf)%zname
         uni   (nf) = zfield(nf)%zunit
         nc    (nf) = zfield(nf)%znc
         cmulti(nf) = zfield(nf)%zcmulti
         cadd  (nf) = zfield(nf)%zcadd
      END DO

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'lim_wri_init_2 : Ice parameters for outputs'
         WRITE(numout,*) '~~~~~~~~~~~~~~'
         WRITE(numout,*) '    number of fields to be stored         noumef = ', noumef
         WRITE(numout,*) '           title                            name     unit      Saving (1/0) ',   &
            &            '    multiplicative constant       additive constant '
         DO nf = 1 , noumef         
            WRITE(numout,*) '   ', titn(nf), '   ', nam(nf),'      ', uni(nf),'  ', nc(nf),'        ', cmulti(nf),   &
               &       '        ', cadd(nf)
         END DO
      ENDIF
      !    
   END SUBROUTINE lim_wri_init_2

#endif

   SUBROUTINE lim_wri_state_2( kt, kid, kh_i )
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE lim_wri_state_2  ***
      !!        
      !! ** Purpose :   create a NetCDF file named cdfile_name which contains 
      !!      the instantaneous ice state and forcing fields for ice model
      !!        Used to find errors in the initial state or save the last
      !!      ocean state in case of abnormal end of a simulation
      !!
      !! History :
      !!   2.0  !  2009-06  (B. Lemaire)
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt               ! ocean time-step index)
      INTEGER, INTENT( in ) ::   kid , kh_i       
      !!----------------------------------------------------------------------

      CALL histdef( kid, "isnowthi", "Snow thickness"          , "m"      , jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "iicethic", "Ice thickness"           , "m"      , jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "iiceprod", "Ice produced"            , "m/kt"   , jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "ileadfra", "Ice concentration"       , "-"      , jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "iicetemp", "Ice temperature"         , "K"      , jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "ioceflxb", "flux at ice base"        , "w/m2"   , jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 
      CALL histdef( kid, "iicevelu", "i-Ice speed (I-point)"   , "m/s"    , jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "iicevelv", "j-Ice speed (I-point)"   , "m/s"    , jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 
      CALL histdef( kid, "isstempe", "Sea surface temperature" , "C"      , jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 
      CALL histdef( kid, "isssalin", "Sea surface salinity"    , "PSU"    , jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 
      CALL histdef( kid, "iicestru", "i-Wind stress over ice (I-pt)", "Pa", jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "iicestrv", "j-Wind stress over ice (I-pt)", "Pa", jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 
      CALL histdef( kid, "iicesflx", "Solar flux over ice"     , "w/m2"   , jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 
      CALL histdef( kid, "iicenflx", "Non-solar flux over ice" , "w/m2"   , jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt )
      CALL histdef( kid, "isnowpre", "Snow precipitation"      , "kg/m2/s", jpi, jpj, kh_i, 1, 1, 1, -99, 32, "inst(x)", rdt, rdt ) 

      CALL histend( kid, snc4set )   ! end of the file definition

      CALL histwrite( kid, "isnowthi", kt, hsnif          , jpi*jpj, (/1/) )   
      CALL histwrite( kid, "iicethic", kt, hicif          , jpi*jpj, (/1/) )    
      CALL histwrite( kid, "iiceprod", kt, hicifp         , jpi*jpj, (/1/) )   
      CALL histwrite( kid, "ileadfra", kt, 1. - frld(:,:) , jpi*jpj, (/1/) )
      CALL histwrite( kid, "iicetemp", kt, sist(:,:) - rt0, jpi*jpj, (/1/) )
      CALL histwrite( kid, "ioceflxb", kt, fbif           , jpi*jpj, (/1/) )
      CALL histwrite( kid, "iicevelu", kt, u_ice          , jpi*jpj, (/1/) )
      CALL histwrite( kid, "iicevelv", kt, v_ice          , jpi*jpj, (/1/) )
      CALL histwrite( kid, "isstempe", kt, sst_m          , jpi*jpj, (/1/) )
      CALL histwrite( kid, "isssalin", kt, sss_m          , jpi*jpj, (/1/) )
      CALL histwrite( kid, "iicestru", kt, utau_ice       , jpi*jpj, (/1/) )
      CALL histwrite( kid, "iicestrv", kt, vtau_ice       , jpi*jpj, (/1/) )
      CALL histwrite( kid, "iicesflx", kt, qsr_ice(:,:,1) , jpi*jpj, (/1/) )
      CALL histwrite( kid, "iicenflx", kt, qns_ice(:,:,1) , jpi*jpj, (/1/) )
      CALL histwrite( kid, "isnowpre", kt, sprecip        , jpi*jpj, (/1/) )

    END SUBROUTINE lim_wri_state_2

#else
   !!----------------------------------------------------------------------
   !!   Default option :         Empty module      NO LIM 2.0 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_wri_2          ! Empty routine
   END SUBROUTINE lim_wri_2
#endif

   !!======================================================================
END MODULE limwri_2
