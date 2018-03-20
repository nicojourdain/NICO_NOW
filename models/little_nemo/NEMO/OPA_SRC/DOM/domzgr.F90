MODULE domzgr
   !!==============================================================================
   !!                       ***  MODULE domzgr   ***
   !! Ocean initialization : domain initialization
   !!==============================================================================
   !! History :  OPA  ! 1995-12  (G. Madec)  Original code : s vertical coordinate
   !!                 ! 1997-07  (G. Madec)  lbc_lnk call
   !!                 ! 1997-04  (J.-O. Beismann) 
   !!            8.5  ! 2002-09  (A. Bozec, G. Madec)  F90: Free form and module
   !!             -   ! 2002-09  (A. de Miranda)  rigid-lid + islands
   !!  NEMO      1.0  ! 2003-08  (G. Madec)  F90: Free form and module
   !!             -   ! 2005-10  (A. Beckmann)  modifications for hybrid s-ccordinates & new stretching function
   !!            2.0  ! 2006-04  (R. Benshila, G. Madec)  add zgr_zco
   !!            3.0  ! 2008-06  (G. Madec)  insertion of domzgr_zps.h90 & conding style
   !!            3.2  ! 2009-07  (R. Benshila) Suppression of rigid-lid option
   !!            3.3  ! 2010-11  (G. Madec) add mbk. arrays associated to the deepest ocean level
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_zgr          : defined the ocean vertical coordinate system
   !!       zgr_bat      : bathymetry fields (levels and meters)
   !!       zgr_bat_zoom : modify the bathymetry field if zoom domain
   !!       zgr_bat_ctl  : check the bathymetry files
   !!       zgr_bot_level: deepest ocean level for t-, u, and v-points
   !!       zgr_z        : reference z-coordinate 
   !!       zgr_zco      : z-coordinate 
   !!       zgr_zps      : z-coordinate with partial steps
   !!       zgr_sco      : s-coordinate
   !!       fssig        : sigma coordinate non-dimensional function
   !!       dfssig       : derivative of the sigma coordinate function    !!gm  (currently missing!)
   !!---------------------------------------------------------------------
   USE oce               ! ocean variables
   USE dom_oce           ! ocean domain
   USE closea            ! closed seas
   USE c1d               ! 1D vertical configuration
   USE in_out_manager    ! I/O manager
   USE iom               ! I/O library
   USE lbclnk            ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp           ! distributed memory computing library
   USE wrk_nemo        ! Memory allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_zgr        ! called by dom_init.F90

   !                                       !!* Namelist namzgr_sco *
   REAL(wp) ::   rn_sbot_min =  300._wp     ! minimum depth of s-bottom surface (>0) (m)
   REAL(wp) ::   rn_sbot_max = 5250._wp     ! maximum depth of s-bottom surface (= ocean depth) (>0) (m)
   REAL(wp) ::   rn_theta    =    6.00_wp   ! surface control parameter (0<=rn_theta<=20)
   REAL(wp) ::   rn_thetb    =    0.75_wp   ! bottom control parameter  (0<=rn_thetb<= 1)
   REAL(wp) ::   rn_rmax     =    0.15_wp   ! maximum cut-off r-value allowed (0<rn_rmax<1)
   LOGICAL  ::   ln_s_sigma  = .false.      ! use hybrid s-sigma -coordinate & stretching function fssig1 (ln_sco=T)
   REAL(wp) ::   rn_bb       =    0.80_wp   ! stretching parameter for song and haidvogel stretching
   !                                        ! ( rn_bb=0; top only, rn_bb =1; top and bottom)
   REAL(wp) ::   rn_hc       =  150._wp     ! Critical depth for s-sigma coordinates

  !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3.1 , NEMO Consortium (2011)
   !! $Id: domzgr.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS       

   SUBROUTINE dom_zgr
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE dom_zgr  ***
      !!                   
      !! ** Purpose :  set the depth of model levels and the resulting 
      !!      vertical scale factors.
      !!
      !! ** Method  : - reference 1D vertical coordinate (gdep._0, e3._0)
      !!              - read/set ocean depth and ocean levels (bathy, mbathy)
      !!              - vertical coordinate (gdep., e3.) depending on the 
      !!                coordinate chosen :
      !!                   ln_zco=T   z-coordinate   
      !!                   ln_zps=T   z-coordinate with partial steps
      !!                   ln_zco=T   s-coordinate 
      !!
      !! ** Action  :   define gdep., e3., mbathy and bathy
      !!----------------------------------------------------------------------
      INTEGER ::   ioptio = 0   ! temporary integer
      !
      NAMELIST/namzgr/ ln_zco, ln_zps, ln_sco
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dom_zgr')
      !
      REWIND( numnam )                 ! Read Namelist namzgr : vertical coordinate'
      READ  ( numnam, namzgr )

      IF(lwp) THEN                     ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'dom_zgr : vertical coordinate'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*) '          Namelist namzgr : set vertical coordinate'
         WRITE(numout,*) '             z-coordinate - full steps      ln_zco = ', ln_zco
         WRITE(numout,*) '             z-coordinate - partial steps   ln_zps = ', ln_zps
         WRITE(numout,*) '             s- or hybrid z-s-coordinate    ln_sco = ', ln_sco
      ENDIF

      ioptio = 0                       ! Check Vertical coordinate options
      IF( ln_zco ) ioptio = ioptio + 1
      IF( ln_zps ) ioptio = ioptio + 1
      IF( ln_sco ) ioptio = ioptio + 1
      IF( ioptio /= 1 )   CALL ctl_stop( ' none or several vertical coordinate options used' )
      !
      ! Build the vertical coordinate system
      ! ------------------------------------
                          CALL zgr_z            ! Reference z-coordinate system (always called)
                          CALL zgr_bat          ! Bathymetry fields (levels and meters)
      IF( ln_zco      )   CALL zgr_zco          ! z-coordinate
      IF( ln_zps      )   CALL zgr_zps          ! Partial step z-coordinate
      IF( ln_sco      )   CALL zgr_sco          ! s-coordinate or hybrid z-s coordinate
      !
      ! final adjustment of mbathy & check 
      ! -----------------------------------
      IF( lzoom       )   CALL zgr_bat_zoom     ! correct mbathy in case of zoom subdomain
      IF( .NOT.lk_c1d )   CALL zgr_bat_ctl      ! check bathymetry (mbathy) and suppress isoated ocean points
                          CALL zgr_bot_level    ! deepest ocean level for t-, u- and v-points
      !
      !

      IF( nprint == 1 .AND. lwp )   THEN
         WRITE(numout,*) ' MIN val mbathy ', MINVAL( mbathy(:,:) ), ' MAX ', MAXVAL( mbathy(:,:) )
         WRITE(numout,*) ' MIN val depth t ', MINVAL( fsdept(:,:,:) ),   &
            &                   ' w ',   MINVAL( fsdepw(:,:,:) ), '3w ', MINVAL( fsde3w(:,:,:) )
         WRITE(numout,*) ' MIN val e3    t ', MINVAL( fse3t(:,:,:) ), ' f ', MINVAL( fse3f(:,:,:) ),  &
            &                   ' u ',   MINVAL( fse3u(:,:,:) ), ' u ', MINVAL( fse3v(:,:,:) ),  &
            &                   ' uw',   MINVAL( fse3uw(:,:,:)), ' vw', MINVAL( fse3vw(:,:,:)),   &
            &                   ' w ',   MINVAL( fse3w(:,:,:) )

         WRITE(numout,*) ' MAX val depth t ', MAXVAL( fsdept(:,:,:) ),   &
            &                   ' w ',   MAXVAL( fsdepw(:,:,:) ), '3w ', MAXVAL( fsde3w(:,:,:) )
         WRITE(numout,*) ' MAX val e3    t ', MAXVAL( fse3t(:,:,:) ), ' f ', MAXVAL( fse3f(:,:,:) ),  &
            &                   ' u ',   MAXVAL( fse3u(:,:,:) ), ' u ', MAXVAL( fse3v(:,:,:) ),  &
            &                   ' uw',   MAXVAL( fse3uw(:,:,:)), ' vw', MAXVAL( fse3vw(:,:,:)),   &
            &                   ' w ',   MAXVAL( fse3w(:,:,:) )
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('dom_zgr')
      !
   END SUBROUTINE dom_zgr


   SUBROUTINE zgr_z
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE zgr_z  ***
      !!                   
      !! ** Purpose :   set the depth of model levels and the resulting 
      !!      vertical scale factors.
      !!
      !! ** Method  :   z-coordinate system (use in all type of coordinate)
      !!        The depth of model levels is defined from an analytical
      !!      function the derivative of which gives the scale factors.
      !!        both depth and scale factors only depend on k (1d arrays).
      !!              w-level: gdepw_0  = fsdep(k)
      !!                       e3w_0(k) = dk(fsdep)(k)     = fse3(k)
      !!              t-level: gdept_0  = fsdep(k+0.5)
      !!                       e3t_0(k) = dk(fsdep)(k+0.5) = fse3(k+0.5)
      !!
      !! ** Action  : - gdept_0, gdepw_0 : depth of T- and W-point (m)
      !!              - e3t_0  , e3w_0   : scale factors at T- and W-levels (m)
      !!
      !! Reference : Marti, Madec & Delecluse, 1992, JGR, 97, No8, 12,763-12,766.
      !!----------------------------------------------------------------------
      INTEGER  ::   jk                     ! dummy loop indices
      REAL(wp) ::   zt, zw                 ! temporary scalars
      REAL(wp) ::   zsur, za0, za1, zkth   ! Values set from parameters in
      REAL(wp) ::   zacr, zdzmin, zhmax    ! par_CONFIG_Rxx.h90
      REAL(wp) ::   zrefdep                ! depth of the reference level (~10m)
      REAL(wp) ::   za2, zkth2, zacr2      ! Values for optional double tanh function set from parameters 
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zgr_z')
      !
      ! Set variables from parameters
      ! ------------------------------
       zkth = ppkth       ;   zacr = ppacr
       zdzmin = ppdzmin   ;   zhmax = pphmax
       zkth2 = ppkth2     ;   zacr2 = ppacr2   ! optional (ldbletanh=T) double tanh parameters

      ! If ppa1 and ppa0 and ppsur are et to pp_to_be_computed
      !  za0, za1, zsur are computed from ppdzmin , pphmax, ppkth, ppacr
      IF(   ppa1  == pp_to_be_computed  .AND.  &
         &  ppa0  == pp_to_be_computed  .AND.  &
         &  ppsur == pp_to_be_computed           ) THEN
         !
         za1  = (  ppdzmin - pphmax / FLOAT(jpkm1)  )                                                      &
            & / ( TANH((1-ppkth)/ppacr) - ppacr/FLOAT(jpk-1) * (  LOG( COSH( (jpk - ppkth) / ppacr) )      &
            &                                                   - LOG( COSH( ( 1  - ppkth) / ppacr) )  )  )
         za0  = ppdzmin - za1 *              TANH( (1-ppkth) / ppacr )
         zsur =   - za0 - za1 * ppacr * LOG( COSH( (1-ppkth) / ppacr )  )
      ELSE
         za1 = ppa1 ;       za0 = ppa0 ;          zsur = ppsur
         za2 = ppa2                            ! optional (ldbletanh=T) double tanh parameter
      ENDIF

      IF(lwp) THEN                         ! Parameter print
         WRITE(numout,*)
         WRITE(numout,*) '    zgr_z   : Reference vertical z-coordinates'
         WRITE(numout,*) '    ~~~~~~~'
         IF(  ppkth == 0._wp ) THEN              
              WRITE(numout,*) '            Uniform grid with ',jpk-1,' layers'
              WRITE(numout,*) '            Total depth    :', zhmax
              WRITE(numout,*) '            Layer thickness:', zhmax/(jpk-1)
         ELSE
            IF( ppa1 == 0._wp .AND. ppa0 == 0._wp .AND. ppsur == 0._wp ) THEN
               WRITE(numout,*) '         zsur, za0, za1 computed from '
               WRITE(numout,*) '                 zdzmin = ', zdzmin
               WRITE(numout,*) '                 zhmax  = ', zhmax
            ENDIF
            WRITE(numout,*) '           Value of coefficients for vertical mesh:'
            WRITE(numout,*) '                 zsur = ', zsur
            WRITE(numout,*) '                 za0  = ', za0
            WRITE(numout,*) '                 za1  = ', za1
            WRITE(numout,*) '                 zkth = ', zkth
            WRITE(numout,*) '                 zacr = ', zacr
            IF( ldbletanh ) THEN
               WRITE(numout,*) ' (Double tanh    za2  = ', za2
               WRITE(numout,*) '  parameters)    zkth2= ', zkth2
               WRITE(numout,*) '                 zacr2= ', zacr2
            ENDIF
         ENDIF
      ENDIF


      ! Reference z-coordinate (depth - scale factor at T- and W-points)
      ! ======================
      IF( ppkth == 0._wp ) THEN            !  uniform vertical grid       
         za1 = zhmax / FLOAT(jpk-1) 
         DO jk = 1, jpk
            zw = FLOAT( jk )
            zt = FLOAT( jk ) + 0.5_wp
            gdepw_0(jk) = ( zw - 1 ) * za1
            gdept_0(jk) = ( zt - 1 ) * za1
            e3w_0  (jk) =  za1
            e3t_0  (jk) =  za1
         END DO
      ELSE                                ! Madec & Imbard 1996 function
         IF( .NOT. ldbletanh ) THEN
            DO jk = 1, jpk
               zw = REAL( jk , wp )
               zt = REAL( jk , wp ) + 0.5_wp
               gdepw_0(jk) = ( zsur + za0 * zw + za1 * zacr * LOG ( COSH( (zw-zkth) / zacr ) )  )
               gdept_0(jk) = ( zsur + za0 * zt + za1 * zacr * LOG ( COSH( (zt-zkth) / zacr ) )  )
               e3w_0  (jk) =          za0      + za1        * TANH(       (zw-zkth) / zacr   )
               e3t_0  (jk) =          za0      + za1        * TANH(       (zt-zkth) / zacr   )
            END DO
         ELSE
            DO jk = 1, jpk
               zw = FLOAT( jk )
               zt = FLOAT( jk ) + 0.5_wp
               ! Double tanh function
               gdepw_0(jk) = ( zsur + za0 * zw + za1 * zacr * LOG ( COSH( (zw-zkth ) / zacr  ) )    &
                  &                            + za2 * zacr2* LOG ( COSH( (zw-zkth2) / zacr2 ) )  )
               gdept_0(jk) = ( zsur + za0 * zt + za1 * zacr * LOG ( COSH( (zt-zkth ) / zacr  ) )    &
                  &                            + za2 * zacr2* LOG ( COSH( (zt-zkth2) / zacr2 ) )  )
               e3w_0  (jk) =          za0      + za1        * TANH(       (zw-zkth ) / zacr  )    &
                  &                            + za2        * TANH(       (zw-zkth2) / zacr2 )
               e3t_0  (jk) =          za0      + za1        * TANH(       (zt-zkth ) / zacr  )    &
                  &                            + za2        * TANH(       (zt-zkth2) / zacr2 )
            END DO
         ENDIF
         gdepw_0(1) = 0._wp                    ! force first w-level to be exactly at zero
      ENDIF

!!gm BUG in s-coordinate this does not work!
      ! deepest/shallowest W level Above/Below ~10m
      zrefdep = 10._wp - 0.1_wp * MINVAL( e3w_0 )                    ! ref. depth with tolerance (10% of minimum layer thickness)
      nlb10 = MINLOC( gdepw_0, mask = gdepw_0 > zrefdep, dim = 1 )   ! shallowest W level Below ~10m
      nla10 = nlb10 - 1                                              ! deepest    W level Above ~10m
!!gm end bug

      IF(lwp) THEN                        ! control print
         WRITE(numout,*)
         WRITE(numout,*) '              Reference z-coordinate depth and scale factors:'
         WRITE(numout, "(9x,' level   gdept    gdepw     e3t      e3w  ')" )
         WRITE(numout, "(10x, i4, 4f9.2)" ) ( jk, gdept_0(jk), gdepw_0(jk), e3t_0(jk), e3w_0(jk), jk = 1, jpk )
      ENDIF
      DO jk = 1, jpk                      ! control positivity
         IF( e3w_0  (jk) <= 0._wp .OR. e3t_0  (jk) <= 0._wp )   CALL ctl_stop( 'dom:zgr_z: e3w or e3t =< 0 '    )
         IF( gdepw_0(jk) <  0._wp .OR. gdept_0(jk) <  0._wp )   CALL ctl_stop( 'dom:zgr_z: gdepw or gdept < 0 ' )
      END DO
      !
      IF( nn_timing == 1 )  CALL timing_stop('zgr_z')
      !
   END SUBROUTINE zgr_z


   SUBROUTINE zgr_bat
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE zgr_bat  ***
      !! 
      !! ** Purpose :   set bathymetry both in levels and meters
      !!
      !! ** Method  :   read or define mbathy and bathy arrays
      !!       * level bathymetry:
      !!      The ocean basin geometry is given by a two-dimensional array,
      !!      mbathy, which is defined as follow :
      !!            mbathy(ji,jj) = 1, ..., jpk-1, the number of ocean level
      !!                              at t-point (ji,jj).
      !!                            = 0  over the continental t-point.
      !!      The array mbathy is checked to verified its consistency with
      !!      model option. in particular:
      !!            mbathy must have at least 1 land grid-points (mbathy<=0)
      !!                  along closed boundary.
      !!            mbathy must be cyclic IF jperio=1.
      !!            mbathy must be lower or equal to jpk-1.
      !!            isolated ocean grid points are suppressed from mbathy
      !!                  since they are only connected to remaining
      !!                  ocean through vertical diffusion.
      !!      ntopo=-1 :   rectangular channel or bassin with a bump 
      !!      ntopo= 0 :   flat rectangular channel or basin 
      !!      ntopo= 1 :   mbathy is read in 'bathy_level.nc' NetCDF file
      !!                   bathy  is read in 'bathy_meter.nc' NetCDF file
      !!
      !! ** Action  : - mbathy: level bathymetry (in level index)
      !!              - bathy : meter bathymetry (in meters)
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, jl, jk            ! dummy loop indices
      INTEGER  ::   inum                      ! temporary logical unit
      INTEGER  ::   ii_bump, ij_bump, ih      ! bump center position
      INTEGER  ::   ii0, ii1, ij0, ij1, ik    ! local indices
      REAL(wp) ::   r_bump , h_bump , h_oce   ! bump characteristics 
      REAL(wp) ::   zi, zj, zh, zhmin         ! local scalars
      INTEGER , POINTER, DIMENSION(:,:) ::   idta   ! global domain integer data
      REAL(wp), POINTER, DIMENSION(:,:) ::   zdta   ! global domain scalar data
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zgr_bat')
      !
      CALL wrk_alloc( jpidta, jpjdta, idta )
      CALL wrk_alloc( jpidta, jpjdta, zdta )
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '    zgr_bat : defines level and meter bathymetry'
      IF(lwp) WRITE(numout,*) '    ~~~~~~~'

      !                                               ! ================== ! 
      IF( ntopo == 0 .OR. ntopo == -1 ) THEN          !   defined by hand  !
         !                                            ! ================== !
         !                                            ! global domain level and meter bathymetry (idta,zdta)
         !
         IF( ntopo == 0 ) THEN                        ! flat basin
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) '         bathymetry field: flat basin'
            idta(:,:) = jpkm1                            ! before last level
            zdta(:,:) = gdepw_0(jpk)                     ! last w-point depth
            h_oce     = gdepw_0(jpk)
         ELSE                                         ! bump centered in the basin
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) '         bathymetry field: flat basin with a bump'
            ii_bump = jpidta / 2                           ! i-index of the bump center
            ij_bump = jpjdta / 2                           ! j-index of the bump center
            r_bump  = 50000._wp                            ! bump radius (meters)       
            h_bump  =  2700._wp                            ! bump height (meters)
            h_oce   = gdepw_0(jpk)                         ! background ocean depth (meters)
            IF(lwp) WRITE(numout,*) '            bump characteristics: '
            IF(lwp) WRITE(numout,*) '               bump center (i,j)   = ', ii_bump, ii_bump
            IF(lwp) WRITE(numout,*) '               bump height         = ', h_bump , ' meters'
            IF(lwp) WRITE(numout,*) '               bump radius         = ', r_bump , ' index'
            IF(lwp) WRITE(numout,*) '            background ocean depth = ', h_oce  , ' meters'
            !                                        
            DO jj = 1, jpjdta                              ! zdta :
               DO ji = 1, jpidta
                  zi = FLOAT( ji - ii_bump ) * ppe1_m / r_bump
                  zj = FLOAT( jj - ij_bump ) * ppe2_m / r_bump
                  zdta(ji,jj) = h_oce - h_bump * EXP( -( zi*zi + zj*zj ) )
               END DO
            END DO
            !                                              ! idta :
            IF( ln_sco ) THEN                                   ! s-coordinate (zsc       ): idta()=jpk
               idta(:,:) = jpkm1
            ELSE                                                ! z-coordinate (zco or zps): step-like topography
               idta(:,:) = jpkm1
               DO jk = 1, jpkm1
                  WHERE( gdept_0(jk) < zdta(:,:) .AND. zdta(:,:) <= gdept_0(jk+1) )   idta(:,:) = jk
               END DO
            ENDIF
         ENDIF
         !                                            ! set GLOBAL boundary conditions 
         !                                            ! Caution : idta on the global domain: use of jperio, not nperio
         IF( jperio == 1 .OR. jperio == 4 .OR. jperio == 6 ) THEN
            idta( :    , 1    ) = -1                ;      zdta( :    , 1    ) = -1._wp
            idta( :    ,jpjdta) =  0                ;      zdta( :    ,jpjdta) =  0._wp
         ELSEIF( jperio == 2 ) THEN
            idta( :    , 1    ) = idta( : ,  3  )   ;      zdta( :    , 1    ) = zdta( : ,  3  )
            idta( :    ,jpjdta) = 0                 ;      zdta( :    ,jpjdta) =  0._wp
            idta( 1    , :    ) = 0                 ;      zdta( 1    , :    ) =  0._wp
            idta(jpidta, :    ) = 0                 ;      zdta(jpidta, :    ) =  0._wp
         ELSE
            ih = 0                                  ;      zh = 0._wp
            IF( ln_sco )   ih = jpkm1               ;      IF( ln_sco )   zh = h_oce
            idta( :    , 1    ) = ih                ;      zdta( :    , 1    ) =  zh
            idta( :    ,jpjdta) = ih                ;      zdta( :    ,jpjdta) =  zh
            idta( 1    , :    ) = ih                ;      zdta( 1    , :    ) =  zh
            idta(jpidta, :    ) = ih                ;      zdta(jpidta, :    ) =  zh
         ENDIF

         !                                            ! local domain level and meter bathymetries (mbathy,bathy)
         mbathy(:,:) = 0                                   ! set to zero extra halo points
         bathy (:,:) = 0._wp                               ! (require for mpp case)
         DO jj = 1, nlcj                                   ! interior values
            DO ji = 1, nlci
               mbathy(ji,jj) = idta( mig(ji), mjg(jj) )
               bathy (ji,jj) = zdta( mig(ji), mjg(jj) )
            END DO
         END DO
         !
         !                                            ! ================ !
      ELSEIF( ntopo == 1 ) THEN                       !   read in file   ! (over the local domain)
         !                                            ! ================ !
         !
         IF( ln_zco )   THEN                          ! zco : read level bathymetry 
            CALL iom_open ( 'bathy_level.nc', inum )  
            CALL iom_get  ( inum, jpdom_data, 'Bathy_level', bathy )
            CALL iom_close( inum )
            mbathy(:,:) = INT( bathy(:,:) )
            !                                                ! =====================
            IF( cp_cfg == "orca" .AND. jp_cfg == 2 ) THEN    ! ORCA R2 configuration
               !                                             ! =====================
               IF( nn_cla == 0 ) THEN
                  ii0 = 140   ;   ii1 = 140                  ! Gibraltar Strait open 
                  ij0 = 102   ;   ij1 = 102                  ! (Thomson, Ocean Modelling, 1995)
                  DO ji = mi0(ii0), mi1(ii1)
                     DO jj = mj0(ij0), mj1(ij1)
                        mbathy(ji,jj) = 15
                     END DO
                  END DO
                  IF(lwp) WRITE(numout,*)
                  IF(lwp) WRITE(numout,*) '      orca_r2: Gibraltar strait open at i=',ii0,' j=',ij0
                  !
                  ii0 = 160   ;   ii1 = 160                  ! Bab el mandeb Strait open
                  ij0 = 88    ;   ij1 = 88                   ! (Thomson, Ocean Modelling, 1995)
                  DO ji = mi0(ii0), mi1(ii1)
                     DO jj = mj0(ij0), mj1(ij1)
                        mbathy(ji,jj) = 12
                     END DO
                  END DO
                  IF(lwp) WRITE(numout,*)
                  IF(lwp) WRITE(numout,*) '      orca_r2: Bab el Mandeb strait open at i=',ii0,' j=',ij0
               ENDIF
               !
            ENDIF
            !
         ENDIF
         IF( ln_zps .OR. ln_sco )   THEN              ! zps or sco : read meter bathymetry
            CALL iom_open ( 'bathy_meter.nc', inum ) 
            CALL iom_get  ( inum, jpdom_data, 'Bathymetry', bathy )
            CALL iom_close( inum )
            !                                                ! =====================
            IF( cp_cfg == "orca" .AND. jp_cfg == 2 ) THEN    ! ORCA R2 configuration
               !                                             ! =====================
              IF( nn_cla == 0 ) THEN
                 ii0 = 140   ;   ii1 = 140                   ! Gibraltar Strait open 
                 ij0 = 102   ;   ij1 = 102                   ! (Thomson, Ocean Modelling, 1995)
                 DO ji = mi0(ii0), mi1(ii1)
                    DO jj = mj0(ij0), mj1(ij1)
                       bathy(ji,jj) = 284._wp
                    END DO
                 END DO
                 IF(lwp) WRITE(numout,*)
                 IF(lwp) WRITE(numout,*) '      orca_r2: Gibraltar strait open at i=',ii0,' j=',ij0
                 !
                 ii0 = 160   ;   ii1 = 160                   ! Bab el mandeb Strait open
                 ij0 = 88    ;   ij1 = 88                    ! (Thomson, Ocean Modelling, 1995)
                 DO ji = mi0(ii0), mi1(ii1)
                    DO jj = mj0(ij0), mj1(ij1)
                       bathy(ji,jj) = 137._wp
                    END DO
                 END DO
                 IF(lwp) WRITE(numout,*)
                 IF(lwp) WRITE(numout,*) '             orca_r2: Bab el Mandeb strait open at i=',ii0,' j=',ij0
              ENDIF
              !
           ENDIF
            !
        ENDIF
         !                                            ! =============== !
      ELSE                                            !      error      !
         !                                            ! =============== !
         WRITE(ctmp1,*) 'parameter , ntopo = ', ntopo
         CALL ctl_stop( '    zgr_bat : '//trim(ctmp1) )
      ENDIF
      !
      !                                               ! =========================== !
      IF( nclosea == 0 ) THEN                         !   NO closed seas or lakes   !
         DO jl = 1, jpncs                             ! =========================== !
            DO jj = ncsj1(jl), ncsj2(jl)
               DO ji = ncsi1(jl), ncsi2(jl)
                  mbathy(ji,jj) = 0                   ! suppress closed seas and lakes from bathymetry
                  bathy (ji,jj) = 0._wp               
               END DO
            END DO
         END DO
      ENDIF
      !
      !                                               ! =========================== !
      !                                               !     set a minimum depth     !
      !                                               ! =========================== !
      IF ( .not. ln_sco ) THEN
         IF( rn_hmin < 0._wp ) THEN    ;   ik = - INT( rn_hmin )                                      ! from a nb of level
         ELSE                          ;   ik = MINLOC( gdepw_0, mask = gdepw_0 > rn_hmin, dim = 1 )  ! from a depth
         ENDIF
         zhmin = gdepw_0(ik+1)                                                         ! minimum depth = ik+1 w-levels 
         WHERE( bathy(:,:) <= 0._wp )   ;   bathy(:,:) = 0._wp                         ! min=0     over the lands
         ELSE WHERE                     ;   bathy(:,:) = MAX(  zhmin , bathy(:,:)  )   ! min=zhmin over the oceans
         END WHERE
         IF(lwp) write(numout,*) 'Minimum ocean depth: ', zhmin, ' minimum number of ocean levels : ', ik
      ENDIF
      !
      CALL wrk_dealloc( jpidta, jpjdta, idta )
      CALL wrk_dealloc( jpidta, jpjdta, zdta )
      !
      IF( nn_timing == 1 )  CALL timing_stop('zgr_bat')
      !
   END SUBROUTINE zgr_bat


   SUBROUTINE zgr_bat_zoom
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE zgr_bat_zoom  ***
      !!
      !! ** Purpose : - Close zoom domain boundary if necessary
      !!              - Suppress Med Sea from ORCA R2 and R05 arctic zoom
      !!
      !! ** Method  : 
      !!
      !! ** Action  : - update mbathy: level bathymetry (in level index)
      !!----------------------------------------------------------------------
      INTEGER ::   ii0, ii1, ij0, ij1   ! temporary integers
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '    zgr_bat_zoom : modify the level bathymetry for zoom domain'
      IF(lwp) WRITE(numout,*) '    ~~~~~~~~~~~~'
      !
      ! Zoom domain
      ! ===========
      !
      ! Forced closed boundary if required
      IF( lzoom_s )   mbathy(  : , mj0(jpjzoom):mj1(jpjzoom) )      = 0
      IF( lzoom_w )   mbathy(      mi0(jpizoom):mi1(jpizoom) , :  ) = 0
      IF( lzoom_e )   mbathy(      mi0(jpiglo+jpizoom-1):mi1(jpiglo+jpizoom-1) , :  ) = 0
      IF( lzoom_n )   mbathy(  : , mj0(jpjglo+jpjzoom-1):mj1(jpjglo+jpjzoom-1) )      = 0
      !
      ! Configuration specific domain modifications
      ! (here, ORCA arctic configuration: suppress Med Sea)
      IF( cp_cfg == "orca" .AND. lzoom_arct ) THEN
         SELECT CASE ( jp_cfg )
         !                                        ! =======================
         CASE ( 2 )                               !  ORCA_R2 configuration
            !                                     ! =======================
            IF(lwp) WRITE(numout,*) '                   ORCA R2 arctic zoom: suppress the Med Sea'
            ii0 = 141   ;   ii1 = 162      ! Sea box i,j indices
            ij0 =  98   ;   ij1 = 110
            !                                     ! =======================
         CASE ( 05 )                              !  ORCA_R05 configuration
            !                                     ! =======================
            IF(lwp) WRITE(numout,*) '                   ORCA R05 arctic zoom: suppress the Med Sea'
            ii0 = 563   ;   ii1 = 642      ! zero over the Med Sea boxe
            ij0 = 314   ;   ij1 = 370 
         END SELECT
         !
         mbathy( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 0   ! zero over the Med Sea boxe
         !
      ENDIF
      !
   END SUBROUTINE zgr_bat_zoom


   SUBROUTINE zgr_bat_ctl
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE zgr_bat_ctl  ***
      !!
      !! ** Purpose :   check the bathymetry in levels
      !!
      !! ** Method  :   The array mbathy is checked to verified its consistency
      !!      with the model options. in particular:
      !!            mbathy must have at least 1 land grid-points (mbathy<=0)
      !!                  along closed boundary.
      !!            mbathy must be cyclic IF jperio=1.
      !!            mbathy must be lower or equal to jpk-1.
      !!            isolated ocean grid points are suppressed from mbathy
      !!                  since they are only connected to remaining
      !!                  ocean through vertical diffusion.
      !!      C A U T I O N : mbathy will be modified during the initializa-
      !!      tion phase to become the number of non-zero w-levels of a water
      !!      column, with a minimum value of 1.
      !!
      !! ** Action  : - update mbathy: level bathymetry (in level index)
      !!              - update bathy : meter bathymetry (in meters)
      !!----------------------------------------------------------------------
      !!
      INTEGER ::   ji, jj, jl                    ! dummy loop indices
      INTEGER ::   icompt, ibtest, ikmax         ! temporary integers
      REAL(wp), POINTER, DIMENSION(:,:) ::  zbathy
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zgr_bat_ctl')
      !
      CALL wrk_alloc( jpi, jpj, zbathy )
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '    zgr_bat_ctl : check the bathymetry'
      IF(lwp) WRITE(numout,*) '    ~~~~~~~~~~~'

      !                                          ! Suppress isolated ocean grid points
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*)'                   suppress isolated ocean grid points'
      IF(lwp) WRITE(numout,*)'                   -----------------------------------'
      icompt = 0
      DO jl = 1, 2
         IF( nperio == 1 .OR. nperio  ==  4 .OR. nperio  ==  6 ) THEN
            mbathy( 1 ,:) = mbathy(jpim1,:)           ! local domain is cyclic east-west
            mbathy(jpi,:) = mbathy(  2  ,:)
         ENDIF
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               ibtest = MAX(  mbathy(ji-1,jj), mbathy(ji+1,jj),   &
                  &           mbathy(ji,jj-1), mbathy(ji,jj+1)  )
               IF( ibtest < mbathy(ji,jj) ) THEN
                  IF(lwp) WRITE(numout,*) ' the number of ocean level at ',   &
                     &   'grid-point (i,j) =  ',ji,jj,' is changed from ', mbathy(ji,jj),' to ', ibtest
                  mbathy(ji,jj) = ibtest
                  icompt = icompt + 1
               ENDIF
            END DO
         END DO
      END DO
      IF( icompt == 0 ) THEN
         IF(lwp) WRITE(numout,*)'     no isolated ocean grid points'
      ELSE
         IF(lwp) WRITE(numout,*)'    ',icompt,' ocean grid points suppressed'
      ENDIF
      IF( lk_mpp ) THEN
         zbathy(:,:) = FLOAT( mbathy(:,:) )
         CALL lbc_lnk( zbathy, 'T', 1._wp )
         mbathy(:,:) = INT( zbathy(:,:) )
      ENDIF

      !                                          ! East-west cyclic boundary conditions
      IF( nperio == 0 ) THEN
         IF(lwp) WRITE(numout,*) ' mbathy set to 0 along east and west boundary: nperio = ', nperio
         IF( lk_mpp ) THEN
            IF( nbondi == -1 .OR. nbondi == 2 ) THEN
               IF( jperio /= 1 )   mbathy(1,:) = 0
            ENDIF
            IF( nbondi == 1 .OR. nbondi == 2 ) THEN
               IF( jperio /= 1 )   mbathy(nlci,:) = 0
            ENDIF
         ELSE
            IF( ln_zco .OR. ln_zps ) THEN
               mbathy( 1 ,:) = 0
               mbathy(jpi,:) = 0
            ELSE
               mbathy( 1 ,:) = jpkm1
               mbathy(jpi,:) = jpkm1
            ENDIF
         ENDIF
      ELSEIF( nperio == 1 .OR. nperio == 4 .OR. nperio ==  6 ) THEN
         IF(lwp) WRITE(numout,*)' east-west cyclic boundary conditions on mbathy: nperio = ', nperio
         mbathy( 1 ,:) = mbathy(jpim1,:)
         mbathy(jpi,:) = mbathy(  2  ,:)
      ELSEIF( nperio == 2 ) THEN
         IF(lwp) WRITE(numout,*) '   equatorial boundary conditions on mbathy: nperio = ', nperio
      ELSE
         IF(lwp) WRITE(numout,*) '    e r r o r'
         IF(lwp) WRITE(numout,*) '    parameter , nperio = ', nperio
         !         STOP 'dom_mba'
      ENDIF

      !  Boundary condition on mbathy
      IF( .NOT.lk_mpp ) THEN 
!!gm     !!bug ???  think about it !
         !   ... mono- or macro-tasking: T-point, >0, 2D array, no slab
         zbathy(:,:) = FLOAT( mbathy(:,:) )
         CALL lbc_lnk( zbathy, 'T', 1._wp )
         mbathy(:,:) = INT( zbathy(:,:) )
      ENDIF

      ! Number of ocean level inferior or equal to jpkm1
      ikmax = 0
      DO jj = 1, jpj
         DO ji = 1, jpi
            ikmax = MAX( ikmax, mbathy(ji,jj) )
         END DO
      END DO
!!gm  !!! test to do:   ikmax = MAX( mbathy(:,:) )   ???
      IF( ikmax > jpkm1 ) THEN
         IF(lwp) WRITE(numout,*) ' maximum number of ocean level = ', ikmax,' >  jpk-1'
         IF(lwp) WRITE(numout,*) ' change jpk to ',ikmax+1,' to use the exact ead bathymetry'
      ELSE IF( ikmax < jpkm1 ) THEN
         IF(lwp) WRITE(numout,*) ' maximum number of ocean level = ', ikmax,' < jpk-1' 
         IF(lwp) WRITE(numout,*) ' you can decrease jpk to ', ikmax+1
      ENDIF

      IF( lwp .AND. nprint == 1 ) THEN      ! control print
         WRITE(numout,*)
         WRITE(numout,*) ' bathymetric field :   number of non-zero T-levels '
         WRITE(numout,*) ' ------------------'
         CALL prihin( mbathy, jpi, jpj, 1, jpi, 1, 1, jpj, 1, 3, numout )
         WRITE(numout,*)
      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj, zbathy )
      !
      IF( nn_timing == 1 )  CALL timing_stop('zgr_bat_ctl')
      !
   END SUBROUTINE zgr_bat_ctl


   SUBROUTINE zgr_bot_level
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE zgr_bot_level  ***
      !!
      !! ** Purpose :   defines the vertical index of ocean bottom (mbk. arrays)
      !!
      !! ** Method  :   computes from mbathy with a minimum value of 1 over land
      !!
      !! ** Action  :   mbkt, mbku, mbkv :   vertical indices of the deeptest 
      !!                                     ocean level at t-, u- & v-points
      !!                                     (min value = 1 over land)
      !!----------------------------------------------------------------------
      !!
      INTEGER ::   ji, jj   ! dummy loop indices
      REAL(wp), POINTER, DIMENSION(:,:) ::  zmbk
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zgr_bot_level')
      !
      CALL wrk_alloc( jpi, jpj, zmbk )
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '    zgr_bot_level : ocean bottom k-index of T-, U-, V- and W-levels '
      IF(lwp) WRITE(numout,*) '    ~~~~~~~~~~~~~'
      !
      mbkt(:,:) = MAX( mbathy(:,:) , 1 )    ! bottom k-index of T-level (=1 over land)
      !                                     ! bottom k-index of W-level = mbkt+1
      DO jj = 1, jpjm1                      ! bottom k-index of u- (v-) level
         DO ji = 1, jpim1
            mbku(ji,jj) = MIN(  mbkt(ji+1,jj  ) , mbkt(ji,jj)  )
            mbkv(ji,jj) = MIN(  mbkt(ji  ,jj+1) , mbkt(ji,jj)  )
         END DO
      END DO
      ! converte into REAL to use lbc_lnk ; impose a min value of 1 as a zero can be set in lbclnk 
      zmbk(:,:) = REAL( mbku(:,:), wp )   ;   CALL lbc_lnk(zmbk,'U',1.)   ;   mbku  (:,:) = MAX( INT( zmbk(:,:) ), 1 )
      zmbk(:,:) = REAL( mbkv(:,:), wp )   ;   CALL lbc_lnk(zmbk,'V',1.)   ;   mbkv  (:,:) = MAX( INT( zmbk(:,:) ), 1 )
      !
      CALL wrk_dealloc( jpi, jpj, zmbk )
      !
      IF( nn_timing == 1 )  CALL timing_stop('zgr_bot_level')
      !
   END SUBROUTINE zgr_bot_level


   SUBROUTINE zgr_zco
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zgr_zco  ***
      !!
      !! ** Purpose :   define the z-coordinate system
      !!
      !! ** Method  :   set 3D coord. arrays to reference 1D array 
      !!----------------------------------------------------------------------
      INTEGER  ::   jk
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zgr_zco')
      !
      DO jk = 1, jpk
            gdept(:,:,jk) = gdept_0(jk)
            gdepw(:,:,jk) = gdepw_0(jk)
            gdep3w(:,:,jk) = gdepw_0(jk)
            e3t (:,:,jk) = e3t_0(jk)
            e3u (:,:,jk) = e3t_0(jk)
            e3v (:,:,jk) = e3t_0(jk)
            e3f (:,:,jk) = e3t_0(jk)
            e3w (:,:,jk) = e3w_0(jk)
            e3uw(:,:,jk) = e3w_0(jk)
            e3vw(:,:,jk) = e3w_0(jk)
      END DO
      !
      IF( nn_timing == 1 )  CALL timing_stop('zgr_zco')
      !
   END SUBROUTINE zgr_zco


   SUBROUTINE zgr_zps
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zgr_zps  ***
      !!                     
      !! ** Purpose :   the depth and vertical scale factor in partial step
      !!      z-coordinate case
      !!
      !! ** Method  :   Partial steps : computes the 3D vertical scale factors
      !!      of T-, U-, V-, W-, UW-, VW and F-points that are associated with
      !!      a partial step representation of bottom topography.
      !!
      !!        The reference depth of model levels is defined from an analytical
      !!      function the derivative of which gives the reference vertical
      !!      scale factors.
      !!        From  depth and scale factors reference, we compute there new value
      !!      with partial steps  on 3d arrays ( i, j, k ).
      !!
      !!              w-level: gdepw(i,j,k)  = fsdep(k)
      !!                       e3w(i,j,k) = dk(fsdep)(k)     = fse3(i,j,k)
      !!              t-level: gdept(i,j,k)  = fsdep(k+0.5)
      !!                       e3t(i,j,k) = dk(fsdep)(k+0.5) = fse3(i,j,k+0.5)
      !!
      !!        With the help of the bathymetric file ( bathymetry_depth_ORCA_R2.nc),
      !!      we find the mbathy index of the depth at each grid point.
      !!      This leads us to three cases:
      !!
      !!              - bathy = 0 => mbathy = 0
      !!              - 1 < mbathy < jpkm1    
      !!              - bathy > gdepw(jpk) => mbathy = jpkm1  
      !!
      !!        Then, for each case, we find the new depth at t- and w- levels
      !!      and the new vertical scale factors at t-, u-, v-, w-, uw-, vw- 
      !!      and f-points.
      !! 
      !!        This routine is given as an example, it must be modified
      !!      following the user s desiderata. nevertheless, the output as
      !!      well as the way to compute the model levels and scale factors
      !!      must be respected in order to insure second order accuracy
      !!      schemes.
      !!
      !!         c a u t i o n : gdept_0, gdepw_0 and e3._0 are positives
      !!         - - - - - - -   gdept, gdepw and e3. are positives
      !!      
      !!  Reference :   Pacanowsky & Gnanadesikan 1997, Mon. Wea. Rev., 126, 3248-3270.
      !!----------------------------------------------------------------------
      !!
      INTEGER  ::   ji, jj, jk       ! dummy loop indices
      INTEGER  ::   ik, it           ! temporary integers
      LOGICAL  ::   ll_print         ! Allow  control print for debugging
      REAL(wp) ::   ze3tp , ze3wp    ! Last ocean level thickness at T- and W-points
      REAL(wp) ::   zdepwp, zdepth   ! Ajusted ocean depth to avoid too small e3t
      REAL(wp) ::   zmax             ! Maximum depth
      REAL(wp) ::   zdiff            ! temporary scalar
      REAL(wp) ::   zrefdep          ! temporary scalar
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  zprt
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zgr_zps')
      !
      CALL wrk_alloc( jpi, jpj, jpk, zprt )
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '    zgr_zps : z-coordinate with partial steps'
      IF(lwp) WRITE(numout,*) '    ~~~~~~~ '
      IF(lwp) WRITE(numout,*) '              mbathy is recomputed : bathy_level file is NOT used'

      ll_print = .FALSE.                   ! Local variable for debugging
      
      IF(lwp .AND. ll_print) THEN          ! control print of the ocean depth
         WRITE(numout,*)
         WRITE(numout,*) 'dom_zgr_zps:  bathy (in hundred of meters)'
         CALL prihre( bathy, jpi, jpj, 1,jpi, 1, 1, jpj, 1, 1.e-2, numout )
      ENDIF


      ! bathymetry in level (from bathy_meter)
      ! ===================
      zmax = gdepw_0(jpk) + e3t_0(jpk)          ! maximum depth (i.e. the last ocean level thickness <= 2*e3t_0(jpkm1) )
      bathy(:,:) = MIN( zmax ,  bathy(:,:) )    ! bounded value of bathy (min already set at the end of zgr_bat)
      WHERE( bathy(:,:) == 0._wp )   ;   mbathy(:,:) = 0       ! land  : set mbathy to 0
      ELSE WHERE                     ;   mbathy(:,:) = jpkm1   ! ocean : initialize mbathy to the max ocean level
      END WHERE

      ! Compute mbathy for ocean points (i.e. the number of ocean levels)
      ! find the number of ocean levels such that the last level thickness
      ! is larger than the minimum of e3zps_min and e3zps_rat * e3t_0 (where
      ! e3t_0 is the reference level thickness
      DO jk = jpkm1, 1, -1
         zdepth = gdepw_0(jk) + MIN( e3zps_min, e3t_0(jk)*e3zps_rat )
         WHERE( 0._wp < bathy(:,:) .AND. bathy(:,:) <= zdepth )   mbathy(:,:) = jk-1
      END DO

      ! Scale factors and depth at T- and W-points
      DO jk = 1, jpk                        ! intitialization to the reference z-coordinate
         gdept(:,:,jk) = gdept_0(jk)
         gdepw(:,:,jk) = gdepw_0(jk)
         e3t  (:,:,jk) = e3t_0  (jk)
         e3w  (:,:,jk) = e3w_0  (jk)
      END DO
      ! 
      DO jj = 1, jpj
         DO ji = 1, jpi
            ik = mbathy(ji,jj)
            IF( ik > 0 ) THEN               ! ocean point only
               ! max ocean level case
               IF( ik == jpkm1 ) THEN
                  zdepwp = bathy(ji,jj)
                  ze3tp  = bathy(ji,jj) - gdepw_0(ik)
                  ze3wp = 0.5_wp * e3w_0(ik) * ( 1._wp + ( ze3tp/e3t_0(ik) ) )
                  e3t(ji,jj,ik  ) = ze3tp
                  e3t(ji,jj,ik+1) = ze3tp
                  e3w(ji,jj,ik  ) = ze3wp
                  e3w(ji,jj,ik+1) = ze3tp
                  gdepw(ji,jj,ik+1) = zdepwp
                  gdept(ji,jj,ik  ) = gdept_0(ik-1) + ze3wp
                  gdept(ji,jj,ik+1) = gdept(ji,jj,ik) + ze3tp
                  !
               ELSE                         ! standard case
                  IF( bathy(ji,jj) <= gdepw_0(ik+1) ) THEN   ;   gdepw(ji,jj,ik+1) = bathy(ji,jj)
                  ELSE                                       ;   gdepw(ji,jj,ik+1) = gdepw_0(ik+1)
                  ENDIF
!gm Bug?  check the gdepw_0
                  !       ... on ik
                  gdept(ji,jj,ik) = gdepw_0(ik) + ( gdepw  (ji,jj,ik+1) - gdepw_0(ik) )   &
                     &                          * ((gdept_0(      ik  ) - gdepw_0(ik) )   &
                     &                          / ( gdepw_0(      ik+1) - gdepw_0(ik) ))
                  e3t  (ji,jj,ik) = e3t_0  (ik) * ( gdepw  (ji,jj,ik+1) - gdepw_0(ik) )   & 
                     &                          / ( gdepw_0(      ik+1) - gdepw_0(ik) ) 
                  e3w  (ji,jj,ik) = 0.5_wp * ( gdepw(ji,jj,ik+1) + gdepw_0(ik+1) - 2._wp * gdepw_0(ik) )   &
                     &                     * ( e3w_0(ik) / ( gdepw_0(ik+1) - gdepw_0(ik) ) )
                  !       ... on ik+1
                  e3w  (ji,jj,ik+1) = e3t  (ji,jj,ik)
                  e3t  (ji,jj,ik+1) = e3t  (ji,jj,ik)
                  gdept(ji,jj,ik+1) = gdept(ji,jj,ik) + e3t(ji,jj,ik)
               ENDIF
            ENDIF
         END DO
      END DO
      !
      it = 0
      DO jj = 1, jpj
         DO ji = 1, jpi
            ik = mbathy(ji,jj)
            IF( ik > 0 ) THEN               ! ocean point only
               e3tp (ji,jj) = e3t(ji,jj,ik  )
               e3wp (ji,jj) = e3w(ji,jj,ik  )
               ! test
               zdiff= gdepw(ji,jj,ik+1) - gdept(ji,jj,ik  )
               IF( zdiff <= 0._wp .AND. lwp ) THEN 
                  it = it + 1
                  WRITE(numout,*) ' it      = ', it, ' ik      = ', ik, ' (i,j) = ', ji, jj
                  WRITE(numout,*) ' bathy = ', bathy(ji,jj)
                  WRITE(numout,*) ' gdept = ', gdept(ji,jj,ik), ' gdepw = ', gdepw(ji,jj,ik+1), ' zdiff = ', zdiff
                  WRITE(numout,*) ' e3tp  = ', e3t  (ji,jj,ik), ' e3wp  = ', e3w  (ji,jj,ik  )
               ENDIF
            ENDIF
         END DO
      END DO

      ! Scale factors and depth at U-, V-, UW and VW-points
      DO jk = 1, jpk                        ! initialisation to z-scale factors
         e3u (:,:,jk) = e3t_0(jk)
         e3v (:,:,jk) = e3t_0(jk)
         e3uw(:,:,jk) = e3w_0(jk)
         e3vw(:,:,jk) = e3w_0(jk)
      END DO
      DO jk = 1,jpk                         ! Computed as the minimum of neighbooring scale factors
         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1   ! vector opt.
               e3u (ji,jj,jk) = MIN( e3t(ji,jj,jk), e3t(ji+1,jj,jk) )
               e3v (ji,jj,jk) = MIN( e3t(ji,jj,jk), e3t(ji,jj+1,jk) )
               e3uw(ji,jj,jk) = MIN( e3w(ji,jj,jk), e3w(ji+1,jj,jk) )
               e3vw(ji,jj,jk) = MIN( e3w(ji,jj,jk), e3w(ji,jj+1,jk) )
            END DO
         END DO
      END DO
      CALL lbc_lnk( e3u , 'U', 1._wp )   ;   CALL lbc_lnk( e3uw, 'U', 1._wp )   ! lateral boundary conditions
      CALL lbc_lnk( e3v , 'V', 1._wp )   ;   CALL lbc_lnk( e3vw, 'V', 1._wp )
      !
      DO jk = 1,jpkm1                         ! Computed as the minimum of neighbooring scale factors
         DO jj = 1, jpj
            DO ji = mi0(1),mi1(1)
               e3u(ji,jj,jk) = MIN(e3t(ji,jj,jk),e3t(ji+1,jj,jk))
            END DO
         END DO
      END DO
      DO jk = 1,jpkm1                         ! Computed as the minimum of neighbooring scale factors
         DO jj = mj0(1),mj1(1)
            DO ji = 1, jpi
               e3v(ji,jj,jk) = MIN(e3t(ji,jj,jk),e3t(ji,jj+1,jk))
            END DO
         END DO
      END DO

      DO jk = 1, jpk                        ! set to z-scale factor if zero (i.e. along closed boundaries)
         WHERE( e3u (:,:,jk) == 0._wp )   e3u (:,:,jk) = e3t_0(jk)
         WHERE( e3v (:,:,jk) == 0._wp )   e3v (:,:,jk) = e3t_0(jk)
         WHERE( e3uw(:,:,jk) == 0._wp )   e3uw(:,:,jk) = e3w_0(jk)
         WHERE( e3vw(:,:,jk) == 0._wp )   e3vw(:,:,jk) = e3w_0(jk)
      END DO
     
      
      ! Scale factor at F-point
      DO jk = 1, jpk                        ! initialisation to z-scale factors
         e3f(:,:,jk) = e3t_0(jk)
      END DO
      DO jk = 1, jpk                        ! Computed as the minimum of neighbooring V-scale factors
         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1   ! vector opt.
               e3f(ji,jj,jk) = MIN( e3v(ji,jj,jk), e3v(ji+1,jj,jk) )
            END DO
         END DO
      END DO
      CALL lbc_lnk( e3f, 'F', 1._wp )       ! Lateral boundary conditions
      !
      DO jk = 1, jpk                        ! set to z-scale factor if zero (i.e. along closed boundaries)
         WHERE( e3f(:,:,jk) == 0._wp )   e3f(:,:,jk) = e3t_0(jk)
      END DO
!!gm  bug ? :  must be a do loop with mj0,mj1
      ! 
!!$      e3t(:,mj0(1),:) = e3t(:,mj0(2),:)     ! we duplicate factor scales for jj = 1 and jj = 2
!!$      e3w(:,mj0(1),:) = e3w(:,mj0(2),:) 
!!$      e3u(:,mj0(1),:) = e3u(:,mj0(2),:) 
!!$      e3v(:,mj0(1),:) = e3v(:,mj0(2),:) 
!!$      e3f(:,mj0(1),:) = e3f(:,mj0(2),:) 

      ! Control of the sign
      IF( MINVAL( e3t  (:,:,:) ) <= 0._wp )   CALL ctl_stop( '    zgr_zps :   e r r o r   e3t   <= 0' )
      IF( MINVAL( e3w  (:,:,:) ) <= 0._wp )   CALL ctl_stop( '    zgr_zps :   e r r o r   e3w   <= 0' )
      IF( MINVAL( gdept(:,:,:) ) <  0._wp )   CALL ctl_stop( '    zgr_zps :   e r r o r   gdepw <  0' )
      IF( MINVAL( gdepw(:,:,:) ) <  0._wp )   CALL ctl_stop( '    zgr_zps :   e r r o r   gdepw <  0' )
     
      ! Compute gdep3w (vertical sum of e3w)
      gdep3w(:,:,1) = 0.5_wp * e3w(:,:,1)
      DO jk = 2, jpk
         gdep3w(:,:,jk) = gdep3w(:,:,jk-1) + e3w(:,:,jk) 
      END DO
        
      !                                               ! ================= !
      IF(lwp .AND. ll_print) THEN                     !   Control print   !
         !                                            ! ================= !
         DO jj = 1,jpj
            DO ji = 1, jpi
               ik = MAX( mbathy(ji,jj), 1 )
               zprt(ji,jj,1) = e3t   (ji,jj,ik)
               zprt(ji,jj,2) = e3w   (ji,jj,ik)
               zprt(ji,jj,3) = e3u   (ji,jj,ik)
               zprt(ji,jj,4) = e3v   (ji,jj,ik)
               zprt(ji,jj,5) = e3f   (ji,jj,ik)
               zprt(ji,jj,6) = gdep3w(ji,jj,ik)
            END DO
         END DO
         WRITE(numout,*)
         WRITE(numout,*) 'domzgr e3t(mbathy)'      ;   CALL prihre(zprt(:,:,1),jpi,jpj,1,jpi,1,1,jpj,1,1.e-3,numout)
         WRITE(numout,*)
         WRITE(numout,*) 'domzgr e3w(mbathy)'      ;   CALL prihre(zprt(:,:,2),jpi,jpj,1,jpi,1,1,jpj,1,1.e-3,numout)
         WRITE(numout,*)
         WRITE(numout,*) 'domzgr e3u(mbathy)'      ;   CALL prihre(zprt(:,:,3),jpi,jpj,1,jpi,1,1,jpj,1,1.e-3,numout)
         WRITE(numout,*)
         WRITE(numout,*) 'domzgr e3v(mbathy)'      ;   CALL prihre(zprt(:,:,4),jpi,jpj,1,jpi,1,1,jpj,1,1.e-3,numout)
         WRITE(numout,*)
         WRITE(numout,*) 'domzgr e3f(mbathy)'      ;   CALL prihre(zprt(:,:,5),jpi,jpj,1,jpi,1,1,jpj,1,1.e-3,numout)
         WRITE(numout,*)
         WRITE(numout,*) 'domzgr gdep3w(mbathy)'   ;   CALL prihre(zprt(:,:,6),jpi,jpj,1,jpi,1,1,jpj,1,1.e-3,numout)
      ENDIF  
      !
      CALL wrk_dealloc( jpi, jpj, jpk, zprt )
      !
      IF( nn_timing == 1 )  CALL timing_stop('zgr_zps')
      !
   END SUBROUTINE zgr_zps


   FUNCTION fssig( pk ) RESULT( pf )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE eos_init  ***
      !!       
      !! ** Purpose :   provide the analytical function in s-coordinate
      !!          
      !! ** Method  :   the function provide the non-dimensional position of
      !!                T and W (i.e. between 0 and 1)
      !!                T-points at integer values (between 1 and jpk)
      !!                W-points at integer values - 1/2 (between 0.5 and jpk-0.5)
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in) ::   pk   ! continuous "k" coordinate
      REAL(wp)             ::   pf   ! sigma value
      !!----------------------------------------------------------------------
      !
      pf =   (   TANH( rn_theta * ( -(pk-0.5_wp) / REAL(jpkm1) + rn_thetb )  )   &
         &     - TANH( rn_thetb * rn_theta                                )  )   &
         & * (   COSH( rn_theta                           )                      &
         &     + COSH( rn_theta * ( 2._wp * rn_thetb - 1._wp ) )  )              &
         & / ( 2._wp * SINH( rn_theta ) )
      !
   END FUNCTION fssig


   FUNCTION fssig1( pk1, pbb ) RESULT( pf1 )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE eos_init  ***
      !!
      !! ** Purpose :   provide the Song and Haidvogel version of the analytical function in s-coordinate
      !!
      !! ** Method  :   the function provides the non-dimensional position of
      !!                T and W (i.e. between 0 and 1)
      !!                T-points at integer values (between 1 and jpk)
      !!                W-points at integer values - 1/2 (between 0.5 and jpk-0.5)
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in) ::   pk1   ! continuous "k" coordinate
      REAL(wp), INTENT(in) ::   pbb   ! Stretching coefficient
      REAL(wp)             ::   pf1   ! sigma value
      !!----------------------------------------------------------------------
      !
      IF ( rn_theta == 0 ) then      ! uniform sigma
         pf1 = - ( pk1 - 0.5_wp ) / REAL( jpkm1 )
      ELSE                        ! stretched sigma
         pf1 =   ( 1._wp - pbb ) * ( SINH( rn_theta*(-(pk1-0.5_wp)/REAL(jpkm1)) ) ) / SINH( rn_theta )              &
            &  + pbb * (  (TANH( rn_theta*( (-(pk1-0.5_wp)/REAL(jpkm1)) + 0.5_wp) ) - TANH( 0.5_wp * rn_theta )  )  &
            &        / ( 2._wp * TANH( 0.5_wp * rn_theta ) )  )
      ENDIF
      !
   END FUNCTION fssig1


   SUBROUTINE zgr_sco
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zgr_sco  ***
      !!                     
      !! ** Purpose :   define the s-coordinate system
      !!
      !! ** Method  :   s-coordinate
      !!         The depth of model levels is defined as the product of an
      !!      analytical function by the local bathymetry, while the vertical
      !!      scale factors are defined as the product of the first derivative
      !!      of the analytical function by the bathymetry.
      !!      (this solution save memory as depth and scale factors are not
      !!      3d fields)
      !!          - Read bathymetry (in meters) at t-point and compute the
      !!         bathymetry at u-, v-, and f-points.
      !!            hbatu = mi( hbatt )
      !!            hbatv = mj( hbatt )
      !!            hbatf = mi( mj( hbatt ) )
      !!          - Compute gsigt, gsigw, esigt, esigw from an analytical
      !!         function and its derivative given as function.
      !!            gsigt(k) = fssig (k    )
      !!            gsigw(k) = fssig (k-0.5)
      !!            esigt(k) = fsdsig(k    )
      !!            esigw(k) = fsdsig(k-0.5)
      !!      This routine is given as an example, it must be modified
      !!      following the user s desiderata. nevertheless, the output as
      !!      well as the way to compute the model levels and scale factors
      !!      must be respected in order to insure second order a!!uracy
      !!      schemes.
      !!
      !! Reference : Madec, Lott, Delecluse and Crepon, 1996. JPO, 26, 1393-1408.
      !!----------------------------------------------------------------------
      !
      INTEGER  ::   ji, jj, jk, jl           ! dummy loop argument
      INTEGER  ::   iip1, ijp1, iim1, ijm1   ! temporary integers
      REAL(wp) ::   zcoeft, zcoefw, zrmax, ztaper   ! temporary scalars
      !
      REAL(wp), POINTER, DIMENSION(:,:  ) :: zenv, ztmp, zmsk, zri, zrj, zhbat
      REAL(wp), POINTER, DIMENSION(:,:,:) :: gsigw3, gsigt3, gsi3w3
      REAL(wp), POINTER, DIMENSION(:,:,:) :: esigt3, esigw3, esigtu3, esigtv3, esigtf3, esigwu3, esigwv3           

      NAMELIST/namzgr_sco/ rn_sbot_max, rn_sbot_min, rn_theta, rn_thetb, rn_rmax, ln_s_sigma, rn_bb, rn_hc
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('zgr_sco')
      !
      CALL wrk_alloc( jpi, jpj,      zenv, ztmp, zmsk, zri, zrj, zhbat                           )
      CALL wrk_alloc( jpi, jpj, jpk, gsigw3, gsigt3, gsi3w3                                      )
      CALL wrk_alloc( jpi, jpj, jpk, esigt3, esigw3, esigtu3, esigtv3, esigtf3, esigwu3, esigwv3 )
      !
      REWIND( numnam )                       ! Read Namelist namzgr_sco : sigma-stretching parameters
      READ  ( numnam, namzgr_sco )

      IF(lwp) THEN                           ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'dom:zgr_sco : s-coordinate or hybrid z-s-coordinate'
         WRITE(numout,*) '~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namzgr_sco'
         WRITE(numout,*) '      sigma-stretching coeffs '
         WRITE(numout,*) '      maximum depth of s-bottom surface (>0)       rn_sbot_max   = ' ,rn_sbot_max
         WRITE(numout,*) '      minimum depth of s-bottom surface (>0)       rn_sbot_min   = ' ,rn_sbot_min
         WRITE(numout,*) '      surface control parameter (0<=rn_theta<=20)  rn_theta      = ', rn_theta
         WRITE(numout,*) '      bottom  control parameter (0<=rn_thetb<= 1)  rn_thetb      = ', rn_thetb
         WRITE(numout,*) '      maximum cut-off r-value allowed              rn_rmax       = ', rn_rmax
         WRITE(numout,*) '      Hybrid s-sigma-coordinate                    ln_s_sigma    = ', ln_s_sigma
         WRITE(numout,*) '      stretching parameter (song and haidvogel)    rn_bb         = ', rn_bb
         WRITE(numout,*) '      Critical depth                               rn_hc         = ', rn_hc
      ENDIF

      gsigw3  = 0._wp   ;   gsigt3  = 0._wp   ;   gsi3w3  = 0._wp
      esigt3  = 0._wp   ;   esigw3  = 0._wp 
      esigtu3 = 0._wp   ;   esigtv3 = 0._wp   ;   esigtf3 = 0._wp
      esigwu3 = 0._wp   ;   esigwv3 = 0._wp

      hift(:,:) = rn_sbot_min                     ! set the minimum depth for the s-coordinate
      hifu(:,:) = rn_sbot_min
      hifv(:,:) = rn_sbot_min
      hiff(:,:) = rn_sbot_min

      !                                        ! set maximum ocean depth
      bathy(:,:) = MIN( rn_sbot_max, bathy(:,:) )

      DO jj = 1, jpj
         DO ji = 1, jpi
           IF( bathy(ji,jj) > 0._wp )   bathy(ji,jj) = MAX( rn_sbot_min, bathy(ji,jj) )
         END DO
      END DO
      !                                        ! =============================
      !                                        ! Define the envelop bathymetry   (hbatt)
      !                                        ! =============================
      ! use r-value to create hybrid coordinates
      DO jj = 1, jpj
         DO ji = 1, jpi
            zenv(ji,jj) = MAX( bathy(ji,jj), rn_sbot_min )
         END DO
      END DO
      ! 
      ! Smooth the bathymetry (if required)
      scosrf(:,:) = 0._wp             ! ocean surface depth (here zero: no under ice-shelf sea)
      scobot(:,:) = bathy(:,:)        ! ocean bottom  depth
      !
      jl = 0
      zrmax = 1._wp
      !                                                     ! ================ !
      DO WHILE( jl <= 10000 .AND. zrmax > rn_rmax )         !  Iterative loop  !
         !                                                  ! ================ !
         jl = jl + 1
         zrmax = 0._wp
         zmsk(:,:) = 0._wp
         DO jj = 1, nlcj
            DO ji = 1, nlci
               iip1 = MIN( ji+1, nlci )      ! force zri = 0 on last line (ji=ncli+1 to jpi)
               ijp1 = MIN( jj+1, nlcj )      ! force zrj = 0 on last raw  (jj=nclj+1 to jpj)
               zri(ji,jj) = ABS( zenv(iip1,jj  ) - zenv(ji,jj) ) / ( zenv(iip1,jj  ) + zenv(ji,jj) )
               zrj(ji,jj) = ABS( zenv(ji  ,ijp1) - zenv(ji,jj) ) / ( zenv(ji  ,ijp1) + zenv(ji,jj) )
               zrmax = MAX( zrmax, zri(ji,jj), zrj(ji,jj) )
               IF( zri(ji,jj) > rn_rmax )   zmsk(ji  ,jj  ) = 1._wp
               IF( zri(ji,jj) > rn_rmax )   zmsk(iip1,jj  ) = 1._wp
               IF( zrj(ji,jj) > rn_rmax )   zmsk(ji  ,jj  ) = 1._wp
               IF( zrj(ji,jj) > rn_rmax )   zmsk(ji  ,ijp1) = 1._wp
            END DO
         END DO
         IF( lk_mpp )   CALL mpp_max( zrmax )   ! max over the global domain
         ! lateral boundary condition on zmsk: keep 1 along closed boundary (use of MAX)
         ztmp(:,:) = zmsk(:,:)   ;   CALL lbc_lnk( zmsk, 'T', 1._wp )
         DO jj = 1, nlcj
            DO ji = 1, nlci
                zmsk(ji,jj) = MAX( zmsk(ji,jj), ztmp(ji,jj) )
            END DO
         END DO
         !
         IF(lwp)WRITE(numout,*) 'zgr_sco :   iter= ',jl, ' rmax= ', zrmax, ' nb of pt= ', INT( SUM(zmsk(:,:) ) )
         !
         DO jj = 1, nlcj
            DO ji = 1, nlci
               iip1 = MIN( ji+1, nlci )     ! last  line (ji=nlci)
               ijp1 = MIN( jj+1, nlcj )     ! last  raw  (jj=nlcj)
               iim1 = MAX( ji-1,  1  )      ! first line (ji=nlci)
               ijm1 = MAX( jj-1,  1  )      ! first raw  (jj=nlcj)
               IF( zmsk(ji,jj) == 1._wp ) THEN
                  ztmp(ji,jj) =   (                                                                                   &
             &      zenv(iim1,ijp1)*zmsk(iim1,ijp1) + zenv(ji,ijp1)*zmsk(ji,ijp1) + zenv(iip1,ijp1)*zmsk(iip1,ijp1)   &
             &    + zenv(iim1,jj  )*zmsk(iim1,jj  ) + zenv(ji,jj  )*    2._wp     + zenv(iip1,jj  )*zmsk(iip1,jj  )   &
             &    + zenv(iim1,ijm1)*zmsk(iim1,ijm1) + zenv(ji,ijm1)*zmsk(ji,ijm1) + zenv(iip1,ijm1)*zmsk(iip1,ijm1)   &
             &                    ) / (                                                                               &
             &                      zmsk(iim1,ijp1) +               zmsk(ji,ijp1) +                 zmsk(iip1,ijp1)   &
             &    +                 zmsk(iim1,jj  ) +                   2._wp     +                 zmsk(iip1,jj  )   &
             &    +                 zmsk(iim1,ijm1) +               zmsk(ji,ijm1) +                 zmsk(iip1,ijm1)   &
             &                        )
               ENDIF
            END DO
         END DO
         !
         DO jj = 1, nlcj
            DO ji = 1, nlci
               IF( zmsk(ji,jj) == 1._wp )   zenv(ji,jj) = MAX( ztmp(ji,jj), bathy(ji,jj) )
            END DO
         END DO
         !
         ! Apply lateral boundary condition   CAUTION: kept the value when the lbc field is zero
         ztmp(:,:) = zenv(:,:)   ;   CALL lbc_lnk( zenv, 'T', 1._wp )
         DO jj = 1, nlcj
            DO ji = 1, nlci
               IF( zenv(ji,jj) == 0._wp )   zenv(ji,jj) = ztmp(ji,jj)
            END DO
         END DO
         !                                                  ! ================ !
      END DO                                                !     End loop     !
      !                                                     ! ================ !
      !
      ! Fill ghost rows with appropriate values to avoid undefined e3 values with some mpp decompositions
      DO ji = nlci+1, jpi 
         zenv(ji,1:nlcj) = zenv(nlci,1:nlcj)
      END DO
      !
      DO jj = nlcj+1, jpj
         zenv(:,jj) = zenv(:,nlcj)
      END DO
      !
      ! Envelope bathymetry saved in hbatt
      hbatt(:,:) = zenv(:,:) 
      IF( MINVAL( gphit(:,:) ) * MAXVAL( gphit(:,:) ) <= 0._wp ) THEN
         CALL ctl_warn( ' s-coordinates are tapered in vicinity of the Equator' )
         DO jj = 1, jpj
            DO ji = 1, jpi
               ztaper = EXP( -(gphit(ji,jj)/8._wp)**2 )
               hbatt(ji,jj) = rn_sbot_max * ztaper + hbatt(ji,jj) * ( 1._wp - ztaper )
            END DO
         END DO
      ENDIF
      !
      IF(lwp) THEN                             ! Control print
         WRITE(numout,*)
         WRITE(numout,*) ' domzgr: hbatt field; ocean depth in meters'
         WRITE(numout,*)
         CALL prihre( hbatt(1,1), jpi, jpj, 1, jpi, 1, 1, jpj, 1, 0._wp, numout )
         IF( nprint == 1 )   THEN        
            WRITE(numout,*) ' bathy  MAX ', MAXVAL( bathy(:,:) ), ' MIN ', MINVAL( bathy(:,:) )
            WRITE(numout,*) ' hbatt  MAX ', MAXVAL( hbatt(:,:) ), ' MIN ', MINVAL( hbatt(:,:) )
         ENDIF
      ENDIF

      !                                        ! ==============================
      !                                        !   hbatu, hbatv, hbatf fields
      !                                        ! ==============================
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) ' zgr_sco: minimum depth of the envelop topography set to : ', rn_sbot_min
      ENDIF
      hbatu(:,:) = rn_sbot_min
      hbatv(:,:) = rn_sbot_min
      hbatf(:,:) = rn_sbot_min
      DO jj = 1, jpjm1
        DO ji = 1, jpim1   ! NO vector opt.
           hbatu(ji,jj) = 0.50_wp * ( hbatt(ji  ,jj) + hbatt(ji+1,jj  ) )
           hbatv(ji,jj) = 0.50_wp * ( hbatt(ji  ,jj) + hbatt(ji  ,jj+1) )
           hbatf(ji,jj) = 0.25_wp * ( hbatt(ji  ,jj) + hbatt(ji  ,jj+1)   &
              &                     + hbatt(ji+1,jj) + hbatt(ji+1,jj+1) )
        END DO
      END DO
      ! 
      ! Apply lateral boundary condition
!!gm  ! CAUTION: retain non zero value in the initial file this should be OK for orca cfg, not for EEL
      zhbat(:,:) = hbatu(:,:)   ;   CALL lbc_lnk( hbatu, 'U', 1._wp )
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF( hbatu(ji,jj) == 0._wp ) THEN
               IF( zhbat(ji,jj) == 0._wp )   hbatu(ji,jj) = rn_sbot_min
               IF( zhbat(ji,jj) /= 0._wp )   hbatu(ji,jj) = zhbat(ji,jj)
            ENDIF
         END DO
      END DO
      zhbat(:,:) = hbatv(:,:)   ;   CALL lbc_lnk( hbatv, 'V', 1._wp )
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF( hbatv(ji,jj) == 0._wp ) THEN
               IF( zhbat(ji,jj) == 0._wp )   hbatv(ji,jj) = rn_sbot_min
               IF( zhbat(ji,jj) /= 0._wp )   hbatv(ji,jj) = zhbat(ji,jj)
            ENDIF
         END DO
      END DO
      zhbat(:,:) = hbatf(:,:)   ;   CALL lbc_lnk( hbatf, 'F', 1._wp )
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF( hbatf(ji,jj) == 0._wp ) THEN
               IF( zhbat(ji,jj) == 0._wp )   hbatf(ji,jj) = rn_sbot_min
               IF( zhbat(ji,jj) /= 0._wp )   hbatf(ji,jj) = zhbat(ji,jj)
            ENDIF
         END DO
      END DO

!!bug:  key_helsinki a verifer
      hift(:,:) = MIN( hift(:,:), hbatt(:,:) )
      hifu(:,:) = MIN( hifu(:,:), hbatu(:,:) )
      hifv(:,:) = MIN( hifv(:,:), hbatv(:,:) )
      hiff(:,:) = MIN( hiff(:,:), hbatf(:,:) )

      IF( nprint == 1 .AND. lwp )   THEN
         WRITE(numout,*) ' MAX val hif   t ', MAXVAL( hift (:,:) ), ' f ', MAXVAL( hiff (:,:) ),  &
            &                        ' u ',   MAXVAL( hifu (:,:) ), ' v ', MAXVAL( hifv (:,:) )
         WRITE(numout,*) ' MIN val hif   t ', MINVAL( hift (:,:) ), ' f ', MINVAL( hiff (:,:) ),  &
            &                        ' u ',   MINVAL( hifu (:,:) ), ' v ', MINVAL( hifv (:,:) )
         WRITE(numout,*) ' MAX val hbat  t ', MAXVAL( hbatt(:,:) ), ' f ', MAXVAL( hbatf(:,:) ),  &
            &                        ' u ',   MAXVAL( hbatu(:,:) ), ' v ', MAXVAL( hbatv(:,:) )
         WRITE(numout,*) ' MIN val hbat  t ', MINVAL( hbatt(:,:) ), ' f ', MINVAL( hbatf(:,:) ),  &
            &                        ' u ',   MINVAL( hbatu(:,:) ), ' v ', MINVAL( hbatv(:,:) )
      ENDIF
!! helsinki

      !                                            ! =======================
      !                                            !   s-ccordinate fields     (gdep., e3.)
      !                                            ! =======================
      !
      ! non-dimensional "sigma" for model level depth at w- and t-levels

      IF( ln_s_sigma ) THEN        ! Song and Haidvogel style stretched sigma for depths
         !                         ! below rn_hc, with uniform sigma in shallower waters
         DO ji = 1, jpi
            DO jj = 1, jpj

               IF( hbatt(ji,jj) > rn_hc ) THEN    !deep water, stretched sigma
                  DO jk = 1, jpk
                     gsigw3(ji,jj,jk) = -fssig1( REAL(jk,wp)-0.5_wp, rn_bb )
                     gsigt3(ji,jj,jk) = -fssig1( REAL(jk,wp)       , rn_bb )
                  END DO
               ELSE ! shallow water, uniform sigma
                  DO jk = 1, jpk
                     gsigw3(ji,jj,jk) =   REAL(jk-1,wp)            / REAL(jpk-1,wp)
                     gsigt3(ji,jj,jk) = ( REAL(jk-1,wp) + 0.5_wp ) / REAL(jpk-1,wp)
                  END DO
               ENDIF
               IF( nprint == 1 .AND. lwp )   WRITE(numout,*) 'gsigw3 1 jpk    ', gsigw3(ji,jj,1), gsigw3(ji,jj,jpk)
               !
               DO jk = 1, jpkm1
                  esigt3(ji,jj,jk  ) = gsigw3(ji,jj,jk+1) - gsigw3(ji,jj,jk)
                  esigw3(ji,jj,jk+1) = gsigt3(ji,jj,jk+1) - gsigt3(ji,jj,jk)
               END DO
               esigw3(ji,jj,1  ) = 2._wp * ( gsigt3(ji,jj,1  ) - gsigw3(ji,jj,1  ) )
               esigt3(ji,jj,jpk) = 2._wp * ( gsigt3(ji,jj,jpk) - gsigw3(ji,jj,jpk) )
               !
               ! Coefficients for vertical depth as the sum of e3w scale factors
               gsi3w3(ji,jj,1) = 0.5_wp * esigw3(ji,jj,1)
               DO jk = 2, jpk
                  gsi3w3(ji,jj,jk) = gsi3w3(ji,jj,jk-1) + esigw3(ji,jj,jk)
               END DO
               !
               DO jk = 1, jpk
                  zcoeft = ( REAL(jk,wp) - 0.5_wp ) / REAL(jpkm1,wp)
                  zcoefw = ( REAL(jk,wp) - 1.0_wp ) / REAL(jpkm1,wp)
                  gdept (ji,jj,jk) = ( scosrf(ji,jj) + (hbatt(ji,jj)-rn_hc)*gsigt3(ji,jj,jk)+rn_hc*zcoeft )
                  gdepw (ji,jj,jk) = ( scosrf(ji,jj) + (hbatt(ji,jj)-rn_hc)*gsigw3(ji,jj,jk)+rn_hc*zcoefw )
                  gdep3w(ji,jj,jk) = ( scosrf(ji,jj) + (hbatt(ji,jj)-rn_hc)*gsi3w3(ji,jj,jk)+rn_hc*zcoeft )
               END DO
               !
            END DO   ! for all jj's
         END DO    ! for all ji's

         DO ji = 1, jpim1
            DO jj = 1, jpjm1
               DO jk = 1, jpk
                  esigtu3(ji,jj,jk) = ( hbatt(ji,jj)*esigt3(ji,jj,jk)+hbatt(ji+1,jj)*esigt3(ji+1,jj,jk) )   &
                     &              / ( hbatt(ji,jj)+hbatt(ji+1,jj) )
                  esigtv3(ji,jj,jk) = ( hbatt(ji,jj)*esigt3(ji,jj,jk)+hbatt(ji,jj+1)*esigt3(ji,jj+1,jk) )   &
                     &              / ( hbatt(ji,jj)+hbatt(ji,jj+1) )
                  esigtf3(ji,jj,jk) = ( hbatt(ji,jj)*esigt3(ji,jj,jk)+hbatt(ji+1,jj)*esigt3(ji+1,jj,jk)     &
                     &                + hbatt(ji,jj+1)*esigt3(ji,jj+1,jk)+hbatt(ji+1,jj+1)*esigt3(ji+1,jj+1,jk) )   &
                     &              / ( hbatt(ji,jj)+hbatt(ji+1,jj)+hbatt(ji,jj+1)+hbatt(ji+1,jj+1) )
                  esigwu3(ji,jj,jk) = ( hbatt(ji,jj)*esigw3(ji,jj,jk)+hbatt(ji+1,jj)*esigw3(ji+1,jj,jk) )   &
                     &              / ( hbatt(ji,jj)+hbatt(ji+1,jj) )
                  esigwv3(ji,jj,jk) = ( hbatt(ji,jj)*esigw3(ji,jj,jk)+hbatt(ji,jj+1)*esigw3(ji,jj+1,jk) )   &
                     &              / ( hbatt(ji,jj)+hbatt(ji,jj+1) )
                  !
                  e3t(ji,jj,jk) = ( (hbatt(ji,jj)-rn_hc)*esigt3 (ji,jj,jk) + rn_hc/FLOAT(jpkm1) )
                  e3u(ji,jj,jk) = ( (hbatu(ji,jj)-rn_hc)*esigtu3(ji,jj,jk) + rn_hc/FLOAT(jpkm1) )
                  e3v(ji,jj,jk) = ( (hbatv(ji,jj)-rn_hc)*esigtv3(ji,jj,jk) + rn_hc/FLOAT(jpkm1) )
                  e3f(ji,jj,jk) = ( (hbatf(ji,jj)-rn_hc)*esigtf3(ji,jj,jk) + rn_hc/FLOAT(jpkm1) )
                  !
                  e3w (ji,jj,jk) = ( (hbatt(ji,jj)-rn_hc)*esigw3 (ji,jj,jk) + rn_hc/FLOAT(jpkm1) )
                  e3uw(ji,jj,jk) = ( (hbatu(ji,jj)-rn_hc)*esigwu3(ji,jj,jk) + rn_hc/FLOAT(jpkm1) )
                  e3vw(ji,jj,jk) = ( (hbatv(ji,jj)-rn_hc)*esigwv3(ji,jj,jk) + rn_hc/FLOAT(jpkm1) )
               END DO
            END DO
         END DO

         CALL lbc_lnk( e3t , 'T', 1._wp )
         CALL lbc_lnk( e3u , 'U', 1._wp )
         CALL lbc_lnk( e3v , 'V', 1._wp )
         CALL lbc_lnk( e3f , 'F', 1._wp )
         CALL lbc_lnk( e3w , 'W', 1._wp )
         CALL lbc_lnk( e3uw, 'U', 1._wp )
         CALL lbc_lnk( e3vw, 'V', 1._wp )

         !
      ELSE   ! not ln_s_sigma
         !
         DO jk = 1, jpk
           gsigw(jk) = -fssig( REAL(jk,wp)-0.5_wp )
           gsigt(jk) = -fssig( REAL(jk,wp)        )
         END DO
         IF( nprint == 1 .AND. lwp )   WRITE(numout,*) 'gsigw 1 jpk    ', gsigw(1), gsigw(jpk)
         !
         ! Coefficients for vertical scale factors at w-, t- levels
!!gm bug :  define it from analytical function, not like juste bellow....
!!gm        or betteroffer the 2 possibilities....
         DO jk = 1, jpkm1
            esigt(jk  ) = gsigw(jk+1) - gsigw(jk)
            esigw(jk+1) = gsigt(jk+1) - gsigt(jk)
         END DO
         esigw( 1 ) = 2._wp * ( gsigt(1  ) - gsigw(1  ) ) 
         esigt(jpk) = 2._wp * ( gsigt(jpk) - gsigw(jpk) )

!!gm  original form
!!org DO jk = 1, jpk
!!org    esigt(jk)=fsdsig( FLOAT(jk)     )
!!org    esigw(jk)=fsdsig( FLOAT(jk)-0.5 )
!!org END DO
!!gm
         !
         ! Coefficients for vertical depth as the sum of e3w scale factors
         gsi3w(1) = 0.5_wp * esigw(1)
         DO jk = 2, jpk
            gsi3w(jk) = gsi3w(jk-1) + esigw(jk)
         END DO
!!gm: depuw, depvw can be suppressed (modif in ldfslp) and depw=dep3w can be set (save 3 3D arrays)
         DO jk = 1, jpk
            zcoeft = ( REAL(jk,wp) - 0.5_wp ) / REAL(jpkm1,wp)
            zcoefw = ( REAL(jk,wp) - 1.0_wp ) / REAL(jpkm1,wp)
            gdept (:,:,jk) = ( scosrf(:,:) + (hbatt(:,:)-hift(:,:))*gsigt(jk) + hift(:,:)*zcoeft )
            gdepw (:,:,jk) = ( scosrf(:,:) + (hbatt(:,:)-hift(:,:))*gsigw(jk) + hift(:,:)*zcoefw )
            gdep3w(:,:,jk) = ( scosrf(:,:) + (hbatt(:,:)-hift(:,:))*gsi3w(jk) + hift(:,:)*zcoeft )
         END DO
!!gm: e3uw, e3vw can be suppressed  (modif in dynzdf, dynzdf_iso, zdfbfr) (save 2 3D arrays)
         DO jj = 1, jpj
            DO ji = 1, jpi
               DO jk = 1, jpk
                 e3t(ji,jj,jk) = ( (hbatt(ji,jj)-hift(ji,jj))*esigt(jk) + hift(ji,jj)/REAL(jpkm1,wp) )
                 e3u(ji,jj,jk) = ( (hbatu(ji,jj)-hifu(ji,jj))*esigt(jk) + hifu(ji,jj)/REAL(jpkm1,wp) )
                 e3v(ji,jj,jk) = ( (hbatv(ji,jj)-hifv(ji,jj))*esigt(jk) + hifv(ji,jj)/REAL(jpkm1,wp) )
                 e3f(ji,jj,jk) = ( (hbatf(ji,jj)-hiff(ji,jj))*esigt(jk) + hiff(ji,jj)/REAL(jpkm1,wp) )
                 !
                 e3w (ji,jj,jk) = ( (hbatt(ji,jj)-hift(ji,jj))*esigw(jk) + hift(ji,jj)/REAL(jpkm1,wp) )
                 e3uw(ji,jj,jk) = ( (hbatu(ji,jj)-hifu(ji,jj))*esigw(jk) + hifu(ji,jj)/REAL(jpkm1,wp) )
                 e3vw(ji,jj,jk) = ( (hbatv(ji,jj)-hifv(ji,jj))*esigw(jk) + hifv(ji,jj)/REAL(jpkm1,wp) )
               END DO
            END DO
         END DO
         !
      ENDIF ! ln_s_sigma


      !
      where (e3t   (:,:,:).eq.0.0)  e3t(:,:,:) = 1.0
      where (e3u   (:,:,:).eq.0.0)  e3u(:,:,:) = 1.0
      where (e3v   (:,:,:).eq.0.0)  e3v(:,:,:) = 1.0
      where (e3f   (:,:,:).eq.0.0)  e3f(:,:,:) = 1.0
      where (e3w   (:,:,:).eq.0.0)  e3w(:,:,:) = 1.0
      where (e3uw  (:,:,:).eq.0.0)  e3uw(:,:,:) = 1.0
      where (e3vw  (:,:,:).eq.0.0)  e3vw(:,:,:) = 1.0


      fsdept(:,:,:) = gdept (:,:,:)
      fsdepw(:,:,:) = gdepw (:,:,:)
      fsde3w(:,:,:) = gdep3w(:,:,:)
      fse3t (:,:,:) = e3t   (:,:,:)
      fse3u (:,:,:) = e3u   (:,:,:)
      fse3v (:,:,:) = e3v   (:,:,:)
      fse3f (:,:,:) = e3f   (:,:,:)
      fse3w (:,:,:) = e3w   (:,:,:)
      fse3uw(:,:,:) = e3uw  (:,:,:)
      fse3vw(:,:,:) = e3vw  (:,:,:)
!!
      ! HYBRID : 
      DO jj = 1, jpj
         DO ji = 1, jpi
            DO jk = 1, jpkm1
               IF( scobot(ji,jj) >= fsdept(ji,jj,jk) )   mbathy(ji,jj) = MAX( 2, jk )
               IF( scobot(ji,jj) == 0._wp            )   mbathy(ji,jj) = 0
            END DO
         END DO
      END DO
      IF( nprint == 1 .AND. lwp ) WRITE(numout,*) ' MIN val mbathy h90 ', MINVAL( mbathy(:,:) ),   &
         &                                                       ' MAX ', MAXVAL( mbathy(:,:) )

      !                                               ! =============
      IF(lwp) THEN                                    ! Control print
         !                                            ! =============
         WRITE(numout,*) 
         WRITE(numout,*) ' domzgr: vertical coefficients for model level'
         WRITE(numout, "(9x,'  level    gsigt      gsigw      esigt      esigw      gsi3w')" )
         WRITE(numout, "(10x,i4,5f11.4)" ) ( jk, gsigt(jk), gsigw(jk), esigt(jk), esigw(jk), gsi3w(jk), jk=1,jpk )
      ENDIF
      IF( nprint == 1  .AND. lwp )   THEN         ! min max values over the local domain
         WRITE(numout,*) ' MIN val mbathy  ', MINVAL( mbathy(:,:)   ), ' MAX ', MAXVAL( mbathy(:,:) )
         WRITE(numout,*) ' MIN val depth t ', MINVAL( fsdept(:,:,:) ),   &
            &                          ' w ', MINVAL( fsdepw(:,:,:) ), '3w '  , MINVAL( fsde3w(:,:,:) )
         WRITE(numout,*) ' MIN val e3    t ', MINVAL( fse3t (:,:,:) ), ' f '  , MINVAL( fse3f (:,:,:) ),   &
            &                          ' u ', MINVAL( fse3u (:,:,:) ), ' u '  , MINVAL( fse3v (:,:,:) ),   &
            &                          ' uw', MINVAL( fse3uw(:,:,:) ), ' vw'  , MINVAL( fse3vw(:,:,:) ),   &
            &                          ' w ', MINVAL( fse3w (:,:,:) )

         WRITE(numout,*) ' MAX val depth t ', MAXVAL( fsdept(:,:,:) ),   &
            &                          ' w ', MAXVAL( fsdepw(:,:,:) ), '3w '  , MAXVAL( fsde3w(:,:,:) )
         WRITE(numout,*) ' MAX val e3    t ', MAXVAL( fse3t (:,:,:) ), ' f '  , MAXVAL( fse3f (:,:,:) ),   &
            &                          ' u ', MAXVAL( fse3u (:,:,:) ), ' u '  , MAXVAL( fse3v (:,:,:) ),   &
            &                          ' uw', MAXVAL( fse3uw(:,:,:) ), ' vw'  , MAXVAL( fse3vw(:,:,:) ),   &
            &                          ' w ', MAXVAL( fse3w (:,:,:) )
      ENDIF
      !
      IF(lwp) THEN                                  ! selected vertical profiles
         WRITE(numout,*)
         WRITE(numout,*) ' domzgr: vertical coordinates : point (1,1,k) bathy = ', bathy(1,1), hbatt(1,1)
         WRITE(numout,*) ' ~~~~~~  --------------------'
         WRITE(numout,"(9x,' level   gdept    gdepw    gde3w     e3t      e3w  ')")
         WRITE(numout,"(10x,i4,4f9.2)") ( jk, fsdept(1,1,jk), fsdepw(1,1,jk),     &
            &                                 fse3t (1,1,jk), fse3w (1,1,jk), jk=1,jpk )
         DO jj = mj0(20), mj1(20)
            DO ji = mi0(20), mi1(20)
               WRITE(numout,*)
               WRITE(numout,*) ' domzgr: vertical coordinates : point (20,20,k)   bathy = ', bathy(ji,jj), hbatt(ji,jj)
               WRITE(numout,*) ' ~~~~~~  --------------------'
               WRITE(numout,"(9x,' level   gdept    gdepw    gde3w     e3t      e3w  ')")
               WRITE(numout,"(10x,i4,4f9.2)") ( jk, fsdept(ji,jj,jk), fsdepw(ji,jj,jk),     &
                  &                                 fse3t (ji,jj,jk), fse3w (ji,jj,jk), jk=1,jpk )
            END DO
         END DO
         DO jj = mj0(74), mj1(74)
            DO ji = mi0(100), mi1(100)
               WRITE(numout,*)
               WRITE(numout,*) ' domzgr: vertical coordinates : point (100,74,k)   bathy = ', bathy(ji,jj), hbatt(ji,jj)
               WRITE(numout,*) ' ~~~~~~  --------------------'
               WRITE(numout,"(9x,' level   gdept    gdepw    gde3w     e3t      e3w  ')")
               WRITE(numout,"(10x,i4,4f9.2)") ( jk, fsdept(ji,jj,jk), fsdepw(ji,jj,jk),     &
                  &                                 fse3t (ji,jj,jk), fse3w (ji,jj,jk), jk=1,jpk )
            END DO
         END DO
      ENDIF

!!gm bug?  no more necessary?  if ! defined key_helsinki
      DO jk = 1, jpk
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( fse3w(ji,jj,jk) <= 0._wp .OR. fse3t(ji,jj,jk) <= 0._wp ) THEN
                  WRITE(ctmp1,*) 'zgr_sco :   e3w   or e3t   =< 0  at point (i,j,k)= ', ji, jj, jk
                  CALL ctl_stop( ctmp1 )
               ENDIF
               IF( fsdepw(ji,jj,jk) < 0._wp .OR. fsdept(ji,jj,jk) < 0._wp ) THEN
                  WRITE(ctmp1,*) 'zgr_sco :   gdepw or gdept =< 0  at point (i,j,k)= ', ji, jj, jk
                  CALL ctl_stop( ctmp1 )
               ENDIF
            END DO
         END DO
      END DO
!!gm bug    #endif
      !
      CALL wrk_dealloc( jpi, jpj,      zenv, ztmp, zmsk, zri, zrj, zhbat                           )
      CALL wrk_dealloc( jpi, jpj, jpk, gsigw3, gsigt3, gsi3w3                                      )
      CALL wrk_dealloc( jpi, jpj, jpk, esigt3, esigw3, esigtu3, esigtv3, esigtf3, esigwu3, esigwv3 )
      !
      IF( nn_timing == 1 )  CALL timing_stop('zgr_sco')
      !
   END SUBROUTINE zgr_sco

   !!======================================================================
END MODULE domzgr
