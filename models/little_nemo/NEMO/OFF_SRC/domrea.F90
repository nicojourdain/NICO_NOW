MODULE domrea
   !!======================================================================
   !!                       ***  MODULE domrea  ***
   !! Ocean initialization : read the ocean domain meshmask file(s)
   !!======================================================================
   !! History :  3.3  ! 2010-05  (C. Ethe)  Full reorganization of the off-line
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_rea        : read mesh and mask file(s)
   !!                    nmsh = 1  :   mesh_mask file
   !!                         = 2  :   mesh and mask file
   !!                         = 3  :   mesh_hgr, mesh_zgr and mask
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE dommsk          ! domain: masks
   USE lbclnk          ! lateral boundary condition - MPP exchanges
   USE trc_oce         ! shared ocean/biogeochemical variables
   USE lib_mpp 
   USE in_out_manager
   USE wrk_nemo  

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_rea    ! routine called by inidom.F90
   !!----------------------------------------------------------------------
   !! NEMO/OFF 3.3 , NEMO Consortium (2010)
   !! $Id: domrea.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_rea
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dom_rea  ***
      !!                   
      !! ** Purpose :  Read the NetCDF file(s) which contain(s) all the
      !!      ocean domain informations (mesh and mask arrays). This (these)
      !!      file(s) is (are) used for visualisation (SAXO software) and
      !!      diagnostic computation.
      !!
      !! ** Method  :   Read in a file all the arrays generated in routines
      !!      domhgr, domzgr, and dommsk. Note: the file contain depends on
      !!      the vertical coord. used (z-coord, partial steps, s-coord)
      !!                    nmsh = 1  :   'mesh_mask.nc' file
      !!                         = 2  :   'mesh.nc' and mask.nc' files
      !!                         = 3  :   'mesh_hgr.nc', 'mesh_zgr.nc' and
      !!                                  'mask.nc' files
      !!      For huge size domain, use option 2 or 3 depending on your 
      !!      vertical coordinate.
      !!
      !! ** input file : 
      !!      meshmask.nc  : domain size, horizontal grid-point position,
      !!                     masks, depth and vertical scale factors
      !!----------------------------------------------------------------------
      USE iom
      !!
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      INTEGER  ::   ik, inum0 , inum1 , inum2 , inum3 , inum4   ! local integers
      REAL(wp) ::   zrefdep         ! local real
      REAL(wp), POINTER, DIMENSION(:,:) :: zmbk, zprt, zprw
      !!----------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'dom_rea : read NetCDF mesh and mask information file(s)'
      IF(lwp) WRITE(numout,*) '~~~~~~~'

      CALL wrk_alloc( jpi, jpj, zmbk, zprt, zprw )

      zmbk(:,:) = 0._wp

      SELECT CASE (nmsh)
         !                                     ! ============================
         CASE ( 1 )                            !  create 'mesh_mask.nc' file
            !                                  ! ============================

            IF(lwp) WRITE(numout,*) '          one file in "mesh_mask.nc" '
            CALL iom_open( 'mesh_mask', inum0 )

            inum2 = inum0                                            ! put all the informations
            inum3 = inum0                                            ! in unit inum0
            inum4 = inum0

            !                                  ! ============================
         CASE ( 2 )                            !  create 'mesh.nc' and 
            !                                  !         'mask.nc' files
            !                                  ! ============================

            IF(lwp) WRITE(numout,*) '          two files in "mesh.nc" and "mask.nc" '
            CALL iom_open( 'mesh', inum1 )
            CALL iom_open( 'mask', inum2 )

            inum3 = inum1                                            ! put mesh informations 
            inum4 = inum1                                            ! in unit inum1 

            !                                  ! ============================
         CASE ( 3 )                            !  create 'mesh_hgr.nc'
            !                                  !         'mesh_zgr.nc' and
            !                                  !         'mask.nc'     files
            !                                  ! ============================

            IF(lwp) WRITE(numout,*) '          three files in "mesh_hgr.nc" , "mesh_zgr.nc" and "mask.nc" '
            CALL iom_open( 'mesh_hgr', inum3 ) ! create 'mesh_hgr.nc'
            CALL iom_open( 'mesh_zgr', inum4 ) ! create 'mesh_zgr.nc'
            CALL iom_open( 'mask'    , inum2 ) ! create 'mask.nc'

         END SELECT

         !                                                         ! masks (inum2) 
         CALL iom_get( inum2, jpdom_data, 'tmask', tmask )
         CALL iom_get( inum2, jpdom_data, 'umask', umask )
         CALL iom_get( inum2, jpdom_data, 'vmask', vmask )
         CALL iom_get( inum2, jpdom_data, 'fmask', fmask )

#if defined key_c1d
         ! set umask and vmask equal tmask in 1D configuration
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '**********  1D configuration : set umask and vmask equal tmask ********'
         IF(lwp) WRITE(numout,*) '**********                                                     ********'

         umask(:,:,:) = tmask(:,:,:)
         vmask(:,:,:) = tmask(:,:,:)
#endif

#if defined key_degrad
         CALL iom_get( inum2, jpdom_data, 'facvolt', facvol )
#endif

         !                                                         ! horizontal mesh (inum3)
         CALL iom_get( inum3, jpdom_data, 'glamt', glamt )
         CALL iom_get( inum3, jpdom_data, 'glamu', glamu )
         CALL iom_get( inum3, jpdom_data, 'glamv', glamv )
         CALL iom_get( inum3, jpdom_data, 'glamf', glamf )

         CALL iom_get( inum3, jpdom_data, 'gphit', gphit )
         CALL iom_get( inum3, jpdom_data, 'gphiu', gphiu )
         CALL iom_get( inum3, jpdom_data, 'gphiv', gphiv )
         CALL iom_get( inum3, jpdom_data, 'gphif', gphif )

         CALL iom_get( inum3, jpdom_data, 'e1t', e1t )
         CALL iom_get( inum3, jpdom_data, 'e1u', e1u )
         CALL iom_get( inum3, jpdom_data, 'e1v', e1v )
         
         CALL iom_get( inum3, jpdom_data, 'e2t', e2t )
         CALL iom_get( inum3, jpdom_data, 'e2u', e2u )
         CALL iom_get( inum3, jpdom_data, 'e2v', e2v )

         e1e2t(:,:) = e1t(:,:) * e2t(:,:)                              ! surface at T-points

         CALL iom_get( inum3, jpdom_data, 'ff', ff )

         CALL iom_get( inum4, jpdom_data, 'mbathy', zmbk )              ! number of ocean t-points
         mbathy(:,:) = INT( zmbk(:,:) )
         
         CALL zgr_bot_level                                             ! mbk. arrays (deepest ocean t-, u- & v-points
         !
         IF( ln_sco ) THEN                                         ! s-coordinate
            CALL iom_get( inum4, jpdom_data, 'hbatt', hbatt )
            CALL iom_get( inum4, jpdom_data, 'hbatu', hbatu )
            CALL iom_get( inum4, jpdom_data, 'hbatv', hbatv )
            CALL iom_get( inum4, jpdom_data, 'hbatf', hbatf )
            
            CALL iom_get( inum4, jpdom_unknown, 'gsigt', gsigt ) ! scaling coef.
            CALL iom_get( inum4, jpdom_unknown, 'gsigw', gsigw )
            CALL iom_get( inum4, jpdom_unknown, 'gsi3w', gsi3w ) 
            CALL iom_get( inum4, jpdom_unknown, 'esigt', esigt )
            CALL iom_get( inum4, jpdom_unknown, 'esigw', esigw )

            CALL iom_get( inum4, jpdom_data, 'e3t', e3t ) ! scale factors
            CALL iom_get( inum4, jpdom_data, 'e3u', e3u )
            CALL iom_get( inum4, jpdom_data, 'e3v', e3v )
            CALL iom_get( inum4, jpdom_data, 'e3w', e3w )

            CALL iom_get( inum4, jpdom_unknown, 'gdept_0', gdept_0 ) ! depth
            CALL iom_get( inum4, jpdom_unknown, 'gdepw_0', gdepw_0 )
         ENDIF

 
         IF( ln_zps ) THEN                                           ! z-coordinate - partial steps
            CALL iom_get( inum4, jpdom_unknown, 'gdept_0', gdept_0 )    ! reference depth
            CALL iom_get( inum4, jpdom_unknown, 'gdepw_0', gdepw_0 )
            CALL iom_get( inum4, jpdom_unknown, 'e3t_0'  , e3t_0   )    ! reference scale factors
            CALL iom_get( inum4, jpdom_unknown, 'e3w_0'  , e3w_0   )
            !
            IF( nmsh <= 6 ) THEN                                        ! 3D vertical scale factors
               CALL iom_get( inum4, jpdom_data, 'e3t', e3t )
               CALL iom_get( inum4, jpdom_data, 'e3u', e3u )
               CALL iom_get( inum4, jpdom_data, 'e3v', e3v )
               CALL iom_get( inum4, jpdom_data, 'e3w', e3w )
            ELSE                                                        ! 2D bottom scale factors
               CALL iom_get( inum4, jpdom_data, 'e3t_ps', e3tp )
               CALL iom_get( inum4, jpdom_data, 'e3w_ps', e3wp )
               !                                                        ! deduces the 3D scale factors
               DO jk = 1, jpk
                  e3t(:,:,jk) = e3t_0(jk)                                     ! set to the ref. factors
                  e3u(:,:,jk) = e3t_0(jk)
                  e3v(:,:,jk) = e3t_0(jk)
                  e3w(:,:,jk) = e3w_0(jk)
               END DO
               DO jj = 1,jpj                                                  ! adjust the deepest values
                  DO ji = 1,jpi
                     ik = mbkt(ji,jj)
                     e3t(ji,jj,ik) = e3tp(ji,jj) * tmask(ji,jj,1) + e3t_0(1) * ( 1._wp - tmask(ji,jj,1) )
                     e3w(ji,jj,ik) = e3wp(ji,jj) * tmask(ji,jj,1) + e3w_0(1) * ( 1._wp - tmask(ji,jj,1) )
                  END DO
               END DO
               DO jk = 1,jpk                         ! Computed as the minimum of neighbooring scale factors
                  DO jj = 1, jpjm1
                     DO ji = 1, jpim1
                        e3u(ji,jj,jk) = MIN( e3t(ji,jj,jk), e3t(ji+1,jj,jk) )
                        e3v(ji,jj,jk) = MIN( e3t(ji,jj,jk), e3t(ji,jj+1,jk) )
                     END DO
                  END DO
               END DO
               CALL lbc_lnk( e3u , 'U', 1._wp )   ;   CALL lbc_lnk( e3uw, 'U', 1._wp )   ! lateral boundary conditions
               CALL lbc_lnk( e3v , 'V', 1._wp )   ;   CALL lbc_lnk( e3vw, 'V', 1._wp )
               !
               DO jk = 1, jpk                        ! set to z-scale factor if zero (i.e. along closed boundaries)
                  WHERE( e3u(:,:,jk) == 0._wp )   e3u(:,:,jk) = e3t_0(jk)
                  WHERE( e3v(:,:,jk) == 0._wp )   e3v(:,:,jk) = e3t_0(jk)
               END DO
            END IF

            IF( iom_varid( inum4, 'gdept', ldstop = .FALSE. ) > 0 ) THEN   ! 3D depth of t- and w-level
               CALL iom_get( inum4, jpdom_data, 'gdept', gdept )
               CALL iom_get( inum4, jpdom_data, 'gdepw', gdepw )
            ELSE                                                           ! 2D bottom depth
               CALL iom_get( inum4, jpdom_data, 'hdept', zprt )
               CALL iom_get( inum4, jpdom_data, 'hdepw', zprw )
               !
               DO jk = 1, jpk                                              ! deduces the 3D depth
                  gdept(:,:,jk) = gdept_0(jk)
                  gdepw(:,:,jk) = gdepw_0(jk)
               END DO
               DO jj = 1, jpj
                  DO ji = 1, jpi
                     ik = mbkt(ji,jj)
                     IF( ik > 0 ) THEN
                        gdepw(ji,jj,ik+1) = zprw(ji,jj)
                        gdept(ji,jj,ik  ) = zprt(ji,jj)
                        gdept(ji,jj,ik+1) = gdept(ji,jj,ik) + e3t(ji,jj,ik)
                     ENDIF
                  END DO
               END DO
            ENDIF
            !
         ENDIF

         IF( ln_zco ) THEN           ! Vertical coordinates and scales factors
            CALL iom_get( inum4, jpdom_unknown, 'gdept_0', gdept_0 ) ! depth
            CALL iom_get( inum4, jpdom_unknown, 'gdepw_0', gdepw_0 )
            CALL iom_get( inum4, jpdom_unknown, 'e3t_0'  , e3t_0   )
            CALL iom_get( inum4, jpdom_unknown, 'e3w_0'  , e3w_0   )
            DO jk = 1, jpk
               e3t  (:,:,jk) = e3t_0(jk)                                     ! set to the ref. factors
               e3u  (:,:,jk) = e3t_0(jk)
               e3v  (:,:,jk) = e3t_0(jk)
               e3w  (:,:,jk) = e3w_0(jk)
               gdept(:,:,jk) = gdept_0(jk)
               gdepw(:,:,jk) = gdepw_0(jk)
            END DO
         ENDIF

!!gm BUG in s-coordinate this does not work!
      ! deepest/shallowest W level Above/Below ~10m
      zrefdep = 10._wp - ( 0.1_wp * MINVAL(e3w_0) )                  ! ref. depth with tolerance (10% of minimum layer thickness)
      nlb10 = MINLOC( gdepw_0, mask = gdepw_0 > zrefdep, dim = 1 )   ! shallowest W level Below ~10m
      nla10 = nlb10 - 1                                              ! deepest    W level Above ~10m
!!gm end bug

      ! Control printing : Grid informations (if not restart)
      ! ----------------

      IF(lwp .AND. .NOT.ln_rstart ) THEN
         WRITE(numout,*)
         WRITE(numout,*) '          longitude and e1 scale factors'
         WRITE(numout,*) '          ------------------------------'
         WRITE(numout,9300) ( ji, glamt(ji,1), glamu(ji,1),   &
            glamv(ji,1), glamf(ji,1),   &
            e1t(ji,1), e1u(ji,1),   &
            e1v(ji,1), ji = 1, jpi,10)
9300     FORMAT( 1x, i4, f8.2,1x, f8.2,1x, f8.2,1x, f8.2, 1x,    &
            f19.10, 1x, f19.10, 1x, f19.10 )

         WRITE(numout,*)
         WRITE(numout,*) '          latitude and e2 scale factors'
         WRITE(numout,*) '          -----------------------------'
         WRITE(numout,9300) ( jj, gphit(1,jj), gphiu(1,jj),   &
            &                     gphiv(1,jj), gphif(1,jj),   &
            &                     e2t  (1,jj), e2u  (1,jj),   &
            &                     e2v  (1,jj), jj = 1, jpj, 10 )
      ENDIF


      IF( nprint == 1 .AND. lwp ) THEN
         WRITE(numout,*) '          e1u e2u '
         CALL prihre( e1u,jpi,jpj,jpi-5,jpi,1,jpj-5,jpj,1,0.,numout )
         CALL prihre( e2u,jpi,jpj,jpi-5,jpi,1,jpj-5,jpj,1,0.,numout )
         WRITE(numout,*) '          e1v e2v  '
         CALL prihre( e1v,jpi,jpj,jpi-5,jpi,1,jpj-5,jpj,1,0.,numout )
         CALL prihre( e2v,jpi,jpj,jpi-5,jpi,1,jpj-5,jpj,1,0.,numout )
      ENDIF

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '              Reference z-coordinate depth and scale factors:'
         WRITE(numout, "(9x,' level   gdept    gdepw     e3t      e3w  ')" )
         WRITE(numout, "(10x, i4, 4f9.2)" ) ( jk, gdept_0(jk), gdepw_0(jk), e3t_0(jk), e3w_0(jk), jk = 1, jpk )
      ENDIF

      DO jk = 1, jpk
         IF( e3w_0  (jk) <= 0._wp .OR. e3t_0  (jk) <= 0._wp )   CALL ctl_stop( ' e3w_0 or e3t_0 =< 0 ' )
         IF( gdepw_0(jk) <  0._wp .OR. gdept_0(jk) <  0._wp )   CALL ctl_stop( ' gdepw_0 or gdept_0 < 0 ' )
      END DO
      !                                     ! ============================
      !                                     !        close the files 
      !                                     ! ============================
      SELECT CASE ( nmsh )
         CASE ( 1 )                
            CALL iom_close( inum0 )
         CASE ( 2 )
            CALL iom_close( inum1 )
            CALL iom_close( inum2 )
         CASE ( 3 )
            CALL iom_close( inum2 )
            CALL iom_close( inum3 )
            CALL iom_close( inum4 )
      END SELECT
      !
      CALL wrk_dealloc( jpi, jpj, zmbk, zprt, zprw )
      !
   END SUBROUTINE dom_rea


   SUBROUTINE zgr_bot_level
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE zgr_bot_level  ***
      !!
      !! ** Purpose :   defines the vertical index of ocean bottom (mbk. arrays)
      !!
      !! ** Method  :   computes from mbathy with a minimum value of 1 over land
      !!
      !! ** Action  : - update mbathy: level bathymetry (in level index)
      !!----------------------------------------------------------------------
      !
      INTEGER ::   ji, jj   ! dummy loop indices
      REAL(wp), POINTER, DIMENSION(:,:) :: zmbk
      !!----------------------------------------------------------------------

      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '    zgr_bot_level : ocean bottom k-index of T-, U-, V- and W-levels '
      IF(lwp) WRITE(numout,*) '    ~~~~~~~~~~~~~'
      !
      CALL wrk_alloc( jpi, jpj, zmbk )
      !
      mbkt(:,:) = MAX( mbathy(:,:) , 1 )    ! bottom k-index of T-level (=1 over land)
      !
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
   END SUBROUTINE zgr_bot_level

   !!======================================================================
END MODULE domrea
