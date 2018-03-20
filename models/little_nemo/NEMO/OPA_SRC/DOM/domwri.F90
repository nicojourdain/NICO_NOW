MODULE domwri
   !!======================================================================
   !!                       ***  MODULE domwri  ***
   !! Ocean initialization : write the ocean domain mesh file(s)
   !!======================================================================
   !! History :  OPA  ! 1997-02  (G. Madec)  Original code
   !!            8.1  ! 1999-11  (M. Imbard)  NetCDF FORMAT with IOIPSL
   !!   NEMO     1.0  ! 2002-08  (G. Madec)  F90 and several file
   !!            3.0  ! 2008-01  (S. Masson) add dom_uniq 
   !!            4.0  ! 2011-01  (A. R. Porter, STFC Daresbury) dynamical allocation
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_wri        : create and write mesh and mask file(s)
   !!   dom_uniq       :
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE in_out_manager  ! I/O manager
   USE iom             ! I/O library
   USE lbclnk          ! lateral boundary conditions - mpp exchanges
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! Memory allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC dom_wri        ! routine called by inidom.F90

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: domwri.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_wri
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dom_wri  ***
      !!                   
      !! ** Purpose :   Create the NetCDF file(s) which contain(s) all the
      !!      ocean domain informations (mesh and mask arrays). This (these)
      !!      file(s) is (are) used for visualisation (SAXO software) and
      !!      diagnostic computation.
      !!
      !! ** Method  :   Write in a file all the arrays generated in routines
      !!      domhgr, domzgr, and dommsk. Note: the file contain depends on
      !!      the vertical coord. used (z-coord, partial steps, s-coord)
      !!            MOD(nmsh, 3) = 1  :   'mesh_mask.nc' file
      !!                         = 2  :   'mesh.nc' and mask.nc' files
      !!                         = 0  :   'mesh_hgr.nc', 'mesh_zgr.nc' and
      !!                                  'mask.nc' files
      !!      For huge size domain, use option 2 or 3 depending on your 
      !!      vertical coordinate.
      !!
      !!      if     nmsh <= 3: write full 3D arrays for e3[tuvw] and gdep[tuvw]
      !!      if 3 < nmsh <= 6: write full 3D arrays for e3[tuvw] and 2D arrays 
      !!                        corresponding to the depth of the bottom t- and w-points
      !!      if 6 < nmsh <= 9: write 2D arrays corresponding to the depth and the
      !!                        thickness (e3[tw]_ps) of the bottom points 
      !!
      !! ** output file :   meshmask.nc  : domain size, horizontal grid-point position,
      !!                                   masks, depth and vertical scale factors
      !!----------------------------------------------------------------------
      !!
      INTEGER           ::   inum0    ! temprary units for 'mesh_mask.nc' file
      INTEGER           ::   inum1    ! temprary units for 'mesh.nc'      file
      INTEGER           ::   inum2    ! temprary units for 'mask.nc'      file
      INTEGER           ::   inum3    ! temprary units for 'mesh_hgr.nc'  file
      INTEGER           ::   inum4    ! temprary units for 'mesh_zgr.nc'  file
      CHARACTER(len=21) ::   clnam0   ! filename (mesh and mask informations)
      CHARACTER(len=21) ::   clnam1   ! filename (mesh informations)
      CHARACTER(len=21) ::   clnam2   ! filename (mask informations)
      CHARACTER(len=21) ::   clnam3   ! filename (horizontal mesh informations)
      CHARACTER(len=21) ::   clnam4   ! filename (vertical   mesh informations)
      INTEGER           ::   ji, jj, jk   ! dummy loop indices
      !                                   !  workspaces
      REAL(wp), POINTER, DIMENSION(:,:  ) :: zprt, zprw 
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zdepu, zdepv
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dom_wri')
      !
      CALL wrk_alloc( jpi, jpj, zprt, zprw )
      CALL wrk_alloc( jpi, jpj, jpk, zdepu, zdepv )
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'dom_wri : create NetCDF mesh and mask information file(s)'
      IF(lwp) WRITE(numout,*) '~~~~~~~'
      
      clnam0 = 'mesh_mask'  ! filename (mesh and mask informations)
      clnam1 = 'mesh'       ! filename (mesh informations)
      clnam2 = 'mask'       ! filename (mask informations)
      clnam3 = 'mesh_hgr'   ! filename (horizontal mesh informations)
      clnam4 = 'mesh_zgr'   ! filename (vertical   mesh informations)
      
      SELECT CASE ( MOD(nmsh, 3) )
         !                                  ! ============================
      CASE ( 1 )                            !  create 'mesh_mask.nc' file
         !                                  ! ============================
         CALL iom_open( TRIM(clnam0), inum0, ldwrt = .TRUE., kiolib = jprstlib )
         inum2 = inum0                                            ! put all the informations
         inum3 = inum0                                            ! in unit inum0
         inum4 = inum0
         
         !                                  ! ============================
      CASE ( 2 )                            !  create 'mesh.nc' and 
         !                                  !         'mask.nc' files
         !                                  ! ============================
         CALL iom_open( TRIM(clnam1), inum1, ldwrt = .TRUE., kiolib = jprstlib )
         CALL iom_open( TRIM(clnam2), inum2, ldwrt = .TRUE., kiolib = jprstlib )
         inum3 = inum1                                            ! put mesh informations 
         inum4 = inum1                                            ! in unit inum1 
         !                                  ! ============================
      CASE ( 0 )                            !  create 'mesh_hgr.nc'
         !                                  !         'mesh_zgr.nc' and
         !                                  !         'mask.nc'     files
         !                                  ! ============================
         CALL iom_open( TRIM(clnam2), inum2, ldwrt = .TRUE., kiolib = jprstlib )
         CALL iom_open( TRIM(clnam3), inum3, ldwrt = .TRUE., kiolib = jprstlib )
         CALL iom_open( TRIM(clnam4), inum4, ldwrt = .TRUE., kiolib = jprstlib )
         !
      END SELECT
      
      !                                                         ! masks (inum2) 
      CALL iom_rstput( 0, 0, inum2, 'tmask', tmask, ktype = jp_i1 )     !    ! land-sea mask
      CALL iom_rstput( 0, 0, inum2, 'umask', umask, ktype = jp_i1 )
      CALL iom_rstput( 0, 0, inum2, 'vmask', vmask, ktype = jp_i1 )
      CALL iom_rstput( 0, 0, inum2, 'fmask', fmask, ktype = jp_i1 )
      
      CALL dom_uniq( zprw, 'T' )
      zprt = tmask(:,:,1) * zprw                               !    ! unique point mask
      CALL iom_rstput( 0, 0, inum2, 'tmaskutil', zprt, ktype = jp_i1 )  
      CALL dom_uniq( zprw, 'U' )
      zprt = umask(:,:,1) * zprw
      CALL iom_rstput( 0, 0, inum2, 'umaskutil', zprt, ktype = jp_i1 )  
      CALL dom_uniq( zprw, 'V' )
      zprt = vmask(:,:,1) * zprw
      CALL iom_rstput( 0, 0, inum2, 'vmaskutil', zprt, ktype = jp_i1 )  
      CALL dom_uniq( zprw, 'F' )
      zprt = fmask(:,:,1) * zprw
      CALL iom_rstput( 0, 0, inum2, 'fmaskutil', zprt, ktype = jp_i1 )  

      !                                                         ! horizontal mesh (inum3)
      CALL iom_rstput( 0, 0, inum3, 'glamt', glamt, ktype = jp_r4 )     !    ! latitude
      CALL iom_rstput( 0, 0, inum3, 'glamu', glamu, ktype = jp_r4 )
      CALL iom_rstput( 0, 0, inum3, 'glamv', glamv, ktype = jp_r4 )
      CALL iom_rstput( 0, 0, inum3, 'glamf', glamf, ktype = jp_r4 )
      
      CALL iom_rstput( 0, 0, inum3, 'gphit', gphit, ktype = jp_r4 )     !    ! longitude
      CALL iom_rstput( 0, 0, inum3, 'gphiu', gphiu, ktype = jp_r4 )
      CALL iom_rstput( 0, 0, inum3, 'gphiv', gphiv, ktype = jp_r4 )
      CALL iom_rstput( 0, 0, inum3, 'gphif', gphif, ktype = jp_r4 )
      
      CALL iom_rstput( 0, 0, inum3, 'e1t', e1t, ktype = jp_r8 )         !    ! e1 scale factors
      CALL iom_rstput( 0, 0, inum3, 'e1u', e1u, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum3, 'e1v', e1v, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum3, 'e1f', e1f, ktype = jp_r8 )
      
      CALL iom_rstput( 0, 0, inum3, 'e2t', e2t, ktype = jp_r8 )         !    ! e2 scale factors
      CALL iom_rstput( 0, 0, inum3, 'e2u', e2u, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum3, 'e2v', e2v, ktype = jp_r8 )
      CALL iom_rstput( 0, 0, inum3, 'e2f', e2f, ktype = jp_r8 )
      
      CALL iom_rstput( 0, 0, inum3, 'ff', ff, ktype = jp_r8 )           !    ! coriolis factor
      
      ! note that mbkt is set to 1 over land ==> use surface tmask
      zprt(:,:) = tmask(:,:,1) * REAL( mbkt(:,:) , wp )
      CALL iom_rstput( 0, 0, inum4, 'mbathy', zprt, ktype = jp_i2 )     !    ! nb of ocean T-points
            
      IF( ln_sco ) THEN                                         ! s-coordinate
         CALL iom_rstput( 0, 0, inum4, 'hbatt', hbatt )         !    ! depth
         CALL iom_rstput( 0, 0, inum4, 'hbatu', hbatu ) 
         CALL iom_rstput( 0, 0, inum4, 'hbatv', hbatv )
         CALL iom_rstput( 0, 0, inum4, 'hbatf', hbatf )
         !
         CALL iom_rstput( 0, 0, inum4, 'gsigt', gsigt )         !    ! scaling coef.
         CALL iom_rstput( 0, 0, inum4, 'gsigw', gsigw )  
         CALL iom_rstput( 0, 0, inum4, 'gsi3w', gsi3w )
         CALL iom_rstput( 0, 0, inum4, 'esigt', esigt )
         CALL iom_rstput( 0, 0, inum4, 'esigw', esigw )
         !
         CALL iom_rstput( 0, 0, inum4, 'e3t', e3t )             !    ! scale factors
         CALL iom_rstput( 0, 0, inum4, 'e3u', e3u )
         CALL iom_rstput( 0, 0, inum4, 'e3v', e3v )
         CALL iom_rstput( 0, 0, inum4, 'e3w', e3w )
         !
         CALL iom_rstput( 0, 0, inum4, 'gdept_0' , gdept_0 )    !    ! stretched system
         CALL iom_rstput( 0, 0, inum4, 'gdepw_0' , gdepw_0 )
      ENDIF
      
      IF( ln_zps ) THEN                                         ! z-coordinate - partial steps
         !
         IF( nmsh <= 6 ) THEN                                   !    ! 3D vertical scale factors
            CALL iom_rstput( 0, 0, inum4, 'e3t', e3t )         
            CALL iom_rstput( 0, 0, inum4, 'e3u', e3u )
            CALL iom_rstput( 0, 0, inum4, 'e3v', e3v )
            CALL iom_rstput( 0, 0, inum4, 'e3w', e3w )
         ELSE                                                   !    ! 2D masked bottom ocean scale factors
            DO jj = 1,jpj   
               DO ji = 1,jpi
                  e3tp(ji,jj) = e3t(ji,jj,mbkt(ji,jj)) * tmask(ji,jj,1)
                  e3wp(ji,jj) = e3w(ji,jj,mbkt(ji,jj)) * tmask(ji,jj,1)
               END DO
            END DO
            CALL iom_rstput( 0, 0, inum4, 'e3t_ps', e3tp )      
            CALL iom_rstput( 0, 0, inum4, 'e3w_ps', e3wp )
         END IF
         !
         IF( nmsh <= 3 ) THEN                                   !    ! 3D depth
            CALL iom_rstput( 0, 0, inum4, 'gdept', gdept, ktype = jp_r4 )     
            DO jk = 1,jpk   
               DO jj = 1, jpjm1   
                  DO ji = 1, fs_jpim1   ! vector opt.
                     zdepu(ji,jj,jk) = MIN( gdept(ji,jj,jk) , gdept(ji+1,jj  ,jk) )
                     zdepv(ji,jj,jk) = MIN( gdept(ji,jj,jk) , gdept(ji  ,jj+1,jk) )
                  END DO   
               END DO   
            END DO
            CALL lbc_lnk( zdepu, 'U', 1. )   ;   CALL lbc_lnk( zdepv, 'V', 1. ) 
            CALL iom_rstput( 0, 0, inum4, 'gdepu', zdepu, ktype = jp_r4 )
            CALL iom_rstput( 0, 0, inum4, 'gdepv', zdepv, ktype = jp_r4 )
            CALL iom_rstput( 0, 0, inum4, 'gdepw', gdepw, ktype = jp_r4 )
         ENDIF
         DO jj = 1,jpj   
            DO ji = 1,jpi
               zprt(ji,jj) = gdept(ji,jj,mbkt(ji,jj)  ) * tmask(ji,jj,1)
               zprw(ji,jj) = gdepw(ji,jj,mbkt(ji,jj)+1) * tmask(ji,jj,1)
            END DO
         END DO
         CALL iom_rstput( 0, 0, inum4, 'hdept', zprt, ktype = jp_r4 )     
         CALL iom_rstput( 0, 0, inum4, 'hdepw', zprw, ktype = jp_r4 ) 
         !
         CALL iom_rstput( 0, 0, inum4, 'gdept_0', gdept_0 )     !    ! reference z-coord.
         CALL iom_rstput( 0, 0, inum4, 'gdepw_0', gdepw_0 )
         CALL iom_rstput( 0, 0, inum4, 'e3t_0'  , e3t_0   )
         CALL iom_rstput( 0, 0, inum4, 'e3w_0'  , e3w_0   )
      ENDIF
      
      IF( ln_zco ) THEN
         !                                                      ! z-coordinate - full steps
         CALL iom_rstput( 0, 0, inum4, 'gdept_0', gdept_0 )     !    ! depth
         CALL iom_rstput( 0, 0, inum4, 'gdepw_0', gdepw_0 )
         CALL iom_rstput( 0, 0, inum4, 'e3t_0'  , e3t_0   )     !    ! scale factors
         CALL iom_rstput( 0, 0, inum4, 'e3w_0'  , e3w_0   )
      ENDIF
      !                                     ! ============================
      !                                     !        close the files 
      !                                     ! ============================
      SELECT CASE ( MOD(nmsh, 3) )
      CASE ( 1 )                
         CALL iom_close( inum0 )
      CASE ( 2 )
         CALL iom_close( inum1 )
         CALL iom_close( inum2 )
      CASE ( 0 )
         CALL iom_close( inum2 )
         CALL iom_close( inum3 )
         CALL iom_close( inum4 )
      END SELECT
      !
      CALL wrk_dealloc( jpi, jpj, zprt, zprw )
      CALL wrk_dealloc( jpi, jpj, jpk, zdepu, zdepv )
      !
      IF( nn_timing == 1 )  CALL timing_stop('dom_wri')
      !
   END SUBROUTINE dom_wri


   SUBROUTINE dom_uniq( puniq, cdgrd )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dom_uniq  ***
      !!                   
      !! ** Purpose :   identify unique point of a grid (TUVF)
      !!
      !! ** Method  :   1) aplly lbc_lnk on an array with different values for each element
      !!                2) check which elements have been changed
      !!----------------------------------------------------------------------
      !
      CHARACTER(len=1)        , INTENT(in   ) ::   cdgrd   ! 
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::   puniq   ! 
      !
      REAL(wp) ::  zshift   ! shift value link to the process number
      INTEGER  ::  ji       ! dummy loop indices
      LOGICAL, DIMENSION(SIZE(puniq,1),SIZE(puniq,2),1) ::  lldbl  ! store whether each point is unique or not
      REAL(wp), POINTER, DIMENSION(:,:) :: ztstref
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dom_uniq')
      !
      CALL wrk_alloc( jpi, jpj, ztstref )
      !
      ! build an array with different values for each element 
      ! in mpp: make sure that these values are different even between process
      ! -> apply a shift value according to the process number
      zshift = jpi * jpj * ( narea - 1 )
      ztstref(:,:) = RESHAPE( (/ (zshift + REAL(ji,wp), ji = 1, jpi*jpj) /), (/ jpi, jpj /) )
      !
      puniq(:,:) = ztstref(:,:)                   ! default definition
      CALL lbc_lnk( puniq, cdgrd, 1. )            ! apply boundary conditions
      lldbl(:,:,1) = puniq(:,:) == ztstref(:,:)   ! check which values have been changed 
      !
      puniq(:,:) = 1.                             ! default definition
      ! fill only the inner part of the cpu with llbl converted into real 
      puniq(nldi:nlei,nldj:nlej) = REAL( COUNT( lldbl(nldi:nlei,nldj:nlej,:), dim = 3 ) , wp )
      !
      CALL wrk_dealloc( jpi, jpj, ztstref )
      !
      IF( nn_timing == 1 )  CALL timing_stop('dom_uniq')
      !
   END SUBROUTINE dom_uniq

   !!======================================================================
END MODULE domwri
