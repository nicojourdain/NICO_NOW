MODULE geo2ocean
   !!======================================================================
   !!                     ***  MODULE  geo2ocean  ***
   !! Ocean mesh    :  ???
   !!======================================================================
   !! History :  OPA  !  07-1996  (O. Marti)  Original code
   !!   NEMO     1.0  !  02-2008  (G. Madec)  F90: Free form
   !!            3.0  !  
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   repcmo      : 
   !!   angle       :
   !!   geo2oce     :
   !!   repere      :   old routine suppress it ???
   !!----------------------------------------------------------------------
   USE dom_oce         ! mesh and scale factors
   USE phycst          ! physical constants
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp         ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   rot_rep, repcmo, repere, geo2oce, oce2geo   ! only rot_rep should be used
                                             ! repcmo and repere are keep only for compatibility.
                                             ! they are only a useless overlay of rot_rep

   PUBLIC   obs_rot

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   &
      gsint, gcost,   &  ! cos/sin between model grid lines and NP direction at T point
      gsinu, gcosu,   &  ! cos/sin between model grid lines and NP direction at U point
      gsinv, gcosv,   &  ! cos/sin between model grid lines and NP direction at V point
      gsinf, gcosf       ! cos/sin between model grid lines and NP direction at F point

   LOGICAL ,              SAVE, DIMENSION(4)     ::   linit = .FALSE.
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   gsinlon, gcoslon, gsinlat, gcoslat

   LOGICAL ::   lmust_init = .TRUE.        !: used to initialize the cos/sin variables (se above)

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: geo2ocean.F90 2715 2011-03-30 15:58:35Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE repcmo ( pxu1, pyu1, pxv1, pyv1,   &
                       px2 , py2 )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE repcmo  ***
      !!
      !! ** Purpose :   Change vector componantes from a geographic grid to a
      !!      stretched coordinates grid.
      !!
      !! ** Method  :   Initialization of arrays at the first call.
      !!
      !! ** Action  : - px2 : first  componante (defined at u point)
      !!              - py2 : second componante (defined at v point)
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   ), DIMENSION(jpi,jpj) ::   pxu1, pyu1   ! geographic vector componantes at u-point
      REAL(wp), INTENT(in   ), DIMENSION(jpi,jpj) ::   pxv1, pyv1   ! geographic vector componantes at v-point
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   px2          ! i-componante (defined at u-point)
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   py2          ! j-componante (defined at v-point)
      !!----------------------------------------------------------------------
      
      ! Change from geographic to stretched coordinate
      ! ----------------------------------------------
      CALL rot_rep( pxu1, pyu1, 'U', 'en->i',px2 )
      CALL rot_rep( pxv1, pyv1, 'V', 'en->j',py2 )
      
   END SUBROUTINE repcmo


   SUBROUTINE rot_rep ( pxin, pyin, cd_type, cdtodo, prot )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE rot_rep  ***
      !!
      !! ** Purpose :   Rotate the Repere: Change vector componantes between
      !!                geographic grid <--> stretched coordinates grid.
      !!
      !! History :
      !!   9.2  !  07-04  (S. Masson)  
      !!                  (O. Marti ) Original code (repere and repcmo)
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT( IN ) ::   pxin, pyin   ! vector componantes
      CHARACTER(len=1),             INTENT( IN ) ::   cd_type      ! define the nature of pt2d array grid-points
      CHARACTER(len=5),             INTENT( IN ) ::   cdtodo       ! specify the work to do:
      !!                                                           ! 'en->i' east-north componantes to model i componante
      !!                                                           ! 'en->j' east-north componantes to model j componante
      !!                                                           ! 'ij->e' model i-j componantes to east componante
      !!                                                           ! 'ij->n' model i-j componantes to east componante
      REAL(wp), DIMENSION(jpi,jpj), INTENT(out) ::   prot      
      !!----------------------------------------------------------------------

      ! Initialization of gsin* and gcos* at first call
      ! -----------------------------------------------

      IF( lmust_init ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' rot_rep : geographic <--> stretched'
         IF(lwp) WRITE(numout,*) ' ~~~~~    coordinate transformation'
         !
         CALL angle       ! initialization of the transformation
         lmust_init = .FALSE.
      ENDIF
      
      SELECT CASE (cdtodo)
      CASE ('en->i')      ! 'en->i' est-north componantes to model i componante
         SELECT CASE (cd_type)
         CASE ('T')   ;   prot(:,:) = pxin(:,:) * gcost(:,:) + pyin(:,:) * gsint(:,:)
         CASE ('U')   ;   prot(:,:) = pxin(:,:) * gcosu(:,:) + pyin(:,:) * gsinu(:,:)
         CASE ('V')   ;   prot(:,:) = pxin(:,:) * gcosv(:,:) + pyin(:,:) * gsinv(:,:)
         CASE ('F')   ;   prot(:,:) = pxin(:,:) * gcosf(:,:) + pyin(:,:) * gsinf(:,:)
         CASE DEFAULT   ;   CALL ctl_stop( 'Only T, U, V and F grid points are coded' )
         END SELECT
      CASE ('en->j')      ! 'en->j' est-north componantes to model j componante
         SELECT CASE (cd_type)
         CASE ('T')   ;   prot(:,:) = pyin(:,:) * gcost(:,:) - pxin(:,:) * gsint(:,:)
         CASE ('U')   ;   prot(:,:) = pyin(:,:) * gcosu(:,:) - pxin(:,:) * gsinu(:,:)
         CASE ('V')   ;   prot(:,:) = pyin(:,:) * gcosv(:,:) - pxin(:,:) * gsinv(:,:)   
         CASE ('F')   ;   prot(:,:) = pyin(:,:) * gcosf(:,:) - pxin(:,:) * gsinf(:,:)   
         CASE DEFAULT   ;   CALL ctl_stop( 'Only T, U, V and F grid points are coded' )
         END SELECT
      CASE ('ij->e')      ! 'ij->e' model i-j componantes to est componante
         SELECT CASE (cd_type)
         CASE ('T')   ;   prot(:,:) = pxin(:,:) * gcost(:,:) - pyin(:,:) * gsint(:,:)
         CASE ('U')   ;   prot(:,:) = pxin(:,:) * gcosu(:,:) - pyin(:,:) * gsinu(:,:)
         CASE ('V')   ;   prot(:,:) = pxin(:,:) * gcosv(:,:) - pyin(:,:) * gsinv(:,:)
         CASE ('F')   ;   prot(:,:) = pxin(:,:) * gcosf(:,:) - pyin(:,:) * gsinf(:,:)
         CASE DEFAULT   ;   CALL ctl_stop( 'Only T, U, V and F grid points are coded' )
         END SELECT
      CASE ('ij->n')      ! 'ij->n' model i-j componantes to est componante
         SELECT CASE (cd_type)
         CASE ('T')   ;   prot(:,:) = pyin(:,:) * gcost(:,:) + pxin(:,:) * gsint(:,:)
         CASE ('U')   ;   prot(:,:) = pyin(:,:) * gcosu(:,:) + pxin(:,:) * gsinu(:,:)
         CASE ('V')   ;   prot(:,:) = pyin(:,:) * gcosv(:,:) + pxin(:,:) * gsinv(:,:)
         CASE ('F')   ;   prot(:,:) = pyin(:,:) * gcosf(:,:) + pxin(:,:) * gsinf(:,:)
         CASE DEFAULT   ;   CALL ctl_stop( 'Only T, U, V and F grid points are coded' )
         END SELECT
      CASE DEFAULT   ;   CALL ctl_stop( 'rot_rep: Syntax Error in the definition of cdtodo' )
      END SELECT
      
   END SUBROUTINE rot_rep


   SUBROUTINE angle
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE angle  ***
      !! 
      !! ** Purpose :   Compute angles between model grid lines and the North direction
      !!
      !! ** Method  :
      !!
      !! ** Action  :   Compute (gsint, gcost, gsinu, gcosu, gsinv, gcosv, gsinf, gcosf) arrays:
      !!      sinus and cosinus of the angle between the north-south axe and the 
      !!      j-direction at t, u, v and f-points
      !!
      !! History :
      !!   7.0  !  96-07  (O. Marti )  Original code
      !!   8.0  !  98-06  (G. Madec )
      !!   8.5  !  98-06  (G. Madec )  Free form, F90 + opt.
      !!   9.2  !  07-04  (S. Masson)  Add T, F points and bugfix in cos lateral boundary
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj   ! dummy loop indices
      INTEGER ::   ierr     ! local integer
      REAL(wp) ::   &
         zlam, zphi,            &  ! temporary scalars
         zlan, zphh,            &  !    "         "
         zxnpt, zynpt, znnpt,   &  ! x,y components and norm of the vector: T point to North Pole
         zxnpu, zynpu, znnpu,   &  ! x,y components and norm of the vector: U point to North Pole
         zxnpv, zynpv, znnpv,   &  ! x,y components and norm of the vector: V point to North Pole
         zxnpf, zynpf, znnpf,   &  ! x,y components and norm of the vector: F point to North Pole
         zxvvt, zyvvt, znvvt,   &  ! x,y components and norm of the vector: between V points below and above a T point
         zxffu, zyffu, znffu,   &  ! x,y components and norm of the vector: between F points below and above a U point
         zxffv, zyffv, znffv,   &  ! x,y components and norm of the vector: between F points left  and right a V point
         zxuuf, zyuuf, znuuf       ! x,y components and norm of the vector: between U points below and above a F point
      !!----------------------------------------------------------------------

      ALLOCATE( gsint(jpi,jpj), gcost(jpi,jpj),   & 
         &      gsinu(jpi,jpj), gcosu(jpi,jpj),   & 
         &      gsinv(jpi,jpj), gcosv(jpi,jpj),   &  
         &      gsinf(jpi,jpj), gcosf(jpi,jpj), STAT=ierr )
      IF(lk_mpp)   CALL mpp_sum( ierr )
      IF( ierr /= 0 )   CALL ctl_stop('STOP', 'angle_msh_geo: unable to allocate arrays' )

      ! ============================= !
      ! Compute the cosinus and sinus !
      ! ============================= !
      ! (computation done on the north stereographic polar plane)

      DO jj = 2, jpjm1
!CDIR NOVERRCHK
         DO ji = fs_2, jpi   ! vector opt.

            ! north pole direction & modulous (at t-point)
            zlam = glamt(ji,jj)
            zphi = gphit(ji,jj)
            zxnpt = 0. - 2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            zynpt = 0. - 2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            znnpt = zxnpt*zxnpt + zynpt*zynpt

            ! north pole direction & modulous (at u-point)
            zlam = glamu(ji,jj)
            zphi = gphiu(ji,jj)
            zxnpu = 0. - 2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            zynpu = 0. - 2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            znnpu = zxnpu*zxnpu + zynpu*zynpu

            ! north pole direction & modulous (at v-point)
            zlam = glamv(ji,jj)
            zphi = gphiv(ji,jj)
            zxnpv = 0. - 2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            zynpv = 0. - 2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            znnpv = zxnpv*zxnpv + zynpv*zynpv

            ! north pole direction & modulous (at f-point)
            zlam = glamf(ji,jj)
            zphi = gphif(ji,jj)
            zxnpf = 0. - 2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            zynpf = 0. - 2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            znnpf = zxnpf*zxnpf + zynpf*zynpf

            ! j-direction: v-point segment direction (around t-point)
            zlam = glamv(ji,jj  )
            zphi = gphiv(ji,jj  )
            zlan = glamv(ji,jj-1)
            zphh = gphiv(ji,jj-1)
            zxvvt =  2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * COS( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            zyvvt =  2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * SIN( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            znvvt = SQRT( znnpt * ( zxvvt*zxvvt + zyvvt*zyvvt )  )
            znvvt = MAX( znvvt, 1.e-14 )

            ! j-direction: f-point segment direction (around u-point)
            zlam = glamf(ji,jj  )
            zphi = gphif(ji,jj  )
            zlan = glamf(ji,jj-1)
            zphh = gphif(ji,jj-1)
            zxffu =  2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * COS( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            zyffu =  2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * SIN( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            znffu = SQRT( znnpu * ( zxffu*zxffu + zyffu*zyffu )  )
            znffu = MAX( znffu, 1.e-14 )

            ! i-direction: f-point segment direction (around v-point)
            zlam = glamf(ji  ,jj)
            zphi = gphif(ji  ,jj)
            zlan = glamf(ji-1,jj)
            zphh = gphif(ji-1,jj)
            zxffv =  2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * COS( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            zyffv =  2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * SIN( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            znffv = SQRT( znnpv * ( zxffv*zxffv + zyffv*zyffv )  )
            znffv = MAX( znffv, 1.e-14 )

            ! j-direction: u-point segment direction (around f-point)
            zlam = glamu(ji,jj+1)
            zphi = gphiu(ji,jj+1)
            zlan = glamu(ji,jj  )
            zphh = gphiu(ji,jj  )
            zxuuf =  2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * COS( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            zyuuf =  2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * SIN( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            znuuf = SQRT( znnpf * ( zxuuf*zxuuf + zyuuf*zyuuf )  )
            znuuf = MAX( znuuf, 1.e-14 )

            ! cosinus and sinus using scalar and vectorial products
            gsint(ji,jj) = ( zxnpt*zyvvt - zynpt*zxvvt ) / znvvt
            gcost(ji,jj) = ( zxnpt*zxvvt + zynpt*zyvvt ) / znvvt

            gsinu(ji,jj) = ( zxnpu*zyffu - zynpu*zxffu ) / znffu
            gcosu(ji,jj) = ( zxnpu*zxffu + zynpu*zyffu ) / znffu

            gsinf(ji,jj) = ( zxnpf*zyuuf - zynpf*zxuuf ) / znuuf
            gcosf(ji,jj) = ( zxnpf*zxuuf + zynpf*zyuuf ) / znuuf

            ! (caution, rotation of 90 degres)
            gsinv(ji,jj) = ( zxnpv*zxffv + zynpv*zyffv ) / znffv
            gcosv(ji,jj) =-( zxnpv*zyffv - zynpv*zxffv ) / znffv

         END DO
      END DO

      ! =============== !
      ! Geographic mesh !
      ! =============== !

      DO jj = 2, jpjm1
         DO ji = fs_2, jpi   ! vector opt.
            IF( MOD( ABS( glamv(ji,jj) - glamv(ji,jj-1) ), 360. ) < 1.e-8 ) THEN
               gsint(ji,jj) = 0.
               gcost(ji,jj) = 1.
            ENDIF
            IF( MOD( ABS( glamf(ji,jj) - glamf(ji,jj-1) ), 360. ) < 1.e-8 ) THEN
               gsinu(ji,jj) = 0.
               gcosu(ji,jj) = 1.
            ENDIF
            IF(      ABS( gphif(ji,jj) - gphif(ji-1,jj) )         < 1.e-8 ) THEN
               gsinv(ji,jj) = 0.
               gcosv(ji,jj) = 1.
            ENDIF
            IF( MOD( ABS( glamu(ji,jj) - glamu(ji,jj+1) ), 360. ) < 1.e-8 ) THEN
               gsinf(ji,jj) = 0.
               gcosf(ji,jj) = 1.
            ENDIF
         END DO
      END DO

      ! =========================== !
      ! Lateral boundary conditions !
      ! =========================== !

      ! lateral boundary cond.: T-, U-, V-, F-pts, sgn
      CALL lbc_lnk( gcost, 'T', -1. )   ;   CALL lbc_lnk( gsint, 'T', -1. )
      CALL lbc_lnk( gcosu, 'U', -1. )   ;   CALL lbc_lnk( gsinu, 'U', -1. )
      CALL lbc_lnk( gcosv, 'V', -1. )   ;   CALL lbc_lnk( gsinv, 'V', -1. )
      CALL lbc_lnk( gcosf, 'F', -1. )   ;   CALL lbc_lnk( gsinf, 'F', -1. )

   END SUBROUTINE angle


   SUBROUTINE geo2oce ( pxx, pyy, pzz, cgrid,     &
                        pte, ptn )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE geo2oce  ***
      !!      
      !! ** Purpose :
      !!
      !! ** Method  :   Change wind stress from geocentric to east/north
      !!
      !! History :
      !!        !         (O. Marti)  Original code
      !!        !  91-03  (G. Madec)
      !!        !  92-07  (M. Imbard)
      !!        !  99-11  (M. Imbard) NetCDF format with IOIPSL
      !!        !  00-08  (D. Ludicone) Reduced section at Bab el Mandeb
      !!   8.5  !  02-06  (G. Madec)  F90: Free form
      !!   3.0  !  07-08  (G. Madec)  geo2oce suppress lon/lat agruments
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   ) ::  pxx, pyy, pzz
      CHARACTER(len=1)            , INTENT(in   ) ::  cgrid
      REAL(wp), DIMENSION(jpi,jpj), INTENT(  out) ::  pte, ptn
      !!
      REAL(wp), PARAMETER :: rpi = 3.141592653e0
      REAL(wp), PARAMETER :: rad = rpi / 180.e0
      INTEGER ::   ig     !
      INTEGER ::   ierr   ! local integer
      !!----------------------------------------------------------------------

      IF( .NOT. ALLOCATED( gsinlon ) ) THEN
         ALLOCATE( gsinlon(jpi,jpj,4) , gcoslon(jpi,jpj,4) ,   &
            &      gsinlat(jpi,jpj,4) , gcoslat(jpi,jpj,4) , STAT=ierr )
         IF( lk_mpp    )   CALL mpp_sum( ierr )
         IF( ierr /= 0 )   CALL ctl_stop('STOP', 'angle_msh_geo: unable to allocate arrays' )
      ENDIF

      SELECT CASE( cgrid)
         CASE ( 'T' )   
            ig = 1
            IF( .NOT. linit(ig) ) THEN 
               gsinlon(:,:,ig) = SIN( rad * glamt(:,:) )
               gcoslon(:,:,ig) = COS( rad * glamt(:,:) )
               gsinlat(:,:,ig) = SIN( rad * gphit(:,:) )
               gcoslat(:,:,ig) = COS( rad * gphit(:,:) )
               linit(ig) = .TRUE.
            ENDIF
         CASE ( 'U' )   
            ig = 2
            IF( .NOT. linit(ig) ) THEN 
               gsinlon(:,:,ig) = SIN( rad * glamu(:,:) )
               gcoslon(:,:,ig) = COS( rad * glamu(:,:) )
               gsinlat(:,:,ig) = SIN( rad * gphiu(:,:) )
               gcoslat(:,:,ig) = COS( rad * gphiu(:,:) )
               linit(ig) = .TRUE.
            ENDIF
         CASE ( 'V' )   
            ig = 3
            IF( .NOT. linit(ig) ) THEN 
               gsinlon(:,:,ig) = SIN( rad * glamv(:,:) )
               gcoslon(:,:,ig) = COS( rad * glamv(:,:) )
               gsinlat(:,:,ig) = SIN( rad * gphiv(:,:) )
               gcoslat(:,:,ig) = COS( rad * gphiv(:,:) )
               linit(ig) = .TRUE.
            ENDIF
         CASE ( 'F' )   
            ig = 4
            IF( .NOT. linit(ig) ) THEN 
               gsinlon(:,:,ig) = SIN( rad * glamf(:,:) )
               gcoslon(:,:,ig) = COS( rad * glamf(:,:) )
               gsinlat(:,:,ig) = SIN( rad * gphif(:,:) )
               gcoslat(:,:,ig) = COS( rad * gphif(:,:) )
               linit(ig) = .TRUE.
            ENDIF
         CASE default   
            WRITE(ctmp1,*) 'geo2oce : bad grid argument : ', cgrid
            CALL ctl_stop( ctmp1 )
      END SELECT
      
      pte = - gsinlon(:,:,ig) * pxx + gcoslon(:,:,ig) * pyy
      ptn = - gcoslon(:,:,ig) * gsinlat(:,:,ig) * pxx    &
            - gsinlon(:,:,ig) * gsinlat(:,:,ig) * pyy    &
            + gcoslat(:,:,ig) * pzz
!!$   ptv =   gcoslon(:,:,ig) * gcoslat(:,:,ig) * pxx    &
!!$         + gsinlon(:,:,ig) * gcoslat(:,:,ig) * pyy    &
!!$         + gsinlat(:,:,ig) * pzz
      !
   END SUBROUTINE geo2oce

   SUBROUTINE oce2geo ( pte, ptn, cgrid,     &
                        pxx , pyy , pzz )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE oce2geo  ***
      !!      
      !! ** Purpose :
      !!
      !! ** Method  :   Change vector from east/north to geocentric
      !!
      !! History :
      !!        !         (A. Caubel)  oce2geo - Original code
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT( IN    ) ::  pte, ptn
      CHARACTER(len=1)            , INTENT( IN    ) ::  cgrid
      REAL(wp), DIMENSION(jpi,jpj), INTENT(   OUT ) ::  pxx , pyy , pzz
      !!
      REAL(wp), PARAMETER :: rpi = 3.141592653E0
      REAL(wp), PARAMETER :: rad = rpi / 180.e0
      INTEGER ::   ig     !
      INTEGER ::   ierr   ! local integer
      !!----------------------------------------------------------------------

      IF( ALLOCATED( gsinlon ) ) THEN
         ALLOCATE( gsinlon(jpi,jpj,4) , gcoslon(jpi,jpj,4) ,   &
            &      gsinlat(jpi,jpj,4) , gcoslat(jpi,jpj,4) , STAT=ierr )
         IF( lk_mpp    )   CALL mpp_sum( ierr )
         IF( ierr /= 0 )   CALL ctl_stop('STOP', 'angle_msh_geo: unable to allocate arrays' )
      ENDIF

      SELECT CASE( cgrid)
         CASE ( 'T' )   
            ig = 1
            IF( .NOT. linit(ig) ) THEN 
               gsinlon(:,:,ig) = SIN( rad * glamt(:,:) )
               gcoslon(:,:,ig) = COS( rad * glamt(:,:) )
               gsinlat(:,:,ig) = SIN( rad * gphit(:,:) )
               gcoslat(:,:,ig) = COS( rad * gphit(:,:) )
               linit(ig) = .TRUE.
            ENDIF
         CASE ( 'U' )   
            ig = 2
            IF( .NOT. linit(ig) ) THEN 
               gsinlon(:,:,ig) = SIN( rad * glamu(:,:) )
               gcoslon(:,:,ig) = COS( rad * glamu(:,:) )
               gsinlat(:,:,ig) = SIN( rad * gphiu(:,:) )
               gcoslat(:,:,ig) = COS( rad * gphiu(:,:) )
               linit(ig) = .TRUE.
            ENDIF
         CASE ( 'V' )   
            ig = 3
            IF( .NOT. linit(ig) ) THEN 
               gsinlon(:,:,ig) = SIN( rad * glamv(:,:) )
               gcoslon(:,:,ig) = COS( rad * glamv(:,:) )
               gsinlat(:,:,ig) = SIN( rad * gphiv(:,:) )
               gcoslat(:,:,ig) = COS( rad * gphiv(:,:) )
               linit(ig) = .TRUE.
            ENDIF
         CASE ( 'F' )   
            ig = 4
            IF( .NOT. linit(ig) ) THEN 
               gsinlon(:,:,ig) = SIN( rad * glamf(:,:) )
               gcoslon(:,:,ig) = COS( rad * glamf(:,:) )
               gsinlat(:,:,ig) = SIN( rad * gphif(:,:) )
               gcoslat(:,:,ig) = COS( rad * gphif(:,:) )
               linit(ig) = .TRUE.
            ENDIF
         CASE default   
            WRITE(ctmp1,*) 'geo2oce : bad grid argument : ', cgrid
            CALL ctl_stop( ctmp1 )
      END SELECT

       pxx = - gsinlon(:,:,ig) * pte - gcoslon(:,:,ig) * gsinlat(:,:,ig) * ptn 
       pyy =   gcoslon(:,:,ig) * pte - gsinlon(:,:,ig) * gsinlat(:,:,ig) * ptn
       pzz =   gcoslat(:,:,ig) * ptn

      
   END SUBROUTINE oce2geo


   SUBROUTINE repere ( px1, py1, px2, py2, kchoix, cd_type )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE repere  ***
      !!        
      !! ** Purpose :   Change vector componantes between a geopgraphic grid 
      !!      and a stretched coordinates grid.
      !!
      !! ** Method  :   
      !!
      !! ** Action  :
      !!
      !! History :
      !!        !  89-03  (O. Marti)  original code
      !!        !  92-02  (M. Imbard)
      !!        !  93-03  (M. Guyon)  symetrical conditions
      !!        !  98-05  (B. Blanke)
      !!   8.5  !  02-08  (G. Madec)  F90: Free form
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   ), DIMENSION(jpi,jpj) ::   px1, py1   ! two horizontal components to be rotated
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   px2, py2   ! the two horizontal components in the model repere
      INTEGER , INTENT(in   )                     ::   kchoix     ! type of transformation
      !                                                           ! = 1 change from geographic to model grid.
      !                                                           ! =-1 change from model to geographic grid
      CHARACTER(len=1), INTENT(in   ), OPTIONAL   ::   cd_type    ! define the nature of pt2d array grid-points
      !
      CHARACTER(len=1) ::   cl_type      ! define the nature of pt2d array grid-points (T point by default)
      !!----------------------------------------------------------------------

      cl_type = 'T'
      IF( PRESENT(cd_type) )   cl_type = cd_type
         !
      SELECT CASE (kchoix)
      CASE ( 1)      ! change from geographic to model grid.
         CALL rot_rep( px1, py1, cl_type, 'en->i', px2 )
         CALL rot_rep( px1, py1, cl_type, 'en->j', py2 )
      CASE (-1)      ! change from model to geographic grid
         CALL rot_rep( px1, py1, cl_type, 'ij->e', px2 )
         CALL rot_rep( px1, py1, cl_type, 'ij->n', py2 )
      CASE DEFAULT   ;   CALL ctl_stop( 'repere: Syntax Error in the definition of kchoix (1 OR -1' )
      END SELECT
      
   END SUBROUTINE repere


   SUBROUTINE obs_rot ( psinu, pcosu, psinv, pcosv )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE obs_rot  ***
      !!
      !! ** Purpose :   Copy gsinu, gcosu, gsinv and gsinv
      !!                to input data for rotations of
      !!                current at observation points
      !!
      !! History :
      !!   9.2  !  09-02  (K. Mogensen)
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT( OUT )::   psinu, pcosu, psinv, pcosv   ! copy of data
      !!----------------------------------------------------------------------

      ! Initialization of gsin* and gcos* at first call
      ! -----------------------------------------------

      IF( lmust_init ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' obs_rot : geographic <--> stretched'
         IF(lwp) WRITE(numout,*) ' ~~~~~~~   coordinate transformation'

         CALL angle       ! initialization of the transformation
         lmust_init = .FALSE.

      ENDIF

      psinu(:,:) = gsinu(:,:)
      pcosu(:,:) = gcosu(:,:)
      psinv(:,:) = gsinv(:,:)
      pcosv(:,:) = gcosv(:,:)

   END SUBROUTINE obs_rot

  !!======================================================================
END MODULE geo2ocean
