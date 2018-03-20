MODULE diaptr
   !!======================================================================
   !!                       ***  MODULE  diaptr  ***
   !! Ocean physics:  Computes meridonal transports and zonal means
   !!=====================================================================
   !! History :  1.0  ! 2003-09  (C. Talandier, G. Madec)  Original code
   !!            2.0  ! 2006-01  (A. Biastoch)  Allow sub-basins computation
   !!            3.2  ! 2010-03  (O. Marti, S. Flavoni) Add fields
   !!            3.3  ! 2010-10  (G. Madec)  dynamical allocation
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dia_ptr      : Poleward Transport Diagnostics module
   !!   dia_ptr_init : Initialization, namelist read
   !!   dia_ptr_wri  : Output of poleward fluxes
   !!   ptr_vjk      : "zonal" sum computation of a "meridional" flux array
   !!   ptr_tjk      : "zonal" mean computation of a tracer field
   !!   ptr_vj       : "zonal" and vertical sum computation of a "meridional" flux array
   !!                   (Generic interface to ptr_vj_3d, ptr_vj_2d)
   !!----------------------------------------------------------------------
   USE oce              ! ocean dynamics and active tracers
   USE dom_oce          ! ocean space and time domain
   USE phycst           ! physical constants
   USE ldftra_oce       ! ocean active tracers: lateral physics
   USE dianam           !
   USE iom              ! IOM library
   USE ioipsl           ! IO-IPSL library
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library
   USE lbclnk           ! lateral boundary condition - processor exchanges
   USE timing           ! preformance summary
   USE wrk_nemo         ! working arrays

   IMPLICIT NONE
   PRIVATE

   INTERFACE ptr_vj
      MODULE PROCEDURE ptr_vj_3d, ptr_vj_2d
   END INTERFACE

   PUBLIC   dia_ptr_init   ! call in opa module
   PUBLIC   dia_ptr        ! call in step module
   PUBLIC   ptr_vj         ! call by tra_ldf & tra_adv routines
   PUBLIC   ptr_vjk        ! call by tra_ldf & tra_adv routines

   !                                           !!** namelist  namptr  **
   LOGICAL , PUBLIC ::   ln_diaptr  = .FALSE.   !: Poleward transport flag (T) or not (F)
   LOGICAL , PUBLIC ::   ln_subbas  = .FALSE.   !: Atlantic/Pacific/Indian basins calculation
   LOGICAL , PUBLIC ::   ln_diaznl  = .FALSE.   !: Add zonal means and meridional stream functions
   LOGICAL , PUBLIC ::   ln_ptrcomp = .FALSE.   !: Add decomposition : overturning (and gyre, soon ...)
   INTEGER , PUBLIC ::   nn_fptr    = 15        !: frequency of ptr computation  [time step]
   INTEGER , PUBLIC ::   nn_fwri    = 15        !: frequency of ptr outputs      [time step]

   REAL(wp), ALLOCATABLE, SAVE, PUBLIC, DIMENSION(:) ::   htr_adv, htr_ldf, htr_ove   !: Heat TRansports (adv, diff, overturn.)
   REAL(wp), ALLOCATABLE, SAVE, PUBLIC, DIMENSION(:) ::   str_adv, str_ldf, str_ove   !: Salt TRansports (adv, diff, overturn.)
   
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   btmsk                  ! T-point basin interior masks
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   btm30                  ! mask out Southern Ocean (=0 south of 30°S)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   htr  , str             ! adv heat and salt transports (approx)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   tn_jk, sn_jk , v_msf   ! i-mean T and S, j-Stream-Function
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   sjk  , r1_sjk          ! i-mean i-k-surface and its inverse        
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   htr_eiv, str_eiv       ! bolus adv heat ans salt transports ('key_diaeiv')
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   v_msf_eiv              ! bolus j-streamfuction              ('key_diaeiv')


   INTEGER ::   niter       !
   INTEGER ::   nidom_ptr   !
   INTEGER ::   numptr      ! logical unit for Poleward TRansports
   INTEGER ::   nptr        ! = 1 (ln_subbas=F) or = 5 (glo, atl, pac, ind, ipc) (ln_subbas=T) 

   REAL(wp) ::   rc_sv    = 1.e-6_wp   ! conversion from m3/s to Sverdrup
   REAL(wp) ::   rc_pwatt = 1.e-15_wp  ! conversion from W    to PW (further x rau0 x Cp)
   REAL(wp) ::   rc_ggram = 1.e-6_wp   ! conversion from g    to Pg

   REAL(wp), TARGET, DIMENSION(:),   ALLOCATABLE, SAVE :: p_fval1d
   REAL(wp), TARGET, DIMENSION(:,:), ALLOCATABLE, SAVE :: p_fval2d

   !! Integer, 1D workspace arrays. Not common enough to be implemented in 
   !! wrk_nemo module.
   INTEGER, ALLOCATABLE, SAVE, DIMENSION(:) :: ndex  , ndex_atl     , ndex_pac     , ndex_ind     , ndex_ipc
   INTEGER, ALLOCATABLE, SAVE, DIMENSION(:) ::         ndex_atl_30  , ndex_pac_30  , ndex_ind_30  , ndex_ipc_30
   INTEGER, ALLOCATABLE, SAVE, DIMENSION(:) :: ndex_h, ndex_h_atl_30, ndex_h_pac_30, ndex_h_ind_30, ndex_h_ipc_30

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: diaptr.F90 3385 2012-05-08 08:59:30Z charris $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   FUNCTION dia_ptr_alloc()
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_ptr_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER               ::   dia_ptr_alloc   ! return value
      INTEGER, DIMENSION(6) ::   ierr
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !
      ALLOCATE( btmsk(jpi,jpj,nptr) ,           &
         &      htr_adv(jpj) , str_adv(jpj) ,   &
         &      htr_ldf(jpj) , str_ldf(jpj) ,   &
         &      htr_ove(jpj) , str_ove(jpj),    &
         &      htr(jpj,nptr) , str(jpj,nptr) , &
         &      tn_jk(jpj,jpk,nptr) , sn_jk (jpj,jpk,nptr) , v_msf(jpj,jpk,nptr) , &
         &      sjk  (jpj,jpk,nptr) , r1_sjk(jpj,jpk,nptr) , STAT=ierr(1)  )
         !
#if defined key_diaeiv
      ALLOCATE( htr_eiv(jpj,nptr) , str_eiv(jpj,nptr) , &
         &      v_msf_eiv(jpj,jpk,nptr) , STAT=ierr(2) )
#endif
      ALLOCATE( p_fval1d(jpj), p_fval2d(jpj,jpk), Stat=ierr(3))
      !
      ALLOCATE(ndex(jpj*jpk),        ndex_atl(jpj*jpk), ndex_pac(jpj*jpk), &
         &     ndex_ind(jpj*jpk),    ndex_ipc(jpj*jpk),                    &
         &     ndex_atl_30(jpj*jpk), ndex_pac_30(jpj*jpk), Stat=ierr(4))

      ALLOCATE(ndex_ind_30(jpj*jpk), ndex_ipc_30(jpj*jpk),                   &
         &     ndex_h(jpj),          ndex_h_atl_30(jpj), ndex_h_pac_30(jpj), &
         &     ndex_h_ind_30(jpj),   ndex_h_ipc_30(jpj), Stat=ierr(5) )
         !
     ALLOCATE( btm30(jpi,jpj) , STAT=ierr(6)  )
         !
      dia_ptr_alloc = MAXVAL( ierr )
      IF(lk_mpp)   CALL mpp_sum( dia_ptr_alloc )
      !
   END FUNCTION dia_ptr_alloc


   FUNCTION ptr_vj_3d( pva )   RESULT ( p_fval )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_vj_3d  ***
      !!
      !! ** Purpose :   i-k sum computation of a j-flux array
      !!
      !! ** Method  : - i-k sum of pva using the interior 2D vmask (vmask_i).
      !!              pva is supposed to be a masked flux (i.e. * vmask*e1v*e3v)
      !!
      !! ** Action  : - p_fval: i-k-mean poleward flux of pva
      !!----------------------------------------------------------------------
      REAL(wp) , INTENT(in), DIMENSION(jpi,jpj,jpk) ::   pva   ! mask flux array at V-point
      !!
      INTEGER                  ::   ji, jj, jk   ! dummy loop arguments
      INTEGER                  ::   ijpj         ! ???
      REAL(wp), POINTER, DIMENSION(:) :: p_fval  ! function value
      !!--------------------------------------------------------------------
      !
      p_fval => p_fval1d

      ijpj = jpj
      p_fval(:) = 0._wp
      DO jk = 1, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! Vector opt.
               p_fval(jj) = p_fval(jj) + pva(ji,jj,jk) * tmask_i(ji,jj) 
            END DO
         END DO
      END DO
#if defined key_mpp_mpi
      IF(lk_mpp)   CALL mpp_sum( p_fval, ijpj, ncomm_znl)
#endif
      !
   END FUNCTION ptr_vj_3d


   FUNCTION ptr_vj_2d( pva )   RESULT ( p_fval )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_vj_2d  ***
      !!
      !! ** Purpose :   "zonal" and vertical sum computation of a i-flux array
      !!
      !! ** Method  : - i-k sum of pva using the interior 2D vmask (vmask_i).
      !!      pva is supposed to be a masked flux (i.e. * vmask*e1v*e3v)
      !!
      !! ** Action  : - p_fval: i-k-mean poleward flux of pva
      !!----------------------------------------------------------------------
      IMPLICIT none
      REAL(wp) , INTENT(in), DIMENSION(jpi,jpj) ::   pva   ! mask flux array at V-point
      !!
      INTEGER                  ::   ji,jj       ! dummy loop arguments
      INTEGER                  ::   ijpj        ! ???
      REAL(wp), POINTER, DIMENSION(:) :: p_fval ! function value
      !!--------------------------------------------------------------------
      ! 
      p_fval => p_fval1d

      ijpj = jpj
      p_fval(:) = 0._wp
      DO jj = 2, jpjm1
         DO ji = nldi, nlei   ! No vector optimisation here. Better use a mask ?
            p_fval(jj) = p_fval(jj) + pva(ji,jj) * tmask_i(ji,jj)
         END DO
      END DO
#if defined key_mpp_mpi
      CALL mpp_sum( p_fval, ijpj, ncomm_znl )
#endif
      ! 
   END FUNCTION ptr_vj_2d


   FUNCTION ptr_vjk( pva, pmsk )   RESULT ( p_fval )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_vjk  ***
      !!
      !! ** Purpose :   i-sum computation of a j-velocity array
      !!
      !! ** Method  : - i-sum of pva using the interior 2D vmask (vmask_i).
      !!              pva is supposed to be a masked flux (i.e. * vmask)
      !!
      !! ** Action  : - p_fval: i-mean poleward flux of pva
      !!----------------------------------------------------------------------
      !!
      IMPLICIT none
      REAL(wp) , INTENT(in), DIMENSION(jpi,jpj,jpk)           ::   pva    ! mask flux array at V-point
      REAL(wp) , INTENT(in), DIMENSION(jpi,jpj)    , OPTIONAL ::   pmsk   ! Optional 2D basin mask
      !!
      INTEGER                           :: ji, jj, jk ! dummy loop arguments
      REAL(wp), POINTER, DIMENSION(:,:) :: p_fval     ! return function value
#if defined key_mpp_mpi
      INTEGER, DIMENSION(1) ::   ish
      INTEGER, DIMENSION(2) ::   ish2
      INTEGER               ::   ijpjjpk
#endif
#if defined key_mpp_mpi
      REAL(wp), POINTER, DIMENSION(:) ::   zwork    ! mask flux array at V-point
#endif
      !!--------------------------------------------------------------------
      !
#if defined key_mpp_mpi
      ijpjjpk = jpj*jpk
      CALL wrk_alloc( jpj*jpk, zwork )
#endif

      p_fval => p_fval2d

      p_fval(:,:) = 0._wp
      !
      IF( PRESENT( pmsk ) ) THEN 
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
!!gm here, use of tmask_i  ==> no need of loop over nldi, nlei....
               DO ji =  nldi, nlei   ! No vector optimisation here. Better use a mask ?
                  p_fval(jj,jk) = p_fval(jj,jk) + pva(ji,jj,jk) * e1v(ji,jj) * fse3v(ji,jj,jk) * pmsk(ji,jj)
               END DO
            END DO
         END DO
      ELSE 
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji =  nldi, nlei   ! No vector optimisation here. Better use a mask ?
                  p_fval(jj,jk) = p_fval(jj,jk) + pva(ji,jj,jk) * e1v(ji,jj) * fse3v(ji,jj,jk) * tmask_i(ji,jj)
               END DO
            END DO
         END DO
      END IF
      !
#if defined key_mpp_mpi
      ish(1) = ijpjjpk  ;   ish2(1) = jpj   ;   ish2(2) = jpk
      zwork(1:ijpjjpk) = RESHAPE( p_fval, ish )
      CALL mpp_sum( zwork, ijpjjpk, ncomm_znl )
      p_fval(:,:) = RESHAPE( zwork, ish2 )
#endif
      !
#if defined key_mpp_mpi
      CALL wrk_dealloc( jpj*jpk, zwork )
#endif
      !
   END FUNCTION ptr_vjk


   FUNCTION ptr_tjk( pta, pmsk )   RESULT ( p_fval )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_tjk  ***
      !!
      !! ** Purpose :   i-sum computation of e1t*e3t * a tracer field
      !!
      !! ** Method  : - i-sum of mj(pta) using tmask
      !!
      !! ** Action  : - p_fval: i-sum of e1t*e3t*pta
      !!----------------------------------------------------------------------
      !!
      REAL(wp) , INTENT(in), DIMENSION(jpi,jpj,jpk) ::   pta    ! tracer flux array at T-point
      REAL(wp) , INTENT(in), DIMENSION(jpi,jpj)     ::   pmsk   ! Optional 2D basin mask
      !!
      INTEGER                           :: ji, jj, jk   ! dummy loop arguments
      REAL(wp), POINTER, DIMENSION(:,:) :: p_fval       ! return function value
#if defined key_mpp_mpi
      INTEGER, DIMENSION(1) ::   ish
      INTEGER, DIMENSION(2) ::   ish2
      INTEGER               ::   ijpjjpk
#endif
#if defined key_mpp_mpi
      REAL(wp), POINTER, DIMENSION(:) ::   zwork    ! mask flux array at V-point
#endif
      !!-------------------------------------------------------------------- 
      !
#if defined key_mpp_mpi
      ijpjjpk = jpj*jpk
      CALL wrk_alloc( jpj*jpk, zwork )
#endif

      p_fval => p_fval2d

      p_fval(:,:) = 0._wp
      DO jk = 1, jpkm1
         DO jj = 2, jpjm1
            DO ji =  nldi, nlei   ! No vector optimisation here. Better use a mask ?
               p_fval(jj,jk) = p_fval(jj,jk) + pta(ji,jj,jk) * e1t(ji,jj) * fse3t(ji,jj,jk) * pmsk(ji,jj)
            END DO
         END DO
      END DO
#if defined key_mpp_mpi
      ish(1) = jpj*jpk   ;   ish2(1) = jpj   ;   ish2(2) = jpk
      zwork(1:ijpjjpk)= RESHAPE( p_fval, ish )
      CALL mpp_sum( zwork, ijpjjpk, ncomm_znl )
      p_fval(:,:)= RESHAPE( zwork, ish2 )
#endif
      !
#if defined key_mpp_mpi
      CALL wrk_dealloc( jpj*jpk, zwork )
#endif
      !    
   END FUNCTION ptr_tjk


   SUBROUTINE dia_ptr( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dia_ptr  ***
      !!----------------------------------------------------------------------
      USE oce,     vt  =>   ua   ! use ua as workspace
      USE oce,     vs  =>   va   ! use va as workspace
      IMPLICIT none
      !!
      INTEGER, INTENT(in) ::   kt   ! ocean time step index
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) ::   zv               ! local scalar
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )   CALL timing_start('dia_ptr')
      !
      IF( kt == nit000 .OR. MOD( kt, nn_fptr ) == 0 )   THEN
         !
         IF( MOD( kt, nn_fptr ) == 0 ) THEN 
            !
            IF( ln_diaznl ) THEN               ! i-mean temperature and salinity
               DO jn = 1, nptr
                  tn_jk(:,:,jn) = ptr_tjk( tsn(:,:,:,jp_tem), btmsk(:,:,jn) ) * r1_sjk(:,:,jn)
               END DO
            ENDIF
            !
            !                          ! horizontal integral and vertical dz 
            !                                ! eulerian velocity
            v_msf(:,:,1) = ptr_vjk( vn(:,:,:) ) 
            DO jn = 2, nptr
               v_msf(:,:,jn) = ptr_vjk( vn(:,:,:), btmsk(:,:,jn)*btm30(:,:) ) 
            END DO
#if defined key_diaeiv
            DO jn = 1, nptr                  ! bolus velocity
               v_msf_eiv(:,:,jn) = ptr_vjk( v_eiv(:,:,:), btmsk(:,:,jn) )   ! here no btm30 for MSFeiv
            END DO
            !                                ! add bolus stream-function to the eulerian one
            v_msf(:,:,:) = v_msf(:,:,:) + v_msf_eiv(:,:,:)
#endif
            !
            !                          ! Transports
            !                                ! local heat & salt transports at T-points  ( tsn*mj[vn+v_eiv] )
            vt(:,:,jpk) = 0._wp   ;   vs(:,:,jpk) = 0._wp
            DO jk= 1, jpkm1
               DO jj = 2, jpj
                  DO ji = 1, jpi
#if defined key_diaeiv 
                     zv = ( vn(ji,jj,jk) + vn(ji,jj-1,jk) + v_eiv(ji,jj,jk) + v_eiv(ji,jj-1,jk) ) * 0.5_wp
#else
                     zv = ( vn(ji,jj,jk) + vn(ji,jj-1,jk) ) * 0.5_wp
#endif 
                     vt(ji,jj,jk) = zv * tsn(ji,jj,jk,jp_tem)
                     vs(ji,jj,jk) = zv * tsn(ji,jj,jk,jp_sal)
                  END DO
               END DO
            END DO
!!gm useless as overlap areas are not used in ptr_vjk
            CALL lbc_lnk( vs, 'V', -1. )   ;   CALL lbc_lnk( vt, 'V', -1. )
!!gm
            !                                ! heat & salt advective transports (approximation)
            htr(:,1) = SUM( ptr_vjk( vt(:,:,:) ) , 2 ) * rc_pwatt   ! SUM over jk + conversion
            str(:,1) = SUM( ptr_vjk( vs(:,:,:) ) , 2 ) * rc_ggram
            DO jn = 2, nptr 
               htr(:,jn) = SUM( ptr_vjk( vt(:,:,:), btmsk(:,:,jn)*btm30(:,:) ) , 2 ) * rc_pwatt   ! mask Southern Ocean
               str(:,jn) = SUM( ptr_vjk( vs(:,:,:), btmsk(:,:,jn)*btm30(:,:) ) , 2 ) * rc_ggram   ! mask Southern Ocean
            END DO

            IF( ln_ptrcomp ) THEN            ! overturning transport
               htr_ove(:) = SUM( v_msf(:,:,1) * tn_jk(:,:,1), 2 ) * rc_pwatt   ! SUM over jk + conversion
               str_ove(:) = SUM( v_msf(:,:,1) * sn_jk(:,:,1), 2 ) * rc_ggram
            END IF
            !                                ! Advective and diffusive transport
            htr_adv(:) = htr_adv(:) * rc_pwatt        ! these are computed in tra_adv... and tra_ldf... routines 
            htr_ldf(:) = htr_ldf(:) * rc_pwatt        ! here just the conversion in PW and Gg
            str_adv(:) = str_adv(:) * rc_ggram
            str_ldf(:) = str_ldf(:) * rc_ggram

#if defined key_diaeiv
            DO jn = 1, nptr                  ! Bolus component
               htr_eiv(:,jn) = SUM( v_msf_eiv(:,:,jn) * tn_jk(:,:,jn), 2 ) * rc_pwatt   ! SUM over jk
               str_eiv(:,jn) = SUM( v_msf_eiv(:,:,jn) * sn_jk(:,:,jn), 2 ) * rc_ggram   ! SUM over jk
            END DO
#endif
            !                                ! "Meridional" Stream-Function
            DO jn = 1, nptr
               DO jk = 2, jpk 
                  v_msf    (:,jk,jn) = v_msf    (:,jk-1,jn) + v_msf    (:,jk,jn)       ! Eulerian j-Stream-Function
#if defined key_diaeiv
                  v_msf_eiv(:,jk,jn) = v_msf_eiv(:,jk-1,jn) + v_msf_eiv(:,jk,jn)       ! Bolus    j-Stream-Function

#endif
               END DO
            END DO
            v_msf    (:,:,:) = v_msf    (:,:,:) * rc_sv       ! converte in Sverdrups
#if defined key_diaeiv
            v_msf_eiv(:,:,:) = v_msf_eiv(:,:,:) * rc_sv
#endif
         ENDIF
         !
         CALL dia_ptr_wri( kt )                        ! outputs
         !
      ENDIF
      !
#if defined key_mpp_mpi
      IF( kt == nitend .AND. l_znl_root )   CALL histclo( numptr )      ! Close the file
#else
      IF( kt == nitend )                    CALL histclo( numptr )      ! Close the file
#endif
      !
      IF( nn_timing == 1 )   CALL timing_stop('dia_ptr')
      !
   END SUBROUTINE dia_ptr


   SUBROUTINE dia_ptr_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dia_ptr_init  ***
      !!                   
      !! ** Purpose :   Initialization, namelist read
      !!----------------------------------------------------------------------
      INTEGER ::   jn           ! dummy loop indices 
      INTEGER ::   inum, ierr   ! local integers
#if defined key_mpp_mpi
      INTEGER, DIMENSION(1) :: iglo, iloc, iabsf, iabsl, ihals, ihale, idid
#endif
      !!
      NAMELIST/namptr/ ln_diaptr, ln_diaznl, ln_subbas, ln_ptrcomp, nn_fptr, nn_fwri
      !!----------------------------------------------------------------------

      REWIND( numnam )                 ! Read Namelist namptr : poleward transport parameters
      READ  ( numnam, namptr )

      IF(lwp) THEN                     ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'dia_ptr_init : poleward transport and msf initialization'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namptr : set ptr parameters'
         WRITE(numout,*) '      Poleward heat & salt transport (T) or not (F)      ln_diaptr  = ', ln_diaptr
         WRITE(numout,*) '      Overturning heat & salt transport                  ln_ptrcomp = ', ln_ptrcomp
         WRITE(numout,*) '      T & S zonal mean and meridional stream function    ln_diaznl  = ', ln_diaznl 
         WRITE(numout,*) '      Global (F) or glo/Atl/Pac/Ind/Indo-Pac basins      ln_subbas  = ', ln_subbas
         WRITE(numout,*) '      Frequency of computation                           nn_fptr    = ', nn_fptr
         WRITE(numout,*) '      Frequency of outputs                               nn_fwri    = ', nn_fwri
      ENDIF
      
      IF( ln_diaptr) THEN  
     
         IF( nn_timing == 1 )   CALL timing_start('dia_ptr_init')
      
         IF( ln_subbas ) THEN   ;   nptr = 5       ! Global, Atlantic, Pacific, Indian, Indo-Pacific
         ELSE                   ;   nptr = 1       ! Global only
         ENDIF

         !                                      ! allocate dia_ptr arrays
         IF( dia_ptr_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'dia_ptr_init : unable to allocate arrays' )

         rc_pwatt = rc_pwatt * rau0 * rcp          ! conversion from K.s-1 to PetaWatt

         IF( lk_mpp )   CALL mpp_ini_znl( numout )     ! Define MPI communicator for zonal sum

         IF( ln_subbas ) THEN                ! load sub-basin mask
            CALL iom_open( 'subbasins', inum )
            CALL iom_get( inum, jpdom_data, 'atlmsk', btmsk(:,:,2) )   ! Atlantic basin
            CALL iom_get( inum, jpdom_data, 'pacmsk', btmsk(:,:,3) )   ! Pacific  basin
            CALL iom_get( inum, jpdom_data, 'indmsk', btmsk(:,:,4) )   ! Indian   basin
            CALL iom_close( inum )
            btmsk(:,:,5) = MAX ( btmsk(:,:,3), btmsk(:,:,4) )          ! Indo-Pacific basin
            WHERE( gphit(:,:) < -30._wp)   ;   btm30(:,:) = 0._wp      ! mask out Southern Ocean
            ELSE WHERE                     ;   btm30(:,:) = tmask(:,:,1)
            END WHERE
         ENDIF
         btmsk(:,:,1) = tmask_i(:,:)                                   ! global ocean
      
         DO jn = 1, nptr
            btmsk(:,:,jn) = btmsk(:,:,jn) * tmask_i(:,:)               ! interior domain only
         END DO
      
         IF( lk_vvl )   CALL ctl_stop( 'diaptr: error in vvl case as constant i-mean surface is used' )

         !                                   ! i-sum of e1v*e3v surface and its inverse
         DO jn = 1, nptr
            sjk(:,:,jn) = ptr_tjk( tmask(:,:,:), btmsk(:,:,jn) )
            r1_sjk(:,:,jn) = 0._wp
            WHERE( sjk(:,:,jn) /= 0._wp )   r1_sjk(:,:,jn) = 1._wp / sjk(:,:,jn)
         END DO

      ! Initialise arrays to zero because diatpr is called before they are first calculated
      ! Note that this means diagnostics will not be exactly correct when model run is restarted.
      htr_adv(:) = 0._wp ; str_adv(:) =  0._wp ;  htr_ldf(:) = 0._wp ; str_ldf(:) =  0._wp

#if defined key_mpp_mpi 
         iglo (1) = jpjglo                   ! MPP case using MPI  ('key_mpp_mpi')
         iloc (1) = nlcj
         iabsf(1) = njmppt(narea)
         iabsl(:) = iabsf(:) + iloc(:) - 1
         ihals(1) = nldj - 1
         ihale(1) = nlcj - nlej
         idid (1) = 2
         CALL flio_dom_set( jpnj, nproc/jpni, idid, iglo, iloc, iabsf, iabsl, ihals, ihale, 'BOX', nidom_ptr )
#else
         nidom_ptr = FLIO_DOM_NONE
#endif
      IF( nn_timing == 1 )   CALL timing_stop('dia_ptr_init')
      !
      ENDIF 
      ! 
   END SUBROUTINE dia_ptr_init


   SUBROUTINE dia_ptr_wri( kt )
      !!---------------------------------------------------------------------
      !!                ***  ROUTINE dia_ptr_wri  ***
      !!
      !! ** Purpose :   output of poleward fluxes
      !!
      !! ** Method  :   NetCDF file
      !!----------------------------------------------------------------------
      !!
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !!
      INTEGER, SAVE ::   nhoridz, ndepidzt, ndepidzw
      INTEGER, SAVE ::   ndim  , ndim_atl     , ndim_pac     , ndim_ind     , ndim_ipc
      INTEGER, SAVE ::           ndim_atl_30  , ndim_pac_30  , ndim_ind_30  , ndim_ipc_30
      INTEGER, SAVE ::   ndim_h, ndim_h_atl_30, ndim_h_pac_30, ndim_h_ind_30, ndim_h_ipc_30
      !!
      CHARACTER (len=40) ::   clhstnam, clop, clop_once, cl_comment   ! temporary names
      INTEGER            ::   iline, it, itmod, ji, jj, jk            !
#if defined key_iomput
      INTEGER            ::   inum                                    ! temporary logical unit
#endif
      REAL(wp)           ::   zsto, zout, zdt, zjulian                ! temporary scalars
      !!
      REAL(wp), POINTER, DIMENSION(:)   ::   zphi, zfoo    ! 1D workspace
      REAL(wp), POINTER, DIMENSION(:,:) ::   z_1           ! 2D workspace
      !!-------------------------------------------------------------------- 
      !
      CALL wrk_alloc( jpi      , zphi , zfoo )
      CALL wrk_alloc( jpi , jpk, z_1 )

      ! define time axis
      it    = kt / nn_fptr
      itmod = kt - nit000 + 1
      
      ! Initialization
      ! --------------
      IF( kt == nit000 ) THEN
         niter = ( nit000 - 1 ) / nn_fptr
         zdt = rdt
         IF( nacc == 1 )   zdt = rdtmin
         !
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'dia_ptr_wri : poleward transport and msf writing: initialization , niter = ', niter
            WRITE(numout,*) '~~~~~~~~~~~~'
         ENDIF

         ! Reference latitude (used in plots)
         ! ------------------
         !                                           ! =======================
         IF( cp_cfg == "orca" ) THEN                 !   ORCA configurations
            !                                        ! =======================
            IF( jp_cfg == 05  )   iline = 192   ! i-line that passes near the North Pole
            IF( jp_cfg == 025 )   iline = 384   ! i-line that passes near the North Pole
            IF( jp_cfg == 1   )   iline =  96   ! i-line that passes near the North Pole
            IF( jp_cfg == 2   )   iline =  48   ! i-line that passes near the North Pole
            IF( jp_cfg == 4   )   iline =  24   ! i-line that passes near the North Pole
            zphi(1:jpj) = 0._wp
            DO ji = mi0(iline), mi1(iline) 
               zphi(1:jpj) = gphiv(ji,:)         ! if iline is in the local domain
               ! Correct highest latitude for some configurations - will work if domain is parallelized in J ?
               IF( jp_cfg == 05 ) THEN
                  DO jj = mj0(jpjdta), mj1(jpjdta) 
                     zphi( jj ) = zphi(mj0(jpjdta-1)) + ( zphi(mj0(jpjdta-1))-zphi(mj0(jpjdta-2)) ) * 0.5_wp
                     zphi( jj ) = MIN( zphi(jj), 90._wp )
                  END DO
               END IF
               IF( jp_cfg == 1 .OR. jp_cfg == 2 .OR. jp_cfg == 4 ) THEN
                  DO jj = mj0(jpjdta-1), mj1(jpjdta-1) 
                     zphi( jj ) = 88.5_wp
                  END DO
                  DO jj = mj0(jpjdta  ), mj1(jpjdta  ) 
                     zphi( jj ) = 89.5_wp
                  END DO
               END IF
            END DO
            ! provide the correct zphi to all local domains
#if defined key_mpp_mpi
            CALL mpp_sum( zphi, jpj, ncomm_znl )        
#endif
            !                                        ! =======================
         ELSE                                        !   OTHER configurations 
            !                                        ! =======================
            zphi(1:jpj) = gphiv(1,:)             ! assume lat/lon coordinate, select the first i-line
            !
         ENDIF
         !
         ! Work only on westmost processor (will not work if mppini2 is used)
#if defined key_mpp_mpi
         IF( l_znl_root ) THEN 
#endif
            !
            ! OPEN netcdf file 
            ! ----------------
            ! Define frequency of output and means
            zsto = nn_fptr * zdt
            IF( ln_mskland )   THEN    ! put 1.e+20 on land (very expensive!!)
               clop      = "ave(only(x))"
               clop_once = "once(only(x))"
            ELSE                       ! no use of the mask value (require less cpu time)
               clop      = "ave(x)"       
               clop_once = "once"
            ENDIF

            zout = nn_fwri * zdt
            zfoo(1:jpj) = 0._wp

            CALL ymds2ju( nyear, nmonth, nday, rdt, zjulian )  ! Compute julian date from starting date of the run
            zjulian = zjulian - adatrj                         ! set calendar origin to the beginning of the experiment

#if defined key_iomput
            ! Requested by IPSL people, use by their postpro...
            IF(lwp) THEN
               CALL dia_nam( clhstnam, nn_fwri,' ' )
               CALL ctl_opn( inum, 'date.file', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp, narea )
               WRITE(inum,*) clhstnam
               CLOSE(inum)
            ENDIF
#endif

            CALL dia_nam( clhstnam, nn_fwri, 'diaptr' )
            IF(lwp)WRITE( numout,*)" Name of diaptr NETCDF file : ", clhstnam

            ! Horizontal grid : zphi()
            CALL histbeg(clhstnam, 1, zfoo, jpj, zphi,   &
               1, 1, 1, jpj, niter, zjulian, zdt*nn_fptr, nhoridz, numptr, domain_id=nidom_ptr)
            ! Vertical grids : gdept_0, gdepw_0
            CALL histvert( numptr, "deptht", "Vertical T levels",   &
               &                   "m", jpk, gdept_0, ndepidzt, "down" )
            CALL histvert( numptr, "depthw", "Vertical W levels",   &
               &                   "m", jpk, gdepw_0, ndepidzw, "down" )
            !
            CALL wheneq ( jpj*jpk, MIN(sjk(:,:,1), 1._wp), 1, 1., ndex  , ndim  )      ! Lat-Depth
            CALL wheneq ( jpj    , MIN(sjk(:,1,1), 1._wp), 1, 1., ndex_h, ndim_h )     ! Lat

            IF( ln_subbas ) THEN
               z_1(:,1) = 1._wp
               WHERE ( gphit(jpi/2,:) < -30._wp )   z_1(:,1) = 0._wp
               DO jk = 2, jpk
                  z_1(:,jk) = z_1(:,1)
               END DO
               !                       ! Atlantic (jn=2)
               CALL wheneq ( jpj*jpk, MIN(sjk(:,:,2)         , 1._wp), 1, 1., ndex_atl     , ndim_atl      ) ! Lat-Depth
               CALL wheneq ( jpj*jpk, MIN(sjk(:,:,2)*z_1(:,:), 1._wp), 1, 1., ndex_atl_30  , ndim_atl_30   ) ! Lat-Depth
               CALL wheneq ( jpj    , MIN(sjk(:,1,2)*z_1(:,1), 1._wp), 1, 1., ndex_h_atl_30, ndim_h_atl_30 ) ! Lat
               !                       ! Pacific (jn=3)
               CALL wheneq ( jpj*jpk, MIN(sjk(:,:,3)         , 1._wp), 1, 1., ndex_pac     , ndim_pac      ) ! Lat-Depth
               CALL wheneq ( jpj*jpk, MIN(sjk(:,:,3)*z_1(:,:), 1._wp), 1, 1., ndex_pac_30  , ndim_pac_30   ) ! Lat-Depth
               CALL wheneq ( jpj    , MIN(sjk(:,1,3)*z_1(:,1), 1._wp), 1, 1., ndex_h_pac_30, ndim_h_pac_30 ) ! Lat
               !                       ! Indian (jn=4)
               CALL wheneq ( jpj*jpk, MIN(sjk(:,:,4)         , 1._wp), 1, 1., ndex_ind     , ndim_ind      ) ! Lat-Depth
               CALL wheneq ( jpj*jpk, MIN(sjk(:,:,4)*z_1(:,:), 1._wp), 1, 1., ndex_ind_30  , ndim_ind_30   ) ! Lat-Depth
               CALL wheneq ( jpj    , MIN(sjk(:,1,4)*z_1(:,1), 1._wp), 1, 1., ndex_h_ind_30, ndim_h_ind_30 ) ! Lat
               !                       ! Indo-Pacific (jn=5)
               CALL wheneq ( jpj*jpk, MIN(sjk(:,:,5)         , 1._wp), 1, 1., ndex_ipc     , ndim_ipc      ) ! Lat-Depth
               CALL wheneq ( jpj*jpk, MIN(sjk(:,:,5)*z_1(:,:), 1._wp), 1, 1., ndex_ipc_30  , ndim_ipc_30   ) ! Lat-Depth
               CALL wheneq ( jpj    , MIN(sjk(:,1,5)*z_1(:,1), 1._wp), 1, 1., ndex_h_ipc_30, ndim_h_ipc_30 ) ! Lat
            ENDIF
            ! 
#if defined key_diaeiv
            cl_comment = ' (Bolus part included)'
#else
            cl_comment = '                      '
#endif
            IF( ln_diaznl ) THEN             !  Zonal mean T and S
               CALL histdef( numptr, "zotemglo", "Zonal Mean Temperature","C" ,   &
                  1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop, zsto, zout )
               CALL histdef( numptr, "zosalglo", "Zonal Mean Salinity","PSU"  ,   &
                  1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop, zsto, zout )

               CALL histdef( numptr, "zosrfglo", "Zonal Mean Surface","m^2"   ,   &
                  1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop_once, zsto, zout )
               !
               IF (ln_subbas) THEN 
                  CALL histdef( numptr, "zotematl", "Zonal Mean Temperature: Atlantic","C" ,   &
                     1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop, zsto, zout )
                  CALL histdef( numptr, "zosalatl", "Zonal Mean Salinity: Atlantic","PSU"  ,   &
                     1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop, zsto, zout )
                  CALL histdef( numptr, "zosrfatl", "Zonal Mean Surface: Atlantic","m^2"   ,   &
                     1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop_once, zsto, zout )

                  CALL histdef( numptr, "zotempac", "Zonal Mean Temperature: Pacific","C"  ,   &
                     1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop, zsto, zout )
                  CALL histdef( numptr, "zosalpac", "Zonal Mean Salinity: Pacific","PSU"   ,   &
                     1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop, zsto, zout )
                  CALL histdef( numptr, "zosrfpac", "Zonal Mean Surface: Pacific","m^2"    ,   &
                     1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop_once, zsto, zout )

                  CALL histdef( numptr, "zotemind", "Zonal Mean Temperature: Indian","C"   ,   &
                     1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop, zsto, zout )
                  CALL histdef( numptr, "zosalind", "Zonal Mean Salinity: Indian","PSU"    ,   &
                     1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop, zsto, zout )
                  CALL histdef( numptr, "zosrfind", "Zonal Mean Surface: Indian","m^2"     ,   &
                     1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop_once, zsto, zout )

                  CALL histdef( numptr, "zotemipc", "Zonal Mean Temperature: Pacific+Indian","C" ,   &
                     1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop, zsto, zout )
                  CALL histdef( numptr, "zosalipc", "Zonal Mean Salinity: Pacific+Indian","PSU"  ,   &
                     1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop, zsto, zout )
                  CALL histdef( numptr, "zosrfipc", "Zonal Mean Surface: Pacific+Indian","m^2"   ,   &
                     1, jpj, nhoridz, jpk, 1, jpk, ndepidzt, 32, clop_once, zsto, zout )
               ENDIF
            ENDIF
            !
            !  Meridional Stream-Function (Eulerian and Bolus)
            CALL histdef( numptr, "zomsfglo", "Meridional Stream-Function: Global"//TRIM(cl_comment),"Sv" ,   &
               1, jpj, nhoridz, jpk, 1, jpk, ndepidzw, 32, clop, zsto, zout )
            IF( ln_subbas .AND. ln_diaznl ) THEN
               CALL histdef( numptr, "zomsfatl", "Meridional Stream-Function: Atlantic"//TRIM(cl_comment),"Sv" ,   &
                  1, jpj, nhoridz, jpk, 1, jpk, ndepidzw, 32, clop, zsto, zout )
               CALL histdef( numptr, "zomsfpac", "Meridional Stream-Function: Pacific"//TRIM(cl_comment),"Sv"  ,   &
                  1, jpj, nhoridz, jpk, 1, jpk, ndepidzw, 32, clop, zsto, zout )
               CALL histdef( numptr, "zomsfind", "Meridional Stream-Function: Indian"//TRIM(cl_comment),"Sv"   ,   &
                  1, jpj, nhoridz, jpk, 1, jpk, ndepidzw, 32, clop, zsto, zout )
               CALL histdef( numptr, "zomsfipc", "Meridional Stream-Function: Indo-Pacific"//TRIM(cl_comment),"Sv" ,&
                  1, jpj, nhoridz, jpk, 1, jpk, ndepidzw, 32, clop, zsto, zout )
            ENDIF
            !
            !  Heat transport 
            CALL histdef( numptr, "sophtadv", "Advective Heat Transport"      ,   &
               "PW", 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
            CALL histdef( numptr, "sophtldf", "Diffusive Heat Transport"      ,   &
               "PW",1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
            IF ( ln_ptrcomp ) THEN 
               CALL histdef( numptr, "sophtove", "Overturning Heat Transport"    ,   &
                  "PW",1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
            END IF
            IF( ln_subbas ) THEN
               CALL histdef( numptr, "sohtatl", "Heat Transport Atlantic"//TRIM(cl_comment),  &
                  "PW", 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
               CALL histdef( numptr, "sohtpac", "Heat Transport Pacific"//TRIM(cl_comment) ,  &
                  "PW", 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
               CALL histdef( numptr, "sohtind", "Heat Transport Indian"//TRIM(cl_comment)  ,  &
                  "PW", 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
               CALL histdef( numptr, "sohtipc", "Heat Transport Pacific+Indian"//TRIM(cl_comment), &
                  "PW", 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
            ENDIF
            !
            !  Salt transport 
            CALL histdef( numptr, "sopstadv", "Advective Salt Transport"      ,   &
               "Giga g/s", 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
            CALL histdef( numptr, "sopstldf", "Diffusive Salt Transport"      ,   &
               "Giga g/s", 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
            IF ( ln_ptrcomp ) THEN 
               CALL histdef( numptr, "sopstove", "Overturning Salt Transport"    ,   &
                  "Giga g/s", 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
            END IF
#if defined key_diaeiv
            ! Eddy induced velocity
            CALL histdef( numptr, "zomsfeiv", "Bolus Meridional Stream-Function: global",   &
               "Sv"      , 1, jpj, nhoridz, jpk, 1, jpk, ndepidzw, 32, clop, zsto, zout )
            CALL histdef( numptr, "sophteiv", "Bolus Advective Heat Transport",   &
               "PW"      , 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
            CALL histdef( numptr, "sopsteiv", "Bolus Advective Salt Transport",   &
               "Giga g/s", 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
#endif
            IF( ln_subbas ) THEN
               CALL histdef( numptr, "sostatl", "Salt Transport Atlantic"//TRIM(cl_comment)      ,  &
                  "Giga g/s", 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
               CALL histdef( numptr, "sostpac", "Salt Transport Pacific"//TRIM(cl_comment)      ,   &
                  "Giga g/s", 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
               CALL histdef( numptr, "sostind", "Salt Transport Indian"//TRIM(cl_comment)      ,    &
                  "Giga g/s", 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
               CALL histdef( numptr, "sostipc", "Salt Transport Pacific+Indian"//TRIM(cl_comment),  &
                  "Giga g/s", 1, jpj, nhoridz, 1, 1, 1, -99, 32, clop, zsto, zout )
            ENDIF
            !
            CALL histend( numptr )
            !
         END IF
#if defined key_mpp_mpi
      END IF
#endif

#if defined key_mpp_mpi
      IF( MOD( itmod, nn_fptr ) == 0 .AND. l_znl_root ) THEN
#else
      IF( MOD( itmod, nn_fptr ) == 0  ) THEN
#endif
         niter = niter + 1

         IF( ln_diaznl ) THEN 
            CALL histwrite( numptr, "zosrfglo", niter, sjk  (:,:,1) , ndim, ndex )
            CALL histwrite( numptr, "zotemglo", niter, tn_jk(:,:,1)  , ndim, ndex )
            CALL histwrite( numptr, "zosalglo", niter, sn_jk(:,:,1)  , ndim, ndex )

            IF (ln_subbas) THEN 
               CALL histwrite( numptr, "zosrfatl", niter, sjk(:,:,2), ndim_atl, ndex_atl )
               CALL histwrite( numptr, "zosrfpac", niter, sjk(:,:,3), ndim_pac, ndex_pac )
               CALL histwrite( numptr, "zosrfind", niter, sjk(:,:,4), ndim_ind, ndex_ind )
               CALL histwrite( numptr, "zosrfipc", niter, sjk(:,:,5), ndim_ipc, ndex_ipc )

               CALL histwrite( numptr, "zotematl", niter, tn_jk(:,:,2)  , ndim_atl, ndex_atl )
               CALL histwrite( numptr, "zosalatl", niter, sn_jk(:,:,2)  , ndim_atl, ndex_atl )
               CALL histwrite( numptr, "zotempac", niter, tn_jk(:,:,3)  , ndim_pac, ndex_pac )
               CALL histwrite( numptr, "zosalpac", niter, sn_jk(:,:,3)  , ndim_pac, ndex_pac )
               CALL histwrite( numptr, "zotemind", niter, tn_jk(:,:,4)  , ndim_ind, ndex_ind )
               CALL histwrite( numptr, "zosalind", niter, sn_jk(:,:,4)  , ndim_ind, ndex_ind )
               CALL histwrite( numptr, "zotemipc", niter, tn_jk(:,:,5)  , ndim_ipc, ndex_ipc )
               CALL histwrite( numptr, "zosalipc", niter, sn_jk(:,:,5)  , ndim_ipc, ndex_ipc )
            END IF
         ENDIF

         ! overturning outputs:
         CALL histwrite( numptr, "zomsfglo", niter, v_msf(:,:,1), ndim, ndex )
         IF( ln_subbas .AND. ln_diaznl ) THEN
            CALL histwrite( numptr, "zomsfatl", niter, v_msf(:,:,2) , ndim_atl_30, ndex_atl_30 )
            CALL histwrite( numptr, "zomsfpac", niter, v_msf(:,:,3) , ndim_pac_30, ndex_pac_30 )
            CALL histwrite( numptr, "zomsfind", niter, v_msf(:,:,4) , ndim_ind_30, ndex_ind_30 )
            CALL histwrite( numptr, "zomsfipc", niter, v_msf(:,:,5) , ndim_ipc_30, ndex_ipc_30 )
         ENDIF
#if defined key_diaeiv
         CALL histwrite( numptr, "zomsfeiv", niter, v_msf_eiv(:,:,1), ndim  , ndex   )
#endif

         ! heat transport outputs:
         IF( ln_subbas ) THEN
            CALL histwrite( numptr, "sohtatl", niter, htr(:,2)  , ndim_h_atl_30, ndex_h_atl_30 )
            CALL histwrite( numptr, "sohtpac", niter, htr(:,3)  , ndim_h_pac_30, ndex_h_pac_30 )
            CALL histwrite( numptr, "sohtind", niter, htr(:,4)  , ndim_h_ind_30, ndex_h_ind_30 )
            CALL histwrite( numptr, "sohtipc", niter, htr(:,5)  , ndim_h_ipc_30, ndex_h_ipc_30 )
            CALL histwrite( numptr, "sostatl", niter, str(:,2)  , ndim_h_atl_30, ndex_h_atl_30 )
            CALL histwrite( numptr, "sostpac", niter, str(:,3)  , ndim_h_pac_30, ndex_h_pac_30 )
            CALL histwrite( numptr, "sostind", niter, str(:,4)  , ndim_h_ind_30, ndex_h_ind_30 )
            CALL histwrite( numptr, "sostipc", niter, str(:,5)  , ndim_h_ipc_30, ndex_h_ipc_30 )
         ENDIF

         CALL histwrite( numptr, "sophtadv", niter, htr_adv     , ndim_h, ndex_h )
         CALL histwrite( numptr, "sophtldf", niter, htr_ldf     , ndim_h, ndex_h )
         CALL histwrite( numptr, "sopstadv", niter, str_adv     , ndim_h, ndex_h )
         CALL histwrite( numptr, "sopstldf", niter, str_ldf     , ndim_h, ndex_h )
         IF( ln_ptrcomp ) THEN 
            CALL histwrite( numptr, "sopstove", niter, str_ove(:) , ndim_h, ndex_h )
            CALL histwrite( numptr, "sophtove", niter, htr_ove(:) , ndim_h, ndex_h )
         ENDIF
#if defined key_diaeiv
         CALL histwrite( numptr, "sophteiv", niter, htr_eiv(:,1)  , ndim_h, ndex_h )
         CALL histwrite( numptr, "sopsteiv", niter, str_eiv(:,1)  , ndim_h, ndex_h )
#endif
         !
      ENDIF
      !
      CALL wrk_dealloc( jpi      , zphi , zfoo )
      CALL wrk_dealloc( jpi , jpk, z_1 )
      !
  END SUBROUTINE dia_ptr_wri

   !!======================================================================
END MODULE diaptr
