MODULE dynhpg
   !!======================================================================
   !!                       ***  MODULE  dynhpg  ***
   !! Ocean dynamics:  hydrostatic pressure gradient trend
   !!======================================================================
   !! History :  OPA  !  1987-09  (P. Andrich, M.-A. Foujols)  hpg_zco: Original code
   !!            5.0  !  1991-11  (G. Madec)
   !!            7.0  !  1996-01  (G. Madec)  hpg_sco: Original code for s-coordinates
   !!            8.0  !  1997-05  (G. Madec)  split dynber into dynkeg and dynhpg
   !!            8.5  !  2002-07  (G. Madec)  F90: Free form and module
   !!            8.5  !  2002-08  (A. Bozec)  hpg_zps: Original code
   !!   NEMO     1.0  !  2005-10  (A. Beckmann, B.W. An)  various s-coordinate options
   !!                 !         Original code for hpg_ctl, hpg_hel hpg_wdj, hpg_djc, hpg_rot 
   !!             -   !  2005-11  (G. Madec) style & small optimisation
   !!            3.3  !  2010-10  (C. Ethe, G. Madec) reorganisation of initialisation phase
   !!            3.4  !  2011-11  (H. Liu) hpg_prj: Original code for s-coordinates
   !!                 !           (A. Coward) suppression of hel, wdj and rot options
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dyn_hpg      : update the momentum trend with the now horizontal
   !!                  gradient of the hydrostatic pressure
   !!   dyn_hpg_init : initialisation and control of options
   !!       hpg_zco  : z-coordinate scheme
   !!       hpg_zps  : z-coordinate plus partial steps (interpolation)
   !!       hpg_sco  : s-coordinate (standard jacobian formulation)
   !!       hpg_djc  : s-coordinate (Density Jacobian with Cubic polynomial)
   !!       hpg_prj  : s-coordinate (Pressure Jacobian with Cubic polynomial)
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE trdmod          ! ocean dynamics trends 
   USE trdmod_oce      ! ocean variables trends
   USE in_out_manager  ! I/O manager
   USE prtctl          ! Print control
   USE lbclnk          ! lateral boundary condition 
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! Memory Allocation
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_hpg        ! routine called by step module
   PUBLIC   dyn_hpg_init   ! routine called by opa module

   !                                              !!* Namelist namdyn_hpg : hydrostatic pressure gradient 
   LOGICAL , PUBLIC ::   ln_hpg_zco    = .TRUE.    !: z-coordinate - full steps
   LOGICAL , PUBLIC ::   ln_hpg_zps    = .FALSE.   !: z-coordinate - partial steps (interpolation)
   LOGICAL , PUBLIC ::   ln_hpg_sco    = .FALSE.   !: s-coordinate (standard jacobian formulation)
   LOGICAL , PUBLIC ::   ln_hpg_djc    = .FALSE.   !: s-coordinate (Density Jacobian with Cubic polynomial)
   LOGICAL , PUBLIC ::   ln_hpg_prj    = .FALSE.   !: s-coordinate (Pressure Jacobian scheme)
   LOGICAL , PUBLIC ::   ln_dynhpg_imp = .FALSE.   !: semi-implicite hpg flag

   INTEGER  ::   nhpg  =  0   ! = 0 to 7, type of pressure gradient scheme used ! (deduced from ln_hpg_... flags)

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: dynhpg.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_hpg( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_hpg  ***
      !!
      !! ** Method  :   Call the hydrostatic pressure gradient routine 
      !!              using the scheme defined in the namelist
      !!   
      !! ** Action : - Update (ua,va) with the now hydrastatic pressure trend
      !!             - Save the trend (l_trddyn=T)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  ztrdu, ztrdv
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dyn_hpg')
      !
      IF( l_trddyn ) THEN                    ! Temporary saving of ua and va trends (l_trddyn)
         CALL wrk_alloc( jpi,jpj,jpk, ztrdu, ztrdv )
         ztrdu(:,:,:) = ua(:,:,:)  
         ztrdv(:,:,:) = va(:,:,:) 
      ENDIF      
      !
      SELECT CASE ( nhpg )      ! Hydrostatic pressure gradient computation
      CASE (  0 )   ;   CALL hpg_zco    ( kt )      ! z-coordinate
      CASE (  1 )   ;   CALL hpg_zps    ( kt )      ! z-coordinate plus partial steps (interpolation)
      CASE (  2 )   ;   CALL hpg_sco    ( kt )      ! s-coordinate (standard jacobian formulation)
      CASE (  3 )   ;   CALL hpg_djc    ( kt )      ! s-coordinate (Density Jacobian with Cubic polynomial)
      CASE (  4 )   ;   CALL hpg_prj    ( kt )      ! s-coordinate (Pressure Jacobian scheme)
      END SELECT
      !
      IF( l_trddyn ) THEN      ! save the hydrostatic pressure gradient trends for momentum trend diagnostics
         ztrdu(:,:,:) = ua(:,:,:) - ztrdu(:,:,:)
         ztrdv(:,:,:) = va(:,:,:) - ztrdv(:,:,:)
         CALL trd_mod( ztrdu, ztrdv, jpdyn_trd_hpg, 'DYN', kt )
         CALL wrk_dealloc( jpi,jpj,jpk, ztrdu, ztrdv )
      ENDIF          
      !
      IF(ln_ctl)   CALL prt_ctl( tab3d_1=ua, clinfo1=' hpg  - Ua: ', mask1=umask,   &
         &                       tab3d_2=va, clinfo2=       ' Va: ', mask2=vmask, clinfo3='dyn' )
      !
      IF( nn_timing == 1 )  CALL timing_stop('dyn_hpg')
      !
   END SUBROUTINE dyn_hpg


   SUBROUTINE dyn_hpg_init
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE dyn_hpg_init  ***
      !!
      !! ** Purpose :   initializations for the hydrostatic pressure gradient
      !!              computation and consistency control
      !!
      !! ** Action  :   Read the namelist namdyn_hpg and check the consistency
      !!      with the type of vertical coordinate used (zco, zps, sco)
      !!----------------------------------------------------------------------
      INTEGER ::   ioptio = 0      ! temporary integer
      !!
      NAMELIST/namdyn_hpg/ ln_hpg_zco, ln_hpg_zps, ln_hpg_sco,     &
         &                 ln_hpg_djc, ln_hpg_prj, ln_dynhpg_imp
      !!----------------------------------------------------------------------
      !
      REWIND( numnam )               ! Read Namelist namdyn_hpg
      READ  ( numnam, namdyn_hpg )
      !
      IF(lwp) THEN                   ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'dyn_hpg_init : hydrostatic pressure gradient initialisation'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namdyn_hpg : choice of hpg scheme'
         WRITE(numout,*) '      z-coord. - full steps                             ln_hpg_zco    = ', ln_hpg_zco
         WRITE(numout,*) '      z-coord. - partial steps (interpolation)          ln_hpg_zps    = ', ln_hpg_zps
         WRITE(numout,*) '      s-coord. (standard jacobian formulation)          ln_hpg_sco    = ', ln_hpg_sco
         WRITE(numout,*) '      s-coord. (Density Jacobian: Cubic polynomial)     ln_hpg_djc    = ', ln_hpg_djc
         WRITE(numout,*) '      s-coord. (Pressure Jacobian: Cubic polynomial)    ln_hpg_prj    = ', ln_hpg_prj
         WRITE(numout,*) '      time stepping: centered (F) or semi-implicit (T)  ln_dynhpg_imp = ', ln_dynhpg_imp
      ENDIF
      !
      IF( ln_hpg_djc )   &
         &   CALL ctl_stop('dyn_hpg_init : Density Jacobian: Cubic polynominal method &
                           & currently disabled (bugs under investigation). Please select &
                           & either  ln_hpg_sco or  ln_hpg_prj instead')
      !
      IF( lk_vvl .AND. .NOT. (ln_hpg_sco.OR.ln_hpg_prj) )   &
         &   CALL ctl_stop('dyn_hpg_init : variable volume key_vvl requires:&
                           & the standard jacobian formulation hpg_sco or &
                           & the pressure jacobian formulation hpg_prj')
      !
      !                               ! Set nhpg from ln_hpg_... flags
      IF( ln_hpg_zco )   nhpg = 0
      IF( ln_hpg_zps )   nhpg = 1
      IF( ln_hpg_sco )   nhpg = 2
      IF( ln_hpg_djc )   nhpg = 3
      IF( ln_hpg_prj )   nhpg = 4
      !
      !                               ! Consistency check
      ioptio = 0 
      IF( ln_hpg_zco )   ioptio = ioptio + 1
      IF( ln_hpg_zps )   ioptio = ioptio + 1
      IF( ln_hpg_sco )   ioptio = ioptio + 1
      IF( ln_hpg_djc )   ioptio = ioptio + 1
      IF( ln_hpg_prj )   ioptio = ioptio + 1
      IF( ioptio /= 1 )   CALL ctl_stop( 'NO or several hydrostatic pressure gradient options used' )
      !
   END SUBROUTINE dyn_hpg_init


   SUBROUTINE hpg_zco( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE hpg_zco  ***
      !!
      !! ** Method  :   z-coordinate case, levels are horizontal surfaces.
      !!      The now hydrostatic pressure gradient at a given level, jk,
      !!      is computed by taking the vertical integral of the in-situ
      !!      density gradient along the model level from the suface to that
      !!      level:    zhpi = grav .....
      !!                zhpj = grav .....
      !!      add it to the general momentum trend (ua,va).
      !!            ua = ua - 1/e1u * zhpi
      !!            va = va - 1/e2v * zhpj
      !! 
      !! ** Action : - Update (ua,va) with the now hydrastatic pressure trend
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt    ! ocean time-step index
      !!
      INTEGER  ::   ji, jj, jk       ! dummy loop indices
      REAL(wp) ::   zcoef0, zcoef1   ! temporary scalars
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  zhpi, zhpj 
      !!----------------------------------------------------------------------
      !  
      CALL wrk_alloc( jpi,jpj,jpk, zhpi, zhpj )
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn:hpg_zco : hydrostatic pressure gradient trend'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~   z-coordinate case '
      ENDIF
      
      zcoef0 = - grav * 0.5_wp      ! Local constant initialization 

      ! Surface value
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            zcoef1 = zcoef0 * fse3w(ji,jj,1)
            ! hydrostatic pressure gradient
            zhpi(ji,jj,1) = zcoef1 * ( rhd(ji+1,jj,1) - rhd(ji,jj,1) ) / e1u(ji,jj)
            zhpj(ji,jj,1) = zcoef1 * ( rhd(ji,jj+1,1) - rhd(ji,jj,1) ) / e2v(ji,jj)
            ! add to the general momentum trend
            ua(ji,jj,1) = ua(ji,jj,1) + zhpi(ji,jj,1)
            va(ji,jj,1) = va(ji,jj,1) + zhpj(ji,jj,1)
         END DO
      END DO

      !
      ! interior value (2=<jk=<jpkm1)
      DO jk = 2, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zcoef1 = zcoef0 * fse3w(ji,jj,jk)
               ! hydrostatic pressure gradient
               zhpi(ji,jj,jk) = zhpi(ji,jj,jk-1)   &
                  &           + zcoef1 * (  ( rhd(ji+1,jj,jk)+rhd(ji+1,jj,jk-1) )   &
                  &                       - ( rhd(ji  ,jj,jk)+rhd(ji  ,jj,jk-1) )  ) / e1u(ji,jj)

               zhpj(ji,jj,jk) = zhpj(ji,jj,jk-1)   &
                  &           + zcoef1 * (  ( rhd(ji,jj+1,jk)+rhd(ji,jj+1,jk-1) )   &
                  &                       - ( rhd(ji,jj,  jk)+rhd(ji,jj  ,jk-1) )  ) / e2v(ji,jj)
               ! add to the general momentum trend
               ua(ji,jj,jk) = ua(ji,jj,jk) + zhpi(ji,jj,jk)
               va(ji,jj,jk) = va(ji,jj,jk) + zhpj(ji,jj,jk)
            END DO
         END DO
      END DO
      !
      CALL wrk_dealloc( jpi,jpj,jpk, zhpi, zhpj )
      !
   END SUBROUTINE hpg_zco


   SUBROUTINE hpg_zps( kt )
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE hpg_zps  ***
      !!                    
      !! ** Method  :   z-coordinate plus partial steps case.  blahblah...
      !! 
      !! ** Action  : - Update (ua,va) with the now hydrastatic pressure trend
      !!---------------------------------------------------------------------- 
      INTEGER, INTENT(in) ::   kt    ! ocean time-step index
      !!
      INTEGER  ::   ji, jj, jk                       ! dummy loop indices
      INTEGER  ::   iku, ikv                         ! temporary integers
      REAL(wp) ::   zcoef0, zcoef1, zcoef2, zcoef3   ! temporary scalars
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  zhpi, zhpj 
      !!----------------------------------------------------------------------
      !
      CALL wrk_alloc( jpi,jpj,jpk, zhpi, zhpj )
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn:hpg_zps : hydrostatic pressure gradient trend'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~   z-coordinate with partial steps - vector optimization'
      ENDIF


      ! Local constant initialization
      zcoef0 = - grav * 0.5_wp

      !  Surface value (also valid in partial step case)
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            zcoef1 = zcoef0 * fse3w(ji,jj,1)
            ! hydrostatic pressure gradient
            zhpi(ji,jj,1) = zcoef1 * ( rhd(ji+1,jj  ,1) - rhd(ji,jj,1) ) / e1u(ji,jj)
            zhpj(ji,jj,1) = zcoef1 * ( rhd(ji  ,jj+1,1) - rhd(ji,jj,1) ) / e2v(ji,jj)
            ! add to the general momentum trend
            ua(ji,jj,1) = ua(ji,jj,1) + zhpi(ji,jj,1)
            va(ji,jj,1) = va(ji,jj,1) + zhpj(ji,jj,1)
         END DO
      END DO


      ! interior value (2=<jk=<jpkm1)
      DO jk = 2, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zcoef1 = zcoef0 * fse3w(ji,jj,jk)
               ! hydrostatic pressure gradient
               zhpi(ji,jj,jk) = zhpi(ji,jj,jk-1)   &
                  &           + zcoef1 * (  ( rhd(ji+1,jj,jk) + rhd(ji+1,jj,jk-1) )   &
                  &                       - ( rhd(ji  ,jj,jk) + rhd(ji  ,jj,jk-1) )  ) / e1u(ji,jj)

               zhpj(ji,jj,jk) = zhpj(ji,jj,jk-1)   &
                  &           + zcoef1 * (  ( rhd(ji,jj+1,jk) + rhd(ji,jj+1,jk-1) )   &
                  &                       - ( rhd(ji,jj,  jk) + rhd(ji,jj  ,jk-1) )  ) / e2v(ji,jj)
               ! add to the general momentum trend
               ua(ji,jj,jk) = ua(ji,jj,jk) + zhpi(ji,jj,jk)
               va(ji,jj,jk) = va(ji,jj,jk) + zhpj(ji,jj,jk)
            END DO
         END DO
      END DO


      ! partial steps correction at the last level  (use gru & grv computed in zpshde.F90)
# if defined key_vectopt_loop
         jj = 1
         DO ji = jpi+2, jpij-jpi-1   ! vector opt. (forced unrolling)
# else
      DO jj = 2, jpjm1
         DO ji = 2, jpim1
# endif
            iku = mbku(ji,jj)
            ikv = mbkv(ji,jj)
            zcoef2 = zcoef0 * MIN( fse3w(ji,jj,iku), fse3w(ji+1,jj  ,iku) )
            zcoef3 = zcoef0 * MIN( fse3w(ji,jj,ikv), fse3w(ji  ,jj+1,ikv) )
            IF( iku > 1 ) THEN            ! on i-direction (level 2 or more)
               ua  (ji,jj,iku) = ua(ji,jj,iku) - zhpi(ji,jj,iku)         ! subtract old value
               zhpi(ji,jj,iku) = zhpi(ji,jj,iku-1)                   &   ! compute the new one
                  &            + zcoef2 * ( rhd(ji+1,jj,iku-1) - rhd(ji,jj,iku-1) + gru(ji,jj) ) / e1u(ji,jj)
               ua  (ji,jj,iku) = ua(ji,jj,iku) + zhpi(ji,jj,iku)         ! add the new one to the general momentum trend
            ENDIF
            IF( ikv > 1 ) THEN            ! on j-direction (level 2 or more)
               va  (ji,jj,ikv) = va(ji,jj,ikv) - zhpj(ji,jj,ikv)         ! subtract old value
               zhpj(ji,jj,ikv) = zhpj(ji,jj,ikv-1)                   &   ! compute the new one
                  &            + zcoef3 * ( rhd(ji,jj+1,ikv-1) - rhd(ji,jj,ikv-1) + grv(ji,jj) ) / e2v(ji,jj)
               va  (ji,jj,ikv) = va(ji,jj,ikv) + zhpj(ji,jj,ikv)         ! add the new one to the general momentum trend
            ENDIF
# if ! defined key_vectopt_loop
         END DO
# endif
      END DO
      !
      CALL wrk_dealloc( jpi,jpj,jpk, zhpi, zhpj )
      !
   END SUBROUTINE hpg_zps


   SUBROUTINE hpg_sco( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE hpg_sco  ***
      !!
      !! ** Method  :   s-coordinate case. Jacobian scheme.
      !!      The now hydrostatic pressure gradient at a given level, jk,
      !!      is computed by taking the vertical integral of the in-situ
      !!      density gradient along the model level from the suface to that
      !!      level. s-coordinates (ln_sco): a corrective term is added
      !!      to the horizontal pressure gradient :
      !!         zhpi = grav .....  + 1/e1u mi(rhd) di[ grav dep3w ]
      !!         zhpj = grav .....  + 1/e2v mj(rhd) dj[ grav dep3w ]
      !!      add it to the general momentum trend (ua,va).
      !!         ua = ua - 1/e1u * zhpi
      !!         va = va - 1/e2v * zhpj
      !!
      !! ** Action : - Update (ua,va) with the now hydrastatic pressure trend
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt    ! ocean time-step index
      !!
      INTEGER  ::   ji, jj, jk                 ! dummy loop indices
      REAL(wp) ::   zcoef0, zuap, zvap, znad   ! temporary scalars
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  zhpi, zhpj 
      !!----------------------------------------------------------------------
      !
      CALL wrk_alloc( jpi,jpj,jpk, zhpi, zhpj )
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn:hpg_sco : hydrostatic pressure gradient trend'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~   s-coordinate case, OPA original scheme used'
      ENDIF

      ! Local constant initialization
      zcoef0 = - grav * 0.5_wp
      ! To use density and not density anomaly
      IF ( lk_vvl ) THEN   ;     znad = 1._wp          ! Variable volume
      ELSE                 ;     znad = 0._wp         ! Fixed volume
      ENDIF

      ! Surface value
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.   
            ! hydrostatic pressure gradient along s-surfaces
            zhpi(ji,jj,1) = zcoef0 / e1u(ji,jj) * ( fse3w(ji+1,jj  ,1) * ( znad + rhd(ji+1,jj  ,1) )   &
               &                                  - fse3w(ji  ,jj  ,1) * ( znad + rhd(ji  ,jj  ,1) ) )
            zhpj(ji,jj,1) = zcoef0 / e2v(ji,jj) * ( fse3w(ji  ,jj+1,1) * ( znad + rhd(ji  ,jj+1,1) )   &
               &                                  - fse3w(ji  ,jj  ,1) * ( znad + rhd(ji  ,jj  ,1) ) )
            ! s-coordinate pressure gradient correction
            zuap = -zcoef0 * ( rhd   (ji+1,jj,1) + rhd   (ji,jj,1) + 2._wp * znad )   &
               &           * ( fsde3w(ji+1,jj,1) - fsde3w(ji,jj,1) ) / e1u(ji,jj)
            zvap = -zcoef0 * ( rhd   (ji,jj+1,1) + rhd   (ji,jj,1) + 2._wp * znad )   &
               &           * ( fsde3w(ji,jj+1,1) - fsde3w(ji,jj,1) ) / e2v(ji,jj)
            ! add to the general momentum trend
            ua(ji,jj,1) = ua(ji,jj,1) + zhpi(ji,jj,1) + zuap
            va(ji,jj,1) = va(ji,jj,1) + zhpj(ji,jj,1) + zvap
         END DO  
      END DO   
            
      ! interior value (2=<jk=<jpkm1)
      DO jk = 2, jpkm1                                  
         DO jj = 2, jpjm1     
            DO ji = fs_2, fs_jpim1   ! vector opt.      
               ! hydrostatic pressure gradient along s-surfaces
               zhpi(ji,jj,jk) = zhpi(ji,jj,jk-1) + zcoef0 / e1u(ji,jj)   & 
                  &           * (  fse3w(ji+1,jj,jk) * ( rhd(ji+1,jj,jk) + rhd(ji+1,jj,jk-1) + 2*znad )   & 
                  &              - fse3w(ji  ,jj,jk) * ( rhd(ji  ,jj,jk) + rhd(ji  ,jj,jk-1) + 2*znad )  )
               zhpj(ji,jj,jk) = zhpj(ji,jj,jk-1) + zcoef0 / e2v(ji,jj)   &
                  &           * (  fse3w(ji,jj+1,jk) * ( rhd(ji,jj+1,jk) + rhd(ji,jj+1,jk-1) + 2*znad )   &
                  &              - fse3w(ji,jj  ,jk) * ( rhd(ji,jj,  jk) + rhd(ji,jj  ,jk-1) + 2*znad )  )
               ! s-coordinate pressure gradient correction
               zuap = -zcoef0 * ( rhd   (ji+1,jj  ,jk) + rhd   (ji,jj,jk) + 2._wp * znad )   &
                  &           * ( fsde3w(ji+1,jj  ,jk) - fsde3w(ji,jj,jk) ) / e1u(ji,jj)
               zvap = -zcoef0 * ( rhd   (ji  ,jj+1,jk) + rhd   (ji,jj,jk) + 2._wp * znad )   &
                  &           * ( fsde3w(ji  ,jj+1,jk) - fsde3w(ji,jj,jk) ) / e2v(ji,jj)
               ! add to the general momentum trend
               ua(ji,jj,jk) = ua(ji,jj,jk) + zhpi(ji,jj,jk) + zuap
               va(ji,jj,jk) = va(ji,jj,jk) + zhpj(ji,jj,jk) + zvap
            END DO
         END DO
      END DO
      !
      CALL wrk_dealloc( jpi,jpj,jpk, zhpi, zhpj )
      !
   END SUBROUTINE hpg_sco

   SUBROUTINE hpg_djc( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE hpg_djc  ***
      !!
      !! ** Method  :   Density Jacobian with Cubic polynomial scheme
      !! 
      !! Reference: Shchepetkin and McWilliams, J. Geophys. Res., 108(C3), 3090, 2003
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt    ! ocean time-step index
      !!
      INTEGER  ::   ji, jj, jk          ! dummy loop indices
      REAL(wp) ::   zcoef0, zep, cffw   ! temporary scalars
      REAL(wp) ::   z1_10, cffu, cffx   !    "         "
      REAL(wp) ::   z1_12, cffv, cffy   !    "         "
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  zhpi, zhpj 
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  dzx, dzy, dzz, dzu, dzv, dzw
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  drhox, drhoy, drhoz, drhou, drhov, drhow
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  rho_i, rho_j, rho_k
      !!----------------------------------------------------------------------
      !
      CALL wrk_alloc( jpi, jpj, jpk, dzx  , dzy  , dzz  , dzu  , dzv  , dzw   ) 
      CALL wrk_alloc( jpi, jpj, jpk, drhox, drhoy, drhoz, drhou, drhov, drhow ) 
      CALL wrk_alloc( jpi, jpj, jpk, rho_i, rho_j, rho_k,  zhpi,  zhpj        ) 
      !

      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn:hpg_djc : hydrostatic pressure gradient trend'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~   s-coordinate case, density Jacobian with cubic polynomial scheme'
      ENDIF

      ! Local constant initialization
      zcoef0 = - grav * 0.5_wp
      z1_10  = 1._wp / 10._wp
      z1_12  = 1._wp / 12._wp

      !----------------------------------------------------------------------------------------
      !  compute and store in provisional arrays elementary vertical and horizontal differences
      !----------------------------------------------------------------------------------------

!!bug gm   Not a true bug, but... dzz=e3w  for dzx, dzy verify what it is really

      DO jk = 2, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               drhoz(ji,jj,jk) = rhd   (ji  ,jj  ,jk) - rhd   (ji,jj,jk-1)
               dzz  (ji,jj,jk) = fsde3w(ji  ,jj  ,jk) - fsde3w(ji,jj,jk-1)
               drhox(ji,jj,jk) = rhd   (ji+1,jj  ,jk) - rhd   (ji,jj,jk  )
               dzx  (ji,jj,jk) = fsde3w(ji+1,jj  ,jk) - fsde3w(ji,jj,jk  )
               drhoy(ji,jj,jk) = rhd   (ji  ,jj+1,jk) - rhd   (ji,jj,jk  )
               dzy  (ji,jj,jk) = fsde3w(ji  ,jj+1,jk) - fsde3w(ji,jj,jk  )
            END DO
         END DO
      END DO

      !-------------------------------------------------------------------------
      ! compute harmonic averages using eq. 5.18
      !-------------------------------------------------------------------------
      zep = 1.e-15

!!bug  gm  drhoz not defined at level 1 and used (jk-1 with jk=2)
!!bug  gm  idem for drhox, drhoy et ji=jpi and jj=jpj

      DO jk = 2, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               cffw = 2._wp * drhoz(ji  ,jj  ,jk) * drhoz(ji,jj,jk-1)

               cffu = 2._wp * drhox(ji+1,jj  ,jk) * drhox(ji,jj,jk  )
               cffx = 2._wp * dzx  (ji+1,jj  ,jk) * dzx  (ji,jj,jk  )
  
               cffv = 2._wp * drhoy(ji  ,jj+1,jk) * drhoy(ji,jj,jk  )
               cffy = 2._wp * dzy  (ji  ,jj+1,jk) * dzy  (ji,jj,jk  )

               IF( cffw > zep) THEN
                  drhow(ji,jj,jk) = 2._wp *   drhoz(ji,jj,jk) * drhoz(ji,jj,jk-1)   &
                     &                    / ( drhoz(ji,jj,jk) + drhoz(ji,jj,jk-1) )
               ELSE
                  drhow(ji,jj,jk) = 0._wp
               ENDIF

               dzw(ji,jj,jk) = 2._wp *   dzz(ji,jj,jk) * dzz(ji,jj,jk-1)   &
                  &                  / ( dzz(ji,jj,jk) + dzz(ji,jj,jk-1) )

               IF( cffu > zep ) THEN
                  drhou(ji,jj,jk) = 2._wp *   drhox(ji+1,jj,jk) * drhox(ji,jj,jk)   &
                     &                    / ( drhox(ji+1,jj,jk) + drhox(ji,jj,jk) )
               ELSE
                  drhou(ji,jj,jk ) = 0._wp
               ENDIF

               IF( cffx > zep ) THEN
                  dzu(ji,jj,jk) = 2._wp *   dzx(ji+1,jj,jk) * dzx(ji,jj,jk)   &
                     &                  / ( dzx(ji+1,jj,jk) + dzx(ji,jj,jk) )
               ELSE
                  dzu(ji,jj,jk) = 0._wp
               ENDIF

               IF( cffv > zep ) THEN
                  drhov(ji,jj,jk) = 2._wp *   drhoy(ji,jj+1,jk) * drhoy(ji,jj,jk)   &
                     &                    / ( drhoy(ji,jj+1,jk) + drhoy(ji,jj,jk) )
               ELSE
                  drhov(ji,jj,jk) = 0._wp
               ENDIF

               IF( cffy > zep ) THEN
                  dzv(ji,jj,jk) = 2._wp *   dzy(ji,jj+1,jk) * dzy(ji,jj,jk)   &
                     &                  / ( dzy(ji,jj+1,jk) + dzy(ji,jj,jk) )
               ELSE
                  dzv(ji,jj,jk) = 0._wp
               ENDIF

            END DO
         END DO
      END DO

      !----------------------------------------------------------------------------------
      ! apply boundary conditions at top and bottom using 5.36-5.37
      !----------------------------------------------------------------------------------
      drhow(:,:, 1 ) = 1.5_wp * ( drhoz(:,:, 2 ) - drhoz(:,:,  1  ) ) - 0.5_wp * drhow(:,:,  2  )
      drhou(:,:, 1 ) = 1.5_wp * ( drhox(:,:, 2 ) - drhox(:,:,  1  ) ) - 0.5_wp * drhou(:,:,  2  )
      drhov(:,:, 1 ) = 1.5_wp * ( drhoy(:,:, 2 ) - drhoy(:,:,  1  ) ) - 0.5_wp * drhov(:,:,  2  )

      drhow(:,:,jpk) = 1.5_wp * ( drhoz(:,:,jpk) - drhoz(:,:,jpkm1) ) - 0.5_wp * drhow(:,:,jpkm1)
      drhou(:,:,jpk) = 1.5_wp * ( drhox(:,:,jpk) - drhox(:,:,jpkm1) ) - 0.5_wp * drhou(:,:,jpkm1)
      drhov(:,:,jpk) = 1.5_wp * ( drhoy(:,:,jpk) - drhoy(:,:,jpkm1) ) - 0.5_wp * drhov(:,:,jpkm1)


      !--------------------------------------------------------------
      ! Upper half of top-most grid box, compute and store
      !-------------------------------------------------------------

!!bug gm   :  e3w-de3w = 0.5*e3w  ....  and de3w(2)-de3w(1)=e3w(2) ....   to be verified
!          true if de3w is really defined as the sum of the e3w scale factors as, it seems to me, it should be

      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            rho_k(ji,jj,1) = -grav * ( fse3w(ji,jj,1) - fsde3w(ji,jj,1) )               &
               &                   * (  rhd(ji,jj,1)                                    &
               &                     + 0.5_wp * ( rhd(ji,jj,2) - rhd(ji,jj,1) )         &
               &                              * ( fse3w (ji,jj,1) - fsde3w(ji,jj,1) )   &
               &                              / ( fsde3w(ji,jj,2) - fsde3w(ji,jj,1) )  ) 
         END DO
      END DO

!!bug gm    : here also, simplification is possible
!!bug gm    : optimisation: 1/10 and 1/12 the division should be done before the loop

      DO jk = 2, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.

               rho_k(ji,jj,jk) = zcoef0 * ( rhd   (ji,jj,jk) + rhd   (ji,jj,jk-1) )                                   &
                  &                     * ( fsde3w(ji,jj,jk) - fsde3w(ji,jj,jk-1) )                                   &
                  &            - grav * z1_10 * (                                                                     &
                  &     ( drhow (ji,jj,jk) - drhow (ji,jj,jk-1) )                                                     &
                  &   * ( fsde3w(ji,jj,jk) - fsde3w(ji,jj,jk-1) - z1_12 * ( dzw  (ji,jj,jk) + dzw  (ji,jj,jk-1) ) )   &
                  &   - ( dzw   (ji,jj,jk) - dzw   (ji,jj,jk-1) )                                                     &
                  &   * ( rhd   (ji,jj,jk) - rhd   (ji,jj,jk-1) - z1_12 * ( drhow(ji,jj,jk) + drhow(ji,jj,jk-1) ) )   &
                  &                             )

               rho_i(ji,jj,jk) = zcoef0 * ( rhd   (ji+1,jj,jk) + rhd   (ji,jj,jk) )                                   &
                  &                     * ( fsde3w(ji+1,jj,jk) - fsde3w(ji,jj,jk) )                                   &
                  &            - grav* z1_10 * (                                                                      &
                  &     ( drhou (ji+1,jj,jk) - drhou (ji,jj,jk) )                                                     &
                  &   * ( fsde3w(ji+1,jj,jk) - fsde3w(ji,jj,jk) - z1_12 * ( dzu  (ji+1,jj,jk) + dzu  (ji,jj,jk) ) )   &
                  &   - ( dzu   (ji+1,jj,jk) - dzu   (ji,jj,jk) )                                                     &
                  &   * ( rhd   (ji+1,jj,jk) - rhd   (ji,jj,jk) - z1_12 * ( drhou(ji+1,jj,jk) + drhou(ji,jj,jk) ) )   &
                  &                            )

               rho_j(ji,jj,jk) = zcoef0 * ( rhd   (ji,jj+1,jk) + rhd   (ji,jj,jk) )                                   &
                  &                     * ( fsde3w(ji,jj+1,jk) - fsde3w(ji,jj,jk) )                                   &
                  &            - grav* z1_10 * (                                                                      &
                  &     ( drhov (ji,jj+1,jk) - drhov (ji,jj,jk) )                                                     &
                  &   * ( fsde3w(ji,jj+1,jk) - fsde3w(ji,jj,jk) - z1_12 * ( dzv  (ji,jj+1,jk) + dzv  (ji,jj,jk) ) )   &
                  &   - ( dzv   (ji,jj+1,jk) - dzv   (ji,jj,jk) )                                                     &
                  &   * ( rhd   (ji,jj+1,jk) - rhd   (ji,jj,jk) - z1_12 * ( drhov(ji,jj+1,jk) + drhov(ji,jj,jk) ) )   &
                  &                            )

            END DO
         END DO
      END DO
      CALL lbc_lnk(rho_k,'W',1.)
      CALL lbc_lnk(rho_i,'U',1.)
      CALL lbc_lnk(rho_j,'V',1.)


      ! ---------------
      !  Surface value
      ! ---------------
      DO jj = 2, jpjm1
         DO ji = fs_2, fs_jpim1   ! vector opt.
            zhpi(ji,jj,1) = ( rho_k(ji+1,jj  ,1) - rho_k(ji,jj,1) - rho_i(ji,jj,1) ) / e1u(ji,jj)
            zhpj(ji,jj,1) = ( rho_k(ji  ,jj+1,1) - rho_k(ji,jj,1) - rho_j(ji,jj,1) ) / e2v(ji,jj)
            ! add to the general momentum trend
            ua(ji,jj,1) = ua(ji,jj,1) + zhpi(ji,jj,1)
            va(ji,jj,1) = va(ji,jj,1) + zhpj(ji,jj,1)
         END DO
      END DO

      ! ----------------
      !  interior value   (2=<jk=<jpkm1)
      ! ----------------
      DO jk = 2, jpkm1
         DO jj = 2, jpjm1 
            DO ji = fs_2, fs_jpim1   ! vector opt.
               ! hydrostatic pressure gradient along s-surfaces
               zhpi(ji,jj,jk) = zhpi(ji,jj,jk-1)                                &
                  &           + (  ( rho_k(ji+1,jj,jk) - rho_k(ji,jj,jk  ) )    &
                  &              - ( rho_i(ji  ,jj,jk) - rho_i(ji,jj,jk-1) )  ) / e1u(ji,jj)
               zhpj(ji,jj,jk) = zhpj(ji,jj,jk-1)                                &
                  &           + (  ( rho_k(ji,jj+1,jk) - rho_k(ji,jj,jk  ) )    &
                  &               -( rho_j(ji,jj  ,jk) - rho_j(ji,jj,jk-1) )  ) / e2v(ji,jj)
               ! add to the general momentum trend
               ua(ji,jj,jk) = ua(ji,jj,jk) + zhpi(ji,jj,jk)
               va(ji,jj,jk) = va(ji,jj,jk) + zhpj(ji,jj,jk)
            END DO
         END DO
      END DO
      !
      CALL wrk_dealloc( jpi, jpj, jpk, dzx  , dzy  , dzz  , dzu  , dzv  , dzw   ) 
      CALL wrk_dealloc( jpi, jpj, jpk, drhox, drhoy, drhoz, drhou, drhov, drhow ) 
      CALL wrk_dealloc( jpi, jpj, jpk, rho_i, rho_j, rho_k,  zhpi,  zhpj        ) 
      !
   END SUBROUTINE hpg_djc


   SUBROUTINE hpg_prj( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE hpg_prj  ***
      !!
      !! ** Method  :   s-coordinate case.
      !!      A Pressure-Jacobian horizontal pressure gradient method
      !!      based on the constrained cubic-spline interpolation for
      !!      all vertical coordinate systems
      !!
      !! ** Action : - Update (ua,va) with the now hydrastatic pressure trend
      !!             - Save the trend (l_trddyn=T)
      !!
      !!----------------------------------------------------------------------
      INTEGER, PARAMETER  :: polynomial_type = 1    ! 1: cubic spline, 2: linear
      INTEGER, INTENT(in) ::   kt                   ! ocean time-step index
      !!
      INTEGER  ::   ji, jj, jk, jkk                 ! dummy loop indices
      REAL(wp) ::   zcoef0, znad                    ! temporary scalars
      !!
      !! The local variables for the correction term
      INTEGER  :: jk1, jis, jid, jjs, jjd
      REAL(wp) :: zuijk, zvijk, zpwes, zpwed, zpnss, zpnsd, zdeps
      REAL(wp) :: zrhdt1 
      REAL(wp) :: zdpdx1, zdpdx2, zdpdy1, zdpdy2
      INTEGER  :: zbhitwe, zbhitns
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zdeptht, zrhh 
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zhpi, zu, zv, fsp, xsp, asp, bsp, csp, dsp
      !!----------------------------------------------------------------------
      !
      CALL wrk_alloc( jpi,jpj,jpk, zhpi, zu, zv, fsp, xsp, asp, bsp, csp, dsp ) 
      CALL wrk_alloc( jpi,jpj,jpk, zdeptht, zrhh ) 
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn:hpg_prj : hydrostatic pressure gradient trend'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~   s-coordinate case, cubic spline pressure Jacobian'
      ENDIF

      !!----------------------------------------------------------------------
      ! Local constant initialization
      zcoef0 = - grav 
      znad = 0.0_wp
      IF( lk_vvl ) znad = 1._wp

      ! Clean 3-D work arrays
      zhpi(:,:,:) = 0._wp
      zrhh(:,:,:) = rhd(:,:,:)
      
      ! Preparing vertical density profile "zrhh(:,:,:)" for hybrid-sco coordinate
      DO jj = 1, jpj
        DO ji = 1, jpi   
          jk = mbathy(ji,jj)
          IF( jk <= 0 ) THEN; zrhh(ji,jj,:) = 0._wp
          ELSE IF(jk == 1) THEN; zrhh(ji,jj, jk+1:jpk) = rhd(ji,jj,jk)
          ELSE IF(jk < jpkm1) THEN
             DO jkk = jk+1, jpk
                zrhh(ji,jj,jkk) = interp1(fsde3w(ji,jj,jkk),   fsde3w(ji,jj,jkk-1), &
                                         fsde3w(ji,jj,jkk-2), rhd(ji,jj,jkk-1), rhd(ji,jj,jkk-2))
             END DO 
          ENDIF
        END DO
      END DO

      ! Transfer the depth of "T(:,:,:)" to vertical coordinate "zdeptht(:,:,:)"
      DO jj = 1, jpj
        DO ji = 1, jpi
          zdeptht(ji,jj,1) = 0.5_wp * fse3w(ji,jj,1)
          zdeptht(ji,jj,1) = zdeptht(ji,jj,1) - sshn(ji,jj) * znad
          DO jk = 2, jpk
             zdeptht(ji,jj,jk) = zdeptht(ji,jj,jk-1) + fse3w(ji,jj,jk)
          END DO
        END DO
      END DO

      DO jk = 1, jpkm1
        DO jj = 1, jpj
          DO ji = 1, jpi
            fsp(ji,jj,jk) = zrhh(ji,jj,jk)
            xsp(ji,jj,jk) = zdeptht(ji,jj,jk)
          END DO
        END DO
      END DO

      ! Construct the vertical density profile with the 
      ! constrained cubic spline interpolation
      ! rho(z) = asp + bsp*z + csp*z^2 + dsp*z^3
      CALL cspline(fsp,xsp,asp,bsp,csp,dsp,polynomial_type)      

      ! Integrate the hydrostatic pressure "zhpi(:,:,:)" at "T(ji,jj,1)"
      DO jj = 2, jpj
        DO ji = 2, jpi 
          zrhdt1 = zrhh(ji,jj,1) - interp3(zdeptht(ji,jj,1),asp(ji,jj,1), &
                                         bsp(ji,jj,1),   csp(ji,jj,1), &
                                         dsp(ji,jj,1) ) * 0.5_wp * zdeptht(ji,jj,1)
          zrhdt1 = MAX(zrhdt1, 1000._wp - rau0)        ! no lighter than fresh water

          ! assuming linear profile across the top half surface layer
          zhpi(ji,jj,1) =  0.5_wp * fse3w(ji,jj,1) * zrhdt1  
        END DO
      END DO

      ! Calculate the pressure "zhpi(:,:,:)" at "T(ji,jj,2:jpkm1)"
      DO jk = 2, jpkm1                                  
        DO jj = 2, jpj     
          DO ji = 2, jpi
            zhpi(ji,jj,jk) = zhpi(ji,jj,jk-1) +                          &
                             integ2(zdeptht(ji,jj,jk-1), zdeptht(ji,jj,jk),&
                                    asp(ji,jj,jk-1),    bsp(ji,jj,jk-1), &
                                    csp(ji,jj,jk-1),    dsp(ji,jj,jk-1))
          END DO
        END DO
      END DO

      ! Z coordinate of U(ji,jj,1:jpkm1) and V(ji,jj,1:jpkm1)
      DO jj = 2, jpjm1     
        DO ji = 2, jpim1  
          zu(ji,jj,1) = - ( fse3u(ji,jj,1) - sshu_n(ji,jj) * znad)
          zv(ji,jj,1) = - ( fse3v(ji,jj,1) - sshv_n(ji,jj) * znad)
        END DO
      END DO

      DO jk = 2, jpkm1                                  
        DO jj = 2, jpjm1     
          DO ji = 2, jpim1  
            zu(ji,jj,jk) = zu(ji,jj,jk-1)- fse3u(ji,jj,jk)
            zv(ji,jj,jk) = zv(ji,jj,jk-1)- fse3v(ji,jj,jk)
          END DO
        END DO
      END DO
               
      DO jk = 1, jpkm1                                  
        DO jj = 2, jpjm1     
          DO ji = 2, jpim1  
            zu(ji,jj,jk) = zu(ji,jj,jk) + 0.5_wp * fse3u(ji,jj,jk)
            zv(ji,jj,jk) = zv(ji,jj,jk) + 0.5_wp * fse3v(ji,jj,jk)
          END DO
        END DO
      END DO

      DO jk = 1, jpkm1                                  
        DO jj = 2, jpjm1     
          DO ji = 2, jpim1  
            zpwes = 0._wp; zpwed = 0._wp
            zpnss = 0._wp; zpnsd = 0._wp
            zuijk = zu(ji,jj,jk)
            zvijk = zv(ji,jj,jk)

            !!!!!     for u equation
            IF( jk <= mbku(ji,jj) ) THEN
               IF( -zdeptht(ji+1,jj,mbku(ji,jj)) >= -zdeptht(ji,jj,mbku(ji,jj)) ) THEN
                 jis = ji + 1; jid = ji
               ELSE
                 jis = ji;     jid = ji +1
               ENDIF

               ! integrate the pressure on the shallow side
               jk1 = jk 
               zbhitwe = 0
               DO WHILE ( -zdeptht(jis,jj,jk1) > zuijk )
                 IF( jk1 == mbku(ji,jj) ) THEN
                   zbhitwe = 1
                   EXIT
                 ENDIF
                 zdeps = MIN(zdeptht(jis,jj,jk1+1), -zuijk)
                 zpwes = zpwes +                                    & 
                      integ2(zdeptht(jis,jj,jk1), zdeps,            &
                             asp(jis,jj,jk1),    bsp(jis,jj,jk1), &
                             csp(jis,jj,jk1),    dsp(jis,jj,jk1))
                 jk1 = jk1 + 1
               END DO
            
               IF(zbhitwe == 1) THEN
                 zuijk = -zdeptht(jis,jj,jk1)
               ENDIF

               ! integrate the pressure on the deep side
               jk1 = jk 
               zbhitwe = 0
               DO WHILE ( -zdeptht(jid,jj,jk1) < zuijk )
                 IF( jk1 == 1 ) THEN
                   zbhitwe = 1
                   EXIT
                 ENDIF
                 zdeps = MAX(zdeptht(jid,jj,jk1-1), -zuijk)
                 zpwed = zpwed +                                        & 
                        integ2(zdeps,              zdeptht(jid,jj,jk1), &
                               asp(jid,jj,jk1-1), bsp(jid,jj,jk1-1),  &
                               csp(jid,jj,jk1-1), dsp(jid,jj,jk1-1) )
                 jk1 = jk1 - 1
               END DO
            
               IF( zbhitwe == 1 ) THEN
                 zdeps = zdeptht(jid,jj,1) + MIN(zuijk, sshn(jid,jj)*znad)
                 zrhdt1 = zrhh(jid,jj,1) - interp3(zdeptht(jid,jj,1), asp(jid,jj,1), &
                                                 bsp(jid,jj,1),    csp(jid,jj,1), &
                                                 dsp(jid,jj,1)) * zdeps
                 zrhdt1 = MAX(zrhdt1, 1000._wp - rau0)        ! no lighter than fresh water
                 zpwed  = zpwed + 0.5_wp * (zrhh(jid,jj,1) + zrhdt1) * zdeps
               ENDIF

               ! update the momentum trends in u direction

               zdpdx1 = zcoef0 / e1u(ji,jj) * (zhpi(ji+1,jj,jk) - zhpi(ji,jj,jk))
               IF( lk_vvl ) THEN
                 zdpdx2 = zcoef0 / e1u(ji,jj) * & 
                         ( REAL(jis-jid, wp) * (zpwes + zpwed) + (sshn(ji+1,jj)-sshn(ji,jj)) ) 
                ELSE
                 zdpdx2 = zcoef0 / e1u(ji,jj) * REAL(jis-jid, wp) * (zpwes + zpwed) 
               ENDIF

               ua(ji,jj,jk) = ua(ji,jj,jk) + (zdpdx1 + zdpdx2) * &
               &           umask(ji,jj,jk) * tmask(ji,jj,jk) * tmask(ji+1,jj,jk)
            ENDIF
  
            !!!!!     for v equation
            IF( jk <= mbkv(ji,jj) ) THEN
               IF( -zdeptht(ji,jj+1,mbkv(ji,jj)) >= -zdeptht(ji,jj,mbkv(ji,jj)) ) THEN
                 jjs = jj + 1; jjd = jj
               ELSE
                 jjs = jj    ; jjd = jj + 1
               ENDIF

               ! integrate the pressure on the shallow side
               jk1 = jk 
               zbhitns = 0
               DO WHILE ( -zdeptht(ji,jjs,jk1) > zvijk )
                 IF( jk1 == mbkv(ji,jj) ) THEN
                   zbhitns = 1
                   EXIT
                 ENDIF
                 zdeps = MIN(zdeptht(ji,jjs,jk1+1), -zvijk)
                 zpnss = zpnss +                                      & 
                        integ2(zdeptht(ji,jjs,jk1), zdeps,            &
                               asp(ji,jjs,jk1),    bsp(ji,jjs,jk1), &
                               csp(ji,jjs,jk1),    dsp(ji,jjs,jk1) )
                 jk1 = jk1 + 1
               END DO
            
               IF(zbhitns == 1) THEN
                 zvijk = -zdeptht(ji,jjs,jk1)
               ENDIF

               ! integrate the pressure on the deep side
               jk1 = jk 
               zbhitns = 0
               DO WHILE ( -zdeptht(ji,jjd,jk1) < zvijk )
                 IF( jk1 == 1 ) THEN
                   zbhitns = 1
                   EXIT
                 ENDIF
                 zdeps = MAX(zdeptht(ji,jjd,jk1-1), -zvijk)
                 zpnsd = zpnsd +                                        & 
                        integ2(zdeps,              zdeptht(ji,jjd,jk1), &
                               asp(ji,jjd,jk1-1), bsp(ji,jjd,jk1-1), &
                               csp(ji,jjd,jk1-1), dsp(ji,jjd,jk1-1) )
                 jk1 = jk1 - 1
               END DO
            
               IF( zbhitns == 1 ) THEN
                 zdeps = zdeptht(ji,jjd,1) + MIN(zvijk, sshn(ji,jjd)*znad)
                 zrhdt1 = zrhh(ji,jjd,1) - interp3(zdeptht(ji,jjd,1), asp(ji,jjd,1), &
                                                 bsp(ji,jjd,1),    csp(ji,jjd,1), &
                                                 dsp(ji,jjd,1) ) * zdeps
                 zrhdt1 = MAX(zrhdt1, 1000._wp - rau0)        ! no lighter than fresh water
                 zpnsd  = zpnsd + 0.5_wp * (zrhh(ji,jjd,1) + zrhdt1) * zdeps
               ENDIF

               ! update the momentum trends in v direction

               zdpdy1 = zcoef0 / e2v(ji,jj) * (zhpi(ji,jj+1,jk) - zhpi(ji,jj,jk))
               IF( lk_vvl ) THEN
                   zdpdy2 = zcoef0 / e2v(ji,jj) * &
                           ( REAL(jjs-jjd, wp) * (zpnss + zpnsd) + (sshn(ji,jj+1)-sshn(ji,jj)) ) 
               ELSE
                   zdpdy2 = zcoef0 / e2v(ji,jj) * REAL(jjs-jjd, wp) * (zpnss + zpnsd ) 
               ENDIF

               va(ji,jj,jk) = va(ji,jj,jk) + (zdpdy1 + zdpdy2)*&
               &              vmask(ji,jj,jk)*tmask(ji,jj,jk)*tmask(ji,jj+1,jk)
            ENDIF

                    
           END DO
        END DO
      END DO
      !
      CALL wrk_dealloc( jpi,jpj,jpk, zhpi, zu, zv, fsp, xsp, asp, bsp, csp, dsp ) 
      CALL wrk_dealloc( jpi,jpj,jpk, zdeptht, zrhh ) 
      !
   END SUBROUTINE hpg_prj

   SUBROUTINE cspline(fsp, xsp, asp, bsp, csp, dsp, polynomial_type)
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE cspline  ***
      !!       
      !! ** Purpose :   constrained cubic spline interpolation
      !!          
      !! ** Method  :   f(x) = asp + bsp*x + csp*x^2 + dsp*x^3 
      !! Reference: CJC Kruger, Constrained Cubic Spline Interpoltation
      !!
      !!----------------------------------------------------------------------
      IMPLICIT NONE
      REAL(wp), DIMENSION(:,:,:), INTENT(in)  :: fsp, xsp           ! value and coordinate
      REAL(wp), DIMENSION(:,:,:), INTENT(out) :: asp, bsp, csp, dsp ! coefficients of 
                                                                    ! the interpoated function
      INTEGER, INTENT(in) :: polynomial_type                        ! 1: cubic spline 
                                                                    ! 2: Linear

      ! Local Variables      
      INTEGER  ::   ji, jj, jk                 ! dummy loop indices
      INTEGER  ::   jpi, jpj, jpkm1
      REAL(wp) ::   zdf1, zdf2, zddf1, zddf2, ztmp1, ztmp2, zdxtmp
      REAL(wp) ::   zdxtmp1, zdxtmp2, zalpha
      REAL(wp) ::   zdf(size(fsp,3))
      !!----------------------------------------------------------------------

      jpi   = size(fsp,1)
      jpj   = size(fsp,2)
      jpkm1 = size(fsp,3) - 1

      
      IF (polynomial_type == 1) THEN     ! Constrained Cubic Spline
         DO ji = 1, jpi
            DO jj = 1, jpj
           !!Fritsch&Butland's method, 1984 (preferred, but more computation)              
           !    DO jk = 2, jpkm1-1
           !       zdxtmp1 = xsp(ji,jj,jk)   - xsp(ji,jj,jk-1)  
           !       zdxtmp2 = xsp(ji,jj,jk+1) - xsp(ji,jj,jk)  
           !       zdf1    = ( fsp(ji,jj,jk)   - fsp(ji,jj,jk-1) ) / zdxtmp1
           !       zdf2    = ( fsp(ji,jj,jk+1) - fsp(ji,jj,jk)   ) / zdxtmp2
           !
           !       zalpha = ( zdxtmp1 + 2._wp * zdxtmp2 ) / ( zdxtmp1 + zdxtmp2 ) / 3._wp
           !     
           !       IF(zdf1 * zdf2 <= 0._wp) THEN
           !           zdf(jk) = 0._wp
           !       ELSE
           !         zdf(jk) = zdf1 * zdf2 / ( ( 1._wp - zalpha ) * zdf1 + zalpha * zdf2 )
           !       ENDIF
           !    END DO
           
           !!Simply geometric average
               DO jk = 2, jpkm1-1
                  zdf1 = (fsp(ji,jj,jk) - fsp(ji,jj,jk-1)) / (xsp(ji,jj,jk) - xsp(ji,jj,jk-1))
                  zdf2 = (fsp(ji,jj,jk+1) - fsp(ji,jj,jk)) / (xsp(ji,jj,jk+1) - xsp(ji,jj,jk))
            
                  IF(zdf1 * zdf2 <= 0._wp) THEN
                     zdf(jk) = 0._wp
                  ELSE
                     zdf(jk) = 2._wp * zdf1 * zdf2 / (zdf1 + zdf2)
                  ENDIF
               END DO
           
               zdf(1)     = 1.5_wp * ( fsp(ji,jj,2) - fsp(ji,jj,1) ) / &
                          &          ( xsp(ji,jj,2) - xsp(ji,jj,1) ) -  0.5_wp * zdf(2)
               zdf(jpkm1) = 1.5_wp * ( fsp(ji,jj,jpkm1) - fsp(ji,jj,jpkm1-1) ) / &
                          &          ( xsp(ji,jj,jpkm1) - xsp(ji,jj,jpkm1-1) ) - &
                          & 0.5_wp * zdf(jpkm1 - 1)
   
               DO jk = 1, jpkm1 - 1
                 zdxtmp = xsp(ji,jj,jk+1) - xsp(ji,jj,jk) 
                 ztmp1  = (zdf(jk+1) + 2._wp * zdf(jk)) / zdxtmp
                 ztmp2  =  6._wp * (fsp(ji,jj,jk+1) - fsp(ji,jj,jk)) / zdxtmp / zdxtmp
                 zddf1  = -2._wp * ztmp1 + ztmp2 
                 ztmp1  = (2._wp * zdf(jk+1) + zdf(jk)) / zdxtmp
                 zddf2  =  2._wp * ztmp1 - ztmp2 
      
                 dsp(ji,jj,jk) = (zddf2 - zddf1) / 6._wp / zdxtmp
                 csp(ji,jj,jk) = ( xsp(ji,jj,jk+1) * zddf1 - xsp(ji,jj,jk)*zddf2 ) / 2._wp / zdxtmp
                 bsp(ji,jj,jk) = ( fsp(ji,jj,jk+1) - fsp(ji,jj,jk) ) / zdxtmp - & 
                               & csp(ji,jj,jk) * ( xsp(ji,jj,jk+1) + xsp(ji,jj,jk) ) - &
                               & dsp(ji,jj,jk) * ((xsp(ji,jj,jk+1) + xsp(ji,jj,jk))**2 - &
                               &                   xsp(ji,jj,jk+1) * xsp(ji,jj,jk))
                 asp(ji,jj,jk) = fsp(ji,jj,jk) - xsp(ji,jj,jk) * (bsp(ji,jj,jk) + &
                               &                (xsp(ji,jj,jk) * (csp(ji,jj,jk) + &
                               &                 dsp(ji,jj,jk) * xsp(ji,jj,jk))))
               END DO
            END DO
         END DO
 
      ELSE IF (polynomial_type == 2) THEN     ! Linear
         DO ji = 1, jpi
            DO jj = 1, jpj
               DO jk = 1, jpkm1-1
                  zdxtmp =xsp(ji,jj,jk+1) - xsp(ji,jj,jk) 
                  ztmp1 = fsp(ji,jj,jk+1) - fsp(ji,jj,jk)
   
                  dsp(ji,jj,jk) = 0._wp
                  csp(ji,jj,jk) = 0._wp
                  bsp(ji,jj,jk) = ztmp1 / zdxtmp
                  asp(ji,jj,jk) = fsp(ji,jj,jk) - bsp(ji,jj,jk) * xsp(ji,jj,jk)
               END DO
            END DO
         END DO

      ELSE
           CALL ctl_stop( 'invalid polynomial type in cspline' )
      ENDIF

      
   END SUBROUTINE cspline


   FUNCTION interp1(x, xl, xr, fl, fr)  RESULT(f) 
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE interp1  ***
      !!       
      !! ** Purpose :   1-d linear interpolation
      !!          
      !! ** Method  :  
      !!                interpolation is straight forward
      !!                extrapolation is also permitted (no value limit) 
      !!
      !!----------------------------------------------------------------------
      IMPLICIT NONE
      REAL(wp), INTENT(in) ::  x, xl, xr, fl, fr   
      REAL(wp)             ::  f ! result of the interpolation (extrapolation)
      REAL(wp)             ::  zdeltx
      !!----------------------------------------------------------------------

      zdeltx = xr - xl
      IF(abs(zdeltx) <= 10._wp * EPSILON(x)) THEN
        f = 0.5_wp * (fl + fr)
      ELSE
        f = ( (x - xl ) * fr - ( x - xr ) * fl ) / zdeltx
      ENDIF
      
   END FUNCTION interp1

   FUNCTION interp2(x, a, b, c, d)  RESULT(f) 
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE interp1  ***
      !!       
      !! ** Purpose :   1-d constrained cubic spline interpolation
      !!          
      !! ** Method  :  cubic spline interpolation
      !!
      !!----------------------------------------------------------------------
      IMPLICIT NONE
      REAL(wp), INTENT(in) ::  x, a, b, c, d   
      REAL(wp)             ::  f ! value from the interpolation
      !!----------------------------------------------------------------------

      f = a + x* ( b + x * ( c + d * x ) ) 

   END FUNCTION interp2


   FUNCTION interp3(x, a, b, c, d)  RESULT(f) 
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE interp1  ***
      !!       
      !! ** Purpose :   Calculate the first order of deriavtive of
      !!                a cubic spline function y=a+b*x+c*x^2+d*x^3
      !!          
      !! ** Method  :   f=dy/dx=b+2*c*x+3*d*x^2
      !!
      !!----------------------------------------------------------------------
      IMPLICIT NONE
      REAL(wp), INTENT(in) ::  x, a, b, c, d   
      REAL(wp)             ::  f ! value from the interpolation
      !!----------------------------------------------------------------------

      f = b + x * ( 2._wp * c + 3._wp * d * x)

   END FUNCTION interp3

   
   FUNCTION integ2(xl, xr, a, b, c, d)  RESULT(f) 
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE interp1  ***
      !!       
      !! ** Purpose :   1-d constrained cubic spline integration
      !!          
      !! ** Method  :  integrate polynomial a+bx+cx^2+dx^3 from xl to xr 
      !!
      !!----------------------------------------------------------------------
      IMPLICIT NONE
      REAL(wp), INTENT(in) ::  xl, xr, a, b, c, d   
      REAL(wp)             ::  za1, za2, za3      
      REAL(wp)             ::  f                   ! integration result
      !!----------------------------------------------------------------------

      za1 = 0.5_wp * b 
      za2 = c / 3.0_wp 
      za3 = 0.25_wp * d 

      f  = xr * ( a + xr * ( za1 + xr * ( za2 + za3 * xr ) ) ) - &
         & xl * ( a + xl * ( za1 + xl * ( za2 + za3 * xl ) ) )

   END FUNCTION integ2


   !!======================================================================
END MODULE dynhpg
