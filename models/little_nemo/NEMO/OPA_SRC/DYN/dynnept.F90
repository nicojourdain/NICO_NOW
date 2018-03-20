MODULE dynnept
   !!======================================================================
   !!                       ***  MODULE  dynnept  ***
   !! Ocean dynamics: Neptune effect as proposed by Greg Holloway,
   !!                 recoded version of simplest case (u*, v* only)
   !!======================================================================
   !! History :  1.0  !  2007-06  (Michael Dunphy)  Modular form: - new namelist parameters
   !!                                                             - horizontal diffusion for Neptune
   !!                                                             - vertical diffusion for gm in momentum eqns
   !!                                                             - option to use Neptune in Coriolis eqn
   !!                    2011-08  (Jeff Blundell, NOCS) Simplified form for temporally invariant u*, v*
   !!                                               Horizontal and vertical diffusivity formulations removed
   !!                                               Dynamic allocation of storage added
   !!                                               Option of ramping Neptune vel. down
   !!                                               to zero added in shallow depths added
   !!----------------------------------------------------------------------
   !! dynnept_alloc        :
   !! dyn_nept_init        :
   !! dyn_nept_div_cur_init:
   !! dyn_nept_cor         :
   !! dyn_nept_vel         :
   !! dyn_nept_smooth_vel  :
   !!----------------------------------------------------------------------
   USE oce              ! ocean dynamics and tracers
   USE dom_oce          ! ocean space and time domain
   USE obc_oce          ! ocean lateral open boundary condition
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! distributed memory computing
   USE prtctl           ! Print control
   USE phycst
   USE lbclnk
   USE wrk_nemo        ! Memory Allocation

   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC dyn_nept_init      ! routine called by nemogcm.F90
   PUBLIC dyn_nept_cor       ! routine called by step.F90
   !! dynnept_alloc()         is called only by dyn_nept_init, within this module
   !! dyn_nept_div_cur_init   is called only by dyn_nept_init, within this module
   !! dyn_nept_vel            is called only by dyn_nept_cor,  within this module

   !! * Shared module variables
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)    :: zunep, zvnep  ! Neptune u and v
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: zhdivnep      ! hor. div for Neptune vel.
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: zmrotnep      ! curl for Neptune vel.


   !! * Namelist namdyn_nept variables
   LOGICAL, PUBLIC  ::  ln_neptsimp        = .FALSE.  ! yes/no simplified neptune

   LOGICAL          ::  ln_smooth_neptvel  = .FALSE.  ! yes/no smooth zunep, zvnep
   REAL(wp)         ::  rn_tslse           =  1.2e4   ! value of lengthscale L at the equator
   REAL(wp)         ::  rn_tslsp           =  3.0e3   ! value of lengthscale L at the pole
!! Specify whether to ramp down the Neptune velocity in shallow
!! water, and the depth range controlling such ramping down
   LOGICAL          ::  ln_neptramp        = .FALSE.  ! ramp down Neptune velocity in shallow water
   REAL(wp)         ::  rn_htrmin          =  100.0   ! min. depth of transition range
   REAL(wp)         ::  rn_htrmax          =  200.0   ! max. depth of transition range

   !! * Module variables


   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !!   OPA 9.0 , implemented by Bedford Institute of Oceanography
   !!----------------------------------------------------------------------

 CONTAINS

   INTEGER FUNCTION dynnept_alloc()
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dynnept_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( zunep(jpi,jpj) , zvnep(jpi,jpj) ,     &
         &      zhdivnep(jpi,jpj,jpk) , zmrotnep(jpi,jpj,jpk) , STAT=dynnept_alloc )
         !
      IF( dynnept_alloc /= 0 )   CALL ctl_warn('dynnept_alloc: array allocate failed.')
   END FUNCTION dynnept_alloc


   SUBROUTINE dyn_nept_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_nept_init  ***
      !!
      !! ** Purpose :   Read namelist parameters, initialise arrays
      !!                and compute the arrays zunep and zvnep
      !!
      !! ** Method  :   zunep =
      !!                zvnep =
      !!
      !! ** History :  1.0  !   07-05  (Zeliang Wang)   Original code for zunep, zvnep
      !!               1.1  !   07-06  (Michael Dunphy) namelist and  initialisation
      !!               2.0  ! 2011-07  (Jeff Blundell, NOCS)
      !!                    ! Simplified form for temporally invariant u*, v*
      !!                    ! Horizontal and vertical diffusivity formulations removed
      !!                    ! Includes optional tapering-off in shallow depths
      !!----------------------------------------------------------------------
      USE iom
      !!
      INTEGER  ::   ji, jj, jk    ! dummy loop indices
      REAL(wp) :: unemin,unemax,vnemin,vnemax   ! extrema of (u*, v*) fields
      REAL(wp) :: zhdivmin,zhdivmax             ! extrema of horizontal divergence of (u*, v*) fields
      REAL(wp) :: zmrotmin,zmrotmax             ! extrema of the curl of the (u*, v*) fields
      REAL(wp) :: ustar,vstar                   ! (u*, v*) before tapering in shallow water
      REAL(wp) :: hramp                         ! depth over which Neptune vel. is ramped down
      !
      REAL(wp), POINTER, DIMENSION(:,:  ) :: ht, htn, tscale, tsp, hur_n, hvr_n, hu_n, hv_n      
      REAL(wp), POINTER, DIMENSION(:,:,:) :: znmask
      !!
      NAMELIST/namdyn_nept/ ln_neptsimp, ln_smooth_neptvel, rn_tslse, rn_tslsp,      &
                            ln_neptramp, rn_htrmin, rn_htrmax
      !!----------------------------------------------------------------------
      ! Define the (simplified) Neptune parameters
      ! ==========================================

      REWIND( numnam )                  ! Read Namelist namdyn_nept:  Simplified Neptune
      READ  ( numnam, namdyn_nept )

      IF(lwp) THEN                      ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'dyn_nept_init : Simplified Neptune module'
         WRITE(numout,*) '~~~~~~~~~~~~~'
         WRITE(numout,*) ' -->   Reading namelist namdyn_nept parameters:'
         WRITE(numout,*) '       ln_neptsimp          = ', ln_neptsimp
         WRITE(numout,*)
         IF( ln_neptsimp ) THEN
            WRITE(numout,*) '       ln_smooth_neptvel    = ', ln_smooth_neptvel
            WRITE(numout,*) '       rn_tslse             = ', rn_tslse
            WRITE(numout,*) '       rn_tslsp             = ', rn_tslsp
            WRITE(numout,*)
            WRITE(numout,*) '       ln_neptramp          = ', ln_neptramp
            WRITE(numout,*) '       rn_htrmin            = ', rn_htrmin
            WRITE(numout,*) '       rn_htrmax            = ', rn_htrmax
            WRITE(numout,*)
         ENDIF
      ENDIF
      !
      IF( .NOT. ln_neptsimp ) RETURN
      !                                 ! Dynamically allocate local work arrays
      CALL wrk_alloc( jpi, jpj     , ht, htn, tscale, tsp, hur_n, hvr_n, hu_n, hv_n  ) 
      CALL wrk_alloc( jpi, jpj, jpk, znmask                                          ) 

      IF( ln_smooth_neptvel ) THEN
         IF(lwp) WRITE(numout,*) ' -->   neptune velocities will be smoothed'
      ELSE
         IF(lwp) WRITE(numout,*) ' -->   neptune velocities will not be smoothed'
      ENDIF

      IF( ln_neptramp ) THEN
          IF(lwp) WRITE(numout,*) ' -->   ln_neptramp enabled, ramp down Neptune'
          IF(lwp) WRITE(numout,*) ' -->   velocity components in shallow water'
      ELSE
          IF(lwp) WRITE(numout,*) ' -->   ln_neptramp disabled'
      ENDIF


!!    Perform dynamic allocation of shared module variables
      IF( dynnept_alloc() /= 0 )   CALL ctl_warn('dynnept_alloc: array allocate failed.')

      IF( .not. ln_rstart ) THEN      ! If restarting, these arrays are read from the restart file
         zhdivnep(:,:,:) = 0.0_wp
         zmrotnep(:,:,:) = 0.0_wp
      END IF

      ! Computation of nmask: same as fmask, but fmask cannot be used
      ! because it is modified after it is computed in dom_msk
      ! (this can be optimised to save memory, such as merge into next loop)
      DO jk = 1, jpk
         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1   ! vector loop
               znmask(ji,jj,jk) = tmask(ji,jj  ,jk) * tmask(ji+1,jj  ,jk)   &
                   &            * tmask(ji,jj+1,jk) * tmask(ji+1,jj+1,jk)
            END DO
         END DO
      END DO

      CALL lbc_lnk( znmask, 'F', 1.0_wp )


      ! now compute zunep, zvnep (renamed from earlier versions)

      zunep(:,:) = 0.0_wp
      zvnep(:,:) = 0.0_wp

      htn(:,:) = 0.0_wp            ! ocean depth at F-point
      DO jk = 1, jpk
         htn(:,:) = htn(:,:) + fse3f(:,:,jk) * znmask(:,:,jk)
      END DO

      IF( ln_smooth_neptvel ) THEN
         CALL dyn_nept_smooth_vel( htn, ht, .TRUE. )
      !! overwrites ht with a smoothed version of htn
      ELSE
         ht(:,:) = htn(:,:)
      !! use unsmoothed version of htn
      ENDIF
      CALL lbc_lnk( ht, 'F', 1.0_wp )

      !! Compute tsp, a stream function for the Neptune velocity,
      !! with the usual geophysical sign convention
      !! Then zunep = -latitudinal derivative "-(1/H)*d(tsp)/dy"
      !!      zvnep = longitudinal derivative " (1/H)*d(tsp)/dx"

      tsp(:,:)    = 0.0_wp
      tscale(:,:) = 0.0_wp

      tscale(:,:) = rn_tslsp + (rn_tslse - rn_tslsp) *   &
                   ( 0.5_wp + 0.5_wp * COS( 2.0_wp * rad * gphif(:,:) )  )
      tsp   (:,:) = -2.0_wp * omega * SIN( rad * gphif(:,:) ) * tscale(:,:) * tscale(:,:) * ht(:,:)


      IF( ln_smooth_neptvel ) THEN
         CALL dyn_nept_smooth_vel( hu, hu_n, .TRUE. )
      !! overwrites hu_n with a smoothed version of hu
      ELSE
         hu_n(:,:) = hu(:,:)
      !! use unsmoothed version of hu
      ENDIF
      CALL lbc_lnk( hu_n, 'U', 1.0_wp )
      hu_n(:,:) = hu_n(:,:) * umask(:,:,1)

      WHERE( hu_n(:,:) == 0.0_wp )
         hur_n(:,:) = 0.0_wp
      ELSEWHERE
         hur_n(:,:) = 1.0_wp / hu_n(:,:)
      END WHERE


      IF( ln_smooth_neptvel ) THEN
         CALL dyn_nept_smooth_vel( hv, hv_n, .TRUE. )
      !! overwrites hv_n with a smoothed version of hv
      ELSE
         hv_n(:,:) = hv(:,:)
      !! use unsmoothed version of hv
      ENDIF
      CALL lbc_lnk( hv_n, 'V', 1.0_wp )
      hv_n(:,:) = hv_n(:,:) * vmask(:,:,1)

      WHERE( hv_n == 0.0_wp )
         hvr_n(:,:) = 0.0_wp
      ELSEWHERE
         hvr_n(:,:) = 1.0_wp / hv_n(:,:)
      END WHERE


      unemin =  1.0e35
      unemax = -1.0e35
      vnemin =  1.0e35
      vnemax = -1.0e35
      hramp = rn_htrmax - rn_htrmin
      DO jj = 2, jpj-1
         DO ji = 2, jpi-1
            if ( umask(ji,jj,1) /= 0.0_wp ) then
               ustar =-1.0_wp/e2u(ji,jj) * hur_n(ji,jj) * ( tsp(ji,jj)-tsp(ji,jj-1) ) * umask(ji,jj,1)
               if ( ln_neptramp ) then
!!                Apply ramp down to velocity component
                  if ( hu_n(ji,jj) <= rn_htrmin ) then
                    zunep(ji,jj) = 0.0_wp
                   else if ( hu_n(ji,jj) >= rn_htrmax ) then
                    zunep(ji,jj) = ustar
                   else if ( hramp > 0.0_wp ) then
                    zunep(ji,jj) = ( hu_n(ji,jj) - rn_htrmin) * ustar/hramp
                  endif
                else
                 zunep(ji,jj) = ustar
               endif 
             else
              zunep(ji,jj) = 0.0_wp
            endif
            if ( vmask(ji,jj,1) /= 0.0_wp ) then
               vstar = 1.0_wp/e1v(ji,jj) * hvr_n(ji,jj) * ( tsp(ji,jj)-tsp(ji-1,jj) ) * vmask(ji,jj,1)
               if ( ln_neptramp ) then
!!                Apply ramp down to velocity component
                  if ( hv_n(ji,jj) <= rn_htrmin ) then
                    zvnep(ji,jj) = 0.0_wp
                   else if ( hv_n(ji,jj) >= rn_htrmax ) then
                    zvnep(ji,jj) = vstar
                   else if ( hramp > 0.0_wp ) then
                    zvnep(ji,jj) = ( hv_n(ji,jj) - rn_htrmin) * vstar/hramp
                  endif
                else
                  zvnep(ji,jj) = vstar
               endif
             else
              zvnep(ji,jj) = 0.0_wp
            endif
            unemin = min( unemin, zunep(ji,jj) )
            unemax = max( unemax, zunep(ji,jj) )
            vnemin = min( vnemin, zvnep(ji,jj) )
            vnemax = max( vnemax, zvnep(ji,jj) )
         END DO
      END DO
      CALL lbc_lnk( zunep, 'U', -1.0_wp )
      CALL lbc_lnk( zvnep, 'V', -1.0_wp )
      WRITE(numout,*) '      zunep: min, max       = ', unemin,unemax
      WRITE(numout,*) '      zvnep: min, max       = ', vnemin,vnemax
      WRITE(numout,*)

      !!  Compute, once and for all, the horizontal divergence (zhdivnep)
      !!  and the curl (zmrotnep) of the Neptune velocity field (zunep, zvnep)
      CALL dyn_nept_div_cur_init

      !! Check the ranges of the computed divergence & vorticity
      zhdivmin =  1.0e35
      zhdivmax = -1.0e35
      zmrotmin =  1.0e35
      zmrotmax = -1.0e35
      hramp = rn_htrmax - rn_htrmin
      DO jk = 1, jpkm1                                 ! Horizontal slab
         DO jj = 2, jpj-1
            DO ji = 2, jpi-1
               zhdivmin = min( zhdivmin, zhdivnep(ji,jj,jk) )
               zhdivmax = max( zhdivmax, zhdivnep(ji,jj,jk) )
               zmrotmin = min( zmrotmin, zmrotnep(ji,jj,jk) )
               zmrotmax = max( zmrotmax, zmrotnep(ji,jj,jk) )
            END DO
         END DO
      END DO
      WRITE(numout,*) '   zhdivnep: min, max       = ', zhdivmin,zhdivmax
      WRITE(numout,*) '   zmrotnep: min, max       = ', zmrotmin,zmrotmax
      WRITE(numout,*)

!!    Deallocate temporary workspace arrays, which are all local to
!!    this routine, except where passed as arguments to other routines
      CALL wrk_dealloc( jpi, jpj     , ht, htn, tscale, tsp, hur_n, hvr_n, hu_n, hv_n  ) 
      CALL wrk_dealloc( jpi, jpj, jpk, znmask                                          ) 
      !
   END SUBROUTINE dyn_nept_init


   SUBROUTINE dyn_nept_div_cur_init
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE dyn_nept_div_cur_init  ***
      !!
      !! ** Purpose :   compute the horizontal divergence and the relative
      !!                vorticity of the time-invariant u* and v* Neptune
      !!                effect velocities (called zunep, zvnep)
      !!
      !! ** Method  : - Divergence:
      !!      - compute the divergence given by :
      !!         zhdivnep = 1/(e1t*e2t*e3t) ( di[e2u*e3u zunep] + dj[e1v*e3v zvnep] )
      !!      - compute the curl in tensorial formalism:
      !!         zmrotnep = 1/(e1f*e2f) ( di[e2v zvnep] - dj[e1u zunep] )
      !!      Note: Coastal boundary condition: lateral friction set through
      !!      the value of fmask along the coast (see dommsk.F90) and shlat
      !!      (namelist parameter)
      !!
      !! ** Action  : - compute zhdivnep, the hor. divergence of (u*, v*)
      !!              - compute zmrotnep, the rel. vorticity  of (u*, v*)
      !!
      !! History :  OPA  ! 1987-06  (P. Andrich, D. L Hostis)  Original code
      !!            4.0  ! 1991-11  (G. Madec)
      !!            6.0  ! 1993-03  (M. Guyon)  symetrical conditions
      !!            7.0  ! 1996-01  (G. Madec)  s-coordinates
      !!            8.0  ! 1997-06  (G. Madec)  lateral boundary cond., lbc
      !!            8.1  ! 1997-08  (J.M. Molines)  Open boundaries
      !!            8.2  ! 2000-03  (G. Madec)  no slip accurate
      !!  NEMO      1.0  ! 2002-09  (G. Madec, E. Durand)  Free form, F90
      !!             -   ! 2005-01  (J. Chanut) Unstructured open boundaries
      !!             -   ! 2003-08  (G. Madec)  merged of cur and div, free form, F90
      !!             -   ! 2005-01  (J. Chanut, A. Sellar) unstructured open boundaries
      !!            3.3  ! 2010-09  (D.Storkey and E.O'Dea) bug fixes for BDY module
      !!                 ! 2011-06  (Jeff Blundell, NOCS) Adapt code from divcur.F90
      !!                 !           to compute Neptune effect fields only
      !!----------------------------------------------------------------------
      !
      INTEGER  ::   ji, jj, jk          ! dummy loop indices
      !!----------------------------------------------------------------------
      !    
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'dyn_nept_div_cur_init :'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~~~~~~'
      IF(lwp) WRITE(numout,*) 'horizontal velocity divergence and'
      IF(lwp) WRITE(numout,*) 'relative vorticity of Neptune flow'
#if defined key_noslip_accurate
   !!----------------------------------------------------------------------
   !!   'key_noslip_accurate'                     2nd order centered scheme
   !!                                                4th order at the coast
   !!----------------------------------------------------------------------
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'WARNING: key_noslip_accurate option'
      IF(lwp) WRITE(numout,*) 'not implemented in simplified Neptune'
      CALL ctl_warn( ' noslip_accurate option not implemented' )
#endif

   !!----------------------------------------------------------------------
   !!   Default option                           2nd order centered schemes
   !!----------------------------------------------------------------------

      ! Apply the div and curl operators to the depth-dependent velocity
      ! field produced by multiplying (zunep, zvnep) by (umask, vmask), exactly
      ! equivalent to the equivalent calculation in the unsimplified code
      !                                                ! ===============
      DO jk = 1, jpkm1                                 ! Horizontal slab
         !                                             ! ===============
         !                                             ! --------
         ! Horizontal divergence                       !   div
         !                                             ! --------
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vector opt.
               zhdivnep(ji,jj,jk) =   &
               &   (  e2u(ji  ,jj  )*fse3u(ji  ,jj  ,jk) * zunep(ji  ,jj  ) * umask(ji  ,jj  ,jk)    &
               &    - e2u(ji-1,jj  )*fse3u(ji-1,jj  ,jk) * zunep(ji-1,jj  ) * umask(ji-1,jj  ,jk)    &
               &    + e1v(ji  ,jj  )*fse3v(ji  ,jj  ,jk) * zvnep(ji  ,jj  ) * vmask(ji  ,jj  ,jk)    &
               &    - e1v(ji  ,jj-1)*fse3v(ji  ,jj-1,jk) * zvnep(ji  ,jj-1) * vmask(ji  ,jj-1,jk) )  &
               &  / ( e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,jk) )
            END DO
         END DO

#if defined key_obc
         IF( Agrif_Root() ) THEN
            ! open boundaries (div must be zero behind the open boundary)
            !  mpp remark: The zeroing of zhdivnep can probably be extended to 1->jpi/jpj for the correct row/column
            IF( lp_obc_east  )  zhdivnep(nie0p1:nie1p1,nje0  :nje1  ,jk) = 0.0_wp      ! east
            IF( lp_obc_west  )  zhdivnep(niw0  :niw1  ,njw0  :njw1  ,jk) = 0.0_wp      ! west
            IF( lp_obc_north )  zhdivnep(nin0  :nin1  ,njn0p1:njn1p1,jk) = 0.0_wp      ! north
            IF( lp_obc_south )  zhdivnep(nis0  :nis1  ,njs0  :njs1  ,jk) = 0.0_wp      ! south
         ENDIF
#endif
         IF( .NOT. AGRIF_Root() ) THEN
            IF ((nbondi ==  1).OR.(nbondi == 2))  zhdivnep(nlci-1 , :     ,jk) = 0.0_wp   ! east
            IF ((nbondi == -1).OR.(nbondi == 2))  zhdivnep(2      , :     ,jk) = 0.0_wp   ! west
            IF ((nbondj ==  1).OR.(nbondj == 2))  zhdivnep(:      ,nlcj-1 ,jk) = 0.0_wp   ! north
            IF ((nbondj == -1).OR.(nbondj == 2))  zhdivnep(:      ,2      ,jk) = 0.0_wp   ! south
         ENDIF

         !                                             ! --------
         ! relative vorticity                          !   rot
         !                                             ! --------
         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1   ! vector opt.
               zmrotnep(ji,jj,jk) =   &
                  &       (  e2v(ji+1,jj  ) * zvnep(ji+1,jj  ) * vmask(ji+1,jj  ,jk)     &
                  &        - e2v(ji  ,jj  ) * zvnep(ji  ,jj  ) * vmask(ji  ,jj  ,jk)     &
                  &        - e1u(ji  ,jj+1) * zunep(ji  ,jj+1) * umask(ji  ,jj+1,jk)     &
                  &        + e1u(ji  ,jj  ) * zunep(ji  ,jj  ) * umask(ji  ,jj  ,jk)  )  &
                  &       * fmask(ji,jj,jk) / ( e1f(ji,jj) * e2f(ji,jj) )
            END DO
         END DO
         !                                             ! ===============
      END DO                                           !   End of slab
      !                                                ! ===============

      ! 4. Lateral boundary conditions on zhdivnep and zmrotnep
      ! ----------------------------------=======-----=======
      CALL lbc_lnk( zhdivnep, 'T', 1. )   ;   CALL lbc_lnk( zmrotnep , 'F', 1. )     ! lateral boundary cond. (no sign change)
      !
   END SUBROUTINE dyn_nept_div_cur_init


   SUBROUTINE dyn_nept_cor( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_nept_cor  ***
      !!
      !! ** Purpose :  Add or subtract the Neptune velocity from the now velocities
      !!
      !! ** Method  :  First call : kt not equal to lastkt -> subtract zunep, zvnep
      !!               Second call: kt     equal to lastkt -> add zunep, zvnep
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt       ! ocean time-step index
      !!
      INTEGER, SAVE :: lastkt             ! store previous kt
      DATA lastkt/-1/                     ! initialise previous kt
      !!----------------------------------------------------------------------
      !
      IF( ln_neptsimp ) THEN
         !
         IF( lastkt /= kt ) THEN          ! 1st call for this kt: subtract the Neptune velocities zunep, zvnep from un, vn
            CALL dyn_nept_vel( -1 )      ! -1 = subtract
            !
         ELSE                              ! 2nd call for this kt: add the Neptune velocities zunep, zvnep to un, vn
            CALL dyn_nept_vel(  1 )      !  1 = add
            !
         ENDIF
         !
         lastkt = kt     ! Store kt
         !
      ENDIF
      !
   END SUBROUTINE dyn_nept_cor


   SUBROUTINE dyn_nept_vel( ksign )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_nept_vel  ***
      !!
      !! ** Purpose :  Add or subtract the Neptune velocity from the now
      !!               velocities based on ksign
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   ksign    ! 1 or -1 to add or subtract neptune velocities
      !!
      INTEGER :: jk                       ! dummy loop index
      !!----------------------------------------------------------------------
      !
      ! Adjust the current velocity un, vn by adding or subtracting the
      ! Neptune velocities zunep, zvnep, as determined by argument ksign
      DO jk=1, jpk
         un(:,:,jk) = un(:,:,jk) + ksign * zunep(:,:) * umask(:,:,jk)
         vn(:,:,jk) = vn(:,:,jk) + ksign * zvnep(:,:) * vmask(:,:,jk)
      END DO
      !
   END SUBROUTINE dyn_nept_vel


   SUBROUTINE dyn_nept_smooth_vel( htold, htnew, ld_option )

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_nept_smooth_vel  ***
      !!
      !! ** Purpose :  Compute smoothed topography field.
      !!
      !! ** Action : - Updates the array htnew (output) with a smoothed
      !!               version of the (input) array htold. Form of smoothing
      !!               algorithm is controlled by the (logical) argument ld_option.
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in   )  ::  htold      ! temporary 2D workspace
      LOGICAL                     , INTENT(in   )  ::  ld_option  ! temporary 2D workspace
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout)  ::  htnew      ! temporary 2D workspace
      !
      INTEGER                           ::  ji, jj  ! dummy loop indices
      INTEGER , POINTER, DIMENSION(:,:) ::  nb, iwork
      REAL(wp), POINTER, DIMENSION(:,:) ::  work    ! temporary 2D workspace
      !!----------------------------------------------------------------------
      !
      CALL wrk_alloc( jpi, jpj, nb, iwork ) 
      CALL wrk_alloc( jpi, jpj, work      ) 
      !
      iwork(:,:) = 0

      !! iwork is a mask of gridpoints: iwork = 1 => ocean, iwork = 0 => land
      WHERE( htold(:,:) > 0 )
         iwork(:,:) = 1
         htnew(:,:) = htold(:,:)
      ELSEWHERE
         iwork(:,:) = 0
         htnew(:,:) = 0.0_wp
      END WHERE
      !! htnew contains valid ocean depths from htold, or zero

      !! set work to a smoothed/averaged version of htnew; choice controlled by ld_option
      !! nb is set to the sum of the weights of the valid values used in work
      IF( ld_option ) THEN

         !! Apply scale-selective smoothing in determining work from htnew
         DO jj=2,jpj-1
            DO ji=2,jpi-1
               work(ji,jj) = 4.0*htnew( ji , jj ) +                        &
                           & 2.0*htnew(ji+1, jj ) + 2.0*htnew(ji-1, jj ) + &
                           & 2.0*htnew( ji ,jj+1) + 2.0*htnew( ji ,jj-1) + &
                           &     htnew(ji+1,jj+1) +     htnew(ji+1,jj-1) + &
                           &     htnew(ji-1,jj+1) +     htnew(ji-1,jj-1)

               nb(ji,jj)   = 4 * iwork( ji , jj ) +                        &
                           & 2 * iwork(ji+1, jj ) + 2 * iwork(ji-1, jj ) + &
                           & 2 * iwork( ji ,jj+1) + 2 * iwork( ji ,jj-1) + &
                           &     iwork(ji+1,jj+1) +     iwork(ji+1,jj-1) + &
                           &     iwork(ji-1,jj+1) +     iwork(ji-1,jj-1)
            END DO
         END DO

      ELSE

         !! Apply simple 9-point averaging in determining work from htnew
         DO jj=2,jpj-1
            DO ji=2,jpi-1
               work(ji,jj) =  htnew( ji , jj ) +                    &
                           &  htnew(ji+1, jj ) + htnew(ji-1, jj ) + &
                           &  htnew( ji ,jj+1) + htnew( ji ,jj-1) + &
                           &  htnew(ji+1,jj+1) + htnew(ji+1,jj-1) + &
                           &  htnew(ji-1,jj+1) + htnew(ji-1,jj-1)

               nb(ji,jj) =    iwork( ji , jj ) +                    &
                           &  iwork(ji+1, jj ) + iwork(ji-1, jj ) + &
                           &  iwork( ji ,jj+1) + iwork( ji ,jj-1) + &
                           &  iwork(ji+1,jj+1) + iwork(ji+1,jj-1) + &
                           &  iwork(ji-1,jj+1) + iwork(ji-1,jj-1)
            END DO
         END DO

      ENDIF

      !! write averaged value of work into htnew,
      !! if average is valid and point is unmasked
      WHERE( (htold(:,:) /= 0.0_wp ) .AND. ( nb(:,:) /= 0 ) )
         htnew(:,:) = work(:,:)/real(nb(:,:))
      ELSEWHERE
         htnew(:,:) = 0.0_wp
      END WHERE

      !!    Deallocate temporary workspace arrays, all local to this routine
      CALL wrk_dealloc( jpi, jpj, nb, iwork ) 
      CALL wrk_dealloc( jpi, jpj, work      ) 
      !
   END SUBROUTINE dyn_nept_smooth_vel

END MODULE dynnept
