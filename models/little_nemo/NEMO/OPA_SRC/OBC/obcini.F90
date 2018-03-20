 MODULE obcini
   !!======================================================================
   !!                       ***  MODULE  obcini  ***
   !! OBC initial state :  Open boundary initial state
   !!======================================================================
   !! History :  8.0  !  97-07  (J.M. Molines, G. Madec)  Original code
   !!   NEMO     1.0  !  02-11  (C. Talandier, A-M. Treguier) Free surface, F90
   !!            2.0  !  05-11  (V. Garnier) Surface pressure gradient organization
   !!----------------------------------------------------------------------
#if defined key_obc
   !!----------------------------------------------------------------------
   !!   'key_obc'                                  Open Boundary Conditions
   !!----------------------------------------------------------------------
   !!   obc_init       : initialization for the open boundary condition
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE phycst          ! physical constants
   USE obc_oce         ! open boundary condition: ocean
   USE obcdta          ! open boundary condition: data
   USE in_out_manager  ! I/O units
   USE lib_mpp         ! MPP library
   USE dynspg_oce      ! flag lk_dynspg_flt

   IMPLICIT NONE
   PRIVATE

   PUBLIC   obc_init   ! routine called by opa.F90

   !! * Substitutions
#  include "obc_vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obcini.F90 2715 2011-03-30 15:58:35Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS
   
   SUBROUTINE obc_init
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE obc_init  ***
      !!         
      !! ** Purpose :   Initialization of the dynamics and tracer fields at 
      !!              the open boundaries.
      !!
      !! ** Method  :   initialization of open boundary variables
      !!      (u, v) over 3 time step and 3 rows
      !!      (t, s) over 2 time step and 2 rows
      !!      if ln_rstart = .FALSE. : no restart, fields set to zero
      !!      if ln_rstart = .TRUE.  : restart, fields are read in a file 
      !!      if rdpxxx = 0 then lfbc is set true for this boundary.
      !!
      !! ** Input   :   restart.obc file, restart file for open boundaries 
      !!----------------------------------------------------------------------
      USE obcrst,   ONLY :   obc_rst_read   ! Make obc_rst_read routine available
      !!
      INTEGER  ::   ji, jj, istop , inumfbc
      INTEGER, DIMENSION(4) ::   icorner
      REAL(wp), DIMENSION(2) ::   ztestmask
      !!
      NAMELIST/namobc/ rn_dpein, rn_dpwin, rn_dpnin, rn_dpsin,       &
         &             rn_dpeob, rn_dpwob, rn_dpnob, rn_dpsob,       &
         &             rn_volemp, nn_obcdta, cn_obcdta,    &
         &             ln_obc_clim, ln_vol_cst, ln_obc_fla,ln_obc_rstart
      !!----------------------------------------------------------------------

      REWIND( numnam )              ! Namelist namobc : open boundaries
      READ  ( numnam, namobc )

      ! convert DOCTOR namelist name into the OLD names
      nobc_dta = nn_obcdta
      cffile   = cn_obcdta
      rdpein   = rn_dpein
      rdpwin   = rn_dpwin
      rdpsin   = rn_dpsin
      rdpnin   = rn_dpnin
      rdpeob   = rn_dpeob
      rdpwob   = rn_dpwob
      rdpsob   = rn_dpsob
      rdpnob   = rn_dpnob
      volemp   = rn_volemp

      !                              ! allocate obc arrays
      IF( obc_oce_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'obc_init : unable to allocate obc_oce arrays' )
      ! allocation below not needed with new obcdta
      !IF( obc_dta_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'obc_init : unable to allocate obc_dta arrays' )

      ! By security we set rdpxin and rdpxob respectively to 1. and 15. if the corresponding OBC is not activated
      IF( .NOT.lp_obc_east  ) THEN   ;   rdpein = 1.   ;   rdpeob = 15.   ;   END IF
      IF( .NOT.lp_obc_west  ) THEN   ;   rdpwin = 1.   ;   rdpwob = 15.   ;   END IF
      IF( .NOT.lp_obc_north ) THEN   ;   rdpnin = 1.   ;   rdpnob = 15.   ;   END IF
      IF( .NOT.lp_obc_south ) THEN   ;   rdpsin = 1.   ;   rdpsob = 15.   ;   END IF

      ! number of open boudaries and open boundary indicators
      nbobc = 0
      IF( lp_obc_east  )   nbobc = nbobc + 1
      IF( lp_obc_west  )   nbobc = nbobc + 1
      IF( lp_obc_north )   nbobc = nbobc + 1
      IF( lp_obc_south )   nbobc = nbobc + 1

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'obc_init : initialization of open boundaries'
      IF(lwp) WRITE(numout,*) '~~~~~~~~'
      IF(lwp) WRITE(numout,*) '   Number of open boundaries    nbobc = ', nbobc
      IF(lwp) WRITE(numout,*)

      ! control prints
      IF(lwp) WRITE(numout,*) '   Namelist namobc'
      IF(lwp) WRITE(numout,*) '      data in file (=1) or initial state used (=0)   nn_obcdta   = ', nn_obcdta
      IF(lwp) WRITE(numout,*) '      climatology (true) or not                      ln_obc_clim = ', ln_obc_clim
      IF(lwp) WRITE(numout,*) '      vol_cst (true) or not:                         ln_vol_cst  = ', ln_vol_cst
      IF(lwp) WRITE(numout,*) ' '
      IF(lwp) WRITE(numout,*) '   WARNING                                                  '
      IF(lwp) WRITE(numout,*) '      Flather"s algorithm is applied with explicit free surface scheme                 '
      IF(lwp) WRITE(numout,*) '      or with free surface time-splitting scheme                                       '
      IF(lwp) WRITE(numout,*) '      Nor radiation neither relaxation is allowed with explicit free surface scheme:   '
      IF(lwp) WRITE(numout,*) '      Radiation and/or relaxation is allowed with free surface time-splitting scheme '
      IF(lwp) WRITE(numout,*) '      depending of the choice of rdpXin = rdpXob  = 0. for open boundaries             '
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '      For the filtered free surface case,                                              '
      IF(lwp) WRITE(numout,*) '      radiation, relaxation or presciption of data can be applied                      '
      IF(lwp) WRITE(numout,*)

      IF( lwp.AND.lp_obc_east ) THEN
         WRITE(numout,*) '      East open boundary :'
         WRITE(numout,*) '         i index                    jpieob   = ', jpieob
         WRITE(numout,*) '         damping time scale (days)  rn_dpeob = ', rn_dpeob
         WRITE(numout,*) '         damping time scale (days)  rn_dpein = ', rn_dpein
      ENDIF

      IF( lwp.AND.lp_obc_west ) THEN
         WRITE(numout,*) '      West open boundary :'
         WRITE(numout,*) '         i index                    jpiwob   = ', jpiwob
         WRITE(numout,*) '         damping time scale (days)  rn_dpwob = ', rn_dpwob
         WRITE(numout,*) '         damping time scale (days)  rn_dpwin = ', rn_dpwin
      ENDIF

      IF( lwp.AND.lp_obc_north ) THEN
         WRITE(numout,*) '      North open boundary :'
         WRITE(numout,*) '         j index                    jpjnob   = ', jpjnob
         WRITE(numout,*) '         damping time scale (days)  rn_dpnob = ', rn_dpnob
         WRITE(numout,*) '         damping time scale (days)  rn_dpnin = ', rn_dpnin
      ENDIF

      IF( lwp.AND.lp_obc_south ) THEN
         WRITE(numout,*) '      South open boundary :'
         WRITE(numout,*) '         j index                    jpjsob   = ', jpjsob
         WRITE(numout,*) '         damping time scale (days)  rn_dpsob = ', rn_dpsob
         WRITE(numout,*) '         damping time scale (days)  rn_dpsin = ', rn_dpsin
         WRITE(numout,*)
      ENDIF

!      IF( nbobc >= 2 .AND. jperio /= 0 )   &
!         &   CALL ctl_stop( ' Cyclic or symmetric, and open boundary condition are not compatible' )

      ! 1. Initialisation of constants 
      ! ------------------------------
      ! ...                          convert rdp$ob in seconds
      ! Fixed Bdy flag              inbound                outbound
      lfbceast  = .FALSE.   ;   rdpein = rdpein * rday    ;   rdpeob = rdpeob * rday
      lfbcwest  = .FALSE.   ;   rdpwin = rdpwin * rday    ;   rdpwob = rdpwob * rday
      lfbcnorth = .FALSE.   ;   rdpnin = rdpnin * rday    ;   rdpnob = rdpnob * rday
      lfbcsouth = .FALSE.   ;   rdpsin = rdpsin * rday    ;   rdpsob = rdpsob * rday
      inumfbc = 0
      ! ... look for Fixed Boundaries (rdp = 0 )
      ! ... When specified, lbcxxx flags are set to TRUE and rdpxxx are set to
      ! ...  a small arbitrary value, (to avoid division by zero further on). 
      ! ...  rdpxxx is not used anymore.
      IF( lp_obc_east )  THEN
         IF( (rdpein+rdpeob) == 0 )  THEN
            lfbceast = .TRUE.   ;   rdpein = 1e-3   ;   rdpeob = 1e-3
            inumfbc = inumfbc+1
         ELSEIF ( (rdpein*rdpeob) == 0 )  THEN
            CALL ctl_stop( 'obc_init : rn_dpein & rn_dpeob must be both zero or non zero' )
         END IF
      END IF

      IF( lp_obc_west )  THEN
         IF( (rdpwin + rdpwob) == 0 )  THEN
            lfbcwest = .TRUE.     ;     rdpwin = 1e-3     ;     rdpwob = 1e-3
            inumfbc = inumfbc+1
         ELSEIF ( (rdpwin*rdpwob) == 0 )  THEN
            CALL ctl_stop( 'obc_init : rn_dpwin & rn_dpwob must be both zero or non zero' )
         END IF
      END IF
      IF( lp_obc_north )  THEN
         IF( (rdpnin + rdpnob) == 0 )  THEN
            lfbcnorth = .TRUE.     ;     rdpnin = 1e-3     ;     rdpnob = 1e-3
            inumfbc = inumfbc+1
         ELSEIF ( (rdpnin*rdpnob) == 0 )  THEN
            CALL ctl_stop( 'obc_init : rn_dpnin & rn_dpnob must be both zero or non zero' )
         END IF
      END IF
      IF( lp_obc_south )  THEN
         IF( (rdpsin + rdpsob) == 0 )  THEN
            lfbcsouth = .TRUE.   ;   rdpsin = 1e-3   ;   rdpsob = 1e-3
            inumfbc = inumfbc+1
         ELSEIF ( (rdpsin*rdpsob) == 0 )  THEN
            CALL ctl_stop( 'obc_init : rn_dpsin & rn_dpsob must be both zero or non zero' )
         END IF
      END IF

      ! 2.  Clever mpp indices for loops on the open boundaries. 
      !     The loops will be performed only on the processors 
      !     that contain a given open boundary.
      ! --------------------------------------------------------

      IF( lp_obc_east ) THEN
         ! ...   mpp initialization
         nie0   = max( 1, min(jpieob   - nimpp+1, jpi     ) )
         nie1   = max( 0, min(jpieob   - nimpp+1, jpi - 1 ) )
         nie0p1 = max( 1, min(jpieob+1 - nimpp+1, jpi     ) )
         nie1p1 = max( 0, min(jpieob+1 - nimpp+1, jpi - 1 ) )
         nie0m1 = max( 1, min(jpieob-1 - nimpp+1, jpi     ) )
         nie1m1 = max( 0, min(jpieob-1 - nimpp+1, jpi - 1 ) )
         nje0   = max( 2, min(jpjed    - njmpp+1, jpj     ) )
         nje1   = max( 0, min(jpjef    - njmpp+1, jpj - 1 ) )
         nje0p1 = max( 1, min(jpjedp1  - njmpp+1, jpj     ) )
         nje0m1 = max( 1, min(jpjed    - njmpp+1, jpj     ) )
         nje1m1 = max( 0, min(jpjefm1  - njmpp+1, jpj - 1 ) )
         nje1m2 = max( 0, min(jpjefm1-1- njmpp+1, jpj - 1 ) )
         IF(lwp) THEN
            IF( lfbceast ) THEN
               WRITE(numout,*)'     '
               WRITE(numout,*)'         Specified East Open Boundary'
            ELSE
               WRITE(numout,*)'     '
               WRITE(numout,*)'         Radiative East Open Boundary'
            END IF
         END IF
      END IF

      IF( lp_obc_west ) THEN
         ! ...   mpp initialization
         niw0   = max( 1, min(jpiwob   - nimpp+1, jpi     ) )
         niw1   = max( 0, min(jpiwob   - nimpp+1, jpi - 1 ) )
         niw0p1 = max( 1, min(jpiwob+1 - nimpp+1, jpi     ) )
         niw1p1 = max( 0, min(jpiwob+1 - nimpp+1, jpi - 1 ) )
         njw0   = max( 2, min(jpjwd    - njmpp+1, jpj     ) )
         njw1   = max( 0, min(jpjwf    - njmpp+1, jpj - 1 ) )
         njw0p1 = max( 1, min(jpjwdp1  - njmpp+1, jpj     ) )
         njw0m1 = max( 1, min(jpjwd    - njmpp+1, jpj     ) )
         njw1m1 = max( 0, min(jpjwfm1  - njmpp+1, jpj - 1 ) )
         njw1m2 = max( 0, min(jpjwfm1-1- njmpp+1, jpj - 1 ) )
         IF(lwp) THEN
            IF( lfbcwest ) THEN
               WRITE(numout,*)'     '
               WRITE(numout,*)'         Specified West Open Boundary'
            ELSE
               WRITE(numout,*)'     '
               WRITE(numout,*)'         Radiative West Open Boundary'
            END IF
         END IF
      END IF
 
      IF( lp_obc_north ) THEN
         ! ...   mpp initialization
         nin0   = max( 2, min(jpind    - nimpp+1, jpi     ) )
         nin1   = max( 0, min(jpinf    - nimpp+1, jpi - 1 ) )
         nin0p1 = max( 1, min(jpindp1  - nimpp+1, jpi     ) )
         nin0m1 = max( 1, min(jpind    - nimpp+1, jpi     ) )
         nin1m1 = max( 0, min(jpinfm1  - nimpp+1, jpi - 1 ) )
         nin1m2 = max( 0, min(jpinfm1-1- nimpp+1, jpi - 1 ) )
         njn0   = max( 1, min(jpjnob   - njmpp+1, jpj     ) )
         njn1   = max( 0, min(jpjnob   - njmpp+1, jpj - 1 ) )
         njn0p1 = max( 1, min(jpjnob+1 - njmpp+1, jpj     ) )
         njn1p1 = max( 0, min(jpjnob+1 - njmpp+1, jpj - 1 ) )
         njn0m1 = max( 1, min(jpjnob-1 - njmpp+1, jpj     ) )
         njn1m1 = max( 0, min(jpjnob-1 - njmpp+1, jpj - 1 ) )
         IF(lwp) THEN
            IF( lfbcnorth ) THEN
               WRITE(numout,*)'     '
               WRITE(numout,*)'         Specified North Open Boundary'
            ELSE
               WRITE(numout,*)'     '
               WRITE(numout,*)'         Radiative North Open Boundary'
            END IF
         END IF
      END IF

      IF( lp_obc_south ) THEN
         ! ...   mpp initialization
         nis0   = max( 2, min(jpisd    - nimpp+1, jpi     ) )
         nis1   = max( 0, min(jpisf    - nimpp+1, jpi - 1 ) )
         nis0p1 = max( 1, min(jpisdp1  - nimpp+1, jpi     ) )
         nis0m1 = max( 1, min(jpisd    - nimpp+1, jpi     ) )
         nis1m1 = max( 0, min(jpisfm1  - nimpp+1, jpi - 1 ) )
         nis1m2 = max( 0, min(jpisfm1-1- nimpp+1, jpi - 1 ) )
         njs0   = max( 1, min(jpjsob   - njmpp+1, jpj     ) )
         njs1   = max( 0, min(jpjsob   - njmpp+1, jpj - 1 ) )
         njs0p1 = max( 1, min(jpjsob+1 - njmpp+1, jpj     ) )
         njs1p1 = max( 0, min(jpjsob+1 - njmpp+1, jpj - 1 ) )
         IF(lwp) THEN
            IF( lfbcsouth ) THEN
               WRITE(numout,*)'     '
               WRITE(numout,*)'         Specified South Open Boundary'
            ELSE
               WRITE(numout,*)'     '
               WRITE(numout,*)'         Radiative South Open Boundary'
            END IF
         END IF
      END IF

      ! 3. mask correction for OBCs
      ! ---------------------------

      IF( lp_obc_east ) THEN
         !... (jpjed,jpjefm1),jpieob
         bmask(nie0p1:nie1p1,nje0:nje1m1) = 0.e0

         ! ... initilization to zero
         uemsk(:,:) = 0.e0   ;   vemsk(:,:) = 0.e0   ;   temsk(:,:) = 0.e0

         ! ... set 2D mask on East OBC,  Vopt
         DO ji = fs_nie0, fs_nie1
            DO jj = nje0, nje1
               uemsk(jj,:) = umask(ji,  jj,:) * tmask_i(ji,jj)   * tmask_i(ji+1,jj)
               vemsk(jj,:) = vmask(ji+1,jj,:) * tmask_i(ji+1,jj) 
               temsk(jj,:) = tmask(ji+1,jj,:) * tmask_i(ji+1,jj) 
            END DO
         END DO

      END IF

      IF( lp_obc_west ) THEN
         ! ... (jpjwd,jpjwfm1),jpiwob
         bmask(niw0:niw1,njw0:njw1m1) = 0.e0

         ! ... initilization to zero
         uwmsk(:,:) = 0.e0   ;   vwmsk(:,:) = 0.e0   ;   twmsk(:,:) = 0.e0  

         ! ... set 2D mask on West OBC,  Vopt
         DO ji = fs_niw0, fs_niw1
            DO jj = njw0, njw1
               uwmsk(jj,:) = umask(ji,jj,:) * tmask_i(ji,jj)   * tmask_i(ji+1,jj)
               vwmsk(jj,:) = vmask(ji,jj,:) * tmask_i(ji,jj)  
               twmsk(jj,:) = tmask(ji,jj,:) * tmask_i(ji,jj)
            END DO
         END DO

      END IF

      IF( lp_obc_north ) THEN
         ! ... jpjnob,(jpind,jpisfm1)
         bmask(nin0:nin1m1,njn0p1:njn1p1) = 0.e0

         ! ... initilization to zero
         unmsk(:,:) = 0.e0   ;   vnmsk(:,:) = 0.e0   ;   tnmsk(:,:) = 0.e0

         ! ... set 2D mask on North OBC,  Vopt
         DO jj = fs_njn0, fs_njn1
            DO ji = nin0, nin1
               unmsk(ji,:) = umask(ji,jj+1,:) * tmask_i(ji,jj+1) 
               vnmsk(ji,:) = vmask(ji,jj  ,:) * tmask_i(ji,jj)   * tmask_i(ji,jj+1)
               tnmsk(ji,:) = tmask(ji,jj+1,:) * tmask_i(ji,jj+1)
            END DO
         END DO

      END IF

      IF( lp_obc_south ) THEN 
         ! ... jpjsob,(jpisd,jpisfm1)
         bmask(nis0:nis1m1,njs0:njs1) = 0.e0

         ! ... initilization to zero
         usmsk(:,:) = 0.e0   ;   vsmsk(:,:) = 0.e0   ;   tsmsk(:,:) = 0.e0

         ! ... set 2D mask on South OBC,  Vopt
         DO jj = fs_njs0, fs_njs1 
            DO ji = nis0, nis1
               usmsk(ji,:) = umask(ji,jj,:) * tmask_i(ji,jj) 
               vsmsk(ji,:) = vmask(ji,jj,:) * tmask_i(ji,jj) * tmask_i(ji,jj+1)
               tsmsk(ji,:) = tmask(ji,jj,:) * tmask_i(ji,jj)
            END DO
         END DO

      END IF

      ! ... Initialize obcumask and obcvmask for the Force filtering 
      !     boundary condition in dynspg_flt
      obcumask(:,:) = umask(:,:,1)
      obcvmask(:,:) = vmask(:,:,1)

      ! ... Initialize obctmsk on overlap region and obcs. This mask
      !     is used in obcvol.F90 to calculate cumulate flux E-P. 
      !     obc Tracer point are outside the domain ( U/V obc points) ==> masked by obctmsk
      !     - no flux E-P on obcs and overlap region (jpreci = jprecj = 1)
      obctmsk(:,:) = tmask_i(:,:)     

      IF( lp_obc_east ) THEN
         ! ... East obc Force filtering mask for the grad D
         obcumask(nie0  :nie1  ,nje0p1:nje1m1) = 0.e0
         obcvmask(nie0p1:nie1p1,nje0p1:nje1m1) = 0.e0
         ! ... set to 0 on East OBC
         obctmsk(nie0p1:nie1p1,nje0:nje1) = 0.e0
      END IF

      IF( lp_obc_west ) THEN
         ! ... West obc Force filtering mask for the grad D
         obcumask(niw0:niw1,njw0:njw1) = 0.e0
         obcvmask(niw0:niw1,njw0:njw1) = 0.e0
         ! ... set to 0 on West OBC
         obctmsk(niw0:niw1,njw0:njw1) = 0.e0
      END IF

      IF( lp_obc_north ) THEN
         ! ... North obc Force filtering mask for the grad D
         obcumask(nin0p1:nin1m1,njn0p1:njn1p1) = 0.e0
         obcvmask(nin0p1:nin1m1,njn0  :njn1  ) = 0.e0
         ! ... set to 0 on North OBC
         obctmsk(nin0:nin1,njn0p1:njn1p1) = 0.e0
      END IF

      IF( lp_obc_south ) THEN
         ! ... South obc Force filtering mask for the grad D
         obcumask(nis0p1:nis1m1,njs0:njs1) = 0.e0
         obcvmask(nis0p1:nis1m1,njs0:njs1) = 0.e0
         ! ... set to 0 on South OBC
         obctmsk(nis0:nis1,njs0:njs1) = 0.e0
      END IF

      ! 3.1 Total lateral surface 
      ! -------------------------
      obcsurfeast  = 0.e0       ;       obcsurfwest  = 0.e0   
      obcsurfnorth = 0.e0       ;       obcsurfsouth = 0.e0   
      obcsurftot = 0.e0

      IF( lp_obc_east .AND. lp_obc_east_barotp_corr ) THEN ! ... East open boundary lateral surface
         DO ji = nie0, nie1
            DO jj = 1, jpj 
               obcsurfeast = obcsurfeast+hu(ji,jj)*e2u(ji,jj)*uemsk(jj,1) * MAX(obctmsk(ji,jj),obctmsk(ji+1,jj) )
            END DO
         END DO
         obcsurftot = obcsurftot + obcsurfeast
      END IF

      IF( lp_obc_west .AND. lp_obc_west_barotp_corr ) THEN ! ... West open boundary lateral surface
         DO ji = niw0, niw1
            DO jj = 1, jpj 
               obcsurfwest = obcsurfwest+hu(ji,jj)*e2u(ji,jj)*uwmsk(jj,1) * MAX(obctmsk(ji,jj),obctmsk(ji+1,jj) )
            END DO
         END DO
         obcsurftot = obcsurftot + obcsurfwest
      END IF

      IF( lp_obc_north .AND. lp_obc_north_barotp_corr ) THEN ! ... North open boundary lateral surface
         DO jj = njn0, njn1
            DO ji = 1, jpi
               obcsurfnorth = obcsurfnorth+hv(ji,jj)*e1v(ji,jj)*vnmsk(ji,1) * MAX(obctmsk(ji,jj),obctmsk(ji,jj+1) )
            END DO
         END DO
         obcsurftot = obcsurftot + obcsurfnorth
      END IF

      IF( lp_obc_south .AND. lp_obc_south_barotp_corr ) THEN ! ... South open boundary lateral surface
         DO jj = njs0, njs1
            DO ji = 1, jpi
               obcsurfsouth = obcsurfsouth+hv(ji,jj)*e1v(ji,jj)*vsmsk(ji,1) * MAX(obctmsk(ji,jj),obctmsk(ji,jj+1) )
            END DO
         END DO
         obcsurftot = obcsurftot + obcsurfsouth
      END IF

      IF( lk_mpp )   CALL mpp_sum( obcsurftot )   ! sum over the global domain

      ! 5. Control print on mask 
      !    The extremities of the open boundaries must be in land
      !    or else correspond to an "ocean corner" between two open boundaries. 
      !    corner 1 is southwest, 2 is south east, 3 is northeast, 4 is northwest. 
      ! --------------------------------------------------------------------------

      icorner(:)=0

      ! ... control of the west boundary
      IF( lp_obc_west ) THEN
         IF( jpiwob < 2 .OR.  jpiwob >= jpiglo-2 ) THEN
            WRITE(ctmp1,*) ' jpiwob exceed ', jpiglo-2, 'or less than 2'
            CALL ctl_stop( ctmp1 )
         END IF
         ztestmask(:)=0.
         DO ji=niw0,niw1
            IF( (njw0 + njmpp - 1) == jpjwd ) ztestmask(1)=ztestmask(1)+ tmask(ji,njw0,1)
            IF( (njw1 + njmpp - 1) == jpjwf ) ztestmask(2)=ztestmask(2)+ tmask(ji,njw1,1)
         END DO
         IF( lk_mpp )   CALL mpp_sum( ztestmask, 2 )   ! sum over the global domain

         IF( ztestmask(1) /= 0. ) icorner(1)=icorner(1)+1
         IF( ztestmask(2) /= 0. ) icorner(4)=icorner(4)+1
      END IF

      ! ... control of the east boundary
      IF( lp_obc_east ) THEN
         IF( jpieob < 4 .OR.  jpieob >= jpiglo ) THEN
            WRITE(ctmp1,*) ' jpieob exceed ', jpiglo, ' or less than 4'
            CALL ctl_stop( ctmp1 )
         END IF
         ztestmask(:)=0.
         DO ji=nie0p1,nie1p1
            IF( (nje0 + njmpp - 1) == jpjed ) ztestmask(1)=ztestmask(1)+ tmask(ji,nje0,1)
            IF( (nje1 + njmpp - 1) == jpjef ) ztestmask(2)=ztestmask(2)+ tmask(ji,nje1,1)
         END DO
         IF( lk_mpp )   CALL mpp_sum( ztestmask, 2 )   ! sum over the global domain

        IF( ztestmask(1) /= 0. ) icorner(2)=icorner(2)+1
        IF( ztestmask(2) /= 0. ) icorner(3)=icorner(3)+1
      END IF

      ! ... control of the north boundary
      IF( lp_obc_north ) THEN
         IF( jpjnob < 4 .OR.  jpjnob >= jpjglo ) THEN
            WRITE(ctmp1,*) 'jpjnob exceed ', jpjglo, ' or less than 4'
            CALL ctl_stop( ctmp1 )
         END IF
         ztestmask(:)=0.
         DO jj=njn0p1,njn1p1
            IF( (nin0 + nimpp - 1) == jpind ) ztestmask(1)=ztestmask(1)+ tmask(nin0,jj,1)
            IF( (nin1 + nimpp - 1) == jpinf ) ztestmask(2)=ztestmask(2)+ tmask(nin1,jj,1)
         END DO
         IF( lk_mpp )   CALL mpp_sum( ztestmask, 2 )   ! sum over the global domain

         IF( ztestmask(1) /= 0. ) icorner(4)=icorner(4)+1
         IF( ztestmask(2) /= 0. ) icorner(3)=icorner(3)+1
      END IF

      ! ... control of the south boundary
      IF( lp_obc_south ) THEN
         IF( jpjsob < 2 .OR.  jpjsob >= jpjglo-2 ) THEN
            WRITE(ctmp1,*) ' jpjsob exceed ', jpjglo-2, ' or less than 2'
            CALL ctl_stop( ctmp1 )
         END IF
         ztestmask(:)=0.
         DO jj=njs0,njs1
            IF( (nis0 + nimpp - 1) == jpisd ) ztestmask(1)=ztestmask(1)+ tmask(nis0,jj,1)
            IF( (nis1 + nimpp - 1) == jpisf ) ztestmask(2)=ztestmask(2)+ tmask(nis1,jj,1)
         END DO
         IF( lk_mpp )   CALL mpp_sum( ztestmask, 2 )   ! sum over the global domain

         IF( ztestmask(1) /= 0. ) icorner(1)=icorner(1)+1
         IF( ztestmask(2) /= 0. ) icorner(2)=icorner(2)+1
      END IF

!!$      IF( icorner(1) == 2 ) THEN
!!$         IF(lwp) WRITE(numout,*)
!!$         IF(lwp) WRITE(numout,*) ' South West ocean corner, two open boudaries'
!!$         IF(lwp) WRITE(numout,*) ' ========== '
!!$         IF(lwp) WRITE(numout,*)
!!$         IF( jpisd /= jpiwob.OR.jpjsob /= jpjwd ) &
!!$              &   CALL ctl_stop( ' Open boundaries do not fit, we stop' )
!!$
!!$      ELSE IF( icorner(1) == 1 ) THEN
!!$         CALL ctl_stop( ' Open boundaries do not fit at SW corner, we stop' )
!!$      END IF 
!!$
!!$      IF( icorner(2) == 2 ) THEN
!!$          IF(lwp) WRITE(numout,*)
!!$          IF(lwp) WRITE(numout,*) ' South East ocean corner, two open boudaries'
!!$          IF(lwp) WRITE(numout,*) ' ========== '
!!$          IF(lwp) WRITE(numout,*)
!!$          IF( jpisf /= jpieob+1.OR.jpjsob /= jpjed ) &
!!$               &   CALL ctl_stop( ' Open boundaries do not fit, we stop' )
!!$      ELSE IF( icorner(2) == 1 ) THEN
!!$         CALL ctl_stop( ' Open boundaries do not fit at SE corner, we stop' )
!!$      END IF 
!!$
!!$      IF( icorner(3) == 2 ) THEN
!!$         IF(lwp) WRITE(numout,*)
!!$         IF(lwp) WRITE(numout,*) ' North East ocean corner, two open boudaries'
!!$         IF(lwp) WRITE(numout,*) ' ========== '
!!$         IF(lwp) WRITE(numout,*)
!!$         IF( jpinf /= jpieob+1 .OR. jpjnob+1 /= jpjef ) &
!!$              &   CALL ctl_stop( ' Open boundaries do not fit, we stop' )
!!$       ELSE IF( icorner(3) == 1 ) THEN
!!$          CALL ctl_stop( ' Open boundaries do not fit at NE corner, we stop' )
!!$       END IF 
!!$
!!$      IF( icorner(4) == 2 ) THEN
!!$         IF(lwp) WRITE(numout,*)
!!$         IF(lwp) WRITE(numout,*) ' North West ocean corner, two open boudaries'
!!$         IF(lwp) WRITE(numout,*) ' ========== '
!!$         IF(lwp) WRITE(numout,*)
!!$         IF( jpind /= jpiwob.OR.jpjnob+1 /= jpjwf ) &
!!$              &   CALL ctl_stop( ' Open boundaries do not fit, we stop' )
!!$       ELSE IF( icorner(4) == 1 ) THEN
!!$          CALL ctl_stop( ' Open boundaries do not fit at NW corner, we stop' )
!!$       END IF 

      ! 6. Initialization of open boundary variables (u, v, t, s)
      ! --------------------------------------------------------------
      !   only if at least one boundary is  radiative 
! LITTLE_NEMO      IF ( inumfbc < nbobc .AND.  ln_rstart ) THEN
! LITTLE_NEMO         !  Restart from restart.obc
! LITTLE_NEMO         CALL obc_rst_read

! LITTLE_NEMO
       IF(lwp) WRITE(numout,*)
       IF(lwp) WRITE(numout,*) ' obcini : '
       IF(lwp) WRITE(numout,*) '   ln_rstart     : ', ln_rstart
       IF(lwp) WRITE(numout,*) '   ln_obc_rstart : ', ln_obc_rstart
       
       IF ( .NOT. ln_rstart .AND. ln_obc_rstart ) THEN
          IF(lwp) WRITE(numout,*) ' obcini : Warning!! ln_rstart = .F. => we force ln_obc_rstart =.F. '
          ln_obc_rstart = .FALSE.
       ENDIF 

       IF ( ln_rstart .AND. ln_obc_rstart ) THEN
          IF  ( inumfbc < nbobc ) THEN
             IF(lwp) WRITE(numout,*) '   => We read the OBC restart file '
             CALL obc_rst_read
          ELSE
             IF(lwp) WRITE(numout,*) '   => We DO NOT read the OBC restart file (since all OBCs are fixed) '
          ENDIF         
! LITTLE_NEMO          
       ELSE

!         ! ... Initialization to zero of radiation arrays.
!         !     Those have dimensions of local subdomains

          uebnd(:,:,:,:) = 0.e0   ;   unbnd(:,:,:,:) = 0.e0
          vebnd(:,:,:,:) = 0.e0   ;   vnbnd(:,:,:,:) = 0.e0
          tebnd(:,:,:,:) = 0.e0   ;   tnbnd(:,:,:,:) = 0.e0
          sebnd(:,:,:,:) = 0.e0   ;   snbnd(:,:,:,:) = 0.e0

          uwbnd(:,:,:,:) = 0.e0   ;   usbnd(:,:,:,:) = 0.e0
          vwbnd(:,:,:,:) = 0.e0   ;   vsbnd(:,:,:,:) = 0.e0
          twbnd(:,:,:,:) = 0.e0   ;   tsbnd(:,:,:,:) = 0.e0
          swbnd(:,:,:,:) = 0.e0   ;   ssbnd(:,:,:,:) = 0.e0

       END IF

      ! 7. Control print
      ! -----------------------------------------------------------------

      ! ... control of the east boundary
      IF( lp_obc_east ) THEN
         istop = 0
         IF( jpieob < 4 .OR.  jpieob >= jpiglo ) THEN
            IF(lwp) WRITE(numout,cform_err)
            IF(lwp) WRITE(numout,*) '            jpieob exceed ', jpim1, ' or less than 4'
            istop = istop + 1
         END IF

         IF( lk_mpp ) THEN
            ! ... 
            IF( nimpp > jpieob-5) THEN
               IF(lwp) WRITE(numout,cform_err)
               IF(lwp) WRITE(numout,*) '        A sub-domain is too close to the East OBC'
               IF(lwp) WRITE(numout,*) '        nimpp must be < jpieob-5'
               istop = istop + 1
            ENDIF
         ELSE

            ! ... stop if  e r r o r (s)   detected
            IF( istop /= 0 ) THEN
               WRITE(ctmp1,*) istop,' obcini : E R R O R (S) detected : stop'
               CALL ctl_stop( ctmp1 )
            ENDIF
         ENDIF
      ENDIF

      ! ... control of the west boundary
      IF( lp_obc_west ) THEN
         istop = 0
         IF( jpiwob < 2 .OR.  jpiwob >= jpiglo ) THEN
            IF(lwp) WRITE(numout,cform_err)
            IF(lwp) WRITE(numout,*) '            jpiwob exceed ', jpim1, ' or less than 2'
            istop = istop + 1
         END IF

         IF( lk_mpp ) THEN
            IF( (nimpp < jpiwob+5) .AND. (nimpp > 1) ) THEN
               IF(lwp) WRITE(numout,cform_err)
               IF(lwp) WRITE(numout,*) '        A sub-domain is too close to the West OBC'
               IF(lwp) WRITE(numout,*) '        nimpp must be > jpiwob-5 or =1'
               istop = istop + 1
            ENDIF
         ELSE
   
            ! ... stop if  e r r o r (s)   detected
            IF( istop /= 0 ) THEN
               WRITE(ctmp1,*) istop,' obcini : E R R O R (S) detected : stop'
               CALL ctl_stop( ctmp1 )
            ENDIF
         ENDIF
      ENDIF

      ! control of the north boundary
      IF( lp_obc_north ) THEN
         istop = 0
         IF( jpjnob < 4 .OR.  jpjnob >= jpjglo ) THEN
            IF(lwp) WRITE(numout,cform_err)
            IF(lwp) WRITE(numout,*) '          jpjnob exceed ', jpjm1,' or less than 4'
            istop = istop + 1
         END IF

         IF( lk_mpp ) THEN
            IF( njmpp > jpjnob-5) THEN
               IF(lwp) WRITE(numout,cform_err)
               IF(lwp) WRITE(numout,*) '        A sub-domain is too close to the North OBC'
               IF(lwp) WRITE(numout,*) '        njmpp must be < jpjnob-5'
               istop = istop + 1
            ENDIF
         ELSE
   
            ! ... stop if  e r r o r (s)   detected
            IF( istop /= 0 ) THEN
                WRITE(ctmp1,*) istop,' obcini : E R R O R (S) detected : stop'
               CALL ctl_stop( ctmp1 )
           ENDIF
         ENDIF
      ENDIF

      ! control of the south boundary
      IF( lp_obc_south ) THEN
         istop = 0
         IF( jpjsob < 2 .OR. jpjsob >= jpjglo ) THEN
            IF(lwp) WRITE(numout,cform_err)
            IF(lwp) WRITE(numout,*) '          jpjsob exceed ', jpjm1,' or less than 2'
            istop = istop + 1
         END IF

         IF( lk_mpp ) THEN
            IF( (njmpp < jpjsob+5) .AND. (njmpp > 1) ) THEN
               IF(lwp) WRITE(numout,cform_err)
               IF(lwp) WRITE(numout,*) '        A sub-domain is too close to the South OBC'
               IF(lwp) WRITE(numout,*) '        njmpp must be > jpjsob+5 or =1'
               istop = istop + 1
            ENDIF
         ELSE
   
            ! ... stop if  e r r o r (s)   detected
            IF( istop /= 0 ) THEN
               WRITE(ctmp1,*) istop,' obcini : E R R O R (S) detected : stop'
               CALL ctl_stop( ctmp1 )
            ENDIF
         ENDIF
      ENDIF

   END SUBROUTINE obc_init

#else
   !!---------------------------------------------------------------------------------
   !!   Dummy module                                                NO open boundaries
   !!---------------------------------------------------------------------------------
CONTAINS
   SUBROUTINE obc_init      ! Dummy routine
   END SUBROUTINE obc_init
#endif

   !!=================================================================================
END MODULE obcini
