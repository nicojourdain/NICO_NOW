MODULE tradmp
   !!======================================================================
   !!                       ***  MODULE  tradmp  ***
   !! Ocean physics: internal restoring trend on active tracers (T and S)
   !!======================================================================
   !! History :  OPA  ! 1991-03  (O. Marti, G. Madec)  Original code
   !!                 ! 1992-06  (M. Imbard)  doctor norme
   !!                 ! 1996-01  (G. Madec)  statement function for e3
   !!                 ! 1997-05  (G. Madec)  macro-tasked on jk-slab
   !!                 ! 1998-07  (M. Imbard, G. Madec) ORCA version
   !!            7.0  ! 2001-02  (M. Imbard)  cofdis, Original code
   !!            8.1  ! 2001-02  (G. Madec, E. Durand)  cleaning
   !!  NEMO      1.0  ! 2002-08  (G. Madec, E. Durand)  free form + modules
   !!            3.2  ! 2009-08  (G. Madec, C. Talandier)  DOCTOR norm for namelist parameter
   !!            3.3  ! 2010-06  (C. Ethe, G. Madec) merge TRA-TRC 
   !!            3.4  ! 2011-04  (G. Madec, C. Ethe) Merge of dtatem and dtasal + suppression of CPP keys
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_dmp_alloc : allocate tradmp arrays
   !!   tra_dmp       : update the tracer trend with the internal damping
   !!   tra_dmp_init  : initialization, namlist read, parameters control
   !!   dtacof_zoom   : restoring coefficient for zoom domain
   !!   dtacof        : restoring coefficient for global domain
   !!   cofdis        : compute the distance to the coastline
   !!----------------------------------------------------------------------
   USE oce            ! ocean: variables
   USE dom_oce        ! ocean: domain variables
   USE trdmod_oce     ! ocean: trend variables
   USE trdtra         ! active tracers: trends
   USE zdf_oce        ! ocean: vertical physics
   USE phycst         ! physical constants
   USE dtatsd         ! data: temperature & salinity
   USE zdfmxl         ! vertical physics: mixed layer depth
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE prtctl         ! Print control
   USE wrk_nemo       ! Memory allocation
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_dmp      ! routine called by step.F90
   PUBLIC   tra_dmp_init ! routine called by opa.F90
   PUBLIC   dtacof       ! routine called by in both tradmp.F90 and trcdmp.F90
   PUBLIC   dtacof_zoom  ! routine called by in both tradmp.F90 and trcdmp.F90

   !                                !!* Namelist namtra_dmp : T & S newtonian damping *
   LOGICAL, PUBLIC ::   ln_tradmp = .TRUE.    !: internal damping flag
   INTEGER         ::   nn_hdmp   =   -1      ! = 0/-1/'latitude' for damping over T and S
   INTEGER         ::   nn_zdmp   =    0      ! = 0/1/2 flag for damping in the mixed layer
   REAL(wp)        ::   rn_surf   =   50._wp  ! surface time scale for internal damping        [days]
   REAL(wp)        ::   rn_bot    =  360._wp  ! bottom time scale for internal damping         [days]
   REAL(wp)        ::   rn_dep    =  800._wp  ! depth of transition between rn_surf and rn_bot [meters]
   INTEGER         ::   nn_file   =    2      ! = 1 create a damping.coeff NetCDF file 

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   strdmp   !: damping salinity trend (psu/s)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ttrdmp   !: damping temperature trend (Celcius/s)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   resto    !: restoring coeff. on T and S (s-1)

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: tradmp.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION tra_dmp_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION tra_dmp_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( strdmp(jpi,jpj,jpk) , ttrdmp(jpi,jpj,jpk), resto(jpi,jpj,jpk), STAT= tra_dmp_alloc )
      !
      IF( lk_mpp            )   CALL mpp_sum ( tra_dmp_alloc )
      IF( tra_dmp_alloc > 0 )   CALL ctl_warn('tra_dmp_alloc: allocation of arrays failed')
      !
   END FUNCTION tra_dmp_alloc


   SUBROUTINE tra_dmp( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE tra_dmp  ***
      !!                  
      !! ** Purpose :   Compute the tracer trend due to a newtonian damping
      !!      of the tracer field towards given data field and add it to the
      !!      general tracer trends.
      !!
      !! ** Method  :   Newtonian damping towards t_dta and s_dta computed 
      !!      and add to the general tracer trends:
      !!                     ta = ta + resto * (t_dta - tb)
      !!                     sa = sa + resto * (s_dta - sb)
      !!         The trend is computed either throughout the water column
      !!      (nlmdmp=0) or in area of weak vertical mixing (nlmdmp=1) or
      !!      below the well mixed layer (nlmdmp=2)
      !!
      !! ** Action  : - (ta,sa)   tracer trends updated with the damping trend
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT(in) ::   kt   ! ocean time-step index
      !!
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zta, zsa             ! local scalars
      REAL(wp), POINTER, DIMENSION(:,:,:,:) ::  zts_dta 
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start( 'tra_dmp')
      !
      CALL wrk_alloc( jpi, jpj, jpk, jpts,  zts_dta )
      !                           !==   input T-S data at kt   ==!
      CALL dta_tsd( kt, zts_dta )            ! read and interpolates T-S data at kt
      !
      SELECT CASE ( nn_zdmp )     !==    type of damping   ==!
      !
      CASE( 0 )                   !==  newtonian damping throughout the water column  ==!
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  zta = resto(ji,jj,jk) * ( zts_dta(ji,jj,jk,jp_tem) - tsb(ji,jj,jk,jp_tem) )
                  zsa = resto(ji,jj,jk) * ( zts_dta(ji,jj,jk,jp_sal) - tsb(ji,jj,jk,jp_sal) )
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) + zta
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) + zsa
                  strdmp(ji,jj,jk) = zsa           ! save the trend (used in asmtrj)
                  ttrdmp(ji,jj,jk) = zta      
               END DO
            END DO
         END DO
         !
      CASE ( 1 )                  !==  no damping in the turbocline (avt > 5 cm2/s)  ==!
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  IF( avt(ji,jj,jk) <= 5.e-4_wp ) THEN
                     zta = resto(ji,jj,jk) * ( zts_dta(ji,jj,jk,jp_tem) - tsb(ji,jj,jk,jp_tem) )
                     zsa = resto(ji,jj,jk) * ( zts_dta(ji,jj,jk,jp_sal) - tsb(ji,jj,jk,jp_sal) )
                  ELSE
                     zta = 0._wp
                     zsa = 0._wp  
                  ENDIF
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) + zta
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) + zsa
                  strdmp(ji,jj,jk) = zsa           ! save the salinity trend (used in asmtrj)
                  ttrdmp(ji,jj,jk) = zta
               END DO
            END DO
         END DO
         !
      CASE ( 2 )                  !==  no damping in the mixed layer   ==!
         DO jk = 1, jpkm1
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  IF( fsdept(ji,jj,jk) >= hmlp (ji,jj) ) THEN
                     zta = resto(ji,jj,jk) * ( zts_dta(ji,jj,jk,jp_tem) - tsb(ji,jj,jk,jp_tem) )
                     zsa = resto(ji,jj,jk) * ( zts_dta(ji,jj,jk,jp_sal) - tsb(ji,jj,jk,jp_sal) )
                  ELSE
                     zta = 0._wp
                     zsa = 0._wp  
                  ENDIF
                  tsa(ji,jj,jk,jp_tem) = tsa(ji,jj,jk,jp_tem) + zta
                  tsa(ji,jj,jk,jp_sal) = tsa(ji,jj,jk,jp_sal) + zsa
                  strdmp(ji,jj,jk) = zsa           ! save the salinity trend (used in asmtrj)
                  ttrdmp(ji,jj,jk) = zta
               END DO
            END DO
         END DO
         !
      END SELECT
      !
      IF( l_trdtra )   THEN       ! trend diagnostic
         CALL trd_tra( kt, 'TRA', jp_tem, jptra_trd_dmp, ttrdmp )
         CALL trd_tra( kt, 'TRA', jp_sal, jptra_trd_dmp, strdmp )
      ENDIF
      !                           ! Control print
      IF(ln_ctl)   CALL prt_ctl( tab3d_1=tsa(:,:,:,jp_tem), clinfo1=' dmp  - Ta: ', mask1=tmask,   &
         &                       tab3d_2=tsa(:,:,:,jp_sal), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
      !
      CALL wrk_dealloc( jpi, jpj, jpk, jpts,  zts_dta )
      !
      IF( nn_timing == 1 )  CALL timing_stop( 'tra_dmp')
      !
   END SUBROUTINE tra_dmp


   SUBROUTINE tra_dmp_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_dmp_init  ***
      !! 
      !! ** Purpose :   Initialization for the newtonian damping 
      !!
      !! ** Method  :   read the nammbf namelist and check the parameters
      !!----------------------------------------------------------------------
      NAMELIST/namtra_dmp/ ln_tradmp, nn_hdmp, nn_zdmp, rn_surf, rn_bot, rn_dep, nn_file
      !!----------------------------------------------------------------------

      REWIND ( numnam )                  ! Read Namelist namtra_dmp : temperature and salinity damping term
      READ   ( numnam, namtra_dmp )
      
      IF( lzoom )   nn_zdmp = 0          ! restoring to climatology at closed north or south boundaries

      IF(lwp) THEN                       ! Namelist print
         WRITE(numout,*)
         WRITE(numout,*) 'tra_dmp_init : T and S newtonian damping'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*) '   Namelist namtra_dmp : set damping parameter'
         WRITE(numout,*) '      add a damping termn or not      ln_tradmp = ', ln_tradmp
         WRITE(numout,*) '      T and S damping option          nn_hdmp   = ', nn_hdmp
         WRITE(numout,*) '      mixed layer damping option      nn_zdmp   = ', nn_zdmp, '(zoom: forced to 0)'
         WRITE(numout,*) '      surface time scale (days)       rn_surf   = ', rn_surf
         WRITE(numout,*) '      bottom time scale (days)        rn_bot    = ', rn_bot
         WRITE(numout,*) '      depth of transition (meters)    rn_dep    = ', rn_dep
         WRITE(numout,*) '      create a damping.coeff file     nn_file   = ', nn_file
         WRITE(numout,*)
      ENDIF

      IF( ln_tradmp ) THEN               ! initialization for T-S damping
         !
         IF( tra_dmp_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'tra_dmp_init: unable to allocate arrays' )
         !
         SELECT CASE ( nn_hdmp )
         CASE (  -1  )   ;   IF(lwp) WRITE(numout,*) '   tracer damping in the Med & Red seas only'
         CASE ( 1:90 )   ;   IF(lwp) WRITE(numout,*) '   tracer damping poleward of', nn_hdmp, ' degrees'
         CASE DEFAULT
            WRITE(ctmp1,*) '          bad flag value for nn_hdmp = ', nn_hdmp
            CALL ctl_stop(ctmp1)
         END SELECT
         !
         SELECT CASE ( nn_zdmp )
         CASE ( 0 )   ;   IF(lwp) WRITE(numout,*) '   tracer damping throughout the water column'
         CASE ( 1 )   ;   IF(lwp) WRITE(numout,*) '   no tracer damping in the turbocline (avt > 5 cm2/s)'
         CASE ( 2 )   ;   IF(lwp) WRITE(numout,*) '   no tracer damping in the mixed layer'
         CASE DEFAULT
            WRITE(ctmp1,*) 'bad flag value for nn_zdmp = ', nn_zdmp
            CALL ctl_stop(ctmp1)
         END SELECT
         !
         IF( .NOT.ln_tsd_tradmp ) THEN
            CALL ctl_warn( 'tra_dmp_init: read T-S data not initialized, we force ln_tsd_tradmp=T' )
            CALL dta_tsd_init( ld_tradmp=ln_tradmp )        ! forces the initialisation of T-S data
         ENDIF
         !
         strdmp(:,:,:) = 0._wp       ! internal damping salinity trend (used in asmtrj)
         ttrdmp(:,:,:) = 0._wp
         !                          ! Damping coefficients initialization
         IF( lzoom ) THEN   ;   CALL dtacof_zoom( resto )
         ELSE               ;   CALL dtacof( nn_hdmp, rn_surf, rn_bot, rn_dep, nn_file, 'TRA', resto )
         ENDIF
         !
      ENDIF
      !
   END SUBROUTINE tra_dmp_init


   SUBROUTINE dtacof_zoom( presto )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dtacof_zoom  ***
      !!
      !! ** Purpose :   Compute the damping coefficient for zoom domain
      !!
      !! ** Method  : - set along closed boundary due to zoom a damping over
      !!                6 points with a max time scale of 5 days.
      !!              - ORCA arctic/antarctic zoom: set the damping along
      !!                south/north boundary over a latitude strip.
      !!
      !! ** Action  : - resto, the damping coeff. for T and S
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout)  ::   presto   ! restoring coeff. (s-1)
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) ::   zlat, zlat0, zlat1, zlat2, z1_5d   ! local scalar
      REAL(wp), DIMENSION(6)  ::   zfact               ! 1Dworkspace
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start( 'dtacof_zoom')
      !

      zfact(1) =  1._wp
      zfact(2) =  1._wp
      zfact(3) = 11._wp / 12._wp
      zfact(4) =  8._wp / 12._wp
      zfact(5) =  4._wp / 12._wp
      zfact(6) =  1._wp / 12._wp
      zfact(:) = zfact(:) / ( 5._wp * rday )    ! 5 days max restoring time scale

      presto(:,:,:) = 0._wp

      ! damping along the forced closed boundary over 6 grid-points
      DO jn = 1, 6
         IF( lzoom_w )   presto( mi0(jn+jpizoom):mi1(jn+jpizoom), : , : )                    = zfact(jn)   ! west  closed
         IF( lzoom_s )   presto( : , mj0(jn+jpjzoom):mj1(jn+jpjzoom), : )                    = zfact(jn)   ! south closed 
         IF( lzoom_e )   presto( mi0(jpiglo+jpizoom-1-jn):mi1(jpiglo+jpizoom-1-jn) , : , : ) = zfact(jn)   ! east  closed 
         IF( lzoom_n )   presto( : , mj0(jpjglo+jpjzoom-1-jn):mj1(jpjglo+jpjzoom-1-jn) , : ) = zfact(jn)   ! north closed
      END DO

      !                                           ! ====================================================
      IF( lzoom_arct .AND. lzoom_anta ) THEN      !  ORCA configuration : arctic zoom or antarctic zoom
         !                                        ! ====================================================
         IF(lwp) WRITE(numout,*)
         IF(lwp .AND. lzoom_arct ) WRITE(numout,*) '              dtacof_zoom : ORCA    Arctic zoom'
         IF(lwp .AND. lzoom_arct ) WRITE(numout,*) '              dtacof_zoom : ORCA Antarctic zoom'
         IF(lwp) WRITE(numout,*)
         !
         !                          ! Initialization : 
         presto(:,:,:) = 0._wp
         zlat0 = 10._wp                     ! zlat0 : latitude strip where resto decreases
         zlat1 = 30._wp                     ! zlat1 : resto = 1 before zlat1
         zlat2 = zlat1 + zlat0              ! zlat2 : resto decreases from 1 to 0 between zlat1 and zlat2
         z1_5d = 1._wp / ( 5._wp * rday )   ! z1_5d : 1 / 5days

         DO jk = 2, jpkm1           ! Compute arrays resto ; value for internal damping : 5 days
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zlat = ABS( gphit(ji,jj) )
                  IF( zlat1 <= zlat .AND. zlat <= zlat2 ) THEN
                     presto(ji,jj,jk) = 0.5_wp * z1_5d * (  1._wp - COS( rpi*(zlat2-zlat)/zlat0 )  ) 
                  ELSEIF( zlat < zlat1 ) THEN
                     presto(ji,jj,jk) = z1_5d
                  ENDIF
               END DO
            END DO
         END DO
         !
      ENDIF
      !                             ! Mask resto array
      presto(:,:,:) = presto(:,:,:) * tmask(:,:,:)
      !
      IF( nn_timing == 1 )  CALL timing_stop( 'dtacof_zoom')
      !
   END SUBROUTINE dtacof_zoom


   SUBROUTINE dtacof( kn_hdmp, pn_surf, pn_bot, pn_dep,  &
      &               kn_file, cdtype , presto           )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dtacof  ***
      !!
      !! ** Purpose :   Compute the damping coefficient
      !!
      !! ** Method  :   Arrays defining the damping are computed for each grid
      !!                point for temperature and salinity (resto)
      !!                Damping depends on distance to coast, depth and latitude
      !!
      !! ** Action  : - resto, the damping coeff. for T and S
      !!----------------------------------------------------------------------
      USE iom
      USE ioipsl
      !!
      INTEGER                         , INTENT(in   )  ::  kn_hdmp    ! damping option
      REAL(wp)                        , INTENT(in   )  ::  pn_surf    ! surface time scale (days)
      REAL(wp)                        , INTENT(in   )  ::  pn_bot     ! bottom time scale (days)
      REAL(wp)                        , INTENT(in   )  ::  pn_dep     ! depth of transition (meters)
      INTEGER                         , INTENT(in   )  ::  kn_file    ! save the damping coef on a file or not
      CHARACTER(len=3)                , INTENT(in   )  ::  cdtype     ! =TRA or TRC (tracer indicator)
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout)  ::  presto     ! restoring coeff. (s-1)
      !
      INTEGER  ::   ji, jj, jk                  ! dummy loop indices
      INTEGER  ::   ii0, ii1, ij0, ij1          ! local integers
      INTEGER  ::   inum0, icot                 !   -       -
      REAL(wp) ::   zinfl, zlon                 ! local scalars
      REAL(wp) ::   zlat, zlat0, zlat1, zlat2   !   -      -
      REAL(wp) ::   zsdmp, zbdmp                !   -      -
      CHARACTER(len=20)                   :: cfile
      REAL(wp), POINTER, DIMENSION(:    ) :: zhfac 
      REAL(wp), POINTER, DIMENSION(:,:  ) :: zmrs 
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zdct 
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('dtacof')
      !
      CALL wrk_alloc( jpk, zhfac          )
      CALL wrk_alloc( jpi, jpj, zmrs      )
      CALL wrk_alloc( jpi, jpj, jpk, zdct )
      !                                   ! ====================
      !                                   !  ORCA configuration : global domain
      !                                   ! ====================
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '              dtacof : Global domain of ORCA'
      IF(lwp) WRITE(numout,*) '              ------------------------------'
      !
      presto(:,:,:) = 0._wp
      !
      IF( kn_hdmp > 0 ) THEN      !  Damping poleward of 'nn_hdmp' degrees  !
         !                        !-----------------------------------------!
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '              Damping poleward of ', kn_hdmp, ' deg.'
         !
         CALL iom_open ( 'dist.coast.nc', icot, ldstop = .FALSE. )
         !
         IF( icot > 0 ) THEN          ! distance-to-coast read in file
            CALL iom_get  ( icot, jpdom_data, 'Tcoast', zdct )
            CALL iom_close( icot )
         ELSE                         ! distance-to-coast computed and saved in file (output in zdct)
            CALL cofdis( zdct )
         ENDIF

         !                            ! Compute arrays resto 
         zinfl = 1000.e3_wp                ! distance of influence for damping term
         zlat0 = 10._wp                    ! latitude strip where resto decreases
         zlat1 = REAL( kn_hdmp )           ! resto = 0 between -zlat1 and zlat1
         zlat2 = zlat1 + zlat0             ! resto increases from 0 to 1 between |zlat1| and |zlat2|

         DO jj = 1, jpj
            DO ji = 1, jpi
               zlat = ABS( gphit(ji,jj) )
               IF ( zlat1 <= zlat .AND. zlat <= zlat2 ) THEN
                  presto(ji,jj,1) = 0.5_wp * (  1._wp - COS( rpi*(zlat-zlat1)/zlat0 )  )
               ELSEIF ( zlat > zlat2 ) THEN
                  presto(ji,jj,1) = 1._wp
               ENDIF
            END DO
         END DO

         IF ( kn_hdmp == 20 ) THEN       ! North Indian ocean (20N/30N x 45E/100E) : resto=0
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zlat = gphit(ji,jj)
                  zlon = MOD( glamt(ji,jj), 360._wp )
                  IF ( zlat1 < zlat .AND. zlat < zlat2 .AND. 45._wp < zlon .AND. zlon < 100._wp ) THEN
                     presto(ji,jj,1) = 0._wp
                  ENDIF
               END DO
            END DO
         ENDIF

         zsdmp = 1._wp / ( pn_surf * rday )
         zbdmp = 1._wp / ( pn_bot  * rday )
         DO jk = 2, jpkm1
            DO jj = 1, jpj
               DO ji = 1, jpi
                  zdct(ji,jj,jk) = MIN( zinfl, zdct(ji,jj,jk) )
                  !   ... Decrease the value in the vicinity of the coast
                  presto(ji,jj,jk) = presto(ji,jj,1 ) * 0.5_wp * (  1._wp - COS( rpi*zdct(ji,jj,jk)/zinfl)  )
                  !   ... Vertical variation from zsdmp (sea surface) to zbdmp (bottom)
                  presto(ji,jj,jk) = presto(ji,jj,jk) * (  zbdmp + (zsdmp-zbdmp) * EXP(-fsdept(ji,jj,jk)/pn_dep)  )
               END DO
            END DO
         END DO
         !
      ENDIF

      !                                  ! =========================
      !                                  !  Med and Red Sea damping    (ORCA configuration only)
      !                                  ! =========================
      IF( cp_cfg == "orca" .AND. ( kn_hdmp > 0 .OR. kn_hdmp == -1 ) ) THEN
         IF(lwp)WRITE(numout,*)
         IF(lwp)WRITE(numout,*) '              ORCA configuration: Damping in Med and Red Seas'
         !
         zmrs(:,:) = 0._wp
         !
         SELECT CASE ( jp_cfg )
         !                                           ! =======================
         CASE ( 4 )                                  !  ORCA_R4 configuration 
            !                                        ! =======================
            ij0 =  50   ;   ij1 =  56                    ! Mediterranean Sea

            ii0 =  81   ;   ii1 =  91   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp
            ij0 =  50   ;   ij1 =  55
            ii0 =  75   ;   ii1 =  80   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp
            ij0 =  52   ;   ij1 =  53
            ii0 =  70   ;   ii1 =  74   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp
            ! Smooth transition from 0 at surface to 1./rday at the 18th level in Med and Red Sea
            DO jk = 1, 17
               zhfac (jk) = 0.5_wp * (  1._wp - COS( rpi * REAL(jk-1,wp) / 16._wp )  ) / rday
            END DO
            DO jk = 18, jpkm1
               zhfac (jk) = 1._wp / rday
            END DO
            !                                        ! =======================
         CASE ( 2 )                                  !  ORCA_R2 configuration 
            !                                        ! =======================
            ij0 =  96   ;   ij1 = 110                    ! Mediterranean Sea
            ii0 = 157   ;   ii1 = 181   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp
            ij0 = 100   ;   ij1 = 110
            ii0 = 144   ;   ii1 = 156   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp
            ij0 = 100   ;   ij1 = 103
            ii0 = 139   ;   ii1 = 143   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp
            !
            ij0 = 101   ;   ij1 = 102                    ! Decrease before Gibraltar Strait
            ii0 = 139   ;   ii1 = 141   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 0._wp
            ii0 = 142   ;   ii1 = 142   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp / 90._wp
            ii0 = 143   ;   ii1 = 143   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 0.40_wp
            ii0 = 144   ;   ii1 = 144   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 0.75_wp
            !
            ij0 =  87   ;   ij1 =  96                    ! Red Sea
            ii0 = 147   ;   ii1 = 163   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp
            !
            ij0 =  91   ;   ij1 =  91                    ! Decrease before Bab el Mandeb Strait
            ii0 = 153   ;   ii1 = 160   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 0.80_wp
            ij0 =  90   ;   ij1 =  90
            ii0 = 153   ;   ii1 = 160   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 0.40_wp
            ij0 =  89   ;   ij1 =  89
            ii0 = 158   ;   ii1 = 160   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp / 90._wp
            ij0 =  88   ;   ij1 =  88
            ii0 = 160   ;   ii1 = 163   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 0._wp
            ! Smooth transition from 0 at surface to 1./rday at the 18th level in Med and Red Sea
            DO jk = 1, 17
               zhfac (jk) = 0.5_wp * (  1._wp - COS( rpi * REAL(jk-1,wp) / 16._wp )  ) / rday
            END DO
            DO jk = 18, jpkm1
               zhfac (jk) = 1._wp / rday
            END DO
            !                                        ! =======================
         CASE ( 05 )                                 !  ORCA_R05 configuration
            !                                        ! =======================
            ii0 = 568   ;   ii1 = 574                    ! Mediterranean Sea
            ij0 = 324   ;   ij1 = 333   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp
            ii0 = 575   ;   ii1 = 658
            ij0 = 314   ;   ij1 = 366   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp
            !
            ii0 = 641   ;   ii1 = 651                    ! Black Sea (remaining part
            ij0 = 367   ;   ij1 = 372   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp
            !
            ij0 = 324   ;   ij1 = 333                    ! Decrease before Gibraltar Strait
            ii0 = 565   ;   ii1 = 565   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp / 90._wp
            ii0 = 566   ;   ii1 = 566   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 0.40_wp
            ii0 = 567   ;   ii1 = 567   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 0.75_wp
            !
            ii0 = 641   ;   ii1 = 665                    ! Red Sea
            ij0 = 270   ;   ij1 = 310   ;   zmrs( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1) ) = 1._wp
            !
            ii0 = 666   ;   ii1 = 675                    ! Decrease before Bab el Mandeb Strait
            ij0 = 270   ;   ij1 = 290   
            DO ji = mi0(ii0), mi1(ii1)
               zmrs( ji , mj0(ij0):mj1(ij1) ) = 0.1_wp * ABS( FLOAT(ji - mi1(ii1)) )
            END DO 
            zsdmp = 1._wp / ( pn_surf * rday )
            zbdmp = 1._wp / ( pn_bot  * rday )
            DO jk = 1, jpk
               zhfac(jk) = (  zbdmp + (zsdmp-zbdmp) * EXP( -fsdept(1,1,jk)/pn_dep )  )
            END DO
            !                                       ! ========================
         CASE ( 025 )                               !  ORCA_R025 configuration 
            !                                       ! ========================
            CALL ctl_stop( ' Not yet implemented in ORCA_R025' )
            !
         END SELECT

         DO jk = 1, jpkm1
            presto(:,:,jk) = zmrs(:,:) * zhfac(jk) + ( 1._wp - zmrs(:,:) ) * presto(:,:,jk)
         END DO

         ! Mask resto array and set to 0 first and last levels
         presto(:,:, : ) = presto(:,:,:) * tmask(:,:,:)
         presto(:,:, 1 ) = 0._wp
         presto(:,:,jpk) = 0._wp
         !                         !--------------------!
      ELSE                         !     No damping     !
         !                         !--------------------!
         CALL ctl_stop( 'Choose a correct value of nn_hdmp or put ln_tradmp to FALSE' )
      ENDIF

      !                            !--------------------------------!
      IF( kn_file == 1 ) THEN      !  save damping coef. in a file  !
         !                         !--------------------------------!
         IF(lwp) WRITE(numout,*) '              create damping.coeff.nc file'
         IF( cdtype == 'TRA' ) cfile = 'damping.coeff'
         IF( cdtype == 'TRC' ) cfile = 'damping.coeff.trc'
         cfile = TRIM( cfile )
         CALL iom_open  ( cfile, inum0, ldwrt = .TRUE., kiolib = jprstlib )
         CALL iom_rstput( 0, 0, inum0, 'Resto', presto )
         CALL iom_close ( inum0 )
      ENDIF
      !
      CALL wrk_dealloc( jpk, zhfac)
      CALL wrk_dealloc( jpi, jpj, zmrs )
      CALL wrk_dealloc( jpi, jpj, jpk, zdct )
      !
      IF( nn_timing == 1 )  CALL timing_stop('dtacof')
      !
   END SUBROUTINE dtacof


   SUBROUTINE cofdis( pdct )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE cofdis  ***
      !!
      !! ** Purpose :   Compute the distance between ocean T-points and the
      !!      ocean model coastlines. Save the distance in a NetCDF file.
      !!
      !! ** Method  :   For each model level, the distance-to-coast is 
      !!      computed as follows : 
      !!       - The coastline is defined as the serie of U-,V-,F-points
      !!      that are at the ocean-land bound.
      !!       - For each ocean T-point, the distance-to-coast is then 
      !!      computed as the smallest distance (on the sphere) between the 
      !!      T-point and all the coastline points.
      !!       - For land T-points, the distance-to-coast is set to zero.
      !!      C A U T I O N : Computation not yet implemented in mpp case.
      !!
      !! ** Action  : - pdct, distance to the coastline (argument)
      !!              - NetCDF file 'dist.coast.nc' 
      !!----------------------------------------------------------------------
      USE ioipsl      ! IOipsl librairy
      !!
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT( out ) ::   pdct   ! distance to the coastline
      !!
      INTEGER ::   ji, jj, jk, jl   ! dummy loop indices
      INTEGER ::   iju, ijt, icoast, itime, ierr, icot   ! local integers
      CHARACTER (len=32) ::   clname                     ! local name
      REAL(wp) ::   zdate0                               ! local scalar
      REAL(wp), POINTER, DIMENSION(:,:) ::  zxt, zyt, zzt, zmask
      REAL(wp), POINTER, DIMENSION(:  ) ::  zxc, zyc, zzc, zdis    ! temporary workspace
      LOGICAL , ALLOCATABLE, DIMENSION(:,:) ::  llcotu, llcotv, llcotf   ! 2D logical workspace
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('cofdis')
      !
      CALL wrk_alloc( jpi, jpj , zxt, zyt, zzt, zmask    )
      CALL wrk_alloc( 3*jpi*jpj, zxc, zyc, zzc, zdis     )
      ALLOCATE( llcotu(jpi,jpj), llcotv(jpi,jpj), llcotf(jpi,jpj)  )
      !
      IF( lk_mpp    )   CALL mpp_sum( ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'cofdis: requested local arrays unavailable')

      ! 0. Initialization
      ! -----------------
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'cofdis : compute the distance to coastline'
      IF(lwp) WRITE(numout,*) '~~~~~~'
      IF(lwp) WRITE(numout,*)
      IF( lk_mpp ) &
           & CALL ctl_stop('         Computation not yet implemented with key_mpp_...', &
           &               '         Rerun the code on another computer or ', &
           &               '         create the "dist.coast.nc" file using IDL' )

      pdct(:,:,:) = 0._wp
      zxt(:,:) = COS( rad * gphit(:,:) ) * COS( rad * glamt(:,:) )
      zyt(:,:) = COS( rad * gphit(:,:) ) * SIN( rad * glamt(:,:) )
      zzt(:,:) = SIN( rad * gphit(:,:) )


      ! 1. Loop on vertical levels
      ! --------------------------
      !                                                ! ===============
      DO jk = 1, jpkm1                                 ! Horizontal slab
         !                                             ! ===============
         ! Define the coastline points (U, V and F)
         DO jj = 2, jpjm1
            DO ji = 2, jpim1
               zmask(ji,jj) =  ( tmask(ji,jj+1,jk) + tmask(ji+1,jj+1,jk) &
                   &           + tmask(ji,jj  ,jk) + tmask(ji+1,jj  ,jk) )
               llcotu(ji,jj) = ( tmask(ji,jj,  jk) + tmask(ji+1,jj  ,jk) == 1._wp ) 
               llcotv(ji,jj) = ( tmask(ji,jj  ,jk) + tmask(ji  ,jj+1,jk) == 1._wp ) 
               llcotf(ji,jj) = ( zmask(ji,jj) > 0._wp ) .AND. ( zmask(ji,jj) < 4._wp )
            END DO
         END DO

         ! Lateral boundaries conditions
         llcotu(:, 1 ) = umask(:,  2  ,jk) == 1
         llcotu(:,jpj) = umask(:,jpjm1,jk) == 1
         llcotv(:, 1 ) = vmask(:,  2  ,jk) == 1
         llcotv(:,jpj) = vmask(:,jpjm1,jk) == 1
         llcotf(:, 1 ) = fmask(:,  2  ,jk) == 1
         llcotf(:,jpj) = fmask(:,jpjm1,jk) == 1

         IF( nperio == 1 .OR. nperio == 4 .OR. nperio == 6 ) THEN
            llcotu( 1 ,:) = llcotu(jpim1,:)
            llcotu(jpi,:) = llcotu(  2  ,:)
            llcotv( 1 ,:) = llcotv(jpim1,:)
            llcotv(jpi,:) = llcotv(  2  ,:)
            llcotf( 1 ,:) = llcotf(jpim1,:)
            llcotf(jpi,:) = llcotf(  2  ,:)
         ELSE
            llcotu( 1 ,:) = umask(  2  ,:,jk) == 1
            llcotu(jpi,:) = umask(jpim1,:,jk) == 1
            llcotv( 1 ,:) = vmask(  2  ,:,jk) == 1
            llcotv(jpi,:) = vmask(jpim1,:,jk) == 1
            llcotf( 1 ,:) = fmask(  2  ,:,jk) == 1
            llcotf(jpi,:) = fmask(jpim1,:,jk) == 1
         ENDIF
         IF( nperio == 3 .OR. nperio == 4 ) THEN
            DO ji = 1, jpim1
               iju = jpi - ji + 1
               llcotu(ji,jpj  ) = llcotu(iju,jpj-2)
               llcotf(ji,jpjm1) = llcotf(iju,jpj-2)
               llcotf(ji,jpj  ) = llcotf(iju,jpj-3)
            END DO
            DO ji = jpi/2, jpim1
               iju = jpi - ji + 1
               llcotu(ji,jpjm1) = llcotu(iju,jpjm1)
            END DO
            DO ji = 2, jpi
               ijt = jpi - ji + 2
               llcotv(ji,jpjm1) = llcotv(ijt,jpj-2)
               llcotv(ji,jpj  ) = llcotv(ijt,jpj-3)
            END DO
         ENDIF
         IF( nperio == 5 .OR. nperio == 6 ) THEN
            DO ji = 1, jpim1
               iju = jpi - ji
               llcotu(ji,jpj  ) = llcotu(iju,jpjm1)
               llcotf(ji,jpj  ) = llcotf(iju,jpj-2)
            END DO
            DO ji = jpi/2, jpim1
               iju = jpi - ji
               llcotf(ji,jpjm1) = llcotf(iju,jpjm1)
            END DO
            DO ji = 1, jpi
               ijt = jpi - ji + 1
               llcotv(ji,jpj  ) = llcotv(ijt,jpjm1)
            END DO
            DO ji = jpi/2+1, jpi
               ijt = jpi - ji + 1
               llcotv(ji,jpjm1) = llcotv(ijt,jpjm1)
            END DO
         ENDIF

         ! Compute cartesian coordinates of coastline points
         ! and the number of coastline points
         icoast = 0
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( llcotf(ji,jj) ) THEN
                  icoast = icoast + 1
                  zxc(icoast) = COS( rad*gphif(ji,jj) ) * COS( rad*glamf(ji,jj) )
                  zyc(icoast) = COS( rad*gphif(ji,jj) ) * SIN( rad*glamf(ji,jj) )
                  zzc(icoast) = SIN( rad*gphif(ji,jj) )
               ENDIF
               IF( llcotu(ji,jj) ) THEN
                  icoast = icoast+1
                  zxc(icoast) = COS( rad*gphiu(ji,jj) ) * COS( rad*glamu(ji,jj) )
                  zyc(icoast) = COS( rad*gphiu(ji,jj) ) * SIN( rad*glamu(ji,jj) )
                  zzc(icoast) = SIN( rad*gphiu(ji,jj) )
               ENDIF
               IF( llcotv(ji,jj) ) THEN
                  icoast = icoast+1
                  zxc(icoast) = COS( rad*gphiv(ji,jj) ) * COS( rad*glamv(ji,jj) )
                  zyc(icoast) = COS( rad*gphiv(ji,jj) ) * SIN( rad*glamv(ji,jj) )
                  zzc(icoast) = SIN( rad*gphiv(ji,jj) )
               ENDIF
            END DO
         END DO

         ! Distance for the T-points
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( tmask(ji,jj,jk) == 0._wp ) THEN
                  pdct(ji,jj,jk) = 0._wp
               ELSE
                  DO jl = 1, icoast
                     zdis(jl) = ( zxt(ji,jj) - zxc(jl) )**2   &
                        &     + ( zyt(ji,jj) - zyc(jl) )**2   &
                        &     + ( zzt(ji,jj) - zzc(jl) )**2
                  END DO
                  pdct(ji,jj,jk) = ra * SQRT( MINVAL( zdis(1:icoast) ) )
               ENDIF
            END DO
         END DO
         !                                                ! ===============
      END DO                                              !   End of slab
      !                                                   ! ===============


      ! 2. Create the  distance to the coast file in NetCDF format
      ! ----------------------------------------------------------    
      clname = 'dist.coast'
      itime  = 0
      CALL ymds2ju( 0     , 1      , 1     , 0._wp , zdate0 )
      CALL restini( 'NONE', jpi    , jpj   , glamt, gphit ,   &
         &          jpk   , gdept_0, clname, itime, zdate0,   &
         &          rdt   , icot                         )
      CALL restput( icot, 'Tcoast', jpi, jpj, jpk, 0, pdct )
      CALL restclo( icot )
      !
      CALL wrk_dealloc( jpi, jpj , zxt, zyt, zzt, zmask    )
      CALL wrk_dealloc( 3*jpi*jpj, zxc, zyc, zzc, zdis     )
      DEALLOCATE( llcotu, llcotv, llcotf  )
      !
      IF( nn_timing == 1 )  CALL timing_stop('cofdis')
      !
   END SUBROUTINE cofdis
   !!======================================================================
END MODULE tradmp
