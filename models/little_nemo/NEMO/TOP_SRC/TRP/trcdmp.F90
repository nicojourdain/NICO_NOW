MODULE trcdmp
   !!======================================================================
   !!                       ***  MODULE  trcdmp  ***
   !! Ocean physics: internal restoring trend on passive tracers
   !!======================================================================
   !! History :  OPA  !  1991-03  (O. Marti, G. Madec)  Original code
   !!                 !  1996-01  (G. Madec) statement function for e3
   !!                 !  1997-05  (H. Loukos)  adapted for passive tracers
   !!    NEMO    9.0  !  2004-03  (C. Ethe)    free form + modules
   !!            3.2  !  2007-02  (C. Deltel)  Diagnose ML trends for passive tracers
   !!            3.3  !  2010-06  (C. Ethe, G. Madec) merge TRA-TRC 
   !!----------------------------------------------------------------------
#if  defined key_top && defined key_trcdmp 
   !!----------------------------------------------------------------------
   !!   key_trcdmp                                         internal damping
   !!----------------------------------------------------------------------
   !!   trc_dmp      : update the tracer trend with the internal damping
   !!   trc_dmp_init : initialization, namlist read, parameters control
   !!----------------------------------------------------------------------
   USE oce_trc         ! ocean dynamics and tracers variables
   USE trc             ! ocean passive tracers variables
   USE trcnam_trp      ! passive tracers transport namelist variables
   USE trcdta
   USE tradmp
   USE prtctl_trc      ! Print control for debbuging
   USE trdtra

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_dmp            ! routine called by step.F90
   PUBLIC trc_dmp_alloc      ! routine called by nemogcm.F90

   LOGICAL , PUBLIC, PARAMETER ::   lk_trcdmp = .TRUE.   !: internal damping flag

   !                                !!* Namelist namtrc_dmp : passive tracer newtonian damping *
   INTEGER  ::   nn_hdmp_tr =   -1   ! = 0/-1/'latitude' for damping over passive tracer
   INTEGER  ::   nn_zdmp_tr =    0   ! = 0/1/2 flag for damping in the mixed layer
   REAL(wp) ::   rn_surf_tr =   50.  ! surface time scale for internal damping        [days]
   REAL(wp) ::   rn_bot_tr  =  360.  ! bottom time scale for internal damping         [days]
   REAL(wp) ::   rn_dep_tr  =  800.  ! depth of transition between rn_surf and rn_bot [meters]
   INTEGER  ::   nn_file_tr =    2   ! = 1 create a damping.coeff NetCDF file 

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   restotr   ! restoring coeff. on tracers (s-1)

   !! * Substitutions
#  include "top_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Header: /home/opalod/NEMOCVSROOT/NEMO/TOP_SRC/TRP/trcdmp.F90,v 1.11 2006/09/01 14:03:49 opalod Exp $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION trc_dmp_alloc()
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_dmp_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( restotr(jpi,jpj,jpk) , STAT=trc_dmp_alloc )
      !
      IF( trc_dmp_alloc /= 0 )   CALL ctl_warn('trc_dmp_alloc: failed to allocate array')
      !
   END FUNCTION trc_dmp_alloc


   SUBROUTINE trc_dmp( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_dmp  ***
      !!                  
      !! ** Purpose :   Compute the passive tracer trend due to a newtonian damping
      !!      of the tracer field towards given data field and add it to the
      !!      general tracer trends.
      !!
      !! ** Method  :   Newtonian damping towards trdta computed 
      !!      and add to the general tracer trends:
      !!                     trn = tra + restotr * (trdta - trb)
      !!         The trend is computed either throughout the water column
      !!      (nlmdmptr=0) or in area of weak vertical mixing (nlmdmptr=1) or
      !!      below the well mixed layer (nlmdmptr=2)
      !!
      !! ** Action  : - update the tracer trends tra with the newtonian 
      !!                damping trends.
      !!              - save the trends ('key_trdmld_trc')
      !!----------------------------------------------------------------------
      !!
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index
      !!
      INTEGER  ::   ji, jj, jk, jn       ! dummy loop indices
      REAL(wp) ::   ztra                 ! temporary scalars
      CHARACTER (len=22) :: charout
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   ztrtrd
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_dmp')
      !
      ! 0. Initialization (first time-step only)
      !    --------------
      IF( kt == nittrc000 ) CALL trc_dmp_init

      IF( l_trdtrc )   CALL wrk_alloc( jpi, jpj, jpk, ztrtrd )   ! temporary save of trends

      ! 1. Newtonian damping trends on tracer fields
      ! --------------------------------------------
      ! Initialize the input fields for newtonian damping
      CALL trc_dta( kt )
      !                                                          ! ===========
      DO jn = 1, jptra                                           ! tracer loop
         !                                                       ! ===========
         IF( l_trdtrc ) ztrtrd(:,:,:) = tra(:,:,:,jn)    ! save trends 

         IF( lutini(jn) ) THEN
            !
            SELECT CASE ( nn_zdmp_trc )
            !
            CASE( 0 )                !==  newtonian damping throughout the water column  ==!
               DO jk = 1, jpkm1
                  DO jj = 2, jpjm1
                     DO ji = fs_2, fs_jpim1   ! vector opt.
                        ztra = restotr(ji,jj,jk) * ( trdta(ji,jj,jk,jn) - trb(ji,jj,jk,jn) )
                        tra(ji,jj,jk,jn) = tra(ji,jj,jk,jn) + ztra
                     END DO
                  END DO
               END DO
            !
            CASE ( 1 )                !==  no damping in the turbocline (avt > 5 cm2/s)  ==!
               DO jk = 1, jpkm1
                  DO jj = 2, jpjm1
                     DO ji = fs_2, fs_jpim1   ! vector opt.
                        IF( avt(ji,jj,jk) <= 5.e-4 )  THEN 
                           ztra = restotr(ji,jj,jk) * ( trdta(ji,jj,jk,jn) - trb(ji,jj,jk,jn) )
                           tra(ji,jj,jk,jn) = tra(ji,jj,jk,jn) + ztra
                        ENDIF
                     END DO
                  END DO
               END DO
            !
            CASE ( 2 )               !==  no damping in the mixed layer   ==! 
               DO jk = 1, jpkm1
                  DO jj = 2, jpjm1
                     DO ji = fs_2, fs_jpim1   ! vector opt.
                        IF( fsdept(ji,jj,jk) >= hmlp (ji,jj) ) THEN
                           ztra = restotr(ji,jj,jk,jn) * ( trdta(ji,jj,jk,jn) - trb(ji,jj,jk,jn) )
                           tra(ji,jj,jk,jn) = tra(ji,jj,jk,jn) + ztra
                        END IF
                     END DO
                  END DO
               END DO
            !  
            END SELECT
            ! 
         ENDIF
         !
         IF( l_trdtrc ) THEN
            ztrtrd(:,:,:) = tra(:,:,:,jn) -  ztrtrd(:,:,:)
            CALL trd_tra( kt, 'TRC', jn, jptra_trd_dmp, ztrtrd )
         END IF
         !                                                       ! ===========
      END DO                                                     ! tracer loop
      !                                                          ! ===========
      IF( l_trdtrc )  CALL wrk_dealloc( jpi, jpj, jpk, ztrtrd )
      !                                          ! print mean trends (used for debugging)
      IF( ln_ctl )   THEN
         WRITE(charout, FMT="('dmp ')") ;  CALL prt_ctl_trc_info(charout)
                                           CALL prt_ctl_trc( tab4d=tra, mask=tmask, clinfo=ctrcnm, clinfo2='trd' )
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('trc_dmp')
      !
   END SUBROUTINE trc_dmp


   SUBROUTINE trc_dmp_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_dmp_init  ***
      !! 
      !! ** Purpose :   Initialization for the newtonian damping 
      !!
      !! ** Method  :   read the nammbf namelist and check the parameters
      !!              called by trc_dmp at the first timestep (nittrc000)
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_dmp_init')
      !
      SELECT CASE ( nn_hdmp_tr )
      CASE (  -1  )   ;   IF(lwp) WRITE(numout,*) '   tracer damping in the Med & Red seas only'
      CASE ( 1:90 )   ;   IF(lwp) WRITE(numout,*) '   tracer damping poleward of', nn_hdmp_tr, ' degrees'
      CASE DEFAULT
         WRITE(ctmp1,*) '          bad flag value for nn_hdmp_tr = ', nn_hdmp_tr
         CALL ctl_stop(ctmp1)
      END SELECT

      SELECT CASE ( nn_zdmp_tr )
      CASE ( 0 )   ;   IF(lwp) WRITE(numout,*) '   tracer damping throughout the water column'
      CASE ( 1 )   ;   IF(lwp) WRITE(numout,*) '   no tracer damping in the turbocline (avt > 5 cm2/s)'
      CASE ( 2 )   ;   IF(lwp) WRITE(numout,*) '   no tracer damping in the mixed layer'
      CASE DEFAULT
         WRITE(ctmp1,*) 'bad flag value for nn_zdmp_tr = ', nn_zdmp_tr
         CALL ctl_stop(ctmp1)
      END SELECT

      IF( .NOT. lk_dtatrc )   &
         &   CALL ctl_stop( 'no passive tracer data define key_dtatrc' )

      IF( .NOT. lk_tradmp )   &
         &   CALL ctl_stop( 'passive trace damping need key_tradmp to compute damping coef.' )
      !
      !                          ! Damping coefficients initialization
      IF( lzoom ) THEN   ;   CALL dtacof_zoom( restotr )
      ELSE               ;   CALL dtacof( nn_hdmp_tr, rn_surf_tr, rn_bot_tr, rn_dep_tr,  &
                             &            nn_file_tr, 'TRC'     , restotr                )
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('trc_dmp_init')
      !
   END SUBROUTINE trc_dmp_init
#else
   !!----------------------------------------------------------------------
   !!   Default key                                     NO internal damping
   !!----------------------------------------------------------------------
   LOGICAL , PUBLIC, PARAMETER ::   lk_trcdmp = .FALSE.    !: internal damping flag
CONTAINS
   SUBROUTINE trc_dmp( kt )        ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'trc_dmp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_dmp
#endif
   !!======================================================================
END MODULE trcdmp
