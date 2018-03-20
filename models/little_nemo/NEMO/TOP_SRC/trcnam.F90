MODULE trcnam
   !!======================================================================
   !!                       ***  MODULE trcnam  ***
   !! TOP :   Read and print options for the passive tracer run (namelist)
   !!======================================================================
   !! History :    -   !  1996-11  (M.A. Foujols, M. Levy)  original code
   !!              -   !  1998-04  (M.A Foujols, L. Bopp) ahtrb0 for isopycnal mixing
   !!              -   !  1999-10  (M.A. Foujols, M. Levy) separation of sms
   !!              -   !  2000-07  (A. Estublier) add TVD and MUSCL : Tests on ndttrc
   !!              -   !  2000-11  (M.A Foujols, E Kestenare) trcrat, ahtrc0 and aeivtr0
   !!              -   !  2001-01 (E Kestenare) suppress ndttrc=1 for CEN2 and TVD schemes
   !!             1.0  !  2005-03 (O. Aumont, A. El Moussaoui) F90
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_nam    :  Read and print options for the passive tracer run (namelist)
   !!----------------------------------------------------------------------
   USE oce_trc           ! shared variables between ocean and passive tracers
   USE trc               ! passive tracers common variables
   USE trcnam_trp        ! Transport namelist
   USE trcnam_lobster    ! LOBSTER namelist
   USE trcnam_pisces     ! PISCES namelist
   USE trcnam_cfc        ! CFC SMS namelist
   USE trcnam_c14b       ! C14 SMS namelist
   USE trcnam_my_trc     ! MY_TRC SMS namelist
   USE trdmod_oce       
   USE trdmod_trc_oce
   USE iom               ! I/O manager

   IMPLICIT NONE
   PRIVATE 

   PUBLIC trc_nam      ! called in trcini

   !! * Substitutions
#  include "top_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcnam.F90 3319 2012-03-05 16:03:27Z cetlod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_nam
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_nam  ***
      !!
      !! ** Purpose :   READ and PRINT options for the passive tracer run (namelist) 
      !!
      !! ** Method  : - read passive tracer namelist 
      !!              - read namelist of each defined SMS model
      !!                ( (LOBSTER, PISCES, CFC, MY_TRC )
      !!---------------------------------------------------------------------
      INTEGER ::  jn, ierr
      ! Definition of a tracer as a structure
      TYPE(PTRACER), DIMENSION(jptra) :: sn_tracer  ! type of tracer for saving if not key_iomput
      !!
      NAMELIST/namtrc/ nn_dttrc, nn_writetrc, ln_rsttr, nn_rsttr, &
         &             cn_trcrst_in, cn_trcrst_out, sn_tracer, ln_trcdta, ln_trcdmp
#if defined key_trdmld_trc  || defined key_trdtrc
      NAMELIST/namtrc_trd/ nn_trd_trc, nn_ctls_trc, rn_ucf_trc, &
         &                ln_trdmld_trc_restart, ln_trdmld_trc_instant, &
         &                cn_trdrst_trc_in, cn_trdrst_trc_out, ln_trdtrc
#endif
      NAMELIST/namtrc_dia/ ln_diatrc, ln_diabio, nn_writedia, nn_writebio

      !!---------------------------------------------------------------------

      IF(lwp) WRITE(numout,*) 'trc_nam : read the passive tracer namelists'
      IF(lwp) WRITE(numout,*) '~~~~~~~'

      CALL ctl_opn( numnat, 'namelist_top', 'OLD', 'FORMATTED', 'SEQUENTIAL', 1, numout, .FALSE. )

      ! Namelist nattrc (files)
      ! ----------------------------------------------
      nn_dttrc      = 1                 ! default values
      nn_writetrc   = 10 
      ln_rsttr      = .FALSE.
      nn_rsttr      =  0
      cn_trcrst_in  = 'restart_trc'
      cn_trcrst_out = 'restart_trc'
      !
      DO jn = 1, jptra
         WRITE( sn_tracer(jn)%clsname,'("TR_",I1)'           ) jn
         WRITE( sn_tracer(jn)%cllname,'("TRACER NUMBER ",I1)') jn
         sn_tracer(jn)%clunit  = 'mmole/m3'
         sn_tracer(jn)%llinit  = .FALSE.
         sn_tracer(jn)%llsave  = .TRUE.
      END DO
      ln_trcdta = .FALSE.
      ln_trcdmp = .FALSE.


      REWIND( numnat )               ! read nattrc
      READ  ( numnat, namtrc )

      DO jn = 1, jptra
         ctrcnm    (jn) = TRIM( sn_tracer(jn)%clsname )
         ctrcln    (jn) = TRIM( sn_tracer(jn)%cllname )
         ctrcun    (jn) = TRIM( sn_tracer(jn)%clunit  )
         ln_trc_ini(jn) =       sn_tracer(jn)%llinit
         ln_trc_wri(jn) =       sn_tracer(jn)%llsave
      END DO

      !!KPE  computes the first time step of tracer model
      nittrc000 = nit000 + nn_dttrc - 1
 

      IF(lwp) THEN                   ! control print
         WRITE(numout,*)
         WRITE(numout,*) ' Namelist : namtrc'
         WRITE(numout,*) '   time step freq. for passive tracer           nn_dttrc      = ', nn_dttrc
         WRITE(numout,*) '   restart  for passive tracer                  ln_rsttr      = ', ln_rsttr
         WRITE(numout,*) '   control of time step for passive tracer      nn_rsttr      = ', nn_rsttr
         WRITE(numout,*) '   first time step for pass. trac.              nittrc000     = ', nittrc000
         WRITE(numout,*) '   frequency of outputs for passive tracers     nn_writetrc   = ', nn_writetrc  
         WRITE(numout,*) '   Read inputs data from file (y/n)             ln_trcdta     = ', ln_trcdta
         WRITE(numout,*) '   Damping of passive tracer (y/n)              ln_trcdmp     = ', ln_trcdmp
         WRITE(numout,*) ' '
         DO jn = 1, jptra
            WRITE(numout,*) '  tracer nb : ', jn, '    short name : ', ctrcnm(jn)
         END DO
         WRITE(numout,*) ' '
      ENDIF

      rdttrc(:) = rdttra(:) * FLOAT( nn_dttrc )   ! vertical profile of passive tracer time-step
  
      IF(lwp) THEN                   ! control print
        WRITE(numout,*) 
        WRITE(numout,*) '    Passive Tracer  time step    rdttrc  = ', rdttrc(1)
        WRITE(numout,*) 
      ENDIF

      ln_diatrc = .FALSE.
      ln_diabio = .FALSE.
      nn_writedia = 10
      nn_writebio = 10

      REWIND( numnat )               !  namelist namtoptrd : passive tracer trends diagnostic
      READ  ( numnat, namtrc_dia )

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*)
         WRITE(numout,*) ' Namelist : namtrc_dia'
         WRITE(numout,*) '    save additionnal diagnostics arrays         ln_diatrc   = ', ln_diatrc
         WRITE(numout,*) '    save additionnal biology diagnostics arrays ln_diabio   = ', ln_diabio
         WRITE(numout,*) '    frequency of outputs for additional arrays  nn_writedia = ', nn_writedia
         WRITE(numout,*) '    frequency of outputs for biological trends  nn_writebio = ', nn_writebio
         WRITE(numout,*) ' '
      ENDIF

      IF( ln_diatrc .AND. .NOT. lk_iomput ) THEN 
         ALLOCATE( trc2d(jpi,jpj,jpdia2d), trc3d(jpi,jpj,jpk,jpdia3d),  &
           &       ctrc2d(jpdia2d), ctrc2l(jpdia2d), ctrc2u(jpdia2d) ,  & 
           &       ctrc3d(jpdia3d), ctrc3l(jpdia3d), ctrc3u(jpdia3d) ,  STAT = ierr ) 
         IF( ierr > 0 )   CALL ctl_stop( 'STOP', 'trcnam: unable to allocate add. diag. array' )
      ENDIF

      IF( ( ln_diabio .AND. .NOT. lk_iomput ) .OR. l_trdtrc ) THEN
         ALLOCATE( trbio (jpi,jpj,jpk,jpdiabio) , &
           &       ctrbio(jpdiabio), ctrbil(jpdiabio), ctrbiu(jpdiabio), STAT = ierr ) 
         IF( ierr > 0 )   CALL ctl_stop( 'STOP', 'trcnam: unable to allocate bio. diag. array' )
      ENDIF

      ! namelist of transport
      ! ---------------------
      CALL trc_nam_trp


      IF( ln_trcdmp .AND. .NOT.ln_trcdta ) THEN
         CALL ctl_warn( 'trc_nam: passive tracer damping requires data from files we set ln_trcdta to TRUE' )
         ln_trcdta = .TRUE.
      ENDIF
      !
      IF( ln_rsttr .AND. .NOT.ln_trcdmp .AND. ln_trcdta ) THEN
          CALL ctl_warn( 'trc_nam: passive tracer restart and  data intialisation, ',   &
             &           'we keep the restart values and set ln_trcdta to FALSE' )
         ln_trcdta = .FALSE.
      ENDIF
      !
      IF( .NOT.ln_trcdta ) THEN
         ln_trc_ini(:) = .FALSE.
      ENDIF

      IF(lwp) THEN                   ! control print
         IF( ln_rsttr ) THEN
            WRITE(numout,*)
            WRITE(numout,*) '    read a restart file for passive tracer : ', TRIM( cn_trcrst_in )
            WRITE(numout,*)
         ELSE
            IF( .NOT.ln_trcdta ) THEN
                WRITE(numout,*)
                WRITE(numout,*) '  All the passive tracers are initialised with constant values '
                WRITE(numout,*)
            ENDIF
         ENDIF
      ENDIF


#if defined key_trdmld_trc || defined key_trdtrc
         nn_trd_trc  = 20
         nn_ctls_trc =  9
         rn_ucf_trc   =  1.
         ln_trdmld_trc_instant = .TRUE.
         ln_trdmld_trc_restart =.FALSE.
         cn_trdrst_trc_in  = "restart_mld_trc"
         cn_trdrst_trc_out = "restart_mld_trc"
         ln_trdtrc(:) = .FALSE.

         REWIND( numnat )               !  namelist namtoptrd : passive tracer trends diagnostic
         READ  ( numnat, namtrc_trd )

         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) ' trd_mld_trc_init : read namelist namtrc_trd                    '
            WRITE(numout,*) ' ~~~~~~~~~~~~~~~~                                               '
            WRITE(numout,*) '   * frequency of trends diagnostics   nn_trd_trc             = ', nn_trd_trc
            WRITE(numout,*) '   * control surface type              nn_ctls_trc            = ', nn_ctls_trc
            WRITE(numout,*) '   * restart for ML diagnostics        ln_trdmld_trc_restart  = ', ln_trdmld_trc_restart
            WRITE(numout,*) '   * flag to diagnose trends of                                 '
            WRITE(numout,*) '     instantantaneous or mean ML T/S   ln_trdmld_trc_instant  = ', ln_trdmld_trc_instant
            WRITE(numout,*) '   * unit conversion factor            rn_ucf_trc             = ', rn_ucf_trc
            DO jn = 1, jptra
               IF( ln_trdtrc(jn) ) WRITE(numout,*) '    compute ML trends for tracer number :', jn
            END DO
         ENDIF
#endif


      ! namelist of SMS
      ! ---------------      
      IF( lk_lobster ) THEN   ;   CALL trc_nam_lobster      ! LOBSTER bio-model
      ELSE                    ;   IF(lwp) WRITE(numout,*) '          LOBSTER not used'
      ENDIF

      IF( lk_pisces  ) THEN   ;   CALL trc_nam_pisces      ! PISCES  bio-model
      ELSE                    ;   IF(lwp) WRITE(numout,*) '          PISCES not used'
      ENDIF

      IF( lk_cfc     ) THEN   ;   CALL trc_nam_cfc         ! CFC     tracers
      ELSE                    ;   IF(lwp) WRITE(numout,*) '          CFC not used'
      ENDIF

      IF( lk_c14b     ) THEN   ;   CALL trc_nam_c14b         ! C14 bomb     tracers
      ELSE                    ;   IF(lwp) WRITE(numout,*) '          C14 not used'
      ENDIF

      IF( lk_my_trc  ) THEN   ;   CALL trc_nam_my_trc      ! MY_TRC  tracers
      ELSE                    ;   IF(lwp) WRITE(numout,*) '          MY_TRC not used'
      ENDIF
      !
   END SUBROUTINE trc_nam

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_nam                      ! Empty routine   
   END SUBROUTINE trc_nam
#endif

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcnam.F90 3319 2012-03-05 16:03:27Z cetlod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE  trcnam
