MODULE zdfini
   !!======================================================================
   !!                      ***  MODULE  zdfini  ***
   !! Ocean physics :   read vertical mixing namelist and check consistancy
   !!======================================================================
   !! History :  8.0  ! 1997-06  (G. Madec)  Original code from inimix
   !!            1.0  ! 2002-08  (G. Madec)  F90 : free form
   !!             -   ! 2005-06  (C. Ethe) KPP parameterization
   !!             -   ! 2009-07  (G. Madec) add avmb, avtb in restart for cen2 advection
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   zdf_init    : initialization, namelist read, and parameters control
   !!----------------------------------------------------------------------
   USE par_oce         ! mesh and scale factors
   USE ldftra_oce      ! ocean active tracers: lateral physics
   USE ldfdyn_oce      ! ocean dynamics lateral physics
   USE zdf_oce         ! TKE vertical mixing          
   USE lib_mpp         ! distribued memory computing
   USE zdftke          ! TKE vertical mixing
   USE zdfgls          ! GLS vertical mixing
   USE zdfkpp          ! KPP vertical mixing          
   USE zdfddm          ! double diffusion mixing      
   USE zdfevd          ! enhanced vertical diffusion  
   USE zdfric          ! Richardson vertical mixing   
   USE tranpc          ! convection: non penetrative adjustment
   USE ldfslp          ! iso-neutral slopes

   USE in_out_manager  ! I/O manager
   USE iom             ! IOM library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_init   ! routine called by opa.F90
   
   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: zdfini.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE zdf_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_init  ***
      !! 
      !! ** Purpose :   initializations of the vertical ocean physics
      !!
      !! ** Method  :   Read namelist namzdf, control logicals 
      !!----------------------------------------------------------------------
      INTEGER ::   ioptio       ! temporary scalar
      !!
      NAMELIST/namzdf/ rn_avm0, rn_avt0, nn_avb, nn_havtb, ln_zdfexp, nn_zdfexp,   &
         &              ln_zdfevd, nn_evdm, rn_avevd, ln_zdfnpc, nn_npc, nn_npcp
      !!----------------------------------------------------------------------

      REWIND( numnam )           !* Read namzdf namelist : vertical mixing parameters
      READ  ( numnam, namzdf )

      IF(lwp) THEN               !* Parameter print
         WRITE(numout,*)
         WRITE(numout,*) 'zdf_init: vertical physics'
         WRITE(numout,*) '~~~~~~~~'
         WRITE(numout,*) '   Namelist namzdf : set vertical mixing mixing parameters'
         WRITE(numout,*) '      vertical eddy viscosity             rn_avm0   = ', rn_avm0
         WRITE(numout,*) '      vertical eddy diffusivity           rn_avt0   = ', rn_avt0
         WRITE(numout,*) '      constant background or profile      nn_avb    = ', nn_avb
         WRITE(numout,*) '      horizontal variation for avtb       nn_havtb  = ', nn_havtb
         WRITE(numout,*) '      time splitting / backward scheme    ln_zdfexp = ', ln_zdfexp
         WRITE(numout,*) '      number of time step                 nn_zdfexp = ', nn_zdfexp
         WRITE(numout,*) '      enhanced vertical diffusion         ln_zdfevd = ', ln_zdfevd
         WRITE(numout,*) '         applied on momentum (=1/0)       nn_evdm   = ', nn_evdm
         WRITE(numout,*) '      vertical coefficient for evd        rn_avevd  = ', rn_avevd
         WRITE(numout,*) '      non-penetrative convection (npc)    ln_zdfnpc = ', ln_zdfnpc
         WRITE(numout,*) '      npc call  frequency                 nn_npc    = ', nn_npc
         WRITE(numout,*) '      npc print frequency                 nn_npcp   = ', nn_npcp
      ENDIF

      !                          !* Parameter & logical controls
      !                          !  ----------------------------
      !
      !                               ! ... check of vertical mixing scheme on tracers
      !                                              ==> will be done in trazdf module
      !
      !                               ! ... check of mixing coefficient
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '   vertical mixing option :'
      ioptio = 0
      IF( lk_zdfcst ) THEN
         IF(lwp) WRITE(numout,*) '      constant eddy diffusion coefficients'
         ioptio = ioptio+1
      ENDIF
      IF( lk_zdfric ) THEN
         IF(lwp) WRITE(numout,*) '      Richardson dependent eddy coefficients'
         ioptio = ioptio+1
      ENDIF
      IF( lk_zdftke ) THEN
         IF(lwp) WRITE(numout,*) '      TKE dependent eddy coefficients'
         ioptio = ioptio+1
      ENDIF
      IF( lk_zdfgls ) THEN
         IF(lwp) WRITE(numout,*) '      GLS dependent eddy coefficients'
         ioptio = ioptio+1
      ENDIF
      IF( lk_zdfkpp ) THEN
         IF(lwp) WRITE(numout,*) '      KPP dependent eddy coefficients'
         ioptio = ioptio+1
      ENDIF
      IF( ioptio == 0 .OR. ioptio > 1 .AND. .NOT. lk_esopa )   &
         &   CALL ctl_stop( ' one and only one vertical diffusion option has to be defined ' )
      !
      !                               ! ... Convection
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '   convection :'
      ioptio = 0
      IF( ln_zdfnpc ) THEN
         IF(lwp) WRITE(numout,*) '      use non penetrative convective scheme'
         ioptio = ioptio+1
      ENDIF
      IF( ln_zdfevd ) THEN
         IF(lwp) WRITE(numout,*) '      use enhanced vertical dif. scheme'
         ioptio = ioptio+1
      ENDIF
      IF( lk_zdftke ) THEN
         IF(lwp) WRITE(numout,*) '      use the 1.5 turbulent closure'
      ENDIF
      IF( lk_zdfgls ) THEN
         IF(lwp) WRITE(numout,*) '      use the GLS closure scheme'
      ENDIF
      IF( lk_zdfkpp ) THEN
         IF(lwp) WRITE(numout,*) '      use the KPP closure scheme'
         IF(lk_mpp) THEN
            IF(lwp) WRITE(numout,cform_err)
            IF(lwp) WRITE(numout,*) 'The KPP scheme is not ready to run in MPI'
         ENDIF
      ENDIF
      IF ( ioptio > 1 .AND. .NOT. lk_esopa )   CALL ctl_stop( ' chose between ln_zdfnpc and ln_zdfevd' )
      IF( ioptio == 0 .AND. .NOT.( lk_zdftke .OR. lk_zdfgls .OR. lk_zdfkpp ) )           &
         CALL ctl_stop( ' except for TKE, GLS or KPP physics, a convection scheme is',   &
         &              ' required: ln_zdfevd or ln_zdfnpc logicals' )

      !                               !* Background eddy viscosity and diffusivity profil
      IF( nn_avb == 0 ) THEN                ! Define avmb, avtb from namelist parameter
         avmb(:) = rn_avm0
         avtb(:) = rn_avt0                     
      ELSE                                  ! Background profile of avt (fit a theoretical/observational profile (Krauss 1990)
         avmb(:) = rn_avm0
         avtb(:) = rn_avt0 + ( 3.e-4_wp - 2._wp * rn_avt0 ) * 1.e-4_wp * gdepw_0(:)   ! m2/s
         IF(ln_sco .AND. lwp)   CALL ctl_warn( 'avtb profile not valid in sco' )
      ENDIF
      !
      IF( ln_rstart ) THEN                  !  Read avmb, avtb in restart (if exist)
         ! if ln_traadv_cen, avmb, avtb have been modified in traadv_cen2 module. 
         ! To ensure the restartability, avmb & avtb are written in the restart 
         ! file in traadv_cen2 end read here. 
         IF( iom_varid( numror, 'avmb', ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numror, jpdom_unknown, 'avmb', avmb )
            CALL iom_get( numror, jpdom_unknown, 'avtb', avtb )
         ENDIF
      ENDIF
      !                                     ! 2D shape of the avtb
      avtb_2d(:,:) = 1.e0                        ! uniform 
      !
      IF( nn_havtb == 1 ) THEN                   ! decrease avtb in the equatorial band
           !  -15S -5S : linear decrease from avt0 to avt0/10.
           !  -5S  +5N : cst value avt0/10.
           !   5N  15N : linear increase from avt0/10, to avt0
           WHERE(-15. <= gphit .AND. gphit < -5 )   avtb_2d = (1.  - 0.09 * (gphit + 15.))
           WHERE( -5. <= gphit .AND. gphit <  5 )   avtb_2d =  0.1
           WHERE(  5. <= gphit .AND. gphit < 15 )   avtb_2d = (0.1 + 0.09 * (gphit -  5.))
      ENDIF
      !
   END SUBROUTINE zdf_init

   !!======================================================================
END MODULE zdfini
