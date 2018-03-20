MODULE trcnam_trp
   !!======================================================================
   !!                       ***  MODULE  trcnam_trp  ***
   !! TOP :   namelist read options for transport
   !!======================================================================
   !! History :   1.0  !  2004-03  (C. Ethe)  Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_nam_trp  : read the passive tracer namelist for transport
   !!----------------------------------------------------------------------
   USE trc                 ! ocean passive tracers variables
   USE in_out_manager      ! ocean dynamics and active tracers variables

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_nam_trp    ! routine called by step module
 
   !                                                 !!: ** Advection (nam_trcadv) **
   LOGICAL , PUBLIC ::   ln_trcadv_cen2   = .FALSE.   ! 2nd order centered scheme flag
   LOGICAL , PUBLIC ::   ln_trcadv_tvd    = .TRUE.    ! TVD scheme flag
   LOGICAL , PUBLIC ::   ln_trcadv_muscl  = .FALSE.   ! MUSCL scheme flag
   LOGICAL , PUBLIC ::   ln_trcadv_muscl2 = .FALSE.   ! MUSCL2 scheme flag
   LOGICAL , PUBLIC ::   ln_trcadv_ubs    = .FALSE.   ! UBS scheme flag
   LOGICAL , PUBLIC ::   ln_trcadv_qck    = .FALSE.   ! QUICKEST scheme flag

   !                                                 !!: ** lateral mixing namelist (nam_trcldf) **
   LOGICAL , PUBLIC ::   ln_trcldf_diff  = .FALSE.    !: flag of perform or not the lateral diff.
   LOGICAL , PUBLIC ::   ln_trcldf_lap   = .TRUE.     !: laplacian operator
   LOGICAL , PUBLIC ::   ln_trcldf_bilap = .FALSE.    !: bilaplacian operator
   LOGICAL , PUBLIC ::   ln_trcldf_level = .FALSE.    !: iso-level direction
   LOGICAL , PUBLIC ::   ln_trcldf_hor   = .FALSE.    !: horizontal (geopotential) direction
   LOGICAL , PUBLIC ::   ln_trcldf_iso   = .TRUE.     !: iso-neutral direction
   REAL(wp), PUBLIC ::   rn_ahtrc_0                   !: diffusivity coefficient for passive tracer (m2/s)
   REAL(wp), PUBLIC ::   rn_ahtrb_0                   !: background diffusivity coefficient for passive tracer (m2/s)

   !                                                 !!: ** Treatment of Negative concentrations ( nam_trcrad )
   LOGICAL , PUBLIC ::   ln_trcrad       = .TRUE.     !: flag to artificially correct negative concentrations

   !                                                 !!: ** Vertical diffusion (nam_trczdf) **
   LOGICAL , PUBLIC ::   ln_trczdf_exp = .FALSE.      !: explicit vertical diffusion scheme flag
   INTEGER , PUBLIC ::   nn_trczdf_exp = 3             !: number of sub-time step (explicit time stepping)


#if defined key_trcdmp
   !                                                 !!: ** newtonian damping namelist (nam_trcdmp) **
   INTEGER , PUBLIC ::   nn_hdmp_tr      =   -1       ! = 0/-1/'latitude' for damping over passive tracer
   INTEGER , PUBLIC ::   nn_zdmp_tr      =    0       ! = 0/1/2 flag for damping in the mixed layer
   REAL(wp), PUBLIC ::   rn_surf_tr      =   50.      ! surface time scale for internal damping        [days]
   REAL(wp), PUBLIC ::   rn_bot_tr       =  360.      ! bottom time scale for internal damping         [days]
   REAL(wp), PUBLIC ::   rn_dep_tr       =  800.      ! depth of transition between rn_surf and rn_bot [meters]
   INTEGER , PUBLIC ::   nn_file_tr      =    2       ! = 1 create a damping.coeff NetCDF file 
#endif

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcnam_trp.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_nam_trp
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_nam_trp  ***
      !!                
      !! ** Purpose :   Read Namelist for tracer transport option
      !!----------------------------------------------------------------------
      NAMELIST/namtrc_adv/ ln_trcadv_cen2 , ln_trcadv_tvd   ,    &
         &                 ln_trcadv_muscl, ln_trcadv_muscl2,    &
         &                 ln_trcadv_ubs  , ln_trcadv_qck

      NAMELIST/namtrc_ldf/ ln_trcldf_diff , ln_trcldf_lap  ,     &
         &                 ln_trcldf_bilap, ln_trcldf_level,     &
         &                 ln_trcldf_hor  , ln_trcldf_iso  , rn_ahtrc_0, rn_ahtrb_0
      NAMELIST/namtrc_zdf/ ln_trczdf_exp  , nn_trczdf_exp
      NAMELIST/namtrc_rad/ ln_trcrad
#if defined key_trcdmp
      NAMELIST/namtrc_dmp/ nn_hdmp_tr, nn_zdmp_tr, rn_surf_tr, &
        &                  rn_bot_tr , rn_dep_tr , nn_file_tr
#endif
      !!----------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_nam_trp: read namelist for tracer transport'
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~'

      REWIND ( numnat )               ! Read Namelist namtrc_adv : tracer advection scheme
      READ   ( numnat, namtrc_adv )

      IF(lwp) THEN                    ! Namelist print
         WRITE(numout,*)
         WRITE(numout,*) 'trc_adv_ctl : choice/control of the tracer advection scheme'
         WRITE(numout,*) '~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namtrc_adv : chose a advection scheme for tracers'
         WRITE(numout,*) '      2nd order advection scheme     ln_trcadv_cen2   = ', ln_trcadv_cen2
         WRITE(numout,*) '      TVD advection scheme           ln_trcadv_tvd    = ', ln_trcadv_tvd
         WRITE(numout,*) '      MUSCL  advection scheme        ln_trcadv_muscl  = ', ln_trcadv_muscl
         WRITE(numout,*) '      MUSCL2 advection scheme        ln_trcadv_muscl2 = ', ln_trcadv_muscl2
         WRITE(numout,*) '      UBS    advection scheme        ln_trcadv_ubs    = ', ln_trcadv_ubs
         WRITE(numout,*) '      QUICKEST advection scheme      ln_trcadv_qck    = ', ln_trcadv_qck
      ENDIF
      !
      REWIND( numnat )                ! Namelist namtrc_ldf
      READ  ( numnat, namtrc_ldf )

      IF(lwp) THEN                    ! Namelist print
         WRITE(numout,*)
         WRITE(numout,*) 'trc:ldf_ctl : lateral tracer diffusive operator'
         WRITE(numout,*) '~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namtrc_ldf : set lateral mixing parameters (type, direction, coefficients)'
         WRITE(numout,*) '      perform lateral diffusion or not                   ln_trcldf_diff  = ', ln_trcldf_diff
         WRITE(numout,*) '      laplacian operator                                 ln_trcldf_lap   = ', ln_trcldf_lap
         WRITE(numout,*) '      bilaplacian operator                               ln_trcldf_bilap = ', ln_trcldf_bilap
         WRITE(numout,*) '      iso-level                                          ln_trcldf_level = ', ln_trcldf_level
         WRITE(numout,*) '      horizontal (geopotential)                          ln_trcldf_hor   = ', ln_trcldf_hor
         WRITE(numout,*) '      iso-neutral                                        ln_trcldf_iso   = ', ln_trcldf_iso
         WRITE(numout,*) '      diffusivity coefficient                                 rn_ahtrc_0 = ', rn_ahtrc_0
         WRITE(numout,*) '      background hor. diffusivity                             rn_ahtrb_0 = ', rn_ahtrb_0
      ENDIF

      !                                ! Vertical mixing
      REWIND( numnat )                 !   ! Read namtopzdf namelist
      READ  ( numnat, namtrc_zdf )

      IF(lwp) THEN                     !   ! Control print
         WRITE(numout,*)
         WRITE(numout,*) '   Namelist namtrc_zdf : set vertical diffusion parameters'
         WRITE(numout,*) '      time splitting / backward scheme ln_trczdf_exp = ', ln_trczdf_exp
         WRITE(numout,*) '      number of time step              nn_trczdf_exp = ', nn_trczdf_exp
      ENDIF

      !
      REWIND( numnat )                 !   Read Namelist namtoprad
      READ  ( numnat, namtrc_rad )

      IF(lwp) THEN                     !   ! Control print
         WRITE(numout,*)
         WRITE(numout,*) '   Namelist namtrc_rad : treatment of negative concentrations'
         WRITE(numout,*) '      correct artificially negative concen. or not ln_trcrad = ', ln_trcrad
      ENDIF


# if defined key_trcdmp
      REWIND ( numnat )                  ! Read Namelist namtra_dmp : temperature and salinity damping term
      READ   ( numnat, namtrc_dmp )
      IF( lzoom )   nn_zdmp_trc = 0           ! restoring to climatology at closed north or south boundaries

      IF(lwp) THEN                       ! Namelist print
         WRITE(numout,*)
         WRITE(numout,*) 'trc_dmp : Passive tracers newtonian damping'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*) '   Namelist namtrc_dmp : set damping parameter'
         WRITE(numout,*) '      tracer damping option          nn_hdmp_tr = ', nn_hdmp_tr
         WRITE(numout,*) '      mixed layer damping option     nn_zdmp_tr = ', nn_zdmp_tr, '(zoom: forced to 0)'
         WRITE(numout,*) '      surface time scale (days)      rn_surf_tr = ', rn_surf_tr
         WRITE(numout,*) '      bottom time scale (days)       rn_bot_tr  = ', rn_bot_tr
         WRITE(numout,*) '      depth of transition (meters)   rn_dep_tr  = ', rn_dep_tr
         WRITE(numout,*) '      create a damping.coeff file    nn_file_tr = ', nn_file_tr
      ENDIF
#endif
      !
   END SUBROUTINE trc_nam_trp
   
#else
   !!----------------------------------------------------------------------
   !!   Dummy module :                                         No TOP model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_nam_trp              ! Empty routine
   END SUBROUTINE trc_nam_trp
#endif

  !!======================================================================
END MODULE trcnam_trp
