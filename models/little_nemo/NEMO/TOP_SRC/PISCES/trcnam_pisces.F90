MODULE trcnam_pisces
   !!======================================================================
   !!                      ***  MODULE trcnam_lobster  ***
   !! TOP :   initialisation of some run parameters for PISCES bio-model
   !!======================================================================
   !! History :    -   !  1999-10 (M.A. Foujols, M. Levy) original code
   !!              -   !  2000-01 (L. Bopp) hamocc3, p3zd
   !!             1.0  !  2003-08 (C. Ethe)  module F90
   !!             2.0  !  2007-12  (C. Ethe, G. Madec) from trcnam.pisces.h90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'   :                                   PISCES bio-model
   !!----------------------------------------------------------------------
   !! trc_nam_pisces       : PISCES model namelist read
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE par_trc         ! TOP parameters
   USE trc             ! TOP variables
   USE sms_pisces      ! sms trends
   USE iom             ! I/O manager


   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_nam_pisces   ! called by trcnam.F90 module


   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcnam_pisces.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_nam_pisces
      !!----------------------------------------------------------------------
      !!                     ***  trc_nam_pisces  ***  
      !!
      !! ** Purpose :   read PISCES namelist
      !!
      !! ** input   :   file 'namelist.trc.sms' containing the following
      !!             namelist: natext, natbio, natsms
      !!                       natkriest ("key_kriest")
      !!----------------------------------------------------------------------
      !!
      INTEGER :: jl, jn
      TYPE(DIAG), DIMENSION(jp_pisces_2d) :: pisdia2d
      TYPE(DIAG), DIMENSION(jp_pisces_3d) :: pisdia3d
      !!
      NAMELIST/nampisbio/ nrdttrc, wsbio, xkmort, ferat3, wsbio2
#if defined key_kriest
      NAMELIST/nampiskrp/ xkr_eta, xkr_zeta, xkr_mass_min, xkr_mass_max
#endif
      NAMELIST/nampisdia/ pisdia3d, pisdia2d     ! additional diagnostics
      NAMELIST/nampisdmp/ ln_pisdmp, nn_pisdmp, ln_pisclo

      !!----------------------------------------------------------------------

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_nam_pisces : read PISCES namelists'
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'


      !                               ! Open the namelist file
      !                               ! ----------------------
      CALL ctl_opn( numnatp, 'namelist_pisces', 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )

      REWIND( numnatp )                    
      READ  ( numnatp, nampisbio )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' Namelist : nampisbio'
         WRITE(numout,*) '    frequence pour la biologie                nrdttrc   =', nrdttrc
         WRITE(numout,*) '    POC sinking speed                         wsbio     =', wsbio
         WRITE(numout,*) '    half saturation constant for mortality    xkmort    =', xkmort
         WRITE(numout,*) '    Fe/C in zooplankton                       ferat3    =', ferat3
         WRITE(numout,*) '    Big particles sinking speed               wsbio2    =', wsbio2
      ENDIF

#if defined key_kriest

      !                               ! nampiskrp : kriest parameters
      !                               ! -----------------------------
      xkr_eta      = 0.62        
      xkr_zeta     = 1.62        
      xkr_mass_min = 0.0002     
      xkr_mass_max = 1.      

      REWIND( numnatp )                     ! read natkriest
      READ  ( numnatp, nampiskrp )

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) ' Namelist : nampiskrp'
         WRITE(numout,*) '    Sinking  exponent                        xkr_eta      = ', xkr_eta
         WRITE(numout,*) '    N content exponent                       xkr_zeta     = ', xkr_zeta
         WRITE(numout,*) '    Minimum mass for Aggregates              xkr_mass_min = ', xkr_mass_min
         WRITE(numout,*) '    Maximum mass for Aggregates              xkr_mass_max = ', xkr_mass_max
         WRITE(numout,*)
     ENDIF


     ! Computation of some variables
     xkr_massp = 5.7E-6 * 7.6 * xkr_mass_min**xkr_zeta

#endif
      !
      IF( .NOT.lk_iomput .AND. ln_diatrc ) THEN
         !
         ! Namelist nampisdia
         ! -------------------
         DO jl = 1, jp_pisces_2d
            WRITE(pisdia2d(jl)%sname,'("2D_",I1)') jl                      ! short name
            WRITE(pisdia2d(jl)%lname,'("2D DIAGNOSTIC NUMBER ",I2)') jl    ! long name
            pisdia2d(jl)%units = ' '                                       ! units
         END DO
         !                                 ! 3D output arrays
         DO jl = 1, jp_pisces_3d
            WRITE(pisdia3d(jl)%sname,'("3D_",I1)') jl                      ! short name
            WRITE(pisdia3d(jl)%lname,'("3D DIAGNOSTIC NUMBER ",I2)') jl    ! long name
            pisdia3d(jl)%units = ' '                                       ! units
         END DO

         REWIND( numnatp )               ! 
         READ  ( numnatp, nampisdia )

         DO jl = 1, jp_pisces_2d
            jn = jp_pcs0_2d + jl - 1
            ctrc2d(jn) = pisdia2d(jl)%sname
            ctrc2l(jn) = pisdia2d(jl)%lname
            ctrc2u(jn) = pisdia2d(jl)%units
         END DO

         DO jl = 1, jp_pisces_3d
            jn = jp_pcs0_3d + jl - 1
            ctrc3d(jn) = pisdia3d(jl)%sname
            ctrc3l(jn) = pisdia3d(jl)%lname
            ctrc3u(jn) = pisdia3d(jl)%units
         END DO

         IF(lwp) THEN                   ! control print
            WRITE(numout,*)
            WRITE(numout,*) ' Namelist : natadd'
            DO jl = 1, jp_pisces_3d
               jn = jp_pcs0_3d + jl - 1
               WRITE(numout,*) '  3d diag nb : ', jn, '    short name : ', ctrc3d(jn), &
                 &             '  long name  : ', ctrc3l(jn), '   unit : ', ctrc3u(jn)
            END DO
            WRITE(numout,*) ' '

            DO jl = 1, jp_pisces_2d
               jn = jp_pcs0_2d + jl - 1
               WRITE(numout,*) '  2d diag nb : ', jn, '    short name : ', ctrc2d(jn), &
                 &             '  long name  : ', ctrc2l(jn), '   unit : ', ctrc2u(jn)
            END DO
            WRITE(numout,*) ' '
         ENDIF
         !
      ENDIF

      REWIND( numnatp )
      READ  ( numnatp, nampisdmp )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*)
         WRITE(numout,*) ' Namelist : nampisdmp'
         WRITE(numout,*) '    Relaxation of tracer to glodap mean value             ln_pisdmp      =', ln_pisdmp
         WRITE(numout,*) '    Frequency of Relaxation                               nn_pisdmp      =', nn_pisdmp
         WRITE(numout,*) '    Restoring of tracer to initial value  on closed seas  ln_pisclo      =', ln_pisclo
         WRITE(numout,*) ' '
      ENDIF

   END SUBROUTINE trc_nam_pisces

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                   No PISCES bio-model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_nam_pisces                      ! Empty routine
   END  SUBROUTINE  trc_nam_pisces
#endif  

   !!======================================================================
END MODULE trcnam_pisces
