MODULE trcini_c14b
   !!======================================================================
   !!                         ***  MODULE trcini_c14b  ***
   !! TOP :   initialisation of the C14 bomb tracer
   !!======================================================================
   !! History :  1.0  ! 2005-10  (Z. Lachkar) Original code
   !!            2.0  ! 2007-12  (C. Ethe) 
   !!----------------------------------------------------------------------
#if defined key_c14b
   !!----------------------------------------------------------------------
   !!   'key_c14b'                                          C14 bomb tracer
   !!----------------------------------------------------------------------
   !! trc_ini_c14b      : C14 model initialisation
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE par_trc         ! TOP parameters
   USE trc             ! TOP variables
   USE trcsms_c14b     ! C14 sms trends

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_ini_c14b   ! called by trcini.F90 module

   !                             ! With respect to data file !!
   INTEGER  ::   jpybeg = 1765   ! starting year for C14
   INTEGER  ::   jpyend = 2002   ! ending year for C14
   INTEGER  ::   nrec            ! number of year in CO2 Concentrations file
   INTEGER  ::   nmaxrec 
   INTEGER  ::   inum1, inum2    ! unit number

   REAL(wp) ::   ys40 = -40.     ! 40 degrees south
   REAL(wp) ::   ys20 = -20.     ! 20 degrees south
   REAL(wp) ::   yn20 =  20.     ! 20 degrees north
   REAL(wp) ::   yn40 =  40.     ! 40 degrees north

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcini_c14b.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_ini_c14b
      !!-------------------------------------------------------------------
      !!                     ***  trc_ini_c14b  ***  
      !!
      !! ** Purpose :   initialization for C14 model
      !!
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, jl, jm
      REAL(wp) ::   zyear
      !!----------------------------------------------------------------------

      !                     ! Allocate C14b arrays
      IF( trc_sms_c14b_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'trc_ini_c14b : unable to allocate C14b arrays' )

      CALL trc_ctl_c14b     !  Control consitency

      IF(lwp) WRITE(numout,*) ''
      IF(lwp) WRITE(numout,*) ' trc_ini_c14b: initialisation of Bomb C14 chemical model'
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~'


      ! Initialization of boundaries conditions
      ! --------------------------------------- 
      qtr_c14(:,:) = 0._wp
      
      ! Initialization of qint in case of  no restart 
      !----------------------------------------------
      IF( .NOT. ln_rsttr ) THEN    
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'Initialization de qint ; No restart : qint equal zero '
         ENDIF
         trn     (:,:,:,jpc14) = 0._wp
         qint_c14(:,:        ) = 0._wp
      ENDIF


      ! Read CO2 atmospheric concentrations file...
      ! read CO2 data from year jpybeg to year jpyend
      !------------------------------------------------

      nrec    = ( jpyend - jpybeg + 1 )     ! number of year in CO2 Concentrations file
      nmaxrec = 2 * nrec

      IF(lwp) WRITE(numout,*) 'Read CO2 atmospheric concentrations file '
  
      CALL ctl_opn( inum1, 'splco2.dat', 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
      REWIND(inum1)
      
      DO jm = 1, 5        ! Skip over 1st six descriptor lines
         READ(inum1,'(1x)')
      END DO

      ! get  CO2 data
      DO jm = 1, nmaxrec
         READ(inum1, *)  zyear, spco2(jm)
         IF (lwp) WRITE(numout, '(f7.1,f9.4)')  zyear, spco2(jm)
      END DO
      WRITE(numout,*)
      CLOSE(inum1)

      IF (lwp) WRITE(numout,*) 'Read C-14 atmospheric concentrations file '

      CALL ctl_opn( inum2, 'atmc14.dat', 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
      REWIND(inum2)

      ! Skip over 1st descriptor line
      READ(inum2, '(1x)')

      ! READ FILE
      DO jm = 1, nrec
         READ(inum2,*) zyear, bomb(jm,1), bomb(jm,2), bomb(jm,3)
         IF (lwp) WRITE(numout, '(f7.1, 3f9.4)') zyear, bomb(jm,1), bomb(jm,2), bomb(jm,3)
      END DO
      CLOSE(inum2)

      ! Conversion unit : Now atm units are in real C-14 [per mil]
      ! C-14(Orr) = C-14(per mil)/10.0
       DO jm = 1, nrec
         bomb(jm,1) = ( bomb(jm,1 ) + 17.40 ) * 0.1
         bomb(jm,2) = ( bomb(jm,2 ) + 10.40 ) * 0.1
         bomb(jm,3) = ( bomb(jm,3 ) + 14.65 ) * 0.1
       END DO

       ! Linear  interpolation of the C-14 source fonction
       ! in linear latitude band  (20N,40N) and (20S,40S)
       !------------------------------------------------------
       DO jj = 1 , jpj
          DO ji = 1 , jpi
            IF( gphit(ji,jj) >= yn40 ) THEN
                 fareaz(ji,jj,1) = 0.
                 fareaz(ji,jj,2) = 0.
                 fareaz(ji,jj,3) = 1.
            ELSE IF( gphit(ji,jj ) <= ys40) THEN
                 fareaz(ji,jj,1) = 1.
                 fareaz(ji,jj,2) = 0.
                 fareaz(ji,jj,3) = 0.
            ELSE IF( gphit(ji,jj) >= yn20 ) THEN
                 fareaz(ji,jj,1) = 0.
                 fareaz(ji,jj,2) = 2. * ( 1. - gphit(ji,jj) / yn40 )
                 fareaz(ji,jj,3) = 2. * gphit(ji,jj) / yn40 - 1.
            ELSE IF( gphit(ji,jj) <= ys20 ) THEN
                 fareaz(ji,jj,1) = 2. * gphit(ji,jj) / ys40 - 1.
                 fareaz(ji,jj,2) = 2. * ( 1. - gphit(ji,jj) / ys40 )
                 fareaz(ji,jj,3) = 0.
            ELSE
                 fareaz(ji,jj,1) = 0.
                 fareaz(ji,jj,2) = 1.
                 fareaz(ji,jj,3) = 0.
            ENDIF
         END DO
      END DO
      !
      IF(lwp) WRITE(numout,*) 'Initialization of C14 bomb tracer done'
      IF(lwp) WRITE(numout,*) ' '
      !
   END SUBROUTINE trc_ini_c14b


   SUBROUTINE trc_ctl_c14b
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_ctl_c14b  ***
      !!
      !! ** Purpose :   control the cpp options, namelist and files 
      !!----------------------------------------------------------------------

      IF(lwp) THEN
          WRITE(numout,*) ' C14 bomb Model '
          WRITE(numout,*) ' '
      ENDIF

      ! Check number of tracers
      ! -----------------------   
      IF( jp_c14b > 1)   CALL ctl_stop( ' Change jp_c14b to be equal 1 in par_c14b.F90' )

      ! Check tracer names
      ! ------------------
      IF( ctrcnm(jpc14) /= 'C14B' ) THEN
          ctrcnm(jpc14)  = 'C14B'
          ctrcln(jpc14)  = 'Bomb C14 concentration'
      ENDIF

      IF(lwp) THEN
         CALL ctl_warn( ' we force tracer names' )
         WRITE(numout,*) ' tracer nb: ',jpc14,' name = ',ctrcnm(jpc14), ctrcln(jpc14)
         WRITE(numout,*) ' '
      ENDIF

      ! Check tracer units
      ! ------------------
      IF( ctrcun(jpc14) /= 'ration' ) THEN
          ctrcun(jpc14)  = 'ration'
          IF(lwp) THEN
             CALL ctl_warn( ' we force tracer unit' )
             WRITE(numout,*) ' tracer  ',ctrcnm(jpc14), 'UNIT= ',ctrcun(jpc14)
             WRITE(numout,*) ' '
          ENDIF
       ENDIF
      !
   END SUBROUTINE trc_ctl_c14b
   
#else
   !!----------------------------------------------------------------------
   !!   Dummy module                                    No C14 bomb tracer
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_ini_c14b             ! Empty routine
   END SUBROUTINE trc_ini_c14b
#endif

   !!======================================================================
END MODULE trcini_c14b
