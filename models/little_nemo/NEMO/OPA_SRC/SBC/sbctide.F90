MODULE sbctide
  !!=================================================================================
  !!                       ***  MODULE  sbctide  ***
  !! Initialization of tidal forcing
  !! History :  9.0  !  07  (O. Le Galloudec)  Original code
  !!=================================================================================
  !! * Modules used
  USE oce             ! ocean dynamics and tracers variables
  USE dom_oce         ! ocean space and time domain
  USE in_out_manager  ! I/O units
  USE ioipsl          ! NetCDF IPSL library
  USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
  USE phycst
  USE daymod
  USE dynspg_oce
  USE tide_mod
  USE iom

  IMPLICIT NONE
  PUBLIC

  REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: pot_astro
  LOGICAL, PUBLIC :: ln_tide_pot = .false.
#if defined key_tide

  LOGICAL, PUBLIC, PARAMETER ::   lk_tide  = .TRUE.

  REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:) :: omega_tide 

  REAL(wp), ALLOCATABLE, DIMENSION(:) ::  &
       v0tide,      &
       utide,       &
       ftide

  REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: amp_pot,phi_pot

  INTEGER, PUBLIC :: nb_harmo
  INTEGER, PUBLIC, ALLOCATABLE, DIMENSION(:) :: ntide
  INTEGER, PUBLIC :: nn_tide, kt_tide

  !!---------------------------------------------------------------------------------
  !!   OPA 9.0 , LODYC-IPSL  (2003)
  !!---------------------------------------------------------------------------------

CONTAINS

  SUBROUTINE sbc_tide ( kt )
    !!----------------------------------------------------------------------
    !!                 ***  ROUTINE sbc_tide  ***
    !!----------------------------------------------------------------------      
    !! * Arguments
    INTEGER, INTENT( in ) ::   kt     ! ocean time-step
    !! * Local declarations
    INTEGER  :: jk,ji
    CHARACTER(LEN=4), DIMENSION(jpmax_harmo) :: clname
    !!----------------------------------------------------------------------

    NAMELIST/nam_tide/ln_tide_pot,nb_harmo,clname,nn_tide

    IF ( kt == nit000 ) THEN

       IF( .NOT. lk_dynspg_ts  )  CALL ctl_stop( 'STOP', 'sbc_tide : tidal potential use only with time splitting' )

    ! Read Namelist nam_tide

    nn_tide=INT(rday/rdt)

    CALL tide_init_Wave

    REWIND ( numnam )
    READ   ( numnam, nam_tide )

    IF(lwp) THEN
       WRITE(numout,*)
       WRITE(numout,*) 'sbc_tide : Initialization of the tidal components'
       WRITE(numout,*) '~~~~~~~ '
    ENDIF

    IF(lwp) THEN
       WRITE(numout,*) '        Namelist nam_tide'
       WRITE(numout,*) '        Apply astronomical potential : ln_tide_pot =', ln_tide_pot
       WRITE(numout,*) '        nb_harmo    = ', nb_harmo
       CALL flush(numout)
    ENDIF

    ALLOCATE(ntide     (nb_harmo))
    DO jk=1,nb_harmo
       DO ji=1,jpmax_harmo
          IF (TRIM(clname(jk)) .eq. Wave(ji)%cname_tide) THEN
             ntide(jk) = ji
             EXIT
          END IF
       END DO
    END DO
    ALLOCATE(omega_tide(nb_harmo))
    ALLOCATE(v0tide    (nb_harmo))
    ALLOCATE(utide     (nb_harmo))
    ALLOCATE(ftide     (nb_harmo))
    ALLOCATE(amp_pot(jpi,jpj,nb_harmo))
    ALLOCATE(phi_pot(jpi,jpj,nb_harmo))
    ALLOCATE(pot_astro(jpi,jpj))
    ENDIF

    IF ( MOD( kt - 1, nn_tide ) == 0 ) THEN
      kt_tide = kt
      CALL tide_harmo(omega_tide, v0tide, utide, ftide, ntide, nb_harmo)
    ENDIF

    amp_pot(:,:,:) = 0.e0
    phi_pot(:,:,:) = 0.e0
    pot_astro(:,:) = 0.e0

    IF (ln_tide_pot          ) CALL tide_init_potential

  END SUBROUTINE sbc_tide

  SUBROUTINE tide_init_potential
    !!----------------------------------------------------------------------
    !!                 ***  ROUTINE tide_init_potential  ***
    !!----------------------------------------------------------------------
    !! * Local declarations
    INTEGER  :: ji,jj,jk
    REAL(wp) :: zcons,ztmp1,ztmp2,zlat,zlon


    DO jk=1,nb_harmo
       zcons=0.7*Wave(ntide(jk))%equitide*ftide(jk)
       do ji=1,jpi
          do jj=1,jpj
             ztmp1 = amp_pot(ji,jj,jk)*COS(phi_pot(ji,jj,jk))
             ztmp2 = -amp_pot(ji,jj,jk)*SIN(phi_pot(ji,jj,jk))
             zlat = gphit(ji,jj)*rad !! latitude en radian
             zlon = glamt(ji,jj)*rad !! longitude en radian
             ! le potentiel est composé des effets des astres:
             IF (Wave(ntide(jk))%nutide .EQ.1) THEN
                ztmp1= ztmp1 + zcons*(SIN(2.*zlat))*COS(v0tide(jk)+utide(jk)+Wave(ntide(jk))%nutide*zlon)
                ztmp2= ztmp2 - zcons*(SIN(2.*zlat))*SIN(v0tide(jk)+utide(jk)+Wave(ntide(jk))%nutide*zlon)
             ENDIF
             IF (Wave(ntide(jk))%nutide.EQ.2) THEN
                ztmp1= ztmp1 + zcons*(COS(zlat)**2)*COS(v0tide(jk)+utide(jk)+Wave(ntide(jk))%nutide*zlon)
                ztmp2= ztmp2 - zcons*(COS(zlat)**2)*SIN(v0tide(jk)+utide(jk)+Wave(ntide(jk))%nutide*zlon)
             ENDIF
             amp_pot(ji,jj,jk)=SQRT(ztmp1**2+ztmp2**2)
             phi_pot(ji,jj,jk)=ATAN2(-ztmp2/MAX(1.E-10,SQRT(ztmp1**2+ztmp2**2)),ztmp1/MAX(1.E-10,SQRT(ztmp1**2+ztmp2**2)))
          enddo
       enddo
    END DO

  END SUBROUTINE tide_init_potential

#else
  !!----------------------------------------------------------------------
  !!   Default case :   Empty module
  !!----------------------------------------------------------------------
  LOGICAL, PUBLIC, PARAMETER ::   lk_tide = .FALSE.
CONTAINS
  SUBROUTINE sbc_tide( kt )      ! Empty routine
    INTEGER         , INTENT(in) ::   kt         ! ocean time-step
    WRITE(*,*) 'sbc_tide: You should not have seen this print! error?', kt
  END SUBROUTINE sbc_tide
#endif
  !!======================================================================

END MODULE sbctide
