MODULE updtide
  !!=================================================================================
  !!                       ***  MODULE  updtide  ***
  !! Initialization of tidal forcing
  !! History :  9.0  !  07  (O. Le Galloudec)  Original code
  !!=================================================================================
#if defined key_tide
  !! * Modules used
  USE oce             ! ocean dynamics and tracers variables
  USE dom_oce         ! ocean space and time domain
  USE in_out_manager  ! I/O units
  USE phycst
  USE sbctide
  USE dynspg_oce

  IMPLICIT NONE
  PUBLIC

  !! * Routine accessibility
  PUBLIC upd_tide
  !!---------------------------------------------------------------------------------
  !!   OPA 9.0 , LODYC-IPSL  (2003)
  !!---------------------------------------------------------------------------------

CONTAINS

  SUBROUTINE upd_tide (kt,kit)
    !!----------------------------------------------------------------------
    !!                 ***  ROUTINE upd_tide  ***
    !!----------------------------------------------------------------------      
    !! * Local declarations

    INTEGER, INTENT( in ) ::   kt,kit      ! ocean time-step index
    INTEGER  :: ji,jj,jk
    REAL (wp), DIMENSION(nb_harmo) :: zwt 
    !...............................................................................
    ! Potentiel astronomique
    !...............................................................................

    pot_astro(:,:)=0.e0

    IF (lk_dynspg_ts) THEN
       zwt(:) = omega_tide(:)* ((kt-kt_tide)*rdt + kit*(rdt/REAL(nn_baro,wp)))
    ELSE
       zwt(:) = omega_tide(:)*(kt-kt_tide)*rdt
    ENDIF

    do jk=1,nb_harmo
       do ji=1,jpi
          do jj=1,jpj
             pot_astro(ji,jj)=pot_astro(ji,jj) + (amp_pot(ji,jj,jk)*COS(zwt(jk)+phi_pot(ji,jj,jk)))      
          enddo
       enddo
    enddo

  END SUBROUTINE upd_tide

#else
  !!----------------------------------------------------------------------
  !!   Dummy module :                                        NO TIDE
  !!----------------------------------------------------------------------
CONTAINS
  SUBROUTINE upd_tide( kt,kit )          ! Empty routine
    INTEGER,INTENT (IN) :: kt, kit
    WRITE(*,*) 'upd_tide: You should not have seen this print! error?', kt
  END SUBROUTINE upd_tide

#endif

  !!======================================================================

END MODULE updtide
