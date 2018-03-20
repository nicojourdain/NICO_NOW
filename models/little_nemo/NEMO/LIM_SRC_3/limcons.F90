MODULE limcons
   !!======================================================================
   !!                   ***  MODULE  limcons  ***
   !! LIM-3 Sea Ice :   conservation check
   !!======================================================================
   !! History :   -   ! Original code from William H. Lipscomb, LANL
   !!            3.0  ! 2004-06  (M. Vancoppenolle)   Energy Conservation 
   !!            4.0  ! 2011-02  (G. Madec)  add mpp considerations
   !!----------------------------------------------------------------------
#if defined key_lim3
   !!----------------------------------------------------------------------
   !!   'key_lim3' :                                   LIM3 sea-ice model
   !!----------------------------------------------------------------------
   !!    lim_cons   :   checks whether energy, mass and salt are conserved 
   !!----------------------------------------------------------------------
   USE par_ice          ! LIM-3 parameter
   USE ice              ! LIM-3 variables
   USE dom_ice          ! LIM-3 domain
   USE dom_oce          ! ocean domain
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_column_sum
   PUBLIC   lim_column_sum_energy
   PUBLIC   lim_cons_check

   !!----------------------------------------------------------------------
   !! NEMO/LIM3 4.0 , UCL - NEMO Consortium (2011)
   !! $Id: limcons.F90 2777 2011-06-07 09:55:02Z smasson $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE lim_column_sum( ksum, pin, pout )
      !!-------------------------------------------------------------------
      !!               ***  ROUTINE lim_column_sum ***
      !!
      !! ** Purpose : Compute the sum of xin over nsum categories
      !!
      !! ** Method  : Arithmetics
      !!
      !! ** Action  : Gets xin(ji,jj,jl) and computes xout(ji,jj)
      !!---------------------------------------------------------------------
      INTEGER                   , INTENT(in   ) ::   ksum   ! number of categories/layers
      REAL(wp), DIMENSION(:,:,:), INTENT(in   ) ::   pin    ! input field
      REAL(wp), DIMENSION(:,:)  , INTENT(  out) ::   pout   ! output field
      !
      INTEGER ::   jl   ! dummy loop indices
      !!---------------------------------------------------------------------
      !
      pout(:,:) = pin(:,:,1)
      DO jl = 2, ksum
         pout(:,:) = pout(:,:) + pin(:,:,jl)
      END DO
      !
   END SUBROUTINE lim_column_sum


   SUBROUTINE lim_column_sum_energy( ksum, klay, pin, pout)
      !!-------------------------------------------------------------------
      !!               ***  ROUTINE lim_column_sum_energy ***
      !!
      !! ** Purpose : Compute the sum of xin over nsum categories
      !!              and nlay layers
      !!
      !! ** Method  : Arithmetics
      !!---------------------------------------------------------------------
      INTEGER                               , INTENT(in   ) ::   ksum   !: number of categories
      INTEGER                               , INTENT(in   ) ::   klay   !: number of vertical layers
      REAL(wp), DIMENSION(jpi,jpj,jkmax,jpl), INTENT(in   ) ::   pin   !: input field
      REAL(wp), DIMENSION(jpi,jpj)          , INTENT(  out) ::   pout   !: output field
      !
      INTEGER ::   jk, jl   ! dummy loop indices
      !!---------------------------------------------------------------------
      !
      pout(:,:) = 0._wp
      DO jl = 1, ksum
         DO jk = 2, klay 
            pout(:,:) = pout(:,:) + pin(:,:,jk,jl)
         END DO
      END DO
      !
   END SUBROUTINE lim_column_sum_energy


   SUBROUTINE lim_cons_check( px1, px2, pmax_err, cd_fieldid )
      !!-------------------------------------------------------------------
      !!               ***  ROUTINE lim_cons_check ***
      !!
      !! ** Purpose : Test the conservation of a certain variable
      !!              For each physical grid cell, check that initial 
      !!              and final values
      !!              of a conserved field are equal to within a small value.
      !!
      !! ** Method  :
      !!---------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(in   ) ::   px1          !: initial field
      REAL(wp), DIMENSION(:,:), INTENT(in   ) ::   px2          !: final field
      REAL(wp)                , INTENT(in   ) ::   pmax_err     !: max allowed error
      CHARACTER(len=15)       , INTENT(in   ) ::   cd_fieldid   !: field identifyer
      !
      INTEGER  ::   ji, jj          ! dummy loop indices
      INTEGER  ::   inb_error       ! number of g.c where there is a cons. error
      LOGICAL  ::   llconserv_err   ! = .true. if conservation check failed
      REAL(wp) ::   zmean_error     ! mean error on error points
      !!---------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*) ' lim_cons_check '
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~ '

      llconserv_err = .FALSE.
      inb_error     = 0
      zmean_error   = 0._wp
      IF( MAXVAL( px2(:,:) - px1(:,:) ) > pmax_err )   llconserv_err = .TRUE.

      IF( llconserv_err ) THEN
         DO jj = 1, jpj 
            DO ji = 1, jpi
               IF( ABS( px2(ji,jj) - px1(ji,jj) ) > pmax_err ) THEN
                  inb_error   = inb_error + 1
                  zmean_error = zmean_error + ABS( px2(ji,jj) - px1(ji,jj) )
                  !
                  IF(lwp) THEN
                     WRITE (numout,*) ' ALERTE 99 '
                     WRITE (numout,*) ' Conservation error: ', cd_fieldid
                     WRITE (numout,*) ' Point             : ', ji, jj 
                     WRITE (numout,*) ' lat, lon          : ', gphit(ji,jj), glamt(ji,jj)
                     WRITE (numout,*) ' Initial value     : ', px1(ji,jj)
                     WRITE (numout,*) ' Final value       : ', px2(ji,jj)
                     WRITE (numout,*) ' Difference        : ', px2(ji,jj) - px1(ji,jj)
                  ENDIF
               ENDIF
            END DO
         END DO
         !
      ENDIF
      IF(lk_mpp)   CALL mpp_sum( inb_error   )
      IF(lk_mpp)   CALL mpp_sum( zmean_error )
      !
      IF( inb_error > 0 .AND. lwp ) THEN
         zmean_error = zmean_error / REAL( inb_error, wp )
         WRITE(numout,*) ' Conservation check for : ', cd_fieldid
         WRITE(numout,*) ' Number of error points : ', inb_error
         WRITE(numout,*) ' Mean error on these pts: ', zmean_error
      ENDIF
      !
   END SUBROUTINE lim_cons_check

#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty module            NO LIM sea-ice model
   !!----------------------------------------------------------------------
#endif
   !!======================================================================
END MODULE limcons
