MODULE obcdyn_bt
   !!======================================================================
   !!                       ***  MODULE  obcdyn_bt  ***
   !! Ocean dynamics:   Radiation/prescription of sea surface heights on each open boundary
   !!======================================================================
   !! History :  1.0  ! 2005-12  (V. Garnier) original code
   !!----------------------------------------------------------------------
#if ( defined key_dynspg_ts || defined key_dynspg_exp ) && defined key_obc
   !!----------------------------------------------------------------------
   !!   'key_dynspg_ts'     OR                   time spliting free surface
   !!   'key_dynspg_exp'    AND                       explicit free surface
   !!   'key_obc'                                   Open Boundary Condition
   !!----------------------------------------------------------------------
   !!   obc_dyn_bt        : call the subroutine for each open boundary
   !!   obc_dyn_bt_east   : Flather's algorithm at the east open boundary
   !!   obc_dyn_bt_west   : Flather's algorithm at the west open boundary
   !!   obc_dyn_bt_north  : Flather's algorithm at the north open boundary
   !!   obc_dyn_bt_south  : Flather's algorithm at the south open boundary
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers 
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE obc_oce         ! ocean open boundary conditions
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp         ! distributed memory computing
   USE obcdta          ! ocean open boundary conditions
   USE in_out_manager  ! I/O manager
   USE dynspg_oce      ! surface pressure gradient     (free surface with time-splitting)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   obc_dyn_bt  ! routine called in dynnxt (explicit free surface case)

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: obcdyn_bt.F90 2715 2011-03-30 15:58:35Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE obc_dyn_bt( kt )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE obc_dyn_bt  ***
      !!
      !! ** Purpose :   Apply Flather's algorithm at open boundaries for the explicit
      !!              free surface case and free surface case with time-splitting
      !!
      !!      This routine is called in dynnxt.F routine and updates ua, va and sshn. 
      !!
      !!      The logical variable lp_obc_east, and/or lp_obc_west, and/or lp_obc_north, 
      !!      and/or lp_obc_south allow the user to determine which boundary is an
      !!      open one (must be done in the param_obc.h90 file).
      !!
      !! Reference :   Flather, R. A., 1976, Mem. Soc. R. Sci. Liege, Ser. 6, 10, 141-164
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt
      !!----------------------------------------------------------------------

      IF( lp_obc_east  )   CALL obc_dyn_bt_east 
      IF( lp_obc_west  )   CALL obc_dyn_bt_west 
      IF( lp_obc_north )   CALL obc_dyn_bt_north
      IF( lp_obc_south )   CALL obc_dyn_bt_south

      IF( lk_mpp ) THEN
         IF( kt >= nit000+3 .AND. ln_rstart ) THEN
            CALL lbc_lnk( sshb, 'T',  1. )
            CALL lbc_lnk( ub  , 'U', -1. )
            CALL lbc_lnk( vb  , 'V', -1. )
         END IF
         CALL lbc_lnk( sshn, 'T',  1. )
         CALL lbc_lnk( ua  , 'U', -1. )
         CALL lbc_lnk( va  , 'V', -1. )
      ENDIF

   END SUBROUTINE obc_dyn_bt

# if defined key_dynspg_exp

   SUBROUTINE obc_dyn_bt_east 
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE obc_dyn_bt_east  ***
      !!              
      !! ** Purpose :
      !!      Apply Flather's algorithm on east OBC velocities ua, va 
      !!      Fix sea surface height (sshn) on east open boundary
      !!      The logical lfbceast must be .TRUE.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------

      DO ji = nie0, nie1
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               ua(ji,jj,jk) = ua(ji,jj,jk) + sqrt( grav*hur (ji,jj) )               &
                  &                      * ( ( sshn(ji,jj) + sshn(ji+1,jj) ) * 0.5  &
                  &                          - sshfoe(jj) ) * uemsk(jj,jk)
            END DO
         END DO
      END DO
      DO ji = nie0p1, nie1p1
         DO jj = 1, jpj
            sshn(ji,jj) = sshn(ji,jj) * (1.-temsk(jj,1)) + temsk(jj,1)*sshfoe(jj)
         END DO
      END DO

   END SUBROUTINE obc_dyn_bt_east


   SUBROUTINE obc_dyn_bt_west 
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE obc_dyn_bt_west  ***
      !!                  
      !! ** Purpose :
      !!      Apply Flather algorithm on west OBC velocities ua, va
      !!      Fix sea surface height (sshn) on west open boundary
      !!      The logical lfbcwest must be .TRUE.
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      DO ji = niw0, niw1
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               ua(ji,jj,jk) = ua(ji,jj,jk) - sqrt( grav*hur (ji,jj) )               &
                  &                      * ( ( sshn(ji,jj) + sshn(ji+1,jj) ) * 0.5  &
                  &                          - sshfow(jj) ) * uwmsk(jj,jk)
            END DO
         END DO
         DO jj = 1, jpj
            sshn(ji,jj) = sshn(ji,jj) * (1.-twmsk(jj,1)) + twmsk(jj,1)*sshfow(jj)
         END DO
      END DO
      !
   END SUBROUTINE obc_dyn_bt_west


   SUBROUTINE obc_dyn_bt_north 
      !!------------------------------------------------------------------------------
      !!                ***  SUBROUTINE obc_dyn_bt_north  ***
      !!
      !! ** Purpose :
      !!      Apply Flather algorithm on north OBC velocities ua, va
      !!      Fix sea surface height (sshn) on north open boundary
      !!      The logical lfbcnorth must be .TRUE.
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      DO jj = njn0, njn1
         DO jk = 1, jpkm1
            DO ji = 1, jpi
               va(ji,jj,jk) = va(ji,jj,jk) + sqrt( grav*hvr (ji,jj) )               &
                  &                      * ( ( sshn(ji,jj) + sshn(ji,jj+1) ) * 0.5  &
                  &                          - sshfon(ji) ) * vnmsk(ji,jk)
            END DO
         END DO
      END DO
      DO jj = njn0p1, njn1p1
         DO ji = 1, jpi
            sshn(ji,jj)= sshn(ji,jj) * (1.-tnmsk(ji,1)) + sshfon(ji)*tnmsk(ji,1)
         END DO
      END DO
      !
   END SUBROUTINE obc_dyn_bt_north


   SUBROUTINE obc_dyn_bt_south 
      !!----------------------------------------------------------------------
      !!                ***  SUBROUTINE obc_dyn_bt_south  ***
      !!                    
      !! ** Purpose :
      !!      Apply Flather algorithm on south OBC velocities ua, va
      !!      Fix sea surface height (sshn) on south open boundary
      !!      The logical lfbcsouth must be .TRUE.
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      DO jj = njs0, njs1
         DO jk = 1, jpkm1
            DO ji = 1, jpi
               va(ji,jj,jk) = va(ji,jj,jk) - sqrt( grav*hvr (ji,jj) )               &
                  &                       * ( ( sshn(ji,jj) + sshn(ji,jj+1) ) * 0.5 &
                  &                           - sshfos(ji) ) * vsmsk(ji,jk)
            END DO
         END DO
         DO ji = 1, jpi
            sshn(ji,jj)= sshn(ji,jj) * (1.-tsmsk(ji,1)) + tsmsk(ji,1) * sshfos(ji)
         END DO
      END DO
      !
   END SUBROUTINE obc_dyn_bt_south

# elif defined key_dynspg_ts

   SUBROUTINE obc_dyn_bt_east 
      !!------------------------------------------------------------------------------
      !!                  ***  SUBROUTINE obc_dyn_bt_east  ***
      !!
      !! ** Purpose :
      !!      Apply Flather's algorithm on east OBC velocities ua, va
      !!      Fix sea surface height (sshn) on east open boundary
      !!      The logical lfbceast must be .TRUE.
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      DO ji = nie0, nie1
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               ua(ji,jj,jk) = ( ua(ji,jj,jk) + sshfoe_b(ji,jj) ) * uemsk(jj,jk)
            END DO
         END DO
      END DO
      DO ji = nie0p1, nie1p1
         DO jj = 1, jpj
            sshn(ji,jj) = sshn(ji,jj) * (1.-temsk(jj,1)) + temsk(jj,1)*sshn_b(ji,jj)
         END DO
      END DO
      !
   END SUBROUTINE obc_dyn_bt_east


   SUBROUTINE obc_dyn_bt_west 
      !!---------------------------------------------------------------------
      !!                  ***  SUBROUTINE obc_dyn_bt_west  ***
      !!
      !! ** Purpose :   Apply Flather algorithm on west OBC velocities ua, va
      !!      Fix sea surface height (sshn) on west open boundary
      !!      The logical lfbcwest must be .TRUE.
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      DO ji = niw0, niw1
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               ua(ji,jj,jk) = ( ua(ji,jj,jk) + sshfow_b(ji,jj) ) * uwmsk(jj,jk)
            END DO
         END DO
         DO jj = 1, jpj
            sshn(ji,jj) = sshn(ji,jj) * (1.-twmsk(jj,1)) + twmsk(jj,1)*sshn_b(ji,jj)
         END DO
      END DO
      !
   END SUBROUTINE obc_dyn_bt_west


   SUBROUTINE obc_dyn_bt_north 
      !!------------------------------------------------------------------------------
      !!                ***  SUBROUTINE obc_dyn_bt_north  ***
      !!                
      !! ** Purpose :
      !!      Apply Flather algorithm on north OBC velocities ua, va
      !!      Fix sea surface height (sshn) on north open boundary
      !!      The logical lfbcnorth must be .TRUE.
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      DO jj = njn0, njn1
         DO jk = 1, jpkm1
            DO ji = 1, jpi
               va(ji,jj,jk) = ( va(ji,jj,jk) + sshfon_b(ji,jj) ) * vnmsk(jj,jk)
            END DO
         END DO
      END DO
      DO jj = njn0p1, njn1p1
         DO ji = 1, jpi
            sshn(ji,jj)= sshn(ji,jj) * (1.-tnmsk(ji,1)) + sshn_b(ji,jj)*tnmsk(ji,1)
         END DO
      END DO
      !
   END SUBROUTINE obc_dyn_bt_north


   SUBROUTINE obc_dyn_bt_south 
      !!------------------------------------------------------------------------------
      !!                ***  SUBROUTINE obc_dyn_bt_south  ***
      !!                  
      !! ** Purpose :
      !!      Apply Flather algorithm on south OBC velocities ua, va
      !!      Fix sea surface height (sshn) on south open boundary
      !!      The logical lfbcsouth must be .TRUE.
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      DO jj = njs0, njs1
         DO jk = 1, jpkm1
            DO ji = 1, jpi
               va(ji,jj,jk) = ( va(ji,jj,jk) + sshfos_b(ji,jj) ) * vsmsk(jj,jk)
            END DO
         END DO
         DO ji = 1, jpi
            sshn(ji,jj)= sshn(ji,jj) * (1.-tsmsk(ji,1)) + tsmsk(ji,1) * sshn_b(ji,jj)
         END DO
      END DO
      !
   END SUBROUTINE obc_dyn_bt_south

# endif

#else
   !!----------------------------------------------------------------------
   !!   Default option       No Open Boundaries or not explicit fre surface
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE obc_dyn_bt      ! Dummy routine
   END SUBROUTINE obc_dyn_bt
#endif

   !!======================================================================
END MODULE obcdyn_bt
