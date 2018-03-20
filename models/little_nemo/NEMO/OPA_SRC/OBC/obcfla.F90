MODULE obcfla
   !!======================================================================
   !!                       ***  MODULE  obcfla  ***
   !! Ocean dynamics:   Flather's algorithm at open boundaries for the time-splitting
   !!======================================================================
   !! History :  2.0  ! 2005-12  (V. Garnier) original code
   !!            3.3  ! 2010-11  (G. Madec) 
   !!            4.0  ! 2011-02  (G. Madec) velocity & ssh passed in argument
   !!----------------------------------------------------------------------
#if defined key_obc   &&   defined key_dynspg_ts
   !!----------------------------------------------------------------------
   !!   'key_obc'          and                      Open Boundary Condition
   !!   'key_dynspg_ts'                    free surface with time splitting
   !!----------------------------------------------------------------------
   !!   obc_fla_ts        : call the subroutine for each open boundary
   !!   obc_fla_ts_east   : Flather on the east  open boundary velocities & ssh
   !!   obc_fla_ts_west   : Flather on the west  open boundary velocities & ssh
   !!   obc_fla_ts_north  : Flather on the north open boundary velocities & ssh
   !!   obc_fla_ts_south  : Flather on the south open boundary velocities & ssh
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers 
   USE dom_oce         ! ocean space and time domain
   USE dynspg_oce      ! surface pressure gradient variables
   USE phycst          ! physical constants
   USE obc_oce         ! ocean open boundary conditions
   USE obcdta          ! ocean open boundary conditions: climatology

   IMPLICIT NONE
   PRIVATE

   PUBLIC   obc_fla_ts   ! routine called in dynspg_ts (free surface time splitting case)

   !!----------------------------------------------------------------------
   !! NEMO/OPA 4.0 , NEMO Consortium (2011)
   !! $Id: obcfla.F90 2715 2011-03-30 15:58:35Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE obc_fla_ts( pua, pva, p_sshn, p_ssha )
      !!----------------------------------------------------------------------
      !!                      SUBROUTINE obc_fla_ts
      !!
      !! ** Purpose :   Apply Flather's algorithm at open boundaries for the 
      !!      time-splitting free surface case (barotropic variables)
      !!
      !!      This routine is called in dynspg_ts.F90 routine 
      !!
      !!      The logical variable lp_obc_east, and/or lp_obc_west, and/or lp_obc_north,
      !!      and/or lp_obc_south allow the user to determine which boundary is an
      !!      open one (must be done in the obc_par.F90 file).
      !!
      !! ** Reference : Flather, R. A., 1976, Mem. Soc. R. Sci. Liege, Ser. 6, 10, 141-164
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   pua   , pva      ! after barotropic velocities
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   p_sshn, p_ssha   ! before, now, after sea surface height
      !!----------------------------------------------------------------------
      !
      IF( lp_obc_east  )   CALL obc_fla_ts_east ( pua, pva, p_sshn, p_ssha ) 
      IF( lp_obc_west  )   CALL obc_fla_ts_west ( pua, pva, p_sshn, p_ssha )
      IF( lp_obc_north )   CALL obc_fla_ts_north( pua, pva, p_sshn, p_ssha )
      IF( lp_obc_south )   CALL obc_fla_ts_south( pua, pva, p_sshn, p_ssha ) 
      !
   END SUBROUTINE obc_fla_ts


   SUBROUTINE obc_fla_ts_east( pua, pva, p_sshn, p_ssha ) 
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE obc_fla_ts_east  ***
      !!
      !! ** Purpose :   Apply Flather's algorithm on east OBC velocities ua, va
      !!              Fix sea surface height (p_sshn) on east open boundary
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   pua   , pva      ! after barotropic velocities
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   p_sshn, p_ssha   ! before, now, after sea surface height
      !
      INTEGER ::   ji, jj   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      DO ji = nie0, nie1
         DO jj = 1, jpj
            pua     (ji,jj) = (  ubtfoe(jj) * hur(ji,jj) + SQRT( grav*hur(ji,jj) )          &
               &            * ( ( p_sshn(ji,jj) + p_sshn(ji+1,jj) ) * 0.5 - sshfoe(jj) )  ) * uemsk(jj,1)
            sshfoe_b(ji,jj) =    sshfoe_b(ji,jj)         + SQRT( grav*hur(ji,jj) )          &
               &            * ( ( p_sshn(ji,jj) + p_sshn(ji+1,jj) ) * 0.5 - sshfoe(jj) )    * uemsk(jj,1)
         END DO
      END DO
      DO ji = nie0p1, nie1p1
         DO jj = 1, jpj
            p_ssha(ji,jj) = p_ssha(ji,jj) * ( 1. - temsk(jj,1) ) + temsk(jj,1) * sshfoe(jj)
            pva   (ji,jj) = vbtfoe(jj) * hvr(ji,jj) * uemsk(jj,1)
         END DO
      END DO
      !
   END SUBROUTINE obc_fla_ts_east


   SUBROUTINE obc_fla_ts_west( pua, pva, p_sshn, p_ssha )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE obc_fla_ts_west  ***
      !! 
      !! ** Purpose :   Apply Flather's algorithm on west OBC velocities ua, va
      !!              Fix sea surface height (p_sshn) on west open boundary
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   pua   , pva      ! after barotropic velocities
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   p_sshn, p_ssha   ! before, now, after sea surface height
      !
      INTEGER ::   ji, jj   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      DO ji = niw0, niw1
         DO jj = 1, jpj
            pua     (ji,jj) = ( ubtfow(jj) * hur(ji,jj) - sqrt( grav * hur(ji,jj) )            &
               &            * ( ( p_sshn(ji,jj) + p_sshn(ji+1,jj) ) * 0.5 - sshfow(jj) ) ) * uwmsk(jj,1)
            pva     (ji,jj) =   vbtfow(jj) * hvr(ji,jj) * uwmsk(jj,1)
            sshfow_b(ji,jj) =   sshfow_b(ji,jj) - SQRT( grav * hur(ji,jj) )                    &
               &            * ( ( p_sshn(ji,jj) + p_sshn(ji+1,jj) ) * 0.5 - sshfow(jj) )   * uwmsk(jj,1)
            p_ssha  (ji,jj) = p_ssha(ji,jj) * ( 1. - twmsk(jj,1) ) + twmsk(jj,1)*sshfow(jj)
         END DO
      END DO
      !
   END SUBROUTINE obc_fla_ts_west


   SUBROUTINE obc_fla_ts_north( pua, pva, p_sshn, p_ssha )
      !!----------------------------------------------------------------------
      !!                     SUBROUTINE obc_fla_ts_north
      !!
      !! ** Purpose :   Apply Flather's algorithm on north OBC velocities ua, va
      !!              Fix sea surface height (p_sshn) on north open boundary
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   pua   , pva      ! after barotropic velocities
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   p_sshn, p_ssha   ! before, now, after sea surface height
      !
      INTEGER ::   ji, jj   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      DO jj = njn0, njn1
         DO ji = 1, jpi
            pva     (ji,jj) = ( vbtfon(ji) * hvr(ji,jj) + sqrt( grav * hvr(ji,jj) )            &
               &                * ( ( p_sshn(ji,jj) + p_sshn(ji,jj+1) ) * 0.5 - sshfon(ji) ) ) * vnmsk(ji,1)
            sshfon_b(ji,jj) =   sshfon_b(ji,jj) + sqrt( grav * hvr(ji,jj) )                    &
               &                * ( ( p_sshn(ji,jj) + p_sshn(ji,jj+1) ) * 0.5 - sshfon(ji) )   * vnmsk(ji,1)
         END DO
      END DO
      DO jj = njn0p1, njn1p1
         DO ji = 1, jpi
            p_ssha(ji,jj) = p_ssha(ji,jj) * ( 1. - tnmsk(ji,1) ) + sshfon(ji) * tnmsk(ji,1)
            pua   (ji,jj) = ubtfon(ji) * hur(ji,jj) * vnmsk(ji,1)
         END DO
      END DO
      !
   END SUBROUTINE obc_fla_ts_north


   SUBROUTINE obc_fla_ts_south( pua, pva, p_sshn, p_ssha )
      !!----------------------------------------------------------------------
      !!                     SUBROUTINE obc_fla_ts_south
      !!
      !! ** Purpose :   Apply Flather's algorithm on south OBC velocities ua, va
      !!              Fix sea surface height (p_sshn) on south open boundary
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   pua   , pva      ! after barotropic velocities
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   p_sshn, p_ssha   ! before, now, after sea surface height
      !
      INTEGER ::   ji, jj   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      DO jj = njs0, njs1
         DO ji = 1, jpi
            pva     (ji,jj) = ( vbtfos(ji) * hvr(ji,jj) - sqrt( grav * hvr(ji,jj) )            &
               &                * ( ( p_sshn(ji,jj) + p_sshn(ji,jj+1) ) * 0.5 - sshfos(ji) ) ) * vsmsk(ji,1)
            pua     (ji,jj) =   ubtfos(ji) * hur(ji,jj) * vsmsk(ji,1)
            sshfos_b(ji,jj) =   sshfos_b(ji,jj) - sqrt( grav * hvr(ji,jj) )                    &
               &                * ( ( p_sshn(ji,jj) + p_sshn(ji,jj+1) ) * 0.5 - sshfos(ji) )   * vsmsk(ji,1)
            p_ssha  (ji,jj) = p_ssha(ji,jj) * (1. - tsmsk(ji,1) ) + tsmsk(ji,1) * sshfos(ji)
         END DO
      END DO
      !
   END SUBROUTINE obc_fla_ts_south
   
#else
   !!----------------------------------------------------------------------
   !!   Dummy module :                             No OBC or time-splitting
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE obc_fla_ts( pua, pva, p_sshn, p_ssha )
      REAL, DIMENSION(:,:)::   pua, pva, p_sshn, p_ssha
      WRITE(*,*) 'obc_fla_ts: You should not have seen this print! error?', pua(1,1), pva(1,1), p_sshn(1,1), p_ssha(1,1)
   END SUBROUTINE obc_fla_ts
#endif
   !!======================================================================
END MODULE obcfla
