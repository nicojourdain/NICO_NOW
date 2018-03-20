MODULE trcopt
   !!======================================================================
   !!                         ***  MODULE trcopt  ***
   !! TOP :   LOBSTER Compute the light availability in the water column
   !!======================================================================
   !! History :    -   !  1995-05  (M. Levy) Original code
   !!              -   !  1999-09  (J.-M. Andre, M. Levy) 
   !!              -   !  1999-11  (C. Menkes, M.-A. Foujols) itabe initial
   !!              -   !  2000-02  (M.A. Foujols) change x**y par exp(y*log(x))
   !!   NEMO      2.0  !  2007-12  (C. Deltel, G. Madec)  F90
   !!             3.2  !  2009-04  (C. Ethe, G. Madec)  minor optimisation + style
   !!----------------------------------------------------------------------
#if defined key_lobster
   !!----------------------------------------------------------------------
   !!   'key_lobster'                                     LOBSTER bio-model
   !!----------------------------------------------------------------------
   !!   trc_opt        :   Compute the light availability in the water column
   !!----------------------------------------------------------------------
   USE oce_trc         !
   USE trc
   USE sms_lobster
   USE prtctl_trc      ! Print control for debbuging

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_opt   ! called in trcprg.F90

   !!* Substitution
#  include "top_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcopt.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_opt( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_opt  ***
      !!
      !! ** Purpose :   computes the light propagation in the water column
      !!              and the euphotic layer depth
      !!
      !! ** Method  :   local par is computed in w layers using light propagation
      !!              mean par in t layers are computed by integration
      !!
!!gm please remplace the '???' by true comments
      !! ** Action  :   xpar   ???
      !!                neln   ???
      !!                xze    ???
      !!---------------------------------------------------------------------
      !!
      INTEGER, INTENT( in ) ::   kt   ! index of the time stepping
      !!
      INTEGER  ::   ji, jj, jk          ! dummy loop indices
      CHARACTER (len=25) ::   charout   ! temporary character
      REAL(wp) ::   zpig                ! log of the total pigment
      REAL(wp) ::   zkr, zkg            ! total absorption coefficient in red and green
      REAL(wp) ::   zcoef               ! temporary scalar
      REAL(wp), POINTER, DIMENSION(:,:  ) :: zpar100, zpar0m 
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zparr, zparg
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_opt')
      !
      ! Allocate temporary workspace
      CALL wrk_alloc( jpi, jpj,      zpar100, zpar0m )
      CALL wrk_alloc( jpi, jpj, jpk, zparr, zparg    )

      IF( kt == nittrc000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' trc_opt : LOBSTER optic-model'
         IF(lwp) WRITE(numout,*) ' ~~~~~~~ '
      ENDIF

      !                                          ! surface irradiance
      zpar0m (:,:)   = qsr   (:,:) * 0.43        ! ------------------
      zpar100(:,:)   = zpar0m(:,:) * 0.01
      xpar   (:,:,1) = zpar0m(:,:)
      zparr  (:,:,1) = zpar0m(:,:) * 0.5
      zparg  (:,:,1) = zpar0m(:,:) * 0.5

      !                                          ! Photosynthetically Available Radiation (PAR)
      zcoef = 12 * redf / rcchl / rpig           ! --------------------------------------
      DO jk = 2, jpk                                  ! local par at w-levels
         DO jj = 1, jpj
            DO ji = 1, jpi
               zpig = LOG(  MAX( TINY(0.), trn(ji,jj,jk-1,jp_lob_phy) ) * zcoef  )
               zkr  = xkr0 + xkrp * EXP( xlr * zpig )
               zkg  = xkg0 + xkgp * EXP( xlg * zpig )
               zparr(ji,jj,jk) = zparr(ji,jj,jk-1) * EXP( -zkr * fse3t(ji,jj,jk-1) )
               zparg(ji,jj,jk) = zparg(ji,jj,jk-1) * EXP( -zkg * fse3t(ji,jj,jk-1) )
            END DO
        END DO
      END DO
      DO jk = 1, jpkm1                                ! mean par at t-levels
         DO jj = 1, jpj
            DO ji = 1, jpi
               zpig = LOG(  MAX( TINY(0.), trn(ji,jj,jk,jp_lob_phy) ) * zcoef  )
               zkr  = xkr0 + xkrp * EXP( xlr * zpig )
               zkg  = xkg0 + xkgp * EXP( xlg * zpig )
               zparr(ji,jj,jk) = zparr(ji,jj,jk) / ( zkr * fse3t(ji,jj,jk) ) * ( 1 - EXP( -zkr * fse3t(ji,jj,jk) ) )
               zparg(ji,jj,jk) = zparg(ji,jj,jk) / ( zkg * fse3t(ji,jj,jk) ) * ( 1 - EXP( -zkg * fse3t(ji,jj,jk) ) )
               xpar (ji,jj,jk) = MAX( zparr(ji,jj,jk) + zparg(ji,jj,jk), 1.e-15 )
            END DO
         END DO
      END DO

      !                                          ! Euphotic layer
      !                                          ! --------------
      neln(:,:) = 1                                   ! euphotic layer level
      DO jk = 1, jpk                                  ! (i.e. 1rst T-level strictly below EL bottom)
         DO jj = 1, jpj
           DO ji = 1, jpi
              IF( xpar(ji,jj,jk) >= zpar100(ji,jj) )   neln(ji,jj) = jk + 1 
              !                                       ! nb. this is to ensure compatibility with
              !                                       ! nmld_trc definition in trd_mld_trc_zint
           END DO
         END DO
      END DO
      !                                               ! Euphotic layer depth
      DO jj = 1, jpj
         DO ji = 1, jpi
            xze(ji,jj) = fsdepw(ji,jj,neln(ji,jj))
         END DO
      END DO 


      IF(ln_ctl) THEN      ! print mean trends (used for debugging)
         WRITE(charout, FMT="('opt')")
         CALL prt_ctl_trc_info( charout )
         CALL prt_ctl_trc( tab4d=trn, mask=tmask, clinfo=ctrcnm )
      ENDIF
      !
      CALL wrk_dealloc( jpi, jpj,      zpar100, zpar0m )
      CALL wrk_dealloc( jpi, jpj, jpk, zparr, zparg    )
      !
      IF( nn_timing == 1 )  CALL timing_stop('trc_opt')
      !
   END SUBROUTINE trc_opt

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE trc_opt( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_opt: You should not have seen this print! error?', kt
   END SUBROUTINE trc_opt
#endif 

   !!======================================================================
END MODULE  trcopt
