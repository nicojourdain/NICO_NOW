MODULE trcini_lobster
   !!======================================================================
   !!                         ***  MODULE trcini_lobster  ***
   !! TOP :   initialisation of the LOBSTER biological model
   !!======================================================================
   !! History :   OPA  !  1999-09  (M. Levy) Original code
   !!              -   !  2000-12  (0. Aumont, E. Kestenare) add sediment 
   !!   NEMO      1.0  !  2004-03  (C. Ethe) Modularity
   !!              -   !  2005-03  (O. Aumont, A. El Moussaoui) F90
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec) from trcini.lobster1.h90
   !!----------------------------------------------------------------------
#if defined key_lobster
   !!----------------------------------------------------------------------
   !!   'key_lobster'                                     LOBSTER bio-model
   !!----------------------------------------------------------------------
   !! trc_ini_lobster  : LOBSTER model initialisation
   !!----------------------------------------------------------------------
   USE par_trc         ! TOP parameters
   USE sms_lobster     ! Source Minus Sink variables
   USE oce_trc         ! ocean variables
   USE trc
   USE lbclnk 

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_ini_lobster   ! called by trcini.F90 module

#  include "top_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcini_lobster.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_ini_lobster
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE trc_ini_lobster  ***
      !! ** purpose :   specific initialisation for LOBSTER bio-model
      !!----------------------------------------------------------------------
      !!
      INTEGER  ::   ji, jj, jk, jn
      REAL(wp) ::   ztest, zfluo, zfluu
      REAL(wp), POINTER, DIMENSION(:,:  ) :: zrro
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zdm0
      !!---------------------------------------------------------------------

      ! Allocate temporary workspace
      CALL wrk_alloc( jpi, jpj,      zrro )
      CALL wrk_alloc( jpi, jpj, jpk, zdm0 )


      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_ini_lobster :   LOBSTER biochemical model initialisation'
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~~'

      !                                ! Allocate LOBSTER arrays
      IF( sms_lobster_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'trc_ini_lobster: unable to allocate LOBSTER arrays' )



      ! initialization of fields for optical model
      ! --------------------------------------------
      xze (:,:)   = 5._wp
      xpar(:,:,:) = 0._wp

      ! initialization for passive tracer remineralisation-damping  array
      ! -----------------------------------------------------------------

      DO jn = jp_lob0, jp_lob1
         remdmp(:,jn) = tminr
      END DO

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) ' trcini: compute remineralisation-damping arrays for tracers'
      ENDIF

      ! initialization of biological variables
      ! ------------------------------------------

      ! Calculate vertical distribution of newly formed biogenic poc
      ! in the water column in the case of max. possible bottom depth
      ! ------------------------------------------------------------

      zdm0 = 0._wp
      zrro = 1._wp
      DO jk = jpkb, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zfluo = ( fsdepw(ji,jj,jk  ) / fsdepw(ji,jj,jpkb) )**xhr 
               zfluu = ( fsdepw(ji,jj,jk+1) / fsdepw(ji,jj,jpkb) )**xhr
               IF( zfluo.GT.1. )   zfluo = 1._wp
               zdm0(ji,jj,jk) = zfluo - zfluu
               IF( jk <= jpkb-1 )   zdm0(ji,jj,jk) = 0._wp
               zrro(ji,jj) = zrro(ji,jj) - zdm0(ji,jj,jk)
            END DO
         END DO
      END DO
      !
      zdm0(:,:,jpk) = zrro(:,:)

      ! Calculate vertical distribution of newly formed biogenic poc
      ! in the water column with realistic topography (first "dry" layer
      ! contains total fraction, which has passed to the upper layers)
      ! ----------------------------------------------------------------------
      dminl(:,:)   = 0._wp
      dmin3(:,:,:) = zdm0
      DO jk = 1, jpk
         DO jj = 1, jpj
            DO ji = 1, jpi
               IF( tmask(ji,jj,jk) == 0._wp ) THEN
                  dminl(ji,jj) = dminl(ji,jj) + dmin3(ji,jj,jk)
                  dmin3(ji,jj,jk) = 0._wp
               ENDIF
            END DO
         END DO
      END DO

      DO jj = 1, jpj
         DO ji = 1, jpi
            IF( tmask(ji,jj,1) == 0 )   dmin3(ji,jj,1) = 0._wp
         END DO
      END DO

      ! Coastal mask 
      ! ------------   
      cmask(:,:) = 0._wp
      DO ji = 2, jpi-1
         DO jj = 2, jpj-1
            IF( tmask(ji,jj,1) == 1._wp ) THEN
               ztest=tmask(ji+1,jj,1)*tmask(ji-1,jj,1)*tmask(ji,jj+1,1)*tmask(ji,jj-1,1)
               IF( ztest == 0 )   cmask(ji,jj) = 1._wp
            ENDIF
         END DO
      END DO

      CALL lbc_lnk( cmask, 'T', 1. )

      ! Coastal surface
      ! ---------------
      areacot = glob_sum( e1e2t(:,:) * cmask(:,:) )

      ! Initialization of tracer concentration in case of  no restart 
      !-------------------------------------------------------------
      IF( .NOT. ln_rsttr ) THEN    

# if defined key_eel_r6 || defined key_eel_r2
         ! LOBSTER initialisation for EEL
         ! ----------------------
         ! here: analytical initialisation used in Levy et al. (2001)
         
         DO jk = 1, 7
            trn(:,:,jk,jp_lob_det) = 0.016 * tmask(:,:,jk)
            trn(:,:,jk,jp_lob_zoo) = 0.018 * tmask(:,:,jk)
            trn(:,:,jk,jp_lob_phy) = 0.036 * tmask(:,:,jk) 
            trn(:,:,jk,jp_lob_no3) = 1.e-5 * tmask(:,:,jk)
            trn(:,:,jk,jp_lob_nh4) = 5.e-4 * tmask(:,:,jk)
            trn(:,:,jk,jp_lob_dom) = 0.017 * tmask(:,:,jk)
         END DO
         
         trn(:,:, 8,jp_lob_det) = 0.020   * tmask(:,:, 8)
         trn(:,:, 8,jp_lob_zoo) = 0.027   * tmask(:,:, 8)
         trn(:,:, 8,jp_lob_phy) = 0.041   * tmask(:,:, 8)
         trn(:,:, 8,jp_lob_no3) = 0.00022 * tmask(:,:, 8)
         trn(:,:, 8,jp_lob_nh4) = 0.0033  * tmask(:,:, 8)
         trn(:,:, 8,jp_lob_dom) = 0.021   * tmask(:,:, 8)
         
         trn(:,:, 9,jp_lob_det) = 0.0556  * tmask(:,:, 9)
         trn(:,:, 9,jp_lob_zoo) = 0.123   * tmask(:,:, 9)
         trn(:,:, 9,jp_lob_phy) = 0.122   * tmask(:,:, 9)
         trn(:,:, 9,jp_lob_no3) = 0.028   * tmask(:,:, 9)
         trn(:,:, 9,jp_lob_nh4) = 0.024   * tmask(:,:, 9)
         trn(:,:, 9,jp_lob_dom) = 0.06    * tmask(:,:, 9)
         
         trn(:,:,10,jp_lob_det) = 0.025   * tmask(:,:,10)
         trn(:,:,10,jp_lob_zoo) = 0.016   * tmask(:,:,10)
         trn(:,:,10,jp_lob_phy) = 0.029   * tmask(:,:,10)
         trn(:,:,10,jp_lob_no3) = 2.462   * tmask(:,:,10)
         trn(:,:,10,jp_lob_nh4) = 0.04    * tmask(:,:,10)
         trn(:,:,10,jp_lob_dom) = 0.022   * tmask(:,:,10)
         
         trn(:,:,11,jp_lob_det) = 0.0057  * tmask(:,:,11)
         trn(:,:,11,jp_lob_zoo) = 0.0005  * tmask(:,:,11)
         trn(:,:,11,jp_lob_phy) = 0.0006  * tmask(:,:,11)
         trn(:,:,11,jp_lob_no3) = 3.336   * tmask(:,:,11)
         trn(:,:,11,jp_lob_nh4) = 0.005   * tmask(:,:,11)
         trn(:,:,11,jp_lob_dom) = 0.004   * tmask(:,:,11)
         
         trn(:,:,12,jp_lob_det) = 0.002   * tmask(:,:,12)
         trn(:,:,12,jp_lob_zoo) = 1.e-6   * tmask(:,:,12)
         trn(:,:,12,jp_lob_phy) = 5.e-6   * tmask(:,:,12)
         trn(:,:,12,jp_lob_no3) = 4.24    * tmask(:,:,12)
         trn(:,:,12,jp_lob_nh4) = 0.001   * tmask(:,:,12)
         trn(:,:,12,jp_lob_dom) = 3.e-5   * tmask(:,:,12)
         
         DO jk=13,jpk
            trn(:,:,jk,jp_lob_det) = 0.e0
            trn(:,:,jk,jp_lob_zoo) = 0.e0
            trn(:,:,jk,jp_lob_phy) = 0.e0
            trn(:,:,jk,jp_lob_nh4) = 0.e0
            trn(:,:,jk,jp_lob_dom) = 0.e0
         END DO
         
         trn(:,:,13,jp_lob_no3) = 5.31  * tmask(:,:,13)
         trn(:,:,14,jp_lob_no3) = 6.73  * tmask(:,:,14)
         trn(:,:,15,jp_lob_no3) = 8.32  * tmask(:,:,15)
         trn(:,:,16,jp_lob_no3) = 10.13 * tmask(:,:,16)
         trn(:,:,17,jp_lob_no3) = 11.95 * tmask(:,:,17)
         trn(:,:,18,jp_lob_no3) = 13.57 * tmask(:,:,18)
         trn(:,:,19,jp_lob_no3) = 15.08 * tmask(:,:,19)
         trn(:,:,20,jp_lob_no3) = 16.41 * tmask(:,:,20)
         trn(:,:,21,jp_lob_no3) = 17.47 * tmask(:,:,21)
         trn(:,:,22,jp_lob_no3) = 18.29 * tmask(:,:,22)
         trn(:,:,23,jp_lob_no3) = 18.88 * tmask(:,:,23)
         trn(:,:,24,jp_lob_no3) = 19.30 * tmask(:,:,24)
         trn(:,:,25,jp_lob_no3) = 19.68 * tmask(:,:,25)
         trn(:,:,26,jp_lob_no3) = 19.91 * tmask(:,:,26)
         trn(:,:,27,jp_lob_no3) = 19.99 * tmask(:,:,27)
         trn(:,:,28,jp_lob_no3) = 20.01 * tmask(:,:,28)
         trn(:,:,29,jp_lob_no3) = 20.01 * tmask(:,:,29)
         trn(:,:,30,jp_lob_no3) = 20.01 * tmask(:,:,30)


# elif defined key_gyre || defined key_orca_r2
         ! LOBSTER initialisation for GYRE
         ! ----------------------
         ! here:  init NO3=f(density) by asklod AS Kremeur 2005-07
         trn(:,:,:,jp_lob_det) = 0.1 * tmask(:,:,:)
         trn(:,:,:,jp_lob_zoo) = 0.1 * tmask(:,:,:)
         trn(:,:,:,jp_lob_nh4) = 0.1 * tmask(:,:,:)
         trn(:,:,:,jp_lob_phy) = 0.1 * tmask(:,:,:)
         trn(:,:,:,jp_lob_dom) = 1.0 * tmask(:,:,:)
         DO jk = 1, jpk
            DO jj = 1, jpj
               DO ji = 1, jpi
                  IF( rhd(ji,jj,jk) <= 24.5e-3 ) THEN
                     trn(ji,jj,jk,jp_lob_no3) = 2. * tmask(ji,jj,jk)
                  ELSE
                     trn(ji,jj,jk,jp_lob_no3) = ( 15.55 * ( rhd(ji,jj,jk) * 1000. ) - 380.11 ) * tmask(ji,jj,jk)
                  ENDIF
               END DO
            END DO
         END DO
#endif

      ENDIF

      !  initialize the POC in sediments
      sedpocb(:,:) = 0._wp
      sedpocn(:,:) = 0._wp
      sedpoca(:,:) = 0._wp
      !
      IF(lwp) WRITE(numout,*) 'Initialization of LOBSTER tracers done'
      !
      CALL wrk_dealloc( jpi, jpj,      zrro )
      CALL wrk_dealloc( jpi, jpj, jpk, zdm0 )
      !
   END SUBROUTINE trc_ini_lobster

#else
   !!----------------------------------------------------------------------
   !!   Dummy module                                   No LOBSTER bio-model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_ini_lobster             ! Empty routine
   END SUBROUTINE trc_ini_lobster
#endif

   !!======================================================================
END MODULE trcini_lobster
