MODULE sbcfwb
   !!======================================================================
   !!                       ***  MODULE  sbcfwb  ***
   !! Ocean fluxes   : domain averaged freshwater budget
   !!======================================================================
   !! History :  OPA  ! 2001-02  (E. Durand)  Original code
   !!   NEMO     1.0  ! 2002-06  (G. Madec)  F90: Free form and module
   !!            3.0  ! 2006-08  (G. Madec)  Surface module
   !!            3.2  ! 2009-07  (C. Talandier) emp mean s spread over erp area 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_fwb      : freshwater budget for global ocean configurations
   !!                  in free surface and forced mode
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE sbc_oce         ! surface ocean boundary condition
   USE phycst          ! physical constants
   USE sbcrnf          ! ocean runoffs
   USE sbcssr          ! SS damping terms
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! distribued memory computing library
   USE wrk_nemo        ! work arrays
   USE timing          ! Timing
   USE lbclnk          ! ocean lateral boundary conditions
   USE lib_fortran

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_fwb    ! routine called by step

   REAL(wp) ::   a_fwb_b   ! annual domain averaged freshwater budget
   REAL(wp) ::   a_fwb     ! for 2 year before (_b) and before year.
   REAL(wp) ::   fwfold    ! fwfold to be suppressed
   REAL(wp) ::   area      ! global mean ocean surface (interior domain)

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: sbcfwb.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_fwb( kt, kn_fwb, kn_fsbc )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sbc_fwb  ***
      !!
      !! ** Purpose :   Control the mean sea surface drift
      !!
      !! ** Method  :   several ways  depending on kn_fwb
      !!                =0 no control 
      !!                =1 global mean of emp set to zero at each nn_fsbc time step
      !!                =2 annual global mean corrected from previous year
      !!                =3 global mean of emp set to zero at each nn_fsbc time step
      !!                   & spread out over erp area depending its sign
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt       ! ocean time-step index
      INTEGER, INTENT( in ) ::   kn_fsbc  ! 
      INTEGER, INTENT( in ) ::   kn_fwb   ! ocean time-step index
      !
      INTEGER  ::   inum, ikty, iyear   ! local integers
      REAL(wp) ::   z_fwf, z_fwf_nsrf, zsum_fwf, zsum_erp   ! local scalars
      REAL(wp) ::   zsurf_neg, zsurf_pos, zsurf_tospread    !   -      -
      REAL(wp), POINTER, DIMENSION(:,:) ::   ztmsk_neg, ztmsk_pos, ztmsk_tospread, z_wgt, zerp_cor
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('sbc_fwb')
      !
      CALL wrk_alloc( jpi,jpj, ztmsk_neg, ztmsk_pos, ztmsk_tospread, z_wgt, zerp_cor )
      !
      IF( kt == nit000 ) THEN
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'sbc_fwb : FreshWater Budget correction'
            WRITE(numout,*) '~~~~~~~'
            IF( kn_fwb == 1 )   WRITE(numout,*) '          instantaneously set to zero'
            IF( kn_fwb == 2 )   WRITE(numout,*) '          adjusted from previous year budget'
            IF( kn_fwb == 3 )   WRITE(numout,*) '          fwf set to zero and spread out over erp area'
         ENDIF
         !
         IF( kn_fwb == 3 .AND. nn_sssr /= 2 )   CALL ctl_stop( 'sbc_fwb: nn_fwb = 3 requires nn_sssr = 2, we stop ' )
         !
         area = glob_sum( e1e2t(:,:) )           ! interior global domain surface
      ENDIF
      

      SELECT CASE ( kn_fwb )
      !
      CASE ( 1 )                             !==  global mean fwf set to zero  ==!
         !
         IF( MOD( kt-1, kn_fsbc ) == 0 ) THEN
            z_fwf = glob_sum( e1e2t(:,:) * ( emp(:,:) - rnf(:,:) ) ) / area   ! sum over the global domain
            emp (:,:) = emp (:,:) - z_fwf 
            emps(:,:) = emps(:,:) - z_fwf 
         ENDIF
         !
      CASE ( 2 )                             !==  fwf budget adjusted from the previous year  ==!
         !
         IF( kt == nit000 ) THEN                   ! initialisation
            !                                         ! Read the corrective factor on precipitations (fwfold)
            CALL ctl_opn( inum, 'EMPave_old.dat', 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
            READ ( inum, "(24X,I8,2ES24.16)" ) iyear, a_fwb_b, a_fwb
            CLOSE( inum )
            fwfold = a_fwb                            ! current year freshwater budget correction
            !                                         ! estimate from the previous year budget
            IF(lwp)WRITE(numout,*)
            IF(lwp)WRITE(numout,*)'sbc_fwb : year = ',iyear  , ' freshwater budget correction = ', fwfold
            IF(lwp)WRITE(numout,*)'          year = ',iyear-1, ' freshwater budget read       = ', a_fwb
            IF(lwp)WRITE(numout,*)'          year = ',iyear-2, ' freshwater budget read       = ', a_fwb_b
         ENDIF   
         !                                         ! Update fwfold if new year start
         ikty = 365 * 86400 / rdttra(1)    !!bug  use of 365 days leap year or 360d year !!!!!!!
         IF( MOD( kt, ikty ) == 0 ) THEN
            a_fwb_b = a_fwb
            a_fwb   = glob_sum( e1e2t(:,:) * sshn(:,:) )   ! sum over the global domain
            a_fwb   = a_fwb * 1.e+3 / ( area * 86400. * 365. )     ! convert in Kg/m3/s = mm/s
!!gm        !                                                      !!bug 365d year 
            fwfold =  a_fwb                           ! current year freshwater budget correction
            !                                         ! estimate from the previous year budget
         ENDIF
         ! 
         IF( MOD( kt-1, kn_fsbc ) == 0 ) THEN      ! correct the freshwater fluxes
            emp (:,:) = emp (:,:) + fwfold
            emps(:,:) = emps(:,:) + fwfold
         ENDIF
         !
         IF( kt == nitend .AND. lwp ) THEN         ! save fwfold value in a file
            CALL ctl_opn( inum, 'EMPave.dat', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE., narea )
            WRITE( inum, "(24X,I8,2ES24.16)" ) nyear, a_fwb_b, a_fwb
            CLOSE( inum )
         ENDIF
         !
      CASE ( 3 )                             !==  global fwf set to zero and spread out over erp area  ==!
         !
         IF( MOD( kt-1, kn_fsbc ) == 0 ) THEN
            ztmsk_pos(:,:) = tmask_i(:,:)                      ! Select <0 and >0 area of erp
            WHERE( erp < 0._wp )   ztmsk_pos = 0._wp
            ztmsk_neg(:,:) = tmask_i(:,:) - ztmsk_pos(:,:)
            !
            zsurf_neg = glob_sum( e1e2t(:,:)*ztmsk_neg(:,:) )   ! Area filled by <0 and >0 erp 
            zsurf_pos = glob_sum( e1e2t(:,:)*ztmsk_pos(:,:) )
            !                                                  ! fwf global mean 
            z_fwf = glob_sum( e1e2t(:,:) * ( emp(:,:) - rnf(:,:) ) ) / area
            !            
            IF( z_fwf < 0._wp ) THEN         ! spread out over >0 erp area to increase evaporation
                zsurf_tospread      = zsurf_pos
                ztmsk_tospread(:,:) = ztmsk_pos(:,:)
            ELSE                             ! spread out over <0 erp area to increase precipitation
                zsurf_tospread      = zsurf_neg
                ztmsk_tospread(:,:) = ztmsk_neg(:,:)
            ENDIF
            !
            zsum_fwf   = glob_sum( e1e2t(:,:) * z_fwf )         ! fwf global mean over <0 or >0 erp area
!!gm :  zsum_fwf   = z_fwf * area   ???  it is right?  I think so....
            z_fwf_nsrf =  zsum_fwf / ( zsurf_tospread + rsmall )
            !                                                  ! weight to respect erp field 2D structure 
            zsum_erp = glob_sum( ztmsk_tospread(:,:) * erp(:,:) * e1e2t(:,:) )
            z_wgt(:,:) = ztmsk_tospread(:,:) * erp(:,:) / ( zsum_erp + rsmall )
            !                                                  ! final correction term to apply
            zerp_cor(:,:) = -1. * z_fwf_nsrf * zsurf_tospread * z_wgt(:,:)
            !
!!gm   ===>>>>  lbc_lnk should be useless as all the computation is done over the whole domain !
            CALL lbc_lnk( zerp_cor, 'T', 1. )
            !
            emp (:,:) = emp (:,:) + zerp_cor(:,:)
            emps(:,:) = emps(:,:) + zerp_cor(:,:)
            erp (:,:) = erp (:,:) + zerp_cor(:,:)
            !
            IF( nprint == 1 .AND. lwp ) THEN                   ! control print
               IF( z_fwf < 0._wp ) THEN
                  WRITE(numout,*)'   z_fwf < 0'
                  WRITE(numout,*)'   SUM(erp+)     = ', SUM( ztmsk_tospread(:,:)*erp(:,:)*e1e2t(:,:) )*1.e-9,' Sv'
               ELSE
                  WRITE(numout,*)'   z_fwf >= 0'
                  WRITE(numout,*)'   SUM(erp-)     = ', SUM( ztmsk_tospread(:,:)*erp(:,:)*e1e2t(:,:) )*1.e-9,' Sv'
               ENDIF
               WRITE(numout,*)'   SUM(empG)     = ', SUM( z_fwf*e1e2t(:,:) )*1.e-9,' Sv'
               WRITE(numout,*)'   z_fwf         = ', z_fwf      ,' Kg/m2/s'
               WRITE(numout,*)'   z_fwf_nsrf    = ', z_fwf_nsrf ,' Kg/m2/s'
               WRITE(numout,*)'   MIN(zerp_cor) = ', MINVAL(zerp_cor) 
               WRITE(numout,*)'   MAX(zerp_cor) = ', MAXVAL(zerp_cor) 
            ENDIF
         ENDIF
         !
      CASE DEFAULT                           !==  you should never be there  ==!
         CALL ctl_stop( 'sbc_fwb : wrong nn_fwb value for the FreshWater Budget correction, choose either 1, 2 or 3' )
         !
      END SELECT
      !
      CALL wrk_dealloc( jpi,jpj, ztmsk_neg, ztmsk_pos, ztmsk_tospread, z_wgt, zerp_cor )
      !
      IF( nn_timing == 1 )  CALL timing_stop('sbc_fwb')
      !
   END SUBROUTINE sbc_fwb

   !!======================================================================
END MODULE sbcfwb
