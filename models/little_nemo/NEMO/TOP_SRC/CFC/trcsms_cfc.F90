MODULE trcsms_cfc
   !!======================================================================
   !!                      ***  MODULE trcsms_cfc  ***
   !! TOP : CFC main model
   !!======================================================================
   !! History :  OPA  !  1999-10  (JC. Dutay)  original code
   !!  NEMO      1.0  !  2004-03  (C. Ethe) free form + modularity
   !!            2.0  !  2007-12  (C. Ethe, G. Madec)  reorganisation
   !!----------------------------------------------------------------------
#if defined key_cfc
   !!----------------------------------------------------------------------
   !!   'key_cfc'                                               CFC tracers
   !!----------------------------------------------------------------------
   !!   trc_sms_cfc  :  compute and add CFC suface forcing to CFC trends
   !!   trc_cfc_cst  :  sets constants for CFC surface forcing computation
   !!----------------------------------------------------------------------
   USE oce_trc       ! Ocean variables
   USE par_trc       ! TOP parameters
   USE trc           ! TOP variables
   USE trdmod_oce
   USE trdmod_trc
   USE iom           ! I/O library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_sms_cfc         ! called in ???    
   PUBLIC   trc_sms_cfc_alloc   ! called in trcini_cfc.F90

   INTEGER , PUBLIC, PARAMETER ::   jphem  =   2   ! parameter for the 2 hemispheres
   INTEGER , PUBLIC            ::   jpyear         ! Number of years read in CFC1112 file
   INTEGER , PUBLIC            ::   ndate_beg      ! initial calendar date (aammjj) for CFC
   INTEGER , PUBLIC            ::   nyear_res      ! restoring time constant (year)
   INTEGER , PUBLIC            ::   nyear_beg      ! initial year (aa) 
   
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   p_cfc    ! partial hemispheric pressure for CFC
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   xphem    ! spatial interpolation factor for patm
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qtr_cfc  ! flux at surface
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qint_cfc ! cumulative flux 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   patm     ! atmospheric function

   REAL(wp), DIMENSION(4,2) ::   soa   ! coefficient for solubility of CFC [mol/l/atm]
   REAL(wp), DIMENSION(3,2) ::   sob   !    "               "
   REAL(wp), DIMENSION(4,2) ::   sca   ! coefficients for schmidt number in degre Celcius
      
   !                          ! coefficients for conversion
   REAL(wp) ::   xconv1 = 1.0          ! conversion from to 
   REAL(wp) ::   xconv2 = 0.01/3600.   ! conversion from cm/h to m/s: 
   REAL(wp) ::   xconv3 = 1.0e+3       ! conversion from mol/l/atm to mol/m3/atm
   REAL(wp) ::   xconv4 = 1.0e-12      ! conversion from mol/m3/atm to mol/m3/pptv 

   !! * Substitutions
#  include "top_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcsms_cfc.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_sms_cfc( kt )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sms_cfc  ***
      !!
      !! ** Purpose :   Compute the surface boundary contition on CFC 11 
      !!             passive tracer associated with air-mer fluxes and add it 
      !!             to the general trend of tracers equations.
      !!
      !! ** Method  : - get the atmospheric partial pressure - given in pico -
      !!              - computation of solubility ( in 1.e-12 mol/l then in 1.e-9 mol/m3)
      !!              - computation of transfert speed ( given in cm/hour ----> cm/s )
      !!              - the input function is given by : 
      !!                speed * ( concentration at equilibrium - concentration at surface )
      !!              - the input function is in pico-mol/m3/s and the
      !!                CFC concentration in pico-mol/m3
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT(in) ::   kt    ! ocean time-step index
      !
      INTEGER  ::   ji, jj, jn, jl, jm, js
      INTEGER  ::   iyear_beg, iyear_end
      INTEGER  ::   im1, im2, ierr
      REAL(wp) ::   ztap, zdtap        
      REAL(wp) ::   zt1, zt2, zt3, zv2
      REAL(wp) ::   zsol      ! solubility
      REAL(wp) ::   zsch      ! schmidt number 
      REAL(wp) ::   zpp_cfc   ! atmospheric partial pressure of CFC
      REAL(wp) ::   zca_cfc   ! concentration at equilibrium
      REAL(wp) ::   zak_cfc   ! transfert coefficients
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)  ::   zpatm     ! atmospheric function
      !!----------------------------------------------------------------------
      !
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_sms_cfc')
      !
      ALLOCATE( zpatm(jphem,jp_cfc), STAT=ierr )
      IF( ierr > 0 ) THEN
         CALL ctl_stop( 'trc_sms_cfc: unable to allocate zpatm array' )   ;   RETURN
      ENDIF

      IF( kt == nittrc000 )   CALL trc_cfc_cst

      ! Temporal interpolation
      ! ----------------------
      iyear_beg = nyear - 1900
      IF ( nmonth <= 6 ) THEN
         iyear_beg = iyear_beg - 1
         im1       =  6 - nmonth + 1
         im2       =  6 + nmonth - 1
      ELSE
         im1       = 12 - nmonth + 7
         im2       =      nmonth - 7
      ENDIF
      iyear_end = iyear_beg + 1

      !                                                  !------------!
      DO jl = 1, jp_cfc                                  !  CFC loop  !
         !                                               !------------!
         jn = jp_cfc0 + jl - 1
         ! time interpolation at time kt
         DO jm = 1, jphem
            zpatm(jm,jl) = (  p_cfc(iyear_beg, jm, jl) * FLOAT (im1)  &
               &           +  p_cfc(iyear_end, jm, jl) * FLOAT (im2) ) / 12.
         END DO
         
         !                                                         !------------!
         DO jj = 1, jpj                                            !  i-j loop  !
            DO ji = 1, jpi                                         !------------!
 
               ! space interpolation
               zpp_cfc  =       xphem(ji,jj)   * zpatm(1,jl)   &
                  &     + ( 1.- xphem(ji,jj) ) * zpatm(2,jl)

               ! Computation of concentration at equilibrium : in picomol/l
               ! coefficient for solubility for CFC-11/12 in  mol/l/atm
               IF( tmask(ji,jj,1) .GE. 0.5 ) THEN
                  ztap  = ( tsn(ji,jj,1,jp_tem) + 273.16 ) * 0.01
                  zdtap = sob(1,jl) + ztap * ( sob(2,jl) + ztap * sob(3,jl) ) 
                  zsol  =  EXP( soa(1,jl) + soa(2,jl) / ztap + soa(3,jl) * LOG( ztap )   &
                     &                    + soa(4,jl) * ztap * ztap + tsn(ji,jj,1,jp_sal) * zdtap ) 
               ELSE
                  zsol  = 0.e0
               ENDIF
               ! conversion from mol/l/atm to mol/m3/atm and from mol/m3/atm to mol/m3/pptv    
               zsol = xconv4 * xconv3 * zsol * tmask(ji,jj,1)  
               ! concentration at equilibrium
               zca_cfc = xconv1 * zpp_cfc * zsol * tmask(ji,jj,1)             
  
               ! Computation of speed transfert
               !    Schmidt number
               zt1  = tsn(ji,jj,1,jp_tem)
               zt2  = zt1 * zt1 
               zt3  = zt1 * zt2
               zsch = sca(1,jl) + sca(2,jl) * zt1 + sca(3,jl) * zt2 + sca(4,jl) * zt3

               !    speed transfert : formulae of wanninkhof 1992
               zv2     = wndm(ji,jj) * wndm(ji,jj)
               zsch    = zsch / 660.
               zak_cfc = ( 0.39 * xconv2 * zv2 / SQRT(zsch) ) * tmask(ji,jj,1)

               ! Input function  : speed *( conc. at equil - concen at surface )
               ! trn in pico-mol/l idem qtr; ak in en m/a
               qtr_cfc(ji,jj,jl) = -zak_cfc * ( trb(ji,jj,1,jn) - zca_cfc )   &
#if defined key_degrad
                  &                         * facvol(ji,jj,1)                           &
#endif
                  &                         * tmask(ji,jj,1) * ( 1. - fr_i(ji,jj) )
               ! Add the surface flux to the trend
               tra(ji,jj,1,jn) = tra(ji,jj,1,jn) + qtr_cfc(ji,jj,jl) / fse3t(ji,jj,1) 

               ! cumulation of surface flux at each time step
               qint_cfc(ji,jj,jl) = qint_cfc(ji,jj,jl) + qtr_cfc(ji,jj,jl) * rdt
               !                                               !----------------!
            END DO                                             !  end i-j loop  !
         END DO                                                !----------------!
         !                                                  !----------------!
      END DO                                                !  end CFC loop  !
      !                                                     !----------------!
      IF( ln_diatrc ) THEN
        !
        IF( lk_iomput ) THEN
           CALL iom_put( "qtrCFC11"  , qtr_cfc (:,:,1) )
           CALL iom_put( "qintCFC11" , qint_cfc(:,:,1) )
        ELSE
           trc2d(:,:,jp_cfc0_2d    ) = qtr_cfc (:,:,1)
           trc2d(:,:,jp_cfc0_2d + 1) = qint_cfc(:,:,1)
        END IF
        !
      END IF
 
      IF( l_trdtrc ) THEN
          DO jn = jp_cfc0, jp_cfc1
            CALL trd_mod_trc( tra(:,:,:,jn), jn, jptra_trd_sms, kt )   ! save trends
          END DO
      END IF
      !
      IF( nn_timing == 1 )  CALL timing_stop('trc_sms_cfc')
      !
   END SUBROUTINE trc_sms_cfc


   SUBROUTINE trc_cfc_cst
      !!---------------------------------------------------------------------
      !!                     ***  trc_cfc_cst  ***  
      !!
      !! ** Purpose : sets constants for CFC model
      !!---------------------------------------------------------------------

      ! coefficient for CFC11 
      !----------------------

      ! Solubility
      soa(1,1) = -229.9261 
      soa(2,1) =  319.6552
      soa(3,1) =  119.4471
      soa(4,1) =  -1.39165

      sob(1,1) =  -0.142382
      sob(2,1) =   0.091459
      sob(3,1) =  -0.0157274

      ! Schmidt number 
      sca(1,1) = 3501.8
      sca(2,1) = -210.31
      sca(3,1) =  6.1851
      sca(4,1) = -0.07513

      ! coefficient for CFC12 
      !----------------------

      ! Solubility
      soa(1,2) = -218.0971
      soa(2,2) =  298.9702
      soa(3,2) =  113.8049
      soa(4,2) =  -1.39165

      sob(1,2) =  -0.143566
      sob(2,2) =   0.091015
      sob(3,2) =  -0.0153924

      ! schmidt number 
      sca(1,2) =  3845.4 
      sca(2,2) =  -228.95
      sca(3,2) =  6.1908 
      sca(4,2) =  -0.067430

   END SUBROUTINE trc_cfc_cst


   INTEGER FUNCTION trc_sms_cfc_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sms_cfc_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( xphem   (jpi,jpj)        ,     &
         &      qtr_cfc (jpi,jpj,jp_cfc) ,     &
         &      qint_cfc(jpi,jpj,jp_cfc) , STAT=trc_sms_cfc_alloc )
         !
      IF( trc_sms_cfc_alloc /= 0 ) CALL ctl_warn('trc_sms_cfc_alloc : failed to allocate arrays.')
      !
   END FUNCTION trc_sms_cfc_alloc

#else
   !!----------------------------------------------------------------------
   !!   Dummy module                                         No CFC tracers
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_sms_cfc( kt )       ! Empty routine
      WRITE(*,*) 'trc_sms_cfc: You should not have seen this print! error?', kt
   END SUBROUTINE trc_sms_cfc
#endif

   !!======================================================================
END MODULE trcsms_cfc
