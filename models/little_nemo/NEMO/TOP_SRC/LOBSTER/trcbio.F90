MODULE trcbio
   !!======================================================================
   !!                         ***  MODULE trcbio  ***
   !! TOP :   LOBSTER
   !!======================================================================
   !! History :    -   !  1999-07  (M. Levy) Original code
   !!              -   !  2000-12  (E. Kestenare) assign a parameter to name individual tracers
   !!              -   !  2001-03  (M. Levy)  LNO3 + dia2d 
   !!             2.0  !  2007-12  (C. Deltel, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_lobster
   !!----------------------------------------------------------------------
   !!   'key_lobster'                                     LOBSTER bio-model
   !!----------------------------------------------------------------------
   !!   trc_bio        :  
   !!----------------------------------------------------------------------
   USE oce_trc         !
   USE trc             ! 
   USE sms_lobster     ! 
   USE lbclnk          ! 
   USE prtctl_trc      ! Print control for debbuging
   USE trdmod_oce
   USE trdmod_trc
   USE iom
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_bio    ! called in ???

   !!* Substitution
#  include "top_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcbio.F90 3294 2012-01-28 16:44:18Z rblod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_bio( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_bio  ***
      !!
      !! ** Purpose :   compute the now trend due to biogeochemical processes
      !!              and add it to the general trend of passive tracers equations
      !!
      !! ** Method  :   each now biological flux is calculated in function of now
      !!              concentrations of tracers.
      !!              depending on the tracer, these fluxes are sources or sinks.
      !!              the total of the sources and sinks for each tracer
      !!              is added to the general trend.
      !!        
      !!                      tra = tra + zf...tra - zftra...
      !!                                     |         |
      !!                                     |         |
      !!                                  source      sink
      !!        
      !!              IF 'key_diabio' defined , the biogeochemical trends
      !!              for passive tracers are saved for futher diagnostics.
      !!---------------------------------------------------------------------
      !!
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index      
      !!
      INTEGER  ::   ji, jj, jk, jl
      REAL(wp) ::   zdet, zzoo, zphy, zno3, znh4, zdom      ! now concentrations
      REAL(wp) ::   zlno3, zlnh4, zle, zlt                  ! limitation terms for phyto
      REAL(wp) ::   zno3phy, znh4phy, zphynh4, zphydom
      REAL(wp) ::   zphydet, zphyzoo, zdetzoo
      REAL(wp) ::   zzoonh4, zzoodom, zzoodet, zdetnh4, zdetdom
      REAL(wp) ::   znh4no3, zdomnh4, zppz, zpdz, zpppz, zppdz, zfood
      REAL(wp) ::   zfilpz, zfildz, zphya, zzooa, zno3a
      REAL(wp) ::   znh4a, zdeta, zdoma, zzoobod, zboddet, zdomaju
      REAL(wp) ::   ze3t
      REAL(wp), POINTER,   DIMENSION(:,:,:) :: zw2d
      REAL(wp), POINTER, DIMENSION(:,:,:,:) :: zw3d
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( nn_timing == 1 )  CALL timing_start('trc_bio')
      !
      IF( ln_diatrc ) THEN
         CALL wrk_alloc( jpi, jpj,     17, zw2d )
         CALL wrk_alloc( jpi, jpj, jpk, 3, zw3d )
      ENDIF

      IF( kt == nittrc000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' trc_bio: LOBSTER bio-model'
         IF(lwp) WRITE(numout,*) ' ~~~~~~~'
      ENDIF

      fbod(:,:) = 0.e0
      IF( ln_diatrc ) THEN
         zw2d  (:,:,:) = 0.e0
         zw3d(:,:,:,:) = 0.e0
      ENDIF

      !                                      ! -------------------------- !
      DO jk = 1, jpkbm1                      !  Upper ocean (bio-layers)  !
         !                                   ! -------------------------- !
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1 
               ! trophic variables( det, zoo, phy, no3, nh4, dom)
               ! ------------------------------------------------

               ! negative trophic variables DO not contribute to the fluxes
               zdet = MAX( 0.e0, trn(ji,jj,jk,jp_lob_det) )
               zzoo = MAX( 0.e0, trn(ji,jj,jk,jp_lob_zoo) )
               zphy = MAX( 0.e0, trn(ji,jj,jk,jp_lob_phy) )
               zno3 = MAX( 0.e0, trn(ji,jj,jk,jp_lob_no3) )
               znh4 = MAX( 0.e0, trn(ji,jj,jk,jp_lob_nh4) )
               zdom = MAX( 0.e0, trn(ji,jj,jk,jp_lob_dom) )

               ! Limitations
               zlt   = 1.
               zle   = 1. - EXP( -xpar(ji,jj,jk) / aki / zlt )
               ! psinut,akno3,aknh4 added by asklod AS Kremeur 2005-03
               zlno3 = zno3 * EXP( -psinut * znh4 ) / ( akno3 + zno3 )
               zlnh4 = znh4 / (znh4+aknh4)  

               ! sinks and sources
               !    phytoplankton production and exsudation
               zno3phy = tmumax * zle * zlt * zlno3 * zphy
               znh4phy = tmumax * zle * zlt * zlnh4 * zphy

               !    fphylab added by asklod AS Kremeur 2005-03
               zphydom = rgamma * (1 - fphylab) * (zno3phy + znh4phy)
               zphynh4 = rgamma * fphylab * (zno3phy + znh4phy)
               ! zooplankton production
               !    preferences
               zppz = rppz
               zpdz = 1. - rppz
               zpppz = ( zppz * zphy ) / ( ( zppz * zphy + zpdz * zdet ) + 1.e-13 )
               zppdz = ( zpdz * zdet ) / ( ( zppz * zphy + zpdz * zdet ) + 1.e-13 )
               zfood = zpppz * zphy + zppdz * zdet
               !    filtration 
               zfilpz = taus * zpppz / (aks + zfood)
               zfildz = taus * zppdz / (aks + zfood)
               !    grazing
               zphyzoo = zfilpz * zphy * zzoo
               zdetzoo = zfildz * zdet * zzoo

               ! fecal pellets production
               zzoodet = rpnaz * zphyzoo + rdnaz * zdetzoo

               ! zooplankton liquide excretion
               zzoonh4 = tauzn * fzoolab * zzoo  
               zzoodom = tauzn * (1 - fzoolab) * zzoo

               ! mortality
               !    phytoplankton mortality
               zphydet = tmminp * zphy

               !    zooplankton mortality
               !    closure : flux fbod is redistributed below level jpkbio
               zzoobod = tmminz * zzoo * zzoo
               fbod(ji,jj) = fbod(ji,jj) + (1-fdbod) * zzoobod * fse3t(ji,jj,jk)
               zboddet = fdbod * zzoobod

               ! detritus and dom breakdown
               zdetnh4 = taudn * fdetlab * zdet
               zdetdom = taudn * (1 - fdetlab) * zdet

               zdomnh4 = taudomn * zdom

               ! flux added to express how the excess of nitrogen from 
               ! PHY, ZOO and DET to DOM goes directly to NH4 (flux of ajustment)
               zdomaju = (1 - redf/reddom) * (zphydom + zzoodom + zdetdom)

               ! Nitrification 
               znh4no3 = taunn * znh4

               ! determination of trends
               !    total trend for each biological tracer
               zphya =   zno3phy + znh4phy - zphynh4 - zphydom - zphyzoo - zphydet
               zzooa =   zphyzoo + zdetzoo - zzoodet - zzoodom - zzoonh4 - zzoobod
               zno3a = - zno3phy + znh4no3
               znh4a = - znh4phy - znh4no3 + zphynh4 + zzoonh4 + zdomnh4 + zdetnh4 + zdomaju
               zdeta =   zphydet + zzoodet - zdetzoo - zdetnh4 - zdetdom + zboddet
               zdoma =   zphydom + zzoodom + zdetdom - zdomnh4 - zdomaju

               ! tracer flux at totox-point added to the general trend
               tra(ji,jj,jk,jp_lob_det) = tra(ji,jj,jk,jp_lob_det) + zdeta
               tra(ji,jj,jk,jp_lob_zoo) = tra(ji,jj,jk,jp_lob_zoo) + zzooa
               tra(ji,jj,jk,jp_lob_phy) = tra(ji,jj,jk,jp_lob_phy) + zphya
               tra(ji,jj,jk,jp_lob_no3) = tra(ji,jj,jk,jp_lob_no3) + zno3a
               tra(ji,jj,jk,jp_lob_nh4) = tra(ji,jj,jk,jp_lob_nh4) + znh4a
               tra(ji,jj,jk,jp_lob_dom) = tra(ji,jj,jk,jp_lob_dom) + zdoma


               IF( ( ln_diabio .AND. .NOT. lk_iomput ) .OR. l_trdtrc ) THEN
                  trbio(ji,jj,jk,jp_lob0_trd     ) = zno3phy
                  trbio(ji,jj,jk,jp_lob0_trd +  1) = znh4phy
                  trbio(ji,jj,jk,jp_lob0_trd +  2) = zphynh4
                  trbio(ji,jj,jk,jp_lob0_trd +  3) = zphydom
                  trbio(ji,jj,jk,jp_lob0_trd +  4) = zphyzoo
                  trbio(ji,jj,jk,jp_lob0_trd +  5) = zphydet
                  trbio(ji,jj,jk,jp_lob0_trd +  6) = zdetzoo
                  !  trend number 8 in trcsed
                  trbio(ji,jj,jk,jp_lob0_trd +  8) = zzoodet
                  trbio(ji,jj,jk,jp_lob0_trd +  9) = zzoobod
                  trbio(ji,jj,jk,jp_lob0_trd + 10) = zzoonh4
                  trbio(ji,jj,jk,jp_lob0_trd + 11) = zzoodom
                  trbio(ji,jj,jk,jp_lob0_trd + 12) = znh4no3
                  trbio(ji,jj,jk,jp_lob0_trd + 13) = zdomnh4
                  trbio(ji,jj,jk,jp_lob0_trd + 14) = zdetnh4
                  trbio(ji,jj,jk,jp_lob0_trd + 15) = zdetdom
                  !  trend number 17 in trcexp
                ENDIF
                IF( ln_diatrc ) THEN
                  ! convert fluxes in per day
                  ze3t = fse3t(ji,jj,jk) * 86400.
                  zw2d(ji,jj,1)  = zw2d(ji,jj,1)  + zno3phy * ze3t
                  zw2d(ji,jj,2)  = zw2d(ji,jj,2)  + znh4phy * ze3t
                  zw2d(ji,jj,3)  = zw2d(ji,jj,3)  + zphydom * ze3t
                  zw2d(ji,jj,4)  = zw2d(ji,jj,4)  + zphynh4 * ze3t
                  zw2d(ji,jj,5)  = zw2d(ji,jj,5)  + zphyzoo * ze3t
                  zw2d(ji,jj,6)  = zw2d(ji,jj,6)  + zphydet * ze3t
                  zw2d(ji,jj,7)  = zw2d(ji,jj,7)  + zdetzoo * ze3t
                  zw2d(ji,jj,8)  = zw2d(ji,jj,8)  + zzoodet * ze3t
                  zw2d(ji,jj,9)  = zw2d(ji,jj,9)  + zzoobod * ze3t
                  zw2d(ji,jj,10) = zw2d(ji,jj,10) + zzoonh4 * ze3t
                  zw2d(ji,jj,11) = zw2d(ji,jj,11) + zzoodom * ze3t
                  zw2d(ji,jj,12) = zw2d(ji,jj,12) + znh4no3 * ze3t
                  zw2d(ji,jj,13) = zw2d(ji,jj,13) + zdomnh4 * ze3t
                  zw2d(ji,jj,14) = zw2d(ji,jj,14) + zdetnh4 * ze3t
                  zw2d(ji,jj,15) = zw2d(ji,jj,15) + ( zno3phy + znh4phy - zphynh4 - zphydom - zphyzoo - zphydet ) * ze3t
                  zw2d(ji,jj,16) = zw2d(ji,jj,16) + ( zphyzoo + zdetzoo - zzoodet - zzoobod - zzoonh4 - zzoodom ) * ze3t
                  zw2d(ji,jj,17) = zw2d(ji,jj,17) + zdetdom * ze3t
                  !   
                  zw3d(ji,jj,jk,1) = zno3phy * 86400
                  zw3d(ji,jj,jk,2) = znh4phy * 86400     
                  zw3d(ji,jj,jk,3) = znh4no3 * 86400   
                   ! 
                ENDIF
            END DO
         END DO
      END DO

      !                                      ! -------------------------- !
      DO jk = jpkb, jpkm1                    !  Upper ocean (bio-layers)  !
         !                                   ! -------------------------- !
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1 
               ! remineralisation of all quantities towards nitrate 

               !    trophic variables( det, zoo, phy, no3, nh4, dom)
               !       negative trophic variables DO not contribute to the fluxes
               zdet = MAX( 0.e0, trn(ji,jj,jk,jp_lob_det) )
               zzoo = MAX( 0.e0, trn(ji,jj,jk,jp_lob_zoo) )
               zphy = MAX( 0.e0, trn(ji,jj,jk,jp_lob_phy) )
               zno3 = MAX( 0.e0, trn(ji,jj,jk,jp_lob_no3) )
               znh4 = MAX( 0.e0, trn(ji,jj,jk,jp_lob_nh4) )
               zdom = MAX( 0.e0, trn(ji,jj,jk,jp_lob_dom) )

               !    Limitations
               zlt   = 0.e0
               zle   = 0.e0
               zlno3 = 0.e0
               zlnh4 = 0.e0

               !    sinks and sources
               !       phytoplankton production and exsudation
               zno3phy = 0.e0
               znh4phy = 0.e0
               zphydom = 0.e0
               zphynh4 = 0.e0

               !    zooplankton production
               zphyzoo = 0.e0      ! grazing
               zdetzoo = 0.e0

               zzoodet = 0.e0      ! fecal pellets production

               zzoonh4 = tauzn * fzoolab * zzoo         ! zooplankton liquide excretion
               zzoodom = tauzn * (1 - fzoolab) * zzoo

               !    mortality
               zphydet = tmminp * zphy      ! phytoplankton mortality

               zzoobod = 0.e0               ! zooplankton mortality
               zboddet = 0.e0               ! closure : flux fbod is redistributed below level jpkbio

               !    detritus and dom breakdown
               zdetnh4 = taudn * fdetlab * zdet
               zdetdom = taudn * (1 - fdetlab) * zdet

               zdomnh4 = taudomn * zdom
               zdomaju = (1 - redf/reddom) * (zphydom + zzoodom + zdetdom)

               !    Nitrification
               znh4no3 = taunn * znh4


               ! determination of trends
               !     total trend for each biological tracer
               zphya =   zno3phy + znh4phy - zphynh4 - zphydom - zphyzoo - zphydet
               zzooa =   zphyzoo + zdetzoo - zzoodet - zzoodom - zzoonh4 - zzoobod
               zno3a = - zno3phy + znh4no3 
               znh4a = - znh4phy - znh4no3 + zphynh4 + zzoonh4 + zdomnh4 + zdetnh4 + zdomaju
               zdeta = zphydet + zzoodet  - zdetzoo - zdetnh4 - zdetdom + zboddet
               zdoma = zphydom + zzoodom + zdetdom - zdomnh4 - zdomaju

               ! tracer flux at totox-point added to the general trend
               tra(ji,jj,jk,jp_lob_det) = tra(ji,jj,jk,jp_lob_det) + zdeta
               tra(ji,jj,jk,jp_lob_zoo) = tra(ji,jj,jk,jp_lob_zoo) + zzooa
               tra(ji,jj,jk,jp_lob_phy) = tra(ji,jj,jk,jp_lob_phy) + zphya
               tra(ji,jj,jk,jp_lob_no3) = tra(ji,jj,jk,jp_lob_no3) + zno3a
               tra(ji,jj,jk,jp_lob_nh4) = tra(ji,jj,jk,jp_lob_nh4) + znh4a
               tra(ji,jj,jk,jp_lob_dom) = tra(ji,jj,jk,jp_lob_dom) + zdoma
               !
               IF( ( ln_diabio .AND. .NOT. lk_iomput ) .OR. l_trdtrc ) THEN
                  trbio(ji,jj,jk,jp_lob0_trd     ) = zno3phy
                  trbio(ji,jj,jk,jp_lob0_trd +  1) = znh4phy
                  trbio(ji,jj,jk,jp_lob0_trd +  2) = zphynh4
                  trbio(ji,jj,jk,jp_lob0_trd +  3) = zphydom
                  trbio(ji,jj,jk,jp_lob0_trd +  4) = zphyzoo
                  trbio(ji,jj,jk,jp_lob0_trd +  5) = zphydet
                  trbio(ji,jj,jk,jp_lob0_trd +  6) = zdetzoo
                  !  trend number 8 in trcsed
                  trbio(ji,jj,jk,jp_lob0_trd +  8) = zzoodet
                  trbio(ji,jj,jk,jp_lob0_trd +  9) = zzoobod
                  trbio(ji,jj,jk,jp_lob0_trd + 10) = zzoonh4
                  trbio(ji,jj,jk,jp_lob0_trd + 11) = zzoodom
                  trbio(ji,jj,jk,jp_lob0_trd + 12) = znh4no3
                  trbio(ji,jj,jk,jp_lob0_trd + 13) = zdomnh4
                  trbio(ji,jj,jk,jp_lob0_trd + 14) = zdetnh4
                  trbio(ji,jj,jk,jp_lob0_trd + 15) = zdetdom
                  !  trend number 17 in trcexp 
                ENDIF
                IF( ln_diatrc ) THEN
                  ! convert fluxes in per day
                  ze3t = fse3t(ji,jj,jk) * 86400.
                  zw2d(ji,jj,1)  = zw2d(ji,jj,1)  + zno3phy * ze3t
                  zw2d(ji,jj,2)  = zw2d(ji,jj,2)  + znh4phy * ze3t
                  zw2d(ji,jj,3)  = zw2d(ji,jj,3)  + zphydom * ze3t
                  zw2d(ji,jj,4)  = zw2d(ji,jj,4)  + zphynh4 * ze3t
                  zw2d(ji,jj,5)  = zw2d(ji,jj,5)  + zphyzoo * ze3t
                  zw2d(ji,jj,6)  = zw2d(ji,jj,6)  + zphydet * ze3t
                  zw2d(ji,jj,7)  = zw2d(ji,jj,7)  + zdetzoo * ze3t
                  zw2d(ji,jj,8)  = zw2d(ji,jj,8)  + zzoodet * ze3t
                  zw2d(ji,jj,9)  = zw2d(ji,jj,9)  + zzoobod * ze3t
                  zw2d(ji,jj,10) = zw2d(ji,jj,10) + zzoonh4 * ze3t
                  zw2d(ji,jj,11) = zw2d(ji,jj,11) + zzoodom * ze3t
                  zw2d(ji,jj,12) = zw2d(ji,jj,12) + znh4no3 * ze3t
                  zw2d(ji,jj,13) = zw2d(ji,jj,13) + zdomnh4 * ze3t
                  zw2d(ji,jj,14) = zw2d(ji,jj,14) + zdetnh4 * ze3t
                  zw2d(ji,jj,15) = zw2d(ji,jj,15) + ( zno3phy + znh4phy - zphynh4 - zphydom - zphyzoo - zphydet ) * ze3t
                  zw2d(ji,jj,16) = zw2d(ji,jj,16) + ( zphyzoo + zdetzoo - zzoodet - zzoobod - zzoonh4 - zzoodom ) * ze3t
                  zw2d(ji,jj,17) = zw2d(ji,jj,17) + zdetdom * ze3t
                  !   
                  zw3d(ji,jj,jk,1) = zno3phy * 86400
                  zw3d(ji,jj,jk,2) = znh4phy * 86400
                  zw3d(ji,jj,jk,3) = znh4no3 * 86400
                   !
                ENDIF
            END DO
         END DO
      END DO

      IF( ln_diatrc ) THEN
         !
         DO jl = 1, 17 
            CALL lbc_lnk( zw2d(:,:,jl),'T', 1. )
         END DO
         DO jl = 1, 3
            CALL lbc_lnk( zw3d(:,:,:,jl),'T', 1. )
         END DO
         IF( lk_iomput ) THEN
            ! Save diagnostics
            CALL iom_put( "TNO3PHY", zw2d(:,:,1) )
            CALL iom_put( "TNH4PHY", zw2d(:,:,2) )
            CALL iom_put( "TPHYDOM", zw2d(:,:,3) )
            CALL iom_put( "TPHYNH4", zw2d(:,:,4) )
            CALL iom_put( "TPHYZOO", zw2d(:,:,5) )
            CALL iom_put( "TPHYDET", zw2d(:,:,6) )
            CALL iom_put( "TDETZOO", zw2d(:,:,7) )
            CALL iom_put( "TZOODET", zw2d(:,:,8) )
            CALL iom_put( "TZOOBOD", zw2d(:,:,9) )
            CALL iom_put( "TZOONH4", zw2d(:,:,10) )
            CALL iom_put( "TZOODOM", zw2d(:,:,11) )
            CALL iom_put( "TNH4NO3", zw2d(:,:,12) )
            CALL iom_put( "TDOMNH4", zw2d(:,:,13) )
            CALL iom_put( "TDETNH4", zw2d(:,:,14) )
            CALL iom_put( "TPHYTOT", zw2d(:,:,15) )
            CALL iom_put( "TZOOTOT", zw2d(:,:,16) )
            ! 
            CALL iom_put( "FNO3PHY", zw3d(:,:,:,1) )
            CALL iom_put( "FNH4PHY", zw3d(:,:,:,2) )
            CALL iom_put( "FNH4NO3", zw3d(:,:,:,3) )
            !
         ELSE
            !
            trc2d(:,:,jp_lob0_2d    ) = zw2d(:,:,1) 
            trc2d(:,:,jp_lob0_2d + 1) = zw2d(:,:,2) 
            trc2d(:,:,jp_lob0_2d + 2) = zw2d(:,:,3) 
            trc2d(:,:,jp_lob0_2d + 3) = zw2d(:,:,4) 
            trc2d(:,:,jp_lob0_2d + 4) = zw2d(:,:,5) 
            trc2d(:,:,jp_lob0_2d + 5) = zw2d(:,:,6) 
            trc2d(:,:,jp_lob0_2d + 6) = zw2d(:,:,7) 
                     ! trend number 8 is in trcsed.F
            trc2d(:,:,jp_lob0_2d +  8) = zw2d(:,:,8) 
            trc2d(:,:,jp_lob0_2d +  9) = zw2d(:,:,9) 
            trc2d(:,:,jp_lob0_2d + 10) = zw2d(:,:,10) 
            trc2d(:,:,jp_lob0_2d + 11) = zw2d(:,:,11) 
            trc2d(:,:,jp_lob0_2d + 12) = zw2d(:,:,12) 
            trc2d(:,:,jp_lob0_2d + 13) = zw2d(:,:,13) 
            trc2d(:,:,jp_lob0_2d + 14) = zw2d(:,:,14) 
            trc2d(:,:,jp_lob0_2d + 15) = zw2d(:,:,15) 
            trc2d(:,:,jp_lob0_2d + 16) = zw2d(:,:,16) 
            trc2d(:,:,jp_lob0_2d + 17) = zw2d(:,:,17) 
            ! trend number 19 is in trcexp.F
            trc3d(:,:,:,jp_lob0_3d    ) = zw3d(:,:,:,1) 
            trc3d(:,:,:,jp_lob0_3d + 1) = zw3d(:,:,:,2) 
            trc3d(:,:,:,jp_lob0_3d + 2) = zw3d(:,:,:,3) 
         ENDIF
        !
      ENDIF

      IF( ln_diabio .AND. .NOT. lk_iomput )  THEN
         DO jl = jp_lob0_trd, jp_lob1_trd
            CALL lbc_lnk( trbio(:,:,1,jl),'T', 1. )
         END DO 
      ENDIF
      !
      IF( l_trdtrc ) THEN
         DO jl = jp_lob0_trd, jp_lob1_trd
            CALL trd_mod_trc( trbio(:,:,:,jl), jl, kt )   ! handle the trend
         END DO
      ENDIF

      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('bio')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_diatrc ) THEN
         CALL wrk_dealloc( jpi, jpj,     17, zw2d )
         CALL wrk_dealloc( jpi, jpj, jpk, 3, zw3d )
      ENDIF
      !
      IF( nn_timing == 1 )  CALL timing_stop('trc_bio')
      !
   END SUBROUTINE trc_bio

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE trc_bio( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_bio: You should not have seen this print! error?', kt
   END SUBROUTINE trc_bio
#endif 

   !!======================================================================
END MODULE  trcbio
