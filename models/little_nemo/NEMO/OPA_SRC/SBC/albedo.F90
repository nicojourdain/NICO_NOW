MODULE albedo
   !!======================================================================
   !!                       ***  MODULE  albedo  ***
   !! Ocean forcing:  bulk thermohaline forcing of the ocean (or ice)
   !!=====================================================================
   !! History :  8.0  ! 2001-04  (LIM 1.0)
   !!   NEMO     1.0  ! 2003-07  (C. Ethe, G. Madec)  Optimization (old name:shine)
   !!             -   ! 2004-11  (C. Talandier)  add albedo_init
   !!             -   ! 2001-06  (M. Vancoppenolle) LIM 3.0
   !!             -   ! 2006-08  (G. Madec)  cleaning for surface module
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   albedo_ice  : albedo for   ice (clear and overcast skies)
   !!   albedo_oce  : albedo for ocean (clear and overcast skies)
   !!   albedo_init : initialisation of albedo computation
   !!----------------------------------------------------------------------
   USE phycst          ! physical constants
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! MPP library
   USE wrk_nemo        ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC   albedo_ice   ! routine called sbcice_lim.F90
   PUBLIC   albedo_oce   ! routine called by ???

   INTEGER  ::   albd_init = 0      !: control flag for initialization
   REAL(wp) ::   zzero     = 0.e0   ! constant values
   REAL(wp) ::   zone      = 1.e0   !    "       "

   REAL(wp) ::   c1     = 0.05    ! constants values
   REAL(wp) ::   c2     = 0.10    !    "        "
   REAL(wp) ::   rmue   = 0.40    !  cosine of local solar altitude

   !                               !!* namelist namsbc_alb
   REAL(wp) ::   rn_cloud  = 0.06   !  cloudiness effect on snow or ice albedo (Grenfell & Perovich, 1984)
#if defined key_lim3
   REAL(wp) ::   rn_albice = 0.53   !  albedo of melting ice in the arctic and antarctic (Shine & Hendersson-Sellers)
#else
   REAL(wp) ::   rn_albice = 0.50   !  albedo of melting ice in the arctic and antarctic (Shine & Hendersson-Sellers)
#endif
   REAL(wp) ::   rn_alphd  = 0.80   !  coefficients for linear interpolation used to compute
   REAL(wp) ::   rn_alphdi = 0.72   !  albedo between two extremes values (Pyane, 1972)
   REAL(wp) ::   rn_alphc  = 0.65   ! 

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id: albedo.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE albedo_ice( pt_ice, ph_ice, ph_snw, pa_ice_cs, pa_ice_os )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE albedo_ice  ***
      !!          
      !! ** Purpose :   Computation of the albedo of the snow/ice system 
      !!                as well as the ocean one
      !!       
      !! ** Method  : - Computation of the albedo of snow or ice (choose the 
      !!                rignt one by a large number of tests
      !!              - Computation of the albedo of the ocean
      !!
      !! References :   Shine and Hendersson-Sellers 1985, JGR, 90(D1), 2243-2250.
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:) ::   pt_ice      !  ice surface temperature (Kelvin)
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:) ::   ph_ice      !  sea-ice thickness
      REAL(wp), INTENT(in   ), DIMENSION(:,:,:) ::   ph_snw      !  snow thickness
      REAL(wp), INTENT(  out), DIMENSION(:,:,:) ::   pa_ice_cs   !  albedo of ice under clear    sky
      REAL(wp), INTENT(  out), DIMENSION(:,:,:) ::   pa_ice_os   !  albedo of ice under overcast sky
      !!
      INTEGER  ::   ji, jj, jl    ! dummy loop indices
      INTEGER  ::   ijpl          ! number of ice categories (3rd dim of ice input arrays)
      REAL(wp) ::   zalbpsnm      ! albedo of ice under clear sky when snow is melting
      REAL(wp) ::   zalbpsnf      ! albedo of ice under clear sky when snow is freezing
      REAL(wp) ::   zalbpsn       ! albedo of snow/ice system when ice is coverd by snow
      REAL(wp) ::   zalbpic       ! albedo of snow/ice system when ice is free of snow
      REAL(wp) ::   zithsn        ! = 1 for hsn >= 0 ( ice is cov. by snow ) ; = 0 otherwise (ice is free of snow)
      REAL(wp) ::   zitmlsn       ! = 1 freezinz snow (pt_ice >=rt0_snow) ; = 0 melting snow (pt_ice<rt0_snow)
      REAL(wp) ::   zihsc1        ! = 1 hsn <= c1 ; = 0 hsn > c1
      REAL(wp) ::   zihsc2        ! = 1 hsn >= c2 ; = 0 hsn < c2
      !!
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zalbfz    ! = rn_alphdi for freezing ice ; = rn_albice for melting ice
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   zficeth   !  function of ice thickness
      !!---------------------------------------------------------------------
      
      ijpl = SIZE( pt_ice, 3 )                     ! number of ice categories

      CALL wrk_alloc( jpi,jpj,ijpl, zalbfz, zficeth )

      IF( albd_init == 0 )   CALL albedo_init      ! initialization 

      !---------------------------
      !  Computation of  zficeth
      !---------------------------
      ! ice free of snow and melts
      WHERE     ( ph_snw == 0._wp .AND. pt_ice >= rt0_ice )   ;   zalbfz(:,:,:) = rn_albice
      ELSE WHERE                                              ;   zalbfz(:,:,:) = rn_alphdi
      END  WHERE

      WHERE     ( 1.5  < ph_ice                     )  ;  zficeth = zalbfz
      ELSE WHERE( 1.0  < ph_ice .AND. ph_ice <= 1.5 )  ;  zficeth = 0.472  + 2.0 * ( zalbfz - 0.472 ) * ( ph_ice - 1.0 )
      ELSE WHERE( 0.05 < ph_ice .AND. ph_ice <= 1.0 )  ;  zficeth = 0.2467 + 0.7049 * ph_ice              &
         &                                                                 - 0.8608 * ph_ice * ph_ice     &
         &                                                                 + 0.3812 * ph_ice * ph_ice * ph_ice
      ELSE WHERE                                       ;  zficeth = 0.1    + 3.6    * ph_ice
      END WHERE

!!gm old code
!      DO jl = 1, ijpl
!         DO jj = 1, jpj
!            DO ji = 1, jpi
!               IF( ph_ice(ji,jj,jl) > 1.5 ) THEN
!                  zficeth(ji,jj,jl) = zalbfz(ji,jj,jl)
!               ELSEIF( ph_ice(ji,jj,jl) > 1.0  .AND. ph_ice(ji,jj,jl) <= 1.5 ) THEN
!                  zficeth(ji,jj,jl) = 0.472 + 2.0 * ( zalbfz(ji,jj,jl) - 0.472 ) * ( ph_ice(ji,jj,jl) - 1.0 )
!               ELSEIF( ph_ice(ji,jj,jl) > 0.05 .AND. ph_ice(ji,jj,jl) <= 1.0 ) THEN
!                  zficeth(ji,jj,jl) = 0.2467 + 0.7049 * ph_ice(ji,jj,jl)                               &
!                     &                    - 0.8608 * ph_ice(ji,jj,jl) * ph_ice(ji,jj,jl)                 &
!                     &                    + 0.3812 * ph_ice(ji,jj,jl) * ph_ice(ji,jj,jl) * ph_ice (ji,jj,jl)
!               ELSE
!                  zficeth(ji,jj,jl) = 0.1 + 3.6 * ph_ice(ji,jj,jl) 
!               ENDIF
!            END DO
!         END DO
!      END DO
!!gm end old code
      
      !----------------------------------------------- 
      !    Computation of the snow/ice albedo system 
      !-------------------------- ---------------------
      
      !    Albedo of snow-ice for clear sky.
      !-----------------------------------------------    
      DO jl = 1, ijpl
         DO jj = 1, jpj
            DO ji = 1, jpi
               !  Case of ice covered by snow.             
               !                                        !  freezing snow        
               zihsc1   = 1.0 - MAX( zzero , SIGN( zone , - ( ph_snw(ji,jj,jl) - c1 ) ) )
               zalbpsnf = ( 1.0 - zihsc1 ) * (  zficeth(ji,jj,jl)                                             &
                  &                           + ph_snw(ji,jj,jl) * ( rn_alphd - zficeth(ji,jj,jl) ) / c1  )   &
                  &     +         zihsc1   * rn_alphd  
               !                                        !  melting snow                
               zihsc2   = MAX( zzero , SIGN( zone , ph_snw(ji,jj,jl) - c2 ) )
               zalbpsnm = ( 1.0 - zihsc2 ) * ( rn_albice + ph_snw(ji,jj,jl) * ( rn_alphc - rn_albice ) / c2 )   &
                  &     +         zihsc2   *   rn_alphc 
               !
               zitmlsn  =  MAX( zzero , SIGN( zone , pt_ice(ji,jj,jl) - rt0_snow ) )   
               zalbpsn  =  zitmlsn * zalbpsnm + ( 1.0 - zitmlsn ) * zalbpsnf
            
               !  Case of ice free of snow.
               zalbpic  = zficeth(ji,jj,jl) 
            
               ! albedo of the system   
               zithsn   = 1.0 - MAX( zzero , SIGN( zone , - ph_snw(ji,jj,jl) ) )
               pa_ice_cs(ji,jj,jl) =  zithsn * zalbpsn + ( 1.0 - zithsn ) *  zalbpic
            END DO
         END DO
      END DO
      
      !    Albedo of snow-ice for overcast sky.
      !----------------------------------------------  
      pa_ice_os(:,:,:) = pa_ice_cs(:,:,:) + rn_cloud       ! Oberhuber correction
      !
      CALL wrk_dealloc( jpi,jpj,ijpl, zalbfz, zficeth )
      !
   END SUBROUTINE albedo_ice


   SUBROUTINE albedo_oce( pa_oce_os , pa_oce_cs )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE albedo_oce  ***
      !! 
      !! ** Purpose :   Computation of the albedo of the ocean
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   pa_oce_os   !  albedo of ocean under overcast sky
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   pa_oce_cs   !  albedo of ocean under clear sky
      !!
      REAL(wp) ::   zcoef   ! local scalar
      !!----------------------------------------------------------------------
      !
      zcoef = 0.05 / ( 1.1 * rmue**1.4 + 0.15 )      ! Parameterization of Briegled and Ramanathan, 1982 
      pa_oce_cs(:,:) = zcoef               
      pa_oce_os(:,:)  = 0.06                         ! Parameterization of Kondratyev, 1969 and Payne, 1972
      !
   END SUBROUTINE albedo_oce


   SUBROUTINE albedo_init
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE albedo_init  ***
      !!
      !! ** Purpose :   initializations for the albedo parameters
      !!
      !! ** Method  :   Read the namelist namsbc_alb
      !!----------------------------------------------------------------------
      NAMELIST/namsbc_alb/ rn_cloud, rn_albice, rn_alphd, rn_alphdi, rn_alphc
      !!----------------------------------------------------------------------
      !
      albd_init = 1                     ! indicate that the initialization has been done
      !
      REWIND( numnam )                  ! Read Namelist namsbc_alb : albedo parameters
      READ  ( numnam, namsbc_alb )
      !
      IF(lwp) THEN                      ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'albedo : set albedo parameters'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*) '   Namelist namsbc_alb : albedo '
         WRITE(numout,*) '      correction for snow and ice albedo                  rn_cloud  = ', rn_cloud
         WRITE(numout,*) '      albedo of melting ice in the arctic and antarctic   rn_albice = ', rn_albice
         WRITE(numout,*) '      coefficients for linear                             rn_alphd  = ', rn_alphd
         WRITE(numout,*) '      interpolation used to compute albedo                rn_alphdi = ', rn_alphdi
         WRITE(numout,*) '      between two extremes values (Pyane, 1972)           rn_alphc  = ', rn_alphc
      ENDIF
      !
   END SUBROUTINE albedo_init

   !!======================================================================
END MODULE albedo
