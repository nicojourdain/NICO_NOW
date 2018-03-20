MODULE limthd_lac_2
#if defined key_lim2
   !!======================================================================
   !!                       ***  MODULE limthd_lac_2   ***
   !!                lateral thermodynamic growth of the ice 
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   lim_lat_acr_2   : lateral accretion of ice
   !!----------------------------------------------------------------------
   USE par_oce          ! ocean parameters
   USE phycst
   USE thd_ice_2
   USE ice_2
   USE limistate_2 
   USE lib_mpp          ! MPP library
   USE wrk_nemo         ! work arrays

   IMPLICIT NONE
   PRIVATE

   PUBLIC   lim_thd_lac_2   ! called by lim_thd_2

   REAL(wp)  ::           &  ! constant values
      epsi20 = 1.e-20  ,  &
      epsi13 = 1.e-13  ,  &
      zzero  = 0.e0    ,  &
      zone   = 1.e0

   !!----------------------------------------------------------------------
   !! NEMO/LIM2 3.3 , UCL - NEMO Consortium (2010)
   !! $Id: limthd_lac_2.F90 3294 2012-01-28 16:44:18Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS
    
   SUBROUTINE lim_thd_lac_2( kideb, kiut )
      !!-------------------------------------------------------------------
      !!               ***   ROUTINE lim_thd_lac_2  ***
      !!  
      !! ** Purpose : Computation of the evolution of the ice thickness and 
      !!      concentration as a function of the heat balance in the leads.
      !!      It is only used for lateral accretion
      !!       
      !! ** Method  : Ice is formed in the open water when ocean lose heat
      !!      (heat budget of open water Bl is negative) .
      !!      Computation of the increase of 1-A (ice concentration) fol-
      !!      lowing the law :
      !!      (dA/dt)acc = F[ (1-A)/(1-a) ] * [ Bl / (Li*h0) ]
      !!       where - h0 is the thickness of ice created in the lead
      !!             - a is a minimum fraction for leads
      !!             - F is a monotonic non-increasing function defined as:
      !!                  F(X)=( 1 - X**exld )**(1.0/exld)
      !!             - exld is the exponent closure rate (=2 default val.)
      !! 
      !! ** Action : - Adjustment of snow and ice thicknesses and heat
      !!                content in brine pockets
      !!             - Updating ice internal temperature
      !!             - Computation of variation of ice volume and mass
      !!             - Computation of frldb after lateral accretion and 
      !!               update h_snow_1d, h_ice_1d and tbif_1d(:,:)      
      !! 
      !! ** References :
      !!      M. Maqueda, 1995, PhD Thesis, Univesidad Complutense Madrid
      !!      Fichefet T. and M. Maqueda 1997, J. Geo. Res., 102(C6), 
      !!                                                12609 -12646   
      !! History :
      !!   1.0  !  01-04 (LIM)  original code
      !!   2.0  !  02-08 (C. Ethe, G. Madec)  F90, mpp
      !!-------------------------------------------------------------------
      INTEGER , INTENT(IN)::  &
         kideb          ,   &  ! start point on which the the computation is applied
         kiut                  ! end point on which the the computation is applied

      ! * Local variables
      INTEGER ::            &
         ji             ,   &  !  dummy loop indices
         iicefr         ,   &  !  1 = existing ice ; 0 = no ice
         iiceform       ,   &  !  1 = ice formed   ; 0 = no ice formed
         ihemis                !  dummy indice
      REAL(wp), POINTER, DIMENSION(:) ::   zqbgow      !  heat budget of the open water (negative)
      REAL(wp), POINTER, DIMENSION(:) ::   zfrl_old    !  previous sea/ice fraction
      REAL(wp), POINTER, DIMENSION(:) ::   zhice_old   !  previous ice thickness
      REAL(wp), POINTER, DIMENSION(:) ::   zhice0      !  thickness of newly formed ice in leads
      REAL(wp), POINTER, DIMENSION(:) ::   zfrlmin     !  minimum fraction for leads
      REAL(wp), POINTER, DIMENSION(:) ::   zdhicbot    !  part of thickness of newly formed ice in leads which 
                                !  has been already used in transport for example
      REAL(wp)  ::  &
         zhemis           ,  &  !  hemisphere (0 = North, 1 = South)
         zhicenew         ,  &  !  new ice thickness
         zholds2          ,  &  !  ratio of previous ice thickness and 2 
         zhnews2          ,  &  !  ratio of new ice thickness and 2 
         zfrlnew          ,  &  !  new sea/ice fraction
         zfrld            ,  &  !  ratio of sea/ice fraction and minimum fraction for leads
         zfrrate          ,  &  !  leads-closure rate
         zdfrl                  !  sea-ice fraction increment
      REAL(wp)  ::  &
         zdh1 , zdh2 , zdh3 , zdh4, zdh5   , &   ! tempory scalars
         ztint , zta1 , zta2 , zta3 , zta4 , &
         zah, zalpha , zbeta
      !!---------------------------------------------------------------------      
               
      CALL wrk_alloc( jpij, zqbgow, zfrl_old, zhice_old, zhice0, zfrlmin, zdhicbot )
      
      !--------------------------------------------------------------
      !   Computation of the heat budget of the open water (negative)
      !--------------------------------------------------------------
      
      DO ji = kideb , kiut      
         zqbgow(ji) = qldif_1d(ji) - qcmif_1d(ji)
      END DO
      
      !-----------------------------------------------------------------
      !   Taking the appropriate values for the corresponding hemisphere
      !-----------------------------------------------------------------
      DO ji = kideb , kiut
         zhemis       = MAX( zzero , SIGN( zone , frld_1d(ji) - 2.0 ) ) 
         ihemis       = INT( 1 + zhemis )
         zhice0  (ji) = hiccrit( ihemis ) 
         zfrlmin (ji) = acrit  ( ihemis )   
         frld_1d (ji) = frld_1d(ji) - 2.0 * zhemis
         zfrl_old(ji) = frld_1d(ji)
      END DO
      
      !-------------------------------------------------------------------
      !     Lateral Accretion (modification of the fraction of open water)
      !     The ice formed in the leads has always a thickness zhice0, but
      !     only a fraction zfrrate of the ice formed contributes to the 
      !     increase of the ice fraction. The remaining part (1-zfrrate)
      !     is rather assumed to lead to an increase in the thickness of the
      !     pre-existing ice (transport for example). 
      !     Morales Maqueda, 1995 - Fichefet and Morales Maqueda, 1997
      !---------------------------------------------------------------------
      
!CDIR NOVERRCHK
      DO ji = kideb , kiut
         iicefr       = 1 - MAX( 0, INT( SIGN( 1.5 * zone , zfrl_old(ji) - 1.0 + epsi13 ) ) )
         !---computation of the leads-closure rate
         zfrld        = MIN( zone , ( 1.0 - frld_1d(ji) ) / ( 1.0 - zfrlmin(ji) ) )
         zfrrate      = ( 1.0 - zfrld**exld )**( 1.0 / exld )
         !--computation of the sea-ice fraction increment and the new fraction 
         zdfrl        = ( zfrrate / zhice0(ji) )  * ( zqbgow(ji) / xlic )
         zfrlnew      = zfrl_old(ji) + zdfrl
         !--update the sea-ice fraction 
         frld_1d   (ji) = MAX( zfrlnew , zfrlmin(ji) )
         !--computation of the remaining part of ice thickness which has been already used
         zdhicbot(ji) =  ( frld_1d(ji) - zfrlnew ) * zhice0(ji) / ( 1.0 - zfrlmin(ji) ) & 
                      -  (  ( 1.0 - zfrrate ) / ( 1.0 - frld_1d(ji) ) )  * ( zqbgow(ji) / xlic ) 
      END DO
 
      !----------------------------------------------------------------------------------------
      !      Ajustement of snow and ice thicknesses and updating the total heat stored in brine pockets  
      !      The thickness of newly formed ice is averaged with that of the pre-existing
      !         (1-Anew) * hinew = (1-Aold) * hiold + ((1-Anew)-(1-Aold)) * h0
      !      Snow is distributed over the new ice-covered area 
      !         (1-Anew) * hsnew = (1-Aold) * hsold            
      !--------------------------------------------------------------------------------------------
      
      DO ji = kideb , kiut
         iicefr       = 1 - MAX( 0, INT( SIGN( 1.5 * zone , zfrl_old(ji) - 1.0 + epsi13 ) ) )
         zhice_old(ji) = h_ice_1d(ji)
         zhicenew      = iicefr * zhice_old(ji) + ( 1 - iicefr ) * zhice0(ji)
         zalpha        = ( 1. - zfrl_old(ji) ) / ( 1.- frld_1d(ji) )
         h_snow_1d(ji) = zalpha * h_snow_1d(ji)
         h_ice_1d (ji) = zalpha * zhicenew + ( 1.0 - zalpha ) * zhice0(ji)
         qstbif_1d(ji) = zalpha * qstbif_1d(ji) 
      END DO
      
      !-------------------------------------------------------
      !   Ajustement of ice internal temperatures
      !-------------------------------------------------------
      
      DO ji = kideb , kiut
         iicefr      = 1 - MAX( 0, INT( SIGN( 1.5 * zone , zfrl_old(ji) - 1.0 + epsi13 ) ) )
         iiceform    = 1 - MAX( 0 ,INT( SIGN( 1.5 * zone , zhice0(ji) - h_ice_1d(ji) ) ) )
         zholds2     = zhice_old(ji)/ 2.
         zhnews2     = h_ice_1d(ji) / 2.
         zdh1        = MAX( zzero ,  zhice_old(ji)   - zhnews2 )
         zdh2        = MAX( zzero , -zhice_old(ji)   + zhnews2 )
         zdh3        = MAX( zzero ,  h_ice_1d(ji) - zholds2 )
         zdh4        = MAX( zzero , -h_ice_1d(ji) + zholds2 )
         zdh5        = MAX( zzero , zhice0(ji)      - zholds2 )
         ztint       =       iiceform   * (  ( zholds2 - zdh3 ) * tbif_1d(ji,3) + zdh4 * tbif_1d(ji,2) )      &
            &                           / MAX( epsi20 , h_ice_1d(ji) - zhice0(ji) )                           &
            &                 + ( 1 - iiceform ) * tfu_1d(ji)
         zta1        = iicefr * ( 1.  - zfrl_old(ji) ) * tbif_1d(ji,2) 
         zta2        = iicefr * ( 1.  - zfrl_old(ji) ) * tbif_1d(ji,3)
         zta3        = iicefr * ( 1.  - zfrl_old(ji) ) * ztint
         zta4        = ( zfrl_old(ji) - frld_1d   (ji) ) * tfu_1d(ji)
         zah         = ( 1. - frld_1d(ji) ) * zhnews2 

         tbif_1d(ji,2) = (  MIN( zhnews2 , zholds2 )                                              * zta1   &
            &          + ( 1 - iiceform ) * ( zholds2 - zdh1 )                                    * zta2   &
            &          + ( iiceform * ( zhnews2 - zhice0(ji) + zdh5 ) + ( 1 - iiceform ) * zdh2 ) * zta3   & 
            &          + MIN ( zhnews2 , zhice0(ji) )                                             * zta4   &
            &          ) / zah
         
         tbif_1d(ji,3) =     (  iiceform * ( zhnews2 - zdh3 )                                          * zta1  &
            &              + ( iiceform * zdh3 + ( 1 - iiceform ) * zdh1 )                             * zta2  &
            &              + ( iiceform * ( zhnews2 - zdh5 ) + ( 1 - iiceform ) * ( zhnews2 - zdh1 ) ) * zta3  & 
            &              + ( iiceform * zdh5 + ( 1 - iiceform ) * zhnews2 )                          * zta4  &
            &            ) / zah
         !---removing the remaining part of ice formed which has been already used
         zbeta         = h_ice_1d(ji) / ( h_ice_1d(ji) + zdhicbot(ji) )
         h_ice_1d(ji)  = h_ice_1d(ji) + zdhicbot(ji)
         tbif_1d (ji,2)= zbeta * tbif_1d(ji,2) + ( 1.0 - zbeta ) * tbif_1d(ji,3)
         tbif_1d (ji,3)= ( 2. * zbeta - 1.0 ) * tbif_1d(ji,3) + ( 2. * zdhicbot(ji) / h_ice_1d(ji) ) * tfu_1d(ji)
         
      END DO
      
      !-------------------------------------------------------------
      !    Computation of variation of ice volume and ice mass 
      !           Vold = (1-Aold) * hiold ; Vnew = (1-Anew) * hinew
      !           dV = Vnew - Vold
      !-------------------------------------------------------------
      
      DO ji = kideb , kiut
         dvlbq_1d  (ji) = ( 1. - frld_1d(ji) ) * h_ice_1d(ji) - ( 1. - zfrl_old(ji) ) * zhice_old(ji)
         rdmicif_1d(ji) = rdmicif_1d(ji) + rhoic * dvlbq_1d(ji)
      END DO
      
      CALL wrk_dealloc( jpij, zqbgow, zfrl_old, zhice_old, zhice0, zfrlmin, zdhicbot )
      !
   END SUBROUTINE lim_thd_lac_2
#else
   !!----------------------------------------------------------------------
   !!                       ***  MODULE limthd_lac_2   ***
   !!                           no sea ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE lim_thd_lac_2           ! Empty routine
   END SUBROUTINE lim_thd_lac_2
#endif
   !!======================================================================
END MODULE limthd_lac_2
