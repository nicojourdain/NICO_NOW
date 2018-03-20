MODULE seddta
   !!======================================================================
   !!                     ***  MODULE  seddta  ***
   !! Sediment data  :  read sediment input data from a file
   !!=====================================================================
#if defined key_sed
   !! * Modules used
   USE sed
   USE sedarr
   USE iom

   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC sed_dta   ! 

   !! *  Module variables
   REAL(wp), DIMENSION(:), ALLOCATABLE ::  smask       ! mask for sediments points
   REAL(wp) ::  rsecday  ! number of second per a day
   REAL(wp) ::  conv1    ! [m/day]--->[cm/s]  
   REAL(wp) ::  conv2    ! [kg/m2/month]-->[g/cm2/s] ( 1 month has 30 days )

   INTEGER ::  numbio

#if defined key_sed_off
   INTEGER ::  numoce
#endif

CONTAINS

   !!---------------------------------------------------------------------------
   !!   sed_dta  : read the NetCDF data file in online version using module iom
   !!---------------------------------------------------------------------------

   SUBROUTINE sed_dta( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_dta  ***
      !!                    
      !! ** Purpose :   Reads data from a netcdf file and 
      !!                initialization of rain and pore water (k=1) components
      !! 
      !!
      !!   History :
      !!        !  04-10  (N. Emprin, M. Gehlen )  Original code
      !!        !  06-04  (C. Ethe)  Re-organization ; Use of iom
      !!----------------------------------------------------------------------

      !! Arguments
      INTEGER, INTENT(in) ::  kt    ! time-step

      !! * Local declarations
      INTEGER  ::  ji, jj, js, jw, ikt

      REAL(wp), DIMENSION(:,:), ALLOCATABLE :: zdta
#if ! defined key_kriest
      REAL(wp), DIMENSION(:)  , ALLOCATABLE :: zdtap, zdtag
#endif 


      !----------------------------------------------------------------------

      ! Initialization of sediment variable 
      ! Spatial dimension is merged, and unity converted if needed
      !-------------------------------------------------------------

      WRITE(numsed,*)
      WRITE(numsed,*) ' sed_dta : Bottom layer fields'
      WRITE(numsed,*) ' ~~~~~~'
      WRITE(numsed,*) ' Data from SMS model'
      WRITE(numsed,*)


      ! open file
      IF( kt == nitsed000 ) THEN
         WRITE(numsed,*) ' sed_dta : Sediment fields'
         CALL iom_open ( 'data_bio_bot'     , numbio )
#if defined key_sed_off
         CALL iom_open( 'data_oce_bot', numoce)
#endif
         rsecday = 60.* 60. * 24.
         conv1   = 1.0e+2 / rsecday 
         conv2   = 1.0e+3 / ( 1.0e+4 * rsecday * 30. ) 

         ! Compute sediment mask
         ALLOCATE( zdta(jpi,jpj) ) 
         DO jj = 1, jpj
            DO ji = 1, jpi
               ikt = MAX( INT( sbathy(ji,jj) ) - 1, 1 )
               zdta(ji,jj) = tmask(ji,jj,ikt) 
            ENDDO
         ENDDO
         ALLOCATE( smask(jpoce) )
         smask(:) = 0.
         CALL pack_arr( jpoce, smask(1:jpoce), zdta(1:jpi,1:jpj), iarroce(1:jpoce) )
      ENDIF


#if ! defined key_kriest   
      ! Initialization of temporaries arrays  
      ALLOCATE( zdtap(jpoce) )    ;   zdtap(:)    = 0. 
      ALLOCATE( zdtag(jpoce) )    ;   zdtag(:)    = 0.  
#endif


      IF( MOD( kt - 1, nfreq ) == 0 ) THEN
         ! reading variables
         WRITE(numsed,*)
         WRITE(numsed,*) ' sed_dta : Bottom layer fields at time  kt = ', kt
         ! reading variables
         trc_data(:,:,:) = 0.
#if ! defined key_sed_off
         DO jj = 1,jpj
            DO ji = 1, jpi
               ikt = mbkt(ji,jj)
               IF ( tmask(ji,jj,ikt) == 1 ) THEN
                  trc_data(ji,jj,1)  = trn  (ji,jj,ikt,jptal)
                  trc_data(ji,jj,2)  = trn  (ji,jj,ikt,jpdic)
                  trc_data(ji,jj,3)  = trn  (ji,jj,ikt,jpno3) / 7.6
                  trc_data(ji,jj,4)  = trn  (ji,jj,ikt,jppo4) / 122.
                  trc_data(ji,jj,5)  = trn  (ji,jj,ikt,jpoxy)
                  trc_data(ji,jj,6)  = trn  (ji,jj,ikt,jpsil)
#   if ! defined key_kriest
                  trc_data(ji,jj,7 ) = sinksil (ji,jj,ikt)
                  trc_data(ji,jj,8 ) = sinking (ji,jj,ikt)
                  trc_data(ji,jj,9 ) = sinking2(ji,jj,ikt)
                  trc_data(ji,jj,10) = sinkcal (ji,jj,ikt)
                  trc_data(ji,jj,11) = tsn     (ji,jj,ikt,jp_tem)
                  trc_data(ji,jj,12) = tsn     (ji,jj,ikt,jp_sal)
#   else
                  trc_data(ji,jj,7 ) = sinksil (ji,jj,ikt)
                  trc_data(ji,jj,8 ) = sinking (ji,jj,ikt)
                  trc_data(ji,jj,9 ) = sinkcal (ji,jj,ikt)
                  trc_data(ji,jj,10) = tsn     (ji,jj,ikt,jp_tem)
                  trc_data(ji,jj,11) = tsn     (ji,jj,ikt,jp_sal)       
#   endif
               ENDIF
            ENDDO
         ENDDO

#else
         CALL iom_get( numbio, jpdom_data, 'ALKBOT'     , trc_data(:,:,1 ) )
         CALL iom_get( numbio, jpdom_data, 'DICBOT'     , trc_data(:,:,2 ) )
         CALL iom_get( numbio, jpdom_data, 'NO3BOT'     , trc_data(:,:,3 ) )
         CALL iom_get( numbio, jpdom_data, 'PO4BOT'     , trc_data(:,:,4 ) )
         CALL iom_get( numbio, jpdom_data, 'O2BOT'      , trc_data(:,:,5 ) )
         CALL iom_get( numbio, jpdom_data, 'SIBOT'      , trc_data(:,:,6 ) )
#   if ! defined key_kriest
         CALL iom_get( numbio, jpdom_data, 'OPALFLXBOT' , trc_data(:,:,7 ) ) 
         CALL iom_get( numbio, jpdom_data, 'POCFLXBOT'  , trc_data(:,:,8 ) ) 
         CALL iom_get( numbio, jpdom_data, 'GOCFLXBOT'  , trc_data(:,:,9 ) ) 
         CALL iom_get( numbio, jpdom_data, 'CACO3FLXBOT', trc_data(:,:,10) ) 
         CALL iom_get( numoce, jpdom_data, 'TBOT'       , trc_data(:,:,11) ) 
         CALL iom_get( numoce, jpdom_data, 'SBOT'       , trc_data(:,:,12) ) 
#   else
         CALL iom_get( numbio, jpdom_data, 'OPALFLXBOT' , trc_data(:,:,7 ) ) 
         CALL iom_get( numbio, jpdom_data, 'POCFLXBOT'  , trc_data(:,:,8 ) ) 
         CALL iom_get( numbio, jpdom_data, 'CACO3FLXBOT', trc_data(:,:,9 ) ) 
         CALL iom_get( numoce, jpdom_data, 'TBOT'       , trc_data(:,:,10) ) 
         CALL iom_get( numoce, jpdom_data, 'SBOT'       , trc_data(:,:,11) ) 
#   endif
#endif

         ! Pore water initial concentration [mol/l] in  k=1
         !-------------------------------------------------

          ! Alkalinity ( 1 umol = 10-6equivalent )
         CALL pack_arr ( jpoce,  pwcp_dta(1:jpoce,jwalk), trc_data(1:jpi,1:jpj,1), iarroce(1:jpoce) )
         ! DIC 
         CALL pack_arr ( jpoce,  pwcp_dta(1:jpoce,jwdic), trc_data(1:jpi,1:jpj,2), iarroce(1:jpoce) )
         ! Nitrates (1 umol/l = 10-6 mol/l)
         CALL pack_arr ( jpoce,  pwcp_dta(1:jpoce,jwno3), trc_data(1:jpi,1:jpj,3), iarroce(1:jpoce) )
         ! Phosphates (1 umol/l = 10-6 mol/l)
         CALL pack_arr ( jpoce,  pwcp_dta(1:jpoce,jwpo4), trc_data(1:jpi,1:jpj,4), iarroce(1:jpoce) )
         ! Oxygen (1 umol/l = 10-6 mol/l)
         CALL pack_arr ( jpoce,  pwcp_dta(1:jpoce,jwoxy), trc_data(1:jpi,1:jpj,5), iarroce(1:jpoce) )        
         ! Silicic Acid [mol.l-1]
         CALL pack_arr ( jpoce,  pwcp_dta(1:jpoce,jwsil), trc_data(1:jpi,1:jpj,6), iarroce(1:jpoce) )                  
         ! DIC13 (mol/l)obtained from dc13 and DIC (12) and PDB 
         CALL iom_get ( numbio,jpdom_data,'DC13',zdta(:,:) )
         CALL pack_arr ( jpoce,  pwcp_dta(1:jpoce,jwc13), zdta(1:jpi,1:jpj), iarroce(1:jpoce) )
         pwcp_dta(1:jpoce,jwc13) = pdb * ( pwcp_dta(1:jpoce,jwc13) * 1.0e-3 + 1.0 )  &
            &                          *   pwcp_dta(1:jpoce,jwdic)         
         
         !  Solid components : 
         !-----------------------
#if ! defined key_kriest
         !  Sinking fluxes for OPAL in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
         CALL pack_arr ( jpoce, rainrm_dta(1:jpoce,jsopal), trc_data(1:jpi,1:jpj,7), iarroce(1:jpoce) ) 
         rainrm_dta(1:jpoce,jsopal) = rainrm_dta(1:jpoce,jsopal) * 1e-4
         !  Sinking fluxes for POC in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
         CALL pack_arr ( jpoce, zdtap(1:jpoce), trc_data(1:jpi,1:jpj,8) , iarroce(1:jpoce) )      
         CALL pack_arr ( jpoce, zdtag(1:jpoce), trc_data(1:jpi,1:jpj,9) , iarroce(1:jpoce) )
         rainrm_dta(1:jpoce,jspoc) =   ( zdtap(1:jpoce) +  zdtag(1:jpoce) ) * 1e-4
         !  Sinking fluxes for Calcite in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
         CALL pack_arr ( jpoce,  rainrm_dta(1:jpoce,jscal), trc_data(1:jpi,1:jpj,10), iarroce(1:jpoce) )
         rainrm_dta(1:jpoce,jscal) = rainrm_dta(1:jpoce,jscal) * 1e-4
         ! vector temperature [°C] and salinity 
         CALL pack_arr ( jpoce,  temp(1:jpoce), trc_data(1:jpi,1:jpj,11), iarroce(1:jpoce) )
         CALL pack_arr ( jpoce,  salt(1:jpoce), trc_data(1:jpi,1:jpj,12), iarroce(1:jpoce) )
#else
         !  Sinking fluxes for OPAL in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
         CALL pack_arr ( jpoce, rainrm_dta(1:jpoce,jsopal), trc_data(1:jpi,1:jpj,7), iarroce(1:jpoce) ) 
         rainrm_dta(1:jpoce,jsopal) = rainrm_dta(1:jpoce,jsopal) * 1e-4
         !  Sinking fluxes for POC in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
         CALL pack_arr ( jpoce, rainrm_dta(1:jpoce,jspoc), trc_data(1:jpi,1:jpj,8) , iarroce(1:jpoce) )      
         rainrm_dta(1:jpoce,jspoc) = rainrm_dta(1:jpoce,jspoc) * 1e-4
         !  Sinking fluxes for Calcite in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
         CALL pack_arr ( jpoce,  rainrm_dta(1:jpoce,jscal), trc_data(1:jpi,1:jpj,9), iarroce(1:jpoce) )
         rainrm_dta(1:jpoce,jscal) = rainrm_dta(1:jpoce,jscal) * 1e-4
         ! vector temperature [°C] and salinity 
         CALL pack_arr ( jpoce,  temp(1:jpoce), trc_data(1:jpi,1:jpj,10), iarroce(1:jpoce) )
         CALL pack_arr ( jpoce,  salt(1:jpoce), trc_data(1:jpi,1:jpj,11), iarroce(1:jpoce) )

#endif
        
         ! Clay rain rate in [mol/(cm**2.s)] 
         ! inputs data in [kg.m-2.mois-1] ---> 1e+3/(1e+4*60*24*60*60) [g.cm-2.s-1]   
         ! divided after by molecular weight g.mol-1      
         zdta(:,:)   = 0.
         CALL iom_get( numbio, jpdom_data, 'CLAY', zdta(:,:) ) 
         CALL pack_arr ( jpoce, rainrm_dta(1:jpoce,jsclay) , zdta(1:jpi,1:jpj), iarroce(1:jpoce) )      
         rainrm_dta(1:jpoce,jsclay) = rainrm_dta(1:jpoce,jsclay) * conv2 / mol_wgt(jsclay)

      ENDIF

      ! sediment pore water at 1st layer (k=1)
      DO jw = 1, jpwat
         pwcp(1:jpoce,1,jw) = pwcp_dta(1:jpoce,jw) * smask(1:jpoce)
      ENDDO

      !  rain
      DO js = 1, jpsol
         rainrm(1:jpoce,js) = rainrm_dta(1:jpoce,js) * smask(1:jpoce)
      ENDDO

      ! Calculation of raintg of each sol. comp.: rainrm in [g/(cm**2.s)]
      DO js = 1, jpsol
         rainrg(1:jpoce,js) = rainrm(1:jpoce,js) *  mol_wgt(js)
      ENDDO

      ! Calculation of raintg = total massic flux rained in each cell (sum of sol. comp.)
      raintg(:) = 0.
      DO js = 1, jpsol
         raintg(1:jpoce) = raintg(1:jpoce) + rainrg(1:jpoce,js)
      ENDDO

      ! computation of dzdep = total thickness of solid material rained [cm] in each cell
      dzdep(1:jpoce) = raintg(1:jpoce) * rdtsed(2) 


      DEALLOCATE( zdta ) 
#if ! defined key_kriest
      DEALLOCATE( zdtap    ) ;  DEALLOCATE( zdtag    ) 
#endif      

      IF( kt == nitsedend )   THEN
         CALL iom_close ( numbio )
#if defined key_sed_off
         CALL iom_close ( numoce )
#endif
      ENDIF
      
   END SUBROUTINE sed_dta

#else
   !!======================================================================
   !! MODULE seddta  :   Dummy module 
   !!======================================================================
CONTAINS
   SUBROUTINE sed_dta ( kt )
     INTEGER, INTENT(in) :: kt
     WRITE(*,*) 'sed_stp: You should not have seen this print! error?', kt 
  END SUBROUTINE sed_dta
#endif

END MODULE seddta
