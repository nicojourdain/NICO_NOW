MODULE par_sed
   !!======================================================================
   !!                        ***  par_sed  ***
   !! Sediment :   set sediment parameter
   !!======================================================================
   !! History :
   !!        !  06-12  (C. Ethe)  Orignal
   !!----------------------------------------------------------------------
#if defined key_sed
   !! Domain characteristics
   USE par_kind
   USE par_oce , ONLY :       &
      jpi      =>   jpi   ,  & !: first  dimension of grid --> i
      jpj      =>   jpj   ,  & !: second dimension of grid --> j
      jpim1    =>   jpim1 ,  & !: jpi - 1
      jpjm1    =>   jpjm1 ,  & !: jpj - 1
      jpij     =>   jpij       !: jpi x jpj
      jp_tem   =>   jp_tem     !: indice of temperature
      jp_sal   =>   jp_sal     !: indice of salintity

#if ! defined key_sed_off
   USE par_pisces
#endif

#if defined key_kriest
   INTEGER, PARAMETER :: jpdta = 11
#else
   INTEGER, PARAMETER :: jpdta = 12
#endif


   ! Vertical sediment geometry
   INTEGER, PARAMETER :: &
      jpksed   = 11         ,   & !: number of sediment layers
      jpksedm1 = jpksed - 1 
      
   ! sediment tracer species
   INTEGER, PARAMETER :: &
      jpsol = 4,         &  !: number of solid component
      jpwat = 7,         &   !: number of pore water component
      jpwatp1 = jpwat + 1

   
   ! pore water components       
   INTEGER, PARAMETER :: &
      jwsil  = 1,        & !: silic acid
      jwoxy  = 2,        & !: oxygen
      jwdic  = 3,        & !: dissolved inorganic carbon
      jwno3  = 4,        & !: nitrate
      jwpo4  = 5,        & !: phosphate
      jwalk  = 6,        & !: alkalinity
      jwc13  = 7           !: dissolved inorganic carbon 13

   ! solid components       
   INTEGER, PARAMETER ::  &
      jsopal  = 1,        & !: opal sediment
      jsclay  = 2,        & !: clay
      jspoc   = 3,        & !: organic carbon
      jscal   = 4           !: calcite

   INTEGER, PARAMETER ::  &
      jptrased   = jpsol + jpwat , &
      jpdia3dsed = 3          , &
      jpdia2dsed = 7
#endif
END MODULE par_sed
