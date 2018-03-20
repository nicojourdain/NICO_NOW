MODULE sedini
   !!======================================================================
   !!              ***  MODULE  sedini  ***
   !! Sediment : define sediment variables
   !!=====================================================================
#if defined key_sed

   !!----------------------------------------------------------------------
   !!   sed_init    : initialization, namelist read, and parameters control
   !!----------------------------------------------------------------------
   !! * Modules used
   USE sed     ! sediment global variable
   USE seddta
   USE sedrst
   USE sedco3
   USE sedchem
   USE sedarr
   USE iom


   IMPLICIT NONE
   PRIVATE

   !! Module variables
   REAL(wp)    ::  &
      sisat   =  800.  ,  &  !: saturation for si    [ mol.l-1]
      claysat =    0.  ,  &  !: saturation for clay  [ mol.l-1]
      rcopal  =   40.  ,  &  !: reactivity for si    [l.mol-1.an-1]
      rcclay  =    0.  ,  &  !: reactivity for clay  [l.mol-1.an-1]
      dcoef   =  8.e-6       !: diffusion coefficient (*por)   [cm**2/s]

   REAL(wp)    ::  &
      redO2   =  172.  ,  &  !: Redfield coef for Oxygene
      redNo3  =   16.  ,  &  !: Redfield coef for Nitrates
      redPo4  =    1.  ,  &  !: Redfield coef for Phosphates
      redC    =  122.  ,  &  !: Redfield coef for Carbone
      redDnit =  97.6  ,  &  !: Redfield coef for denitrification    
      rcorg   =   50.  ,  &  !: reactivity for POC/O2 [l.mol-1.an-1]    
      o2seuil =    1.  ,  &  !: threshold O2 concentration for [mol.l-1]     
      rcorgN  =   50.       !: reactivity for POC/No3 [l.mol-1.an-1]

   REAL(wp)    ::  &
      rccal   = 1000.        !: reactivity for calcite         [l.mol-1.an-1]

   REAL(wp)    ::  &
      dbiot   = 15.          !: coefficient for bioturbation    [cm**2.(1000an-1)]

   LOGICAL     :: &
      ln_rst_sed = .TRUE.    !: initialisation from a restart file or not

   REAL(wp)    ::  &
      ryear = 365. * 24. * 3600. !:  1 year converted in second

   !! *  Routine accessibility
   PUBLIC sed_init          ! routine called by opa.F90

CONTAINS


   SUBROUTINE sed_init
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_init  ***
      !!
      !! ** Purpose :  Initialization of sediment module
      !!               - Reading namelist
      !!               - Read the deepest water layer thickness
      !!                 ( using as mask ) in Netcdf file
      !!               - Convert unity if necessary
      !!               - sets initial sediment composition
      !!                 ( only clay or reading restart file )
      !!               - sets sediment grid, porosity and others constants
      !!
      !!   History :
      !!        !  04-10  (N. Emprin, M. Gehlen )  Original code
      !!        !  06-07  (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------
      INTEGER :: ji, jj, ikt
#if defined key_sed_off
      INTEGER  :: numblt         
      INTEGER  :: nummsh   
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: zdta
#endif
      !!----------------------------------------------------------------------


      ! Reading namelist.sed variables
      !---------------------------------------

      CALL ctl_opn( numsed, 'sediment.output', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )

      WRITE(numsed,*)
      WRITE(numsed,*) '                 L S C E - C E A'
      WRITE(numsed,*) '                 SEDIMENT model'
      WRITE(numsed,*) '                version 2.0  (2007) '
      WRITE(numsed,*)
      WRITE(numsed,*)

    
      WRITE(numsed,*) ' sed_init : Initialization of sediment module  '
      WRITE(numsed,*) ' '

      !                                ! Allocate LOBSTER arrays
      IF( sed_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'sed_ini: unable to allocate sediment model arrays' )


      ! Determination of sediments number of points and allocate global variables
#if defined key_sed_off
      ! Reading Netcdf Pisces file for deepest water layer thickness [m]
      ! This data will be used as mask to merdge space in 1D vector
      !----------------------------------------------------------------

      CALL iom_open ( 'mesh_mask'  , nummsh )   
      CALL iom_open ( 'e3tbot'     , numblt )

      ! mask sediment points for outputs
      CALL iom_get( nummsh, jpdom_data, 'tmask' , tmask )
      CALL iom_get( nummsh, jpdom_data, 'mbathy', sbathy )
      
      ! longitude/latitude values
      CALL iom_get ( nummsh, jpdom_data,'nav_lon', glamt(:,:) )
      CALL iom_get ( nummsh, jpdom_data,'nav_lat', gphit(:,:) )
    
      ! bottom layer thickness
      ALLOCATE( zdta(jpi,jpj) )
      CALL iom_get  ( numblt, jpdom_data, 'E3TBOT', zdta(:,:) )
      epkbot(:,:) = 0.
      DO jj = 1, jpj
         DO ji = 1, jpi
            ikt = MAX( INT( sbathy(ji,jj) ), 1 )
            IF( tmask(ji,jj,ikt) == 1 ) epkbot(ji,jj) = zdta(ji,jj)
         ENDDO
      ENDDO

      CALL iom_close( nummsh )  
      CALL iom_close( numblt ) 

      DEALLOCATE( zdta )
#else

      epkbot(:,:) = 0.
      DO jj = 1, jpj
         DO ji = 1, jpi
            ikt = mbkt(ji,jj) 
            IF( tmask(ji,jj,ikt) == 1 ) epkbot(ji,jj) = e3t_0(ikt)
         ENDDO
      ENDDO
#endif     


      ! computation of total number of ocean points
      !--------------------------------------------
      jpoce  = COUNT( epkbot(:,:) > 0. )


      ! Allocate memory size of global variables
      ALLOCATE( pwcp (jpoce,jpksed,jpwat) )  ;  ALLOCATE( pwcp0 (jpoce,jpksed,jpwat) ) ;  ALLOCATE( pwcp_dta  (jpoce,jpwat) )
      ALLOCATE( solcp(jpoce,jpksed,jpsol) )  ;  ALLOCATE( solcp0(jpoce,jpksed,jpsol) ) ;  ALLOCATE( rainrm_dta(jpoce,jpsol) )
      ALLOCATE( rainrm(jpoce,jpsol) )        ;  ALLOCATE( rainrg(jpoce,jpsol) )        ;  ALLOCATE( raintg(jpoce) ) 
      ALLOCATE( dzdep(jpoce) )               ;  ALLOCATE( iarroce(jpoce) )             ;  ALLOCATE( dzkbot(jpoce) )
      ALLOCATE( temp(jpoce) )                ;  ALLOCATE( salt(jpoce) )                
      ALLOCATE( press(jpoce) )               ;  ALLOCATE( densSW(jpoce) ) 
      ALLOCATE( hipor(jpoce,jpksed) )        ;  ALLOCATE( co3por(jpoce,jpksed) ) 
      ALLOCATE( dz3d(jpoce,jpksed) )         ;  ALLOCATE( volw3d(jpoce,jpksed) )       ;  ALLOCATE( vols3d(jpoce,jpksed) )

      ! Initialization of global variables
      pwcp  (:,:,:) = 0.  ;  pwcp0 (:,:,:) = 0.  ; pwcp_dta  (:,:) = 0.  
      solcp (:,:,:) = 0.  ;  solcp0(:,:,:) = 0.  ; rainrm_dta(:,:) = 0.
      rainrm(:,:  ) = 0.  ;  rainrg(:,:  ) = 0.  ; raintg    (:  ) = 0. 
      dzdep (:    ) = 0.  ;  iarroce(:   ) = 0   ; dzkbot    (:  ) = 0.
      temp  (:    ) = 0.  ;  salt   (:   ) = 0.  
      press (:    ) = 0.  ;  densSW (:   ) = 0. 
      hipor (:,:  ) = 0.  ;  co3por (:,: ) = 0.  
      dz3d  (:,:  ) = 0.  ;  volw3d (:,: ) = 0.  ;  vols3d   (:,:) = 0. 

      ! Chemical variables      
      ALLOCATE( akbs  (jpoce) )  ;  ALLOCATE( ak1s   (jpoce) )  ;  ALLOCATE( ak2s  (jpoce) ) ;  ALLOCATE( akws  (jpoce) )     
      ALLOCATE( ak1ps (jpoce) )  ;  ALLOCATE( ak2ps  (jpoce) )  ;  ALLOCATE( ak3ps (jpoce) ) ;  ALLOCATE( aksis (jpoce) )    
      ALLOCATE( aksps (jpoce) )  ;  ALLOCATE( ak12s  (jpoce) )  ;  ALLOCATE( ak12ps(jpoce) ) ;  ALLOCATE( ak123ps(jpoce) )    
      ALLOCATE( borats(jpoce) )  ;  ALLOCATE( calcon2(jpoce) )    

      akbs  (:) = 0. ;   ak1s   (:) = 0. ;  ak2s  (:) = 0. ;   akws   (:) = 0.
      ak1ps (:) = 0. ;   ak2ps  (:) = 0. ;  ak3ps (:) = 0. ;   aksis  (:) = 0.
      aksps (:) = 0. ;   ak12s  (:) = 0. ;  ak12ps(:) = 0. ;   ak123ps(:) = 0.
      borats(:) = 0. ;   calcon2(:) = 0.

      ! Mass balance calculation  
      ALLOCATE( fromsed(jpoce, jpsol) ) ; ALLOCATE( tosed(jpoce, jpsol) ) ;  ALLOCATE( rloss(jpoce, jpsol) )
      ALLOCATE( tokbot (jpoce, jpwat) ) 

      fromsed(:,:) = 0.    ;   tosed(:,:) = 0. ;  rloss(:,:) = 0.  ;   tokbot(:,:) = 0. 

      ! Read sediment Namelist
      !-------------------------
      CALL sed_init_nam
    
      ! Initialization of sediment geometry
      !------------------------------------
      CALL sed_init_geom


      ! sets initial sediment composition
      ! ( only clay or reading restart file )
      !---------------------------------------
      CALL sed_init_data


      CALL sed_init_wri


   END SUBROUTINE sed_init


   SUBROUTINE sed_init_geom
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_init_geom  ***
      !!
      !! ** Purpose :  Initialization of sediment geometry
      !!               - Read the deepest water layer thickness
      !!                 ( using as mask ) in Netcdf file
      !!               - sets sediment grid, porosity and molecular weight
      !!                 and others constants
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  Original
      !!----------------------------------------------------------------------
      !! * Modules used
      !! * local declarations
      INTEGER ::  &
        ji, jj, jk
 
#if defined key_sed_off
      REAL(wp) , DIMENSION (jpi,jpj) :: zdta
      INTEGER  ::  numpres
#endif
      !---------------------------------------------------------- 

      WRITE(numsed,*) ' sed_init_geom : Initialization of sediment geometry '
      WRITE(numsed,*) ' '

      ! Computation of 1D array of sediments points
      indoce = 0
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF (  epkbot(ji,jj) > 0. ) THEN
               indoce          = indoce + 1
               iarroce(indoce) = (jj - 1) * jpi + ji
            ENDIF
         END DO
      END DO

      IF( indoce .NE. jpoce ) THEN
         WRITE(numsed,*) ' '
         WRITE(numsed,*) 'Warning : number of ocean points indoce = ', indoce, &
            &     ' doesn''t match  number of point where epkbot>0  jpoce = ', jpoce
         WRITE(numsed,*) ' '
         WRITE(numsed,*) ' '
         STOP
      ELSE
         WRITE(numsed,*) ' '
         WRITE(numsed,*) ' total number of ocean points jpoce =  ',jpoce
         WRITE(numsed,*) ' '
      ENDIF

#if defined key_sed_off

      ! Reading Netcdf Pisces file for deepest water layer thickness [m]
      ! This data will be used as mask to merdge space in 1D vector
      !----------------------------------------------------------------
      CALL iom_open ( 'pressbot'   , numpres )
      
      ! pressure  in  bars      
      CALL iom_get ( numpres, jpdom_data,'BATH', zdta(:,:) )
      CALL pack_arr ( jpoce, press(1:jpoce), zdta(1:jpi,1:jpj), iarroce(1:jpoce) )
      press(1:jpoce) = press(1:jpoce) * 1.025e-1 

      CALL iom_close ( numpres )
#endif


      ! mask sediment points for outputs
      DO jk = 1, jpksed
         tmasksed(:,:,jk) = tmask(:,:,1)
      ENDDO

      ! initialization of dzkbot in [cm]
      !------------------------------------------------    
      CALL pack_arr ( jpoce, dzkbot(1:jpoce), epkbot(1:jpi,1:jpj), iarroce(1:jpoce) )
      dzkbot(1:jpoce) = dzkbot(1:jpoce) * 1.e+2 

      ! Geometry and  constants 
      ! sediment layer thickness [cm]
      ! (1st layer= diffusive layer = pur water) 
      !------------------------------------------
      dz(1)  = 0.1
      dz(2)  = 0.3
      dz(3)  = 0.3
      dz(4)  = 0.5
      dz(5)  = 0.5
      dz(6)  = 0.5
      dz(7)  = 1.
      dz(8)  = 1.
      dz(9)  = 1.
      dz(10) = 2.45
      dz(11) = 2.45

      DO jk = 1, jpksed
         DO ji = 1, jpoce
            dz3d(ji,jk) = dz(jk)
         END DO
      ENDDO

      ! Depth(jk)= depth of middle of each layer
      !----------------------------------------
      profsed(1) = -dz(1)/ 2.
      DO jk = 2, jpksed
         profsed(jk) = profsed(jk-1) + dz(jk-1) / 2. + dz(jk) / 2.
      ENDDO

      !  Porosity profile [0]
      !---------------------
      por(1)  = 1.
      por(2)  = 0.95
      por(3)  = 0.9
      por(4)  = 0.85
      por(5)  = 0.81
      por(6)  = 0.75
      por(7)  = 0.75      
      por(8)  = 0.75      
      por(9)  = 0.75      
      por(10) = 0.75
      por(11) = 0.75
 
      ! inverse of  Porosity profile
      !-----------------------------
      por1(:) = 1. - por(:)

      ! Volumes of pore water and solid fractions (vector and array)
      !     WARNING : volw(1) and vols(1) are sublayer volums
      volw(:) = dz(:) * por(:)
      vols(:) = dz(:) * por1(:)

      ! temporary new value for dz3d(:,1) 
      dz3d(1:jpoce,1) = dzkbot(1:jpoce)

      ! WARNING : volw3d(:,1) and vols3d(:,1) are deepest water column volums
      DO jk = 1, jpksed
         volw3d(1:jpoce,jk) = dz3d(1:jpoce,jk) * por (jk)
         vols3d(1:jpoce,jk) = dz3d(1:jpoce,jk) * por1(jk)
      ENDDO

      ! Back to the old sublayer vlaue for dz3d(:,1)
      dz3d(1:jpoce,1) = dz(1)


      !---------------------------------------------
      ! Molecular weight [g/mol] for solid species
      !---------------------------------------------


      ! opal=sio2*0.4(h20)=28+2*16+0.4*(2+16)
      !---------------------------------------
      mol_wgt(jsopal) = 28. + 2. * 16. + 0.4 * ( 2. + 16. )  

      !  clay
      !  some kind of Illit (according to Pape)
      !  K0.58(Al 1.38 Fe(III)0.37Fe(II)0.04Mg0.34)[(OH)2|(Si3.41Al0.59)O10]
      !--------------------------------------------------------------------
      mol_wgt(jsclay) = 0.58 * 39. + 1.38 * 27. + ( 0.37 + 0.04 ) * 56.+ &
         &              0.34 * 24. + 2. * ( 16. + 1. ) + 3.41 * 38. +    &
         &              0.59 * 27. + 10. * 16.

      ! for chemistry Poc : C(122)H(244)O(86)N(16)P(1)
      ! But den sity of Poc is an Hydrated material (= POC + 30H2O)
      ! So C(122)H(355)O(120)N(16)P(1)
      !------------------------------------------------------------
      mol_wgt(jspoc) = ( 122. * 12. + 355. + 120. * 16.+ &
         &                16. * 14. + 31. ) / 122.

      ! CaCO3
      !---------
      mol_wgt(jscal) = 40. + 12. + 3. * 16.

      ! Density of solid material in sediment [g/cm**3]
      !------------------------------------------------
      dens = 2.6

      ! Initialization of diffusion coefficient as function of porosity [cm**2/s]
      !--------------------------------------------------------------------
      diff(:) = dcoef * por(:)


      ! Initialization of time step as function of porosity [cm**2/s]
      !------------------------------------------------------------------
      rdtsed(2:jpksed) = dtsed / ( dens * por1(2:jpksed) )
     
   END SUBROUTINE sed_init_geom

   SUBROUTINE sed_init_nam
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_init_nam  ***
      !!
      !! ** Purpose :  Initialization of sediment geometry
      !!               - Reading namelist and defines constants variables
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  Original
      !!----------------------------------------------------------------------

      INTEGER :: &
         numnamsed = 28

      TYPE PSED
         CHARACTER(len = 20)  :: snamesed   !: short name
         CHARACTER(len = 80 ) :: lnamesed   !: long name
         CHARACTER(len = 20 ) :: unitsed    !: unit
      END TYPE PSED

      TYPE(PSED) , DIMENSION(jpsol     ) :: sedsol
      TYPE(PSED) , DIMENSION(jpwat     ) :: sedwat
      TYPE(PSED) , DIMENSION(jpdia3dsed) :: seddiag3d
      TYPE(PSED) , DIMENSION(jpdia2dsed) :: seddiag2d

      NAMELIST/nam_time/nfreq
      NAMELIST/nam_trased/sedsol, sedwat
      NAMELIST/nam_diased/seddiag3d, seddiag2d
      NAMELIST/nam_reac/sisat, claysat, rcopal, rcclay, dcoef 
      NAMELIST/nam_poc/redO2, redNo3, redPo4, redC, redDnit, &
         &           rcorg, o2seuil, rcorgN
      NAMELIST/nam_cal/rccal
      NAMELIST/nam_dc13/pdb, rc13P, rc13Ca
      NAMELIST/nam_btb/dbiot
      NAMELIST/nam_rst/ln_rst_sed

      INTEGER :: jn, jn1
      !-------------------------------------------------------

      WRITE(numsed,*) ' sed_init_nam : Read namelists '
      WRITE(numsed,*) ' '

      ! ryear = 1 year converted in second
      !------------------------------------
      WRITE(numsed,*) ' '
      WRITE(numsed,*) 'number of seconds in one year : ryear = ', ryear
      WRITE(numsed,*) ' '     

      ! Reading namelist.sed variables
      !---------------------------------
      CALL ctl_opn( numnamsed, 'namelist.sediment', 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )

      dtsed = rdt
      nitsed000 = nittrc000
      nitsedend = nitend
#if ! defined key_sed_off
      nwrised   = nwritetrc
#else
      nwrised   = nwrite
#endif
     ! Diffraction/reaction parameters
      !----------------------------------
      READ( numnamsed, nam_time )
      WRITE(numsed,*) ' namelist nam_time'

#if ! defined key_sed_off
      nfreq     = 1
#endif

      WRITE(numsed,*) ' sedimentation time step              dtsed      = ', dtsed
      WRITE(numsed,*) ' 1st time step for sediment.          nitsed000  = ', nitsed000
      WRITE(numsed,*) ' last time step for sediment.         nitsedend  = ', nitsedend
      WRITE(numsed,*) ' frequency of sediment outputs        nwrised    = ', nwrised
      WRITE(numsed,*) ' frequency of restoring inputs data   nfreq      = ', nfreq
      WRITE(numsed,*) ' '

      REWIND( numnamsed )               ! read nattrc
      READ  ( numnamsed, nam_trased )

      DO jn = 1, jpsol
         sedtrcd(jn) = sedsol(jn)%snamesed
         sedtrcl(jn) = sedsol(jn)%lnamesed
         sedtrcu(jn) = sedsol(jn)%unitsed
      END DO

      DO jn = 1, jpwat
         jn1 = jn + jpsol
         sedtrcd(jn1) = sedwat(jn)%snamesed
         sedtrcl(jn1) = sedwat(jn)%lnamesed
         sedtrcu(jn1) = sedwat(jn)%unitsed
      END DO

      WRITE(numsed,*) ' namelist nam_trased'
      WRITE(numsed,*) ' '
      DO jn = 1, jptrased
         WRITE(numsed,*) 'name of 3d output sediment field number :',jn,' : ',TRIM(sedtrcd(jn))
         WRITE(numsed,*) 'long name ', TRIM(sedtrcl(jn))
         WRITE(numsed,*) ' in unit = ', TRIM(sedtrcu(jn))
         WRITE(numsed,*) ' '
      END DO
      WRITE(numsed,*) ' '

      
      REWIND( numnamsed )
      READ( numnamsed, nam_diased )

      DO jn = 1, jpdia3dsed
         seddia3d(jn) = seddiag3d(jn)%snamesed
         seddia3l(jn) = seddiag3d(jn)%lnamesed
         seddia3u(jn) = seddiag3d(jn)%unitsed
      END DO

      DO jn = 1, jpdia2dsed
         seddia2d(jn) = seddiag2d(jn)%snamesed
         seddia2l(jn) = seddiag2d(jn)%lnamesed
         seddia2u(jn) = seddiag2d(jn)%unitsed
      END DO

      WRITE(numsed,*) ' namelist nam_diased'
      WRITE(numsed,*) ' '
      DO jn = 1, jpdia3dsed
         WRITE(numsed,*) 'name of 3D output diag number :',jn, ' : ', TRIM(seddia3d(jn))
         WRITE(numsed,*) 'long name ', TRIM(seddia3l(jn))
         WRITE(numsed,*) ' in unit = ',TRIM(seddia3u(jn))
         WRITE(numsed,*) ' '
      END DO

      DO jn = 1, jpdia2dsed
         WRITE(numsed,*) 'name of 2D output diag number :',jn, ' : ', TRIM(seddia2d(jn))
         WRITE(numsed,*) 'long name ', TRIM(seddia2l(jn))
         WRITE(numsed,*) ' in unit = ',TRIM(seddia2u(jn))
         WRITE(numsed,*) ' '
      END DO

      WRITE(numsed,*) ' '


      ! Diffraction/reaction parameters
      !----------------------------------
      REWIND( numnamsed )
      READ( numnamsed, nam_reac )
      WRITE(numsed,*) ' namelist nam_reac'
      WRITE(numsed,*) ' saturation for si    sisat   = ', sisat
      WRITE(numsed,*) ' saturation for clay  claysat = ', claysat
      WRITE(numsed,*) ' reactivity for Si    rcopal  = ', rcopal
      WRITE(numsed,*) ' reactivity for clay  rcclay  = ', rcclay
      WRITE(numsed,*) ' diff. coef for por.  dcoef   = ', dcoef
      WRITE(numsed,*) ' '


      ! Unity conversion to get saturation conc. psat in [mol.l-1]
      ! and reactivity rc in  [l.mol-1.s-1]
      !----------------------------------------------------------
      sat_sil   = sisat   * 1.e-6   
      sat_clay  = claysat * 1.e-6

      reac_sil  = rcopal / ryear     
      reac_clay  = rcclay / ryear


      ! Additional parameter linked to POC/O2/No3/Po4
      !----------------------------------------------
      REWIND( numnamsed )
      READ( numnamsed, nam_poc )
      WRITE(numsed,*) ' namelist nam_poc'
      WRITE(numsed,*) ' Redfield coef for oxy     redO2   = ', redO2
      WRITE(numsed,*) ' Redfield coef for no3     redNo3  = ', redNo3
      WRITE(numsed,*) ' Redfield coef for po4     redPo4  = ', redPo4
      WRITE(numsed,*) ' Redfield coef for carbone redC    = ', redC
      WRITE(numsed,*) ' Redfield coef for denitri redDnit = ', redDnit
      WRITE(numsed,*) ' reactivity for POC/O2     rcorg   = ', rcorg
      WRITE(numsed,*) ' threshold O2 concen.      o2seuil = ', o2seuil
      WRITE(numsed,*) ' reactivity for POC/NO3    rcorgN  = ', rcorgN
      WRITE(numsed,*) ' '


      so2ut  = redO2   / redC
      srno3  = redNo3  / redC
      spo4r  = redPo4  / redC
      srDnit = redDnit / redC
      sthro2 = o2seuil * 1.e-6 ! threshold O2 concen. in [mol.l-1]
      ! reactivity rc in  [l.mol-1.s-1]
      reac_poc  = rcorg / ryear
      reac_no3 = rcorgN  / ryear


      ! Carbonate parameters
      !---------------------
      READ( numnamsed, nam_cal )
      WRITE(numsed,*) ' namelist  nam_cal'
      WRITE(numsed,*) ' reactivity for calcite rccal = ', rccal
      WRITE(numsed,*) ' '

      ! reactivity rc in  [l.mol-1.s-1]      
      reac_cal = rccal / ryear


      ! C13  parameters
      !----------------
      READ( numnamsed, nam_dc13 )
      WRITE(numsed,*) ' namelist nam_dc13 ' 
      WRITE(numsed,*) ' 13C/12C in PD Belemnite        PDB    = ', pdb
      WRITE(numsed,*) ' 13C/12C in POC = rc13P*PDB     rc13P  = ', rc13P
      WRITE(numsed,*) ' 13C/12C in CaCO3 = rc13Ca*PDB  rc13Ca = ', rc13Ca
      WRITE(numsed,*) ' '

      
      ! Bioturbation parameter
      !------------------------
      READ( numnamsed, nam_btb )
      WRITE(numsed,*) ' namelist nam_btb ' 
      WRITE(numsed,*) ' coefficient for bioturbation   dbiot    = ', dbiot
      WRITE(numsed,*) ' '

      ! Unity convertion to get bioturb coefficient in [cm2.s-1]
      db = dbiot / ( ryear * 1000. )

      ! Initial value (t=0) for sediment pore water and solid components
      !----------------------------------------------------------------
      READ( numnamsed, nam_rst )
      WRITE(numsed,*) ' namelist  nam_rst ' 
      WRITE(numsed,*) '  boolean term for restart (T or F) ln_rst_sed = ', ln_rst_sed 
      WRITE(numsed,*) ' '

      CLOSE( numnamsed )

   END SUBROUTINE sed_init_nam

   SUBROUTINE sed_init_data
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_init_data  ***
      !!
      !! ** Purpose :  Initialization of sediment module
      !!               - sets initial sediment composition
      !!                 ( only clay or reading restart file )
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  original
      !!----------------------------------------------------------------------
 
      ! local variables
      INTEGER :: &
         ji, jk, zhipor

      !--------------------------------------------------------------------
 

      IF( .NOT. ln_rst_sed ) THEN

          WRITE(numsed,*) ' Initilization of default values of sediment components'

         ! default values for initial pore water concentrations [mol/l]
         pwcp(:,:,:) = 0.
         ! default value for initial solid component (fraction of dry weight dim=[0])
         ! clay
         solcp(:,:,:) = 0.
         solcp(:,2:jpksed,jsclay) = 1.

         ! Initialization of [h+] and [co3--]

         zhipor = 8.
         ! Initialization of [h+] in mol/kg
         DO jk = 1, jpksed
            DO ji = 1, jpoce
               hipor (ji,jk) = 10.**( -1. * zhipor )
            ENDDO
         ENDDO

         co3por(:,:) = 0.

      ELSE   
  
         WRITE(numsed,*) ' Initilization of Sediment components from restart'

         CALL sed_rst_read

      ENDIF


      ! Load initial Pisces Data for bot. wat. Chem and fluxes
      CALL sed_dta ( nitsed000 ) 

      ! Initialization of chemical constants
      CALL sed_chem ( nitsed000 )

      ! Stores initial sediment data for mass balance calculation
      pwcp0 (1:jpoce,1:jpksed,1:jpwat ) = pwcp (1:jpoce,1:jpksed,1:jpwat ) 
      solcp0(1:jpoce,1:jpksed,1:jpsol ) = solcp(1:jpoce,1:jpksed,1:jpsol) 

      ! Conversion of [h+] in mol/Kg to get it in mol/l ( multiplication by density)
      DO jk = 1, jpksed
         hipor(1:jpoce,jk) = hipor(1:jpoce,jk) * densSW(1:jpoce)
      ENDDO


      ! In default case - no restart - sedco3 is run to initiate [h+] and [co32-]
      ! Otherwise initiate values of pH and co3 read in restart
      IF( .NOT. ln_rst_sed ) THEN
         ! sedco3 is run to initiate[h+] [co32-] in mol/l of solution
         CALL sed_co3 ( nitsed000 )

      ENDIF
            
   END SUBROUTINE sed_init_data

   SUBROUTINE sed_init_wri

      INTEGER :: jk

      WRITE(numsed,*)' '
      WRITE(numsed,*)'======== Write summary of initial state   ============'
      WRITE(numsed,*)' '
      WRITE(numsed,*)' '
      WRITE(numsed,*)'-------------------------------------------------------------------'
      WRITE(numsed,*)' Initial Conditions '
      WRITE(numsed,*)'-------------------------------------------------------------------'
      WRITE(numsed,*)'dzm = dzkbot minimum to calculate ', 0.
      WRITE(numsed,*)'Local zone : jpi, jpj : ',jpi, jpj
      WRITE(numsed,*)'jpoce = ',jpoce,' nbtot pts = ',jpij,' nb earth pts = ',jpij - jpoce
      WRITE(numsed,*)'sublayer thickness dz(1) [cm] : ', dz(1)
      WRITE(numsed,*)'Coeff diff for k=1 (cm2/s) : ',diff(1)
      WRITE(numsed,*)' nb solid comp : ',jpsol
      WRITE(numsed,*)'(1=opal,2=clay,3=POC,4=CaCO3)'
      WRITE(numsed,*)'weight mol 1,2,3,4'
      WRITE(numsed,'(4(F0.2,3X))')mol_wgt(jsopal),mol_wgt(jsclay),mol_wgt(jspoc),mol_wgt(jscal)
      WRITE(numsed,*)'nb dissolved comp',jpwat
      WRITE(numsed,*)'(1=silicic acid,2="dissolved" clay,3=O2,4=DIC,5=Nitrate,&
         &6=Phosphates,7=Alk))'
      WRITE(numsed,*)'Psat (umol/l) for silicic Acid and "dissolved" clay'
      WRITE(numsed,'(2(F0.2,3X))') sat_sil * 1e+6,  sat_clay * 1e+6
      WRITE(numsed,*)'reaction rate rc for Op/si,Clay,POC/O2,caco3, POC/No3 (an-1)'
      WRITE(numsed,'(5(F0.2,3X))') reac_sil * ryear, reac_clay * ryear, reac_poc * ryear, &
                                   reac_cal * ryear, reac_no3 * ryear
      WRITE(numsed,*)'redfield coef C,O,N P Dit '
      WRITE(numsed,'(5(F0.2,3X))')1./spo4r,so2ut/spo4r,srno3/spo4r,spo4r/spo4r,srDnit/spo4r
      WRITE(numsed,*)'threshold for stating denitrification [mol/l]'
      WRITE(numsed,'(1PE8.2)') sthrO2
      WRITE(numsed,*)'-------------------------------------------------------------------'
      WRITE(numsed,*)'Min-Max-Mean'
      WRITE(numsed,*)'For each variable : min, max, moy value observed on selected local zone'
      WRITE(numsed,*)'-------------------------------------------------------------------'
      WRITE(numsed,*)'thickness of the last wet layer dzkbot [m]'
      WRITE(numsed,'(3(F0.2,3X))') MINVAL(dzkbot(1:jpoce))/100.,MAXVAL(dzkbot(1:jpoce))/100.,&
         &SUM(dzkbot(1:jpoce))/jpoce/100.
      WRITE(numsed,*)'temp [°C]'
      WRITE(numsed,'(3(F0.2,3X))') MINVAL(temp(1:jpoce)),MAXVAL(temp(1:jpoce)),&
         &                         SUM(temp(1:jpoce))/jpoce
      WRITE(numsed,*)'salt o/oo'
      WRITE(numsed,'(3(F0.2,3X))')MINVAL(salt(1:jpoce)),MAXVAL(salt(1:jpoce)),&
         &                        SUM(salt(1:jpoce))/jpoce
#if defined key_sed_off
      WRITE(numsed,*)'pressure [bar] (depth in m is about 10*pressure)'
      WRITE(numsed,'(3(F0.2,3X))') MINVAL(press(1:jpoce)),MAXVAL(press(1:jpoce)),&
         &                         SUM(press(1:jpoce))/jpoce
#endif
      WRITE(numsed,*)'density of Sea Water'
      WRITE(numsed,'(3(F0.2,3X))') MINVAL(densSW(1:jpoce)), MAXVAL(densSW(1:jpoce)),&
         &                         SUM(densSW(1:jpoce))/jpoce
      WRITE(numsed,*)''
      WRITE(numsed,*)'     Dissolved Components '
      WRITE(numsed,*)'     ====================='
      WRITE(numsed,*)'[Si(OH)4] dissolved (1)(k=1)(µmol/l)(and min value in mol/kg  of solution)'
      WRITE(numsed,'(4(F0.3,2X))') MINVAL(pwcp(1:jpoce,1,jwsil))*1.e+6,MAXVAL(pwcp(1:jpoce,1,jwsil))*1.e+6,&
         &                         SUM(pwcp(1:jpoce,1,jwsil))*1.e+6/jpoce,&
         &                         MINVAL(pwcp(1:jpoce,1,jwsil)*1.e+6/densSW(1:jpoce))
      WRITE(numsed,*)'[O2]    dissolved (3) (k=1)(µmol/l)(and min value in mol/kg  of solution)'
      WRITE(numsed,'(4(F0.3,2X))') MINVAL(pwcp(1:jpoce,1,jwoxy))*1.e+6,MAXVAL(pwcp(1:jpoce,1,jwoxy))*1.e+6,&
         &SUM(pwcp(1:jpoce,1,jwoxy))*1.e+6/jpoce,&
         &MINVAL(pwcp(1:jpoce,1,jwoxy)*1.e+6/densSW(1:jpoce))
      WRITE(numsed,*)'[DIC]    dissolved (4) (k=1)(µmol/l)(and min value in mol/kg  of solution)'
      WRITE(numsed,'(4(F0.3,2X))') MINVAL(pwcp(1:jpoce,1,jwdic))*1.e+6,MAXVAL(pwcp(1:jpoce,1,jwdic))*1.e+6,&
         &SUM(pwcp(1:jpoce,1,jwdic))*1.e+6/jpoce,&
         &MINVAL(pwcp(1:jpoce,1,jwdic)*1.e+6/densSW(1:jpoce))
      WRITE(numsed,*)'[NO3]    dissolved (5) (k=1)(µmol/l)(and min value in mol/kg  of solution)'
      WRITE(numsed,'(4(F0.3,2X))') MINVAL(pwcp(1:jpoce,1,jwno3))*1.e+6,MAXVAL(pwcp(1:jpoce,1,jwno3))*1.e+6,&
         &SUM(pwcp(1:jpoce,1,jwno3))*1.e+6/jpoce,&
         &MINVAL(pwcp(1:jpoce,1,jwno3)*1.e+6/densSW(1:jpoce))
      WRITE(numsed,*)'[PO4]    dissolved (6) (k=1)(µmol/l)(and min value in mol/kg  of solution)'
      WRITE(numsed,'(4(F0.3,2X))') MINVAL(pwcp(1:jpoce,1,jwpo4))*1.e+6,MAXVAL(pwcp(1:jpoce,1,jwpo4))*1.e+6,&
         &SUM(pwcp(1:jpoce,1,jwpo4))*1.e+6/jpoce,&
         &MINVAL(pwcp(1:jpoce,1,jwpo4)*1.e+6/densSW(1:jpoce))
      WRITE(numsed,*)'[Alk]    dissolved (7) (k=1)(µequi)(and min value in mol/kg  of solution)'
      WRITE(numsed,'(4(F0.3,2X))') MINVAL(pwcp(1:jpoce,1,jwalk))*1.e+6,MAXVAL(pwcp(1:jpoce,1,jwalk))*1.e+6,&
         &SUM(pwcp(1:jpoce,1,jwalk))*1.e+6/jpoce,&
         &MINVAL(pwcp(1:jpoce,1,jwalk)*1.e+6/densSW(1:jpoce))
      WRITE(numsed,*)'[DIC13]  dissolved (8) (k=1)(µmol/l)(and min value in mol/kg  of solution)'
      WRITE(numsed,'(4(F0.3,2X))') MINVAL(pwcp(1:jpoce,1,jwc13))*1.e+6,MAXVAL(pwcp(1:jpoce,1,jwc13))*1.e+6,&
         &SUM(pwcp(1:jpoce,1,jwc13))*1.e+6/jpoce,&
         &MINVAL(pwcp(1:jpoce,1,jwc13)*1.e+6/densSW(1:jpoce))
      WRITE(numsed,*)''
      WRITE(numsed,*)'     Solid Components '
      WRITE(numsed,*)'     ====================='
      WRITE(numsed,*)'nmole of Opale rained per dt'
      WRITE(numsed,'(3(1PE9.3,2X))') MINVAL(rainrm(1:jpoce,jsopal))*dtsed,MAXVAL(rainrm(1:jpoce,jsopal))*dtsed,&
         &SUM(rainrm(1:jpoce,1))*dtsed/jpoce
      WRITE(numsed,*)'nmole of Clay rained per dt'
      WRITE(numsed,'(3(1PE9.3,2X))') MINVAL(rainrm(1:jpoce,jsclay))*dtsed,MAXVAL(rainrm(1:jpoce,jsclay))*dtsed,&
         &SUM(rainrm(1:jpoce,jsclay))*dtsed/jpoce
      WRITE(numsed,*)'nmole of POC rained per dt'
      WRITE(numsed,'(3(1PE9.3,2X))') MINVAL(rainrm(1:jpoce,jspoc))*dtsed,MAXVAL(rainrm(1:jpoce,jspoc))*dtsed,&
         &SUM(rainrm(1:jpoce,jspoc))*dtsed/jpoce
      WRITE(numsed,*)'nmole of CaCO3 rained per dt'
      WRITE(numsed,'(3(1PE9.3,2X))') MINVAL(rainrm(1:jpoce,jscal))*dtsed,MAXVAL(rainrm(1:jpoce,jscal))*dtsed,&
         &SUM(rainrm(1:jpoce,jscal))*dtsed/jpoce
      WRITE(numsed,*)' '
      WRITE(numsed,*)'Weight frac of opal rained (%) '
      WRITE(numsed,'(3(F0.3,7X))') MINVAL(rainrg(1:jpoce,jsopal)/raintg(1:jpoce))*100.,&
         &MAXVAL(rainrg(1:jpoce,jsopal)/raintg(1:jpoce))*100.,&
         & SUM(rainrg(1:jpoce,jsopal)/raintg(1:jpoce))*100./jpoce
      WRITE(numsed,*)'Weight frac of clay  rained (%) '
      WRITE(numsed,'(3(F0.3,7X))') MINVAL(rainrg(1:jpoce,jsclay)/raintg(1:jpoce))*100.,&
         &MAXVAL(rainrg(1:jpoce,jsclay)/raintg(1:jpoce))*100.,&
         &SUM(rainrg(1:jpoce,jsclay)/raintg(1:jpoce))*100./jpoce
      WRITE(numsed,*)'Weight frac of POC rained (%)'
      WRITE(numsed,'(3(F0.3,7X))') MINVAL(rainrg(1:jpoce,jspoc)/raintg(1:jpoce))*100.,&
         &MAXVAL(rainrg(1:jpoce,jspoc)/raintg(1:jpoce))*100.,&
         &SUM(rainrg(1:jpoce,jspoc)/raintg(1:jpoce))*100./jpoce
      WRITE(numsed,*)'Weight frac of CaCO3  rained (%)'
      WRITE(numsed,'(3(F0.3,7X))') MINVAL(rainrg(1:jpoce,jscal)/raintg(1:jpoce))*100.,&
         &MAXVAL(rainrg(1:jpoce,jscal)/raintg(1:jpoce))*100.,&
         &SUM(rainrg(1:jpoce,jscal)/raintg(1:jpoce))*100./jpoce
      WRITE(numsed,*)''
      WRITE(numsed,*)' Thickness of sediment layer added by particule rain, dzdep cm '
      WRITE(numsed,*)' =============================================================='
      WRITE(numsed,'(3(F0.5,2X))') MINVAL(dzdep(1:jpoce)),MAXVAL(dzdep(1:jpoce)),SUM(dzdep(1:jpoce))/jpoce
      WRITE(numsed,*)''
      WRITE(numsed,*)' chemical constants K1,pK1,K2,pK2,Kw,pKw and Kb pKb (min max) [mol/kgsol] or [mol/kgsol]2 '
      WRITE(numsed,*)' ========================================================================================='
      WRITE(numsed,'(4(1PE10.3,2X))')MINVAL(ak1s(1:jpoce)),MAXVAL(ak1s(1:jpoce)),-LOG10(MINVAL(ak1s(1:jpoce))),&
         &-LOG10(MAXVAL(ak1s(1:jpoce)))
      WRITE(numsed,'(4(1PE10.3,2X))')MINVAL(ak2s(1:jpoce)),MAXVAL(ak2s(1:jpoce)),-LOG10(MINVAL(ak2s(1:jpoce))),&
         &-LOG10(MAXVAL(ak2s(1:jpoce)))
      WRITE(numsed,'(4(1PE10.3,2X))')MINVAL(akws(1:jpoce)),MAXVAL(akws(1:jpoce)),-LOG10(MINVAL(akws(1:jpoce))),&
         &-LOG10(MAXVAL(akws(1:jpoce)))
      WRITE(numsed,'(4(1PE10.3,2X))')MINVAL(akbs(1:jpoce)),MAXVAL(akbs(1:jpoce)),-LOG10(MINVAL(akbs(1:jpoce))),&
         &-LOG10(MAXVAL(akbs(1:jpoce)))
      WRITE(numsed,*)'and boron'
      WRITE(numsed,'(2(1PE10.3,2X))')MINVAL(borats(1:jpoce)),MAXVAL(borats(1:jpoce))
      WRITE(numsed,*)''
      WRITE(numsed,*)' Compo of initial sediment for point jpoce'
      WRITE(numsed,*)' ========================================='
      WRITE(numsed,*)'solcp(1),   solcp(2),   solcp(3),   solcp(4),   hipor,      pH,         co3por'
      DO jk = 1,jpksed
         WRITE(numsed,'(7(1PE10.3,2X))')solcp(jpoce,jk,jsopal),solcp(jpoce,jk,jsclay),solcp(jpoce,jk,jspoc),solcp(jpoce,jk,jscal),&
            &       hipor(jpoce,jk),-LOG10(hipor(jpoce,jk)/densSW(jpoce)),co3por(jpoce,jk)
      ENDDO
      WRITE(numsed,'(A82)')'pwcp(1),    pwcp(2),    pwcp(3),    pwcp(4),    pwcp(5),    pwcp(6),    pwcp(7)'
      DO jk = 1, jpksed
         WRITE(numsed,'(7(1PE10.3,2X))')pwcp(jpoce,jk,jwsil),pwcp(jpoce,jk,jwoxy),pwcp(jpoce,jk,jwdic),&
            & pwcp(jpoce,jk,jwno3),pwcp(jpoce,jk,jwpo4),pwcp(jpoce,jk,jwalk),pwcp(jpoce,jk,jwc13)
      ENDDO
      WRITE(numsed,*) ' '
      WRITE(numsed,*) ' End Of Initialization '
      WRITE(numsed,*) ' '
!
   END SUBROUTINE sed_init_wri
#else
   !!----------------------------------------------------------------------
   !!   Dummy module :                      NO Sediment model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE sed_ini              ! Empty routine
   END SUBROUTINE sed_ini
#endif


END MODULE sedini
