MODULE seddsr
#if defined key_sed
   !!======================================================================
   !!              ***  MODULE  seddsr  ***
   !!    Sediment : dissolution and reaction in pore water
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sedmat  ! linear system of equations
   USE sedco3  ! carbonate ion and proton concentration 

   PUBLIC sed_dsr

   !! * Module variables

   REAL(wp), DIMENSION(:), ALLOCATABLE, PUBLIC :: cons_o2
   REAL(wp), DIMENSION(:), ALLOCATABLE, PUBLIC :: cons_no3
   REAL(wp), DIMENSION(:), ALLOCATABLE, PUBLIC :: sour_no3
   REAL(wp), DIMENSION(:), ALLOCATABLE, PUBLIC :: sour_c13
   REAL(wp), DIMENSION(:), ALLOCATABLE, PUBLIC ::  dens_mol_wgt  ! molecular density 

CONTAINS
   
   SUBROUTINE sed_dsr( kt ) 
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_dsr  ***
      !! 
      !!  ** Purpose :  computes pore water dissolution and reaction
      !!
      !!  ** Methode :  implicit simultaneous computation of undersaturation
      !!               resulting from diffusive pore water transport and chemical
      !!               pore water reactions. Solid material is consumed according
      !!               to redissolution and remineralisation
      !!
      !!  ** Remarks :
      !!              - undersaturation : deviation from saturation concentration
      !!              - reaction rate   : sink of undersaturation from dissolution
      !!                                 of solid material 
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) f90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------
      !! Arguments
      INTEGER, INTENT(in) ::   kt       ! number of iteration
      ! --- local variables
      INTEGER :: ji, jk, js, jw   ! dummy looop indices
      INTEGER :: nv               ! number of variables in linear tridiagonal eq

      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: zrearat    ! reaction rate in pore water
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: zundsat    ! undersaturation ; indice jpwatp1 is for calcite   
      REAL(wp), DIMENSION(:    ), ALLOCATABLE :: zmo2_0, zmo2_1  ! temp. array for mass balance calculation
      REAL(wp), DIMENSION(:    ), ALLOCATABLE :: zmno3_0, zmno3_1, zmno3_2
      REAL(wp), DIMENSION(:    ), ALLOCATABLE :: zmc13_0, zmc13_1, zmc13_2, zmc13_3
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: zvolc    ! temp. variables
      REAL(wp)  ::  zsolid1, zsolid2, zsolid3, zvolw, zreasat

      !!
      !!----------------------------------------------------------------------

      IF( kt == nitsed000 ) THEN
         WRITE(numsed,*) ' sed_dsr : Dissolution reaction '
         WRITE(numsed,*) ' '
         ! 
         ALLOCATE( dens_mol_wgt((jpoce) ) 
         dens_mol_wgt(1:jpsol) = dens / mol_wgt(1:jpsol)
         ! 
         ALLOCATE( cons_o2 (jpoce) ) ;   ALLOCATE( cons_no3(jpoce) ) 
         ALLOCATE( sour_no3(jpoce) ) ;   ALLOCATE( sour_c13(jpoce) )  
      ENDIF

      ! Initialization of data for mass balance calculation
      !---------------------------------------------------

      tokbot(:,:)  = 0.
      cons_o2 (:)  = 0. 
      cons_no3(:)  = 0.  
      sour_no3(:)  = 0.  
      sour_c13(:)  = 0.       
   
      ! Initializations
      !----------------------
      ALLOCATE( zmo2_0 (jpoce) ) ;  ALLOCATE( zmo2_1 (jpoce) ) 
      ALLOCATE( zmno3_0(jpoce) ) ;  ALLOCATE( zmno3_1(jpoce) )  ;  ALLOCATE( zmno3_2(jpoce) ) 
      ALLOCATE( zmc13_0(jpoce) ) ;  ALLOCATE( zmc13_1(jpoce) )  ;  ALLOCATE( zmc13_2(jpoce) ) ;  ALLOCATE( zmc13_3(jpoce) ) 

      zmo2_0 (:) = 0.  ; zmo2_1 (:) = 0.
      zmno3_0(:) = 0.  ; zmno3_1(:) = 0.  ;  zmno3_2(:) = 0.
      zmc13_0(:) = 0.  ; zmc13_1(:) = 0.  ;  zmc13_2(:) = 0.  ; zmc13_3(:) = 0.
      
      ALLOCATE( zrearat(jpoce,jpksed,3) ) ;  ALLOCATE( zundsat(jpoce,jpksed,3) )      
      zrearat(:,:,:)   = 0.    ;   zundsat(:,:,:) = 0. 


      ALLOCATE( zvolc(jpoce,jpksed,jpsol) ) 
      zvolc(:,:,:)   = 0.

      !--------------------------------------------------------------------
      ! Temporary accomodation to take account of  particule rain deposition
      !---------------------------------------------------------------------
      
      
      ! 1. Change of geometry
      !    Increase of dz3d(2) thickness : dz3d(2) = dz3d(2)+dzdep
      !    Warning : no change for dz(2)
      !---------------------------------------------------------
      dz3d(1:jpoce,2) = dz3d(1:jpoce,2) + dzdep(1:jpoce)

      
      ! New values for volw3d(:,2) and vols3d(:,2)
      ! Warning : no change neither for volw(2) nor  vols(2)
      !------------------------------------------------------
      volw3d(1:jpoce,2) = dz3d(1:jpoce,2) * por(2)
      vols3d(1:jpoce,2) = dz3d(1:jpoce,2) * por1(2)

      ! Conversion of volume units
      !----------------------------
      DO js = 1, jpsol
         DO jk = 1, jpksed
            DO ji = 1, jpoce    
               zvolc(ji,jk,js) = ( vols3d(ji,jk) * dens_mol_wgt(js) ) /  &
                  &              ( volw3d(ji,jk) * 1.e-3 )     
            ENDDO
         ENDDO
      ENDDO

      ! 2. Change of previous solid fractions (due to volum changes) for k=2
      !---------------------------------------------------------------------

      DO js = 1, jpsol
         DO ji = 1, jpoce
            solcp(ji,2,js) = solcp(ji,2,js) * dz(2) / dz3d(ji,2)
         ENDDO
      END DO

      ! 3. New solid fractions (including solid rain fractions) for k=2  
      !------------------------------------------------------------------   
      DO js = 1, jpsol
         DO ji = 1, jpoce
            solcp(ji,2,js) = solcp(ji,2,js) + &
            &           ( rainrg(ji,js) / raintg(ji) ) * ( dzdep(ji) / dz3d(ji,2) )
            ! rainrm are temporary cancel
            rainrm(ji,js) = 0.
         END DO
      ENDDO

      ! 4.  Adjustment of bottom water concen.(pwcp(1)): 
      !     We impose that pwcp(2) is constant. Including dzdep in dz3d(:,2) we assume 
      !     that dzdep has got a porosity of por(2). So pore water volum of jk=2 increase.
      !     To keep pwcp(2) cste we must compensate this "increase" by a slight adjusment
      !     of bottom water concentration.
      !     This adjustment is compensate at the end of routine
      !-------------------------------------------------------------
      DO jw = 1, jpwat
         DO ji = 1, jpoce
            pwcp(ji,1,jw) = pwcp(ji,1,jw) - &
               &            pwcp(ji,2,jw) * dzdep(ji) * por(2) / dzkbot(ji)
         END DO
      ENDDO

 
      !----------------------------------------------------------
      ! 5.  Beginning of  Pore Water diffusion and solid reaction
      !---------------------------------------------------------
      
      !-----------------------------------------------------------------------------
      ! For jk=2,jpksed, and for couple 
      !  1 : jwsil/jsopal  ( SI/Opal )
      !  2 : jsclay/jsclay ( clay/clay ) 
      !  3 : jwoxy/jspoc   ( O2/POC )
      !  reaction rate is a function of solid=concentration in solid reactif in [mol/l] 
      !  and undersaturation in [mol/l].
      !  Solid weight fractions should be in ie [mol/l])
      !  second member and solution are in zundsat variable
      !-------------------------------------------------------------------------

      !number of variables
      nv  = 3
     
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            ! For Silicic Acid and clay
            zundsat(ji,jk,1) = sat_sil   - pwcp(ji,jk,jwsil)
            zundsat(ji,jk,2) = sat_clay
            ! For O2
            zundsat(ji,jk,3) = pwcp(ji,jk,jwoxy) / so2ut 
         ENDDO
      ENDDO
      
      
      ! Definition of reaction rates [rearat]=sans dim 
      ! For jk=1 no reaction (pure water without solid) for each solid compo
      DO ji = 1, jpoce
         zrearat(ji,1,:) = 0.
      ENDDO


      ! left hand side of coefficient matrix
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            zsolid1 = zvolc(ji,jk,jsopal) * solcp(ji,jk,jsopal)
            zsolid2 = zvolc(ji,jk,jsclay) * solcp(ji,jk,jsclay)
            zsolid3 = zvolc(ji,jk,jspoc)  * solcp(ji,jk,jspoc)

            zrearat(ji,jk,1)  = ( reac_sil * dtsed * zsolid1 ) / &
               &                ( 1. + reac_sil * dtsed * zundsat(ji,jk,1 ) )
            zrearat(ji,jk,2)  = ( reac_clay * dtsed * zsolid2 ) / &
               &                ( 1. + reac_clay * dtsed * zundsat(ji,jk,2 ) )
            zrearat(ji,jk,3)  = ( reac_poc  * dtsed * zsolid3 ) / &
               &                ( 1. + reac_poc  * dtsed * zundsat(ji,jk,3 ) )
         ENDDO
      ENDDO


      CALL sed_mat( nv, jpoce, jpksed, zrearat, zundsat )


      ! New solid concentration values (jk=2 to jksed) for each couple 
      DO js = 1, nv
         DO jk = 2, jpksed
            DO ji = 1, jpoce
               zreasat = zrearat(ji,jk,js) * zundsat(ji,jk,js) / zvolc(ji,jk,js)
               solcp(ji,jk,js) = solcp(ji,jk,js) - zreasat
            ENDDO
         ENDDO
      ENDDO
      ! mass of O2/NO3 before POC remin. for mass balance check 
      ! det. of o2 consomation/NO3 production Mc13
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zvolw = volw3d(ji,jk) * 1.e-3
            zmo2_0 (ji)  = zmo2_0 (ji) + pwcp(ji,jk,jwoxy) * zvolw
            zmno3_0(ji)  = zmno3_0(ji) + pwcp(ji,jk,jwno3) * zvolw
            zmc13_0(ji)  = zmc13_0(ji) + pwcp(ji,jk,jwc13) * zvolw
         ENDDO
      ENDDO

      ! New pore water concentrations    
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            ! Acid Silicic 
            pwcp(ji,jk,jwsil)  = sat_sil - zundsat(ji,jk,1)            
            ! For O2 (in mol/l)
            pwcp(ji,jk,jwoxy)  = zundsat(ji,jk,3) * so2ut 
            zreasat = zrearat(ji,jk,3) * zundsat(ji,jk,3)    ! oxygen         
            ! For DIC
            pwcp(ji,jk,jwdic)  = pwcp(ji,jk,jwdic) + zreasat
            ! For nitrates
            pwcp(ji,jk,jwno3)  = pwcp(ji,jk,jwno3) + zreasat * srno3            
            ! For Phosphate (in mol/l)
            pwcp(ji,jk,jwpo4)  = pwcp(ji,jk,jwpo4) + zreasat * spo4r            
            ! For alkalinity
            pwcp(ji,jk,jwalk)  = pwcp(ji,jk,jwalk) - zreasat * ( srno3 + 2.* spo4r )           
            ! For DIC13
            pwcp(ji,jk,jwc13)  = pwcp(ji,jk,jwc13) + zreasat * rc13P * pdb
         ENDDO
      ENDDO


      ! Mass of O2 for mass balance check and det. of o2 consomation
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zvolw = volw3d(ji,jk) * 1.e-3
            zmo2_1 (ji) = zmo2_1 (ji) + pwcp(ji,jk,jwoxy) * zvolw
            zmno3_1(ji) = zmno3_1(ji) + pwcp(ji,jk,jwno3) * zvolw
            zmc13_1(ji) = zmc13_1(ji) + pwcp(ji,jk,jwc13) * zvolw
         ENDDO
      ENDDO

      DO ji = 1, jpoce
         cons_o2 (ji) = zmo2_0 (ji) - zmo2_1 (ji)
         sour_no3(ji) = zmno3_1(ji) - zmno3_0(ji)     
         sour_c13(ji) = zmc13_1(ji) - zmc13_0(ji) 
      ENDDO
 

      !--------------------------------------------------------------------
      ! Begining POC denitrification and NO3- diffusion
      ! (indice n°5 for couple POC/NO3- ie solcp(:,:,jspoc)/pwcp(:,:,jwno3))
      !--------------------------------------------------------------------

      nv = 1
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zundsat(ji,jk,1) = pwcp(ji,jk,jwno3) / srDnit
         ENDDO
      ENDDO
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF( pwcp(ji,jk,jwoxy) < sthrO2 ) THEN
               zsolid1 = zvolc(ji,jk,jspoc) * solcp(ji,jk,jspoc)
               zrearat(ji,jk,1) = ( reac_no3 * dtsed * zsolid1 ) / &
                  &                    ( 1. + reac_no3 * dtsed * zundsat(ji,jk,1 ) )
            ELSE
               zrearat(ji,jk,1) = 0.
            ENDIF
         END DO
      END DO


      ! solves tridiagonal system
      CALL sed_mat( nv, jpoce, jpksed, zrearat, zundsat )


      ! New solid concentration values (jk=2 to jksed) for each couple 
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            zreasat = zrearat(ji,jk,1) * zundsat(ji,jk,1) / zvolc(ji,jk,jspoc)
            solcp(ji,jk,jspoc) = solcp(ji,jk,jspoc) - zreasat
         ENDDO
      ENDDO

      ! New dissolved concentrations
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zreasat = zrearat(ji,jk,1) * zundsat(ji,jk,1)    
            ! For nitrates
            pwcp(ji,jk,jwno3)  =  zundsat(ji,jk,1) * srDnit
            ! For DIC
            pwcp(ji,jk,jwdic)  = pwcp(ji,jk,jwdic) + zreasat
            ! For Phosphate (in mol/l)
            pwcp(ji,jk,jwpo4)  = pwcp(ji,jk,jwpo4) + zreasat * spo4r            
            ! For alkalinity
            pwcp(ji,jk,jwalk)  = pwcp(ji,jk,jwalk) + zreasat * ( srDnit - 2.* spo4r )           
            ! For DIC13
            pwcp(ji,jk,jwc13)  = pwcp(ji,jk,jwc13) + zreasat * rc13P * pdb
         ENDDO
      ENDDO


      ! Mass of O2 for mass balance check and det. of o2 consomation
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zvolw = volw3d(ji,jk) * 1.e-3
            zmno3_2(ji) = zmno3_2(ji) + pwcp(ji,jk ,jwno3) * zvolw
            zmc13_2(ji) = zmc13_2(ji) + pwcp(ji,jk ,jwc13) * zvolw   
         ENDDO
      ENDDO

      DO ji = 1, jpoce
         cons_no3(ji) = zmno3_1(ji) - zmno3_2(ji)  
         sour_c13(ji) = sour_c13(ji) + zmc13_2(ji) - zmc13_1(ji)     
      ENDDO


      !---------------------------
      ! Solves PO4 diffusion 
      !----------------------------

      nv = 1
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zundsat(ji,jk,1) = pwcp(ji,jk,jwpo4)
            zrearat(ji,jk,1) = 0.
         ENDDO
      ENDDO


      ! solves tridiagonal system
      CALL sed_mat( nv, jpoce, jpksed, zrearat, zundsat )


      ! New undsaturation values and dissolved concentrations
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            pwcp(ji,jk,jwpo4) = zundsat(ji,jk,1)
         ENDDO
      ENDDO


      !---------------------------------------------------------------
      ! Performs CaCO3 particle deposition and redissolution (indice 9)
      !--------------------------------------------------------------

      ! computes co3por from the updated pwcp concentrations (note [co3por] = mol/l)

      CALL sed_co3( kt )


      nv = 1
      ! *densSW(l)**2 converts aksps [mol2/kg sol2] into [mol2/l2] to get [undsat] in [mol/l]
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zundsat(ji,jk,1) = aksps(ji) * densSW(ji) * densSW(ji) / calcon2(ji) &
               &                     - co3por(ji,jk)
            ! positive values of undersaturation
            zundsat(ji,jk,1) = MAX( 0., zundsat(ji,jk,1) )            
         ENDDO
      ENDDO

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            zsolid1 = zvolc(ji,jk,jscal) * solcp(ji,jk,jscal)
            zrearat(ji,jk,1) = ( reac_cal * dtsed * zsolid1 ) / &
                  &               ( 1. + reac_cal * dtsed * zundsat(ji,jk,1) )
         END DO
      END DO


      ! solves tridiagonal system
      CALL sed_mat( nv, jpoce, jpksed, zrearat, zundsat )


      ! New solid concentration values (jk=2 to jksed) for cacO3
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            zreasat = zrearat(ji,jk,1) * zundsat(ji,jk,1) / zvolc(ji,jk,jscal)
            solcp(ji,jk,jscal) = solcp(ji,jk,jscal) - zreasat
         ENDDO
      ENDDO

      ! New dissolved concentrations
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zreasat = zrearat(ji,jk,1) * zundsat(ji,jk,1)    
            ! For DIC
            pwcp(ji,jk,jwdic)  = pwcp(ji,jk,jwdic) + zreasat
            ! For alkalinity
            pwcp(ji,jk,jwalk)  = pwcp(ji,jk,jwalk) + 2.* zreasat 
            ! For DIC13
            pwcp(ji,jk,jwc13)  = pwcp(ji,jk,jwc13) + zreasat * rc13Ca * pdb
         ENDDO
      ENDDO

      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zmc13_3(ji) = zmc13_3(ji) + pwcp(ji,jk,jwc13) * volw3d(ji,jk) * 1.e-3                  
         ENDDO
      ENDDO

      DO ji = 1, jpoce     
         sour_c13(ji) = sour_c13(ji) + zmc13_3(ji) - zmc13_2(ji)   
      ENDDO
      
      !-------------------------------------------------
      ! Beginning DIC, Alkalinity and DIC13 diffusion
      !-------------------------------------------------
      
      nv = 3
      DO jk = 1, jpksed
         DO ji = 1, jpoce      
            zundsat(ji,jk,1) = pwcp(ji,jk,jwdic)
            zundsat(ji,jk,2) = pwcp(ji,jk,jwalk)
            zundsat(ji,jk,3) = pwcp(ji,jk,jwc13)
      
            zrearat(ji,jk,1) = 0.
            zrearat(ji,jk,2) = 0.
            zrearat(ji,jk,3) = 0.
      
         ENDDO
      ENDDO


      ! solves tridiagonal system
      CALL sed_mat( nv, jpoce, jpksed, zrearat, zundsat )


      ! New dissolved concentrations      
      DO jk = 1, jpksed
         DO ji = 1, jpoce                      
            pwcp(ji,jk,jwdic) = zundsat(ji,jk,1)
            pwcp(ji,jk,jwalk) = zundsat(ji,jk,2)
            pwcp(ji,jk,jwc13) = zundsat(ji,jk,3)
         ENDDO
      ENDDO            
      
      !----------------------------------
      !   Back to initial geometry
      !-----------------------------
      
      !---------------------------------------------------------------------
      !   1/ Compensation for ajustement of the bottom water concentrations
      !      (see note n° 1 about *por(2))
      !--------------------------------------------------------------------
      DO jw = 1, jpwat
         DO ji = 1, jpoce
            pwcp(ji,1,jw) = pwcp(ji,1,jw) + &
               &            pwcp(ji,2,jw) * dzdep(ji) * por(2) / dzkbot(ji)
         END DO
      ENDDO
      
      !-----------------------------------------------------------------------
      !    2/ Det of new rainrg taking account of the new weight fraction obtained 
      !      in dz3d(2) after diffusion/reaction (react/diffu are also in dzdep!)
      !      This new rain (rgntg rm) will be used in advection/burial routine
      !------------------------------------------------------------------------
      DO js = 1, jpsol
         DO ji = 1, jpoce
            rainrg(ji,js) = raintg(ji) * solcp(ji,2,js)
            rainrm(ji,js) = rainrg(ji,js) / mol_wgt(js)
         END DO
      ENDDO
      
      !  New raintg
      raintg(:) = 0.
      DO js = 1, jpsol
         DO ji = 1, jpoce
            raintg(ji) = raintg(ji) + rainrg(ji,js)
         END DO
      ENDDO
      
      !--------------------------------
      !    3/ back to initial geometry
      !--------------------------------
      DO ji = 1, jpoce
         dz3d  (ji,2) = dz(2)
         volw3d(ji,2) = dz3d(ji,2) * por(2)
         vols3d(ji,2) = dz3d(ji,2) * por1(2)
      ENDDO
      
      !----------------------------------------------------------------------
      !    4/ Saving new amount of material in dzkbot for mass balance check
      !       tokbot in [mol] (implicit *1cm*1cm for spacial dim)
      !----------------------------------------------------------------------
      DO jw = 1, jpwat 
         DO ji = 1, jpoce
            tokbot(ji,jw) = pwcp(ji,1,jw) * 1.e-3 * dzkbot(ji)
         END DO
      ENDDO

      DEALLOCATE( zmo2_0  ) ;  DEALLOCATE( zmno3_1 )  ;  DEALLOCATE( zmno3_2 ) 
      DEALLOCATE( zmc13_0 ) ;  DEALLOCATE( zmc13_1 )  ;  DEALLOCATE( zmc13_2 ) ;  DEALLOCATE( zmc13_3 ) 
      
      DEALLOCATE( zrearat ) ;  DEALLOCATE( zundsat )  ;  DEALLOCATE( zvolc )   
      
   END SUBROUTINE sed_dsr
#else
   !!======================================================================
   !! MODULE seddsr  :   Dummy module 
   !!======================================================================
CONTAINS
   SUBROUTINE sed_dsr ( kt )
     INTEGER, INTENT(in) :: kt
     WRITE(*,*) 'sed_dsr: You should not have seen this print! error?', kt
  END SUBROUTINE sed_dsr
#endif
   
END MODULE seddsr
