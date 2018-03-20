MODULE sedmbc
#if defined key_sed
   !!======================================================================
   !!              ***  MODULE  sedmbc  ***
   !! Sediment : mass balance calculation
   !!=====================================================================

   !!----------------------------------------------------------------------
   !!   sed_mbc    : 
   !!----------------------------------------------------------------------
   !! * Modules used
   USE sed     ! sediment global variable
   USE seddsr

   IMPLICIT NONE
   PRIVATE

   !! *  Routine accessibility
   PUBLIC sed_mbc

   !! * Module variables
   REAL(wp), DIMENSION(jpsol) :: rain_tot      ! total input rain
   REAL(wp), DIMENSION(jpsol) :: fromsed_tot   ! tota input from sediment
   REAL(wp), DIMENSION(jpsol) :: tosed_tot     ! total output from sediment
   REAL(wp), DIMENSION(jpsol) :: rloss_tot     ! total rain loss

   REAL(wp), DIMENSION(jpwat) :: diss_in_tot   ! total input in pore water
   REAL(wp), DIMENSION(jpwat) :: diss_out_tot  ! total output from pore water

   REAL(wp)  :: cons_tot_o2                   ! cumulative o2 consomation
   REAL(wp)  :: sour_tot_no3                  ! cumulative no3 source
   REAL(wp)  :: cons_tot_no3                  ! cumulative no3 consomation
   REAL(wp)  :: sour_tot_c13                  ! cumulative o2 source

   REAL(wp)  :: src13p  
   REAL(wp)  :: src13ca  

CONTAINS


   SUBROUTINE sed_mbc( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_mbc  ***
      !!
      !! ** Purpose :  computation of total tracer inventories for checking
      !!               mass conservation.
      !!
      !!
      !! ** Method   : tracer inventories of each reservoir are computed and added
      !!               subsequently.
      !!
      !!   History :
      !!        !  04-10  (N. Emprin, M. Gehlen )  Original code
      !!        !  06-07  (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------

      !! Arguments
      INTEGER, INTENT(in) :: kt     ! time step

      !! local declarations
      INTEGER  :: ji,js, jw, jk
      REAL(wp) :: zinit, zfinal 
      REAL(wp) :: zinput, zoutput
      REAL(wp) :: zdsw, zvol
      REAL, DIMENSION(jpsol) :: zsolcp_inv_i, zsolcp_inv_f
      REAL, DIMENSION(jpwat) :: zpwcp_inv_i, zpwcp_inv_f
      REAL(wp) ::  zdelta_sil, zdelta_clay
      REAL(wp) ::  zdelta_co2, zdelta_oxy
      REAL(wp) ::  zdelta_po4, zdelta_no3
      REAL(wp) ::  zdelta_c13, zdelta_c13b

      !!----------------------------------------------------------------------
      ! Initilization
      !---------------
      IF( kt == nitsed000 ) THEN
         cons_tot_o2  = 0.
         sour_tot_no3 = 0.
         cons_tot_no3 = 0.
         sour_tot_c13 = 0.

         DO js = 1, jpsol
            rain_tot   (js) = 0.
            fromsed_tot(js) = 0.
            tosed_tot  (js) = 0.
            rloss_tot  (js) = 0.
         ENDDO

         DO jw = 1, jpwat
            diss_in_tot (jw) = 0.
            diss_out_tot(jw) = 0.
         ENDDO

         src13p  = rc13P  * pdb
         src13ca = rc13Ca * pdb
      ENDIF


      ! Calculation of the cumulativ input and output
      ! for mass balance check
      !----------------------------------------------

      ! cumulativ solid
      DO js = 1, jpsol
         DO ji = 1, jpoce
            ! input [mol]
            rain_tot   (js) = rain_tot   (js) + dtsed * rainrm_dta(ji,js)
            fromsed_tot(js) = fromsed_tot(js) + fromsed(ji,js)
            ! output [mol]
            tosed_tot  (js) = tosed_tot (js) + tosed(ji,js)
            rloss_tot  (js) = rloss_tot (js) + rloss(ji,js)
         ENDDO
      ENDDO

      ! cumulativ dissolved
      DO jw = 1, jpwat
         DO ji = 1, jpoce
            ! input [mol]
            diss_in_tot (jw) = diss_in_tot (jw) + pwcp_dta(ji,jw) * 1.e-3 * dzkbot(ji)
            ! output [mol]
            diss_out_tot(jw) = diss_out_tot(jw) + tokbot(ji,jw)
         ENDDO
      ENDDO

      ! cumulativ o2 and no3 consomation
      DO ji = 1, jpoce
         cons_tot_o2  = cons_tot_o2  + cons_o2 (ji)
         sour_tot_no3 = sour_tot_no3 + sour_no3(ji)
         cons_tot_no3 = cons_tot_no3 + cons_no3(ji)
         sour_tot_c13 = sour_tot_c13 + sour_c13(ji)
      ENDDO
     

      ! Mass balance check
      !---------------------
      IF( kt == nitsedend ) THEN
         ! initial and final inventories for solid component (mole/dx.dy) in sediment
         zsolcp_inv_i(:) = 0.
         zsolcp_inv_f(:) = 0.
         zpwcp_inv_i (:) = 0.       
         zpwcp_inv_f (:) = 0.        
         DO js = 1, jpsol
            zdsw = dens / mol_wgt(js)
            DO jk = 2, jpksed
               DO ji = 1, jpoce
                  zvol = vols3d(ji,jk) * zdsw
                  zsolcp_inv_i(js) = zsolcp_inv_i(js) + solcp0(ji,jk,js) * zvol 
                  zsolcp_inv_f(js) = zsolcp_inv_f(js) + solcp (ji,jk,js) * zvol
               ENDDO
            END DO
         ENDDO

         ! initial and final inventories for dissolved component (mole/dx.dy) in sediment
         DO jw = 1, jpwat
            DO jk = 2, jpksed
               DO ji = 1, jpoce 
                  zvol = volw3d(ji,jk) * 1.e-3
                  zpwcp_inv_i(jw) = zpwcp_inv_i(jw) + pwcp0(ji,jk,jw) * zvol
                  zpwcp_inv_f(jw) = zpwcp_inv_f(jw) + pwcp (ji,jk,jw) * zvol
               ENDDO
            END DO
         ENDDO

         ! mass balance for Silica/opal
         zinit      = zsolcp_inv_i(jsopal) + zpwcp_inv_i(jwsil)
         zfinal     = zsolcp_inv_f(jsopal) + zpwcp_inv_f(jwsil)
         zinput     = rain_tot    (jsopal) + diss_in_tot (jwsil)
         zoutput    = tosed_tot   (jsopal) + rloss_tot  (jsopal) + diss_out_tot(jwsil)
         zdelta_sil = ( zfinal + zoutput ) - ( zinit + zinput )


         ! mass balance for Clay
         zinit      = zsolcp_inv_i(jsclay) 
         zfinal     = zsolcp_inv_f(jsclay) 
         zinput     = rain_tot   (jsclay) + fromsed_tot(jsclay) 
         zoutput    = tosed_tot  (jsclay) + rloss_tot  (jsclay) 
         zdelta_clay= ( zfinal + zoutput ) - ( zinit + zinput )

         ! mass balance for carbon ( carbon in POC, CaCo3, DIC )
         zinit      = zsolcp_inv_i(jspoc) + zsolcp_inv_i(jscal) + zpwcp_inv_i(jwdic)
         zfinal     = zsolcp_inv_f(jspoc) + zsolcp_inv_f(jscal) + zpwcp_inv_f(jwdic)
         zinput     = rain_tot   (jspoc) + rain_tot   (jscal) + diss_in_tot(jwdic)
         zoutput    = tosed_tot  (jspoc) + tosed_tot  (jscal) + diss_out_tot(jwdic) &
            &       + rloss_tot  (jspoc) + rloss_tot  (jscal) 
         zdelta_co2 = ( zfinal + zoutput ) - ( zinit + zinput )

         ! mass balance for oxygen : O2 is in POC remineralization
         zinit      = zpwcp_inv_i(jwoxy)
         zfinal     = zpwcp_inv_f(jwoxy)
         zinput     = diss_in_tot(jwoxy)
         zoutput    = diss_out_tot(jwoxy) + cons_tot_o2
         zdelta_oxy = ( zfinal + zoutput ) - ( zinit + zinput )

         ! mass balance for phosphate ( PO4 in POC and dissolved phosphates )
         zinit      = zsolcp_inv_i(jspoc) * spo4r + zpwcp_inv_i(jwpo4)
         zfinal     = zsolcp_inv_f(jspoc) * spo4r + zpwcp_inv_f(jwpo4)
         zinput     = rain_tot   (jspoc) * spo4r + diss_in_tot(jwpo4)
         zoutput    = tosed_tot  (jspoc) * spo4r + diss_out_tot(jwpo4) &
            &       + rloss_tot  (jspoc) * spo4r
         zdelta_po4 = ( zfinal + zoutput ) - ( zinit + zinput )


         ! mass balance for Nitrate
         zinit      = zpwcp_inv_i  (jwno3)
         zfinal     = zpwcp_inv_f  (jwno3)
         zinput     = diss_in_tot (jwno3) + sour_tot_no3
         zoutput    = diss_out_tot(jwno3) + cons_tot_no3
         zdelta_no3 = ( zfinal + zoutput ) - ( zinit + zinput )

         ! mass balance for DIC13
         zinit      =  zpwcp_inv_i(jwc13)   &
            &        + src13p * zsolcp_inv_i(jspoc) + src13Ca * zsolcp_inv_i(jscal) 
         zfinal     =  zpwcp_inv_f(jwc13)   &
            &        + src13p * zsolcp_inv_f(jspoc) + src13Ca * zsolcp_inv_f(jscal)
         zinput     =  diss_in_tot (jwc13)  &
            &        + src13p * rain_tot(jspoc) + src13Ca * rain_tot(jscal)
         zoutput    =  diss_out_tot(jwc13)  &
            &        + src13p * tosed_tot(jspoc) + src13Ca * tosed_tot(jscal) &   
            &        + src13p * rloss_tot(jspoc) + src13Ca * rloss_tot(jscal)
         zdelta_c13 = ( zfinal + zoutput ) - ( zinit + zinput )

         ! other mass balance for DIC13
         zinit      = zpwcp_inv_i  (jwc13)
         zfinal     = zpwcp_inv_f  (jwc13)
         zinput     = diss_in_tot (jwc13) + sour_tot_c13
         zoutput    = diss_out_tot(jwc13)
         zdelta_c13b= ( zfinal + zoutput ) - ( zinit + zinput )    

      END IF

      IF( kt == nitsedend) THEN 

         WRITE(numsed,*)
         WRITE(numsed,*)'==================    General mass balance   ==================  '
         WRITE(numsed,*)' '
         WRITE(numsed,*)' '
         WRITE(numsed,*)' Initial total solid Masses (mole/dx.dy) (k=2-11) '
         WRITE(numsed,*)' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numsed,*)'    Opale,      Clay,       POC,        CaCO3,         C13'
         WRITE(numsed,'(4x,5(1PE10.3,2X))')zsolcp_inv_i(jsopal),zsolcp_inv_i(jsclay),zsolcp_inv_i(jspoc), &
            & zsolcp_inv_i(jscal),( src13P * zsolcp_inv_i(jspoc) + src13Ca * zsolcp_inv_i(jscal) )
         WRITE(numsed,*)' '
         WRITE(numsed,*)' Initial total dissolved Masses (mole/dx.dy) (k=2-11) '
         WRITE(numsed,*)' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numsed,*)'    Si,         O2,         DIC,        Nit         Phos,       DIC13'
         WRITE(numsed,'(4x,6(1PE10.3,2X))') zpwcp_inv_i(jwsil), zpwcp_inv_i(jwoxy), &
            & zpwcp_inv_i(jwdic), zpwcp_inv_i(jwno3), zpwcp_inv_i(jwpo4), zpwcp_inv_i(jwc13)
         WRITE(numsed,*)' '
         WRITE(numsed,*)'  Solid inputs :  Opale,      Clay,       POC,        CaCO3,         C13'
         WRITE(numsed,'(A4,10X,5(1PE10.3,2X))')'Rain : ',rain_tot(jsopal),rain_tot(jsclay),rain_tot(jspoc),&
            & rain_tot(jscal),( src13P * rain_tot(jspoc) + src13Ca * rain_tot(jscal) )
         WRITE(numsed,'(A12,6x,4(1PE10.3,2X))')' From Sed : ',fromsed_tot(jsopal), fromsed_tot(jsclay), &
            & fromsed_tot(jspoc), fromsed_tot(jscal)
         WRITE(numsed,*)'Diss. inputs : Si,    O2,         DIC,         Nit,       Phos,       DIC13'
         WRITE(numsed,'(A9,1x,6(1PE10.3,2X))')' From Pisc : ', diss_in_tot(jwsil), &
            & diss_in_tot(jwoxy), diss_in_tot(jwdic), diss_in_tot(jwno3), diss_in_tot(jwpo4), &
            & diss_in_tot(jwc13)
         WRITE(numsed,*)' '
         WRITE(numsed,*)'Solid output : Opale,      Clay,       POC,        CaCO3,          C13'
         WRITE(numsed,'(A6,8x,5(1PE10.3,2X))')'To sed', tosed_tot(jsopal),tosed_tot(jsclay),tosed_tot(jspoc),&
            & tosed_tot(jscal),( src13P * tosed_tot(jspoc) + src13Ca * tosed_tot(jscal) )
         WRITE(numsed,'(A5,9x,5(1PE10.3,2X))')'Perdu', rloss_tot(jsopal),rloss_tot(jsclay),rloss_tot(jspoc),&
            & rloss_tot(jscal),( src13P * rloss_tot(jspoc) + src13Ca * rloss_tot(jscal) )
         WRITE(numsed,*)'Diss. output : Si,     O2,        DIC,          Nit,       Phos,       DIC13 '  
         WRITE(numsed,'(A7,2x,6(1PE10.3,2X))')'To kbot', diss_out_tot(jwsil), &
            & diss_out_tot(jwoxy), diss_out_tot(jwdic), diss_out_tot(jwno3), diss_out_tot(jwpo4), &
            & diss_out_tot(jwc13)
         WRITE(numsed,*)' '
         WRITE(numsed,*)' Total consomation in POC remineralization [mol]:  O2,         NO3'
         WRITE(numsed,'(51x,2(1PE10.3,2X))') cons_tot_o2,cons_tot_no3
         WRITE(numsed,*)' '
         WRITE(numsed,*)'Final solid  Masses (mole/dx.dy) (k=2-11)'
         WRITE(numsed,*)'    Opale,      Clay,       POC,        CaCO3,          C13'
         WRITE(numsed,'(4x,5(1PE10.3,2X))')zsolcp_inv_f(jsopal),zsolcp_inv_f(jsclay),zsolcp_inv_f(jspoc), &
            & zsolcp_inv_f(jscal),( src13P * zsolcp_inv_f(jspoc) + src13Ca * zsolcp_inv_f(jscal) )
         WRITE(numsed,*)' '
         WRITE(numsed,*)'Final dissolved  Masses (mole/dx.dy) (k=2-11)'
         WRITE(numsed,*)'    Si,        O2,         DIC,        Nit,        Phos,    DIC13'
         WRITE(numsed,'(4x,6(1PE10.3,2X))') zpwcp_inv_f(jwsil), zpwcp_inv_f(jwoxy), &
            & zpwcp_inv_f(jwdic), zpwcp_inv_f(jwno3), zpwcp_inv_f(jwpo4), zpwcp_inv_f(jwc13)
         WRITE(numsed,*)' '     
         WRITE(numsed,*)'Delta : Opale,      Clay,       C,          O,          N,          P,        C13'
         WRITE(numsed,'(7x,7(1PE11.3,1X))') zdelta_sil, zdelta_clay, zdelta_co2, zdelta_oxy, zdelta_no3,&
            &                          zdelta_po4, zdelta_c13
         WRITE(numsed,*)' ' 
         WRITE(numsed,*)'deltaC13bis : ',zdelta_c13b     

         WRITE(numsed,*)'=========================================================================='
         WRITE(numsed,*)' Composition of final sediment for point jpoce'
         WRITE(numsed,*)' ========================================='
         WRITE(numsed,*)'Opale,      Clay,       POC,        CaCo3,      hipor,      pH,         co3por'
         DO jk = 1,jpksed
            WRITE(numsed,'(4(F8.4,4X),3(1PE10.3,2X))') solcp(jpoce,jk,jsopal)*100.,solcp(jpoce,jk,jsclay)*100.,&
               &       solcp(jpoce,jk,jspoc)*100.,solcp(jpoce,jk,jscal)*100.,&
               &       hipor(jpoce,jk),-LOG10(hipor(jpoce,jk)/densSW(jpoce)),co3por(jpoce,jk)
         ENDDO
         WRITE(numsed,*)'Silicic A.,  Oxygen,     DIC,        Nitrats,    Phosphats,  Alkal.,     DIC13'
         DO jk = 1, jpksed
            WRITE(numsed,'(8(1PE10.3,2X))')pwcp(jpoce,jk,jwsil),pwcp(jpoce,jk,jwoxy),&
               & pwcp(jpoce,jk,jwdic),pwcp(jpoce,jk,jwno3),pwcp(jpoce,jk,jwpo4),pwcp(jpoce,jk,jwalk),pwcp(jpoce,jk,jwc13)
         ENDDO
         WRITE(numsed,*)'densSW at the end : ',densSW(jpoce)
         WRITE(numsed,*)'=========================================================================='

      ENDIF
  
   END SUBROUTINE sed_mbc


#else
   !!======================================================================
   !! MODULE sedmbc :   Dummy module 
   !!======================================================================
CONTAINS
   SUBROUTINE sed_mbc( kt )         ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'sed_mbc: You should not have seen this print! error?', kt
   END SUBROUTINE sed_mbc
#endif
END MODULE sedmbc
