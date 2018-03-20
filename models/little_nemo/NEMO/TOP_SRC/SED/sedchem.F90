MODULE sedchem

#if defined key_sed 
   !!======================================================================
   !!                        ***  Module sedchem  ***
   !! sediment :   Variable for chemistry of the CO2 cycle
   !!======================================================================
   !!   modules used
   USE sed     ! sediment global variable
   USE sedarr

   !! * Accessibility
   PUBLIC sed_chem   

   !! * Module variables
   REAL(wp) :: &
      salchl = 1./1.80655 , & ! conversion factor for salinity --> chlorinity (Wooster et al. 1969)
      calcon = 1.03E-2        ! mean calcite concentration [Ca2+] in sea water [mole/kg solution] 

   REAL(wp) :: &              ! coeff. for 1. dissoc. of carbonic acid (Millero, 1995)   
      c10 = 3670.7   , &
      c11 = 62.008   , &
      c12 = 9.7944   , &
      c13 = 0.0118   , &
      c14 = 0.000116

   REAL(wp) :: &              ! coeff. for 2. dissoc. of carbonic acid (Millero, 1995)   
      c20 = 1394.7   , &
      c21 = 4.777    , &
      c22 = 0.       , &
      c23 = 0.0184   , &
      c24 = 0.000118

   REAL(wp) :: &              ! coeff. for 1. dissoc. of boric acid (Dickson and Goyet, 1994)
      cb0  = -8966.90, &
      cb1  = -2890.53, &
      cb2  = -77.942 , &
      cb3  = 1.728   , &
      cb4  = -0.0996 , &
      cb5  = 148.0248, &
      cb6  = 137.1942, &
      cb7  = 1.62142 , &
      cb8  = -24.4344, &
      cb9  = -25.085 , &
      cb10 = -0.2474 , &
      cb11 = 0.053105

   REAL(wp) :: &             ! borat constants
      bor1 = 0.000232, &
      bor2 = 1./10.811

   REAL(wp) :: &             ! constants for calculate concentrations 
      st1  = 0.14     , &    ! for sulfate (Morris & Riley 1966)
      st2  = 1./96.062, &
      ks0  = 141.328  , &
      ks1  = -4276.1  , &
      ks2  = -23.093  , &
      ks3  = -13856.  , &
      ks4  = 324.57   , &
      ks5  = -47.986  , &
      ks6  = 35474.   , &
      ks7  = -771.54  , &
      ks8  = 114.723  , &
      ks9  = -2698.   , &
      ks10 = 1776.    , &
      ks11 = 1.       , &
      ks12 = -0.001005 

   REAL(wp) :: &             ! constants for calculate concentrations 
      ft1  = 0.000067   , &  ! fluorides (Dickson & Riley 1979 )
      ft2  = 1./18.9984 , &
      kf0  = -12.641    , &
      kf1  = 1590.2     , &
      kf2  = 1.525      , &
      kf3  = 1.0        , &
      kf4  =-0.001005

   REAL(wp) :: &             ! coeff. for dissoc. of water (Dickson and Riley, 1979 )
      cw0 = -13847.26  , &
      cw1 = 148.9802   , &
      cw2 = -23.6521   , &
      cw3 = 118.67     , &
      cw4 = -5.977     , &
      cw5 = 1.0495     , &
      cw6 = -0.01615
 
   REAL(wp) :: &             ! coeff. for dissoc. of phosphate (Millero (1974)
      cp10 = 115.54    , &
      cp11 = -4576.752 , &
      cp12 = -18.453   , &
      cp13 = -106.736  , &
      cp14 = 0.69171   , &
      cp15 = -0.65643  , &
      cp16 = -0.01844  , &
      cp20 = 172.1033  , &
      cp21 = -8814.715 , &
      cp22 = -27.927   , &
      cp23 = -160.340  , &
      cp24 = 1.3566    , &
      cp25 = 0.37335   , &
      cp26 = -0.05778  , &
      cp30 = -18.126   , &
      cp31 = -3070.75  , &
      cp32 = 17.27039  , &
      cp33 = 2.81197   , &
      cp34 = -44.99486 , &
      cp35 = -0.09984

   REAL(wp) :: &             ! coeff. for dissoc. of silicates (Millero (1974)  
      cs10 = 117.40    , &  
      cs11 = -8904.2   , & 
      cs12 = -19.334   , & 
      cs13 = -458.79   , & 
      cs14 = 3.5913    , & 
      cs15 = 188.74    , & 
      cs16 = -1.5998   , & 
      cs17 = -12.1652  , & 
      cs18 = 0.07871   , & 
      cs19 = 0.        , & 
      cs20 = 1.        , & 
      cs21 = -0.001005

   REAL(wp) :: &            ! coeff. for apparent solubility equilibrium 
      ! akcc1 = -34.452 , & ! of calcite (Ingle,1975,  1800, eq. 6)
      ! akcc2 = -39.866 , &  
      ! akcc3 = 110.21  , &  
      ! akcc4 = -7.5752E-6  
      akcc1 = -171.9065 , &    ! Millero et al. 1995 from Mucci 1983
      akcc2 = -0.077993 , &  
      akcc3 = 2839.319  , &  
      akcc4 = 71.595    , &  
      akcc5 = -0.77712  , &  
      akcc6 = 0.0028426 , &  
      akcc7 = 178.34    , &  
      akcc8 = -0.07711  , &  
      akcc9 = 0.0041249

   REAL(wp) :: &                 ! universal gas constant
      rgas = 83.145              ! bar.cm3/(mol.K) or R=8.31451 J/(g.mol.K)


   ! coeff. for seawater pressure correction (millero 95)		
   REAL(wp), DIMENSION(8)  :: & 
      devk1, devk2, devk3, devk4, devk5

   DATA devk1/ -25.5   , -15.82    , -29.48  , -25.60    , -48.76    , -14.51   , -23.12   , -26.57   /   
   DATA devk2/ 0.1271  , -0.0219   , 0.1622  , 0.2324    , -0.5304   , 0.1211   ,  0.1758  , 0.2020   /   
   DATA devk3/ 0.      , 0.        , 2.608E-3, -3.6246E-3, 0.        , -0.321E-3, -2.647E-3, -3.042E-3/   
   DATA devk4/-3.08E-3 , 1.13E-3   , -2.84E-3, -5.13E-3  , -11.76E-3 , -2.67E-3 , -5.15E-3 , -4.08E-3 /   
   DATA devk5/0.0877E-3, -0.1475E-3, 0.      , 0.0794E-3 , -0.3692E-3, 0.0427E-3,  0.09E-3 , 0.0714E-3/


   ! coeff. for density of sea water (Millero & Poisson 1981) 
   REAL(wp), DIMENSION(5)  :: Adsw                       
   DATA Adsw/8.24493E-1, -4.0899E-3, 7.6438E-5 , -8.246E-7, 5.3875E-9 /

   REAL(wp), DIMENSION(3)  :: Bdsw 
   DATA Bdsw / -5.72466E-3, 1.0227E-4, -1.6546E-6 /

   REAL(wp)  :: Cdsw = 4.8314E-4

   REAL(wp), DIMENSION(6)  :: Ddsw                    
   DATA Ddsw / 999.842594 , 6.793952E-2 , -9.095290E-3, 1.001685E-4, -1.120083E-6, 6.536332E-9/

CONTAINS

   SUBROUTINE sed_chem( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_chem  ***
      !!
      !! ** Purpose :   set chemical constants
      !!
      !!   History :
      !!        !  04-10  (N. Emprin, M. Gehlen )  Original code
      !!        !  06-04  (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------
      !!* Arguments
      INTEGER, INTENT(in) :: kt                     ! time step

#if ! defined key_sed_off
      INTEGER  :: ji, jj, ikt
      REAL(wp) :: ztkel, ztc, ztc2, zpres, ztr 
      REAL(wp) :: zsal, zsal2, zsqrt, zsal15  
      REAL(wp) :: zis, zis2, zisqrt          
      REAL(wp) :: zdens0, zaw, zbw, zcw    
      REAL(wp) :: zbuf1, zbuf2 
      REAL(wp) :: zcpexp, zcpexp2
      REAL(wp) :: zck1p, zck2p, zck3p, zcksi   
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   zchem_data

#endif
      !!----------------------------------------------------------------------

      IF( MOD( kt - 1, nfreq ) /= 0 ) RETURN

      WRITE(numsed,*) ' Getting Chemical constants from tracer model at time kt = ', kt
      WRITE(numsed,*) ' '


#if defined key_sed_off
      CALL sed_chem_off
#else
      ! reading variables
      ALLOCATE( zchem_data(jpi,jpj,6) )   ;   zchem_data(:,:,:) = 0.

      DO jj = 1,jpj
         DO ji = 1, jpi
            ikt = mbkt(ji,jj) 
            IF ( tmask(ji,jj,ikt) == 1 ) THEN
               zchem_data(ji,jj,1) = ak13 (ji,jj,ikt)
               zchem_data(ji,jj,2) = ak23 (ji,jj,ikt)
               zchem_data(ji,jj,3) = akb3 (ji,jj,ikt)
               zchem_data(ji,jj,4) = akw3 (ji,jj,ikt)
               zchem_data(ji,jj,5) = aksp (ji,jj,ikt)
               zchem_data(ji,jj,6) = borat(ji,jj,ikt)
            ENDIF
         ENDDO
      ENDDO

      CALL pack_arr ( jpoce, ak1s  (1:jpoce), zchem_data(1:jpi,1:jpj,1), iarroce(1:jpoce) )
      CALL pack_arr ( jpoce, ak2s  (1:jpoce), zchem_data(1:jpi,1:jpj,2), iarroce(1:jpoce) )
      CALL pack_arr ( jpoce, akbs  (1:jpoce), zchem_data(1:jpi,1:jpj,3), iarroce(1:jpoce) )
      CALL pack_arr ( jpoce, akws  (1:jpoce), zchem_data(1:jpi,1:jpj,4), iarroce(1:jpoce) )
      CALL pack_arr ( jpoce, aksps (1:jpoce), zchem_data(1:jpi,1:jpj,5), iarroce(1:jpoce) )
      CALL pack_arr ( jpoce, borats(1:jpoce), zchem_data(1:jpi,1:jpj,6), iarroce(1:jpoce) )

      DEALLOCATE( zchem_data )

      DO ji = 1, jpoce
         ztkel   = temp(ji) + 273.16
         ztc     = temp(ji)
         ztc2    = ztc * ztc
         zpres   = press(ji)
         ! zqtt    = ztkel * 0.01
         zsal    = salt(ji)
         zsal2   = zsal * zsal 
         zsqrt   = SQRT( zsal )
         zsal15  = zsqrt * zsal
         zlogt   = LOG( ztkel )
         ztr     = 1./ ztkel
         ! zis=ionic strength (ORNL/CDIAC-74, DOE 94,Dickson and Goyet)
         zis     = 19.924 * zsal / ( 1000. - 1.005 * zsal )
         zis2    = zis * zis
         zisqrt  = SQRT( zis )

         ! Density of Sea Water - F(temp,sal) [kg/m3]
         zdens0 =  Ddsw(1) + Ddsw(2) * ztc + Ddsw(3) * ztc2 &
                  + Ddsw(4) * ztc * ztc2 + Ddsw(5) * ztc2 * ztc2 &
                  + Ddsw(6) * ztc * ztc2 * ztc2
         zaw =  Adsw(1) + Adsw(2) * ztc + Adsw(3)* ztc2 + Adsw(4) * ztc * ztc2 &
              + Adsw(5) * ztc2 * ztc2
         zbw =  Bdsw(1) + Bdsw(2) * ztc + Bdsw(3) * ztc2
         zcw =  Cdsw
         densSW(ji) = zdens0 + zaw * zsal + zbw * zsal15 + zcw * zsal * zsal
         densSW(ji) = densSW(ji) * 1E-3   ! to get dens in [kg/l]

         ! FORMULA FOR CPEXP AFTER EDMOND AND GIESKES (1970) 
         ! (REFERENCE TO CULBERSON AND PYTKOQICZ (1968) AS MADE IN BROECKER ET AL. (1982) 
         ! IS INCORRECT; HERE RGAS IS TAKEN TENFOLD TO CORRECT FOR THE NOTATION OF pres  IN
         ! DBAR INSTEAD OF BAR AND THE EXPRESSION FOR CPEXP IS MULTIPLIED BY LN(10.) 
         ! TO ALLOW USE OF EXP-FUNCTION WITH BASIS E IN THE FORMULA FOR AKSPP 
         ! (CF. EDMOND AND GIESKES (1970), P. 1285 AND P. 1286 (THE SMALL FORMULA ON P. 1286 
         ! IS RIGHT AND CONSISTENT WITH THE SIGN IN PARTIAL MOLAR VOLUME CHANGE AS SHOWN ON P. 1285)
         !-----------------------------------------------------------------------------------------
         zcpexp  = zpres / ( rgas*ztkel )
         zcpexp2 = zpres * zcpexp

         ! For Phodphates (phosphoric acid) (DOE 1994)
         !----------------------------------------------
         zck1p = cp10 + cp11*ztr + cp12*zlogt + ( cp13*ztr + cp14 ) * zsqrt &
            &      + ( cp15*ztr + cp16 ) * zsal
         zck2p = cp20 + cp21*ztr + cp22*zlogt + ( cp23*ztr + cp24 ) * zsqrt &
            &      + ( cp25*ztr + cp26 ) * zsal
         zck3p = cp30 + cp31*ztr + ( cp32*ztr + cp33 ) *  zsqrt &
            &      + ( cp34*ztr + cp35 ) * zsal

         ! For silicates (DOE 1994) change to mol/kg soln) (OCMIP)
         !--------------------------------------------------------
         zcksi = cs10 + cs11*ztr + cs12*zlogt + ( cs13*ztr + cs14) * zisqrt &
            &      + ( cs15*ztr + cs16 ) * zis &
            &      + ( cs17*ztr + cs18 ) * zis2 &
            &      + LOG( 1. + cs19*zsal ) + LOG( cs20 + cs21*zsal )


         !K1, K2 of carbonic acid, KB of boric acid, KW (H2O)
         !---------------------------------------------------
         zak1p  = EXP ( zck1p  )
         zak2p  = EXP ( zck2p  )
         zak3p  = EXP ( zck3p  )
         zaksil = EXP ( zcksi  )

         zbuf1       = - ( devk1(3) + devk2(3)*ztc + devk3(3)*ztc2 )
         zbuf2       = 0.5 * ( devk4(3) + devk5(3)*ztc )
         aksis(ji)     = zaksil * EXP( zbuf1*zcpexp + zbuf2*zcpexp2 )

         zbuf1       = - ( devk1(6) + devk2(6)*ztc + devk3(6)*ztc2 )
         zbuf2       = 0.5 * ( devk4(6) + devk5(6)*ztc )
         ak1ps(ji)   = zak1p * EXP( zbuf1*zcpexp + zbuf2*zcpexp2 )
 
         zbuf1       = - ( devk1(7) + devk2(7)*ztc + devk3(7)*ztc2 )
         zbuf2       = 0.5 * ( devk4(7) + devk5(7)*ztc )
         ak2ps(ji)   = zak2p * EXP( zbuf1*zcpexp + zbuf2*zcpexp2 )

         zbuf1       = - ( devk1(8) + devk2(8)*ztc + devk3(8)*ztc2 )
         zbuf2       = 0.5 * ( devk4(8) + devk5(8)*ztc )
         ak3ps(ji)   = zak3p * EXP( zbuf1*zcpexp + zbuf2*zcpexp2 )

         ak12s  (ji) = ak1s (ji) * ak2s (ji)
         ak12ps (ji) = ak1ps(ji) * ak2ps(ji)
         ak123ps(ji) = ak1ps(ji) * ak2ps(ji) * ak3ps(ji)

         calcon2(ji) = 0.01028 * ( salt(ji) / 35. ) * densSW(ji)
      ENDDO

       
#endif

   END SUBROUTINE sed_chem

#if defined key_sed_off

   SUBROUTINE sed_chem_off
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_chem_off  ***
      !!                    
      !! ** Purpose :   compute chemical constants
      !!
      !!   History :
      !!        !  04-10  (N. Emprin, M. Gehlen )  Original code
      !!        !  06-04  (C. Ethe)  Re-organization 
      !!----------------------------------------------------------------------  
      !! * Local declarations
      INTEGER :: ji

      REAL(wp) ::  ztkel, ztc, ztc2, zpres, ztr 
      REAL(wp) ::  zsal, zsal2, zsqrt, zsal15   
      REAL(wp) ::  zis, zis2, zisqrt            
      REAL(wp) ::  zdens0, zaw, zbw, zcw        
      REAL(wp) ::  zchl, zst, zft, zbuf1, zbuf2 
      REAL(wp) ::  zcpexp, zcpexp2              
      REAL(wp) ::  zckb, zck1, zck2, zckw       
      REAL(wp) ::  zck1p, zck2p, zck3p, zcksi   
      REAL(wp) ::  zak1, zak2, zakb, zakw              
      REAL(wp) ::  zaksp0, zksp, zks, zkf 


      ! CHEMICAL CONSTANTS - DEEP OCEAN
      !-------------------------------------
      ! [chem constants]=mol/kg solution (or (mol/kg sol)2 for akws and aksp)

      DO ji = 1, jpoce
         ztkel   = temp(ji) + 273.16
         ztc     = temp(ji)
         ztc2    = ztc * ztc
         zpres   = press(ji)
         ! zqtt    = ztkel * 0.01
         zsal    = salt(ji)
         zsal2   = zsal * zsal 
         zsqrt   = SQRT( zsal )
         zsal15  = zsqrt * zsal
         zlogt   = LOG( ztkel )
         ztr     = 1./ ztkel
         ! zis=ionic strength (ORNL/CDIAC-74, DOE 94,Dickson and Goyet)
         zis     = 19.924 * zsal / ( 1000. - 1.005 * zsal )
         zis2    = zis * zis
         zisqrt  = SQRT( zis )


         ! Density of Sea Water - F(temp,sal) [kg/m3]
         zdens0 =  Ddsw(1) + Ddsw(2) * ztc + Ddsw(3) * ztc2 &
                  + Ddsw(4) * ztc * ztc2 + Ddsw(5) * ztc2 * ztc2 &
                  + Ddsw(6) * ztc * ztc2 * ztc2
         zaw =  Adsw(1) + Adsw(2) * ztc + Adsw(3)* ztc2 + Adsw(4) * ztc * ztc2 &
              + Adsw(5) * ztc2 * ztc2
         zbw =  Bdsw(1) + Bdsw(2) * ztc + Bdsw(3) * ztc2
         zcw =  Cdsw
         densSW(ji) = zdens0 + zaw * zsal + zbw * zsal15 + zcw * zsal * zsal
         densSW(ji) = densSW(ji) * 1E-3   ! to get dens in [kg/l]


         ! FORMULA FOR CPEXP AFTER EDMOND AND GIESKES (1970) 
         ! (REFERENCE TO CULBERSON AND PYTKOQICZ (1968) AS MADE IN BROECKER ET AL. (1982) 
         ! IS INCORRECT; HERE RGAS IS TAKEN TENFOLD TO CORRECT FOR THE NOTATION OF pres  IN
         ! DBAR INSTEAD OF BAR AND THE EXPRESSION FOR CPEXP IS MULTIPLIED BY LN(10.) 
         ! TO ALLOW USE OF EXP-FUNCTION WITH BASIS E IN THE FORMULA FOR AKSPP 
         ! (CF. EDMOND AND GIESKES (1970), P. 1285 AND P. 1286 (THE SMALL FORMULA ON P. 1286 
         ! IS RIGHT AND CONSISTENT WITH THE SIGN IN PARTIAL MOLAR VOLUME CHANGE AS SHOWN ON P. 1285)
         !-----------------------------------------------------------------------------------------
         zcpexp  = zpres / ( rgas*ztkel )
         zcpexp2 = zpres * zcpexp


         ! chlorinity (WOOSTER ET AL., 1969)
         !---------------------------------------
         zchl = zsal * salchl

         ! total sulfate concentration [mol/kg soln]
         ! --------------------------------------
         zst = st1 * zchl * st2

         ! total fluoride concentration [mol/kg soln]
         ! --------------------------------------
         zft = ft1 * zchl * ft2

         ! dissociation constant for carbonate (Mehrback 74 - Dickson & Millero 87)
         !---------------------------------------------------------------------------
         zck1 = c10*ztr - c11 + c12*zlogt - c13*zsal + c14*zsal2
         zck2 = c20*ztr + c21 - c22*zlogt - c23*zsal + c24*zsal2

         ! dissociation constant for sulfates (Dickson 1990)
         !--------------------------------------------------
         zks = EXP(  ks0 + ks1*ztr + ks2*zlogt &
            &    + ( ks3*ztr + ks4 + ks5*zlogt ) * zisqrt &
            &    + ( ks6*ztr + ks7 + ks8*zlogt ) * zis    &
            &    +   ks9*ztr*zis*zisqrt + ks10*ztr*zis2   &
            &    +   LOG( ks11 + ks12*zsal ) )

         ! dissociation constant for fluorides (Dickson and Riley 79)
         !--------------------------------------------------
         zkf = EXP( kf0 + kf1*ztr + kf2*zisqrt + LOG( kf3 + kf4*zsal ) )

         ! dissociation constant for borates (Doe 94)
         !--------------------------------------------------
         zckb = (  cb0 + cb1*zsqrt + cb2*zsal + cb3*zsal15 + cb4*zsal2) * ztr &
            &  + ( cb5 + cb6*zsqrt + cb7*zsal) &
            &  + ( cb8 + cb9*zsqrt + cb10*zsal) * zlogt &
            &  +   cb11*zsqrt*ztkel + LOG( ( 1. + zst/zks + zft/zkf ) / ( 1. + zst/zks ) ) 

         ! PKW (H2O) (DICKSON AND RILEY, 1979)
         !--------------------------------------
         zckw =   cw0*ztr + cw1 + cw2*zlogt &
            & +( cw3*ztr + cw4 + cw5*zlogt )* zsqrt + cw6*zsal
         
         ! For Phodphates (phosphoric acid) (DOE 1994)
         !----------------------------------------------
         zck1p = cp10 + cp11*ztr + cp12*zlogt + ( cp13*ztr + cp14 ) * zsqrt &
            &      + ( cp15*ztr + cp16 ) * zsal
         zck2p = cp20 + cp21*ztr + cp22*zlogt + ( cp23*ztr + cp24 ) * zsqrt &
            &      + ( cp25*ztr + cp26 ) * zsal
         zck3p = cp30 + cp31*ztr + ( cp32*ztr + cp33 ) *  zsqrt &
            &      + ( cp34*ztr + cp35 ) * zsal

         ! For silicates (DOE 1994) change to mol/kg soln) (OCMIP)
         !--------------------------------------------------------
         zcksi = cs10 + cs11*ztr + cs12*zlogt + ( cs13*ztr + cs14) * zisqrt &
            &      + ( cs15*ztr + cs16 ) * zis &
            &      + ( cs17*ztr + cs18 ) * zis2 &
            &      + LOG( 1. + cs19*zsal ) + LOG( cs20 + cs21*zsal )

         ! apparent solublity product K'SP of calcite in seawater
         ! (S=27-43, T=2-25 DEG C) AT pres =0 (INGLE, 1975, EQ. 6)
         ! prob: olivier a log = ln et C. Heize a LOG10(sal)
         ! aksp0 = 1.E-7*(akcc1+akcc2*sal**(1./3.)+akcc3*log(sal)+akcc4*tkel*tkel)
         ! aksp0 = 1.E-7*(akcc1+akcc2*sal**(1./3.)+akcc3*log10(sal)+akcc4*tkel*tkel)
         !--------------------------------------------------------------------
         zaksp0 = akcc1 + akcc2*ztkel + akcc3*ztr + akcc4 * LOG10(ztkel) &
            &  + ( akcc5 + akcc6*ztkel+ akcc7*ztr ) * zsqrt &
            &  +  akcc8*zsal + akcc9*zsal15

         !K1, K2 of carbonic acid, KB of boric acid, KW (H2O)
         !---------------------------------------------------
         zak1   = 10**( -zck1  )
         zak2   = 10**( -zck2  )
         zakb   = EXP ( zckb   ) 
         zakw   = EXP ( zckw   )
         zksp   = 10**( zaksp0 )



         ! KB of boric acid, K1,K2 of carbonic acid pressure correction 
         ! after Culberson and  AND Pytkowicz (1968) (CF. BROECKER ET AL., 1982) Millero 95
         !--------------------------------------------------------------------------------
         zbuf1       = - ( devk1(1) + devk2(1)*ztc + devk3(1)*ztc2 )
         zbuf2       = 0.5 * ( devk4(1) + devk5(1)*ztc )
         ak1s(ji)    = zak1 * EXP( zbuf1*zcpexp + zbuf2*zcpexp2 )

         zbuf1       = -( devk1(2) + devk2(2)*ztc + devk3(2)*ztc2 )
         zbuf2       = 0.5 * ( devk4(2) + devk5(2)*ztc )
         ak2s(ji)    = zak2 * EXP( zbuf1*zcpexp + zbuf2*zcpexp2 )

         zbuf1       = - ( devk1(3) + devk2(3)*ztc + devk3(3)*ztc2 )
         zbuf2       = 0.5 * ( devk4(3) + devk5(3) * ztc )
         akbs(ji)    = zakb * EXP( zbuf1*zcpexp + zbuf2*zcpexp2 )

         zbuf1       = - ( devk1(4) + devk2(4)*ztc + devk3(4)*ztc2 )
         zbuf2       = 0.5 * ( devk4(4) + devk5(4)*ztc )
         akws(ji)    = zakw * EXP( zbuf1*zcpexp + zbuf2*zcpexp2 )


         ! APPARENT SOLUBILITY PRODUCT K''SP OF CALCITE (OR ARAGONITE)
         ! AS FUNCTION OF PRESSURE FOLLWING EDMOND AND GIESKES (1970)
         ! (P. 1285) AND BERNER (1976)
         !-----------------------------------------------------------------
         ! aksp(ji) = aksp0*exp(zcpexp*(devks-devkst*tc))
         ! or following Mucci
         zbuf1      = - ( devk1(5) + devk2(5)*ztc + devk3(5)*ztc2 )
         zbuf2      = 0.5 *( devk4(5) + devk5(5)*ztc )
         aksps(ji)   = zksp * EXP( zbuf1*zcpexp + zbuf2*zcpexp2 )

         ! For Phodphates (phosphoric acid) (DOE 1994)
         !----------------------------------------------
         zck1p = cp10 + cp11*ztr + cp12*zlogt + ( cp13*ztr + cp14 ) * zsqrt &
            &      + ( cp15*ztr + cp16 ) * zsal
         zck2p = cp20 + cp21*ztr + cp22*zlogt + ( cp23*ztr + cp24 ) * zsqrt &
            &      + ( cp25*ztr + cp26 ) * zsal
         zck3p = cp30 + cp31*ztr + ( cp32*ztr + cp33 ) *  zsqrt &
            &      + ( cp34*ztr + cp35 ) * zsal

         ! For silicates (DOE 1994) change to mol/kg soln) (OCMIP)
         !--------------------------------------------------------
         zcksi = cs10 + cs11*ztr + cs12*zlogt + ( cs13*ztr + cs14) * zisqrt &
            &      + ( cs15*ztr + cs16 ) * zis &
            &      + ( cs17*ztr + cs18 ) * zis2 &
            &      + LOG( 1. + cs19*zsal ) + LOG( cs20 + cs21*zsal )


         !K1, K2 of carbonic acid, KB of boric acid, KW (H2O)
         !---------------------------------------------------
         zak1p  = EXP ( zck1p  )
         zak2p  = EXP ( zck2p  )
         zak3p  = EXP ( zck3p  )
         zaksil = EXP ( zcksi  )

         zbuf1       = - ( devk1(3) + devk2(3)*ztc + devk3(3)*ztc2 )
         zbuf2       = 0.5 * ( devk4(3) + devk5(3)*ztc )
         aksis(ji)     = zaksil * EXP( zbuf1*zcpexp + zbuf2*zcpexp2 )

         zbuf1       = - ( devk1(6) + devk2(6)*ztc + devk3(6)*ztc2 )
         zbuf2       = 0.5 * ( devk4(6) + devk5(6)*ztc )
         ak1ps(ji)   = zak1p * EXP( zbuf1*zcpexp + zbuf2*zcpexp2 )
 
         zbuf1       = - ( devk1(7) + devk2(7)*ztc + devk3(7)*ztc2 )
         zbuf2       = 0.5 * ( devk4(7) + devk5(7)*ztc )
         ak2ps(ji)   = zak2p * EXP( zbuf1*zcpexp + zbuf2*zcpexp2 )

         zbuf1       = - ( devk1(8) + devk2(8)*ztc + devk3(8)*ztc2 )
         zbuf2       = 0.5 * ( devk4(8) + devk5(8)*ztc )
         ak3ps(ji)   = zak3p * EXP( zbuf1*zcpexp + zbuf2*zcpexp2 )

         ! total borat concentration. [mol/l]
         ! or from Millero 1995 [mol/l] : borat(l) = 0.000416_8*(sal/35._8)*densSW(l)
         ! --------------------------------------------------------------------------
         borats(ji) = bor1 * zchl * bor2 * densSW(ji)

         ak12s  (ji) = ak1s (ji) * ak2s (ji)
         ak12ps (ji) = ak1ps(ji) * ak2ps(ji)
         ak123ps(ji) = ak1ps(ji) * ak2ps(ji) * ak3ps(ji)

         calcon2(ji) = 0.01028 * ( zsal / 35. ) * densSW(ji)

      ENDDO

   END SUBROUTINE sed_chem_off

#endif

#else
   !!======================================================================
   !! MODULE sedchem  :   Dummy module 
   !!======================================================================
CONTAINS
   SUBROUTINE sed_chem( kt )         ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'trc_stp: You should not have seen this print! error?', kt
   END SUBROUTINE sed_chem

   !!======================================================================

#endif

END MODULE sedchem
