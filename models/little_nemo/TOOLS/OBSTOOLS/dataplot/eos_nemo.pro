function eos_nemo, t_in, psal
;------------------------------------------------------------------------------
;
; Purpose:
;       To calculate the density using the equation of state used in NEMO.
;       This is based on the Jackett and McDougall (1994) equation of state
;       for calculating the in situ density based on potential temperature
;       and salinity.
;
; Inputs:
;       temperature    =>  1d array potential temperature (deg C)
;       salinity       =>  1d array salinity (PSU)
;
; Outputs:
;       rhop            =>  1d array pot density (kg/m**3)
;
; could add use a difference reference pressure
; observations are in-situ temperature?
;
; Author:
;       D. J. Lea. Dec 2006.
;------------------------------------------------------------------------------

          zws = SQRT( ABS( psal ) )

	sz=size(t_in)
	ndim=sz(0)
	NO_LEVS=sz(ndim)
	
	prhop=psal*0.

;                 
          for jk = 0L, NO_LEVS-1 do begin  

                   zt = T_IN(jk)
                   zs = psal(jk)
; * depth
;                   zh = O_DEP_LEVS(jk)		;used in calculating insitu density only
                   zsr= zws(jk)
; * compute volumic mass pure water at atm pressure
                   zr1= ( ( ( ( 6.536332e-9*zt-1.120083e-6 )*zt+1.001685e-4)*zt   $
                      -9.095290e-3 )*zt+6.793952e-2 )*zt+999.842594
; * seawater volumic mass atm pressure
                   zr2= ( ( ( 5.3875e-9*zt-8.2467e-7 ) *zt+7.6438e-5 ) *zt   $
                      -4.0899e-3 ) *zt+0.824493
                   zr3= ( -1.6546e-6*zt+1.0227e-4 ) *zt-5.72466e-3
                   zr4= 4.8314e-4

; * potential volumic mass (reference to the surface)
                   zrhop= ( zr4*zs + zr3*zsr + zr2 ) *zs + zr1

; * save potential volumic mass
                   prhop(jk) = zrhop

; * add the compression terms
                   ze = ( -3.508914e-8*zt-1.248266e-8 ) *zt-2.595994e-6
                   zbw= (  1.296821e-6*zt-5.782165e-9 ) *zt+1.045941e-4
                   zb = zbw + ze * zs

                   zd = -2.042967e-2
                   zc =   (-7.267926e-5*zt+2.598241e-3 ) *zt+0.1571896
                   zaw= ( ( 5.939910e-6*zt+2.512549e-3 ) *zt-0.1028859 ) *zt - 4.721788
                   za = ( zd*zsr + zc ) *zs + zaw

                   zb1=   (-0.1909078*zt+7.390729 ) *zt-55.87545
                   za1= ( ( 2.326469e-3*zt+1.553190)*zt-65.00517 ) *zt+1044.077
                   zkw= ( ( (-1.361629e-4*zt-1.852732e-2 ) *zt-30.41638 ) *zt + 2098.925 ) *zt+190925.6
                   zk0= ( zb1*zsr + za1 )*zs + zkw

;                   ; in situ density anomaly
;                   prd(jk) = zrhop / (  1.0 - zh / ( zk0 - zh * ( za - zh * zb ) )  )
;
          endfor  



return, prhop

END                  
