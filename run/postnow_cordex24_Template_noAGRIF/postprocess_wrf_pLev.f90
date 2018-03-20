program postprocess

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! N. Jourdain, created in IRD,  Noumea, march 2008
!              updated in CCRC, Sydney, march 2013
!                                last:  july 2013
!
! This script is used to postprocess WRF's outputs
!
! Action: calculate fields on pressure levels
!         (outputs suitable to use tropical cyclone tracking) 
!
!-- Compilation with PGI on editr (NOUMEA) :
! pgf90 -c -I/opt/netcdf-3.6_pgi_706/include make_postprocess_netcdf.f90
! pgf90 -o do make_postprocess_netcdf.o -L /opt/netcdf-3.6_pgi_706/lib -lnetcdf
!
!-- Compilation on raijin (SYDNEY) :
! module load netcdf/4.2.1.1
! NC_INC='-I /apps/netcdf/4.2.1.1/include'
! NC_LIB='-L /apps/netcdf/4.2.1.1/lib -lnetcdf -lnetcdff'
! ifort -c $NC_INC postprocess_wrf.f90
! ifort -o postprowrf postprocess_wrf.o $NC_LIB
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

USE netcdf

IMPLICIT NONE

!- variable ID for netcdf interface
INTEGER :: fidA, fidM, dimID_t, dimID_DSL, dimID_Xm1,   &
& dimID_Ym1, dimID_X, dimID_Zm1, dimID_Y, dimID_Z,      &
& dimID_W, mx, mx1, mx2, my, my1, my2, mz, mz1, mw,     &
& mDSL, Times_ID, XTIME_ID, ITIMESTEP_ID, U_ID, V_ID,   &
& P_ID, PB_ID, T_ID, PH_ID, PHB_ID, Vort_p_ID, W_ID,    &
& U_p_ID, V_p_ID, T_p_ID, RH_p_ID, The_p_ID, MoCo_p_ID, &
& status, i, j, k, l, mt, mz2, X_ID, Y_ID, QVAPOR_ID,   &
& W_p_ID, gz_p_ID

INTEGER,ALLOCATABLE,DIMENSION(:) :: ITIMESTEP

! name of input/output files
CHARACTER(LEN=100) :: file_in, file_out

CHARACTER(LEN=20) :: nam_gz, nam_U, nam_V, nam_T, nam_Vort, nam_MoCo, nam_The, nam_RH, nam_W

CHARACTER(LEN=90) :: att_gz, att_U, att_V, att_T, att_Vort, att_MoCo, att_The, att_RH, att_W

CHARACTER(LEN=19),ALLOCATABLE,DIMENSION(:) :: Times

REAL :: Dx, SH, SC

!- dimensions
REAL,ALLOCATABLE,DIMENSION(:) :: XTIME, X, Y, T

REAL,ALLOCATABLE,DIMENSION(:,:,:,:) :: U, V, Temp, P, PB,&
& PH, PHB, U_M, V_M, dxV, dyU, pr, Vort, Theta, QVAPOR,  &
& RH, Th_e, MoistConv, W, gz

!- variables used for vertical interpolation between M-points:
REAL*4 :: pres_inf, gz_inf, U_inf, V_inf, T_inf, Vort_inf, pres_sup,    &
& gz_sup, U_sup, V_sup, T_sup, Vort_sup, MoCo_inf, MoCo_sup, Th_e_inf,  &
& RH_inf, Th_e_sup, RH_sup

!- variables used for vertical interpolation between W-points:
REAL*4 :: prWi, prWs, pres_infW, pres_supW, W_inf, W_sup

!REAL,ALLOCATABLE,DIMENSION(:,:,:) :: P1

!- variables on pressure levels
REAL,ALLOCATABLE,DIMENSION(:,:,:) :: gz_p, U_p, V_p, T_p, Vort_p, Th_e_p, MoCo_p, RH_p, W_p

INTEGER :: dom, an, mois, jour

!- pressure levels on which interpolation is done:
REAL*4, ALLOCATABLE,DIMENSION(:) :: pLev

INTEGER :: mLev, iLev

!-----------------------------------------------------------------------------------

!- specify the presure levels to save as output:
!  NB: don't leave or remove extra space on the 2 lines below (needed by bash script)
mLev=11
ALLOCATE(pLev(mLev))
pLev=(/950.0, 900.0, 850.0, 800.0, 750.0, 700.0, 600.0, 500.0, 400.0, 300.0, 200.0/)

!input/output file names defined relatively to date in date.tmp:
open(11,file='date.tmp')
read(11,*) dom, an, mois, jour
close(11)
write(file_in, 101) an, dom, an, mois, jour
101 FORMAT('WORK_',i4.4,'/wrfout_d',i2.2,'_',i4.4,'-',i2.2,'-',i2.2,'_00:00:00')
write(*,*) 'Input  file: ', TRIM(file_in)

!--------------------------------------------
! 1 - Read wrf's raw output file 
!--------------------------------------------

status = NF90_OPEN(TRIM(file_in),NF90_WRITE,fidA) ; call erreur(status,.TRUE.,"read")

!- Inquire ID of each file dimension
status = NF90_INQ_DIMID(fidA,"Time",dimID_t)             ; call erreur(status,.TRUE.,"inq_dimID_t")
status = NF90_INQ_DIMID(fidA,"DateStrLen",dimID_DSL)     ; call erreur(status,.TRUE.,"inq_dimID_DSL")
status = NF90_INQ_DIMID(fidA,"west_east",dimID_Xm1)      ; call erreur(status,.TRUE.,"inq_dimID_Xm1")
status = NF90_INQ_DIMID(fidA,"south_north",dimID_Ym1)    ; call erreur(status,.TRUE.,"inq_dimID_Ym1")
status = NF90_INQ_DIMID(fidA,"west_east_stag",dimID_X)   ; call erreur(status,.TRUE.,"inq_dimID_X")
status = NF90_INQ_DIMID(fidA,"bottom_top",dimID_Zm1)     ; call erreur(status,.TRUE.,"inq_dimID_Zm1")
status = NF90_INQ_DIMID(fidA,"south_north_stag",dimID_Y) ; call erreur(status,.TRUE.,"inq_dimID_Y")
status = NF90_INQ_DIMID(fidA,"bottom_top_stag",dimID_Z)  ; call erreur(status,.TRUE.,"inq_dimID_Z")

!- Inquire file dimensions values
status = NF90_INQUIRE_DIMENSION(fidA,dimID_t,len=mt)     ; call erreur(status,.TRUE.,"inq_dim_t")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_DSL,len=mDSL) ; call erreur(status,.TRUE.,"inq_dim_DSL")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_Xm1,len=mx1)  ; call erreur(status,.TRUE.,"inq_dim_Xm1")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_Ym1,len=my1)  ; call erreur(status,.TRUE.,"inq_dim_Ym1")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_X,len=mx)     ; call erreur(status,.TRUE.,"inq_dim_X")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_Zm1,len=mz1)  ; call erreur(status,.TRUE.,"inq_dim_Zm1")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_Y,len=my)     ; call erreur(status,.TRUE.,"inq_dim_Y")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_Z,len=mz)     ; call erreur(status,.TRUE.,"inq_dim_Z")

!- ALLOCATE input variables + moisture convergence and vorticity:
ALLOCATE( Times(mt), XTIME(mt), T(mt), ITIMESTEP(mt) )
ALLOCATE( U        (mx ,my1,mz1,mt), V   (mx1,my ,mz1,mt), MoistConv(mx1,my1,mz1,mt), Vort (mx1,my1,mz1,mt),  &
&         P        (mx1,my1,mz1,mt), PB  (mx1,my1,mz1,mt), PH       (mx1,my1,mz ,mt), PHB  (mx1,my1,mz ,mt),  &
&         U_M      (mx1,my1,mz1,mt), V_M (mx1,my1,mz1,mt), dxV      (mx1,my1,mz1,mt), dyU  (mx1,my1,mz1,mt),  &
&         QVAPOR   (mx1,my1,mz1,mt), RH  (mx1,my1,mz1,mt), Th_e     (mx1,my1,mz1,mt), pr   (mx1,my1,mz1,mt),  &
&         Theta    (mx1,my1,mz1,mt), Temp(mx1,my1,mz1,mt), W        (mx1,my1,mz ,mt), gz   (mx1,my1,mz1,mt)   )

!- Inquire variable ID
status = NF90_INQ_VARID(fidA,"Times",Times_ID)         ; call erreur(status,.TRUE.,"inq_Times_ID")
status = NF90_INQ_VARID(fidA,"XTIME",XTIME_ID)         ; call erreur(status,.TRUE.,"inq_XTIME_ID")
status = NF90_INQ_VARID(fidA,"ITIMESTEP",ITIMESTEP_ID) ; call erreur(status,.TRUE.,"inq_ITIMESTEP_ID")
status = NF90_INQ_VARID(fidA,"U",U_ID)                 ; call erreur(status,.TRUE.,"inq_U_ID")
status = NF90_INQ_VARID(fidA,"V",V_ID)                 ; call erreur(status,.TRUE.,"inq_V_ID")
status = NF90_INQ_VARID(fidA,"W",W_ID)                 ; call erreur(status,.TRUE.,"inq_W_ID")
status = NF90_INQ_VARID(fidA,"P",P_ID)                 ; call erreur(status,.TRUE.,"inq_P_ID")
status = NF90_INQ_VARID(fidA,"PB",PB_ID)               ; call erreur(status,.TRUE.,"inq_PB_ID")
status = NF90_INQ_VARID(fidA,"T",T_ID)                 ; call erreur(status,.TRUE.,"inq_T_ID")
status = NF90_INQ_VARID(fidA,"PH",PH_ID)               ; call erreur(status,.TRUE.,"inq_PH_ID")
status = NF90_INQ_VARID(fidA,"PHB",PHB_ID)             ; call erreur(status,.TRUE.,"inq_PHB_ID")
status = NF90_INQ_VARID(fidA,"QVAPOR",QVAPOR_ID)       ; call erreur(status,.TRUE.,"inq_QVAPOR_ID")

!- Get variable values 
status = NF90_GET_VAR(fidA,Times_ID,Times)         ; call erreur(status,.TRUE.,"getvar_Times")
status = NF90_GET_VAR(fidA,XTIME_ID,XTIME)         ; call erreur(status,.TRUE.,"getvar_XTIME")
status = NF90_GET_VAR(fidA,ITIMESTEP_ID,ITIMESTEP) ; call erreur(status,.TRUE.,"getvar_ITIMESTEP")
status = NF90_GET_VAR(fidA,U_ID,U)                 ; call erreur(status,.TRUE.,"getvar_U")
status = NF90_GET_VAR(fidA,V_ID,V)                 ; call erreur(status,.TRUE.,"getvar_V")
status = NF90_GET_VAR(fidA,W_ID,W)                 ; call erreur(status,.TRUE.,"getvar_W")
status = NF90_GET_VAR(fidA,P_ID,P)                 ; call erreur(status,.TRUE.,"getvar_P")
status = NF90_GET_VAR(fidA,PB_ID,PB)               ; call erreur(status,.TRUE.,"getvar_PB")
status = NF90_GET_VAR(fidA,T_ID,Theta)             ; call erreur(status,.TRUE.,"getvar_Theta")
status = NF90_GET_VAR(fidA,PH_ID,PH)               ; call erreur(status,.TRUE.,"getvar_PH")
status = NF90_GET_VAR(fidA,PHB_ID,PHB)             ; call erreur(status,.TRUE.,"getvar_PHB")
status = NF90_GET_VAR(fidA,QVAPOR_ID,QVAPOR)       ; call erreur(status,.TRUE.,"getvar_QVAPOR")

!- Read resolution in global attributes:
status = NF90_GET_ATT(fidA,NF90_GLOBAL,"DX",Dx)    ; call erreur(status,.TRUE.,"get_resolution")
write(*,*) 'Resolution Dx = ', Dx

!- Close this file
status = NF90_CLOSE(fidA) ; call erreur(status,.TRUE.,"fin_lecture")

!--------------------------------------------------------
! 2- Calculate fields on every specified pressure levels
!--------------------------------------------------------

ALLOCATE( U_p   (mx1,my1,mt), V_p (mx1,my1,mt), Vort_p(mx1,my1,mt), T_p(mx1,my1,mt), &
&         Th_e_p(mx1,my1,mt), RH_p(mx1,my1,mt), MoCo_p(mx1,my1,mt), W_p(mx1,my1,mt), &
&         gz_p  (mx1,my1,mt) )

DO iLev=1,mLev
 
  write(*,*) 'extracting pressure level ', pLev(iLev)
 
  mx2 = mx - 2
  my2 = my - 2
  mz2 = mz - 2
  
  DO l=1,mt !time
  
   do k=1,mz1
  
    !-----------------------
    ! 1- U and V on grid T :
    do i=1,mx1
    do j=1,my1
       U_M(i,j,k,l) = 0.5 * ( U(i,j,k,l) + U(i+1,j  ,k,l) )
       V_M(i,j,k,l) = 0.5 * ( V(i,j,k,l) + V(i  ,j+1,k,l) )
    enddo
    enddo
  
    !--------------------------------------------------
    ! 2- Vorticity and moisture convergence (on T grid)
    Vort( 1  , :  , k, l) = 0.0 ; MoistConv( 1  , :  , k, l) = 0.0
    Vort( mx1, :  , k, l) = 0.0 ; MoistConv( mx1, :  , k, l) = 0.0
    Vort( :  , 1  , k, l) = 0.0 ; MoistConv( :  , 1  , k, l) = 0.0
    Vort( :  , my1, k, l) = 0.0 ; MoistConv( :  , my1, k, l) = 0.0

    do i=2,mx2
    do j=2,my2
 
      ! nj NB: should be adapted for non uniform Dx.......
 
      !Vorticity (in s^-1)
      Vort(i,j,k,l) = ( V_M(i+1,j,k,l) - V_M(i-1,j,k,l) - U_M(i,j+1,k,l) + U_M(i,j-1,k,l) ) / (2*Dx)
  
      !horizontal moisture convergence =-div(q.v) in g.kg^-1.day^-1
      MoistConv(i,j,k,l) = (     U(i+1,j  ,k,l) * 0.5 * ( QVAPOR(i  ,j  ,k,l) + QVAPOR(i+1,j  ,k,l) )      &        
         &                     - U(i  ,j  ,k,l) * 0.5 * ( QVAPOR(i-1,j  ,k,l) + QVAPOR(i  ,j  ,k,l) )      &
         &                     + V(i  ,j+1,k,l) * 0.5 * ( QVAPOR(i  ,j  ,k,l) + QVAPOR(i  ,j+1,k,l) )      &
         &                     - V(i  ,j  ,k,l) * 0.5 * ( QVAPOR(i  ,j+1,k,l) + QVAPOR(i  ,j  ,k,l) )  )   &
         &                 * 86400.0 * (-1.e3) / (2*Dx)
  
    enddo
    enddo
  
    !---------------
    ! 3 - total pressure
    !   - potential temperature <-> real temperature
    !   - computation of Relative Humidity 
    do i=1,mx1
    do j=1,my1
  
       !total pressure
        pr(i,j,k,l)   = ( P(i,j,k,l) + PB(i,j,k,l) ) * 0.01
 
       !total geopotential
        gz(i,j,k,l)   = ( PH(i,j,k,l) + PHB(i,j,k,l) ) 
 
       !real temperature
        Temp(i,j,k,l) = (Theta(i,j,k,l)+300.) * ( pr(i,j,k,l)/1000. )**0.286  
  
       !equivalent potential temperature
        Th_e(i,j,k,l) =  (Theta(i,j,k,l)+300.)                             &
  &                     *(  (Temp(i,j,k,l)+2490*QVAPOR(i,j,k,l))           &
  &                        / Temp(i,j,k,l)                         )
  
       !specific humidity
        SH = QVAPOR(i,j,k,l)/(1.+QVAPOR(i,j,k,l)) 
        SC =  ( 379.9052/(100*pr(i,j,k,l)) )                               &
  &                *exp( 17.2694*(Temp(i,j,k,l)-273.16)                    &
  &                             /(Temp(i,j,k,l)-35.86 )   )
  
       !relative humidity
        RH(i,j,k,l) = min(max(SH/SC,0.0),1.0)
  
    enddo
    enddo
  
   enddo !! do k=1,mz1
  
   !---------------
   ! 4- Interpolation on pressure level: 
 
   do i=1,mx1
   do j=1,my1
  
     !- 1st level pressure :
     !P1(i,j,l) = ( P(i,j,1,l) + PB(i,j,1,l) ) * 0.01
  
     !- fields interpolated between two T-points
     pres_inf    =  0.
     gz_inf      =  0.
     U_inf       =  0.
     V_inf       =  0.
     T_inf       =  0.
     Vort_inf    =  0.
     MoCo_inf    =  0.
     Th_e_inf    =  0.
     RH_inf      =  0.
     do k=1,mz1
       if ( pr(i,j,k,l) .gt. pLev(iLev) ) then
         pres_inf  =  pr       (i,j,k,l)
         gz_inf    =  gz       (i,j,k,l)
         U_inf     =  U_M      (i,j,k,l)
         V_inf     =  V_M      (i,j,k,l)
         T_inf     =  Temp     (i,j,k,l)
         Vort_inf  =  Vort     (i,j,k,l)
         MoCo_inf  =  MoistConv(i,j,k,l)
         Th_e_inf  =  Th_e     (i,j,k,l)
         RH_inf    =  RH       (i,j,k,l)
       end if
       if ( pr(i,j,mz-k,l) .le. pLev(iLev) ) then
         pres_sup  =  pr       (i,j,mz-k,l)
         gz_sup    =  gz       (i,j,mz-k,l)
         U_sup     =  U_M      (i,j,mz-k,l)
         V_sup     =  V_M      (i,j,mz-k,l)
         T_sup     =  Temp     (i,j,mz-k,l)
         Vort_sup  =  Vort     (i,j,mz-k,l)
         MoCo_sup  =  MoistConv(i,j,mz-k,l)
         Th_e_sup  =  Th_e     (i,j,mz-k,l)
         RH_sup    =  RH       (i,j,mz-k,l)
       end if
     enddo
     if (pres_inf .eq. 0.) then
         pres_sup  =  pLev(iLev)
     endif

     !- vertical velocity interpolated betwwen two W-points
     W_inf  =  0.0
     do k=1,mz1
       !- pressure at W-point
       prWi = 0.5 * ( pr(i,j,k   ,l) + pr(i,j,MIN(mz1,k+1)   ,l) )
       prWs = 0.5 * ( pr(i,j,mz-k,l) + pr(i,j,MIN(mz1,mz-k+1),l) )
       if ( prWi .gt. pLev(iLev) ) then
         pres_infW =  prWi
         W_inf     =  W(i,j,k,l)
       end if
       if ( prWs .le. pLev(iLev) ) then
         pres_supW =  prWs 
         W_sup     =  W(i,j,mz-k,l)
       end if
     enddo
     if (pres_infW .eq. 0.) then
         pres_supW =  pLev(iLev)
     endif
  
     !- interpolation
     gz_p(i,j,l)  = (  gz_inf * (pres_sup-pLev(iLev))         &
     &                +gz_sup * (pLev(iLev)-pres_inf)       ) &
     &              / ( pres_sup-pres_inf )    

     U_p(i,j,l)   = (  U_inf * (pres_sup-pLev(iLev))          &
     &                +U_sup * (pLev(iLev)-pres_inf)        ) &
     &              / ( pres_sup-pres_inf )    
  
     V_p(i,j,l)   = (  V_inf * (pres_sup-pLev(iLev))          &
     &                +V_sup * (pLev(iLev)-pres_inf)        ) &
     &              / ( pres_sup-pres_inf )
  
     T_p(i,j,l)   = (  T_inf * (pres_sup-pLev(iLev))          &
     &                +T_sup * (pLev(iLev)-pres_inf)        ) &
     &              / ( pres_sup-pres_inf )
  
     Vort_p(i,j,l) = (  Vort_inf * (pres_sup-pLev(iLev))      &
     &                 +Vort_sup * (pLev(iLev)-pres_inf)    ) &
     &               / ( pres_sup-pres_inf )
  
     Th_e_p(i,j,l) = (  Th_e_inf * (pres_sup-pLev(iLev))      &
     &                 +Th_e_sup * (pLev(iLev)-pres_inf)    ) &
     &               / ( pres_sup-pres_inf )
  
     RH_p(i,j,l)   = (  RH_inf * (pres_sup-pLev(iLev))        &
     &                 +RH_sup * (pLev(iLev)-pres_inf)      ) &
     &               / ( pres_sup-pres_inf )
  
     MoCo_p(i,j,l) = (  MoCo_inf * (pres_sup-pLev(iLev))      &
     &                 +MoCo_sup * (pLev(iLev)-pres_inf)    ) &
     &               / ( pres_sup-pres_inf )  
  
     W_p(i,j,l)    = (  W_inf    * (pres_supW-pLev(iLev))     &
     &                 +W_sup    * (pLev(iLev)-pres_infW)   ) &
     &               / ( pres_supW-pres_infW )
 
   enddo
   enddo
  
   !---------------
   ! 8- pression reduite au niveau de la mer
   !
   !do i=1,mx1
   !do j=1,my1
   !!temperature virtuelle Tvm :
   !Tvm(i,j,l)=Temp(i,j,1,l)*(1+1.608*QVAPOR(i,j,1,l))/(1+QVAPOR(i,j,1,l))+0.65*HGT(i,j,l)/200.
   !!pression :
   !p_SL(i,j,l) = P1(i,j,l)*10**(HGT(i,j,l)/(67.445*Tvm(i,j,l)))
   !!precip :
   !enddo
   !enddo
  
  ENDDO !time
  
  !--------------------------------------------
  ! 3 - Ecriture du nouveau fichier
  !--------------------------------------------

  write(nam_gz,'(a,i3.3)')   'Zg_',   NINT(pLev(iLev))
  write(nam_U,'(a,i3.3)')    'U_',    NINT(pLev(iLev))
  write(nam_V,'(a,i3.3)')    'V_',    NINT(pLev(iLev))
  write(nam_T,'(a,i3.3)')    'T_',    NINT(pLev(iLev))
  write(nam_Vort,'(a,i3.3)') 'Vort_', NINT(pLev(iLev))
  write(nam_MoCo,'(a,i3.3)') 'MoCo_', NINT(pLev(iLev))
  write(nam_The,'(a,i3.3)')  'The_',  NINT(pLev(iLev))
  write(nam_RH,'(a,i3.3)')   'RH_',   NINT(pLev(iLev))
  write(nam_W,'(a,i3.3)')    'W_',    NINT(pLev(iLev))

  write(att_gz,'(a,i3.3,a)')   'Geopotential at ',                      NINT(pLev(iLev)), ' hPa'
  write(att_U,'(a,i3.3,a)')    'x-wind component at ',                  NINT(pLev(iLev)), ' hPa'
  write(att_V,'(a,i3.3,a)')    'y-wind component at ',                  NINT(pLev(iLev)), ' hPa'
  write(att_T,'(a,i3.3,a)')    'Potential Temperature at ',             NINT(pLev(iLev)), ' hPa'
  write(att_Vort,'(a,i3.3,a)') 'Vorticity at ',                         NINT(pLev(iLev)), ' hPa'
  write(att_MoCo,'(a,i3.3,a)') 'Moisture Convergence at ',              NINT(pLev(iLev)), ' hPa'
  write(att_The,'(a,i3.3,a)')  'Equivalent Potential Temperature at ',  NINT(pLev(iLev)), ' hPa'
  write(att_RH,'(a,i3.3,a)')   'Relative Humidity at ',                 NINT(pLev(iLev)), ' hPa'
  write(att_W,'(a,i3.3,a)')    'vertical wind velocity at ',            NINT(pLev(iLev)), ' hPa'

  !- Open this file associated with pressure level pLev(iLev) :
  write(file_out,102) an, dom, an, mois, jour, NINT(pLev(iLev))
  102 FORMAT('WORK_',i4.4,'/wrfout_d',i2.2,'_',i4.4,'-',i2.2,'-',i2.2,'_',i3.3,'.nc')
  write(*,*) 'Output file: ', TRIM(file_out)
  status = NF90_CREATE(TRIM(file_out),NF90_NOCLOBBER,fidM) ; call erreur(status,.TRUE.,"create")

   !- Define output file dimensions 
   status = NF90_DEF_DIM(fidM,"west_east",mx1,dimID_x)       ; call erreur(status,.TRUE.,"def_dimID_x")
   status = NF90_DEF_DIM(fidM,"south_north",my1,dimID_y)     ; call erreur(status,.TRUE.,"def_dimID_y")
   status = NF90_DEF_DIM(fidM,"Time",NF90_UNLIMITED,dimID_t) ; call erreur(status,.TRUE.,"def_dimID_t")
   status = NF90_DEF_DIM(fidM,"DateStrLen",mDSL,dimID_DSL)   ; call erreur(status,.TRUE.,"def_dimID_DSL")

   !- Definition of the variables  
   status = NF90_DEF_VAR(fidM,"Times",NF90_CHAR,(/dimID_DSL,dimID_t/),Times_ID)
   call erreur(status,.TRUE.,"def_var_Time_ID")
   status = NF90_DEF_VAR(fidM,"Time",NF90_FLOAT,(/dimID_t/),XTIME_ID)
   call erreur(status,.TRUE.,"def_var_XTIME_ID")
   status = NF90_DEF_VAR(fidM,"ITIMESTEP",NF90_INT,(/dimID_t/),ITIMESTEP_ID)
   call erreur(status,.TRUE.,"def_var_ITIMESTEP_ID")
   status = NF90_DEF_VAR(fidM,TRIM(nam_gz),NF90_FLOAT,(/dimID_x,dimID_y,dimID_t/),gz_p_ID)
   call erreur(status,.TRUE.,"def_var_gz_p_ID")
   status = NF90_DEF_VAR(fidM,TRIM(nam_U),NF90_FLOAT,(/dimID_x,dimID_y,dimID_t/),U_p_ID)
   call erreur(status,.TRUE.,"def_var_U_p_ID")
   status = NF90_DEF_VAR(fidM,TRIM(nam_V),NF90_FLOAT,(/dimID_x,dimID_y,dimID_t/),V_p_ID)
   call erreur(status,.TRUE.,"def_var_V_p_ID")
   status = NF90_DEF_VAR(fidM,TRIM(nam_W),NF90_FLOAT,(/dimID_x,dimID_y,dimID_t/),W_p_ID)
   call erreur(status,.TRUE.,"def_var_W_p_ID")
   status = NF90_DEF_VAR(fidM,TRIM(nam_T),NF90_FLOAT,(/dimID_x,dimID_y,dimID_t/),T_p_ID)
   call erreur(status,.TRUE.,"def_var_T_p_ID")
   status = NF90_DEF_VAR(fidM,TRIM(nam_Vort),NF90_FLOAT,(/dimID_x,dimID_y,dimID_t/),Vort_p_ID)
   call erreur(status,.TRUE.,"def_var_Vort_p_ID")
   status = NF90_DEF_VAR(fidM,TRIM(nam_MoCo),NF90_FLOAT,(/dimID_x,dimID_y,dimID_t/),MoCo_p_ID)
   call erreur(status,.TRUE.,"def_var_MoCo_p_ID")
   status = NF90_DEF_VAR(fidM,TRIM(nam_The),NF90_FLOAT,(/dimID_x,dimID_y,dimID_t/),The_p_ID)
   call erreur(status,.TRUE.,"def_var_The_p_ID")
   status = NF90_DEF_VAR(fidM,TRIM(nam_RH),NF90_FLOAT,(/dimID_x,dimID_y,dimID_t/),RH_p_ID)
   call erreur(status,.TRUE.,"def_var_RH_p_ID")

   !- Attributes
   status = NF90_PUT_ATT(fidM,XTIME_ID,"long_name","Time")            ; call erreur(status,.TRUE.,"put_att_T_ID")
   status = NF90_PUT_ATT(fidM,gz_p_ID,"description",TRIM(att_gz))     ; call erreur(status,.TRUE.,"put_att_gz_p_ID")
   status = NF90_PUT_ATT(fidM,U_p_ID,"description",TRIM(att_U))       ; call erreur(status,.TRUE.,"put_att_U_p_ID")
   status = NF90_PUT_ATT(fidM,V_p_ID,"description",TRIM(att_V))       ; call erreur(status,.TRUE.,"put_att_V_p_ID")
   status = NF90_PUT_ATT(fidM,W_p_ID,"description",TRIM(att_W))       ; call erreur(status,.TRUE.,"put_att_W_p_ID")
   status = NF90_PUT_ATT(fidM,T_p_ID,"description",TRIM(att_T))       ; call erreur(status,.TRUE.,"put_att_T_p_ID")
   status = NF90_PUT_ATT(fidM,Vort_p_ID,"description",TRIM(att_Vort)) ; call erreur(status,.TRUE.,"def_Vort_p_ID")
   status = NF90_PUT_ATT(fidM,MoCo_p_ID,"description",TRIM(att_MoCo)) ; call erreur(status,.TRUE.,"def_MoCo_p_ID")
   status = NF90_PUT_ATT(fidM,The_p_ID,"description",TRIM(att_The))   ; call erreur(status,.TRUE.,"put_att_The_p_ID")
   status = NF90_PUT_ATT(fidM,RH_p_ID,"description",TRIM(att_RH))     ; call erreur(status,.TRUE.,"put_att_RH_p_ID")

   status = NF90_PUT_ATT(fidM,gz_p_ID,"MemoryOrder","XY")   ; call erreur(status,.TRUE.,"put_att_MeM_Or_ID") 
   status = NF90_PUT_ATT(fidM,U_p_ID,"MemoryOrder","XY")    ; call erreur(status,.TRUE.,"put_att_MeM_Or_ID") 
   status = NF90_PUT_ATT(fidM,V_p_ID,"MemoryOrder","XY")    ; call erreur(status,.TRUE.,"put_att_MeM_Or_ID") 
   status = NF90_PUT_ATT(fidM,W_p_ID,"MemoryOrder","XY")    ; call erreur(status,.TRUE.,"put_att_MeM_Or_ID")
   status = NF90_PUT_ATT(fidM,T_p_ID,"MemoryOrder","XY")    ; call erreur(status,.TRUE.,"put_att_MeM_Or_ID") 
   status = NF90_PUT_ATT(fidM,Vort_p_ID,"MemoryOrder","XY") ; call erreur(status,.TRUE.,"put_att_MeM_Or_ID") 
   status = NF90_PUT_ATT(fidM,MoCo_p_ID,"MemoryOrder","XY") ; call erreur(status,.TRUE.,"put_att_MeM_Or_ID") 
   status = NF90_PUT_ATT(fidM,The_p_ID,"MemoryOrder","XY")  ; call erreur(status,.TRUE.,"put_att_MeM_Or_ID") 
   status = NF90_PUT_ATT(fidM,RH_p_ID,"MemoryOrder","XY")   ; call erreur(status,.TRUE.,"put_att_MeM_Or_ID") 

   status = NF90_PUT_ATT(fidM,gz_p_ID,"units","m^2.s^-2")         ; call erreur(status,.TRUE.,"put_att_gz_ID")
   status = NF90_PUT_ATT(fidM,U_p_ID,"units","m.s^-1")            ; call erreur(status,.TRUE.,"put_att_U_ID")
   status = NF90_PUT_ATT(fidM,V_p_ID,"units","m.s^-1")            ; call erreur(status,.TRUE.,"put_att_V_ID")
   status = NF90_PUT_ATT(fidM,W_p_ID,"units","m.s^-1")            ; call erreur(status,.TRUE.,"put_att_W_ID")
   status = NF90_PUT_ATT(fidM,T_p_ID,"units","K")                 ; call erreur(status,.TRUE.,"put_att_T_ID")
   status = NF90_PUT_ATT(fidM,Vort_p_ID,"units","s^-1")           ; call erreur(status,.TRUE.,"def_Vort_ID")
   status = NF90_PUT_ATT(fidM,MoCo_p_ID,"units","g.kg^-1.day^-1") ; call erreur(status,.TRUE.,"def_MoCo_ID")
   status = NF90_PUT_ATT(fidM,The_p_ID,"units","K")               ; call erreur(status,.TRUE.,"put_att_The_ID")
   status = NF90_PUT_ATT(fidM,RH_p_ID,"units","-")                ; call erreur(status,.TRUE.,"put_att_RH_ID")

   !NB : ces attributs sont indispensables pour que ferret mette les dates
   status = NF90_PUT_ATT(fidM,XTIME_ID,"units","minutes since 1989-01-01 00:00 UTC")
   call erreur(status,.TRUE.,"put_att_XTIME_ID")
   status = NF90_PUT_ATT(fidM,XTIME_ID,"calendar","Gregorian")
   call erreur(status,.TRUE.,"put_att_XTIME_ID")

   !-
   status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Created using postprocess_wrf_pLev.f90")
   call erreur(status,.TRUE.,"put_att_GLOBAL")

   !- End definitions
   status = NF90_ENDDEF(fidM) ; call erreur(status,.TRUE.,"fin_definition")

   !- Values taken by variables :
   status = NF90_PUT_VAR(fidM,Times_ID,Times)         ; call erreur(status,.TRUE.,"var_Times_ID")
   status = NF90_PUT_VAR(fidM,XTIME_ID,XTIME)         ; call erreur(status,.TRUE.,"var_XTIME_ID")
   status = NF90_PUT_VAR(fidM,ITIMESTEP_ID,ITIMESTEP) ; call erreur(status,.TRUE.,"var_ITIMESTEP_ID")
   status = NF90_PUT_VAR(fidM,gz_p_ID,gz_p)           ; call erreur(status,.TRUE.,"var_gz_p_ID")  
   status = NF90_PUT_VAR(fidM,U_p_ID,U_p)             ; call erreur(status,.TRUE.,"var_U_p_ID")  
   status = NF90_PUT_VAR(fidM,V_p_ID,V_p)             ; call erreur(status,.TRUE.,"var_V_p_ID")
   status = NF90_PUT_VAR(fidM,W_p_ID,W_p)             ; call erreur(status,.TRUE.,"var_W_p_ID")
   status = NF90_PUT_VAR(fidM,T_p_ID,T_p)             ; call erreur(status,.TRUE.,"var_T_p_ID")
   status = NF90_PUT_VAR(fidM,Vort_p_ID,Vort_p)       ; call erreur(status,.TRUE.,"def_Vort_p_ID")
   status = NF90_PUT_VAR(fidM,MoCo_p_ID,MoCo_p)       ; call erreur(status,.TRUE.,"def_MoCo_p_ID")
   status = NF90_PUT_VAR(fidM,The_p_ID,Th_e_p)        ; call erreur(status,.TRUE.,"var_The_p_ID")
   status = NF90_PUT_VAR(fidM,RH_p_ID,RH_p)           ; call erreur(status,.TRUE.,"var_RH_p_ID")

  !- End write
  status = NF90_CLOSE(fidM) ;  call erreur(status,.TRUE.,"final")

ENDDO ! loop on iLev

end program postprocess



SUBROUTINE erreur(iret, lstop, chaine)
  ! pour les messages d'erreur
  USE netcdf
  INTEGER, INTENT(in)                     :: iret
  LOGICAL, INTENT(in)                     :: lstop
  CHARACTER(LEN=*), INTENT(in)            :: chaine
  !
  CHARACTER(LEN=80)                       :: message
  !
  IF ( iret .NE. 0 ) THEN
    WRITE(*,*) 'ROUTINE: ', TRIM(chaine)
    WRITE(*,*) 'ERREUR: ', iret
    message=NF90_STRERROR(iret)
    WRITE(*,*) 'CA VEUT DIRE:',TRIM(message)
    IF ( lstop ) STOP
  ENDIF
  !
END SUBROUTINE erreur

