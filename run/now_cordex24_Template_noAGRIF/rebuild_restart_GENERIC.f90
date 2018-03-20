program modif                                         
                                                      
USE netcdf                                            
                                                      
IMPLICIT NONE                                         
                                                      
INTEGER                               :: fidA, status, dimID_t, dimID_z, dimID_y, dimID_x, mt, mz, rhop_ID, sshn_ID, hdivn_ID, rotn_ID, sn_ID,         &
&                                        tn_ID, vn_ID, un_ID, sshb_ID, hdivb_ID, rotb_ID, sb_ID, tb_ID, vb_ID, ub_ID, rdttra1_ID, rdt_ID, gcxb_ID,     &
&                                        gcx_ID, qsr_hc_b_ID, sbc_sc_b_ID, sbc_hc_b_ID, dissl_ID, avmv_ID, avmu_ID, avm_ID, avt_ID, en_ID, emps_b_ID,  &
&                                        emp_b_ID, qns_b_ID, vtau_b_ID, utau_b_ID, rnf_sc_b_ID, rnf_hc_b_ID, rnf_b_ID, adatrj_ID, ndastp_ID, kt_ID,    &
&                                        time_ID, nav_lev_ID, nav_lat_ID, nav_lon_ID, fidM
CHARACTER(LEN=150)                    :: file_in, file_out
INTEGER                               :: mxloc, myloc, xi, xf, yi, yf            
REAL*4,ALLOCATABLE,DIMENSION(:)       :: nav_lev           
REAL*8,ALLOCATABLE,DIMENSION(:)       :: time_counter          
REAL*4,ALLOCATABLE,DIMENSION(:,:)     :: nav_lat, nav_lon         
REAL*8,ALLOCATABLE,DIMENSION(:,:,:)   :: sshn, sshb, gcxb, gcx, sbc_sc_b, sbc_hc_b, emps_b, emp_b, qns_b, vtau_b, utau_b, rnf_sc_b, rnf_hc_b, rnf_b      
REAL*8,ALLOCATABLE,DIMENSION(:,:,:,:) :: rhop, hdivn, rotn, sn, tn, vn, un, hdivb, rotb, sb, tb, vb, ub, qsr_hc_b, dissl, avmv, avmu, avm, avt, en    
REAL*8                                :: rdttra1, rdt, kt, adatrj, ndastp                                                  
REAL*4,ALLOCATABLE,DIMENSION(:,:)     :: GLO_nav_lat, GLO_nav_lon
REAL*8,ALLOCATABLE,DIMENSION(:,:,:)   :: GLO_sshn, GLO_sshb, GLO_gcxb, GLO_gcx, GLO_sbc_sc_b, GLO_sbc_hc_b, GLO_emps_b, GLO_emp_b, GLO_qns_b,          &
&                                        GLO_vtau_b, GLO_utau_b, GLO_rnf_sc_b, GLO_rnf_hc_b, GLO_rnf_b
REAL*8,ALLOCATABLE,DIMENSION(:,:,:,:) :: GLO_rhop, GLO_hdivn, GLO_rotn, GLO_sn, GLO_tn, GLO_vn, GLO_un, GLO_hdivb, GLO_rotb, GLO_sb, GLO_tb, GLO_vb,   &
&                                        GLO_ub, GLO_qsr_hc_b, GLO_dissl, GLO_avmv, GLO_avmu, GLO_avm, GLO_avt, GLO_en
CHARACTER(LEN=10)                     :: a, b, c, d, e, f, g, h, i
INTEGER                               :: kdom, jpnij, jpi, jpj, jpk, jpiglo, jpjglo, jpkglo, jpni, jpnj
INTEGER                               :: NAREA, nlci, nlcj, nldi, nldj, nlei, nlej, nimpp, njmpp, step

!-------------------------------------------------------------------------
! Read restart time step

open(unit=11,file='zzzzrestart_nit.txt',status='old')
read(11,*) step
close(11)
write(*,343) step
343 FORMAT('Rebuilding restart file for time step ',i8)

write(file_out,100) step
100 FORMAT('zzzzrestart_',i8.8,'.nc')

!-------------------------------------------------------------------------
! Read the characteristics of subdomains in zzzzlayout.dat

open(unit=12,file='zzzzlayout.dat',status='old')

read(12,*) a, b, c, d, e, f
read(12,*) jpnij, jpi, jpj, jpk, jpiglo, jpjglo
read(12,*) a, b, c, d, e, f, g, h, i

!- find number of subdomain along x and y:
jpni=CEILING(FLOAT(jpiglo)/FLOAT(jpi))
jpnj=CEILING(FLOAT(jpjglo)/FLOAT(jpj))
if ( jpni*jpnj .ne. jpnij ) then
  jpni=jpni+1
endif
if ( jpni*jpnj .ne. jpnij ) then
  jpni=jpni-1  ! back to normal
  jpnj=jpnj+1
endif
if ( jpni*jpnj .ne. jpnij ) then
  jpnj=jpnj+1  ! +2 from normal
endif
if ( jpni*jpnj .ne. jpnij ) then
  jpnj=jpnj+1  ! +3 from normal
endif
if ( jpni*jpnj .ne. jpnij ) then
  jpnj=jpnj+1  ! +4 from normal
endif
if ( jpni*jpnj .ne. jpnij ) then
  write(*,*) 'WRONG DEFINITION OF jpni AND jpnj >>>>>>>>>> stop !!!!'
  write(*,*) jpni, jpnj, jpnij
  stop
endif

jpkglo=jpk

ALLOCATE(  GLO_rhop    (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_sshn    (jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_hdivn   (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_rotn    (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_sn      (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_tn      (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_vn      (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_un      (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_sshb    (jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_hdivb   (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_rotb    (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_sb      (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_tb      (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_vb      (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_ub      (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_gcxb    (jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_gcx     (jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_qsr_hc_b(jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_sbc_sc_b(jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_sbc_hc_b(jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_dissl   (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_avmv    (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_avmu    (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_avm     (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_avt     (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_en      (jpiglo,jpjglo,jpkglo, 1)  )
ALLOCATE(  GLO_emps_b  (jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_emp_b   (jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_qns_b   (jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_vtau_b  (jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_utau_b  (jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_rnf_sc_b(jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_rnf_hc_b(jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_rnf_b   (jpiglo,jpjglo,        1)  )
ALLOCATE(  GLO_nav_lat (jpiglo,jpjglo          )  )
ALLOCATE(  GLO_nav_lon (jpiglo,jpjglo          )  )

do kdom=1,jpnij

  read(12,*) NAREA, nlci, nlcj, nldi, nldj, nlei, nlej, nimpp, njmpp 

  ! number of grid points on local domain:
  mxloc = nlei-nldi+1
  myloc = nlej-nldj+1

  ! corresponding indices on global grid:
  xi = nimpp+nldi-1
  xf = xi+mxloc-1    ! prev = nimpp+nlei-1
  yi = njmpp+nldj-1
  yf = yi+myloc-1    ! prev = njmpp+nlej-1

  write(*,601) NAREA-1,  1, nlei-nldi+1, xi, xf
  write(*,602) NAREA-1,  1, nlej-nldj+1, yi, yf

  write(*,*) '-----'

  write(file_in,101) step, kdom-1
  write(*,*) 'Reading ', TRIM(file_in)
             
  status = NF90_OPEN(TRIM(file_in),0,fidA) ;  call erreur(status,.TRUE.,"open_netcdf") 
                                                           
  status = NF90_INQ_DIMID(fidA,"t",dimID_t) ; call erreur(status,.TRUE.,"inq_dimID_t")
  status = NF90_INQ_DIMID(fidA,"z",dimID_z) ; call erreur(status,.TRUE.,"inq_dimID_z")
  status = NF90_INQ_DIMID(fidA,"y",dimID_y) ; call erreur(status,.TRUE.,"inq_dimID_y")
  status = NF90_INQ_DIMID(fidA,"x",dimID_x) ; call erreur(status,.TRUE.,"inq_dimID_x")
                                                               
  status = NF90_INQUIRE_DIMENSION(fidA,dimID_t,len=mt)            ; call erreur(status,.TRUE.,"inq_dim_t")
  status = NF90_INQUIRE_DIMENSION(fidA,dimID_z,len=mz)            ; call erreur(status,.TRUE.,"inq_dim_z")
  status = NF90_INQUIRE_DIMENSION(fidA,dimID_y,len=myloc) ; call erreur(status,.TRUE.,"inq_dim_y")
  status = NF90_INQUIRE_DIMENSION(fidA,dimID_x,len=mxloc) ; call erreur(status,.TRUE.,"inq_dim_x")

  if ( mt .ne. 1 .or. mz .ne. jpkglo ) then
    write(*,*) '~!@#$%^* time or vertical coordinates is not correct >>>>>>>>>> stop !!'
    stop
  endif
                               
  ALLOCATE(  rhop    (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  sshn    (mxloc,myloc,   mt)  ) 
  ALLOCATE(  hdivn   (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  rotn    (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  sn      (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  tn      (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  vn      (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  un      (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  sshb    (mxloc,myloc,   mt)  ) 
  ALLOCATE(  hdivb   (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  rotb    (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  sb      (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  tb      (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  vb      (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  ub      (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  gcxb    (mxloc,myloc,   mt)  ) 
  ALLOCATE(  gcx     (mxloc,myloc,   mt)  ) 
  ALLOCATE(  qsr_hc_b(mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  sbc_sc_b(mxloc,myloc,   mt)  ) 
  ALLOCATE(  sbc_hc_b(mxloc,myloc,   mt)  ) 
  ALLOCATE(  dissl   (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  avmv    (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  avmu    (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  avm     (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  avt     (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  en      (mxloc,myloc,mz,mt)  ) 
  ALLOCATE(  emps_b  (mxloc,myloc,   mt)  ) 
  ALLOCATE(  emp_b   (mxloc,myloc,   mt)  ) 
  ALLOCATE(  qns_b   (mxloc,myloc,   mt)  ) 
  ALLOCATE(  vtau_b  (mxloc,myloc,   mt)  ) 
  ALLOCATE(  utau_b  (mxloc,myloc,   mt)  ) 
  ALLOCATE(  rnf_sc_b(mxloc,myloc,   mt)  ) 
  ALLOCATE(  rnf_hc_b(mxloc,myloc,   mt)  ) 
  ALLOCATE(  rnf_b   (mxloc,myloc,   mt)  ) 
  ALLOCATE(  nav_lat (mxloc,myloc      )  ) 
  ALLOCATE(  nav_lon (mxloc,myloc      )  ) 
  if ( kdom .eq. 1 )  ALLOCATE(  nav_lev(mz), time_counter(mt) )  
                               
  status = NF90_INQ_VARID(fidA,"rhop",rhop_ID)         ; call erreur(status,.TRUE.,"inq_rhop_ID")
  status = NF90_INQ_VARID(fidA,"sshn",sshn_ID)         ; call erreur(status,.TRUE.,"inq_sshn_ID")
  status = NF90_INQ_VARID(fidA,"hdivn",hdivn_ID)       ; call erreur(status,.TRUE.,"inq_hdivn_ID")
  status = NF90_INQ_VARID(fidA,"rotn",rotn_ID)         ; call erreur(status,.TRUE.,"inq_rotn_ID")
  status = NF90_INQ_VARID(fidA,"sn",sn_ID)             ; call erreur(status,.TRUE.,"inq_sn_ID")
  status = NF90_INQ_VARID(fidA,"tn",tn_ID)             ; call erreur(status,.TRUE.,"inq_tn_ID")
  status = NF90_INQ_VARID(fidA,"vn",vn_ID)             ; call erreur(status,.TRUE.,"inq_vn_ID")
  status = NF90_INQ_VARID(fidA,"un",un_ID)             ; call erreur(status,.TRUE.,"inq_un_ID")
  status = NF90_INQ_VARID(fidA,"sshb",sshb_ID)         ; call erreur(status,.TRUE.,"inq_sshb_ID")
  status = NF90_INQ_VARID(fidA,"hdivb",hdivb_ID)       ; call erreur(status,.TRUE.,"inq_hdivb_ID")
  status = NF90_INQ_VARID(fidA,"rotb",rotb_ID)         ; call erreur(status,.TRUE.,"inq_rotb_ID")
  status = NF90_INQ_VARID(fidA,"sb",sb_ID)             ; call erreur(status,.TRUE.,"inq_sb_ID")
  status = NF90_INQ_VARID(fidA,"tb",tb_ID)             ; call erreur(status,.TRUE.,"inq_tb_ID")
  status = NF90_INQ_VARID(fidA,"vb",vb_ID)             ; call erreur(status,.TRUE.,"inq_vb_ID")
  status = NF90_INQ_VARID(fidA,"ub",ub_ID)             ; call erreur(status,.TRUE.,"inq_ub_ID")
  status = NF90_INQ_VARID(fidA,"rdttra1",rdttra1_ID)   ; call erreur(status,.TRUE.,"inq_rdttra1_ID")
  status = NF90_INQ_VARID(fidA,"rdt",rdt_ID)           ; call erreur(status,.TRUE.,"inq_rdt_ID")
  status = NF90_INQ_VARID(fidA,"gcxb",gcxb_ID)         ; call erreur(status,.TRUE.,"inq_gcxb_ID")
  status = NF90_INQ_VARID(fidA,"gcx",gcx_ID)           ; call erreur(status,.TRUE.,"inq_gcx_ID")
  status = NF90_INQ_VARID(fidA,"qsr_hc_b",qsr_hc_b_ID) ; call erreur(status,.TRUE.,"inq_qsr_hc_b_ID")
  status = NF90_INQ_VARID(fidA,"sbc_sc_b",sbc_sc_b_ID) ; call erreur(status,.TRUE.,"inq_sbc_sc_b_ID")
  status = NF90_INQ_VARID(fidA,"sbc_hc_b",sbc_hc_b_ID) ; call erreur(status,.TRUE.,"inq_sbc_hc_b_ID")
  status = NF90_INQ_VARID(fidA,"dissl",dissl_ID)       ; call erreur(status,.TRUE.,"inq_dissl_ID")
  status = NF90_INQ_VARID(fidA,"avmv",avmv_ID)         ; call erreur(status,.TRUE.,"inq_avmv_ID")
  status = NF90_INQ_VARID(fidA,"avmu",avmu_ID)         ; call erreur(status,.TRUE.,"inq_avmu_ID")
  status = NF90_INQ_VARID(fidA,"avm",avm_ID)           ; call erreur(status,.TRUE.,"inq_avm_ID")
  status = NF90_INQ_VARID(fidA,"avt",avt_ID)           ; call erreur(status,.TRUE.,"inq_avt_ID")
  status = NF90_INQ_VARID(fidA,"en",en_ID)             ; call erreur(status,.TRUE.,"inq_en_ID")
  status = NF90_INQ_VARID(fidA,"emps_b",emps_b_ID)     ; call erreur(status,.TRUE.,"inq_emps_b_ID")
  status = NF90_INQ_VARID(fidA,"emp_b",emp_b_ID)       ; call erreur(status,.TRUE.,"inq_emp_b_ID")
  status = NF90_INQ_VARID(fidA,"qns_b",qns_b_ID)       ; call erreur(status,.TRUE.,"inq_qns_b_ID")
  status = NF90_INQ_VARID(fidA,"vtau_b",vtau_b_ID)     ; call erreur(status,.TRUE.,"inq_vtau_b_ID")
  status = NF90_INQ_VARID(fidA,"utau_b",utau_b_ID)     ; call erreur(status,.TRUE.,"inq_utau_b_ID")
  status = NF90_INQ_VARID(fidA,"rnf_sc_b",rnf_sc_b_ID) ; call erreur(status,.TRUE.,"inq_rnf_sc_b_ID")
  status = NF90_INQ_VARID(fidA,"rnf_hc_b",rnf_hc_b_ID) ; call erreur(status,.TRUE.,"inq_rnf_hc_b_ID")
  status = NF90_INQ_VARID(fidA,"rnf_b",rnf_b_ID)       ; call erreur(status,.TRUE.,"inq_rnf_b_ID")
  status = NF90_INQ_VARID(fidA,"adatrj",adatrj_ID)     ; call erreur(status,.TRUE.,"inq_adatrj_ID")
  status = NF90_INQ_VARID(fidA,"ndastp",ndastp_ID)     ; call erreur(status,.TRUE.,"inq_ndastp_ID")
  status = NF90_INQ_VARID(fidA,"kt",kt_ID)             ; call erreur(status,.TRUE.,"inq_kt_ID")
  status = NF90_INQ_VARID(fidA,"time_counter",time_ID) ; call erreur(status,.TRUE.,"inq_time_ID")
  status = NF90_INQ_VARID(fidA,"nav_lev",nav_lev_ID)   ; call erreur(status,.TRUE.,"inq_nav_lev_ID")
  status = NF90_INQ_VARID(fidA,"nav_lat",nav_lat_ID)   ; call erreur(status,.TRUE.,"inq_nav_lat_ID")
  status = NF90_INQ_VARID(fidA,"nav_lon",nav_lon_ID)   ; call erreur(status,.TRUE.,"inq_nav_lon_ID")
                                                       
  status = NF90_GET_VAR(fidA,rhop_ID,rhop)             ; call erreur(status,.TRUE.,"getvar_rhop")
  status = NF90_GET_VAR(fidA,sshn_ID,sshn)             ; call erreur(status,.TRUE.,"getvar_sshn")
  status = NF90_GET_VAR(fidA,hdivn_ID,hdivn)           ; call erreur(status,.TRUE.,"getvar_hdivn")
  status = NF90_GET_VAR(fidA,rotn_ID,rotn)             ; call erreur(status,.TRUE.,"getvar_rotn")
  status = NF90_GET_VAR(fidA,sn_ID,sn)                 ; call erreur(status,.TRUE.,"getvar_sn")
  status = NF90_GET_VAR(fidA,tn_ID,tn)                 ; call erreur(status,.TRUE.,"getvar_tn")
  status = NF90_GET_VAR(fidA,vn_ID,vn)                 ; call erreur(status,.TRUE.,"getvar_vn")
  status = NF90_GET_VAR(fidA,un_ID,un)                 ; call erreur(status,.TRUE.,"getvar_un")
  status = NF90_GET_VAR(fidA,sshb_ID,sshb)             ; call erreur(status,.TRUE.,"getvar_sshb")
  status = NF90_GET_VAR(fidA,hdivb_ID,hdivb)           ; call erreur(status,.TRUE.,"getvar_hdivb")
  status = NF90_GET_VAR(fidA,rotb_ID,rotb)             ; call erreur(status,.TRUE.,"getvar_rotb")
  status = NF90_GET_VAR(fidA,sb_ID,sb)                 ; call erreur(status,.TRUE.,"getvar_sb")
  status = NF90_GET_VAR(fidA,tb_ID,tb)                 ; call erreur(status,.TRUE.,"getvar_tb")
  status = NF90_GET_VAR(fidA,vb_ID,vb)                 ; call erreur(status,.TRUE.,"getvar_vb")
  status = NF90_GET_VAR(fidA,ub_ID,ub)                 ; call erreur(status,.TRUE.,"getvar_ub")
  status = NF90_GET_VAR(fidA,rdttra1_ID,rdttra1)       ; call erreur(status,.TRUE.,"getvar_rdttra1")
  status = NF90_GET_VAR(fidA,rdt_ID,rdt)               ; call erreur(status,.TRUE.,"getvar_rdt")
  status = NF90_GET_VAR(fidA,gcxb_ID,gcxb)             ; call erreur(status,.TRUE.,"getvar_gcxb")
  status = NF90_GET_VAR(fidA,gcx_ID,gcx)               ; call erreur(status,.TRUE.,"getvar_gcx")
  status = NF90_GET_VAR(fidA,qsr_hc_b_ID,qsr_hc_b)     ; call erreur(status,.TRUE.,"getvar_qsr_hc_b")
  status = NF90_GET_VAR(fidA,sbc_sc_b_ID,sbc_sc_b)     ; call erreur(status,.TRUE.,"getvar_sbc_sc_b")
  status = NF90_GET_VAR(fidA,sbc_hc_b_ID,sbc_hc_b)     ; call erreur(status,.TRUE.,"getvar_sbc_hc_b")
  status = NF90_GET_VAR(fidA,dissl_ID,dissl)           ; call erreur(status,.TRUE.,"getvar_dissl")
  status = NF90_GET_VAR(fidA,avmv_ID,avmv)             ; call erreur(status,.TRUE.,"getvar_avmv")
  status = NF90_GET_VAR(fidA,avmu_ID,avmu)             ; call erreur(status,.TRUE.,"getvar_avmu")
  status = NF90_GET_VAR(fidA,avm_ID,avm)               ; call erreur(status,.TRUE.,"getvar_avm")
  status = NF90_GET_VAR(fidA,avt_ID,avt)               ; call erreur(status,.TRUE.,"getvar_avt")
  status = NF90_GET_VAR(fidA,en_ID,en)                 ; call erreur(status,.TRUE.,"getvar_en")
  status = NF90_GET_VAR(fidA,emps_b_ID,emps_b)         ; call erreur(status,.TRUE.,"getvar_emps_b")
  status = NF90_GET_VAR(fidA,emp_b_ID,emp_b)           ; call erreur(status,.TRUE.,"getvar_emp_b")
  status = NF90_GET_VAR(fidA,qns_b_ID,qns_b)           ; call erreur(status,.TRUE.,"getvar_qns_b")
  status = NF90_GET_VAR(fidA,vtau_b_ID,vtau_b)         ; call erreur(status,.TRUE.,"getvar_vtau_b")
  status = NF90_GET_VAR(fidA,utau_b_ID,utau_b)         ; call erreur(status,.TRUE.,"getvar_utau_b")
  status = NF90_GET_VAR(fidA,rnf_sc_b_ID,rnf_sc_b)     ; call erreur(status,.TRUE.,"getvar_rnf_sc_b")
  status = NF90_GET_VAR(fidA,rnf_hc_b_ID,rnf_hc_b)     ; call erreur(status,.TRUE.,"getvar_rnf_hc_b")
  status = NF90_GET_VAR(fidA,rnf_b_ID,rnf_b)           ; call erreur(status,.TRUE.,"getvar_rnf_b")
  status = NF90_GET_VAR(fidA,adatrj_ID,adatrj)         ; call erreur(status,.TRUE.,"getvar_adatrj")
  status = NF90_GET_VAR(fidA,ndastp_ID,ndastp)         ; call erreur(status,.TRUE.,"getvar_ndastp")
  status = NF90_GET_VAR(fidA,kt_ID,kt)                 ; call erreur(status,.TRUE.,"getvar_kt")
  status = NF90_GET_VAR(fidA,time_ID,time_counter)     ; call erreur(status,.TRUE.,"getvar_time_counter")
  status = NF90_GET_VAR(fidA,nav_lev_ID,nav_lev)       ; call erreur(status,.TRUE.,"getvar_nav_lev")
  status = NF90_GET_VAR(fidA,nav_lat_ID,nav_lat)       ; call erreur(status,.TRUE.,"getvar_nav_lat")
  status = NF90_GET_VAR(fidA,nav_lon_ID,nav_lon)       ; call erreur(status,.TRUE.,"getvar_nav_lon")
                                                      
  status = NF90_CLOSE(fidA) ; call erreur(status,.TRUE.,"end_reading_netcdf")     
                                                              
  !---------------------------------------                      
  ! Fill global fields:                            
  
  GLO_rhop    ( xi:xf, yi:yf, :, :) = rhop    ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_sshn    ( xi:xf, yi:yf,    :) = sshn    ( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_hdivn   ( xi:xf, yi:yf, :, :) = hdivn   ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_rotn    ( xi:xf, yi:yf, :, :) = rotn    ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_sn      ( xi:xf, yi:yf, :, :) = sn      ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_tn      ( xi:xf, yi:yf, :, :) = tn      ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_vn      ( xi:xf, yi:yf, :, :) = vn      ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_un      ( xi:xf, yi:yf, :, :) = un      ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_sshb    ( xi:xf, yi:yf,    :) = sshb    ( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_hdivb   ( xi:xf, yi:yf, :, :) = hdivb   ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_rotb    ( xi:xf, yi:yf, :, :) = rotb    ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_sb      ( xi:xf, yi:yf, :, :) = sb      ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_tb      ( xi:xf, yi:yf, :, :) = tb      ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_vb      ( xi:xf, yi:yf, :, :) = vb      ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_ub      ( xi:xf, yi:yf, :, :) = ub      ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_gcxb    ( xi:xf, yi:yf,    :) = gcxb    ( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_gcx     ( xi:xf, yi:yf,    :) = gcx     ( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_qsr_hc_b( xi:xf, yi:yf, :, :) = qsr_hc_b( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_sbc_sc_b( xi:xf, yi:yf,    :) = sbc_sc_b( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_sbc_hc_b( xi:xf, yi:yf,    :) = sbc_hc_b( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_dissl   ( xi:xf, yi:yf, :, :) = dissl   ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_avmv    ( xi:xf, yi:yf, :, :) = avmv    ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_avmu    ( xi:xf, yi:yf, :, :) = avmu    ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_avm     ( xi:xf, yi:yf, :, :) = avm     ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_avt     ( xi:xf, yi:yf, :, :) = avt     ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_en      ( xi:xf, yi:yf, :, :) = en      ( 1:nlei-nldi+1 , 1:nlej-nldj+1, :, :)  
  GLO_emps_b  ( xi:xf, yi:yf,    :) = emps_b  ( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_emp_b   ( xi:xf, yi:yf,    :) = emp_b   ( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_qns_b   ( xi:xf, yi:yf,    :) = qns_b   ( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_vtau_b  ( xi:xf, yi:yf,    :) = vtau_b  ( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_utau_b  ( xi:xf, yi:yf,    :) = utau_b  ( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_rnf_sc_b( xi:xf, yi:yf,    :) = rnf_sc_b( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_rnf_hc_b( xi:xf, yi:yf,    :) = rnf_hc_b( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_rnf_b   ( xi:xf, yi:yf,    :) = rnf_b   ( 1:nlei-nldi+1 , 1:nlej-nldj+1,    :)  
  GLO_nav_lat ( xi:xf, yi:yf      ) = nav_lat ( 1:nlei-nldi+1 , 1:nlej-nldj+1      )  
  GLO_nav_lon ( xi:xf, yi:yf      ) = nav_lon ( 1:nlei-nldi+1 , 1:nlej-nldj+1      )  

  DEALLOCATE( rhop, sshn, hdivn, rotn, sn, tn, vn, un, sshb, hdivb, rotb, sb, tb, vb, ub )
  DEALLOCATE( gcxb, gcx, qsr_hc_b, sbc_sc_b, sbc_hc_b, dissl, avmv, avmu, avm, avt, en   )
  DEALLOCATE( emps_b, emp_b, qns_b, vtau_b, utau_b, rnf_sc_b, rnf_hc_b, rnf_b            )
  DEALLOCATE( nav_lat, nav_lon      )

enddo !-- kdom
  
101 FORMAT('zzzzCCCC-OOOO_',i8.8,'_restart_',i4.4,'.nc')
601 FORMAT(i4.4,'  ilocal: ',i3.3,':',i3.3,'  ->  iglobal : ',i3.3,':',i3.3)
602 FORMAT(i4.4,'  jlocal: ',i3.3,':',i3.3,'  ->  jglobal : ',i3.3,':',i3.3)

                                    
!---------------------------------------                      
! Write full (rebuilt) restart file :                                   

write(*,*) 'Rebuilding ', TRIM(file_out)
                                                              
status = NF90_CREATE(TRIM(file_out),or(NF90_NOCLOBBER,NF90_64BIT_OFFSET),fidM) ; call erreur(status,.TRUE.,'create_restart')                     
                                                                
status = NF90_DEF_DIM(fidM,"t",NF90_UNLIMITED,dimID_t) ; call erreur(status,.TRUE.,"def_dimID_t")
status = NF90_DEF_DIM(fidM,"z",mz,dimID_z)             ; call erreur(status,.TRUE.,"def_dimID_z")
status = NF90_DEF_DIM(fidM,"y",jpjglo,dimID_y)         ; call erreur(status,.TRUE.,"def_dimID_y")
status = NF90_DEF_DIM(fidM,"x",jpiglo,dimID_x)         ; call erreur(status,.TRUE.,"def_dimID_x")
                                                              
status = NF90_DEF_VAR(fidM,"rhop",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),rhop_ID)
call erreur(status,.TRUE.,"def_var_rhop_ID")
status = NF90_DEF_VAR(fidM,"sshn",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),sshn_ID)
call erreur(status,.TRUE.,"def_var_sshn_ID")
status = NF90_DEF_VAR(fidM,"hdivn",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),hdivn_ID)
call erreur(status,.TRUE.,"def_var_hdivn_ID")
status = NF90_DEF_VAR(fidM,"rotn",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),rotn_ID)
call erreur(status,.TRUE.,"def_var_rotn_ID")
status = NF90_DEF_VAR(fidM,"sn",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),sn_ID)
call erreur(status,.TRUE.,"def_var_sn_ID")
status = NF90_DEF_VAR(fidM,"tn",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),tn_ID)
call erreur(status,.TRUE.,"def_var_tn_ID")
status = NF90_DEF_VAR(fidM,"vn",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),vn_ID)
call erreur(status,.TRUE.,"def_var_vn_ID")
status = NF90_DEF_VAR(fidM,"un",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),un_ID)
call erreur(status,.TRUE.,"def_var_un_ID")
status = NF90_DEF_VAR(fidM,"sshb",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),sshb_ID)
call erreur(status,.TRUE.,"def_var_sshb_ID")
status = NF90_DEF_VAR(fidM,"hdivb",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),hdivb_ID)
call erreur(status,.TRUE.,"def_var_hdivb_ID")
status = NF90_DEF_VAR(fidM,"rotb",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),rotb_ID)
call erreur(status,.TRUE.,"def_var_rotb_ID")
status = NF90_DEF_VAR(fidM,"sb",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),sb_ID)
call erreur(status,.TRUE.,"def_var_sb_ID")
status = NF90_DEF_VAR(fidM,"tb",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),tb_ID)
call erreur(status,.TRUE.,"def_var_tb_ID")
status = NF90_DEF_VAR(fidM,"vb",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),vb_ID)
call erreur(status,.TRUE.,"def_var_vb_ID")
status = NF90_DEF_VAR(fidM,"ub",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),ub_ID)
call erreur(status,.TRUE.,"def_var_ub_ID")
status = NF90_DEF_VAR(fidM,"rdttra1",NF90_DOUBLE,rdttra1_ID)
call erreur(status,.TRUE.,"def_var_rdttra1_ID")
status = NF90_DEF_VAR(fidM,"rdt",NF90_DOUBLE,rdt_ID)
call erreur(status,.TRUE.,"def_var_rdt_ID")
status = NF90_DEF_VAR(fidM,"gcxb",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),gcxb_ID)
call erreur(status,.TRUE.,"def_var_gcxb_ID")
status = NF90_DEF_VAR(fidM,"gcx",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),gcx_ID)
call erreur(status,.TRUE.,"def_var_gcx_ID")
status = NF90_DEF_VAR(fidM,"qsr_hc_b",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),qsr_hc_b_ID)
call erreur(status,.TRUE.,"def_var_qsr_hc_b_ID")
status = NF90_DEF_VAR(fidM,"sbc_sc_b",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),sbc_sc_b_ID)
call erreur(status,.TRUE.,"def_var_sbc_sc_b_ID")
status = NF90_DEF_VAR(fidM,"sbc_hc_b",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),sbc_hc_b_ID)
call erreur(status,.TRUE.,"def_var_sbc_hc_b_ID")
status = NF90_DEF_VAR(fidM,"dissl",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),dissl_ID)
call erreur(status,.TRUE.,"def_var_dissl_ID")
status = NF90_DEF_VAR(fidM,"avmv",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),avmv_ID)
call erreur(status,.TRUE.,"def_var_avmv_ID")
status = NF90_DEF_VAR(fidM,"avmu",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),avmu_ID)
call erreur(status,.TRUE.,"def_var_avmu_ID")
status = NF90_DEF_VAR(fidM,"avm",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),avm_ID)
call erreur(status,.TRUE.,"def_var_avm_ID")
status = NF90_DEF_VAR(fidM,"avt",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),avt_ID)
call erreur(status,.TRUE.,"def_var_avt_ID")
status = NF90_DEF_VAR(fidM,"en",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_z,dimID_t/),en_ID)
call erreur(status,.TRUE.,"def_var_en_ID")
status = NF90_DEF_VAR(fidM,"emps_b",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),emps_b_ID)
call erreur(status,.TRUE.,"def_var_emps_b_ID")
status = NF90_DEF_VAR(fidM,"emp_b",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),emp_b_ID)
call erreur(status,.TRUE.,"def_var_emp_b_ID")
status = NF90_DEF_VAR(fidM,"qns_b",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),qns_b_ID)
call erreur(status,.TRUE.,"def_var_qns_b_ID")
status = NF90_DEF_VAR(fidM,"vtau_b",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),vtau_b_ID)
call erreur(status,.TRUE.,"def_var_vtau_b_ID")
status = NF90_DEF_VAR(fidM,"utau_b",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),utau_b_ID)
call erreur(status,.TRUE.,"def_var_utau_b_ID")
status = NF90_DEF_VAR(fidM,"rnf_sc_b",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),rnf_sc_b_ID)
call erreur(status,.TRUE.,"def_var_rnf_sc_b_ID")
status = NF90_DEF_VAR(fidM,"rnf_hc_b",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),rnf_hc_b_ID)
call erreur(status,.TRUE.,"def_var_rnf_hc_b_ID")
status = NF90_DEF_VAR(fidM,"rnf_b",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_t/),rnf_b_ID)
call erreur(status,.TRUE.,"def_var_rnf_b_ID")
status = NF90_DEF_VAR(fidM,"adatrj",NF90_DOUBLE,adatrj_ID)
call erreur(status,.TRUE.,"def_var_adatrj_ID")
status = NF90_DEF_VAR(fidM,"ndastp",NF90_DOUBLE,ndastp_ID)
call erreur(status,.TRUE.,"def_var_ndastp_ID")
status = NF90_DEF_VAR(fidM,"kt",NF90_DOUBLE,kt_ID)
call erreur(status,.TRUE.,"def_var_kt_ID")
status = NF90_DEF_VAR(fidM,"time_counter",NF90_DOUBLE,(/dimID_t/),time_ID)
call erreur(status,.TRUE.,"def_var_time_ID")
status = NF90_DEF_VAR(fidM,"nav_lev",NF90_FLOAT,(/dimID_z/),nav_lev_ID)
call erreur(status,.TRUE.,"def_var_nav_lev_ID")
status = NF90_DEF_VAR(fidM,"nav_lat",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lat_ID)
call erreur(status,.TRUE.,"def_var_nav_lat_ID")
status = NF90_DEF_VAR(fidM,"nav_lon",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lon_ID)
call erreur(status,.TRUE.,"def_var_nav_lon_ID")

status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Rebuilt from mpp subdomains using zzzzrebuild_restart.f90")
call erreur(status,.TRUE.,"put_att_global")

status = NF90_ENDDEF(fidM) ; call erreur(status,.TRUE.,"fin_definition") 

status = NF90_PUT_VAR(fidM,rhop_ID,GLO_rhop)         ; call erreur(status,.TRUE.,"var_rhop_ID")
status = NF90_PUT_VAR(fidM,sshn_ID,GLO_sshn)         ; call erreur(status,.TRUE.,"var_sshn_ID")
status = NF90_PUT_VAR(fidM,hdivn_ID,GLO_hdivn)       ; call erreur(status,.TRUE.,"var_hdivn_ID")
status = NF90_PUT_VAR(fidM,rotn_ID,GLO_rotn)         ; call erreur(status,.TRUE.,"var_rotn_ID")
status = NF90_PUT_VAR(fidM,sn_ID,GLO_sn)             ; call erreur(status,.TRUE.,"var_sn_ID")
status = NF90_PUT_VAR(fidM,tn_ID,GLO_tn)             ; call erreur(status,.TRUE.,"var_tn_ID")
status = NF90_PUT_VAR(fidM,vn_ID,GLO_vn)             ; call erreur(status,.TRUE.,"var_vn_ID")
status = NF90_PUT_VAR(fidM,un_ID,GLO_un)             ; call erreur(status,.TRUE.,"var_un_ID")
status = NF90_PUT_VAR(fidM,sshb_ID,GLO_sshb)         ; call erreur(status,.TRUE.,"var_sshb_ID")
status = NF90_PUT_VAR(fidM,hdivb_ID,GLO_hdivb)       ; call erreur(status,.TRUE.,"var_hdivb_ID")
status = NF90_PUT_VAR(fidM,rotb_ID,GLO_rotb)         ; call erreur(status,.TRUE.,"var_rotb_ID")
status = NF90_PUT_VAR(fidM,sb_ID,GLO_sb)             ; call erreur(status,.TRUE.,"var_sb_ID")
status = NF90_PUT_VAR(fidM,tb_ID,GLO_tb)             ; call erreur(status,.TRUE.,"var_tb_ID")
status = NF90_PUT_VAR(fidM,vb_ID,GLO_vb)             ; call erreur(status,.TRUE.,"var_vb_ID")
status = NF90_PUT_VAR(fidM,ub_ID,GLO_ub)             ; call erreur(status,.TRUE.,"var_ub_ID")
status = NF90_PUT_VAR(fidM,rdttra1_ID,rdttra1)       ; call erreur(status,.TRUE.,"var_rdttra1_ID")
status = NF90_PUT_VAR(fidM,rdt_ID,rdt)               ; call erreur(status,.TRUE.,"var_rdt_ID")
status = NF90_PUT_VAR(fidM,gcxb_ID,GLO_gcxb)         ; call erreur(status,.TRUE.,"var_gcxb_ID")
status = NF90_PUT_VAR(fidM,gcx_ID,GLO_gcx)           ; call erreur(status,.TRUE.,"var_gcx_ID")
status = NF90_PUT_VAR(fidM,qsr_hc_b_ID,GLO_qsr_hc_b) ; call erreur(status,.TRUE.,"var_qsr_hc_b_ID")
status = NF90_PUT_VAR(fidM,sbc_sc_b_ID,GLO_sbc_sc_b) ; call erreur(status,.TRUE.,"var_sbc_sc_b_ID")
status = NF90_PUT_VAR(fidM,sbc_hc_b_ID,GLO_sbc_hc_b) ; call erreur(status,.TRUE.,"var_sbc_hc_b_ID")
status = NF90_PUT_VAR(fidM,dissl_ID,GLO_dissl)       ; call erreur(status,.TRUE.,"var_dissl_ID")
status = NF90_PUT_VAR(fidM,avmv_ID,GLO_avmv)         ; call erreur(status,.TRUE.,"var_avmv_ID")
status = NF90_PUT_VAR(fidM,avmu_ID,GLO_avmu)         ; call erreur(status,.TRUE.,"var_avmu_ID")
status = NF90_PUT_VAR(fidM,avm_ID,GLO_avm)           ; call erreur(status,.TRUE.,"var_avm_ID")
status = NF90_PUT_VAR(fidM,avt_ID,GLO_avt)           ; call erreur(status,.TRUE.,"var_avt_ID")
status = NF90_PUT_VAR(fidM,en_ID,GLO_en)             ; call erreur(status,.TRUE.,"var_en_ID")
status = NF90_PUT_VAR(fidM,emps_b_ID,GLO_emps_b)     ; call erreur(status,.TRUE.,"var_emps_b_ID")
status = NF90_PUT_VAR(fidM,emp_b_ID,GLO_emp_b)       ; call erreur(status,.TRUE.,"var_emp_b_ID")
status = NF90_PUT_VAR(fidM,qns_b_ID,GLO_qns_b)       ; call erreur(status,.TRUE.,"var_qns_b_ID")
status = NF90_PUT_VAR(fidM,vtau_b_ID,GLO_vtau_b)     ; call erreur(status,.TRUE.,"var_vtau_b_ID")
status = NF90_PUT_VAR(fidM,utau_b_ID,GLO_utau_b)     ; call erreur(status,.TRUE.,"var_utau_b_ID")
status = NF90_PUT_VAR(fidM,rnf_sc_b_ID,GLO_rnf_sc_b) ; call erreur(status,.TRUE.,"var_rnf_sc_b_ID")
status = NF90_PUT_VAR(fidM,rnf_hc_b_ID,GLO_rnf_hc_b) ; call erreur(status,.TRUE.,"var_rnf_hc_b_ID")
status = NF90_PUT_VAR(fidM,rnf_b_ID,GLO_rnf_b)       ; call erreur(status,.TRUE.,"var_rnf_b_ID")   
status = NF90_PUT_VAR(fidM,adatrj_ID,adatrj)         ; call erreur(status,.TRUE.,"var_adatrj_ID")
status = NF90_PUT_VAR(fidM,ndastp_ID,ndastp)         ; call erreur(status,.TRUE.,"var_ndastp_ID")
status = NF90_PUT_VAR(fidM,kt_ID,kt)                 ; call erreur(status,.TRUE.,"var_kt_ID")
status = NF90_PUT_VAR(fidM,time_ID,time_counter)     ; call erreur(status,.TRUE.,"var_time_ID")
status = NF90_PUT_VAR(fidM,nav_lev_ID,nav_lev)       ; call erreur(status,.TRUE.,"var_nav_lev_ID")
status = NF90_PUT_VAR(fidM,nav_lat_ID,GLO_nav_lat)   ; call erreur(status,.TRUE.,"var_nav_lat_ID")
status = NF90_PUT_VAR(fidM,nav_lon_ID,GLO_nav_lon)   ; call erreur(status,.TRUE.,"var_nav_lon_ID")

status = NF90_CLOSE(fidM)  ; call erreur(status,.TRUE.,"final")

end program modif

!-------------------------------------------------------------------------------

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
