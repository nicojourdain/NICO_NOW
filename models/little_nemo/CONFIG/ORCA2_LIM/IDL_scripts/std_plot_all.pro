pro std_plot_all, doplot = doplot, _extra = ex

  compile_opt idl2, strictarrsubs

@common
@std_common
                                ; scripts for nemo v3_2 and v3_3

  PRINT, ''
  PRINT, ' 	############################################'
  PRINT, ''
  PRINT, '                    LAUNCH of std_plots'
  PRINT, ''
  PRINT, ' 	############################################'
  PRINT, ''
;
  std_iodir_data    = isadirectory(getenv('DIR_DATA'), title = 'path of data in NetCdf format')
  std_iodir_climato = isadirectory(getenv('DIR_CLIMATO'), title = 'path of climatological data')
  std_iodir_mask    = isadirectory(getenv('DIR_MASK'), title = 'path of mask files (ex: subbasins)')
; meshmask
  std_file_mesh = isafile(getenv('FILE_MESH_MASK'), title = 'mesh_mask', iodir = std_iodir_mask)
  std_file_msksub = isafile(getenv('FILE_MASK_SUBDOMAIN'), title = 'sub-bassin masks', iodir = std_iodir_mask)

; Levitus 98
  std_file_Levitus_T =  isafile(getenv('FILE_TEMP_3D'), title = 'Levitus_T', iodir = std_iodir_climato)
  std_file_Levitus_S =  isafile(getenv('FILE_SAL_3D'), title = 'Levitus_S', iodir = std_iodir_climato)
  std_file_reynolds =  isafile(getenv('FILE_SST'), title = 'Reynolds', iodir = std_iodir_climato)
  std_file_oaflux =  isafile(getenv('FILE_FLUX'), title = 'oaflux', iodir = std_iodir_climato)
  std_file_mld =  isafile(getenv('FILE_MLD'), title = 'Mixed layer depth', iodir = std_iodir_climato)

  IF strlowcase(getenv('FILE_GEOHEAT')) EQ 'no' THEN std_file_geoheat = 'no' $
  ELSE std_file_geoheat =  isafile(getenv('FILE_GEOHEAT'), title = 'Geothermal heating', iodir = std_iodir_climato)
;
  allrec =  1 - keyword_set(long(getenv('READ_ONLY_FIRST_RECORD')))
; Output run experience1
  std_file1_T     = isafile(getenv('FILE1_T'), title = 'exp1 grid T input file', iodir = std_iodir_data)
  std_file1_U     = isafile(getenv('FILE1_U'), title = 'exp1 grid U input file', iodir = std_iodir_data)
  std_file1_V     = isafile(getenv('FILE1_V'), title = 'exp1 grid V input file', iodir = std_iodir_data)
  std_file1_I     = isafile(getenv('FILE1_I'), title = 'exp1 ice    input file', iodir = std_iodir_data)
  
; Output run experience2
  std_file2_T     = isafile(getenv('FILE2_T'), title = 'exp2 grid T input file', iodir = std_iodir_data)
  std_file2_U     = isafile(getenv('FILE2_U'), title = 'exp2 grid U input file', iodir = std_iodir_data)
  std_file2_V     = isafile(getenv('FILE2_V'), title = 'exp2 grid V input file', iodir = std_iodir_data)
  std_file2_I     = isafile(getenv('FILE2_I'), title = 'exp2 ice    input file', iodir = std_iodir_data)

  PRINT, ''
  PRINT, '	std_iodir_data : ' + std_iodir_data 
  PRINT, '	std_file1T : ' + std_file1_T
  PRINT, '	std_file1U : ' + std_file1_U
  PRINT, '	std_file1V : ' + std_file1_V
;  PRINT, '	std_file1W : ' + std_file1_W
  PRINT, '	std_file2I : ' + std_file1_I
  PRINT, '	std_file2T : ' + std_file2_T
  PRINT, '	std_file2U : ' + std_file2_U
  PRINT, '	std_file2V : ' + std_file2_V
;  PRINT, '	std_file2W : ' + std_file2_W
  PRINT, '	std_file2I : ' + std_file2_I
  PRINT, ''

;#########################################################################
;##########################  Load Grids   ################################
;#########################################################################
; load the grid
  load_orca, std_file_mesh  
; reading variables
  masknp = read_ncdf('tmaskutil', file = std_file_mesh, /nostruct, /cont_nofill)
;#########################################################################
;############################  Read Data  ################################
;#########################################################################
;
  allrec =  1; - keyword_set(long(getenv('READ_ONLY_FIRST_RECORD')))
;
;;; 3D ;;;
; temperature
  T1 = read_ncdf(getenv('VAR1_T'), allrecords = allrec, direc = 't', filename = std_file1_T ) 
  IF std_file2_T NE std_file1_T THEN BEGIN
    T2 = read_ncdf(getenv('VAR2_T'), allrecords = allrec, direc = 't', filename = std_file2_T ) 
  ENDIF ELSE T2 = {arr:-1}
  TLev = read_ncdf(getenv('VAR_TEMP_3D'), filename = std_file_Levitus_T ) 
  TRey = read_ncdf(getenv('VAR_SST'), filename = std_file_reynolds ) 

; salinity
  S1 = read_ncdf(getenv('VAR1_S'), allrecords = allrec, direc = 't', filename = std_file1_T ) 
  IF std_file2_T NE std_file1_T THEN BEGIN
    S2 = read_ncdf(getenv('VAR2_S'), allrecords = allrec, direc = 't', filename = std_file2_T ) 
  ENDIF ELSE S2 = {arr:-1}
  SLev = read_ncdf(getenv('VAR_SAL_3D'), filename = std_file_Levitus_S ) 

;;; 2D ;;;
; Net Downward heat flux
  Q1 = read_ncdf(getenv('VAR1_QNET'), allrecords = allrec, direc = 't', filename = std_file1_T ) 
  IF std_file2_T NE std_file1_T THEN BEGIN
    Q2 = read_ncdf(getenv('VAR2_QNET'), allrecords = allrec, direc = 't', filename = std_file2_T )
  ENDIF ELSE Q2 = {arr:-1}
; Geothermal heating
  IF std_file_geoheat EQ 'no' THEN geo = {arr:float(getenv('VAR_GEOHEAT'))} $
  ELSE geo = read_ncdf(getenv('VAR_GEOHEAT'), filename =  std_file_geoheat )
  geo = geo.arr*1.e-3          ; convert into W/m2
;climatology 
  QNET = read_ncdf(getenv('VAR_FLUX'), filename = std_file_oaflux )

; erp (evaporation damping)
  ERP1 = read_ncdf(getenv('VAR1_ERP'), allrecords = allrec, direc = 't', filename = std_file1_T )
  ERP1 = {arr:ERP1.arr * 86400., unit:'mm/day', grid:'T'}
  IF std_file2_T NE std_file1_T THEN BEGIN
    ERP2 = read_ncdf(getenv('VAR2_ERP'), allrecords = allrec, direc = 't', filename = std_file2_T )
    ERP2 = {arr:ERP2.arr * 86400., unit:'mm/day', grid:'T'}
  ENDIF ELSE ERP2 = {arr:-1}

; emp (evaporation minus precipitation)
  EMP1 = read_ncdf(getenv('VAR1_EMP'), allrecords = allrec, direc = 't', filename = std_file1_T )
  EMP1 = {arr:EMP1.arr * 86400., unit:'mm/day', grid:'T'}
  IF std_file2_T NE std_file1_T THEN BEGIN
    EMP2 = read_ncdf(getenv('VAR2_EMP'), allrecords = allrec, direc = 't', filename = std_file2_T )
    EMP2 = {arr:EMP2.arr * 86400., unit:'mm/day', grid:'T'}
  ENDIF ELSE EMP2 = {arr:-1}
  
;mixed layer depth
  MLD1 = read_ncdf(getenv('VAR1_MLD'), allrecords = allrec, direc = 't', filename = std_file1_T ) ; 10 m
  IF std_file2_T NE std_file1_T THEN BEGIN
    MLD2 = read_ncdf(getenv('VAR2_MLD'), allrecords = allrec, direc = 't', filename = std_file2_T ) ; 10 m
  ENDIF ELSE MLD2 = {arr:-1}
;climatology 
  MLD = read_ncdf(getenv('VAR_MLD'), filename = std_file_mld )

; velocities
  U1 = read_ncdf(getenv('VAR1_U'), allrecords = allrec, direc = 't', filename = std_file1_U )
  IF strlowcase(getenv('VAR1_U')) EQ 'uocetr_eff' THEN BEGIN
    U1.arr = U1.arr / e3u_3d(/e2) * umask()
    U1.unit = 'm/s'
  ENDIF 
  IF std_file2_U NE std_file1_U THEN BEGIN
    U2 = read_ncdf(getenv('VAR2_U'), allrecords = allrec, direc = 't', filename = std_file2_U )
    IF strlowcase(getenv('VAR2_U')) EQ 'uocetr_eff' THEN BEGIN
      U2.arr = U2.arr / e3u_3d(/e2) * umask()
      U2.unit = 'm/s'
    ENDIF 
  ENDIF ELSE U2 = {arr:-1}
;
  V1 = read_ncdf(getenv('VAR1_V'), allrecords = allrec, direc = 't', filename = std_file1_V )
  IF strlowcase(getenv('VAR1_V')) EQ 'vocetr_eff' THEN BEGIN
    V1.arr = V1.arr / e3v_3d(/e1) * vmask()
    V1.unit = 'm/s'
  ENDIF 
  IF std_file2_V NE std_file1_V THEN BEGIN
    V2 = read_ncdf(getenv('VAR2_V'), allrecords = allrec, direc = 't', filename = std_file2_V )
    IF strlowcase(getenv('VAR2_V')) EQ 'vocetr_eff' THEN BEGIN
      V2.arr = V2.arr / e3v_3d(/e1) * vmask()
      V2.unit = 'm/s'
    ENDIF 
  ENDIF ELSE V2 = {arr:-1}

; ice
  Ithi_1 = read_ncdf(getenv('VAR1_Ithick'), allrecords = allrec, filename = std_file1_I ) 
  caldat, time, mm
  march = where(mm EQ 3, cnt)
  Ithi_march_1 = {arr:1./float(cnt) * total(Ithi_1.arr[*, *, temporary(march)], 3), unit:Ithi_1.unit} 
  sept = where(mm EQ 9, cnt)
  Ithi_sept_1 = {arr:1./float(cnt) * total(Ithi_1.arr[*, *, temporary(sept)], 3), unit:Ithi_1.unit} 
  undefine, Ithi_1
;
  IF std_file2_I NE std_file1_I THEN BEGIN
    Ithi_2 = read_ncdf(getenv('VAR2_Ithick'), allrecords = allrec, filename = std_file2_I ) 
    caldat, time, mm
    march = where(mm EQ 3, cnt)
    Ithi_march_2 = {arr:1./float(cnt) * total(Ithi_2.arr[*, *, temporary(march)], 3), unit:Ithi_2.unit} 
    sept = where(mm EQ 9, cnt)
    Ithi_sept_2 = {arr:1./float(cnt) * total(Ithi_2.arr[*, *, temporary(sept)], 3), unit:Ithi_2.unit} 
    undefine, Ithi_2
  ENDIF ELSE BEGIN 
    Ithi_march_2 = {arr:-1}
    Ithi_sept_2 = {arr:-1}
  ENDELSE
;
  Ifra_1 = read_ncdf(getenv('VAR1_Ifrac'), allrecords = allrec, filename = std_file1_I ) 
  caldat, time, mm
  march = where(mm EQ 3, cnt)
  Ifra_march_1 = {arr:1./float(cnt) * total(Ifra_1.arr[*, *, temporary(march)], 3), unit:Ifra_1.unit} 
  sept = where(mm EQ 9, cnt)
  Ifra_sept_1 = {arr:1./float(cnt) * total(Ifra_1.arr[*, *, temporary(sept)], 3), unit:Ifra_1.unit}
  undefine, Ifra_1
;
  IF std_file2_I NE std_file1_I THEN BEGIN
    Ifra_2 = read_ncdf(getenv('VAR2_Ifrac'), allrecords = allrec, filename = std_file2_I ) 
    caldat, time, mm
    march = where(mm EQ 3, cnt)
    Ifra_march_2 = {arr:1./float(cnt) * total(Ifra_2.arr[*, *, temporary(march)], 3), unit:Ifra_2.unit}
    sept = where(mm EQ 9, cnt)
    Ifra_sept_2 = {arr:1./float(cnt) * total(Ifra_2.arr[*, *, temporary(sept)], 3), unit:Ifra_2.unit}
    undefine, Ifra_2
  ENDIF ELSE BEGIN 
    Ifra_march_2 = {arr:-1}
    Ifra_sept_2 = {arr:-1}
  ENDELSE
;
  jpt = 1
;
; shorter file names for legends...
;
  std_file1_T = file_basename(std_file1_T,'.nc')
  std_file1_T = (strsplit(std_file1_T,'_grid_T',/extract,/regex))[0]
  std_file2_T = file_basename(std_file2_T,'.nc')
  std_file2_T = (strsplit(std_file2_T,'_grid_T',/extract,/regex))[0]
  std_file1_U = file_basename(std_file1_U,'.nc')
  std_file1_U = (strsplit(std_file1_U,'_grid_U',/extract,/regex))[0]
  std_file2_U = file_basename(std_file2_U,'.nc')
  std_file2_U = (strsplit(std_file2_U,'_grid_U',/extract,/regex))[0]
  std_file1_V = file_basename(std_file1_V,'.nc')
  std_file1_V = (strsplit(std_file1_V,'_grid_V',/extract,/regex))[0]
  std_file2_V = file_basename(std_file2_V,'.nc')
  std_file2_V = (strsplit(std_file2_V,'_grid_V',/extract,/regex))[0]
  std_file1_I = file_basename(std_file1_I,'.nc')
  std_file1_I = (strsplit(std_file1_I,'_icemod',/extract,/regex))[0]
  std_file2_I = file_basename(std_file2_I,'.nc')
  std_file2_I = (strsplit(std_file2_I,'_icemod',/extract,/regex))[0]

;#########################################################################
;######################  STANDARD PLOTS   ################################
;#########################################################################

  IF keyword_set(doplot) EQ 0 THEN doplot = 0

; fixed color tabled
  lct, 64
  cnt = 0
  htmltxt = ''
;
  cnt = cnt+1   &   blabla = 'Erp salinity damping term'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_erp, ERP1, ERP2, _extra = ex

  cnt = cnt+1   &   blabla = 'Evaporation - Precipitation - Runoff term'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_emp, EMP1, EMP2, _extra = ex

  cnt = cnt+1   &   blabla = 'Net heat flux'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_qnet, Q1, Q2, QNET, _extra = ex

  cnt = cnt+1   &   blabla = 'Meridionnal Heat Transport'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_mht, Q1.arr+geo, Q2.arr+geo, masknp, std_file_msksub, _extra = ex

  cnt = cnt+1   &   blabla = 'Global Barotropic stream Function'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_bsf, U1, U2, _extra = ex

  cnt = cnt+1   &   blabla = 'mean Temperature diff with New Reynolds'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_sst, T1, T2, TRey, _extra = ex

  cnt = cnt+1   &   blabla = 'mean Salinity diff with Levitus'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_sss, S1, S2, SLev, _extra = ex

  cnt = cnt+1   &   blabla = 'Arctic mean Salinity diff with Levitus'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_ArcSal, S1, SLev, _extra = ex       

  cnt = cnt+1   &   blabla = 'Arctic mean Salinity diff with Levitus and exp2'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_ArcSal, S1, S2, SLev, _extra = ex

  cnt = cnt+1   &   blabla = 'Arctic mean Salinity diff with Levitus at z=100 meters'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_ArcSal, S1, SLev, /z100, _extra = ex       
 
  cnt = cnt+1   &   blabla = 'Arctic mean Salinity diff with Levitus and exp2 at z=100 meters'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_ArcSal, S1, S2, SLev, /z100, _extra = ex

  cnt = cnt+1   &   blabla = 'mean Temperature diff with Levitus at z=100 meters'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_T100m, T1, T2, Tlev, _extra = ex
  
  cnt = cnt+1   &   blabla = 'mean Salinity diff with Levitus at z=100 meters'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_S100m, S1, S2, SLev, _extra = ex
  
  cnt = cnt+1   &   blabla = 'Mixed layer depth'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_mld, MLD1, MLD, _extra = ex

  cnt = cnt+1   &   blabla = 'Mixed layer depth differences'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_mld, MLD1, MLD2, MLD, _extra = ex

  cnt = cnt+1   &   blabla = 'Zonal mean Mixed layer depth'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_ZonMld, MLD1, MLD2, MLD, _extra = ex
  
  cnt = cnt+1   &   blabla = 'Zonal mean Temperature diff with Levitus: Global'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_zonal_T, T1, T2, TLev, _extra = ex

  cnt = cnt+1   &   blabla = 'Zonal mean Temperature diff with Levitus: Atlantic'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_zonal_T, T1, T2, TLev, SUBBASIN = 'Atl', _extra = ex
  
  cnt = cnt+1   &   blabla = 'Zonal mean Temperature diff with Levitus: Indian'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_zonal_T, T1, T2, TLev, SUBBASIN = 'Ind', _extra = ex
  
  cnt = cnt+1   &   blabla = 'Zonal mean Temperature diff with Levitus: Pacific'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_zonal_T, T1, T2, TLev, SUBBASIN = 'Pac', _extra = ex

  cnt = cnt+1   &   blabla = 'Zonal mean Salinity diff with Levitus: Global'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_zonal_S, S1, S2, SLev, _extra = ex

  cnt = cnt+1   &   blabla = 'Zonal mean Salinity diff with Levitus: Atlantic'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_zonal_S, S1, S2, SLev, SUBBASIN = 'Atl', _extra = ex

  cnt = cnt+1   &   blabla = 'Zonal mean Salinity diff with Levitus: Indian'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_zonal_S, S1, S2, SLev, SUBBASIN = 'Ind', _extra = ex 

  cnt = cnt+1   &   blabla = 'Zonal mean Salinity diff with Levitus: Pacific'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_zonal_S, S1, S2, SLev, SUBBASIN = 'Pac', _extra = ex 
  
  cnt = cnt+1   &   blabla = 'Arctic Ice Thickness: MARCH'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_IceThick, Ithi_march_1, Ithi_march_2, /ARC, /MARCH, _extra = ex
  
  cnt = cnt+1   &   blabla = 'Arctic Ice Thickness: SEPT'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_IceThick, Ithi_sept_1, Ithi_sept_2, /ARC, /SEPT, _extra = ex
  
  cnt = cnt+1   &   blabla = 'Antarctic Ice Thickness: MARCH'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_IceThick, Ithi_march_1, Ithi_march_2, /ANT, /MARCH, _extra = ex

  cnt = cnt+1   &   blabla = 'Antarctic Ice Thickness: SEPT'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_IceThick, Ithi_sept_1, Ithi_sept_2, /ANT, /SEPT, _extra = ex

  cnt = cnt+1   &   blabla = 'Arctic Ice Fraction: MARCH'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_IceFrac, Ifra_march_1, Ifra_march_2, /ARC, /MARCH, _extra = ex
  
  cnt = cnt+1   &   blabla = 'Arctic Ice Fraction: SEPT'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_IceFrac, Ifra_sept_1, Ifra_sept_2, /ARC, /SEPT, _extra = ex
  
  cnt = cnt+1   &   blabla = 'Antarctic Ice Fraction: MARCH'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_IceFrac, Ifra_march_1, Ifra_march_2, /ANT, /MARCH, _extra = ex

  cnt = cnt+1   &   blabla = 'Antarctic Ice Fraction: SEPT'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_IceFrac, Ifra_sept_1, Ifra_sept_2, /ANT, /SEPT, _extra = ex

  cnt = cnt+1   &   blabla = 'Meridional stream Function: Global (no Med)'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_msf, V1, V2, SUBBASIN = 'GloNoMed', _extra = ex

  cnt = cnt+1   &   blabla = 'Meridional stream Function: Atlantic'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_msf, V1, V2, SUBBASIN = 'Atl', _extra = ex

  cnt = cnt+1   &   blabla = 'Meridional stream Function: Indian'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_msf, V1, V2, SUBBASIN = 'Ind', _extra = ex

  cnt = cnt+1   &   blabla = 'Meridional stream Function: Indo-Pacific'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_msf, V1, V2, SUBBASIN = 'IndoPac', _extra = ex

  cnt = cnt+1   &   blabla = 'Equatorial Temperature'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_EqT, T1, T2, Tlev, _extra = ex

  cnt = cnt+1   &   blabla = 'Equatorial Salinity'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_EqS, S1, S2, SLev, _extra = ex

  cnt = cnt+1   &   blabla = 'Equatorial zonal velocity'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_EqU, U1, U2, _extra = ex

  cnt = cnt+1   &   blabla = 'Mediterranean salt tongue at depth=700'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_Med_Sspread, S1, S2, SLev, 700, _extra = ex

  cnt = cnt+1   &   blabla = 'Mediterranean salt tongue at depth=1000'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_Med_Sspread, S1, S2, SLev, 1000, _extra = ex

  cnt = cnt+1   &   blabla = 'Mediterranean water at lat=40°N'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_Med_Sdepth, S1, S2, SLev, 40, _extra = ex

  cnt = cnt+1   &   blabla = 'Mediterranean water at lat=38°N'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_Med_Sdepth, S1, S2, SLev, 38, _extra = ex

  cnt = cnt+1   &   blabla = 'Vertical Global mean T & S'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_plot_GlobMeanTS, T1, T2, TLev, S1, S2, SLev, _extra = ex

  IF n_elements(htmltxt) GT 1 THEN putfile, psdir+'std_plot_html_body.txt', htmltxt[1:*]
  
  return
END
