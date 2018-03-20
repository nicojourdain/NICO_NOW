PRO std_ts_read_AMOC, var_name, dt1, dt2, prefix, suffix, t45, t70, t50

  compile_opt idl2, strictarrsubs
  
@common
@std_common

  list = rseries_ncdf(var_name, dt1, dt2, prefix, suffix, /fileslist)
  nfiles = n_elements(list)

  t45 = 0.
  t70 = 0.
  t50 = 0.
  ts_Time = 0.

  trans = strlowcase(var_name) EQ 'vocetr_eff'

  FOR i = 0, nfiles-1 DO BEGIN
    var = read_ncdf(var_name, allrecords = allrec, filename = list[i], /nostruct)
    ts_Time = [ ts_Time, Time]
;
    FOR t = 0, jpt-1 DO BEGIN
      msftot = msf(var[*, *, *, t], TRANSPORT = trans, /nostruct, indexboxzoom = ind)
      yaxis = gphit[ind[0], ind[2]:ind[3]]

; computation of max Atlatic Meridional Overturninc Circulation at 40°N and 50°N
      indy = where(yaxis gt 40 and yaxis le 50)
      domdef, 0, 3500
      t45 = [t45, max(msftot[indy, firstzw:lastzw], /NaN)]
      
; computation of max atlantic Antarctic Bottom Water between 80°S and 65°S
      indy = where(yaxis gt -80 and yaxis le -65)
      domdef, 300, 3500
      t70 = [t70, min(msftot[indy, firstzw:lastzw], /NaN)]

;  computation of max Antarctic Abyssal Bottom Cell between 65°S and 30°N , 
      indy = where(yaxis gt -65 and yaxis le 30)
      domdef, 2500, 5000
      t50 = [t50, min(msftot[indy, firstzw:lastzw], /NaN)]
      
      domdef, 0, jpk-1, /zindex
    ENDFOR

  ENDFOR

  time = ts_Time[1:*]           ; remove first record of 0
  jpt = n_elements(time)
  
  t45 = t45[1:*]                ; remove first record of 0 
  t70 = t70[1:*]                ; remove first record of 0 
  t50 = t50[1:*]                ; remove first record of 0 

  return
end

pro std_ts_AMOC, masknp, s_iodir_data, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs
  
@common
@std_common

; get exp1 info
  vV1 = getenv('VAR1_V')   &   prefix = getenv('V1V_PREF')    &   suffix = getenv('V1V_SUFF')
; get exp2 info
  vV2 = getenv('VAR2_V')   &   prefix2 = getenv('V2V_PREF')   &   suffix2 = getenv('V2V_SUFF')
;
  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_ts_AMOC_'+prefix
  if prefix NE prefix2 then filename = filename + '_'+prefix2
  if KEYWORD_SET(postscript) then openps, filename+'_1.ps', portrait = 1
;
  d1_d2 = '('+strtrim(date1, 1)+' - '+strtrim(date2, 1)+')'
  d1_d2_2 = '('+strtrim(date1_2, 1)+' - '+strtrim(date2_2, 1)+')'
;
  iodir = std_iodir_data

; compute the MSF

  std_ts_read_AMOC, vV1, date1, date2, prefix, suffix, t45, t70, t50
  if prefix NE prefix2 then begin
    tsave = time
    std_ts_read_AMOC, vV2, date1_2, date2_2, prefix2, suffix2, t45_2, t70_2, t50_2
    time = tsave   &   IF n_elements(time) NE jpt THEN stop
  ENDIF

; plots...  

  title = prefix+' '+d1_d2+'!C'+'Max Atlantic MOC between 40N and 50N'
  pltt, t45, 't', 0., 30., date1, date2, /REMPLI, /PORTRAIT $
        ,  small = [1, 2, 1],  TITLE = title, YTITLE = varunit, _extra = ex
  IF prefix NE prefix2 then begin
    title = prefix+' '+d1_d2+' - '+prefix2+' '+d1_d2_2+'!C'+'Max Atlantic MOC between 40N and 50N'
    pltt, t45 - t45_2, 't', -9., 9., date1, date2, /REMPLI, /NOERASE $
          ,  COLOR = 250, small = [1, 2, 2],  TITLE = title, YTITLE = varunit, _extra = ex
  endif
  
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'_1.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps
  if KEYWORD_SET(postscript) then openps, filename+'_2.ps', portrait = 1

  title = prefix+' '+d1_d2+'!C'+'Max AntArctic Bottom Water between 80S and 65S '+d1_d2
  pltt, -t70, 't', 0., 20., date1, date2, /REMPLI, /PORTRAIT, window = 2 $
        , small = [1, 2, 1],  TITLE = title, YTITLE = varunit, _extra = ex
  if prefix NE prefix2 then begin 
    title = prefix+' '+d1_d2+' - '+prefix2+' '+d1_d2_2+'!C'+'Max AntArctic Bottom Water between 80S and 65S'
    pltt, -t70 + t70_2, 't', -5., 5., date1, date2, /REMPLI, /NOERASE $
          ,  COLOR = 250, small = [1, 2, 2],  TITLE = title, YTITLE = varunit, _extra = ex
  endif
  
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'_2.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps
  if KEYWORD_SET(postscript) then openps, filename+'_3.ps', portrait = 1

  title = prefix+' '+d1_d2+'!C'+'Max AntArctic Bottom Cell between 65S and 30N '+d1_d2
  pltt, -t50, 't', 5., 30., date1, date2, /REMPLI, /PORTRAIT, window = 3 $
        ,  small = [1, 2, 1],  TITLE = title, YTITLE = varunit, _extra = ex
  if prefix NE prefix2 then begin
    title = prefix+' '+d1_d2+' - '+prefix2+' '+d1_d2_2+'!C'+'Max AntArctic Bottom Cell between 65S and 30N'
    pltt, -t50 + t50_2, 't', -5., 5., date1, date2, /REMPLI, /NOERASE  $
          ,  COLOR = 250, small = [1, 2, 2],  TITLE = title, YTITLE = varunit, _extra = ex
  endif
  
  domdef, 0, jpk-1, /zindex

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'_3.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps
    
  return
end
