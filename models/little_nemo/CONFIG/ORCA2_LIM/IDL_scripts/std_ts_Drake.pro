pro std_ts_Drake, masknp, s_iodir_data, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs
  
@common
@std_common

; get exp1 info
  vU1 = getenv('VAR1_U')   &   prefix = getenv('V1U_PREF')    &   suffix = getenv('V1U_SUFF')
; get exp2 info
  vU2 = getenv('VAR2_U')   &   prefix2 = getenv('V2U_PREF')   &   suffix2 = getenv('V2U_SUFF')
;
  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_ts_Drake_'+prefix
  if prefix NE prefix2 then filename = filename + '_'+prefix2
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
;
  d1_d2 = '('+strtrim(date1, 1)+' - '+strtrim(date2, 1)+')'
;
  iodir = std_iodir_data
 
; find a point in south America (around 60E-30S)
  if max(glamt) gt 300 then testlam = abs(glamt - 300) else testlam = abs(glamt + 60)
  testlat = abs(gphit + 30)
  index = where(abs(testlam - min(testlam)) lt 1 and abs(testlat - min(testlat)) lt 1 )
  xindex = index[0] mod jpi
  yindex = index[0]/jpi
; define a domain limited to a thin band going from Antactica to this point in south America
  domdef, xindex, xindex+1, 0, yindex, 0, jpk-1, /index ; keep 2 points for x to avoid degenerated dimension...
;
  u1 = rseries_ncdf(vU1, date1, date2, prefix, suffix, FIRSTONLY = 1 - allrec, /nostruct)
  drk1 = fltarr(jpt)
  trans = strlowcase(getenv('VAR1_U')) EQ 'uocetr_eff'
  FOR t = 0, jpt-1 DO BEGIN
    tmp = bsf(u1[*, *, *, t], TRANSPORT = trans, /nostruct)
    drk1[t] = tmp[0, nyt-1]-tmp[0, 0]
  ENDFOR
;
  title = prefix+' '+d1_d2+'!C'+blabla
  pltt, drk1, 't', 0., 200., date1, date2, /REMPLI $
        , small = [1, 2, 1],  TITLE = title, YTITLE = varunit, /PORTRAIT, _extra = ex
  
  if prefix NE prefix2 then begin
;
    d1_d2_2 = '('+strtrim(date1_2, 1)+' - '+strtrim(date2_2, 1)+')'
    tsave = time
    u2 = rseries_ncdf(vU2, date1_2, date2_2, prefix2, suffix2, FIRSTONLY = 1 - allrec, /nostruct)
    drk2 = fltarr(jpt)
    trans = strlowcase(getenv('VAR2_U')) EQ 'uocetr_eff'
    FOR t = 0, jpt-1 DO BEGIN
      tmp = bsf(u2[*, *, *, t], TRANSPORT = trans, /nostruct)
      drk2[t] = tmp[0, nyt-1]-tmp[0, 0]
    ENDFOR
    time = tsave   &   IF n_elements(time) NE jpt THEN stop

    title = prefix+' '+d1_d2+' - '+prefix2+' '+d1_d2_2+'!C'+blabla
    pltt, drk1 - drk2, 't', -30., 30., date1, date2, /REMPLI, /NOERASE $
          , COLOR = 250, small = [1, 2, 2],  TITLE = title, YTITLE = varunit, _extra = ex
    
  endif

  domdef

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps
  
  return
end
