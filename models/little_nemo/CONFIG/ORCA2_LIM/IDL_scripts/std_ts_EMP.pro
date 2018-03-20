pro std_ts_EMP, masknp, s_iodir_data, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs
  
@common
@std_common

; get exp1 info
  vemp = getenv('VAR1_EMP')    &   prefix = getenv('V1EMP_PREF')    &   suffix = getenv('V1EMP_SUFF')
; get exp2 info
  vemp2 = getenv('VAR2_EMP')   &   prefix2 = getenv('V2EMP_PREF')   &   suffix2 = getenv('V2EMP_SUFF')
;
  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_ts_EMP_'+prefix
  if prefix NE prefix2 then filename = filename + '_'+prefix2
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1

  d1_d2 = '('+strtrim(date1, 1)+' - '+strtrim(date2, 1)+')'
;
  iodir = std_iodir_data
;
  surf_oce = e1t * e2t * tmask[*,*,0] * masknp
  surf_oce = total(surf_oce)
;
  ts_EMP = rseries_ncdf(vemp, date1, date2, prefix, suffix, FIRSTONLY = 1 - allrec, direc = 'xy', mask2d = masknp)
  ts_EMP.arr = ts_EMP.arr * ( 1.E-09 * surf_oce )   &   ts_EMP.unit = 'Sv'
  title = prefix+' '+d1_d2+'!C'+blabla
  pltt, ts_EMP, 't', -.001, .001, date1, date2, /REMPLI $
        , small = [1, 2, 1], YTITLE = 'Sv', TITLE = title, /PORTRAIT, _extra = ex
  
  IF prefix NE prefix2 THEN BEGIN

    d1_d2_2 = '('+strtrim(date1_2, 1)+' - '+strtrim(date2_2, 1)+')'
    tsave = time
    ts_EMP2 = rseries_ncdf(vemp2, date1_2, date2_2, prefix2, suffix2, FIRSTONLY = 1 - allrec, direc = 'xy', mask2d = masknp)
    ts_EMP2.arr = ts_EMP2.arr * ( 1.E-09 * surf_oce )   &   ts_EMP2.unit = 'Sv'
    time = tsave   &   IF n_elements(time) NE jpt THEN stop

    title = prefix+' '+d1_d2+' - '+prefix2+' '+d1_d2_2+'!C'+blabla
    pltt, ts_EMP.arr - ts_EMP2.arr, 't', -.001, .001, date1, date2, /REMPLI $
          , color = 250, small = [1, 2, 2], YTITLE = 'Sv', TITLE = title, /noerase, _extra = ex

  ENDIF 

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps
  
  return
end

