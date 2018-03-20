pro std_ts_Q, masknp, s_iodir_data, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs
  
@common
@std_common

; get exp1 info
  vq = getenv('VAR1_Q')    &   prefix = getenv('V1Q_PREF')    &   suffix = getenv('V1Q_SUFF')
; get exp2 info
  vq2 = getenv('VAR2_Q')   &   prefix2 = getenv('V2Q_PREF')   &   suffix2 = getenv('V2Q_SUFF')
;
  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_ts_Q_'+prefix
  if prefix NE prefix2 then filename = filename + '_'+prefix2
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1

  d1_d2 = '('+strtrim(date1, 1)+' - '+strtrim(date2, 1)+')'
;
  iodir = std_iodir_data
;
  ts_Q = rseries_ncdf(vq, date1, date2, prefix, suffix, FIRSTONLY = 1 - allrec, direc = 'xy', mask2d = masknp)
  title = prefix+' '+d1_d2+'!C'+blabla
  pltt, ts_Q, 't', -4, 4, date1, date2, /REMPLI $
        , small = [1, 2, 1], YTITLE = varunit, TITLE = title, /PORTRAIT, _extra = ex
  
  IF prefix NE prefix2 THEN BEGIN

    d1_d2_2 = '('+strtrim(date1_2, 1)+' - '+strtrim(date2_2, 1)+')'
    tsave = time
    ts_Q2 = rseries_ncdf(vq2, date1_2, date2_2, prefix2, suffix2, FIRSTONLY = 1 - allrec, direc = 'xy', mask2d = masknp)
    time = tsave   &   IF n_elements(time) NE jpt THEN stop

    title = prefix+' '+d1_d2+' - '+prefix2+' '+d1_d2_2+'!C'+blabla
    pltt, ts_Q.arr - ts_Q2.arr, 't', -4., 4., date1, date2, /REMPLI $
          , color = 250, small = [1, 2, 2], YTITLE = varunit, TITLE = title, /noerase, _extra = ex

  ENDIF 

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps
  
  return
end

