pro std_ts_SSH, masknp, s_iodir_data, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs
  
@common
@std_common

; get exp1 info
  vssh = getenv('VAR1_SSH')    &   prefix = getenv('V1SSH_PREF')    &   suffix = getenv('V1SSH_SUFF')
; get exp2 info
  vssh2 = getenv('VAR2_SSH')   &   prefix2 = getenv('V2SSH_PREF')   &   suffix2 = getenv('V2SSH_SUFF')
;
  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_ts_SSH_'+prefix
  if prefix NE prefix2 then filename = filename + '_'+prefix2
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1

  d1_d2 = '('+strtrim(date1, 1)+' - '+strtrim(date2, 1)+')'
;
  iodir = std_iodir_data
;
  ts_SSH = rseries_ncdf(vssh, date1, date2, prefix, suffix, FIRSTONLY = 1 - allrec, direc = 'xy', mask2d = masknp)
  ts_SSH.arr = ts_SSH.arr * 100   &   ts_SSH.unit = 'cm'
  title = prefix+' '+d1_d2+'!C'+blabla
  pltt, ts_SSH, 't', -.1, .1, date1, date2, /REMPLI $
        , small = [1, 2, 1], YTITLE = 'cm', TITLE = title, /PORTRAIT, _extra = ex
  
  IF prefix NE prefix2 THEN BEGIN

    d1_d2_2 = '('+strtrim(date1_2, 1)+' - '+strtrim(date2_2, 1)+')'
    tsave = time
    ts_SSH2 = rseries_ncdf(vssh2, date1_2, date2_2, prefix2, suffix2, FIRSTONLY = 1 - allrec, direc = 'xy', mask2d = masknp)
    ts_SSH2.arr = ts_SSH2.arr * 100   &   ts_SSH2.unit = 'cm'
    time = tsave   &   IF n_elements(time) NE jpt THEN stop

    title = prefix+' '+d1_d2+' - '+prefix2+' '+d1_d2_2+'!C'+blabla
    pltt, ts_SSH.arr - ts_SSH2.arr, 't', -.1, .1, date1, date2, /REMPLI $
          , color = 250, small = [1, 2, 2], YTITLE = 'cm', TITLE = title, /noerase, _extra = ex

  ENDIF 

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps
  
  return
end

