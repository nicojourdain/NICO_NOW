pro std_ts_T, masknp, s_iodir_data, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs
  
@common
@std_common

; get exp1 info
  vtemp = getenv('VAR1_T')     &   prefix = getenv('V1T_PREF')         &   suffix = getenv('V1T_SUFF')
  vssh = getenv('VAR1_SSH')    &   sshprefix = getenv('V1SSH_PREF')    &   sshsuffix = getenv('V1SSH_SUFF')
; get exp2 info
  vtemp2 = getenv('VAR2_T')    &   prefix2 = getenv('V2T_PREF')        &   suffix2 = getenv('V2T_SUFF')
  vssh2 = getenv('VAR2_SSH')   &   sshprefix2 = getenv('V2SSH_PREF')   &   sshsuffix2 = getenv('V2SSH_SUFF')
;
  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_ts_T_'+prefix
  if prefix NE prefix2 then filename = filename + '_'+prefix2
  if KEYWORD_SET(postscript) then openps, filename+'_1.ps', portrait = 1

  d1_d2 = '('+strtrim(date1, 1)+' - '+strtrim(date2, 1)+')'

; read levitus data
  std_file_Levitus_T = isafile(getenv('FILE_TEMP_3D'), title = 'Levitus_T', iodir = std_iodir_climato)
  Lev = read_ncdf(getenv('VAR_TEMP_3D'), filename = std_file_Levitus_T, /nostruct ) 
  Lev_xyz = moyenne(Lev, 'xyz', mask2d = masknp)
  levz = moyenne(temporary(Lev), 'xy', mask2d = masknp, /KEEPBOTTOM)
;
  iodir = std_iodir_data
;

; read exp1 data
  std_ts_read, vtemp, date1, date2, prefix, suffix, ts_Temp, ts_z, masknp $
               , WITHSSH = vssh, SSHPREFIX = sshprefix, SSHSUFFIX = sshsuffix, LEVZ = levz

  title = prefix+' '+d1_d2+'!C'+blabla
  pltt, ts_Temp, 't', 1., 4.5, date1, date2, /REMPLI $
        , small = [1, 2, 1], YTITLE = varunit, TITLE = title, /PORTRAIT, _extra = ex

  title = prefix+' '+d1_d2+' - Levitus!C'+blabla
  pltt, ts_Temp - Lev_xyz, 't', -1., 1., date1, date2, /REMPLI $
        , small = [1, 2, 2], YTITLE = varunit, TITLE = title, /NOERASE, _extra = ex

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'_1.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps
  if KEYWORD_SET(postscript) then openps, filename+'_2.ps', portrait = 1

  title = prefix+' '+d1_d2+' - Levitus!C ZT-plot (0-1500m) '+blabla
  pltt, ts_z, 'zt', -2., 2., inter = .2, date1, date2, /REMPLI, style = 'so0so' $ 
        , small = [1, 2, 1], YTITLE = varunit, TITLE = title, boxzoom = 1500., /portrait, window = 1, _extra = ex
  
  title = prefix+' '+d1_d2+' - Levitus!C ZT-plot (0-6000m) '+blabla
  pltt, ts_z, 'zt', -2., 2., inter = .2, date1, date2, /REMPLI, style = 'so0so' $ 
        , small = [1, 2, 2], YTITLE = varunit, TITLE = title, boxzoom = 6000., /NOERASE, _extra = ex
  
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'_2.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  IF prefix NE prefix2 THEN BEGIN

    d1_d2_2 = '('+strtrim(date1_2, 1)+' - '+strtrim(date2_2, 1)+')'
; read exp2 data
    tsave = time
    std_ts_read, vtemp2, date1_2, date2_2, prefix2, suffix2, ts_Temp2, ts_z2, masknp $
                 , WITHSSH = vssh2, SSHPREFIX = sshprefix2, SSHSUFFIX = sshsuffix2, LEVZ = levz
    time = tsave   &   IF n_elements(time) NE jpt THEN stop

    if KEYWORD_SET(postscript) then openps, filename+'_3.ps', portrait = 1

    title = prefix+' '+d1_d2+' - '+prefix2+' '+d1_d2_2+'!C'+blabla
    pltt, ts_Temp - ts_Temp2, 't', -1., 1., date1, date2, /REMPLI $
          , COLOR = 250, small = [1, 2, 1], YTITLE = varunit, TITLE = title, /PORTRAIT, window = 2, _extra = ex

    title = prefix+' '+d1_d2+' - '+prefix2+' '+d1_d2_2+'!C ZT-plot (0-6000m) '+blabla
    pltt, ts_z - ts_z2, 'zt', -1., 1., inter = .1, date1, date2, /REMPLI, style = 'so0so' $ 
          , small = [1, 2, 2], YTITLE = varunit, TITLE = title, boxzoom = 6000., /NOERASE, _extra = ex

    htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'_3.png  />  ' ]
    if KEYWORD_SET(postscript) then closeps
    
  ENDIF

  return
end
