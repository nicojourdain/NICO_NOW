pro std_ts_S, masknp, s_iodir_data, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs
  
@common
@std_common

; get exp1 info
  vsal = getenv('VAR1_S')      &   prefix = getenv('V1S_PREF')         &   suffix = getenv('V1S_SUFF')
  vssh = getenv('VAR1_SSH')    &   sshprefix = getenv('V1SSH_PREF')    &   sshsuffix = getenv('V1SSH_SUFF')
; get exp2 info
  vsal2 = getenv('VAR2_S')     &   prefix2 = getenv('V2S_PREF')        &   suffix2 = getenv('V2S_SUFF')
  vssh2 = getenv('VAR2_SSH')   &   sshprefix2 = getenv('V2SSH_PREF')   &   sshsuffix2 = getenv('V2SSH_SUFF')
;
  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_ts_S_'+prefix
  if prefix NE prefix2 then filename = filename + '_'+prefix2
  if KEYWORD_SET(postscript) then openps, filename+'_1.ps', portrait = 1

  d1_d2 = '('+strtrim(date1, 1)+' - '+strtrim(date2, 1)+')'

; read levitus data
  std_file_Levitus_S =  isafile(getenv('FILE_SAL_3D'), title = 'Levitus_S', iodir = std_iodir_climato)
  Lev = read_ncdf(getenv('VAR_SAL_3D'), filename = std_file_Levitus_S, /nostruct ) 
  Lev_xyz = moyenne(Lev, 'xyz', mask2d = masknp)
  levz = moyenne(temporary(Lev), 'xy', mask2d = masknp, /KEEPBOTTOM)
;
  iodir = std_iodir_data
;

; read exp1 data
  std_ts_read, vsal, date1, date2, prefix, suffix, ts_Sal, ts_z, masknp $
               , WITHSSH = vssh, SSHPREFIX = sshprefix, SSHSUFFIX = sshsuffix, LEVZ = levz

  title = prefix+' '+d1_d2+'!C'+blabla
  pltt, ts_Sal, 't', 34.54, 34.76, date1, date2, /REMPLI $
        , small = [1, 2, 1], YTITLE = varunit, TITLE = title, /PORTRAIT, _extra = ex

  title = prefix+' '+d1_d2+' - Levitus!C'+blabla
  pltt, ts_Sal - Lev_xyz, 't', -.05, .05, date1, date2, /REMPLI $
        , small = [1, 2, 2], YTITLE = varunit, TITLE = title, /NOERASE, _extra = ex

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'_1.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps
  if KEYWORD_SET(postscript) then openps, filename+'_2.ps', portrait = 1

  title = prefix+' '+d1_d2+' - Levitus!C ZT-plot (0-1500m) '+blabla
  pltt, ts_z, 'zt', -.5, .5, INTER = .05, date1, date2, /REMPLI, style = 'so0so' $ 
        , small = [1, 2, 1], YTITLE = varunit, TITLE = title, boxzoom = 1500., /portrait, window = 1, _extra = ex
  
  title = prefix+' '+d1_d2+' - Levitus!C ZT-plot (0-6000m) '+blabla
  pltt, ts_z, 'zt', -.5, .5, INTER = .05, date1, date2, /REMPLI, style = 'so0so' $ 
        , small = [1, 2, 2], YTITLE = varunit, TITLE = title, boxzoom = 6000., /NOERASE, _extra = ex
  
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'_2.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  IF prefix NE prefix2 THEN BEGIN

    d1_d2_2 = '('+strtrim(date1_2, 1)+' - '+strtrim(date2_2, 1)+')'
; read exp2 data
    tsave = time
    std_ts_read, vsal2, date1_2, date2_2, prefix2, suffix2, ts_Sal2, ts_z2, masknp $
                 , WITHSSH = vssh2, SSHPREFIX = sshprefix2, SSHSUFFIX = sshsuffix2, LEVZ = levz
    time = tsave   &   IF n_elements(time) NE jpt THEN stop

    if KEYWORD_SET(postscript) then openps, filename+'_3.ps', portrait = 1

    title = prefix+' '+d1_d2+' - '+prefix2+' '+d1_d2_2+'!C'+blabla
    pltt, ts_Sal - ts_Sal2, 't', -0.1, 0.1, date1, date2, /REMPLI $
          , COLOR = 250, small = [1, 2, 1], YTITLE = varunit, TITLE = title, /PORTRAIT, window = 2, _extra = ex

    title = prefix+' '+d1_d2+' - '+prefix2+' '+d1_d2_2+'!C ZT-plot (0-6000m) '+blabla
    pltt, ts_z - ts_z2, 'zt', -0.1, 0.1, INTER = .01, date1, date2, /REMPLI, style = 'so0so' $ 
          , small = [1, 2, 2], YTITLE = varunit, TITLE = title, boxzoom = 6000., /NOERASE, _extra = ex

    htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'_3.png  />  ' ]
    if KEYWORD_SET(postscript) then closeps

  ENDIF

  return
end

