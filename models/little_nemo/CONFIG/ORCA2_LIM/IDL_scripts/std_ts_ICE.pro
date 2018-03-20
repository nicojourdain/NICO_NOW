pro std_ts_ICE, masknp, s_iodir_data, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs
  
@common
@std_common

; get exp1 info
  vICE1 = getenv('VAR1_ICE')   &   prefix = getenv('V1ICE_PREF')    &   suffix = getenv('V1ICE_SUFF')
; get exp2 info
  vICE2 = getenv('VAR2_ICE')   &   prefix2 = getenv('V2ICE_PREF')   &   suffix2 = getenv('V2ICE_SUFF')
;
  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_ts_AMOC_'+prefix
  if prefix NE prefix2 then filename = filename + '_'+prefix2
  if KEYWORD_SET(postscript) then openps, filename+'_1.ps', portrait = 1
;
  d1_d2 = '('+strtrim(date1, 1)+' - '+strtrim(date2, 1)+')'
;
  iodir = std_iodir_data

  domdef, 0, jpi-1, 30, 90, /xindex
  ICE_N = rseries_ncdf(vICE1, date1, date2, prefix, suffix, FIRSTONLY = 1 - allrec, direc = 'xy', /integration, mask2d = masknp)
  ICE_N.arr = ICE_N.arr * 1.e-12   &   ICE_N.unit = '10^12 m^2'
  domdef, 0, jpi-1, -90, -30, /xindex
  ICE_S = rseries_ncdf(vICE1, date1, date2, prefix, suffix, FIRSTONLY = 1 - allrec, direc = 'xy', /integration, mask2d = masknp)
  ICE_S.arr = ICE_S.arr * 1.e-12   &   ICE_S.unit = '10^12 m^2'

  title = prefix+' '+d1_d2+'!C'+'Global Annual Mean Ice Area (North. Hemisp.)'
  pltt, ICE_N, 't', 0., 15., date1, date2, /REMPLI, /PORTRAIT $
        , small = [1, 2, 1], YTITLE = '10^12 m^2 ', TITLE = title, _extra = ex
  
  title = prefix+' '+d1_d2+'!C'+'Global Annual Mean Ice Area (South. Hemisp.)'
  pltt, ICE_S, 't', 0., 15., date1, date2, /REMPLI, /NOERASE $
        , small = [1, 2, 2], YTITLE = '10^12 m^2 ', TITLE = title, _extra = ex
  
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'_1.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  if prefix NE prefix2 then BEGIN

    d1_d2_2 = '('+strtrim(date1_2, 1)+' - '+strtrim(date2_2, 1)+')'

    tsave = time
    domdef, 0, jpi-1, 30, 90, /xindex
    ICE_N2 = rseries_ncdf(vICE2, date1_2, date2_2, prefix2, suffix2, FIRSTONLY = 1 - allrec, direc = 'xy', /integration, mask2d = masknp)
    ICE_N2.arr = ICE_N2.arr * 1.e-12   &   ICE_N2.unit = '10^12 m^2'
    domdef, 0, jpi-1, -90, -30, /xindex
    ICE_S2 = rseries_ncdf(vICE2, date1_2, date2_2, prefix2, suffix2, FIRSTONLY = 1 - allrec, direc = 'xy', /integration, mask2d = masknp)
    ICE_S2.arr = ICE_S2.arr * 1.e-12   &   ICE_S2.unit = '10^12 m^2'
    time = tsave   &   IF n_elements(time) NE jpt THEN stop

    if KEYWORD_SET(postscript) then openps, filename+'_2.ps', portrait = 1

    title = prefix+' '+d1_d2+' - '+prefix2+' '+d1_d2_2+'!C'+'Global Annual Mean Ice Area (North. Hemisp.)'
    pltt, ICE_N.arr - ICE_N2.arr, 't', -2., 2., date1, date2, /REMPLI, /PORTRAIT, window = 2 $
          , COLOR = 250, small = [1, 2, 1], YTITLE = '10^12 m^2 ', TITLE = title, _extra = ex
    
    title = prefix+' '+d1_d2+' - '+prefix2+' '+d1_d2_2+'!C'+'Global Annual Mean Ice Area (South. Hemisp.)'
    pltt, ICE_S.arr - ICE_S2.arr, 't', -2., 2., date1, date2, /REMPLI, /NOERASE $
          , COLOR = 250, small = [1, 2, 2], YTITLE = '10^12 m^2 ', TITLE = title, _extra = ex

    htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'_2.png  />  ' ]
    if KEYWORD_SET(postscript) then closeps

  endif

  domdef
  

  return
end
