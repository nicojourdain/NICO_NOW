pro std_plot_Med_Sdepth, S1, S2, SLev, lat, POSTSCRIPT = postscript,  _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  slat = strtrim(lat, 1)+'N'
  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_Med_Sdepth_'+slat+'_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
;
  title = 'Salinity ('+slat+')!C'+std_file1_T
  pltz, S1, MININ = 35., MAXIN = 37., INTER = 0.1, FORMAT = '(f4.1)' $
        , small = [1, 2, 1], COAST_THICK = 2, endpoints = [300., lat, 357., lat], TITLE = title $
        , boxzoom = [2000.], ZOOM = 2000., /PORTRAIT, _extra = ex 
;
  if std_file1_T NE std_file2_T then begin
    title = title+ ' - '+std_file2_T
    pltz, S1.arr - S2.arr + valmask*(1.-tmask), MININ = -.4, MAXIN = .4, INTER = .05, STYLE = 'so0so', FORMAT = '(f4.1)' $ 
          , small = [1, 2, 2], COAST_THICK = 2, endpoints = [300., lat, 357., lat], TITLE = title $
          , boxzoom = [2000.], ZOOM = 2000., /noerase, _extra = ex 
  endif else begin
    title = title+ ' - Levitus'
    pltz, S1.arr - SLev.arr + valmask*(1.-tmask), MININ = -1., MAXIN = 1., INTER = 0.1, STYLE = 'so0so', FORMAT = '(f4.1)' $ 
          , small = [1, 2, 2], COAST_THICK = 2, endpoints = [300., lat, 357., lat], TITLE = title $
          , boxzoom = [2000.], ZOOM = 2000., /noerase, _extra = ex 
  endelse

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end
