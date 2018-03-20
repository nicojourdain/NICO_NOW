pro std_plot_GlobMeanTS, T1, T2, TLev, S1, S2, SLev, sEXP1, sEXP2, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_GlobMeanTS_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1

  varunit = T1.unit
  title = 'Temperature Global mean!C'+std_file1_T+' - Levitus (Black)'
  if std_file1_T NE std_file2_T THEN title = title+'!C'+std_file2_T+' - Levitus (Red)'
  plt1d, T1.arr - TLev.arr, typein = 'z', ticklen = 1, MIN = -2., MAX = 2., boxzoom = [4., 5300.], /KEEPBOTTOM $
         , small = [1, 2, 1], XGRIDSTYLE = 2, YGRIDSTYLE = 2, TITLE = title, /PORTRAIT,  _extra = ex
  if std_file1_T NE std_file2_T then begin
    plt1d, T2.arr - TLev.arr, typein = 'z',  ticklen = 1, MIN = -2., MAX = 2., boxzoom = [4., 5300.], /KEEPBOTTOM  $
           , /ov1d, COLOR = 250, TITLE = title, _extra = ex 
  endif
;
  varunit = S1.unit
  title = 'Salinity Global mean!C'+std_file1_T+' - Levitus (Black)'
  if std_file1_T NE std_file2_T THEN title = title+'!C'+std_file2_T+' - Levitus (Red)'
  plt1d, S1.arr - SLev.arr, typein = 'z', ticklen = 1, MIN = -.2, MAX = .2, boxzoom = [4., 5300.], /KEEPBOTTOM $
         , small = [1, 2, 2], XGRIDSTYLE = 2, YGRIDSTYLE = 2, TITLE = title, /NOERASE, _extra = ex
  if std_file1_T NE std_file2_T then begin
    plt1d, S2.arr - SLev.arr, typein = 'z',  ticklen = 1, MIN = -2., MAX = 2., boxzoom = [4., 5300.], /KEEPBOTTOM $
           ,  /ov1d, COLOR = 250, TITLE = title, _extra = ex 
  endif

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end

