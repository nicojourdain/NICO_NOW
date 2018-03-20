pro std_plot_T100m, T1, T2, TLev, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  tmp = min(abs(100 - gdept), ind)
  sdepref = strtrim(round(gdept[ind]), 1)+'m'
  filename = cdti3 + '_T'+sdepref+'_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
;                              
  title = 'Temperature ('+sdepref+')!C'+std_file1_T
  plt, T1, MIN = -2., MAX = 32., INTER = 1., FORMAT = '(I2)'  $
       , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, boxzoom = [floor(gdept[ind]), ceil(gdept[ind])], /portrait, _extra = ex
;
  IF std_file1_T NE std_file2_T THEN BEGIN
    title = title+ ' - '+std_file2_T
    plt, T1.arr - T2.arr, MIN = -2., MAX = 2., INTER = .2, STYLE = 'so0so', FORMAT = '(f4.1)' $
         , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, boxzoom = [floor(gdept[ind]), ceil(gdept[ind])], /noerase,  _extra = ex
  ENDIF ELSE BEGIN
    title = title+ ' - Levitus'
    plt, T1.arr - TLev.arr, MIN = -5., MAX = 5., INTER = .5, STYLE = 'so0so', FORMAT = '(I2)'  $
         , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, boxzoom = [floor(gdept[ind]), ceil(gdept[ind])], /noerase, _extra = ex   
  ENDELSE
;
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end
