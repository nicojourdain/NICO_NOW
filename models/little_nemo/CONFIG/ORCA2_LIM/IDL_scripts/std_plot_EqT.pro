pro std_plot_EqT, T1, T2, TLev, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_EqT_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
  
  title = 'Equatorial Temperature!C'+std_file1_T
  pltz, T1, MININ = 2., MAXIN = 30., INTER = 1., typein = 'xz', FORMAT = '(I2)'  $
        , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, boxzoom = [20., 380., -1., 1., 0., 500.], /PORTRAIT, _extra = ex
  
  if std_file1_T EQ std_file2_T then begin
    T = T1.arr - TLev.arr
    title = title+' - Levitus'
    min = -4.
    max = -min
    inter = .5
    fmt = '(I2)'
  ENDIF ELSE BEGIN 
    T = T1.arr - T2.arr
    title = title+' - '+std_file2_T
    min =  -2.
    max = -min
    inter = .25
    fmt = '(f4.1)'
  ENDELSE 
  
  pltz, T, MININ = min, MAXIN = max, INTER = inter, typein = 'xz', STYLE = 'so0so', FORMAT = fmt $
        , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, boxzoom = [20., 380., -1., 1., 0., 500.], /NOERASE, _extra = ex
                                ; 
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end

