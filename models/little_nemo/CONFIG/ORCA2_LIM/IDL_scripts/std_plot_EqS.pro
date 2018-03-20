pro std_plot_EqS, S1, S2, SLev, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_EqS_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
  
  title = 'Equatorial Salinity!C'+std_file1_T
  pltz, S1, MININ = 33., MAXIN = 37., INTER = .2, typein = 'xz', FORMAT = '(f4.1)'  $
        , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, boxzoom = [20., 380., -1., 1., 0., 500.], /PORTRAIT, _extra = ex
  
  if std_file1_T EQ std_file2_T then begin
    S = S1.arr - Slev.arr
    title = title+' - Levitus'
  ENDIF ELSE BEGIN 
    S = S1.arr - S2.arr
    title = title+' - '+std_file2_T
  ENDELSE
  
  pltz, S, MININ = -1., MAXIN = 1., INTER = 0.1, typein = 'xz', STYLE = 'so0so', FORMAT = '(f4.1)' $
        , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, boxzoom = [20., 380., -1., 1., 0., 500.], /noerase, _extra = ex

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end

