pro std_plot_EqU, U1, U2, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_EqU_'+std_file1_U
  if std_file1_U NE std_file2_U then filename = filename + '_'+std_file2_U
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
  
  title = 'Equatorial Zonal Current!C'+std_file1_U
  pltz, U1, MININ = -1., MAXIN = 1., INTER = .1, typein = 'xz', STYLE = 'so0so', FORMAT = '(f4.1)' $
        , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, boxzoom = [20., 380., -1., 1., 0., 500.], ZOOM = 500, /PORTRAIT, _extra = ex

  if std_file1_U NE std_file2_U then begin
    title = title+' - '+std_file2_U
    pltz, U1.arr-U2.arr, MININ = -.5, MAXIN = .5, INTER = .1, typein = 'xz', STYLE = 'so0so', FORMAT = '(f4.1)' $
          , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, boxzoom = [20., 380., -1., 1., 0., 500.], ZOOM = 500, /PORTRAIT, _extra = ex
  endif
  
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end
