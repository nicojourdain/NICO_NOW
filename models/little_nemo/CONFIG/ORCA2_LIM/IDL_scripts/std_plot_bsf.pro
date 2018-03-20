pro std_plot_bsf, U1, U2, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_BSF_'+std_file1_U
  if std_file1_U NE std_file2_U then filename = filename + '_'+std_file2_U
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
  
  domdef, 0, 6000

  bb1 = bsf(U1.arr, refvalue = 0., refpoint = [25, 0])
  title = 'Barotropic Stream Function!C'+std_file1_U
  plt, bb1, min = -200., max = 200., int = 10., /portrait, FORMAT = '(I4)', STYLE = 'so0so' $
       , small = [1, 2, 1], COAST_THICK = 2, TITLE = title,  _extra = ex

  if std_file1_U NE std_file2_U then BEGIN
    bb2 = bsf(U2.arr, refvalue = 0., refpoint = [25, 0])
    title = 'Barotropic Stream Function!C'+std_file1_U+' - '+std_file2_U
    plt, bb1.arr - bb2.arr, min = -20., max = 20., int = 2.,  FORMAT = '(I2)', STYLE = 'so0so' $
         , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, /NOERASE, _extra = ex
  endif
  
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end

