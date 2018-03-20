pro std_plot_erp, ERP1, ERP2, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_Erp_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
  
  title = 'Erp!C'+std_file1_T
  plt, ERP1,  MIN = -5., MAX = 5., INTER = .5, style = 'so0so' $
       , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, /PORTRAIT, format = '(i2)', _extra = ex
  
  if std_file1_T NE std_file2_T then begin
    title = title+' - '+std_file2_T
    plt, ERP1.arr - ERP2.arr, MIN = -2., MAX = 2., INTER = .25, style = 'so0so' $
         , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, format = '(f4.1)', /NOERASE, _extra = ex
  endif

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end

