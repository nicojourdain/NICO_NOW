pro std_plot_emp, EMP1, EMP2, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_Emp_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
  
  title = 'Emp!C'+std_file1_T
  plt, EMP1,  MIN = -10., MAX = 10., INTER = 1., style = 'so0so' $
       , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, format = '(i3)', /PORTRAIT, _extra = ex
  
  if std_file1_T NE std_file2_T then begin
    title = title+' - '+std_file2_T
    plt, EMP1.arr - EMP2.arr, MIN = -5., MAX = 5., INTER = 1., style = 'so0so' $
         , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, format = '(i3)', /NOERASE, _extra = ex
  endif

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end

