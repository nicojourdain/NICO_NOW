pro std_plot_sss, S1, S2, SLev, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_SSS_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
  
  varunit = S1.unit  
  
  title = 'SSS!C'+std_file1_T
  plt, S1.arr[*, *, 0], MIN = 33., MAX = 41., INTER = .25, format = '(f4.1)' $
       , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, /PORTRAIT, _extra = ex 
                                ;
  if std_file1_T NE std_file2_T then begin
    title = 'SSS!C'+std_file1_T+' - '+std_file2_T
    plt, S1.arr[*, *, 0] - S2.arr[*, *, 0], MIN = -2., MAX = 2., INTER = 0.2, STYLE = 'so0so' $
         , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, /noerase, format = '(f4.1)', _extra = ex
  endif else begin
    title = 'SSS!C'+std_file1_T+' - Levitus'
    plt, S1.arr[*, *, 0] - SLev.arr[*, *, 0], MIN = -3., MAX = 3., INTER = 0.2, STYLE = 'so0so' $
         , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, /noerase, format = '(f4.1)', _extra = ex
  endelse
                                ;
   htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end
