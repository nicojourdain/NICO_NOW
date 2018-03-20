pro std_plot_sst, T1, T2, TRey, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_SST_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1

  if std_file1_T EQ std_file2_T then begin
    T = T1.arr[*, *, 0]
    min = -2.
    max = 32.
    inter = 1. 
    STYLE = 0
    title = 'SST!C'+std_file1_T
  ENDIF ELSE BEGIN 
    T = T1.arr[*, *, 0] - T2.arr[*, *, 0]
    min =  -1.
    max = -min
    inter = 0.1
    STYLE = 'so0so'
    title = 'SST!C'+std_file1_T+' - '+std_file2_T
  ENDELSE 
;
  varunit = T1.unit
; 
  plt, T, MIN = min, MAX = max, INTER = inter, STYLE = STYLE $
       , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, FORMAT = '(I2)', /PORTRAIT, _extra = ex
; 
  title = 'SST!C'+std_file1_T+' - NewReynolds'
  plt, T1.arr[*, *, 0] - TRey.arr, MIN = -8., MAX = 8., INTER = 0.5, STYLE = 'so0so', FORMAT = '(I2)' $
       , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, /NOERASE, _extra = ex
  
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end
