pro std_plot_mld, MLD1, MLD2in, MLDin, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  CASE n_params() OF
    2:BEGIN
      MLD = MLD2in
    END
    3:BEGIN
      IF MLD2in.arr[0] EQ -1 THEN return
      MLD2 = MLD2in
      MLD = MLDin
    END
  ENDCASE
;
  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_MLD_'+std_file1_T
  IF keyword_set(MLD2) THEN filename = filename + '_' + std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
;
  varunit = MLD1.unit
  titleorg = 'MLD!C'
;
  IF keyword_set(MLD2) THEN BEGIN
    title = titleorg+std_file1_T+ ' - '+std_file2_T
    plt, MLD1.arr - MLD2.arr, MIN = -80., MAX = 80., INTER = 10., FORMAT = '(I3)'  $
         , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, /NOCONTOUR, /PORTRAIT, _extra = ex
  ENDIF ELSE BEGIN 
    title = titleorg+std_file1_T
    plt, MLD1, MIN = 0., MAX = 500., INTER = 25., /NOCONTOUR, FORMAT = '(I3)' $
         , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, /PORTRAIT, _extra = ex
  ENDELSE 
; 
  IF keyword_set(MLD2) THEN BEGIN
    title = titleorg+std_file2_T+ ' - DeBoyer'
    tmp = MLD2.arr - MLD.arr
  ENDIF ELSE BEGIN 
    title = titleorg+std_file1_T+ ' - DeBoyer'
    tmp = MLD1.arr - MLD.arr
  ENDELSE
  plt, temporary(tmp), MIN = -80., MAX = 80., INTER = 10., FORMAT = '(I3)'  $
       , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, /NOCONTOUR, /NOERASE, _extra = ex
;  
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps
;
  return
end

