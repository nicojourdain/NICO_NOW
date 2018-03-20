pro std_plot_ArcSal, S1, S2in, SLevin, Z100 = z100, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  CASE n_params() OF
    2:BEGIN
      Slev = S2in
    END
    3:BEGIN
      IF S2in.arr[0] EQ -1 THEN return
      S2 = S2in
      Slev = Slevin
    END
  ENDCASE
;
  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
;
  IF keyword_set(z100) THEN tmp = min(abs(100 - gdept), ind) ELSE ind = 0
  sdepref = strtrim(round(gdept[ind]), 1)+'m'
  filename = cdti3 + '_Arctic_Sal'+sdepref+'_'+std_file1_T
  IF keyword_set(S2) THEN filename = filename + '_' + std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
; 
  domdef, 20, 380, 60, 90
;
  varunit = S1.unit
  titleorg = 'Salinity ('+sdepref+')!C'
;
  IF keyword_set(S2) THEN BEGIN
    title = titleorg+std_file1_T+ ' - '+std_file2_T
    plt, S1.arr - S2.arr, MIN = -4., MAX = 4., INTER = 0.2, CELL_FILL = 2,  STYLE = 'so0so', format = '(f4.1)'  $     
         , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, CHARSIZE = -0.55, GLINETHICK = 2. $      
         , /ORTHO, MAP = [90, 0, 0], LATDEL = 5, boxzoom = [ind, ind], /zindex, /portrait, _extra = ex
  ENDIF ELSE BEGIN 
    title = titleorg+std_file1_T
    plt, S1, MIN = 30.0, MAX = 36., INTER = 0.2, CELL_FILL = 2, format = '(f4.1)' $
         , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, CHARSIZE = -0.55, GLINETHICK = 2. $
         , /ORTHO, MAP = [90, 0, 0], LATDEL = 5, boxzoom = [ind, ind], /zindex, /PORTRAIT, _extra = ex
  ENDELSE 
; 
  IF keyword_set(S2) THEN BEGIN
    title = titleorg+std_file2_T+ ' - Levitus'
    tmp = S2.arr - SLev.arr
  ENDIF ELSE BEGIN 
    title = titleorg+std_file1_T+ ' - Levitus'
    tmp = S1.arr - SLev.arr
  ENDELSE
  plt, temporary(tmp), MIN = -4., MAX = 4., INTER = 0.2, CELL_FILL = 2,  STYLE = 'so0so', format = '(f4.1)'  $     
       , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, CHARSIZE = -0.55, GLINETHICK = 2. $      
       , /ORTHO, MAP = [90, 0, 0], LATDEL = 5, boxzoom = [ind, ind], /zindex, /NOERASE, _extra = ex
;
  domdef
;
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps
  
  return
end
