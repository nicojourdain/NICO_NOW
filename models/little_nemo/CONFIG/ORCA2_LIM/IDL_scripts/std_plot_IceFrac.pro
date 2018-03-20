pro std_plot_IceFrac, Ifra1, Ifra2, ARC = arc, ANT = ant, MARCH = march, SEPT = sept, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla

  var = 'IceFra'
  IF keyword_set(arc) THEN var = var+'_Arc_'
  IF keyword_set(ant) THEN var = var+'_Ant_'
  IF keyword_set(march) THEN var = var+'March'
  IF keyword_set(sept) THEN var = var+'Sept'

  filename = cdti3 + '_'+var+'_'+std_file1_I
  if std_file1_I NE std_file2_I then filename = filename + '_'+std_file2_I
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
  
  IF keyword_set(arc) THEN BEGIN 
    domdef, 20, 380, 50, 90
    map = [90, 0, 0]
  ENDIF
  IF keyword_set(ant) THEN BEGIN 
    domdef, 20, 380, -90, -50
    map = [-90, 0, 0]
  ENDIF
;
  varunit = Ifra1.unit
;
  title = var+'!C'+std_file1_I
  plt, (Ifra1.arr < 1. ) - 1.E-04, MIN = 0., MAX = 1., INTER = .1, /STRICTFILL, CELL_FILL = 2, format = '(f4.1)' $
       , small = [1, 2, 1], COAST_THICK = 2, TITLE = title $
       , CHARSIZE = 1.05, GLINETHICK = 2., /ORTHO, MAP = map, /PORTRAIT, _extra = ex
;                                ;
  if std_file1_I NE std_file2_I then begin            
    title = title + std_file2_I
    plt, Ifra1.arr - Ifra2.arr, MIN = -1, MAX = 1, INTER = .1, STYLE = 'so0so', format = '(f4.1)' $
         , small = [1, 2, 2], COAST_THICK = 2, CELL_FILL = 2, TITLE = title $
         , CHARSIZE = 1.05, GLINETHICK = 2., /ORTHO, MAP = map, /NOERASE, _extra = ex
  endif                        
  
  domdef

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end
