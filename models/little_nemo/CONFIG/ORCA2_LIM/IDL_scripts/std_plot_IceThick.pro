pro std_plot_IceThick, Ithi1, Ithi2, ARC = arc, ANT = ant, MARCH = march, SEPT = sept, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla

  var = 'IceThi'
  IF keyword_set(arc) THEN var = var+'_Arc_'
  IF keyword_set(ant) THEN var = var+'_Ant_'
  IF keyword_set(march) THEN var = var+'March'
  IF keyword_set(sept) THEN var = var+'Sept'

  filename = cdti3 + '_'+var+'_'+std_file1_I
  if std_file1_I NE std_file2_I then filename = filename + '_'+std_file2_I
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
  
  IF keyword_set(arc) THEN BEGIN 
    domdef, 20, 380, 50, 90
    vmin = 0.
    vmax = 10.
    vint = 0.5
    fmt = '(i2)'
    vmind = -3.
    vmaxd = 3.
    vintd = 0.5
    fmtd = '(i2)'
    map = [90, 0, 0]
  ENDIF
  IF keyword_set(ant) THEN BEGIN 
    domdef, 20, 380, -90, -50
    vmin = 0.
    vmax = 3.
    vint = 0.2
    fmt = '(f4.1)'
    vmind = -1.
    vmaxd = 1.
    vintd = 0.2
    fmtd = '(f4.1)'
    map = [-90, 0, 0]
  ENDIF
;
  varunit = Ithi1.unit
;
  title = var+'!C'+std_file1_I
  plt, (Ithi1.arr < 10. ) - 1.E-04, MIN = vmin, MAX = vmax, INTER = vint, /STRICTFILL, CELL_FILL = 2, format = fmt $
       , small = [1, 2, 1], COAST_THICK = 2, TITLE = title $
       , CHARSIZE = 1.05, GLINETHICK = 2., /ORTHO, MAP = map, /PORTRAIT, _extra = ex
;                                ;
  if std_file1_I NE std_file2_I then begin            
    title = title + std_file2_I
    plt, Ithi1.arr - Ithi2.arr, MIN = vmind, MAX = vmaxd, INTER = vintd, STYLE = 'so0so', format = fmtd $
         , small = [1, 2, 2], COAST_THICK = 2, CELL_FILL = 2, TITLE = title $
         , CHARSIZE = 1.05, GLINETHICK = 2., /ORTHO, MAP = map, /NOERASE, _extra = ex
  endif                        
  
  domdef

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end
