pro std_plot_ZonMld, MLD1, MLD2, MLD, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_ZonMld_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
                                ;
  title = std_file1_T+' (Black)!C'
  if std_file1_T NE std_file2_T THEN title = title + std_file2_T+' (Red)!C'
  title = title + 'DeBoyer (Blue)!C'

  plt1d, -MLD1.arr, MIN = -300., MAX = 0., INTER = 10., typein = 'y' $
         , small = [1, 1, 1], boxzoom = 5500, ZOOM = 500, CHARSIZE = .8, TITLE = title, /PORTRAIT, _extra = ex 
                                ;
  IF std_file1_T NE std_file2_T THEN $
     plt1d, -MLD2.arr, MIN = -300., MAX = 0., INTER = 10., typein = 'y' $
            , /ov1d, COLOR = 250, TITLE = title, /NOERASE, _extra = ex ; color 250 = red
                                ;
  plt1d, -MLD.arr, MIN = -300., MAX = 0., INTER = 10., typein = 'y' $
         ,  /ov1d, COLOR = 100, THICK = 3, TITLE = title, /NOERASE, _extra = ex ; color 100 = blue

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end
