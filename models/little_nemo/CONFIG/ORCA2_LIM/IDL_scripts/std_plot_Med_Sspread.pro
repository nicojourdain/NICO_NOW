pro std_plot_Med_Sspread, S1, S2, SLev, depth, POSTSCRIPT = postscript,  _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  tmp = min(abs(depth - gdept), ind)
  sdepref = strtrim(round(gdept[ind]), 1)+'m'
  filename = cdti3 + '_Med_Sspread_'+sdepref+'_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
;  
  title = 'Salinity ('+sdepref+')!C'+std_file1_T
  plt, S1, MININ = 35., MAXIN = 37., INTER = 0.1, FORMAT = '(f4.1)' $
       , small = [1, 2, 1], boxzoom = [270, 365, 5, 70, ind, ind], /zindex, COAST_THICK = 2, TITLE = title, /PORTRAIT, _extra = ex
;
  if std_file1_T NE std_file2_T then begin
    title = title+ ' - '+std_file2_T
    plt, S1.arr - S2.arr, MININ = -.4, MAXIN = .4, INTER = .05, STYLE = 'so0so', FORMAT = '(f4.1)'  $
         , small = [1, 2, 2], boxzoom = [270, 365, 5, 70, ind, ind], /zindex, COAST_THICK = 2, TITLE = title, /noerase, _extra = ex
  endif else begin
    title = title+ ' - Levitus'
    plt, S1.arr - SLev.arr, MININ = -1., MAXIN = 1., INTER = 0.1, STYLE = 'so0so', FORMAT = '(f4.1)'  $
         , small = [1, 2, 2], boxzoom = [270, 365, 5, 70, ind, ind], /zindex, COAST_THICK = 2, TITLE = title, /noerase, _extra = ex
  ENDELSE

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end
