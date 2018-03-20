pro std_plot_S100m, S1, S2, SLev, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  tmp = min(abs(100 - gdept), ind)
  sdepref = strtrim(round(gdept[ind]), 1)+'m'
  filename = cdti3 + '_S'+sdepref+'_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
;
  title = 'Salinity ('+sdepref+')!C'+std_file1_T
  plt, S1, MIN = 33., MAX = 38.2, INTER = .2, format = '(f4.1)' $
       , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, boxzoom = [floor(gdept[ind]), ceil(gdept[ind])], /PORTRAIT, _extra = ex
;
  if std_file1_T NE std_file2_T then begin
    title = title+ ' - '+std_file2_T
    plt, S1.arr[*, *, 0] - S2.arr[*, *, 0], MIN = -1., MAX = 1., INTER = .1, STYLE = 'so0so', format = '(f4.1)' $
         , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, boxzoom = [floor(gdept[ind]), ceil(gdept[ind])], /noerase, _extra = ex   
  endif else begin
    title = title+ ' - Levitus'
    plt, S1.arr-SLev.arr, MIN = -1., MAX = 1., INTER = 0.1, STYLE = 'so0so', format = '(f4.1)' $
         , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, boxzoom = [floor(gdept[ind]), ceil(gdept[ind])], /noerase, _extra = ex
  endelse

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end
