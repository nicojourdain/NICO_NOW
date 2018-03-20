pro std_plot_qnet,  Q1, Q2, QNET, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_Qnet_'+std_file1_T
  if std_file1_T EQ std_file2_T then filename = filename + '_OAFlux' $
  ELSE filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
  
  title = 'Qnet!C'+std_file1_T
  plt,  Q1,  MIN = -200., MAX = 200., INTER = 20., STYLE = 'so0so' $
        , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, FORMAT = '(I4)', /PORTRAIT, _extra = ex

  if std_file1_T EQ std_file2_T then begin
    title = 'Qnet!C'+std_file1_T+' - OAFlux'
    Q = Q1.arr - QNET.arr
  ENDIF ELSE BEGIN 
    title = 'Qnet!C'+std_file1_T+' - '+std_file2_T
    Q = Q1.arr - Q2.arr
  ENDELSE 
  plt, Q, MIN = -100, MAX = 100, INTER = 10, STYLE = 'so0so'  $
       , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, FORMAT = '(I4)', /NOERASE, _extra = ex
                                ;
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end

