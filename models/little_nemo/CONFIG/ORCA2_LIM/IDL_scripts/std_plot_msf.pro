pro std_plot_msf, V1, V2, SUBBASIN = subbasin, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  if KEYWORD_SET(SUBBASIN) then subname = subbasin else subname = "Glo"
  filename = cdti3 + '_MSF_'+subname+'_'+std_file1_V
  if std_file1_V NE std_file2_V then filename = filename + '_'+std_file2_V
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
;
  if KEYWORD_SET(SUBBASIN) then begin
    CASE subname of 
      'Atl' : var = 'atlmsk_nomed'
      'Ind' : var = 'indmsk'
      'IndoPac' : var = 'indpacmsk'
      'GloNoMed' : var = 'glomsk_nomed'
    ENDCASE
    msk = read_ncdf( var, filename = std_file_msksub, /nostruct, _extra = ex )
  endif else msk = tmask[*, *, 0]
;
  CASE subname OF
    'GloNoMed':lat_ext = [-80, 90]
    'Glo':lat_ext = [-80, 90]
    'Atl':lat_ext = [-30, 90]
    'Ind':lat_ext = [-30, 30]
    'IndoPac':lat_ext = [-30, 70]
  ENDCASE
  
  domdef, 0, 6000

  mm1 = msf(V1.arr, msk, indexboxzoom = ind, maskout = ma)
  title = 'Meridional Stream Function, '+subname+'!C'+std_file1_V
  pltz, mm1, 'yz', -20., 20., int = 1., boxzoom = [ind[0:1], lat_ext, 0, 5500], /xindex, FORMAT = '(I3)', /portrait $
        , small = [1, 2, 1], COAST_THICK = 2, zoom = 5500, maskdta = ma, /no_partial, TITLE = title, style = 'so0so'

  if std_file1_V NE std_file2_V then begin
    mm2 = msf(V2.arr, msk, indexboxzoom = ind, maskout = ma)
    title = title+' - '+std_file2_V
    pltz, mm1.arr-mm2.arr, 'yz', -10., 10., int = 1., boxzoom = [ind[0:1], lat_ext, 0, 5500], /xindex,  FORMAT = '(I3)'  $
          , small = [1, 2, 2], COAST_THICK = 2, zoom = 5500, maskdta = ma, /no_partial, TITLE = title, /NOERASE, style = 'so0so'
  endif
  
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps

  return
end

