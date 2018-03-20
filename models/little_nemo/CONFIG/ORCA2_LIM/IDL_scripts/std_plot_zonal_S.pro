pro std_plot_zonal_S, S1, S2, SLev, SUBBASIN = subbasin, POSTSCRIPT = postscript, _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  if KEYWORD_SET(SUBBASIN) then subname = subbasin else subname = "Glo"
  filename = cdti3 + '_ZonalS_'+subname+'_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1

; looking for longitudinal index corresponding to the highest latitude (closest to the North Pole)
  index = where(gphit eq max(gphit))
  nx = index[0] mod jpi
  gphi_save = gphit
  gphit[0, *] = gphit[nx, *]

  if KEYWORD_SET(SUBBASIN) then begin
    tmasksv = tmask
    CASE 1 of 
      subbasin eq 'Atl' : var = 'atlmsk_nomed'
      subbasin eq 'Ind' : var = 'indmsk_nored'
      subbasin eq 'Pac' : var = 'pacmsk'
    ENDCASE
    msk = read_ncdf( var, filename = std_file_msksub, _extra = ex)
; from 2D array to 3D array
    msk = msk.arr[*]#replicate(1., nzt)
    tmask[firstxt:lastxt, firstyt:lastyt, firstzt:lastzt] = tmask[firstxt:lastxt, firstyt:lastyt, firstzt:lastzt] * msk
  endif
;
  title = 'Salinity, '+subname+'!C'+std_file1_T
  pltz, S1, MININ = 32., MAXIN = 37., INTER = .2, typein = 'yz', FORMAT = '(f4.1)' $ 
        , small = [1, 2, 1], COAST_THICK = 2, TITLE = title, boxzoom = 5500, ZOOM = 1000, /PORTRAIT,  _extra = ex 
; 
  if std_file1_T NE std_file2_T then begin
    title = title+' - '+std_file2_T
    pltz, S1.arr-S2.arr, MIN = -1., MAX = 1., INTER = .1, typein = 'yz', FORMAT = '(f4.1)', style = 'so0so' $
          , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, boxzoom = 5500, ZOOM = 1000, /NOERASE,  _extra = ex    
  endif else begin
    title = title+' - Levitus'
    pltz, S1.arr-SLev.arr, MININ = -1, MAXIN = 1., INTER = .1, typein = 'yz', FORMAT = '(f4.1)', style = 'so0so'  $
          , small = [1, 2, 2], COAST_THICK = 2, TITLE = title, boxzoom = 5500, ZOOM = 1000, /NOERASE, _extra = ex
  endelse
;
  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps
;
  if KEYWORD_SET(SUBBASIN) then tmask = tmasksv
  gphit = gphi_save

  return
end
