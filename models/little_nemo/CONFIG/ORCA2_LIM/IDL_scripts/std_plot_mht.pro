PRO std_plot_domht, Q, masknp, mask_filename, ibce, htr, htr_atl

  compile_opt idl2, strictarrsubs

@common  

  Qave = moyenne(Q, 'xy', mask2d = masknp) ; mean value
  Qnet = Q - Qave
  ibce = STRTRIM(Qave, 1)
; 
  msk = read_ncdf('atlmsk_nomed', filename = mask_filename, /nostruct)
  msk = msk[*]#replicate(1., nzt) ; from 2D array to 3D array 
                                
; *1.E-15 to have PetaWatt
  Qx     = moyenne(Qnet*e2t    , 'x', mask2d = masknp, /integration)*1.E-15
  Qx_atl = moyenne(Qnet*e2t*msk, 'x', mask2d = masknp, /integration)*1.E-15
; northward heat flux transport from antartic  
  htr     = total(Qx, /cumulative)
  htr_atl = total(reverse(Qx_atl), /cumulative) ; ! from north to south
  htr_atl = -reverse(htr_atl)
; we take from values north till 30° South
  htr_atl[where(gphit[0, *] lt -30.)] = !Values.F_NaN 

return
END

pro std_plot_mht, Q1, Q2, masknp, mask_filename, POSTSCRIPT = postscript,  _extra = ex

  compile_opt idl2, strictarrsubs

@common  
@std_common

  cdti3 = string(cnt, format = '(i3.3)')
  print, cdti3 + ') ' + blabla
  filename = cdti3 + '_MHT_'+std_file1_T
  if std_file1_T NE std_file2_T then filename = filename + '_'+std_file2_T
  if KEYWORD_SET(postscript) then openps, filename+'.ps', portrait = 1
  
; find the x index with the highest latitude (for the plot)
  index = where(gphit eq max(gphit))
  nx = index[0] mod jpi
  gphi_save = gphit
  gphit[0, *] = gphit[nx, *]
; compute meridional heat transport
  std_plot_domht, Q1, masknp, mask_filename, ibce1, htr1, htr1_atl
  
; update data informations
  varname = 'MHT'
  varunit = 'Pw'
  vargrid = 'T' 

  title = 'MHT (Black) & Atlantic MHT (Blue)!C'+std_file1_T
  subtitle = '(Qnet='+ibce1+' W/m2)'
  plt1d, htr1, 'y', min = -2., max = 2.5, TITLE = title, SUBTITLE = subtitle, /portrait $ 
         , SMALL = [2-(std_file1_T EQ std_file2_T), 2, 1], YTITLE = 'PW',  XGRIDSTYLE = 2, TICKLEN = 1, YGRIDSTYLE = 2, _extra = ex
  plt1d, htr1_atl, 'y', COLOR = 50, /ov1d,  _extra = ex

  if std_file1_T NE std_file2_T then begin
; compute meridional heat transport
    std_plot_domht, Q2, masknp, mask_filename, ibce2, htr2, htr2_atl

    title = 'MHT (Black) & Atlantic MHT (Blue)!C'+std_file2_T
    subtitle = '(Qnet='+ibce2+' W/m2)'
    plt1d, htr2, 'y', min = -2., max = 2.5, TITLE = title, SUBTITLE = subtitle, /noerase $ 
           , SMALL = [2, 2, 2], YTITLE = 'PW',  XGRIDSTYLE = 2, TICKLEN = 1, YGRIDSTYLE = 2, _extra = ex
    plt1d, htr2_atl, 'y', COLOR = 50, /ov1d,  _extra = ex

    varunit = 'Tw'
    title = 'MHT (TeraW) differences!C'+std_file1_T+' - '+std_file2_T
    plt1d, (htr1-htr2)*1.e3, 'y', min = -250., max = 250., TITLE = title $
           , SMALL = [2, 2, 3], TICKLEN = 1, XGRIDSTYLE = 2 $
           , YTITLE = 'TW', YGRIDSTYLE = 2, /NOERASE, _extra = ex
    plt1d, (htr1_atl - htr2_atl)*1.e3, 'y', COLOR = 50, /ov1d,  _extra = ex
    
  endif 

  htmltxt = [ htmltxt, '<hr>'+blabla, '<br><img width="80%" src='+filename+'.png  />  ' ]
  if KEYWORD_SET(postscript) then closeps
  
  gphit = gphi_save
  
  return
end
