pro std_ts_all, doplot = doplot, _extra=ex

  compile_opt idl2, strictarrsubs

@common
@std_common

  PRINT, ''
  PRINT, ' 	############################################'
  PRINT, ''
  PRINT, '                   plot of all TIME SERIES'
  PRINT, ''
  PRINT, ' 	############################################'
  PRINT, ''
;
  std_iodir_data    = isadirectory(getenv('DIR_DATA'), title = 'path of data in NetCdf format')
  std_iodir_climato = isadirectory(getenv('DIR_CLIMATO'), title = 'path of climatological data')
  std_iodir_mask    = isadirectory(getenv('DIR_MASK'), title = 'path of mask files (ex: subbasins)')
; meshmask
  std_file_mesh = isafile(getenv('FILE_MESH_MASK'), title = 'mesh_mask', iodir = std_iodir_mask)
  std_file_msksub = isafile(getenv('FILE_MASK_SUBDOMAIN'), title = 'sub-bassin masks', iodir = std_iodir_mask)
; load the grid
  load_orca, std_file_mesh  
; reading variables
  masknp = read_ncdf('tmaskutil', file = std_file_mesh, /nostruct, /cont_nofill)

  date1   = long(getenv('DATE1'))     &   date2   = long(getenv('DATE2'))
  date1_2 = long(getenv('DATE1_2'))   &   date2_2 = long(getenv('DATE2_2'))

  allrec =  1 - keyword_set(long(getenv('READ_ONLY_FIRST_RECORD')))

;#########################################################################
;######################  STANDARD PLOTS   ################################
;#########################################################################

  IF keyword_set(doplot) EQ 0 THEN doplot = 0

; fixed color tabled
  lct, 64
  cnt = 0
  htmltxt = ''
;
  cnt = cnt+1   &   blabla = 'Global Mean Temperature'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_ts_T, masknp, POSTSCRIPT = postscript, _extra = ex
;
  cnt = cnt+1   &   blabla = 'Global Mean Salinity'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_ts_S, masknp, POSTSCRIPT = postscript, _extra = ex
;
  cnt = cnt+1   &   blabla = 'Global Mean SSH'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_ts_SSH, masknp, POSTSCRIPT = postscript, _extra = ex
;
  cnt = cnt+1   &   blabla = 'Global Mean Q net'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_ts_Q, masknp, POSTSCRIPT = postscript, _extra = ex
;
  cnt = cnt+1   &   blabla = 'Global Mean EMP'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_ts_EMP, masknp, POSTSCRIPT = postscript, _extra = ex
;
  cnt = cnt+1   &   blabla = 'Drake Transport'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_ts_Drake, masknp, POSTSCRIPT = postscript, _extra = ex
;
  cnt = cnt+1   &   blabla = 'Max AMOC'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_ts_AMOC, masknp, POSTSCRIPT = postscript, _extra = ex
;
  cnt = cnt+1   &   blabla = 'Sea-Ice cover'
  IF doplot EQ cnt OR doplot EQ 0 THEN std_ts_ICE, masknp, POSTSCRIPT = postscript, _extra = ex

  IF n_elements(htmltxt) GT 1 THEN putfile, psdir+'std_ts_html_body.txt', htmltxt[1:*]

  return
END
