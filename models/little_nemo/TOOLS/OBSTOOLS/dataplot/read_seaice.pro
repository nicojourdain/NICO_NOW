PRO  read_seaice, Files, NumObs=NumObs, $
               Latitudes=Latitudes, Longitudes=Longitudes, $
               Obs=Obs, modarr=modarr, qc=qcarr, $
               Dates=Dates, rmdi=rmdi, iobs=outiobs, jobs=outjobs, $
               nodates=nodates, quiet=quiet
;------------------------------------------------------------
;IDL program to read in netcdf files of sea ice data.
;
;Author:   D. J. Lea       Feb 2008
;
;------------------------------------------------------------
rmdi = -99999.
NumFiles=n_elements(Files)
;RefDate = '1950-01-01'
;!DATE_SEPARATOR='-'
RefDate=JULDAY(1,1,1950,0,0)		; should be at 0 UTC

; could read in from file

ifile2=0
for ifile = 0, NumFiles-1 do begin
;------------------------------------------------------------
;1. Open the file containing the data
  ncid = ncdf_open(Files(ifile), /nowrite)
  if ncid lt 0 and not keyword_set(quiet) then print, 'Error opening file '+Files(ifile)
  if ncid ge 0 then begin
  if not keyword_set(quiet) then print, 'Opened file '+Files(ifile)+' successfully'

;------------------------------------------------------------
;2. Read in the dimensions in the file
  ncinfo = ncdf_inquire(ncid)
  ncdf_diminq, ncid, 0, name, NData
  if Ndata gt 0 then begin

;------------------------------------------------------------
;3. Allocate the data arrays and read in the data
  lons   = dblarr(NData)
  lats   = dblarr(NData)
  obsval = fltarr(NData)
  modval = fltarr(NData)
  bytQC  = bytarr(NData)  
  QC     = fltarr(NData)
  Dats   = dblarr(NData)
  dts    = replicate(!dt_base, NData)
  iobs = intarr(NData)
  jobs = intarr(NData)

; output attribute and variable info
;  ncattinfo, id
  
  varid = ncdf_varid(ncid, 'lon')
;  if not keyword_set(quiet) then print,varid
  if (varid eq -1) then varid = ncdf_varid(ncid, 'LONGITUDE')
  ncdf_varget, ncid, varid, lons
  
  varid = ncdf_varid(ncid, 'lat')
;  if not keyword_set(quiet) then print,varid
  if (varid eq -1) then varid = ncdf_varid(ncid, 'LATITUDE')
  ncdf_varget, ncid, varid, lats

  if (keyword_set(nodates) eq 0) then begin

  varid = ncdf_varid(ncid, 'JULD')
  if varid ne -1 then begin
    ncdf_varget, ncid, varid, Dats
    dts = Dats+RefDate
  endif else begin
    varid = ncdf_varid(ncid, 'time')
    ncdf_varget, ncid, varid, secs_from_base
    varid = ncdf_varid(ncid, 'SeaIce_dtime')
    ncdf_varget, ncid, varid, dtime
    ncdf_attget, ncid, varid, 'scale_factor', scale_factor    
    if not keyword_set(quiet) then print,'dtime(0): ',dtime(0), scale_factor
    dtime=dtime*scale_factor
;    RefDate = '1981-01-01'
    RefDate = JULDAY(1,1,1981,0,0)	; should be ref from 0 UTC
    dtime = dtime + secs_from_base
    dts = RefDate + dtime/86400.
  endelse

  endif

  if not keyword_set(quiet) then print,'reading sea ice data'   
  varid = ncdf_varid(ncid, 'sea_ice_concentration')
    if not keyword_set(quiet) then print,varid
    if (varid eq -1) then begin
    	 varid2 = ncdf_varid(ncid, 'SEAICE')
         if (varid2 ne -1) then ncdf_varget, ncid, varid2, obsval
    endif
    if (varid ne -1) then ncdf_varget, ncid, varid, obsval
    if not keyword_set(quiet) then print,'scale_factor'
  scale_factor=1.  
  if (varid ne -1) then ncdf_attget, ncid, varid, 'scale_factor', scale_factor

  if not keyword_set(quiet) then print,'_FillValue'
  FillValue=99999
      
;  ncdf_attget, ncid, varid, '_FillValue', FillValue
  if not keyword_set(quiet) then print,FillValue  

  pts = where(obsval eq FillValue, count)

  if not keyword_set(quiet) then print,'scale_factor ',scale_factor  
  obsval=obsval*scale_factor
  
  if count gt 0 then obsval(pts) = rmdi


  if not keyword_set(quiet) then print,'reading sea ice model values'
  varid = ncdf_varid(ncid, 'SEAICE_Hx')
  if (varid ne -1) then ncdf_varget, ncid, varid, modval

  varid = ncdf_varid(ncid, 'IOBS')
  if (varid ne -1) then ncdf_varget, ncid, varid, iobs
  
  varid = ncdf_varid(ncid, 'JOBS')
  if (varid ne -1) then ncdf_varget, ncid, varid, jobs
  

  scale_factor=1
  pts = where(modval eq FillValue or modval eq -9999, count)
  
  modval=modval*scale_factor
  
  if count gt 0 then modval(pts) = rmdi

  	if not keyword_set(quiet) then print,'sea ice qc'

  varid = ncdf_varid(ncid, 'SEAICE_QC')	     
  if (varid ne -1) then begin
   if not keyword_set(quiet) then print,'bytQC' 
   ncdf_varget, ncid, varid, bytQC
   ncdf_attget, ncid, varid, '_FillValue', FillValue
   QC(*) = 0.
   pts = where(bytQC eq FillValue, count)
   if count gt 0 then QC(pts) = rmdi
   pts = where(bytQC ne 48, count)
   if count gt 0 then QC(pts) = 1.  
  endif else begin
   varid = ncdf_varid(ncid, 'confidence_flag')
   ncdf_varget, ncid, varid, QC
  endelse
  
  if ifile2 eq 0 then begin
    Latitudes = [float(lats)]
    Longitudes = [float(lons)]
    Obs = [obsval]
    Modarr = [modval]
    QCarr = [QC]
    Dates = [dts]
    if (n_elements(iobs) gt 0) then outiobs = [iobs]
    if (n_elements(jobs) gt 0) then outjobs = [jobs]
  endif else begin
    Latitudes = [Latitudes, float(lats)]
    Longitudes = [Longitudes, float(lons)]
    Obs = [Obs, obsval]
    Modarr = [Modarr, modval]    
    QCarr = [QCarr, QC]
    Dates = [Dates, dts]
    if (n_elements(iobs) gt 0) then outiobs = [outiobs, iobs]
    if (n_elements(jobs) gt 0) then outjobs = [outjobs, jobs]
  endelse      

ifile2=ifile2+1
endif

  if not keyword_set(quiet) then print,'closing file'

  ncdf_close, ncid

endif
endfor ; ifile  
  
NumObs = n_elements(Latitudes)

if (n_elements(Modarr) ne NumObs) then Modarr=replicate(rmdi,NumObs)

END
