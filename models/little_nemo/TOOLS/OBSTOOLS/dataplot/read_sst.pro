PRO  read_sst, Files, NumObs=NumObs, $
               Latitudes=Latitudes, Longitudes=Longitudes, $
               ObsSST=ObsSST, ModSST=ModSST, SSTQC=SSTQC, $
               Dates=Dates, rmdi=rmdi, Types=Types, iobs=iobs, jobs=jobs, $
               quiet=quiet
;------------------------------------------------------------
;IDL program to read in netcdf files of altimeter data.
;
;Author:  D. J. Lea        - Feb 2008
;
;------------------------------------------------------------
rmdi = -99999.
NumFiles=n_elements(Files)
;print,NumFiles
;RefDate = '1950-01-01'
;!DATE_SEPARATOR='-'
RefDate=JULDAY(1,1,1950,0,0)    ; should be at 0 UTC
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

  if ncinfo.Ndims eq 1 then begin
    ncdf_diminq, ncid, 0, name, NData
  endif else if ncinfo.Ndims eq 2 then begin
    ncdf_diminq, ncid, 1, name, NData
    if (name eq "string8") then  ncdf_diminq, ncid, 0, name, NData
  endif else if ncinfo.Ndims eq 3 then begin
    ncdf_diminq, ncid, 1, name, NLats
    ncdf_diminq, ncid, 2, name, NLons
    NData = NLats * NLons
  endif
    
  if not keyword_set(quiet) then print, 'Number of SST data points ',NData
  
  if (NData gt 0) then begin
;------------------------------------------------------------
;3. Allocate the data arrays and read in the data
  lons   = dblarr(NData)
  lats   = dblarr(NData)
  obsval = fltarr(NData)
  modval = fltarr(NData)
  bytQC  = bytarr(NData)
  QC     = fltarr(NData)
  obstyp = intarr(NData)  
  Dats   = dblarr(NData)
  dts    = replicate(!dt_base, NData)
  iobsa = intarr(NData)
  jobsa = intarr(NData)
  
  varid = ncdf_varid(ncid, 'LONGITUDE')
  if varid ne -1 then ncdf_varget, ncid, varid, lons $
  else begin
    varid = ncdf_varid(ncid, 'lon')
    ncdf_varget, ncid, varid, lons
  endelse
  
  varid = ncdf_varid(ncid, 'LATITUDE')
  if varid ne -1 then ncdf_varget, ncid, varid, lats $
  else begin
    varid = ncdf_varid(ncid, 'lat')
    ncdf_varget, ncid, varid, lats
  endelse
  
  varid = ncdf_varid(ncid, 'JULD')
  if varid ne -1 then begin
    ncdf_varget, ncid, varid, Dats
    dts=RefDate + Dats
    
  endif else begin
    varid = ncdf_varid(ncid, 'time')
    ncdf_varget, ncid, varid, secs_from_base
    varid = ncdf_varid(ncid, 'sst_dtime')
    ncdf_varget, ncid, varid, dtime
    ncdf_attget, ncid, varid, 'scale_factor', scale_factor
    dtime=dtime*scale_factor
;    RefDate = '1981-01-01'
    RefDate = JULDAY(1,1,1981,0,0)	; should be ref from 0 UTC
    dtime = dtime + secs_from_base
    dts = RefDate + dtime/86400.
  endelse
        
  varid = ncdf_varid(ncid, 'SST')
  if varid ne -1 then begin
    ncdf_varget, ncid, varid, obsval
    obsval=float(obsval)        ;ensure obsval is a floating point array
    ncdf_attget, ncid, varid, '_FillValue', FillValue
    pts = where(obsval eq FillValue, count)
    if count gt 0 then obsval(pts) = rmdi
  endif else begin
    varid = ncdf_varid(ncid, 'sea_surface_temperature')
    ncdf_varget, ncid, varid, obsval
    obsval=float(obsval)	;ensure obsval is a floating point array
    ncdf_attget, ncid, varid, '_FillValue', FillValue
    ncdf_attget, ncid, varid, 'add_offset', Offset
    ncdf_attget, ncid, varid, 'scale_factor', Scale
    pts = where(obsval ne FillValue, count)
    if count gt 0 then obsval(pts) = Offset + (obsval(pts) *Scale)
    pts = where(obsval eq FillValue, count)
    if count gt 0 then obsval(pts) = rmdi
  endelse

  varid = ncdf_varid(ncid, 'SST_Hx')
  if varid ne -1 then begin
    ncdf_varget, ncid, varid, modval
    ncdf_attget, ncid, varid, '_FillValue', FillValue
    pts = where(modval eq FillValue, count)
    if count gt 0 then modval(pts) = rmdi
  endif
      
  varid = ncdf_varid(ncid, 'SST_QC')
  if varid ne -1 then begin
    ncdf_varget, ncid, varid, bytQC
    ncdf_attget, ncid, varid, '_FillValue', FillValue
    QC(*) = 0.
    pts = where(bytQC eq FillValue, count)
    if count gt 0 then QC(pts) = rmdi
    pts = where(bytQC ne 48, count)
    if count gt 0 then QC(pts) = bytQC(pts)-48
  endif else begin
    varid = ncdf_varid(ncid, 'confidence_flag')
    ncdf_varget, ncid, varid, bytQC
    QC = float(bytQC)
  endelse
      
  varid = ncdf_varid(ncid, 'SST_DATA_SOURCE')
  if varid ne -1 then ncdf_varget, ncid, varid, obstyp $
  else begin
    varid = ncdf_varid(ncid, 'data_source')
    if varid ne -1 then ncdf_varget, ncid, varid, obstyp
  endelse

  varid = ncdf_varid(ncid, 'callsign')
  if varid ne -1 then begin 
  	ncdf_varget, ncid, varid, callsign
        tmp=strtrim(string(obstyp),2)+' '+strtrim(string(callsign),2)
        obstyp=tmp
  endif else begin
   varid = ncdf_varid(ncid, 'SST_CALL_SIGN')
   if varid ne -1 then begin 
      ncdf_varget, ncid, varid, callsign
        tmp=strtrim(string(obstyp),2)+' '+strtrim(string(callsign),2)
        obstyp=tmp
   endif
  endelse

  varid = ncdf_varid(ncid, 'IOBS')
  if (varid ne -1) then ncdf_varget, ncid, varid, iobsa
  
  varid = ncdf_varid(ncid, 'JOBS')
  if (varid ne -1) then ncdf_varget, ncid, varid, jobsa
      
  if ifile2 eq 0 then begin
    Latitudes = [float(lats)]
    Longitudes = [float(lons)]
    ObsSST = [obsval]
    ModSST = [modval]
    SSTQC = [QC]
    Dates = [dts]
    Types = [obstyp]
    iobs = [iobsa]
    jobs = [jobsa]
  endif else begin
    Latitudes = [Latitudes, float(lats)]
    Longitudes = [Longitudes, float(lons)]
    ObsSST = [ObsSST, obsval]
    ModSST = [ModSST, modval]
    SSTQC = [SSTQC, QC]
    Dates = [Dates, dts]    
    Types = [Types, obstyp]
    iobs = [iobs, iobsa]
    jobs = [jobs, jobsa]       
  endelse      

  ifile2=ifile2+1
  endif
  ncdf_close, ncid

  endif

endfor ; ifile  
  
NumObs = n_elements(Latitudes)

END
