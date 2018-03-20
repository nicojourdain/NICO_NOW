PRO read_enact, Files, DepRange=DepRange, $
                NumData=NumData, $
                OutLats=OutLats, OutLons=OutLons, Instruments=OutInst, $
		Platform=OutPlatform, $
                OutDeps=OutDeps, OutSObs=OutSObs, OutSQC=OutSQC, $
                OutTObs=OutTObs, OutTQC=OutTQC, OutDates=OutDates, $
                OutTMod=OutTMod, OutSMod=OutSMod, rmdi=rmdi, quiet=quiet, $
                iobs=outiobs, jobs=outjobs, kobs=outkobs, $
                ProfileNum=OutProfileNum

;------------------------------------------------------------------------------------------
;Program to read in data from an ENACT format file containing profile data.
;
; Inputs:
;        File        => File name to be read in
;        DepRange    => (optional) Range of depths for which the data is to be extracted.
;
; Outputs:
;        NumProfs    => Number of profiles  -Integer
;        Latitudes   => Latitudes(NumProfs) -Real
;        Longitudes  => Longitudes(NumProfs) -Real
;        Dates       => Dates(NumProfs)  -Date/time structure.
;        Instruments => Instrument type (NumProfs) -String
;        Depths      => Depths of each ob (NumProfs)  -Real
;        Salinity    => Salinity (psu) (NumProfs)  -Real
;        SalQC       => QC flag for salinity (NumProfs) 1=> Good  -Int
;        Temperature => Temperature (deg C) (NumProfs)  -Real
;        TempQC      => QC flag for temperature (NumProfs) 1=> Good  -Int
;        ModelT      => Model temperature (deg C) (NumProfs)  -Real
;        ModelS      => Model salinity (psu) (NumProfs)  -Real
;        rmdi        => Missing data indicator  -Real
;
;Author: Matt Martin. 5/10/07.
;------------------------------------------------------------------------------------------
rmdi = 99999.
if NOT keyword_set(DepRange) then DepRange=[0.,1.e1]   ;Default to top 10 metres.
NumFiles=n_elements(Files)

ifile2=0
for ifile = 0, NumFiles-1 do begin
  File = Files(ifile)
  
;1. Open the netcdf file
  ncid = ncdf_open(File, /nowrite)
  if ncid lt 0 and not keyword_set(quiet) then print, 'Error opening file '+File
  if ncid ge 0 then begin
  if not keyword_set(quiet) then print, 'Opened file '+File+' successfully'
  
  ;2. Get info about file
  ncinfo = ncdf_inquire(ncid)
;  if not keyword_set(quiet) then , ncinfo
  ;3. Read in the relevant dimensions
  for idim = 0, ncinfo.ndims-1 do begin
    ncdf_diminq, ncid, idim, name, dimlen
    if name eq 'N_PROF' then NumProfs = dimlen $
    else if name eq 'N_LEVELS' then NumLevels = dimlen $
    else if name eq 'STRING4' then str4 = dimlen
  endfor
 
  ;4. Set up arrays and read in the data

  if (NumProfs gt 0) then begin  
  JulDays = fltarr(NumProfs)
  Latitudes = fltarr(NumProfs)
  Longitudes = fltarr(NumProfs)
  BytInsts = intarr(4, NumProfs)
  Instruments = strarr(NumProfs)
  Platforms = strarr(NumProfs)
  Instrumentsa = strarr(NumLevels, NumProfs)
  Platformsa = strarr(NumLevels, NumProfs)
  Depths = fltarr(NumLevels, NumProfs)
  Salinity = fltarr(NumLevels, NumProfs)
  SalQC = intarr(NumLevels, NumProfs)
  Temperature = fltarr(NumLevels, NumProfs)
  TempQC = intarr(NumLevels, NumProfs)
  ModelT = fltarr(NumLevels, NumProfs)
  ModelS = fltarr(NumLevels, NumProfs)
  ProfileNum = replicate(1,NumLevels)#indgen(NumProfs)
    
  varid = ncdf_varid(ncid, 'JULD')
  ncdf_varget, ncid, varid, JulDays
  
;  print,JulDays
  
;  BaseDate = var_to_dt(1950,1,1)
;  BaseDate=JULDAY(1,1,1950,0,0)
  BaseDate=double(JULDAY(1,1,1950,0,0))		;should be at 0 UTC
;  info,BaseDate
  Dates = replicate(BaseDate, NumLevels,NumProfs)
;  info,dates
  for iprof = 0L, NumProfs-1 do begin
    secs = JulDays(iprof)* 24. * 60. * 60.
;    dt = dt_add(BaseDate, second=secs)
    dt=BaseDate+JulDays(iprof)
;    CALDAT, dt, mon, day, year, hour, minute, second
;    print,dt-BaseDate, mon, day, year, hour, minute, second
    Dates(0:NumLevels-1,iprof) = replicate(dt,NumLevels)
  endfor

  
 
  varid = ncdf_varid(ncid, 'LONGITUDE')
  ncdf_varget, ncid, varid, Longitudes

  varid = ncdf_varid(ncid, 'LATITUDE')
  ncdf_varget, ncid, varid, Latitudes

  varid = ncdf_varid(ncid, 'DEPH_CORRECTED')
  ncdf_varget, ncid, varid, Depths
  res = ncdf_attinq( ncid, varid , '_FillValue')
  if res.length gt 0 and res.datatype ne "UNKNOWN" then ncdf_attget, ncid, varid, '_FillValue', FillVal $
  else begin
    res = ncdf_attinq( ncid, varid , '_fillvalue')
    ncdf_attget, ncid, varid, '_fillvalue', FillVal
  endelse
  pts = where(Depths eq FillVal, count)
  if count gt 0 then Depths(pts) = rmdi

;  if keyword_set(Salinity) then begin
    varid = ncdf_varid(ncid, 'PSAL_CORRECTED')
    if (varid ne -1) then  begin
;    if not keyword_set(quiet) then print,'reading psal_corrected'
    ncdf_varget, ncid, varid, Salinity
    res = ncdf_attinq( ncid, varid , '_FillValue')
    if res.length gt 0 and res.datatype ne "UNKNOWN" then ncdf_attget, ncid, varid, '_FillValue', FillVal $
    else begin
      res = ncdf_attinq( ncid, varid , '_fillvalue')
      ncdf_attget, ncid, varid, '_fillvalue', FillVal
    endelse
    pts = where(Salinity eq FillVal, count)
    if count gt 0 then Salinity(pts) = rmdi
   endif      
    varid = ncdf_varid(ncid, 'PSAL_CORRECTED_QC')
    ncdf_varget, ncid, varid, SalQC
    res = ncdf_attinq( ncid, varid , '_FillValue')
    if res.length gt 0 and res.datatype ne "UNKNOWN" then ncdf_attget, ncid, varid, '_FillValue', FillVal $
    else begin
      res = ncdf_attinq( ncid, varid , '_fillvalue')
      ncdf_attget, ncid, varid, '_fillvalue', FillVal
    endelse
    SalQC = SalQC - 48
;  endif

;  if keyword_set(Temperature) then begin
    varid = ncdf_varid(ncid, 'POTM_CORRECTED')
    if (varid ne -1) then  begin
;    if not keyword_set(quiet) then print,'reading potm_corrected'
    ncdf_varget, ncid, varid, Temperature
    res = ncdf_attinq( ncid, varid , '_FillValue')
    if res.length gt 0 and res.datatype ne "UNKNOWN" then ncdf_attget, ncid, varid, '_FillValue', FillVal $
    else begin
      res = ncdf_attinq( ncid, varid , '_fillvalue')
      ncdf_attget, ncid, varid, '_fillvalue', FillVal
    endelse
;    if not keyword_set(quiet) then print,'Temp fill value ',fillval
    pts = where(Temperature eq FillVal, count)
    if count gt 0 then Temperature(pts) = rmdi
   endif   
    varid = ncdf_varid(ncid, 'POTM_CORRECTED_QC')
    ncdf_varget, ncid, varid, TempQC
    TempQC = TempQC - 48
;   endif

;  if keyword_set(OutTMod) then begin
    varid = ncdf_varid(ncid, 'POTM_Hx')
    if (varid ne -1) then begin
    ncdf_varget, ncid, varid, ModelT
    res = ncdf_attinq( ncid, varid , '_FillValue')
    if res.length gt 0 and res.datatype ne "UNKNOWN" then ncdf_attget, ncid, varid, '_FillValue', FillVal $
    else begin
      res = ncdf_attinq( ncid, varid , '_fillvalue')
      ncdf_attget, ncid, varid, '_fillvalue', FillVal
    endelse
    pts = where(ModelT eq FillVal, count)
    if count gt 0 then ModelT(pts) = rmdi
    endif else begin
	ModelT=rmdi
    endelse
;   endif

;  if keyword_set(OutSMod) then begin
    varid = ncdf_varid(ncid, 'PSAL_Hx')
    if (varid ne -1) then begin
    ncdf_varget, ncid, varid, ModelS
    res = ncdf_attinq( ncid, varid , '_FillValue')
    if res.length gt 0 and res.datatype ne "UNKNOWN" then ncdf_attget, ncid, varid, '_FillValue', FillVal $
    else begin
      res = ncdf_attinq( ncid, varid , '_fillvalue')
      ncdf_attget, ncid, varid, '_fillvalue', FillVal
    endelse
    pts = where(ModelS eq FillVal, count)
    if count gt 0 then ModelS(pts) = rmdi
    endif else begin
	ModelS=rmdi
    endelse
;  endif

  varid = ncdf_varid(ncid, 'WMO_INST_TYPE')
  ncdf_varget, ncid, varid, BytInsts
  
  pts = where(BytInsts(0,*) eq 32 and $
              BytInsts(1,*) eq 56 and $
              BytInsts(2,*) eq 50 and $
              BytInsts(3,*) eq 48, count)
  if count gt 0 then Instruments(pts) = 'BUOYS'  ; 820
  
  pts = where(BytInsts(0,*) eq 32 and $
              BytInsts(1,*) eq 52 and $
              BytInsts(2,*) eq 48 and $
              BytInsts(3,*) eq 49, count)
  if count gt 0 then Instruments(pts) = 'XBT'    ; 401
  
  pts = where(BytInsts(0,*) eq 32 and $
              BytInsts(1,*) eq 55 and $
              BytInsts(2,*) eq 52 and $
              BytInsts(3,*) eq 49, count)
  if count gt 0 then Instruments(pts) = 'TESAC'  ; 741
  
  pts = where(BytInsts(0,*) eq 32 and $
              BytInsts(1,*) eq 56 and $
              BytInsts(2,*) eq 51 and $
              BytInsts(3,*) eq 49, count)
  if count gt 0 then Instruments(pts) = 'ARGO'   ; 831
  
  pts = where(Instruments eq '', count) 
  if count gt 0 then Instruments(pts) = 'UNKNOWN'
  
  varid = ncdf_varid(ncid, 'PLATFORM_NUMBER')
  ncdf_varget, ncid, varid, platforms
  platforms=string(platforms)
  
  for i=0,numlevels-1 do begin
    platformsa(i,*)=platforms
    instrumentsa(i,*)=instruments
  endfor

  varid = ncdf_varid(ncid, 'IOBS')
  if (varid ne -1) then ncdf_varget, ncid, varid, iobsval

  varid = ncdf_varid(ncid, 'JOBS')
  if (varid ne -1) then ncdf_varget, ncid, varid, jobsval

  varid = ncdf_varid(ncid, 'KOBS')
  if (varid ne -1) then ncdf_varget, ncid, varid, kobsval
    
  ;Select those obs in the required depth range
;  Lats = replv(Latitudes(*), [NumLevels,NumProfs],1)
;  Lons = replv(Longitudes(*), [NumLevels,NumProfs],1)

	Lons=fltarr(NumLevels,NumProfs)
        Lats=fltarr(NumLevels,NumProfs)
	iobs=fltarr(NumLevels,NumProfs)
        jobs=fltarr(NumLevels,NumProfs)
        
	for i=0L,NumProfs-1 do begin
          Lats(*,i)=Latitudes(i)
          Lons(*,i)=Longitudes(i)
          if (n_elements(iobsval) gt 0) then $
	          iobs(*,i)=iobsval(i)
          if (n_elements(jobsval) gt 0) then $
          	  jobs(*,i)=jobsval(i)
        endfor  
  
  pts = where(Depths ge DepRange(0) and Depths le DepRange(1), NumData)
  
  if ifile2 eq 0 then begin  
    OutTObs = [Temperature(pts)]
    OutSObs = [Salinity(pts)]
    OutTMod = [ModelT(pts)]
    OutSMod = [ModelS(pts)]
    OutTQC  = [TempQC(pts)]
    OutSQC  = [SalQC(pts)]
    OutDeps = [Depths(pts)]
    OutLats = [Lats(pts)]
    OutLons = [Lons(pts)]
    OutDates= [Dates(pts)]
    OutInst= [Instrumentsa(pts)]    
    OutPlatform= [Platformsa(pts)]
    OutProfileNum = [ProfileNum(pts)]
    if (n_elements(iobsval) gt 0) then outiobs = [iobs(pts)]
    if (n_elements(jobsval) gt 0) then outjobs = [jobs(pts)]
    if (n_elements(kobsval) gt 0) then outkobs = [kobsval(pts)]
    
  endif else begin
    OutTObs = [OutTObs,Temperature(pts)]
    OutSObs = [OutSObs,Salinity(pts)]
    OutTMod = [OutTMod,ModelT(pts)]
    OutSMod = [OutSMod,ModelS(pts)]
    OutTQC  = [OutTQC,TempQC(pts)]
    OutSQC  = [OutSQC,SalQC(pts)]
    OutDeps = [OutDeps,Depths(pts)]
    OutLats = [OutLats,Lats(pts)]
    OutLons = [OutLons,Lons(pts)]
    OutDates= [OutDates,Dates(pts)]
    OutInst= [OutInst,Instrumentsa(pts)]
    OutPlatform = [OutPlatform, Platformsa(pts)]
    OutProfileNum = [OutProfileNum, ProfileNum(pts)]
    if (n_elements(iobsval) gt 0) then outiobs = [outiobs, iobs(pts)]
    if (n_elements(jobsval) gt 0) then outjobs = [outjobs, jobs(pts)]
    if (n_elements(kobsval) gt 0) then outkobs = [outkobs, kobsval(pts)]

  endelse

	ifile2=ifile2+1
   endif
  ncdf_close, ncid


	endif
  
endfor ;ifile

END
