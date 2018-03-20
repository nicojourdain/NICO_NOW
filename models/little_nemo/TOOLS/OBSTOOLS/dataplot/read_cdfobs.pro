;+---------------------------------------------------------------------------
PRO read_cdfobs, Files, NumObs=NumObs, $
               Latitudes=Latitudes, Longitudes=Longitudes, Depths=Depths, $
               Obs=Observations, ModelVals=ModelVals, qcs=QCs, $
               Ob2=Observations2, ModelVal2=ModelVals2, qc2=QCs2, $
               Ob3=Observations3, $
               Dates=Dates, rmdi=rmdi, iobs=iobs, jobs=jobs, kobs=kobs, $
	            Salinity=Salinity, nodates=nodates, types=types, $
	            filetype=ObsType, quiet=quiet, MDT=MDT, $
               ProfileNum=ProfileNum, error=error, $
               notfussy=notfussy,VarName=VarName  
;+--------------------------------------------------------------------------
; Read in observation and feedback files
; detects filetype and calls the appropriate reading routine
;
; Author:  D. J. Lea       Feb 2008
;+--------------------------------------------------------------------------

; Declare error label. 
ON_IOERROR, IOERROR 
	       
error=0
title=''

; Set types to undefined
types=-1 & tempvar=size(temporary(types))

; Read in netcdf data file and observation operator
;2. Work out which type of data is in the files by looking at the first one.
ncid = ncdf_open(Files(0), /nowrite)
if ncid lt 0 then message, 'Error opening file '+File
result = NCDF_ATTINQ( ncid, 'title', /global)
if result.datatype ne 'UNKNOWN' then ncdf_attget, ncid, 'title', Title, /global

if string(Title) eq "NEMO observation operator output" then ObsType='feedback' $
else $
if string(Title) eq "Forecast class 4 file" then ObsType='ForecastClass4' $
else $
ObsType = 'none'

if ObsType eq 'none' then begin
  varid = ncdf_varid(ncid, 'POTM_CORRECTED')  
  if varid ge 0 then ObsType = 'Prof' 
  
  varid = ncdf_varid(ncid, 'SLA')
  if varid ge 0 then ObsType = 'SSH'
  
  varid = ncdf_varid(ncid, 'SST')
  varid2 = ncdf_varid(ncid, 'sea_surface_temperature')
  varid=max([varid,varid2])
  if varid ge 0 then ObsType = 'SST'
  
  varid = ncdf_varid(ncid, 'SEAICE')
  varid2 = ncdf_varid(ncid, 'sea_ice_concentration')
  varid=max([varid,varid2])
  if varid ge 0 then ObsType = 'SEAICE'    
  
  varid = ncdf_varid(ncid, 'LOGCHL')
  varid2 = ncdf_varid(ncid, 'LogChl')
  varid3 = ncdf_varid(ncid, 'CHL1_mean')
  varid=max([varid,varid2,varid3])
  if varid ge 0 then ObsType = 'LOGCHL'    
  
  varid = ncdf_varid(ncid, 'PCO2')
  if varid ge 0 then ObsType = 'PCO2'

  varid = ncdf_varid(ncid, 'UCRT')
  varid2 = ncdf_varid(ncid, 'VCRT')
  varid=max([varid,varid2])
  if varid ge 0 then ObsType = 'CRT'    
endif
 
ncdf_close, ncid

if not keyword_set(quiet) then print, 'Reading in data as type ',ObsType

  if ObsType eq 'feedback' then begin
  
    if n_elements(DepRange) eq 0 then DepRange=[0,5000]  
  
    read_feedback, Files, DepRange=DepRange, VarName=VarName, NumData=numobs, $
                OutLats=Latitudes, OutLons=Longitudes, $
                OutInstruments=Instruments, OutPlatform=Platform, $
                OutDeps=Depths,  $
                OutObs1=Observations, OutObs2=Observations2, $
                OutMod1=ModelVals, OutMod2=ModelVals2, $
                OutQC1=QCs, OutQC2=QCs2, $
                MDT=MDT, OutDates=Dates, $
                rmdi=rmdi, quiet=quiet, $
                OutProfileNum=ProfileNum
                
;                info, instruments
;                info, platform
                
                types=Platform
                if n_elements(instruments) gt 0 then types=Platform+' '+Instruments
                
  print, 'Varname: ',VarName
  endif else if ObsType eq 'Prof' then begin
  
    if keyword_set(PlotModel) then begin
      OutTMod = 1
      OutSMod = 1
    endif

	if n_elements(DepRange) eq 0 then DepRange=[0,5000]
    
  read_enact, Files, DepRange=DepRange, NumData=NumData, $
                OutLats=Latitudes, OutLons=Longitudes, $
                Instruments=Instruments, Platform=Platform, $
                OutDeps=Depths, OutSObs=OutSObs, OutSQC=OutSQC, $
                OutTObs=OutTObs, OutTQC=OutTQC, OutDates=Dates, $
                OutTMod=OutTMod, OutSMod=OutSMod, rmdi=rmdi, quiet=quiet, $
                iobs=iobs, jobs=jobs, kobs=kobs, $
                ProfileNum=ProfileNum

	types=Instruments+' '+Platform

; if salinity keyword is set then put salinity values first  
    if keyword_set(Salinity) then begin		
      Observations = OutSObs
      ModelVals    = OutSMod    
      QCs          = OutSQC
      Observations2 = OutTObs
      ModelVals2    = OutTMod
      QCs2          = OutTQC
    endif else begin
      Observations = OutTObs
      ModelVals    = OutTMod
      QCs          = OutTQC
      Observations2 = OutSObs
      ModelVals2    = OutSMod
      QCs2          = OutSQC
    endelse 

	numobs=numdata
        numobs=n_elements(latitudes)
	              
  endif else if ObsType eq 'SSH' then begin
    
    read_sla, Files, NumObs=NumObs, $
              Latitudes=Latitudes, Longitudes=Longitudes, $
              ObsSLA=Observations, ModSLA=ModelVals, SLAQC=QCs, $
              MDT=MDT, Satellites=Satellites, types=types, Dates=Dates, rmdi=rmdi, $
	      iobs=iobs, jobs=jobs, mstp=mstp, quiet=quiet
    Depths = fltarr(NumObs)
    
  endif else if ObsType eq 'SST' then begin
     
    read_sst, Files, NumObs=NumObs, $
              Latitudes=Latitudes, Longitudes=Longitudes, $
              ObsSST=Observations, ModSST=ModelVals, SSTQC=QCs, $
              Dates=Dates, rmdi=rmdi, types=types, iobs=iobs, jobs=jobs, $
              quiet=quiet
    Depths = fltarr(NumObs)

  endif else if ObsType eq 'SEAICE' then begin
  
    read_seaice, Files, NumObs=NumObs, $
              Latitudes=Latitudes, Longitudes=Longitudes, $
              Obs=Observations, Modarr=ModelVals, QC=QCs, $
              Dates=Dates, rmdi=rmdi, iobs=iobs, jobs=jobs, nodates=nodates, $
              quiet=quiet
    Depths = fltarr(NumObs)
  
  endif else if ObsType eq 'LOGCHL' then begin
      
    read_chl, Files, NumObs=NumObs, $
              Latitudes=Latitudes, Longitudes=Longitudes, $
              Obs=Observations, Modarr=ModelVals, QC=QCs, $
              Dates=Dates, rmdi=rmdi, iobs=iobs, jobs=jobs, nodates=nodates, $
              quiet=quiet
    Depths = fltarr(NumObs)
             
   endif else if ObsType eq 'PCO2' then begin
      
    read_pco2, Files, NumObs=NumObs, $
              Latitudes=Latitudes, Longitudes=Longitudes, $
              Obs=Observations, Modarr=ModelVals, QC=QCs, $
              Dates=Dates, rmdi=rmdi, iobs=iobs, jobs=jobs, nodates=nodates, $
              quiet=quiet
    Depths = fltarr(NumObs)

   endif else if ObsType eq 'CRT' then begin

    read_crt, Files, NumObs=NumObs, $
              OutLats=Latitudes, OutLons=Longitudes, $
              OutTypes=Types, OutDates=Dates, $
              OutUObs=Observations, OutVObs=Observations2, $
              OutUMod=ModelVals, OutVMod=ModelVals2, Quiet=Quiet, $
              FloatNum=FloatNum, QC=QCs, OutSpeed=Observations3
    rmdi=0.0
    QCs2=QCs
    Depths = fltarr(NumObs)

   endif else if ObsType eq 'ForecastClass4' then begin
   
    print, 'reading forecast class 4 files'
    read_forc_class4, Files, NumObs=NumObs, $
              Latitudes=Latitudes, Longitudes=Longitudes, $
              Depths=Depths, $
              Types=types, Dates=Dates, $
              Obsarr=Observations, Obs2arr=Observations2, $
              Modarr=ModelVals, Mod2arr=ModelVals2, $
              QCs1=QCs, QCs2=QCs2, rmdi=rmdi, $
              notfussy=notfussy
                  
  endif else message, 'Error: ObsType is not set correctly'                 

  if (n_elements(types) eq 0) then begin
  	  types=replicate(rmdi,NumObs)
  endif

  goto, NOERROR
  
IOERROR: error=1
	 print,'read_cdfobs: an error occurred trying read files: ',files

NOERROR:
	
END
