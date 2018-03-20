;+
PRO  read_sla, Files, NumObs=NumObs, $
               Latitudes=Latitudes, Longitudes=Longitudes, $
               ObsSLA=ObsSLA, ModSLA=ModSLA, SLAQC=SLAQC, $
               MDT=MDT, Satellites=Satellites, Types=Types, Dates=Dates, rmdi=rmdi, $
	       iobs=iobs, jobs=jobs, mstp=mstp, quiet=quiet, track=track
;+-----------------------------------------------------------
;IDL program to read in netcdf files of altimeter data.
;
;Author:  D. J. Lea       - Feb 2008
;
;------------------------------------------------------------
rmdi = -99999.
NumFiles=n_elements(Files)
;RefDate = '1950-01-01'
;!DATE_SEPARATOR='-'
RefDate=JULDAY(1,1,1950,0,0)		; should be at 0 UTC

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
;  var = ncdf_varinq( ncid, "SLA" )
  if not keyword_set(quiet) then print,'ncinfo.ndims ',ncinfo.ndims
  ncdf_diminq, ncid, 0, name, NData
  NData2=0
  cycles=1
  if (ncinfo.ndims gt 2) then ncdf_diminq, ncid, 2, name, NData2
  if (ncinfo.ndims gt 1) then ncdf_diminq, ncid, 1, name, cycles


  if not keyword_set(quiet) then print,NData, NData2

 NData=max([NData*cycles, NData2*cycles])

; print,NData, NData2

  if (NData gt 0) then begin
;------------------------------------------------------------
;3. Allocate the data arrays and read in the data
  lons   = dblarr(NData)
  lats   = dblarr(NData)
  obsval = fltarr(NData)
  modval = fltarr(NData)
  mdtval = fltarr(NData)  
  bytQC  = bytarr(NData)
  QC     = fltarr(NData)
  Dats   = dblarr(NData)
  trackval  = intarr(NData)
  Sats   = intarr(NData)
  strSats= strarr(NData)
  StrVal = strarr(8)
;  dts    = replicate(!dt_base, NData)
  dts = dblarr(NData)
  
;  info,lons 
  varid = ncdf_varid(ncid, 'LONGITUDE')
  scale_factor=1.
  if (varid eq -1) then begin 
  	varid = ncdf_varid(ncid, 'Longitudes')
        if (varid eq -1) then begin
             varid = ncdf_varid(ncid, 'longitude')
        endif
        ncdf_attget, ncid, varid, 'scale_factor', scale_factor
  endif
  ncdf_varget, ncid, varid, lons 
  lons=lons*scale_factor
;  info, lons
  if (cycles gt 1) then begin
    lonsn=lons
    for i=1,cycles-1 do begin
       lonsn=[lonsn,lons]
    endfor
    lons=lonsn
  endif

;  info, lons

;  info,lats     
  varid = ncdf_varid(ncid, 'LATITUDE')
  scale_factor=1.
  if (varid eq -1) then begin
  	varid = ncdf_varid(ncid, 'Latitudes')
        if (varid eq -1) then begin
             varid = ncdf_varid(ncid, 'latitude')
        endif
        ncdf_attget, ncid, varid, 'scale_factor', scale_factor
  endif
  ncdf_varget, ncid, varid, lats
  lats=lats*scale_factor
;  info, lats
  if (cycles gt 1) then begin
    latsn=lats
    for i=1,cycles-1 do begin
       latsn=[latsn,lats]
    endfor
    lats=latsn
  endif
  
;  info, lats
    
  varid = ncdf_varid(ncid, 'JULD')
  if (varid eq -1) then begin
        varid = ncdf_varid(ncid, 'time')
  endif
  if (varid ne -1) then begin 
  	ncdf_varget, ncid, varid, Dats
  endif else begin
        varid = ncdf_varid(ncid, 'BeginDates')
        ncdf_varget, ncid, varid, Bds
;        print,bds
;        info, bds
        if (cycles gt 1) then begin
           bds=transpose(bds)
        endif
        varid = ncdf_varid(ncid, 'NbPoints')
        ncdf_varget, ncid, varid, nbpoints

; Read in dataindexes and deltat in cls format file 
; These are used to adjust dates 
        varid = ncdf_varid(ncid, 'DeltaT')
        if (varid ne -1) then begin 
  	  ncdf_varget, ncid, varid, deltat
          ncdf_attget, ncid, varid, 'scale_factor', scale_factor
          deltat=deltat*scale_factor
          varid = ncdf_varid(ncid, 'DataIndexes')
          ncdf_varget, ncid, varid, dataindexes
          
        endif

	print, 'deltat ',deltat
;        print, 'dataindexes ',dataindexes

;        info, nbpoints
;        print,nbpoints
;        info, cycles
        cumbds=[0]
        nbpointst=nbpoints
        dataindexest=dataindexes
        for i=1,cycles-1 do begin
	 	nbpointst=[nbpointst,nbpoints]
                dataindexest=[dataindexest,dataindexes]
        endfor
        cumbds=[0,total(nbpointst,/cumulative)]
;       if not keyword_set(quiet) then print,'cumbds ',cumbds
        nel=n_elements(Bds)
;        info,nel
;       if not keyword_set(quiet) then print,n_elements(dats)
        for i=0,nel-1 do begin
;        print,'*',i, cumbds(i+1)-cumbds(i) 
;           print,i,cumbds(i),cumbds(i+1)-1
           dats(cumbds(i):cumbds(i+1)-1)=bds(i)
           trackval(cumbds(i):cumbds(i+1)-1)=i
           dataindexest(cumbds(i):cumbds(i+1)-1)=dataindexest(cumbds(i):cumbds(i+1)-1)-dataindexest(cumbds(i))
        endfor
        
; adjust dats based on dataindex
	dats=dats+dataindexest*deltat/86400.
        
        
;        print, dats
  endelse


          
  dts=RefDate + Dats 
    
  varid = ncdf_varid(ncid, 'MISSION')
  if (varid ne -1) then begin
  ncdf_varget, ncid, varid, Sats
  ncdf_attget, ncid, varid, 'Value_0', StrVal0           
  ncdf_attget, ncid, varid, 'Value_1', StrVal1  
  ncdf_attget, ncid, varid, 'Value_2', StrVal2  
  ncdf_attget, ncid, varid, 'Value_3', StrVal3      
  ncdf_attget, ncid, varid, 'Value_4', StrVal4  
  ncdf_attget, ncid, varid, 'Value_5', StrVal5 
  ncdf_attget, ncid, varid, 'Value_6', StrVal6    
  ncdf_attget, ncid, varid, 'Value_7', StrVal7
  StrVal=[StrVal0, StrVal1, StrVal2, StrVal3, StrVal4, $
  	StrVal5, StrVal6, StrVal7]    
  for ival = 0, 7 do begin
    pts = where(Sats eq ival, count)
    if count gt 0 then strSats(pts) = StrVal(ival)
  endfor
  endif

  varid = ncdf_varid(ncid, 'SLA')
  ncdf_varget, ncid, varid, obsval
  ncdf_attget, ncid, varid, '_FillValue', FillValue
  scale_factor=1.
  add_offset=0.
  ;if (ncinfo.ndims gt 2) then ncdf_attget, ncid, varid, 'scale_factor', scale_factor
  ncviq=ncdf_varinq(ncid, varid)
  for iatt=0,ncviq.natts-1 do begin
    ncaiq=ncdf_attname(ncid, varid, iatt)
    if ( ncaiq eq 'scale_factor') then ncdf_attget, ncid, varid, 'scale_factor', scale_factor
    if ( ncaiq eq 'add_factor') then ncdf_attget, ncid, varid, 'add_factor', add_factor
  endfor

  if not keyword_set(quiet) then print,'SLA scale factor: ',scale_factor
  if not keyword_set(quiet) then print,'SLA add offset: ',add_offset
  if not keyword_set(quiet) then print,'SLA FillValue: ',FillValue
  pts = where(obsval eq FillValue, count)
;  if not keyword_set(quiet) then print,count
  obsval=obsval*scale_factor+add_offset
  if count gt 0 then obsval(pts) = rmdi
  
  varid = ncdf_varid(ncid, 'SLA_Hx')
  if (varid ne -1) then begin
  ncdf_varget, ncid, varid, modval
  ncdf_attget, ncid, varid, '_FillValue', FillValue
;  if not keyword_set(quiet) then print,'SLA_Hx FillValue ',FillValue
  pts = where(modval eq FillValue or modval eq -9999.0, count)
;  if not keyword_set(quiet) then print,count
  if count gt 0 then modval(pts) = rmdi
  endif
    
  varid = ncdf_varid(ncid, 'SLA_QC')
  if (varid ne -1) then begin
  ncdf_varget, ncid, varid, bytQC
  ncdf_attget, ncid, varid, '_FillValue', FillValue
  QC(*) = 0.
  pts = where(bytQC eq FillValue, count)
  if count gt 0 then QC(pts) = rmdi
  pts = where(bytQC ne 48, count)
  if count gt 0 then QC(pts) = 1.
  endif
  
  varid = ncdf_varid(ncid, 'MDT')
  if (varid ne -1) then ncdf_varget, ncid, varid, mdtval

  varid = ncdf_varid(ncid, 'data_source')
  if varid ne -1 then begin
     ncdf_varget, ncid, varid, obstyp
  endif else begin
     varid = ncdf_varid(ncid, 'MISSION')
     if varid ne -1 then ncdf_varget, ncid, varid, obstyp
  endelse

; reads in multi cycle data the wrong way round!
  if (cycles gt 1) then begin
      obsval=reform(obsval,[cycles,ndata/cycles])
      obsval=transpose(obsval)
      modval=reform(modval,[cycles,ndata/cycles])
      modval=transpose(modval)
      QC=reform(QC,[cycles,ndata/cycles])
      QC=transpose(QC)
  endif

  varid = ncdf_varid(ncid, 'IOBS')
  if (varid ne -1) then ncdf_varget, ncid, varid, iobsval

  varid = ncdf_varid(ncid, 'JOBS')
  if (varid ne -1) then ncdf_varget, ncid, varid, jobsval

  varid = ncdf_varid(ncid, 'MSTP')
  if (varid ne -1) then ncdf_varget, ncid, varid, mstpval

;  info, obssla
;  info, obsval
;  info, modsla
;  info, modval
 
  if ifile2 eq 0 then begin
    Latitudes = [float(lats)]
    Longitudes = [float(lons)]
    ObsSLA = [obsval(*)]
    ModSLA = [modval(*)]
    MDT = [mdtval]
    SLAQC = [QC]
    Dates = [dts]
    Satellites = [strSats]
    if (n_elements(obstyp) gt 0) then Types = [obstyp]
    if (n_elements(iobsval) gt 0) then iobs = [iobsval]
    if (n_elements(jobsval) gt 0) then jobs = [jobsval]
   if (n_elements(mstpval) gt 0) then mstp = [mstpval]
   if (n_elements(trackval) gt 0) then track = [trackval]
  endif else begin
    Latitudes = [Latitudes, float(lats)]
    Longitudes = [Longitudes, float(lons)]
    ObsSLA = [ObsSLA, obsval(*)]
    ModSLA = [ModSLA, modval(*)]
    MDT = [MDT, mdtval]    
    SLAQC = [SLAQC, QC]
    Dates = [Dates, dts]    
    Satellites = [Satellites, strSats]
    if (n_elements(obstyp) gt 0) then Types = [Types, obstyp]
    if (n_elements(iobsval) gt 0) then iobs = [iobs, iobsval]
    if (n_elements(jobsval) gt 0) then jobs = [jobs, jobsval]
    if (n_elements(mstpval) gt 0) then mstp = [mstp, mstpval]
    if (n_elements(trackval) gt 0) then tracks = [track, trackval]
  endelse      

ifile2=ifile2+1
endif
  ncdf_close, ncid

endif

endfor ; ifile  
  
NumObs = n_elements(Latitudes)

END
