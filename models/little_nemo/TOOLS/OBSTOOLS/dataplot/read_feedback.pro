PRO read_feedback, Files, DepRange=DepRange, VarName=VarNames, NumData=NumData, $
                OutLats=OutLats, OutLons=OutLons, $
                OutInstruments=OutInst, OutPlatform=OutPlatform, $
                OutDeps=OutDeps,  $
                OutObs1=OutObs1, OutObs2=OutObs2, $
                OutMod1=OutMod1, OutMod2=OutMod2, $
                OutQC1=OutQC1, OutQC2=OutQC2, $
                MDT=MDT, OutDates=OutDates, $
                rmdi=rmdi, quiet=quiet, $
                OutProfileNum=OutProfileNum

;------------------------------------------------------------------------------------------
;Program to read in data from a feedback format file
;
; Inputs:
;        File        => File name to be read in
;        DepRange    => (optional) Range of depths for which the data is to be extracted.
;
; Outputs:

;
;Author: Matt Martin. 29/09/09
;------------------------------------------------------------------------------------------
rmdi = 99999.
if NOT keyword_set(DepRange) then DepRange=[0.,1.e1]   ;Default to top 10 metres.
NumFiles=n_elements(Files)

ifile2=0
numdata=0
for ifile = 0, NumFiles-1 do begin
  File = Files(ifile)
  
;1. Open the netcdf file
  ncid = ncdf_open(File, /nowrite)
  if ncid lt 0 and not keyword_set(quiet) then print, 'Error opening file '+File
  if ncid ge 0 then begin
  if not keyword_set(quiet) then print, 'Opened file '+File+' successfully'
  
  ;2. Get info about file
  ncinfo = ncdf_inquire(ncid)

  ;3. Read in the relevant dimensions
  NumExtra = 0
  for idim = 0, ncinfo.ndims-1 do begin
    ncdf_diminq, ncid, idim, name, dimlen
    if name eq 'N_OBS' then NumObs = dimlen $
    else if name eq 'N_LEVELS' then NumLevels = dimlen $
    else if name eq 'N_VARS' then NumVars = dimlen $
    else if name eq 'N_QCF' then NumFlags = dimlen $
    else if name eq 'N_ENTRIES' then NumEntries = dimlen $
    else if name eq 'N_EXTRA' then NumExtra = dimlen $
    else if name eq 'STRINGNAM' then strnam = dimlen $
    else if name eq 'STRINGGRID' then strgrid = dimlen $
    else if name eq 'STRINGWMO' then strwmo = dimlen $
    else if name eq 'STRINGTYP' then strtyp = dimlen $
    else if name eq 'STRINGJULD' then strjuld = dimlen
  endfor
 
  ;4. Set up arrays 

  if (NumObs gt 0) then begin  
    ByteNam = intarr(strnam, NumVars)
    if (n_elements(NumEntries) eq 1) then ByteEntries = intarr(strnam, NumEntries)
    ByteId = intarr(strwmo, NumObs)
    ByteType = intarr(strtyp, NumObs)
    ByteJulDRef = intarr(strjuld)
    VarNames = strarr(NumVars)
    if (n_elements(NumEntries) eq 1) then Entries = strarr(NumEntries)
    Id = strarr(NumObs)
    Type = strarr(NumObs)
    if NumExtra gt 0 then begin
      ByteExtra = intarr(strnam, NumExtra)    
      Extra = strarr(NumExtra)    
    endif 
    Longitude = fltarr(NumObs)
    Latitude  = fltarr(NumObs)
    Depth     = fltarr(NumLevels, NumObs)
    DepQC     = intarr(NumLevels, NumObs)
    JulD      = dblarr(NumObs)
    ObsQC     = intarr(NumObs)
    PosQC     = intarr(NumObs)
    JulDQC    = intarr(NumObs)
    Obs       = fltarr(NumLevels, NumObs, NumVars)
    Hx        = fltarr(NumLevels, NumObs, NumVars)
    VarQC     = intarr(NumObs, NumVars)
    LevelQC   = intarr(NumLevels, NumObs, NumVars)
    
  ;5. Read in the data  
    varid = ncdf_varid(ncid, 'VARIABLES')
    ncdf_varget, ncid, varid, ByteNam
;    info, VarNames
;    info, ByteNam
    for ivar= 0, NumVars-1 do begin
      VarNames(ivar) = string(ByteNam(*,ivar))
    endfor
    
    if ifile2 eq 0 then VarName = VarNames(0)
    if VarName ne VarNames(0) then message, 'Can only read in from files containing the same variables'
    
    varid = ncdf_varid(ncid, 'JULD_REFERENCE')
    ncdf_varget, ncid, varid, ByteJulDRef
    JulDRef = string(ByteJulDRef)
    RefDate = JULDAY(fix(strmid(JulDRef,4,2)), fix(strmid(JulDRef,6,2)), fix(strmid(JulDRef,0,4)), $
                     fix(strmid(JulDRef,8,2)), fix(strmid(JulDRef,10,2)), fix(strmid(JulDRef,12,2)))
	print, 'RefDate: ',RefDate
    varid = ncdf_varid(ncid, 'JULD')
    ncdf_varget, ncid, varid, JulD
;    print, JulD
    Dates = dblarr(NumLevels, NumObs)
    for iob = 0L, long(NumObs)-1L do begin
      dt = RefDate + JulD(iob)
;      print,dt, JulD(iob)
      Dates(0:NumLevels-1, iob) = replicate(dt, NumLevels)
    endfor
    
    varid = ncdf_varid(ncid, 'STATION_IDENTIFIER')
    ncdf_varget, ncid, varid, ByteId
    Identifier = string(ByteId)
    
    varid = ncdf_varid(ncid, 'STATION_TYPE')
    ncdf_varget, ncid, varid, ByteType
    Type = string(ByteType)
   
    varid = ncdf_varid(ncid, 'LONGITUDE')
    ncdf_varget, ncid, varid, Longitude
  
    varid = ncdf_varid(ncid, 'LATITUDE')
    ncdf_varget, ncid, varid, Latitude
  
    varid = ncdf_varid(ncid, 'DEPTH')
    ncdf_varget, ncid, varid, Depth
    ncdf_attget, ncid, varid, '_Fillvalue', FillVal
    pts = where(Depth eq FillVal, count)
    if count gt 0 then Depth(pts) = rmdi
    
    varid = ncdf_varid(ncid, 'OBSERVATION_QC')
    ncdf_varget, ncid, varid, ObsQC
    ncdf_attget, ncid, varid, '_Fillvalue', FillVal
    pts = where(ObsQC eq FillVal, count)
    if count gt 0 then ObsQC(pts) = rmdi      
    
    for ivar = 0, NumVars-1 do begin
    
      varid = ncdf_varid(ncid, strtrim(VarNames(ivar))+'_OBS')
      ncdf_varget, ncid, varid, tmp
      Obs(*,*,ivar) = tmp
      ncdf_attget, ncid, varid, '_Fillvalue', FillValObs
  
      varid = ncdf_varid(ncid, strtrim(VarNames(ivar))+'_Hx')
      if (varid gt -1) then begin
         ncdf_varget, ncid, varid, tmp
         Hx(*,*,ivar) = tmp
         ncdf_attget, ncid, varid, '_Fillvalue', FillValHx
      endif else begin
         FillValHx=0
      endelse 
      
      varid = ncdf_varid(ncid, strtrim(VarNames(ivar))+'_QC')
      ncdf_varget, ncid, varid, tmp
      VarQC(*,ivar) = tmp
      ncdf_attget, ncid, varid, '_Fillvalue', FillValQC
      
      
      varid = ncdf_varid(ncid, strtrim(VarNames(ivar))+'_LEVEL_QC')
      ncdf_varget, ncid, varid, tmp
      LevelQC(*,*,ivar) = tmp
      ncdf_attget, ncid, varid, '_Fillvalue', FillValLevQC
          
    endfor        
 
; 	print,' DJL levelqc(*,218,0) ',levelqc(*,218,0)
; 	print,' DJL levelqc(*,218,1) ',levelqc(*,218,1)
    
    pts = where(Obs eq FillValObs, count)
    if count gt 0 then Obs(pts) = rmdi
    pts = where(Hx eq FillValHx, count)
    if count gt 0 then Hx(pts) = rmdi   
    pts = where(VarQC eq FillValQC, count)
    if count gt 0 then VarQC(pts) = rmdi  
    pts = where(LevelQC eq FillValLevQC, count)
    if count gt 0 then LevelQC(pts) = rmdi        

    Obs1 = Obs(*,*,0)
    Hx1  = Hx(*,*,0)
    VarQC1=LevelQC(*,*,0)    
    if NumVars gt 1 then begin
      Obs2 = Obs(*,*,1)
      Hx2  = Hx(*,*,1)
      VarQC2=LevelQC(*,*,1)    
    endif
    
    if strtrim(VarName,2) eq 'SLA' then begin
      MDT = fltarr(NumLevels, NumObs)
      print, NumLevels, NumObs
      varid = ncdf_varid(ncid, 'MDT')
      ncdf_varget, ncid, varid, MDT
      ncdf_attget, ncid, varid, '_Fillvalue', FillVal
      pts = where(MDT eq FillVal, count)
      if count gt 0 then MDT(pts) = rmdi   
      Obs2 = MDT
    endif
          
    ;6. Put the data into the correct form to be output from this routine
    Platformsa = strarr(NumLevels, NumObs)
    Instrumentsa = strarr(NumLevels, NumObs)
    Lons = fltarr(NumLevels,NumObs)
    Lats = fltarr(NumLevels,NumObs)
    ProfileNum = lonarr(NumLevels,NumObs)
        
    for i=0,NumLevels-1 do begin
      Platformsa(i,*)=Type
      Instrumentsa(i,*)=Identifier
    endfor
      
    for i=0L,long(NumObs)-1L do begin
      Lats(*,i)=Latitude(i)
      Lons(*,i)=Longitude(i)
      ProfileNum(*,i) = i+1
    endfor  
;   
   pts = where(Depth ge DepRange(0) and Depth le DepRange(1), NumDataInc)
   NumData=NumData+NumDataInc
;   
   if ifile2 eq 0 then begin  
     OutObs1 = [Obs1(pts)]
     OutMod1 = [Hx1(pts)]
     OutQC1  = [VarQC1(pts)]
     OutDeps = [Depth(pts)]
     OutLats = [Lats(pts)]
     OutLons = [Lons(pts)]
     OutDates= [Dates(pts)]
     OutInst= [Instrumentsa(pts)]    
     OutPlatform= [Platformsa(pts)]
     OutProfileNum = [ProfileNum(pts)]
     if NumVars eq 2 then begin
       OutObs2 = [Obs2(pts)]     
       OutMod2 = [Hx2(pts)]     
       OutQC2  = [VarQC2(pts)]
     endif
   endif else begin
     OutObs1 = [OutObs1, Obs1(pts)]
     OutMod1 = [OutMod1, Hx1(pts)]
     OutQC1  = [OutQC1, VarQC1(pts)]
     OutDeps = [OutDeps, Depth(pts)]
     OutLats = [OutLats, Lats(pts)]
     OutLons = [OutLons, Lons(pts)]
     OutDates= [OutDates, Dates(pts)]
     OutInst= [OutInst, Instrumentsa(pts)]    
     OutPlatform= [OutPlatform, Platformsa(pts)]
     OutProfileNum = [OutProfileNum, ProfileNum(pts)]
     if NumVars eq 2 then begin
       OutObs2 = [OutObs2, Obs2(pts)]     
       OutMod2 = [OutMod2, Hx2(pts)]     
       OutQC2  = [OutQC2, VarQC2(pts)]
     endif
   endelse
   
   if NumVars eq 1 then begin
     if VarName ne 'SLA' then OutObs2 = fltarr(n_elements(OutObs1)) + rmdi
     OutMod2 = fltarr(n_elements(OutMod1)) + rmdi
     OutQC2  = fltarr(n_elements(OutQC1)) + rmdi
   endif
; 
   ifile2=ifile2+1
   endif
  ncdf_close, ncid
  
  endif
  
endfor ;ifile

pts1 = where(OutQC1 eq 1, count1)
pts2 = where(OutQC1 ne 1, count2)
if count1 gt 0 then OutQc1(pts1) = 0
if count2 gt 0 then OutQc1(pts2) = 1
if NumVars gt 1 then begin
  pts1 = where(OutQC2 eq 1, count1)
  pts2 = where(OutQC2 ne 1, count2)
  if count1 gt 0 then OutQc2(pts1) = 0
  if count2 gt 0 then OutQc2(pts2) = 1
endif

END
