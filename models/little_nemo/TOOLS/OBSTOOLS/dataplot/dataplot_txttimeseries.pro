pro plotts1, arrsv, title, typestr, minperc=minperc, $
	juldatemin=juldatemin, juldatemax=juldatemax, $
        emax=emax, emin=emin
;+--------------------------------------------------------
; plot mean and rms timeseries
;
; Author:  D. J. Lea      Feb 2008
;+--------------------------------------------------------

;date_label = LABEL_DATE(DATE_FORMAT = $ 
;   ['%D %M, %Y']) 

;date_label = LABEL_DATE(DATE_FORMAT = $ 
;   ['%D', '%M, %Y']) 
;date_label = LABEL_DATE(DATE_FORMAT = $ 
;   ['%M %Y']) 

date_label = LABEL_DATE(DATE_FORMAT=['%D-%M','%Y'])


; sort times (in case of a repeated day)

timsrt=sort(arrsv(0,*))

taxis=arrsv(0,timsrt)
num=arrsv(1,timsrt)
yaxis=arrsv(2,timsrt)
yaxis2=arrsv(3,timsrt)

; remove any zero times or non-finite values
wh=where(taxis gt 0 and finite(yaxis) and finite(yaxis2))
if (wh(0) gt -1) then begin
taxis=taxis(wh)
num=num(wh)
yaxis=yaxis(wh)
yaxis2=yaxis2(wh)
endif

; remove any with num lt than a specific value
if (n_elements(minperc) eq 1) then begin
maxnum=max(num,min=minnum)
wh=where(num gt maxnum*minperc)
if (wh(0) gt -1) then begin
taxis=taxis(wh)
num=num(wh)
yaxis=yaxis(wh)
yaxis2=yaxis2(wh)
endif
endif

mxt=max(taxis,min=mnt)
print, 'mnt mxt ',mnt, mxt

ymx=max([yaxis,yaxis2],min=ymn)

print, 'ymn ymx ',ymn,ymx

;create a small amount of space around the max and min

spc=(ymx-ymn)*0.05

ymn=ymn-spc*3
ymx=ymx+spc

if (n_elements(emax) gt 0) then ymx=emax
if (n_elements(emin) gt 0) then ymn=emin

; setup time axis range

skip=0
xmx=max(taxis,min=xmn)
if (n_elements(juldatemin) gt 0) then begin
	if (xmn le juldatemin) then xmn=juldatemin
        if (xmx le juldatemin) then skip=1
endif
if (n_elements(juldatemax) gt 0) then begin
	if (xmx ge juldatemax) then xmx=juldatemax
        if (xmn ge juldatemin) then skip=1
endif

if (skip eq 0) then begin
;plot, arrsv(0,timsrt), arrsv(2,timsrt), xstyle=1, linestyle=1, $
;   yrange=[ymn,ymx], $
;   ytitle=typestr, title=title, $
;   XTICKFORMAT = ['LABEL_DATE'], $ 
;   XTICKUNITS = ['Day'], $ 
;   XTICKINTERVAL = 4 

;plot, arrsv(0,timsrt), arrsv(3,timsrt), xstyle=1, /noerase, $
;   yrange=[ymn,ymx], $
;   XTICKFORMAT = ['LABEL_DATE'], $ 
;   XTICKUNITS = ['Day'], $ 
;   XTICKINTERVAL = 4 

;plot, arrsv(0,timsrt), arrsv(2,timsrt), xstyle=1, linestyle=1, $
;   yrange=[ymn,ymx], $
;   ytitle=typestr, title=title, $
;   XTICKFORMAT = ['LABEL_DATE','LABEL_DATE'],$
;   XTICKUNITS = ['Day','Month']

;plot, arrsv(0,timsrt), arrsv(3,timsrt), xstyle=1, /noerase, $
;   yrange=[ymn,ymx], $
;   XTICKFORMAT = ['LABEL_DATE','LABEL_DATE'],$
;   XTICKUNITS = ['Day','Month']

;plot, taxis, yaxis, xstyle=1, linestyle=1, $
;   yrange=[ymn,ymx], $
;   ytitle=typestr, title=title, $
;   XTICKFORMAT = ['LABEL_DATE']
;   
;plot, taxis, yaxis2, xstyle=1, /noerase, $
;   yrange=[ymn,ymx], $
;   XTICKFORMAT = ['LABEL_DATE']

plot, taxis, yaxis, xstyle=1, ystyle=1, linestyle=1, $
   yrange=[ymn,ymx], xrange=[xmn,xmx], $
   ytitle=typestr, title=title, $
   XTICKUNITS=['Time', 'Time'], XTICKFORMAT = ['LABEL_DATE'],YMARGIN=[6,4]

plot, taxis, yaxis2, xstyle=4+1, ystyle=4+1, /noerase, $
   yrange=[ymn,ymx], xrange=[xmn,xmx], $
   XTICKUNITS=['Time', 'Time'], XTICKFORMAT = ['LABEL_DATE'],YMARGIN=[6,4]
	


; key
		xcoord=0.8
		ycoord=0.9
		ycoord=0.35
                ycoord=0.2

                  ycoord2=ycoord-0.05
                  xcoord2=xcoord+0.03
                  xcoord3=xcoord+0.05
                  plots, [xcoord,xcoord2],[ycoord,ycoord], linestyle=0, /normal
                  xyouts, xcoord3, ycoord, 'RMS', /normal
                  plots, [xcoord,xcoord2],[ycoord2,ycoord2], linestyle=1, /normal
                  xyouts, xcoord3, ycoord2, 'mean',/normal                

endif

end


; plot number

pro plotts2, arrsv, title, typestr, minperc=minperc, $
	juldatemin=juldatemin, juldatemax=juldatemax
   

; number
;

;date_label = LABEL_DATE(DATE_FORMAT = $ 
;   ['%D %M, %Y']) 

;date_label = LABEL_DATE(DATE_FORMAT = $ 
;   ['%M %Y']) 

date_label = LABEL_DATE(DATE_FORMAT=['%D-%M','%Y'])



timsrt=sort(arrsv(0,*))

taxis=arrsv(0,timsrt)
yaxis=arrsv(1,timsrt)

wh=where(taxis gt 0 and finite(yaxis))	 ; remove any zero times and non-finite vals
if (wh(0) gt -1) then begin
taxis=taxis(wh)
yaxis=yaxis(wh)
endif
mxt=max(taxis,min=mnt)
print, 'mnt mxt ',mnt, mxt

;plot, arrsv(0,timsrt), arrsv(1,timsrt), xstyle=1, $
;   ytitle='Number of obs assim', title=title, $
;   XTICKFORMAT = ['LABEL_DATE'], $ 
;   XTICKUNITS = ['Day'], $ 
;   XTICKINTERVAL = 4 

;info, taxis
;info, yaxis

;print,taxis, yaxis

;plot, taxis, yaxis, xstyle=1, $
;   ytitle='Number of obs assim', title=title, $
;   XTICKFORMAT = ['LABEL_DATE']

ymx=max(yaxis)*1.05

; setup time axis range

skip=0
xmx=max(taxis,min=xmn)
if (n_elements(juldatemin) gt 0) then begin
	if (xmn le juldatemin) then xmn=juldatemin
        if (xmx le juldatemin) then skip=1
endif
if (n_elements(juldatemax) gt 0) then begin
	if (xmx ge juldatemax) then xmx=juldatemax
        if (xmn ge juldatemin) then skip=1
endif

if (skip eq 0) then $
plot, taxis, yaxis, xstyle=1, ystyle=1, $
   ytitle='Number of obs assim', title=title, yrange=[0,ymx], xrange=[xmn,xmx],$
   XTICKUNITS = ['Time', 'Time'], XTICKFORMAT = ['LABEL_DATE'],YMARGIN=[6,4]


print,'min time ',min(arrsv(0,timsrt)),max(arrsv(0,timsrt)) 

end

PRO dataplot_txttimeseries, files, gif=gif, ps=ps, filtstr=filtstr, view=view, $
	bin=bin, minperc=minperc, datemin=datemin, datemax=datemax, notitle=notitle, $
        emax=emax, emin=emin

; DJL switch off wave compatibility mode
res=execute("waveoff")

if (n_elements(filtstr) eq 0) then filtstr=""
if (n_elements(view) eq 0) then view=0

print, 'dataplot_txttimeseries '

if (n_elements(datemin) gt 0) then begin
	; month, day, year
	juldatemin=julday(datemin(1),datemin(2), datemin(0))
        print, 'juldatemin set:', juldatemin, datemin
endif
if (n_elements(datemax) gt 0) then begin
	; month, day, year
	juldatemax=julday(datemax(1),datemax(2), datemax(0))
        print, 'juldatemax set:', juldatemax, datemax
endif



numfiles=n_elements(files)

print,'numfiles ',numfiles

imax=500
	arrs=dblarr(4,imax)
	arr=dblarr(4)
	arrsv=dblarr(4,10000)

j=0L		; position in full array
for ii=0,numfiles-1 do begin
	print,files(ii)
        OPENR, unit, files(ii), /get_lun
        obstypestr=""
        readf,unit,obstypestr
        typestr=""
        readf,unit,typestr
        xrange=fltarr(2)
        readf,unit,xrange
        yrange=fltarr(2)
        readf,unit,yrange
        binspday=0.0
        readf,unit,binspday

	i=0
	while (~ eof(unit) and i lt imax) do begin        
        readf,unit,arr
	print,i,arr
        if arr(1) eq 0 then arr(2:*)=0
        print,arr
        arrs(*,i)=arr
        i=i+1
        endwhile
 
	numtimes=i
	print, 'numtimes ',numtimes
        
        if (numtimes ge binspday) then begin
        	print,arrs(*,numtimes-binspday:numtimes-1)

; store daily values from each file in full time series array

	arrsv(*,j:j+binspday-1)=arrs(*,numtimes-binspday:numtimes-1)
        j=j+binspday

	endif else begin 
        if (numtimes gt 0) then begin
        
        print, '** numtimes ',numtimes
        print, arrs(*,0:numtimes-1)  
        arrsv(*,j:j+numtimes-1)=arrs(*,0:numtimes-1)        
        j=j+numtimes
                
        endif
        endelse
                
        FREE_LUN, unit
        
        print, obstypestr
        print, typestr
        print, xrange
        print, yrange
        print, binspday
endfor	

print, 'j ',j

arrsv=arrsv(*,0:j-1)

;stop

; bin up the data

;print,arrsv

if (n_elements(bin) gt 0) then begin
if (bin gt 1) then begin
; time 0 num 1 mean 2 rms 3
arrsv2=dblarr(4,j/bin)
for i=0,j/bin-1 do begin

	arrsvtemp=arrsv(*,i*bin:(i+1)*bin-1)
        print,arrsvtemp
	wh=where(arrsvtemp(1,*) gt 0)		; number of obs gt 0
;        print,wh
        if (wh(0) gt -1) then begin 

; number of obs
        arrsv2(1,i)=total(arrsvtemp(1,wh))
; mean
        arrsv2(2,i)=total(arrsvtemp(2,wh)*arrsvtemp(1,wh))/arrsv2(1,i)
; rms
        arrsv2(3,i)=sqrt(total(arrsvtemp(3,wh)^2*arrsvtemp(1,wh))/arrsv2(1,i))
; date (average)
;	print,arrsv(0,i*bin)
	arrsv2(0,i)=total(arrsvtemp(0,*))/bin
;	print,arrsv2(0,i)

	endif

endfor

info, arrsv2

arrsv=arrsv2
	
endif
endif

nel=n_elements(arrsv(0,*))

; produce 1 month/3 month/all average values

finaltime=arrsv(0,nel-1)
onemon=finaltime-30
threemon=finaltime-90

print,arrsv

wh1=where(arrsv(0,*) gt onemon and arrsv(1,*) gt 0)
wh3=where(arrsv(0,*) gt threemon and arrsv(1,*) gt 0)
wh0=where(arrsv(1,*) gt 0)

num1=total(arrsv(1,wh1))
mean1=total(arrsv(2,wh1)*arrsv(1,wh1))/num1
rms1=sqrt(total(arrsv(3,wh1)^2*arrsv(1,wh1))/num1)
print,num1

num3=total(arrsv(1,wh3))
mean3=total(arrsv(2,wh3)*arrsv(1,wh3))/num3
rms3=sqrt(total(arrsv(3,wh3)^2*arrsv(1,wh3))/num3)
print,num3

num0=total(arrsv(1,wh0))
mean0=total(arrsv(2,wh0)*arrsv(1,wh0))/num0
rms0=sqrt(total(arrsv(3,wh0)^2*arrsv(1,wh0))/num0)
print,num0

if (keyword_set(notitle)) then begin
title1=""
title=""
endif else begin 
subtitle=          'rms (mean), 1 month: '+strtrim(string(rms1,format='(G0.4)'),2)+$
	'('+strtrim(string(mean1,format='(G0.3)'),2)+')'
subtitle=subtitle+ ', 3 month: '+strtrim(string(rms3,format='(G0.4)'),2)+$
	'('+strtrim(string(mean3,format='(G0.3)'),2)+')'
subtitle=subtitle+ ', all: '+strtrim(string(rms0,format='(G0.4)'),2)+$
	'('+strtrim(string(mean0,format='(G0.3)'),2)+')'


fullfiltstr=''
if (filtstr ne '') then fullfiltstr=' Type: '+filtstr

title=obstypestr+typestr+'   lons ('+strtrim(string(xrange(0),format='(F0.2)'),2)+$
	','+strtrim(string(xrange(1),format='(F0.2)'),2)+$
        ')   lats ('+strtrim(string(yrange(0),format='(F0.2)'),2)+','+$
        strtrim(string(yrange(1),format='(F0.2)'),2)+')'+fullfiltstr

title1=title+'!C'+subtitle
endelse

if (keyword_set(gif)) then begin
	thisDevice = !D.Name

        Set_Plot, 'Z'			; do graphics in the background
;	Device, Set_Resolution=[640,512], decomposed=0
	Device, Set_Resolution=[800,512], decomposed=0
        Erase                           ; clear any existing stuff
        !p.charsize=0.75
;        setupct, r, g, b		; setup color table

	plotts1, arrsv, title1, typestr, minperc=minperc, $
	juldatemin=juldatemin, juldatemax=juldatemax, $
        emax=emax, emin=emin


	snapshot = TVRD()
        WRITE_GIF,'dataplot_timeseries.gif',snapshot, r, g, b

	plotts2, arrsv, title, typestr, minperc=minperc, $
	juldatemin=juldatemin, juldatemax=juldatemax

        
	snapshot = TVRD()
        WRITE_GIF,'dataplot_numtimeseries.gif',snapshot, r, g, b

        Device, Z_Buffer=1		; reset graphics mode
        Set_Plot, thisDevice
        !p.charsize=0.0

endif

if (keyword_set(ps)) then begin
  ps=1
  eps=0
  landscape=1
  pr2o,file='dataplot_timeseries.ps',landscape=landscape,ps=ps,eps=eps,color=1	
  plotts1, arrsv, title1, typestr, minperc=minperc, $
	juldatemin=juldatemin, juldatemax=juldatemax, $
        emax=emax, emin=emin
  prend2o,view=view
  
  pr2o,file='dataplot_numtimeseries.ps',landscape=landscape,ps=ps,eps=eps,color=1	
  plotts2, arrsv, title, typestr, minperc=minperc, $
	juldatemin=juldatemin, juldatemax=juldatemax
  prend2o,view=view

endif 

end

