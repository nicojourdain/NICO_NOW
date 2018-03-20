;+
PRO colorbar_idl, datain, colors, lablevels, datalevels, data=data, $
	textcolor=textcolor, mincolor=mincolor, position=position
;--------------------------------------------------------------------
; datain - gives the range of values which are represented by colors
; colors - color values
; lablevels - label levels
;
; Author:  D. J. Lea    Nov 2008
;--------------------------------------------------------------------

; keep current graphics keywords

psave=!p
xsave=!x
ysize=!y

	if (n_elements(mincolor) eq 3) then $
	      TVLCT,mincolor(0),mincolor(1),mincolor(2),0 

    mx=max(datain,min=mn)

	normal=1
	if (keyword_set(data)) then normal=0

    
    if (keyword_set(data)) then begin				; data coords
    y1 = !y.crange(0)-0.1*(!y.crange(1)-!y.crange(0))     
    y2 = !y.crange(0)-0.05*(!y.crange(1)-!y.crange(0))
    x1 = !x.crange(0)+0.1*(!x.crange(1)-!x.crange(0))
    x2 = !x.crange(0)+0.9*(!x.crange(1)-!x.crange(0))
    endif else begin						; normal coords
    y1 = !y.window(0)-0.11*(!y.window(1)-!y.window(0))     
    y2 = !y.window(0)-0.05*(!y.window(1)-!y.window(0))
;;    y1 = !y.window(0)+0.05*(!y.window(1)-!y.window(0))     
;;    y2 = !y.window(0)+0.1*(!y.window(1)-!y.window(0))
    x1 = !x.window(0)+0.076*(!x.window(1)-!x.window(0))
    x2 = !x.window(0)+0.924*(!x.window(1)-!x.window(0))
    endelse

    if (n_elements(position) eq 4) then begin
    	coords=position
    endif else begin
	coords=[x1,y1,x2,y2]
    endelse

    print,'coords: ',coords
	print, 'normal: ',normal, ' data: ',keyword_set(data)

;    nb = N_ELEMENTS(colors)               
;    x=coords(0)+(coords(2)-coords(0))*findgen(nb+1)/nb
;    
;    FOR i=0,nb-1 DO BEGIN
;;    print,colors(i)
;    polyfill, transpose([[x(i),x(i+1),x(i+1),x(i),x(i)],$
;      [coords(1),coords(1),coords(3),coords(3),coords(1)]]),$
;      color=colors(i), data=data, normal=normal
;    ENDFOR

;Position is always in normal units will need to do a conversion if 
; data units required...

    fill=0
    cell_fill=1

    maxdat=mx
    mindat=mn
    if (n_elements(datalevels) gt 0) then begin
	    maxdat=max(datalevels,min=mindat)
;            mindat=mindat+(1./!d.table_size)*(maxdat-mindat)
;            print,'datalevels ',datalevels
    endif

; fix to prevent zero contour range
    if (maxdat eq mindat) then begin
        if (maxdat eq 0) then begin
    	maxdat = maxdat + 1e-20
        mindat = mindat - 1e-20
        endif else begin
        maxdat = maxdat * 1.01
        mindat = mindat * 0.99
        if (maxdat lt mindat) then begin
        	tmp = mindat
                mindat = maxdat
                maxdat = tmp
        endif
        endelse
    endif
    
    array1d=(findgen(!d.table_size+1))/!d.table_size*(maxdat-mindat)+mindat
;    array1d=(findgen(!d.table_size+10)-10)/!d.table_size*(maxdat-mindat)+mindat

	print,'!d.table_size ',!d.table_size

;	print,'array1d ',array1d
        print,'min array1d ',min(array1d), max(array1d)
        print,'datain ',datain
        print,'mx ',mx,' mn ',mn

    if (n_elements(datalevels) gt 0) then print, 'datalevels ', datalevels
        
    array2d=array1d#replicate(1,2)
    if (n_elements(datalevels) lt 2) then datalevels=array1d
    
;    print, 'datalevels ', datalevels
;	print,'cell_fill ',cell_fill

    TVLCT, R, G, B, /get
    TVLCT,R(1),G(1),B(1),0        ; bodge fix for wrong black contour   

	if (n_elements(data) gt 0) then print,'data = ',data
        if (n_elements(normal) gt 0) then print,'normal = ',normal
        print,coords

; draw a few times to remove gaps
    nel=n_elements(datalevels)
    print, 'datalevels ',datalevels(0), datalevels(nel-1), nel
    print, 'array2d ',min(array2d), max(array2d)
    if (datalevels(0) lt datalevels(nel-1)) then begin 
    for i=0,2 do begin    
    contour, array2d, levels=datalevels, fill=fill, cell_fill=cell_fill, $
    POSITION=coords, /noerase, xstyle=5, ystyle=5, data=data, normal=normal
    endfor
    endif

    TVLCT,R(0),G(0),B(0),0 

    numlablevels=n_elements(lablevels)
    sigfig=1
	strlabs=strarr(numlablevels)

	repeat begin
        widstr=strtrim(string(sigfig+8),2)
; output 2 significant figures except if 0.

        for i = 0, numlablevels-1 do begin
        format='(g'+widstr+'.'+strtrim(string(sigfig),2)+')'
	strlabs(i)=strcompress(string(lablevels(i),format=format),/remove_all)

;	print,i,'.',strlabs(i),'.',format

        if (strmid(strlabs(i),0,1) eq '0' or strmid(strlabs(i),0,2) eq '-0') then begin
        	format='(g'+widstr+'.'+strtrim(string(sigfig-1),2)+')'
	        strlabs(i)=strcompress(string(lablevels(i),format=format),/remove_all) 
;                print,i,'.',strlabs(i),'.',format               
        endif

;	print,i,'.',strlabs(i),'.',format 

; trim out spaces
	strlabs(i)=strtrim(strlabs(i),2)
       
        strtemp=strlabs(i)
; strip off last two characters if there is 0 after the decimal place
; with a number thereafter
        ptpos=strpos(strtemp,'.0')
        len=strlen(strtemp)
;        if ptpos gt 0 then print,ptpos,len
        if ptpos gt 0 and len-2 ge ptpos then begin
		strlabs(i)=strmid(strtemp,0,len-1)		        
        endif

;if the remaining last character is a point (.) then get rid of it
	ptpos=strpos(strlabs(i),'.')
        len=strlen(strlabs(i))
;	print,ptpos, len
	if ptpos gt 0 and ptpos eq len-1 then begin
        	strlabs(i)=strmid(strlabs(i),0,len-1)
        endif
        
        endfor
;        print,strlabs
;        print,strlen(strlabs)
        sigfig=sigfig+1
        whe=where(strpos(strlabs,'e') gt 0, numexps)
;        print,'whe ',whe, numexps
; all labels should be unique
; should be all exps or none 
; give up if sigfigs 255       
	endrep until (n_elements(unique(strlabs)) eq numlablevels $
        	and (numexps eq 0 or numexps eq numlablevels) $
        	or sigfig gt 255) 

    pcharsize=!p.charsize
    if (pcharsize lt 1) then pcharsize=1
;    tolx=0.01*pcharsize*(!x.crange(1)-!x.crange(0))
    tolx=0.01*(x2-x1)
    
    ypos = y1-0.02*pcharsize*(!y.crange(1)-!y.crange(0))
    for i = 0, numlablevels-1 do begin
;      xpos = x1 + float(i)/float(n_elements(lablevels)-1)*(x2-x1)
	xpos = x1 + (lablevels(i)-mn)/(mx-mn)*(x2-x1)		; position labels correctly relative 

	if (xpos ge x1-tolx and xpos le x2+tolx) then begin 	; don't print any labels off the edge 

;      xyouts, xpos, ypos, strcompress(string(lablevels(i),format='(f7.2)'),/remove_all), $
;              color=!d.table_size-1, data=data, normal=normal, align=0.5

	if (n_elements(textcolor) eq 3) then $
	      TVLCT,textcolor(0),textcolor(1),textcolor(2),0 
;	print,'xyouts ',xpos, ypos, ' ',strlabs(i)
        xyouts, xpos, ypos, strlabs(i), data=data, normal=normal, align=0.5 
	endif

    endfor

; restore graphics keywords
!p=psave
!x=xsave
!y=ysize
                    
END
