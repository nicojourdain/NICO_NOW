;+----------------------------------------------------------------------------------------
; dataplot.pro
; IDL widget based plotting routine for plotting observation and background values
;
; Author: D. J. Lea     -  Feb 2008
;
;+----------------------------------------------------------------------------------------
;example calls:
;
;dataplot,filenamearray
;dataplot,filenamearray,/batch,/gif			; plots data directly to a gif
;dataplot,filenamearray,/batch,/ps			; plots data directly to a ps
;dataplot,filenamearray,/batch,area=area            ; area 4 element array or descriptive string
;dataplot, longitude, latitude, deparr, dayarr, valarr, bkgarr
;dataplot, [longitude, latitude, deparr, dayarr, valarr, bkgarr], rmdi=rmdi, filename=filename 
;
;optional keywords
;area			descriptive string or array [minlon,minlat,maxlon,maxlat]
;typeplot=1-5 		plot obs-bkg or obs bkg etc 
;alldays=1 		plot all days otherwise just plot first day    
;depths			[depmin,depmax] in metres
;showmdt		plot mdt
;obstypeselect		string array of selected obs types to plot 
;printobstypes		keyword to print unique obs types to the terminal
;
;+-----------------------------------------------------------------------------------------
; Note the upper two slider bars control the depth range
; the lower two slider bars control the day range
;------------------------------------------------------------------------------------------


; Event-handling procedure. 
PRO dataplot_event, ev 
 
  ; Retrieve the anonymous structure contained in the user value of 
  ; the top-level base widget.  
 
  WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash 
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval 

;  print,uval

;  print,'event'
;  print,stash.depmin

;  position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
;  print,position 
;  xsize = (position(2) - position(0)) * !D.X_VSIZE
;  ysize = (position(3) - position(1)) * !D.Y_VSIZE
;  xstart = position(0) * !D.X_VSIZE
;  ystart = position(1) * !D.Y_VSIZE
;  print,xsize,ysize,xstart,ystart
  
  ; If the event is generated in the draw widget, update the 
  ; label values with the current cursor position and the value 
  ; of the data point under the cursor. Note that since we have 
  ; passed a pointer to the image array rather than the array 
  ; itself, we must dereference the pointer in the 'image' field 
  ; of the stash structure before getting the subscripted value. 
 
  IF (TAG_NAMES(ev, /STRUCTURE_NAME) eq 'WIDGET_DRAW') THEN BEGIN 
	xdev= ev.X 
	ydev= ev.Y
        
;        print, 'xdev ',xdev, ' ydev ',ydev
        
; convert the pointer position to a data position
  	arr=CONVERT_COORD( xdev, ydev, /device, /to_data) 
  	xdata=arr(0)
	ydata=arr(1)

;	print, xdata , ydata        
        
;        print, 'xdev: ',xdev,' ydev: ',ydev,' xdata: ',xdata, ' ydata: ',ydata
;    WIDGET_CONTROL, stash.label1, $ 
;      SET_VALUE='X position: ' + STRING(xdata) 
;    WIDGET_CONTROL, stash.label2, $ 
;      SET_VALUE='Y position: ' + STRING(ydata) 
;;    WIDGET_CONTROL, stash.label3, $ 
;;      SET_VALUE='Value: ' + $ 
;;      STRING((*stash.imagePtr)[ev.X, ev.Y], FORMAT='(Z12)') 
;    WIDGET_CONTROL, stash.label3, $ 


   ; What kind of event is this?

;eventTypes = ['DOWN', 'UP', 'MOTION']
;0 1 2
;thisEvent = ev.type

;print, 'ev.type ',ev.type

CASE ev.type OF
   0: BEGIN
; dragbox start
   print, 'down ',xdev,ydev, xdata, ydata
         ; Turn motion events on for the draw widget.

      Widget_Control, stash.draw, Draw_Motion_Events=1

	; dragbox
         ; Create a pixmap. Store its ID. Copy window contents into it.

      Window, /Free, /Pixmap, XSize=stash.im_size(1), YSize=stash.im_size(2)
      stash.pixID = !D.Window
      Device, Copy=[0, 0, stash.im_size(1), stash.im_size(2), 0, 0, stash.drawID]
;      WSet, stash.drawID

	print,stash.im_size(0), stash.im_size(1), stash.im_size(2), stash.pixID, stash.drawID
      
      stash.xcorn=xdev
      stash.ycorn=ydev
      
      stash.xdatacorn=xdata
      stash.ydatacorn=ydata
   
;   RETURN
   ENDCASE
   1: BEGIN
; dragbox close
   print, 'up ',xdev,ydev, xdata, ydata
         ; Turn draw motion events off. Clear any events queued for widget.

      Widget_Control, stash.draw, Draw_Motion_Events=0, Clear_Events=1

       WSet, stash.drawID
      Device, Copy=[0, 0, stash.im_size(1), stash.im_size(2), 0, 0, stash.pixID]
      WDelete, stash.pixID
           
      ; zoom in

; dragbox   
; avoid zooming if no dragging has occurred
	pixdiffx = min([abs(stash.xcorn-xdev),abs(stash.ycorn-ydev)])       
        print, '** Pixdiffx ',pixdiffx, stash.xcorn-xdev, stash.ycorn-ydev
        if (pixdiffx gt 1 and finite(xdata) and finite(ydata)) then begin

   	minxdata = min([stash.xdatacorn, xdata], max=maxxdata)
        minydata = min([stash.ydatacorn, ydata], max=maxydata)


	if (minxdata ge -180 and maxxdata le 360 and minydata ge -90 and maxydata le 90) then begin

                stash.xrange=[minxdata,maxxdata]
        	stash.yrange=[minydata,maxydata]
                
	        plotpoints,stash
        endif
        endif	
           
;   RETURN
   ENDCASE
   2: BEGIN
; drag box motion
;   print, 'motion: ',ev.x, ev.y

      WSet, stash.drawID
      Device, Copy=[0, 0, stash.im_size(1), stash.im_size(2), 0, 0, stash.pixID]

   
   sx = stash.xcorn
   sy = stash.ycorn

      PlotS, [sx, sx, xdev, xdev, sx], [sy, ydev, ydev, sy, sy], /Device, $
          Color=!p.color
;         Color=255
   
   ENDCASE
   
;   RETURN
   7: begin
		print,'mouse wheel event ',ev.clicks
		if (ev.clicks eq 1 or ev.clicks eq -1) then begin
		print,'zoom in/out'

		print,'stash.depmin ',stash.depmin

		print,'xdata ',xdata
                
                if (finite(xdata) and finite(ydata)) then begin

; make an adjustment if we're in the Pacific
	xrange=stash.xrange
        typeproj=stash.typeproj
	if (xrange(1) gt 180 and typeproj eq 1) then if (xdata lt 0) then xdata=xdata+360 
       

		oxrange=stash.xrange
		oyrange=stash.yrange

		dataplot_zoom, stash, xdata, ydata, ev.clicks

; plot only if a change has been made
		if (stash.xrange(0) ne oxrange(0) or stash.yrange(0) ne oyrange(0) $
                    or stash.xrange(1) ne oxrange(1) or stash.yrange(1) ne oyrange(1)) then $
			plotpoints, stash

		endif
                
                endif

   ENDCASE
ENDCASE








;      SET_VALUE='Value: ' + STRING(ev.release) 

;	if(ev.release eq 1) then begin
;		print,'left click - search for nearest point'
	if(ev.release eq 1 or ev.release eq 4) then begin		; mouse click

; might want a generalised point selection routine
; for this routine and for plotpoints

		print,'xdata ',xdata, ' ydata ',ydata
                print,'xdev ',xdev, 'ydev ',ydev

		if (finite(xdata) and finite(ydata)) then begin

		print, 'finite'

		xarr1=stash.xarr
		yarr1=stash.yarr
		dep1=stash.dep
		dayarr=stash.dayarr
		daymin=stash.daymin
		daymax=stash.daymax
                obstypes1=stash.obstypes
 		xrange=stash.xrange
		yrange=stash.yrange
		xs=xrange(1)-xrange(0)
		ys=yrange(1)-yrange(0)
		rmdi=stash.rmdi
;                if (stash.salinity eq 0) then begin
;		  obs1=stash.obs
;		  bkg1=stash.bkg
;                 qcarr1=stash.qcarr
;                endif else begin
;		  obs1=stash.obs2
;		  bkg1=stash.bkg2
;                 qcarr1=stash.qcarr2
;		endelse                		
                typeproj=stash.typeproj

; make an adjustment if we're in the Pacific
	if (xrange(1) gt 180 and typeproj eq 1) then if (xdata lt 0) then xdata=xdata+360 

; set reasonable dist criteria

		distmax=(xs/100.)^2+(ys/100.)^2
;		print,'distmax ',distmax


		selpoints,stash,lonsel, latsel, innovsel, qcsel, daysel, obstypsel, obnumsel, numsel, typestr 

; get max min daysel
		mindaysel=min(daysel, max=maxdaysel)

;		print, 'selpoints ',obnumsel

		if (n_elements(lonsel) gt 0) then begin
		dist=((lonsel-xdata)^2+(latsel-ydata)^2)
		res=min(dist,whd)		; whd is the minumum dist point index

;		print,lonsel

	xselstr=""
	yselstr=""
	valselstr=""
	qcselstr=""
	datestr=""
        obnumselstr=""
        mindatestr=""
        maxdatestr=""

;	print,'whd(0) ',whd(0)
;        print,'res ',res, ' distmax ',distmax

	obstypstr=""		
	if (whd(0) gt -1 and res lt distmax) then begin		
	xsel=lonsel(whd)
	ysel=latsel(whd)
 
 	print,'xsel ',xsel,' ysel ',ysel       
 ; make an adjustment if we're in the Pacific
	if (xrange(1) gt 180 and typeproj eq 1) then if (xsel gt 180) then xsel=xsel-360 
               
	valsel=innovsel(whd)
	qcsel=qcsel(whd)
	datesel=daysel(whd)
	obstypsel=obstypsel(whd)
	obnumsel=obnumsel(whd)

	xselstr=string(xsel)
	yselstr=string(ysel)
	valselstr=string(valsel)	
	qcselstr=string(qcsel)
;	datestr=string(datesel)
	obstypstr=string(obstypsel(0))
        obnumselstr=string(obnumsel(0))
        print,'datesel(0) ',datesel(0) 
        print,'obnumsel(0) ',obnumsel(0)
	jul_to_dtstr,datesel(0),datestr
        jul_to_dtstr,mindaysel,mindatestr
        jul_to_dtstr,maxdaysel,maxdatestr
   
	if (ev.release eq 4) then begin
        
        	print,'ev.release ',ev.release

;		window
		wh=where(xarr1 eq xsel and yarr1 eq ysel)
                print,'xsel ',xsel
;                print,'xarr1 ',xarr1
		dep2=dep1(wh)

		if (wh(0) gt -1) then begin
		if (stash.density eq 0) then begin

         if (stash.salinity eq 0) then begin
;                  print,'salinity eq 0'
            obs2=stash.obs(wh)
		      bkg2=stash.bkg(wh)
            qcarr2=stash.qcarr(wh)
         endif else begin
;                  print,'salinity eq 1'
		      obs2=stash.obs2(wh)
		      bkg2=stash.bkg2(wh)
            qcarr2=stash.qcarr2(wh)
		   endelse

      endif else begin

         if (stash.filetype eq 'CRT') then begin

            bkgU=stash.bkg(wh)
            bkgV=stash.bkg2(wh)
            bkg2=sqrt(bkgU*bkgU + bkgV*bkgV)

            obsU=stash.obs(wh)
            obsV=stash.obs2(wh)
            obs2=sqrt(obsU*obsU + obsV*obsV)

            qcarr2=stash.qcarr(wh)

         endif else begin

;            print,'density eq 1'
            bkgt=stash.bkg(wh)
            bkgs=stash.bkg2(wh)
            obst=stash.obs(wh)
            obss=stash.obs2(wh)                  
;		       obs2=obst+obss
;            bkg2=bkgt+bkgs
		      obs2=eos_nemo(obst,obss)
            bkg2=eos_nemo(bkgt,bkgs)
            wh2=where(bkgt eq rmdi or bkgs eq rmdi)
            if (wh2(0) gt -1) then begin
               obs2(wh2)=rmdi
               bkg2(wh2)=rmdi
            endif
            qcarr2=max([[stash.qcarr(wh)],[stash.qcarr2(wh)]],dim=2)                                		

         endelse

		endelse

;		val2=abs(obs1(wh)-bkg1(wh))
		val2=abs(obs2-bkg2)
;		obs2=obs1(wh)
;		bkg2=bkg1(wh)
		obstype2=obstypes1(wh)
;                qcarr2=qcarr1(wh)
                dayarr2=dayarr(wh)
;		val2=sqrt(val1(wh)^2)
		
;		print,'val2 ',val2(wh2)
;		print,'dep2 ',dep2(wh2)
;		plot,val2(wh2),dep2(wh2),ystyle=1,xstyle=1

		profilewindow,dep2,val2,obs2,bkg2,obstype2,qcarr2,dayarr2,datestr,xselstr,yselstr,rmdi, salinity=stash.salinity, $
                	plot_bad_obs=stash.plot_bad_obs, white_on_black=stash.white_on_black, coltable=stash.coltable, $
                        	depmax=stash.depmax, depmin=stash.depmin


; set graphics back to main window
;  		WSET, stash.drawID
 		print,'ev release'
		plotpoints,stash
                
                endif     ; wh(0)>-1

	endif			; ev.release 4 (right mouse)
        endif         
	endif			; left or right mouse click
	
    WIDGET_CONTROL, stash.label1, $ 
      SET_VALUE=stash.labt1 + strtrim(xselstr,1) 
    WIDGET_CONTROL, stash.label2, $ 
      SET_VALUE=stash.labt2 + strtrim(yselstr,1) 
     WIDGET_CONTROL, stash.label3, $ 
      SET_VALUE=typestr+': ' + strtrim(valselstr,1) 
     WIDGET_CONTROL, stash.label4, $ 
      SET_VALUE=stash.labt4 + strtrim(qcselstr,1) 
     WIDGET_CONTROL, stash.label5, $
      SET_VALUE=stash.labt5 + strtrim(datestr,1)
     WIDGET_CONTROL, stash.label6, $ 
      SET_VALUE=stash.labt6 + strtrim(valselstr,1) 
;     WIDGET_CONTROL, stash.label7, $ 
;      SET_VALUE=stash.labt7 + strtrim(string(innovsd),1)
     WIDGET_CONTROL, stash.label8, $
      SET_VALUE=stash.labt8 + strtrim(obstypstr,1)
     WIDGET_CONTROL, stash.label9, $
      SET_VALUE=stash.labt9 + strtrim(obnumselstr,1)
     WIDGET_CONTROL, stash.label10, $
      SET_VALUE=stash.labt10 + string(mindatestr) + ' ' + string(maxdatestr)
		
	endif else begin			; not finite

	print, 'not finite'
        
; check for colorbar click
		if (ydev lt 80) then begin        
 	        toplevel=stash.base
	        fmx=stash.fmx
        	fmn=stash.fmn
      	        mx=stash.mx
        	mn=stash.mn
   		inputmxmnwindow, fmx, fmn, mx, mn, stash.rmdi, toplevel, success
		if (success eq 1) then begin
        	stash.fmx=fmx
        	stash.fmn=fmn
        	plotpoints,stash
        	endif
                endif
        
        endelse			; finite

	endif

		
      
;      print,tag_names(ev)
;      print,ev.id
;      print,ev.top
;      print,ev.handler
;      print,ev.type
;      print,ev.press
;      print,ev.release
;      print,ev.clicks
;      print,ev.modifiers
;      print,ev.ch
;      print,ev.key
       
   ENDIF                   ; end of widget draw events
 
  ; If the event is generated in a button, destroy the widget 
  ; hierarchy. We know we can use this simple test because there 
  ; is only one button in the application. 
 

;  IF (TAG_NAMES(ev, /STRUCTURE_NAME) eq 'WIDGET_COMBOBOX') THEN BEGIN
;   IF (uval eq 'LEVELLIST') THEN BEGIN

   IF (uval eq 'LEVELCHOICE') THEN BEGIN

	stash.depmin=ev.value

		print,'levelchoice'
	print, 'stash.depmin: ',stash.depmin
	print, 'stash.depmax: ',stash.depmax

	if (stash.depmin ge stash.depmax) then begin
		stash.depmax=stash.depmin
	if (stash.depmax gt stash.depmaxl) then stash.depmax=stash.depmaxl

;set sliderb position
	WIDGET_CONTROL, stash.sliderb, set_value=stash.depmax

	endif
	

		plotpoints, stash

		
;		   print,ev.index,widget_info(ev.id,/combobox_gettext)
  
  ENDIF

   IF (uval eq 'LEVELCHOICEB') THEN BEGIN

	stash.depmax=ev.value

		print,'levelchoiceb'
	print, 'stash.depmin: ',stash.depmin
	print, 'stash.depmax: ',stash.depmax

	if (stash.depmax le stash.depmin) then begin
		stash.depmin=stash.depmax
	if (stash.depmin lt stash.depminl) then stash.depmin=stash.depminl

;set sliderb position
	WIDGET_CONTROL, stash.slider, set_value=stash.depmin

	endif
	

		plotpoints, stash

		
;		   print,ev.index,widget_info(ev.id,/combobox_gettext)
  
  ENDIF


  IF (uval eq 'DATERANGE1') THEN BEGIN

	odaymin=stash.daymin
        odaymax=stash.daymax
  
  	stash.daymin=ev.value
;	print, 'ev.drag ',ev.drag

; set slider label
	jul_to_dtstr,stash.daymin,dayminstr, /notime
        WIDGET_CONTROL, stash.slider1label, $ 
        SET_VALUE='min date: '+dayminstr 

	if (stash.daymin ge stash.daymax) then begin
;		stash.daymax=stash.daymin+1
		stash.daymax=stash.daymin
	if (stash.daymax gt stash.daymaxl) then stash.daymax=stash.daymaxl

;set slider2 position
	WIDGET_CONTROL, stash.slider2, set_value=stash.daymax
; set slider 2 label
	jul_to_dtstr,stash.daymax,daymaxstr, /notime
        WIDGET_CONTROL, stash.slider2label, $ 
        SET_VALUE='max date: '+daymaxstr 
	
        
	endif
	
	print,'daterange1'
;	if (ev.drag eq 0) then
        if (stash.daymin ne odaymin or stash.daymax ne odaymax) then plotpoints,stash
	
  ENDIF
  IF (uval eq 'DATERANGE2') THEN BEGIN

	odaymin=stash.daymin
        odaymax=stash.daymax
  
  	stash.daymax=ev.value

; set slider label
	jul_to_dtstr,stash.daymax,daymaxstr, /notime
        WIDGET_CONTROL, stash.slider2label, $ 
        SET_VALUE='max date: '+daymaxstr 

	if (stash.daymax le stash.daymin) then begin
;		stash.daymin=stash.daymax-1
		stash.daymin=stash.daymax
	if (stash.daymin lt stash.dayminl) then stash.daymin=stash.dayminl

;set slider1 position
	WIDGET_CONTROL, stash.slider1, set_value=stash.daymin
; set slider 2 label
	jul_to_dtstr,stash.daymin,dayminstr, /notime
        WIDGET_CONTROL, stash.slider1label, $ 
        SET_VALUE='min date: '+dayminstr 

	endif
	
	print,'daterange2'
;	if (ev.drag eq 0) then 
        if (stash.daymin ne odaymin or stash.daymax ne odaymax) then plotpoints,stash

  ENDIF
  
	IF (uval eq "RADIO1") THEN BEGIN
		print,'uval eq RADIO1'
		stash.typeplot=1
                print, 'typeplot ',stash.typeplot
		plotpoints,stash
	ENDIF
	IF (uval eq "RADIO2") THEN BEGIN
		print,'uval eq RADIO2'	
		stash.typeplot=2
                print, 'typeplot ',stash.typeplot
		plotpoints,stash
	ENDIF
  	IF (uval eq "RADIO3") THEN BEGIN
		print,'uval eq RADIO3'
		stash.typeplot=3
                print, 'typeplot ',stash.typeplot
		plotpoints,stash
	ENDIF
  	IF (uval eq "RADIO4") THEN BEGIN
		print,'uval eq RADIO4'
		stash.typeplot=4
                print, 'typeplot ',stash.typeplot
		plotpoints,stash
	ENDIF

	IF (uval eq "RADIO5") THEN BEGIN
		print,'uval eq RADIO5'
		stash.ombtypeplot=1
                print, 'ombtypeplot ',stash.ombtypeplot
		plotpoints,stash
	ENDIF
	IF (uval eq "RADIO6") THEN BEGIN
		print,'uval eq RADIO6'
		stash.ombtypeplot=2
                print, 'ombtypeplot ',stash.ombtypeplot
		plotpoints,stash
	ENDIF
	IF (uval eq "RADIO7") THEN BEGIN
		print,'uval eq RADIO7'
		stash.ombtypeplot=3
                print, 'ombtypeplot ',stash.ombtypeplot
		plotpoints,stash
	ENDIF

	IF (uval eq "RADIO01") THEN BEGIN
		print,'uval eq RADIO01'
		stash.typeproj=1
		stash.xrange=stash.xrangedef
		stash.yrange=stash.yrangedef
		plotpoints,stash
	ENDIF
	IF (uval eq "RADIO02") THEN BEGIN
		print,'uval eq RADIO02'	
		stash.typeproj=2
		stash.xrange=stash.xrangedef
		stash.yrange=stash.yrangedef
		plotpoints,stash
	ENDIF
  	IF (uval eq "RADIO03") THEN BEGIN
		print,'uval eq RADIO03'
		stash.typeproj=3
		stash.xrange=stash.xrangedef
		stash.yrange=stash.yrangedef
		plotpoints,stash
	ENDIF

	IF (uval eq "RADIO001") THEN BEGIN
		print,'uval eq RADIO001 ev.select: ',ev.select
		stash.plot_bad_obs=ev.select
;		stash.xrange=stash.xrangedef
;		stash.yrange=stash.yrangedef
		plotpoints,stash
	ENDIF


	IF (uval eq "RADIOSAL") THEN BEGIN
	    print,'uval eq RADIOSAL ev.select: ',ev.select
	    salinity=ev.select
;            if (stash.filetype eq "Prof" or stash.filetype eq "feedback") then begin 
	       stash.salinity=salinity
  	       plotpoints,stash
;	    endif
	ENDIF


	IF (uval eq "RADIODENSITY") THEN BEGIN
            print,'uval eq RADIODENSITY ev.select: ',ev.select
            density=ev.select
            if (stash.filetype eq "CRT") then stash.salinity=0
;            if (stash.filetype eq "Prof" or stash.filetype eq "CRT") then begin
               stash.density=density  
       	      plotpoints,stash
;	         endif
	ENDIF


  IF (uval eq "PRINT") THEN BEGIN
  	ps=1
	eps=0
	landscape=1
  	pr2,file=stash.outfile+'.ps',landscape=landscape,ps=ps,eps=eps,color=1	
  	plotpoints,stash
	prend2,/view
  ENDIF

  IF (uval eq "SAVE") THEN BEGIN

	thisDevice = !D.Name
        psave=!p

        Set_Plot, 'Z'			; do graphics in the background
;	Device, Set_Resolution=[800,512], decomposed=0
        Device, Set_Resolution=stash.picsize, decomposed=0
        Erase                           ; clear any existing stuff
        !p.charsize=0.75

;	if (stash.white_on_black eq 0) then begin
;; flip background and foreground color
;                        pcolor=!p.color
;                        pbackground=!p.background
;                        !p.color=pbackground
;                        !p.background=pcolor
;
;        endif
;        print,'!p.color,!p.background ',!p.color,!p.background

        setupct, r, g, b, coltable=stash.coltable, $
        	white_on_black=stash.white_on_black		; setup color table
; plot data
  	plotpoints,stash
	snapshot = TVRD()
        WRITE_GIF,stash.outfile+'.gif',snapshot, r, g, b
        Device, Z_Buffer=1		; reset graphics mode
        Set_Plot, thisDevice
;        !p.charsize=0.0
	!p=psave
        
        spawn,'xv '+stash.outfile+'.gif'

  ENDIF

;Area selection

  xrange=stash.xrange
  yrange=stash.yrange
  areasel,uval,xrange,yrange,success, toplevel=stash.base
  if (success eq 1) then begin
  stash.xrange=xrange
  stash.yrange=yrange
  plotpoints,stash
  endif

      
  IF (uval eq "Timeseries") THEN BEGIN
  	stash.numtimeseries=0
        timeserieswindow,stash
; set graphics back to main window
;  		WSET, stash.drawID
;; 		print,'ev release'
;;		plotpoints,stash

  ENDIF

  IF (uval eq "Num timeseries") THEN BEGIN
	stash.numtimeseries=1  
        timeserieswindow,stash
; set graphics back to main window
;  		WSET, stash.drawID
 		print,'ev release'
		plotpoints,stash

  ENDIF

  IF (uval eq "TS diagram") THEN BEGIN
        tsdiagramwindow,stash
; set graphics back to main window
;  	WSET, stash.drawID
 	print,'ev release'
	plotpoints,stash

  ENDIF

  IF (uval eq "Worst points") THEN BEGIN
        worstpointswindow,stash
  ENDIF

  IF (uval eq "Filter type") THEN BEGIN
  	filterwindow, stash
  ENDIF

  IF (uval eq "Low res map") THEN BEGIN
;        stash.map_file=""
        stash.hires_map=0       
        Widget_Control, stash.loresmap, Set_Button=1
        Widget_Control, stash.hiresmap, Set_Button=0
        plotpoints,stash
  ENDIF

  IF (uval eq "Hi res map") THEN BEGIN
;        stash.map_file="/opt/ukmo/wave/ukmo/data/map_europe.xdr"
	stash.hires_map=1
        Widget_Control, stash.loresmap, Set_Button=0
        Widget_Control, stash.hiresmap, Set_Button=1
        plotpoints,stash
  ENDIF

  IF (uval eq "Plot only bad obs") THEN BEGIN
  	stash.plot_only_bad_obs = 1-stash.plot_only_bad_obs
        Widget_Control, stash.pltonbado, Set_Button=stash.plot_only_bad_obs
        if (stash.plot_bad_obs eq 1) then plotpoints,stash
  ENDIF

  IF (uval eq "White on black") THEN BEGIN
        print,uval,' !p.color,!p.background ',!p.color,!p.background
  	stash.white_on_black = 1-stash.white_on_black
        Widget_Control, stash.whtonblack, Set_Button=stash.white_on_black
;        IF (!d.name eq 'X') then begin 
;                	WSET, stash.drawID
;; flip background and foreground color
;                        pcolor=!p.color
;                        pbackground=!p.background
;                        !p.color=pbackground
;                        !p.background=pcolor

        setupct, r, g, b, coltable=stash.coltable, $
        	white_on_black=stash.white_on_black		; setup color table
			
                        plotpoints,stash
;	endif


  ENDIF


  IF (uval eq "Square psym") THEN BEGIN
        stash.sym=1
        plotpoints,stash
  ENDIF
  IF (uval eq "Round psym") THEN BEGIN
        stash.sym=2
        plotpoints,stash
  ENDIF

  IF (uval eq "incsym") THEN BEGIN
  	stash.symscale=stash.symscale*1.41421
        plotpoints,stash
  ENDIF
  IF (uval eq "decsym") THEN BEGIN
  	stash.symscale=stash.symscale/1.41421
        plotpoints,stash
  ENDIF
  IF (uval eq "resetsym") THEN BEGIN
  	stash.symscale=1.0
        plotpoints,stash
  ENDIF

  IF (uval eq "vertgrad") THEN BEGIN
  	stash.vertgrad=1-stash.vertgrad
        Widget_Control, stash.vertgradmenu, Set_Button=stash.vertgrad
        plotpoints,stash
  ENDIF

  IF (uval eq "Info") THEN BEGIN
         infowindow
  ENDIF

  IF (uval eq "input max/min") THEN BEGIN
        toplevel=stash.base
        fmx=stash.fmx
        fmn=stash.fmn
        mx=stash.mx
        mn=stash.mn
   	inputmxmnwindow, fmx, fmn, mx, mn, stash.rmdi, toplevel, success
	if (success eq 1) then begin
        stash.fmx=fmx
        stash.fmn=fmn
        plotpoints,stash
        endif
        
;        stash.mx=mx
;        stash.mn=mn      
;        info,stash.fmx
;        info,stash.fmn
;        info,stash.mx
;        info,stash.mn

  ENDIF

	  
; store values
  WIDGET_CONTROL, ev.TOP, SET_UVALUE=stash 

;  IF (TAG_NAMES(ev, /STRUCTURE_NAME) eq 'WIDGET_BUTTON') THEN BEGIN 
  IF (uval eq "DONE") THEN BEGIN
;    WIDGET_CONTROL, ev.TOP, /DESTROY 
    WIDGET_CONTROL, stash.base, /DESTROY 
  ENDIF 



;	WIDGET_CONTROL, stash.label3, SET_VALUE=TAG_NAMES(ev, /STRUCTURE_NAME)
;	print,TAG_NAMES(ev, /STRUCTURE_NAME), uval
 
END 

;-----------------
; proceedures
;-----------------

;setup area ranges

PRO areasel, uval, xrange, yrange, success, toplevel=toplevel

;info, xrange
;info, yrange

success=0
  
  IF (uval eq "Global") THEN BEGIN
    	xrange=[-180.,180.]
        yrange=[-90.,90.]
        success=1
  ENDIF
  IF (uval eq "Arctic") THEN BEGIN
    	xrange=[-180.,180.]
        yrange=[70.,90.]
        success=1
  ENDIF
  IF (uval eq "N Atl") THEN BEGIN
    	xrange=[-100.,0.]
        yrange=[25.,70.]
        success=1
  ENDIF
  IF (uval eq "Trop Atl") THEN BEGIN
    	xrange=[-80.,20.]
        yrange=[-25.,25.]
        success=1
  ENDIF
  IF (uval eq "S Atl") THEN BEGIN
    	xrange=[-70.,20.]
        yrange=[-50.,-25.]
        success=1
  ENDIF
  IF (uval eq "N Pac") THEN BEGIN		
        xrange=[120.,-100.+360.]
        yrange=[25.,70.]
        success=1
  ENDIF
  IF (uval eq "Trop Pac") THEN BEGIN
    	xrange=[120.,-80.+360.]
        yrange=[-25.,25.]
        success=1
  ENDIF
  IF (uval eq "S Pac") THEN BEGIN
    	xrange=[120.,-70.+360.]
        yrange=[-50.,-25.]
        success=1
  ENDIF
  IF (uval eq "Indian") THEN BEGIN
    	xrange=[20.,120.]
        yrange=[-50.,30.]
        success=1
  ENDIF
  IF (uval eq "S Ocean") THEN BEGIN
    	xrange=[-180.,180.]
        yrange=[-90.,-50.]
        success=1
  ENDIF
  IF (uval eq "Pacific") THEN BEGIN
    	xrange=[120.,-80.+360.]
        yrange=[-50.,70.]
        success=1
  ENDIF
   IF (uval eq "Atlantic") THEN BEGIN
    	xrange=[-100.,20.]
        yrange=[-50.,70.]
        success=1
  ENDIF
   IF (uval eq "Med") THEN BEGIN
    	xrange=[-15.,38.]
        yrange=[30.,46.]
        success=1
  ENDIF
   IF (uval eq "NWS") THEN BEGIN
    	xrange=[-20.,13.]
        yrange=[40.,65.]
        success=1
  ENDIF
   IF (uval eq "Input Area") THEN BEGIN
   	IF (n_elements(toplevel) gt 0) THEN BEGIN
        print,'success b4',success
   	inputareawindow, xrange, yrange, toplevel, success
        print,'success af',success
        ENDIF
   ENDIF

END

 
;select points to plot

PRO selpoints, stash, lonsel, latsel, innovsel, qcsel, daysel, obstypsel, obnumsel, numsel, typestr, $
	daymin=daymin, daymax=daymax, salinity=salinity, rmsmean=rmsmean, innovsel2=innovsel2

;		print, 'calling selpoints ',stash.netcdf, stash.txt
      print, 'calling selpoints'

		xarr1=stash.xarr
		yarr1=stash.yarr
		dep1=stash.dep
                if (n_elements(salinity) eq 0) then salinity=stash.salinity
		density=stash.density
                mld=stash.mld
      filetype=stash.filetype

		obnum=stash.obnum
;		obnum=lindgen(n_elements(xarr1))	; generate an array of observation numbers
;                					; starting with zero

;	        if (salinity eq 0) then begin                  
;		  obs1=stash.obs
;		  bkg1=stash.bkg
;		  qcarr=stash.qcarr
;                endif else begin
;  		  obs1=stash.obs2
;		  bkg1=stash.bkg2
;		  qcarr=stash.qcarr2
;                endelse              

		if (salinity eq 0) then qcarr=stash.qcarr
      if (salinity eq 1 or mld eq 1) then qcarr=stash.qcarr2
      if (density  eq 1) then begin
         if (filetype eq 'CRT') then qcarr=stash.qcarr $
         else qcarr=max([[stash.qcarr],[stash.qcarr2]],dim=2)
      endif

		obstypes1=stash.obstypes
		depmin=stash.depmin
		depmax=stash.depmax
		xrange=stash.xrange
		yrange=stash.yrange
		dayarr=stash.dayarr
                fdayarr=long(dayarr+stash.dayshi)	; 0:00 hours should be plotted in prev day 
		if (n_elements(daymin) eq 0) then daymin=stash.daymin
		if (n_elements(daymax) eq 0) then daymax=stash.daymax
		fdaymin=long(daymin)
                fdaymax=long(daymax)
		typeplot=stash.typeplot
                ombtypeplot=stash.ombtypeplot
		typeproj=stash.typeproj
		xrange=stash.xrange
		yrange=stash.yrange
		rmdi=stash.rmdi

		print,'dayarr ',max(dayarr)-2454710d, min(dayarr)-2454710d
                print,'dayarr a',max(dayarr+stash.dayshi)-2454710d,min(dayarr+stash.dayshi)-2454710d
		print,'stash.dayshi ',stash.dayshi

		print,'fdayarr mx/mn ',max(fdayarr),min(fdayarr)
                print,'fdaymin ',fdaymin,' fdaymax ',fdaymax

		plot_bad_obs=stash.plot_bad_obs
		
		typestr=''

		if (typeproj eq 2) then begin
			xrange=[-999., 999.]
			yrange=[0., 90.]
		endif
		if (typeproj eq 3) then begin
			xrange=[-999., 999.]
			yrange=[-90, 0.]
		endif

; add 360 if we're in the pacific

		if (xrange(1) gt 180 and typeproj eq 1) then begin
                print,'xrange(1) gt 180 ',xrange
		    wh=where(xarr1 lt 0)
                    xarr1(wh)=xarr1(wh)+360
                endif

		print,'dayarr(0): ',dayarr(0),fdayarr(0), daymin, daymax
		
;		if (plot_bad_obs eq 1) then begin
		wh=where(dep1 ge depmin and dep1 le depmax and $
			fdayarr ge fdaymin and fdayarr le fdaymax and $
			xarr1 ge xrange(0) and xarr1 le xrange(1) and $
			yarr1 ge yrange(0) and yarr1 le yrange(1))
;		endif else begin
;		wh=where(dep1 ge depmin and dep1 le depmax and $
;			fdayarr ge fdaymin and fdayarr le fdaymax and $
;			xarr1 ge xrange(0) and xarr1 le xrange(1) and $
;			yarr1 ge yrange(0) and yarr1 le yrange(1) and qcarr eq 0)
;		endelse		

		nelwh=n_elements(wh)

		print,'nelwh ',nelwh
	
		if (wh(0) gt -1) then begin
;		   obssel=obs1(wh)
;		   bkgsel=bkg1(wh)
                                     
		   qcsel=qcarr(wh)
		   lonsel=xarr1(wh)
		   latsel=yarr1(wh)
		   daysel=dayarr(wh)
		   depsel=dep1(wh)
		   obstypsel=obstypes1(wh)
         obnumsel=obnum(wh)
         numsel=lonarr(nelwh)

		   nel=n_elements(obssel)
                   
; select points here to avoid unnecessary density calculations                   

         obssel1=stash.obs(wh)
         bkgsel1=stash.bkg(wh)
         obssel2=stash.obs2(wh)
         bkgsel2=stash.bkg2(wh)

         if (density eq 0 and mld eq 0) then begin   
            if (salinity eq 0) then begin                  
      		   obssel=obssel1
		         bkgsel=bkgsel1
            endif else begin
  		         obssel=obssel2
		         bkgsel=bkgsel2
            endelse              
         endif else begin
            if (filetype eq 'CRT') then begin			 
               bkgsel=sqrt(bkgsel1*bkgsel1 + bkgsel2*bkgsel2)
               if (salinity eq 0) then begin
                  obssel=sqrt(obssel1*obssel1 + obssel2*obssel2)
               endif else begin
                  obssel=stash.obs3(wh)                
               endelse
            endif else begin				; calculate density if mld flag set
               obssel=eos_nemo(obssel1,obssel2)
               bkgsel=eos_nemo(bkgsel1,bkgsel2)
            endelse
         endelse

; create an array to store the sum of the square differences for calculating profile rms
;		   omb2sel=(obssel - bkgsel)^2
         nel=n_elements(obssel)
      endif else begin			; if there are no obs then exit subroutine
         nel=0
			return
                endelse  

		if ((stash.txt eq 1 or stash.netcdf eq 1) and stash.bindata eq 1) then begin
			innovseli=obssel^2
                        innovseli2=bkgsel^2
                        innovseli3=(obssel-bkgsel)^2
                        innovseli4=obssel*bkgsel
		endif else begin

                             
		if (ombtypeplot eq 1) then begin
                	typestr='obs - bkg'
                        innovseli=obssel-bkgsel
                endif
		if (ombtypeplot eq 2) then begin
                	typestr='obs'
                        innovseli=obssel
                endif
		if (ombtypeplot eq 3) then begin
                	typestr='bkg'
                        innovseli=bkgsel
                endif

		if (keyword_set(rmsmean)) then begin
                	innovseli=innovseli
                        innovseli2=innovseli^2
                endif else begin

		if (typeplot eq 1) then begin
                	typestr='mean '+typestr
                endif
                if (typeplot eq 2) then begin
                	typestr='rms '+typestr
                        innovseli=innovseli^2
                endif
		if (typeplot eq 3) then begin
                	typestr='sd '+typestr
                        innovseli=innovseli^2		; ?
                endif
                if (typeplot eq 4) then begin
                	typestr='mean sq '+typestr
                        innovseli=innovseli^2
                endif

		endelse

		endelse

;      print,'bkgsel ',bkgsel
;      print,'obssel ',obssel

; tolerance check

		if (wh(0) gt -1) then begin
		   wh2=[-1]

;	print,qcsel
                   
                   if (plot_bad_obs eq 0) then begin
;		   if (typeplot eq 1 or typeplot eq 2 $
;			or typeplot eq 3) then $
		   if (ombtypeplot eq 1) then $
;			wh2=where(bkgsel ne rmdi and obssel ne rmdi)
			wh2=where(abs(bkgsel-rmdi) gt abs(rmdi)*0.01 and abs(obssel-rmdi) gt $
	                        abs(rmdi)*0.01 and qcsel eq 0)	; tolerance rmdi check
		   if (ombtypeplot eq 2) then $
;                   if (typeplot eq 4) then $
;		   	wh2=where(obssel ne rmdi)
			wh2=where(abs(obssel-rmdi) gt abs(rmdi)*0.01 and qcsel eq 0)	;tol rmdi check
		   if (ombtypeplot eq 3) then $
;		   if (typeplot eq 5) then $	
;			wh2=where(bkgsel ne rmdi)
			wh2=where(abs(bkgsel-rmdi) gt abs(rmdi)*0.01 and qcsel eq 0)	;tol rmdi check

; check for bad observations
;		if (plot_bad_obs eq 1) then begin
		endif else if (plot_bad_obs eq 1 and stash.plot_only_bad_obs eq 1) then begin
;                	wh2=where(qcsel ne 0 or obssel eq rmdi or bkgsel eq rmdi, count)
;                        wh2=where(qcsel ne 0 or abs(obssel-rmdi) lt abs(rmdi)*0.01,count) 
			wh2=where(qcsel ne 0 and obssel ne rmdi)

;                        wh2=where(qcsel ne 0 or abs(obssel-rmdi) lt abs(rmdi)*0.01 or abs(bkgsel-rmdi) lt abs(rmdi)*0.01,count) 
;                        print,'bad obs: ',count
;                        whtmp=where(qcsel eq 0, count)
;                        print,'qcsel eq 0: ',count
;                        whtmp=where(obssel eq 0, count)
;                        print,'obssel eq 0: ',count
;                        whtmp=where(bkgsel eq 0, count)
;                        print,'bkgsel eq 0: ',count
                endif else begin				; plot all observations
;                	wh2=where(obssel)       
                        wh2=where(obssel ne rmdi)
                endelse

		   if (wh2(0) gt -1) then begin
		      obssel=obssel(wh2)
		      bkgsel=bkgsel(wh2)
                      innovseli=innovseli(wh2)
;                      if ((stash.txt eq 1 or stash.netcdf eq 1) and stash.bindata eq 1) then begin
                      if (n_elements(innovseli2) gt 0) then innovseli2=innovseli2(wh2)
                      if (n_elements(innovseli3) gt 0) then innovseli3=innovseli3(wh2)
                      if (n_elements(innovseli4) gt 0) then innovseli4=innovseli4(wh2)
;		      endif
		      qcsel=qcsel(wh2)
		      lonsel=lonsel(wh2)
		      latsel=latsel(wh2)
		      daysel=daysel(wh2)
		      depsel=depsel(wh2)
		      obstypsel=obstypsel(wh2)
                      obnumsel=obnumsel(wh2)
;                      numsel=numsel(wh2)
		   endif

		print,'nelwh2 ',n_elements(wh2), typeplot, ombtypeplot
		       
		if (wh2(0) gt -1) then begin
                
                ; calculate vertical gradients if required
	if (stash.filetype eq "Prof" and stash.vertgrad eq 1) then $
	calcvertgrad, lonsel, latsel, daysel, obssel, bkgsel, qcsel, depsel, rmdi

; average profiles on the same latitude and longitude
; goes through each point and puts the average in the first instance and flags 
; the others for later removal
; this routine can also be used to bin data
		depmx=max(depsel,min=depmn)
		print,'depmn/mx: ',depmn, depmx
		if (depmx gt depmn or stash.duplicates gt 0 or stash.differences or stash.bindata) then begin

; sort by lat and lon
; presumably will average in time also...

		if (stash.bindata) then begin		; if we are binning data (in 1 deg bins)
                					; use +0.5 to get nearest round number 
		  latbin=double(fix((latsel+stash.binsize(1)/2.)*(1/stash.binsize(1))))/(1/stash.binsize(1))
		  lonbin=double(fix((lonsel+stash.binsize(0)/2.)*(1/stash.binsize(0))))/(1/stash.binsize(0))			
		  latlon=long(latbin*10000000d)+double(lonbin+180d)/360d
                endif else begin
		  latlon=long(double(latsel)*10000000d)+double(lonsel+180d)/360d
                endelse
                
		elsort=sort(latlon)

		latsel=latsel(elsort)
		lonsel=lonsel(elsort)
		obssel=obssel(elsort)
                bkgsel=bkgsel(elsort)
                innovseli=innovseli(elsort)
;                if ((stash.txt eq 1 or stash.netcdf eq 1) and stash.bindata eq 1) then begin
                if (n_elements(innovseli2) gt 0) then innovseli2=innovseli2(elsort)
                if (n_elements(innovseli3) gt 0) then innovseli3=innovseli3(elsort)
                if (n_elements(innovseli4) gt 0) then innovseli4=innovseli4(elsort)
;		endif
		qcsel=qcsel(elsort)
                daysel=daysel(elsort)
                depsel=depsel(elsort)
		obstypsel=obstypsel(elsort)
                obnumsel=obnumsel(elsort)
;                numsel=numsel(elsort)
		latlon=latlon(elsort)
		
		nel=n_elements(obssel)
                accum=0
                isave=0
                lon1=-99999.0
                lat1=-99999.0
                latlon1=-99999.0
                day1=-99999
                num=0
 
; 		if (nel gt 100) then begin
;                info,latsel
; 		print,latsel(0:100)
;                info,lonsel
;                print,lonsel(0:100)
;                info,latlon
;		print,latlon(0:100)
 
; 		latsel(0:100)=latsel(50)
;                lonsel(0:100)=lonsel(50)
; 		latlon=long(double(latsel)*10000000d)+double(lonsel+180d)/360d
 
;                endif
                

		print,'DJL ',max(daysel),min(daysel)

; loop through the data averaging profiles at the same location               
 
                sign=1                                            
                if (stash.duplicates eq 2) then sign=-1		; plot the difference rather than the average	
  
                numsel=lonarr(nel)
		for i=0L,nel-1 do begin
                
                if i mod 100000 eq 0 then print,i,nel

;		if (plot_bad_obs eq 0 and obssel(i) ne rmdi) or $
;                      	(plot_bad_obs eq 1 and lonsel(i) ne rmdi) then begin
		
; 		if (lonsel(i) ne lon1 or latsel(i) ne lat1) then begin

; check for new profile or reaching the end of the data
; check for the end of a day added

		if ((latlon(i) ne latlon1) or (i eq nel-1) or (long(daysel(i)) ne long(day1))) then begin		

;		print,i, latlon(i), latlon1, daysel(i), day1                
;                if i gt 0 then stop
		
; average last profile
		  if (isave ge 0 and num gt 0) then begin
                
                    if (num gt 1) then begin    
                
;                	print,num
                        numsave=num
                        if (stash.duplicates eq 2) then num=1

			if (stash.bindata) then begin 
 			latsel(isave)=latsel(isave)/num
                        lonsel(isave)=lonsel(isave)/num
                        endif                          
                	obssel(isave)=obssel(isave)/num
                        bkgsel(isave)=bkgsel(isave)/num
                        daysel(isave)=daysel(isave)/num
                        depsel(isave)=depsel(isave)/num
                        innovseli(isave)=innovseli(isave)/num
;                        if ((stash.txt eq 1 or stash.netcdf eq 1) and stash.bindata eq 1) then begin
                        if (n_elements(innovseli2) gt 0) then innovseli2(isave)=innovseli2(isave)/num
                        if (n_elements(innovseli3) gt 0) then innovseli3(isave)=innovseli3(isave)/num
                        if (n_elements(innovseli4) gt 0) then innovseli4(isave)=innovseli4(isave)/num
;		        endif

                        num=numsave

; save the num	
			numsel(isave)=num

;                        if (fix(lonsel(isave)) eq -7297 and $
;                            fix(latsel(isave)) eq -5935) then begin
;                         print,'completed ',isave, num                        
;			 print,'obssel ',obssel(isave)
;                         print,'bkgsel ',bkgsel(isave)
;                         endif
                         
                    endif else begin
                    	numsel(isave)=num
                    endelse
                    if ((stash.duplicates gt 0 and num eq 1) or (stash.differences and num gt 1)) then begin
                        obssel(isave)=rmdi
                        bkgsel(isave)=rmdi
                        lonsel(isave)=rmdi
                        latsel(isave)=rmdi
                        qcsel(isave)=rmdi
                        innovseli(isave)=rmdi
;                        if ((stash.txt eq 1 or stash.netcdf eq 1) and stash.bindata eq 1) then begin
                        if (n_elements(innovseli2) gt 0) then innovseli2(isave)=rmdi
                        if (n_elements(innovseli3) gt 0) then innovseli3(isave)=rmdi
                        if (n_elements(innovseli4) gt 0) then innovseli4(isave)=rmdi
;  		        endif
                        
                    endif
                                           
		  endif 

		if (plot_bad_obs eq 0 and obssel(i) ne rmdi) or $
                      	(plot_bad_obs eq 1 and lonsel(i) ne rmdi) then begin

; start a new profile
       		      lon1=lonsel(i)
		      lat1=latsel(i)
                      latlon1=latlon(i)
                      day1=daysel(i)
                      isave=i
                      accum=1
                      num=1

		endif		; plot_bad_obs ...
                      
 ; accumulating                     
                      
		endif else begin
                	
		if (plot_bad_obs eq 0 and obssel(i) ne rmdi) or $
                      	(plot_bad_obs eq 1 and lonsel(i) ne rmdi) then begin
                        
                	num=num+1

			if (stash.bindata) then begin
                        latsel(isave)=latsel(isave)+double(fix((1./stash.binsize(1))*(latsel(i)+stash.binsize(1)/2.)))/(1./stash.binsize(1))	;latsel(isave)+fix(latsel(i)+0.5)
                        lonsel(isave)=lonsel(isave)+double(fix((1./stash.binsize(0))*(lonsel(i)+stash.binsize(0)/2.)))/(1./stash.binsize(0))	;lonsel(isave)+fix(lonsel(i)+0.5)
                        endif

                        obssel(isave)=obssel(isave)+obssel(i)*sign
                        bkgsel(isave)=bkgsel(isave)+bkgsel(i)*sign
                        innovseli(isave)=innovseli(isave)+innovseli(i)*sign
;                        if ((stash.txt eq 1 or stash.netcdf eq 1) and stash.bindata eq 1) then begin
                        if (n_elements(innovseli2) gt 0) then innovseli2(isave)=innovseli2(isave)+innovseli2(i)*sign
                        if (n_elements(innovseli3) gt 0) then innovseli3(isave)=innovseli3(isave)+innovseli3(i)*sign
                        if (n_elements(innovseli4) gt 0) then innovseli4(isave)=innovseli4(isave)+innovseli4(i)*sign
; 		        endif

                        qcsel(isave)=min([qcsel(isave),qcsel(i)])
                        daysel(isave)=daysel(isave)+daysel(i)
                        depsel(isave)=depsel(isave)+depsel(i)

			if (stash.bindata) then begin
                        latsel(i)=rmdi
                        lonsel(i)=rmdi               
                        endif         
                        obssel(i)=rmdi
                        bkgsel(i)=rmdi
                        innovseli(i)=rmdi
;                        if ((stash.txt eq 1 or stash.netcdf eq 1) and stash.bindata eq 1) then begin
                        if (n_elements(innovseli2) gt 0) then innovseli2(i)=rmdi
                        if (n_elements(innovseli3) gt 0) then innovseli3(i)=rmdi
                        if (n_elements(innovseli4) gt 0) then innovseli4(i)=rmdi
; 		        endif
                        lonsel(i)=rmdi
                        latsel(i)=rmdi
                        qcsel(i)=rmdi

;                        if (fix(lonsel(isave)*100) eq -7297 and $
;                       	    fix(latsel(isave)*100) eq -5935) then begin
;                         print,'isave ',isave, num                        
;			  print,'obssel ',obssel(isave)
;                         print,'bkgsel ',bkgsel(isave)
;                        endif

		endif		; plot_bad_obs ...
                        
                endelse 
                      
;                endif

                endfor		; i  nobs

		print,'DJL2x ',max(daysel),min(daysel)


		wh5=where(lonsel ne rmdi)				; clears out but the averaged profiles
                if (wh5(0) gt -1) then begin
		      obssel=obssel(wh5)
		      bkgsel=bkgsel(wh5)
                      innovseli=innovseli(wh5)
;                      if ((stash.txt eq 1 or stash.netcdf eq 1) and stash.bindata eq 1) then begin
                      if (n_elements(innovseli2) gt 0) then innovseli2=innovseli2(wh5)
                      if (n_elements(innovseli3) gt 0) then innovseli3=innovseli3(wh5)
                      if (n_elements(innovseli4) gt 0) then innovseli4=innovseli4(wh5)
;		      endif
		      qcsel=qcsel(wh5)
		      lonsel=lonsel(wh5)
		      latsel=latsel(wh5)
		      daysel=daysel(wh5)
		      obstypsel=obstypsel(wh5)
                      obnumsel=obnumsel(wh5)
                      numsel=numsel(wh5)
                      nel=n_elements(obssel)
                endif else begin			; if there are no obs then exit subroutine
                	nel=0
			return
                endelse               
		endif

		print,'nel wh5 ',n_elements(wh5)
		print,'DJL3 ',max(daysel),min(daysel)

		typeselect=stash.obstypeselect
;                print, 'typeselect ',typeselect
		if (typeselect(0) ne "") then begin
;                typeselecta=strsplit(typeselect,' ,',/extract)		; split string on commas or spaces
		typeselecta=strsplit(typeselect,',',/extract)		; split string on commas only
                nel=n_elements(typeselecta)
                print,'n_elements(typeselecta) ',nel
                for i=0L,nel-1 do begin
                ; string matching (understands * ?)
                st1=strtrim(string(obstypsel),2)		; strip tailing and leading spaces
                st2=strtrim(typeselecta(i),2)
                wh6t=where(strmatch(st1,st2,/FOLD_CASE) EQ 1)	
;                print, 'wh6t ',typeselecta(i), wh6t
		if i eq 0 then wh6=wh6t else wh6=[wh6,wh6t]
                endfor 
;                print,n_elements(wh6)
;                print,wh6
                wh=where(wh6 gt -1)
;                print,wh
                if (wh(0) gt -1) then begin
                      wh6=wh6(wh)
		      obssel=obssel(wh6)
		      bkgsel=bkgsel(wh6)
                      innovseli=innovseli(wh6)
;                      if ((stash.txt eq 1 or stash.netcdf eq 1) and stash.bindata eq 1) then begin
                      if (n_elements(innovseli2) gt 0) then innovseli2=innovseli2(wh6)
                      if (n_elements(innovseli3) gt 0) then innovseli3=innovseli3(wh6)
                      if (n_elements(innovseli4) gt 0) then innovseli4=innovseli4(wh6)
;		      endif
		      qcsel=qcsel(wh6)
		      lonsel=lonsel(wh6)
		      latsel=latsel(wh6)
		      daysel=daysel(wh6)
		      obstypsel=obstypsel(wh6)
                      obnumsel=obnumsel(wh6)
                      numsel=numsel(wh6)
                      nel=n_elements(obssel)
;                      print, 'nel (in obstypeselect) ',nel
                endif else begin			; if there are no obs then exit subroutine
                	print,'no matching obs'
			nel=0
                endelse               
		endif                	
                
                if (stash.printobstypes eq 1) then begin
		s=sort(obstypsel)
                sobstypsel=obstypsel(s)
		u=uniq(sobstypsel)
		print,"unique obs types: ",sobstypsel(u) 
		endif

		print,'nel ',nel
		stash.symsize=1.0
		if (nel gt 250) then stash.symsize=0.75
		if (nel gt 1000) then stash.symsize=0.5
		if (nel gt 2000) then stash.symsize=0.25
		if (nel lt 20) then stash.symsize=2.0

		if (!d.name eq 'Z') then stash.symsize=stash.symsize/2

		print,'stash.symsize: ',stash.symsize

; save data for mean and rms plots
; for ombtypeplot=3 typeplot=2 innovsel = (obs - bkg)^2		mean value available from obssel(i) - bkgsel(i)

; copy innovseli to innovsel (if we've got some valid data to plot / save)
		if (nel gt 0) then innovsel=innovseli

		if (stash.txt eq 1 and stash.bindata eq 1) then begin
 
; The file contains:  longitude, latitude, num of points, obs mean, bkg mean
;         mean sq obs,    mean sq bkg,        mean sq obs - bkg, 	mean obs * bkg 
                
                outfile=stash.outfile+'_selpoints.txt'
                print, 'saving data to ',outfile
                OPENW, unit, outfile, /get_lun
;   	           printf,unit,'Output selpoints data: '
                printf,unit, 3					; file version
                printf,unit, typeplot
                printf,unit, stash.bindata
                printf,unit, xrange, yrange
                printf,unit, min(daysel), max(daysel) 

                for i=0L,nel-1 do begin
                	   printf,unit, lonsel(i), latsel(i), numsel(i), obssel(i), bkgsel(i), $
                        	   innovseli(i), innovseli2(i), innovseli3(i), innovseli4(i), $
                        	   format='(2d10.3, i8, 6d18.8)'
                endfor
                FREE_LUN, unit
               
      endif

		if (stash.netcdf eq 1 and stash.bindata eq 1) then begin

                outfile=stash.outfile+'_selpoints.nc'
                print, 'saving data to ',outfile

                nid=NCDF_CREATE( outfile, /CLOBBER )
                
                NCDF_ATTPUT, nid, 'version', 3, /SHORT, /GLOBAL
                NCDF_ATTPUT, nid, 'bindata', stash.bindata, /FLOAT, /GLOBAL
                NCDF_ATTPUT, nid, 'xrange0', xrange(0), /FLOAT, /GLOBAL
                NCDF_ATTPUT, nid, 'xrange1', xrange(1), /FLOAT, /GLOBAL
                NCDF_ATTPUT, nid, 'yrange0', yrange(0), /FLOAT, /GLOBAL
                NCDF_ATTPUT, nid, 'yrange1', yrange(1), /FLOAT, /GLOBAL
                NCDF_ATTPUT, nid, 'mindaysel', min(daysel), /FLOAT, /GLOBAL
                NCDF_ATTPUT, nid, 'maxdaysel', max(daysel), /FLOAT, /GLOBAL
                
                print, 'nel ',nel
                nelid = NCDF_DIMDEF( nid, 'n', nel )
                
                lonid = NCDF_VARDEF ( nid, 'longitudes', [nelid], /FLOAT )
                latid = NCDF_VARDEF ( nid, 'latitudes', [nelid], /FLOAT ) 
                numid = NCDF_VARDEF ( nid, 'num', [nelid], /LONG )
                obsid = NCDF_VARDEF ( nid, 'obs_mean', [nelid], /FLOAT )
                bkgid = NCDF_VARDEF ( nid, 'bkg_mean', [nelid], /FLOAT )
                innovseli1id = NCDF_VARDEF ( nid, 'obs_mean_sq', [nelid], /FLOAT )
                innovseli2id = NCDF_VARDEF ( nid, 'bkg_mean_sq', [nelid], /FLOAT )
                innovseli3id = NCDF_VARDEF ( nid, 'omb_mean_sq', [nelid], /FLOAT )
                innovseli4id = NCDF_VARDEF ( nid, 'otimesb_mean', [nelid], /FLOAT )
                NCDF_CONTROL, nid, /ENDEF
                
                if (nel gt 0) then begin
                NCDF_VARPUT, nid, lonid, lonsel
                NCDF_VARPUT, nid, latid, latsel
                NCDF_VARPUT, nid, numid, numsel
                NCDF_VARPUT, nid, obsid, obssel
                NCDF_VARPUT, nid, bkgid, bkgsel
                NCDF_VARPUT, nid, innovseli1id, innovseli
                NCDF_VARPUT, nid, innovseli2id, innovseli2
                NCDF_VARPUT, nid, innovseli3id, innovseli3
                NCDF_VARPUT, nid, innovseli4id, innovseli4
                endif
                
                NCDF_CLOSE, nid
                
		endif

      if (stash.filterout ne '') then begin

         ; write out filtered feedback file in new feedback format
         ; with some filthy fudges to emulate NEMOVAR format
         if (nel gt 0) then begin

            print, 'writing filtered feedback file ',stash.filterout

            ; split type string in two to be used as STATION_IDENTIFIER and STATION_TYPE
            typesel=strarr(nel)
            instsel=strarr(nel)

            for iob = 0, nel-1 do begin
               tempsel=strsplit(obstypsel[iob],/extract)
               typesel[iob]=tempsel[0]
               ; catch for data without space and instrument id in types (e.g. altimeter)
               instsel[iob]=''
               if (n_elements(tempsel) gt 1 ) then $
                  instsel[iob]=tempsel[1]
            endfor

            ; setup netcdf file
            nid=NCDF_CREATE( stash.filterout, /CLOBBER )

            ; global attribute to emulate NEMOVAR feedback files
            NCDF_ATTPUT, nid, 'title', "NEMO observation operator output", /GLOBAL
            if (stash.obstypeselect ne '' ) then $
               NCDF_ATTPUT, nid, 'filter', stash.obstypeselect, /GLOBAL
            NCDF_ATTPUT, nid, 'minday', min(daysel), /FLOAT, /GLOBAL
            NCDF_ATTPUT, nid, 'maxday', max(daysel), /FLOAT, /GLOBAL

            nobid = NCDF_DIMDEF( nid, 'N_OBS', nel )
            ndepid = NCDF_DIMDEF( nid, 'N_LEVELS', 1 ) ; temporarily input 1d profile arrays

            if (stash.filetype eq 'feedback') then begin

               ncvars = stash.varname
               nnvars = n_elements(ncvars)

            endif else begin

               nnvars = 1
               ncvars = stash.filetype
               if (stash.filetype eq 'Prof') then begin
                  ncvars = 'POTM'
                  if (salinity eq 1) then ncvars = 'PSAL'
                  nextid = NCDF_DIMDEF( nid, 'N_EXTRA', 1 )
               endif 

            endelse

            nvarid = NCDF_DIMDEF( nid, 'N_VARS', nnvars )

            nentid = NCDF_DIMDEF( nid, 'N_ENTRIES', 1 )

            nstrnamid = NCDF_DIMDEF( nid, 'STRINGNAM', 8 )
            nstrwmoid = NCDF_DIMDEF( nid, 'STRINGWMO', 8 )
            nstrtypid = NCDF_DIMDEF( nid, 'STRINGTYP', 4 )
            nstrjulid = NCDF_DIMDEF( nid, 'STRINGJULD', 14 )

            varid = NCDF_VARDEF ( nid, 'VARIABLES', [nstrnamid,nvarid], /CHAR )
            entid = NCDF_VARDEF ( nid, 'ENTRIES', [nstrnamid,nentid], /CHAR )
            signid = NCDF_VARDEF ( nid, 'STATION_IDENTIFIER', [nstrwmoid,nobid], /CHAR )
            typeid = NCDF_VARDEF ( nid, 'STATION_TYPE', [nstrtypid,nobid], /CHAR )

            lonid = NCDF_VARDEF ( nid, 'LONGITUDE', [nobid], /DOUBLE )
            latid = NCDF_VARDEF ( nid, 'LATITUDE', [nobid], /DOUBLE )
            depid = NCDF_VARDEF ( nid, 'DEPTH', [nobid], /DOUBLE )
;            depid = NCDF_VARDEF ( nid, 'DEPTH', [ndepid,nobid], /DOUBLE )
            julid = NCDF_VARDEF ( nid, 'JULD', [nobid], /DOUBLE )
            jrefid = NCDF_VARDEF ( nid, 'JULD_REFERENCE', [nstrjulid], /CHAR )

            obsqcid = NCDF_VARDEF ( nid, 'OBSERVATION_QC', [nobid], /LONG )

            FillVal=9999
            NCDF_ATTPUT, nid, depid, '_Fillvalue', FillVal, /DOUBLE
            NCDF_ATTPUT, nid, obsqcid, '_Fillvalue', FillVal, /LONG

            obsids=intarr(nnvars)
            bkgids=intarr(nnvars)
            qcids=intarr(nnvars)
            lqcids=intarr(nnvars)

            for ivar = 0, nnvars-1 do begin

               obsids[ivar] = NCDF_VARDEF ( nid, ncvars[ivar]+'_OBS', [nobid], /FLOAT )
               NCDF_ATTPUT, nid, obsids[ivar], '_Fillvalue', FillVal, /FLOAT
               bkgids[ivar] = NCDF_VARDEF ( nid, ncvars[ivar]+'_Hx', [nobid], /FLOAT )
               NCDF_ATTPUT, nid, bkgids[ivar], '_Fillvalue', FillVal, /FLOAT
               qcids[ivar] = NCDF_VARDEF ( nid, ncvars[ivar]+'_QC', [nobid], /LONG )
               NCDF_ATTPUT, nid, qcids[ivar], '_Fillvalue', FillVal, /FLOAT
               lqcids[ivar] = NCDF_VARDEF ( nid, ncvars[ivar]+'_LEVEL_QC', [nobid], /LONG )
               NCDF_ATTPUT, nid, lqcids[ivar], '_Fillvalue', FillVal, /FLOAT

            endfor

            NCDF_CONTROL, nid, /ENDEF

            JulDRef = '19500101000000'
            NCDF_VARPUT, nid, jrefid, JulDRef
            RefDate = JULDAY(fix(strmid(JulDRef,4,2)), fix(strmid(JulDRef,6,2)), fix(strmid(JulDRef,0,4)), $
                             fix(strmid(JulDRef,8,2)), fix(strmid(JulDRef,10,2)), fix(strmid(JulDRef,12,2)))
            NCDF_VARPUT, nid, julid,daysel-RefDate 

            NCDF_VARPUT, nid, varid, ncvars 
            NCDF_VARPUT, nid, entid, 'Hx'

            NCDF_VARPUT, nid, lonid, lonsel
            NCDF_VARPUT, nid, latid, latsel

            NCDF_VARPUT, nid, depid, fltarr(nel)
            NCDF_VARPUT, nid, obsqcid, fltarr(nel)+1

            for ivar = 0, nnvars-1 do begin

               NCDF_VARPUT, nid, obsids[ivar], obssel
               NCDF_VARPUT, nid, bkgids[ivar], bkgsel
               NCDF_VARPUT, nid, qcids[ivar], qcsel+1
               NCDF_VARPUT, nid, lqcids[ivar], fltarr(nel)+1

            endfor

            NCDF_VARPUT, nid, typeid, typesel
            NCDF_VARPUT, nid, signid, instsel

         endif

         NCDF_CLOSE, nid

      endif ; filterout

		if (nel gt 0) then begin	

		   if (typeplot eq 2) then begin
                        innovsel=sqrt(innovsel)
                   endif
		   if (typeplot eq 3) then begin
;  std dev = sqrt ( mean x2 - (mean x)^2)
			x2=innovsel
			print,'typeplot ',typeplot,' ombtypeplot ',ombtypeplot
			if (ombtypeplot eq 1) then $
                        	innovsel=sqrt(x2-(obssel-bkgsel)^2)
                        if (ombtypeplot eq 2) then $
                        	innovsel=sqrt(x2-(obssel)^2)                        
                        if (ombtypeplot eq 3) then $
                        	innovsel=sqrt(x2-(bkgsel)^2)                        
                   endif

		   if (keyword_set(rmsmean)) then begin
                   	innovsel=innovseli						; mean 
                        if (n_elements(innovseli2) gt 0) then innovsel2=innovseli2	; rms                  
                   endif


		   innovmean=total(innovsel)/nel
		   innovmean2=total(innovsel^2)/nel
		   innovsd=sqrt(innovmean2-innovmean^2)
		   print,'mean innovsel ',innovmean
		   print,'sd innovsel ',innovsd

		endif

		

		endif

		endif

		wh=where(numsel eq 0)
                if (wh(0) gt -1) then numsel(wh)=1

END

PRO setupct, r, g, b, coltable=coltable, white_on_black=white_on_black

if (coltable eq 0 or coltable lt -2) then begin
; get color table and modify
                loadct, 13
                stretch, -40, 256
;                tvlct,r,g,b,/get                
;                r(0)=0
;                g(0)=0
;                b(0)=0
;                r(255)=255
;                g(255)=255
;                b(255)=255              
;                tvlct,r,g,b
endif else begin
	if (coltable eq -1) then begin
		restore_colors, 'spectrum.clr'
	endif else if (coltable eq -2) then begin
                restore_colors, '~frbe/spectrum_alt.xdr',/asis
        endif else begin
		loadct,coltable
        endelse            
endelse

tvlct,r,g,b,/get
i0=0
i1=255
if (n_elements(white_on_black) eq 1 and !d.name ne "PS") then begin
	if (white_on_black eq 0) then begin
     		i0=255
                i1=0   
        endif
endif
r(i0)=0
g(i0)=0
b(i0)=0
r(i1)=255
g(i1)=255
b(i1)=255              
tvlct,r,g,b

END

PRO plotpoints, stash

		nplots=1
                if (stash.pmulti eq 2) then nplots=stash.pmulti

		for nplot=0,nplots-1 do begin

		if (stash.busy eq 1) then return
		stash.busy = 1

		num_cols=254

		print,"!d.name ",!d.name 
		IF (stash.drawID gt -1) then begin
  		IF (!d.name eq 'X' or !d.name eq 'Z') then begin 
                        device, decomposed=0
	        endif
                IF (!d.name eq 'X') then WSET, stash.drawID
		endif

		IF (!d.name ne 'Z') then setupct, r, g, b, $
                   coltable=stash.coltable,white_on_black=stash.white_on_black		; setup color table

		noerase=0
		if (stash.pmulti eq 2) then begin
                	if (stash.pmultinum gt 0) then noerase=1
                	!p.multi=[stash.pmultinum,2,1]
                        stash.pmultinum=(stash.pmultinum+1) mod 2
                endif else begin
                	!p.multi=0
                endelse
                
;		xarr1=stash.xarr
;		yarr1=stash.yarr
;		dep1=stash.dep
;		obs1=stash.obs
;		bkg1=stash.bkg
;		depmin=stash.depmin
		xrange=stash.xrange
		yrange=stash.yrange
;		dayarr=stash.dayarr
		daymin=stash.daymin
		daymax=stash.daymax
;		typeplot=stash.typeplot
		typeproj=stash.typeproj
;                print,"stash.map_file: ",stash.map_file
;		if (strlen(stash.map_file) gt 0) then map_file=stash.map_file
                                
;		print,depmin,xrange,yrange

  		dummy=[0,0]
		
		
;		plot,dummy,/nodata,yrange=[-90,90],xrange=[-180,180]
;		plot,dummy,/nodata,yrange=[-50,80],xrange=[-110,40],$
;			xstyle=1,ystyle=1, xtitle='Longitude',ytitle='Latitude'

		if (typeproj ne 1) then begin
		    	origlon=0.
			if (typeproj eq 2) then begin 
                        	origlat=90.
                                if (yrange(0) lt 0) then yrange(0)=45
                                if (yrange(1) lt 0) then yrange(1)=90
                        endif
			if (typeproj eq 3) then begin 
                        	origlat=-90.
                                if (yrange(0) gt 0) then yrange(0)=-90
                                if (yrange(1) gt 0) then yrange(1)=-45                               
                        endif      
			;scale=0.35
                        smap = map_proj_init('Polar Stereographic')
		endif

		; select points to plot
		typestr=""
                
spawn,'echo plotpoints 1 `date`'
                
		selpoints, stash, lonsel, latsel, innovsel, qcsel, daysel, obstypsel, obnumsel, numsel, typestr
                
spawn,'echo plotpoints 2 `date`'

		if (stash.txt eq 0 and stash.netcdf eq 0) then begin
		nelsel=n_elements(innovsel)

                nptsstr=strtrim(string(nelsel),2)
                print,nptsstr, nelsel

;		if (nelsel gt 0) then begin



		;convert lats and lons to projection positions
;                if (nelsel gt 0) then begin
;                print,'min/max lonsel: ',min(lonsel),max(lonsel)
;		if (typeproj ne 1) then begin
;                  coords = map_proj_forward(lonsel,latsel,MAP=smap)
;		  lonsel=coords(0,*)
;		  latsel=coords(1,*)
;		endif
;                endif



		print,'nelsel: ',nelsel
		nelinnov=n_elements(innovsel)
		print,'nelinnov: ',nelinnov

                jul_to_dtstr, daymin, dateminstr, /notime
                jul_to_dtstr, daymax, datemaxstr, /notime		

                strtsal=''
;                print,'stash.filetype ',stash.filetype
                if (stash.filetype eq "Prof" or stash.filetype eq "feedback") then begin
                	   strtsal='T:  '
                     if (stash.salinity eq 1) then strtsal='S:  '
                     if (stash.density  eq 1) then strtsal='Density:  '
                     if (stash.mld eq 1) then strtsal='MLD:  '
                endif else begin
                     if (stash.filetype eq "CRT") then begin
                	      strtsal='U:  '
                        if (stash.salinity eq 1) then strtsal='V:  '
                        if (stash.density  eq 1) then strtsal='Speed:  '
                        if (stash.salinity eq 1 and stash.density  eq 1) then strtsal='Total Speed:  '
                     endif else begin
                	      strtsal=stash.filetype+':  '
                     endelse
                endelse
                if (stash.vertgrad eq 1) then strtsal='Grad '+strtsal
		nptsstr2='Points: '+nptsstr+'  '
		titlestr=strtsal+typestr+': '+dateminstr+' to '+datemaxstr
		print,'titlestr ',titlestr

; get max and min values
		mx=0
                mn=0
                meaninnov=0
                rmsinnov=0
		if (nelinnov gt 0) then begin
; NB these values are just the rms and mean of the points plotted...
; and do not take account of the datapoints used in profiles plotted...
		   mx=max(innovsel)
                   mn=min(innovsel)
		   meaninnov=avg(innovsel)
                   rmsinnov=sqrt(avg(innovsel^2))
;                   n=total(numsel)
;                   wh=where(numsel gt 0)
;                   print, 'innovsel: ',max(innovsel), min(innovsel)
;                   print, 'numsel: ',max(numsel), min(numsel)
;                   if (n gt 0) then begin
;                      x=total(innovsel(wh)*numsel(wh))
;                      x2=total(innovsel(wh)^2*numsel(wh))
;                      meaninnov=x/n
;                      rmsinnov=sqrt(x2/n)
;                   endif
                endif

		subtitle=''
		subtitle=nptsstr2+'depths: '+strtrim(string(long(stash.depmin)),2)+'-'+$
                	strtrim(string(long(stash.depmax)),2)
                if (stash.obstypeselect ne "") then subtitle=subtitle+'  filtered type: '+stash.obstypeselect        
 		subtitle=subtitle+'  extrema: '+$
                	strtrim(string(mn,format='(G0.4)'),2)+', '+strtrim(string(mx,format='(G0.4)'),2)
		subtitle=subtitle+'  mean: '+strtrim(string(meaninnov,format='(G0.4)'))+$
                	' rms: '+strtrim(string(rmsinnov,format='(G0.4)'))
		
;                print,'noerase ',noerase
                
		if (typeproj ne 1) then begin                 
                 map_set, /STEREOGRAPHIC, origlat, 0, /continents, title=titlestr+'!C'+subtitle, $
                         ymargin=[10.,5.], /label, latlab=xrange(0), lonlab=yrange(0), $
                         limit=[yrange(0),xrange(0),yrange(1),xrange(1)],/isotropic, hires=stash.hires_map, noerase=noerase
                endif else begin        
                  P0lon=0
                  P0lat=0
                  if (xrange(1) gt 180) then P0lon=180
                  print,'ranges: ',yrange(0),xrange(0),yrange(1),xrange(1)
                  map_set, P0lat, P0lon, /continents, title=titlestr+'!C'+subtitle, $
                         ymargin=[10.,5.], /label, latlab=xrange(0), lonlab=yrange(0), $
                         limit=[yrange(0),xrange(0),yrange(1),xrange(1)],/isotropic, hires=stash.hires_map, noerase=noerase  
                endelse

                                                                 
		if (nelsel eq 0 or nelinnov eq 0) then begin 
;			pp_contour,fld,title=titlestr,/nodata, /proportional, map_file=map_file
		endif else begin
;			pp_contour,fld,title=titlestr,/nodata, /proportional, map_file=map_file
                          
                if (stash.fmx ne stash.rmdi) then mx=stash.fmx
                if (stash.fmn ne stash.rmdi) then mn=stash.fmn

		print,'setting stash mx/mn'
		stash.mx=mx
                stash.mn=mn

;		levs=contour_levels([mn,mx],nlevels=15)
;		levs=findgen(10+1)/10.*(mx-mn)+mn

		print, 'mn mx ',mn,mx



;; De-select points outside window?
;		
;    xnd= [ -!X.s(0), 1. ]/!X.s(1)
;    ynd= [ -!Y.s(0), 1. ]/!Y.s(1) 
;    ov=0                ; overlap 
;    clip_data= [ (!ppp.position(0)-ov)*xnd(1) + xnd(0) , $
;                 (!ppp.position(1)-ov)*ynd(1) + ynd(0) , $
;                 (!ppp.position(2)+ov)*xnd(1) + xnd(0) , $
;                 (!ppp.position(3)+ov)*ynd(1) + ynd(0) ]
;
;	wh=where(lonsel ge clip_data(0) and lonsel le clip_data(2)  and $
;               latsel ge clip_data(1) and latsel  le clip_data(3))
;	       if (wh(0) gt -1) then begin
;	       	latsel=latsel(wh)
;		lonsel=lonsel(wh)
;		innovsel=innovsel(wh)
;	       endif


		print,'daymin=',daymin
		print,'daymax=',daymax
;		print,dayarr(wh)

	
;		titlestr=typestr+': '+titlestr
		


		mx=max(innovsel)
                mn=min(innovsel)
                if (stash.fmx ne stash.rmdi) then mx=stash.fmx
                if (stash.fmn ne stash.rmdi) then mn=stash.fmn
                
;                stash.mx=mx
;                stash.mn=mn
                
;		levs=contour_levels([mn,mx],nlevels=30)
		levs=findgen(15)/15.*(mx-mn)+mn
                                
                print, 'typeproj: ',typeproj
		
;		wh=sort(innovsel)
;		print,'innovsel ',innovsel(wh)

		clr=fix((innovsel-mn)/(mx-mn)*(num_cols-1))+1

; prevent colours going out of range
		wh=where(clr lt 1)
                if (wh(0) gt -1) then clr(wh)=1
                wh=where(clr gt num_cols)
                if (wh(0) gt -1) then clr(wh)=num_cols

;		print,'clr ',clr(wh)

                case stash.sym of
		1: USERSYM, [-1,-1,1,1,-1],[1,-1,-1,1,1], /FILL
                2: usersym, cos(2.0*!pi*indgen(17)/16.0), sin(2.0*!pi*indgen(17)/16.0), thick=2.0, /fill
                endcase
                 
                 spawn,'echo plotpoints 3 `date`'
                 
;;		rgb=transpose(reform(color24_to_rgb(clr),nelwh,3))

		symsize=stash.symsize*stash.symscale  
;		for i=0L,nelsel-1 do $
;	        plots,lonsel(i),latsel(i),psym=8,color=clr,symsize=symsize

		if (stash.bindata eq 0) then begin
        	plots,lonsel,latsel,psym=8,color=clr,symsize=symsize,noclip=0
;       	        plots,lonsel,latsel,psym=3,color=clr

;		print, 'lonsel latsel'
;		print,lonsel, format='(4g20.10)'
;                print,latsel, format='(4g20.10)'
		endif
                
                
;                print, 'lonseld latseld'
;		print,lonsel - lonsel(0), format='(4g20.10)'
;                print,latsel - latsel(0), format='(4g20.10)'

;		wh=where(lonsel eq lonsel(0) and latsel eq latsel(0))
;                print,wh

		endelse

                 spawn,'echo plotpoints 4 `date`'
		print,"stash.symsize ",stash.symsize

		print,'limit=',[yrange(0),xrange(0),yrange(1),xrange(1)]

;		endif       ; nel lonsel > 0 
                
		if (stash.bindata) then begin
                     fillval=-32000
                     mxlat=max(latsel)
                     mnlat=min(latsel)
                     mxlon=max(lonsel)
                     mnlon=min(lonsel)
                     dlon=stash.binsize(0)
                     dlat=stash.binsize(1)
                     nlats=(mxlat-mnlat)/dlat+1
                     nlons=(mxlon-mnlon)/dlon+1
                     print,'nlons ',nlons,' nlats ',nlats
                     if (nlats ge 2 and nlons ge 2) then begin
                     innovsel2=fltarr(nlons,nlats)
;                     latsel2=fltarr(nlons,nlats)
;                     lonsel2=fltarr(nlons,nlats)
			latsel2=findgen(nlats)*dlat+mnlat
                        lonsel2=findgen(nlons)*dlon+mnlon
                     innovsel2(*)=fillval			; missing data
;                     latsel2(*)=fillval
;                     lonsel2(*)=fillval
                     nelin=n_elements(innovsel)
                     print,mnlat,mnlon
                     iiarr=(lonsel-mnlon)/dlon
                     ijarr=(latsel-mnlat)/dlat
                     print,min(ijarr),max(ijarr),min(ijarr),max(ijarr)
                     info,latsel2
                     info,lonsel2
                     for i=0L,nelin-1 do begin
                     ii=iiarr(i)
                     iip=ii+1
                     if (iip ge nlons-1) then iip=ii
                     iim=ii-1
                     if (iim lt 0) then iim=0
                     ij=ijarr(i)
                     ijp=ij+1
                     if (ijp ge nlats-1) then ijp=ij
                     ijm=ij-1
                     if (ijm lt 0) then ijm=0

                     innovsel2(ii,ij)=innovsel(i)
                     ; fill in gaps
                     if (innovsel2(iip,ij) eq fillval) then innovsel2(iip,ij)=innovsel(i)
                     if (innovsel2(ii,ijp) eq fillval) then innovsel2(ii,ijp)=innovsel(i)
                     if (innovsel2(iim,ij) eq fillval) then innovsel2(iim,ij)=innovsel(i)
                     if (innovsel2(ii,ijm) eq fillval) then innovsel2(ii,ijm)=innovsel(i)
;                     latsel2(ii,ij)=latsel(i)
;                     lonsel2(ii,ij)=lonsel(i)
                     endfor 
                     info,latsel2
                     print,latsel2
                     info,lonsel2
                     print,lonsel2
                     
                     TVLCT, R, G, B, /get
		     TVLCT,R(1),G(1),B(1),0        ; bodge fix for wrong black contour   

			wh=where(innovsel2 gt mx-(mx-mn)*0.001 and innovsel2 ne fillval)
                        if (wh(0) gt -1) then innovsel2(wh)=mx-(mx-mn)*0.001
			wh=where(innovsel2 lt mn and innovsel2 ne fillval)
                        if (wh(0) gt -1) then innovsel2(wh)=mn

		     levs2=nice_contourlevels([mn,mx],nlevels=50)

 		     contour,innovsel2,lonsel2,latsel2,levels=levs2, /overplot, /cell_fill, min_value=-1000
                     
                     TVLCT,R(0),G(0),B(0),0		; restore original colour table
                     endif
                     
		endif

		if (daymin eq daymax and daymin eq 0) then begin
                	print,'No data file'
                        xyouts, 0.15, 0.5, 'No data files', /normal, charsize=8
                endif

; plot colorbar
		levs=nice_contourlevels([mn,mx],nlevels=10)

		print, levs
                
		barclr=findgen(num_cols)+1
                colorbar_idl, [mn,mx], barclr, levs
		endif				; stash.txt eq 0

		stash.busy=0

                if (stash.pmulti eq 2) then begin
                	typeproj=stash.typeproj
                        print, 'old stash.typeproj: ',stash.typeproj	
                	if (typeproj eq 2) then stash.typeproj=3		;plot north and south poles 
                	if (typeproj eq 3) then stash.typeproj=2
                        print, 'new stash.typeproj: ',stash.typeproj
                endif

		endfor			; nplots


end

;------------------------------------------
;Profile plotting window and event handling
;------------------------------------------

PRO profilewindow_event, ev
  WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash2 
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval 
	print,uval
	if (uval eq "PRINTPW") then begin
		ps=1
		eps=0
		landscape=1
		pr2,file="~/plotprofile.ps",landscape=landscape,ps=ps,eps=eps,color=1
		plotprofile,stash2
		prend2,/view
	endif
        if (uval eq "SAVEPW") then begin
  		thisDevice = !D.Name
        	psave=!p

        	Set_Plot, 'Z'			; do graphics in the background
		Device, Set_Resolution=[640,480]    ; clear any existing stuff
        	!p.charsize=0.75

		if (stash2.white_on_black eq 0) then begin
; flip background and foreground color
                        pcolor=!p.color
                        pbackground=!p.background
                        !p.color=pbackground
                        !p.background=pcolor

        	endif
        	print,'!p.color,!p.background ',!p.color,!p.background

        	setupct, r, g, b, $
                   coltable=stash2.coltable,white_on_black=stash2.white_on_black		; setup color table
; plot data
  		plotprofile,stash2
		snapshot = TVRD()
        	WRITE_GIF,"~/plotprofile.gif",snapshot, r, g, b
        	Device, Z_Buffer=1		; reset graphics mode
        	Set_Plot, thisDevice
		!p=psave
        
        	spawn,'xv ~/plotprofile.gif'
        
        endif
        if (uval eq "TXTPW") then begin
        	plotprofile,stash2,/txt
        endif
end

PRO plotprofile,stash2, txt=txt

	if (n_elements(txt) eq 0) then txt=0

;** need to deal with multiple times

	dep2=stash2.dep2
	val2=stash2.val2
	obs2=stash2.obs2
	bkg2=stash2.bkg2
	obstype2=stash2.obstype2
        qcarr2=stash2.qcarr2
        dayarr2=stash2.dayarr2
	datestr=stash2.datestr
	xselstr=stash2.xselstr
	yselstr=stash2.yselstr
	rmdi=stash2.rmdi

        multstr=""
	mindayarr2=min(dayarr2,max=maxdayarr2)
        
        whs=sort(dayarr2)
        u=uniq(dayarr2)

        nu=n_elements(u)
        dayarr2u=dayarr2(u)

        print,'dayarr2u: ',dayarr2u
        
        if (maxdayarr2 ne mindayarr2) then multstr="mult times "+strtrim(string(nu),2)

;	print,'obstype2 ',obstype2

	print,'profile plot_bad_obs: ',stash2.plot_bad_obs
	print, obs2
        print, bkg2
        print, qcarr2

		if (stash2.plot_bad_obs eq 0) then begin
		wh=where(obs2 ne rmdi and qcarr2 eq 0)
                endif else begin
;                wh=where(obs2 eq obs2)
		wh=where(obs2 ne rmdi)
                endelse
                
		if (wh(0) gt -1) then begin
		depo1=dep2(wh)
		valo1=val2(wh)
		obso1=obs2(wh)
		bkgo1=bkg2(wh)
                qcarro1=qcarr2(wh)
		endif
                
                
		if (stash2.plot_bad_obs eq 0) then begin
 		wh=where(bkg2 ne rmdi and qcarr2 eq 0)
		endif else begin
;                wh=where(bkg2 eq bkg2)
		wh=where(bkg2 ne rmdi)
                endelse

		if (wh(0) gt -1) then begin
		depb1=dep2(wh)
		valb1=val2(wh)
		obsb1=obs2(wh)
		bkgb1=bkg2(wh)
                qcarrb1=qcarr2(wh)
		endif
                

;loop thru times

		if (n_elements(bkgb1) gt 0) then begin 
                	xmx=max([obso1,bkgb1],min=xmn)
                        ymx=max([depo1,depb1],min=ymn)
                endif else begin
                	xmx=max(obso1,min=xmn)
                        ymx=max(depo1,min=ymn)
                endelse

; set max and min depth based on selection of depths from the main window
		if (ymx gt stash2.depmax) then ymx=stash2.depmax
                if (ymn lt stash2.depmin) then ymn=stash2.depmin

; reset xrange based on new ymx and ymn
		if (n_elements(bkgb1) gt 0) then begin 
                        wh1=where(depo1 ge ymn and depo1 le ymx)
                        wh2=where(depb1 ge ymn and depb1 le ymx)
                	if (wh1(0) gt -1 and wh2(0) gt -1) then xmx=max([obso1(wh1),bkgb1(wh2)],min=xmn)
                endif else begin
                        wh1=where(depo1 ge ymn and depo1 le ymx)
                	if (wh1(0) gt -1) then xmx=max(obso1(wh1),min=xmn)
                endelse

; add a little bit to xrange to make plots look better
		if (xmn eq xmx) then begin 
                	xmn=xmn-0.5
                        xmx=xmx+0.5
                endif
		dxrange=(xmx-xmn)*0.02
                xmx=xmx+dxrange
                xmn=xmn-dxrange

; add a little bit to yrange to make plots look better
		if (ymn eq ymx) then begin 
                	ymn=ymn-0.5
                        ymx=ymx+0.5
                endif
		dyrange=(ymx-ymn)*0.01
                ymx=ymx+dyrange
                ymn=ymn-dyrange
		print,'** yrange: ',ymn, ymx

		if (txt eq 1) then begin
		        outfile='dataplot_profile.txt'
			print, 'saving data to ',outfile
			OPENW, unit, outfile, /get_lun
                endif
	
		for t=0,nu-1 do begin					; loop through times

		wht=where(dayarr2 eq dayarr2u(t))
		depo=depo1(wht)
                valo=valo1(wht)
                obso=obso1(wht)
                bkgo=bkgo1(wht)
                qcarro=qcarro1(wht)
                dayarro=dayarr2(wht)
		if (n_elements(depb1) gt 0) then depb=depb1(wht)
                if (n_elements(valb1) gt 0) then valb=valb1(wht)
                if (n_elements(obsb1) gt 0) then obsb=obsb1(wht)
                if (n_elements(bkgb1) gt 0) then bkgb=bkgb1(wht)
                if (n_elements(qcarrb1) gt 0) then qcarrb=qcarrb1(wht)
                


;print data

;		if (txt eq 1) then begin
;		print,'rmdi: ',rmdi
;                print,'profile data'
;		for i=0,n_elements(depo)-1 do begin
;                	print,depo(i),obso(i),bkgo(i),qcarro(i)
;                endfor
;                print,'profile data'
;		for i=0,n_elements(depb)-1 do begin
;                	print,depb(i),obsb(i),bkgb(i),qcarrb(i)
;                endfor
;		endif

		typestr="type: "+string(obstype2(0))
	
;get variable type
		vartype=size(obstype2,/type)
		if (vartype ne 7) then begin
			if (obstype2(0) eq rmdi) then typestr=""
		endif

; line thickness
		thick1=2
                thick2=2

;		color1='FFCC66'x
		color1=102
		color2=!p.color
                color3=234
                
                linestyle1=0
                linestyle2=0
                
		who=sort(depo)
                whb=sort(depb)

		obso_srto=obso(who)
                depo_srto=depo(who)
                qcarro_srto=qcarro(who)
                whbad=where(qcarro ne 0)
                
                if (txt eq 0) then begin
                
                if (t eq 0) then $
 		plot,obso_srto,depo_srto,ystyle=1,xstyle=1,linestyle=linestyle1,$
			xrange=[xmn,xmx],yrange=[ymx,ymn],thick=thick1, $
			psym=-4,ytitle='Level',xtitle='Value', $
			title=datestr+" "+multstr+" ("+xselstr+","+yselstr+")   "+typestr, /nodata
               
                oplot,obso_srto,depo_srto,linestyle=linestyle1,psym=-4, thick=thick1, color=color1
                if (whbad(0) gt -1) then begin
                	oplot,obso_srto(whbad),depo_srto(whbad),psym=4, thick=thick1, color=color3
                        oplot,obso_srto(whbad),depo_srto(whbad),psym=1, thick=thick1, color=color3
                endif
		if (n_elements(bkgb) gt 0) then oplot,bkgb(whb),depb(whb),psym=-5,thick=thick2, color=color2,$
                	linestyle=linestyle2
		xcoord=0.8
		ycoord=0.2
;                if (stash2.salinity eq 1) then xcoord=0.15	; left corner legend for salinity
		nel=n_elements(who)
                if (obso(who(0)) lt obso(who(nel-1))) then xcoord=0.15 
                if (t eq 0) then $

                  ycoord2=ycoord-0.05
                  xcoord2=xcoord+0.03
                  xcoord3=xcoord+0.05
                  plots, [xcoord,xcoord2],[ycoord,ycoord], psym=-4, linestyle=linestyle1, /normal, $
                  	thick=thick1, color=color1
                  xyouts, xcoord+0.05, ycoord, 'obs', /normal
                  plots, [xcoord,xcoord2],[ycoord2,ycoord2], psym=-5, linestyle=linestyle2, /normal, $
                  	thick=thick2, color=color2
                  xyouts, xcoord+0.05, ycoord2, 'bkg',/normal                
                
                endif else begin	; txt
                  
                  printf, unit,'observations ',n_elements(who)              
                  for i=0L,n_elements(who)-1 do begin              
                  	printf, unit, depo(i), obso(i), dayarro(i), format='(3f18.5)'
                  endfor
                  printf, unit, 'background ',n_elements(who) 
                  for i=0L,n_elements(whb)-1 do begin
                  	printf, unit, depb(i), bkgb(i), dayarro(i), format='(3f18.5)'
                  endfor              
                                
                endelse
                                        
                endfor  ; t
                
                if (txt eq 1) then FREE_LUN, unit
                
end

PRO profilewindow,dep2,val2,obs2,bkg2,obstype2,qcarr2,dayarr2,datestr,xselstr,yselstr,rmdi,salinity=salinity, $
		plot_bad_obs=plot_bad_obs, density=density, white_on_black=white_on_black, coltable=coltable, $
                depmax=depmax, depmin=depmin
	basepw=WIDGET_BASE(/column)
	drawpw = WIDGET_DRAW(basepw, XSIZE=640, YSIZE=480)
        buttonBase = Widget_Base(basepw, Row=1)
	buttonpw = WIDGET_BUTTON(buttonBase, VALUE='Print',uvalue="PRINTPW") 
	button2pw = WIDGET_BUTTON(buttonBase, VALUE='Save',uvalue="SAVEPW")
        button3pw = WIDGET_BUTTON(buttonBase, VALUE='Text file',uvalue="TXTPW")
	WIDGET_CONTROL, basepw, /REALIZE
	
        if (n_elements(salinity) eq 0) then salinity=0
        if (n_elements(density) eq 0) then density=0
	if (n_elements(plot_bad_obs) eq 0) then plot_bad_obs=0
        if (n_elements(white_on_black) eq 0) then white_on_black=1

		stash2 = { dep2:dep2,val2:val2,obs2:obs2, bkg2:bkg2, $
			obstype2:obstype2, qcarr2:qcarr2, dayarr2:dayarr2, $
			datestr:datestr,xselstr:xselstr, yselstr:yselstr, $
			rmdi:rmdi, salinity:salinity, plot_bad_obs:plot_bad_obs,$
                        density:density, white_on_black:white_on_black, $
                        coltable:coltable, depmax:depmax, depmin:depmin }

		plotprofile,stash2

	WIDGET_CONTROL, basepw, SET_UVALUE=stash2	

	XMANAGER, 'profilewindow', basepw, /NO_BLOCK	
end

;---------------------------------------
;Worst points, window and event handling
;---------------------------------------

PRO worstpoints,stash2

;stash2 is a combination of main stash and worstpoints specific stuff

        xrange=stash2.xrange
        yrange=stash2.yrange
        selpoints,stash2,lonsel, latsel, innovsel, qcsel, daysel, obstypsel, obnumsel, obnumsel, numsel, typestr 

	str1='Worst points in '+$
        '   lons ('+strtrim(string(xrange(0)),2)+','+strtrim(string(xrange(1)),2)+$
                	')   lats ('+strtrim(string(yrange(0)),2)+','+strtrim(string(yrange(1)),2)+')'+$
        '   depths '+strtrim(string(long(stash2.depmin)),2)+'-'+strtrim(string(long(stash2.depmax)),2)                
        print,str1

	WIDGET_CONTROL, stash2.wwlabel1, set_value=str1

        count=n_elements(innovsel)
        wh=sort(abs(innovsel))
        wh0=reverse(wh)			; reverse order starting with the largest        
        cmax=min([count,10])
	stash2.lonselw(0:cmax-1)=lonsel(wh0(0:cmax-1))
        stash2.latselw(0:cmax-1)=latsel(wh0(0:cmax-1))
        stash2.dayselw(0:cmax-1)=daysel(wh0(0:cmax-1))
        print,'lon      lat       '+typestr+'     qc         date    '
        for j1=1,cmax do begin
           j=j1-1
	    jul_to_dtstr,daysel(wh0(j)),datestr
;           datedt=jul_to_dt(daysel(wh0(j)))
;           datestr=strtrim(fix(datedt.year),2)+'/'+ $
;	     strtrim(string(fix(datedt.month),format='(i2.2)'),2)+'/'+ $
;	     strtrim(string(fix(datedt.day),format='(i2.2)'),2)+' '+ $
;	     strtrim(string(fix(datedt.hour),format='(i2.2)'),2)+':'+ $
;	     strtrim(string(fix(datedt.minute),format='(i2.2)'),2)+':'+ $
;	     strtrim(string(fix(datedt.second),format='(i2.2)'),2)
	   
           print,lonsel(wh0(j)),latsel(wh0(j)),innovsel(wh0(j)),$
                qcsel(wh0(j)),' ',datestr

        endfor
        
        if (n_elements(stash2) gt 0) then begin
        i=0
	WIDGET_CONTROL, stash2.labels2(i,0), set_value="Lon" & i=i+1
	WIDGET_CONTROL, stash2.labels2(i,0), set_value="Lat" & i=i+1
	WIDGET_CONTROL, stash2.labels2(i,0), set_value=typestr & i=i+1
	WIDGET_CONTROL, stash2.labels2(i,0), set_value="QC" & i=i+1
        WIDGET_CONTROL, stash2.labels2(i,0), set_value="Date" & i=i+1
        
        for j1=1,cmax do begin
           j=j1-1
           jul_to_dtstr,daysel(wh0(j)),datestr
;           datedt=jul_to_dt(daysel(wh0(j)))
;           datestr=strtrim(fix(datedt.year),2)+'/'+ $
;	     strtrim(string(fix(datedt.month),format='(i2.2)'),2)+'/'+ $
;	     strtrim(string(fix(datedt.day),format='(i2.2)'),2)+' '+ $
;	     strtrim(string(fix(datedt.hour),format='(i2.2)'),2)+':'+ $
;	     strtrim(string(fix(datedt.minute),format='(i2.2)'),2)+':'+ $
;	     strtrim(string(fix(datedt.second),format='(i2.2)'),2)

        i=0
	WIDGET_CONTROL, stash2.labels2(i,j1), $
        	set_value=strtrim(string(lonsel(wh0(j)),format='(f9.3)'),2) & i=i+1
	WIDGET_CONTROL, stash2.labels2(i,j1), $
        	set_value=strtrim(string(latsel(wh0(j)),format='(f9.3)'),2) & i=i+1
	WIDGET_CONTROL, stash2.labels2(i,j1), set_value=strtrim(string(innovsel(wh0(j))),2) & i=i+1
	WIDGET_CONTROL, stash2.labels2(i,j1), set_value=strtrim(string(qcsel(wh0(j))),2) & i=i+1
        WIDGET_CONTROL, stash2.labels2(i,j1), set_value=strtrim(datestr,2) & i=i+1

;           print,lonsel(wh0(j)),latsel(wh0(j)),innovsel(wh0(j)),$
;                qcsel(wh0(j)),' ',datestr

        endfor

        
        endif
	        



end

pro worstpointswindow,stash
	basepw=WIDGET_BASE(/column)
;	drawpw = WIDGET_DRAW(basepw, XSIZE=640, YSIZE=480)

	wwlabel1 = WIDGET_LABEL(basepw, XSIZE=480, VALUE="Worst points")
                
        nj=11
        ni=5
	labels=intarr(nj)
        for j=0,nj-1 do begin        
          labels(j)=Widget_Base(basepw,row=1)
        endfor

        labsiz=[50,50,50,50,125]
	labels2=intarr(ni+2,nj)
        for j=0,nj-1 do begin
         for i=0,ni-1 do begin
         labels2(i,j) = WIDGET_LABEL(labels(j), XSIZE=labsiz(i), $ 
    VALUE="a") 
 	 endfor
         if (j gt 0) then begin
         labels2(i,j) = WIDGET_BUTTON(labels(j), VALUE='Zoom to', uvalue="Zoom to "+strtrim(string(j),2))
         labels2(i+1,j) = WIDGET_BUTTON(labels(j), VALUE='Profile', uvalue="Profile "+strtrim(string(j),2))
         endif
        endfor

        stashb={ wwlabel1:wwlabel1, labels2:labels2, ni:ni, nj:nj, lonselw:dblarr(10), latselw:dblarr(10), dayselw:dblarr(10)}
	stash2=CREATE_STRUCT(stash,stashb)

	buttonpw = WIDGET_BUTTON(basepw, VALUE='Print',uvalue="PRINTPW") 
               
	WIDGET_CONTROL, basepw, /REALIZE
        worstpoints,stash2
	WIDGET_CONTROL, basepw, SET_UVALUE=stash2
	XMANAGER, 'worstpointswindow', basepw, /NO_BLOCK
end

PRO worstpointswindow_event, ev
  WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash2 
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval 
	print,uval

		xarr1=stash2.xarr
		yarr1=stash2.yarr
		dep1=stash2.dep
		dayarr=stash2.dayarr
		daymin=stash2.daymin
		daymax=stash2.daymax
                obstypes1=stash2.obstypes
                xrange=stash2.xrange
		yrange=stash2.yrange
		xs=xrange(1)-xrange(0)
		ys=yrange(1)-yrange(0)
		rmdi=stash2.rmdi
                if (stash2.salinity eq 0) then begin
		  obs1=stash2.obs
		  bkg1=stash2.bkg
                  qcarr1=stash2.qcarr
                endif else begin
		  obs1=stash2.obs2
		  bkg1=stash2.bkg2
                  qcarr1=stash2.qcarr2
		endelse                	

	if (uval eq "PRINTTSW") then begin
		ps=1
		eps=0
		landscape=1
		pr2,file="~/worstpoints.ps",landscape=landscape,ps=ps,eps=eps,color=1
		worstpoints,stash2
		prend2,/view
	endif
        print,strmid(uval,0,7)
        if (strcmp(strmid(uval,0,7),"Zoom to")) then begin
        	num=fix(strmid(uval,7,3))
                xsel=stash2.lonselw(num-1)
                ysel=stash2.latselw(num-1)
                print,num,xsel,ysel  
                dataplot_zoom,stash2, xsel, ysel, 1
                plotpoints,stash2              
                           
        endif
        if (strcmp(strmid(uval,0,7),"Profile")) then begin
        	num=fix(strmid(uval,7,3))
                xsel=stash2.lonselw(num-1)
                ysel=stash2.latselw(num-1)
                daysel=stash2.dayselw(num-1)
;                print,num,xsel,ysel,daysel,daysel-long(daysel),float(daysel)
;                info,dayarr
;                info,daysel
;		info,xarr1
;                info,ysel
;                info,yarr1
;                info,xsel
                
                wh1=where(xarr1 eq xsel and yarr1 eq ysel)
;                print,'dayarr(wh1) ',dayarr(wh1), float(dayarr(wh1))
;                print,'dayarr(wh1)-long ',dayarr(wh1)-long(dayarr(wh1))
;                print,'dep1(wh1) ',dep1(wh1)
                wh2=where(xarr1 eq xsel and yarr1 eq ysel and float(dayarr) eq float(daysel))
;                print,'wh2 ',wh2
;                if (wh2(0) gt -1) then print,'daysel(wh2) ',daysel(wh2)
       
        	wh=where(xarr1 eq xsel and yarr1 eq ysel and long(dayarr) eq long(daysel))
                if (wh(0) gt -1) then begin
		dep2=dep1(wh)
		val2=abs(obs1(wh)-bkg1(wh))
		obs2=obs1(wh)
		bkg2=bkg1(wh)
		obstype2=obstypes1(wh)
                qcarr2=qcarr1(wh)
                dayarr2=dayarr(wh)
		jul_to_dtstr,stash2.dayselw(num-1),datestr
		xselstr=string(xsel)
		yselstr=string(ysel)

		profilewindow,dep2,val2,obs2,bkg2,obstype2,qcarr2,dayarr2,datestr,xselstr,yselstr,rmdi, salinity=stash2.salinity, $
                	white_on_black=stash.white_on_black, coltable=stash.coltable, depmax=stash.depmax, depmin=stash.depmin
                endif
	endif

; store values
  WIDGET_CONTROL, ev.TOP, SET_UVALUE=stash2 

end

PRO filterwindow, stash

	val=stash.obstypeselect

	obstypes=stash.obstypes
        uniquetypes=strjoin(obstypes(uniq(obstypes, sort(obstypes))),string(10b))
        
	base = WIDGET_BASE(GROUP_LEADER=stash.base ,/modal,/column) 
        
        
	intextid = CW_FIELD(base, TITLE = "Filter type", /FRAME, value=val, uvalue='intext', /RETURN_EVENTS) 
        outtextid = WIDGET_TEXT(base, value="Choose from:"+string(10b)+uniquetypes, $
        	scr_xsize=200, scr_ysize=200, /scroll) 

	buttonBase = Widget_Base(base, Row=1)
	   cancelID = Widget_Button(buttonBase, Value='Cancel', uvalue='cancel')
	   acceptID = Widget_Button(buttonBase, Value='Accept', uvalue='accept')

	WIDGET_CONTROL, base, /REALIZE 

	ptr = Ptr_New({text:"", nocancel:0})

	stash2={ ptr:ptr, intextid:intextid }
        WIDGET_CONTROL, base, SET_UVALUE=stash2, /No_Copy

	XMANAGER, 'filterwindow', base        

        theText = (*ptr).text
        nocancel = (*ptr).nocancel
        Ptr_Free, ptr
        
        print, 'finished ',theText, nocancel

; if obstypeselect has changed then replot the points
                
        if (stash.obstypeselect ne theText and nocancel eq 1) then begin
                stash.obstypeselect=theText
        	plotpoints,stash
        endif
        
        
end

PRO filterwindow_event, ev
  WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash2 
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
  WIDGET_CONTROL, ev.ID, GET_VALUE=val
;  	info,uval
;        info,val
	print,uval
        print,val

	if (uval eq "accept" or uval eq "intext") then begin
;        stash2.val=val

	Widget_Control, stash2.intextid, Get_Value=theText
	print,'the text ',theText

        (*stash2.ptr).text=theText
        (*stash2.ptr).nocancel=1
        endif         
        
;   WIDGET_CONTROL, ev.TOP, SET_UVALUE=stash2       
        
        WIDGET_CONTROL, ev.top, /DESTROY

end

;--------------------------------------
; Input area
;--------------------------------------

PRO inputareawindow, xrange, yrange, toplevel, success

        val1=xrange(0)
        val2=xrange(1)
        val3=yrange(0)
        val4=yrange(1)
        
	base = WIDGET_BASE(GROUP_LEADER=toplevel,/modal,/column) 
                
	intextid1 = CW_FIELD(base, TITLE = "lon1", /FRAME, value=val1, uvalue='intext1', /RETURN_EVENTS) 
 	intextid2 = CW_FIELD(base, TITLE = "lon2", /FRAME, value=val2, uvalue='intext2', /RETURN_EVENTS) 
	intextid3 = CW_FIELD(base, TITLE = "lat1", /FRAME, value=val3, uvalue='intext3', /RETURN_EVENTS) 
	intextid4 = CW_FIELD(base, TITLE = "lat2", /FRAME, value=val4, uvalue='intext4', /RETURN_EVENTS) 

;       outtextid = WIDGET_TEXT(base, value="Choose from:"+string(10b)+uniquetypes, $
;        	scr_xsize=200, scr_ysize=200, /scroll) 

	buttonBase = Widget_Base(base, Row=1)
	   acceptID = Widget_Button(buttonBase, Value='Accept', uvalue='accept')
	   cancelID = Widget_Button(buttonBase, Value='Cancel', uvalue='cancel')

	WIDGET_CONTROL, base, /REALIZE 

	ptr = Ptr_New({text1:"", text2:"", text3:"", text4:"", nocancel:0})

	stash2={ ptr:ptr, intextid1:intextid1,  intextid2:intextid2, $
         	intextid3:intextid3, intextid4:intextid4}
        WIDGET_CONTROL, base, SET_UVALUE=stash2, /No_Copy

	XMANAGER, 'inputareawindow', base        

        lon1 = (*ptr).text1
        lon2 = (*ptr).text2
        lat1 = (*ptr).text3
        lat2 = (*ptr).text4
        nocancel = (*ptr).nocancel
        Ptr_Free, ptr
        
        print, 'finished ',lon1, lon2, lat1, lat2, nocancel

; if obstypeselect has changed then replot the points
                
        if (nocancel eq 1) then begin
		xrange=[float(lon1),float(lon2)]
                yrange=[float(lat1),float(lat2)]
                xrange=xrange(sort(xrange))
                yrange=yrange(sort(yrange))
                wh=where(xrange gt 360)
                if (wh(0) gt -1) then xrange(wh)=360
                wh=where(xrange lt -180)
                if (wh(0) gt -1) then xrange(wh)=-180
                wh=where(yrange gt 90)
                if (wh(0) gt -1) then yrange(wh)=90
                wh=where(yrange lt -90)
                if (wh(0) gt -1) then yrange(wh)=-90
                
                if (xrange(0) eq xrange(1)) then xrange(1)=xrange(1)+0.1 
                if (yrange(0) eq yrange(1)) then yrange(1)=yrange(1)+0.1
                
                success=1
        endif
        
        
end

PRO inputareawindow_event, ev
  WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash2 
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
  WIDGET_CONTROL, ev.ID, GET_VALUE=val
;  	info,uval
;        info,val
	print,uval
        print,val

	if (uval eq 'accept') then begin
;        stash2.val=val

	Widget_Control, stash2.intextid1, Get_Value=theText1
	Widget_Control, stash2.intextid2, Get_Value=theText2
        Widget_Control, stash2.intextid3, Get_Value=theText3
        Widget_Control, stash2.intextid4, Get_Value=theText4
	print,'the text ',theText1, theText2, theText3, theText4

        (*stash2.ptr).text1=theText1
        (*stash2.ptr).text2=theText2
        (*stash2.ptr).text3=theText3
        (*stash2.ptr).text4=theText4
        (*stash2.ptr).nocancel=1
        endif         
        
;   WIDGET_CONTROL, ev.TOP, SET_UVALUE=stash2       
        
        WIDGET_CONTROL, ev.top, /DESTROY

end

;--------------------------------------
; Input max min
;--------------------------------------

PRO inputmxmnwindow, fmx, fmn, mx, mn, rmdi, toplevel, success

	print,'fmx/mn ',fmx,fmn
        print,'mx/mn ',mx,mn

        val1=mx
        val2=mn

	base = WIDGET_BASE(GROUP_LEADER=toplevel,/modal,/column) 
                
	intextid1 = CW_FIELD(base, TITLE = "mx", /FRAME, value=val1, uvalue='intext1', /RETURN_EVENTS) 
 	intextid2 = CW_FIELD(base, TITLE = "mn", /FRAME, value=val2, uvalue='intext2', /RETURN_EVENTS) 

	text="Max and min not locked"
        if (fmx ne rmdi) then text="Max locked"
        if (fmn ne rmdi) then text="Min Locked"
        if (fmx ne rmdi and fmn ne rmdi) then text="Max and min locked"
        outtextid = WIDGET_TEXT(base, value=text, $
        	scr_xsize=200, scr_ysize=40) 

	buttonBase = Widget_Base(base, Row=1)
	   acceptID = Widget_Button(buttonBase, Value='Accept/lock', uvalue='accept')
           resetID = Widget_Button(buttonBase, Value='Reset/free', uvalue='reset')
	   cancelID = Widget_Button(buttonBase, Value='Cancel', uvalue='cancel')
           

	WIDGET_CONTROL, base, /REALIZE 

	ptr = Ptr_New({text1:"", text2:"", nocancel:0})

	stash2={ ptr:ptr, intextid1:intextid1,  intextid2:intextid2}
        WIDGET_CONTROL, base, SET_UVALUE=stash2, /No_Copy

	XMANAGER, 'inputmxmnwindow', base        

        fmx = float((*ptr).text1)
        fmn = float((*ptr).text2)

	info, fmx
        info, fmn

        nocancel = (*ptr).nocancel
        Ptr_Free, ptr
        
        print, 'finished ',fmx,fmn, mx, mn

; if obstypeselect has changed then replot the points
                
        if (nocancel eq 1) then begin                
                success=1
        endif
	if (nocancel eq 2) then begin
        	fmx=rmdi
                fmn=rmdi
                success=1
        endif
 
end

PRO inputmxmnwindow_event, ev
  WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash2 
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
  WIDGET_CONTROL, ev.ID, GET_VALUE=val
;  	info,uval
;        info,val
	print,'uval ',uval,' val ',val

	if (uval eq 'accept') then begin
;        stash2.val=val

	Widget_Control, stash2.intextid1, Get_Value=theText1
	Widget_Control, stash2.intextid2, Get_Value=theText2
	print,'the text ',theText1, theText2

        (*stash2.ptr).text1=theText1
        (*stash2.ptr).text2=theText2
        (*stash2.ptr).nocancel=1
        endif         

	if (uval eq 'reset') then begin
        (*stash2.ptr).nocancel=2
        endif
        
;   WIDGET_CONTROL, ev.TOP, SET_UVALUE=stash2       
        
        WIDGET_CONTROL, ev.top, /DESTROY

end



pro infowindow
	basepw=WIDGET_BASE(/column)
	xsz=360

        iwlabel0 = WIDGET_LABEL(basepw, XSIZE=xsz, VALUE="Dataplot")
        iwlabela = WIDGET_LABEL(basepw, XSIZE=xsz, VALUE="--------")
        iwlabelb = WIDGET_LABEL(basepw, XSIZE=xsz, VALUE="")      
	iwlabel1 = WIDGET_LABEL(basepw, XSIZE=xsz, VALUE="To report bugs or for help")
        iwlabel2 = WIDGET_LABEL(basepw, XSIZE=xsz, VALUE="contact Dan Lea")
        iwlabel3 = WIDGET_LABEL(basepw, XSIZE=xsz, VALUE="daniel.lea@metoffice.gov.uk")
 	               
	WIDGET_CONTROL, basepw, /REALIZE

;	WIDGET_CONTROL, basepw, SET_UVALUE=stash2
;	XMANAGER, 'worstpointswindow', basepw, /NO_BLOCK
end

PRO dataplot_zoom, stash, xdata, ydata, zoominout

		xrange=stash.xrange
		yrange=stash.yrange
		xrangedef=stash.xrangedef
		yrangedef=stash.yrangedef
	

		print,xrange,yrange

;		zx=xrange(0)
;		zy=yrange(0)
		sx=xrange(1)-xrange(0)
		sy=yrange(1)-yrange(0)
		sxd=xrangedef(1)-xrangedef(0)
		syd=yrangedef(1)-yrangedef(0)

		print,sx,sy

		if (zoominout eq 1) then begin
		sx=sx/2.
		sy=sy/2.
; try to squarify for zoomed in version
		sx2=(sx+sy)/2.
;		sx=max(sx,sx2)
;		sy=max(sy,sx2) 
;		sx=(sx+sx2)/2.
;		sy=(sy+sx2)/2.

; try to tend to initial proportions for zooming in
		sx=(sx+1.0*(sxd*0.5/(sxd+syd)))/2.
                sy=(sy+1.0*(syd*0.5/(sxd+syd)))/2.

		endif
		if (zoominout eq -1) then begin
		sx=sx*2.
		sy=sy*2.
 ; try to make similar to initial proportions for zooming out               
                sx2=(sx+sy)*2.
		sx=(sx+sx2*(sxd*0.5/(sxd+syd)))/2.
		sy=(sy+sx2*(syd*0.5/(sxd+syd)))/2.
                
		endif
		

		print,'sx sy ',sx,sy, xdata, ydata

                oxrange=xrange                
		
		xrange=[xdata-sx/2.,xdata+sx/2.]
		yrange=[ydata-sy/2.,ydata+sy/2.]

		if (oxrange(1) le 180) then begin		; 0 longitude centred
		if (xrange(0) lt xrangedef(0)) then xrange(0)=xrangedef(0)
		if (xrange(1) gt xrangedef(1)) then xrange(1)=xrangedef(1)
		endif else begin				; 180 longitude centred
 		if (xrange(0) lt xrangedef(0)+180) then xrange(0)=xrangedef(0)+180
		if (xrange(1) gt xrangedef(1)+180) then xrange(1)=xrangedef(1)+180
		endelse               

		if (yrange(0) lt yrangedef(0)) then yrange(0)=yrangedef(0)
		if (yrange(1) gt yrangedef(1)) then yrange(1)=yrangedef(1)

		print,'xrange ',xrange,' yrange ',yrange
                
                stash.xrange=xrange
		stash.yrange=yrange

end

;---------------------------------------
;Time series plotting and event handling
;---------------------------------------

PRO plottimeseries, stash
 
		xrange=stash.xrange
		yrange=stash.yrange
;		dayarr=stash.dayarr
		daymin=stash.daymin
		daymax=stash.daymax
		dayminl=stash.dayminl
      daymaxl=stash.daymaxl

		numtimeseries=stash.numtimeseries		; if 1 then plot number of points

;		print,depmin,xrange,yrange

;		wh=where(dep1 eq ev.index)
;		wh=where(dep1 eq depmin and $
;			dayarr ge daymin and dayarr le daymax)

;	

;		nelwh=n_elements(wh)

		; select points to plot
		typestr=''

		if (stash.plottssel eq 1) then begin
         dayminl=daymin
         daymaxl=daymax
      endif

		print,'djl ',dayminl,daymaxl, min(stash.dayarr), max(stash.dayarr)

		selpoints, stash, lonsel, latsel, innovsel, qcsel, daysel, obstypsel, obnumsel, numsel, typestr, $
                	daymin=dayminl, daymax=daymaxl, /rmsmean, innovsel2=innovsel2

		print,'djl ',min(daysel), max(daysel)

		if (stash.plottssel eq 1) then begin
                	daymaxl=daymaxl+1
                endif
		   

;		plot,daysel, ystyle=1

		print,'n_elements(daysel) ',n_elements(daysel), min(daysel), max(daysel) 
		print, dayminl, daymaxl

; group in 1/4 day bins

;		binspday=double(4.0)    ; every 6 hours
;                binspday=double(8.0)    ; every 3 hours
		binspday=double(stash.binspday)
		nbins=(daymaxl-dayminl)*binspday+1

		print, daymaxl, dayminl, binspday

                x2arr=dblarr(nbins)
                xarr=dblarr(nbins)
                narr=dblarr(nbins)
                tarr=dblarr(nbins)
		meants=dblarr(nbins)
                rmsts=dblarr(nbins)
  
		print, 'nbins: ',nbins

		if n_elements(innovsel) gt 0 then begin

		print,max(daysel),min(daysel)
  
                for ibin=0L,nbins-1 do begin
                  timmn=double(dayminl)+ibin/binspday
                  timmx=timmn+1/binspday
;                  print, ibin, timmn, timmx, timmx-timmn
                  wh=where(daysel ge timmn and daysel lt timmx, count)
                  print, 'ibin ',ibin, timmn, timmx, count
; redo based on the number of observations
                  if (wh(0) ne -1) then begin
                  innovsels=innovsel(wh)
                  innovsel2s=innovsel2(wh)
                  numsels=numsel(wh)
                  
		  x2arr(ibin)=total(innovsel2s*numsels)
		  xarr(ibin)=total(innovsels*numsels)
;		  narr(ibin)=n_elements(innovsels)
		  narr(ibin)=total(numsels)
;                  print,ibin, x2arr(ibin), xarr(ibin)
                  endif
                  tarr(ibin)=timmn
                endfor

		meants=xarr/narr
                rmsts=sqrt(x2arr/narr)

;		print,'meants: ',meants
;		print,'rmsts: ',rmsts

		endif	; n_elements(innovsel)

		wh=where(finite(meants))		; find finite values
		ymx=max([meants(wh),rmsts(wh)],min=ymn)

;put a bit of white space around ymx,ymn
		dymxmn=(ymx-ymn)*0.02
                ymx=ymx+dymxmn
                ymn=ymn-dymxmn

		print,'ymx/mn ',ymx,ymn

                strtsal=''
;                print,'stash.filetype ',stash.filetype
                if (stash.filetype eq 'Prof' or stash.filetype eq 'feedback') then begin
                	strtsal='T:  '
                        if (stash.salinity eq 1) then strtsal='S:  '
                endif else begin
                	strtsal=stash.filetype+':  '
                endelse

                title=strtsal+typestr+'   lons ('+string(xrange(0))+','+string(xrange(1))+$
                	')   lats ('+string(yrange(0))+','+string(yrange(1))+')'  
                print,title            

;                dtarr=jul_to_dt(tarr)   
		dtarr=tarr-0.5			; convert back to julian day

		xmx=max(dtarr,min=xmn)
;put a bit of white space around xmx,xmn
		dxmxmn=(xmx-xmn)*0.02
                xmx=xmx+dxmxmn
                xmn=xmn-dxmxmn


;date_label = LABEL_DATE(DATE_FORMAT = $ 
;   ['%I:%S', '%H', '%D %M, %Y']) 

;date_label = LABEL_DATE(DATE_FORMAT = $ 
;   ['%D %M, %Y']) 

date_label = LABEL_DATE(DATE_FORMAT=['%D-%M','%Y'])



	if (stash.txt eq 0) then begin
	if (numtimeseries ne 1) then begin                       
		plot,dtarr, meants, xstyle=1, linestyle=1, yrange=[ymn,ymx], title=title, $
                       ytitle=typestr, xrange=[xmn,xmx], $
                       XTICKUNITS=['Time', 'Time'], XTICKFORMAT = ['LABEL_DATE'],YMARGIN=[6,4] 
;   XTICKFORMAT = ['LABEL_DATE'], $ 
;   XTICKUNITS = ['Day'], $ 
;   XTICKINTERVAL = 4 

		plot, dtarr, rmsts, xstyle=1, yrange=[ymn,ymx], /noerase, $
                       ytitle=typestr, xrange=[xmn,xmx], $
                       XTICKUNITS=['Time', 'Time'], XTICKFORMAT = ['LABEL_DATE'],YMARGIN=[6,4]
;   XTICKFORMAT = ['LABEL_DATE'], $ 
;   XTICKUNITS = ['Day'], $ 
;   XTICKINTERVAL = 4 


		xcoord=0.8
		ycoord=0.9
		ycoord=0.35
                ycoord=0.2
;		ukmo_legend,xcoord=xcoord,ycoord=ycoord,delta_y=0.05,$
;			['RMS','mean'],linestyle=[0,1],/normal

                  ycoord2=ycoord-0.05
                  xcoord2=xcoord+0.03
                  xcoord3=xcoord+0.05
                  plots, [xcoord,xcoord2],[ycoord,ycoord], linestyle=0, /normal
                  xyouts, xcoord3, ycoord, 'RMS', /normal
                  plots, [xcoord,xcoord2],[ycoord2,ycoord2], linestyle=1, /normal
                  xyouts, xcoord3, ycoord2, 'mean',/normal                

	endif else begin
		plot,dtarr, narr, xstyle=1, title=title, $
                       ytitle='number of obs', $ 
                       XTICKUNITS=['Time', 'Time'], XTICKFORMAT = ['LABEL_DATE'],YMARGIN=[6,4]
;   XTICKFORMAT = ['LABEL_DATE'], $ 
;   XTICKUNITS = ['Day'], $ 
;   XTICKINTERVAL = 4 
      
	endelse
	endif else begin

        outfile=stash.outfile+'_timeseries.txt'
	print, 'saving data to ',outfile
	OPENW, unit, outfile, /get_lun
;   		printf,unit,'Output timeseries data: '
		printf,unit, strtsal
                printf,unit, typestr
                printf,unit, xrange, yrange
                printf,unit, binspday
                nel=n_elements(dtarr)
                for i=0L,nel-1 do begin
                	printf,unit, dtarr(i), narr(i), meants(i), rmsts(i),format='(d18.8,d18.2,d18.8,d18.8)'
                endfor
        FREE_LUN, unit

	endelse

;		print,'min/max lonsel: ',min(lonsel),max(lonsel)

end

PRO timeserieswindow, stash
	basepw=WIDGET_BASE(/column)
	drawpw = WIDGET_DRAW(basepw, XSIZE=640, YSIZE=480)
        buttons=Widget_Base(basepw,row=1)
        
        tlb = Widget_Base(buttons,Title='Push-Buttons', row=1, Scr_XSize=400,$
       /Exclusive)
; no_release only sends select events (not release ones)
   	buttonp1 = Widget_Button(tlb, Value='1 bin per day',uvalue='RADIO1',/no_release)
   	buttonp2 = Widget_Button(tlb, Value='2 bins',uvalue='RADIO2',/no_release)
   	buttonp3=  Widget_Button(tlb, Value='4 bins',uvalue='RADIO3',/no_release)
   	buttonp4 = Widget_Button(tlb, Value='8 bins',uvalue='RADIO4',/no_release)

	if (stash.binspday eq 1) then Widget_Control, buttonp1, Set_Button=1
	if (stash.binspday eq 2) then Widget_Control, buttonp2, Set_Button=1
       	if (stash.binspday eq 4) then Widget_Control, buttonp3, Set_Button=1
	if (stash.binspday eq 8) then Widget_Control, buttonp4, Set_Button=1
 
        tlb2 = Widget_Base(buttons,Title='Push-Buttons', row=1, Scr_XSize=100,$
       /NonExclusive)
	buttonpa1= Widget_Button(tlb2, Value='plot selected period',uvalue='PLOTSEL')


	Widget_Control, buttonpa1, Set_Button=stash.plottssel
        
	buttonpw = WIDGET_BUTTON(buttons, VALUE='Print',uvalue='PRINTTSW') 
        buttonpw2 = WIDGET_BUTTON(buttons, VALUE='Output',uvalue='OUTPUTTSW')
        
	WIDGET_CONTROL, basepw, /REALIZE

	xyouts,0.2,0.5,'working...',/normal,charsize=2.5
	
        plottimeseries,stash

	WIDGET_CONTROL, basepw, SET_UVALUE=stash	

	XMANAGER, 'timeserieswindow', basepw, /NO_BLOCK	
end

PRO timeserieswindow_event, ev
  WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash 
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval 
	print,uval
	if (uval eq 'PRINTTSW') then begin
		ps=1
		eps=0
		landscape=1
		pr2,file='~/timeseries.ps',landscape=landscape,ps=ps,eps=eps,color=1
		plottimeseries,stash
		prend2,/view
	endif
        if (uval eq 'OUTPUTTSW') then begin
        	savetxt=stash.txt
                stash.txt=1
        	plottimeseries,stash
                stash.txt=savetxt
        endif
        if (uval eq 'RADIO1') then begin
        	stash.binspday=1
                plottimeseries,stash        	
        endif
        if (uval eq 'RADIO2') then begin
        	stash.binspday=2
                plottimeseries,stash        	
        endif
        if (uval eq 'RADIO3') then begin
        	stash.binspday=4
                plottimeseries,stash        	
        endif
        if (uval eq 'RADIO4') then begin
        	stash.binspday=8
                plottimeseries,stash        	
        endif
        if (uval eq 'PLOTSEL') then begin
        	stash.plottssel=1-stash.plottssel
                print,'stash.plottssel ',stash.plottssel
                WIDGET_CONTROL, ev.TOP, SET_UVALUE=stash
                plottimeseries,stash        
        endif

        
end

;----------------------------------------
; T-S diagram plotting and event handling
;----------------------------------------

PRO plottsdiagram, stash
 
		xrange=stash.xrange
		yrange=stash.yrange
;		dayarr=stash.dayarr
		daymin=stash.daymin
		daymax=stash.daymax
		dayminl=stash.dayminl
                daymaxl=stash.daymaxl
                xarr=stash.xarr
                yarr=stash.yarr
                rmdi=stash.rmdi

		; select T and S points to plot
		typestr=''
      		selpoints, stash, lonsel, latsel, innovsel, qcsel, daysel, obstypsel, obnumsel, numsel, typestr, $
                	daymin=dayminl, daymax=daymaxl, salinity=1
                        
                ; select points with salinity
                
                nel=n_elements(lonsel)
                
                if (nel gt 0) then begin

		  bkg=stash.bkg
                  bkg2=stash.bkg2
                  obs=stash.obs
                  obs2=stash.obs2
                  dep=stash.dep
                  strtarr=0
 		  depmin=stash.depmin
		  depmax=stash.depmax
                 
		  for i=0L,nel-1 do begin
			wh=where(lonsel(i) eq xarr and latsel(i) eq yarr and $
                          dep ge depmin and dep le depmax )
                        if (wh(0) gt -1) then begin
                        if (strtarr eq 0) then begin
                          strtarr=1
                          bkgsel=bkg(wh)
                          bkg2sel=bkg2(wh)
                          obssel=obs(wh)
                          obs2sel=obs2(wh)
                        endif else begin
		          bkgsel=[bkgsel,bkg(wh)]
                          bkg2sel=[bkg2sel,bkg2(wh)]
		          obssel=[obssel,obs(wh)]
                          obs2sel=[obs2sel,obs2(wh)]
                        endelse
                        endif              
                  endfor

               ;filter out points with missing data
               
               wh=where(bkgsel ne rmdi and bkg2sel ne rmdi $
                    and obssel ne rmdi and obs2sel ne rmdi)
               
                if (wh(0) gt -1) then begin
                  obssel=obssel(wh)
                  bkgsel=bkgsel(wh)
                  obs2sel=obs2sel(wh)
                  bkg2sel=bkg2sel(wh)              
                  
                  xmn=min([obssel,bkgsel],max=xmx)
                  ymn=min([obs2sel,bkg2sel],max=ymx)
               
               title='T-S diagram: dep '+string(depmin)+'-'+string(depmax)
               
		plot,obssel,obs2sel,psym=4, color='FFFFFF'x, xtitle='Temperature',ytitle='Salinity',$
                	xrange=[xmn,xmx], yrange=[ymn,ymx],xstyle=1,ystyle=1, title=title
                oplot,bkgsel,bkg2sel,psym=5, color='FFCC66'x

		xcoord=0.8
		ycoord=0.9
		ycoord=0.2
;		ukmo_legend,xcoord=xcoord,ycoord=ycoord,delta_y=0.05,$
;			['obs','bkg'],psym=[4,5],color=['FFFFFF'x, 'FFCC66'x],/normal

                  ycoord2=ycoord-0.05
                  xcoord2=xcoord+0.03
                  xcoord3=xcoord+0.05
                  plots, [xcoord,xcoord2],[ycoord,ycoord], psym=4, color='FFFFFF'x, /normal
                  xyouts, xcoord+0.05, ycoord, 'obs', /normal
                  plots, [xcoord,xcoord2],[ycoord2,ycoord2], psym=5, color='FFCC66'x, /normal
                  xyouts, xcoord+0.05, ycoord2, 'bkg',/normal                


               endif
               endif

end

PRO tsdiagramwindow, stash
	basepw=WIDGET_BASE(/column)
	drawpw = WIDGET_DRAW(basepw, XSIZE=640, YSIZE=480)
	buttonpw = WIDGET_BUTTON(basepw, VALUE='Print',uvalue='PRINTTSW') 
	WIDGET_CONTROL, basepw, /REALIZE
	
        plottsdiagram,stash

	WIDGET_CONTROL, basepw, SET_UVALUE=stash	

	XMANAGER, 'tsdiagramwindow', basepw, /NO_BLOCK	
end

PRO tsdiagramwindow_event, ev
  WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash 
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval 
	print,uval
	if (uval eq 'PRINTTSW') then begin
		ps=1
		eps=0
		landscape=1
		pr2,file='~/tsdiagram.ps',landscape=landscape,ps=ps,eps=eps,color=1
		plottsdiagram,stash
		prend2,/view
	endif
end

PRO jul_to_dtstr, daysel, datestr, notime=notime

;print,'called jul_to_dtstr ',daysel

; IDL Julian days start at 12 noon

	CALDAT, daysel(0)-0.5, Month, Day, Year, Hour, Minute, Second
           datestr=strtrim(fix(year),2)+'/'+ $
	     strtrim(string(fix(month),format='(i2.2)'),2)+'/'+ $
	     strtrim(string(fix(day),format='(i2.2)'),2)
             
             if (n_elements(notime) eq 0) then $
             	datestr=datestr+' '+$
	     strtrim(string(fix(hour),format='(i2.2)'),2)+':'+ $
	     strtrim(string(fix(minute),format='(i2.2)'),2)+':'+ $
	     strtrim(string(fix(second),format='(i2.2)'),2)

;           datedt=jul_to_dt(daysel)
;           datedt=datedt(0)
;           datestr=strtrim(fix(datedt.year),2)+'/'+ $
;	     strtrim(string(fix(datedt.month),format='(i2.2)'),2)+'/'+ $
;	     strtrim(string(fix(datedt.day),format='(i2.2)'),2)+' '+ $
;	     strtrim(string(fix(datedt.hour),format='(i2.2)'),2)+':'+ $
;	     strtrim(string(fix(datedt.minute),format='(i2.2)'),2)+':'+ $
;	     strtrim(string(fix(datedt.second),format='(i2.2)'),2)

;print,datestr

END

;-----------------------------------------------------------------------
; MAIN routine
; Widget creation routine. 
;  dataplot, [longitude, latitude, deparr, dayarr, valarr, bkgarr]
;+
PRO dataplot, indata, rmdi=rmdi, filename=filename, salinity=salinity, batch=batch, $
	ps=ps, gif=gif, area=area, typeplot=typeplot, ombtypeplot=ombtypeplot, $
        alldays=alldays, depths=depths, $
        mx=mx, mn=mn, showmdt=showmdt, obstypeselect=obstypeselect, printobstypes=printobstypes, $
        daterange=daterange, timeseries=timeseries, binspday=binspday, plot_bad_obs=plot_bad_obs, $
        plot_only_bad_obs=plot_only_bad_obs, $
        numtimeseries=numtimeseries, txt=txt, netcdf=netcdf, vertgrad=vertgrad, healthcheck=healthcheck, $
        duplicates=duplicates, differences=differences, outfile=outfile, white_on_black=white_on_black, $
        bindata=bindata, symscale=symscale, hiresmap=hiresmap, coltable=coltable, $
        picsize=picsize, pmulti=pmulti, typeproj=typeproj, dayrange=dayrange, $
        notfussy=notfussy, mld=mld, binsize=binsize, filterout=filterout
;-
	
; DJL switch off wave compatibility mode
res=execute('waveoff')

; string array obstypeselect

if (n_elements(obstypeselect) eq 0) then obstypeselect=''
if (n_elements(printobstypes) eq 0) then printobstypes=''
if (n_elements(filterout) eq 0) then filterout=''

if (n_elements(binspday) eq 0) then binspday=4		; plot timeseries every 6 hours
plottssel=0	; plot selected period of timeseries
if (n_elements(plot_bad_obs) eq 0) then plot_bad_obs=0	; don't plot bad obs

if (n_elements(plot_only_bad_obs) eq 0) then plot_only_bad_obs=1	; plot only bad obs if selected
if (n_elements(white_on_black) eq 0) then white_on_black=1

if (n_elements(duplicates) eq 0) then duplicates=0	; plot only duplicates if selected
if (n_elements(differences) eq 0) then differences=0	; plot only differences if selected

if (n_elements(numtimeseries) eq 0) then numtimeseries=0	; plot time series of O-B

if (n_elements(txt) eq 0) then txt=0

if (n_elements(netcdf) eq 0) then netcdf=0			; if 1 the output netcdf data

print,obstypeselect

if (n_elements(salinity) eq 0) then salinity=0
density=0
if (n_elements(vertgrad) eq 0) then vertgrad=0

if (n_elements(mld) eq 0) then mld=0

if (n_elements(outfile) eq 0) then outfile="dataplot"

if (n_elements(bindata) eq 0) then bindata=0

if (n_elements(binsize) eq 0) then binsize=[1.0,1.0]

if (n_elements(symscale) eq 0) then symscale=1.0

if (n_elements(hiresmap) eq 0) then begin
	hires_map=0
endif else begin
	hires_map=hiresmap
endelse

if (n_elements(rmdi) eq 0) then rmdi=0

if (n_elements(coltable) eq 0) then begin
	coltable=-1
endif

if (n_elements(picsize) ne 2) then picsize=[800,512]	; default gif resolution

if (n_elements(pmulti) eq 0) then pmulti=0		; default pmulti

sz=size(indata)
nsz=n_elements(sz)
type=sz(nsz-2)	; get type 2 integer, 4 float, 7 string
if (type eq 7) then filename=indata

if (type ne 7) then begin
   nel=n_elements(indata)
   nel8=nel/8
   numobs=nel8

   ; get input values
   if (nel gt 0) then begin
      indata=reform(indata,numobs,8)
      xarr=indata(*,0)
      yarr=indata(*,1)
      dep=indata(*,2)
      dayarrin=indata(*,3)
      obs=indata(*,4)
      bkg=indata(*,5)
      obs2=rmdi
      obs3=rmdi
      bkg2=rmdi
      qcarr=long(indata(*,6))
      qcarr2=rmdi
      obnum=rmdi
      obstypes=long(indata(*,7))
      filetype="generic"
      
      print, 'djl max/min dayarrin ',max(dayarrin), min(dayarrin)
      print, 'djl max/min qcarr ',max(qcarr), min(qcarr)
      
   endif
endif else begin
   if (n_elements(filename) eq 0) then begin
       print,'ERROR: No filename supplied'
       print,'call: dataplot, filenamearr'
       return
   endif
endelse

;read in data
if (n_elements(filename) gt 0) then begin
	read_cdfobs, filename, numobs=numobs, Latitudes=yarr, Longitudes=xarr, $
	      obs=obs, modelvals=bkg, qcs=qcarr, $
         ob2=obs2, modelval2=bkg2, qc2=qcarr2, $
         ob3=obs3, $
         Dates=dayarrin, rmdi=rmdi, $
	      depths=dep, nodates=0, types=obstypes, $
         filetype=filetype, iobs=iobs, jobs=jobs, MDT=MDT, error=error, profilenum=obnum, $
         notfussy=notfussy, VarName=varname

	if (error eq 1) then begin
           numobs=0
           dayarrin=0
           rmdi=-99999
           xarr=rmdi
           yarr=rmdi
           obs=rmdi
           bkg=rmdi
           qcarr=1
           dayarrin=rmdi
           dayarr=rmdi
           dep=rmdi
           filetype=""
           obstypes=rmdi
           obnum=0
	endif 

	print,'error ',error, numobs

	if (numobs gt 0) then begin
        print, 'numobs ',numobs

;setup obnum if it is not produced by read_cdfobs

	if (n_elements(obnum) ne numobs and numobs gt 0) then begin
		obnum=lindgen(n_elements(numobs))	; generate an array of observation numbers
                					; starting with zero
        endif



;if gt 180 then make longitudes negative

	  wh=where(xarr ge 180 and xarr le 360)
	  if (wh(0) gt -1) then begin
 	    xarr(wh)=xarr(wh)-360
	  endif


;calculate vertical gradient
;
;	  if (filetype eq "Prof") then $
;	  calcvertgrad, xarr, yarr, dayarrin, obs, bkg, qcarr, obs2,bkg2, qcarr2, dep, rmdi

; adjust qc values for profile obs data (1 = good)
	  wh=where(bkg ne rmdi)
          if (n_elements(bkg2) gt 0) then begin
            wh2=where(bkg2 ne rmdi)
	    if (filetype eq "Prof" and wh(0) eq -1 and wh2(0) eq -1) then begin
              qcarr=qcarr-1 
              qcarr2=qcarr2-1
	    endif
	  endif
 
          if (keyword_set(showmdt) eq 1) then begin
	    bkg=MDT
          endif


	  print,'rmdi ',rmdi

	  print,'file read in'

spawn,'echo part 1 `date`'

	  if (n_elements(rmdi) eq 0) then rmdi=-1.07374e+09

	  qcarr=fix(qcarr)

	endif		; numobs

	print,'endif numobs'

; select area

        sz=size(area)
        nsz=n_elements(sz)
        type=sz(nsz-2)	; get type 2 integer, 4 float, 7 string
        if (type eq 7) then areasel, area, xrange, yrange, success
        if (type eq 4 or type eq 2) then begin
	  if (nsz eq 4) then begin
             xrange=[area(0),area(2)]
             yrange=[area(1),area(3)]
          endif
	endif			

; set missing obs types to missing data
        if (n_elements(obs2) eq 0) then begin
          obs2=rmdi
          bkg2=rmdi
          qcarr2=rmdi
       endif
        if (n_elements(obs3) eq 0) then begin
          obs3=rmdi
          bkg3=rmdi
          qcarr3=rmdi
       endif

endif		; filename

if (n_elements(varname) eq 0) then varname=''

	  if (n_elements(dayarrin) ne numobs) then dayarrin=replicate(1,numobs)
;	    dep=replicate(0,numobs)


	  if (size(dayarrin,/type) eq 8) then begin 
  	    dayarr=dayarrin.julian
          endif else begin
  	    dayarr=dayarrin
            dayarr=dayarr+0.5			; dataplot prefers each day to be a different integer
          endelse

print,'xxx'

;set contour range mn, mx
if (n_elements(mx) eq 0) then fmx=rmdi else fmx=mx
if (n_elements(mn) eq 0) then fmn=rmdi else fmn=mn
mx=fmx
mn=fmn



  ; Define a monochrome image array for use in the application. 
;  READ_PNG, FILEPATH('mineral.png', $ 
;    SUBDIR=['examples', 'data']), image 
 
  ; Place the image array in a pointer heap variable, so we can 
  ; pass the pointer to the event routine rather than passing the 
  ; entire image array. 
;  imagePtr=PTR_NEW(image, /NO_COPY) 
 
  ; Retrieve the size information from the image array. 
;  im_size=SIZE(*imagePtr) 

;---------------
;Setup defaults
;---------------

  busy=0
  wh=where(dep ge 0)
  depminl=0.
  depmaxl=1.
  if( wh(0) gt -1) then depminl=min(dep(wh),max=depmaxl)
  depscl=fix((depmaxl-depminl)/100.)
  if (depscl lt 1) then depscl=1
  depmin=depminl
;  depmax=depmin+depscl  
  depmax=depmaxl
  pmultinum=0

if (n_elements(depths) eq 2) then begin

 depmin=max([depminl,depths(0)])
 depmax=min([depmaxl,depths(1)])
endif

	print,'n_elements(typeplot) ',n_elements(typeplot)

   if (n_elements(typeplot) eq 0) then typeplot=1
;   if (n_elements(where(bkg ne bkg(0))) gt 1) then begin
;   	typeplot=3
;   endif else begin
;   	typeplot=4
;   endelse
;   endif
   if (typeplot gt 4) then typeplot=4
   if (typeplot lt 1) then typeplot=1
   
   if (n_elements(ombtypeplot) eq 0) then begin
    if (n_elements(where(bkg ne bkg(0))) gt 1) then begin
   	ombtypeplot=1
    endif else begin
   	ombtypeplot=2
    endelse
   endif
   if (ombtypeplot gt 3) then ombtypeplot=3
   if (ombtypeplot lt 1) then ombtypeplot=1

   	print,'typeplot ',typeplot, ' ombtypeplot ',ombtypeplot
   
    if (n_elements(typeproj) eq 0) then typeproj=1

	xrangedef=[-110.,40.]
	yrangedef=[-50.,80.]
	wh=where(xarr lt xrangedef(0) or xarr gt xrangedef(1) or $
		yarr lt yrangedef(0) or yarr gt yrangedef(1),count)
	if (count gt 0) then begin
		xrangedef=[-180.,180.]
		yrangedef=[-90.,90.]
	endif

	if (n_elements(xrange) eq 0) then begin
 	xrange=xrangedef
	yrange=yrangedef
        endif
	symsize=1.0

;  map_file=""

;  plot_bad_obs=0

  dayshi=-10.d/86400.		; small shift to group 0:00 hours with the previous day
;  dayshi=-0.1d

  wh=where(dayarr gt 0 and dayarr lt 9000000)
;  print,dayarr
;  print,wh(0)
  if (wh(0) gt -1) then begin
    daymin=min(dayarr(wh)+dayshi,max=daymax)
  endif else begin
    daymax=0
    daymin=0
  endelse
;  daymax=maxday
;  daymin=minday
  dayminl=daymin
  daymaxl=daymax
  

  print,'** dayminl daymaxl ', dayminl, daymaxl

; ** Health check file   
	if (keyword_set(healthcheck)) then begin
		OPENW, unit, outfile+'_health.txt', /get_lun
   		printf,unit,' Health check data '
                printf,unit,'-------------------'
                printf,unit,' Number of files ',n_elements(filename)
                printf,unit,' List of files '
		printf,unit,filename
                printf,unit,' Filetype ',filetype
                printf,unit,' Number of observations ',numobs
                printf,unit,' Date range in julian days ',dayminl, daymaxl,format='(a,2f20.8)'
		jul_to_dtstr,dayminl,datestr1
                jul_to_dtstr,daymaxl,datestr2
		printf,unit,' Date range ',datestr1,' - ',datestr2
                mxo=0 & mno=0 
                mxb=0 & mnb=0 
                mxob=0 & mnob=0
                rmsomb=0 & meanomb=0
;                info,obs
;                print,obs
;                info,bkg
;                print,bkg 
;                print,'rmdi: ',rmdi
;                wh=where(obs ne rmdi and qcarr lt 1,counto)
; check for missing data within a tolerance 
		wh=where(abs((obs-rmdi)/rmdi) gt 0.01 and qcarr lt 1, counto)
;                print,'min ',min(abs((bkg(wh)-rmdi)/rmdi))
;                print,'min ',min(bkg(wh)), min(obs(wh))
;                print,'min ',max(bkg(wh)), max(obs(wh))
                if (counto gt 0) then mxo=max(obs(wh),min=mno)
;               wh=where(bkg ne rmdi and qcarr lt 1,countb)
		wh=where(abs((bkg-rmdi)/rmdi) gt 0.01 and qcarr lt 1, countb)
                if (countb gt 0) then mxb=max(bkg(wh),min=mnb)
;                wh=where(obs ne rmdi and bkg ne rmdi and qcarr lt 1,countob)
		wh=where(abs((obs-rmdi)/rmdi) gt 0.01 and $
                	abs((bkg-rmdi)/rmdi) gt 0.01 and qcarr lt 1, countob)
                if (countob gt 0) then mxob=max(obs(wh)-bkg(wh),min=mnob)
; calculate rms / mean
		if (countob gt 0) then begin
		   x=total(obs(wh)-bkg(wh))
                   x2=total((obs(wh)-bkg(wh))^2)
                   nel=n_elements(wh)
                   meanomb=x/nel
                   rmsomb=sqrt(x2/nel)
                endif
 
                printf,unit,' Max/min obs ',mxo,mno
                printf,unit,' Max/min bkg ',mxb,mnb
                printf,unit,' Max/min obs-bkg ',mxob,mnob
                printf,unit,' RMS/mean obs-bkg ',rmsomb,meanomb
                printf,unit,' Number of good observations ',counto
                printf,unit,' Number of bad observations ', numobs-counto

                mxo=0 & mno=0 
                mxb=0 & mnb=0 
                mxob=0 & mnob=0 
                rmsomb=0 & meanomb=0 
                wh=where(obs2 ne rmdi and qcarr2 lt 1,counto) 
                if (counto gt 0) then mxo=max(obs2(wh),min=mno)
                wh=where(bkg2 ne rmdi and qcarr2 lt 1,countb)
                if (countb gt 0) then mxb=max(bkg2(wh),min=mnb)
                wh=where(obs2 ne rmdi and bkg2 ne rmdi and qcarr2 lt 1,countob)
                if (countob gt 0) then mxob=max(obs2(wh)-bkg2(wh),min=mnob)
; calculate rms / mean
		if (countob gt 0) then begin
		   x=total(obs2(wh)-bkg2(wh))
                   x2=total((obs2(wh)-bkg2(wh))^2)
                   nel=n_elements(wh)
                   meanomb=x/nel
                   rmsomb=sqrt(x2/nel)
                endif
                
                printf,unit,' Max/min obs2 ',mxo,mno
                printf,unit,' Max/min bkg2 ',mxb,mnb
                printf,unit,' Max/min obs2-bkg2 ',mxob,mnob
                printf,unit,' RMS/mean obs2-bkg2 ',rmsomb,meanomb                
                printf,unit,' Number of good observations2 ',counto
                printf,unit,' Number of bad observations2 ', numobs-counto

	        FREE_LUN,unit
        endif

        daymax=daymaxl			; default to show everything
        if (keyword_set(alldays)) then daymax=daymaxl         ;print alldays
        if (n_elements(daterange) eq 2) then begin
        	daymin=daterange(0)
                if (daterange(0) lt dayminl) then daymin=dayminl
                if (daterange(0) gt daymaxl) then daymin=daymaxl
                if (daterange(1) gt dayminl and daterange(1) le daymaxl) then begin
                	daymax=daterange(1)
                endif else begin
                daymax=daymaxl                
                endelse
        endif
        if (n_elements(daterange) eq 1) then begin		; select number of days before end or after beginning
             	print,'daymin b4: ',daymin, daymax
                daymax=daymaxl
                daymin=dayminl
                if (daterange(0) lt 0) then begin
                	if (daterange(0) eq -9999) then begin
                        	daymin=daymax
                        endif else begin
	                	daymin=daymax+daterange(0)
                        endelse
                endif
                if (daterange(0) gt 0) then daymax=daymin+daterange(0)
        endif                                 

; select day from the start day
	if (n_elements(dayrange) eq 1) then begin
        	daymin=dayminl+dayrange
                daymax=daymin
        endif

	if (n_elements(dayrange) eq 2) then begin
        	daymin=dayminl+dayrange(0)
                daymax=dayminl+dayrange(1)
        endif


; prevent error where only one day plotted  
	daymaxsl=daymaxl
        dayminsl=dayminl
	if (daymaxsl le dayminsl+1) then daymaxsl=dayminsl+1
; stop day going out of range
	if (daymax gt daymaxl) then daymax=daymaxl
        if (daymax lt dayminl) then daymax=dayminl
        if (daymin lt dayminl) then daymin=dayminl
        if (daymin gt daymaxl) then daymin=daymaxl

	print,'daymin af: ',daymin, daymax

  sym=1
   
;-------------
;Setup window
;-------------

if (not keyword_set(batch)) then begin 
  ; Create a base widget to hold the application. 
;	base0=WIDGET_BASE()

  base = WIDGET_BASE(/COLUMN,mbar=bar) 
;   base = WIDGET_BASE(/row)

 
  ; setup menu bar
  
  menu1 = WIDGET_BUTTON(bar, VALUE='Areas', /MENU) 
button1 = WIDGET_BUTTON(menu1, VALUE='Global', uvalue='Global') 
button2 = WIDGET_BUTTON(menu1, VALUE='Arctic', uvalue='Arctic')
button2a = WIDGET_BUTTON(menu1, VALUE='Atlantic', uvalue='Atlantic') 
button3 = WIDGET_BUTTON(menu1, VALUE='N Atl', uvalue='N Atl')
button4 = WIDGET_BUTTON(menu1, VALUE='Trop Atl', uvalue='Trop Atl')
button5 = WIDGET_BUTTON(menu1, VALUE='S Atl', uvalue='S Atl')
button5a = WIDGET_BUTTON(menu1, VALUE='Pacific', uvalue='Pacific')
button6 = WIDGET_BUTTON(menu1, VALUE='N Pac', uvalue='N Pac')
button7 = WIDGET_BUTTON(menu1, VALUE='Trop Pac', uvalue='Trop Pac')
button8 = WIDGET_BUTTON(menu1, VALUE='S Pac', uvalue='S Pac')
button9 = WIDGET_BUTTON(menu1, VALUE='Indian', uvalue='Indian')
button10 = WIDGET_BUTTON(menu1, VALUE='S Ocean', uvalue='S Ocean')
button11 = WIDGET_BUTTON(menu1, VALUE='Med', uvalue='Med')
button12 = WIDGET_BUTTON(menu1, VALUE='N West Shelf', uvalue='NWS')
button13 = WIDGET_BUTTON(menu1, VALUE='Input Area', uvalue='Input Area')

  menu2 = WIDGET_BUTTON(bar, VALUE='Plot', /MENU)
button1 = WIDGET_BUTTON(menu2, VALUE='Timeseries', uvalue='Timeseries')
button1a =  WIDGET_BUTTON(menu2, VALUE='Num timeseries', uvalue='Num timeseries')
button2 = WIDGET_BUTTON(menu2, VALUE='T-S diagram', uvalue='TS diagram') 

  menu1a = WIDGET_BUTTON(bar, VALUE='Find',/MENU)
button1a= WIDGET_BUTTON(menu1a, VALUE='Worst points', uvalue='Worst points')
button1a1 = WIDGET_BUTTON(menu1a, VALUE='Filter type', uvalue='Filter type')

  menu3 = WIDGET_BUTTON(bar, VALUE='Config', /MENU)
loresmap = WIDGET_BUTTON(menu3, VALUE='Low res map', uvalue='Low res map', /checked_menu)
hiresmap = WIDGET_BUTTON(menu3, VALUE='Hi res map', uvalue='Hi res map', /checked_menu)

button3 = WIDGET_BUTTON(menu3, VALUE='Square psym', uvalue='Square psym')
button4 = WIDGET_BUTTON(menu3, VALUE='Round psym', uvalue='Round psym')

pltonbado = WIDGET_BUTTON(menu3, VALUE='Plot only bad obs', uvalue='Plot only bad obs', /checked_menu)

whtonblack = WIDGET_BUTTON(menu3, VALUE='White on black', uvalue='White on black', /checked_menu)

incsym = WIDGET_BUTTON(menu3, VALUE='Psym size+ ', uvalue='incsym')
decsym = WIDGET_BUTTON(menu3, VALUE='Psym size- ', uvalue='decsym')
resetsym = WIDGET_BUTTON(menu3, VALUE='Psym size reset ', uvalue='resetsym')

vertgradmenu = WIDGET_BUTTON(menu3, VALUE='Vert gradient', uvalue='vertgrad', /checked_menu)
button12 = WIDGET_BUTTON(menu3, VALUE='Input max/min', uvalue='input max/min')


  menu4 = WIDGET_BUTTON(bar, VALUE='Help', /MENU)
help1 = WIDGET_BUTTON(menu4, VALUE='Info', uvalue='Info')

if (n_elements(filename) gt 0) then begin
  menu5 = WIDGET_BUTTON(bar, VALUE=filename[0])
nel=n_elements(filename)
nel2=nel
nelmx=32
if (nel2 gt nelmx) then nel2=nelmx
for i=0L, nel2-1 do begin
  id = WIDGET_BUTTON(menu5, VALUE=filename[i])
endfor
if (nel gt nelmx) then id=WIDGET_BUTTON(menu5, VALUE='... etc ...')
endif

Widget_Control, loresmap, Set_Button=1
Widget_Control, vertgradmenu, Set_Button=vertgrad 

Widget_Control, pltonbado, Set_Button=plot_only_bad_obs
Widget_Control, whtonblack, Set_Button=white_on_black
 
  ; Create a draw widget based on the size of the image, and 
  ; set the MOTION_EVENTS keyword so that events are generated 
  ; as the cursor moves across the image. Setting the BUTTON_EVENTS 
  ; keyword rather than MOTION_EVENTS would require the user to 
  ; click on the image before an event is generated. 

;	im_size=[2,1024,800]
;	im_size=[2,800,800]
;	im_size=[2,800,720]
;	im_size=[2,1024,720]
        im_size=[2,1024,680]
;	im_size=[2,800,640]

  draw = WIDGET_DRAW(base, XSIZE=im_size[1], YSIZE=im_size[2], $ 
    /BUTTON_EVENTS,/WHEEL_EVENTS, uvalue='MAPWINDOW') 

;	base2=widget_base(GROUP_LEADER=base,/column)

;  droplist = WIDGET_COMBOBOX(base2,value=string([indgen(101)]),$
;  	uvalue='LEVELLIST', FRAME=2)

;  button = WIDGET_BUTTON(base2, value='test',uvalue='test')


;	print,'depminl/maxl ',depminl, depmaxl
;        print,'value depmin ',depmin
;        print,'value depmax ',depmax

depmaxlsl=depmaxl
depminlsl=depminl
if (depmaxlsl eq depminlsl) then depmaxlsl=depmaxlsl+1
print, 'depminlsl, depmaxlsl ',depminlsl, depmaxlsl
  slider = WIDGET_SLIDER(base,maximum=depmaxlsl,minimum=depminlsl,scroll=depscl,$
  	value=depmin,uvalue='LEVELCHOICE', ysize=32)

  sliderb = WIDGET_SLIDER(base,maximum=depmaxlsl,minimum=depminlsl, scroll=depscl,$
        value=depmax,uvalue='LEVELCHOICEB', ysize=32)


  jul_to_dtstr,daymin,dayminstr,/notime
  slider1label = WIDGET_LABEL(base, VALUE="min date: "+dayminstr, xsize=800,ysize=14)
  slider1 = WIDGET_SLIDER(base,minimum=dayminsl,maximum=daymaxsl,scroll=1,$
  	value=daymin,uvalue='DATERANGE1',/suppress_value,/drag)

  jul_to_dtstr,daymax,daymaxstr,/notime
  slider2label = WIDGET_LABEL(base, VALUE="max date: "+daymaxstr, xsize=800,ysize=14)
  slider2 = WIDGET_SLIDER(base,minimum=dayminsl,maximum=daymaxsl,scroll=1,$
  	value=daymax,uvalue='DATERANGE2',/suppress_value,/drag)



;  slider1 = WIDGET_SLIDER(base,minimum=dayminl,maximum=daymaxl,scroll=1,$
;  	value=daymin,uvalue='DATERANGE1',PRO_SET_VALUE="datesliderset",/drag)
;
;  slider2 = WIDGET_SLIDER(base,minimum=dayminl,maximum=daymaxl,scroll=1,$
;  	value=daymax,uvalue='DATERANGE2',PRO_SET_VALUE="datesliderset",/drag)
	 
  ; Labels for stats
   
  labt1='Lon: '
  labt2='Lat: '
  labt3='Value: '
  labt4='QC: '
  labt5='Date: '
  labt6='Value: '
  labt7=' '
  labt8='Type: ' 
  labt9='obnum: '
  labt10='visible date range: '
  
  ; Create label widgets to hold the cursor position and 
  ; Hexadecimal value of the pixel under the cursor. 
  labelb=Widget_Base(base,row=1)
  
  labsiz=105
  labsizl1=150
  labsizlg=150
  labsizl2=135
  label7=0
  lb_ysize=14
  label1 = WIDGET_LABEL(labelb, XSIZE=labsiz, $ 
    VALUE=labt1, ysize=lb_ysize) 
  label2 = WIDGET_LABEL(labelb, XSIZE=labsiz, $ 
    VALUE=labt2, ysize=lb_ysize) 
  label3 = WIDGET_LABEL(labelb, XSIZE=labsizl1, $ 
    VALUE=labt3, ysize=lb_ysize) 
  label4 = WIDGET_LABEL(labelb, XSIZE=labsiz, $ 
    VALUE=labt4, ysize=lb_ysize)
  label5 = WIDGET_LABEL(labelb, XSIZE=labsizlg, $ 
    VALUE=labt5, ysize=lb_ysize) 
  label6 = WIDGET_LABEL(labelb, XSIZE=labsiz, $ 
    VALUE=labt6, ysize=lb_ysize)
;  label7 = WIDGET_LABEL(labelb, XSIZE=labsiz, $ 
;    VALUE=labt7, ysize=lb_ysize)
  label8 = WIDGET_LABEL(labelb, XSIZE=labsizl2, $ 
    VALUE=labt8, ysize=lb_ysize)
  label9 = WIDGET_LABEL(labelb, XSIZE=labsiz, $ 
    VALUE=labt9, ysize=lb_ysize)

  labelb2 = Widget_Base(base,row=1)
  label10 = WIDGET_LABEL(labelb2, XSIZE=1024, $
    VALUE=labt10, ysize=lb_ysize)

   tlba = Widget_Base(base,row=1)

   t1b = Widget_Base(tlba,Title='Push-Buttons', row=1, Scr_XSize=200,$
   /Exclusive)
   button1 = Widget_Button(t1b, Value='mean',uvalue='RADIO1',/no_release)
   button2 = Widget_Button(t1b, Value='rms',uvalue='RADIO2',/no_release)
   button3 = Widget_Button(t1b, Value='sd',uvalue='RADIO3',/no_release)
   button4 = Widget_Button(t1b, Value='ms',uvalue='RADIO4',/no_release)

   if (typeplot eq 1) then Widget_Control, button1, Set_Button=typeplot
   if (typeplot eq 2) then Widget_Control, button2, Set_Button=typeplot
   if (typeplot eq 3) then Widget_Control, button3, Set_Button=typeplot
   if (typeplot eq 4) then Widget_Control, button4, Set_Button=typeplot
  
   t1c = Widget_Base(tlba,Title='Push-Buttons', row=1, Scr_XSize=200,$
   /Exclusive)
   button5 = Widget_Button(t1c, Value='obs - bkg', uvalue='RADIO5',/no_release)
   button6 = Widget_Button(t1c, Value='obs', uvalue='RADIO6',/no_release)
   button7 = Widget_Button(t1c, Value='bkg', uvalue='RADIO7',/no_release)

   if (ombtypeplot eq 1) then Widget_Control, button5, /Set_Button
   if (ombtypeplot eq 2) then Widget_Control, button6, /Set_Button
   if (ombtypeplot eq 3) then Widget_Control, button7, /Set_Button

;   tlb = Widget_Base(tlba,Title='Push-Buttons', row=1, Scr_XSize=400,$
;   /Exclusive)
;; no_release only sends select events (not release ones)
;   button1 = Widget_Button(tlb, Value='rms Obs - Bkg',uvalue='RADIO1',/no_release)
;   button2 = Widget_Button(tlb, Value='sum(Obs - Bkg)^2',uvalue='RADIO2',/no_release)
;   button3=  Widget_Button(tlb, Value='Obs - Bkg',uvalue='RADIO3',/no_release)
;   button4 = Widget_Button(tlb, Value='Obs',uvalue='RADIO4',/no_release)
;   button5 = Widget_Button(tlb, Value='Bkg',uvalue='RADIO5',/no_release)
;
;   if (typeplot eq 1) then Widget_Control, button1, Set_Button=typeplot
;   if (typeplot eq 2) then Widget_Control, button2, Set_Button=typeplot
;   if (typeplot eq 3) then Widget_Control, button3, Set_Button=typeplot
;   if (typeplot eq 4) then Widget_Control, button4, Set_Button=typeplot
;   if (typeplot eq 5) then Widget_Control, button5, Set_Button=typeplot
  
   tlb1 = Widget_Base(tlba,Title='Push-Buttons', row=1, Scr_XSize=220,$
   /Exclusive)
   button01 = Widget_Button(tlb1, Value='Lat/Lon',uvalue='RADIO01',/no_release)
   button02 = Widget_Button(tlb1, Value='N Polar',uvalue='RADIO02',/no_release)
   button03 = Widget_Button(tlb1, Value='S Polar',uvalue='RADIO03',/no_release)

    Widget_Control, button01, Set_Button=typeproj


   tlb1a = Widget_Base(tlba,Title='Push-Buttons', row=1, Scr_XSize=250,$
   /NonExclusive)
   button001 = Widget_Button(tlb1a, Value='Plot Bad Obs', uvalue='RADIO001')

   Widget_Control, button001, Set_Button=plot_bad_obs


   if filetype eq 'CRT' then begin

      button002 = Widget_Button(tlb1a, Value='Meridional', uvalue='RADIOSAL')
      Widget_Control, button002, Set_Button=salinity

      button003 = Widget_Button(tlb1a, Value='Speed', uvalue='RADIODENSITY')
      Widget_Control, button003, Set_Button=density

   endif else begin
;         tlb1b = Widget_Base(tlba,Title='Push-Buttons', row=1, Scr_XSize=90,$
;         /NonExclusive)
      button002 = Widget_Button(tlb1a, Value='Salinity', uvalue='RADIOSAL')
      Widget_Control, button002, Set_Button=salinity
;         tlb1c = Widget_Base(tlba,Title='Push-Buttons', row=1, Scr_XSize=90,$
;          /NonExclusive)
      button003 = Widget_Button(tlb1a, Value='Density', uvalue='RADIODENSITY')
      Widget_Control, button003, Set_Button=density
   endelse

 
   tlb2= Widget_Base(tlba,row=1)
  button = WIDGET_BUTTON(tlb2, VALUE='Print',uvalue="PRINT") 
  button = WIDGET_BUTTON(tlb2, VALUE='Save',uvalue="SAVE")
   
  button = WIDGET_BUTTON(tlb2, VALUE='Done',uvalue="DONE") 



  ; Realize the widget hierarchy. 
  WIDGET_CONTROL, base, /REALIZE 
;  WIDGET_CONTROL, base2, /REALIZE 
;   WIDGET_CONTROL, tlb, /REALIZE 

  ; Retrieve the widget ID of the draw widget. Note that the widget 
  ; hierarchy must be realized before you can retrieve this value. 
  WIDGET_CONTROL, draw, GET_VALUE=drawID 

 
  ; Create an anonymous array to hold the image data and widget IDs 
  ; of the label widgets. 
;  stash = { imagePtr:imagePtr, label1:label1, label2:label2, $ 
;            label3:label3, $
;	    xarr:xarr, yarr:yarr, dep:dep, val:val } 


spawn,'echo part 2 `date`'


  stash = { label1:label1, label2:label2, $ 
            label3:label3, label4:label4, $
            label5:label5, label6:label6, $
            label7:label7, label8:label8, $
            label9:label9, label10:label10, $
            labt1:labt1, labt2:labt2, $
            labt3:labt3, labt4:labt4, $
            labt5:labt5, labt6:labt6, $
            labt7:labt7, labt8:labt8, $
            labt9:labt9, labt10:labt10, $
            slider1label:slider1label, $
            slider2label:slider2label, $
            drawID:drawID, draw:draw, base:base, $
            im_size:im_size, pixID:0, $
            xcorn:0, ycorn:0, xdatacorn:0.0, ydatacorn:0.0, $
            slider1:slider1, slider2:slider2, $
            slider:slider, sliderb:sliderb, $
            hiresmap:hiresmap, loresmap:loresmap, vertgradmenu:vertgradmenu, $
            pltonbado:pltonbado, plot_only_bad_obs:plot_only_bad_obs, $
            whtonblack:whtonblack, white_on_black:white_on_black, $
            duplicates:duplicates, differences:differences, $
            bindata:bindata, binsize:binsize, $
            filetype:filetype, $
            xarr:xarr, yarr:yarr, dep:dep, dayarr:dayarr, $
            obs:obs, bkg:bkg, qcarr:qcarr, obstypes: obstypes, $
            obs2:obs2, obs3:obs3, bkg2:bkg2, qcarr2:qcarr2, $
            obnum:obnum, $
            depmin:depmin, typeplot:typeplot, ombtypeplot:ombtypeplot, $
            typeproj:typeproj, $
            hires_map:hires_map, $
            plot_bad_obs:plot_bad_obs, salinity: salinity, $
            density:density, vertgrad:vertgrad, mld:mld, $
            daymin:daymin, daymax:daymax, dayminl:dayminl, daymaxl:daymaxl, $
            dayshi:dayshi, $
            depminl:depminl, depmaxl:depmaxl, depmax:depmax, depscl:depscl, $
            xrange:xrange, yrange:yrange, $
            fmx:fmx, fmn:fmn, mx:mx, mn:mn, $
            symsize:symsize, sym:sym, picsize:picsize, $
            pmulti:pmulti, pmultinum:pmultinum, $
            outfile:outfile, $ 
            xrangedef:xrangedef, yrangedef:yrangedef, rmdi:rmdi, $
            obstypeselect:obstypeselect, printobstypes:printobstypes, $
            plottssel:plottssel, $
            binspday:binspday, numtimeseries:numtimeseries, txt:txt, netcdf:netcdf, $
            busy:busy, symscale:symscale, coltable:coltable, $
            filterout:filterout, varname:varname } 
	     
  ; Set the user value of the top-level base widget equal to the 
  ; 'stash' array. 
  WIDGET_CONTROL, base, SET_UVALUE=stash 
 
  ; Make the draw widget the current IDL drawable area. 
  WSET, drawID 

spawn,'echo part 3 `date`' 

  ; Draw the image into the draw widget. 
  plotpoints, stash

; plotpoints updates mx/mn values make sure these are included
  WIDGET_CONTROL, base, SET_UVALUE=stash 

spawn,'echo part 4 `date`' 

  ; Call XMANAGER to manage the widgets. 
  XMANAGER, 'dataplot', base, /NO_BLOCK
;  XMANAGER, 'dataplot', base
endif else begin

;do batch

print,"batch"

drawID=-1

print, 'filetype: ',filetype

  stash = { drawID:drawID,$	    
            filetype:filetype, $
            xarr:xarr, yarr:yarr, dep:dep, dayarr:dayarr, $
            obs:obs, bkg:bkg, qcarr:qcarr, obstypes: obstypes, $
            obs2:obs2, bkg2:bkg2, qcarr2:qcarr2, $
            obnum:obnum, $
            depmin:depmin, typeplot:typeplot, ombtypeplot:ombtypeplot, $
            typeproj:typeproj, $
            hires_map:hires_map, $
            plot_only_bad_obs:plot_only_bad_obs, $
            white_on_black:white_on_black, $
            duplicates:duplicates, differences:differences, $
            bindata: bindata, binsize:binsize, $
            plot_bad_obs:plot_bad_obs, salinity: salinity, $
            density:density, vertgrad:vertgrad, mld:mld, $
            daymin:daymin, daymax:daymax, dayminl:dayminl, daymaxl:daymaxl, $
            dayshi:dayshi, $            
            depminl:depminl, depmaxl:depmaxl, depmax:depmax, depscl:depscl, $
            xrange:xrange, yrange:yrange, $
            fmx:fmx, fmn:fmn, mx:mx, mn:mn, $
            symsize:symsize, sym:sym, picsize:picsize, $
            pmulti:pmulti, pmultinum:pmultinum, $ 
            outfile:outfile, $ 
            xrangedef:xrangedef, yrangedef:yrangedef, rmdi:rmdi, $
            obstypeselect:obstypeselect, printobstypes:printobstypes, $
            plottssel:plottssel, $
            binspday:binspday, numtimeseries:numtimeseries, txt:txt, netcdf:netcdf, $
            busy:busy, symscale:symscale, coltable:coltable, $
            filterout:filterout, varname:varname } 

if (keyword_set(ps)) then begin
  	ps=1
	eps=0
	landscape=1
  	pr2,file=stash.outfile+'.ps',landscape=landscape,ps=ps,eps=eps,color=1	

; plot data
	if keyword_set(timeseries) then	begin	; plot timeseries if keyword is set
        	plottimeseries,stash
        endif else begin			; otherwise plot points on a map
  		plotpoints,stash
	endelse

	prend2,view=0

endif 

if (keyword_set(gif)) then begin
	thisDevice = !D.Name

        Set_Plot, 'Z'			; do graphics in the background
;	Device, Set_Resolution=[640,512], decomposed=0
;	Device, Set_Resolution=[800,512], decomposed=0
        Device, Set_Resolution=picsize, decomposed=0
        Erase                           ; clear any existing stuff
        !p.charsize=0.75
        setupct, r, g, b, coltable=stash.coltable, $
        	white_on_black=stash.white_on_black		; setup color table

; plot data
	if keyword_set(timeseries) then	begin	; plot timeseries if keyword is set
        	plottimeseries,stash
        endif else begin			; otherwise plot points on a map
  		plotpoints,stash
	endelse

;	if (keyword_set(gif)) then begin
;        IMAGE_GRAB, /gif, filename=stash.outfile+'.gif',/overwrite        
	snapshot = TVRD()
        WRITE_GIF,stash.outfile+'.gif',snapshot, r, g, b
;        endif
        Device, Z_Buffer=1		; reset graphics mode
        Set_Plot, thisDevice
        !p.charsize=0.0

endif

if (keyword_set(txt)) then begin
	if keyword_set(timeseries) then	begin	; plot timeseries if keyword is set
        	plottimeseries,stash
        endif else begin			; otherwise plot points on a map
  		plotpoints,stash
;		print,'no txt version of plotpoints!'
	endelse
endif	

if (keyword_set(netcdf)) then begin
	if keyword_set(timeseries) then begin
        	print,'no netcdf version of timeseries!'
        endif else begin
  		plotpoints,stash        
        endelse
endif

if (stash.filterout ne '') then begin

   selpoints, stash, lonsel, latsel, innovsel, qcsel, daysel, obstypsel, obnumsel, numsel, typestr

endif

endelse            ; batch
 
END 

