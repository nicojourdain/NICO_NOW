  PRO Pr2,device,$
         bw=bw,$
         color=o_color,$
         pcl=pcl,$
         ps=ps,$
         cps=cps,$
         eps=eps,$
         cgm=cgm,$
         screen=screen,$
         transparency=o_transparency,$
         encapsulated=encapsulated,$
         bpp=bpp,$
         portrait=portrait,$
         landscape=landscape,$
         table=table,$
         reread=reread,$
         noflip=noflip,$
         verbose=verbose,$
         xsize=xsize,$
         ysize=ysize,$
         xoffset=xoff,$
         yoffset=yoff,$
         scale=scale,$
         file=file,$
         destination=destination
;+----------------------------------------------------------
; NAME:pr2
; like pr, but uses nice fonts
;
; Author:  D. J. Lea        Feb 2008
;+----------------------------------------------------------

COMMON pr2, view_landscape

	set_plot,'ps'

	if (n_elements(color) eq 0) then color=1
        if (n_elements(landscape) eq 0) then landscape=1
        if (keyword_set(portrait)) then landscape=0
        encapsulated=1
        if (keyword_set(eps)) then encapsulated=1
        if (keyword_set(ps)) then encapsulated=0
        
        view_landscape=landscape
        
	DEVICE, filename=file, COLOR=color, landscape=landscape, $
        	encapsulated=encapsulated, xsize=xsize, ysize=ysize


!p.font=0
device,/helv
	 
;return

end
