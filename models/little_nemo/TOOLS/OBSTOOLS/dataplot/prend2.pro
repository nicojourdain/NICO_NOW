PRO PREND2,in_printer,keep=keep,noprint=noprint,double=double,$
    view=view,spin_landscape_ps=spin_landscape_ps,$
    eps_preview=eps_preview,colour_preview=colour_preview,$
    dpi_preview=dpi_preview, suppress_stderr=suppress_stderr
;+
; NAME:prend
; 
; Author:  D. J. Lea       Feb 2008

COMMON pr2, view_landscape

; get filename of postscript device  

r = fstat(!D.UNIT)
filename = r.name

device,/close          
set_plot,'x'

if keyword_set(view) then begin

   if (view_landscape) then begin
      spawn,'gv -orientation=landscape -swap '+filename
   endif else begin
      spawn,'gv '+filename 
   endelse

endif

!p.font=-1
!p.charsize=0
!p.charthick=0
!p.thick=0
!p.background=0

;return

end
