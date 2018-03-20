function nice_contourlevels, data, nlevels=nlevels
;+----------------------------------------------------------------------------------------
; nice_contourlevels.pro
;
; Select nice contour levels based on the data input and the number of levels
;
; Author:  D. J. Lea     -  Feb 2008
;
;+----------------------------------------------------------------------------------------

if (n_elements(nlevels) eq 0) then nlevels=15

mx=max(data)
mn=min(data)

; use this to select colors to plot and labels

clevels=findgen(nlevels)/nlevels*(mx-mn)+mn

ocint=1./nlevels*(mx-mn)
print,'ocint ',ocint

; contour interval at 2 sig figs
;digits=2
;p10 = floor(alog10(abs(ocint)))
;expo = 10.0d^(digits -1 - p10)
;cint = long(ocint*expo)/expo
;print,'cint ',cint

;does it end in 5 or 0?
digits=1
p10 = floor(alog10(abs(ocint)))
expo = 10.0d^(digits -1 - p10)
cint = long(ocint*expo)/expo
print,'cint ',cint

if (mx ne mn) then begin
mxfix=fix(mx/cint)*cint
mnfix=fix(mn/cint)*cint
print, mx, mxfix
print, mn, mnfix

; nice contour values

;calculate new nlevels
nlevels=fix((mxfix-mnfix)/cint+1)

;print,nlevels
if (nlevels gt 0) then begin
	clevels=findgen(nlevels)*cint+mnfix
endif else begin
	clevels=mnfix
endelse
endif else begin
	clevels=mn
endelse

return, clevels

end
