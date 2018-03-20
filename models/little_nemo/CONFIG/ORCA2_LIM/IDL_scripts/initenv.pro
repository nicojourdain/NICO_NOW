;
; This is the initialisation file.
; it defines the !path and the defaut values of some of the common variables
;
; this is supposed to speed-up IDL...
;
; a = fltarr(1000,1000,100)
; a = 0
;
; path definition
;
!path = expand_path('+' + getenv('SAXO_DIR') ) + ':' + expand_path('+' + !dir)
;
; compatibility with the old version
;
keep_compatibility, 0
;
; define all the commons
;
@all_cm
;
; define default directories
;
homedir = './'
iodir = './'
psdir = isadirectory(getenv('PS_DIR'), title = 'Select the default postscripts directory')
imagedir = './'
animdir = './'
;
; define printer parameters
;
printer_human_names = ''
printer_machine_names = ''
print_command = ''
;
; colors ...
;
device, decomposed = 0
device, retain = 2
lct, 65
;
; postscript parameters ...
;
key_portrait = 0
page_size = [20.9903, 29.7039]
windowsize_scale = 1.0000
archive_ps = 0
;
;========================================================
; end of the part that should be modified by the users...
;========================================================
;
; if needed, keep compatibility with the old version
;
@updateold
;
