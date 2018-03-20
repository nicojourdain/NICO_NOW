PRO std_ts_read, var_name, dt1, dt2, prefix, suffix, ts, ts_z, masknp $
                 , WITHSSH = withssh, SSHPREFIX = sshprefix, SSHSUFFIX = sshsuffix, LEVZ = levz

  compile_opt idl2, strictarrsubs
  
@common
@std_common


  list = rseries_ncdf(var_name, dt1, dt2, prefix, suffix, /fileslist)
  nfiles = n_elements(list)
  IF keyword_set(withssh) THEN BEGIN
    sshlist = rseries_ncdf(withssh, dt1, dt2, sshprefix, sshsuffix, /fileslist)
    IF nfiles NE n_elements(sshlist) THEN stop
  ENDIF

  ts = 0.
  ts_Time = 0.
  ts_z = fltarr(jpk)

  FOR i = 0, nfiles-1 DO BEGIN
    IF keyword_set(withssh) THEN ssh = read_ncdf(withssh, allrecords = allrec, filename = sshlist[i], /nostruct)
    var = read_ncdf(var_name, allrecords = allrec, filename = list[i], /nostruct)
    ts_Time = [ ts_Time, Time]
;
    IF jpt EQ 1 THEN txyz = moyenne(var, 'xyz', mask2d = masknp, ssh = ssh) $
    ELSE txyz = grossemoyenne(var, 'xyz', mask2d = masknp, ssh = ssh)
    ts = [ ts, txyz ]
;
    IF jpt EQ 1 THEN tz = moyenne(temporary(var), 'xy', mask2d = masknp, ssh = ssh, /KEEPBOTTOM) $
    ELSE tz = grossemoyenne(temporary(var), 'xy', mask2d = masknp, ssh = ssh, /KEEPBOTTOM)
    IF keyword_set(levz) THEN ts_z = [ [ts_z], [tz - (levz[*] # replicate(1., jpt))] ] $
    ELSE ts_z = [ [ts_z], [tz] ]
  ENDFOR

  time = ts_Time[1:*]           ; remove first record of 0
  jpt = n_elements(time)
  
  ts = ts[1:*]        ; remove first record of 0 
  ts_z = ts_z[*, 1:*]           ; remove first record of 0


return
end
