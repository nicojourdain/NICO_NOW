#!/usr/bin/env python2.6

import os
import shutil

def freer(oldname, newname, fdir, edits):

    fin = file(os.path.join(fdir,oldname))
    lines = fin.readlines()
    fin.close()

    if not os.path.exists('src'):
        print "creating src directory"
        os.mkdir('src')
    fout = file(os.path.join('src',newname), 'w')

    lastline = None
    for nextline in lines:
        for edit in edits:
            nextline = nextline.replace(edit[0],edit[1])
        if nextline.strip().startswith('&'):
            parts = lastline.split('!')
            if parts[0].endswith('\n'):
                parts[0] = parts[0].replace('\n',' &\n')
            else:
                parts[0] = parts[0] + ' & '
            lastline = '!'.join(parts)
            nextline = nextline.replace('&',' ')
        if lastline is not None:
            fout.write(lastline)
        lastline = nextline

    # - write out last line (this assumes it is not a continuation line)
    fout.write(lastline)

def scrip(adir, apairs, edits=[]):

    if not os.path.exists('src'):
        print "creating src directory"
        os.mkdir('src')

    for (f1,f2) in apairs:
        if not f2.endswith('.f90'):
            shutil.copy(os.path.join(adir,f1), os.path.join('src',f2))
        else:
            freer(f1,f2,adir,edits)

if __name__ == "__main__":

    # - changes to scrip routines made here

    pairs = [('constants.f', 'constants.f90'),
              ('copyright', 'copyright'),
              ('grids.f', 'grids.f90'),
              ('iounits.f', 'iounits.f90'),
              ('kinds_mod.f', 'kinds_mod.f90'),
              ('netcdf.f', 'netcdf_mod.f90'),
              ('remap.f', 'remap.f90'),
              ('remap_bicubic.f', 'remap_bicubic.f90'),
              ('remap_bilinear.f', 'remap_bilinear.f90'),
              ('remap_conserv.f', 'remap_conserv.f90'),
              ('remap_distwgt.f', 'remap_distwgt.f90'),
              ('remap_read.f', 'remap_read.f90'),
              ('remap_vars.f', 'remap_vars.f90'),
              ('remap_write.f', 'remap_write.f90'),
              ('timers.f', 'timers.f90')]

    # - add some edits
    # - note that this is very crude method since every line is inspected for the first string
    # - in every input file
    # - you have been warned!

    ed1 = [" .and."]
    ed2 = ["subroutine netcdf_error_handler(istat, mess)"]
    ed3 = ["    &    istat   ! integer status returned by netCDF function call",
           "      character (len=*), intent(in), optional :: mess"]
    ed4 = ["       if (present(mess)) then",
           "         print *,'Error in netCDF: ',nf_strerror(istat), 'Message: ',mess",
           "       else",
           "         print *,'Error in netCDF: ',nf_strerror(istat)",
           "       endif"]

    edits = [(". and.",'\n'.join(ed1)),
             ("subroutine netcdf_error_handler(istat)", '\n'.join(ed2)),
             ("    &    istat   ! integer status returned by netCDF function call", '\n'.join(ed3)),
             ("        print *,'Error in netCDF: ',nf_strerror(istat)", '\n'.join(ed4))
            ]
    scrip('SCRIP1.4/source', pairs, edits)


    # - on to NOCS routines

    pairs = [('scrip.F90', 'scrip.F90'),
             ('scripgrid.F90', 'scripgrid.F90'),
             ('scripgrid_mod.F90', 'scripgrid_mod.F90'),
             ('scripinterp.F90', 'scripinterp.F90'),
             ('scripinterp_mod.F90', 'scripinterp_mod.F90'),
             ('scripshape.F90', 'scripshape.F90')]
    scrip('nocsutil', pairs)

    changes = """
      SCRIP code, version 1.4, from Los Alamos National Laboratory (http://climate.lanl.gov/Software/SCRIP)

      Changes made at NOCS for inclusion of weights generation code in NEMO 3.3 and later:

          - File extensions changed from '.f' to '.f90'
          - File netcdf.f renamed as netcdf_mod.f90 to avoid clash with netcdf library module filename
          - File netcdf.f modified to add error message to netcdf_error_handler
          - Small bug in remap_conserv when using gfortran compiler: replace ". and." with " .and."
          - continuation lines reformatted with '&' moved from the start of the continuation line to 
            the end of the line before
    """
    fp = file("src/CHANGES_BY_NOCS","w")
    fp.write(changes)
    fp.close()
