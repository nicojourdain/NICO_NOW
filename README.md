# NEMO - OASIS - WRF (NOW)

## preprocessing/

The preprocessing directory contains bash/fortran tools to build input files needed for NEMO and OASIS (see WPS user guide to build WRF's input). See specific README.txt file.

## models/

The model directory contains old versions of NEMO, OASIS and XIOS that were modified and tested for coupling. See specific README.txt file.

To rather use up-to-date versions, visit [the NEMO and XIOS page](http://forge.ipsl.jussieu.fr/nemo/wiki/Users), [the OASIS page](https://portal.enes.org/oasis), or [the WRF page](http://www2.mmm.ucar.edu/wrf/users/download/get_source.html) and register as a user.

For example, you can get XIOS-1 or XIOS-2 through these command lines :
```shell
svn co http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-1.0 XIOS  ## XIOS-1
svn co -r 1011 http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/trunk XIOS     ## XIOS-2
```

And, for example, you can get NEMO through this kind of commands (you need to be registered) :
```shell
svn co http://forge.ipsl.jussieu.fr/nemo/svn/trunk MY_NEMO           ## for the last trunk version
svn co -r 6402 http://forge.ipsl.jussieu.fr/nemo/svn/trunk MY_NEMO   ## for version 6402 of the trunk
svn co http://forge.ipsl.jussieu.fr/nemo/svn/branches/2015/nemo_v3_6_STABLE MY_NEMO   ## NEMO3.6 STABLE 
```

And, for example, you can get OASIS3-MCT2 through this command:
```shell
svn co http://oasis3mct.cerfacs.fr/svn/branches/OASIS3-MCT_2.0_branch/oasis3-mct oa3mct
```

For WRF, any version from v3.6 should work. See WRF documentation for compiling and preprocessing (WPS). The only thing that differs for coupling with OASIS is that you need to add the key\_cpp\_oasis3 compilation key in the configure file:
```shell
ARCH_LOCAL = -DNONSTANDARD_SYSTEM_FUNC -DCHUNK=64 -DXEON_OPTIMIZED_WSM5 -DOPTIMIZE_CFL_TEST -DWRF_USE_CLM -Dkey_cpp_oasis3
```

## run/

This contains scripts to run NOW on raijin. See specific README.txt file. To adapt for your own architecture...

## Acknowledgment

The NOW model is described in Samson et al. (2014). The provided model version was tested on the Australian National Computational Infrastructure thanks to the help of Sébastien Masson and Rachid Benshila.

Samson, G., Masson, S., Lengaigne, M., Keerthi, M., Vialard, J., Pous, S., Madec, G., Jourdain, N. C., Jullien, S., Menkes, C. and Marchesiello, P. (2014). The NOW Regional Coupled Model: Application to the Tropical Indian Ocean Climate and Tropical Cyclones Activity. _Journal of Advances in Modeling Earth Systems_, 6(3), 700-722. [doi:10.1002/2014MS000324](http://onlinelibrary.wiley.com/doi/10.1002/2014MS000324/abstract)
