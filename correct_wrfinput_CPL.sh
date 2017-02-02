#!/bin/bash

CONFIG='trop075_nest025'

for DOM in 'd01'  'd02'
do


mdss mkdir NICO/now/input/wrf_${CONFIG}/CPL

#module unload netcdf
#module load netcdf
#NC_INC='-I /apps/netcdf/4.2.1.1/include'
#NC_LIB='-L /apps/netcdf/4.2.1.1/lin -lnetcdf -lnetcdff'

for YEAR in $(seq 1989 2012)
do

  file="wrfinput_${DOM}_${YEAR}-01-01_${YEAR}-12-31"
  echo $file

  mdss get NICO/now/input/wrf_${CONFIG}/${file}

  ## check version to see wether CPLMASK is already in wrfinput file
  VERSION=`ncdump -h ${file} |grep "OUTPUT FROM REAL_EM" | cut -d ' ' -f7 | sed -e "s/V// ; s/\.//" | cut -c 1-2`
  if [ $VERSION -ge 36 ]; then
    ncks -O -x -v CPLMASK $file $file
  fi

  ncks -A  cplmask_${DOM}_${CONFIG}.nc $file

  mdss put $file NICO/now/input/wrf_${CONFIG}/CPL/. 

done


done
