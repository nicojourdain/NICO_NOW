#!/bin/bash
#PBS -P PPPP
#PBS -q normal
#PBS -l wd
#PBS -l walltime=24:00:00,mem=4Gb

WORKDIR='WWWW'
CONFIG='CCCC'
CASE='OOOO'
YEAR='YYYY'
DOM=XXXX

cd $WORKDIR

if [ -d WORK_${YEAR} ]; then

  echo "******************************************"
  echo "*   Netcdf compression                    "
  echo "******************************************"

  cd WORK_${YEAR}

  rm -f tmp.nc

  module load netcdf/4.2.1.1

  for file in wrfout_d0${DOM}_${YEAR}-*-*_00:00:00
  do

    # daily average instead of 3-hourly
    ncra $file tmp.nc
    if [ -f tmp.nc ]; then
      mv -f tmp.nc $file
    else
      echo "!@#$% PROBLEM WITH NCRA OF FILE $file"
      exit
    fi

    # compress
    nccopy -d 9 $file tmp.nc
    if [ -f tmp.nc ]; then
      mv -f tmp.nc $file
    else
      echo "!@#$% PROBLEM WITH COMPRESSION OF FILE $file"
      exit
    fi

    # change files name:
    file2=`echo $file | sed -e "s/_00\:00\:00/\.nc/g"`
    mv $file $file2

  done

  cd $WORKDIR

else 

  echo "******************************************"
  echo "*    No files to compress !!!!!           "
  echo "******************************************"

  exit
  
fi

##########################################################

qsub export_d0${DOM}.sh


