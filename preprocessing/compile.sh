#!/bin/bash

#module list
#module unload netcdf
#module load netcdf/4.1.1-intel

# Fortran compiler :
FC='ifort'

# Netcdf libraries :
export NC_INC='-I /usr/local/netcdf/intel/4.1.1/include'
export NC_LIB='-L /usr/local/netcdf/intel/4.1.1/lib -lnetcdf -lnetcdff'

file=$1

 filobj=`basename $file | sed -e "s/\.f90/\.o/g"`
 namexe=`basename $file | sed -e "s/\.f90//g"`

 rm -f $namexe $filobj
 $FC -c $NC_INC $file
 $FC -o $namexe $filobj $NC_LIB
 
 if [ -f $namexe ]; then
  rm -f $filobj
  echo "$namexe   [oK]"
 else
  echo "~!@#%^&* ERROR : $namexe HAS NOT BEEN CREATED >>>>>>>> stop !!"
  exit
 fi
