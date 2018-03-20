#!/bin/bash
#PBS -P PPPP
#PBS -q normal
#PBS -l wd
#PBS -l walltime=24:00:00,mem=16Gb

WORKDIR='WWWW'
NRUN=NNNN
CONFPAR='cccc'
CASE='OOOO'

cd $WORKDIR

echo "******************************************"
echo "*   Netcdf compression before export      "
echo "******************************************"

rm -f tmp.nc

module unload netcdf
module load netcdf/4.2.1.1

#- Netcdf library for fortran scripts (not for running models)
NC_INC='-I /apps/netcdf/4.2.1.1/include'
NC_LIB='-L /apps/netcdf/4.2.1.1/lib  -lnetcdf -lnetcdff'

cd OUTPUT_${NRUN}

chown ncj561:e14 *

###################################################################
##-- rebuild NEMO outputs (nests + parent grid)
##   if xios's iodef.xml is in "multiple_file" mode :

MULTIPLE_FILE=`grep multiple_file ../iodef.xml  | wc -l`

if [ $MULTIPLE_FILE -gt 0 ]; then

    echo "############################################################################"
    echo "## xios is in multiple_file mode ==> rebuilding outputs into single files ##"

    for file in `ls -1 *${CONFPAR}-${CASE}_*_0000.nc`
    do

      prefix_file=`echo $file | sed -e "s/_0000\.nc//g"`
      sed -e "s/pppppp/${prefix_file}/g" ../rebuild_outputs_GENERIC.f90 > rebuild_outputs.f90
      ifort -c $NC_INC rebuild_outputs.f90
      ifort -o rebuild_outputs rebuild_outputs.o $NC_LIB
      ./rebuild_outputs
      rm -f rebuild_outputs rebuild_outputs.o rebuild_outputs.f90

      file_out=`echo $file | sed -e "s/_0000//g"`
      if [ -f ${file_out} ]; then
        echo "${file_out}   [oK]"
        rm -f ${prefix_file}_[0-9][0-9][0-9][0-9].nc
      else
        echo "~!@#%^&* ERROR : ${file_out} HAS NOT BEEN CREATED !!!"
        echo "                 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> STOP !!"
        exit
      fi

    done

fi

##########################################################
##-- compress WRF's output files:
##   ( read list of files from this run only, not to read
##     outputs from new on going simulation )

NFILES=`ls -1 wrfout* | wc -l | cut -d ' ' -f1`
for ifile in $(seq 1 $NFILES)
do
  file=`ls -1 wrfout* | head -${ifile} | tail -1`
  nccopy -d 9 $file tmp.nc
  if [ -f tmp.nc ]; then
    mv -f tmp.nc $file
  else
    echo "!@#$% PROBLEM WITH COMPRESSION OF FILE $file"
    echo " >>>>>>>>>> STOP !"
    exit
  fi 
  ifile=`expr $ifile + 1`
done

NFILES=`ls -1 wrfhrly* | wc -l | cut -d ' ' -f1`
for ifile in $(seq 1 ${NFILES})
do
  file=`ls -1 wrfhrly* | head -${ifile} | tail -1`
  nccopy -d 9 $file tmp.nc
  if [ -f tmp.nc ]; then
    mv -f tmp.nc $file
  else
    echo "!@#$% PROBLEM WITH COMPRESSION OF FILE $file"
    echo " >>>>>>>>>> STOP !"
    exit
  fi
done

##########################################################
##-- compress NEMO's output files:

NFILES=`ls -1 *_[1-6][dh]_*.nc | wc -l | cut -d ' ' -f1`
for ifile in $(seq 1 $NFILES)
do
   file=`ls -1 *_[1-6][dh]_*.nc | head -${ifile} | tail -1`
   nccopy -d 9 $file tmp.nc
   if [ -f tmp.nc ]; then
      mv -f tmp.nc $file
   else
      echo "!@#$% PROBLEM WITH COMPRESSION OF FILE $file"
      echo " >>>>>>>>>> STOP !"
      exit
   fi
   ifile=`expr $ifile + 1`
done

##########################################################
##-- send outputs and restarts to storage disk

cd $WORKDIR

qsub export_now_${NRUN}.sh


