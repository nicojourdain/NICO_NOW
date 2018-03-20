#!/bin/bash
#PBS -P PPPP
#PBS -q copyq
#PBS -l wd
#PBS -l other=mdss,walltime=02:00:00 

WORKDIR='WWWW'
STOCKDIR='SSSS'
CONFIG='CCCC'
CASE='OOOO'
YEAR='YYYY'
iDOM=XXXX

cd $WORKDIR

echo " "
echo " Copy input files from ${STOCKDIR}/output/now_${CONFIG}_${CASE}/${YEAR}"

if [ ! -d TMP ]; then
  mkdir TMP
else
  rm -f TMP/*
fi

if [ ! -d WORK_${YEAR} ]; then 
  mkdir WORK_${YEAR}
fi

cd WORK_${YEAR}

iDOM="0${iDOM}"
for MONTH in 01 02 03 04 05 06 07 08 09 10 11 12
do
 mdss get NICO/now/output/now_${CONFIG}_${CASE}/${YEAR}/wrfout_d${iDOM}_${YEAR}-${MONTH}-*_00:00:00
 mdss get NICO/now/output/now_${CONFIG}_${CASE}/${YEAR}/wrfhrly_d${iDOM}_${YEAR}-${MONTH}-*_00:00:00
done

echo " "
echo "Transfer completed."
echo " "

cd $WORKDIR

qsub concat_d${iDOM}.sh

