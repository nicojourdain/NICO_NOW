#!/bin/bash
#PBS -P PPPP
#PBS -q copyq
#PBS -l wd
#PBS -l other=mdss,walltime=00:02:00 


WORKDIR='WWWW'
STOCKDIR='SSSS'
CONFIG='CCCC'
CASE='OOOO'
YEAR='YYYY'
DOMAIN=XXXX
MAX_DOM=ZZZZ

cd $WORKDIR

DOMAIN="0${DOMAIN}"

echo " "
echo "Copy files to ${STOCKDIR}..."

mdss mkdir ${STOCKDIR}/output/now_${CONFIG}_${CASE}/MONTHLY_FULL
mdss mkdir ${STOCKDIR}/output/now_${CONFIG}_${CASE}/MONTHLY_DIURNAL
mdss mkdir ${STOCKDIR}/output/now_${CONFIG}_${CASE}/MONTHLY_2D
mdss mkdir ${STOCKDIR}/output/now_${CONFIG}_${CASE}/3HOURLY_pLev 
mdss mkdir ${STOCKDIR}/output/now_${CONFIG}_${CASE}/DAILY
mdss mkdir ${STOCKDIR}/output/now_${CONFIG}_${CASE}/DAILY/${YEAR}

if [ ! -d TMP ]; then
  mkdir TMP
else
  rm -f TMP/*
fi

#############################################

netmv -p -d TMP -l walltime=05:00:00 WORK_${YEAR}/wrfout_d${DOMAIN}_${YEAR}-0[1-3]-??.nc  ${STOCKDIR}/output/now_${CONFIG}_${CASE}/DAILY/${YEAR}/.
netmv -p -d TMP -l walltime=05:00:00 WORK_${YEAR}/wrfout_d${DOMAIN}_${YEAR}-0[4-6]-??.nc  ${STOCKDIR}/output/now_${CONFIG}_${CASE}/DAILY/${YEAR}/.
netmv -p -d TMP -l walltime=05:00:00 WORK_${YEAR}/wrfout_d${DOMAIN}_${YEAR}-0[7-9]-??.nc  ${STOCKDIR}/output/now_${CONFIG}_${CASE}/DAILY/${YEAR}/.
netmv -p -d TMP -l walltime=05:00:00 WORK_${YEAR}/wrfout_d${DOMAIN}_${YEAR}-1[0-2]-??.nc  ${STOCKDIR}/output/now_${CONFIG}_${CASE}/DAILY/${YEAR}/.

##########################################
#- Exporting monthly means and daily pLev

netmv -p -d TMP -l walltime=01:00:00 ${WORKDIR}/wrfout_d${DOMAIN}_${YEAR}_monthly.nc        ${STOCKDIR}/output/now_${CONFIG}_${CASE}/MONTHLY_FULL/.
netmv -p -d TMP -l walltime=01:00:00 ${WORKDIR}/wrfout_d${DOMAIN}_${YEAR}_mon_*.nc          ${STOCKDIR}/output/now_${CONFIG}_${CASE}/MONTHLY_2D/.

netmv -p -d TMP -l walltime=02:00:00 ${WORKDIR}/wrfout_d${DOMAIN}_${YEAR}-0[1-3]_diur_*.nc  ${STOCKDIR}/output/now_${CONFIG}_${CASE}/MONTHLY_DIURNAL
netmv -p -d TMP -l walltime=02:00:00 ${WORKDIR}/wrfout_d${DOMAIN}_${YEAR}-0[4-6]_diur_*.nc  ${STOCKDIR}/output/now_${CONFIG}_${CASE}/MONTHLY_DIURNAL
netmv -p -d TMP -l walltime=02:00:00 ${WORKDIR}/wrfout_d${DOMAIN}_${YEAR}-0[7-9]_diur_*.nc  ${STOCKDIR}/output/now_${CONFIG}_${CASE}/MONTHLY_DIURNAL
netmv -p -d TMP -l walltime=02:00:00 ${WORKDIR}/wrfout_d${DOMAIN}_${YEAR}-1[0-2]_diur_*.nc  ${STOCKDIR}/output/now_${CONFIG}_${CASE}/MONTHLY_DIURNAL

for PLEV in `grep "pLev=" postprocess_wrf_pLev.f90 | sed -e "s/pLev=(\///g ; s/\/)//g ; s/\,/ /g ; s/\.0//g"`  '10m'
do

 netmv -p -d TMP -l walltime=03:00:00 ${WORKDIR}/wrfout_d${DOMAIN}_${YEAR}_3HOURLY_${PLEV}.nc ${STOCKDIR}/output/now_${CONFIG}_${CASE}/3HOURLY_pLev/.

done

############

if [ $DOMAIN -lt ${MAX_DOM} ]; then
  DP1=`expr $DOMAIN + 1`
  echo "--- End : submitting import_d0${DP1}.sh for next domain"
  qsub import_d0${DP1}.sh
else
  YEARp1=`expr $YEAR + 1`
  echo "${YEARp1}" >> post_${CONFIG}_${CASE}.db
  echo "--- End : last domain to process => go for the following year"
  qsub postprocess.sh
fi

