#!/bin/bash
#PBS -P PPPP
#PBS -q copyq
#PBS -l wd
#PBS -l other=mdss 

WORKDIR='WWWW'
CONFIG='CCCC'
CONFPAR='cccc'
CASE='OOOO'
YEAR='YYYY'
MONTH='MMMM'
DAY='DDDD'
YEARf='yyyy'
MONTHf='mmmm'
DAYf='dddd'
STOCKDIR='SSSS'
NRUN=NNNN
MAX_DOM_WRF=GGGG
NZOOM=ZZZZ
NITEND=UUUU

######################################################################
## NB: YOU CAN USE A PROJECT DIRECTORY ON MASSDATA DIFFERENT FROM PPPP
## (e.g. PROJMDSS='n81' Otherwise, just leave PROJMDSS=PPPP below)
#PROJMDSS='PPPP'
PROJMDSS='e14'

YEARp1=`expr $YEAR + 1`

echo " "
echo "Copy files to ${STOCKDIR}..."

mdss mkdir ${STOCKDIR}/restart/now_${CONFIG}_${CASE}
mdss mkdir ${STOCKDIR}/output/now_${CONFIG}_${CASE}
mdss mkdir ${STOCKDIR}/output/now_${CONFIG}_${CASE}/${YEAR}
mdss mkdir ${STOCKDIR}/output/now_${CONFIG}_${CASE}/${YEARp1}

if [ ! -d TMP ]; then
  mkdir TMP
fi

#####################################################################
##- export WRF's output files

for iDOM in $(seq 1 ${MAX_DOM_WRF})
do

  iDOM="0${iDOM}"

  ## start with wrfout files ##

  NFILES=`ls -1 ${WORKDIR}/OUTPUT_${NRUN}/wrfout_d${iDOM}* | wc -l`
  echo "Exporting $NFILES wrf output files for domain d${iDOM}"
 
  ##-- ONLY FOR d01 IN CORDEX24 config : do not copy last output (only one time step in it)
  if [ $iDOM -eq 1 ]; then 
    last_file=`ls -1 ${WORKDIR}/OUTPUT_${NRUN}/wrfout_d${iDOM}* | tail -1`
    rm -f ${last_file}
  fi 

  ##-- separate into several transfers if too many files (no more than 100 allowed)
  if [ $NFILES -gt 100 ]; then
    first_month=`ls -1 ${WORKDIR}/OUTPUT_${NRUN}/wrfout_d${iDOM}* | head -1`
    first_month=`basename $first_month | cut -d '_' -f3 | cut -d '-' -f2`
    last_month=`ls -1  ${WORKDIR}/OUTPUT_${NRUN}/wrfout_d${iDOM}* | tail -1`
    last_month=`basename $last_month | cut -d '_' -f3 | cut -d '-' -f2`
    echo "first_month=$first_month  last_month=$last_month"
    iimonth=$first_month
    while [ $iimonth -le $last_month ];
    do
      echo "    > exporting wrfout_d${iDOM}_${YEAR}-${iimonth}*"
      netmv -P $PROJMDSS -p -d TMP -l walltime=03:00:00 ${WORKDIR}/OUTPUT_${NRUN}/wrfout_d${iDOM}_${YEAR}-${iimonth}*  ${STOCKDIR}/output/now_${CONFIG}_${CASE}/${YEAR}/.
      iimonth=`expr $iimonth + 1`
      if [ ${iimonth} -lt 10 ]; then
        iimonth="0${iimonth}"
      fi
    done
  else
    echo "    > exporting wrfout_d${iDOM}*"
    netmv -P $PROJMDSS -p -d TMP -l walltime=03:00:00 ${WORKDIR}/OUTPUT_${NRUN}/wrfout_d${iDOM}* ${STOCKDIR}/output/now_${CONFIG}_${CASE}/${YEAR}/.
  fi

  ## then wrfhrly ##

  NFILES=`ls -1 ${WORKDIR}/OUTPUT_${NRUN}/wrfhrly_d${iDOM}* | wc -l`
  echo "Exporting $NFILES wrf output files for domain d${iDOM}"
  
  ##-- do not copy last output (only one time step in it)
  last_file=`ls -1 ${WORKDIR}/OUTPUT_${NRUN}/wrfhrly_d${iDOM}* | tail -1`
  rm -f ${last_file}

  ##-- separate into several transfers if too many files (no more than 100 allowed)
  if [ $NFILES -gt 100 ]; then
    first_month=`ls -1 ${WORKDIR}/OUTPUT_${NRUN}/wrfhrly_d${iDOM}* | head -1`
    first_month=`basename $first_month | cut -d '_' -f3 | cut -d '-' -f2`
    last_month=`ls -1  ${WORKDIR}/OUTPUT_${NRUN}/wrfhrly_d${iDOM}* | tail -1`
    last_month=`basename $last_month | cut -d '_' -f3 | cut -d '-' -f2`
    echo "first_month=$first_month  last_month=$last_month"
    iimonth=$first_month
    while [ $iimonth -le $last_month ];
    do
      echo "    > exporting wrfhrly_d${iDOM}_${YEAR}-${iimonth}*"
      netmv -P $PROJMDSS -p -d TMP -l walltime=03:00:00 ${WORKDIR}/OUTPUT_${NRUN}/wrfhrly_d${iDOM}_${YEAR}-${iimonth}*  ${STOCKDIR}/output/now_${CONFIG}_${CASE}/${YEAR}/.
      iimonth=`expr $iimonth + 1`
      if [ ${iimonth} -lt 10 ]; then
        iimonth="0${iimonth}"
      fi
    done
  else
    echo "    > exporting wrfhrly_d${iDOM}*"
    netmv -P $PROJMDSS -p -d TMP -l walltime=03:00:00 ${WORKDIR}/OUTPUT_${NRUN}/wrfhrly_d${iDOM}* ${STOCKDIR}/output/now_${CONFIG}_${CASE}/${YEAR}/.
  fi

done ## iDOM

#####################################################################
##- export NEMO's output files

NFILES=`ls -1 ${WORKDIR}/OUTPUT_${NRUN}/${CONFPAR}-${CASE}_[1-6][dh]*.nc | wc -l`
echo "Exporting $NFILES NEMO output files"
#NB: separate into several transfers if too many files (no more than 100 allowed)
netmv -P $PROJMDSS -p -d TMP -N NEMOout ${WORKDIR}/OUTPUT_${NRUN}/${CONFPAR}-${CASE}_[1-6][dh]*.nc ${STOCKDIR}/output/now_${CONFIG}_${CASE}/${YEAR}/.

for iZOOM in $(seq 1 ${NZOOM})
do
  NFILES=`ls -1 ${WORKDIR}/OUTPUT_${NRUN}/${iZOOM}_${CONFPAR}-${CASE}_[1-6][dh]*.nc | wc -l`
  echo "Exporting $NFILES NEMO output files for NEST $NZOOM"
  echo "netmv -P $PROJMDSS -p -d TMP ${WORKDIR}/OUTPUT_${NRUN}/${iZOOM}_${CONFPAR}-${CASE}_[1-6][dh]*.nc ${STOCKDIR}/output/now_${CONFIG}_${CASE}/${YEAR}/."
  ##-- separate into several transfers if too many files (no more than 100 allowed)
  netmv -P $PROJMDSS -p -d TMP -N NEMOoutzoom ${WORKDIR}/OUTPUT_${NRUN}/${iZOOM}_${CONFPAR}-${CASE}_[1-6][dh]*.nc ${STOCKDIR}/output/now_${CONFIG}_${CASE}/${YEAR}/.
done

####################################################################
##- Export previous OASIS restart files
##  (keep last ones for next resubmission)

NRST=`ls -1 ${WORKDIR}/sstoc_Od01_to_Ad01_[0-9]*.nc |wc -l`
for iRST in $(seq 2 $NRST) ##NB: start at 2 to keep last one for next resubmission
do
  DTMP_RST=`ls -1 ${WORKDIR}/sstoc_Od01_to_Ad01_[0-9]*.nc | tail -${iRST} | head -1`
  NUMRST=`basename $DTMP_RST | cut -d '_' -f5 | cut -d '.' -f1`
  tar cvf oasis_${NUMRST}.tar sstoc_Od0?_to_Ad0?_${NUMRST}.nc flxat_Ad0?_to_Od0?_${NUMRST}.nc
  echo "Exporting oasis_${NUMRST}.tar"
  if [ -f oasis_${NUMRST}.tar ]; then
    netmv -P $PROJMDSS -p -d TMP -l walltime=00:20:00 oasis_${NUMRST}.tar  ${STOCKDIR}/restart/now_${CONFIG}_${CASE}/.
    rm -f sstoc_Od0?_to_Ad0?_${NUMRST}.nc flxat_Ad0?_to_Od0?_${NUMRST}.nc
  else
    echo "~!@#$%^&* oasis_${NUMRST}.tar has not been created >>>>>>> stop !!!!"
    exit
  fi
done

####################################################################
##- Export WRF's restart files
#      no more than 100 files in a single netmv
#      NB: starting to copy last restart date (for next re-submission)

NRST=`ls -1 ${WORKDIR}/wrfrst_d01_*_00:00:00_0000 | wc -l`
iRST=2 ## NB: the last restart file is kept for the next run
while [ $iRST -le $NRST ]; 
do
 DTMP_RST=`ls -1 ${WORKDIR}/wrfrst_d01_*_00:00:00_0000 | tail -${iRST} | head -1`
 DATE_RST=`basename $DTMP_RST | cut -d '_' -f3`
 tar -cvf wrfrst_${DATE_RST}.tar  wrfrst_d0*_${DATE_RST}_00:00:00_*
 echo "Exporting wrfrst_${DATE_RST}.tar"
 if [ -f wrfrst_${DATE_RST}.tar ]; then
   netmv -P $PROJMDSS -p -d TMP -l walltime=03:00:00 ${WORKDIR}/wrfrst_${DATE_RST}.tar ${STOCKDIR}/restart/now_${CONFIG}_${CASE}/.
   rm -f wrfrst_d0*_${DATE_RST}_00:00:00_*
 else
   echo "!@#$%^&* ERROR: wrfrst_${DATE_RST}.tar has not been created >>>>>> stop"
   exit
 fi
 iRST=`expr $iRST + 1`
done

####################################################################
##- Export NEMO's previous restart files
##  (keep the last one for next re-submission)

MRST=`ls -1 ${WORKDIR}/restart_????????.nc | wc -l`
for jRST in $(seq 2 $MRST)  ## NB: the last restart file is kept for the next run
do
 DTRP_RST=`ls -1 ${WORKDIR}/restart_????????.nc | tail -${jRST} | head -1`
 NIT_RST=`basename ${DTRP_RST} | cut -d '_' -f2 | cut -d '.' -f1`
 tar -cvf ${CONFPAR}-${CASE}_${NIT_RST}_restart.tar  restart_${NIT_RST}.nc restart_${NIT_RST}.obc
 echo "Exporting ${CONFPAR}-${CASE}_${NIT_RST}_restart.tar"
 if [ -f ${CONFPAR}-${CASE}_${NIT_RST}_restart.tar ]; then
   netmv -P $PROJMDSS -p -d TMP -N NEMOrst ${CONFPAR}-${CASE}_${NIT_RST}_restart.tar ${STOCKDIR}/restart/now_${CONFIG}_${CASE}/.
   rm -f ${CONFPAR}-${CASE}_${NIT_RST}_restart_*.nc *${NIT_RST}_restart_*.obc
 else
   echo "!@#$%^&* ERROR: ${CONFPAR}-${CASE}_${NIT_RST}_restart.tar has not been created >>>>>> stop"
   exit
 fi
done

for iZOOM in $(seq 1 $NZOOM)
do

  NRST=`ls -1 ${WORKDIR}/${iZOOM}_restart_????????.nc | wc -l`
  for jRST in $(seq 2 $NRST) ## NB: the last restart file is kept for the next run
  do
   DTMP_RST=`ls -1 ${WORKDIR}/${iZOOM}_restart_????????.nc | tail -${jRST} | head -1`
   NIT_RST=`basename ${DTMP_RST} | cut -d '_' -f3 | cut -d '.' -f1`
   tar -cvf ${iZOOM}_${CONFPAR}-${CASE}_${NIT_RST}_restart.tar ${iZOOM}_restart_${NIT_RST}.nc
   echo "Exporting ${iZOOM}_${CONFPAR}-${CASE}_${NIT_RST}_restart.tar"
   if [ -f ${iZOOM}_${CONFPAR}-${CASE}_${NIT_RST}_restart.tar ]; then
     netmv -P ${PROJMDSS} -p -d TMP -N NEMOrstzoom ${iZOOM}_${CONFPAR}-${CASE}_${NIT_RST}_restart.tar ${STOCKDIR}/restart/now_${CONFIG}_${CASE}/.
     rm -f ${iZOOM}_restart_${NIT_RST}.nc
   else
     echo "!@#$%^&* ERROR: ${iZOOM}_${CONFPAR}-${CASE}_${NIT_RST}_restart.tar has not been created >>>>>> stop"
     exit
   fi
  done

done

####################################################################

netmv -P $PROJMDSS -p -d TMP -l walltime=00:30:00 ${WORKDIR}/*nam*.${NRUN}     ${STOCKDIR}/output/now_${CONFIG}_${CASE}/${YEAR}/.
netmv -P $PROJMDSS -p -d TMP -l walltime=00:30:00 ${WORKDIR}/*ocean.output.${NRUN} ${STOCKDIR}/output/now_${CONFIG}_${CASE}/${YEAR}/.

echo "Transfer completed."
echo " "
