#!/bin/bash
#PBS -P e14
#PBS -q express
#PBS -l wd
#PBS -l walltime=00:05:00

###################################################
# N. JOURDAIN, UNSW-CCRC, MAR. 2014               #
#                                                 #
# This is the script to execute for long runs     #
#                                                 #
# It will:                                        #
# 1- inititalize variables                        #
# 2- launch a batch script to import variables    #
#    -> the later script launching the batch      #
#       script to run wrf and then to export files#
#       and to relaunch the present script        # 
###################################################

##############################################
##-- User's choices:

PROJ='e14'      #- NB: must be the same as PBS -P above

CONFIG='cordex24' ## FULL CONFIG NAME (e.g. "trop075" or "trop075_nest025")
                  ## NB: THIS NAME SHOULD NOT START WITH A NUMBER

CONFPAR='cordex24' #- IF NO NEST SHOULD BE EQUAL TO $CONFIG
                   #  IF NESTS, SHOULD BE THE ABSOLUTE PARENT CONFIG NAME 
                   #  (e.g. CONFPAR="trop075" when CONFIG="trop075_nest025")

CASE='BMJv2_BILAP' # should not be too long (>20 char.) otherwise, NEMO file names are affected

YEAR0=1989      #- initial state of the long experiment

YEAR_MAX=2009   #- stop after $YEAR_MAX is completed

NRUN_MAX=500    #- stop after $NRUN_MAX re-submissions

NDAYS=366       #- Split year in 3 runs (adjust for end of year)

WORKDIR="/short/e14/ncj561/now/run/now_${CONFIG}_${CASE}"

STOCKDIR="NICO/now"  #- input, restart, output

WRFdir='../../models/wrf3.5.1_CPL'

NEMOdir='../../models/little_nemo'

#- Netcdf library for small fortran scripts (not for NEMO)
NC_INC='-I /apps/netcdf/4.2.1.1/include'
NC_LIB='-L /apps/netcdf/4.2.1.1/lib  -lnetcdf -lnetcdff'

#####################################################
##-- initializations

export PROJECT="$PROJ"

cd $WORKDIR

#####################################################
##-- read nb of nests for each model :

MAX_DOM_WRF=`cat namelist_wrf_GENERIC_${CONFIG} | grep max_dom | sed -e "s/\,//g" |cut -d '=' -f2 | sed -e "s/ //g"`
NLIN=`cat AGRIF_FixedGrids.in |wc -l`
NLIN="0${NLIN}"
NZOOM_NEMO=`echo "$NLIN / 3" | bc`
MAX_DOM_NEMO=`expr ${NZOOM_NEMO} + 1`
echo "**** Nb of grids in WRF  : ${MAX_DOM_WRF}"
echo "**** Nb of grids in NEMO : ${MAX_DOM_NEMO}"
echo " "

######################################################
##-- Manage nb of days, prod_wrf.db and prod_nemo.db

if [ -f prod_wrf.db ] && [ -f prod_nemo.db ]; then
read NRUN YEAR MONTH DAY << EOF
`tail -1 prod_wrf.db`
EOF
read NRUNnem YEARnem MONTHnem DAYnem NITENDM1 NITENDM1ZOOM << EOF
`tail -1 prod_nemo.db`
EOF
if [ $MONTHnem -lt 10 ]; then
  MONTHnem="0`expr $MONTHnem + 0`"  # put as "0x" whatever it was
fi
if [ $DAYnem -lt 10 ]; then
  DAYnem="0`expr $DAYnem + 0`"  # put as "0x" whatever it was
fi
if [ ! $NRUN == $NRUNnem ] || [ ! $YEAR == $YEARnem ] || [ ! $MONTH == $MONTHnem ] || [ ! $DAY == $DAYnem ]; then
  echo "!@#$%^&* ERROR : mismatch between dates in prod_wrf.db and prod_nemo.db !!!"
  echo " prod_wrf.db  : `tail -1 prod_wrf.db`"
  echo " prod_nemo.db : `tail -1 prod_nemo.db`"
  exit
fi
else
echo "1 ${YEAR0} 01 01"   > prod_wrf.db
echo "1 ${YEAR0} 01 01 0" > prod_nemo.db
YEAR=${YEAR0}
MONTH=01
DAY=01
NRUN=1
NITENDM1=0  ## last time step of previous nemo run
## add last time step of previous runs on children domains
## at the end of the line in prod_nemo.db :
for iZOOM in $(seq 1 ${NZOOM_NEMO})
do
  sed -e "s/$/ 0/g" prod_nemo.db > tmp
  mv tmp prod_nemo.db
done
fi

if [ $YEAR -gt ${YEAR_MAX} ]; then
  echo " "
  echo "Year greater then YEAR_MAX >>>>>>>> stop !!"
  exit
fi

if [ $NRUN -gt ${NRUN_MAX} ]; then
  echo " "
  echo "Nb of re-submission greater then NRUN_MAX >>>>>>>> stop !!"
  exit
fi

TEST_RESUB=`grep "$YEAR $MONTH $DAY ok" prod_wrf.db | wc -l`
if [ $TEST_RESUB -gt 0 ]; then
  echo '!@#$%^&* Something has gone wrong with prod_wrf.db, please check >>>>>> STOP'
  exit
fi

TEST_RESUB=`grep "ok ok" prod_wrf.db | wc -l`
if [ $TEST_RESUB -gt 0 ]; then
  echo '!@#$%^&* Something has gone wrong with prod_wrf.db, please check >>>>>> STOP'
  exit
fi

# adjust nb of days to finish at the end of the year
if [ ! -f calculate_end_date ]; then
  ifort -o calculate_end_date calculate_end_date.f90
fi
echo "$YEAR $MONTH $DAY $NDAYS" > start_date_duration
read YEARf MONTHf DAYf NDAYScorr << EOF
`./calculate_end_date`
EOF
if [ $NDAYScorr -ne $NDAYS ]; then
 echo "Adjusting run length to finish at the end of current year"
 NDAYS=$NDAYScorr
 echo "$YEAR $MONTH $DAY $NDAYS" > start_date_duration
read YEARf MONTHf DAYf NDAYScorr << EOF
`./calculate_end_date`
EOF
fi
echo " -> Run duration = $NDAYS days"
echo " "

##-- calculate corresponding number of time steps for NEMO:
RN_DT=`grep "rn_rdt " namelist_nemo_GENERIC_${CONFPAR} |cut -d '=' -f2 | cut -d '!' -f1 | sed -e "s/ //g"`
NIT000=`echo "$NITENDM1 + 1" | bc`
NITEND=`echo "$NITENDM1 + ${NDAYS} * 86400 / ${RN_DT}" | bc`

# Restart interval (minutes) at the end of run only:
RESTART_INT=`expr $NDAYS \* 1440`

echo " Restart interval = ${RESTART_INT} minutes"

DUR_SECS=`expr $NDAYS \* 86400`

echo "******************************************"
echo "* NOW (NEMO-OASIS-WRF) SIMULATION :       "
echo "*   project $PROJ                         "
echo "*   config  $CONFIG                       "
echo "*   case    $CASE                         "
echo "*   from    ${DAY}/${MONTH}/${YEAR}       "
echo "*   to      ${DAYf}/${MONTHf}/${YEARf}    "
echo "*   NEMO step $NIT000 to $NITEND          "
echo "******************************************"

echo " "

###############################################################
##-- create executable to rebuild NEMO restart files

## need to modify CONFIG and CASE in rebuild_restart.f90
if [ $NRUN -eq 1 ]; then
  rm -f rebuild_restart
fi

if [ ! -f rebuild_restart ]; then
  module unload netcdf
  module load netcdf/4.2.1.1
  sed -e "s/CCCC/${CONFPAR}/g ; s/OOOO/${CASE}/g ; s/zzzz//g" rebuild_restart_GENERIC.f90 > rebuild_restart.f90
  ifort -c $NC_INC rebuild_restart.f90
  ifort -o rebuild_restart rebuild_restart.o $NC_LIB
  module unload netcdf
fi

for iZOOM in $(seq 1 ${NZOOM_NEMO})
do

  ## need to modify CONFIG and CASE in rebuild_restart.f90
  if [ $NRUN -eq 1 ]; then
    rm -f ${iZOOM}_rebuild_restart
  fi

  if [ ! -f ${iZOOM}_rebuild_restart ]; then
    module unload netcdf
    module load netcdf/4.2.1.1
    sed -e "s/CCCC/${CONFPAR}/g ; s/OOOO/${CASE}/g ; s/zzzz/${iZOOM}_/g" rebuild_restart_GENERIC.f90 > ${iZOOM}_rebuild_restart.f90
    ifort -c $NC_INC ${iZOOM}_rebuild_restart.f90
    ifort -o ${iZOOM}_rebuild_restart ${iZOOM}_rebuild_restart.o $NC_LIB
    module unload netcdf
  fi

done

echo " "

###############################################################
##-- edit WRF namelist :

echo "Editing WRF's namelist.input..."

if [ $NRUN -gt 1 ]; then
  sed -e "s/RESTART/\.true\./g"  namelist_wrf_GENERIC_${CONFIG} > namelist.input
  RSTW=1
else
  sed -e "s/RESTART/\.false\./g" namelist_wrf_GENERIC_${CONFIG} > namelist.input
  RSTW=0
fi

sed -e "s/YYYY/${YEAR}/g ; s/MMMM/${MONTH}/g ; s/DDDD/${DAY}/g ; s/NDAYS/${NDAYS}/g ; s/RSTINT/${RESTART_INT}/g" namelist.input > tmp
mv -f tmp namelist.input 

echo " "

###############################################################
##-- edit NEMO namelist (for xxxx075, start from restart built from ORCA025):

echo "Editing NEMO's namelist..."

if [ $NRUN -gt 1 ] || [ $CONFPAR == "xxxx075" ]; then
  sed -e "s/RESTNEM/\.true\./g"  namelist_nemo_GENERIC_${CONFPAR} > namelist
  RSTN=1
else
  sed -e "s/RESTNEM/\.false\./g" namelist_nemo_GENERIC_${CONFPAR} > namelist
  RSTN=0
fi

##- Specific treatment for TROP075's restart/initial state:
if [ $NRUN -eq 1 ] && [ $CONFPAR == "xxxx075" ]; then
  sed -e "s/AAAA/  0 /g ; s/BBBB/false/g" namelist > tmp
  mv -f tmp namelist
else
  sed -e "s/AAAA/  2 /g ; s/BBBB/true/g"  namelist > tmp
  mv -f tmp namelist
fi

sed -e "s/CCCC/${CONFPAR}/g ; s/OOOO/${CASE}/g ; s/IIII/${YEAR0}0101/g ; s/NIT000/${NIT000}/g ; s/NITEND/${NITEND}/g" namelist > tmp
mv -f tmp namelist

for iZOOM in $(seq 1 ${NZOOM_NEMO})
do
  echo "Editing NEMO's ${iZOOM}_namelist..."
  if [ $NRUN -gt 1 ] || [ $CONFPAR == "xxxx075" ]; then
    sed -e "s/RESTNEM/\.true\./g"  ${iZOOM}_namelist_nemo_GENERIC_${CONFPAR} > ${iZOOM}_namelist
    RST=1
  else
    sed -e "s/RESTNEM/\.false\./g" ${iZOOM}_namelist_nemo_GENERIC_${CONFPAR} > ${iZOOM}_namelist
    RST=0
  fi
  ##- Specific treatment for TROP075's restart/initial state:
  if [ $NRUN -eq 1 ] && [ $CONFPAR == "xxxx075" ]; then
    sed -e "s/AAAA/  0 /g" ${iZOOM}_namelist > tmp
    mv -f tmp ${iZOOM}_namelist
  else
    sed -e "s/AAAA/  2 /g"  ${iZOOM}_namelist > tmp
    mv -f tmp ${iZOOM}_namelist
  fi
  ##- calculate initial and last time step for the child domains :
  ##-- calculate corresponding number of time steps for NEMO:
  RN_DT_ZOOM=`grep "rn_rdt " ${iZOOM}_namelist_nemo_GENERIC_${CONFPAR} |cut -d '=' -f2 | cut -d '!' -f1 | sed -e "s/ //g"`
  NIT000_ZOOM=`echo "( ${NITENDM1} * ${RN_DT} / ${RN_DT_ZOOM} ) + 1" | bc`
  NITEND_ZOOM=`echo "( ${NITENDM1} * ${RN_DT} / ${RN_DT_ZOOM} ) + ${NDAYS} * 86400 / ${RN_DT_ZOOM}" | bc`
  ##--
  sed -e "s/CCCC/${CONFPAR}/g ; s/OOOO/${CASE}/g ; s/IIII/${YEAR0}0101/g ; s/NIT000/${NIT000_ZOOM}/g ; s/NITEND/${NITEND_ZOOM}/g" ${iZOOM}_namelist > tmp
  mv -f tmp ${iZOOM}_namelist
done

echo " "

###############################################################
##-- edit namcouple (OASIS' namelist) :
##
##   NB: from ocean to atmosphere, the lag is NEMO's time step 
##   on the parent/child grid if it is sent from the parent/child. 
##   From atmosphere to ocean, the lag is the time step of WRF's 
##   child grid whatever if it is sent from parent or child.

echo "Editing OASIS's namcouple..."

##- PARENT GRIDS :

PERIODIC=`cat namelist_wrf_GENERIC_${CONFIG} | grep periodic_x | sed -e "s/\,/ /g" | sed -e "s/\.//g" | awk '{print $3}'`
LKJ=`echo $PERIODIC | wc -c`
if [ $LKJ -le 1 ]; then
  PERIODIC="false"
fi
NBXd01=`cat namelist_wrf_GENERIC_${CONFIG} | grep e_we | sed -e "s/\,/ /g" | awk '{print $3}'`
if [ $PERIODIC == "true" ]; then
  echo "Parent grid is periodic."
  NBXd01m1=$NBXd01
  GRID_PER="P  2"
else
  echo "Parent grid is not periodic."
  NBXd01m1=`expr $NBXd01 - 1`
  GRID_PER="R  0"
fi
NBYd01=`cat namelist_wrf_GENERIC_${CONFIG} | grep e_sn | sed -e "s/\,/ /g" | awk '{print $3}'`
NBYd01m1=`expr $NBYd01 - 1`
NBTNEMd01=`echo ${RN_DT} | sed -e "s/\.//g"`
NBTWRFd01=`cat namelist_wrf_GENERIC_${CONFIG} | grep " time_step " | sed -e "s/\,//g" |cut -d '=' -f2 | sed -e "s/ //g"`
CPLFREQd01=`echo "${NBTNEMd01} * 10" | bc`
sed -e "s/DUR_SECS/${DUR_SECS}/g ; s/nbxNEMd01/${NBXd01m1}/g ; s/nbyNEMd01/${NBYd01m1}/g ; s/nbxWRFd01/${NBXd01}/g ; s/nbyWRFd01/${NBYd01}/g ; s/NBTNEMd01/${NBTNEMd01}/g ; s/NBTWRFd01/${NBTWRFd01}/g ; s/cplfreqd01/${CPLFREQd01}/g ; s/gridper/${GRID_PER}/g" namcouple_GENERIC_${CONFIG} > namcouple
echo "**** WRF  GRID1 DIMENSIONS = (${NBXd01},${NBYd01}) ; WRF  GRID1 TIME STEP = ${NBTWRFd01}"
echo "**** NEMO GRID1 DIMENSIONS = (${NBXd01m1},${NBYd01m1}) ; NEMO GRID1 TIME STEP = ${NBTNEMd01}"

##- CHILD GRIDS :

for iDOMWRF in $(seq 2 ${MAX_DOM_WRF})
do

  PGR=`grep parent_grid_ratio namelist.input | cut -d '=' -f2 | sed -e "s/ //g" |cut -d ',' -f${iDOMWRF}`
  NBXchild=`cat namelist_wrf_GENERIC_${CONFIG} | grep e_we | cut -d '=' -f2 | sed -e "s/ //g" |cut -d ',' -f${iDOMWRF}`
  NBYchild=`cat namelist_wrf_GENERIC_${CONFIG} | grep e_sn | cut -d '=' -f2 | sed -e "s/ //g" |cut -d ',' -f${iDOMWRF}`
  DT_RATIO_child=`cat namelist_wrf_GENERIC_${CONFIG} | grep parent_time_step_ratio | cut -d '=' -f2 | sed -e "s/ //g" |cut -d ',' -f${iDOMWRF}`
  NBTWRFchild=`echo "${NBTWRFd01} / ${DT_RATIO_child}" | bc`
  sed -e "s/nbxWRFd0${iDOMWRF}/${NBXchild}/g ; s/nbyWRFd0${iDOMWRF}/${NBYchild}/g ; s/NBTWRFd0${iDOMWRF}/${NBTWRFchild}/g" namcouple > tmp
  mv -f tmp namcouple
  echo "**** WRF  GRID${iDOMWRF} DIMENSIONS = (${NBXchild},${NBYchild}) ; WRF  GRID${iDOMWRF} TIME STEP = ${NBTWRFchild}" 

done

for iDOMNEM in $(seq 2 ${MAX_DOM_NEMO})
do

  iZOOM=`expr $iDOMNEM - 1`
  #- find lines to read in AGRIF_FixedGrids.in 
  for iLIN in 2 5 8 11 14
  do
    iLINm1=`expr $iLIN - 1`
    iLINp1=`expr $iLIN + 1`
    iZOOM_TEST=`cat AGRIF_FixedGrids.in | head -${iLINm1} |tail -1 | cut -d ' ' -f1`
    iGRD_PARENT=`cat AGRIF_FixedGrids.in | head -${iLINp1} |tail -1 | cut -d ' ' -f1`
    if [ ${iZOOM_TEST} == ${iZOOM} ]; then
      read IMIN IMAX JMIN JMAX PGRX PGRY PGRT << EOF
      `cat AGRIF_FixedGrids.in | head -${iLIN} |tail -1` 
EOF
      break
    fi
  done
  #-
  NBXchildOCE=`echo "( $IMAX - $IMIN + 1 ) * $PGRX + 1" | bc`
  NBYchildOCE=`echo "( $JMAX - $JMIN + 1 ) * $PGRY + 1" | bc`
  NBTNEMchild=`grep "rn_rdt " ${iZOOM}_namelist_nemo_GENERIC_${CONFPAR} |cut -d '=' -f2 | cut -d '!' -f1 | sed -e "s/\.//g ; s/ //g"`
  CPLFREQchild=`echo "${NBTNEMchild} * 2" | bc`
  sed -e "s/nbxNEMd0${iDOMNEM}/${NBXchildOCE}/g ; s/nbyNEMd0${iDOMNEM}/${NBYchildOCE}/g ; s/NBTNEMd0${iDOMNEM}/${NBTNEMchild}/g ; s/cplfreqd0${iDOMNEM}/${CPLFREQd01}/g" namcouple > tmp
  mv -f tmp namcouple
  echo "**** NEMO GRID${iDOMNEM} DIMENSIONS = (${NBXchildOCE},${NBYchildOCE}) ; NEMO GRID${iDOMNEM} TIME STEP = ${NBTNEMchild}"

done

echo " "

############################################################
##-- create a few links :

rm -f wrf.exe CAMtr_volume_mixing_ratio
ln -s ${WRFdir}/bin/wrf.coupled.CO2vary.hourly_daily.raijin.dmpar.exe  wrf.exe
ln -s ${WRFdir}/test/em_real/CAMtr_volume_mixing_ratio.RCP8.5  CAMtr_volume_mixing_ratio

rm -f *.TBL *_DATA
for file in ${WRFdir}/run/*.TBL
do
  ln -s $file
done
for file in ${WRFdir}/run/*_DATA
do
  ln -s $file
done

rm -f nemo.exe
if [ ${NZOOM_NEMO} -gt 0 ]; then
  ln -s ${NEMOdir}/CONFIG/${CONFPAR}_agrifcpl/BLD/bin/nemo.exe nemo.exe
else
  ln -s ${NEMOdir}/CONFIG/${CONFPAR}_cpl/BLD/bin/nemo.exe nemo.exe
fi

rm -f xios_server.exe
ln -s ../../models/xios_cpl/bin/xios_server.exe xios_server.exe

############################################################
##-- prepare next batch scripts

STOCKDIR2=`echo $STOCKDIR |sed -e "s/\//\\\\\\\\\//g"`
WORKDIR2=`echo $WORKDIR  |sed -e "s/\//\\\\\\\\\//g"`

sed -e "s/YYYY/${YEAR}/g ; s/SSSS/${STOCKDIR2}/g ; s/PPPP/${PROJ}/g ; s/WWWW/${WORKDIR2}/g ; s/CCCC/${CONFIG}/g ; s/cccc/${CONFPAR}/g ; s/OOOO/${CASE}/g ; s/DDDD/${DAY}/g ; s/MMMM/${MONTH}/g ; s/yyyy/${YEARf}/g ; s/mmmm/${MONTHf}/g ; s/dddd/${DAYf}/g ; s/NNNN/${NRUN}/g ; s/GGGG/${MAX_DOM_WRF}/g ; s/ZZZZ/${NZOOM_NEMO}/g ; s/UUUU/${NITEND}/g" export_now_GENERIC.sh > export_now_${NRUN}.sh

sed -e "s/YYYY/${YEAR}/g ; s/PPPP/${PROJ}/g ; s/SSSS/${STOCKDIR2}/g ; s/WWWW/${WORKDIR2}/g ; s/CCCC/${CONFIG}/g ; s/cccc/${CONFPAR}/g ; s/OOOO/${CASE}/g ; s/NNNN/${NRUN}/g ; s/DDDD/${DAY}/g ; s/MMMM/${MONTH}/g ; s/UUUU/${NITEND}/g ; s/VVVV/${NITENDM1}/g ; s/DDAAYYFF/${DAYf}/g ; s/MMOONNTTHHFF/${MONTHf}/g ; s/YYEEAARRFF/${YEARf}/g ; s/ZZZZ/${NZOOM_NEMO}/g" run_now_GENERIC.sh > run_now_${NRUN}.sh

sed -e "s/NNNN/${NRUN}/g ; s/YYYY/${YEAR}/g ; s/PPPP/${PROJ}/g ; s/WWWW/${WORKDIR2}/g ; s/CCCC/${CONFIG}/g ; s/cccc/${CONFPAR}/g ; s/OOOO/${CASE}/g ; s/DDDD/${DAY}/g ; s/MMMM/${MONTH}/g ; s/GGGG/${MAX_DOM_WRF}/g ; s/VVVV/${NITENDM1}/g ; s/TTTT/${NITENDM1ZOOM}/g ; s/ZZZZ/${NZOOM_NEMO}/g ; s/SSSS/${STOCKDIR2}/g" import_now_GENERIC.sh > import_now_${NRUN}.sh

sed -e "s/PPPP/${PROJ}/g ; s/WWWW/${WORKDIR2}/g ; s/NNNN/${NRUN}/g ; s/ZZZZ/${NZOOM_NEMO}/g ; s/GGGG/${MAX_DOM_WRF}/g ; s/cccc/${CONFPAR}/g ; s/OOOO/${CASE}/g" compress_now_GENERIC.sh > compress_now_${NRUN}.sh

chmod +x export_now_${NRUN}.sh run_now_${NRUN}.sh compress_now_${NRUN}.sh import_now_${NRUN}.sh

##############################################################
##-- import files (import.sh will launch the long simulation)

qsub ./import_now_${NRUN}.sh
