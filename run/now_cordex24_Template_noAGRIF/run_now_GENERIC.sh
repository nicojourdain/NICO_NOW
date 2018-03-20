#!/bin/bash
#PBS -P PPPP
#PBS -q normal
#PBS -l wd
#PBS -l ncpus=112,other=iobound,mem=400Gb,walltime=47:53:00

## NB: change nb of proc in mpirun if change ncpus

PROJ='PPPP'
WORKDIR='WWWW'
STOCKDIR='SSSS'
CONFIG='CCCC'
CONFPAR='cccc'
CASE='OOOO'
YEAR='YYYY'
MONTH='MMMM'
DAY='DDDD'
NRUN=NNNN
YEARf=YYEEAARRFF
MONTHf=MMOONNTTHHFF
DAYf=DDAAYYFF
NITEND=UUUU
NITENDM1=VVVV
NZOOM_NEMO=ZZZZ

export PROJECT="$PROJ"

MAX_DOM_WRF=`cat namelist_wrf_GENERIC_${CONFIG} | grep max_dom | sed -e "s/\,//g" |cut -d '=' -f2 | sed -e "s/ //g"`
MAX_DOM_NEMO=`expr ${NZOOM_NEMO} + 1`

cd $WORKDIR

echo "******************************************"
echo "* NOW (NEMO-OASIS-WRF) SIMULATION         " 
echo "*   project $PROJ                         "
echo "*   config  $CONFIG                       "
echo "*   case    $CASE                         "
echo "*   year    $YEAR                         "
echo "******************************************"

##########################################################
##-- run

echo "-np 80 -bind-to-none ./wrf.exe"         > app.conf
echo "-np 28 -bind-to-none ./nemo.exe"        >> app.conf
echo "-np  4 -bind-to-none ./xios_server.exe" >> app.conf

#time mpirun --app app.conf

mpirun -bycore -np 1 ./xios_server.exe : -np 15 ./wrf.exe : -np 1 ./xios_server.exe : -np 15 ./wrf.exe : -np 1 ./xios_server.exe : -np 15 ./wrf.exe : -np 1 ./xios_server.exe : -np 15 ./wrf.exe : -np 20 ./wrf.exe : -np 28 ./nemo.exe

##########################################################
##-- compress output files:

if [ ! -d OUTPUT_${NRUN} ]; then
  mkdir OUTPUT_${NRUN}
fi
mv -f wrfout_d0*                        OUTPUT_${NRUN}/.
mv -f wrfhrly_d0*                       OUTPUT_${NRUN}/.
mv -f ${CONFPAR}-${CASE}_[1-6][dh]_*nc  OUTPUT_${NRUN}/.

for iZOOM in $(seq 1 ${NZOOM_NEMO})
do
  mv -f ${iZOOM}_${CONFPAR}-${CASE}_[1-6][dh]_*nc  OUTPUT_${NRUN}/.
done

grep xios app.conf | sed -e "s/\.\///g" > OUTPUT_${NRUN}/app.copy

qsub compress_now_${NRUN}.sh

##########################################################
##-- prepare next run if every little thing went all right

NTEST_O=`ls -1 OUTPUT_${NRUN}/wrfout* |wc -l`
NTEST_H=`ls -1 OUTPUT_${NRUN}/wrfhrly* |wc -l`
NTEST_R=`ls -1 wrfrst* |wc -l`
NTESQ_O=`ls -1 OUTPUT_${NRUN}/${CONFPAR}-${CASE}_[1-6][dh]_*nc |wc -l`
NTESQ_R=`ls -1 ${CONFPAR}-${CASE}_*_restart_*.nc |wc -l`

if [ ${NTEST_O} -gt 0 ] && [ ${NTEST_H} -gt 0 ] && [ ${NTEST_R} -gt 0 ] && [ ${NTESQ_O} -gt 0 ] && [ ${NTESQ_R} -gt 0 ]; then

  ##############################
  ## last OASIS restart
  for iDOMNEM in $(seq 1 ${MAX_DOM_NEMO})
  do
  for iDOMWRF in $(seq 1 ${MAX_DOM_WRF})
  do
    if [ -f sO${iDOMNEM}A${iDOMWRF}.nc ]; then
      mv -f sO${iDOMNEM}A${iDOMWRF}.nc sstoc_Od0${iDOMNEM}_to_Ad0${iDOMWRF}_${NRUN}.nc
    fi
    if [ -f fA${iDOMWRF}O${iDOMNEM}.nc ]; then
      mv -f fA${iDOMWRF}O${iDOMNEM}.nc flxat_Ad0${iDOMWRF}_to_Od0${iDOMNEM}_${NRUN}.nc
    fi
  done
  done 
   
  ##############################
  ## last WRF restart date:
  LAST_RESTART=`ls -lrt wrfrst_d01* |tail -1 | sed -e "s/  / /g" | cut -d ' ' -f9`
  DATE_RST=`echo $LAST_RESTART | cut -d '_' -f3`
  YEAR_RST=`echo $DATE_RST     | cut -d '-' -f1`
  MONTH_RST=`echo $DATE_RST    | cut -d '-' -f2`
  DAY_RST=`echo $DATE_RST      | cut -d '-' -f3`
  echo " "
  echo "Last restart created for ${DAY_RST}/${MONTH_RST}/${YEAR_RST}"
  echo "  ---> writting this date in prod_wrf.db"
  echo " "

  ##############################
  ## last NEMO restart date:
  ##-- write last restart time step of mother grid in prod_nemo.db:
  LAST_RESTART_NIT=`ls -lrt ${CONFPAR}-${CASE}_*_restart_*.nc |tail -1 | sed -e "s/${CONFPAR}-${CASE}//g" | cut -d '_' -f2`
  echo " "
  echo "Last restart created at parent ocean time step ${LAST_RESTART_NIT}"
  echo "  ---> writting this date in prod_nemo.db"
  echo " "
  echo "$LAST_RESTART_NIT" > restart_nit.txt
  ##-- add last restart time step on chidren grids (at the end of last line in prod_nemo.db):
  for iZOOM in $(seq 1 ${NZOOM_NEMO})
  do
    LAST_RESTART_NIT_ZOOM=`ls -lrt ${iZOOM}_${CONFPAR}-${CASE}_*_restart_*.nc |tail -1 | sed -e "s/${iZOOM}_${CONFPAR}-${CASE}//g" | cut -d '_' -f2`
    sed -e "`wc -l prod_nemo.db|cut -d ' ' -f1`s/$/ ${LAST_RESTART_NIT_ZOOM}/g" prod_nemo.db > tmp
    mv tmp prod_nemo.db
    echo " "
    echo "Last restart created for zoom nb $iZOOM at ocean time step ${LAST_RESTART_NIT_ZOOM}"
    echo "  ---> writting this date in prod_nemo.db"
    echo " "
    echo "$LAST_RESTART_NIT_ZOOM" > ${iZOOM}_restart_nit.txt
  done

  ##############################
  ## rebuild restart file for mother grid (group mpp subdomains):
  ./rebuild_restart
  if [ -f restart_${LAST_RESTART_NIT}.nc ]; then
    ##rm -f ${CONFPAR}-${CASE}_${LAST_RESTART_NIT}_restart_*.nc
    mv -f restart.obc.output restart_${LAST_RESTART_NIT}.obc
  else
    echo "!@#$%^* restart_${LAST_RESTART_NIT}.nc has not been created by rebuild_restart.f90"
    echo "                                                         >>>>>>>>>>>>>>>>>>>> stop"
    exit
  fi

  ## rebuild restart file for children grids :
  for iZOOM in $(seq 1 ${NZOOM_NEMO})
  do
     ./${iZOOM}_rebuild_restart
     LAST_RESTART_NIT_ZOOM=`cat ${iZOOM}_restart_nit.txt`
     if [ -f ${iZOOM}_restart_${LAST_RESTART_NIT_ZOOM}.nc ]; then
       rm -f ${iZOOM}_${CONFPAR}-${CASE}_${LAST_RESTART_NIT_ZOOM}_restart_*.nc
     else
       echo "!@#$%^* ${iZOOM}_restart_${LAST_RESTART_NIT_ZOOM}.nc has not been created by ${iZOOM}_rebuild_restart.f90"
       echo "                                                                                >>>>>>>>>>>>>>>>>>>> stop"
       exit
     fi
  done

  ##########
  ## prepare initial state for following iteration:
  sed -e "s/${YEAR} ${MONTH} ${DAY}/${YEAR} ${MONTH} ${DAY} ok/g"                         prod_wrf.db  > tmp
  mv -f tmp prod_wrf.db
  NRUNm1=$NRUN
  NRUNm2=`expr $NRUN - 1`
  NRUN=`expr $NRUN + 1`
  echo "${NRUN} ${YEAR_RST} ${MONTH_RST} ${DAY_RST}"         >> prod_wrf.db
  TMPTMP="${LAST_RESTART_NIT}"
  for iZOOM in $(seq 1 ${NZOOM_NEMO})
  do
    LAST_RESTART_NIT_ZOOM=`cat ${iZOOM}_restart_nit.txt`
    TMPTMP="${TMPTMP} ${LAST_RESTART_NIT_ZOOM}"
  done
  echo "${NRUN} ${YEARf} ${MONTHf} ${DAYf} ${TMPTMP}"        >> prod_nemo.db

  ##########
  ## clean WORKDIR:
  if [ ${YEAR_RST} == ${YEAR} ]; then
    echo "keeping forcing files in working directory"
  else
    rm -f wrflowinp_d0[1-3]*
    rm -f wrfinput_d0[1-3]*
    rm -f wrfbdy_d0[1-3]*
    for OBC in north south east west
    do
      rm -f OBC_${OBC}_${CONFPAR}_y${YEARm2}.nc
      rm -f obc_${OBC}_y${YEARm2}.nc
    done
  fi
  rm -f TMP/*
  rm -f import_now_${NRUNm2}.sh
  rm -f run_now_${NRUNm2}.sh
  rm -f import_now_${NRUNm2}.sh.[oe][0-9]*
  rm -f export_now_${NRUNm2}.sh.[oe][0-9]*
  rm -f compress_now_${NRUNm2}.[oe][0-9]*
  rm -f debug.0[1-3].[0-9][0-9][0-9]*
  #rm -f import_now_${NRUNm1}.sh.[oe][0-9]*
  #rm -f export_now_${NRUNm1}.sh.[oe][0-9]*
  #rm -f compress_now_${NRUNm1}.[oe][0-9]*
  #rm -f production_now.sh.[oe][0-9]*

else

  echo ' '
  echo '!@#$%^&* BIG PROBLEM : no wrfout* or wrfrst* or no NEMO output or NEMO restart files created !! >>>>>>> STOP'
  exit

fi

##########################################################
##-- send outputs and restarts to storage disk

mv -f namelist.output namelist.output.${NRUNm1}
mv -f namelist.input  namelist.input.${NRUNm1}
mv -f rsl.error.0000  rsl.error_${NRUNm1}
mv -f rsl.out.0000    rsl.out_${NRUNm1}
mv -f namelist        namelist.${NRUNm1}
mv -f ocean.output    ocean.output.${NRUNm1}
mv -f namcouple       namcouple.${NRUNm1}
for iZOOM in $(seq 1 ${NZOOM_NEMO})
do
  mv -f ${iZOOM}_namelist      ${iZOOM}_namelist.${NRUNm1}
  mv -f ${iZOOM}_ocean.output  ${iZOOM}_ocean.output.${NRUNm1}
done

##########################################################
##-- launch next year of the experiment

qsub ./production_now.sh 

