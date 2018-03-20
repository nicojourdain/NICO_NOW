#!/bin/bash
#PBS -P PPPP
#PBS -q normal
#PBS -l wd
#PBS -l walltime=40:00:00,mem=25Gb

module load netcdf/4.2.1.1

NC_INC='-I /apps/netcdf/4.2.1.1/include'
NC_LIB='-L /apps/netcdf/4.2.1.1/lib -lnetcdf -lnetcdff'

WORKDIR='WWWW'
CONFIG='CCCC'
CASE='OOOO'
YEAR='YYYY'
DOMAIN=XXXX

cd $WORKDIR

if [ ! -f postprowrf ]; then
  ifort -c $NC_INC postprocess_wrf_pLev.f90
  ifort -o postprowrf postprocess_wrf_pLev.f90 $NC_LIB
fi

DOMAIN="0${DOMAIN}"

if [ -d WORK_${YEAR} ]; then

  chmod -R a+wrx WORK_${YEAR}
  chown -R ncj561:e14 WORK_${YEAR}

  echo "******************************************"
  echo "*   Extract Monthly means                 "
  echo "******************************************"

  rm -f tmp.nc

  for MONTH in 01 02 03 04 05 06 07 08 09 10 11 12
  do

    ######################################################################################
    #-- monthly mean of diurnal cycle for many 2D variables (3-hour temporal resolution):
    ######################################################################################
    ncea      WORK_${YEAR}/wrfout_d${DOMAIN}_${YEAR}-${MONTH}-*_00:00:00                 wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc
    ncap2 -O -s "Time=XTIME"              wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc
    ncatted -O -a ,Time,d,, -a units,Time,c,c,"minutes since 1989-01-01 00:00 UTC" -a "long_name",Time,c,c,"Time" -a "calendar",Time,c,c,"Gregorian"  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc             
    ncks -O -F -d soil_layers_stag,1      wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc
    ncks -v Time,SWDOWN,GLW,OLR           wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diur_radflx.nc
    ncks -v Time,LH,HFX,QFX               wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diur_heatflx.nc
    ncks -v Time,PSFC,TSLB,SMOIS          wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diur_PTS.nc
    ncks -v Time,PBLH,UST                 wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diur_turb.nc
    ######################################################################################
    #-- monthly mean of diurnal cycle for many 2D variables (30-min temporal resolution):
    ######################################################################################
    TIMESTART=`ncdump -v Time wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diur_turb.nc | tail -5 | grep "Time =" | sed -e "s/\,//g" | awk '{print $3}'`
    ncea      WORK_${YEAR}/wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}-*_00:00:00    wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc
    mtt=`ncdump -h wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc |grep UNLIMITED | sed -e "s/(//g" | awk '{print $6}'`
    ncap2 -O -F -s "Time=SST(:,1,1); idt=1 ; while(idt<=$mtt){Time(idt) = 30.0*(idt-1) + $TIMESTART; idt++;}"   wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc
    ncatted -O -a units,Time,c,c,"minutes since 1989-01-01 00:00 UTC" -a "long_name",Time,c,c,"Time" -a "calendar",Time,c,c,"Gregorian"  wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc
    ncks -v Time,U10,V10                 wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diur_UV10.nc
    ncks -v Time,PREC_ACC_C,PREC_ACC_NC  wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diur_rain.nc
    ncks -v Time,T2,Q2                   wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diur_TQ2.nc
    ncks -v Time,SST                     wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diur_SST.nc

    for NAM in radflx heatflx PTS turb UV10 rain TQ2 SST
    do
      if [ ! -f wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diur_${NAM}.nc ]; then
        echo "!@#$%^&* >>>>> STOP ! wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diur_${NAM}.nc does not exist"
        exit
      fi
    done

    ################################
    #-- usual monthly mean
    ################################
    ncra  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_monthly.nc
    ncks -v Time,SWDOWN,GLW,OLR           wrfout_d${DOMAIN}_${YEAR}-${MONTH}_monthly.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_mon_radflx.nc
    ncks -v Time,LH,HFX,QFX               wrfout_d${DOMAIN}_${YEAR}-${MONTH}_monthly.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_mon_heatflx.nc
    ncks -v Time,PSFC,TSLB,SMOIS          wrfout_d${DOMAIN}_${YEAR}-${MONTH}_monthly.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_mon_PTS.nc
    ncks -v Time,PBLH,UST                 wrfout_d${DOMAIN}_${YEAR}-${MONTH}_monthly.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_mon_turb.nc
    ncks -v Time,U10,V10                  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_monthly.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_mon_UV10.nc
    ncks -v Time,T2,Q2                    wrfout_d${DOMAIN}_${YEAR}-${MONTH}_monthly.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_mon_TQ2.nc
    ncks -v Time,SST                      wrfout_d${DOMAIN}_${YEAR}-${MONTH}_monthly.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_mon_SST.nc
    # Take rainfall rates from wrfhrly (PREC_ACC_* only saved in these files) :
    ncra  wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_monthly.nc
    ncks -v Time,PREC_ACC_C,PREC_ACC_NC   wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_monthly.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_mon_rain.nc

    for NAM in radflx heatflx PTS turb UV10 rain TQ2 SST
    do
      if [ ! -f wrfout_d${DOMAIN}_${YEAR}-${MONTH}_mon_${NAM}.nc ]; then
        echo "!@#$%^&* >>>>> STOP ! wrfout_d${DOMAIN}_${YEAR}-${MONTH}_mon_${NAM}.nc does not exist"
        exit
      fi
    done
    rm -f wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc  ## Too big to be stored
    rm -f wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_monthly.nc  wrfhrly_d${DOMAIN}_${YEAR}-${MONTH}_diurnal.nc

    #####################################################
    #-- extract 3-hourly fields on a few pressure levels
    #####################################################
    for DAY in $(seq 1 31)
    do
      if [ $DAY -lt 10 ]; then
        DAY=`echo "0$DAY"`
      fi
      if [ -f WORK_${YEAR}/wrfout_d${DOMAIN}_${YEAR}-${MONTH}-${DAY}_00:00:00 ]; then
        echo "${DOMAIN} ${YEAR} ${MONTH} ${DAY}" > date.tmp
        ./postprowrf
      fi
    done

    ###########################################################
    #-- extract 3-hourly fields of U10,V10, PSFC (for tracker)
    ###########################################################
    for DAY in $(seq 1 31)
    do
      if [ $DAY -lt 10 ]; then
        DAY=`echo "0$DAY"`
      fi
      if [ -f WORK_${YEAR}/wrfout_d${DOMAIN}_${YEAR}-${MONTH}-${DAY}_00:00:00 ]; then
        ncks -F -v U10,V10,PSFC,GLW,SWDOWN,T2,Q2,SST,LANDMASK WORK_${YEAR}/wrfout_d${DOMAIN}_${YEAR}-${MONTH}-${DAY}_00:00:00 WORK_${YEAR}/wrfout_d${DOMAIN}_${YEAR}-${MONTH}-${DAY}_10m.nc
      fi
    done

    ####################################################################################
    #-- monthly means of fields on pressure levels defined in postprocess_wrf_pLev.f90
    ####################################################################################
    for PLEV in `grep "pLev=" postprocess_wrf_pLev.f90 | sed -e "s/pLev=(\///g ; s/\/)//g ; s/\,/ /g ; s/\.0//g"`
    do
      ncea  WORK_${YEAR}/wrfout_d${DOMAIN}_${YEAR}-${MONTH}-*_${PLEV}.nc  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diur_${PLEV}.nc
      ncra  wrfout_d${DOMAIN}_${YEAR}-${MONTH}_diur_${PLEV}.nc            wrfout_d${DOMAIN}_${YEAR}-${MONTH}_mon_${PLEV}.nc
    done

    ##-- CHECK --
    if [ ! -f wrfout_d${DOMAIN}_${YEAR}-${MONTH}_monthly.nc ]; then
      echo "!@#$%^&* PROBLEM: wrfout_d${DOMAIN}_${YEAR}-${MONTH}_monthly.nc has not been created !!"
      echo "   >>>>>>>>> STOP"
      exit  
    fi

    chown ncj561:e14 *.nc
    chmod a+rx *.nc

  done   ##-- for MONTH

  ####################################################
  ##- Concatenate yearly file of monthly 2D values:
  ####################################################
  for NAM in radflx heatflx PTS turb UV10 rain TQ2 SST 
  do
    ncrcat  wrfout_d${DOMAIN}_${YEAR}-*_mon_${NAM}.nc  wrfout_d${DOMAIN}_${YEAR}_mon_${NAM}.nc
    if [ -f wrfout_d${DOMAIN}_${YEAR}_mon_${NAM}.nc ]; then
      rm -f wrfout_d${DOMAIN}_${YEAR}-*_mon_${NAM}.nc
    else
      echo "!@#$%^&* ERROR: concatenation of wrfout_d${DOMAIN}_${YEAR}_mon_${NAM}.nc failed >> stop"
      exit
    fi
  done

  for PLEV in `grep "pLev=" postprocess_wrf_pLev.f90 | sed -e "s/pLev=(\///g ; s/\/)//g ; s/\,/ /g ; s/\.0//g"`
  do
    ncrcat  wrfout_d${DOMAIN}_${YEAR}-*_mon_${PLEV}.nc  wrfout_d${DOMAIN}_${YEAR}_mon_${PLEV}.nc
    if [ -f wrfout_d${DOMAIN}_${YEAR}_mon_${PLEV}.nc ]; then
      rm -f wrfout_d${DOMAIN}_${YEAR}-*_mon_${PLEV}.nc
    else
      echo "!@#$%^&* ERROR: concatenation of wrfout_d${DOMAIN}_${YEAR}_mon_${PLEV}.nc failed >> stop"
      exit
    fi
  done

  ######################################################
  ##- Concatenate yearly file of full monthly fields:
  ######################################################
  ncrcat wrfout_d${DOMAIN}_${YEAR}-*_monthly.nc wrfout_d${DOMAIN}_${YEAR}_monthly.nc
  if [ -f wrfout_d${DOMAIN}_${YEAR}_monthly.nc ]; then
    rm -f wrfout_d${DOMAIN}_${YEAR}-*_monthly.nc
    nccopy -d 9 wrfout_d${DOMAIN}_${YEAR}_monthly.nc troll.nc
    mv -f troll.nc wrfout_d${DOMAIN}_${YEAR}_monthly.nc
  else
    echo "!@#$%^&* ERROR: concatenation of wrfout_d${DOMAIN}_${YEAR}_monthly.nc failed >> stop"
    exit
  fi

  ########################################################################################
  ##- Concatenate yearly files of 3-hourly pLev and 10m fields (usefull for TC tracking):
  ########################################################################################
  for PLEV in `grep "pLev=" postprocess_wrf_pLev.f90 | sed -e "s/pLev=(\///g ; s/\/)//g ; s/\,/ /g ; s/\.0//g"`  '10m'
  do
    ncrcat WORK_${YEAR}/wrfout_d${DOMAIN}_${YEAR}-*_${PLEV}.nc  wrfout_d${DOMAIN}_${YEAR}_3HOURLY_${PLEV}.nc
    if [ -f wrfout_d${DOMAIN}_${YEAR}_3HOURLY_${PLEV}.nc ]; then
      rm -f WORK_${YEAR}/wrfout_d${DOMAIN}_${YEAR}-*_${PLEV}.nc
      nccopy -d 9 wrfout_d${DOMAIN}_${YEAR}_3HOURLY_${PLEV}.nc troll.nc
      mv -f troll.nc wrfout_d${DOMAIN}_${YEAR}_3HOURLY_${PLEV}.nc
    else
      echo "!@#$%^&* ERROR: concatenation of wrfout_d${DOMAIN}_${YEAR}_3HOURLY_${PLEV}.nc failed !! >>>> stop"
      exit
    fi
  done

  rm -f TMP/*

  rm -f WORK_${YEAR}/wrfhrly_d${DOMAIN}_${YEAR}*

else   ##-- if [ -d WORK_${YEAR} ]; then

  echo "******************************************"
  echo "*    No files to concatenate  !!!!!       "
  echo "******************************************"

  exit
  
fi     ##-- if [ -d WORK_${YEAR} ]; then

#######################################################
##-- Compress 3-hourly to daily files before exporting

qsub compress_d${DOMAIN}.sh  
