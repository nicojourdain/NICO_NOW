#!/bin/bash
#PBS -P e14
#PBS -q express
#PBS -l wd
#PBS -l walltime=00:01:00

################################################################################################
# N. JOURDAIN, UNSW-CCRC, FEB. 2013                                                            #
#                                                                                              #
# This script is the user's interface for WRF's post-processing loops.                         #
#                                                                                              # 
# It takes the user's choices and propagates them to modify all the generic files *_POST.sh :  #
#                                                                                              #
# - import_POST.sh   -> import_d01.sh   (+ import_d02.sh   and other child-grids if required)  #
# - concat_POST.sh   -> concat_d01.sh   (+ concat_d02.sh   and other child-grids if required)  #
# - compress_POST.sh -> compress_d01.sh (+ compress_d02.sh and other child-grids if required)  #
# - export_POST.sh   -> export_d01.sh   (+ export_d02.sh   and other child-grids if required)  #
#                                                                                              #
# >> import*sh get files from massdata (edit if you want to get files from another location)   #
# >> concat*sh calculate fields on pressure levels, diurnal, monthly files, etc                #
# >> compress*sh transform WRF's raw 3-hourly outputs in daily outputs and compress them       #
# >> export*sh put all the post-processed files  onto the massdata.                            #
#                                                                                              #
#                                                                                              #
# NB: everything is made for the case of daily output files containing 8 time steps (i.e. 3-h) #
#                                                                                              #
################################################################################################

##############################################
##-- User's choices:

PROJ='e14'           #- must be the same as PBS -P header

CONFIG='cordex24'    #- name of the domain set-up

CASE='BMJv2_BILAP'   #- specific name of this simulation

WORKDIR=`pwd`        #- working directory for the post-processing

STOCKDIR="NICO/now"  #- output directory on massdata used for both
                     #  WRF's raw outputs and for post-processed files

MAX_DOM=1            #- number of grids
                     #  =1 to postprocess d01 only
                     #  =2 to postprocess both d01 and d02
                     #  =3 to postprocess d01, d02, and d03, etc...

MAXYEAR=2009         #- year after which the automatic re-submission stops

##############################################
##-- initializations

if [ -f post_${CONFIG}_${CASE}.db ]; then
  YEAR=`tail -1 post_${CONFIG}_${CASE}.db | sed -e "s/ //g"`
else
  YEAR=1989
  echo "1989" > post_${CONFIG}_${CASE}.db
fi

if [ $YEAR -gt $MAXYEAR ]; then
  echo "!@#$% YOU SPECIFIED MAXIMUM YEAR AS $MAXYEAR !!! >>>>>> stop"
  exit
fi

export PROJECT="$PROJ"

cd $WORKDIR

##############################################

echo "******************************************"
echo "*   project $PROJ                         "
echo "*   config  $CONFIG                       "
echo "*   case    $CASE                         "
echo "*   year    $YEAR                         "
echo "*   number of domains ${MAX_DOM}          "
echo "******************************************"

############################################################
##-- prepare next batch scripts

STOCKDIR2=`echo $STOCKDIR |sed -e "s/\//\\\\\\\\\//g"`
WORKDIR2=`echo $WORKDIR  |sed -e "s/\//\\\\\\\\\//g"`

DOM=1
while [ $DOM -le ${MAX_DOM} ];
do

sed -e "s/YYYY/${YEAR}/g ; s/SSSS/${STOCKDIR2}/g ; s/PPPP/${PROJ}/g ; s/WWWW/${WORKDIR2}/g ; s/CCCC/${CONFIG}/g ; s/OOOO/${CASE}/g ; s/XXXX/${DOM}/g ; s/ZZZZ/${MAX_DOM}/g"  export_POST.sh > export_d0${DOM}.sh

sed -e "s/YYYY/${YEAR}/g ; s/SSSS/${STOCKDIR2}/g ; s/PPPP/${PROJ}/g ; s/WWWW/${WORKDIR2}/g ; s/CCCC/${CONFIG}/g ; s/OOOO/${CASE}/g ; s/XXXX/${DOM}/g"  import_POST.sh > import_d0${DOM}.sh

sed -e "s/YYYY/${YEAR}/g ; s/SSSS/${STOCKDIR2}/g ; s/PPPP/${PROJ}/g ; s/WWWW/${WORKDIR2}/g ; s/CCCC/${CONFIG}/g ; s/OOOO/${CASE}/g ; s/XXXX/${DOM}/g"  concat_POST.sh > concat_d0${DOM}.sh

sed -e "s/YYYY/${YEAR}/g ; s/SSSS/${STOCKDIR2}/g ; s/PPPP/${PROJ}/g ; s/WWWW/${WORKDIR2}/g ; s/CCCC/${CONFIG}/g ; s/OOOO/${CASE}/g ; s/XXXX/${DOM}/g"  compress_POST.sh > compress_d0${DOM}.sh

chmod +x export_d0*.sh concat_d0*.sh compress_d0*.sh import_d0*.sh

DOM=`expr $DOM + 1`

done

##############################################################
##-- import files (import.sh will launch the long simulation)

qsub ./import_d01.sh
