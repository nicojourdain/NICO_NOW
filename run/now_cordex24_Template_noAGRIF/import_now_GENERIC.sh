#!/bin/bash
#PBS -P PPPP
#PBS -q copyq
#PBS -l wd
#PBS -l other=mdss 

WORKDIR='WWWW'
STOCKDIR='SSSS'
CONFIG='CCCC'
CONFPAR='cccc'
CASE='OOOO'
YEAR='YYYY'
MONTH='MMMM'
DAY='DDDD'
MAX_DOM_WRF=GGGG
NZOOM_NEMO=ZZZZ  ## nb of NEMO_AGRIF nests
NRUN=NNNN
NITENDM1=VVVV
NITENDM1ZOOM=TTTT

MAX_DOM_WRF=`cat namelist_wrf_GENERIC_${CONFIG} | grep max_dom | sed -e "s/\,//g" |cut -d '=' -f2 | sed -e "s/ //g"`
MAX_DOM_NEMO=`expr ${NZOOM_NEMO} + 1`

######################################################################
## NB: YOU CAN USE A PROJECT DIRECTORY ON MASSDATA DIFFERENT FROM PPPP
## (e.g. PROJMDSS='$PROJMDSS' Otherwise, just leave PROJMDSS=PPPP below)
#PROJMDSS='PPPP'
PROJMDSS='e14'

echo " "
echo " Copy input files from ${STOCKDIR}/input/nemo_${CONFIG} and ${STOCKDIR}/input/wrf_${CONFIG}"

echo " "
if [ ! -d TMP ]; then
  mkdir TMP
fi

DATE0=`grep nn_date0 namelist | head -1 | awk '{print $3}'`
Y0=`echo $DATE0 | cut -c 1-4`
M0=`echo $DATE0 | cut -c 5-6`

YEARm1=`expr $YEAR - 1`
# because no data before:
if [ $YEAR -eq $Y0 ]; then
  YEARm1=$Y0
fi
YEARp1=`expr $YEAR + 1`

######################################################################
##-- import OASIS' weight/interpolation

for iDOMNEM in $(seq 1 ${MAX_DOM_NEMO})
do

for iDOMWRF in $(seq 1 ${MAX_DOM_WRF})
do

  rm -f mozaic_Ad0${iDOMWRF}_to_Od0${iDOMNEM}.nc mozaic_Od0${iDOMNEM}_to_Ad0${iDOMWRF}.nc
  
  if [ ! -f mozaic_Ad0${iDOMWRF}_to_Od0${iDOMNEM}_${CONFIG}.nc ]; then
    mdss -P $PROJMDSS get ${STOCKDIR}/input/oa3mct_${CONFIG}/mozaic_Ad0${iDOMWRF}_to_Od0${iDOMNEM}_${CONFIG}.nc
  fi
  
  if [ ! -f mozaic_Od0${iDOMNEM}_to_Ad0${iDOMWRF}_${CONFIG}.nc ]; then
    mdss -P $PROJMDSS get ${STOCKDIR}/input/oa3mct_${CONFIG}/mozaic_Od0${iDOMNEM}_to_Ad0${iDOMWRF}_${CONFIG}.nc
  fi

  if [ -f mozaic_Ad0${iDOMWRF}_to_Od0${iDOMNEM}_${CONFIG}.nc ]; then
    ln -s mozaic_Ad0${iDOMWRF}_to_Od0${iDOMNEM}_${CONFIG}.nc mozaic_Ad0${iDOMWRF}_to_Od0${iDOMNEM}.nc
  fi
  if [ -f mozaic_Od0${iDOMNEM}_to_Ad0${iDOMWRF}_${CONFIG}.nc ]; then
    ln -s mozaic_Od0${iDOMNEM}_to_Ad0${iDOMWRF}_${CONFIG}.nc mozaic_Od0${iDOMNEM}_to_Ad0${iDOMWRF}.nc
  fi

done

done

######################################################################
##-- import OASIS' restart or initial state

rm -f fA[1-9]O[1-9].nc  sO[1-9]A[1-9].nc

if [ $NRUN == 1 ]; then

  # manually built initial state :
  mdss -P $PROJMDSS get ${STOCKDIR}/input/oa3mct_${CONFIG}/flxat_Ad0?_to_Od0?_${CONFIG}.nc
  mdss -P $PROJMDSS get ${STOCKDIR}/input/oa3mct_${CONFIG}/sstoc_Od0?_to_Ad0?_${CONFIG}.nc
  
  for iDOMNEM in $(seq 1 ${MAX_DOM_NEMO})
  do

  for iDOMWRF in $(seq 1 ${MAX_DOM_WRF})
  do

    ## NB: no links here because these oasis files are overwritten later

    if [ -f flxat_Ad0${iDOMWRF}_to_Od0${iDOMNEM}_${CONFIG}.nc ]; then
      cp -p flxat_Ad0${iDOMWRF}_to_Od0${iDOMNEM}_${CONFIG}.nc fA${iDOMWRF}O${iDOMNEM}.nc
    else
      echo "NB: flxat_Ad0${iDOMWRF}_to_Od0${iDOMNEM}_${CONFIG}.nc does not exist (maybe not needed ?)"
    fi

    if [ -f sstoc_Od0${iDOMNEM}_to_Ad0${iDOMWRF}_${CONFIG}.nc ]; then
      cp -p sstoc_Od0${iDOMNEM}_to_Ad0${iDOMWRF}_${CONFIG}.nc sO${iDOMNEM}A${iDOMWRF}.nc
    else
      echo "NB: sstoc_Od0${iDOMNEM}_to_Ad0${iDOMWRF}_${CONFIG}.nc does not exist (maybe not needed?)"
    fi

  done

  done

else

  # restart from previous submission:
  NRUNm1=`expr $NRUN - 1`

  if [ ! -f flxat_Ad01_to_Od01_${NRUNm1}.nc ] || [ ! -f sstoc_Od01_to_Ad01_${NRUNm1}.nc]; then
    echo "no oasis restart files => bringing them from massdata..."
    mdss -P $PROJMDSS get ${STOCKDIR}/restart/now_${CONFIG}_${CASE}/oasis_${NRUNm1}.tar
    tar xvf oasis_${NRUNm1}.tar
  else
    echo "flxat_Ad01_to_Od01_${NRUNm1}.nc and sstoc_Od01_to_Ad01_${NRUNm1}.nc were already there"
    echo "and we assume that the equivalent files needed by yhe nests are here as well..."
  fi
  echo " "

  for iDOMNEM in $(seq 1 ${MAX_DOM_NEMO})
  do

  for iDOMWRF in $(seq 1 ${MAX_DOM_WRF})
  do

    ## NB: no links here because these oasis files are overwritten later

    if [ -f flxat_Ad0${iDOMWRF}_to_Od0${iDOMNEM}_${NRUNm1}.nc ]; then
      cp -p flxat_Ad0${iDOMWRF}_to_Od0${iDOMNEM}_${NRUNm1}.nc fA${iDOMWRF}O${iDOMNEM}.nc
    else
      echo "NB: flxat_Ad0${iDOMWRF}_to_Od0${iDOMNEM}_${NRUNm1}.nc does not exist (maybe not needed ?)"
    fi

    if [ -f sstoc_Od0${iDOMNEM}_to_Ad0${iDOMWRF}_${NRUNm1}.nc ]; then
      cp -p sstoc_Od0${iDOMNEM}_to_Ad0${iDOMWRF}_${NRUNm1}.nc sO${iDOMNEM}A${iDOMWRF}.nc
    else
      echo "NB: sstoc_Od0${iDOMNEM}_to_Ad0${iDOMWRF}_${NRUNm1}.nc does not exist (maybe not needed?)"
    fi

  done

  done

fi

######################################################################
##-- import WRF grids inputs if they are not already there

echo " Copy input files from ${STOCKDIR}/input/wrf_${CONFIG}"
for iGRID in $(seq 1 $MAX_DOM_WRF)
do
  if [ ! -f wrflowinp_d0${iGRID}_${YEAR}-01-01_${YEAR}-12-31 ]; then
   mdss -P $PROJMDSS get ${STOCKDIR}/input/wrf_${CONFIG}/wrflowinp_d0${iGRID}_${YEAR}-01-01_${YEAR}-12-31
  else
   echo "wrflowinp_d0${iGRID}_${YEAR}-01-01_${YEAR}-12-31 was already there"
  fi
  #--
  if [ ! -f wrfinput_d0${iGRID}_${YEAR}-01-01_${YEAR}-12-31 ]; then
   mdss -P $PROJMDSS get ${STOCKDIR}/input/wrf_${CONFIG}/CPL/wrfinput_d0${iGRID}_${YEAR}-01-01_${YEAR}-12-31
  else
   echo "wrfinput_d0${iGRID}_${YEAR}-01-01_${YEAR}-12-31 was already there"
  fi
done
if [ ! -f wrfbdy_d01_${YEAR}-01-01_${YEAR}-12-31 ]; then
 mdss -P $PROJMDSS get ${STOCKDIR}/input/wrf_${CONFIG}/wrfbdy_d01_${YEAR}-01-01_${YEAR}-12-31
else
 echo "wrfbdy_d01_${YEAR}-01-01_${YEAR}-12-31 was already there"
fi

##-- remove links and recreate them:

rm -f wrflowinp_d0[1-3] wrfinput_d0[1-3] wrfbdy_d0[1-3]
ln -s wrfbdy_d01_${YEAR}-01-01_${YEAR}-12-31     wrfbdy_d01
for iGRID in $(seq 1 $MAX_DOM_WRF)
do
 ln -s wrflowinp_d0${iGRID}_${YEAR}-01-01_${YEAR}-12-31  wrflowinp_d0${iGRID}
 ln -s wrfinput_d0${iGRID}_${YEAR}-01-01_${YEAR}-12-31   wrfinput_d0${iGRID}
done
echo " "

########################################################################
##-- import NEMO input that are not time-dependent if not already there :

rm -f K1rowdrg.nc M2rowdrg.nc mask_itf.nc
if [ ! -f alltides_${CONFPAR}.nc ]; then
  mdss -P ${PROJMDSS} get ${STOCKDIR}/input/nemo_${CONFPAR}/alltides_${CONFPAR}.nc
fi
ln -s -v alltides_${CONFPAR}.nc K1rowdrg.nc
ln -s -v alltides_${CONFPAR}.nc M2rowdrg.nc
ln -s -v alltides_${CONFPAR}.nc mask_itf.nc

if [ $CONFPAR == "trop075" ]; then
  rm -f ahmcoef.nc
  if [ ! -f ahmcoef_${CONFPAR}.nc ]; then
    mdss -P ${PROJMDSS} get ${STOCKDIR}/input/nemo_${CONFPAR}/ahmcoef_${CONFPAR}.nc
  fi
  ln -s ahmcoef_${CONFPAR}.nc ahmcoef.nc
fi

# take updated bathy if there is a nest:
rm -f bathy_meter.nc
if [ ! -f bathy_meter_${CONFPAR}.nc ]; then
  mdss -P ${PROJMDSS} get ${STOCKDIR}/input/nemo_${CONFPAR}/bathy_meter_${CONFPAR}.nc
fi
ln -s -v bathy_meter_${CONFPAR}.nc bathy_meter.nc

rm -f chlorophyll.nc
if [ ! -f chlorophyll_${CONFPAR}.nc ]; then
  mdss -P ${PROJMDSS} get ${STOCKDIR}/input/nemo_${CONFPAR}/chlorophyll_${CONFPAR}.nc
fi
ln -s -v chlorophyll_${CONFPAR}.nc chlorophyll.nc

rm -f runoff.nc
if [ ! -f runoff_${CONFPAR}.nc ]; then
  mdss -P ${PROJMDSS} get ${STOCKDIR}/input/nemo_${CONFPAR}/runoff_${CONFPAR}.nc
fi
ln -s -v runoff_${CONFPAR}.nc runoff.nc

rm -f coordinates.nc
if [ ! -f coordinates_${CONFPAR}.nc ]; then
  mdss -P ${PROJMDSS} get ${STOCKDIR}/input/nemo_${CONFPAR}/coordinates_${CONFPAR}.nc
fi
ln -s -v coordinates_${CONFPAR}.nc coordinates.nc

for iZOOM in $(seq 1 ${NZOOM_NEMO})
do

  rm -f ${iZOOM}_K1rowdrg.nc ${iZOOM}_M2rowdrg.nc ${iZOOM}_mask_itf.nc
  if [ ! -f ${iZOOM}_alltides_${CONFPAR}.nc ]; then
    mdss -P ${PROJMDSS} get ${STOCKDIR}/input/nemo_${CONFPAR}/${iZOOM}_alltides_${CONFPAR}.nc
  fi
  ln -s -v ${iZOOM}_alltides_${CONFPAR}.nc  ${iZOOM}_K1rowdrg.nc
  ln -s -v ${iZOOM}_alltides_${CONFPAR}.nc  ${iZOOM}_M2rowdrg.nc
  ln -s -v ${iZOOM}_alltides_${CONFPAR}.nc  ${iZOOM}_mask_itf.nc

  if [ $CONFPAR == "trop075" ]; then
    rm -f ${iZOOM}_ahmcoef.nc
    if [ ! -f ${iZOOM}_ahmcoef_${CONFPAR}.nc ]; then
      mdss -P ${PROJMDSS} get ${STOCKDIR}/input/nemo_${CONFPAR}/${iZOOM}_ahmcoef_${CONFPAR}.nc
    fi
    ln -s ${iZOOM}_ahmcoef_${CONFPAR}.nc ${iZOOM}_ahmcoef.nc
  fi

  rm -f ${iZOOM}_bathy_meter.nc
  if [ ! -f ${iZOOM}_bathy_meter_${CONFPAR}.nc ]; then
    mdss -P ${PROJMDSS} get ${STOCKDIR}/input/nemo_${CONFPAR}/${iZOOM}_bathy_meter_${CONFPAR}.nc
  fi
  ln -s -v ${iZOOM}_bathy_meter_${CONFPAR}.nc ${iZOOM}_bathy_meter.nc

  rm -f ${iZOOM}_chlorophyll.nc
  if [ ! -f ${iZOOM}_chlorophyll_${CONFPAR}.nc ]; then
    mdss -P ${PROJMDSS} get ${STOCKDIR}/input/nemo_${CONFPAR}/${iZOOM}_chlorophyll_${CONFPAR}.nc
  fi
  ln -s -v ${iZOOM}_chlorophyll_${CONFPAR}.nc ${iZOOM}_chlorophyll.nc

  rm -f ${iZOOM}_runoff.nc
  if [ ! -f ${iZOOM}_runoff_${CONFPAR}.nc ]; then
    mdss -P ${PROJMDSS} get ${STOCKDIR}/input/nemo_${CONFPAR}/${iZOOM}_runoff_${CONFPAR}.nc
  fi
  ln -s -v ${iZOOM}_runoff_${CONFPAR}.nc ${iZOOM}_runoff.nc

  rm -f ${iZOOM}_coordinates.nc

  if [ ! -f ${iZOOM}_coordinates_${CONFPAR}.nc ]; then
    mdss -P ${PROJMDSS} get ${STOCKDIR}/input/nemo_${CONFPAR}/${iZOOM}_coordinates_${CONFPAR}.nc
  fi
  ln -s -v ${iZOOM}_coordinates_${CONFPAR}.nc ${iZOOM}_coordinates.nc

done

########################################################################
##-- import NEMO's Open Boundary Conditions (OBCs)

for OBC in south north east west
do

  rm -f obc_${OBC}_y${YEAR}.nc
  if [ ! -f OBC_${OBC}_${CONFPAR}_y${YEAR}.nc ]; then
    mdss -P $PROJMDSS get ${STOCKDIR}/input/nemo_${CONFPAR}/OBC_${OBC}_${CONFPAR}_y${YEAR}.nc
  fi
  ln -s -v OBC_${OBC}_${CONFPAR}_y${YEAR}.nc obc_${OBC}_y${YEAR}.nc

  rm -f obc_${OBC}_y${YEARm1}.nc
  if [ ! -f OBC_${OBC}_${CONFPAR}_y${YEARm1}.nc ]; then
    mdss -P $PROJMDSS get ${STOCKDIR}/input/nemo_${CONFPAR}/OBC_${OBC}_${CONFPAR}_y${YEARm1}.nc
  fi
  ln -s -v OBC_${OBC}_${CONFPAR}_y${YEARm1}.nc obc_${OBC}_y${YEARm1}.nc

  rm -f obc_${OBC}_y${YEARp1}.nc
  if [ ! -f OBC_${OBC}_${CONFPAR}_y${YEARp1}.nc ]; then
    mdss -P $PROJMDSS get ${STOCKDIR}/input/nemo_${CONFPAR}/OBC_${OBC}_${CONFPAR}_y${YEARp1}.nc
  fi
  ln -s -v OBC_${OBC}_${CONFPAR}_y${YEARp1}.nc obc_${OBC}_y${YEARp1}.nc

done

######################################################################
##-- import WRF restarts if not already there :

RSTW=`grep " restart " namelist.input | awk '{print $3}' | sed -e "s/\.//g ; s/\,//g"`
if [ $RSTW == "true" ]; then
  if [ ! -f wrfrst_d01_${YEAR}-${MONTH}-${DAY}_00:00:00_0000 ]; then
    echo "Copy restart files from ${STOCKDIR}/restart/now_${CONFIG}_${CASE}"
    mdss -P $PROJMDSS get ${STOCKDIR}/restart/now_${CONFIG}_${CASE}/wrfrst_${YEAR}-${MONTH}-${DAY}.tar
    tar -xvf wrfrst_${YEAR}-${MONTH}-${DAY}.tar
  else
    echo "wrfrst_d0*_${YEAR}-${MONTH}-${DAY}_00:00:00_* are assumed to be already all there"
  fi
else
  echo "Not in restart mode -> no need to import any restart"
fi

########################################################################
##-- import NEMO's initial state or restart

rm -f restart.nc restart.obc
rm -f dta_temp.nc dta_sal.nc
RSTN=`grep "from a restart file" namelist | awk '{print $3}' | sed -e "s/\.//g"`
NIT_RST=${NITENDM1}
if [ $RSTN == "true" ]; then
  if [ $NIT_RST -eq 0 ]; then
    mdss -P $PROJMDSS get ${STOCKDIR}/input/nemo_${CONFPAR}/${CONFPAR}_restart_00000000.nc
    ln -s -v ${CONFPAR}_restart_00000000.nc restart.nc
  else
    if [ ! -f restart_${NIT_RST}.nc ] || [ ! -f restart_${NIT_RST}.obc ]; then
      echo "Copy restart files from ${STOCKDIR}/restart/now_${CONFIG}-${CASE}"
      mdss -P $PROJMDSS get ${STOCKDIR}/restart/now_${CONFIG}_${CASE}/${CONFPAR}-${CASE}_${NIT_RST}_restart.tar
      tar xvf ${CONFPAR}-${CASE}_${NIT_RST}_restart.tar
    fi
    ln -s -v restart_${NIT_RST}.nc   restart.nc
    ln -s -v restart_${NIT_RST}.obc  restart.obc
  fi
else
  echo "Not in restart mode -> import initial T,S state"
  if [ ! -f dta_temp_${CONFPAR}_y${Y0}m${M0}.nc ] || [ ! -f dta_sal_${CONFPAR}_y${Y0}m${M0}.nc ]; then
    echo "Copy initial state from ${STOCKDIR}/input/nemo_${CONFPAR}"
    mdss -P $PROJMDSS get ${STOCKDIR}/input/nemo_${CONFPAR}/dta_temp_${CONFPAR}_y${Y0}m${M0}.nc
    mdss -P $PROJMDSS get ${STOCKDIR}/input/nemo_${CONFPAR}/dta_sal_${CONFPAR}_y${Y0}m${M0}.nc
  fi
  ln -s -v dta_temp_${CONFPAR}_y${Y0}m${M0}.nc dta_temp.nc
  ln -s -v dta_sal_${CONFPAR}_y${Y0}m${M0}.nc  dta_sal.nc
fi

for iZOOM in $(seq 1 ${NZOOM_NEMO})
do

  rm -f ${iZOOM}_restart.nc
  rm -f ${iZOOM}_dta_temp.nc ${iZOOM}_dta_sal.nc
  RSTN=`grep "from a restart file" ${iZOOM}_namelist | awk '{print $3}' | sed -e "s/\.//g"`
  NIT_RST=${NITENDM1ZOOM}  ## NB: ADAPT THIS IF MORE THAN ONE AGRIF NEST ....
  if [ $RSTN == "true" ]; then
    if [ $NIT_RST -eq 0 ]; then
      echo "Importing ${iZOOM}_${CONFPAR}_restart_00000000.nc"
      mdss -P $PROJMDSS get ${STOCKDIR}/input/nemo_${CONFPAR}/${iZOOM}_${CONFPAR}_restart_00000000.nc
      ln -s -v ${iZOOM}_${CONFPAR}_restart_00000000.nc ${iZOOM}_restart.nc
    else
      if [ ! -f ${iZOOM}_restart_${NIT_RST}.nc ]; then
        echo "Copy restart files from ${STOCKDIR}/restart/now_${CONFIG}-${CASE}"
        echo "     --> ${iZOOM}_${CONFPAR}-${CASE}_${NIT_RST}_restart.tar"
        mdss -P $PROJMDSS get ${STOCKDIR}/restart/now_${CONFIG}_${CASE}/${iZOOM}_${CONFPAR}-${CASE}_${NIT_RST}_restart.tar
        tar xvf ${iZOOM}_${CONFPAR}-${CASE}_${NIT_RST}_restart.tar
      fi
      TJP=`expr ${iZOOM} + 5`
      NIT_RST_ZOOM=`tail -1 prod_nemo.db | cut -d ' ' -f${TJP}`
      ln -s -v ${iZOOM}_restart_${NIT_RST_ZOOM}.nc   ${iZOOM}_restart.nc
    fi
  else
    echo "Not in restart mode -> import initial T,S state"
    if [ ! -f ${iZOOM}_dta_temp_${CONFPAR}_y${Y0}m${M0}.nc ] || [ ! -f ${iZOOM}_dta_sal_${CONFPAR}_y${Y0}m${M0}.nc ]; then
      echo "Copy initial state from ${STOCKDIR}/input/nemo_${CONFPAR}"
      mdss -P $PROJMDSS get ${STOCKDIR}/input/nemo_${CONFPAR}/${iZOOM}_dta_temp_${CONFPAR}_y${Y0}m${M0}.nc
      mdss -P $PROJMDSS get ${STOCKDIR}/input/nemo_${CONFPAR}/${iZOOM}_dta_sal_${CONFPAR}_y${Y0}m${M0}.nc
    fi
    ln -s -v ${iZOOM}_dta_temp_${CONFPAR}_y${Y0}m${M0}.nc ${iZOOM}_dta_temp.nc
    ln -s -v ${iZOOM}_dta_sal_${CONFPAR}_y${Y0}m${M0}.nc  ${iZOOM}_dta_sal.nc
  fi

done

########################################################################

echo " "
echo "Transfer completed."
echo " "
echo " Launching the long simulation NOW !"

qsub run_now_${NRUN}.sh
