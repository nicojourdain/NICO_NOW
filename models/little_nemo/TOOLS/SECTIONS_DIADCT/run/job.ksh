#!/bin/ksh
set -x
#####################################################
#     CONFIGURATION CHOICE                          #
#####################################################
#CONFIG=ORCA2_LIM GYRE
CONFIG=ORCA2_LIM

#####################################################
#     CONFIGURATION DESCRIPTION                     #
#####################################################
# each configuration is defined by:
#   - INSPACE: directory the coordinate netcdf file is
#   - COOR_FIL: configuration's coordinate netcdf file
#   - LIST:     configuration's list of sections
#
#####################################################
case $CONFIG in
#
ORCA2_LIM)
INSPACE=/dataref/rd/INITIALISATION/ORCA2
COOR_FIL=coordinates.nc
LIST=list_sections.ascii_global
;;
#
GYRE)
INSPACE=/perm/ms/fr/ar5
COOR_FIL=mesh_mask.nc
LIST=list_sections.ascii_GYRE
;;
#
esac

#####################################################
#     RUN                                           #
#####################################################
BIN=diadct_sections.exe

export CTLDIR=`pwd`
cd ${CTLDIR}
rm -f coordinates.nc ${BIN} list_sections.ascii
cp ../${BIN} .
ln -sf ${LIST} list_sections.ascii
ln -s $INSPACE/$COOR_FIL ./coordinates.nc
./${BIN}
mv section_ijglobal.diadct section_ijglobal.diadct_$CONFIG 
