#!/bin/bash
######################################################
# Author : Rachid Benshila for NEMO
# Contact : rblod@locean-ipsl.upmc.fr
#
# Some functions called from makenemo
# Fmake_WORK      : create links in the WORK
######################################################
#set -x
set -o posix
#set -u
#set -e
#+
#
# =============
# Fmake_WORK.sh
# =============
#
# -----------------------
# Make the WORK directory
# -----------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fmake_WORK.sh
#
#
# DESCRIPTION
# ===========
#
#
# Make the WORK directory:
#
# - Create lin in NEW_CONF/WORK
# - Use specified sub-directories previously
# - OPA has to be done first !!!
#
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fmake_WORK.sh ORCA2_LIM OPA_SRC LIM_SRC_2
#
#
# TODO
# ====
#
# option debug
#
#
# EVOLUTIONS
# ==========
#
# $Id: Fmake_WORK.sh 3294 2012-01-28 16:44:18Z rblod $
#
#
#
#   * creation
#
#-
declare ZSRC=$1 ; shift 
declare ZCONF=$1 ; shift
ZTAB=( $@ )
declare i=0 ; declare NDIR=${#ZTAB[@]}

echo "Creating ${ZCONF}/WORK = ${ZTAB[*]} for ${ZCONF}"

[ ! -d ${ZCONF}/MY_SRC ] && \mkdir ${ZCONF}/MY_SRC
[   -d ${ZCONF}/WORK   ] || \mkdir ${ZCONF}/WORK

if [ "${ZSRC}" != "none" ] ; then 
  if [ -d ${ZSRC} ] ; then 
     ln -sf ${ZSRC}/*.[Ffh]90 ${ZCONF}/MY_SRC/. 
     echo "MY_SRC content is linked to ${ZSRC}" 
  else
     echo "External directory for MY_SRC does not exist. Using default."
  fi
else 
echo "MY_SRC directory is : ${ZCONF}/MY_SRC"
fi

\rm -f ../${1}/WORK/*

while [ $i -lt $NDIR ]
do
   [ "${ZTAB[i]}" = "OPA_SRC" ] && ln -sf ${NEMO_DIR}/OPA_SRC/*.[Ffh]90 ${ZCONF}/WORK
   [ "${ZTAB[i]}" = "OPA_SRC" ] && ln -sf ${NEMO_DIR}/OPA_SRC/*/*.[Ffh]90 ${ZCONF}/WORK && break
   let i=$i+1
done

i=0
while [ $i -lt $NDIR ]
do
   if [ "${ZTAB[i]}" != "OPA_SRC" ]; then
      ln -sf ${NEMO_DIR}/${ZTAB[i]}/*.[Ffh]90 ${ZCONF}/WORK
      ln -sf ${NEMO_DIR}/${ZTAB[i]}/*/*.[Ffh]90 ${ZCONF}/WORK 2>/dev/null
   fi
   let i=$i+1
done

for i in `(cd ${ZCONF}/MY_SRC ; ls *.[Ffh]90 2>/dev/null ) `
   do
      [ -f ${ZCONF}/MY_SRC/$i ] &&  ln -sf $PWD/${ZCONF}/MY_SRC/${i} ${ZCONF}/WORK/.
   done

unset -v  ZCONF
unset -v ZTAB
unset -v  i
unset -v NDIR

