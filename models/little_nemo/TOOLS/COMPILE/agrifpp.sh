#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ==========
# agrifpp.sh
# ==========
#
# ----------------------------
# Preform AGrif pre-processing
# ----------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ agrifpp.sh
#
#
# DESCRIPTION
# ===========
#
#
# Look after key agrif, if yes the conv is used, otherwise standard pre-processing is performed.
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./agrifpp.sh 1 -Dkey
# 
#  or
#
#  $ ./agrifpp.sh -Dkey
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
# $Id: agrifpp.sh 2143 2010-10-04 12:49:55Z rblod $
#
#
#
#   * creation
#
#-

if [ "$1" == "1" ]; then
   shift
   MYFILE=`echo $* |awk -F" " '{print $NF}' `  
   FPP_OPT=` echo $* |awk '{  for (i=1 ; i<NF ; i++ )  { printf " %s",  $i } }'`


   if [ "${MYFILE}" != "par_oce.F90" -a  ! -f ${NEMO_TDIR}/${NEW_CONF}}/OPAFILES/param_done ]; then
      cpp ${FPP_OPT} ${CONFIG_DIR}/${NEW_CONF}/WORK/par_oce.F90 > ${NEMO_TDIR}/${NEW_CONF}/OPAFILES/par_oce.F90
      (cd ${NEMO_TDIR}/${NEW_CONF}/OPAFILES ; ${NEMO_TDIR}/${NEW_CONF}/OPAFILES/conv agrif_opa.in -rm -comdirin ./ -comdirout AGRIF_MODELFILES/ -convfile par_oce.F90 > /dev/null )
      cpp ${FPP_OPT}  -I${NEMO_TDIR}/${NEW_CONF}/OPAFILES/AGRIF_INC  ${NEMO_TDIR}/${NEW_CONF}/OPAFILES/AGRIF_MODELFILES/par_oce.F90 > ${NEMO_TDIR}/${NEW_CONF}/OPAFILES/par_oce.F90
      touch ${NEMO_TDIR}/${NEW_CONF}/OPAFILES/param_done
   elif  [ "${MYFILE}" == "par_oce.F90" -a -f ${NEMO_TDIR}/${NEW_CONF}/OPAFILES/param_done ] ; then
      cd  ${NEMO_TDIR}/${NEW_CONF}/OPAFILES
      cpp ${FPP_OPT} -I${NEMO_TDIR}/${NEW_CONF}/OPAFILES/AGRIF_INC ${MYFILE}
      exit
   fi

   if [ "${MYFILE}" == agrif2model.F90 ]; then
      cpp  -I${NEMO_TDIR}/${NEW_CONF}/OPAFILES/AGRIF_INC $@
      exit
   fi 

   cpp  -I${NEMO_TDIR}/${NEW_CONF}/OPAFILES/AGRIF_INC $@ > ${NEMO_TDIR}/${NEW_CONF}/OPAFILES/${MYFILE}
   ( cd ${NEMO_TDIR}/${NEW_CONF}/OPAFILES ; ${NEMO_TDIR}/${NEW_CONF}/OPAFILES/conv agrif_opa.in -rm -comdirin ./ -comdirout AGRIF_MODELFILES/ -convfile ${MYFILE} > /dev/null )
   cd  ${NEMO_TDIR}/${NEW_CONF}
   sed 's/[ 	]*$//' OPAFILES/AGRIF_MODELFILES/${MYFILE}> OPAFILES/${MYFILE}
   cpp ${FPP_OPT} -IOPAFILES/AGRIF_INC OPAFILES/${MYFILE} 
else
   shift
   cpp $@
fi

