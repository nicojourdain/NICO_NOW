#####################################################
# Author : Simona Flavoni for NEMO
# Contact : sflod@locean-ipsl.upmc.fr
#
# Some scripts called by sette.sh 
# fcm_job.sh   : simple job to run NEMO with fcm 
######################################################
#set -vx
set -o posix
#set -u
#set -e
#+
#
# ================
# fcm_job.sh
# ================
#
# --------------------------
# Simple job for NEMO tests 
# --------------------------
#
# SYNOPSIS
# ========
#
# :: lauches the script $JOB_FILE interactive or batch, one task or MPI
#
#  $ ./fcm_job.sh NUMBER_OF_PROCS JOB_FILE INTERACT MPI_FLAG
#
#
# DESCRIPTION
# ===========
#
# Simple job for SET TESTS for NEMO (SETTE)
# 
#  
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./fcm_job.sh NUMBER_OF_PROCS JOB_FILE INTERACT MPI_FLAG
#
#  run a 
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
# $Id: fcm_job.sh 3336 2012-03-30 09:29:20Z clevy $
#
#
#
#   * creation
#
#-
#

usage=" Usage : ./fcm_job.sh  NUMBER_OF_PROCS JOB_FILE INTERACT MPI_FLAG"
usage=" example : ./fcm_job.sh 8 SHORT no/yes no/yes"


minargcount=4
        if [ ${#} -lt ${minargcount} ]
        then
                echo "not enough arguments for fcm_job.sh script"
                echo "control number of argument of fcm_job.sh in sette.sh"
                echo "${usage}"
        exit 1
        fi
        unset minargcount
	if [ ! -f ${SETTE_DIR}/output.sette ] ; then
	        touch ${SETTE_DIR}/output.sette
	fi
       

export NB_PROCS=$1
export JOB_FILE=$2
export INTERACT=$3
export MPI_FLAG=$4
################################################################
# RUN OPA
cd ${EXE_DIR}
if [ "$INTERACT" == "yes" ]; then
        eval $JOB_FILE
else if [ "$INTERACT" == "no" ]; then
# submit job to batch system 
        if [ "$NB_PROC" == "1" ]; then
	   eval ${BATCH_COMMAND_SEQ} $JOB_FILE ; echo  ${BATCH_COMMAND_SEQ} $JOB_FILE
        else
	   eval ${BATCH_COMMAND_PAR} $JOB_FILE ; echo ${BATCH_COMMAND_PAR} $JOB_FILE
        fi
fi
fi
