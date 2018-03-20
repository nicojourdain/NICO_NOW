#!/bin/bash
#############################################################
# Author : Simona Flavoni for NEMO
# Contact : sflod@locean-ipsl.upmc.fr
#
# sette.sh   : principal script of SET TEsts for NEMO (SETTE)
# ----------------------------------------------------------------------
# NEMO/SETTE , NEMO Consortium (2010)
# Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
# ----------------------------------------------------------------------
#
#############################################################
#set -vx
set -o posix
#set -u
#set -e
#+
#
# ===================
# sette_beginner.sh
# ===================
#
# ----------------------------------------------
# Set of tests for NEMO for beginners
# ----------------------------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ ./sette_beginner.sh
#
# DESCRIPTION
# ===========
# First simple example of how to use SETTE: create GYRE_SHORT configuration, compile it with 1 proc, and test it for a SHORT test: 5days
#-
#
# Compiler among those in NEMOGCM/ARCH
COMPILER=PW6_VARGAS
export BATCH_COMMAND_PAR="llsubmit"
export BATCH_COMMAND_SEQ=$BATCH_COMMAND_PAR
# export MPI_INTERACT="no"

# Directory to run the tests
SETTE_DIR=$(cd $(dirname "$0"); pwd)
MAIN_DIR=${SETTE_DIR%/SETTE}
CONFIG_DIR=${MAIN_DIR}/CONFIG
TOOLS_DIR=${MAIN_DIR}/TOOLS
COMPIL_DIR=${TOOLS_DIR}/COMPILE

CMP_NAM=${1:-$COMPILER}
# Copy job_batch_COMPILER file for specific compiler into job_batch_template
cd ${SETTE_DIR}
cp BATCH_TEMPLATE/batch-${COMPILER} job_batch_template || exit

# Run for GYRE CONFIG
# small test to start
# compile GYRE configuration with gfortran_osx compiler run with 1 proc, by default in cpp_GYRE.fcm file : 
export TEST_NAME="TEST02"
cd ${SETTE_DIR}
. ../CONFIG/makenemo -m ${CMP_NAM} -n GYRE_SHORT -r GYRE
cd ${SETTE_DIR}
. param.cfg 
. all_functions.sh
# creation of execution directory
. prepare_exe_dir.sh
JOB_FILE=${EXE_DIR}/run_job.sh
cd ${EXE_DIR}
# setting namelist parameters
# experience name
set_namelist namelist cn_exp \"GYRE_SHORT\"
# first time step
set_namelist namelist nn_it000 1
# last time step
set_namelist namelist nn_itend 120
# frequency of creation of a restart file
set_namelist namelist nn_stock 60

cd ${SETTE_DIR}
# . ./prepare_job.sh input_file_config_name NB_PROCS TEST_NAME MPI_FLAG JOB_FILE
. ./prepare_job.sh input_GYRE.cfg 1 SHORT no $JOB_FILE
# run job, with 1 processor, test named SHORT (= 60 time steps)
#. ./fcm_job.sh NB_PROCS JOB_FILE INTERACT_FLAG MPI_FLAG
    cd ${SETTE_DIR}   
. ./fcm_job.sh 1 $JOB_FILE no no

