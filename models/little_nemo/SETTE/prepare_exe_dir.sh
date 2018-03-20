#!/bin/bash
##########################################################################
# Author : Simona Flavoni for NEMO
# Contact : sflod@locean-ipsl.upmc.fr
#
# ----------------------------------------------------------------------
# NEMO/SETTE , NEMO Consortium (2010)
# Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
# ----------------------------------------------------------------------
#
# Some scripts called by sette.sh
# prepare_exe_dir.sh : script prepares execution directory for test
##########################################################################
#set -x
set -o posix
#set -u
#set -e
#+
#
# ==================
# prepare_exe_dir.sh
# ==================
#
# ----------------------------------------------
# Set of functions used by sette.sh (NEMO tests) 
# ----------------------------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ ./prepare_exe_dir.sh
#
# DESCRIPTION
# ===========
#
# prepare_exe_dir.sh creates execution directory takes name of TEST_NAME defined in every test in sette.sh
# 
# it is necessary to define in sette.sh TEST_NAME ( example : export TEST_NAME="LONG") to create execution directory in where run test.
#
# NOTE : each test has to run in its own directory ( of execution), if not existing files are re-written (for example namelist)
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./prepare_exe_dir.sh
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
# $Id: $
#
#   * creation
#-


cd ${CONFIG_DIR}
mkdir -p ${NEW_CONF}/${TEST_NAME}

export EXE_DIR=${CONFIG_DIR}/${NEW_CONF}/${TEST_NAME}

cp -r ${CONFIG_DIR}/${NEW_CONF}/EXP00/* ${EXE_DIR}/.
cp -r ${SETTE_DIR}/iodef_sette.xml ${EXE_DIR}/iodef.xml
cd ${EXE_DIR}
