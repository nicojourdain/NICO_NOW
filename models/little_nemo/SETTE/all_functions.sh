######################################################
# Author : Simona Flavoni for NEMO
# Contact : sflod@locean-ipsl.upmc.fr
#
# ----------------------------------------------------------------------
# NEMO/SETTE , NEMO Consortium (2010)
# Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
# ----------------------------------------------------------------------
#
# Some scripts called by sette.sh
# all_functions.sh   : all functions used by sette.sh  
######################################################
#set -x
set -o posix
#set -u
#set -e
#+
#
# ================
# all_functions.sh
# ================
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
#  $ ./set_namelist INPUT_NAMELIST VARIABLE VALUE 
#  $ post_test_tidyup 
#
#
# DESCRIPTION
# ===========
#
# function superegrep
#   input variable value
#
# function set_namelist
#   input namelist_name variable value
#   output namelist
#
# function post_test_tidyup
#   creates nemo_validation tree, and save output & debug files
#   this function creates tree of validation in NEMO_VALIDATION_DIR as follows : 
#
# NEMO_VALIDATION_DIR/WCONFIG_NAME/WCOMPILER_NAME/REVISION_NUMBER(or DATE)/TEST_NAME
# 
# NEMO_VALIDATION_DIR           : is choosen in param.cfg
#
# WCONFIG_NAME                  : set by makenemo at the moment of compilation
#
# WCOMPILER_NAME                : set by makenemo at the moment of compilation
#
# REVISION_NUMBER(or DATE)      : revision number by svn info, if problems with svn date is taken
#
# TEST_NAME                     : set in sette.sh for each configuration to be tested (directory TEST_NAME is created under ${NEW_CONF} directory )
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./set_namelist namelist          nn_itend        75
#  $ ./set_namelist namelist_ice_lim2 cn_icerst_in  \"00101231_restart_ice\"
#  $ post_test_tidyup 
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
# $Id: all_functions.sh 3294 2012-01-28 16:44:18Z rblod $
#
#   * creation
#-
# function to find namelists parameters
supergrep () {
            grep "^ *$1 *=" $2 | sed -e "s% *\!.*%%"
    }

usage=" Usage : set_namelist input_namelist variable_name value"
usage=" if value is a string ths is neede syntax : ./set_namelist namelist_name var_name \"new_value\" "

# function to set namelists parameters
set_namelist () {
	minargcount=3
	if [ ${#} -lt ${minargcount} ]
	then
		echo "not enought arguments for set_namelist"
		echo "${usage}"
		exit 1
	fi
	unset minargcount
	if [  ! -f ${SETTE_DIR}/output.sette ] ; then
                touch ${SETTE_DIR}/output.sette
        fi

        echo "executing script : set_namelist $@" >> ${SETTE_DIR}/output.sette
        echo "################" >> ${SETTE_DIR}/output.sette
      
	VAR_NAME=$( supergrep $2 ${EXE_DIR}/$1 )
	if [ ${#VAR_NAME} -eq 0 ] 
	then
		echo "doing \"set_namelist $@\". "
		echo "variable: \"$2\" is empty"
		echo "control that variable $2 is in \"${EXE_DIR}/$1\" "
		echo "exit"
		echo "error in executing script : set_namelist $@" >> ${SETTE_DIR}/output.sette
		echo "....." >> ${SETTE_DIR}/output.sette
		exit 1
	fi
        sed -e "s:${VAR_NAME}.*:${VAR_NAME}:" ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1
        sed -e "s:${VAR_NAME}:$2=$3:"  ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1

        echo "finished script : set_namelist $@" >> ${SETTE_DIR}/output.sette
        echo "++++++++++++++++" >> ${SETTE_DIR}/output.sette
        echo "                " >> ${SETTE_DIR}/output.sette
}

# function to tidy up after each test and populate the NEMO_VALIDATION store
post_test_tidyup () {
#
# requires the following variables defined and exported from the calling script:
#  SETTE_DIR
#  INPUT_DIR
#  EXE_DIR
#  CONFIG_DIR
#  NEMO_VALIDATION_DIR
#  NEW_CONF
#  CMP_NAM
#  TEST_NAME
echo "SETTE directory is : ${SETTE_DIR}"
echo "INPUT directory is : ${INPUT_DIR}"
echo "EXECUTION directory is : ${EXE_DIR}"
echo "CONFIG directory is : ${CONFIG_DIR}"
echo "VALIDATION directory is : ${NEMO_VALIDATION_DIR}"
echo "NEW CONFIGURATION is : ${NEW_CONF}"
echo "COMPILER is : ${CMP_NAM}"
echo "TEST is : ${TEST_NAME}"
################################################################
# SMALL DEBUG
    if [ ! -r ${EXE_DIR}/time.step ]
        then
        echo "file time.step does not exist"   >> ${SETTE_DIR}/output.sette
        echo "some problems during execution of model"  >> ${SETTE_DIR}/output.sette 
        exit 1
    else
        echo "file time.step exists"  >> ${SETTE_DIR}/output.sette
        echo "ok model run"   >> ${SETTE_DIR}/output.sette
    fi
################################################################

################################################################
# Creation of NEMO_VALIDATION tree
    export LANG=en_US 
    cd ${CONFIG_DIR}
    cd ../
    REVISION_NB=`svn info | grep -i "Revision:" | sed -e "s/ //" | cut -d ":" -f 2`
    if [ ${#REVISION_NB} -eq 0 ]
    then
    	echo "some problems with svn info command"
    	echo "some problems with svn info command" >> ${SETTE_DIR}/output.sette
    	REVISION_NB=`date +%Y%m%d`
    	echo "put in ${REVISION_NB} date"
    	echo "put in ${REVISION_NB} date" >> ${SETTE_DIR}/output.sette
    else
    echo "value of revision number of NEMOGCM: ${REVISION_NB}"
    fi
    cd ${NEMO_VALIDATION_DIR}
    `mkdir -p ${NEMO_VALIDATION_DIR}/W${NEW_CONF}/${CMP_NAM}/${REVISION_NB}/${TEST_NAME}`
    NEMO_VALID=${NEMO_VALIDATION_DIR}/W${NEW_CONF}/${CMP_NAM}/${REVISION_NB}/${TEST_NAME}
    if [ -d ${NEMO_VALID} ] ; then
	echo "created ${NEMO_VALID} directory"   >> ${SETTE_DIR}/output.sette
    else 
	echo "problems in creating ${NEMO_VALID} directory"   >> ${SETTE_DIR}/output.sette
	echo "EXIT,"
	exit 1
    fi
    # Save output & debug files in NEMO_VALIDATION tree
    echo "saving ocean & ice output, solver.stat, tracer.stat files ...." >> ${SETTE_DIR}/output.sette
    echo "            " >> ${SETTE_DIR}/output.sette
    [ -f ${EXE_DIR}/*ocean.output ] && cp ${EXE_DIR}/*ocean.output ${NEMO_VALID}/.
    [ -f ${EXE_DIR}/*solver.stat ] && cp ${EXE_DIR}/*solver.stat ${NEMO_VALID}/.
    [ -f ${EXE_DIR}/*tracer.stat ] && cp ${EXE_DIR}/*tracer.stat ${NEMO_VALID}/.
    if [ -n "$(ls ${NEMO_VALID}/*solver*)" ] ; then
	echo "moved solver.stat in ${NEMO_VALID} directory"  >> ${SETTE_DIR}/output.sette
	echo "moved solver.stat in ${NEMO_VALID} directory"  
    else
	echo "problem in looking for solver.stat file in ${NEMO_VALID} directory"  >> ${SETTE_DIR}/output.sette
	echo "solver.stat IS NOT in ${NEMO_VALID} directory" 
    fi
    if [ -n "$(ls ${NEMO_VALID}/*ocean.output*)" ] ; then
	echo "moved ocean.output in ${NEMO_VALID} directory"  >> ${SETTE_DIR}/output.sette
	echo "moved ocean.output in ${NEMO_VALID} directory" 
    else
	echo "problem in looking for ocean.output file in ${NEMO_VALID} directory"  >> ${SETTE_DIR}/output.sette
	echo "ocean.output IS NOT in ${NEMO_VALID} directory" 
    fi
    if [ -n "$(ls ${NEMO_VALID}/*tracer.stat*)" ] ; then
        echo "moved tracer.stat in ${NEMO_VALID} directory"  >> ${SETTE_DIR}/output.sette
        echo "moved tracer.stat in ${NEMO_VALID} directory"
    else
        echo "problem in looking for tracer.stat file in ${NEMO_VALID} directory"  >> ${SETTE_DIR}/output.sette
        echo "tracer.stat IS NOT in ${NEMO_VALID} directory"
    fi
}
