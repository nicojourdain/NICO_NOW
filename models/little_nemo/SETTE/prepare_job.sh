#####################################################
# Author : Italo Epicoco for NEMO
# Contact : italo.epicoco@unisalento.it
#
# Some scripts called by sette.sh 
# prepare_job.sh   : create the job script for running job 
######################################################
#set -vx
set -o posix
#set -u
#set -e
#+
#
# ================
# prepare_job.sh
# ================
#
# --------------------------
# create the job script for NEMO tests 
# --------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ ./prepare_job.sh INPUT_FILE_CONFIG_NAME NUMBER_PROC TEST_NAME MPI_FLAG JOB_FILE
#
#
# DESCRIPTION
# ===========
#
# Part of the SETTE package to run tests for NEMO
# 
# prepare the script $JOB_FILE to run the tests 
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./prepare_job.sh INPUT_FILE_CONFIG_NAME NUMBER_PROC TEST_NAME MPI_FLAG $JOB_FILE
#
# prepare the $JOB_FILE for execution 
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
# $Id: prepare_job.sh 3050 2011-11-07 14:11:34Z acc $
#
#
#
#   * creation
#
#-
#

usage=" Usage : ./prepare_job.sh INPUT_FILE_CONFIG_NAME NUMBER_PROC TEST_NAME MPI_FLAG JOB_FILE"
usage=" example : ./prepare_job.sh input_ORCA2_LIM_PISCES.cfg 8 SHORT no/yes $JOB_FILE"


minargcount=5
        if [ ${#} -lt ${minargcount} ]
        then
                echo "not enough arguments for prepare_job.sh script"
                echo "control number of argument of prepare_job.sh in sette.sh"
                echo "${usage}"
        exit 1
        fi
        unset minargcount
	if [ ! -f ${SETTE_DIR}/output.sette ] ; then
	        touch ${SETTE_DIR}/output.sette
	fi
       
#
# set and export TEST_NAME. It will be used within the post_test_tidyup function
#
INPUTARFILE=$1
NB_PROC=$2
TEST_NAME=$3
MPI_FLAG=$4
JOB_FILE=$5

# export EXE_DIR. This directory is used to execute model 
#
export EXE_DIR
#
#
echo "date: `date`" >> ${SETTE_DIR}/output.sette
echo "" >> ${SETTE_DIR}/output.sette
echo "running config: ${NEW_CONF}" >> ${SETTE_DIR}/output.sette
echo "" >> ${SETTE_DIR}/output.sette
echo "list of cpp_keys: " >> ${SETTE_DIR}/output.sette
echo "`more ${SETTE_DIR}/../CONFIG/${NEW_CONF}/cpp_${NEW_CONF}.fcm`" >> ${SETTE_DIR}/output.sette
echo "" >> ${SETTE_DIR}/output.sette
echo "compiling with: ${CMP_NAM}" >> ${SETTE_DIR}/output.sette
echo "" >> ${SETTE_DIR}/output.sette
echo "executing script : \"fcm_job $@\" " >> ${SETTE_DIR}/output.sette
echo "            " >> ${SETTE_DIR}/output.sette

################################################################
# SET INPUT
# get the input tarfile if needed
tar_file=$(sed -ne "1,1p" ${SETTE_DIR}/$INPUTARFILE)
if [ "$(cat ${SETTE_DIR}/$INPUTARFILE | grep -c ".tar" )" -ne 0 ] ; then
	echo "looking for tar file" >> ${SETTE_DIR}/output.sette
	echo "            " >> ${SETTE_DIR}/output.sette
	cp ${FORCING_DIR}/${tar_file} ${INPUT_DIR}/.
	echo "file in ${INPUT_DIR}"
	if [ ! -f ${INPUT_DIR}/${tar_file} ] ; then 
		echo "PROBLEM during copy of tar file" >> ${SETTE_DIR}/output.sette
		echo "tar file IS NOT present in ${INPUT_DIR} directory " >> ${SETTE_DIR}/output.sette
		echo "            " >> ${SETTE_DIR}/output.sette
		echo "PROBLEM during copy of tar file" 
		exit 1
	else  
		cd ${EXE_DIR} ; tar xvof ${INPUT_DIR}/*.tar ; gunzip -f *gz
	fi
fi 

if [ ! -f ${EXE_DIR}/namelist_ice ] ; then 
if [ -f ${EXE_DIR}/namelist_ice_lim2 ] ; then 
   echo "choosing for namelist ice " >> ${SETTE_DIR}/output.sette
   echo "            " >> ${SETTE_DIR}/output.sette
   cp ${EXE_DIR}/namelist_ice_lim2 ${EXE_DIR}/namelist_ice
elif  [ -f ${EXE_DIR}/namelist_ice_lim3 ] ; then 
   cp ${EXE_DIR}/namelist_ice_lim3 ${EXE_DIR}/namelist_ice
fi
fi


if [ "$(cat ${CONFIG_DIR}/${NEW_CONF}/cpp_${NEW_CONF}.fcm | grep -c "agrif" )" -ne 0 ] ; then
	#- Namelist for ocean and ice (agrif fine grid)
   	echo "choosing for namelist for AGRIF" >> ${SETTE_DIR}/output.sette
   	echo "            " >> ${SETTE_DIR}/output.sette
	cp ${EXE_DIR}/1_namelist_ice_lim2 ${EXE_DIR}/1_namelist_ice 
fi
################################################################

################################################################
# RUN OPA
cd ${EXE_DIR}
if [ ! -r ${EXE_DIR}/opa ]
    then
    echo "executable opa does not exist"
    echo "executable opa does not exist, exit"  >> ${SETTE_DIR}/output.sette
    exit 1
fi

# example for NOCS Altix system using PBS batch submission (requires ${SETTE_DIR}/sette_batch_template file)
#
  #  if [ ${MPI_FLAG} == "no" ] ; then
		case ${COMPILER} in 
			ALTIX_NAUTILUS_MPT)
                                NB_REM=$( echo $NB_PROC | awk '{print $1 % 4}')
		        	if [ ${NB_REM} == 0 ] ; then
					# number of processes required is an integer multiple of 4
					#
					NB_NODES=$( echo $NB_PROC | awk '{print $1 / 4}')
				else
					#
					# number of processes required is not an integer multiple of 4
					# round up the number of nodes required.
					#
					NB_NODES=$( echo $NB_PROC | awk '{printf("%d",$1 / 4 + 1 )}')
	       			fi
				;;
			*)
				NB_NODES=${NB_PROC}
				;;

		esac
#
# Pass settings into job file by using sed to edit predefined strings
#
        cat ${SETTE_DIR}/job_batch_template | sed -e"s/NODES/${NB_NODES}/" -e"s/NPROCS/${NB_PROC}/" \
             -e"s:DEF_SETTE_DIR:${SETTE_DIR}:" -e"s:DEF_INPUT_DIR:${INPUT_DIR}:" \
             -e"s:DEF_EXE_DIR:${EXE_DIR}:" \
             -e"s:DEF_CONFIG_DIR:${CONFIG_DIR}:" \
             -e"s:MPI_FLAG:${MPI_FLAG}:" \
             -e"s:DEF_NEMO_VALIDATION:${NEMO_VALIDATION_DIR}:" -e"s:DEF_NEW_CONF:${NEW_CONF}:" \
             -e"s:DEF_CMP_NAM:${CMP_NAM}:" -e"s:DEF_TEST_NAME:${TEST_NAME}:" > run_sette_test.job
	
#
# create the unique submission job script
#
	if [ ! -f $JOB_FILE ] ; then
		mv run_sette_test.job $JOB_FILE
	else
	    e=`grep -n "# END_BODY" ${JOB_FILE} | cut -d : -f 1`
            e=$(($e - 1))
	    head -$e $JOB_FILE > ${JOB_FILE}_new 
	    mv ${JOB_FILE}_new ${JOB_FILE}
	    l=`wc -l run_sette_test.job | sed -e "s:run_sette_test.job::"`
	    b=`grep -n "# BODY" run_sette_test.job | cut -d : -f 1`
	    t=$(($l - $b))
	    tail -$t run_sette_test.job >> $JOB_FILE
	fi
	
	chmod a+x $JOB_FILE ; echo "$JOB_FILE is ready"

#fi
