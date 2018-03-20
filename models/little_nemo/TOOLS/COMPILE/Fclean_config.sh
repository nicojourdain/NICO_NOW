#!/bin/bash
######################################################
# Author : Simona Flavoni for NEMO
# Contact : sflod@locean-ipsl.upmc.fr
#
# Some functions called from makenemo
# Fclean_config   : config removing 
######################################################
#set -x
set -o posix
#set -u
#set -e
#+
#
# ================
# Fclean_config.sh
# ================
#
# ------------------------
# Remove the configuration
# ------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fclean_config.sh CONFNAME
#
#
# DESCRIPTION
# ===========
#
#
# Remove the configuration:
#
# - remove CONFIG_NAME/WORK
# - remove CONFIG_NAME/BLD
# - remove CONFIG_NAME from TOOLS/TOOLS/COMPILE/cfg.txt 
# 
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fclean_config.sh ORCA2_LIM
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
# $Id: Fclean_config.sh 2158 2010-10-20 17:30:03Z sflod $
#
#
#
#   * creation
#
#-
 NEW_CONF=${x_n}
 if [ ${#NEW_CONF} -eq 0 ] ; then
 	   echo " "
	   echo "No configuration specified, please use makenemo -n CONFIG clean_config "
 else
    echo "Are you sure that you want to remove this directory $NEW_CONF? [y/n] "
    read answer
    answer=`echo $answer | sed 's/^[y].*$/y/'`
    if [  -z "$answer" -o "x$answer" = "xy" ]; then
 	   # testing if configuration exists
 	   if [ "$(cat ${CONFIG_DIR}/cfg.txt | grep "${NEW_CONF} ")"  == "" ] ; then
		  echo "The configuration ${NEW_CONF} does not exist in file cfg.txt"     
		  echo "No removing configuration"
		  echo " "
        else
		  rm -rf ${CONFIG_DIR}/${NEW_CONF}
		  sed -e "/${NEW_CONF} /d"  ${CONFIG_DIR}/cfg.txt >  ${CONFIG_DIR}/cfg.tmp
		  mv  ${CONFIG_DIR}/cfg.tmp  ${CONFIG_DIR}/cfg.txt
		  echo "${NEW_CONF} configuration REMOVED" 
        fi
    else
	   echo " "
	   echo "nothing to remove"
    fi
 fi 
 unset -v answer
