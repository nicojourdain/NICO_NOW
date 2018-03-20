#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ==================
# Fcheck_archfile.sh
# ==================
#
# --------------------------
# Check the compilation file
# --------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fcheck_archfile.sh
#
#
# DESCRIPTION
# ===========
#
#
# Check the choice of the compiler.
# Three cases :  
#
# - There was a previous choice
# - A new one has be specified, we use this one
# - No information, exit
#
# We use TOOLS/COMPILE/arch.fcm to see if something was chosen. 
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fcheck_archfile.sh ARCHFILE COMPILER
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
# $Id: Fcheck_archfile.sh 3294 2012-01-28 16:44:18Z rblod $
#
#
#
#   * creation
#
#-

if [ ${#2} -eq 0 ]; then
   if [ ! -f ${COMPIL_DIR}/$1 ]; then
   echo "Warning !!!"
   echo "NO compiler chosen"
   echo "Try makenemo -h for help"
   echo "EXITING..."
   exit 1 
   fi
else
   myfile=$( find ${MAIN_DIR}/ARCH -name arch-${2}.fcm -print )
   if [ ${#myfile} -gt 0 ]; then
      ln -sf  ${myfile} ${COMPIL_DIR}/$1
   else
      echo "Warning !!!"
      echo "Compiler not existing"
      echo "Try makenemo -h for help"
      echo "EXITING..."
      exit 1       
   fi   
fi
