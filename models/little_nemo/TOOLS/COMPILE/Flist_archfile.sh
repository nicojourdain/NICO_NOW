#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ==================
# Flist_archfile.sh
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
#  $ Flist_archfile.sh Institute
#
#
# DESCRIPTION
# ===========
#
#
# List arch file available.
# The first line of each file in NEMO/ARCH directory is echoed.  
#
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Flist_archfile.sh 
#
#  $ ./Flist_archfile.sh CNRS
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
# $Id: Flist_archfile.sh 3294 2012-01-28 16:44:18Z rblod $
#
#
#
#   * creation
#
#-

if [ -n "$2" ]; then
shift
fi

echo "Available compilers for -m option :"
for file in  $(ls ${MAIN_DIR}/ARCH | grep fcm )
do
zvar1=${file#arch-}
zvar2=$(head -1 ${MAIN_DIR}/ARCH/$file)
#echo "${zvar1%.fcm} : ${zvar2#\#}"
printf "%-20s %s %-s\n" "${zvar1%.fcm}" : "${zvar2#\#}"
done


if [ "$1" == "all" ]; then
   for dir  in  $(ls ${MAIN_DIR}/ARCH | grep -v fcm )
   do 
      echo "Available compilers at ${dir} :"
      for file in  $(ls ${MAIN_DIR}/ARCH/${dir} | grep  fcm )
      do
      zvar1=${file#arch-}
      zvar2=$(head -1 ${MAIN_DIR}/ARCH/${dir}/$file)
      #echo "${zvar1%.fcm} : ${zvar2#\#}"
      printf "%-20s %s %-s\n" "${zvar1%.fcm}" : "${zvar2#\#}"
      done
   done
elif [ -d ${MAIN_DIR}/ARCH/${1} ]; then
      echo "Available compilers at $1 :"
      for file in  $(ls ${MAIN_DIR}/ARCH/$1 | grep fcm )
      do
      zvar1=${file#arch-}
      zvar2=$(head -1 ${MAIN_DIR}/ARCH/${1}/$file)
      #echo "${zvar1%.fcm} : ${zvar2#\#}"
      printf "%-20s %s %-s\n" "${zvar1%.fcm}" : "${zvar2#\#}"
      done
else
      echo "Available consortium member sub-directories :"
      for dir  in  $(ls ${MAIN_DIR}/ARCH | grep -v fcm )
      do
         echo ${dir}
      done
      echo "use \"makenemo -h all\" or \"makenemo -m help\" to see compilers available in member's sub-directories"
fi
