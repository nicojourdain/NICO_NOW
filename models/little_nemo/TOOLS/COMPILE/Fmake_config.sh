#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ===============
# Fmake_config.sh
# ===============
#
# ---------------
# Make the config 
# ---------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fmake_config.sh
#
#
# DESCRIPTION
# ===========
#
#
# - Make the config directory 
# - Create repositories needed :
#  
#  - EXP00 for namelist
#  - MY_SRC for user sources
#  - BLD for compilation 
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fmake_config.sh CONFIG_NAME REF_CONFIG_NAME 
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
# $Id: Fmake_config.sh 3294 2012-01-28 16:44:18Z rblod $
#
#
#
#   * creation
#
#-
\mkdir  ${1}
\mkdir  ${1}/EXP00
\mkdir  ${1}/MY_SRC
\cp -R  ${2}/cpp_${2}.fcm ${1}/cpp_${1}.fcm
\cp -R  ${2}/EXP00/*namelist* ${1}/EXP00/.
[ -f ${2}/EXP00/AGRIF_FixedGrids.in ] &&  \cp -R  ${2}/EXP00/AGRIF_FixedGrids.in ${1}/EXP00/.
[ -f ${2}/EXP00/iodef.xml ] &&  \cp -R  ${2}/EXP00/iodef.xml ${1}/EXP00/.
[ -f ${2}/EXP00/xmlio_server.def ] &&  \cp -R  ${2}/EXP00/xmlio_server.def ${1}/EXP00/.
[ -d    ${2}/MY_SRC ] && \cp  ${2}/MY_SRC/* ${1}/MY_SRC/. 2> /dev/null
