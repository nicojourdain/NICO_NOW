#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ============
# Fmake_bld.sh
# ============
#
# --------------------
# Make build directory
# --------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fmake_bld.sh
#
#
# DESCRIPTION
# ===========
#
#
# Under CONFIG_NAME :
# - Make the build directory 
# - Create repositories needed :
# - BLD for compilation 
#
# A tmpdir can be specified for memory issues.
#
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fmake_bld.sh NEMOGCM/CONFIG GYRE  /usr/tmp
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
# $Id: Fmake_bld.sh 3294 2012-01-28 16:44:18Z rblod $
#
#
#
#   * creation
#
#-
[ ! -d ${3}/${2} ] && \mkdir   ${3}/${2}
[ ! -d ${3}/${2}/BLD ] && \mkdir  ${3}/${2}/BLD
[ ! -d ${1}/${2}/BLD ]   && ln -sf ${3}/${2}/BLD ${1}/${2}/BLD
[ -f ${1}/${NEW_CONF}/cpp_${NEW_CONF}.fcm ] && ln -sf ${1}/${NEW_CONF}/cpp_${NEW_CONF}.fcm ${COMPIL_DIR}/cpp.fcm
rm -f  ${1}/${NEW_CONF}/BLD/fcm.bld.lock
