#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ==============
# Fprep_agrif.sh
# ==============
#
# ---------------------
# Preparation for AGRIF
# ---------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fprep_agrif.sh
#
#
# DESCRIPTION
# ===========
#
#
# Prepare directories for AGRIF and copy files needed
#
# Compile the conv
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fprep_agrif.sh CONFIG_NAME
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
# $Id: Fprep_agrif.sh 3294 2012-01-28 16:44:18Z rblod $
#
#
#
#   * creation
#
#-

#- AGRIF conv
if [ "$AGRIFUSE" == 1 ]; then

#- CONV
#fcm build ${TOOLS_DIR}/conv.cfg || exit 1
gmake -C ${MAIN_DIR}/EXTERNAL/AGRIF/LIB

#- AGRIF sources
[ ! -d $2/$1/OPAFILES ] && mkdir  $2/$1/OPAFILES
[ ! -d $2/$1/OPAFILES/AGRIF_INC ] && mkdir  $2/$1/OPAFILES/AGRIF_INC
[ ! -d $2/$1/OPAFILES/AGRIF_MODELFILES ] && mkdir  $2/$1/OPAFILES/AGRIF_MODELFILES
cp -f -r ${MAIN_DIR}/EXTERNAL/AGRIF/agrif_opa.in  $2/$1/OPAFILES/
cp -f -r ${MAIN_DIR}/EXTERNAL/AGRIF/conv  $2/$1/OPAFILES/

fi
