#!/bin/bash
#
# purpose:
#   small script to check in all *90 files of the current directory and subdirectories 
#   if all lines with wrk_alloc have their corresponding lines with wrk_dealloc
#
# use:
#   call chk_wrk_alloc.sh from the directory you want to check
#
# example:
#   cd ~/dev_NEMO_MERGE_2011/NEMOGCM/NEMO
#   ../TOOLS/MISCELLANEOUS/chk_wrk_alloc.sh
#
set -ue
#
echo "check for all *90 files contained in "$( pwd )" and its subdirectories"
#
for ff in $( grep -il wrk_nemo_2 $( find . -name "*90" ) )
do

    # number of lines with wrk_alloc
    n1=$( grep -ic "call *wrk_alloc *(" $ff )  
    # replace wrk_alloc with wrk_dealloc and count the lines
    n2=$( sed -e "s/wrk_alloc/wrk_dealloc/" $ff | grep -ic "call *wrk_dealloc *(" )
    # we should get n2 = 2 * n1...
    [ $(( 2 * $n1 )) -ne $n2 ] && echo "problem with wrk_alloc in $ff" 
   
    # same story but for wrk_dealloc
    n1=$( grep -ic "call *wrk_dealloc *(" $ff )  
    n2=$( sed -e "s/wrk_dealloc/wrk_alloc/" $ff | grep -ic "call *wrk_alloc *(" )
    [ $(( 2 * $n1 )) -ne $n2 ] && echo "problem with wrk_dealloc in $ff" 
   
done
