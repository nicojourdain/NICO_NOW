#!/bin/sh 
#+
#
# .. program:: std_main.sh
#
# ================
# std_main.sh
# ================
#
# -------------------------------------
# launch idl scripts to produce graphics diagnostics in Postscript, PDF or HTML document
# -------------------------------------
#
# SYNOPSIS
# ========
#
# ``std_main.sh -ts`` or ``std_main.sh -plot`` 
# 
# DESCRIPTION
# ===========
#
# .. option:: -ts    to produce time series 
# .. option:: -plot  to produce maps and sections
# .. option:: -html  to produce html document
# .. option:: -pdf   to produce pdf document
# .. option:: -noidl to skip the call to IDL and the production of Postscript
# .. option:: -vm    to use IDL virtual machine (free) instead of IDL
# .. option:: -help  to get help!
#
# variables have to be defined in std_plot_vardef.sh (or std_ts_vardef.sh)
# before calling std_main.sh -plot (or std_main.sh -ts)
#
# EXAMPLES
# ========
# $ ./std_main.sh -ts html
# $ ./std_main.sh -ts pdf -noidl
# 
# $ ./std_main.sh -plot -pdf -vm
#
# AUTHOR - date
# ===========
# Françoise Pinsard - 01/2010 - LOCEAN
# Simona Flavoni    - 01/2010 - LOCEAN
# Sebastien Masson  - 04/2011 - LOCEAN
#
#-----------------------------------------------------------
# Usage...
#-----------------------------------------------------------
#
#-
system=$(uname)
case "${system}" in
   AIX|IRIX64)
      echo " www : no specific posix checking"
   ;;
   *)
      set -o posix
   ;;
esac
#
usage="Usage: std_main.sh [OPTION]
Options
 -ts    to produce time series 
 -plot  to produce maps and sections
 -html  to produce html document
 -pdf   to produce pdf document
 -noidl to skip the call to IDL and the production of Postscript
 -vm    to use IDL virtual machine (free) instead of IDL
 -help  to get this help
"
#
vm=0
noidl=0
format=ps
while [ ! -z "${1}" ]
do
    case ${1} in
	-plot|--plot) PLOTTYPE=plot ;;
	-ts|--ts) PLOTTYPE=ts ;;
	-f|-format|--format) format=${2}	  shift	 ;;
	-html|--html) format=html ;;
	-pdf|--pdf) format=pdf ;;
	-h|-help|--help)	
	    echo "${usage}"
	    exit 0	
	    ;;
	-ni|--ni|-noidl|--noidl) noidl=1 ;;
	-vm|--vm|-virtual_machine|--virtual_machine) vm=1 ;;
	*) # other choice
	    echo "${usage}"
	    exit 1
	    ;;
    esac
    shift # next flag
done
#
PLOTTYPE=${PLOTTYPE:-NG}
if [[ ( "$PLOTTYPE" != "plot" ) && ( "$PLOTTYPE" != "ts" ) ]]
then
    echo 'the type of plot must be defined with the option -plot or -ts'
	exit 1
fi
export PLOTTYPE
#
set -u
#
#
tstexe () {
    type ${1}
    status_type=${?}
    if [ ${status_type} -ne 0 ]
    then
	echo "eee : ${2}"
	exit 1
    fi
}
#
#-----------------------------------------------------------
# define output directory for POSTCRIPT files
#-----------------------------------------------------------
#
. ./std_${PLOTTYPE}_vardef.sh
[ ! -d ${PS_DIR} ] && mkdir -p ${PS_DIR}
#
#-----------------------------------------------------------
# run IDL
#-----------------------------------------------------------
#
if [ $noidl -eq 0 ]
then 
    tstexe ${idl_command} "idl not found"
#
    if [ $vm -eq 1 ]
    then
	${idl_command} -vm=std_main.sav
    else 
	${idl_command} -IDL_STARTUP 'initenv' << EOF
std_${PLOTTYPE}_all, /postscript
EOF
    fi
    status_idl=${?}
    if [ ${status_idl} -ne 0 ]
    then
	echo "eee : error in the execution of IDL"
	exit 1
    fi
fi
#
#-----------------------------------------------------------
# produce the final document
#-----------------------------------------------------------
#
# build the list of ps that has been created by IDL
pslist=$( grep "img width" ${PS_DIR}/std_${PLOTTYPE}_html_body.txt | sed -e "s/.*src=\(.*\)png.*/\1/" )
#
case ${format} in
#__________________________________________________________
# PDF
    pdf)
# check if ps2pdf available
	ps2pdf_command=$( which ps2pdf )
	tstexe ${ps2pdf_command} "ps2pdf not found"
# check if texexec available
	texexec_command=$( which texexec )
	tstexe ${texexec_command} "texexec not found"
#
	filepdf=all_${PLOTTYPE}.pdf
	[ ! -d ${PDF_DIR} ] && mkdir -p ${PDF_DIR}
	pdflist=''
	for file in ${pslist} ; do
	    ps2pdf -sPAPERSIZE=a4 ${PS_DIR}/${file}ps ${PDF_DIR}/${file}pdf
	    echo "ps2pdf ${file}ps done"
	    pdflist=${pdflist}' '${PDF_DIR}/${file}pdf
	done
	texexec --pdfarrange --result=$PDF_DIR/$filepdf $pdflist
	rm -f $PDF_DIR/$( basename $filepdf .pdf ).aux $PDF_DIR/$( basename $filepdf .pdf ).log
	echo ${PDF_DIR}/$filepdf done
#commented because convert gives images of bad quality
#	convert ${PS_DIR}/*.ps /tmp/all_${PLOTTYPE}.pdf
#       convert -resize 800x600 ${PS_DIR}/${exp1}_${exp2}/ps/*.ps /tmp/all_${PLOTTYPE}.pdf
	;;
#__________________________________________________________
# HTML
    html)
	filehtml=all_${PLOTTYPE}.html
	[ ! -d ${HTML_DIR} ] && mkdir -p ${HTML_DIR}
# check if convert available
	convert_command=$( which convert )
	tstexe ${convert_command} "convert not found"
# convert each ps to png	
	for file in ${pslist} ; do
	    ${convert_command} -antialias ${PS_DIR}/${file}ps ${HTML_DIR}/${file}png
	done
# creation of the HTML file
	cat << EOF > ${HTML_DIR}/$filehtml
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
</head>
<body>
EOF
	cat ${PS_DIR}/std_${PLOTTYPE}_html_body.txt >>  ${HTML_DIR}/$filehtml
	cat << EOF >> ${HTML_DIR}/$filehtml
<hr>
</body>
</html>
EOF
	echo ${HTML_DIR}/$filehtml done
	;;
    ps)	;; # nothing to do...
    *)	
	echo " format ${format} not implemented"
	exit 1
	;;
esac

    #
#=====================
# prepare to put images on dods
# +++ to finish it
#LOGIN=xxxx
#Tag_Name=ORCA2_LIM2
#rsh ${LOGIN}@gaya.idris.fr exec /bin/ksh <<EOF
#       cd ${homegaya}/IGCM_OUT/${Tag_Name}/CORE2/INTERAN/${PLOTTYPE}_pdf/${exp1}
#       /usr/local/bin/dods_rm DODS/pub/${LOGIN}/${Tag_Name}/CORE2/INTERAN/${PLOTTYPE}_pdf/${exp1} > /dev/null 2>&1
#       /usr/local/bin/dods_cp ${exp1} DODS/pub/${LOGIN}/${Tag_Name}/CORE2/INTERAN/${PLOTTYPE}_pdf/${exp1} > /dev/null 2>&1
#EOF
#=====================
# end
exit 0
