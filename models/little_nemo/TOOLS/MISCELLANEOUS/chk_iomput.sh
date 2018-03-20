#!/bin/bash
#------------------------------------------------
#$Id: chk_iomput.sh 2404 2010-11-17 21:03:13Z smasson $
#------------------------------------------------
#
set -u
#
# if not argument -> get the help
[ $# -eq 0 ] && ./$0 --help && exit
#
inxml=0
insrc=0
while [ $# -gt 0 ]   # Until you run out of parameters . . . 
do    
    case "$1" in 
	-h|--help)
	    echo
	    echo 'Description:'
	    echo '      check that an xml file is coherant with the source code:'
	    echo '         - all variable ids defined by "call iom_put" must have their counterpart'
	    echo '           in the variable definition in xml file'
	    echo '         - list variable ids defined in xml file without any corresponding call'
	    echo '           to iom_put. This can be done but it is useless as iom will only ouput zeros'
	    echo '         - all variable ids used in the files definition in xml file must have'
	    echo '           their counterpart in the variable definition in xml file'
	    echo 'Usage:'
	    echo '      chk_iomput.sh [OPTION]'
	    echo '  or  chk_iomput.sh [OPTION] xmlfile DIRECTORIES'
	    echo '         with:'
	    echo '           xmlfile:  the xml file to test'
	    echo '           DIRECTORIES: a list of directories containing the source code'
	    echo 'Options'
	    echo ' -h, --help           to get this help'
	    echo ' --inxml              only print all variable definitions found in the xml file'
	    echo ' --insrc              only print all variable definitions found in the source code'
	    echo 'Examples'
	    echo '      chk_iomput.sh'
	    echo '      chk_iomput.sh --help'
	    echo '      chk_iomput.sh ../../CONFIG/ORCA2_LIM/EXP00/iodef.xml "../../NEMO/OPA_SRC/ ../../NEMO/LIM_SRC_2/"'
	    echo
	    exit ;;
        --inxml) inxml=1 ;;
        --insrc) insrc=1 ;;
	-*) echo ; echo "illegal option" ; ./$0 --help && exit ;;
	*) [ $# -ne 2 ] && echo && echo "wrong number of arguments" && ./$0 --help && exit
	    xmlfile=${1}
	    srcdir=${2}
	   shift
    esac
    shift       # Check next set of parameters. 
done 
#
[ ! -f "$xmlfile" ] && echo "$xmlfile not found, we stop..." && exit
for i in $srcdir 
do
    [ ! -d $i ] && echo "$i is not a directory, we stop..." && exit
done
#
#------------------------------------------------
#
[ $inxml -eq 1 ] && grep "< *field * id *=" $xmlfile
[ $insrc -eq 1 ] && find $srcdir -name "*.[Ffh]90" -exec grep -iH "^[^\!]*call  *iom_put *(" {} \;
[ $(( $insrc + $inxml )) -ge 1 ] && exit
#
#------------------------------------------------
#
# list of file containing "CALL iom_put" in $srcdir
#
srclist=$( find $srcdir -name "*.[Ffh]90" -exec grep -il "^[^\!]*call  *iom_put *(" {} \; )
#
# list of variables used in "CALL iom_put"
#
varlistsrc=$( find $srcdir -name "*.[Ffh]90" -exec grep -i  "^[^\!]*call  *iom_put *(" {} \; | sed -e "s/.*iom_put *( *[\"\']\([^\"\']*\)[\"\'] *,.*/\1/" | sort -d )
#
# list of variables defined in the xml file
#
varlistxml=$( grep "< *field * id *=" $xmlfile  | sed -e "s/^.*< *field * id *= *[\"\']\([^\"\']*\)[\"\'].*/\1/" | sort -d )
#
# list of variables to be outputed in the xml file
#
varlistout=$( grep "< *field * ref *=" $xmlfile  | sed -e "s/^.*< *field * ref *= *[\"\']\([^\"\']*\)[\"\'].*/\1/" | sort -d )
#
echo "--------------------------------------------------"
echo  check if all iom_put found in $srcdir
echo  have a corresponding variable definition in $xmlfile
echo "--------------------------------------------------"
for var in $varlistsrc
do
    tst=$( echo " "$varlistxml" " | grep -c " $var " )
    if [ $tst -ne 1 ] 
    then
	echo "problem with $var: $tst lines corresponding to its definition in $xmlfile, but defined in the code in"
	for f in $srclist
	do
	    grep -iH "^[^\!]*call  *iom_put *( *[\"\']${var}[\"\'] *," $f
	done
	echo
    fi
done
#
echo "--------------------------------------------------"
echo  check if all variables defined in $xmlfile
echo  have a corresponding \"call iom_put\" in sources found in $srcdir
echo "--------------------------------------------------"
#
for var in $varlistxml
do
    found=$( echo " "$varlistsrc" " | grep -c " $var " )
    if [ $found -eq 0 ] 
    then
	echo \"call iom_put\" not found for variable $var
	grep "< *field * id *= *[\"\']${var}[\"\']" $xmlfile
	echo
    fi
done
#
echo "--------------------------------------------------"
echo  ${xmlfile}: check if all variables to be outputed in files are really defined...
echo "--------------------------------------------------"
#
# list of variables defined in the xml file
for var in $varlistout
do
    found=$( echo " "$varlistxml" " | grep -c " $var " )
    [ $found -eq 0 ] && echo variable to be outputed but not defined: $var 
done



exit
