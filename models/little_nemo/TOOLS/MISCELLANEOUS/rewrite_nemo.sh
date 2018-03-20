#!/bin/bash
#
set -u
#set -xv
#
# for on each file containing a call to work alloc (exept BDY files that are too complicated...)
#for i in $( ack -il "^ *call *wrk_alloc *\(" | grep -v BDY )
for i in $( egrep -iRl "^ *call *wrk_alloc *\(" * | grep "90$" | grep -v BDY )
do
# create a temporary file that will be easier to process...
    sed -e "s/dimension/DIMENSION/" -e "s/pointer/POINTER/" $i > tmp$$
# replace "dimension(...), pointer" to "pointer, dimension(...)"
    sed -e "s/, *DIMENSION *(\([^)]*\)) *, *POINTER/, POINTER, DIMENSION(\1)/" tmp$$ > tmp2$$
    mv tmp2$$ tmp$$ 
# remove all comments in $i
    sed -e "s/\!.*//" tmp$$ > tmp2$$
    mv tmp2$$ tmp$$
# numbers of the lines defining the beginning and the end of each subroutine or functions
    n1=$( egrep -ic "(^ *subroutine *[0-9a-zA-Z_]|function *[0-9a-zA-Z_][0-9a-zA-Z_]* *\(.*\))" tmp$$ )
    n2=$( egrep -ic "(^ *end *subroutine|^ *end *function)" tmp$$ )
    if [ $n1 -ne $n2 ]
    then
	echo "error with the definition of subroutine/function blocks in $i" 
	exit
    fi
    linesbegin=$( egrep -in "(^ *subroutine *[0-9a-zA-Z_]|function *[0-9a-zA-Z_][0-9a-zA-Z_]* *\(.*\))" tmp$$ | sed -e "s/:.*//" )
    linesend=$( egrep -in "(^ *end *subroutine|^ *end *function)" tmp$$ | sed -e "s/:.*//" | sort -rn )
#
# number of the lines containing wrk_alloc
    cnt=$( grep -ci "^ *call *wrk_alloc *(" tmp$$ )
# for each of these lines
    ll=1
    while [ $ll -le $cnt ]
    do
# get the line with its number
	line=$( grep -in "^ *call *wrk_alloc *(" tmp$$ | sed -n ${ll}p | sed -e "s/\!.*//" )
# get its number
	lline=$( echo $line | sed -e "s/:.*//" )
# keep only the arument of wrk_alloc between ()
	line=$( echo $line | sed -e "s/[^(]*\((.*)\).*/\1/" | sed -e "s/, *k[ijkl]start *=[^,]*,/,/" | sed -e "s/, *k[ijkl]start *=.*)/ )/" )
# find in which subroutine or function is located this call to wrk_alloc: l1 beginning l2: end
	for lll in $linesbegin
	do
	    [ $lline -gt $lll ] && l1=$lll
	done
	for lll in $linesend
	do
	    [ $lline -lt $lll ] && l2=$lll
	done
	if [ $l2 -le $l1 ]
	then
	    echo "error in $i in the indentification subroutine/function block containing line $lline"
	    exit
	fi
# get the last argument of the call to wrk_alloc (between the last "," and ")"
	lw=$( echo $line | sed -e "s/.*,\(.*\)).*/\1/" | sed -e "s/ *//g" )
# get the line containing the definition of this argument and from it get the number of dimension of the argument
	tst=$( sed -n ${l1},${l2}p tmp$$  | grep -i "POINTER *, *DIMENSION *(.*) *::.*"${lw} | egrep -ic "[^0-9a-zA-Z_]("${lw}" *$|"${lw}" *,)" )
	if [ $tst -ne 1 ]
	then
	    echo "error with ${lw} definition in $i" 
	    exit
	fi
	dimsz=$( sed -n ${l1},${l2}p tmp$$  | grep -i "POINTER *, *DIMENSION *(.*) *::.*"${lw} | egrep -i "[^0-9a-zA-Z_]("${lw}" *$|"${lw}" *,)" | sed -e "s/.*POINTER *, *DIMENSION *(\(.*\)) *::.*/\1/" | sed -e "s/ //g" | wc -c )
	dimsz=$(( $dimsz / 2 ))
# get the number of variables concerned by the call to wrk_alloc (keep anly the , and count them)
	nvar=$( echo $line | sed -e "s/[^,]//g" | wc -c )
	nvar=$(( $nvar - $dimsz ))
# get the définition of the size of the dimensions from the call to wrk_alloc 
	case $dimsz in
	    1) dim=$( echo $line | sed -e "s/( *\([^,]*\),.*)/\1/" | sed -e "s/ //g" ) ;; # keep between ( and the 1st ,
	    2) dim=$( echo $line | sed -e "s/( *\([^,]*,[^,]*\),.*)/\1/" | sed -e "s/ //g" ) ;; # keep between ( and the 2nd ,
	    3) dim=$( echo $line | sed -e "s/( *\([^,]*,[^,]*,[^,]*\),.*)/\1/" | sed -e "s/ //g" ) ;; # keep between ( and the 3rd ,
	    4) dim=$( echo $line | sed -e "s/( *\([^,]*,[^,]*,[^,]*,[^,]*\),.*)/\1/" | sed -e "s/ //g" ) ;; # keep between ( and the 4th ,
	    *) echo "error with the number of dimensions of $lw in $i"; exit ;;
	esac
# for each variable, change its definition...
	nn=1 
	var=$lw # start from the last argument
	while [ $nn -le $nvar ]
	do
# get the line where this variable is defined.	
	    tst=$( sed -n ${l1},${l2}p tmp$$  | grep -ni "POINTER *, *DIMENSION *(.*) *::.*"${var} | egrep -ic "[^0-9a-zA-Z_]("${var}" *$|"${var}" *,)" )
	    if [ $tst -ne 1 ]
	    then
		echo "error with ${lw} definition in $i" 
		exit
	    fi
# get the line number where the variable is defined
	    lvar=$( sed -n ${l1},${l2}p tmp$$  | grep -ni "POINTER *, *DIMENSION *(.*) *::.*"${var} | egrep -i "[^0-9a-zA-Z_]("${var}" *$|"${var}" *,)" | sed -e "s/:.*//" )
	    lvar=$(( $lvar + $l1 - 1 ))
# get the dimension definition and compare it to ${dim}
	    dim2=$( sed -n -e "${lvar}p" $i | sed -e "s/dimension/DIMENSION/" -e "s/\!.*//" | sed -e "s/.*, *DIMENSION *(\(.*\)).*/\1/" | sed -e "s/ //g" )
	    if [ $( echo $dim2 | wc -c ) -ne $(( $dimsz * 2 )) ] 
	    then 
		if [ "$dim2" != "$dim" ] 
		then 
		    echo "problem in $i in the defininition of variable $var: its size is $dim but we have $dim2"
		    sed -n -e "${lvar}p" $i
		    exit
		fi
	    fi
# remove pointer
	    sed -e "${lvar}s/pointer/POINTER/" $i | sed -e "${lvar}s/, *POINTER *,/,/" | sed -e "${lvar}s/, *POINTER */ /" > tmp2$$
	    mv tmp2$$ $i
# redefine the dimensions 
	    case $dimsz in
		1) sed -e "${lvar}s/\(DIMENSION *(\) *: *)/\1${dim})/" $i > tmp2$$ ;;
		2) sed -e "${lvar}s/\(DIMENSION *(\) *: *, *: *)/\1${dim})/" $i > tmp2$$ ;;
		3) sed -e "${lvar}s/\(DIMENSION *(\) *: *, *: *, *: *)/\1${dim})/" $i > tmp2$$ ;;
		4) sed -e "${lvar}s/\(DIMENSION *(\) *: *, *: *, *: *, *: *)/\1${dim})/" $i > tmp2$$ ;;
	    esac
	    mv tmp2$$ $i
	    
# define the next variable (previous argument)
	    var=$( echo $line | sed -e "s/.*,\(.*\), *[^0-9a-zA-Z_]*${var}[^0-9a-zA-Z_]*[,)].*/\1/" | sed -e "s/ *//g" )
	    nn=$(( $nn + 1 ))
	done
	
	ll=$(( $ll + 1 ))
    done
#delete wrk_alloc line
    sed -e "/call *wrk_alloc *(/d" -e "/CALL *wrk_alloc *(/d" \
	-e "/call *wrk_dealloc *(/d" -e "/CALL *wrk_dealloc *(/d" \
	-e "/USE wrk_nemo/d"  -e "/use wrk_nemo/d" $i > tmp$$
    mv tmp$$ $i
    
    echo $i done
done
#
# some specific changes...
#
# OPA_SRC/SBC/albedo.F90
sed -e "s/DIMENSION(jpi,jpj,ijpl)/DIMENSION(jpi,jpj,SIZE(pt_ice,3))/" OPA_SRC/SBC/albedo.F90 > tmp$$
mv tmp$$ OPA_SRC/SBC/albedo.F90
# LIM_SRC_2/limrhg_2.F90
sed -e "s/DIMENSION(jpi,jpj+2)/DIMENSION(jpi,0:jpj+1)/" LIM_SRC_2/limrhg_2.F90 > tmp$$
mv tmp$$ LIM_SRC_2/limrhg_2.F90
# LIM_SRC_3/limitd_me.F90
sed -e "s/DIMENSION(jpi,jpj,jpl+2)/DIMENSION(jpi,jpj,-1:jpl)/" LIM_SRC_3/limitd_me.F90 > tmp$$
mv tmp$$ LIM_SRC_3/limitd_me.F90
# LIM_SRC_3/limitd_th.F90
sed -e "s/DIMENSION(jpi,jpj,jpl+1)/DIMENSION(jpi,jpj,0:jpl)/" LIM_SRC_3/limitd_th.F90 > tmp$$
mv tmp$$ LIM_SRC_3/limitd_th.F90
# LIM_SRC_3/limthd_dif.F90
sed -e "s/DIMENSION(kiut,nlay_i+1)/DIMENSION(kiut,0:nlay_i)/" \
    -e "s/DIMENSION(kiut,nlay_s+1)/DIMENSION(kiut,0:nlay_s)/" LIM_SRC_3/limthd_dif.F90 > tmp$$
mv tmp$$ LIM_SRC_3/limthd_dif.F90
# LIM_SRC_3/limthd_ent.F90
sed -e "s/DIMENSION(jpij,jkmax+4)/DIMENSION(jpij,0:jkmax+3)/" \
    -e "s/DIMENSION(jkmax+4,jkmax+4)/DIMENSION(0:jkmax+3,0:jkmax+3)/" LIM_SRC_3/limthd_ent.F90 > tmp$$
mv tmp$$ LIM_SRC_3/limthd_ent.F90
# OPA_SRC/DYN/divcur.F90
sed -e "s/DIMENSION(jpi+4,jpj)/DIMENSION(-1:jpi+2,jpj)/" OPA_SRC/DYN/divcur.F90 > tmp$$
mv tmp$$ OPA_SRC/DYN/divcur.F90
# OPA_SRC/LDF/ldfslp.F90
sed -e "s/DIMENSION(jpi,jpj,jpk,2)/DIMENSION(jpi,jpj,jpk,0:1)/" \
    -e "s/DIMENSION(jpi,jpj,2,2)/DIMENSION(jpi,jpj,0:1,0:1)/" OPA_SRC/LDF/ldfslp.F90 > tmp$$
mv tmp$$ OPA_SRC/LDF/ldfslp.F90
# OPA_SRC/ZDF/zdfkpp.F90
sed -e "s/DIMENSION(jpi,3) *::* zmoek/DIMENSION(jpi,0:2) ::   zmoek/" OPA_SRC/ZDF/zdfkpp.F90 > tmp$$
mv tmp$$ OPA_SRC/ZDF/zdfkpp.F90

# link for limrhg.F90...
cd LIM_SRC_2
ln -sf ../LIM_SRC_3/limrhg.F90 .
