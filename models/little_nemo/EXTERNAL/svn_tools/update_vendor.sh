#!/bin/bash
# N.B. run this script on EXTERNAL directory!!!

set -u

if [ $# -ne 4 ]; then
    echo "Usage: $0 vendor_branch nemo_vendor_old new username"
    echo "first  argument: vendor name (for example: IOIPSL)"
    echo "second argument: old tag name of the vendor in NEMO vendors deposit (for example: v2_1_9 or 53)"
    echo "third  argument: definition of the tag or the revision number of the vendor to be updated (for example: v2_2_1 or 114)"
    echo "fourth argument: user login name"
    echo
    echo "run this script on EXTERNAL/svn_tools directory"
    echo
    echo
    echo "examples"
    echo
    echo "./update_vendor.sh IOIPSL v2_1_9 v2_2_1 smasson"
    echo "./update_vendor.sh XMLF90 r_53 114 smasson"
    echo "./update_vendor.sh XMLIO_SERVER r_53 114 smasson"
    echo "./update_vendor.sh AGRIF r_00 1988 smasson"

   exit 1
fi

vendor_branch=$1
nemo_vendor_old=$2
new=$3
username=$4

#-
#-----------------------------------------------------------------------------------
# path definitions... could need to be changed...
#-----------------------------------------------------------------------------------
#-

#- My projet url
svn_nemo_vendor=svn+ssh://$username@forge.ipsl.jussieu.fr/ipsl/forge/projets/nemo/svn/vendors/$vendor_branch

#- EXTERNAL directory in which we want to merge vendors
svn_external=svn+ssh://$username@forge.ipsl.jussieu.fr/ipsl/forge/projets/nemo/svn/branches/DEV_r1879_FCM/NEMOGCM/EXTERNAL

#- Vendor project original url
case $vendor_branch in
    IOIPSL) vendor_url=http://forge.ipsl.jussieu.fr/igcmg/svn/$vendor_branch/tags ;;
    AGRIF) vendor_url=http://forge.ipsl.jussieu.fr/nemo/svn/trunk/$vendor_branch ;;
    XMLF90) vendor_url=http://forge.ipsl.jussieu.fr/ioserver/svn/$vendor_branch ;;
    XMLIO_SERVER) vendor_url=http://forge.ipsl.jussieu.fr/ioserver/svn/XMLIO_SERVER/trunk ;;
    fcm) echo "url of fcm undefined..." ; exit 2 ;;
    *)  echo "wrong definition of the vendor name..." ; exit 3 ;;
esac

#- get a tag or a revision??
if [ "$vendor_branch" = "IOIPSL" ] 
then
    rev_or_tag="tag"
else
    rev_or_tag="rev"
fi

#-
#-----------------------------------------------------------------------------------
# end of path definitions...
#-----------------------------------------------------------------------------------
#-
echo
echo "########################################################"
echo "#                                                      #"
echo "#  run this script in EXTERNAL/svn_tools directory!!!  #"
echo "#                                                      #"
echo "########################################################"
echo

#- checkout new vendor version without the .svn directories (-> use export instead of checkout) and put it in a working_directory
echo
echo "Downloading $vendor_branch tag $new into working_directory..."
echo
[[ ( -d working_directory ) || ( -f working_directory ) ]] && rm -rf working_directory # cleaning
if [ "$rev_or_tag" = "tag" ] 
then
    svn export $vendor_url/$new working_directory      # specify the tag path
else
    svn export -r $new $vendor_url working_directory   # specify the revision number
fi

#- update the vendor deposit:
#- 1) create a clean version of the current version (take care od added and removed file) and commit it
#- 2) tag it with a new name $nemo_vendor_new
if [ "$rev_or_tag" = "tag" ] 
then
    nemo_vendor_new=$new
else
    nemo_vendor_new=r_$new
fi
echo
echo "updating $vendor_branch/current and tagging this new version as $vendor_branch/$nemo_vendor_new"
echo
./svn_load_dirs.pl -t $nemo_vendor_new $svn_nemo_vendor current working_directory
#- 
#- merge old and new version of $vendor_branch in $svn_external/$vendor_branch
#-
echo
echo "##############################################"
echo
echo "merge old and new version of $vendor_branch in $svn_external/$vendor_branch"
echo "if everything is ok you should execute the folling command:"
echo
echo "svn merge $svn_nemo_vendor/$nemo_vendor_old $svn_nemo_vendor/current ../$vendor_branch"

echo
echo
echo "##############################################"
echo
echo "Commiting the merge of $vendor_branch changes toward $svn_external/$vendor_branch"
echo "if everything is ok you should execute the folling command:"
echo
echo "svn ci --message \"merging $vendor_branch/$nemo_vendor_new into the EXTERNAL deposit\" ../$vendor_branch"
echo
echo

 
## NOTE : after a merge we have conflicts
## to resole them it is necessary to do, for example : svn resolved IOIPSL/directory with conflits

## NOTE : if you have a good merge then you have to committ:
## cd $tmp_dir/IOIPSL
## svn status ../IOIPSL
## svn ci --username $username --message "merge IOIPSL version $nemo_vendor_new"

exit 0

