#!/bin/bash

# ///////////////////////////////////////////////////////////////////////////////////////
# Downloads HYDE2013 files used for GCP runs and converts to NetCDF using Ferret and NCO.
# beni@climate.unibe.ch
# ---------------------------------------------------------------------------------------
# mydir=`pwd`
# scen="hyde31_upper"
# era="BC AD"

# ## Loop over the three (best,lower,upper)
# for iscen in $scen
# do
#     echo "*********************************************************************"
#     echo "SCENARIO ${iscen}"
#     echo "---------------------------------------------------------------------"
            
#     ## Create directory to store files
#     dirnam="/alphadata01/bstocker/data/landuse_data/hyde3_1/${iscen}"
#     if [ ! -e $dirnam ]
#       then
#       echo "creating subdirectory structure $dirnam"
#       mkdir -p $dirnam
#     fi
    
#     ## Define host address
#     host="ftp://ftp.pbl.nl/hyde/${iscen}/"

#     ## Get list of files on FTP server
#     lftp -f get_filelist_${iscen}.lftp > ${dirnam}/filelist_${iscen}.txt

#     ## download readme file
#     wget ${host}hyde_readme_ftp.txt

#     cd ${dirnam}

#     ## Loop over files in list
#     rm yrlist.txt
#     for jdx in $era
#     do
#         if [ $jdx == "BC" ]
#         then
#             let mult=-1
#             smallera="bc"
#         else
#             let mult=1
#             smallera="ad"
#         fi
        
#         ## get only files with pattern '_lu' in file name
#         #list=`cat filelist_${iscen}.txt|grep _lu|grep $jdx`
#         #list=`cat filelist_${iscen}.txt|grep _pop|grep $smallera` # hyde31_final
#         list=`cat filelist_${iscen}.txt|grep _pop|grep $jdx` # hyde31_upper

#         echo $list
        
#         for idx in $list
#         do
#             echo "/////////////////////////////////////////////////////////////////////"
#             echo "downloading ${host}${idx} ..."
#             echo "---------------------------------------------------------------------"
#             #wget ${host}${idx}

#             ## Unzip the file
#             #unzip ${idx}

#             ## Get year from file name
#             #echo "idx = $idx"
#             #echo "jdx = $jdx"
#             #echo "smallera = $smallera"
#             if [ $smallera == "bc" ]
#             then
#                 #let pos=`echo $idx | awk '{print index( $0, "bc" )}'` hyde31_final
#                 let pos=`echo $idx | awk '{print index( $0, "BC" )}'` #hyde31_upper
#             else
#                 #let pos=`echo $idx | awk '{print index( $0, "ad" )}'` hyde31_final
#                 let pos=`echo $idx | awk '{print index( $0, "AD" )}'` #hyde31_upper
#             fi
#             let pos=${pos}-3
#             #echo "pos: $pos"
#             let yrabs=${idx:2:${pos}}
#             let yr=${mult}*${yrabs}
#             #echo "yr: $yr"

#             if [ $jdx == "BC" ]
#             then
#                 argyr="${yrabs}BC"
#             else
#                 argyr="${yrabs}AD"
#             fi
#             yr_string=`printf "%06i\n" ${yr}` 
#             echo "${yr_string}"
#             echo ${yr}>>yrlist.txt

#             #echo "yr:    ${yr}"
#             #echo "argyr: ${argyr}"
            
#             ## Convert ascii to NetCDF (single annual fields)
# # /opt/local/ferret/bin/ferret <<EOF    
# # go "../asc2cdf_hyde31_pop.jnl" ${yr} ${argyr}
# # quit
# # EOF
# # #         rm tmp*.nc


#         done
        
#     done

#     # cat yrlist.txt
#     cd $mydir
#     # cat filelist_${iscen}.out
   
# done

## Regrid using the R function (loop over scenarios inside R script!!!)
#R CMD BATCH --no-save --no-restore regrid_landuse_hyde.R regrid_landuse_hyde.out
#R CMD BATCH --no-save --no-restore regrid_population_hyde31.R regrid_population_hyde31.out

scen="upper"
res="halfdeg"
mydir=`pwd`

## Combine annual fields along time axis

for iscen in $scen
do

    ## cd to data directory
    dirnam="/alphadata01/bstocker/data/landuse_data/hyde3_1/hyde31_${iscen}"
    cd $dirnam

    for ires in ${res}
    do
    
        # ## Make 'time' a record dimension in all multi-decadal files
        echo "concatenate files along time axis..."
        #list=`ls -tr landuse_hyde31_${iscen}_${ires}_0*.cdf`
        list=`ls -tr population_hyde31_${iscen}_${ires}_*.cdf`
        k=0
        for idx in $list
        do
            echo $idx
            let k=k+1
            tmpn=tmp_`printf "%02d" $k`.nc
            echo "cmd: ncks -O --mk_rec_dmn TIME ${idx} ${tmpn}"
            ncks -O --mk_rec_dmn TIME ${idx} ${tmpn}
        done
        
        ## Concatenate all multi-decadal files to single file for historical period
        ncrcat -O tmp*.nc population_hyde31_${iscen}_${ires}.cdf
        #rm tmp*.nc

    done

    cd $mydir

done

## modify attributes
#./prepare_landuse_hyde2013.sh

