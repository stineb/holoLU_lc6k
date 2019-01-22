#!/bin/bash

# ///////////////////////////////////////////////////////////////////////////////////////
# Downloads HYDE2013 files used for GCP runs and converts to NetCDF using Ferret and NCO.
# beni@climate.unibe.ch
# ---------------------------------------------------------------------------------------
mydir=`pwd`
scen="hyde31_final"
era="bc ad"

## Loop over the three (best,lower,upper)
for iscen in $scen
do
    echo "*********************************************************************"
    echo "SCENARIO ${iscen}"
    echo "---------------------------------------------------------------------"
            
    ## Create directory to store files
    dirnam="/alphadata01/bstocker/data/landuse_data/hyde3_1/${iscen}"
    if [ ! -e $dirnam ]
      then
      echo "creating subdirectory structure $dirnam"
      mkdir -p $dirnam
    fi
    
    ## Define host address
    host="ftp://ftp.pbl.nl/hyde/${iscen}/"

    ## Get list of files on FTP server
    lftp -f get_filelist_${iscen}.lftp > ${dirnam}/filelist_${iscen}.txt

    ## download readme file
    wget ${host}hyde_readme_ftp.txt

    cd ${dirnam}

    ## Loop over files in list
    rm yrlist.txt
    for jdx in $era
    do
        if [ $jdx == "bc" ]
        then
            let mult=-1
        else
            let mult=1            
        fi

        ## get only files with pattern '_lu' in file name
        list=`cat filelist_${iscen}.txt|grep _lu|grep $jdx`
        
        for idx in $list
        do
            echo "/////////////////////////////////////////////////////////////////////"
            echo "downloading ${host}${idx} ..."
            echo "---------------------------------------------------------------------"
            wget ${host}${idx}

            # ## Unzip the file
            # unzip ${idx}

            # ## Get year from file name
            # # echo $idx
            # if [ $jdx == "bc" ]
            # then
            #     let pos=`echo $idx | awk '{print index( $0, "bc" )}'`
            # else
            #     let pos=`echo $idx | awk '{print index( $0, "ad" )}'`
            # fi
            # let pos=${pos}-3
            # # echo $pos
            # let yr=${idx:2:${pos}}
            # let yr=${mult}*${yr}
            # # echo $yr
            # yr_string=`printf "%06i\n" ${yr}` 
            # # echo "${yr_string}"
            # echo ${yr}>>yrlist.txt

#         ## Convert ascii to NetCDF (single annual fields)
# /opt/local/ferret/bin/ferret <<EOF    
# go "../asc2cdf_hyde.jnl" ${yr}
# quit
# EOF
#         rm tmp*.nc


        done
        
    done

    # cat yrlist.txt
    cd $mydir
    # cat filelist_${iscen}.out
   
done

# # Regrid using the R function (loop over scenarios inside R script!!!)
# R CMD BATCH --no-save --no-restore regrid_landuse_hyde.R regrid_landuse_hyde.out

# scen="best upper lower"
# res="halfdeg 1x1deg"

# ## Combine annual fields along time axis

# for iscen in $scen
# do

#     ## move to subdirectory
#     mv landuse_hyde2013_${iscen}_${res}_*.cdf ${iscen}

#     cd ${iscen}

#     for ires in ${res}
#     do
    
#         ## Make 'time' a record dimension in all multi-decadal files
#         echo "concatenate files along time axis..."
#         list=`ls landuse_hyde2013_${iscen}_${ires}_*.cdf`
#         k=0
#         rm tmp*.nc
#         for idx in $list
#         do
#             echo $idx
#             let k=k+1
#             tmpn=tmp_`printf "%02d" $k`.nc
#             echo $tmpn
#             ncks -O --mk_rec_dmn TIME ${idx} ${tmpn}
#         done
        
#         ## Concatenate all multi-decadal files to single file for historical period
#         ncrcat -O tmp*.nc landuse_hyde2013_${iscen}_${ires}_1500-2013.cdf
#         rm tmp*.nc
#         mv landuse_hyde2013_${iscen}_${ires}_1500-2013.cdf ..

#     done

#     cd ..

# done

# ## modify attributes
# ./prepare_landuse_hyde2013.sh

