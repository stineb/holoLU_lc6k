#!/bin/bash

scen="hyde31_concave"

for iscen in $scen
do

    ## move to subdirectory
    dirnam="/alphadata01/bstocker/data/landuse_data/hyde3_1/${iscen}"
    cd ${dirnam}

    list=`ls landuse_${iscen}_halfdeg.cdf`

    for idx in $list
    do
        ncatted -a units,crop,a,c,'area fraction' $idx
        ncatted -a units,past,a,c,'area fraction' $idx
        ncatted -a units,crop_abs,a,c,km^2 $idx
        ncatted -a units,past_abs,a,c,km^2 $idx
        
        ncatted -a long_name,crop,a,c,'cropland fraction' $idx
        ncatted -a long_name,past,a,c,'pasture fraction' $idx
        ncatted -a long_name,crop_abs,a,c,'cropland area' $idx
        ncatted -a long_name,past_abs,a,c,'pasture area' $idx
        
        ncatted -a history,global,o,c,"Original provided by K. Klein Goldewijk. Regridded by Benjamin Stocker, available on http://www.climate.unibe.ch/~bstocker/downloads.html " $idx
        ncatted -a title,global,o,c,"HYDE3.1, scenario $iscen landuse data" $idx
    
    done
done