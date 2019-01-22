mydir=`pwd`
dirnam="/alphadata01/bstocker/data/landuse_data/luh_rcp_hurtt"
cd $dirnam

list=`cat yrlist.txt`
for idx in $list
do
    let year=$idx
    echo "year $year"
    yr_string=`printf "%06i\n" ${year}`
    let index=$year-1499
    echo "index $index" 

    ## extract single time slice. 'year' corresponds to index in file as it is at annual resolution
    ncks -O -H -F -d TIME,${index} harvest_hurtt_byarea_v2_historical_1500-2004_halfdeg.nc harvest_hurtt_byarea_v2_halfdeg_${yr_string}.nc
    ncks -O --mk_rec_dmn TIME harvest_hurtt_byarea_v2_halfdeg_${yr_string}.nc harvest_hurtt_byarea_v2_halfdeg_${yr_string}.nc
    
done

#ncrcat -O harvest_hurtt_byarea_v2_halfdeg_??????.nc harvest_hurtt_byarea_v2_halfdeg_hydeslices.nc



cd $mydir