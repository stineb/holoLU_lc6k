
mydir=`pwd`
dirnam="/alphadata01/bstocker/data/landuse_data/kk10/"
cd $dirnam

list=`cat yrlist.txt`
for idx in $list
do
    let year=$idx
    echo "year $year"
    yr_string=`printf "%06i\n" ${year}`
    let index=$year+6050
    echo "index $index" 

    ## extract single time slice. 'year' corresponds to index in file as it is at annual resolution
    ncks -O -H -F -d time,${index} KK11_30m_8k_merge.nc landuse_KK11_halfdeg_${yr_string}.nc
    # ncks -O --mk_rec_dmn time landuse_KK11_halfdeg_${yr_string}.nc landuse_KK11_halfdeg_${yr_string}.nc

    #ncrcat -O landuse_KK11_halfdeg_*.nc landuse_KK11_halfdeg_hydeslices.cdf
    
done

cd $mydir