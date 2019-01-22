#!/bin/bash

## copy to give new names
cp fluc_r1_holoLU2.nc data/fluc_hyde31_stocker17pnas.nc
cp fluc_r2_holoLU2.nc data/fluc_kk10_stocker17pnas.nc
cp fluc_r3_holoLU2.nc data/fluc_hyde31u_stocker17pnas.nc
cp fluc_r7_holoLU2.nc data/fluc_kk10d_stocker17pnas.nc
cp fluc_r8_holoLU2.nc data/fluc_hyde32_stocker17pnas.nc
cp fluc_r9_holoLU2.nc data/fluc_hyde32u_stocker17pnas.nc

cp luarea_r1_holoLU2.nc data/luarea_hyde31_stocker17pnas.nc
cp luarea_r2_holoLU2.nc data/luarea_kk10_stocker17pnas.nc
cp luarea_r3_holoLU2.nc data/luarea_hyde31u_stocker17pnas.nc
cp luarea_r7_holoLU2.nc data/luarea_kk10d_stocker17pnas.nc
cp luarea_r8_holoLU2.nc data/luarea_hyde32_stocker17pnas.nc
cp luarea_r9_holoLU2.nc data/luarea_hyde32u_stocker17pnas.nc

## attributes editing

## fluc files
list="data/fluc_hyde31_stocker17pnas.nc data/fluc_kk10_stocker17pnas.nc data/fluc_hyde31u_stocker17pnas.nc data/fluc_kk10d_stocker17pnas.nc data/fluc_hyde32_stocker17pnas.nc data/fluc_hyde32u_stocker17pnas.nc"

for idx in $list
do
    echo "adding attributes for variables/dimensions in file $idx ..."
    ncatted -O -a units,TIME,o,c,"year CE" $idx
    ncatted -O -a units,fLUC,a,c,"gC m-2 (10 yr)-1" $idx
    ncatted -O -a long_name,fLUC,a,c,"CO2 emissions due to anthropogenic land use change per 10 year period for which dimension TIME marks the mid-point." $idx
done

echo "GLOBAL ATTRIBUTES..."

## HYDE 31
echo "HYDE 31"
ncatted -a title,global,o,c,"HYDE 3.1 scenario landuse CO2 emissions (Stocker et al., 2017, PNAS), http://dx.doi.org/10.3334/ORNLDAAC/1382" data/fluc_hyde31_stocker17pnas.nc

## KK10
echo "KK10"
ncatted -a title,global,o,c,"KK10 scenario landuse CO2 emissions (Stocker et al., 2017, PNAS), http://dx.doi.org/10.3334/ORNLDAAC/1382" data/fluc_kk10_stocker17pnas.nc

## HYDE31 upper
echo "HYDE31U"
ncatted -a title,global,o,c,"HYDE 3.1 upper scenario landuse CO2 emissions (Stocker et al., 2017, PNAS), http://dx.doi.org/10.3334/ORNLDAAC/1382" data/fluc_hyde31u_stocker17pnas.nc


## KK10D
echo "KK10D"
ncatted -a title,global,o,c,"KK10D scenario landuse CO2 emissions (Stocker et al., 2017, PNAS), http://dx.doi.org/10.3334/ORNLDAAC/1382" data/fluc_kk10d_stocker17pnas.nc


## HYDE32
echo "HYDE32"
ncatted -a title,global,o,c,"HYDE 3.2 scenario landuse CO2 emissions (Stocker et al., 2017, PNAS), http://dx.doi.org/10.3334/ORNLDAAC/1382" data/fluc_hyde32_stocker17pnas.nc


## HYDE32 upper
echo "HYDE32u"
ncatted -a title,global,o,c,"HYDE 3.2 upper scenario landuse CO2 emissions (Stocker et al., 2017, PNAS), http://dx.doi.org/10.3334/ORNLDAAC/1382" data/fluc_hyde32u_stocker17pnas.nc
 
#########################################

## luarea files
list="data/luarea_hyde31_stocker17pnas.nc data/luarea_kk10_stocker17pnas.nc data/luarea_hyde31u_stocker17pnas.nc data/luarea_kk10d_stocker17pnas.nc data/luarea_hyde32_stocker17pnas.nc data/luarea_hyde32u_stocker17pnas.nc"

for idx in $list
do
    echo "adding attributes for variables/dimensions in file $idx ..."
    ncatted -O -a long_name,Z,o,c,"land unit (gridcell tile): 1=primary, 2=secondary, 3=cropland, 4=pasture, 5=built-up" $idx
    ncatted -O -a units,TIME,o,c,"year CE" $idx
    ncatted -O -a units,luarea,a,c,"gridcell area fraction" $idx
    ncatted -O -a long_name,luarea,a,c,"gridcell area fraction covered by land unit i (i=primary, secondary, cropland, pasture, buit-up)" $idx
done

echo "GLOBAL LUAREA FILES ..."

## HYDE 31
echo "HYDE 31"
ncatted -a history,global,o,c,"Original data by Goldewijk et al., 2001. Processed for application as described in Stocker et al., 2017, PNAS." data/luarea_hyde31_stocker17pnas.nc
ncatted -a title,global,a,c,"HYDE 3.1 scenario landuse area (Stocker et al., 2017, PNAS), http://dx.doi.org/10.3334/ORNLDAAC/1382" data/luarea_hyde31_stocker17pnas.nc

## KK10
echo "KK10"
ncatted -a history,global,o,c,"Original data by Kaplan et al., 2009. Processed for application as described in Stocker et al., 2017, PNAS." data/luarea_kk10_stocker17pnas.nc
ncatted -a title,global,a,c,"KK10 scenario landuse area (Stocker et al., 2017, PNAS), http://dx.doi.org/10.3334/ORNLDAAC/1382" data/luarea_kk10_stocker17pnas.nc

## HYDE31 upper
echo "HYDE31 upper"
ncatted -a history,global,o,c,"Original data based on Goldewijk et al., 2001. Processed for application as described in Stocker et al., 2017, PNAS." data/luarea_hyde31u_stocker17pnas.nc
ncatted -a title,global,a,c,"HYDE 3.1 upper scenario landuse area (Stocker et al., 2017, PNAS), http://dx.doi.org/10.3334/ORNLDAAC/1382" data/luarea_hyde31u_stocker17pnas.nc


## KK10D
echo "KK10D"
ncatted -a history,global,o,c,"Modified from KK10 as described in Stocker et al., 2017, PNAS." data/luarea_kk10d_stocker17pnas.nc
ncatted -a title,global,a,c,"KK10D scenario landuse area (Stocker et al., 2017, PNAS), http://dx.doi.org/10.3334/ORNLDAAC/1382" data/luarea_kk10d_stocker17pnas.nc


## HYDE32
echo "HYDE32"
ncatted -a history,global,o,c,"Original data from Goldewijk, 2016. Processed for application as described in Stocker et al., 2017, PNAS." data/luarea_hyde32_stocker17pnas.nc
ncatted -a title,global,a,c,"HYDE 3.2 scenario landuse area (Stocker et al., 2017, PNAS), http://dx.doi.org/10.3334/ORNLDAAC/1382" data/luarea_hyde32_stocker17pnas.nc


## HYDE32 upper
echo "HYDE32u"
ncatted -a history,global,o,c,"Original data from Goldewijk, 2016. Processed for application as described in Stocker et al., 2017, PNAS." data/luarea_hyde32u_stocker17pnas.nc
ncatted -a title,global,a,c,"HYDE 3.2 upper scenario landuse area (Stocker et al., 2017, PNAS), http://dx.doi.org/10.3334/ORNLDAAC/1382" data/luarea_hyde32u_stocker17pnas.nc

