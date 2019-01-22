#!/bin/bash

source ~/.bash_profile

# fils="landuse_hyde31_final_pastcorr_lpjgr.cdf landuse_KK11_pastcorr_lpjgr.cdf shiftcultinfo_hyde31_final_lpjgr_holoLU2.nc shiftcultinfo_kk10_lpjgr_holoLU2.nc land_suit_sage_lpjgr.nc perm_lpjgr_holoLU2.nc harvest_hurtt_byarea_v2_lpjgr_backby_hyde31_final.nc harvest_hurtt_byarea_v2_lpjgr_backby_kk10.nc landuse_hyde31_concave_pastcorr_lpjgr.cdf landuse_hyde31_Ruddiman_pastcorr_lpjgr.cdf landuse_hyde31_upper_pastcorr_lpjgr.cdf shiftcultinfo_hyde31_Ruddiman_lpjgr_holoLU2.nc shiftcultinfo_hyde31_concave_lpjgr_holoLU2.nc shiftcultinfo_hyde31_upper_lpjgr_holoLU2.nc harvest_hurtt_byarea_v2_lpjgr_backby_hyde31_Ruddiman.nc harvest_hurtt_byarea_v2_lpjgr_backby_hyde31_concave.nc harvest_hurtt_byarea_v2_lpjgr_backby_hyde31_upper.nc"

# fils="landuse_hyde31_concave_pastcorr_lpjgr.cdf landuse_hyde31_Ruddiman_pastcorr_lpjgr.cdf landuse_hyde31_upper_pastcorr_lpjgr.cdf shiftcultinfo_hyde31_Ruddiman_lpjgr_holoLU2.nc shiftcultinfo_hyde31_concave_lpjgr_holoLU2.nc shiftcultinfo_hyde31_upper_lpjgr_holoLU2.nc harvest_hurtt_byarea_v2_lpjgr_backby_hyde31_Ruddiman.nc harvest_hurtt_byarea_v2_lpjgr_backby_hyde31_concave.nc harvest_hurtt_byarea_v2_lpjgr_backby_hyde31_upper.nc"

# for ifil in $fils; do

#   scp $ifil bstocker@kuplaptev.unibe.ch:/alphadata01/bstocker/lpx/holoLU2/input_data/anthropogenic2d/landuse/

# done


# ## to data directory and then upload
# dir="/alphadata01/bstocker/data/landuse_data/"
# fils="inaccess_lpjgr_holoLU2.nc fopen_r0_holoLU2.nc perm_lpjgr_holoLU2.nc perm_holoLU2.nc"
# for i in $fils
# do
#   mv $i $dir
# done

# cd $dir
# for i in $fils
# do
#   urs $i
# done


# subdir="hyde3_1/hyde31_Ruddiman/"
# fils="shiftcultinfo_hyde31_Ruddiman_lpjgr_holoLU2.nc harvest_hurtt_byarea_v2_lpjgr_backby_hyde31_Ruddiman.nc harvest_hurtt_byarea_v2_halfdeg_backby_hyde31_Ruddiman.nc landuse_hyde31_Ruddiman_pastcorr_lpjgr.cdf landuse_hyde31_Ruddiman_lpjgr.cdf landuse_hyde31_Ruddiman_halfdeg.cdf"

# # for i in $fils
# # do
# #   mv $i $dir$subdir
# # done

# cd $dir$subdir
# for i in $fils
# do
#   urs $i
# done

# subdir="hyde3_1/hyde31_concave/"
# fils="shiftcultinfo_hyde31_concave_lpjgr_holoLU2.nc harvest_hurtt_byarea_v2_lpjgr_backby_hyde31_concave.nc harvest_hurtt_byarea_v2_halfdeg_backby_hyde31_concave.nc landuse_hyde31_concave_pastcorr_lpjgr.cdf landuse_hyde31_concave_lpjgr.cdf landuse_hyde31_concave_halfdeg.cdf "
# # for i in $fils
# # do
# #   mv $i $dir$subdir
# # done
# cd $dir$subdir
# for i in $fils
# do
#   urs $i
# done

# subdir="hyde3_1/hyde31_upper/"
# fils="shiftcultinfo_hyde31_upper_lpjgr_holoLU2.nc harvest_hurtt_byarea_v2_lpjgr_backby_hyde31_upper.nc harvest_hurtt_byarea_v2_halfdeg_backby_hyde31_upper.nc landuse_hyde31_upper_pastcorr_lpjgr.cdf landuse_hyde31_upper_lpjgr.cdf landuse_hyde31_upper_halfdeg.cdf "

# # for i in $fils
# # do
# #   mv $i $dir$subdir
# # done
# cd $dir$subdir
# for i in $fils
# do
#   urs $i
# done

# subdir="kk10"
# fils="shiftcultinfo_kk10_lpjgr_holoLU2.nc harvest_hurtt_byarea_v2_lpjgr_backby_kk10.nc harvest_hurtt_byarea_v2_halfdeg_backby_kk10.nc landuse_KK11_pastcorr_lpjgr.cdf landuse_KK11_halfdeg_hydeslices.nc landuse_KK11_lpjgr.nc"

# # for i in $fils
# # do
# #   mv $i $dir$subdir
# # done

# cd $dir$subdir
# for i in $fils
# do
#   urs $i
# done

