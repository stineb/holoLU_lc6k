library(ncdf)
source('/alphadata01/bstocker/utilities/cdf.write.R')
source('/alphadata01/bstocker/utilities/area.R')

# infil  <- "landuse_hyde31_final_halfdeg.cdf"
# outfil <- "harvest_hurtt_byarea_v2_halfdeg_backby_hyde31_final.nc"

infil  <- "landuse_hyde32_upper_halfdeg.cdf"
outfil <- "harvest_hurtt_byarea_v2_halfdeg_backby_hyde32_upper.nc"

# End product: field for every hyde time step defining harvested area

## Read cropland fields (HYDE 3.1)
lu <- open.ncdf( infil )
time_hyde <- lu$dim$TIME$vals
nlon <- lu$dim$LONGITUDE$len
nlat <- lu$dim$LATITUDE$len
crop_abs_hyde <- get.var.ncdf(lu, "crop_abs")
close.ncdf(lu)

# ## Read cropland fields (KK10)
# lu <- open.ncdf( "/alphadata01/bstocker/data/landuse_data/kk10/landuse_KK11_halfdeg_hydeslices.nc" )
# crop_kk10 <- get.var.ncdf(lu, "crop")
# close.ncdf(lu)

# lu <- open.ncdf( "/alphadata01/bstocker/data/landuse_data/kk10/landuse_KK11delayed_halfdeg_hydeslices.nc" )
# crop_kk10_del <- get.var.ncdf(lu, "crop")
# close.ncdf(lu)

# ## convert KK10 data to absolute values
# crop_abs_kk10 <- crop_kk10 * NA
# crop_abs_kk10_del <- crop_kk10 * NA
# for (ilon in 1:lu$dim$LONGITUDE$len){
#   for (ilat in 1:lu$dim$LATITUDE$len){
#     if (!is.na(crop_kk10[ilon,ilat,1])){
#       crop_abs_kk10[ilon,ilat,]     <- crop_kk10[ilon,ilat,]     * area( lu$dim$LATITUDE$vals[ilat], 0.5, 0.5 )
#       crop_abs_kk10_del[ilon,ilat,] <- crop_kk10_del[ilon,ilat,] * area( lu$dim$LATITUDE$vals[ilat], 0.5, 0.5 )
#     }
#   }
# }

## Read harvest area fraction (LUH GCP 2013 data)
lu <- open.ncdf("/alphadata01/bstocker/data/landuse_data/luh_rcp_hurtt/harvest_hurtt_byarea_v2_halfdeg_hydeslices_tmp.nc")
time_harv <- lu$dim$TIME$vals
harv <- get.var.ncdf(lu,"f_vbh")
close.ncdf(lu)

## Read continent file
nc <- open.ncdf("/alphadata01/bstocker/data/masks/regmask_halfdeg.nc")
mask <- get.var.ncdf(nc,"mask")
ncont <- dim(mask)[3] - 1
close.ncdf(nc)


## Use continent-wise total cropland area to back project harvest area fraction for each continent separately
## -----------------------------------------------------
## Time series of total cropland area for each continent
cropland_per_continent_hyde     <- array( 0.0, dim=c(length(time_hyde),ncont))
# cropland_per_continent_kk10     <- array( 0.0, dim=c(length(time_hyde),ncont))
# cropland_per_continent_kk10_del <- array( 0.0, dim=c(length(time_hyde),ncont))

for (icont in 1:ncont){
  for (itim in 1:length(time_hyde)){
    tmp <- crop_abs_hyde[,,itim]
    cropland_per_continent_hyde[itim,icont]     <- sum( tmp[ mask[,,9]==icont ], na.rm=TRUE )
    # tmp <- crop_abs_kk10[,,itim]
    # cropland_per_continent_kk10[itim,icont]     <- sum( tmp[ mask[,,9]==icont ], na.rm=TRUE )
    # tmp <- crop_abs_kk10_del[,,itim]
    # cropland_per_continent_kk10_del[itim,icont] <- sum( tmp[ mask[,,9]==icont ], na.rm=TRUE )
  }
}

# ## Alternative: back-project globally uniform
# cropland_global_hyde <- rep( 0.0, length(time_hyde) )
# cropland_global_kk10 <- rep( 0.0, length(time_hyde) )
# for (itim in 1:length(time_hyde)){
#   cropland_global_hyde[itim] <- sum( crop_abs_hyde[,,itim], na.rm=TRUE )
#   cropland_global_kk10[itim] <- sum( crop_abs_kk10[,,itim], na.rm=TRUE )
# }



## Fill remaining time steps with harvest back-projected by cropland fraction
harv_out_hyde     <- crop_abs_hyde * 0.0
# harv_out_kk10     <- crop_abs_hyde * 0.0
# harv_out_kk10_del <- crop_abs_hyde * 0.0

pivotyear <- 1960
istart <- 1
iend   <- which.min(abs(time_hyde-pivotyear))
ipivotyear_harv <- which.min(abs(time_harv-pivotyear))
imax   <- length(harv_out_hyde[1,1,])

## take the last years' data from original data
for (iyr in which(time_hyde>=pivotyear)){
  harv_out_hyde[,,iyr] <- harv[,,which.min(abs(time_hyde[iyr]-time_harv))]
}


# harv_out_kk10[,,iend:imax] <- harv[,,ipivotyear_harv:length(harv[1,1,])]
# harv_out_kk10[,,imax]      <- harv[,,length(harv[1,1,])]

# harv_out_kk10_del[,,iend:imax] <- harv[,,ipivotyear_harv:length(harv[1,1,])]
# harv_out_kk10_del[,,imax]      <- harv[,,length(harv[1,1,])]


for (icont in 1:ncont){
  for (ilon in 1:lu$dim$LONGITUDE$len){
    for (ilat in 1:lu$dim$LATITUDE$len){
      if (!is.na(harv[ilon,ilat,1])&&!is.na(mask[ilon,ilat,1])){
        if (mask[ilon,ilat,9]==icont){
          for (itim in istart:(iend-1)){

            ## back-project for earlier years
            harv_out_hyde[ilon,ilat,itim]     <- harv_out_hyde[ilon,ilat,iend]     * cropland_per_continent_hyde[itim,icont]     / cropland_per_continent_hyde[iend,icont]
            # harv_out_kk10[ilon,ilat,itim]     <- harv_out_kk10[ilon,ilat,iend]     * cropland_per_continent_kk10[itim,icont]     / cropland_per_continent_kk10[iend,icont]
            # harv_out_kk10_del[ilon,ilat,itim] <- harv_out_kk10_del[ilon,ilat,iend] * cropland_per_continent_kk10_del[itim,icont] / cropland_per_continent_kk10_del[iend,icont]

          }
        }
      } 
    }
  }
}

# for (ilon in 1:lu$dim$LONGITUDE$len){
#   for (ilat in 1:lu$dim$LATITUDE$len){
#     if (!is.na(harv[ilon,ilat,1])){
#       for (itim in istart:(iend-1)){

#         ## back-project for earlier years
#         harv_out_hyde[ilon,ilat,itim] <- harv_out_hyde[ilon,ilat,iend] * cropland_global_hyde[itim] / cropland_global_hyde[iend]
#         harv_out_kk10[ilon,ilat,itim] <- harv_out_kk10[ilon,ilat,iend] * cropland_global_kk10[itim] / cropland_global_kk10[iend]

#       }
#     }
#   }
# }

cdf.write( harv_out_hyde, "aharv",
          lu$dim$LONGITUDE$vals, lu$dim$LATITUDE$vals,
          outfil,
          z_dim=NA, time=time_hyde,
          make.zdim=FALSE, make.tdim=TRUE,
          nvars=1,
          vartype="NC_FLOAT",
          verbose=FALSE
          )

# cdf.write( harv_out_kk10, "aharv",
#           lu$dim$LONGITUDE$vals, lu$dim$LATITUDE$vals,
#           "harvest_hurtt_byarea_v2_halfdeg_backby_kk10.nc",
#           z_dim=NA, time=time_hyde,
#           make.zdim=FALSE, make.tdim=TRUE,
#           nvars=1,
#           vartype="NC_FLOAT",
#           verbose=FALSE
#           )

# cdf.write( harv_out_kk10_del, "aharv",
#           lu$dim$LONGITUDE$vals, lu$dim$LATITUDE$vals,
#           "harvest_hurtt_byarea_v2_halfdeg_backby_kk10del.nc",
#           z_dim=NA, time=time_hyde,
#           make.zdim=FALSE, make.tdim=TRUE,
#           nvars=1,
#           vartype="NC_FLOAT",
#           verbose=FALSE
#           )


# cdf.write( harv_out_hyde, "aharv",
#           lu$dim$LONGITUDE$vals, lu$dim$LATITUDE$vals,
#           "harvest_hurtt_byarea_v2_halfdeg_backby_globalhyde31_final.nc",
#           z_dim=NA, time=time_hyde,
#           make.zdim=FALSE, make.tdim=TRUE,
#           nvars=1,
#           vartype="NC_FLOAT",
#           verbose=FALSE
#           )

# cdf.write( harv_out_kk10, "aharv",
#           lu$dim$LONGITUDE$vals, lu$dim$LATITUDE$vals,
#           "harvest_hurtt_byarea_v2_halfdeg_backby_globalkk10.nc",
#           z_dim=NA, time=time_hyde,
#           make.zdim=FALSE, make.tdim=TRUE,
#           nvars=1,
#           vartype="NC_FLOAT",
#           verbose=FALSE
#           )

