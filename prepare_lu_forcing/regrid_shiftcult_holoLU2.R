library(ncdf)

# infil_crop <- "landuse_hyde31_final_lpjgr.cdf"
# outfil     <- "shiftcultinfo_hyde31_final_lpjgr_holoLU2.nc"

# infil_crop <- "landuse_KK11_lpjgr.nc"
# outfil     <- "shiftcultinfo_kk10_lpjgr_holoLU2.nc"

# infil_crop   <- "landuse_hyde31_Ruddiman_pastcorr_lpjgr.cdf"
# outfil       <- "shiftcultinfo_hyde31_Ruddiman_lpjgr_holoLU2.nc"

# infil_crop   <- "landuse_KK11delayed_lpjgr.nc"
# outfil       <- "shiftcultinfo_kk11delayed_lpjgr_holoLU2.nc"

infil_crop <- "landuse_hyde32_upper_lpjgr.nc"
outfil     <- "shiftcultinfo_hyde32_upper_lpjgr_holoLU2.nc"

## Read land suitability field (already regridded to lpjgr by 'regrid_suit_holoLU2.jnl')
nc <- open.ncdf( "land_suit_sage_lpjgr.nc" )
suit_sage <- get.var.ncdf(nc, "SUIT")
close.ncdf(nc)

## Read permanen/non-permanent field
nc <- open.ncdf( "perm_lpjgr_holoLU2.nc" )
perm <- get.var.ncdf(nc, "PERM" )
close.ncdf(nc)
time <- nc$dim$TIME$vals

## Read output land mask
nc <- open.ncdf( "/card/forcings/lpx/soil/peltier2004/landmask_pelt04_rs_lpjgr.cdf" )
land <- get.var.ncdf(nc, "LAND" )
close.ncdf(nc)
land <- land[,,1]
lon <- nc$dim$LONGITUDE$vals
lat <- nc$dim$LATITUDE$vals

## Read cropland/pasture fields (HYDE 3.1)
lu <- open.ncdf( infil_crop )
crop_hyde <- get.var.ncdf(lu, "crop")
past_hyde <- get.var.ncdf(lu, "past")
time_hyde <- get.var.ncdf(lu, "TIME")
close.ncdf(lu)

## scale back fallow-rotation factor from 3/2 to 1 (with population density) between 1850 and 1960
fallow_factor <- rep(NA,length(time))
for (itim in 1:length(time)){
  fallow_factor[itim] <- max( 1.0, min( 3.0/2.0, 3.0/2.0 - 0.5/110.0 * (time[itim]-1850.0) ) )
}

## Loop over each grid cells on coarse grid and find nearest grid cell to get its 
## information "suitable" fraction
suit_out  <- crop_hyde * NA
totime    <- crop_hyde * NA

for (ilon in 1:nc$dim$LONGITUDE$len){
  for (ilat in 1:nc$dim$LATITUDE$len){
    if (!is.na(land[ilon,ilat]) && !is.na(perm[ilon,ilat,1])){
      # for (itim in 1:59){
      for (itim in 1:58){
        if (perm[ilon,ilat,itim]==2){

          ## permanent agriculture          
          ## "suitable fraction" is cropland area scaled by the "fallow factor" to account for fallow-rotation
          suit_out[ilon,ilat,itim] <- min( 1.0, fallow_factor[itim] * crop_hyde[ilon,ilat,itim] )

          ## land turnover time
          totime[ilon,ilat,itim]   <- 3.0 / fallow_factor[itim]


        } else {

          ## non-permanent agriculture
          ## "suitable fraction" is cropland area scaled by the "fallow factor" to account for fallow-rotation
          suit_out[ilon,ilat,itim] <- suit_sage[ilon,ilat]

          ## turnover time
          totime[ilon,ilat,itim] <- 4.0

        }
      }      
    }
  }
}

# Inaccessible fraction
inaccess <- land - suit_sage 


## Write to NetCDF file
cdf.write( suit_out,"crop_suit",
          lon, lat,
          outfil,
          z_dim=NA,time=time_hyde,
          make.zdim=FALSE,make.tdim=TRUE,
          nvars=2,
          var2=totime, varnam2="tau_land",
          vartype="NC_FLOAT",
          verbose=FALSE
          )

# cdf.write( inaccess,"inaccess",
#           lon, lat,
#           "inaccess_lpjgr_holoLU2.nc",
#           z_dim=NA,time=time,
#           make.zdim=FALSE,make.tdim=FALSE,
#           vartype="NC_FLOAT",
#           verbose=FALSE
#           )

