## Read cropland/pasture fields (HYDE 3.1)
# infil <- "landuse_KK11_lpjgr.nc"
# infil  <- "landuse_hyde31_Ruddiman_lpjgr.cdf"
# infil  <- "landuse_KK11delayed_lpjgr.nc"
infil  <- "landuse_hyde32_upper_lpjgr.nc"

# outfil <- "landuse_KK11_pastcorr_lpjgr.cdf"
# outfil <- "landuse_hyde31_Ruddiman_pastcorr_lpjgr.cdf"
# outfil <- "landuse_KK11delayed_pastcorr_lpjgr.cdf"
outfil <- "landuse_hyde32_upper_pastcorr_lpjgr.cdf"

nc <- open.ncdf( infil )
crop_hyde <- get.var.ncdf(nc, "crop")
past_hyde <- get.var.ncdf(nc, "past")
close.ncdf(nc)

lon <- nc$dim$LONGITUDE$vals
lat <- nc$dim$LATITUDE$vals
time <- nc$dim$TIME$vals

## Read LPX output from control simulation
nc <- open.ncdf( "fopen_r0_holoLU2.nc" )
fopen <- get.var.ncdf(nc, "FOPEN" )
close.ncdf(nc)

## remove naturally open vegetation fraction from pasture fraction
past_corr <- past_hyde
for (itim in 1:length(time)){
  past_corr[,,itim] <- past_hyde[,,itim] - fopen
}
past_corr[ past_corr < 0.0 ] <- 0.0

## Write to NetCDF file
cdf.write( crop_hyde,"crop",
          lon, lat,
          outfil,
          z_dim=NA,time=time,
          make.zdim=FALSE, make.tdim=TRUE,
          nvars=2,
          var2=past_corr, varnam2="past",
          vartype="NC_FLOAT",
          verbose=FALSE
          )
