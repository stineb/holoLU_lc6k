load( "fluc_holoLU2.Rdata" )
load( "landuse_tseries.Rdata" )
source('/alphadata01/bstocker/utilities/area.R')

print("loading...")

# lufils <- c( 
#   # "landuse_hyde31_final_pastcorr_lpjgr.cdf",
#   # "landuse_hyde32_baseline_pastcorr_lpjgr",
#   , 
  
#   )

## read files
nc <- open.ncdf( "landuse_KK11_pastcorr_lpjgr.cdf" )
crop_kk10 <- get.var.ncdf(nc, "crop")
past_kk10 <- get.var.ncdf(nc, "past")
lat <- get.var.ncdf(nc,"LATITUDE")
time_hyde31 <- get.var.ncdf(nc, "TIME")
close.ncdf(nc)

nc <- open.ncdf( "landuse_KK11delayed_pastcorr_lpjgr.cdf" )
crop_kk10d <- get.var.ncdf(nc, "crop")
past_kk10d <- get.var.ncdf(nc, "past")
time_hyde31 <- get.var.ncdf(nc, "TIME")
close.ncdf(nc)

# nc <- open.ncdf( lufils[3] )
# crop_kk10 <- get.var.ncdf(nc, "crop")
# past_kk10 <- get.var.ncdf(nc, "past")
# time_hyde31 <- get.var.ncdf(nc, "TIME")
# close.ncdf(nc)

# nc <- open.ncdf( lufils[4] )
# crop_kk10d <- get.var.ncdf(nc, "crop")
# past_kk10d <- get.var.ncdf(nc, "past")
# time_hyde31 <- get.var.ncdf(nc, "TIME")
# close.ncdf(nc)

area_bylat <- sapply( lat, FUN = function(x) area(x,0.5,0.5) )
area <- NA*crop_kk10[,,1]
for (ilon in 1:dim(crop_kk10)[1]){ area[ilon,] <- area_bylat } 

crop_kk10_abs  <- crop_kk10 * NA
past_kk10_abs  <- past_kk10 * NA
crop_kk10d_abs <- crop_kk10d * NA
past_kk10d_abs <- past_kk10d * NA
for (itim in 1:dim(crop_kk10)[3]){
  crop_kk10_abs[,,itim]  <- crop_kk10[,,itim] * area
  past_kk10_abs[,,itim]  <- past_kk10[,,itim] * area
  crop_kk10d_abs[,,itim] <- crop_kk10[,,itim] * area
  past_kk10d_abs[,,itim] <- past_kk10[,,itim] * area
}

## from comparison of input data
crop <- read.csv('crop_totals_hyde.csv')
crop[,2:7] <- crop[,2:7]*1e7

past <- read.csv('past_totals_hyde.csv')
past[,2:7] <- past[,2:7]*1e7

crop$fluc_intensity_hyde31 <- crop$year * NA
crop$fluc_intensity_hyde32 <- crop$year * NA

for (itim in 1:dim(crop)[1]){

  crop$fluc_intensity_hyde31[itim] <- fluc$hyde31$cumfluc[ which(fluc$hyde31$year==crop$year[itim]) ] / (crop$hyde31_in_lpjgr[itim]+past$hyde31_in_lpjgr[itim])
  crop$fluc_intensity_hyde32[itim] <- fluc$hyde32$cumfluc[ which(fluc$hyde32$year==crop$year[itim]) ] / (crop$hyde32_in_lpjgr[itim]+past$hyde32_in_lpjgr[itim])

  crop$fluc_intensity_kk10[itim]   <- fluc$kk10$cumfluc[ which(fluc$kk10$year==crop$year[itim]) ]    / (sum(crop_kk10_abs[,,itim], na.rm=TRUE)+sum(past_kk10_abs[,,itim], na.rm=TRUE))
  crop$fluc_intensity_kk10d[itim]  <- fluc$kk10d$cumfluc[ which(fluc$kk10d$year==crop$year[itim]) ]  / (sum(crop_kk10d_abs[,,itim], na.rm=TRUE)+sum(past_kk10d_abs[,,itim], na.rm=TRUE))

}


pdf( "emission_intensity.pdf", width=8, height=6 )
plot( crop$year, crop$fluc_intensity_hyde31, type="l" )
lines( crop$year, crop$fluc_intensity_hyde32, col="blue" )
lines( crop$year, crop$fluc_intensity_kk10, col="turquoise3" )
lines( crop$year, crop$fluc_intensity_kk10d, col="goldenrod" )
dev.off()
