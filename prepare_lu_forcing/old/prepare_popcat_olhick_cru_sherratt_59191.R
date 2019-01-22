#----------------------------------------------------------------------------------------
# CATEGORIES STATES/EMPIRES AND AGRICULTURAL GROUPS
# READS ORIGINAL DATA AND RESHAPES IT TO SPATIAL ARRAY
# Lewthwaite JW, Sherratt A (1980) Chronological Atlas. In: Sherratt A (ed) Cambridge Encyclopedia of Archeology.
# Cambridge University Press, Cambridge, pp 437-452

# SCANNED AND PROCESSED BY J<F6>rgen Olofsson

# Columns
# 1: lon
# 2: lat
# 3: 4000 BC
# 4: 2000 BC
# 5: AD 1
# 6: AD 1000

# Labels for col 3 to 6
# 1:  Agricultural Groups
# 2:  States and Empires

# 59191 grids (0.5 x 0.5 degree) based on CRU
#----------------------------------------------------------------------------------------
source('/alphadata01/bstocker/utilities/cdf.write.R')

cat.raw <- read.table(
  '/alphadata01/bstocker/data/landuse_data/olofsson/cru_sherratt_59191/cru_sherratt_59191.txt'
  , col.names=c('lon','lat','cat4kbc','cat2kbc','cat01ad','cat1kad') 
  )

gridlist_landuse_7_VHA_2008_OH

## reshape raw data onto a quasi-spatial array
ncells <- dim(cat.raw)[1]
nslice <- 4
lon <- seq( -179.75, 179.75, 0.5 )
lat <- seq( -89.97, 89.75, 0.5 )

calc_ilon <- function( lon, res=0.5 ) {
  ## assumes that 'lon' is NOT right (eastern) edge of gridcell
  ## 'lon' may be left edge, gridcell center, or anything else
  ## left of the right edge.
  ilon <- (lon + 180) * 1/res + 1 
  ilon <- floor(ilon)
  return(ilon)
}

calc_ilat <- function( lat, res=0.5 ) {
  ## assumes that 'lat' is NOT upper (northern) edge of gridcell
  ## 'lat' may be lower edge, gridcell center, or anything else
  ## below the upper edge.
  ilat <- (lat + 90) * 1/res + 1 
  ilat <- floor(ilat)
  return(ilat)
}

cat.resh <- array( NA, dim=c( length(lon), length(lat), nslice ))
for (idx in 1:ncells){
  thislon <- cat.raw$lon[idx]
  thislat <- cat.raw$lat[idx]
  ilon <- calc_ilon( thislon )
  ilat <- calc_ilat( thislat )

  cat.resh[ilon,ilat,1] <- cat.raw$cat4kbc[idx]
  cat.resh[ilon,ilat,2] <- cat.raw$cat2kbc[idx]
  cat.resh[ilon,ilat,3] <- cat.raw$cat01ad[idx]
  cat.resh[ilon,ilat,4] <- cat.raw$cat1kad[idx]
}

## Write to NetCDF file
cdf.write(cat.resh,"category",
          lon,lat,
          "popcat_olhick_cru_sherratt_59191.nc",
          z_dim=NA,time=c(-4000,-2000,1,1000),
          make.zdim=FALSE,make.tdim=TRUE,
          nvars=1,
          vartype="NC_FLOAT",
          verbose=FALSE
          )
