#----------------------------------------------------------------------------------------
# PREPARES NETCDF FILE DEFININT THE LAND USE CATEGORY FOLLOWING OLOFSSON & HICKLER (2008)
# NON-PERMANENT VS PERMANENT AGRICULTURUE
# Methodology for selection described in Olofsson & Hickler paper (2008 VHA)

# Olofsson, J. & Hickler, T. (2008) Effects of human land-use on the global carbon cycle during the last 6,000 years.
# Vegetation History and Archaeobotany, 17, 606-615.


# Time slices through time (one land-use class per grid cell per time slice):

# Columns
# 1: lon
# 2: lat
# 3: 4000 BC to 3001 BC
# 4: 3000 BC to 1000 BC
# 5: 1000 BC to AD 499
# 6: AD 500 to AD 1499
# 7: AD 1500 to AD 1774
# 8: AD 1775 to AD 1920
# 9: AD 1921 to AD 1998

# Three land-use classes (boolean)
# 0:  no land-use
# 1:  non-permanent land-use (~"slash-and-burn")
# 2:  permanent land-use

# 22650 grids (0.5 x 0.5 degree), only cells that are under land use at least one time slice


# CONTACT:
# J<F6>rgen Olofsson (jorgen.olofsson@nateko.lu.se)
#----------------------------------------------------------------------------------------
source('/alphadata01/bstocker/utilities/cdf.write.R')

col.names <- c('lon','lat','lucat4_3kbc','lucat3_1kbc','lucat1kbc_500ad','lucat500_1500ad','lucat1500_1774','lucat1775_1920','lucat1921_1998')
cat.raw <- read.table(
  '/alphadata01/bstocker/data/landuse_data/olofsson/gridlist_landuse_7_VHA_2008_OH/gridlist_landuse_7_VHA_2008_OH.txt'
  , col.names=col.names
  )

## reshape raw data onto a quasi-spatial array
ncells <- dim(cat.raw)[1]
nslice <- 7
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

  cat.resh[ilon,ilat,1] <- cat.raw$lucat4_3kbc[idx]  
  cat.resh[ilon,ilat,2] <- cat.raw$lucat3_1kbc[idx]  
  cat.resh[ilon,ilat,3] <- cat.raw$lucat1kbc_500ad[idx]  
  cat.resh[ilon,ilat,4] <- cat.raw$lucat500_1500ad[idx]  
  cat.resh[ilon,ilat,5] <- cat.raw$lucat1500_1774[idx]  
  cat.resh[ilon,ilat,6] <- cat.raw$lucat1775_1920[idx]  
  cat.resh[ilon,ilat,7] <- cat.raw$lucat1921_1998[idx]  

}

## Write to NetCDF file
cdf.write(cat.resh,"category",
          lon,lat,
          "lucat_olhick_gridlist_landuse_7_VHA_2008_OH.nc",
          z_dim=NA,time=c(-4000,-3000,-1000,500,1500,1775,1921),
          make.zdim=FALSE,make.tdim=TRUE,
          nvars=1,
          vartype="NC_FLOAT",
          verbose=FALSE
          )

# 11  -16.5 66.0           0           0               0               2

