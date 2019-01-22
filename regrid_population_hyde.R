####################################################################
## Function opens original HYDE landuse data and passes it on to function regrid.landuse()
## -----------------------------------------------------------------
regrid.population.hyde <- function( year, scenario, grid.out, verbose=FALSE, aligned=FALSE ){ 

  ## year=-8000
  ## scenario="hyde31_final"
  ## grid.out="halfdeg"
  ## aligned=TRUE
  ## verbose=TRUE

  library(RNetCDF)
  source('/alphadata01/bstocker/lpx/lpxtools/trunk/landuse/get.land.avail.R')
  source('/alphadata01/bstocker/lpx/lpxtools/trunk/landuse/regrid_landuse.R')
  source('/alphadata01/bstocker/lpx/lpxtools/trunk/landuse/cdf.write.R')
  
  if (verbose){
    print("*******************")
    print(paste( "YEAR ", as.character(year) ))
    print("*******************")
  }

  dir <- "/alphadata01/bstocker/data/landuse_data/hyde32/"
  
  ## Define grid of original landuse data.
  ## lon(i), lat(j): longitude and latitude of gridcell center (i,j) 
  ## Define "by hand" because numbers in NetCDF are not very accurate 
  dx <- 1/12
  dy <- 1/12
  lon <- seq(from=-180+dx/2, to=180-dx-dx/2, by=dx) #var.get.nc(nc,dimname_orig_lon)
  lat <- seq(from=-90+dy/2, to=90-dy-dy/2, by=dy) #var.get.nc(nc,dimname_orig_lat)
  varname_orig <- c("POPC")
  dimname_orig_time <- "TIME"
  ncat <- length(varname_orig)
  
  ## Initialise array containing land use areas of all categories in original file(s)
  lu <- array(0,c(length(lon),length(lat),ncat)) 
  
  ## Read original file, lu is in units of km2
  if (verbose) {print("reading original population data...")}
  fil.orig <- paste(
                    dir,scenario,"/raw/population_",year,".nc",
                    sep=""
                    )
  if (verbose) {print(paste("file name",fil.orig))}
  nc <- open.nc(fil.orig)  
  time <- var.get.nc(nc,dimname_orig_time,c(1),c(1))
  for (k in seq(ncat)) {
    lu[,,k] <- var.get.nc(nc,varname_orig[k])
  }
  close.nc(nc)
  
  ## Read land mask file for output
  out.land.avail <- get.land.avail( grid.out, verbose=verbose, fraction=FALSE )
  land.avail <- out.land.avail$avail/1e6  # converted to km2
  lono <- out.land.avail$lon
  lato <- out.land.avail$lat
  
  ## Call regridding function
  ## 'lu' is absolute area (km2)
  out.regrid.population <- regrid.landuse(
                                       lu,
                                       land.avail,
                                       fraction=FALSE,
                                       mass=TRUE,
                                       aligned=aligned,
                                       lon=lon, lat=lat,
                                       dx=dx, dy=dy,
                                       lono=lono, lato=lato,
                                       verbose=verbose
                                       )
  
  ## Write NetCDF output
  writeyear <- sprintf( "%06i", year )
  cdf.write(
            out.regrid.population$lu[,,1], "pop",
            out.regrid.population$lon,
            out.regrid.population$lat,
            paste(dir,scenario,"/population_",scenario,"_",grid.out,"_",writeyear,".cdf",sep=""),
            time=time,make.tdim=TRUE,
            nvars=1
            )
  print("done.")

}


## /////////////////////////////////////////////////////////////////
## LOOP OVER FILES TO REGRID
## -----------------------------------------------------------------
scenarios=c("hyde32_baseline")
for (iscen in scenarios){
  yrs <- read.table( paste(
                           "/alphadata01/bstocker/data/landuse_data/hyde32/",
                           iscen,
                           "/yrlist.txt",
                           sep=""
                           ),
                    col.names=c("year"))
  yearad <- sort(yrs$year)
  for (iyr in 1:length(yearad)){
    regrid.population.hyde( yearad[iyr], iscen, "halfdeg", aligned=TRUE, verbose=TRUE )
  }
}

