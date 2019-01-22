# ####################################################################
# ## Function opens original HYDE landuse data and passes it on to function regrid.landuse()
# ## -----------------------------------------------------------------
regrid.landuse.holoLU2 <- function( fil_in, fil_out, harvest_data=FALSE ){ 
	
  library(ncdf)
  source('/alphadata01/bstocker/lpx/lpxtools/trunk/landuse/get.land.avail.R')
  source('/alphadata01/bstocker/lpx/lpxtools/trunk/landuse/regrid_landuse.R')
  source('/alphadata01/bstocker/lpx/lpxtools/trunk/landuse/cdf.write.R')
    
  scenario <- "hyde31_final"
  verbose <- TRUE
  grid.out <- "lpjgr"

  ## Read original file
  if (verbose) {print("reading original landuse data...")}
  print(paste("original file",fil_in))
  nc <- open.ncdf(fil_in)  
  time <- get.var.ncdf( nc, "TIME" )

  if ( harvest_data ){
    harv <- get.var.ncdf( nc, "aharv" )
    lu <- array( NA, dim=c(dim(harv),1))
    lu[,,,1] <- harv
  } else {
    crop <- get.var.ncdf( nc, "crop" )
    past <- get.var.ncdf( nc, "past" )
    lu <- array( NA, dim=c(dim(crop),2))
    lu[,,,1] <- crop
    lu[,,,2] <- past
  }

  close.ncdf(nc)
  lon <- nc$dim$LONGITUDE$vals
  lat <- nc$dim$LATITUDE$vals
  dx <- lon[2]-lon[1]
  dy <- lat[2]-lat[1]
    
  ## Read land mask file for output
  land.avail <- get.land.avail( grid.out, verbose=verbose )

  ## Call regridding function
  if ( harvest_data ) {

    harv_regridded <- array( NA, dim=c(dim(land.avail$avail),length(time)) )
    for (itim in 1:length(time)){
      print("*******************")
      print(paste("YEAR",time[itim]))
      out.regrid.landuse <- regrid.landuse( lu[,,itim,], land.avail$avail, 
                                            harvest=TRUE,
                                            fraction=TRUE, aligned=FALSE, 
                                            lon=lon, lat=lat, 
                                            lono=land.avail$lon, lato=land.avail$lat, 
                                            dx=dx, dy=dy, 
                                            verbose=verbose 
                                            )
      print("successful")
      print("*******************")

      harv_regridded[,,itim] <- out.regrid.landuse$lu.rel
    }

  } else {

    lu_regridded <- array( NA, dim=c(dim(land.avail$avail),length(time),2) )
    for (itim in 1:length(time)){
    
      print("*******************")
      print(paste("YEAR",time[itim]))
      out.regrid.landuse <- regrid.landuse( lu[,,itim,], land.avail$avail, 
                                            fraction=TRUE, aligned=FALSE, 
                                            lon=lon, lat=lat, 
                                            lono=land.avail$lon, lato=land.avail$lat, 
                                            dx=dx, dy=dy, 
                                            verbose=verbose 
                                            )
      print("successful")
      print("*******************")

      lu_regridded[,,itim,1] <- out.regrid.landuse$lu.rel[,,1]
      lu_regridded[,,itim,2] <- out.regrid.landuse$lu.rel[,,2]
    }

  }

  lon_out <- out.regrid.landuse$lon
  lat_out <- out.regrid.landuse$lat
  
  ## Write NetCDF output
  if (harvest_data) {

    cdf.write(
          harv_regridded, "aharv",
          lon_out,
          lat_out,
          fil_out,
          time=time, make.tdim=TRUE
          )

  } else {

    cdf.write(
              lu_regridded[,,,1], "crop",
              lon_out,
              lat_out,
              fil_out,
              time=time, make.tdim=TRUE,
              nvars=2,
              var2=lu_regridded[,,,2], varnam2="past",
              )

  }


  print("done.")

}


## /////////////////////////////////////////////////////////////////
## LOOP OVER FILES TO REGRID
## -----------------------------------------------------------------

## cropland/pasture files:
# list_in <- c( "landuse_hyde31_concave_halfdeg.cdf", "landuse_hyde31_Ruddiman_halfdeg.cdf", "landuse_hyde31_upper_halfdeg.cdf" )
# list_out <- c( "landuse_hyde31_concave_lpjgr.cdf", "landuse_hyde31_Ruddiman_lpjgr.cdf", "landuse_hyde31_upper_lpjgr.cdf" )

# list_in  <- c( "landuse_KK11delayed_halfdeg_hydeslices.nc" )
# list_out <- c( "landuse_KK11delayed_lpjgr.nc" )

list_in  <- c( "landuse_hyde32_upper_halfdeg.cdf" )
list_out <- c( "landuse_hyde32_upper_lpjgr.nc" )

for (ifil in 1:length(list_in)){
  regrid.landuse.holoLU2( list_in[ifil], list_out[ifil], harvest_data=FALSE )
}

## harvest files:
# list_in <-  c( "harvest_hurtt_byarea_v2_halfdeg_backby_hyde31_concave.nc", "harvest_hurtt_byarea_v2_halfdeg_backby_hyde31_upper.nc", "harvest_hurtt_byarea_v2_halfdeg_backby_hyde31_Ruddiman.nc" )
# list_out <- c( "harvest_hurtt_byarea_v2_lpjgr_backby_hyde31_concave.nc"  , "harvest_hurtt_byarea_v2_lpjgr_backby_hyde31_upper.nc"  , "harvest_hurtt_byarea_v2_lpjgr_backby_hyde31_Ruddiman.nc"   )

# list_in <-  c( "harvest_hurtt_byarea_v2_halfdeg_backby_kk11del.nc" )
# list_out <- c( "harvest_hurtt_byarea_v2_lpjgr_backby_kk11del.nc"   )

list_in <-  c( "harvest_hurtt_byarea_v2_halfdeg_backby_hyde32_upper.nc" )
list_out <- c( "harvest_hurtt_byarea_v2_lpjgr_backby_hyde32_upper.nc"   )

for (ifil in 1:length(list_in)){
  regrid.landuse.holoLU2( list_in[ifil], list_out[ifil], harvest_data=TRUE )
}
