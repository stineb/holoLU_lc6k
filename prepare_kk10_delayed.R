library(ncdf4)
library(ncdf)
library(abind)
library(zoo)
source("/alphadata01/bstocker/utilities/cdf.write.R")
source("/alphadata01/bstocker/lpx/lpxtools/trunk/landuse/get.land.avail.R")
source('/alphadata01/bstocker/utilities/regrid_landuse.R')
source('/alphadata01/bstocker/utilities/area.R')

dir <- "/alphadata01/bstocker/data/landuse_data/kk10/"
yrlist <- read.table(paste(dir,"yrlist.txt",sep=""))
yrlist <- sort(yrlist$V1)

yrstring <- rep(NA, length(yrlist))
for (idx in 1:length(yrlist)){
  yrstring[idx] <- sprintf("%06i", yrlist[idx])
}

overwrite <- TRUE

if (!file.exists("kk10d.Rdata")){

  filnam <- paste(dir,"landuse_KK11_halfdeg_hydeslices.nc",sep="")
  kk11 <- open.ncdf( filnam, write=FALSE )
  outlu <- array(NA, dim=c(kk11$dim$lon$len, kk11$dim$lat$len, 59))
  time <- get.var.ncdf(kk11,"TIME")
  crop <- get.var.ncdf(kk11,"crop")
  past <- get.var.ncdf(kk11,"past")
  lon <- get.var.ncdf(kk11,"LONGITUDE")
  lat <- get.var.ncdf(kk11,"LATITUDE")
  close.ncdf(kk11)

  ##------------------------------------------------------
  ## Create region mask
  ##------------------------------------------------------
  nc <- open.ncdf( "/alphadata01/bstocker/data/masks/regmask_halfdeg.nc", write=FALSE )
  cont <- get.var.ncdf(nc,"mask")
  close.ncdf(nc)

  regs <- c("MA","SA","AF","SA","ME","CH","EU","OZ","NA","NEA")
  regs_name <- c("Meso-America","South America","Africa","South Asia","Middle-East","China","Eurasia","Ozeania","North America","Northeast Asia")
  nreg <- length(regs)

  yrzero <- rep(NA,nreg)
  yrpivot <- 1500

  regmask <- array( NA, dim=c( length(lon), length(lat), nreg ) )

  ## Meso-America
  ireg <- 1
  yrzero[ireg] <- -8000
  yrpivot[ireg] <- 1500
  regmask[,,ireg] <- cont[,,1] ## take South America + Mesoamerica
  regmask[,which(lat<12.5),ireg] <- 0.0  ## exclude all cells <12.5 (south america)

  ## South America
  ireg <- 2
  yrzero[ireg] <- -8000
  yrpivot[ireg] <- 1500
  regmask[,,ireg] <- cont[,,1] ## take South America + Mesoamerica
  regmask[,which(lat>=12.5),ireg] <- 0.0  ## exclude all cells >12.5 (south america)

  ## Sub-Saharan Africa
  ireg <- 3
  yrzero[ireg] <- -3000
  yrpivot[ireg] <- 1700
  regmask[,,ireg] <- cont[,,2] ## take Africa
  regmask[,which(lat>22),ireg] <- 0.0  ## exclude all cells south of 22 deg N

  ## northern africa
  tmp <- cont[,,2] ## take Africa
  tmp[,which(lat<=22)] <- 0.0  ## exclude all cells south of 22 deg N

  ## South Asia 
  ireg <- 4
  yrzero[ireg] <- -5000
  yrpivot[ireg] <- 1000
  regmask[,,ireg] <- cont[,,3] ## take "South Asia"
  regmask[which(lon<68),,ireg] <- 0.0  ## exclude all cells west of 68 deg E

  ## Middle East
  ireg <- 5
  yrzero[ireg] <- -8000
  yrpivot[ireg] <- 1000
  regmask[,,ireg] <- cont[,,3] ## take "South Asia"
  regmask[which(lon>=68),,ireg] <- 0.0  ## exclude all cells east of 68 deg E
  regmask[,,ireg] <- pmax( tmp, regmask[,,ireg], na.rm=TRUE )

  ## China
  ireg <- 6
  yrzero[ireg] <- -6000
  yrpivot[ireg] <- 1000
  regmask[,,ireg] <- cont[,,4] ## take "China"

  ## Eurasia
  ireg <- 7
  yrzero[ireg] <- -7000
  yrpivot[ireg] <- 1000
  regmask[,,ireg] <- cont[,,5] ## take "Western Eurasia"

  ## Ozeania  NO DELAY NEEDED
  ireg <- 8
  yrzero[ireg] <- -1500
  yrpivot[ireg] <- 1500
  regmask[,,ireg] <- cont[,,6] ## take "Ozeania"

  ## North America
  ireg <- 9
  yrzero[ireg] <- -2000
  yrpivot[ireg] <- 1500
  regmask[,,ireg] <- cont[,,7] ## take "North America"

  ## North East Asia
  ireg <- 10
  yrzero[ireg] <- -8000
  yrpivot[ireg] <- 1000
  regmask[,,ireg] <- cont[,,8] ## take "North East Asia = Siberia"

  for (ireg in 1:nreg){
    tmp <- regmask[,,ireg]
    tmp[ is.na(cont[,,1]) ] <- NA
    regmask[,,ireg] <- tmp
  }

  # regmask[ c(is.na(cont[,,1]), ireg) ] <- NA

  # Write NetCDF output
  cdf.write(
            regmask, "regmask",
            lon,
            lat,
            "regmask_holoLU.nc",
            make.zdim=TRUE, z_dim=1:nreg,
            vartype="NC_FLOAT",
            verbose=FALSE
            )
  print("done.")


  ##------------------------------------------------------
  ## Limit emergence of agriculture by region: "delayed"
  ##------------------------------------------------------
  print('delay emergence ...')
  past_del <- NA * past
  crop_del <- NA * crop

  for (ireg in 1:nreg){
  # for (ireg in 1:2){

    for (iyr in which(time>yrzero[ireg] & time<yrpivot[ireg])) {

      ## find corresponding year in un-delayed data
      yr_use <- ( time[iyr] - yrzero[ireg] ) * ( yrpivot[ireg] - (-10000) ) / ( yrpivot[ireg] - yrzero[ireg] ) + (-10000) 
      
      for (ilon in 1:length(lon)){
        for (ilat in 1:length(lat)){
          if (!is.na(crop[ilon,ilat,1])){
            if (!is.na(regmask[ilon,ilat,ireg])){
              if (regmask[ilon,ilat,ireg]==1){
                if (ireg==8){
                  
                  ## no correction for Ozeania
                  crop_del[ilon,ilat,] <- crop[ilon,ilat,]
                  past_del[ilon,ilat,] <- past[ilon,ilat,]

                } else {

                  crop_del[ilon,ilat,which(time<=yrzero[ireg])] <- 0.0
                  past_del[ilon,ilat,which(time<=yrzero[ireg])] <- 0.0

                  crop_del[ilon,ilat,iyr] <- approx( time, crop[ilon,ilat,], yr_use )$y
                  past_del[ilon,ilat,iyr] <- approx( time, past[ilon,ilat,], yr_use )$y

                  crop_del[ilon,ilat,which(time>=yrpivot[ireg])] <- crop[ilon,ilat,which(time>=yrpivot[ireg])]
                  past_del[ilon,ilat,which(time>=yrpivot[ireg])] <- past[ilon,ilat,which(time>=yrpivot[ireg])]
                
                }
              }
            }
          }
        }
      }
    }
  }


  ##------------------------------------------------------
  ## Interpolate between 1850 and 1960 to avoid weird feature
  ## with decreasing and then re-increasing cropland and 
  ## pasture area in many regions.
  ##------------------------------------------------------
  crop_int <- crop_del
  past_int <- past_del

  print('interpolating between 1850 and 1960 ...')
  for (ilon in 1:length(lon)){
    for (ilat in 1:length(lat)){
      if (!is.na(crop_int[ilon,ilat,1])){
        crop_int[ilon,ilat,which(time>1850 & time<1960)] <- NA
        crop_int[ilon,ilat,] <- na.approx( crop_int[ilon,ilat,] )
      }
    }
  }

  for (ilon in 1:length(lon)){
    for (ilat in 1:length(lat)){
      if (!is.na(past_int[ilon,ilat,1])){
        past_int[ilon,ilat,which(time>1850 & time<1960)] <- NA
        past_int[ilon,ilat,] <- na.approx( past_int[ilon,ilat,] )
      }
    }
  }


  ##------------------------------------------------------
  ## Sum up total cropland area per region total cropland over time area by region
  ##------------------------------------------------------
  print('summing over regions ...')
  area_bylat <- sapply( lat, FUN = function(x) area(x,0.5,0.5) )
  area <- NA*crop[,,1]
  for (ilon in 1:dim(crop)[1]){ area[ilon,] <- area_bylat } 

  tot_crop <- array( NA, dim=c(nreg,length(time)))
  tot_crdl <- array( NA, dim=c(nreg,length(time)))
  for (ireg in 1:nreg){
  # for (ireg in 1:2){
    for (iyr in 1:length(time)){
      tot_crop[ireg,iyr] <- sum( crop[,,iyr]     * regmask[,,ireg] * area[,], na.rm=TRUE )
      tot_crdl[ireg,iyr] <- sum( crop_del[,,iyr] * regmask[,,ireg] * area[,], na.rm=TRUE )
    }
  }
  
  save( tot_crop, tot_crdl, crop_del, past_del, lon, lat, time, file="kk10d.Rdata" )

} else {

  load("kk10d.Rdata")

}


##------------------------------------------------------
## Plot
##------------------------------------------------------
pdf( "kk10_delayed_by_cont.pdf", width=10, height=12 )
par( las=1, mfrow=c(4,3) )
for (ireg in 1:nreg){
# for (ireg in 1:2){
  ## all years
  plot( time, tot_crop[ireg,]*1e-12, type="l", xlab="year CE", ylab=expression(paste("cropland area (mio. km"^2,")")) )
  lines( time, tot_crdl[ireg,]*1e-12, col='blue' )
  title( regs_name[ireg] )
  abline( v=yrzero[ireg], col='red')
  abline( v=yrpivot[ireg], col='springgreen4')
  legend( "topleft", c("KK10", "KK10D"), lwd=1, col=c("black","blue"), bty="n" )
  text( yrzero[ireg], 0.5e-12 * range(tot_crop[ireg,])[2], paste("", as.character( yrzero[ireg] ), "CE" ), col="red", adj=c(0,0) )
  text( yrpivot[ireg], 0.7e-12 * range(tot_crop[ireg,])[2], paste("", as.character( yrpivot[ireg] ), "CE " ), col="springgreen4", adj=c(1,0) )
  # ## 1450-2000
  # plot( time, tot_crop[ireg,]*1e-12, type="l", xlab="year CE", ylab="cropland area (mio. km2)", xlim=c(1450,2000) )
  # lines( time, tot_crdl[ireg,]*1e-12, col='blue' )
  # title( regs_name[ireg] )
  # abline( v=yrzero[ireg], col='red')
  # abline( v=yrpivot[ireg], col='red')

}
plot( time, apply( tot_crop, 2, FUN = sum ) * 1e-12, type="l", xlab="year CE", ylab=expression(paste("cropland area (mio. km"^2,")")) )
lines( time, apply( tot_crdl, 2, FUN = sum ) * 1e-12, col='blue' )
title( "Global" )
legend( "topleft", c("KK10", "KK10D"), lwd=1, col=c("black","blue"), bty="n" )


dev.off()


# ##------------------------------------------------------
# ## Write NetCDF output
# ##------------------------------------------------------
# cdf.write(
#           crop_del, "crop",
#           lon,
#           lat,
#           paste("landuse_KK11delayed_halfdeg_hydeslices.nc",sep=""),
#           time=time,make.tdim=TRUE,
#           nvars=2,
#           var2=past_int, varnam2="past",
#           vartype="NC_FLOAT",
#           verbose=FALSE
#           )
# print("done.")


