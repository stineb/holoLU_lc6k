library(ncdf4)
library(ncdf)
library(abind)
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

# filnam <- paste(dir,"landuse_KK11_halfdeg_",yrstring[1],".nc",sep="")
filnam <- paste(dir,"KK11_30m_8k_merge.nc",sep="")
kk11 <- open.ncdf( filnam, write=FALSE )
outlu <- array(NA, dim=c(kk11$dim$lon$len, kk11$dim$lat$len, 59))
time_kk11 <- get.var.ncdf(kk11,"time")
adtime_kk11 <- 1950-time_kk11

## fill time slices with KK11 data where available (years in yrlist)
for (idx in 1:length(yrlist)){
  iyr <- which.min( abs( yrlist[idx]-adtime_kk11 ) )
  outlu[,,idx+4] <- get.var.ncdf(kk11,"land_use",start=c(1,1,iyr),count=c(1,1,1))
  # nc <- open.ncdf(paste(dir,"landuse_KK11_halfdeg_",yrstring[idx],".nc",sep=""))
  # outlu[,,idx+4] <- get.var.ncdf(nc,"land_use")
  # close.ncdf(nc)
}
close.ncdf(kk11)

## prepare this file beforehand running first prepare_fpast_fcrop.R and then prepare_fpast.jnl
filnam <- "fpast_STAGE2_hyde31_final_halfdeg.nc"
nc <- open.ncdf( filnam, write=FALSE )
fpast <- get.var.ncdf(nc,"FPAST_FILLED")
close.ncdf(nc)

## extend year list to cover all HYDE slices
outyrlist <- c(-10000,-9000,-8000,-7000,yrlist,2005)
yrstring <- rep(NA, length(outyrlist))
for (idx in 1:length(outyrlist)){
  yrstring[idx] <- sprintf("%06i", outyrlist[idx])
}

## for year -10000, all is zero
outlu[,,1] <- 0.0

## for hyde-year 2005, take values of 2000
outlu[,,59] <- outlu[,,58]

## land-sea mask: NA
outlu[ is.na(outlu[,,5]) ] <- NA

outcrop <- outlu
outpast <- outlu
for (ilon in 1:kk11$dim$lon$len){
  for (ilat in 1:kk11$dim$lat$len){
    if (!is.na(outlu[ilon,ilat,5])){

      ## interpolate to zero for earlier years
      outlu[ilon,ilat,1:4] <- approx( c(outyrlist[1],outyrlist[5]), c(outlu[ilon,ilat,1],outlu[ilon,ilat,5]), outyrlist[1:4] )$y

      ## use information from hyde to make disctinction between crop/past
      outpast[ilon,ilat,] <- outlu[ilon,ilat,] * fpast[ilon,ilat,]
      outcrop[ilon,ilat,] <- outlu[ilon,ilat,] * (1.0 - fpast[ilon,ilat,])

    }
  }
}

##------------------------------------------------------
## Create region mask
##------------------------------------------------------
nc <- open.ncdf( "/alphadata01/bstocker/data/masks/regmask_halfdeg.nc", write=FALSE )
cont <- get.var.ncdf(nc,"mask")

regs <- c("MA","SA","AF","SA","ME","CH","EU","OZ","NA","NEA")
regs_name <- c("Meso-America","South America","Africa","South Asia","Middle-East","China","Eurasia","Ozeania","North America","Northeast Asia")
nreg <- length(regs)

yrzero <- rep(NA,nreg)
yrpivot <- 1500
iyrpivot <- which( yrpivot==outyrlist )

regmask <- array( NA, dim=c( kk11$dim$lon$len, kk11$dim$lat$len, nreg ) )

## Meso-America
ireg <- 1
yrzero[ireg] <- -8000
regmask[,,ireg] <- cont[,,1] ## take South America + Mesoamerica
regmask[,which(kk11$dim$lat$vals<12.5),ireg] <- 0.0  ## exclude all cells <12.5 (south america)

## South America
ireg <- 2
yrzero[ireg] <- -8000
regmask[,,ireg] <- cont[,,1] ## take South America + Mesoamerica
regmask[,which(kk11$dim$lat$vals>=12.5),ireg] <- 0.0  ## exclude all cells >12.5 (south america)

## Sub-Saharan Africa
ireg <- 3
yrzero[ireg] <- -1500
regmask[,,ireg] <- cont[,,2] ## take Africa
regmask[,which(kk11$dim$lat$vals>22),ireg] <- 0.0  ## exclude all cells south of 22 deg N

## South Asia 
ireg <- 4
yrzero[ireg] <- -3000
regmask[,,ireg] <- cont[,,3] ## take "South Asia"
regmask[which(kk11$dim$lon$vals<68),,ireg] <- 0.0  ## exclude all cells west of 68 deg E

## Middle East
ireg <- 5
yrzero[ireg] <- -8000
regmask[,,ireg] <- cont[,,3] ## take "South Asia"
regmask[which(kk11$dim$lon$vals>=68),,ireg] <- 0.0  ## exclude all cells east of 68 deg E

## China
ireg <- 6
yrzero[ireg] <- -3500
regmask[,,ireg] <- cont[,,4] ## take "China"

## Eurasia
ireg <- 7
yrzero[ireg] <- -6000
regmask[,,ireg] <- cont[,,4] ## take "Western Eurasia"

## Ozeania
ireg <- 8
yrzero[ireg] <- -1000
regmask[,,ireg] <- cont[,,6] ## take "Ozeania"

## North America
ireg <- 9
yrzero[ireg] <- -2000
regmask[,,ireg] <- cont[,,7] ## take "North America"

## North East Asia
ireg <- 10
yrzero[ireg] <- -2000
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
          kk11$dim$lon$vals,
          kk11$dim$lat$vals,
          "regmask_holoLU.nc",
          make.zdim=TRUE, z_dim=1:nreg,
          vartype="NC_FLOAT",
          verbose=FALSE
          )
print("done.")


##------------------------------------------------------
## Plot total cropland over time area by region
##------------------------------------------------------
area_bylat <- sapply( kk11$dim$lat$vals, FUN = function(x) area(x,0.5,0.5) )
area <- NA*outcrop[,,1]
for (ilon in 1:dim(outcrop)[1]){ area[ilon,] <- area_bylat } 

tot <- array( NA, dim=c(nreg,length(outyrlist)))
for (ireg in 1:nreg){
  for (iyr in 1:length(outyrlist)){
    tot[ireg,iyr] <- sum( outcrop[,,iyr] * regmask[,,ireg] * area[,], na.rm=TRUE )
  }
}


tot <- array( NA, dim=c(nreg,length(outyrlist)))
for (ireg in 1:nreg){

  plot( outyrlist, tot[ireg,], type="l" )

}



##------------------------------------------------------
## Limit emergence of agriculture by region: "delayed"
##------------------------------------------------------
outpast_del <- NA * outpast
outcrop_del <- NA * outcrop

for (ireg in 1:nreg){

  outpast_del[,,which(outyrlist<=yrzero[ireg])] <- 0.0
  outcrop_del[,,which(outyrlist<=yrzero[ireg])] <- 0.0

  for (iyr in which(outyrlist>yrzero[ireg])) {

    ## find corresponding year in un-delayed data
    iyr_use <- 

  }

}



# regrid to make conform with HYDE (gicew), one file per time slice
for (itim in 1:length(outyrlist)){

  ioutlu <- abind( outcrop[,,itim], outpast[,,itim], along=3 )

  ## Call regridding function
  out.regrid.landuse <- regrid.landuse( ioutlu, land.avail$avail, fraction=TRUE, aligned=TRUE, lon=nc$dim$lon$vals, lat=nc$dim$lat$vals, verbose=TRUE )

  # Write NetCDF output
  cdf.write(
            out.regrid.landuse$lu.rel[,,1], "crop",
            out.regrid.landuse$lon,
            out.regrid.landuse$lat,
            paste("landuse_KK11_halfdeg_",yrstring[itim],".nc",sep=""),
            time=outyrlist[itim],make.tdim=TRUE,
            nvars=2,
            var2=out.regrid.landuse$lu.rel[,,2], varnam2="past",
            vartype="NC_FLOAT",
            verbose=FALSE
            )
  print("done.")

}

# combine time slices into single file again (stupid)
outlu <- array(NA, dim=c(nc$dim$lon$len, nc$dim$lat$len, 59, 2))

for (idx in 1:length(outyrlist)){

  print(paste(dir,"landuse_KK11_halfdeg_",yrstring[idx],".nc",sep=""))
  nc <- open.ncdf(paste("landuse_KK11_halfdeg_",yrstring[idx],".nc",sep=""))
  outlu[,,idx,1] <- get.var.ncdf(nc,"crop")
  outlu[,,idx,2] <- get.var.ncdf(nc,"past")
  close.ncdf(nc)

}

# Write NetCDF output
cdf.write(
          outlu[,,,1], "crop",
          out.regrid.landuse$lon,
          out.regrid.landuse$lat,
          paste("landuse_KK11_halfdeg_hydeslices.nc",sep=""),
          time=outyrlist,make.tdim=TRUE,
          nvars=2,
          var2=outlu[,,,2], varnam2="past",
          vartype="NC_FLOAT",
          verbose=FALSE
          )
print("done.")


