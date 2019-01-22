library(ncdf)
source("/alphadata01/bstocker/utilities/cdf.write.R")

dir <- "/alphadata01/bstocker/data/landuse_data/kk10/"
yrlist <- read.table(paste(dir,"yrlist.txt",sep=""))
yrlist <- sort(yrlist$V1)
outyrlist <- c(-10000,-9000,-8000,-7000,yrlist,2005)

filnam <- "fcrop_fpast_hyde31_final_halfdeg.nc"
nc <- open.ncdf( filnam, write=FALSE )
fpast <- get.var.ncdf(nc,"FPAST")
close.ncdf(nc)

outfpast <- array(NA, dim=c(nc$dim$LONGITUDE$len, nc$dim$LATITUDE$len, 59))
outcrop <- array(NA, dim=c(nc$dim$LONGITUDE$len, nc$dim$LATITUDE$len, 59))

for (ilon in 1:nc$dim$LONGITUDE$len){
  for (ilat in 1:nc$dim$LATITUDE$len){

    if (!is.na(fpast[ilon,ilat,59])){

      ## find earliest data point where pasture/cropland information is avaiable in HYDE 
      itim <- 1
      while (is.na(fpast[ilon,ilat,itim])){ itim <- itim + 1 }
      itim <- min(itim,length(outyrlist))
      
      outfpast[ilon,ilat,1:itim] <- fpast[ilon,ilat,itim]

      ## use remaining years' information from hyde to make disctinction
      outfpast[ilon,ilat,(itim+1):length(outyrlist)] <- fpast[ilon,ilat,(itim+1):length(outyrlist)]

    }
  }
}

cdf.write(outfpast,"fpast",
          nc$dim$LONGITUDE$vals, nc$dim$LATITUDE$vals,
          "fpast_STAGE1_hyde31_final_halfdeg.nc",
          z_dim=NA,time=outyrlist,
          make.zdim=FALSE,make.tdim=TRUE,
          nvars=1,
          vartype="NC_FLOAT",
          verbose=FALSE
          )