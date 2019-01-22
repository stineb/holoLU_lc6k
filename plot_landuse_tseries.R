library(ncdf)
source('/alphadata01/bstocker/utilities/area.R')


add_alpha <- function( col, alpha ){
  ## add alpha to color given as a name
  col    <- col2rgb( col, alpha=TRUE )/255
  col[4] <- alpha
  col    <- rgb(col[1,],col[2,],col[3,],col[4,])
  return( col )
}

do.load <- FALSE
do.calc <- FALSE

################
## cropland area vs time
################

if (do.load){

  print("loading...")

  lufils <- c( 
    "landuse_hyde31_final_halfdeg.cdf", 
    "landuse_hyde32_baseline_halfdeg.cdf",
    "landuse_KK11_halfdeg_hydeslices.nc",
    "landuse_KK11delayed_halfdeg_hydeslices.nc"
    )

  popfils <- c(
    "population_hyde31_final_halfdeg.cdf",
    "population_hyde32_baseline_halfdeg.cdf"
    )

  name_scen <- c(
                  "HYDE 3.1",
                  "HYDE 3.2",
                  "KK10",
                  "KK10D"
                  )

  ## read files
  nc <- open.ncdf( lufils[1] )
  crop_hyde31 <- get.var.ncdf(nc, "crop_abs")*1e6  # convert to m2
  past_hyde31 <- get.var.ncdf(nc, "past_abs")*1e6  # convert to m2
  time_hyde31 <- get.var.ncdf(nc, "TIME")
  close.ncdf(nc)

  nc <- open.ncdf( lufils[2] )
  crop_hyde32 <- get.var.ncdf(nc, "crop_abs")*1e6  # convert to m2
  past_hyde32 <- get.var.ncdf(nc, "past_abs")*1e6  # convert to m2
  time_hyde32 <- get.var.ncdf(nc, "TIME")
  close.ncdf(nc)

  nc <- open.ncdf( lufils[3] )
  crop_kk10 <- get.var.ncdf(nc, "crop")
  past_kk10 <- get.var.ncdf(nc, "past")
  lon <- get.var.ncdf(nc,"LONGITUDE")
  lat <- get.var.ncdf(nc,"LATITUDE")
  time_kk10 <- get.var.ncdf(nc, "TIME")
  close.ncdf(nc)

  nc <- open.ncdf( lufils[4] )
  crop_kk10d <- get.var.ncdf(nc, "crop")
  past_kk10d <- get.var.ncdf(nc, "past")
  time_kk10d <- get.var.ncdf(nc, "TIME")
  close.ncdf(nc)

  nc <- open.ncdf( popfils[1] )
  pop_hyde31 <- get.var.ncdf(nc, "pop")
  close.ncdf(nc)

  nc <- open.ncdf( popfils[2] )
  pop_hyde32 <- get.var.ncdf(nc, "pop")
  close.ncdf(nc)

  nc <- open.ncdf( "regmask_holoLU.nc" )
  regm <- get.var.ncdf(nc,"regmask")
  close.ncdf(nc)

  ## calculate sums per continent
  regs <- c("MA","SA","AF","SA","ME","CH","EU","OZ","NA","NEA")
  regs_name <- c("Meso-America","South America","Africa","South Asia","Middle-East","China","Eurasia","Ozeania","North America","Northeast Asia")
  nreg <- length(regs)

}

## from comparison of input data
crop <- read.csv('crop_totals_hyde.csv')
crop[,2:7] <- crop[,2:7]*1e7

past <- read.csv('past_totals_hyde.csv')
past[,2:7] <- past[,2:7]*1e7

if (do.calc) {

  print("calculating ... ")

  pop_hyde31_cont <- array( NA, dim=c(length(time_hyde31),nreg))
  crp_hyde31_cont <- array( NA, dim=c(length(time_hyde31),nreg))
  pst_hyde31_cont <- array( NA, dim=c(length(time_hyde31),nreg))

  colnames(pop_hyde31_cont) <- regs
  colnames(crp_hyde31_cont) <- regs
  colnames(pst_hyde31_cont) <- regs

  rownames(pop_hyde31_cont) <- as.character(time_hyde31)
  rownames(crp_hyde31_cont) <- as.character(time_hyde31)
  rownames(pst_hyde31_cont) <- as.character(time_hyde31)

  pop_hyde32_cont <- array( NA, dim=c(length(time_hyde32),nreg))
  crp_hyde32_cont <- array( NA, dim=c(length(time_hyde32),nreg))
  pst_hyde32_cont <- array( NA, dim=c(length(time_hyde32),nreg))

  colnames(pop_hyde32_cont) <- regs
  colnames(crp_hyde32_cont) <- regs
  colnames(pst_hyde32_cont) <- regs

  rownames(pop_hyde32_cont) <- as.character(time_hyde32)
  rownames(crp_hyde32_cont) <- as.character(time_hyde32)
  rownames(pst_hyde32_cont) <- as.character(time_hyde32)

  crp_kk10_cont <- array( NA, dim=c(length(time_kk10),nreg))
  pst_kk10_cont <- array( NA, dim=c(length(time_kk10),nreg))

  colnames(crp_kk10_cont) <- regs
  colnames(pst_kk10_cont) <- regs

  rownames(crp_kk10_cont) <- as.character(time_hyde31)
  rownames(pst_kk10_cont) <- as.character(time_hyde31)

  crp_kk10d_cont <- array( NA, dim=c(length(time_kk10d),nreg))
  pst_kk10d_cont <- array( NA, dim=c(length(time_kk10d),nreg))

  colnames(crp_kk10d_cont) <- regs
  colnames(pst_kk10d_cont) <- regs

  rownames(crp_kk10d_cont) <- as.character(time_hyde31)
  rownames(pst_kk10d_cont) <- as.character(time_hyde31)

  area_bylat <- sapply( lat, FUN = function(x) area(x,0.5,0.5) )
  area <- NA*crop_kk10[,,1]
  for (ilon in 1:dim(crop_kk10)[1]){ area[ilon,] <- area_bylat } 

  for (ireg in 1:nreg){

    ## total population in this continent
    ## hyde31
    tmp_pop <- array( NA, dim=dim(pop_hyde31) )
    tmp_crp <- array( NA, dim=dim(crop_hyde31) )
    tmp_pst <- array( NA, dim=dim(past_hyde31) )
    for (itim in 1:dim(tmp_pop)[3]){
      tmp_pop[,,itim] <- pop_hyde31[,,itim] * regm[,,ireg]    
      tmp_crp[,,itim] <- crop_hyde31[,,itim] * regm[,,ireg]    
      tmp_pst[,,itim] <- past_hyde31[,,itim] * regm[,,ireg]    
    }
    pop_hyde31_cont[,ireg] <- apply( tmp_pop, c(3), FUN=sum, na.rm=TRUE )
    crp_hyde31_cont[,ireg] <- apply( tmp_crp, c(3), FUN=sum, na.rm=TRUE )
    pst_hyde31_cont[,ireg] <- apply( tmp_pst, c(3), FUN=sum, na.rm=TRUE )

    ## hyde32
    tmp_pop <- array( NA, dim=dim(pop_hyde32) )
    tmp_crp <- array( NA, dim=dim(crop_hyde32) )
    tmp_pst <- array( NA, dim=dim(past_hyde32) )
    for (itim in 1:dim(tmp_pop)[3]){
      tmp_pop[,,itim] <- pop_hyde32[,,itim] * regm[,,ireg]    
      tmp_crp[,,itim] <- crop_hyde32[,,itim] * regm[,,ireg]    
      tmp_pst[,,itim] <- past_hyde32[,,itim] * regm[,,ireg]    
    }
    pop_hyde32_cont[,ireg] <- apply( tmp_pop, c(3), FUN=sum, na.rm=TRUE )
    crp_hyde32_cont[,ireg] <- apply( tmp_crp, c(3), FUN=sum, na.rm=TRUE )
    pst_hyde32_cont[,ireg] <- apply( tmp_pst, c(3), FUN=sum, na.rm=TRUE )

    ## kk10
    tmp_crp <- array( NA, dim=dim(crop_kk10) )
    tmp_pst <- array( NA, dim=dim(past_kk10) )
    for (itim in 1:dim(tmp_crp)[3]){
      tmp_crp[,,itim] <- crop_kk10[,,itim] * area * regm[,,ireg]    
      tmp_pst[,,itim] <- past_kk10[,,itim] * area * regm[,,ireg]    
    }
    crp_kk10_cont[,ireg] <- apply( tmp_crp, c(3), FUN=sum, na.rm=TRUE )
    pst_kk10_cont[,ireg] <- apply( tmp_pst, c(3), FUN=sum, na.rm=TRUE )

    ## kk10d
    tmp_crp <- array( NA, dim=dim(crop_kk10d) )
    tmp_pst <- array( NA, dim=dim(past_kk10d) )
    for (itim in 1:dim(tmp_crp)[3]){
      tmp_crp[,,itim] <- crop_kk10d[,,itim] * area * regm[,,ireg]    
      tmp_pst[,,itim] <- past_kk10d[,,itim] * area * regm[,,ireg]    
    }
    crp_kk10d_cont[,ireg] <- apply( tmp_crp, c(3), FUN=sum, na.rm=TRUE )
    pst_kk10d_cont[,ireg] <- apply( tmp_pst, c(3), FUN=sum, na.rm=TRUE )

  }

  crp_hyde31_glob <- apply( crop_hyde31, c(3), FUN=sum, na.rm=TRUE )
  pst_hyde31_glob <- apply( past_hyde31, c(3), FUN=sum, na.rm=TRUE )
  pop_hyde31_glob <- apply( pop_hyde31,  c(3), FUN=sum, na.rm=TRUE )
  pcl_hyde31_glob <- crp_hyde31_glob / pop_hyde31_glob

  crp_hyde32_glob <- apply( crop_hyde32, c(3), FUN=sum, na.rm=TRUE )
  pst_hyde32_glob <- apply( past_hyde32, c(3), FUN=sum, na.rm=TRUE )
  pop_hyde32_glob <- apply( pop_hyde32,  c(3), FUN=sum, na.rm=TRUE )
  pcl_hyde32_glob <- crp_hyde32_glob / pop_hyde32_glob

  crp_kk10_glob  <- apply( crp_kk10_cont, c(1), FUN=sum, na.rm=TRUE )
  pst_kk10_glob  <- apply( pst_kk10_cont, c(1), FUN=sum, na.rm=TRUE )
  pcl_kk10_glob  <- crp_kk10_glob / pop_hyde31_glob

  crp_kk10d_glob <- apply( crp_kk10d_cont, c(1), FUN=sum, na.rm=TRUE )
  pst_kk10d_glob <- apply( pst_kk10d_cont, c(1), FUN=sum, na.rm=TRUE )
  pcl_kk10d_glob <- crp_kk10d_glob / pop_hyde31_glob

}

save( 
  crp_hyde31_cont,
  crp_hyde32_cont,
  crp_kk10_cont,
  crp_kk10d_cont,

  crp_hyde31_glob,
  crp_hyde32_glob,
  crp_kk10_glob,
  crp_kk10d_glob,

  pst_hyde31_cont,
  pst_hyde32_cont,
  pst_kk10_cont,
  pst_kk10d_cont,

  pst_hyde31_glob,
  pst_hyde32_glob,
  pst_kk10_glob,
  pst_kk10d_glob,

  file="landuse_tseries.Rdata"
  )


##//////////////////////////////////////////////////////
## Continent-wise plot per-capita land use in this continent over time
##------------------------------------------------------
## hyde 31
pdf( "percapitalanduse_by_cont_hyde.pdf", width=12, height=8 )
par( las=1, mfrow=c(3,4) )
for (ireg in 1:nreg){
  # plot( time_hyde31, ((crp_hyde31_cont[,ireg]+pst_hyde31_cont[,ireg])/pop_hyde31_cont[,ireg])*1e-4, type="l", xlab="year CE", ylab="per-capita cropland area (ha/cap.)", ylim=c(0,max((crp_hyde31_cont[,ireg]/pop_hyde31_cont[,ireg])*1e-4)))
  # lines( time_hyde32, ((crp_hyde32_cont[,ireg]+pst_hyde32_cont[,ireg])/pop_hyde32_cont[,ireg])*1e-4, col="blue" )
  plot( time_hyde31, ((crp_hyde31_cont[,ireg])/pop_hyde31_cont[,ireg])*1e-4, type="l", xlab="year CE", ylab="per-capita cropland area (ha/cap.)", ylim=c(0,max((crp_hyde31_cont[,ireg]/pop_hyde31_cont[,ireg])*1e-4)))
  lines( time_hyde32, ((crp_hyde32_cont[,ireg])/pop_hyde32_cont[,ireg])*1e-4, col="blue" )
  title( regs_name[ireg] )
  legend( "topleft", c("HYDE 3.1", "HYDE 3.2"), col=c("black", "blue"), lty=1, bty="n")
}
plot( time_hyde31, pcl_hyde31_glob*1e-4, type="l", xlab="year CE", ylab="per-capita cropland area (ha/cap.)", ylim=c(0,max(pcl_hyde31_glob*1e-4,pcl_hyde32_glob*1e-4)))
lines( time_hyde32, pcl_hyde32_glob*1e-4, col="blue" )
title( "Global" )
legend( "topleft", c("HYDE 3.1", "HYDE 3.2"), col=c("black", "blue"), lty=1, bty="n" )
dev.off()

## kk10
pdf( "percapitalanduse_by_cont_kk10.pdf", width=12, height=8 )
par( las=1, mfrow=c(3,4) )
for (ireg in 1:nreg){
  # plot(  time_kk10,  ((crp_kk10_cont[,ireg]+pst_kk10_cont[,ireg])/pop_hyde31_cont[,ireg])*1e-4, type="l", xlab="year CE", ylab="per-capita cropland area (ha/cap.)", ylim=c(0,max((crp_kk10_cont[,ireg]/pop_hyde31_cont[,ireg])*1e-4)))
  # lines( time_kk10d, ((crp_kk10d_cont[,ireg]+pst_kk10d_cont[,ireg])/pop_hyde31_cont[,ireg])*1e-4, col="blue" )
  plot(  time_kk10,  ((crp_kk10_cont[,ireg])/pop_hyde31_cont[,ireg])*1e-4, type="l", xlab="year CE", ylab="per-capita cropland area (ha/cap.)", ylim=c(0,max((crp_kk10_cont[,ireg]/pop_hyde31_cont[,ireg])*1e-4)))
  lines( time_kk10d, ((crp_kk10d_cont[,ireg])/pop_hyde31_cont[,ireg])*1e-4, col="blue" )
  title( regs_name[ireg] )
  legend( "topleft", c("KK10", "KK10D"), col=c("black", "blue"), lty=1, bty="n" )
}
plot( time_kk10, pcl_kk10_glob*1e-4, type="l", xlab="year CE", ylab="per-capita cropland area (ha/cap.)", ylim=c(0,max(pcl_kk10_glob*1e-4,pcl_kk10d_glob*1e-4)))
lines( time_kk10d, pcl_kk10d_glob*1e-4, col="blue" )
title( "Global" )
legend( "topleft", c("KK10", "KK10D"), col=c("black", "blue"), lty=1, bty="n" )

plot( time_kk10, crp_kk10_glob / crp_hyde31_glob, type="l", xlab="year CE", ylab="KK10:HYDE31" )

dev.off()



##//////////////////////////////////////////////////////
## Global cropland area over time
##------------------------------------------------------
magn <- 4
ncols <- 2
nrows <- 1
widths <- magn*c(2,1)
heights <- rep(magn,nrows)
order <- matrix(c(1:(nrows*ncols)),nrows,ncols,byrow =TRUE)

pdf( "cropland_area.pdf", width=sum(widths), height=sum(heights) )

  panel <- layout(
            order,
            widths=widths,
            heights=heights,
            TRUE
            )

  ## HOLOCENE
  ylim <- c(0,18)
  xlim <- c(-10000,1500)
  par( xaxs="i", yaxs="i", las=1, mar=c(5,4,1,0.5) )
  plot( xlim, ylim, type="n", xlim=xlim, ylim=ylim, axes=FALSE, ylab="global cropland area (mio. km2)", xlab="years CE" )
  axis( 1, at=seq(xlim[1],xlim[2],by=1000),tck=-0.03, lwd=1.5); axis( 1 , at=seq(xlim[1],xlim[2],by=100),labels=F, tck=-0.01)
  axis( 3, at=seq(xlim[1],xlim[2],by=1000),tck=-0.03, lwd=1.5, labels=F ); axis( 3 , at=seq(xlim[1],xlim[2],by=100),labels=F, tck=-0.01)
  axis( 2, at=seq(ylim[1],ylim[2],by=2),tck=-0.03, lwd=1.5);            axis( 2 , at=seq(ylim[1],ylim[2],by=0.5),labels=F, tck=-0.01)
  axis( 4, at=seq(ylim[1],ylim[2],by=2),tck=-0.03, lwd=1.5, labels=F ); axis( 4 , at=seq(ylim[1],ylim[2],by=0.5),labels=F, tck=-0.01)

  box( lwd=1.5 )

  lines( time_hyde31, crp_hyde31_glob*1e-12, col="tomato", lwd=1.5  )
  lines( crop$year,   crop$hyde31_in_lpjgr*1e-12, col="tomato", lwd=1.5, lty=2  )
  # polygon( c(fluc_rdc$hyde31$mill, rev(fluc_rdc$hyde31$mill)), c(fluc_rdc$hyde31$cumfluc, rev(fluc_rdc$hyde31u$cumfluc)), col=add_alpha("tomato", 0.5), border=NA )

  lines( time_hyde32, crp_hyde32_glob*1e-12, col="orchid", lwd=1.5  )
  lines( crop$year,   crop$hyde32_in_lpjgr*1e-12, col="orchid", lwd=1.5, lty=2  )
  # polygon( c(fluc_rdc$hyde32$mill, rev(fluc_rdc$hyde32$mill)), c(fluc_rdc$hyde32$cumfluc, rev(fluc_rdc$hyde32u$cumfluc)), col=add_alpha("orchid", 0.5), border=NA )

  lines( time_kk10, crp_kk10_glob*1e-12, col="turquoise3", lwd=1.5, lty=2 )
  lines( time_kk10d, crp_kk10d_glob*1e-12, col="turquoise3", lwd=1.5 )

  ## grey bands for periods (holocene)
  periodsBP <- read.csv( 'periods_holocene.csv' )$periodsBP
  periodsAD <- periodsBP + 1950
  periodsName <- paste( 
    as.character( -periodsBP*1e-3 )[1:length(periodsBP)-1],
    "-",
    as.character( -periodsBP*1e-3 )[2:length(periodsBP)],
    sep=""
    )
  period_margins <- periodsAD
  nper <- length(period_margins)-1
  rect( period_margins[1], ylim[1], period_margins[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], ylim[1], period_margins[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5], ylim[1], period_margins[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

  legend( "topleft", c("HYDE 3.1", "HYDE 3.2", "KK10", "KK10D" ), col=c("tomato","orchid","turquoise3","turquoise3"), bty="n", lty=c(1,1,2,1), lwd=1.5 )

  ## LAST MILLENNIUM
  xlim <- c(1500,2000)
  par( xaxs="i", yaxs="i", las=1, mar=c(5,0.5,1,4))
  plot( xlim, ylim, type="n", xlim=xlim, ylim=ylim, axes=FALSE, ylab="", xlab="years CE", lwd=1.5)
  axis( 1, at=seq(xlim[1],xlim[2],by=100),tck=-0.03, lwd=1.5); axis( 1 , at=seq(xlim[1],xlim[2],by=10),labels=F, tck=-0.01)
  axis( 3, at=seq(xlim[1],xlim[2],by=100),tck=-0.03, lwd=1.5, labels=F ); axis( 3 , at=seq(xlim[1],xlim[2],by=10),labels=F, tck=-0.01)
  axis( 2, at=seq(ylim[1],ylim[2],by=2),tck=-0.03, lwd=1.5, labels=F); axis( 2 , at=seq(ylim[1],ylim[2],by=0.5),labels=F, tck=-0.01)
  axis( 4, at=seq(ylim[1],ylim[2],by=2),tck=-0.03, lwd=1.5 ); axis( 4 , at=seq(ylim[1],ylim[2],by=0.5),labels=F, tck=-0.01)
  box( lwd=1.5 )

  lines( time_hyde31, crp_hyde31_glob*1e-12, col="tomato", lwd=1.5  )
  lines( crop$year,  crop$hyde31, col="tomato", lwd=1.5  )

  # polygon( c(fluc_rdc$hyde31$year, rev(fluc_rdc$hyde31$year)), c(fluc_rdc$hyde31$cumfluc, rev(fluc_rdc$hyde31u$cumfluc)), col=add_alpha("tomato", 0.5), border=NA )

  lines( time_hyde32, crp_hyde32_glob*1e-12, col="orchid", lwd=1.5  )
  # polygon( c(fluc_rdc$hyde32$year, rev(fluc_rdc$hyde32$year)), c(fluc_rdc$hyde32$cumfluc, rev(fluc_rdc$hyde32u$cumfluc)), col=add_alpha("orchid", 0.5), border=NA )

  lines( time_kk10, crp_kk10_glob*1e-12, col="turquoise3", lwd=1.5, lty=2 )
  lines( time_kk10d, crp_kk10d_glob*1e-12, col="turquoise3", lwd=1.5 )

  period_margins <- read.csv( 'periods_lastmill.csv' )$period_margins
  periodsName <- paste( 
    as.character( period_margins )[1:length(period_margins)-1],
    "-",
    as.character( period_margins )[2:length(period_margins)],
    sep=""
    )
  nper <- length(period_margins)-1
  rect( period_margins[1], ylim[1], period_margins[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], ylim[1], period_margins[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5], ylim[1], period_margins[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[7], ylim[1], period_margins[8], ylim[2], col=rgb(0,0,0,0.1), border=NA )

dev.off()

## organise data
hyde31 <- data.frame( 
  crp=apply( crp_hyde31_cont[,c(1,2)], 1, FUN=sum )[26:28]*1e-12,
  pst=apply( pst_hyde31_cont[,c(1,2)], 1, FUN=sum )[26:28]*1e-12,
  pop=apply( pop_hyde31_cont[,c(1,2)], 1, FUN=sum )[26:28]
  )
hyde31$tot <- hyde31$crp+hyde31$pst

hyde32 <- data.frame( 
  crp=apply( crp_hyde32_cont[,c(1,2)], 1, FUN=sum )[26:28]*1e-12,
  pst=apply( pst_hyde32_cont[,c(1,2)], 1, FUN=sum )[26:28]*1e-12,
  pop=apply( pop_hyde32_cont[,c(1,2)], 1, FUN=sum )[26:28]
  )
hyde32$tot <- hyde32$crp+hyde32$pst

kk10 <- data.frame( 
  crp=apply( crp_kk10_cont[,c(1,2)], 1, FUN=sum )[26:28]*1e-12,
  pst=apply( pst_kk10_cont[,c(1,2)], 1, FUN=sum )[26:28]*1e-12
  )
kk10$tot <- kk10$crp+kk10$pst

SMAm <- list( hyde31=hyde31, hyde32=hyde32, kk10=kk10 )

hyde31 <- data.frame( 
  crp=crp_hyde31_cont[26:28,9]*1e-12,
  pst=pst_hyde31_cont[26:28,9]*1e-12,
  pop=pop_hyde31_cont[26:28,9]
  )
hyde31$tot <- hyde31$crp+hyde31$pst

hyde32 <- data.frame( 
  crp=crp_hyde32_cont[26:28,9]*1e-12,
  pst=pst_hyde32_cont[26:28,9]*1e-12,
  pop=pop_hyde32_cont[26:28,9]
  )
hyde32$tot <- hyde32$crp+hyde32$pst

kk10 <- data.frame( 
  crp=crp_kk10_cont[26:28,9]*1e-12,
  pst=pst_kk10_cont[26:28,9]*1e-12
  )
kk10$tot <- kk10$crp+kk10$pst

NAm <- list( hyde31=hyde31, hyde32=hyde32, kk10=kk10 )

# print("########################")
# print("CROPLAND")
# print("########################")

# print("---------------------------------------------")
# print("SOUTH AND MESO America")
# print("---------------------------------------------")

# ## Get numbers
# print("HYDE 3.1")
# print("Population in south and meso america (mio)")
# tmp_pop <- (apply( pop_hyde31_cont[,c(1,2)], 1, FUN=sum )[26:28])
# print( tmp_pop*1e-6 )
# print("Cropland area in south and meso america (mio km2)")
# tmp_crp <- apply( crp_hyde31_cont[,c(1,2)], 1, FUN=sum )[26:28] 
# print( tmp_crp*1e-12 )
# print("per-captita cropland area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("---------------------------------------------")
# print("HYDE 3.2")
# print("Population in south and meso america (mio)")
# tmp_pop <- (apply( pop_hyde32_cont[,c(1,2)], 1, FUN=sum )[26:28])
# print( tmp_pop*1e-6 )
# print("Cropland area in south and meso america (mio km2)")
# tmp_crp <- apply( crp_hyde32_cont[,c(1,2)], 1, FUN=sum )[26:28] 
# print( tmp_crp*1e-12 )
# print("per-captita cropland area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("---------------------------------------------")
# print("HYDE KK10")
# print("Population in south and meso america (mio)")
# tmp_pop <- (apply( pop_hyde31_cont[,c(1,2)], 1, FUN=sum )[26:28])
# print( tmp_pop*1e-6 )
# print("Cropland area in south and meso america (mio km2)")
# tmp_crp <- apply( crp_kk10_cont[,c(1,2)], 1, FUN=sum )[26:28] 
# print( tmp_crp*1e-12 )
# print("per-captita cropland area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("per-captita cropland area (ha / cap) based on HYDE32 population")
# tmp_pop <- (apply( pop_hyde32_cont[,c(1,2)], 1, FUN=sum )[26:28])
# print( (tmp_crp / tmp_pop)*1e-4 )

# print("---------------------------------------------")
# print("NORTH America")
# print("---------------------------------------------")

# print("HYDE 3.1")
# print("Population in north america (mio)")
# tmp_pop <- pop_hyde31_cont[26:28,9]
# print( tmp_pop*1e-6 )
# print("Cropland area in north america (mio km2)")
# tmp_crp <-crp_hyde31_cont[26:28,9]
# print( tmp_crp*1e-12 )
# print("per-captita cropland area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("---------------------------------------------")
# print("HYDE 3.2")
# print("Population in north america (mio)")
# tmp_pop <- pop_hyde32_cont[26:28,9]
# print( tmp_pop*1e-6 )
# print("Cropland area in north america (mio km2)")
# tmp_crp <- crp_hyde32_cont[26:28,9]
# print( tmp_crp*1e-12 )
# print("per-captita cropland area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("---------------------------------------------")
# print("HYDE KK10")
# print("Population in north america (mio)")
# tmp_pop <- pop_hyde31_cont[26:28,9]
# print( tmp_pop*1e-6 )
# print("Cropland area in north america (mio km2)")
# tmp_crp <- crp_kk10_cont[26:28,9]
# print( tmp_crp*1e-12 )
# print("per-captita cropland area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("per-captita cropland area (ha / cap) based on HYDE32 population")
# tmp_pop <- pop_hyde32_cont[26:28,9]
# print( (tmp_crp / tmp_pop)*1e-4 )

# print("########################")
# print("PASTURE")
# print("########################")

# print("---------------------------------------------")
# print("SOUTH AND MESO America")
# print("---------------------------------------------")

# ## Get numbers
# print("HYDE 3.1")
# print("Population in south and meso america (mio)")
# tmp_pop <- (apply( pop_hyde31_cont[,c(1,2)], 1, FUN=sum )[26:28])
# print( tmp_pop*1e-6 )
# print("pasture area in south and meso america (mio km2)")
# tmp_crp <- apply( pst_hyde31_cont[,c(1,2)], 1, FUN=sum )[26:28]
# print( tmp_crp*1e-12 )
# print("per-captita pasture area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("---------------------------------------------")
# print("HYDE 3.2")
# print("Population in south and meso america (mio)")
# tmp_pop <- (apply( pop_hyde32_cont[,c(1,2)], 1, FUN=sum )[26:28])
# print( tmp_pop*1e-6 )
# print("pasture area in south and meso america (mio km2)")
# tmp_crp <- apply( pst_hyde32_cont[,c(1,2)], 1, FUN=sum )[26:28]
# print( tmp_crp*1e-12 )
# print("per-captita pasture area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("---------------------------------------------")
# print("HYDE KK10")
# print("Population in south and meso america (mio)")
# tmp_pop <- (apply( pop_hyde31_cont[,c(1,2)], 1, FUN=sum )[26:28])
# print( tmp_pop*1e-6 )
# print("pasture area in south and meso america (mio km2)")
# tmp_crp <- apply( pst_kk10_cont[,c(1,2)], 1, FUN=sum )[26:28]
# print( tmp_crp*1e-12 )
# print("per-captita pasture area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("per-captita pasture area (ha / cap) based on HYDE32 population")
# tmp_pop <- (apply( pop_hyde32_cont[,c(1,2)], 1, FUN=sum )[26:28])
# print( (tmp_crp / tmp_pop)*1e-4 )

# print("---------------------------------------------")
# print("NORTH America")
# print("---------------------------------------------")

# print("HYDE 3.1")
# print("Population in north america (mio)")
# tmp_pop <- pop_hyde31_cont[26:28,9]
# print( tmp_pop*1e-6 )
# print("pasture area in north america (mio km2)")
# tmp_crp <- pst_hyde31_cont[26:28,9]
# print( tmp_crp*1e-12 )
# print("per-captita pasture area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("---------------------------------------------")
# print("HYDE 3.2")
# print("Population in north america (mio)")
# tmp_pop <- pop_hyde32_cont[26:28,9]
# print( tmp_pop*1e-6 )
# print("pasture area in north america (mio km2)")
# tmp_crp <- pst_hyde32_cont[26:28,9]
# print( tmp_crp*1e-12 )
# print("per-captita pasture area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("---------------------------------------------")
# print("HYDE KK10")
# print("Population in north america (mio)")
# tmp_pop <- pop_hyde31_cont[26:28,9]
# print( tmp_pop*1e-6 )
# print("pasture area in north america (mio km2)")
# tmp_crp <- pst_kk10_cont[26:28,9]
# print( tmp_crp*1e-12 )
# print("per-captita pasture area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("per-captita pasture area (ha / cap) based on HYDE32 population")
# tmp_pop <- pop_hyde32_cont[26:28,9]
# print( (tmp_crp / tmp_pop)*1e-4 )


##########################################################################



# print("########################")
# print("CROPLAND")
# print("########################")

# print("---------------------------------------------")
# print("ALL AMERICAS")
# print("---------------------------------------------")

# ## Get numbers
# print("HYDE 3.1")
# print("Population in south and meso america (mio)")
# tmp_pop <- (apply( pop_hyde31_cont[,c(1,2,9)], 1, FUN=sum )[26:28])
# print( tmp_pop*1e-6 )
# print("Cropland area in south and meso america (mio km2)")
# tmp_crp <- apply( crp_hyde31_cont[,c(1,2,9)], 1, FUN=sum )[26:28] 
# print( tmp_crp*1e-12 )
# print("per-captita cropland area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("---------------------------------------------")
# print("HYDE 3.2")
# print("Population in south and meso america (mio)")
# tmp_pop <- (apply( pop_hyde32_cont[,c(1,2,9)], 1, FUN=sum )[26:28])
# print( tmp_pop*1e-6 )
# print("Cropland area in south and meso america (mio km2)")
# tmp_crp <- apply( crp_hyde32_cont[,c(1,2,9)], 1, FUN=sum )[26:28] 
# print( tmp_crp*1e-12 )
# print("per-captita cropland area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("---------------------------------------------")
# print("HYDE KK10")
# print("Population in south and meso america (mio)")
# tmp_pop <- (apply( pop_hyde31_cont[,c(1,2,9)], 1, FUN=sum )[26:28])
# print( tmp_pop*1e-6 )
# print("Cropland area in south and meso america (mio km2)")
# tmp_crp <- apply( crp_kk10_cont[,c(1,2,9)], 1, FUN=sum )[26:28] 
# print( tmp_crp*1e-12 )
# print("per-captita cropland area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("per-captita cropland area (ha / cap) based on HYDE32 population")
# tmp_pop <- (apply( pop_hyde32_cont[,c(1,2,9)], 1, FUN=sum )[26:28])
# print( (tmp_crp / tmp_pop)*1e-4 )

# print("########################")
# print("PASTURE")
# print("########################")

# print("---------------------------------------------")
# print("ALL AMERICAS")
# print("---------------------------------------------")

# ## Get numbers
# print("HYDE 3.1")
# print("Population in south and meso america (mio)")
# tmp_pop <- (apply( pop_hyde31_cont[,c(1,2,9)], 1, FUN=sum )[26:28])
# print( tmp_pop*1e-6 )
# print("pasture area in south and meso america (mio km2)")
# tmp_crp <- apply( pst_hyde31_cont[,c(1,2,9)], 1, FUN=sum )[26:28]
# print( tmp_crp*1e-12 )
# print("per-captita pasture area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("---------------------------------------------")
# print("HYDE 3.2")
# print("Population in south and meso america (mio)")
# tmp_pop <- (apply( pop_hyde32_cont[,c(1,2,9)], 1, FUN=sum )[26:28])
# print( tmp_pop*1e-6 )
# print("pasture area in south and meso america (mio km2)")
# tmp_crp <- apply( pst_hyde32_cont[,c(1,2,9)], 1, FUN=sum )[26:28]
# print( tmp_crp*1e-12 )
# print("per-captita pasture area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("---------------------------------------------")
# print("HYDE KK10")
# print("Population in south and meso america (mio)")
# tmp_pop <- (apply( pop_hyde31_cont[,c(1,2,9)], 1, FUN=sum )[26:28])
# print( tmp_pop*1e-6 )
# print("pasture area in south and meso america (mio km2)")
# tmp_crp <- apply( pst_kk10_cont[,c(1,2,9)], 1, FUN=sum )[26:28]
# print( tmp_crp*1e-12 )
# print("per-captita pasture area (ha / cap)")
# print( tmp_crp/tmp_pop*1e-4 )
# print("per-captita pasture area (ha / cap) based on HYDE32 population")
# tmp_pop <- (apply( pop_hyde32_cont[,c(1,2,9)], 1, FUN=sum )[26:28])
# print( (tmp_crp / tmp_pop)*1e-4 )


##########################################################################



print("########################")
print("CROPLAND PLUS PASTURE")
print("########################")

print("---------------------------------------------")
print("ALL AMERICAS")
print("---------------------------------------------")

## Get numbers
print("HYDE 3.1")
print("Population in all america (mio)")
tmp_pop <- (apply( pop_hyde31_cont[,c(1,2,9)], 1, FUN=sum )[26:28])
print( tmp_pop*1e-6 )
print("Cropland area in all america (mio km2)")
tmp_crp <- apply( crp_hyde31_cont[,c(1,2,9)], 1, FUN=sum )[26:28] + apply( pst_hyde31_cont[,c(1,2,9)], 1, FUN=sum )[26:28] 
print( tmp_crp*1e-12 )
print("per-captita total agricultural area (ha / cap)")
print( tmp_crp/tmp_pop*1e-4 )
print("---------------------------------------------")
print("HYDE 3.2")
print("Population in all america (mio)")
tmp_pop <- (apply( pop_hyde32_cont[,c(1,2,9)], 1, FUN=sum )[26:28])
print( tmp_pop*1e-6 )
print("total agricultural area in all america (mio km2)")
tmp_crp <- apply( crp_hyde32_cont[,c(1,2,9)], 1, FUN=sum )[26:28] + apply( pst_hyde32_cont[,c(1,2,9)], 1, FUN=sum )[26:28] 
print( tmp_crp*1e-12 )
print("per-captita total agricultural area (ha / cap)")
print( tmp_crp/tmp_pop*1e-4 )
print("---------------------------------------------")
print("HYDE KK10")
print("Population in all america (mio)")
tmp_pop <- (apply( pop_hyde31_cont[,c(1,2,9)], 1, FUN=sum )[26:28])
print( tmp_pop*1e-6 )
print("total agricultural area in all america (mio km2)")
tmp_crp <- apply( crp_kk10_cont[,c(1,2,9)], 1, FUN=sum )[26:28] + apply( pst_kk10_cont[,c(1,2,9)], 1, FUN=sum )[26:28] 
print( tmp_crp*1e-12 )
print("per-captita total agricultural area (ha / cap)")
print( tmp_crp/tmp_pop*1e-4 )
print("per-captita total agricultural area (ha / cap) based on HYDE32 population")
tmp_pop <- (apply( pop_hyde32_cont[,c(1,2,9)], 1, FUN=sum )[26:28])
print( (tmp_crp / tmp_pop)*1e-4 )



