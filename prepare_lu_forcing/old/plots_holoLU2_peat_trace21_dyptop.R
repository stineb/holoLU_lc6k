## ///////////////////////////////////////////////////////////////
## READ DATA
## ---------------------------------------------------------------
## get change in peat C ~= NEP
runningdiff <- function( vec ){
  len  <- length( vec )
  diff <- rep( NA, len )
  diff[1] <- 0.0
  diff[2:len] <- vec[2:len] - vec[1:(len-1)]
  return( diff )
}

trace129 <- list()

## peat variables averaged across sites
filn <- "peatvars_avg_across_sites_trace21_129.dat"
col.names <- c( "year", "peatc_dens_avg_site", "peatnep_dens_avg_site", "peatnpp_dens_avg_site", "peatrh_dens_avg_site", "peatarea_avg_site" )
trace129$vars_site <- read.table( filn, col.names=col.names )
spar <- 0.7
trace129$vars_site$peatc_dens_avg_site.spl <- smooth.spline( trace129$vars_site$year, trace129$vars_site$peatc_dens_avg_site, spar=spar )$y

## peatland area
filn <- "peatarea_trace21_129.dat"
col.names <- c( "year", "peatarea_global", "peatarea_maskedby_yu", "peatarea_maskedby_yu_north" )
trace129$area <- read.table( filn, col.names=col.names )

## add scaled area 
scale <- 3.7e12 / mean(trace129$area$peatarea_maskedby_yu_north[2190:2204])
trace129$area$peatarea_maskedby_yu_north_scaled <- trace129$area$peatarea_maskedby_yu_north * scale

## peat C
filn <- "peatc_trace21_129.dat"
col.names <- c( "year", "peatc_global", "peatc_maskedby_yu", "peatc_maskedby_yu_north", "netpeat" )
trace129$c <- read.table( filn, col.names=col.names )

# add splines
spar <- 0.7
trace129$c$peatc_global.spl            <- smooth.spline( trace129$c$year, trace129$c$peatc_global, spar=spar )$y
trace129$c$peatc_maskedby_yu.spl       <- smooth.spline( trace129$c$year, trace129$c$peatc_maskedby_yu, spar=spar )$y
trace129$c$peatc_maskedby_yu_north.spl <- smooth.spline( trace129$c$year, trace129$c$peatc_maskedby_yu_north, spar=spar )$y

## peat dC (GtC/yr; conversion factor 10 because output is given at 10 yr year steps)
trace129$dc$year                        <- trace129$c$year
trace129$dc$netpeat_dc                  <- runningdiff( trace129$c$netpeat )/10
trace129$dc$peatdc_global               <- runningdiff( trace129$c$peatc_global )/10
trace129$dc$peatdc_maskedby_yu          <- runningdiff( trace129$c$peatc_maskedby_yu )/10
trace129$dc$peatdc_maskedby_yu_north    <- runningdiff( trace129$c$peatc_maskedby_yu_north )/10
trace129$vars_site$peatdc_dens_avg_site <- runningdiff( trace129$vars_site$peatc_dens_avg_site )/10

filn <- "peatglobnep_trace21_129.dat"
col.names <- c( "year", "peatglobnep_global", "peatglobnep_maskedby_yu", "peatglobnep_maskedby_yu_north" )
trace129$globnep <- read.table( filn, col.names=col.names )

filn <- "peatnep_trace21_129.dat"
col.names <- c( "year", "peatnep_global", "peatnep_maskedby_yu", "peatnep_maskedby_yu_north" )
trace129$nep <- read.table( filn, col.names=col.names )

filn <- "peatnpp_trace21_129.dat"
col.names <- c( "year", "peatnpp_global", "peatnpp_maskedby_yu", "peatnpp_maskedby_yu_north" )
trace129$npp <- read.table( filn, col.names=col.names )

filn <- "peatrh_trace21_129.dat"
col.names <- c( "year", "peatrh_global", "peatrh_maskedby_yu", "peatrh_maskedby_yu_north" )
trace129$rh <- read.table( filn, col.names=col.names )

## data for Charly
write.csv( trace129$area, row.names=FALSE, file="peatland_area_lpx_trace129.csv" )
save( trace129, file="peatland_data_lpx_trace129.Rdata" )



# ## get change in peat C ~= NEP
# runningdiff <- function( vec, per ){
#   len  <- length( vec )
#   diff <- rep( NA, len )
#   i <- 0
#   idx_end <- per
#   while (idx_end < len){
#     idx_start   <- i * per + 1
#     idx_end     <- i * per + per 
#     diff[idx_start:idx_end] <- ( vec[idx_end] - vec[idx_start] ) / per
#     i <- i + 1
#   }
#   return( diff )
# }

# ## spline and moving average
# spar <- 0.7
# trace129$vars_site$peatdc_dens_avg_site.spl <- smooth.spline( trace129$vars_site$year, trace129$vars_site$peatdc_dens_avg_site, spar=spar )$y
# trace129$vars_site$peatdc_dens_avg_site.avg <- filter( trace129$vars_site$peatdc_dens_avg_site, rep(1/100,100), sides=1 )
# trace129$vars_site$peatnep_dens_avg_site.spl <- smooth.spline( trace129$vars_site$year, trace129$vars_site$peatnep_dens_avg_site, spar=spar )$y

# ## splines and moving averages
# trace129$dc$peatdc_global.spl <- smooth.spline( trace129$dc$year, trace129$dc$peatdc_global, spar=spar )$y
# trace129$dc$peatdc_maskedby_yu.spl <- smooth.spline( trace129$dc$year, trace129$dc$peatdc_maskedby_yu, spar=spar )$y
# trace129$dc$peatdc_maskedby_yu_north.spl <- smooth.spline( trace129$dc$year, trace129$dc$peatdc_maskedby_yu_north, spar=spar )$y

# trace129$dc$peatdc_global.avg <- filter( trace129$dc$peatdc_global, rep(1/100,100), sides=1 )
# trace129$dc$peatdc_maskedby_yu.avg <- filter( trace129$dc$peatdc_maskedby_yu, rep(1/100,100), sides=1 )
# trace129$dc$peatdc_maskedby_yu_north.avg <- filter( trace129$dc$peatdc_maskedby_yu_north, rep(1/100,100), sides=1 )

# trace129$nep$peatnep_global.spl <- smooth.spline( trace129$nep$year, trace129$nep$peatnep_global, spar=spar )$y
# trace129$nep$peatnep_maskedby_yu.spl <- smooth.spline( trace129$nep$year, trace129$nep$peatnep_maskedby_yu, spar=spar )$y
# trace129$nep$peatnep_maskedby_yu_north.spl <- smooth.spline( trace129$nep$year, trace129$nep$peatnep_maskedby_yu_north, spar=spar )$y


## read charly's data in PgC / 10 yr
dstep <- 10
charly <- read.csv("/alphadata01/bstocker/data/yu_data/NCB/NCB_65sites_mean.csv")
df_charly_pt_mean <- data.frame( age=rev(charly$age) )
df_charly_pt_mean$year <- 1950 - df_charly_pt_mean$age + 5
df_charly_pt_mean$median <- rev(charly$NCB.50.) * dstep / 1000
df_charly_pt_mean$cumsum_median <- cumsum(rev(charly$NCB.50.)) * dstep / 1000

# # trace21_r87$peatNEP <- read.table("peatNEP_21kyr.dat")[,2]
# # trace21_r87$peatNEP.spl <- smooth.spline( trace21_r123$year, trace21_r123$peatNEP, spar=0.01 )$y

## ///////////////////////////////////////////////////////////////
## Common plot parameters
## ---------------------------------------------------------------

# ## ///////////////////////////////////////////////////////////////
# ## PEAT  AREA
# ## ---------------------------------------------------------------
# ylim <- c(0,6)
# xlim <- c(-20000,2000)

# pdf(paste("fig/peat_area_trace21_r129_",as.character(xlim[1]),".pdf",sep=""),width=10,height=6)
#   par( xaxs="i", yaxs="i", las=1, mar=c(4,4,2,3) )
#   plot( xlim, ylim, axes=FALSE, type="n", xlab="year AD", ylab="peatland area [M m2]" )
#   axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
#   axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=0.2),labels=F,tck=-0.01)
#   axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
#   axis(4,lwd=1.75);  axis(4,at=seq(ylim[1],ylim[2],by=0.2),labels=F,tck=-0.01)
#   box(lwd=1.75)

#   lines( trace129$area$year, trace129$area$peatarea_global/1e12, lwd=1.75, col="black" )
#   lines( trace129$area$year, trace129$area$peatarea_maskedby_yu/1e12, lwd=1.75, col="red" )
#   lines( trace129$area$year, trace129$area$peatarea_maskedby_yu_north/1e12, lwd=1.75, col="blue" )
#   lines( trace129$area$year, trace129$area$peatarea_maskedby_yu_north_scaled/1e12, lwd=1.75, col="magenta" )
#   lines( trace129$vars_site$year, trace129$vars_site$peatarea_avg_site/1e12, lwd=1.75, col="green" )
#   # lines( trace129$area$netpeat/1e15, lwd=1.75, col="grey50" )

#   legend( "topleft", 
#     c("simulated peatland area global", 
#       "simulated peatland area in today's peatland areas (including tropical)", 
#       "simulated peatland area in today's northern peatland areas (>30 deg N)", 
#       "scaled simulated peatland area in today's northern peatland areas (>30 deg N)",
#       "simulated peatland area in gridcells corresponding to site locations"
#       ),
#     col=c("black","red","blue","magenta","green"),
#     lwd=1.75, lty=1, bty="n"
#     )
# dev.off()


## ///////////////////////////////////////////////////////////////
## PEAT  C
## ---------------------------------------------------------------
ylim <- c(0,900)
xlim <- c(-20000,2000)

pdf(paste("fig/peatC_trace21_r129_",as.character(xlim[1]),".pdf",sep=""),width=10,height=6)
  par( xaxs="i", yaxs="i", las=1, mar=c(4,4,2,3) )
  plot( xlim, ylim, axes=FALSE, type="n", xlab="year AD", ylab="peat C [GtC]" )
  axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
  axis(2,at=seq(ylim[1],ylim[2],by=100),lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=20),labels=F,tck=-0.01)
  axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
  axis(4,at=seq(ylim[1],ylim[2],by=100),lwd=1.75);  axis(4,at=seq(ylim[1],ylim[2],by=20),labels=F,tck=-0.01)
  box(lwd=1.75)

  lines( trace129$c$year, trace129$c$netpeat/1e15, lwd=1.75, col="grey50" )

  lines( trace129$c$year, trace129$c$peatc_global/1e15, lwd=1.75, col="black" )
  # lines( trace129$c$year, trace129$c$peatc_global.spl/1e15, lwd=1 )

  # lines( trace129$nep$year, cumsum(trace129$nep$peatnep_global)/1e15, lwd=1, col="cyan" )

  lines( trace129$c$year, trace129$c$peatc_maskedby_yu/1e15, lwd=1.75, col="red" )
  # lines( trace129$c$year, trace129$c$peatc_maskedby_yu.spl/1e15, lwd=1 )

  lines( trace129$c$year, trace129$c$peatc_maskedby_yu_north/1e15, lwd=1.75, col="blue" )
  # lines( trace129$c$year, trace129$c$peatc_maskedby_yu_north.spl/1e15, lwd=1 )

  lines( trace129$c$year, trace129$vars_site$peatc_dens_avg_site * trace129$area$peatarea_maskedby_yu_north_scaled / 1e15, lwd=1.75, col="magenta" )
  # lines( trace129$c$year, trace129$vars_site$peatc_dens_avg_site.spl * trace129$area$peatarea_maskedby_yu_north_scaled / 1e15, lwd=1 )

  # lines( trace129$c$year, trace129$c$peatc_global.spl/1e15, lwd=1 )

  # lines( trace129$c$netpeat/1e15, lwd=1.75, col="grey50" )

  lines( df_charly_pt_mean$year, df_charly_pt_mean$cumsum_median, lwd=1.75, col="green" )
  # lines( df_charly_pt_mean$year, cumsum(df_charly_pt_mean$median), lwd=1.75, col="red", lty=2 )

  legend( "topleft", 
    c("simulated peat-related difference in global soil C", 
      "simulated peat C global",
     "simulated peat C in today's peatland areas (including tropical)",
     "simulated peat C in today's northern peatland areas (>30 deg N)",
     "simulated peat C at locations of sites, upscaled by simulated area",
     "data-based cumulative NCB Yu / Massa /Loisel data"),
    col=c("grey50","black","red","blue","magenta","green"),
    lwd=1.75, lty=1, bty="n"
    )
dev.off()

ylim <- c(0,800)
pdf(paste("fig/dc_peat_lgmpres.pdf",sep=""),width=10,height=6)
  par( xaxs="i", yaxs="i", las=1, mar=c(4,4,2,3) )
  plot( xlim, ylim, axes=FALSE, type="n", xlab="year AD", ylab="peat C [GtC]" )
  axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
  axis(2,at=seq(ylim[1],ylim[2],by=100),lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=20),labels=F,tck=-0.01)
  axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
  axis(4,at=seq(ylim[1],ylim[2],by=100),lwd=1.75);  axis(4,at=seq(ylim[1],ylim[2],by=20),labels=F,tck=-0.01)
  box(lwd=1.75)

  lines( trace129$c$year, trace129$c$peatc_global/1e15, lwd=1.75, col="black" )
  # lines( trace129$c$year, trace129$c$peatc_global.spl/1e15, lwd=1 )

  # lines( trace129$nep$year, cumsum(trace129$nep$peatnep_global)/1e15, lwd=1, col="cyan" )

  lines( trace129$c$year, trace129$c$peatc_maskedby_yu/1e15, lwd=1.75, col="red" )
  # lines( trace129$c$year, trace129$c$peatc_maskedby_yu.spl/1e15, lwd=1 )


  # lines( trace129$c$year, trace129$c$peatc_global.spl/1e15, lwd=1 )

  # lines( trace129$c$netpeat/1e15, lwd=1.75, col="grey50" )

  lines( df_charly_pt_mean$year, df_charly_pt_mean$cumsum_median, lwd=1.75, col="green" )
  # lines( df_charly_pt_mean$year, cumsum(df_charly_pt_mean$median), lwd=1.75, col="red", lty=2 )

  legend( "topleft", 
    c(
      "LPX peat C, global",
      "LPX peat C, in today's peatland areas",
     "NCB by YML"),
    col=c("black","red","green"),
    lwd=1.75, lty=1, bty="n"
    )
dev.off()


## ///////////////////////////////////////////////////////////////
## PEAT dC
## ---------------------------------------------------------------
ylim <- c(-40,100)
xlim <- c(-20000,2000)

pdf(paste("fig/peat_dC_trace21_r129_",as.character(xlim[1]),".pdf",sep=""),width=10,height=6)

  par( xaxs="i", yaxs="i", las=1 )
  plot( xlim, ylim, axes=FALSE, type="n", xlab="year AD", ylab=expression( paste( "peat ", Delta, "C (GtC/1000 yr)", sep="" ) ) )  #, xlim=c(0,2000)
  axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
  axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
  axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
  axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
  box(lwd=1.75)

  ## weird
  trace129$dc$peatdc_global[length(trace129$dc$peatdc_global)] <- 0.0
  trace129$dc$peatdc_maskedby_yu[length(trace129$dc$peatdc_global)] <- 0.0
  trace129$dc$peatdc_maskedby_yu_north[length(trace129$dc$peatdc_global)] <- 0.0

  boxl <- 100
  lines( trace129$dc$year, filter( trace129$dc$netpeat_dc/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1.0, col=rgb(0,0,0,0.1) )
  lines( trace129$dc$year, filter( trace129$dc$peatdc_global/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1.0, col=rgb(0,0,0,0.3) )
  lines( trace129$dc$year, filter( trace129$dc$peatdc_maskedby_yu/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1.0, col=rgb(1,0,0,0.3) )
  lines( trace129$dc$year, filter( trace129$dc$peatdc_maskedby_yu_north/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1.0, col=rgb(0,0,1,0.3) )
  lines( trace129$dc$year, filter( trace129$vars_site$peatdc_dens_avg_site * trace129$area$peatarea_maskedby_yu_north_scaled/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1.0, col=rgb(1,0,1,0.3) )

  spar <- 0.7
  lines( smooth.spline( trace129$dc$year, trace129$dc$netpeat_dc/1e12, spar=spar ), lwd=1.75, col="grey50" )
  lines( smooth.spline( trace129$dc$year, trace129$dc$peatdc_global/1e12, spar=spar ), lwd=1.75, col="black" )
  lines( smooth.spline( trace129$dc$year, trace129$dc$peatdc_maskedby_yu/1e12, spar=spar ), lwd=1.75, col="red" )
  lines( smooth.spline( trace129$dc$year, trace129$dc$peatdc_maskedby_yu_north/1e12, spar=spar ), lwd=1.75, col="blue" )
  lines( smooth.spline( trace129$dc$year, trace129$vars_site$peatdc_dens_avg_site * trace129$area$peatarea_maskedby_yu_north_scaled/1e12, spar=spar), lwd=1.75, col="magenta" )


  # lines( trace129$globnep$year, trace129$globnep$peatglobnep_global/1e12, lwd=1, col=rgb(0,0,0,0.3) )
  # # lines( smooth.spline( trace129$globnep$year, trace129$globnep$peatglobnep_global/1e12, spar=spar ), lwd=1.75, col="black" )
  # lines( trace129$globnep$year, trace129$globnep$peatglobnep_maskedby_yu/1e12, lwd=1, col=rgb(1,0,0,0.3) )
  # # lines( smooth.spline( trace129$globnep$year, trace129$globnep$peatglobnep_maskedby_yu/1e12, spar=spar ), lwd=1.75, col="red" )
  # lines( trace129$globnep$year, trace129$globnep$peatglobnep_maskedby_yu_north/1e12, lwd=1, col=rgb(0,0,1,0.3) )
  # # lines( smooth.spline( trace129$globnep$year, trace129$globnep$peatglobnep_maskedby_yu_north/1e12, spar=spar ), lwd=1.75, col="blue" )
  # lines( trace129$dc$year, trace129$vars_site$peatnep_dens_avg_site * trace129$area$peatarea_maskedby_yu_north_scaled/1e12, lwd=1.75, col=rgb(1,0,1,0.3 ) )


  lines( df_charly_pt_mean$year, df_charly_pt_mean$median * 1e2, lwd=1.75, col="green" )

  legend( "topleft", 
    c("ann. peat-related diff in soil C balance global", 
      "ann. peat C balance global", 
      "ann. peat C balance in today's peatland areas (including tropical)", 
      "ann. peat C balance in today's northern peatland areas (>30 deg N)", 
      "scaled ann. peat C balance in today's northern peatland areas (>30 deg N)", 
      "data-based NCB Yu / Massa /Loisel data"),
    col=c("grey50","black","red","blue","magenta","green"),
    lwd=1.75, lty=1, bty="n"
    )

dev.off()



# ## ///////////////////////////////////////////////////////////////
# ## PEAT globNEP
# ## ---------------------------------------------------------------
# ylim <- c(-40,100)
# xlim <- c(-20000,2000)

# pdf(paste("fig/peat_globNEP_trace21_r129_",as.character(xlim[1]),".pdf",sep=""),width=10,height=6)

#   par( xaxs="i", yaxs="i", las=1 )
#   plot( xlim, ylim, axes=FALSE, type="n", xlab="year AD", ylab=expression( paste( "peat NEP (GtC/1000 yr)", sep="" ) ) )
#   axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
#   axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
#   axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
#   axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
#   box(lwd=1.75)

#   boxl <- 1
#   lines( trace129$dc$year, filter( trace129$globnep$peatglobnep_global/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1, col=rgb(0,0,0,0.3) )
#   lines( trace129$dc$year, filter( trace129$globnep$peatglobnep_maskedby_yu/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1, col=rgb(1,0,0,0.3) )
#   lines( trace129$dc$year, filter( trace129$globnep$peatglobnep_maskedby_yu_north/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1, col=rgb(0,0,1,0.3) )
#   lines( trace129$dc$year, filter( trace129$vars_site$peatnep_dens_avg_site * trace129$area$peatarea_maskedby_yu_north_scaled/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1, col=rgb(1,0,1,0.3) )

#   boxl <- 30
#   lines( trace129$dc$year, filter( trace129$globnep$peatglobnep_global/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1, col="black" )
#   lines( trace129$dc$year, filter( trace129$globnep$peatglobnep_maskedby_yu/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1, col="red" )
#   lines( trace129$dc$year, filter( trace129$globnep$peatglobnep_maskedby_yu_north/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1, col="blue" )
#   lines( trace129$dc$year, filter( trace129$vars_site$peatnep_dens_avg_site * trace129$area$peatarea_maskedby_yu_north_scaled/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1, col="magenta" )

#   lines( df_charly_pt_mean$year, df_charly_pt_mean$median * 1e2, lwd=1.75, col="green" )

#   legend( "topleft", 
#     c("peatland NEP global", 
#       "peatland NEP in today's peatland areas (including tropical)", 
#       "peatland NEP in today's northern peatland areas (>30 deg N)", 
#       "scaled peatland NEP in today's northern peatland areas (>30 deg N)", 
#       "data-based NCB Yu / Massa /Loisel data"),
#     col=c("black","red","blue","magenta","green"),
#     lwd=1.75, lty=1, bty="n"
#     )

# dev.off()


## ///////////////////////////////////////////////////////////////
## PEAT globNEP - alternative plot for holoLU2 paper
## ---------------------------------------------------------------
ylim <- c(0,100)
xlim <- c(-12000,2000)

pdf(paste("fig/peat_globNEP_holoLU2_trace21_r129_",as.character(xlim[1]),".pdf",sep=""),width=8,height=6)

  par( xaxs="i", yaxs="i", las=1 )
  plot( xlim, ylim, axes=FALSE, type="n", xlab="year AD", ylab=expression( paste( "peat NEP (GtC/1000 yr)", sep="" ) ), xlim=c() )
  axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
  axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
  axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
  axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
  box(lwd=1.75)

  boxl <- 1
  lines( trace129$dc$year, filter( trace129$globnep$peatglobnep_global/1e12, rep(1/boxl,boxl), sides=2 ), lwd=1, col=rgb(1,0,0,0.3) )

  boxl <- 30
  lines( trace129$dc$year, filter( trace129$globnep$peatglobnep_global/1e12, rep(1/boxl,boxl), sides=2 ), lwd=2, col=rgb(1,0,0,1) )

  lines( df_charly_pt_mean$year, df_charly_pt_mean$median * 1e2, lwd=1.75, col=rgb(0,0,0,0.3) )

  legend( "topleft", 
    c("simulated LPX peatland global NEP", 
      # "peatland NEP in today's peatland areas (including tropical)", 
      # "peatland NEP in today's northern peatland areas (>30 deg N)", 
      # "scaled peatland NEP in today's northern peatland areas (>30 deg N)", 
      "data-based NCB Yu / Massa / Loisel data"),
    col=c("red",
      # "red",
      # "blue",
      # "magenta",
      rgb(0,0,0,0.3)),
      lwd=1.75, lty=1, bty="n"
      )

dev.off()


# ## ///////////////////////////////////////////////////////////////
# ## PEAT NEP
# ## ---------------------------------------------------------------
# ylim <- c(-2,8)
# xlim <- c(-20000,2000)

# pdf(paste("fig/peat_nep_trace21_r129_",as.character(xlim[1]),".pdf",sep=""),width=10,height=6)

#   par( xaxs="i", yaxs="i", las=1 )
#   plot( xlim, ylim, axes=FALSE, type="n", xlab="year AD", ylab=expression( paste( "peat NEP (gC/m2)", sep="" ) ) )
#   axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
#   axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=0.5),labels=F,tck=-0.01)
#   axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
#   axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=0.5),labels=F,tck=-0.01)
#   box(lwd=1.75)

#   lines( trace129$nep$year, trace129$nep$peatnep_global, lwd=1, col=rgb(0,0,0,0.3) )
#   lines( smooth.spline( trace129$nep$year, trace129$nep$peatnep_global, spar=spar ), lwd=1.75, col="black" )

#   lines( trace129$nep$year, trace129$nep$peatnep_maskedby_yu, lwd=1, col=rgb(1,0,0,0.3) )
#   lines( smooth.spline( trace129$nep$year, trace129$nep$peatnep_maskedby_yu, spar=spar ), lwd=1.75, col="red" )

#   lines( trace129$nep$year, trace129$nep$peatnep_maskedby_yu_north, lwd=1, col=rgb(0,0,1,0.3) )
#   lines( smooth.spline( trace129$nep$year, trace129$nep$peatnep_maskedby_yu_north, spar=spar ), lwd=1.75, col="blue" )

#   lines( trace129$dc$year, trace129$vars_site$peatnep_dens_avg_site, lwd=1.75, col=rgb(1,0,1,0.3 ) )
#   lines( smooth.spline(trace129$dc$year, trace129$vars_site$peatnep_dens_avg_site, spar=spar ), lwd=1.75, col="magenta" )


#   # lines( df_charly_pt_mean$year, df_charly_pt_mean$median * 1e2, lwd=1.75, col="green" )

#   legend( "topleft", 
#     c("peatland NEP global", 
#       "peatland NEP in today's peatland areas (including tropical)", 
#       "peatland NEP in today's northern peatland areas (>30 deg N)", 
#       "peatland NEP at location of 65 sites"), 
#     col=c("black","red","blue","magenta"),
#     lwd=1.75, lty=1, bty="n"
#     )

# dev.off()


# ## ///////////////////////////////////////////////////////////////
# ## PEAT NPP
# ## ---------------------------------------------------------------
# ylim <- c(0,280)
# xlim <- c(-20000,2000)

# pdf(paste("fig/peat_npp_trace21_r129_",as.character(xlim[1]),".pdf",sep=""),width=10,height=6)
#   par( xaxs="i", yaxs="i", las=1 )
#   plot( xlim, ylim, axes=FALSE, type="n", xlab="year AD", ylab="peatland NPP (GtC/yr)" )
#   axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
#   axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
#   axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
#   axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
#   box(lwd=1.75)

#   lines( trace129$npp$year, trace129$npp$peatnpp_global, lwd=1.75, col="black" )
#   lines( trace129$npp$year, trace129$npp$peatnpp_maskedby_yu, lwd=1.75, col="red" )
#   lines( trace129$npp$year, trace129$npp$peatnpp_maskedby_yu_north, lwd=1.75, col="blue" )
#   lines( trace129$npp$year, trace129$vars_site$peatnpp_dens_avg_site, lwd=1.75, col="magenta" )

#   # lines( trace129$rh$year, trace129$rh$peatrh_global, lwd=1.0, col=rgb(0,0,0,0.5) )
#   # lines( trace129$rh$year, trace129$rh$peatrh_maskedby_yu, lwd=1.0, col=rgb(1,0,0,0.5) )
#   # lines( trace129$rh$year, trace129$rh$peatrh_maskedby_yu_north, lwd=1.0, col=rgb(0,0,1,0.5) )
#   # lines( trace129$rh$year, trace129$vars_site$peatrh_dens_avg_site * trace129$area$peatarea_maskedby_yu_north_scaled, lwd=1.0, col=rgb(1,0,1,0.5) )

#   legend( "topleft", 
#     c("simulated peatland NPP global", 
#       "simulated peatland NPP in today's peatland areas (including tropical)", 
#       "simulated peatland NPP in today's northern peatland areas (>30 deg N)", 
#       "simulated peatland NPP at location of 65 sites"),
#     col=c("black","red","blue","magenta"),
#     lwd=1.75, lty=1, bty="n"
#     )
# dev.off()


# ## ///////////////////////////////////////////////////////////////
# ## PEAT RH
# ## ---------------------------------------------------------------
# ylim <- c(0,280)
# xlim <- c(-20000,2000)

# pdf(paste("fig/peat_rh_trace21_r129_",as.character(xlim[1]),".pdf",sep=""),width=10,height=6)
#   par( xaxs="i", yaxs="i", las=1 )
#   plot( xlim, ylim, axes=FALSE, type="n", xlab="year AD", ylab="peatland Rh (GtC/yr)" )
#   axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
#   axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
#   axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
#   axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
#   box(lwd=1.75)

#   lines( trace129$rh$year, trace129$rh$peatrh_global, lwd=1.75, col="black" )
#   lines( trace129$rh$year, trace129$rh$peatrh_maskedby_yu, lwd=1.75, col="red" )
#   lines( trace129$rh$year, trace129$rh$peatrh_maskedby_yu_north, lwd=1.75, col="blue" )
#   lines( trace129$rh$year, trace129$vars_site$peatrh_dens_avg_site, lwd=1.75, col="magenta" )

#   legend( "topleft", 
#     c("simulated peatland Rh global",
#       "simulated peatland Rh in today's peatland areas (including tropical)",
#       "simulated peatland Rh in today's northern peatland areas (>30 deg N)",
#       "simulated peatland Rh at location of 65 sites"),
#     col=c("black","red","blue","magenta"),
#     lwd=1.75, lty=1, bty="n"
#     )
# dev.off()


# ## ///////////////////////////////////////////////////////////////
# ## PEAT NPP/RH
# ## ---------------------------------------------------------------
# ylim <- c(0.8,1.3)
# xlim <- c(-20000,2000)

# pdf(paste("fig/peat_npp_ov_rh_trace21_r129_",as.character(xlim[1]),".pdf",sep=""),width=10,height=6)
#   par( xaxs="i", yaxs="i", las=1 )
#   plot( xlim, ylim, axes=FALSE, type="n", xlab="year AD", ylab="peatland Rh (GtC/yr)" )
#   axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
#   axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
#   axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=1000),labels=F,tck=-0.01)
#   axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
#   box(lwd=1.75)

#   lines( trace129$rh$year, trace129$npp$peatnpp_global/trace129$rh$peatrh_global, lwd=1.75, col="black" )
#   lines( trace129$rh$year, trace129$npp$peatnpp_maskedby_yu/trace129$rh$peatrh_maskedby_yu, lwd=1.75, col="red" )
#   lines( trace129$rh$year, trace129$npp$peatnpp_maskedby_yu_north/trace129$rh$peatrh_maskedby_yu_north, lwd=1.75, col="blue" )
#   lines( trace129$rh$year, trace129$vars_site$peatnpp_dens_avg_site/trace129$vars_site$peatrh_dens_avg_site, lwd=1.75, col="magenta" )

#   legend( "topleft", 
#     c("simulated peatland NPP/Rh global",
#       "simulated peatland NPP/Rh in today's peatland areas (including tropical)",
#       "simulated peatland NPP/Rh in today's northern peatland areas (>30 deg N)",
#       "simulated peatland NPP/Rh at location of 65 sites"),
#     col=c("black","red","blue","magenta"),
#     lwd=1.75, lty=1, bty="n"
#     )
# dev.off()
