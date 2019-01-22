source('get_statistics.R')
source("/alphadata01/bstocker/sofun/utils_sofun/analysis_sofun/add_alpha.R")

## Evaluate land C budget for two periods only: pre X vs. post X
yr_prepost <- 1850

##-------------------------------------------------------------------------------
## Get time series delta C tot for Holocene, last millennium, and industrial era
##-------------------------------------------------------------------------------
load('dc_terr_elsig_holocene.Rdata')
load('dc_terr_bauska_lastmill.Rdata')
load('dc_terr_indus.Rdata')

yrend_elsig <- tail(df_cum_land_uptake$year,1)
if (yrend_elsig<yr_prepost){
  ## Pivot year is within the data coverage by Bauska
  ## ==> cut Elsig data after start year of Bauska data
  yrstart_bauska       <- df_cum_land_uptake_bauska$year[1]
  iyr_end_elsig        <- which( df_cum_land_uptake$year==yrstart_bauska )
  df_cum_land_uptake_1 <- df_cum_land_uptake[iyr_end_elsig,1:2002]

  ## ==> cut Bauska data after 'yr_prepost'
  iyr_end_bauska       <- which( df_cum_land_uptake_bauska$year==yr_prepost )
  df_cum_land_uptake_2 <- df_cum_land_uptake_bauska[iyr_end_bauska,1:1001]
  df_cum_land_uptake_2[1,2:1001] <- df_cum_land_uptake_2[1,2:1001] * 1e-14

  # Bauska data ends in 1920 (end of budget anyway)
  df_cum_land_uptake_3 <- df_cum_land_uptake_bauska[dim(df_cum_land_uptake_bauska)[1],1:1001] 
  df_cum_land_uptake_3[1,2:1001] <- df_cum_land_uptake_3[1,2:1001] * 1e-14 - df_cum_land_uptake_2[1,2:1001]

}

##-------------------------------------------------------------------------------
## Combine Holocene and Lastmill data into single data frame containing cumulative change in the two periods
##-------------------------------------------------------------------------------
## Number of random draws from independent time series 
nruns <- 5000
dc_terr_per <- array( NA, dim=c(2,nruns) )

## Sample data
colnr_elsig  <- sample( 2:2002, nruns, replace=TRUE )
colnr_bauska <- sample( 2:1001, nruns, replace=TRUE )

for (irun in 1:nruns){
  dc_terr_per[1,irun] <- df_cum_land_uptake_1[1,colnr_elsig[irun]] + df_cum_land_uptake_2[1,colnr_bauska[irun]]
  dc_terr_per[2,irun] <- df_cum_land_uptake_3[1,colnr_bauska[irun]]
}

dc_terr_per_sum <- as.data.frame(get_statistics( dc_terr_per, 1:nruns ))

##-------------------------------------------------------------------------------
## Get time series for delta peat C
##-------------------------------------------------------------------------------
## LPX
trace129 <- list()
filn <- "peatc_trace21_129.dat"
col.names <- c( "time", "peatc_global", "peatc_maskedby_yu", "peatc_maskedby_yu_north", "netpeat" )
trace129$c <- read.table( filn, col.names=col.names )

iyrpivot <- which.min( abs( yr_prepost - trace129$c$time ) )
iyrstart <- which.min( abs( -9050 - trace129$c$time ) ) 
iyrend   <- which.min( abs( 1920 - trace129$c$time ) )
dc_pt_lpx_per <- array( NA, dim=c(2,1) )

dc_pt_lpx_per[1,1] <- (trace129$c$peatc_global[ iyrpivot ] - trace129$c$peatc_global[ iyrstart ]) * 1e-15
dc_pt_lpx_per[2,1] <- (trace129$c$peatc_global[ iyrend ]   - trace129$c$peatc_global[ iyrpivot ]) * 1e-15

## YML
load( "pt_yu_lastmill.Rdata" )

iyrpivot <- which.min( abs( yr_prepost - df_cum_pt_mean_lhnfix$year ) )
iyrstart <- which.min( abs( -9050 - df_cum_pt_mean_lhnfix$year ) ) 
iyrend   <- which.min( abs( 1920 - df_cum_pt_mean_lhnfix$year ) )
dc_pt_yml_per <- array( NA, dim=c(2,1000) )

for (idx in 1:1000){
  dc_pt_yml_per[1,idx] <- (df_cum_pt_mean_lhnfix[ iyrpivot, idx+2 ] - df_cum_pt_mean_lhnfix[ iyrstart, idx+2 ]) #* 1e-15
  dc_pt_yml_per[2,idx] <- (df_cum_pt_mean_lhnfix[ iyrend, idx+2 ]   - df_cum_pt_mean_lhnfix[ iyrpivot, idx+2 ]) #* 1e-15
}

dc_pt_yml_per_sum <- as.data.frame(get_statistics( dc_pt_yml_per, 1:1000 ))


##-------------------------------------------------------------------------------
## Bootstrap remainder
##-------------------------------------------------------------------------------
## Number of random draws from independent time series 
nruns <- 5000

dc_remaninder <- array( NA, dim=c(2,nruns) )

## Sample data
colnr_dc <- sample( 1:5000, nruns, replace=TRUE )
colnr_pt <- sample( 2:1000, nruns, replace=TRUE )

for (irun in 1:nruns){
  dc_remaninder[,irun] <- dc_terr_per[,colnr_dc[irun]] - dc_pt_yml_per[,colnr_pt[irun]]
}

## get statistics
dc_remaninder_sum <- as.data.frame( get_statistics( dc_remaninder, 1:nruns ) )


##-------------------------------------------------------------------------------
## Get time series of LUC emissions
##-------------------------------------------------------------------------------
load("fluc_holoLU2.Rdata")

iyrpivot <- which.min( abs( yr_prepost - fluc$hyde31$year ) )
iyrstart <- which.min( abs( -9050 - fluc$hyde31$year ) ) 
iyrend   <- which.min( abs( 1920 - fluc$hyde31$year ) )
dc_luc_per <- array( NA, dim=c(2,6) )

dc_luc_per[1,1] <- fluc$hyde31$cumfluc[iyrpivot] - fluc$hyde31$cumfluc[iyrstart] 
dc_luc_per[2,1] <- fluc$hyde31$cumfluc[iyrend]   - fluc$hyde31$cumfluc[iyrpivot] 

dc_luc_per[1,2] <- fluc$kk10$cumfluc[iyrpivot] - fluc$kk10$cumfluc[iyrstart] 
dc_luc_per[2,2] <- fluc$kk10$cumfluc[iyrend]   - fluc$kk10$cumfluc[iyrpivot] 

dc_luc_per[1,3] <- fluc$hyde31u$cumfluc[iyrpivot] - fluc$hyde31u$cumfluc[iyrstart] 
dc_luc_per[2,3] <- fluc$hyde31u$cumfluc[iyrend]   - fluc$hyde31u$cumfluc[iyrpivot] 

dc_luc_per[1,4] <- fluc$kk10d$cumfluc[iyrpivot] - fluc$kk10d$cumfluc[iyrstart] 
dc_luc_per[2,4] <- fluc$kk10d$cumfluc[iyrend]   - fluc$kk10d$cumfluc[iyrpivot] 

dc_luc_per[1,5] <- fluc$hyde32$cumfluc[iyrpivot] - fluc$hyde32$cumfluc[iyrstart] 
dc_luc_per[2,5] <- fluc$hyde32$cumfluc[iyrend]   - fluc$hyde32$cumfluc[iyrpivot] 

dc_luc_per[1,6] <- fluc$hyde32u$cumfluc[iyrpivot] - fluc$hyde32u$cumfluc[iyrstart] 
dc_luc_per[2,6] <- fluc$hyde32u$cumfluc[iyrend]   - fluc$hyde32u$cumfluc[iyrpivot] 

colnames(dc_luc_per) <- c("hyde31","kk10","hyde31u","kk10d","hyde32","hyde32u")
rownames(dc_luc_per) <- c("pre-1850","1850-1920")
dc_luc_per <- as.data.frame(dc_luc_per)

# ##-------------------------------------------------------------------------------
# ## Emergent constraint plot
# ##-------------------------------------------------------------------------------
# pdf("ec_prepost.pdf", width=8, height=6)

#   ylim <- c(100,250)
#   par( las=1, xaxs="i", yaxs="i" )
#   plot(   dc_luc_per$hyde31[2],  dc_luc_per$hyde31[1], ylim=ylim, xlim=c(0,80), xlab="eLUC 1850-1920", ylab="eLUC pre-1850", pch=21, cex=2, bg="grey50" )
#   points( dc_luc_per$hyde31u[2], dc_luc_per$hyde31u[1], pch=21, cex=2, bg=add_alpha("grey50", 0.5) )
#   points( dc_luc_per$hyde32[2],  dc_luc_per$hyde32[1], pch=21, cex=2, bg="burlywood3" )
#   points( dc_luc_per$hyde32u[2], dc_luc_per$hyde32u[1], pch=21, cex=2, bg=add_alpha("burlywood3",0.5) )
#   points( dc_luc_per$kk10[2],    dc_luc_per$kk10[1], pch=21, cex=2, bg="khaki3" )

#   rect( -(dc_remainder_outarr$mean[7]-dc_remainder_outarr$sd[7]), ylim[1], -(dc_remainder_outarr$mean[7]+dc_remainder_outarr$sd[7]), ylim[2], col=rgb(0,0,0,0.3), border=NA )

# dev.off()

##-------------------------------------------------------------------------------
## Barplot
##-------------------------------------------------------------------------------
library(Hmisc)
library(plotrix)

## Plot bars for the periods
pdf("barplot_prepost_holoLU2.pdf", width=6, height=6 )
  par( las=1, xaxs="i" )
  ylim <- c(-60,20)
  xlim <- c(1,7)
  # rect( mybar1[1,], rep(ylim[1],4), mybar1[4,], rep(ylim[2],4), col=rgb(0,0,0,0.2), border=NA )

  mybar1 <- barplot( 
                    t( cbind( dc_terr_per_sum$mean[2], dc_pt_yml_per_sum$mean[2], dc_remaninder_sum$mean[2] ) ),
                    beside = TRUE,
                    ylim=ylim, xlim=xlim,
                    col=c("springgreen3","dodgerblue3","darkgoldenrod2"),
                    space=c(0.2,1.5),
                    border=TRUE, axes=FALSE,
                    xlab="", ylab="C (GtC)"       
                    # names.arg=c(paste("pre-",yr_prepost,sep=""),paste("post-",yr_prepost,sep=""))
                    )

  axis(2,lwd=1.5,at=seq(ylim[1],ylim[2],by=20));  axis(2,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
  axis(4,lwd=1.5,labels=F,at=seq(ylim[1],ylim[2],by=20));  axis(4,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
  abline(0,0)

  errbar(
         mybar1,
         t( cbind( dc_terr_per_sum$mean[2], dc_pt_yml_per_sum$mean[2], dc_remaninder_sum$mean[2] ) ),
         t( cbind( dc_terr_per_sum$mean[2]+dc_terr_per_sum$sd[2], dc_pt_yml_per_sum$mean[2]+dc_pt_yml_per_sum$sd[2], dc_remaninder_sum$mean[2]+dc_remaninder_sum$sd[2] ) ),
         t( cbind( dc_terr_per_sum$mean[2]-dc_terr_per_sum$sd[2], dc_pt_yml_per_sum$mean[2]-dc_pt_yml_per_sum$sd[2], dc_remaninder_sum$mean[2]-dc_remaninder_sum$sd[2] ) ),
         # t( cbind( dc_land_outarr$mean, dc_pt_yml_per_sum$mean, dc_remaninder_sum$mean ) ),
         # t( cbind( dc_land_outarr$mean, dc_pt_yml_per_sum$mean, dc_remaninder_sum$mean ) ),
         add=TRUE, pch=NA
         )

  ## add grey rectangles 
  left  <- 5.5
  width <- 0.3
  # rect( left[1,1], ylim[1], left[1,2], ylim[2], col=rgb(0,0,0,0.1), border=NA )

  ## add points for fluc
  offl  <- 0.2
  width <- 0.3
  rect( left,  0, left+width, -fluc_per_lastmill$hyde31u[7], col=add_alpha("grey50", 0.5),    border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( left,  0, left+width, -fluc_per_lastmill$hyde31[7],  col="grey50",                    border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( left+width,  0, left+2*width, -fluc_per_lastmill$hyde32u[7], col=add_alpha("burlywood3",0.5), border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( left+width,  0, left+2*width, -fluc_per_lastmill$hyde32[7],  col="burlywood3",                border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( left+2*width,  0, left+width+3*width, -fluc_per_lastmill$kk10[7],    col=add_alpha("khaki3",1.0),     border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )

  # ## add points for simulated peat C balance
  # par(new=TRUE)
  # plotCI( left[2,]+0.7, y=pt_outarr$lpx, pt_outarr$err_lpx, pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  # par(new=TRUE)
  # plotCI( left[3,]+0.7, y=dc_remainder_outarr$mean_lpx, uiw=dc_remainder_outarr$sd_lpx, col="darkgoldenrod4", pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  
  # # par(new=TRUE)
  # # plot( left[2,]+0.7, pt_lpx_outarr$peatc_global, col="dodgerblue4", pch=1, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=2 )
  # # par(new=TRUE)
  # # plot( left[2,]+0.7, pt_lpx_outarr$peatc_maskedby_yu, col="dodgerblue4", pch=2, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=2 )
  # # par(new=TRUE)
  # # plot( left[2,]+0.7, pt_lpx_outarr$peatc_maskedby_yu_north, col="dodgerblue4", pch=6, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=2 )
  
  # # # par(new=TRUE)
  # # # plot( left[2,]+0.7, pt_lpx_outarr$peatc_at_sites, col="dodgerblue4", pch=5, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )

  # # ## add points for simulated total non-peat terrestrial C balance ~= remainder?
  # # # par(new=TRUE)
  # # # plot( left[2,]+1.92, dc_lpx_holocene_outarr$dc_lpx, col="darkgoldenrod4", pch=1, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )

  # legend(
  #        "bottomleft",
  #        c("total terrestrial","peat","other processes"),
  #        fill=c("springgreen3","dodgerblue3","darkgoldenrod2"),
  #        bty="n",
  #        box.lwd=0
  #        )
  # legend(
  #        "bottom",
  #        c("HYDE standard","KK10","KK10D","HYDE upper","HYDE concave","HYDE Ruddiman"),
  #        pch=8,
  #        bty="n",
  #        col=c("black","red","cyan","green","blue","magenta"),
  #        )
  # # legend(
  # #        "bottomright",
  # #        c("net peat C effect, LPX"), #, "terr. non-peat C, LPX", "peat C at sites, LPX"),
  # #        pch=c(1), #,1,5), 
  # #        bty="n",
  # #        col=c("dodgerblue4") #,"darkgoldenrod4","dodgerblue4"),
  #        # )

dev.off()


