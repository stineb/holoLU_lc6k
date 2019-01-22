load("dc_remainder_holocene.Rdata")
load("pt_outarr.Rdata")
load("fluc_per.Rdata")
load("dc_elsig.Rdata")
source("/alphadata01/bstocker/sofun/utils_sofun/analysis_sofun/add_alpha.R")

library(Hmisc)
library(plotrix)

## Plot bars for the periods
pdf("bal_comparison_barplot_holocene_holoLU2.pdf", width=8, height=6 )
  par( las=1, xaxs="i" )
  ylim <- c(-200,200)
  xlim <- c(1.2,26)
  # rect( mybar1[1,], rep(ylim[1],4), mybar1[4,], rep(ylim[2],4), col=rgb(0,0,0,0.2), border=NA )

  mybar1 <- barplot( 
                    # t( cbind( dc_land_outarr$mean, pt_outarr$mean_yu, dc_remainder_outarr$mean_yu ) ),
                    t( cbind( dc_land_outarr$median, pt_outarr$median_yu, dc_remainder_outarr$median_yu ) ),
                    beside = TRUE,
                    ylim=ylim, xlim=xlim,
                    col=c("springgreen3","dodgerblue3","darkgoldenrod2"),
                    space=c(0.2,1.5),
                    border=TRUE, axes=FALSE,
                    xlab="period (ka BP)", ylab="C balance per period (GtC)",           
                    names.arg=rownames(dc_land_outarr)
                    )

  axis(2,lwd=1.5,at=seq(ylim[1],ylim[2],by=50));  axis(2,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
  axis(4,lwd=1.5,labels=F,at=seq(ylim[1],ylim[2],by=50));  axis(4,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
  abline(0,0)

  errbar(
         mybar1,
         # t( cbind( dc_land_outarr$mean, pt_outarr$mean_yu, dc_remainder_outarr$mean_yu ) ),
         # t( cbind( dc_land_outarr$mean+dc_land_outarr$sd, pt_outarr$mean_yu+pt_outarr$sd_yu, dc_remainder_outarr$mean_yu+dc_remainder_outarr$sd_yu ) ),
         # t( cbind( dc_land_outarr$mean-dc_land_outarr$sd, pt_outarr$mean_yu-pt_outarr$sd_yu, dc_remainder_outarr$mean_yu-dc_remainder_outarr$sd_yu ) ),
         t( cbind( dc_land_outarr$median, pt_outarr$median_yu, dc_remainder_outarr$median_yu ) ),
         t( cbind( dc_land_outarr$q10,    pt_outarr$q10_yu,    dc_remainder_outarr$q10_yu ) ),
         t( cbind( dc_land_outarr$q90,    pt_outarr$q90_yu,    dc_remainder_outarr$q90_yu ) ),
         add=TRUE, pch=NA
         )

  # errbar(
  #        mybar1,
  #        t(outarr_lpx),
  #        t(outarrerr_lpx[,c(1,3,5)]),
  #        t(outarrerr_lpx[,c(2,4,6)]),
  #        add=TRUE
  #        )

  ## add grey rectangles 
  left  <- mybar1 - 0.7
  right <- mybar1 + 0.6
  rect( left[1,1], ylim[1], left[1,2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[1,3], ylim[1], left[1,4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[1,5], ylim[1], 26, ylim[2], col=rgb(0,0,0,0.1), border=NA )

  ## add bars for fluc
  offl  <- 0.2
  width <- 0.3
  rect( right[3,]+offl,         rep(0,5), right[3,]+offl+width,  -fluc_per_holocene$hyde31u, col=add_alpha("grey50", 0.5),    border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl,         rep(0,5), right[3,]+offl+width,  -fluc_per_holocene$hyde31,  col="grey50",                    border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+width,   rep(0,5), right[3,]+offl+2*width,-fluc_per_holocene$hyde32u, col=add_alpha("burlywood3",0.5), border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+width,   rep(0,5), right[3,]+offl+2*width,-fluc_per_holocene$hyde32,  col="burlywood3",                border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+2*width, rep(0,5), right[3,]+offl+3*width,-fluc_per_holocene$kk10,    col=add_alpha("khaki3",0.5),     border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+2*width, rep(0,5), right[3,]+offl+3*width,-fluc_per_holocene$kk10d,   col="khaki3",                    border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  # par(new=TRUE)
  # plot( right[3,]+0.5, -fluc_per_holocene$r4, col="blue", pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  # par(new=TRUE)
  # plot( right[3,]+0.5, -fluc_per_holocene$r3, col="green", pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  # par(new=TRUE)
  # plot( right[3,]+0.5, -fluc_per_holocene$r7, col="cyan", pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  # par(new=TRUE)
  # plot( right[3,]+0.5, -fluc_per_holocene$r2, col="red", pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  # par(new=TRUE)
  # plot( right[3,]+0.5, -fluc_per_holocene$r1, col="black", pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )

  ## add points for simulated peat C balance
  par(new=TRUE)
  plotCI( left[2,]+0.7, y=pt_outarr$lpx_netpeat, pt_outarr$err_lpx, pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  # plotCI( left[2,]+0.7, y=pt_outarr$lpx_peatc_global, pt_outarr$err_lpx, pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  par(new=TRUE)
  plotCI( left[3,]+0.7, y=dc_remainder_outarr$mean_lpx, uiw=dc_remainder_outarr$sd_lpx, col="darkgoldenrod4", pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  
  # par(new=TRUE)
  # plot( left[2,]+0.7, pt_lpx_outarr$peatc_global, col="dodgerblue4", pch=1, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=2 )
  # par(new=TRUE)
  # plot( left[2,]+0.7, pt_lpx_outarr$peatc_maskedby_yu, col="dodgerblue4", pch=2, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=2 )
  # par(new=TRUE)
  # plot( left[2,]+0.7, pt_lpx_outarr$peatc_maskedby_yu_north, col="dodgerblue4", pch=6, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=2 )
  
  # # par(new=TRUE)
  # # plot( left[2,]+0.7, pt_lpx_outarr$peatc_at_sites, col="dodgerblue4", pch=5, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )

  # ## add points for simulated total non-peat terrestrial C balance ~= remainder?
  # # par(new=TRUE)
  # # plot( left[2,]+1.92, dc_lpx_holocene_outarr$dc_lpx, col="darkgoldenrod4", pch=1, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )

  legend(
         "bottomleft",
         c("total terrestrial","peat","other processes"),
         fill=c("springgreen3","dodgerblue3","darkgoldenrod2"),
         bty="n",
         box.lwd=0
         )
  legend(
         "bottom",
         c( "HYDE 3.1", "HYDE 3.2", "KK10D (KK10)" ),
         bty="n",
         fill=c("grey50","burlywood3","khaki3"),
         box.lwd=0
         )
  # legend(
  #        "bottomright",
  #        c("net peat C effect, LPX"), #, "terr. non-peat C, LPX", "peat C at sites, LPX"),
  #        pch=c(1), #,1,5), 
  #        bty="n",
  #        col=c("dodgerblue4") #,"darkgoldenrod4","dodgerblue4"),
         # )

dev.off()


