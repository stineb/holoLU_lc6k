load( "dc_terr_bauska_lastmill.Rdata" )
load( "dc_remainder_lastmill.Rdata" )
load( "pt_outarr_lastmill.Rdata" ) 
load( "fluc_per.Rdata" )

library(Hmisc)
library(plotrix)

add_alpha <- function( col, alpha ){
  ## add alpha to color given as a name
  col    <- col2rgb( col, alpha=TRUE )/255
  col[4] <- alpha
  col    <- rgb(col[1,],col[2,],col[3,],col[4,])
  return( col )
}

# ## Plot bars for the periods
# ## Combine output arrays
# outarr <- dc_lastmill_outarr
# outarr$dc_pt <- pt_outarr$dc_pt
# outarr$remainder <- dc_remainder_outarr$dc_remainder

# outarrerr <- dc_lastmill_outarrerr
# outarrerr$err_dc_pt <- pt_outarrerr$err_dc_pt
# outarrerr$err_remainder <- dc_remainder_outarrerr$err_dc_remainder


pdf("cbal_comparison_barplot_lastmill_holoLU2.pdf", width=10, height=6 )

  par( las=1 )
  ylim <- c(-100,40)
  xlim <- c(2.2,35)
  # rect( mybar1[1,], rep(ylim[1],4), mybar1[4,], rep(ylim[2],4), col=rgb(0,0,0,0.2), border=NA )

  mybar1 <- barplot( 
                    t( cbind( dc_lastmill_outarr$median, pt_outarr$median_lhnfix, dc_remainder_outarr$median ) ),
                    # t( cbind( dc_lastmill_outarr$mean, pt_outarr$mean_lhnfix, dc_remainder_outarr$mean ) ),
                    beside = TRUE,
                    ylim=ylim, xlim=xlim,
                    col=c("springgreen3","dodgerblue3","darkgoldenrod2"),
                    space=c(0.2,1.5),
                    border=TRUE,
                    axes=FALSE,
                    xlab="period (yr CE)", ylab="C balance change per period (GtC)",
                    names.arg=rownames(dc_lastmill_outarr)
                    )

  axis(2,lwd=1.5,at=seq(ylim[1],ylim[2],by=20));  axis(2,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
  axis(4,lwd=1.5,labels=F,at=seq(ylim[1],ylim[2],by=20));  axis(4,at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
  # box(lwd=1.75)

  abline(0,0)

  errbar(
         mybar1,
         # t( cbind( dc_lastmill_outarr$mean,                     pt_outarr$mean_lhnfix,                     dc_remainder_outarr$mean ) ),
         # t( cbind( dc_lastmill_outarr$mean+dc_lastmill_outarr$sd, pt_outarr$mean_lhnfix+pt_outarr$sd_lhnfix, dc_remainder_outarr$mean+dc_remainder_outarr$sd ) ),
         # t( cbind( dc_lastmill_outarr$mean-dc_lastmill_outarr$sd, pt_outarr$mean_lhnfix-pt_outarr$sd_lhnfix, dc_remainder_outarr$mean-dc_remainder_outarr$sd ) ),
         t( cbind( dc_lastmill_outarr$median,                     pt_outarr$median_lhnfix,                     dc_remainder_outarr$median ) ),
         t( cbind( dc_lastmill_outarr$q10,                        pt_outarr$q10_lhnfix,                        dc_remainder_outarr$q10 ) ),
         t( cbind( dc_lastmill_outarr$q90,                        pt_outarr$q90_lhnfix,                        dc_remainder_outarr$q90 ) ),
         add=TRUE, pch=NA
         )

  ## add grey rectangles 
  left <- mybar1 - 0.7
  right <- mybar1 + 0.6
  rect( left[1,1], ylim[1], left[1,2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[1,3], ylim[1], left[1,4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[1,5], ylim[1], left[1,6], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[1,7], ylim[1], right[3,7]+1.1, ylim[2], col=rgb(0,0,0,0.1), border=NA )

  ## add points for fluc
  # hyde <- cbind( fluc_per_lastmill$r1, fluc_per_lastmill$r2, fluc_per_lastmill$r3, fluc_per_lastmill$r4, fluc_per_lastmill$r5, fluc_per_lastmill$r7 )
  # mean_hyde <- apply( hyde, c(1), FUN = mean, na.rm=TRUE )
  # min_hyde<- apply( hyde, c(1), FUN = min, na.rm=TRUE )
  # max_hyde<- apply( hyde, c(1), FUN = max, na.rm=TRUE )
  # par(new=TRUE)
  # plotCI( right[3,]+0.5, y=mean_hyde, ui=max_hyde, li=min_hyde, col="grey50", pch=NA, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )

  ## add bars for fluc
  offl  <- 0.2
  width <- 0.3
  rect( right[3,]+offl,         rep(0,5), right[3,]+offl+width,  -fluc_per_lastmill$hyde31u, col=add_alpha("grey50", 0.5),    border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl,         rep(0,5), right[3,]+offl+width,  -fluc_per_lastmill$hyde31,  col="grey50",                    border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+width,   rep(0,5), right[3,]+offl+2*width,-fluc_per_lastmill$hyde32u, col=add_alpha("burlywood3",0.5), border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+width,   rep(0,5), right[3,]+offl+2*width,-fluc_per_lastmill$hyde32,  col="burlywood3",                border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+2*width, rep(0,5), right[3,]+offl+3*width,-fluc_per_lastmill$kk10,    col=add_alpha("khaki3",1.0),     border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  # rect( right[3,]+offl+2*width, rep(0,5), right[3,]+offl+3*width,-fluc_per_lastmill$kk10d,   col="khaki3",                    border=NA ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )

  # par(new=TRUE)
  # plot( right[3,]+0.5, -fluc_per_lastmill$r5, col="magenta", pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  # par(new=TRUE)
  # plot( right[3,]+0.5, -fluc_per_lastmill$r4, col="blue", pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  # par(new=TRUE)
  # plot( right[3,]+0.5, -fluc_per_lastmill$r3, col="green", pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  # par(new=TRUE)
  # plot( right[3,]+0.5, -fluc_per_lastmill$r7, col="cyan", pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  # par(new=TRUE)
  # plot( right[3,]+0.5, -fluc_per_lastmill$r2, col="red", pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  # par(new=TRUE)
  # plot( right[3,]+0.5, -fluc_per_lastmill$r1, col="black", pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )

  ## add points for simulated peat C balance
  par(new=TRUE)
  plotCI( left[2,]+0.7, y=pt_outarr$lpx, uiw=0.0, pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  par(new=TRUE)
  # plotCI( left[3,]+0.7, y=dc_remainder_outarr$mean_lpx, uiw=dc_remainder_outarr$sd_lpx, col="darkgoldenrod4", pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  plotCI( left[3,]+0.7, y=dc_remainder_outarr$median_lpx, ui=dc_remainder_outarr$q90_lpx, , li=dc_remainder_outarr$q10_lpx, col="darkgoldenrod4", pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )

  # ## add points for simulated peat C balance
  # par(new=TRUE)
  # plot( left[2,]+0.7,  pt_lpx_lastmill_outarr$netpeat, col="dodgerblue4", pch=1, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  # par(new=TRUE)
  # plot( left[2,]+0.7,  pt_lpx_lastmill_outarr$peatc_global, col="dodgerblue4", pch=1, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  # par(new=TRUE)
  # plot( left[2,]+0.7, pt_lpx_lastmill_outarr$peatc_maskedby_yu, col="dodgerblue4", pch=2, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  # par(new=TRUE)
  # plot( left[2,]+0.7, pt_lpx_lastmill_outarr$peatc_maskedby_yu_north, col="dodgerblue4", pch=6, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  
  # par(new=TRUE)
  # plot( left[2,]+0.7, pt_lpx_lastmill_outarr$peatc_at_sites, col="dodgerblue4", pch=5, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )

  # ## add points for simulated total non-peat terrestrial C balance ~= remainder?
  # par(new=TRUE)
  # plot( left[2,]+1.92, dc_lpx_lastmill_outarr$dc_lpx, col="orange4", pch=1, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )

  legend(
         "bottomleft",
         c("total terrestrial","peat","other processes"),
         fill=c("springgreen3","dodgerblue3","darkgoldenrod2"),
         bty="n",
         box.lwd=0
         )
  legend(
         "bottom",
         c( "HYDE 3.1", "HYDE 3.2", "KK10" ),
         bty="n",
         fill=c("grey50","burlywood3","khaki3"),
         box.lwd=0
         )

  # legend(
  #        "bottomright",
  #        c("net peat C effect, LPX"), #, "terr. non-peat C, LPX", "peat C at sites, LPX"),
  #        pch=c(1), #,1,5), 
  #        bty="n",
  #        col=c("dodgerblue4") #,"orange4","dodgerblue4"),
  #        )

dev.off()