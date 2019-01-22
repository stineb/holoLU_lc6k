## Reduced plot (HYDE 3.1, HYDE 3.2, KK10, and KK10D)

load( "fluc_holoLU2.Rdata" )
load( "landuse_tseries.Rdata" )

add_alpha <- function( col, alpha ){
  ## add alpha to color given as a name
  col    <- col2rgb( col, alpha=TRUE )/255
  col[4] <- alpha
  col    <- rgb(col[1,],col[2,],col[3,],col[4,])
  return( col )
}

magn <- 4
ncols <- 2
nrows <- 1
widths <- magn*c(2,1)
heights <- rep(magn,nrows)
order <- matrix(c(1:(nrows*ncols)),nrows,ncols,byrow =TRUE)

pdf( "fluc_holoLU2.pdf", width=sum(widths), height=sum(heights) )

  panel <- layout(
            order,
            widths=widths,
            heights=heights,
            TRUE
            )

  ## HOLOCENE
  ylim <- c(-50,400)
  xlim <- c(-10,1.5)
  par( xaxs="i", yaxs="i", las=1, mar=c(5,4,1,0.5) )
  plot( xlim, ylim, type="n", xlim=xlim, ylim=ylim, axes=FALSE, ylab="cumulative eLUC (GtC)", xlab="1000 years CE", lwd=1.5, col="grey50" )
  axis(1, at=seq(xlim[1],xlim[2],by=1.000),tck=-0.03, lwd=1.5); axis( 1 , at=seq(xlim[1],xlim[2],by=0.100),labels=F, tck=-0.01)
  axis(3, at=seq(xlim[1],xlim[2],by=1.000),tck=-0.03, lwd=1.5, labels=F ); axis( 3 , at=seq(xlim[1],xlim[2],by=0.100),labels=F, tck=-0.01)
  axis( 2, at=seq(ylim[1],ylim[2],by=50),tck=-0.03, lwd=1.5); axis( 2 , at=seq(ylim[1],ylim[2],by=10),labels=F, tck=-0.01)
  axis( 4, at=seq(ylim[1],ylim[2],by=50),tck=-0.03, lwd=1.5, labels=F ); axis( 4 , at=seq(ylim[1],ylim[2],by=10),labels=F, tck=-0.01)

  box( lwd=1.5 )

  rect( xlim[1], mintotc[1], xlim[2], maxtotc, col=rgb(0,0,0,0.2), border=NA )

  lines( fluc_rdc$hyde31$mill, fluc_rdc$hyde31$cumfluc, col="tomato", lwd=1.5  )
  polygon( c(fluc_rdc$hyde31$mill, rev(fluc_rdc$hyde31$mill)), c(fluc_rdc$hyde31$cumfluc, rev(fluc_rdc$hyde31u$cumfluc)), col=add_alpha("tomato", 0.5), border=NA )

  lines( fluc_rdc$hyde32$mill, fluc_rdc$hyde32$cumfluc, col="orchid", lwd=1.5  )
  polygon( c(fluc_rdc$hyde32$mill, rev(fluc_rdc$hyde32$mill)), c(fluc_rdc$hyde32$cumfluc, rev(fluc_rdc$hyde32u$cumfluc)), col=add_alpha("orchid", 0.5), border=NA )

  lines( fluc_rdc$kk10$mill, fluc_rdc$kk10$cumfluc, col="turquoise3", lwd=1.5, lty=2 )
  lines( fluc_rdc$kk10$mill, fluc_rdc$kk10d$cumfluc, col="turquoise3", lwd=1.5 )

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
  rect( period_margins[1]/1000, -50, period_margins[2]/1000, 400, col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3]/1000, -50, period_margins[4]/1000, 400, col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5]/1000, -50, period_margins[6]/1000, 400, col=rgb(0,0,0,0.1), border=NA )

  legend( "topleft", c("HYDE 3.1", "HYDE 3.2", "KK10", "KK10D" ), col=c("tomato","orchid","turquoise3","turquoise3"), bty="n", lty=c(1,1,2,1), lwd=1.5 )

  ## LAST MILLENNIUM
  xlim <- c(1500,2000)
  par( xaxs="i", yaxs="i", las=1, mar=c(5,0.5,1,4))
  plot( xlim, ylim, type="n", xlim=xlim, ylim=ylim, axes=FALSE, ylab="", xlab="years CE", lwd=1.5)
  axis( 1, at=seq(xlim[1],xlim[2],by=100),tck=-0.03, lwd=1.5); axis( 1 , at=seq(xlim[1],xlim[2],by=10),labels=F, tck=-0.01)
  axis( 3, at=seq(xlim[1],xlim[2],by=100),tck=-0.03, lwd=1.5, labels=F ); axis( 3 , at=seq(xlim[1],xlim[2],by=10),labels=F, tck=-0.01)
  axis( 2, at=seq(ylim[1],ylim[2],by=50),tck=-0.03, lwd=1.5, labels=F); axis( 2 , at=seq(ylim[1],ylim[2],by=10),labels=F, tck=-0.01)
  axis( 4, at=seq(ylim[1],ylim[2],by=50),tck=-0.03, lwd=1.5 ); axis( 4 , at=seq(ylim[1],ylim[2],by=10),labels=F, tck=-0.01)
  box( lwd=1.5 )

  rect( xlim[1], mintotc[1], xlim[2], maxtotc, col=rgb(0,0,0,0.2), border=NA )

  lines( fluc_rdc$hyde31$year, fluc_rdc$hyde31$cumfluc, col="tomato", lwd=1.5  )
  polygon( c(fluc_rdc$hyde31$year, rev(fluc_rdc$hyde31$year)), c(fluc_rdc$hyde31$cumfluc, rev(fluc_rdc$hyde31u$cumfluc)), col=add_alpha("tomato", 0.5), border=NA )

  lines( fluc_rdc$hyde32$year, fluc_rdc$hyde32$cumfluc, col="orchid", lwd=1.5  )
  polygon( c(fluc_rdc$hyde32$year, rev(fluc_rdc$hyde32$year)), c(fluc_rdc$hyde32$cumfluc, rev(fluc_rdc$hyde32u$cumfluc)), col=add_alpha("orchid", 0.5), border=NA )

  lines( fluc_rdc$kk10$year,  fluc_rdc$kk10$cumfluc, col="turquoise3", lwd=1.5, lty=2 )
  lines( fluc_rdc$kk10d$year, fluc_rdc$kk10d$cumfluc, col="turquoise3", lwd=1.5 )

  period_margins <- read.csv( 'periods_lastmill.csv' )$period_margins
  periodsName <- paste( 
    as.character( period_margins )[1:length(period_margins)-1],
    "-",
    as.character( period_margins )[2:length(period_margins)],
    sep=""
    )
  nper <- length(period_margins)-1
  rect( period_margins[1], -50, period_margins[2], 400, col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], -50, period_margins[4], 400, col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5], -50, period_margins[6], 400, col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[7], -50, period_margins[8], 400, col=rgb(0,0,0,0.1), border=NA )

dev.off()

## Print cumulative fluxes (-1850, 1850-2000, -2000)
i1850 <- which.min( abs(fluc$hyde31$year - 1850) )
i2000 <- which.min( abs(fluc$hyde31$year - 2000) )
print(      "CUMULATIVE FLUX | -1850      | 1850-2000   | -2000 ")
print(      "-------------------------------------------------- ")
print(paste("HYDE 3.1, basel.|",fluc$hyde31$cumfluc[i1850], "|",fluc$hyde31$cumfluc[i2000] -fluc$hyde31$cumfluc[i1850], "  |",fluc$hyde31$cumfluc[i2000]))
print(paste("HYDE 3.1, upper |",fluc$hyde31u$cumfluc[i1850],"|",fluc$hyde31u$cumfluc[i2000]-fluc$hyde31u$cumfluc[i1850],"  |",fluc$hyde31u$cumfluc[i2000]))
print(paste("KK10            |",fluc$kk10$cumfluc[i1850], "|",fluc$kk10$cumfluc[i2000]- fluc$kk10$cumfluc[i1850], "  |",fluc$kk10$cumfluc[i2000]))
print(paste("KK10D           |",fluc$kk10d$cumfluc[i1850],"|",fluc$kk10d$cumfluc[i2000]-fluc$kk10d$cumfluc[i1850],"  |",fluc$kk10d$cumfluc[i2000]))
print(paste("HYDE 3.2, basel.|",fluc$hyde32$cumfluc[i1850], "|",fluc$hyde32$cumfluc[i2000] -fluc$hyde32$cumfluc[i1850], "  |",fluc$hyde32$cumfluc[i2000]))
print(paste("HYDE 3.2, upper |",fluc$hyde32u$cumfluc[i1850],"|",fluc$hyde32u$cumfluc[i2000]-fluc$hyde32u$cumfluc[i1850],"  |",fluc$hyde32u$cumfluc[i2000]))


