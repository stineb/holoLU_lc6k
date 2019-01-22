fluc_r1 <- read.table( "fluc_r1_holoLU2.dat", col.names=c("year","fluc") )
fluc_r2 <- read.table( "fluc_r2_holoLU2.dat", col.names=c("year","fluc") )
fluc_r3 <- read.table( "fluc_r3_holoLU2.dat", col.names=c("year","fluc") )
fluc_r4 <- read.table( "fluc_r4_holoLU2.dat", col.names=c("year","fluc") )
fluc_r5 <- read.table( "fluc_r5_holoLU2.dat", col.names=c("year","fluc") )
fluc_r1l <- read.table( "fluc_r1lholoLU2.dat", col.names=c("year","fluc") )

fluc_r1$fluc[1] <- 0
fluc_r2$fluc[1] <- 0
fluc_r3$fluc[1] <- 0
fluc_r4$fluc[1] <- 0
fluc_r5$fluc[1] <- 0
fluc_r1l$fluc[1] <- 0

fluc_r1$mill <- fluc_r1$year / 1000
fluc_r2$mill <- fluc_r2$year / 1000
fluc_r3$mill <- fluc_r3$year / 1000
fluc_r4$mill <- fluc_r4$year / 1000
fluc_r5$mill <- fluc_r5$year / 1000
fluc_r1l$mill<- fluc_r1l$year / 1000

istart <- which.min( abs( fluc_r1$year - (-10000) ) )
iend   <- length( fluc_r1$year )

fluc_r1 <- fluc_r1[ istart:iend, ]
fluc_r2 <- fluc_r2[ istart:iend, ]
fluc_r3 <- fluc_r3[ istart:iend, ]
fluc_r4 <- fluc_r4[ istart:iend, ]
fluc_r5 <- fluc_r5[ istart:iend, ]
fluc_r1l <- fluc_r1l[ istart:iend, ]

fluc_r1$cumfluc <- cumsum(fluc_r1$fluc)
fluc_r2$cumfluc <- cumsum(fluc_r2$fluc)
fluc_r3$cumfluc <- cumsum(fluc_r3$fluc)
fluc_r4$cumfluc <- cumsum(fluc_r4$fluc)
fluc_r5$cumfluc <- cumsum(fluc_r5$fluc)
fluc_r1l$cumfluc <- cumsum(fluc_r1l$fluc)

fluc_r1_rdc <- fluc_r1[ seq( 1, dim( fluc_r1 )[1], 10 ), ]
fluc_r2_rdc <- fluc_r2[ seq( 1, dim( fluc_r2 )[1], 10 ), ]
fluc_r3_rdc <- fluc_r3[ seq( 1, dim( fluc_r3 )[1], 10 ), ]
fluc_r4_rdc <- fluc_r4[ seq( 1, dim( fluc_r4 )[1], 10 ), ]
fluc_r5_rdc <- fluc_r5[ seq( 1, dim( fluc_r5 )[1], 10 ), ]
fluc_r1l_rdc <- fluc_r1l[ seq( 1, dim( fluc_r1l )[1], 10 ), ]

totc <- read.table( "../output_holoLU2/trans_r0_holoLU2.totc.out", col.names=c("year","totc", "lutotc") )
istart <- which.min( abs( totc$year - (-10000) ) )
iend   <- length( totc$year )
totc <- totc[ istart:iend, ]
totc$dtotc <- totc$totc - mean( totc$totc )
totc$mill  <- totc$year / 1000
totc_rdc <- totc[ seq( 1, dim( totc )[1], 10 ), ]
mintotc <- min(totc$dtotc)
maxtotc <- max(totc$dtotc)

name_scen <- c(
  "HYDE standard",
  "KK11",
  "HYDE upper",
  "HYDE concave",
  "HYDE Ruddiman",
  "HYDE standard, slow land turnover"
  )

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

  ylim <- c(-50,400)
  xlim <- c(-10,1.5)
  par( xaxs="i", yaxs="i", las=1, mar=c(5,4,1,0.5) )
  plot( xlim, ylim, type="n", xlim=xlim, ylim=ylim, axes=FALSE, ylab="cumulative eLUC (GtC)", xlab="1000 years AD", lwd=1.5, col="grey50" )
  axis(1, at=seq(xlim[1],xlim[2],by=1.000),tck=-0.03, lwd=1.5); axis( 1 , at=seq(xlim[1],xlim[2],by=0.100),labels=F, tck=-0.01)
  axis(3, at=seq(xlim[1],xlim[2],by=1.000),tck=-0.03, lwd=1.5, labels=F ); axis( 3 , at=seq(xlim[1],xlim[2],by=0.100),labels=F, tck=-0.01)
  axis( 2, at=seq(ylim[1],ylim[2],by=50),tck=-0.03, lwd=1.5); axis( 2 , at=seq(ylim[1],ylim[2],by=10),labels=F, tck=-0.01)
  axis( 4, at=seq(ylim[1],ylim[2],by=50),tck=-0.03, lwd=1.5, labels=F ); axis( 4 , at=seq(ylim[1],ylim[2],by=10),labels=F, tck=-0.01)

  box( lwd=1.5 )

  rect( xlim[1], mintotc[1], xlim[2], maxtotc, col=rgb(0,0,0,0.2), border=NA )
  lines( fluc_r1_rdc$mill, fluc_r1_rdc$cumfluc, col="black", lwd=1.5  )
  lines( fluc_r2_rdc$mill, fluc_r2_rdc$cumfluc, col="red", lwd=1.5 )
  lines( fluc_r3_rdc$mill, fluc_r3_rdc$cumfluc, col="green", lwd=1.5 )
  lines( fluc_r4_rdc$mill, fluc_r4_rdc$cumfluc, col="blue", lwd=1.5 )
  lines( fluc_r5_rdc$mill, fluc_r5_rdc$cumfluc, col="magenta", lwd=1.5 )
  lines( fluc_r1l_rdc$mill, fluc_r1l_rdc$cumfluc, col="grey50", lwd=1.5  )

  period_margins <- c( 760, 960, 1200, 1500, 1650, 1760, 1920 )
  # period_margins <- c( 900, 1100, 1300, 1500, 1700, 1900 )
  rect( period_margins[1], -50, period_margins[2], -30, col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], -50, period_margins[4], -30, col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5], -50, period_margins[6], -30, col=rgb(0,0,0,0.1), border=NA )

  periodsBP <- c(-11000,-9000,-7000,-5000,-3000,-1000)
  periodsAD <- periodsBP + 1950
  periodsName <- c("11-9","9-7","7-5","5-3","3-1")
  periodsBP <- periodsAD - 1950
  period_margins <- periodsAD
  nper <- length(period_margins)-1

  period_margins <- periodsAD/1000
  rect( period_margins[1], -30, period_margins[2], -10, col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], -30, period_margins[4], -10, col=rgb(0,0,0,0.1), border=NA )

  legend( "topleft", name_scen, col=c("black","red","green","blue","magenta","grey50"), bty="n", lty=1, lwd=1.5 )

  xlim <- c(1500,2000)
  par( xaxs="i", yaxs="i", las=1, mar=c(5,0.5,1,4))
  plot( xlim, ylim, type="n", xlim=xlim, ylim=ylim, axes=FALSE, ylab="", xlab="years AD", lwd=1.5)
  axis( 1, at=seq(xlim[1],xlim[2],by=100),tck=-0.03, lwd=1.5); axis( 1 , at=seq(xlim[1],xlim[2],by=10),labels=F, tck=-0.01)
  axis( 3, at=seq(xlim[1],xlim[2],by=100),tck=-0.03, lwd=1.5, labels=F ); axis( 3 , at=seq(xlim[1],xlim[2],by=10),labels=F, tck=-0.01)
  axis( 2, at=seq(ylim[1],ylim[2],by=50),tck=-0.03, lwd=1.5, labels=F); axis( 2 , at=seq(ylim[1],ylim[2],by=10),labels=F, tck=-0.01)
  axis( 4, at=seq(ylim[1],ylim[2],by=50),tck=-0.03, lwd=1.5 ); axis( 4 , at=seq(ylim[1],ylim[2],by=10),labels=F, tck=-0.01)
  box( lwd=1.5 )

  rect( xlim[1], mintotc[1], xlim[2], maxtotc, col=rgb(0,0,0,0.2), border=NA )
  lines( fluc_r1_rdc$year, fluc_r1_rdc$cumfluc, col="black", lwd=1.5  )
  lines( fluc_r2_rdc$year, fluc_r2_rdc$cumfluc, col="red", lwd=1.5 )
  lines( fluc_r3_rdc$year, fluc_r3_rdc$cumfluc, col="green", lwd=1.5 )
  lines( fluc_r4_rdc$year, fluc_r4_rdc$cumfluc, col="blue", lwd=1.5 )
  lines( fluc_r5_rdc$year, fluc_r5_rdc$cumfluc, col="magenta", lwd=1.5 )
  lines( fluc_r1l_rdc$year, fluc_r1l_rdc$cumfluc, col="grey50", lwd=1.5 )

  period_margins <- c( 760, 960, 1200, 1500, 1650, 1760, 1920 )
  # period_margins <- c( 900, 1100, 1300, 1500, 1700, 1900 )
  rect( period_margins[1], -50, period_margins[2], -30, col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], -50, period_margins[4], -30, col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5], -50, period_margins[6], -30, col=rgb(0,0,0,0.1), border=NA )

dev.off()

## Print cumulative fluxes (-1850, 1850-2000, -2000)
i1850 <- which.min( abs(fluc_r1_rdc$year - 1850) )
i2000 <- which.min( abs(fluc_r1_rdc$year - 2000) )
print(      "CUMULATIVE FLUX | -1850      | 1850-2000   | -2000 ")
print(      "-------------------------------------------------- ")
print(paste("HYDE            |",fluc_r1_rdc$cumfluc[i1850],"|",fluc_r1_rdc$cumfluc[i2000]-fluc_r1_rdc$cumfluc[i1850],"  |",fluc_r1_rdc$cumfluc[i2000]))
print(paste("KK11            |",fluc_r2_rdc$cumfluc[i1850],"|",fluc_r2_rdc$cumfluc[i2000]-fluc_r2_rdc$cumfluc[i1850],"  |",fluc_r2_rdc$cumfluc[i2000]))
print(paste("upper           |",fluc_r3_rdc$cumfluc[i1850],"|",fluc_r3_rdc$cumfluc[i2000]-fluc_r3_rdc$cumfluc[i1850],"  |",fluc_r3_rdc$cumfluc[i2000]))
print(paste("concave         |",fluc_r4_rdc$cumfluc[i1850],"|",fluc_r4_rdc$cumfluc[i2000]-fluc_r4_rdc$cumfluc[i1850],"  |",fluc_r4_rdc$cumfluc[i2000]))
print(paste("Ruddiman        |",fluc_r5_rdc$cumfluc[i1850],"|",fluc_r5_rdc$cumfluc[i2000]-fluc_r5_rdc$cumfluc[i1850],"  |",fluc_r5_rdc$cumfluc[i2000]))
print(paste("HYDE slow       |",fluc_r1l_rdc$cumfluc[i1850],"|",fluc_r1l_rdc$cumfluc[i2000]-fluc_r1l_rdc$cumfluc[i1850],"  |",fluc_r1l_rdc$cumfluc[i2000]))

## Evaluate fluc in periods: HOLOCENE
periodsBP <- c(-11000,-9000,-7000,-5000,-3000,-1000)
periodsAD <- periodsBP + 1950
periodsName <- c("11-9","9-7","7-5","5-3","3-1")

# ## 11 kyr BP is to be represented by -8990 AD = 10940 BP
# periodsAD[1] <- -8990
# periodsBP <- periodsAD - 1950

period_margins <- periodsAD
nper <- length(period_margins)-1

## Create data frame for evaluating periods
## period margins (start and end)
fluc_per_holocene <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
fluc_per_holocene$iper_start <- rep( NA, nper )
fluc_per_holocene$iper_end <- rep( NA, nper )
fluc_per_holocene$r1 <- rep( NA, nper )
fluc_per_holocene$r2 <- rep( NA, nper )
fluc_per_holocene$r3 <- rep( NA, nper )
fluc_per_holocene$r4 <- rep( NA, nper )
fluc_per_holocene$r5 <- rep( NA, nper )
fluc_per_holocene$r1l <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){
  fluc_per_holocene$iper_start[i] <- which( fluc_per_holocene$per_start[i] == fluc_r1$year )
  fluc_per_holocene$iper_end[i]   <- which( fluc_per_holocene$per_end[i]   == fluc_r1$year )

  fluc_per_holocene$r1[i] <- fluc_r1$cumfluc[fluc_per_holocene$iper_end[i]] - fluc_r1$cumfluc[fluc_per_holocene$iper_start[i]] 
  fluc_per_holocene$r2[i] <- fluc_r2$cumfluc[fluc_per_holocene$iper_end[i]] - fluc_r2$cumfluc[fluc_per_holocene$iper_start[i]] 
  fluc_per_holocene$r3[i] <- fluc_r3$cumfluc[fluc_per_holocene$iper_end[i]] - fluc_r3$cumfluc[fluc_per_holocene$iper_start[i]] 
  fluc_per_holocene$r4[i] <- fluc_r4$cumfluc[fluc_per_holocene$iper_end[i]] - fluc_r4$cumfluc[fluc_per_holocene$iper_start[i]] 
  fluc_per_holocene$r5[i] <- fluc_r5$cumfluc[fluc_per_holocene$iper_end[i]] - fluc_r5$cumfluc[fluc_per_holocene$iper_start[i]] 
  fluc_per_holocene$r1l[i] <- fluc_r1l$cumfluc[fluc_per_holocene$iper_end[i]] - fluc_r1l$cumfluc[fluc_per_holocene$iper_start[i]] 
}

## Evaluate fluc in periods: LAST MILLENNIUM
period_margins <- c( 760, 960, 1200, 1500, 1650, 1760, 1920 )
# period_margins <- c( 900, 1100, 1300, 1500, 1700, 1900 )
nper <- length(period_margins)-1

## Create data frame for evaluating periods
## period margins (start and end)
fluc_per_lastmill <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
fluc_per_lastmill$iper_start <- rep( NA, nper )
fluc_per_lastmill$iper_end <- rep( NA, nper )
fluc_per_lastmill$r1 <- rep( NA, nper )
fluc_per_lastmill$r2 <- rep( NA, nper )
fluc_per_lastmill$r3 <- rep( NA, nper )
fluc_per_lastmill$r4 <- rep( NA, nper )
fluc_per_lastmill$r5 <- rep( NA, nper )
fluc_per_lastmill$r1l <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){
  fluc_per_lastmill$iper_start[i] <- which( fluc_per_lastmill$per_start[i] == fluc_r1$year )
  fluc_per_lastmill$iper_end[i] <- which( fluc_per_lastmill$per_end[i] == fluc_r1$year )

  fluc_per_lastmill$r1[i] <- fluc_r1$cumfluc[fluc_per_lastmill$iper_end[i]] - fluc_r1$cumfluc[fluc_per_lastmill$iper_start[i]] 
  fluc_per_lastmill$r2[i] <- fluc_r2$cumfluc[fluc_per_lastmill$iper_end[i]] - fluc_r2$cumfluc[fluc_per_lastmill$iper_start[i]] 
  fluc_per_lastmill$r3[i] <- fluc_r3$cumfluc[fluc_per_lastmill$iper_end[i]] - fluc_r3$cumfluc[fluc_per_lastmill$iper_start[i]] 
  fluc_per_lastmill$r4[i] <- fluc_r4$cumfluc[fluc_per_lastmill$iper_end[i]] - fluc_r4$cumfluc[fluc_per_lastmill$iper_start[i]] 
  fluc_per_lastmill$r5[i] <- fluc_r5$cumfluc[fluc_per_lastmill$iper_end[i]] - fluc_r5$cumfluc[fluc_per_lastmill$iper_start[i]] 
  fluc_per_lastmill$r1l[i] <- fluc_r1l$cumfluc[fluc_per_lastmill$iper_end[i]] - fluc_r1l$cumfluc[fluc_per_lastmill$iper_start[i]] 
}

save( fluc_per_holocene, fluc_per_lastmill, file="fluc_per.Rdata")
