fluc_r1 <- read.table( "fluc_r1_holoLU2.dat", col.names=c("year","fluc") )
fluc_r2 <- read.table( "fluc_r2_holoLU2.dat", col.names=c("year","fluc") )
fluc_r3 <- read.table( "fluc_r3_holoLU2.dat", col.names=c("year","fluc") )
fluc_r4 <- read.table( "fluc_r4_holoLU2.dat", col.names=c("year","fluc") )
fluc_r5 <- read.table( "fluc_r5_holoLU2.dat", col.names=c("year","fluc") )
fluc_r7 <- read.table( "fluc_r7_holoLU2.dat", col.names=c("year","fluc") )
fluc_r8 <- read.table( "fluc_r8_holoLU2.dat", col.names=c("year","fluc") )
fluc_r9 <- read.table( "fluc_r9_holoLU2.dat", col.names=c("year","fluc") )
fluc_r1l <- read.table( "fluc_r1lholoLU2.dat", col.names=c("year","fluc") )

fluc_r1$fluc[1] <- 0
fluc_r2$fluc[1] <- 0
fluc_r3$fluc[1] <- 0
fluc_r4$fluc[1] <- 0
fluc_r5$fluc[1] <- 0
fluc_r7$fluc[1] <- 0
fluc_r8$fluc[1] <- 0
fluc_r9$fluc[1] <- 0
fluc_r1l$fluc[1] <- 0

fluc_r1$mill <- fluc_r1$year / 1000
fluc_r2$mill <- fluc_r2$year / 1000
fluc_r3$mill <- fluc_r3$year / 1000
fluc_r4$mill <- fluc_r4$year / 1000
fluc_r5$mill <- fluc_r5$year / 1000
fluc_r7$mill <- fluc_r7$year / 1000
fluc_r8$mill <- fluc_r8$year / 1000
fluc_r9$mill <- fluc_r9$year / 1000
fluc_r1l$mill<- fluc_r1l$year / 1000

istart <- which.min( abs( fluc_r1$year - (-10000) ) )
iend   <- length( fluc_r1$year )

fluc_r1 <- fluc_r1[ istart:iend, ]
fluc_r2 <- fluc_r2[ istart:iend, ]
fluc_r3 <- fluc_r3[ istart:iend, ]
fluc_r4 <- fluc_r4[ istart:iend, ]
fluc_r5 <- fluc_r5[ istart:iend, ]
fluc_r7 <- fluc_r7[ istart:iend, ]
fluc_r8 <- fluc_r8[ istart:iend, ]
fluc_r9 <- fluc_r9[ istart:iend, ]
fluc_r1l <- fluc_r1l[ istart:iend, ]

fluc_r1$cumfluc <- cumsum(fluc_r1$fluc)
fluc_r2$cumfluc <- cumsum(fluc_r2$fluc)
fluc_r3$cumfluc <- cumsum(fluc_r3$fluc)
fluc_r4$cumfluc <- cumsum(fluc_r4$fluc)
fluc_r5$cumfluc <- cumsum(fluc_r5$fluc)
fluc_r7$cumfluc <- cumsum(fluc_r7$fluc)
fluc_r8$cumfluc <- cumsum(fluc_r8$fluc)
fluc_r9$cumfluc <- cumsum(fluc_r9$fluc)
fluc_r1l$cumfluc <- cumsum(fluc_r1l$fluc)

fluc_r1_rdc <- fluc_r1[ seq( 1, dim( fluc_r1 )[1], 10 ), ]
fluc_r2_rdc <- fluc_r2[ seq( 1, dim( fluc_r2 )[1], 10 ), ]
fluc_r3_rdc <- fluc_r3[ seq( 1, dim( fluc_r3 )[1], 10 ), ]
fluc_r4_rdc <- fluc_r4[ seq( 1, dim( fluc_r4 )[1], 10 ), ]
fluc_r5_rdc <- fluc_r5[ seq( 1, dim( fluc_r5 )[1], 10 ), ]
fluc_r7_rdc <- fluc_r7[ seq( 1, dim( fluc_r7 )[1], 10 ), ]
fluc_r8_rdc <- fluc_r8[ seq( 1, dim( fluc_r8 )[1], 10 ), ]
fluc_r9_rdc <- fluc_r9[ seq( 1, dim( fluc_r9 )[1], 10 ), ]
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

fluc <- list( 
  hyde31  =fluc_r1,
  hyde31u =fluc_r3,
  hyde32  =fluc_r8,
  hyde32u =fluc_r9,
  kk10    =fluc_r2,
  kk10d   =fluc_r7
  )

fluc_rdc <- list(
  hyde31  =fluc_r1_rdc,
  hyde31u =fluc_r3_rdc,
  hyde32  =fluc_r8_rdc,
  hyde32u =fluc_r9_rdc,
  kk10    =fluc_r2_rdc,
  kk10d   =fluc_r7_rdc
  )

## save to file
save( fluc, fluc_rdc, mintotc, maxtotc, file="fluc_holoLU2.Rdata" )

## Evaluate fluc in periods: HOLOCENE
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

## Create data frame for evaluating periods
## period margins (start and end)
fluc_per_holocene <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
fluc_per_holocene$iper_start <- rep( NA, nper )
fluc_per_holocene$iper_end <- rep( NA, nper )

fluc_per_holocene$hyde31   <- rep( NA, nper )
fluc_per_holocene$hyde31u  <- rep( NA, nper )
fluc_per_holocene$hyde32   <- rep( NA, nper )
fluc_per_holocene$hyde32u  <- rep( NA, nper )
fluc_per_holocene$kk10     <- rep( NA, nper )
fluc_per_holocene$kk10d    <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){
  fluc_per_holocene$iper_start[i] <- which( fluc_per_holocene$per_start[i] == fluc_r1$year )
  fluc_per_holocene$iper_end[i]   <- which( fluc_per_holocene$per_end[i]   == fluc_r1$year )

  fluc_per_holocene$hyde31  [i] <- fluc$hyde31  $cumfluc[ fluc_per_holocene$iper_end[i] ] - fluc$hyde31  $cumfluc[ fluc_per_holocene$iper_start[i] ] 
  fluc_per_holocene$hyde31u [i] <- fluc$hyde31u $cumfluc[ fluc_per_holocene$iper_end[i] ] - fluc$hyde31u $cumfluc[ fluc_per_holocene$iper_start[i] ] 
  fluc_per_holocene$hyde32  [i] <- fluc$hyde32  $cumfluc[ fluc_per_holocene$iper_end[i] ] - fluc$hyde32  $cumfluc[ fluc_per_holocene$iper_start[i] ] 
  fluc_per_holocene$hyde32u [i] <- fluc$hyde32u $cumfluc[ fluc_per_holocene$iper_end[i] ] - fluc$hyde32u $cumfluc[ fluc_per_holocene$iper_start[i] ] 
  fluc_per_holocene$kk10    [i] <- fluc$kk10    $cumfluc[ fluc_per_holocene$iper_end[i] ] - fluc$kk10    $cumfluc[ fluc_per_holocene$iper_start[i] ] 
  fluc_per_holocene$kk10d   [i] <- fluc$kk10d   $cumfluc[ fluc_per_holocene$iper_end[i] ] - fluc$kk10d   $cumfluc[ fluc_per_holocene$iper_start[i] ] 
}


## Evaluate fluc in periods: LAST MILLENNIUM
period_margins <- read.csv( 'periods_lastmill.csv' )$period_margins
periodsName <- paste( 
  as.character( period_margins )[1:length(period_margins)-1],
  "-",
  as.character( period_margins )[2:length(period_margins)],
  sep=""
  )
nper <- length(period_margins)-1

## Create data frame for evaluating periods
## period margins (start and end)
fluc_per_lastmill <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
fluc_per_lastmill$iper_start <- rep( NA, nper )
fluc_per_lastmill$iper_end <- rep( NA, nper )
fluc_per_lastmill$hyde31  <- rep( NA, nper )
fluc_per_lastmill$hyde31u <- rep( NA, nper )
fluc_per_lastmill$hyde32  <- rep( NA, nper )
fluc_per_lastmill$hyde32u <- rep( NA, nper )
fluc_per_lastmill$kk10    <- rep( NA, nper )
fluc_per_lastmill$kk10d   <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){
  fluc_per_lastmill$iper_start[i] <- which( fluc_per_lastmill$per_start[i] == fluc_r1$year )
  fluc_per_lastmill$iper_end[i] <- which( fluc_per_lastmill$per_end[i] == fluc_r1$year )

  fluc_per_lastmill$hyde31  [i] <- fluc$hyde31  $cumfluc[ fluc_per_lastmill$iper_end[i] ] - fluc$hyde31  $cumfluc[ fluc_per_lastmill$iper_start[i] ] 
  fluc_per_lastmill$hyde31u [i] <- fluc$hyde31u $cumfluc[ fluc_per_lastmill$iper_end[i] ] - fluc$hyde31u $cumfluc[ fluc_per_lastmill$iper_start[i] ] 
  fluc_per_lastmill$hyde32  [i] <- fluc$hyde32  $cumfluc[ fluc_per_lastmill$iper_end[i] ] - fluc$hyde32  $cumfluc[ fluc_per_lastmill$iper_start[i] ] 
  fluc_per_lastmill$hyde32u [i] <- fluc$hyde32u $cumfluc[ fluc_per_lastmill$iper_end[i] ] - fluc$hyde32u $cumfluc[ fluc_per_lastmill$iper_start[i] ] 
  fluc_per_lastmill$kk10    [i] <- fluc$kk10    $cumfluc[ fluc_per_lastmill$iper_end[i] ] - fluc$kk10    $cumfluc[ fluc_per_lastmill$iper_start[i] ] 
  fluc_per_lastmill$kk10d   [i] <- fluc$kk10d   $cumfluc[ fluc_per_lastmill$iper_end[i] ] - fluc$kk10d   $cumfluc[ fluc_per_lastmill$iper_start[i] ] 
}

save( fluc_per_holocene, fluc_per_lastmill, file="fluc_per.Rdata")
