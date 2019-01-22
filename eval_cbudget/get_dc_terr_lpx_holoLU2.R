trace128 <- list()

## peat variables averaged across sites
filn <- "totc_trace21_128.dat"
col.names <- c( "year", "totc" )
trace128 <- read.table( filn, col.names=col.names )
spar <- 0.7
trace128$totc.spl <- smooth.spline( trace128, spar=spar )$y

plot(  trace128$year, trace128$totc, type="l" )
lines( trace128$year, test, col="red")

#####

## HOLOCENE EVALUATION OF PERIODS
## Alternativ: fixe Margins
periodsBP <- c(-11000,-7000,-5000,-2000,-400)
periodsAD <- periodsBP + 1950
periodsName <- c("11-7","7-5","5-2","2-0 (1 - 1560 AD)")
periodsAD[1] <- -8990 ## 11 kyr BP is to be represented by -8990 AD = 10940 BP
periodsBP <- periodsAD - 1950
period_margins <- periodsAD
nper <- length(period_margins)-1

## Create data frame for evaluating periods
## period margins (start and end)
dc_per_holocene_lpx <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
dc_per_holocene_lpx$iper_start <- rep( NA, nper )
dc_per_holocene_lpx$iper_end <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){
  dc_per_holocene_lpx$iper_start[i] <- which.min( abs( dc_per_holocene_lpx$per_start[i] - trace128$year ) )
  dc_per_holocene_lpx$iper_end[i] <- which.min( abs( dc_per_holocene_lpx$per_end[i] - trace128$year ) )

  ## Evaluate cumulative balance change in different periods for each run
  dc_per_holocene_lpx$dc_lpx[i] <- (trace128$totc.spl[dc_per_holocene_lpx$iper_end[i]] - trace128$totc.spl[dc_per_holocene_lpx$iper_start[i]]) * 1e-15

}

## add column for period "name"
dc_per_holocene_lpx$name <- paste( as.character( dc_per_holocene_lpx$per_start ) , "-", as.character( dc_per_holocene_lpx$per_end ), sep="" )

## create data frames to be plotted with hist() and errbar()
row.names( dc_per_holocene_lpx ) <- periodsName
dc_lpx_holocene_outarr <- subset( dc_per_holocene_lpx, select=c( dc_lpx ) )


## LAST MILLENNIUM EVALUATION OF PERIODS
period_margins <- c( 760, 960, 1200, 1500, 1650, 1760, 1920 )
periodsName <- paste( as.character( period_margins[1:6]), "-", as.character( period_margins[2:7]) )
nper <- length(period_margins)-1

dc_per_lastmill_lpx <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
dc_per_lastmill_lpx$iper_start <- rep( NA, nper )
dc_per_lastmill_lpx$iper_end <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){
  dc_per_lastmill_lpx$iper_start[i] <- which.min( abs( dc_per_lastmill_lpx$per_start[i] - trace128$year ) )
  dc_per_lastmill_lpx$iper_end[i] <- which.min( abs( dc_per_lastmill_lpx$per_end[i] - trace128$year ) )

  ## Evaluate cumulative balance change in different periods for each run
  dc_per_lastmill_lpx$dc_lpx[i] <- (trace128$totc[dc_per_lastmill_lpx$iper_end[i]] - trace128$totc[dc_per_lastmill_lpx$iper_start[i]]) * 1e-15

}

## add column for period "name"
dc_per_lastmill_lpx$name <- paste( as.character( dc_per_lastmill_lpx$per_start ) , "-", as.character( dc_per_lastmill_lpx$per_end ), sep="" )

## create data frames to be plotted with hist() and errbar()
row.names( dc_per_lastmill_lpx ) <- periodsName
dc_lpx_lastmill_outarr <- subset( dc_per_lastmill_lpx, select=c( dc_lpx ) )


## save the stuff
save( dc_lpx_lastmill_outarr, dc_per_lastmill_lpx, dc_lpx_holocene_outarr, dc_per_holocene_lpx, file="dc_lpx.Rdata" )

