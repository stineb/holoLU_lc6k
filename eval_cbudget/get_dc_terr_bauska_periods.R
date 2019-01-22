load('dc_terr_bauska_lastmill.Rdata')
library(Hmisc)

period_margins <- c( 760, 960, 1200, 1500, 1650, 1760, 1920 ) ## use this
periodsName <- paste( as.character(period_margins[1:6]), "-", as.character(period_margins[2:7]), sep="" )
nper <- length(period_margins)-1

## Create data frame for evaluating periods
## period margins (start and end)
dc_per_bauska <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
dc_per_bauska$iper_start <- rep( NA, nper )
dc_per_bauska$iper_end <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){
  dc_per_bauska$iper_start[i] <- which.min( abs( dc_per_bauska$per_start[i] - df_cum_land_uptake_bauska$year ) )
  dc_per_bauska$iper_end[i] <- which.min( abs( dc_per_bauska$per_end[i] - df_cum_land_uptake_bauska$year ) )
}

## Evaluate cumulative balance change in different periods for each run
for (n in 1:nruns){

  numstring <- sprintf( "%04d", n-1)
  colstring <- paste( "r", numstring, sep="" )

  dc_per_bauska[[ colstring ]] <- rep( NA, nper )

  for (i in 1:nper){

    dc_per_bauska[[ colstring ]][i] <- df_cum_land_uptake_bauska[[ colstring ]][dc_per_bauska$iper_end[i]] - df_cum_land_uptake_bauska[[ colstring ]][dc_per_bauska$iper_start[i]]

  }

}

## add column for period "name"
dc_per_bauska$name <- paste( as.character( dc_per_bauska$per_start ) , "-", as.character( dc_per_bauska$per_end ), sep="" )

## Get mean and SD of cumulative balance changes in each period
dc_per_bauska$mean     <- apply( dc_per_bauska[,2:nruns+1], c(1), FUN=mean )
dc_per_bauska$median   <- apply( dc_per_bauska[,2:nruns+1], c(1), FUN=median )
dc_per_bauska$sd       <- apply( dc_per_bauska[,2:nruns+1], c(1), FUN=sd )
dc_per_bauska$q90      <- apply( dc_per_bauska[,2:nruns+1], c(1), FUN=function(x) quantile( x, 0.9 ) )
dc_per_bauska$q10      <- apply( dc_per_bauska[,2:nruns+1], c(1), FUN=function(x) quantile( x, 0.1 ) )

## take only subset of dc_per_bauska
dc_sub <- subset( dc_per_bauska, select=c( name, per_start, per_end, iper_start, iper_end, mean, median, q90, q10, sd ) )

## create data frames to be plotted with hist() and errbar()
dc_bauska_outarr <- data.frame( mean=dc_sub$mean*1e-14, median=dc_sub$median*1e-14, sd=dc_sub$sd*1e-14, q10=dc_sub$q10*1e-14, q90=dc_sub$q90*1e-14 )
row.names( dc_bauska_outarr ) <- dc_sub$name

## Plot bars for the periods
pdf("cbal_barplot_lastmill_holoLU2.pdf", width=8, height=6 )
par( las=1 )
ylim <- c(-100,100)

mybar1 <- barplot( 
                  t(dc_bauska_outarr$mean),
                  ylim=ylim,
                  col=c("green3"),
                  border=TRUE,
                  xlab="period [year AD]"
                  )
abline(0,0)

errbar(
       mybar1,
       t(dc_bauska_outarr$mean),
       t(dc_bauska_outarr$mean+dc_bauska_outarr$sd),
       t(dc_bauska_outarr$mean-dc_bauska_outarr$sd),
       add=TRUE
       )

## add grey rectangles 
left <- mybar1 - 0.6
right <- mybar1 + 0.6
rect( left[1], ylim[1], right[1], ylim[2], col=rgb(0,0,0,0.1), border=NA )
rect( left[3], ylim[1], right[3], ylim[2], col=rgb(0,0,0,0.1), border=NA )
rect( left[5], ylim[1], right[5], ylim[2], col=rgb(0,0,0,0.1), border=NA )

dev.off()

save( df_cum_land_uptake_bauska, dc_per_bauska, dc_bauska_outarr, file="dc_terr_bauska.Rdata" )
