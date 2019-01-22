load('dc_terr_indus.Rdata')
library(Hmisc)

period_margins <- read.csv( 'periods_indus.csv' )$period_margins
periodsName <- paste( 
  as.character( period_margins )[1:length(period_margins)-1],
  "-",
  as.character( period_margins )[2:length(period_margins)],
  sep=""
  )
nper <- length(period_margins)-1

## Create data frame for evaluating periods
## period margins (start and end)
dc_per_indus <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
dc_per_indus$iper_start <- rep( NA, nper )
dc_per_indus$iper_end <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){
  dc_per_indus$iper_start[i] <- which.min( abs( dc_per_indus$per_start[i] - df_sum_land_uptake$year ) )
  dc_per_indus$iper_end[i] <- which.min( abs( dc_per_indus$per_end[i] - df_sum_land_uptake$year ) )

  dc_per_indus$mean <- df_sum_land_uptake$cum_mean[dc_per_indus$iper_end[i]] - df_sum_land_uptake$cum_mean[dc_per_indus$iper_start[i]]

}

## create data frames to be plotted with hist() and errbar()
dc_indus_outarr <- data.frame( mean=dc_per_indus$mean  )
row.names( dc_indus_outarr ) <- periodsName

save( df_sum_land_uptake, dc_per_indus, dc_indus_outarr, file="dc_terr_indus.Rdata" )


# ## Evaluate cumulative balance change in different periods for each run
# for (n in 1:nruns){

#   numstring <- sprintf( "%04d", n-1)
#   colstring <- paste( "r", numstring, sep="" )

#   dc_per_indus[[ colstring ]] <- rep( NA, nper )

#   for (i in 1:nper){

#     dc_per_indus[[ colstring ]][i] <- df_cum_land_uptake_bauska[[ colstring ]][dc_per_indus$iper_end[i]] - df_cum_land_uptake_bauska[[ colstring ]][dc_per_indus$iper_start[i]]

#   }

# }

# ## add column for period "name"
# dc_per_indus$name <- paste( as.character( dc_per_indus$per_start ) , "-", as.character( dc_per_indus$per_end ), sep="" )

# ## Get mean and SD of cumulative balance changes in each period
# dc_per_indus$mean     <- apply( dc_per_indus[,2:nruns+1], c(1), FUN=mean )
# dc_per_indus$median   <- apply( dc_per_indus[,2:nruns+1], c(1), FUN=median )
# dc_per_indus$sd       <- apply( dc_per_indus[,2:nruns+1], c(1), FUN=sd )
# dc_per_indus$q90      <- apply( dc_per_indus[,2:nruns+1], c(1), FUN=function(x) quantile( x, 0.9 ) )
# dc_per_indus$q10      <- apply( dc_per_indus[,2:nruns+1], c(1), FUN=function(x) quantile( x, 0.1 ) )

# ## take only subset of dc_per_indus
# dc_sub <- subset( dc_per_indus, select=c( name, per_start, per_end, iper_start, iper_end, mean, median, q90, q10, sd ) )

## create data frames to be plotted with hist() and errbar()
dc_bauska_outarr <- data.frame( mean=dc_sub$mean*1e-14, median=dc_sub$median*1e-14, sd=dc_sub$sd*1e-14, q10=dc_sub$q10*1e-14, q90=dc_sub$q90*1e-14 )
row.names( dc_bauska_outarr ) <- dc_sub$name

# ## Plot bars for the periods
# pdf("cbal_barplot_lastmill_holoLU2.pdf", width=8, height=6 )
# par( las=1 )
# ylim <- c(-100,100)

# mybar1 <- barplot( 
#                   t(dc_bauska_outarr$mean),
#                   ylim=ylim,
#                   col=c("green3"),
#                   border=TRUE,
#                   xlab="period [year AD]"
#                   )
# abline(0,0)

# errbar(
#        mybar1,
#        t(dc_bauska_outarr$mean),
#        t(dc_bauska_outarr$mean+dc_bauska_outarr$sd),
#        t(dc_bauska_outarr$mean-dc_bauska_outarr$sd),
#        add=TRUE
#        )

# ## add grey rectangles 
# left <- mybar1 - 0.6
# right <- mybar1 + 0.6
# rect( left[1], ylim[1], right[1], ylim[2], col=rgb(0,0,0,0.1), border=NA )
# rect( left[3], ylim[1], right[3], ylim[2], col=rgb(0,0,0,0.1), border=NA )
# rect( left[5], ylim[1], right[5], ylim[2], col=rgb(0,0,0,0.1), border=NA )

# dev.off()

