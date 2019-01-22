load('dc_terr_elsig_holocene.Rdata')

## re-define period margins
periodsBP <- c(-11000,-9000,-7000,-5000,-3000,-1000)
periodsAD <- periodsBP + 1950
periodsName <- c("11-9","9-7","7-5","5-3","3-1")
periodsBP <- periodsAD - 1950
period_margins <- periodsAD
nper <- length(period_margins)-1

do.load <- FALSE

if (do.load){
  ## Create data frame for evaluating periods
  ## period margins (start and end)
  dc_per <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
  dc_per$iper_start <- rep( NA, nper )
  dc_per$iper_end <- rep( NA, nper )

  ## period margin's corresponding index in full (annual) data frame
  for (i in 1:nper){
    dc_per$iper_start[i] <- which.min( abs( dc_per$per_start[i] - df_cum_land_uptake$year ) )
    dc_per$iper_end[i] <- which.min( abs( dc_per$per_end[i] - df_cum_land_uptake$year ) )
  }

  ## Evaluate cumulative balance change in different periods for each run
  for (n in 1:nruns){

    numstring <- sprintf( "%05d", n-1)
    colstring <- paste( "r", numstring, sep="" )

    dc_per[[ colstring ]] <- rep( NA, nper )

    for (i in 1:nper){

      dc_per[[ colstring ]][i] <- df_cum_land_uptake[[ colstring ]][dc_per$iper_end[i]] - df_cum_land_uptake[[ colstring ]][dc_per$iper_start[i]]

    }

  }

  ## add column for period "name"
  dc_per$name <- paste( as.character( dc_per$per_start ) , "-", as.character( dc_per$per_end ), sep="" )

  ## Get mean and SD of cumulative balance changes in each period
  dc_per$mean   <- apply( dc_per[,5:nruns+4], c(1), FUN=mean )
  dc_per$sd     <- apply( dc_per[,5:nruns+4], c(1), FUN=sd )
  dc_per$median <- apply( dc_per[,5:nruns+4], c(1), FUN=median )
  dc_per$q90    <- apply( dc_per[,5:nruns+4], c(1), FUN=function(x) quantile( x, 0.9 ) )
  dc_per$q10    <- apply( dc_per[,5:nruns+4], c(1), FUN=function(x) quantile( x, 0.1 ) )


  ## take only subset of dc_per
  dc_sub <- subset( dc_per, select=c( name, per_start, per_end, iper_start, iper_end, mean, sd, median, q90, q10 ))
}

## create data frames to be plotted with hist() and errbar()
dc_elsig_outarr <- data.frame( median=dc_sub$median, mean=dc_sub$mean, q10=dc_sub$q10, q90=dc_sub$q90, sd=dc_sub$sd )
row.names( dc_elsig_outarr ) <- periodsName

## Plot bars for the periods
pdf("/alphadata01/bstocker/holoLU2/cbal_barplot_holocene_holoLU2.pdf", width=8, height=6 )
  par( las=1 )
  ylim <- c(-60,300)
  # rect( mybar1[1,], rep(ylim[1],4), mybar1[4,], rep(ylim[2],4), col=rgb(0,0,0,0.2), border=NA )

  mybar1 <- barplot( 
                    t(dc_elsig_outarr$mean),
                    ylim=ylim,
                    col=c("green3"),
                    border=TRUE,
                    xlab="period [kyr BP]"
                    )
  abline(0,0)

  errbar(
         mybar1,
         t(dc_elsig_outarr$mean),
         t(dc_elsig_outarr$mean-dc_elsig_outarr$sd),
         t(dc_elsig_outarr$mean+dc_elsig_outarr$sd),
         add=TRUE
         )

  ## add grey rectangles 
  left <- mybar1 - 0.6
  right <- mybar1 + 0.6
  rect( left[1], ylim[1], right[1], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[3], ylim[1], right[3], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[5], ylim[1], right[5], ylim[2], col=rgb(0,0,0,0.1), border=NA )

dev.off()

save( df_land_uptake, dc_elsig_outarr, dc_elsig_outarrerr, dc_per, file="dc_elsig.Rdata" )
