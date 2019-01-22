load('dc_terr_elsig_holocene.Rdata')
source('get_statistics_df.R')

## re-define period margins
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

do.load <- TRUE

if (do.load){
  ## Create data frame for evaluating periods
  ## period margins (start and end)
  dc_land_per <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
  dc_land_per$iper_start <- rep( NA, nper )
  dc_land_per$iper_end <- rep( NA, nper )

  dc_ocean_per <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
  dc_ocean_per$iper_start <- rep( NA, nper )
  dc_ocean_per$iper_end <- rep( NA, nper )

  ## period margin's corresponding index in full (annual) data frame
  for (i in 1:nper){
    dc_land_per$iper_start[i] <- which.min( abs( dc_land_per$per_start[i] - df_cum_land_uptake$year ) )
    dc_land_per$iper_end[i]   <- which.min( abs( dc_land_per$per_end[i] - df_cum_land_uptake$year ) )

    dc_ocean_per$iper_start[i] <- which.min( abs( dc_ocean_per$per_start[i] - df_cum_ocean_uptake$year ) )
    dc_ocean_per$iper_end[i]   <- which.min( abs( dc_ocean_per$per_end[i] - df_cum_ocean_uptake$year ) )
  }

  ## Evaluate cumulative balance change in different periods for each run
  for (n in 1:nruns){

    numstring <- sprintf( "%05d", n-1)
    colstring <- paste( "r", numstring, sep="" )

    dc_land_per[[ colstring ]] <- rep( NA, nper )
    dc_ocean_per[[ colstring ]] <- rep( NA, nper )

    for (i in 1:nper){

      dc_land_per[[ colstring ]][i] <- df_cum_land_uptake[[ colstring ]][dc_land_per$iper_end[i]] - df_cum_land_uptake[[ colstring ]][dc_land_per$iper_start[i]]
      dc_ocean_per[[ colstring ]][i] <- df_cum_ocean_uptake[[ colstring ]][dc_ocean_per$iper_end[i]] - df_cum_ocean_uptake[[ colstring ]][dc_ocean_per$iper_start[i]]

    }

  }

  ## add column for period "name"
  dc_land_per$name <- paste( as.character( dc_land_per$per_start ) , "-", as.character( dc_land_per$per_end ), sep="" )
  dc_ocean_per$name <- paste( as.character( dc_ocean_per$per_start ) , "-", as.character( dc_ocean_per$per_end ), sep="" )

  ## Get mean and SD of cumulative balance changes in each period
  dc_land_per <- get_statistics_df( dc_land_per, 5:nruns+4 )
  dc_ocean_per <- get_statistics_df( dc_ocean_per, 5:nruns+4 )

}

## create data frames to be plotted with hist() and errbar()
dc_land_outarr <- data.frame( median=dc_land_per$median, mean=dc_land_per$mean, q10=dc_land_per$q10, q90=dc_land_per$q90, sd=dc_land_per$sd )
row.names( dc_land_outarr ) <- periodsName

dc_ocean_outarr <- data.frame( median=dc_ocean_per$median, mean=dc_ocean_per$mean, q10=dc_ocean_per$q10, q90=dc_ocean_per$q90, sd=dc_ocean_per$sd )
row.names( dc_ocean_outarr ) <- periodsName

## Plot bars for the periods
pdf("cbal_land_barplot_holocene_holoLU2.pdf", width=8, height=6 )
  par( las=1 )
  ylim <- c(-60,200)
  # rect( mybar1[1,], rep(ylim[1],4), mybar1[4,], rep(ylim[2],4), col=rgb(0,0,0,0.2), border=NA )

  mybar1 <- barplot( 
                    t(dc_land_outarr$mean),
                    ylim=ylim,
                    col=c("springgreen"),
                    border=TRUE,
                    xlab="period [kyr BP]",
                    names.arg=rownames(dc_land_outarr)
                    )
  abline(0,0)

  errbar(
         mybar1,
         t(dc_land_outarr$mean),
         t(dc_land_outarr$mean-dc_land_outarr$sd),
         t(dc_land_outarr$mean+dc_land_outarr$sd),
         add=TRUE
         )

  ## add grey rectangles 
  left <- mybar1 - 0.6
  right <- mybar1 + 0.6
  rect( left[1], ylim[1], right[1], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[3], ylim[1], right[3], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[5], ylim[1], right[5], ylim[2], col=rgb(0,0,0,0.1), border=NA )

dev.off()

pdf("cbal_ocean_barplot_holocene_holoLU2.pdf", width=8, height=6 )
  par( las=1 )
  ylim <- c(-200,50)
  # rect( mybar1[1,], rep(ylim[1],4), mybar1[4,], rep(ylim[2],4), col=rgb(0,0,0,0.2), border=NA )

  mybar1 <- barplot( 
                    t(dc_ocean_outarr$mean),
                    ylim=ylim,
                    col=c("dodgerblue"),
                    border=TRUE,
                    xlab="period (ka BP)",
                    names.arg=rownames(dc_ocean_outarr)
                    )

  axis(2,lwd=1.5,at=seq(ylim[1],ylim[2],by=50));  axis(2,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
  axis(4,lwd=1.5,labels=F,at=seq(ylim[1],ylim[2],by=50));  axis(4,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
  abline(0,0)

  errbar(
         mybar1,
         t(dc_ocean_outarr$mean),
         t(dc_ocean_outarr$mean-dc_ocean_outarr$sd),
         t(dc_ocean_outarr$mean+dc_ocean_outarr$sd),
         add=TRUE
         )

  ## add grey rectangles 
  left <- mybar1 - 0.6
  right <- mybar1 + 0.6
  rect( left[1], ylim[1], right[1], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[3], ylim[1], right[3], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[5], ylim[1], right[5], ylim[2], col=rgb(0,0,0,0.1), border=NA )

dev.off()

save( df_ocean_uptake, dc_ocean_outarr, dc_ocean_per, df_land_uptake, dc_land_outarr, dc_land_per, file="dc_elsig.Rdata" )
