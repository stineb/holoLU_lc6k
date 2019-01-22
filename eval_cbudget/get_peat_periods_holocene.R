load( "pt_yu_holocene.Rdata" )

## /////////////////////////////////////////////////////////////////////////////////////////
## Reads simulated LPX peat C and creates array to be plotted.
## -----------------------------------------------------------------------------------------
trace129 <- list()

## peat variables averaged across sites
filn <- "peatvars_avg_across_sites_trace21_129.dat"
col.names <- c( "year", "peatc_dens_avg_site", "peatnep_dens_avg_site", "peatnpp_dens_avg_site", "peatrh_dens_avg_site", "peatarea_avg_site" )
trace129$vars_site <- read.table( filn, col.names=col.names )
spar <- 0.7
trace129$vars_site$peatc_dens_avg_site.spl <- smooth.spline( trace129$vars_site$year, trace129$vars_site$peatc_dens_avg_site, spar=spar )$y

## peatland area
filn <- "peatarea_trace21_129.dat"
col.names <- c( "time", "peatarea_global", "peatarea_maskedby_yu", "peatarea_maskedby_yu_north" )
trace129$area <- read.table( filn, col.names=col.names )

## add scaled area 
scale <- 3.7e12 / mean(trace129$area$peatarea_maskedby_yu_north[2190:2204])
trace129$area$peatarea_maskedby_yu_north_scaled <- trace129$area$peatarea_maskedby_yu_north * scale

## peat C
filn <- "peatc_trace21_129.dat"
col.names <- c( "time", "peatc_global", "peatc_maskedby_yu", "peatc_maskedby_yu_north", "netpeat" )
trace129$c <- read.table( filn, col.names=col.names )
trace129$c$peatc_at_sites <- trace129$vars_site$peatc_dens_avg_site * trace129$area$peatarea_maskedby_yu_north_scaled

## /////////////////////////////////////////////////////////////////////////////////////////
## HOLOCENE: Create data frame for evaluating periods
## -----------------------------------------------------------------------------------------
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

## period margins (start and end)
dc_pt_per_mean <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
dc_pt_per_mean$iper_start <- rep( NA, nper )
dc_pt_per_mean$iper_end <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){
  dc_pt_per_mean$iper_start[i] <- which.min( abs( dc_pt_per_mean$per_start[i] - df_cum_pt_mean$year ) )
  dc_pt_per_mean$iper_end[i]   <- which.min( abs( dc_pt_per_mean$per_end[i] - df_cum_pt_mean$year ) )
}


## /////////////////////////////////////////////////////////////////////////////////////////
## Evaluate cumulative balance change in different periods for each run from YML data
## -----------------------------------------------------------------------------------------
for (n in 1:nruns){
  numstring <- sprintf( "%05d", n-1)
  colstring <- paste( "r", numstring, sep="" )
  dc_pt_per_mean[[ colstring ]]   <- rep( NA, nper )
  for (i in 1:nper){
    dc_pt_per_mean[[ colstring ]][i] <- ( df_cum_pt_mean[[ colstring ]][dc_pt_per_mean$iper_end[i]] - df_cum_pt_mean[[ colstring ]][dc_pt_per_mean$iper_start[i]] )
  }
}

## add column for period "name"
dc_pt_per_mean$name <- paste( as.character( dc_pt_per_mean$per_start ) , "-", as.character( dc_pt_per_mean$per_end ), sep="" )

## Get mean and SD of cumulative balance changes in each period
dc_pt_per_mean$mean   <- apply( dc_pt_per_mean[,5:nruns+4], c(1), FUN=mean )
dc_pt_per_mean$sd     <- apply( dc_pt_per_mean[,5:nruns+4], c(1), FUN=sd )
dc_pt_per_mean$median <- apply( dc_pt_per_mean[,5:nruns+4], c(1), FUN=median )
dc_pt_per_mean$q90    <- apply( dc_pt_per_mean[,2:nruns+1], c(1), FUN=function(x) quantile( x, 0.9 ) )
dc_pt_per_mean$q10    <- apply( dc_pt_per_mean[,2:nruns+1], c(1), FUN=function(x) quantile( x, 0.1 ) )

## take only subset of dc_pt_per_mean (drop remaining columns)
dc_pt_mean_sub <- subset( dc_pt_per_mean, select=c( name, per_start, per_end, iper_start, iper_end, mean, sd, median, q90, q10 ) ) 


## /////////////////////////////////////////////////////////////////////////////////////////
## Evaluate balance change from LPX data
## -----------------------------------------------------------------------------------------
## period margins (start and end)
dc_pt_per_lpx <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
dc_pt_per_lpx$iper_start <- rep( NA, nper )
dc_pt_per_lpx$iper_end <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){

  dc_pt_per_lpx$iper_start[i] <- which.min( abs( dc_pt_per_lpx$per_start[i] - trace129$area$time ) )
  dc_pt_per_lpx$iper_end[i]   <- which.min( abs( dc_pt_per_lpx$per_end[i] - trace129$area$time ) )

  dc_pt_per_lpx$netpeat[i]                 <- (trace129$c$netpeat[dc_pt_per_lpx$iper_end[i]]                 - trace129$c$netpeat[dc_pt_per_lpx$iper_start[i]])/1e15
  dc_pt_per_lpx$peatc_global[i]            <- (trace129$c$peatc_global[dc_pt_per_lpx$iper_end[i]]            - trace129$c$peatc_global[dc_pt_per_lpx$iper_start[i]])/1e15
  dc_pt_per_lpx$peatc_maskedby_yu[i]       <- (trace129$c$peatc_maskedby_yu[dc_pt_per_lpx$iper_end[i]]       - trace129$c$peatc_maskedby_yu[dc_pt_per_lpx$iper_start[i]])/1e15
  dc_pt_per_lpx$peatc_maskedby_yu_north[i] <- (trace129$c$peatc_maskedby_yu_north[dc_pt_per_lpx$iper_end[i]] - trace129$c$peatc_maskedby_yu_north[dc_pt_per_lpx$iper_start[i]])/1e15
  dc_pt_per_lpx$peatc_at_sites[i]          <- (trace129$c$peatc_at_sites[dc_pt_per_lpx$iper_end[i]]          - trace129$c$peatc_at_sites[dc_pt_per_lpx$iper_start[i]])/1e15

}

pt_lpx_outarr <- subset( dc_pt_per_lpx, select=c( netpeat, peatc_global, peatc_maskedby_yu, peatc_maskedby_yu_north, peatc_at_sites ) )
row.names( pt_lpx_outarr ) <- periodsName

save( pt_lpx_outarr, dc_pt_per_lpx, file="dc_peat_lpx.Rdata" )


## /////////////////////////////////////////////////////////////////////////////////////////
## Create data frame for plotting bars
## -----------------------------------------------------------------------------------------
## create data frames to be plotted with hist() and errbar()
pt_outarr <- data.frame( 
  mean_yu=dc_pt_mean_sub$mean, median_yu=dc_pt_mean_sub$median, 
  q10_yu=dc_pt_mean_sub$q10, q90_yu=dc_pt_mean_sub$q90, sd_yu=dc_pt_mean_sub$sd,
  lpx_netpeat=pt_lpx_outarr$netpeat, err_lpx=0,
  lpx_peatc_global=pt_lpx_outarr$peatc_global,
  lpx_peatc_maskedby_yu=pt_lpx_outarr$peatc_maskedby_yu
  )
row.names( pt_outarr ) <- periodsName

## Save data to Rdata file
save( pt_outarr, df_pt_mean, dc_pt_per_mean, file="pt_outarr.Rdata" )


# ## period margin's corresponding index in full (annual) data frame
# dc_pt_mean_sub$lpx  <- rep( NA, nper )
# for (i in 1:nper){
#   iper_start <- which.min( abs( dc_pt_per_mean$per_start[i] - trace129$c$year ) )
#   iper_end   <- which.min( abs( dc_pt_per_mean$per_end[i] - trace129$c$year ) )
#   print(paste("get LPX data from",trace129$c$year[iper_start],"to",trace129$c$year[iper_end]))
#   dc_pt_mean_sub$lpx[i] <- ( trace129$c$peatc_global[iper_end] - trace129$c$peatc_global[iper_start] ) * 1e-15
# }


## /////////////////////////////////////////////////////////////////////////////////////////
## Plot bars for the periods
## -----------------------------------------------------------------------------------------
pdf("cbal_pt_barplot_holocene_holoLU2.pdf", width=8, height=6 )

  par( las=1 )
  ylim <- c(-50,300)
  # rect( mybar1[1,], rep(ylim[1],4), mybar1[4,], rep(ylim[2],4), col=rgb(0,0,0,0.2), border=NA )

  mybar1 <- barplot( 
                    t(cbind(pt_outarr$mean_yu, pt_outarr$lpx)),
                    ylim=ylim,
                    col=c("dodgerblue4","dodgerblue2"),
                    border=TRUE,
                    xlab="period (kyr BP)",
                    beside=TRUE
                    )
  abline(0,0)

  errbar(
         mybar1,  
         t(cbind(pt_outarr$mean_yu,pt_outarr$lpx)),
         t( cbind( pt_outarr$mean_yu - pt_outarr$sd_yu, pt_outarr$lpx ) ),
         t( cbind( pt_outarr$mean_yu + pt_outarr$sd_yu, pt_outarr$lpx ) ),
         add=TRUE
         )

  # ## add grey rectangles 
  # left <- mybar1 - 0.6
  # right <- mybar1 + 0.6
  # rect( left[1], ylim[1], right[1], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  # rect( left[3], ylim[1], right[3], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  # rect( left[5], ylim[1], right[5], ylim[2], col=rgb(0,0,0,0.1), border=NA )

dev.off()

