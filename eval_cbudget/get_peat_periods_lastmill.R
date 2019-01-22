load( "pt_yu_lastmill.Rdata" )
source('get_statistics_df.R')

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

## peat NEP
filn <- "peatglobnep_trace21_129.dat"
col.names <- c( "year", "peatglobnep_global", "peatglobnep_maskedby_yu", "peatglobnep_maskedby_yu_north" )
trace129$globnep <- read.table( filn, col.names=col.names )


## /////////////////////////////////////////////////////////////////////////////////////////
## LAST MILLENNIUM: Create data frame for evaluating periods
## -----------------------------------------------------------------------------------------
period_margins <- read.csv( 'periods_lastmill.csv' )$period_margins
periodsName <- paste( 
  as.character( period_margins )[1:length(period_margins)-1],
  "-",
  as.character( period_margins )[2:length(period_margins)],
  sep=""
  )
nper <- length(period_margins)-1

## period margins (start and end)
dc_pt_per_mean_exeter <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
dc_pt_per_mean_exeter$iper_start <- rep( NA, nper )
dc_pt_per_mean_exeter$iper_end <- rep( NA, nper )

dc_pt_per_mean_lhnfix <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
dc_pt_per_mean_lhnfix$iper_start <- rep( NA, nper )
dc_pt_per_mean_lhnfix$iper_end <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){
  dc_pt_per_mean_exeter$iper_start[i] <- which.min( abs( dc_pt_per_mean_exeter$per_start[i] - df_cum_pt_mean_exeter$year ) )
  dc_pt_per_mean_exeter$iper_end[i]   <- which.min( abs( dc_pt_per_mean_exeter$per_end[i] - df_cum_pt_mean_exeter$year ) )

  dc_pt_per_mean_lhnfix$iper_start[i] <- which.min( abs( dc_pt_per_mean_lhnfix$per_start[i] - df_cum_pt_mean_lhnfix$year ) )
  dc_pt_per_mean_lhnfix$iper_end[i]   <- which.min( abs( dc_pt_per_mean_lhnfix$per_end[i] - df_cum_pt_mean_lhnfix$year ) )
}


## /////////////////////////////////////////////////////////////////////////////////////////
## Evaluate cumulative balance change in different periods for each run from YML data
## -----------------------------------------------------------------------------------------
for (n in 1:nruns){
  numstring <- sprintf( "%05d", n-1)
  colstring <- paste( "r", numstring, sep="" )
  dc_pt_per_mean_exeter[[ colstring ]]   <- rep( NA, nper )
  dc_pt_per_mean_lhnfix[[ colstring ]]   <- rep( NA, nper )
  for (i in 1:(nper)){
    dc_pt_per_mean_exeter[[ colstring ]][i] <- ( df_cum_pt_mean_exeter[[ colstring ]][dc_pt_per_mean_exeter$iper_end[i]] - df_cum_pt_mean_exeter[[ colstring ]][dc_pt_per_mean_exeter$iper_start[i]] )
    dc_pt_per_mean_lhnfix[[ colstring ]][i] <- ( df_cum_pt_mean_lhnfix[[ colstring ]][dc_pt_per_mean_lhnfix$iper_end[i]] - df_cum_pt_mean_lhnfix[[ colstring ]][dc_pt_per_mean_lhnfix$iper_start[i]] )
  }
}

## add column for period "name"
dc_pt_per_mean_exeter$name <- paste( as.character( dc_pt_per_mean_exeter$per_start ) , "-", as.character( dc_pt_per_mean_exeter$per_end ), sep="" )
dc_pt_per_mean_lhnfix$name <- paste( as.character( dc_pt_per_mean_lhnfix$per_start ) , "-", as.character( dc_pt_per_mean_lhnfix$per_end ), sep="" )

## Get mean and SD of cumulative balance changes in each period
dc_pt_per_mean_exeter <- get_statistics_df( dc_pt_per_mean_exeter, 5:nruns+4 )
dc_pt_per_mean_lhnfix <- get_statistics_df( dc_pt_per_mean_lhnfix, 5:nruns+4 )

## take only subset of dc_pt_per_mean_exeter (drop remaining columns)
dc_pt_mean_sub_exeter <- subset( dc_pt_per_mean_exeter, select=c( name, per_start, per_end, iper_start, iper_end, mean, sd, median, q90, q10 ) ) 
dc_pt_mean_sub_lhnfix <- subset( dc_pt_per_mean_lhnfix, select=c( name, per_start, per_end, iper_start, iper_end, mean, sd, median, q90, q10 ) ) 


## /////////////////////////////////////////////////////////////////////////////////////////
## Evaluate balance change from LPX data
## -----------------------------------------------------------------------------------------
## period margins (start and end)
dc_pt_per_lpx <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
dc_pt_per_lpx$iper_start <- rep( NA, nper )
dc_pt_per_lpx$iper_end <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){

  dc_pt_per_lpx$iper_start[i] <- which.min( abs( dc_pt_per_lpx$per_start[i] - trace129$c$time ) )
  dc_pt_per_lpx$iper_end[i]   <- which.min( abs( dc_pt_per_lpx$per_end[i] - trace129$c$time ) )

  dc_pt_per_lpx$netpeat[i]                 <- (trace129$c$netpeat[dc_pt_per_lpx$iper_end[i]]                 - trace129$c$netpeat[dc_pt_per_lpx$iper_start[i]])/1e15
  dc_pt_per_lpx$peatc_global[i]            <- (trace129$c$peatc_global[dc_pt_per_lpx$iper_end[i]]            - trace129$c$peatc_global[dc_pt_per_lpx$iper_start[i]])/1e15
  dc_pt_per_lpx$peatc_maskedby_yu[i]       <- (trace129$c$peatc_maskedby_yu[dc_pt_per_lpx$iper_end[i]]       - trace129$c$peatc_maskedby_yu[dc_pt_per_lpx$iper_start[i]])/1e15
  dc_pt_per_lpx$peatc_maskedby_yu_north[i] <- (trace129$c$peatc_maskedby_yu_north[dc_pt_per_lpx$iper_end[i]] - trace129$c$peatc_maskedby_yu_north[dc_pt_per_lpx$iper_start[i]])/1e15
  dc_pt_per_lpx$peatc_at_sites[i]          <- (trace129$c$peatc_at_sites[dc_pt_per_lpx$iper_end[i]]          - trace129$c$peatc_at_sites[dc_pt_per_lpx$iper_start[i]])/1e15

  dc_pt_per_lpx$cumpeatnep[i]              <- sum( trace129$globnep$peatglobnep_global[ dc_pt_per_lpx$iper_start[i]:dc_pt_per_lpx$iper_end[i] ]*1e-15 )

}

pt_lpx_outarr <- subset( dc_pt_per_lpx, select=c( netpeat, peatc_global, peatc_maskedby_yu, peatc_maskedby_yu_north, peatc_at_sites ) )
row.names( pt_lpx_outarr ) <- periodsName

save( pt_lpx_outarr, dc_pt_per_lpx, file="dc_peat_lpx_lastmill.Rdata" )


## /////////////////////////////////////////////////////////////////////////////////////////
## Create data frame for plotting bars
## -----------------------------------------------------------------------------------------
## create data frames to be plotted with hist() and errbar()
pt_outarr <- data.frame( 
  median_exeter=dc_pt_mean_sub_exeter$median, median_lhnfix=dc_pt_mean_sub_lhnfix$median, 
  mean_exeter=dc_pt_mean_sub_exeter$mean,     mean_lhnfix=dc_pt_mean_sub_lhnfix$mean, 
  sd_exeter=dc_pt_mean_sub_exeter$sd,         sd_lhnfix=dc_pt_mean_sub_lhnfix$sd, 
  q10_exeter=dc_pt_mean_sub_exeter$q10,       q10_lhnfix=dc_pt_mean_sub_lhnfix$q10, 
  q90_exeter=dc_pt_mean_sub_exeter$q90,       q90_lhnfix=dc_pt_mean_sub_lhnfix$q90, 
  lpx=pt_lpx_outarr$netpeat 
  )
row.names( pt_outarr ) <- periodsName

# pt_outarrerr <- data.frame( dc_pt_exeter_q10=dc_pt_mean_sub_exeter$q10, err_dc_pt_lhnfix_q10=dc_pt_mean_sub_lhnfix$q10,  dc_pt_exeter_q90=dc_pt_mean_sub_exeter$q90, err_dc_pt_lhnfix_q90=dc_pt_mean_sub_lhnfix$q90, err_dc_pt_lpx_global=0 )
# row.names( pt_outarrerr ) <- periodsName

## Save data to Rdata file
save( pt_outarr, dc_pt_per_mean_lhnfix, file="pt_outarr_lastmill.Rdata" )

# ## period margin's corresponding index in full (annual) data frame
# dc_pt_mean_sub$lpx  <- rep( NA, nper )
# for (i in 1:nper){
#   iper_start <- which.min( abs( dc_pt_per_mean_exeter$per_start[i] - trace129$c$year ) )
#   iper_end   <- which.min( abs( dc_pt_per_mean_exeter$per_end[i] - trace129$c$year ) )
#   print(paste("get LPX data from",trace129$c$year[iper_start],"to",trace129$c$year[iper_end]))
#   dc_pt_mean_sub$lpx[i] <- ( trace129$c$peatc_global[iper_end] - trace129$c$peatc_global[iper_start] ) * 1e-15
# }


## /////////////////////////////////////////////////////////////////////////////////////////
## Plot bars for the periods
## -----------------------------------------------------------------------------------------
pdf("cbal_pt_barplot_lastmill_holoLU2.pdf", width=10, height=6 )

  par( las=1 )
  ylim <- c(-2,20)
  # rect( mybar1[1,], rep(ylim[1],4), mybar1[4,], rep(ylim[2],4), col=rgb(0,0,0,0.2), border=NA )

  mybar1 <- barplot( 
                    t( cbind( pt_outarr$mean_lhnfix, pt_outarr$lpx ) ),
                    ylim=ylim,
                    col=c("dodgerblue4","dodgerblue1"),
                    border=TRUE,
                    xlab="period (kyr BP)",
                    beside=TRUE, names.arg=rownames(pt_outarr)
                    )
  abline(0,0)

  errbar(
         mybar1,
         t( cbind( pt_outarr$mean_lhnfix, pt_outarr$lpx ) ),
         t( cbind( pt_outarr$mean_lhnfix+pt_outarr$sd_lhnfix, pt_outarr$lpx ) ),
         t( cbind( pt_outarr$mean_lhnfix-pt_outarr$sd_lhnfix, pt_outarr$lpx ) ),
         add=TRUE
         )

  # ## add grey rectangles 
  # left <- mybar1 - 0.6
  # right <- mybar1 + 0.6
  # rect( left[1], ylim[1], right[1], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  # rect( left[3], ylim[1], right[3], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  # rect( left[5], ylim[1], right[5], ylim[2], col=rgb(0,0,0,0.1), border=NA )

dev.off()

