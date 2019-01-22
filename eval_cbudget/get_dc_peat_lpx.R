## /////////////////////////////////////////////////////////////////////////////////////////
## Reads simulated LPX peat C and creates array to be plotted.
## -----------------------------------------------------------------------------------------
trace129 <- list()

## peat variables averaged across sites
filn <- "peatvars_avg_across_sites_trace21_129.dat"
col.names <- c( "time", "peatc_dens_avg_site", "peatnep_dens_avg_site", "peatnpp_dens_avg_site", "peatrh_dens_avg_site" )
trace129$vars_site <- read.table( filn, col.names=col.names )

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

## FOR EVALUATION OF PERIODS: HOLOCENE
  ## Alternativ: fixe Margins
  periodsBP <- c(-11000,-7000,-5000,-2000,-400)
  periodsAD <- periodsBP + 1950
  periodsName <- c("11-7","7-5","5-2","2-0 (1 - 1560 AD)")
  periodsAD[1] <- -8990 ## 11 kyr BP is to be represented by -8990 AD = 10940 BP
  periodsBP <- periodsAD - 1950
  period_margins <- periodsAD
  nper <- length(period_margins)-1

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

  save( pt_lpx_outarr, file="dc_peat_lpx.Rdata" )

## FOR EVALUATION OF PERIODS: LAST MILLENNIUM
  ## Alternativ: fixe Margins
  period_margins <- c( 760, 960, 1200, 1500, 1650, 1760, 1920 )
  # period_margins <- c( 760, 960, 1200, 1460, 1530, 1760, 1920 )
  nper <- length(period_margins)-1

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

  pt_lpx_lastmill_outarr <- subset( dc_pt_per_lpx, select=c( netpeat, peatc_global, peatc_maskedby_yu, peatc_maskedby_yu_north, peatc_at_sites ) )
  row.names( pt_lpx_lastmill_outarr ) <- paste( as.character( period_margins[1:nper] ), "-", as.character( period_margins[2:(nper+1)] ), sep="" )

  save( pt_lpx_lastmill_outarr, file="dc_peat_lpx_lastmill.Rdata" )




