## /////////////////////////////////////////////////////////////////////////////////////////
## Reads the individual time series of total terr. C and peat C., calculates remainder 
## ('nruns' realisations) and statistics across individual realisations of remainder.
## -----------------------------------------------------------------------------------------
load("dc_terr_elsig_holocene.Rdata")
# load("dc_lpx.Rdata")
load("dc_peat_lpx.Rdata")
load("dc_elsig.Rdata")
load("pt_yu_holocene.Rdata")
load("pt_mean_outarrerr.Rdata")

source('get_statistics_df.R')

trace129 <- list()
filn <- "peatc_trace21_129.dat"
col.names <- c( "year", "peatc_global", "peatc_maskedby_yu", "peatc_maskedby_yu_north", "netpeat" )
trace129$c <- read.table( filn, col.names=col.names )

library(Hmisc)

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

  ## Number of random draws from independent time series 
  nruns <- 5000

  print("get remainder for time series...")

  ## Create data frame containing time series for "remainder"
  df_remainder     <- data.frame( year=df_land_uptake$year )
  df_remainder_lpx <- data.frame( year=df_land_uptake$year )

  ## Sample data
  colnr_dc <- sample( 2:2002, nruns, replace=TRUE )
  colnr_pt <- sample( 2:1001, nruns, replace=TRUE )

  iyr_pt_start <- which( df_pt_mean$year == df_remainder$year[1] )
  iyr_pt_end   <- which( df_pt_mean$year == df_remainder$year[length(df_remainder$year)] )

  # iyr_lpx_start <- which( trace129$c$year == df_remainder$year[1] )
  # iyr_lpx_end   <- which( trace129$c$year == df_remainder$year[length(df_remainder$year)] )

  for (irun in 1:nruns){

    numstring <- sprintf( "%05d", irun-1)
    colstring <- paste( "r", numstring, sep="" )

    df_remainder[[ colstring ]]     <- df_land_uptake[,colnr_dc[irun]] - df_pt_mean[iyr_pt_start:iyr_pt_end,colnr_pt[irun]]
    # df_remainder_lpx[[ colstring ]] <- df_land_uptake[,colnr_dc[irun]] - trace129$c$peatglobnep_global[iyr_lpx_start:iyr_lpx_end]/1e15

  }

  ## get statistics
  df_remainder <- get_statistics_df( df_remainder, 2:nruns+1 )

  print("get cumulative sum of remainder...")

  ## get cumulative remainder
  df_cum_remainder <- data.frame( year=df_land_uptake$year )
  for (irun in 1:nruns){

    numstring <- sprintf( "%05d", irun-1)
    colstring <- paste( "r", numstring, sep="" )

    df_cum_remainder[[ colstring ]] <- cumsum( df_remainder[[ colstring ]] )

  }
  
  ## get statistics
  df_cum_remainder <- get_statistics_df( df_cum_remainder, 2:nruns+1 )


  print("get remainder for periods...")

  # # ## test plot: do they add up?
  # # pdf("test.pdf")
  # # plot( df_land_uptake$year, df_land_uptake$mean, type="l" )
  # # lines( df_remainder$year, df_pt_mean$mean[iyr_pt_start:iyr_pt_end] + df_remainder$mean, col="red" )
  # # dev.off()

  ## Create periods-array for "remainder"
  df_per_remainder     <- subset( dc_land_per, select=c( per_start, per_end, iper_start, iper_end ) )
  df_per_remainder_lpx <- df_per_remainder

  colnr_dc <- sample( 5:2005, nruns, replace=TRUE )
  colnr_pt <- sample( 5:1004, nruns, replace=TRUE )

  for (irun in 1:nruns){

    numstring <- sprintf( "%05d", irun-1)
    colstring <- paste( "r", numstring, sep="" )

    df_per_remainder[[ colstring ]]     <- dc_land_per[,colnr_dc[irun]] - dc_pt_per_mean[,colnr_pt[irun]]
    # df_per_remainder_lpx[[ colstring ]] <- dc_land_per[,colnr_dc[irun]] - dc_pt_per_lpx$peatc_global[]
    df_per_remainder_lpx[[ colstring ]] <- dc_land_per[,colnr_dc[irun]] - dc_pt_per_lpx$netpeat[]

  }

  ## add column for period "name"
  df_per_remainder$name <- paste( as.character( df_per_remainder$per_start ) , "-", as.character( df_per_remainder$per_end ), sep="" )
  df_per_remainder_lpx$name <- paste( as.character( df_per_remainder_lpx$per_start ) , "-", as.character( df_per_remainder_lpx$per_end ), sep="" )

  ## get statistics
  df_per_remainder     <- get_statistics_df( df_per_remainder,     2:nruns+1 )
  df_per_remainder_lpx <- get_statistics_df( df_per_remainder_lpx, 2:nruns+1 )

  ## take only subset of df_per_remainder
  dc_remainder_sub     <- subset( df_per_remainder, select=c( name, per_start, per_end, iper_start, iper_end, mean, sd, median, q10, q90 ))
  dc_remainder_sub_lpx <- subset( df_per_remainder_lpx, select=c( name, per_start, per_end, iper_start, iper_end, mean, sd, median, q10, q90 ))

  ## create data frames to be plotted with hist() and errbar()
  dc_remainder_outarr <- data.frame( 
    median_yu=dc_remainder_sub$median, median_lpx=dc_remainder_sub_lpx$median, 
    mean_yu=dc_remainder_sub$mean, mean_lpx=dc_remainder_sub_lpx$mean,
    q10_yu=dc_remainder_sub$q10, q10_lpx=dc_remainder_sub_lpx$q10, 
    q90_yu=dc_remainder_sub$q90, q90_lpx=dc_remainder_sub_lpx$q90, 
    sd_yu=dc_remainder_sub$sd, sd_lpx=dc_remainder_sub_lpx$sd
    )
  row.names( dc_remainder_outarr ) <- periodsName

  # dc_remainder_outarrerr <- data.frame( dc_remainder_q10=dc_remainder_sub$q10, dc_remainder_lpx_q10=dc_remainder_sub_lpx$q10, dc_remainder_q90=dc_remainder_sub$q90, dc_remainder_lpx_q90=dc_remainder_sub_lpx$q90 )
  # row.names( dc_remainder_outarrerr ) <- periodsName

  save( df_remainder, df_cum_remainder, dc_remainder_outarr, file="dc_remainder_holocene.Rdata" )

} 


