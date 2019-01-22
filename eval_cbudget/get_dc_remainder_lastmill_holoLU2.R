load("dc_terr_bauska_lastmill.Rdata")
load("dc_peat_lpx_lastmill.Rdata")
load("pt_outarr_lastmill.Rdata")

source('get_statistics_df.R')

library(Hmisc)

do.load <- TRUE

## LAST MILLENNIUM: Create data frame for evaluating periods
# ## period margins (start and end)
# period_margins <- c( 760, 960, 1200, 1500, 1650, 1760, 1920 ) ## use this
# # period_margins <- c( 900, 1100, 1300, 1500, 1700, 1900 )
# periodsName <- paste( as.character(period_margins[1:6]), "-", as.character(period_margins[2:7]), sep="" )
# nper <- length(period_margins)-1
period_margins <- read.csv( 'periods_lastmill.csv' )$period_margins
periodsName <- paste( 
  as.character( period_margins )[1:length(period_margins)-1],
  "-",
  as.character( period_margins )[2:length(period_margins)],
  sep=""
  )
nper <- length(period_margins)-1

if (do.load){
  ## --------------------------------------------------------------------------------
  ## REMAINDER PER PERIOD
  ## --------------------------------------------------------------------------------
  ## Number of random draws from independent time series 
  nruns <- 5000

  ## Create periods-array for "remainder"
  df_per_remainder_lastmill     <- subset( dc_per_lastmill, select=c( per_start, per_end ) )
  df_per_remainder_lastmill_lpx <- df_per_remainder_lastmill

  colnr_dc <- sample( 5:1004, nruns, replace=TRUE )
  colnr_pt <- sample( 5:1004, nruns, replace=TRUE )

  for (irun in 1:nruns){

    numstring <- sprintf( "%05d", irun-1)
    colstring <- paste( "r", numstring, sep="" )

    df_per_remainder_lastmill[[ colstring ]]     <- dc_per_lastmill[,colnr_dc[irun]] * 1e-14 - dc_pt_per_mean_lhnfix[,colnr_pt[irun]]
    df_per_remainder_lastmill_lpx[[ colstring ]] <- dc_per_lastmill[,colnr_dc[irun]] * 1e-14 - dc_pt_per_lpx$netpeat[]

  }

  ## add column for period "name"
  df_per_remainder_lastmill$name <- paste( as.character( df_per_remainder_lastmill$per_start ) , "-", as.character( df_per_remainder_lastmill$per_end ), sep="" )
  df_per_remainder_lastmill_lpx$name <- paste( as.character( df_per_remainder_lastmill_lpx$per_start ) , "-", as.character( df_per_remainder_lastmill_lpx$per_end ), sep="" )

  ## get mean and standard deviation
  df_per_remainder_lastmill <- get_statistics_df( df_per_remainder_lastmill, 3:nruns+2 )
  df_per_remainder_lastmill_lpx <- get_statistics_df( df_per_remainder_lastmill_lpx, 3:nruns+2 )

  ## take only subset of df_per_remainder_lastmill
  dc_remainder_sub <- subset( df_per_remainder_lastmill, select=c( name, per_start, per_end, mean, sd, median, q10, q90 ))
  dc_remainder_sub_lpx <- subset( df_per_remainder_lastmill_lpx, select=c( name, per_start, per_end, mean, sd, median, q10, q90 ))

  ## create data frames to be plotted with hist() and errbar()
  dc_remainder_outarr <- data.frame( 
    mean=dc_remainder_sub$mean,     mean_lpx=dc_remainder_sub_lpx$mean,
    median=dc_remainder_sub$median, median_lpx=dc_remainder_sub_lpx$median,
    sd=dc_remainder_sub$sd,   sd_lpx=dc_remainder_sub_lpx$sd,
    q10=dc_remainder_sub$q10, q10_lpx=dc_remainder_sub_lpx$q10,
    q90=dc_remainder_sub$q90, q90_lpx=dc_remainder_sub_lpx$q90 
    )
  row.names( dc_remainder_outarr ) <- df_per_remainder_lastmill$name

  save( dc_remainder_outarr, file="dc_remainder_lastmill.Rdata" )

}


