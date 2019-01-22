## /////////////////////////////////////////////////////////////////////////////////////////
## Reads original NCB data for each of the 1000 reconstructions, and evaluates peat C 
## balance in different periods of the Holocene, creating one column of the 
## Holocene-period balance arrays 'pt_mean_outarr', 'pt_median_outarr', 'pt_mean_outarrerr', 
## and 'pt_median_outarrerr'.
## -----------------------------------------------------------------------------------------
library(Hmisc)

source('get_statistics_df.R')

do.load <- TRUE

dstep <- 10

charly <- read.csv("/alphadata01/bstocker/data/yu_data/NCB/NCB_65sites_mean.csv")
df_charly_pt_mean <- data.frame( age=rev(charly$age) )
df_charly_pt_mean$year <- 1950 - df_charly_pt_mean$age + 5
df_charly_pt_mean$median <- rev(charly$NCB.50.) * dstep / 1000
df_charly_pt_mean$cumsum_median <- cumsum( df_charly_pt_mean$median )

if (do.load){

  ## load Charly's raw data: contains list ("age" "NCB" "NCP" "NCR" "NCU"), where NCB (NCP, ...) are arrays of c(1377,1000) containing the 1000 iterations 
  # load('/alphadata01/bstocker/data/yu_data/NCB/MC_NCB_Mean.rdata') ## tmp (this is what I used since June 2015)
  # load('/alphadata01/bstocker/data/yu_data/NCB_16apr2016/NCB_LehighDataset.RData') ## tmp (this is what Charly sent me on 16.4.2016)
  load('/alphadata01/bstocker/data/yu_data/NCB_04mai2016/NCB_LehighDataset_0.000112.RData') ## tmp

  ## number of individual runs
  nruns <- dim( tmp$NCB )[2]

  ## Remove last point in dataframe (is NA)
  tmp$NCB  <- tmp$NCB[1:dim(tmp$NCB)[1]-1,]
  tmp$age  <- tmp$age[1:length(tmp$age)-1]

  ## Create data frame
  df_pt_mean     <- data.frame( age=rev(tmp$age) )
  df_cum_pt_mean <- data.frame( age=rev(tmp$age) )

  ## Add column names
  nrunvec <- seq(0, nruns-1, 1)
  namsvec <- sprintf( "%05d", nrunvec )
  colsvec <- paste( "r", namsvec, sep="" )

  for ( i in 1:nruns) {
    ## convert units
    df_pt_mean[[ colsvec[i] ]] <- rev( tmp$NCB[,i] * dstep / 1000 )

    # ## add noise: uncertainty of total area (not included in "june 2015" data)
    # df_pt_mean[[ colsvec[i] ]] <- df_pt_mean[[ colsvec[i] ]] * rnorm( 1, 3.7, 0.3 ) / 3.7

    ## Cumulative changes of each MC time series
    df_cum_pt_mean[[ colsvec[i] ]] <- cumsum( df_pt_mean[[ colsvec[i] ]] )

  }

  ## add column for year AD (center to mid-decade: '+ 5' )
  df_pt_mean$year     <- 1950 - df_pt_mean$age + 5
  df_cum_pt_mean$year <- 1950 - df_cum_pt_mean$age + 5

  ## get mean and standard deviation at each time step and attach to data frame
  df_pt_mean     <- get_statistics_df( df_pt_mean, 2:nruns+1 )
  df_cum_pt_mean <- get_statistics_df( df_cum_pt_mean, 2:nruns+1 )

}

## Save data to Rdata file
save( df_pt_mean, df_cum_pt_mean, nruns, file="pt_yu_holocene.Rdata" )



