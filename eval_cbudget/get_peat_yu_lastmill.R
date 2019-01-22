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
  load('/alphadata01/bstocker/data/yu_data/NCB_16apr2016/NCB_ExeterDataset.RData') ## tmp2 (this is what I used since June 2015)
  tmp2 <- tmp
  # load('/alphadata01/bstocker/data/yu_data/NCB_16apr2016/NCB_LehighDataset.RData') ## tmp (this is what Charly sent me on 16.4.2016)
  # load('/alphadata01/bstocker/data/yu_data/NCB/MC_NCB_Mean.rdata') ## tmp (this is what I used since June 2015)
  load('/alphadata01/bstocker/data/yu_data/NCB_04mai2016/NCB_LehighDataset_0.000112.RData') ## tmp

  ## number of individual runs
  nruns <- dim( tmp2$NCB )[2]

  ##--------------------------------------------------------------------------
  ## STANDARDISE DATA
  ##--------------------------------------------------------------------------
  ## Remove last point in dataframes (is NA)
  tmp$NCB  <- tmp$NCB[1:dim(tmp$NCB)[1]-1,]
  tmp$age  <- tmp$age[1:length(tmp$age)-1]

  tmp2$NCB  <- tmp2$NCB[1:dim(tmp2$NCB)[1]-1,]
  tmp2$age  <- tmp2$age[1:length(tmp2$age)-1]

  ## Create data frame
  df_pt_mean_lehigh     <- data.frame( age=rev(tmp$age) )
  df_cum_pt_mean_lehigh <- data.frame( age=rev(tmp$age) )

  df_pt_mean_exeter     <- data.frame( age=rev(tmp2$age) )
  df_cum_pt_mean_exeter <- data.frame( age=rev(tmp2$age) )

  ## add column for year AD (center to mid-decade: '+ 5' )
  df_pt_mean_exeter$year     <- 1950 - df_pt_mean_exeter$age + 5
  df_cum_pt_mean_exeter$year <- 1950 - df_cum_pt_mean_exeter$age + 5

  df_pt_mean_lehigh$year     <- 1950 - df_pt_mean_lehigh$age + 5
  df_cum_pt_mean_lehigh$year <- 1950 - df_cum_pt_mean_lehigh$age + 5

  iyr_lehigh <- which.min( abs(df_pt_mean_lehigh$year-1200) )
  iyr_exeter <- which.min( abs(df_pt_mean_exeter$year-1200) )

  ## Add column names
  nrunvec <- seq(0, nruns-1, 1)
  namsvec <- sprintf( "%05d", nrunvec )
  colsvec <- paste( "r", namsvec, sep="" )

  for ( i in 1:nruns) {
    ## convert units
    df_pt_mean_lehigh[[ colsvec[i] ]] <- rev(  tmp$NCB[,i] * dstep / 1000 )
    df_pt_mean_exeter[[ colsvec[i] ]] <- rev( tmp2$NCB[,i] * dstep / 1000 )

    # ## add noise: uncertainty of total area
    # df_pt_mean_lehigh[[ colsvec[i] ]] <- df_pt_mean_lehigh[[ colsvec[i] ]] * rnorm( 1, 3.7, 0.3 ) / 3.7
    # df_pt_mean_exeter[[ colsvec[i] ]] <- df_pt_mean_exeter[[ colsvec[i] ]] * rnorm( 1, 3.7, 0.3 ) / 3.7

  }

  ##--------------------------------------------------------------------------
  ## Get statistics
  ##--------------------------------------------------------------------------
  df_pt_mean_lehigh <- get_statistics_df( df_pt_mean_lehigh, 3:nruns+2 )
  df_pt_mean_exeter <- get_statistics_df( df_pt_mean_exeter, 3:nruns+2 )

  ##--------------------------------------------------------------------------
  ## ALTERNATIVE 1: CRUDE FIX: hold mean constant after 1200 and add iav of previous 10 years
  ##--------------------------------------------------------------------------
  df_pt_mean_lhnfix                             <- df_pt_mean_lehigh
  iav_lehigh                                    <- df_pt_mean_lehigh[,3:(nruns+2)] - df_pt_mean_lehigh$mean
  len                                           <- dim(df_pt_mean_lehigh)[1]
  idxs                                          <- c( rep( (iyr_lehigh-9):iyr_lehigh, 7 ), ((iyr_lehigh-5):iyr_lehigh) )
  df_pt_mean_lhnfix[iyr_lehigh:len,3:(nruns+2)] <- df_pt_mean_lehigh$mean[iyr_lehigh] + iav_lehigh[idxs,]
  df_pt_mean_lhnfix <- get_statistics_df( df_pt_mean_lhnfix, 3:nruns+2 )

  ##--------------------------------------------------------------------------
  ## ALTERNATIVE 2: LEHIGH + EXETER-TREND (RELATIVE CHANGE) AFTER 1200 AD
  ##--------------------------------------------------------------------------
  df_pt_mean_lhxtrd <- df_pt_mean_lehigh[,1:(nruns+2)]
  spl <- smooth.spline( df_pt_mean_exeter$year, df_pt_mean_exeter$mean, spar=0.05 )$y
  rel <- spl / mean( spl[ which(df_pt_mean_exeter$year>=1170 & df_pt_mean_exeter$year<=1200 ) ] )
  for (colidx in 3:nruns+2){
    df_pt_mean_lhxtrd[which(df_pt_mean_lhxtrd$year>1200),colidx] <- df_pt_mean_lhxtrd[which( df_pt_mean_lhxtrd$year==1205 ),colidx] * rel[which(df_pt_mean_exeter$year>1200)]
  }
  df_pt_mean_lhxtrd <- get_statistics_df( df_pt_mean_lhxtrd, 3:nruns+2 )

  ##--------------------------------------------------------------------------
  ## Get cumulative peat C and their statistics
  ##--------------------------------------------------------------------------
  df_cum_pt_mean_lehigh <- df_pt_mean_lehigh
  df_cum_pt_mean_exeter <- df_pt_mean_exeter
  df_cum_pt_mean_lhnfix <- df_pt_mean_lhnfix
  df_cum_pt_mean_lhxtrd <- df_pt_mean_lhxtrd
  
  for ( i in 1:nruns) {
    # ## add noise: uncertainty of total area
    # df_pt_mean_lehigh[[ colsvec[i] ]] <- df_pt_mean_lehigh[[ colsvec[i] ]] * rnorm( 1, 3.7, 0.3 ) / 3.7
    # df_pt_mean_exeter[[ colsvec[i] ]] <- df_pt_mean_exeter[[ colsvec[i] ]] * rnorm( 1, 3.7, 0.3 ) / 3.7

    ## Cumulative changes of each MC time series
    df_cum_pt_mean_lehigh[[ colsvec[i] ]] <- cumsum( df_pt_mean_lehigh[[ colsvec[i] ]] )
    df_cum_pt_mean_exeter[[ colsvec[i] ]] <- cumsum( df_pt_mean_exeter[[ colsvec[i] ]] )
    df_cum_pt_mean_lhnfix[[ colsvec[i] ]] <- cumsum( df_pt_mean_lhnfix[[ colsvec[i] ]] )
    df_cum_pt_mean_lhxtrd[[ colsvec[i] ]] <- cumsum( df_pt_mean_lhxtrd[[ colsvec[i] ]] )
  }

  df_cum_pt_mean_lehigh <- get_statistics_df( df_cum_pt_mean_lehigh, 3:nruns+2 )
  df_cum_pt_mean_exeter <- get_statistics_df( df_cum_pt_mean_exeter, 3:nruns+2 )
  df_cum_pt_mean_lhnfix <- get_statistics_df( df_cum_pt_mean_lhnfix, 3:nruns+2 )
  df_cum_pt_mean_lhxtrd <- get_statistics_df( df_cum_pt_mean_lhxtrd, 3:nruns+2 )

}

pdf( "test.pdf" )
plot( df_pt_mean_lehigh$year, df_pt_mean_lehigh$mean / dstep, type="l", xlim=c(750,2000), ylim=c(0,0.2) )
lines( smooth.spline( df_pt_mean_exeter$year, df_pt_mean_exeter$mean / dstep, spar=0.05 ), col="red" )
lines( df_pt_mean_lhxtrd$year, df_pt_mean_lhxtrd$mean / dstep, lty=2 )
dev.off()

## Save data to Rdata file
save( 
  df_pt_mean_exeter, df_cum_pt_mean_exeter, 
  df_pt_mean_lehigh, df_cum_pt_mean_lehigh, 
  df_pt_mean_lhnfix, df_cum_pt_mean_lhnfix, 
  df_pt_mean_lhxtrd, df_cum_pt_mean_lhxtrd, 
  nruns, dstep, 
  file="pt_yu_lastmill.Rdata" 
  )

