## ------------------------------------------------------------------------------
## MAIL FROM THOMAS BAUSKA, 2.6.2015:

## They are all text files with the columns of interest probably being  
## the first three (year, ocean uptake (moles of C per year) and land  
## uptake (moles of C per year)).  Most of the  Monte Carlo simulations  
## have 1000 members indicated by the last four digits of the file name  
## but a few have only 100. The experiment should be listed in a table in  
## the supplemental material.   They are as follows:


## TEST_AgeV2_Mann_XXXX.dat

## 1000 simulations in Mann et al, 2008 NH temperature imposed as SST.   
## This is the core experiment discussed in the text.

## TEST_AgeV2_Marcott_XXXX..dat

## 1000 simulations with Marcott et al., 2012 global temperature imposed as SST

## NOSST_AgeV2_ddec_test_XXXX..dat

## 1000 simulations in with no change in SST

## TEST_AgeV2_Mann_e12_XXXX..dat

## 100 simulations with photosynthetic fractionation set to e -12; Mann  
## SST imposed

## TEST_AgeV2_Mann_e24_XXXX..dat

## 100 simulations with photosynthetic fractionation set to e -24; Mann  
## SST imposed.

## I didn't have time to check all the experiments so hopefully nothing  
## weird shows up.  For example, don't use anything that doesn't say  
## AgeV2 as the ice core age model changed slightly at one opine and I  
## had to redo all the experiments.
## ------------------------------------------------------------------------------
library(Hmisc)

c_molmass <- 12.0107
nruns <- 1000
do.load <- TRUE

## read one file to initialise data frame
names <- c( 'year', 'ocean_uptake', 'land_uptake', 'd13atm', 'd13s', 'd13hr', 'pco2a', 'pco2s', 'hr+npp0', 'hr13+npp130', 'pf13', 'dna13', 'flux13_diseq_sa', 'flux13_diseq_ba', 'flux13_netfl', 'dns*r13a*frac_as', 'nep*r13a*frac_ab', 'pf', 'dna', 'budgetc13' )
dc <- read.table( '/alphadata01/bstocker/data/bauska_ddec/ddec_output/TEST_AgeV2_Mann_0000.dat', col.names=names )
df_land_uptake_bauska     <- data.frame( year=dc$year )
df_cum_land_uptake_bauska <- data.frame( year=dc$year )

if (do.load){
  for (i in 1:nruns){

    numstring <- sprintf( "%04d", i-1)
    colstring <- paste( "r", numstring, sep="" )

    ## read all files into one data frame, TAKE CUMULATIVE SUM FOR VALUES IN df_cum_land_uptake_bauska !!!
    filn <- paste( "/alphadata01/bstocker/data/bauska_ddec/ddec_output/TEST_AgeV2_Mann_",numstring,".dat", sep="" )
    dc <- read.table( filn, col.names=names )
    df_land_uptake_bauska[[ colstring ]]     <- dc$land_uptake
    df_cum_land_uptake_bauska[[ colstring ]] <- cumsum(dc$land_uptake)

  }
}

## get mean and standard deviation and attach to data frame
df_land_uptake_bauska$mean     <- apply( df_land_uptake_bauska[,2:nruns+1], c(1), FUN=mean )
df_land_uptake_bauska$median   <- apply( df_land_uptake_bauska[,2:nruns+1], c(1), FUN=median )
df_land_uptake_bauska$sd       <- apply( df_land_uptake_bauska[,2:nruns+1], c(1), FUN=sd )
df_land_uptake_bauska$q90      <- apply( df_land_uptake_bauska[,2:nruns+1], c(1), FUN=function(x) quantile( x, 0.9 ) )
df_land_uptake_bauska$q10      <- apply( df_land_uptake_bauska[,2:nruns+1], c(1), FUN=function(x) quantile( x, 0.1 ) )

df_cum_land_uptake_bauska$mean     <- apply( df_cum_land_uptake_bauska[,2:nruns+1], c(1), FUN=mean )
df_cum_land_uptake_bauska$median   <- apply( df_cum_land_uptake_bauska[,2:nruns+1], c(1), FUN=median )
df_cum_land_uptake_bauska$sd       <- apply( df_cum_land_uptake_bauska[,2:nruns+1], c(1), FUN=sd )
df_cum_land_uptake_bauska$q90      <- apply( df_cum_land_uptake_bauska[,2:nruns+1], c(1), FUN=function(x) quantile( x, 0.9 ) )
df_cum_land_uptake_bauska$q10      <- apply( df_cum_land_uptake_bauska[,2:nruns+1], c(1), FUN=function(x) quantile( x, 0.1 ) )

## save to file
save( df_land_uptake_bauska, df_cum_land_uptake_bauska, nruns, file='dc_terr_bauska_lastmill.Rdata' )


# ## add mean to plot
# lines( df_cum_land_uptake_bauska$year, df_cum_land_uptake_bauska$mean * c_molmass * 1e-15, col="red", lwd=2 )

# ## add +/- 1-sigma range to plot
# polygon( 
#         c( df_cum_land_uptake_bauska$year, rev(df_cum_land_uptake_bauska$year) ), 
#         c( (df_cum_land_uptake_bauska$mean - df_cum_land_uptake_bauska$sd) * c_molmass * 1e-15, rev( (df_cum_land_uptake_bauska$mean + df_cum_land_uptake_bauska$sd) * c_molmass * 1e-15 ) ), 
#         col=rgb(1,0,0,0.3), border=NA 
#         )

# ## find period margins: local maxima and minima
# period_margins <- 755 # first year in data set
# df_sub <- subset( df_cum_land_uptake_bauska, select=c( year, mean ) )

# ## first maximum
# period <- c(900,1000)
# tmp <- subset( df_cum_land_uptake_bauska, year >= period[1] & year <= period[2], select=c( year, mean ) )
# period_margins <- c( period_margins, tmp$year[ which.max( tmp$mean ) ] )

# ## first minimum
# period <- c(1000,1400)
# tmp <- subset( df_cum_land_uptake_bauska, year >= period[1] & year <= period[2], select=c( year, mean ) )
# period_margins <- c( period_margins, tmp$year[ which.min( tmp$mean ) ] )

# ## second maximum
# period <- c(1400,1500)
# tmp <- subset( df_cum_land_uptake_bauska, year >= period[1] & year <= period[2], select=c( year, mean ) )
# period_margins <- c( period_margins, tmp$year[ which.max( tmp$mean ) ] )

# ## second minimum
# period <- c(1500,1600)
# tmp <- subset( df_cum_land_uptake_bauska, year >= period[1] & year <= period[2], select=c( year, mean ) )
# period_margins <- c( period_margins, tmp$year[ which.min( tmp$mean ) ] )

# ## third maximum
# period <- c(1600,1900)
# tmp <- subset( df_cum_land_uptake_bauska, year >= period[1] & year <= period[2], select=c( year, mean ) )
# period_margins <- c( period_margins, tmp$year[ which.max( tmp$mean ) ] )

# ## end of time series
# period_margins <- c( period_margins, df_cum_land_uptake_bauska$year[length(df_cum_land_uptake_bauska$year)])

# ## Alternativ: fixe Margins
# # period_margins <- c( 760, 960, 1200, 1460, 1530, 1760, 1920 )
# period_margins <- c( 760, 960, 1200, 1500, 1650, 1760, 1920 )

# nper <- length(period_margins)-1

# ## add period margins to plot
# # abline( v=period_margins )
# # print( period_margins )
# rect( period_margins[1], ylim[1], period_margins[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
# rect( period_margins[3], ylim[1], period_margins[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
# rect( period_margins[5], ylim[1], period_margins[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

# for (i in 1:length(period_margins)){
#   text( period_margins[i], 80, as.character( period_margins[i] ), cex=0.7 )
# }

# dev.off()




