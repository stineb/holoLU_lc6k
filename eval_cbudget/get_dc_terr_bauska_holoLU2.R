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

## read one file to initialise data frame
names <- c( 'year', 'ocean_uptake', 'land_uptake', 'd13atm', 'd13s', 'd13hr', 'pco2a', 'pco2s', 'hr+npp0', 'hr13+npp130', 'pf13', 'dna13', 'flux13_diseq_sa', 'flux13_diseq_ba', 'flux13_netfl', 'dns*r13a*frac_as', 'nep*r13a*frac_ab', 'pf', 'dna', 'budgetc13' )
dc <- read.table( '/alphadata01/bstocker/data/bauska_ddec/ddec_output/TEST_AgeV2_Mann_0000.dat', col.names=names )
df_land_uptake_bauska <- data.frame( year=dc$year )

## open plot
ylim <- c(-100,100)

# magn <- 6
# ncols <- 1
# nrows <- 2
# widths <- rep(1.4*magn,ncols)
# heights <- rep(magn,nrows)
# heights[2] <- 0.4*heights[1]
# order <- matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE)

# pdf( "dC_terr_bauska_holoLU.pdf", width=sum(widths), height=sum(heights) )
pdf( "dC_terr_bauska_holoLU.pdf", width=8, height=6 )
par( las=1 )

plot( range(df_land_uptake_bauska$year), ylim, type="n", ylab="cumulative terrestrial C balance [PgC]", xlab="year AD")

for (i in 1:nruns){

  numstring <- sprintf( "%04d", i-1)
  colstring <- paste( "r", numstring, sep="" )

  ## read all files into one data frame, TAKE CUMULATIVE SUM FOR VALUES IN df_land_uptake_bauska !!!
  filn <- paste( "/alphadata01/bstocker/data/bauska_ddec/ddec_output/TEST_AgeV2_Mann_",numstring,".dat", sep="" )
  dc <- read.table( filn, col.names=names )
  df_land_uptake_bauska[[ colstring ]] <- cumsum(dc$land_uptake)

  ## plot this time series of land c uptake into the previously opened plot
  lines( df_land_uptake_bauska$year, df_land_uptake_bauska[[ colstring ]] * c_molmass * 1e-15, col=rgb(0,0,0,0.05) )

}

## get mean and standard deviation and attach to data frame
df_land_uptake_bauska$mean <- apply( df_land_uptake_bauska[,2:nruns+1], c(1), FUN=mean )
df_land_uptake_bauska$sd   <- apply( df_land_uptake_bauska[,2:nruns+1], c(1), FUN=sd )

## add mean to plot
lines( df_land_uptake_bauska$year, df_land_uptake_bauska$mean * c_molmass * 1e-15, col="red", lwd=2 )

## add +/- 1-sigma range to plot
polygon( 
        c( df_land_uptake_bauska$year, rev(df_land_uptake_bauska$year) ), 
        c( (df_land_uptake_bauska$mean - df_land_uptake_bauska$sd) * c_molmass * 1e-15, rev( (df_land_uptake_bauska$mean + df_land_uptake_bauska$sd) * c_molmass * 1e-15 ) ), 
        col=rgb(1,0,0,0.3), border=NA 
        )

## find period margins: local maxima and minima
period_margins <- 755 # first year in data set
df_sub <- subset( df_land_uptake_bauska, select=c( year, mean ) )

## first maximum
period <- c(900,1000)
tmp <- subset( df_land_uptake_bauska, year >= period[1] & year <= period[2], select=c( year, mean ) )
period_margins <- c( period_margins, tmp$year[ which.max( tmp$mean ) ] )

## first minimum
period <- c(1000,1400)
tmp <- subset( df_land_uptake_bauska, year >= period[1] & year <= period[2], select=c( year, mean ) )
period_margins <- c( period_margins, tmp$year[ which.min( tmp$mean ) ] )

## second maximum
period <- c(1400,1500)
tmp <- subset( df_land_uptake_bauska, year >= period[1] & year <= period[2], select=c( year, mean ) )
period_margins <- c( period_margins, tmp$year[ which.max( tmp$mean ) ] )

## second minimum
period <- c(1500,1600)
tmp <- subset( df_land_uptake_bauska, year >= period[1] & year <= period[2], select=c( year, mean ) )
period_margins <- c( period_margins, tmp$year[ which.min( tmp$mean ) ] )

## third maximum
period <- c(1600,1900)
tmp <- subset( df_land_uptake_bauska, year >= period[1] & year <= period[2], select=c( year, mean ) )
period_margins <- c( period_margins, tmp$year[ which.max( tmp$mean ) ] )

## end of time series
period_margins <- c( period_margins, df_land_uptake_bauska$year[length(df_land_uptake_bauska$year)])

## Alternativ: fixe Margins
period_margins <- c( 760, 960, 1200, 1460, 1530, 1760, 1920 )

nper <- length(period_margins)-1

## add period margins to plot
# abline( v=period_margins )
# print( period_margins )
rect( period_margins[1], ylim[1], period_margins[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
rect( period_margins[3], ylim[1], period_margins[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
rect( period_margins[5], ylim[1], period_margins[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

for (i in 1:length(period_margins)){
  text( period_margins[i], 80, as.character( period_margins[i] ), cex=0.7 )
}

dev.off()


## Create data frame for evaluating periods
## period margins (start and end)
dc_per_bauska <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
dc_per_bauska$iper_start <- rep( NA, nper )
dc_per_bauska$iper_end <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){
  dc_per_bauska$iper_start[i] <- which.min( abs( dc_per_bauska$per_start[i] - df_land_uptake_bauska$year ) )
  dc_per_bauska$iper_end[i] <- which.min( abs( dc_per_bauska$per_end[i] - df_land_uptake_bauska$year ) )
}

## Evaluate cumulative balance change in different periods for each run
for (n in 1:nruns){

  numstring <- sprintf( "%04d", n-1)
  colstring <- paste( "r", numstring, sep="" )

  dc_per_bauska[[ colstring ]] <- rep( NA, nper )

  for (i in 1:nper){

    dc_per_bauska[[ colstring ]][i] <- df_land_uptake_bauska[[ colstring ]][dc_per_bauska$iper_end[i]] - df_land_uptake_bauska[[ colstring ]][dc_per_bauska$iper_start[i]]

  }

}

## add column for period "name"
dc_per_bauska$name <- paste( as.character( dc_per_bauska$per_start ) , "-", as.character( dc_per_bauska$per_end ), sep="" )

## Get mean and SD of cumulative balance changes in each period
dc_per_bauska$mean <- apply( dc_per_bauska[,5:nruns+4], c(1), FUN=mean )
dc_per_bauska$sd   <- apply( dc_per_bauska[,5:nruns+4], c(1), FUN=sd )

## take only subset of dc_per_bauska
dc_sub <- subset( dc_per_bauska, select=c( name, per_start, per_end, iper_start, iper_end, mean, sd ))

## create data frames to be plotted with hist() and errbar()
dc_bauska_outarr <- data.frame( dc_terr=dc_sub$mean*1e-14 )
row.names( dc_bauska_outarr ) <- dc_sub$name

dc_bauska_outarrerr <- data.frame( err_dc_terr=dc_sub$sd*1e-14 )
row.names( dc_bauska_outarrerr ) <- dc_sub$name

## Plot bars for the periods
pdf("/alphadata01/bstocker/holoLU2/cbal_barplot_lastmill_holoLU2.pdf", width=8, height=6 )
par( las=1 )
ylim <- c(-100,100)

mybar1 <- barplot( 
                  t(dc_bauska_outarr),
                  ylim=ylim,
                  col=c("green3"),
                  border=TRUE,
                  xlab="period [year AD]"
                  )
abline(0,0)

errbar(
       mybar1,
       t(dc_bauska_outarr),
       t(dc_bauska_outarr+dc_bauska_outarrerr),
       t(dc_bauska_outarr-dc_bauska_outarrerr),
       add=TRUE
       )

## add grey rectangles 
left <- mybar1 - 0.6
right <- mybar1 + 0.6
rect( left[1], ylim[1], right[1], ylim[2], col=rgb(0,0,0,0.1), border=NA )
rect( left[3], ylim[1], right[3], ylim[2], col=rgb(0,0,0,0.1), border=NA )
rect( left[5], ylim[1], right[5], ylim[2], col=rgb(0,0,0,0.1), border=NA )

dev.off()

save( df_land_uptake_bauska, dc_per_bauska, dc_bauska_outarr, dc_bauska_outarrerr, file="dc_bauska.Rdata" )



