## mc_elsig_m1a_hilda_4box_caco3_5000+70_5000yr_15apr09_ddec.dat
## _____________________________________________________________

## Output used in Elsig et al., Nature, 2009; 

## 2000 Monte Carlo runs with pulse response model (versDec99)

## Data format: output time series of all 2000 runs written into same file sequentially
## -------------
## number of output time steps      0.10550000D+04
## number of runs                   0.20000000D+04
## start time of output            -0.89950000D+04
## end time of run                  0.15600000D+04
## output step                      0.10000000D+02

## in file: output of runs, stored sequentially
## 1 col: time
## 2. col: net air-to-sea flux, fas: (GtC/yr)
## 3. col: net air-to-land flux, fab (GtC/yr) 
## ------------------------------------------------------------------------------
library(Hmisc)

c_molmass <- 12.0107
nsteps <- 1055
startyr <- -8995
endyr <- 1560
dstep <- 10

## read one file to initialise data frame
names <- c( 'year', 'ocean_uptake', 'land_uptake' )
data <- read.table( '/alphadata01/bstocker/data/elsig_ddec/monte_carlo_elsig09nat_results/input_to_analysis_outputMCrun/mc_elsig_m1a_hilda_4box_caco3_5000+70_5000yr_15apr09_ddec.dat', col.names=names )

df_cum_land_uptake  <- data.frame( year=seq( from=startyr, to=endyr, by=dstep) )
df_land_uptake  <- data.frame( year=seq( from=startyr, to=endyr, by=dstep) )

irun <- 0
irow <- 1

## separate data array into columns for each run
while (irow <= dim(data)[1]){

  if ( data$year[irow] == startyr ) { 

    irun <- irun + 1 

    numstring <- sprintf( "%05d", irun-1)
    colstring <- paste( "r", numstring, sep="" )

    df_land_uptake[[ colstring ]]  <- data[irow:(irow+nsteps),3] * dstep
    df_cum_land_uptake[[ colstring ]]  <- cumsum(data[irow:(irow+nsteps),3]) * dstep

    irow <- irow + nsteps + 1

  } else {

    print("ERROR: wrong fast-forward of index")
    break

  }

}

nruns <- irun

## get mean and standard deviation and attach to data frame
df_cum_land_uptake$mean <- apply( df_cum_land_uptake[,2:nruns+1], c(1), FUN=mean )
df_cum_land_uptake$sd   <- apply( df_cum_land_uptake[,2:nruns+1], c(1), FUN=sd )

df_land_uptake$mean <- apply( df_land_uptake[,2:nruns+1], c(1), FUN=mean )
df_land_uptake$sd   <- apply( df_land_uptake[,2:nruns+1], c(1), FUN=sd )

## open plot
pdf( "dC_terr_elsig_holoLU.pdf", width=8, height=6 )
ylim <- c(-10,400)
par( las=1 )

plot( range(df_cum_land_uptake$year), ylim, type="n", ylab="cumulative terrestrial C balance [PgC]", xlab="year AD")

for (i in 1:nruns){

  numstring <- sprintf( "%05d", i-1)
  colstring <- paste( "r", numstring, sep="" )

  ## plot this time series of land c uptake into the previously opened plot
  lines( df_cum_land_uptake$year, df_cum_land_uptake[[ colstring ]], col=rgb(0,0,0,0.02) )

}

## add mean to plot
lines( df_cum_land_uptake$year, df_cum_land_uptake$mean, col="red", lwd=2 )

## add +/- 1-sigma range to plot
# lines( df_cum_land_uptake$year, (df_cum_land_uptake$mean - df_cum_land_uptake$sd), col="red", lwd=1 )
# lines( df_cum_land_uptake$year, (df_cum_land_uptake$mean + df_cum_land_uptake$sd), col="red", lwd=1 )

polygon( 
        c( df_cum_land_uptake$year, rev(df_cum_land_uptake$year) ), 
        c( (df_cum_land_uptake$mean - df_cum_land_uptake$sd), rev( (df_cum_land_uptake$mean + df_cum_land_uptake$sd) ) ), 
        col=rgb(1,0,0,0.3), border=NA 
        )



## EVALUATION OF PERIODS
## Alternativ: fixe Margins
periodsBP <- c(-11000,-7000,-5000,-2000,-400)
periodsAD <- periodsBP + 1950
periodsName <- c("11-7","7-5","5-2","2-0 (1 - 1560 AD)")

## 11 kyr BP is to be represented by -8990 AD = 10940 BP
periodsAD[1] <- -8990
periodsBP <- periodsAD - 1950

period_margins <- periodsAD

nper <- length(period_margins)-1

## add period margins to plot
# abline( v=period_margins )
# print( period_margins )
rect( period_margins[1], ylim[1], period_margins[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
rect( period_margins[3], ylim[1], period_margins[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
rect( period_margins[5], ylim[1], period_margins[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

for (i in 1:length(period_margins)){
  text( period_margins[i], 80, paste( as.character( -periodsBP[i] ), "yr BP"), cex=0.7 )
}

dev.off()


## Create data frame for evaluating periods
## period margins (start and end)
dc_per <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
dc_per$iper_start <- rep( NA, nper )
dc_per$iper_end <- rep( NA, nper )

## period margin's corresponding index in full (annual) data frame
for (i in 1:nper){
  dc_per$iper_start[i] <- which.min( abs( dc_per$per_start[i] - df_cum_land_uptake$year ) )
  dc_per$iper_end[i] <- which.min( abs( dc_per$per_end[i] - df_cum_land_uptake$year ) )
}

## Evaluate cumulative balance change in different periods for each run
for (n in 1:nruns){

  numstring <- sprintf( "%05d", n-1)
  colstring <- paste( "r", numstring, sep="" )

  dc_per[[ colstring ]] <- rep( NA, nper )

  for (i in 1:nper){

    dc_per[[ colstring ]][i] <- df_cum_land_uptake[[ colstring ]][dc_per$iper_end[i]] - df_cum_land_uptake[[ colstring ]][dc_per$iper_start[i]]

  }

}

## add column for period "name"
dc_per$name <- paste( as.character( dc_per$per_start ) , "-", as.character( dc_per$per_end ), sep="" )

## Get mean and SD of cumulative balance changes in each period
dc_per$mean <- apply( dc_per[,5:nruns+4], c(1), FUN=mean )
dc_per$sd   <- apply( dc_per[,5:nruns+4], c(1), FUN=sd )

## take only subset of dc_per
dc_sub <- subset( dc_per, select=c( name, per_start, per_end, iper_start, iper_end, mean, sd ))

## create data frames to be plotted with hist() and errbar()
dc_elsig_outarr <- data.frame( dc_terr=dc_sub$mean )
row.names( dc_elsig_outarr ) <- periodsName

dc_elsig_outarrerr <- data.frame( err_dc_terr=dc_sub$sd )
row.names( dc_elsig_outarrerr ) <- periodsName

## Plot bars for the periods
pdf("/alphadata01/bstocker/holoLU2/cbal_barplot_holocene_holoLU2.pdf", width=8, height=6 )
par( las=1 )
ylim <- c(-50,300)
# rect( mybar1[1,], rep(ylim[1],4), mybar1[4,], rep(ylim[2],4), col=rgb(0,0,0,0.2), border=NA )

mybar1 <- barplot( 
                  t(dc_elsig_outarr),
                  ylim=ylim,
                  col=c("green3"),
                  border=TRUE,
                  xlab="period [kyr BP]"
                  )
abline(0,0)

errbar(
       mybar1,
       t(dc_elsig_outarr),
       t(dc_elsig_outarr+dc_elsig_outarrerr),
       t(dc_elsig_outarr-dc_elsig_outarrerr),
       add=TRUE
       )

## add grey rectangles 
left <- mybar1 - 0.6
right <- mybar1 + 0.6
rect( left[1], ylim[1], right[1], ylim[2], col=rgb(0,0,0,0.1), border=NA )
rect( left[3], ylim[1], right[3], ylim[2], col=rgb(0,0,0,0.1), border=NA )
rect( left[5], ylim[1], right[5], ylim[2], col=rgb(0,0,0,0.1), border=NA )

dev.off()

save( df_land_uptake, dc_elsig_outarr, dc_elsig_outarrerr, dc_per, file="dc_elsig.Rdata" )

