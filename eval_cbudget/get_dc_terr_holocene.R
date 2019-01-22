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

source('get_statistics_df.R')

c_molmass <- 12.0107
nsteps <- 1055
startyr <- -8995
endyr <- 1560
dstep <- 10

## read one file to initialise data frame
names <- c( 'year', 'ocean_uptake', 'land_uptake' )
data <- read.table( '/alphadata01/bstocker/data/elsig_ddec/monte_carlo_elsig09nat_results/input_to_analysis_outputMCrun/mc_elsig_m1a_hilda_4box_caco3_5000+70_5000yr_15apr09_ddec.dat', col.names=names )

df_cum_land_uptake  <- data.frame( year=seq( from=startyr, to=endyr, by=dstep) )
df_land_uptake      <- data.frame( year=seq( from=startyr, to=endyr, by=dstep) )

df_cum_ocean_uptake  <- data.frame( year=seq( from=startyr, to=endyr, by=dstep) )
df_ocean_uptake      <- data.frame( year=seq( from=startyr, to=endyr, by=dstep) )

irun <- 0
irow <- 1

## separate data array into columns for each run
while (irow <= dim(data)[1]){

  if ( data$year[irow] == startyr ) { 

    irun <- irun + 1 

    numstring <- sprintf( "%05d", irun-1)
    colstring <- paste( "r", numstring, sep="" )

    df_land_uptake[[ colstring ]]      <- data$land_uptake[irow:(irow+nsteps)] * dstep
    df_cum_land_uptake[[ colstring ]]  <- cumsum(data$land_uptake[irow:(irow+nsteps)]) * dstep

    df_ocean_uptake[[ colstring ]]      <- data$ocean_uptake[irow:(irow+nsteps)] * dstep
    df_cum_ocean_uptake[[ colstring ]]  <- cumsum(data$ocean_uptake[irow:(irow+nsteps)]) * dstep

    irow <- irow + nsteps + 1

  } else {

    print("ERROR: wrong fast-forward of index")
    break

  }

}

nruns <- irun

## get mean and standard deviation and attach to data frame
df_land_uptake <- get_statistics_df( df_land_uptake, 2:nruns+1)
df_cum_land_uptake <- get_statistics_df( df_cum_land_uptake, 2:nruns+1)

df_ocean_uptake <- get_statistics_df( df_ocean_uptake, 2:nruns+1)
df_cum_ocean_uptake <- get_statistics_df( df_cum_ocean_uptake, 2:nruns+1)

## save to file
save( df_land_uptake, df_cum_land_uptake, df_ocean_uptake, df_cum_ocean_uptake, nruns, file='dc_terr_elsig_holocene.Rdata' )




