library(abind)
source('get_statistics.R')
source("/alphadata01/bstocker/sofun/utils_sofun/analysis_sofun/add_alpha.R")

##-------------------------------------------------------------------------------
## Get time series delta C tot for Holocene, last millennium, and industrial era
##-------------------------------------------------------------------------------
load('dc_terr_elsig_holocene.Rdata')
load('dc_terr_bauska_lastmill.Rdata')
load('dc_terr_indus.Rdata')

## ==> cut Elsig data after start year of Bauska data
yrstart_bauska <- df_land_uptake_bauska$year[1]
iyr_end_elsig  <- which( df_land_uptake$year==yrstart_bauska )
df_land_uptake <- df_land_uptake[1:iyr_end_elsig,1:2002]

##-------------------------------------------------------------------------------
## Combine Holocene and Lastmill data into single data frame containing cumulative change in the two periods
##-------------------------------------------------------------------------------
## Number of random draws from independent time series 
nruns <- 5000

## Sample data
colnr_elsig  <- sample( 2:2002, nruns, replace=TRUE )
colnr_bauska <- sample( 2:1001, nruns, replace=TRUE )

len <- dim(df_land_uptake)[1] + dim(df_land_uptake_bauska)[1] - 1
dc_merged     <- array( NA, dim=c( len, nruns ) )
dc_cum_merged <- array( NA, dim=c( len, nruns ) )

for (irun in 1:nruns){

  ## first part is from elsig data
  dc_merged[1:iyr_end_elsig,irun] <- df_land_uptake[,colnr_elsig[irun]]

  ## second part is from bauska data
  dc_merged[(iyr_end_elsig+1):len,irun] <- df_land_uptake_bauska[2:dim(df_land_uptake_bauska)[1],colnr_bauska[irun]] * 1e-14

  ## cumulate 
  dc_cum_merged[,irun] <- cumsum( dc_merged[,irun] )

}

## get statistics
dc_merged_sum     <- as.data.frame( get_statistics( dc_merged, 1:nruns ) )
dc_cum_merged_sum <- as.data.frame( get_statistics( dc_cum_merged, 1:nruns ) )

## add years
year <- rep( NA, dim(dc_merged)[1])
year[1:iyr_end_elsig]       <- df_land_uptake$year
year[(iyr_end_elsig+1):len] <- df_land_uptake_bauska$year[2:dim(df_land_uptake_bauska)[1]]

dc_merged_sum$year <- year
dc_cum_merged_sum$year <- year

save( dc_merged_sum, dc_cum_merged_sum, file="dc_merged.Rdata" )

##-------------------------------------------------------------------------------
## Conform merded data to 10-year time step for all periods
##-------------------------------------------------------------------------------
iyr_start_bauska <- 976
tmp1  <- dc_merged[1:iyr_start_bauska,]
year1 <- year[1:iyr_start_bauska]

tmp2  <- dc_merged[(iyr_start_bauska+1):dim(dc_merged)[1],]
year2 <- year[(iyr_start_bauska+1):dim(dc_merged)[1]]

year2_collapsed <- seq( 765, 1920, by=10 )
tmp2_collapsed  <- array( NA, dim=c(length(year2_collapsed),dim(dc_merged)[2]))

for (iyr in 1:length(year2_collapsed)){
  tmp2_collapsed[iyr,] <- apply( tmp2[ which( year2==year2_collapsed[iyr]-5 ):which( year2==year2_collapsed[iyr]+4 ) , ], 2, FUN=sum )
}

dc_merged_collapsed <- abind( tmp1, tmp2_collapsed, along=1 )
year_collapsed      <- c( year1, year2_collapsed )

dc_cum_merged_collapsed <- dc_merged_collapsed
for (irun in 1:nruns){
  dc_cum_merged_collapsed[,irun] <- cumsum( dc_merged_collapsed[,irun] )
}

## add statistics
dc_merged_collapsed_sum     <- as.data.frame( get_statistics( dc_merged_collapsed, 1:nruns ) )
dc_cum_merged_collapsed_sum <- as.data.frame( get_statistics( dc_cum_merged_collapsed, 1:nruns ) )

dc_merged_collapsed_sum$year <- year_collapsed
dc_cum_merged_collapsed_sum$year <- year_collapsed

save( dc_merged_sum, dc_cum_merged_sum, dc_merged_collapsed_sum, dc_cum_merged_collapsed_sum, file="dc_merged.Rdata" )


##-------------------------------------------------------------------------------
## Get remainder
##-------------------------------------------------------------------------------
load("pt_yu_lastmill.Rdata" )

## cut peat array to years available in dc dataframe 
lastyr  <- dc_merged_collapsed_sum$year[length(dc_merged_collapsed_sum$year)]
ilastyr <- which( df_pt_mean_lhnfix$year == lastyr )

firstyr  <- dc_merged_collapsed_sum$year[1]
ifirstyr <- which( df_pt_mean_lhnfix$year == firstyr )

df_pt_mean_lhnfix <- df_pt_mean_lhnfix[ ifirstyr:ilastyr, ]

## Sample data
nruns <- 5000

colnr_dc <- sample( 1:5000, nruns, replace=TRUE )
colnr_pt <- sample( 3:1002, nruns, replace=TRUE )

dc_remainder_merged     <- array( NA, dim=c( dim(dc_merged_collapsed)[1], nruns ) )
dc_cum_remainder_merged <- array( NA, dim=c( dim(dc_merged_collapsed)[1], nruns ) )

for (irun in 1:nruns){

  dc_remainder_merged[,irun] <- dc_merged_collapsed[,colnr_dc[irun]] - df_pt_mean_lhnfix[,colnr_pt[irun]]

  ## cumulate 
  dc_cum_remainder_merged[,irun] <- cumsum( dc_remainder_merged[,irun] )

}

## add statistics
dc_remainder_merged_sum     <- as.data.frame( get_statistics( dc_remainder_merged, 1:nruns ) )
dc_cum_remainder_merged_sum <- as.data.frame( get_statistics( dc_cum_remainder_merged, 1:nruns ) )

dc_remainder_merged_sum$year     <- year_collapsed
dc_cum_remainder_merged_sum$year <- year_collapsed

save( dc_merged_sum, dc_cum_merged_sum, dc_merged_collapsed_sum, dc_cum_merged_collapsed_sum, dc_remainder_merged_sum, dc_cum_remainder_merged_sum, file="dc_merged.Rdata" )

