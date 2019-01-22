library(Hmisc)

df_sum_land_uptake_indus <- read.table( 
  '/alphadata01/bstocker/data/joos_ddec/ddec_biota_uptake.dat', 
  col.names=c( 'year', 'mean', 'mean-1sigma', 'mean+1sigma', 'sigma' ),
  skip=26 
  )
df_sum_land_uptake_indus$cum_mean <- cumsum( df_sum_land_uptake_indus$mean )

plot( df_sum_land_uptake_indus$year, df_sum_land_uptake_indus$cum_mean, type="l" )
abline( v=c(1600, 1750, 1950))

sum( df_sum_land_uptake_indus$mean[which( df_sum_land_uptake_indus$year>1850 & df_sum_land_uptake_indus$year<1950 )] )

save( df_sum_land_uptake_indus, file='dc_terr_indus.Rdata' )


