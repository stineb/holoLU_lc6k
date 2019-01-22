## /////////////////////////////////////////////////////////////////////////////////////////
## Reads original NCB data for each of the 1000 reconstructions, and evaluates peat C 
## balance in different periods of the Holocene, creating one column of the 
## Holocene-period balance arrays 'pt_mean_outarr', 'pt_median_outarr', 'pt_mean_outarrerr', 
## and 'pt_median_outarrerr'.
## -----------------------------------------------------------------------------------------
library(Hmisc)

dstep <- 10

## read processed data sent by Charly
charly <- read.csv("/alphadata01/bstocker/data/yu_data/NCB/NCB_65sites_median.csv")
df_charly_pt_median <- data.frame( age=rev(charly$age) )
df_charly_pt_median$year <- 1950 - df_charly_pt_median$age + 5
df_charly_pt_median$median <- rev(charly$NCB.50.) * dstep / 1000
df_charly_pt_median$cumsum_median <- cumsum(rev(charly$NCB.50.)) * dstep / 1000

charly <- read.csv("/alphadata01/bstocker/data/yu_data/NCB/NCB_65sites_mean.csv")
df_charly_pt_mean <- data.frame( age=rev(charly$age) )
df_charly_pt_mean$year <- 1950 - df_charly_pt_mean$age + 5
df_charly_pt_mean$median <- rev(charly$NCB.50.) * dstep / 1000
df_charly_pt_mean$cumsum_median <- cumsum(rev(charly$NCB.50.)) * dstep / 1000

## FOR EVALUATION OF PERIODS
## Alternativ: fixe Margins
periodsBP <- c(-11000,-7000,-5000,-2000,-400)
periodsAD <- periodsBP + 1950
periodsName <- c("11-7","7-5","5-2","2-0 (1 - 1560 AD)")
periodsAD[1] <- -8990 ## 11 kyr BP is to be represented by -8990 AD = 10940 BP
periodsBP <- periodsAD - 1950
period_margins <- periodsAD
nper <- length(period_margins)-1

## load Charly's raw data: contains list ("age" "NCB" "NCP" "NCR" "NCU"), where NCB (NCP, ...) are arrays of c(1377,1000) containing the 1000 iterations 
load('/alphadata01/bstocker/data/yu_data/NCB/MC_NCB_Mean.rdata') ## tmp
load('/alphadata01/bstocker/data/yu_data/NCB/MC_NCB_Median.rdata') ## tmp2

## number of individual runs
nruns <- dim( tmp$NCB )[2]
test  <- dim( tmp2$NCB )[2]
if (nruns!=test){print("WARNING: MEAN AND MEDIAN DF HAVE NOT IDENTICAL DIMENSIONS")}

## Remove last point in dataframe (is NA)
tmp$NCB  <- tmp$NCB[1:dim(tmp$NCB)[1]-1,]
tmp2$NCB <- tmp2$NCB[1:dim(tmp2$NCB)[1]-1,]

tmp$age  <- tmp$age[1:length(tmp$age)-1]
tmp2$age <- tmp2$age[1:length(tmp2$age)-1]

## Create data frame
df_pt_mean <- data.frame( age=rev(tmp$age) )
df_pt_median <- data.frame( age=rev(tmp2$age) )

df_cum_pt_mean <- data.frame( age=rev(tmp$age) )
df_cum_pt_median <- data.frame( age=rev(tmp2$age) )

## Add column names
nrunvec <- seq(0, nruns-1, 1)
namsvec <- sprintf( "%05d", nrunvec )
colsvec <- paste( "r", namsvec, sep="" )
for ( i in 1:nruns) {
  df_pt_mean[[ colsvec[i] ]]   <- rev(tmp$NCB[,i] * dstep / 1000)
  df_pt_median[[ colsvec[i] ]] <- rev(tmp2$NCB[,i] * dstep / 1000)

  ## Separate data frame for cumulative changes
  df_cum_pt_mean[[ colsvec[i] ]]   <- cumsum( rev(tmp$NCB[,i] * dstep / 1000) )
  # df_cum_pt_median[[ colsvec[i] ]] <- cumsum( rev(tmp2$NCB[,i] * dstep / 1000) )
}

## add column for year AD (center to mid-decade: '+ 5' )
df_pt_mean$year <- 1950 - df_pt_mean$age + 5
df_pt_median$year <- 1950 - df_pt_median$age + 5 

df_cum_pt_mean$year <- 1950 - df_cum_pt_mean$age + 5
df_cum_pt_median$year <- 1950 - df_cum_pt_median$age + 5 

## get mean and standard deviation and attach to data frame
df_pt_mean$mean   <- apply( df_pt_mean[,2:nruns+1], c(1), FUN=mean )
df_pt_median$mean <- apply( df_pt_median[,2:nruns+1], c(1), FUN=mean )

df_pt_mean$median   <- apply( df_pt_mean[,2:nruns+1], c(1), FUN=median )
df_pt_median$median <- apply( df_pt_median[,2:nruns+1], c(1), FUN=median )

df_pt_mean$sd   <- apply( df_pt_mean[,2:nruns+1], c(1), FUN=sd )
df_pt_median$sd <- apply( df_pt_median[,2:nruns+1], c(1), FUN=sd )

df_cum_pt_mean$mean   <- apply( df_cum_pt_mean[,2:nruns+1], c(1), FUN=mean )
df_cum_pt_median$mean <- apply( df_cum_pt_median[,2:nruns+1], c(1), FUN=mean )

df_cum_pt_mean$median   <- apply( df_cum_pt_mean[,2:nruns+1], c(1), FUN=median )
df_cum_pt_median$median <- apply( df_cum_pt_median[,2:nruns+1], c(1), FUN=median )

df_cum_pt_mean$sd   <- apply( df_cum_pt_mean[,2:nruns+1], c(1), FUN=sd )
df_cum_pt_median$sd <- apply( df_cum_pt_median[,2:nruns+1], c(1), FUN=sd )


## Plot: time series of NCB, MEDIAN METHOD, HOLOCENE
pdf( "dC_pt_yu_holoLU_medianmethod.pdf", width=8, height=6 )
  ylim <- c(-0.02,0.120)
  par( las=1 )

  plot( range(df_pt_median$year), ylim, type="n", ylab="peat C balance [PgC/yr]", xlab="year AD")

  for (i in 1:100){

    ## plot this time series of land c uptake into the previously opened plot
    lines( df_pt_median$year, df_pt_median[[ colsvec[i] ]] / dstep, col=rgb(0,0,0,0.01) )

  }

  ## add +/- 1-sigma range to plot
  polygon( 
          c( df_pt_median$year, rev(df_pt_median$year) ), 
          c( (df_pt_median$mean - df_pt_median$sd), rev( (df_pt_median$mean + df_pt_median$sd) ) ) / dstep, 
          col=rgb(1,0,0,0.3), border=NA 
          )


  ## Add mean to plot
  lines( df_pt_median$year, df_pt_median$mean / dstep, col="red", lwd=2 )

  ## Add median to plot
  lines( df_pt_median$year, df_pt_median$median / dstep, col="red", lwd=1, lty=2 )

  # ## Add Charly's median to plot
  # lines( df_charly_pt_median$year, df_charly_pt_median$median / dstep, col="blue", lwd=1 )

  ## add period margins to plot
  # abline( v=period_margins )
  # print( period_margins )
  rect( period_margins[1], ylim[1], period_margins[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], ylim[1], period_margins[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5], ylim[1], period_margins[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

  for (i in 1:length(period_margins)){
    text( period_margins[i], ylim[1], paste( as.character( -periodsBP[i] ), "yr BP"), cex=0.7 )
  }

  legend( "topleft", c("my mean","my median"), lty=c(1,2), bty="n", col=c("red","red"), lwd=c(2,1) )

dev.off()

## Plot: time series of CUMULATIVE NCB, MEDIAN METHOD, HOLOCENE
pdf( "dC_cum_pt_yu_holoLU_medianmethod.pdf", width=8, height=6 )
  ylim <- c(0,650)
  par( las=1 )

  plot( range(df_cum_pt_median$year), ylim, type="n", ylab="peat C balance [PgC/1000 yr]", xlab="year AD")

  for (i in 1:100){

    ## plot this time series of land c uptake into the previously opened plot
    lines( df_cum_pt_median$year, df_cum_pt_median[[ colsvec[i] ]], col=rgb(0,0,0,0.01) )

  }

  ## add +/- 1-sigma range to plot
  polygon( 
          c( df_cum_pt_median$year, rev(df_cum_pt_median$year) ), 
          c( (df_cum_pt_median$mean - df_cum_pt_median$sd), rev( (df_cum_pt_median$mean + df_cum_pt_median$sd) ) ), 
          col=rgb(1,0,0,0.3), border=NA 
          )

  ## Add mean to plot
  lines( df_cum_pt_median$year, df_cum_pt_median$mean, col="red", lwd=2 )

  ## Add median to plot
  lines( df_cum_pt_median$year, df_cum_pt_median$median, col="red", lwd=1, lty=2 )

  # ## Add Charly's median to plot
  # lines( df_charly_pt_median$year, df_charly_pt_median$cumsum_median, col="blue", lwd=1 )

  ## add period margins to plot
  # abline( v=period_margins )
  # print( period_margins )
  rect( period_margins[1], ylim[1], period_margins[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], ylim[1], period_margins[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5], ylim[1], period_margins[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

  for (i in 1:length(period_margins)){
    text( period_margins[i], ylim[1], paste( as.character( -periodsBP[i] ), "yr BP"), cex=0.7 )
  }

  legend( "topleft", c("my mean","my median"), lty=c(1,2), bty="n", col=c("red","red"), lwd=c(2,1) )

dev.off()

## Plot: time series of NCB, MEAN METHOD, HOLOCENE
pdf( "dC_pt_yu_holoLU_meanmethod.pdf", width=8, height=6 )
  ylim <- c(-0.02,0.120)
  par( las=1 )

  plot( range(df_pt_mean$year), ylim, type="n", ylab="peat C balance [PgC/yr]", xlab="year AD")

  for (i in 1:100){

    ## plot this time series of land c uptake into the previously opened plot
    lines( df_pt_mean$year, df_pt_mean[[ colsvec[i] ]] / dstep, col=rgb(0,0,0,0.01) )

  }

  ## add +/- 1-sigma range to plot
  polygon( 
          c( df_pt_mean$year, rev(df_pt_mean$year) ), 
          c( (df_pt_mean$mean - df_pt_mean$sd), rev( (df_pt_mean$mean + df_pt_mean$sd) ) ) / dstep, 
          col=rgb(1,0,0,0.3), border=NA 
          )


  ## Add mean to plot
  lines( df_pt_mean$year, df_pt_mean$mean / dstep, col="red", lwd=2 )

  ## Add median to plot
  lines( df_pt_mean$year, df_pt_mean$median / dstep, col="red", lwd=1, lty=2 )

  # ## Add Charly's median to plot
  # lines( df_charly_pt_mean$year, df_charly_pt_mean$median / dstep, col="blue", lwd=1 )

  ## add period margins to plot
  # abline( v=period_margins )
  # print( period_margins )
  rect( period_margins[1], ylim[1], period_margins[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], ylim[1], period_margins[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5], ylim[1], period_margins[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

  for (i in 1:length(period_margins)){
    text( period_margins[i], ylim[1], paste( as.character( -periodsBP[i] ), "yr BP"), cex=0.7 )
  }

  # legend( "topleft", c("my mean","my median", "Charly's median"), lty=c(1,2,1), bty="n", col=c("red","red","blue"), lwd=c(2,1,1) )
  legend( "topleft", c("my mean","my median"), lty=c(1,2), bty="n", col=c("red","red"), lwd=c(2,1) )

dev.off()

# ## Plot: time series of CUMULATIVE NCB, MEAN METHOD, HOLOCENE
pdf( "dC_cum_pt_yu_holoLU_meanmethod.pdf", width=8, height=6 )
  ylim <- c(0,650)
  par( las=1 )

  plot( range(df_cum_pt_mean$year), ylim, type="n", ylab="peat C balance [PgC/1000 yr]", xlab="year AD")

  for (i in 1:100){

    ## plot this time series of land c uptake into the previously opened plot
    lines( df_cum_pt_mean$year, df_cum_pt_mean[[ colsvec[i] ]], col=rgb(0,0,0,0.01) )

  }

  ## add +/- 1-sigma range to plot
  polygon( 
          c( df_cum_pt_mean$year, rev(df_cum_pt_mean$year) ), 
          c( (df_cum_pt_mean$mean - df_cum_pt_mean$sd), rev( (df_cum_pt_mean$mean + df_cum_pt_mean$sd) ) ), 
          col=rgb(1,0,0,0.3), border=NA 
          )

  ## Add mean to plot
  lines( df_cum_pt_mean$year, df_cum_pt_mean$mean, col="red", lwd=2 )

  ## Add median to plot
  lines( df_cum_pt_mean$year, df_cum_pt_mean$median, col="red", lwd=1, lty=2 )

  # ## Add Charly's median to plot
  # lines( df_charly_pt_mean$year, df_charly_pt_mean$cumsum_median, col="blue", lwd=1 )

  ## add period margins to plot
  # abline( v=period_margins )
  # print( period_margins )
  rect( period_margins[1], ylim[1], period_margins[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], ylim[1], period_margins[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5], ylim[1], period_margins[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

  for (i in 1:length(period_margins)){
    text( period_margins[i], ylim[1], paste( as.character( -periodsBP[i] ), "yr BP"), cex=0.7 )
  }

  legend( "topleft", c("my mean","my median"), lty=c(1,2), bty="n", col=c("red","red"), lwd=c(2,1) )

dev.off()

## Plot: time series of NCB, MEAN METHOD, LAST MILLENNIUM
pdf( "dC_pt_yu_holoLU_meanmethod_lastmill.pdf", width=8, height=6 )
  # period_margins <- c( 760, 960, 1200, 1460, 1530, 1760, 1920 )
  period_margins <- c( 760, 960, 1200, 1500, 1650, 1760, 1920 )
  ylim <- c(-0.02,0.120)
  xlim <- c(-200,1945)
  par( las=1 )

  plot( xlim, ylim, type="n", ylab="peat C balance [PgC/yr]", xlab="year AD")

  for (i in 1:100){

    ## plot this time series of land c uptake into the previously opened plot
    lines( df_pt_mean$year, df_pt_mean[[ colsvec[i] ]] / dstep, col=rgb(0,0,0,0.1) )

  }

  ## add +/- 1-sigma range to plot
  polygon( 
          c( df_pt_mean$year, rev(df_pt_mean$year) ), 
          c( (df_pt_mean$mean - df_pt_mean$sd), rev( (df_pt_mean$mean + df_pt_mean$sd) ) ) / dstep, 
          col=rgb(1,0,0,0.3), border=NA 
          )


  ## Add mean to plot
  lines( df_pt_mean$year, df_pt_mean$mean / dstep, col="red", lwd=2 )

  ## Add median to plot
  lines( df_pt_mean$year, df_pt_mean$median / dstep, col="red", lwd=1, lty=2 )

  # ## Add Charly's median to plot
  # lines( df_charly_pt_mean$year, df_charly_pt_mean$median / dstep, col="blue", lwd=1 )

  ## add period margins to plot
  # abline( v=period_margins )
  # print( period_margins )
  rect( period_margins[1], ylim[1], period_margins[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], ylim[1], period_margins[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5], ylim[1], period_margins[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

  for (i in 1:length(period_margins)){
    text( period_margins[i], ylim[1], paste( as.character( period_margins[i] ), "AD"), cex=0.7 )
  }

  # legend( "topleft", c("my mean","my median", "Charly's median"), lty=c(1,2,1), bty="n", col=c("red","red","blue"), lwd=c(2,1,1) )
  legend( "topleft", c("my mean","my median"), lty=c(1,2), bty="n", col=c("red","red"), lwd=c(2,1) )

dev.off()

## Plot: time series of CUMULATIVE NCB, MEAN METHOD, LAST MILLENNIUM
pdf( "dC_cum_pt_yu_holoLU_meanmethod_lastmill.pdf", width=8, height=6 )
  ylim <- c(550,650)
  par( las=1 )

  plot( xlim, ylim, type="n", ylab="peat C balance [PgC/1000 yr]", xlab="year AD")

  for (i in 1:100){

    ## plot this time series of land c uptake into the previously opened plot
    lines( df_cum_pt_mean$year, df_cum_pt_mean[[ colsvec[i] ]], col=rgb(0,0,0,0.1) )

  }

  ## add +/- 1-sigma range to plot
  polygon( 
          c( df_cum_pt_mean$year, rev(df_cum_pt_mean$year) ), 
          c( (df_cum_pt_mean$mean - df_cum_pt_mean$sd), rev( (df_cum_pt_mean$mean + df_cum_pt_mean$sd) ) ), 
          col=rgb(1,0,0,0.3), border=NA 
          )

  ## Add mean to plot
  lines( df_cum_pt_mean$year, df_cum_pt_mean$mean, col="red", lwd=2 )

  ## Add median to plot
  lines( df_cum_pt_mean$year, df_cum_pt_mean$median, col="red", lwd=1, lty=2 )

  # ## Add Charly's median to plot
  # lines( df_charly_pt_mean$year, df_charly_pt_mean$cumsum_median, col="blue", lwd=1 )

  ## add period margins to plot
  # abline( v=period_margins )
  # print( period_margins )
  rect( period_margins[1], ylim[1], period_margins[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], ylim[1], period_margins[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5], ylim[1], period_margins[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

  for (i in 1:length(period_margins)){
    text( period_margins[i], ylim[1], paste( as.character( period_margins[i] ), "AD"), cex=0.7 )
  }

  legend( "topleft", c("my mean","my median"), lty=c(1,2), bty="n", col=c("red","red"), lwd=c(2,1) )

dev.off()


## HOLOCENE: Create data frame for evaluating periods
  ## re-define period margins
  periodsBP <- c(-11000,-7000,-5000,-2000,-400)
  periodsAD <- periodsBP + 1950
  periodsName <- c("11-7","7-5","5-2","2-0 (1 - 1560 AD)")
  periodsAD[1] <- -8990 ## 11 kyr BP is to be represented by -8990 AD = 10940 BP
  periodsBP <- periodsAD - 1950
  period_margins <- periodsAD
  nper <- length(period_margins)-1

  ## period margins (start and end)
  dc_pt_per_mean <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
  dc_pt_per_mean$iper_start <- rep( NA, nper )
  dc_pt_per_mean$iper_end <- rep( NA, nper )

  dc_pt_per_median <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
  dc_pt_per_median$iper_start <- rep( NA, nper )
  dc_pt_per_median$iper_end <- rep( NA, nper )

  ## period margin's corresponding index in full (annual) data frame
  for (i in 1:nper){
    dc_pt_per_mean$iper_start[i] <- which.min( abs( dc_pt_per_mean$per_start[i] - df_cum_pt_mean$year ) )
    dc_pt_per_mean$iper_end[i]   <- which.min( abs( dc_pt_per_mean$per_end[i] - df_cum_pt_mean$year ) )

    dc_pt_per_median$iper_start[i] <- which.min( abs( dc_pt_per_median$per_start[i] - df_cum_pt_median$year ) )
    dc_pt_per_median$iper_end[i]   <- which.min( abs( dc_pt_per_median$per_end[i] - df_cum_pt_median$year ) )
  }

  ## Evaluate cumulative balance change in different periods for each run
  for (n in 1:nruns){

    numstring <- sprintf( "%05d", n-1)
    colstring <- paste( "r", numstring, sep="" )

    dc_pt_per_mean[[ colstring ]]   <- rep( NA, nper )
    dc_pt_per_median[[ colstring ]] <- rep( NA, nper )

    for (i in 1:nper){

      dc_pt_per_mean[[ colstring ]][i] <- (df_cum_pt_mean[[ colstring ]][dc_pt_per_mean$iper_end[i]] - df_cum_pt_mean[[ colstring ]][dc_pt_per_mean$iper_start[i]])
      dc_pt_per_median[[ colstring ]][i] <- (df_cum_pt_median[[ colstring ]][dc_pt_per_median$iper_end[i]] - df_cum_pt_median[[ colstring ]][dc_pt_per_median$iper_start[i]])

    }

  }

  ## add column for period "name"
  dc_pt_per_mean$name <- paste( as.character( dc_pt_per_mean$per_start ) , "-", as.character( dc_pt_per_mean$per_end ), sep="" )
  dc_pt_per_median$name <- paste( as.character( dc_pt_per_median$per_start ) , "-", as.character( dc_pt_per_median$per_end ), sep="" )

  ## Get mean and SD of cumulative balance changes in each period
  dc_pt_per_mean$mean <- apply( dc_pt_per_mean[,5:nruns+4], c(1), FUN=mean )
  dc_pt_per_mean$sd   <- apply( dc_pt_per_mean[,5:nruns+4], c(1), FUN=sd )

  dc_pt_per_median$mean <- apply( dc_pt_per_median[,5:nruns+4], c(1), FUN=mean )
  dc_pt_per_median$sd   <- apply( dc_pt_per_median[,5:nruns+4], c(1), FUN=sd )

  ## take only subset of dc_pt_per_mean (drop remaining columns)
  dc_pt_mean_sub <- subset( dc_pt_per_mean, select=c( name, per_start, per_end, iper_start, iper_end, mean, sd ))
  dc_pt_median_sub <- subset( dc_pt_per_median, select=c( name, per_start, per_end, iper_start, iper_end, mean, sd ))

  ## create data frames to be plotted with hist() and errbar()
  pt_mean_outarr <- data.frame( dc_pt=dc_pt_mean_sub$mean )
  row.names( pt_mean_outarr ) <- periodsName

  pt_median_outarr <- data.frame( dc_pt=dc_pt_median_sub$mean )
  row.names( pt_median_outarr ) <- periodsName

  pt_mean_outarrerr <- data.frame( err_dc_pt=dc_pt_mean_sub$sd )
  row.names( pt_mean_outarrerr ) <- periodsName

  pt_median_outarrerr <- data.frame( err_dc_pt=dc_pt_median_sub$sd )
  row.names( pt_median_outarrerr ) <- periodsName

## Plot bars for the periods
pdf("/alphadata01/bstocker/holoLU2/cbal_pt_barplot_holocene_holoLU2.pdf", width=8, height=6 )
  par( las=1 )
  ylim <- c(-50,300)
  # rect( mybar1[1,], rep(ylim[1],4), mybar1[4,], rep(ylim[2],4), col=rgb(0,0,0,0.2), border=NA )

  mybar1 <- barplot( 
                    t(pt_mean_outarr),
                    ylim=ylim,
                    col=c("green3"),
                    border=TRUE,
                    xlab="period [kyr BP]"
                    )
  abline(0,0)

  errbar(
         mybar1,
         t(pt_mean_outarr),
         t(pt_mean_outarr+pt_mean_outarrerr),
         t(pt_mean_outarr-pt_mean_outarrerr),
         add=TRUE
         )

  ## add grey rectangles 
  left <- mybar1 - 0.6
  right <- mybar1 + 0.6
  rect( left[1], ylim[1], right[1], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[3], ylim[1], right[3], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[5], ylim[1], right[5], ylim[2], col=rgb(0,0,0,0.1), border=NA )

dev.off()

## Save data to Rdata file
save( pt_mean_outarr, pt_mean_outarrerr, pt_median_outarr, pt_median_outarrerr, df_pt_mean, df_pt_median, dc_pt_per_mean, dc_pt_per_median, file="pt_mean_outarrerr" )




