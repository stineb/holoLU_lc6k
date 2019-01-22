## Yu Holocene data
load( "pt_yu_holocene.Rdata" )

## HOLOCENE: Create data frame for evaluating periods
## re-define period margins
# periodsBP <- c(-10000,-7000,-5000,-2000,-400)
periodsBP <- c(-11000,-9000,-7000,-5000,-3000,-1000)
periodsAD <- periodsBP + 1950
periodsName <- c("11-9","9-7","7-5","5-3","3-1")
periodsBP <- periodsAD - 1950
period_margins <- periodsAD
nper <- length(period_margins)-1

## LPX data
trace129 <- list()
filn <- "peatglobnep_trace21_129.dat"
col.names <- c( "year", "peatglobnep_global", "peatglobnep_maskedby_yu", "peatglobnep_maskedby_yu_north" )
trace129$globnep <- read.table( filn, col.names=col.names )

## Plot: time series of NCB, MEAN METHOD, HOLOCENE
pdf( "dC_pt_yu_holoLU_meanmethod.pdf", width=8, height=6 )

  ylim <- c(-0.02,0.120)
  xlim <- c(-12000,850)
  par( las=1 , xaxs="i", yaxs="i" )

  plot( range(df_pt_mean$year), ylim, type="n", ylab="peat C balance [PgC/yr]", xlab="year AD", xlim=xlim, axes=FALSE )
  axis( 1, lwd=1.75 );  axis( 1, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
  axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
  axis( 2, lwd=1.75 );  axis( 2, at=seq(xlim[1],xlim[2],by=0.005), labels=F, tck=-0.01 )
  axis( 4, lwd=1.75, labels=FALSE );  axis( 4, at=seq(xlim[1],xlim[2],by=0.005), labels=F, tck=-0.01 )
  box( lwd=1.75 )

  for (i in 1:100){

    ## plot this time series of land c uptake into the previously opened plot
    lines( df_pt_mean$year, df_pt_mean[[ colsvec[i] ]] / dstep, col=add_alpha( "dodgerblue4", 0.01 ) )

  }

  ## add +/- 1-sigma range to plot
  polygon( 
          c( df_pt_mean$year, rev(df_pt_mean$year) ), 
          c( (df_pt_mean$q16), rev( (df_pt_mean$q84) ) ) / dstep, 
          col=add_alpha( "dodgerblue4", 0.3 ), border=NA 
          )


  # ## Add mean to plot
  # lines( df_pt_mean$year, df_pt_mean$mean / dstep, col="red", lwd=2 )

  ## Add median to plot
  lines( df_pt_mean$year, df_pt_mean$median / dstep, col="dodgerblue4", lwd=2, lty=1 )

  # ## Add Charly's median to plot
  # lines( df_charly_pt_mean$year, df_charly_pt_mean$median / dstep, col="blue", lwd=1 )

  ## Add model NEP to plot
  boxl <- 1
  lines( trace129$globnep$year, filter( trace129$globnep$peatglobnep_global/1e15, rep(1/boxl,boxl), sides=2 ), lwd=1, col=add_alpha( "dodgerblue1", 0.3 ) )
  boxl <- 30
  lines( trace129$globnep$year, filter( trace129$globnep$peatglobnep_global/1e15, rep(1/boxl,boxl), sides=2 ), lwd=2, col="dodgerblue1" )


  ## add period margins to plot
  # abline( v=period_margins )
  # print( period_margins )
  rect( period_margins[1], ylim[1], period_margins[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], ylim[1], period_margins[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5], ylim[1], period_margins[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

dev.off()


## Plot: time series of CUMULATIVE NCB, MEAN METHOD, HOLOCENE
pdf( "dC_cum_pt_yu_holoLU_meanmethod.pdf", width=8, height=6 )

  ylim <- c(-20,700)
  xlim <- c(-12000,850)
  par( las=1 , xaxs="i", yaxs="i" )

  plot( range(df_cum_pt_mean$year), ylim, type="n", ylab="cumulative peat C since 11,850 BCE [PgC]", xlab="year AD", xlim=xlim )

  axis( 1, lwd=1.75 );  axis( 1, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
  axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
  axis( 2, lwd=1.75 );  axis( 2, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01 )
  axis( 4, lwd=1.75, labels=FALSE );  axis( 4, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01 )
  box( lwd=1.75 )

  ## add period margins to plot
  # abline( v=period_margins )
  # print( period_margins )
  rect( period_margins[1], ylim[1], period_margins[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[3], ylim[1], period_margins[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( period_margins[5], ylim[1], period_margins[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

  ## PEAT C
  ## plot this time series of land c uptake into the previously opened plot
  for (i in 1:nruns){
    lines( df_cum_pt_mean$year, df_cum_pt_mean[[ colsvec[i] ]], col=add_alpha( "dodgerblue4", 0.01 ) )
  }
  ## add +/- 1-sigma range to plot
  polygon( 
          c( df_cum_pt_mean$year, rev(df_cum_pt_mean$year) ), 
          # c( (df_cum_pt_mean$mean - df_cum_pt_mean$sd), rev( (df_cum_pt_mean$mean + df_cum_pt_mean$sd) ) ), 
          c( (df_cum_pt_mean$q16), rev( (df_cum_pt_mean$q84) ) ), 
          col=add_alpha( "dodgerblue4", 0.3 ), border=NA 
          )
  # ## Add mean to plot
  # lines( df_cum_pt_mean$year, df_cum_pt_mean$mean, col="red", lwd=2 )

  # ## Add median to plot
  lines( df_cum_pt_mean$year, df_cum_pt_mean$median, col="dodgerblue4", lwd=2, lty=1 )
  # lines( df_cum_pt_mean$year, cumsum( df_pt_mean$median ), col="red", lwd=3, lty=1  )

  ## Add Charly's median to plot
  # lines( df_charly_pt_mean$year, df_charly_pt_mean$cumsum_median, col="blue", lwd=1 )

  ## Add model cumulative NEP to plot
  lines( trace129$globnep$year[ which.min( abs( trace129$globnep$year - df_cum_pt_mean$year[1] ) ):length(trace129$globnep$peatglobnep_global) ], 
    cumsum( trace129$globnep$peatglobnep_global[ which.min( abs( trace129$globnep$year - df_cum_pt_mean$year[1] ) ):length(trace129$globnep$peatglobnep_global) ])/1e14, 
    lwd=2, col="dodgerblue1"
    )

dev.off()


