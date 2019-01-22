  ## get mean and standard deviation at each time step and attach to data frame
  get_statistics_df <- function( df, cols ){

    df$mean     <- apply( df[,cols], c(1), FUN=mean )
    df$median   <- apply( df[,cols], c(1), FUN=median )
    df$sd       <- apply( df[,cols], c(1), FUN=sd )
    df$q90      <- apply( df[,cols], c(1), FUN=function(x) quantile( x, 0.9, na.rm=TRUE ) )
    df$q10      <- apply( df[,cols], c(1), FUN=function(x) quantile( x, 0.1, na.rm=TRUE ) )

    return(df)

  }