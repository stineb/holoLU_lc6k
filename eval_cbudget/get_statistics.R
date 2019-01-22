  ## get mean and standard deviation at each time step and attach to data frame
  get_statistics <- function( inarray, cols ){

    out <- array( NA, dim=c(dim(inarray)[1],5) )
    out[,1] <- apply( inarray[,cols], c(1), FUN=mean )
    out[,2] <- apply( inarray[,cols], c(1), FUN=median )
    out[,3] <- apply( inarray[,cols], c(1), FUN=sd )
    out[,4] <- apply( inarray[,cols], c(1), FUN=function(x) quantile( x, 0.9, na.rm=TRUE ) )
    out[,5] <- apply( inarray[,cols], c(1), FUN=function(x) quantile( x, 0.1, na.rm=TRUE ) )

    colnames(out) <- c("mean","median","sd","q90","q10")

    return(out)

  }