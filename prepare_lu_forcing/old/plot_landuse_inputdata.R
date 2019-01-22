library(ncdf)
library(fields)
library(sp)
library(maptools)
library(rgeos)

source('~/.Rprofile')

inextlower <- function( vec, val ){
  i <- 1
  while (val>vec[i]) {
    i <- i + 1
  }
  return( i-1 )
}

inextupper <- function( vec, val ){
  i <- 1
  while (val>vec[i]) {
    i <- i + 1
  }
  return( i )
}

interpol_field <- function( time0, time1, field0, field1, outtime ){
  outfield <- field0 + (field1-field0)/(time1-time0) * (outtime - time0)
  return(outfield)
}


################
## SAGE
# ################
  # ## Read land suitability field (SAGE)
  # suit <- open.ncdf( "/alphadata01/bstocker/data/landuse_data/sage/land_suit_sage_halfdeg.nc" )
  # suit_sage <- get.var.ncdf(suit, "DATA")
  # close.ncdf(suit)

  # lev <- c(0,1,10)

  # magn <- 4
  # ncols <- 2
  # nrows <- 1
  # widths <- rep(1.6*magn,ncols)
  # widths[1] <- widths[2]*0.15
  # heights <- rep(magn,nrows)
  # order <- matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE)

  # ylim <- c(0.05,1)
  # pdf( "fsuit_sage.pdf", width=sum(widths), height=sum(heights) )

  #   panel <- layout(
  #             order,
  #             widths=widths,
  #             heights=heights,
  #             TRUE
  #             )
  #   # layout.show(panel)


  #   ## Color key - not plotting it yet
  #   maxval.max <- 1 
  #   # out.mycolorbar <- mycolorbar( c("wheat3","tomato2","tomato4"), lev, orient="v", plot=FALSE )

  #   par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
  #   out.mycolorbar <- mycolorbar( c("wheat3","green4"), lev, orient="v", plot=TRUE )

  #   par(mar=c(1,0,1,1),xaxs="i", yaxs="i",las=1)
  #   image( 
  #             # suit$dim$LONGITUDE$vals, suit$dim$LATITUDE$vals, 
  #             suit_sage, 
  #             ylim=ylim, 
  #             zlim=lev[1:2], yaxt="n", xaxt="n",
  #             col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
  #             )
  #   # axis(2,at=seq(0,1,1/(length(lat.labels)-1)),labels=lat.labels)
  #   # axis(4,at=seq(0,1,1/(length(lat.labels)-1)),labels=F)
  #   # axis(3,at=seq(0,1,1/(length(lon.labels)-1)),labels=F)
  #   # axis(1,at=seq(0,1,1/(length(lon.labels)-1)),labels=lon.labels)
  #   text(0.03,0.08,"suitable fraction for agriculture (SAGE)",cex=1.5,adj=c(0,0))

  # dev.off()

################
## PERMANENT AGRICULTURE
################
  # ## Read land suitability field (SAGE)
  # nc <- open.ncdf( "/alphadata01/bstocker/data/landuse_data/perm_holoLU2.nc" )
  # perm <- get.var.ncdf(nc, "permanent")
  # time <- nc$dim$TIME$vals
  # close.ncdf(nc)

  # lev <- c(1,2,2)

  # magn <- 4
  # ncols <- 2
  # nrows <- 4
  # widths <- rep(2*magn,ncols)
  # heights <- rep(magn,nrows)
  # heights[1] <- 0.3*heights[2]
  # order <- matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE)

  # ylim <- c(0.15,1)
  # itime <- c( -4000,-2000, 3, 1000, 1700, 2000 )

  # pdf( "perm_holoLU2.pdf", width=sum(widths), height=sum(heights) )

  #   panel <- layout(
  #               order,
  #               widths=widths,
  #               heights=heights,
  #               TRUE
  #               )
  #   # layout.show(panel)

  #   par(mar=c(0,3,0,0),xaxs="i", yaxs="i",las=1)
  #   plot( 0:1, 0:1, type="n", axes=FALSE )
  #   text( 0, 0.2, "permanent agriculture", cex=3.0, adj=c(0,0) )
  #   plot( 0:1, 0:1, type="n", axes=FALSE )

  #   for (i in 1:length(itime)){

  #     par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)

  #     itim <- which.min(abs(time - itime[i]))

  #     ## Color key - not plotting it yet
  #     maxval.max <- 1 
  #     # out.mycolorbar <- mycolorbar( c("wheat3","tomato2","tomato4"), lev, orient="v", plot=FALSE )
  #     out.mycolorbar <- mycolorbar( c("wheat3","orangered3"), lev, orient="v", plot=FALSE )

  #     image( 
  #       # lon.lpx, lat.lpx, 
  #       perm[,,itim], 
  #       ylim=ylim, 
  #       zlim=lev[1:2], yaxt="n", xaxt="n",
  #       col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
  #       )
  #     # axis(2,at=seq(0,1,1/(length(lat.labels)-1)),labels=lat.labels)
  #     # axis(4,at=seq(0,1,1/(length(lat.labels)-1)),labels=F)
  #     # axis(3,at=seq(0,1,1/(length(lon.labels)-1)),labels=F)
  #     # axis(1,at=seq(0,1,1/(length(lon.labels)-1)),labels=lon.labels)
  #     text( 0.03, 0.18, paste(as.character(time[itim]), "AD"),cex=2.0,adj=c(0,0))

  #   }

    
  # dev.off()

# ################
# ## CROPLAND EARLY
# ################
  # fils <- c( 
  #   "landuse_hyde31_final_lpjgr.cdf", 
  #   "landuse_hyde31_upper_lpjgr.cdf",
  #   "landuse_hyde31_concave_lpjgr.cdf", 
  #   "landuse_hyde31_Ruddiman_lpjgr.cdf", 
  #   "landuse_KK11_pastcorr_lpjgr.cdf"
  #   )
  # name_scen <- c(
  #   "HYDE standard",
  #   "HYDE upper",
  #   "HYDE concave",
  #   "HYDE Ruddiman",
  #   "KK11"
  #   )

  # lev <- c(0,0.01,0.03,0.05,0.1,0.15,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
  # itime <- c( -5000,-3000, -1000, 1, 1000 )
  # color <- c("wheat3","green4","yellow","red")
  # out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=FALSE )

  # magn <- 4
  # ncols <- length(fils)+1
  # nrows <- length(itime)+2
  # widths <- rep(1.6*magn,ncols)
  # widths[1] <- 0.2*widths[2]
  # heights <- rep(magn,nrows)
  # heights[1] <- 0.3*heights[2]
  # heights[length(heights)] <- 0.25*heights[2]
  # order <- matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=FALSE)

  # # ylim <- c(0.15,0.95)
  # ylim <- c(0,1)
  # pdf( "fcrop_EARLY_holoLU2_lpjgr.pdf", width=sum(widths), height=sum(heights) )

  # panel <- layout(
  #           order,
  #           widths=widths,
  #           heights=heights,
  #           TRUE
  #           )
  # # layout.show(panel)

  # par( mar=c(1,1,1,1), xaxs="i", yaxs="i", las=1 )
  # plot( 0:1, 0:1, type="n", axes=FALSE )
  # for (i in 1:length(itime)){
  #   itim <- which.min(abs(time - itime[i]))
  #   plot( 0:1, 0:1, type="n", axes=FALSE )
  #   text( 0.8, 0.01, paste( as.character( time[itim]), "AD" ), cex=4.0, adj=c(0,0), srt=90 )
  # }
  # plot( 0:1, 0:1, type="n", axes=FALSE )

  # for (ifil in 1:length(fils)){

  #   ## Read NetCDF file
  #   nc <- open.ncdf( fils[ifil] )
  #   crop <- get.var.ncdf(nc, "crop")
  #   close.ncdf(nc)

  #   par( mar=c(1,1,1,1), xaxs="i", yaxs="i", las=1 )
  #   plot( 0:1, 0:1, type="n", axes=FALSE )
  #   text( 0, 0.2, name_scen[ifil], cex=4.0, adj=c(0,0) )

  #   for (i in 1:length(itime)){

  #     itim <- which.min(abs(time - itime[i]))

  #     par(mar=c(1,1,1,1),xaxs="i", yaxs="i",las=1)
  #     image( 
  #       # lon.lpx, lat.lpx, 
  #       crop[,,itim], 
  #       ylim=ylim, 
  #       zlim=lev[1:2], yaxt="n", xaxt="n",
  #       col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
  #       )
  #   }
  #   par(mar=c(4,1,1,1),xaxs="i", yaxs="i",las=1)
  #   if (ifil==3) {
  #     out.mycolorbar <- mycolorbar( color, lev, orient="h", plot=TRUE, cex.axis=2.0 )
  #   } else {
  #     plot( 0:1, 0:1, type="n", axes=FALSE )
  #   }

  # }

  # dev.off()

################
## CROPLAND LATE
################
  # fils <- c( 
  #   "landuse_hyde31_final_lpjgr.cdf", 
  #   "landuse_hyde31_upper_lpjgr.cdf",
  #   "landuse_hyde31_concave_lpjgr.cdf", 
  #   "landuse_hyde31_Ruddiman_lpjgr.cdf", 
  #   "landuse_KK11_pastcorr_lpjgr.cdf"
  #   )
  # name_scen <- c(
  #   "HYDE standard",
  #   "HYDE upper",
  #   "HYDE concave",
  #   "HYDE Ruddiman",
  #   "KK11"
  #   )

  # lev <- c(0,1,10)
  # color <- c("wheat3","green4","yellow","red")
  # out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=FALSE )
  # itime <- c( 1500, 1700, 1850, 2000 )

  # magn <- 4
  # ncols <- length(fils)+1
  # nrows <- length(itime)+2
  # widths <- rep(1.6*magn,ncols)
  # widths[1] <- 0.2*widths[2]
  # heights <- rep(magn,nrows)
  # heights[1] <- 0.3*heights[2]
  # heights[length(heights)] <- 0.25*heights[2]
  # order <- matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=FALSE)

  # # ylim <- c(0.15,0.95)
  # ylim <- c(0,1)
  # pdf( "fcrop_LATE_holoLU2_lpjgr.pdf", width=sum(widths), height=sum(heights) )

  # panel <- layout(
  #           order,
  #           widths=widths,
  #           heights=heights,
  #           TRUE
  #           )
  # # layout.show(panel)

  # par( mar=c(1,1,1,1), xaxs="i", yaxs="i", las=1 )
  # plot( 0:1, 0:1, type="n", axes=FALSE )
  # for (i in 1:length(itime)){
  #   itim <- which.min(abs(time - itime[i]))
  #   plot( 0:1, 0:1, type="n", axes=FALSE )
  #   text( 0.8, 0.01, paste( as.character( time[itim]), "AD" ), cex=4.0, adj=c(0,0), srt=90 )
  # }
  # plot( 0:1, 0:1, type="n", axes=FALSE )

  # for (ifil in 1:length(fils)){

  #   ## Read NetCDF file
  #   nc <- open.ncdf( fils[ifil] )
  #   crop <- get.var.ncdf(nc, "crop")
  #   close.ncdf(nc)

  #     ## Color key - not plotting it yet
  #     par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
  #     out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=FALSE )

  #     par( mar=c(1,1,1,1), xaxs="i", yaxs="i", las=1 )
  #     plot( 0:1, 0:1, type="n", axes=FALSE )
  #     text( 0, 0.2, name_scen[ifil], cex=4.0, adj=c(0,0) )

  #     for (i in 1:length(itime)){

  #       itim <- which.min(abs(time - itime[i]))

  #       par(mar=c(1,1,1,1),xaxs="i", yaxs="i",las=1)
  #       image( 
  #         crop[,,itim], 
  #         ylim=ylim, 
  #         zlim=lev[1:2], yaxt="n", xaxt="n",
  #         col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
  #         )

  #     }
  #     par(mar=c(4,1,1,1),xaxs="i", yaxs="i",las=1)
  #     if (ifil==3) {
  #       out.mycolorbar <- mycolorbar( color, lev, orient="h", plot=TRUE, cex.axis=2.0 )
  #     } else {
  #       plot( 0:1, 0:1, type="n", axes=FALSE )
  #     }
    
  # }

  # dev.off()

################
## LUC in periods: HYDE vs. KK11
################
  fils <- c( 
            "landuse_hyde31_final_halfdeg.cdf", 
            "landuse_KK11_halfdeg_hydeslices.nc",
            "landuse_KK11delayed_halfdeg_hydeslices.nc"
            )
  name_scen <- c(
                  "HYDE standard",
                  "KK11",
                  "KK11d"
                  )

  ## read files
  nc <- open.ncdf( fils[1] )
  crop_hyde <- get.var.ncdf(nc, "crop")
  past_hyde <- get.var.ncdf(nc, "past")
  time <- get.var.ncdf(nc, "TIME")
  close.ncdf(nc)

  nc <- open.ncdf( fils[2] )
  crop_kk11 <- get.var.ncdf(nc, "crop")
  past_kk11 <- get.var.ncdf(nc, "past")
  close.ncdf(nc)

  nc <- open.ncdf( fils[3] )
  crop_kk11d <- get.var.ncdf(nc, "crop")
  past_kk11d <- get.var.ncdf(nc, "past")
  close.ncdf(nc)

  magn <- 4
  ncols <- 4
  nrows <- 1
  widths <- rep(1.6*magn,ncols)
  widths[1] <- 0.15*widths[2]
  heights <- rep(magn,nrows)
  order <- matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=FALSE)

  ylim <- c(0.05,1)

  ## HOLOCENE PERIODS
  ## Alternativ: fixe Margins
    periodsBP <- c(-11000,-9000,-7000,-5000,-3000,-1000)
    periodsAD <- periodsBP + 1950
    periodsName <- c("11-9","9-7","7-5","5-3","3-1")

    periodsAD[1] <- -8990 ## 11 kyr BP is to be represented by -8990 AD = 10940 BP
    periodsBP <- periodsAD - 1950
    period_margins <- periodsAD
    nper <- length(period_margins)-1

    for (i in 1:nper){

      ## interpolate cropland area to period margin
      myinextlower <- inextlower( time, period_margins[i])
      myinextupper <- inextupper( time, period_margins[i])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_hyde[,,myinextlower] #+ past_hyde[,,myinextlower]
      field1 <- crop_hyde[,,myinextupper] #+ past_hyde[,,myinextupper]
      field_hyde_0 <- interpol_field( time0, time1, field0, field1, period_margins[i] )

      myinextlower <- inextlower( time, period_margins[i+1])
      myinextupper <- inextupper( time, period_margins[i+1])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_hyde[,,myinextlower] #+ past_hyde[,,myinextlower]
      field1 <- crop_hyde[,,myinextupper] #+ past_hyde[,,myinextupper]
      field_hyde_1 <- interpol_field( time0, time1, field0, field1, period_margins[i+1] )

      myinextlower <- inextlower( time, period_margins[i])
      myinextupper <- inextupper( time, period_margins[i])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_kk11[,,myinextlower] #+ past_kk11[,,myinextlower]
      field1 <- crop_kk11[,,myinextupper] #+ past_kk11[,,myinextupper]
      field_kk11_0 <- interpol_field( time0, time1, field0, field1, period_margins[i] )
      print(paste("for first field, using interpolation between", time0, "and", time1, "to", period_margins[i]))

      myinextlower <- inextlower( time, period_margins[i+1])
      myinextupper <- inextupper( time, period_margins[i+1])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_kk11[,,myinextlower] #+ past_kk11[,,myinextlower]
      field1 <- crop_kk11[,,myinextupper] #+ past_kk11[,,myinextupper]
      field_kk11_1 <- interpol_field( time0, time1, field0, field1, period_margins[i+1] )
      print(paste("for second field, using interpolation between", time0, "and", time1, "to", period_margins[i+1]))

      myinextlower <- inextlower( time, period_margins[i])
      myinextupper <- inextupper( time, period_margins[i])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_kk11d[,,myinextlower] #+ past_kk11[,,myinextlower]
      field1 <- crop_kk11d[,,myinextupper] #+ past_kk11[,,myinextupper]
      field_kk11d_0 <- interpol_field( time0, time1, field0, field1, period_margins[i] )
      print(paste("for first field, using interpolation between", time0, "and", time1, "to", period_margins[i]))

      myinextlower <- inextlower( time, period_margins[i+1])
      myinextupper <- inextupper( time, period_margins[i+1])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_kk11d[,,myinextlower] #+ past_kk11[,,myinextlower]
      field1 <- crop_kk11d[,,myinextupper] #+ past_kk11[,,myinextupper]
      field_kk11d_1 <- interpol_field( time0, time1, field0, field1, period_margins[i+1] )
      print(paste("for second field, using interpolation between", time0, "and", time1, "to", period_margins[i+1]))

      lev <- c(-1, -0.5, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.5, 1)
      color <- c("springgreen4","springgreen","wheat3","wheat3","orange","red")
      filn <- paste( "dLUC_map_holocene_p",i,".pdf", sep="" )
      pdf( filn, width=sum(widths), height=sum(heights) )

        panel <- layout(
                  order,
                  widths=widths,
                  heights=heights,
                  TRUE
                  )
        # layout.show(panel)

        ## Color key
        par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
        out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

        par(mar=c(1,1,1,1),xaxs="i", yaxs="i",las=1)
        image(
              field_hyde_1 - field_hyde_0,
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(0.03,0.08,paste("change in cropland area, HYDE,",periodsName[i],"kyr BP"),cex=1.5,adj=c(0,0))

        image( 
              field_kk11_1 - field_kk11_0, 
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(0.03,0.08,paste("change in cropland area, KK11,",periodsName[i],"kyr BP"),cex=1.5,adj=c(0,0))

        image( 
              field_kk11d_1 - field_kk11d_0, 
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(0.03,0.08,paste("change in cropland area, KK11-delayed,",periodsName[i],"kyr BP"),cex=1.5,adj=c(0,0))

      dev.off()


      lev <- c(0, 0.01, 0.1, 0.15, 0.2, 0.4, 0.7, 1)
      color <- c("wheat3","green4","yellow","orange","red")
      filn <- paste( "LUC_map_holocene_p",as.character(-periodsBP[i+1]),".pdf", sep="" )
      pdf( filn, width=sum(widths), height=sum(heights) )

        panel <- layout(
                  order,
                  widths=widths,
                  heights=heights,
                  TRUE
                  )
        # layout.show(panel)

        ## Color key
        par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
        out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

        image(
              field_hyde_1,
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(0.03,0.08,paste("cropland area, HYDE,",as.character(-periodsBP[i+1]),"yr BP"),cex=1.5,adj=c(0,0))

        image( 
              field_kk11_1, 
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(0.03,0.08,paste("cropland area, KK11,",as.character(-periodsBP[i+1]),"yr BP"),cex=1.5,adj=c(0,0))

        image( 
              field_kk11d_1, 
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(0.03,0.08,paste("cropland area, KK11-delayed,",as.character(-periodsBP[i+1]),"yr BP"),cex=1.5,adj=c(0,0))

      dev.off()

      if (i==1){

        filn <- paste( "LUC_map_holocene_p",as.character(-periodsBP[i]),".pdf", sep="" )
        pdf( filn, width=sum(widths), height=sum(heights) )

          panel <- layout(
                    order,
                    widths=widths,
                    heights=heights,
                    TRUE
                    )
          # layout.show(panel)

          ## Color key
          par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
          out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

          image(
                field_hyde_0,
                ylim=ylim, 
                zlim=range(lev), yaxt="n", xaxt="n",
                col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
                )
          text(0.03,0.08,paste("cropland area, HYDE,",as.character(-periodsBP[i]),"yr BP"),cex=1.5,adj=c(0,0))

          image( 
                field_kk11_0, 
                ylim=ylim, 
                zlim=range(lev), yaxt="n", xaxt="n",
                col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
                )
          text(0.03,0.08,paste("cropland area, KK11,",as.character(-periodsBP[i]),"yr BP"),cex=1.5,adj=c(0,0))

          image( 
                field_kk11d_0, 
                ylim=ylim, 
                zlim=range(lev), yaxt="n", xaxt="n",
                col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
                )
          text(0.03,0.08,paste("cropland area, KK11-delayed,",as.character(-periodsBP[i]),"yr BP"),cex=1.5,adj=c(0,0))

        dev.off()

      }


    }

  ## LAST MILLENNIUM PERIODS
  ## Alternativ: fixe Margins
    # period_margins <- c( 760, 960, 1200, 1460, 1530, 1760, 1920 )
    period_margins <- c( 760, 960, 1200, 1500, 1650, 1760, 1920 ) ## use this
    periodsName <- paste( as.character( period_margins[1:6]), "-", as.character( period_margins[2:7]) )
    nper <- length(period_margins)-1

    for (i in 1:6){

      ## interpolate cropland area to period margin
      myinextlower <- inextlower( time, period_margins[i])
      myinextupper <- inextupper( time, period_margins[i])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_hyde[,,myinextlower] #+ past_hyde[,,myinextlower]
      field1 <- crop_hyde[,,myinextupper] #+ past_hyde[,,myinextupper]
      field_hyde_0 <- interpol_field( time0, time1, field0, field1, period_margins[i] )

      myinextlower <- inextlower( time, period_margins[i+1])
      myinextupper <- inextupper( time, period_margins[i+1])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_hyde[,,myinextlower] #+ past_hyde[,,myinextlower]
      field1 <- crop_hyde[,,myinextupper] #+ past_hyde[,,myinextupper]
      field_hyde_1 <- interpol_field( time0, time1, field0, field1, period_margins[i+1] )

      myinextlower <- inextlower( time, period_margins[i])
      myinextupper <- inextupper( time, period_margins[i])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_kk11[,,myinextlower] #+ past_kk11[,,myinextlower]
      field1 <- crop_kk11[,,myinextupper] #+ past_kk11[,,myinextupper]
      field_kk11_0 <- interpol_field( time0, time1, field0, field1, period_margins[i] )
      print(paste("for first field, using interpolation between", time0, "and", time1, "to", period_margins[i]))

      myinextlower <- inextlower( time, period_margins[i+1])
      myinextupper <- inextupper( time, period_margins[i+1])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_kk11[,,myinextlower] #+ past_kk11[,,myinextlower]
      field1 <- crop_kk11[,,myinextupper] #+ past_kk11[,,myinextupper]
      field_kk11_1 <- interpol_field( time0, time1, field0, field1, period_margins[i+1] )
      print(paste("for second field, using interpolation between", time0, "and", time1, "to", period_margins[i+1]))

      lev <- c(-1, -0.5, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.5, 1)
      color <- c("springgreen4","springgreen","wheat3","wheat3","orange","red")
      filn <- paste( "dLUC_map_lastmill_p",i,".pdf", sep="" )
      pdf( filn, width=sum(widths), height=sum(heights) )

        panel <- layout(
                  order,
                  widths=widths,
                  heights=heights,
                  TRUE
                  )
        # layout.show(panel)

        ## Color key
        par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
        out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

        par(mar=c(1,1,1,1),xaxs="i", yaxs="i",las=1)
        image(
              field_hyde_1 - field_hyde_0,
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(0.03,0.08,paste("change in cropland area, HYDE,",periodsName[i],"yr AD"),cex=1.5,adj=c(0,0))
        image( 
              field_kk11_1 - field_kk11_0, 
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(0.03,0.08,paste("change in cropland area, KK11,",periodsName[i],"yr AD"),cex=1.5,adj=c(0,0))

      dev.off()

      lev <- c(0, 0.01, 0.1, 0.15, 0.2, 0.4, 0.7, 1)
      color <- c("wheat3","green4","yellow","orange","red")
      filn <- paste( "LUC_map_lastmill_p",as.character(period_margins[i+1]),".pdf", sep="" )
      pdf( filn, width=sum(widths), height=sum(heights) )

        panel <- layout(
                  order,
                  widths=widths,
                  heights=heights,
                  TRUE
                  )
        # layout.show(panel)

        ## Color key
        par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
        out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

        par(mar=c(1,1,1,1),xaxs="i", yaxs="i",las=1)
        image(
              field_hyde_1,
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(0.03,0.08,paste("cropland area, HYDE,",as.character(period_margins[i+1]),"yr AD"),cex=1.5,adj=c(0,0))
        image( 
              field_kk11_1, 
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(0.03,0.08,paste("cropland area, KK11,",as.character(period_margins[i+1]),"yr AD"),cex=1.5,adj=c(0,0))

      dev.off()

      if (i==1){

        filn <- paste( "LUC_map_lastmill_p",as.character(period_margins[i]),".pdf", sep="" )
        pdf( filn, width=sum(widths), height=sum(heights) )

          panel <- layout(
                    order,
                    widths=widths,
                    heights=heights,
                    TRUE
                    )
          # layout.show(panel)

          ## Color key
          par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
          out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

          par(mar=c(1,1,1,1),xaxs="i", yaxs="i",las=1)
          image(
                field_hyde_0,
                ylim=ylim, 
                zlim=range(lev), yaxt="n", xaxt="n",
                col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
                )
          text(0.03,0.08,paste("cropland area, HYDE,",as.character(period_margins[i]),"yr AD"),cex=1.5,adj=c(0,0))
          image( 
                field_kk11_0, 
                ylim=ylim, 
                zlim=range(lev), yaxt="n", xaxt="n",
                col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
                )
          text(0.03,0.08,paste("cropland area, KK11,",as.character(period_margins[i]),"yr AD"),cex=1.5,adj=c(0,0))

        dev.off()

      }

    }


################
## OPEN VEGETATION
################
  # ## Read land suitability field (SAGE)
  # nc <- open.ncdf( "/alphadata01/bstocker/data/landuse_data/fopen_r0_holoLU2.nc" )
  # fopen <- get.var.ncdf(nc, "FOPEN")
  # close.ncdf(nc)

  # lev <- c(0,1,10)

  # magn <- 4
  # ncols <- 2
  # nrows <- 1
  # widths <- rep(1.6*magn,ncols)
  # widths[1] <- widths[2]*0.15
  # heights <- rep(magn,nrows)
  # order <- matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE)

  # ylim <- c(0,1)
  # pdf( "fopen_r0_holoLU2.pdf", width=sum(widths), height=sum(heights) )

  #   panel <- layout(
  #             order,
  #             widths=widths,
  #             heights=heights,
  #             TRUE
  #             )
  #   # layout.show(panel)


  #   ## Color key - not plotting it yet
  #   maxval.max <- 1 
  #   # out.mycolorbar <- mycolorbar( c("wheat3","tomato2","tomato4"), lev, orient="v", plot=FALSE )

  #   par(mar=c(1,3,1,1), xaxs="i", yaxs="i", las=1 )
  #   out.mycolorbar <- mycolorbar( c("wheat3","tomato2","tomato3"), lev, orient="v", plot=TRUE )

  #   par(mar=c(1,0,1,1), xaxs="i", yaxs="i", las=1 )
  #   image( 
  #     # lon.lpx, lat.lpx, 
  #     fopen, 
  #     ylim=ylim, 
  #     zlim=lev[1:2], yaxt="n", xaxt="n",
  #     col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
  #     )
  #   # axis(2,at=seq(0,1,1/(length(lat.labels)-1)),labels=lat.labels)
  #   # axis(4,at=seq(0,1,1/(length(lat.labels)-1)),labels=F)
  #   # axis(3,at=seq(0,1,1/(length(lon.labels)-1)),labels=F)
  #   # axis(1,at=seq(0,1,1/(length(lon.labels)-1)),labels=lon.labels)
  #   text(0.03,0.03,"open vegetation fraction (LPX)",cex=1.5,adj=c(0,0))

  # dev.off()


# ################
# ## PASTURE FRACTION
# ################
#   ## Read file
#   nc <- open.ncdf( "/alphadata01/bstocker/holoLU2/fpast_STAGE1_hyde31_final_halfdeg.nc" )
#   fpast <- get.var.ncdf(nc, "fpast")
#   close.ncdf(nc)

#   magn <- 4
#   ncols <- 2
#   nrows <- 1
#   widths <- rep(1.6*magn,ncols)
#   widths[1] <- widths[2]*0.15
#   heights <- rep(magn,nrows)
#   order <- matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE)

#   itim <- which.min(abs(time - 1850))
#   out <- fpast[,,itim]

#   ylim <- c(0.05,1)
#   pdf( "fpast_hyde_halfdeg.pdf", width=sum(widths), height=sum(heights) )

#     panel <- layout(
#               order,
#               widths=widths,
#               heights=heights,
#               TRUE
#               )
#     # layout.show(panel)


#     ## Color key - not plotting it yet
#     maxval.max <- 1 
#     # out.mycolorbar <- mycolorbar( c("wheat3","tomato2","tomato4"), lev, orient="v", plot=FALSE )

#     par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)

#     ## Color key
#     lev <- seq(0.0, 1.0, 0.1)
#     colors <- colorRampPalette( c("wheat3","tomato2","tomato4") )( length(lev) )
#     colors <- rev(colors)
#     colors <- colorRampPalette( c("grey80",colors) )( length(lev)+1 )

#     out.mycolorbar <- mycolorbar( colors, c(-1,lev), orient="v", plot=TRUE )
#     out[is.na(suit_sage)] <- -0.99

#     par(mar=c(1,0,1,1),xaxs="i", yaxs="i",las=1)
#     image( 
#       out, 
#       ylim=ylim, 
#       zlim=lev[1:2], yaxt="n", xaxt="n",
#       col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
#       )
#     text(0.03,0.08,"pasture fraction from HYDE, 1850 AD",cex=1.5,adj=c(0,0))

#   dev.off()


#   lon <- seq(-179.75, 179.75, 0.5)
#   lat <- seq(-89.75, 89.75, 0.5)

#   ylim <- c(0.05,1)
#   pdf( "fpast_hyde_halfdeg_alternativeplot.pdf", width=sum(widths), height=sum(heights) )

#     maxval.max <- 1 

#     par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)

#     ## Color key
#     lev <- seq(0.0, 1.0, 0.1)
#     colors <- colorRampPalette( c("wheat3","tomato2","tomato4") )( length(lev) )
#     colors <- rev( colors )

#     out.mycolorbar <- mycolorbar( colors, c(-1,lev), orient="v", plot=FALSE )
#     out <- fpast[,,itim]
#     # out[is.na(suit_sage)] <- -0.99

#     par(mar=c(3,3,1,1),xaxs="i", yaxs="i",las=1)
#     # image.plot( 
#     #           lon, lat, out,
#     #           ylim=ylim, zlim=c(0,1),
#     #           yaxt="n", xaxt="n",
#     #           col=out.mycolorbar$colors
#     #           )
#     image.plot( 
#               lon, lat, out,
#               col=out.mycolorbar$colors,
#               ylim=c(-60,85),
#               cex.axis=0.7
#               # , yaxt="n", xaxt="n"
#               )
#     # data(wrld_simpl)
#     # plot(wrld_simpl, add = TRUE)

#     wbuf <- gBuffer(wrld_simpl,width=0.00001)
#     plot(wbuf, add = TRUE, lwd=0.7)

#     # text(0.03,0.08,"pasture fraction from HYDE, 1850 AD",cex=1.5,adj=c(0,0))

#   dev.off()


