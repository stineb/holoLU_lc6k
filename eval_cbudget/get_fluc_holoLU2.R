source('/alphadata01/bstocker/lpx/lpxtools/trunk/landuse/get.f.luc.R')
source('/alphadata01/bstocker/lpx/lpxtools/trunk/landuse/cdf.write.R')

netcdf <- TRUE

## ////////////////////////////////////////////////////////////////////////
## HOLOLU2
## ------------------------------------------------------------------------
if (netcdf) {
  cdfstr <- ""
  dir <- '/alphadata01/bstocker/output_netcdf_holoLU2/'
} else {
  cdfstr <- "_fromASCII"
  # dir <- '/alphadata01/bstocker/output_gcp2013_for_holoLU2/'
  dir <- '/alphadata01/bstocker/output_holoLU2/'
}

name.nolu   <- "r0_holoLU2"
name.lu.vec <- c( "r1_holoLU2", "r2_holoLU2", "r3_holoLU2", "r7_holoLU2", "r8_holoLU2", "r9_holoLU2" )

for (iscen in 1:length(name.lu.vec)){

  name.lu <- name.lu.vec[iscen]

  ## get LUC emissions
  out.f.luc <- get.f.luc( name.nolu, name.lu, dir, netcdf=netcdf, byflux=FALSE )

  ## annual output to netcdf
  write.table(
              out.f.luc$tseries[,1:2],
              file=paste("/alphadata01/bstocker/holoLU2/fluc_",name.lu,".dat",sep=""),
              row.names=FALSE, col.names=FALSE
              )

  if (netcdf) {
    cdf.write( out.f.luc$field, "fLUC",
              out.f.luc$lon, out.f.luc$lat,
              paste("/alphadata01/bstocker/holoLU2/fluc_",name.lu,".nc",sep=""),
              time=out.f.luc$tseries$time, make.tdim=TRUE,
              verbose=TRUE
              )
    cdf.write( out.f.luc$field_luarea, "luarea",
              out.f.luc$lon, out.f.luc$lat,
              paste("/alphadata01/bstocker/holoLU2/luarea_",name.lu,".nc",sep=""),
              time=out.f.luc$tseries$time, make.tdim=TRUE,
              make.zdim=TRUE, z_dim=1:5,
              verbose=TRUE
              )
    save(out.f.luc,file="fLUC_m.Rdata")
  }

}




# ## ////////////////////////////////////////////////////////////////////////
# ## GCP TEST
# ## ------------------------------------------------------------------------
# if (netcdf) {
#   cdfstr <- ""
#   dir <- '/alphadata01/bstocker/output_netcdf_gcp2013/'
# } else {
#   cdfstr <- "_fromASCII"
#   dir <- '/alphadata01/bstocker/output_gcp2013/'
# }

# name.simset <- 'gcp2013_v12'
# name.nolu <- paste('s2_historical_',name.simset,'_1x1deg',sep="")
# name.lu   <- paste('s3_historical_',name.simset,'_1x1deg',sep="")

# ## get LUC emissions
# out.f.luc <- get.f.luc( name.nolu, name.lu, dir, netcdf=netcdf, byflux=TRUE )

# ## annual output to netcdf
# write.table(
#             out.f.luc$tseries[,1:2],
#             file=paste("/alphadata01/bstocker/holoLU2/test_fluc_gcp.dat",sep=""),
#             row.names=FALSE, col.names=FALSE
#             )

# if (netcdf) {
#   cdf.write( out.f.luc$field, "fLUC",
#             out.f.luc$lon, out.f.luc$lat,
#             paste("/alphadata01/bstocker/holoLU2/test_fluc_gcp.nc",sep=""),
#             time=out.f.luc$tseries$time, make.tdim=TRUE,
#             verbose=TRUE
#             )
#   save(out.f.luc,file="fLUC_m.Rdata")
# }


