source('/alphadata01/bstocker/lpx/lpxtools/trunk/landuse/get.f.luc.R')
source('/alphadata01/bstocker/lpx/lpxtools/trunk/landuse/cdf.write.R')

#dir <- '/alphadata01/bstocker/output_holoLU2/'
#dir <- '/alphadata01/bstocker/output_trunk/'
dir <- '/alphadata01/bstocker/output_gcp2013_for_holoLU2/'

# get.fluc.holoLU2 <- function(name.lu){

name.nolu <- "r0_holoLU2"
#   name.lu   <- "test"
#    name.lu   <- "r1_holoLU2"

# name.nolu <- "s2_historical_gcp2013_v12_1x1deg"
# name.lu   <- "s3_historical_gcp2013_v12_1x1deg"

## get LUC emissions
name.lu   <- "r1_holoLU2"
out.f.luc.r1.byflux <- get.f.luc( name.nolu, name.lu, dir, netcdf=FALSE, byflux=TRUE, tstart=-10000, tend=2001 )
out.f.luc.r1.bytotc <- get.f.luc( name.nolu, name.lu, dir, netcdf=FALSE, byflux=FALSE, tstart=-10000, tend=2001 )

name.lu   <- "r2_holoLU2"
out.f.luc.r2.byflux <- get.f.luc( name.nolu, name.lu, dir, netcdf=FALSE, byflux=TRUE, tstart=-10000, tend=2001 )
out.f.luc.r2.bytotc <- get.f.luc( name.nolu, name.lu, dir, netcdf=FALSE, byflux=FALSE, tstart=-10000, tend=2001 )

# ## annual output to table
# write.table(
#             out.f.luc.r1.byflux$tseries[,1:2],
#             file=paste("/alphadata01/bstocker/holoLU2/fluc_",name.lu,".txt",sep=""),
#             row.names=FALSE, col.names=FALSE
#             )
# tmp <- out.f.luc.r1.byflux$tseries[,1:2]
# tmp[,2] <- cumsum(tmp[,2])
# write.table(
#             tmp[,1:2],
#             file=paste("/alphadata01/bstocker/holoLU2/cumfluc_",name.lu,".txt",sep=""),
#             row.names=FALSE, col.names=FALSE
#             )

# if (netcdf) {
#   cdf.write( out.f.luc.r1.byflux$field, "fluc",
#             out.f.luc.r1.byflux$lon, out.f.luc.r1$lat,
#             paste("/alphadata01/bstocker/holoLU2/fluc_",name.lu,".nc",sep=""),
#             time=out.f.luc.r1$tseries$time, make.tdim=TRUE,
#             verbose=TRUE
#             )
#   save(out.f.luc.r1,file=paste("fluc_",name.lu,".Rdata",sep=""))
# }

# }
# 
# name.lu <- c("r1_holoLU2","r2_holoLU2","r3_holoLU2","r4_holoLU2","r5_holoLU2")
# 
# for (idx in name.lu){
#   get.fluc.holoLU2(idx)
# }

## Test:
plot(out.f.luc.r1.byflux$tseries$time, cumsum(out.f.luc.r1.byflux$tseries$f.luc), type="l")
lines(out.f.luc.r2.byflux$tseries$time, cumsum(out.f.luc.r2.byflux$tseries$f.luc), type="l", col="red" )

plot(out.f.luc.r1.bytotc$tseries$time, cumsum(out.f.luc.r1.bytotc$tseries$f.luc), type="l" )
lines(out.f.luc.r2.bytotc$tseries$time, cumsum(out.f.luc.r2.bytotc$tseries$f.luc), type="l", col="red" )

################################################
### GCP OUTPUT
dir <- '/alphadata01/bstocker/output_gcp2013/'
name.nolu <- "s2_historical_gcp2013_v12_1x1deg"
name.lu   <- "s3_historical_gcp2013_v12_1x1deg"

## get LUC emissions
out.f.luc.gcp.byflux <- get.f.luc( name.nolu, name.lu, dir, netcdf=FALSE, byflux=TRUE )
out.f.luc.gcp.bytotc <- get.f.luc( name.nolu, name.lu, dir, netcdf=FALSE, byflux=FALSE )


## Test:
plot(out.f.luc.gcp.byflux$tseries$time, (out.f.luc.gcp.byflux$tseries$f.luc), type="l" )
plot(out.f.luc.gcp.bytotc$tseries$time, (out.f.luc.gcp.bytotc$tseries$f.luc), type="l" )


