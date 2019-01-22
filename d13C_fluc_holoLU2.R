options(digits=16)

r0 <- read.table( "../output_holoLU2/trans_r0_holoLU2.totc.out", col.names=c("year", "totc", "prim" ) )
r1 <- read.table( "../output_holoLU2/trans_r1_holoLU2.totc.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )
r2 <- read.table( "../output_holoLU2/trans_r2_holoLU2.totc.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )
r3 <- read.table( "../output_holoLU2/trans_r3_holoLU2.totc.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )
r4 <- read.table( "../output_holoLU2/trans_r4_holoLU2.totc.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )
r5 <- read.table( "../output_holoLU2/trans_r5_holoLU2.totc.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )
r1n <- read.table( "../output_holoLU2/trans_r1nholoLU2.totc.out", col.names=c("year", "totc", "prim", "crop", "past", "built") )

## remove columns
r0$prim <- NULL
r1$prim <- NULL
r2$prim <- NULL
r3$prim <- NULL
r4$prim <- NULL
r5$prim <- NULL
# r1n$prim <- NULL

r1$secd <- NULL
r2$secd <- NULL
r3$secd <- NULL
r4$secd <- NULL
r5$secd <- NULL
# r1n$secd <- NULL

r1$crop <- NULL
r2$crop <- NULL
r3$crop <- NULL
r4$crop <- NULL
r5$crop <- NULL
# r1n$crop <- NULL

r1$past <- NULL
r2$past <- NULL
r3$past <- NULL
r4$past <- NULL
r5$past <- NULL
# r1n$past <- NULL

r1$built <- NULL
r2$built <- NULL
r3$built <- NULL
r4$built <- NULL
r5$built <- NULL
# r1n$built <- NULL

r0$totc13 <- read.table( "../output_holoLU2/trans_r0_holoLU2.totc13.out", col.names=c("year", "totc", "prim") )$totc
r1$totc13 <- read.table( "../output_holoLU2/trans_r1_holoLU2.totc13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r2$totc13 <- read.table( "../output_holoLU2/trans_r2_holoLU2.totc13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r3$totc13 <- read.table( "../output_holoLU2/trans_r3_holoLU2.totc13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r4$totc13 <- read.table( "../output_holoLU2/trans_r4_holoLU2.totc13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r5$totc13 <- read.table( "../output_holoLU2/trans_r5_holoLU2.totc13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
# r1n$totc13 <- read.table( "../output_holoLU2/trans_r1nholoLU2.totc13.out", col.names=c("year", "totc", "prim", "crop", "past", "built") )$totc

r0$in13 <- read.table( "../output_holoLU2/trans_r0_holoLU2.npp13.out", col.names=c("year", "totc", "prim") )$totc
r1$in13 <- read.table( "../output_holoLU2/trans_r1_holoLU2.npp13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r2$in13 <- read.table( "../output_holoLU2/trans_r2_holoLU2.npp13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r3$in13 <- read.table( "../output_holoLU2/trans_r3_holoLU2.npp13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r4$in13 <- read.table( "../output_holoLU2/trans_r4_holoLU2.npp13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r5$in13 <- read.table( "../output_holoLU2/trans_r5_holoLU2.npp13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
# r1n$in13 <- read.table( "../output_holoLU2/trans_r1nholoLU2.npp13.out", col.names=c("year", "totc", "prim", "crop", "past", "built") )$totc

r0$out13 <- read.table( "../output_holoLU2/trans_r0_holoLU2.rh13.out", col.names=c("year", "totc", "prim") )$totc
r1$out13 <- read.table( "../output_holoLU2/trans_r1_holoLU2.rh13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r2$out13 <- read.table( "../output_holoLU2/trans_r2_holoLU2.rh13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r3$out13 <- read.table( "../output_holoLU2/trans_r3_holoLU2.rh13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r4$out13 <- read.table( "../output_holoLU2/trans_r4_holoLU2.rh13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r5$out13 <- read.table( "../output_holoLU2/trans_r5_holoLU2.rh13.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
# r1n$out13 <- read.table( "../output_holoLU2/trans_r1nholoLU2.rh13.out", col.names=c("year", "totc", "prim", "crop", "past", "built") )$totc

r0$in12 <- read.table( "../output_holoLU2/trans_r0_holoLU2.npp.out", col.names=c("year", "totc", "prim") )$totc
r1$in12 <- read.table( "../output_holoLU2/trans_r1_holoLU2.npp.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r2$in12 <- read.table( "../output_holoLU2/trans_r2_holoLU2.npp.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r3$in12 <- read.table( "../output_holoLU2/trans_r3_holoLU2.npp.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r4$in12 <- read.table( "../output_holoLU2/trans_r4_holoLU2.npp.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r5$in12 <- read.table( "../output_holoLU2/trans_r5_holoLU2.npp.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
# r1n$in12 <- read.table( "../output_holoLU2/trans_r1nholoLU2.npp.out", col.names=c("year", "totc", "prim", "crop", "past", "built") )$totc

r0$out12 <- read.table( "../output_holoLU2/trans_r0_holoLU2.rh.out", col.names=c("year", "totc", "prim") )$totc
r1$out12 <- read.table( "../output_holoLU2/trans_r1_holoLU2.rh.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r2$out12 <- read.table( "../output_holoLU2/trans_r2_holoLU2.rh.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r3$out12 <- read.table( "../output_holoLU2/trans_r3_holoLU2.rh.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r4$out12 <- read.table( "../output_holoLU2/trans_r4_holoLU2.rh.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r5$out12 <- read.table( "../output_holoLU2/trans_r5_holoLU2.rh.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
# r1n$out12 <- read.table( "../output_holoLU2/trans_r1nholoLU2.rh.out", col.names=c("year", "totc", "prim", "crop", "past", "built") )$totc

r0$nep12 <- read.table( "../output_holoLU2/trans_r0_holoLU2.nep.out", col.names=c("year", "totc", "prim") )$totc
r1$nep12 <- read.table( "../output_holoLU2/trans_r1_holoLU2.nep.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r2$nep12 <- read.table( "../output_holoLU2/trans_r2_holoLU2.nep.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r3$nep12 <- read.table( "../output_holoLU2/trans_r3_holoLU2.nep.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r4$nep12 <- read.table( "../output_holoLU2/trans_r4_holoLU2.nep.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
r5$nep12 <- read.table( "../output_holoLU2/trans_r5_holoLU2.nep.out", col.names=c("year", "totc", "prim", "secd", "crop", "past", "built") )$totc
# r1n$nep12 <- read.table( "../output_holoLU2/trans_r1nholoLU2.nep.out", col.names=c("year", "totc", "prim", "crop", "past", "built") )$totc

istart <- which.min( abs( r1$year - (-10000) ) )
iend   <- length( r1$year )

r0 <- r0[ istart:iend, ]
r1 <- r1[ istart:iend, ]
r2 <- r2[ istart:iend, ]
r3 <- r3[ istart:iend, ]
r4 <- r4[ istart:iend, ]
r5 <- r5[ istart:iend, ]
# r1n <- r1n[ istart:iend, ]

# r0$diffC <- r0$totc*0
# r1$diffC <- r1$totc*0
# r2$diffC <- r1$totc*0
# r3$diffC <- r1$totc*0
# r4$diffC <- r1$totc*0
# r5$diffC <- r1$totc*0
# # r1n$diffC <- r1$totc*0

# r0$diffC[2:dim(r0)[1]] <-  r0$totc[1:(dim(r0)[1]-1)] - r0$totc[2:dim(r0)[1]]
# r1$diffC[2:dim(r1)[1]] <-  r1$totc[1:(dim(r1)[1]-1)] - r1$totc[2:dim(r1)[1]]
# r2$diffC[2:dim(r1)[1]] <-  r2$totc[1:(dim(r1)[1]-1)] - r2$totc[2:dim(r1)[1]]
# r3$diffC[2:dim(r1)[1]] <-  r3$totc[1:(dim(r1)[1]-1)] - r3$totc[2:dim(r1)[1]]
# r4$diffC[2:dim(r1)[1]] <-  r4$totc[1:(dim(r1)[1]-1)] - r4$totc[2:dim(r1)[1]]
# r5$diffC[2:dim(r1)[1]] <-  r5$totc[1:(dim(r1)[1]-1)] - r5$totc[2:dim(r1)[1]]
# # r1n$diffC[2:dim(r1)[1]] <-  r1n$totc[1:(dim(r1)[1]-1)] - r1n$totc[2:dim(r1)[1]]

# r0$diffC13 <- r0$totc*0
# r1$diffC13 <- r1$totc*0
# r2$diffC13 <- r2$totc*0
# r3$diffC13 <- r3$totc*0
# r4$diffC13 <- r4$totc*0
# r5$diffC13 <- r5$totc*0
# # r1n$diffC13 <- r1n$totc*0

# r0$totcinout <- r0$totc*NA
# r1$totcinout <- r1$totc*NA
# r2$totcinout <- r2$totc*NA
# r3$totcinout <- r3$totc*NA
# r4$totcinout <- r4$totc*NA
# r5$totcinout <- r1$totc*NA
# # r1n$totcinout <- r1$totc*NA

# r0$totcinout[1] <- r0$totc[1]
# r1$totcinout[1] <- r1$totc[1]
# r2$totcinout[1] <- r2$totc[1]
# r3$totcinout[1] <- r3$totc[1]
# r4$totcinout[1] <- r4$totc[1]
# r5$totcinout[1] <- r5$totc[1]
# # r1n$totcinout[1] <- r1n$totc[1]

# for (i in 2:length(r1$year)){

#   r0$totcinout[i] <- r0$totcinout[i-1] + r0$in12[i] - r0$out12[i]
#   r1$totcinout[i] <- r1$totcinout[i-1] + r1$in12[i] - r1$out12[i]
#   r2$totcinout[i] <- r2$totcinout[i-1] + r2$in12[i] - r2$out12[i]
#   r3$totcinout[i] <- r3$totcinout[i-1] + r3$in12[i] - r3$out12[i]
#   r4$totcinout[i] <- r4$totcinout[i-1] + r4$in12[i] - r4$out12[i]
#   r5$totcinout[i] <- r5$totcinout[i-1] + r5$in12[i] - r5$out12[i]
#   # r1n$totcinout[i] <- r1n$totcinout[i-1] + r1n$in12[i] - r1n$out12[i]

#   r0$diffC13[i] <- ( r0$totc13[i-1] * r0$totc[i-1] - r0$totc13[i] * r0$totc[i] ) / ( r0$diffC[i] )
#   r1$diffC13[i] <- ( r1$totc13[i-1] * r1$totc[i-1] - r1$totc13[i] * r1$totc[i] ) / ( r1$diffC[i] )
#   r2$diffC13[i] <- ( r2$totc13[i-1] * r2$totc[i-1] - r2$totc13[i] * r2$totc[i] ) / ( r2$diffC[i] )
#   r3$diffC13[i] <- ( r3$totc13[i-1] * r3$totc[i-1] - r3$totc13[i] * r3$totc[i] ) / ( r3$diffC[i] )
#   r4$diffC13[i] <- ( r4$totc13[i-1] * r4$totc[i-1] - r4$totc13[i] * r4$totc[i] ) / ( r4$diffC[i] )
#   r5$diffC13[i] <- ( r5$totc13[i-1] * r5$totc[i-1] - r5$totc13[i] * r5$totc[i] ) / ( r5$diffC[i] )
#   # r1n$diffC13[i] <- ( r1n$totc13[i-1] * r1n$totc[i-1] - r1n$totc13[i] * r1n$totc[i] ) / ( r1n$diffC[i] )

# }

## Write CSV output files
write.csv( r0, file="cbalance_r0_holoLU2.csv", row.names=FALSE )
write.csv( r1, file="cbalance_r1_holoLU2.csv", row.names=FALSE )
write.csv( r2, file="cbalance_r2_holoLU2.csv", row.names=FALSE )
write.csv( r3, file="cbalance_r3_holoLU2.csv", row.names=FALSE )
write.csv( r4, file="cbalance_r4_holoLU2.csv", row.names=FALSE )
write.csv( r5, file="cbalance_r5_holoLU2.csv", row.names=FALSE )
# write.csv( r1n, file="cbalance_r1n_holoLU2.csv", row.names=FALSE )

## Write Fortran-formatted files
formatted.out.r0 <- vector( "character", length(r1$year) )
formatted.out.r1 <- vector( "character", length(r1$year) )
formatted.out.r2 <- vector( "character", length(r1$year) )
formatted.out.r3 <- vector( "character", length(r1$year) )
formatted.out.r4 <- vector( "character", length(r1$year) )
formatted.out.r5 <- vector( "character", length(r1$year) )
# formatted.out.r1n <- vector( "character", length(r1$year) )

for (i in 1:length(r1$year)){
  formatted.out.r0[i] <- sprintf( "%6i   %6f   %6f   %6f   %6f   %6f   %6f   %6f", r0[i,1], r0[i,2], r0[i,3], r0[i,4], r0[i,5], r0[i,6], r0[i,7], r0[i,8] )
  formatted.out.r1[i] <- sprintf( "%6i   %6f   %6f   %6f   %6f   %6f   %6f   %6f", r1[i,1], r1[i,2], r1[i,3], r1[i,4], r1[i,5], r1[i,6], r1[i,7], r1[i,8] )
  formatted.out.r2[i] <- sprintf( "%6i   %6f   %6f   %6f   %6f   %6f   %6f   %6f", r2[i,1], r2[i,2], r2[i,3], r2[i,4], r2[i,5], r2[i,6], r2[i,7], r2[i,8] )
  formatted.out.r3[i] <- sprintf( "%6i   %6f   %6f   %6f   %6f   %6f   %6f   %6f", r3[i,1], r3[i,2], r3[i,3], r3[i,4], r3[i,5], r3[i,6], r3[i,7], r3[i,8] )
  formatted.out.r4[i] <- sprintf( "%6i   %6f   %6f   %6f   %6f   %6f   %6f   %6f", r4[i,1], r4[i,2], r4[i,3], r4[i,4], r4[i,5], r4[i,6], r4[i,7], r4[i,8] )
  formatted.out.r5[i] <- sprintf( "%6i   %6f   %6f   %6f   %6f   %6f   %6f   %6f", r5[i,1], r5[i,2], r5[i,3], r5[i,4], r5[i,5], r5[i,6], r5[i,7], r5[i,8] )
  # formatted.out.r1n[i] <- sprintf( "%6i   %6f   %6f   %6f   %6f   %6f   %6f  %6f", r1n[i,1], r1n[i,2], r1n[i,3], r1n[i,4], r1n[i,5], r1n[i,6], r1n[i,7], r1n[i,8])
}

writeLines( formatted.out.r0, "cbalance_r0_holoLU2.txt" )
writeLines( formatted.out.r1, "cbalance_r1_holoLU2.txt" )
writeLines( formatted.out.r2, "cbalance_r2_holoLU2.txt" )
writeLines( formatted.out.r3, "cbalance_r3_holoLU2.txt" )
writeLines( formatted.out.r4, "cbalance_r4_holoLU2.txt" )
writeLines( formatted.out.r5, "cbalance_r5_holoLU2.txt" )
# writeLines( formatted.out.r1n, "cbalance_r1n_holoLU2.txt" )
