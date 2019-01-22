## /////////////////////////////////////////////
## Produces bar plot with error bars for compa-
## rison of total terrestrial C budget, peat
## uptake and ALCC emissions.
## 
## beni@climate.unibe.ch
## ---------------------------------------------
library(Hmisc)

## /////////////////////////////////////////////
## Define periods
## ---------------------------------------------
periodsBP <- c(-11000,-7000,-5000,-2000,-400)
periodsAD <- periodsBP + 1950
periodsName <- c("11-7","7-5","5-2","2-0 (1 - 1560 AD)")

## 11 kyr BP is to be represented by -8990 AD = 10940 BP
periodsAD[1] <- -8990

## /////////////////////////////////////////////
## Define total terrestrial C budget with error
## for each period with Elsig et al. (2009) data
## ---------------------------------------------
elsig <- read.table(
                    '/alphadata01/bstocker/holoC/fab_cum_mc_elsig_m1a_hilda_4box_caco3_5000+70_5000yr_15apr09_ddec.dat',
                    header = TRUE
                    )

## Find index corresponding to period edges
periods_i <- array(NA,c(length(periodsAD)))
for (per in seq(length(periodsAD))){
  periods_i[per] <- which.min(abs(elsig$time-periodsAD[per]))
}

## Define inventory change between period edges
cumCterr <- array(NA,c(length(periodsAD)))
stdCterr <- array(NA,c(length(periodsAD)))
for (per in seq(length(periodsAD))){
  cumCterr[per] <- elsig$fcummean[periods_i[per]]
  stdCterr[per] <- elsig$fcumstddv[periods_i[per]]
}

## Std.dev. of inventory *change* (DeltaC): sqrt(var(fcum_i)^2+var(fcum_i+1)^2)
deltaCterr <- array(NA,c(length(periodsAD)-1))
stdDlCterr <- array(NA,c(length(periodsAD)-1))
for (per in seq(length(periodsAD)-1)){
  deltaCterr[per] <- cumCterr[per+1]-cumCterr[per]
  stdDlCterr[per] <- sqrt(stdCterr[per]^2+stdCterr[per+1]^2)
}

## /////////////////////////////////////////////
## Read in total terrestrial C budget from model
## ---------------------------------------------
## this file is created with 
deltaCterr.mod <- array(NA,dim=c(length(periodsAD)-1,2))
# dC.lpx.yu <- read.table("/alphadata01/bstocker/holoC/totC_balance_yu_r2178.dat",
#                         na.strings="-999.0000", col.names=c("time", "totC")
#                         )

dC.lpx.trace21 <- read.table("/alphadata01/bstocker/holoC/totC_balance_trace21_87.dat",  # used 43 before
                        na.strings="-999.0000", col.names=c("time", "totC")
                        )
# dC.lpx.yu$time <- -dC.lpx.yu$time*1000-1950
dC.lpx.trace21$time <- dC.lpx.trace21$time-1950
dC.lpx.trace21$totC <- dC.lpx.trace21$totC/1e15

for (per in seq(length(periodsAD)-1)){
  ## Yu, fixed
  # deltaCterr.mod[per,1] <- dC.lpx.yu$totC[which.min(abs(dC.lpx.yu$time-periodsAD[per+1]))] - dC.lpx.yu$totC[which.min(abs(dC.lpx.yu$time-periodsAD[per]))]
  ## trace21
  deltaCterr.mod[per,2] <- dC.lpx.trace21$totC[which.min(abs(dC.lpx.trace21$time-periodsAD[per+1]))] - dC.lpx.trace21$totC[which.min(abs(dC.lpx.trace21$time-periodsAD[per]))]

}


## /////////////////////////////////////////////
## Define peatland C balance with error for each
## period with Yu (2010) data.
## ---------------------------------------------
yu_north <- read.csv(
                 "/alphadata01/bstocker/holoC/PeatlandsDataForRenatoSpahni_Oct2011Update_March2012_north.csv",
                 header=TRUE
                 )
yu_tropic <- read.csv(
                 "/alphadata01/bstocker/holoC/PeatlandsDataForRenatoSpahni_Oct2011Update_March2012_tropics.csv",
                 header=TRUE
                 )
yu_south <- read.csv(
                 "/alphadata01/bstocker/holoC/PeatlandsDataForRenatoSpahni_Oct2011Update_March2012_south.csv",
                 header=TRUE
                 )

deltaCpeat <- array(NA,c(length(periodsAD)-1,3))
stdDlCpeat <- array(NA,c(length(periodsAD)-1,3))

## 11 - 7 kyr
deltaCpeat[1,1] <- sum(yu_north$NCB..GtC.kyr.[8:11])
stdDlCpeat[1,1] <- sqrt(sum(yu_north$NCB.error..GtC.kyr.[8:11]^2))

deltaCpeat[1,2] <- sum(yu_tropic$NCB..GtC.kyr.[8:11])
stdDlCpeat[1,2] <- sqrt(sum(yu_tropic$NCB.error..GtC.kyr.[8:11]^2))

deltaCpeat[1,3] <- sum(yu_south$NCB..GtC.kyr.[8:11])
stdDlCpeat[1,3] <- sqrt(sum(yu_south$NCB.error..GtC.kyr.[8:11]^2))

## 7 - 5 kyr
deltaCpeat[2,1] <- sum(yu_north$NCB..GtC.kyr.[6:7])
stdDlCpeat[2,1] <- sqrt(sum(yu_north$NCB.error..GtC.kyr.[6:7]^2))

deltaCpeat[2,2] <- sum(yu_tropic$NCB..GtC.kyr.[6:7])
stdDlCpeat[2,2] <- sqrt(sum(yu_tropic$NCB.error..GtC.kyr.[6:7]^2))

deltaCpeat[2,3] <- sum(yu_south$NCB..GtC.kyr.[6:7])
stdDlCpeat[2,3] <- sqrt(sum(yu_south$NCB.error..GtC.kyr.[6:7]^2))

## 5 - 2 kyr
deltaCpeat[3,1] <- sum(yu_north$NCB..GtC.kyr.[3:5])
stdDlCpeat[3,1] <- sqrt(sum(yu_north$NCB.error..GtC.kyr.[3:5]^2))

deltaCpeat[3,2] <- sum(yu_tropic$NCB..GtC.kyr.[3:5])
stdDlCpeat[3,2] <- sqrt(sum(yu_tropic$NCB.error..GtC.kyr.[3:5]^2))

deltaCpeat[3,3] <- sum(yu_south$NCB..GtC.kyr.[3:5])
stdDlCpeat[3,3] <- sqrt(sum(yu_south$NCB.error..GtC.kyr.[3:5]^2))

## 2 - ~present
deltaCpeat[4,1] <- sum(yu_north$NCB..GtC.kyr.[1:2])
stdDlCpeat[4,1] <- sqrt(sum(yu_north$NCB.error..GtC.kyr.[1:2]^2))

deltaCpeat[4,2] <- sum(yu_tropic$NCB..GtC.kyr.[1:2])
stdDlCpeat[4,2] <- sqrt(sum(yu_tropic$NCB.error..GtC.kyr.[1:2]^2))

deltaCpeat[4,3] <- sum(yu_south$NCB..GtC.kyr.[1:2])
stdDlCpeat[4,3] <- sqrt(sum(yu_south$NCB.error..GtC.kyr.[1:2]^2))

## For completeness: 13 - 11 kyr
deltaCpeat_pre <- array(NA,c(3))
stdDlCpeat_pre <- array(NA,c(3))

deltaCpeat_pre[1] <- sum(yu_north$NCB..GtC.kyr.[12:13])
stdDlCpeat_pre[1] <- sqrt(sum(yu_north$NCB.error..GtC.kyr.[12:13]^2))

deltaCpeat_pre[2] <- sum(yu_tropic$NCB..GtC.kyr.[12:13])
stdDlCpeat_pre[2] <- sqrt(sum(yu_tropic$NCB.error..GtC.kyr.[12:13]^2))

deltaCpeat_pre[3] <- sum(yu_south$NCB..GtC.kyr.[12:13])
stdDlCpeat_pre[3] <- sqrt(sum(yu_south$NCB.error..GtC.kyr.[12:13]^2))

## /////////////////////////////////////////////
## Read in peat uptake data from model
## ---------------------------------------------
# dCpeat.lpx <- read.table("/alphadata01/bstocker/holoC/peatC_balance.dat",
#                          na.strings="-999.0000", col.names=c("time","trc","yu","trace21")
#                          )
## this file is created with /alphadata01/bstocker/holoC/analysis/write_ascii_holoC.jnl
trace21 <- read.table( "/alphadata01/bstocker/holoC/peatC_trace21_87.dat", col.names=c("year","peatCmask","peatoldpeatCmask","peatC","peatoldpeatC","netpeatC"))

trace21$year <- trace21$year
trace21[,2:6] <- trace21[,2:6]/1e15

# pdf("test.pdf")
# plot( trace21$year, trace21$trc, type="l" )
# lines( trace21$year, trace21$yu, col="red" )
# ## plot( trace21$year, trace21$trace21, type="l", col="green" )
# dev.off()

deltaCpeat.mod <- array(NA,dim=c(length(periodsAD)-1,3))
for (per in seq(length(periodsAD)-1)){
  
  deltaCpeat.mod[per,1] <- trace21$peatC[which.min(abs(trace21$year-periodsAD[per+1]))] - trace21$peatC[which.min(abs(trace21$year-periodsAD[per]))]

  deltaCpeat.mod[per,2] <- trace21$peatCmask[which.min(abs(trace21$year-periodsAD[per+1]))] - trace21$peatCmask[which.min(abs(trace21$year-periodsAD[per]))]

    # deltaCpeat.mod[per,2] <- trace21$trc[which.min(abs(trace21$year-periodsAD[per+1]))] - trace21$trc[which.min(abs(trace21$year-periodsAD[per]))]

    # deltaCpeat.mod[per,3] <- trace21$trace21[which.min(abs(trace21$year-periodsAD[per+1]))] - trace21$trace21[which.min(abs(trace21$year-periodsAD[per]))]
}


## /////////////////////////////////////////////
## "Construct" Tarnocai estimate using a
## different total C stock in northern peat
## lands and assuming same accumulation/decompo-
## sition rates and same extra-boreal stock as in
## Yu. 
## ---------------------------------------------

## The global total C stock in peatlands after Yu, 2011
totPtYu <- 612
NoPtYu  <- 547

## The C stock in *northern* peatlands after
## Tarnocai et al. (2009) (sum of Histels and
## Histosols).
NoPtTrc <- 365

## Take peat buildup since 11 kyr BP as a fraction of
## total peat stocks today (~85%).
NoPtTrcHolo <- NoPtTrc * sum(deltaCpeat[,1]) / NoPtYu

## Take relative C accumulation in northern peatlands
## in each period from Yu data.
fNoPtYu <- deltaCpeat[,1] / sum(deltaCpeat[,1])

## Assume same temporal dynamics of northern
## peat buildup but with absolute values according
## to Tarnocai. For extra-boreal peatlands, take
## values directly from Yu.
deltaCpeatTrc <- array(NA,c(length(periodsAD)-1,3))
deltaCpeatTrc[,1] <- fNoPtYu * NoPtTrcHolo
deltaCpeatTrc[,2] <- deltaCpeat[,2]
deltaCpeatTrc[,3] <- deltaCpeat[,3]

## /////////////////////////////////////////////
## Reduce peat uptake by 10% because last period
## only covers 1 - 1550 AD (in landuse data).
## ---------------------------------------------
deltaCpeat[4,] <- deltaCpeat[4,] * 0.9
deltaCpeatTrc[4,] <- deltaCpeatTrc[4,] * 0.9

## /////////////////////////////////////////////
## Define LU flux
## ---------------------------------------------
LUhyde   <- c(-0.52,-1.05,-7.14,-10.7)
LUkk10   <- c(-12.9,-11.4,-28.3,-48.9)
LUkaplan <- c(-40,-30,-80,-120)

## /////////////////////////////////////////////
## Combine data into array for plotting
## ---------------------------------------------
deltaCpeatGlobal <- rowSums(deltaCpeat)
stdDlCpeatGlobal <- sqrt(rowSums(stdDlCpeat*stdDlCpeat))

deltaCpeatGlobalTrc <- rowSums(deltaCpeatTrc)

gapYu <- deltaCterr-rowSums(deltaCpeat)
errGapYu <- sqrt(stdDlCterr^2+stdDlCpeatGlobal^2)

gapTrc <- deltaCterr-rowSums(deltaCpeatTrc)

arrayYu    <- cbind( deltaCterr, deltaCpeatGlobal, gapYu, LUkaplan)
errarrayYu <- cbind(stdDlCterr,  stdDlCpeatGlobal, errGapYu, LUkaplan*0 )

arrayTrc <- cbind(deltaCterr,deltaCpeatGlobalTrc,gapTrc,LUkk10)
arrayTrc2 <- cbind(deltaCterr*0,deltaCpeatGlobalTrc*0,gapTrc*0,LUhyde)

row.names(arrayYu) <- periodsName
row.names(arrayTrc) <- periodsName

## /////////////////////////////////////////////
## Write text file output
## ---------------------------------------------
write.table(
            arrayYu,
            file="arrayYu.txt",
            sep="\t",
            col.names=FALSE,
            row.names=FALSE
            )
write.table(
            arrayTrc,
            file="arrayTrc.txt",
            sep="\t",
            col.names=FALSE,
            row.names=FALSE
            )

## /////////////////////////////////////////////
## Bar plot with error bars
## ---------------------------------------------
pdf("/alphadata01/bstocker/holoLU2/ ")
par( las=1 )
ylim <- c(-200,300)
# rect( mybar1[1,], rep(ylim[1],4), mybar1[4,], rep(ylim[2],4), col=rgb(0,0,0,0.2), border=NA )
mybar1 <- barplot(
                  t(arrayYu),
                  beside = TRUE,
                  legend.text = NULL,
                  ylim = ylim,
                  col=c("green3","dodgerblue1","yellow","grey50"),
                  space=c(0.2,1.5),
                  border=TRUE
                  )
par(new=TRUE)
mybar2 <- barplot(
                 t(arrayTrc),
                 beside = TRUE,
                 legend.text = NULL,
                 ylim = ylim,
                 col=c("green3","dodgerblue4","orange","black"),
                 space=c(0.2,1.5),
                 xlab="period [kyr BP]",
                 ylab="carbon uptake [GtC]",
                 xaxt="n",
                 yaxt="n"
                 )
par(new=TRUE)
mybar3 <- barplot(
                  t(arrayTrc2),
                  beside = TRUE,
                  legend.text = NULL,
                  ylim = ylim,
                  col=c("green3","dodgerblue4","orange","white"),
                  space=c(0.2,1.5),
                  xlab="period [kyr BP]",
                  ylab="carbon uptake [GtC]",
                  xaxt="n",
                  yaxt="n",
                  border="white"
                 )
mybar1 <- rbind(mybar1[1:3,],mybar1[4,]+40)
errbar(
       mybar1,
       t(arrayYu),
       t(arrayYu+errarrayYu),
       t(arrayYu-errarrayYu),
       add=TRUE
       )

# symbols( mybar1[1,], deltaCterr.mod[,1], inches=FALSE, circles=rep(0.2,4), add=TRUE,
#         fg="green4", lwd=2 )
symbols( mybar1[1,], deltaCterr.mod[,2], inches=FALSE, circles=rep(0.2,4), add=TRUE,
        fg="green4", lwd=2 )

symbols( mybar1[2,], deltaCpeat.mod[,1], inches=FALSE, circles=rep(0.2,4), add=TRUE,
        fg="dodgerblue3", lwd=2 )
symbols( mybar1[2,], deltaCpeat.mod[,2], inches=FALSE, squares=rep(0.4,4), add=TRUE,
        fg="dodgerblue3", lwd=2 )
# symbols( mybar1[2,], deltaCpeat.mod[,2], inches=FALSE, stars=cbind(rep(0.2,4),rep(0.2,4),rep(0.2,4),rep(0.2,4)),
#         add=TRUE,
#         fg="dodgerblue3", lwd=2 )
# symbols( mybar1[2,], deltaCpeat.mod[,3], inches=FALSE, squares=rep(0.4,4), add=TRUE,
#         fg="dodgerblue3", lwd=2 )

abline(0,0)
legend(
       "topright",
       c("total terrestrial budget","peat buildup","other processes","ALCC"),
       fill=c("green3","dodgerblue1","yellow","grey50"),
       text.col="white",
       inset=c(0.02,0),
       bty="n",
       box.lwd=0
       )
legend(
       "topright",
       c("total terrestrial budget","peat buildup","other processes","ALCC"),
       fill=c("green3","dodgerblue4","orange","black"),
       bty="n",
       box.lwd=0
       )
legend(
       "bottomleft",
       c("LPX DYPTOP","LPX DYPTOP - visible today"),
       bty="n",
       pch=c(1,22)
       )
#box()
dev.off()


## /////////////////////////////////////////////
## NUMBERS
## ---------------------------------------------
## Did peat build up between 5 and 2 ka BP that is *not* present today?

print(paste("amount of peat C at ",periodsAD[4],trace21$peatC[which.min(abs(trace21$year-periodsAD[4]))]))
print(paste("amount of peat C at ",periodsAD[5],trace21$peatC[which.min(abs(trace21$year-periodsAD[5]))]))


