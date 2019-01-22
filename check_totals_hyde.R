crop <- read.csv('crop_totals_hyde.csv')
past <- read.csv('past_totals_hyde.csv')

pdf( "crop_check_totals_hyde.pdf", width=15,height=6)
par( mfrow=c(1,2))
plot( crop$year,  crop$hyde31, type="l" )
lines( crop$year, crop$hyde31_in_halfdeg, lty=2 )
lines( crop$year, crop$hyde31_in_lpjgr, lty=3 )
lines( crop$year, crop$hyde32, col="red" )
lines( crop$year, crop$hyde32_in_halfdeg, col="red", lty=2 )
lines( crop$year, crop$hyde32_in_lpjgr, col="red", lty=3 )

plot( crop$year,  crop$hyde31, type="l", xlim=c(1000,2000) )
lines( crop$year, crop$hyde31_in_halfdeg, lty=2 )
lines( crop$year, crop$hyde31_in_lpjgr, lty=3 )
lines( crop$year, crop$hyde32, col="red" )
lines( crop$year, crop$hyde32_in_halfdeg, col="red", lty=2 )
lines( crop$year, crop$hyde32_in_lpjgr, col="red", lty=3 )

dev.off()


pdf( "past_check_totals_hyde.pdf", width=15,height=6)
par( mfrow=c(1,2))
plot( past$year,  past$hyde31, type="l" )
lines( past$year, past$hyde31_in_halfdeg*1e-1, lty=2 )
lines( past$year, past$hyde31_in_lpjgr*1e-1, lty=3 )
lines( past$year, past$hyde32, col="red" )
lines( past$year, past$hyde32_in_halfdeg*1e-1, col="red", lty=2 )
lines( past$year, past$hyde32_in_lpjgr*1e-1, col="red", lty=3 )

plot( past$year,  past$hyde31, type="l", xlim=c(1000,2000) )
lines( past$year, past$hyde31_in_halfdeg*1e-1, lty=2 )
lines( past$year, past$hyde31_in_lpjgr*1e-1, lty=3 )
lines( past$year, past$hyde32, col="red" )
lines( past$year, past$hyde32_in_halfdeg*1e-1, col="red", lty=2 )
lines( past$year, past$hyde32_in_lpjgr*1e-1, col="red", lty=3 )

dev.off()