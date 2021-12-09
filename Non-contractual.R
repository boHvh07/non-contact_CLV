library(BTYD)
data(donationsSummary)

# add first purchase to beginning

donations<-c(11104,donationsSummary$annual.trans)

par(mfrow=c(1,1))
par(mai=c(.8,.8,.2,.2))
plot(seq(1996,2006,1),donationsSummary$annual.trans, type="b", ylab="Total number of repeat transactions", xlab="Year", main="", xaxt='n')
x.tickmarks.yrs.all <- c( "'96","'97","'98","'99","'00","'01","'02","'03","'04","'05","'06" )
#axis(1, at = seq(0, 11, by = 1))
axis(1, at=seq(1996,2006,1),labels = x.tickmarks.yrs.all)
abline(v=2001.5,col = "red", lwd = 2)
text(x = 1999,y = 5000,"Calibration", cex=1, pos=3, col="black", font = 2)
text(x = 2004,y = 5000,"Validation", cex=1, pos=3, col="black", font = 2)



## Get the calibration period recency-frequency matrix from the donation data:
rf.matrix <- donationsSummary$rf.matrix
rf.matrix
