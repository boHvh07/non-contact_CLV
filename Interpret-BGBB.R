#### Parameter Estimates and Distributions ####
par(mfrow=c(1,2))
par(mai=c(.8,.8,.5,.2))
temp<-bgbb.PlotTransactionRateHeterogeneity(params)
par(mai=c(.8,.8,.5,.2))
temp<-bgbb.PlotDropoutRateHeterogeneity(params)


#### Model Fit Aggregate ###
inc.annual.trans <- donationsSummary$annual.trans   # incremental annual transactions

par(mfrow=c(1,1))
## Plot the comparison of actual and expected total incremental transactions across
## both the calibration and holdout periods:
par(mai=c(.8,.8,.3,.2))

pred<-bgbb.PlotTrackingInc(params, rf.matrix, inc.annual.trans, xticklab=x.tickmarks.yrs.all)[2,]

text(x = 4,y = 5000,"Calibration", cex=1, pos=3, col="black", font = 2)
text(x = 7,y = 5000,"Validation", cex=1, pos=3, col="black", font = 2)

# checking from paper (not needed)

al<-params[1]
be<-params[2]
ga<-params[3]
de<-params[4]

nn<-seq(1,11)
N<-sum(rf.matrix[,4])
Eq8<-(al/(al+be))*(de/(ga-1))*(1-(gamma(ga+de)*gamma(1+de+nn)/(gamma(ga+de+nn)*gamma(1+de))))

cum_donations<-N*Eq8
donations<-c(cum_donations[1],diff(cum_donations))


#### Model fit ####
par(mai=c(.8,.8,.5,.2))

comp<-bgbb.HeatmapHoldoutExpectedTrans(params, n.cal=6, n.star=5)

comp1<-comp

# rotate matrix so it's the same direction as the heatmap, this is just to make the numbers easier to read
rotate <- function(x) t(apply(x, 2, rev))
library(kableExtra)
kable(rotate(rotate(rotate(t(round(comp,2))))), format = "pipe")

n.star <- 5                        # Number of transaction opportunities in the holdout period
x.star <- donationsSummary$x.star  # Transactions made by each calibration period bin in the holdout period

X<-x.star/rf.matrix[,"custs"]

hol_rf_trans<-as.data.frame(cbind(rf.matrix[,1:2],X))

actual_rf<-reshape(hol_rf_trans, idvar="t.x", timevar="x", direction="wide")

# change NA's to 0
actual_rf[is.na(actual_rf)] <- 0

# re-order columns and rows
actual_rf<-actual_rf[order(actual_rf[,1]),order(actual_rf[1,])]

# make tx to rowname
rownames(actual_rf) <- actual_rf[,8]

# delete tx
actual_rf<-actual_rf[,c(-8)]

#actual_rf

# to make it look nice and rotated in same way as heatmap
kable(rotate(rotate(rotate((round(actual_rf,2))))), format = "pipe")

\\\\

par(mai=c(.8,.8,.5,.2))

## Plot the comparison of actual and conditional expected holdout period frequencies,
## binned according to calibration period frequencies:
comp<-bgbb.PlotFreqVsConditionalExpectedFrequency(params, n.star, rf.matrix, x.star)


rownames(comp) <- c("act", "exp", "bin")
comp

par(mai=c(.8,.8,.5,.2))

comp<-bgbb.PlotRecVsConditionalExpectedFrequency(params, n.star, rf.matrix, x.star)

rownames(comp) <- c("act", "exp", "bin")
comp

#### P(Alive) & Increasing Frequency Paradox ####
par(mfrow=c(1,1),mai=c(.8,.8,.5,.2))
plot(comp1[2:5,5], ylab ="Expected transactions in next 5 periods", xlab="Frequency holding last donation at 4", type="b", xaxt="n")
xtick<-seq(1, 4, by=1)
axis(side=1, at=xtick, labels = TRUE)


par(mfrow=c(1,1))
par(mai=c(.8,.8,.5,.2))

plot(bgbb.PAlive(params, x=1:4, t.x=4, n.cal=6), ylab ="Probability that customer is alive next period", xlab="Frequency holding last donation at 4", ylim=c(0,1), type="b", xaxt="n")
xtick<-seq(1, 4, by=1)
axis(side=1, at=xtick, labels = TRUE)