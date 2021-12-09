#### CLV ####
BGBBCLV<-function(params,m,d,T) {
  params<-unname(params)
  al<-params[1]
  be<-params[2]
  ga<-params[3]
  de<-params[4]
  CLV<-1   # at time zero there has to be a purchase
  for (i in 1:T) {
    CLV<-CLV+(al/(al+be))*(beta(ga,de+i)/beta(ga,de))*1/(1+d)^{i}
  }
  CLV=m*CLV  # convert discount expected purchases into expected value
  return(CLV)    #return the CLV
} 

CLV<-BGBBCLV(params = params, m=50,d=.1,T=200)

#### RLV ####
m<-50
DERT<-bgbb.rf.matrix.DERT(params, donationsSummary$rf.matrix, d=0.1)
RLV<-m*DERT
RLVmatrix<-cbind(donationsSummary$rf.matrix,round(RLV,2)) 
RLVmatrix


#### Pareto NBD ####
data(cdnowSummary)

## Get the calibration period customer-by-sufficient-statistic matrix from the cdnow data:
cbs <- cdnowSummary$cbs

head(cbs[,1:3])

## Estimate parameters for the Pareto/NBD model from the CBS:
par.start <- c(0.5, 1, 0.5, 1)
params <- pnbd.EstimateParameters(cbs, par.start)
params

## Check log-likelihood of the params:
pnbd.cbs.LL(params, cbs)

#Distribution
par(mfrow=c(1,2))
par(mai=c(.8,.8,.5,.2))
temp<-pnbd.PlotTransactionRateHeterogeneity(params)
par(mai=c(.8,.8,.5,.2))
temp<-pnbd.PlotDropoutRateHeterogeneity(params)


## Plot the comparison of actual and expected calibration period frequencies:
par(mfrow=c(1,1))
par(mai=c(.8,.8,.5,.2))
pnbd.PlotFrequencyInCalibration(params, cbs, censor=7, plotZero=TRUE)


T.star <- 39                # Length of holdout period
x.star <- cbs[,"x.star"]    # Transactions made by each customer in the holdout period

## Plot the comparison of actual and conditional expected holdout period frequencies,
## binned according to calibration period frequencies:
par(mai=c(.8,.8,.5,.2))
pnbd.PlotFreqVsConditionalExpectedFrequency(params, T.star, cbs, x.star, censor=7)


## Plot the comparison of actual and conditional expected holdout period frequencies,
## binned according to calibration period recencies:
par(mai=c(.8,.8,.5,.2))
pnbd.PlotRecVsConditionalExpectedFrequency(params, cbs, T.star, x.star)



#### BG/NBD ####
data(cdnowSummary)

## Get the calibration period customer-by-sufficient-statistic matrix from the cdnow data:
cbs <- cdnowSummary$cbs

## Estimate parameters for the BG/NBD model from the CBS:
par.start <- c(1,3,1,3)
params <- bgnbd.EstimateParameters(cbs, par.start)
params

par(mfrow=c(1,2))
par(mai=c(.8,.8,.5,.2))
temp<-bgnbd.PlotTransactionRateHeterogeneity(params)
par(mai=c(.8,.8,.5,.2))
temp<-bgnbd.PlotDropoutRateHeterogeneity(params) 


## Check log-likelihood of the params:
bgnbd.cbs.LL(params, cbs)

## Plot the comparison of actual and expected calibration period frequencies:
par(mfrow=c(1,1))
bgnbd.PlotFrequencyInCalibration(params, cbs, censor=7, plotZero=TRUE)


T.star <- 39                # Length of holdout period
x.star <- cbs[,"x.star"]    # Transactions made by each customer in the holdout period

## Plot the comparison of actual and conditional expected holdout period frequencies,
## binned according to calibration period frequencies:
bgnbd.PlotFreqVsConditionalExpectedFrequency(params, T.star, cbs, x.star, censor=7)


## Plot the comparison of actual and conditional expected holdout period frequencies,
## binned according to calibration period recencies:
bgnbd.PlotRecVsConditionalExpectedFrequency(params, cbs, T.star, x.star)
