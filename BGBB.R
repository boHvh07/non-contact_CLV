#            alpha beta gamma delta
par.start <- c(1, .5, 1, .5)

params <- bgbb.EstimateParameters(rf.matrix, par.start)
params


## Check log-likelihood of the params:
LL<-bgbb.rf.matrix.LL(params, rf.matrix)

## store parameters next to names
names(params) <- c("alpha", "beta", "gamma", "delta");

round(params,2)
