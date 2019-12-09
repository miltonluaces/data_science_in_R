# FUNCTIONS DLM-SMC WITH ZERO-INFLATED POISSON DISTRIBUTION
# ==========================================================================================================
    
library(dlm)
library(mvtnorm)


# Forecast samples with Zero-inflated Poisson for each time t
#-----------------------------------------------------------------------------------------------------------
# Paramets : number of samples desired nSamples, array of estimations of Lambda (fixed with repetition at zero periods from FixLambdaHat function) and an array of estimations of alpha (proportion of zeros at each time t)
# Returns  : a matrix with n data (n size of time series) and nSamples for each time t
HurdlePoissonFcstSamples = function(nSamples, lambdaHat, alphaHat) {
  nD = length(lambdaHat); nD
  fcstSamples = matrix(nrow=nSamples,ncol=nD); 
  fcst = -1;
  for(t in 2:(nD+1)) {
    fs = numeric(0);
    for(i in 1:nSamples) {
      rnd = runif(1, 0, 1); r
      lam = lambdaHat[t-1]
      alp = alphaHat[t-1]
      if(alp + (1-alp) * exp(-lam) <= rnd) { fs = 0; } else {  fs = (1-alp) * rpois(1, lam) }
      fcst = c(fcst, fs)
    }
    fcstSamples[,t-1] = fcst; 
  }
  fcstSamples
}

