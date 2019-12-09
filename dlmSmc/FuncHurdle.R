# FUNCTIONS FOR DLM-SMC WITH HURDLE-POISSON DISTRIBUTION
# ==========================================================================================================
    
library(dlm)
library(mvtnorm)


# Fixes the array of estimations of lambda for non-zeros repeating estimation at time t when value is zero in original Data
#-----------------------------------------------------------------------------------------------------------
# Paramets : original time series Data, array of estimations of lambda LambdaHat
# Returns  : an array of estimations of lambda including periods with zero value (repeating last estimation of lambda in that case)
FixLambdaHat = function(Data, LambdaHat) {
   FixedLambdaHat = numeric(0);  
   lastLambda = 0;
   fixedLambda = -1;
   l = 1;
   for(t in 1:length(Data)) {
      if(Data[t] == 0) { 
        FixedLambdaHat = c(FixedLambdaHat, lastLambda);
      } 
      if(Data[t]!= 0) {
        FixedLambdaHat = c(FixedLambdaHat, LambdaHat[l]);
        lastLambda = LambdaHat[l];
        l = l+1; 
      }
   }
   FixedLambdaHat
}


# Beta parameters for each time t. Calculates the beta parameters a and b from time series for each time t
#-----------------------------------------------------------------------------------------------------------
# Paramets : time series Data
# Returns  : a matrix with first column is parameter a, second column is parameter b for each time t of the time series
BetaParams = function(Data) {
  nT = length(Data)
  Bp = matrix(nrow=nT,ncol=2);
  #uniform density U[0,1] (non-informative prior)
  a = 1; b = 1;
  for(t in 1:nT) {
    if(Data[t] == 0) { a = a+1; } else { b = b+1; }; 
    Bp[t,1] = a;
    Bp[t,2] = b;
  }
  Bp
}


# Beta-binomial sample for time t. Obtains a sample from the beta binomial model for a time t and a set o parameters for Beta for each time
#-----------------------------------------------------------------------------------------------------------
# Paramets : Beta parameters (from BetaParams function) and index t (time t of the time series)
# Returns : a Beta-Binomial sample for time t
BetaBinSample = function(betaParams, t) {
  nT = length(betaParams); nT
  a = betaParams[t,1]; a
  b = betaParams[t,2]; b
  betSamp = rbeta(1, a, b); 
  binSamp = rbinom(1, nT, betSamp); 
  binSamp 
}

# Beta sample for time t. Obtains a sample from the beta model for a time t 
#-----------------------------------------------------------------------------------------------------------
# Paramets : Beta parameters (from BetaParams function) and index t (time t of the time series)
# Returns : a Beta-Binomial sample for time t
BetaSample = function(betaParams, t) {
  nT = length(betaParams); nT
  a = betaParams[t,1]; a
  b = betaParams[t,2]; b
  betSamp = rbeta(1, a, b); 
  betSamp 
}


# Forecast samples with hurdle-Poisson for each time t  (Obsolete)
#-----------------------------------------------------------------------------------------------------------
# Paramets : number of samples desired nSamples, array of estimations of Lambda (fixed with repetition at zero periods from FixLambdaHat function) fixLambdaHat and array of Beta parameters a and b betaParams (from BetaParams function)
# Returns  : a matrix with n data (n size of time series) and nSamples for each time t
HurdlePoissonFcstSamplesObs = function(nSamples, fixLambdaHat, betaParams) {
  nD = length(fixLambdaHat); nD
  yHat = matrix(nrow=nSamples,ncol=nD); 
  fcst = -1;
  for(t in 2:(nD+1)) {
    yh = numeric(0);
    for(i in 1:nSamples) {
      r = runif(1, 0, 1); r
      s = (BetaBinSample(betaParams, t-1))/nD; s
      if(s <= r) { fcst = 0; } else {  fcst = rpois(1, fixLambdaHat[t-1]) }
      yh = c(yh, fcst); yh
    }
    yHat[,t-1] = yh; 
  }
  yHat
}

# Forecast samples with hurdle-Poisson for each time t  
#-----------------------------------------------------------------------------------------------------------
# Paramets : number of samples desired nSamples, array of estimations of Lambda (fixed with repetition at zero periods from FixLambdaHat function) fixLambdaHat and array of Beta parameters a and b betaParams (from BetaParams function)
# Returns  : a matrix with n data (n size of time series) and nSamples for each time t
HurdlePoissonFcstSamples = function(nSamples, fixLambdaHat, betaParams) {
  nD = length(fixLambdaHat); nD
  fcstSamples = matrix(nrow=nSamples,ncol=nD); 
  f = -1;
  for(t in 2:(nD+1)) {
    fcst = numeric(0);
    for(i in 1:nSamples) {
      rnd = runif(1, 0, 1)
      alp = BetaSample(betaParams, t-1)
      lam = fixLambdaHat[t-1]
      if(alp <= rnd) { f = 0; } else {  f = rpois(1, lam) }
      fcst = c(fcst, f)
    }
    fcstSamples[,t-1] = fcst; 
  }
  fcstSamples
}

# Forecast with hurdle-Poisson for each time t  
#-----------------------------------------------------------------------------------------------------------
# Paramets : forecastSamples (from HurdlePoissonFcstSamples function) and a threshold for zeros
# Returns  : an array of forecast for each time t
HurdlePoissonFcst = function(fcstSamples, threshold) {
  T = length(fcstSamples[1,]); T
  S = length(fcstSamples[,1]); S
  Fcst = numeric(0);
  for(t in 1:T) {
  fcst = -1
    nz = 0;
    m = 0;
    for(s in 1:S) {
       if(fcstSamples[s,t] == 0) { nz = nz+1 } else { m = m + fcstSamples[s,t] }
    }
    if((nz/S) >= threshold) { fcst = 0 } else { fcst = m /(S-nz)}
    Fcst = c(Fcst, fcst);
  }
  Fcst
}


# Split function (zeros/non-zeros). Extract a subset of zeros or non-zeros from a time series
#-----------------------------------------------------------------------------------------------------------
# Paramets : time series Data, boolean zeros (false for non-zeros)
# Returns  : time series with zeros or non-zeros
ExtractSubset = function(Data, zeros) {
  DataF = data.frame(Data)
  colnames(DataF) = "col1";
  if(zeros) { subData = subset(DataF, col1 == 0) }
  else { subData = subset(DataF, col1 > 0) }
  return (subData$col1)
}


# Generate a matrix of n samples (obsolete). Generates a matrix of nSamples
#-----------------------------------------------------------------------------------------------------------
# Paramets : number of data in time series nData, number of zeros nZeros, number of samples nSamples
# Returns  : the matrix
GenZeros = function(nData, nZeros, nSamples) {
  zeros = matrix(nrow=nSamples,ncol=nData)
  z = numeric(0); 
  for(i in 1:nData) {
    index = round(runif(1,1,nData)); index
    if(index <= nZeros) { z = c(z,0); }
  }
  zeros   
}


# Beta-binomial (obsolete). This function calculates proportion of zeros and samples from beta-binomial the number of zeros for each time t
#-----------------------------------------------------------------------------------------------------------
# Returns a matrix with two columns, in the first the proportion of zeros and in the second a sample of number of zeros for each time t
# the third column is the probability of zeros 
NZerosPropSample = function(Data) { 
  nT = length(Data)
  NZeros = matrix(nrow=nT,ncol=3);
  #uniform density U[0,1] (non-informative prior)
  a = 1; b = 1;
  for(t in 1:nT) {
    if(Data[t] == 0) { a = a+1; } else { b = b+1; }; a; b;
    alphaHat = (a-1)/(a+b-2); 
    NZeros[t,1] = alphaHat;
    betSamp = rbeta(1, a, b); betSamp
    binSamp = rbinom(1, nT, betSamp); binSamp
    NZeros[t,2] = binSamp
    NZeros[t,3] = binSamp/nT;
  }
  NZeros
}

# Forecast samples with hurdle-Poisson for each time t
#-----------------------------------------------------------------------------------------------------------
# Paramets : number of samples desired nSamples, array of estimations of alpha (proportion of zeros at each time t) and array of estimations of Lambda (fixed with repetition at zero periods from FixLambdaHat function) fixLambdaHat 
# Returns  : a matrix with n data (n size of time series) and nSamples for each time t
HurdlePoissonFcstSamples = function(nSamples, alphaHat, lambdaHat) {
  nT = length(lambdaHat); nT
  FcstSamples = matrix(nrow=nSamples,ncol=nT); 
  f = -1;
  for(t in 2:(nT+1)) {
    fcst = numeric(0);
    for(i in 1:nSamples) {
      rnd = runif(1, 0, 1); r
      alp = alphaHat[t-1]
      lam = lambdaHat[t-1]
      if(alp <= r) { f = 0; } else {  f = rpois(1, lam) }
      fcst = c(fcst, f)
    }
    FcstSamples[,t-1] = fcst; 
  }
  FcstSamples
}




# Unit Tests 
# ==========================================================================================================

# BetaSample
#-----------------------------------------------------------------------------------------------------------
A = numeric(0);
for(t in 1:length(Data)) {
  a = BetaSample(betaParams, t); a
  A = c(A,a)
}
A
