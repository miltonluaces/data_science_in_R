# BASIC TESTING
# ==========================================================================================================
# ==========================================================================================================


library(dlm)
library(mvtnorm)
sapply(list.files(pattern="Func[A-Za-z]*.R", path="./", full.names=TRUE), source);



# Poisson 
# ==========================================================================================================

# Data 
#-----------------------------------------------------------------------------------------------------------
nF = 13;
F = matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1), nrow=nF, ncol=1); F
J1 = matrix(c(1, 1, 0, 1), nrow=2, ncol=2, byrow = TRUE); J1
J2 = function(k) { matrix(c(cos(k*pi/6), sin(k*pi/6), -sin(k*pi/6), cos(k*pi/6)), nrow=2, ncol=2, byrow = TRUE) }
G = bdiag(J1, J2(1), J2(2), J2(3), J2(4), J2(5), -1); G
V = 10 
W = 10*diag(nF); W

Data = c(9,10,11,7,17,13,14,7,3,8,11,6,9,14,11,9,8,7,13,8,13,9,12,14,10,8,6,11,7,15); Data
meanData = mean(Data); meanData
varData = var(Data); varData
  
lm = log(10); lm  
m0 = matrix(c(lm, rep(0, 12)), nrow=nF, ncol=1); m0
C0 = 10^6*diag(nF); C0

nSamples = 10;
nW = 18

# Calculation
#-----------------------------------------------------------------------------------------------------------
LambdaHat = DlmSmcPoisson(Data, F, G, m0, C0, V, W, nW); LambdaHat
fcstSamples = PoissonFcstSamples(nSamples, LambdaHat); fcstSamples
fcstMean = FcstMean(fcstSamples); fcstMean

# TS Plots
#-----------------------------------------------------------------------------------------------------------
plotHistFcst(Data, fcstMean, "SMC with Poisson Distribution")
plotResiduals(Data, fcstMean, "SMC with Poisson Distribution. Residuals")
    
# Distribution plots
plot(hist(fcstSamples))
par(mfrow=c(4,4))
for(i in 1:16) {
  plot(density(fcstSamples[,i]), main= paste("sample", i))
}
par(mfrow=c(1,1))



# Hurdle-Poisson 
# ==========================================================================================================


# Data
#-----------------------------------------------------------------------------------------------------------
Data = c(0,0,9,10,0,11,0,0,7,17,13,14,7,3,8,11,0,6,9,0,14,11,9,8,7,13,8,13,0,0,0,9,12,14,10,8,6,11,7,15); Data
DataNZ = ExtractSubset(Data, FALSE); DataNZ

# Calculation
#-----------------------------------------------------------------------------------------------------------
lambdaHat = DlmSmcPoisson(DataNZ, F, G, m0, C0, V, W, nW); lambdaHat
fixLambdaHat = FixLambdaHat(Data, LambdaHat); fixLambdaHat
betaParams = BetaParams(Data); betaParams
fcstSamples = HurdlePoissonFcstSamples(10, fixLambdaHat, betaParams); fcstSamples
fcstMean = FcstMean(fcstSamples); fcstMean

threshold = 0.85;
fcstMean = HurdlePoissonFcst(fcstSamples, threshold); fcst

# Plots
#-----------------------------------------------------------------------------------------------------------

# Parameter plots
par(mfrow=c(2,1))
plot(lambdaHat, type="l", main="Estimation of lambda parameter", xlab="time t", ylab="value")
plot(fixLambdaHat, type="l", main="Estimation of lambda for whole time series", xlab="time t", ylab="value")
par(mfrow=c(1,1))

plot(betaParams[,2], main="Beta parameters a and b", xlab="time t", ylab="value", type="l", col="blue")
lines(betaParams[,1], col="red")

# TS Plots
plotHistFcst(Data, fcstMean, "SMC with Hurlde-Poisson Distribution")
plotResiduals(Data, fcstMean, "SMC with Hurdle-Poisson Distribution. Residuals")
    
# Distribution plots
plot(hist(fcstSamples))
par(mfrow=c(4,4))
for(i in 1:16) {
  plot(density(fcstSamples[,i]), main= paste("sample", i))
}
par(mfrow=c(1,1))



# Negative-Binomial
# ==========================================================================================================

# Data
#-----------------------------------------------------------------------------------------------------------
F = matrix(c(1, 0, 1, 0, 1), 5, 1); F
J1 = matrix(c(1, 0, 1, 1), 2, 2)
J2 = matrix(c(cos(pi/2), -sin(pi/2), sin(pi/2), cos(pi/2)), nrow=2, ncol=2)
G = bdiag(J1, J2, -1); G
m0 = matrix(c(rep(0, length(F))),nrow=length(F), ncol=1); m0
C0 = 1 * diag(5); C0
V = 1
W = 1 * diag(length(F)); W
n = 2
nW = 18
nSamples = 5


# data and forecasting horizon N
Data = c(0,1,2,0,2,2,0,2,2,0,0,1,0,0,1,1,2,2,0,0,0,0,0,1); Data
m = mean(Data); m
v = var(Data); v
yHat = SeqMCNegBin(Data, n, F, G, m0, C0, V, W, nW, nSamples); yHat 
fcst = CalcFcst(yHat)

Data = c(9,10,11,7,17,13,14,7,3,8,11,6,9,14,11,9,8,7,13,8,13,9,12,14); Data
m = mean(Data); m
v = var(Data); v

r = GetR(m,v); r
p = GetP(m,r); p
n = 17

# Calculation
#-----------------------------------------------------------------------------------------------------------

yHat = SeqMCNegBin(Data, n, F, G, m0, C0, V, W, nW, nSamples); yHat 
fcst = CalcFcst(yHat)

# Plots
#-----------------------------------------------------------------------------------------------------------

plotHistFcst(Data, fcst, "SMC with Negative Binomial Distribution")
plotResiduals(Data, fcst, "SMC with Negative Binomial Distribution. Residuals")

#distribution
plot(hist(yHat))
par(mfrow=c(4,4))
for(i in 1:16) {
  plot(density(yHat[,i]), main= paste("sample", i))
}
par(mfrow=c(1,1))


