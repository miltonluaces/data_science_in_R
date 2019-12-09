# IMPORTS ===========================================================================================================================================================================================

library(R6)
library(MiscFunctions)
library(nnet)
library(kernlab)
library(tseries)
library(forecast)


# NN MODELS ====================================================================================================================================

Norm <- function(x, min, max) { y = (x - min)/(max-min) }
UnNorm <- function(y, min, max) { y * (max-min) + min }

NNModel <- function(formula) {
  nnet(formula, size=2, linout=T, trace=F, decay = 0.8)
}

SVModel <- function(formula, data) {
 ksvm(formula, data=data)
}

# PREDICTION ====================================================================================================================================

NNPredict <- function(nnm, values, min, max) {
  nn.predict = predict(nnm, values); pred = UnNorm(nn.predict, min, max) 
  data.frame(values, pred)
}

