# IMPORTS =====================================================================================================================================================

library(xlsx)
library(MiscFunctions)
library(nnet)
library(NeuralNetTools)
source('Multivariate/R/NN.R')
source('Multivariate/R/Norm.R')

# Data =====================================================================================================================================================

#suma 2

dataSum2 = list(c(2,5), c(6,9), c(1,1), c(1,2), c(7,1), c(3,7), c(1,8))
outSum2 = c(7, 17, 2, 3, 8, 10, 9)
norY2 = Norm$new(outSum2)

#suma 4
dataSum4 = list(c(2,5,3,1), c(2,6,9,4), c(1,8,4,2), c(1,2,1,3), c(7,8,6,4), c(3,5,7,8), c(2,1,4,3))
outSum4 = c(11,19,14,7,25,23,10)
norY4 = Norm$new(outSum4)

#Fun: x1 root x2 + x3 
dataFun = list(c(5,2,6), c(7,3,9), c(12,4,4), c(16,2,7), c(2,2,1), c(1,4,8), c(27,3,4))
outFun = c(8.23,10.91,5.86,11,2.41,9,7)
norYFun = Norm$new(outFun)

#Time Series
dataTs = list(c(1,2,3,4), c(2,3,4,5), c(3,4,5,4), c(4,5,4,3), c(5,4,3,2), c(4,3,2,1), c(3,2,1,2), c(2,1,2,3))
outTs = c(5,4,3,2,1,2,3,4)
norYTs = Norm$new(outTs)
dataTs

# Testing basico: nnnet

# sum 2
dfSum2 = data.frame(rbind(c(2,5), c(6,9), c(1,1), c(1,2), c(7,1), c(3,7), c(1,8))); dfSum2
nn.sum2 = nnet(outSum2 ~ dfSum2$X1 + dfSum2$X2, size=4, linout=T,trace=F, decay = 0, maxit = 30000) 
pred = predict(nn.sum2, dfSum2) 

res = data.frame(outSum2, round(pred,2), round(abs(outSum2-pred),2), round(abs(outSum2-pred)/outSum2*100,2))
names(res) = c('Exp', 'Obt', 'AbsErr', 'RelErr'); res
c(sum(res$AbsErr), round(sum(res$AbsErr)/sum(res$Exp),2))

# sum 4
dfSum4 = data.frame(rbind(c(2,5,3,1), c(2,6,9,4), c(1,8,4,2), c(1,2,1,3), c(7,8,6,4), c(3,5,7,8), c(2,1,4,3))); dfSum4
nn.sum4 = nnet(outSum4 ~ dfSum4$X1 + dfSum4$X2 + dfSum4$X3 + dfSum4$X4, size=4, linout=T,trace=F, decay = 0, maxit = 30000) 
pred = predict(nn.sum4, dfSum4) 

res = data.frame(outSum4, round(pred,2), round(abs(outSum4-pred),2), round(abs(outSum4-pred)/outSum4*100,2))
names(res) = c('Exp', 'Obt', 'AbsErr', 'RelErr'); res
c(sum(res$AbsErr), round(sum(res$AbsErr)/sum(res$Exp),2))

#Fun
dfFun = data.frame(rbind(c(5,2,6), c(7,3,9), c(12,4,4), c(16,2,7), c(2,2,1), c(1,4,8), c(27,3,4))); dfFun
nn.fun = nnet(outFun ~ dfFun$X1 + dfFun$X2 + dfFun$X3, size=4, linout=T,trace=F, decay = 0, maxit = 30000) 
pred = predict(nn.fun, dfFun) 

res = data.frame(outFun, round(pred,2), round(abs(outFun-pred),2), round(abs(outFun-pred)/outFun*100,2))
names(res) = c('Exp', 'Obt', 'AbsErr', 'RelErr'); res
c(sum(res$AbsErr), round(sum(res$AbsErr)/sum(res$Exp),2))

#Time series
dfTs = data.frame(rbind(c(1,2,3,4), c(2,3,4,5), c(3,4,5,4), c(4,5,4,3), c(5,4,3,2), c(4,3,2,1), c(3,2,1,2), c(2,1,2,3))); dfTs
outTs = rbind(5,4,3,2,1,2,3,4); colnames(outTs) = 'r' 
nn.ts = nnet(outTs ~ dfTs$X1 + dfTs$X2 + dfTs$X3 + dfTs$X4, size=4, linout=T,trace=F, decay = 0, maxit = 30000) 
pred = predict(nn.ts, dfTs) 

nn.ts = nnet(dfTs, outTs, size=4, linout=T,trace=F, decay = 0, maxit = 30000) 

res = data.frame(outTs, round(pred,2), round(abs(outTs-pred),2), round(abs(outTs-pred)/outTs*100,2))
names(res) = c('Exp', 'Obt', 'AbsErr', 'RelErr'); res
c(sum(res$AbsErr), round(sum(res$AbsErr)/sum(res$Exp),2))

# Weights, LekProfile sensitivity, Graph, Garson & Olden importance of inputs 
nn.ts
neuralweights(nn.ts)
plotnet(nn.ts, alpha=0.6)
gar = garson(nn.ts); gar$data
plot(gar)
old = olden(nn.ts); old$data
plot(old)
lekprofile(nn.ts) 

source('Multivariate/R/NNAnalysis.R')
lek.fun(nn.ts, resp.name=outTs)

x = c(1,2,3,4,5)
x
X = cbind(x)
X

# Function Approximation =====================================================================================================================================================

# NN 2-3-1 complete, prueba suma

#suma 2
nn = NN$new(nInput=2, nHidden=3, nOutput=1, connHidden=list(c(1,2), c(1,2), c(1,2)), connOutput=list(c(1,2,3)), n=0.4, m=0.6, actFun='logsig'); #nn$Summary()
nn = NN$new(nInput=2, nHidden=5, nOutput=1, connHidden=list(c(1,2), c(1,2), c(1,2), c(1,2), c(1,2)), connOutput=list(c(1,2,3,4,5)), n=0.3, m=0.6, actFun='logsig'); #nn$Summary()
nn$Summary()

for(e in 1:10000) { 
  for(i in 1:length(dataSum2)) {  nn$Train(dataSum2[[i]], desOutput=norY2$Norm(outSum2[[i]]), epochs=1); }
}

for(i in 1:length(dataSum2)) {  print(norY2$UnNorm(nn$Process(dataSum2[[i]]))); }
norY2$UnNorm(nn$Process(c(6,5)))
norY2$UnNorm(nn$Process(c(3,3)))
norY2$UnNorm(nn$Process(c(7,7)))


#suma 4
source('Multivariate/R/NN.R')
norY4$xMin = 5
norY4$xMax = 27
norY4$xRange = 22

nn = NN$new(nInput=4, nHidden1=5, nOutput=1, connHidden1=list(c(1,2,3,4), c(1,2,3,4), c(1,2,3,4), c(1,2,3,4), c(1,2,3,4)), connOutput=list(c(1,2,3,4,5)), n=0.3, m=0.6, actFun='logsig'); #nn$Summary() 
for(e in 1:1000) { for(i in 1:length(dataSum4)) {  nn$Train(dataSum4[[i]], desOutput=norY4$Norm(outSum4[[i]]), epochs=1); } }
nn$CheckError(norY4, dataSum4, outSum4, 2)

#fun: x1 root x2 + x3 
nn = NN$new(nInput=3, nHidden=5, nOutput=1, connHidden=list(c(1,2,3), c(1,2,3), c(1,2,3), c(1,2,3), c(1,2,3)), connOutput=list(c(1,2,3,4,5)), n=0.3, m=0.6, actFun='logsig'); #nn$Summary()
nn$Summary()

for(e in 1:10000) { 
  for(i in 1:length(dataFun)) {  nn$Train(dataFun[[i]], desOutput=norYFun$Norm(outFun[[i]]), epochs=1); }
}

for(i in 1:length(dataFun)) {  print(norYFun$UnNorm(nn$Process(dataFun[[i]]))); }


# Time Series =====================================================================================================================================================

norYTs = Norm$new(outTs)
norYTs$xMin = 0
norYTs$xMax = 6
norYTs$xRange = 6
dataTs
outTs

nn = NN$new(nInput=4, nHidden=5, nOutput=1, connHidden=list(c(1,2,3,4), c(1,2,3,4), c(1,2,3,4), c(1,2,3,4), c(1,2,3,4)), connOutput=list(c(1,2,3,4,5)), n=0.2, m=0.4, actFun='logsig'); #nn$Summary() 
nn = NN$new(nInput=4, nHidden=9, nOutput=1, connHidden=list(c(1,2,3,4), c(1,2,3,4), c(1,2,3,4), c(1,2,3,4), c(1,2,3,4), c(1,2,3,4), c(1,2,3,4), c(1,2,3,4), c(1,2,3,4)), connOutput=list(c(1,2,3,4,5,6,7,8,9)), n=0.1, m=0.4, actFun='logsig'); #nn$Summary() 
for(e in 1:1000) { for(i in 1:length(dataTs)) {  nn$Train(dataTs[[i]], desOutput=norYTs$Norm(outTs[[i]]), epochs=1); } }
nn$CheckError(norYTs, dataTs, outTs, 2)


