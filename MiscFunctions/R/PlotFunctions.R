# IMPORTS PLOTS =====================================================================================================================

library(sp)

# BASIC PLOTS =====================================================================================================================

# Clears the plot and the console screens
Cls <- function() {
  cat("\014")
  plot.new()
}


# TIME SERIES PLOTS ===============================================================================================================

# Plots a time series history and forecast in one plot
PlotSeries <- function(hist, fcst=0, fcst2=0, last=0, title="Time series") {
  if(last==0) { last = length(hist)} 
  hist = hist[(length(hist)-last):length(hist)]
  xMax= length(hist) + max(length(fcst), length(fcst2)) + 1
  yMax = max (max(hist), max (fcst), max(fcst2))
  plot(c(hist, fcst), type ="n", xlim=c(1, xMax), ylim=c(0,yMax)) 
  if(length(fcst2)>1) { 
    lines(c(hist, fcst2), type ="l", col="pink", lwd="2") 
  } 
  if(length(fcst)>1) {
    lines(c(hist,fcst),  type ="l", col="red", lwd="2"); 
  }
  lines(hist,  type ="l", col="blue", lwd="2") 
}

# Plots a time series history and regressor in two plots
PlotHistReg <- function(hist, reg, histTitle='History', regTitle='Regressor') {
  par(mfrow = c(2,1))
  plot(hist,  type ="l", col="blue", lwd="2", main=histTitle)
  plot(reg,  type ="l", col="red", lwd="2", main=regTitle)
  par(mfrow = c(1,1))
}

# Plots a time series history and regressor in one plots
PlotBothHistReg <- function(hist, reg, Title = 'History and regressor') {
  par(mfrow = c(2,1))
  plot(hist,  type ="l", col="blue", lwd="2", main=Title)
  par(new=TRUE)
  plot(reg,  type ="l", col="red", lwd="2")
  par(mfrow = c(1,1))
}

# Plots a time series and a transformed one
TransfSeriesPlot <- function(origSerie, transfSerie) {
  pl = length(unlist(origSerie)) ; pl
  ml = length(transfSerie); ml
  minimum = min (min(origSerie), min (transfSerie)); minimum
  maximum = max (max(origSerie), max (transfSerie)); maximum
  diff = pl-ml; diff
  plot(unlist(origSerie)[diff:pl], type ="l", col="blue", lwd="2", ylim=c(minimum,maximum))
  lines(transfSerie, type ="l", col="red", lwd="2")
}

PlotSeriesWithReg <- function(hist,reg,regFcst, fcst1,fcst2) {
  par(mfrow = c(2,1))
  #par(mar=c(4,3,2,2))
  plot(c(hist,fcst1),  type ='l', col='lightsteelblue', lwd='2', main='History and fcst (with and without regressor)', xlab=NULL)
  lines(c(hist,fcst2),  type ='l', col='red', lwd='2', xlab=NULL)
  lines(hist,  type ='l', col='blue', lwd='2', xlab=NULL)
  plot(c(reg,regFcst),  type ='l', col='lightsteelblue', lwd='2', main='Regressor hist and fcst', xlab=NULL)
  lines(reg,  type ='l', col='blue', lwd='2', xlab=NULL)
  par(mfrow = c(1,1))
}

PlotSeriesWith2Reg <- function(hist,reg1,regFcst1,reg2,regFcst2,fcst1,fcst2) {
  par(mfrow = c(2,1))
  #par(mar=c(4,3,2,2))
  plot(c(hist,fcst1),  type ='l', col='lightsteelblue', lwd='2', main='History and fcst (with and without regressor)', xlab=NULL)
  lines(c(hist,fcst2),  type ='l', col='red', lwd='2', xlab=NULL)
  lines(hist,  type ='l', col='blue', lwd='2', xlab=NULL)
  plot(c(reg1,regFcst1),  type ='l', col='lightsteelblue', lwd='2', main='Regressor hist and fcst', xlab=NULL)
  lines(reg1,  type ='l', col='blue', lwd='2', xlab=NULL)
  lines(c(reg2,regFcst2),  type ='l', col='pink', lwd='2', xlab=NULL)
  lines(c(reg2),  type ='l', col='red', lwd='2', xlab=NULL)
  par(mfrow = c(1,1))
}

PlotNorm = function(tss) {
  if(length(tss) > 6) { return(0) }
  cols = c('blue', 'red', 'darkgreen', 'black', 'brown', 'green')
  nrm = Nrm$new(tss)
  nTs = numeric(length(tss))
  for(i in 1:length(tss)) { 
    nTs[i] = nrm$Norm(tss[i])
    if(i==1) plot(nTs[[i]], type='l', col=cols[i], lwd=2, ylim=c(0,1)) 
    else lines(nTs[[i]], type='l', col=cols[i], lwd=2, ylim=c(0,1))
  }
}


# DISTRIBUTION PLOTS ================================================================================================================

# Plots a distribution
DistPlot <-function(set, p, showPlot) {
  d = density(set); d
  q = quantile(set, p)
  if(showPlot == TRUE) {
    par(mfrow = c(2,1))
    plot(sort(set), pch=19, ylab="value")
    plot(d, main= paste("Percentile ", p, " = ", q), col="blue", lwd=2)
    abline(v=q:q)
    par(mfrow = c(1,1))
  }
  q  
}


# TESTING PLOTS =======================================================================================================================

# QQPlot from a list of data (Normality testing)
QQPlot <- function(data) {
  qqnorm(data, pch=19, main=paste("Normality Checking"))
  qqline(data, col="gray")
}


# MAP PLOTS =======================================================================================================================

PlotMap = function(maps, mapIndex, valData) {
  map = maps[mapIndex]
  map@data=data.frame(valData)
  spplot(map,c('valData'))
}


#Test: PIB per cápita por provincia en orden
#mapFile = 'C://Rlang/MiscFunctions/data/ESP_adm2.rds'
#maps=readRDS(mapFile)
#provs = RemoveAccent(maps@data$NAME_2)
#provsData = data.frame(provs, rep(0, length(provs)))
#colnames(provsData) = c('Nombre', 'Valor')
#provsData[provsData$Nombre=='Almería', 2] = 1  

#valData=c(32.942,16.268,18.442,20.267,19.401,15.538,23.854,26.671,25.735,16.173,17.934,23.005,17.299,15.837,20.318,17.656,25.660,16.580,17.436,30.734,18.293,22.885,14.870,20.517,26.084,24.032,  18.264,29.227,17.189,18.662,28.104,16.729,21.106,22.986,20.727,19.480,18.965,18.960,22.634,23.021,18.062,21.443,25.119,24.226,16.994,20.721,24.382,28.653,18.684,24.785,19.808,19.808,19.808,19.808,19.808,19.808);
#PlotMap(mapData, mapIndex=11, valData)
