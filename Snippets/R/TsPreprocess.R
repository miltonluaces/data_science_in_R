library(psych)
library(onewaytests)


# Functions ==========================================================================================================

# Descomponse a time series in trend, seasonal and random
CalcDescFcst = function(ts) {
  desc = decompose(ts); 
  fitTrend <- auto.arima(desc$trend) 
  fitSea <- auto.arima(desc$seasonal)
  c(fitTrend, fitSea)
}

# Get significant lags from a Pacf. nSds = number of standard deviations (i.e.: 95%=2, 99%=3)
GetSignificantPacf = function(ts, nSds=2, plot=TRUE) {
  p = pacf(ts, plot=plot)
  lim = nSds/sqrt(length(ts))
  df = data.frame(which(p$acf>lim), p$acf[p$acf>lim])
  colnames(df) = c('index', 'acf') 
  df
}

# Get best lag for a ts and a regressor
GetLag = function(ts, reg, nSds=2, plot=TRUE) {
  c = ccf(ts,reg, na.action=na.pass, plot=plot)
  lim = nSds/sqrt(length(c$acf))
  c$lag[which(abs(c$acf)==max(abs(c$acf)))]
}

# Shift a time series to the right (shift positive) or to the left (shift negative)
Shift = function(x, shift) {
  if(shift > 0) c(rep(NA,times=shift), x[1:(length(x)-shift)])
  else c(x[(1-shift):length(x)], rep(NA,times=-shift))
}

# Create dataframe with n dummy series for a categorical time series
CreateDummies = function(serie) {
  factors = factor(serie)
  levs =levels(factors)
  dummies = numeric(0)
  for(i in 1:length(levs)) {
    dummy = serie==levs[i]
    dummies = cbind(dummies, dummy*1)
  }
  dummies
}

CalcAmpSerie = function(serie, mw) {
  ma = CalcMA(ts, n=mw)
  amp = abs((serie-ma)/ma)
  amp
}

CalcSerieMean = function(serie, mw, trim=0.2) {
  n = length(serie)
  winsor.mean(serie[(n-mw):n], trim=trim, na.rm = TRUE)
}

CalcDecay = function(ampMean) {
  (1/ampMean/100)^(1+ampMean) 
}

Split = function(ts,span) {
  ns=length(ts)%/%span;ns
  split(ts, cut(seq_along(ts), ns, labels = FALSE)) 
}

EliminateHolidays = function(ts, cal) {
  newTs=rep(0,length(ts)); newTs
  if(cal[1]==0) newTs[1]=ts[2] else newTs[1]=ts[1]
  i=2
  while(i<=length(ts)-1) {
    if(cal[i]==0) {
      ini=i-1; end=i+1; j=1
      while(cal[i+j]==0) { j=j+1; end=i+j } 
      if(end > length(ts)) end=length(ts)
      if(end-ini==2) { newTs[i]=(ts[ini]+ts[end])/2}
      else { 
        for(j in (ini+1):(end-1)) { 
          newTs[j]=LineEq(j, ini, ts[ini], end, ts[end]) 
        }  
      }
      i=end
    }  
    else{
      newTs[i]=ts[i]; i=i+1
    }
  }
  if(cal[length(ts)]==0) newTs[length(ts)]=ts[(length(ts)-1)] else newTs[length(ts)]=ts[length(ts)] 
  newTs
}

#Interpolate -1 values
EliminateCensored = function(ts) {
  newTs=rep(0,length(ts)); newTs
  if(ts[1]==-1) newTs[1]=ts[2] else newTs[1]=ts[1]
  i=2
  while(i<=length(ts)-1) {
    if(ts[i]==-1) {
      ini=i-1; end=i+1; j=1
      ini; end
      while(ts[i+j]==-1) { j=j+1; end=i+j } 
      ini;end
      if(end-ini==2) { newTs[i]=(ts[ini]+ts[end])/2}
      else { 
        for(j in (ini+1):(end-1)) { 
          newTs[j]=LineEq(j, ini, ts[ini], end, ts[end]) 
        }  
      }
      i=end
    }  
    else{
      newTs[i]=ts[i]; i=i+1
    }
  }
  if(ts[length(ts)]==-1) newTs[length(ts)]=ts[(length(ts)-1)] else newTs[length(ts)]=ts[length(ts)] 
  newTs
}

CheckOutlierFcst = function(hist, mw, fcst) {
  n = length(hist)
  mw = min(mw, n)
  last = hist[(n-mw):n]
  m = mean(last)
  s = sd(last)
  mf = mean(fcst)
  ext = max(abs(fcst-mf))+mf
  p = pnorm(ext,m, s); round(p,3)
}

CheckVarFcst = function(hist, mw, fcst) {
  n = length(hist)
  mw = min(mw, n)
  last = hist[(n-mw):n]
  gr = c(rep('h', times=length(last)), rep('f', times=length(fcst)))
  res = bartlett.test(c(last,fcst), gr);   p=res$p.value; round(p,3)
}

CalcSdRatio = function(hist, mw, fcst) {
  if(length(fcst <= 1)) { return(1) }
  n = length(hist)
  mw = min(mw, n)
  last = hist[(n-mw):n]
  ratio = sd(fcst)/sd(last)
  round(ratio,3)
}

CalcMedAE = function(errTs, mw) {
  n=length(errTs)
  median(errTs[(n-mw+1):n])
}

CalcMedAEs = function(errDf, mw) {
  apply(errDf, 2, CalcMedAE, mw)
}

RegEffectRemovalByPerTs = function(ts, rePer) {
  tsre = numeric(length(ts))
  tsre[1] = ts[1]
  interp = FALSE
  ini = 0
  for(i in 2:length(ts)) {
    if(rePer[i]==0) {
      if(interp == TRUE) { for(j in (ini+1):(i-1)) { tsre[j] = LineEq(j, x1=ini, y1=ts[ini], x2=i, y2=ts[i]) }; interp = FALSE }
      tsre[i] = ts[i] 
    }
    if(rePer[i]==1) {
      if(interp == FALSE) { ini = i-1; interp = TRUE }
    }
  }
  tsre
}

RegEffectRemovalByIndex = function(ts, iniPer, endPer) {
  tsre = numeric(length(ts))
  tsre[1] = ts[1]
  index = 1
  i=2
  while(i <= length(ts)) {
    if(i < iniPer[index] | index > length(iniPer)) {
      tsre[i] = ts[i]
      i=i+1
    }
    else {
      for(j in iniPer[index]:endPer[index]) {
        inI = iniPer[index]-1; enD = endPer[index]+1
        tsre[j] = LineEq(j, x1=inI, y1=ts[inI], x2=enD, y2=ts[enD])
      }
      i = enD
      index = index + 1
    }
  }
  tsre
}

ReplacePerWithStat = function(ts, re) {
  tsre = numeric(length(ts))
  tsre[1] = ts[1]
  replace = FALSE
  ini = 0
  for(i in 2:length(ts)) {
    if(re[i]==0) {
      if(replace == TRUE) { 
        perStat = mean(ts[(ini+1):(i-1)]); for(j in (ini+1):(i-1)) { tsre[j] = perStat }; replace = FALSE }
      tsre[i] = ts[i] 
    }
    if(re[i]==1) {
      if(replace == FALSE) { ini = i-1; replace = TRUE }
    }
  }
  tsre
}

GetPerStats = function(ts, re) {
  perStats = numeric(0)
  regEffPer = FALSE
  ini = 0
  for(i in 2:length(ts)) {
    if(re[i]==0 && regEffPer == TRUE) { 
        perStat = mean(ts[(ini+1):(i-1)]); perStats = c(perStats, perStat);  regEffPer = FALSE 
    }
    if(re[i]==1) {
      if(regEffPer == FALSE) { ini = i-1; regEffPer = TRUE }
    }
  }
  perStats
}

GetRegEffStats = function(ts, iniEff, endEff) {
  if(length(iniEff) != length(endEff)) { print("Error. Different length of ini, end"); return(-1 ) }
  regEffStats = numeric(length(iniEff));
  for(i in 1:length(iniEff)) {
    regEffStats[i] = mean(ts[iniEff[i]:endEff[i]])
  }
  regEffStats
}

GetLastTs = function(ts, n) {
  end = length(ts)
  ini = end-n+1; if(ini < 1) ini=1
  ts[ini:end]
}

