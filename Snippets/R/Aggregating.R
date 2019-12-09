library(lubridate)
library(binhf)

# TS AGGREGATION ===========================================================================================================

# Group a time series by span
Group = function(ts, span, cens=FALSE) {
  gts = numeric(0)
  i=1
  while(i <= length(ts)-span+1) { gts = c(gts, sum(ts[i:(i+span-1)])); i=i+span }
  gts
}

# Generate Weekly time series from daily
GenerateWTS = function(dts, iniDate) {
  wIniDate = as.Date(iniDate)
  i=0
  while(wday(wIniDate, label=FALSE) != 2) { wIniDate=wIniDate+1; i=i+1;  }
  dtsForW = dts[(1+i):length(dts)]
  wts = Group(dtsForW, span=7)
  res=list() 
  res$wts = wts 
  res$wIniDate = wIniDate
  res
}

# Generate monthly time series from daily
GenerateMTS = function(dts, iniDate) {
  mIniDate = as.Date(iniDate)
  i=0
  while(day(mIniDate) != 1) { mIniDate=mIniDate+1; i=i+1;  }
  dtsForM = dts[(1+i):length(dts)]
  iniD = as.Date(mIniDate)
  ini=1
  mts = numeric(0)
  weights = numeric(0)
  span = days_in_month(iniD)
  while(ini < length(dtsForM)-span+1) {
    span = days_in_month(iniD)
    mts = c(mts, sum(dtsForM[ini:(ini+span-1)])); 
    weights = c(weights, span)
    ini=ini+span; 
    iniD = iniD+span
  }
  res=list() 
  res$mts = mts 
  res$mIniDate = mIniDate
  res$weights = weights
  res
}

# Apportion weekly time series with working days wDays, generating daily
ApportionWTS = function(wts, wDays) {
  nwDays = 7 - wDays
  appWTS = numeric(0)
  for(i in 1:length(wts)) {
    val = wts[i]/wDays
    appWTS = c(appWTS, rep(val, wDays))
    appWTS = c(appWTS, rep(0, nwDays))    
  }
  appWTS
}


# Apportion monthly time series with working days wDays, generating daily
ApportionMTS = function(mts, iniDate, wDays) {
  nwDays = 7 - wDays
  wlfactor = wDays/7
  appMTS = numeric(0)
  wDaysArr = c(rep(1, wDays), rep(0, nwDays))
  wDaysArr = shift(wDaysArr, 1)
  d = as.Date(iniDate)
  for(i in 1:length(mts)) {
    dim = days_in_month(d)
    val = mts[i]/(dim*wlfactor)
    for(j in 1:dim) {
      wd = wday(d, label=FALSE)
      appMTS = c(appMTS, val * wDaysArr[wd])
      d = d+1
    }
  }
  as.numeric(appMTS)
}


