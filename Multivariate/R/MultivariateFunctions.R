library(RANN)
library(R6)
library(MiscFunctions)


# Pattern distance: KDTree ==========================================================================================================

hist = c(1,2,3,4,5,4,3,2,1,2,3,4,5,4,3,2,1,2,3,4,5)
fcst = c(3.8, 2.7, 1.9, 0.75)
mw=4

GetX = function(ts, mw) {
  X = data.frame(NULL)
  for(i in 1:(length(ts)-mw)) { X = (rbind(X, ts[i:(mw+i-1)])) }
  X
}

X = GetX(hist, 4); X
q = data.frame(cbind(2,3,4,6)); q
d = nn2(X,q, k=3); d
#d

GetDistances = function(hist, fcst, mw, k) {
  X = GetX(hist, mw)
  hf = c(hist[(length(hist)-mw+1):length(hist)], fcst[1:(length(fcst)-1)])
  q = GetX(hf, mw)
  nn2(X,q, k=1)$nn.dists
}

#GetDistances(hist, fcst, mw, k=4)
