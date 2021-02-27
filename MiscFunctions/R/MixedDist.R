# IMPORTS =====================================================================================================================================================

library(BAMMtools)


# Clustering functions --------------------------------------------------------------------------------------------------------

WSS = function(X) {
  m = mean(X)
  ss = sum((X-m)^2); ss
}

GVF = function(X, ints) {
  m=mean(X)
  ss=WSS(X)
  n=length(ints)
  if(n<=1) return(-1)
  if(n==2) return(ss)
  ints[1] = ints[1]-1

  wss=0
  bss=0
  for(i in 2:n) {
    Xi = X[X>ints[i-1] & X<=ints[i]]
    wssi=WSS(Xi)
    wss=wss+wssi
    bss=bss+length(Xi)*((m-mean(Xi))^2)
  }
  gvf = (ss-wss)/ss

  res=list()
  res$ss = ss
  res$wss = wss
  res$bss = bss
  res$gvf = gvf
  res
}

GetMixedIntervals = function(X, thres) {
  maxInts=4
  bestInts=max(X)
  bestGfv=-1
  for(i in 3:maxInts) {
    ints = getJenksBreaks(X, i)
    gfv = GVF(X, ints)$gvf
    if(gfv>thres) { bestInts=ints[2:length(ints)]; bestGfv=gfv; break; }
  }
  res=list()
  res$ints = bestInts
  res$gfv = bestGfv
  res
}

# Split/Join functions --------------------------------------------------------------------------------------------------------

Split = function(ts, ints) {
  tss=data.frame(row.names=seq(1,length(ts)))
  n=length(ints)
  if(n==1) { tss=cbind(tss, ts); return(tss) }
  ints=ints[1:(n-1)]; n=n-1
  tsr=ts
  for(i in 0:n) {
    tsi=tsr
    tsi[tsi<=ints[(n-i)]]=0
    tss=cbind(tss, tsi)
    tsr=tsr-tsi
  }
  if(ncol(tss)==2) tss = PermDfCols(tss, 1, 2)
  if(ncol(tss)==3) tss = PermDfCols(tss, 1, 3)
  tss
}

Join = function(ts1, ts2, ts3=0) {
  ts1 = as.numeric(ts1)
  ts2 = as.numeric(ts2)
  ts3 = as.numeric(ts3)
  as.numeric(ts1+ts2+ts3)
} 


ts1 = c(1,2,3,4,3,2,1,2,3,4,2,3,4,5,4,3,2,3,4,5,6,5,7,5,3,2,4,5,7,5,3,2,2,1,3,4,5,2,4,1,7,4,5,6)
ts2 = c(1,2,3,4,3,20,1,2,3,4,2,3,4,5,4,23,22,3,4,5,26,5,7,5,3,2,18,5,7,5,3,2,2,19,3,4,5,2,27,1,7,4,5,6)
ts3 = c(1,2,3,4,3,20,1,2,3,4,42,3,4,5,4,23,22,3,4,5,26,5,7,5,3,2,18,5,7,45,3,2,2,19,3,4,5,2,27,1,7,4,48,6)
ts4 = c(1,86,3,4,3,20,1,2,3,4,42,3,4,5,4,23,22,3,4,5,26,5,7,95,3,2,18,5,7,45,3,92,2,19,3,4,85,2,27,1,7,4,48,6)

res1 = GetMixedIntervals(ts1, 0.9); res1
res2 = GetMixedIntervals(ts2, 0.9); res2
res3 = GetMixedIntervals(ts3, 0.9); res3
res4 = GetMixedIntervals(ts4, 0.9); res4

df1 = Split(ts1, res1$ints); df1
df2 = Split(ts2, res2$ints); df2
df3 = Split(ts3, res3$ints); df3
df4 = Split(ts4, res4$ints); df4

Join(df3[,1],df3[,2])
Join(df3[,1],df3[,2],df3[,3])


