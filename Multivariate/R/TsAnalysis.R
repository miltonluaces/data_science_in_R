library(TSclust)
library(cluster)
library(R6)
library(MiscFunctions)

GetX = function(ts, mw) {
  X = data.frame(NULL)
  for(i in 1:(length(ts)-mw)) { X = (rbind(X, ts[i:(mw+i-1)])) }
  X
}

GetY = function(ts, mw) {
  ts[(mw+1):length(ts)]
}

GetXRegs = function(ts, regs, mw) {
  X = data.frame(NULL)
  for(i in 1:(length(ts)-mw)) { 
    x = ts[i:(mw+i)]; x
    for(j in 1: length(regs)) { 
      x = c(x, regs[,j][mw+i]); x
    }
    X = rbind(X, x); X
  }
  X=X[,c(1:mw, (mw+2):ncol(X),mw+1)]
  X
}

GetFunValue = function(mw, nPers) {
  f=numeric(0)
  for(i in 1:(nPers-1)) { f=c(f, ts[((i*mw)+1)]) }
  f=c(f,ts[length(ts)])
  f
}

CalcSax = function(tss, mw, w, alpha) {
  ncols=ncol(tss)
  tsn=data.frame()
  sax=data.frame()
  f=numeric(0)
  for(i in 1:nrow(tss)) {
    tsss = as.numeric(tss[i,])
    z=ZNorm(tsss)
    f=c(f, z[(ncols)])
    p=PAA(z[1:(ncols-1)],w)
    tsn=rbind(tsn,p)
    s=convert.to.SAX.symbol(p,alpha)
    sax=rbind(sax,s)
  }
  sax$f=f; 
  sax
}

CalculateSax = function(data, alpha, wSize) {
  ncols=ncol(data)
  tsn=data.frame()
  sax=data.frame()
  for(i in 1:nrow(data)) {
    tss = as.numeric(data[i,])
    z=ZNorm(tss)
    p=PAA(z[1:ncols],wSize)
    tsn=rbind(tsn,p)
    s=convert.to.SAX.symbol(p,alpha)
    sax=rbind(sax,s)
  }
  sax
}


CalcDist = function(sax, alpha, w, n) {
  dist = matrix(nrow=alpha,ncol=alpha)
  for(i in 1:alpha) {
    for(j in 1:alpha) {
      dist[i,j] = MINDIST.SAX(sax[i,1:w], sax[j,1:w], alpha, n)
    }
  }
  dist = as.dist(dist)
  dist
}

HieClustering = function(dist, k) {
  if(length(dist[dist>0])==0) { k=1 }
  fit = hclust(dist, method="ward.D")
  plot(fit)
  groups = cutree(fit, k=k) 
  groups
}

Anova = function(grs, f) {
  if(sd(grs)==0) { 
    print('No groups'); 
    return(1) 
  }
  else {
    an = data.frame(1:length(grs), grs, f)
    a = aov(f~grs,data=an) 
    print(summary(a))
    summary(a)[[1]][["Pr(>F)"]][[1]]
  }
}

TsFuncEval = function(ts, mw, w, alpha, n) {
  tss=Split(ts,mw);tss
  f=GetFunValue(mw, length(tss));f
  sax = CalcSax(tss);sax
  dist = CalcDist(sax, alpha, w, n); dist
  grs = HieClustering(dist, k=3); grs
  Anova(grs, f)
}

CalcGroupedSd <- function(groups, n) {
  sum=0
  for(i in 1:length(groups)) {
    m = mean(groups[[i]])
    for(j in 1:length(groups[[i]])) {  
      sum = sum +  abs(groups[[i]][j] - m) 
    }
  }
  sum/n
}

CalcVarCoeff <- function(data) {
  vc = sd(data)/mean(data)
  round(vc*100,2)
}

DataInPlusOneGroups <- function(groups) {
  sum = 0
  for(i in 1:length(groups)) {
    n = length(groups[[i]])
    if(n>1) { sum = sum + n }
  }
  sum
}

CalcLearneable <- function(ts, regs=NULL, mw, alpha, w, n=100, print=TRUE) {
  if(is.null(regs) | length(regs) == 0) { tss=GetX(ts,mw+1) }
  else { tss=GetXRegs(ts,regs,mw); }
  sax=CalcSax(tss, mw, w, alpha)
  saxOr = OrderDf(sax); saxOr
  groups = GroupEquals(saxOr); groups
  n1=nrow(sax);n1
  sd1 = CalcGroupedSd(groups, n1); sd1
  n2=DataInPlusOneGroups(groups); n2
  sd2 = CalcGroupedSd(groups, n2); sd2
  prop = round((n2/n1)*100,2)
  if(print) print(groups)
  c(round(sd1,2),round(sd2,2), n1, n2, prop)
}


SelectMw <- function(ts, regs=NULL, mwMin, mwMax, alpha, wProp, n, repPropThres) {
  bestMw = -1
  bestS2 = 1000
  bestProp = -1
  for(i in mwMin:mwMax) {
    w = i %/% (1/wProp)
    res = CalcLearneable(ts=ts, regs, mw=i, alpha=alpha, w=w, n=100, print=FALSE)
    if(res[5] > repPropThres && res[2] < bestS2) { bestS2=res[2]; bestMw=i; bestProp = res[5]; }
  }
  c(bestMw, bestS2, bestProp)
}

CalcErrorTs <- function(model, horizon, absolute=FALSE) {
  errTs= numeric(0)
  for(i in 1:(length(model$ts)-model$mw-horizon+1)) {
    out = model$FcstFrom(i, horizon)
    out[out<0] = 0
    exp = model$ts[(i+model$mw):(i+model$mw+horizon-1)]
    err = mean((out-exp)/horizon);
    if(absolute) { errTs = c(errTs, abs(err)) } else { errTs = c(errTs, err) }
  }
  errTs
}

CalcFcstError <- function(ts, regs=NULL, regsFcst=NULL, it=5, nHidden, mw, horizon, plot=FALSE) {
  colors = c('blue', 'red', 'green', 'pink', 'brown', 'grey')
  names = vector(mode='list', length=0)
  errors = vector(mode='list', length=0)
  range=0.001; maxIt=6000; normFactor=100; normMinFactor=0.2; normMaxFactor = 0.2; maxVarRatio=1.3; histForVR=mw*2

  tdnn = TDNN$new(ts=ts, nHidden=nHidden, dec=-1, range=range, maxIt=maxIt, mw=mw, normFactor=normFactor, normMinFactor=normMinFactor, normMaxFactor = normMaxFactor, maxVarRatio=maxVarRatio, histForVR=histForVR)
  o = tdnn$Forecast(horizon=horizon, it=it); 
  err = CalcErrorTs(tdnn, horizon, absolute=TRUE);
  names = cbind(names, 'univ')
  res = data.frame(err)
  if(plot) { plot(err, type='l', lwd=2, col='black') }
  
  if(!is.null(regs)) { 
    for(i in 1:length(regs)) {
      Regs=data.frame(regs[i])
      RegsFcst = data.frame(regsFcst[i])
      tdrnn = TDRNN$new(ts=ts, regs=Regs, mw=12, nHidden=nHidden, dec=0, range=0.5, maxIt=maxIt, checkLag=TRUE,maxVarRatio=maxVarRatio, histForVR=histForVR)
      o2 = tdrnn$Forecast(RegsFcst, horizon=horizon, it=it);
      err = CalcErrorTs(tdrnn, horizon, absolute=TRUE);
      names = cbind(names, paste('multi', i, sep=''))
      res = data.frame(res, err)
      if(plot) { lines(err, type='l', lwd=2, col=colors[i]) }
    }
    
    if(length(regs)>=2) {
      Regs=data.frame(regs[1], regs[2])
      RegsFcst = data.frame(regsFcst[1], regsFcst[2])
      tdrnn = TDRNN$new(ts=ts, regs=Regs, mw=12, nHidden=nHidden, dec=0, range=0.5, maxIt=maxIt, checkLag=TRUE,maxVarRatio=maxVarRatio, histForVR=histForVR)
      o2 = tdrnn$Forecast(RegsFcst, horizon=horizon, it=it);
      err = CalcErrorTs(tdrnn, horizon, absolute=TRUE);
      names = cbind(names, paste('multi12', i, sep=''))
      res = data.frame(res, err)
      if(plot) { lines(err, type='l', lwd=2, col='pink') }
    }
    
    if(length(regs)==3) {
      Regs=data.frame(regs[2], regs[3])
      RegsFcst = data.frame(regsFcst[2], regsFcst[3])
      tdrnn = TDRNN$new(ts=ts, regs=Regs, mw=12, nHidden=nHidden, dec=0, range=0.5, maxIt=maxIt, checkLag=TRUE,maxVarRatio=maxVarRatio, histForVR=histForVR)
      o2 = tdrnn$Forecast(RegsFcst, horizon=horizon, it=it);
      err = CalcErrorTs(tdrnn, horizon, absolute=TRUE);
      names = cbind(names, paste('multi23', i, sep=''))
      res = data.frame(res, err)
      if(plot) { lines(err, type='l', lwd=2, col='pink') }
  
      Regs=data.frame(regs[1], regs[3])
      RegsFcst = data.frame(regsFcst[2], regsFcst[3])
      tdrnn = TDRNN$new(ts=ts, regs=Regs, mw=12, nHidden=nHidden, dec=0, range=0.5, maxIt=maxIt, checkLag=TRUE,maxVarRatio=maxVarRatio, histForVR=histForVR)
      o2 = tdrnn$Forecast(RegsFcst, horizon=horizon, it=it);
      err = CalcErrorTs(tdrnn, horizon, absolute=TRUE);
      names = cbind(names, paste('multi13', i, sep=''))
      res = data.frame(res, err)
      if(plot) { lines(err, type='l', lwd=2, col='brown') }
    
      Regs=data.frame(regs[1], regs[2], regs[3])
      RegsFcst = data.frame(regsFcst[1], regsFcst[2], regsFcst[3])
      tdrnn = TDRNN$new(ts=ts, regs=Regs, mw=12, nHidden=nHidden, dec=0, range=0.5, maxIt=maxIt, checkLag=TRUE,maxVarRatio=maxVarRatio, histForVR=histForVR)
      o2 = tdrnn$Forecast(RegsFcst, horizon=horizon, it=it);
      err = CalcErrorTs(tdrnn, horizon, absolute=TRUE);
      names = cbind(names, paste('multi123', i, sep=''))
      res = data.frame(res, err)
      if(plot) { lines(err, type='l', lwd=2, col='grey') }
    }
  }
  
  colnames(res) = names
  res
}



