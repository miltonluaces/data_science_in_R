# CLASS TDRNN: TIME DELAY NN WITH REGRESSORS  ================================================================================================================================================

library(R6)
library(nnet)
library(MiscFunctions)

TDRNN = R6Class(
  
  classname='TDRNN', 
  
  public=list(
  
    # Public Fields ----
    
    nn=NULL,
    mw=0,
    dec=0,
    range=0,
    maxIt=0,
    nHidden=0,
    X=NULL,
    Y=NULL,
    negAllowed=FALSE,
    horizon=0,
    ffrom=0,
    minDecExp=4,
    maxVarRatio=1.3,
    histForVR=6,
    
    ts=NULL,
    tsNor=NULL,
    
    lags=NULL,
    checkLag=FALSE,
    
    
    # Constructor ----      
    
    initialize = function(ts, regs, mw, nHidden, dec=0, minDecExp=4, range=0.5, maxIt=100, negAllowed=FALSE, normFactor=1, normMinFactor=0, normMaxFactor=1, checkLag=FALSE, cal=0, maxVarRatio=1.3, histForVR=6) {
      # init
      self$dec = dec
      self$range = range
      self$maxIt = maxIt
      self$nHidden = nHidden
      self$mw = mw
      self$negAllowed = negAllowed
      self$minDecExp = minDecExp
      # var control
      self$maxVarRatio=maxVarRatio
      self$histForVR=histForVR
      
      # lag
      if(checkLag==TRUE) {
        self$lags = private$GetLags(regs)
        shReg = private$ShiftRegs(regs, self$lags)
        reg = shReg
      }
      #norm
      ts[ts<0]=0
      if(normFactor<=0) { normFactor=1; normMinFactor=0; normMaxFactor=1 }
      self$tsNor = Nrm$new(ts, normFactor, normMinFactor, normMaxFactor)
      self$ts = self$tsNor$Norm(ts)
      for(i in 1:length(regs)) { 
         regNor = Nrm$new(regs[i], normFactor, normMinFactor, normMaxFactor) 
         regs[i] =regNor$Norm(regs[i])
         private$regsNor = cbind(private$regsNor, regNor)
      }
      private$regs = regs
      self$X = private$GetXRegs(self$ts, private$regs, self$mw) 
      self$Y = private$GetY(self$ts, self$mw); 
      private$SetNames()
    },
  
    # Public Methods ----      
    
    Forecast = function(regsFcst, horizon=1, it=1) {
      for(i in 1:length(regsFcst)) { 
        regNor = private$regsNor[[i]]; 
        regsFcst[i] = regNor$Norm(regsFcst[i]) 
      } 
      colnames(regsFcst) = colnames(private$regs)
      private$regsWithFcst = rbind(private$regs, regsFcst)
      self$horizon = horizon
      bestMae = .Machine$double.xmax
      bestFcst = 0; bestDec=self$dec
      for(i in 0:(it-1)) {
        if(self$dec < 0 && i <= 4*self$minDecExp) dec = GetExponentBy5(self$minDecExp, i) else dec = bestDec
        self$nn = nnet(self$X, self$Y, size=self$nHidden, linout=T, trace=F, decay=self$dec, rang=0.5, maxit=6000) 
        fcst = self$FcstFrom(length(self$ts)-self$mw+1, horizon)
        mae = private$CrossValidation(horizon)
        ratio = CalcSdRatio(self$ts, self$histForVR, fcst) #print(ratio)
        if(ratio <= self$maxVarRatio && mae < bestMae) { bestMae = mae; bestDec=dec; bestFcst = fcst }
      }
      self$dec=bestDec
      #print(paste('MAE = ', mae, '%', sep=''))
      if(self$negAllowed==FALSE) { bestFcst[bestFcst<0] = 0 }
      forecast = self$tsNor$UnNorm(bestFcst)
      if(length(private$calFcst)>= horizon) { forecast = forecast * private$calFcst[1:horizon] }
      forecast
    },
    
    FcstFrom = function(start, horizon) {
      fcst = numeric(horizon)
      x = self$ts[start:(start+self$mw-1)]
      for(j in 1: length(private$regsWithFcst)) { x = c(x, private$regsWithFcst[,j][start+self$mw]) }
      for(i in 1:horizon) {
        y = predict(self$nn, x)
        fcst[i] = y
        if(i<12) {
          x = c(x[2:self$mw], y) 
          for(j in 1: length(private$regsWithFcst)) { x = c(x, private$regsWithFcst[,j][start+i+self$mw]) }
        }
      }
      fcst
    }
  ),

  
  private = list(
    
    # Private Fields ----
    
    calFcst=NULL,
    
    regs=NULL,
    regsWithFcst = NULL,
    regsNor=list(),
    
    # Private Methods ----
    
    CrossValidation = function(horizon) {
      aec=0
      for(i in 1:(length(self$ts)-self$mw-horizon+1)) {
        out = self$FcstFrom(i, horizon)
        out[out<0] = 0
        exp = self$ts[(i+self$mw):(i+self$mw+horizon-1)]
        aec = aec + sum(abs(out-exp))/horizon
      }
      mae = round((aec/sum(self$ts[1:(length(self$ts)-self$mw-horizon+1)])*100),2)
      mae    
    },
    
    Mae = function(out, exp) {
      sum(abs(out-exp))
    },
    
    GetXRegs = function(ts, regs, mw) {
      X = data.frame(NULL)
      for(i in 1:(length(ts)-mw)) { 
        x = ts[i:(mw+i-1)]; x
        for(j in 1: length(regs)) { 
          x = c(x, regs[,j][mw+i]); x
        }
        X = rbind(X, x); X
      }
      X
    },
    
    GetX = function(ts, mw) {
      X = data.frame(NULL)
      for(i in 1:(length(ts)-mw)) { X = rbind(X, ts[i:(mw+i-1)]) }
      X
    },
    
    GetY = function(ts, mw) {
      y = ts[(mw+1):length(ts)]
      cbind(y)
    },
    
    GetLast = function(ts, n) {
      ts[(length(ts)-n+1):length(ts)]
    }, 
    
    SetNames = function() {
      xNames = NULL
      for(i in 1:self$mw) { xNames = c(xNames, paste('Tn-', self$mw-i+1, sep='')) }
      if(length(private$regs > 0)) {
        for(i in 1:length(private$regs)) { xNames = c(xNames, paste('R', i, sep='')) }
      }
      names(self$X) = xNames
      colnames(self$Y) = 'Tn'
    }, 
    
    GetLags = function(regs) {
      lags = numeric(length(regs))
      for(i in 1:length(regs)) { lags[i] = GetLag(ts, regs[i], plot=FALSE) }
      lags
    },
    
    ShiftRegs = function(regs, lags) {
      shiftRegs = NULL
      for(i in 1:length(regs)) {
        if(lags[i] > 0) {
          shReg = Shift(regs[,i], lags[i])
          shReg[is.na(shReg)] = 0
          shiftRegs = cbind(shiftRegs, shReg) 
        }
      }
      shiftRegs
    }
  )
)