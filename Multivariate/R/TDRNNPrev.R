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
    lags=NULL,
    checkLag=FALSE,
    
    # Constructor ----      
    
    initialize = function(ts, regs, mw, nHidden, dec=0, range=0.5, maxIt=100, negAllowed=FALSE, normFactor=1, normMinFactor=0, normMaxFactor=1, checkLag=FALSE) {
      # init
      self$dec = dec
      self$range = range
      self$maxIt = maxIt
      self$nHidden = nHidden
      self$mw = mw
      self$negAllowed = negAllowed
      # lag
      if(checkLag==TRUE) {
        self$lags = private$GetLags(regs)
        shReg = private$ShiftRegs(regs, self$lags)
        reg = shReg
      }
      #norm
      if(normFactor<=0) { normFactor=1; normMinFactor=0; normMaxFactor=1 }
      private$tsNor = Nrm$new(ts, normFactor, normMinFactor, normMaxFactor)
      private$ts = private$tsNor$Norm(ts)
      for(i in 1:length(regs)) { 
         regNor = Norm$new(regs[i], normFactor, normMinFactor, normMaxFactor) 
         regs[i] =regNor$Norm(regs[i])
         private$regsNor = cbind(private$regsNor, regNor)
      }
      private$regs = regs
      self$X = private$GetXRegs(private$ts, private$regs, self$mw) 
      self$Y = private$GetY(private$ts, self$mw); 
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
      bestMae = .Machine$double.xmax
      bestFcst = 0
      for(i in 1:it) {
        self$nn = nnet(self$X, self$Y, size=self$nHidden, linout=T, trace=F, decay=self$dec, rang=self$range, maxit=self$maxIt) 
        fcst = private$FcstFrom(length(private$ts)-self$mw+1, horizon)
        mae = private$CrossValidation(horizon)
        if(mae < bestMae) { bestMae = mae; bestFcst = fcst }
      }
      print(paste('MAE = ', mae, '%', sep=''))
      if(self$negAllowed==FALSE) { bestFcst[bestFcst<0] = 0 }
      private$tsNor$UnNorm(bestFcst)
    }
  ),

  
  private = list(
    
    # Private Fields ----
    
    ts=NULL,
    regs=NULL,
    regsWithFcst = NULL,
    tsNor=NULL,
    regsNor=list(),
    
    # Private Methods ----
    
    CrossValidation = function(horizon) {
      ae=0
      for(i in 1:(length(private$ts)-self$mw-horizon+1)) {
        out = private$FcstFrom(i, horizon)
        exp = private$ts[(i+self$mw):(i+self$mw+horizon-1)]
        ae = ae + sum(abs(out-exp))
      }
      mae = round((ae/sum(private$ts)*100),2)
      mae    
    },
    
    Mae = function(out, exp) {
      sum(abs(out-exp))
    },
    
    FcstFrom = function(start, horizon) {
      fcst = numeric(horizon)
      x = private$ts[start:(start+self$mw-1)]
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