# IMPORTS ===========================================================================================================================================================================================

library(R6)
library(MiscFunctions)


# CLASS MULTIVARIATE FORECASTING ==========================================================================================================================================================================================

MultivarForecasting = R6Class(
  
  classname='MultivarForecasting', 
  
  public=list(
    
    # Public Fields ----
    
    ts=NULL,
    mw=6,
    multReg=NULL,
    nHidden=6,
    decay=-1, 
    maxIt=100,
    data=0,
    uniFcst=0,
    
    # Public Methods ----     
    
    initialize = function() {
      self$multReg = MultivarRegression$new()
    },
    
    LoadParamsNeuNet = function(mw=6, nHidden=6, decay=-1, maxIt=100) {
      self$nHidden=nHidden
      self$decay=decay
      self$mw=mw
      self$maxIt=maxIt
      self$multReg$LoadParamsNeuNet(nHidden, decay, maxIt)
    },
    
    LoadParamsRndFor = function() {
      self$multReg$LoadParamsRndFor()
    },
    
    LoadParamsLinMod = function() {
      self$multReg$LoadParamsLinMod()
    },
    
    LoadData = function(ts,regs, horizon, it) {
      self$ts = ts
      range=0.5; maxIt=self$maxIt; normFactor=1; normMinFactor=0.2; normMaxFactor = 0.2; maxVarRatio=1.3; histForVR=12
      private$tdnn = TDNN$new(ts=ts, nHidden=self$nHidden, dec=self$decay, range=range, maxIt=self$maxIt, mw=self$mw, normFactor=normFactor, normMinFactor=normMinFactor, normMaxFactor = normMaxFactor, maxVarRatio=maxVarRatio, histForVR=histForVR)
      self$uniFcst = private$tdnn$Forecast(horizon=horizon, it=it)
      osf = private$tdnn$GetOneStepFcstTs()
      
      ini = self$mw+1; end = nrow(regs)   
      osf = osf[ini:end]
      ts = ts[ini:end]
      self$data = data.frame(osf , regs[ini:end,], ts) 
      
      self$multReg$LoadForCalculate(self$data)
    },
    
    Forecast = function(horizon=0, regsFcst) {
      if(horizon==0 | horizon > nrow(regsFcst)) { horizon=nrow(regsFcst)}
      X = data.frame(self$uniFcst, regsFcst)
      fcst = self$multReg$Calculate(X)
      fcst[1:horizon]
    }, 
    
    FcstFrom = function(start, horizon) {
      if(horizon==0 | horizon > nrow(regsFcst)) { horizon=nrow(regsFcst)}
      fromUniFcst = private$tdnn$FcstFrom(start, horizon)
      fromRegsFcst =  regs[start:(start+horizon-1),]
      X = data.frame(fromUniFcst, fromRegsFcst)
      fcst = self$multReg$Calculate(X)
      fcst[1:horizon]
    }
  ),
  
  
  private = list(
    
     #Private Fields ----
  
     tdnn=NULL 
  
    
     #Private Methods ----
  )  
)    
