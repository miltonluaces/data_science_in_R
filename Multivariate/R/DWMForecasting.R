# IMPORTS ===========================================================================================================================================================================================

library(R6)
library(nnet)
library(MiscFunctions)

# CLASS INTERVALFCST ===========================================================================================================================================================================================


DWMForecasting = R6Class(
  
  classname='DWMForecasting', 
  
  public=list(
    
    # Public Fields ----
    
    nHidden=6,
    decay=-1,
    maxIt=6000,
    mw=6,
    maxVarRatio=1.3,
    histForVR=12,
    
    # Public Methods ----     
    
    initialize = function(nHidden, decay, maxIt, mw, maxVarRatio, histForVR) {
      self$nHidden = nHidden
      self$decay = decay
      self$maxIt = maxIt
      self$mw = mw
      self$maxVarRatio = maxVarRatio
      self$histForVR = histForVR
    },
    
    Calculate = function(dts, iniDate, wDays, dHorizon, wHorizon, mHorizon) {
      wts = GenerateWTS(dts, iniDate)
      mts = GenerateMTS(dts, iniDate)
      
      dFcst = private$Forecast(dts, dHorizon)
      wFcst = private$Forecast(wts$wts, wHorizon)
      mFcst = private$Forecast(mts$mts, mHorizon)
      
      dwFcst = ApportionWTS(wFcst, wDays)
      dmFcst = ApportionMTS(mFcst, iniDate, wDays)
      
      dfc = dFcst
      wfc = dwFcst[(length(dFcst)+1):length(dwFcst)]
      mfc = dmFcst[(length(dwFcst)+1):length(dmFcst)]
      fcst = c(dfc, wfc, mfc); fcst
    }
  ),
  
  
  private = list(
    
    # Private Fields ----
    
    tdnn=0,
    
    # Private Methods ----

    Forecast = function(ts, horizon) {
      tdnn = TDNN$new(ts=ts, nHidden=self$nHidden, dec=self$decay, minDecExp=4, range=0.5, maxIt=self$maxIt, mw=self$mw,cal=0, maxVarRatio=self$maxVarRatio, histForVR=self$histForVR)
      tdnn$Forecast(horizon=horizon)
    }
  )  
)  
