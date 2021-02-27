# IMPORTS =====================================================================================================================================================

library(R6)


Nrm = R6Class(
  
  classname = 'Nrm', 
  
  public = list(
    
    # Public Fields ----
    
    xMin=0, 
    xMax=0,
    xRange=0,
    factor=1,
    
    # Public Methods ----      
    
    initialize = function(X, factor=1, minFactor=0, maxFactor=0) {
      self$factor=factor
      self$xMin = min(X) * (1 - minFactor)  
      self$xMax = max(X) * (1 + maxFactor)  
      self$xRange = self$xMax - self$xMin
      if(self$xRange == 0) { self$xRange = self$xMax }
    },
    
    Norm = function(x) { 
      nx = (x - self$xMin)/self$xRange
      fnx =  nx * self$factor
      fnx
    },

    UnNorm = function(nx) { 
      x = (nx/self$factor) * self$xRange + self$xMin 
      x
    }
  )

)  
    
NormDF = function(df) {
  for(i in 1:ncol(df)) {
    nrm = Nrm$new(df[,i]) 
    df[,i] = nrm$Norm(df[,i])
  }
  df
}