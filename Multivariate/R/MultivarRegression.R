# IMPORTS ===========================================================================================================================================================================================

library(R6)
library(MiscFunctions)
library(nnet)
library(randomForest)
library(NeuralNetTools)
library(e1071)

# CLASS INTERVALFCST ==========================================================================================================================================================================================

MultivarRegression = R6Class(
  
  classname='MultivarRegression', 
  public=list(
    
    # Public Fields ----
    
    method='',
    model=NULL,
    
    nHidden=0,
    decay=-1,
    maxIt=100,
    
    nTree=100,
    mTry=5,
    
    # Public Methods ----     
    
    initialize = function() {
    },
  
    LoadParamsNeuNet = function(nHidden=6, decay=-1, maxIt=100) {
      self$method='NeuNet'
      self$nHidden = nHidden
      self$decay = decay
      self$maxIt = maxIt
    },
    
    LoadParamsRndFor = function(nTree=100, mTry=5) {
      self$method='RndFor'
      self$nTree = nTree
      self$mTry = mTry
    },
    
    LoadParamsLinMod = function() {
      self$method='LinMod'
      
    },
    
    LoadParamsSuVeMa = function() {
      self$method = 'SuVeMa'
    },
    
    LoadForCalculate = function(data) {
      if(class(data) =='numeric') { data=data.frame(data) }
      if(ncol(data)==2) { 
        data = cbind(data[,1], data[,1], data[,2])
        colnames(data) = c('X1', 'X2', 'Y')
      }
      private$data = data
      colnames(private$data)[ncol(data)] = 'Y'
      private$p = ncol(private$data)-1
      Xdf = private$data[,1:private$p]
      Y = private$data[,ncol(private$data)]
      if(self$method=='NeuNet') { self$model = nnet(Y~., private$data, size=self$nHidden, linout=T, trace=F, decay=self$decay, rang=0.5, maxit=self$maxIt) }
      if(self$method=='RndFor') { self$model = randomForest(Y ~., data=private$data, type=regression, mtry=min(self$mTry, private$p), importance=TRUE, na.action=na.omit, ntree=self$nTree) }
      if(self$method=='LinMod') { self$model = lm(Y ~., data=private$data, na.action=na.omit) }
      if(self$method=='SuVeMa') { self$model = svm(Y ~., data=private$data, na.action=na.omit, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))) }
    },
    
    Calculate = function(X) {
      if(ncol(X)==1) { X = cbind(X,X) }
      newData = rbind(X) 
      colnames(newData) = colnames(private$data[,1:(ncol(private$data)-1)])
      newData=data.frame(newData)
      y = predict(self$model, newData)
    },
    
    CalcImportance = function() {
      gar = garson(self$model, bar_plot=FALSE)
      gar$rel_imp
    }
  ),
  
  
  private = list(
    
    # Private Fields ----
    
    data=0,
    p=0
    
    # Private Methods ----
  )  
)    




