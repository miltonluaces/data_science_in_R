# IMPORTS ===========================================================================================================================================================================================

library(nnet)
library(R6)
library(MiscFunctions)
library(RANN)
library(NeuralNetTools)

# CLASS REGANALYSIS ===========================================================================================================================================================================================


# IMPORTS ===========================================================================================================================================================================================

library(R6)
library(MiscFunctions)


# CLASS REGRESSOR ANALYSIS ==========================================================================================================================================================================================

RegAnalysis = R6Class(
  
  classname='RegAnalysis', 
  
  public=list(
    
    # Public Fields ----
    
    regs=0,
    impMeans=0,
    X=0, 
    Xi=0,
    Yi=0,
    dists=0,
    d=0,
    m=-1,
    s=-1,
    pValue=-1,
    
    # Public Methods ----     
    
    initialize = function() {
      
    },
    
    LoadData = function(regs, impMeans) {
      self$regs = regs
      self$impMeans = impMeans
      self$X = regs[,1:(ncol(regs)-1)]
      norms = NULL
      for(i in 1:ncol(self$X)) {
        norm = Nrm$new(self$X[,i]); 
        self$X[,i] = norm$Nrm(self$X[,i]) 
        private$norms = c(private$norms, norm)
      }
      self$Xi = self$X * 1/self$impMeans
      self$dists = nn2(self$Xi,self$Xi)
      self$m = mean(self$dists$nn.dists[,2])
      self$s = sd(self$dists$nn.dists[,2])
    },
    
    Calculate = function(regsFcst) {
      Y = regsFcst
      for(i in 1:ncol(self$X)) { Y[,i] = private$norms[[i]]$Norm(Y[,i]) }
      self$Yi = Y * 1/self$impMeans 
      self$d = nn2(self$Xi,self$Yi)
      self$pValue = 1-pnorm(self$d$nn.dists[,1], mean=self$m, sd=self$s); self$pValue
    }
  ),
  
  
  private = list(
    
    #Private Fields ----
    
    norms= NULL 
    
    #Private Methods ----
    
  )  
)


#WRegAnalysisInit = function(regs, impMeans) { 
#  wra <<- list()
#  wra$ra <<- RegAnalysis$new() 
#  wra$ra$LoadData(regs, impMeans)
  
#  loadPrps <<- list()
#  loadPrps$Xi <<- wra$ra$Xi
#  loadPrps$dmatIndex <<- wra$ra$dists$nn.idx
#  loadPrps$dmatDists <<- wra$ra$dists$nn.dists
#  loadPrps$m <<- wra$ra$m
#  loadPrps$s <<- wra$ra$s
#  loadPrps
#}

#WRegAnalysisCalc = function(regsFcst) {
#  wra$ra$Calculate(regsFcst=regsFcst)
#  fcstPrps <<- list()
#  fcstPrps$Yi <<- wra$ra$Yi
#  fcstPrps$dIndex <<- wra$ra$d$nn.idx
#  fcstPrps$dDists <<- wra$ra$d$nn.dists
#  fcstPrps$pValue <<- wra$ra$pValue
#  fcstPrps 
#}


SelectRegressors = function(ts, regs) {
  res = data.frame(row.names=c('lag','r2'))
  for(i in 1:length(regs)) {
    if(min(regs[i]) == max(regs[i])) { 
      lag=0; r2=0 
    }
    else {
      lag = GetLag(ts, regs[i], plot=FALSE)
      regSh = Shift(as.numeric(unlist(regs[i])), lag)
      rs = as.numeric(unlist(regSh))
      r2 = (cor(ts, rs, use='na.or.complete'))^2
    }
    res = cbind(res, c(round(lag,0), round(r2,2)))
  }
  colnames(res) = rep(1:length(regs))
  res
}


MRAE = function(data, nHidden, decay, maxIt, it, calcImportance=FALSE, combRegs='', combCols='') {
  c = ncol(data)
  mrae = numeric(it)
  imps = numeric(0)
  for(i in 1:it) {
    mr = MultivarRegression$new()
    mr$LoadParamsNeuNet(nHidden=6, decay=0.001, maxIt=300)
    mr$LoadForCalculate(data)
    m = mean(data[,c])
    mae = mean(abs(mr$Calculate(data[1:(c-1)]) - data[,c]))
    mrae[i] = round(mae/m, 3)*100
    imp = mr$CalcImportance()
    normImp = imp/sum(imp)
    imps = rbind(imps, normImp)
  }
  meanImp = apply(imps, 2, mean)
  sdImp = apply(imps, 2, sd)
  meanErr = mean(mrae)
  sdErr = sd(mrae); sdErr[is.na(sdErr)]=0
  res = list()
  res$mraeErr = mrae
  res$meanSt = round(m * meanErr/100, 2)
  res$meanErr = round(meanErr,2)
  res$sdErr = round(sdErr,2)
  res$impMeans = round(meanImp,2)
  res$impSds = round(sdImp,2)
  res$combRegs = combRegs
  res$combCols = combCols
  res
}


MRAESObsolete = function(data, nHidden=6, decay=0.001, maxIt=300, it=5) {
  mraes = NULL
  n=ncol(data)
  d1 = data[,c(1,n)] 
  mrae = MRAE(data=d1, nHidden, decay, maxIt, it); mraeRes = c(mrae$meanErr, mrae$sdErr)
  mraes = rbind(mraes, mraeRes)
  d2 = data[,c(2,n)] 
  mrae = MRAE(data=d2, nHidden, decay, maxIt, it); mraeRes = c(mrae$meanErr, mrae$sdErr)
  mraes = rbind(mraes, mraeRes)
  d3 = data[,c(3,n)] 
  mrae = MRAE(data=d3, nHidden, decay, maxIt, it); mraeRes = c(mrae$meanErr, mrae$sdErr)
  mraes = rbind(mraes, mraeRes)
  d12 = data[,c(1,2,n)] 
  mrae = MRAE(data=d12, nHidden, decay, maxIt, it); mraeRes = c(mrae$meanErr, mrae$sdErr)
  mraes = rbind(mraes, mraeRes)
  d13 = data[,c(1,3,n)] 
  mrae = MRAE(data=d13, nHidden, decay, maxIt, it); mraeRes = c(mrae$meanErr, mrae$sdErr)
  mraes = rbind(mraes, mraeRes)
  d23 = data[,c(2,3,n)]
  mrae = MRAE(data=d23, nHidden, decay, maxIt, it); mraeRes = c(mrae$meanErr, mrae$sdErr)
  mraes = rbind(mraes, mraeRes)
  d123 = data[,c(1,2,3,n)] 
  mrae = MRAE(data=d123, nHidden, decay, maxIt, it); mraeRes = c(mrae$meanErr, mrae$sdErr)
  mraes = rbind(mraes, mraeRes)
  rgs = c(1, 2, 3, 12, 13, 23, 123)
  res = data.frame(row.names=rgs, mraes)
  res = cbind(rgs, res)
  names(res) = c('Rgs', 'Mean', 'Sd'); res
}


MRAES = function(data, regCodes=0, nHidden=6, decay=0.001, maxIt=300, it=5, trace=FALSE) {
  if(length(regCodes) < ncol(data)) { regCodes = seq(1:ncol(data)) }
  fun = data[length(data)]
  nRegs = regCodes[(length(regCodes)-1)]
  bc = BinaryCombinatory(nRegs)
  mraes = NULL
  for(i in 1:nrow(bc)) {
    rInd = which(bc[i,]==1)
    sel = regCodes%in%rInd
    selIdx = which(sel==TRUE)
    dataTry = data[,selIdx]
    dataTry = cbind(dataTry, fun)
    combName = toString(rInd)
    if(combName != '') {
      if(trace==TRUE) { print(paste(toString(rInd), ' : ', toString(selIdx))) }
      mrae = MRAE(data=dataTry, nHidden=nHidden, decay=decay, maxIt=maxIt, it=it, combRegs=toString(rInd), combCols=toString(selIdx)); mraeRes = c(mrae$combRegs,  mrae$combCols, mrae$meanSt, mrae$meanErr, mrae$sdErr)
      mraes = rbind(mraes, mraeRes)
    }
  }
  colnames(mraes) = c('Regs', 'Columns', 'Error', 'RelError', 'RelSd')
  mraes = data.frame(mraes, stringsAsFactors = FALSE, row.names = NULL)
  mraes
}

ConvertCatDf = function(df) { 
  regCodes= numeric(0)
  df = FactorToChar(df)
  df[df==''] = 'Null'
  df = CharToFactor(df)
  dfCat = data.frame(row.names=1:nrow(df))
  c=1
  for(i in 1:(length(df)-1)) {
    if(class(df[,i]) == 'factor') {
      Cat = factor(df[,i])
      dummies = model.matrix(~Cat+0)
      dfCat = cbind(dfCat, dummies)    
      regCodes = c(regCodes, rep(i,(ncol(dummies))))
      c = c+ncol(dummies)
    } else {
      dfCat = cbind(dfCat, df[,i])   
      colnames(dfCat)[c] = paste('Num',i,'')
      regCodes = c(regCodes, i)
      c=c+1
    }
  }
  dfCat
  res = list()
  res$regCodes = regCodes
  res$dfCat = dfCat
  res
}

#ConvertCatDf2 = function(numDf, catDf, f) {
#  numDf = data.frame(numDf)
#  catDf = data.frame(catDf)
#  f = data.frame(f)
#  df = data.frame(numDf, catDf, f)
#  ConvertCatDf(df)
#}

ConvertCatDf2 = function(numDf, catDf, f) {
  numDf = data.frame(numDf)
  catDf = data.frame(catDf)
  f = data.frame(f)
  df = data.frame(numDf, catDf, f)
  print(paste('nrows:', nrow(df), 'ncols:', ncol(df)))
  ConvertCatDf(df)
}