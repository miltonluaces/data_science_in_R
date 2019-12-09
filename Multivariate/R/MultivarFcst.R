# IMPORTS ===========================================================================================================================================================================================

library(R6)
library(MiscFunctions)


# CLASS MULTIVARIATE FORECASTING ==========================================================================================================================================================================================

MultivarFcst = R6Class(
  
  classname='MultivarFcst', 
  
  public=list(
    
    # Public Fields ----
    
    ts=NULL,
    regs=NULL,
    selRegs=NULL,
    mw=6,
    nHidden=6,
    decay=-1, 
    maxIt=100,
    data=0,
    alpha=0.05,
    n=1,
    lfThres=0.5,
    pValues=0,
    prop=-1,
    loadFactor=0,
    tsRecNull=FALSE,
    outThres=0.99,
    nForRecurrFcst=300,
    
    tsRec=0,
    tsReg=0,
    reSt=0,
    
    reStFcst=0,
    recurrFcst=0,
    regeffFcst=0,
    totalFcst=0,
    
    
    # Public Methods ----     
    
    initialize = function(mw=6, nHidden=6, decay=-1, maxIt=100, alpha=0.05, n=1, lfThres=0.5, outThres=0.99, regeffDecay=0.1, nForRecurrFcst=300) {
      self$nHidden=nHidden
      self$decay=decay
      self$mw=mw
      self$maxIt=maxIt
      self$alpha=alpha
      self$n=n
      self$lfThres=lfThres
      self$outThres=outThres
      self$nForRecurrFcst=nForRecurrFcst
      
      private$multReg = MultivarRegression$new()
      private$multReg$LoadParamsNeuNet(nHidden, regeffDecay, maxIt)
    },
    
    LoadData = function(ts, cal=0, iniPer, endPer, actPer=0, regs) {
      if(length(iniPer) != length(endPer)) { print('Error.Different length in effect periods'); return() }
      if(nrow(regs) != length(iniPer)) { print('Error. Different number of effect periods than regressor values'); return() }
      if(iniPer[length(iniPer)] > length(ts)) { print('Error. IniPer must not exceed ts length') }
      if(endPer[length(endPer)] > length(ts)) { endPer[length(endPer)] = length(ts) }
      if(actPer != 0 && length(actPer) != length(iniPer)) { print('Error. IniPer and ActPer must have same length') }
      self$ts=ts
      self$regs=regs
      self$regs$fVal = -1
      if(cal != 0 && length(cal) >= length(ts)) { 
        self$ts=EliminateHolidays(ts, cal) 
        private$calFcst = cal[(length(ts)+1):length(cal)] 
      }
      self$loadFactor = GetLoadFactor(self$ts)
      
      if(private$IsSparse()) {
        if(self$loadFactor==0) { self$tsRec = self$ts; self$tsRecNull = TRUE } else { self$tsRec = private$CalcSparseRecurrentTs(self$ts, iniPer, endPer, self$alpha, n=self$n) }
      }
      else {
        res = CalcRecurrentTs(ts=self$ts, iniEff=iniPer, endEff=endPer, alpha=self$alpha, n=self$n)
        self$tsRec = res$tsRec
        self$pValues = res$pValues
        self$prop = res$prop
      }  
      res = CalcLim(self$tsRec, self$outThres)
      tsf = Truncate(self$tsRec, res$lim)
      self$tsRec = tsf
      lastTsRec = GetLastTs(self$tsRec, self$nForRecurrFcst)
      
      self$tsReg = self$ts - self$tsRec
      self$reSt = GetRegEffStats(self$tsReg, iniPer, endPer)
      self$regs$fVal = self$reSt
      if(length(actPer)<=1) { self$selRegs = self$regs } else { self$selRegs = SelectDfRows(self$regs, actPer) }
      private$tdnn = TDNN$new(ts=lastTsRec, mw=self$mw, nHidden=self$nHidden, dec=self$decay, cal=cal, range=0.5, maxIt=self$maxIt, normFactor=1, normMinFactor=0.1, normMaxFactor = 0.1, maxVarRatio=1.4, histForVR=12, nnType='nnet')
      private$multReg$LoadForCalculate(self$selRegs)
      #self$prop = 123456
    },
    
    Forecast = function(horizon=0, it=5, regsFcst, iniPer, endPer) {
      if(self$tsRecNull==TRUE) {
        self$recurrFcst = rep(0, horizon)
      } else {
        self$recurrFcst = private$tdnn$Forecast(horizon=horizon, it=it)
        if(private$IsSparse()) { self$recurrFcst = ConvertToInt(self$recurrFcst) }
      } 
      reStFcst = private$multReg$Calculate(regsFcst)[1:horizon]
      self$reStFcst = reStFcst[!is.na(reStFcst)]
      self$totalFcst = private$Join(self$recurrFcst, self$reStFcst, iniPer, endPer)
      self$regeffFcst = self$totalFcst - self$recurrFcst
      if(length(private$calFcst)>= horizon) { 
        self$recurrFcst = self$recurrFcst * private$calFcst[1:horizon] 
        self$regeffFcst = self$regeffFcst * private$calFcst[1:horizon]
        self$totalFcst = self$totalFcst * private$calFcst[1:horizon] 
      }
    }
  ),
  
  
  private = list(
    
     #Private Fields ----
  
     tdnn=NULL, 
     multReg=NULL,
     calFcst=NULL,
     
     #Private Methods ----
     Join = function(recurrFcst, reStFcst, reIniPer, reEndPer) {
       n = length(self$tsRec)
       reIniPer = reIniPer-n+1
       reEndPer = reEndPer-n+1
       totalFcst = recurrFcst
       p = length(reIniPer)
       for(i in 1:p) {
         for(j in reIniPer[i]:reEndPer[i]) { totalFcst[j] = totalFcst[j] + reStFcst[i]/p }
       }
       totalFcst
     },
     
     IsSparse = function() {
       self$loadFactor < self$lfThres
     },
     
     CalcSparseRecurrentTs = function(ts, iniEff, endEff, alpha, n) {
       its = InterpPrdsWithMeans(ts, iniEff, endEff, n)
       mw = round(mean(endEff - iniEff + 1))
       if(sum(its)==0) { pValue = 0} else { pValue = wilcox.test(its, alternative='greater', exact=FALSE)$p.value }
       ma = numeric(0)
       if(pValue < alpha) { ma = rep(0, length(its)); self$tsRecNull=TRUE } else { ma = CalcMA(its, mw); self$tsRecNull=FALSE }
       ma
     }
  )  
)   
