# IMPORTS ===========================================================================================================================================================================================

library(R6)
library(MiscFunctions)


# SlowMovingMethods ==========================================================================================================================================================================================


    
    GetLoadFactor = function(ts) {
      length(ts[ts>0])/length(ts)
    }

    RemovePrd = function(ts, ini, end) {
       c(ts[1:(ini-1)], ts[(end+1):length(ts)])
    }
     
    PeriodsToNA = function(ts, iniEff, endEff) {
       for(i in 1: length(iniEff)) {  ts[iniEff[i]:endEff[i]] = NA }
       ts
    }
     
    InterpPrdWithMeans = function(ts, ini, end, n=1) {
       halfSize = ((end-ini+1)*n)/2
       preIni = ini-halfSize; if(preIni<1) preIni = 1; preEnd = ini-1
       postEnd = end+halfSize; ; postIni = end+1
       m = mean(c(ts[preIni:preEnd], ts[postIni:postEnd]))
       if(postEnd > length(ts)) { m = mean(preIni:preEnd) }
       ts[ini:end] = m
       ts
    }
     
    InterpPrdsWithMeans = function(ts, inis, ends, n) {
       for(i in 1:length(inis)) { ts = InterpPrdWithMeans(ts, inis[i], ends[i], n) }  
       ts
    }
     
    ConvertToInt = function(ts) {
       its = numeric(0)
       rest = 0
       for(i in 1:length(ts)) {
         rest = rest + ts[i]
         restInt = round(rest)
         its[i] = restInt; rest = rest - restInt
       }
       its
    }
    
 
