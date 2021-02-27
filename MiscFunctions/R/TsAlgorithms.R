# IMPORTS =======================================================================================================

library(RANN)
library(qicharts)
library(qcc)


# DATA FUNCTIONS ==================================================================================================================

LoadData = function(skuId) {
  #skuId=417714
  env = list()
  env$obs = obsTs[[skuId]]
  env$reCal = reCalTs[[skuId]]
  env$cal = calTs[[skuId]]
  
  env$horizon = fcstHorizon[[skuId]]
  
  env$data = dataset[[skuId]]
  env$IniPeriod = env$data[,c('IniPeriod')]+1
  env$EndPeriod = env$data[,c('EndPeriod')]+1
  
  env$dataFcst = datasetFcst[[skuId]]
  env$fcstPer = env$dataFcst[,c('Fcst_IniPeriod', 'Fcst_EndPeriod')]
  
  env$data1 = env$data[,c('Price', 'TotalDays', 'AvgSales')]; env$X1 = env$data1[,1:2]
  env$data2 = env$data[,c('Price', 'TotalDays', 'Folleto', 'AvgSales')]; env$X2 = env$data2[,1:3]
  
  env$regsFcst = env$dataFcst[,c('Fcst_Price', 'Fcst_TotalDays')]
  env
}

# FUNCTIONS ==================================================================================================================


CheckEffectShape = function(data, alpha=3, wSize=3, plot=FALSE) {
  data = t(apply(data, 1, EliminateCensored))
  maxDist = sqrt((alpha-1)^2 * wSize); maxDist
  res = list()
  res$sax = CalculateSax(data, alpha, wSize)
  res$dists = nn2(res$sax,res$sax)
  res$meanDists = round(mean(res$dists$nn.dists[,2])/maxDist,2)

  res$saxValues = numeric(nrow(data))
  for(i in 1:nrow(data)) { 
    val = 0;
    for(j in 0:(alpha-1)) { val = val + res$sax[i,(j+1)]*10^j }
    res$saxValues[i] = val
  }
  if(plot==FALSE) pdf(file=NULL)
  pareto = paretochart(res$saxValues) 
  if(plot==FALSE) dev.off
  res$words = rownames(pareto)
  res$freqs = round(pareto$Percentage,2)
  res
}

ApplyShape = function(min, max, alpha=3, word=3) {
  range = max-min
  levels = numeric(alpha)
  for(i in 0:(alpha-1)) {
    levels[i+1] = min + (range * i)/(alpha-1)
  }
  ts = numeric(nchar(word))
  for(i in 1:nchar(word)) {
    letter = as.numeric(substr(word,i,i))
    ts[i] = levels[letter]
  }
  ts
}

