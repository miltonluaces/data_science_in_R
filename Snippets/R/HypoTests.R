# BASIC FUNCTIONS =================================================================================================================

# Normality assumption
NormalityTest <- function(data) {
  pVal = -1
  tryCatch({
    pVal = shapiro.test(data)$p.value
    print(paste('Normality p value = ', pVal))
  }, warning = function(w) {
    print('warnings in Shapiro test')
  }, error = function(e) {
    pVal = -1
    print('Shapiro test could not be performed')
  }, finally = {
    return(pVal)
  })
}

# Homoskedasticity assumption 
HomoskedasticityTest <- function(x,y) { 
  pVal = -1
  tryCatch({
    pVal = bartlett.test(y ~ x)$p.value
  }, warning = function(w) {
    print('warnings in Bartlett test')
  }, error = function(e) {
    pVal = -1
    print('Bartlett test could not be performed')
  }, finally = {
    return(pVal)
  })
}

# Kruskal-Wallis Test
AnovaTest <- function(x,y) {
  pVal = -1
  tryCatch({
    pVal = kruskal.test(y ~ x)$p.value
  }, warning = function(w) {
    print('warnings in Shapiro test')
  }, error = function(e) {
    pVal = -1
    print('Shapiro test could not be performed')
  }, finally = {
    return(pVal)
  })
}

WilcoxTest <- function(x, y) {
  pVal = -1
  tryCatch({
    pVal = wilcox.test(x,y, alternative='greater', exact=FALSE)$p.value
  }, 
  error = function(e) {
    pVal = -1
    print('Wilcox test could not be performed')
  }, finally = {
    return(pVal)
  })
}

CalcRecurrentTs <- function(ts, n, iniEff, endEff, alpha) {
  res <<- list()
  res$tsRec <<- ts
  res$pValues <<- numeric(length(iniEff))
  res$prop <<- -1
  if(length(iniEff) != length(endEff)) { return -1 }
  hits=0
  for(i in 1:length(res$pValues)) {
    s = endEff[i]-iniEff[i]+1 
    iniCtrl = iniEff[i]-s*n; if(iniCtrl<1) iniCtrl=1
    endCtrl = iniEff[i]-1
    m = mean(res$tsRec[iniCtrl:endCtrl])
    res$tsRec[iniEff[i]:endEff[i]] = rep(m, s)
    res$pValues[i] = WilcoxTest(ts[iniEff[i]:endEff[i]], res$tsRec[iniCtrl:endCtrl])
    if(res$pValues[i]<=alpha) hits=hits+1
  }
  prop = hits/length(res$pValues)
  res$prop = prop
  res
}
