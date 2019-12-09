# IMPORTS =====================================================================================================================================================


CalcLim = function(ts, p=0.95) { 
  res = list()
  res$m = mean(ts) 
  res$s = sd(ts)
  res$lim = qnorm(p, res$m, res$s)
  res
}
    
Truncate = function(ts, lim) {
  outIdxs = which(ts > lim)
  ts[outIdxs] = lim
  ts
}

