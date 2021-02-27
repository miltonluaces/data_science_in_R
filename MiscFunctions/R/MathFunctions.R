# BASIC FUNCTIONS =================================================================================================================

# Discretize a real value by a factor significance level 
Discretize <- function(value, factor) {
  round(round(value / factor) * factor, 2);
}

# Line Equation 
LineEq <- function(x, x1, y1, x2, y2) {
  if(x1 == x2) return(0)
  a = (y2-y1) / (x2 - x1)
  b =  -a * x1 + y1
  y = a * x + b  
  y
}

# Ramp Function 
Ramp <- function(x, a1, a2) {
  y = numeric(length(x))
  for(i in 1:length(x)) {
    if(x[i] < a1) { y[i] = 0 } else if(x[i] > a2) { y[i] = 1 } else { y[i] = LineEq(x[i],a1,a2,0,1) }
  }
  return(y)
}

# Load Factor 0s/n ratio 
LoadFactor <- function(serie) {
  bools = mapply(serie, FUN=sign)
  sum(bools)/length(bools)
}

ZNorm <- function(x) {
  s = sd(x)
  if(s==0) { return(rep(0, length(x))) }
  else { return((x - mean(x))/sd(x)) }
}

ZNormCens <- function(x) {
  s = sd(x)
  if(s==0) { return(rep(0, length(x))) }
  else { return((x[x>=0] - mean(x))/sd(x)) }
}

# STAT FUNCTIONS ==================================================================================================================

R2 <- function(x,y) {
  cor(x, y)^2; 
}


# TIME SERIES FUNCTIONS ===========================================================================================================

# Discretize a list of values by a factor significance level
DiscretizeList <- function(values, factor) {
  discValues = numeric(length(values));
  for (i in 0:length(values)) { discValues[i] = Discretize(values[i], factor); }
  discValues
}

# Trunk lists of time series values to the min lenght from the tail.
GetEqualTails <- function(ts1, ts2) {
  n1 = length(ts1); n2 = length(ts2)
  if(n1 >= n2) {  ts1 = tail(ts1, -(n1-n2)) } else { ts2 = tail(ts2, -(n2-n1)) }
  list(ts1, ts2)
}


# GROUPING FUNCTIONS ==============================================================================================================

# Create a dictionary form pairs (xi,yi). Useful for groups or distributions
CreateDictionary <- function(x, y) {
  xFac = levels(factor(x))
  groups = vector(mode="list", length=length(xFac))
  names(groups) = xFac
  for(i in 1:length(x)) { 
    if(is.na(x[i]) | is.na(y[i])) next;
    key = toString(x[i])
    val = y[i]
    groups[[key]] = cbind(groups[[key]], val) 
  }
  groups
}

# Resampling groups
Resample <- function(group, minSize) {
  m = mean(group)
  s = sd(group)
  while(length(group) < minSize) {
    sample = round(rnorm(1, m, s))
    if(sample >=0) { group = c(group, sample) }
  }
  group
}

GroupEquals <- function(data) {
  g=1
  group = c(data[1,ncol(data)]); 
  groups = vector(mode='list', nrow(unique(data[,1:(ncol(data)-1)])))
  for(i in 2: nrow(data)) {
    a = as.numeric(data[i,1:(ncol(data)-1)])
    b = as.numeric(data[(i-1),1:(ncol(data)-1)])
    if(all(a==b)) { group = c(group, data[i,ncol(data)]) } else { groups[[g]] = group; g=g+1; group = data[i,ncol(data)] }
  }
  if(length(group)>0) { groups[[g]] = group }
  groups
} 

OrderDf <- function(df) {
  for(i in (ncol(df)-1):1) { df = df[order(df[,i]),] }
  df
}


# OTHER TS FUNCTIONS ==============================================================================================================

GetVectorFromDataFrame <- function(row) {
  names(row) = NULL
  vec = unlist(c(row))
  vec
}

GetMonths <- function() {
  months = c('ENERO', 'FEBRERO', 'MARZO', 'ABRIL', 'MAYO', 'JUNIO', 'JULIO', 'AGOSTO', 'SEPTIEMBRE', 'OCTUBRE', 'NOVIEMBRE', 'DICIEMBRE')
  months
}

#Moving Average
CalcMA <- function(x, n=5){
  ma = filter(x,rep(1/n,n), sides=2)
  pad = n%/%2
  notNa = which(!is.na(ma))
  c(rep(ma[notNa[1]], pad), ma[(pad+1):(length(ma)-pad)], rep(ma[notNa[length(notNa)]], pad))
}  


InverseOrder <- function(vector) {
  vector[length(vector):1]
}

ReplaceNanWithValue <- function(df, colIndex, repValue) {
  df[is.nan(df[,colIndex]),colIndex] = repValue
}

ReplaceOutlierWithThreshold <- function(df, colIndex, threshold) {
  df[,colIndex> threshold,colIndex] = threshold
}

GetExponentBy10 <- function(minDecExp, i) {
  10^-(minDecExp-i %% minDecExp)
}

GetExponentBy5 <- function(minDecExp, i) {
  j=i%/%2
  e = 10^-(minDecExp-j %% minDecExp)
  if(i%%2==0) e else 5*e
}

#for(i in 0:3) { print(GetExponentBy10(4, i)) }
#for(i in 0:7) { print(GetExponentBy5(4, i)) }

BinaryCombinatory <- function(nDigits) {
  m = as.data.frame(matrix(rep(c(0,1),nDigits),ncol=nDigits))
  combs = expand.grid(m); combs
}

SelectDfRows = function(df, activeRows) {
  idx = which(activeRows==1)
  df[idx,]
}

AdjR2 = function(r2, n, p) {
  1 - (((1-r2)*(n-1))/(n-p-1))
}

PermDfCols = function(df, c1, c2) {
  aux = df[c1]
  df[c1] = df[c2]
  df[c2] = aux
  df
}

