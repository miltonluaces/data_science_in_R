# Load Data


PlotSerie <- function(serie, index=1) {
  switch(index, '1'='blue', '2'='red', '3'='green', '4'='black')
  plot(serie, type="l", col=index, lwd=2)
}

PlotSeries <- function(series, title='Indexes') {
  normSeries = NormalizeSeries(series)
  colors = c('blue', 'red', 'darkgreen', 'black', 'brown', 'orange', 'grey')
  plot(normSeries[,1], type="l", col=colors[1], lwd=2, main = title)
  for(i in 2:length(normSeries[1,])) {
    lines(normSeries[,i], type="l", col=colors[i], lwd=2)
  }
}

PlotIndexes <- function(data, names) {
  series = numeric(0);
  for(i in 1: length(names)) { series = cbind(series, data[[names[i]]])  }
  PlotSeries(series, )
}

Updown <- function(serie) {
  ud = numeric(0)
  for(i in 2: length(serie)) {
    if(serie[i] >= serie[i-1]) ud = c(ud, 1) else ud = c(ud, -1)
  }
  return(ud)
}

MinLength <- function(series) {
  min = 100000
  for(i in 1:length(series[1,])) {
    if(length(series[,i]) < min) { min = length(series[,i])}
  }
  min
}

Trunk <- function(serie, n) {
  ini = length(serie)-n+1
  end = length(serie)
  return(serie[ini:end])
}

Normalize <- function(serie) {
  normSerie = numeric(0)
  min = min(serie)
  max = max(serie)
  for(i in 1:length(serie)) {
    norm = (serie[i] - min)/(max-min) 
    normSerie = c(normSerie, norm)
  }
  return(normSerie)
}

NormalizeSeries <- function(series) {
  min = MinLength(series)
  normSeries = numeric(0)
  for(i in 1:length(series[1,])) {
    if(length(series[,i] > min)) { series[,i] = Trunk(series[,i], min) }
    normSerie = Normalize(series[,i])
    normSeries = cbind(normSeries, normSerie)
  }
  normSeries
}


