# IMPORTS ===========================================================================================================================================================================================

library(R6)
library(MiscFunctions)
library(scales)
library(cluster)
library(fpc)

# PREPROCESS ===========================================================================================================================================================================================

#cars = read.csv('Multivariate/inst/cars.csv', sep=';', stringsAsFactors = FALSE)
#cars1 = cars[,c('sym', 'height', 'rpm')]
#cars2  = na.omit(cars1) # listwise deletion of missing
#data = scale(cars2); data # standardize variables


# DETERMINE K ===========================================================================================================================================================================================

DetermineK <- function(data, maxCenters) {
  wss = (nrow(data)-1) * sum(apply(data,2,var)); wss
  for (i in 2:maxCenters) { wss[i] = sum(kmeans(data, centers=i)$withinss) }
  tot = sum(wss)
  pwss = (wss/tot)*100; 
  cwss = cumsum(pwss); cwss
  plot(c(0,cwss), type="l", col='blue', lwd = 2, panel.first = grid(nx=20, ny=20, lty = 1, lwd = 1), ylim=c(0,100), main='Determine K centers', xlab="Number of Clusters", ylab="Percentage of information")
  print(t(percent(cwss/100)))
  return(cwss)
}

# K-MEANS ===========================================================================================================================================================================================

GetK <- function(cwss, cut) { 
  min(which(ks>cut))
}

#fit1 = kmeans(data, centers=5)
#fit2 = pam(data, k=5)

Centers <- function(data, fit) {
  aggregate(data,by=list(fit$cluster),FUN=mean)
}

UpdateCluster <- function(data, fit) {
  data = data.frame(data, fit$cluster) 
  colnames(data)[ncol(data)] = 'cluster'
  data  
}

#cluster.stats(data, fit1$cluster)


# PLOTS ===========================================================================================================================================================================================

ClustPlot <- function(data, fit) {
  par(mfrow=c(1,2))
  clusplot(data, fit$cluster, color=TRUE, shade=FALSE, labels=2, lines=0, main='Clustering: principal components')
  plotcluster(data, fit$cluster, main='Classification')
  par(mfrow=c(1,1))
}

ClustPlot3d = function(data, xtitle, ytitle, ztitle) {
  plot3d(data[2:4], type='s', size=1, col=data$cluster, zlab=ztitle, ylab=ytitle, xlab=ztitle, expand=1.3)
  text3d(data[,2:4],texts=substr(data[,1],1,3), adj=c(1,1), font=4)
}
