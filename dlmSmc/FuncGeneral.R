# GENERAL FUNCTIONS REPOSITORY
# ==========================================================================================================
    

library(dlm)
library(mvtnorm)


# Plot functions
# ==========================================================================================================

# Plot historic and forecasting time series joined
#-----------------------------------------------------------------------------------------------------------
# Paramets : historic time series hist, forecast time series fcst, title of the plot
# Returns  : No returns. Makes the plot.
plotHistFcst = function(hist, fcst, title) {
  plot(hist, type="l", lwd=3, col="blue", main=title, xlab="time", ylab="value")
  lines(fcst, type="l", lwd=3, col="red")
}

# Plot residuals of historic and forecasting time series 
#-----------------------------------------------------------------------------------------------------------
# Paramets : historic time series hist, forecast time series fcst, title of the plot
# Returns  : No returns. Makes the plot.
plotResiduals = function(hist, fcst, title) {
  plot(hist-fcst, type="l", lwd=3, col="black", main=title, xlab="time", ylab="residual")
}
