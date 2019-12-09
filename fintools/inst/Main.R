# Imports =====================================================================================================================================================

rm(list = ls()) 
path='C:/RScripts/fintools/'; 
source(paste(path, '/R/Portfolio.R', sep=''))
source(paste(path, '/R/Monitor.R', sep=''))
source(paste(path, '/R/FinPlots.R', sep=''))

# Portfolio Testing =====================================================================================================================================================

port$Load()
port$Show()

port$UpdateCurrValues(c(25,28))
port$Show()

inv1 = c('2015-01-01', 'AAPL', 'Apple 2', 10, 521.30, 526.30,0,0,0,0,0,0,0,0,0)
inv2 = c('2015-01-01', 'MSFT', 'Microsoft 2', 20, 521.30, 526.30,0,0,0,0,0,0,0,0,0)
port$AddInvest(inv1)
port$AddInvest(inv2)
port$Show()

port$RemoveInvest('MSFT')
port$Show()

port$Save()


# Load data =====================================================================================================================================================

mon = Monitor$new(path, iniData='2016-01-01')
mon$LoadStocks('Open')
mon$stockSyms[1]

mon$ShowCharts()

# Plots =====================================================================================================================================================

ts1 = mon$GetTs(1)
ts2 = mon$GetTs(2)
PlotSerie(ts1)
PlotSeries(cbind(ts1, ts2))
