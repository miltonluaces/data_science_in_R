# Imports ---------------------------------------------------------------------------------------------------------

source("FinModels.R")

# Load Data ---------------------------------------------------------------------------------------------------------

IndexSymbols = vector(mode="list", length=18)
symbols = c( 'SP500', 'Nasdaq', 'Ftse100', 'DowJones', 'DAX', 'Nikkei', 'HangSeng', 'CAC40', 'Rusell2000', 'SP500Vix', 'Merval', 'Ibex', 'Ibovespa', 'Eurostoxx','HUIGold', 'CSI300', 'CboeGoldVix', 'CboeEuroVix')
names(IndexSymbols) = symbols
IndexSymbols[[1]] = '^GSPC'; IndexSymbols[[2]]='^IXIC'; IndexSymbols[[3]]='^FTSE'; IndexSymbols[[4]]='^DJI';IndexSymbols[[5]]='^GDAXI';IndexSymbols[[6]]='^N225';
IndexSymbols[[7]]='^HSI';IndexSymbols[[8]]='^FCHI'; IndexSymbols[[9]]='^RUT';IndexSymbols[[10]]='^VIX';IndexSymbols[[11]]='^MERV';IndexSymbols[[12]]='^IBEX';IndexSymbols[[13]]='^BVSP';
IndexSymbols[[14]]='^STOXX50E';IndexSymbols[[15]]='^HUI';IndexSymbols[[16]]='000300.SS';IndexSymbols[[17]]='^GVZ';IndexSymbols[[18]]='^EVZ'

Ind = vector(mode="list", length=18)
names(Ind) = symbols
for(i in 1:18) {
  data = LoadData(IndexSymbols[i])
  Ind[[i]] = GetSerie(data, "Open", 1, 180)
  print(paste('Serie', i, ' : ', names[i], 'loaded'))
}

#names(Indexes2)= c('DJOilGas', 'CboeOilVix', 'Emerging')
#Indexes2[[1]]='^DJUSEN';Indexes2[[2]]='^VVIX';Indexes[[3]]='^EMX'

# Plots ---------------------------------------------------------------------------------------------------------

cor(Ind$SP500, Ind$Nasdaq)^2

ud = Updown(Ind$SP500); 
plot(ud, type='lwd')


# Normalized Plots
data = Ind
names =  cbind('SP500', 'DowJones')
series = numeric(0);
for(i in 1: length(names)) { series = cbind(series, data[[names[i]]])  }
PlotSeries(series)

PlotIndexes(Ind, cbind('SP500', 'Nasdaq', 'Nikkei'))

