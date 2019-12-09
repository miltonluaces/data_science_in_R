source('FinModels.R')
source('FundVariables.R')

# Load Data ---------------------------------------------------------------------------------------------------------

LoadFundData <- function(stockCodes) {
  prefix = "http://finance.yahoo.com/d/quotes.csv?s="
  varfix = "&f="
  url = prefix
  for(i in 1:length(stockCodes)) { url = paste(url, stockCodes[i]) }
  url = paste(url, varfix, env$data.varStr)
  
  data = read.csv(url, header=FALSE)
  data
}

# Testing ---------------------------------------------------------------------------------------------------------
varSymbols = LoadVarSymbols()


stockCodes = c('MSFT', 'BAC', 'CAT', 'AMZN' ); stockCodes
varStr = 'snghw'
data = LoadFundData(stockCodes); data

