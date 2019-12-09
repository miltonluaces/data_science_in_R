# IMPORTS =====================================================================================================================================================

library(R6)
library(xlsx)
library(quantmod)

# Monitor Class =====================================================================================================================================================

Monitor = R6Class(
  
  classname = 'Monitor', 
  
  public = list(
    
    # Public Fields ----
    
    stockSyms=NULL,
    etfSyms=NULL,
    fundSyms=NULL,
    indexSyms=NULL,
    
    stocks=NULL, 
    etfs=NULL,
    funds=NULL,
    indexes=NULL,
    iniData=0,
    
    # Public Methods ----      
    
    initialize = function(path, iniData) {
      options(stringsAsFactors = FALSE)
      self$iniData = iniData
      private$path=path
      private$dataPath = paste(private$path, 'data/Data.xls', sep='')
      self$LoadData()
    },
    
    LoadData = function() { 
      data = read.xlsx(private$dataPath, sheetName='Monitor')
      self$stockSyms = na.omit(data[,'Stocks'])
      self$etfSyms = na.omit(data[,'ETFs'])
      self$fundSyms = na.omit(data[,'Funds'])
      self$indexSyms = na.omit(data[,'Indexes'])
    },
    
    LoadStocks = function(prop) {
      self$stocks = vector(mode="list", length=0)
      for(i in 1:length(self$stockSyms)) {
        data = self$LoadStock(self$stockSyms[i])
        ts = self$GetSerie(data, prop, 1, 180)
        self$stocks = cbind(self$stocks, ts)
      }
      colnames(self$stocks) = self$stockSyms
    },
    
    LoadStock = function(stockCode) {
      iniYmd = private$GetYMD(self$iniData)
      iniYear = iniYmd[1]; iniMonth = strtoi(iniYmd[2])-1; iniDay = iniYmd[3];
      today = Sys.Date()
      endYmd = private$GetYMD(today)
      endYear = iniYmd[1]; endMonth = strtoi(iniYmd[2])-1; endDay = iniYmd[3];
      
      prefix = "http://ichart.finance.yahoo.com/table.csv?s="
      sufix = "&&ignore=.csv"
      url = paste(prefix, stockCode, "&d=", endMonth , "&e=", endDay, "&f=", endYear, "&g=", "d", "&a=", iniMonth, "&b=", iniDay, "&c=", iniYear, sufix)
      data = read.csv(url, header=TRUE)
      data
    },
    
    GetSerie = function(data, prop, ini, end) {
      data[[prop]][ini:end]
    },
    
    GetStockSyms = function() {
      self$stocks[0,]
    },
    
    GetTs = function(index) {
      unlist(self$stocks[,index])
    },
    
    ShowChart = function(stockCode) {
      sym = getSymbols(stockCode, auto.assign=FALSE)
      chartSeries(sym, subset='last 3 months', type='line', name=stockCode)
      #addBBands()
      #addDEMA()
    },
    
    ShowCharts = function() {
      for(i in 1:length(self$stockSyms)) {
        sym = mon$stockSyms[i]
        mon$ShowChart(sym)
        line = readline('Press key')
      }
    }
  ),
  
  private = list(
    path='',
    dataPath='', 
 
    GetYMD = function(d) {
      year = substr(d, 1, 4)
      month = substr(d, 6,7)
      day = substr(d, 9,10)
      comps = c(year, month, day)
      comps
    }
  )
  
)  
