# IMPORTS =====================================================================================================================================================

library(R6)
library(gridExtra)
library(grid)
library(xlsx)

# CLASS =====================================================================================================================================================

Portfolio = R6Class(
  
  classname = 'Portfolio', 
  
  public = list(
    
    # Public Fields ----
    
    port = NULL,
    path = '',
    
    # Public Methods ----      
    
    initialize = function(path) {
      self$path = path
      private$dataPath = paste(self$path, 'data/Data.xls', sep='')
      options(stringsAsFactors = FALSE)
      self$port = data.frame(Date=character(), Code=character(), Desc=character(), Qty=double(), PurchValue=double(), CurrValue=double(), CurrAmount=double(), PurchAmount=double(), LossAmount=double(), ProfitAmount=double(), PropAmount=double(), PropProfit=double(), StopLoss=double(), TakeProfit=double())
      names(self$port) = c('Date', 'Stock', 'Desc', 'Qty', 'PurchValue', 'CurrValue', 'PurchAmount', 'CurrAmount', 'LossAmount', 'ProfitAmount', 'PropAmount', 'PropProfit', 'StopLoss', 'TakeProfit')
    },
    
    AddInvest = function(inv) {
      self$port[nrow(self$port)+1,] = inv
    },
    
    RemoveInvest = function(stock) {
      self$port = self$port[self$port$Stock!=stock,]
    },
    
    Show = function() { 
      tt=ttheme_default( core=list(bg_params = list(fill = c('lightcyan', 'lightblue'), col=NA), fg_params=list(fontface=3)), colhead=list(fg_params=list(col="navyblue")), rowhead=list(fg_params=list(col="navyblue", fontface=2L)))
      grid.arrange(tableGrob(self$port, theme=tt))
    },
    
    Load = function() {
      self$port = read.xlsx(private$dataPath, sheetName='Portfolio')
    },
    
    Save = function() {
      write.xlsx(self$port, file=private$dataPath, sheetName='Portfolio')
    },

    UpdateCurrValues = function(newValues) {
      self$port[,'CurrValue'] = newValues
    }
    
  ),
  
  private = list(
    dataPath = ''
  )
)  
