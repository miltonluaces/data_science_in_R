CreateEnv <- function() {
  env <<- list(
    data.variables=0, data.varStr=0
  )
  class(env); 
}

Initialize <- function() {
  env$data.variables <<- LoadVarSymbols(); 
  env$data.varStr <<- 'snbaopl1'
}

LoadVarSymbols <- function() {
    VarSymbols = vector(mode="list", length=28)
    
    VarSymbols['s'] = 'General.Symbol'
    VarSymbols['n'] = 'General.Name'
    VarSymbols['s1'] = 'General.SharesOwned'
    
    
    VarSymbols['x'] = 'General.Stock Exchange'
    VarSymbols['j2'] = 'General.SharesOutstanding'
    VarSymbols['f6'] ='General.FloatShares'
    VarSymbols['v'] = 'General.MoreInfo'
    VarSymbols['j1'] = 'General.MarketCapitalization'
    VarSymbols['j3'] = 'General.MarketCapRT'
    VarSymbols['n4'] = 'General.Notes'
    
    VarSymbols['a'] = 'Pricing.Ask'	
    VarSymbols['b'] = 'Pricing.AskBid'	
    VarSymbols['b2'] = 'Pricing.AskRT'	
    VarSymbols['b3'] = 'Pricing.BidRT'	
    VarSymbols['p'] = 'Pricing.PreviousClose'	
    VarSymbols['o'] = 'Pricing.Open'	
    
    VarSymbols['c1'] = 'Date.Change'	
    VarSymbols['d1'] = 'Date.LastTrade'
    VarSymbols['c'] = 'Date.ChangeAndPercentChange'
    VarSymbols['d2'] = 'Date.Trade'
    VarSymbols['c6'] = 'Date.ChangeRT'	
    VarSymbols['t1'] = 'Date.LastTradeTime'
    VarSymbols['k2'] = 'Date.ChangePercentRT'
    VarSymbols['p2'] = 'Date.ChangeInPercent'	
    
    VarSymbols['y'] = 'Dividends.DividendYield'
    VarSymbols['d'] = 'Dividends.DividendPerShare'
    VarSymbols['r1'] = 'Dividends.DividendPayDate'
    VarSymbols['q'] = 'Dividends.ExDividendDate'
    
    VarSymbols['c8'] = 'Averages.AfterHoursChangeRT'
    VarSymbols['m5'] = 'Averages.ChangeFrom200MA'
    VarSymbols['c3'] = 'Averages.Commission'
    VarSymbols['m6'] = 'Averages.PercentChangeFrom200MA'
    VarSymbols['g'] = 'Averages.Days Low'	
    VarSymbols['m7'] = 'Averages.ChangeFrom50MA'
    VarSymbols['h'] = 'Averages.Days High'	
    VarSymbols['m8'] = 'Averages.PercentChangeFrom50MA'
    VarSymbols['k1'] = 'Averages.LastTradeRTWithTime'	
    VarSymbols['m3'] = 'Averages.50MA'
    VarSymbols['l'] = 'Averages.LastTradeWithTime'	
    VarSymbols['m4'] = 'Averages.200MA'
    VarSymbols['l1'] = 'Averages.LastTradePriceOnly'	
    VarSymbols['t8'] = 'Averages.1YrTargetPrice'	
    
    VarSymbols['w1'] = 'Misc.Days Value Change'	
    VarSymbols['g1'] = 'Misc.HoldingsGainPercent'
    VarSymbols['w4'] = 'Misc.DaysValueChangeRT'	
    VarSymbols['g3'] = 'Misc.AnnualizedGain'
    VarSymbols['p1'] = 'Misc.PricePaid'	
    VarSymbols['g4'] = 'Misc.HoldingsGain'
    VarSymbols['m'] = 'Misc.DaysRange'	
    VarSymbols['g5'] = 'Misc.HoldingsGainPercentRT'                   
    VarSymbols['m2'] = 'Misc.DaysRangeRT'
    VarSymbols['g6'] = 'Misc.HoldingsGainRT' 
    VarSymbols['t7'] = 'Misc.TickerTrend'
    VarSymbols['t6'] = 'Misc.TradeLinks'
    VarSymbols['i5'] = 'Misc.OrderBookRT'
    
    VarSymbols['k'] = 'WeekInfo.52WeekHigh'	
    VarSymbols['j'] = 'WeekInfo.52WeekLow'	
    VarSymbols['j5'] = 'WeekInfo.ChangeFrom52WeekLow'	
    VarSymbols['k4'] = 'WeekInfo.ChangeFrom52WeekHigh'	
    VarSymbols['j6'] = 'WeekInfo.PercentChangeFrom52WeekLow'	
    VarSymbols['k5'] = 'WeekInfo.PercentChangeFrom52WeekHigh'	
    VarSymbols['w'] = 'WeekInfo.52WeekRange'
    VarSymbols['v'] = 'Volume.Volume'	
    VarSymbols['a5'] = 'Volume.AskSize'	                                                              
    VarSymbols['b6'] = 'Volume.BidSize'	
    VarSymbols['k3'] = 'Volume.LastTradeSize'
    VarSymbols['a2'] = 'Volume.AverageDailyVolume'
    
    VarSymbols['l2'] = 'Ratios.High Limit'
    VarSymbols['e'] = 'Ratios.EarningsPerShare'	
    VarSymbols['l3'] = 'Ratios.Low Limit'
    VarSymbols['e7'] = 'Ratios.EPSEstimateCurrentYear'	
    VarSymbols['v1'] = 'Ratios.HoldingsValue'
    VarSymbols['e8'] = 'Ratios.EPS EstimateNextYear'	
    VarSymbols['v7'] = 'Ratios.HoldingsValueRT'
    VarSymbols['e9'] = 'Ratios.EPS Estimate Next Quarter	s6 Revenue'
    VarSymbols['b4'] = 'Ratios.BookValue'	
    VarSymbols['j4'] = 'Ratios.EBITDA'	
    VarSymbols['p5'] = 'Ratios.Price/Sales'
    VarSymbols['p6'] = 'Ratios.Price/Book'	
    VarSymbols['r'] = 'Ratios.P/E'	
    VarSymbols['r2'] = 'Ratios.P/E_RT'	
    VarSymbols['r5'] = 'Ratios.PEG'	
    VarSymbols['r6'] = 'Ratios.Price/EPS_EstimateCurrentYear'	
    VarSymbols['r7'] = 'Ratios.Price/EPS_EstimateNextYear'	
    VarSymbols['s7'] = 'Ratios.ShortRatio'
    
    VarSymbols
}

#Entry Point
env = CreateEnv()
Initialize()
