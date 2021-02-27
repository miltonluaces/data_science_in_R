# IMPORTS ============================================================================================================================

library(rClr)


# BASICS =============================================================================================================================

# Connection ---------------------------------------------------------------------------------------------------------------------------------

# # Load Config 
LoadConfig <- function(path, configPath, file) {
   params = read.table(paste(configPath,file, sep='', collapse = ''), header=T, sep="\t")
   psPath = GetParameter(params, 'psPath')
   Import(psPath, 'ObjRelMap.dll')
   Import(psPath, 'LibBasic.dll')
   if(!exists('ri')) { ri <<- clrNew('ECC.Data.RInterop', path, configPath) }
   params
}

# Connect 
Connect <- function(params) {

  provider = GetParameter(params, 'provid')
  connStr = GetParameter(params, 'connStr')
  decodedConnStr = clrCallStatic('ECC.Lib.ConnectionStringPassword', 'Decode', toString(connStr));
  clrCallStatic('ECC.Lib.StaticResources', 'Initialize')
  clrCallStatic('ECC.Lib.DbEngine', 'Connect', provider, decodedConnStr)
  clrCallStatic('ECC.Data.InitialSetup', 'Run')
}

# Get Db Managers ---------------------------------------------------------------------------------------------------------------------------------

GetOldGd <- function() {
  gd <<- clrNew('ECC.Data.GenericDataStore')
}

GetNewGd <- function() {
  newGd <<- clrNew('ECC.Data.PS100Entities')
}


# LOAD FUNCTIONS ========================================================================================================================

# Load Business Objects ---------------------------------------------------------------------------------------------------------------------------------

LoadSku <- function(skuId) {
  sku = clrCall(ri, 'LoadSku', as.integer(skuId))
  sku
}

LoadSkuByMatNode <- function(matCode, nodeCode) {
  material = clrNew('ECC.Data.Material')
  clrCall(oldGd, 'Load', material, matCode)
  
  ipalNode = clrNew('ECC.Data.IPALNode')
  clrCall(oldGd, 'Load', ipalNode, nodeCode)
  
  sku = clrNew('ECC.Data.Sku');
  clrCall(oldGd, 'Load', sku, material, ipalNode)
  sku
}

LoadSkus <- function(condition) {
  skus = clrCall(ri, 'LoadSkus', condition)
  skus
}

# Get Dfu / Serie ---------------------------------------------------------------------------------------------------------------------------------

GetDfu <- function(sku, periodicity) {
  dfu = clrCall(ri, 'GetDfu', sku, periodicity)
  dfu
}

GetDS <- function(sku, periodicity) {
  dfu = clrCall(ri, 'GetDfu', sku, periodicity)
  serie = clrCall(ri, 'GetSerie', dfu)
  ds = clrNew('ECC.Data.DfuSerie', dfu,serie)
  ds
}

GetRegDS <- function(sku, periodicity, tsmId) {
  dfu = clrCall(ri, 'GetDfu', sku, periodicity)
  serie = clrNew('ECC.Data.Serie')
  clrCall(gd, 'Load', serie, tsmId)
  ds = clrNew('ECC.Data.DfuSerie', dfu,serie)
  ds
}

# Load Time Series ---------------------------------------------------------------------------------------------------------------------------------

LoadHist <- function(sku, periodicity) {
  ds = GetDS(sku, periodicity)
  Hots = clrNew('ECC.Data.HistTSValueCollection')
  clrCall(gd, 'Load' , Hots, ds)
  hots = clrCall(ri, 'GetSerie', Hots);
  hots
}

LoadFcst <- function(sku, periodicity) {
  ds = GetDS(sku, periodicity)
  Fots = clrNew('ECC.Data.FcstTSValueCollection')
  clrCall(gd, 'Load' , Fots, ds)
  fots = clrCall(ri, 'GetSerie', Fots);
  fots
}

LoadRegByCode <- function(sku, periodicity, tsmCode) {
  serie = clrCall(ri, 'LoadSerie', tsmCode)
  LoadRegBySerie(sku, periodicity, serie)
}

LoadRegById <- function(sku, periodicity, tsmId) {
  serie = clrCall(ri, 'LoadSerie', as.integer(tsmId))
  LoadRegBySerie(sku, periodicity, serie)
}

LoadRegBySerie <- function(sku, periodicity, serie) {
  dfu = clrCall(ri, 'GetDfu', sku, periodicity)
  ds = clrNew('ECC.Data.DfuSerie', dfu, serie)
  Reg = clrNew('ECC.Data.HistRegTSValueCollection')
  clrCall(gd, 'Load' , Reg, ds)
  reg = clrCall(ri, 'GetSerie', Reg);
  reg
}


# SAVE FUNCTIONS ===========================================================================================================================

SaveSku <- function(sku) {
  clrCall(ri, 'SaveSku', sku)
}
