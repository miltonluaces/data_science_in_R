# Call C# Classes and Methods =======================================================================================================

library(RMySQL)
library(rClr)
library(knitr)


# CONNECT DATABASES ==================================================================================================================

# Connect MySql ----------------------------------------------------------------------------------------------------------------------

ConnectMySql <- function() {
  conn <- dbConnect(MySQL(), user = 'root', password = 'Cama10rillo', host = '192.168.100.1', dbname='planningstudio100')
  res = dbSendQuery(conn, "select * from tecc_sku")
  dbFetch(res)
}

# Connect Sql Server -----------------------------------------------------------------------------------------------------------------

ConnectSqlServer <- function() {
}


# Load From Excel ---------------------------------------------------------------------------------------------------------------------------------

LoadFromExcel <- function() {
  table = loadFile("TSeries")
  nTs = length(table); nTs
  #tsPrices = GetSeries(table, 1,nTs/2)
  #tsDmds = GetSeries(table, nTs/2+1,nTs)
}

# CALL .NET FROM R ==================================================================================================================

CallDotNetRandom <- function() {
  #clrLoadAssembly('C:/ProyectosVisualStudio/Genesis/Algoritmos/ConsoleTesting/bin/Release/ClassicalStat.dll')
  #randGen <- clrNew('ClassicalStat.RandomGen')
  #clrCall(randGen, 'NextDouble')
  #clrCall(randGen, 'NextDouble', min=2, max=5)
}

# CALL .R FROM .NET ==================================================================================================================

testText <- function() { print("prueba")}

testInt <- function() { return(7) }

testArray <- function(vecParam) {
  print(paste('third: ', vecParam[3]))
}

testParamsNumeric <- function(paramNum) { print(paste('Numeric: ', paramNum)) }

testParamsNumeric2 <- function(paramNum1, paramNum2) { 
  print(paste('Sum Numerics: ', paramNum1 + paramNum2)) 
  prodRet = paramNum1 * paramNum2;
  prodRet
}

testParamsNumericList <- function(paramNumList) { print(paramNumList) }

testParamsString <- function(str) { print(str) }

testParamsBool <- function(paramBool) { print(paramBool) }


# REPORTING ==================================================================================================================

Report <- function() {
  #s = system.file("misc", "Elast1.R", package = "knitr"); stitch(s)  
}
