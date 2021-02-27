# IMPORTS =====================================================================================================================================================

RemoveAccents <- function(strs) {
  reps = list('Á'='A','É'='E','Í'='I','Ó'='O','Ú'='U','á'='a','é'='e','í'='i','ó'='o','ú'='u')
  chartr(paste(names(reps), collapse=''), paste(reps, collapse=''), strs)
}

RemoveAccentsAscii <- function(strs) {
  strs = iconv(strs, to='ASCII//TRANSLIT')
  strs
}

ReplaceSpcWith_ <- function(strs) {
  reps = list(' '='_')
  chartr(paste(names(reps), collapse=''), paste(reps, collapse=''), strs)
}

FactorToChar = function(df) {
  for(i in 1:ncol(df)) { 
    if(is.factor(df[,i])) { df[,i] = as.character(df[,i]) }
  }
  df
}

CharToFactor = function(df) {
  for(i in 1:ncol(df)) { 
    if(is.character(df[,i])) { df[,i] = as.factor(df[,i]) }
  }
  df
}
