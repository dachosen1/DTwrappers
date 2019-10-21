# dat:  a data.frame object

# variable.names:  a character vector with the column names to be removed from dat.

dt.remove.variables <- function(dat, the.variables){
  require(data.table)
  setDT(dat)
  
  dat[, (the.variables) := NULL]
  
  return(dat)
}

