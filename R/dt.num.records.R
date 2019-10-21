# dat:  a data.frame object.

# the.variables:  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

# na.rm:  a logical value specifying whether missing values should be removed from the calculations specified by the.functions.

# the.filter:  a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

# grouping.variables:  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

# grouping.type:  a character value specifying whether the grouping should be sorted (keyby) or as is (by).  Defaults to keyby unless "by" is specified.

# add.function.name:  a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.

# include.total:  a logical value specifying whether the.functions should be applied both to the entire table and to the groups specified in grouping.variables.

# format.as:  a character value with "long" corresponding to long format.  All other values of format.as will produce results in wide format.

dt.num.records <- function(dat, the.variables = ".", na.rm = TRUE, the.filter = NULL, grouping.variables = NULL, grouping.type = "keyby", add.function.name = TRUE, include.total = TRUE, format.as = "wide", ...){
  require(data.table)
  setDT(dat)
  
  other.params <- sprintf("na.rm = %d", na.rm)
  
  the.results <- dt.lapply(dat = dat, the.variables = the.variables, the.functions = "num.records", the.filter = the.filter, grouping.variables = grouping.variables, grouping.type = grouping.type, add.function.name = add.function.name, other.params = other.params, ...)
  
  if(include.total == TRUE & !is.null(grouping.variables)){
    total.results <- dt.lapply(dat = dat, the.variables = the.variables, the.functions = "num.records", the.filter = the.filter, grouping.variables = NULL, na.rm = na.rm, add.function.name = add.function.name, ...)
    
    total.results[, (grouping.variables) := "All Rows"]
    
    the.results <- rbindlist(l = list(the.results, total.results), fill = TRUE)
  }
  
  if(format.as == "long"){
    options(warn = -1)
    the.results <- melt.data.table(data = the.results, id.vars = grouping.variables, variable.factor = F, value.factor = F)
    
    the.results[, row := 1:.N]
    
    the.results[, intermediate := lapply(X = variable, FUN = function(x){return(strsplit(x = variable, split = "_"))[[1]]}), by = "row"]
    
    the.results[, statistic := lapply(X = intermediate, FUN = function(x){return(paste(x[2:length(x)], collapse = "_"))}), by = "row"]
    
    the.results[, variable := lapply(X = intermediate, FUN = function(x){return(paste(x[1], collapse = "_"))}), by = "row"]
    
    the.results[, intermediate := NULL]
    the.results[, row := NULL]
    
    setcolorder(x = the.results, neworder = c(grouping.variables, "variable", "statistic", "value"))
    
    the.results[, statistic := NULL]
    setnames(x = the.results, old = "value", new = "num.records")
    
    if(grouping.type == "keyby"){
      setorderv(x = the.results, cols = "variable", order = 1)
    }
  }
  return(the.results[])
}
