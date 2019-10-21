# dat:  a data.frame object.

# digits:  the number of digits to round to.

# the.variables:  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

# the.functions:  a character vector or list specifying the name of the function to apply to the variables.  This may either be specified by the name of the function as a character (e.g. "mean") or by defining a function; e.g. function(x){return(mean(x = x))} that can be computed on each column of data as specified in the.variables.

# the.filter:  a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

# grouping.variables:  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

# grouping.type:  a character value specifying whether the grouping should be sorted (keyby) or as is (by).  Defaults to keyby unless "by" is specified.

# add.function.name:  a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.

# All other inputs:  see help(prettyNum)

dt.round.and.format.numerics <- function(dat, digits, the.variables = ".", the.filter = NULL, grouping.variables = NULL, add.function.name = FALSE, big.mark = "", big.interval = 3L, small.mark  = "", small.interval = 5L, decimal.mark = getOption("OutDec"), input.d.mark = decimal.mark, preserve.width = c("common", "individual", "none"), ...){

  other.params <- sprintf("digits = %d, big.mark = '%s', big.interval = %s, small.mark  = '%s', small.interval = '%s', decimal.mark = '%s', input.d.mark = '%s', preserve.width = '%s'", digits, big.mark, big.interval, small.mark, small.interval, decimal.mark, input.d.mark, preserve.width)
  
  
    
  return(dt.lapply(dat = dat, the.variables = the.variables, the.functions = "round.and.format.numerics", the.filter = the.filter, grouping.variables = grouping.variables, add.function.name = add.function.name, other.params = other.params, ...))
}
