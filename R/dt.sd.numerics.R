# dat:  a data.frame object.

# the.variables:  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

# the.filter:  a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

# grouping.variables:  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

# grouping.type:  a character value specifying whether the grouping should be sorted (keyby) or as is (by).  Defaults to keyby unless "by" is specified.

# add.function.name:  a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.

# non.numeric.value:  if "missing", returns NA for variables that are not numeric, integer, logical, or complex.  Otherwise returns first entry of the vector.


# wrapper function that computes the standard deviation for each selected quantitative variable in each group after applying a filter.

dt.sd.numerics <- function(dat, the.variables = ".", na.rm = TRUE, the.filter = NULL, grouping.variables = NULL, grouping.type = "keyby", add.function.name = FALSE, non.numeric.value = "missing", ...){
  
  other.params <- sprintf("na.rm = %s", na.rm)
  
  return(dt.lapply(dat = dat, the.variables = the.variables, the.function = "sd.numerics", the.filter = the.filter, grouping.variables = grouping.variables, grouping.type = grouping.type, add.function.name = add.function.name, non.numeric.value = non.numeric.value, other.params = other.params, ...))
}
