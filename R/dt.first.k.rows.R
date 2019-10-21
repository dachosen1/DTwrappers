#' dat:  a data.frame object.

#' k:  an integer indicating how many rows to select.  Note that grouping statements will select the first k rows in each group.  Additionally, if k is larger than the number of records in a group, then the maximum number of records will be selected.  When non-integer or non-positive values of k are selected, the algorithm will select k = max(c(1, round(k))).  If k is not a numeric or integer value, then by default k is set to 1.

#' the.variables:  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' the.filter:  a character value or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' grouping.variables:  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) and do not exist in the.variables will be used.

#' grouping.type:  a character value specifying whether the grouping should be sorted (keyby) or as is (by).  Defaults to keyby unless "by" is specified.

#' Calls dt.select with first.k = k.

#' @export
dt.first.k.rows <- function(dat, k = NULL, the.variables = ".", the.filter = NULL, grouping.variables = NULL, grouping.type = "keyby", ...){
  return(dt.select(dat = dat, the.variables = the.variables, the.filter = the.filter, grouping.variables = grouping.variables, grouping.type = grouping.type, first.k = k, ...))
}

