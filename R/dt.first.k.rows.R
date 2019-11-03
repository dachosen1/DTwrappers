#' dt.first.k.rows
#' 
#' @description A functions that returns a data frame that is based on the variables and the grouping varaibles statement.   
#' 
#' @param dat:  a data.frame object.
#' @param k:  an integer indicating how many rows to select.  Note that grouping statements will select the first k rows in each group.  Additionally, if k is larger than the number of records in a group, then the maximum number of records will be selected.  When non-integer or non-positive values of k are selected, the algorithm will select k = max(c(1, round(k))).  If k is not a numeric or integer value, then by default k is set to 1.
#' @param the.variables:  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).
#' @param the.filter:  a character value or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.
#' @param grouping.variables:  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) and do not exist in the.variables will be used.
#' @param grouping.type:  a character value specifying whether the grouping should be sorted (keyby) or as is (by).  Defaults to keyby unless "by" is specified.
#' @export
#' 
#' @examples 
#' @import formulaic
#' @source dt.select.R
#' @source create.filter.expression.R
#' 
#' id.name = "User ID"
#' age.name = "Age"
#' product.name = "Product"
#' gender.name  = "Gender"
#' region.name = "Region"
#' 
#' 
#' dt.first.k.rows(dat = snack.dat, k = 2, the.variables = c(id.name, age.name, product.name), grouping.variables = gender.name, grouping.type = "by")
#' 
#' dt.first.k.rows(dat = snack.dat, k = 1, the.variables = c(id.name, age.name, product.name), grouping.variables = c(gender.name, region.name), grouping.type = "keyby")
#' 
#' 
#' 
dt.first.k.rows <- function(dat, k = NULL, the.variables = ".", the.filter = NULL, grouping.variables = NULL, grouping.type = "keyby", ...){
  return(dt.select(dat = dat, the.variables = the.variables, the.filter = the.filter, grouping.variables = grouping.variables, grouping.type = grouping.type, first.k = k, ...))
}

