#' df.define.variable
#' 
#' @description A function that an user can re-define the variables. Users have freedom to set the variable name and the the values. The values could be either expression, direct code(data.table format), or character. For details, please see the examples.   
#' 
#' @param dat:  a data.frame object
#' @param variable.name:  a character value specifying the name of the new column
#' @param the.values: a vector of data specifying the values of the new column.
#' @param specification: Defaults to "by.value" unless the.values is set to be an expression or a vector, then specification should be set as "by.expression".
#' @param the.filter: a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function. Defaults to "NULL" unless specified.
#' @note the data.frame dat will be converted to a data.table object to facilitate adding the new column by reference (e.g. efficiently with regard to the usage of memory)
#' 
#' @export 
#' @examples 
#' 
#' data('snack.dat')
#' age.name = "Age"
#' income.name = "Income"
#' region.name = "Region"
#' 
#' snack.dat <- dt.define.variable(dat = snack.dat, variable.name = "Age Decade", the.values = snack.dat[, floor(get(age.name) / 10)])
#' snack.dat[1:10, .SD, .SDcols = c(age.name, "Age Decade")]
#' 
#' snack.dat <- dt.define.variable(dat = snack.dat, variable.name = "Income in Thousands", the.values = expression(floor(get(income.name) / 10^3)), specification = "by.expression")
#' snack.dat[1:10, .SD, .SDcols = c(income.name, "Income in Thousands")]
#' snack.dat <- dt.define.variable(dat = snack.dat, variable.name = "Income in Thousands", the.values = "floor(get(income.name) / 10^3)", specification = "by.expression")
#' snack.dat[1:10, .SD, .SDcols = c(income.name, "Income in Thousands")]
#' snack.dat <- dt.define.variable(dat = snack.dat, variable.name = "Region and Country", the.values = expression(sprintf('%s, USA', get(region.name))), specification = "by.expression")
#' snack.dat[1:10, .SD, .SDcols = c(region.name, "Region and Country")]
#' @import data.table
#' @source create.filter.expression.R
#' @export 
dt.define.variable <- function(dat, variable.name, the.values, specification = "by.value", the.filter = NULL){
  data.table::setDT(dat)
  
  the.filter <- create.filter.expression(the.filter = the.filter)
  
  if(specification == "by.expression"){
    the.values <- parse(text = the.values)
  }
    
  dat[eval(the.filter), eval(variable.name) := eval(the.values)]

  return(dat)
}

