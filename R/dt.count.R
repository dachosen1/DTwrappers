#' dt.count
#' 
#' @description A function that counts the number of rows that meets the qualification such as filter and the by. If both filter and the by are not set up, then it will return the number of row. 
#' 
#' @param dat:  a data.frame object.
#' @param the.variables:  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).
#' @param the.functions:  a character vector or list specifying the name of the function to apply to the variables.  User may either define their own functions by specifying the name of the function as a character (e.g. c('mean', 'sd')) or by specifying a function; e.g. function(x){return(mean(x = x))} that can be computed on each column of data as specified in the.variables.
#' @param the.filter:  a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.
#' @param grouping.variables:  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.
#' @param grouping.type:  a character value specifying whether the grouping should be sorted (keyby) or as is (by).  Defaults to keyby unless "by" is specified.
#' @param count.name:  a character value specifying the name of the column of counts in the resulting table.
#'
#' @export 
#' @import formulaic
#' @source create.filter.expression.R
#' @examples 
#' 
#'  dat = snack.dat
#'  age.name = "Age"
#'  income.name = "Income"
#'  region.name = "Region"
#'  gender.name = "Gender"
#'  
#'  dt.count(dat = dat)
#'  dt.count(dat = dat, grouping.variables = c(region.name, gender.name))
#'  dt.count(dat = snack.dat, the.filter = "Age > 65", grouping.variables = c(region.name, gender.name), count.name = "Records with Age > 65") 
#'  dt.count(dat = dat, the.filter = 'get(age.name) > 20', grouping.variables = c(age.name,income.name), grouping.type = 'keyby', count.name = 'count')
#'  
dt.count <- function(dat, the.filter = NULL, grouping.variables = NULL, grouping.type = "keyby", count.name = "N", ...){
  
  require(data.table)
  setDT(dat)
  
  the.filter <- create.filter.expression(the.filter = the.filter)
  
  
  if(grouping.type == "by"){
    if(!is.null(the.function)){
      the.count <- dat[eval(the.filter), .(the_new_name_happens_to_be = the.function(get()))]
    }
    
    the.count <- dat[eval(the.filter), .(the_new_name_happens_to_be = .N), by = eval(grouping.variables)]
  }
  if(grouping.type != "by"){
    the.count <- dat[eval(the.filter), .(the_new_name_happens_to_be = .N), keyby = eval(grouping.variables)]
  }
  
  setnames(x = the.count, old = "the_new_name_happens_to_be", new = count.name)
  
  return(the.count)
}