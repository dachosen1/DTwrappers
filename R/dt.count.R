#' dt.count
#' 
#' @description A function that counts the number of rows that meets the qualification such as filter and the by. If both filter and the by are not set up, then it will return the number of rows. 
#' 
#' @param dat:  a data.frame object.
#' @param the.filter: a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function. Defaults to "NULL" unless specified.
#' @param grouping.variables:  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.
#' @param grouping.type:  a character value specifying whether the grouping should be sorted (keyby) or as is (by).  Defaults to keyby unless "by" is specified.
#' @param count.name:  a character value specifying the name of the column of counts in the resulting table. Defaults to "N" unless specified.
#' @note the data.frame dat will be converted to a data.table object to facilitate adding the new column by reference (e.g. efficiently with regard to the usage of memory)
#'
#' @export 
#' @examples 
#' 
#'  data('snack.dat')
#'  age.name = "Age"
#'  income.name = "Income"
#'  region.name = "Region"
#'  gender.name = "Gender"
#'  
#'  dt.count(dat = snack.dat)
#'  dt.count(dat = snack.dat, grouping.variables = c(region.name, gender.name))
#'  dt.count(dat = snack.dat, the.filter = "Age > 65", grouping.variables = c(region.name, gender.name), count.name = "Records with Age > 65") 
#'  dt.count(dat = snack.dat, the.filter = 'get(age.name) > 20', grouping.variables = c(age.name,income.name), grouping.type = 'keyby', count.name = 'count')
#' 
#' @source create.filter.expression.R
#' @import data.table
#' @export  
dt.count <- function(dat, the.filter = NULL, grouping.variables = NULL, grouping.type = "keyby", count.name = "N", ...){
  
  data.table::setDT(dat)
  
  the.filter <- create.filter.expression(the.filter = the.filter)
  
  
  if(grouping.type == "by"){
    the.count <- dat[eval(the.filter), .(the_new_name_happens_to_be = .N), by = eval(grouping.variables)]
  }
  if(grouping.type != "by"){
    the.count <- dat[eval(the.filter), .(the_new_name_happens_to_be = .N), keyby = eval(grouping.variables)]
  }
  
  setnames(x = the.count, old = "the_new_name_happens_to_be", new = count.name)
  
  return(the.count)
}
