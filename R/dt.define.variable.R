#' dat:  a data.frame object

#' variable.name:  a character value specifying the name of the new column

#' the.values:  a vector of data specifying the values of the new column.

#' Note:  the data.frame dat will be converted to a data.table object to facilitate adding the new column by reference (e.g. efficiently with regard to the usage of memory)

#' @export 
dt.define.variable <- function(dat, variable.name, the.values, specification = "by.value", the.filter = NULL){
  require(data.table)
  setDT(dat)
  
  the.filter <- create.filter.expression(the.filter = the.filter)
  
  if(specification == "by.expression"){
    the.values <- parse(text = the.values)
  }
    
  dat[eval(the.filter), eval(variable.name) := eval(the.values)]

  return(dat)
}
