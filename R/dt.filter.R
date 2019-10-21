#' dat:  a data.frame

#' the.filter:  a character value or expression stating the logical operations to be performed.

#' A subset of the data corresponding to the rows on which the filtering statement are TRUE is returned.

#' @export
dt.filter <- function(dat, the.filter = NULL){
  require(data.table)
  setDT(dat)
  
  the.filter <- create.filter.expression(the.filter = the.filter)

  return(dat[eval(the.filter),])
}
