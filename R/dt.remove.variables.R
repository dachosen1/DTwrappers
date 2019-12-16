#'dt.remove.variables
#' @description  a function to remove selected columns from dat
#'
#' @param  dat  a data.frame object
#' @param the.variables  a character vector with the column names to be removed from dat.
#'
#' @import data.table
#' @export
dt.remove.variables <- function(dat, the.variables) {
  data.table::setDT(dat)
  
  dat[, (the.variables) := NULL]
  
  return(dat)
}
