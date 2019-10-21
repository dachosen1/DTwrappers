#' dat:  a data.frame object.
#' the.variables:  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.

#' add.function.name:  a logical value.  If TRUE, then the names of the variables are concatenated with the suffix convert.factor.to.character.

#' @export
dt.convert.factor.to.character <- function(dat, the.variables = ".", add.function.name = FALSE, ...){
  
  return(dt.lapply(dat = dat, the.variables = the.variables, the.functions = "convert.factor.to.character", add.function.name = add.function.name, ...))
}
