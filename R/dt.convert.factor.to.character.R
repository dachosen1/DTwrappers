#' dt.convert.factor.to.character
#' 
#' @description A function that converts a factor to a character. 
#' 
#' @param  dat:  a data.frame object.
#' @param  the.variables:  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.
#' @param  add.function.name:  a logical value.  If TRUE, then the names of the variables are concatenated with the suffix convert.factor.to.character.
#' @export
#' @source convert.factor.character.R
#' @source dt.lapply.R
#' @import formulaic
#' @examples
#'  dat <- snack.dat
#'  income.group.name = "Income Group"
#'  region.name = "Region"
#'  dat[, eval(income.group.name) := as.factor(get(income.group.name))]
#'  dat[, eval(region.name) := as.factor(get(region.name))]
#'  str(dt.convert.factor.to.character(dat = dat, the.variables = c(income.group.name, region.name)))
#' 
#' @export
dt.convert.factor.to.character <-
  function(dat,
           the.variables = ".",
           add.function.name = FALSE,
           ...) {
    return(
      dt.lapply(
        dat = dat,
        the.variables = the.variables,
        the.functions = "convert.factor.to.character",
        add.function.name = add.function.name,
        ...
      )
    )
  }

