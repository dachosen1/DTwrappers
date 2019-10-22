#' create.filter.expression
#' 
#'  @description A function that filters expression. Expression could be a character, logical value, or logical operation code.
#' 
#'  @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.
#'  @export
#'  @import formulaic
#'  @import dplyr
#'  @examples 
#'  
#'  dat <- snack.dat
#'  income.group.name <- "Income Group"
#'  region.name <- "Region"
#'  
#'  dat[, eval(income.group.name) := as.factor(get(income.group.name))]
#'  dat[, eval(region.name) := as.factor(get(region.name))]
#'  str(dat[, lapply(X = .SD, FUN = "convert.factor.to.character"), .SDcols = c(income.group.name, region.name)])
#'  

create.filter.expression <- function(the.filter){
  if(is.null(the.filter)){
    the.filter <- TRUE
  }
  if(is.character(the.filter)){
    the.filter <- parse(text = the.filter)
  }
  return(the.filter)
}
