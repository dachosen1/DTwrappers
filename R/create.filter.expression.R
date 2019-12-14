#' create.filter.expression
#' 
#' @description A function that creates filter expressions. Expression could be a character value, logical value, or expression stating the logical operations.
#' 
#' @param the.filter The filter criteria could be a character value, logical value, or expression stating the logical operations to be performed in filtering the data.
#' @export
#' @examples 
#'
#' age.name = "Age"
#' region.name = "Region"
#'  
#' create.filter.expression(the.filter = NULL)
#' create.filter.expression(the.filter = c(age.name, region.name))
#' create.filter.expression(the.filter = age.name == region.name)
#' create.filter.expression(the.filter = "get(region.name) == 'South'")
#' create.filter.expression(the.filter = "get(age.name) > 20")
#' create.filter.expression(the.filter = "get(region.name) == 'South' & get(age.name) > 20")
#'  
#' @export
create.filter.expression <- function(the.filter){
  if(is.null(the.filter)){
    the.filter <- TRUE
  }
  if(is.character(the.filter)){
    the.filter <- parse(text = the.filter)
  }
  return(the.filter)
}


