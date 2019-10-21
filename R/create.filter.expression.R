#' Internal function
#' 
#' 

#' the.filter:  a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.
#' @export
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
