#' Convert factor to characters
#' 
#' function that converts factors to character 
#' 
#' @param x:  a vector of any kind.  If x is a factor, it will be converted to a character vector.
#' 
#' @export
convert.factor.to.character <- function(x, ...){
  if(is.factor(x)){
    x <- as.character(x = x)
  }
  return(x)
}
