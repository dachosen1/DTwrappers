#' total missing
#'
#' @param x:  a vector
#' @export
total.missing <- sum.missing <- function(x, ...) {
  return(sum(is.na(x)))
}
