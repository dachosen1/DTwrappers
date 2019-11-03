#' var numerics
#' x:  a vector. 
#' na.rm:  a logical value specifying whether missing values should be removed from the calculations specified by the.functions.
#'
#' non.numeric.value:if "missing", returns NA for variables that are not numeric, integer, logical, or complex.  Otherwise returns first entry of the vector.
#' If x is numeric, integer, logical, or complex, then its variance will be computed.  Otherwise, the first value of x will be returned untouched.
#' @export
var.numerics <-
  function(x,
           na.rm = TRUE,
           non.numeric.value = "missing",
           ...) {
    if (is.numeric(x) | is.integer(x) | is.logical(x) | is.complex(x)) {
      return(var(x = x, na.rm = na.rm))
    }
    if (non.numeric.value == "missing") {
      return(NA)
    }
    return(x[1])
  }