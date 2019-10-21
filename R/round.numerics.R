# x:  a vector. 

# digits:  the number of digits to round to

# If x is numeric, integer, logical, or complex, then it will be rounded to the specified number of digits.  Otherwise, the values of x will be returned untouched.

round.numerics <- function(x, digits = 0, ...){
  if(is.numeric(x) | is.integer(x) | is.logical(x) | is.complex(x)){
    x <- round(x = x, digits = digits)
  }
  return(x)
}