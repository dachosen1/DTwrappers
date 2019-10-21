# x:  a vector

total.measured <- sum.measured <- function(x, ...){
  return(sum(!is.na(x)))
}
