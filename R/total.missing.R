# x:  a vector

total.missing <- sum.missing <- function(x, ...){
  return(sum(is.na(x)))
}
