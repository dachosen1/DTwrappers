# x:  a vector

mean.missing <- function(x, ...){
  return(mean(is.na(x)))
}
