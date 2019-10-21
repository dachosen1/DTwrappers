# x:  a vector

mean.measured <- function(x, ...){
  return(mean(!is.na(x)))
}
