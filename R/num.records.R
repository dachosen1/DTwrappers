# x:  a vector

num.records <- function(x, na.rm = FALSE, ...){
  if(na.rm == TRUE){
    return(length(x = x[!is.na(x)], ...))
  }
  return(length(x = x, ...))
}
