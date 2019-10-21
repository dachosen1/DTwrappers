# internal function

# x:  a vector

# na.rm:  a logical value specifying whether missing values should be removed from the calculations specified by the.functions.

lower.quartile <- function(x, na.rm = TRUE, ...){
  return(quantile(x = x, probs = 0.25, na.rm = na.rm))
}
