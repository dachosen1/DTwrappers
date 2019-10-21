# x:  a character vector of values that should be a numeric vector but was coerced to a character due to a small number of entries.

# threshold.for.numeric:  a value between 0 and 1 specifying the maximum proportion of x that does not "look" numeric, e.g. "2.154" is a character value that can be converted to a numeric value.. If threshold.for.numeric = 0.1, then no more than 10% of the values in x can be values that do not "look" numeric.

identify.character.coercion.culprits <- function(x, threshold.for.numeric = 0.5, ...){
  w1 <- which(is.na(x))
  
  options(warn = -1)
  y <- as.numeric(x)
#  options(warn = 0)
  
  if(mean(is.na(y)) > threshold.for.numeric){
    return(NA)
  }
  
  w2 <- which(is.na(y))
  
  the.indices <- w2[!(w2 %in% w1)]
  the.culprits <- unique(x[the.indices])
  return(the.culprits)
}

