# x:  a numeric vector

# k:  the number of digits to round to.  This number will be exact, in that there will be exactly k decimal places listed even if this includes lagging zeros.  For instance, setting k = 5 for x = 2.54 would result in 2.54000

# decimal:  The character specifying the decimal, which splits between whole numbers (greater than 1) and the fractional component (less than 1).

round.exactly.k.digits <- function(x, k = 0, decimal = ".", ...){
  
  y <- round(x = x, digits = k)
  
  if(y == floor(y)){
    if(k <= 0){
      res <- as.character(y)
    }
    if(k > 0){
      res <- sprintf("%d.%s", x, paste(rep.int(x = 0, times = k), collapse = ""))
    }
  }
  if(y != floor(y)){
    the.pieces <- strsplit(x = as.character(y), split = decimal, fixed = TRUE)[[1]]
    
    nc <- nchar(the.pieces[2])
    
    if(nc == k){
      res <- as.character(y)
    }
    if(nc < k){
      res <- sprintf("%s%s", y, paste(rep.int(x = 0, times = k - nc), collapse = ""))
    }
  }
  return(res)
}


