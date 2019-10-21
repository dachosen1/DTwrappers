# digits:  the number of digits to round to

# All other inputs:  see help(prettyNum)

round.and.format.numerics <- function(x, digits = 0, big.mark = "",   big.interval = 3L, small.mark  = "", small.interval = 5L, decimal.mark = getOption("OutDec"), input.d.mark = decimal.mark, preserve.width = c("common", "individual", "none"), zero.print = NULL, replace.zero = FALSE, drop0trailing = FALSE, is.cmplx = NA, ...){
  if(is.numeric(x) | is.integer(x) | is.logical(x)){
    x <- prettyNum(x = round(x = x, digits = digits), big.mark = big.mark, big.interval = big.interval, small.mark  = small.mark, small.interval = small.interval, decimal.mark = decimal.mark, input.d.mark = input.d.mark, preserve.width = preserve.width, zero.print = zero.print, replace.zero = replace.zero, drop0trailing = drop0trailing, is.cmplx = is.cmplx, ...)
  }
  return(x)
}
