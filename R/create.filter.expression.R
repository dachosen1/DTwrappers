#' create.filter.expression
#' 
#'  @description A function that filters expression. Expression could be a character, logical value, or logical operation code.
#' 
#'  @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.
#'  @export
#'  @examples 
#'  
#'  @import formulaic
#'  @import dplyr
#'  
#'  dat <- snack.dat
#'  
#'  age.name = "Age"
#'  region.name = "Region"
#'  
#'  the.filter = c(age.name, region.name)
#'  create.filter.expression(the.filter = the.filter)
#'  create.filter.expression(the.filter = age.name == region.name)
#'  
create.filter.expression <- function(the.filter){
  if(is.null(the.filter)){
    the.filter <- TRUE
  }
  if(is.character(the.filter)){
    the.filter <- parse(text = the.filter)
  }
  return(the.filter)
}

dat %>% filter(get(age.name) > 18) %>% summarize(mean = mean(get(age.name)))
