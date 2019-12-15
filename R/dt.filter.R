#' dt.filter
#'
#' @description A fucntion that subsets the data corresponding to the filtering statement.
#'
#' @param dat:  a data.frame
#' @param the.filter:  a character value or expression stating the logical operations to be performed.
#'
#' @import formulaic
#' 
#' @examples
#' region.name = "Region"
#' gender.name = "Gender"
#' product.name = "Product"
#' income.name = "Income"
#' age.name = "Age"
#' persona.name = "Persona"
#' 
#' dt.filter(dat = formulaic::snack.dat, the.filter = "Region == 'Northeast' & Gender == 'Female' & Age >= 80 & Income > 145000 & Product == 'Cookie_Crumble'")
#' dt.filter(dat = formulaic::snack.dat, the.filter = "get(region.name) == 'Northeast' & get(gender.name) == 'Female' & get(age.name) >= 80 & get(income.name) > 145000 & get(product.name) == 'Cookie_Crumble'")
#' dt.filter(dat = formulaic::snack.dat, the.filter = expression(get(age.name) < 35 & get(persona.name) == "Millenial Muncher" & get(product.name) == "Tiramisoup" & get(income.name) <= 25000))
#'
#' @export
dt.filter <- function(dat, the.filter = NULL) {
  data.table::setDT(dat)
  
  the.filter <- create.filter.expression(the.filter = the.filter)
  
  return(dat[eval(the.filter),])
}
