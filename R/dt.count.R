#' create.filter.expression
#'
#' @description A function that creates filter expressions. Expression could be a character value,
#' logical value, or expression stating the logical operations.
#'
#' @param the.filter The filter criteria could be a character value, logical value, or expression
#' stating the logical operations to be performed in filtering the data.
#'
#' @export
#' @examples
#' age.name = "Age"
#' region.name = "Region"
#'
#' create.filter.expression(the.filter = NULL)
#' create.filter.expression(the.filter = c(age.name, region.name))
#' create.filter.expression(the.filter = age.name == region.name)
#' create.filter.expression(the.filter = "get(region.name) == 'South'")
#' create.filter.expression(the.filter = "get(age.name) > 20")
#' create.filter.expression(the.filter = "get(region.name) == 'South' & get(age.name) > 20")
#' 
#'@export
create.filter.expression <- function(the.filter) {
  if (is.null(the.filter)) {
    the.filter <- TRUE
  }
  if (is.character(the.filter)) {
    the.filter <- parse(text = the.filter)
  }
  return(the.filter)
}




#' dt.count
#'
#' @description A function that counts the number of rows that meets the qualification such as filter and the by. If both filter and the by are not set up, then it will return the number of rows.
#'
#' @param dat  a data.frame object.
#' @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function. Defaults to "NULL" unless specified.
#' @param grouping.variables  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.
#' @param grouping.type  a character value specifying whether the grouping should be sorted (keyby) or as is (by).  Defaults to keyby unless "by" is specified.
#' @param count.name  a character value specifying the name of the column of counts in the resulting table. Defaults to "N" unless specified.
#' @note the data.frame dat will be converted to a data.table object to facilitate adding the new column by reference (e.g. efficiently with regard to the usage of memory)
#' @import formulaic
#' @export
#' @examples
#'
#'  age.name = "Age"
#'  income.name = "Income"
#'  region.name = "Region"
#'  gender.name = "Gender"
#'
#'  dt.count(dat = formulaic::snack.dat)
#'
#'  dt.count(dat = formulaic::snack.dat,
#'  grouping.variables = c(region.name, gender.name))
#'
#'  dt.count(dat = formulaic::snack.dat,
#'  the.filter = "Age > 65",
#'  grouping.variables = c(region.name, gender.name),
#'  count.name = "Records with Age > 65")
#'
#'  dt.count(dat = formulaic::snack.dat,
#'  the.filter = 'get(age.name) > 20',
#'  grouping.variables = c(age.name,income.name),
#'  grouping.type = 'keyby', count.name = "counts")
#'
#' @source DTwrapers::create.filter.expression
#' @import data.table
#' @export
dt.count <-
  function(dat,
           the.filter = NULL,
           grouping.variables = NULL,
           grouping.type = "keyby",
           count.name = "N") {
    data.table::setDT(dat)
    "." <- NULL
    the.filter <- create.filter.expression(the.filter = the.filter)
    
    
    if (grouping.type == "by") {
      the.count <-
        dat[eval(the.filter), .(the_new_name_happens_to_be = .N), by = eval(grouping.variables)]
    }
    if (grouping.type != "by") {
      the.count <-
        dat[eval(the.filter), .(the_new_name_happens_to_be = .N), keyby = eval(grouping.variables)]
    }
    
    setnames(x = the.count, old = "the_new_name_happens_to_be", new = count.name)
    
    return(the.count)
  }


#' dt.filter
#'
#' @description A fucntion that subsets the data corresponding to the filtering statement.
#'
#' @param dat  a data.frame
#' @param the.filter  a character value or expression stating the logical operations to be performed.
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
#' dt.filter(dat = formulaic::snack.dat, 
#'          the.filter = "Region == 'Northeast' & Gender == 'Female' & Age >= 80 & Income > 145000 & 
#'          Product == 'Cookie_Crumble'")
#' dt.filter(dat = formulaic::snack.dat, 
#'          the.filter = "get(region.name) == 'Northeast' & get(gender.name) == 'Female' & 
#'          get(age.name) >= 80 & get(income.name) > 145000 & get(product.name) == 'Cookie_Crumble'")

#' dt.filter(
#'  dat = formulaic::snack.dat,
#'  the.filter = expression(
#'    get(age.name) < 35 &
#'      get(persona.name) == "Millenial Muncher" &
#'      get(product.name) == "Tiramisoup" & get(income.name) <= 25000
#'  )
#')
#'
#' @source DTwrapers::create.filter.expression
#' @import data.table
#' @export
dt.filter <- function(dat, the.filter = NULL) {
  data.table::setDT(dat)

  the.filter <- create.filter.expression(the.filter = the.filter)
  return(dat[eval(the.filter), ])
}


#' dt select
#'
#' @description This provides more flexibility in selecting the rows to include such as first.k, last.k, or specific row by setting up the parameters.
#'
#'
#' @param dat A data.frame object.
#' @param the.variables A character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).
#' @param the.filter A character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.
#' @param grouping.variables A character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.
#' @param grouping.type  A character value specifying whether the grouping should be sorted (keyby) or as is (by).  Defaults to keyby unless "by" is specified.
#' @param first.k An integer indicating how many rows to select starting from the first row.  Note that grouping statements will select up to this number of rows in each group.  Additionally, if first.k is larger than the number of records in a group, then the maximum number of records will be selected.  When non-integer or non-positive values of first.k are selected, the algorithm will select first.k = max(c(1, round(first.k))).  If first.k is not a numeric or integer value, then by default first.k is set to select all of the rows.  Specifying row.indices takes precedence to specifying the parameter first.k; if row.indices is not NULL, then row.indices will be used, and first.k will not. Meanwhile, first.k takes precedence to last.k when both are specified.  See below.
#' @param last.k An integer indicating how many rows to select starting from the last row.  Note that grouping statements will select up to this number of rows in each group.  Additionally, if last.k is larger than the number of records in a group, then the maximum number of records will be selected.  When non-integer or non-positive values of last.k are selected, the algorithm will select last.k = max(c(1, round(last.k))).  If last.k is not a numeric or integer value, then by default last.k is set to select all of the rows.  Specifying row.indices takes precedence to specifying the parameter last.k (see below); if row.indices is not NULL, then it will be used, and last.k will not.  Meanwhile, first.k takes precedence to last.k when both are specified.
#' @param row.indices An integer vector specifying the row indices to return.  When grouping.variables is specified, these indices will be applied to each group.  Note that specifications outside of the range from 1 to the number of rows will be limited to existing rows from the data and group.  Specifying row.indices takes precedence to specifying the parameters first.k and last.k.  If row.indices is not NULL, it will be used.
#'
#'
#' @import formulaic
#' @source DTwrapers::create.filter.expression
#' @export
#' @examples
#'
#' id.name = 'User ID'
#' awareness.name = 'Awareness'
#' consideration.name = 'Consideration'
#' consumption.name = 'Consumption'
#' satisfaction.name = 'Satisfaction'
#' advocacy.name = 'Advocacy'
#' gender.name = 'Gender'
#'
#' dt.select(dat = formulaic::snack.dat,
#'          the.variables = c(id.name, awareness.name))
#' dt.select(
#'  dat = formulaic::snack.dat,
#'  the.filter = "Age > 65 &
#'           Region == 'Northeast' & Product == 'Tiramisoup' &
#'           Awareness == 1",
#'  the.variables = c(
#'    consideration.name,
#'    consumption.name,
#'    satisfaction.name,
#'    advocacy.name
#'  ),
#'  grouping.variables = c(gender.name)
#')
#'
#'dt.select(
#'  dat = formulaic::snack.dat,
#'  the.filter = "Age > 65 &
#'           Region == 'Northeast' & Product == 'Tiramisoup' &
#'           Awareness == 1",
#'  the.variables = c(
#'    consideration.name,
#'    consumption.name,
#'    satisfaction.name,
#'    advocacy.name
#'  ),
#'  grouping.variables = c(gender.name),
#'  first.k = 2
#')
#'
#'dt.select(
#'  dat = formulaic::snack.dat,
#'  the.filter = "Age > 65 &
#'          Region == 'Northeast' & Product == 'Tiramisoup' &
#'          Awareness == 1",
#'  the.variables = c(
#'    consideration.name,
#'    consumption.name,
#'    satisfaction.name,
#'    advocacy.name
#'  ),
#'  grouping.variables = c(gender.name),
#'  last.k = 2
#' )
#'          
#' dt.select(
#' dat = formulaic::snack.dat,
#'  the.filter = "Age > 65 & Region == 'Northeast' &
#'           Product == 'Tiramisoup' & Awareness == 1",
#'  the.variables = c(
#'    consideration.name,
#'    consumption.name,
#'    satisfaction.name,
#'    advocacy.name
#'  ),
#'  grouping.variables = c(gender.name),
#'  first.k = 2,
#'  last.k = 2
#')
#'
#'dt.select(
#'  dat = formulaic::snack.dat,
#'  the.filter = "Age > 65 & Region == 'Northeast' & Product == 'Tiramisoup' & Awareness == 1",
#'  the.variables = c(
#'    consideration.name,
#'    consumption.name,
#'    satisfaction.name,
#'    advocacy.name
#'  ),
#'  grouping.variables = c(gender.name),
#'  row.indices = 7:9
#' )
#'
#' @export
dt.select <-
  function(dat,
           the.variables = ".",
           the.filter = NULL,
           grouping.variables = NULL,
           grouping.type = "keyby",
           first.k = NULL,
           last.k = NULL,
           row.indices = NULL) {
    data.table::setDT(dat)
    
    .N <- NULL
    
    if (the.variables[1] == ".") {
      the.variables <- names(dat)
    }
    
    the.filter <- create.filter.expression(the.filter = the.filter)
    
    grouping.variables <-
      unique(grouping.variables[grouping.variables %in% names(dat)])
    
    the.variables <-
      unique(the.variables[the.variables %in% names(dat) &
                             !(the.variables %in% grouping.variables)])
    
    specified.first.k <- !is.null(first.k)
    specified.last.k <- !is.null(last.k)
    specified.row.indices <- !is.null(row.indices)
    
    if (specified.row.indices == TRUE) {
      total.rows <- dat[, .N]
      j.statement <-
        ".SD[row.indices[row.indices %in% 1:min(.N, total.rows)]], .SDcols = the.variables"
    }
    if (specified.row.indices == FALSE) {
      if (specified.first.k == TRUE) {
        if (!is.numeric(first.k) & !is.integer(first.k)) {
          first.k <- dat[, .N]
        }
        
        first.k <- max(c(1, round(first.k)))
        
        j.statement <-
          ".SD[1:min(.N, first.k)], .SDcols = the.variables"
      }
      if (specified.first.k == FALSE & specified.last.k == TRUE) {
        if (!is.numeric(last.k) & !is.integer(last.k)) {
          last.k <- dat[, .N]
        }
        
        last.k <- max(c(1, round(last.k)))
        
        j.statement <-
          ".SD[max(1, 1 + .N - last.k):.N], .SDcols = the.variables"
      }
      if (specified.first.k == FALSE & specified.last.k == FALSE) {
        j.statement <- ".SD, .SDcols = the.variables"
      }
      
      if (specified.first.k == T & specified.last.k == T) {
        j.statement <-
          '.SD[c(1:min(.N, first.k), max(1, 1 + .N - last.k):.N)], .SDcols = the.variables'
      }
    }
    
    null.status.grouping.variables <- is.null(grouping.variables)
    if (null.status.grouping.variables == TRUE) {
      grouping.statement <- ""
    }
    if (null.status.grouping.variables == FALSE) {
      grouping.word <- "keyby"
      if (grouping.type == "by") {
        grouping.word <- "by"
      }
      grouping.statement <-
        sprintf(", %s = c(%s)", grouping.word, paste(sprintf("'%s'", grouping.variables), collapse = ", "))
    }
    
    dt.statement <-
      sprintf("dat[%s, %s%s]", the.filter, j.statement, grouping.statement)
    
    the.results <- eval(parse(text = dt.statement))
    
    return(the.results)
  }


