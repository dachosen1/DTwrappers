# dat:  a data.frame object.

#' @the.variables:  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' the.filter:  a character value or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' grouping.variables:  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) and do not exist in the.variables will be used.

#' grouping.type:  a character value specifying whether the grouping should be sorted (keyby) or as is (by).  Defaults to keyby unless "by" is specified.

#' Calls dt.select with first.k = 1.

dt.first.row <-
  function(dat,
           the.variables = ".",
           the.filter = NULL,
           grouping.variables = NULL,
           grouping.type = "keyby",
           ...) {
    return(
      dt.select(
        dat = dat,
        the.variables = the.variables,
        the.filter = the.filter,
        grouping.variables = grouping.variables,
        grouping.type = grouping.type,
        first.k = 1,
        ...
      )
    )
  }
