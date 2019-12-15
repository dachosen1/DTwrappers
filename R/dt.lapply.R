#' dt.lapply
#'
#' @description  TBD
#'
#' @param dat:  a data.frame object.
#' @param the.variables:  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).
#' @param the.functions:  a character vector or list specifying the name of the function to apply to the variables.  This may either be specified by the name of the function as a character (e.g. "mean") or by defining a function; e.g. function(x){return(mean(x = x))} that can be computed on each column of data as specified in the.variables.
#' @param the.filter:  a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.
#' @param grouping.variables:  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.
#' @param grouping.type:  a character value specifying whether the grouping should be sorted (keyby) or as is (by).  Defaults to keyby unless "by" is specified.
#' @param add.function.name:  a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.  Only applies if the.functions is of length 1.
#' @param other.params:  a character value specifying any additional parameters needed to call the.functions.  For instance, if the.functions = "mean", and you would like to remove missing values, then specifying other.params = "na.rm = TRUE" as a character would suffice.  Multiple parameters can be specified with comma separation, e.g. other.params = "trim = 1, na.rm = TRUE".  Note that all of the parameters supplied must apply to all of the.functions
#'
#' @import data.table
#'
#' @export
dt.lapply <-
  function(dat,
           the.variables = ".",
           the.functions,
           the.filter = NULL,
           grouping.variables = NULL,
           grouping.type = "keyby",
           add.function.name = TRUE,
           other.params = "",
           ...) {
    data.table::setDT(dat)
    
    if (length(the.functions) == 0 |
        is.null(the.functions) | is.na(the.functions[1])) {
      return("Error:  the.functions must be specified.")
    }
    
    if (the.variables[1] == ".") {
      the.variables <- names(dat)
    }
    
    grouping.variables <-
      unique(grouping.variables[grouping.variables %in% names(dat)])
    
    the.variables <-
      unique(the.variables[the.variables %in% names(dat) &
                             !(the.variables %in% grouping.variables)])
    
    the.filter <- create.filter.expression(the.filter = the.filter)
    
    the.grid <-
      setDT(expand.grid(Variable = the.variables, Function = the.functions))
    
    num.functions <- length(the.functions)
    
    if (num.functions > 1 | add.function.name == TRUE) {
      the.grid[, Outcome := sprintf("%s_%s", Variable, Function)]
    }
    if (num.functions == 1 & add.function.name == FALSE) {
      the.grid[, Outcome := Variable]
    }
    
    for (i in 1:the.grid[, .N]) {
      the.grid[, Command := sprintf("`%s` = %s(`%s`, ...)", Outcome, Function, Variable)]
    }
    
    j.statement <-
      sprintf(".(%s)", the.grid[, paste(Command, collapse = ", ")])
    
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
    
    dt.statement <-
      gsub(
        pattern = "...",
        replacement = other.params,
        x = dt.statement,
        fixed = TRUE
      )
    these.results <- eval(parse(text = dt.statement))
    return(these.results)
  }
