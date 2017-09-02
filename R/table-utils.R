# table-utils.R


#' Sampler for the first observation of each unique value for a variable.
#'
#' \code{oneOfEach} identifies the unique values from a particular column 
#' of a data
#'
#' @param dt: The \code{data.table} from which to sample.
#' @param column: Name of the column of interest.
#' @param n: Number of first-occurring observation(s) to grab for each 
#'           unique value of \code{column}.
#' @export
nOfEach = function(dt, column, n) {
  # TODO: how/what to test:
  # 1. Empty table.
  # 2. Missing column
  # 3. Value(s) with fewer than n observations.
  # 4. Universal quantification on nrow (should match #(unique(column))).
  if (n < 1) { stop("Choose to take at least one of each value") }
  vals = dt[, get(column)]
  indexer = sapply( X=unique(vals), FUN=function(v) { which(v == vals)[1:n] } )
  dt[indexer, ]
}


#' Sampler of first observation of a particular table column.
#'
#' \code{oneOfEach} grabs the first observation for each unique value 
#' for a particular column within a table.
#'
#' @param ...: Arguments to \code{nOfEach}.
#' @return Data table with first occurrence of each unique value from the 
#'         indicated column in the given table.
#' @seealso \code{\link{nOfEach}}
#' @export
oneOfEach = function(...) {
  # TODO: how/what to test:
  # 0. Empty table
  # 0.5. Missing column
  # 1. duplicate 'n' argument --> error.
  # 2. no duplicates for any of the unique values of the column chosen (universal).
  nOfEach(..., n=1)
}
