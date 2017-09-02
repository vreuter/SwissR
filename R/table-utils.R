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
  if (n < 1) { stop("Choose to take at least one of each value") }
  vals = unique(dt[, get(column)])
  indexer = sapply( X=vals, FUN=function(v) { which(v == vals)[1:n] } )
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
  nOfEach(..., n=1)
}
