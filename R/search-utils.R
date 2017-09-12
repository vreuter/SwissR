# Tools for searching.


#' Locator of the proper bin (index) of a rate.
#'
#' \code{binRate} takes a vector of values that it interprets as endpoints of 
#' bins, among which a rate is to be placed. It wraps \code{findInterval}
#' binary search as the strategy for determining the index of the "bin" into 
#" which the rate should be placed.
#'
#' @param rate Rate for which to find bin placement.
#' @param endpoints Vector of bin bounds.
#' @param sorted Whether the bounds have been sorted.
#' @param nonnegative Whether the rate is required to be nonnegative.
#' @param leOne Whether the rate is required to be no more than one.
#' @return Index of the bin into which the rate should be placed.
#' @family search
#' @seealso \code{\link[base]{findInterval}}
#' @export
binRate = function(rate, bounds, sorted=FALSE, nonnegative=TRUE, leOne=TRUE) {
  bounds = unique(bounds)
  if (!sorted) { bounds = sort(bounds) }
  if (nonnegative) {
    if (rate < 0) { stop("Negative rate: ", rate) }
    if (0 != bounds[1]) { bounds = c(0, bounds) }
  }
  if (leOne) {
    if (rate > 1) { stop("Too-high rate: ", rate) }
    if (1 != bounds[length(bounds)]) { bounds = c(bounds, 1) }
  }
  findInterval(rate, bounds)
}
