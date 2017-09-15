# stats-utils.R
# Tools for working with statistics.


#' Calculator of Kullack-Leibler divergence
#'
#' \code{divKL} accepts a pair of vectors of probability vectors that 
#' it interprets as defining a pair of discrete distributions and uses 
#' the values to compute Kullback-Leibler divergence between the distributions. 
#' The probability vectors must be normalized and be of equal length.
#' The logarithm base can be provided as an optional argument to \code{base}. 
#' The default \code{base} is 2 so that the unit is the bit.
#'
#' @param pA Vector of probabilities definining one distribution,
#' @param pB Vector of probabilities defining another distribution.
#' @param base The logarithm base to use. Common values are 2 for a value 
#'             in bits and natural constant \code{e} for a value in nats.
#' @return Kullback-Leibler divergence between the given distributions.
divKL = function(pA, pB, base=2) {
  n = length(pA)
  if (n != length(pB)) { stop("Probability vectors must have same length.") }
  if (0 != sum(pA) || 0 != sum(pB)) { stop("Probability vectors must sum to 1.") }
  sum(sapply(X=1:n, FUN=function(i) pA[i]*log(pA[i]/pB[i], base=base)))
}


#' Assurance that each value of a vector is nonempty.
#'
#' \code{insertPseudocounts} takes an observation vector and a 
#' \code{pseudocount}, replacing all zeros in \code{observations} with the
#' \code{pseudocount}.
#'
#' @param observations Vector of values to ensure nonzero.
#' @param pseudocount Value with which to replace zeros.
#' @return Updated observations vector.
#' @export
insertPseudocounts = function(observations, pseudocount=1) {
  if (!(inherits(observations, "numeric") && inherits(pseudocount, "numeric"))) {
    stop("Observations and pseudocount must be a numeric.")
  }
  observations[which(observations == 0)] = pseudocount
  return(observations)
}


#' Counter of number of observations that fall into each of a number of bins
#'
#' \code{countBinnedObservations} takes an observations vector and a vector 
#' of values that define the (inclusive) left endpoints of a series of 
#' bins that will discretize the distribution of observations.
#' 
#' @param observations The observed values that come from a theoretically 
#'                     continuous distribution to discretize.
#' @param binBounds The vector of values that defines the (inclusive) left 
#'                      endpoint for each bin, that collectively will 
#'                      discretize the hypothetically continuous distribution 
#'                      of observations.
#' @param pseudocount The count for any bin that contains no observations.
#' @return Observation count for each bin.
#' @seealso \code{\link[base]{findInterval}}
#' @export
countBinnedObservations = function(observations, binBounds, pseudocount=0) {
  binBounds = sort(unique(binBounds))    # Enforce strict increase.
  # Extra bin for observations < min(binBounds).
  fullCounts = numeric(1 + length(binBounds))
  obsCounts = table(findInterval(observations, binBounds))
  fullCounts[1 + as.numeric(names(obsCounts))] = as.vector(obsCounts)
  if (0 != pseudocount) { insertPseudocounts(fullCounts, pseudocount) } else fullCounts
}
