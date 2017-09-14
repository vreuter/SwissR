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
