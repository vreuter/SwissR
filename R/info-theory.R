# Information theory tools


#' Calculator of Shannon entropy for a vector of observations.
#'
#' \code{shannonEntropy} calculates the Shannon entropy for a vector 
#' of observations, using empirical probability of each unique value 
#' as its probability. The input vector is assumed to contain 
#' observations from a discrete distribution. If working with continuous 
#' data, the observations should be discretized prior to using this function.
#' The base two logarithm is used, so the unit for the result is bits.
#'
#' @param observations The observed values for which to calculate entropy.
#' @return The Shannon entropy of the empirical distribution defined by 
#'         the given observations.
#' @export
shannonEntropy = function(observations) {
  # Handle non-factor input.
  if (!is.factor(observations)) {
    observations = factor(observations, levels=unique(observations))
  }
  num_obs = length(observations)
  counts = sapply(X=levels(observations), FUN=function(x) sum(x == observations))
  probs = sapply(X=counts, FUN=function(n) n/num_obs)
  # We're protected from zero-probability cases since we've defined the 
  # domain to be the set of unique observations.
  weighted_information = sapply(X=probs, FUN=function(p) p*log2(1/p))
  sum(weighted_information)
}


#' Alias for \code{shannonEntropy}
#' @seealso \code{\link{shannonEntropy}}
#' @export
entropy = shannonEntropy
