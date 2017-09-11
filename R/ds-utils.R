# Data structure utility functions


#' Counter of items of certain type(s) from a composite collection.
#'
#' \code{countItems} takes a composite object and a collection of types from 
#' of objects of interest and counts how many objects of those types occur 
#' in total in the composite collection.
#'
#' @param composite The composite object (e.g., list) from which to count items.
#' @param singletonTypes The collection of names of object types of interest 
#' (e.g., \code{c("data.table", "data.frame")}).
#' @return The cumulative count of items of one of the types named in 
#'         \code{singletonTypes}.
#' @export
countListItems = function(composite, singletonTypes) {
  lists = Filter(function(obj) inherits(obj, list), composite)
  numSingletons = length(Filter(function(obj) inherits(obj, singletonTypes)))
  if (0 == length(lists)) numSingletons else (numSingletons + countListItems(lists, singletonTypes))
}
