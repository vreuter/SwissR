# functools.R
# Tools for working with functions and Functionals.


#' List builder
#'
#' \code{foldRightList} takes a function and a collection of items, 
#' then applies that function to each of the items, creating a list 
#' in which each item becomes a key/name and the return value from 
#' function \code{f} applied to each item is bound to that item, 
#' i.e. key/name.
#'
#' @param f Function to apply to the collection of \code{items}.
#' @param items The collection of inputs, in which each then becomes 
#'              a key/name in the resulting list.
#' @return \code{list} in which each name is an element from 
#'         \code{items} and each value is the result of applying 
#'         \code{f} to that item.
#' @seealso \code{\link[base]{Reduce}} for what's wrapped.
#' @export
foldRightList = function(f, items) { Reduce(f, items, init=list()) }


#' Helper for creating function argument to rightward list folder.
#'
#' Working directly with \code{base::Reduce} can be a little tricky, 
#' so this function aims to simplify using it. It's intended to be 
#' partially applied with something like:
#' \code{pryr::partial(updateFoldRight, f=identity)}
#' and then passed as the function argument to \code{foldRightList}.
#'
#' @param result The growing collection of a \code{Reduce} operation 
#'               in which the "reduction" is actually list formation.
#' @param newArg The new argument being processed.
#' @param f The function to apply to the new argument to generate a 
#'          new element for \code{result}.
#' @seealso \code{\link{foldRightList}} for the intended destination 
#'          of a partially-applied version of this, 
#'          \code{\link[base]{Reduce}} for what's wrapped.
#' @export
updateFoldRight = function(result, newArg, f) {
  result[[newArg]] = f(newArg)
  return(result)
}
