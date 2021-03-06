% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functools.R
\name{foldRightList}
\alias{foldRightList}
\title{List builder}
\usage{
foldRightList(f, items)
}
\arguments{
\item{f}{Function to apply to the collection of \code{items}.}

\item{items}{The collection of inputs, in which each then becomes 
a key/name in the resulting list.}
}
\value{
\code{list} in which each name is an element from 
        \code{items} and each value is the result of applying 
        \code{f} to that item.
}
\description{
\code{foldRightList} takes a function and a collection of items, 
then applies that function to each of the items, creating a list 
in which each item becomes a key/name and the return value from 
function \code{f} applied to each item is bound to that item, 
i.e. key/name.
}
\seealso{
\code{\link[base]{Reduce}} for what's wrapped.
}
