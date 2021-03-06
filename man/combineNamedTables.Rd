% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-utils.R
\name{combineNamedTables}
\alias{combineNamedTables}
\title{Creator of long data that preserves individual table identity.}
\usage{
combineNamedTables(dts, newCol = "id")
}
\arguments{
\item{dts}{Named list of data tables to combine.}

\item{newCol}{Name for the new field to create in each table.}
}
\value{
Combined (long) data table.
}
\description{
\code{combineNamedTables} takes a (named) list of data tables and 
interprets each value in \code{names(dts)} as a constant value that should 
be added as \code{newCol} for each observation in each element of \code{dts}.
That is, each table in \code{dts} gets a constant value for \code{newCol}. 
That value is the key/name in the \code{dts} list that maps to the table.
The tables are then combined row-wise to create long data, but original 
table identity is preserved in \code{newCol}.
}
