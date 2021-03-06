% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkg-utils.R
\name{refreshPackage}
\alias{refreshPackage}
\title{Installer and loader of a package, either local or from github}
\usage{
refreshPackage(packPath, local = TRUE, name = NULL, nameFromUrl = NULL,
  load = TRUE, force = FALSE)
}
\arguments{
\item{packPath}{Path to the package to install and load.}

\item{local}{Use local source for the installation, default \code{TRUE}.}

\item{name}{Name for the package to load, or a strategy with which it 
is to be inferred. If omitted, then the package is simply 
reinstalled, not also reloaded.}

\item{nameFromUrl}{Strategy with which to infer package name from URL, 
optional; this is only used if \code{local} is 
\code{FALSE}. Omit to only reinstall and skip reload.}

\item{load}{Whether to load the package into the global environment 
if it was successfully located and installed.}

\item{force}{Whether to force installation even if there's no hash delta.}
}
\description{
\code{refreshPackage} takes a path to a file or folder, or a URL,  
determines whether it exists, and installs it. In order for local 
source to be used, the path given must exist locally and \code{local} 
must be true. Once the package given by \code{packPath} is installed, 
this function attempts to load the package, using either a given name or 
an inferred one. If no \code{name} is given and the name inference 
strategy employed fails to yield a viable package name candidate, then 
the attempt to load the package is skipped.
}
\seealso{
Other packages: \code{\link{locatePackage}}
}
