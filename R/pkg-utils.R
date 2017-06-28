# Utility functions related to working with R packages

library("devtools")


#' Installer and loader of a package, either local or from github
#'
#' \code{RefreshPackage} takes a path to a file or folder, or a URL,  
#' determines whether it exists, and installs it. In order for local 
#' source to be used, the path given must exist locally and \code{useLocal} 
#' must be true. Once the package given by \code{packPath} is installed, 
#' this function attempts to load the package, using either a given name or 
#' an inferred one. If no \code{name} is given and the name inference 
#' strategy employed fails to yield a viable package name candidate, then 
#' the attempt to load the package is skipped.
#'
#' @param packPath Path to the package to install and load.
#' @param useLocal Whether to use local source for the installation.
#' @param name Name for the package to load.
#' @param nameFromUrl Strategy with which to infer package name from URL.
#' @export
RefreshPackage = function(packPath, useLocal = FALSE, name = NULL, 
  nameFromUrl = NULL) {
# TODO: implement default name inference for package from URL (e.g., GitHub).
  
  # Local source for installation needs existence and explicit specification.
  local = file_test(packPath, "-d") & useLocal

  # Install.
  if (local) { devtools::install_local(packPath) }
  else {
    # If local installation was requested but not possible, issue warning.
    if (useLocal) { 
      warning(sprintf(
        "Could not use local install option for '%s'; does it exist?", 
        packPath))
    }
    devtools::install_github(packPath)
  }

  # Loading phase
  # First, infer name.
  if (!is.null(name)) { packName = name }
  else if (local) { packName = basename(packPath) }
  else {
    if (is.null(nameFromUrl)) {
      warning(sprintf(
        "Installed package from '%s' but could not infer name to reload", 
        packPath))
    }
    # Early return since we have nothing to load.
    return()
  }

  # Reload the package if a name was given or successfully inferred.
  library(packName)

}
