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
#' @param name Name for the package to load, or a strategy with which it 
#'             is to be inferred. If omitted, then the package is simply 
#'             reinstalled, not also reloaded.
#' @param nameFromUrl Strategy with which to infer package name from URL, 
#'                    optional; this is only used if \code{useLocal} is 
#'                    \code{FALSE}. Omit to only reinstall and skip reload.
#' @export
RefreshPackage = function(packPath, useLocal, 
  name = NULL, nameFromUrl = NULL) {
# TODO: implement default name inference for package from URL (e.g., GitHub).
  
  # Local source for installation needs existence and explicit specification.
  local = file_test("-d", packPath) & useLocal

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
    # An alternative control flow here would be to set a NULL packPath and 
    # then do one check at the end. Instead, do control flow with early 
    # returns so that we can provide case-specific messaging.
    
    if (is.null(nameFromUrl)) {
      sprintf("Installed package from '%s', but it cannot be loaded with 
        neither a name nor a strategy with which to infer one.", packPath)
      warning(msg)
      # Early return since we lack name and inference strategy.
      return()
    }
    
    packName = nameFromUrl(packPath)
    if (is.null(packName) | identical("", packName)) {
      msg = sprintf("Failed to infer name for package installed from '%s', 
        so no attempt will be made to load it.", packPath)
      warning(msg)
      # Early return since the name inference failed.
      return()
    }
  }

  # Reload the package if a name was given or successfully inferred.
  library(packName, character.only = TRUE)

}
