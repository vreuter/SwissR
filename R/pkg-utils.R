# Utility functions related to working with R packages

library("devtools")


#' Finder of package installation source location candidates.
#'
#' \code{locatePackage} uses a package name, path(s) to folder(s), and 
#' some environment variables to find all of the extant location candidates 
#' for the installation from source for package indicated by \code{name}.
#'
#' @param pkg_name Name of the package of interest.
#' @param paths Collection of full filepaths, each of which should be 
#'              considered for the presence of package \code{pkg_name}.
#' @param env_vars Names of environment variables, each of which should 
#'                 point to a filepath to consider for the presence of 
#'                 \code{pkg_name}.
#' @return Path to first location at which \code{pkgName} exists.
#' @family packages
#' @export
locatePackage = function(
  pkg_name, paths = NULL, env_vars = c("CODE", "STAGE", "CODEBASE")) {
  
  # Prioritize explicitly specified paths.
  if (is.null(paths)) {
    candidates = c()
  } else {
    pathCandidates = sapply(
      X = paths, FUN = function(basepath) {.filepath(basepath, pkg_name)})
    # Nulls will cause error here, but that's OK; there shouldn't be NULLs.
    candidates = pathCandidates[which(sapply(
      X = pathCandidates, FUN = function(p) { file_test("-d", p) }))]
  }
  
  # Secondarily, consider environment variables.
  varCandidates = sapply(X = env_vars, 
    FUN = function(var) { .envVarPath(var, pkg_name) })
  candidates = append(candidates, 
    varCandidates[-which(is.null(varCandidates))])
  
  pkg_path = candidates[which(sapply(X = candidates, FUN = .isDir))][1]
  if (is.na(pkg_path)) NULL else pkg_path
}



#' Installer and loader of a package, either local or from github
#'
#' \code{refreshPackage} takes a path to a file or folder, or a URL,  
#' determines whether it exists, and installs it. In order for local 
#' source to be used, the path given must exist locally and \code{local} 
#' must be true. Once the package given by \code{packPath} is installed, 
#' this function attempts to load the package, using either a given name or 
#' an inferred one. If no \code{name} is given and the name inference 
#' strategy employed fails to yield a viable package name candidate, then 
#' the attempt to load the package is skipped.
#'
#' @param packPath Path to the package to install and load.
#' @param local Use local source for the installation, default \code{TRUE}.
#' @param name Name for the package to load, or a strategy with which it 
#'             is to be inferred. If omitted, then the package is simply 
#'             reinstalled, not also reloaded.
#' @param nameFromUrl Strategy with which to infer package name from URL, 
#'                    optional; this is only used if \code{local} is 
#'                    \code{FALSE}. Omit to only reinstall and skip reload.
#' @family packages
#' @export
refreshPackage = function(packPath, local = TRUE, 
  name = NULL, nameFromUrl = NULL) {
# TODO: implement default name inference for package from URL (e.g., GitHub).
  
  # First, use information about desire to use local code to determine 
  # whether we should try to expand the argument to the package path 
  # parameter. Specifically, if installation from local source is desired, 
  # then the argument specifying the package may just be a name rather than 
  # a path.
  if (local && !file_test("-d", packPath)) {
    message(sprintf(
      "Attempting local install, trying path expansion for '%s'", packPath))
    packPath = locatePackage(packPath)
  }

  # Local source for installation needs existence and explicit specification.
  local = file_test("-d", packPath) && local

  # Install.
  if (local) { devtools::install_local(packPath) }
  else {
    # If local installation was requested but not possible, issue warning.
    if (local) { 
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
