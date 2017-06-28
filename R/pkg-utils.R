# Utility functions related to working with R packages

library("devtools")


# TODO: implement default name inference strategy for package from URL (e.g., GitHub).
RefreshPackage = function(packPath, useLocal = FALSE, name = NULL, nameFromUrl = NULL) {
  
  # Use of local source for installation requires both availability and explicit specification.
  local = file_test(packPath, "-d") & useLocal

  # Installation
  if (local) { devtools::install_local(packPath) }
  else {
    # If local installation was requested but not possible, issue suitable warning.
    if (useLocal) { warning(sprintf("Could not use local install option for '%s'; does it exist?", packPath)) }
    devtools::install_github(packPath)
  }

  # Name inference
  if (!is.null(name)) { packName = name }
  else if (local) { packName = basename(packPath) }
  else {
    if (is.null(nameFromUrl)) { warning(sprintf("Installed package from '%s' but could not infer name to reload", packPath)) }
    return()
  }

  # Reload the package if name was given or inferred.
  library(packName)

}
