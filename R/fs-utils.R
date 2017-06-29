# fs-utils.R
# Author: Vince Reuter
# email: vince.reuter@gmail.com


#' Path expansion utility
#'
#' \code{ExpandPath} takes a path and expands user/environment variables.
#' \code{NULL}, \code{NA}, and empty path inputs are returned as-is. For 
#' environment variables, the assumption is that each will be prefixed with 
#' a dollar-sign \code{$}.
#'
#' @param path The filesystem path to expand.
#' @return Input path with user/environment variables expanded.
#' @export
ExpandPath = function(path) {

  # Handle null/empty input.
  if (is.null(path) | is.na(path) | identical("", path)) { return(path) }

  # Helper functions
  chopPath = function(p) { 
    if (p == dirname(p)) p else c(chopPath(dirname(p)), basename(p)) }
  expand = function(pathPart) { 
    if (startsWith(pathPart, "$")) {
      system(sprintf("echo %s", pathPart), intern = TRUE)
    } else { pathPart }
  }

  # Split path; short-circuit return or ensure no reference to this folder.
  parts = chopPath(path)
  if (length(parts) < 2) { return(parts) }
  if (identical(".", parts[1])) { parts = parts[2:length(parts)] }

  # Expand any environment variables and return the complete path.
  fullPath = do.call(file.path, lapply(parts, expand))
  return(fullPath)
}
