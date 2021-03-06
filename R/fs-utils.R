# fs-utils.R
# Author: Vince Reuter
# email: vince.reuter@gmail.com


#' Path expansion utility
#'
#' \code{expandPath} takes a path and expands user/environment variables.
#' \code{NULL}, \code{NA}, and empty path inputs are returned as-is. For 
#' environment variables, the assumption is that each will be prefixed with 
#' a dollar-sign \code{$}.
#'
#' @param path The filesystem path to expand.
#' @return Input path with user/environment variables expanded.
#' @family paths
#' @export
expandPath = function(path) {

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


#' Builder of path to file within project's results folder.
#'
#' \code{makeFilePath} uses information about file paths from a project 
#' configuration object, in conjunction with a sample name and how to 
#' deal with subfolder(s), to create a path to the output file that's 
#' known to the project for the sample indicated by \code{sampleName}, and 
#' that corresponds to a particular type/format (e.g., as generated by a 
#' particular pipeline).
#'
#' @param base Project configuration object (environment- or list-like), from  
#'             which the parent folder is derived. Alternatively, an actual 
#'             path to a parent folder.
#' @param sampleName Name for the sample for which to create filepath.
#' @param subdir What to assume about the project's folder structure. If a 
#'               Boolean value, this indicates whether the expectation is for 
#'               the file to live within a results subfolder named with the 
#'               \code{sampleName}. If a string, this indicates the actual
#'               name of the subfolder within the project's results folder. 
#'               If a function, it should accept \code{sampleName} and the 
#'               derive a name for the appropriate subfolder that way.
#' @param suffix Text to place between the sample name and the extension.
#'               The default is no suffix, which will also result from an 
#'               argument that's empty, \code{NULL}, or \code{FALSE}.
#' @return Path expected for file for the project defined by \code{base} and 
#'         the sample indicated by \code{sampleName}. The correspondence 
#'         with file type/format is captured by the \code{subdir} and 
#'         \code{extension} specifications.
#' @family paths
#' @export
makeFilePath = function(base, sampleName, subdir, extension, suffix = NULL) {

  # Use sample name as filename base, possibly with suffix.
  nameBase = sampleName
  if (is.character(suffix)) { nameBase = paste0(nameBase, suffix) }

  # Filename can be constructed right awawy since it doesn't depend on any 
  # of the logic regarding input type for base or subdir.
  filename = sprintf("%s.%s", nameBase, extension)
  
  # Allow base to be raw text or a project config (named/nested list).
  if (!is.character(base)) { base = base$metadata$results_subdir }

  # Allow Boolean or callable argument to subdir parameter (rather than text).
  if(is.logical(subdir)) {
    # Omit subdir if it's toggled off; otherwise, use sample name.
    if (!subdir) { return(file.path(base, filename)) }
    subdir = sampleName
  } else if (is.function(subdir)) { subdir = subdir(sampleName) }
  
  return(file.path(base, subdir, filename))
}


#' Builder of folder(s)
#'
#' \code{makedirs} creates a folder and any intermediates needed, 
#' doing nothing if the folder already exists.
#'
#' @param dirpath Full path to the folder to create.
#' @param permissions Permissions string.
#' @param force_chmod Whether to change the permissions to the given 
#'                    setting if the folder already exists.
#' @family paths
#' @export
makedirs = function(dirpath, permissions = "0777", force_chmod = FALSE) {
  if (file_test("-d", dirpath)) {
    if (force_chmod) { Sys.chmod(dirpath, mode = permissions) }
    return(FALSE)
  } else {
    dir.create(dirpath, recursive = TRUE, mode = permissions)
    return(TRUE)
  }
}


#' Filepath builder leveraging environment variable
#'
#' \code{.envVarPath} uses the name of an environment variable and the name 
#' of a folder to create a filepath.
#'
#' @param var_name The name of the environment variable, the value of which 
#'                 is used as the base for the filepath that's constructed. 
#'                 That base is what will be returned from something like 
#'                 \code{dirname(filepath)}, with \code{filepath} being the 
#'                 value returned by this function.
#' @param folder Name for folder under the value of \code{var_name}.
#' @return The filepath with the value of \code{var_name} as the based/parent 
#'         folder and \code{folder} within it. Empty string if the 
#'         environment variable isn't defined.
#' @family paths
.envVarPath = function(var_name, folder) {
  var_path = Sys.getenv(var_name)
  if ( is.null(var_path) | identical("", var_path) ) { return("") }
  return(file.path(var_path, folder))
}


#' Basic filepath builder.
#'
#' \code{.filepath} expands a base filepath and joins it to a folder name.
#'
#' @param base A base filepath to which to join a folder name.
#' @param folder The name to join to \code{base} to create a filepath.
#' @family paths
.filepath = function(base, folder) { file.path(path.expand(base), folder) }


#' Judge of existence of path as a directory
#'
#' \code{.isDir} determines whether a given path is an extant directory.
#' This is a convenience function, wrapping \code{file_test} and handling 
#' \code{NULL} or empty input path.
#'
#' @param path The path to test for status as extant directory.
#' @return Whether the given path is an extant directory. \code{FALSE} if 
#'         \code{path} is \code{NULL} or empty.
#' @family paths
.isDir = function(path) {
  if ( is.null(path) | identical("", path) ) { return(FALSE) }
  return(file_test("-d", path))
}
