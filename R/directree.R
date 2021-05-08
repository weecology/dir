

#' @title Create a Directory Tree
#'
#' @description Creates a tree of directories with flexible structure by
#'  running \code{\link[base]{dirs.create}} over \code{paths} defined
#'  by the generalized \code{...} inputs, which are processed through
#'  \code{\link{normalize_file_path}}.
#'
#' @param ...  \code{character} vectors for file paths. 
#'  See \code{\link[base]{file.path}}.
#'
#' @param fsep Path separator to use (assumed to be ASCII).
#'  See \code{\link[base]{file.path}}.
#'
#' @param winslash Separator to be used on Windows - ignored elsewhere. 
#'  See \code{\link[base]{normalizePath}}.
#'
#' @param mustWork \code{logical}: if \code{TRUE} then an error is given if 
#'  the result cannot be determined; if \code{NA}, then a warning.
#'  See \code{\link[base]{normalizePath}}.
#'
#' @param showWarnings \code{logical}: should the warnings on failure be 
#'  shown? 
#'  See \code{\link[base]{dir.create}}.
#'
#' @param recursive \code{logical}: should elements of the path other than 
#'  the last be created? If \code{TRUE}, like the Unix command 
#'  \code{mkdir -p}. The default argument is changed to \code{TRUE} compared
#'  to \code{\link[base]{dir.create}} and \code{\link{dirs.create}}.
#'  See \code{\link[base]{dir.create}}.
#'
#' @param mode The mode to be used on Unix-alikes: it will be coerced by 
#'  \code{\link[base]{as.octmode}}. For \code{\link[base]{Sys.chmod}} it 
#'  is recycled along paths.
#'  See \code{\link[base]{dir.create}}.
#'
#' @return \code{logical} vector of results of \code{\link[base]{dir.create}}
#'  on each of the \code{paths} (which name the vector).
#'  See \code{\link[base]{dir.create}}.
#'
#' @examples
#'  \dontrun{
#'
#'    tree.create("root", c("branch1", "branch2"))
#'  
#'  }
#'
#' @export
#'
tree.create <- function (...,
                         fsep         = .Platform$file.sep,
                         winslash     = "\\", 
                         mustWork     = NA,
                         showWarnings = TRUE, 
                         recursive    = TRUE, 
                         mode         = "0777") {

  paths <- mapply(...,
                  FUN       = normalized_file_path, 
                  fsep      = fsep,
                  winslash  = winslash,
                  mustWork  = mustWork, 
                  USE.NAMES = FALSE)


  dirs.create(paths        = paths,
              showWarnings = showWarnings,
              recursive    = recursive,
              mode         = mode)

 
}

#' @title Flexibly Create Multiple Directories
#'
#' @description Creates multiple directories at once by running
#'  \code{\link[base]{dir.create}} over multiple 
#'  \code{paths} through \code{\link[base]{mapply}}.
#'
#' @param paths \code{character} vector containing multiple path names. 
#'  See \code{\link[base]{dir.create}}.
#'
#' @param showWarnings \code{logical}: should the warnings on failure be 
#'  shown? 
#'  See \code{\link[base]{dir.create}}.
#'
#' @param recursive \code{logical}: should elements of the path other than 
#'  the last be created? If \code{true}, like the Unix command 
#'  \code{mkdir -p}.
#'  See \code{\link[base]{dir.create}}.
#'
#' @param mode The mode to be used on Unix-alikes: it will be coerced by 
#'  \code{\link[base]{as.octmode}}. For \code{\link[base]{Sys.chmod}} it 
#'  is recycled along paths.
#'  See \code{\link[base]{dir.create}}.
#'
#' @return \code{logical} vector of results of \code{\link[base]{dir.create}}
#'  on each of the \code{paths} (which name the vector).
#'  See \code{\link[base]{dir.create}}.
#'
#' @examples
#'  \dontrun{
#'
#'    paths <- normalized_file_path(".", c("folder1", "folder2"),
#'                                  mustWork = FALSE)
#'    dirs.create(paths)
#'  
#'  }
#'
#' @export
#'
dirs.create <- function (paths, 
                         showWarnings = TRUE, 
                         recursive    = FALSE, 
                         mode         = "0777") {

  mapply(FUN          = dir.create,
         path         = paths,
         showWarnings = showWarnings,
         recursive    = recursive,
         mode         = mode, 
         USE.NAMES    = TRUE)
  
}




#' @title Platform-Independent Normalized File Paths
#'
#' @description Combines \code{\link[base]{normalizePath}} and 
#'  \code{\link[base]{file.path}} to produce normalized canonical paths
#'  for files in a platform-independent fashion.
#'
#' @param ...  \code{character} vectors for file paths. 
#'  See \code{\link[base]{file.path}}.
#'
#' @param fsep Path separator to use (assumed to be ASCII).
#'  See \code{\link[base]{file.path}}.
#'
#' @param winslash Separator to be used on Windows - ignored elsewhere. 
#'  See \code{\link[base]{normalizePath}}.
#'
#' @param mustWork \code{logical}: if \code{TRUE} then an error is given if 
#'  the result cannot be determined; if \code{NA}, then a warning.
#'  See \code{\link[base]{normalizePath}}.
#'
#' @return \code{character} vector of the full file paths.
#'  See \code{\link[base]{normalizePath}} and \code{\link[base]{file.path}}.
#'
#' @examples
#'  normalized_file_path("~")
#'  normalized_file_path(".", "folder", "file.ext", mustWork = FALSE)
#'  # compare to 
#'  normalizePath(c(".", "folder", "file.ext"), mustWork = FALSE)
#'  file.path(".", "folder", "file.ext")
#'
#' @export
#'
normalized_file_path <- function (..., 
                                  fsep     = .Platform$file.sep,
                                  winslash = "\\", 
                                  mustWork = NA) {

  normalizePath(path     = file.path(..., fsep = fsep), 
                winslash = winslash, 
                mustWork = mustWork)

}


