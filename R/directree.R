

#' @title Create a Directory Tree
#'
#' @description Creates a tree of directories with flexible structure by
#'  running \code{\link[base]{dirs.create}} over \code{paths} defined
#'  by the generalized \code{tree} input list, which is processed through
#'  \code{\link{tree_paths}}.
#'
#' @param tree \code{list} object describing a directory tree hierarchicaly as
#'  ingested by \code{\link{tree_paths}}. See \code{Examples}.
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
#'  \cr \cr
#'  \strong{NB:} The default argument is changed to \code{FALSE} compared to
#'  \code{NA}, given that the folders are being created and so are by 
#'  default, assumed to not yet exist. The argument is retained, however,
#'  as it can be useful for avoiding overwriting of existing folders.
#'
#' @param showWarnings \code{logical}: should the warnings on failure be 
#'  shown? 
#'  See \code{\link[base]{dir.create}}.
#'
#' @param recursive \code{logical}: should elements of the path other than 
#'  the last be created? If \code{TRUE}, like the Unix command 
#'  \code{mkdir -p}. 
#'  \cr \cr
#'  \strong{NB:} The default argument is changed to \code{TRUE} compared
#'  to \code{\link[base]{dir.create}} and \code{\link{dirs.create}} in order
#'  to create the full tree. Setting \code{recursive = FALSE} will generate
#'  a "tree" with only its final (branch tip) folders created.
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
#'    1
#'  
#'   }
#'
#' @export
#'
tree.create <- function (tree         = list(),
                         fsep         = .Platform$file.sep,
                         winslash     = "\\", 
                         mustWork     = NA,
                         showWarnings = TRUE, 
                         recursive    = TRUE, 
                         mode         = "0777") {

  dirs.create(paths        = tree_paths(tree      = tree, 
                                        fsep      = fsep,
                                        winslash  = winslash,
                                        mustWork  = mustWork,
                                        recursive = recursive),
              showWarnings = showWarnings,
              recursive    = recursive,
              mode         = mode)

 
}

#' @title Create the Paths for a Directory Tree
#'
#' @description Creates a vector of paths for a directory tree through
#'  dot-based list expansion
#'
#' @param tree \code{list} object describing a directory tree.
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
#' @param recursive \code{logical}: should elements of the path other than 
#'  the last be created? If \code{TRUE}, like the Unix command 
#'  \code{mkdir -p}. 
#'  \cr \cr
#'  \strong{NB:} The default argument is changed to \code{TRUE} compared
#'  to \code{\link[base]{dir.create}} and \code{\link{dirs.create}} in order
#'  to create the full tree. Setting \code{recursive = FALSE} will generate
#'  a "tree" with only its final (branch tip) folders created.
#'
#' @return \code{character} vector of results of the \code{...} entries
#'  expanded and run through \code{\link{normalized_file_path}}.
#'
#'
#' @export
#'
tree_paths <- function (tree      = list(),
                        fsep      = .Platform$file.sep,
                        winslash  = "\\", 
                        mustWork  = NA,
                        recursive = TRUE) {


# take your time in here, its a bit more complex, figure
# out the best way to generalize an input

  depth <- list_depth(tree)

# if recursive is false, we only want the tip-most folders

  for(i in 1:depth){

    width[i] <- length(

  }



  #
  #  paths <- SOMETHING <- tree
  #

#  paths <- NULL
#  
#  mapply(FUN       = normalized_file_path, 
#         path      = paths,
#         fsep      = fsep,
#         winslash  = winslash,
#         mustWork  = mustWork, 
#         USE.NAMES = FALSE)

}




#' @title Determine the depth of a list
#'
#' @description Evaluate an input for the depth of its nesting. 
#'
#' @details If \code{xlist = list()}, then technically the input value is a 
#'  list, but is empty (of length \code{0}), so depth is returned as \code{0}.
#'
#' @param xlist Focal input \code{list}.
#'
#' @return \code{integer} value of the depth of the list.
#' 
#' @examples
#'  list_depth("a")
#'  list_depth(list())
#'  list_depth(list("a"))
#'  list_depth(list(list("a")))
#'
#' @export 
#'
list_depth <- function (xlist) {

  xx <- match.call()
  xxx <- deparse(xx[[2]])

  if (xxx == "list()") {

    0L

  } else if (is.list(xlist)) {

    1L + max(sapply(xlist, list_depth))

  } else {

    0L

  }

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

  normalizePath(path     = file.path(..., 
                                     fsep = fsep), 
                winslash = winslash, 
                mustWork = mustWork)

}


