#' @title Flexibly create a directory
#'
#' @description Creates the specified folder structure
#'  for a directory as well as a YAML file that tracks the
#'  setup configurations. 
#'
#' @details At each level of the hierarchy, the more
#'  basal folders are required to be present (verified using 
#'  \code{\link{verify}}) and the relevant folders at that level are created 
#'  if not present (using \code{\link{create}}).
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @param ... additional arguments to pass to \code{\link{dir.create}}.
#'
#' @param tree 
#'

#'
#' @return f
#'
#' @examples
#'  \donttest{
#'   create_dir()
#'  }
#'
#' @name create_dir
#'
NULL

#' @rdname create_dir
#'
#' @export
#'
create_dir <- function(tree, quiet = FALSE, ...){
  paths <- articulate(tree = tree, quiet = quiet)
  create(paths = paths, quiet = quiet, ...)
}




#' @title Create folders if they do not exist
#'
#' @description 
#'  A wrapper around \code{\link{dir.create}} that creates multiple folders 
#'    if they do not already exist.
#'
#' @param paths \code{character} vector of the folder paths.
#'
#' @param ... additional arguments to pass to \code{\link{dir.create}}.
#'
#' @details Rather than \code{\link{dir.create}}, which by default tries to
#'  create the directory regardless of its presence, \code{create} only
#'  runs \code{\link{dir.create}} for folders whose \code{paths} do not 
#'  already exist.
#' 
#' @return 
#'  \code{data.table} of \code{paths} and whether or not they were created,
#'  invisibly.
#'
#' @examples
#'  \donttest{
#'   create("./new_folder")
#'  }
#'
#' @name create
#'
NULL


#' @rdname create
#'
#' @export
#'
create <- function(paths, ...){
  return_if_missing(paths)
  UseMethod("create")
}

#' @rdname create
#'
#' @export
#'
create.character <- function(paths, ...){

  npaths <- length(paths)
  out <- data.frame(path = paths, result = NA)
  for(i in 1:npaths){
    if(!dir.exists(paths[i])){
      dir.create(paths[i], ...)
      out$result[i] <- "created"
    } else{
      out$result[i] <- "already existed"
    }
  }
  invisible(out)
}

#' @rdname create
#'
#' @export
#'
create.list <- function(paths, ...){
  articulated_paths <- articulate(paths)
  create(articulated_paths, ...)
}