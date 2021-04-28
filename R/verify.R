#' @title Verify that folders exist.
#'
#' @description 
#'  Throws an error if any of the folders (specified by \code{path}) do 
#'  not exist. 
#'
#' @param paths \code{character} vector of the folder paths.
#'
#' @return 
#'  Throws an error if any of the folders do not exist, otherwise \code{NULL}
#'
#' @examples
#'  \donttest{
#'   verify(".")
#'  }
#'
#' @name verify
#'
NULL

#' @rdname verify
#'
#' @export
#'
verify <- function(paths = NULL){
  return_if_null(paths)
  misses <- NULL
  for(i in 1:length(paths)){
    if (!dir.exists(paths[i])){
      misses <- c(misses, paths[i])
    }
  }
  nmisses <- length(misses)
  if (nmisses == 0){
    return(invisible(NULL))
  } 
  if (nmisses == 1){
    msg <- paste0("\n Folder does not exist at ", misses)
  } else{
    msg_m <- paste(misses, collapse = ", ")
    msg <- paste0("\n Folders do not exist at ", msg_m)
  }
  msg2 <- c("Missing directory components", msg, 
             "\n Run `create_dir` to create directory")
  stop(msg2, call. = FALSE)
}

