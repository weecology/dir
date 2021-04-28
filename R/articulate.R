#' @title Articulate input 
#'
#' @description 
#'  ffff
#'
#' @param tree \code{list} defining the directory.
#'
#' @param quiet \code{logical} indicator if progress messages should be
#'  quieted.
#'
#' @return 
#'  Directory tree as a \code{character} \code{vector}.
#'
#' @examples
#'  \donttest{
#'   articulate()
#'  }
#'
#' @name tree
#'
NULL

#' @rdname tree
#'
#' @export
#'
articulate <- function(x){
  return_if_missing(x)
  UseMethod("articulate")
}

#' @rdname tree
#'
#' @export
#'
articulate.list <- function(x){



}



