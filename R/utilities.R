#' @title If a value is missing, trigger the parent function's return
#'
#' @description If the focal input is \code{NULL}, return \code{value} from
#'  the parent function. Should only be used within a function.
#'
#' @param x Focal input.
#'
#' @param value If \code{x} is missing, \code{\link{return}} this input
#'  from the parent function. 
#'
#' @return If \code{x} is not missing, \code{NULL} is returned invisibly. If 
#'  \code{x} is missing, if \code{value} is provided, the result of 
#'  \code{\link{return}} with \code{value} as its input evaluated within the 
#'   parent function's environment is returned, otherwise \code{NULL} is 
#'   returned invisibly.
#' 
#' @examples
#'  ff <- function(x = 1, missing_return = "hello"){
#'    return_if_missing(x, missing_return)
#'    x
#'  }
#'  ff()
#'
#' @export 
#'
return_if_missing <- function(x, value){
  if(missing(x)){
    if(missing(value)){
      value <- NULL
    }
    do.call(return, args = list(value), envir = sys.frame(-1))
  } else {
    invisible(NULL)
  }
}

#
# TO DO: make the do.call return invisibly when value is missing
#



#' @title Determine the depth of a list
#'
#' @description Measure the depth a list's nesting. 
#'
#' @details If \code{xlist = list()}, then technically the input value is a 
#'  list, but is empty (of length \code{0}), so depth is returned as \code{0}.
#'
#' @param x Focal \code{list} to be evaluated.
#'
#' @return \code{integer} value of the depth of the list.
#' 
#' @examples
#'  list1 <- list(main  = "main", 
#'                other = "other")
#'  list2 <- list(main  = list(data    = "data", 
#'                             code    = "code",
#'                             results = "results"), 
#'                other = "other")
#'  list3 <- list(main  = list(data    = "data", 
#'                             code    = "code",
#'                             results = list("2021-01-01" = "2021-01-01",
#'                                            "2021-01-02" = "2021-01-02")), 
#'                other = "other")
#'  
#'  depth(list1)
#'  depth(list2)
#'  depth(list3)
#'
#' @export 
#'
depth <- function(x){
  return_if_missing(x, value = NULL)
  xx <- match.call()
  xxx <- deparse(xx[[2]])
  if(xxx == "list()"){
    0L
  } else if (is.list(x)){

    1L + max(sapply(x, depth))

  } else {
    0L
  }
}