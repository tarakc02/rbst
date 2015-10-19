#' Compare two keys
#' 
#' To create a custom \code{compare} method for a class, define a method that
#' takes two objects of the class, \code{key1} and \code{key2}, and returns 
#' -1, 0, or 1.
#' @export
compare <- function(key1, key2) UseMethod("compare")

#' @export
compare.default <- function(key1, key2) {
    if (key1 < key2) return(-1L)
    if (key1 > key2) return(1L)
    0L
}