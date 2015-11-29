#' Compare two keys
#' 
#' To use your own comparator, create a function that compares two keys and 
#' returns an integer, and use it as an argument when constructing a 
#' \code{bst}
#' 
#' @param key1 Keys to compare
#' @param key2 Keys to compare
#' @export
compare_default <- function(key1, key2) {
    if (key1 < key2) return(-1L)
    if (key1 > key2) return(1L)
    0L
}