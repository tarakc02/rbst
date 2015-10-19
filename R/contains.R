#' Does the tree contain a given key?
#' 
#' @param tree A \code{bst} object
#' @param key The key to search for
#' 
#' @details returns TRUE or FALSE
#' @export
contains <- function(tree, key) UseMethod("contains")

#' @export
contains.bst <- function(tree, key) !is.null(retrieve(tree, key))