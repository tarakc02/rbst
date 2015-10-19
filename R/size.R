#' Get the size of a tree
#' 
#' Returns the number of nodes in a binary search tree
#' 
#' @param tree A \code{bst}
#' @export
size <- function(tree) UseMethod("size")

#' @export
size.bst <- function(tree) size(tree$root)

size.bstnode <- function(node) {
    node$n
}
size.NULL <- function(nada) 0

#' Is the tree empty?
#' 
#' @param tree A \code{bst}
#' 
#' @export
is_empty <- function(tree) {
    size(tree) == 0
}