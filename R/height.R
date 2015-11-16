#' The height of a tree
#' 
#' Used for confirming balance and testing
#' 
#' @param tree A \code{bst}
#' @export
height <- function(tree) UseMethod("height")

#' @export
height.bst <- function(tree) {
    height(tree$root) + 1L
}

height.bstnode <- function(node) {
    1L + max(height(node$left), height(node$right))
}

height.NULL <- function(nada) -1L