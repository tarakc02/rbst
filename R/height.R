#' The height of a tree
#' 
#' Used for confirming balance and testing
#' 
#' @param tree A \code{bst}
#' @export
height <- function(tree) UseMethod("height")

height.bst <- function(tree) {
    height(tree$root)
}

height.bstnode <- function(node) {
    1L + max(height(node$left), height(node$right))
}

height.NULL <- function(nada) -1L