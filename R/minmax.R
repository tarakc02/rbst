#' Get the smallest or largest key
#' 
#' @param tree A \code{bst}
#' @export
min_key <- function(tree) {
    assertthat::assert_that(inherits(tree, "bst"))
    if (is_empty(tree)) stop("Tree is empty")
    node <- min(tree$root)
    node$key
}

min.bstnode <- function(node, ...) {
    if (is.null(node$left)) return(node)
    min(node$left)
}

#' @describeIn min_key Get the largest key
#' @export
max_key <- function(tree) {
    assertthat::assert_that(inherits(tree, "bst"))
    if (is_empty(tree)) stop("Tree is empty")
    node <- tree$root
    while (!is.null(node$right)) node <- node$right
    node$key
}