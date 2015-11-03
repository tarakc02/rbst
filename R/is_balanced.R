#' @export
is_balanced <- function(tree, ...) UseMethod("is_balanced")

#' @export
is_balanced.bst <- function(tree) {
    black <- 0
    x <- tree$root
    while (!is.null(x)) {
        if (!is_red(x)) black <- black + 1
        x <- x$left
    }
    is_balanced(tree$root, black)
}

is_balanced.bstnode <- function(x, black) {
    if (!is_red(x)) black <- black - 1
    is_balanced(x$left, black) && is_balanced(x$right, black)
} 

is_balanced.NULL <- function(x, black) black == 0