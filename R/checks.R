check <- function(tree) {
    is_bst(tree) &&
        is_23(tree) &&
        is_balanced(tree)
}

is_bst <- function(tree, ...) UseMethod("is_bst")

is_bst.bst <- function(tree) {
    is_bst(tree$root, tree$compare, NULL, NULL)
}

is_bst.bstnode <- function(node, compare, min, max) {
    if (!is.null(min) && compare(node$key, min) <= 0) return(FALSE)
    if (!is.null(max) && compare(node$key, max) >= 0) return(FALSE)
    is_bst(node$left, compare, min, node$key) && 
        is_bst(node$right, compare, node$key, max)
}

is_bst.NULL <- function(node, compare, min, max) TRUE

is_23 <- function(tree) UseMethod("is_23")

is_23.bst <- function(tree) {
    is_23(tree$root)
}

is_23.bstnode <- function(node) {
    # no right-leaning red links
    if (is_red(node$right)) return(FALSE)
    
    # no sequences of two red links in a row
    if (is_red(node) && is_red(node$left)) return(FALSE)
    
    # recurse
    is_23(node$left) && is_23(node$right)
}

is_23.NULL <- function(node) TRUE