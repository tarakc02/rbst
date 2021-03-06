# an in-place insert, only to be used for constructing a new bst via as.bst()
insert2 <- function(tree, key, value, ...) UseMethod("insert2")

insert2.bst <- function(tree, key, value) {
    tree$root <- insert2(tree$root, key, value, tree$compare)
    tree$root$red <- FALSE
    tree
}

insert2.bstnode <- function(node, key, value, compare) {

    comp <- compare(key, node$key)
    if (comp < 0L)      node$left  <- insert2(node$left,  key, value, compare)
    else if (comp > 0L) node$right <- insert2(node$right, key, value, compare)
    else                node$value <- value
    
    node <- balance(node)
    node$n <- 1L + size(node$left) + size(node$right)
    node
}

insert2.NULL <- function(node, key, value, compare) {
    bstnode(key, value, 1L, TRUE)
}

#' Convert a named vector or list to a bst
#' 
#' Convenience function to quickly populate a bst, given a named list/vector.
#' 
#' @param x A named vector or list
#' @param compare The comparator function
#' 
#' @export
as.bst <- function(x, compare) UseMethod("as.bst")

#' @importFrom assertthat assert_that
#' @export
as.bst.default <- function(x, compare = compare_default) {
    bst(names(x), x, compare)
}