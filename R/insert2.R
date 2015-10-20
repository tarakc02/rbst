# an in-place insert, only to be used for constructing a new bst via as.bst()
insert2 <- function(tree, key, value) UseMethod("insert2")

insert2.bst <- function(tree, key, value) {
    tree$root <- insert2(tree$root, key, value)
    tree
}

insert2.bstnode <- function(node, key, value) {

    comp <- compare(key, node$key)
    if (comp < 0L)      node$left  <- insert2(node$left,  key, value)
    else if (comp > 0L) node$right <- insert2(node$right, key, value)
    else                node$value <- value
    
    node$n <- 1L + size(node$left) + size(node$right)
    node
}

insert2.NULL <- function(node, key, value) {
    bstnode(key, value, 1L)
}

#' Convert a named vector or list to a bst
#' 
#' Convenience function to quickly populate a bst, given a named list/vector.
#' 
#' @details The normal \link{insert} operation has to make copies of some of 
#' the nodes of a tree in order to maintain the non-destructive nature of the 
#' update -- on average this will be around \code{log(n)} copies where n is 
#' the size of the \code{bst}. In the special case where a new tree is created 
#' and populated from an existing list, so there is no expectation of 
#' continued access to versions of the tree between inserts, we save a bit of 
#' time and unnecessary work by inserting without any copying.
#' @export
as.bst <- function(x) UseMethod("as.bst")

#' @importFrom assertthat assert_that
#' @export
as.bst.default <- function(x) {
    if (any(vapply(x, is.null, logical(1))))
        stop("Can't insert NULL values")
    if (any(is.na(names(x)))) stop("Keys can not be NA")
    num_of_keys <- length(names(x))
    num_of_vals <- length(x)
    
    # will use names as the keys
    assert_that(num_of_keys == num_of_vals)
    
    tree <- bst()
    mapply(function(k, v) insert2(tree, k, v), names(x), x)
    tree
}