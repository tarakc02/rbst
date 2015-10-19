#' Retrieve the value associated with a given key
#' 
#' If the key exists in the tree, \code{retrieve} returns the associated value. 
#' Otherwise, \code{retrieve} returns \code{NULL}.
#' 
#' @param tree A \code{bst}
#' @param key The key to search for
#' 
#' @examples
#' mytree <- bst()
#' mytree <- insert(mytree, "a", 7)
#' retrieve(mytree, "a")
#' # can also use subset operator
#' mytree["a"]
#' @export
retrieve <- function(tree, key) UseMethod("retrieve")

#' @import assertthat
#' @export
retrieve.bst <- function(tree, key) {
    assertthat::assert_that(is.scalar(key))
    assertthat::assert_that(!is.na(key))
    retrieve(tree$root, key)
}

retrieve.bstnode <- function(node, key) {
    x <- compare(key, node$key)
    if (x < 0)      return(retrieve(node$left, key))
    else if (x > 0) return(retrieve(node$right, key))
    else            return(node$value)
}

retrieve.NULL <- function(node, key) NULL

#' @rdname retrieve
#' @export
`[.bst` <- function(tree, key) retrieve(tree, key)