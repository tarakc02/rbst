#' Retrieve the value associated with a given key
#' 
#' If the key exists in the tree, \code{retrieve} returns the associated value. 
#' Otherwise, \code{retrieve} returns \code{NULL}.
#' 
#' @param tree A \code{bst}
#' @param key The key to search for
#' @param ... Ignored
#' 
#' @examples
#' mytree <- bst()
#' mytree <- insert(mytree, "a", 7)
#' retrieve(mytree, "a")
#' # can also use subset operator
#' mytree["a"]
#' @export
retrieve <- function(tree, key, ...) UseMethod("retrieve")

#' @importFrom assertthat assert_that
#' @importFrom assertthat is.scalar
#' @export
retrieve.bst <- function(tree, key) {
    assert_that(is.scalar(key))
    assert_that(!is.na(key))
    retrieve(tree$root, key, NULL)
}

retrieve.bstnode <- function(node, key, sentinel) {
    x <- compare(key, node$key)
    if (x < 0)      return(retrieve(node$left, key, sentinel))
    else return(retrieve(node$right, key, node))
}

retrieve.NULL <- function(node, key, sentinel) sentinel$value

#' @rdname retrieve
#' @export
`[.bst` <- function(tree, key) retrieve(tree, key)